{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_dec_oggvorbis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pa_base,
  pa_register,
  pa_stream,
  paio_messagequeue,
  ctypes,
  OggHfObject,
  paio_log;

type
  { TPAOggVorbisDecoderSource }

  TPAOggVorbisDecoderSource = class(TPAStreamSource, IPAPlayable, IPAStream)
  private
    FInited: Boolean;
    // libvorbisfile's own ov_seekable() verdict, captured while opening on the
    // main thread (InitValues) and refreshed by the worker (InitOgg). More
    // accurate than the generic stream probe: a chained or live ogg can be
    // stream-seekable yet report itself unseekable as a bitstream.
    FLibSeekable: Boolean;
    Fogg: TOggDecFloat;
    function InitOgg: Boolean;
    procedure DeInitOgg;
  protected
    procedure SetStream(AValue: TStream); override;
    function InternalOutputToDestination: Boolean; override;
    procedure SignalDestinationsDone; override;
    function HandleMessage(var AMsg: TPAIOMessage): Boolean; override;
    // IPAPlayable
    function  CanSeek: Boolean;
    function  GetPosition: Double;
    procedure SetPosition(AValue: Double);
    function  GetMaxPosition: Double;

  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean); override;
    destructor Destroy; override;
    procedure InitValues;
    //IStreamSource
    property Stream;
    //IPAPlayable
    property  Position: Double read GetPosition write SetPosition;
    property  MaxPosition: Double read GetMaxPosition;
  end;

implementation

{ TPAOggVorbisDecoderSource }

procedure TPAOggVorbisDecoderSource.SetStream(AValue: TStream);
begin
  if FStream=AValue then Exit;
  if FStream <> nil then
  begin
    StopData;
    DeInitOgg;
  end;
  inherited SetStream(AValue);

end;

function TPAOggVorbisDecoderSource.InitOgg: Boolean;
begin
  Result := False;
  if FStream = nil then
  begin
    TPALog.Warning(ClassName, 'init failed: no stream');
    Exit;
  end;

  if FInited then
    Exit;

  Fogg := TOggDecFloat.TryCreate(FStream, False);
  // TryCreate returns nil on a non-ogg/unsupported stream, and Info can be nil
  // even on a non-nil decoder for a malformed one; dereferencing Fogg.Info below
  // would crash, so bail out with a message instead.
  if (Fogg = nil) or (Fogg.Info = nil) then
  begin
    TPALog.Warning(ClassName, 'init failed: invalid or unsupported ogg/vorbis stream');
    FreeAndNil(Fogg);
    Exit;
  end;
  Channels:=Fogg.Info^.channels;
  SamplesPerSecond:=FOgg.Info^.rate;
  FLibSeekable:=Fogg.Seekable;
  Format:=afFloat32;
  FInited:=True;
  TPALog.Info(ClassName, 'initialized');
  Result := True;
end;

procedure TPAOggVorbisDecoderSource.DeInitOgg;
begin
  if not FInited then
    Exit;
  FInited := False;
  FreeAndNil(FOgg);
end;

function TPAOggVorbisDecoderSource.InternalOutputToDestination: Boolean;
var
  ChannelData: PPSingle;
  ReadSamples: Integer;
  PlexedData: TSingleArray;
  BitStream: Integer = 0;
begin
  Result := False;
  if not FInited then
    if not InitOgg then
    begin
      // init failed (bad file): tell the destinations we're done so the pipeline
      // shuts down instead of the destination worker hanging on EndOfData.
      SignalDestinationsDone;
      Exit;
    end;

  ReadSamples := FOgg.ReadFloat(ChannelData, AUDIO_BUFFER_FLOAT_SAMPLES div FChannels, @BitStream);

  Result := ReadSamples > 0;

  if Result then
  begin
    PlexedData := JoinChannels(ChannelData, FChannels, ReadSamples);
    WriteToBuffer(PlexedData[0], Length(PlexedData)*SizeOf(Single), ReadSamples<=0);
  end
  else
    SignalDestinationsDone;
end;

procedure TPAOggVorbisDecoderSource.SignalDestinationsDone;
begin
  inherited SignalDestinationsDone;
end;

function TPAOggVorbisDecoderSource.HandleMessage(var AMsg: TPAIOMessage
  ): Boolean;
begin
  Result := True;
  case AMsg.Message of
    PAM_Seek:
      if FInited then
        Fogg.TimePosition:=AMsg.Data;
  else
    Result := False;
  end;
end;

function TPAOggVorbisDecoderSource.CanSeek: Boolean;
begin
  // ov_seekable already accounts for the stream's seek callback, so it supersedes
  // the generic stream probe for vorbis.
  Result := FLibSeekable;
end;

function TPAOggVorbisDecoderSource.GetPosition: Double;
begin
  if not FInited then
    Exit(0);
  Result := Fogg.TimePosition;

end;

procedure TPAOggVorbisDecoderSource.SetPosition(AValue: Double);
begin
  if not FInited then
    Exit;
  if not CanSeek then
  begin
    LogSeekRefused;
    Exit;
  end;

  FMsgQueue.PostMessage(PAM_Seek, AValue);
end;

function TPAOggVorbisDecoderSource.GetMaxPosition: Double;
begin
  if not FInited then
    Exit(0);

  Result := Fogg.TimeLength;
end;

constructor TPAOggVorbisDecoderSource.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create(AStream, AOwnsStream);
  Format:=afFloat32;
  // Populate Channels/SamplesPerSecond from the header now. Otherwise they keep
  // the defaults (2 / 44100) until the worker decodes the first buffer, so any
  // downstream component that reads them before StartData (e.g. the noise
  // removal link sizing its per-channel state) gets the wrong values.
  InitValues;
end;

destructor TPAOggVorbisDecoderSource.Destroy;
begin
  // stop the worker thread before freeing FOgg, which its Execute loop
  // (InternalOutputToDestination) reads from. DeInitOgg frees the decoder
  // object that InitOgg created, otherwise it leaks once per decoder.
  DestroyWaitSync;
  DeInitOgg;
  inherited Destroy;
end;

procedure TPAOggVorbisDecoderSource.InitValues;
var
  Tmp: TOggDecFloat;
  StartPos: Int64;
begin
  StartPos := FStream.Position;
  Tmp := TOggDecFloat.TryCreate(FStream, False);
  if Assigned(Tmp) then
  begin
    // Info can be nil even on a non-nil decoder for a malformed stream;
    // dereferencing it would crash, so only read it when present.
    if Tmp.Info <> nil then
    begin
      Channels:=Tmp.Info^.channels;
      SamplesPerSecond:=Tmp.Info^.rate;
      // Tmp is fully opened (TryCreate calls ov_test_open), so ov_seekable is
      // valid here -- capture it before the worker has its own decoder.
      FLibSeekable:=Tmp.Seekable;
    end
    else
      TPALog.Warning(ClassName, 'init failed: invalid or unsupported ogg/vorbis stream');
    Tmp.Free;
  end;
  Format:=afFloat32;
  // rewind so the worker's decoder reads from where we started.
  FStream.Position:=StartPos;
end;

initialization
  PARegister(partDecoder, TPAOggVorbisDecoderSource, 'OGG/Vorbis', '.ogg', 'OggS', 4);

end.

