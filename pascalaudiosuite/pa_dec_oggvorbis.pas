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
  OggHfObject;

type
  { TPAOggVorbisDecoderSource }

  TPAOggVorbisDecoderSource = class(TPAStreamSource, IPAPlayable, IPAStream)
  private
    FInited: Boolean;
    Fogg: TOggDecFloat;
    function InitOgg: Boolean;
    procedure DeInitOgg;
  protected
    procedure SetStream(AValue: TStream); override;
    function InternalOutputToDestination: Boolean; override;
    procedure SignalDestinationsDone; override;
    procedure HandleMessage(var AMsg: TPAIOMessage); override;
    // IPAPlayable
    function  CanSeek: Boolean;
    function  GetPosition: Double;
    procedure SetPosition(AValue: Double);
    function  GetMaxPosition: Double;

  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean); override;
    procedure InitValues;
    //IPAPlayable
    procedure Play;
    procedure Pause;
    procedure Stop;
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
    Exit;

  if FInited then
    Exit;

  Fogg := TOggDecFloat.TryCreate(FStream, False);
  Channels:=Fogg.Info^.channels;
  SamplesPerSecond:=FOgg.Info^.rate;
  Format:=afFloat32;
  FInited:=True;
  Result := True;
end;

procedure TPAOggVorbisDecoderSource.DeInitOgg;
begin
  if not FInited then
    Exit;

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
      Exit;

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

procedure TPAOggVorbisDecoderSource.HandleMessage(var AMsg: TPAIOMessage);
begin
  case AMsg.Message of
    PAM_Seek:
      if FInited then
        Fogg.TimePosition:=AMsg.Data;
  end;
end;

function TPAOggVorbisDecoderSource.CanSeek: Boolean;
begin
  Result := True;
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
end;

procedure TPAOggVorbisDecoderSource.InitValues;
var
  Tmp: TOggDecFloat;
begin
  Tmp := TOggDecFloat.TryCreate(FStream, False);
  Channels:=Tmp.Info^.channels;
  SamplesPerSecond:=Tmp.Info^.rate;
  Format:=afFloat32;
  Tmp.Free;
  FStream.Position:=0;
end;

procedure TPAOggVorbisDecoderSource.Play;
begin

end;

procedure TPAOggVorbisDecoderSource.Pause;
begin

end;

procedure TPAOggVorbisDecoderSource.Stop;
begin

end;

initialization
  PARegister(partDecoder, TPAOggVorbisDecoderSource, 'OGG/Vorbis', '.ogg', 'OggS', 4);

end.

