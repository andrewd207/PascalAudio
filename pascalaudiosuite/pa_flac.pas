{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_flac;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pa_base, flac_classes, pa_register, pa_stream, paio_messagequeue, paio_log;

type

  { TPAFlacSource }

  TPAFlacSource = class (TPAStreamSource, IPAStream, IPAPlayable)
  protected
    procedure SetStream(AValue: TStream); override;
    function HandleMessage(var AMsg: TPAIOMessage): Boolean; override;
  private
    FFlac: TFlacStreamDecoder;
    FIgnore: Boolean;
    function HandleData(Sender: TFlacStreamDecoder; ASamples: Integer; AChannels: Integer; AChannelData: PPLongInt): Boolean;
    // IPAPlayable
    function  CanSeek: Boolean;
    function  GetPosition: Double;
    procedure SetPosition(AValue: Double);
    function  GetMaxPosition: Double;

    procedure InternalSetPosition(APosition: Double);
  protected
    function InternalOutputToDestination: Boolean; override;
  public
    destructor Destroy; override;
    property  Position: Double read GetPosition write SetPosition;
    property  MaxPosition: Double read GetMaxPosition;
    property Stream;
  end;

  { TPAFlacEncoderLink }

  TPAFlacEncoderLink = class(TPAStreamDestination)
  private
    FEnc: TFlacStreamEncoder;
    FInited: Boolean;
    FCompressionLevel: Integer;
    FInt32Buf: array of LongInt; // reusable S16 -> int32 conversion buffer
    procedure InitEncoder;
    procedure FinishEncode;
  protected
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; override;
    procedure BeforeStreamFree; override;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean); override;
    destructor  Destroy; override;
    // Add a VORBIS_COMMENT tag (e.g. AddComment('TITLE','Song')). Must be called
    // before the first audio buffer arrives -- libFLAC locks metadata in when the
    // encoder is initialised on the first InternalProcessData (same window as the
    // ogg/vorbis encoder's AddComment).
    procedure AddComment(const TagName, Content: String);
    property  CompressionLevel: Integer read FCompressionLevel write FCompressionLevel;
    property  Stream;
  end;

implementation

uses
  paio_types;

{ TPAFlacSource }

destructor TPAFlacSource.Destroy;
begin
  // stop the worker thread (which uses FFlac in InternalOutputToDestination)
  // before freeing FFlac, then let the ancestor free the stream.
  DestroyWaitSync;
  FreeAndNil(FFlac);
  inherited Destroy;
end;

procedure TPAFlacSource.SetStream(AValue: TStream);
begin
  if Assigned(FFlac) then
    FreeAndNil(FFlac);

  inherited SetStream(AValue);

  if not Assigned(FStream) then
    Exit;

  FFlac := TFlacStreamDecoder.Create(FStream, False);
  FFlac.OnOutput:=@HandleData;
  FFlac.ProcessUntilEndOfMetadata;

  Channels:=FFlac.Channels;
  SamplesPerSecond:=FFlac.SampleRate;
  case FFlac.BitsPerSample of
    16: Format:=afS16;
  else
    raise Exception.Create('unsupported flac data type');
  end;
end;

function TPAFlacSource.HandleMessage(var AMsg: TPAIOMessage): Boolean;
begin
  Result := True;
  case AMsg.Message of
    PAM_Seek:
      begin
        InternalSetPosition(AMsg.Data);
      end;
  else
    Result := FAlse;
  end;
end;

function TPAFlacSource.HandleData(Sender: TFlacStreamDecoder; ASamples: Integer; AChannels: Integer; AChannelData: PPLongInt): Boolean;
var
  ChannelsData: array[0..7] of SmallInt; // 16 bit signed. max 8 channels
  i, j: Integer;
  Msg: TPAIOMessage;
begin
  Result := True;
  if FIgnore then
    Exit;
  if FMsgQueue.HasMessage then
  begin
    Msg := FMsgQueue.PopMessage;

    case Msg.Message of
      PAM_Seek:
        begin
          InternalSetPosition(Msg.Data);
          Msg.Free;
        end
    else
      FMsgQueue.InsertBefore([Msg.Message], Msg);
    end;

  end;

  for i := 0 to ASamples-1 do
  begin
    // plex channels
    for j := 0 to AChannels-1 do
    begin
      ChannelsData[j] := AChannelData[j][i];
    end;
    WriteToBuffer(ChannelsData[0],SizeOf(Smallint)*AChannels, False);
  end;
end;

function TPAFlacSource.CanSeek: Boolean;
begin
  Result := StreamCanSeek;
end;

function TPAFlacSource.GetPosition: Double;
begin
  if not Assigned(FFlac) then
    Exit(0);
  Result := FFlac.DecodedSamplePosition / SamplesPerSecond;
end;

procedure TPAFlacSource.SetPosition(AValue: Double);
begin
  if not CanSeek then
  begin
    LogSeekRefused;
    Exit;
  end;
  FMsgQueue.PostMessage(PAM_Seek, AValue);
end;

function TPAFlacSource.GetMaxPosition: Double;
begin
  REsult := FFlac.TotalSamples / SamplesPerSecond;
end;

procedure TPAFlacSource.InternalSetPosition(APosition: Double);
begin
  FIgnore:=True;
  try
    FFlac.Flush;
    FFlac.SeekAbsolute(Trunc(APosition * SamplesPerSecond));
  finally
    FIgnore:=False;
  end;
end;

function TPAFlacSource.InternalOutputToDestination: Boolean;
begin
  Result := FFlac.ProcessSingle;
  if not Result or (FFlac.State = fsdsEndOfStream) then
  begin
    Result := False;
    FFlac.Flush;
    SignalDestinationsDone;
  end;
end;

{ TPAFlacEncoderLink }

constructor TPAFlacEncoderLink.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create(AStream, AOwnsStream);
  // FLAC encodes integer PCM; take 16-bit interleaved from the pipeline.
  Format := afS16;
  FCompressionLevel := 5;
  // TPAStreamDestination.Create assigns FStream directly, so it is valid now; the
  // encoder writes its output there. Format/metadata are set later in InitEncoder,
  // once the DataSource has supplied Channels/SamplesPerSecond, and after any
  // AddComment calls.
  FEnc := TFlacStreamEncoder.Create(FStream);
end;

destructor TPAFlacEncoderLink.Destroy;
begin
  // inherited stops the worker and runs BeforeStreamFree (which finishes the
  // encode while FStream is still alive) before the stream is freed.
  inherited Destroy;
  FEnc.Free;
end;

procedure TPAFlacEncoderLink.AddComment(const TagName, Content: String);
begin
  FEnc.AddVorbisComment(TagName, Content);
end;

procedure TPAFlacEncoderLink.InitEncoder;
begin
  if FInited then
    Exit;
  FInited := True;
  FEnc.Channels := Channels;
  FEnc.BitsPerSample := 16;
  FEnc.SampleRate := SamplesPerSecond;
  FEnc.CompressionLevel := FCompressionLevel;
  if FEnc.Init then
    TPALog.Info(ClassName, 'initialized')
  else
    TPALog.Error(ClassName, 'failed to start FLAC encoder');
end;

procedure TPAFlacEncoderLink.FinishEncode;
begin
  if not FInited then
    Exit;
  FEnc.Finish;
  FInited := False;
end;

function TPAFlacEncoderLink.InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64;
var
  Frames, Total, i: Integer;
  Src: PSmallInt;
begin
  if not FInited then
    InitEncoder;

  Result := ACount;

  // FLAC wants one FLAC__int32 per sample (sign-extended), interleaved by frame.
  Frames := ACount div (BytesPerSample(afS16) * Channels);
  Total  := Frames * Channels;
  if Total > 0 then
  begin
    if Length(FInt32Buf) < Total then
      SetLength(FInt32Buf, Total);
    Src := PSmallInt(@AData);
    for i := 0 to Total-1 do
      FInt32Buf[i] := Src[i]; // S16 -> int32, sign-extended
    FEnc.ProcessInterleaved(@FInt32Buf[0], Frames);
  end;

  // finish in the worker thread when the source flags the last buffer, mirroring
  // the ogg encoder (doing it from EndOfData would race the worker).
  if AIsLastData then
    FinishEncode;
end;

procedure TPAFlacEncoderLink.BeforeStreamFree;
begin
  // flush + patch STREAMINFO even if the last-data flag never arrived (e.g. the
  // encode was interrupted), while FStream is still valid.
  if FInited then
    FinishEncode;
end;

initialization
  PARegister(partDecoder, TPAFlacSource, 'FLAC', '.flac', 'fLaC', 4);
  PARegister(partEncoder, TPAFlacEncoderLink, 'FLAC', '.flac', 'fLaC', 4);
end.

