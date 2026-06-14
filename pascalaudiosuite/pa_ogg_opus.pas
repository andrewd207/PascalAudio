unit pa_ogg_opus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pa_base,
  pa_register,
  pa_stream,
  paio_messagequeue,
  paio_log,
  paio_ogg_opus;
type

  { TPAOggOpusDecoderSource }

  TPAOggOpusDecoderSource = class(TPAStreamSource, IPAPlayable, IPAStream)
  private
    FInited: Boolean;
    FOpus: TOggOpusDecoder;
    FBuffer: array of Byte;
    function InitOpus: Boolean;
    procedure DeInitOpus;
  protected
    procedure SetStream(AValue: TStream); override;
    function  InternalOutputToDestination: Boolean; override;
    procedure SignalDestinationsDone; override;
    function HandleMessage(var AMsg: TPAIOMessage): Boolean; override;
    // IPAPlayable
    function  CanSeek: Boolean;
    function  GetPosition: Double;
    procedure SetPosition(AValue: Double);
    function  GetMaxPosition: Double;
    function  GetSamplesPerSecond: Integer; override;
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

uses paio_opus;

const
  // largest opus frame: 120 ms at 48 kHz, in samples per channel.
  OPUS_MAX_FRAME_SAMPLES = 5760;

{ TPAOggOpusDecoderSource }

function TPAOggOpusDecoderSource.InitOpus: Boolean;
begin
  Result := False;
  if FStream = nil then
  begin
    TPALog.Warning(ClassName, 'init failed: no stream');
    Exit;
  end;

  if FInited then
    Exit;

  try
    FOpus := TOggOpusDecoder.Create(FStream);
  except
    on E: Exception do
    begin
      // invalid file
      TPALog.Warning('TPAOggOpusDecoderSource', 'failed to open opus stream: ' + E.Message);
      FInited := False;
      FOpus := nil; // shouldn't be set...just to be clear
      Exit;        // Result is already False; don't fall through and deref nil
    end;
  end;
  FOpus.InitDecoder;
  Channels:=FOpus.Channels;

  // Size the PCM output buffer for the largest opus frame: 120 ms at 48 kHz =
  // 5760 samples per channel. The old code used GetSize() -- the size of the
  // opus *decoder struct* -- which is unrelated to the PCM buffer; it happens to
  // be large enough for typical 20 ms packets but is too small for 60/120 ms
  // frames, so opus_decode_float returned BUFFER_TOO_SMALL and the decode ended
  // early on such files.
  SetLength(FBuffer, OPUS_MAX_FRAME_SAMPLES * SizeOf(Single) * Channels);

  SamplesPerSecond:=48000;
  Format:=afFloat32;
  FInited:=True;
  TPALog.Info(ClassName, 'initialized');
  Result := True;
end;

procedure TPAOggOpusDecoderSource.DeInitOpus;
begin
  if not FInited then
    Exit;
  FInited:=False;
  SetLength(FBuffer, 0);
  FreeAndNil(FOpus);
end;

procedure TPAOggOpusDecoderSource.SetStream(AValue: TStream);
begin
  inherited SetStream(AValue);
end;

function TPAOggOpusDecoderSource.InternalOutputToDestination: Boolean;
var
  ReadSamples: Integer;
begin
  Result := False;
  if not FInited then
    if not InitOpus then
    begin
      // couldn't initialise (e.g. invalid/garbage file): tell the destinations
      // we're done so the pipeline shuts down instead of spinning forever
      // waiting for data that will never arrive.
      SignalDestinationsDone;
      Exit;
    end;

  ReadSamples := FOpus.DecodePacket(@FBuffer[0], Length(FBuffer), True);

  Result := ReadSamples > 0;

  if Result then
  begin
    WriteToBuffer(FBuffer[0], ReadSamples * SizeOf(Single) * Channels , ReadSamples<=0);
  end
  else
    SignalDestinationsDone;
end;

procedure TPAOggOpusDecoderSource.SignalDestinationsDone;
begin
  inherited SignalDestinationsDone;
end;

function TPAOggOpusDecoderSource.HandleMessage(var AMsg: TPAIOMessage): Boolean;
var
  lPosition: QWord;
begin
  Result := True;
  case AMsg.Message of
    PAM_Seek:
      begin
        if FInited then
        begin
          lPosition:=QWord(AMsg.Data);
          // will constrain it
          FOpus.SamplePosition:=lPosition;
        end;
      end;
  else
    Result := False;
  end;
end;

function TPAOggOpusDecoderSource.CanSeek: Boolean;
begin
  Result := StreamCanSeek;
end;

function TPAOggOpusDecoderSource.GetPosition: Double;
begin
  if not FInited then
    Result := 0.0
  else
    Result := FOpus.SamplePosition / SamplesPerSecond;
end;

procedure TPAOggOpusDecoderSource.SetPosition(AValue: Double);
begin
  if not FInited then
    Exit;
  if not CanSeek then
  begin
    LogSeekRefused;
    Exit;
  end;
  FMsgQueue.PostMessage(PAM_Seek, Trunc(AValue*SamplesPerSecond));
end;

function TPAOggOpusDecoderSource.GetMaxPosition: Double;
begin
  if not FInited then
    Result := 0.0
  else
    Result := FOpus.TotalSamples / SamplesPerSecond;
end;

function TPAOggOpusDecoderSource.GetSamplesPerSecond: Integer;
begin
  // technically this can be multiple things. 48000, 24000, 12000 etc.
  Result:=48000;
end;

constructor TPAOggOpusDecoderSource.Create(AStream: TStream;
  AOwnsStream: Boolean);
begin
  inherited Create(AStream, AOwnsStream);
  Format := afFloat32
end;

destructor TPAOggOpusDecoderSource.Destroy;
begin
  // stop the worker thread (which uses FOpus in InternalOutputToDestination)
  // before freeing FOpus, then let the ancestor free the stream.
  DestroyWaitSync;
  DeInitOpus;
  inherited Destroy;
end;

procedure TPAOggOpusDecoderSource.InitValues;
var
  Tmp: TOggOpusDecoder;
begin
  Tmp := TOggOpusDecoder.Create(Stream);
  try
    FChannels:=Tmp.Channels;
    Format:=afFloat32;
  finally
    Tmp.Free;
  end;
end;

end.

