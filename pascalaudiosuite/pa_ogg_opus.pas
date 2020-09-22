unit pa_ogg_opus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pa_base,
  pa_register,
  pa_stream,
  paio_messagequeue,
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
    procedure HandleMessage(var AMsg: TPAIOMessage); override;
    // IPAPlayable
    function  CanSeek: Boolean;
    function  GetPosition: Double;
    procedure SetPosition(AValue: Double);
    function  GetMaxPosition: Double;
    function  GetSamplesPerSecond: Integer; override;
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

uses paio_opus;

{ TPAOggOpusDecoderSource }

function TPAOggOpusDecoderSource.InitOpus: Boolean;
begin
  Result := False;
  if FStream = nil then
    Exit;

  if FInited then
    Exit;

  try
    FOpus := TOggOpusDecoder.Create(FStream);
  except
    // invalid file
    FInited := False;
    FOpus := nil; // shouldn't be set...just to be clear
  end;
  FOpus.InitDecoder;
  Channels:=FOpus.Channels;

  SetLength(FBuffer, TOpusDecoder.GetSize(Channels));

  SamplesPerSecond:=48000;
  Format:=afFloat32;
  FInited:=True;
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
      Exit;

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

procedure TPAOggOpusDecoderSource.HandleMessage(var AMsg: TPAIOMessage);
var
  lPosition: QWord;
begin
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
  end;
end;

function TPAOggOpusDecoderSource.CanSeek: Boolean;
begin
  Result := True;
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

procedure TPAOggOpusDecoderSource.Play;
begin

end;

procedure TPAOggOpusDecoderSource.Pause;
begin

end;

procedure TPAOggOpusDecoderSource.Stop;
begin

end;

end.

