unit pa_mmdevice;

{$mode objfpc}{$H+}
{$ifdef windows}

interface

uses
  Classes, windows, SysUtils, paio_mmdevice, pa_base, pa_register;


type

  { TPAMMDestination }

  TPAMMDestination = class(TPAAudioDestination)
  private
    FEnum: TMMDeviceEnumerator;
    FDevice: TMMDevice;
    FClient: TAudioClient;
    FRender: TAudioRenderClient;
    FMixFormat: PWaveFormatEx;
    FBufferSize: Integer;
    procedure Init;
    procedure DeInit;
  protected
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; override;

  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TPAMMDestination }

procedure TPAMMDestination.Init;
var
  lFormat: TWaveFormatEx;
begin
  if Assigned(FRender) then
    Exit;
  WriteLn('init');

  FClient := FDevice.ActivateClient;
  FClient.Initialize(AUDCLNT_SHAREMODE_SHARED, 0, 10000000, 0, FMixFormat);
  FBufferSize := FClient.BufferSize;

  FRender := FClient.GetRenderClient;
  WriteLn('init end');
end;

procedure TPAMMDestination.DeInit;
begin
  if not Assigned(FRender) then
    Exit;

  FClient.Stop;
  FreeAndNil(FRender);

end;

function TPAMMDestination.InternalProcessData(const AData; ACount: Int64;
  AIsLastData: Boolean): Int64;
var
  lmmData: PByte;
  lFrameCount, lDuration: Integer;
  lSpaceAvailable, lGetAmount: DWORD;
  lDataSize: DWORD;
  lData: PByte;
  lStart: Boolean;
begin
  Result := ACount;

  if not Assigned(FDevice) then
    Exit;

  lStart:=False;
  if not Assigned(FRender) then
  begin
    lStart := True;
    Init;
  end;

  lData := @AData;


//  WriteLn('loop');
  while ACount > 0 do
  begin
    lFrameCount := ACount div BytesPerSample(Format) div Channels;
    if lStart then
      lSpaceAvailable:=FBufferSize
    else
      lSpaceAvailable := FClient.CurrentPadding;

    lGetAmount := lSpaceAvailable;
    if lSpaceAvailable > lFrameCount then
      lGetAmount := lFrameCount;

    if FRender.GetBuffer(lGetAmount, @lmmData) then
    begin
      lDataSize := lGetAmount * Channels * BytesPerSample(Format);
      Move(lData^, lmmData^, lDataSize);
      FRender.ReleaseBuffer(lGetAmount, 0);
      Inc(lData, lDataSize);
      Dec(ACount, lDataSize);
      if lStart then
      begin
        WriteLn('start');
        FClient.start;
        lStart:=False;
      end;
      lDuration := lGetAmount * (1000 div SamplesPerSecond);
      Sleep(1);//lDuration div 2);
    end;
  end;
  //sleep(100);
//  WriteLn('loop end');

  if AIsLastData then
    DeInit;
end;

constructor TPAMMDestination.Create;
begin
  inherited Create;
  FEnum := TMMDeviceEnumerator.Create;
  FDevice := FEnum.GetDefaultAudioEndpoint(eRender, eMultimedia);
  FClient := FDevice.ActivateClient;
  FClient.GetMixFormat(FMixFormat);
  Channels := FMixFormat^.nChannels;
  SamplesPerSecond := FMixFormat^.nSamplesPerSec;
  Format:= afFloat32;
end;

destructor TPAMMDestination.Destroy;
begin
  DeInit;
  FreeAndNil(FClient);
  FreeAndNil(FDevice);
  FreeAndNil(FEnum);
  inherited Destroy;
  Writeln('Destroyed Device');
end;

initialization
  PARegister(partDeviceOut, TPAMMDestination, 'MMDevice');

{$else}
interface
implementation
{$endif}
end.

