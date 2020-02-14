unit paio_mmdevice;
{$mode objfpc}{$H+}
{$IFDEF WINDOWS}

{$INTERFACES COM}

interface

uses
  Classes, SysUtils, windows, shlobj, ActiveX;

const
  IID_IAudioClient : GUID = (Data1: $1cb9ad4c; Data2: $dbfa; Data3: $4c32; Data4: ($b1,$78, $c2,$f5,$68,$a7,$03,$b2));
  IID_IAudioRenderClient : GUID = ( Data1: $f294acfc; Data2: $3146; Data3: $4483; Data4: ($a7,$bf, $ad,$dc,$a7,$c2,$60,$e2));

  IID_IMMNotificationClient: TGUID = (Data1: $7991eec9; Data2: $7e89;Data3: $4d85; Data4: ($83,$90,$6c,$70,$3c,$ec,$60,$c0));
  IID_IMMDevice: TGUID = ( Data1: $d666063f; Data2: $1587; Data3: $4e43; Data4: ($81,$f1, $b9,$48,$e8,$07,$36,$3f));

  IID_IMMDeviceCollection: TGUID = (Data1: $0bd7a1be; Data2: $7a1a; Data3: $44db; Data4: ($83,$97,$cc,$53,$92,$38,$7b,$5e));

  CLSID_MMDeviceEnumerator: TGUID = (Data1: $bcde0395; Data2: $e52f; Data3: $467c; Data4: ($8e,$3d, $c4,$57,$92,$91,$69,$2e));
  IID_IMMDeviceEnumerator: TGUID = (Data1: $a95664d2; Data2: $9614; Data3: $4f35; Data4: ($a7, $46, $de, $8d, $b6, $36, $17, $e6));

type
  TEDataFlow = (eRender = 0, eCapture = 1, eAll = 2);
  TERole = (eConsole = 0, eMultimedia = 1, eCommunications = 2);

  PIPropertyStore = ^IPropertyStore;

  AUDCLNT_SHAREMODE = (AUDCLNT_SHAREMODE_SHARED = 0, AUDCLNT_SHAREMODE_EXCLUSIVE = 1);

  IAudioClient = interface(IUnknown)
  ['{1cb9ad4c-dbfa-4c32-b178-c2f568a703b2}']
    function Initialize(ShareMode: AUDCLNT_SHAREMODE; StreamFlags: DWORD; hnsBufferDuration: Int64; hnsPeriodicity: Int64; pFormat: PWAVEFORMATEX; AudioSessionGuid: LPGUID): HRESULT; stdcall;
    function GetBufferSize(pNumBufferFrames: PUInt32): HRESULT; stdcall;
    function GetStreamLatency(phnsLatency: PInt64): HRESULT; stdcall;
    function GetCurrentPadding(pNumPaddingFrames: PUINT32): HRESULT; stdcall;
    function IsFormatSupported(ShareMode: AUDCLNT_SHAREMODE; pFormat: PWaveFormatEx;  out ppClosestMatch: PWaveFormatEx): HRESULT; stdcall;
    function GetMixFormat(out ppDeviceFormat: PWAVEFORMATEX):HRESULT; stdcall;
    function GetDevicePeriod(phnsDefaultDevicePeriod: PInt64; phnsMinimumDevicePeriod: PInt64):HRESULT; stdcall;
    function Start: HRESULT; stdcall;
    function Stop: HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function SetEventHandle(eventHandle: HANDLE): HRESULT; stdcall;
    function GetService(riid: REFIID; ppv: PPointer): HRESULT; stdcall;
  end;

  IAudioRenderClient = interface(IUnknown)
  ['{f294acfc-3146-4483-a7bf-addca7c260e2}']
    function GetBuffer(NumFramesRequested: UINT32; ppData: PPByte): HRESULT; stdcall;
    function ReleaseBuffer(NumFramesWritten: UINT32; dwFlags: DWORD): HRESULT; stdcall;
  end;

  PIMMNotificationClient = ^IMMNotificationClient;
  IMMNotificationClient = interface(IUnknown)
  ['{7991eec9-7e89-4d85-8390-6c703cec60c0}']

   function OnDeviceStateChanged(pwstrDeviceId: LPCWSTR; dwNewState: DWORD): HRESULT; stdcall;
   function OnDeviceAdded(pwstrDeviceId: LPCWSTR): HResult; stdcall;
   function OnDeviceRemoved(pwstrDeviceId: LPCWSTR): HResult; stdcall;
   function OnDefaultDeviceChanged(flow: TEDataFlow; role: TERole; pwstrDeviceId: LPCWSTR): HResult; stdcall;
   function OnPropertyValueChanged(pwstrDeviceId: LPCWSTR; key: PROPERTYKEY): HResult; stdcall;

  end;

  PPIMMDevice = ^PIMMDevice;
  PIMMDevice = ^IMMDevice;
  IMMDevice = interface(IUnknown)
  ['{d666063f-1587-4e43-81f1-b948e807363f}']
    function Activate(iid:  GUID; dwClsCtx: DWORD;pActivationParams: PPROPVARIANT; ppv:PPointer): HRESULT; stdcall;
    function OpenPropertyStore(stgmAccess: DWORD; ppProperties : PIPropertyStore{array}): HResult; stdcall;
    function GetId(ppstrId: PLPWStr): HRESULT; stdcall;
    function GetState(pdwState: PDWORD): HRESULT; stdcall;
  end;


  PPIMMDeviceCollection = ^PIMMDeviceCollection;
  PIMMDeviceCollection = ^IMMDeviceCollection;
  IMMDeviceCollection = interface(IUnknown)
  ['{0bd7a1be-7a1a-44db-8397-cc5392387b5e}']
    function GetCount(pcDevices: PUINT): HRESULT; stdcall;
    function Item(nDevice: UINT; ppDevice: PPIMMDevice): HRESULT; stdcall;
  end;



  IMMDeviceEnumerator = interface(IUnknown)
  ['{a95664d2-9614-4f35-a746-de8db63617e6}']
    function EnumAudioEndpoints(dataFlow: TEDataFlow; dwStateMask: DWORD; ppDevices: PPIMMDeviceCollection): HResult; stdcall;
    function GetDefaultAudioEndpoint(dataFlow: TEDataFlow; role: TERole; ppEndpoint: PPIMMDevice): HResult; stdcall;
    function GetDevice(pwstrId: LPCWSTR; ppDevice: PPIMMDevice): HResult; stdcall;
    function RegisterEndpointNotificationCallback(pClient: PIMMNotificationClient ): HREsult; stdcall;
    function UnregisterEndpointNotificationCallback(pClient: PIMMNotificationClient ): HREsult; stdcall;
  end;

  { TAudioRenderClient }

  TAudioRenderClient = class
  private
    FIntf: IAudioRenderClient;
    constructor Create(Aintf: IAudioRenderClient);
  public
    destructor Destroy; override;
    function  GetBuffer(NumFramesRequested: UINT32; ppData: PPByte): Boolean;
    procedure ReleaseBuffer(NumFramesWritten: UINT32; dwFlags: DWORD);

  end;

  { TAudioClient }

  TAudioClient = class
  private
    FIntf: IAudioClient;
    constructor Create(AIntf: IAudioClient);
    function GetBufferSize: DWord;
    function GetCurrentPadding: DWord;
  public
    destructor Destroy; override;
    procedure GetMixFormat(out AFormat: PWaveFormatEx);
    procedure Initialize(ShareMode: AUDCLNT_SHAREMODE; StreamFlags: DWORD; hnsBufferDuration: Int64; hnsPeriodicity: Int64; pFormat: PWAVEFORMATEX; AudioSessionGuid: LPGUID = nil);
    function GetRenderClient: TAudioRenderClient;
    procedure Start;
    procedure Stop;
    property BufferSize: DWord read GetBufferSize; { in frames }
    property CurrentPadding: DWord read GetCurrentPadding; { in frames }
  end;

  { TMMDevice }

  TMMDevice = class
  private
    FInterface: IMMDevice;
    constructor Create(AInterface: IMMDevice);
    function GetState: Dword;
  public
    destructor Destroy; override;
    function ActivateClient: TAudioClient;
    property State: Dword read GetState;
  end;

  { TMMDeviceEnumerator }

  TMMDeviceEnumerator = class
  private
    FEnumerator: IMMDeviceEnumerator;
  public
    function GetDefaultAudioEndpoint(ADataFlow: TEDataFlow; ARole : TERole): TMMDevice;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

procedure deleteme;
begin
  end;

{ TAudioRenderClient }

constructor TAudioRenderClient.Create(Aintf: IAudioRenderClient);
begin
  FIntf := Aintf;
end;

destructor TAudioRenderClient.Destroy;
begin
  FIntf._Release;
  inherited Destroy;
end;

function TAudioRenderClient.GetBuffer(NumFramesRequested: UINT32; ppData: PPByte): Boolean;
begin
  Result := FIntf.GetBuffer(NumFramesRequested, ppData) = S_OK;
end;

procedure TAudioRenderClient.ReleaseBuffer(NumFramesWritten: UINT32; dwFlags: DWORD);
begin
  FIntf.ReleaseBuffer(NumFramesWritten, dwFlags);
end;

{ TAudioClient }

constructor TAudioClient.Create(AIntf: IAudioClient);
begin
  FIntf := AIntf;
end;

function TAudioClient.GetBufferSize: DWord;
begin
  FIntf.GetBufferSize(@Result);
end;

function TAudioClient.GetCurrentPadding: DWord;
begin
  FIntf.GetCurrentPadding(@Result);
end;

destructor TAudioClient.Destroy;
begin
  FIntf._Release;
  inherited Destroy;
end;

procedure TAudioClient.GetMixFormat(out AFormat: PWaveFormatEx);
begin
  FIntf.GetMixFormat(AFormat);
end;

procedure TAudioClient.Initialize(ShareMode: AUDCLNT_SHAREMODE;
  StreamFlags: DWORD; hnsBufferDuration: Int64; hnsPeriodicity: Int64;
  pFormat: PWAVEFORMATEX; AudioSessionGuid: LPGUID);
begin
  FIntf.Initialize(ShareMode, StreamFlags, hnsBufferDuration, hnsPeriodicity, pFormat, AudioSessionGuid);
end;

function TAudioClient.GetRenderClient: TAudioRenderClient;
var
  lIntf: IAudioRenderClient;
begin
  Result := nil;
  if FIntf.GetService(IID_IAudioRenderClient, @lIntf) = S_OK then
    Result := TAudioRenderClient.Create(lIntf);
end;

procedure TAudioClient.Start;
begin
  FIntf.Start;
end;

procedure TAudioClient.Stop;
begin
  FIntf.Stop;
end;

{ TMMDevice }

constructor TMMDevice.Create(AInterface: IMMDevice);
begin
  FInterface := AInterface;
end;

function TMMDevice.GetState: Dword;
begin
  FInterface.GetState(@Result);
end;

destructor TMMDevice.Destroy;
begin
  FInterface._Release;
  inherited Destroy;
end;

function TMMDevice.ActivateClient: TAudioClient;
var
  lIntf: IAudioClient;
begin
  Result := nil;
  if FInterface.Activate(IID_IAudioClient, CLSCTX_ALL, nil, @lIntf) = S_OK then
    Result := TAudioClient.Create(lIntf);
end;

{ TMMDeviceEnumerator }

constructor TMMDeviceEnumerator.Create;
begin
  if not CoCreateInstance(CLSID_MMDeviceEnumerator, nil, CLSCTX_ALL, IID_IMMDeviceEnumerator,FEnumerator) = S_OK then
    Raise Exception.Create('Unable to create instance of IMMDeviceEnumerator');
end;

destructor TMMDeviceEnumerator.Destroy;
begin
  FEnumerator._Release;
  inherited Destroy;
end;

function TMMDeviceEnumerator.GetDefaultAudioEndpoint(ADataFlow: TEDataFlow; ARole: TERole): TMMDevice;
var
  lSuccess: HRESULT;
  lIntf: IMMDevice;
begin
  Result := nil;
  if FEnumerator.GetDefaultAudioEndpoint(ADataFlow, ARole, @lIntf) = S_OK then
    Result := TMMDevice.Create(lIntf);
end;
initialization
  CoInitializeEx(nil, COINIT_MULTITHREADED);


{$ELSE}
interface
implementation
{$ENDIF}
end.

