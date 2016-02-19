{
    This unit is part of the PascalAudio project.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and license.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit ladspa_classes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ladspa, dynlibs;

type

  ELADSPANilMethod = class(Exception);

  TLADSPADescriptor = class;

  { TLADSPALib }

  TLADSPALib = class
  private
    FLib: TLibHandle;
    FQuery: LADSPA_DescriptorFunction;
    FDescriptors: TList;
    function GetDescriptor(AIndex: Integer): TLADSPADescriptor;
    function GetLoaded: Boolean;
    procedure LoadDescriptors;
  public
    constructor Create(ALibName: String);
    destructor Destroy; override;
    function Count: Integer;
    property Descriptor[AIndex: Integer]: TLADSPADescriptor read GetDescriptor;
    property Loaded: Boolean read GetLoaded;
  end;


  TLADSPAInstance = class;
  TLADSPAInstanceClass = Class of TLADSPAInstance;
  { TLADSPADescriptor }

  TLADSPADescriptor = class
  public
    type
       TPortDescriptor = (pdInputAudio, pdInputControl, pdOutputAudio, pdOutputControl);
       TPortType = (ptAudio, ptFloat, ptToggle, ptSampleRate, ptLogarithmic, ptInteger);

       { TPortHint }

       TPortHint = class
       private
         FDescriptor: LADSPA_PortDescriptor;
         FHint: PLADSPA_PortRangeHint;
         FName: PChar;
         FIndex: Integer;
         function GetDataType: TPortType;
         function GetDefaultToggle: Boolean;
         function GetDescriptor: TPortDescriptor;
         function GetFloatDefault: Single;
         function GetHasDefault: Boolean;
         function GetHasHighBounds: Boolean;
         function GetHasLowBounds: Boolean;
         function FloatGetHighBounds: Integer;
         function GetIntegerDefault: Integer;
         function GetIntegerHighBounds: Integer;
         function GetIntegerLowBounds: Integer;
         function GetLogarithmicDefault: Single;
         function GetLogarithmicLowBounds: Single;
         function GetLogarthimicHighBounds: Single;
         function FloatGetLowBounds: Integer;
         function GetName: String;
         function GetSampleRateDefault(ASampleRate: Integer): Integer;
         function GetSampleRateHighBounds(ASampleRate: Integer): Integer;
         function GetSampleRateLowBounds(ASampleRate: Integer): Integer;
       public
         constructor Create(AIndex: Integer; ADescriptor: LADSPA_PortDescriptor; AName: PChar; AHint: PLADSPA_PortRangeHint);
         property Descriptor: TPortDescriptor read GetDescriptor;
         property DataType: TPortType read GetDataType;
         property Name: String read GetName;
         property HasLowBounds: Boolean read GetHasLowBounds;
         property HasHighBounds: Boolean read GetHasHighBounds;
         property HasDefault: Boolean read GetHasDefault;
         property DefaultToggle: Boolean read GetDefaultToggle;
         property DefaultSampleRate[ASampleRate: Integer]: Integer read GetSampleRateDefault;
         property DefaultLogarithmic: Single read GetLogarithmicDefault;
         property DefaultInteger: Integer read GetIntegerDefault;
         property DefaultFloat: Single read GetFloatDefault;
         property LowBoundsSampleRate[ASampleRate: Integer]: Integer read GetSampleRateLowBounds;
         property HighBoundsSampleRate[ASampleRate: Integer]: Integer read GetSampleRateHighBounds;
         property LowBoundsLogarithmic: Single read GetLogarithmicLowBounds;
         property HighBoundsLogarthimic: Single read GetLogarthimicHighBounds;
         property HighBoundsInteger: Integer read GetIntegerHighBounds;
         property LowBoundsInteger: Integer read GetIntegerLowBounds;
         property HighBoundsFloat: Integer read FloatGetHighBounds;
         property LowBoundsFloat: Integer read FloatGetLowBounds;
       end;
  private
    FDescriptor: PLADSPA_Descriptor;
    FPorts: TList;
    function GetCopyright: String;
    function GetImplementationData: Pointer;
    function GetLabel: String;
    function GetMaker: String;
    function GetName: String;
    function GetPort(AIndex: Integer): TPortHint;
    function GetPortCount: Integer;
    function GetPropertyCount: Integer;
    function GetUniqueID: DWord;
    procedure LoadPortsHints;
  public
    constructor Create(ADescriptor: PLADSPA_Descriptor);
    destructor  Destroy; override;
    function CreateInstance(ASampleRate: DWord; AClass: TLADSPAInstanceClass = nil): TLADSPAInstance;
    property UniqueID: DWord read GetUniqueID;
    property Label_: String read GetLabel;
    property PropertyCount: Integer read GetPropertyCount;
    property Name: String read GetName;
    property Maker: String read GetMaker;
    property Copyright: String read GetCopyright;
    property PortCount: Integer read GetPortCount;
    property Port[AIndex: Integer]: TPortHint read GetPort;
    property ImplementationData: Pointer read GetImplementationData;
    property Descriptor: PLADSPA_Descriptor read FDescriptor;
  end;



  { TLADSPAInstance }

  TLADSPAInstance = class
  public
    type

      { TPort }

      TPort = class(TLADSPADescriptor.TPortHint)
      private
        FInstance: TLADSPAInstance;
        FInternalValue: LADSPA_Data;
        procedure InitValue;
      public
        constructor Create(AInstance: TLADSPAInstance; AIndex: Integer; ADescriptor: LADSPA_PortDescriptor; AName: PChar; AHint: PLADSPA_PortRangeHint);
        procedure SetValue(AValue: Integer);
        procedure SetValue(AValue: Single);
        procedure SetValue(AValue: Boolean);
        procedure SetValue(AValue: PSingle);
      end;
    TPortArray = array of TPort;
  private
    FClass: TLADSPADescriptor;
    FInstanceValid: Boolean;
    FSampleRate: DWord;
    FPluginHandle: LADSPA_Handle;
    FPorts: TList;
    FActivated: Boolean;
    function GetAudioInputs: TPortArray;
    function GetAudioOutputs: TPortArray;
    function GetPort(AIndex: Integer): TPort;
    function GetPortCount: Integer;
    function GetRunAddingAvailable: Boolean;
    procedure LoadPorts;
  protected
    procedure ConnectPort(APort: DWord; ADataLocation: PSingle);
    // override this for any child class for a specific plugin.
    // Otherwise the constructor won't know which descriptor to use to create an instance.
    class function  GetUniqueID: DWord; virtual;
    class constructor Create;
  public
    // this constructor is only useful for children with GetUniqueID overridden.
    constructor Create(ASampleRate: DWord); virtual;

    constructor Create(ADescriptor: TLADSPADescriptor; ASampleRate: DWord); virtual;
    class function TryCreate(AUniqueID: DWord; ASampleRate: DWord): TLADSPAInstance;
    destructor  Destroy; override;

    procedure Activate;
    procedure Deactivate;
    procedure Reset;
    procedure Run(ASampleCount: DWord);
    procedure RunAdding(ASampleCount: DWord);
    procedure SetRunAddingGain(AGain: Single);

    property Klass: TLADSPADescriptor read FClass;
    property Handle: LADSPA_Handle read FPluginHandle;
    property PortCount: Integer read GetPortCount;
    property Port[AIndex: Integer]: TPort read GetPort;
    property AudioInputs: TPortArray read GetAudioInputs;
    property AudioOutputs: TPortArray read GetAudioOutputs;
    property InstanceValid: Boolean read FInstanceValid;
    property RunAddingAvailable: Boolean read GetRunAddingAvailable;
  end;

{$IFDEF UNIX}
const
  DefaultLibExtension = '.so';
{$ENDIF}
{$IFDEF MSWINDOWS}
const
  DefaultLibExtension = '.dll';
{$ENDIF}


// extension is the full extension ".so"
procedure LoadEffectsDir(ADirectory: String; AExtension: String = DefaultLibExtension);
procedure LoadEffectsLibrary(AFileName: String);

procedure RegisterEffectDescriptor(AUniqueID: DWord; ADescriptor: TLADSPADescriptor);
procedure RegisterEffectClass(AUniqueID: DWord; AEffectClass: TLADSPAInstanceClass);
function  FindEffectClass(AUniqueID: DWord): TLADSPAInstanceClass;
function  FindEffectClass(AUniqueID: DWord; out ADescriptor: TLADSPADescriptor): TLADSPAInstanceClass;
function  FindEffectDescriptor(AUniqueID: DWord): TLADSPADescriptor;

// if the plugin is loaded then it will create a instance. The instance class will
// be a custom class if one is registered. Otherwise it will be TLADSPAInstance.
function  CreateEffectClass(AUniqueID: DWord; ASampleRate: Integer): TLADSPAInstance;

implementation

uses
  AVL_Tree;

var
  lLoadedLibs: TList;
  lRegisteredIds: TAVLTree;
  lRegisteredClasses: TAVLTree;

type
  { TRegisterNode }

  TRegisterNode = class(TAVLTreeNode)
  private
    function GetUniqueID: DWord;
    procedure SetUniqueID(AValue: DWord);
  public
    Obj: TObject;
    Klass: TLADSPAInstanceClass;
    property UniqueID: DWord read GetUniqueID write SetUniqueID;
  end;

function TRegisterNode.GetUniqueID: DWord;
begin
  Result := PtrUint(Data);
end;

procedure TRegisterNode.SetUniqueID(AValue: DWord);
begin
  Data := Pointer(PtrUint(AValue));
end;

procedure LoadEffectsDir(ADirectory: String; AExtension: String);
var
  R: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(ADirectory)+'*'+AExtension, faAnyFile, R) = 0 then
  repeat
    LoadEffectsLibrary(IncludeTrailingPathDelimiter(ADirectory)+R.Name);
  until FindNext(R) <> 0;
  FindClose(R);
end;

procedure LoadEffectsLibrary(AFileName: String);
var
  l: TLADSPALib;
  i: Integer;
begin
  l := TLADSPALib.Create(AFileName);
  if l.Loaded then
  begin
    lLoadedLibs.Add(l);
    //WriteLn('Loaded ', AFileName);

    for i := 0 to l.Count-1 do
    begin
      RegisterEffectDescriptor(l.Descriptor[i].UniqueID, l.Descriptor[i]);
    end;
  end
  else
    l.Free;
end;

procedure RegisterEffectDescriptor(AUniqueID: DWord; ADescriptor: TLADSPADescriptor);
var
  Existing: TLADSPADescriptor;
  N : TRegisterNode;
begin
  Existing := FindEffectDescriptor(AUniqueID);
  if Assigned(Existing) then
  begin
    // raise exception?
    exit;
  end;

  N := TRegisterNode.Create;
  N.UniqueID := AUniqueID;
  N.Obj:=ADescriptor;

  lRegisteredIds.Add(N);
end;

procedure RegisterEffectClass(AUniqueID: DWord;
  AEffectClass: TLADSPAInstanceClass);
var
  Existing: TLADSPAInstanceClass;
  D: TLADSPADescriptor;
  N : TRegisterNode;
begin
  Existing := FindEffectClass(AUniqueID, D);
  if Assigned(Existing) then
  begin
    // raise exception?
    exit;
  end;

  N := TRegisterNode.Create;
  N.UniqueID := AUniqueID;
  N.Klass:=AEffectClass;

  lRegisteredClasses.Add(N);
end;

function FindEffectClass(AUniqueID: DWord): TLADSPAInstanceClass;
var
  Dummy: TLADSPADescriptor;
begin
  Result := FindEffectClass(AUniqueID, Dummy);
end;

function FindEffectClass(AUniqueID: DWord; out ADescriptor: TLADSPADescriptor): TLADSPAInstanceClass;
var
  DescNode : TRegisterNode;
  ClassNode : TRegisterNode;
begin
  Result := nil;
  DescNode := TRegisterNode(lRegisteredIds.Find(Pointer(PtrUint(AUniqueID))));
  if Assigned(DescNode) then
  begin
    ADescriptor := TLADSPADescriptor(DescNode.Obj);

    // see if we have connected the class to the descriptor already so we can save a lookup in the registered classes.
    if DescNode.Klass <> nil then
      Exit(DescNode.Klass);

    ClassNode := TRegisterNode(lRegisteredClasses.Find(Pointer(PtrUint(AUniqueID))));
    if Assigned(ClassNode) then
    begin
      Result := TLADSPAInstanceClass(ClassNode.Obj);
      ClassNode.Obj := DescNode.Obj; // setting this could be useful at some point
    end;
    DescNode.Klass:=Result; // speedup future lookups
  end;
end;

function FindEffectDescriptor(AUniqueID: DWord): TLADSPADescriptor;
var
  N : TRegisterNode;
begin
  Result := nil;
  N := TRegisterNode(lRegisteredIds.Find(Pointer(PtrUint(AUniqueID))));

  if Assigned(N) then
  begin
    Result := TLADSPADescriptor(N.Obj);
  end;
end;

function CreateEffectClass(AUniqueID: DWord; ASampleRate: Integer): TLADSPAInstance;
var
  klass: TLADSPAInstanceClass;
  descriptor: TLADSPADescriptor;
begin
  Result := nil;
  klass := FindEffectClass(AUniqueID, descriptor);
  if Assigned(klass) then
  begin
    Result := klass.Create(descriptor, ASampleRate);
  end;
end;

{ TLADSPAInstance.TPort }

procedure TLADSPAInstance.TPort.InitValue;
begin
  if HasDefault then
  begin
    case DataType of
      TLADSPADescriptor.TPortType.ptFloat: SetValue(DefaultFloat);
      TLADSPADescriptor.TPortType.ptLogarithmic: SetValue(DefaultLogarithmic);
      TLADSPADescriptor.TPortType.ptInteger: SetValue(DefaultInteger);
      TLADSPADescriptor.TPortType.ptSampleRate: SetValue(DefaultSampleRate[FInstance.FSampleRate]);
      TLADSPADescriptor.TPortType.ptToggle: SetValue(DefaultToggle);
    end;
  end;

  if DataType <> TLADSPADescriptor.TPortType.ptAudio then
  begin
    FInstance.ConnectPort(FIndex, @FInternalValue);
  end;
end;

constructor TLADSPAInstance.TPort.Create(AInstance: TLADSPAInstance;
  AIndex: Integer; ADescriptor: LADSPA_PortDescriptor; AName: PChar;
  AHint: PLADSPA_PortRangeHint);
begin
  inherited Create(AIndex, ADescriptor, AName, AHint);
  FInstance := AInstance;
  InitValue;
end;

procedure TLADSPAInstance.TPort.SetValue(AValue: Integer);
begin
  if HasHighBounds and (AValue > GetIntegerHighBounds) then
    AValue := GetIntegerHighBounds;
  if HasLowBounds and (AValue < GetIntegerLowBounds) then
    AValue := GetIntegerLowBounds;
  FInternalValue:=AValue;
end;

procedure TLADSPAInstance.TPort.SetValue(AValue: Single);
begin
  if HasHighBounds and (AValue > FloatGetHighBounds) then
    AValue := FloatGetHighBounds;
  if HasLowBounds and (AValue < FloatGetLowBounds) then
    AValue := FloatGetLowBounds;
  FInternalValue:=AValue;
end;

procedure TLADSPAInstance.TPort.SetValue(AValue: Boolean);
begin
  FInternalValue := Ord(AValue);
end;

procedure TLADSPAInstance.TPort.SetValue(AValue: PSingle);
begin
  FInstance.ConnectPort(FIndex, AValue);
end;

{ TLADSPADescriptor.TPortHint }

function TLADSPADescriptor.TPortHint.GetDefaultToggle: Boolean;
begin
  Result := (FHint^.HintDescriptor and LADSPA_HINT_DEFAULT_1) <> 0;
end;

function TLADSPADescriptor.TPortHint.GetDataType: TPortType;
begin
  if FHint^.HintDescriptor and LADSPA_HINT_TOGGLED <> 0 then
    Exit(ptToggle)
  else if FHint^.HintDescriptor and LADSPA_HINT_LOGARITHMIC <> 0 then
    Exit(ptLogarithmic)
  else if FHint^.HintDescriptor and LADSPA_HINT_INTEGER <> 0 then
    Exit(ptInteger)
  else if FHint^.HintDescriptor and LADSPA_HINT_SAMPLE_RATE <> 0 then
    Exit(ptSampleRate)
  else
    Result := ptFloat;

  if (Result = ptFloat) and (Descriptor in [pdInputAudio, pdOutputAudio]) then
    Result := ptAudio;
end;

function TLADSPADescriptor.TPortHint.GetDescriptor: TPortDescriptor;
begin
  case FDescriptor and (LADSPA_PORT_INPUT or LADSPA_PORT_OUTPUT) of
    LADSPA_PORT_INPUT   : Result := pdInputAudio;
    LADSPA_PORT_OUTPUT  : Result := pdOutputAudio;
  end;
  if FDescriptor and LADSPA_PORT_CONTROL > 0 then
    Result := TPortDescriptor(Ord(Result)+1);
end;

function TLADSPADescriptor.TPortHint.GetFloatDefault: Single;
var
  Mask: LADSPA_PortRangeHintDescriptor;
begin
  Result := 0;
  Mask := FHint^.HintDescriptor and LADSPA_HINT_DEFAULT_MASK;
  if Mask and LADSPA_HINT_DEFAULT_0 <> 0 then
    Exit(0)
  else if Mask and LADSPA_HINT_DEFAULT_1 <> 0 then
    Exit(1)
  else if Mask and LADSPA_HINT_DEFAULT_100 <> 0 then
    Exit(100)
  else if Mask and LADSPA_HINT_DEFAULT_MINIMUM <> 0 then
    Exit(FHint^.LowerBound)
  else if Mask and LADSPA_HINT_DEFAULT_MAXIMUM <> 0 then
    Exit(FHint^.UpperBound)
  else if Mask and LADSPA_HINT_DEFAULT_LOW <> 0 then
    Exit(FHint^.LowerBound * 0.75 + FHint^.UpperBound * 0.25)
  else if Mask and LADSPA_HINT_DEFAULT_MIDDLE <> 0 then
    Exit(FHint^.LowerBound * 0.5 + FHint^.UpperBound * 0.5)
  else if Mask and LADSPA_HINT_DEFAULT_HIGH <> 0 then
    Exit(FHint^.LowerBound * 0.25 + FHint^.UpperBound * 0.75)
  // probably only for samplerate but anyway...
  else if Mask and LADSPA_HINT_DEFAULT_440 <> 0 then
    Exit(440);

end;

function TLADSPADescriptor.TPortHint.GetHasDefault: Boolean;
begin
  Result := FHint^.HintDescriptor and LADSPA_HINT_DEFAULT_MASK <> 0;
end;

function TLADSPADescriptor.TPortHint.GetHasHighBounds: Boolean;
begin
  Result := FHint^.HintDescriptor and LADSPA_HINT_BOUNDED_ABOVE <> 0;
end;

function TLADSPADescriptor.TPortHint.GetHasLowBounds: Boolean;
begin
  Result := FHint^.HintDescriptor and LADSPA_HINT_BOUNDED_BELOW <> 0;
end;

function TLADSPADescriptor.TPortHint.FloatGetHighBounds: Integer;
begin
  Result := Trunc(FHint^.UpperBound);
end;

function TLADSPADescriptor.TPortHint.GetIntegerDefault: Integer;
begin
  Result := Trunc(GetFloatDefault);
end;

function TLADSPADescriptor.TPortHint.GetIntegerHighBounds: Integer;
begin
  Result := Trunc(FHint^.UpperBound);
end;

function TLADSPADescriptor.TPortHint.GetIntegerLowBounds: Integer;
begin
  Result := Trunc(FHint^.LowerBound);
end;

function TLADSPADescriptor.TPortHint.GetLogarithmicDefault: Single;
var
  Mask: LADSPA_PortRangeHintDescriptor;
begin
  Result := 0;
  Mask := FHint^.HintDescriptor and LADSPA_HINT_DEFAULT_MASK;
  if Mask and LADSPA_HINT_DEFAULT_MINIMUM <> 0 then
    Exit(LowBoundsLogarithmic)
  else if Mask and LADSPA_HINT_DEFAULT_MAXIMUM <> 0 then
    Exit(HighBoundsLogarthimic)
  else if Mask and LADSPA_HINT_DEFAULT_LOW <> 0 then
    Exit(exp(ln(FHint^.LowerBound) * 0.75 + ln(FHint^.UpperBound) * 0.25))
  else if Mask and LADSPA_HINT_DEFAULT_MIDDLE <> 0 then
    Exit(exp(ln(FHint^.LowerBound) * 0.5 + ln(FHint^.UpperBound) * 0.5))
  else if Mask and LADSPA_HINT_DEFAULT_HIGH <> 0 then
    Exit(exp(ln(FHint^.LowerBound) * 0.25 + ln(FHint^.UpperBound) * 0.75));

end;

function TLADSPADescriptor.TPortHint.GetLogarithmicLowBounds: Single;
begin
  Result := ln(FHint^.LowerBound);
end;

function TLADSPADescriptor.TPortHint.GetLogarthimicHighBounds: Single;
begin
  Result := ln(FHint^.UpperBound);
end;

function TLADSPADescriptor.TPortHint.FloatGetLowBounds: Integer;
begin
  Result := Trunc(FHint^.LowerBound);
end;

function TLADSPADescriptor.TPortHint.GetName: String;
begin
  Result := FName;
end;

function TLADSPADescriptor.TPortHint.GetSampleRateDefault(ASampleRate: Integer): Integer;
begin
  Result := GetIntegerDefault * ASampleRate;
end;

function TLADSPADescriptor.TPortHint.GetSampleRateHighBounds(
  ASampleRate: Integer): Integer;
begin
  Result := HighBoundsInteger * ASampleRate;
end;

function TLADSPADescriptor.TPortHint.GetSampleRateLowBounds(ASampleRate: Integer
  ): Integer;
begin
  Result := LowBoundsInteger * ASampleRate;
end;

constructor TLADSPADescriptor.TPortHint.Create(AIndex: Integer;
  ADescriptor: LADSPA_PortDescriptor; AName: PChar; AHint: PLADSPA_PortRangeHint
  );
begin
  FIndex := AIndex;
  FDescriptor:=ADescriptor;
  FName:=AName;
  FHint := AHint;
end;

{ TLADSPAInstance }

procedure TLADSPAInstance.LoadPorts;
var
  lPort: TPort;
  i: Integer;
begin
  if not FInstanceValid then
    Exit;
  for i := 0 to FClass.PortCount-1 do
  begin
    lPort := TPort.Create(Self, i, FClass.Port[i].FDescriptor, FClass.Port[i].FName, FClass.Port[i].FHint);
    FPorts.Add(lPort);
  end;
end;

function TLADSPAInstance.GetPort(AIndex: Integer): TPort;
begin
  Result := TPort(FPorts[AIndex]);
end;

function TLADSPAInstance.GetAudioInputs: TPortArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  if not FInstanceValid then
    Exit;
  for i := 0 to FPorts.Count-1 do
  begin
    if Port[i].Descriptor = pdInputAudio then
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := Port[i];

    end;
  end;
end;

function TLADSPAInstance.GetAudioOutputs: TPortArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  if not FInstanceValid then
    Exit;
  for i := 0 to FPorts.Count-1 do
  begin
    if Port[i].Descriptor = pdOutputAudio then
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := Port[i];
    end;
  end;
end;

function TLADSPAInstance.GetPortCount: Integer;
begin
  Result := 0;
  if not FInstanceValid then
    Exit;
  Result := FPorts.Count;
end;

function TLADSPAInstance.GetRunAddingAvailable: Boolean;
begin
  Result := Klass.FDescriptor^.run_adding <> nil;
end;

constructor TLADSPAInstance.Create(ADescriptor: TLADSPADescriptor;
  ASampleRate: DWord);
begin
  FClass := ADescriptor;
  FSampleRate:=ASampleRate;
  FPluginHandle := FClass.FDescriptor^.instantiate(FClass.FDescriptor, FSampleRate);
  FInstanceValid:=True;
  FPorts := TList.Create;
  LoadPorts;
end;

class function TLADSPAInstance.TryCreate(AUniqueID: DWord; ASampleRate: DWord): TLADSPAInstance;
var
  Desc: TLADSPADescriptor;
  PluginClass: TLADSPAInstanceClass;
begin
  Result := nil;
  PluginClass := FindEffectClass(AUniqueId, Desc);
  if PluginClass <> nil then
  begin
    Result := PluginClass.Create(Desc, ASampleRate);
  end
  else if Desc <> nil then
  begin
    // the desc exists but there is no specific class for the plugin type
    // so create a generic one.
    Result := TLADSPAInstance.Create(Desc, ASampleRate);
  end;

end;

procedure TLADSPAInstance.ConnectPort(APort: DWord; ADataLocation: PSingle);
begin
  FClass.FDescriptor^.connect_port(FPluginHandle, APort, ADataLocation);
end;

class function TLADSPAInstance.GetUniqueID: DWord;
begin
  Result := DWord(-1);
end;

class constructor TLADSPAInstance.Create;
begin
  if GetUniqueID <> Dword(-1) then
    RegisterEffectClass(GetUniqueID, TLADSPAInstanceClass(ClassType));
end;

constructor TLADSPAInstance.Create(ASampleRate: DWord);
var
  PluginClass: TLADSPAInstanceClass;
  Desc: TLADSPADescriptor;
  UniqueId: Dword;
begin
  UniqueId := GetUniqueID;
  if UniqueId <> Dword(-1) then
  begin
    PluginClass := FindEffectClass(UniqueId, Desc);
    if PluginClass <> nil then
    begin
      Create(Desc, ASampleRate);
    end;
  end;
end;

procedure TLADSPAInstance.Activate;
begin
  if not FInstanceValid then
    Exit;
  if FActivated then
    Exit;
  FActivated := True;
  if Assigned(FClass.FDescriptor^.activate) then
    FClass.FDescriptor^.activate(FPluginHandle);
end;

procedure TLADSPAInstance.Run(ASampleCount: DWord);
begin
  if not FInstanceValid then
    Exit;
  if not FActivated then
    Activate;
  FClass.FDescriptor^.run(FPluginHandle, ASampleCount);
end;

procedure TLADSPAInstance.RunAdding(ASampleCount: DWord);
begin
  if not FInstanceValid then
    Exit;
  if not FActivated then
    Activate;

  if Assigned(FClass.FDescriptor^.run_adding) then
    FClass.FDescriptor^.run_adding(FPluginHandle, ASampleCount)
  else
    raise ELADSPANilMethod.Create('run_adding not available');

end;

procedure TLADSPAInstance.SetRunAddingGain(AGain: Single);
begin
  if not FInstanceValid then
    Exit;
  if Assigned(FClass.FDescriptor^.set_run_adding_gain) then
    FClass.FDescriptor^.set_run_adding_gain(FPluginHandle, AGain)
  else
    raise ELADSPANilMethod.Create('set_run_adding_gain not available');
end;

procedure TLADSPAInstance.Deactivate;
begin
  if not FActivated then
    Exit;
  FActivated:=False;
  if Assigned(FClass.FDescriptor^.deactivate) then
    FClass.FDescriptor^.deactivate(FPluginHandle);
end;

procedure TLADSPAInstance.Reset;
begin
  Deactivate;
  Activate;
end;

destructor TLADSPAInstance.Destroy;
var
  O: Pointer;
begin
  for O in FPorts do
    TObject(O).Free;
  FPorts.Free;
  FClass.FDescriptor^.cleanup(FPluginHandle);
  inherited Destroy;
end;

{ TLADSPADescriptor }

function TLADSPADescriptor.GetCopyright: String;
begin
  Result := FDescriptor^.Copyright;
end;

function TLADSPADescriptor.GetImplementationData: Pointer;
begin
  Result := FDescriptor^.ImplementationData;
end;

function TLADSPADescriptor.GetLabel: String;
begin
  Result := FDescriptor^.Label_;
end;

function TLADSPADescriptor.GetMaker: String;
begin
  Result := FDescriptor^.Maker;
end;

function TLADSPADescriptor.GetName: String;
begin
  Result := FDescriptor^.Name;
end;

function TLADSPADescriptor.GetPort(AIndex: Integer): TPortHint;
begin
  Result := TPortHint(FPorts[AIndex]);
end;

function TLADSPADescriptor.GetPortCount: Integer;
begin
  Result := FDescriptor^.PortCount;
end;

function TLADSPADescriptor.GetPropertyCount: Integer;
begin
  Result := FDescriptor^.Properties;
end;

function TLADSPADescriptor.GetUniqueID: DWord;
begin
  Result := FDescriptor^.UniqueID;
end;

procedure TLADSPADescriptor.LoadPortsHints;
var
  PortHint: TPortHint;
  i: Integer;
begin
  for i := 0 to PortCount-1 do
  begin
    PortHint := TPortHint.Create(i, FDescriptor^.PortDescriptors[i], FDescriptor^.PortNames[i], @FDescriptor^.PortRangeHints[i]);
    FPorts.Add(PortHint);
  end;
end;

constructor TLADSPADescriptor.Create(ADescriptor: PLADSPA_Descriptor);
begin
  FDescriptor:=ADescriptor;
  FPorts := TList.Create;
  LoadPortsHints;
end;

destructor TLADSPADescriptor.Destroy;
var
  O: Pointer;
begin
  for O in FPorts do
    TObject(O).Free;
  FPorts.Free;
  inherited Destroy;
end;

function TLADSPADescriptor.CreateInstance(ASampleRate: DWord;
  AClass: TLADSPAInstanceClass): TLADSPAInstance;
begin
  if not Assigned(AClass) then
    AClass := TLADSPAInstance;

  Result := AClass.Create(Self, ASampleRate);
end;

{ TLADSPALib }

function TLADSPALib.GetLoaded: Boolean;
begin
  Result := (FLib <> 0) and (FQuery <> nil);
end;


function TLADSPALib.GetDescriptor(AIndex: Integer): TLADSPADescriptor;
begin
  Result := TLADSPADescriptor(FDescriptors[AIndex]);
end;

procedure TLADSPALib.LoadDescriptors;
var
  i: Integer = 0;
  D: PLADSPA_Descriptor;
  O: TLADSPADescriptor;
  //j: Integer;
begin
 repeat
   D := FQuery(i);
   if Assigned(D) then
   begin
     O := TLADSPADescriptor.Create(D);

     FDescriptors.Add(O);
     {WriteLn('Loaded ', O.Label_);
     //if O.Label_ = 'mixer' then
     begin
     for j := 0 to O.PortCount-1 do
       WriteLn('  ', O.Port[j].Name, ' - ', O.Port[j].DataType);

     end;}
   end;
   Inc(i);
 until D = nil;
end;

constructor TLADSPALib.Create(ALibName: String);
begin
  FDescriptors := TList.Create;
  FLib := LoadLibrary(ALibName);
  if FLib <> NilHandle then
  begin
    Pointer(FQuery) := GetProcAddress(FLib, LADSPA_DescriptorExport);
    if Assigned(FQuery) then
      LoadDescriptors;
  end;
end;

destructor TLADSPALib.Destroy;
var
  O: Pointer;
begin
  inherited Destroy;
  if FLib <> NilHandle then
    UnloadLibrary(FLib);
  for O in FDescriptors do
    TObject(O).Free;
  FDescriptors.Free;
end;

function TLADSPALib.Count: Integer;
begin
  Result := FDescriptors.Count;
end;

procedure FreeLibs;
var
  l: TLADSPALib;
  p: pointer absolute l;
begin
   for p in lLoadedLibs do
     l.Free;
   lLoadedLibs.Free;
end;

initialization
  lLoadedLibs := TList.Create;
  lRegisteredIds := TAVLTree.Create;
  lRegisteredClasses := TAVLTree.Create;

finalization

  lRegisteredIds.Free;
  lRegisteredClasses.Free;
  FreeLibs;

end.
