{
    This unit is part of PascalAudioIO package.

    Dynamic binding to libfaac (Freeware Advanced Audio Coder), the AAC *encoder*.
    libfaac is LGPL. The library is loaded at runtime; if it is missing the unit
    still loads and FaacLibraryLoaded returns False.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit paio_faac;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, dynlibs, Math, paio_log;

const
  {$IFDEF UNIX}
  FAAC_LIBNAME = 'libfaac.so.0';
  {$ELSE}
  FAAC_LIBNAME = 'libfaac.dll';
  {$ENDIF}

  FAAC_CFG_VERSION = 105;

  // MPEG version
  FAAC_MPEG4 = 0;
  FAAC_MPEG2 = 1;

  // AAC object types
  FAAC_OBJ_MAIN = 1;
  FAAC_OBJ_LOW  = 2;   // AAC-LC
  FAAC_OBJ_SSR  = 3;
  FAAC_OBJ_LTP  = 4;

  // input sample formats
  FAAC_INPUT_NULL  = 0;
  FAAC_INPUT_16BIT = 1;
  FAAC_INPUT_24BIT = 2;
  FAAC_INPUT_32BIT = 3;
  FAAC_INPUT_FLOAT = 4;

  // output stream format
  FAAC_RAW_STREAM  = 0;
  FAAC_ADTS_STREAM = 1;

type
  faacEncHandle = Pointer;
  PPByteFaac = ^PByte;

  psymodellist_t = packed record
    ptr: Pointer;
    name: PChar;
  end;

  // mirrors faacEncConfiguration (faaccfg.h) -- the struct is #pragma pack(1), so
  // this MUST be a packed record and use C 'unsigned long' (culong = 8 bytes on
  // 64-bit) for bitRate/quantqual or every field past them shifts.
  PfaacEncConfiguration = ^faacEncConfiguration;
  faacEncConfiguration = packed record
    version: cint;
    name: PChar;
    copyright: PChar;
    mpegVersion: cuint;
    aacObjectType: cuint;
    jointmode: cuint;          // union { jointmode; allowMidside }
    useLfe: cuint;
    useTns: cuint;
    bitRate: culong;
    bandWidth: cuint;
    quantqual: culong;
    outputFormat: cuint;
    psymodellist: Pointer;
    psymodelidx: cuint;
    inputFormat: cuint;
    shortctl: cint;
    channel_map: array[0..63] of cint;
    pnslevel: cint;
  end;

var
  faacEncGetVersion: function(faac_id_string, faac_copyright_string: PPChar): cint; cdecl;
  faacEncGetCurrentConfiguration: function(hEncoder: faacEncHandle): PfaacEncConfiguration; cdecl;
  faacEncSetConfiguration: function(hEncoder: faacEncHandle; config: PfaacEncConfiguration): cint; cdecl;
  faacEncOpen: function(sampleRate: culong; numChannels: cuint;
    inputSamples: pculong; maxOutputBytes: pculong): faacEncHandle; cdecl;
  faacEncGetDecoderSpecificInfo: function(hEncoder: faacEncHandle; ppBuffer: PPByteFaac;
    pSizeOfDecoderSpecificInfo: pculong): cint; cdecl;
  faacEncEncode: function(hEncoder: faacEncHandle; inputBuffer: Pointer; samplesInput: cuint;
    outputBuffer: PByte; bufferSize: cuint): cint; cdecl;
  faacEncClose: function(hEncoder: faacEncHandle): cint; cdecl;

  // libfaac allocates the DecoderSpecificInfo buffer with libc malloc; free it
  // through the matching libc free (not FPC's heap manager).
  procedure cfree(P: Pointer); cdecl; external 'c' name 'free';

  procedure LoadFaacLibrary(ALibName: String = '');
  procedure UnloadFaacLibrary;
  function  FaacLibraryLoaded: Boolean;

type

  { TFAACEncoder -- thin wrapper: open, configure for 16-bit AAC-LC, hand it
    interleaved S16 PCM, get back raw AAC access units plus the AudioSpecificConfig
    needed by the MP4 muxer. }

  TFAACEncoder = class
  private
    FHandle: faacEncHandle;
    FSampleRate: Integer;
    FChannels: Integer;
    FInputSamples: culong;    // S16 samples wanted per Encode call (frame*channels)
    FMaxOutputBytes: culong;
    FOutBuf: array of Byte;
    FQuality: LongWord;       // VBR quantizer quality (~10..500, 100 default)
    FBitRate: LongWord;       // per-channel bitrate; 0 => use quality (VBR)
    FConfigured: Boolean;
    procedure ApplyConfig;
  public
    constructor Create(ASampleRate, AChannels: Integer);
    destructor Destroy; override;
    // The AAC frame length in PCM samples per channel (1024 for AAC-LC).
    function SamplesPerFrame: Integer;
    // AudioSpecificConfig (DecoderSpecificInfo) for the container's esds.
    function GetAudioSpecificConfig: TBytes;
    // Encode interleaved S16. AInput points at ASampleCount int16 samples
    // (== frames*channels). Pass nil/0 to flush the encoder's lookahead at EOF.
    // Returns False on error; AFrame may legitimately be empty (encoder delay).
    function Encode(AInput: PSmallInt; ASampleCount: Integer; out AFrame: TBytes): Boolean;
    property SampleRate: Integer read FSampleRate;
    property Channels: Integer read FChannels;
    property InputSampleCount: culong read FInputSamples;
    // set before the first Encode call.
    property Quality: LongWord read FQuality write FQuality;
    property BitRate: LongWord read FBitRate write FBitRate;
  end;

implementation

uses
  fgl;

var
  libHandle: TLibHandle = 0;
  libList: specialize TFPGMap<String, PPointer>;

procedure LoadFaacLibrary(ALibName: String);
var
  i: Integer;
begin
  if libHandle <> 0 then
    UnloadFaacLibrary;

  if ALibName = '' then
    ALibName := FAAC_LIBNAME;

  libHandle := LoadLibrary(ALibName);
  if libHandle = 0 then
  begin
    TPALog.Warning('paio_faac', 'could not load ' + ALibName + '; AAC encoding unavailable');
    Exit;
  end;

  libList := specialize TFPGMap<String, PPointer>.Create;
  libList.Add('faacEncGetVersion', @faacEncGetVersion);
  libList.Add('faacEncGetCurrentConfiguration', @faacEncGetCurrentConfiguration);
  libList.Add('faacEncSetConfiguration', @faacEncSetConfiguration);
  libList.Add('faacEncOpen', @faacEncOpen);
  libList.Add('faacEncGetDecoderSpecificInfo', @faacEncGetDecoderSpecificInfo);
  libList.Add('faacEncEncode', @faacEncEncode);
  libList.Add('faacEncClose', @faacEncClose);

  for i := 0 to libList.Count-1 do
    libList.Data[i]^ := GetProcAddress(libHandle, libList.Keys[i]);
end;

procedure UnloadFaacLibrary;
var
  i: Integer;
begin
  if libHandle = 0 then
    Exit;
  for i := 0 to libList.Count-1 do
    libList.Data[i]^ := nil;
  UnloadLibrary(libHandle);
  FreeAndNil(libList);
  libHandle := 0;
end;

function FaacLibraryLoaded: Boolean;
begin
  Result := libHandle <> 0;
end;

{ TFAACEncoder }

constructor TFAACEncoder.Create(ASampleRate, AChannels: Integer);
begin
  if not FaacLibraryLoaded then
    LoadFaacLibrary;
  if not FaacLibraryLoaded then
  begin
    TPALog.Error(ClassName, 'libfaac not available');
    Exit;
  end;

  // libfaac does float math that can produce NaN/inf; FPC leaves FPU exceptions
  // unmasked by default, so unmask them here (as the suite does for its workers)
  // or faacEncEncode raises EInvalidOp.
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

  FSampleRate := ASampleRate;
  FChannels := AChannels;
  FQuality := 100;
  FBitRate := 0;

  FHandle := faacEncOpen(culong(ASampleRate), cuint(AChannels),
                         @FInputSamples, @FMaxOutputBytes);
  if FHandle = nil then
  begin
    TPALog.Error(ClassName, 'faacEncOpen failed');
    Exit;
  end;
  SetLength(FOutBuf, FMaxOutputBytes);
end;

destructor TFAACEncoder.Destroy;
begin
  if FHandle <> nil then
    faacEncClose(FHandle);
  inherited Destroy;
end;

procedure TFAACEncoder.ApplyConfig;
var
  cfg: PfaacEncConfiguration;
begin
  if FConfigured or (FHandle = nil) then
    Exit;
  cfg := faacEncGetCurrentConfiguration(FHandle);
  cfg^.mpegVersion := FAAC_MPEG4;
  cfg^.aacObjectType := FAAC_OBJ_LOW;
  cfg^.inputFormat := FAAC_INPUT_16BIT;
  cfg^.outputFormat := FAAC_RAW_STREAM;   // raw AAC frames for the MP4 muxer
  cfg^.useTns := 0;
  if FBitRate > 0 then
    cfg^.bitRate := FBitRate
  else
    cfg^.quantqual := FQuality;
  if faacEncSetConfiguration(FHandle, cfg) = 0 then
    TPALog.Error(ClassName, 'faacEncSetConfiguration rejected the settings');
  FConfigured := True;
end;

function TFAACEncoder.SamplesPerFrame: Integer;
begin
  if FChannels > 0 then
    Result := FInputSamples div culong(FChannels)
  else
    Result := 1024;
end;

function TFAACEncoder.GetAudioSpecificConfig: TBytes;
var
  p: PByte;
  sz: culong;
begin
  Result := nil;
  if FHandle = nil then
    Exit;
  ApplyConfig; // ASC reflects the configuration, so lock it in first
  p := nil;
  sz := 0;
  if faacEncGetDecoderSpecificInfo(FHandle, @p, @sz) <> 0 then
  begin
    TPALog.Warning(ClassName, 'faacEncGetDecoderSpecificInfo failed');
    Exit;
  end;
  if (p <> nil) and (sz > 0) then
  begin
    SetLength(Result, sz);
    Move(p^, Result[0], sz);
  end;
  if p <> nil then
    cfree(p); // malloc'd by libfaac
end;

function TFAACEncoder.Encode(AInput: PSmallInt; ASampleCount: Integer; out AFrame: TBytes): Boolean;
var
  n: cint;
begin
  AFrame := nil;
  Result := False;
  if FHandle = nil then
    Exit;
  ApplyConfig;

  n := faacEncEncode(FHandle, AInput, cuint(ASampleCount), @FOutBuf[0], cuint(FMaxOutputBytes));
  if n < 0 then
  begin
    TPALog.Error(ClassName, 'faacEncEncode failed');
    Exit;
  end;
  Result := True;
  if n > 0 then
  begin
    SetLength(AFrame, n);
    Move(FOutBuf[0], AFrame[0], n);
  end;
end;

initialization
  LoadFaacLibrary;
finalization
  UnloadFaacLibrary;
end.
