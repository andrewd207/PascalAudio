{libfaad2 which this file links to is GPL}
{*
** FAAD2 - Freeware Advanced Audio (AAC) Decoder including SBR decoding
** Copyright (C) 2003-2005 M. Bakker, Nero AG, http://www.nero.com
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
**
** Any non-GPL usage of this software or parts of this software is strictly
** forbidden.
**
** The "appropriate copyright message" mentioned in section 2c of the GPLv2
** must read: "Code from FAAD2 is copyright (c) Nero AG, www.nero.com"
**
** Commercial non-GPL licensing of this software is possible.
** For more info contact Nero AG through Mpeg4AAClicense@nero.com.
**
** $Id: neaacdec.h,v 1.13 2009/01/26 23:51:15 menno Exp $
**}

unit paio_faad2;

{ $DEFINE drm}

{$mode objfpc}{$H+}
{$packrecords 16}
{$optimization noorderfields}



interface

uses
  Classes, SysUtils, dynlibs;

const
  FAAD2_VERSION  = '2.7';
  {$IFNDEF drm}
  FAAD_LIBNAME= 'libfaad.so';
  {$ELSE}
  FAAD_LIBNAME= 'libfaad_drm.so';
  {$ENDIF}

type
  TAACObjectType = (
    otMain      = 1,
    otLC        = 2,
    otSSR       = 3,
    otLTP       = 4,
    otHE_AAC    = 5,
    otER_LC     = 17,
    otER_LTP    = 19,
    otLD        = 23,
    otDRM_ER_LC = 27 // special object type for DRM
  );

  TAACHeaderType = (
    htRAW       = 0,
    htADIF      = 1,
    htADTS      = 2,
    htLATM      = 3
  );

  TAACSBRSignaling = (
    sbrsNO_SBR           = 0,
    sbrsSBR_UPSAMPLED    = 1,
    sbrsSBR_DOWNSAMPLED  = 2,
    sbrsNO_SBR_UPSAMPLED = 3
  );

  // library output formats
  TAACFormat = (
    FAAD_FMT_16BIT  = 1,
    FAAD_FMT_24BIT  = 2,
    FAAD_FMT_32BIT  = 3,
    FAAD_FMT_FLOAT  = 4,
    FAAD_FMT_DOUBLE  = 5
  );

 const
   FAAD_FMT_FIXED = FAAD_FMT_FLOAT;

   //Capabilities
   LC_DEC_CAP           = 1 shl 0; // Can decode LC
   MAIN_DEC_CAP         = 1 shl 1; // Can decode MAIN
   LTP_DEC_CAP          = 1 shl 2; // Can decode LTP
   LD_DEC_CAP           = 1 shl 3; // Can decode LD
   ERROR_RESILIENCE_CAP = 1 shl 4; // Can decode ER
   FIXED_POINT_CAP      = 1 shl 5; // Fixed point

   // Channel definitions
   FRONT_CHANNEL_CENTER  = 1;
   FRONT_CHANNEL_LEFT    = 2;
   FRONT_CHANNEL_RIGHT   = 3;
   SIDE_CHANNEL_LEFT     = 4;
   SIDE_CHANNEL_RIGHT    = 5;
   BACK_CHANNEL_LEFT     = 6;
   BACK_CHANNEL_RIGHT    = 7;
   BACK_CHANNEL_CENTER   = 8;
   LFE_CHANNEL           = 9;
   UNKNOWN_CHANNEL       = 0;

   // DRM channel definitions
   DRMCH_MONO            = 1;
   DRMCH_STEREO          = 2;
   DRMCH_SBR_MONO        = 3;
   DRMCH_SBR_STEREO      = 4;
   DRMCH_SBR_PS_STEREO   = 5;

   { A decode call can eat up to FAAD_MIN_STREAMSIZE bytes per decoded channel,
   so at least so much bytes per channel should be available in this stream }
   FAAD_MIN_STREAMSIZE   = 768;  // 6144 bits/channel

type

  PNeAACDecHandle = Pointer;

  Pmp4AudioSpecificConfig = ^Tmp4AudioSpecificConfig;
  Tmp4AudioSpecificConfig =  record
    //* Audio Specific Info */
    objectTypeIndex: Byte;
    samplingFrequencyIndex: Byte;
    {$ifdef cpu64}_padding0: array[0..5] of Byte;{$else}_padding0: word{needs testing}{$endif}
    samplingFrequency: DWord;
    {$ifdef cpu64}_padding1: array[0..3] of Byte;{$endif}
    channelsConfiguration: Byte;

    //* GA Specific Info */
    frameLengthFlag: Byte;
    dependsOnCoreCoder: Byte;
    //_padding1: DWord;
    coreCoderDelay: Word;
    extensionFlag: Byte;
    aacSectionDataResilienceFlag: Byte;
    aacScalefactorDataResilienceFlag: Byte;
    aacSpectralDataResilienceFlag: Byte;
    epConfig: Byte;

    sbr_present_flag: ByteBool;
    forceUpSampling: ByteBool;
    downSampledSBR: ByteBool;
    //padding:array[0..20] of Byte;
  end;

  PNeAACDecConfiguration = ^TNeAACDecConfiguration;
  TNeAACDecConfiguration = record
    defObjectType: Byte;
    {$ifdef cpu64}_padding0: array[0..6] of byte;{$endif}
    defSampleRate: DWord;
    {$ifdef cpu64}_padding1: array[0..3] of byte;{$endif}
    outputFormat: Byte;
    downMatrix: Byte;
    useOldADTSFormat: ByteBool;
    dontUpSampleImplicitSBR: ByteBool;
  end;

  PNeAACDecFrameInfo = ^TNeAACDecFrameInfo;
  TNeAACDecFrameInfo = record
    bytesconsumed:DWord;
    {$ifdef cpu64}_padding0: array[0..3] of byte;{$endif}
    samples: qWord;
    channels: Byte;
    error: Byte;
    samplerate: QWord;

    //* SBR: 0: off, 1: on; upsample, 2: on; downsampled, 3: off; upsampled */
    sbr: Byte;

    //* MPEG-4 ObjectType */
    object_type: Byte;

    //* AAC header type; MP4 will be signalled as RAW also */
    header_type:Byte;

    //* multichannel configuration */
    num_front_channels: Byte;
    num_side_channels: Byte;
    num_back_channels: Byte;
    num_lfe_channels: Byte;
    channel_position: array[0..63] of Byte;

    //* PS: 0: off, 1: on */
    ps: Byte;
    //dummy: array[0..3] of Int64; // the lib is overwriting 16 bytes! so set some buffer space
  end;

  { TAACDecoder }

  TAACDecoder = class
  private
    FChannels: Byte;
    FHandle: PNeAACDecHandle;
    FLastResult: LongInt;
    FSampleRate: DWord;
    function GetConfig: PNeAACDecConfiguration;
    procedure SetConfig(AValue: PNeAACDecConfiguration);
    procedure SetLastResult(AValue: LongInt);
  public
    class function GetErroMessage(AError: Byte): String; static;
    class function AudioSpecificConfig(ABuffer:PByte; ABufferSize: DWord; AMp4ASC: Pmp4AudioSpecificConfig): Byte;
    constructor Create;
    procedure Init(ABuffer: PByte; ABufferSize: DWord);
    procedure Init2(ABuffer: PByte; ASizeOfDecoderSpecificInfo: DWord);
    destructor Destroy; override;
    procedure InitDrm;
    procedure PostSeekReset(AFrame: LongInt);
    function Decode(AInfo: PNeAACDecFrameInfo; ABuffer: PByte;  ABufferSize: DWord): Pointer; overload;
    function Decode(AInfo: PNeAACDecFrameInfo; ABuffer: PByte;  ABufferSize: DWord; ASampleBuffer: PPByte; ASampleBufferSize: DWord): Pointer; overload;
    property Channels: Byte read FChannels;
    property SampleRate: DWord read FSampleRate;
    property Config: PNeAACDecConfiguration read GetConfig write SetConfig;
    property LastResult: LongInt read FLastResult write SetLastResult;
    property Handle: PNeAACDecHandle read FHandle;
  end;

var
  NeAACDecGetErrorMessage: function (errcode: Byte): PChar; cdecl;
  NeAACDecGetCapabilities: function: DWord; cdecl;
  NeAACDecOpen: function: PNeAACDecHandle; cdecl;
  NeAACDecGetCurrentConfiguration: function(hDecoder: PNeAACDecHandle): PNeAACDecConfiguration; cdecl;
  NeAACDecSetConfiguration: function(hDecoder: PNeAACDecHandle; config: PNeAACDecConfiguration): Byte; cdecl;

//* Init the library based on info from the AAC file (ADTS/ADIF) */
  NeAACDecInit: function(hDecoder: PNeAACDecHandle; buffer: PByte; buffer_size: DWord; samplerate: PDWord; channels: PByte): LongInt cdecl;
///* Init the library using a DecoderSpecificInfo */
  NeAACDecInit2: function(hDecoder: PNeAACDecHandle; pbuffer: PByte; SizeOfDecoderSpecificInfo: DWord;samplerate: PDWord; channels: PByte): Byte; cdecl;
//* Init the library for DRM */
  NeAACDecInitDRM: function(hDecoder: PNeAACDecHandle; samplerate: DWord; channels: Byte): Byte; cdecl;

  NeAACDecPostSeekReset: procedure(hDecoder: PNeAACDecHandle; frame: LongInt); cdecl;
  NeAACDecClose: procedure(hDecoder: PNeAACDecHandle); cdecl;

  NeAACDecDecode: function(hDecoder: PNeAACDecHandle; hInfo: PNeAACDecFrameInfo; buffer: PByte; buffer_size: DWord): Pointer; cdecl;
  NeAACDecDecode2: function(hDecoder: PNeAACDecHandle; hInfo: PNeAACDecFrameInfo; buffer: PByte; buffer_size: DWord; sample_buffer: PPByte; sample_buffer_size: DWord): Pointer; cdecl;
  NeAACDecAudioSpecificConfig: function(pBuffer: PByte; buffer_size: DWord; mp4ASC: Pmp4AudioSpecificConfig): Byte; cdecl;

  procedure LoadFaadLibrary(ALibName: String = '');
  procedure UnloadFaadLibrary;
  function FaadLibraryLoaded: Boolean;

implementation
uses
  fgl;

var
  libHandle: TLibHandle = 0;
  //libList: TStringList;
  libList: specialize TFPGMap<String, PPointer>;

procedure LoadFaadLibrary(ALibName: String);
var
  i: Integer;
  lString: String;
  lPointer: Pointer;
begin
  if libHandle <> 0 then
    UnloadFaadLibrary;

  if ALibName = '' then
    ALibName:=FAAD_LIBNAME;

  libHandle := LoadLibrary(ALibName);
  if libHandle = 0 then
    Exit;

  libList := specialize TFPGMap<String, PPointer>.Create;

  libList.Add('NeAACDecGetErrorMessage', @NeAACDecGetErrorMessage);
  libList.Add('NeAACDecGetCapabilities', @NeAACDecGetCapabilities);
  libList.Add('NeAACDecOpen', @NeAACDecOpen);
  libList.Add('NeAACDecGetCurrentConfiguration', @NeAACDecGetCurrentConfiguration);
  libList.Add('NeAACDecSetConfiguration' , @NeAACDecSetConfiguration);
  libList.Add('NeAACDecInit', @NeAACDecInit);
  libList.Add('NeAACDecInit2', @NeAACDecInit2);
  libList.Add('NeAACDecInitDRM', @NeAACDecInitDRM);
  libList.Add('NeAACDecPostSeekReset', @NeAACDecPostSeekReset);
  libList.Add('NeAACDecClose', @NeAACDecClose);
  libList.Add('NeAACDecDecode', @NeAACDecDecode);
  libList.Add('NeAACDecDecode2', @NeAACDecDecode2);
  libList.Add('NeAACDecAudioSpecificConfig', @NeAACDecAudioSpecificConfig);

  for i := 0 to libList.Count-1 do
  begin
    lString := libList.Keys[i];
    lPointer := GetProcAddress(libHandle, lString);
    libList.Data[i]^ := lPointer;
  end;


end;

procedure UnloadFaadLibrary;
var
  i: Integer;
begin
  if libHandle = 0 then
    Exit;

  for i := 0 to libList.Count-1 do
    libList.Data[i]^ := nil;

  UnloadLibrary(libHandle);

  FreeAndNil(libList);
end;

function FaadLibraryLoaded: Boolean;
begin
  Result := libHandle <> 0;

end;

{ TAACDecoder }

constructor TAACDecoder.Create;
begin
  if not FaadLibraryLoaded then
    LoadFaadLibrary('');

  FHandle:= NeAACDecOpen();
end;

procedure TAACDecoder.Init(ABuffer: PByte; ABufferSize: DWord);
begin
  LastResult := NeAACDecInit(FHandle, ABuffer, ABufferSize, @FSampleRate, @FChannels);
end;

procedure TAACDecoder.Init2(ABuffer: PByte; ASizeOfDecoderSpecificInfo: DWord);
begin
  LastResult := NeAACDecInit2(FHandle, ABuffer, ASizeOfDecoderSpecificInfo, @FSampleRate, @FChannels);
end;

procedure TAACDecoder.InitDrm;
begin
  if NeAACDecInitDRM = nil then
    raise Exception.Create('Loaded Faad Library does not support DRM!');
  LastResult := NeAACDecInitDRM(FHandle, FSampleRate, FChannels);
end;


procedure TAACDecoder.PostSeekReset(AFrame: LongInt);
begin
  NeAACDecPostSeekReset(FHandle, AFrame);
end;

function TAACDecoder.Decode(AInfo: PNeAACDecFrameInfo; ABuffer: PByte;
  ABufferSize: DWord): Pointer;
begin
  Result := NeAACDecDecode(FHandle, AInfo, ABuffer, ABufferSize);
end;

function TAACDecoder.Decode(AInfo: PNeAACDecFrameInfo; ABuffer: PByte;
  ABufferSize: DWord; ASampleBuffer: PPByte; ASampleBufferSize: DWord): Pointer;
begin
  Result := NeAACDecDecode2(FHandle, AInfo, ABuffer, ABufferSize, ASampleBuffer, ASampleBufferSize);
end;

function TAACDecoder.GetConfig: PNeAACDecConfiguration;
begin
  Result := NeAACDecGetCurrentConfiguration(FHandle);
end;

procedure TAACDecoder.SetConfig(AValue: PNeAACDecConfiguration);
begin
  LastResult := NeAACDecSetConfiguration(FHandle, AValue);
end;

procedure TAACDecoder.SetLastResult(AValue: LongInt);
begin
  FLastResult:=AValue;
  if FLastResult <> 0 then
    WriteLn('Error: ', GetErroMessage(AValue));
end;

class function TAACDecoder.GetErroMessage(AError: Byte): String;
begin
  if not FaadLibraryLoaded then
    LoadFaadLibrary('');
  Result := NeAACDecGetErrorMessage(AError);
end;

class function TAACDecoder.AudioSpecificConfig(ABuffer: PByte;
  ABufferSize: DWord; AMp4ASC: Pmp4AudioSpecificConfig): Byte;
begin
  if not FaadLibraryLoaded then
    LoadFaadLibrary('');

  Result := NeAACDecAudioSpecificConfig(ABuffer, ABufferSize, AMp4ASC);
end;

destructor TAACDecoder.Destroy;
begin
  NeAACDecClose(FHandle);
  inherited Destroy;
end;

finalization
  UnloadFaadLibrary;

end.

