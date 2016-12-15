{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pa_base, pa_stream;

type
  TPARegisterType = (
    partDecoder,
    partEncoder,
    partDeviceIn,
    partDeviceOut,
    partFilter
  );

  TFileMagic = array of char;
const
  cPAFileMagicEmpty = #0#0#0#0;

// generic register
procedure PARegister(AType: TPARegisterType; AClass: TClass; AName: String; AExtension: String = ''; AMagic: TFileMagic = cPAFileMagicEmpty; AMagicLen: Integer = 4);
function  PARegisteredGet(AType: TPARegisterType; AName: String): TClass;

// enumerate
function PARegisteredGetList(AType: TPARegisterType): TStrings;

// specific registers. can be used instead of PARegister
procedure PARegisterDecoder(ASource: TPAStreamSourceClass; AName: String; AExtension: String; AMagic: TFileMagic);
procedure PARegisterEncoder(ASource: TPAStreamDestinationClass; AName: String; AExtension: String; AMagic: TFileMagic);
procedure PARegisterDeviceOut(AOutput: TPAAudioDestinationClass; AName: String);
procedure PARegisterDeviceIn(AInput: TPAAudioDestinationClass; AName: String);
procedure PARegisterFilter(AFilter: TPAAudioLinkClass; AName: String);

// decoders
//function PARegisteredGetDecoderClass(AExtention: String): TPAStreamSourceClass;
function PARegisteredGetDecoderClass(AMagic: TFileMagic): TPAStreamSourceClass;
function PARegisteredGetDecoderClass(AStream: TStream): TPAStreamSourceClass;
function PARegisteredGetDecoderClass(AFileName: String; AOnlyUseExtention: Boolean): TPAStreamSourceClass;

// encoders
function PARegisteredGetEncoderClass(AExtention: String): TPAStreamDestinationClass;
function PARegisteredGetEncoderClass(AMagic: TFileMagic): TPAStreamDestinationClass;

// devices
function PARegisteredGetDeviceOut(AName: String): TPAAudioDestinationClass;
function PARegisteredGetDeviceIn(AName: String): TPAAudioSourceClass;

// filters
function PARegisteredGetFilter(AName: String): TPAAudioLinkClass;

implementation

uses
  fgl;

type
  TAudioClassEntry = class
    AudioClass: TClass;
    Name: String;
    Extention: String;
    Magic: TFileMagic;
    MagicLen: Integer;
  end;

  TAudioClassList = specialize TFPGObjectList<TAudioClassEntry>;

var
  EncoderList: TAudioClassList;
  DecoderList: TAudioClassList;
  DeviceOutList: TAudioClassList;
  DeviceInList: TAudioClassList;
  FilterList: TAudioClassList;

procedure PARegisterDecoder(ASource: TPAStreamSourceClass; AName: String; AExtension: String; AMagic: TFileMagic);
var
  lSource: TAudioClassEntry;
begin
  lSource := TAudioClassEntry.Create;
  lSource.Name:=AName;
  lSource.Extention:=AExtension;
  lSource.Magic := AMagic;
  lSource.AudioClass:=ASource;
  DecoderList.Add(lSource);
end;

procedure PARegisterEncoder(ASource: TPAStreamDestinationClass; AName: String; AExtension: String; AMagic: TFileMagic);
var
  lDest: TAudioClassEntry;
begin
  lDest := TAudioClassEntry.Create;
  lDest.Name:=AName;
  lDest.Extention:=AExtension;
  lDest.Magic := AMagic;
  lDest.AudioClass:=ASource;
  EncoderList.Add(lDest);
end;

procedure PARegister(AType: TPARegisterType; AClass: TClass; AName: String; AExtension: String; AMagic: TFileMagic; AMagicLen: Integer);
var
  Entry: TAudioClassEntry;
  List: TAudioClassList;
begin
  case AType of
    partDecoder   : List := DecoderList;
    partEncoder   : List := EncoderList;
    partDeviceIn  : List := DeviceInList;
    partDeviceOut : List := DeviceOutList;
    partFilter    : List := FilterList;
  end;

  Entry := TAudioClassEntry.Create;
  Entry.Name:=AName;
  Entry.AudioClass:=AClass;
  Entry.Extention:=AExtension;
  Entry.Magic := AMagic;
  Entry.MagicLen:=AMagicLen;
  List.Add(Entry);
end;

procedure PARegisterDeviceOut(AOutput: TPAAudioDestinationClass; AName: String);
begin
  PARegister(partDeviceOut, AOutput, AName);
end;

procedure PARegisterDeviceIn(AInput: TPAAudioDestinationClass; AName: String);
begin
  PARegister(partDeviceIn, AInput, AName);
end;

procedure PARegisterFilter(AFilter: TPAAudioLinkClass; AName: String);
begin
  PARegister(partFilter, AFilter, AName);
end;

function PARegisteredGetDecoderClass(AExtention: String): TPAStreamSourceClass;
var
  i: TAudioClassEntry;
begin
  for i in DecoderList do
    if CompareStr(i.Extention, AExtention) = 0 then
      Exit(TPAStreamSourceClass(i.AudioClass));

  Result := nil;
end;

function PARegisteredGetDecoderClass(AMagic: TFileMagic): TPAStreamSourceClass;
var
  i: TAudioClassEntry;
begin
  for i in DecoderList do
  begin
    if CompareMem(@i.Magic[0], @AMagic[0], i.MagicLen) then
      Exit(TPAStreamSourceClass(i.AudioClass));
  end;

  Result := nil;
end;

function PARegisteredGetDecoderClass(AStream: TStream): TPAStreamSourceClass;
var
  lSavedPos: Int64;
  lMagic: TFileMagic;
begin
  Result := nil;
  lSavedPos := AStream.Position;
  AStream.Position:=0;
  SetLength(lMagic, 4);
  if AStream.Read(lMagic[0], SizeOf(lMagic)) = SizeOf(lMagic) then
    Result := PARegisteredGetDecoderClass(lMagic);

  AStream.Position:=lSavedPos;
end;

function PARegisteredGetDecoderClass(AFileName: String; AOnlyUseExtention: Boolean): TPAStreamSourceClass;
var
  lFile: TFileStream;
begin
  if AOnlyUseExtention then
    Exit(PARegisteredGetDecoderClass(ExtractFileExt(AFileName)));

  Result := nil;

  lFile := TFileStream.Create(AFileName, fmOpenRead);
  try
    Result := PARegisteredGetDecoderClass(lFile);
  finally
    lFile.Free;
  end;
end;

function PARegisteredGetEncoderClass(AExtention: String): TPAStreamDestinationClass;
var
  i: TAudioClassEntry;
begin
  for i in EncoderList do
    if i.Name = AExtention then
      Exit(TPAStreamDestinationClass(i.AudioClass));

  Result := nil;
end;

function PARegisteredGetEncoderClass(AMagic: TFileMagic): TPAStreamDestinationClass;
var
  i: TAudioClassEntry;
begin
  for i in EncoderList do
    if i.Magic = AMagic then
      Exit(TPAStreamDestinationClass(i.AudioClass));

  Result := nil;
end;

function PARegisteredGetDeviceOut(AName: String): TPAAudioDestinationClass;
begin
  if AName = '' then
    Exit(TPAAudioDestinationClass(DeviceOutList.Items[0].AudioClass));

  Result := TPAAudioDestinationClass(PARegisteredGet(partDeviceOut, AName));
end;

function PARegisteredGetDeviceIn(AName: String): TPAAudioSourceClass;
begin
  Result := TPAAudioSourceClass(PARegisteredGet(partDeviceOut, AName));
end;

function PARegisteredGetFilter(AName: String): TPAAudioLinkClass;
begin
  Result := TPAAudioLinkClass(PARegisteredGet(partFilter, AName));
end;

function PARegisteredGet(AType: TPARegisterType; AName: String): TClass;
var
  i: TAudioClassEntry;
  List: TAudioClassList;
begin
  case AType of
    partDecoder   : List := DecoderList;
    partEncoder   : List := EncoderList;
    partDeviceIn  : List := DeviceInList;
    partDeviceOut : List := DeviceOutList;
    partFilter    : List := FilterList;
  end;
  for i in List do
    if i.Name = AName then
      Exit(i.AudioClass);

  Result := nil;
end;

function PARegisteredGetList(AType: TPARegisterType): TStrings;
var
  i: TAudioClassEntry;
  List: TAudioClassList;
begin
  case AType of
    partDecoder   : List := DecoderList;
    partEncoder   : List := EncoderList;
    partDeviceIn  : List := DeviceInList;
    partDeviceOut : List := DeviceOutList;
    partFilter    : List := FilterList;
  end;

  Result := TStringList.Create;

  for i in List do
  begin
    Result.AddObject(i.Name, TObject(i.AudioClass));
  end;

  if Result.Count = 0 then
  FreeAndNil(Result);

end;

initialization
  EncoderList := TAudioClassList.Create;
  DecoderList := TAudioClassList.Create;
  DeviceOutList := TAudioClassList.Create;
  DeviceInList := TAudioClassList.Create;
  FilterList := TAudioClassList.Create;
finalization
  EncoderList.Free;
  DecoderList.Free;
  DeviceOutList.Free;
  DeviceInList.Free;
  FilterList.Free;
end.

