{
    This unit is part of the PascalAudio project.

    Copyright (c) 2020 by Andrew Haines.

    See the files COPYING.modifiedLGPL and license.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit mp4codec;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, quicktimeatoms;

type
  TFourCC = quicktimeatoms.TAtomName;

  TMP4CodecClass = class of TMP4Codec;

  { TMP4Codec }

  TMP4Codec = class
  protected
    FAtom: TAtom;
  public
    procedure Filter(AData: PByte; ASize: Integer); virtual;
    constructor Create(AAtom: TAtom); virtual;
    property Atom: TAtom read FAtom;

  end;

  procedure MP4RegisterCodec(ACodec: TMP4CodecClass; AFourCC: TFourCC);
  function  MP4LookupCodec(AFourCC: TFourCC; out ACodecClass: TMP4CodecClass): Boolean;

  operator := (const A: String): TFourCC;

implementation
uses
  fgl;

type
  TMP4CodecMap = specialize TFPGMap<Cardinal{TFourCC}, TMP4CodecClass>;

var
  gCodecList: TMP4CodecMap;

procedure MP4RegisterCodec(ACodec: TMP4CodecClass; AFourCC: TFourCC);
begin
  gCodecList.AddOrSetData(AFourCC, ACodec);
end;

function MP4LookupCodec(AFourCC: TFourCC; out ACodecClass: TMP4CodecClass): Boolean;
var
  lIndex: Integer;
begin
  Result := gCodecList.Find(AFourCC, lIndex);
  if Result then
    ACodecClass:=gCodecList.Data[lIndex];
end;

// this already exists for TAtomName but this is duplicated so quicktimeatoms doesn't have to be used
operator:=(const A: String): TFourCC;
begin
  if Length(A) <> 4 then
    raise Exception.Create('FourCC codes are 4 chars long!');
  Result.Chars := A;
end;

{ TMP4Codec }

procedure TMP4Codec.Filter(AData: PByte; ASize: Integer);
begin
  // does nothing.
end;

constructor TMP4Codec.Create(AAtom: TAtom);
begin
  FAtom := AAtom;
end;

initialization
  gCodecList := TMP4CodecMap.Create;
  gCodecList.Sorted:=True;

finalization
  gCodecList.Free;

end.

