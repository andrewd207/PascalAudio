{
    This unit is part of Pascal Audio IO package.

    Copyright (c) 2016 by Andrew Haines.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      A nicer wrapper for the plain c ogg vorbis bindings.
}
unit OggHfObject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vorbis,ogg, ctypes;

type

  PPSingle = ^PSingle; // array of channel data

  { TOggDecFloat }
  POggDecFloat = ^TOggDecFloat;
  TOggDecFloat = class
  private
    FStream: TStream;
    FOwnsStream: Boolean;
    //FOgg: POggVorbis_File; // points to ogg when ov_open is successful
    FOgg: OggVorbis_File;
    FOpen: Boolean;
    function GetBitrate(ABitstream: LongInt): LongInt;
    function GetBitrateInstant: LongInt;
    function GetComment(ABitstream: LongInt): Pvorbis_comment;
    function GetInfo(AIndex: Integer): Pvorbis_info;
    function GetPCMLength(ABitstream: LongInt): Double;
    function GetPCMPosition: Int64;
    function GetRawLength(ABitstream: LongInt): Double;
    function GetRawPosition: Int64;
    function GetSeekable: Boolean;
    function GetSerialNumber(ABitstream: LongInt): LongInt;
    function GetStreams:longint;
    function GetTimeLength(ABitstream: LongInt): Double;
    function GetTimePosition: Double;
    procedure SetPCMPosition(AValue: Int64);
    procedure SetRawPosition(AValue: Int64);
    procedure SetTimePosition(AValue: Double);
    constructor CreateTest(AFileName: String; out IsOk: Boolean);
    constructor CreateTest(AFile: TStream; out IsOk: Boolean);
  private
     function SeekCB(offset: ogg_int64_t; whence: cint): cint; cdecl;
     function CloseCB: cint; cdecl;
     function TellCB: clong; cdecl;
     function ReadCB (ptr: pointer; size, nmemb: csize_t): csize_t; cdecl;
     function GetFileSize: Int64;

  public
    class function IsValidOgg(AFile: TStream; AOgg: POggDecFloat = nil): Boolean;
    procedure FinishCreate; // use if Ogg was created by IsValidOgg
    class function TryCreate(AStream: TStream; AOwnsStream: Boolean): TOggDecFloat;
    class function TryCreate(AFileName: String): TOggDecFloat;
    constructor Create(AFileName: String);
    constructor Create(AFile: TStream; AOwnsStream: Boolean);
    destructor  Destroy; override;
// constructor function open_callbacks(f:pointer;vf:POggVorbis_File;initial:PAnsiChar;ibytes:longint;callbacks:ov_callbacks):longint;
//constructor function ov_open(f:pointer;vf:POggVorbis_File;initial:PAnsiChar;ibytes:longint):longint;
//function ov_test_callbacks(f:pointer;vf:POggVorbis_File;initial:PAnsiChar;ibytes:longint;callbacks:ov_callbacks):longint;
//class function ov_test(f:pointer;vf:POggVorbis_File;initial:PAnsiChar;ibytes:longint):longint;
// ??function ov_test_open(vf:POggVorbis_File):longint;

    function PCMSeekPage(pos:ogg_int64_t):longint;
    function TimeSeekPage(milliseconds:ogg_int64_t):longint;

    function Read(ABuffer: Pointer; ABytesReq: LongInt; ABitstream: PLongint): LongInt;
    function ReadFloat(out ABuffer: PPSingle; AMaxSamples: Integer; ABitstream: PLongint): LongInt;
    property Info: Pvorbis_info index -1 read GetInfo;
    property InfoB[ABitstream: LongInt]: Pvorbis_info read GetInfo;
    property Streams: LongInt read GetStreams;
    property BitrateB[ABitstream: LongInt]: LongInt read GetBitrate;
    property Bitrate: LongInt index -1 read GetBitrate;
    property BitrateInstant: LongInt read GetBitrateInstant;
    property CommentB[ABitstream: LongInt]: Pvorbis_comment read GetComment;
    property Comment: Pvorbis_comment index -1 read GetComment;
    property TimePosition: Double read GetTimePosition write SetTimePosition;
    property PCMPosition: Int64 read GetPCMPosition write SetPCMPosition;
    property RawPosition: Int64 read GetRawPosition write SetRawPosition;
    property TimeLengthB[ABitstream: LongInt]: Double read GetTimeLength;
    property TimeLength: Double index -1 read GetTimeLength;
    property PCMLengthB[ABitstream: LongInt]: Double read GetPCMLength;
    property PCMLength: Double index -1 read GetPCMLength;
    property RawLengthB[ABitstream: LongInt]: Double read GetRawLength;
    property RawLength: Double index -1 read GetRawLength;
    property SerialNumberB[ABitstream: LongInt]: LongInt read GetSerialNumber;
    property SerialNumber: LongInt index -1 read GetSerialNumber;

    property Seekable: Boolean read GetSeekable;
    property FileSize: Int64 read GetFileSize;
  end;

implementation

var
  callbacks: ov_callbacks;

{ TOggDecFloat }

function TOggDecFloat.GetBitrate(ABitstream: LongInt): LongInt;
begin
  Result:=ov_bitrate(FOgg, ABitstream);
end;

function TOggDecFloat.GetBitrateInstant: LongInt;
begin
  Result := ov_bitrate_instant(FOgg);
end;

function TOggDecFloat.GetComment(ABitstream: LongInt): Pvorbis_comment;
begin
  Result:=ov_comment(FOgg, ABitstream);
end;

function TOggDecFloat.GetInfo(AIndex: Integer): Pvorbis_info;
begin
  Result:=ov_info(FOgg, AIndex);
end;

function TOggDecFloat.GetPCMLength(ABitstream: LongInt): Double;
begin
  Result:=ov_pcm_total(FOgg, ABitstream);
end;

function TOggDecFloat.GetPCMPosition: Int64;
begin
  Result:=ov_pcm_tell(FOgg);
end;

function TOggDecFloat.GetRawLength(ABitstream: LongInt): Double;
begin
  Result:=ov_raw_total(FOgg, ABitstream);
end;

function TOggDecFloat.GetRawPosition: Int64;
begin
  Result:=ov_raw_tell(FOgg);
end;

function TOggDecFloat.GetSeekable: Boolean;
begin
  Result:=ov_seekable(FOgg) <> 0;
end;

function TOggDecFloat.GetSerialNumber(ABitstream: LongInt): LongInt;
begin
  Result:=ov_serialnumber(FOgg, ABitstream);
end;

function TOggDecFloat.GetStreams: longint;
begin
  Result:=ov_streams(FOgg);
end;

function TOggDecFloat.GetTimeLength(ABitstream: LongInt): Double;
begin
  Result:=ov_time_total(FOgg, ABitstream);
end;

function TOggDecFloat.GetTimePosition: Double;
begin
  Result:=ov_time_tell(FOgg);
end;

procedure TOggDecFloat.SetPCMPosition(AValue: Int64);
begin
  ov_pcm_seek(FOgg, AValue);
end;

procedure TOggDecFloat.SetRawPosition(AValue: Int64);
begin
  ov_raw_seek(FOgg, AValue);
end;

procedure TOggDecFloat.SetTimePosition(AValue: Double);
begin
  ov_time_seek(FOgg, AValue);
end;

class function TOggDecFloat.IsValidOgg(AFile: TStream; AOgg: POggDecFloat): Boolean;
var
  o: TOggDecFloat;
begin
  o := TOggDecFloat.CreateTest(AFile, Result);
  if Result and Assigned(AOgg) then
    AOgg^ := o
  else
    o.Free;
end;

procedure TOggDecFloat.FinishCreate;
begin
  ov_test_open(FOgg);
end;

class function TOggDecFloat.TryCreate(AFileName: String): TOggDecFloat;
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  Result := TryCreate(F, True);
end;

class function TOggDecFloat.TryCreate(AStream: TStream; AOwnsStream: Boolean): TOggDecFloat;
var
  IsOk: Boolean;
begin
  Result := CreateTest(AStream, IsOk);
  if Not IsOk then
    FreeAndNil(Result)
  else
  begin
     Result.FinishCreate;
     Result.FOwnsStream:=AOwnsStream;
  end;
end;

constructor TOggDecFloat.Create(AFileName: String);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  Create(F, True);
end;

constructor TOggDecFloat.Create(AFile: TStream; AOwnsStream: Boolean);
var
  IsOk: Boolean;
begin
  CreateTest(AFile, IsOk);
  if not IsOk then
    Raise Exception.Create('invalid ogg file');
  FinishCreate;
  FOwnsStream := AOwnsStream;
end;

function OggReadCBGlue(ptr: pointer; size, nmemb: csize_t; datasource: pointer): csize_t; cdecl;
begin
  Result := TOggDecFloat(datasource).ReadCB(ptr,size,nmemb);
end;

constructor TOggDecFloat.CreateTest(AFileName: String; out IsOk: Boolean);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  CreateTest(F, IsOk);
end;

constructor TOggDecFloat.CreateTest(AFile: TStream; out IsOk: Boolean);
begin
  FStream := AFile;
  IsOk := ov_test_callbacks(Pointer(Self), FOgg, nil, 0, callbacks) = 0;
  if not IsOk then
  begin
    Fopen := False;
    ov_clear(FOgg);
    FStream.Free;
  end
  else
    FOpen := True;
end;
{const
      SEEK_SET=0;
      SEEK_CUR=1;
      SEEK_END=2;}

function TOggDecFloat.SeekCB(offset: ogg_int64_t; whence: cint): cint; cdecl;
begin
 Result := FStream.Seek(offset, TSeekOrigin(whence));
end;

function TOggDecFloat.CloseCB: cint; cdecl;
begin
  Result := 0;
end;

function TOggDecFloat.TellCB: clong; cdecl;
begin
  Result:=FStream.Position;
end;

function TOggDecFloat.ReadCB(ptr: pointer; size, nmemb: csize_t): csize_t;
  cdecl;
begin
 Result := FStream.Read(ptr^,nmemb*size);
end;

function TOggDecFloat.GetFileSize: Int64;
begin
   Result := 0;
   if Assigned(FStream) then
     Result := FStream.Size;
end;

destructor TOggDecFloat.Destroy;
begin
  if FOpen then
  begin
    ov_clear(FOgg);
    if FOwnsStream then
      FStream.Free;
  end;
  inherited Destroy;
end;

function TOggDecFloat.PCMSeekPage(pos: ogg_int64_t): longint;
begin
  Result := ov_pcm_seek_page(Fogg, pos);
end;

function TOggDecFloat.TimeSeekPage(milliseconds: ogg_int64_t): longint;
begin
  Result := ov_time_seek_page(Fogg, milliseconds);
end;

function TOggDecFloat.Read(ABuffer: Pointer; ABytesReq: LongInt; ABitstream: PLongint): LongInt;
begin
  Result := ov_read(Fogg, ABuffer, ABytesReq, False, 2, True, ABitstream);
end;

function TOggDecFloat.ReadFloat(out ABuffer: PPSingle; AMaxSamples: Integer; ABitstream: PLongint): LongInt;
begin
  Result := ov_read_float(Fogg, ABuffer, AMaxSamples, ABitstream);
end;

initialization
  callbacks.read := @OggReadCBGlue;
  callbacks.close:= close_func(@TOggDecFloat.CloseCB);
  callbacks.seek := seek_func(@TOggDecFloat.SeekCB);
  callbacks.tell := tell_func(@TOggDecFloat.TellCB);

end.

