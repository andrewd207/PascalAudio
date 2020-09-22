unit paio_ogg_container;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type


  { TOggPageHeader }

  POggPageHeader = ^TOggPageHeader;
  TOggPageHeader = object
  private
    function GetisFirstOfLogicalBitstream: Boolean;
    function GetisFreshPacket: Boolean;
    function GetisLastPageOfLogicalBitstream: Boolean;
  public
    Signature: array[0..3] of Char; // OggS
    Version: Byte;
    Flags: Byte;
    Position: Int64; // The position after the page has been processed. The page end sample
    Serial: DWord;
    PageIndex: DWord;
    Checksum: DWord;
    SegmentsCount: Byte;
    SegmentSize: array of Byte; // dynamic. max 255
    // The Following are not stored in the header but are determined when the header is read.
    _PacketCount: Integer;
    _DataSize: Integer;
    _LastPacketNeedsNextPage: Boolean;
    constructor ReadFromStream(AStream: TStream; AFillOtherData: Boolean);
    property isFreshPacket: Boolean read GetisFreshPacket;
    property isFirstPageOfLogicalBitstream: Boolean read GetisFirstOfLogicalBitstream;
    property isLastPageOfLogicalBitstream: Boolean read GetisLastPageOfLogicalBitstream;
    function HeaderSize: Integer;
    function SegmentDataSize(out PacketCount: Integer): Integer; // total size of all segment data
    function SegmentDataSize: Integer;
    function Size: Integer; // header + data size
  end;



implementation

{ TOggPageHeader }

function TOggPageHeader.GetisFirstOfLogicalBitstream: Boolean;
begin
  Result := Flags and 2 = 2;
end;

function TOggPageHeader.GetisFreshPacket: Boolean;
begin
  Result := Flags and 1 = 0;
end;

function TOggPageHeader.GetisLastPageOfLogicalBitstream: Boolean;
begin
  Result := Flags and 4 = 4;
end;

constructor TOggPageHeader.ReadFromStream(AStream: TStream;
  AFillOtherData: Boolean);
var
  i: Byte;
begin
  AStream.Read(Signature, 4);
  Version :=AStream.ReadByte;
  Flags := AStream.ReadByte;
  Position:=LEtoN(AStream.ReadQWord);
  Serial:=LEtoN(AStream.ReadDWord);
  PageIndex:=LEtoN(AStream.ReadDWord);
  Checksum:=LEtoN(AStream.ReadDWord);
  SegmentsCount := AStream.ReadByte;
  SetLength(SegmentSize, SegmentsCount);
  AStream.Read(SegmentSize[0], SegmentsCount);

  _PacketCount := 0;
  _DataSize := 0;
  _LastPacketNeedsNextPage := False;

  if AFillOtherData then
  begin
    _LastPacketNeedsNextPage:= ((SegmentsCount > 0) and (SegmentSize[SegmentsCount-1] = 255));
    for i := 0 to SegmentsCount-1 do
    begin
      _DataSize+=SegmentSize[i];
      if SegmentSize[i] < 255 then
        Inc(_PacketCount);
    end;
    // packets are not otherwise counted if they don't have a value < 255.
    if _LastPacketNeedsNextPage then
      Inc(_PacketCount);
  end;
end;

function TOggPageHeader.HeaderSize: Integer;
begin
  Result := 26 + (Length(SegmentSize));
end;

function TOggPageHeader.SegmentDataSize(out PacketCount: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  PacketCount:=0;

  for i := 0 to SegmentsCount-1 do
  begin
    Result += SegmentSize[i];
    if SegmentSize[i] < 255 then
      Inc(PacketCount);
  end;
end;

function TOggPageHeader.SegmentDataSize: Integer;
var
  lDummy: Integer;
begin
  Result := SegmentDataSize(lDummy);
end;

function TOggPageHeader.Size: Integer;
var
  lDummy: Integer;
begin
  Result := HeaderSize + SegmentDataSize(lDummy);
end;

end.

