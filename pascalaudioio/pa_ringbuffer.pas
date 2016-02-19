{
    This unit is part of the PascalAudio project.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and license.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_ringbuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TRingBuffer }

  TRingBuffer = class
  private
    FMem: PByte;
    FWritePos: Integer;
    FReadPos: Integer;
    FUsedSpace: Integer;
    FTotalSpace: Integer;
    function GetFreeSpace: Integer;
  public
    constructor Create(ASize: Integer);
    destructor Destroy; override;
    function Write(const ASource; ASize: Integer): Integer;
    function Read(var ADest; ASize: Integer): Integer;
    property FreeSpace: Integer read GetFreeSpace;
    property UsedSpace: Integer read FUsedSpace;

  end;

implementation

{ TRingBuffer }

function TRingBuffer.GetFreeSpace: Integer;
begin
  Result := FTotalSpace-FUsedSpace;
end;

constructor TRingBuffer.Create(ASize: Integer);
begin
  FMem:=Getmem(ASize);
  FTotalSpace:=ASize;
end;

destructor TRingBuffer.Destroy;
begin
  Freemem(FMem);
  inherited Destroy;
end;

function Min(A,B: Integer): Integer;
begin
  if A < B then Exit(A);
  Result := B;
end;

function TRingBuffer.Write(const ASource; ASize: Integer): Integer;
var
  EOB: Integer; // end of buffer
  WSize: Integer;
  WTotal: Integer = 0;
begin
  if FUsedSpace = 0 then
  begin
    // give the best chance of not splitting the data at buffer end.
    FWritePos:=0;
    FReadPos:=0;
  end;
  if ASize > FreeSpace then
    raise Exception.Create('Ring buffer overflow');
  Result := ASize;
  Inc(FUsedSpace, ASize);
  while ASize > 0 do
  begin
    EOB := FTotalSpace - FWritePos;
    WSize := Min(ASize, EOB);
    Move(PByte(@ASource)[WTotal], FMem[FWritePos], WSize);
    Inc(FWritePos, WSize);
    Dec(ASize, WSize);

    if FWritePos >= FTotalSpace then
      FWritePos:= 0;
  end;
end;

function TRingBuffer.Read(var ADest; ASize: Integer): Integer;
var
  EOB: Integer; // end of buffer
  RSize: Integer;
  RTotal: Integer = 0;
begin
  if ASize > UsedSpace then
    raise Exception.Create('Ring buffer underflow');
  ASize := Min(ASize, UsedSpace);
  Result := ASize;

  Dec(FUsedSpace, ASize);
  while ASize > 0 do
  begin
    EOB := FTotalSpace - FReadPos;
    RSize := Min(EOB, ASize);
    Move(FMem[FReadPos], PByte(@ADest)[RTotal],RSize);
    Dec(ASize, RSize);
    Inc(FReadPos, RSize);
    if FReadPos >= FTotalSpace then
      FReadPos:=0;
  end;
end;

end.

