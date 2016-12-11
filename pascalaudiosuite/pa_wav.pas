{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_wav;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pa_base, fpwavformat;

type

  { TPAWavSource }

  TPAWavSource = class(TPAAudioSource, IPAStream)
  private
    FOwnsStream: Boolean;
    FWavFormat: TWaveFormat;
    FStream: TStream;
    FValid: Boolean;
    FCurrentChunk: TChunkHeader;
    procedure ReadHeader;
    function LoadNextChunk: Boolean;
    function GetStream: TStream;
    procedure SetStream(AValue: TStream);
  protected
    function InternalOutputToDestination: Boolean; override;
  public
    // When a stream is assigned this object owns it and will free it.
    constructor Create(AStream: TStream; AOwnsStream: Boolean); reintroduce;
    destructor Destroy; override;
    property Stream: TStream read GetStream write SetStream;
    property OwnsStream: Boolean read FOwnsStream;
  end;

  TPAWavDest = class(TPAAudioDestination, IPAStream)
  private
    const
      cFileSizeOff = 4;
      cDataSizeOff = 40;
  private
    FStream: TStream;
    FOwnsStream: Boolean;
    FInited: Boolean;
    FFinished: Boolean;
    FDataSize: DWord;
    function GetStream: TStream;
    procedure InitStream;
    procedure FinishStream;
  protected
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; override;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean); reintroduce;
    destructor  Destroy; override;
    property    Stream: TStream read GetStream;
  end;

implementation

{ TPAWavSource }

procedure LEtoN(var fmt: TWaveFormat); overload;
begin
  // from fpwavreader
  with fmt, ChunkHeader do begin
    Size := LEtoN(Size);
    Format := LEtoN(Format);
    Channels := LEtoN(Channels);
    SampleRate := LEtoN(SampleRate);
    ByteRate := LEtoN(ByteRate);
    BlockAlign := LEtoN(BlockAlign);
    BitsPerSample := LEtoN(BitsPerSample);
  end;
end;


procedure NtoLE(var fmt: TWaveFormat); overload;
begin
  // from fpwavreader
  with fmt, ChunkHeader do begin
    Size := NtoLE(Size);
    Format := NtoLE(Format);
    Channels := NtoLE(Channels);
    SampleRate := NtoLE(SampleRate);
    ByteRate := NtoLE(ByteRate);
    BlockAlign := NtoLE(BlockAlign);
    BitsPerSample := NtoLE(BitsPerSample);
  end;
end;

{ TPAWavDest }

function TPAWavDest.GetStream: TStream;
begin
  Result := FStream;
end;

procedure TPAWavDest.InitStream;
var
  Riff: TRiffHeader;
  WavChunk: TWaveFormat;
  DataChunk: TChunkHeader;
begin
  FInited := True;

  Riff.ChunkHeader.ID := AUDIO_CHUNK_ID_RIFF;
  Riff.Format := AUDIO_CHUNK_ID_WAVE;
  Riff.ChunkHeader.Size:=0; // fill in later

  WavChunk.ChunkHeader.ID := AUDIO_CHUNK_ID_fmt;
  WavChunk.ChunkHeader.Size:=16;
  WavChunk.Format := AUDIO_FORMAT_PCM;

  WavChunk.BitsPerSample := 16;
  WavChunk.Channels:=Channels;
  WavChunk.SampleRate:= SamplesPerSecond div Channels;
  WavChunk.ByteRate:=(WavChunk.BitsPerSample * Channels) div 8;
  NtoLE(WavChunk);

  DataChunk.ID := AUDIO_CHUNK_ID_data;
  DataChunk.Size:=0; // fill in later

  FStream.Write(Riff, SizeOf(Riff));
  FStream.Write(WavChunk, SizeOf(WavChunk));
  FStream.Write(DataChunk, SizeOf(DataChunk));
  //That's it.
end;

procedure TPAWavDest.FinishStream;
begin
  if FFinished then
    Exit;

  FFinished:= True;

  // write total size -1
  FStream.Position:=cFileSizeOff;
  FStream.WriteDWord(NtoLE(DWord(FStream.Size-SizeOf(TChunkHeader))));

  // write raw data size
  FStream.Position:=cDataSizeOff;
  FStream.WriteDWord(NtoLE(FDataSize));
  FStream.Seek(0, soEnd);
end;

function TPAWavDest.InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64;
begin
  if not FInited then
    InitStream;

  Result := FStream.Write(AData, ACount);
  Inc(FDataSize, Result);

  if AIsLastData then
  begin
    FinishStream;
  end;
end;

constructor TPAWavDest.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  Inherited Create;
  Format:= afS16;
  FStream := AStream;
  FOwnsStream:=AOwnsStream;
end;

destructor TPAWavDest.Destroy;
begin
  if FOwnsStream then
    FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TPAWavSource.ReadHeader;
var
  Riff: TRiffHeader;
  RCount: Integer;
begin
  FValid := False;
  if not Assigned(FStream) then
    Exit;
  FStream.Seek(0, soBeginning);
  RCount := FStream.Read(Riff, SizeOf(Riff));
  if RCount <> SizeOf(Riff) then
    Exit;
  Riff.ChunkHeader.Size:=LEtoN(Riff.ChunkHeader.Size);
  FValid := (Riff.ChunkHeader.ID = AUDIO_CHUNK_ID_RIFF)
        and (Riff.Format = AUDIO_CHUNK_ID_WAVE);
  if not FValid then
    Exit;
  FStream.Read(FWavFormat, SizeOf(FWavFormat));
  LEtoN(FWavFormat);
  FValid := (FWavFormat.ChunkHeader.ID = AUDIO_CHUNK_ID_fmt)
        and (FWavFormat.Format = AUDIO_FORMAT_PCM);

  Channels:=FWavFormat.Channels;
  SamplesPerSecond:=FWavFormat.SampleRate;
  Format:=afS16;
end;

function TPAWavSource.LoadNextChunk: Boolean;
var
  RCount: LongInt;
begin
  Result := False;
  RCount := FStream.Read(FCurrentChunk, SizeOf(FCurrentChunk));
  if RCount < SizeOf(FCurrentChunk) then
  begin
    FCurrentChunk.Size:=0;
    FCurrentChunk.ID := '    ';
    Exit;
  end;
  FCurrentChunk.Size:=LeToN(FCurrentChunk.Size);
  Result := True;
end;

function TPAWavSource.GetStream: TStream;
begin
  Result := FStream;
end;

procedure TPAWavSource.SetStream(AValue: TStream);
begin
  if FStream = AValue then Exit;
  FStream := AValue;
  ReadHeader;
end;

function TPAWavSource.InternalOutputToDestination: Boolean;
var
  Buf: array[0..AUDIO_BUFFER_SIZE-1] of byte;
  RCount: Integer;
  WSize: Integer = 0;
  OutOfChunks: Boolean = False;
begin
  Result := False;
  if not FValid then
    Exit;

  while (WSize < SizeOf(Buf)) and not OutOfChunks do
  begin
    if FCurrentChunk.Size = 0 then
      repeat
        OutOfChunks := not LoadNextChunk;
        if OutOfChunks then
          Break;
      until FCurrentChunk.ID = AUDIO_CHUNK_ID_data;

    RCount := FStream.Read(Buf[WSize], Min(SizeOf(Buf)-WSize, FCurrentChunk.Size));
    Dec(FCurrentChunk.Size, RCount);
    Inc(WSize, RCount);
  end;

  Result := FStream.Position < FStream.Size;

  if WSize > 0 then
    WriteToBuffer(Buf, WSize, not Result);
end;

constructor TPAWavSource.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;
  FOwnsStream:=AOwnsStream;
  Stream := AStream;
end;

destructor TPAWavSource.Destroy;
begin
  if Assigned(FStream) and (FOwnsStream) then
    FreeAndNil(FStream);
  inherited Destroy;
end;

end.

