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
  Classes, SysUtils, pa_base, pa_stream, pa_register, paio_messagequeue,
  fpwavformat, paio_log;

type

  { TPAWavSource }

  TPAWavSource = class(TPAStreamSource, IPAPlayable, IPAStream)
  private
    FWavFormat: TWaveFormat;
    FValid: Boolean;
    FCurrentChunk: TChunkHeader;
    // Location of the PCM data chunk, established in ReadHeader, used for seeking
    // and position reporting. FDataPos is the byte offset within the data chunk
    // of the next sample to read; it is only written from the worker thread.
    FDataStart: Int64;
    FDataSize: Int64;
    FDataPos: Int64;
    FBlockAlign: Integer;
    FBytesPerSecond: Integer;
    procedure ReadHeader;
    function LoadNextChunk: Boolean;
    function FindDataChunk: Boolean;
  protected
    procedure SetStream(AValue: TStream); override;
    function InternalOutputToDestination: Boolean; override;
    function HandleMessage(var AMsg: TPAIOMessage): Boolean; override;
    // IPAPlayable
    function  CanSeek: Boolean;
    function  GetPosition: Double;
    procedure SetPosition(AValue: Double);
    function  GetMaxPosition: Double;
  public
    // IPAPlayable
    procedure Play;
    procedure Pause;
    procedure Stop;
    property  Position: Double read GetPosition write SetPosition;
    property  MaxPosition: Double read GetMaxPosition;
  end;

  TPAWavDest = class(TPAStreamDestination, IPAStream)
  private
    const
      cFileSizeOff = 4;
      cDataSizeOff = 40;
  private
    FInited: Boolean;
    FFinished: Boolean;
    FDataSize: DWord;
    procedure InitStream;
    procedure FinishStream;
  protected
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; override;
    procedure BeforeStreamFree; override;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean); override;
    property    Stream;
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

procedure TPAWavDest.InitStream;
var
  Riff: TRiffHeader;
  WavChunk: TWaveFormat;
  DataChunk: TChunkHeader;
begin
  FInited := True;
  TPALog.Info(ClassName, 'initialized');

  Riff.ChunkHeader.ID := AUDIO_CHUNK_ID_RIFF;
  Riff.Format := AUDIO_CHUNK_ID_WAVE;
  Riff.ChunkHeader.Size:=0; // fill in later

  WavChunk.ChunkHeader.ID := AUDIO_CHUNK_ID_fmt;
  WavChunk.ChunkHeader.Size:=16;
  WavChunk.Format := AUDIO_FORMAT_PCM;

  WavChunk.BitsPerSample := 16;
  WavChunk.Channels:=Channels;
  // SamplesPerSecond is the true frame rate (PulseAudio uses it directly as the
  // sample rate). BlockAlign = bytes per frame; ByteRate = bytes per second.
  WavChunk.SampleRate:= SamplesPerSecond;
  WavChunk.BlockAlign:= (WavChunk.BitsPerSample * Channels) div 8;
  WavChunk.ByteRate:= WavChunk.SampleRate * WavChunk.BlockAlign;
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

procedure TPAWavDest.BeforeStreamFree;
begin
  // ensure the RIFF/data sizes get patched even if the final buffer never
  // carried the last-data flag (e.g. data ended on a buffer boundary).
  if FInited then
    FinishStream;
end;

constructor TPAWavDest.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  Inherited Create(AStream, AOwnsStream);
  Format:= afS16;
end;

procedure TPAWavSource.ReadHeader;
var
  Riff: TRiffHeader;
  RCount: Integer;
begin
  FValid := False;

  if not Assigned(FStream) then
  begin
    TPALog.Warning(ClassName, 'init failed: no stream');
    Exit;
  end;
  FStream.Seek(0, soBeginning);
  RCount := FStream.Read(Riff, SizeOf(Riff));
  if RCount <> SizeOf(Riff) then
  begin
    TPALog.Warning(ClassName, 'init failed: stream too short for RIFF header');
    Exit;
  end;
  Riff.ChunkHeader.Size:=LEtoN(Riff.ChunkHeader.Size);
  FValid := (Riff.ChunkHeader.ID = AUDIO_CHUNK_ID_RIFF)
        and (Riff.Format = AUDIO_CHUNK_ID_WAVE);
  if not FValid then
  begin
    TPALog.Warning(ClassName, 'init failed: not a RIFF/WAVE file');
    Exit;
  end;
  FStream.Read(FWavFormat, SizeOf(FWavFormat));
  LEtoN(FWavFormat);
  FValid := (FWavFormat.ChunkHeader.ID = AUDIO_CHUNK_ID_fmt)
        and (FWavFormat.Format = AUDIO_FORMAT_PCM);
  if not FValid then
  begin
    TPALog.Warning(ClassName, 'init failed: missing or non-PCM fmt chunk');
    Exit;
  end;

  // We only read the 16 PCM bytes of the fmt body, but the chunk may be larger
  // (e.g. an 18-byte fmt carrying a cbSize field). Skip the remainder, honoring
  // RIFF word alignment, so the stream lands on the next chunk header.
  if FValid and (FWavFormat.ChunkHeader.Size > 16) then
    FStream.Seek((FWavFormat.ChunkHeader.Size - 16) + (FWavFormat.ChunkHeader.Size and 1), soCurrent);

  Channels:=FWavFormat.Channels;
  SamplesPerSecond:=FWavFormat.SampleRate;
  Format:=afS16;

  // Cache the frame geometry so seeking/position can convert between seconds and
  // byte offsets. BlockAlign = bytes per frame; ByteRate = bytes per second.
  FBlockAlign := (FWavFormat.BitsPerSample * FWavFormat.Channels) div 8;
  FBytesPerSecond := FWavFormat.SampleRate * FBlockAlign;

  // Locate the data chunk now (rather than lazily in InternalOutputToDestination)
  // so MaxPosition is known and seeks have a fixed reference point.
  FDataStart := 0;
  FDataSize := 0;
  FDataPos := 0;
  FillChar(FCurrentChunk, SizeOf(FCurrentChunk), 0);
  if not FindDataChunk then
    TPALog.Warning(ClassName, 'no data chunk found');

  TPALog.Info(ClassName, 'initialized');
end;

function TPAWavSource.FindDataChunk: Boolean;
begin
  // Scan forward from the current position (just past the fmt chunk) skipping
  // any non-data chunks (LIST/fact/PEAK, etc.) until the data chunk is reached.
  // On success the stream is left positioned at the start of the PCM body and
  // FCurrentChunk/FDataStart/FDataSize describe it.
  Result := False;
  while LoadNextChunk do
  begin
    if FCurrentChunk.ID = AUDIO_CHUNK_ID_data then
    begin
      FDataStart := FStream.Position;
      FDataSize := FCurrentChunk.Size;
      Result := True;
      Exit;
    end;
    // Skip the body; RIFF chunks are word-aligned so pad odd sizes.
    FStream.Seek(FCurrentChunk.Size + (FCurrentChunk.Size and 1), soCurrent);
  end;
end;

function TPAWavSource.LoadNextChunk: Boolean;
var
  RCount: LongInt;
  Remaining: Int64;
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

  // Clamp the data chunk to the bytes actually present. Handles a size of 0
  // (some writers leave it unset) and a size larger than the file -- truncated
  // files, or the 0xFFFFFFFF "until EOF" streaming sentinel. Without this an
  // oversized DWord size reaches the signed-Integer Min() in
  // InternalOutputToDestination as a negative count, giving a misbehaving read
  // and an effectively infinite loop.
  if FCurrentChunk.ID = AUDIO_CHUNK_ID_data then
  begin
    Remaining := FStream.Size - FStream.Position;
    if (FCurrentChunk.Size = 0) or (FCurrentChunk.Size > Remaining) then
      FCurrentChunk.Size := Remaining;
  end;

  Result := True;
end;

procedure TPAWavSource.SetStream(AValue: TStream);
begin
  inherited SetStream(AValue);
  if Assigned(AValue) then
    ReadHeader
  else
    FValid := False;
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
        if FCurrentChunk.ID <> AUDIO_CHUNK_ID_data then
        begin
          // Skip the body of a non-data chunk (e.g. LIST/fact/PEAK, which most
          // encoders -- including ffmpeg -- place between fmt and data). RIFF
          // chunks are word-aligned, so account for the pad byte on odd sizes.
          // Without this we read the chunk's body as if it were the next
          // chunk header and misparse the whole file.
          FStream.Seek(FCurrentChunk.Size + (FCurrentChunk.Size and 1), soCurrent);
          FCurrentChunk.Size := 0;
        end;
      until FCurrentChunk.ID = AUDIO_CHUNK_ID_data;

    RCount := FStream.Read(Buf[WSize], Min(SizeOf(Buf)-WSize, FCurrentChunk.Size));
    Dec(FCurrentChunk.Size, RCount);
    Inc(WSize, RCount);
    // A data chunk whose declared Size is larger than the bytes actually present
    // (truncated/ill-formed/streaming WAV) leaves FCurrentChunk.Size > 0 while
    // Read returns 0 at EOF. Without this guard the loop spins forever reading
    // nothing.
    if RCount = 0 then
      Break;
  end;

  Result := FStream.Position < FStream.Size;

  // Track how far into the data chunk we are (clamped) for position reporting.
  if FDataSize > 0 then
    FDataPos := Min(Max(FStream.Position - FDataStart, Int64(0)), FDataSize);

  if WSize > 0 then
    WriteToBuffer(Buf, WSize, not Result);

  // No more data: tell the destinations we're done. Without this the
  // destination worker never gets EndOfData and the pipeline hangs (the other
  // decoder sources all signal here too).
  if not Result then
    SignalDestinationsDone;
end;

function TPAWavSource.HandleMessage(var AMsg: TPAIOMessage): Boolean;
var
  lByte: Int64;
begin
  Result := True;
  case AMsg.Message of
    PAM_Seek:
      if FValid and (FBytesPerSecond > 0) then
      begin
        // seconds -> byte offset in the data chunk, snapped down to a frame
        // boundary and clamped to the chunk extent.
        lByte := Trunc(Double(AMsg.Data) * FBytesPerSecond);
        if lByte < 0 then
          lByte := 0;
        if lByte > FDataSize then
          lByte := FDataSize;
        lByte := lByte - (lByte mod FBlockAlign);
        FStream.Position := FDataStart + lByte;
        FCurrentChunk.ID := AUDIO_CHUNK_ID_data;
        FCurrentChunk.Size := FDataSize - lByte;
        FDataPos := lByte;
      end;
  else
    Result := False;
  end;
end;

function TPAWavSource.CanSeek: Boolean;
begin
  Result := FValid and (FBytesPerSecond > 0) and (FDataSize > 0);
end;

function TPAWavSource.GetPosition: Double;
begin
  if (not FValid) or (FBytesPerSecond = 0) then
    Exit(0);
  Result := FDataPos / FBytesPerSecond;
end;

procedure TPAWavSource.SetPosition(AValue: Double);
begin
  if not CanSeek then
    Exit;
  FMsgQueue.PostMessage(PAM_Seek, AValue);
end;

function TPAWavSource.GetMaxPosition: Double;
begin
  if (not FValid) or (FBytesPerSecond = 0) then
    Exit(0);
  Result := FDataSize / FBytesPerSecond;
end;

procedure TPAWavSource.Play;
begin

end;

procedure TPAWavSource.Pause;
begin

end;

procedure TPAWavSource.Stop;
begin

end;

initialization
  PARegister(partEncoder, TPAWavDest,   'Wave/PCM', '.wav' ,'RIFF', 4);
  PARegister(partDecoder, TPAWavSource, 'Wave/PCM', '.wav');
end.

