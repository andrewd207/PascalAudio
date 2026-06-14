{
    This unit is part of PascalAudioIO package.

    Copyright (c) 2026 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

{
  TMP4Muxer -- writes raw AAC frames into a minimal but valid MP4/M4A container.

  Layout produced (non-faststart; moov after mdat so chunk offsets are final the
  moment we write them):

    ftyp
    mdat   <- raw AAC frames, one after another; box size back-patched at Finalize
    moov
      mvhd
      trak
        tkhd
        mdia
          mdhd
          hdlr 'soun'
          minf
            smhd
            dinf/dref 'url ' (self-contained)
            stbl
              stsd/mp4a/esds   <- carries the encoder's AudioSpecificConfig
              stts             <- one entry: every frame is SamplesPerFrame long
              stsc             <- one entry: all samples in a single chunk
              stsz             <- per-frame sizes
              stco             <- one entry: absolute offset of the mdat payload

  Usage:
    M := TMP4Muxer.Create(OutStream, True, 44100, 2, 1024, ASCBytes);
    for each encoded AAC frame: M.AddSample(FrameData, FrameLen);
    M.Finalize;           // or just free it -- Destroy finalizes if needed
    M.Free;

  The AudioSpecificConfig (a.k.a. DecoderSpecificInfo / codec_private) comes from
  the AAC encoder (e.g. faacEncGetDecoderSpecificInfo); this unit does not invent
  it. The output stream must be seekable (mdat size + nothing else is patched, but
  Finalize seeks back to it).
}
unit mp4muxer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, paio_log;

type

  // 'data' atom well-known type codes.
  TMP4DataType = (mdtUTF8 = 1, mdtJPEG = 13, mdtPNG = 14);

  TMP4Tag = record
    FourCC: string;     // 4-byte ilst atom name, e.g. #$A9'nam' or 'covr'
    DataType: LongWord; // 'data' atom type indicator
    Value: TBytes;      // raw payload (UTF-8 text bytes, or image bytes)
  end;

  TMP4Chapter = record
    TimeSec: Double;
    Title: UTF8String;
  end;

const
  cChapterTrackID = 2; // the QuickTime text chapter track's track_ID

type

  { TMP4Muxer }

  TMP4Muxer = class
  private
    FStream: TStream;
    FOwnsStream: Boolean;
    FSampleRate: Integer;
    FChannels: Integer;
    FSamplesPerFrame: Integer;
    FASC: TBytes;                  // AudioSpecificConfig from the encoder
    FSampleSizes: array of LongWord;
    FSampleCount: Integer;
    FMdatSizePos: Int64;           // file offset of the mdat box size field
    FMdatDataStart: Int64;         // file offset of the first sample (stco)
    FMaxBitrate: LongWord;
    FAvgBitrate: LongWord;
    FTags: array of TMP4Tag;
    FChapters: array of TMP4Chapter;
    FTextSizes: array of LongWord;  // chapter text sample sizes (in mdat)
    FTextDataStart: Int64;          // file offset of first chapter text sample
    FStarted: Boolean;
    FFinalized: Boolean;
    procedure StartFile;
    procedure WriteMoov;
    procedure WriteMetadata(AOut: TStream);
    procedure WriteUdta(AOut: TStream);
    procedure AddRawTag(const AFourCC: string; ADataType: LongWord; const AValue: TBytes);
    procedure SortChapters;
    procedure WriteChapterSamples;          // emits chapter titles into mdat
    procedure WriteChpl(AOut: TStream);     // Nero chapter list (udta)
    procedure WriteChapterTrack(AOut: TStream); // QuickTime text track
    function  HasChapters: Boolean;
    function  TotalDuration: QWord;  // in media timescale units (== samples)
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean;
      ASampleRate, AChannels, ASamplesPerFrame: Integer;
      const AAudioSpecificConfig: TBytes);
    destructor Destroy; override;
    // append one raw AAC access unit (frame). No ADTS header.
    procedure AddSample(const AData; ASize: Integer);
    // iTunes-style metadata. Add tags before Finalize (they are written into the
    // moov). AddTextTag takes the raw 4-byte ilst atom name; the named helpers fill
    // in the conventional #$A9-prefixed fourccs.
    procedure AddTextTag(const AFourCC: string; const AValue: UTF8String);
    procedure SetTitle(const AValue: UTF8String);    // ©nam
    procedure SetArtist(const AValue: UTF8String);   // ©ART
    procedure SetAlbum(const AValue: UTF8String);     // ©alb
    procedure SetComment(const AValue: UTF8String);   // ©cmt
    procedure SetGenre(const AValue: UTF8String);     // ©gen
    procedure SetDate(const AValue: UTF8String);      // ©day
    procedure SetEncoder(const AValue: UTF8String);   // ©too
    // Embed cover art ('covr'). The image format (JPEG/PNG) is detected from the
    // data; pass it raw (a whole .jpg/.png file's bytes).
    procedure AddCoverArt(const AData; ASize: Integer);
    // Add a chapter marker starting at ATimeSec. Both a Nero 'chpl' list and a
    // QuickTime text chapter track are written, for broad + Apple compatibility.
    // The first chapter is treated as starting at 0. Call before Finalize.
    procedure AddChapter(ATimeSec: Double; const ATitle: UTF8String);
    // write ftyp/mdat patch/moov. Safe to call once; Destroy calls it if needed.
    procedure Finalize;
    property SampleCount: Integer read FSampleCount;
    // optional bitrate hints written into the DecoderConfigDescriptor.
    property MaxBitrate: LongWord read FMaxBitrate write FMaxBitrate;
    property AvgBitrate: LongWord read FAvgBitrate write FAvgBitrate;
  end;

implementation

{ low-level big-endian writers }

procedure PutU8(S: TStream; V: Byte); inline;
begin
  S.WriteBuffer(V, 1);
end;

procedure PutU16(S: TStream; V: Word); inline;
var b: array[0..1] of Byte;
begin
  b[0] := (V shr 8) and $FF; b[1] := V and $FF;
  S.WriteBuffer(b, 2);
end;

procedure PutU24(S: TStream; V: LongWord); inline;
var b: array[0..2] of Byte;
begin
  b[0] := (V shr 16) and $FF; b[1] := (V shr 8) and $FF; b[2] := V and $FF;
  S.WriteBuffer(b, 3);
end;

procedure PutU32(S: TStream; V: LongWord); inline;
var b: array[0..3] of Byte;
begin
  b[0] := (V shr 24) and $FF; b[1] := (V shr 16) and $FF;
  b[2] := (V shr 8)  and $FF; b[3] := V and $FF;
  S.WriteBuffer(b, 4);
end;

procedure PutU64(S: TStream; V: QWord); inline;
begin
  PutU32(S, LongWord(V shr 32));
  PutU32(S, LongWord(V and $FFFFFFFF));
end;

procedure PutFourCC(S: TStream; const ACC: string); inline;
begin
  // ACC is always 4 ASCII chars.
  S.WriteBuffer(ACC[1], 4);
end;

// MPEG-4 descriptor "expandable" length, always 4 bytes (widely accepted form).
procedure PutDescrLen(S: TStream; ALen: LongWord);
var b: array[0..3] of Byte;
begin
  b[0] := ((ALen shr 21) and $7F) or $80;
  b[1] := ((ALen shr 14) and $7F) or $80;
  b[2] := ((ALen shr 7)  and $7F) or $80;
  b[3] :=  (ALen and $7F);
  S.WriteBuffer(b, 4);
end;

// Begin an atom: write a placeholder size + name, return the size-field position.
function BeginAtom(S: TStream; const AName: string): Int64;
begin
  Result := S.Position;
  PutU32(S, 0);          // size, patched by EndAtom
  PutFourCC(S, AName);
end;

// Patch the atom size to (current position - size-field position).
procedure EndAtom(S: TStream; ASizePos: Int64);
var
  EndPos: Int64;
begin
  EndPos := S.Position;
  S.Position := ASizePos;
  PutU32(S, LongWord(EndPos - ASizePos));
  S.Position := EndPos;
end;

// 3x3 video transform matrix, identity (required even for audio-only tracks).
procedure PutIdentityMatrix(S: TStream);
begin
  PutU32(S, $00010000); PutU32(S, 0);          PutU32(S, 0);
  PutU32(S, 0);          PutU32(S, $00010000); PutU32(S, 0);
  PutU32(S, 0);          PutU32(S, 0);          PutU32(S, $40000000);
end;

{ TMP4Muxer }

constructor TMP4Muxer.Create(AStream: TStream; AOwnsStream: Boolean;
  ASampleRate, AChannels, ASamplesPerFrame: Integer;
  const AAudioSpecificConfig: TBytes);
begin
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  FSampleRate := ASampleRate;
  FChannels := AChannels;
  FSamplesPerFrame := ASamplesPerFrame;
  FASC := Copy(AAudioSpecificConfig);
  FSampleCount := 0;
  SetLength(FSampleSizes, 0);
  if Length(FASC) = 0 then
    TPALog.Warning(ClassName, 'no AudioSpecificConfig given; the file will not be decodable');
end;

destructor TMP4Muxer.Destroy;
begin
  // make sure an interrupted mux still produces a complete file.
  if FStarted and not FFinalized then
    Finalize;
  if FOwnsStream then
    FStream.Free;
  inherited Destroy;
end;

procedure TMP4Muxer.StartFile;
var
  ftypPos: Int64;
begin
  if FStarted then
    Exit;
  FStarted := True;

  // ftyp: major brand M4A , then compatible brands.
  ftypPos := BeginAtom(FStream, 'ftyp');
  PutFourCC(FStream, 'M4A ');     // major brand
  PutU32(FStream, 0);             // minor version
  PutFourCC(FStream, 'M4A ');     // compatible brands...
  PutFourCC(FStream, 'mp42');
  PutFourCC(FStream, 'isom');
  EndAtom(FStream, ftypPos);

  // mdat: write the header now, remember where to patch the size, and where the
  // payload starts (that offset is what stco points at).
  FMdatSizePos := FStream.Position;
  PutU32(FStream, 0);             // size, patched in Finalize
  PutFourCC(FStream, 'mdat');
  FMdatDataStart := FStream.Position;
end;

procedure TMP4Muxer.AddSample(const AData; ASize: Integer);
begin
  if FFinalized then
  begin
    TPALog.Error(ClassName, 'AddSample after Finalize; ignored');
    Exit;
  end;
  if not FStarted then
    StartFile;
  if ASize <= 0 then
    Exit;

  FStream.WriteBuffer(AData, ASize);

  if FSampleCount >= Length(FSampleSizes) then
    SetLength(FSampleSizes, (FSampleCount + 1) * 2);
  FSampleSizes[FSampleCount] := LongWord(ASize);
  Inc(FSampleCount);
end;

function TMP4Muxer.TotalDuration: QWord;
begin
  Result := QWord(FSampleCount) * QWord(FSamplesPerFrame);
end;

const
  cC = #$A9; // the iTunes copyright-sign metadata prefix

procedure TMP4Muxer.AddRawTag(const AFourCC: string; ADataType: LongWord; const AValue: TBytes);
var
  n: Integer;
begin
  if FFinalized then
  begin
    TPALog.Warning(ClassName, 'metadata added after Finalize is ignored');
    Exit;
  end;
  if Length(AFourCC) <> 4 then
  begin
    TPALog.Warning(ClassName, 'ignoring metadata tag with non-4-byte name');
    Exit;
  end;
  n := Length(FTags);
  SetLength(FTags, n + 1);
  FTags[n].FourCC := AFourCC;
  FTags[n].DataType := ADataType;
  FTags[n].Value := AValue;
end;

procedure TMP4Muxer.AddTextTag(const AFourCC: string; const AValue: UTF8String);
var
  Bytes: TBytes;
begin
  SetLength(Bytes, Length(AValue));
  if Length(AValue) > 0 then
    Move(AValue[1], Bytes[0], Length(AValue));
  AddRawTag(AFourCC, Ord(mdtUTF8), Bytes);
end;

procedure TMP4Muxer.AddCoverArt(const AData; ASize: Integer);
var
  Bytes: TBytes;
  p: PByte;
  dt: LongWord;
begin
  if ASize <= 0 then
    Exit;
  p := @AData;
  // sniff the format: PNG = 89 50 4E 47, JPEG = FF D8.
  if (ASize >= 4) and (p[0] = $89) and (p[1] = $50) and (p[2] = $4E) and (p[3] = $47) then
    dt := Ord(mdtPNG)
  else if (ASize >= 2) and (p[0] = $FF) and (p[1] = $D8) then
    dt := Ord(mdtJPEG)
  else
  begin
    TPALog.Warning(ClassName, 'cover art is neither PNG nor JPEG; tagging as JPEG');
    dt := Ord(mdtJPEG);
  end;
  SetLength(Bytes, ASize);
  Move(AData, Bytes[0], ASize);
  AddRawTag('covr', dt, Bytes);
end;

procedure TMP4Muxer.AddChapter(ATimeSec: Double; const ATitle: UTF8String);
var
  n: Integer;
begin
  if FFinalized then
  begin
    TPALog.Warning(ClassName, 'chapter added after Finalize is ignored');
    Exit;
  end;
  if ATimeSec < 0 then
    ATimeSec := 0;
  n := Length(FChapters);
  SetLength(FChapters, n + 1);
  FChapters[n].TimeSec := ATimeSec;
  FChapters[n].Title := ATitle;
end;

function TMP4Muxer.HasChapters: Boolean;
begin
  Result := Length(FChapters) > 0;
end;

procedure TMP4Muxer.SortChapters;
var
  i, j: Integer;
  tmp: TMP4Chapter;
begin
  // small lists; insertion sort by time. Then clamp the first to 0 so the chapter
  // track covers the whole timeline from the start.
  for i := 1 to High(FChapters) do
  begin
    tmp := FChapters[i];
    j := i - 1;
    while (j >= 0) and (FChapters[j].TimeSec > tmp.TimeSec) do
    begin
      FChapters[j+1] := FChapters[j];
      Dec(j);
    end;
    FChapters[j+1] := tmp;
  end;
  if Length(FChapters) > 0 then
    FChapters[0].TimeSec := 0;
end;

procedure TMP4Muxer.SetTitle(const AValue: UTF8String);   begin AddTextTag(cC+'nam', AValue); end;
procedure TMP4Muxer.SetArtist(const AValue: UTF8String);  begin AddTextTag(cC+'ART', AValue); end;
procedure TMP4Muxer.SetAlbum(const AValue: UTF8String);   begin AddTextTag(cC+'alb', AValue); end;
procedure TMP4Muxer.SetComment(const AValue: UTF8String); begin AddTextTag(cC+'cmt', AValue); end;
procedure TMP4Muxer.SetGenre(const AValue: UTF8String);   begin AddTextTag(cC+'gen', AValue); end;
procedure TMP4Muxer.SetDate(const AValue: UTF8String);    begin AddTextTag(cC+'day', AValue); end;
procedure TMP4Muxer.SetEncoder(const AValue: UTF8String); begin AddTextTag(cC+'too', AValue); end;

procedure TMP4Muxer.WriteMetadata(AOut: TStream);
var
  pMeta, pHdlr, pIlst, pItem, pData: Int64;
  i: Integer;
begin
  // meta is a FULL box: a version/flags word precedes its child atoms.
  pMeta := BeginAtom(AOut, 'meta');
    PutU32(AOut, 0);                 // version/flags

    pHdlr := BeginAtom(AOut, 'hdlr');
      PutU32(AOut, 0);               // version/flags
      PutU32(AOut, 0);               // pre_defined
      PutFourCC(AOut, 'mdir');       // handler_type: metadata
      PutFourCC(AOut, 'appl');       // reserved[0] (iTunes convention)
      PutU32(AOut, 0); PutU32(AOut, 0); // reserved[1..2]
      PutU8(AOut, 0);                // name (empty, null-terminated)
    EndAtom(AOut, pHdlr);

    pIlst := BeginAtom(AOut, 'ilst');
      for i := 0 to High(FTags) do
      begin
        pItem := BeginAtom(AOut, FTags[i].FourCC);
          pData := BeginAtom(AOut, 'data');
            PutU32(AOut, FTags[i].DataType); // type: 1=UTF-8, 13=JPEG, 14=PNG
            PutU32(AOut, $00000000);         // locale / country+language
            if Length(FTags[i].Value) > 0 then
              AOut.WriteBuffer(FTags[i].Value[0], Length(FTags[i].Value));
          EndAtom(AOut, pData);
        EndAtom(AOut, pItem);
      end;
    EndAtom(AOut, pIlst);

  EndAtom(AOut, pMeta);
end;

procedure TMP4Muxer.WriteChpl(AOut: TStream);
var
  i, n, len: Integer;
  s: UTF8String;
  pChpl: Int64;
begin
  // Nero ChapterListBox: version 1, a reserved 32-bit, a 1-byte count, then per
  // chapter an 8-byte start time in 100-nanosecond units and a counted UTF-8
  // title. Limited to 255 chapters.
  n := Length(FChapters);
  if n > 255 then
    n := 255;
  pChpl := BeginAtom(AOut, 'chpl');
    PutU8(AOut, 1);            // version
    PutU24(AOut, 0);           // flags
    PutU32(AOut, 0);           // reserved (present for version != 0)
    PutU8(AOut, n);            // chapter count
    for i := 0 to n - 1 do
    begin
      PutU64(AOut, Round(FChapters[i].TimeSec * 10000000.0)); // 100-ns units
      s := FChapters[i].Title;
      len := Length(s);
      if len > 255 then
        len := 255;
      PutU8(AOut, len);
      if len > 0 then
        AOut.WriteBuffer(s[1], len);
    end;
  EndAtom(AOut, pChpl);
end;

procedure TMP4Muxer.WriteUdta(AOut: TStream);
var
  pUdta: Int64;
begin
  if (not HasChapters) and (Length(FTags) = 0) then
    Exit;
  pUdta := BeginAtom(AOut, 'udta');
    if HasChapters then
      WriteChpl(AOut);
    if Length(FTags) > 0 then
      WriteMetadata(AOut);
  EndAtom(AOut, pUdta);
end;

procedure TMP4Muxer.Finalize;
var
  MdatEnd: Int64;
begin
  if FFinalized then
    Exit;
  if not FStarted then
    StartFile; // produce a (sample-less) but structurally valid file
  FFinalized := True;

  // chapter title samples also live in mdat, after the audio.
  if HasChapters then
  begin
    SortChapters;
    WriteChapterSamples;
  end;

  // patch the mdat box size now that all samples are written.
  MdatEnd := FStream.Position;
  FStream.Position := FMdatSizePos;
  PutU32(FStream, LongWord(MdatEnd - FMdatSizePos));
  FStream.Position := MdatEnd;

  WriteMoov;
end;

procedure TMP4Muxer.WriteChapterSamples;
var
  i: Integer;
  s: UTF8String;
  len: Word;
begin
  FTextDataStart := FStream.Position;
  SetLength(FTextSizes, Length(FChapters));
  for i := 0 to High(FChapters) do
  begin
    s := FChapters[i].Title;
    if Length(s) > High(Word) then
      SetLength(s, High(Word));
    len := Length(s);
    // QuickTime text sample: 16-bit length prefix then the UTF-8 bytes.
    PutU16(FStream, len);
    if len > 0 then
      FStream.WriteBuffer(s[1], len);
    FTextSizes[i] := 2 + len;
  end;
end;

procedure TMP4Muxer.WriteChapterTrack(AOut: TStream);
var
  pTrak, pMdia, pMinf, pStbl, pAtom, pInner: Int64;
  i, nch: Integer;
  units: array of QWord;
  dur: QWord;
  total: QWord;
begin
  nch := Length(FChapters);
  total := TotalDuration;

  // chapter start times -> media-timescale units (timescale == sample rate).
  SetLength(units, nch + 1);
  for i := 0 to nch - 1 do
    units[i] := Round(FChapters[i].TimeSec * FSampleRate);
  units[0] := 0;            // first chapter anchors the track at 0
  units[nch] := total;      // sentinel end

  pTrak := BeginAtom(AOut, 'trak');

    // tkhd: track 2, disabled flag set (0) so players don't render the text.
    pAtom := BeginAtom(AOut, 'tkhd');
      PutU32(AOut, $00000001);        // version 0, flags = track_enabled only
      PutU32(AOut, 0); PutU32(AOut, 0);
      PutU32(AOut, cChapterTrackID);  // track_ID
      PutU32(AOut, 0);
      PutU32(AOut, LongWord(total));
      PutU32(AOut, 0); PutU32(AOut, 0);
      PutU16(AOut, 0);                // layer
      PutU16(AOut, 0);                // alternate_group
      PutU16(AOut, 0);                // volume (text = 0)
      PutU16(AOut, 0);
      PutIdentityMatrix(AOut);
      PutU32(AOut, 0);                // width
      PutU32(AOut, 0);                // height
    EndAtom(AOut, pAtom);

    pMdia := BeginAtom(AOut, 'mdia');

      pAtom := BeginAtom(AOut, 'mdhd');
        PutU32(AOut, 0);
        PutU32(AOut, 0); PutU32(AOut, 0);
        PutU32(AOut, FSampleRate);    // timescale (match audio)
        PutU32(AOut, LongWord(total));
        PutU16(AOut, $55C4);          // 'und'
        PutU16(AOut, 0);
      EndAtom(AOut, pAtom);

      pAtom := BeginAtom(AOut, 'hdlr');
        PutU32(AOut, 0);
        PutU32(AOut, 0);
        PutFourCC(AOut, 'text');      // handler_type: text
        PutU32(AOut, 0); PutU32(AOut, 0); PutU32(AOut, 0);
        PutU8(AOut, 0);
      EndAtom(AOut, pAtom);

      pMinf := BeginAtom(AOut, 'minf');

        // gmhd / gmin -- generic (text) media header.
        pAtom := BeginAtom(AOut, 'gmhd');
          pInner := BeginAtom(AOut, 'gmin');
            PutU32(AOut, 0);          // version/flags
            PutU16(AOut, $0040);      // graphics mode (copy)
            PutU16(AOut, 0); PutU16(AOut, 0); PutU16(AOut, 0); // opcolor
            PutU16(AOut, 0);          // balance
            PutU16(AOut, 0);          // reserved
          EndAtom(AOut, pInner);
        EndAtom(AOut, pAtom);

        // dinf/dref (self-contained)
        pAtom := BeginAtom(AOut, 'dinf');
          pInner := BeginAtom(AOut, 'dref');
            PutU32(AOut, 0);
            PutU32(AOut, 1);
            pStbl := BeginAtom(AOut, 'url ');
              PutU32(AOut, $00000001);
            EndAtom(AOut, pStbl);
          EndAtom(AOut, pInner);
        EndAtom(AOut, pAtom);

        pStbl := BeginAtom(AOut, 'stbl');

          // stsd: a single QuickTime 'text' sample description.
          pAtom := BeginAtom(AOut, 'stsd');
            PutU32(AOut, 0);
            PutU32(AOut, 1);          // entry_count
            pInner := BeginAtom(AOut, 'text');
              PutU16(AOut, 0); PutU16(AOut, 0); PutU16(AOut, 0); // reserved[6]
              PutU16(AOut, 1);        // data_reference_index
              PutU32(AOut, 0);        // displayFlags
              PutU32(AOut, 1);        // textJustification (center=1)
              PutU16(AOut, 0); PutU16(AOut, 0); PutU16(AOut, 0); // bgColor
              PutU16(AOut, 0); PutU16(AOut, 0); PutU16(AOut, 0); PutU16(AOut, 0); // defaultTextBox
              PutU32(AOut, 0); PutU32(AOut, 0); // reserved
              PutU16(AOut, 0);        // fontNumber
              PutU16(AOut, 0);        // fontFace
              PutU8(AOut, 0);         // reserved
              PutU16(AOut, 0);        // reserved
              PutU16(AOut, $FFFF); PutU16(AOut, $FFFF); PutU16(AOut, $FFFF); // fgColor (white)
              PutU8(AOut, 0);         // textName (counted string, empty)
            EndAtom(AOut, pInner);
          EndAtom(AOut, pAtom);

          // stts: one entry per chapter, duration = gap to the next chapter.
          pAtom := BeginAtom(AOut, 'stts');
            PutU32(AOut, 0);
            PutU32(AOut, LongWord(nch));
            for i := 0 to nch - 1 do
            begin
              dur := units[i+1] - units[i];
              PutU32(AOut, 1);
              PutU32(AOut, LongWord(dur));
            end;
          EndAtom(AOut, pAtom);

          // stsc: all chapter samples in one chunk.
          pAtom := BeginAtom(AOut, 'stsc');
            PutU32(AOut, 0);
            PutU32(AOut, 1);
            PutU32(AOut, 1);
            PutU32(AOut, LongWord(nch));
            PutU32(AOut, 1);
          EndAtom(AOut, pAtom);

          // stsz: per-sample sizes (2-byte length prefix + title bytes).
          pAtom := BeginAtom(AOut, 'stsz');
            PutU32(AOut, 0);
            PutU32(AOut, 0);
            PutU32(AOut, LongWord(nch));
            for i := 0 to nch - 1 do
              PutU32(AOut, FTextSizes[i]);
          EndAtom(AOut, pAtom);

          // stco: single chunk offset into mdat (the chapter sample region).
          pAtom := BeginAtom(AOut, 'stco');
            PutU32(AOut, 0);
            PutU32(AOut, 1);
            PutU32(AOut, LongWord(FTextDataStart));
          EndAtom(AOut, pAtom);

        EndAtom(AOut, pStbl);
      EndAtom(AOut, pMinf);
    EndAtom(AOut, pMdia);
  EndAtom(AOut, pTrak);
end;

procedure TMP4Muxer.WriteMoov;
var
  Moov: TMemoryStream;
  pMoov, pTrak, pMdia, pMinf, pStbl, pStsd, pMp4a, pEsds: Int64;
  ascLen, dsiContent, dcdContent, esContent: LongWord;
  i: Integer;
begin
  ascLen := Length(FASC);
  // descriptor content lengths (each nested descriptor header is tag(1)+len(4)).
  dsiContent := ascLen;                       // DecoderSpecificInfo (0x05)
  dcdContent := 13 + (5 + dsiContent);        // DecoderConfigDescriptor (0x04)
  esContent  := 3 + (5 + dcdContent) + (5 + 1); // ES_Descriptor (0x03): hdr + DCD + SLConfig

  Moov := TMemoryStream.Create;
  pMoov := BeginAtom(Moov, 'moov');

    // mvhd
    pStsd := BeginAtom(Moov, 'mvhd');
      PutU32(Moov, 0);
      PutU32(Moov, 0);
      PutU32(Moov, 0);
      PutU32(Moov, FSampleRate);
      PutU32(Moov, LongWord(TotalDuration));
      PutU32(Moov, $00010000);
      PutU16(Moov, $0100);
      PutU16(Moov, 0);
      PutU32(Moov, 0); PutU32(Moov, 0);
      PutIdentityMatrix(Moov);
      PutU32(Moov, 0); PutU32(Moov, 0); PutU32(Moov, 0);
      PutU32(Moov, 0); PutU32(Moov, 0); PutU32(Moov, 0);
      if HasChapters then
        PutU32(Moov, cChapterTrackID + 1)  // tracks 1 (audio) + 2 (text) used
      else
        PutU32(Moov, 2);
    EndAtom(Moov, pStsd);

    // trak
    pTrak := BeginAtom(Moov, 'trak');

      // tkhd
      pStsd := BeginAtom(Moov, 'tkhd');
        PutU32(Moov, $00000007);         // version 0, flags: enabled|in movie|in preview
        PutU32(Moov, 0);                 // creation
        PutU32(Moov, 0);                 // modification
        PutU32(Moov, 1);                 // track_ID
        PutU32(Moov, 0);                 // reserved
        PutU32(Moov, LongWord(TotalDuration));
        PutU32(Moov, 0); PutU32(Moov, 0); // reserved
        PutU16(Moov, 0);                 // layer
        PutU16(Moov, 0);                 // alternate_group
        PutU16(Moov, $0100);             // volume 1.0 (audio)
        PutU16(Moov, 0);                 // reserved
        PutIdentityMatrix(Moov);
        PutU32(Moov, 0);                 // width
        PutU32(Moov, 0);                 // height
      EndAtom(Moov, pStsd);

      // tref/chap -> point the audio track at the text chapter track.
      if HasChapters then
      begin
        pStsd := BeginAtom(Moov, 'tref');
          pMp4a := BeginAtom(Moov, 'chap');
            PutU32(Moov, cChapterTrackID);
          EndAtom(Moov, pMp4a);
        EndAtom(Moov, pStsd);
      end;

      // mdia
      pMdia := BeginAtom(Moov, 'mdia');

        // mdhd
        pStsd := BeginAtom(Moov, 'mdhd');
          PutU32(Moov, 0);
          PutU32(Moov, 0);
          PutU32(Moov, 0);
          PutU32(Moov, FSampleRate);     // timescale
          PutU32(Moov, LongWord(TotalDuration));
          PutU16(Moov, $55C4);           // language 'und'
          PutU16(Moov, 0);               // pre_defined
        EndAtom(Moov, pStsd);

        // hdlr
        pStsd := BeginAtom(Moov, 'hdlr');
          PutU32(Moov, 0);
          PutU32(Moov, 0);               // pre_defined
          PutFourCC(Moov, 'soun');       // handler_type
          PutU32(Moov, 0); PutU32(Moov, 0); PutU32(Moov, 0); // reserved
          PutFourCC(Moov, 'PASa');       // name (null-terminated)
          PutU8(Moov, 0);
        EndAtom(Moov, pStsd);

        // minf
        pMinf := BeginAtom(Moov, 'minf');

          // smhd
          pStsd := BeginAtom(Moov, 'smhd');
            PutU32(Moov, 0);
            PutU16(Moov, 0);             // balance
            PutU16(Moov, 0);             // reserved
          EndAtom(Moov, pStsd);

          // dinf/dref
          pStsd := BeginAtom(Moov, 'dinf');
            pMp4a := BeginAtom(Moov, 'dref');
              PutU32(Moov, 0);           // version/flags
              PutU32(Moov, 1);           // entry_count
              pEsds := BeginAtom(Moov, 'url ');
                PutU32(Moov, $00000001); // flags: media in same file
              EndAtom(Moov, pEsds);
            EndAtom(Moov, pMp4a);
          EndAtom(Moov, pStsd);

          // stbl
          pStbl := BeginAtom(Moov, 'stbl');

            // stsd -> mp4a -> esds
            pStsd := BeginAtom(Moov, 'stsd');
              PutU32(Moov, 0);           // version/flags
              PutU32(Moov, 1);           // entry_count
              pMp4a := BeginAtom(Moov, 'mp4a');
                // SampleEntry
                PutU16(Moov, 0); PutU16(Moov, 0); PutU16(Moov, 0); // reserved[6]
                PutU16(Moov, 1);         // data_reference_index
                // AudioSampleEntry
                PutU32(Moov, 0);         // version/revision
                PutU32(Moov, 0);         // vendor
                PutU16(Moov, FChannels);
                PutU16(Moov, 16);        // sample size (bits)
                PutU16(Moov, 0);         // pre_defined
                PutU16(Moov, 0);         // reserved
                PutU32(Moov, LongWord(FSampleRate) shl 16); // 16.16 fixed
                // esds
                pEsds := BeginAtom(Moov, 'esds');
                  PutU32(Moov, 0);       // version/flags
                  // ES_Descriptor
                  PutU8(Moov, $03);
                  PutDescrLen(Moov, esContent);
                  PutU16(Moov, 0);       // ES_ID
                  PutU8(Moov, 0);        // flags
                  // DecoderConfigDescriptor
                  PutU8(Moov, $04);
                  PutDescrLen(Moov, dcdContent);
                  PutU8(Moov, $40);      // objectTypeIndication: Audio ISO/IEC 14496-3 (AAC)
                  PutU8(Moov, $15);      // streamType=5 (audio), upstream=0, reserved=1
                  PutU24(Moov, 0);       // bufferSizeDB
                  PutU32(Moov, FMaxBitrate);
                  PutU32(Moov, FAvgBitrate);
                  // DecoderSpecificInfo (the AudioSpecificConfig)
                  PutU8(Moov, $05);
                  PutDescrLen(Moov, dsiContent);
                  if ascLen > 0 then
                    Moov.WriteBuffer(FASC[0], ascLen);
                  // SLConfigDescriptor
                  PutU8(Moov, $06);
                  PutDescrLen(Moov, 1);
                  PutU8(Moov, $02);      // predefined: MP4
                EndAtom(Moov, pEsds);
              EndAtom(Moov, pMp4a);
            EndAtom(Moov, pStsd);

            // stts: one run, every frame SamplesPerFrame long
            pStsd := BeginAtom(Moov, 'stts');
              PutU32(Moov, 0);
              PutU32(Moov, 1);           // entry_count
              PutU32(Moov, LongWord(FSampleCount));
              PutU32(Moov, LongWord(FSamplesPerFrame));
            EndAtom(Moov, pStsd);

            // stsc: one chunk holding all samples
            pStsd := BeginAtom(Moov, 'stsc');
              PutU32(Moov, 0);
              PutU32(Moov, 1);           // entry_count
              PutU32(Moov, 1);           // first_chunk
              PutU32(Moov, LongWord(FSampleCount)); // samples_per_chunk
              PutU32(Moov, 1);           // sample_description_index
            EndAtom(Moov, pStsd);

            // stsz: per-sample sizes
            pStsd := BeginAtom(Moov, 'stsz');
              PutU32(Moov, 0);
              PutU32(Moov, 0);           // sample_size 0 => sizes follow
              PutU32(Moov, LongWord(FSampleCount));
              for i := 0 to FSampleCount - 1 do
                PutU32(Moov, FSampleSizes[i]);
            EndAtom(Moov, pStsd);

            // stco: single chunk offset (absolute, into mdat payload)
            pStsd := BeginAtom(Moov, 'stco');
              PutU32(Moov, 0);
              PutU32(Moov, 1);           // entry_count
              PutU32(Moov, LongWord(FMdatDataStart));
            EndAtom(Moov, pStsd);

          EndAtom(Moov, pStbl);
        EndAtom(Moov, pMinf);
      EndAtom(Moov, pMdia);
    EndAtom(Moov, pTrak);

    // the QuickTime text chapter track (track 2), if any.
    if HasChapters then
      WriteChapterTrack(Moov);

    // udta: Nero chapter list + iTunes metadata.
    WriteUdta(Moov);
  EndAtom(Moov, pMoov);

  Moov.Position := 0;
  FStream.CopyFrom(Moov, Moov.Size);
  Moov.Free;

  if FMdatDataStart > High(LongWord) then
    TPALog.Warning(ClassName, 'mdat exceeds 4GB; 32-bit stco offsets are invalid (needs co64)');
end;

end.
