{
    This unit is part of the PascalAudio project.

    Copyright (c) 2020 by Andrew Haines.

    See the files COPYING.modifiedLGPL and license.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit quicktimeatoms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TAtomName = record
    case integer of
     0: (Int: Cardinal);
     1: (Bytes: array[0..3] of byte);
     2: (Chars: array[0..3] of char);
  end;

  TAtom = class;
  TAtomClass = class of TAtom;
  TAtomList = class;

  TAtomFoundEvent = function (Sender: TAtomList; AAtom: TAtom): Boolean of object;

  TAtomClassLookup = function (Sender: TAtomList; AParent: TAtom; AStream: TStream): TAtomClass of object;

  { TAtomList }

  TAtomList = class(TList)
  private
    FParent: TAtom;
    function GetAtom(AIndex: Integer): TAtom;
    procedure SetAtom(AIndex: Integer; AValue: TAtom);
  public
    constructor Create(AParentAtom: TAtom);
    destructor Destroy; override;
    procedure LoadAtoms(AStream: TStream; AFoundEvent: TAtomFoundEvent; AMaxPos: QWord; AClassLookup: TAtomClassLookup = nil);
    // AAtomIndex is the times the Atom is found. Not the Actual Child Index from the parent
    function FindAtom(AAtomName: TAtomName; var AtomResult: TAtom; AAtomOccurence: Integer = 1): Boolean;
    function FindAtom(AAtomPath: RawByteString): TAtom;
    //function FindAtom(AAtomName: String; var AtomResult: TAtom; AfterAtom: TAtom = nil): Boolean;
    property Atom[AIndex: Integer]: TAtom read GetAtom write SetAtom; default;
    property Parent: TAtom read FParent;
  end;

  { TAtom }

  TAtom = class
  private
    FAtomPosition: Int64;
    FAtomSize: QWord;
    FAtomName: TAtomName;
    FAtoms: TAtomList;
    FClassLookup: TAtomClassLookup;
    function GetAtoms: TAtomList;
    procedure LoadHeader;
    procedure LoadAtoms;
  protected
    FDataOffset: Byte;
    FStream: TStream;
    function DataSize: QWord;
    property AtomClassLookup: TAtomClassLookup read FClassLookup write FClassLookup;
  public
    procedure StreamSeek(ASeekPosition: TSeekOrigin; AOffset: PtrInt = 0);
    class function PeekAtom(AStream: TStream; out AAtomName: TAtomName): Boolean;
    procedure SaveToStream(AStream: TStream);
    property AtomName: TAtomName read FAtomName;
    property AtomSize: QWord read FAtomSize;
    property Atoms: TAtomList read GetAtoms;
    property StreamPosition: Int64 read FAtomPosition;
    constructor Create(AStream: TStream); virtual;
    destructor Destroy; override;
    class function CreateAtomClass(AStream: TStream): TAtom;
    function DataAsString:String;
    function DataAsDWord: DWord;
  end;

  TftypAtom = class(TAtom)
  private
    function GetAltBrand(AIndex: Integer): String;
    function GetAltBrandCount: Integer;
    function GetMajorBrand: String;
    function GetVersion: LongWord;
  public
   property MajorBrand: String read GetMajorBrand;
   property Version: LongWord read GetVersion;
   property AltBrandCount: Integer read GetAltBrandCount;
   property AltBrand[AIndex: Integer]: String read GetAltBrand;
  end;

  { TsttsAtom }

  TsttsAtom = class(TAtom)
  private
    function GetCount: Integer;
    function GetItem(AIndex: Integer): Integer;
  public
    property Count: Integer read GetCount;
    property Item[AIndex: Integer]: Integer read GetItem;
  end;

  { TstszAtom }

  TstszAtom = class(TAtom)   // sample size atom
  private
  const
    cVersionPos = 8; // 1 byte
    cFlagsPos = 9; // 3 bytes
    cSampleSizePos = cFlagsPos + 3;
    cNumEntriesPos = cSampleSizePos + 4;
    cSampleSizeTablePos = cNumEntriesPos +4;
  function GetSampleCount: LongWord;
  function GetFlags: LongWord;
  function GetSampleSizeEntry(AIndex: LongWord): LongWord;
  function GetSampleSize: LongWord;
  function GetVersion: Byte;
  public
    property Version: Byte read GetVersion;
    property Flags: LongWord read GetFlags;
    property SampleSize: LongWord read GetSampleSize;
    property SampleCount: LongWord read GetSampleCount;
    property SampleSizeEntry[AIndex: LongWord]: LongWord read GetSampleSizeEntry;
    procedure CopySampleTable(ABuffer: PDword);
  end;

  { TstcoAtom }

  TstcoAtom = class(TAtom)
    FVersion: Byte;
    FFlags: DWord;
    FEntries: DWord;
  private
    function GetOffset(AIndex: Integer): DWord;
    function GetStringFromOffset(AIndex: Integer): String;
  public
    constructor Create(AStream: TStream); override;
    property Count: DWord read FEntries;
    property Offset[AIndex: Integer]: DWord read GetOffset;
    property StringFromOffset[AIndex: Integer]: String read GetStringFromOffset;
    procedure CopyTable(ABuffer: PDWord);
  end;


  { TmetaAtom }

  TmetaAtom = class(TAtom)
    FVersion: Byte;
    FFlags: DWord;
    constructor Create(AStream: TStream); override;
  end;

  { TstscAtom }

  TstscAtom = class(TmetaAtom)
  private
    type
      TChunkEntry = record
        FirstChunk,
        SamplesPerChunk,
        SampleDescriptionID: DWord;
      end;
   private

    FEntryCount: DWord;
    FEntries: Array of TChunkEntry;
  public
   constructor Create(AStream: TStream); override;
   function SampleIndexToChunkIndex(ASampleID: DWord; ASampleIndex: DWord; out AFirstIndexInChunk: DWord): DWord;
  published
    property Count: DWord read FEntryCount;
  end;


  TSampleDecriptionAtom = class;
  { TstsdAtom }

  TstsdAtom = class(TmetaAtom)
  private
    FEntryCount: DWord;
    function SampleDescAtomClassLookup(Sender: TAtomList; AParent: TAtom; AStream: TStream): TAtomClass;
  public
    constructor Create(AStream: TStream); override;
    property Count: DWord read FEntryCount;

  end;

  TSampleDecriptionAtom = class(TAtom)
  private
  const
    cBaseSampleSizePos = 14;
    function GetDataReferenceIndex: Word;
  public
 // children of stsd atom
    constructor Create(AStream: TStream); override;
    property Format:  TAtomName read FAtomName;
    property DataReferenceIndex: Word read GetDataReferenceIndex;
  end;

  { TSoundSampleDecriptionAtom }

  TSoundSampleDecriptionAtom = class(TSampleDecriptionAtom)
  private
  const
    cV0Start = cBaseSampleSizePos+2;
    cV1Start = cV0Start+20;
    cV2Start = cV1Start+16;
    cV2Size  = 54;
  function CustomAtomClassLookup(Sender: TAtomList; AParent: TAtom;
    AStream: TStream): TAtomClass;
    function GetBytesPerFrame: DWord;
    function GetBytesPerPacket: DWord;
    function GetBytesPerSample: DWord;
    function GetChannels: Word;
    function GetCompressionID: Word;
    function GetSampleRate: Double;
    function GetSampleSize: Word;
    function GetSamplesPerPacket: DWord;
    function GetVersion: Word;
  public
    constructor Create(AStream: TStream); override;
    property Version: Word read GetVersion;
    // property RevisionLevel: Word; // is always 0
    // property Vendor: DWord;// is always 0
    property Channels: Word read GetChannels;
    property SampleSize: Word read GetSampleSize;
    property CompressionID: Word read GetCompressionID; // 0 for version = 0. other for other versions
    //property PacketSize: Word read GetPacketSize; // always 0
    property SampleRate: Double read GetSampleRate; // 16bit . 16.bit fixed point words

    // version 1+ properties
    property SamplesPerPacket: DWord read GetSamplesPerPacket;
    property BytesPerPacket: DWord read GetBytesPerPacket;
    property BytesPerFrame: DWord read GetBytesPerFrame;
    property BytesPerSample: DWord read GetBytesPerSample;
    // we don't support v3 yet.
  end;

  //TSoundSampleAAVDDecriptionAtom = class(TSoundSampleDecriptionAtom)

  //end;

  { TesdsAtom }

  TesdsAtom = class(TmetaAtom)
  private
    FAvgBitrate: DWord;
    FBuffersizeDB: DWord;
    FDecoderConfig: Word;
    FDecoderConfigLength: Byte;
    FESId: Word;
    FMaxBitrate: DWord;
    FObjectTypeID: Byte;
    FSLConfig: Byte;
    FStreamPriority: Byte;
    FStreamType: Byte;
    function GetChannelConfig: Byte;
    function GetObjectType: Byte;
    function GetSampleRateIndex: Byte;
  published
    property ESId: Word read FESId;
    property StreamPriority: Byte read FStreamPriority;

    property ObjectTypeID: Byte read FObjectTypeID;
    property StreamType: Byte read FStreamType;
    property BuffersizeDB: DWord read FBuffersizeDB;
    property MaxBitrate: DWord read FMaxBitrate;
    property AvgBitrate: DWord read FAvgBitrate;
    property DecoderConfigLength: Byte read FDecoderConfigLength;
    property DecoderConfig: Word read FDecoderConfig;
    property SLConfig: Byte read FSLConfig; // usually one byte! but apparently it can be more
    property SampleRateIndex: Byte read GetSampleRateIndex;
    property ObjectType: Byte read GetObjectType;
    property ChannelConfig: Byte read GetChannelConfig;
  public
    constructor Create(AStream: TStream); override;
  end;


  { TdataAtom }

  TdataAtom = class(TAtom)
    FDataType: DWord;
    FReserved: DWord;
    constructor Create(AStream: TStream); override;
  end;

  { TchapAtom }

  TchapAtom = class(TAtom)
  private
    function GetChapterTrak: DWord;
  published
     property ChapterTrak: DWord read GetChapterTrak;
  end;

  { TmdhdAtom }

  TmdhdAtom = class(TAtom)
    FVersion: Byte;
    FFlags: DWord;
    FCreationTime: QWord;
    FModificationTime: QWord;
    FTimeScale: DWord; // samplerate
    FDuration: QWord;
    FLanguage: Word;
    FQuality: Word;
    constructor Create(AStream: TStream); override;
  end;

  { Ttkhd }

  Ttkhd = class(TAtom)
    FVersion: Byte;
    FFlags: DWord; // 3 bytes
    FCreationTime: DWord;
    FModificationTime: DWord;
    FTrackID: DWord;
    FReserved: DWord;
    FDuration: DWord;
    FReserved2: DWord;
    FLayer: Word;
    FAlternativeGroup: Word;
    FVolume: Word;
    FReserved3: Word;
    FMatrixStructure: array[0..2] of array[0..2] of DWord;
    FTrackWidth: DWord;
    FTrackHeight: DWord;
    constructor Create(AStream: TStream); override;
  end;



function IsAtom(AtomName: TAtomName): Boolean;
function IsAtom(AtomName: TAtomName; out IsContainer: Boolean): Boolean;
function IsAtom(AtomName: TAtomName; out IsContainer: Boolean; out AtomClass: TAtomClass): Boolean;

procedure AddAtom(Atom: TAtomName; IsContainer: Boolean; AtomClass: TAtomClass = nil);
operator := (A: Cardinal): TAtomName;
operator := (A: TAtomName): Cardinal;
operator := (A: TAtomName): RawByteString;
operator := (A: RawByteString): TAtomName;
operator = (A,B: TAtomName): Boolean;
operator <> (A,B: TAtomName): Boolean;
operator < (A,B: TAtomName): Boolean;
operator > (A,B: TAtomName): Boolean;

implementation

type
  TAtomNameRecord = record
    N: TAtomName;  // Name
    B: Boolean;    // IsConainer
    C: TAtomClass;
  end;

{ TstscAtom }

constructor TstscAtom.Create(AStream: TStream);
var
  i: Integer;
begin
  inherited Create(AStream);
  FEntryCount := BEtoN(FStream.ReadDWord);
  SetLength(FEntries, FEntryCount);
  for i := 0 to FEntryCount-1 do
  begin
    FEntries[i].FirstChunk:=BEtoN(FStream.ReadDWord);
    FEntries[i].SamplesPerChunk:=BEtoN(FStream.ReadDWord);
    FEntries[i].SampleDescriptionID:=BEtoN(FStream.ReadDWord);
  end;
end;

function TstscAtom.SampleIndexToChunkIndex(ASampleID: DWord;
  ASampleIndex: DWord; out AFirstIndexInChunk: DWord): DWord;
var
  lLastChunk: DWord = 1;
  i: Integer;
  lCountedSamples: QWord = 0;
  j: DWord;
begin
  Result := 0;
  AFirstIndexInChunk := 0;
  while True do
  begin
    for i := 0 to FEntryCount -1 do
    begin
      // if chunks numbers are skipped then they are duplicate to the last chunk index
      if FEntries[i].FirstChunk > lLastChunk+1 then
      begin
        for j := lLastChunk+1 to FEntries[i].FirstChunk-1 do
        begin
          if FEntries[i-1].SamplesPerChunk + lCountedSamples >{=} ASampleIndex then
          begin
            AFirstIndexInChunk:= lCountedSamples;
            Exit;
          end;
          Inc(lCountedSamples, FEntries[i-1].SamplesPerChunk);
          Inc(Result);
        end;
      end;


      if FEntries[i].SamplesPerChunk + lCountedSamples > ASampleIndex then
      begin
        AFirstIndexInChunk:= lCountedSamples;
        Exit;
      end;
      Inc(Result);
      Inc(lCountedSamples, FEntries[i].SamplesPerChunk);
      lLastChunk:=FEntries[i].FirstChunk;
    end;
    // exited loop without result so keep counting last entry as it must repeat
    repeat
      if lCountedSamples + FEntries[i].SamplesPerChunk >= ASampleIndex then
      begin
        AFirstIndexInChunk:= lCountedSamples;
        Exit;
      end;

      Inc(Result, FEntries[i].SamplesPerChunk);
      Inc(lCountedSamples, FEntries[i].SamplesPerChunk);

    until False;
  end;
end;

{ TesdsAtom }

function TesdsAtom.GetSampleRateIndex: Byte;
begin
  Result := FDecoderConfig shr 7 and %1111;
end;

function TesdsAtom.GetObjectType: Byte;
begin
  Result := FDecoderConfig shr 11 and %11111;
end;

function TesdsAtom.GetChannelConfig: Byte;
begin
  Result := FDecoderConfig shr 3 and %1111;
end;

constructor TesdsAtom.Create(AStream: TStream);
var
  lLen, lTag: Byte;
begin
  inherited Create(AStream);
  StreamSeek(soBeginning, 12);
  lTag:= FStream.ReadByte;
  if lTag = $03 {MP4ESDescrTag} then
  begin
    repeat
      lLen := FStream.ReadByte;
    until lLen <> $80;
    FESId:= BEton(FStream.ReadWord);
    FStreamPriority:= FStream.ReadByte;

    lTag := FStream.ReadByte;
    if ltag = $04 {MP4DecConfigDescrTag} then
    begin
      repeat
        lLen := FStream.ReadByte;
      until lLen <> $80;
      FObjectTypeID:=FStream.ReadByte;
      FStreamType:=FStream.ReadByte;
      FStream.Read(FBuffersizeDB, 3); // todo fix this
      FMaxBitrate:=BEtoN(FStream.ReadDWord);
      FAvgBitrate:=BEtoN(FStream.ReadDWord);
      lTag := FStream.ReadByte;
      if ltag = $05 {MP4DecSpecificDescrTag} then
      begin
        repeat
          lLen := FStream.ReadByte;
        until lLen <> $80;
        if lLen <> 2 then raise Exception.Create('MP4DecSpecificDescrTag length > 1 we can''t handle that yet!');
        FDecoderConfigLength:=lLen;
        FDecoderConfig:={BEtoN}(FStream.ReadWord);
        //WriteLn(BinStr(FDecoderConfig, 16));

        lTag := FStream.ReadByte;
        if ltag = $06 {MP4SLConfigDescrTag} then
        begin
          repeat
            lLen := FStream.ReadByte;
          until lLen <> $80;
          if lLen <> 1 then raise Exception.Create('MP4SLConfigDescrTag length > 1 we can''t handle that yet!');
          FSLConfig:=FStream.ReadByte;
        end;
      end;
    end;
  end;
end;

{ TstszAtom }

function TstszAtom.GetSampleCount: LongWord;
begin
  StreamSeek(soBeginning, cNumEntriesPos);
  Result := BEtoN(FStream.ReadDWord);
end;

function TstszAtom.GetFlags: LongWord;
begin
  StreamSeek(soBeginning, cVersionPos);
  Result := LEtoN(FStream.ReadDWord) and $00FFFFFF;
end;

function TstszAtom.GetSampleSizeEntry(AIndex: LongWord): LongWord;
begin
  StreamSeek(soBeginning, cSampleSizeTablePos+(AIndex*4));
  Result := BEtoN(FStream.ReadDWord);
end;

function TstszAtom.GetSampleSize: LongWord;
begin
  StreamSeek(soBeginning, cSampleSizePos);
  Result := BEtoN(FStream.ReadDWord);
end;

function TstszAtom.GetVersion: Byte;
begin
  StreamSeek(soBeginning, cVersionPos);
  Result := FStream.ReadByte;

end;

procedure TstszAtom.CopySampleTable(ABuffer: PDword);
var
  lCount : Int64;
  i: Integer;
begin
  lCount := SampleCount;
  StreamSeek(soBeginning, cSampleSizeTablePos);
  for i := 0 to lCount-1 do
    ABuffer[i] := BEtoN(FStream.ReadDWord);
end;

{ TSoundSampleDecriptionAtom }

function TSoundSampleDecriptionAtom.GetBytesPerFrame: DWord;
begin
  if Version < 1 then
    Exit(0);
  StreamSeek(soBeginning, cV1Start+8);
  Result := LEtoN(FStream.ReadDWord);
end;

function TSoundSampleDecriptionAtom.CustomAtomClassLookup(Sender: TAtomList; AParent: TAtom; AStream: TStream): TAtomClass;
var
  lSavedPos: Int64;
  lFormat: TAtomName;
  lIsContainer: Boolean;
  lAtomClass: TAtomClass;
begin
  Result := TAtom;
  lSavedPos:=FStream.Position;
  FStream.Seek(4, soCurrent);
  FStream.Read(lFormat, 4);
  FStream.Seek(-8, soCurrent);

  {$TODO some sample desc format search with registrations...?}
  if lFormat.Chars = 'esds' then
    Result := TesdsAtom
  else
  begin
    if IsAtom(lFormat, lIsContainer, lAtomClass) then
      Result := lAtomClass;
  end;



end;

function TSoundSampleDecriptionAtom.GetBytesPerPacket: DWord;
begin
  if Version < 1 then
    Exit(0);
  StreamSeek(soBeginning, cV1Start+4);
  Result := LEtoN(FStream.ReadDWord);
end;

function TSoundSampleDecriptionAtom.GetBytesPerSample: DWord;
begin
  if Version < 1 then
    Exit(0);
  StreamSeek(soBeginning, cV1Start+12);
  Result := LEtoN(FStream.ReadDWord);
end;

function TSoundSampleDecriptionAtom.GetChannels: Word;
begin
  StreamSeek(soBeginning, cV0Start+8);
  Result := BEtoN(FStream.ReadWord);
end;

function TSoundSampleDecriptionAtom.GetCompressionID: Word;
begin
  StreamSeek(soBeginning, cV0Start+12);
  Result := LEtoN(FStream.ReadWord);
end;

function TSoundSampleDecriptionAtom.GetSampleRate: Double;
var
  Int: Word;
  Decimal: Word;
begin
  StreamSeek(soBeginning, cV0Start+16);

  Int := BEtoN(FStream.ReadWord);
  Decimal := BEtoN(FStream.ReadWord);

  Result := Int + ( Decimal / $FFFF);
end;

function TSoundSampleDecriptionAtom.GetSampleSize: Word;
begin
  StreamSeek(soBeginning, cV0Start+10);
  Result := LEtoN(FStream.ReadWord);
end;

function TSoundSampleDecriptionAtom.GetSamplesPerPacket: DWord;
begin
  if Version < 1 then
    Exit(0);
  StreamSeek(soBeginning, cV1Start);
  Result := LEtoN(FStream.ReadDWord);
end;

function TSoundSampleDecriptionAtom.GetVersion: Word;
begin
  StreamSeek(soBeginning, cV0Start);
  Result := BEtoN(FStream.ReadWord);
end;

constructor TSoundSampleDecriptionAtom.Create(AStream: TStream);
begin
  inherited Create(AStream);
  AtomClassLookup:=@CustomAtomClassLookup;
  case Version of
    0: FDataOffset:=cV1Start;
    1: FDataOffset:=cV2Start;
    2: FDataOffset:=cV2Size;
  end;
  {WriteLn('RefIndex ', DataReferenceIndex);
  WriteLn('Version ', Version);
  WriteLn('Channels ',Channels);
  WriteLn('SampleRate ', SampleRate);
  WriteLn('SampleSize ', SampleSize);
  WriteLn('CompressionID ', CompressionID);}
end;

{ TSampleDecriptionAtom }

function TSampleDecriptionAtom.GetDataReferenceIndex: Word;
begin
  StreamSeek(soBeginning, cBaseSampleSizePos);
  Result := BEtoN(FStream.ReadWord);
end;

constructor TSampleDecriptionAtom.Create(AStream: TStream);
begin
  inherited Create(AStream); // reads atom size and atom name.
  FDataOffset:=AtomSize-8; // override in children
end;

{ TftypAtom }

function TftypAtom.GetAltBrand(AIndex: Integer): String;
begin
  StreamSeek(soCurrent);
  FStream.Seek(8+(AIndex*4), soCurrent);
  SetLength(Result, 4);
  FStream.Read(Result[1], 4);
  Result := Trim(Result);
end;

function TftypAtom.GetAltBrandCount: Integer;
begin
  Result := (AtomSize-16) div 4;
end;

function TftypAtom.GetMajorBrand: String;
begin
  StreamSeek(soCurrent);
  SetLength(Result, 4);
  FStream.Read(Result[1], 4);
  Result := Trim(Result);
end;

function TftypAtom.GetVersion: LongWord;
begin
  StreamSeek(soCurrent);
  FStream.Seek(4, soCurrent);
  Result := BEtoN(FStream.ReadDWord);
end;

{ TstcoAtom }

function TstcoAtom.GetOffset(AIndex: Integer): DWord;
begin
  StreamSeek(soCurrent);
  FStream.Seek(AIndex*SizeOf(DWord), soFromCurrent);
  Result := BEton(FStream.ReadDWord);
end;

function TstcoAtom.GetStringFromOffset(AIndex: Integer): String;
var
  lOffset: DWord;
  sLength: Word;
begin
  lOffset := Offset[AIndex];
  FStream.Position:=lOffset;
  sLength:=BEtoN(FStream.ReadWord);
  SetLength(Result, sLength);
  FStream.Read(Result[1], sLength);
end;

constructor TstcoAtom.Create(AStream: TStream);
begin
  inherited Create(AStream);
  StreamSeek(soCurrent);
  FVersion:=FStream.ReadByte;
  FStream.Read(FFlags, 3);
  FFlags := BEtoN(FFlags);
  FEntries:=BEtoN(FStream.ReadDWord);
  FDataOffset:=FStream.Position - FAtomPosition;
end;

procedure TstcoAtom.CopyTable(ABuffer: PDWord);
var
  i: Integer;
begin
  StreamSeek(soCurrent);
  for i := 0 to FEntries-1 do
    ABuffer[i] := BEtoN(FStream.ReadDWord);
end;

{ Ttkhd }

constructor Ttkhd.Create(AStream: TStream);
var
  i, j: Integer;
begin
  inherited Create(AStream);
  StreamSeek(soCurrent);
  FVersion:=FStream.ReadByte;
  FStream.Read(FFlags, 3);
  FFlags := BEtoN(FFlags);
  FCreationTime:= BEtoN(FStream.ReadDWord);
  FModificationTime:=BEtoN(FStream.ReadDWord);
  FTrackID:=BEtoN(FStream.ReadDWord);
  FReserved:=BEtoN(FStream.ReadDWord);
  FDuration:=BEtoN(FStream.ReadDWord);
  FReserved2:=BEtoN(FStream.ReadDWord);
  FLayer:=BEtoN(FStream.ReadWord);
  FAlternativeGroup:=BEtoN(FStream.ReadWord);
  FVolume:=BEtoN(FStream.ReadWord);
  FReserved:=BEtoN(FStream.ReadWord);
  for i := 0 to 2 do for j := 0 to 2 do
    FMatrixStructure[i][j] := BEtoN(FStream.ReadDWord);
  FStream.Read(FMatrixStructure, 36);
  //FMatrixStructure: array[0..35] of byte;
  FTrackWidth:=BEtoN(FStream.ReadDWord);
  FTrackHeight:=BEtoN(FStream.ReadDWord);

  FDataOffset:=FStream.Position - FAtomPosition;

end;

{ TmdhdAtom }

constructor TmdhdAtom.Create(AStream: TStream);
begin
  inherited Create(AStream);
  StreamSeek(soCurrent);
  FVersion:=FStream.ReadByte;
  FStream.Read(FFlags, 3);
  FFlags := BEtoN(FFlags);
  if FVersion = 0 then
  begin
    FCreationTime:=BEtoN(FStream.ReadDWord);
    FModificationTime:=BEtoN(FStream.ReadDWord);;
  end
  else
  begin
    FCreationTime := BEtoN(FStream.ReadQWord);
    FModificationTime:=BEtoN(FStream.ReadQWord);;
  end;
  FTimeScale:=BEtoN(FStream.ReadDWord);
  if FVersion = 0 then
    FDuration:=BEtoN(FStream.ReadDWord)
  else
    FDuration:=BEtoN(FStream.ReadQWord);
  FLanguage := BEtoN(FStream.ReadWord);
  FQuality := BEtoN(FStream.ReadWord);
  FDataOffset:=FStream.Position - FAtomPosition;
end;

{ TstsdAtom }

function TstsdAtom.SampleDescAtomClassLookup(Sender: TAtomList; AParent: TAtom; AStream: TStream): TAtomClass;
var
  lSavedPos: Int64;
  lFormat: TAtomName;
begin
  Result := TSampleDecriptionAtom;
  lSavedPos:=FStream.Position;
  FStream.Seek(4, soCurrent);
  FStream.Read(lFormat, 4);
  FStream.Seek(-8, soCurrent);

  {$TODO some sample desc format search with registrations...?}
  if lFormat.Chars = 'aavd' then
    Result := TSoundSampleDecriptionAtom
  else if lFormat.Chars = 'mp4a' then
    Result := TSoundSampleDecriptionAtom;

  // okay more work needed here. the stsd atom can have many different types of desc atoms.


end;

constructor TstsdAtom.Create(AStream: TStream);
begin
  AtomClassLookup:=@SampleDescAtomClassLookup;
  inherited Create(AStream);

  FStream.Read(FEntryCount, 4);
  FEntryCount := BEtoN(FEntryCount);
  Inc(FDataOffset, 4);
  //Writeln('Created TstsdAtom');
end;

{ TchapAtom }

function TchapAtom.GetChapterTrak: DWord;
begin
  Result := DataAsDWord;
end;

{ TdataAtom }

constructor TdataAtom.Create(AStream: TStream);
begin
  inherited Create(AStream);
  StreamSeek(soCurrent);
  //FStream.Seek(-4, soFromCurrent);
  FDataType:=BEtoN(FStream.ReadDWord);
  FReserved:=BEtoN(FStream.ReadDWord);
  Inc(FDataOffset, 8);
end;

{ TmetaAtom }

constructor TmetaAtom.Create(AStream: TStream);
begin
  inherited Create(AStream);
  StreamSeek(soCurrent);
  //FStream.Seek(-4, soFromCurrent);
  FVersion:=FStream.ReadByte;
  FStream.Read(FFlags, 3);
  FFlags := BEtoN(FFlags);
  Inc(FDataOffset, 4);

end;

{ TsttsAtom }

function TsttsAtom.GetCount: Integer;
var
  sttsVersion: Byte;
begin
  Result := 0;
  StreamSeek(soCurrent);
  sttsVersion:=FStream.ReadByte;
  if sttsVersion <> 0 then
    Exit;

  FStream.Seek(3, soFromCurrent); // seek past 3 byte flags
  Result := BEtoN(FStream.ReadDWord);
end;

function TsttsAtom.GetItem(AIndex: Integer): Integer;
var
  sttsVersion: Byte;
  c: Integer;
  lIndex: Integer = -1;
begin
  Result := 0;
  StreamSeek(soCurrent);
  Fstream.Seek(8, soFromCurrent);
  if (AIndex>=Count) or (AIndex<0)then
    Exit;
  while lIndex < AIndex do
  begin
    Inc(lIndex, BEtoN(FStream.ReadDWord));
    Result:=BEtoN(FStream.ReadDWord);
  end;
end;

  { TAtomList }

function TAtomList.GetAtom(AIndex: Integer): TAtom;
begin
  Result := TAtom(Items[AIndex]);
end;

procedure TAtomList.SetAtom(AIndex: Integer; AValue: TAtom);
begin
  Items[AIndex] := AValue;
end;

constructor TAtomList.Create(AParentAtom: TAtom);
begin
  Inherited Create;
  FParent := AParentAtom;
end;

destructor TAtomList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    TObject(Items[I]).Free;
  inherited Destroy;
end;

procedure TAtomList.LoadAtoms(AStream: TStream; AFoundEvent: TAtomFoundEvent; AMaxPos: QWord; AClassLookup: TAtomClassLookup);
var
  lAtom: TAtom;
begin
  while AStream.Position < AMaxPos -8 do //AStream.Size do
  begin
    if Assigned(AClassLookup) then
      lAtom := AClassLookup(Self, Parent, AStream).Create(AStream)
    else
      lAtom := TAtomClass.CreateAtomClass(AStream);
    if Assigned(AFoundEvent) then
    begin
      if not AFoundEvent(Self, lAtom) then
      begin
        lAtom.StreamSeek(soBeginning);
        lAtom.Free;
        Break;
      end;
    end;
    // adds atom to the atom list
    AddAtom(lAtom.AtomName, False);
    lAtom.StreamSeek(soEnd);
    Add(lAtom);
  end;
end;

function TAtomList.FindAtom(AAtomName: TAtomName; var AtomResult: TAtom;
  AAtomOccurence: Integer): Boolean;
var
  FoundCount: Integer = 0;
  I : Integer;
begin
  //WriteLn('Looking for atom (',Copy(AAtomName.Chars,0,4),')');
  Result := False;
  AtomResult := nil;
  if Count = 0 then
  begin
    //WriteLn('Node atoms: ',Count);
    Exit;
  end;

  if AAtomOccurence < 1 then
      AAtomOccurence:=0;

  for I := 0 to Count-1 do
  begin
    //WriteLn('Comparing to ('+Copy(Atom[I].AtomName.Chars,0,4)+')');
    if Atom[I].AtomName = AAtomName then
      begin
        Inc(FoundCount);
        if FoundCount = AAtomOccurence then
          begin
            //WriteLn('Found Atom ',Copy(Atom[I].AtomName.Chars,0,4));
            Result:= True;
            AtomResult := Atom[I];
            Exit;
          end;
      end
      else
        ;//WriteLn('Looking for: ', AAtomName.Chars, ' found: ', AfterAtom.AtomName.Chars);

  end;
end;

function TAtomList.FindAtom(AAtomPath: RawByteString): TAtom;
var
  AtomPath: TStringList;
  I,J: Integer;
  AtomList: TAtomList;
  TmpTmpAtom: TAtom = nil;
  TmpAtom: TAtom = nil;
  TmpAtomName: TAtomName;
  TmpAtomOcc: Integer;
begin
  Result := nil;
  AtomPath:= TStringList.Create;
  AtomPath.StrictDelimiter:=True;
  AtomPath.Delimiter:='/';
  AtomPath.DelimitedText:=AAtomPath;
  AtomList := Self;
  TmpTmpAtom:=nil;
  for I := 0 to AtomPath.Count-1 do
    begin
      TmpAtomName.Chars := Copy(AtomPath[I], 1,4);
      if Length(AtomPath[I]) > 4 then
        TmpAtomOcc:=StrToint(Copy(AtomPath[I],6,Length(AtomPath[I])-5))
      else
        TmpAtomOcc:=1;
      TmpTmpAtom:= TmpAtom;
      if AtomList.FindAtom(TmpAtomName, TmpAtom, TmpAtomOcc) then
        AtomList := TmpAtom.Atoms
      else
        begin
          if Assigned(TmpTmpAtom) then
            WriteLn('Failed: ', AAtomPath, ' got to : ', Copy(TmpTmpAtom.AtomName.Chars,0,4))
          else
            WriteLn('Failed: ', AAtomPath, ' got nowhere');
          Exit;
        end;
    end;
  Result := TmpAtom;

end;

{ TAtom }

procedure TAtom.LoadHeader;
var
  lAtomType: Cardinal;
  IsContainer: Boolean;
begin
  FAtomPosition := FStream.Position;
  try
    FStream.Position:=FAtomPosition;
    lAtomType:=BEtoN(FStream.ReadDWord);
    if lAtomType = 1 then // 64bit atom
    begin
      FStream.Read(FAtomName, 4);
      FAtomSize := BEtoN(Fstream.ReadQWord);
      FDataOffset:=16;
    end
    else
    begin
      FAtomSize:=lAtomType;
      FStream.Read(FAtomName, 4);
      FDataOffset:=8;
    end;
    if FAtomName.Chars[0] = #0 then
      Raise Exception.Create(ClassName+': invalid atom name');
    //WriteLn(Copy(FAtomName.Chars,0,4));
  finally
    FStream.Position:=FAtomPosition;
  end;
end;

procedure TAtom.LoadAtoms;
begin
  StreamSeek(soCurrent);
  FAtoms.LoadAtoms(FStream, nil, FAtomPosition + FAtomSize, AtomClassLookup);
end;

function TAtom.DataSize: QWord;
begin
  Result := FAtomSize-FDataOffset;
end;

class function TAtom.PeekAtom(AStream: TStream; out AAtomName: TAtomName): Boolean;
var
  SPos: Int64;
  lAtomType: DWord;
begin
  Result := False;
  SPos := AStream.Position;
  try
    lAtomType:=BEtoN(AStream.ReadDWord);
    if lAtomType = 1 then
    begin
      AStream.Read(AAtomName, 4);
      //FAtomSize := BEtoN(Fstream.ReadQWord);
    end
    else
    begin
      //FAtomSize:=lAtomType;
      AStream.Read(AAtomName, 4);
    end;
    Result := True;
  finally
    AStream.Position:=SPos;
  end;
end;

function TAtom.GetAtoms: TAtomList;
var
  SubAtomName: TAtomName;
  IsContainer: Boolean = False;
  LoadSubitems: Boolean;
begin
  if FAtoms = nil then
  begin
    StreamSeek(soCurrent);
    LoadSubitems := TAtom.PeekAtom(FStream, SubAtomName) and IsAtom(SubAtomName);

    FAtoms := TAtomList.Create(Self);
    if LoadSubitems or (IsAtom(FAtomName, IsContainer) and IsContainer) then
    begin
      //WriteLn('Gettin child atoms for: ', copy(FAtomName.Chars,0,4));
        StreamSeek(soCurrent);
        LoadAtoms;
    end;
  end;
  Result := FAtoms;
end;

procedure TAtom.StreamSeek(ASeekPosition: TSeekOrigin; AOffset: PtrInt);
begin
  case ASeekPosition of
    soEnd: FStream.Position:=FAtomPosition + FAtomSize+AOffset;
    soBeginning:FStream.Position:=FAtomPosition+AOffset;
    soCurrent: FStream.Position:=FAtomPosition+FDataOffset+AOffset;
  end;
end;

procedure TAtom.SaveToStream(AStream: TStream);
begin
  StreamSeek(soCurrent);
  AStream.CopyFrom(FStream, DataSize);
end;

constructor TAtom.Create(AStream: TStream);
begin
  FStream := AStream;
  LoadHeader;
end;

destructor TAtom.Destroy;
begin
  if Assigned(FAtoms) then
    FAtoms.Free;
  inherited Destroy;
end;

class function TAtom.CreateAtomClass(AStream: TStream): TAtom;
var
  lAtomName: TAtomname;
  NewClass: TAtomClass;
  lIsContainer: Boolean;
begin
  if TAtom.PeekAtom(AStream, lAtomName)
  and IsAtom(lAtomName, lIsContainer, NewClass) then
  begin
    // NewClass is assigned already
  end
  else
  begin
    NewClass := TAtom;
    //WriteLn('unknown atom: ', lAtomName.Chars);
  end;

  Result := NewClass.Create(AStream);

end;

function TAtom.DataAsString: String;
begin
  StreamSeek(soCurrent);
  SetLength(Result, DataSize);
  FStream.Read(Result[1], DataSize);
end;

function TAtom.DataAsDWord: DWord;
begin
  StreamSeek(soCurrent);
  FStream.Read(Result, 4);
  Result := BEToN(Result);
end;

var
  lAtoms: array of TAtomNameRecord;

function FindInsertionIndex(AtomName: TAtomName): Integer;
var
  Idx: Integer;
  Size: Integer;
  lMax: Integer;
  lMin: Integer;
begin
  Result := 0;
  if Length(lAtoms) = 0 then
    Exit;

  //Idx:= Length(lAtoms) div 2;
  //Size := High(lAtoms);
  lMax := High(lAtoms);
  lMin := 0;

  while lMin < lMax do
  begin
    Idx := (lMax+lMin) div 2;
    //WriteLn(Format('Idx = %d lMin = %d lMax = %d', [idx,lmin,lmax]));
    if not(Idx < lMax) then
      Exit(Length(lAtoms)); // the insertion point is after all items.

    if lAtoms[Idx].N < AtomName then
      lMin:= Idx+1
    else
      lMax := Idx;
  end;
    Idx := lMin;

  if lAtoms[lMin].N < AtomName then
    Inc(Idx);

  if Idx > Length(lAtoms) then Idx := Length(lAtoms);
  if Idx < 0 then Idx := 0;
  Result := Idx;
end;

function IsAtom(AtomName: TAtomName): Boolean;
var
  C: Boolean;
begin
  Result := IsAtom(AtomName, C);
end;

function IsAtom(AtomName: TAtomName; out IsContainer: Boolean): Boolean;
var
  C: TAtomClass;
begin
  Result := IsAtom(AtomName, IsContainer, C);

end;

function IsAtom(AtomName: TAtomName; out IsContainer: Boolean; out
  AtomClass: TAtomClass): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  if Length(lAtoms) = 0 then
    Exit;
  //WriteLn(Format('IsAtom ; Length = %d',[Length(latoms)]));
  Idx := FindInsertionIndex(AtomName);
  if lAtoms[Idx].N > AtomName then
    Dec(Idx)
  else if lAtoms[Idx].N < AtomName then
    Inc(Idx);

  if Idx > High(lAtoms) then
    Idx := High(lAtoms)
  else if Idx < 0 then
    Idx := 0;



  if lAtoms[Idx].N = AtomName then
    Result := True;

  if Result then
  begin
    IsContainer:=lAtoms[Idx].B;
    AtomClass:=lAtoms[Idx].C;
  end;
  if not Result then
    IsContainer:=False;
  //if Result then
  //  WriteLn('We Think ',Idx,'(Length = ',Length(latoms),') is right for ',AtomName.Chars,' = ', Result, ' cls ', AtomClass.ClassName);
end;

procedure AddAtom(Atom: TAtomName; IsContainer: Boolean; AtomClass: TAtomClass = nil);
var
  Idx: Integer;
  NewAtom: TAtomName;
  MoveItems: Integer;
begin
  NewAtom := Atom;
  if not IsAtom(NewAtom) then
  begin
    Idx := FindInsertionIndex(Atom);
    //WriteLn('Inserting @ ', Idx);
    SetLength(lAtoms, Length(lAtoms)+1);

    MoveItems := High(lAtoms)-Idx;
    Move(lAtoms[Idx], lAtoms[Idx+1], MoveItems*SizeOf(TAtomNameRecord));
    lAtoms[Idx].N := NewAtom;
    lAtoms[Idx].B := IsContainer;
    if AtomClass = nil then
      AtomClass := TAtom;
    lAtoms[Idx].C := AtomClass;
  end;
 { for idx := 0 to High(lAtoms) do
     WriteLn('al: "',TAtomName(lAtoms[Idx].N).Int,'"');
  WriteLn('----');}
end;

operator:=(A: Cardinal): TAtomName;
begin
  Result.Int:=A;
end;

operator:=(A: TAtomName): Cardinal;
begin
  Result:=A.Int;
end;

operator:=(A: TAtomName): RawByteString;
begin
  Result:=Copy(A.Chars,0, 4);
end;


operator:=(A: RawByteString): TAtomName;
begin
  if Length(A) <> 4 then
  begin
    raise Exception.CreateFmt('Invalid Atom name ''%s'', Length must be 4 is ''%d''!',[A, Length(A)]);
  end;
  Result.Chars := Copy(A, 1, 4);
end;

operator=(A, B: TAtomName): Boolean;
begin
  Result := A.Int = B.Int;
end;

operator<>(A, B: TAtomName): Boolean;
begin
  Result := A.Int <> B.Int;
end;

operator<(A, B: TAtomName): Boolean;
begin
  Result := A.Int < B.Int;
end;

operator>(A, B: TAtomName): Boolean;
begin
  Result := A.Int>B.Int;
end;

initialization
  AddAtom('moov', True);
  AddAtom('trak', True);
  AddAtom('mvhd', False); // was true....
  AddAtom('tkhd', False, Ttkhd);
  AddAtom('tref', True);
  AddAtom('edts', True);
  AddAtom('elst', False);
  AddAtom('mdia', True);
  AddAtom('minf', True);
  AddAtom('dinf', True);
  AddAtom('stbl', True);
  AddAtom('hdlr', False);
  AddAtom('gmhd', True);
  AddAtom('gmin', False);
  AddAtom('text', False);
  AddAtom('udta', True);
  AddAtom('stts', False, TsttsAtom);
    AddAtom('stsd', True, TstsdAtom);
    AddAtom('stco', False, TstcoAtom);
    AddAtom('stsz', False, TstszAtom);
    AddAtom('stsc', False, TstscAtom);
    AddAtom('esds', False);
  AddAtom('meta', True, TmetaAtom);
    AddAtom('data', False, TdataAtom);
  AddAtom('ilst', True);
  AddAtom(#$A9'nam', True);
  AddAtom(#$A9'cmt', True);
  AddAtom(#$A9'alb', True);
  AddAtom(#$A9'ART', True);
  AddAtom('aArt', True);
  AddAtom('chap', False, TchapAtom);
  AddAtom('covr', True);
  AddAtom('mdhd', False, TmdhdAtom);
  AddAtom('ftyp', False, TftypAtom);
  AddAtom('mp4a', True, TSoundSampleDecriptionAtom);
  AddAtom('esds', False, TesdsAtom);



end.

