{
    This unit is part of Pascal Audio IO package.

    Copyright (c) 2020 by Andrew Haines.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

{
   Currently this only handles Channel Mapping=0. Can be 1 or 2 channels.
   Channel Mapping = 1 can have 1..8 channels.
   Channel Mapping = 255 is used by custom implementations.
   https://tools.ietf.org/html/rfc7845#section-5.1.1
}
unit paio_ogg_opus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, paio_opus, paio_ogg_container, paio_vorbis_comment, paio_types;

type

  { TOggOpusDecoder }

  TOggOpusDecoder = class
  private
    FChannels: Integer;
    FComments: TVorbisComments;
    FMappingFamily: Integer;
    FOutputGain: Integer;
    FSamplePosition: QWord;
    FPreskip: Integer;
    FSamplerate: Integer;
    FStream: TStream;
    FDecoder: TOpusDecoder;
    FPage: TOggPageHeader;
    FPageDataStart: Int64; // the compressed data position after the index.
    FPageIndex: Integer;
    FPageSegmentIndex: Integer;
    FBuffer: array[0..AUDIO_BUFFER_SIZE-1] of Byte;
    FFirstDataPagePos: QWord;
    FTotalSamples: QWord;
    function FindPageStart: Boolean;
    function GetTotalSamples: QWord;
    function ReadHeader: Boolean;
    function ReadComments: Boolean;
    function LoadFirstDataPage: Boolean;
    function ReadPageHeader: Integer;
    function IsEndOfPage: Boolean;
    function NextPacketSize(out ANeedNewPage: Boolean): Integer;
    function FindBestPageForSample(ASampleNumber: QWord; out ASegmentIndex: Integer): Boolean;
    procedure SetSamplePosition(AValue: QWord);
    function PreviousPageSampleEnd: Int64;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    procedure InitDecoder;
    function DecodePacket(ADestData: Pointer; ADestSize: Integer; AUseFloat: Boolean=False): Integer;
    property Channels: Integer read FChannels;
    property OriginalSamplerate: Integer read FSamplerate; // data is always 48000 but the original source is this number
    property Preskip: Integer read FPreskip; // number of samples to ignore from the lead-in of the data.
    property OutputGain: Integer read FOutputGain;
    property MappingFamily: Integer read FMappingFamily; // currently only 0 is implemented
    property Comments: TVorbisComments read FComments;
    property SamplePosition: QWord read FSamplePosition write SetSamplePosition;
    property TotalSamples: QWord read GetTotalSamples;
  end;

  { TOpusHead }

  TOpusHead = object
    Magic: array[0..7] of Char;
    Version: Byte;
    Channels: Byte;
    PreSkip: Word;
    SampleRate: DWord;
    OutputGain: Word;
    MappingFamily: Byte;
    //mapping table if Family is not 0
    procedure ReadFromStream(AStream: TStream);
  end;

  TChannelMappingFamily1 = (
    cmMono = 1,
    cmStereo = 2,
    cmLinearSurround = 3,
    cmQuadraphonic = 4,
    cm50Surround = 5,
    cm51Surround = 6,
    cm61Surround = 7,
    cm71Surround = 8);

  // family 255 is used for custom implementations

implementation

{ TOpusHead }

procedure TOpusHead.ReadFromStream(AStream: TStream);
begin
  AStream.Read(Magic[0], 8);
  Version:=AStream.ReadByte;
  Channels := AStream.ReadByte;
  PreSkip:=LEtoN(AStream.ReadWord);
  SampleRate:=LEtoN(AStream.ReadDWord);
  OutputGain:=LEtoN(AStream.ReadWord);
  MappingFamily := AStream.ReadByte;
end;

{ TOggOpusDecoder }

function TOggOpusDecoder.ReadHeader: Boolean;
var
  //lPage: TOggPageHeader;
  lOpusHead: TOpusHead;
begin
  Result := False;
  ReadPageHeader;
  //lPage.ReadFromStream(FStream);
  if FPage.Signature <> 'OggS' then
    Exit;

  // check checksum :P

  Result := FPage.isFirstPageOfLogicalBitstream
        and (FPage.Position = 0)
        and (FPage.PageIndex = 0)
        and (FPage.SegmentsCount = 1);

  if not Result then
    Exit;
  // now read opus
  lOpusHead.ReadFromStream(FStream);
  Result := lOpusHead.Magic = 'OpusHead';
  if Result then
  begin
    FChannels:=lOpusHead.Channels;
    FPreskip:=lOpusHead.PreSkip;
    FSamplerate:=lOpusHead.SampleRate;
    FOutputGain:=lOpusHead.OutputGain;
    FMappingFamily:=lOpusHead.MappingFamily;
  end;

end;

function TOggOpusDecoder.GetTotalSamples: QWord;
var
  lPosition: Int64;
  lSegmentIndex: Integer;
  lPage: TOggPageHeader;
begin
  if FTotalSamples = 0 then
  begin
    // save positions
    lPosition := FStream.Position;
    lSegmentIndex := FPageSegmentIndex;
    FStream.Seek(-1, soFromEnd); // seek to just before the end of the stream
    if FindPageStart then
    begin
      lPage.ReadFromStream(FStream, True);
      FTotalSamples := lPage.Position;
    end
    else
    begin
      // TODO couldn't find page
    end;
    // restore positions
    FStream.Position:=lPosition;
    FPageSegmentIndex:=lSegmentIndex;
  end;
  Result := FTotalSamples - Preskip;
end;

function TOggOpusDecoder.ReadComments: Boolean;
var
  //lPage: TOggPageHeader;
  lMagic: Array[0..7] of Char;
  lDummy: Boolean;
  lPacketSize: Integer;
  lEndPos, lPos, lSkip: Int64;
begin

  ReadPageHeader;
  //lPage.ReadFromStream(FStream);
  Result := (FPage.Flags = 0)
        and (FPage.PageIndex = 1)
        //and (lPage.SegmentsCount = 1)
        ;
  if not Result then
    Exit;

  lPacketSize := NextPacketSize(lDummy);
  lEndPos := FStream.Position+lPacketSize;

  FStream.Read(lMagic, 8);
  Result := lMagic = 'OpusTags';
  if not Result then
    Exit;

  lSkip := lPacketSize - (FStream.Position-lPos);
  FComments.LoadFromStream(FStream);
  //FStream.Seek(lSkip, fsFromCurrent); // skip unused space in comments
  FStream.Position:=lEndPos;

  FFirstDataPagePos:=FStream.Position;
end;

function TOggOpusDecoder.LoadFirstDataPage: Boolean;
begin
  ReadPageHeader;
  //FChannels:=FPage.;
end;

function TOggOpusDecoder.ReadPageHeader: Integer;
var
  i: Integer;
begin
  FPage.ReadFromStream(FStream, True);
  FPageDataStart:=FStream.Position;
  FPageIndex:=FPage.PageIndex;
  FPageSegmentIndex:=0;
  Result := 0;
  for i := 0 to FPage.SegmentsCount-1 do
    Result += FPage.SegmentSize[i];

end;

function TOggOpusDecoder.IsEndOfPage: Boolean;
begin
  Result := FPageSegmentIndex = FPage.SegmentsCount;
end;

function TOggOpusDecoder.NextPacketSize(out ANeedNewPage: Boolean): Integer;
var
  lSegment: Byte;
  i: Integer;
begin
  Result := 0;
  for i := FPageSegmentIndex to FPage.SegmentsCount-1 do
  begin
    lSegment := FPage.SegmentSize[i];
    Result += lSegment;
    Inc(FPageSegmentIndex);
    if lSegment < 255 then
      Break;
  end;
  ANeedNewPage := lSegment = 255;
end;

function TOggOpusDecoder.FindPageStart: Boolean;
var
  lBuf: array[0..2] of Char;

begin
  Result := False;
  repeat
    case Char(FStream.ReadByte) of
      'S': FStream.Seek(-4, soFromCurrent);
      'g': FStream.Seek(-2, soFromCurrent);
      'O':
      begin
        FStream.Read(lBuf[0], 3);
        if lBuf = {O}'ggS' then
        begin
          FStream.Seek(-4, soFromCurrent);
          Result := True;
         end
        else
          FStream.Seek(-9, soFromCurrent);
      end;
    else
      // we just read the first char in the stream and it wasn't 'O';
      if FStream.Position = 1 then
        Exit(False); // couldn't find a page :(
      FStream.Seek(-5, soFromCurrent);
    end;
   until Result;
end;

function TOggOpusDecoder.FindBestPageForSample(ASampleNumber: QWord; out
  ASegmentIndex: Integer): Boolean;
var
  lMid: Int64;
  lPageStart: Int64;
  lPageEnd, lPreviousPageSampleEnd: Int64;
  lNeedsNewPage: Boolean;
  lBuf: Array[0..AUDIO_BUFFER_SIZE-1] of Byte;
  lSamplesPerFrame, lPacketCount, lPacketSize,
    lCurrentSegmentIndex: Integer;
  lLow, lHigh: QWord;

  procedure LocatePacketInPage;
  var
    lStreamPos: Int64;
  begin
    while FPageSegmentIndex < FPage.SegmentsCount do
      begin
        // store the segment index
        lCurrentSegmentIndex := FPageSegmentIndex;
        // store the SamplePosition in the stream of the segment index start
        lStreamPos := FStream.Position;
        lPacketSize := NextPacketSize(lNeedsNewPage);
        // TODO handle lNeedsNewPage
        FStream.Read(lBuf[0], lPacketSize);
        lSamplesPerFrame := FDecoder.PacketGetSamplesPerFrame(@lBuf, 48000);
        if lSamplesPerFrame + FSamplePosition > ASampleNumber then
        begin
          // rewind the stream and index to the last segment and we're done.
          FPageSegmentIndex := lCurrentSegmentIndex;
          FStream.Position:=lStreamPos;
          ASegmentIndex:=lCurrentSegmentIndex;
          Exit;
        end;
        Inc(FSamplePosition, lSamplesPerFrame);
      end;
  end;
begin
  Result := False;
  if ASampleNumber > FPage.Position then
    lLow := FPageDataStart + FPage.SegmentDataSize(lPacketCount)
  else
    lLow := FFirstDataPagePos;

  if ASampleNumber < FPage.Position then
    lHigh := FPageDataStart + FPage.SegmentDataSize(lPacketCount)
  else
    lHigh:=FStream.Size;
  repeat
    // move to the middle of our shrinking search area
    lMid := (lHigh - lLow) div 2 + lLow;
    FStream.Seek(lMid, soBeginning);

    FindPageStart; // locate the page that starts at or before our current position
    lPageStart:=FStream.Position;
    ReadPageHeader; // page we are comparing to position
    lPageEnd:=lPageStart+FPage.SegmentDataSize(lPacketCount);

    if FPage.Position {- Preskip} > ASampleNumber then
    begin
      // we are on the first page and the end of the page is greater than the sample we want
      if lPageStart = FFirstDataPagePos then
      begin
        LocatePacketInPage;
        Result := True;
        Exit;
      end;

      lPreviousPageSampleEnd := PreviousPageSampleEnd;
      if  lPreviousPageSampleEnd <= ASampleNumber then
      begin
        // this is the end sample of this, the preceding, page and therefore the
        // start of the page we are checking
        FSamplePosition := lPreviousPageSampleEnd - Preskip; // the end of the
        // this is the page our data is on!
        LocatePacketInPage;
        Result := True;
        Exit;
      end
      else
      begin
        //FStream.Position:=lStreamPos;
        //ReadPageHeader;// just to get back to  the same place
        lHigh := lPageStart;
        Continue;
      end;
    end
    else
    begin
      //lPageSize := FPage.SegmentDataSize(lPacketCount);
      // if we didn't find it then seek forward
      lLow := lPageEnd;
      Continue;
    end;


  until lLow  >= lHigh;



end;

procedure TOggOpusDecoder.SetSamplePosition(AValue: QWord);
var
  lSegmentIndex: Integer;
  lBuf: array[0..AUDIO_BUFFER_SIZE] of Byte;
begin
  if FSamplePosition=AValue then Exit;
  if AValue > TotalSamples then
    AValue:=FTotalSamples;
  FSamplePosition:=AValue;
  FindBestPageForSample(AValue, lSegmentIndex);
  {FDecoder.Init(48000, Channels);
  FPageSegmentIndex:=0;
  while FPageSegmentIndex < lSegmentIndex do}


end;

function TOggOpusDecoder.PreviousPageSampleEnd: Int64;
var
  lPage: TOggPageHeader;
  lPosition: Int64;
begin
  if FPage.PageIndex = 2 then
    Exit(0);

  lPosition := FStream.Position;
  FStream.Position:=FPageDataStart-FPage.HeaderSize - 4;
  FindPageStart;
  lPage.ReadFromStream(FStream, True);
  FStream.Position:=lPosition;
  Result := lPage.Position;
end;

constructor TOggOpusDecoder.Create(AStream: TStream);
begin
  FStream := AStream;
  FTotalSamples:=0;
  FComments := TVorbisComments.Create;
  if not (ReadHeader and ReadComments)then
    raise Exception.Create('Invalid Ogg/Opus File');
  ReadPageHeader;
end;

destructor TOggOpusDecoder.Destroy;
begin
  FComments.Free;
  inherited Destroy;
end;

procedure TOggOpusDecoder.InitDecoder;
var
  lError: Integer;
begin
  FDecoder := TOpusDecoder.Create(48000, Channels, lError);
end;

function TOggOpusDecoder.DecodePacket(ADestData: Pointer; ADestSize: Integer; AUseFloat: Boolean = False): Integer;
var
  lNeedNewPage: Boolean;
  lPacketSize: Integer;
  lDataSize: Integer = 0;
  lSkip: Integer;
begin
  Result := 0;
  while FPageIndex < 2 do
  begin
    ReadPageHeader;
    if FPage.PageIndex < 2  then
      FStream.Seek(FPage.SegmentDataSize, soFromCurrent);
  end;

  if IsEndOfPage then
  begin
    if FPage.isLastPageOfLogicalBitstream
    or (FStream.Position = FStream.Size)
    then
      Exit(0);

    ReadPageHeader;
  end;

  repeat
    lPacketSize := NextPacketSize(lNeedNewPage);
    FStream.Read(FBuffer[lDataSize], lPacketSize);

    lDataSize+= lPacketSize;

    if lNeedNewPage then
      ReadPageHeader;
  until lNeedNewPage = False;

  if lDataSize > 0 then
  begin
    if AUseFloat and FDecoder.DecodeFloat(@FBuffer[0], lDataSize, ADestData, ADestSize div SizeOf(Single) div Channels, False) then
      Result := FDecoder.LastError
    else if not AUseFloat and FDecoder.Decode(@FBuffer[0], lDataSize, ADestData, ADestSize div SizeOf(Word) div Channels, False) then
      Result := FDecoder.LastError;
    if IsEndOfPage and (FPage.isLastPageOfLogicalBitstream) then
      Result := FDecoder.LastPacketDuration;
    Inc(FSamplePosition, Result);
  end;

end;

end.

