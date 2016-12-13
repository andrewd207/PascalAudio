{
    This unit is part of Pascal Audio IO package.

    Copyright (c) 2016 by Andrew Haines.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
{ This links to libFLAC which is licensed under the BSD License
  https://xiph.org/flac/license.html
  https://git.xiph.org/?p=flac.git;a=blob_plain;f=COPYING.Xiph
}
unit flac_classes;

{$mode objfpc}{$H+}
{$PACKRECORDS C}
{$linklib FLAC}
interface

uses
  Classes, ctypes, md5;

{$DEFINE FLAC_INTF_TYPES}
  {$i flac_format.inc}
  {$i flac_callbacks.inc}
  {$i flac_metadata.inc}
  {$i flac_encode.inc}
  {$i flac_decode.inc}
{$UNDEF FLAC_INTF_TYPES}

type

  TFlacStreamMetadata = class
  private
    FMetadata: Pointer;
    FType: TFlacMetadataType;
    FOwns: Boolean;
  public
    constructor DefaultCreate(AType: TFlacMetadataType); virtual; // this should not be used directly!!
    class function CreateInstance(AType: TFlacMetadataType; AObj: Pointer = nil): TFlacStreamMetadata;
    destructor Destroy; override;
    function Clone: TFlacStreamMetadata;
    function IsEqual(ABlock: TFlacStreamMetadata): Boolean;
    function GetType: TFlacMetadataType;
  end;

  // a bunch of stuff to implement here
  TFlacStreamMetadataApplication = class(TFlacStreamMetadata)
    //function SetData(Data: PByte; Length: LongWord): Boolean;
    // implement me
  end;

  TFlacStreamMetadataSeekTable = class(TFlacStreamMetadata)
    // implement me
  end;

  TFlacStreamMetadataVorbisComment = class(TFlacStreamMetadata)
    // implement me
  end;

  TFlacStreamMetadataCueSheet = class(TFlacStreamMetadata)
    // implement me
  end;

  TFlacStreamMetadataPicture = class(TFlacStreamMetadata)
    // implement me
  end;

  TFlacStreamMetadataPadding = class(TFlacStreamMetadata)
    // implement me
  end;

  TFlacStreamMetadataStreamInfo = class(TFlacStreamMetadata)
  private
    function GetBitsPerSample: cunsigned;
    function GetChannels: cunsigned;
    function GetMaxBlocksize: cunsigned;
    function GetMaxFramesize: cunsigned;
    function GetMD5Sum: TMD5Digest;
    function GetMinBlocksize: cunsigned;
    function GetMinFramesize: cunsigned;
    function GetSamplerate: cunsigned;
    function GetTotalSamples: QWord;
    procedure SetBitsPerSample(AValue: cunsigned);
    procedure SetChannels(AValue: cunsigned);
    procedure SetMaxBlocksize(AValue: cunsigned);
    procedure SetMaxFramesize(AValue: cunsigned);
    procedure SetMD5Sum(AValue: TMD5Digest);
    procedure SetMinBlocksize(AValue: cunsigned);
    procedure SetMinFramesize(AValue: cunsigned);
    procedure SetSamplerate(AValue: cunsigned);
    procedure SetTotalSamples(AValue: QWord);
  public
    property MinBlocksize: cunsigned read GetMinBlocksize write SetMinBlocksize;
    property MaxBlocksize: cunsigned read GetMaxBlocksize write SetMaxBlocksize;
    property MinFramesize: cunsigned read GetMinFramesize write SetMinFramesize;
    property MaxFramesize: cunsigned read GetMaxFramesize write SetMaxFramesize;
    property Samplerate: cunsigned read GetSamplerate write SetSamplerate;
    property Channels: cunsigned read GetChannels write SetChannels;
    property BitsPerSample: cunsigned read GetBitsPerSample write SetBitsPerSample;
    property TotalSamples: QWord read GetTotalSamples write SetTotalSamples;
    property MD5Sum: TMD5Digest read GetMD5Sum write SetMD5Sum;

  end;

  TFlacMetadataSimpleIterator = class
  private
    FIterator: Pointer;
    function GetApplicationID: Byte;
    function GetBlock(UsePadding: Boolean = False): TFlacStreamMetadata;
    function GetBlockLength: LongWord;
    function GetBlockOffset: PtrInt;
    function GetBlockType: TFlacMetadataType;
    function GetIsLast: Boolean;
    function GetIsWritable: Boolean;
    function GetStatus: TFlacMetadataSimpleIteratorStatus;
    procedure SetBlock(UsePadding: Boolean; AValue: TFlacStreamMetadata);
  public
    constructor Create;
    destructor Destroy; override;
    property Status: TFlacMetadataSimpleIteratorStatus read GetStatus;
    property IsWritable: Boolean read GetIsWritable;
    function MoveNext: Boolean;
    function MovePrev: Boolean;
    property IsLast: Boolean read GetIsLast;
    property BlockOffset: PtrInt read GetBlockOffset;
    property BlockType: TFlacMetadataType read GetBlockType;
    property BlockLength: LongWord read GetBlockLength;
    property ApplicationID: Byte read GetApplicationID;
    property Block[UsePadding: Boolean{ignored for get}]: TFlacStreamMetadata read GetBlock write SetBlock;
    function InsertBlockAfter(ABlock: TFlacStreamMetadata; UsePadding: Boolean): Boolean;
    function DeleteBlock(ABlock: TFlacStreamMetadata; UsePadding: Boolean): Boolean;
  end;

  TFlacMetadataChain = class
  private
    FMetadataChain: Pointer; // PFlacMetadataChainStruct;
    function GetStatus: TFlacMetadataChainStatus;
    class function ClassStreamRead (ptr: Pointer; size, num_elems: csize_t; handle: PFlacIOHandle): csize_t; cdecl; static;
    class function ClassStreamWrite(ptr: Pointer; size, num_elems: csize_t; handle: PFlacIOHandle): csize_t; cdecl; static;
    class function ClassStreamSeek (handle: PFlacIOHandle; offset: Int64; whence: cint): cint; cdecl; static;
    class function ClassStreamTell(handle: PFlacIOHandle): Int64; cdecl; static;
    class function ClassStreamEOF(handle: PFlacIOHandle): cint; cdecl; static;
    class function ClassStreamClose (handle: PFlacIOHandle): cint; cdecl; static;
    class var
      Callbacks: TFlacIOCallbacks;
  public
    class constructor Create;
    constructor Create;
    destructor Destroy; override;
    property  Status: TFlacMetadataChainStatus read GetStatus;
    function  Read(AStream: TStream): Boolean;
    function  ReadOgg(AStream: TStream): Boolean;
    function  CheckIfTempFileNeeded(AUsePAdding: Boolean): Boolean;
    function  Write(AStream: TStream; AUsePadding: Boolean): Boolean;
    function  WriteWithTempFile(AStream: TStream; ATempFile: TStream; AUsePadding: Boolean): Boolean;
    procedure MergePadding;
    procedure SortPadding;
  end;

  TFlacMetadataIterator = class
  private
    FIterator: Pointer;
    function GetBlock: TFlacStreamMetadata;
    procedure SetBlock(AValue: TFlacStreamMetadata);
  public
    constructor Create;
    destructor Destroy; override;
    function Init(Chain: TFlacMetadataChain): Boolean;
    function Next: Boolean;
    function Prev: Boolean;
    function BlockType: TFlacMetadataType;
    property Block: TFlacStreamMetadata read GetBlock write SetBlock;
    function DeleteBlock(ReplaceWithPAdding: Boolean): Boolean;
    function InsertBlockBefore(ABlock: TFlacStreamMetadata): Boolean;
    function InsertBlockAfter(ABlock: TFlacStreamMetadata): Boolean;

  end;

  TFlacStreamEncoder = class
  private
    FApodization: String;
    FBlockSize: Integer;
    FCompressionLevel: Integer;
    FDoLooseMidSideStereo: Boolean;
    FDoMidSideStereo: Boolean;
    FEncoder: Pointer;  // PFlacStreamEncoderStruct
    FOggSerialNumber: LongWord;
    FStreamableSubset: Boolean;
    FVerify: Boolean;
    function GetBitsPerSample: Integer;
    function GetChannels: Integer;
    function GetDoEscapeCoding: Boolean;
    function GetDoExhaustiveModelSearch: Boolean;
    function GetDoQlpCoeffPrecSearch: Boolean;
    function GetMaxLPCOrder: Integer;
    function GetMaxResidualPartitionOrder: Integer;
    function GetMinResidualPartitionOrder: Integer;
    function GetQlpCoeffPrecision: Integer;
    function GetRiceParameterSearchDist: Integer;
    function GetSampleRate: Integer;
    function GetState: TFlacStreamEncoderState;
    function GetTotalSamplesEstimate: QWord;
    procedure SetApodization(AValue: String);
    procedure SetBitsPerSample(AValue: Integer);
    procedure SetBlockSize(AValue: Integer);
    procedure SetChannels(AValue: Integer);
    procedure SetCompressionLevel(AValue: Integer);
    procedure SetDoEscapeCoding(AValue: Boolean);
    procedure SetDoExhaustiveModelSearch(AValue: Boolean);
    procedure SetDoLooseMidSideStereo(AValue: Boolean);
    procedure SetDoMidSideStereo(AValue: Boolean);
    procedure SetDoQlpCoeffPrecSearch(AValue: Boolean);
    procedure SetMaxLPCOrder(AValue: Integer);
    procedure SetMaxResidualPartitionOrder(AValue: Integer);
    procedure SetMinResidualPartitionOrder(AValue: Integer);
    procedure SetOggSerialNumber(ASerialNumber: LongWord);
    procedure SetQlpCoeffPrecision(AValue: Integer);
    procedure SetRiceParameterSearchDist(AValue: Integer);
    procedure SetSampleRate(AValue: Integer);
    procedure SetStreamableSubset(AValue: Boolean);
    procedure SetTotalSamplesEstimate(AValue: QWord);
    procedure SetVerify(AValue: Boolean);
  private
    class function  ClassReadHandler(Encoder: Pointer; Buffer: PByte; bytes: csize_t; userdata: Pointer):TFlacStreamEncoderReadStatus; cdecl; static;
    class function  ClassWriteHandler(Encoder: Pointer; Buffer: PByte; bytes: csize_t; samples, current_frame: cunsigned; userdata: Pointer):TFlacStreamEncoderWriteStatus; cdecl; static;
    class function  ClassSeekHandler(Encoder: Pointer; absolute_byte_offset: QWord; userdata: Pointer):TFlacStreamEncoderSeekStatus; cdecl; static;
    class function  ClassTellHandler(Encoder: Pointer; absolute_byte_offset: PQWord; userdata: Pointer):TFlacStreamEncoderTellStatus; cdecl; static;
    class procedure ClassMetadataHandler(Encoder: Pointer; Metadata: Pointer; userdata: Pointer); cdecl; static;
    class procedure ClassProgressHandler(Encoder: Pointer; bytes_written, samples_Written: QWord; frames_written, total_frames_estimate: cunsigned; userdata: Pointer); cdecl; static;
    class function  OfObjectToAddr(Method: TMethod): Pointer; static;
  protected
    function  ReadHandler(Buffer: PByte; Bytes: PtrUInt): TFlacStreamEncoderReadStatus; virtual; abstract;
    function  WriteHandler(Buffer: PByte; Bytes: PtrUInt; Samples, CurrentFrame: LongWord): TFlacStreamEncoderWriteStatus; virtual; abstract;
    function  SeekHandler(AbsByteOffset: QWord): TFlacStreamEncoderSeekStatus; virtual; abstract;
    function  TellHandler(AbsByteOffset: PQWord): TFlacStreamEncoderTellStatus; virtual; abstract;
    procedure MetadataHandler(Metadata: Pointer); virtual; abstract;
    procedure ProgressHandler(BytesWritten, SamplesWritten: QWord; FramesWritten, TotalFramesEstimate: LongWord); virtual; abstract;
  public
    constructor Create;
    destructor  Destroy; override;

    property    OggSerialNumber: LongWord read FOggSerialNumber write SetOggSerialNumber;
    property    Verify: Boolean read FVerify write SetVerify;
    property    StreamableSubset: Boolean read FStreamableSubset write SetStreamableSubset;
    property    Channels: Integer read GetChannels write SetChannels;
    property    BitsPerSample: Integer read GetBitsPerSample write SetBitsPerSample;
    property    SampleRate: Integer read GetSampleRate write SetSampleRate;
    property    CompressionLevel: Integer read FCompressionLevel write SetCompressionLevel;
    property    BlockSize: Integer read FBlockSize write SetBlockSize;
    property    DoMidSideStereo: Boolean read FDoMidSideStereo write SetDoMidSideStereo;
    property    DoLooseMidSideStereo: Boolean read FDoLooseMidSideStereo write SetDoLooseMidSideStereo;
    property    Apodization: String read FApodization write SetApodization;
    property    MaxLPCOrder: Integer read GetMaxLPCOrder write SetMaxLPCOrder;
    property    QlpCoeffPrecision: Integer read GetQlpCoeffPrecision write SetQlpCoeffPrecision;
    property    DoQlpCoeffPrecSearch: Boolean read GetDoQlpCoeffPrecSearch write SetDoQlpCoeffPrecSearch;
    property    DoEscapeCoding: Boolean read GetDoEscapeCoding write SetDoEscapeCoding;
    property    DoExhaustiveModelSearch: Boolean read GetDoExhaustiveModelSearch write SetDoExhaustiveModelSearch;
    property    MinResidualPartitionOrder: Integer read GetMinResidualPartitionOrder write SetMinResidualPartitionOrder;
    property    MaxResidualPartitionOrder: Integer read GetMaxResidualPartitionOrder write SetMaxResidualPartitionOrder;
    property    RiceParameterSearchDist: Integer read GetRiceParameterSearchDist write SetRiceParameterSearchDist;
    property    TotalSamplesEstimate: QWord read GetTotalSamplesEstimate write SetTotalSamplesEstimate;
    property    State: TFlacStreamEncoderState read GetState;
    //property    VerifyDecoderState: TFlacStreamDecoderState read GetVerifyDecoderState;
    // procedure SetMetadata
  end;

  TFlacStreamDecoder = class;

  TFlacStreamDecoderWriteEvent = function (Sender: TFlacStreamDecoder; Samples: Integer; Channels: Integer; ChannelData: PPLongInt): Boolean of object;
  TFlacStreamDecoderMetadataEvent = procedure (Sender: TFlacStreamDecoder; Metadata: TFlacStreamMetadata) of object;
  TFlacStreamDecoderErrorEvent = procedure (Sender: TFlacStreamDecoder; Error: TFlacStreamDecoderErrorStatus) of object;

  TFlacStreamDecoder = class
  private
    FDecoder: Pointer;
    FOnError: TFlacStreamDecoderErrorEvent;
    FOnMetadata: TFlacStreamDecoderMetadataEvent;
    FOnOutput: TFlacStreamDecoderWriteEvent;
    FStream: TStream;
    FChannels: Integer;
    FSampleRate: Integer;
    FBitsperSample: Integer;
    function GetMd5Checking: Boolean;
    function GetResolvedStateString: String;
    function GetState: TFlacStreamDecoderState;
    procedure SetMd5Checking(AValue: Boolean);
    class function ClassStreamRead(Decoder: Pointer; Buffer: PByte; bytes: pcsize_t; client_data: pointer): TFlacStreamDecoderReadStatus; cdecl; static;
    class function ClassStreamSeek(Decoder: Pointer; abs_byte_offset: QWord; client_data: pointer): TFlacStreamDecoderSeekStatus; cdecl; static;
    class function ClassStreamTell(Decoder: Pointer; abs_byte_offset: PQWord; client_data: pointer): TFlacStreamDecoderTellStatus; cdecl; static;
    class function ClassStreamLength(Decoder: Pointer; stream_length: PQWord; client_data: pointer): TFlacStreamDecoderLengthStatus; cdecl; static;
    class function ClassStreamEOF(Decoder: Pointer; client_data: pointer): TFlacBool; cdecl; static;
    class function ClassStreamWrite(Decoder: Pointer; frame: Pointer; buffer: PPLongInt; client_data: pointer): TFlacStreamDecoderWriteStatus; cdecl; static;
    class procedure ClassStreamMetadata(Decoder: Pointer; metadata: Pointer; client_data: pointer); cdecl; static;
    class procedure ClassStreamError(Decoder: Pointer; status: TFlacStreamDecoderErrorStatus; client_data: pointer); cdecl; static;

  protected
    function  HandleWriteDecodedData(ASamplesPerChannel: Integer; AChannels: Integer;  ChannelsData: PPLongInt): Boolean; virtual;
    procedure HandleMetadataObject(AMetadataObject: TFlacStreamMetadata); virtual;
    procedure HandleError(AError: TFlacStreamDecoderErrorStatus); virtual;
  public
    constructor Create(AStream: TStream; AIsOgg: Boolean);
    destructor  Destroy; override;
    function    SetOggSerialNumber(ASerialNumber: LongWord): Boolean;
    property    Md5Checking: Boolean read GetMd5Checking write SetMd5Checking;
    function    SetMetadataRespond(AType: TFlacMetadataType): Boolean;
    function    SetMetadataIgnore(AType: TFlacMetadataType): Boolean;
    function    SetMetadataIgnoreAll: Boolean;
    property    State: TFlacStreamDecoderState read GetState;
    property    ResolvedStateString: String read GetResolvedStateString;
    function    TotalSamples: QWord;
    function    Channels: Integer;
    function    ChannelAssignment: TFlacChannelAssignment;
    function    BitsPerSample: Integer;
    function    SampleRate: Integer;
    function    BlockSize: Integer;
    function    DecodePosition: QWord;
    function    Flush: Boolean;
    function    Reset: Boolean;
    function    ProcessSingle: Boolean;
    function    ProcessUntilEndOfMetadata: Boolean;
    function    ProcessUntilEndOfStream: Boolean;
    function    SkipSingleFrame: Boolean;
    function    SeekAbsolute(ASample: QWord): Boolean;
    property    OnOutput: TFlacStreamDecoderWriteEvent read FOnOutput write FOnOutput;
    property    OnMetadata: TFlacStreamDecoderMetadataEvent read FOnMetadata write FOnMetadata;
    property    OnError: TFlacStreamDecoderErrorEvent read FOnError write FOnError;
  end;


implementation

{$DEFINE FLAC_INTF}
{$i flac_format.inc}
{$i flac_callbacks.inc}
{$i flac_metadata.inc}
{$i flac_encode.inc}
{$i flac_decode.inc}
{$UNDEF FLAC_INTF}

{$DEFINE FLAC_IMPL}
{$i flac_format.inc}
{$i flac_callbacks.inc}
{$i flac_metadata.inc}
{$i flac_encode.inc}
{$i flac_decode.inc}
{$UNDEF FLAC_IMPL}

{ TFlacStreamMetadata }

constructor TFlacStreamMetadata.DefaultCreate(AType: TFlacMetadataType);
begin
  FType:=AType;
end;

class function TFlacStreamMetadata.CreateInstance(AType: TFlacMetadataType; AObj: Pointer): TFlacStreamMetadata;
begin
  case AType of
    fmtApplication: Result :=  TFlacStreamMetadataApplication.DefaultCreate(fmtApplication);
    fmtCueSheet: Result :=  TFlacStreamMetadataCueSheet.DefaultCreate(fmtCueSheet);
    fmtPicture: Result :=  TFlacStreamMetadataPicture.DefaultCreate(fmtPicture);
    fmtSeekTable: Result :=  TFlacStreamMetadataSeekTable.DefaultCreate(fmtSeekTable);
    fmtVorbisComment: Result :=  TFlacStreamMetadataVorbisComment.DefaultCreate(fmtVorbisComment);
    fmtPadding: Result :=  TFlacStreamMetadataPadding.DefaultCreate(AType);
    fmtStreamInfo: Result :=  TFlacStreamMetadataStreamInfo.DefaultCreate(AType);
  end;

  if Assigned(AObj) then
    Result.FMetadata:=AObj
  else
  begin
    Result.FMetadata:=FLAC__metadata_object_new(AType);
    Result.FOwns:=True;
  end;

end;

destructor TFlacStreamMetadata.Destroy;
begin
  if FOwns then
    FLAC__metadata_object_delete(FMetadata);
  inherited Destroy;
end;

function TFlacStreamMetadata.Clone: TFlacStreamMetadata;
begin
  Result := CreateInstance(FType, FLAC__metadata_object_clone(FMetadata));
end;

function TFlacStreamMetadata.IsEqual(ABlock: TFlacStreamMetadata): Boolean;
begin
  Result := FLAC__metadata_object_is_equal(FMetadata, ABlock.FMetadata);
end;

function TFlacStreamMetadata.GetType: TFlacMetadataType;
begin
  Result := FType;
end;

{ TFlacStreamMetadataStreamInfo }

function TFlacStreamMetadataStreamInfo.GetBitsPerSample: cunsigned;
begin
  Result := PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.BitsPerSample;
end;

function TFlacStreamMetadataStreamInfo.GetChannels: cunsigned;
begin
  Result := PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.Channels;
end;

function TFlacStreamMetadataStreamInfo.GetMaxBlocksize: cunsigned;
begin
  Result := PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.MaxBlocksize;
end;

function TFlacStreamMetadataStreamInfo.GetMaxFramesize: cunsigned;
begin
  Result := PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.MaxFramesize;
end;

function TFlacStreamMetadataStreamInfo.GetMD5Sum: TMD5Digest;
begin
  Result := TMD5Digest(PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.MD5Sum);
end;

function TFlacStreamMetadataStreamInfo.GetMinBlocksize: cunsigned;
begin
  Result := PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.MinBlocksize;
end;

function TFlacStreamMetadataStreamInfo.GetMinFramesize: cunsigned;
begin
  Result := PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.MinFramesize;

end;

function TFlacStreamMetadataStreamInfo.GetSamplerate: cunsigned;
begin
  Result := PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.Samplerate;
end;

function TFlacStreamMetadataStreamInfo.GetTotalSamples: QWord;
begin
  Result := PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.TotalSamples;
end;

procedure TFlacStreamMetadataStreamInfo.SetBitsPerSample(AValue: cunsigned);
begin
  PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.BitsPerSample := AValue;
end;

procedure TFlacStreamMetadataStreamInfo.SetChannels(AValue: cunsigned);
begin
  PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.Channels := AValue;
end;

procedure TFlacStreamMetadataStreamInfo.SetMaxBlocksize(AValue: cunsigned);
begin
  PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.MaxBlocksize := AValue;
end;

procedure TFlacStreamMetadataStreamInfo.SetMaxFramesize(AValue: cunsigned);
begin
  PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.MaxFramesize := AValue;
end;

procedure TFlacStreamMetadataStreamInfo.SetMD5Sum(AValue: TMD5Digest);
begin
  PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.MD5Sum := AValue;
end;

procedure TFlacStreamMetadataStreamInfo.SetMinBlocksize(AValue: cunsigned);
begin
  PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.MinBlocksize := AValue;
end;

procedure TFlacStreamMetadataStreamInfo.SetMinFramesize(AValue: cunsigned);
begin
  PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.MinFramesize := AValue;
end;

procedure TFlacStreamMetadataStreamInfo.SetSamplerate(AValue: cunsigned);
begin
  PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.Samplerate := AValue;
end;

procedure TFlacStreamMetadataStreamInfo.SetTotalSamples(AValue: QWord);
begin
  PFlacStreamMetadataStruct(FMetadata)^.Data.StreamInfo.TotalSamples := AValue;
end;


{ TFlacMetadataSimpleIterator }

function TFlacMetadataSimpleIterator.GetBlock(UsePadding: Boolean): TFlacStreamMetadata;
var
  lType: TFlacMetadataType;
begin
  lType := GetBlockType;
  Result := TFlacStreamMetadata.CreateInstance(lType, FLAC__metadata_simple_iterator_get_block(FIterator));
end;

function TFlacMetadataSimpleIterator.GetApplicationID: Byte;
begin
  if not FLAC__metadata_simple_iterator_get_application_id(FIterator, @Result) then
    Result := 0;
end;

function TFlacMetadataSimpleIterator.GetBlockLength: LongWord;
begin
  Result := FLAC__metadata_simple_iterator_get_block_length(FIterator);
end;

function TFlacMetadataSimpleIterator.GetBlockOffset: PtrInt;
begin
  Result := FLAC__metadata_simple_iterator_get_block_offset(FIterator);
end;

function TFlacMetadataSimpleIterator.GetBlockType: TFlacMetadataType;
begin
  Result := FLAC__metadata_simple_iterator_get_block_type(FIterator);
end;

function TFlacMetadataSimpleIterator.GetIsLast: Boolean;
begin
  Result := FLAC__metadata_simple_iterator_is_last(FIterator);
end;

function TFlacMetadataSimpleIterator.GetIsWritable: Boolean;
begin
  Result := FLAC__metadata_simple_iterator_is_writable(FIterator);
end;

function TFlacMetadataSimpleIterator.GetStatus: TFlacMetadataSimpleIteratorStatus;
begin
  Result := FLAC__metadata_simple_iterator_status(FIterator);
end;

procedure TFlacMetadataSimpleIterator.SetBlock(UsePadding: Boolean; AValue: TFlacStreamMetadata);
begin
  FLAC__metadata_simple_iterator_set_block(FIterator, AValue.FMetadata, UsePadding);
end;

constructor TFlacMetadataSimpleIterator.Create;
begin
  FIterator:=FLAC__metadata_simple_iterator_new;
end;

destructor TFlacMetadataSimpleIterator.Destroy;
begin
  FLAC__metadata_simple_iterator_delete(FIterator);
  inherited Destroy;
end;

function TFlacMetadataSimpleIterator.MoveNext: Boolean;
begin
  Result := FLAC__metadata_simple_iterator_next(FIterator);
end;

function TFlacMetadataSimpleIterator.MovePrev: Boolean;
begin
  Result := FLAC__metadata_simple_iterator_prev(FIterator);
end;

function TFlacMetadataSimpleIterator.InsertBlockAfter(ABlock: TFlacStreamMetadata; UsePadding: Boolean): Boolean;
begin
  Result := FLAC__metadata_simple_iterator_insert_block_after(FIterator, ABlock.FMetadata, UsePadding);
end;

function TFlacMetadataSimpleIterator.DeleteBlock(ABlock: TFlacStreamMetadata; UsePadding: Boolean): Boolean;
begin
  Result := FLAC__metadata_simple_iterator_delete_block(FIterator, ABlock.FMetadata, UsePadding);
end;

{ TFlacMetadataChain }

function TFlacMetadataChain.GetStatus: TFlacMetadataChainStatus;
begin
  Result := FLAC__metadata_chain_status(FMetadataChain);
end;

class function TFlacMetadataChain.ClassStreamRead(ptr: Pointer; size, num_elems: csize_t; handle: PFlacIOHandle): csize_t; cdecl;
var
  Stream: TStream absolute handle;
begin
  Result := Stream.Read(ptr^, num_elems*size);
end;

class function TFlacMetadataChain.ClassStreamWrite(ptr: Pointer; size, num_elems: csize_t; handle: PFlacIOHandle): csize_t; cdecl;
var
  Stream: TStream absolute handle;
begin
  Result := Stream.Write(ptr^, num_elems*size);
end;

class function TFlacMetadataChain.ClassStreamSeek(handle: PFlacIOHandle; offset: Int64; whence: cint): cint; cdecl;
var
  Stream: TStream absolute handle;
  Origin: TSeekOrigin;
begin
  case whence of
    SEEK_SET: Origin:=soBeginning;
    SEEK_CUR: Origin:=soCurrent;
    SEEK_END: Origin:=soEnd;
  end;

  Result := Stream.Seek(offset, Origin);
end;

class function TFlacMetadataChain.ClassStreamTell(handle: PFlacIOHandle): Int64; cdecl;
var
  Stream: TStream absolute handle;
begin
  Result := Stream.Position;
end;

class function TFlacMetadataChain.ClassStreamEOF(handle: PFlacIOHandle): cint; cdecl;
var
  Stream: TStream absolute handle;
begin
  if Stream.Position < Stream.Size then
    Result := 0
  else
    Result := -1;
end;

class function TFlacMetadataChain.ClassStreamClose(handle: PFlacIOHandle): cint; cdecl;
var
  Stream: TStream absolute handle;
begin
  try
    Stream.Free;
    Result := 0;
  except
    Result := -1;
  end;

end;

class constructor TFlacMetadataChain.Create;
begin
  with TFlacMetadataChain do
  begin
    Callbacks.read:=TFlacIOCallbackRead(@TFlacMetadataChain.ClassStreamRead);
    Callbacks.write:=TFlacIOCallbackWrite(@TFlacMetadataChain.ClassStreamWrite);
    Callbacks.seek:=TFlacIOCallbackSeek(@TFlacMetadataChain.ClassStreamSeek);
    Callbacks.tell:=TFlacIOCallbackTell(@TFlacMetadataChain.ClassStreamTell);
    Callbacks.eof:=TFlacIOCallbackEof(@TFlacMetadataChain.ClassStreamEOF);
    Callbacks.close:=TFlacIOCallbackClose(@TFlacMetadataChain.ClassStreamClose);
  end;
end;

constructor TFlacMetadataChain.Create;
begin
  FMetadataChain := FLAC__metadata_chain_new;
end;

destructor TFlacMetadataChain.Destroy;
begin
  FLAC__metadata_chain_delete(FMetadataChain);
  inherited Destroy;
end;

function TFlacMetadataChain.Read(AStream: TStream): Boolean;
begin
  Result := FLAC__metadata_chain_read_with_callbacks(FMetadataChain, Pointer(AStream), @Callbacks);
end;

function TFlacMetadataChain.ReadOgg(AStream: TStream): Boolean;
begin
  Result := FLAC__metadata_chain_read_ogg_with_callbacks(FMetadataChain, Pointer(AStream), @Callbacks);
end;

function TFlacMetadataChain.CheckIfTempFileNeeded(AUsePAdding: Boolean): Boolean;
begin
  Result := FLAC__metadata_chain_check_if_tempfile_needed(FMetadataChain, AUsePadding);
end;

function TFlacMetadataChain.Write(AStream: TStream; AUsePadding: Boolean): Boolean;
begin
  Result := FLAC__metadata_chain_write_with_callbacks(FMetadataChain, AUsePadding, Pointer(AStream),@Callbacks);
end;

function TFlacMetadataChain.WriteWithTempFile(AStream: TStream; ATempFile: TStream; AUsePadding: Boolean): Boolean;
begin
  Result := FLAC__metadata_chain_write_with_callbacks_and_tempfile(FMetadataChain, AUsePadding, Pointer(AStream),@Callbacks, Pointer(ATempFile), @Callbacks);
end;

procedure TFlacMetadataChain.MergePadding;
begin
  FLAC__metadata_chain_merge_padding(FMetadataChain);
end;

procedure TFlacMetadataChain.SortPadding;
begin
  FLAC__metadata_chain_sort_padding(FMetadataChain);
end;

{ TFlacMetadataIterator }

function TFlacMetadataIterator.GetBlock: TFlacStreamMetadata;
begin
  Result := TFlacStreamMetadata.CreateInstance(BlockType, FLAC__metadata_iterator_get_block(FIterator));
end;

procedure TFlacMetadataIterator.SetBlock(AValue: TFlacStreamMetadata);
begin
  FLAC__metadata_iterator_set_block(FIterator, AValue.FMetadata);
end;

constructor TFlacMetadataIterator.Create;
begin
  FIterator:=FLAC__metadata_iterator_new;
end;

destructor TFlacMetadataIterator.Destroy;
begin
  FLAC__metadata_iterator_delete(FIterator);
  inherited Destroy;
end;

function TFlacMetadataIterator.Init(Chain: TFlacMetadataChain): Boolean;
begin
  Result := FLAC__metadata_iterator_init(FIterator, Chain.FMetadataChain);
end;

function TFlacMetadataIterator.Next: Boolean;
begin
  Result := FLAC__metadata_iterator_next(FIterator);
end;

function TFlacMetadataIterator.Prev: Boolean;
begin
  Result := FLAC__metadata_iterator_prev(FIterator);
end;

function TFlacMetadataIterator.BlockType: TFlacMetadataType;
begin
  Result := FLAC__metadata_iterator_get_block_type(FIterator);
end;

function TFlacMetadataIterator.DeleteBlock(ReplaceWithPAdding: Boolean): Boolean;
begin
  Result := FLAC__metadata_iterator_delete_block(FIterator,ReplaceWithPAdding);
end;

function TFlacMetadataIterator.InsertBlockBefore(ABlock: TFlacStreamMetadata): Boolean;
begin
  Result := FLAC__metadata_iterator_insert_block_before(FIterator, ABlock.FMetadata);
end;

function TFlacMetadataIterator.InsertBlockAfter(ABlock: TFlacStreamMetadata): Boolean;
begin
  Result := FLAC__metadata_iterator_insert_block_after(FIterator, ABlock.FMetadata);
end;

{ TFlacStreamEncoder }

procedure TFlacStreamEncoder.SetApodization(AValue: String);
begin
  FLAC__stream_encoder_set_apodization(FEncoder, PChar(AValue));
end;

function TFlacStreamEncoder.GetChannels: Integer;
begin
  Result := FLAC__stream_encoder_get_channels(FEncoder);
end;

function TFlacStreamEncoder.GetDoEscapeCoding: Boolean;
begin
  Result := FLAC__stream_encoder_get_do_escape_coding(FEncoder);
end;

function TFlacStreamEncoder.GetDoExhaustiveModelSearch: Boolean;
begin
  Result := FLAC__stream_encoder_get_do_exhaustive_model_search(FEncoder);
end;

function TFlacStreamEncoder.GetDoQlpCoeffPrecSearch: Boolean;
begin
  Result := FLAC__stream_encoder_get_do_qlp_coeff_prec_search(FEncoder);
end;

function TFlacStreamEncoder.GetMaxLPCOrder: Integer;
begin
  Result := FLAC__stream_encoder_get_max_lpc_order(FEncoder);
end;

function TFlacStreamEncoder.GetMaxResidualPartitionOrder: Integer;
begin
  Result := FLAC__stream_encoder_get_max_residual_partition_order(FEncoder);
end;

function TFlacStreamEncoder.GetMinResidualPartitionOrder: Integer;
begin
  Result := FLAC__stream_encoder_get_min_residual_partition_order(FEncoder);
end;

function TFlacStreamEncoder.GetQlpCoeffPrecision: Integer;
begin
  Result := FLAC__stream_encoder_get_qlp_coeff_precision(FEncoder);
end;

function TFlacStreamEncoder.GetRiceParameterSearchDist: Integer;
begin
  Result := FLAC__stream_encoder_get_rice_parameter_search_dist(FEncoder);
end;

function TFlacStreamEncoder.GetSampleRate: Integer;
begin
  Result := FLAC__stream_encoder_get_sample_rate(FEncoder);
end;

function TFlacStreamEncoder.GetBitsPerSample: Integer;
begin
  Result := FLAC__stream_encoder_get_bits_per_sample(FEncoder);
end;

function TFlacStreamEncoder.GetState: TFlacStreamEncoderState;
begin
  Result := FLAC__stream_encoder_get_state(FEncoder);
end;

function TFlacStreamEncoder.GetTotalSamplesEstimate: QWord;
begin
  Result := FLAC__stream_encoder_get_total_samples_estimate(FEncoder);
end;

procedure TFlacStreamEncoder.SetBitsPerSample(AValue: Integer);
begin
  FLAC__stream_encoder_set_bits_per_sample(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetBlockSize(AValue: Integer);
begin
  FLAC__stream_encoder_set_blocksize(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetChannels(AValue: Integer);
begin
  FLAC__stream_encoder_set_channels(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetCompressionLevel(AValue: Integer);
begin
  FLAC__stream_encoder_set_compression_level(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetDoEscapeCoding(AValue: Boolean);
begin
  FLAC__stream_encoder_set_do_escape_coding(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetDoExhaustiveModelSearch(AValue: Boolean);
begin
  FLAC__stream_encoder_set_do_exhaustive_model_search(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetDoLooseMidSideStereo(AValue: Boolean);
begin
  FLAC__stream_encoder_set_loose_mid_side_stereo(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetDoMidSideStereo(AValue: Boolean);
begin
  FLAC__stream_encoder_set_do_mid_side_stereo(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetDoQlpCoeffPrecSearch(AValue: Boolean);
begin
  FLAC__stream_encoder_set_do_qlp_coeff_prec_search(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetMaxLPCOrder(AValue: Integer);
begin
  FLAC__stream_encoder_set_max_lpc_order(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetMaxResidualPartitionOrder(AValue: Integer);
begin
  FLAC__stream_encoder_set_max_residual_partition_order(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetMinResidualPartitionOrder(AValue: Integer);
begin
  FLAC__stream_encoder_set_min_residual_partition_order(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetOggSerialNumber(ASerialNumber: LongWord);
begin
  FLAC__stream_encoder_set_ogg_serial_number(FEncoder, ASerialNumber);
end;

procedure TFlacStreamEncoder.SetQlpCoeffPrecision(AValue: Integer);
begin
  FLAC__stream_encoder_set_qlp_coeff_precision(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetRiceParameterSearchDist(AValue: Integer);
begin
  FLAC__stream_encoder_set_rice_parameter_search_dist(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetSampleRate(AValue: Integer);
begin
  FLAC__stream_encoder_set_sample_rate(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetStreamableSubset(AValue: Boolean);
begin
  FLAC__stream_encoder_set_streamable_subset(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetTotalSamplesEstimate(AValue: QWord);
begin
  FLAC__stream_encoder_set_total_samples_estimate(FEncoder, AValue);
end;

procedure TFlacStreamEncoder.SetVerify(AValue: Boolean);
begin
  FLAC__stream_encoder_set_verify(FEncoder, AValue);
end;

class function TFlacStreamEncoder.ClassReadHandler(Encoder: Pointer; Buffer: PByte; bytes: csize_t; userdata: Pointer): TFlacStreamEncoderReadStatus; cdecl;
var
  Obj: TFlacStreamEncoder absolute userdata;
begin
  Result := Obj.ReadHandler(Buffer, Bytes);
end;

class function TFlacStreamEncoder.ClassWriteHandler(Encoder: Pointer; Buffer: PByte; bytes: csize_t; samples, current_frame: cunsigned; userdata: Pointer): TFlacStreamEncoderWriteStatus; cdecl;
var
  Obj: TFlacStreamEncoder absolute userdata;
begin
  Result := Obj.WriteHandler(Buffer, bytes, samples, current_frame);
end;

class function TFlacStreamEncoder.ClassSeekHandler(Encoder: Pointer; absolute_byte_offset: QWord; userdata: Pointer): TFlacStreamEncoderSeekStatus; cdecl;
var
  Obj: TFlacStreamEncoder absolute userdata;
begin
  Result := Obj.SeekHandler(absolute_byte_offset);
end;

class function TFlacStreamEncoder.ClassTellHandler(Encoder: Pointer; absolute_byte_offset: PQWord; userdata: Pointer): TFlacStreamEncoderTellStatus; cdecl;
var
  Obj: TFlacStreamEncoder absolute userdata;
begin
  Result := Obj.TellHandler(absolute_byte_offset);

end;

class procedure TFlacStreamEncoder.ClassMetadataHandler(Encoder: Pointer; Metadata: Pointer; userdata: Pointer); cdecl;
var
  Obj: TFlacStreamEncoder absolute userdata;
begin
  Obj.MetadataHandler(Metadata);
end;

class procedure TFlacStreamEncoder.ClassProgressHandler(Encoder: Pointer; bytes_written, samples_Written: QWord; frames_written, total_frames_estimate: cunsigned; userdata: Pointer); cdecl;
var
  Obj: TFlacStreamEncoder absolute userdata;
begin
  Obj.ProgressHandler(bytes_written, samples_Written, frames_written, total_frames_estimate);
end;

class function TFlacStreamEncoder.OfObjectToAddr(Method: TMethod): Pointer;
begin
  Result := Method.Code;
end;

constructor TFlacStreamEncoder.Create;
begin
  FEncoder:=FLAC__stream_encoder_new;
  FLAC__stream_encoder_init_stream(FEncoder,
     TFlacStreamEncoderWriteCallback(@TFlacStreamEncoder.ClassWriteHandler),
     TFlacStreamEncoderSeekCallback(@TFlacStreamEncoder.ClassSeekHandler),
     TFlacStreamEncoderTellCallback(@TFlacStreamEncoder.ClassTellHandler),
     TFlacStreamEncoderMetadataCallback(@TFlacStreamEncoder.ClassMetadataHandler),
     Self
    );
end;

destructor TFlacStreamEncoder.Destroy;
begin
  FLAC__stream_encoder_delete(FEncoder);
  inherited Destroy;
end;

{ TFlacStreamDecoder }

function TFlacStreamDecoder.GetMd5Checking: Boolean;
begin
  Result := FLAC__stream_decoder_get_md5_checking(FDecoder);
end;

function TFlacStreamDecoder.GetResolvedStateString: String;
begin
  Result := FLAC__stream_decoder_get_resolved_state_string(FDecoder);
end;

function TFlacStreamDecoder.GetState: TFlacStreamDecoderState;
begin
  Result := FLAC__stream_decoder_get_state(FDecoder);
end;

procedure TFlacStreamDecoder.SetMd5Checking(AValue: Boolean);
begin
  FLAC__stream_decoder_set_md5_checking(FDecoder, AValue);
end;

class function TFlacStreamDecoder.ClassStreamRead(Decoder: Pointer; Buffer: PByte; bytes: pcsize_t; client_data: pointer): TFlacStreamDecoderReadStatus; cdecl;
var
  lDecoder: TFlacStreamDecoder absolute client_data;
begin
  try
    bytes^ := lDecoder.FStream.Read(Buffer^, bytes^);
    if bytes^ > 0 then
      Result := fsdrsContinue
    else
      Result := fsdrsEndOfStream;
  except
    Result := fsdrsReadAbort;
  end;
end;

class function TFlacStreamDecoder.ClassStreamSeek(Decoder: Pointer; abs_byte_offset: QWord; client_data: pointer): TFlacStreamDecoderSeekStatus; cdecl;
var
  lDecoder: TFlacStreamDecoder absolute client_data;
begin
  try
    lDecoder.FStream.Seek(abs_byte_offset, soBeginning);
    Result := fsdssOk;
  except
    on e: EStreamError do
      Result := fsdssUnsupported;
    else
      Result := fsdssError;
  end;
end;

class function TFlacStreamDecoder.ClassStreamTell(Decoder: Pointer; abs_byte_offset: PQWord; client_data: pointer): TFlacStreamDecoderTellStatus; cdecl;
var
  lDecoder: TFlacStreamDecoder absolute client_data;
begin
  try
    abs_byte_offset^:=lDecoder.FStream.Position;
    Result := fsdtsOK;
  except
    on e: EStreamError do
      Result := fsdtsUnsupported;
    else
      Result := fsdtsError;
  end;
end;

class function TFlacStreamDecoder.ClassStreamLength(Decoder: Pointer; stream_length: PQWord; client_data: pointer): TFlacStreamDecoderLengthStatus; cdecl;
var
  lDecoder: TFlacStreamDecoder absolute client_data;
begin
  try
    stream_length^:=lDecoder.FStream.Size;
    Result := fsdlsOk;
  except
    on e: EStreamError do
      Result := fsdlsUnsupported;
    else
      Result := fsdlsError;
  end;
end;

class function TFlacStreamDecoder.ClassStreamEOF(Decoder: Pointer; client_data: pointer): TFlacBool; cdecl;
var
  lDecoder: TFlacStreamDecoder absolute client_data;
begin
  Result := lDecoder.FStream.Position = lDecoder.FStream.Size;
end;

class function TFlacStreamDecoder.ClassStreamWrite(Decoder: Pointer; frame: Pointer; buffer: PPLongInt; client_data: pointer): TFlacStreamDecoderWriteStatus; cdecl;
var
  lDecoder: TFlacStreamDecoder absolute client_data;
  lFrame: PFlacFrame absolute Frame;
begin
  try
    if lDecoder.HandleWriteDecodedData(lFrame^.Header.Blocksize, lFrame^.Header.Channels, buffer) then
      Result := fsdwsContinue
    else
      Result := fsdwsAbort;
  except
    Result := fsdwsAbort;
  end;
end;

class procedure TFlacStreamDecoder.ClassStreamMetadata(Decoder: Pointer; metadata: Pointer; client_data: pointer); cdecl;
var
  lDecoder: TFlacStreamDecoder absolute client_data;
  lMetadata: PFlacStreamMetadataStruct absolute metadata;
  lType: TFlacMetadataType;
  lMetadataObject : TFlacStreamMetadata;
begin
  lMetadataObject := TFlacStreamMetadata.CreateInstance(lMetadata^.Type_, lMetadata);
  lDecoder.HandleMetadataObject(lMetadataObject);
  lMetadataObject.Free;
end;

class procedure TFlacStreamDecoder.ClassStreamError(Decoder: Pointer; status: TFlacStreamDecoderErrorStatus; client_data: pointer); cdecl;
var
  lDecoder: TFlacStreamDecoder absolute client_data;
begin
  lDecoder.HandleError(status);
end;

function TFlacStreamDecoder.HandleWriteDecodedData(ASamplesPerChannel: Integer; AChannels: Integer; ChannelsData: PPLongInt): Boolean;
begin
  Result := Assigned(FOnOutput) and FOnOutput(Self, ASamplesPerChannel, AChannels, ChannelsData);
end;

procedure TFlacStreamDecoder.HandleError(AError: TFlacStreamDecoderErrorStatus);
begin

end;

procedure TFlacStreamDecoder.HandleMetadataObject(AMetadataObject: TFlacStreamMetadata);
var
  StreamInfo: TFlacStreamMetadataStreamInfo absolute AMetadataObject;
begin
  if StreamInfo.GetType = fmtStreamInfo then
  begin
    FChannels:=StreamInfo.Channels;
    FSampleRate:=StreamInfo.Samplerate;
    FBitsperSample:=StreamInfo.BitsPerSample;
  end;
  if Assigned(FOnMetadata) then
    FOnMetadata(Self, AMetadataObject);
end;

constructor TFlacStreamDecoder.Create(AStream: TStream; AIsOgg: Boolean);
begin
  FDecoder := FLAC__stream_decoder_new;
  FStream := AStream;
  if not AIsOgg then
    FLAC__stream_decoder_init_stream(FDecoder,
       TFlacStreamDecoderReadCB(@TFlacStreamDecoder.ClassStreamRead),
       TFlacStreamDecoderSeekCB(@TFlacStreamDecoder.ClassStreamSeek),
       TFlacStreamDecoderTellCB(@TFlacStreamDecoder.ClassStreamTell),
       TFlacStreamDecoderLengthCB(@TFlacStreamDecoder.ClassStreamLength),
       TFlacStreamDecoderEofCB(@TFlacStreamDecoder.ClassStreamEOF),
       TFlacStreamDecoderWriteCB(@TFlacStreamDecoder.ClassStreamWrite),
       TFlacStreamDecoderMetadataCB(@TFlacStreamDecoder.ClassStreamMetadata),
       TFlacStreamDecoderErrorCB(@TFlacStreamDecoder.ClassStreamError),
       Pointer(Self)
       )
  else
    FLAC__stream_decoder_init_ogg_stream(FDecoder,
       TFlacStreamDecoderReadCB(@TFlacStreamDecoder.ClassStreamRead),
       TFlacStreamDecoderSeekCB(@TFlacStreamDecoder.ClassStreamSeek),
       TFlacStreamDecoderTellCB(@TFlacStreamDecoder.ClassStreamTell),
       TFlacStreamDecoderLengthCB(@TFlacStreamDecoder.ClassStreamLength),
       TFlacStreamDecoderEofCB(@TFlacStreamDecoder.ClassStreamEOF),
       TFlacStreamDecoderWriteCB(@TFlacStreamDecoder.ClassStreamWrite),
       TFlacStreamDecoderMetadataCB(@TFlacStreamDecoder.ClassStreamMetadata),
       TFlacStreamDecoderErrorCB(@TFlacStreamDecoder.ClassStreamError),
       Pointer(Self)
       );
end;

destructor TFlacStreamDecoder.Destroy;
begin
  inherited Destroy;
end;

function TFlacStreamDecoder.SetOggSerialNumber(ASerialNumber: LongWord): Boolean;
begin
  Result := FLAC__stream_decoder_set_ogg_serial_number(FDecoder, ASerialNumber);
end;

function TFlacStreamDecoder.SetMetadataRespond(AType: TFlacMetadataType): Boolean;
begin
  Result := FLAC__stream_decoder_set_metadata_respond(FDecoder, AType);
end;

function TFlacStreamDecoder.SetMetadataIgnore(AType: TFlacMetadataType): Boolean;
begin
  Result := FLAC__stream_decoder_set_metadata_ignore(FDecoder, AType);
end;

function TFlacStreamDecoder.SetMetadataIgnoreAll: Boolean;
begin
  Result := FLAC__stream_decoder_set_metadata_ignore_all(FDecoder);
end;

function TFlacStreamDecoder.TotalSamples: QWord;
begin
  Result := FLAC__stream_decoder_get_total_samples(FDecoder);
end;

function TFlacStreamDecoder.Channels: Integer;
begin
  Result := FChannels;//FLAC__stream_decoder_get_channels(FDecoder);
end;

function TFlacStreamDecoder.ChannelAssignment: TFlacChannelAssignment;
begin
  Result := FLAC__stream_decoder_get_channel_assignment(FDecoder);
end;

function TFlacStreamDecoder.BitsPerSample: Integer;
begin
  Result := FBitsperSample;//FLAC__stream_decoder_get_bits_per_sample(FDecoder);
end;

function TFlacStreamDecoder.SampleRate: Integer;
begin
  Result := FSampleRate;//FLAC__stream_decoder_get_sample_rate(FDecoder);
end;

function TFlacStreamDecoder.BlockSize: Integer;
begin
  Result := FLAC__stream_decoder_get_blocksize(FDecoder);
end;

function TFlacStreamDecoder.DecodePosition: QWord;
begin
  if not FLAC__stream_decoder_get_decode_position(FDecoder, @Result) then
    Result := 0;
end;

function TFlacStreamDecoder.Flush: Boolean;
begin
  Result := FLAC__stream_decoder_flush(FDecoder);
end;

function TFlacStreamDecoder.Reset: Boolean;
begin
  Result := FLAC__stream_decoder_reset(FDecoder);
end;

function TFlacStreamDecoder.ProcessSingle: Boolean;
begin
  Result := FLAC__stream_decoder_process_single(FDecoder);
end;

function TFlacStreamDecoder.ProcessUntilEndOfMetadata: Boolean;
begin
  Result := FLAC__stream_decoder_process_until_end_of_metadata(FDecoder);
end;

function TFlacStreamDecoder.ProcessUntilEndOfStream: Boolean;
begin
  Result := FLAC__stream_decoder_process_until_end_of_stream(FDecoder);
end;

function TFlacStreamDecoder.SkipSingleFrame: Boolean;
begin
  Result := FLAC__stream_decoder_skip_single_frame(FDecoder);
end;

function TFlacStreamDecoder.SeekAbsolute(ASample: QWord): Boolean;
begin
  Result := FLAC__stream_decoder_seek_absolute(FDecoder, ASample);
end;


end.

