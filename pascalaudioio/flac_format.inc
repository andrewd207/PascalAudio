{%mainunit flac.pas}

{* libFLAC - Free Lossless Audio Codec library
 * Copyright (C) 2000-2009  Josh Coalson
 * Copyright (C) 2011-2013  Xiph.Org Foundation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * - Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of the Xiph.org Foundation nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *}
{$IFDEF FLAC_INTF_TYPES}
type
  TFlacBool = LongBool;

const
  FLAC__MAX_METADATA_TYPE_CODE              = 126;
  FLAC__MIN_BLOCK_SIZE                      = 16;
  FLAC__MAX_BLOCK_SIZE                      = 65535;
  FLAC__SUBSET_MAX_BLOCK_SIZE_48000HZ       = 4608;
  FLAC__MAX_CHANNELS                        = 8;
  FLAC__MIN_BITS_PER_SAMPLE                 = 4;
  FLAC__MAX_BITS_PER_SAMPLE                 = 32;
  FLAC__REFERENCE_CODEC_MAX_BITS_PER_SAMPLE = 24;
  FLAC__MAX_SAMPLE_RATE                     = 655350;
  FLAC__MAX_LPC_ORDER                       = 32;
  FLAC__SUBSET_MAX_LPC_ORDER_48000HZ        = 12;
  FLAC__MIN_QLP_COEFF_PRECISION             = 5;
  FLAC__MAX_QLP_COEFF_PRECISION             = 15;
  FLAC__MAX_FIXED_ORDER                     = 4;
  FLAC__MAX_RICE_PARTITION_ORDER            = 15;
  FLAC__SUBSET_MAX_RICE_PARTITION_ORDER     = 8;

  FLAC__STREAM_SYNC_LENGTH                  = 4;

type
  TFlacEntropyCodingMethodType = (
    fecmPartitionedRice = 0,
    fecmPartitionedRice2 = 1
  );

  TFlacSubframeType = (
    sftConstant = 0,
    sftVerbatim = 1,
    sftFixed = 2,
    sftLPC = 3
  );

   TFlacFrameNumberType = (
    ffntFrameNumber,
    ffntSampleNumber
  );

 TFlacChannelAssignment = (
   fcaIndependant = 0,
   fcaLeftSide    = 1,
   fcaRightSide   = 2,
   fcaMidSide     = 3
  );

 TFlacMetadataType = (
    fmtStreamInfo = 0,
    fmtPadding = 1,
    fmtApplication = 2,
    fmtSeekTable = 3,
    fmtVorbisComment = 4,
    fmtCueSheet = 5,
    fmtPicture = 6,
    fmtUnknown = 7
  );
 TFlacStreamMetadataPictureType = (
    fsmpOther = 0,
    fsmpFileIconStandard = 1,
    fsmpFileIcon = 2,
    fsmpFrontCover = 3,
    fsmpBackCover = 4,
    fsmpLeafletPage = 5,
    fsmpMedia = 6,
    fsmpLeadArtist = 7,
    fsmpArtist = 8,
    fsmpConductor = 9,
    fsmpBand = 10,
    fsmpComposer = 11,
    fsmpLyricist = 12,
    fsmpRecordingLocation = 13,
    fsmpDuringRecording = 14,
    fsmpDuringPerformance = 15,
    fsmpVideoScreenCapture = 16,
    fsmpFish = 17,
    fsmpIllustration = 18,
    fsmpBandLogotype = 19,
    fsmpPublisherLogotype = 20,
    fsmpUndefined
  );
{$ENDIF}

{$IFDEF FLAC_INTF}

var
  FLAC__VERSION_STRING: PChar; cvar; external;
  FLAC__VENDOR_STRING: PChar; cvar; external;
  FLAC__STREAM_SYNC_STRING: array[0..3] of char; cvar; external;
  FLAC__STREAM_SYNC: cunsigned; cvar; external;
  FLAC__STREAM_SYNC_LEN: cunsigned; cvar; external;
type


  PFlacEntropyCodingMethodPartitionedRiceContentsStruct = ^TFlacEntropyCodingMethodPartitionedRiceContentsStruct;
  TFlacEntropyCodingMethodPartitionedRiceContentsStruct = record
    Parameters: pcunsigned;
    RawBits: pcunsigned;
    CapacityByOrder: cunsigned;
  end;

  PFlacEntropyCodingMethodPartitionedRiceStruct = ^TFlacEntropyCodingMethodPartitionedRiceStruct;
  TFlacEntropyCodingMethodPartitionedRiceStruct = record
    Order: cunsigned;
    Contents: PFlacEntropyCodingMethodPartitionedRiceContentsStruct;
  end;

var
  FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_ORDER_LEN: cunsigned; cvar; external;
  FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_PARAMETER_LEN: cunsigned; cvar; external;
  FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE2_PARAMETER_LEN: cunsigned; cvar; external;
  FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_RAW_LEN: cunsigned; cvar; external;
  FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_ESCAPE_PARAMETER: cunsigned; cvar; external;
  FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE2_ESCAPE_PARAMETER: cunsigned; cvar; external;

type
  PFlacEntropyCodingMethodStruct = ^TFlacEntropyCodingMethodStruct;
  TFlacEntropyCodingMethodStruct = record
    Type_: TFlacEntropyCodingMethodType;
    PartitionedRice: TFlacEntropyCodingMethodPartitionedRiceStruct;
  end;



var
  FLAC__SubframeTypeString: PChar; cvar; external;

type
  TFlacSubframeConstant = record
    value: LongInt;
  end;

  TFlacSubframeVerbatim = record
    data: PLongInt;
  end;

  TFlacSubframeFixed = record
    Order: cunsigned;
    Warmup: array[0..FLAC__MAX_FIXED_ORDER-1] of LongInt;
    Residual: PLongInt;
  end;

  TFlacSubframeLPC = record
    Order: cunsigned;
    qlp_coeff_precision: cunsigned;
    QuantizationLevel: cint;
    qlp_coeff: array[0..FLAC__MAX_LPC_ORDER-1] of cint32;
    Warmup: array[0..FLAC__MAX_LPC_ORDER-1] of cint32;
    Residual: PLongInt;
  end;
var
  FLAC__SUBFRAME_LPC_QLP_COEFF_PRECISION_LEN: cunsigned; cvar; external;
  FLAC__SUBFRAME_LPC_QLP_SHIFT_LEN: cunsigned; cvar; external;

type
  TFlacSubframe = record
    Type_: TFlacSubframeType;
    data: record
      case TFlacSubframeType of
        sftConstant: (constant: TFlacSubframeConstant;);
        sftFixed   : (fixed: TFlacSubframeFixed;);
        sftLPC     : (lpc: TFlacSubframeLPC;);
        sftVerbatim: (verbatim: TFlacSubframeVerbatim;);
      end;
    WastedBits: cunsigned;
  end;

var
  FLAC__SUBFRAME_ZERO_PAD_LEN: cunsigned; cvar; external;
  FLAC__SUBFRAME_TYPE_LEN: cunsigned; cvar; external;
  FLAC__SUBFRAME_WASTED_BITS_FLAG_LEN: cunsigned; cvar; external;
  FLAC__SUBFRAME_TYPE_CONSTANT_BYTE_ALIGNED_MASK: cunsigned; cvar; external;
  FLAC__SUBFRAME_TYPE_VERBATIM_BYTE_ALIGNED_MASK: cunsigned; cvar; external;
  FLAC__SUBFRAME_TYPE_FIXED_BYTE_ALIGNED_MASK: cunsigned; cvar; external;
  FLAC__SUBFRAME_TYPE_LPC_BYTE_ALIGNED_MASK: cunsigned; cvar; external;

var
  FLAC__FrameNumberTypeString: PChar; cvar; external;

type


  TFlacFrameHeader = record
    Blocksize: cunsigned;
    SampleRate: cunsigned;
    Channels: cunsigned;
    ChannelAssignment: TFlacChannelAssignment;
    BitsPerSample: cunsigned;
    NumberType: TFlacFrameNumberType;
    number: record
      case TFlacFrameNumberType of
        ffntFrameNumber : (FrameNumber: LongWord);
        ffntSampleNumber: (SampleNumber: QWord);
      end;
    CRC: Byte;
  end;
var
  FLAC__FRAME_HEADER_SYNC: cunsigned; cvar; external;
  FLAC__FRAME_HEADER_SYNC_LEN: cunsigned; cvar; external;
  FLAC__FRAME_HEADER_RESERVED_LEN: cunsigned; cvar; external;
  FLAC__FRAME_HEADER_BLOCKING_STRATEGY_LEN: cunsigned; cvar; external;
  FLAC__FRAME_HEADER_BLOCK_SIZE_LEN: cunsigned; cvar; external;
  FLAC__FRAME_HEADER_SAMPLE_RATE_LEN: cunsigned; cvar; external;
  FLAC__FRAME_HEADER_CHANNEL_ASSIGNMENT_LEN: cunsigned; cvar; external;
  FLAC__FRAME_HEADER_BITS_PER_SAMPLE_LEN: cunsigned; cvar; external;
  FLAC__FRAME_HEADER_ZERO_PAD_LEN: cunsigned; cvar; external;
  FLAC__FRAME_HEADER_CRC_LEN: cunsigned; cvar; external;
type
  TFlacFrameFooter = record
    CRC: Word;
  end;
var
  FLAC__FRAME_FOOTER_CRC_LEN: cunsigned; cvar; external;
type
  PFlacFrame = ^TFlacFrame;
  TFlacFrame = record
    Header: TFlacFrameHeader;
    Subframes: array[0..FLAC__MAX_CHANNELS-1] of TFlacSubframe;
    Footer: TFlacFrameFooter;
  end;



var
  FLAC__MetadataTypeString: PChar; cvar; external;

type
  TFlacStreamMetadataStreamInfoStruct = record
    MinBlocksize,
    MaxBlocksize: cunsigned;
    MinFramesize,
    MaxFramesize: cunsigned;
    Samplerate: cunsigned;
    Channels: cunsigned;
    BitsPerSample: cunsigned;
    TotalSamples: QWord;
    MD5Sum: array[0..15] of byte;
  end;
var
  FLAC__STREAM_METADATA_STREAMINFO_MIN_BLOCK_SIZE_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_STREAMINFO_MAX_BLOCK_SIZE_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_STREAMINFO_MIN_FRAME_SIZE_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_STREAMINFO_MAX_FRAME_SIZE_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_STREAMINFO_SAMPLE_RATE_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_STREAMINFO_CHANNELS_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_STREAMINFO_BITS_PER_SAMPLE_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_STREAMINFO_TOTAL_SAMPLES_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_STREAMINFO_MD5SUM_LEN: cunsigned; cvar; external;
const
  FLAC__STREAM_METADATA_STREAMINFO_LENGTH = 34;
type
  TFlacStreamMetadataPaddingStruct = record
    dummy: cint;
  end;

  TFlacStreamMetadataApplicationStruct = record
    Id: array[0..3] of byte;
    Data: PByte;
  end;

  PFlacStreamMetadataSeekPoint = ^TFlacStreamMetadataSeekPoint;
  TFlacStreamMetadataSeekPoint = record
    SampleNumber: QWord;
    StreamOffset: QWord;
    FrameSamples: cunsigned;
  end;
var
  FLAC__STREAM_METADATA_SEEKPOINT_SAMPLE_NUMBER_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_SEEKPOINT_STREAM_OFFSET_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_SEEKPOINT_FRAME_SAMPLES_LEN: cunsigned; cvar; external;
const
  FLAC__STREAM_METADATA_SEEKPOINT_LENGTH = 18;
var
  FLAC__STREAM_METADATA_SEEKPOINT_PLACEHOLDER: QWord; cvar; external;
type
  PFlacStreamMetadataSeekTableStruct = ^TFlacStreamMetadataSeekTableStruct;
  TFlacStreamMetadataSeekTableStruct = record
    Points: PFlacStreamMetadataSeekPoint;
  end;

  PFlacStreamMetadataVorbisCommentEntryStruct = ^TFlacStreamMetadataVorbisCommentEntryStruct;
  TFlacStreamMetadataVorbisCommentEntryStruct = record
    Length: LongWord;
    Entry: PByte;
  end;
var
  FLAC__STREAM_METADATA_VORBIS_COMMENT_ENTRY_LENGTH_LEN: cunsigned; cvar; external;
type
  TFlacStreamMetadataVorbisCommentStruct = record
    VendorString: TFlacStreamMetadataVorbisCommentEntryStruct;
    NumComments: LongWord;
    Comments: PFlacStreamMetadataVorbisCommentEntryStruct;
  end;
var
  FLAC__STREAM_METADATA_VORBIS_COMMENT_NUM_COMMENTS_LEN: cunsigned; cvar; external;
type
  PFlacStreamMetadataCueSheetIndex = ^TFlacStreamMetadataCueSheetIndex;
  TFlacStreamMetadataCueSheetIndex = record
    Offset: QWord;
    Number: Byte;
  end;
var
  FLAC__STREAM_METADATA_CUESHEET_INDEX_OFFSET_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_CUESHEET_INDEX_NUMBER_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_CUESHEET_INDEX_RESERVED_LEN: cunsigned; cvar; external;

type
  TFlacBit = 0..1;
  PFlacStreamMetadataCueSheetTrack = ^TFlacStreamMetadataCueSheetTrack;
  TFlacStreamMetadataCueSheetTrack = record
    Offset: QWord;
    Number: Byte;
    ISRC: array[0..12] of byte;
    Type_: TFlacBit;
    PreEmphasis: TFlacBit;
    NumIndices: Byte;
    Indices: PFlacStreamMetadataCueSheetIndex;
  end;
var
  FLAC__STREAM_METADATA_CUESHEET_TRACK_OFFSET_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_CUESHEET_TRACK_NUMBER_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_CUESHEET_TRACK_ISRC_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_CUESHEET_TRACK_TYPE_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_CUESHEET_TRACK_PRE_EMPHASIS_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_CUESHEET_TRACK_RESERVED_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_CUESHEET_TRACK_NUM_INDICES_LEN: cunsigned; cvar; external;
type
  PFlacStreamMetadataCueSheetStruct = ^TFlacStreamMetadataCueSheet;
  TFlacStreamMetadataCueSheetStruct = record
    MediaCatalogNumber: array[0..128] of Byte;
    LeadIn: QWord;
    IsCD: TFlacBool;
    NumTracks: cunsigned;
    Tracks: PFlacStreamMetadataCueSheetTrack;
  end;
var
  FLAC__STREAM_METADATA_CUESHEET_MEDIA_CATALOG_NUMBER_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_CUESHEET_LEAD_IN_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_CUESHEET_IS_CD_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_CUESHEET_RESERVED_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_CUESHEET_NUM_TRACKS_LEN: cunsigned; cvar; external;

var
  FLAC__StreamMetadata_Picture_TypeString: PChar; cvar; external;
type
  PFlacStreamMetadataPictureStruct = ^TFlacStreamMetadataPicture;
  TFlacStreamMetadataPictureStruct = record
    Type_: TFlacStreamMetadataPictureType;
    MimeType: PChar;
    Description: PByte;
    Width: LongWord;
    Height: LongWord;
    Depth: LongWord;
    Colors: LongWord;
    DataLength: LongWord;
    Data: PByte;
  end;
var
  FLAC__STREAM_METADATA_PICTURE_TYPE_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_PICTURE_MIME_TYPE_LENGTH_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_PICTURE_DESCRIPTION_LENGTH_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_PICTURE_WIDTH_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_PICTURE_HEIGHT_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_PICTURE_DEPTH_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_PICTURE_COLORS_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_PICTURE_DATA_LENGTH_LEN: cunsigned; cvar; external;
type
  TFlacStreamMetadataUnknown = record
    Data: PByte;
  end;
  PPFlacStreamMetadataStruct = ^PFlacStreamMetadataStruct;
  PFlacStreamMetadataStruct = ^TFlacStreamMetadataStruct;
  TFlacStreamMetadataStruct = record
    Type_: TFlacMetadataType;
    IsLast: TFlacBool;
    Length: cunsigned;
    Data: record
      case TFlacMetadataType of
       fmtStreamInfo: (StreamInfo: TFlacStreamMetadataStreamInfoStruct);
       fmtPadding: (PAdding: TFlacStreamMetadataPadding);
       fmtApplication: (Application: TFlacStreamMetadataApplicationStruct);
       fmtSeekTable: (SeekTable: TFlacStreamMetadataSeekTableStruct);
       fmtVorbisComment: (VorbisComment: TFlacStreamMetadataVorbisCommentStruct);
       fmtCueSheet: (CueSheet: TFlacStreamMetadataCueSheetStruct);
       fmtPicture: (Picture: TFlacStreamMetadataPictureStruct);
       fmtUnknown: (Unknown: TFlacStreamMetadataUnknown);
     end;
  end;
var
  FLAC__STREAM_METADATA_IS_LAST_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_TYPE_LEN: cunsigned; cvar; external;
  FLAC__STREAM_METADATA_LENGTH_LEN: cunsigned; cvar; external;
const
  FLAC__STREAM_METADATA_HEADER_LENGTH = 4;

function FLAC__format_sample_rate_is_valid(sample_rate: cunsigned): TFlacBool; cdecl; external;
function FLAC__format_blocksize_is_subset(blocksize, samplerate: cunsigned): TFlacBool; cdecl; external;
function FLAC__format_sample_rate_is_subset(sample_rate: cunsigned): TFlacBool; cdecl; external;
function FLAC__format_vorbiscomment_entry_name_is_legal(name: PChar): TFlacBool; cdecl; external;
function FLAC__format_vorbiscomment_entry_value_is_legal(Value: PByte; length: cunsigned): TFlacBool; cdecl; external;
function FLAC__format_vorbiscomment_entry_is_legal(entry: PByte; length: cunsigned): TFlacBool; cdecl; external;
function FLAC__format_seektable_is_legal(seek_table: PFlacStreamMetadataSeekTableStruct): TFlacBool; cdecl; external;
function FLAC__format_seektable_sort(seek_table: PFlacStreamMetadataSeekTableStruct): cunsigned; cdecl; external;
function FLAC__format_cuesheet_is_legal(cue_sheet: PFlacStreamMetadataCueSheetStruct; check_cd_da_subset: TFlacBool; violation: PPChar): TFlacBool; cdecl; external;
function FLAC__format_picture_is_legal(picture: PFlacStreamMetadataPictureStruct; violation: PPChar): TFlacBool; cdecl; external;


{$ENDIF}
