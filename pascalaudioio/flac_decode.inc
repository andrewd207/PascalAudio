{%mainuint flac_classes.pas}

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
  TFlacStreamDecoderState = (
    fsdsSearchForMetadata = 0,
    fsdsReadMetadata,
    fsdsSearchForFrameSync,
    fsdsReadFrame,
    fsdsEndOfStream,
    fsdsOggError,
    fsdsSeekError,
    fsdsDecoderAborted,
    fsdsMemoryAllocationError,
    fsdsUninitialized
  );

  TFlacStreamDecoderInitStatus = (
   fsdisOk,
   fsdisUnsupportedContainer,
   fsdisInvalidCallbacks,
   fsdisMemoryAllocationError,
   fsdisErrorOpeningFile,
   fsdisAlreadyInitialized
  );

  TFlacStreamDecoderReadStatus = (
    fsdrsContinue = 0,
    fsdrsEndOfStream,
    fsdrsReadAbort
  );

  TFlacStreamDecoderSeekStatus = (
   fsdssOk,
   fsdssError,
   fsdssUnsupported
  );

  TFlacStreamDecoderTellStatus = (
    fsdtsOK,
    fsdtsError,
    fsdtsUnsupported
  );

  TFlacStreamDecoderLengthStatus = (
    fsdlsOk,
    fsdlsError,
    fsdlsUnsupported
  );

  TFlacStreamDecoderWriteStatus = (
    fsdwsContinue,
    fsdwsAbort
  );

  TFlacStreamDecoderErrorStatus = (
    fsdesLostSync,
    fsdesBadHeader,
    fsdesFrameCrcMismatch,
    fsdesUnparseableStream
  );
{$ENDIF}

{$IFDEF FLAC_INTF}
type
  PFlacStreamDecoderStruct = ^TFlacStreamDecoderStruct;
  TFlacStreamDecoderStruct = record
    prot: pointer;
    priv: pointer;
  end;

  TFlacStreamDecoderReadCB = function (Decoder: PFlacStreamDecoderStruct; Buffer: PByte; bytes: pcsize_t; client_data: pointer): TFlacStreamDecoderReadStatus; cdecl;
  TFlacStreamDecoderSeekCB = function (Decoder: PFlacStreamDecoderStruct; abs_byte_offset: QWord; client_data: pointer): TFlacStreamDecoderSeekStatus; cdecl;
  TFlacStreamDecoderTellCB = function (Decoder: PFlacStreamDecoderStruct; abs_byte_offset: PQWord; client_data: pointer): TFlacStreamDecoderTellStatus; cdecl;
  TFlacStreamDecoderLengthCB = function (Decoder: PFlacStreamDecoderStruct; stream_length: PQWord; client_data: pointer): TFlacStreamDecoderLengthStatus; cdecl;
  TFlacStreamDecoderEofCB = function (Decoder: PFlacStreamDecoderStruct; client_data: pointer): TFlacBool; cdecl;
  TFlacStreamDecoderWriteCB = function (Decoder: PFlacStreamDecoderStruct; frame: PFlacFrame; buffer: PPLongInt; client_data: pointer): TFlacStreamDecoderWriteStatus; cdecl;
  TFlacStreamDecoderMetadataCB = procedure (Decoder: PFlacStreamDecoderStruct; metadata: PFlacStreamMetadataStruct; client_data: pointer); cdecl;
  TFlacStreamDecoderErrorCB = procedure (Decoder: PFlacStreamDecoderStruct; status: TFlacStreamDecoderErrorStatus; client_data: pointer); cdecl;

function  FLAC__stream_decoder_new: PFlacStreamDecoderStruct; cdecl; external;
procedure FLAC__stream_decoder_delete(decoder: PFlacStreamDecoderStruct); cdecl; external;
function  FLAC__stream_decoder_set_ogg_serial_number(decoder: PFlacStreamDecoderStruct; serial_number: clong): TFlacBool; cdecl; external;
function  FLAC__stream_decoder_set_md5_checking(decoder: PFlacStreamDecoderStruct; AValue: TFlacBool): TFlacBool; cdecl; external;
function  FLAC__stream_decoder_set_metadata_respond(decoder: PFlacStreamDecoderStruct; AType: TFlacMetadataType): TFlacBool; cdecl; external;
//function  FLAC__stream_decoder_set_metadata_respond_application(decoder: PFlacStreamDecoderStruct; id: array[0..3] of Byte): TFlacBool; cdecl; external;
function  FLAC__stream_decoder_set_metadata_ignore(decoder: PFlacStreamDecoderStruct; AType: TFlacMetadataType): TFlacBool; cdecl; external;
//function  FLAC__stream_decoder_set_metadata_ignore_application(decoder: PFlacStreamDecoderStruct; id: array[0..3] of Byte): TFlacBool; cdecl; external;
function  FLAC__stream_decoder_set_metadata_ignore_all(decoder: PFlacStreamDecoderStruct): TFlacBool; cdecl; external;
function  FLAC__stream_decoder_get_state(decoder: PFlacStreamDecoderStruct): TFlacStreamDecoderState; cdecl; external;
function  FLAC__stream_decoder_get_resolved_state_string(decoder: PFlacStreamDecoderStruct): PChar; cdecl; external;
function  FLAC__stream_decoder_get_md5_checking(decoder: PFlacStreamDecoderStruct): TFlacBool; cdecl; external;
function  FLAC__stream_decoder_get_total_samples(decoder: PFlacStreamDecoderStruct): QWord; cdecl; external;
function  FLAC__stream_decoder_get_channels(decoder: PFlacStreamDecoderStruct): cunsigned; cdecl; external;
function  FLAC__stream_decoder_get_channel_assignment(decoder: PFlacStreamDecoderStruct): TFlacChannelAssignment; cdecl; external;
function  FLAC__stream_decoder_get_bits_per_sample(decoder: PFlacStreamDecoderStruct): cunsigned; cdecl; external;
function  FLAC__stream_decoder_get_sample_rate(decoder: PFlacStreamDecoderStruct): cunsigned; cdecl; external;
function  FLAC__stream_decoder_get_blocksize(decoder: PFlacStreamDecoderStruct): cunsigned; cdecl; external;
function  FLAC__stream_decoder_get_decode_position(decoder: PFlacStreamDecoderStruct; position: PQword): TFlacBool; cdecl; external;
function  FLAC__stream_decoder_init_stream(
             decoder: PFlacStreamDecoderStruct;
             read_cb: TFlacStreamDecoderReadCB;
             seek_cb: TFlacStreamDecoderSeekCB;
             tell_cb: TFlacStreamDecoderTellCB;
             length_cb: TFlacStreamDecoderLengthCB;
             eof_cb: TFlacStreamDecoderEofCB;
             write_cb: TFlacStreamDecoderWriteCB;
             metadata_cb: TFlacStreamDecoderMetadataCB;
             error_cb: TFlacStreamDecoderErrorCB;
             userdata: pointer
          ): TFlacStreamDecoderInitStatus; cdecl; external;

function  FLAC__stream_decoder_init_ogg_stream(decoder: PFlacStreamDecoderStruct;
             read_cb: TFlacStreamDecoderReadCB;
             seek_cb: TFlacStreamDecoderSeekCB;
             tell_cb: TFlacStreamDecoderTellCB;
             length_cb: TFlacStreamDecoderLengthCB;
             eof_cb: TFlacStreamDecoderEofCB;
             write_cb: TFlacStreamDecoderWriteCB;
             metadata_cb: TFlacStreamDecoderMetadataCB;
             error_cb: TFlacStreamDecoderErrorCB;
             userdata: pointer
         ): TFlacStreamDecoderInitStatus; cdecl; external;
// skipped a bunch of variants of decoder_init_X. for FILE and filename

function  FLAC__stream_decoder_flush(decoder: PFlacStreamDecoderStruct): TFlacBool; cdecl; external;
function  FLAC__stream_decoder_reset(decoder: PFlacStreamDecoderStruct): TFlacBool; cdecl; external;
function  FLAC__stream_decoder_process_single(decoder: PFlacStreamDecoderStruct): TFlacBool; cdecl; external;
function  FLAC__stream_decoder_process_until_end_of_metadata(decoder: PFlacStreamDecoderStruct): TFlacBool; cdecl; external;
function  FLAC__stream_decoder_process_until_end_of_stream(decoder: PFlacStreamDecoderStruct): TFlacBool; cdecl; external;
function  FLAC__stream_decoder_skip_single_frame(decoder: PFlacStreamDecoderStruct): TFlacBool; cdecl; external;
function  FLAC__stream_decoder_seek_absolute(decoder: PFlacStreamDecoderStruct; sample: QWord): TFlacBool; cdecl external;




{$ENDIF}

{$IFDEF FLAC_IMPL}
{$ENDIF}
