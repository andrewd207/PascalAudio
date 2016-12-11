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
  TFlacStreamEncoderState = (
    sesOK = 0,
    sesUninitialized,
    sesOggError,
    sesVerifyDecoderError,
    sesVerifyMismatchInAudioData,
    sesClientError,
    sesIOError,
    sesFramingError,
    sesMemoryAllocationError
    );

  TFlacStreamEncoderInitStatus = (
    seisOK = 0,
    seisEncoderError,
    seisUnsupportedContainer,
    seisInvalidCallbacks,
    seisInvalidNumberOfChannels,
    seisInvalidBitsPerSample,
    seisInvalidSampleRate,
    seisInvalidBlockSize,
    seisInvalidMaxLPC_Order,
    seisInvalidQLP_CEFF_Precision,
    seisBlockSizeTooSmallForLPC_Order,
    seisNotStreamable,
    seisInvalidMetadata,
    seisAlreadyInitialized
  );

  TFlacStreamEncoderReadStatus = (
    sersContinue,
    sersEndOfStream,
    sersAbort,
    sersUnsupported
  );

  TFlacStreamEncoderWriteStatus = (
    sewsOk = 0,
    sewsFatalError
  );

  TFlacStreamEncoderSeekStatus = (
    sessOk = 0,
    sessError,
    sessUnsupported
  );

  TFlacStreamEncoderTellStatus = (
    setsOk,
    setsError,
    setsUnsupported
  );
{$ENDIF}


{$IFDEF FLAC_INTF}

type

{PFlacStreamEncoderProtected = ^TFlacStreamEncoderProtected;
  TFlacStreamEncoderProtected = record
  end;
  PFlacStreamEncoderPrivate = ^TFlacStreamEncoderPrivate;
  TFlacStreamEncoderPrivate = record
  end;}

  PFlacStreamEncoderStruct = Pointer;
  {TFlacStreamEncoderStruct = record
    protected_: PFlacStreamEncoderProtected;
    private_: PFlacStreamEncoderPrivate;
  end;}

  //PPFlacStreamMetadataStruct = ^PFlacStreamMetadataStruct;
  //PFlacStreamMetadataStruct = pointer;

  TFlacStreamEncoderReadCallback = function(Encoder: PFlacStreamEncoderStruct; Buffer: PByte; bytes: csize_t; userdata: Pointer):TFlacStreamEncoderReadStatus; cdecl;
  TFlacStreamEncoderWriteCallback = function(Encoder: PFlacStreamEncoderStruct; Buffer: PByte; bytes: csize_t; samples, current_frame: cunsigned; userdata: Pointer):TFlacStreamEncoderWriteStatus; cdecl;
  TFlacStreamEncoderSeekCallback = function(Encoder: PFlacStreamEncoderStruct; absolute_byte_offset: QWord; userdata: Pointer):TFlacStreamEncoderSeekStatus; cdecl;
  TFlacStreamEncoderTellCallback = function(Encoder: PFlacStreamEncoderStruct; absolute_byte_offset: PQWord; userdata: Pointer):TFlacStreamEncoderTellStatus; cdecl;
  TFlacStreamEncoderMetadataCallback = procedure(Encoder: PFlacStreamEncoderStruct; Metadata: PFlacStreamMetadataStruct; userdata: Pointer); cdecl;
  TFlacStreamEncoderProgressCallback = procedure(Encoder: PFlacStreamEncoderStruct; bytes_written, samples_Written: QWord; frames_written, total_frames_estimate: cunsigned; userdata: Pointer); cdecl;

function  FLAC__stream_encoder_new: PFlacStreamEncoderStruct; cdecl; external;
procedure FLAC__stream_encoder_delete(Encoder: PFlacStreamEncoderStruct); cdecl; external;
function  FLAC__stream_encoder_set_ogg_serial_number(Encoder: PFlacStreamEncoderStruct; SerialNumber: LongWord): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_verify(Encoder: PFlacStreamEncoderStruct; AValue: TFlacBool): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_streamable_subset(Encoder: PFlacStreamEncoderStruct; AValue: TFlacBool): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_channels(Encoder: PFlacStreamEncoderStruct; AValue: cunsigned): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_bits_per_sample(Encoder: PFlacStreamEncoderStruct; AValue: cunsigned): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_sample_rate(Encoder: PFlacStreamEncoderStruct; AValue: cunsigned): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_compression_level(Encoder: PFlacStreamEncoderStruct; AValue: cunsigned): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_blocksize(Encoder: PFlacStreamEncoderStruct; AValue: cunsigned): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_do_mid_side_stereo(Encoder: PFlacStreamEncoderStruct; AValue: TFlacBool): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_loose_mid_side_stereo(Encoder: PFlacStreamEncoderStruct; AValue: TFlacBool): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_apodization(Encoder: PFlacStreamEncoderStruct; ASpecification: PChar): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_max_lpc_order(Encoder: PFlacStreamEncoderStruct; AValue: cunsigned): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_qlp_coeff_precision(Encoder: PFlacStreamEncoderStruct; AValue: cunsigned): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_do_qlp_coeff_prec_search(Encoder: PFlacStreamEncoderStruct; AValue: TFlacBool): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_do_escape_coding(Encoder: PFlacStreamEncoderStruct; AValue: TFlacBool): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_do_exhaustive_model_search(Encoder: PFlacStreamEncoderStruct; AValue: TFlacBool): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_min_residual_partition_order(Encoder: PFlacStreamEncoderStruct; AValue: cunsigned): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_max_residual_partition_order(Encoder: PFlacStreamEncoderStruct; AValue: cunsigned): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_rice_parameter_search_dist(Encoder: PFlacStreamEncoderStruct; AValue: cunsigned): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_total_samples_estimate(Encoder: PFlacStreamEncoderStruct; AValue: QWord): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_set_metadata(Encoder: PFlacStreamEncoderStruct; Metadata: PPFlacStreamMetadataStruct; ANumBlocks: cunsigned): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_get_state(Encoder: PFlacStreamEncoderStruct): TFlacStreamEncoderState; cdecl; external;
//function  FLAC__stream_encoder_get_verify_decoder_state(Encoder: PFlacStreamEncoderStruct): TFlacStreamDecoderState; cdecl; external;
function  FLAC__stream_encoder_get_resolved_state_string(Encoder: PFlacStreamEncoderStruct): PChar; cdecl; external;
procedure FLAC__stream_encoder_get_verify_decoder_error_stats(Encoder: PFlacStreamEncoderStruct; ABsoluteSample: PQWord; FrameNumber, Channel, Sample: pcunsigned; Expected, Got: PLongInt); cdecl; external;
function  FLAC__stream_encoder_get_verify(Encoder: PFlacStreamEncoderStruct): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_get_streamable_subset(Encoder: PFlacStreamEncoderStruct): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_get_channels(Encoder: PFlacStreamEncoderStruct): cunsigned; cdecl; external;
function  FLAC__stream_encoder_get_bits_per_sample(Encoder: PFlacStreamEncoderStruct): cunsigned; cdecl; external;
function  FLAC__stream_encoder_get_sample_rate(Encoder: PFlacStreamEncoderStruct): cunsigned; cdecl; external;
function  FLAC__stream_encoder_get_blocksize(Encoder: PFlacStreamEncoderStruct): cunsigned; cdecl; external;
function  FLAC__stream_encoder_get_do_mid_side_stereo(Encoder: PFlacStreamEncoderStruct): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_get_loose_mid_side_stereo(Encoder: PFlacStreamEncoderStruct): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_get_max_lpc_order(Encoder: PFlacStreamEncoderStruct): cunsigned; cdecl; external;
function  FLAC__stream_encoder_get_qlp_coeff_precision(Encoder: PFlacStreamEncoderStruct): cunsigned; cdecl; external;
function  FLAC__stream_encoder_get_do_qlp_coeff_prec_search(Encoder: PFlacStreamEncoderStruct): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_get_do_escape_coding(Encoder: PFlacStreamEncoderStruct): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_get_do_exhaustive_model_search(Encoder: PFlacStreamEncoderStruct): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_get_min_residual_partition_order(Encoder: PFlacStreamEncoderStruct): cunsigned; cdecl; external;
function  FLAC__stream_encoder_get_max_residual_partition_order(Encoder: PFlacStreamEncoderStruct): cunsigned; cdecl; external;
function  FLAC__stream_encoder_get_rice_parameter_search_dist(Encoder: PFlacStreamEncoderStruct): cunsigned; cdecl; external;
function  FLAC__stream_encoder_get_total_samples_estimate(Encoder: PFlacStreamEncoderStruct): QWord; cdecl; external;
function  FLAC__stream_encoder_init_stream(
               Encoder: PFlacStreamEncoderStruct;
               WriteCB: TFlacStreamEncoderWriteCallback;
               SeekCB: TFlacStreamEncoderSeekCallback;
               TellCB: TFlacStreamEncoderTellCallback;
               MetadataCB: TFlacStreamEncoderMetadataCallback;
               UserData: Pointer
               ): TFlacStreamEncoderInitStatus; cdecl; external;

function  FLAC__stream_encoder_init_ogg_stream(
               Encoder: PFlacStreamEncoderStruct;
               ReadCB: TFlacStreamEncoderReadCallback;
               WriteCB: TFlacStreamEncoderWriteCallback;
               SeekCB: TFlacStreamEncoderSeekCallback;
               TellCB: TFlacStreamEncoderTellCallback;
               MetadataCB: TFlacStreamEncoderMetadataCallback;
               UserData: Pointer
               ): TFlacStreamEncoderInitStatus; cdecl; external;
function  FLAC__stream_encoder_init_FILE(
               Encoder: PFlacStreamEncoderStruct;
               file_: pcint;
               ProgressCB: TFlacStreamEncoderProgressCallback;
               UserData: Pointer
               ): TFlacStreamEncoderInitStatus; cdecl; external;

function  FLAC__stream_encoder_init_ogg_FILE(
               Encoder: PFlacStreamEncoderStruct;
               file_: pcint;
               ProgressCB: TFlacStreamEncoderProgressCallback;
               UserData: Pointer
               ): TFlacStreamEncoderInitStatus; cdecl; external;

function  FLAC__stream_encoder_init_file(
               Encoder: PFlacStreamEncoderStruct;
               filename: PChar;
               ProgressCB: TFlacStreamEncoderProgressCallback;
               UserData: Pointer
               ): TFlacStreamEncoderInitStatus; cdecl; external;

function  FLAC__stream_encoder_init_ogg_file(
               Encoder: PFlacStreamEncoderStruct;
               filename: PChar;
               ProgressCB: TFlacStreamEncoderProgressCallback;
               UserData: Pointer
               ): TFlacStreamEncoderInitStatus; cdecl; external;

function  FLAC__stream_encoder_finish(Encoder: PFlacStreamEncoderStruct): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_process(Encoder: PFlacStreamEncoderStruct; Buffer: PLongint; Samples: cunsigned): TFlacBool; cdecl; external;
function  FLAC__stream_encoder_process_interleaved(Encoder: PFlacStreamEncoderStruct; Buffer: PLongint; Samples: cunsigned): TFlacBool; cdecl; external;


{$ENDIF}

{$IFDEF FLAC_IMPL}
{$ENDIF}
