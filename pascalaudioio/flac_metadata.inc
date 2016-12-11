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
  TFlacMetadataSimpleIteratorStatus = (
    fmsisOk = 0,
    fmsisIllegalInput,
    fmsisErrorOpeningFile,
    fmsisNotAFlacFile,
    fmsisNotWritable,
    fmsisBadMetadata,
    fmsisReadError,
    fmsisSeekError,
    fmsisWriteError,
    fmsisRenameError,
    fmsisUnlinkError,
    fmsisMemoryAllocationError,
    fmsisInternalError
  );
  TFlacMetadataChainStatus = (
    fmcsOK = 0,
    fmcsIllegalInput,
    fmcsErrorOpeningFile,
    fmcsNotAFlacFile,
    fmcsNotWritable,
    fmcsBadMetadata,
    fmcsReadError,
    fmcsSeekError,
    fmcsWriteError,
    fmcsRenameError,
    fmcsUnlinkError,
    fmcsMemoryAllocationError,
    fmcsInternalError,
    fmcsInvalidCallbacks,
    fmcsReadWriteMismatch,
    fmcsWrongWriteCall
  );
{$ENDIF}

{$IFDEF FLAC_INTF}

function FLAC__metadata_get_streaminfo(Filename: PChar; streaminfo: PFlacStreamMetadataStruct): TFlacBool; cdecl; external;
function FLAC__metadata_get_tags(Filename: PChar; tags: PPFlacStreamMetadataStruct): TFlacBool; cdecl; external;
function FLAC__metadata_get_cuesheet(Filename: PChar; cuesheet: PPFlacStreamMetadataStruct): TFlacBool; cdecl; external;
function FLAC__metadata_get_picture(Filename: PChar; picture: PPFlacStreamMetadataStruct; Type_: TFlacStreamMetadataPictureType; mimetype: PChar; description: PByte; max_width, max_height, max_depth, max_colors: cunsigned): TFlacBool; cdecl; external;

type
  PFlacMetadataSimpleIteratorStruct = ^TFlacMetadataSimpleIteratorStruct;
  TFlacMetadataSimpleIteratorStruct = record
    // opaque
  end;


var
  FLAC__Metadata_SimpleIteratorStatusString: PChar; cvar; external;

function  FLAC__metadata_simple_iterator_new: PFlacMetadataSimpleIteratorStruct; cdecl; external;
procedure FLAC__metadata_simple_iterator_delete(Iterator: PFlacMetadataSimpleIteratorStruct); cdecl; external;
function  FLAC__metadata_simple_iterator_status(Iterator: PFlacMetadataSimpleIteratorStruct): TFlacMetadataSimpleIteratorStatus; cdecl; external;
function  FLAC__metadata_simple_iterator_init(Iterator: PFlacMetadataSimpleIteratorStruct; Filename: PChar; ReadOnly, PreserveFileStats: TFlacBool): TFlacbool; cdecl; external;

function  FLAC__metadata_simple_iterator_is_writable(Iterator: PFlacMetadataSimpleIteratorStruct): TFlacbool; cdecl; external;
function  FLAC__metadata_simple_iterator_next(Iterator: PFlacMetadataSimpleIteratorStruct): TFlacbool; cdecl; external;
function  FLAC__metadata_simple_iterator_prev(Iterator: PFlacMetadataSimpleIteratorStruct): TFlacbool; cdecl; external;
function  FLAC__metadata_simple_iterator_is_last(Iterator: PFlacMetadataSimpleIteratorStruct): TFlacbool; cdecl; external;
function  FLAC__metadata_simple_iterator_get_block_offset(Iterator: PFlacMetadataSimpleIteratorStruct): coff_t; cdecl; external;
function  FLAC__metadata_simple_iterator_get_block_type(Iterator: PFlacMetadataSimpleIteratorStruct): TFlacMetadataType; cdecl; external;
function  FLAC__metadata_simple_iterator_get_block_length(Iterator: PFlacMetadataSimpleIteratorStruct): cunsigned; cdecl; external;
function  FLAC__metadata_simple_iterator_get_application_id(Iterator: PFlacMetadataSimpleIteratorStruct; ID: PByte): TFlacbool; cdecl; external;
function  FLAC__metadata_simple_iterator_get_block(Iterator: PFlacMetadataSimpleIteratorStruct): PFlacStreamMetadataStruct; cdecl; external;
function  FLAC__metadata_simple_iterator_set_block(Iterator: PFlacMetadataSimpleIteratorStruct; block: PFlacStreamMetadataStruct; UsePadding: TFlacBool): TFlacbool; cdecl; external;
function  FLAC__metadata_simple_iterator_insert_block_after(Iterator: PFlacMetadataSimpleIteratorStruct; block: PFlacStreamMetadataStruct; UsePadding: TFlacBool): TFlacbool; cdecl; external;
function  FLAC__metadata_simple_iterator_delete_block(Iterator: PFlacMetadataSimpleIteratorStruct; block: PFlacStreamMetadataStruct; UsePadding: TFlacBool): TFlacbool; cdecl; external;

type
  PFlacMetadataChainStruct = ^TFlacMetadataChainStruct;
  TFlacMetadataChainStruct = record
  end;

  PFlacMetadataIteratorStruct = ^TFlacMetadataIteratorStruct;
  TFlacMetadataIteratorStruct = record
  end;


var
  FLAC__Metadata_ChainStatusString: PPChar; cvar; external;

// chain
function  FLAC__metadata_chain_new:PFlacMetadataChainStruct; cdecl; external;
procedure FLAC__metadata_chain_delete(chain: PFlacMetadataChainStruct); cdecl; external;
function  FLAC__metadata_chain_status(chain: PFlacMetadataChainStruct): TFlacMetadataChainStatus; cdecl; external;
function  FLAC__metadata_chain_read(chain: PFlacMetadataChainStruct; Filename: PChar): TFlacbool; cdecl; external;
function  FLAC__metadata_chain_read_ogg(chain: PFlacMetadataChainStruct; Filename: PChar): TFlacbool; cdecl; external;
function  FLAC__metadata_chain_read_with_callbacks(chain: PFlacMetadataChainStruct; handle: PFlacIOHandle; callbacks: PFlacIOCallbacks): TFlacbool; cdecl; external;
function  FLAC__metadata_chain_read_ogg_with_callbacks(chain: PFlacMetadataChainStruct; handle: PFlacIOHandle; callbacks: PFlacIOCallbacks): TFlacbool; cdecl; external;
function  FLAC__metadata_chain_check_if_tempfile_needed(chain: PFlacMetadataChainStruct; usepadding: TFlacBool): TFlacbool; cdecl; external;
function  FLAC__metadata_chain_write(chain: PFlacMetadataChainStruct; usepadding: TFlacBool; preserve_file_Stats: TFlacBool ): TFlacbool; cdecl; external;
function  FLAC__metadata_chain_write_with_callbacks(chain: PFlacMetadataChainStruct; usepadding: TFlacBool; handle: PFlacIOHandle; callbacks: PFlacIOCallbacks): TFlacbool; cdecl; external;
function  FLAC__metadata_chain_write_with_callbacks_and_tempfile(chain: PFlacMetadataChainStruct; usepadding: TFlacBool; handle: PFlacIOHandle; callbacks: PFlacIOCallbacks; temphandle: PFlacIOHandle; temp_callbacks: PFlacIOCallbacks): TFlacbool; cdecl; external;
procedure FLAC__metadata_chain_merge_padding(chain: PFlacMetadataChainStruct); cdecl; external;
procedure FLAC__metadata_chain_sort_padding(chain: PFlacMetadataChainStruct); cdecl; external;

// iterator
function  FLAC__metadata_iterator_new: PFlacMetadataIteratorStruct; cdecl; external;
procedure FLAC__metadata_iterator_delete(iterator: PFlacMetadataIteratorStruct); cdecl; external;
function  FLAC__metadata_iterator_init(iterator: PFlacMetadataIteratorStruct; chain: PFlacMetadataChainStruct): TFlacbool; cdecl; external;
function  FLAC__metadata_iterator_next(iterator: PFlacMetadataIteratorStruct): TFlacbool; cdecl; external;
function  FLAC__metadata_iterator_prev(iterator: PFlacMetadataIteratorStruct): TFlacbool; cdecl; external;
function  FLAC__metadata_iterator_get_block_type(iterator: PFlacMetadataIteratorStruct): TFlacMetadataType; cdecl; external;
function  FLAC__metadata_iterator_get_block(iterator: PFlacMetadataIteratorStruct): PFlacStreamMetadataStruct; cdecl; external;
function  FLAC__metadata_iterator_set_block(iterator: PFlacMetadataIteratorStruct; block: PFlacStreamMetadataStruct): TFlacbool; cdecl; external;
function  FLAC__metadata_iterator_delete_block(iterator: PFlacMetadataIteratorStruct; replace_with_padding: TFlacBool): TFlacbool; cdecl; external;
function  FLAC__metadata_iterator_insert_block_before(iterator: PFlacMetadataIteratorStruct; block: PFlacStreamMetadataStruct): TFlacbool; cdecl; external;
function  FLAC__metadata_iterator_insert_block_after(iterator: PFlacMetadataIteratorStruct; block: PFlacStreamMetadataStruct): TFlacbool; cdecl; external;

// streammetadata
function  FLAC__metadata_object_new(type_ : TFlacMetadataType): PFlacStreamMetadataStruct; cdecl; external;
function  FLAC__metadata_object_clone(obj : PFlacStreamMetadataStruct): PFlacStreamMetadataStruct; cdecl; external;
procedure FLAC__metadata_object_delete(obj: PFlacStreamMetadataStruct); cdecl; external;
function  FLAC__metadata_object_is_equal(block1: PFlacStreamMetadataStruct; block2: PFlacStreamMetadataStruct): TFlacbool; cdecl; external;
function  FLAC__metadata_object_application_set_data(obj: PFlacStreamMetadataStruct; data: PByte; lenght: cunsigned; copy: TFlacBool): TFlacbool; cdecl; external;
function  FLAC__metadata_object_seektable_resize_points(obj: PFlacStreamMetadataStruct; new_num_points: cunsigned): TFlacbool; cdecl; external;
procedure FLAC__metadata_object_seektable_set_point(obj: PFlacStreamMetadataStruct; point_num: cunsigned; point: TFlacStreamMetadataSeekPoint); cdecl; external;
function  FLAC__metadata_object_seektable_insert_point(obj: PFlacStreamMetadataStruct; point_num: cunsigned; point: TFlacStreamMetadataSeekPoint): TFlacbool; cdecl; external;
function  FLAC__metadata_object_seektable_delete_point(obj: PFlacStreamMetadataStruct; point_num: cunsigned): TFlacbool; cdecl; external;
function  FLAC__metadata_object_seektable_is_legal(obj: PFlacStreamMetadataStruct): TFlacbool; cdecl; external;
function  FLAC__metadata_object_seektable_template_append_placeholders(obj: PFlacStreamMetadataStruct; num: cunsigned): TFlacbool; cdecl; external;
function  FLAC__metadata_object_seektable_template_append_point(obj: PFlacStreamMetadataStruct; sample_number: QWord): TFlacbool; cdecl; external;
function  FLAC__metadata_object_seektable_template_append_points(obj: PFlacStreamMetadataStruct; sample_numbers: PQWord; num: cunsigned): TFlacbool; cdecl; external;
function  FLAC__metadata_object_seektable_template_append_spaced_points(obj: PFlacStreamMetadataStruct; num: cunsigned; total_samples: QWord): TFlacbool; cdecl; external;
function  FLAC__metadata_object_seektable_template_append_spaced_points_by_samples(obj: PFlacStreamMetadataStruct; samples: cunsigned; total_samples: QWord): TFlacbool; cdecl; external;
function  FLAC__metadata_object_seektable_template_sort(obj: PFlacStreamMetadataStruct; compact: TFlacBool): TFlacbool; cdecl; external;
function  FLAC__metadata_object_vorbiscomment_set_vendor_string(obj: PFlacStreamMetadataStruct; entry: TFlacStreamMetadataVorbisCommentStruct; copy: TFlacBool): TFlacbool; cdecl; external;
function  FLAC__metadata_object_vorbiscomment_resize_comments(obj: PFlacStreamMetadataStruct; new_num_comments: cunsigned): TFlacbool; cdecl; external;
function  FLAC__metadata_object_vorbiscomment_set_comment(obj: PFlacStreamMetadataStruct; comment_num: cunsigned; entry: TFlacStreamMetadataVorbisCommentStruct; copy: TFlacBool): TFlacbool; cdecl; external;
function  FLAC__metadata_object_vorbiscomment_insert_comment(obj: PFlacStreamMetadataStruct; comment_num: cunsigned; entry: TFlacStreamMetadataVorbisCommentStruct; copy: TFlacBool): TFlacbool; cdecl; external;
function  FLAC__metadata_object_vorbiscomment_append_comment(obj: PFlacStreamMetadataStruct; entry: TFlacStreamMetadataVorbisCommentStruct; copy: TFlacBool): TFlacbool; cdecl; external;
function  FLAC__metadata_object_vorbiscomment_replace_comment(obj: PFlacStreamMetadataStruct;  entry: TFlacStreamMetadataVorbisCommentStruct; all: TFlacBool; copy: TFlacBool): TFlacbool; cdecl; external;
function  FLAC__metadata_object_vorbiscomment_delete_comment(obj: PFlacStreamMetadataStruct; comment_num: cunsigned): TFlacbool; cdecl; external;
function  FLAC__metadata_object_vorbiscomment_entry_from_name_value_pair(obj: PFlacStreamMetadataStruct): TFlacbool; cdecl; external;
function  FLAC__metadata_object_vorbiscomment_find_entry_from(obj: PFlacStreamMetadataStruct; offset: cunsigned; field_name: PChar): cint; cdecl; external;
function  FLAC__metadata_object_vorbiscomment_remove_entry_matching(obj: PFlacStreamMetadataStruct; field_name: PChar): cint; cdecl; external;
function  FLAC__metadata_object_vorbiscomment_remove_entries_matching(obj: PFlacStreamMetadataStruct; field_name: PChar): cint; cdecl; external;

// vorbis comment entry
function  FLAC__metadata_object_vorbiscomment_entry_from_name_value_pair(entry: PFlacStreamMetadataVorbisCommentEntry; field_name, field_value: pchar): TFlacbool; cdecl; external;
function  FLAC__metadata_object_vorbiscomment_entry_matches(entry: PFlacStreamMetadataVorbisCommentEntry; field_name: PChar; field_name_length: cunsigned): TFlacbool; cdecl; external;

// cue sheet track
function  FLAC__metadata_object_cuesheet_track_new: PFlacStreamMetadataCueSheetTrack; cdecl; external;
function  FLAC__metadata_object_cuesheet_track_clone(obj: PFlacStreamMetadataCueSheetTrack): PFlacStreamMetadataCueSheetTrack; cdecl; external;
procedure FLAC__metadata_object_cuesheet_track_delete(obj: PFlacStreamMetadataCueSheetTrack); cdecl; external;

// cue sheet
function  FLAC__metadata_object_cuesheet_track_resize_indices(obj: PFlacStreamMetadataCueSheetStruct; track_num, new_num_indices: cunsigned): TFlacBool; cdecl; external;
function  FLAC__metadata_object_cuesheet_track_insert_index(obj: PFlacStreamMetadataCueSheetStruct; track_num, index_num: cunsigned; index: TFlacStreamMetadataCueSheetIndex): TFlacBool; cdecl; external;
function  FLAC__metadata_object_cuesheet_track_insert_blank_index(obj: PFlacStreamMetadataCueSheetStruct; track_num, index_num: cunsigned): TFlacBool; cdecl; external;
function  FLAC__metadata_object_cuesheet_track_delete_index(obj: PFlacStreamMetadataCueSheetStruct; track_num, index_num: cunsigned): TFlacBool; cdecl; external;
function  FLAC__metadata_object_cuesheet_resize_tracks(obj: PFlacStreamMetadataCueSheetStruct; new_num_tracks: cunsigned): TFlacBool; cdecl; external;
function  FLAC__metadata_object_cuesheet_set_track(obj: PFlacStreamMetadataCueSheetStruct): TFlacBool; cdecl; external;
function  FLAC__metadata_object_cuesheet_insert_track(obj: PFlacStreamMetadataCueSheetStruct; track_num: cunsigned; track : PFlacStreamMetadataCueSheetTrack; copy: TFlacBool): TFlacBool; cdecl; external;
function  FLAC__metadata_object_cuesheet_insert_blank_track(obj: PFlacStreamMetadataCueSheetStruct; track_num: cunsigned): TFlacBool; cdecl; external;
function  FLAC__metadata_object_cuesheet_delete_track(obj: PFlacStreamMetadataCueSheetStruct; track_num: cunsigned): TFlacBool; cdecl; external;
function  FLAC__metadata_object_cuesheet_is_legal(obj: PFlacStreamMetadataCueSheetStruct; check_cd_da_subset: TFlacBool; violation: PPChar): TFlacBool; cdecl; external;
function  FLAC__metadata_object_cuesheet_calculate_cddb_id(obj: PFlacStreamMetadataCueSheetStruct): LongWord; cdecl; external;

// picture
function  FLAC__metadata_object_picture_set_mime_type(obj: PFlacStreamMetadataPictureStruct; mime_type: PChar; copy: TFlacBool): TFlacBool; cdecl; external;
function  FLAC__metadata_object_picture_set_description(obj: PFlacStreamMetadataPictureStruct; description: PByte; copy: TFlacBool): TFlacBool; cdecl; external;
function  FLAC__metadata_object_picture_set_data(obj: PFlacStreamMetadataPictureStruct; data: PByte; length: LongWord): TFlacBool; cdecl; external;
function  FLAC__metadata_object_picture_is_legal(obj: PFlacStreamMetadataPictureStruct; Violation: PPChar): TFlacBool; cdecl; external;


{$ENDIF}
