{libsamplerate which this file links to is GPL}
(*
** Copyright (C) 2002-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
*/

/*
** This code is part of Secret Rabbit Code aka libsamplerate. A commercial
** use license for this code is available, please see:
**		http://www.mega-nerd.com/SRC/procedure.html
*)

(*
** API documentation is available here:
**     http://www.mega-nerd.com/SRC/api.html
*)

unit samplerate;

{$mode objfpc}{$H+}
{$packrecords c}
{$linklib samplerate}

interface

uses
  ctypes;

type

//* Opaque data type SRC_STATE. */
  PSRC_STATE = ^SRC_STATE;
  SRC_STATE = record
  end;

//* SRC_DATA is used to pass data to src_simple() and src_process(). */
  PSRC_DATA = ^SRC_DATA;
  SRC_DATA = record
    data_in,
    data_out: pcfloat;

    input_frames,
    output_frames: clong;
    input_frames_used,
    output_frames_gen: clong;

    end_of_input: cint;

    src_ratio: cdouble;
  end;


//* SRC_CB_DATA is used with callback based API. */
  SRC_CB_DATA = record
    frames : clong;
    data_in: pcfloat;
  end;

(*
** User supplied callback function type for use with src_callback_new()
** and src_callback_read(). First parameter is the same pointer that was
** passed into src_callback_new(). Second parameter is pointer to a
** pointer. The user supplied callback function must modify *data to
** point to the start of the user supplied float array. The user supplied
** function must return the number of frames that **data points to.
*)
  ppcfloat = ^pcfloat;
  src_callback_t = function(cb_data: pointer; data: ppcfloat): clong; cdecl;

(*
**	Standard initialisation function : return an anonymous pointer to the
**	internal state of the converter. Choose a converter from the enums below.
**	Error returned in *error.
*)

function src_new (converter_type: cint; channels: cint; error: pcint): PSRC_STATE; cdecl; external;

(*
**	Initilisation for callback based API : return an anonymous pointer to the
**	internal state of the converter. Choose a converter from the enums below.
**	The cb_data pointer can point to any data or be set to NULL. Whatever the
**	value, when processing, user supplied function "func" gets called with
**	cb_data as first parameter.
*)

function src_callback_new (func: src_callback_t; converter_type: cint; channels: cint;
				error: pcint;cb_data: pointer): PSRC_STATE; cdecl; external;

(*
**	Cleanup all internal allocations.
**	Always returns NULL.
*)

function src_delete (state: PSRC_STATE): PSRC_STATE; cdecl; external;

(*
**	Standard processing function.
**	Returns non zero on error.
*)

function src_process (state: PSRC_STATE; data: PSRC_DATA): cint; cdecl; external;

(*
**	Callback based processing function. Read up to frames worth of data from
**	the converter int *data and return frames read or -1 on error.
*)
function src_callback_read (state: PSRC_STATE; src_ratio: cdouble; frames: clong; data: pcfloat): clong; cdecl; external;

(*
**	Simple interface for performing a single conversion from input buffer to
**	output buffer at a fixed conversion ratio.
**	Simple interface does not require initialisation as it can only operate on
**	a single buffer worth of audio.
*)
function src_simple (data: PSRC_DATA; converter_type: cint; channels: cint ): cint; cdecl; external;

(*
** This library contains a number of different sample rate converters,
** numbered 0 through N.
**
** Return a string giving either a name or a more full description of each
** sample rate converter or NULL if no sample rate converter exists for
** the given value. The converters are sequentially numbered from 0 to N.
*)

 function src_get_name (converter_type: cint): PChar; cdecl; external;
 function src_get_description (converter_type: cint): PChar; cdecl; external;
 function src_get_version: PChar; cdecl; external;

(*
**	Set a new SRC ratio. This allows step responses
**	in the conversion ratio.
**	Returns non zero on error.
*)

function src_set_ratio (state: PSRC_STATE; new_ratio: cdouble): cint; cdecl; external;

(*
**	Reset the internal SRC state.
**	Does not modify the quality settings.
**	Does not free any memory allocations.
**	Returns non zero on error.
*)

function src_reset (state: PSRC_STATE): cint; cdecl; external;

(*
** Return TRUE if ratio is a valid conversion ratio, FALSE
** otherwise.
*)

function src_is_valid_ratio (ratio: cdouble ): cint; cdecl; external;

(*
**	Return an error number.
*)

function src_error (state: PSRC_STATE): cint; cdecl; external;

(*
**	Convert the error number into a string.
*)
function src_strerror (error: cint): PChar;  cdecl; external;

(*
** The following enums can be used to set the interpolator type
** using the function src_set_converter().
*)

const
  SRC_SINC_BEST_QUALITY    = 0;
  SRC_SINC_MEDIUM_QUALITY  = 1;
  SRC_SINC_FASTEST         = 2;
  SRC_ZERO_ORDER_HOLD      = 3;
  SRC_LINEAR               = 4;


(*
** Extra helper functions for converting from short to float and
** back again.
*)

procedure src_short_to_float_array (const_in: pcshort; out_data: pcfloat; len: cint) cdecl; external;
procedure src_float_to_short_array (const_in: pcfloat; out_data: pcshort; len: cint); cdecl; external;

procedure src_int_to_float_array (const_in: pcint; out_data: pcfloat; len: cint); cdecl; external;
procedure src_float_to_int_array (const_in: pcfloat; out_data: pcint; len: cint); cdecl; external;


implementation

end.

