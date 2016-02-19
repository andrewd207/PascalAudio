(*-
 * Copyright (c) 2005 Boris Mikhaylov
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *)



unit bs2b;

{$mode objfpc}{$H+}
{$PACKRECORDS C}
{$linklib bs2b}

interface

uses
  Classes, SysUtils, ctypes;

type
  Pbs2b_int24 = ^bs2b_int24;
  bs2b_int24 = packed record
    octet0: byte;
    octet1: byte;
    octet2: shortint;
  end;

  Pbs2b_uint24 = ^bs2b_uint24;
  bs2b_uint24 = packed record
    octet0: byte;
    octet1: byte;
    octet2: byte;
  end;

  Pbs2bd = ^Tbs2bd;

  { Tbs2bd }

  Tbs2bd = object sealed
  public
    const
      BS2B_VERSION_MAJOR   = 3;
      BS2B_VERSION_MINOR   = 1;
      BS2B_VERSION_RELEASE = 0;
      BS2B_VERSION_INT = (BS2B_VERSION_MAJOR shl 16) or (BS2B_VERSION_MINOR shl 8) or (BS2B_VERSION_RELEASE);
      BS2B_MINSRATE = 2000;
      BS2B_MAXSRATE = 384000;
      BS2B_DEFAULT_SRATE = 44100;
      BS2B_MINFCUT = 300;
      BS2B_MAXFCUT = 2000;
      BS2B_MINFEED = 10;   //* 1 dB */
      BS2B_MAXFEED = 150;   //* 15 dB */
      BS2B_DEFAULT_CLEVEL  = LongWord(700) or ((LongWord(45) shl 16));
      BS2B_CMOY_CLEVEL    = (LongWord(700) or (LongWord(60) shl 16));
      BS2B_JMEIER_CLEVEL  = ( LongWord(650) or ( LongWord(95) shl 16));
  private
     _level: LongWord;             //* Crossfeed level */
     srate: LongWord;             //* Sample rate (Hz) */
     a0_lo, b1_lo: double;        //* Lowpass IIR filter coefficients */
     a0_hi, a1_hi, b1_hi: double; //* Highboost IIR filter coefficients */
     gain: double;                //* Global gain against overloading */
     //* Buffer of last filtered sample: [0] 1-st channel, [1] 2-d channel */
     asis: array[0..1] of double;
     lo: array[0..1] of double;
     hi: array[0..1] of double;
     //struct { double asis[ 2 ], lo[ 2 ], hi[ 2 ]; } lfs;
  private
     procedure SetLevel(ALevel: LongWord);
     function GetLevel: LongWord;
     procedure SetLevelfCut(fcut: cint);
     function GetLevelfCut: cint;
     procedure SetLevelFeed(feed: cint);
     function GetLevelFeed: cint;
     function GetLevelDelay: cint;
     procedure SetSampleRate(rate: Longword);
     function GetSampleRate:Longword;
  public
     function Open: Pbs2bd; static;
     procedure Close; //frees object
     procedure Clear;
     function IsClear: Boolean;
     function Version: pchar;
     function VersionInt: LongWord;

     procedure CrossFeed_d(sample: PDouble; n: cint);
     procedure CrossFeed_dbe(sample: PDouble; n: cint);
     procedure CrossFeed_dle(sample: PDouble; n: cint);

     procedure CrossFeed_f(sample: PSingle; n: cint);
     procedure CrossFeed_fbe(sample: PSingle; n: cint);
     procedure CrossFeed_fle(sample: PSingle; n: cint);

     procedure CrossFeed_s32(sample: PLongint; n: cint);
     procedure CrossFeed_s32be(sample: PLongint; n: cint);
     procedure CrossFeed_s32le(sample: PLongint; n: cint);
     procedure CrossFeed_u32(sample: PLongWord; n: cint);
     procedure CrossFeed_u32be(sample: PLongWord; n: cint);
     procedure CrossFeed_u32le(sample: PLongWord; n: cint);

     procedure CrossFeed_s16(sample: PSmallint; n: cint);
     procedure CrossFeed_s16be(sample: PSmallint; n: cint);
     procedure CrossFeed_s16le(sample: PSmallint; n: cint);
     procedure CrossFeed_u16(sample: PWord; n: cint);
     procedure CrossFeed_u16be(sample: PWord; n: cint);
     procedure CrossFeed_u16le(sample: PWord; n: cint);

     procedure CrossFeed_s8(sample: PShortInt; n: cint);
     procedure CrossFeed_u8(sample: PByte; n: cint);

     procedure CrossFeed_s24(sample: Pbs2b_int24; n: cint);
     procedure CrossFeed_s24be(sample: Pbs2b_int24; n: cint);
     procedure CrossFeed_s24le(sample: Pbs2b_int24; n: cint);
     procedure CrossFeed_u24(sample: Pbs2b_uint24; n: cint);
     procedure CrossFeed_u24be(sample: Pbs2b_uint24; n: cint);
     procedure CrossFeed_u24le(sample: Pbs2b_uint24; n: cint);

     property Level: LongWord read GetLevel write SetLevel;
     property LevelCutFreq: cint read GetLevelfCut write SetLevelfCut;
     property LevelFeed: cint read GetLevelFeed write SetLevelFeed;
     property LevelDelay: cint read GetLevelDelay;
     property SampleRate: LongWord read GetSampleRate write SetSampleRate;
  end;

implementation

function bs2b_open: Pbs2bd; cdecl; external;
procedure bs2b_close(bs2bdp: Pbs2bd); cdecl; external;
procedure bs2b_set_level( bs2bd: Pbs2bd; level: Longword); cdecl; external;
function bs2b_get_level(bs2bd: Pbs2bd ): LongWord; cdecl; external;
procedure bs2b_set_level_fcut(bs2bd: Pbs2bd; fcut: cint ); cdecl; external;
function bs2b_get_level_fcut(bs2bd: Pbs2bd ): cint; cdecl; external;
procedure bs2b_set_level_feed(bs2bd: Pbs2bd; feed: cint ); cdecl; external;
function bs2b_get_level_feed(bs2bd: Pbs2bd ): cint; cdecl; external;
function bs2b_get_level_delay(bs2bd: Pbs2bd ): cint; cdecl; external;
procedure bs2b_set_srate(bs2bd: Pbs2bd; srate: LongWord ); cdecl; external;
function bs2b_get_srate(bs2bd: Pbs2bd ): LongWord; cdecl; external;
procedure bs2b_clear(bs2bd: Pbs2bd ); cdecl; external;
function bs2b_is_clear(bs2bd: Pbs2bd ): cint; cdecl; external;
function bs2b_runtime_version: pchar; cdecl; external;
function bs2b_runtime_version_int: LongWord; cdecl; external;

procedure bs2b_cross_feed_d(bs2bd: Pbs2bd; sample: PDouble; n: cint ); cdecl; external;
procedure bs2b_cross_feed_dbe(bs2bd: Pbs2bd; sample: PDouble; n: cint ); cdecl; external;
procedure bs2b_cross_feed_dle(bs2bd: Pbs2bd; sample: PDouble; n: cint ); cdecl; external;
procedure bs2b_cross_feed_f(bs2bd: Pbs2bd; sample: PSingle; n: cint ); cdecl; external;
procedure bs2b_cross_feed_fbe(bs2bd: Pbs2bd; sample: PSingle; n: cint ); cdecl; external;
procedure bs2b_cross_feed_fle(bs2bd: Pbs2bd; sample: PSingle; n: cint ); cdecl; external;
procedure bs2b_cross_feed_s32(bs2bd: Pbs2bd; sample: PLongInt; n: cint); cdecl; external;
procedure bs2b_cross_feed_u32(bs2bd: Pbs2bd; sample: PLongWord; n: cint); cdecl; external;
procedure bs2b_cross_feed_s32be(bs2bd: Pbs2bd; sample: PLongInt; n: cint); cdecl; external;
procedure bs2b_cross_feed_u32be(bs2bd: Pbs2bd; sample: PLongWord; n: cint); cdecl; external;
procedure bs2b_cross_feed_s32le(bs2bd: Pbs2bd; sample: PLongInt; n: cint); cdecl; external;
procedure bs2b_cross_feed_u32le(bs2bd: Pbs2bd; sample: PLongWord; n: cint); cdecl; external;
procedure bs2b_cross_feed_s16(bs2bd: Pbs2bd; sample: PSmallInt; n: cint); cdecl; external;
procedure bs2b_cross_feed_u16(bs2bd: Pbs2bd; sample: PWord; n: cint); cdecl; external;
procedure bs2b_cross_feed_s16be(bs2bd: Pbs2bd; sample: PSmallInt; n: cint); cdecl; external;
procedure bs2b_cross_feed_u16be(bs2bd: Pbs2bd; sample: PWord; n: cint); cdecl; external;
procedure bs2b_cross_feed_s16le(bs2bd: Pbs2bd; sample: PSmallInt; n: cint); cdecl; external;
procedure bs2b_cross_feed_u16le(bs2bd: Pbs2bd; sample: PWord; n: cint); cdecl; external;
procedure bs2b_cross_feed_s8(bs2bd: Pbs2bd; sample: PShortInt; n: cint); cdecl; external;
procedure bs2b_cross_feed_u8(bs2bd: Pbs2bd; sample: PByte; n: cint); cdecl; external;
procedure bs2b_cross_feed_s24(bs2bd: Pbs2bd; sample: Pbs2b_int24; n: cint); cdecl; external;
procedure bs2b_cross_feed_u24(bs2bd: Pbs2bd; sample: Pbs2b_uint24; n: cint); cdecl; external;
procedure bs2b_cross_feed_s24be(bs2bd: Pbs2bd; sample: Pbs2b_int24; n: cint); cdecl; external;
procedure bs2b_cross_feed_u24be(bs2bd: Pbs2bd; sample: Pbs2b_uint24; n: cint); cdecl; external;
procedure bs2b_cross_feed_s24le(bs2bd: Pbs2bd; sample: Pbs2b_int24; n: cint); cdecl; external;
procedure bs2b_cross_feed_u24le(bs2bd: Pbs2bd; sample: Pbs2b_uint24; n: cint); cdecl; external;

{ Tbs2bd }

procedure Tbs2bd.SetLevel(ALevel: LongWord);
begin
  bs2b_set_level(@self, ALevel);
end;

function Tbs2bd.GetLevel: LongWord;
begin
  Result := bs2b_get_level(@self);

end;

procedure Tbs2bd.SetLevelfCut(fcut: cint);
begin
  bs2b_set_level_fcut(@self, fcut);
end;

function Tbs2bd.GetLevelfCut: cint;
begin
  Result := bs2b_get_level_fcut(@self);
end;

procedure Tbs2bd.SetLevelFeed(feed: cint);
begin
  bs2b_set_level_feed(@self, feed);
end;

function Tbs2bd.GetLevelFeed: cint;
begin
  Result := bs2b_get_level_feed(@self);
end;

function Tbs2bd.GetLevelDelay: cint;
begin
  Result := bs2b_get_level_delay(@self);
end;

procedure Tbs2bd.SetSampleRate(rate: Longword);
begin
  bs2b_set_srate(@self, rate);
end;

function Tbs2bd.GetSampleRate: Longword;
begin
  Result := bs2b_get_srate(@self);
end;

function Tbs2bd.Open: Pbs2bd;
begin
  Result := bs2b_open;
end;

procedure Tbs2bd.Close;
begin
  bs2b_close(@self);
end;

procedure Tbs2bd.Clear;
begin
  bs2b_clear(@self);
end;

function Tbs2bd.IsClear: Boolean;
begin
  Result := bs2b_is_clear(@self) = 1;
end;

function Tbs2bd.Version: pchar;
begin
  Result := bs2b_runtime_version;
end;

function Tbs2bd.VersionInt: LongWord;
begin
  Result := bs2b_runtime_version_int;
end;

procedure Tbs2bd.CrossFeed_d(sample: PDouble; n: cint);
begin
  bs2b_cross_feed_d(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_dbe(sample: PDouble; n: cint);
begin
  bs2b_cross_feed_dbe(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_dle(sample: PDouble; n: cint);
begin
  bs2b_cross_feed_dle(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_f(sample: PSingle; n: cint);
begin
  bs2b_cross_feed_f(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_fbe(sample: PSingle; n: cint);
begin
  bs2b_cross_feed_fbe(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_fle(sample: PSingle; n: cint);
begin
  bs2b_cross_feed_fle(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_s32(sample: PLongint; n: cint);
begin
  bs2b_cross_feed_s32(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_s32be(sample: PLongint; n: cint);
begin
  bs2b_cross_feed_s32be(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_s32le(sample: PLongint; n: cint);
begin
  bs2b_cross_feed_s32le(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_u32(sample: PLongWord; n: cint);
begin
  bs2b_cross_feed_u32(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_u32be(sample: PLongWord; n: cint);
begin
  bs2b_cross_feed_u32be(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_u32le(sample: PLongWord; n: cint);
begin
  bs2b_cross_feed_u32le(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_s16(sample: PSmallint; n: cint);
begin
  bs2b_cross_feed_s16(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_s16be(sample: PSmallint; n: cint);
begin
  bs2b_cross_feed_s16be(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_s16le(sample: PSmallint; n: cint);
begin
  bs2b_cross_feed_s16le(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_u16(sample: PWord; n: cint);
begin
  bs2b_cross_feed_u16(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_u16be(sample: PWord; n: cint);
begin
  bs2b_cross_feed_u16be(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_u16le(sample: PWord; n: cint);
begin
  bs2b_cross_feed_u16le(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_s8(sample: PShortInt; n: cint);
begin
  bs2b_cross_feed_s8(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_u8(sample: PByte; n: cint);
begin
  bs2b_cross_feed_u8(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_s24(sample: Pbs2b_int24; n: cint);
begin
  bs2b_cross_feed_s24(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_s24be(sample: Pbs2b_int24; n: cint);
begin
  bs2b_cross_feed_s24be(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_s24le(sample: Pbs2b_int24; n: cint);
begin
  bs2b_cross_feed_s24le(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_u24(sample: Pbs2b_uint24; n: cint);
begin
  bs2b_cross_feed_u24(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_u24be(sample: Pbs2b_uint24; n: cint);
begin
  bs2b_cross_feed_u24be(@self, sample, n);
end;

procedure Tbs2bd.CrossFeed_u24le(sample: Pbs2b_uint24; n: cint);
begin
  bs2b_cross_feed_u24le(@self, sample, n);
end;

end.

