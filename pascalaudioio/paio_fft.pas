{
    This unit is part of PascalAudioIO package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

{ A small, dependency-free radix-2 Cooley-Tukey FFT written from scratch so the
  package stays under its own (modified LGPL) license. Operates on single
  precision floats. For real audio input use ForwardReal, which returns the
  magnitude spectrum from DC to Nyquist. }

unit paio_fft;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TPAIOFFT }

  TPAIOFFT = class
  private
    FSize: Integer;
    FBits: Integer;
    FBitRev: array of Integer;
    FCos: array of Single;   // twiddle table, length Size div 2
    FSin: array of Single;
    FRe: array of Single;    // scratch work buffers
    FIm: array of Single;
    procedure BuildTables;
  public
    // ASize must be a power of two (>= 2).
    constructor Create(ASize: Integer);
    // Forward FFT of Size real samples in AInput. Writes the magnitude of each
    // bin from DC..Nyquist into AMagnitude, which must hold BinCount floats.
    // AInput is not modified.
    procedure ForwardReal(AInput: PSingle; AMagnitude: PSingle);
    // number of magnitude bins ForwardReal produces (DC..Nyquist inclusive)
    function  BinCount: Integer;
    property  Size: Integer read FSize;
  end;

function IsPowerOfTwo(N: Integer): Boolean;

implementation

function IsPowerOfTwo(N: Integer): Boolean;
begin
  Result := (N > 1) and ((N and (N - 1)) = 0);
end;

{ TPAIOFFT }

constructor TPAIOFFT.Create(ASize: Integer);
begin
  if not IsPowerOfTwo(ASize) then
    raise Exception.CreateFmt('TPAIOFFT: size %d is not a power of two', [ASize]);
  FSize := ASize;
  FBits := 0;
  while (1 shl FBits) < FSize do
    Inc(FBits);
  SetLength(FRe, FSize);
  SetLength(FIm, FSize);
  BuildTables;
end;

procedure TPAIOFFT.BuildTables;
var
  i, j, rev, b: Integer;
begin
  // bit-reversal permutation indices
  SetLength(FBitRev, FSize);
  for i := 0 to FSize - 1 do
  begin
    rev := 0;
    j := i;
    for b := 0 to FBits - 1 do
    begin
      rev := (rev shl 1) or (j and 1);
      j := j shr 1;
    end;
    FBitRev[i] := rev;
  end;

  // twiddle factors at full resolution: angle = -2*pi*k / Size
  SetLength(FCos, FSize div 2);
  SetLength(FSin, FSize div 2);
  for i := 0 to (FSize div 2) - 1 do
  begin
    FCos[i] := Cos(-2.0 * Pi * i / FSize);
    FSin[i] := Sin(-2.0 * Pi * i / FSize);
  end;
end;

function TPAIOFFT.BinCount: Integer;
begin
  Result := FSize div 2 + 1;
end;

procedure TPAIOFFT.ForwardReal(AInput: PSingle; AMagnitude: PSingle);
var
  i, half, step, twStep, k, pair: Integer;
  ar, ai, br, bi, wr, wi, tr, ti: Single;
  re, im: PSingle;
begin
  re := @FRe[0];
  im := @FIm[0];

  // load input in bit-reversed order, imaginary part zero
  for i := 0 to FSize - 1 do
  begin
    re[FBitRev[i]] := AInput[i];
    im[FBitRev[i]] := 0;
  end;

  // iterative butterflies, stage size doubling each pass
  half := 1;
  while half < FSize do
  begin
    step   := half * 2;
    twStep := FSize div step; // stride into the full-resolution twiddle table
    i := 0;
    while i < FSize do
    begin
      k := 0;
      for pair := 0 to half - 1 do
      begin
        wr := FCos[k];
        wi := FSin[k];
        ar := re[i + pair];
        ai := im[i + pair];
        br := re[i + pair + half];
        bi := im[i + pair + half];
        tr := wr * br - wi * bi;
        ti := wr * bi + wi * br;
        re[i + pair]        := ar + tr;
        im[i + pair]        := ai + ti;
        re[i + pair + half] := ar - tr;
        im[i + pair + half] := ai - ti;
        Inc(k, twStep);
      end;
      Inc(i, step);
    end;
    half := step;
  end;

  // magnitude of each bin from DC to Nyquist
  for i := 0 to FSize div 2 do
    AMagnitude[i] := Sqrt(re[i] * re[i] + im[i] * im[i]);
end;

end.
