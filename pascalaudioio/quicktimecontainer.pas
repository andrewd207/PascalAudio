{
    This unit is part of the PascalAudio project.

    Copyright (c) 2020 by Andrew Haines.

    See the files COPYING.modifiedLGPL and license.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit quicktimecontainer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, quicktimeatoms;

type


  TTopLevelAtoms = set of (tla_ftyp, tla_moov, tla_mdat);

  { TQuicktimeContainer }

  TQuicktimeContainer = class
    function AtomLoaded(Sender: TAtomList; AAtom: TAtom): Boolean;
  private
    FPresentAtoms: TTopLevelAtoms;
    FAtoms: TAtomList;
    FOwnsStream: Boolean;
    FStream: TStream;
    function GetIsValidFile: Boolean;
    procedure LoadTopLevelAtoms;
  public
    constructor Create(AFilename: String);
    constructor Create(AStream: TStream; AOwnsStream: Boolean);
    property Atoms: TAtomList read FAtoms;
    property Stream: TStream read FStream;
    destructor Destroy; override;
    property IsValidFile: Boolean read GetIsValidFile;
  end;

// audiodata: 'moov/trak/mdia/mdhd' // samplerate and other stuff too. not channels
// audiodata: 'moov/trak:%d/mdia/minf/stbl/stsd'
// chaptrak:  'moov/trak/tref/chap'
// tags:      'moov/udta/meta/ilst'
// chapframes:'moov/trak:%d/mdia/minf/stbl/stts'  // %d is the trak from chaptrak in track 1
// chapters:  'moov/trak:%d/udta/meta/ilst'  // %d is the trak from chaptrak in track 1
//


implementation

{ TQuicktimeContainer }

function TQuicktimeContainer.AtomLoaded(Sender: TAtomList; AAtom: TAtom): Boolean;
begin
  Result := True;
  if FPresentAtoms <> [tla_ftyp, tla_moov, tla_mdat] then
  begin
    if AAtom.AtomName = 'ftyp' then
      Include(FPresentAtoms, tla_ftyp)
    else if AAtom.AtomName = 'moov' then
      Include(FPresentAtoms, tla_moov)
    else if AAtom.AtomName = 'mdat' then
      Include(FPresentAtoms, tla_mdat);
    if (Sender.Count = 0) and not (tla_ftyp in FPresentAtoms) then
      Result := False;
  end;
end;

procedure TQuicktimeContainer.LoadTopLevelAtoms;
var
  Atom: TAtom;
begin
  FStream.Position:=0;
  FAtoms.LoadAtoms(FStream, @AtomLoaded, FStream.Size);
end;

function TQuicktimeContainer.GetIsValidFile: Boolean;
begin
  Result := FPresentAtoms = [tla_ftyp, tla_moov, tla_mdat];
end;

constructor TQuicktimeContainer.Create(AFilename: String);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
  Create(F, True);
end;

constructor TQuicktimeContainer.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  FOwnsStream := AOwnsStream;
  FStream := AStream;

  FAtoms := TAtomList.Create(nil);
  LoadTopLevelAtoms;
end;

destructor TQuicktimeContainer.Destroy;
begin
  if FOwnsStream then
    FStream.Free;
  if Assigned(FAtoms) then
    FAtoms.Free;
  inherited Destroy;
end;

end.


