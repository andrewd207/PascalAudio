{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_cdaudio;

{$mode objfpc}{$H+}

interface
{$IFDEF UNIX}
uses
  Classes, SysUtils,
  pa_base,
  lincd, cdrom,
  discid
  ;

type

  { TPAAudioCDSource }

  TPAAudioCDSource = class(TPAAudioSource)
  public
    type
      TTrackInfo = TTocEntry;
      TTracks = array of TTrackInfo;
  private
    FDevice: String;
    FFirstTrack: Integer;
    FHandle: Integer;
    FInited: Boolean;
    FLastTrack: Integer;
    FTrackCount: Integer;
    FTracks: TTracks;
    FFirstFrame: Integer;
    FFrameIndex: Integer;
    FLastFrame: Integer;
    function GetCDDBId: Integer;
    function GetTrack(AIndex: Integer): TTrackInfo;
    function Inited: Boolean;
    procedure DeInit;
    procedure SetFirstTrack(AValue: Integer);
    procedure SetLastTrack(AValue: Integer);
    procedure SetTrackCount(AValue: Integer);
  protected
    function InternalOutputToDestination: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Device: String read FDevice write FDevice;
    property Track[AIndex: Integer]: TTrackInfo read GetTrack;
    property TrackCount: Integer read FTrackCount write SetTrackCount;
    property FirstTrack: Integer read FFirstTrack write SetFirstTrack;
    property LastTrack: Integer read FLastTrack write SetLastTrack;
    property CDDBId: Integer read GetCDDBId;
  end;

{$ENDIF}

implementation
{$IFDEF UNIX}
uses
  BaseUnix, Unix;

{ TPAAudioCDSource }

function TPAAudioCDSource.Inited: Boolean;
var
  TocHeader: Tcdrom_tochdr;
  TocEntry: Tcdrom_tocentry;
  i: Integer;
  t: Integer=0;
begin
  if FInited then
    Exit(True);

  Result := False;

  // check if device not set or device doesn't exist
  if not ((FDevice <> '') and FileExists(FDevice)) then
    Exit;

  FHandle := FpOpen(FDevice, Open_RdOnly or Open_NonBlock);
  if FHandle < 0 then
    Exit;

  if FpIOCtl(FHandle, CDROMREADTOCHDR, @TocHeader)<>0 then
    Exit;

  if TocHeader.cdth_trk1-TocHeader.cdth_trk0 > 0 then
  begin
    SetLength(FTracks, TocHeader.cdth_trk1-TocHeader.cdth_trk0 +1);
    for i := TocHeader.cdth_trk0 to TocHeader.cdth_trk1 do
      begin
        TocEntry.cdte_track := i;
        TocEntry.cdte_format := CDROM_MSF;
        FpIOCtl(FHandle, CDROMREADTOCENTRY, @TocEntry);
        FTracks[t].min:=TocEntry.cdte_addr.msf.minute;
        FTracks[t].sec:=TocEntry.cdte_addr.msf.second;
        FTracks[t].frame:=((TocEntry.cdte_addr.msf.minute*60) + TocEntry.cdte_addr.msf.second) * 75 + TocEntry.cdte_addr.msf.frame;
        if i = TocHeader.cdth_trk0 then
          FFirstFrame:=FTracks[t].frame;
        Inc(t);
      end;
    // now set the last tracks end position as the end of the cd
    TocEntry.cdte_track := $AA;
    TocEntry.cdte_format := CDROM_MSF;
    FpIOCtl(FHandle, CDROMREADTOCENTRY, @TocEntry);
    FTracks[t].min:=TocEntry.cdte_addr.msf.minute;
    FTracks[t].sec:=TocEntry.cdte_addr.msf.second;
    FTracks[t].frame:=((TocEntry.cdte_addr.msf.minute*60) + TocEntry.cdte_addr.msf.second) * 75 + TocEntry.cdte_addr.msf.frame;
    FLastFrame := FTracks[t].frame;
  end;

  FFrameIndex:=FFirstFrame;

  FInited:=True;

  Channels:=2;
  Format:=afS16;
  SamplesPerSecond:=44100;

  if FirstTrack = 0 then
    FirstTrack:=1;
  if LastTrack = 0 then
    LastTrack :=TrackCount;

  FirstTrack:=FFirstTrack;
  LastTrack:=FLastTrack;


  Result := True;
end;

procedure TPAAudioCDSource.DeInit;
begin
  if not FInited then
    Exit;
  FInited:=False;
  if FHandle >= 0 then
    FpClose(FHandle);
  FHandle:=-1;
  SetLength(FTracks, 0);
end;

procedure TPAAudioCDSource.SetFirstTrack(AValue: Integer);
begin
  FFirstTrack:=AValue;
  if not Inited then
    Exit;
  FFirstFrame:=FTracks[AValue-1].frame;
  FFrameIndex:=FFirstFrame;
end;

procedure TPAAudioCDSource.SetLastTrack(AValue: Integer);
begin
  FLastTrack:=AValue;
  if not Inited then
    Exit;

  FLastFrame:=FTracks[AValue].frame;
end;

procedure TPAAudioCDSource.SetTrackCount(AValue: Integer);
begin
  if FTrackCount=AValue then Exit;
  FTrackCount:=Length(FTracks);
end;

function TPAAudioCDSource.GetTrack(AIndex: Integer): TTrackInfo;
begin
  if not Inited then
    Exit;
  Result := FTracks[AIndex];
end;

function TPAAudioCDSource.GetCDDBId: Integer;
begin
  Result := 0;
  if not Inited then
    Exit;

  Result := CDDBDiscID(FTracks, Length(FTracks));
end;

function FrameToCDAddr(AFrame: Integer): Tcdrom_addr;
var
  Secs: Integer;
begin
  Secs := AFrame div 75;
  with Result.msf do
  begin
    frame:= AFrame mod 75;
    minute:= Secs div 60;
    second:= Secs mod 60;

    WriteLn(Format('%d:%d.%d',[minute,second,frame]));
  end;
end;

function Min(A,B: Integer): Integer;
begin
  if A < B then
    Exit(A);
  Result := B;
end;

function TPAAudioCDSource.InternalOutputToDestination: Boolean;
const
  FRAME_SIZE = 2352;
var
  ra: Tcdrom_read_audio;
  buffer: array[0..3] of array [0..FRAME_SIZE-1] of byte;
  RCount: Integer;
  res: BaseUnix.cint;
begin
  Result := False;
  if not Inited then
    Exit;

  RCount := Min(FLastFrame-FFrameIndex, 4);

  ra.addr := FrameToCDAddr(FFrameIndex);
  ra.addr_format:=CDROM_MSF;
  ra.nframes:=RCount;
  ra.buf:=@buffer;

  if RCount > 0 then
  begin
    res := FpIOCtl(FHandle,CDROMREADAUDIO,@ra);
    if Res = 0 then
    begin
      Inc(FFrameIndex, ra.nframes);
      WriteToBuffer(buffer[0], ra.nframes*FRAME_SIZE, FFrameIndex < FLastFrame);
      Result := True;
    end;

  end;


end;

constructor TPAAudioCDSource.Create;
begin
  inherited Create;
  FHandle:=-1;
end;

destructor TPAAudioCDSource.Destroy;
begin
  DeInit;
  inherited Destroy;
end;
{$ENDIF}
end.

