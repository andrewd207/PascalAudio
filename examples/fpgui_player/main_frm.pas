unit main_frm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_label, fpg_progressbar, fpg_button, pa_base, pa_stream;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    Label1: TfpgLabel;
    btnOpen: TfpgButton;
    ProgressBar1: TfpgProgressBar;
    lblPosition: TfpgLabel;
    lblTotal: TfpgLabel;
    {@VFD_HEAD_END: MainForm}
    ProgTimer: TfpgTimer;
    FSource: TPAStreamSource;
    FDest: TPAAudioDestination;
    FFileName: String;

    procedure btnOpenClick(Sender: TObject);
    procedure ProgressMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ProgTimerTimer(Sender: TObject);
    function  GetFilter: String;
    procedure OpenFile(AFileName: String);
  public
    procedure AfterCreate; override;
    procedure BeforeDestruction; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  pa_dec_oggvorbis,
  pa_flac,
  pa_wav,
  pa_register,
  pa_pulse_simple,
  fpg_dialogs;

{@VFD_NEWFORM_IMPL}

function SecondsToTime(ASeconds: Double): String;
var
  Hours,
  Minutes,
  Seconds: Integer;
  function Pad(AInteger: Integer): String;
  begin
    Result := IntToStr(AInteger);
    if Length(Result) < 2 then
      Result := '0'+Result;
  end;

begin
  Hours := Trunc(ASeconds) div 3600;
  Minutes := Trunc(ASeconds) div 60 mod 60;
  Seconds:= Trunc(ASeconds) mod 60;

  if Hours > 0 then
    Result := Pad(Hours)+':'
  else
    Result := '';

  Result := Result + Pad(Minutes)+':'+Pad(Seconds);


end;

procedure TMainForm.ProgTimerTimer(Sender: TObject);
var
  Playable: IPAPlayable;
  PosMax,
  PosCurrent: Double;
begin
  if Assigned(FSource) then
  if FSource.GetInterface('IPAPlayable', Playable) then
  begin
    PosMax := Playable.GetMaxPosition;
    PosCurrent:=Playable.GetPosition;
    ProgressBar1.Max:=Trunc(PosMax*100);
    ProgressBar1.Position:=Trunc(PosCurrent*100);

    lblPosition.Text := SecondsToTime(PosCurrent);
    lblTotal.Text:=SecondsToTime(PosMax);
  end;
end;

function TMainForm.GetFilter: String;
var
  Items: TStrings;
  Extentions: TStrings;
  i: Integer;
  AllAudioFilter: String;
  EachFilter: String;
begin
  Extentions := TStringList.Create;
  Items := PARegisteredGetList(partDecoder, Extentions);

  if Assigned(Items) then
  begin
    for i := 0 to Items.Count-1 do
    begin
      if i > 0 then
        AllAudioFilter := AllAudioFilter +';';
      AllAudioFilter := AllAudioFilter + '*'+Extentions[i];
      EachFilter:=EachFilter+'|'+Items[I]+'|*'+Extentions[i];

    end;
    Items.Free;
  end;
  Extentions.Free;

  Result := 'Audio files|'+AllAudioFilter+EachFilter+'|All files|*';

end;

procedure TMainForm.OpenFile(AFileName: String);
begin
  if Assigned(FSource) then
  begin
    FSource.Free;
    //FDest.Free;
  end;

  FSource := PARegisteredGetDecoderClass(AFileName, False).Create(TFileStream.Create(AFileName, fmOpenRead));

  if not Assigned(FDest) then
    FDest   := PARegisteredGetDeviceOut('').Create;
  FDest.DataSource := FSource;
  FSource.StartData;
  ProgTimer.Enabled:=True;
end;

procedure TMainForm.btnOpenClick(Sender: TObject);
var
  dlg: TfpgFileDialog;
begin
  dlg := TfpgFileDialog.Create(Self);
  if FFileName <> '' then
    dlg.InitialDir := ExtractFileDir(FFileName)
  else
    dlg.InitialDir:=GetUserDir;
  dlg.Filter:=GetFilter;

  if dlg.RunOpenFile then
  begin
    dlg.Close;
    FFileName:=dlg.FileName;
    OpenFile(dlg.FileName);
    Label1.Text:=dlg.FileName;
  end;

  dlg.Free;

end;

procedure TMainForm.ProgressMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
var
  Percent: Double;
  Playable: IPAPlayable;
begin
  if Assigned(FSource) then
  if FSource.GetInterface('IPAPlayable', Playable) then
  begin
    Percent:=AMousePos.x / ProgressBar1.Width;
    Playable.SetPosition(Playable.GetMaxPosition*Percent);
  end;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(960, 231, 380, 211);
  WindowTitle := 'MainForm';
  Hint := '';
  IconName := '';

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(20, 55, 345, 15);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  btnOpen := TfpgButton.Create(self);
  with btnOpen do
  begin
    Name := 'btnOpen';
    SetPosition(285, 20, 80, 23);
    Anchors := [anRight,anTop];
    Text := 'Open File';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick:=@btnOpenClick;
  end;

  ProgressBar1 := TfpgProgressBar.Create(self);
  with ProgressBar1 do
  begin
    Name := 'ProgressBar1';
    SetPosition(20, 80, 345, 22);
    Anchors := [anLeft,anRight,anTop];
    Hint := 'Click to seek';
    ParentShowHint := False;
    ShowHint := True;
    OnMouseDown:=@ProgressMouseDown;
  end;

  lblPosition := TfpgLabel.Create(self);
  with lblPosition do
  begin
    Name := 'lblPosition';
    SetPosition(20, 110, 80, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'CurPos';
  end;

  lblTotal := TfpgLabel.Create(self);
  with lblTotal do
  begin
    Name := 'lblTotal';
    SetPosition(285, 110, 80, 15);
    Anchors := [anRight,anTop];
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Total';
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
  ProgTimer := TfpgTimer.Create(100);
  ProgTimer.OnTimer:=@ProgTimerTimer;
end;

procedure TMainForm.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if Assigned(FSource) then
    FreeAndNil(FSource);
  if Assigned(FDest) then
    FreeAndNil(FDest);
end;

end.

