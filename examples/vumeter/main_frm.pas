unit main_frm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_widget, fpg_form, fpg_label, fpg_button,
  pa_base, pa_fft;

type

  { TVUMeter
    A simple custom widget that draws a row of vertical bars, one per band. }

  TVUMeter = class(TfpgWidget)
  private
    FBars: array of Single;   // displayed level 0..1 (with smooth fall-off)
    FRef: Single;             // adaptive normalisation reference
  protected
    procedure HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBands(const AValues: array of Single); // raw band energies
    procedure Decay;                                     // ease bars toward 0
  end;

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    btnOpen: TfpgButton;
    lblFile: TfpgLabel;
    FMeter: TVUMeter;
    FPump: TfpgTimer;
    FSource: TPAAudioSource;
    FFFT: TPAFFTLink;
    FDest: TPAAudioDestination;
    FFileName: String;
    procedure btnOpenClick(Sender: TObject);
    procedure PumpTimer(Sender: TObject);
    procedure GotSpectrum(Sender: TObject; const AMagnitudes: array of Single;
      AChannel, ASampleRate: Integer);
    function  GetFilter: String;
    procedure OpenFile(AFileName: String);
  public
    procedure AfterCreate; override;
    procedure BeforeDestruction; override;
  end;

implementation

uses
  pa_dec_oggvorbis, pa_flac, pa_wav, pa_m4a, pa_register, pa_pulse_simple,
  fpg_dialogs;

const
  BANDS = 16;

{ TVUMeter }

constructor TVUMeter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetLength(FBars, BANDS);
  FRef := 1e-6;
end;

procedure TVUMeter.SetBands(const AValues: array of Single);
var
  i, n: Integer;
  v, mx: Single;
begin
  n := Length(AValues);
  if n > Length(FBars) then
    n := Length(FBars);

  // Track the loudest band so the meter auto-scales to the material instead of
  // needing a fixed gain: jump up instantly, ease back down.
  mx := 0;
  for i := 0 to n - 1 do
    if AValues[i] > mx then
      mx := AValues[i];
  if mx > FRef then
    FRef := mx
  else
    FRef := FRef * 0.95 + mx * 0.05;
  if FRef < 1e-6 then
    FRef := 1e-6;

  for i := 0 to n - 1 do
  begin
    v := AValues[i] / FRef;
    if v > 1 then
      v := 1;
    if v > FBars[i] then
      FBars[i] := v                       // rise instantly
    else
      FBars[i] := FBars[i] * 0.7 + v * 0.3; // fall smoothly
  end;
  Invalidate;
end;

procedure TVUMeter.Decay;
var
  i: Integer;
begin
  for i := 0 to High(FBars) do
    FBars[i] := FBars[i] * 0.85;
  Invalidate;
end;

procedure TVUMeter.HandlePaint;
var
  i, gap, bw, x, bh: Integer;
  c: TfpgColor;
begin
  Canvas.Clear(clBlack);
  if Length(FBars) = 0 then
    Exit;
  gap := 3;
  bw := (Width - gap * (Length(FBars) + 1)) div Length(FBars);
  if bw < 1 then
    bw := 1;
  for i := 0 to High(FBars) do
  begin
    x  := gap + i * (bw + gap);
    bh := Round(FBars[i] * Height);
    if FBars[i] >= 0.85 then
      c := clRed
    else if FBars[i] >= 0.6 then
      c := clYellow
    else
      c := clGreen;
    Canvas.SetColor(c);
    Canvas.FillRectangle(x, Height - bh, bw, bh);
  end;
end;

{ TMainForm }

procedure TMainForm.GotSpectrum(Sender: TObject; const AMagnitudes: array of Single;
  AChannel, ASampleRate: Integer);
begin
  // Runs on the main thread (the FFT link marshals via Queue and PumpTimer
  // pumps the queue). NOTE: this spectrum is for audio that is still buffered
  // downstream in PulseAudio, so visually it leads the speakers by the output
  // latency. Shrink the Pulse buffer or delay the display to tighten sync.
  FMeter.SetBands(AMagnitudes);
end;

procedure TMainForm.PumpTimer(Sender: TObject);
begin
  // fpGUI's loop doesn't pump TThread.Queue for us, so do it here. This is what
  // actually delivers GotSpectrum on the main thread.
  CheckSynchronize(0);
  if (FDest = nil) or (not FDest.Working) then
    FMeter.Decay; // drain the bars when nothing is playing
end;

function TMainForm.GetFilter: String;
var
  Items, Extentions: TStrings;
  i: Integer;
  AllAudioFilter, EachFilter: String;
begin
  Extentions := TStringList.Create;
  Items := PARegisteredGetList(partDecoder, Extentions);
  if Assigned(Items) then
  begin
    for i := 0 to Items.Count - 1 do
    begin
      if i > 0 then
        AllAudioFilter := AllAudioFilter + ';';
      AllAudioFilter := AllAudioFilter + '*' + Extentions[i];
      EachFilter := EachFilter + '|' + Items[i] + '|*' + Extentions[i];
    end;
    Items.Free;
  end;
  Extentions.Free;
  Result := 'Audio files|' + AllAudioFilter + EachFilter + '|All files|*';
end;

procedure TMainForm.OpenFile(AFileName: String);
begin
  // detach the sink, then rebuild source + FFT link for the new file (channel
  // count / sample rate may differ, and the link captures them at first data).
  if Assigned(FDest) then
    FDest.DataSource := nil;
  FreeAndNil(FSource);
  FreeAndNil(FFFT);

  FSource := PARegisteredGetDecoderClass(AFileName, False).Create(
               TFileStream.Create(AFileName, fmOpenRead));

  FFFT := TPAFFTLink.Create;
  FFFT.BandCount   := BANDS;          // 16 log-spaced bars
  FFFT.ChannelMode := fcmMixToMono;
  FFFT.OnSpectrum  := @GotSpectrum;

  if not Assigned(FDest) then
    FDest := PARegisteredGetDeviceOut('').Create;

  // chain: decoder -> FFT analyser -> audio out
  FFFT.DataSource := FSource;
  FDest.DataSource := FFFT;
  FSource.StartData;
end;

procedure TMainForm.btnOpenClick(Sender: TObject);
var
  dlg: TfpgFileDialog;
begin
  dlg := TfpgFileDialog.Create(Self);
  if FFileName <> '' then
    dlg.InitialDir := ExtractFileDir(FFileName)
  else
    dlg.InitialDir := GetUserDir;
  dlg.Filter := GetFilter;
  if dlg.RunOpenFile then
  begin
    dlg.Close;
    FFileName := dlg.FileName;
    OpenFile(dlg.FileName);
    lblFile.Text := ExtractFileName(dlg.FileName);
  end;
  dlg.Free;
end;

procedure TMainForm.AfterCreate;
begin
  Name := 'MainForm';
  SetPosition(300, 200, 430, 280);
  WindowTitle := 'PascalAudio - 16-band VU Meter';

  btnOpen := TfpgButton.Create(self);
  with btnOpen do
  begin
    Name := 'btnOpen';
    SetPosition(10, 10, 90, 26);
    Text := 'Open File';
    OnClick := @btnOpenClick;
  end;

  lblFile := TfpgLabel.Create(self);
  with lblFile do
  begin
    Name := 'lblFile';
    SetPosition(110, 14, 310, 18);
    Anchors := [anLeft, anRight, anTop];
    Text := '(no file)';
  end;

  FMeter := TVUMeter.Create(self);
  with FMeter do
  begin
    Name := 'FMeter';
    SetPosition(10, 46, 410, 224);
    Anchors := [anLeft, anRight, anTop, anBottom];
  end;

  // ~33 fps: pumps queued spectra and animates fall-off
  FPump := TfpgTimer.Create(30);
  FPump.OnTimer := @PumpTimer;
  FPump.Enabled := True;
end;

procedure TMainForm.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if Assigned(FDest) then
    FDest.DataSource := nil;
  FreeAndNil(FSource);
  FreeAndNil(FFFT);
  FreeAndNil(FDest);
end;

end.
