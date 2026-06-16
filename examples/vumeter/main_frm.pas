unit main_frm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_widget, fpg_form, fpg_label, fpg_button,
  pa_base, pa_fft;

type

  // How the meter auto-scales each frame:
  //   nmPerBand - every bar scales against its own running peak (loud bass won't
  //               flatten the quieter highs; shows spectral shape/activity).
  //   nmPerMax  - all bars scale against one shared peak across the whole frame
  //               (preserves the real loudness balance between bands).
  TVUNormMode = (nmPerBand, nmPerMax);

  { TVUMeter
    A simple custom widget that draws a row of vertical bars, one per band. }

  TVUMeter = class(TfpgWidget)
  private
    FBars: array of Single;   // displayed level 0..1 (with snappy fall-off)
    FRef: array of Single;    // per-band adaptive normalisation reference
    FGlobalRef: Single;       // shared reference used in nmPerMax mode
    FNormMode: TVUNormMode;
  protected
    procedure HandlePaint; override;
    procedure HandleResize(AWidth, AHeight: TfpgCoord); override; // repaint on resize
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBands(const AValues: array of Single); // update bars from a spectrum
    procedure Decay;                                    // ease bars toward 0 when idle
    property NormMode: TVUNormMode read FNormMode write FNormMode;
  end;

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    btnOpen: TfpgButton;
    btnNorm: TfpgButton;
    lblFile: TfpgLabel;
    FMeter: TVUMeter;
    FPump: TfpgTimer;
    FSource: TPAAudioSource;
    FFFT: TPAFFTLink;
    FDest: TPAAudioDestination;
    FFileName: String;
    procedure btnOpenClick(Sender: TObject);
    procedure btnNormClick(Sender: TObject);
    procedure UpdateNormButton;
    procedure PumpTimer(Sender: TObject);
    function  GetFilter: String;
    procedure OpenFile(AFileName: String);
  public
    procedure AfterCreate; override;
    procedure BeforeDestruction; override;
  end;

implementation

uses
  pa_dec_oggvorbis, pa_flac, pa_wav, pa_m4a, pa_register, pa_pulse,
  fpg_dialogs;

const
  BANDS = 128;

{ TVUMeter }

constructor TVUMeter.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  SetLength(FBars, BANDS);
  SetLength(FRef, BANDS);
  for i := 0 to High(FRef) do
    FRef[i] := 1e-6;
  FGlobalRef := 1e-6;
  FNormMode := nmPerBand;
end;

procedure TVUMeter.SetBands(const AValues: array of Single);
var
  i, n: Integer;
  v, mx, ref: Single;
begin
  n := Length(AValues);
  if n > Length(FBars) then
    n := Length(FBars);

  // In nmPerMax, derive one shared reference from the loudest band this frame
  // (snap up, ease down). nmPerBand updates each band's own reference below.
  if FNormMode = nmPerMax then
  begin
    mx := 0;
    for i := 0 to n - 1 do
      if AValues[i] > mx then
        mx := AValues[i];
    if mx > FGlobalRef then
      FGlobalRef := mx
    else
      FGlobalRef := FGlobalRef * 0.80 + mx * 0.20;
    if FGlobalRef < 1e-6 then
      FGlobalRef := 1e-6;
  end;

  for i := 0 to n - 1 do
  begin
    if FNormMode = nmPerBand then
    begin
      // each bar scales against its OWN running peak, so loud bass bands don't
      // set the reference for the whole meter and flatten the quieter highs.
      if AValues[i] > FRef[i] then
        FRef[i] := AValues[i]
      else
        FRef[i] := FRef[i] * 0.80 + AValues[i] * 0.20;
      if FRef[i] < 1e-6 then
        FRef[i] := 1e-6;
      ref := FRef[i];
    end
    else
      ref := FGlobalRef;

    v := AValues[i] / ref;
    if v > 1 then
      v := 1;
    if v > FBars[i] then
      FBars[i] := v                        // rise instantly to catch transients
    else
      FBars[i] := FBars[i] * 0.4 + v * 0.6; // snappy fall
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
  i, gap, bw, x, bh, w, h: Integer;
  c: TfpgColor;
begin
  Canvas.Clear(clBlack);
  if Length(FBars) = 0 then
    Exit;
  // Use ActualWidth/Height, not Width/Height: the latter return the *preferred*
  // size, which doesn't change on a runtime (anchor) resize, so the bars would
  // keep drawing at the original dimensions.
  w := ActualWidth;
  h := ActualHeight;
  // Adaptive gap: 3 px looks good with few bars, but with many bars (e.g. 128)
  // fixed gaps eat the whole width. Drop the gap to keep the bars visible, and
  // to 0 once they'd otherwise vanish.
  gap := 3;
  while (gap > 0) and ((w - gap * (Length(FBars) + 1)) div Length(FBars) < 2) do
    Dec(gap);
  bw := (w - gap * (Length(FBars) + 1)) div Length(FBars);
  if bw < 1 then
    bw := 1;
  for i := 0 to High(FBars) do
  begin
    x  := gap + i * (bw + gap);
    bh := Round(FBars[i] * h);
    if FBars[i] >= 0.85 then
      c := clRed
    else if FBars[i] >= 0.6 then
      c := clYellow
    else
      c := clGreen;
    Canvas.SetColor(c);
    Canvas.FillRectangle(x, h - bh, bw, bh);
  end;
end;

procedure TVUMeter.HandleResize(AWidth, AHeight: TfpgCoord);
begin
  inherited HandleResize(AWidth, AHeight);
  // bar widths/heights are derived from Width/Height in HandlePaint, so force a
  // full repaint whenever the anchors stretch us -- otherwise we keep drawing at
  // the old size.
  Invalidate;
end;

{ TMainForm }

procedure TMainForm.PumpTimer(Sender: TObject);
var
  ch: Integer;
  mag: TSingleArray;
  got: Boolean;
begin
  // The FFT link produces one frame per FrameIntervalMS of *audio*, but they
  // arrive in bursts (Pulse hands us data in chunks). The FIFO is our jitter
  // buffer: consume exactly ONE frame per tick so a burst plays out smoothly
  // over the following ticks instead of collapsing into a single update. Only
  // drop frames when we've genuinely fallen behind, to stay synced to "now".
  ch := 0;
  mag := nil;
  got := False;
  if Assigned(FFFT) then
  begin
    // catch up: if the backlog is deep we're behind playback, so discard the
    // stale frames and keep latency low (leave a couple as cushion).
    while FFFT.BufferedFrameCount > 2 do
      FFFT.PullFrame(ch, mag);
    got := FFFT.PullFrame(ch, mag); // then show a single frame this tick
  end;
  if got then
    FMeter.SetBands(mag)
  else if (FDest = nil) or (not FDest.Working) then
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
var
  Pulse: TPAPulseAsyncDestination;
begin
  // Rebuild the whole chain for the new file. The sink is recreated too: reusing
  // a Pulse stream after it drained on the previous song's end-of-data leaves it
  // corked/with stale buffering, so the second song pulled data slowly (choppy).
  if Assigned(FDest) then
    FDest.DataSource := nil;
  FreeAndNil(FSource);
  FreeAndNil(FFFT);
  FreeAndNil(FDest);

  FSource := PARegisteredGetDecoderClass(AFileName, False).Create(
               TFileStream.Create(AFileName, fmOpenRead));

  FFFT := TPAFFTLink.Create;
  FFFT.BandCount       := BANDS;        // 16 log-spaced bars
  FFFT.ChannelMode     := fcmMixToMono;
  FFFT.FrameIntervalMS := 16;           // one spectrum per 16 ms of audio (~60 fps)
  // The timer pulls frames; we never need the buffer to grow much.
  FFFT.MaxBufferedFrames := 16;

  if not Assigned(FDest) then
  begin
    // Use the async Pulse sink with a small buffer: the default (registered
    // simple) sink buffers ~0.5 s, so it pulls data in big bursts and the
    // analyser produces spectra only ~twice a second (choppy meter). A small
    // LatencyMS makes Pulse pull often, so spectra arrive smoothly.
    Pulse := TPAPulseAsyncDestination.Create;
    Pulse.LatencyMS := 40;
    FDest := Pulse;
  end;

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

procedure TMainForm.UpdateNormButton;
begin
  if FMeter.NormMode = nmPerBand then
    btnNorm.Text := 'Norm: Per-band'
  else
    btnNorm.Text := 'Norm: Per-max';
end;

procedure TMainForm.btnNormClick(Sender: TObject);
begin
  if FMeter.NormMode = nmPerBand then
    FMeter.NormMode := nmPerMax
  else
    FMeter.NormMode := nmPerBand;
  UpdateNormButton;
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

  btnNorm := TfpgButton.Create(self);
  with btnNorm do
  begin
    Name := 'btnNorm';
    SetPosition(106, 10, 130, 26);
    OnClick := @btnNormClick;
  end;

  lblFile := TfpgLabel.Create(self);
  with lblFile do
  begin
    Name := 'lblFile';
    SetPosition(244, 14, 176, 18);
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
  UpdateNormButton; // label the toggle from the meter's initial mode

  // ~60 fps: pumps queued spectra at the frame rate
  FPump := TfpgTimer.Create(16);
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
