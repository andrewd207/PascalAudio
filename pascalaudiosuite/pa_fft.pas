{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

{ TPAFFTLink is a middle link that forwards audio unchanged and, as data flows
  through, computes a Hann-windowed FFT and emits a magnitude spectrum via
  OnSpectrum. Intended for spectrum analysers / VU-style bar meters.

  Set BandCount to 0 to receive the full FFT magnitude spectrum (DC..Nyquist).
  Set it to a value in [8..128] to receive that many log-spaced bands instead,
  which is what a bar meter with a fixed number of bars wants.

  The result is marshalled to the main thread (via Queue) and coalesced, so the
  handler is GUI-safe and won't be flooded when FFT frames arrive faster than
  the GUI can draw. As with every queued event in the suite, the main thread
  must pump the synchronize queue (CheckSynchronize or an LCL/fpGUI loop). }

unit pa_fft;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pa_base, paio_fft;

const
  PAFFT_MIN_BANDS = 8;
  PAFFT_MAX_BANDS = 128;

type
  // how multi-channel audio is reduced before the FFT
  TPAFFTChannelMode = (
    fcmMixToMono,     // average all channels -> one spectrum (channel 0)
    fcmFirstChannel,  // analyse channel 0 only -> one spectrum
    fcmPerChannel     // one spectrum per source channel
  );

  // AMagnitudes holds either the full FFT bins (DC..Nyquist) when BandCount = 0,
  // or BandCount log-spaced band energies when BandCount > 0. AChannel is the
  // spectrum index (0 for mono/first-channel modes, the source channel for
  // per-channel mode).
  TPAFFTSpectrumEvent = procedure(Sender: TObject; const AMagnitudes: array of Single;
    AChannel: Integer; ASampleRate: Integer) of object;

  { TPAFFTLink }

  TPAFFTLink = class(TPAAudioLink)
  private
    FWindowSize: Integer;
    FBandCount: Integer;
    FMode: TPAFFTChannelMode;
    FOnSpectrum: TPAFFTSpectrumEvent;

    FNeedInit: Boolean;
    FFFT: TPAIOFFT;
    FBins: Integer;           // FFT bins produced (DC..Nyquist)
    FOutCount: Integer;       // values we emit: FBins, or FBandCount when banding
    FSrcChannels: Integer;       // source channel count, captured at init
    FRate: Integer;
    FTargets: Integer;        // number of spectra we emit

    // worker-thread scratch (touched only inside InternalProcessData)
    FWindow: array of Single;             // Hann window, length FWindowSize
    FFill: array of array of Single;      // [target] accumulation buffers
    FFillPos: array of Integer;           // [target] samples accumulated
    FWinBuf: array of Single;             // windowed frame fed to the FFT
    FTmpMag: array of Single;             // raw FFT magnitudes (FBins)
    FTmpOut: array of Single;             // emitted values (FOutCount)
    FBandLo: array of Integer;            // [band] first bin (inclusive)
    FBandHi: array of Integer;            // [band] last bin (inclusive)

    // shared between worker and main thread, guarded by FCrit
    FCrit: TRTLCriticalSection;
    FMag: array of array of Single;       // [target] latest emitted values
    FReady: array of Boolean;             // [target] fresh spectrum pending
    FQueued: Boolean;                     // a DispatchSpectra is already queued

    procedure SetWindowSize(AValue: Integer);
    procedure SetBandCount(AValue: Integer);
    procedure SetMode(AValue: TPAFFTChannelMode);
    procedure InitData;
    procedure BuildBands;
    procedure PushSample(ATarget: Integer; AValue: Single);
    procedure ComputeAndDeliver(ATarget: Integer);
    procedure DispatchSpectra; // runs on the main thread
  protected
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; override;
    function  GetFormat: TPAAudioFormat; override; // force float32 input
  public
    constructor Create; override;
    destructor  Destroy; override;
    // power of two, minimum 8 (2^3); set before audio starts flowing
    property WindowSize: Integer read FWindowSize write SetWindowSize;
    // 0 = full FFT spectrum; otherwise number of log-spaced bands (8..128)
    property BandCount: Integer read FBandCount write SetBandCount;
    property ChannelMode: TPAFFTChannelMode read FMode write SetMode;
    property OnSpectrum: TPAFFTSpectrumEvent read FOnSpectrum write FOnSpectrum;
  end;

implementation

uses
  Math;

{ TPAFFTLink }

constructor TPAFFTLink.Create;
begin
  inherited Create;
  InitCriticalSection(FCrit);
  FWindowSize := 1024;
  FBandCount := 0;
  FMode := fcmMixToMono;
  FNeedInit := True;
end;

destructor TPAFFTLink.Destroy;
begin
  // stop the worker thread (it uses FFFT/buffers) before tearing them down,
  // and drop any spectrum dispatch still queued for the main thread.
  DestroyWaitSync;
  TThread.RemoveQueuedEvents(Self);
  FFFT.Free;
  DoneCriticalSection(FCrit);
  inherited Destroy;
end;

procedure TPAFFTLink.SetWindowSize(AValue: Integer);
begin
  if AValue = FWindowSize then
    Exit;
  if not IsPowerOfTwo(AValue) then
    raise Exception.CreateFmt('TPAFFTLink: WindowSize %d is not a power of two', [AValue]);
  if AValue < 8 then
    raise Exception.CreateFmt('TPAFFTLink: WindowSize %d is below the minimum of 8 (2^3)', [AValue]);
  FWindowSize := AValue;
  FNeedInit := True;
end;

procedure TPAFFTLink.SetBandCount(AValue: Integer);
begin
  if AValue = FBandCount then
    Exit;
  if (AValue <> 0) and ((AValue < PAFFT_MIN_BANDS) or (AValue > PAFFT_MAX_BANDS)) then
    raise Exception.CreateFmt('TPAFFTLink: BandCount %d must be 0 or within %d..%d',
      [AValue, PAFFT_MIN_BANDS, PAFFT_MAX_BANDS]);
  FBandCount := AValue;
  FNeedInit := True;
end;

procedure TPAFFTLink.SetMode(AValue: TPAFFTChannelMode);
begin
  if AValue = FMode then
    Exit;
  FMode := AValue;
  FNeedInit := True;
end;

function TPAFFTLink.GetFormat: TPAAudioFormat;
begin
  Result := afFloat32;
end;

procedure TPAFFTLink.BuildBands;
var
  b, lo, hi, lastHi, maxBin: Integer;
  logLo, logHi: Double;
begin
  // Log-spaced bands across bins 1..(FBins-1); bin 0 (DC) is excluded. Each band
  // is clamped to advance by at least one bin so low bands are never empty.
  SetLength(FBandLo, FBandCount);
  SetLength(FBandHi, FBandCount);
  maxBin := FBins - 1;
  logLo := Ln(1);
  logHi := Ln(maxBin);
  lastHi := 0;
  for b := 0 to FBandCount - 1 do
  begin
    lo := Round(Exp(logLo + (logHi - logLo) * b / FBandCount));
    hi := Round(Exp(logLo + (logHi - logLo) * (b + 1) / FBandCount)) - 1;
    if lo <= lastHi then
      lo := lastHi + 1;
    if hi < lo then
      hi := lo;
    if hi > maxBin then
      hi := maxBin;
    if lo > maxBin then
      lo := maxBin;
    FBandLo[b] := lo;
    FBandHi[b] := hi;
    lastHi := hi;
  end;
end;

procedure TPAFFTLink.InitData;
var
  i: Integer;
begin
  FNeedInit := False;

  FSrcChannels := Channels;
  if FSrcChannels < 1 then
    FSrcChannels := 1;
  FRate := SamplesPerSecond;

  if FMode = fcmPerChannel then
    FTargets := FSrcChannels
  else
    FTargets := 1;

  FreeAndNil(FFFT);
  FFFT := TPAIOFFT.Create(FWindowSize);
  FBins := FFFT.BinCount;

  if FBandCount > 0 then
  begin
    FOutCount := FBandCount;
    BuildBands;
  end
  else
    FOutCount := FBins;

  // Hann window
  SetLength(FWindow, FWindowSize);
  for i := 0 to FWindowSize - 1 do
    FWindow[i] := 0.5 - 0.5 * Cos(2.0 * Pi * i / FWindowSize);

  SetLength(FWinBuf, FWindowSize);
  SetLength(FTmpMag, FBins);
  SetLength(FTmpOut, FOutCount);

  SetLength(FFill, FTargets);
  SetLength(FFillPos, FTargets);
  for i := 0 to FTargets - 1 do
  begin
    SetLength(FFill[i], FWindowSize);
    FFillPos[i] := 0;
  end;

  EnterCriticalSection(FCrit);
  try
    SetLength(FMag, FTargets);
    SetLength(FReady, FTargets);
    for i := 0 to FTargets - 1 do
    begin
      SetLength(FMag[i], FOutCount);
      FReady[i] := False;
    end;
    FQueued := False;
  finally
    LeaveCriticalSection(FCrit);
  end;
end;

procedure TPAFFTLink.PushSample(ATarget: Integer; AValue: Single);
begin
  FFill[ATarget][FFillPos[ATarget]] := AValue;
  Inc(FFillPos[ATarget]);
  if FFillPos[ATarget] = FWindowSize then
  begin
    FFillPos[ATarget] := 0;
    ComputeAndDeliver(ATarget);
  end;
end;

procedure TPAFFTLink.ComputeAndDeliver(ATarget: Integer);
var
  i, b: Integer;
  energy: Single;
begin
  for i := 0 to FWindowSize - 1 do
    FWinBuf[i] := FFill[ATarget][i] * FWindow[i];

  FFFT.ForwardReal(@FWinBuf[0], @FTmpMag[0]);

  if FBandCount > 0 then
  begin
    // each band = RMS energy of its bins (sqrt of summed magnitude squares)
    for b := 0 to FOutCount - 1 do
    begin
      energy := 0;
      for i := FBandLo[b] to FBandHi[b] do
        energy := energy + FTmpMag[i] * FTmpMag[i];
      FTmpOut[b] := Sqrt(energy);
    end;
  end
  else
    for i := 0 to FOutCount - 1 do
      FTmpOut[i] := FTmpMag[i];

  EnterCriticalSection(FCrit);
  try
    for i := 0 to FOutCount - 1 do
      FMag[ATarget][i] := FTmpOut[i];
    FReady[ATarget] := True;
    if not FQueued then
    begin
      FQueued := True;
      Queue(@DispatchSpectra);
    end;
  finally
    LeaveCriticalSection(FCrit);
  end;
end;

procedure TPAFFTLink.DispatchSpectra;
var
  snap: array of array of Single;
  pending: array of Boolean;
  rate, n, ch: Integer;
begin
  EnterCriticalSection(FCrit);
  try
    n := FTargets;
    rate := FRate;
    SetLength(snap, n);
    SetLength(pending, n);
    for ch := 0 to n - 1 do
    begin
      pending[ch] := FReady[ch];
      if FReady[ch] then
      begin
        snap[ch] := Copy(FMag[ch], 0, FOutCount);
        FReady[ch] := False;
      end;
    end;
    FQueued := False;
  finally
    LeaveCriticalSection(FCrit);
  end;

  if not Assigned(FOnSpectrum) then
    Exit;
  for ch := 0 to n - 1 do
    if pending[ch] then
      FOnSpectrum(Self, snap[ch], ch, rate);
end;

function TPAFFTLink.InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64;
var
  B: PAudioBuffer;
  Samples: PSingle;
  frames, f, c, base: Integer;
  acc: Single;
begin
  if FNeedInit then
    InitData;

  // 1) forward the data downstream unchanged (passthrough)
  B := BufferPool.GetBufferFromPool(True);
  B^.Format      := Format;
  B^.UsedData    := ACount;
  B^.IsEndOfData := AIsLastData;
  Move(AData, B^.Data, ACount);
  WriteToDestinations(B);

  // 2) feed the analyser. Data is float32 (GetFormat forces conversion).
  Samples := PSingle(@AData);
  frames := (ACount div SizeOf(Single)) div FSrcChannels;
  for f := 0 to frames - 1 do
  begin
    base := f * FSrcChannels;
    case FMode of
      fcmFirstChannel:
        PushSample(0, Samples[base]);
      fcmPerChannel:
        for c := 0 to FSrcChannels - 1 do
          PushSample(c, Samples[base + c]);
      else // fcmMixToMono
        begin
          acc := 0;
          for c := 0 to FSrcChannels - 1 do
            acc := acc + Samples[base + c];
          PushSample(0, acc / FSrcChannels);
        end;
    end;
  end;

  Result := ACount;
end;

end.
