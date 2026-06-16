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
  through, computes a Hann-windowed FFT spectrum.

  A new spectrum ("frame") is produced every FrameIntervalMS of *audio* (a
  sliding window with that hop), independent of how the audio buffers actually
  arrive. Frames are pushed into a thread-safe FIFO; the consumer PULLS them
  with PullFrame at whatever rate it likes (typically a GUI timer) and decides
  itself when to drop frames if it is falling behind (drain the FIFO, or check
  BufferedFrameCount). The FIFO is capped at MaxBufferedFrames so it can never
  grow without bound if nothing pulls.

  Set BandCount to 0 to get the full FFT magnitude spectrum (DC..Nyquist), or
  8..128 to get that many log-spaced band energies (what a bar meter wants). }

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

  { TPAFFTLink }

  TPAFFTLink = class(TPAAudioLink)
  private
    FWindowSize: Integer;
    FBandCount: Integer;
    FMode: TPAFFTChannelMode;
    FFrameIntervalMS: Integer;
    FMaxBufferedFrames: Integer;

    FNeedInit: Boolean;
    FFFT: TPAIOFFT;
    FBins: Integer;           // FFT bins produced (DC..Nyquist)
    FOutCount: Integer;       // values we emit: FBins, or FBandCount when banding
    FSrcChannels: Integer;    // source channel count, captured at init
    FSrcFormat: TPAAudioFormat; // source sample format, captured at init
    FRate: Integer;
    FTargets: Integer;        // number of spectra we emit
    FHopSamples: Integer;     // samples of audio between emitted frames

    // worker-thread scratch (touched only inside InternalProcessData)
    FWindow: array of Single;             // Hann window, length FWindowSize
    FFill: array of array of Single;      // [target] sliding ring of last samples
    FFillPos: array of Integer;           // [target] ring write position
    FFilled: array of Integer;            // [target] samples seen (capped at window)
    FHopAccum: array of Integer;          // [target] samples since last emit
    FWinBuf: array of Single;             // windowed frame fed to the FFT
    FTmpMag: array of Single;             // raw FFT magnitudes (FBins)
    FTmpOut: array of Single;             // emitted values (FOutCount)
    FBandLo: array of Integer;            // [band] first bin (inclusive)
    FBandHi: array of Integer;            // [band] last bin (inclusive)

    // frame FIFO, shared between worker (push) and consumer (pull)
    FFrameCrit: TRTLCriticalSection;
    FFrameChan: array of Integer;         // parallel FIFO arrays
    FFrameMag: array of array of Single;
    FFrameHead, FFrameCount: Integer;     // ring indices into the FIFO arrays

    procedure SetWindowSize(AValue: Integer);
    procedure SetBandCount(AValue: Integer);
    procedure SetMode(AValue: TPAFFTChannelMode);
    procedure SetFrameIntervalMS(AValue: Integer);
    procedure SetMaxBufferedFrames(AValue: Integer);
    procedure ComputeHop;
    procedure InitData;
    procedure BuildBands;
    procedure PushSample(ATarget: Integer; AValue: Single);
    procedure ComputeAndDeliver(ATarget: Integer);
    procedure PushFrame(AChannel: Integer);     // worker: enqueue FTmpOut
    procedure ClearFrames;
  protected
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; override;
    // Report the SOURCE's format so the framework does NOT convert data before
    // handing it to us -- keeps this a true passthrough (no S16->float32
    // doubling). We convert to float locally for the FFT.
    function  GetFormat: TPAAudioFormat; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    // Pop the oldest buffered frame. Call from your own timer at whatever rate
    // you want. Returns False when nothing is buffered, leaving AMagnitudes /
    // AChannel untouched (var, not out -- so `while PullFrame(c,m) do ...; use m`
    // keeps the last frame). On success AMagnitudes is sized to the number of
    // values (BinCount, or BandCount when banding); AChannel is the spectrum
    // index (0 for mono/first, source channel for per-channel).
    function  PullFrame(var AChannel: Integer; var AMagnitudes: TSingleArray): Boolean;
    // How many frames are waiting. Use it to drop when you're behind, e.g.
    //   while BufferedFrameCount > 2 do PullFrame(c, m); // skip stale frames
    function  BufferedFrameCount: Integer;

    // power of two, minimum 8 (2^3); set before audio starts flowing
    property WindowSize: Integer read FWindowSize write SetWindowSize;
    // 0 = full FFT spectrum; otherwise number of log-spaced bands (8..128)
    property BandCount: Integer read FBandCount write SetBandCount;
    property ChannelMode: TPAFFTChannelMode read FMode write SetMode;
    // Audio time between frames, in milliseconds: one spectrum is produced for
    // every FrameIntervalMS of audio, regardless of buffering. 0 (default) means
    // one frame per window (no overlap). Values smaller than the window duration
    // overlap; larger ones skip audio between frames.
    property FrameIntervalMS: Integer read FFrameIntervalMS write SetFrameIntervalMS;
    // FIFO cap: oldest frames are dropped past this so the buffer can't grow
    // unbounded if the consumer stops pulling. Default 64.
    property MaxBufferedFrames: Integer read FMaxBufferedFrames write SetMaxBufferedFrames;
    // source sample rate (for mapping bins to Hz); valid once audio is flowing
    property SampleRate: Integer read FRate;
  end;

implementation

uses
  Math;

{ TPAFFTLink }

constructor TPAFFTLink.Create;
begin
  inherited Create;
  InitCriticalSection(FFrameCrit);
  FWindowSize := 1024;
  FBandCount := 0;
  FMode := fcmMixToMono;
  FFrameIntervalMS := 0;
  FMaxBufferedFrames := 64;
  FNeedInit := True;
end;

destructor TPAFFTLink.Destroy;
begin
  // stop the worker thread (it uses FFFT/buffers) before tearing them down.
  DestroyWaitSync;
  FFFT.Free;
  DoneCriticalSection(FFrameCrit);
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

procedure TPAFFTLink.SetFrameIntervalMS(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  FFrameIntervalMS := AValue;
  ComputeHop; // applies immediately (only changes the hop)
end;

procedure TPAFFTLink.SetMaxBufferedFrames(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  FMaxBufferedFrames := AValue;
end;

procedure TPAFFTLink.ComputeHop;
begin
  if (FFrameIntervalMS <= 0) or (FRate <= 0) then
    FHopSamples := FWindowSize          // one frame per window (no overlap)
  else
    FHopSamples := Max(1, Round(FFrameIntervalMS * FRate / 1000.0));
end;

function TPAFFTLink.GetFormat: TPAAudioFormat;
begin
  if Assigned(DataSource) and (DataSource.GetSourceObject is IPAAudioInformation) then
    Result := (DataSource.GetSourceObject as IPAAudioInformation).GetFormat
  else
    Result := inherited GetFormat;
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
  // Forward in the source's own format (true passthrough). Pin the field
  // WriteToDestinations stamps onto buffers. We convert to float locally.
  FSrcFormat := Format;
  SetFormat(FSrcFormat);

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

  ComputeHop;

  // Hann window
  SetLength(FWindow, FWindowSize);
  for i := 0 to FWindowSize - 1 do
    FWindow[i] := 0.5 - 0.5 * Cos(2.0 * Pi * i / FWindowSize);

  SetLength(FWinBuf, FWindowSize);
  SetLength(FTmpMag, FBins);
  SetLength(FTmpOut, FOutCount);

  SetLength(FFill, FTargets);
  SetLength(FFillPos, FTargets);
  SetLength(FFilled, FTargets);
  SetLength(FHopAccum, FTargets);
  for i := 0 to FTargets - 1 do
  begin
    SetLength(FFill[i], FWindowSize);
    FFillPos[i] := 0;
    FFilled[i] := 0;
    FHopAccum[i] := 0;
  end;

  ClearFrames;
end;

procedure TPAFFTLink.PushSample(ATarget: Integer; AValue: Single);
begin
  // sliding ring of the most recent FWindowSize samples
  FFill[ATarget][FFillPos[ATarget]] := AValue;
  FFillPos[ATarget] := (FFillPos[ATarget] + 1) mod FWindowSize;
  if FFilled[ATarget] < FWindowSize then
    Inc(FFilled[ATarget]);
  Inc(FHopAccum[ATarget]);
  // emit once we have a full window and FHopSamples of audio have passed
  if (FFilled[ATarget] >= FWindowSize) and (FHopAccum[ATarget] >= FHopSamples) then
  begin
    Dec(FHopAccum[ATarget], FHopSamples);
    ComputeAndDeliver(ATarget);
  end;
end;

procedure TPAFFTLink.ComputeAndDeliver(ATarget: Integer);
var
  i, b, oldest: Integer;
  energy: Single;
begin
  // copy the ring out in chronological order (oldest sample is at FFillPos), and
  // apply the window.
  oldest := FFillPos[ATarget];
  for i := 0 to FWindowSize - 1 do
    FWinBuf[i] := FFill[ATarget][(oldest + i) mod FWindowSize] * FWindow[i];

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

  PushFrame(ATarget);
end;

procedure TPAFFTLink.PushFrame(AChannel: Integer);
var
  cap, idx, i: Integer;
begin
  EnterCriticalSection(FFrameCrit);
  try
    cap := Length(FFrameChan);
    if cap = 0 then
      Exit; // not allocated yet (shouldn't happen post-init)
    if FFrameCount >= FMaxBufferedFrames then
    begin
      // FIFO full: drop the oldest frame to make room.
      FFrameHead := (FFrameHead + 1) mod cap;
      Dec(FFrameCount);
    end;
    idx := (FFrameHead + FFrameCount) mod cap;
    FFrameChan[idx] := AChannel;
    SetLength(FFrameMag[idx], FOutCount);
    for i := 0 to FOutCount - 1 do
      FFrameMag[idx][i] := FTmpOut[i];
    Inc(FFrameCount);
  finally
    LeaveCriticalSection(FFrameCrit);
  end;
end;

function TPAFFTLink.PullFrame(var AChannel: Integer; var AMagnitudes: TSingleArray): Boolean;
var
  cap: Integer;
begin
  Result := False;
  EnterCriticalSection(FFrameCrit);
  try
    if FFrameCount = 0 then
      Exit; // leave AChannel/AMagnitudes as the caller had them

    cap := Length(FFrameChan);
    AChannel := FFrameChan[FFrameHead];
    AMagnitudes := Copy(FFrameMag[FFrameHead], 0, Length(FFrameMag[FFrameHead]));
    FFrameHead := (FFrameHead + 1) mod cap;
    Dec(FFrameCount);
    Result := True;
  finally
    LeaveCriticalSection(FFrameCrit);
  end;
end;

function TPAFFTLink.BufferedFrameCount: Integer;
begin
  EnterCriticalSection(FFrameCrit);
  Result := FFrameCount;
  LeaveCriticalSection(FFrameCrit);
end;

procedure TPAFFTLink.ClearFrames;
begin
  EnterCriticalSection(FFrameCrit);
  try
    // ring sized one larger than the cap so head/tail never collide
    SetLength(FFrameChan, FMaxBufferedFrames + 1);
    SetLength(FFrameMag, FMaxBufferedFrames + 1);
    FFrameHead := 0;
    FFrameCount := 0;
  finally
    LeaveCriticalSection(FFrameCrit);
  end;
end;

function TPAFFTLink.InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64;
var
  B: PAudioBuffer;
  pf: PSingle;
  ps: PSmallInt;
  frames, f, c, base: Integer;
  acc: Single;

  function Sample(AIndex: Integer): Single; inline;
  begin
    if FSrcFormat = afFloat32 then
      Result := pf[AIndex]
    else
      Result := ps[AIndex] * (1.0 / 32768.0); // afS16
  end;

begin
  if FNeedInit then
    InitData;

  // 1) forward the data downstream unchanged (true passthrough, native format).
  B := BufferPool.GetBufferFromPool(True, @FlushPendingSends);
  B^.Format      := Format;
  B^.UsedData    := ACount;
  B^.IsEndOfData := AIsLastData;
  Move(AData, B^.Data, ACount);
  WriteToDestinations(B);

  // 2) feed the analyser, converting to float locally. Only PCM formats can be
  // analysed; skip encoded (afRaw) data.
  if FSrcFormat in [afS16, afFloat32] then
  begin
    pf := PSingle(@AData);
    ps := PSmallInt(@AData);
    frames := (ACount div BytesPerSample(FSrcFormat)) div FSrcChannels;
    for f := 0 to frames - 1 do
    begin
      base := f * FSrcChannels;
      case FMode of
        fcmFirstChannel:
          PushSample(0, Sample(base));
        fcmPerChannel:
          for c := 0 to FSrcChannels - 1 do
            PushSample(c, Sample(base + c));
        else // fcmMixToMono
          begin
            acc := 0;
            for c := 0 to FSrcChannels - 1 do
              acc := acc + Sample(base + c);
            PushSample(0, acc / FSrcChannels);
          end;
      end;
    end;
  end;

  Result := ACount;
end;

end.
