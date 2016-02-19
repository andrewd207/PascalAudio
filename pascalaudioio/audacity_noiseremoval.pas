{
 audacity which this file was converted from is GPLv2
 http://www.audacityteam.org/about/license/
 }
unit audacity_noiseremoval;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, audacity_realfftf;//realfft_c;

type

  PPSingle=^PSingle;
  TSingleArray = array of Single;

  TNoiseRemoval = class;
  TNoiseWriteProc = procedure(ASender: TNoiseRemoval; AData: PSingle; ASampleCount: Integer) of Object;

  { TNoiseRemoval }

  TNoiseRemoval = class
  private
    FDoProfile: Boolean;
    FHasProfile: Boolean;

    // Parameters chosen before the first phase
    FSampleRate: Double;
    FWindowSize: Integer;
    FSpectrumSize: Integer;
    FMinSignalTime: Single; // in secs

    // The frequency-indexed noise threshold derived during the first
    // phase of analysis
    FNoiseThreshold: array of Single; // length in FSpectrumSize

    // Parameters that affect the noise removal, regardless of how the
    // noise profile was extracted
    FSensitivity: Double;
    FFreqSmoothingHz: Double;
    FNoiseGain: Double;      // in dB, should be negative
    FAttackDecayTime: Double;
    FbLeaveNoise: Boolean;

    // change this later
    procedure Initialize;
    procedure Reset; // StartNewTrack
    procedure ProcessSamples(len: Integer; Buffer:PSingle);
    procedure FillFirstHistoryWindow;
    procedure ApplyFreqSmoothing(ASpec: PSingle);
    procedure GetProfile;
    procedure RemoveNoise;
    procedure RotateHistoryWindows;
    procedure FinishTrack;
    procedure Cleanup;
  private
    //FOutputTrack: PSingle; // WaveTrack;
    FInSampleCount: Integer;
    FOutSampleCount: Integer;
    FInputPos: Integer;
    FFFT: PFFT;
    FFFTBuffer: PSingle;             // FWindowSize
    FWindow: PSingle;                // FWindowSize
    FFreqSmoothingBins: Integer;
    FAttackDecayBlocks: Integer;
    FOneBlockAttackDecay: Single;
    FNoiseAttenFactor: Single;
    FSensitivityFactor: Single;
    FMinSignalBlocks: Integer;
    FHistoryLen: Integer;
    FInWaveBuffer: PSingle;          // FWindowSize
    FOutOverlapBuffer: PSingle;      // FWindowSize
    FSpectrums:  array of PSingle;   // FHistoryLen x FSpectrumSize
    FGains: array of PSingle;        // FHistoryLen x FSpectrumSize
    FRealFFTs: array of PSingle;     // FHistoryLen x FWindowSize
    FImagFFTs: array of PSingle;     // FHistoryLen x FWindowSize
    FWriteProc: TNoiseWriteProc;
    FInited: Boolean;
    FTotalRead: QWord;
    function GetNoiseProfile: TSingleArray;
    procedure SetAttackDecayTime(AValue: Double);
    procedure SetFreqSmoothingHz(AValue: Double);
    procedure SetGain(AValue: Double);
    procedure SetNoiseProfile(AValue: TSingleArray);
    procedure SetSensitivity(AValue: Double);
  public
    constructor Create;
    destructor Destroy; override;
    function Init(ASampleRate: Integer): Boolean;
    function Process(AData: PSingle; ASampleCount: Integer; AGetNoiseProfile: Boolean): Boolean;
    procedure Flush; // finish writing out data in buffers.

    property NoiseProfile: TSingleArray read GetNoiseProfile write SetNoiseProfile;

    // these have defaults
    property Sensitivity: Double read FSensitivity write SetSensitivity;
    property Gain: Double read FNoiseGain write SetGain;
    property FreqSmoothingHz: Double read FFreqSmoothingHz write SetFreqSmoothingHz;
    property AttackDecayTime: Double read FAttackDecayTime write SetAttackDecayTime;
    property LeaveNoise: Boolean read FbLeaveNoise write FbLeaveNoise;



    // don't mess with these.
    property SampleRate: Double read FSampleRate;// write FSampleRate;
    property WindowSize: Integer read FWindowSize;// write FWindowSize;
    property SpectrumSize: Integer read FSpectrumSize;// write FSpectrumSize;
    property MinSignalTime: Single read FMinSignalTime;// write FMinSignalTime; // in secs

    // This must be assigned or av's will occur
    property WriteProc: TNoiseWriteProc read FWriteProc write FWriteProc;
  end;

implementation
uses
  math;

{ TNoiseRemoval }

function NewFloatArray(ALength: Integer): PSingle; inline;
begin
  Result := Getmem(ALength*SizeOf(Single));
end;

procedure TNoiseRemoval.Initialize;
var
  i: Integer;
begin
  FFreqSmoothingBins := Trunc(FFreqSmoothingHz * FWindowSize / FSampleRate);
  FAttackDecayBlocks := 1 + Trunc(FAttackDecayTime * FSampleRate / (FWindowSize / 2));
  // Applies to amplitudes, divide by 20:
  FNoiseAttenFactor  := power(10, FNoiseGain/20);

  // Applies to gain factors which apply to amplitudes, divide by 20:
  //FOneBlockAttackDecay := power(10.0, (FNoiseGain / (20.0 * FAttackDecayBlocks)));
  FOneBlockAttackDecay := power(10.0, (FNoiseGain /  FAttackDecayBlocks) / 20 );
  // Applies to power, divide by 10:
  FSensitivityFactor := power(10.0, FSensitivity/10.0);
  FMinSignalBlocks := Trunc(FMinSignalTime * FSampleRate / (FWindowSize / 2));
  if( FMinSignalBlocks < 1 ) then
    FMinSignalBlocks := 1;
  FHistoryLen := (2 * FAttackDecayBlocks) - 1;

  if (FHistoryLen < FMinSignalBlocks) then
    FHistoryLen := FMinSignalBlocks;

  SetLength(FSpectrums, FHistoryLen);
  SetLength(FGains, FHistoryLen);
  SetLength(FRealFFTs, FHistoryLen);
  SetLength(FImagFFTs, FHistoryLen);
  for i := 0 to FHistoryLen-1 do
  begin
    FSpectrums[i] := NewFloatArray(FSpectrumSize);
    FGains[i]     := NewFloatArray(FSpectrumSize);
    FRealFFTs[i]  := NewFloatArray(FSpectrumSize);
    FImagFFTs[i]  := NewFloatArray(FSpectrumSize);
  end;

   // Initialize the FFT
   FFFT := TFFT.InitializeFFT(FWindowSize);

   FFFTBuffer        := NewFloatArray(FWindowSize);
   FInWaveBuffer     := NewFloatArray(FWindowSize);
   FWindow           := NewFloatArray(FWindowSize);
   FOutOverlapBuffer := NewFloatArray(FWindowSize);

   // Create a Hanning window function
   for i := 0 to FWindowSize-1 do
      FWindow[i] := 0.5 - 0.5 * cos((2.0*pi*i) / FWindowSize);

   if FDoProfile then
   begin
      FillChar(FNoiseThreshold[0], SizeOf(FNoiseThreshold[0])*FSpectrumSize, 0);
      //for i := 0 to FSpectrumSize-1 do
      //   FNoiseThreshold[i] := float(0);
   end;

end;

procedure TNoiseRemoval.Reset;
var
  i, j: Integer;
begin
  for i := 0 to FHistoryLen-1 do
  begin
    for j := 0 to FSpectrumSize-1 do
    begin
      FSpectrums[i][j] := 0;
      FGains[i][j] := FNoiseAttenFactor;
      FRealFFTs[i][j] := 0.0;
      FImagFFTs[i][j] := 0.0;
    end;
  end;

  for j := 0 to FWindowSize-1 do
    FOutOverlapBuffer[j] := 0.0;

  FInputPos := 0;
  FInSampleCount := 0;
  FOutSampleCount := -(FWindowSize div 2) * (FHistoryLen - 1);
end;

function Min(A, B: Integer): Integer;
begin
  if A < B then
    Exit(A);
  Result := B;
end;

procedure TNoiseRemoval.ProcessSamples(len: Integer; Buffer: PSingle);
var
  i: Integer;
  avail: Integer;
begin
  //while((len and FOutSampleCount) < FInSampleCount) do
  while len > 0 do
  begin
    avail := Min(len, FWindowSize - FInputPos);
    for i := 0 to avail-1 do
      FInWaveBuffer[FInputPos + i] := buffer[i];
    buffer += avail;
    len -= avail;
    FInputPos += avail;

    if (FInputPos = FWindowSize) then
    begin
      FillFirstHistoryWindow();
      if (FDoProfile) then
        GetProfile()
      else
        RemoveNoise();
      RotateHistoryWindows();

      // Rotate halfway for overlap-add
      //for(i = 0; i < mWindowSize / 2; i++) {
      for i := 0 to FWindowSize div 2 -1 do
        FInWaveBuffer[i] := FInWaveBuffer[i + FWindowSize div 2];

      FInputPos := FWindowSize div 2;
    end;
  end;
end;

procedure TNoiseRemoval.FillFirstHistoryWindow;
var
  i: Integer;
begin
  for i := 0 to FWindowSize-1 do
    FFFTBuffer[i] := FInWaveBuffer[i];
  FFFT^.RealFFTf(FFFTBuffer);
  //for(i = 1; i < (mSpectrumSize-1); i++) {
  for i := 1 to FSpectrumSize-2 do
  begin
    FRealFFTs[0][i] := FFFTBuffer[FFFT^.BitReversed[i]  ];
    FImagFFTs[0][i] := FFFTBuffer[FFFT^.BitReversed[i]+1];
    FSpectrums[0][i] := FRealFFTs[0][i]*FRealFFTs[0][i] + FImagFFTs[0][i]*FImagFFTs[0][i];
    FGains[0][i] := FNoiseAttenFactor;
  end;

   // DC and Fs/2 bins need to be handled specially
   FSpectrums[0][0] := FFFTBuffer[0]*FFFTBuffer[0];
   FSpectrums[0][FSpectrumSize-1] := FFFTBuffer[1]*FFFTBuffer[1];
   FGains[0][0] := FNoiseAttenFactor;
   FGains[0][FSpectrumSize-1] := FNoiseAttenFactor;
end;

function Max(A,B: Integer): Integer; inline;
begin
  if A>B then
    Exit(A);
  Result := B;
end;

procedure TNoiseRemoval.ApplyFreqSmoothing(ASpec: PSingle);
var
  tmp: PSingle;
  i, j, j0, j1: Integer;
begin
  tmp := NewFloatArray(FSpectrumSize);
  for i := 0 to FSpectrumSize-1 do
  begin
      j0 := Max(0, i - FFreqSmoothingBins);
      j1 := Min(FSpectrumSize-1, i + FFreqSmoothingBins);
      tmp[i] := 0.0;
      //for(j = j0; j <= j1; j++)
      for j := j0 to j1-1 do
      begin
         tmp[i] += Aspec[j];
      end;
      tmp[i] := tmp[i] / (j1 - j0 + 1);
   end;

   //for(i = 0; i < mSpectrumSize; i++)
   for i := 0 to  FSpectrumSize-1 do
      Aspec[i] := tmp[i];

   Freemem(Tmp);
end;

procedure TNoiseRemoval.GetProfile;
var
  start,
  finish,
  i, j: Integer;
  min: Single;
begin
  // The noise threshold for each frequency is the maximum
  // level achieved at that frequency for a minimum of
  // mMinSignalBlocks blocks in a row - the max of a min.

  start := FHistoryLen - FMinSignalBlocks;
  finish := FHistoryLen;


  for j := 0 to FSpectrumSize-1 do
  begin
    min := FSpectrums[start][j];
    for i := start+1 to finish-1 do
      if (FSpectrums[i][j] < min) then
        min := FSpectrums[i][j];

    if (min > FNoiseThreshold[j]) then
      FNoiseThreshold[j] := min;
  end;

  FOutSampleCount += FWindowSize div 2; // what is this for?  Not used when we are getting the profile?
end;

procedure TNoiseRemoval.RemoveNoise;
var
  center: Integer;
  start,
  finish,
  i,j : Integer;
  min: Single;
  out_: Integer;
begin
  center := FHistoryLen div 2;
  start  := center - FMinSignalBlocks div 2;
  finish := start + FMinSignalBlocks;

   // Raise the gain for elements in the center of the sliding history
   for j := 0 to FSpectrumSize-1 do
   begin
      min := FSpectrums[start][j];
      //for (i = start+1; i < finish; i++) {
      for i := start+1 to finish-1 do
      begin
        if (FSpectrums[i][j] < min) then
          min := FSpectrums[i][j];
      end;
      if (min > FSensitivityFactor * FNoiseThreshold[j]) and (FGains[center][j] < 1.0) then
      begin
         if (FbLeaveNoise) then
           FGains[center][j] := 0.0
         else
           FGains[center][j] := 1.0;
      end
      else
      begin
         if (FbLeaveNoise) then
           FGains[center][j] := 1.0;
      end;
   end;

   // Decay the gain in both directions;
   // note that mOneBlockAttackDecay is less than 1.0
   // of linear attenuation per block
   for j := 0 to FSpectrumSize-1 do
   begin
     for i := center+1 to FHistoryLen-1 do
     begin
       if (FGains[i][j] < FGains[i - 1][j] * FOneBlockAttackDecay) then
         FGains[i][j] := FGains[i - 1][j] * FOneBlockAttackDecay;
       if (FGains[i][j] < FNoiseAttenFactor) then
         FGains[i][j] := FNoiseAttenFactor;
     end;
     for i := center-1 downto 0 do
     begin
       if (FGains[i][j] < FGains[i + 1][j] * FOneBlockAttackDecay) then
         FGains[i][j] := FGains[i + 1][j] * FOneBlockAttackDecay;
       if (FGains[i][j] < FNoiseAttenFactor) then
         FGains[i][j] := FNoiseAttenFactor;
     end;
   end;


   // Apply frequency smoothing to output gain
   out_ := FHistoryLen - 1;  // end of the queue

   ApplyFreqSmoothing(FGains[out_]);

   // Apply gain to FFT
   //for (j = 0; j < (mSpectrumSize-1); j++) {
   for j := 0 to FSpectrumSize-2 do
   begin
     FFFTBuffer[j*2  ] := FRealFFTs[out_][j] * FGains[out_][j];
     FFFTBuffer[j*2+1] := FImagFFTs[out_][j] * FGains[out_][j];
   end;
   // The Fs/2 component is stored as the imaginary part of the DC component
   FFFTBuffer[1] := FRealFFTs[out_][FSpectrumSize-1] * FGains[out_][FSpectrumSize-1];

   // Invert the FFT into the output buffer
   FFFT^.InverseRealFFTf(FFFTBuffer);

   // Overlap-add
   for j := 0 to FSpectrumSize-2 do
   begin
      FOutOverlapBuffer[j*2  ] += FFFTBuffer[FFFT^.BitReversed[j]  ] * FWindow[j*2  ];
      FOutOverlapBuffer[j*2+1] += FFFTBuffer[FFFT^.BitReversed[j]+1] * FWindow[j*2+1];
   end;

   // Output the first half of the overlap buffer, they're done -
   // and then shift the next half over.

   if (FOutSampleCount >= 0) then // ...but not if it's the first half-window
   begin
      //FOutputTrack->Append((samplePtr)mOutOverlapBuffer, floatSample, mWindowSize / 2);
     FWriteProc(Self, FOutOverlapBuffer, FWindowSize div 2);
   end;


   FOutSampleCount += FWindowSize div 2;
   //for(j = 0; j < mWindowSize / 2; j++)
   for j := 0 to FWindowSize div 2 -1 do
   begin
      FOutOverlapBuffer[j] := FOutOverlapBuffer[j + (FWindowSize div 2)];
      FOutOverlapBuffer[j + (FWindowSize div 2)] := 0.0;
   end
end;

procedure TNoiseRemoval.RotateHistoryWindows;
var
  last: Integer;
  i: Integer;
  lastSpectrum: PSingle;
  lastGain: PSingle;
  lastRealFFT: PSingle;
  lastImagFFT: PSingle;
begin
  last := FHistoryLen - 1;

   // Remember the last window so we can reuse it
   lastSpectrum := FSpectrums[last];
   lastGain     := FGains[last];
   lastRealFFT  := FRealFFTs[last];
   lastImagFFT  := FImagFFTs[last];

   // Rotate each window forward
   //for(i = last; i >= 1; i--) {
   for i := last downto 1 do
   begin
     FSpectrums[i] := FSpectrums[i-1];
     FGains[i]     := FGains[i-1];
     FRealFFTs[i]  := FRealFFTs[i-1];
     FImagFFTs[i]  := FImagFFTs[i-1];
   end;

   // Reuse the last buffers as the new first window
   FSpectrums[0] := lastSpectrum;
   FGains[0]     := lastGain;
   FRealFFTs[0]  := lastRealFFT;
   FImagFFTs[0]  := lastImagFFT;
end;

procedure TNoiseRemoval.FinishTrack;
var
  empty: PSingle;
  i: Integer;
begin
  // Keep flushing empty input buffers through the history
  // windows until we've output exactly as many samples as
  // were input.
  // Well, not exactly, but not more than mWindowSize/2 extra samples at the end.
  // We'll delete them later in ProcessOne.
  empty := NewFloatArray(FWindowSize div 2);
  //for(i = 0; i < mWindowSize / 2; i++)
  for i := 0 to FWindowSize div 2 -1 do
    empty[i] := 0.0;

  while (FOutSampleCount < FInSampleCount) do
    ProcessSamples(FWindowSize div 2, empty);

  Freemem(empty);
end;

procedure TNoiseRemoval.Cleanup;
var
  i: Integer;
begin
   FFFT^.EndFFT;

   if (FDoProfile) then
      ApplyFreqSmoothing(@FNoiseThreshold[0]);


   for i := 0 to FHistoryLen-1 do
   begin
     FreeMem(FSpectrums[i]);
     FreeMem(FGains[i]);
     FreeMem(FRealFFTs[i]);
     FreeMem(FImagFFTs[i]);
   end;
   SetLength(FSpectrums,0);
   SetLength(FGains,0);
   SetLength(FRealFFTs,0);
   SetLength(FImagFFTs,0);

   FreeMem(FFFTBuffer);
   FreeMem(FInWaveBuffer);
   FreeMem(FWindow);
   FreeMem(FOutOverlapBuffer);

   FInited := False;
end;

function TNoiseRemoval.GetNoiseProfile: TSingleArray;
begin
  SetLength(Result, FSpectrumSize);
  Move(FNoiseThreshold[0], Result[0], FSpectrumSize);
end;

procedure TNoiseRemoval.SetAttackDecayTime(AValue: Double);
begin
  if FAttackDecayTime=AValue then Exit;
  if AValue < 0.0 then AValue := 0;
  if AValue > 1.0 then AValue := 1.0;
  FAttackDecayTime:=AValue;
end;

procedure TNoiseRemoval.SetFreqSmoothingHz(AValue: Double);
begin
  if FFreqSmoothingHz=AValue then Exit;
  if AValue<0 then AValue:=0;
  if AValue>1000 then AValue := 1000;
  FFreqSmoothingHz:=AValue;
end;

procedure TNoiseRemoval.SetGain(AValue: Double);
begin
  if FNoiseGain=AValue then Exit;
  if AValue > 0 then AValue:=0;
  if AValue < -48 then AValue := -48;

  FNoiseGain:=AValue;
end;

procedure TNoiseRemoval.SetNoiseProfile(AValue: TSingleArray);
begin
  SetLength(FNoiseThreshold, FSpectrumSize);
  Move(AValue[0], FNoiseThreshold[0], FSpectrumSize);
  FHasProfile:=True;

  FDoProfile:=False;
  Cleanup; // set after FDoProfile so the profile is not processed.

end;

procedure TNoiseRemoval.SetSensitivity(AValue: Double);
begin
  if FSensitivity=AValue then Exit;
  if AValue < -20.0 then AValue:=-20.0;
  if AValue > 20.0 then AValue := 20.0;
  FSensitivity:=AValue;
end;

constructor TNoiseRemoval.Create;
begin
  FWindowSize:=2048;
  FSpectrumSize:= 1 + FWindowSize div 2;

  // loaded prefs
  FSensitivity := 0.0;
  FNoiseGain := -24.0;
  FFreqSmoothingHz := 150.0;
  FAttackDecayTime:= 0.15;
  FbLeaveNoise:=False;

  FMinSignalTime := 0.05;
  FHasProfile := False;
  FDoProfile := True;

  SetLength(FNoiseThreshold, FSpectrumSize);

end;

destructor TNoiseRemoval.Destroy;
begin
  SetLength(FNoiseThreshold, 0);
  inherited Destroy;
end;

function TNoiseRemoval.Init(ASampleRate: Integer): Boolean;
begin
  FSampleRate:=ASampleRate;
  Initialize;
  FInited:=True;
  Result := True;
  Reset;
end;

function TNoiseRemoval.Process(AData: PSingle; ASampleCount: Integer;
  AGetNoiseProfile: Boolean): Boolean;
begin
  if not FInited then
    Raise Exception.Create('TNoiseRemoval is not Inited');

  if not AGetNoiseProfile and not FHasProfile then
    raise Exception.Create('Tried to remove noise without profile.');


  FDoProfile:=AGetNoiseProfile;

  if FDoProfile and (FTotalRead = 0) then
  begin
    Initialize;
    reset;
  end;

  Inc(FTotalRead, ASampleCount);
  ProcessSamples(ASampleCount, AData);
  Result := True;

  if {AGetNoiseProfile or }FDoProfile then
  begin
    FHasProfile:=True;
    Cleanup; // triggers the data in FNoiseThreshold to be processed

    // must be set after Cleanup() is called
    //FDoProfile:=False;

    //Initialize;
    FTotalRead:=0;
  end;

  FHasProfile:=True;
  FDoProfile := False;

end;

procedure TNoiseRemoval.Flush;
begin
  if not FInited then
    Exit;

  FinishTrack;
  Cleanup; // Sets FInited to False
end;

initialization
SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision, exUnderflow,exZeroDivide]);

end.

