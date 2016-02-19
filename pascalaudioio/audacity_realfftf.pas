{
 audacity which this file was converted from is GPLv2
 http://www.audacityteam.org/about/license/
 }
unit audacity_realfftf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PFFT = ^TFFT;

  { TFFT }

  TFFT = object
    BitReversed: PInteger;
    SinTable: PSingle;
    Points: Integer;
    FPCSinTable: array of Single;
    function  InitializeFFT(FFTLen: Integer): PFFT; static;
    procedure EndFFT;
    function  GetFFT(FFTLen: Integer): PFFT; static;
    procedure ReleaseFFT;
    procedure InverseRealFFTf(buffer: PSingle);
    procedure CleanupFFT; static; // ???
    procedure RealFFTf(buffer: PSingle);
    procedure ReorderToTime(Buffer: PSingle; TimeOut: PSingle);
    procedure ReorderToFreq(Buffer: PSingle; RealOut: PSingle; ImagOut: PSingle);
  end;


implementation
uses
  Math;

const
  PI = 3.14159265358979323846;
  MAX_HFFT = 10;
var
  FFTArray: array[0..MAX_HFFT-1] of PFFT;
  FFTLockCount: array[0..MAX_HFFT-1] of Integer;

{ TFFT }

function TFFT.InitializeFFT(FFTLen: Integer): PFFT;
var
  i: Integer;
  temp: Integer;
  mask: Integer;
begin
   Result := New(PFFT);
   if Result = nil then
     raise EOutOfMemory.Create('Error allocating memory for FFT');


   with Result^ do begin

   {*
   *  FFT size is only half the number of data points
   *  The full FFT output can be reconstructed from this FFT's output.
   *  (This optimization can be made since the data is real.)
   *}
   Points := FFTLen div 2;

   SetLength(FPCSinTable, 2*Points);
   SinTable:=@FPCSinTable[0];

   BitReversed := Getmemory(Points*SizeOf(Integer));
   if BitReversed = nil then
     raise EOutOfMemory.Create('Error allocating memory for BitReversed.');

   for i := 0 to Points-1 do
   begin
      temp:=0;
      mask := Points div 2;
      while mask > 0 do
      begin
        //for(mask=h->Points/2;mask>0;mask >>= 1)
        //   temp=(temp >> 1) + (i&mask ? h->Points : 0);
        temp := (temp shr 1);
        if (i and mask) <> 0 then
          temp := temp + Points;
        //else temp := temp + 0;  // why would you do that?
        mask := mask shr 1;
      end;

      BitReversed[i]:=temp;
   end;

   for i := 0 to Points-1 do
   begin
     SinTable[BitReversed[i]  ]:= -sin(2*PI*i/(2*Points));
     SinTable[BitReversed[i]+1]:= -cos(2*PI*i/(2*Points));
   end;

{$ifdef EXPERIMENTAL_EQ_SSE_THREADED}
   // new SSE FFT routines work on live data
   for(i=0;i<32;i++)
      if((1<<i)&fftlen)
         h->pow2Bits=i;
{$endif}

   end; // with Result^


end;

procedure TFFT.EndFFT;
begin
  if Points>0 then
  begin
    Freemem(BitReversed);
    SetLength(FPCSinTable, 0);
    SinTable:=nil;
  end;

  Dispose(PFFT(@Self));
end;

function TFFT.GetFFT(FFTLen: Integer): PFFT;
var
  h: Integer = 0;
  n: Integer;
begin
  n := fftlen div 2;

  while (h<MAX_HFFT) and (FFTArray[h] <> nil) and (n <> FFTArray[h]^.Points) do
  begin
    if (h<MAX_HFFT) then
    begin
      if(FFTArray[h] = nil) then
      begin
        FFTArray[h] := InitializeFFT(fftlen);
        FFTLockCount[h] := 0;
      end;
      Inc(FFTLockCount[h]);
      Exit(FFTArray[h]);
    end
    else begin
      // All buffers used, so fall back to allocating a new set of tables
      Exit(InitializeFFT(fftlen));
    end;
    Inc(h);
  end;
end;

procedure TFFT.ReleaseFFT;
var
  h: Integer = 0;
begin

   while (h<MAX_HFFT) and (FFTArray[h] <> @Self) do
   begin
     if(h<MAX_HFFT) then
     begin
       Dec(FFTLockCount[h]);
     end
     else
     begin
       EndFFT;
     end;
     Inc(h);
   end;
end;

procedure TFFT.InverseRealFFTf(buffer: PSingle);
var
  A, B: PSingle;
  sptr: PSingle;
  endptr1, endptr2: PSingle;
  br1: PInteger;
  HRplus,HRminus,HIplus,HIminus: Single;
  v1,v2,sin,cos: Single;
  ButterfliesPerGroup: Integer;
begin

   ButterfliesPerGroup:=Points div 2;

   //* Massage input to get the input for a real output sequence. */
   A:=@buffer[2];
   B:=@buffer[Points*2-2];
   br1:=@BitReversed[1];
   while(A<B) do
   begin
      sin:=SinTable[br1^];
      cos:=SinTable[br1[1]];
      //HRplus = (HRminus = *A     - *B    ) + (*B     * 2);
      HRminus:=A^-B^;
      HRplus:=HRminus+ (B^ *2);

      //HIplus = (HIminus = *(A+1) - *(B+1)) + (*(B+1) * 2);
      HIminus:=A[1]-B[1];
      HIplus:=HIminus+(B[1] *2);

      v1 := (sin*HRminus + cos*HIplus);
      v2 := (cos*HRminus - sin*HIplus);
      A^ := (HRplus  + v1) * single(0.5);
      B^ := A^ - v1;
      A[1] := (HIminus - v2) * single(0.5);
      B[1] := A[1] - HIminus;

      A+=2;
      B-=2;
      Inc(br1);
   end;
   //* Handle center bin (just need conjugate) */
   A[1] :=-A[1];
   {* Handle DC bin separately - this ignores any Fs/2 component
   buffer[1]=buffer[0]=buffer[0]/2;*}
   //* Handle DC and Fs/2 bins specially */
   //* The DC bin is passed in as the real part of the DC complex value */
   //* The Fs/2 bin is passed in as the imaginary part of the DC complex value */
   //* (v1+v2) = buffer[0] == the DC component */
   //* (v1-v2) = buffer[1] == the Fs/2 component */
   v1:=0.5*(buffer[0]+buffer[1]);
   v2:=0.5*(buffer[0]-buffer[1]);
   buffer[0]:=v1;
   buffer[1]:=v2;

   {*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   *}

   endptr1:=@buffer[Points*2];

   while(ButterfliesPerGroup>0) do
   begin
      A:=buffer;
      B:=@buffer[ButterfliesPerGroup*2];
      sptr:=@SinTable[0];

      while(A<endptr1) do
      begin
         sin:=sptr^; Inc(sptr); // *(sptr++);
         cos:=sptr^; Inc(sptr); // *(sptr++);
         endptr2:=B;
         while(A<endptr2) do
         begin
            v1:=B^*cos - B[1]*sin;
            v2:=B^*sin + B[1]*cos;
            B^ := (A^+v1)*Single(0.5);
            A^ := B^ - v1; Inc(A); Inc(B); //*(A++)=*(B++)-v1;
            B^ := (A^ + v2)* Single(0.5);    //*B=(*A+v2)*(fft_type)0.5;
            A^ := B^ - v2; Inc(A); Inc(B); //*(A++)=*(B++)-v2;
         end;
         A:=B;
         B := @B[ButterfliesPerGroup*2];
      end;
      ButterfliesPerGroup := ButterfliesPerGroup shr 1;
   end;
end;

procedure TFFT.CleanupFFT;
var
  h: Integer;
begin

   for h :=0 to  MAX_HFFT-1do begin
      if((FFTLockCount[h] <= 0) and (FFTArray[h] <> nil)) then
      begin
        FFTArray[h]^.EndFFT;
        FFTArray[h] := nil;
      end;
   end;
end;

procedure TFFT.RealFFTf(buffer: PSingle);
var
  A, B: PSingle;
  sptr: PSingle;
  endptr1, endptr2: PSingle;
  br1, br2: PInteger;
  HRplus,HRminus,HIplus,HIminus: Single;
  v1,v2,sin_,cos_: Single;
  ButterfliesPerGroup: Integer;
begin
  ButterfliesPerGroup:=Points div 2;

   {*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   *}

   endptr1:=buffer+Points*2;

   while(ButterfliesPerGroup>0) do
   begin
      A:=buffer;
      B:=buffer+ButterfliesPerGroup*2;
      sptr:=@SinTable[0];

      while(A<endptr1) do
      begin
         sin_:=sptr^;
         cos_ := sptr[1];
         endptr2:=B;
         while(A<endptr2) do
         begin
           v1 := B^ * cos_ + B[1] * sin_;  //v1=*B*cos + *(B+1)*sin;
           v2 := B^ * sin_ - B[1] * cos_;  //v2=*B*sin - *(B+1)*cos;
           B^ := A^+v1;                    //*B=(*A+v1);
           A^ := B^-2*v1; Inc(A); Inc(B);  //*(A++)=*(B++)-2*v1;

           B^ := A^-v2;                    //*B=(*A-v2);
           A^ := B^+2*v2; Inc(A); Inc(B);  //*(A++)=*(B++)+2*v2;
         end;
         A:=B;
         B:=B+ButterfliesPerGroup*2;
         sptr:=sptr+2;
      end;

      ButterfliesPerGroup := ButterfliesPerGroup shr 1;
   end;

   //* Massage output to get the output for a real input sequence. */
   br1:=@BitReversed[1];   // is this wrong? Should be @BitReversed[0] ; ?
   br2:=@BitReversed[Points-1];

   while(br1<br2) do
   begin
      sin_:=SinTable[br1[0]];
      cos_:=SinTable[br1[1]];
      A:=@buffer[br1^];
      B:=@buffer[br2^];
      //HRplus = (HRminus = *A     - *B    ) + (*B     * 2);
      HRminus := A^ - B^;
      HRplus := HRminus + (B^ * 2);

      //HIplus = (HIminus = *(A+1) - *(B+1)) + (*(B+1) * 2);
      HIminus := A[1] - B[1];
      HIplus := HIminus + (B[1] * 2);

      v1 := (sin_*HRminus - cos_*HIplus);
      v2 := (cos_*HRminus + sin_*HIplus);
      A^ := (HRplus  + v1) * single(0.5);
      B^ := A^ - v1;
      A[1] := (HIminus + v2) * single(0.5);
      B[1] := A[1] - HIminus;

      Inc(br1);
      Dec(br2);
   end;
   //* Handle the center bin (just need a conjugate) */
   A:=buffer+br1[1];
   A^ := -A^;
   {* Handle DC bin separately - and ignore the Fs/2 bin
   buffer[0]+=buffer[1];
   buffer[1]=(fft_type)0;*}
   ///* Handle DC and Fs/2 bins separately */
   ///* Put the Fs/2 value into the imaginary part of the DC bin */
   v1:=buffer[0]-buffer[1];
   buffer[0]+=buffer[1];
   buffer[1]:=v1;

end;

procedure TFFT.ReorderToTime(Buffer: PSingle; TimeOut: PSingle);
var
  i: Integer;
begin
  // Copy the data into the real outputs
   //for(int i=0;i<hFFT->Points;i++) {
  for i := 0 to Points-1 do
  begin
    TimeOut[i*2  ]:=buffer[BitReversed[i]  ];
    TimeOut[i*2+1]:=buffer[BitReversed[i]+1];
  end;
end;

procedure TFFT.ReorderToFreq(Buffer: PSingle; RealOut: PSingle; ImagOut: PSingle
  );
var
  i: Integer;
begin
  // Copy the data into the real and imaginary outputs
  //for(int i=1;i<hFFT->Points;i++)
  for i := 1 to Points-1 do
  begin

      RealOut[i]:=buffer[BitReversed[i]  ];
      ImagOut[i]:=buffer[BitReversed[i]+1];
   end;
   RealOut[0] := buffer[0]; // DC component
   ImagOut[0] := 0;
   RealOut[Points] := buffer[1]; // Fs/2 component
   ImagOut[Points] := 0;
end;

initialization
  FillChar(FFTArray, SizeOf(FFTArray), 0);
  FillChar(FFTLockCount, SizeOf(FFTLockCount), 0);

end.

