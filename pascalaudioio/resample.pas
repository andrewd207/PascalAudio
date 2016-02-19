{ libresample which this file links to is LGPLv2.1 }
{
    This unit is part of the PascalAudio project.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and license.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit resample;

{$mode objfpc}{$H+}

{$linklib resample}

interface

uses
  ctypes, pa_ringbuffer, paio_types;

type
  { TResampleChannel }

  TResampleChannel = class
  private
    FResample: Pointer;
    FOutBuffer: TRingBuffer;
  public
    constructor Create;
    destructor Destroy; override;
    function WriteData(AData: PSingle; ASampleCount: Integer; AFactor: Double; AIsLastData: Boolean): Integer;
    function ReadData(var ADest; ASize: Integer): Integer;
    function ReadSingle: Single;
  end;

  { TResampleHelper }

  TResampleHelper = class
    Channels: array of TResampleChannel;
    //function PlexOutBuffers(out ASamples: Integer): PSingle; // the caller frees this result
    function Write(const AData: PSingle; ASampleCount: Integer; AFactor: Double; AIsLastData: Boolean): TSingleArray;

    constructor Create(AChannels: Integer);
    destructor  Destroy; override;

  end;

// libresample functions. LGPL
function resample_open(highQuality: cint; minFactor, maxFactor: cdouble): Pointer cdecl; external;

function resample_dup(handle: pointer): Pointer cdecl; external;

function resample_get_filter_width(handle: pointer): cint cdecl; external;

function resample_process(handle: pointer;
                          factor: cdouble;
                          inBuffer: pcfloat;
                          inBufferLen: cint;
                          lastFlag: cint;
                          inBufferUsed: pcint;
                          outBuffer: pcfloat;
                          outBufferLen: cint): cint cdecl; external;

procedure resample_close(handle: pointer) cdecl; external;



implementation
uses
  sysutils;

{ TResampleChannel }

constructor TResampleChannel.Create;
begin
  FResample := resample_open(1, 0.1, 7.0);
  FOutBuffer := TRingBuffer.Create(AUDIO_BUFFER_SIZE*7);
end;

destructor TResampleChannel.Destroy;
begin
  inherited Destroy;
  FOutBuffer.Free;
  resample_close(FResample);
end;

function TResampleChannel.WriteData(AData: PSingle; ASampleCount: Integer;
  AFactor: Double; AIsLastData: Boolean): Integer;
var
  WrittenSamples: cint;
  InPos: Integer;
  InBufUsed: cint;
  OutPos: Integer;
  OutBuffer: array[0..AUDIO_BUFFER_SIZE*5] of Single;
begin
  InPos := 0;
  OutPos := 0;
  Result := 0;
  repeat
    WrittenSamples := resample_process(FResample,
                                AFactor,
                                @AData[InPos],
                                ASampleCount-InPos,
                                Ord(AIsLastData),
                                @InBufUsed,
                                @OutBuffer[0],
                                Length(OutBuffer));
    Inc(InPos, InBufUsed);
    //WriteLn('InnerLoop InPos = ', InPos, ' InUsed = ' , InBufUsed, ' Written = ', WrittenSamples, ' SampleCount = ', ASampleCount);
    if WrittenSamples > 0 then
    begin
      Inc(OutPos, WrittenSamples);

      Inc(Result, FOutBuffer.Write(OutBuffer[0], WrittenSamples*SizeOf(Single))div 4);
    end;

  until (WrittenSamples = 0) or (InPos = ASampleCount);
  //Result := OutPos
end;

function TResampleChannel.ReadData(var ADest; ASize: Integer): Integer;
begin
  Result := FOutBuffer.Read(ADest, ASize);
end;

function TResampleChannel.ReadSingle: Single;
begin
  FOutBuffer.Read(Result, SizeOf(Single));
end;


{function TResampleHelper.PlexOutBuffers(out ASamples: Integer): PSingle;
var
  i: Integer;
  j: Integer;
begin
  ASize := Sizeof(Single) * OutBuffersLength[0] ;
  Result := GetMem(ASize);
  for i := 0 to OutBuffersLength[0]-1 do
  begin
    for j := 0 to ChannelCount-1 do
      Result[i*j] := OutBuffers[j][i];
  end;
end;}

function TResampleHelper.Write(const AData: PSingle; ASampleCount: Integer;
  AFactor: Double; AIsLastData: Boolean): TSingleArray;
var
  i: Integer;
  j: Integer;
  OutChannel: array of Single;
  SamplesPerChannel: Integer;
  WrittenSamples: Integer;
  MaxSamples: Integer; // data available across all channels
begin
  Result := nil;
  MaxSamples := 0;
  SamplesPerChannel := ASampleCount div Length(Channels);
  SetLength(OutChannel, SamplesPerChannel);
  for i := 0 to High(Channels) do begin
    for j := 0 to SamplesPerChannel -1 do // BytesPerSample(2) / AChannels
      OutChannel[j] := AData[j*Length(Channels)+i]; // to float
    WrittenSamples := Channels[i].WriteData(@OutChannel[0], SamplesPerChannel, AFactor, AIsLastData);
    //WriteLn(Format('Channel(%d) Wrote %d samples ',[i+1, WrittenSamples]));
    if (MaxSamples = 0) or (WrittenSamples < MaxSamples) then
      MaxSamples := WrittenSamples;
  end;

  SetLength(Result, MaxSamples*Length(Channels));
  //Result := Getmem(SizeOf(Single)*MaxSamples*Length(Channels));
  for i := 0 to High(Channels) do
    for j := 0 to MaxSamples-1 do
      Result[j*Length(Channels)+i] := Channels[i].ReadSingle;

end;

constructor TResampleHelper.Create(AChannels: Integer);
var
  i: Integer;
begin
  SetLength(Channels, AChannels);
  for i := 0 to High(Channels) do
    Channels[i] := TResampleChannel(TResampleChannel.Create);
end;

destructor TResampleHelper.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(Channels) do
  begin
    Channels[i].Free;
  end;
  inherited Destroy;
end;

end.

