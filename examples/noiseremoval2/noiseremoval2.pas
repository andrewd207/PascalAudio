program noiseremoval2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} cthreads, cmem,{$ENDIF}
  Classes, sysutils, noiseremovalmultichannel,
  pa_base, pa_dec_oggvorbis, pa_stream, pa_pulse_simple;

const
  NoiseSampleFile = '../noiseremoval/noisestereo.ogg';
  NoisyFile = '../noiseremoval/noisyaudiostereo.ogg';

function GetRawAudio(AFile: String): TMemoryStream;
var
  StreamDest: TPAStreamDestination;
  ogg: TPAOggVorbisDecoderSource;
begin
  // extract raw float32 data from
  ogg := TPAOggVorbisDecoderSource.Create;
  ogg.Stream := TFileStream.Create(AFile, fmOpenRead or fmShareDenyNone);
  Result := TMemoryStream.Create;
  StreamDest := TPAStreamDestination.Create(Result);
  StreamDest.DataSource := ogg;
  ogg.StartData;
  sleep(1000);
  ogg.Stream.Free;
  ogg.Free;
  while StreamDest.Working  do
    sleep(1);
  StreamDest.Free;
  Result.Position:=0;
end;

procedure PlayAudio(AData: PSingle; ASamplesCount: Integer);
var
  pulse: TPAPulseDestination;
  streamsource: TPAStreamSource;
  mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  Mem.Write(AData^, ASamplesCount*SizeOf(Single));
  Mem.Position:=0;

  streamsource:=TPAStreamSource.Create;
  streamsource.Stream := Mem;
  streamsource.Channels:=2;
  streamsource.Format:=afFloat32;
  streamsource.SamplesPerSecond:=44100;

  pulse := TPAPulseDestination.Create;
  pulse.DataSource := streamsource;

  streamsource.StartData;

  while pulse.Working do
    Sleep(1);

  streamsource.Free;
  pulse.Free;
  mem.Free;
end;

var
  RawNoise: TMemoryStream;
  RawAudio: TMemoryStream;
  FilteredAudio: PSingle;
  FilteredSamples: Integer;

type

  { TFooNoiseRemoval }

  TFooNoiseRemoval = class(TNoiseRemovalMultiChannel)
    OutStream: TStream;
    procedure WriteData(ASender: TObject; AData: PSingle; ASampleCount: Integer) ;
  end;


function FilterNoise(ANoiseSample, ANoisyAudio: TMemoryStream; out Samples: Integer): PSingle;
var
  NoiseRemoval: TFooNoiseRemoval;
begin
  Result := nil;
  NoiseRemoval := TFooNoiseRemoval.Create(2, 44100);
  NoiseRemoval.OutStream := TMemoryStream.Create;
  NoiseRemoval.WriteProc := @NoiseRemoval.WriteData;

  NoiseRemoval.ReadNoiseProfile(PSingle(ANoiseSample.Memory), ANoiseSample.Size div SizeOf(Single));

  // while not out of data do
  // begin
    { we'll process the whole stream in one go.}
  NoiseRemoval.ProcessNoise(PSingle(ANoisyAudio.Memory), ANoisyAudio.Size div SizeOf(Single));
  // end;
  NoiseRemoval.Flush; // output any remaining data

  Result:=GetMem(NoiseRemoval.OutStream.Size);
  Samples := NoiseRemoval.OutStream.Size div SizeOf(Single);
  NoiseRemoval.OutStream.Position:=0;
  NoiseRemoval.OutStream.Read(Result^, NoiseRemoval.OutStream.Size);
  NoiseRemoval.OutStream.Free;
end;

{ TFooNoiseRemoval }

procedure TFooNoiseRemoval.WriteData(ASender: TObject; AData: PSingle;
  ASampleCount: Integer);
begin
  OutStream.Write(AData^, ASampleCount*SizeOf(Single));
end;

begin
  RawNoise := GetRawAudio(NoiseSampleFile);
  RawAudio := GetRawAudio(NoisyFile);

  FilteredAudio:=FilterNoise(RawNoise, RawAudio, FilteredSamples);
  RawNoise.Free;
  RawAudio.Free;

  PlayAudio(FilteredAudio, FilteredSamples);
  Freemem(FilteredAudio);
end.

