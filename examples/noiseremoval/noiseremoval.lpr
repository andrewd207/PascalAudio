program noiseremoval;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cmem,
  cthreads,
  {$ENDIF}
  Classes, pa_noiseremoval, pa_pulse_simple, pa_dec_oggvorbis, pa_stream, pa_base, sysutils;

const
  NoiseSampleFile = 'noise.ogg';
  FileName = 'noisyaudio.ogg';

function GetNoiseStream: TMemoryStream;
var
  StreamDest: TPAStreamDestination;
  ogg: TPAOggVorbisDecoderSource;

begin
  ogg := TPAOggVorbisDecoderSource.Create;
  ogg.Stream := TFileStream.Create(NoiseSampleFile, fmOpenRead or fmShareDenyNone);

  Result := TMemoryStream.Create;
  StreamDest := TPAStreamDestination.Create(Result);
  StreamDest.DataSource := ogg;

  // first extract audiodata.
  ogg.StartData;
  //while ogg.Working  do
  //  sleep(0);
  sleep(1000);
  ogg.Stream.Free;
  ogg.Free;

  while StreamDest.Working  do
    sleep(1);

  StreamDest.Free;

  Result.Position:=0;
end;

var
  RawNoiseStream: TMemoryStream;
  ogg: TPAOggVorbisDecoderSource;
  noise: TPANoiseRemovalLink;
  pulse: TPAPulseDestination;
begin
  // open ogg file to be cleaned
  ogg := TPAOggVorbisDecoderSource.Create;
  ogg.Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);

  // create pulse destination to listen to audio
  pulse := TPAPulseDestination.Create;
  pulse.DataSource := ogg;

  // play unfiltered audio
  ogg.StartData;

  //wait to finish
  while pulse.Working do
    sleep(1);

  pulse.free;
  ogg.Free;
  // sleep(1000);
  ogg := TPAOggVorbisDecoderSource.Create;
  ogg.Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  ogg.InitValues; // important! sets channels etc for when noise profile requests it

  // create noise filter
  noise := TPaNoiseRemovalLink.Create;
  noise.DataSource := ogg;

  pulse := TPAPulseDestination.Create;
  pulse.DataSource := noise;

  // set the noise profile
  RawNoiseStream := GetNoiseStream;
  noise.SetNoiseProfile(0, PSingle(RawNoiseStream.Memory), RawNoiseStream.Size div SizeOf(Single));
  RawNoiseStream.Free;

  // filter audio and play to pulse
  ogg.StartData;

  //wait to finish
  while ogg.Working or pulse.Working do
    sleep(1);

  pulse.DataSource := ogg;

  pulse.free;
  noise.free;
  ogg.Free;
end.

