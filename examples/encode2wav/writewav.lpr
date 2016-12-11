program writewav;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, sysutils, pa_wav, pa_dec_oggvorbis;

var
  Ogg: TPAOggVorbisDecoderSource;
  Wav: TPAWavDest;

const
  InFile = '../noiseremoval/noisyaudio.ogg';
  OutFile = 'writewav.wav';

procedure CreateObjects;
begin
  Ogg := TPAOggVorbisDecoderSource.Create;
  Ogg.Stream := TFileStream.Create(InFile, fmOpenRead);
  Ogg.InitValues;
  Wav := TPAWavDest.Create(TFileStream.Create(OutFile, fmOpenWrite or fmCreate), True);
  Wav.DataSource := Ogg;
end;

procedure Encode;
begin
  Ogg.StartData;

  while Ogg.Working or Wav.Working do
    CheckSynchronize(1);
end;

procedure FreeObjects;
begin
  Ogg.Stream.Free;
  Ogg.Free;
  Wav.Free;
end;


begin
  CreateObjects;
  Encode;
  FreeObjects;
end.

