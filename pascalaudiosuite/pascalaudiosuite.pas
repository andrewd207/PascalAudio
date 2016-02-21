{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PascalAudioSuite;

interface

uses
  pa_base, pa_lists, pa_resample, pa_enc_oggvorbis, pa_stream, pa_ladspa, 
  pa_dec_oggvorbis, pa_noiseremoval, pa_cdaudio, pa_process, pa_wav, 
  pa_binaural, pa_samplerate, pa_pulse_simple, pa_sox, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PascalAudioSuite', @Register);
end.
