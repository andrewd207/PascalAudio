{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PascalAudioIO;

interface

uses
  ladspa, samplerate, resample, pa_ringbuffer, ladspa_classes, 
  audacity_noiseremoval, audacity_realfftf, OggHfObject, flac, paio_types, 
  bs2b, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PascalAudioIO', @Register);
end.
