{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PascalAudioIO;

{$warn 5023 off : no warning about unused units}
interface

uses
  ladspa, samplerate, resample, pa_ringbuffer, ladspa_classes, 
  audacity_noiseremoval, audacity_realfftf, OggHfObject, flac_classes, 
  paio_types, bs2b, paio_channelhelper, paio_utils, noiseremovalmultichannel, 
  paio_messagequeue, paio_faad2, paio_mmdevice, mp4codec, mp4codec_mp4a, 
  quicktimeatoms, quicktimecontainer, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PascalAudioIO', @Register);
end.
