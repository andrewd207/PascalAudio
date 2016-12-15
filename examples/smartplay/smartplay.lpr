program smartplay;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  pa_base,
  pa_register,
  pa_stream,
  pa_pulse_simple,
  pa_flac,
  pa_dec_oggvorbis,
  pa_wav;

var
  ReaderClass: TPAStreamSourceClass;
  Reader: TPAStreamSource;
  DevOutClass: TPAAudioDestinationClass;
  DevOut: TPAAudioDestination;

begin
  ReaderClass := PARegisteredGetDecoderClass(ParamStr(1), False);
  DevOutClass := PARegisteredGetDeviceOut('');  // empty name just gets first device

  Reader := ReaderClass.Create(TFileStream.Create(ParamStr(1), fmOpenRead), True);
  DevOut := DevOutClass.Create;
  DevOut.DataSource := Reader;

  WriteLn('ReaderClassname = ', Reader.ClassName);
  WriteLn('DevOutClassname = ', DevOutClass.ClassName);

  Reader.StartData;

  while Reader.Working or DevOut.Working do
    CheckSynchronize;
end.

