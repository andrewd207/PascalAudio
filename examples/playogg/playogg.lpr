program playogg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, pa_dec_oggvorbis,
  pa_base,
  {$IFDEF UNIX}
  pa_pulse_simple,
  {$else}
  pa_mmdevice,
  {$ENDIF}
  sysutils;

var
  ogg: TPAOggVorbisDecoderSource;
  Dest : TPAAudioDestination;
  FileName: String;
begin
  FileName := ParamStr(1);
  if (FileName = '') or not FileExists(FileName) then
  begin
    WriteLn('  Usage: ',ExtractFileName(ParamStr(0)),' ''file.ogg''');
    Exit;
  end;
  // create ogg decoder
  ogg := TPAOggVorbisDecoderSource.Create(TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone), True);

  // create audio out destination
  {$IFDEF UNIX}
    Dest := TPAPulseDestination.Create;
  {$else}
    Dest := TPAMMDestination.Create;
  {$ENDIF}

  // assign ogg as source of data
  Dest.DataSource := ogg;

  // start the chain from the first link.
  ogg.StartData;

  sleep(1000);
  //ogg.Position:=222;
  // while decoding sleep
  while Dest.Working  do
    Sleep(1);
  Dest.Terminate;
  Dest.WaitFor;
  Dest.free;
  ogg.free;
end.

