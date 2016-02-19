program playogg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, pa_dec_oggvorbis, pa_pulse_simple, sysutils;

var
  ogg: TPAOggVorbisDecoderSource;
  pulse: TPAPulseDestination;
  FileName: String;

begin
  FileName := ParamStr(1);
  if (FileName = '') or not FileExists(FileName) then
  begin
    WriteLn('  Usage: ',ExtractFileName(ParamStr(0)),' ''file.ogg''');
    Exit;
  end;
  // create ogg decoder
  ogg := TPAOggVorbisDecoderSource.Create;

  // create pulse destination
  pulse := TPAPulseDestination.Create;
  // assign ogg as source of data
  pulse.DataSource := ogg;

  // set tthe file stream ogg reads from
  ogg.Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);

  // start the chain from the first link.
  ogg.StartData;

  sleep(1000);
  //ogg.Position:=222;
  // while decoding sleep
  while ogg.Working do
    Sleep(1);
  pulse.free;
  ogg.free;
end.

