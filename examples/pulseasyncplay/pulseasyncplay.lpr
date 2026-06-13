program pulseasyncplay;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, pa_dec_oggvorbis, pa_base, pa_pulse;
var
  ogg: TPAOggVorbisDecoderSource;
  dest: TPAPulseAsyncDestination;
begin
  if (ParamStr(1) = '') or not FileExists(ParamStr(1)) then
  begin
    WriteLn('usage: ', ExtractFileName(ParamStr(0)), ' file.ogg');
    Halt(1);
  end;
  ogg := TPAOggVorbisDecoderSource.Create(TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyNone), True);
  dest := TPAPulseAsyncDestination.Create;
  dest.DataSource := ogg;
  ogg.StartData;
  while dest.Working do
    Sleep(10);
  dest.Terminate;
  dest.WaitFor;
  dest.Free;
  ogg.Free;
  WriteLn('done');
end.
