program playopus;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, pa_ogg_opus,
  pa_base,
  {$IFDEF UNIX}
  pa_pulse_simple,
  {$else}
  pa_mmdevice,
  {$ENDIF}
  sysutils;

var
  opus: TPAOggOpusDecoderSource;
  Dest : TPAAudioDestination;
  FileName: String;
  lPos: Integer;
  lMaxPos: Int64;
begin
  FileName := ParamStr(1);
  if (FileName = '') or not FileExists(FileName) then
  begin
    WriteLn('  Usage: ',ExtractFileName(ParamStr(0)),' ''file.opus''');
    Exit;
  end;
  // create opus decoder
  opus := TPAOggOpusDecoderSource.Create(TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone), True);

  // create audio out destination
  {$IFDEF UNIX}
    Dest := TPAPulseDestination.Create;
  {$else}
    Dest := TPAMMDestination.Create;
  {$ENDIF}

  // assign ogg/opus as source of data
  Dest.DataSource := opus;

  // start the chain from the first link.
  opus.StartData;

  sleep(1000);
  // while decoding sleep


  lMaxPos := Trunc(opus.MaxPosition);
  lPos := 0;

  opus.Position := 44;

  while Dest.Working  do
  begin
    Sleep(100);
    if Trunc(opus.Position) <> lPos then
    begin

      lPos := Trunc(opus.Position);
      Write(#13); // output next on the same line..
      Write(Format('%d:%.2d / %d:%.2d', [lPos div 60, lpos mod 60, lMaxPos div 60, lMaxPos mod 60]));



      //WriteLn(lPos,'/',lMaxPos);
    end;
  end;
  WriteLn;
  Dest.Terminate;
  Dest.WaitFor;
  Dest.free;
  opus.free;
end.

