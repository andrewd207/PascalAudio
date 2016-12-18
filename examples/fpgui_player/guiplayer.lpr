program guiplayer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, fpg_main,
  main_frm;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  fpgApplication.CreateForm(TMainForm, frm);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.

