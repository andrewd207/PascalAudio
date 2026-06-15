program observer;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, pa_dec_oggvorbis, pa_base, pa_observer, pa_pulse;

type
  { Holds the OnDataFinished handler. Suite events are "of object", so the
    callback has to live on an object. }
  TWatcher = class
    Finished: Boolean;
    procedure DataFinished(Sender: TObject);
  end;

procedure TWatcher.DataFinished(Sender: TObject);
begin
  // Runs on the main thread: TPAObserverLink marshals the event via Queue().
  WriteLn('observer: data finished');
  Finished := True;
end;

var
  ogg: TPAOggVorbisDecoderSource;
  obs: TPAObserverLink;
  dest: TPAPulseAsyncDestination;
  watcher: TWatcher;
begin
  if (ParamStr(1) = '') or not FileExists(ParamStr(1)) then
  begin
    WriteLn('usage: ', ExtractFileName(ParamStr(0)), ' file.ogg');
    Halt(1);
  end;

  watcher := TWatcher.Create;
  ogg := TPAOggVorbisDecoderSource.Create(
           TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyNone), True);
  obs := TPAObserverLink.Create;
  dest := TPAPulseAsyncDestination.Create;

  obs.OnDataFinished := @watcher.DataFinished;

  // chain: ogg -> observer -> pulse
  obs.DataSource := ogg;
  dest.DataSource := obs;

  ogg.StartData;

  while dest.Working do
    CheckSynchronize(10); // pump the queue so DataFinished can fire

  // make sure the queued callback gets dispatched even if it landed after the
  // final Working check
  while not watcher.Finished do
    CheckSynchronize(10);

  dest.Terminate;  dest.WaitFor;
  obs.Terminate;   obs.WaitFor;

  dest.Free;
  obs.Free;
  ogg.Free;
  watcher.Free;
  WriteLn('done');
end.
