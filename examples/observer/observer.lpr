program observer;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DateUtils, pa_dec_oggvorbis, pa_base, pa_observer, pa_pulse;

{ This example shows the two different "finished" signals in the suite and how
  they differ in timing:

    * TPAObserverLink.OnDataFinished fires the instant the pipeline hands its
      last buffer downstream into Pulse's queue. PulseAudio still has buffered
      audio to play, so this is the EARLY signal.

    * TPAAudioDestination.OnDataEnded (here the Pulse destination) fires only
      after FStream.Drain returns, i.e. after PulseAudio has physically played
      everything. This is the TRUE "done playing" signal.

  Both are marshalled to the main thread via Queue(), so the main loop must
  pump the synchronize queue (CheckSynchronize) for them to fire. }

type
  TWatcher = class
    Start: TDateTime;
    FedAll: Boolean;
    PlaybackDone: Boolean;
    // observer link: pipeline finished feeding (early)
    procedure DataFinished(Sender: TObject);
    // pulse destination: drain complete, audio actually played (late)
    procedure DataEnded(Sender: IPAAudioDestination; AUserObj: TObject);
    function MsSinceStart: Integer;
  end;

function TWatcher.MsSinceStart: Integer;
begin
  Result := MilliSecondsBetween(Now, Start);
end;

procedure TWatcher.DataFinished(Sender: TObject);
begin
  WriteLn(Format('[%5d ms] observer.OnDataFinished  -> pipeline fed all data (Pulse still playing)', [MsSinceStart]));
  FedAll := True;
end;

procedure TWatcher.DataEnded(Sender: IPAAudioDestination; AUserObj: TObject);
begin
  WriteLn(Format('[%5d ms] pulse.OnDataEnded        -> drain done, playback finished', [MsSinceStart]));
  PlaybackDone := True;
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
  watcher.Start := Now;

  ogg := TPAOggVorbisDecoderSource.Create(
           TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyNone), True);
  obs := TPAObserverLink.Create;
  dest := TPAPulseAsyncDestination.Create;

  // early signal, on the observer link
  obs.OnDataFinished := @watcher.DataFinished;
  // true "playback done" signal, on the destination (fires after Pulse drains)
  dest.OnDataEnded[nil] := @watcher.DataEnded;

  // chain: ogg -> observer -> pulse
  obs.DataSource := ogg;
  dest.DataSource := obs;

  ogg.StartData;

  // wait for the real end (drain complete), pumping the queue so both
  // callbacks can fire. PlaybackDone is set by pulse.OnDataEnded.
  while not watcher.PlaybackDone do
    CheckSynchronize(10);

  dest.Terminate;  dest.WaitFor;
  obs.Terminate;   obs.WaitFor;

  dest.Free;
  obs.Free;
  ogg.Free;
  watcher.Free;
  WriteLn('done');
end.
