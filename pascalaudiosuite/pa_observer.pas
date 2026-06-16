{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_observer;

{$mode objfpc}{$H+}

interface

uses
  Classes, pa_base;

type
  { TPAObserverLink

    A middle link that forwards audio unchanged and lets you observe the
    stream. Insert it anywhere in a chain to be notified when the data has
    finished flowing through, without altering the audio.

    OnDataFinished is marshalled to the main thread (via TThread.Queue), so the
    handler is safe to touch the GUI. As with every queued event in the suite,
    the main thread must pump the synchronize queue (CheckSynchronize, or an
    LCL/fpGUI message loop) for the callback to fire. }

  TPAObserverLink = class(TPAAudioLink)
  private
    FOnDataFinished: TNotifyEvent;
    procedure DoDataFinished; // runs on the main thread
  protected
    function  InternalProcessData(const AData; ACount: Int64;
                AIsLastData: Boolean): Int64; override;
    procedure SignalDestinationsDone; override;
  public
    property OnDataFinished: TNotifyEvent read FOnDataFinished write FOnDataFinished;
  end;

implementation

{ TPAObserverLink }

function TPAObserverLink.InternalProcessData(const AData; ACount: Int64;
  AIsLastData: Boolean): Int64;
var
  B: PAudioBuffer;
begin
  // forward the data unchanged to whatever is chained after us. Pass the
  // FlushPendingSends pump: if the pool is empty we may block here while output
  // we already produced sits queued (PAM_SendBuffer) on our own thread,
  // undelivered -- so the destination starves and never returns a buffer. The
  // pump delivers that pending output while we wait, breaking the deadlock.
  // (Without it the chain stalled after a few buffers under back-pressure.)
  B := BufferPool.GetBufferFromPool(True, @FlushPendingSends);
  B^.Format      := Format;
  B^.UsedData    := ACount;
  B^.IsEndOfData := AIsLastData;
  Move(AData, B^.Data, ACount);
  WriteToDestinations(B);
  Result := ACount;
end;

procedure TPAObserverLink.SignalDestinationsDone;
begin
  inherited SignalDestinationsDone; // propagate EndOfData downstream first
  // SignalDestinationsDone runs in the worker thread; marshal the user
  // callback to the main thread so handlers don't race the GUI.
  if Assigned(FOnDataFinished) then
    Queue(@DoDataFinished);
end;

procedure TPAObserverLink.DoDataFinished;
begin
  if Assigned(FOnDataFinished) then
    FOnDataFinished(Self);
end;

end.
