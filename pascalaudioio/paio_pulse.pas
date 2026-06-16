{
    This unit is part of Pascal Audio IO package.

    Copyright (c) 2026 by Andrew Haines.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

{ io-layer wrapper around the asynchronous PulseAudio API (libpulse, not
  libpulse-simple). It drives a pa_threaded_mainloop + pa_context + pa_stream
  internally but exposes a small blocking interface (Open/Write/Drain/Close)
  so callers can treat it like the simple API. }

unit paio_pulse;

{$mode objfpc}{$H+}

interface

// USEPULSE gates the whole unit so it compiles to an empty unit where libpulse
// isn't available (e.g. Windows builds), matching pa_pulse_simple.
{$IFDEF USEPULSE}

uses
  Classes, SysUtils, ctypes,
  pulse_sample, pulse_def, pulse_context, pulse_stream,
  pulse_thread_mainloop, pulse_operation;

type
  TPASampleFormat = pulse_sample.TPASampleFormat;

  { TPulseAsyncStream }

  TPulseAsyncStream = class
  private
    FMainloop: PPAThreadedMainloop;
    FContext: PPAContext;
    FStream: PPAStream;
    FSpec: TPASampleSpec;
    FName: AnsiString;
    FLastError: AnsiString;
    FOpen: Boolean;
    FTargetLatencyMS: LongWord;
  public
    constructor Create(const AName: AnsiString; ARate, AChannels: Integer; AFormat: TPASampleFormat);
    destructor Destroy; override;
    // connect to ADevice (or the default sink when empty). Blocks until the
    // context and playback stream are ready. Returns False on failure (see
    // LastError).
    function  Open(const ADevice: AnsiString = ''): Boolean;
    // Cap the server-side buffer to roughly this many milliseconds (with
    // PA_STREAM_ADJUST_LATENCY) so the writer is paced close to real time
    // instead of racing ahead into PulseAudio's large default (~2 s) buffer.
    // 0 (default) keeps the server's default sizing. Set before Open.
    property  TargetLatencyMS: LongWord read FTargetLatencyMS write FTargetLatencyMS;
    // write ASize bytes, blocking until the server has accepted all of them.
    // returns the number of bytes written.
    function  Write(const AData; ASize: Integer): Integer;
    // block until the server has played everything written so far.
    procedure Drain;
    procedure Close;
    property  IsOpen: Boolean read FOpen;
    property  LastError: AnsiString read FLastError;
  end;

{$ENDIF}

implementation

{$IFDEF USEPULSE}

// PulseAudio invokes these as plain C callbacks. The bindings declare the
// callback types without an explicit calling convention, so we match that.
// userdata is the TPAThreadedMainloop we want to wake on each event.

procedure ContextStateCB(c: PPAContext; userdata: Pointer); cdecl;
begin
  PPAThreadedMainloop(userdata)^.Signal(False);
end;

procedure StreamStateCB(s: PPAStream; userdata: Pointer); cdecl;
begin
  PPAThreadedMainloop(userdata)^.Signal(False);
end;

procedure StreamWriteCB(p: PPAStream; nbytes: csize_t; userdata: Pointer); cdecl;
begin
  PPAThreadedMainloop(userdata)^.Signal(False);
end;

procedure StreamSuccessCB(s: PPAStream; success: cint; userdata: Pointer); cdecl;
begin
  PPAThreadedMainloop(userdata)^.Signal(False);
end;

{ TPulseAsyncStream }

constructor TPulseAsyncStream.Create(const AName: AnsiString; ARate, AChannels: Integer; AFormat: TPASampleFormat);
begin
  inherited Create;
  FName := AName;
  if FName = '' then
    FName := 'PascalAudio';
  FSpec.Format := AFormat;
  FSpec.Rate := ARate;
  FSpec.Channels := AChannels;
end;

destructor TPulseAsyncStream.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TPulseAsyncStream.Open(const ADevice: AnsiString): Boolean;
var
  cstate: TPAContextState;
  sstate: TPAStreamState;
  dev: PChar;
  FAttr: TPABufferAttr;
  ConnRes: cint;
begin
  Result := False;
  FLastError := '';

  FMainloop := pa_threaded_mainloop_new();
  if FMainloop = nil then
  begin
    FLastError := 'could not create threaded mainloop';
    Exit;
  end;
  if FMainloop^.Start <> 0 then
  begin
    FLastError := 'could not start threaded mainloop';
    Exit;
  end;

  FMainloop^.Lock;
  try
    FContext := TPAContext.New(FMainloop^.GetAPI, PChar(FName));
    if FContext = nil then
    begin
      FLastError := 'could not create context';
      Exit;
    end;
    FContext^.SetStateCallback(@ContextStateCB, FMainloop);
    if FContext^.Connect(nil, [], nil) < 0 then
    begin
      FLastError := 'context connect failed';
      Exit;
    end;

    // wait for the context to be ready (or fail).
    repeat
      cstate := FContext^.GetState;
      if cstate = csREADY then
        Break;
      if cstate in [csFAILED, csTERMINATED] then
      begin
        FLastError := 'context connection failed';
        Exit;
      end;
      FMainloop^.Wait;
    until False;

    FStream := TPAStream.New(FContext, 'playback', @FSpec, nil);
    if FStream = nil then
    begin
      FLastError := 'could not create stream';
      Exit;
    end;
    FStream^.SetStateCallback(@StreamStateCB, FMainloop);
    FStream^.SetWriteCallback(@StreamWriteCB, FMainloop);

    if ADevice = '' then
      dev := nil
    else
      dev := PChar(ADevice);

    if FTargetLatencyMS > 0 then
    begin
      // -1 (all bits set) means "let the server pick". We must bound maxlength,
      // not just tlength: the server ACCEPTS writes up to maxlength even though
      // it only TARGETS tlength, so leaving maxlength at the ~4 MB default lets
      // the writer race ~seconds ahead. Cap maxlength so back-pressure paces the
      // whole chain to real time. ADJUST_LATENCY bounds overall sink latency.
      FillChar(FAttr, SizeOf(FAttr), $FF);
      FAttr.tlength   := pa_usec_to_bytes(QWord(FTargetLatencyMS) * 1000, @FSpec);
      FAttr.maxlength := FAttr.tlength * 2;
      ConnRes := FStream^.ConnectPlayback(dev, @FAttr, LongWord(streamflagADJUST_LATENCY), nil, nil);
    end
    else
      ConnRes := FStream^.ConnectPlayback(dev, nil, 0, nil, nil);
    if ConnRes < 0 then
    begin
      FLastError := 'stream connect failed';
      Exit;
    end;

    // wait for the stream to be ready (or fail).
    repeat
      sstate := FStream^.GetState;
      if sstate = ssREADY then
        Break;
      if sstate in [ssFAILED, ssTERMINATED] then
      begin
        FLastError := 'stream connection failed';
        Exit;
      end;
      FMainloop^.Wait;
    until False;

    FOpen := True;
    Result := True;
  finally
    FMainloop^.Unlock;
  end;
end;

function TPulseAsyncStream.Write(const AData; ASize: Integer): Integer;
var
  p: PByte;
  remaining: Integer;
  writable: csize_t;
  n: csize_t;
begin
  Result := 0;
  if not FOpen then
    Exit;

  p := @AData;
  remaining := ASize;

  FMainloop^.Lock;
  try
    while remaining > 0 do
    begin
      // wait until the server can accept data.
      writable := FStream^.WritableSize;
      while (writable = 0) and (writable <> csize_t(-1)) do
      begin
        FMainloop^.Wait;
        writable := FStream^.WritableSize;
      end;
      if writable = csize_t(-1) then
      begin
        FLastError := 'writable size query failed';
        Break;
      end;

      n := remaining;
      if n > writable then
        n := writable;

      if FStream^.Write(p, n, nil, 0, smRELATIVE) < 0 then
      begin
        FLastError := 'stream write failed';
        Break;
      end;

      Inc(p, n);
      Dec(remaining, n);
      Inc(Result, n);
    end;
  finally
    FMainloop^.Unlock;
  end;
end;

procedure TPulseAsyncStream.Drain;
var
  op: PPAOperation;
begin
  if not FOpen then
    Exit;

  FMainloop^.Lock;
  try
    op := FStream^.Drain(@StreamSuccessCB, FMainloop);
    if op <> nil then
    begin
      while op^.GetState = osRUNNING do
        FMainloop^.Wait;
      op^.Unref;
    end;
  finally
    FMainloop^.Unlock;
  end;
end;

procedure TPulseAsyncStream.Close;
begin
  // stop the mainloop thread first so the rest can be torn down without
  // holding the lock or racing the callbacks.
  if Assigned(FMainloop) then
    FMainloop^.Stop;

  if Assigned(FStream) then
  begin
    FStream^.Disconnect;
    FStream^.Unref;
    FStream := nil;
  end;
  if Assigned(FContext) then
  begin
    FContext^.Disconnect;
    FContext^.Unref;
    FContext := nil;
  end;
  if Assigned(FMainloop) then
  begin
    FMainloop^.Free;
    FMainloop := nil;
  end;

  FOpen := False;
end;

{$ENDIF}

end.
