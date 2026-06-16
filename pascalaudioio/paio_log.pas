{
    This unit is part of Pascal Audio IO package.

    Copyright (c) 2016 by Andrew Haines.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit paio_log;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPALogLevel = (llDebug, llInfo, llWarning, llError);

  // The handler is called from whichever thread emitted the message -- including
  // the pipeline worker threads -- so an implementation MUST be thread-safe.
  TPALogEvent = procedure(ALevel: TPALogLevel; const ASource, AMessage: String) of object;

  EPALog = class(Exception);

  { TPALog

    Global, thread-safe log sink. This is a purely static class: it holds no
    per-instance state and must never be instantiated (the constructor raises).
    Use the class methods, and set a single process-wide Handler to capture
    output. With no handler set the default is to write to StdErr, serialized so
    messages from different threads don't interleave. }

  TPALog = class
  private
    class var FHandler: TPALogEvent;
    class var FMinLevel: TPALogLevel;
    class var FLock: TRTLCriticalSection;
    class function  GetHandler: TPALogEvent; static;
    class procedure SetHandler(AValue: TPALogEvent); static;
    class function  GetMinLevel: TPALogLevel; static;
    class procedure SetMinLevel(AValue: TPALogLevel); static;
  public
    // TPALog is static; instantiating it is a programming error.
    constructor Create;

    // Each method comes in two forms: a plain one that logs the message text
    // verbatim, and a Format one taking an "array of const" -- Pascal can't
    // give an open array a default value, so this is an overload rather than a
    // defaulted parameter. The plain form does NOT run Format, so it is safe
    // for arbitrary text containing stray '%'.
    class procedure Log(ALevel: TPALogLevel; const ASource, AMessage: String); static; overload;
    class procedure Log(ALevel: TPALogLevel; const ASource, AFmt: String; const AArgs: array of const); static; overload;
    class procedure Debug  (const ASource, AMessage: String); static; overload;
    class procedure Debug  (const ASource, AFmt: String; const AArgs: array of const); static; overload;
    class procedure Info   (const ASource, AMessage: String); static; overload;
    class procedure Info   (const ASource, AFmt: String; const AArgs: array of const); static; overload;
    class procedure Warning(const ASource, AMessage: String); static; overload;
    class procedure Warning(const ASource, AFmt: String; const AArgs: array of const); static; overload;
    class procedure Error  (const ASource, AMessage: String); static; overload;
    class procedure Error  (const ASource, AFmt: String; const AArgs: array of const); static; overload;

    // Messages below this level are dropped. Defaults to llWarning, so routine
    // Info "initialized" chatter stays off the console; set llInfo/llDebug to
    // see more.
    class property MinLevel: TPALogLevel read GetMinLevel write SetMinLevel;
    // The single process-wide sink. Set to nil to restore the StdErr default.
    class property Handler: TPALogEvent read GetHandler write SetHandler;
  end;

implementation

const
  LOG_LEVEL_NAMES: array[TPALogLevel] of String =
    ('DEBUG', 'INFO', 'WARN', 'ERROR');

constructor TPALog.Create;
begin
  raise EPALog.Create('TPALog is a static class and must not be instantiated; use its class methods.');
end;

class function TPALog.GetHandler: TPALogEvent;
begin
  EnterCriticalSection(FLock);
  try
    Result := FHandler;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

class procedure TPALog.SetHandler(AValue: TPALogEvent);
begin
  EnterCriticalSection(FLock);
  try
    FHandler := AValue;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

class function TPALog.GetMinLevel: TPALogLevel;
begin
  EnterCriticalSection(FLock);
  try
    Result := FMinLevel;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

class procedure TPALog.SetMinLevel(AValue: TPALogLevel);
begin
  EnterCriticalSection(FLock);
  try
    FMinLevel := AValue;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

class procedure TPALog.Log(ALevel: TPALogLevel; const ASource, AMessage: String);
var
  H: TPALogEvent;
begin
  // Snapshot the handler and level threshold under the lock, then release it
  // before invoking the handler: holding the lock across a user callback would
  // serialize all logging on a possibly-slow sink and deadlock if the handler
  // logs again (re-entrancy).
  EnterCriticalSection(FLock);
  try
    if ALevel < FMinLevel then
      Exit;
    H := FHandler;
  finally
    LeaveCriticalSection(FLock);
  end;

  if Assigned(H) then
    H(ALevel, ASource, AMessage)
  else
  begin
    // No handler: fall back to StdErr, but serialize the write so concurrent
    // worker threads don't interleave their output mid-line.
    EnterCriticalSection(FLock);
    try
      WriteLn(StdErr, '[', LOG_LEVEL_NAMES[ALevel], '] ', ASource, ': ', AMessage);
      Flush(StdErr);
    finally
      LeaveCriticalSection(FLock);
    end;
  end;
end;

class procedure TPALog.Log(ALevel: TPALogLevel; const ASource, AFmt: String; const AArgs: array of const);
var
  S: String;
begin
  // A logging call must never throw, so a malformed format string is caught and
  // surfaced rather than propagated to the caller.
  try
    S := Format(AFmt, AArgs);
  except
    on E: Exception do
      S := AFmt + ' [log format error: ' + E.Message + ']';
  end;
  Log(ALevel, ASource, S);
end;

class procedure TPALog.Debug(const ASource, AMessage: String);
begin
  Log(llDebug, ASource, AMessage);
end;

class procedure TPALog.Debug(const ASource, AFmt: String; const AArgs: array of const);
begin
  Log(llDebug, ASource, AFmt, AArgs);
end;

class procedure TPALog.Info(const ASource, AMessage: String);
begin
  Log(llInfo, ASource, AMessage);
end;

class procedure TPALog.Info(const ASource, AFmt: String; const AArgs: array of const);
begin
  Log(llInfo, ASource, AFmt, AArgs);
end;

class procedure TPALog.Warning(const ASource, AMessage: String);
begin
  Log(llWarning, ASource, AMessage);
end;

class procedure TPALog.Warning(const ASource, AFmt: String; const AArgs: array of const);
begin
  Log(llWarning, ASource, AFmt, AArgs);
end;

class procedure TPALog.Error(const ASource, AMessage: String);
begin
  Log(llError, ASource, AMessage);
end;

class procedure TPALog.Error(const ASource, AFmt: String; const AArgs: array of const);
begin
  Log(llError, ASource, AFmt, AArgs);
end;

initialization
  InitCriticalSection(TPALog.FLock);
  // default to Warning so routine "initialized" Info chatter stays off the
  // console; apps that want it can call TPALog.MinLevel := llInfo (or llDebug).
  TPALog.FMinLevel := llWarning;

finalization
  DoneCriticalSection(TPALog.FLock);

end.
