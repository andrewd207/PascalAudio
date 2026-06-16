{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_pulse_simple;

{$mode objfpc}{$H+}

interface
{$IFDEF USEPULSE}
uses
  Classes, SysUtils, pa_base, pa_register, pulse_simple, paio_log;


type
  { TPAPulseDestination }

  TPAPulseDestination = class(TPAAudioDestination)
  private
    FPulse: PPASimple;
    FInited: Boolean;
    procedure Init;
    procedure DeInit;
  protected
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;
  {$ENDIF}

implementation
{$IFDEF USEPULSE}
uses
  pulse_error, pulse_def, pulse_sample, ctypes;


{ TPAPulseDestination }

procedure TPAPulseDestination.Init;
var
  SS: TPASampleSpec;
  Info: IPAAudioInformation;
  error: cint;
begin
  FInited:=True;
  TPALog.Info(ClassName, 'initialized');
  Info := DataSource.GetSourceObject as IPAAudioInformation;
  SS.Init;
  SS.Channels:=Info.Channels;
  SS.Rate:=Info.SamplesPerSecond;
  SS.Format:=sfFloat32LE;

  //WriteLn('Pulse Channels = ', SS.Channels);
  //Writeln('Pulse Rate = ', SS.Rate);

  error := 0;
  FPulse:=TPASimple.New(nil,PChar(ParamStr(0)),sdPLAYBACK, nil, 'test', @SS, nil, nil, @error);
  // New returns nil on failure (and only then is error meaningful); test that
  // rather than the possibly-uninitialised error out-param.
  if FPulse = nil then
    TPALog.Error('pa_pulse_simple', 'init failed: ' + pa_strerror(error));
end;

procedure TPAPulseDestination.DeInit;
begin
  FPulse^.Drain(nil);
  FPulse^.Free;
  FPulse := nil;
  FInited:=False;
  EndOfData;
end;

function TPAPulseDestination.InternalProcessData(const AData; ACount: Int64;
  AIsLastData: Boolean): Int64;
var
  Error: cint;
  Res: cint;
begin
  if not FInited then
    Init;
  Result := ACount;
  // Only write when there's actually data: an end-of-data buffer can arrive
  // empty, and a zero-length write is pointless.
  if ACount > 0 then
  begin
    Error := 0;
    // Check Write's RETURN value, not the error out-param: pa_simple_write only
    // sets *error when it fails (return < 0). The old code tested the out-param
    // directly, so on success it read uninitialised stack garbage and logged
    // spurious "write failed:" errors.
    Res := FPulse^.Write(@AData, ACount, @Error);
    if Res < 0 then
      TPALog.Error('pa_pulse_simple', 'write failed: ' + pa_strerror(Error));
  end;
  if AIsLastData then
    DeInit;
end;

constructor TPAPulseDestination.Create;
begin
  inherited Create; // base destination provides its 2 pooled buffers
  Format := afFloat32;
end;


destructor TPAPulseDestination.Destroy;
begin
  // inherited stops the worker thread (it uses FPulse in InternalProcessData).
  inherited Destroy;
  // FPulse is normally freed by DeInit when the last buffer arrives; if playback
  // was interrupted (destroyed mid-stream) that never ran, so free it here to
  // avoid leaking the pulse connection.
  if FInited and Assigned(FPulse) then
  begin
    FPulse^.Drain(nil);
    FPulse^.Free;
    FPulse := nil;
    FInited := False;
  end;
end;
initialization
  PARegister(partDeviceOut, TPAPulseDestination, 'PulseAudio');
{$ENDIF}
end.

