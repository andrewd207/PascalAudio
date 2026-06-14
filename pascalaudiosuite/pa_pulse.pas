{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2026 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

{ Suite playback destination backed by the asynchronous PulseAudio API (via
  the paio_pulse io layer), as opposed to pa_pulse_simple which uses the
  blocking libpulse-simple API. }

unit pa_pulse;

{$mode objfpc}{$H+}

interface

// USEPULSE gates the whole unit so it compiles to an empty unit where libpulse
// isn't available (e.g. Windows), matching pa_pulse_simple.
{$IFDEF USEPULSE}

uses
  Classes, SysUtils, pa_base, paio_pulse, paio_log;

type

  { TPAPulseAsyncDestination }

  TPAPulseAsyncDestination = class(TPAAudioDestination)
  private
    FStream: TPulseAsyncStream;
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
  pulse_sample;

{ TPAPulseAsyncDestination }

procedure TPAPulseAsyncDestination.Init;
var
  Info: IPAAudioInformation;
begin
  FInited := True;
  Info := DataSource.GetSourceObject as IPAAudioInformation;
  // the base converts incoming data to Format (afFloat32, set in Create), so
  // tell PulseAudio we are feeding it float32.
  FStream := TPulseAsyncStream.Create('PascalAudio', Info.SamplesPerSecond, Info.Channels, sfFloat32LE);
  if not FStream.Open then
    TPALog.Error('pa_pulse', 'async open failed: ' + FStream.LastError);
end;

procedure TPAPulseAsyncDestination.DeInit;
begin
  if Assigned(FStream) then
  begin
    FStream.Drain;
    FreeAndNil(FStream);
  end;
  FInited := False;
  EndOfData;
end;

function TPAPulseAsyncDestination.InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64;
begin
  if not FInited then
    Init;
  Result := ACount;
  FStream.Write(AData, ACount);
  if AIsLastData then
    DeInit;
end;

constructor TPAPulseAsyncDestination.Create;
begin
  BufferPool.AllocateBuffers(4);
  inherited Create;
  Format := afFloat32;
end;

destructor TPAPulseAsyncDestination.Destroy;
begin
  // inherited stops the worker thread (it uses FStream in InternalProcessData).
  inherited Destroy;
  // FStream is normally drained and freed by DeInit on the last buffer; if
  // playback was interrupted mid-stream that never ran, so free it here to
  // avoid leaking the pulse connection (mirrors the pa_pulse_simple fix).
  if Assigned(FStream) then
    FreeAndNil(FStream);
end;

{$ENDIF}

end.
