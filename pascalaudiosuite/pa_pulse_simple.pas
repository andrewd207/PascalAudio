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
  Classes, SysUtils, pa_base, pulse_simple;


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
  Info := DataSource.GetSourceObject as IPAAudioInformation;
  SS.Init;
  SS.Channels:=Info.Channels;
  SS.Rate:=Info.SamplesPerSecond;
  SS.Format:=sfFloat32LE;

  //WriteLn('Pulse Channels = ', SS.Channels);
  //Writeln('Pulse Rate = ', SS.Rate);

  FPulse:=TPASimple.New(nil,PChar(ParamStr(0)),sdPLAYBACK, nil, 'test', @SS, nil, nil, @error);
  if error < 0 then
    WriteLn('Error initing pulse data ',pa_strerror(error));
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
begin
  if not FInited then
    Init;
  Result := ACount;
  //WriteLn('Writing to pulse');
  FPulse^.Write(@AData, ACount, @error);
  if error < 0 then
    WriteLn('Error Writin pulse data ',pa_strerror(error));
  if AIsLastData then
    DeInit;
end;

constructor TPAPulseDestination.Create;
begin
  BufferPool.AllocateBuffers(4);
  inherited Create;
  Format := afFloat32;
end;


destructor TPAPulseDestination.Destroy;
begin
  inherited Destroy;
end;
{$ENDIF}
end.

