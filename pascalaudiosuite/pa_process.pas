{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_process;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pa_base, process;

type

  { TPAProcessSource }

  TPAProcessSource = class;

  //useful to continue generating data for output from multiple commands
  TPAProcessEndedEvent = procedure (ASource: TPAProcessSource; var ARunAgain: Boolean)of Object;

  TPAProcessSource = class(TPAAudioSource, IPAAudioInformation)
  protected
    FProcess: TProcess;
    //used by DoProcessEnded;
    FRunAgain: Boolean;
    FAllowProcessAccess: Boolean;
    procedure RunProcess;
    procedure BeforeExecuteLoop; override;
    function InternalOutputToDestination: Boolean; override;
  private
    FOnProcessEnded: TPAProcessEndedEvent;
    function GetProcess: TProcess;
    // IPAAudioInformation
    procedure DoProcessEnded;
  public
    constructor Create; override;
    destructor Destroy; override;
    property  Process: TProcess read GetProcess; // returns nil once started
    property  OnProcessEnded: TPAProcessEndedEvent read FOnProcessEnded write FOnProcessEnded;
  end;

implementation

{ TPAProcessSource }

procedure TPAProcessSource.RunProcess;
begin
  //WriteLn('Running Process');
  FProcess.Execute;
end;

procedure TPAProcessSource.BeforeExecuteLoop;
begin
  inherited BeforeExecuteLoop;
  RunProcess;
end;

function TPAProcessSource.InternalOutputToDestination: Boolean;
var
  Buf: array[0..AUDIO_BUFFER_SIZE-1] of byte;
  RCount: Integer;
begin
  while (FProcess.Stderr.NumBytesAvailable > 0) do
  begin
    // clear stderr
    RCount := FProcess.Stderr.NumBytesAvailable;
    if RCount > SizeOf(Buf) then
      RCount := SizeOf(Buf);
    FProcess.Stderr.Read(Buf, RCount);
    Write(StdErr,Copy(PChar(@Buf),0,RCount));
  end;

  if FProcess.Running or (FProcess.Output.NumBytesAvailable > 0) then
  begin
    RCount := FProcess.Output.NumBytesAvailable;
    //WriteLn('Read ', RCount);
    if RCount > SizeOf(Buf) then
      RCount := SizeOf(Buf);
    RCount := FProcess.Output.Read(Buf, SizeOf(Buf));
    if RCount > 0 then
      WriteToBuffer(Buf, RCount, False);
  end;

  // still data left so call again
  if (FProcess.Output.NumBytesAvailable > 0) then
    Exit(True);

  // process is still running so there might be more data
  if FProcess.Running then
    Exit(True)
  else
    ;//WriteLn('Exit Status = ', FProcess.ExitStatus);

  // last chance for more data. check to run process again
  if Assigned(FOnProcessEnded) then
  begin
    Synchronize(@DoProcessEnded);
    if FRunAgain then
    begin
      // if the callback want's us to run the process again with possibly new
      // commands, do it to keep generating data
      RunProcess;
      Exit(True);
    end;
  end;

  // there is no data and no running process so we're done
  Result := False;
  SignalDestinationsDone;
end;

function TPAProcessSource.GetProcess: TProcess;
begin
  // FAllowProcessAccess is set and unset in DoProcessEnded to allow access safely when syncing

  if Working and (not FAllowProcessAccess) then
    Exit(nil);
  Result := FProcess;
end;

procedure TPAProcessSource.DoProcessEnded;
begin
  // run from synchronize
  //WriteLn('Checking want more');
  FAllowProcessAccess:=True;
  FRunAgain:=False;
  if Assigned(FOnProcessEnded) then
    FOnProcessEnded(Self, FRunAgain);
  FAllowProcessAccess:=False;
end;

constructor TPAProcessSource.Create;
begin
  inherited Create;
  FProcess := TProcess.Create(nil);
  FProcess.Options:=[poUsePipes, poNoConsole];
end;

destructor TPAProcessSource.Destroy;
begin
  if FProcess.Running then
    FProcess.Terminate(0);
  FProcess.Free;
  inherited Destroy;
end;

end.

