{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_stream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pa_base;

type

  { TPAStreamSource }

  TPAStreamSource = class(TPAAudioSource, IPAStream)
  private
    FStream: TStream;
    function GetStream: TStream;
  protected
    function InternalOutputToDestination: Boolean; override;
  public
    property  Stream: TStream read GetStream write FStream;
  end;

  { TPAStreamDestination }

  TPAStreamDestination = class(TPAAudioDestination, IPAStream)
  private
    FStream: TStream;
    function GetStream: TStream;
  protected
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; override;
    procedure EndOfData; override;
  public
    constructor Create(AStream: TStream);
    property  Stream: TStream read GetStream write FStream;
  end;

implementation

{ TPAStreamSource }

function TPAStreamSource.GetStream: TStream;
begin
  Result := FStream;
end;

function TPAStreamSource.InternalOutputToDestination: Boolean;
var
  Buffer: PAudioBuffer;
begin

  Buffer := BufferPool.GetBufferFromPool(True);
  Buffer^.UsedData := FStream.Read(Buffer^.Data, AUDIO_BUFFER_SIZE);
  Buffer^.IsEndOfData:= Buffer^.UsedData < AUDIO_BUFFER_SIZE;

  Result := Buffer^.UsedData > 0;
  WriteToDestinations(Buffer);
  if Not Result then
    SignalDestinationsDone;

end;

{ TPAStreamDestination }

function TPAStreamDestination.GetStream: TStream;
begin
  Result := FStream;
end;

function TPAStreamDestination.InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64;
begin
  if Assigned(FStream) then
    Result := FStream.Write(AData, ACount);

  if AIsLastData then
    EndOfData;

end;

procedure TPAStreamDestination.EndOfData;
begin
  inherited EndOfData;
  //WriteLn('StreamDEstination EndOf Data;');
  FBufferManager.Flush;
end;

constructor TPAStreamDestination.Create(AStream: TStream);
begin
  FStream := AStream;
  inherited Create;
  Format := afRaw;
end;

{ TPAStreamSource }



end.

