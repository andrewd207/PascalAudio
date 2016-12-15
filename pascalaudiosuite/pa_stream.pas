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

  TPAStreamSourceClass = class of TPAStreamSource;
  TPAStreamSource = class(TPAAudioSource, IPAStream)
  protected
    FStream: TStream;
    FOwnsStream: Boolean;
    procedure SetStream(AValue: TStream); virtual;
    function GetStream: TStream; virtual;
  private
    function GetOwnsStream: Boolean;
    procedure SetOwnsStream(AValue: Boolean);

  protected
    function InternalOutputToDestination: Boolean; override;
  public
    constructor Create; override; // you must set ownsstream and stream
    constructor Create(AStream: TStream; AOwnsStream: Boolean = True); virtual;
    destructor Destroy; override;
    property  Stream: TStream read GetStream write SetStream;
    property  OwnsStream: Boolean read GetOwnsStream write SetOwnsStream;
  end;

  { TPAStreamDestination }

  TPAStreamDestinationClass = class of TPAStreamDestination;
  TPAStreamDestination = class(TPAAudioDestination, IPAStream)
  private
    FOwnsStream: Boolean;
    function GetOwnsStream: Boolean;
    procedure SetOwnsStream(AValue: Boolean);
  protected
    FStream: TStream;
    function GetStream: TStream; virtual;
    procedure SetStream(AValue: TStream); virtual;
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; override;
    procedure EndOfData; override;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean); virtual;
    destructor  Destroy; override;
    property  Stream: TStream read GetStream write SetStream;
    property  OwnsStream: Boolean read GetOwnsStream write SetOwnsStream;
  end;

implementation

{ TPAStreamSource }

function TPAStreamSource.GetStream: TStream;
begin
  Result := FStream;
end;

function TPAStreamSource.GetOwnsStream: Boolean;
begin
  Result := FOwnsStream;
end;

procedure TPAStreamSource.SetOwnsStream(AValue: Boolean);
begin
  FOwnsStream := AValue;
end;

procedure TPAStreamSource.SetStream(AValue: TStream);
begin
  if Assigned(FStream) and (AValue <> FStream) and FOwnsStream then
    FreeAndNil(FStream);
  FStream := AValue;
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

constructor TPAStreamSource.Create;
begin
  Create(nil, True);
end;

constructor TPAStreamSource.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;
  OwnsStream := AOwnsStream;
  Stream := AStream;
end;

destructor TPAStreamSource.Destroy;
begin
  if FOwnsStream and Assigned(FStream) then
    FreeAndNil(FStream);
  inherited Destroy;
end;

{ TPAStreamDestination }

function TPAStreamDestination.GetStream: TStream;
begin
  Result := FStream;
end;

function TPAStreamDestination.GetOwnsStream: Boolean;
begin
  Result := FOwnsStream;
end;

procedure TPAStreamDestination.SetOwnsStream(AValue: Boolean);
begin
  FOwnsStream := AValue;
end;

procedure TPAStreamDestination.SetStream(AValue: TStream);
begin
  if Assigned(FStream) and FOwnsStream then
    FreeAndNil(FStream);
  FStream := AValue;
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

constructor TPAStreamDestination.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  FStream := AStream;
  inherited Create;
  Format := afRaw;
end;

destructor TPAStreamDestination.Destroy;
begin
  Stream := nil; // this will free the stream if we own it.
  inherited Destroy;
end;

{ TPAStreamSource }



end.

