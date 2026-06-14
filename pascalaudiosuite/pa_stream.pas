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
  Classes, SysUtils, pa_base, paio_log;

type

  { TPAStreamSource }

  TPAStreamSourceClass = class of TPAStreamSource;
  TPAStreamSource = class(TPAAudioSource, IPAStream)
  protected
    FStream: TStream;
    FOwnsStream: Boolean;
    FStreamSeekable: Boolean;
    procedure SetStream(AValue: TStream); virtual;
    function GetStream: TStream; virtual;
  private
    function GetOwnsStream: Boolean;
    procedure SetOwnsStream(AValue: Boolean);

  protected
    function InternalOutputToDestination: Boolean; override;
    // True if the underlying stream supports repositioning. Seek-capable codecs
    // should fold this into their CanSeek so a non-seekable stream (a pipe, a live
    // feed) is reported honestly. Probed once when the stream is assigned.
    function StreamCanSeek: Boolean;
    // Emit a uniform warning when a seek is asked of a source that cannot seek.
    procedure LogSeekRefused;
  public
    constructor Create; override; // you must set ownsstream and stream
    constructor Create(AStream: TStream; AOwnsStream: Boolean = True); virtual;
    destructor Destroy; override;
    // IPAPlayable transport, shared by every source. Seek-capable codecs supply
    // CanSeek/SetPosition (via IPAPlayable); Stop only rewinds when the source can
    // seek -- a non-seekable source (e.g. a pipe) just halts in place.
    procedure Play; virtual;
    procedure Pause; virtual;
    procedure Stop; virtual;
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
    // called during Destroy after the worker thread has stopped but before the
    // stream is freed; subclasses (e.g. WAV) finalize/patch the stream here.
    procedure BeforeStreamFree; virtual;
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
var
  Cur: Int64;
begin
  if Assigned(FStream) and (AValue <> FStream) and FOwnsStream then
    FreeAndNil(FStream);
  FStream := AValue;

  // Probe seekability once, here, while the worker is still idle (probing later
  // would race the worker's reads). Best-effort: a seekable stream lets us read
  // and restore its position; a non-seekable one (pipe/socket) raises or fails.
  FStreamSeekable := False;
  if Assigned(FStream) then
    try
      Cur := FStream.Position;
      FStream.Position := FStream.Size; // a real seek to end...
      FStream.Position := Cur;          // ...then restore where we were.
      FStreamSeekable := True;
    except
      FStreamSeekable := False;
    end;
end;

function TPAStreamSource.StreamCanSeek: Boolean;
begin
  Result := FStreamSeekable;
end;

procedure TPAStreamSource.LogSeekRefused;
begin
  TPALog.Warning(ClassName, 'seek requested on a non-seekable stream; ignoring');
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
  DestroyWaitSync;
  if FOwnsStream and Assigned(FStream) then
    FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TPAStreamSource.Play;
begin
  if FPlayState = psPlaying then
    Exit;
  // From both psStopped and psPaused, (re)starting the pump resumes from the
  // source's current read position. StartData sets FPlayState := psPlaying.
  StartData;
end;

procedure TPAStreamSource.Pause;
begin
  if FPlayState <> psPlaying then
    Exit;
  // Halt the pump without signalling end-of-data, so downstream links/destinations
  // stay alive and simply stop receiving. The worker clears FWorking, which stops
  // the self-reposted PAM_Data cycle.
  StopData;
  FPlayState := psPaused;
end;

procedure TPAStreamSource.Stop;
var
  Playable: IPAPlayable;
begin
  if FPlayState <> psStopped then
    StopData;
  FPlayState := psStopped;
  // Rewind to the start, but only if this source actually supports seeking; a
  // non-seekable source (a pipe, a live stream) simply stops where it is.
  if Supports(Self, IPAPlayable, Playable) and Playable.CanSeek then
    Playable.SetPosition(0)
  else
    TPALog.Debug(ClassName, 'stop: stream is not seekable, staying at current position');
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

procedure TPAStreamDestination.BeforeStreamFree;
begin
  // default: nothing. subclasses finalize the stream here.
end;

destructor TPAStreamDestination.Destroy;
begin
  // Stop the worker thread first (inherited) so it can't write to the stream
  // after we free it. When we own the stream, releasing it before the thread
  // stopped was a use-after-free.
  inherited Destroy;
  BeforeStreamFree;
  Stream := nil; // this will free the stream if we own it.
end;

{ TPAStreamSource }



end.

