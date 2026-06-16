{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_base;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, syncobjs, pa_lists, paio_types, paio_messagequeue, paio_log, fgl;

const
  AUDIO_BUFFER_SIZE  = paio_types.AUDIO_BUFFER_SIZE;
  AUDIO_BUFFER_FLOAT_SAMPLES = paio_types.AUDIO_BUFFER_FLOAT_SAMPLES;

  // messages for eventqueue
  PAM_ObjectIsDestroying = -1;
  PAM_Data               = 0;
  PAM_DataEnd            = 1;
  PAM_StopWorking        = 2;
  PAM_SendBuffer         = 3;


  PAM_Seek               = 100;



type
  TSingleArray = paio_types.TSingleArray;
  TChannelArray = paio_types.TChannelArray;

  TPAAudioFormat = (
                 afRaw, // used to write the audio data directly without converting. Used for encoded data like ogg output.
                 afS16,
                 afFloat32
                 );

  // Transport state for a source. Distinct from Working: a paused source is not
  // Working (its pump is halted) but has NOT reached end-of-data, so a player can
  // tell "paused" apart from "finished".
  TPAPlayState = (psStopped, psPlaying, psPaused);

  PAudioBuffer = ^TAudioBuffer;
  TAudioBuffer = record
    Data: array [0..AUDIO_BUFFER_SIZE-1] of byte; // 4096 samples @ 2 bytes per sample.
    UsedData: Integer;
    IsEndOfData: Boolean;
    Format: TPAAudioFormat;
  end;


  { IPAAudioInformation }

  IPAAudioInformation = interface
    ['IPAAudioInformation']
  // private
    function  GetSamplesPerSecond: Integer;
    procedure SetSamplesPerSecond(AValue: Integer);
    function  GetFormat: TPAAudioFormat;
    procedure SetFormat(AValue: TPAAudioFormat);
    function  GetChannels: Integer;
    procedure SetChannels(AValue: Integer);
  // public
    property  Channels: Integer read GetChannels write SetChannels;
    property  SamplesPerSecond: Integer read GetSamplesPerSecond write SetSamplesPerSecond;
    property  Format: TPAAudioFormat read GetFormat write SetFormat;
  end;

  IPAAudioSource = interface;

  { IPAAudioDestination }

  IPAAudioDestination = interface
    ['IPAAudioDestination']
    function  GetDataSource: IPAAudioSource;
    procedure SetDataSource(AValue: IPAAudioSource);
    //function  WriteData(const Data; ACount: Int64): Int64;
    function  WriteBuffer(ABuffer: PAudioBuffer): Boolean;
    procedure SetDataEvent;
    procedure EndOfData;

    function  GetObject: TObject;
    property  DataSource: IPAAudioSource read GetDataSource write SetDataSource;
  end;

  IPAAudioSource = interface
    ['IPAAudioSource']
    procedure AddDestination(AValue: IPAAudioDestination);
    procedure RemoveDestination(AValue: IPAAudioDestination);
    //procedure WriteToDestinations(const Data; ASize: PtrUInt);
    procedure WriteToDestinations(const ABuffer: PAudioBuffer);
    function  GetSourceObject: TObject;
  end;

  IPAPlayable = interface
    ['IPAPlayable']
    function  CanSeek: Boolean;
    function  GetPosition: Double;
    procedure SetPosition(AValue: Double);
    function  GetMaxPosition: Double;
    procedure Play;
    procedure Pause;
    procedure Stop;
    property  Position: Double read GetPosition write SetPosition;
    property  MaxPosition: Double read GetMaxPosition;
  end;

  IPAStream = interface
    ['IPAStream']
    function GetStream: TStream;
    procedure SetStream(AValue: TStream);
    property Stream: TStream read GetStream write SetStream;
    property OwnsStream: Boolean;
  end;

  // One send message carries the produced buffer plus every destination that
  // still wants it. The worker thread does the fan-out: the last destination
  // gets the original buffer, earlier ones get copies (so a copy is only ever
  // made when there is real fan-out). See TPAAudioSource.DeliverBufferMessage.
  TPABufferMessage = class(TPAIOMessage)
    Buffer: PAudioBuffer;
    Dests: array of IPAAudioDestination;
    constructor Create(ABuffer: PAudioBuffer; const ADests: array of IPAAudioDestination);
    destructor Destroy; override;
  end;

  { TPABufferEvent }

  TPABufferEvent = class(TSimpleEvent)
  private
    FIsSet: Boolean;
    // dedicated lock guarding FIsSet together with its paired underlying
    // Set/ResetEvent syscall. SetEvent and ResetEvent run on different threads
    // (producer vs consumer); without this the cached FIsSet flag could be read
    // stale, skipping the real signal -> a missed wakeup (up to a WaitFor
    // timeout of latency, or a dropped buffer). It is intentionally a separate
    // critical section so it can never block on the pool/manager locks.
    FCrit: TRTLCriticalSection;
  public
    Buffer: PAudioBuffer;
    OwnerDestroyed: Boolean; // means buffer pool should free the event
    constructor Create;
    destructor Destroy; override;
    procedure SetEvent;
    procedure ResetEvent;
    function IsSet: Boolean;
  end;
  { TAudioLink }

  { TPAAudioInformationBase }

  TPAAudioInformationBase = class(TThread, IPAAudioInformation)
  protected
    // These variables can contain a different value from Format. if DataSource is assigned then Datasource's values are used.
    FSamplesPerSecond: Integer;
    FFormat: TPAAudioFormat;
    FChannels: Integer;
    FMsgQueue: TPAIOMessageQueue;
    // IPAAudioInformation
    function  GetSamplesPerSecond: Integer; virtual;
    function  GetFormat: TPAAudioFormat; virtual;
    function  GetChannels: Integer; virtual;
    procedure SetSamplesPerSecond(AValue: Integer); virtual;
    procedure SetFormat(AValue: TPAAudioFormat); virtual;
    procedure SetChannels(AValue: Integer); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    // IPAAudioInformation
    property  Channels: Integer read GetChannels write SetChannels;
    property  SamplesPerSecond: Integer read GetSamplesPerSecond write SetSamplesPerSecond;
    property  Format: TPAAudioFormat read GetFormat write SetFormat;
  end;

  { TPAAudioSource }

  TPAAudioSourceClass = class of TPAAudioSource;
  // First link in a chain.
  TPAAudioSource = class(TPAAudioInformationBase, IPAAudioSource)
  private
    FFreeEvent: TSimpleEvent; // only used during the destructor
    FDestroySynced: Boolean;  // ensures DestroyWaitSync only runs once
    FSignaled: Boolean;
    FDestBuf: PAudioBuffer;
    //Datasource: IPAAudioSource;
    procedure EnsureAudioBuffer;
  protected
    function  DestroyWaitSync: Boolean;
    procedure Execute; override;
    procedure BeforeExecuteLoop; virtual;
    procedure AfterExecuteLoop; virtual;
    function InternalOutputToDestination: Boolean; virtual; abstract;
    function HandleMessage(var AMsg: TPAIOMessage): Boolean; virtual;
    // IPAAudioSource
    procedure AddDestination(AValue: IPAAudioDestination); virtual;
    procedure RemoveDestination(AValue: IPAAudioDestination); virtual;
    procedure WriteToDestinations(const ABuffer: PAudioBuffer);{ virtual;}
    function  WriteToBuffer(const AData; ASize: Integer; AIsLastData: Boolean): Integer;
    // Fan a send message out to its destinations: the last one gets the original
    // buffer, earlier ones get copies. Returns True when every destination has
    // accepted it (message can be freed), or False if a destination's queue was
    // full -- the message keeps the not-yet-delivered destinations and the caller
    // re-queues it to retry. Runs on the worker thread.
    function  DeliverBufferMessage(AMsg: TPABufferMessage): Boolean;
    // Deliver pending PAM_SendBuffer messages (output we've produced but not yet
    // handed downstream) without producing more, so a thread blocked in
    // GetBufferFromPool can unstick the chain. Runs on this stage's worker thread.
    procedure FlushPendingSends;
    procedure SignalDestinationsDone; virtual;
    function  GetSourceObject: TObject;
    procedure ClearSignal;

  private
    FDestinations: specialize TFPGList<IPAAudioDestination>;
    FWorking: Boolean;
  protected
    // owned by the transport (Play/Pause/Stop in TPAStreamSource) and the worker
    // loop, which sets psStopped when the source reaches end-of-data.
    FPlayState: TPAPlayState;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure StartData;
    function  Working: Boolean;
    procedure StopData; // Unsets wait event
    property  PlayState: TPAPlayState read FPlayState;
  end;

  PBufferWaitEvent = ^TBufferWaitEvent;
  TBufferWaitEvent = record
    WaitEvent: TPABufferEvent;
    Next : PBufferWaitEvent
  end;

  { TPABufferPool }

  TPABufferPool = class
    FCrit: TRTLCriticalSection;
    FBuffers: TList; // has both used and unused buffers
    FBufferList: TPAFifoList; // only has available buffers
    FWaitList: TPAFifoList;
    FTargetCount: Integer; // how many buffers the pool should own (sum of live
                           // managers' counts); returns above this are freed, not
                           // pooled, so the pool can't grow across chains.
    function CheckWaitFor(ABuffer: PAudioBuffer): Boolean;
    procedure WriteAvailBuffers;
    function BufferInPool(ABuf: PAudioBuffer): Boolean;
  public
    procedure AllocateBuffers(ACount: Integer);
    // Remove up to ACount currently-free buffers from the pool and release their
    // memory. Called when a buffer manager is destroyed so the global (shared,
    // singleton) pool stays balanced: without it every chain we build adds
    // ACount buffers that are never reclaimed, so opening song after song grows
    // the pool unbounded -- which weakens back-pressure and lets passthrough
    // links (the FFT analyser) race ahead of playback, flooding/dropping frames.
    procedure DeallocateBuffers(ACount: Integer);
    procedure FreeBuffers;
    // APump, if given, is called while blocked waiting for a buffer. A producer
    // can block here mid-InternalProcessData while output it already produced is
    // still queued (as PAM_SendBuffer) on its own message queue, undelivered --
    // so the destination starves and never returns a buffer. APump is the
    // caller's FlushPendingSends, which delivers that queued output so the
    // destination can drain it and return buffers. Without it the blocking pool
    // deadlocks (blocked producer <-> starved consumer).
    function  GetBufferFromPool(AWait: Boolean = True; APump: TThreadMethod = nil): PAudioBuffer;
    procedure ReturnBufferToPool(ABuffer: PAudioBuffer);
    function  TotalBufferCount: Integer; // all buffers the pool owns (used + free)
    function  FreeBufferCount: Integer;  // buffers currently available
    constructor Create;
    destructor Destroy; override;
  end;

  { TPAAudioBufferManager }

  TPAAudioBufferManager = class
  private
    FBufferUsed: TPABufferEvent;
    FOwner: IPAAudioDestination;
    FCanDropData: Boolean;
    FBuffers: TPAFifoList;
    FBufferCount: Integer; // used to limit buffers we use from the pool
    FBuffersWaitingToQueue: Integer;
    function GetQueuedBufferCount: Integer;
  public
    constructor Create(AOwner: IPAAudioDestination; ABufferCount: Integer; ADropDataIfFull: Boolean);
    destructor  Destroy; override;
    procedure Flush;
    function  WriteBuffer(ABuffer: PAudioBuffer): Boolean;
    function  NextFilledBuffer: PAudioBuffer; // removes the buffer from the manager. caller must return it to the pool
    function  Empty: Boolean;
  end;

  { TPAAudioDestination }

  TPAAudioDataEndEvent = procedure(Sender: IPAAudioDestination; AUserObj: TObject) of object;

  TPAAudioDestinationClass = class of TPAAudioDestination;
  // last link in a chain
  TPAAudioDestination = class(TPAAudioInformationBase, IPAAudioDestination)
  private
    FFreeObject: TSimpleEvent;
    FDataSource: IPAAudioSource;
    FonDataEnded: TPAAudioDataEndEvent;
    FUserObj: TObject;
    function GetOnDataEnded(AUserObj: TObject): TPAAudioDataEndEvent;
    function GetWorking: Boolean;
    procedure SetOnDataEnded(AUserObj: TObject; AValue: TPAAudioDataEndEvent);
    procedure DoOnDataEnded;
  protected
    FBufferManager: TPAAudioBufferManager;
    FDataIsEnded: Boolean;
    procedure Execute; override;
    // internal
    function  GetDataSource: IPAAudioSource;
    procedure SetDataSource(AValue: IPAAudioSource);
    // actually writes the data from the queue to the destination from inside the thread
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; virtual; abstract;
    procedure EndOfData; virtual;
    function  GetObject: TObject;
    // a destination's channel count / sample rate describe the audio it
    // receives, so report the upstream source's values (like TPAAudioLink does)
    // rather than the defaults. Encoders/sinks read these (e.g. the vorbis
    // encoder and WAV header) and otherwise saw 2ch / 44100 for every source.
    function  GetChannels: Integer; override;
    function  GetSamplesPerSecond: Integer; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    // queues data in a buffer to be written as the thread can process it
    function  WriteBuffer(ABuffer: PAudioBuffer): Boolean;
    procedure SetDataEvent; {virtual;}
    property  Working: Boolean read GetWorking;
    property  DataSource: IPAAudioSource read GetDataSource write SetDataSource;
    property  OnDataEnded[AUserObj: TObject]: TPAAudioDataEndEvent read GetOnDataEnded write SetOnDataEnded;
  end;

  TPAAudioLinkClass = class of TPAAudioLink;
  // TAudioLink has something before and something after it in the chain

  { TPAAudioLink }

  TPAAudioLink = class(TPAAudioSource, IPAAudioSource, IPAAudioDestination, IPAAudioInformation)
  private
    FDataSource: IPAAudioSource;
    FCrit: TRTLCriticalSection;
  protected
    FDataIsEnded: Boolean;
    FBufferManager: TPAAudioBufferManager;
    procedure BufferEmpty;  // call this after a PAudioBuffer has been processed
    // for IPAAudioSource. processes input data and writes it to destinations
    function  InternalOutputToDestination: Boolean; override;
    // for IPAAudioDestination. writes data in buffer to whatever codec etc
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; virtual; abstract;
    // IPAAudioDestination
    function  GetDataSource: IPAAudioSource; virtual;
    procedure SetDataSource(AValue: IPAAudioSource); virtual;
    function  WriteBuffer(ABuffer: PAudioBuffer): Boolean; virtual;
    // IPAAudioInformation
    function  GetSamplesPerSecond: Integer; override;
    function  GetFormat: TPAAudioFormat; override;
    function  GetChannels: Integer; override;
    procedure SetDataEvent;
    procedure EndOfData; virtual; // called by audiosource
    function  GetObject: TObject;

    procedure Execute; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property  DataSource: IPAAudioSource read GetDataSource write SetDataSource;
  end;

  TPAAudioConvertProc = function (InData: Pointer; AInDataSize: Integer; var AOutSize: Integer): Pointer;

  var
    DefaultAudioFormat: TPAAudioFormat = afFloat32;

    //usage NewData := ConvertAudio[FromFormat][ToFormat](InData, InDataSize, OutDataSize);
    ConvertAudio: array[TPAAudioFormat] of array[TPAAudioFormat] of TPAAudioConvertProc;


  function BytesPerSample(AFormat: TPAAudioFormat): Integer;

  function NewChannelArray(AChannels: Integer; ASamplesPerChannel: Integer): TChannelArray;
  function SplitChannels(AData: PSingle; ASamples: Integer; AChannels: Integer): TChannelArray;
  function JoinChannels(AChannelData: TChannelArray; ASamples: Integer = -1): TSingleArray;
  function JoinChannels(AChannelData: PPSingle; AChannels: Integer; ASamples: Integer): TSingleArray;

  // Merge multiple channels into a mono channel
  // function MergeChannels(AChannelData: TChannelArray): TSingleArray;

   function Min(A,B: Integer): Integer;
   function Max(A,B: Integer): Integer;

function BufferPool: TPABufferPool; // threadsafe class

implementation
uses
  paio_utils, Math;

// Audio DSP and the C codec libraries (libvorbis, libsamplerate, ...) routinely
// produce denormals, NaNs and infinities as ordinary intermediate values. FPC
// leaves the FPU exceptions unmasked by default, and the control word is
// per-thread, so each worker thread must mask them or libvorbis float math
// raises "Floating point overflow/invalid operation/division by zero".
procedure MaskAudioFPExceptions;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
end;

var
  InternalBufferPool: TPABufferPool = nil;

function Min(A,B: Integer): Integer;
begin
  if A < B then Exit(A);
  Result := B;
end;

function Max(A,B: Integer): Integer;
begin
  if A > B then Exit(A);
  Result := B;
end;

function BytesPerSample(AFormat: TPAAudioFormat): Integer;
begin
  case AFormat of
    afRaw:     Result := 0;
    afS16:     Result := 2;
    afFloat32: Result := 4;
  end;
end;

function NewChannelArray(AChannels: Integer; ASamplesPerChannel: Integer): TChannelArray;
begin
  Result := paio_utils.NewChannelArray(AChannels, ASamplesPerChannel);
end;

function SplitChannels(AData: PSingle; ASamples: Integer; AChannels: Integer): TChannelArray;
begin
  Result := paio_utils.SplitChannels(AData, ASamples, AChannels);
end;

function JoinChannels(AChannelData: TChannelArray; ASamples: Integer): TSingleArray;
begin
  Result := paio_utils.JoinChannels(AChannelData, ASamples);
end;

function JoinChannels(AChannelData: PPSingle; AChannels: Integer; ASamples: Integer): TSingleArray;
begin
  Result := paio_utils.JoinChannels(AChannelData, AChannels, ASamples);
end;


function BufferPool: TPABufferPool;
begin
  if InternalBufferPool = nil then
    InternalBufferPool := TPABufferPool.Create;
  Result := InternalBufferPool;
end;


function ConvertNoConvert(InData: Pointer; AInDataSize: Integer; var AOutSize: Integer): Pointer;
begin
  // this shouldn't be possible but just in case.
  Result := InData;
  AOutSize:=AInDataSize;
end;

function ConvertS16ToFloat32(InData: Pointer; AInDataSize: Integer; var AOutSize: Integer): Pointer;
var
  Orig: PSmallInt absolute InData;
  NewData: PSingle;
  Values: Integer;
  i: Integer;
begin
  Values := AInDataSize div SizeOf(SmallInt);
  AOutSize:=Values*SizeOf(Single);
  NewData :=Getmem(AOutSize);
  for i := 0 to Values-1 do
    NewData[i] := Orig[i] / $8000;

  Result := NewData;
end;

function ConvertFloat32ToS16(InData: Pointer; AInDataSize: Integer; var AOutSize: Integer): Pointer;
var
  Orig: PSingle absolute InData;
  NewData: PSmallInt;
  Values: Integer;
  i: Integer;
begin
  Values := AInDataSize div SizeOf(Single);
  AOutSize:=Values*SizeOf(SmallInt);
  NewData :=Getmem(AOutSize);
  for i := 0 to Values-1 do
    NewData[i] := Trunc(Orig[i] * $8000);

  Result := NewData;

end;

function ConvertS16SwapToFloat32(InData: Pointer; AInDataSize: Integer; var AOutSize: Integer): Pointer;
var
  Smallints : PSmallInt absolute InData;
  I: Integer;
begin
  for I := 0 to AInDataSize div SizeOf(SmallInt) -1 do
    Smallints[I] := SwapEndian(Smallints[I]);

  Result := ConvertS16ToFloat32(InData, AInDataSize, AOutSize);
end;

{ TPABufferMessage }

constructor TPABufferMessage.Create(ABuffer: PAudioBuffer; const ADests: array of IPAAudioDestination);
var
  i: Integer;
begin
  inherited Create(PAM_SendBuffer);
  Buffer:=ABuffer;
  SetLength(Dests, Length(ADests));
  for i := 0 to High(ADests) do
    Dests[i] := ADests[i];
end;

destructor TPABufferMessage.Destroy;
begin
  if Assigned(Buffer) then
    BufferPool.ReturnBufferToPool(Buffer);
  inherited Destroy;
end;


{ TPABufferEvent }

constructor TPABufferEvent.Create;
begin
  inherited Create;
  InitCriticalSection(FCrit);
end;

destructor TPABufferEvent.Destroy;
begin
  DoneCriticalSection(FCrit);
  inherited Destroy;
end;

procedure TPABufferEvent.SetEvent;
begin
  EnterCriticalSection(FCrit);
  try
    if FIsSet then
      Exit;
    inherited SetEvent;
    FIsSet:=True;
  finally
    LeaveCriticalSection(FCrit);
  end;
end;

procedure TPABufferEvent.ResetEvent;
begin
  EnterCriticalSection(FCrit);
  try
    if not FIsSet then
      Exit;
    FIsSet:=False;
    inherited ResetEvent;
  finally
    LeaveCriticalSection(FCrit);
  end;
end;

function TPABufferEvent.IsSet: Boolean;
begin
  EnterCriticalSection(FCrit);
  try
    Result := FIsSet;
  finally
    LeaveCriticalSection(FCrit);
  end;
end;

{ TPABufferPool }

function TPABufferPool.CheckWaitFor(ABuffer: PAudioBuffer): Boolean;
// Must be called with FCrit held. Hands ABuffer to the first live waiter and
// returns True; returns False when there is no waiter (caller keeps the buffer).
var
  Event: TPABufferEvent;
begin
  if ABuffer = nil then
    Raise Exception.Create('Trying to insert a null buffer into the queue');
  Result := False;
  // Pull waiters until we find a live one. Events flagged OwnerDestroyed belong
  // to waiters that already self-serviced on timeout; the pool disposes them.
  repeat
    Event := TPABufferEvent(FWaitList.GetObject);
    if Event = nil then
      Exit(False);
    if Event.OwnerDestroyed then
      FreeAndNil(Event);
  until Assigned(Event);

  // hand the buffer straight to the waiting object
  Event.Buffer := ABuffer;
  Event.SetEvent;
  Result := True;
end;

procedure TPABufferPool.WriteAvailBuffers;
begin
  //WriteLn('Pool: Buffers Available: ', FBufferList.Count);
end;

function TPABufferPool.BufferInPool(ABuf: PAudioBuffer): Boolean;
begin
  Result := FBufferList.Contains(ABuf);
end;

procedure TPABufferPool.AllocateBuffers(ACount: Integer);
var
  Buf: PAudioBuffer;
  I: Integer;
begin
  //WriteLn('Allocating Buffers: ', ACount);

    EnterCriticalsection(FCrit);
    try
      Inc(FTargetCount, ACount); // this manager's share of the shared pool
    finally
      LeaveCriticalsection(FCrit);
    end;

    for I := 0 to ACount-1 do begin
      Buf := GetMem(SizeOF(TAudioBuffer));
      FillChar(Buf^, SizeOf(TAudioBuffer), 0); // make sure it's zero
      EnterCriticalsection(FCrit);
      try
        FBuffers.Add(Buf); // this is to free the memory
        if not CheckWaitFor(Buf) then
          FBufferList.AddItem(Buf);
      finally
        LeaveCriticalsection(FCrit);
      end;
    end;

end;

procedure TPABufferPool.DeallocateBuffers(ACount: Integer);
var
  Buf: Pointer;
begin
  EnterCriticalsection(FCrit);
  try
    // Lower the target by this manager's share. At teardown most of its buffers
    // are still in flight (not free yet), so we can only reclaim what is free
    // right now; the rest are over-target and get freed in ReturnBufferToPool as
    // they trickle back. This is what keeps the pool (and its free list) bounded.
    Dec(FTargetCount, ACount);
    if FTargetCount < 0 then
      FTargetCount := 0;
    while (FBuffers.Count > FTargetCount) and (FBufferList.Count > 0) do
    begin
      Buf := FBufferList.GetItem; // take one off the available (free) list
      if Buf = nil then
        Break;
      FBuffers.Remove(Buf);       // drop it from the master list too...
      FreeMem(Buf);               // ...and release its memory
    end;
  finally
    LeaveCriticalsection(FCrit);
  end;
end;

procedure TPABufferPool.FreeBuffers;
begin
  // :)
end;

function TPABufferPool.GetBufferFromPool(AWait: Boolean = True; APump: TThreadMethod = nil): PAudioBuffer;
var
  WaitEvent: TPABufferEvent;
  Res: TWaitResult;
begin
  // Block when the pool is exhausted -- this blocking IS the chain's global
  // back-pressure: a producer waits here until the realtime sink returns a
  // buffer, which paces the whole pipeline to playback. Growing the pool on
  // demand instead removed that pacing entirely, so sources/links raced the
  // whole song into the sink's buffer ahead of time (the VU meter finished
  // while audio kept playing).
  //
  // Check the free list and register a waiter atomically, so a concurrent
  // ReturnBufferToPool can't slip a buffer into the list past our waiter.
  EnterCriticalsection(FCrit);
  try
    Result := PAudioBuffer(FBufferList.GetItem); // can return nil
    if Assigned(Result) or not AWait then
      Exit;
    WaitEvent := TPABufferEvent.Create;
    WaitEvent.ResetEvent;
    FWaitList.AddObject(WaitEvent);
  finally
    LeaveCriticalsection(FCrit);
  end;

  repeat
    // Deliver any output we're already holding so the destination can drain it
    // and return a buffer (which may be handed straight to our WaitEvent). Pump
    // first, then wait on a short timeout so a freed buffer is picked up fast.
    if Assigned(APump) then
      APump;
    Res := WaitEvent.WaitFor(100);
    if Res = wrSignaled then
    begin
      // CheckWaitFor dequeued our event and handed it a returned buffer.
      Result := WaitEvent.Buffer;
      FreeAndNil(WaitEvent);
      Break;
    end;

    // Timed out: decide atomically vs ReturnBufferToPool whether we were just
    // handed a buffer, otherwise self-service from the list.
    EnterCriticalsection(FCrit);
    try
      if WaitEvent.IsSet then
      begin
        Result := WaitEvent.Buffer;
        FreeAndNil(WaitEvent);
      end
      else
      begin
        Result := PAudioBuffer(FBufferList.GetItem);
        if Assigned(Result) then
        begin
          // Grabbed our own buffer; flag the event so a returner disposes it
          // instead of handing it a buffer (which would orphan that buffer).
          WaitEvent.Buffer := nil;
          WaitEvent.OwnerDestroyed := True;
        end;
      end;
    finally
      LeaveCriticalsection(FCrit);
    end;
  until Assigned(Result);
end;

procedure TPABufferPool.ReturnBufferToPool(ABuffer: PAudioBuffer);
begin
  if ABuffer = nil then
    Raise Exception.Create('Tried to add a nil buffer to pool');
  ABuffer^.IsEndOfData:=False;
  ABuffer^.UsedData:=0;

  // Hand the buffer to a waiter or put it back on the free list as one atomic
  // step (shares FCrit with GetBufferFromPool's check-and-register).
  EnterCriticalsection(FCrit);
  try
    if BufferInPool(ABuffer) then
      Raise Exception.Create('Buffer already in pool!');
    // A live waiter takes priority -- it needs a buffer now, even if we're over
    // target. Otherwise: if the pool currently owns more buffers than it should
    // (a chain was torn down and its buffers are trickling back), reclaim this
    // one instead of pooling it; this is how the deferred shrink completes.
    if not CheckWaitFor(ABuffer) then
    begin
      if FBuffers.Count > FTargetCount then
      begin
        FBuffers.Remove(ABuffer);
        FreeMem(ABuffer);
      end
      else
        FBufferList.AddItem(ABuffer);
    end;
  finally
    LeaveCriticalsection(FCrit);
  end;
end;

function TPABufferPool.TotalBufferCount: Integer;
begin
  EnterCriticalsection(FCrit);
  try
    Result := FBuffers.Count;
  finally
    LeaveCriticalsection(FCrit);
  end;
end;

function TPABufferPool.FreeBufferCount: Integer;
begin
  EnterCriticalsection(FCrit);
  try
    Result := FBufferList.Count;
  finally
    LeaveCriticalsection(FCrit);
  end;
end;

constructor TPABufferPool.Create;
begin
  InitCriticalSection(FCrit);
  FBuffers := TList.Create;
  FBufferList:= TPAFifoList.Create;
  FWaitList := TPAFifoList.Create;
end;

destructor TPABufferPool.Destroy;
var
  P: Pointer;
  Event: TObject;
begin
  for P in FBuffers do
    Freemem(P);
  FBuffers.Free;
  FBufferList.Free;
  // dispose any waiter events still queued (e.g. abandoned on a timeout that
  // no later ReturnBufferToPool reclaimed).
  repeat
    Event := FWaitList.GetObject;
    if Assigned(Event) then
      Event.Free;
  until Event = nil;
  FWaitList.Free;
  DoneCriticalsection(FCrit);
  inherited Destroy;
end;

{ TPAAudioBufferManager }

function TPAAudioBufferManager.GetQueuedBufferCount: Integer;
begin
  Result := FBuffers.Count;
end;

constructor TPAAudioBufferManager.Create(AOwner: IPAAudioDestination; ABufferCount: Integer; ADropDataIfFull: Boolean);
begin
  FOwner := AOwner;
  FBuffers := TPAFifoList.Create;
  FBufferCount:= ABufferCount;
  FBufferUsed := TPABufferEvent.Create;
  FCanDropData:=ADropDataIfFull;
  BufferPool.AllocateBuffers(ABufferCount);
end;

destructor TPAAudioBufferManager.Destroy;
var
  B: PAudioBuffer;
begin
  // Return any buffers still queued in this manager to the pool, otherwise they
  // leak (the FIFO list object is freed but the buffers inside it are not), and
  // DeallocateBuffers below would have fewer free buffers to reclaim.
  repeat
    B := PAudioBuffer(FBuffers.GetItem);
    if B <> nil then
      BufferPool.ReturnBufferToPool(B);
  until B = nil;
  FBuffers.Free;
  FBufferUsed.Free;
  // We added FBufferCount buffers to the shared pool in Create; reclaim that many
  // now so the pool doesn't grow every time a chain is built and torn down.
  BufferPool.DeallocateBuffers(FBufferCount);
  inherited Destroy;
end;

procedure TPAAudioBufferManager.Flush;
begin
  {if FInProgressBuffer <> nil then
  begin
    FInProgressBuffer:=nil;
    FOwner.SetDataEvent;
  end;}
end;

function TPAAudioBufferManager.WriteBuffer(ABuffer: PAudioBuffer): Boolean;
var
  Res: TWaitResult;
begin
  Result := False;
  //WriteLn('Manager for ', FOwner.GetObject.ClassName,' adding buffer = nil: ', ABuffer = nil);
  if ABuffer = nil then
    Exit;
  if GetQueuedBufferCount >= FBufferCount then
  begin
    FBufferUsed.ResetEvent;
    case FBufferUsed.WaitFor(100) of
      wrTimeout:
        begin
          FBufferUsed.SetEvent;
          if GetQueuedBufferCount >= FBufferCount then
            Exit;

        end;
      wrAbandoned:
        begin
          BufferPool.ReturnBufferToPool(ABuffer);
          Exit(True);
        end;
    end;
  end;

  //if GetQueuedBufferCount >= FBufferCount then
  //  WriteLn(FOwner.GetObject.Classname, ' queued buffers = ', GetQueuedBufferCount);

  Result := True;
  FBuffers.AddItem(ABuffer);

  FOwner.SetDataEvent;

  //WriteLn('Manager for ', FOwner.GetObject.ClassName,' got buffer. FirstBuffer = nil: ', FFirstBuffer = nil);
end;


function TPAAudioBufferManager.NextFilledBuffer: PAudioBuffer;
begin
  //WriteLn(FOwner.GetObject.ClassName,' asking for filled buffer have one?', FFirstBuffer <> nil);
  Result := PAudioBuffer(FBuffers.GetItem);
  if FBuffers.Count < FBufferCount then
    FBufferUsed.SetEvent;
  //WriteLn(FOwner.GetObject.ClassName,' Unblocking if blocked. Used = ', GetQueuedBufferCount);

  //WriteLn(FOwner.GetObject.ClassName,' gave buffer. nil? ', Result = nil);
end;


function TPAAudioBufferManager.Empty: Boolean;
var
  i: integer = 0;
  B: PAudioBuffer;
begin
  Result := (FBuffers.Count = 0) and (FBuffersWaitingToQueue = 0);
end;

{ TPAAudioDestination }

function TPAAudioDestination.GetOnDataEnded(AUserObj: TObject): TPAAudioDataEndEvent;
begin
  Result := FonDataEnded;
end;

function TPAAudioDestination.GetWorking: Boolean;
begin
  Result := not (FDataIsEnded and FBufferManager.Empty);
end;

procedure TPAAudioDestination.SetOnDataEnded(AUserObj: TObject;
  AValue: TPAAudioDataEndEvent);
begin
  FUserObj := AUserObj;
  FonDataEnded:=AValue;
end;

procedure TPAAudioDestination.DoOnDataEnded;
begin
  if Assigned(FonDataEnded) then
    FonDataEnded(Self, FUserObj);
end;

procedure TPAAudioDestination.Execute;
var
  Buffer: PAudioBuffer;
  Res: TWaitResult;
  RealData: Pointer;
  RealDataSize: Integer;
  Msg: TPAIOMessage;
begin
  MaskAudioFPExceptions;

  while not Terminated do
  begin
    try
      Res := FMsgQueue.WaitMessage(1000, Msg);
      if Res = wrSignaled then
      begin
        case Msg.Message of
        PAM_DataEnd: ;
        PAM_Data :
          begin
            Buffer := FBufferManager.NextFilledBuffer;

            if Assigned(Buffer) then
            begin
              try
                try
                  // see if we need to convert the data to a different format.
                  if (Format <> afRaw) and not (Buffer^.Format in [Format, afRaw]) then
                    RealData := ConvertAudio[Buffer^.Format][Format](@Buffer^.Data, Buffer^.UsedData, RealDataSize)
                  else
                  begin
                    RealData:=@Buffer^.Data;
                    RealDataSize:=Buffer^.UsedData;
                  end;
                  if Buffer^.IsEndOfData then
                    Buffer^.IsEndOfData := True; // for debugging

                  InternalProcessData(RealData^, RealDataSize, Buffer^.IsEndOfData);
                except
                  on E: Exception do
                  begin
                    TPALog.Error(ClassName, E.Message);
                  end;
                end;
                // if a conversion has occured then free the allocated data.
                if RealData <> @Buffer^.Data then
                  Freemem(RealData);

              finally
                BufferPool.ReturnBufferToPool(Buffer);
              end;
            end;
          end;
        PAM_ObjectIsDestroying:
          begin
            Terminate;
            FFreeObject.SetEvent;
          end;
        end;
        if Assigned(Msg) then
          Msg.Free;
      end
      else
        if Res <> wrTimeout then
          TPALog.Warning(ClassName, 'unexpected WaitMessage result: %d', [Ord(Res)]);

    except
      on E: Exception do
      begin
        TPALog.Error(ClassName, E.Message);
      end;
    end;
  end;
end;

function TPAAudioDestination.GetDataSource: IPAAudioSource;
begin
  Result := FDataSource;
end;

function TPAAudioDestination.GetChannels: Integer;
begin
  if Assigned(DataSource) and (DataSource.GetSourceObject is IPAAudioInformation) then
    Result := (DataSource.GetSourceObject as IPAAudioInformation).GetChannels
  else
    Result := inherited GetChannels;
end;

function TPAAudioDestination.GetSamplesPerSecond: Integer;
begin
  if Assigned(DataSource) and (DataSource.GetSourceObject is IPAAudioInformation) then
    Result := (DataSource.GetSourceObject as IPAAudioInformation).GetSamplesPerSecond
  else
    Result := inherited GetSamplesPerSecond;
end;

procedure TPAAudioDestination.SetDataSource(AValue: IPAAudioSource);
begin
  if FDataSource <> nil then
    FDataSource.RemoveDestination(Self);

  FDataSource := AValue;

  if FDataSource <> nil then
    FDataSource.AddDestination(Self);
end;

procedure TPAAudioDestination.EndOfData;
begin
  //WriteLn(ClassName, ' notified data done');
  FDataIsEnded:=True;
  FBufferManager.Flush; // marks any buffered data as ready to be processed
  FMsgQueue.PostMessage(PAM_DataEnd);
  Queue(@DoOnDataEnded);
end;

function TPAAudioDestination.GetObject: TObject;
begin
  Result := Self;
end;

constructor TPAAudioDestination.Create;
begin
  // 2 buffers is enough under the back-pressured pull model: one being processed
  // while the next is queued. The pool stays balanced (see DeallocateBuffers).
  FBufferManager := TPAAudioBufferManager.Create(Self, 2, False);
  inherited Create;
end;

destructor TPAAudioDestination.Destroy;
begin
  FFreeObject := TSimpleEvent.Create;
  FMsgQueue.PostMessage(PAM_ObjectIsDestroying);
  FFreeObject.WaitFor(5000); // wait upto 5 seconds for execute to exit
  FFreeObject.Free;

  FBufferManager.Free;
  inherited Destroy;
end;

function TPAAudioDestination.WriteBuffer(ABuffer: PAudioBuffer): Boolean;
begin
  Result := FBufferManager.WriteBuffer(ABuffer);
  if Result then
    SetDataEvent;
end;


procedure TPAAudioDestination.SetDataEvent;
begin
  FMsgQueue.PostMessage(PAM_Data);
end;

{ TPAAudioLink }

procedure TPAAudioLink.BufferEmpty;
begin
  if FBufferManager.Empty then
    StopData;
end;

function TPAAudioLink.InternalOutputToDestination: Boolean;
var
  Buf: PAudioBuffer;
  RealData: Pointer;
  RealDataSize: Integer =0;
begin

  //WriteLn(Classname, ' Internal Output to dest');
  Buf := FBufferManager.NextFilledBuffer;
  Result := True;
  if Assigned(Buf) then
  begin
   // this causes the specialized descendant to process the data and produce output.
   try
     // see if we need to convert the data to a different format.
     if (Format <> afRaw) and not (Buf^.Format in [Format, afRaw]) then
     begin
       //WriteLn('Converting from ', Buf^.Format ,' to ', Format);
       RealData := ConvertAudio[Buf^.Format][Format](@Buf^.Data, Buf^.UsedData, RealDataSize)
     end
     else
     begin
       RealData:=@Buf^.Data;
       RealDataSize:=Buf^.UsedData;
     end;
     InternalProcessData(RealData^, RealDataSize, Buf^.IsEndOfData);
   except
     on E: Exception do
     begin
       TPALog.Error(ClassName, E.Message);
     end;
   end;

   // if a conversion took place then free converted data since it wasn't from the pool
   if RealData <> @Buf^.Data then
     Freemem(RealData);

   BufferPool.ReturnBufferToPool(Buf);
  end;

end;

function TPAAudioLink.GetDataSource: IPAAudioSource;
begin
  Result := FDataSource;
end;

procedure TPAAudioLink.SetDataSource(AValue: IPAAudioSource);
begin
  if FDataSource <> nil then
    FDataSource.RemoveDestination(Self); // this does nothing if not in list yet

  FDataSource := AValue;

  if FDataSource <> nil then
    FDataSource.AddDestination(Self);

end;

function TPAAudioLink.WriteBuffer(ABuffer: PAudioBuffer): Boolean;
begin
  Result := FBufferManager.WriteBuffer(ABuffer);
  //WriteLn(ClassName, ' got buffer. sent to manager');
  SetDataEvent;
end;

procedure TPAAudioLink.SetDataEvent;
begin
  FMsgQueue.PostMessage(PAM_Data);
  //WriteLn(Classname,' setdataevent');
end;

procedure TPAAudioLink.EndOfData;
begin
  EnterCriticalsection(FCrit);
  //WriteLn(ClassName, ': Data is ended');
  FDataIsEnded:=True;

  FBufferManager.Flush;

  // ensure we process "data" even if empty to signal destination
  FMsgQueue.PostMessage(PAM_DataEnd);

  LeaveCriticalsection(FCrit);
end;

function TPAAudioLink.GetObject: TObject;
begin
  REsult := Self;
end;

procedure TPAAudioLink.Execute;
var
  BeforeLoopCalled: Boolean;
  Res: TWaitResult;
  Msg: TPAIOMessage;
  BufferMessage: TPABufferMessage absolute Msg;
begin
  MaskAudioFPExceptions;
  BeforeLoopCalled:=False;
  while not Terminated do
  begin
    try
    Res := FMsgQueue.WaitMessage(1000, Msg);
    case Res of
    wrSignaled:
    begin
      //WriteLn('Link message: ', Msg.Message);
      case Msg.Message of
      PAM_Data:
        begin
          if not BeforeLoopCalled then
          begin
            BeforeLoopCalled:=True;
            BeforeExecuteLoop;
          end;
        //WriteLn(ClassName,' :Source Loop');
          InternalOutputToDestination;
        end;
      PAM_DataEnd: SignalDestinationsDone; // this message should only be Sent from the datasource
      PAM_StopWorking: FWorking := False;
      PAM_ObjectIsDestroying:
        begin
          Terminate;
          FFreeEvent.SetEvent;
        end;
      PAM_SendBuffer:
        begin
          if not DeliverBufferMessage(BufferMessage) then
          begin
           // a destination's queue was full. Retry before processing more data,
           // but after higher-priority messages like PAM_ObjectIsDestroying.
           FMsgQueue.InsertBefore([PAM_Data, PAM_SendBuffer, PAM_DataEnd], Msg);
           Msg := nil; // back in the queue; don't free it
          end;
        end;
      else
        // it is a message specific for the child to choose to handle
        if not HandleMessage(Msg) then
          Raise Exception.Create('Unhandled message');

      end;
      // free the consumed message. branches that re-queue it (PAM_SendBuffer)
      // set Msg := nil so it is not freed here.
      if Assigned(Msg) then
        Msg.Free;
    end;
    wrTimeout: ; // maybe reset event after a while?
    else
      ;//WriteLn(StdErr, ClassName,' waitfor = ', Res);  // Triggered when Freed

    end;

    except
      on E: Exception do
        // Just log on the worker thread. We must NOT Synchronize here: the call
        // it marshalled (RaiseE) was a no-op anyway, and during shutdown the main
        // thread is no longer pumping CheckSynchronize, so Synchronize itself
        // raises EThreadError ('Thread error') -- unhandled, right inside this
        // handler -- which is the crash users saw on closing while playing.
        TPALog.Error(ClassName, E.Message);
    end;
  end;
  AfterExecuteLoop;
  //WriteLn('Left LOOP!');

end;

procedure TPAAudioSource.EnsureAudioBuffer;
var
  Res: TWaitResult;
begin
  if FDestBuf = nil then
    FDestBuf:= BufferPool.GetBufferFromPool(True, @FlushPendingSends)
  else if FDestBuf^.UsedData = AUDIO_BUFFER_SIZE then
  begin
    WriteToDestinations(FDestBuf);
    FDestBuf := BufferPool.GetBufferFromPool(True, @FlushPendingSends);
  end;
  //WriteLn(ClassName,' got buffer');
end;


function TPAAudioSource.DestroyWaitSync: Boolean;
begin
  // safe to call at the start of each destructor in the chain: only the first
  // call posts the destroy message and waits; the rest no-op via FDestroySynced.
  Result := True;
  if FDestroySynced then
    Exit;
  FDestroySynced := True;
  FFreeEvent := TSimpleEvent.Create;
  FMsgQueue.InsertMessage(PAM_ObjectIsDestroying);
  FFreeEvent.WaitFor(5000); // wait up to 5 seconds for execute to finish.
  // The worker sets FFreeEvent before WaitFor can return, so it's done with it.
  FreeAndNil(FFreeEvent);
end;

procedure TPAAudioSource.Execute;
var
  BeforeLoopCalled: Boolean;
  Res: TWaitResult;
  Msg: TPAIOMessage;
  BufferMessage: TPABufferMessage absolute Msg;
  lName: String;
begin
  MaskAudioFPExceptions;
  lName := ClassName;
  BeforeLoopCalled:=False;
  while not Terminated do
  begin
    try
    Res := FMsgQueue.WaitMessage(1000, Msg);
    case Res of
    wrSignaled:
    begin
      case Msg.Message of
      PAM_Data:
        begin
          if not BeforeLoopCalled then
          begin
            BeforeLoopCalled:=True;
            BeforeExecuteLoop;
          end;
        //WriteLn(ClassName,' :Source Loop');
          // Only pump while playing. Pause/Stop clear FWorking (via
          // StopData/PAM_StopWorking); a PAM_Data still sitting in the queue at
          // that point must be ignored outright -- not just left un-reposted --
          // otherwise it would read (and emit) one more buffer past the pause, and
          // for Stop it would advance the position again after the rewind seek.
          if not FWorking then
            // halted: drop this stray data tick.
          else if InternalOutputToDestination = False then
          begin
            //WriteLn('Stopping Data');
            // reached end-of-data: the source is finished, not paused.
            FPlayState := psStopped;
            StopData;
          end
          else
          begin
            // keep the self-perpetuating pump going.
            if not Self.InheritsFrom(TPAAudioLink) then
              FMsgQueue.PostMessage(PAM_Data);
          end;
        end;
      PAM_StopWorking: FWorking := False;
      PAM_ObjectIsDestroying:
        begin
          Terminate;
          FFreeEvent.SetEvent;
        end;
      PAM_SendBuffer:
        begin
          if not DeliverBufferMessage(BufferMessage) then
          begin
           // a destination's queue was full. Retry before processing more data,
           // but after higher-priority messages like PAM_ObjectIsDestroying.
           FMsgQueue.InsertBefore([PAM_Data, PAM_SendBuffer, PAM_DataEnd], Msg);
           Msg := nil; // back in the queue; don't free it
          end;
        end;
      else
        // it is a message specific for the child to choose to handle
        if not HandleMessage(Msg) then
          Raise Exception.Create('Unhandled message');

      end;
      // free the consumed message. branches that re-queue it (PAM_SendBuffer)
      // set Msg := nil so it is not freed here.
      if Assigned(Msg) then
        Msg.Free;
    end;
    wrTimeout: ; // maybe reset event after a while?
    else
      ;//WriteLn(StdErr, ClassName,' waitfor = ', Res);  // Triggered when Freed

    end;

    except
      on E: Exception do
        // Just log on the worker thread. We must NOT Synchronize here: the call
        // it marshalled (RaiseE) was a no-op anyway, and during shutdown the main
        // thread is no longer pumping CheckSynchronize, so Synchronize itself
        // raises EThreadError ('Thread error') -- unhandled, right inside this
        // handler -- which is the crash users saw on closing while playing.
        TPALog.Error(ClassName, E.Message);
    end;
  end;
  AfterExecuteLoop;
end;

procedure TPAAudioSource.BeforeExecuteLoop;
begin

end;

procedure TPAAudioSource.AfterExecuteLoop;
begin

end;

function TPAAudioSource.HandleMessage(var AMsg: TPAIOMessage): Boolean;
begin
  Result := False;
  // messages for the subclasses should be handled here
  // this is called from the context of this thread
end;

procedure TPAAudioSource.AddDestination(AValue: IPAAudioDestination);
var
  i: Integer;
begin
  i := FDestinations.IndexOf(AValue);
  if i >= 0 then
    FDestinations.Delete(i);

  FDestinations.Add(AValue);
end;

procedure TPAAudioSource.RemoveDestination(AValue: IPAAudioDestination);
var
  i: Integer;
begin
  i := FDestinations.IndexOf(AValue);
  if i >= 0 then
    FDestinations.Delete(i);
end;

procedure TPAAudioSource.WriteToDestinations(const ABuffer: PAudioBuffer);
var
  i: Integer;
  Dests: array of IPAAudioDestination;
begin
  //WriteLn(ClassName, ' writing buffer to dests');

  if FDestinations.Count < 1 then
  begin
    // return the buffer to the pool or we will run out of buffers.
    // normally the dest will return the buffer to the pool.
    BufferPool.ReturnBufferToPool(ABuffer);
    Exit;
  end;

  // mark the type of data in the buffer.
  ABuffer^.Format:=FFormat;

  // Post a single send message with the buffer and a snapshot of the wanted
  // destinations. This is just the hand-off -- it never copies and never touches
  // the pool, so a producer can't block here mid-output. The worker thread does
  // the fan-out (and any copy for >1 destination) when it processes the message.
  SetLength(Dests, FDestinations.Count);
  for i := 0 to FDestinations.Count - 1 do
    Dests[i] := IPAAudioDestination(FDestinations[i]);

  FMsgQueue.PostMessage(TPABufferMessage.Create(ABuffer, Dests));
end;

procedure TPAAudioSource.FlushPendingSends;
var
  Msg: TPAIOMessage;
  Held: TList;
  i: Integer;
begin
  // Drain our queue once: deliver PAM_SendBuffer messages, hold everything else
  // untouched and in order. Must NOT process PAM_Data (that would produce more
  // output and re-enter GetBufferFromPool). Runs on our own worker thread, so no
  // one else pops; concurrent posts only append and stay after the held ones.
  Held := TList.Create;
  try
    while FMsgQueue.HasMessage do
    begin
      Msg := FMsgQueue.PopMessage;
      if Msg = nil then
        Break;
      if Msg.Message = PAM_SendBuffer then
      begin
        if DeliverBufferMessage(TPABufferMessage(Msg)) then
          Msg.Free                 // fully delivered
        else
          Held.Add(Msg);           // a destination was full -- retry in order
      end
      else
        Held.Add(Msg);
    end;
    for i := Held.Count - 1 downto 0 do
      FMsgQueue.InsertMessage(TPAIOMessage(Held[i]));
  finally
    Held.Free;
  end;
end;

function TPAAudioSource.DeliverBufferMessage(AMsg: TPABufferMessage): Boolean;
var
  Dst: IPAAudioDestination;
  SendBuf: PAudioBuffer;
  IsLast: Boolean;
  i: Integer;
begin
  // Deliver to each remaining destination in turn. The final one is handed the
  // original buffer; any earlier ones get a fresh copy (so a copy happens only
  // with real fan-out). On a full destination we stop and report not-done so the
  // caller re-queues us with whatever destinations are left.
  while Length(AMsg.Dests) > 0 do
  begin
    Dst := AMsg.Dests[0];
    IsLast := Length(AMsg.Dests) = 1;
    if IsLast then
      SendBuf := AMsg.Buffer
    else
    begin
      SendBuf := BufferPool.GetBufferFromPool(True);
      SendBuf^ := AMsg.Buffer^;
    end;

    if Dst.WriteBuffer(SendBuf) then
    begin
      if IsLast then
        AMsg.Buffer := nil; // original handed off; don't return it when freed
      // drop the delivered destination from the front of the list
      for i := 1 to High(AMsg.Dests) do
        AMsg.Dests[i - 1] := AMsg.Dests[i];
      SetLength(AMsg.Dests, Length(AMsg.Dests) - 1);
    end
    else
    begin
      // destination full: discard the unused copy and ask to be re-queued.
      if not IsLast then
        BufferPool.ReturnBufferToPool(SendBuf);
      Exit(False);
    end;
  end;
  Result := True;
end;

function TPAAudioInformationBase.GetSamplesPerSecond: Integer;
begin
  Result := FSamplesPerSecond;
end;

procedure TPAAudioInformationBase.SetSamplesPerSecond(AValue: Integer);
begin
  FSamplesPerSecond := AValue;
end;

function TPAAudioInformationBase.GetFormat: TPAAudioFormat;
begin
  Result := FFormat;
end;

procedure TPAAudioInformationBase.SetFormat(AValue: TPAAudioFormat);
begin
  FFormat:=AValue;
end;

function TPAAudioInformationBase.GetChannels: Integer;
begin
  Result := FChannels;
end;

procedure TPAAudioInformationBase.SetChannels(AValue: Integer);
begin
  FChannels := AValue;
end;

constructor TPAAudioInformationBase.Create;
begin
  FChannels := 2;
  FSamplesPerSecond:=44100;
  FFormat:=DefaultAudioFormat;
  FMsgQueue := TPAIOMessageQueue.Create;
  inherited Create(False);
end;

destructor TPAAudioInformationBase.Destroy;
begin
  FMsgQueue.Free;
  inherited Destroy;
end;

function TPAAudioSource.WriteToBuffer(const AData; ASize: Integer; AIsLastData: Boolean): Integer;
var
  WCount: Integer;
  Tmp: PAudioBuffer;
begin
  Result := 0;

  repeat
    EnsureAudioBuffer; // Waits for an available buffer
    // ok now we have a buffer
    Tmp := FDestBuf;
    WCount := Min(ASize, AUDIO_BUFFER_SIZE-FDestBuf^.UsedData);

    Move(PByte(@AData)[Result], FDestBuf^.Data[FDestBuf^.UsedData], WCount);
    Inc(FDestBuf^.UsedData, WCount);
    Inc(Result, WCount);
    Dec(ASize, WCount);
    if FDestBuf^.UsedData = AUDIO_BUFFER_SIZE then
    begin
      FDestBuf^.IsEndOfData:=AIsLastData and (ASize = 0);
      WriteToDestinations(FDestBuf);
      FDestBuf := nil;
    end;
  until ASize = 0;

  if AIsLastData and Assigned(FDestBuf) then
  begin
    FDestBuf^.IsEndOfData:=True;
    WriteToDestinations(FDestBuf);
    // Null it so the trailing flush in SignalDestinationsDone doesn't send the
    // very same buffer a second time (which gets returned to the pool twice ->
    // "Buffer already in pool!"). This bit the rate-changing links, whose final
    // output is a partial buffer that takes this path.
    FDestBuf := nil;
  end;
end;

procedure TPAAudioSource.SignalDestinationsDone;
var
  i: Integer;
begin
  if FSignaled then
    Exit;
  FSignaled:=True;

  // ensure we've written all data to destinations
  if FDestBuf <> nil then
  begin
    FDestBuf^.IsEndOfData:=True;
    WriteToDestinations(FDestBuf);
    FDestBuf := nil;
  end;

  //WriteLn(ClassName,' telling destinations done');
  for i := 0 to FDestinations.Count-1 do
    IPAAudioDestination(FDestinations[i]).EndOfData;


end;

function TPAAudioSource.GetSourceObject: TObject;
begin
  Result := Self;
end;

procedure TPAAudioSource.ClearSignal;
begin
  FSignaled:=False;
end;

function TPAAudioLink.GetSamplesPerSecond: Integer;
begin
  if Assigned(DataSource) and (DataSource.GetSourceObject is IPAAudioInformation) then
    Result := (DataSource.GetSourceObject as IPAAudioInformation).GetSamplesPerSecond
  else
    Result := inherited GetSamplesPerSecond;
end;

function TPAAudioLink.GetFormat: TPAAudioFormat;
begin
  // DefaultFormat is afFloat32LE. Setting this value allows us to convert data written in writebuffer to afFloat32LE
  {if Assigned(DataSource) and (DataSource.GetSourceObject is IPAAudioInformation) then
    Result := (DataSource.GetSourceObject as IPAAudioInformation).GetFormat
  else}
    Result := inherited GetFormat;
end;

function TPAAudioLink.GetChannels: Integer;
begin
  if Assigned(DataSource) and (DataSource.GetSourceObject is IPAAudioInformation) then
    Result := (DataSource.GetSourceObject as IPAAudioInformation).GetChannels
  else
    Result:=inherited GetChannels;
end;

constructor TPAAudioSource.Create;
begin
  inherited Create;
  FDestinations := specialize TFPGList<IPAAudioDestination>.create;
  BufferPool.AllocateBuffers(1);
end;

destructor TPAAudioSource.Destroy;
begin
  DestroyWaitSync;
  // Return the partially-filled buffer we were still accumulating into. On a
  // normal end-of-data SignalDestinationsDone flushes and nils it, but when the
  // source is freed mid-stream (e.g. the user loads another song) it's still
  // held here -- without this it leaks from the pool every time.
  if Assigned(FDestBuf) then
  begin
    BufferPool.ReturnBufferToPool(FDestBuf);
    FDestBuf := nil;
  end;
  // Create added 1 buffer to the shared pool (for FDestBuf); reclaim it so the
  // pool stays balanced. Applies to every source AND link (TPAAudioLink derives
  // from TPAAudioSource), so each chain returns exactly what it added.
  BufferPool.DeallocateBuffers(1);
  FDestinations.Free;
  inherited Destroy;
end;

procedure TPAAudioSource.StartData;
begin
  FSignaled:=False;
  FMsgQueue.PostMessage(PAM_Data);
  FWorking:=True;
  FPlayState:=psPlaying;
end;

function TPAAudioSource.Working: Boolean;
begin
  Result := FWorking;
end;

procedure TPAAudioSource.StopData;
begin
  if FWorking then
  begin
    if Assigned(FDestBuf) and (FDestBuf^.UsedData > 0) then
      FMsgQueue.PostMessage(PAM_SendBuffer);
    FMsgQueue.PostMessage(PAM_StopWorking);
  end;
end;

constructor TPAAudioLink.Create;
begin
  InitCriticalSection(FCrit);
  // 2 incoming buffers as a destination; inherited TPAAudioSource.Create adds 1
  // for the link's own output accumulator. Lean but deadlock-free under the
  // back-pressured pull model.
  FBufferManager := TPAAudioBufferManager.Create(Self, 2, False);
  inherited Create;
end;

destructor TPAAudioLink.Destroy;
begin
  // stop the worker thread before freeing the resources its Execute loop uses
  // (FBufferManager / FCrit), otherwise it can touch them after they're gone.
  DestroyWaitSync;
  FBufferManager.Free;
  DoneCriticalsection(FCrit);
  inherited Destroy;
end;

initialization
  // nil by default
  Fillchar(ConvertAudio, SizeOf(ConvertAudio), 0);
  //ConvertAudio[afRaw][] := nil;
  ConvertAudio[afS16]     [afFloat32] := @ConvertS16ToFloat32;
  ConvertAudio[afS16]     [afS16] := @ConvertNoConvert;
  ConvertAudio[afFloat32] [afFloat32]:= @ConvertNoConvert;
  ConvertAudio[afFloat32] [afS16]:= @ConvertFloat32ToS16;

finalization
  if InternalBufferPool <> nil then
    FreeAndNil(InternalBufferPool);

end.

