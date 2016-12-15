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
  Classes, SysUtils, syncobjs, pa_lists, paio_types;

const
  AUDIO_BUFFER_SIZE  = paio_types.AUDIO_BUFFER_SIZE;
  AUDIO_BUFFER_FLOAT_SAMPLES = paio_types.AUDIO_BUFFER_FLOAT_SAMPLES;


type
  TSingleArray = paio_types.TSingleArray;
  TChannelArray = paio_types.TChannelArray;

  TPAAudioFormat = (
                 afRaw, // used to write the audio data directly without converting. Used for encoded data like ogg output.
                 afS16,
                 afFloat32
                 );

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
    function  WriteBuffer(ABuffer: PAudioBuffer): Int64;
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


  { TPABufferEvent }

  TPABufferEvent = class(TSimpleEvent)
  private
    FIsSet: Boolean;
  public
    Buffer: PAudioBuffer;
    OwnerDestroyed: Boolean; // means buffer pool should free the event
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
    // IPAAudioInformation
    function  GetSamplesPerSecond: Integer; virtual;
    function  GetFormat: TPAAudioFormat; virtual;
    function  GetChannels: Integer; virtual;
    procedure SetSamplesPerSecond(AValue: Integer); virtual;
    procedure SetFormat(AValue: TPAAudioFormat); virtual;
    procedure SetChannels(AValue: Integer); virtual;
  public
    constructor Create; virtual;
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
    E: Exception;
    FBufferAvailable: TPABufferEvent;
    FSignaled: Boolean;
    FDestBuf: PAudioBuffer;
    //Datasource: IPAAudioSource;
    procedure EnsureAudioBuffer;
    procedure RaiseE;
  protected
    procedure Execute; override;
    procedure BeforeExecuteLoop; virtual;
    procedure AfterExecuteLoop; virtual;
    function InternalOutputToDestination: Boolean; virtual; abstract;
    // IPAAudioSource
    procedure AddDestination(AValue: IPAAudioDestination); virtual;
    procedure RemoveDestination(AValue: IPAAudioDestination); virtual;
    procedure WriteToDestinations(const ABuffer: PAudioBuffer);{ virtual;}
    function  WriteToBuffer(const AData; ASize: Integer; AIsLastData: Boolean): Integer;
    procedure SignalDestinationsDone; virtual;
    function  GetSourceObject: TObject;
    procedure ClearSignal;

  private
    FDataEvent: TSimpleEvent;
    FDestinations: TFPList;
    FWorking: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure StartData;
    function  Working: Boolean;
    procedure StopData; // Unsets wait event
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
    function CheckWaitFor(ABuffer: PAudioBuffer): Boolean;
    procedure WriteAvailBuffers;
    function BufferInPool(ABuf: PAudioBuffer): Boolean;
  public
    procedure AllocateBuffers(ACount: Integer);
    procedure FreeBuffers;
    function  GetBufferFromPool(AWait: Boolean = True): PAudioBuffer;
    procedure ReturnBufferToPool(ABuffer: PAudioBuffer);
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
    //FEvent: TObj
    //procedure  NextEmptyBuffer;
    FBuffersWaitingToQueue: Integer;
    function GetQueuedBufferCount: Integer;
  public
    constructor Create(AOwner: IPAAudioDestination; ABufferCount: Integer; ADropDataIfFull: Boolean);
    destructor  Destroy; override;
    procedure Flush;
    function  WriteBuffer(ABuffer: PAudioBuffer): Int64;
    //function  WriteData(const Data; ACount: Int64): Int64;
    //property  BufferCount: Integer read FBufferCount;
    function  NextFilledBuffer: PAudioBuffer; // removes the buffer from the manager. caller must return it to the pool
    //procedure BufferProcessed; // call this after the buffer obtained by NextFilledBuffer is processed
    function  Empty: Boolean;
  end;

  { TPAAudioDestination }

  TPAAudioDataEndEvent = procedure(Sender: IPAAudioDestination; AUserObj: TObject) of object;

  TPAAudioDestinationClass = class of TPAAudioDestination;
  // last link in a chain
  TPAAudioDestination = class(TPAAudioInformationBase, IPAAudioDestination)
  private
    FDataSource: IPAAudioSource;
    FonDataEnded: TPAAudioDataEndEvent;
    FUserObj: TObject;
    function GetOnDataEnded(AUserObj: TObject): TPAAudioDataEndEvent;
    function GetWorking: Boolean;
    procedure SetOnDataEnded(AUserObj: TObject; AValue: TPAAudioDataEndEvent);
    procedure DoOnDataEnded;
  protected
    FDataEvent: TPABufferEvent;
    FDataEventLock: TRTLCriticalSection;
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
    procedure LockDataEvent;
    procedure UnlockDataEvent;
  public
    constructor Create; override;
    destructor  Destroy; override;
    // queues data in a buffer to be written as the thread can process it
    //function  WriteData(const Data; ACount: Int64): Int64; virtual;
    function  WriteBuffer(ABuffer: PAudioBuffer): Int64;
    procedure SetDataEvent; {virtual;}
    property  Working: Boolean read GetWorking;
    property  DataSource: IPAAudioSource read GetDataSource write SetDataSource;
    property  OnDataEnded[AUserObj: TObject]: TPAAudioDataEndEvent read GetOnDataEnded write SetOnDataEnded;
  end;

  TPAAudioLinkClass = class of TPAAudioLink;
  // TAudioLink has something before and something after it in the chain
  TPAAudioLink = class(TPAAudioSource, IPAAudioSource, IPAAudioDestination, IPAAudioInformation)
  private
    FDataSource: IPAAudioSource;
    FCrit: TRTLCriticalSection;
  protected
    //FDataAvailableEvent: TSimpleEvent;
    FDataIsEnded: Boolean;
    FBufferManager: TPAAudioBufferManager;
    //FBufferCurrent: PAudioBuffer;
    //procedure BufferFilled; // call this after a PAudioBuffer is ready to be processed
    procedure BufferEmpty;  // call this after a PAudioBuffer has been processed
    // for IPAAudioSource. processes input data and writes it to destinations
    function  InternalOutputToDestination: Boolean; override;
    // for IPAAudioDestination. writes data in buffer to whatever codec etc
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; virtual; abstract;
    // IPAAudioDestination
    function  GetDataSource: IPAAudioSource; virtual;
    procedure SetDataSource(AValue: IPAAudioSource); virtual;
    function  WriteBuffer(ABuffer: PAudioBuffer): Int64; virtual;
    // IPAAudioInformation
    function  GetSamplesPerSecond: Integer; override;
    function  GetFormat: TPAAudioFormat; override;
    function  GetChannels: Integer; override;
    //function  WriteData(const Data; ACount: Int64): Int64; virtual;
    procedure SetDataEvent;
    procedure EndOfData; virtual; // called by audiosource
    function  GetObject: TObject;
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
  paio_utils;

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


{ TPABufferEvent }

procedure TPABufferEvent.SetEvent;
begin
  if FIsSet then
    Exit;
  (SELF as TSimpleEvent).SetEvent;
  FIsSet:=True;
end;

procedure TPABufferEvent.ResetEvent;
begin
  if not FIsSet then
    Exit;
  FIsSet:=False;
  (SELF as TSimpleEvent).ResetEvent;
end;

function TPABufferEvent.IsSet: Boolean;
begin

end;

{ TPABufferPool }

function TPABufferPool.CheckWaitFor(ABuffer: PAudioBuffer): Boolean;
var
  Event: TPABufferEvent;
begin
  Result := False;
  WriteAvailBuffers;
  Event := TPABufferEvent(FWaitList.GetObject);
  if Event <> nil then
  begin
    Result := True;
    //WriteLn('Pool: Buffer given to wait event, 0x',hexStr(Pointer(ABuffer)));
    // The buffer can get sent straight to the object waiting for it
    Event.Buffer:=ABuffer;
    Event.SetEvent;
  end;
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

    for I := 0 to ACount-1 do begin
      Buf := AllocMem(SizeOF(TAudioBuffer)); // memory is 0's
      if not CheckWaitFor(Buf) then
      begin
        FBufferList.AddItem(Buf);
      end;
      EnterCriticalsection(FCrit);
      FBuffers.Add(Buf); // this is to free the memory
      LeaveCriticalsection(FCrit);
    end;

end;

procedure TPABufferPool.FreeBuffers;
begin
  // :)
end;

function TPABufferPool.GetBufferFromPool(AWait: Boolean = True): PAudioBuffer;
var
  C: Integer;
  WaitEvent: TPABufferEvent;
  Res: TWaitResult;
  TimeoutCount: Integer = 0;
begin
  //WriteLn('Pool: Asking for buffer. Available = ',FFirstAvailable <> nil);
  WriteAvailBuffers;
  C := FBufferList.Count;
  Result := PAudioBuffer(FBufferList.GetItem); // can return nil
  if (Result = nil) and (AWait) then
  begin
    WaitEvent := TPABufferEvent.Create;
    WaitEvent.ResetEvent;
    FWaitList.AddObject(WaitEvent);
    repeat
      Res := WaitEvent.WaitFor(1000);
      if not (Res in [wrSignaled, wrTimeout]) then
        ;//WriteLn(StdErr, 'Pool: WaitFor = ', Res);
      if Res = wrTimeout then
      begin
        //WriteLn(StdErr, 'Pool WaitforTimedOut waiting for buffer Available = ', FBufferList.Count);
        Result := PAudioBuffer(FBufferList.GetItem);
        if Result <> nil then
          Break;
      end;
    until Res = wrSignaled;
    WaitEvent.ResetEvent;
    //WriteLn('Pool: Buffer from wait event, 0x',hexStr(Pointer(WaitEvent.Buffer)));
    Result := WaitEvent.Buffer;
    WaitEvent.Buffer:=nil;
    WaitEvent.Free;
    if Result = nil then
      WriteLn(StdErr, 'Error nil Buffer from waitlist!');

  end;

  //WriteLn('Got Buffer from pool');
end;

procedure TPABufferPool.ReturnBufferToPool(ABuffer: PAudioBuffer);
var
  WaitEvent: PBufferWaitEvent;
begin
  if ABuffer = nil then
    Exit;
  if BufferInPool(ABuffer) then
    WriteLn('!!!!!!!!!!!!!!!!!!! !!!!!!!!!!!!!!!!!!!!!!! Buffer already in pool!!!!!!!!');
  WriteAvailBuffers;
  //WriteLN('Pool: Buffer available');
  ABuffer^.IsEndOfData:=False;
  ABuffer^.UsedData:=0;

  if not CheckWaitFor(ABuffer) then
  begin
    FBufferList.AddItem(ABuffer);
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
begin
  for P in FBuffers do
    Freemem(P);
  FBuffers.Free;
  FBufferList.Free;
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
  //FBufferUsed.SetEvent;
  FCanDropData:=ADropDataIfFull;
  BufferPool.AllocateBuffers(ABufferCount);
end;

destructor TPAAudioBufferManager.Destroy;
begin
  FBuffers.Free;
  FBufferUsed.Free;
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

function TPAAudioBufferManager.WriteBuffer(ABuffer: PAudioBuffer): Int64;
var
  Res: TWaitResult;
begin
  //WriteLn('Manager for ', FOwner.GetObject.ClassName,' adding buffer = nil: ', ABuffer = nil);
  if ABuffer = nil then
    Exit;
  Inc(FBuffersWaitingToQueue);
  while GetQueuedBufferCount >= FBufferCount do
  begin
    //WriteLn(FOwner.GetObject.ClassName,' Blocking until a buffer is used');
    repeat
      Res := FBufferUsed.WaitFor(200);
      if not (Res in [wrSignaled, wrTimeout]) then
        WriteLn('Manager: WaitFor = ', Res);
      if Res = wrTimeout then
      begin
        WriteLn(FOwner.GetObject.ClassName,' timeout while blocking waiting for data to be used! Setting dataEvent');
        FOwner.SetDataEvent;
      end
      else if Res = wrAbandoned then
      begin
        BufferPool.ReturnBufferToPool(ABuffer);
        Exit;
      end;
    until (Res = wrSignaled) or (GetQueuedBufferCount < FBufferCount);
    if Res <> wrSignaled then
      WriteLn('Buffer was used without setting event.');
      //raise Exception.Create('buffer was uses without setting FBufferUsed event!');


  end;
  Dec(FBuffersWaitingToQueue);
  FBufferUsed.ResetEvent;


  if GetQueuedBufferCount >= FBufferCount then
    WriteLn(FOwner.GetObject.Classname, ' queued buffers = ', GetQueuedBufferCount);

  Result := ABuffer^.UsedData;

  FBuffers.AddItem(ABuffer);

  FOwner.SetDataEvent;

  //WriteLn('Manager for ', FOwner.GetObject.ClassName,' got buffer. FirstBuffer = nil: ', FFirstBuffer = nil);
end;

{procedure TPAAudioBufferManager.NextEmptyBuffer;
var
  res: TWaitResult;
begin
  FInProgressBuffer := BufferPool.GetBufferFromPool;
  //WriteLn('Asking for next empty buffer');
  if FInProgressBuffer = nil then
  begin
    //WriteLn('All Buffers used. AllowDropData? = ', FCanDropData);
    if FCanDropData then
      Exit
    else
    begin
      // possible hang here if data is never read for any reason
      // maybe use a sync obj here so we can quit if terminated
      //WriteLn('Maybe entering infinite loop');


      FBufferEmptyEvent.ResetEvent;
      BufferPool.NotifyEmptyBuffer(FBufferEmptyEvent);
      // not a typo. BufferProcessed needs the critical section to free a buffer we are waiting for.
      LeaveCriticalsection(FBufferLock);
        repeat
          res := FBufferEmptyEvent.WaitFor(1000);
          if Res = wrTimeout then
            WriteLn('timed out waiting empty buffer');
        until res = wrSignaled;
        FInProgressBuffer := FBufferEmptyEvent.Buffer;
      EnterCriticalsection(FBufferLock);

      //WriteLn('exited possible infinite loop');
    end;
  end;

  if FLastBuffer <> nil then
    FLastBuffer^.NextBuffer:=FInProgressBuffer;

  FLastBuffer:=FInProgressBuffer;


  //WriteLn('NextEmptyBuffer = ', HexStr(Result));

end;}

function TPAAudioBufferManager.NextFilledBuffer: PAudioBuffer;
begin
  //WriteLn(FOwner.GetObject.ClassName,' asking for filled buffer have one?', FFirstBuffer <> nil);
  Result := PAudioBuffer(FBuffers.GetItem);
  //WriteLn(FOwner.GetObject.ClassName,' Unblocking if blocked. Used = ', GetQueuedBufferCount);
  FBufferUsed.SetEvent;

  //WriteLn(FOwner.GetObject.ClassName,' gave buffer. nil? ', Result = nil);
end;

{function TPAAudioBufferManager.WriteData(const Data; ACount: Int64): Int64;
var
  DataAsByte: array [0..0] of byte absolute Data;
  //DataAsByte: Pbyte absolute Data;
  //DataIndex: Integer = 0;
  WriteCount: Integer;
begin
  // exit
  EnterCriticalsection(FBufferLock);
  //WriteLn('entering crit');
  //WriteLn(FOwner.GetObject.ClassName, ' : Writing Data in manager ',ACount);
  //ABufferFilled:=False;
  Result := 0;
  try
    repeat
      if not Assigned(FInProgressBuffer) then
      begin
        // uses lock
        NextEmptyBuffer;
        //WriteLn('Grabbed buffer.');
      end;

      if Assigned(FInProgressBuffer) then
      begin
        WriteCount := Min(AUDIO_BUFFER_SIZE - FInProgressBuffer^.UsedData, ACount);
        //WriteLn('WriteCount = ', WriteCount, ' Asked: ', ACount);
        Move(DataAsByte[Result], FInProgressBuffer^.Data[FInProgressBuffer^.UsedData], WriteCount);
        Inc(FInProgressBuffer^.UsedData, WriteCount);

        Inc(Result, WriteCount);
        Dec(ACount, WriteCount);

        // if the buffer is full then mark it as used
        if FInProgressBuffer^.UsedData >= AUDIO_BUFFER_SIZE then
        begin
          NextEmptyBuffer;
        end;
      end
      else
      begin
        //WriteLn('FBufferCurrent not assigned!');
        Break;
      end;
    until ACount = 0;

  finally
    LeaveCriticalsection(FBufferLock);
    //WriteLn('left crit');
  end;

end;}

{procedure TPAAudioBufferManager.BufferProcessed;
var
  Buf: PAudioBuffer;
begin
  WriteLn('000000000000000000000000000000000');
  Buf := FFirstBuffer;
  if Buf = nil then
    Exit;
  EnterCriticalsection(FBufferLock);
    FFirstBuffer:=Buf^.NextBuffer;
    BufferPool.ReturnBufferToPool(Buf);
  LeaveCriticalsection(FBufferLock);
end;}

function TPAAudioBufferManager.Empty: Boolean;
var
  i: integer = 0;
  B: PAudioBuffer;
begin
  Result := (FBuffers.Count = 0) and (FBuffersWaitingToQueue = 0);
  {if not Result then
  begin

    B := FFirstBuffer;
    repeat
      inc(i);
      B := B^.NextBuffer;
    until B = nil;
    WriteLn(FOwner.GetObject.ClassName,' Buffer not empty. Count = ', i);
  end
  else
    WriteLn(FOwner.GetObject.ClassName,' Buffer empty');
   }
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
begin
  while not Terminated do
  begin
    try
      Res := FDataEvent.WaitFor(1000);
      if Res = wrSignaled then
      begin
        Buffer := FBufferManager.NextFilledBuffer;
        if Assigned(Buffer) then
        begin
          try
            // see if we need to convert the data to a different format.
            if (Format <> afRaw) and not (Buffer^.Format in [Format, afRaw]) then
              RealData := ConvertAudio[Buffer^.Format][Format](@Buffer^.Data, Buffer^.UsedData, RealDataSize)
            else
            begin
              RealData:=@Buffer^.Data;
              RealDataSize:=Buffer^.UsedData;
            end;

            InternalProcessData(RealData^, RealDataSize, Buffer^.IsEndOfData);
          except
            on E: Exception do
            begin
              WriteLn(ClassName+' Exception: '+E.Message);
            end;
          end;
          // if a conversion has occured then free the allocated data.
          if RealData <> @Buffer^.Data then
            Freemem(RealData);
          BufferPool.ReturnBufferToPool(Buffer);
          //FBufferManager.BufferProcessed;
        end
        else
        begin
           //WriteLn('No data to process. Blocking unitil ready');
          LockDataEvent;
          FDataEvent.ResetEvent; // get set when a new block is available
          if FDataIsEnded then
            ;
          UnlockDataEvent;
        end;
      end
      else
        if Res <> wrTimeout then
          WriteLn(ClassName,' waitfor = ', Res);

    except
      on E: Exception do
      begin
        WriteLn(ClassName+' Exception: '+E.Message);
      end;
    end;
  end;
end;

function TPAAudioDestination.GetDataSource: IPAAudioSource;
begin
  Result := FDataSource;
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
  FDataEvent.SetEvent;
  Synchronize(@DoOnDataEnded);
end;

function TPAAudioDestination.GetObject: TObject;
begin
  Result := Self;
end;

procedure TPAAudioDestination.LockDataEvent;
begin
  EnterCriticalsection(FDataEventLock);
end;

procedure TPAAudioDestination.UnlockDataEvent;
begin
  LeaveCriticalsection(FDataEventLock);
end;

constructor TPAAudioDestination.Create;
begin
  FDataEvent := TPABufferEvent.Create;
  InitCriticalSection(FDataEventLock);
  FBufferManager := TPAAudioBufferManager.Create(Self, 4, False);
  inherited Create;
end;

destructor TPAAudioDestination.Destroy;
begin
  FBufferManager.Free;
  DoneCriticalSection(FDataEventLock);
  FDataEvent.Free;
  inherited Destroy;
end;

function TPAAudioDestination.WriteBuffer(ABuffer: PAudioBuffer): Int64;
begin
  LockDataEvent;
  try
    Result := FBufferManager.WriteBuffer(ABuffer);
    SetDataEvent;
  finally
    UnlockDataEvent;
  end;
end;

{function TPAAudioDestination.WriteData(const Data; ACount: Int64): Int64;
var
  NewBufferAvailable: Boolean;
begin
  {if FDataIsAtEnd then
    Exit; // no more data
}
  Result := FBufferManager.WriteData(Data, ACount, NewBufferAvailable);

  if FDataIsEnded then
    FBufferManager.Flush;

  if NewBufferAvailable or FDataIsEnded then
    FDataEvent.SetEvent; // what if event is already set?
end;}

procedure TPAAudioDestination.SetDataEvent;
begin
  //WriteLn(ClassName,' data event set');
  LockDataEvent;
  try
    FDataEvent.SetEvent;
  finally
    UnlockDataEvent;
  end;
end;

{ TPAAudioLink }

{procedure TPAAudioLink.BufferFilled;
begin
  //WriteLn(ClassName,' : Setting Event');
  StartData;
end;}

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
  Result := Buf <> nil;
  if Assigned(Buf) then
  begin
    {if Buf^.IsEndOfData and FBufferManager.Empty then
      FDataIsEnded:=True;}
    //WriteLn(ClassNAme,' Have Buf. Calling InternalProcessData');
    //WriteLn('Writing ',Buf^.UsedData,' bytes');
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
       WriteLn(ClassName+' Exception: '+E.Message);
     end;
   end;

   if RealData <> @Buf^.Data then
     Freemem(RealData);

   BufferPool.ReturnBufferToPool(Buf);

    //BufferEmpty; // if manager is empty unset data event
  end;

  //if FDataIsEnded and FBufferManager.Empty then
  if FBufferManager.Empty and (((Buf = nil) or Buf^.IsEndOfData) and FDataIsEnded) then
  begin
    SignalDestinationsDone; // ....
    Result := False;
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

function TPAAudioLink.WriteBuffer(ABuffer: PAudioBuffer): Int64;
begin
  Result := FBufferManager.WriteBuffer(ABuffer);
  //WriteLn(ClassName, ' got buffer. sent to manager');
  SetDataEvent;
end;

procedure TPAAudioLink.SetDataEvent;
begin
  FDataEvent.SetEvent;
  //WriteLn(Classname,' setdataevent');
end;

procedure TPAAudioLink.EndOfData;
begin
  EnterCriticalsection(FCrit);
  //WriteLn(ClassName, ': Data is ended');
  FDataIsEnded:=True;

  FBufferManager.Flush;

  // ensure we process "data" even if empty to signal destination
  FDataEvent.SetEvent;

  //if FBufferManager.Empty then
  //begin

  //  BufferFilled; // causes Execute loop to iterate
  //end;
  LeaveCriticalsection(FCrit);
end;

function TPAAudioLink.GetObject: TObject;
begin
  REsult := Self;
end;

procedure TPAAudioSource.EnsureAudioBuffer;
var
  Res: TWaitResult;
begin
  if FDestBuf = nil then
    FDestBuf:= BufferPool.GetBufferFromPool(True)
  else if FDestBuf^.UsedData = AUDIO_BUFFER_SIZE then
  begin
    WriteToDestinations(FDestBuf);
    FDestBuf := BufferPool.GetBufferFromPool(True);
  end;
  //WriteLn(ClassName,' got buffer');
end;

procedure TPAAudioSource.RaiseE;
begin
  //Raise E;
end;

procedure TPAAudioSource.Execute;
var
  BeforeLoopCalled: Boolean;
  Res: TWaitResult;
begin
  BeforeLoopCalled:=False;
  while not Terminated do
  begin
    try
    Res := FDataEvent.WaitFor(1000);
    case Res of
    wrSignaled :
    begin
      if not BeforeLoopCalled then
      begin
        BeforeLoopCalled:=True;
        BeforeExecuteLoop;
      end;
      //WriteLn(ClassName,' :Source Loop');
      if InternalOutputToDestination = False then
      begin
        //WriteLn('Stopping Data');
        StopData;
      end;
    end;
    wrTimeout: ; // maybe reset event after a while?
    else
      ;//WriteLn(StdErr, ClassName,' waitfor = ', Res);  // Triggered when Freed

    end;

    except
      on E: Exception do
      begin
        WriteLn(ClassName+' Exception: '+E.Message);
        Synchronize(@RaiseE);

        //prin
      end;
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

procedure TPAAudioSource.AddDestination(AValue: IPAAudioDestination);
var
  i: Integer;
begin
  i := FDestinations.IndexOf(Pointer(AValue));
  if i >= 0 then
    FDestinations.Delete(i);

  FDestinations.Add(Pointer(AValue));
end;

procedure TPAAudioSource.RemoveDestination(AValue: IPAAudioDestination);
var
  i: Integer;
begin
  i := FDestinations.IndexOf(Pointer(AValue));
  if i >= 0 then
    FDestinations.Delete(i);
end;

procedure TPAAudioSource.WriteToDestinations(const ABuffer: PAudioBuffer);
var
  i: Integer;
  Buf: PAudioBuffer;
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

  //WriteLn('Writing to dest');
  for i := 1 to FDestinations.Count-1 do
  begin
    // when we write buffers then it owns the buffer so each destination needs a copy except the first
    Buf := BufferPool.GetBufferFromPool(True);
    Buf^  := ABuffer^;

    IPAAudioDestination(FDestinations[i]).WriteBuffer(Buf);
  end;
  IPAAudioDestination(FDestinations[0]).WriteBuffer(ABuffer);
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
  inherited Create(False);
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
  FDataEvent := TSimpleEvent.Create;
  FBufferAvailable := TPABufferEvent.Create;

  inherited Create;
  FDestinations := TFPList.Create;
  BufferPool.AllocateBuffers(1);
end;

destructor TPAAudioSource.Destroy;
begin
  FDestinations.Free;
  FDataEvent.Free;
  FBufferAvailable.Free;
  inherited Destroy;
end;

procedure TPAAudioSource.StartData;
begin
  FSignaled:=False;
  FDataEvent.SetEvent;
  FWorking:=True;
end;

function TPAAudioSource.Working: Boolean;
begin
  Result := FWorking;
end;

procedure TPAAudioSource.StopData;
var
  i: Integer;
begin
  FDataEvent.ResetEvent;
  FWorking:=False;
end;

constructor TPAAudioLink.Create;
begin
  InitCriticalSection(FCrit);
  FBufferManager := TPAAudioBufferManager.Create(Self, 6, False);
  inherited Create;
end;

destructor TPAAudioLink.Destroy;
begin
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

