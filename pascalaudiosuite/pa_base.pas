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
  Classes, SysUtils, syncobjs, pa_lists, paio_types, paio_messagequeue;

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

  TPABufferMessage = class(TPAIOMessage)
    Buffer: PAudioBuffer;
    Dest: IPAAudioDestination;
    constructor Create(ABuffer: PAudioBuffer; ADest: IPAAudioDestination);
    destructor Destroy; override;
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
    E: Exception;
    FFreeEvent: TSimpleEvent; // only used during the destructor
    FSignaled: Boolean;
    FDestBuf: PAudioBuffer;
    //Datasource: IPAAudioSource;
    procedure EnsureAudioBuffer;
    procedure RaiseE;
  protected
    function  DestroyWaitSync: Boolean;
    procedure Execute; override;
    procedure BeforeExecuteLoop; virtual;
    procedure AfterExecuteLoop; virtual;
    function InternalOutputToDestination: Boolean; virtual; abstract;
    procedure HandleMessage(var AMsg: TPAIOMessage); virtual;
    // IPAAudioSource
    procedure AddDestination(AValue: IPAAudioDestination); virtual;
    procedure RemoveDestination(AValue: IPAAudioDestination); virtual;
    procedure WriteToDestinations(const ABuffer: PAudioBuffer);{ virtual;}
    function  WriteToBuffer(const AData; ASize: Integer; AIsLastData: Boolean): Integer;
    procedure SignalDestinationsDone; virtual;
    function  GetSourceObject: TObject;
    procedure ClearSignal;

  private
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

{ TPABufferMessage }

constructor TPABufferMessage.Create(ABuffer: PAudioBuffer; ADest: IPAAudioDestination);
begin
  inherited Create(PAM_SendBuffer);
  Buffer:=ABuffer;
  Dest := ADest;
end;

destructor TPABufferMessage.Destroy;
begin
  if Assigned(Buffer) then
    BufferPool.ReturnBufferToPool(Buffer);
  inherited Destroy;
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
  //WriteLn('Pool: Asking for buffer. Available = ',FBufferList.Count);
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

  if GetQueuedBufferCount >= FBufferCount then
    WriteLn(FOwner.GetObject.Classname, ' queued buffers = ', GetQueuedBufferCount);

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
  while not Terminated do
  begin
    try
      Res := FMsgQueue.WaitMessage(1000, Msg);
      if Res = wrSignaled then
      begin
        case Msg.Message of
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
  FMsgQueue.PostMessage(PAM_DataEnd);
  Synchronize(@DoOnDataEnded);
end;

function TPAAudioDestination.GetObject: TObject;
begin
  Result := Self;
end;

constructor TPAAudioDestination.Create;
begin
  FBufferManager := TPAAudioBufferManager.Create(Self, 4, False);
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
  Result := Buf <> nil;
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
       WriteLn(ClassName+' Exception: '+E.Message);
     end;
   end;

   // if a conversion took place then free converted data since it wasn't from the pool
   if RealData <> @Buf^.Data then
     Freemem(RealData);

   BufferPool.ReturnBufferToPool(Buf);
  end;

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

function TPAAudioLink.WriteBuffer(ABuffer: PAudioBuffer): Boolean;
begin
  Result := FBufferManager.WriteBuffer(ABuffer);
  //WriteLn(ClassName, ' got buffer. sent to manager');
  SetDataEvent;
end;

procedure TPAAudioLink.SetDataEvent;
begin
  FMsgQueue.PostMessage(PAM_DataEnd);
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

function TPAAudioSource.DestroyWaitSync: Boolean;
begin
  // it's safe to call DestroyWaitSync at each destructor start and it won't keep setting the event
  if Assigned(FFreeEvent) then
    Exit(True);
  FFreeEvent := TSimpleEvent.Create;
  FMsgQueue.InsertMessage(PAM_ObjectIsDestroying);
  FFreeEvent.WaitFor(5000); // wait up to 5 seconds for execute to finish
  FFreeEvent.Free; // but don't set to nil!
end;

procedure TPAAudioSource.Execute;
var
  BeforeLoopCalled: Boolean;
  Res: TWaitResult;
  Msg: TPAIOMessage;
  BufferMessage: TPABufferMessage absolute Msg;
begin
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
          if InternalOutputToDestination = False then
          begin
          //WriteLn('Stopping Data');
            StopData;
          end
          else
          begin
            // keep processing data
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
          if not BufferMessage.Dest.WriteBuffer(BufferMessage.Buffer) then
          begin
           // make sure this is sent before we process more data
           // insert message before other data messages but after other messages like PAM_ObjectIsDestroying
           //WriteLn('Couldn''t send buffer re-queing it');
           FMsgQueue.InsertBefore([PAM_Data, PAM_SendBuffer, PAM_DataEnd], Msg);
           Msg := nil; // to avoid freeing since it's back in the queue
          end
          else
            BufferMessage.Buffer:=nil; // otherwise when the message is freed it will return the buffer to the pool
        end;
      else
        // it is a message specific for the child to choose to handle
        HandleMessage(Msg);
        if Assigned(Msg) then
          Msg.Free;

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

procedure TPAAudioSource.HandleMessage(var AMsg: TPAIOMessage);
begin
  // messages for the subclasses should be handled here
  // this is called from the context of this thread
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
  Msg: TPABufferMessage;
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

    Msg := TPABufferMessage.Create(Buf, IPAAudioDestination(FDestinations[i]));
    FMsgQueue.PostMessage(Msg);
  end;
  Msg := TPABufferMessage.Create(ABuffer, IPAAudioDestination(FDestinations[0]));
  FMsgQueue.PostMessage(Msg);
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
  FDestinations := TFPList.Create;
  BufferPool.AllocateBuffers(1);
end;

destructor TPAAudioSource.Destroy;
begin
  DestroyWaitSync;
  FDestinations.Free;
  inherited Destroy;
end;

procedure TPAAudioSource.StartData;
begin
  FSignaled:=False;
  FMsgQueue.PostMessage(PAM_Data);
  FWorking:=True;
end;

function TPAAudioSource.Working: Boolean;
begin
  Result := FWorking;
end;

procedure TPAAudioSource.StopData;
begin
  if FWorking then
  FMsgQueue.PostMessage(PAM_StopWorking);
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

