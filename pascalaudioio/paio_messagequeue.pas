{
    This unit is part of Pascal Audio IO package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

{
  This implements a threadsafe message queue using a critical section and a
  TSimpleEvent. It's pretty generic and could be used in most anything. The
  messages are class based so data in a message can be freed easily.
}

unit paio_messagequeue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs;

type
  TPAIOMessage = class
  private
    FNext: TPAIOMessage;
    FMsg: Integer;
    FSimpleData: Variant;
  public
    constructor Create(AMsg: Integer);
    property Message: Integer read FMsg;
    property Data: Variant read FSimpleData write FSimpleData;
  end;


  // A threadsafe message queue

  { TPAIOMessageQueue }

  TPAIOMessageQueue = class
  private
    FLock: TRTLCriticalSection;
    FFirst: TPAIOMessage;
    FLast: TPAIOMessage;
    FHasMessage: TSimpleEvent;
    FCount: Integer;
    procedure FreeMessages;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   PostMessage(AMessage: TPAIOMessage); // append message to end of the queue
    procedure   PostMessage(AMsg: Integer; AData: Variant);
    procedure   PostMessage(AMsg: Integer);
    function    PopMessage: TPAIOMessage; // remove message from start of the queue
    function    HasMessage: Boolean;
    procedure   InsertMessage(AMsg: Integer); // inserts message to the start of the queue to be used in the queue
    procedure   InsertMessage(AMsg: TPAIOMessage); // inserts message to the start of the queue to be used in the queue
    procedure   InsertBefore(AMessages: array of Integer; AMsgObject: TPAIOMessage); // message will be inserted before messages of [types]
    function    WaitMessage(ATimeout: Integer; var AMsg: TPAIOMessage): TWaitResult;
    function    WaitMessage(ATimeout: Integer): TWaitResult;
    function    Count: Integer;
  end;

implementation

{ TPAIOMessage }

constructor TPAIOMessage.Create(AMsg: Integer);
begin
  FMsg:=AMsg;
end;

{ TPAIOMessageQueue }

procedure TPAIOMessageQueue.FreeMessages;
var
  Tmp: TPAIOMessage;
begin
  EnterCriticalsection(FLock);
  try
    while Assigned(FFirst) do
    begin
      Tmp := FFirst;
      FFirst := FFirst.FNext;
      Tmp.Free;
    end;
  finally
    LeaveCriticalsection(FLock);
  end;
end;

constructor TPAIOMessageQueue.Create;
begin
  InitCriticalSection(FLock);
  FHasMessage := TSimpleEvent.Create;
end;

destructor TPAIOMessageQueue.Destroy;
begin
  FreeMessages;
  FHasMessage.Free;
  DoneCriticalsection(FLock);
  inherited Destroy;
end;

procedure TPAIOMessageQueue.PostMessage(AMessage: TPAIOMessage);
begin
  EnterCriticalsection(FLock);
  Inc(FCount);
  try
    if Assigned(FLast) then
      FLast.FNext := AMessage;

    if not Assigned(FFirst) then
      FFirst := AMessage;

    FLast := AMessage;
    AMessage.FNext := nil;

  finally
    FHasMessage.SetEvent;
    LeaveCriticalsection(FLock);
  end;
end;

procedure TPAIOMessageQueue.PostMessage(AMsg: Integer; AData: Variant);
var
  Msg: TPAIOMessage;
begin
  Msg := TPAIOMessage.Create(AMsg);
  MSg.Data:=AData;
  PostMessage(Msg);
end;

procedure TPAIOMessageQueue.PostMessage(AMsg: Integer);
var
  Msg: TPAIOMessage;
begin
  Msg := TPAIOMessage.Create(AMsg);
  PostMessage(Msg);
end;


function TPAIOMessageQueue.PopMessage: TPAIOMessage;
begin
  Result := nil;
  EnterCriticalsection(FLock);
  try
    if Assigned(FFirst) then
    begin
      Dec(FCount);
      Result := FFirst;
      FFirst := FFirst.FNext;
      //Result.FNext := nil;
      if Result = FLast then
      begin
        FLast := nil;
        if FCount > 0 then
          Raise Exception.Create('Message reference shenanigans');
        FHasMessage.ResetEvent;
      end;

    end
    else if FCount > 0 then
      Raise Exception.Create('Message reference shenanigans');
  finally

    LeaveCriticalsection(FLock);
  end;
end;

function TPAIOMessageQueue.HasMessage: Boolean;
begin
  Result := FFirst <> nil;
end;

procedure TPAIOMessageQueue.InsertMessage(AMsg: Integer);
begin
  InsertMessage(TPAIOMessage.Create(AMsg));
end;

procedure TPAIOMessageQueue.InsertMessage(AMsg: TPAIOMessage);
begin
  EnterCriticalsection(FLock);
  Inc(FCount);
  try
    AMsg.FNext := FFirst;
    if FLast = nil then
      FLast := AMsg;
    FFirst := AMsg;
  finally
    if (AMsg.FNext = nil) and (AMsg <> FLast) then Raise Exception.Create('invalid insertion onto queue');
    FHasMessage.SetEvent;
    LeaveCriticalsection(FLock);
  end;
end;

operator in (A: Integer; L: Array of Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i in L do
    if i = A then
      Exit(True);

end;

procedure TPAIOMessageQueue.InsertBefore(AMessages: array of Integer; AMsgObject: TPAIOMessage);
var
  Prev: TPAIOMessage = nil;
  Current: TPAIOMessage;
begin
  EnterCriticalsection(FLock);
  try
    if not Assigned(FFirst) then
    begin
      PostMessage(AMsgObject);
      Exit;
    end;

    Current := FFirst;
    repeat
      if Current.Message in AMessages then
      begin
        Inc(Fcount);
        AMsgObject.FNext := Current;
        if Assigned(Prev) then
          Prev.FNext := AMsgObject
        else
        begin
          if Current <> FFirst then
            Raise Exception.Create('bad');
          FFirst := AMsgObject;
        end;
        Exit;
      end;
      Prev := Current;
      Current := Current.FNext;
    until Current = nil;
    PostMessage(AMsgObject);
    {if Assigned(FLast) then
      FLast.FNext := AMsgObject
    else
    begin
      FFirst := AMsgObject;
      FLast := AMsgObject;
    end;}
  finally
    if (AMsgObject.FNext = nil) and (AMsgObject <> FLast) then Raise Exception.Create('invalid insertion onto queue');
    LeaveCriticalsection(FLock);
  end;
end;

function TPAIOMessageQueue.WaitMessage(ATimeout: Integer; var AMsg: TPAIOMessage): TWaitResult;
begin
  {if FCount > 0 then
  begin
    Result := wrSignaled;
    AMsg := PopMessage;
    if Assigned(AMsg) then
      Exit;
  end;}
  Result := FHasMessage.WaitFor(ATimeout);
  if Result = wrSignaled then
    AMsg := PopMessage
  else
    AMsg := nil;
end;

function TPAIOMessageQueue.WaitMessage(ATimeout: Integer): TWaitResult;
begin
  Result := FHasMessage.WaitFor(ATimeout);
end;

function TPAIOMessageQueue.Count: Integer;
begin
  Result := FCount;
end;

end.

