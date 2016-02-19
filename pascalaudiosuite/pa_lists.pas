{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_lists;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PPAFifoItem = ^TPAFifoItem;
  TPAFifoItem = record
    Item: Pointer;
    Next: PPAFifoItem;
  end;

  { TPAFifoList }

  TPAFifoList = class
  private
    FCrit: TRTLCriticalSection;
    FFirst: PPAFifoItem;
    FLast: PPAFifoItem;
    FCount: Integer;
  public
    procedure AddObject(AObject: TObject);
    function  GetObject: TObject;
    procedure AddItem(AItem: Pointer);
    function  GetItem: Pointer;
    function  Count: Integer;
    function  Contains(AItem: Pointer): Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TPAFifoList }

procedure TPAFifoList.AddObject(AObject: TObject);
begin
  AddItem(Pointer(AObject));
end;

function TPAFifoList.GetObject: TObject;
begin
  Result:= TObject(GetItem);
end;

procedure TPAFifoList.AddItem(AItem: Pointer);
var
  Tmp: PPAFifoItem;
begin
  if AItem = nil then
  begin
    WriteLn('Added nil item to list!');
    Exit;
  end;
  New(Tmp);
  Tmp^.Item:=AItem;
  Tmp^.Next:=nil;
  try
    EnterCriticalsection(FCrit);
    if FFirst = nil then
      FFirst := Tmp
    else
      FLast^.Next := Tmp;
    FLast := Tmp;
    Inc(FCount);
  finally
    LeaveCriticalsection(FCrit);
  end;
end;

function TPAFifoList.GetItem: Pointer;
var
  Tmp: PPAFifoItem;
begin
  Result := nil;
  try
    EnterCriticalsection(FCrit);
    if FFirst <> nil then
    begin
      Tmp := FFirst;
      Result := Tmp^.Item;
      FFirst := Tmp^.Next;

      // check if we just used the last item in the list
      if Tmp = FLast then
        FLast := nil;
      Dec(FCount);
      Dispose(Tmp);

    end;
  finally
    LeaveCriticalsection(FCrit);
  end;
end;

function TPAFifoList.Count: Integer;
begin
  Result := FCount;
end;

function TPAFifoList.Contains(AItem: Pointer): Boolean;
var
  Tmp: PPAFifoItem;
begin
  Result := False;
  EnterCriticalsection(FCrit);
  try
   Tmp := FFirst;
   while Tmp <> nil do
   begin
     if Tmp^.Item = AItem then
       Exit(True);
     Tmp := Tmp^.Next;
   end;
  finally
    LeaveCriticalsection(FCrit);
  end;
end;

constructor TPAFifoList.Create;
begin
  InitCriticalSection(FCrit);
  FFirst:=nil;
  FLast:=nil;
end;

destructor TPAFifoList.Destroy;
var
  Tmp: PPAFifoItem;
begin
  FLast:=nil;
  while FFirst <> nil do
  begin
    Tmp := FFirst^.Next;
    Dispose(FFirst);
    FFirst:=Tmp;
  end;

  DoneCriticalsection(FCrit);
  inherited Destroy;
end;

end.

