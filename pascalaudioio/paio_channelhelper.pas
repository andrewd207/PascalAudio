{
    This unit is part of Pascal Audio IO package.

    Copyright (c) 2016 by Andrew Haines.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit paio_channelhelper;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, paio_types, pa_ringbuffer;

type
  IPAIODataIOInterface = interface
  ['IPAIODataIOInterface']
    procedure WriteDataIO(ASender: IPAIODataIOInterface; AData: PSingle; ASamples: Integer);
  end;

  { TPAIOChannelHelper }

  TPAIOChannelHelper = class(IPAIODataIOInterface)
  private
    FOutputs: TList;
    FTarget: IPAIODataIOInterface; // where we will send plexed data.
    FBuffers: TChannelArray;
    FPos: array of Integer;
    // called by the individual channel objects.
    procedure WriteDataIO(ASender: IPAIODataIOInterface; AData: PSingle; ASamples: Integer);
    procedure AllocateBuffers;
    procedure SendDataToTarget;
  public
    constructor Create(APlexedTarget: IPAIODataIOInterface);
    destructor Destroy; override;
    property Outputs: TList read FOutputs;// of  IPAIOSplitterJoinerInterface. Each is a channel in order.
    procedure Write(AData: PSingle; ASamples: Integer); // this expects interleaved data.
  end;


implementation
uses
  paio_utils;

{ TPAIOChannelHelper }

procedure TPAIOChannelHelper.WriteDataIO(ASender: IPAIODataIOInterface; AData: PSingle; ASamples: Integer);
var
  BufIndex: Integer;
  BufSize, WCount: Integer;
  Written: Integer = 0;
begin
  BufIndex := FOutputs.IndexOf(Pointer(ASender));

  if BufIndex = -1 then
    raise Exception.Create('Trying to write data from an unknown instance');

  AllocateBuffers;

  BufSize := Length(FBuffers[0]);

  While ASamples > 0 do
  begin
    WCount := Min(BufSize-FPos[BufIndex], ASamples);
    Move(AData[Written], FBuffers[BufIndex][0], WCount*SizeOf(Single));
    Inc(Written, WCount);
    Dec(ASamples, WCount);
    Inc(FPos[BufIndex], WCount);

    if BufIndex = High(FBuffers) then
      SendDataToTarget;
  end;
end;

procedure TPAIOChannelHelper.AllocateBuffers;
begin
  if Length(FBuffers) <> FOutputs.Count then
  begin
    SetLength(FBuffers, 0);
    FBuffers := NewChannelArray(FOutputs.Count, AUDIO_BUFFER_SIZE*2);
    SetLength(FPos, FOutputs.Count);
  end;
end;

procedure TPAIOChannelHelper.SendDataToTarget;
var
  Plexed: TSingleArray;
  HighestCount: Integer = 0;
  i: Integer;
begin
  for i := 0 to High(FPos) do
    if FPos[i] > HighestCount then
      HighestCount:=FPos[i];
  Plexed := JoinChannels(FBuffers, HighestCount);

  FTarget.WriteDataIO(Self, @Plexed[0], Length(Plexed));

  for i := 0 to High(FPos) do
    Dec(FPos[i], HighestCount);
end;

constructor TPAIOChannelHelper.Create(APlexedTarget: IPAIODataIOInterface);
begin
  FOutputs := TList.Create;
  FTarget := APlexedTarget;
end;

destructor TPAIOChannelHelper.Destroy;
begin
  FOutputs.Free;
  inherited Destroy;
end;

procedure TPAIOChannelHelper.Write(AData: PSingle; ASamples: Integer);
var
  Channels: TChannelArray;
  i: Integer;
  Pos: Integer = 0;
  WCount: Integer;
begin
  AllocateBuffers;
  Channels := SplitChannels(AData, ASamples, Outputs.Count);
  while ASamples > 0 do
  begin
    WCount := Min(1024, ASamples div Outputs.Count);
    for i := 0 to Outputs.Count-1 do
    begin
      IPAIODataIOInterface(Outputs.Items[i]).WriteDataIO(Self, @Channels[i][Pos], WCount);
    end;
    Dec(ASamples, WCount * Outputs.Count);
    Inc(Pos, WCount);
  end;
end;

end.

