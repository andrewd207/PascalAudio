{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_noiseremoval;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pa_base, audacity_noiseremoval;

type
  { TPANoiseRemovalLink }

  TPANoiseRemovalLink = class(TPAAudioLink, IPAAudioInformation)
  private
    FInited: Boolean;
    FNoise: array of TNoiseRemoval;
    FOutData: TChannelArray;
    FOutWritePos: array of Integer;
    FIsLast: Boolean;
    procedure InitData;
    procedure NoiseRemovalDone(ASender: TNoiseRemoval; AData: PSingle; ASampleCount: Integer);
  protected
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; override;
    procedure SignalDestinationsDone; override;
    function GetFormat: TPAAudioFormat; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    // set this before we staret processing but after datasource is assigned.
    procedure SetNoiseProfile(AChannel: Integer; AData: PSingle; ASampleCount: Integer);
  end;

implementation

{ TPANoiseRemovalLink }

procedure TPANoiseRemovalLink.InitData;
var
  i: Integer;
begin
  if FInited then
    Exit;

  FInited := True;

  SetLength(FNoise, Channels);
  for i := 0 to Channels-1 do
  begin
    FNoise[i] := TNoiseRemoval.Create;
    //FNoise[i].Level:=30; //does nothing!
    //FNoise[i].Sensitivity:=20;//3.95;
    FNoise[i].Sensitivity:=9.95;
   // FNoise[i].Gain:=-48;
   // FNoise[i].AttackDecayTime:=1000;
    FNoise[i].Init(SamplesPerSecond);
    FNoise[i].WriteProc:=@NoiseRemovalDone;
  end;

  FOutData := NewChannelArray(Channels, AUDIO_BUFFER_FLOAT_SAMPLES div Channels);
  SetLength(FOutWritePos, Channels);
end;

procedure TPANoiseRemovalLink.NoiseRemovalDone(ASender: TNoiseRemoval; AData: PSingle; ASampleCount: Integer);
var
  i: Integer;
  WriteSize: Integer;
  JoinedData: TSingleArray;
begin
  for i := 0 to High(FNoise) do
  begin
    if FNoise[i] = ASender then
      Break;
  end;

  repeat
    WriteSize := ASampleCount;
    if WriteSize > Length(FOutData[0]) - FOutWritePos[i] then
      WriteSize := Length(FOutData[0]) - FOutWritePos[i];

    Dec(ASampleCount, WriteSize);
    Move(AData^, FOutData[i][FOutWritePos[i]], SizeOf(Single) * WriteSize);
    Inc(FOutWritePos[i], WriteSize);
  until ASampleCount = 0;

  // everytime the last channel is written plex the channels and write it out.
  if i = High(FOutData) then
  begin
    JoinedData := JoinChannels(FOutData, FOutWritePos[i]);
    WriteToBuffer(JoinedData[0], Length(JoinedData)*SizeOf(Single), FIsLast);
    for i := 0 to High(FOutWritePos) do
      FOutWritePos[i] := 0;
  end;
end;

function TPANoiseRemovalLink.InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64;
var
  ChanData: TChannelArray;
  Samples,
  i: Integer;
begin
  Samples := ACount div SizeOf(Single);
  ChanData:= SplitChannels(@AData, Samples, Channels);
  for i := Low(ChanData) to High(ChanData) do
  begin
    FNoise[i].Process(@ChanData[i][0], Length(ChanData[i]), False);
    FIsLast:=AIsLastData;
    if FIsLast then
      FNoise[i].Flush;
  end;
end;

procedure TPANoiseRemovalLink.SignalDestinationsDone;
begin
  inherited SignalDestinationsDone;
end;

function TPANoiseRemovalLink.GetFormat: TPAAudioFormat;
begin
  Result:=afFloat32;
end;

constructor TPANoiseRemovalLink.Create;
begin
  inherited Create;
end;

destructor TPANoiseRemovalLink.Destroy;
begin
  inherited Destroy;
end;

procedure TPANoiseRemovalLink.SetNoiseProfile(AChannel: Integer; AData: PSingle;
  ASampleCount: Integer);
var
  Profile: TSingleArray;
begin
  InitData;
  FNoise[AChannel].Process(AData, ASampleCount, True);

  Profile := FNoise[AChannel].NoiseProfile;
  // call init again because inited is false after Setting the noise profile.
  FNoise[AChannel].Init(SamplesPerSecond);
end;

end.

