{
    This unit is part of Pascal Audio IO package.

    Copyright (c) 2016 by Andrew Haines.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

unit noiseremovalmultichannel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, paio_channelhelper, audacity_noiseremoval;

type

  TNoiseWriteProc = audacity_noiseremoval.TNoiseWriteProc;

  { TNoiseRemovalChannel }

  TNoiseRemovalChannel = class(TNoiseRemoval, IPAIODataIOInterface)
    HasProfile: Boolean;
    ProfileComplete: Boolean;
    procedure WriteDataIO(ASender: IPAIODataIOInterface; AData: PSingle; ASamples: Integer);
  end;

  { TNoiseRemovalMultiChannel }

  TNoiseRemovalMultiChannel = class(IPAIODataIOInterface)
  private
    FChannels,
    FSampleRate: Integer;
    FHelper: TPAIOChannelHelper;
    FNoise: array of TNoiseRemovalChannel;
    FWriteProc: TNoiseWriteProc;
    //IPAIODataIOInterface
    procedure WriteDataIO(ASender: IPAIODataIOInterface; AData: PSingle; ASamples: Integer);
    procedure DataWrite(ASender: TObject; AData: PSingle; ASampleCount: Integer);
  public
    constructor Create(AChannels: Integer; ASampleRate: Integer);
    destructor Destroy; override;
    procedure ReadNoiseProfile(AData: PSingle; ASamples: Integer);
    procedure ProcessNoise(AData: PSingle; ASamples: Integer);
    procedure Flush;
    property WriteProc: TNoiseWriteProc read FWriteProc write FWriteProc;
  end;

implementation

{ TMultiChannelNoiseRemoval }

procedure TNoiseRemovalMultiChannel.WriteDataIO(ASender: IPAIODataIOInterface; AData: PSingle; ASamples: Integer);
begin
  if Assigned(FWriteProc) then
    FWriteProc(Self, AData, ASamples);
end;

procedure TNoiseRemovalMultiChannel.DataWrite(ASender: TObject; AData: PSingle; ASampleCount: Integer);
begin
  (FHelper as IPAIODataIOInterface).WriteDataIO(ASender as IPAIODataIOInterface, AData, ASampleCount);
end;

constructor TNoiseRemovalMultiChannel.Create(AChannels: Integer;
  ASampleRate: Integer);
var
  i: Integer;
begin
  FChannels:=AChannels;
  FSampleRate:=ASampleRate;
  FHelper := TPAIOChannelHelper.Create(Self);
  SetLength(FNoise, AChannels);
  for i := 0 to High(FNoise) do
  begin
    FNoise[i] := TNoiseRemovalChannel.Create;
    FNoise[i].WriteProc:=@DataWrite;
    FNoise[i].Init(ASampleRate);
    FHelper.Outputs.Add(FNoise[i] as IPAIODataIOInterface);
  end;
end;

destructor TNoiseRemovalMultiChannel.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FNoise) do
  begin
    FNoise[i].Free;
  end;
  SetLength(FNoise, 0);
  FHelper.Free;
end;

procedure TNoiseRemovalMultiChannel.ReadNoiseProfile(AData: PSingle;
  ASamples: Integer);
var
  i: Integer;
begin
  FHelper.Write(AData, ASamples);
  for i := 0 to High(FNoise) do
  begin
    FNoise[i].ProfileComplete:=True;
    FNoise[i].Process(nil, 0, True, False);
    FNoise[i].HasProfile:=True;
    FNoise[i].Init(FSampleRate);
  end;
end;

procedure TNoiseRemovalMultiChannel.ProcessNoise(AData: PSingle;
  ASamples: Integer);
begin
  FHelper.Write(AData, ASamples);
end;

procedure TNoiseRemovalMultiChannel.Flush;
var
  i: Integer;
begin
  for i := 0 to High(FNoise) do
    FNoise[i].Flush;
end;

procedure TNoiseRemovalChannel.WriteDataIO(ASender: IPAIODataIOInterface;
  AData: PSingle; ASamples: Integer);
begin
  Process(AData, ASamples, not HasProfile, not HasProfile);
end;


end.


