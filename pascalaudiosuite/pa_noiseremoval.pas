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
  Classes, SysUtils, pa_base, audacity_noiseremoval, paio_channelhelper;

type
  { TPANoiseRemovalLink }

  TPANoiseRemovalLink = class(TPAAudioLink, IPAAudioInformation, IPAIODataIOInterface)
  private
    FInited: Boolean;
    FNoise: array of TNoiseRemoval;
    FHelper: TPAIOChannelHelper;
    FIsLast: Boolean;
    procedure InitData;
    procedure NoiseRemovalDone(ASender: TObject; AData: PSingle; ASampleCount: Integer);
    procedure WriteDataIO(ASender: IPAIODataIOInterface; AData: PSingle; ASamples: Integer);
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

type

 { TPANoiseRemval }

 TPANoiseRemoval = class(TNoiseRemoval, IPAIODataIOInterface)
   procedure WriteDataIO(ASender: IPAIODataIOInterface; AData: PSingle; ASamples: Integer);
 end;

{ TPANoiseRemval }

procedure TPANoiseRemoval.WriteDataIO(ASender: IPAIODataIOInterface; AData: PSingle; ASamples: Integer);
begin
  Process(AData, ASamples, False);
end;

{ TPANoiseRemovalLink }

procedure TPANoiseRemovalLink.InitData;
var
  i: Integer;
begin
  if FInited then
    Exit;

  FInited := True;
  if Assigned(FHelper) then
    FreeAndNil(FHelper);
  FHelper := TPAIOChannelHelper.Create(Self);

  SetLength(FNoise, Channels);
  for i := 0 to Channels-1 do
  begin
    FNoise[i] := TPANoiseRemoval.Create;
    FHelper.Outputs.Add(FNoise[i] as IPAIODataIOInterface);
    //FNoise[i].Level:=30; //does nothing!
    //FNoise[i].Sensitivity:=20;//3.95;
    //FNoise[i].Sensitivity:=9.95;
   // FNoise[i].Gain:=-48;
   // FNoise[i].AttackDecayTime:=1000;
    FNoise[i].Init(SamplesPerSecond);
    FNoise[i].WriteProc:=@NoiseRemovalDone;
  end;

end;

procedure TPANoiseRemovalLink.NoiseRemovalDone(ASender: TObject; AData: PSingle; ASampleCount: Integer);
begin
  (FHelper as IPAIODataIOInterface).WriteDataIO(ASender as IPAIODataIOInterface, AData, ASampleCount);
end;

procedure TPANoiseRemovalLink.WriteDataIO(ASender: IPAIODataIOInterface; AData: PSingle; ASamples: Integer);
begin
  // write plexed output from FHelper to buffer.
  WriteToBuffer(AData^, ASamples*SizeOf(Single), FIsLast);
end;

function TPANoiseRemovalLink.InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64;
var
  i: Integer;
begin
  FHelper.Write(PSingle(@AData), ACount div SizeOf(Single));
  FIsLast:=AIsLastData;
  if FIsLast then
    for i := Low(FNoise) to High(FNoise) do
      FNoise[i].Flush;
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
  Format := afFloat32;
end;

destructor TPANoiseRemovalLink.Destroy;
begin
  FHelper.Free;
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

