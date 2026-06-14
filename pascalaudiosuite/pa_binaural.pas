{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_binaural;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pa_base, bs2b, paio_log;

type
  { TPABinauralLink }

  TPABinauralLink = class(TPAAudioLink, IPAAudioInformation)
  private
    Fbs2bLevel: LongWord;
    Fbs2bLevelCutFreq: LongWord;
    Fbs2bLevelFeed: LongWord;
    FInstance: Pbs2bd;
    FInited: Boolean;
    procedure InitData;
  protected
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; override;
    procedure SignalDestinationsDone; override;
    function GetFormat: TPAAudioFormat; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Level: LongWord read Fbs2bLevel write Fbs2bLevel;
    property LevelCutFreq: LongWord read Fbs2bLevelCutFreq write Fbs2bLevelCutFreq;
    property LeveLFeed: LongWord read Fbs2bLevelFeed write Fbs2bLevelFeed;
  end;

implementation

{ TPABinauralLink }

procedure TPABinauralLink.InitData;
begin
  FInited:=True; // otherwise InitData re-opens a new bs2bd every buffer (leak)
  TPALog.Info(ClassName, 'initialized');
  FInstance:=Tbs2bd.Open;
  FInstance^.SampleRate:=SamplesPerSecond;
  FInstance^.Level:=Level;
  FInstance^.LevelCutFreq:=LevelCutFreq;
  FInstance^.LevelFeed:=LeveLFeed;
end;

function TPABinauralLink.InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64;
var
  Data: PSingle;
  Samples: Integer;
  B: PAudioBuffer;
begin
  if not FInited then
    InitData;

  // frames = bytes / bytes-per-float / 2 channels. The operands were inverted
  // (SizeOf(Single) div ACount div 2), which is 0 for any real buffer, so
  // CrossFeed_f processed nothing and the effect was a silent no-op.
  Samples := ACount div SizeOf(Single) div 2;

  B := BufferPool.GetBufferFromPool(True);

  B^.Format:=Format;
  Move(AData, B^.Data, ACount);
  B^.UsedData:=ACount;
  B^.IsEndOfData:=AIsLastData;



  FInstance^.CrossFeed_f(@B^.Data, Samples);
  WriteToDestinations(B);//}
  {

  FInstance^.CrossFeed_f(PSingle(@Data), Samples);
  WriteToBuffer(Data, ACount, AIsLastData);//}



  Result := ACount;

  if AIsLastData then FInstance^.Clear;
end;

procedure TPABinauralLink.SignalDestinationsDone;
begin
  inherited SignalDestinationsDone;
end;

function TPABinauralLink.GetFormat: TPAAudioFormat;
begin
  Result := afFloat32;
end;

constructor TPABinauralLink.Create;
begin
  inherited Create;
end;

destructor TPABinauralLink.Destroy;
begin
  // stop the worker thread (it uses FInstance in InternalProcessData) before
  // closing the bs2b instance.
  DestroyWaitSync;
  if FInited then
    FInstance^.Close;
  inherited Destroy;
end;

end.

