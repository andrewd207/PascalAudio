{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_resample;

{$mode objfpc}{$H+}

interface

{$IFDEF UNIX}

uses
  Classes, SysUtils, unixtype, pa_base, resample;

type

  { TPAResampleLink }

  TPAResampleLink = class(TPAAudioLink, IPAAudioInformation)
  private
    FResampleHelper: TResampleHelper;
    FOutSamplesPerSecond: Integer;
    FInited: Boolean;
    SourceSamplesPS: Integer;
    procedure InitData;
    procedure FinishConvert;
  protected
    function  GetSamplesPerSecond: Integer; override;
    procedure SetSamplesPerSecond(AValue: Integer); override;
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; override;
    procedure SignalDestinationsDone; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  ctypes, pa_ringbuffer;

procedure TPAResampleLink.InitData;
begin
  FInited:=True;
  with (DataSource.GetSourceObject as IPAAudioInformation) do
  begin
    SourceSamplesPS:=SamplesPerSecond;
  end;
  FResampleHelper := TResampleHelper.Create(Channels);
  //WriteLn('done Init data');
end;

procedure TPAResampleLink.FinishConvert;
begin
  FreeAndNil(FResampleHelper);
end;

function TPAResampleLink.GetSamplesPerSecond: Integer;
begin
  Result:=FOutSamplesPerSecond;
end;

procedure TPAResampleLink.SetSamplesPerSecond(AValue: Integer);
begin
  FOutSamplesPerSecond:=AValue;
end;

function TPAResampleLink.InternalProcessData(const AData; ACount: Int64;
  AIsLastData: Boolean): Int64;
var
  i: Integer;
  InBufUsed: cint;
  ConvertedData: TSingleArray;
  ConvertedSize: Integer;
  Count: Integer;
  InPos: Integer;
  OutPos: Integer;
begin
  if not FInited then
    InitData;

  // check if sample is the same and just pass the data forward if it is. No need to process it
  if (SourceSamplesPS = FOutSamplesPerSecond)
  then
  begin
    Result := WriteToBuffer(AData, ACount, AIsLastData);
    Exit;
  end;

  {Helper:= TResampleHelper.Create(AData, ACount, Channels, FOutSamplesPerSecond / SourceSamplesPS);

  for i := 0 to High(Channels) do
  begin
    InPos := 0;
    OutPos := 0;
    repeat

      Count := resample_process(FResample,
                                FOutSamplesPerSecond / SourceSamplesPS,
                                @Helper.InBuffers[i][InPos],
                                Helper.InSamplesCount-InPos,
                                1,//1,//Ord(AIsLastData),//0, // IsLastData
                                @InBufUsed,
                                @Helper.OutBuffers[i][OutPos],
                                Helper.OutBufferLength - OutPos);
      Inc(InPos, InBufUsed);
      //WriteLn('InnerLoop InPos = ', InPos, ' InUsed = ' , InBufUsed);
      if Count > 0 then
        Inc(OutPos, Count);


    until (Count < 0) or ((Count = 0) and (InPos = Helper.InSamplesCount));
    Helper.OutBuffersLength[i] := OutPos;
    //WriteLn('Channel ', i, ' BufferOutCount = ',Helper.OutBuffersLength[i], ' Orig Length = ', Helper.InSamplesCount);
  end;

  ConvertedData := Helper.PlexOutBuffers(ConvertedSize);}

  ConvertedData := FResampleHelper.Write(PSingle(@AData), ACount div SizeOf(Single), FOutSamplesPerSecond / SourceSamplesPS, AIsLastData);


  Result := WriteToBuffer(ConvertedData[0], Length(ConvertedData)*SizeOf(Single), AIsLastData);
  //Result := WriteToBuffer(AData, ACount, AIsLastData);

  //FreeMem(ConvertedData);
  SetLength(ConvertedData, 0);
  //Helper.Free;

end;

procedure TPAResampleLink.SignalDestinationsDone;
begin
  inherited SignalDestinationsDone;
end;

constructor TPAResampleLink.Create;
begin
  inherited Create;
  FOutSamplesPerSecond:=44100;
  FFormat:=afFloat32;
end;

destructor TPAResampleLink.Destroy;
begin
  inherited Destroy;
end;

{ TStereoFloat }

{$ELSE}
implementation
{$ENDIF}

end.

