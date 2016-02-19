{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    libsamplerate upon which this depends is GPL

}
unit pa_samplerate;

{$mode objfpc}{$H+}
{$packrecords c}

interface

uses
  Classes, SysUtils, pa_base, samplerate;


type

  { TPASampleRateLink }

  TPASampleRateLink = class(TPAAudioLink)
  private
    FSrc: PSRC_STATE;
    FOutSamplesPerSecond: Integer;
    FOutChannels: Integer;
    FInited: Boolean;
    procedure InitData;
    procedure FinishConvert;
  protected
    function  GetSamplesPerSecond: Integer; override;
    procedure SetSamplesPerSecond(AValue: Integer); override;
    function  GetChannels: Integer; override;
    procedure SetChannels(AValue: Integer); override;
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; override;
    procedure SignalDestinationsDone; override;
  public
    constructor Create; override;
    destructor Destroy; override;

  end;

implementation
uses
  ctypes, math;






{ TPASampleRateLink }

procedure TPASampleRateLink.InitData;
var
  error: cint;
begin
  //WriteLn('Init data');
  FInited:=True;
  FSrc:= src_new(SRC_SINC_FASTEST, FOutChannels, @error);
  if error <> 0 then
    WriteLn('ProcessResult: ',src_strerror(error));
  src_set_ratio(FSrc, SamplesPerSecond / (DataSource.GetSourceObject as IPAAudioInformation).SamplesPerSecond);
  //WriteLn(src_is_valid_ratio(SamplesPerSecond / (DataSource.GetSourceObject as IPAAudioInformation).SamplesPerSecond));
  //WriteLn('done Init data');
end;

procedure TPASampleRateLink.FinishConvert;
begin
  src_delete(FSrc);
  FInited:=False;
end;

function TPASampleRateLink.GetSamplesPerSecond: Integer;
begin
  Result:=FOutSamplesPerSecond;
end;

procedure TPASampleRateLink.SetSamplesPerSecond(AValue: Integer);
begin
  FOutSamplesPerSecond:=AValue;
end;

function TPASampleRateLink.GetChannels: Integer;
begin
  Result := FOutChannels;
end;

procedure TPASampleRateLink.SetChannels(AValue: Integer);
begin
  FOutChannels:=AValue;
end;




//function StereoShortToFloatArrays


function TPASampleRateLink.InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64;
var
  sdata: SRC_DATA;
  Converted: Pointer;
  ConvertedToFloat: pcfloat absolute Converted;
  ConvertedToShort: pcshort absolute Converted;
  OutData: pointer;
  OutDataSize: Integer;
  Ratio: Single;
  SourceSamplesPS: Integer;
  SourceFormat: TPAAudioFormat;
  SourceChannels: Integer;
  res: cint;
begin
   WriteLn('samplerate process');
  if not FInited then
    InitData;

  with (DataSource.GetSourceObject as IPAAudioInformation) do
  begin
    SourceSamplesPS:=SamplesPerSecond;
    SourceFormat:=Format;
    SourceChannels:=Channels;
  end;

  sdata.src_ratio:=SamplesPerSecond / SourceSamplesPS;

  //Converted := Getmem(Max(ACount, Trunc(ACount * 4{sdata.src_ratio})+1)+4);

  // alloc enough memory for before and after frames. whichever is greater
  OutDataSize := Max(ACount, Trunc(ACount * sdata.src_ratio))+100;
  OutDataSize := ACount * 4 + 4;
  WriteLn('DataSize : ', OutDataSize, ' : ', ACount * 4 + 4);
  OutData := GetMem(OutDataSize);

  //src_short_to_float_array(pcshort(@AData), ConvertedToFloat, ACount div BytesPerSample(Format));
  //ConvertShortIntsToFloat(PShortInt(@AData), ConvertedToFloat, ACount div BytesPerSample);

//  sdata.data_in:=ConvertedToFloat;
  sdata.data_in:=@AData;
  sdata.data_out:=pcfloat(OutData);
  sdata.input_frames:=ACount div BytesPerSample(Format) div SourceChannels;

  //sdata.input_frames:=(ACount div ((SourceSamplesPS div SourceBytesPerSample) div SourceChannels));
  //sdata.output_frames:= OutDataSize div 2 div SourceChannels;
  sdata.output_frames:= OutDataSize div 2 div SourceChannels;
  sdata.end_of_input:=0;//ord(FDataIsEnded and FBufferManager.Empty);

{  WriteLn('input frames avail: ', sdata.input_frames);

  WriteLn('Processing Data:', ACount);}

  res:=src_process(FSrc, @sdata);
  if Res <> 0 then
    WriteLn('ProcessResult: ',src_strerror(res));
 {
  WriteLn('input frames avail: ', sdata.input_frames);
  WriteLn('input frames used: ', sdata.input_frames_used);
  WriteLn('output frames generated: ', sdata.output_frames_gen);
  }

  //src_float_to_short_array(sdata.data_out, ConvertedToShort, sdata.output_frames_gen * FOutChannels);
  //WriteToBuffer(ConvertedToShort^, sdata.output_frames_gen * FOutChannels * FOutBytesPerSample, AIsLastData);

  WriteToBuffer(sdata.data_out^, sdata.output_frames_gen * FOutChannels * BytesPerSample(afFloat32), AIsLastData);

  //Freemem(Converted);
  FreeMem(OutData);

end;

procedure TPASampleRateLink.SignalDestinationsDone;
begin
  inherited SignalDestinationsDone;
end;

constructor TPASampleRateLink.Create;
begin
  inherited Create;
  FOutSamplesPerSecond:=44100;
  FFormat:=afFloat32;
end;

destructor TPASampleRateLink.Destroy;
begin
  inherited Destroy;
end;



end.

