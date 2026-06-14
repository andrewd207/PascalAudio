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
  Classes, SysUtils, pa_base, samplerate, paio_log;


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
  // libsamplerate resamples but never remaps channels: input and output must
  // have the same channel count. Adopt the source's channel count so the
  // converter, the frame math below, and the channel count we report
  // downstream all agree (otherwise frame counts are miscomputed and
  // src_process reports "arrays overlap").
  FOutChannels := (DataSource.GetSourceObject as IPAAudioInformation).Channels;
  FSrc:= src_new(SRC_SINC_FASTEST, FOutChannels, @error);
  if error <> 0 then
    TPALog.Error('TPASampleRateLink', 'src_new failed: ' + src_strerror(error));
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
  OutData: pointer;
  OutDataSize: Integer;
  InFrames: Integer;
  OutFrames: Integer;
  SourceSamplesPS: Integer;
  res: cint;
begin
  //WriteLn('samplerate process');
  if not FInited then
    InitData;

  SourceSamplesPS := (DataSource.GetSourceObject as IPAAudioInformation).SamplesPerSecond;

  sdata.src_ratio:=SamplesPerSecond / SourceSamplesPS;

  // Input is float32 (the base converts to Format=afFloat32 before calling us).
  InFrames := ACount div BytesPerSample(afFloat32) div FOutChannels;

  // Output buffer must hold input_frames * ratio frames; round up and add a
  // little slack for the converter's edge handling. Size the allocation in
  // floats (4 bytes), and tell libsamplerate the true frame capacity -- the
  // old code sized output_frames as bytes/2 (a 16-bit-sample assumption),
  // claiming twice the real capacity and overflowing the heap buffer.
  OutFrames := Trunc(InFrames * sdata.src_ratio) + 16;
  OutDataSize := OutFrames * FOutChannels * SizeOf(cfloat);
  OutData := GetMem(OutDataSize);

  sdata.data_in:=@AData;
  sdata.data_out:=pcfloat(OutData);
  sdata.input_frames:=InFrames;
  sdata.output_frames:=OutFrames;
  sdata.end_of_input:=0;//ord(FDataIsEnded and FBufferManager.Empty);

  res:=src_process(FSrc, @sdata);
  if Res <> 0 then
    TPALog.Error('TPASampleRateLink', 'src_process failed: ' + src_strerror(res));

  WriteToBuffer(sdata.data_out^, sdata.output_frames_gen * FOutChannels * SizeOf(cfloat), AIsLastData);

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
  FOutChannels:=2;
  FFormat:=afFloat32;
end;

destructor TPASampleRateLink.Destroy;
begin
  // stop the worker thread (it uses FSrc in InternalProcessData) before freeing
  // the libsamplerate state; FinishConvert (src_delete) was otherwise never
  // called, leaking the converter and its internal buffers.
  DestroyWaitSync;
  if FInited then
    FinishConvert;
  inherited Destroy;
end;



end.

