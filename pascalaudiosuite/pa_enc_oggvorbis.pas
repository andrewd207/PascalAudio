{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_enc_oggvorbis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pa_base,
  pa_stream,
  pa_register,
  ogg, vorbis,
  ctypes;

type

  { TPAOggVorbisEncoderLink }

  TPAOggVorbisEncoderLink = class(TPAStreamDestination)
  private
    FBigEndian: Boolean;
    FOggStream: ogg_stream_state;
    FInfo: vorbis_info;
    FDSPState: vorbis_dsp_state;
    FComment: vorbis_comment;
    FBlock: vorbis_block;
    FInited: Boolean;
    FQuality: cfloat;
    FSerialNumber: cint;
    FWrittenBytes: Qword;
    procedure InitEncoder;
    procedure WritePage(ForceFlush: Boolean);
    procedure FinishEncode;
  protected
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; override;
    procedure EndOfData; override;
    procedure SetStream(AValue: TStream); override;

  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean); override;
    function   GetWrittenSeconds: QWord;
    procedure  AddComment(TagName: Utf8String; Content: Utf8String);
    property   Quality: cfloat read FQuality write FQuality;
    property   SerialNumber: cint read FSerialNumber write FSerialNumber;
    property   BigEndian: Boolean read FBigEndian write FBigEndian;
  end;

implementation

{ TPAOggVorbisEncoderLink }

procedure TPAOggVorbisEncoderLink.InitEncoder;
var
  Pkt: ogg_packet;
  PktComment: ogg_packet;
  PktCode: ogg_packet;
begin
  if FInited then
    Exit;

  vorbis_info_init(FInfo);
  vorbis_encode_setup_vbr(FInfo, Channels, SamplesPerSecond, 1 / Quality);
  vorbis_encode_setup_init(FInfo);
  vorbis_analysis_init(FDSPState, FInfo);
  vorbis_block_init(FDSPState, FBlock);
  ogg_stream_init(FOggStream, SerialNumber);

  vorbis_analysis_headerout(FDSPState, FComment, Pkt, PktComment, PktCode);
  ogg_stream_packetin(FOggStream, Pkt);
  ogg_stream_packetin(FOggStream, PktComment);
  ogg_stream_packetin(FOggStream, PktCode);

  WritePage(True);
  FInited:=True;
end;

procedure TPAOggVorbisEncoderLink.WritePage(ForceFlush: Boolean);
var
  Page: ogg_page;
  w: Boolean;
begin
  repeat
    if ForceFlush then
      w := ogg_stream_flush(FOggStream, Page) <> 0
    else
      w := ogg_stream_pageout(FOggStream, Page) <> 0;
    if w then
    begin
      //WriteLn('ogg writing to destinations');
      //WriteToBuffer(Page.header^,Page.header_len, False);
      //WriteToBuffer(Page.body^,Page.body_len, False);
      FStream.Write(Page.header^,Page.header_len);
      FStream.Write(Page.body^,Page.body_len);
      //WriteLn('ogg wrote to destinations');
    end
    else
      break;
  until False;
end;

type
  Tvorbis_analysisHack = function(var block: vorbis_block; op: pOgg_packet): cint; cdecl;


procedure TPAOggVorbisEncoderLink.FinishEncode;
var
  op: ogg_packet;
begin
  //WriteLn('Finishing encode');
  vorbis_analysis_wrote(FDSPState, 0);
  while vorbis_analysis_blockout(FDSPState, FBlock) = 1 do
  begin
    if Tvorbis_analysisHack(@vorbis_analysis)(FBlock, @op) = 0 then
      ogg_stream_packetin(FOggStream, op);
  end;
  // Flush ogg data to Destinations
  WritePage(True);

  ogg_stream_clear(FOggStream);
  vorbis_block_clear(FBlock);
  vorbis_dsp_clear(FDSPState);
  vorbis_info_clear(FInfo);
  vorbis_comment_clear(FComment);

  FInited:=False;
end;

function TPAOggVorbisEncoderLink.InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64;
var
  buffer: ppcfloat;
  i: Integer;
  j: Integer;
  samples: Integer;
  op: ogg_packet;
  res: Integer;
begin
  if not FInited then
    InitEncoder;
  //WriteLn('ogg process data. Ended=',FDataIsEnded);
  // move to end;
  Result := ACount;

  Inc(FWrittenBytes, ACount);

  Samples := ACount div (BytesPerSample(afFloat32) * Channels);
  if samples > 0 then
  begin
    // split the channels into contigous data from interleaved.
    buffer := vorbis_analysis_buffer(FDSPState, samples);
    for j := 0 to Channels-1 do
      for i := 0 to samples-1 do
        buffer[j][i] := PSingle(@AData)[i*channels+j];

    vorbis_analysis_wrote(FDSPState, samples);

    while vorbis_analysis_blockout(FDSPState, FBlock) = 1 do
    begin
      Tvorbis_analysisHack(@vorbis_analysis)(FBlock, nil);
      vorbis_bitrate_addblock(FBlock);
      while vorbis_bitrate_flushpacket(FDSPState, op) > 0 do
        ogg_stream_packetin(FOggStream, op);
    end;

  end;
  WritePage(False or AIsLastData);
  {if AIsLastData then
    FinishEncode;}
end;

procedure TPAOggVorbisEncoderLink.EndOfData;
begin
  FinishEncode;
  inherited EndOfData;
end;

procedure TPAOggVorbisEncoderLink.SetStream(AValue: TStream);
begin
  inherited SetStream(AValue);
  if Assigned(AValue) then
  begin
    FSerialNumber:=1;
    vorbis_comment_init(FComment);
  end;
end;

constructor TPAOggVorbisEncoderLink.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create(AStream, AOwnsStream);
  FFormat:=afRaw;
end;

function TPAOggVorbisEncoderLink.GetWrittenSeconds: QWord;
begin
  if FWrittenBytes = 0 then
    Exit(0);
  Result := FWrittenBytes div BytesPerSample(DefaultAudioFormat) div Channels div SamplesPerSecond;
end;

procedure TPAOggVorbisEncoderLink.AddComment(TagName: Utf8String; Content: Utf8String);
begin
  vorbis_comment_add_tag(FComment,PChar(TagName),PChar(Content));
end;

initialization
  PARegister(partEncoder, TPAOggVorbisEncoderLink, 'OGG/Vorbis', '.ogg', 'OggS');
end.

