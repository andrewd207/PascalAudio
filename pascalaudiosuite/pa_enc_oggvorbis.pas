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
  ctypes, paio_log;

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
    procedure BeforeStreamFree; override;
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
  BaseQuality: cfloat;
begin
  if FInited then
    Exit;

  // vorbis_encode_setup_vbr wants a base_quality in -0.1..1.0. Quality is the
  // 0..1 quality the caller asked for, so pass it straight through (clamped).
  // The old code passed 1/Quality, which is out of range for any Quality < ~0.9
  // and is +Inf when Quality is left at its 0 default -- that leaves FInfo's
  // codec_setup half-built and later analysis reads garbage codebooks.
  BaseQuality := Quality;
  if BaseQuality < -0.1 then
    BaseQuality := -0.1
  else if BaseQuality > 1.0 then
    BaseQuality := 1.0;

  vorbis_info_init(FInfo);
  vorbis_encode_setup_vbr(FInfo, Channels, SamplesPerSecond, BaseQuality);
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
  TPALog.Info(ClassName, 'initialized');
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
  // tell vorbis there is no more input; the last block it hands back carries the
  // end-of-stream marker.
  vorbis_analysis_wrote(FDSPState, 0);
  // Drain through the SAME bitrate-managed path used while encoding
  // (addblock/flushpacket). Using the raw vorbis_analysis(vb,&op) path here
  // mixed the two APIs and left the final EOS packet stuck in the bitrate
  // manager, so the stream was written without an EOS page -- valid audio, but
  // tools that read the trailing granulepos (ffprobe's quick duration, seeking)
  // saw garbage/zero length.
  while vorbis_analysis_blockout(FDSPState, FBlock) = 1 do
  begin
    Tvorbis_analysisHack(@vorbis_analysis)(FBlock, nil);
    vorbis_bitrate_addblock(FBlock);
    while vorbis_bitrate_flushpacket(FDSPState, op) > 0 do
      ogg_stream_packetin(FOggStream, op);
  end;
  // Flush ogg data to Destinations (writes the EOS page since the last packetin
  // carried e_o_s).
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
  // Finish here, in the worker thread, when the source flags the last buffer.
  // FinishEncode drives libvorbis (vorbis_analysis/FBlock) and writes the ogg
  // pages; doing it from the EndOfData override instead ran it in the SOURCE's
  // thread concurrently with this method in the destination worker, so two
  // threads drove the same libvorbis/ogg state at once -> corrupt pages
  // ("page after EOS", sequence gaps) and intermittent crashes inside
  // vorbis_analysis.
  if AIsLastData then
    FinishEncode;
end;

procedure TPAOggVorbisEncoderLink.BeforeStreamFree;
begin
  // if the encode was interrupted (EndOfData never ran), flush the trailing ogg
  // pages + EOS and clear libvorbis state before the stream is freed, so the
  // output isn't truncated and the vorbis structures don't leak.
  if FInited then
    FinishEncode;
end;

procedure TPAOggVorbisEncoderLink.SetStream(AValue: TStream);
begin
  inherited SetStream(AValue);
  // serial number and comment block are set up once in Create (the base ctor
  // bypasses this override anyway); don't re-init here or a stream reassignment
  // would leak/clear the existing vorbis_comment.
  if Assigned(AValue) and (FSerialNumber = 0) then
    FSerialNumber:=1;
end;

constructor TPAOggVorbisEncoderLink.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create(AStream, AOwnsStream);
  FFormat:=afRaw;
  // TPAStreamDestination.Create assigns FStream directly and never calls the
  // virtual SetStream, so do the encoder's stream-dependent setup here: a valid
  // (non-zero) ogg serial number and an initialised comment block. Without this
  // every stream was written with serial 0 and AddComment touched an
  // uninitialised vorbis_comment.
  FSerialNumber:=1;
  vorbis_comment_init(FComment);
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

