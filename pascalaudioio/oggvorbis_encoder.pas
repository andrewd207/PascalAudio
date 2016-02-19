unit oggvorbis_encoder;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vorbis{, ogg, pa_base};
implementation
(*

type

  { TOggVorbisEncoder }

  TOggVorbisEncoder = class(IAudioDestination)
    FOutStream: TStream;
    oggstream: ogg_stream_state;
    info: vorbis_info;
    dsp_state: vorbis_dsp_state;
    comment: vorbis_comment;
    block: vorbis_block;
    FInited: Boolean;
    FEncoding: Boolean;
    BitsPerSample: Integer;
    procedure Init;
    procedure WritePage(ForceFlush: Boolean);
    { begin IAudioTransformation methods }
    function  GetSamplesPerSecond: Integer;
    procedure SetSamplesPerSecond(AValue: Integer);
    function  GetBytesPerSample: Integer;
    procedure SetBytesPerSample(AValue: Integer);
    function  GetChannels: Integer;
    procedure SetChannels(AValue: Integer);
    function  GetDataSource: IAudioTransformation;
    procedure SetDataSource(AValue: IAudioTransformation);
    function  GetDataTarget: IAudioTransformation;
    procedure SetDataTarget(AValue: IAudioTransformation);

    function  ReadData(var Data; ACount: QWord): QWord;
    function  WriteData(var Data; ACount: QWord): QWord;
    { end IAudioTransformation methods }
  public
    constructor Create(AOutStream: TStream; Channels: Integer; SamplesPerSecond: Integer; Quality: Single);
    procedure AddComment(TagName: Utf8String; Content: Utf8String);

    procedure EncodeData(data: PByte; Length: Integer; bigendian: Boolean);
    procedure FinishEncode;
    procedure Flush;
  end;

implementation
uses ctypes;

{ TOggVorbisEncoder }
function  ogg_stream_check(var os: ogg_stream_state): cint; cdecl; external {$IFDEF DYNLINK}ogglib{$ENDIF};
constructor TOggVorbisEncoder.Create(AOutStream: TStream; Channels: Integer;
  SamplesPerSecond: Integer; Quality: Single);
begin
  FOutStream := AOutStream;

  vorbis_info_init(info);
  vorbis_encode_setup_vbr(info, Channels, SamplesPerSecond, Quality);
  vorbis_encode_setup_init(info);
  vorbis_analysis_init(dsp_state, info);
  vorbis_block_init(dsp_state, block);
  vorbis_comment_init(comment);
  ogg_stream_init(oggstream, 0);

  BitsPerSample := 16;

end;

procedure TOggVorbisEncoder.AddComment(TagName: Utf8String; Content: Utf8String);
begin
  IF FEncoding then
    Raise Exception.Create('dude you can''t add comments one you''ve started encoding');
  vorbis_comment_add_tag(comment,PChar(TagName),PChar(Content));
end;

procedure TOggVorbisEncoder.Init;
var
  op: ogg_packet;
  op_comm: ogg_packet;
  op_code: ogg_packet;
begin
  if FInited then
    Exit;

  vorbis_analysis_headerout(dsp_state, comment, op, op_comm, op_code);
  ogg_stream_packetin(oggstream, op);
  ogg_stream_packetin(oggstream, op_comm);
  ogg_stream_packetin(oggstream, op_code);

  WritePage(True);
  FInited:=True;
end;

procedure TOggVorbisEncoder.WritePage(ForceFlush: Boolean);
var
  page: ogg_page;
  w: Boolean;
begin
  repeat
    if ForceFlush then
      w := ogg_stream_flush(oggstream, page) <> 0
    else
      w := ogg_stream_pageout(oggstream, page) <> 0;
    if w then
    begin
      FOutStream.Write(page.header^,page.header_len);
      FOutStream.Write(page.body^,page.body_len);
    end
    else
      break;
  until False;
end;

function TOggVorbisEncoder.GetSamplesPerSecond: Integer;
begin
  Result := info.rate;
end;

procedure TOggVorbisEncoder.SetSamplesPerSecond(AValue: Integer);
begin
  /// done with create. change? will have to delay ogg setup
end;

function TOggVorbisEncoder.GetBytesPerSample: Integer;
begin
  Result := BitsPerSample div 8;
end;

procedure TOggVorbisEncoder.SetBytesPerSample(AValue: Integer);
begin
  BitsPerSample:= AValue * 8;
end;

function TOggVorbisEncoder.GetChannels: Integer;
begin
  Result := info.channels;
end;

procedure TOggVorbisEncoder.SetChannels(AValue: Integer);
begin
  /// done with create. change? will have to delay ogg setup
end;

function TOggVorbisEncoder.GetDataSource: IAudioTransformation;
begin

end;

procedure TOggVorbisEncoder.SetDataSource(AValue: IAudioTransformation);
begin

end;

function TOggVorbisEncoder.GetDataTarget: IAudioTransformation;
begin

end;

procedure TOggVorbisEncoder.SetDataTarget(AValue: IAudioTransformation);
begin

end;

function TOggVorbisEncoder.ReadData(var Data; ACount: QWord): QWord;
begin

end;

function TOggVorbisEncoder.WriteData(var Data; ACount: QWord): QWord;
begin

end;

//function  vorbis_analysis(var vb: vorbis_block; op: Pogg_packet): cint; cdecl; external;
type
  TProcHack = function(var block: vorbis_block; op: pOgg_packet): cint; cdecl;

procedure TOggVorbisEncoder.EncodeData(data: PByte; Length: Integer; bigendian: Boolean);
var
  buffer: ppcfloat;
  i: Integer;
  j: Integer;
  samples: Integer;
  channels: Integer;
  op: ogg_packet;
  res: Integer;
begin
  if not FInited then
    Init;
  FEncoding := True;
  channels:= info.channels;
  Samples := Length div ((BitsPerSample div 8) * channels);
  if samples = 0 then
    exit;
  buffer := vorbis_analysis_buffer(dsp_state, samples);
  if bigendian then
  begin
    for i := 0 to  samples-1 do
    begin
      for j := 0 to channels-1 do
      begin
        case BitsPerSample of
          16: buffer[j][i] := cfloat(BEtoN((data)[i*channels+j])) / 32768;
           8: buffer[j][i] := cfloat(BEtoN((data)[i*channels+j])) / $8000;//32768;
        else
          writeln('unsupported bps');
        end;
        //writeln(buffer[j][i]);
        //buffer[j][i]:=((data[2*(i*channels + j) + 1] shl 8) or ($00ff and cint(data[2*(i*channels + j)])))/32768.0;
      end;
    end;
  end
  else
  begin
    for i := 0 to  samples-1 do
    begin
      for j := 0 to channels-1 do
      begin
        case BitsPerSample of
          16: buffer[j][i] := cfloat(LEtoN(PSmallInt(data)[i*channels+j])) / 32768;
           8: buffer[j][i] := cfloat(LEtoN(PShortInt(data)[i*channels+j])) / $8000;//32768;
        else
          writeln('unsupported bps');
        end;
      end;
    end;
  end;

  vorbis_analysis_wrote(dsp_state, samples);

  while vorbis_analysis_blockout(dsp_state, block) = 1 do
  begin
    TProcHack(@vorbis_analysis)(block, nil);
    vorbis_bitrate_addblock(block);
    while vorbis_bitrate_flushpacket(dsp_state, op) > 0 do
      ogg_stream_packetin(oggstream, op);
  end;

  WritePage(False);

end;

procedure TOggVorbisEncoder.Flush;
begin
  WritePage(True);
end;

procedure TOggVorbisEncoder.FinishEncode;
var
  op: ogg_packet;
begin
  vorbis_analysis_wrote(dsp_state, 0);
  while vorbis_analysis_blockout(dsp_state, block) = 1 do
  begin
    if TProcHack(@vorbis_analysis)(block, @op) = 0 then
      ogg_stream_packetin(oggstream, op);
  end;
  Flush;
  ogg_stream_clear(oggstream);
  vorbis_block_clear(block);
  vorbis_dsp_clear(dsp_state);
  vorbis_info_clear(info);
  vorbis_comment_clear(comment);
end;
*)
end.

