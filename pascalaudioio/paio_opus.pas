{*
   Copyright (c) 2010-2011 Xiph.Org Foundation, Skype Limited
   Written by Jean-Marc Valin and Koen Vos */
*}

{*
   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
   - Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
   - Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*}
{
    This unit is part of Pascal Audio IO package.

    Binding created by Andrew Haines.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit paio_opus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, dynlibs, fgl;

const
  OPUS_OK               = 0;
  OPUS_BAD_ARG          =-1;
  OPUS_BUFFER_TOO_SMALL =-2;
  OPUS_INTERNAL_ERROR   =-3;
  OPUS_INVALID_PACKET   =-4;
  OPUS_UNIMPLEMENTED    =-5;
  OPUS_INVALID_STATE    =-6;
  OPUS_ALLOC_FAIL       =-7;

  OPUS_SET_APPLICATION_REQUEST       = 4000;
  OPUS_GET_APPLICATION_REQUEST       = 4001;
  OPUS_SET_BITRATE_REQUEST           = 4002;
  OPUS_GET_BITRATE_REQUEST           = 4003;
  OPUS_SET_MAX_BANDWIDTH_REQUEST     = 4004;
  OPUS_GET_MAX_BANDWIDTH_REQUEST     = 4005;
  OPUS_SET_VBR_REQUEST               = 4006;
  OPUS_GET_VBR_REQUEST               = 4007;
  OPUS_SET_BANDWIDTH_REQUEST         = 4008;
  OPUS_GET_BANDWIDTH_REQUEST         = 4009;
  OPUS_SET_COMPLEXITY_REQUEST        = 4010;
  OPUS_GET_COMPLEXITY_REQUEST        = 4011;
  OPUS_SET_INBAND_FEC_REQUEST        = 4012;
  OPUS_GET_INBAND_FEC_REQUEST        = 4013;
  OPUS_SET_PACKET_LOSS_PERC_REQUEST  = 4014;
  OPUS_GET_PACKET_LOSS_PERC_REQUEST  = 4015;
  OPUS_SET_DTX_REQUEST               = 4016;
  OPUS_GET_DTX_REQUEST               = 4017;
  OPUS_SET_VBR_CONSTRAINT_REQUEST    = 4020;
  OPUS_GET_VBR_CONSTRAINT_REQUEST    = 4021;
  OPUS_SET_FORCE_CHANNELS_REQUEST    = 4022;
  OPUS_GET_FORCE_CHANNELS_REQUEST    = 4023;
  OPUS_SET_SIGNAL_REQUEST            = 4024;
  OPUS_GET_SIGNAL_REQUEST            = 4025;
  OPUS_GET_LOOKAHEAD_REQUEST         = 4027;
  OPUS_RESET_STATE= 4028;
  OPUS_GET_SAMPLERATE_REQUEST        = 4029;
  OPUS_GET_FINAL_RANGE_REQUEST       = 4031;
  OPUS_GET_PITCH_REQUEST             = 4033;
  OPUS_SET_GAIN_REQUEST              = 4034;
  OPUS_GET_GAIN_REQUEST              = 4045; // Should have been 4035
  OPUS_SET_LSB_DEPTH_REQUEST         = 4036;
  OPUS_GET_LSB_DEPTH_REQUEST         = 4037;
  OPUS_GET_LAST_PACKET_DURATION_REQUEST     = 4039;
  OPUS_SET_EXPERT_FRAME_DURATION_REQUEST    = 4040;
  OPUS_GET_EXPERT_FRAME_DURATION_REQUEST    = 4041;
  OPUS_SET_PREDICTION_DISABLED_REQUEST      = 4042;
  OPUS_GET_PREDICTION_DISABLED_REQUEST      = 4043;
  // Don't use 4045, it's already taken by OPUS_GET_GAIN_REQUEST
  OPUS_SET_PHASE_INVERSION_DISABLED_REQUEST = 4046;
  OPUS_GET_PHASE_INVERSION_DISABLED_REQUEST = 4047;
  OPUS_GET_IN_DTX_REQUEST                   = 4049;



  // Values for the various encoder CTLs
  OPUS_AUTO                            =-1000;
  OPUS_BITRATE_MAX                     =  -1;
  OPUS_APPLICATION_VOIP                =2048;
  OPUS_APPLICATION_AUDIO               =2049;
  OPUS_APPLICATION_RESTRICTED_LOWDELAY =2051;
  OPUS_SIGNAL_VOICE                    =3001;
  OPUS_SIGNAL_MUSIC                    =3002;
  OPUS_BANDWIDTH_NARROWBAND            =1101;
  OPUS_BANDWIDTH_MEDIUMBAND            =1102;
  OPUS_BANDWIDTH_WIDEBAND              =1103;
  OPUS_BANDWIDTH_SUPERWIDEBAND         =1104;
  OPUS_BANDWIDTH_FULLBAND              =1105;

  OPUS_LIB = 'libopus.so.0';

type

  TOpusApplicationMode = (oamVOIP = OPUS_APPLICATION_VOIP,
                          oamAUDIO= OPUS_APPLICATION_AUDIO,
                          oamRestrictedLowDelay = OPUS_APPLICATION_RESTRICTED_LOWDELAY);

  TOpusBandwidth = (obAuto = OPUS_AUTO,
                    obNarrowBand = OPUS_BANDWIDTH_NARROWBAND,
                    obMediumBand = OPUS_BANDWIDTH_MEDIUMBAND,
                    obWideBand = OPUS_BANDWIDTH_WIDEBAND,
                    obSuperWideBand = OPUS_BANDWIDTH_SUPERWIDEBAND,
                    obFullBand = OPUS_BANDWIDTH_FULLBAND);

  TOpusForceChannels = (ofcAuto = OPUS_AUTO,
                        ofcForcedMono = 1,
                        ofcForcedStereo = 2);

  { TOpusBaseCtl }

  TOpusBaseCtl = class
  private
    FLastError: Integer;
  protected
    FOpus: Pointer; // encoder or decoder
    ctlfunc: function(obj: Pointer; request: cint): cint; varargs; cdecl;
    function GetBandwidth(AIndex: Integer): TOpusBandwidth;
    function GetCtlBool(AIndex: Integer): Boolean;
    function GetCtlInt(AIndex: Integer): Integer;
    procedure SetBandwidth(AIndex: Integer; AValue: TOpusBandwidth);
    procedure SetCtlBool(AIndex: Integer; AValue: Boolean);
    procedure SetCtlInt(AIndex: Integer; AValue: Integer);
  public
    constructor Create; // this base class is never publicly constructed
    procedure ResetState;
    property LastError: Integer read FLastError;
    // common ctl's
    property FinalRange: Integer index OPUS_GET_FINAL_RANGE_REQUEST read GetCtlInt;
    property Bandwidth: TOpusBandwidth index OPUS_GET_BANDWIDTH_REQUEST read GetBandwidth write SetBandwidth;
    property Samplerate: Integer index OPUS_GET_SAMPLERATE_REQUEST read GetCtlInt;
    property PhaseInversionDisabled: Boolean index OPUS_GET_PHASE_INVERSION_DISABLED_REQUEST read GetCtlBool write SetCtlBool;
    property InDTX: Boolean index OPUS_GET_LOOKAHEAD_REQUEST read GetCtlBool;
  end;

  { TOpusEncoder }



  TOpusEncoder = class(TOpusBaseCtl)
  private
    function GetApplication(AIndex: Integer): TOpusApplicationMode;
    function GetForceChannels(AIndex: Integer): TOpusForceChannels;
    procedure SetApplication(AIndex: Integer; AValue: TOpusApplicationMode);
    procedure SetForceChannels(AIndex: Integer; AValue: TOpusForceChannels);
  public
    class function GetSize(AChannels: Integer): Integer;
    // main functions
    constructor Create(ASampleRate: Integer; AChannels: Integer; AMode: TOpusApplicationMode; out Error: Integer);
    procedure Init(ASampleRate: Integer; AChannels: Integer; AMode: TOpusApplicationMode);
    function Encode(APCMData: pcint16; AFrameSize: Integer; AData: Pointer; AMaxDataBytes: Integer; out AEncodedSize: Integer): Boolean;
    function EncodeFloat(APCMData: pcfloat; AFrameSize: Integer; AData: Pointer; AMaxDataBytes: Integer; out AEncodedSize: Integer): Boolean;

    // ctl's
    property Complexity: Integer index OPUS_GET_COMPLEXITY_REQUEST read GetCtlInt write SetCtlInt;
    property Bitrate: Integer index OPUS_GET_BITRATE_REQUEST read GetCtlInt write SetCtlInt;
    property VBR: Boolean index OPUS_GET_VBR_REQUEST read GetCtlBool write SetCtlBool;
    property VBRConstraint: Boolean index OPUS_GET_VBR_CONSTRAINT_REQUEST read GetCtlBool write SetCtlBool;
    property ForceChannels: TOpusForceChannels index OPUS_GET_FORCE_CHANNELS_REQUEST read GetForceChannels write SetForceChannels;
    property MaxBandwidth: Integer index OPUS_GET_MAX_BANDWIDTH_REQUEST read GetCtlInt write SetCtlInt;
    property Signal: Integer index OPUS_GET_SIGNAL_REQUEST read GetCtlInt write SetCtlInt;
    property Application: TOpusApplicationMode index OPUS_GET_APPLICATION_REQUEST read GetApplication write SetApplication;
    property Lookahead: Integer index OPUS_GET_LOOKAHEAD_REQUEST read GetCtlInt;
    property InbandFEC: Boolean index OPUS_GET_INBAND_FEC_REQUEST read GetCtlBool write SetCtlBool;
    property PacketLossPercent: Integer index OPUS_GET_PACKET_LOSS_PERC_REQUEST read GetCtlInt write SetCtlInt;
    property DTX: Boolean index OPUS_GET_DTX_REQUEST read GetCtlBool write SetCtlBool;
    property LSBDepth: Integer index OPUS_GET_LOOKAHEAD_REQUEST read GetCtlInt write SetCtlInt;
    property ExpertFrameDuration: Integer index OPUS_GET_EXPERT_FRAME_DURATION_REQUEST read GetCtlInt write SetCtlInt;
    property PredictionDisabled: Integer index OPUS_GET_PREDICTION_DISABLED_REQUEST read GetCtlInt write SetCtlInt;
  end;

  { TOpusDecoder }

  TOpusDecoder = class(TOpusBaseCtl)
  public
    class function GetSize(AChannels: Integer): Integer;
    class function PacketGetNumFrames(APacket: Pointer; ALength: Integer): Integer;
    class function PacketGetSamplesPerFrame(APacket: Pointer; ASampleRate: Integer): Integer;
    constructor Create(ASamplerate: Integer; AChannels: Integer; out AError: Integer);
    destructor Destroy; override;
    function Init(ASampleRate: Integer; AChannels: Integer): Boolean;
    function Decode     (AData: Pointer; ALength: Integer; APCMOut: pcint16; AFrameSize: Integer; ADecodeForwardErrorCorrection: Boolean): Boolean;
    function DecodeFloat(AData: Pointer; ALength: Integer; APCMOut: pcfloat; AFrameSize: Integer; ADecodeForwardErrorCorrection: Boolean): Boolean;
    property LastError: Integer read FLastError;

    property Gain: Integer index OPUS_GET_GAIN_REQUEST read GetCtlInt write SetCtlInt;
    property LastPacketDuration: Integer index OPUS_GET_LAST_PACKET_DURATION_REQUEST read GetCtlInt;
    property Pitch: Integer index OPUS_GET_PITCH_REQUEST read GetCtlInt;
  end;

  { TOpusLib }

  TOpusLib = class
  private type
    P_OpusEncoder = ^T_OpusEncoder;
    T_OpusEncoder = record end;
    P_OpusDecoder = ^T_OpusDecoder;
    T_OpusDecoder = record end;
    P_OpusRepacketizer = ^T_OpusRepacketizer;
    T_OpusRepacketizer = record end;

  private class var
    FLibHandle: TLibHandle;
    FFuncs: specialize TFPGMap<String, PPointer>;
    // strings
    opus_strerror: function (error: cint): pchar; cdecl;
    opus_get_version: function: pchar; cdecl;
    // encoder
    opus_encoder_get_size: function(channels: cint): cint; cdecl;
    opus_encoder_create: function(fs: cint32; channels: cint; application: cint; error: pcint): P_OpusEncoder; cdecl;
    opus_encoder_init: function(st: P_OpusEncoder; fs: cint32; channels: cint; application_: cint): cint; cdecl;
    opus_encode: function(st: P_OpusEncoder; pcm: pcint16; frame_size: cint; data: pointer; max_data_bytes: cint): cint; cdecl;
    opus_encode_float: function(st: P_OpusEncoder; pcm: pcfloat; frame_size: cint; data: pointer; max_data_bytes: cint): cint; cdecl;
    opus_encoder_destroy: procedure(st: P_OpusEncoder); cdecl;
    opus_encoder_ctl: function(st: P_OpusEncoder; request: cint): cint; varargs; cdecl;
    opus_decoder_get_size: function(channels: cint): cint;
    // decoder
    opus_decoder_create: function(fs: cint32; channels: cint; error: pcint): P_OpusDecoder; cdecl;
    opus_decoder_init: function(st: P_OpusDecoder; fs: cint32; channels: cint): cint; cdecl;
    opus_decode: function(st: P_OpusDecoder; data: pointer; len: cint; pcm: pcint16; frame_size: cint; decode_fec: cint): cint; cdecl;
    opus_decode_float: function(st: P_OpusDecoder; data: pointer; len: cint; pcm: pcfloat; frame_size: cint; decode_fec: cint): cint; cdecl;
    opus_decoder_ctl: function(st: P_OpusDecoder; request: cint): cint; varargs; cdecl;
    opus_decoder_destroy: procedure(st: P_OpusDecoder); cdecl;

    // packet stuff
    opus_packet_parse: function(data: pointer; len: cint; out_toc: pointer; frames: ppointer{[48]}; size: pcint16{48}; payload_offset: pcint): cint; cdecl;
    opus_packet_get_bandwidth: function(data: pointer): cint; cdecl;
    opus_packet_get_samples_per_frame: function(data: pointer; fs: cint32): cint; cdecl;
    opus_packet_get_nb_channels: function(data: pointer): cint; cdecl;
    opus_packet_get_nb_frames: function(packet: ppointer; len: cint): cint; cdecl;
    opus_packet_get_nb_samples: function(dec: P_OpusDecoder; packet: ppointer; len: cint): cint; cdecl;
    // repacketizer
    opus_repacketizer_get_size: function: cint; cdecl;
    opus_repacketizer_init: function(rp: P_OpusRepacketizer): P_OpusRepacketizer; cdecl;
    opus_repacketizer_create: function: P_OpusRepacketizer; cdecl;
    opus_repacketizer_destroy: procedure(rp: P_OpusRepacketizer); cdecl;
    opus_repacketizer_cat: function(rp: P_OpusRepacketizer; data: Pointer; len: cint): cint; cdecl;
    opus_repacketizer_out_range: function(rp: P_OpusRepacketizer; beg: cint; end_: cint; data: pointer; maxlen: cint): cint32; cdecl;
    opus_repacketizer_get_nb_frames: function(packet: ppointer; len: cint): cint; cdecl;
    opus_repacketizer_out: function(rp: P_OpusRepacketizer; data: pointer; maxlen: cint): cint32; cdecl;

  public
    class function  TryLoadLib(ALibName: String = OPUS_LIB): Boolean;
    class procedure UnloadLib;
    class destructor Destroy;
  end;

  //TOpusIdPage = object()

implementation

{ TOpusDecoder }

class function TOpusDecoder.GetSize(AChannels: Integer): Integer;
begin
  Result := 0;
  if not TOpusLib.TryLoadLib() then
    Exit;

  Result := TOpusLib.opus_decoder_get_size(AChannels);
end;

class function TOpusDecoder.PacketGetNumFrames(APacket: Pointer;
  ALength: Integer): Integer;
begin
  Result := TOpusLib.opus_packet_get_nb_frames(APacket, ALength);
end;

class function TOpusDecoder.PacketGetSamplesPerFrame(APacket: Pointer;
  ASampleRate: Integer): Integer;
begin
  Result := TOpusLib.opus_packet_get_samples_per_frame(APacket, ASampleRate);
end;

constructor TOpusDecoder.Create(ASamplerate: Integer; AChannels: Integer; out AError: Integer);
begin
  inherited Create;
  FOpus:=TOpusLib.opus_decoder_create(ASamplerate, AChannels, @AError);
end;

destructor TOpusDecoder.Destroy;
begin
  TOpusLib.opus_decoder_destroy(FOpus);
  inherited Destroy;
end;

function TOpusDecoder.Init(ASampleRate: Integer; AChannels: Integer): Boolean;
begin
  FLastError:=TOpusLib.opus_decoder_init(FOpus, ASamplerate, AChannels);
  Result := FLastError = OPUS_OK;
end;

function TOpusDecoder.Decode(AData: Pointer; ALength: Integer; APCMOut: pcint16;
  AFrameSize: Integer; ADecodeForwardErrorCorrection: Boolean): Boolean;
begin
  FLastError:=TOpusLib.opus_decode(FOpus, AData, ALength, APCMOut, AFrameSize, Ord(ADecodeForwardErrorCorrection));
  Result := FLastError >= OPUS_OK;
end;

function TOpusDecoder.DecodeFloat(AData: Pointer; ALength: Integer;
  APCMOut: pcfloat; AFrameSize: Integer; ADecodeForwardErrorCorrection: Boolean
  ): Boolean;
begin
  FLastError:=TOpusLib.opus_decode_float(FOpus, AData, ALength, APCMOut, AFrameSize, Ord(ADecodeForwardErrorCorrection));
  Result := FLastError >= OPUS_OK;
end;

{ TOpusEncoder }

constructor TOpusEncoder.Create(ASampleRate: Integer; AChannels: Integer;
  AMode: TOpusApplicationMode; out Error: Integer);
begin
  inherited Create;
  FOpus := TOpusLib.opus_encoder_create(ASampleRate, AChannels, ord(AMode), @Error);
end;

procedure TOpusEncoder.Init(ASampleRate: Integer; AChannels: Integer;
  AMode: TOpusApplicationMode);
begin
  FLastError := TOpusLib.opus_encoder_init(FOpus, ASampleRate, AChannels, Ord(AMode));
end;

function TOpusEncoder.Encode(APCMData: pcint16; AFrameSize: Integer;
  AData: Pointer; AMaxDataBytes: Integer; out AEncodedSize: Integer): Boolean;
begin
  AEncodedSize:=0;
  FLastError := TOpusLib.opus_encode(FOpus, APCMData, AFrameSize, AData, AMaxDataBytes);
  Result := FLastError >= OPUS_OK;
  if Result then
    AEncodedSize := FLastError;
end;

function TOpusEncoder.EncodeFloat(APCMData: pcfloat; AFrameSize: Integer;
  AData: Pointer; AMaxDataBytes: Integer; out AEncodedSize: Integer): Boolean;
begin
  AEncodedSize:=0;
  FLastError := TOpusLib.opus_encode_float(FOpus, APCMData, AFrameSize, AData, AMaxDataBytes);
  Result := FLastError >= OPUS_OK;
  if Result then
    AEncodedSize := FLastError;
end;

function TOpusBaseCtl.GetCtlInt(AIndex: Integer): Integer;
begin
  FLastError := ctlfunc(FOpus, AIndex, @Result);
end;

function TOpusEncoder.GetForceChannels(AIndex: Integer): TOpusForceChannels;
begin
  Result := TOpusForceChannels(GetCtlInt(AIndex));
end;

procedure TOpusEncoder.SetApplication(AIndex: Integer;
  AValue: TOpusApplicationMode);
begin
  SetCtlInt(AIndex, Ord(AValue));
end;

procedure TOpusBaseCtl.SetBandwidth(AIndex: Integer; AValue: TOpusBandwidth);
begin
  SetCtlInt(AIndex, Ord(AValue));
end;

function TOpusBaseCtl.GetCtlBool(AIndex: Integer): Boolean;
begin
  Result := GetCtlInt(AIndex) <> 0;
end;

function TOpusBaseCtl.GetBandwidth(AIndex: Integer): TOpusBandwidth;
begin
  Result := TOpusBandwidth(GetCtlInt(AIndex));
end;

function TOpusEncoder.GetApplication(AIndex: Integer): TOpusApplicationMode;
begin
  Result := TOpusApplicationMode(GetCtlInt(AIndex));
end;

procedure TOpusBaseCtl.SetCtlBool(AIndex: Integer; AValue: Boolean);
begin
  SetCtlInt(AIndex, Ord(AValue));
end;

procedure TOpusBaseCtl.SetCtlInt(AIndex: Integer; AValue: Integer);
begin
  // a typo in the opus headers messes up our nice neat method
  if AIndex = OPUS_GET_GAIN_REQUEST then AIndex := OPUS_SET_GAIN_REQUEST+1;

  FLastError := ctlfunc(FOpus, AIndex-1, cint32(AValue));
end;

procedure RaiseOpusNotLoadedException;
begin
  Raise Exception.Create('Opus library is not loaded. Unable to Call dynamic Procedure');
end;

constructor TOpusBaseCtl.Create;
begin
  if not TOpusLib.TryLoadLib then
    RaiseOpusNotLoadedException;
  if InheritsFrom(TOpusDecoder) then
    Pointer(ctlfunc):=TOpusLib.opus_decoder_ctl
  else
    Pointer(ctlfunc):=TOpusLib.opus_encoder_ctl;
end;

procedure TOpusBaseCtl.ResetState;
begin
  if Self.InheritsFrom(TOpusEncoder) then
    FLastError:=TOpusLib.opus_encoder_ctl(FOpus, OPUS_RESET_STATE)
  else
    FLastError:=TOpusLib.opus_decoder_ctl(FOpus, OPUS_RESET_STATE);
end;

procedure TOpusEncoder.SetForceChannels(AIndex: Integer;
  AValue: TOpusForceChannels);
begin
  SetCtlInt(AIndex, Ord(AValue));
end;

class function TOpusEncoder.GetSize(AChannels: Integer): Integer;
begin
  if not TOpusLib.TryLoadLib then
    Raise Exception.Create('Opus library is not loaded. Unable to Create Instance');

  Result := TOpusLib.opus_encoder_get_size(AChannels);
end;

{ TOpusLib }

class function TOpusLib.TryLoadLib(ALibName: String): Boolean;
var
  i: Integer;
begin
  Result := True;
  if FLibHandle <> 0 then
    Exit;

  FLibHandle := LoadLibrary(ALibName);
  if FLibHandle <> 0 then
  begin
    FFuncs := specialize TFPGMap<String, PPointer>.Create;
    FFuncs.Add('opus_strerror', @opus_strerror);
    FFuncs.Add('opus_get_version', @opus_get_version);
    FFuncs.Add('opus_encoder_get_size', @opus_encoder_get_size);
    FFuncs.Add('opus_encoder_create', @opus_encoder_create);
    FFuncs.Add('opus_encoder_init', @opus_encoder_init);
    FFuncs.Add('opus_encode', @opus_encode);
    FFuncs.Add('opus_encode_float', @opus_encode_float);
    FFuncs.Add('opus_encoder_destroy', @opus_encoder_destroy);
    FFuncs.Add('opus_encoder_ctl', @opus_encoder_ctl);
    // decoder
    FFuncs.Add('opus_decoder_get_size', @opus_decoder_get_size);
    FFuncs.Add('opus_decoder_create', @opus_decoder_create);
    FFuncs.Add('opus_decoder_init', @opus_decoder_init);
    FFuncs.Add('opus_decode', @opus_decode);
    FFuncs.Add('opus_decode_float', @opus_decode_float);
    FFuncs.Add('opus_decoder_ctl', @opus_decoder_ctl);
    FFuncs.Add('opus_decoder_destroy', @opus_decoder_destroy);
    // packet stuff
    FFuncs.Add('opus_packet_parse', @opus_packet_parse);
    FFuncs.Add('opus_packet_get_bandwidth', @opus_packet_get_bandwidth);
    FFuncs.Add('opus_packet_get_samples_per_frame', @opus_packet_get_samples_per_frame);
    FFuncs.Add('opus_packet_get_nb_channels', @opus_packet_get_nb_channels);
    FFuncs.Add('opus_packet_get_nb_frames', @opus_packet_get_nb_frames);
    FFuncs.Add('opus_packet_get_nb_samples', @opus_packet_get_nb_samples);
    // repacketizer
    FFuncs.Add('opus_repacketizer_get_size', @opus_repacketizer_get_size);
    FFuncs.Add('opus_repacketizer_init', @opus_repacketizer_init);
    FFuncs.Add('opus_repacketizer_create', @opus_repacketizer_create);
    FFuncs.Add('opus_repacketizer_destroy', @opus_repacketizer_destroy);
    FFuncs.Add('opus_repacketizer_cat', @opus_repacketizer_cat);
    FFuncs.Add('opus_repacketizer_out_range', @opus_repacketizer_out_range);
    FFuncs.Add('opus_repacketizer_get_nb_frames', @opus_repacketizer_get_nb_frames);
    FFuncs.Add('opus_repacketizer_out', @opus_repacketizer_out);

    for i := 0 to FFuncs.Count-1 do
    begin
      FFuncs.Data[i]^ := GetProcedureAddress(FLibHandle, FFuncs.Keys[i]);
    end;

  end
  else
  begin
    Result := False;
    for i := 0 to FFuncs.Count-1 do
    begin
      FFuncs.Data[i]^ := @RaiseOpusNotLoadedException;
    end;
  end;
end;

class procedure TOpusLib.UnloadLib;
var
  i: Integer;
begin
  if FLibHandle <> 0 then
  begin
    for i := 0 to FFuncs.Count-1 do
    begin
      FFuncs.Data[i]^ := @RaiseOpusNotLoadedException;

    end;
    FreeAndNil(FFuncs);
    UnloadLibrary(FLibHandle);
    FLibHandle:=0;
  end;
end;

class destructor TOpusLib.Destroy;
begin
  UnloadLib;
end;

end.

