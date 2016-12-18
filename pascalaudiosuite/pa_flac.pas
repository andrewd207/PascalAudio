{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_flac;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pa_base, flac_classes, pa_register, pa_stream, paio_messagequeue;

type
  TPAFlacSource = class (TPAStreamSource, IPAStream, IPAPlayable)
  protected
    procedure SetStream(AValue: TStream); override;
    procedure HandleMessage(var AMsg: TPAIOMessage); override;
  private
    FFlac: TFlacStreamDecoder;
    FIgnore: Boolean;
    function HandleData(Sender: TFlacStreamDecoder; ASamples: Integer; AChannels: Integer; AChannelData: PPLongInt): Boolean;
    // IPAPlayable
    function  CanSeek: Boolean;
    function  GetPosition: Double;
    procedure SetPosition(AValue: Double);
    function  GetMaxPosition: Double;

    procedure InternalSetPosition(APosition: Double);
  protected
    function InternalOutputToDestination: Boolean; override;
  public
    procedure Play;
    procedure Pause;
    procedure Stop;
    property  Position: Double read GetPosition write SetPosition;
    property  MaxPosition: Double read GetMaxPosition;
    property Stream;
  end;

implementation

uses
  paio_types;

{ TPAFlacSource }

procedure TPAFlacSource.SetStream(AValue: TStream);
begin
  if Assigned(FFlac) then
    FreeAndNil(FFlac);

  inherited SetStream(AValue);

  if not Assigned(FStream) then
    Exit;

  FFlac := TFlacStreamDecoder.Create(FStream, False);
  FFlac.OnOutput:=@HandleData;
  FFlac.ProcessUntilEndOfMetadata;

  Channels:=FFlac.Channels;
  SamplesPerSecond:=FFlac.SampleRate;
  case FFlac.BitsPerSample of
    16: Format:=afS16;
  else
    raise Exception.Create('unsupported flac data type');
  end;
end;

procedure TPAFlacSource.HandleMessage(var AMsg: TPAIOMessage);
begin
  case AMsg.Message of
    PAM_Seek:
      begin
        InternalSetPosition(AMsg.Data);
      end;
  end;
end;

function TPAFlacSource.HandleData(Sender: TFlacStreamDecoder; ASamples: Integer; AChannels: Integer; AChannelData: PPLongInt): Boolean;
var
  ChannelsData: array[0..7] of SmallInt; // 16 bit signed. max 8 channels
  i, j: Integer;
  Msg: TPAIOMessage;
begin
  Result := True;
  if FIgnore then
    Exit;
  if FMsgQueue.HasMessage then
  begin
    Msg := FMsgQueue.PopMessage;

    case Msg.Message of
      PAM_Seek:
        begin
          InternalSetPosition(Msg.Data);
          Msg.Free;
        end
    else
      FMsgQueue.InsertBefore([Msg.Message], Msg);
    end;

  end;

  for i := 0 to ASamples-1 do
  begin
    // plex channels
    for j := 0 to AChannels-1 do
    begin
      ChannelsData[j] := AChannelData[j][i];
    end;
    WriteToBuffer(ChannelsData[0],SizeOf(Smallint)*AChannels, False);
  end;
end;

function TPAFlacSource.CanSeek: Boolean;
begin
  Result := True;
end;

function TPAFlacSource.GetPosition: Double;
begin
  if not Assigned(FFlac) then
    Exit(0);
  Result := FFlac.DecodedSamplePosition / SamplesPerSecond;
end;

procedure TPAFlacSource.SetPosition(AValue: Double);
begin
  FMsgQueue.PostMessage(PAM_Seek, AValue);
end;

function TPAFlacSource.GetMaxPosition: Double;
begin
  REsult := FFlac.TotalSamples / SamplesPerSecond;
end;

procedure TPAFlacSource.InternalSetPosition(APosition: Double);
begin
  FIgnore:=True;
  try
    FFlac.Flush;
    FFlac.SeekAbsolute(Trunc(APosition * SamplesPerSecond));
  finally
    FIgnore:=False;
  end;
end;

function TPAFlacSource.InternalOutputToDestination: Boolean;
begin
  Result := FFlac.ProcessSingle;
  if not Result or (FFlac.State = fsdsEndOfStream) then
  begin
    Result := False;
    FFlac.Flush;
    SignalDestinationsDone;
  end;
end;

procedure TPAFlacSource.Play;
begin

end;

procedure TPAFlacSource.Pause;
begin

end;

procedure TPAFlacSource.Stop;
begin

end;

initialization
  PARegister(partDecoder, TPAFlacSource, 'FLAC', '.flac', 'fLaC', 4);
end.

