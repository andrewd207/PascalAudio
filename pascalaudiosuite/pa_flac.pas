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
  Classes, SysUtils, pa_base, flac_classes;

type
  TPAFlacSource = class (TPAAudioSource, IPAStream)
  private
    FOwnsStream: Boolean;
    FStream: TStream;
    FFlac: TFlacStreamDecoder;
    function GetStream: TStream;
    function HandleData(Sender: TFlacStreamDecoder; ASamples: Integer; AChannels: Integer; AChannelData: PPLongInt): Boolean;
  protected
    function InternalOutputToDestination: Boolean; override;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean); reintroduce;
    destructor Destroy; override;
    property Stream: TStream read GetStream;
  end;

implementation

{ TPAFlacSource }

function TPAFlacSource.GetStream: TStream;
begin
  Result := FStream;
end;

function TPAFlacSource.HandleData(Sender: TFlacStreamDecoder; ASamples: Integer; AChannels: Integer; AChannelData: PPLongInt): Boolean;
var
  ChannelsData: array[0..7] of SmallInt; // 16 bit signed. max 8 channels
  i, j: Integer;
begin
  Result := True;
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

constructor TPAFlacSource.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  Inherited Create;
  FFlac := TFlacStreamDecoder.Create(AStream, False);
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

destructor TPAFlacSource.Destroy;
begin
  FFlac.Free;
  inherited Destroy;
end;

end.

