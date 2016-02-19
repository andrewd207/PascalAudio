{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_ladspa;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pa_base, ladspa_classes;

type

  { TPALADSPALink }

  TPALADSPALink = class(TPAAudioLink, IPAAudioInformation)
  private
    FInstance: TLADSPAInstance;
    FInited: Boolean;
    procedure InitData;
  protected
    function  InternalProcessData(const AData; ACount: Int64; AIsLastData: Boolean): Int64; override;
    procedure SignalDestinationsDone; override;
  public
    constructor Create(AInstance: TLADSPAInstance);
    destructor Destroy; override;
    property LADSPA: TLADSPAInstance read FInstance;
  end;

implementation

{ TPALADSPALink }

procedure TPALADSPALink.InitData;
begin
  FInited:=True;
  FInstance.Reset; // starting new data stream
end;

function TPALADSPALink.InternalProcessData(const AData; ACount: Int64;
  AIsLastData: Boolean): Int64;
var
  ChannelData: TChannelArray;
  InputPorts: TLADSPAInstance.TPortArray;
  OutputPorts: TLADSPAInstance.TPortArray;
  i: Integer;
  OutputData: TChannelArray;
  PlexedData: TSingleArray;
  SamplesPerChannel: Integer;
begin
  if not FInited then
    InitData;

  ChannelData := SplitChannels(PSingle(@AData), ACount div SizeOf(Single), Channels);
  SamplesPerChannel := Length(ChannelData[0]);
  InputPorts := FInstance.AudioInputs;
  OutputPorts := FInstance.AudioOutputs;

  SetLength(OutputData, Channels);

  for i := 0 to High(InputPorts) do
    InputPorts[i].SetValue(@ChannelData[i][0]);

  for i := 0 to High(OutputPorts) do
  begin
    // allocate output data the same size
    SetLength(OutputData[i], SamplesPerChannel);
    OutputPorts[i].SetValue(@OutputData[i][0]);
  end;

  FInstance.Run(SamplesPerChannel);

  PlexedData := JoinChannels(OutputData);

  WriteToBuffer(PlexedData[0], Length(PlexedData)*SizeOf(Single), AIsLastData);
end;

procedure TPALADSPALink.SignalDestinationsDone;
begin
  inherited SignalDestinationsDone;
end;

constructor TPALADSPALink.Create(AInstance: TLADSPAInstance);
begin
  Inherited Create;
  FInstance := AInstance;
end;

destructor TPALADSPALink.Destroy;
begin
  inherited Destroy;
end;

end.

