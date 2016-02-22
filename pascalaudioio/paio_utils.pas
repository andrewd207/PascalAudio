{
    This unit is part of Pascal Audio IO package.

    Copyright (c) 2016 by Andrew Haines.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit paio_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, paio_types;



function NewChannelArray(AChannels: Integer; ASamplesPerChannel: Integer): TChannelArray;
function SplitChannels(AData: PSingle; ASamples: Integer; AChannels: Integer): TChannelArray;
function JoinChannels(AChannelData: TChannelArray; ASamples: Integer = -1): TSingleArray;
function JoinChannels(AChannelData: PPSingle; AChannels: Integer; ASamples: Integer): TSingleArray;

function Min(A,B: Integer): Integer;
function Max(A,B: Integer): Integer;

implementation

function Min(A,B: Integer): Integer;
begin
  if A < B then Exit(A);
  Result := B;
end;

function Max(A,B: Integer): Integer;
begin
  if A > B then Exit(A);
  Result := B;
end;

function NewChannelArray(AChannels: Integer; ASamplesPerChannel: Integer): TChannelArray;
var
  i: Integer;
begin
  SetLength(Result, AChannels);
  for i := 0 to AChannels-1 do
    SetLength(Result[i], ASamplesPerChannel);
end;

// Samples is total samples not samples per channel.
// So Samples = 1000 if 2 Channels have 500 each
function SplitChannels(AData: PSingle; ASamples: Integer; AChannels: Integer): TChannelArray;
var
  SamplesPerChannel: Integer;
  i, j: Integer;
begin
  SamplesPerChannel:=ASamples div AChannels;
  //SetLength(Result, AChannels);
  Result := NewChannelArray(AChannels, SamplesPerChannel);
  for i := 0 to AChannels-1 do
  begin
    //SetLength(Result[i], SamplesPerChannel);
    for j := 0 to SamplesPerChannel-1 do
    begin
      Result[i][j] := AData[j*AChannels+i];
    end;
  end;
end;

function JoinChannels(AChannelData: TChannelArray; ASamples: Integer): TSingleArray;
var
  i: Integer;
  j: Integer;
  Samples: Integer;
begin
  if Length(AChannelData) > 0 then
  begin
   if ASamples <> -1 then
     Samples := ASamples
   else
     Samples := Length(AChannelData[0]);

   SetLength(Result, Length(AChannelData) * Samples);
    for i := 0 to High(AChannelData) do
      for j := 0 to Samples-1 do
        Result[j*Length(AChannelData)+i] := AChannelData[i][j];
  end
  else
    SetLength(Result, 0);
end;

function JoinChannels(AChannelData: PPSingle; AChannels: Integer;
  ASamples: Integer): TSingleArray;
var
  i: Integer;
  j: Integer;
begin
  if ASamples > 0 then
  begin
   SetLength(Result, AChannels * ASamples);
    for i := 0 to AChannels-1 do
      for j := 0 to ASamples-1 do
        Result[j*AChannels+i] := AChannelData[i][j];
  end
  else
    SetLength(Result, 0);

end;


end.

