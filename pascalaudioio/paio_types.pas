{
    This unit is part of Pascal Audio IO package.

    Copyright (c) 2016 by Andrew Haines.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit paio_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  AUDIO_BUFFER_SIZE  = 8192;
  AUDIO_BUFFER_FLOAT_SAMPLES = AUDIO_BUFFER_SIZE div 4;

type
  PPSingle = ^PSingle;
  TSingleArray = array of Single;
  TChannelArray = array of TSingleArray;

implementation

end.

