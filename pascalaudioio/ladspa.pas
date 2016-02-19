{* ladspa.pas

   Linux Audio Developer's Simple Plugin API Version 1.1[LGPL].
   Copyright (C) 2000-2002 Richard W.E. Furse, Paul Barton-Davis,
   Stefan Westerfeld.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   as published by the Free Software Foundation; either version 2.1 of
   the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA. *}
unit ladspa;

{$mode objfpc}{$H+}
{$packrecords c}
{$calling c}

interface

uses
  Classes, SysUtils, ctypes;

type

  PLADSPA_Data = ^LADSPA_Data;
  LADSPA_Data = cfloat;

  LADSPA_Properties = cint;
const
  LADSPA_PROPERTY_REALTIME        = $1;
  LADSPA_PROPERTY_INPLACE_BROKEN  = $2;
  LADSPA_PROPERTY_HARD_RT_CAPABLE = $4;

  //#define LADSPA_IS_REALTIME(x)        ((x) & LADSPA_PROPERTY_REALTIME)
  //#define LADSPA_IS_INPLACE_BROKEN(x)  ((x) & LADSPA_PROPERTY_INPLACE_BROKEN)
  //#define LADSPA_IS_HARD_RT_CAPABLE(x) ((x) & LADSPA_PROPERTY_HARD_RT_CAPABLE)

type
  PLADSPA_PortDescriptor = ^LADSPA_PortDescriptor;
  LADSPA_PortDescriptor = cint;
const
  LADSPA_PORT_INPUT   = $1;
  LADSPA_PORT_OUTPUT  = $2;
  LADSPA_PORT_CONTROL = $4;
  LADSPA_PORT_AUDIO   = $8;
//#define LADSPA_IS_PORT_INPUT(x)   ((x) & LADSPA_PORT_INPUT)
//#define LADSPA_IS_PORT_OUTPUT(x)  ((x) & LADSPA_PORT_OUTPUT)
//#define LADSPA_IS_PORT_CONTROL(x) ((x) & LADSPA_PORT_CONTROL)
//#define LADSPA_IS_PORT_AUDIO(x)   ((x) & LADSPA_PORT_AUDIO)

type
  LADSPA_PortRangeHintDescriptor = cint;
const
  LADSPA_HINT_BOUNDED_BELOW   = $1;
  LADSPA_HINT_BOUNDED_ABOVE   = $2;
  LADSPA_HINT_TOGGLED         = $4;
  LADSPA_HINT_SAMPLE_RATE     = $8;
  LADSPA_HINT_LOGARITHMIC     = $10;
  LADSPA_HINT_INTEGER         = $20;
  LADSPA_HINT_DEFAULT_MASK    = $3C0;
  LADSPA_HINT_DEFAULT_NONE    = 0;
  LADSPA_HINT_DEFAULT_MINIMUM = $40;
  LADSPA_HINT_DEFAULT_LOW     = $80;
  LADSPA_HINT_DEFAULT_MIDDLE  = $C0;
  LADSPA_HINT_DEFAULT_HIGH    = $100;
  LADSPA_HINT_DEFAULT_MAXIMUM = $140;
  LADSPA_HINT_DEFAULT_0       = $200;
  LADSPA_HINT_DEFAULT_1       = $240;
  LADSPA_HINT_DEFAULT_100     = $280;
  LADSPA_HINT_DEFAULT_440     = $2C0;
{
#define LADSPA_IS_HINT_BOUNDED_BELOW(x)   ((x) & LADSPA_HINT_BOUNDED_BELOW)
#define LADSPA_IS_HINT_BOUNDED_ABOVE(x)   ((x) & LADSPA_HINT_BOUNDED_ABOVE)
#define LADSPA_IS_HINT_TOGGLED(x)         ((x) & LADSPA_HINT_TOGGLED)
#define LADSPA_IS_HINT_SAMPLE_RATE(x)     ((x) & LADSPA_HINT_SAMPLE_RATE)
#define LADSPA_IS_HINT_LOGARITHMIC(x)     ((x) & LADSPA_HINT_LOGARITHMIC)
#define LADSPA_IS_HINT_INTEGER(x)         ((x) & LADSPA_HINT_INTEGER)

#define LADSPA_IS_HINT_HAS_DEFAULT(x)     ((x) & LADSPA_HINT_DEFAULT_MASK)
#define LADSPA_IS_HINT_DEFAULT_MINIMUM(x) (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_MINIMUM)
#define LADSPA_IS_HINT_DEFAULT_LOW(x)     (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_LOW)
#define LADSPA_IS_HINT_DEFAULT_MIDDLE(x)  (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_MIDDLE)
#define LADSPA_IS_HINT_DEFAULT_HIGH(x)    (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_HIGH)
#define LADSPA_IS_HINT_DEFAULT_MAXIMUM(x) (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_MAXIMUM)
#define LADSPA_IS_HINT_DEFAULT_0(x)       (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_0)
#define LADSPA_IS_HINT_DEFAULT_1(x)       (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_1)
#define LADSPA_IS_HINT_DEFAULT_100(x)     (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_100)
#define LADSPA_IS_HINT_DEFAULT_440(x)     (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                            == LADSPA_HINT_DEFAULT_440)
}

type
  PLADSPA_PortRangeHint = ^LADSPA_PortRangeHint;
  LADSPA_PortRangeHint  = record
    HintDescriptor: LADSPA_PortRangeHintDescriptor;
    LowerBound: LADSPA_Data;
    UpperBound: LADSPA_Data;
  end;

  LADSPA_Handle = pointer;
  PLADSPA_Descriptor = ^LADSPA_Descriptor;
  LADSPA_Descriptor = record
    UniqueID: cunsigned;
    Label_: PChar;
    Properties: LADSPA_Properties; //count
    Name: PChar;
    Maker: PChar;
    Copyright: PChar;
    PortCount: cunsigned;
    PortDescriptors: PLADSPA_PortDescriptor;
    PortNames: PPChar;
    PortRangeHints: PLADSPA_PortRangeHint;
    ImplementationData: Pointer;
    instantiate:          function (Descriptor:  PLADSPA_Descriptor; SampleRate: cunsigned): LADSPA_Handle;
    connect_port:         procedure (Instance: LADSPA_Handle; Port: cunsigned; DataLocation: PLADSPA_Data);
    activate:             procedure (Instance: LADSPA_Handle);
    run:                  procedure (Instance: LADSPA_Handle; SampleCount: cunsigned);
    run_adding:           procedure (Instance: LADSPA_Handle; SampleCount: cunsigned);
    set_run_adding_gain:  procedure (Instance: LADSPA_Handle; Gain: LADSPA_Data);
    deactivate:           procedure (Instance: LADSPA_Handle);
    cleanup:              procedure (Instance: LADSPA_Handle);
  end;


  LADSPA_DescriptorFunction = function(Index : cunsigned) : PLADSPA_Descriptor;
const
  LADSPA_DescriptorExport = 'ladspa_descriptor';



implementation

end.

