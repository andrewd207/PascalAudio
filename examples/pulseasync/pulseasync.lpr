program pulseasync;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, Math, paio_pulse, pulse_sample;
const
  Rate = 44100; Channels = 2; Secs = 2; Freq = 440.0;
var
  stream: TPulseAsyncStream;
  buf: array of Single;
  i: Integer;
  v: Single;
begin
  // generate a short interleaved-stereo float32 sine
  SetLength(buf, Rate*Secs*Channels);
  for i := 0 to (Rate*Secs)-1 do
  begin
    v := 0.2 * Sin(2*Pi*Freq*i/Rate);
    buf[i*Channels]   := v;
    buf[i*Channels+1] := v;
  end;

  stream := TPulseAsyncStream.Create('pulseasync demo', Rate, Channels, sfFloat32LE);
  try
    if not stream.Open then
    begin
      WriteLn('Open failed: ', stream.LastError);
      Halt(1);
    end;
    WriteLn('opened. writing ', Length(buf)*SizeOf(Single), ' bytes');
    WriteLn('wrote ', stream.Write(buf[0], Length(buf)*SizeOf(Single)), ' bytes');
    stream.Drain;
    WriteLn('drained, closing');
  finally
    stream.Free;
  end;
  WriteLn('done');
end.
