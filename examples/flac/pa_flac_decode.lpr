program pa_flac_decode;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, pa_flac, pa_stream;

var
  SourceFlac: TPAFlacSource;
  DestStream: TPAStreamDestination;

  OutputStream: THandleStream;

procedure CreateObjects;
var
  InputStream: TFileStream;
begin

  InputStream := TFileStream.Create(ParamStr(1), fmOpenRead);
  OutputStream := THandleStream.Create(StdOutputHandle); // :)

  SourceFlac := TPAFlacSource.Create(InputStream, True);
  DestStream := TPAStreamDestination.Create(OutputStream);
  DestStream.DataSource := SourceFlac;

  // write some info to stderr since stdout will contain raw audio data
  WriteLn(StdErr, 'Channels: ', SourceFlac.Channels);
  WriteLn(StdErr, 'SampleRate: ', SourceFlac.SamplesPerSecond);
  WriteLn(StdErr, 'Format: ', SourceFlac.Format);
end;

procedure FreeObjects;
begin
  SourceFlac.Free;
  DestStream.Free;
  OutputStream.Free;
end;

procedure Decode;
begin
  SourceFlac.StartData;

  while SourceFlac.Working or DestStream.Working do
    CheckSynchronize(1);
end;

begin
  CreateObjects;
  Decode;
  FreeObjects;
end.

