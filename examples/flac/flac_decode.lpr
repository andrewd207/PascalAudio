program flac_decode;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, flac_classes, paio_channelhelper;

type
  TDecode = class
  private
    Decoder: TFlacStreamDecoder;
    OutputStream: TStream;
    function DataEvent(Sender: TFlacStreamDecoder; Samples: Integer; Channels: Integer; ChannelData: PPLongInt): Boolean;
    procedure MetadataEvent(Sender: TFlacStreamDecoder; Metadata: TFlacStreamMetadata);
  public
    constructor Create(AFile: String);
  end;
var
  Decode: TDecode;

{ TDecode }

constructor TDecode.Create(AFile: String);
var
  InputStream: TFileStream;
begin
  // create input and output streams
  InputStream := TFileStream.Create(AFile, fmOpenRead);
  OutputStream := THandleStream.Create(StdOutputHandle); // :)
  // create the decoder object and set callbacks
  Decoder := TFlacStreamDecoder.Create(InputStream, False);
  Decoder.OnOutput:=@DataEvent;
  Decoder.OnMetadata:=@MetadataEvent;

  // process metadata to fill channels, samplerate and bitspersample
  Decoder.ProcessUntilEndOfMetadata;

  // write some info to stderr since stdout will contain raw audio data
  WriteLn(StdErr, 'Channels: ', Decoder.Channels);
  WriteLn(StdErr, 'SampleRate: ', Decoder.SampleRate);
  WriteLn(StdErr, 'BitsPerSample: ', Decoder.BitsPerSample);

  // process the audio data
  Decoder.ProcessUntilEndOfStream;
  Decoder.Flush;

  // free stuff
  Decoder.Free;
  OutputStream.Free;
  InputStream.Free;
end;

function TDecode.DataEvent(Sender: TFlacStreamDecoder; Samples: Integer; Channels: Integer; ChannelData: PPLongInt): Boolean;
var
  i, j: Integer;
  DataSize: Integer;
begin
  // write the output plexing the channels
  DataSize:=Sender.BitsPerSample div 8;
  for i := 0 to Samples-1 do
    for j := 0 to Channels-1 do
      OutputStream.Write(ChannelData[j][i], DataSize);
  Result := True;
end;

procedure TDecode.MetadataEvent(Sender: TFlacStreamDecoder; Metadata: TFlacStreamMetadata);
begin
  WriteLn(Stderr, 'Metadata: ', Metadata.ClassName);
end;

begin
  Decode := TDecode.Create(ParamStr(1));
  Decode.Free;
end.

