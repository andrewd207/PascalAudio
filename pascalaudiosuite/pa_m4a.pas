{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2019 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_m4a;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pa_base,
  pa_register,
  pa_stream,
  paio_faad2,
  paio_messagequeue,
  quicktimecontainer,
  quicktimeatoms,
  mp4codec;

type

  { TPAM4ADecoderSource }

  TPAM4ADecoderSource = class(TPAStreamSource, IPAPlayable, IPAStream)
  private
    FInited: Boolean;
    FDecoder: TAACDecoder;
    FContainer: TQuicktimeContainer;
    FSampleIndex: Integer;
    FSampleTable: array of LongWord;
    FChunkOffset: array of LongWord;
    FChunkTable: TstscAtom;
    FTimeToSample: TsttsAtom;
    FFrame: TNeAACDecFrameInfo;
    FBuffer: array[0..1023] of Byte;
    FSampleSize: Integer;
    FCodec: TMP4Codec;

    function ReadNextSample(out AIsLast: Boolean): Boolean;
    procedure InitAudio;
    procedure DeInitAudio;
  protected
    procedure SetStream(AValue: TStream); override;
    function InternalOutputToDestination: Boolean; override;
    function HandleMessage(var AMsg: TPAIOMessage): Boolean; override;
    function  GetPosition: Double;
    procedure SetPosition(AValue: Double);
    function  GetMaxPosition: Double;
  public
    function  CanSeek: Boolean;
    procedure Play;
    procedure Pause;
    procedure Stop;
    property  Position: Double read GetPosition write SetPosition;
    property  MaxPosition: Double read GetMaxPosition;
    constructor Create(AStream: TStream; AOwnsStream: Boolean=True); override;
    destructor Destroy; override;
  end;

implementation
uses MD5;

{ TPAM4ADecoderSource }

function TPAM4ADecoderSource.ReadNextSample(out AIsLast: Boolean): Boolean;
var
  i: Integer;
  lFirstIndex, lChunkIndex: DWord;
  lSamplesOffset: DWord = 0;
  lCount: LongInt;
begin
  Result := FSampleIndex < Length(FSampleTable);
  AIsLast:=FSampleIndex+1 >= Length(FSampleTable);
  if not Result then
    Exit;
  //WriteLn('Sample index: ', FSampleIndex);
  FSampleSize := FSampleTable[FSampleIndex];
  //WriteLn('Sample Size: ', FSampleSize);

  lChunkIndex := FChunkTable.SampleIndexToChunkIndex(1, FSampleIndex, lFirstIndex);


  for i := lFirstIndex to FSampleIndex-1 do
    Inc(lSamplesOffset, FSampleTable[i]);

  FContainer.Stream.Position:=FChunkOffset[lChunkIndex] + lSamplesOffset;
  lCount := FContainer.Stream.Read(FBuffer[0], FSampleSize);
  //WriteLN('read = ', lCount);

  if Assigned(FCodec) then
  begin
    FCodec.Filter(@FBuffer[0], FSampleSize);
    //WriteLn('codec filtered' );
  end;
  Inc(FSampleIndex);
  {Write(Pred(FSampleIndex), ': ');
  for i := 0 to 7 do
    Write('0x'+HexStr(FBuffer[i], 2)+' ');
  WriteLn;}
end;

procedure TPAM4ADecoderSource.InitAudio;
var
  lmp4aAtom: TSoundSampleDecriptionAtom;
  lesds: TesdsAtom;

  lBuf: Array[0..254] of Byte;
  lConfig: Word;
  lSize: Integer;
  lSampleTable: TstszAtom;
  lSampleOffSet: TstcoAtom;
  lDecoderConfig: PNeAACDecConfiguration;
  lmp4ASC: Tmp4AudioSpecificConfig;
  lstsdAtom: TstsdAtom;
  lCodecClass: TMP4CodecClass;
begin
  if FInited then
    Exit;
  FInited:=True;
  FContainer := TQuicktimeContainer.Create(FStream, False);
  if not FContainer.IsValidFile then
  begin
    DeInitAudio;
    Exit;
  end;

  FChunkTable := TstscAtom(FContainer.Atoms.FindAtom('moov/trak/mdia/minf/stbl/stsc'));
  FTimeToSample := TsttsAtom(FContainer.Atoms.FindAtom('moov/trak/mdia/minf/stbl/stts'));

  lSampleTable := TstszAtom(FContainer.Atoms.FindAtom('moov/trak/mdia/minf/stbl/stsz'));
  SetLength(FSampleTable, lSampleTable.SampleCount);
  //Writeln(Length(FSampleTable), ' samples');
  lSampleTable.CopySampleTable(@FSampleTable[0]);

  lSampleOffSet := TstcoAtom(FContainer.Atoms.FindAtom('moov/trak/mdia/minf/stbl/stco'));
  SetLength(FChunkOffset, lSampleOffSet.Count);
  lSampleOffSet.CopyTable(@FChunkOffset[0]);

  lstsdAtom := TstsdAtom(FContainer.Atoms.FindAtom('moov/trak/mdia/minf/stbl/stsd'));


  lmp4aAtom := TSoundSampleDecriptionAtom(lstsdAtom.Atoms.Atom[0]);
  //WriteLn(lmp4aAtom.ClassName);
  //WriteLn(lmp4aAtom.AtomName.Chars);

  if MP4LookupCodec(lmp4aAtom.AtomName, lCodecClass) then
  begin
    FCodec := lCodecClass.Create(lmp4aAtom);
  end;

  if lmp4aAtom = nil then
  begin
    DeInitAudio;
    Exit;
  end;

  lesds := TesdsAtom(lmp4aAtom.Atoms.FindAtom('esds'));
  lConfig:=lesds.DecoderConfig;



  FDecoder := TAACDecoder.Create;
  lDecoderConfig := FDecoder.Config;
  //WriteLn(FDecoder.GetErroMessage(FDecoder.LastResult));;
  lDecoderConfig^.outputFormat:=ord(FAAD_FMT_FLOAT);
  FDecoder.Config := lDecoderConfig;

  FDecoder.Init2(@lConfig, 2);
  WriteLn('Init2: ', FDecoder.LastResult);

  WriteLn('AudioSpcecifiConfig: ',FDecoder.AudioSpecificConfig(@lConfig, 2, @lmp4ASC));
  Channels:=FDecoder.Channels;
  SamplesPerSecond:=FDecoder.SampleRate;

end;

procedure TPAM4ADecoderSource.DeInitAudio;
begin
  if not FInited then
    Exit;
  FInited:=False;
  FreeAndNil(FCodec);
  FreeAndNil(FDecoder);
  FreeAndNil(FContainer);
end;

procedure TPAM4ADecoderSource.SetStream(AValue: TStream);
begin
  if FStream=AValue then Exit;
  if FStream <> nil then
  begin
    StopData;
    DeInitAudio;
  end;
  inherited SetStream(AValue);
end;

function TPAM4ADecoderSource.InternalOutputToDestination: Boolean;
var
  lRes: Pointer;
  lSampleBuffer: array[0..2047] of Byte;
  lIsLast: Boolean;
begin
  if not FInited then
    InitAudio;

  Result := ReadNextSample(lIsLast);

  if Result then
  begin

    //WriteLN(MD5Print(MD5Buffer(FBuffer[0], FSampleSize)));
    lRes := FDecoder.Decode(@FFrame, @FBuffer[0], FSampleSize);
    with FFrame do
    begin
      {WriteLn('bytesconsumed: ',bytesconsumed);
      WriteLn('samples: ', samples);
      WriteLn('channels: ', channels);
      WriteLn('error: ',error);
      writeln('samplerate: ', samplerate);
      //* SBR: 0: off, 1: on; upsample, 2: on; downsampled, 3: off; upsampled */
      WriteLn('sbr: ', sbr);
      //* MPEG-4 ObjectType */
      WriteLn('object_type: ',object_type);
      //Halt;}
    end;

    if FFrame.error <> 0 then
      WriteLn('Error: ', FDecoder.GetErroMessage(FFrame.error));
    if (FFrame.samples > 0) then
    begin
      WriteToBuffer(lRes^,FFrame.samples*4, False);
    end;
    {

    WriteLn(SysUtils.Format('Dec Bytes: %d, data := 0x%s Samples: %d', [FFrame.bytesconsumed, Hexstr(PtrUint(lRes), 16), FFrame.samples]));
    //if FFrame.;
    if (FFrame.error > 0) or (FFrame.bytesconsumed > 0) or (lRes <> nil) then
    begin

     writeln;
    end;     }
  end
  else
    REsult := False; // for debugging stop point

  if not Result then
    SignalDestinationsDone;
  {if lIsLast then
    Result := False;}
  //sleep(1000);





end;

function TPAM4ADecoderSource.HandleMessage(var AMsg: TPAIOMessage): Boolean;
var
  lSample, lSampleIndex: Int64;
  lOffset: Integer;
begin
  Result := True;
  case AMsg.Message of
    PAM_Seek:
      if FInited then
      begin
        lSample := Trunc(Double((SamplesPerSecond / Channels) * AMsg.Data));
        lSampleIndex := FTimeToSample.FindSampleIndex(lSample, lOffset);
        FSampleIndex:=lSampleIndex;
      end;
  else
    Result := False;
  end;
end;

function TPAM4ADecoderSource.GetPosition: Double;
begin
  if not FInited then
    Exit(0);
  Result := FTimeToSample.FindSampleFromIndex(FSampleIndex) / SamplesPerSecond;
end;

procedure TPAM4ADecoderSource.SetPosition(AValue: Double);
begin
  if not FInited then
    Exit;

  FMsgQueue.PostMessage(PAM_Seek, AValue);
end;

function TPAM4ADecoderSource.GetMaxPosition: Double;
begin
  Result := FTimeToSample.TotalSamples /  SamplesPerSecond;
  WriteLn('Channels: ', Channels, ' SPS: ', SamplesPerSecond);
end;

function TPAM4ADecoderSource.CanSeek: Boolean;
begin
  Result := True;
end;

procedure TPAM4ADecoderSource.Play;
begin

end;

procedure TPAM4ADecoderSource.Pause;
begin

end;

procedure TPAM4ADecoderSource.Stop;
begin

end;

constructor TPAM4ADecoderSource.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create(AStream, AOwnsStream);
  Format:=afFloat32;
end;

destructor TPAM4ADecoderSource.Destroy;
begin
  // stop the worker thread (which uses FContainer/FDecoder in
  // InternalOutputToDestination) before freeing them, then free the stream.
  DestroyWaitSync;
  DeInitAudio;
  inherited Destroy;
end;

initialization
  PARegister(partDecoder, TPAM4ADecoderSource, 'MP4audio', '.m4a', 'ftyp', 4, 4); // ftyp is at 4 offset

end.

