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
  mp4codec,
  paio_log;

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
    FBuffer: array of Byte; // grown to hold the largest sample (was a fixed
                            // 1024-byte array that AAC frames could overrun)
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

  // grow the read buffer to fit this sample. AAC frames routinely exceed the
  // old fixed 1024-byte buffer (FAAD allows ~768 bytes per channel), which
  // overflowed the stack/heap buffer.
  if FSampleSize > Length(FBuffer) then
    SetLength(FBuffer, FSampleSize);

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
    TPALog.Warning(ClassName, 'init failed: not a valid m4a/mp4 file');
    DeInitAudio;
    Exit;
  end;

  FChunkTable := TstscAtom(FContainer.Atoms.FindAtom('moov/trak/mdia/minf/stbl/stsc'));
  FTimeToSample := TsttsAtom(FContainer.Atoms.FindAtom('moov/trak/mdia/minf/stbl/stts'));
  lSampleTable := TstszAtom(FContainer.Atoms.FindAtom('moov/trak/mdia/minf/stbl/stsz'));
  lSampleOffSet := TstcoAtom(FContainer.Atoms.FindAtom('moov/trak/mdia/minf/stbl/stco'));

  // these tables are required to locate (stsc/stco/stsz) and seek (stts) samples;
  // ReadNextSample and the seek handler dereference them unconditionally.
  if (FChunkTable = nil) or (FTimeToSample = nil) or (lSampleTable = nil) or (lSampleOffSet = nil) then
  begin
    TPALog.Warning(ClassName, 'init failed: missing sample tables (stsc/stts/stsz/stco)');
    DeInitAudio;
    Exit;
  end;

  SetLength(FSampleTable, lSampleTable.SampleCount);
  //Writeln(Length(FSampleTable), ' samples');
  lSampleTable.CopySampleTable(@FSampleTable[0]);

  SetLength(FChunkOffset, lSampleOffSet.Count);
  lSampleOffSet.CopyTable(@FChunkOffset[0]);

  lstsdAtom := TstsdAtom(FContainer.Atoms.FindAtom('moov/trak/mdia/minf/stbl/stsd'));

  // bail out (rather than dereference nil) if the sample description is missing.
  if (lstsdAtom = nil) or (lstsdAtom.Atoms.Count = 0) then
  begin
    TPALog.Warning(ClassName, 'init failed: missing sample description (stsd)');
    DeInitAudio;
    Exit;
  end;

  lmp4aAtom := TSoundSampleDecriptionAtom(lstsdAtom.Atoms.Atom[0]);
  if lmp4aAtom = nil then
  begin
    TPALog.Warning(ClassName, 'init failed: missing audio sample description');
    DeInitAudio;
    Exit;
  end;

  if MP4LookupCodec(lmp4aAtom.AtomName, lCodecClass) then
    FCodec := lCodecClass.Create(lmp4aAtom);

  lesds := TesdsAtom(lmp4aAtom.Atoms.FindAtom('esds'));
  if lesds = nil then
  begin
    TPALog.Warning(ClassName, 'init failed: missing decoder config (esds)');
    DeInitAudio;
    Exit;
  end;

  FDecoder := TAACDecoder.Create;
  lDecoderConfig := FDecoder.Config;
  lDecoderConfig^.outputFormat:=ord(FAAD_FMT_FLOAT);
  FDecoder.Config := lDecoderConfig;

  // pass the full AudioSpecificConfig (any length) to the decoder.
  FDecoder.Init2(lesds.DecoderConfigData, lesds.DecoderConfigLength);
  FDecoder.AudioSpecificConfig(lesds.DecoderConfigData, lesds.DecoderConfigLength, @lmp4ASC);
  Channels:=FDecoder.Channels;
  SamplesPerSecond:=FDecoder.SampleRate;
  TPALog.Info(ClassName, 'initialized');
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
        // AMsg.Data is the target position in seconds. Convert to the media time
        // position (PCM samples, since the AAC media timescale is the sample
        // rate) and map that to a frame index via the stts table. The old code
        // divided by Channels, so every seek on a multichannel file landed at
        // position/Channels; GetPosition has no such factor, so seek disagreed
        // with the reported position.
        lSample := Trunc(Double(SamplesPerSecond * AMsg.Data));
        lSampleIndex := FTimeToSample.FindSampleIndex(lSample, lOffset);
        if lSampleIndex < 0 then
          lSampleIndex := 0
        else if lSampleIndex > Length(FSampleTable) then
          lSampleIndex := Length(FSampleTable);
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
  if not CanSeek then
  begin
    LogSeekRefused;
    Exit;
  end;

  FMsgQueue.PostMessage(PAM_Seek, AValue);
end;

function TPAM4ADecoderSource.GetMaxPosition: Double;
begin
  // can be queried before playback starts (FTimeToSample/SamplesPerSecond not
  // ready yet); guard against a nil deref and a divide by zero.
  if (not FInited) or (FTimeToSample = nil) or (SamplesPerSecond = 0) then
    Exit(0);
  Result := FTimeToSample.TotalSamples /  SamplesPerSecond;
end;

function TPAM4ADecoderSource.CanSeek: Boolean;
begin
  Result := StreamCanSeek;
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

