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

  TPAM4ADecoderSource = class(TPAStreamSource, {IPAPlayable,} IPAStream)
  private
    FInited: Boolean;
    FDecoder: TAACDecoder;
    FContainer: TQuicktimeContainer;
    FSampleIndex: Integer;
    FSampleTable: array of LongWord;
    FChunkOffset: array of LongWord;
    FChunkTable: TstscAtom;
    FFrame: TNeAACDecFrameInfo;
    FBuffer: array[0..1023] of Byte;
    FSampleSize: Integer;
    FCodec: TMP4Codec;
    function ReadNextSample: Boolean;
    procedure InitAudio;
    procedure DeInitAudio;
  protected
    procedure SetStream(AValue: TStream); override;
    function InternalOutputToDestination: Boolean; override;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean=True); override;
  end;

implementation

{ TPAM4ADecoderSource }

function TPAM4ADecoderSource.ReadNextSample: Boolean;
var
  i: Integer;
  lFirstIndex, lChunkIndex: DWord;
  lSamplesOffset: DWord = 0;
begin
  Result := FSampleIndex < Length(FSampleTable);
  if not Result then
    Exit;
  Writeln('Reading Index: ', FSampleIndex);
  FSampleSize := FSampleTable[FSampleIndex];

  lChunkIndex := FChunkTable.SampleIndexToChunkIndex(1, FSampleIndex, lFirstIndex);

  for i := lFirstIndex to FSampleIndex-1 do
    Inc(lSamplesOffset, FSampleTable[i]);

  FContainer.Stream.Position:=FChunkOffset[lChunkIndex] + lSamplesOffset;
  FContainer.Stream.Read(FBuffer[0], FSampleSize);

  if Assigned(FCodec) then
    FCodec.Filter(@FBuffer[0], FSampleSize);
  Inc(FSampleIndex);
  Write(Pred(FSampleIndex), ': ');
  {for i := 0 to 7 do
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

  lSampleTable := TstszAtom(FContainer.Atoms.FindAtom('moov/trak/mdia/minf/stbl/stsz'));
  SetLength(FSampleTable, lSampleTable.SampleCount);
  Writeln(Length(FSampleTable), ' samples');
  lSampleTable.CopySampleTable(@FSampleTable[0]);

  lSampleOffSet := TstcoAtom(FContainer.Atoms.FindAtom('moov/trak/mdia/minf/stbl/stco'));
  SetLength(FChunkOffset, lSampleOffSet.Count);
  lSampleOffSet.CopyTable(@FChunkOffset[0]);

  lstsdAtom := TstsdAtom(FContainer.Atoms.FindAtom('moov/trak/mdia/minf/stbl/stsd'));


  lmp4aAtom := TSoundSampleDecriptionAtom(lstsdAtom.Atoms.Atom[0]);
  WriteLn(lmp4aAtom.ClassName);
  WriteLn(lmp4aAtom.AtomName.Chars);

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
  WriteLn(FDecoder.GetErroMessage(FDecoder.LastResult));;
  lDecoderConfig^.outputFormat:=ord(FAAD_FMT_FLOAT);
  //lDecoderConfig^.downMatrix:=0;
  //lDecoderConfig^.defObjectType:=Ord(otLC);
  //lDecoderConfig^.defSampleRate:=Trunc(lmp4aAtom.SampleRate);
  //lDecoderConfig^.dontUpSampleImplicitSBR:= False;
  FDecoder.Config := lDecoderConfig;

  FDecoder.Init2(@lConfig, 2);

  FDecoder.AudioSpecificConfig(@lConfig, 2, @lmp4ASC);

  WriteLn(FDecoder.Channels);
  Channels:=FDecoder.Channels;
  SamplesPerSecond:=FDecoder.SampleRate;

end;

procedure TPAM4ADecoderSource.DeInitAudio;
begin
  if not FInited then
    Exit;
  FInited:=False;
  if Assigned(FContainer) then
  begin
    Freeandnil(FContainer);
    FreeAndNil(FDecoder);
  end;
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
begin
  if not FInited then
    InitAudio;


  Result := ReadNextSample;

  if Result then
  begin
    lRes := FDecoder.Decode(@FFrame, @FBuffer[0], FSampleSize);
    if FFrame.error <> 0 then
      WriteLn('Error: ', FDecoder.GetErroMessage(FFrame.error));

    if (FFrame.samples > 0) then
    begin
      WriteToBuffer(lRes^,FFrame.samples*4, False);
    end;

    WriteLn(SysUtils.Format('Dec Bytes: %d, data := 0x%s Samples: %d', [FFrame.bytesconsumed, Hexstr(PtrUint(lRes), 16), FFrame.samples]));
    //if FFrame.;
    if (FFrame.error > 0) or (FFrame.bytesconsumed > 0) or (lRes <> nil) then
    begin

     writeln;
    end;
  end;

  //sleep(1000);





end;

constructor TPAM4ADecoderSource.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create(AStream, AOwnsStream);
  Format:=afFloat32;
end;























end.

