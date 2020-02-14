{
    This unit is part of the PascalAudio project.

    Copyright (c) 2020 by Andrew Haines.

    See the files COPYING.modifiedLGPL and license.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit mp4codec_mp4a;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mp4codec;

type

  { Tmp4aCodec }

  Tmp4aCodec = class(TMP4Codec)
    procedure Filter(AData: PByte; ASize: Integer); override;

  end;

implementation

{ Tmp4aCodec }

procedure Tmp4aCodec.Filter(AData: PByte; ASize: Integer);
begin
  // do nothing atm. maybe decode data here
end;

initialization
  MP4RegisterCodec(Tmp4aCodec, 'mp4a');

end.

