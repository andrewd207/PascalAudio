unit paio_vorbis_comment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TVorbisComments }

  TVorbisComments = class
  private
    FUserComments: TStrings;
    FVendor: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  published
    property Vendor: String read FVendor write FVendor;
    property UserComments: TStrings read FUserComments;
  end;

implementation

{ TVorbisComments }

constructor TVorbisComments.Create;
begin
  FUserComments := TStringList.Create;
end;

destructor TVorbisComments.Destroy;
begin
  FUserComments.Free;
  inherited Destroy;
end;

procedure TVorbisComments.SaveToStream(AStream: TStream);
var
  s: String;
begin
  AStream.WriteDWord(NtoLE(DWord(Length(FVendor))));
  AStream.Write(FVendor[1], Length(FVendor));
  AStream.WriteDWord(NtoLE(DWord(FUserComments.Count)));
  for s in FUserComments do
  begin
    AStream.WriteDWord(NtoLE(DWord(Length(s))));
    AStream.Write(s[1], Length(s));
  end;
end;

procedure TVorbisComments.LoadFromStream(AStream: TStream);
var
  lLength, lCount: DWord;
  lItem: String;
begin
  lLength := LEtoN(AStream.ReadDWord);
  SetLength(FVendor, lLength);
  AStream.Read(FVendor[1], lLength);

  lCount := LEtoN(AStream.ReadDWord);
  while lCount > 0 do
  begin
    lLength := LEtoN(AStream.ReadDWord);
    SetLength(lItem, lLength);
    AStream.Read(lItem[1], lLength);
    FUserComments.Add(lItem);
    Dec(lCount);
  end;
  // possibly a framing bit after that but we don't try to read it here.
end;

end.

