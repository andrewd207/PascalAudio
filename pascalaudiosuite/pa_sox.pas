{
    This unit is part of PascalAudioSuite package.

    Copyright (c) 2016 by Andrew Haines.

    See the files COPYING.modifiedLGPL and LICENSES.txt, included in this
    distribution, for details about the license.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit pa_sox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pa_base, pa_process;

type

  { TPASoxSource }

  TPASoxSource = class(TPAProcessSource)
  private
    FFileName: String;
    procedure SetFileName(AValue: String);
  protected
    procedure BeforeExecuteLoop; override;
  public
    constructor Create; override;
    property FileName: String read FFileName write SetFileName;
  end;

implementation

{ TPASoxSource }

procedure TPASoxSource.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
end;

procedure TPASoxSource.BeforeExecuteLoop;
begin
  FProcess.Parameters.AddStrings([FFileName, '-t', 'raw', '--bits', '32', '--encoding', 'float']);
  FProcess.Parameters.AddStrings(['-r', IntToStr(SamplesPerSecond), '-c', IntToStr(Channels), '-']);
  inherited BeforeExecuteLoop;
end;

constructor TPASoxSource.Create;
begin
  inherited Create;
  FProcess.Executable:='sox';
  Format:=afFloat32;
end;

end.

