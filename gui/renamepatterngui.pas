unit renamepatterngui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TFmRenamePattern }

  TFmRenamePattern = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    edtPatternName: TEdit;
  private
    function GetPatternName: string;
    procedure SetPatternName(AValue: string);
    { private declarations }
  public
    { public declarations }
    property PatternName: string read GetPatternName write SetPatternName;
  end;

var
  FmRenamePattern: TFmRenamePattern;

implementation

{ TFmRenamePattern }

function TFmRenamePattern.GetPatternName: string;
begin
  Result := edtPatternName.Text;
end;

procedure TFmRenamePattern.SetPatternName(AValue: string);
begin
  edtPatternName.Text := AValue;
end;

initialization
  {$I renamepatterngui.lrs}

end.

