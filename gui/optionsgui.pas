unit optionsgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, global;

type

  { TfmOptions }

  TfmOptions = class(TForm)
    btnCancel: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    edtCurrentSampleMap: TLabeledEdit;
    Panel1: TPanel;
    procedure Button2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSettings: TSettings;
    procedure SetSettings(const AValue: TSettings);
    function SettingsChanged: Boolean;
    { private declarations }
  public
    { public declarations }
    property Settings: TSettings read FSettings write SetSettings;
  end; 

var
  fmOptions: TfmOptions;

implementation

{ TfmOptions }

{
  Make sure when the form closes, the settings are saved on OK or discarded on Cancel
}
procedure TfmOptions.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  case ModalResult of
    mrOK:
    begin
      //
    end;
    mrCancel:
    begin
      //
    end
  end;
end;

{
  Fetch all settings and set gui counterparts
}
procedure TfmOptions.FormCreate(Sender: TObject);
begin
  //
end;

procedure TfmOptions.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TfmOptions.FormShow(Sender: TObject);
begin
  if not Assigned(FSettings) then
  begin
    raise Exception.Create('Settings parameter not set');
    exit;
  end;
end;

procedure TfmOptions.SetSettings(const AValue: TSettings);
begin
  if FSettings = AValue then exit;
  FSettings := AValue;

  // Set all component values
  edtCurrentSampleMap.Text := FSettings.SampleMap;
end;

function TfmOptions.SettingsChanged: Boolean;
begin
  Result := (edtCurrentSampleMap.Text <> FSettings.SampleMap);
end;

procedure TfmOptions.Button2Click(Sender: TObject);
begin
  //
end;

initialization
  {$I optionsgui.lrs}

end.

