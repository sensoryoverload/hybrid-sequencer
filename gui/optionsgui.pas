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
    btnSelectFolder: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    edtCurrentSampleMap: TLabeledEdit;
    Panel1: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure btnSelectFolderClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSettings: TSettings;
    procedure SetSettings(const AValue: TSettings);
    function SettingsChanged: Boolean;
    procedure WriteChanges;
    { private declarations }
  public
    { public declarations }
    property Settings: TSettings read FSettings write SetSettings;
  end; 

var
  fmOptions: TfmOptions;

implementation

{ TfmOptions }

procedure TfmOptions.WriteChanges;
begin
  if FSettings.SampleMap <> edtCurrentSampleMap.Text then
  begin
    FSettings.SampleMap := edtCurrentSampleMap.Text;
  end;
end;

{
  Make sure when the form closes, the settings are saved on OK or discarded on Cancel
}
procedure TfmOptions.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  case ModalResult of
    mrOK:
    begin
      WriteChanges;
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

procedure TfmOptions.btnSelectFolderClick(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
  begin
    edtCurrentSampleMap.Text := ExtractFilePath(SelectDirectoryDialog1.FileName);
  end;
end;

initialization
  {$I optionsgui.lrs}

end.

