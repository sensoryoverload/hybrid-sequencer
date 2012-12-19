unit plugin_distortion_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dialcontrol, pluginnodegui, plugin_distortion, global_command,
  globalconst;

type

  { TPluginDistortionGUI }

  TPluginDistortionGUI = class(TGenericPluginGUI)
  private
    FDrive: TParameterControl;
  protected
    procedure DriveChange(Sender: TObject);
    procedure DriveStartChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); override;
  end;

implementation

constructor TPluginDistortionGUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDrive := TParameterControl.Create(Self);
  FDrive.Left := 10;
  FDrive.Top := 10;
  FDrive.Width := 60;
  FDrive.Height := 10;
  FDrive.Caption := 'Drive';
  FDrive.Min := 1;
  FDrive.Max := 10;
  FDrive.Value := 1;
  FDrive.Parent := Self;
  FDrive.OnChange := @DriveChange;
  FDrive.OnStartChange := @DriveStartChange;
end;

destructor TPluginDistortionGUI.Destroy;
begin
  inherited Destroy;
end;

procedure TPluginDistortionGUI.Update(Subject: THybridPersistentModel);
begin
  if Subject is TPluginDistortion then
  begin
    FDrive.Value := TPluginDistortion(Subject).Drive;
  end;
end;

procedure TPluginDistortionGUI.DriveStartChange(Sender: TObject);
var
  lDistortionCommand: TDistortionCommand;
begin
  lDistortionCommand := TDistortionCommand.Create(Self.ObjectID);
  try
    lDistortionCommand.Drive := FDrive.Value;
    lDistortionCommand.Persist := True;

    GCommandQueue.PushCommand(lDistortionCommand);
  except
    lDistortionCommand.Free;
  end;
end;

procedure TPluginDistortionGUI.DriveChange(Sender: TObject);
var
  lDistortionCommand: TDistortionCommand;
begin
  lDistortionCommand := TDistortionCommand.Create(Self.ObjectID);
  try
    lDistortionCommand.Drive := FDrive.Value;
    lDistortionCommand.Persist := False;

    GCommandQueue.PushCommand(lDistortionCommand);
  except
    lDistortionCommand.Free;
  end;
end;

end.

