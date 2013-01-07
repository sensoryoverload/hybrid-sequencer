unit plugin_decimate_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dialcontrol, pluginnodegui, plugin_decimate, global_command,
  globalconst;

type

  { TPluginDecimateGUI }

  TPluginDecimateGUI = class(TGenericPluginGUI)
  private
    FBits: TParameterControl;
  protected
    procedure BitsChange(Sender: TObject);
    procedure BitsStartChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); override;
  end;

implementation

constructor TPluginDecimateGUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBits := TParameterControl.Create(Self);
  FBits.Left := 10;
  FBits.Top := 10;
  FBits.Width := 60;
  FBits.Height := 10;
  FBits.Caption := 'Drive';
  FBits.Min := 1;
  FBits.Max := 10;
  FBits.Value := 1;
  FBits.Parent := Self;
  FBits.OnChange := @BitsChange;
  FBits.OnStartChange := @BitsStartChange;

  FBits := TParameterControl.Create(Self);
  FBits.Left := 10;
  FBits.Top := 10;
  FBits.Width := 60;
  FBits.Height := 10;
  FBits.Caption := 'Drive';
  FBits.Min := 1;
  FBits.Max := 10;
  FBits.Value := 1;
  FBits.Parent := Self;
  FBits.OnChange := @BitsChange;
  FBits.OnStartChange := @BitsStartChange;
end;

destructor TPluginDecimateGUI.Destroy;
begin
  inherited Destroy;
end;

procedure TPluginDecimateGUI.Update(Subject: THybridPersistentModel);
begin
  if Subject is TPluginDecimate then
  begin
    FBits.Value := TPluginDecimate(Subject).Drive;
  end;
end;

procedure TPluginDecimateGUI.BitsStartChange(Sender: TObject);
var
  lDecimateCommand: TDecimateCommand;
begin
  lDecimateCommand := TDecimateCommand.Create(Self.ObjectID);
  try
    lDecimateCommand.Drive := FBits.Value;
    lDecimateCommand.Persist := True;

    GCommandQueue.PushCommand(lDecimateCommand);
  except
    lDecimateCommand.Free;
  end;
end;

procedure TPluginDecimateGUI.BitsChange(Sender: TObject);
var
  lDecimateCommand: TDecimateCommand;
begin
  lDecimateCommand := TDecimateCommand.Create(Self.ObjectID);
  try
    lDecimateCommand.Drive := FBits.Value;
    lDecimateCommand.Persist := False;

    GCommandQueue.PushCommand(lDecimateCommand);
  except
    lDecimateCommand.Free;
  end;
end;

end.

