unit plugin_freeverb_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dialcontrol, pluginnodegui, plugin_freeverb, global_command,
  globalconst;

type

  { TPluginFreeverbGUI }

  TPluginFreeverbGUI = class(TGenericPluginGUI)
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

constructor TPluginFreeverbGUI.Create(AOwner: TComponent);
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

destructor TPluginFreeverbGUI.Destroy;
begin
  inherited Destroy;
end;

procedure TPluginFreeverbGUI.Update(Subject: THybridPersistentModel);
begin
  if Subject is TPluginFreeverb then
  begin
    FBits.Value := TPluginFreeverb(Subject).Drive;
  end;
end;

procedure TPluginFreeverbGUI.BitsStartChange(Sender: TObject);
var
  lFreeverbCommand: TFreeverbCommand;
begin
  lFreeverbCommand := TFreeverbCommand.Create(Self.ObjectID);
  try
    lFreeverbCommand.Drive := FBits.Value;
    lFreeverbCommand.Persist := True;

    GCommandQueue.PushCommand(lFreeverbCommand);
  except
    lFreeverbCommand.Free;
  end;
end;

procedure TPluginFreeverbGUI.BitsChange(Sender: TObject);
var
  lFreeverbCommand: TFreeverbCommand;
begin
  lFreeverbCommand := TFreeverbCommand.Create(Self.ObjectID);
  try
    lFreeverbCommand.Drive := FBits.Value;
    lFreeverbCommand.Persist := False;

    GCommandQueue.PushCommand(lFreeverbCommand);
  except
    lFreeverbCommand.Free;
  end;
end;

end.

