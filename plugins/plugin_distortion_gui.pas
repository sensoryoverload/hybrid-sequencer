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
    function SetupParameterControls(ALeft, ATop: Integer; ACaption: string;
      AMin, AMax, AValue: Single; AParameter: TDistortionParameter): TParameterControl;
  protected
    procedure DoParameterChange(Sender: TObject);
    procedure DoParameterStartChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); override;
  end;

implementation

function TPluginDistortionGUI.SetupParameterControls(
  ALeft,
  ATop: Integer;
  ACaption: string;
  AMin,
  AMax,
  AValue: Single;
  AParameter: TDistortionParameter): TParameterControl;
begin
  Result := TParameterControl.Create(Self);
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Width := 60;
  Result.Height := 10;
  Result.Caption := ACaption;
  Result.Min := AMin;
  Result.Max := AMax;
  Result.Value := AValue;
  Result.Parent := Self;
  Result.Tag := Integer(AParameter);
  Result.OnChange := @DoParameterChange;
  Result.OnStartChange := @DoParameterStartChange;
end;

constructor TPluginDistortionGUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDrive := SetupParameterControls(10, 10, 'Drive', 2, 10, 2, dpDrive);
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

procedure TPluginDistortionGUI.DoParameterStartChange(Sender: TObject);
var
  lGenericCommand: TDistortionCommand;
begin
  lGenericCommand := TDistortionCommand.Create(Self.ObjectID);
  try
    lGenericCommand.Parameter := TDistortionParameter(TParameterControl(Sender).Tag);
    lGenericCommand.MidiLearn := TParameterControl(Sender).MidiMappingMode;
    lGenericCommand.ObjectID := Self.ObjectID;
    lGenericCommand.Value := TParameterControl(Sender).Value;
    lGenericCommand.Persist := True;

    GCommandQueue.PushCommand(lGenericCommand);
  except
    lGenericCommand.Free;
  end;
end;

procedure TPluginDistortionGUI.DoParameterChange(Sender: TObject);
var
  lGenericCommand: TDistortionCommand;
begin
  lGenericCommand := TDistortionCommand.Create(Self.ObjectID);
  try
    lGenericCommand.Parameter := TDistortionParameter(TParameterControl(Sender).Tag);
    lGenericCommand.MidiLearn := TParameterControl(Sender).MidiMappingMode;
    lGenericCommand.ObjectID := Self.ObjectID;
    lGenericCommand.Value := TParameterControl(Sender).Value;
    lGenericCommand.Persist := False;

    GCommandQueue.PushCommand(lGenericCommand);
  except
    lGenericCommand.Free;
  end;
end;

end.

