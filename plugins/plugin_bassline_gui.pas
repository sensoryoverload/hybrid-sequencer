unit plugin_bassline_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dialcontrol, pluginnodegui, plugin_bassline, global_command,
  globalconst, tb303;

type

  { TPluginBasslineGUI }

  TPluginBasslineGUI = class(TGenericPluginGUI)
  private
    FPitch: TParameterControl;
    FCutoff: TParameterControl;
    FReso: TParameterControl;
    FEnvMod: TParameterControl;
    FDecay: TParameterControl;
    FAccent: TParameterControl;
  protected
    procedure DoParameterChange(Sender: TObject);
    procedure DoParameterStartChange(Sender: TObject);
    function SetupParameterControls(ALeft, ATop: Integer; ACaption: string;
      AMin, AMax, AValue: Single; ATbParameter: TTbParameter): TParameterControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); override;
  end;

implementation

procedure TPluginBasslineGUI.DoParameterChange(Sender: TObject);
var
  lGenericCommand: TBasslineParameterCommand;
begin
  lGenericCommand := TBasslineParameterCommand.Create(ObjectOwnerID);
  try
    lGenericCommand.Parameter := TTbParameter(TParameterControl(Sender).Tag);
    lGenericCommand.MidiLearn := TParameterControl(Sender).MidiMappingMode;
    lGenericCommand.ObjectID := Self.ObjectID;
    lGenericCommand.Value := TParameterControl(Sender).Value;
    lGenericCommand.Persist := False;

    GCommandQueue.PushCommand(lGenericCommand);
  except
    lGenericCommand.Free;
  end;
end;

procedure TPluginBasslineGUI.DoParameterStartChange(Sender: TObject);
var
  lGenericCommand: TBasslineParameterCommand;
begin
  lGenericCommand := TBasslineParameterCommand.Create(ObjectOwnerID);
  try
    lGenericCommand.Parameter := TTbParameter(TParameterControl(Sender).Tag);
    lGenericCommand.MidiLearn := TParameterControl(Sender).MidiMappingMode;
    lGenericCommand.ObjectID := Self.ObjectID;
    lGenericCommand.Value := TParameterControl(Sender).Value;
    lGenericCommand.Persist := True;

    GCommandQueue.PushCommand(lGenericCommand);
  except
    lGenericCommand.Free;
  end;
end;

function TPluginBasslineGUI.SetupParameterControls(
  ALeft,
  ATop: Integer;
  ACaption: string;
  AMin,
  AMax,
  AValue: Single;
  ATbParameter: TTbParameter): TParameterControl;
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
  Result.Tag := Integer(ATbParameter);
  Result.OnChange := @DoParameterChange;
  Result.OnStartChange := @DoParameterStartChange;
end;

constructor TPluginBasslineGUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPitch := SetupParameterControls(10, 60, 'Pitch', 0, 1, 0.5, tbPitch);
  FCutoff := SetupParameterControls(10, 80, 'Cutoff', 0, 1, 1, tbCutoff);
  FReso := SetupParameterControls(10, 100, 'Reso', 0, 1, 0, tbReso);
  FEnvMod := SetupParameterControls(10, 120, 'EnvMod', 0, 1, 0.5, tbEnvMod);
  FDecay := SetupParameterControls(10, 140, 'Decay', 0, 1, 0.5, tbDecay);
  FAccent := SetupParameterControls(10, 160, 'Accent', 0, 1, 0, tbAccent);
end;

destructor TPluginBasslineGUI.Destroy;
begin
  inherited Destroy;
end;

procedure TPluginBasslineGUI.Update(Subject: THybridPersistentModel);
begin
  if Subject is TPluginBassline then
  begin
    FPitch.Value := TPluginBassline(Subject).Pitch;
    FCutoff.Value := TPluginBassline(Subject).Cutoff;
    FReso.Value := TPluginBassline(Subject).Reso;
    FEnvMod.Value := TPluginBassline(Subject).EnvMod;
    FDecay.Value := TPluginBassline(Subject).Decay;
    FAccent.Value := TPluginBassline(Subject).Accent;
  end;
end;

end.

