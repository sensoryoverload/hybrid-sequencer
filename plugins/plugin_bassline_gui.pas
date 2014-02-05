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
{    FCutoff: TParameterControl;
    FReso: TParameterControl;
    FEnvMod: TParameterControl;
    FDecay: TParameterControl;
    FAccent: TParameterControl;
    FWave: TParameterControl;
    FOverDrive: TParameterControl;}
  protected
    procedure DoParameterChange(Sender: TObject);
    procedure DoParameterStartChange(Sender: TObject);
    function SetupParameterControls(ALeft, ATop: Integer; ACaption: string;
      AMin, AMax, AValue: Single; ATbParameter: TTbParameter): TParameterControl;
  public
    constructor Create(AOwner: TComponent); override;
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

{  FCutoff := SetupParameterControls(10, 30, 'Cutoff', 0, 1, 1, tbCutoff);
  FReso := SetupParameterControls(10, 50, 'Reso', 0, 1, 0, tbReso);
  FEnvMod := SetupParameterControls(10, 70, 'EnvMod', 0, 1, 0.5, tbEnvMod);
  FDecay := SetupParameterControls(10, 90, 'Decay', 0, 1, 0.5, tbDecay);
  FAccent := SetupParameterControls(10, 110, 'Accent', 0, 1, 0, tbAccent);
  FWave := SetupParameterControls(10, 130, 'Wave', 0, 1, 0, tbWave);
  FOverDrive := SetupParameterControls(10, 150, 'OverDrive', 1, 20, 1, tbOverdrive);}
end;

procedure TPluginBasslineGUI.Update(Subject: THybridPersistentModel);
begin
  if Subject is TPluginBassline then
  begin
   { FCutoff.Value := TPluginBassline(Subject).Cutoff;
    FReso.Value := TPluginBassline(Subject).Reso;
    FEnvMod.Value := TPluginBassline(Subject).EnvMod;
    FDecay.Value := TPluginBassline(Subject).Decay;
    FAccent.Value := TPluginBassline(Subject).Accent;
    FWave.Value := TPluginBassline(Subject).Wave;
    FOverDrive.Value := TPluginBassline(Subject).OverDrive;}
  end;
end;

end.

