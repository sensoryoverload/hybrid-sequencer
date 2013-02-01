unit plugin_freeverb_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dialcontrol, pluginnodegui, plugin_freeverb, global_command,
  globalconst, freereverb;

type

  { TPluginFreeverbGUI }

  TPluginFreeverbGUI = class(TGenericPluginGUI)
  private
    FRoomSize: TParameterControl;
    FDamp: TParameterControl;
    FWidth: TParameterControl;
    FMode: TParameterControl;
    FDry: TParameterControl;
    FWet: TParameterControl;
    FPreDelay: TParameterControl;
  protected
    procedure DoParameterChange(Sender: TObject);
    procedure DoParameterStartChange(Sender: TObject);
    function SetupParameterControls(ALeft, ATop: Integer; ACaption: string;
      AMin, AMax, AValue: Single; AReverbParameter: TReverbParameter): TParameterControl;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update(Subject: THybridPersistentModel); override;
  end;

implementation

uses
  global;

procedure TPluginFreeverbGUI.DoParameterChange(Sender: TObject);
var
  lGenericCommand: TFreeverbParameterCommand;
begin
  lGenericCommand := TFreeverbParameterCommand.Create(ObjectOwnerID);
  try
    lGenericCommand.Parameter := TReverbParameter(TParameterControl(Sender).Tag);
    lGenericCommand.MidiLearn := TParameterControl(Sender).MidiMappingMode;
    lGenericCommand.ObjectID := Self.ObjectID;
    lGenericCommand.Value := TParameterControl(Sender).Value;
    lGenericCommand.Persist := False;

    GCommandQueue.PushCommand(lGenericCommand);
  except
    lGenericCommand.Free;
  end;
end;

procedure TPluginFreeverbGUI.DoParameterStartChange(Sender: TObject);
var
  lGenericCommand: TFreeverbParameterCommand;
begin
  lGenericCommand := TFreeverbParameterCommand.Create(ObjectOwnerID);
  try
    lGenericCommand.Parameter := TReverbParameter(TParameterControl(Sender).Tag);
    lGenericCommand.MidiLearn := TParameterControl(Sender).MidiMappingMode;
    lGenericCommand.ObjectID := Self.ObjectID;
    lGenericCommand.Value := TParameterControl(Sender).Value;
    lGenericCommand.Persist := True;

    GCommandQueue.PushCommand(lGenericCommand);
  except
    lGenericCommand.Free;
  end;
end;

function TPluginFreeverbGUI.SetupParameterControls(
  ALeft,
  ATop: Integer;
  ACaption: string;
  AMin,
  AMax,
  AValue: Single;
  AReverbParameter: TReverbParameter): TParameterControl;
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
  Result.Tag := Integer(AReverbParameter);
  Result.OnChange := @DoParameterChange;
  Result.OnStartChange := @DoParameterStartChange;
end;

constructor TPluginFreeverbGUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRoomSize := SetupParameterControls(10, 10, 'roomsize', 0, 1, initialroom, rpRoomSize);
  FDamp := SetupParameterControls(10, 30, 'damp', 0, 1, initialdamp, rpDamp);
  FWidth := SetupParameterControls(10, 50, 'width', 0, 1, initialwidth, rpWidth);
  FMode := SetupParameterControls(10, 70, 'mode', 0, 1, initialmode, rpMode);
  FDry := SetupParameterControls(10, 90, 'dry', 0, 1, initialdry, rpDry);
  FWet := SetupParameterControls(10, 110, 'wet', 0, 1, initialwet, rpWet);
  FPreDelay := SetupParameterControls(10, 130, 'pre-delay', 0, GSettings.SampleRate / 2, 0, rpPreDelay);
end;

procedure TPluginFreeverbGUI.Update(Subject: THybridPersistentModel);
begin
  if Subject is TPluginFreeverb then
  begin
    FRoomSize.Value := TPluginFreeverb(Subject).RoomSize;
    FDamp.Value := TPluginFreeverb(Subject).Damp;
    FWidth.Value := TPluginFreeverb(Subject).Width;
    FMode.Value := TPluginFreeverb(Subject).Mode;
    FDry.Value := TPluginFreeverb(Subject).Dry;
    FWet.Value := TPluginFreeverb(Subject).Wet;
    FPreDelay.Value := TPluginFreeverb(Subject).PreDelay;
  end;
end;

end.

