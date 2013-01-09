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
  protected
    procedure DoParameterChange(Sender: TObject);
    procedure DoParameterStartChange(Sender: TObject);
    function SetupParameterControls(ALeft, ATop: Integer; ACaption: string;
      AMin, AMax, AValue: Single; AReverbParameter: TReverbParameter): TParameterControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); override;
  end;

implementation

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

  FRoomSize := SetupParameterControls(10, 60, 'Roomsize', 0, 1, initialroom, rpRoomSize);
  FDamp := SetupParameterControls(10, 80, 'Damp', 0, 1, initialdamp, rpDamp);
  FWidth := SetupParameterControls(10, 100, 'Width', 0, 1, initialwidth, rpWidth);
  FMode := SetupParameterControls(10, 120, 'Mode', 0, 1, initialmode, rpMode);
  FDry := SetupParameterControls(10, 140, 'Dry', 0, 1, initialdry, rpDry);
  FWet := SetupParameterControls(10, 160, 'Wet', 0, 1, initialwet, rpWet);
end;

destructor TPluginFreeverbGUI.Destroy;
begin
  inherited Destroy;
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
  end;
end;

end.

