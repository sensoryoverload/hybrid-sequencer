unit plugin_decimate_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dialcontrol, pluginnodegui, plugin_decimate, global_command,
  globalconst, global;

type

  { TPluginDecimateGUI }

  TPluginDecimateGUI = class(TGenericPluginGUI)
  private
    FBits: TParameterControl;
    FSampleRate: TParameterControl;
    function SetupParameterControls(ALeft, ATop: Integer; ACaption: string;
      AMin, AMax, AValue: Single; AParameter: TDecimateParameter): TParameterControl;
  protected
    procedure DoParameterChange(Sender: TObject);
    procedure DoParameterStartChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); override;
  end;

implementation

function TPluginDecimateGUI.SetupParameterControls(
  ALeft,
  ATop: Integer;
  ACaption: string;
  AMin,
  AMax,
  AValue: Single;
  AParameter: TDecimateParameter): TParameterControl;
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

constructor TPluginDecimateGUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBits := SetupParameterControls(10, 10, 'Bits', 1, 16, 16, dpBits);
  FSampleRate := SetupParameterControls(10, 30, 'Samplerate', 1,
    GSettings.SampleRate, GSettings.SampleRate, dpSampleRate);
end;

destructor TPluginDecimateGUI.Destroy;
begin
  inherited Destroy;
end;

procedure TPluginDecimateGUI.Update(Subject: THybridPersistentModel);
begin
  if Subject is TPluginDecimate then
  begin
    FBits.Value := TPluginDecimate(Subject).Bits;
    FSampleRate.Value := TPluginDecimate(Subject).SampleRate;
  end;
end;

procedure TPluginDecimateGUI.DoParameterStartChange(Sender: TObject);
var
  lGenericCommand: TDecimateParameterCommand;
begin
  lGenericCommand := TDecimateParameterCommand.Create(Self.ObjectID);
  try
    lGenericCommand.Parameter := TDecimateParameter(TParameterControl(Sender).Tag);
    lGenericCommand.MidiLearn := TParameterControl(Sender).MidiMappingMode;
    lGenericCommand.ObjectID := Self.ObjectID;
    lGenericCommand.Value := TParameterControl(Sender).Value;
    lGenericCommand.Persist := True;

    GCommandQueue.PushCommand(lGenericCommand);
  except
    lGenericCommand.Free;
  end;
end;

procedure TPluginDecimateGUI.DoParameterChange(Sender: TObject);
var
  lGenericCommand: TDecimateParameterCommand;
begin
  lGenericCommand := TDecimateParameterCommand.Create(Self.ObjectID);
  try
    lGenericCommand.Parameter := TDecimateParameter(TParameterControl(Sender).Tag);
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

