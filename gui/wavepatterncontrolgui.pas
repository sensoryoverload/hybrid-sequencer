unit wavepatterncontrolgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, StdCtrls, Spin,
  ExtCtrls, ComCtrls, globalconst, dialcontrol, wavegui, pattern, wave;

type

  { TWavePatternControlGUI }

  TWavePatternControlGUI = class(TFrame, IObserver)
    btnDouble: TButton;
    btnHalf: TButton;
    cbPitchAlgo: TComboBox;
    edtFilename: TLabeledEdit;
    fspnPitch: TFloatSpinEdit;
    gbAudioTrackSettings: TGroupBox;
    Label1: TLabel;
    lblPitch: TLabel;
    spnBeatsPerMinute: TSpinEdit;
    tbThreshold: TTrackBar;
    ToggleControl1: TToggleControl;
    TrackBar1: TTrackBar;

    procedure btnDoubleClick(Sender: TObject);
    procedure btnHalfClick(Sender: TObject);
    procedure cbPitchAlgoChange(Sender: TObject);
    procedure tbLatencyChange(Sender: TObject);
    procedure tbThresholdMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TrackBar1Change(Sender: TObject);
    procedure LoopMetricChange(Sender: TObject);
    procedure cbPitchedChange(Sender: TObject);
    procedure spnBeatsPerMinuteChange(Sender: TObject);
    procedure spnPitchChange(Sender: TObject);
  private
    { private declarations }
    FModel: TWavePattern;
    FWaveGUI: TWaveGUI;

    FConnected: Boolean;
    FObjectOwnerID: string;
    FObjectID: string;
    FObjectOwner: TObject;

    FPitched: Boolean;
    FPitch: Single;
    FRealBPM: Single;
    procedure Setpitch(const Avalue: Single);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;
    procedure Update(Subject: THybridPersistentModel); reintroduce;

    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string; virtual;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);

    property Connected: Boolean read FConnected;
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property ObjectID: string read GetObjectID write SetObjectID;
    property WaveGUI: TWaveGUI read FWaveGUI write FWaveGUI;

    property Pitch: Single read FPitch write SetPitch default 1;
    property Pitched: Boolean read FPitched write FPitched default False;
    property RealBPM: Single read FRealBPM write FRealBPM default 120;

//    property Model: TObject read FModelObject write FModelObject;
    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
    function GetModel: THybridPersistentModel;
    procedure SetModel(AModel: THybridPersistentModel);
    property Model: THybridPersistentModel read GetModel write SetModel;
  end;

implementation

uses utils, global_command, global;

{ TWavePatternControlGUI }

procedure TWavePatternControlGUI.Connect;
begin
  DBLog(Format('Start Connect waveform (%s)', [FModel.ObjectID]));
  if Assigned(FModel) then
  begin
    FModel.Attach(FWaveGUI);

    FConnected := True;
  end;
  DBLog(Format('End Connect waveform (%s)', [FModel.ObjectID]));
end;

procedure TWavePatternControlGUI.Disconnect;
begin
  DBLog(Format('Start Disconnect waveform (%s)', [FModel.ObjectID]));
  if Assigned(FModel) then
  begin
    FWaveGUI.Disconnect;
    FModel.Detach(FWaveGUI);

    FConnected := False;
  end;
  DBLog(Format('End Disconnect waveform (%s)', [FModel.ObjectID]));
end;

procedure TWavePatternControlGUI.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TWavePatternControl.Update');

  if spnBeatsPerMinute.Value <> TWavePattern(Subject).RealBPM then
  begin
    spnBeatsPerMinute.Value := TWavePattern(Subject).RealBPM;
  end;

  if cbPitchAlgo.ItemIndex <> Integer(TWavePattern(Subject).PitchAlgorithm) then
  begin
    cbPitchAlgo.ItemIndex := Integer(TWavePattern(Subject).PitchAlgorithm);
  end;

  DBLog('end TWavePatternControl.Update');
end;

function TWavePatternControlGUI.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TWavePatternControlGUI.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

function TWavePatternControlGUI.GetObjectOwnerID: string;
begin
  Result := FObjectOwnerID;
end;

procedure TWavePatternControlGUI.SetObjectOwnerID(const AObjectOwnerID: string
  );
begin
  FObjectOwnerID := AObjectOwnerID;
end;

function TWavePatternControlGUI.GetModel: THybridPersistentModel;
begin
  Result := THybridPersistentModel(FModel);
end;

procedure TWavePatternControlGUI.SetModel(AModel: THybridPersistentModel);
begin
  FModel := TWavePattern(AModel);
end;

procedure TWavePatternControlGUI.spnBeatsPerMinuteChange(Sender: TObject);
var
  lChangeRealBPMCommand: TChangeRealBPMCommand;
begin
  lChangeRealBPMCommand := TChangeRealBPMCommand.Create(FObjectID);
  try
//    lChangeRealBPMCommand.RealBPM := spnBeatsPerMinute.Value;

    GCommandQueue.PushCommand(lChangeRealBPMCommand);
  except
    lChangeRealBPMCommand.Free;
  end;
end;

procedure TWavePatternControlGUI.cbPitchedChange(Sender: TObject);
var
  lTogglePitchCommand: TTogglePitchCommand;
begin
  lTogglePitchCommand := TTogglePitchCommand.Create(FObjectID);
  try
//    lTogglePitchCommand.State := cbPitched.Checked;

    GCommandQueue.PushCommand(lTogglePitchCommand);
  except
    lTogglePitchCommand.Free;
  end;
end;

procedure TWavePatternControlGUI.spnPitchChange(Sender: TObject);
var
  lChangePitchCommand: TChangePitchCommand;
begin
  lChangePitchCommand := TChangePitchCommand.Create(FObjectID);
  try
    lChangePitchCommand.Pitch := fspnPitch.Value;

    GCommandQueue.PushCommand(lChangePitchCommand);
  except
    lChangePitchCommand.Free;
  end;
end;

procedure TWavePatternControlGUI.TrackBar1Change(Sender: TObject);
begin
  if Assigned(GSettings.SelectedPattern) then
  begin
    FWaveGUI.ZoomFactorX := TrackBar1.Position;
    FWaveGUI.CacheIsDirty := True;
    FWaveGUI.Invalidate;
  end;
end;

procedure TWavePatternControlGUI.tbThresholdMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lChangeThresholdCommand: TChangeThresHoldCommand;
begin
  lChangeThresholdCommand := TChangeThresHoldCommand.Create(FObjectID);
  try
    lChangeThresholdCommand.Threshold :=
      Round((100 * tbThreshold.Position) / tbThreshold.Max);

    GCommandQueue.PushCommand(lChangeThresholdCommand);
  except
    lChangeThresholdCommand.Free;
  end;
end;

procedure TWavePatternControlGUI.cbPitchAlgoChange(Sender: TObject);
var
  lChangeStretchAlgoCommand: TChangeStretchAlgoCommand;
begin
  lChangeStretchAlgoCommand := TChangeStretchAlgoCommand.Create(FObjectID);
  try
    lChangeStretchAlgoCommand.PitchAlgorithm := TPitchAlgorithm(cbPitchAlgo.ItemIndex);

    GCommandQueue.PushCommand(lChangeStretchAlgoCommand);
  except
    lChangeStretchAlgoCommand.Free;
  end;
end;

procedure TWavePatternControlGUI.tbLatencyChange(Sender: TObject);
var
  FChangeLatencyCommand: TChangeLatencyCommand;
begin
  // Double the length of the loop
  FChangeLatencyCommand := TChangeLatencyCommand.Create(FObjectID);
  try
    //FChangeLatencyCommand.Latency := tbLatency.Position;

    GCommandQueue.PushCommand(FChangeLatencyCommand);
  except
    FChangeLatencyCommand.Free;
  end;
end;

procedure TWavePatternControlGUI.btnDoubleClick(Sender: TObject);
var
  FDoubleLoopLengthCommand: TDoubleLoopLengthCommand;
begin
  // Double the length of the loop
  FDoubleLoopLengthCommand := TDoubleLoopLengthCommand.Create(FObjectID);
  try
    GCommandQueue.PushCommand(FDoubleLoopLengthCommand);
  except
    FDoubleLoopLengthCommand.Free;
  end;
end;

procedure TWavePatternControlGUI.btnHalfClick(Sender: TObject);
var
  FHalveLoopLengthCommand: THalveLoopLengthCommand;
begin
  // Halve the length of the loop
  FHalveLoopLengthCommand := THalveLoopLengthCommand.Create(FObjectID);
  try
    GCommandQueue.PushCommand(FHalveLoopLengthCommand);
  except
    FHalveLoopLengthCommand.Free;
  end;
end;

procedure TWavePatternControlGUI.LoopMetricChange(Sender: TObject);
begin
  //
end;


procedure TWavePatternControlGUI.Setpitch(const Avalue: Single);
begin
  if Avalue > 8 then
    FPitch := 8
  else if Avalue < 0.1 then
    FPitch := 0.1
  else
    FPitch := Avalue;
end;

constructor TWavePatternControlGUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FConnected := False;

  cbPitchAlgo.Items.Add('None');
  cbPitchAlgo.Items.Add('SoundTouch Eco');
  cbPitchAlgo.Items.Add('SoundTouch HQ');
  cbPitchAlgo.Items.Add('FFT');
  cbPitchAlgo.Items.Add('Rubberband');
  cbPitchAlgo.Items.Add('Pitched');

  FWaveGUI := TWaveGUI.Create(nil);
  FWaveGUI.Align := alClient;
  FWaveGUI.Parent := Self;
end;

destructor TWavePatternControlGUI.Destroy;
begin
  FWaveGUI.Free;

  inherited Destroy;
end;

initialization
  {$I wavepatterncontrolgui.lrs}

end.

