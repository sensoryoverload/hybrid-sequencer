unit wavepatterncontrolgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, StdCtrls, Spin,
  ExtCtrls, ComCtrls, globalconst, dialcontrol, wavegui, pattern, wave;

type

  { TWavePatternControlGUI }

  TWavePatternControlGUI = class(TFrame, IObserver)
    cbPitched: TCheckBox;
    edtFilename: TLabeledEdit;
    gbAudioTrackSettings: TGroupBox;
    Label1: TLabel;
    lblLoopEnd: TLabel;
    lblLoopLength: TLabel;
    lblLoopStart: TLabel;
    lblRootNote: TLabel;
    lblThreshold: TStaticText;
    spnBeatsPerMinute: TFloatSpinEdit;
    spnPitchValue: TFloatSpinEditControl;
    spnRootNote: TSpinEdit;
    tbThreshold: TTrackBar;
    ToggleControl1: TToggleControl;
    TrackBar1: TTrackBar;
    vcLoopEndBar: TValueControl;
    vcLoopEndBeat: TValueControl;
    vcLoopEndFrac: TValueControl;
    vcLoopLengthBar: TValueControl;
    vcLoopLengthBeat: TValueControl;
    vcLoopLengthFrac: TValueControl;
    vcLoopStartBar: TValueControl;
    vcLoopStartBeat: TValueControl;
    vcLoopStartFrac: TValueControl;

    procedure TrackBar1Change(Sender: TObject);
    procedure LoopMetricChange(Sender: TObject);
    procedure cbPitchedChange(Sender: TObject);
    procedure spnBeatsPerMinuteChange(Sender: TObject);
    procedure spnPitchChange(Sender: TObject);
    procedure tbThresholdChange(Sender: TObject);
  private
    { private declarations }
    FModel: TWavePattern;
    FWavePatternGUI: TWaveGUI;

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
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;

    property ObjectID: string read GetObjectID write SetObjectID;
    property WavePatternGUI: TWaveGUI read FWavePatternGUI write FWavePatternGUI;

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
  if Assigned(FModel) then
  begin
    FModel.Attach(Self);
    FModel.Attach(FWavePatternGUI);

//    FWavePatternGUI.ZoomFactorX := 5;
//    FWavePatternGUI.ZoomFactorY := 1;
  end;
end;

procedure TWavePatternControlGUI.Disconnect;
begin
  DBLog(Format('Disconnect waveform (%s)', [FWavePatternGUI.ObjectID]));
  if Assigned(FModel) then
  begin
    FModel.Detach(FWavePatternGUI);
    FModel.Detach(Self);
  end;
end;

procedure TWavePatternControlGUI.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TWavePatternControl.Update');

  spnBeatsPerMinute.Value := TWavePattern(Subject).RealBPM;

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
    lChangeRealBPMCommand.RealBPM := spnBeatsPerMinute.Value;

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
    lTogglePitchCommand.State := cbPitched.Checked;

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
    lChangePitchCommand.Pitch := spnPitchValue.Value;

    GCommandQueue.PushCommand(lChangePitchCommand);
  except
    lChangePitchCommand.Free;
  end;
end;

procedure TWavePatternControlGUI.tbThresholdChange(Sender: TObject);
var
  lChangeThresholdCommand: TChangeThresHoldCommand;
begin
  lChangeThresholdCommand := TChangeThresHoldCommand.Create(FObjectID);
  try
    lChangeThresholdCommand.Threshold := tbThreshold.Position;

    GCommandQueue.PushCommand(lChangeThresholdCommand);
  except
    lChangeThresholdCommand.Free;
  end;
  { Put in command object
      GAudioStruct.SelectedTrack.SelectedPattern.WaveForm.TransientThreshold:=
        Round((100 * tbThreshold.Position) / tbThreshold.Max);
  }
end;

procedure TWavePatternControlGUI.TrackBar1Change(Sender: TObject);
begin
  if Assigned(GSettings.SelectedPatternGUI) then
  begin
    FWavePatternGUI.ZoomFactorX := TrackBar1.Position;
    FWavePatternGUI.CacheIsDirty := True;
    FWavePatternGUI.Repaint;
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

  FWavePatternGUI := TWaveGUI.Create(nil);
  FWavePatternGUI.Align := alClient;
  FWavePatternGUI.Parent := Self;
end;

destructor TWavePatternControlGUI.Destroy;
begin
  FWavePatternGUI.Free;

  inherited Destroy;
end;

initialization
  {$I wavepatterncontrolgui.lrs}

end.

