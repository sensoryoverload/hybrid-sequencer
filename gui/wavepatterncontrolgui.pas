unit wavepatterncontrolgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, StdCtrls, Spin,
  ExtCtrls, ComCtrls, Menus, globalconst, dialcontrol, wavegui, pattern, wave,
  pluginhost, pluginhostgui, plugin, patternoverview, LCLintf;

type
  { TWavePatternControlGUI }

  TWavePatternControlGUI = class(TFrame, IObserver)
    btnAutomationSelect: TButton;
    btnDouble: TButton;
    btnHalf: TButton;
    cbPitchAlgo: TComboBox;
    cbQuantize: TComboBox;
    lblAutomation: TLabel;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    pcBPM: TParameterControl;
    pcPitch: TParameterControl;
    LoopEnabled: TToggleControl;
    pupSelectAutomation: TPopupMenu;
    TreeView1: TTreeView;

    procedure btnAutomationSelectClick(Sender: TObject);
    procedure btnDoubleClick(Sender: TObject);
    procedure btnHalfClick(Sender: TObject);
    procedure cbPitchAlgoChange(Sender: TObject);
    procedure cbQuantizeChange(Sender: TObject);
    procedure LoopEnabledChange(Sender: TObject);
    procedure lThresholdChange(Sender: TObject);
    procedure lThresholdStartChange(Sender: TObject);
    procedure pcBPMChange(Sender: TObject);
    procedure pcBPMStartChange(Sender: TObject);
    procedure pcPitchChange(Sender: TObject);
    procedure pcPitchStartChange(Sender: TObject);
    procedure DoChancheRealBPMCommand(APersist: Boolean);
    procedure DoChangePitchCommand(APersist: Boolean);
    procedure cbPitchedChange(Sender: TObject);
  private
    { private declarations }
    FUpdateSubject: THybridPersistentModel;
    FIsDirty: Boolean;

    FModel: TWavePattern;
    FWaveGUI: TWaveGUI;
    FWaveOverview: TWavePatternOverview;

    FConnected: Boolean;
    FObjectOwnerID: string;
    FObjectID: string;
    FObjectOwner: TObject;

    FPitched: Boolean;
    FPitch: Single;
    FRealBPM: Single;
    procedure DeviceClick(Sender: TObject);
    procedure DeviceParameterClick(Sender: TObject);
    function GetEnabled: Boolean; reintroduce;
    procedure Setpitch(const Avalue: Single);
  protected
    procedure DoWaveZoom(ALeftPercentage, ARightPercentage: single);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;
    procedure Update(Subject: THybridPersistentModel); reintroduce;
    procedure UpdateView(AForceRedraw: Boolean = False);

    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string; virtual;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    procedure PopulateAutomationControls(Sender: TObject);

    property Connected: Boolean read FConnected;
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property ObjectID: string read GetObjectID write SetObjectID;
    property WaveGUI: TWaveGUI read FWaveGUI write FWaveGUI;
    property Enabled: Boolean read GetEnabled;

    property Pitch: Single read FPitch write SetPitch default 1;
    property Pitched: Boolean read FPitched write FPitched default False;
    property RealBPM: Single read FRealBPM write FRealBPM default 120;

    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
    function GetModel: THybridPersistentModel;
    procedure SetModel(AModel: THybridPersistentModel);
    property Model: THybridPersistentModel read GetModel write SetModel;
  end;

implementation

uses utils, global_command, global;

{ TWavePatternControlGUI }

procedure TWavePatternControlGUI.Connect;
var
  lPortParameter: TPortParameter;
  lPluginNode: TPluginNode;
begin
  DBLog(Format('Start Connect waveform (%s)', [FModel.ObjectID]));
  if Assigned(FModel) then
  begin
    FModel.Attach(FWaveGUI);
    FModel.Attach(FWaveOverview);
    FWaveGUI.SelectedAutomationDeviceId := FModel.SelectedAutomationDeviceId;
    FWaveGUI.SelectedAutomationParameterId := FModel.SelectedAutomationParameterId;

    DoWaveZoom(0, 100);

    case FWaveGUI.EditMode of
      emPatternEdit:
      begin
        lPluginNode := TPluginNode(
          GObjectMapper.GetModelObject(FModel.SelectedAutomationParameterId));
        if Assigned(lPluginNode) then
        begin
          lPortParameter := TPortParameter(
            GObjectMapper.GetModelObject(FModel.SelectedAutomationParameterId));
          if Assigned(lPortParameter) then
          begin
            btnAutomationSelect.Caption :=
              lPluginNode.PluginName + ' > ' + lPortParameter.Caption;
          end;
        end;
      end;
      emAutomationEdit:
      begin
        btnAutomationSelect.Caption := 'None';
      end;
    end;

    FWaveGUI.UpdateView(True);

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
    FModel.Detach(FWaveOverview);

    FConnected := False;
  end;
  DBLog(Format('End Disconnect waveform (%s)', [FModel.ObjectID]));
end;

procedure TWavePatternControlGUI.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TWavePatternControl.Update');

  FUpdateSubject := Subject;

  FIsDirty := True;

  if pcBPM.Value <> TWavePattern(Subject).RealBPM then
  begin
    pcBPM.Value := TWavePattern(Subject).RealBPM;
  end;

  if cbPitchAlgo.ItemIndex <> Integer(TWavePattern(Subject).PitchAlgorithm) then
  begin
    cbPitchAlgo.ItemIndex := Integer(TWavePattern(Subject).PitchAlgorithm);
  end;

  if cbQuantize.ItemIndex <> Integer(TWavePattern(Subject).QuantizeSetting) then
  begin
    cbQuantize.ItemIndex := Integer(TWavePattern(Subject).QuantizeSetting);
  end;

  if pcPitch.Value <> TWavePattern(Subject).Pitch then
  begin
    pcPitch.Value := TWavePattern(Subject).Pitch;
  end;

  DBLog('end TWavePatternControl.Update');
end;

procedure TWavePatternControlGUI.UpdateView(AForceRedraw: Boolean = False);
begin
  FWaveGUI.UpdateView(FIsDirty or AForceRedraw);

  if FIsDirty then
  begin
    FIsDirty := False;

    Invalidate;
  end;
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

procedure TWavePatternControlGUI.PopulateAutomationControls(
  Sender: TObject);
var
  lNodeIndex: Integer;
  lPluginNode: TPluginNode;
  lParamaterIndex: Integer;
  lDeviceItem: TMenuItemObject;
  lDeviceParameterItem: TMenuItemObject;
begin
  pupSelectAutomation.Items.Clear;
  lDeviceItem := TMenuItemObject.Create(pupSelectAutomation);
  lDeviceItem.Caption := 'None';
  lDeviceItem.ObjectId := '';
  lDeviceItem.ObjectType := miotNone;
  lDeviceItem.OnClick := @DeviceClick;
  pupSelectAutomation.Items.Add(lDeviceItem);

  for lNodeIndex := 0 to Pred(FModel.PluginProcessor.NodeList.Count) do
  begin
    lPluginNode := TPluginNode(FModel.PluginProcessor.NodeList[lNodeIndex]);
    lDeviceItem := TMenuItemObject.Create(pupSelectAutomation);
    lDeviceItem.Caption := lPluginNode.PluginName;
    lDeviceItem.ObjectId := lPluginNode.ObjectID;
    lDeviceItem.ObjectType := miotDevice;
    lDeviceItem.DeviceId := '';
    pupSelectAutomation.Items.Add(lDeviceItem);

    for lParamaterIndex := Low(lPluginNode.InputControls) to High(lPluginNode.InputControls) do
    begin
      lDeviceParameterItem := TMenuItemObject.Create(pupSelectAutomation);
      lDeviceParameterItem.Caption := lPluginNode.InputControls[lParamaterIndex].Caption;
      lDeviceParameterItem.ObjectId := lPluginNode.InputControls[lParamaterIndex].ObjectID;
      lDeviceParameterItem.Plugin := lPluginNode;
      lDeviceParameterItem.PluginParameter := lPluginNode.InputControls[lParamaterIndex];
      lDeviceParameterItem.ObjectType := miotDeviceParameter;
      lDeviceParameterItem.ParameterId := lPluginNode.InputControls[lParamaterIndex].Caption;
      lDeviceParameterItem.DeviceId := lPluginNode.ObjectID;
      lDeviceParameterItem.OnClick := @DeviceParameterClick;
      lDeviceItem.Add(lDeviceParameterItem);
    end;
  end;
end;

procedure TWavePatternControlGUI.DeviceParameterClick(Sender: TObject);
begin
  if Sender is TMenuItemObject then
  begin
    if TMenuItemObject(Sender).ObjectType = miotDeviceParameter then
    begin
      FWaveGUI.EditMode := emAutomationEdit;

      FWaveGUI.SelectedAutomationDeviceId := TMenuItemObject(Sender).DeviceId;
      FWaveGUI.SelectedAutomationParameterId := TMenuItemObject(Sender).ParameterId;

      btnAutomationSelect.Caption :=
        TMenuItemObject(Sender).Parent.Caption + ' > ' +
        TMenuItemObject(Sender).Caption;

      FWaveGUI.UpdateView(True);
    end;
  end;
end;

procedure TWavePatternControlGUI.DeviceClick(Sender: TObject);
begin
  if Sender is TMenuItemObject then
  begin
    if TMenuItemObject(Sender).ObjectType = miotNone then
    begin
      FWaveGUI.EditMode := emPatternEdit;
      btnAutomationSelect.Caption := 'None';
      FWaveGUI.UpdateView(True);
    end;
  end;
end;

procedure TWavePatternControlGUI.cbPitchedChange(Sender: TObject);
var
  lTogglePitchCommand: TTogglePitchCommand;
begin
  lTogglePitchCommand := TTogglePitchCommand.Create(FObjectID);
  try
    GCommandQueue.PushCommand(lTogglePitchCommand);
  except
    lTogglePitchCommand.Free;
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

procedure TWavePatternControlGUI.cbQuantizeChange(Sender: TObject);
var
  lChangeQuantizeCommand: TChangeQuantizeCommand;
begin
  lChangeQuantizeCommand := TChangeQuantizeCommand.Create(FObjectID);
  try
    lChangeQuantizeCommand.Value := TQuantizeSettings(cbQuantize.ItemIndex);

    GCommandQueue.PushCommand(lChangeQuantizeCommand);
  except
    lChangeQuantizeCommand.Free;
  end;
end;

procedure TWavePatternControlGUI.LoopEnabledChange(Sender: TObject);
begin
  // todo
end;

procedure TWavePatternControlGUI.lThresholdChange(Sender: TObject);
var
  lThresholdCommand: TUpdateThresholdCommand;
begin
  lThresholdCommand := TUpdateThresholdCommand.Create(FObjectID);
  try
//    lThresholdCommand.Sensitivity := lThreshold.Value;

    GCommandQueue.PushCommand(lThresholdCommand);
  except
    lThresholdCommand.Free;
  end;

  FWaveGUI.Invalidate;
end;

procedure TWavePatternControlGUI.lThresholdStartChange(Sender: TObject);
begin
  //
end;

procedure TWavePatternControlGUI.DoChancheRealBPMCommand(APersist: Boolean);
var
  lChangeRealBPMCommand: TChangeRealBPMCommand;
begin
  lChangeRealBPMCommand := TChangeRealBPMCommand.Create(FObjectID);
  try
    lChangeRealBPMCommand.Persist := APersist;
    lChangeRealBPMCommand.RealBPM := pcBPM.Value;

    GCommandQueue.PushCommand(lChangeRealBPMCommand);
  except
    lChangeRealBPMCommand.Free;
  end;
end;

procedure TWavePatternControlGUI.pcBPMChange(Sender: TObject);
begin
  DoChancheRealBPMCommand(False);
end;

procedure TWavePatternControlGUI.pcBPMStartChange(Sender: TObject);
begin
  DoChancheRealBPMCommand(True);
end;

procedure TWavePatternControlGUI.DoChangePitchCommand(APersist: Boolean);
var
  lChangePitchCommand: TChangePitchCommand;
begin
  lChangePitchCommand := TChangePitchCommand.Create(FObjectID);
  try
    lChangePitchCommand.Pitch := pcPitch.Value;

    GCommandQueue.PushCommand(lChangePitchCommand);
  except
    lChangePitchCommand.Free;
  end;
end;

procedure TWavePatternControlGUI.pcPitchChange(Sender: TObject);
begin
  DoChangePitchCommand(False);
end;

procedure TWavePatternControlGUI.pcPitchStartChange(Sender: TObject);
begin
  DoChangePitchCommand(True);
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

procedure TWavePatternControlGUI.btnAutomationSelectClick(Sender: TObject);
var
  pnt: TPoint;
begin
  if GetCursorPos(pnt) then
  begin
    pupSelectAutomation.PopUp(pnt.X, pnt.Y);
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

procedure TWavePatternControlGUI.Setpitch(const Avalue: Single);
begin
  if Avalue > 8 then
    FPitch := 8
  else if Avalue < 0.1 then
    FPitch := 0.1
  else
    FPitch := Avalue;
end;

function TWavePatternControlGUI.GetEnabled: Boolean;
begin
  Result := FModel.Enabled;
end;

procedure TWavePatternControlGUI.DoWaveZoom(ALeftPercentage,
  ARightPercentage: single);
begin
  FWaveGUI.ZoomFactorX := 100 / (ARightPercentage - ALeftPercentage);
  FWaveGUI.Offset := Round(ALeftPercentage);
  FWaveGUI.Invalidate;
end;

constructor TWavePatternControlGUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIsDirty := False;

  FConnected := False;

  cbPitchAlgo.Items.Add('None');
  cbPitchAlgo.Items.Add('SoundTouch Eco');
  cbPitchAlgo.Items.Add('SoundTouch HQ');
  cbPitchAlgo.Items.Add('Pitched');
  cbPitchAlgo.Items.Add('Beats');

  cbQuantize.Items.Add('-');
  cbQuantize.Items.Add('4 Beats');
  cbQuantize.Items.Add('1 Beat');
  cbQuantize.Items.Add('1/4 Beat');
  cbQuantize.Items.Add('1/8 Beat');
  cbQuantize.Items.Add('1/16 Beat');
  cbQuantize.Items.Add('1/32 Beat');

  FWaveGUI := TWaveGUI.Create(nil);
  FWaveGUI.Align := alClient;
  FWaveGUI.Parent := Self;

  FWaveOverview := TWavePatternOverview.Create(nil);
  FWaveOverview.ZoomCallback := @DoWaveZoom;
  FWaveOverview.Width := 120;
  FWaveOverview.Height := 20;
  FWaveOverview.Left := 5;
  FWaveOverview.Top := 4;
  FWaveOverview.Parent := Panel1;
end;

destructor TWavePatternControlGUI.Destroy;
begin
  FWaveGUI.Free;
  FWaveOverview.Free;

  inherited Destroy;
end;

initialization
  {$I wavepatterncontrolgui.lrs}

end.

