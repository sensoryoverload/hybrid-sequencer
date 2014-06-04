unit midipatterncontrolgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, ExtCtrls, StdCtrls,
  PairSplitter, Menus, globalconst, midipatterngui, midi, midigui, sampler,
  samplegui, bankgui, global, patternoverview, LCLintf;

type
{ TMidiPatternControlGUI }

  TMidiPatternControlGUI = class(TFrame, IObserver)
    btnAutomationSelect: TButton;
    btnControllerSelect: TButton;
    cbMidiChannel: TComboBox;
    cbQuantize: TComboBox;
    lblControllers: TLabel;
    lblAutomation: TLabel;
    lblMidiChannel: TLabel;
    lblQuantize: TLabel;
    MenuItem1: TMenuItem;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    pnlMidiGrid: TPanel;
    pnlMidiSettings: TPanel;
    pupSelectController: TPopupMenu;
    pupSelectAutomation: TPopupMenu;

    procedure btnAutomationSelectClick(Sender: TObject);
    procedure btnControllerSelectClick(Sender: TObject);
    procedure cbQuantizeChange(Sender: TObject);
    procedure cbSampleBankSelecterChange(Sender: TObject);
    procedure cbMidiChannelChange(Sender: TObject);
  private
    { private declarations }
    FMidiPatternGUI: TMidiPatternGUI;
    FMidiOverview: TMidiPatternOverview;

    FRootNote: Integer;
    FMidiChannel: Integer;

    FConnected: Boolean;
    FObjectOwnerID: string;
    FObjectID: string;
    FModel: TMidiPattern;
    FObjectOwner: TObject;
  protected
    procedure DeviceClick(Sender: TObject);
    procedure DeviceParameterClick(Sender: TObject);
    procedure ControllerClick(Sender: TObject);
    procedure DoMidiZoom(ALeftPercentage, ARightPercentage: single);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure Update(Subject: THybridPersistentModel); reintroduce;
    procedure UpdateView(AForceRedraw: Boolean = False);

    procedure PopulateAutomationControls(Sender: TObject);
    procedure PopulateControllerSelection;

    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string; virtual;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    function GetModel: THybridPersistentModel;
    procedure SetModel(AModel: THybridPersistentModel);
    property Connected: Boolean read FConnected;
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property ObjectID: string read GetObjectID write SetObjectID;
    property MidiPatternGUI: TMidiPatternGUI read FMidiPatternGUI write FMidiPatternGUI;
    property RootNote: Integer read FRootNote write FRootNote default 0;
    property MidiChannel: Integer read FMidiChannel write FMidiChannel;
    property Model: THybridPersistentModel read GetModel write SetModel;
    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
  end;

implementation

uses utils, global_command, track, pluginhostgui, plugin;

{ TMidiPatternControlGUI }

constructor TMidiPatternControlGUI.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);

  FConnected := False;

  FMidiPatternGUI := TMidiPatternGUI.Create(nil);

  cbQuantize.Items.Add('None');
  cbQuantize.Items.Add('4 bars');
  cbQuantize.Items.Add('2 bars');
  cbQuantize.Items.Add('1 bar');
  cbQuantize.Items.Add('1/2');
  cbQuantize.Items.Add('1/3');
  cbQuantize.Items.Add('1/4');
  cbQuantize.Items.Add('1/6');
  cbQuantize.Items.Add('1/8');
  cbQuantize.Items.Add('1/16');
  cbQuantize.Items.Add('1/32');

  cbQuantize.ItemIndex := 3;

  cbMidiChannel.Items.Add('1');
  cbMidiChannel.Items.Add('2');
  cbMidiChannel.Items.Add('3');
  cbMidiChannel.Items.Add('4');
  cbMidiChannel.Items.Add('5');
  cbMidiChannel.Items.Add('6');
  cbMidiChannel.Items.Add('7');
  cbMidiChannel.Items.Add('8');
  cbMidiChannel.Items.Add('9');
  cbMidiChannel.Items.Add('10');
  cbMidiChannel.Items.Add('11');
  cbMidiChannel.Items.Add('12');
  cbMidiChannel.Items.Add('13');
  cbMidiChannel.Items.Add('14');
  cbMidiChannel.Items.Add('15');
  cbMidiChannel.Items.Add('16');

  PopulateControllerSelection;

  pnlMidiGrid.Align := alNone;
  pnlMidiGrid.Align := alClient;

  FMidiPatternGUI.Align := alClient;
  FMidiPatternGUI.Parent := nil;
  FMidiPatternGUI.Parent := pnlMidiGrid;

  FMidiOverview := TMidiPatternOverview.Create(nil);
  FMidiOverview.ZoomCallback := @DoMidiZoom;
  FMidiOverview.Width := 120;
  FMidiOverview.Height := 20;
  FMidiOverview.Left := 5;
  FMidiOverview.Top := 4;
  FMidiOverview.Parent := pnlMidiSettings;
end;

destructor TMidiPatternControlGUI.Destroy;
begin
  FMidiPatternGUI.Free;
  FMidiOverview.Free;

  inherited Destroy;
end;

procedure TMidiPatternControlGUI.Connect;
var
  lPortParameter: TPortParameter;
  lPluginNode: TPluginNode;
begin
  if Assigned(FModel) then
  begin
    FModel.Attach(FMidiPatternGUI);

    FModel.Attach(FMidiOverview);

    FMidiPatternGUI.ZoomFactorX := 1000;
    FMidiPatternGUI.ZoomFactorY := 1000;
    FMidiPatternGUI.QuantizeSetting := FModel.QuantizeSetting;
    FMidiPatternGUI.MidiChannel := FModel.MidiChannel;
    FMidiPatternGUI.SelectedAutomationDeviceId := FModel.SelectedAutomationDeviceId;
    FMidiPatternGUI.SelectedAutomationParameterId := FModel.SelectedAutomationParameterId;

    DoMidiZoom(0, 100);

    case FMidiPatternGUI.EditMode of
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

    FMidiPatternGUI.UpdateView(True);

    FConnected := True;
  end;
end;

procedure TMidiPatternControlGUI.Disconnect;
begin
  if Assigned(FModel) then
  begin
    FMidiOverview.Disconnect;
    FModel.Detach(FMidiOverview);

    FMidiPatternGUI.Disconnect;
    FModel.Detach(FMidiPatternGUI);

    FConnected := False;
  end;
end;

procedure TMidiPatternControlGUI.DoMidiZoom(ALeftPercentage,
  ARightPercentage: single);
begin
  FMidiPatternGUI.ZoomFactorX := 100 / (ARightPercentage - ALeftPercentage);
  FMidiPatternGUI.Offset := - Round(ALeftPercentage);

  FMidiPatternGUI.Invalidate;
end;

procedure TMidiPatternControlGUI.PopulateAutomationControls(Sender: TObject);
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

  if Assigned(FModel) then
  begin
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
        lDeviceParameterItem.DeviceId := lPluginNode.PluginName;
        lDeviceParameterItem.ParameterId := lPluginNode.InputControls[lParamaterIndex].Caption;
        lDeviceParameterItem.OnClick := @DeviceParameterClick;
        lDeviceItem.Add(lDeviceParameterItem);
      end;
    end;
 end;
end;

procedure TMidiPatternControlGUI.PopulateControllerSelection;
var
  lControllerItem: TMenuItem;

  procedure CreateControllerItem(AControllerId: Integer; ACaption: string; AHint: string = '');
  begin
    lControllerItem := TMenuItem.Create(pupSelectController);
    lControllerItem.Caption := ACaption;
    lControllerItem.Hint := AHint;
    lControllerItem.Tag := AControllerId;
    lControllerItem.OnClick := @ControllerClick;
    pupSelectController.Items.Add(lControllerItem);
  end;

begin
  CreateControllerItem(MIDI_CC_NONE, 'None');
  pupSelectController.Items.AddSeparator;
  CreateControllerItem(MIDI_VELOCITY, 'Velocity');
  pupSelectController.Items.AddSeparator;

  CreateControllerItem(0, 'Bank Select (coarse)', '0..127');
  CreateControllerItem(1, 'Modulation Wheel (coarse)', '0..127');
  CreateControllerItem(2, 'Breath Control (coarse)', '0..127');
  CreateControllerItem(3, 'Continuous controller #3', '0..127');
  CreateControllerItem(4, 'Foot Controller (coarse)', '0..127');
  CreateControllerItem(5, 'Portamento Time (coarse)', '0..127');
  CreateControllerItem(6, 'Data Entry Slider (coarse)', '0..127');
  CreateControllerItem(7, 'Main Volume (coarse)', '0..127');
  CreateControllerItem(8, 'Stereo Balance (coarse)', '0..127');
  CreateControllerItem(9, 'Continuous controller #9', '0..127');
  CreateControllerItem(10, 'Pan (coarse)', '0=left 64=center 127=right');
  CreateControllerItem(11, 'Expression (sub-Volume) (coarse)', '0..127');
  CreateControllerItem(12, 'Effect Control 1 (coarse)', '0..127');
  CreateControllerItem(13, 'Effect Control 2 (coarse)', '0..127');
  CreateControllerItem(14, 'Continuous controller #14', '0..127');
  CreateControllerItem(15, 'Continuous controller #15', '0..127');
  CreateControllerItem(16, 'General Purpose Slider 1', '0..127');
  CreateControllerItem(17, 'General Purpose Slider 2', '0..127');
  CreateControllerItem(18, 'General Purpose Slider 3', '0..127');
  CreateControllerItem(19, 'General Purpose Slider 4', '0..127');
  CreateControllerItem(20, 'Continuous controller #20', '0..127');
  CreateControllerItem(21, 'Continuous controller #21', '0..127');
  CreateControllerItem(22, 'Continuous controller #22', '0..127');
  CreateControllerItem(23, 'Continuous controller #23', '0..127');
  CreateControllerItem(24, 'Continuous controller #24', '0..127');
  CreateControllerItem(25, 'Continuous controller #25', '0..127');
  CreateControllerItem(26, 'Continuous controller #26', '0..127');
  CreateControllerItem(27, 'Continuous controller #27', '0..127');
  CreateControllerItem(28, 'Continuous controller #28', '0..127');
  CreateControllerItem(29, 'Continuous controller #29', '0..127');
  CreateControllerItem(30, 'Continuous controller #30', '0..127');
  CreateControllerItem(31, 'Continuous controller #31', '0..127');
  CreateControllerItem(32, 'Bank Select (fine)', '0..127  usually ignored');
  CreateControllerItem(33, 'Modulation Wheel (fine)', '0..127');
  CreateControllerItem(34, 'Breath Control (fine)', '0..127');
  CreateControllerItem(35, 'Continuous controller #3 (fine)', '0..127');
  CreateControllerItem(36, 'Foot Controller (fine)', '0..127');
  CreateControllerItem(37, 'Portamento Time (fine)', '0..127');
  CreateControllerItem(38, 'Data Entry Slider (fine)', '0..127');
  CreateControllerItem(39, 'Main Volume (fine)', '0..127  usually ignored');
  CreateControllerItem(40, 'Stereo Balance (fine)', '0..127');
  CreateControllerItem(41, 'Continuous controller #9 (fine)', '0..127');
  CreateControllerItem(42, 'Pan (fine)', '0..127  usually ignored');
  CreateControllerItem(43, 'Expression (sub-Volume) (fine)', '0..127  usually ignored');
  CreateControllerItem(44, 'Effect Control 1 (fine)', '0..127');
  CreateControllerItem(45, 'Effect Control 2 (fine)', '0..127');
  CreateControllerItem(46, 'Continuous controller #14 (fine)', '0..127');
  CreateControllerItem(47, 'Continuous controller #15 (fine)', '0..127');
  CreateControllerItem(48, 'Continuous controller #16', '0..127');
  CreateControllerItem(49, 'Continuous controller #17', '0..127');
  CreateControllerItem(50, 'Continuous controller #18', '0..127');
  CreateControllerItem(51, 'Continuous controller #19', '0..127');
  CreateControllerItem(52, 'Continuous controller #20 (fine)', '0..127');
  CreateControllerItem(53, 'Continuous controller #21 (fine)', '0..127');
  CreateControllerItem(54, 'Continuous controller #22 (fine)', '0..127');
  CreateControllerItem(55, 'Continuous controller #23 (fine)', '0..127');
  CreateControllerItem(56, 'Continuous controller #24 (fine)', '0..127');
  CreateControllerItem(57, 'Continuous controller #25 (fine)', '0..127');
  CreateControllerItem(58, 'Continuous controller #26 (fine)', '0..127');
  CreateControllerItem(59, 'Continuous controller #27 (fine)', '0..127');
  CreateControllerItem(60, 'Continuous controller #28 (fine)', '0..127');
  CreateControllerItem(61, 'Continuous controller #29 (fine)', '0..127');
  CreateControllerItem(62, 'Continuous controller #30 (fine)', '0..127');
  CreateControllerItem(63, 'Continuous controller #31 (fine)', '0..127');
  CreateControllerItem(64, 'Hold pedal (Sustain) on/off', '0..63=off  64..127=on');
  CreateControllerItem(65, 'Portamento on/off', '0..63=off  64..127=on');
  CreateControllerItem(66, 'Sustenuto Pedal on/off', '0..63=off  64..127=on');
  CreateControllerItem(67, 'Soft Pedal on/off', '0..63=off  64..127=on');
  CreateControllerItem(68, 'Legato Pedal on/off', '0..63=off  64..127=on');
  CreateControllerItem(69, 'Hold Pedal 2 on/off', '0..63=off  64..127=on');
  CreateControllerItem(70, 'Sound Variation', '0..127');
  CreateControllerItem(71, 'Sound Timbre', '0..127');
  CreateControllerItem(72, 'Sound Release Time', '0..127');
  CreateControllerItem(73, 'Sound Attack Time', '0..127');
  CreateControllerItem(74, 'Sound Brighness', '0..127');
  CreateControllerItem(75, 'Sound Control 6', '0..127');
  CreateControllerItem(76, 'Sound Control 7', '0..127');
  CreateControllerItem(77, 'Sound Control 8', '0..127');
  CreateControllerItem(78, 'Sound Control 9', '0..127');
  CreateControllerItem(79, 'Sound Control 10', '0..127');
  CreateControllerItem(80, 'General Purpose Button', '0..63=off  64..127=on');
  CreateControllerItem(81, 'General Purpose Button', '0..63=off  64..127=on');
  CreateControllerItem(82, 'General Purpose Button', '0..63=off  64..127=on');
  CreateControllerItem(83, 'General Purpose Button', '0..63=off  64..127=on');
  CreateControllerItem(84, 'Undefined on/off', '0..63=off  64..127=on');
  CreateControllerItem(85, 'Undefined on/off', '0..63=off  64..127=on');
  CreateControllerItem(86, 'Undefined on/off', '0..63=off  64..127=on');
  CreateControllerItem(87, 'Undefined on/off', '0..63=off  64..127=on');
  CreateControllerItem(88, 'Undefined on/off', '0..63=off  64..127=on');
  CreateControllerItem(89, 'Undefined on/off', '0..63=off  64..127=on');
  CreateControllerItem(90, 'Undefined on/off', '0..63=off  64..127=on');
  CreateControllerItem(91, 'Effects Level', '0..127');
  CreateControllerItem(92, 'Tremulo Level', '0..127');
  CreateControllerItem(93, 'Chorus Level', '0..127');
  CreateControllerItem(94, 'Celeste (Detune) Level', '0..127');
  CreateControllerItem(95, 'Phaser Level', '0..127');
  CreateControllerItem(96, 'Data entry +1', 'ignored');
  CreateControllerItem(97, 'Data entry -1', 'ignored');
  CreateControllerItem(98, 'Non-Registered Parameter Number (coarse)', '0..127');
  CreateControllerItem(99, 'Non-Registered Parameter Number (fine)', '0..127');
  CreateControllerItem(100, 'Registered Parameter Number (coarse)', '0..127');
  CreateControllerItem(101, 'Registered Parameter Number (fine)', '0..127');
  CreateControllerItem(102, 'Undefined', '?');
  CreateControllerItem(103, 'Undefined', '?');
  CreateControllerItem(104, 'Undefined', '?');
  CreateControllerItem(105, 'Undefined', '?');
  CreateControllerItem(106, 'Undefined', '?');
  CreateControllerItem(107, 'Undefined', '?');
  CreateControllerItem(108, 'Undefined', '?');
  CreateControllerItem(109, 'Undefined', '?');
  CreateControllerItem(110, 'Undefined', '?');
  CreateControllerItem(111, 'Undefined', '?');
  CreateControllerItem(112, 'Undefined', '?');
  CreateControllerItem(113, 'Undefined', '?');
  CreateControllerItem(114, 'Undefined', '?');
  CreateControllerItem(115, 'Undefined', '?');
  CreateControllerItem(116, 'Undefined', '?');
  CreateControllerItem(117, 'Undefined', '?');
  CreateControllerItem(118, 'Undefined', '?');
  CreateControllerItem(119, 'Undefined', '?');
  CreateControllerItem(120, 'All Sound Off', 'ignored');
  CreateControllerItem(121, 'All Controllers Off', 'ignored');
  CreateControllerItem(122, 'Local Keyboard On/Off', '0..63=off  64..127=on');
  CreateControllerItem(123, 'All Notes Off', 'ignored');
  CreateControllerItem(124, 'Omni Mode Off', 'ignored');
  CreateControllerItem(125, 'Omni Mode On', 'ignored');
  CreateControllerItem(126, 'Monophonic Mode On', '**');
  CreateControllerItem(127, 'Polyphonic Mode On (mono=off)', 'ignored');
end;

procedure TMidiPatternControlGUI.DeviceParameterClick(Sender: TObject);
begin
  if Sender is TMenuItemObject then
  begin
    if TMenuItemObject(Sender).ObjectType = miotDeviceParameter then
    begin
      FMidiPatternGUI.EditMode := emAutomationEdit;

      FMidiPatternGUI.SelectedAutomationDeviceId := TMenuItemObject(Sender).DeviceId;
      FMidiPatternGUI.SelectedAutomationParameterId := TMenuItemObject(Sender).ParameterId;

      btnAutomationSelect.Caption :=
        TMenuItemObject(Sender).Parent.Caption + ' > ' +
        TMenuItemObject(Sender).Caption;

      FMidiPatternGUI.UpdateView(True);
    end;
  end;
end;

procedure TMidiPatternControlGUI.ControllerClick(Sender: TObject);
var
  lMenuItem: TMenuItem;
begin
  if Sender is TMenuItem then
  begin
    lMenuItem := TMenuItem(Sender);

    // Set controller select button to selected controller item caption
    btnControllerSelect.Caption := lMenuItem.Caption;

    if lMenuItem.Tag = MIDI_CC_NONE then
    begin
      FMidiPatternGUI.EditMode := emPatternEdit;
    end
    else
    begin
      FMidiPatternGUI.EditMode := emControllerEdit;
    end;

    FMidiPatternGUI.SelectedController := lMenuItem.Tag;
    FMidiPatternGUI.UpdateView(True);
  end;
end;

procedure TMidiPatternControlGUI.DeviceClick(Sender: TObject);
begin
  if Sender is TMenuItemObject then
  begin
    if TMenuItemObject(Sender).ObjectType = miotNone then
    begin
      FMidiPatternGUI.EditMode := emPatternEdit;
      btnAutomationSelect.Caption := 'None';
      FMidiPatternGUI.UpdateView(True);
    end;
  end;
end;

procedure TMidiPatternControlGUI.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TPatternControls.Update');

  if TTrack(Subject.ObjectOwner).PatternList.IndexOf(Subject) = -1 then
  begin
    //Parent := nil;
  end;

  if cbMidiChannel.ItemIndex <> TMidiPattern(Subject).MidiChannel then
  begin
    cbMidiChannel.ItemIndex := TMidiPattern(Subject).MidiChannel;
  end;

  if cbQuantize.ItemIndex <> TMidiPattern(Subject).QuantizeSetting then
  begin
    cbQuantize.ItemIndex :=  TMidiPattern(Subject).QuantizeSetting;
  end;

  DBLog('end TPatternControls.Update');
end;

procedure TMidiPatternControlGUI.UpdateView(AForceRedraw: Boolean = False);
begin
  //
end;

function TMidiPatternControlGUI.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TMidiPatternControlGUI.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

function TMidiPatternControlGUI.GetObjectOwnerID: string;
begin
  Result := FObjectOwnerID;
end;

procedure TMidiPatternControlGUI.SetObjectOwnerID(const AObjectOwnerID: string
  );
begin
  FObjectOwnerID := AObjectOwnerID;
end;

function TMidiPatternControlGUI.GetModel: THybridPersistentModel;
begin
  Result := THybridPersistentModel(FModel);
end;

procedure TMidiPatternControlGUI.SetModel(AModel: THybridPersistentModel);
begin
  FModel := TMidiPattern(AModel);
end;

procedure TMidiPatternControlGUI.cbQuantizeChange(Sender: TObject);
var
  lQuantizeSettingCommand: TQuantizeSettingCommand;
begin

  if Assigned(FMidiPatternGUI) then
  begin
    FMidiPatternGUI.QuantizeSetting := cbQuantize.ItemIndex;

    case FMidiPatternGUI.QuantizeSetting of
    0: FMidiPatternGUI.QuantizeValue := -1;
    1: FMidiPatternGUI.QuantizeValue := 22050 * 4;
    2: FMidiPatternGUI.QuantizeValue := 22050 * 2;
    3: FMidiPatternGUI.QuantizeValue := 22050;
    4: FMidiPatternGUI.QuantizeValue := 22050 / 2;
    5: FMidiPatternGUI.QuantizeValue := 22050 / 3;
    6: FMidiPatternGUI.QuantizeValue := 22050 / 4;
    7: FMidiPatternGUI.QuantizeValue := 22050 / 6;
    8: FMidiPatternGUI.QuantizeValue := 22050 / 8;
    9: FMidiPatternGUI.QuantizeValue := 22050 / 16;
    10: FMidiPatternGUI.QuantizeValue := 22050 / 32;
    end;
  end;

  lQuantizeSettingCommand := TQuantizeSettingCommand.Create(FObjectID);
  try
    lQuantizeSettingCommand.QuantizeSetting := cbQuantize.ItemIndex;

    GCommandQueue.PushCommand(lQuantizeSettingCommand);

  except
    lQuantizeSettingCommand.Free;
  end;
end;

procedure TMidiPatternControlGUI.btnAutomationSelectClick(Sender: TObject);
var
  pnt: TPoint;
begin
  if GetCursorPos(pnt) then
  begin
    pupSelectAutomation.PopUp(pnt.X, pnt.Y);
  end;
end;

procedure TMidiPatternControlGUI.btnControllerSelectClick(Sender: TObject);
var
  pnt: TPoint;
begin
  if GetCursorPos(pnt) then
  begin
    pupSelectController.PopUp(pnt.X, pnt.Y);
  end;
end;

procedure TMidiPatternControlGUI.cbSampleBankSelecterChange(Sender: TObject);
begin
  // TODO change used bank
end;

procedure TMidiPatternControlGUI.cbMidiChannelChange(Sender: TObject);
var
  lChangeMidiChannelCommand: TChangeMidiChannelCommand;
begin
  lChangeMidiChannelCommand := TChangeMidiChannelCommand.Create(FObjectID);
  try
    lChangeMidiChannelCommand.MidiChannel := cbMidiChannel.ItemIndex;

    GCommandQueue.PushCommand(lChangeMidiChannelCommand);
  except
    lChangeMidiChannelCommand.Free;
  end;
end;

initialization
  {$I midipatterncontrolgui.lrs}

end.

