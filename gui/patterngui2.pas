{
  Copyright (C) 2009 Robbert Latumahina

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  patterngui.pas
}

unit patterngui2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, waveform,
  waveformgui, midi, midigui, graphics, track, LCLType, globalconst,
  global_command, global, pattern, dialcontrol, ComCtrls, StdCtrls, Spin,
  ExtCtrls, plugin, pluginhost, pluginhostgui, contnrs;

type

  { TPatternControls }

  TPatternControls = class(TFrame, IObserver)
    cbPitched: TCheckBox;
    cbQuantize: TComboBox;
    cbQuickSeek: TCheckBox;
    cbAntiAliasFilter: TCheckBox;
    cbSampleBankSelecter: TComboBox;
    cbMidiChannel: TComboBox;
    edtFilename: TLabeledEdit;
    gbPluginSettings: TGroupBox;
    lblMidiChannel: TLabel;
    lblSampleBank: TLabel;
    lblSequenceWidth: TLabel;
    pnlMidiGrid: TPanel;
    pnlMidigridOverview: TPanel;
    splitMidiTab: TSplitter;
    splitMidiGrid: TSplitter;
    spnPitchValue: TFloatSpinEditControl;
    gbAudioTrackSettings: TGroupBox;
    gbMidiTrackSettings: TGroupBox;
    Label1: TLabel;
    lblLoopEnd: TLabel;
    lblLoopLength: TLabel;
    lblLoopStart: TLabel;
    lblQuantize: TLabel;
    lblRootNote: TLabel;
    lblThreshold: TStaticText;
    pcPatternTabs: TPageControl;
    spnBeatsPerMinute: TFloatSpinEdit;
    spnRootNote: TSpinEdit;
    tsPlugins: TTabSheet;
    tbThreshold: TTrackBar;
    ToggleControl1: TToggleControl;
    TrackBar1: TTrackBar;
    tsAudio: TTabSheet;
    tsMIDI: TTabSheet;
    ValueControl1: TValueControl;
    ValueControl2: TValueControl;
    vcLoopEndBar: TValueControl;
    vcLoopEndBeat: TValueControl;
    vcLoopEndFrac: TValueControl;
    vcLoopLengthBar: TValueControl;
    vcLoopLengthBeat: TValueControl;
    vcLoopLengthFrac: TValueControl;
    vcLoopStartBar: TValueControl;
    vcLoopStartBeat: TValueControl;
    vcLoopStartFrac: TValueControl;
    procedure cbAntiAliasFilterChange(Sender: TObject);
    procedure cbPitchedChange(Sender: TObject);
    procedure cbQuantizeChange(Sender: TObject);
    procedure cbQuickSeekChange(Sender: TObject);
    procedure cbSampleBankSelecterChange(Sender: TObject);
    procedure cbMidiChannelChange(Sender: TObject);
    procedure lbToolBoxSelectionChange(Sender: TObject; User: boolean);
    procedure spnBeatsPerMinuteChange(Sender: TObject);
    procedure spnPitchChange(Sender: TObject);
    procedure tbThresholdChange(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure LoopMetricChange(Sender: TObject);
    procedure ValueControl1Change(Sender: TObject);
    procedure ValueControl2Change(Sender: TObject);
  private
    FObjectOwnerID: string;
    FObjectID: string;
    FModelObject: TObject;
    FObjectOwner: TObject;

    FModel: TPattern;

    // State reference to server
    FWaveForm: TWaveForm;
    FMidiGrid: TMidiGrid;
    FPluginProcessor: TPluginProcessor;

    FWaveFormGUI: TWaveFormGUI;
    FMidiGridGUI: TMidiGridGUI;
    FPluginProcessorGUI: TPluginProcessorGUI;
    FMidigridOverview: TMidigridOverview;

    FPitched: Boolean;
    FSyncQuantize: Boolean;
    FOkToPlay: Boolean;
    FPitch: Single;
    FRealBPM: Single;
    FRootNote: Integer;
    FMidiChannel: Integer;
    FPlaying: Boolean;
    FScheduled: Boolean;
    FSelected: Boolean;
    FPatternLength: Longint;

    procedure Setpitch(const Avalue: Single);
//    procedure spnPitchChange(Sender: TObject);
//    procedure tbThresholdChange(Sender: TObject);
    { private declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure Update(Subject: THybridPersistentModel); reintroduce;
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    property ObjectID: string read GetObjectID write SetObjectID;
    property Model: TPattern read FModel write FModel;
    property SyncQuantize: Boolean read FSyncQuantize write FSyncQuantize;
    property WaveFormGUI: TWaveFormGUI read FWaveFormGUI write FWaveFormGUI;
    property MidiGridGUI: TMidiGridGUI read FMidiGridGUI write FMidiGridGUI;
    property PluginProcessorGUI: TPluginProcessorGUI read FPluginProcessorGUI write FPluginProcessorGUI;
    property WaveForm: TWaveForm read FWaveForm write FWaveForm;
    property MidiGrid: TMidiGrid read FMidiGrid write FMidiGrid;
    property OkToPlay: Boolean read FOkToPlay write FOkToPlay;
    property Pitch: Single read FPitch write SetPitch default 1;
    property Pitched: Boolean read FPitched write FPitched default False;
    property RealBPM: Single read FRealBPM write FRealBPM default 120;
    property RootNote: Integer read FRootNote write FRootNote default 0;
    property MidiChannel: Integer read FMidiChannel write FMidiChannel;
    property Playing: Boolean read FPlaying write FPlaying default False;
    property Scheduled: Boolean read FScheduled write FScheduled default False;
    property Selected: Boolean read FSelected write FSelected;
    property PatternLength: Longint read FPatternLength write FPatternLength;
    property ObjectOwnerID: string read FObjectOwnerID write FObjectOwnerID;
    property ModelObject: TObject read FModelObject write FModelObject;
    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
  end;

implementation

uses
  utils, patterngui, audiostructure, sampler;

procedure TPatternControls.cbQuantizeChange(Sender: TObject);
var
  lQuantizeSettingCommand: TQuantizeSettingCommand;
begin

  if Assigned(FMidiGridGUI) then
  begin
    FMidiGridGUI.QuantizeSetting := cbQuantize.ItemIndex;

    case FMidiGridGUI.QuantizeSetting of
    0: FMidiGridGUI.QuantizeValue := -1;
    1: FMidiGridGUI.QuantizeValue := 100 * 4;
    2: FMidiGridGUI.QuantizeValue := 100 * 2;
    3: FMidiGridGUI.QuantizeValue := 100;
    4: FMidiGridGUI.QuantizeValue := 100 / 2;
    5: FMidiGridGUI.QuantizeValue := 100 / 3;
    6: FMidiGridGUI.QuantizeValue := 100 / 4;
    7: FMidiGridGUI.QuantizeValue := 100 / 6;
    8: FMidiGridGUI.QuantizeValue := 100 / 8;
    9: FMidiGridGUI.QuantizeValue := 100 / 16;
    10: FMidiGridGUI.QuantizeValue := 100 / 32;
    end;
  end;

  lQuantizeSettingCommand := TQuantizeSettingCommand.Create(MidiGrid.ObjectID);
  try
    lQuantizeSettingCommand.QuantizeSetting := cbQuantize.ItemIndex;

    GCommandQueue.PushCommand(lQuantizeSettingCommand);

  except
    lQuantizeSettingCommand.Free;
  end;
end;

procedure TPatternControls.cbQuickSeekChange(Sender: TObject);
begin
  if Assigned(FWaveFormGUI) then
  begin
    FWaveForm.WSOLA.QuickSeek := cbQuickSeek.Checked;
  end;
end;

procedure TPatternControls.cbSampleBankSelecterChange(Sender: TObject);
begin
  // TODO change used bank
end;

procedure TPatternControls.cbMidiChannelChange(Sender: TObject);
var
  lChangeMidiChannelCommand: TChangeMidiChannelCommand;
begin
  lChangeMidiChannelCommand := TChangeMidiChannelCommand.Create(TPatternGUI(Owner).ObjectID);
  try
    lChangeMidiChannelCommand.MidiChannel := cbMidiChannel.ItemIndex;

    GCommandQueue.PushCommand(lChangeMidiChannelCommand);
  except
    lChangeMidiChannelCommand.Free;
  end;
end;

procedure TPatternControls.lbToolBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  // Change selection (State or Command pattern?)
end;

procedure TPatternControls.spnBeatsPerMinuteChange(Sender: TObject);
var
  lChangeRealBPMCommand: TChangeRealBPMCommand;
begin
  lChangeRealBPMCommand := TChangeRealBPMCommand.Create(TPatternGUI(Owner).ObjectID);
  try
    lChangeRealBPMCommand.RealBPM := spnBeatsPerMinute.Value;

    GCommandQueue.PushCommand(lChangeRealBPMCommand);
  except
    lChangeRealBPMCommand.Free;
  end;
end;

procedure TPatternControls.cbPitchedChange(Sender: TObject);
var
  lTogglePitchCommand: TTogglePitchCommand;
begin
  lTogglePitchCommand := TTogglePitchCommand.Create(TPatternGUI(Owner).ObjectID);
  try
    lTogglePitchCommand.State := cbPitched.Checked;

    GCommandQueue.PushCommand(lTogglePitchCommand);
  except
    lTogglePitchCommand.Free;
  end;
end;

procedure TPatternControls.cbAntiAliasFilterChange(Sender: TObject);
begin
  if Assigned(FWaveFormGUI) then
  begin
    FWaveForm.WSOLA.AntiAliasFilter := cbAntiAliasFilter.Checked;
  end;
end;

procedure TPatternControls.spnPitchChange(Sender: TObject);
var
  lChangePitchCommand: TChangePitchCommand;
begin
  lChangePitchCommand := TChangePitchCommand.Create(TPatternGUI(Owner).ObjectID);
  try
    lChangePitchCommand.Pitch := spnPitchValue.Value;

    GCommandQueue.PushCommand(lChangePitchCommand);
  except
    lChangePitchCommand.Free;
  end;
end;

procedure TPatternControls.tbThresholdChange(Sender: TObject);
var
  lChangeThresholdCommand: TChangeThresHoldCommand;
begin
  lChangeThresholdCommand := TChangeThresHoldCommand.Create(TPatternGUI(Owner).ObjectID);
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


procedure TPatternControls.Setpitch(const Avalue: Single);
begin
  if Avalue > 8 then
    FPitch := 8
  else if Avalue < 0.1 then
    FPitch := 0.1
  else
    FPitch := Avalue;
end;

constructor TPatternControls.Create(AOwner: TComponent);
var
  i: Integer;
begin
  DBLog('start TPatternControls.Create');

  inherited Create(AOwner);

  Align := alClient;
  FMidiGridGUI := TMidiGridGUI.Create(Self);
  FWaveFormGUI := TWaveFormGUI.Create(Self);
  FPluginProcessorGUI := TPluginProcessorGUI.Create(Self);
  FMidigridOverview := TMidigridOverview.Create(Self);
  FMidigridOverview.ZoomCallback := @FMidiGridGUI.HandleZoom;

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

  cbSampleBankSelecter.Items.Add('Local');
  for i := 0 to Pred(GAudioStruct.Sampler.BankList.Count) do
  begin
    cbSampleBankSelecter.Items.Add(TSampleBank(GAudioStruct.Sampler.BankList[i]).BankName);
  end;

  ChangeControlStyle(Self, [csDisplayDragImage], [], True);

  DBLog('end TPatternControls.Create');
end;

destructor TPatternControls.Destroy;
begin
  FMidiGridGUI.Free;
  FWaveFormGUI.Free;
  FPluginProcessorGUI.Free;
  FMidigridOverview.Free;

  inherited Destroy;
end;

procedure TPatternControls.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TPatternControls.Update');

  if cbMidiChannel.ItemIndex <> TPattern(Subject).MidiChannel then
  begin
    cbMidiChannel.ItemIndex := TPattern(Subject).MidiChannel;
  end;

  DBLog('end TPatternControls.Update');
end;

function TPatternControls.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TPatternControls.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

procedure TPatternControls.Connect;
begin
  DBLog('start TPatternControls.Connect');

  FMidiGrid := Model.MidiGrid;
  FMidiGrid.Attach(FMidiGridGUI);
  FMidiGridGUI.Model := FMidiGrid;
  FMidiGridGUI.ObjectID := FMidiGrid.ObjectID;
  FMidiGridGUI.ObjectOwnerID := FMidiGrid.ObjectOwnerID;

  FMidiGridGUI.ZoomFactorX := 1000;
  FMidiGridGUI.ZoomFactorY := 1000;
  FMidiGridGUI.Align := alClient;
  FMidiGridGUI.Parent := nil;
  FMidiGridGUI.Parent := pnlMidiGrid;//tsMIDI;

  FMidigrid.Attach(FMidigridOverview);
  FMidigridOverview.ObjectID := FMidiGrid.ObjectID;
  FMidigridOverview.ObjectOwnerID := FMidiGrid.ObjectOwnerID;

  FMidigridOverview.Align := alClient;
  FMidigridOverview.Parent := nil;
  FMidigridOverview.Parent := pnlMidigridOverview;

  FWaveForm := Model.WaveForm;
  FWaveForm.Attach(FWaveFormGUI);
  FWaveFormGUI.Model := FWaveForm;
  FWaveFormGUI.ObjectID := FWaveForm.ObjectID;
  FWaveFormGUI.ObjectOwnerID := FWaveForm.ObjectOwnerID;
  FWaveFormGUI.Connect;

  DBLog(Format('Connected waveform (%s)', [FWaveFormGUI.ObjectID]));

  FWaveFormGUI.ZoomFactorX := 5;
  FWaveFormGUI.ZoomFactorY := 1;
  FWaveFormGUI.Align := alClient;
  FWaveFormGUI.Parent := nil;
  FWaveFormGUI.Parent := tsAudio;

  FPluginProcessor := Model.PluginProcessor;
  FPluginProcessor.Attach(FPluginProcessorGUI);
  FPluginProcessorGUI.Model := FPluginProcessor;
  FPluginProcessorGUI.ObjectID := FPluginProcessor.ObjectID;
  FPluginProcessorGUI.ObjectOwnerID := FPluginProcessor.ObjectOwnerID;
  FPluginProcessorGUI.Connect;

  DBLog(Format('Connected pluginhost (%s)', [FPluginProcessorGUI.ObjectID]));

  FPluginProcessorGUI.Align := alClient;
  FPluginProcessorGUI.Parent := nil;
  FPluginProcessorGUI.Parent := tsPlugins;

  FPluginProcessor.Notify;

  DBLog('end TPatternControls.Connect');
end;

procedure TPatternControls.Disconnect;
begin
  DBLog('start TPatternControls.Disconnect');

  FMidiGrid.Detach(FMidiGridGUI);
  FMidiGridGUI.Parent := nil;
  DBLog(Format('Disconnected midigrid (%s)', [FMidiGridGUI.ObjectID]));

  FWaveForm.Detach(FWaveFormGUI);
  FWaveFormGUI.Parent := nil;
  DBLog(Format('Disconnected waveform (%s)', [FWaveFormGUI.ObjectID]));

  FPluginProcessor.Detach(FPluginProcessorGUI);
  FPluginProcessorGUI.Parent := nil;
  DBLog(Format('Disconnected pluginprocessor (%s)', [FPluginProcessorGUI.ObjectID]));

  DBLog('end TPatternControls.Disconnect');
end;



procedure TPatternControls.TrackBar1Change(Sender: TObject);
begin
  if Assigned(GSettings.SelectedPatternGUI) then
  begin
    FWaveFormGUI.ZoomFactorX := TrackBar1.Position;
    FWaveFormGUI.CacheIsDirty := True;
    FWaveFormGUI.Repaint;
  end;
end;

procedure TPatternControls.LoopMetricChange(Sender: TObject);
begin
  //
end;

procedure TPatternControls.ValueControl1Change(Sender: TObject);
begin
  {if Assigned(FWaveFormGUI) then
  begin}
    FWaveForm.WSOLA.SequenceMS := ValueControl1.Value;
  {end;}
end;

procedure TPatternControls.ValueControl2Change(Sender: TObject);
begin
  FWaveForm.WSOLA.SeekWindowMS := ValueControl2.Value;
end;


initialization
  {$I patterngui2.lrs}

end.

