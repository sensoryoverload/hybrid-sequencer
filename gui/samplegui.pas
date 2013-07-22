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

  samplegui.pas
}

unit samplegui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, globalconst,
  dialcontrol, utils, contnrs, LCLType, Controls, Graphics,
  StdCtrls, ComCtrls, Spin, PairSplitter, sampler, global_command;

type

  TChangeSelectSample = procedure (Sender: TObject; AObjectID: string) of object;

  TKeyPressed = procedure (Sender: TObject; AKey: Integer) of object;

  TSampleKeyboardControl = class;

  { TWaveEdit }

  TWaveEdit = class(TCustomControl)
  private
    FChannelCount: Integer;
    FData: PSingle;
    FDataSize: Integer;
    FZoom: Single;
  protected
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Data: PSingle read FData write FData;
    property DataSize: Integer read FDataSize write FDataSize;
    property ChannelCount: Integer read FChannelCount write FChannelCount;
    property Zoom: Single read FZoom write FZoom;
  end;

  { TSampleView }

  TSampleView = class(TFrame, IObserver)
    dcCutoffModAmount: TDialControl;
    dcEnvModAmount: TDialControl;
    dcOsc1PW: TDialControl;
    dcOsc2PW: TDialControl;
    dcOsc3PW: TDialControl;
    dcPitchEnvelopeAttack: TDialControl;
    dcPitchEnvelopeDecay: TDialControl;
    dcPitchEnvelopeRelease: TDialControl;
    dcPitchEnvelopeSustain: TDialControl;
    dcLpCutoff: TDialControl;
    dcAmpEnvelopeAttack: TDialControl;
    dcAmpEnvelopeDecay: TDialControl;
    dcAmpEnvelopeRelease: TDialControl;
    dcAmpEnvelopeSustain: TDialControl;
    dcOSC2Level: TDialControl;
    dcOSC3Level: TDialControl;
    dcOsc2ModAmount: TDialControl;
    dcOsc3ModAmount: TDialControl;
    dcOsc2Pitch: TDialControl;
    dcOsc3Pitch: TDialControl;
    dcLpResonance: TDialControl;
    dcOSC1Level: TDialControl;
    dcFilterEnvelopeAttack: TDialControl;
    dcFilterEnvelopeDecay: TDialControl;
    dcFilterEnvelopeSustain: TDialControl;
    dcFilterEnvelopeRelease: TDialControl;
    dcGlobalLevel: TDialControl;
    dcLFO1Rate: TDialControl;
    dcLFO2Rate: TDialControl;
    dcLFO3Rate: TDialControl;
    dcSaturateDrivePreFilter: TDialControl;
    dcSaturateDrivePostFilter: TDialControl;
    gbLFO: TGroupBox;
    lblBaseNote: TLabel;
    lblLowNote: TLabel;
    lblHighNote: TLabel;
    lblOSC2ModAmount1: TLabel;
    lblLFO1: TLabel;
    lblLFO2: TLabel;
    lblLFO3: TLabel;
    lblFilterSelect: TLabel;
    lblPitch: TLabel;
    lblAmp: TLabel;
    lblFilter: TLabel;
    lblOSC1: TLabel;
    lblOSC1ModAmount: TLabel;
    dcOsc1Pitch: TDialControl;
    dcOsc1ModAmount: TDialControl;
    gbOscillators: TGroupBox;
    gbFilter: TGroupBox;
    gbEnvelopes: TGroupBox;
    gbGlobal: TGroupBox;
    lblOSC2: TLabel;
    lblOSC2ModAmount: TLabel;
    lblOSC3: TLabel;
    lblOSC3ModAmount: TLabel;
    lsFilterSelect: TListSelect;
    lsCutoffModSource: TListSelect;
    lsLFO1WaveSelector: TListSelect;
    lsLFO2WaveSelector: TListSelect;
    lsLFO3WaveSelector: TListSelect;
    lsLowNote: TListSelect;
    lsBaseNote: TListSelect;
    lsHighNote: TListSelect;
    lsOsc1WaveSelector: TListSelect;
    lsOsc1ModSource: TListSelect;
    lsOsc2ModSource: TListSelect;
    lsOsc3ModSource: TListSelect;
    lsOsc2WaveSelector: TListSelect;
    lsOsc3WaveSelector: TListSelect;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    pnlControls: TPanel;
    tcFilterSwith: TToggleControl;
    procedure DoParameterChange(Sender: TObject);
    procedure DoToggleChange(Sender: TObject);
    procedure DoParameterStartChange(Sender: TObject);
  private
    { private declarations }
    FObjectOwnerID: string;
    FObjectID: string;
    FModel: TSample;
    FObjectOwner: TObject;
    FEnabled: Boolean;

    //FKeyboard: TSampleKeyboardControl;
    procedure DoListSelectionChange(Sender: TObject);
    procedure SetEnableControls(const AValue: Boolean);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce;
    procedure UpdateView;
    procedure Connect;
    procedure Disconnect;
    procedure DoKeyChange(Sender: TObject; AKey: Integer);
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string; virtual;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    function GetModel: THybridPersistentModel;
    procedure SetModel(AModel: THybridPersistentModel);
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property EnableControls: Boolean read FEnabled write SetEnableControls;
    property ObjectID: string read GetObjectID write SetObjectID;
    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
    property Model: THybridPersistentModel read GetModel write SetModel;
  end;

  { TSampleSelectControl }

  TSampleSelectControl = class(THybridPersistentView)
  private
    FCaption: string;
    FSampleLocation: string;
    FOnChange: TChangeSelectSample;
    FSelected: Boolean;
    FSampleView: TSampleView;

    procedure SetCaption(const AValue: string);

  public
    procedure Update(Subject: THybridPersistentModel); override;
    property SampleView: TSampleView read FSampleView write FSampleView;
    property SampleLocation: string read FSampleLocation write FSampleLocation;
  published
    property Caption: string read FCaption write SetCaption;
    property OnChange: TChangeSelectSample read FOnChange write FOnChange;
    property Selected: Boolean read FSelected write FSelected;
  end;

  { TSampleKeyboardControl }

  TSampleKeyboardControl = class(TPersistentCustomControl)
  private
    FCaption: string;
    FOnKeyChange: TKeyPressed;
    FSampleView: TSampleView;
    FSelectedKey: Integer;
    procedure SetCaption(const AValue: string);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure Update(Subject: THybridPersistentModel); override;
    property SampleView: TSampleView read FSampleView write FSampleView;
    property SelectedKey: Integer read FSelectedKey write FSelectedKey;
  published
    property Caption: string read FCaption write SetCaption;
    property OnKeyChange: TKeyPressed read FOnKeyChange write FOnKeyChange;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
  end;

implementation

uses
  global, bankgui, filters;

{ TSampleView }

procedure TSampleView.DoParameterChange(Sender: TObject);
var
  lGenericCommand: TSampleParameterCommand;
begin
  lGenericCommand := TSampleParameterCommand.Create(ObjectOwnerID);
  try
    lGenericCommand.Parameter := TSampleParameter(TDialControl(Sender).Tag);
    lGenericCommand.MidiLearn := TDialControl(Sender).MidiMappingMode;
    lGenericCommand.ObjectID := Self.ObjectID;
    lGenericCommand.Value := TDialControl(Sender).Value;
    lGenericCommand.Persist := False;

    GCommandQueue.PushCommand(lGenericCommand);
  except
    lGenericCommand.Free;
  end;
end;

procedure TSampleView.DoToggleChange(Sender: TObject);
var
  lGenericCommand: TSampleParameterCommand;
begin
  lGenericCommand := TSampleParameterCommand.Create(ObjectOwnerID);
  try
    lGenericCommand.Parameter := TSampleParameter(TToggleControl(Sender).Tag);
    lGenericCommand.ObjectID := Self.ObjectID;
    lGenericCommand.Value := not TToggleControl(Sender).SwitchedOn;
    lGenericCommand.Persist := True;

    GCommandQueue.PushCommand(lGenericCommand);
  except
    lGenericCommand.Free;
  end;
end;

procedure TSampleView.DoListSelectionChange(Sender: TObject);
var
  lGenericCommand: TSampleParameterCommand;
begin
  lGenericCommand := TSampleParameterCommand.Create(ObjectOwnerID);
  try
    lGenericCommand.Parameter := TSampleParameter(TListSelect(Sender).Tag);
    lGenericCommand.ObjectID := Self.ObjectID;
    lGenericCommand.Value := TListSelect(Sender).ItemIndex;
    lGenericCommand.Persist := True;

    GCommandQueue.PushCommand(lGenericCommand);
  except
    lGenericCommand.Free;
  end;
end;

procedure TSampleView.DoParameterStartChange(Sender: TObject);
var
  lGenericCommand: TSampleParameterCommand;
begin
  lGenericCommand := TSampleParameterCommand.Create(ObjectOwnerID);
  try
    lGenericCommand.Parameter := TSampleParameter(TDialControl(Sender).Tag);
    lGenericCommand.MidiLearn := TDialControl(Sender).MidiMappingMode;
    lGenericCommand.ObjectID := Self.ObjectID;
    lGenericCommand.Value := TDialControl(Sender).Value;
    lGenericCommand.Persist := True;

    GCommandQueue.PushCommand(lGenericCommand);
  except
    lGenericCommand.Free;
  end;
end;

procedure TSampleView.SetEnableControls(const AValue: Boolean);
var
  lIndex: Integer;
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;

    for lIndex := 0 to Pred(ComponentCount) do
    begin
      if Components[lIndex] is TDialControl then
      begin
        TDialControl(Components[lIndex]).Enabled := FEnabled;
      end
      else if Components[lIndex] is TListSelect then
      begin
        TListSelect(Components[lIndex]).Enabled := FEnabled;
      end;
    end;
  end;
end;

constructor TSampleView.Create(AOwner: TComponent);

  procedure FillWaveSelectionComboBox(AListSelect: TListSelect; ASampleParamter: TSampleParameter);
  var
    lIndex: Integer;
  begin
    for lIndex := Low(WFStrings) to High(WFStrings) do
    begin
      AListSelect.Items.Add(WFStrings[lIndex]);
    end;
    AListSelect.Tag := Integer(ASampleParamter);
    AListSelect.OnChange := @DoListSelectionChange;
  end;

  procedure FillModSourceComboBox(AListSelect: TListSelect; ASampleParamter: TSampleParameter);
  var
    lIndex: Integer;
  begin
    for lIndex := Low(ModSourceDescr) to High(ModSourceDescr) do
    begin
      AListSelect.Items.Add(ModSourceDescr[lIndex]);
    end;
    AListSelect.Tag := Integer(ASampleParamter);
    AListSelect.OnChange := @DoListSelectionChange;
  end;

  procedure FillNoteComboBox(AListSelect: TListSelect; ASampleParamter: TSampleParameter);
  var
    lIndex: Integer;
  begin
    for lIndex := 0 to 127 do
    begin
      AListSelect.Items.Add(IntToStr(lIndex));
    end;
    AListSelect.Tag := Integer(ASampleParamter);
    AListSelect.OnChange := @DoListSelectionChange;
  end;

  procedure FillFilterTypeListSelect(AListSelect: TListSelect; ASampleParamter: TSampleParameter);
  begin
    AListSelect.Items.Add('Lowpass');
    AListSelect.Items.Add('Highpass');
    AListSelect.Items.Add('Bandpass');
    AListSelect.Items.Add('Notch');
    AListSelect.Items.Add('Moog');
    AListSelect.Items.Add('303ish');
    AListSelect.Items.Add('Trans');
    AListSelect.Tag := Integer(ASampleParamter);
    AListSelect.OnChange := @DoListSelectionChange;
  end;

  procedure SetupDialControl(ADialControl: TDialControl; ASampleParamter: TSampleParameter;
    ALowest: Single; AHighest: Single; ADefaultValue: Single);
  begin
    ADialControl.Tag := Integer(ASampleParamter);
    ADialControl.OnChange :=  @DoParameterChange;
    ADialControl.OnStartChange := @DoParameterStartChange;
    ADialControl.Lowest := ALowest;
    ADialControl.Highest := AHighest;
    ADialControl.Value := ADefaultValue;
  end;

  procedure SetupToggleControl(AToggleControl: TToggleControl; ASampleParameter: TSampleParameter;
    ADefaultValue: Boolean);
  begin
    AToggleControl.Tag := Integer(ASampleParameter);
    AToggleControl.OnChange := @DoToggleChange;
    AToggleControl.SwitchedOn := ADefaultValue;
  end;

begin
  inherited Create(AOwner);

  Height := 143;

{  FKeyboard := TSampleKeyboardControl.Create(Self);
  FKeyboard.OnKeyChange := @DoKeyChange;
  FKeyboard.Height := 50;}

//  FKeyboard.Parent := Self;
 // FKeyboard.Align := alBottom;

  {
    Connect undo command event handlers
  }

  // Filter
  SetupDialControl(dcLpCutoff, spFilter_Cutoff, 0, 1, 1);
  FillModSourceComboBox(lsCutoffModSource, spFilter_Cutoff_ModSource);
  SetupDialControl(dcCutoffModAmount, spFilter_Cutoff_ModAmount, 0, 0.3, 0);
  SetupToggleControl(tcFilterSwith, spFilter_Active, True);

  SetupDialControl(dcLpResonance, spFilter_Resonance, 0, 1, 0);
  SetupDialControl(dcEnvModAmount, spFilter_Envelope_Amount, 0, 0.3, 0);

  FillFilterTypeListSelect(lsFilterSelect, spFilter_Type);

  // Oscillators
  SetupDialControl(dcOsc1Pitch, spOSC1_Pitch, -24, 24, 0);
  FillWaveSelectionComboBox(lsOsc1WaveSelector, spOSC1_Waveform);
  FillModSourceComboBox(lsOsc1ModSource, spOSC1_ModSource);
  SetupDialControl(dcOsc1ModAmount, spOSC1_ModAmount, 0, 1, 0);
  SetupDialControl(dcOSC1Level, spOSC1_Level, 0, 1, 0.3);
  SetupDialControl(dcOsc1PW, spOSC1_PulseWidth, 0.01, 1, 1);

  SetupDialControl(dcOsc2Pitch, spOSC2_Pitch, -24, 24, 0);
  FillWaveSelectionComboBox(lsOsc2WaveSelector, spOSC2_Waveform);
  FillModSourceComboBox(lsOsc2ModSource, spOSC2_ModSource);
  SetupDialControl(dcOsc2ModAmount, spOSC2_ModAmount, 0, 1, 0);
  SetupDialControl(dcOSC2Level, spOSC2_Level, 0, 1, 0);
  SetupDialControl(dcOsc2PW, spOSC2_PulseWidth, 0.01, 1, 1);

  SetupDialControl(dcOsc3Pitch, spOSC3_Pitch, -24, 24, 0);
  FillWaveSelectionComboBox(lsOsc3WaveSelector, spOSC3_Waveform);
  FillModSourceComboBox(lsOsc3ModSource, spOSC3_ModSource);
  SetupDialControl(dcOsc3ModAmount, spOSC3_ModAmount, 0, 1, 0);
  SetupDialControl(dcOSC3Level, spOSC3_Level, 0, 1, 0);
  SetupDialControl(dcOsc3PW, spOSC3_PulseWidth, 0.01, 1, 1);

  // Envelopes
  SetupDialControl(dcAmpEnvelopeAttack, spAmplifierEnv_Attack, 0.01, 1, 0.01);
  SetupDialControl(dcAmpEnvelopeDecay, spAmplifierEnv_Decay, 0.01, 1, 0.5);
  SetupDialControl(dcAmpEnvelopeSustain, spAmplifierEnv_Sustain, 0.01, 1, 0.01);
  SetupDialControl(dcAmpEnvelopeRelease, spAmplifierEnv_Release, 0.01, 1, 0.01);

  SetupDialControl(dcFilterEnvelopeAttack, spFilterEnv_Attack, 0.01, 1, 0.01);
  SetupDialControl(dcFilterEnvelopeDecay, spFilterEnv_Decay, 0.01, 1, 0.5);
  SetupDialControl(dcFilterEnvelopeSustain, spFilterEnv_Sustain, 0.01, 1, 0.01);
  SetupDialControl(dcFilterEnvelopeRelease, spFilterEnv_Release, 0.01, 1, 0.01);

  SetupDialControl(dcPitchEnvelopeAttack, spPitchEnv_Attack, 0.01, 1, 0.01);
  SetupDialControl(dcPitchEnvelopeDecay, spPitchEnv_Decay, 0.01, 1, 0.5);
  SetupDialControl(dcPitchEnvelopeSustain, spPitchEnv_Sustain, 0.01, 1, 0.01);
  SetupDialControl(dcPitchEnvelopeRelease, spPitchEnv_Release, 0.01, 1, 0.01);

  SetupDialControl(dcLFO1Rate, spLFO1_Rate, 0, 1, 0.3);
  FillWaveSelectionComboBox(lsLFO1WaveSelector, spLFO1_Waveform);
  SetupDialControl(dcLFO2Rate, spLFO2_Rate, 0, 1, 0.3);
  FillWaveSelectionComboBox(lsLFO2WaveSelector, spLFO2_Waveform);
  SetupDialControl(dcLFO3Rate, spLFO3_Rate, 0, 1, 0.3);
  FillWaveSelectionComboBox(lsLFO3WaveSelector, spLFO3_Waveform);

  SetupDialControl(dcGlobalLevel, spGlobal_Level, 0.01, 2, 1);

  FillNoteComboBox(lsLowNote, spLow_Note);
  FillNoteComboBox(lsHighNote, spHigh_Note);
  FillNoteComboBox(lsBaseNote, spBase_Note);

  SetupDialControl(dcSaturateDrivePreFilter, spSaturateDrivePreFilter, 1, 10, 1);
  SetupDialControl(dcSaturateDrivePostFilter, spPostFilterFeedback, 1, 5, 1);

  SetEnableControls(False);
end;

destructor TSampleView.Destroy;
begin

  inherited Destroy;
end;

procedure TSampleView.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TSampleView.Update');

  dcLpCutoff.Value := TSample(Subject).Filter.Frequency;
  dcLpResonance.Value := TSample(Subject).Filter.Resonance;
  lsCutoffModSource.ItemIndex := Integer(TSample(Subject).Filter.FreqModSource);
  dcCutoffModAmount.Value := TSample(Subject).Filter.FreqModAmount;
  dcEnvModAmount.Value := TSample(Subject).Filter.EnvelopeAmount;
  tcFilterSwith.SwitchedOn := TSample(Subject).Filter.Active;
  lsFilterSelect.ItemIndex := Integer(TSample(Subject).Filter.FilterType);

  dcOsc1Pitch.Value := TSample(Subject).Osc1.Pitch;
  lsOsc1WaveSelector.ItemIndex := Integer(TSample(Subject).Osc1.WaveForm);
  lsOsc1ModSource.ItemIndex := Integer(TSample(Subject).Osc1.ModSource);
  dcOsc1ModAmount.Value := TSample(Subject).Osc1.ModAmount;
  dcOSC1Level.Value := TSample(Subject).Osc1.Level;
  dcOsc1PW.Value := TSample(Subject).Osc1.PulseWidth;

  dcOsc2Pitch.Value := TSample(Subject).Osc2.Pitch;
  lsOsc2WaveSelector.ItemIndex := Integer(TSample(Subject).Osc2.WaveForm);
  lsOsc2ModSource.ItemIndex := Integer(TSample(Subject).Osc2.ModSource);
  dcOsc2ModAmount.Value := TSample(Subject).Osc2.ModAmount;
  dcOSC2Level.Value := TSample(Subject).Osc2.Level;
  dcOsc2PW.Value := TSample(Subject).Osc2.PulseWidth;

  dcOsc3Pitch.Value := TSample(Subject).Osc3.Pitch;
  lsOsc3WaveSelector.ItemIndex := Integer(TSample(Subject).Osc3.WaveForm);
  lsOsc3ModSource.ItemIndex := Integer(TSample(Subject).Osc3.ModSource);
  dcOsc3ModAmount.Value := TSample(Subject).Osc3.ModAmount;
  dcOSC3Level.Value := TSample(Subject).Osc3.Level;
  dcOsc3PW.Value := TSample(Subject).Osc3.PulseWidth;

  dcAmpEnvelopeAttack.Value := TSample(Subject).AmpEnvelope.Attack;
  dcAmpEnvelopeDecay.Value := TSample(Subject).AmpEnvelope.Decay;
  dcAmpEnvelopeSustain.Value := TSample(Subject).AmpEnvelope.Sustain;
  dcAmpEnvelopeRelease.Value := TSample(Subject).AmpEnvelope.Release;

  dcFilterEnvelopeAttack.Value := TSample(Subject).FilterEnvelope.Attack;
  dcFilterEnvelopeDecay.Value := TSample(Subject).FilterEnvelope.Decay;
  dcFilterEnvelopeSustain.Value := TSample(Subject).FilterEnvelope.Sustain;
  dcFilterEnvelopeRelease.Value := TSample(Subject).FilterEnvelope.Release;

  dcPitchEnvelopeAttack.Value := TSample(Subject).PitchEnvelope.Attack;
  dcPitchEnvelopeDecay.Value := TSample(Subject).PitchEnvelope.Decay;
  dcPitchEnvelopeSustain.Value := TSample(Subject).PitchEnvelope.Sustain;
  dcPitchEnvelopeRelease.Value := TSample(Subject).PitchEnvelope.Release;

  dcLFO1Rate.Value := TSample(Subject).LFO1.Pitch;
  lsLFO1WaveSelector.ItemIndex := Integer(TSample(Subject).LFO1.WaveForm);
  dcLFO2Rate.Value := TSample(Subject).LFO2.Pitch;
  lsLFO2WaveSelector.ItemIndex := Integer(TSample(Subject).LFO2.WaveForm);
  dcLFO3Rate.Value := TSample(Subject).LFO3.Pitch;
  lsLFO3WaveSelector.ItemIndex := Integer(TSample(Subject).LFO3.WaveForm);

  dcGlobalLevel.Value := TSample(Subject).GlobalLevel;
  lsLowNote.ItemIndex := TSample(Subject).LowNote;
  lsHighNote.ItemIndex := TSample(Subject).HighNote;
  lsBaseNote.ItemIndex := TSample(Subject).Key;

  dcSaturateDrivePreFilter.Value := TSample(Subject).SaturateDrivePreFilter;
  dcSaturateDrivePostFilter.Value := TSample(Subject).SaturateDrivePostFilter;

  SetEnableControls(FEnabled);

  DBLog('end TSampleView.Update');
end;

procedure TSampleView.UpdateView;
begin
  //
end;

procedure TSampleView.Connect;
begin
  //
end;

procedure TSampleView.Disconnect;
begin
  //
end;

procedure TSampleView.DoKeyChange(Sender: TObject; AKey: Integer);
begin
  writeln(format('Play note: %d', [AKey]));
end;

function TSampleView.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TSampleView.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

function TSampleView.GetObjectOwnerID: string;
begin
  Result := FObjectOwnerID;
end;

procedure TSampleView.SetObjectOwnerID(const AObjectOwnerID: string);
begin
  FObjectOwnerID := AObjectOwnerID;
end;

function TSampleView.GetModel: THybridPersistentModel;
begin
  Result := THybridPersistentModel(FModel);
end;

procedure TSampleView.SetModel(AModel: THybridPersistentModel);
begin
  FModel := TSample(AModel);
end;

{ TSampleSelectControl }

procedure TSampleSelectControl.SetCaption(const AValue: string);
begin
  FCaption := AValue;
end;

procedure TSampleSelectControl.Update(Subject: THybridPersistentModel);
begin
  writeln('start TSampleSelectControl.Update');

  FCaption := TSample(Subject).SampleName;
  FSelected := TSample(Subject).Selected;

  if Assigned(FSampleView) then
  begin
    FSampleView.Update(THybridPersistentModel(FSampleView.Model));
  end;

  writeln('end TSampleSelectControl.Update');
end;

{ TSampleKeyboardControl }

procedure TSampleKeyboardControl.SetCaption(const AValue: string);
begin
  if FCaption = AValue then exit;
  FCaption := AValue;
end;

constructor TSampleKeyboardControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSelectedKey := -1;
end;

destructor TSampleKeyboardControl.Destroy;
begin
  inherited Destroy;
end;

procedure TSampleKeyboardControl.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

procedure TSampleKeyboardControl.Paint;
var
  Bitmap: TBitmap;
  lKey: Integer;
  lKeyWidth: single;
begin
  Bitmap := TBitmap.Create;
  try
    // Initializes the Bitmap Size
    Bitmap.Height := Height;
    Bitmap.Width := Width;

    // Outline color
    Bitmap.Canvas.Pen.Style:= psSolid;
    Bitmap.Canvas.Pen.Color:= clBlack;

    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.Brush.Style := bsSolid;
    Bitmap.Canvas.Rectangle(0, 0, Bitmap.Width, Bitmap.Height);

    Bitmap.Canvas.Brush.Color := clBlack;

    lKeyWidth := Bitmap.Width / 128;
    for lKey := 0 to 127 do
    begin
      case lKey mod 12 of
        1, 3, 6, 8, 10: // Black keys
        begin
          Bitmap.Canvas.Brush.Color := clBlack;
          Bitmap.Canvas.Pen.Color := clBlack;

          Bitmap.Canvas.Rectangle(Round(lKeyWidth * lKey), 1,
            Round(lKeyWidth * lKey + lKeyWidth), Bitmap.Height - 1);
        end;
        0, 5:           // Full white keys
        begin
          Bitmap.Canvas.Brush.Color := clBlack;
          Bitmap.Canvas.Pen.Color := clBlack;
          Bitmap.Canvas.Line(Round(lKeyWidth * lKey), 1,
            Round(lKeyWidth * lKey), Bitmap.Height - 1);
        end;
        2, 4, 7, 9, 11: // White keys
        begin
          Bitmap.Canvas.Brush.Color := clWhite;
          Bitmap.Canvas.Pen.Color := clWhite;

          Bitmap.Canvas.Rectangle(Round(lKeyWidth * lKey), 1,
            Round(lKeyWidth * lKey + lKeyWidth), Bitmap.Height - 1);
        end;
      end;
    end;

    if FSelectedKey <> -1 then
    begin
      Bitmap.Canvas.Brush.Color := clGray;
      Bitmap.Canvas.Pen.Color := clGray;
      Bitmap.Canvas.Rectangle(Round(lKeyWidth * FSelectedKey), 1,
        Round(lKeyWidth * FSelectedKey + lKeyWidth), Bitmap.Height - 1);
    end;

    Bitmap.Canvas.TextOut(0, 0, inttostr(FSelectedKey));

    Canvas.Draw(0, 0, Bitmap);
  finally
    Bitmap.Free;
  end;

  inherited Paint;
end;

procedure TSampleKeyboardControl.Update(Subject: THybridPersistentModel);
begin
  //
end;

procedure TSampleKeyboardControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  FSelectedKey := Trunc(X / (Width / 128));

  Invalidate;

  if Assigned(FOnKeyChange) then
  begin
    FOnKeyChange(Self, FSelectedKey);
  end;
end;

procedure TSampleKeyboardControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
end;

{ TWaveEdit }

constructor TWaveEdit.Create(Aowner: Tcomponent);
begin
  inherited Create(Aowner);

  FZoom := 4;
end;

destructor TWaveEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TWaveEdit.Erasebackground(Dc: Hdc);
begin
  //inherited Erasebackground(Dc);
end;

procedure TWaveEdit.Paint;
var
  bmp: TBitmap;
  screenloop: integer;
  zeroline: integer;
begin
  bmp := TBitmap.Create;
  try
    bmp.Height := Height;
    bmp.Width := Width;
    zeroline := Height div 2;

    bmp.Canvas.Pen.Color := clBlack;
    bmp.Canvas.Clipping := False;
    bmp.Canvas.Rectangle(0, 0, Width, Height);

    bmp.Canvas.Pen.Color := clBlack;
    bmp.Canvas.Line(0, zeroline, Width, zeroline);

    bmp.Canvas.Pen.Color := clBlue;
    bmp.Canvas.MoveTo(0, zeroline);

    if (FDataSize > 0) and (FChannelCount > 0) and Assigned(FData) then
    begin
      FZoom := (FDataSize / SizeOf(Single)) / bmp.Width;

      for ScreenLoop := 0 to Pred(bmp.Width) do
      begin
        bmp.Canvas.LineTo(ScreenLoop, Round(FData[Trunc(ScreenLoop * FZoom)] * zeroline) + zeroline);
      end;
    end;

    Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;

  inherited Paint;
end;

initialization
  {$I samplegui.lrs}

end.

