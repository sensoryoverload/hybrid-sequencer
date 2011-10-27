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
  StdCtrls, ComCtrls, Spin, sampler, global_command;

type

  TChangeSelectSample = procedure (Sender: TObject; AObjectID: string) of object;

  TKeyPressed = procedure (Sender: TObject; AKey: Integer) of object;

  TSampleKeyboardControl = class;

  { TSampleView }

  TSampleView = class(TFrame, IObserver)
    cbCutoffModSource: TComboBox;
    cbLowNote: TComboBox;
    cbHighNote: TComboBox;
    cbBaseNote: TComboBox;
    cbResoModSource: TComboBox;
    cbOsc1WaveSelector: TComboBox;
    cbOsc1ModSource: TComboBox;
    cbLFO1WaveSelector: TComboBox;
    cbOsc2WaveSelector: TComboBox;
    cbOsc2ModSource: TComboBox;
    cbLFO2WaveSelector: TComboBox;
    cbOsc3WaveSelector: TComboBox;
    cbOsc3ModSource: TComboBox;
    cbLFO3WaveSelector: TComboBox;
    dcCutoffModAmount: TDialControl;
    dcResoModAmount: TDialControl;
    dcPitchEnvelopeAttack: TDialControl;
    dcPitchEnvelopeDecay: TDialControl;
    dcPitchEnvelopeRelease: TDialControl;
    dcPitchEnvelopeSustain: TDialControl;
    dcLowpassCutoff: TDialControl;
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
    dcResonance: TDialControl;
    dcOSC1Level: TDialControl;
    dcFilterEnvelopeAttack: TDialControl;
    dcFilterEnvelopeDecay: TDialControl;
    dcFilterEnvelopeSustain: TDialControl;
    dcFilterEnvelopeRelease: TDialControl;
    dcGlobalLevel: TDialControl;
    dcLFO1Rate: TDialControl;
    dcLFO2Rate: TDialControl;
    dcLFO3Rate: TDialControl;
    gbLFO: TGroupBox;
    lblBaseNote: TLabel;
    lblLowNote: TLabel;
    lblHighNote: TLabel;
    lblOSC2ModAmount1: TLabel;
    lblLFO1: TLabel;
    lblLFO2: TLabel;
    lblLFO3: TLabel;
    lblOSC2ModAmount2: TLabel;
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
    procedure DoSelectionChange(Sender: TObject);
    procedure DoParameterChange(Sender: TObject);
    procedure DoParameterStartChange(Sender: TObject);
  private
    { private declarations }
    FObjectOwnerID: string;
    FObjectID: string;
    FModel: TSample;
    FObjectOwner: TObject;
    FSelectedBankObjectID: string;
    FEnabled: Boolean;

    FKeyboard: TSampleKeyboardControl;
    procedure SetEnableControls(const AValue: Boolean);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce;
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
    FCaptionWidth: Integer;
    FOnChange: TChangeSelectSample;
    FSelected: Boolean;
    FSampleView: TSampleView;

    procedure SetCaption(const AValue: string);

  public
    procedure Update(Subject: THybridPersistentModel); reintroduce;
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
    procedure Update(Subject: THybridPersistentModel); reintroduce;
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
    lGenericCommand.ObjectID := Self.ObjectID;
    lGenericCommand.Value := TDialControl(Sender).Value;
    lGenericCommand.Persist := False;

    GCommandQueue.PushCommand(lGenericCommand);
  except
    lGenericCommand.Free;
  end;
end;

procedure TSampleView.DoSelectionChange(Sender: TObject);
var
  lGenericCommand: TSampleParameterCommand;
begin
  lGenericCommand := TSampleParameterCommand.Create(ObjectOwnerID);
  try
    lGenericCommand.Parameter := TSampleParameter(TCustomComboBox(Sender).Tag);
    lGenericCommand.ObjectID := Self.ObjectID;
    lGenericCommand.Value := TCustomComboBox(Sender).ItemIndex;
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
      end;
    end;
  end;
end;

constructor TSampleView.Create(AOwner: TComponent);

  procedure FillWaveSelectionComboBox(AComboBox: TComboBox; ASampleParamter: TSampleParameter);
  var
    lIndex: Integer;
  begin
    for lIndex := Low(WFStrings) to High(WFStrings) do
    begin
      AComboBox.Items.Add(WFStrings[lIndex]);
    end;
    AComboBox.Tag := Integer(ASampleParamter);
    AComboBox.OnChange := @DoSelectionChange;
  end;

  procedure FillModSourceComboBox(AComboBox: TComboBox; ASampleParamter: TSampleParameter);
  var
    lIndex: Integer;
  begin
    for lIndex := Low(ModSourceDescr) to High(ModSourceDescr) do
    begin
      AComboBox.Items.Add(ModSourceDescr[lIndex]);
    end;
    AComboBox.Tag := Integer(ASampleParamter);
    AComboBox.OnChange := @DoSelectionChange;
  end;

  procedure FillNoteComboBox(AComboBox: TComboBox; ASampleParamter: TSampleParameter);
  var
    lIndex: Integer;
  begin
    for lIndex := 0 to 127 do
    begin
      AComboBox.Items.Add(IntToStr(lIndex));
    end;
    AComboBox.Tag := Integer(ASampleParamter);
    AComboBox.OnChange := @DoSelectionChange;
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

begin
  inherited Create(AOwner);

  FKeyboard := TSampleKeyboardControl.Create(nil);
  FKeyboard.OnKeyChange := @DoKeyChange;
  FKeyboard.Height := 50;

//  FKeyboard.Parent := Self;
 // FKeyboard.Align := alBottom;

  {
    Connect undo command event handlers
  }

  // Filter
  SetupDialControl(dcLowpassCutoff, spFilter_Cutoff, 20, 20000, 20000);
  FillModSourceComboBox(cbCutoffModSource, spFilter_Cutoff_ModSource);
  SetupDialControl(dcCutoffModAmount, spFilter_Cutoff_ModAmount, 0, 1, 0);

  SetupDialControl(dcResonance, spFilter_Resonance, 0, 1, 0);
  FillModSourceComboBox(cbResoModSource, spFilter_Resonance_ModSource);
  SetupDialControl(dcResoModAmount, spFilter_Resonance_ModAmount, 0, 1, 0);

  // Oscillators
  SetupDialControl(dcOsc1Pitch, spOSC1_Pitch, -24, 24, 0);
  FillWaveSelectionComboBox(cbOsc1WaveSelector, spOSC1_Waveform);
  FillModSourceComboBox(cbOsc1ModSource, spOSC1_ModSource);
  SetupDialControl(dcOsc1ModAmount, spOSC1_ModAmount, 0, 1, 0);
  SetupDialControl(dcOSC1Level, spOSC1_Level, 0, 1, 0.3);

  SetupDialControl(dcOsc2Pitch, spOSC2_Pitch, -24, 24, 0);
  FillWaveSelectionComboBox(cbOsc2WaveSelector, spOSC2_Waveform);
  FillModSourceComboBox(cbOsc2ModSource, spOSC2_ModSource);
  SetupDialControl(dcOsc2ModAmount, spOSC2_ModAmount, 0, 1, 0);
  SetupDialControl(dcOSC2Level, spOSC2_Level, 0, 1, 0);

  SetupDialControl(dcOsc3Pitch, spOSC3_Pitch, -24, 24, 0);
  FillWaveSelectionComboBox(cbOsc3WaveSelector, spOSC3_Waveform);
  FillModSourceComboBox(cbOsc3ModSource, spOSC3_ModSource);
  SetupDialControl(dcOsc3ModAmount, spOSC3_ModAmount, 0, 1, 0);
  SetupDialControl(dcOSC3Level, spOSC3_Level, 0, 1, 0);

  SetupDialControl(dcAmpEnvelopeAttack, spAmplifierEnv_Attack, 0.01, 5, 0.01);
  SetupDialControl(dcAmpEnvelopeDecay, spAmplifierEnv_Decay, 0.01, 5, 0.5);
  SetupDialControl(dcAmpEnvelopeSustain, spAmplifierEnv_Sustain, 0, 1, 0);
  SetupDialControl(dcAmpEnvelopeRelease, spAmplifierEnv_Release, 0.01, 5, 0.2);

  SetupDialControl(dcFilterEnvelopeAttack, spFilterEnv_Attack, 0.01, 5, 0.01);
  SetupDialControl(dcFilterEnvelopeDecay, spFilterEnv_Decay, 0.01, 5, 0.5);
  SetupDialControl(dcFilterEnvelopeSustain, spFilterEnv_Sustain, 0, 1, 0);
  SetupDialControl(dcFilterEnvelopeRelease, spFilterEnv_Release, 0.01, 5, 0.2);

  SetupDialControl(dcPitchEnvelopeAttack, spPitchEnv_Attack, 0.01, 5, 0.01);
  SetupDialControl(dcPitchEnvelopeDecay, spPitchEnv_Decay, 0.01, 5, 0.5);
  SetupDialControl(dcPitchEnvelopeSustain, spPitchEnv_Sustain, 0, 1, 0);
  SetupDialControl(dcPitchEnvelopeRelease, spPitchEnv_Release, 0.01, 5, 0.2);

  SetupDialControl(dcLFO1Rate, spLFO1_Rate, 0.05, 1000, 10);
  FillWaveSelectionComboBox(cbLFO1WaveSelector, spLFO1_Waveform);
  SetupDialControl(dcLFO2Rate, spLFO2_Rate, 0.05, 1000, 10);
  FillWaveSelectionComboBox(cbLFO2WaveSelector, spLFO2_Waveform);
  SetupDialControl(dcLFO3Rate, spLFO3_Rate, 0.05, 1000, 10);
  FillWaveSelectionComboBox(cbLFO3WaveSelector, spLFO3_Waveform);

  SetupDialControl(dcGlobalLevel, spGlobal_Level, 0.01, 1, 0.5);

  FillNoteComboBox(cbLowNote, spLow_Note);
  FillNoteComboBox(cbHighNote, spHigh_Note);
  FillNoteComboBox(cbBaseNote, spBase_Note);

  SetEnableControls(False);
end;

destructor TSampleView.Destroy;
begin
  if Assigned(FKeyboard) then
    FKeyboard.Free;

  inherited Destroy;
end;

procedure TSampleView.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TSampleView.Update');

  dcLowpassCutoff.Value := TSample(Subject).Filter.Frequency;
  cbCutoffModSource.ItemIndex := Integer(TSample(Subject).Filter.FreqModSource);
  dcCutoffModAmount.Value := TSample(Subject).Filter.FreqModAmount;
  dcResonance.Value := TSample(Subject).Filter.Resonance;
  cbResoModSource.ItemIndex := Integer(TSample(Subject).Filter.ResoModSource);
  dcResoModAmount.Value := TSample(Subject).Filter.ResoModAmount;

  dcOsc1Pitch.Value := TSample(Subject).Osc1.Pitch;
  cbOsc1WaveSelector.ItemIndex := Integer(TSample(Subject).Osc1.WaveForm);
  cbOsc1ModSource.ItemIndex := Integer(TSample(Subject).Osc1.ModSource);
  dcOsc1ModAmount.Value := TSample(Subject).Osc1.ModAmount;
  dcOSC1Level.Value := TSample(Subject).Osc1.Level;

  dcOsc2Pitch.Value := TSample(Subject).Osc2.Pitch;
  cbOsc2WaveSelector.ItemIndex := Integer(TSample(Subject).Osc2.WaveForm);
  cbOsc2ModSource.ItemIndex := Integer(TSample(Subject).Osc2.ModSource);
  dcOsc2ModAmount.Value := TSample(Subject).Osc2.ModAmount;
  dcOSC2Level.Value := TSample(Subject).Osc2.Level;

  dcOsc3Pitch.Value := TSample(Subject).Osc3.Pitch;
  cbOsc3WaveSelector.ItemIndex := Integer(TSample(Subject).Osc3.WaveForm);
  cbOsc3ModSource.ItemIndex := Integer(TSample(Subject).Osc3.ModSource);
  dcOsc3ModAmount.Value := TSample(Subject).Osc3.ModAmount;
  dcOSC3Level.Value := TSample(Subject).Osc3.Level;

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
  cbLFO1WaveSelector.ItemIndex := Integer(TSample(Subject).LFO1.ModSource);
  dcLFO2Rate.Value := TSample(Subject).LFO2.Pitch;
  cbLFO2WaveSelector.ItemIndex := Integer(TSample(Subject).LFO2.ModSource);
  dcLFO3Rate.Value := TSample(Subject).LFO3.Pitch;
  cbLFO3WaveSelector.ItemIndex := Integer(TSample(Subject).LFO3.ModSource);

  dcGlobalLevel.Value := TSample(Subject).GlobalLevel;
  cbLowNote.ItemIndex := TSample(Subject).LowNote;
  cbHighNote.ItemIndex := TSample(Subject).HighNote;
  cbBaseNote.ItemIndex := TSample(Subject).Key;

  SetEnableControls(FEnabled);

  DBLog('end TSampleView.Update');
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

initialization
  {$I samplegui.lrs}

end.

