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
  dialcontrol, utils, contnrs, LCLType, Controls, Graphics, StdCtrls, ComCtrls,
  sampler, global_command;

type

  TChangeSelectSample = procedure (Sender: TObject; AObjectID: string) of object;

  TKeyPressed = procedure (Sender: TObject; AKey: Integer) of object;

  TSampleKeyboardControl = class;

  { TSampleView }

  TSampleView = class(TFrame, IObserver)
    cbOsc1WaveSelector: TComboBox;
    cbOsc1ModSource: TComboBox;
    cbOsc2WaveSelector: TComboBox;
    cbOsc2ModSource: TComboBox;
    cbOsc3WaveSelector: TComboBox;
    cbOsc3ModSource: TComboBox;
    dcPitchEnvelopeAttack: TDialControl;
    dcPitchEnvelopeDecay: TDialControl;
    dcPitchEnvelopeRelease: TDialControl;
    dcPitchEnvelopeSustain: TDialControl;
    dcCutoff: TDialControl;
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
    Label1: TLabel;
    lblOSC2: TLabel;
    lblOSC2ModAmount: TLabel;
    lblOSC3: TLabel;
    lblOSC3ModAmount: TLabel;
    pcSampler: TPageControl;
    tsSample: TTabSheet;
    tsTuning: TTabSheet;
    procedure DoParameterChange(Sender: TObject);
    procedure DoParameterStartChange(Sender: TObject);
  private
    { private declarations }
    FObjectOwnerID: string;
    FObjectID: string;
    FModelObject: TObject;
    FObjectOwner: TObject;
    FSelectedBankObjectID: string;
    FEnabled: Boolean;

    FKeyboard: TSampleKeyboardControl;
    procedure SetEnabled(const AValue: Boolean);
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
    property Enabled: Boolean read FEnabled write SetEnabled;
    property ObjectID: string read GetObjectID write SetObjectID;
    property ObjectOwnerID: string read FObjectOwnerID write FObjectOwnerID;
    property ModelObject: TObject read FModelObject write FModelObject;
    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
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

procedure TSampleView.SetEnabled(const AValue: Boolean);
var
  lIndex: Integer;
begin
  if FEnabled = AValue then exit;
  FEnabled := AValue;

  for lIndex := 0 to Pred(ComponentCount) do
  begin
    if Components[lIndex] is TDialControl then
    begin
      TDialControl(Components[lIndex]).Enabled := FEnabled;
    end;
  end;
end;

constructor TSampleView.Create(AOwner: TComponent);

  procedure FillWaveSelectionComboBox(AComboBox: TComboBox);
  var
    lIndex: Integer;
  begin
    for lIndex := Low(WFStrings) to High(WFStrings) do
    begin
      AComboBox.Items.Add(WFStrings[lIndex]);
    end;
  end;

  procedure FillModSourceComboBox(AComboBox: TComboBox);
  var
    lIndex: Integer;
  begin
    for lIndex := Low(ModSourceDescr) to High(ModSourceDescr) do
    begin
      AComboBox.Items.Add(ModSourceDescr[lIndex]);
    end;
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

  FKeyboard.Parent := Self;
  FKeyboard.Align := alBottom;
  pcSampler.Align := alClient;

  {
    Connect undo command event handlers
  }

  // Filter
  SetupDialControl(dcCutoff, spFilter_Cutoff, 20, 20000, 20000);
  SetupDialControl(dcResonance, spFilter_Resonance, 0, 1, 0);

  // Oscillators
  SetupDialControl(dcOsc1Pitch, spOSC1_Pitch, -24, 24, 0);
  SetupDialControl(dcOsc2Pitch, spOSC2_Pitch, -24, 24, 0);
  SetupDialControl(dcOsc3Pitch, spOSC3_Pitch, -24, 24, 0);

  FillWaveSelectionComboBox(cbOsc1WaveSelector);
  FillModSourceComboBox(cbOsc1ModSource);

  SetupDialControl(dcOsc1ModAmount, spOSC1_ModAmount, 0, 1, 0);
  SetupDialControl(dcOsc2ModAmount, spOSC2_ModAmount, 0, 1, 0);
  SetupDialControl(dcOsc3ModAmount, spOSC3_ModAmount, 0, 1, 0);

  FillWaveSelectionComboBox(cbOsc1WaveSelector);
  FillModSourceComboBox(cbOsc1ModSource);

  FillWaveSelectionComboBox(cbOsc2WaveSelector);
  FillModSourceComboBox(cbOsc2ModSource);

  FillWaveSelectionComboBox(cbOsc3WaveSelector);
  FillModSourceComboBox(cbOsc3ModSource);

  SetupDialControl(dcOSC1Level, spOSC1_Level, 0, 1, 1);
  SetupDialControl(dcOSC2Level, spOSC2_Level, 0, 1, 1);
  SetupDialControl(dcOSC3Level, spOSC3_Level, 0, 1, 1);

  SetupDialControl(dcAmpEnvelopeAttack, spAmplifierEnv_Attack, 0.01, 5, 0.01);
  SetupDialControl(dcAmpEnvelopeDecay, spAmplifierEnv_Decay, 0.01, 5, 0.5);
  SetupDialControl(dcAmpEnvelopeSustain, spAmplifierEnv_Sustain, 0, 1, 0);
  SetupDialControl(dcAmpEnvelopeRelease, spAmplifierEnv_Release, 0.01, 10, 0.5);

  SetupDialControl(dcFilterEnvelopeAttack, spFilterEnv_Attack, 0.01, 5, 0.01);
  SetupDialControl(dcFilterEnvelopeDecay, spFilterEnv_Decay, 0.01, 5, 0.5);
  SetupDialControl(dcFilterEnvelopeSustain, spFilterEnv_Sustain, 0, 1, 0);
  SetupDialControl(dcFilterEnvelopeRelease, spFilterEnv_Release, 0.01, 10, 0.5);

  SetupDialControl(dcPitchEnvelopeAttack, spPitchEnv_Attack, 0.01, 5, 0.01);
  SetupDialControl(dcPitchEnvelopeDecay, spPitchEnv_Decay, 0.01, 5, 0.5);
  SetupDialControl(dcPitchEnvelopeSustain, spPitchEnv_Sustain, 0, 1, 0);
  SetupDialControl(dcPitchEnvelopeRelease, spPitchEnv_Release, 0.01, 10, 0.5);
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

  dcCutoff.Value := TSample(Subject).Filter.Frequency;
  dcResonance.Value := TSample(Subject).Filter.Resonance;

  dcOsc1Pitch.Value := TSample(Subject).Osc1.Pitch;
  dcOsc1ModAmount.Value := TSample(Subject).Osc1.ModAmount;
  dcOSC1Level.Value := TSample(Subject).Osc1.Level;

  dcOsc2Pitch.Value := TSample(Subject).Osc2.Pitch;
  dcOsc2ModAmount.Value := TSample(Subject).Osc2.ModAmount;
  dcOSC2Level.Value := TSample(Subject).Osc2.Level;

  dcOsc3Pitch.Value := TSample(Subject).Osc3.Pitch;
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

  DBLog('end TSampleView.Update');
end;

procedure TSampleView.Connect;
var
  lSelectedBank: TSampleBank;
begin
  lSelectedBank := TSampleBank(GObjectMapper.GetModelObject(ObjectID));
end;

procedure TSampleView.Disconnect;
var
  lSelectedSample: TSample;
  lSelectedBank: TSampleBank;
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
    FSampleView.Update(THybridPersistentModel(FSampleView.ModelObject));
    {FSampleView.dcCutoff.Value := TSample(Subject).Filter.Frequency;
    FSampleView.dcResonance.Value := TSample(Subject).Filter.Resonance;

    FSampleView.dcOsc1Pitch.Value := TSample(Subject).Osc1.Pitch;
    FSampleView.dcOsc1ModAmount.Value := TSample(Subject).Osc1.ModAmount;
    FSampleView.dcOSC1Level.Value := TSample(Subject).Osc1.Level;

    FSampleView.dcOsc2Pitch.Value := TSample(Subject).Osc2.Pitch;
    FSampleView.dcOsc2ModAmount.Value := TSample(Subject).Osc2.ModAmount;
    FSampleView.dcOSC2Level.Value := TSample(Subject).Osc2.Level;

    FSampleView.dcOsc3Pitch.Value := TSample(Subject).Osc3.Pitch;
    FSampleView.dcOsc3ModAmount.Value := TSample(Subject).Osc3.ModAmount;
    FSampleView.dcOSC3Level.Value := TSample(Subject).Osc3.Level; }
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

