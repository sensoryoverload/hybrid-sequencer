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
    dcAmpEnvelopeAttack1: TDialControl;
    dcAmpEnvelopeDecay1: TDialControl;
    dcAmpEnvelopeRelease1: TDialControl;
    dcAmpEnvelopeSustain1: TDialControl;
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

    FKeyboard: TSampleKeyboardControl;
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

  // filter
  dcCutoff.Tag := Integer(spFilter_Cutoff);
  dcCutoff.OnChange := @DoParameterChange;
  dcCutoff.OnStartChange := @DoParameterStartChange;
  dcCutoff.Value := dcCutoff.Highest;

  dcResonance.Tag := Integer(spFilter_Resonance);
  dcResonance.OnChange := @DoParameterChange;
  dcResonance.OnStartChange := @DoParameterStartChange;
  dcResonance.Value := dcResonance.Lowest;

  // osc 1
  dcOsc1Pitch.Tag := Integer(spOSC1_Pitch);
  dcOsc1Pitch.OnChange := @DoParameterChange;
  dcOsc1Pitch.OnStartChange := @DoParameterStartChange;
  dcOsc1Pitch.Lowest := -24;
  dcOsc1Pitch.Highest := 24;
  dcOsc1Pitch.Value := 0;

  dcOsc1ModAmount.Tag := Integer(spOSC1_ModAmount);
  dcOsc1ModAmount.OnChange := @DoParameterChange;
  dcOsc1ModAmount.OnStartChange := @DoParameterStartChange;
  dcOsc1ModAmount.Lowest := 0;
  dcOsc1ModAmount.Highest := 1;
  dcOsc1ModAmount.Value := 0;

  FillWaveSelectionComboBox(cbOsc1WaveSelector);
  FillModSourceComboBox(cbOsc1ModSource);

  // osc 2
  dcOsc2Pitch.Tag := Integer(spOSC2_Pitch);
  dcOsc2Pitch.OnChange := @DoParameterChange;
  dcOsc2Pitch.OnStartChange := @DoParameterStartChange;
  dcOsc2Pitch.Lowest := -24;
  dcOsc2Pitch.Highest := 24;
  dcOsc2Pitch.Value := 0;


  dcOsc2ModAmount.Tag := Integer(spOSC2_ModAmount);
  dcOsc2ModAmount.OnChange := @DoParameterChange;
  dcOsc2ModAmount.OnStartChange := @DoParameterStartChange;
  dcOsc2ModAmount.Lowest := 0;
  dcOsc2ModAmount.Highest := 1;
  dcOsc2ModAmount.Value := 0;

  FillWaveSelectionComboBox(cbOsc2WaveSelector);
  FillModSourceComboBox(cbOsc2ModSource);

  // osc 3
  dcOsc3Pitch.Tag := Integer(spOSC3_Pitch);
  dcOsc3Pitch.OnChange := @DoParameterChange;
  dcOsc3Pitch.OnStartChange := @DoParameterStartChange;
  dcOsc3Pitch.Lowest := -24;
  dcOsc3Pitch.Highest := 24;
  dcOsc3Pitch.Value := 0;

  dcOsc3ModAmount.Tag := Integer(spOSC3_ModAmount);
  dcOsc3ModAmount.OnChange := @DoParameterChange;
  dcOsc3ModAmount.OnStartChange := @DoParameterStartChange;
  dcOsc3ModAmount.Lowest := 0;
  dcOsc3ModAmount.Highest := 1;
  dcOsc3ModAmount.Value := 0;

  FillWaveSelectionComboBox(cbOsc3WaveSelector);
  FillModSourceComboBox(cbOsc3ModSource);
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
  dcOsc2Pitch.Value := TSample(Subject).Osc2.Pitch;
  dcOsc2ModAmount.Value := TSample(Subject).Osc2.ModAmount;
  dcOsc3Pitch.Value := TSample(Subject).Osc3.Pitch;
  dcOsc3ModAmount.Value := TSample(Subject).Osc3.ModAmount;

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
    FSampleView.dcCutoff.Value := TSample(Subject).Filter.Frequency;
    FSampleView.dcResonance.Value := TSample(Subject).Filter.Resonance;
    FSampleView.dcOsc1Pitch.Value := TSample(Subject).Osc1.Pitch;
    FSampleView.dcOsc1ModAmount.Value := TSample(Subject).Osc1.ModAmount;
    FSampleView.dcOsc2Pitch.Value := TSample(Subject).Osc2.Pitch;
    FSampleView.dcOsc2ModAmount.Value := TSample(Subject).Osc2.ModAmount;
    FSampleView.dcOsc3Pitch.Value := TSample(Subject).Osc3.Pitch;
    FSampleView.dcOsc3ModAmount.Value := TSample(Subject).Osc3.ModAmount;
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

