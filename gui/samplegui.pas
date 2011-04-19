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
    dcCutoff: TDialControl;
    dcResonance: TDialControl;
    pcSampler: TPageControl;
    TrackBar1: TTrackBar;
    tsGlobal: TTabSheet;
    tsModulators: TTabSheet;
    tsSample: TTabSheet;
    tsTuning: TTabSheet;
    procedure dcCutoffChange(Sender: TObject);
    procedure dcCutoffStartChange(Sender: TObject);
    procedure dcResonanceChange(Sender: TObject);
    procedure dcResonanceStartChange(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
  global, bankgui;

{ TSampleView }

procedure TSampleView.dcCutoffChange(Sender: TObject);
var
  lFilterCutoffCommand: TFilterCutoffCommand;
begin
  DBLog('dcCutoffChange %f', dcCutoff.Value);

  lFilterCutoffCommand := TFilterCutoffCommand.Create(ObjectOwnerID);
  try
    lFilterCutoffCommand.ObjectID := Self.ObjectID;
    lFilterCutoffCommand.CutoffValue := dcCutoff.Value;
    lFilterCutoffCommand.Persist := False;

    GCommandQueue.PushCommand(lFilterCutoffCommand);
  except
    lFilterCutoffCommand.Free;
  end;
end;

procedure TSampleView.dcCutoffStartChange(Sender: TObject);
var
  lFilterCutoffCommand: TFilterCutoffCommand;
begin
  DBLog('dcCutoffChange %f', dcCutoff.Value);

  lFilterCutoffCommand := TFilterCutoffCommand.Create(ObjectOwnerID);
  try
    lFilterCutoffCommand.ObjectID := Self.ObjectID;
    lFilterCutoffCommand.CutoffValue := dcCutoff.Value;
    lFilterCutoffCommand.Persist := True;

    GCommandQueue.PushCommand(lFilterCutoffCommand);
  except
    lFilterCutoffCommand.Free;
  end;
end;

procedure TSampleView.dcResonanceChange(Sender: TObject);
var
  lFilterCutoffCommand: TFilterCutoffCommand;
begin
  DBLog('dcCutoffChange %f', dcCutoff.Value);

  lFilterCutoffCommand := TFilterCutoffCommand.Create(ObjectOwnerID);
  try
    lFilterCutoffCommand.ObjectID := Self.ObjectID;
    lFilterCutoffCommand.CutoffValue := dcCutoff.Value;
    lFilterCutoffCommand.Persist := False;

    GCommandQueue.PushCommand(lFilterCutoffCommand);
  except
    lFilterCutoffCommand.Free;
  end;
end;

procedure TSampleView.dcResonanceStartChange(Sender: TObject);
var
  lFilterCutoffCommand: TFilterCutoffCommand;
begin
  DBLog('dcCutoffChange %f', dcCutoff.Value);

  lFilterCutoffCommand := TFilterCutoffCommand.Create(ObjectOwnerID);
  try
    lFilterCutoffCommand.ObjectID := Self.ObjectID;
    lFilterCutoffCommand.CutoffValue := dcCutoff.Value;
    lFilterCutoffCommand.Persist := True;

    GCommandQueue.PushCommand(lFilterCutoffCommand);
  except
    lFilterCutoffCommand.Free;
  end;
end;

procedure TSampleView.TrackBar1Change(Sender: TObject);
var
  lFilterCutoffCommand: TFilterCutoffCommand;
begin
  DBLog('dcCutoffChange %f', dcCutoff.Value);

  lFilterCutoffCommand := TFilterCutoffCommand.Create(ObjectOwnerID);
  try
    lFilterCutoffCommand.ObjectID := Self.ObjectID;
    lFilterCutoffCommand.CutoffValue := TrackBar1.Position;
    lFilterCutoffCommand.Persist := False;

    GCommandQueue.PushCommand(lFilterCutoffCommand);
  except
    lFilterCutoffCommand.Free;
  end;
end;

procedure TSampleView.TrackBar1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lFilterCutoffCommand: TFilterCutoffCommand;
begin
  DBLog('dcCutoffChange %f', dcCutoff.Value);

  lFilterCutoffCommand := TFilterCutoffCommand.Create(ObjectOwnerID);
  try
    lFilterCutoffCommand.ObjectID := Self.ObjectID;
    lFilterCutoffCommand.CutoffValue := TrackBar1.Position;
    lFilterCutoffCommand.Persist := True;

    GCommandQueue.PushCommand(lFilterCutoffCommand);
  except
    lFilterCutoffCommand.Free;
  end;
end;


constructor TSampleView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FKeyboard := TSampleKeyboardControl.Create(nil);
  FKeyboard.OnKeyChange := @DoKeyChange;
  FKeyboard.Height := 50;

  FKeyboard.Parent := Self;
  FKeyboard.Align := alBottom;
  pcSampler.Align := alClient;
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
  //TrackBar1.Position := Round(TSample(Subject).Filter.Frequency);

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

