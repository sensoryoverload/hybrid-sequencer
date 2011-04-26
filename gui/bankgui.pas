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

  bankgui.pas
}

unit bankgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, globalconst,
  sampler, samplegui, Controls, Contnrs, global, utils, Graphics, LCLType,
  StdCtrls, DBGrids, Grids;

type

  { TBankView }

  TBankView = class(TFrame, IObserver)
    gbSampleSelect: TGroupBox;
    lbSampleSelector: TListBox;
    procedure lbSampleSelectorClick(Sender: TObject);
    procedure lbSampleSelectorDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbSampleSelectorDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure sbSamplesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure sbSamplesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    { private declarations }
    FBank: TSampleBank;
    FSampleListGUI: TObjectList;
    FObjectOwnerID: string;
    FObjectID: string;
    FSelectedSampleGUI: TSampleView;
    FOldSelectedSample: string;

    FSampleView: TSampleView;
    procedure CreateSampleGUI(AObjectID: string);
    procedure DeleteSampleGUI(AObjectID: string);
    procedure DoChangeSelectedSample(Subject: THybridPersistentModel);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Update(Subject: THybridPersistentModel); reintroduce;
    function GetObjectOwnerID: string;
    procedure SetObjectOwnerID(const AValue: string);
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    property Bank: TSampleBank read FBank write FBank;
    property SampleListGUI: TObjectList read FSampleListGUI write FSampleListGUI;
    property ObjectID: string read GetObjectID write SetObjectID;
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
  end;

  TBankSelectControl = class(TPersistentCustomControl)
  private
    FCaption: string;
    FCaptionWidth: Integer;
    FOnChange: TChangeSelectSample;
    FSelected: Boolean;
    FSampleBank: TSampleBank;

    procedure SetCaption(const AValue: string);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure UpdateControl;
    procedure Update(Subject: THybridPersistentModel); reintroduce;
    property SampleBank: TSampleBank read FSampleBank write FSampleBank;
  published
    property Caption: string read FCaption write SetCaption;
    property OnChange: TChangeSelectSample read FOnChange write FOnChange;
    property Selected: Boolean read FSelected write FSelected;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
  end;

implementation

uses
  DialControl, ComCtrls, global_command;

{ TBankView }

constructor TBankView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSampleListGUI := TObjectList.Create;

  FSampleView := TSampleView.Create(nil);
  FSampleView.dcCutoff.Enabled := False;
  FSampleView.dcResonance.Enabled := False;
  FSampleView.dcOsc1Pitch.Enabled := False;
  FSampleView.dcOsc1ModAmount.Enabled := False;
  FSampleView.dcOsc2Pitch.Enabled := False;
  FSampleView.dcOsc2ModAmount.Enabled := False;
  FSampleView.dcOsc3Pitch.Enabled := False;
  FSampleView.dcOsc3ModAmount.Enabled := False;
  FSampleView.Parent := Self;

  ChangeControlStyle(Self, [csDisplayDragImage], [], True);
end;

destructor TBankView.Destroy;
begin
  if Assigned(FSampleView) then
    FSampleView.Free;

  if Assigned(FSampleListGUI) then
    FSampleListGUI.Free;

  inherited Destroy;
end;

procedure TBankView.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TBankView.Update');

  DiffLists(
    TSampleBank(Subject).SampleList,
    FSampleListGUI,
    @CreateSampleGUI,
    @DeleteSampleGUI);

  DoChangeSelectedSample(Subject);

  DBLog('end TBankView.Update');
end;

procedure TBankView.Connect;
begin
  Bank := TSampleBank(GObjectMapper.GetModelObject(ObjectID));
end;


function TBankView.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TBankView.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

procedure TBankView.sbSamplesDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  lTreeView: TTreeView;
  lCreateSampleCommand: TCreateSampleCommand;
begin
  if Source is TTreeView then
  begin
    lTreeView := TTreeView(Source);

    DBLog('lTreeView.Selected.Text: ' + lTreeView.Selected.Text);

    // Get tbank object
    lCreateSampleCommand := TCreateSampleCommand.Create(ObjectID);
    try
      lCreateSampleCommand.SampleLocation := lTreeView.Selected.Text;

      GCommandQueue.PushCommand(lCreateSampleCommand);
    except
      lCreateSampleCommand.Free;
    end;
  end;
end;

procedure TBankView.lbSampleSelectorDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True; // TODO True if valid sample
end;

procedure TBankView.lbSampleSelectorDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  lTreeView: TTreeView;
  lCreateSampleCommand: TCreateSampleCommand;
begin
  if Source is TTreeView then
  begin
    lTreeView := TTreeView(Source);

    DBLog('lTreeView.Selected.Text: ' + lTreeView.Selected.Text);

    // Get tbank object
    lCreateSampleCommand := TCreateSampleCommand.Create(ObjectID);
    try
      lCreateSampleCommand.SampleLocation := lTreeView.Selected.Text;

      GCommandQueue.PushCommand(lCreateSampleCommand);
    except
      lCreateSampleCommand.Free;
    end;
  end;
end;

procedure TBankView.lbSampleSelectorClick(Sender: TObject);
var
  lSelectedObjectID: string;
  lIndex: Integer;
  lSelectSampleCommand: TSelectSampleCommand;
begin
  // Search for selected (clicked) sample in listbox
  for lIndex := 0 to Pred(TListBox(Sender).Count) do
  begin
    if TListBox(Sender).Selected[lIndex] then
    begin
      lSelectedObjectID := TSampleSelectControl(TListBox(Sender).Items.Objects[lIndex]).ObjectID;
      break;
    end;
  end;

  if FOldSelectedSample <> lSelectedObjectID then
  begin
    // Command will toggle all selected samplecontrols to false and set this one
    // true; Also changes viewed sample editor
    lSelectSampleCommand := TSelectSampleCommand.Create(Self.ObjectID);
    try
      lSelectSampleCommand.ObjectID := lSelectedObjectID;
      lSelectSampleCommand.SelectedID := lSelectedObjectID;
      lSelectSampleCommand.Persist := True;

      GCommandQueue.PushCommand(lSelectSampleCommand);
    except
      lSelectSampleCommand.Free;
    end;
  end;
end;

procedure TBankView.sbSamplesDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := True; // TODO True if valid sample
end;

procedure TBankView.CreateSampleGUI(AObjectID: string);
var
  lSampleSelectControl: TSampleSelectControl;
  lSample: TSample;
begin
  DBLog('start TBankView.CreateSampleGUI');

  // Get state from server
  lSample := TSample(GObjectMapper.GetModelObject(AObjectID));
  if Assigned(lSample) then
  begin
    lSampleSelectControl := TSampleSelectControl.Create(Self.ObjectID);
    lSampleSelectControl.ObjectID := AObjectID;
    lSampleSelectControl.ObjectOwnerID := lSample.ObjectOwnerID;
    lSampleSelectControl.Caption := lSample.SampleLocation;
    lSampleSelectControl.SampleLocation := lSample.SampleLocation;
    lSampleSelectControl.Selected := lSample.Selected;
    lSampleSelectControl.SampleView := FSampleView;

    FSampleListGUI.Add(lSampleSelectControl);

    if lbSampleSelector.Items.IndexOfObject(lSampleSelectControl) = -1 then
    begin
      lbSampleSelector.Items.AddObject(lSampleSelectControl.SampleLocation, lSampleSelectControl);
    end;
  end;

  Invalidate;

  DBLog('end TBankView.CreateSampleGUI');
end;

procedure TBankView.DeleteSampleGUI(AObjectID: string);
var
  lSampleSelectControl: TSampleSelectControl;
  lIndex: Integer;
begin
  DBLog('start TBankView.DeleteSampleGUI');

  // update track gui
  FSelectedSampleGUI := nil;

  for lIndex := Pred(FSampleListGUI.Count) downto 0 do
  begin
    lSampleSelectControl := TSampleSelectControl(FSampleListGUI[lIndex]);

    if lSampleSelectControl.ObjectID = AObjectID then
    begin
      lbSampleSelector.Items.Delete(lbSampleSelector.Items.IndexOfObject(lSampleSelectControl));

      FSampleListGUI.Remove(lSampleSelectControl);
    end;
  end;

  Invalidate;

  DBLog('end TBankView.DeleteSampleGUI');
end;

procedure TBankView.DoChangeSelectedSample(Subject: THybridPersistentModel);
var
  lOldSelectedSample: TSample;
  lNewSelectedSample: TSample;
  lSampleIndex: Integer;
  lListBoxIndex: Integer;
  lNewObjectID: string;
begin
  DBLog('start TBankView.DoChangeSelectedSample');

  // Set listbox focus to selected sample (by TCommand from Model)
  for lSampleIndex := 0 to Pred(TSampleBank(Subject).SampleList.Count) do
  begin
    if TSample(TSampleBank(Subject).SampleList[lSampleIndex]).Selected then
    begin
      lNewObjectID := TSample(TSampleBank(Subject).SampleList[lSampleIndex]).ObjectID;

      for lListBoxIndex := 0 to Pred(lbSampleSelector.Count) do
      begin
        lbSampleSelector.Selected[lListBoxIndex] :=
          (TSampleSelectControl(lbSampleSelector.Items.Objects[lListBoxIndex]).ObjectID =
          lNewObjectID);
      end;

      break;
    end;
  end;

  if lNewObjectID = '' then exit;

  //
  for lSampleIndex := 0 to Pred(FSampleListGUI.Count) do
  begin
    TSampleSelectControl(FSampleListGUI[lSampleIndex]).Selected :=
      (TSampleSelectControl(FSampleListGUI[lSampleIndex]).ObjectID = lNewObjectID);
  end;

  // Can be unassigned when first used
  if FOldSelectedSample <> '' then
  begin
    lOldSelectedSample := TSample(GObjectMapper.GetModelObject(FOldSelectedSample));

    if Assigned(lOldSelectedSample) then
    begin
      // Release last sample connection
      lOldSelectedSample.Detach(FSampleView);
      FSampleView.dcCutoff.Enabled := False;
      FSampleView.dcResonance.Enabled := False;
      FSampleView.dcOsc1Pitch.Enabled := False;
      FSampleView.dcOsc1ModAmount.Enabled := False;
      FSampleView.dcOsc2Pitch.Enabled := False;
      FSampleView.dcOsc2ModAmount.Enabled := False;
      FSampleView.dcOsc3Pitch.Enabled := False;
      FSampleView.dcOsc3ModAmount.Enabled := False;
    end;
  end;

  lNewSelectedSample := TSample(GObjectMapper.GetModelObject(lNewObjectID));

  if Assigned(lNewSelectedSample) then
  begin
    FSampleView.ObjectID := lNewObjectID;
    FSampleView.ObjectOwnerID := ObjectID;

    // Attach new view
    lNewSelectedSample.Attach(FSampleView);
    FSampleView.dcCutoff.Enabled := True;
    FSampleView.dcResonance.Enabled := True;
    FSampleView.dcOsc1Pitch.Enabled := True;
    FSampleView.dcOsc1ModAmount.Enabled := True;
    FSampleView.dcOsc2Pitch.Enabled := True;
    FSampleView.dcOsc2ModAmount.Enabled := True;
    FSampleView.dcOsc3Pitch.Enabled := True;
    FSampleView.dcOsc3ModAmount.Enabled := True;

    FOldSelectedSample := lNewSelectedSample.ObjectID;
  end;

  Invalidate;

  DBLog('end TBankView.DoChangeSelectedSample');
end;

function TBankView.GetObjectOwnerID: string;
begin
  Result := FObjectOwnerID;
end;

procedure TBankView.SetObjectOwnerID(const AValue: string);
begin
  FObjectOwnerID := AValue;
end;

{ TBankSelectControl }

procedure TBankSelectControl.SetCaption(const AValue: string);
begin
  FCaption := AValue;
end;

constructor TBankSelectControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csDisplayDragImage];

  Constraints.MinWidth := 50;
  Constraints.MaxWidth := 50;
  Constraints.MinHeight := 30;
  Constraints.MaxHeight := 30;
  Width := 50;
  Height := 30;
end;

destructor TBankSelectControl.Destroy;
begin
  inherited Destroy;
end;

procedure TBankSelectControl.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

procedure TBankSelectControl.Paint;
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font.Size:= 7;

    // Initializes the Bitmap Size
    Bitmap.Height := Height;
    Bitmap.Width := Width;

    // Outline color
    Bitmap.Canvas.Pen.Style:= psSolid;
    Bitmap.Canvas.Pen.Color:= clBlack;

    Bitmap.Canvas.Brush.Color := clLtGray;
    Bitmap.Canvas.Rectangle(0, 0, Width, Height);

    // Switched on/off state color
    if FSelected then
      Bitmap.Canvas.Brush.Color := clLime
    else
      Bitmap.Canvas.Brush.Color := Color;

    FCaptionWidth := Canvas.TextWidth(FCaption);

    Bitmap.Canvas.Rectangle(3, 3, Width - 3, Height - 3);
    Bitmap.Canvas.TextOut((Width shr 1) - (FCaptionWidth shr 1), 1, FCaption);

    Canvas.Draw(0, 0, Bitmap);
  finally
    Bitmap.Free;
  end;

  inherited Paint;
end;

procedure TBankSelectControl.UpdateControl;
begin
  Repaint;
end;

procedure TBankSelectControl.Update(Subject: THybridPersistentModel);
begin
  writeln('start TBankSelectControl.Update');

  FCaption := TSample(Subject).SampleName;

  writeln('end TBankSelectControl.Update');
end;

procedure TBankSelectControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  case Button of
    mbLeft:
    begin
      FSelected := True;

      // Callback will toggle all selected samplecontrols to false and set this one
      // true; Also changes viewed sample editor
      if Assigned(FOnChange) then
      begin
        FOnChange(Self, ObjectID);
      end;
    end;
  end;
end;

procedure TBankSelectControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  Repaint;
end;

initialization
  {$I bankgui.lrs}

end.

