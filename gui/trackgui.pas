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

  trackgui.pas
}

unit trackgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, ExtCtrls, ComCtrls,
  dialcontrol, Controls, LCLType, Graphics, globalconst, ContNrs, global, track,
  global_command, ShellCtrls, pattern, patterngui, LCLIntf, Menus, ActnList,
  audiostructure;

type
  TPatternChangeEvent = procedure of object;

  TTrackGUI = class;

  { TTrackGUI }

  TTrackGUI = class(TFrame, IObserver)
    acDeleteTrack: TAction;
    ActionList1: TActionList;
    ComboBox1: TComboBox;
    cbOutput: TComboBox;
    DialControl1: TDialControl;
    dcLowFreq: TDialControl;
    dcLowLevel: TDialControl;
    dcHighFreq: TDialControl;
    dcMidFreq: TDialControl;
    dcMidLevel: TDialControl;
    dcHighLevel: TDialControl;
    miDeleteTrack: TMenuItem;
    pnlPatterns: TPanel;
    pnlTrackControls: TPanel;
    pmTrack: TPopupMenu;
    tcOn: TToggleControl;
    vcLevel: TVolumeControl;
    procedure acDeleteTrackExecute(Sender: TObject);
    procedure dcHighFreqChange(Sender: TObject);
    procedure pnlPatternsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Splitter2Moved(Sender: TObject);
    procedure TrackControlsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TrackControlsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure TrackControlsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlPatternsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tcOnChange(Sender: TObject);
    procedure vcLevelChange(Sender: TObject);
    procedure vcLevelStartChange(Sender: TObject);
  private
    { private declarations }
    FSelected: Boolean;
    FShuffleOldPos: TPoint;
    FShuffling: Boolean;
    FOnTracksRefreshGUI: TTracksRefreshGUIEvent;
    FPatternListGUI: TObjectList;
    FObjectID: string;
    FOnUpdateTrackControls: TNotifyEvent;
    FSelectedPattern: TPatternGUI;
    FShuffle: TShuffle;
    FModelObject: TObject;
    FObjectOwner: TObject;
    FObjectOwnerID: string;

    procedure SetSelected(const AValue: Boolean);
    procedure CreatePatternGUI(AObjectID: string);
    procedure DeletePatternGUI(AObjectID: string);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce;
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    property ObjectID: string read GetObjectID write SetObjectID;
    property Selected: Boolean read FSelected write SetSelected;
    property OnTracksRefreshGUI: TTracksRefreshGUIEvent read FOnTracksRefreshGUI write FOnTracksRefreshGUI;
    property IsShuffling: Boolean read FShuffling write FShuffling;
    property Shuffle: TShuffle read FShuffle write FShuffle;
    property OnUpdateTrackControls: TNotifyEvent read FOnUpdateTrackControls write FOnUpdateTrackControls;
    property PanelPatterns: TPanel read pnlPatterns write pnlPatterns;
    property PatternListGUI: TObjectList read FPatternListGUI write FPatternListGUI;
    property SelectedPattern: TPatternGUI read FSelectedPattern write FSelectedPattern;
    property ModelObject: TObject read FModelObject write FModelObject;
    property ObjectOwnerID: string read FObjectOwnerID write FObjectOwnerID;
    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
  end;


implementation

uses
  utils;

{ TTrackGUI }

procedure TTrackGUI.tcOnChange(Sender: TObject);
var
  lActivateTrack: TActivateTrackCommand;
begin
  DBLog('start Track On/Off ' + ObjectID);

  // Send Channel On/Off command
  lActivateTrack := TActivateTrackCommand.Create(ObjectID);
  try
    lActivateTrack.ActiveState := tcOn.SwitchedOn;

    GCommandQueue.PushCommand(lActivateTrack);
  except
    lActivateTrack.Free;
  end;

  DBLog('start Track On/Off ' + ObjectID);
end;

procedure TTrackGUI.vcLevelChange(Sender: TObject);
var
  lTrackLevelCommand: TTrackLevelCommand;
begin
  lTrackLevelCommand := TTrackLevelCommand.Create(ObjectID);
  try
    lTrackLevelCommand.Persist := False;
    lTrackLevelCommand.TrackLevel := vcLevel.Position;

    GCommandQueue.PushCommand(lTrackLevelCommand);
  except
    lTrackLevelCommand.Free;
  end;
end;

procedure TTrackGUI.vcLevelStartChange(Sender: TObject);
var
  lTrackLevelCommand: TTrackLevelCommand;
begin
  lTrackLevelCommand := TTrackLevelCommand.Create(ObjectID);
  try
    lTrackLevelCommand.Persist := True;
    lTrackLevelCommand.TrackLevel := vcLevel.Position;

    GCommandQueue.PushCommand(lTrackLevelCommand);
  except
    lTrackLevelCommand.Free;
  end;
end;


procedure TTrackGUI.SetSelected(const AValue: Boolean);
begin
  FSelected := AValue;

  {if FSelected then
    self.Color := clWhite
  else
    Self.Color := clLtGray;}

  Invalidate;
end;

procedure TTrackGUI.Update(Subject: THybridPersistentModel);
var
  i: Integer;
begin
  // Retrieve state
  DBLog('start TTrack.Update');

  DiffLists(
    TTrack(Subject).PatternList,
    PatternListGUI,
    @CreatePatternGUI,
    @DeletePatternGUI);

  tcOn.SwitchedOn := TTrack(Subject).Active;
  vcLevel.Position := TTrack(Subject).Volume;

  for i := 0 to Pred(Self.PatternListGUI.Count) do
  begin
    if Assigned(TTrack(Subject).SelectedPattern) then
    begin
      if TPatternGUI(Self.PatternListGUI[i]).ObjectID = TTrack(Subject).SelectedPattern.ObjectID then
      begin
        GSettings.SelectedPatternGUI := Self.PatternListGUI[i];
        Self.SelectedPattern := TPatternGUI(Self.PatternListGUI[i]);
        OnTracksRefreshGUI(Self);
        break;
      end;
    end;
  end;

  Invalidate;

  DBLog('end TTrack.Update');
end;

function TTrackGUI.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TTrackGUI.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

procedure TTrackGUI.CreatePatternGUI(AObjectID: string);
var
  lPatternGUI: TPatternGUI;
  lPattern: TPattern;
begin
  DBLog('start TTrack.CreatePatternGUI' + AObjectID);

  // Get state from server
  lPattern := TPattern(GObjectMapper.GetModelObject(AObjectID));
  if Assigned(lPattern) then
  begin
    lPatternGUI := TPatternGUI.Create(Self);
    lPatternGUI.ObjectID := AObjectID;
    lPatternGUI.ModelObject := lPattern;
    lPatternGUI.Model := lPattern;
    lPatternGUI.ObjectOwnerID := Self.ObjectID;
    lPatternGUI.Position := lPattern.Position;
    lPatternGUI.OnTracksRefreshGUI := OnTracksRefreshGUI;
    lPatternGUI.Parent := Self;
    lPatternGUI.Connect;

    PatternListGUI.Add(lPatternGUI);

    lPatternGUI.Text := lPattern.PatternName;
  end;
  DBLog('end TTrack.CreatePatternGUI');
end;

procedure TTrackGUI.DeletePatternGUI(AObjectID: string);
var
  lPatternGUI: TPatternGUI;
  lIndex: Integer;
begin
  // update track gui
  GSettings.SelectedPatternGUI := nil;

  for lIndex := Pred(PatternListGUI.Count) downto 0 do
  begin
    lPatternGUI := TPatternGUI(PatternListGUI[lIndex]);
    if lPatternGUI.ObjectID = AObjectID then
    begin
      try
        lPatternGUI.Disconnect;

      except
        on e:exception do
        begin
          DBLog('DEBUG: ' + e.message);
        end;
      end;
      PatternListGUI.Remove(lPatternGUI);
    end;
  end;
end;

constructor TTrackGUI.Create(AOwner: TComponent);
begin
  DBLog('start TTrack.Create');

  inherited Create(AOwner);

  FPatternListGUI := TObjectList.create(True);
  FShuffle := TShuffle.Create;
  tcOn.OnChange := @tcOnChange;

  ChangeControlStyle(Self, [csDisplayDragImage], [], True);

  DBLog('end TTrack.Create');
end;

destructor TTrackGUI.Destroy;
begin
  if Assigned(FPatternListGUI) then
    FPatternListGUI.Free;

  if Assigned(FShuffle) then
    FShuffle.Free;

  inherited Destroy;
end;

procedure TTrackGUI.pnlPatternsDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  P, Q:Tpoint;
  lCreatePattern: TCreatePatternCommand;
  lRepositionPattern: TRepositonPatternCommand;
  lMovePatternToTrackCommand: TMovePatternToTrackCommand;
  lTreeView: TTreeView;
  lPatternGUI: TPatternGUI;
  lPosition: Integer;
begin
  DBLog('start TTrack.pnlPatternsDragDrop');

  Inherited DragDrop(Source, X, Y);

  // Insert pattern in track from treeview-node
  if Source is TTreeView then
  begin
    lTreeView := TTreeView(Source);
    {
      If Source is wav file then create pattern
      else if Source is pattern then move pattern (or copy with Ctrl held)
    }
    lCreatePattern := TCreatePatternCommand.Create(Self.ObjectID);
    try
      lCreatePattern.PatternName := lTreeView.Selected.Text;
      lCreatePattern.Position := (Y div 15) * 15;
      lCreatePattern.SourceType := fsWave;
      lCreatePattern.SourceLocation := TTreeFolderData(lTreeView.Selected.Data).Path;

      GCommandQueue.PushCommand(lCreatePattern);
    except
      lCreatePattern.Free;
    end;
  end
  else
  // Move pattern inside of track
  if Source is TPatternGUI then
  begin
    GetCursorPos(P);
    Q := ScreenToControl(P);
    lPosition := (Q.Y div 15) * 15;

    lPatternGUI := TPatternGUI(Source);

    // Just update position inside track
    if lPatternGUI.ObjectOwnerID = ObjectID then
    begin
      lRepositionPattern := TRepositonPatternCommand.Create(Self.ObjectID);
      lRepositionPattern.ObjectID := lPatternGUI.ObjectID;
      try
        lRepositionPattern.Position := lPosition;

        GCommandQueue.PushCommand(lRepositionPattern);
      except
        lRepositionPattern.Free;
      end;
    end
    else
    begin
      // SourceTrack was original track containing pattern
      lMovePatternToTrackCommand := TMovePatternToTrackCommand.Create(Self.ObjectID);
      try
        lMovePatternToTrackCommand.SourceTrackID := lPatternGUI.ObjectOwnerID;
        lMovePatternToTrackCommand.TargetTrackID := ObjectID;
        lMovePatternToTrackCommand.PatternID := lPatternGUI.ObjectID;
        lMovePatternToTrackCommand.Position := lPosition;
        GCommandQueue.PushCommand(lMovePatternToTrackCommand);
      except
        lMovePatternToTrackCommand.Free;
      end;
    end;
  end;

  DBLog('end TTrack.pnlPatternsDragDrop');
end;

procedure TTrackGUI.TrackControlsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Selected:= True;

  FShuffling:= True;
  FShuffleOldPos.X:= X;
  FShuffleOldPos.Y:= Y;

  FShuffle.trackobject := Self;

  if Assigned(OnTracksRefreshGUI) then
    OnTracksRefreshGUI(Self);

  inherited Mousedown(Button, Shift, X, Y);
end;

procedure TTrackGUI.pnlPatternsDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);

  Accept := True;
end;

procedure TTrackGUI.acDeleteTrackExecute(Sender: TObject);
var
  lCommandDeleteTrack: TDeleteTrackCommand;
begin
  lCommandDeleteTrack := TDeleteTrackCommand.Create(ObjectID);
  try
    lCommandDeleteTrack.ObjectIdList.Add(ObjectID);

    GCommandQueue.PushCommand(lCommandDeleteTrack);
  except
    lCommandDeleteTrack.Free;
  end;
end;

procedure TTrackGUI.dcHighFreqChange(Sender: TObject);
begin

end;

procedure TTrackGUI.Splitter2Moved(Sender: TObject);
begin
  OnUpdateTrackControls(Self);
end;

procedure TTrackGUI.TrackControlsMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  if FShuffling then
  begin
    Self.Left:= Self.Left + (X - FShuffleOldPos.X);

    Shuffle.x := Self.Left;
  end;
end;

procedure TTrackGUI.TrackControlsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  FShuffling:= False;
end;

{ TSelectPatternUpdate }

{procedure TSelectPatternUpdate.DoExecute(AObject: TObject);
var
  lIndexPattern: Integer;
  lIndexID: Integer;
  lTrackGUI: TTrackGUI;
  lPatternGUI: TPatternGUI;
begin
  DBLog('start TSelectPatternUpdate.Execute');
  lTrackGUI := TTrackGUI(AObject);

  if Assigned(lTrackGUI) then
  begin
    for lIndexID := 0 to Pred(ObjectIDList.Count) do
    begin
      for lIndexPattern := 0 to Pred(lTrackGUI.PatternListGUI.Count) do
      begin
        lPatternGUI := TPatternGUI(lTrackGUI.PatternListGUI[lIndexPattern]);

        if lPatternGUI.ObjectID = ObjectIDList[lIndexID] then
        begin
          GSettings.SelectedPatternGUI := lPatternGUI;
          lTrackGUI.OnTracksRefreshGUI(lTrackGUI);
        end;
      end;
    end;
  end;

  DBLog('end TSelectPatternUpdate.Execute');
end;}


initialization
  {$I trackgui.lrs}
  RegisterClass(TTrackGUI);

end.

