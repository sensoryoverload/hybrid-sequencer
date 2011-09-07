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
  global_command, ShellCtrls, pattern, LCLIntf, Menus, ActnList,
  audiostructure, patterngui;

type
  TPatternChangeEvent = procedure of object;

  TTrackGUI = class;

  { TTrackGUI }

  TTrackGUI = class(TFrame, IObserver)
    acDeleteTrack: TAction;
    acCreateWavePattern: TAction;
    acCreateMidiPattern: TAction;
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
    miCreateMidiPattern: TMenuItem;
    miCreateWavePattern: TMenuItem;
    miDeleteTrack: TMenuItem;
    pnlPatterns: TPanel;
    pnlTrackControls: TPanel;
    pmTrack: TPopupMenu;
    pupPatternList: TPopupMenu;
    tcOn: TToggleControl;
    vcLevel: TVolumeControl;
    procedure acCreateMidiPatternExecute(Sender: TObject);
    procedure acCreateWavePatternExecute(Sender: TObject);
    procedure acDeleteTrackExecute(Sender: TObject);
    procedure dcHighFreqChange(Sender: TObject);
    procedure miDeleteTrackClick(Sender: TObject);
    procedure pnlPatternsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure pnlPatternsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
    FTrack: TTrack;
    FObjectOwnerID: string;
    FObjectOwner: TObject;
    FClickLocationY: Integer;
    FModel: TTrack;

    procedure ReleasePattern(Data: PtrInt);
    procedure SetSelected(const AValue: Boolean);
    procedure CreatePatternGUI(AObjectID: string);
    procedure DeletePatternGUI(AObjectID: string);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce;
    procedure Connect;
    procedure Disconnect;
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string; virtual;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property ObjectID: string read GetObjectID write SetObjectID;
    property Selected: Boolean read FSelected write SetSelected;
    property OnTracksRefreshGUI: TTracksRefreshGUIEvent read FOnTracksRefreshGUI write FOnTracksRefreshGUI;
    property IsShuffling: Boolean read FShuffling write FShuffling;
    property Shuffle: TShuffle read FShuffle write FShuffle;
    property OnUpdateTrackControls: TNotifyEvent read FOnUpdateTrackControls write FOnUpdateTrackControls;
    property PanelPatterns: TPanel read pnlPatterns write pnlPatterns;
    property PatternListGUI: TObjectList read FPatternListGUI write FPatternListGUI;
    property SelectedPattern: TPatternGUI read FSelectedPattern write FSelectedPattern;
    property Track: TTrack read FTrack write FTrack;
    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
    function GetModel: THybridPersistentModel;
    procedure SetModel(AModel: THybridPersistentModel);
    property Model: THybridPersistentModel read GetModel write SetModel;
  end;


implementation

uses
  utils, wavepatterngui, midipatterngui, wave, midi;

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

  if Assigned(OnTracksRefreshGUI) then
  begin
    OnTracksRefreshGUI(nil);
  end;

  //Invalidate;

  DBLog('end TTrack.Update');
end;

procedure TTrackGUI.Connect;
begin
  //
end;

procedure TTrackGUI.Disconnect;
begin
  //
end;

function TTrackGUI.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TTrackGUI.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

function TTrackGUI.GetObjectOwnerID: string;
begin
  Result := FObjectOwnerID;
end;

procedure TTrackGUI.SetObjectOwnerID(const AObjectOwnerID: string);
begin
  FObjectOwnerID := AObjectOwnerID;
end;

function TTrackGUI.GetModel: THybridPersistentModel;
begin
  Result := THybridPersistentModel(FModel);
end;

procedure TTrackGUI.SetModel(AModel: THybridPersistentModel);
begin
  FModel := TTrack(AModel);
end;

procedure TTrackGUI.CreatePatternGUI(AObjectID: string);
var
  lWavePatternGUI: TWavePatternGUI;
  lWavePattern: TWavePattern;
  lMidiPatternGUI: TMidiPatternGUI;
  lMidiPattern: TMidiPattern;
  lPattern: TPattern;

begin
  DBLog('start TTrack.CreatePatternGUI' + AObjectID);

  // Get state from server
  lPattern := TPattern(GObjectMapper.GetModelObject(AObjectID));
  if Assigned(lPattern) then
  begin
    if lPattern.ClassType = 'TWavePattern' then
    begin
      lWavePattern := TWavePattern(GObjectMapper.GetModelObject(AObjectID));
      if Assigned(lWavePattern) then
      begin
        lWavePatternGUI := TWavePatternGUI.Create(Self);
        lWavePattern.Attach(lWavePatternGUI);
        lWavePatternGUI.Position := lPattern.Position;
        lWavePatternGUI.Text := lPattern.PatternName;
        lWavePatternGUI.OnTracksRefreshGUI := OnTracksRefreshGUI;
        lWavePatternGUI.Parent := nil;
        lWavePatternGUI.Parent := pnlPatterns;
        PatternListGUI.Add(lWavePatternGUI);
      end;
    end
    else if lPattern.ClassType = 'TMidiPattern' then
    begin
      lMidiPattern := TMidiPattern(GObjectMapper.GetModelObject(AObjectID));
      if Assigned(lMidiPattern) then
      begin
        lMidiPatternGUI := TMidiPatternGUI.Create(Self);
        lMidiPattern.Attach(lMidiPatternGUI);
        lMidiPatternGUI.Position := lPattern.Position;
        lMidiPatternGUI.Text := lPattern.PatternName;
        lMidiPatternGUI.OnTracksRefreshGUI := OnTracksRefreshGUI;
        lMidiPatternGUI.Parent := nil;
        lMidiPatternGUI.Parent := pnlPatterns;
        PatternListGUI.Add(lMidiPatternGUI);
      end;
    end;
  end;
  DBLog('end TTrack.CreatePatternGUI');
end;

procedure TTrackGUI.ReleasePattern(Data: PtrInt);
var
  lPatternGUI: TPatternGUI;
begin
  GSettings.SelectedPatternGUI := nil;

  lPatternGUI := TPatternGUI(Data);
  lPatternGUI.Parent := nil;
  PatternListGUI.Remove(lPatternGUI);
end;

procedure TTrackGUI.DeletePatternGUI(AObjectID: string);
var
  lPatternGUI: TPatternGUI;
  lWavePatternGUI: TWavePatternGUI;
  lMidiPatternGUI: TMidiPatternGUI;
  lIndex: Integer;
begin
  // update track gui
  GSettings.SelectedPatternGUI := nil;

  for lIndex := Pred(PatternListGUI.Count) downto 0 do
  begin
    lPatternGUI := TPatternGUI(PatternListGUI[lIndex]);
    if lPatternGUI.ObjectID = AObjectID then
    begin
      Application.QueueAsyncCall(@ReleasePattern, PtrInt(TMidiPatternGUI(lPatternGUI)));
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

//  Inherited DragDrop(Source, X, Y);

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

procedure TTrackGUI.pnlPatternsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FClickLocationY := Y;
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

procedure TTrackGUI.acCreateMidiPatternExecute(Sender: TObject);
var
  lCreateMidiPattern: TCreatePatternCommand;
begin
  lCreateMidiPattern := TCreatePatternCommand.Create(FObjectID);
  try
    lCreateMidiPattern.PatternName := '- midi -';
    lCreateMidiPattern.Position := (FClickLocationY div 15) * 15;
    lCreateMidiPattern.SourceType := fsMidi;

    GCommandQueue.PushCommand(lCreateMidiPattern);
  except
    lCreateMidiPattern.Free;
  end;
end;

procedure TTrackGUI.acCreateWavePatternExecute(Sender: TObject);
var
  lCreateWavePattern: TCreatePatternCommand;
begin
  lCreateWavePattern := TCreatePatternCommand.Create(FObjectID);
  try
    lCreateWavePattern.PatternName := '- audio -';
    lCreateWavePattern.Position := (FClickLocationY div 15) * 15;
    lCreateWavePattern.SourceType := fsWave;

    GCommandQueue.PushCommand(lCreateWavePattern);
  except
    lCreateWavePattern.Free;
  end;
end;

procedure TTrackGUI.dcHighFreqChange(Sender: TObject);
begin

end;

procedure TTrackGUI.miDeleteTrackClick(Sender: TObject);
begin
  // todo
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

