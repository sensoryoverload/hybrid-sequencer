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

  midigui.pas
}

unit midigui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, LCLType, Graphics, Menus, globalconst,
  jacktypes, ContNrs, wave, global_command, midi, global, unix, BaseUNIX,
  lclintf, ActnList, pattern, plugin;

const
  DIVBY1000 = 1 / 1000;
  KEYS_PER_OCTAVE = 12;
  PIANO_WIDTH = 30;
  STRETCHNOTE_HOTSPOT = 20;

type
  TMidiGridOptions = set of (PianoKeyboard, DrumMap, MidiChannel, MidiNote);
  TKey = (keyBlack, keyWhite);
  TNoteAction = (naAdjustLeft, naAdjustRight, naDrag, naNone);
//  TZoomCallback = procedure(AZoomTimeLeft, AZoomTimeRight: Integer) of object;

  TMidiPatternGUI = class;

  { TMidiNoteGUI }

  TMidiNoteGUI = class(THybridPersistentView)
  private
    { GUI }
    FOriginalNoteLength: Integer;
    FOriginalNoteLocation: Integer;
    FOriginalNote: Integer;

    FMidiPattern: TMidiPatternGUI;

    { Audio }
    FNoteLocation: Integer; // Which time format ? in samples ??
    FNote: Integer; // 0..127
    FNoteVelocity: Integer; // 0..127
    FNoteLength: Integer;
    FSelected: Boolean;
    function QuantizeLocation(ALocation: Integer): Integer;
  public
    constructor Create(AObjectOwner: string); reintroduce;
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
    procedure Update(Subject: THybridPersistentModel); override;
    property Note: Integer read FNote write FNote;
    property NoteLocation: Integer read FNoteLocation write FNoteLocation;
    property OriginalNoteLocation: Integer read FOriginalNoteLocation write FOriginalNoteLocation;
    property OriginalNote: Integer read FOriginalNote write FOriginalNote;
    property NoteVelocity: Integer read FNoteVelocity write FNoteVelocity;
    property NoteLength: Integer read FNoteLength write FNoteLength;
    property OriginalNoteLength: Integer read FOriginalNoteLength write FOriginalNoteLength;
    property MidiGridGUI: TMidiPatternGUI read FMidiPattern write FMidiPattern;
  published
    property Selected: Boolean read FSelected write FSelected;
  end;

  { TMidiPatternGUI }

  TMidiPatternGUI = class(TFrame, IObserver)
    acDeleteNote: TAction;
    acDuplicate: TAction;
    acLoop: TAction;
    alNoteActions: TActionList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miDelete: TMenuItem;
    pmNoteMenu: TPopupMenu;
    procedure acDeleteNoteExecute(Sender: TObject);
    procedure acDuplicateExecute(Sender: TObject);
    procedure acLoopExecute(Sender: TObject);
    procedure FrameDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FrameDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FrameResize(Sender: TObject);
  private
    FSelectedController: Integer;
    FSelectedControllerEvent: TMidiData;

    FUpdateSubject: THybridPersistentModel;
    FIsDirty: Boolean;
    FForceRedraw: Boolean;

    FObjectID: string;
    FObjectOwnerID: string;
    FModel: TMidiPattern;

    { GUI }
    FOffset: Integer;
    FOldLocationOffset: Integer;

    FNoteOffset: Integer;
    FOldNoteOffset: Integer;
    FNoteInfoWidth: Integer;
    FEditMode: TEditMode;
    FSelectedAutomationDeviceId: string;
    FSelectedAutomationDevice: TAutomationDevice;
    FSelectedAutomationParameterId: string;
    FSelectedAutomationParameter: TAutomationDataList;
    FSelectedAutomationEvent: TAutomationData;

    // Zoom vars
    FZoomFactorX: Single;
    FZoomFactorY: Single;
    FZoomFactorToScreenX: Single;
    FZoomFactorToDataX: Single;
    FZoomFactorToScreenY: Single;
    FZoomFactorToDataY: Single;
    FOldZoomFactorX: Single;
    FOldZoomFactorY: Single;
    FZoomNoteHeight: Integer;

    // Coordinate vars
    FOldX: Integer;
    FOldY: Integer;
    FMouseX: Integer;
    FMouseY: Integer;
    FOrgNoteX: Integer;
    FOrgNoteY: Integer;
    FMouseNoteX: Integer;
    FMouseNoteY: Integer;

    FOldQuantizedNote: Integer;
    FOldQuantizedLocation: Integer;
    FOldNoteLengthDiff: Integer;

    FZoomingMode: Boolean;
    FBitmap: TBitmap;

    FRubberBandSelect: TRect;
    FRubberBandMode: Boolean;
    FOptionsView: TMidiGridOptions;

    FDragging: Boolean;
    FDraggedNote: TMidiNoteGUI;
    FDragMode: Short;

    FDraggingLoopMarker: Boolean;
    FDraggedLoopMarker: TLoopMarkerGUI;
    FMouseButton: TMouseButton;

    FNoteAction: TNoteAction;

    FNoteHighlightLocation: Integer;
    FNoteHighlightNote: Integer;

    { Audio }
    FRealCursorPosition: Integer;
    FCursorAdder: Single;
    FLoopStart: TLoopMarkerGUI;
    FLoopEnd: TLoopMarkerGUI;
    FLoopLength: TLoopMarkerGUI;
    FNoteListGUI: TObjectList;
    FQuantizeSetting: Integer;
    FQuantizeValue: Single;

    FOldCursorPosition: Integer;

    FRootNote: Integer;
    FMidiChannel: Integer;

    function GetEnabled: Boolean; reintroduce;

    procedure HandleControllerEditMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleControllerEditMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleControllerEditMouseMove(Shift: TShiftState; X, Y: Integer);

    procedure HandleLoopMarkerMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleLoopMarkerMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleLoopMarkerMouseMove(Shift: TShiftState; X, Y: Integer);

    procedure HandleNoteMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleNoteMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleNoteMouseMove(Shift: TShiftState; X, Y: Integer);

    procedure HandleAutomationEditMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleAutomationEditMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleAutomationEditMouseMove(Shift: TShiftState; X, Y: Integer);

    function LoopMarkerAt(X: Integer; AMargin: Single): TLoopMarkerGUI;
    function QuantizeLocation(ALocation: Integer): Integer;
    function NoteUnderCursor(AX, AY: Integer; var ANoteAction: TNoteAction): TMidiNoteGUI;
    function ConvertTimeToScreen(ATime: Integer): Integer;
    function ConvertScreenToTime(AX: Integer): Integer;
    function ConvertNoteToScreen(ANote: Integer): Integer;
    function ConvertScreenToNote(AY: Integer): Integer;
    procedure ReleaseNote(Data: PtrInt);
    procedure SetOffset(AValue: Integer);
    procedure SetQuantizeSetting(AValue: Integer);
    procedure SetSelectedAutomationDeviceId(AValue: string);
    procedure SetSelectedAutomationParameterId(AValue: string);
    procedure SetZoomFactorX(const AValue: Single);
    procedure SetZoomFactorY(const AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce;
    procedure UpdateView(AForceRedraw: Boolean = False);
    procedure Connect;
    procedure Disconnect;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    procedure HandleZoom(AZoomTimeLeft, AZoomTimeRight: Integer);

    property ObjectID: string read GetObjectID write SetObjectID;
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property Offset: Integer read FOffset write SetOffset;
    property NoteOffset: Integer read FNoteOffset write FNoteOffset;
    property NoteListGUI: TObjectList read FNoteListGUI write FNoteListGUI;
    property ZoomFactorX: Single read FZoomFactorX write SetZoomFactorX;
    property ZoomFactorY: Single read FZoomFactorY write SetZoomFactorY;
    property RealCursorPosition: Integer read FRealCursorPosition write FRealCursorPosition;
    property CursorAdder: Single read FCursorAdder write FCursorAdder;
    property LoopStart: TLoopMarkerGUI read FLoopStart write FLoopStart;
    property LoopEnd: TLoopMarkerGUI read FLoopEnd write FLoopEnd;
    property LoopLength: TLoopMarkerGUI read FLoopLength write FLoopLength;
    property Enabled: Boolean read GetEnabled;
    property QuantizeSetting: Integer read FQuantizeSetting write SetQuantizeSetting default 1;
    property QuantizeValue: Single read FQuantizeValue write FQuantizeValue default 22050;
    property RootNote: Integer read FRootNote write FRootNote default 0;
    property MidiChannel: Integer read FMidiChannel write FMidiChannel;
    property EditMode: TEditMode read FEditMode write FEditMode;
    property SelectedAutomationDeviceId: string read FSelectedAutomationDeviceId write SetSelectedAutomationDeviceId;
    property SelectedAutomationParameterId: string read FSelectedAutomationParameterId write SetSelectedAutomationParameterId;
    property SelectedController: Integer read FSelectedController write FSelectedController;
    property IsDirty: Boolean read FIsDirty write FIsDirty;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure MouseLeave; override;
    procedure DoOnResize; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure CreateNoteGUI(AObjectID: string);
    procedure DeleteNoteGUI(AObjectID: string);
    function NoteByObjectID(AObjectID: string): TMidiNoteGUI;
    procedure NoteListByRect(AObjectIDList: TStringList; ARect: TRect);
    function GetModel: THybridPersistentModel;
    procedure SetModel(AModel: THybridPersistentModel);
  end;

implementation

uses
  utils, appcolors, sampler, ComCtrls;

{ TMidiNoteGUI }

constructor TMidiNoteGUI.Create(AObjectOwner: string);
begin
  inherited Create(AObjectOwner);

  FOriginalNoteLength:= 0;
  FSelected:= False;
end;

destructor TMidiNoteGUI.Destroy;
begin
  inherited Destroy;
end;

procedure TMidiNoteGUI.Connect;
begin
  inherited Connect;
end;

procedure TMidiNoteGUI.Disconnect;
begin
  inherited Disconnect;
end;

procedure TMidiNoteGUI.Update(Subject: THybridPersistentModel);
var
  lNote: TMidiNote;
begin
  DBLog('start TMidiNoteGUI.Update');

  lNote := TMidiNote(Subject);

  if Assigned(lNote) then
  begin
    Selected := lNote.Selected;
    NoteLocation := lNote.NoteLocation;
    Note := lNote.Note;
    NoteVelocity := lNote.NoteVelocity;
    NoteLength := lNote.NoteLength;
  end;

  DBLog('end TMidiNoteGUI.Update');
end;

procedure TMidiPatternGUI.acDeleteNoteExecute(Sender: TObject);
var
  lDeleteNoteCommand: TDeleteNotesCommand;
begin
  lDeleteNoteCommand := TDeleteNotesCommand.Create(Self.ObjectID);
  try
    lDeleteNoteCommand.ObjectID := FDraggedNote.ObjectID;

    GCommandQueue.PushCommand(lDeleteNoteCommand);
  except
    lDeleteNoteCommand.Free;
  end;
end;

procedure TMidiPatternGUI.acDuplicateExecute(Sender: TObject);
begin
  // Duplicate after current selection end point
end;

procedure TMidiPatternGUI.acLoopExecute(Sender: TObject);
begin
  // Set loop to current selection
end;

procedure TMidiPatternGUI.FrameDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  lSucces: Boolean;
  lNote: Integer;
  lTreeView: TTreeView;
  lCreateNoteCommand: TCreateNotesCommand;
  lCreateSampleCommand: TCreateSampleCommand;
begin
  {
    Drop samples to midigrid instantely creates a sample in the current samplebank
    for this midigrid.
  }
  if Source is TTreeView then
  begin
    lTreeView := TTreeView(Source);

    DBLog('lTreeView.Selected.Text: ' + lTreeView.Selected.Text);

    lSucces := False;

    lCreateNoteCommand := TCreateNotesCommand.Create(Self.ObjectID);
    try
      if FQuantizeValue = 0 then
        lCreateNoteCommand.NoteLength := 22050
      else
        lCreateNoteCommand.NoteLength := Round(FQuantizeValue);

      lNote := ConvertScreenToNote(Y - FNoteOffset);
      lCreateNoteCommand.Note := lNote;
      lCreateNoteCommand.Location := QuantizeLocation(ConvertScreenToTime(X - FOffset));

      GCommandQueue.PushCommand(lCreateNoteCommand);

      lSucces := True;
    except
      lCreateNoteCommand.Free;
    end;

    // Create sample in sampler based on dragged waveform
    // set low, high and base -note to 'lCreateNoteCommand.Note' value
    if lSucces then
    begin
//      lCreateSampleCommand := TCreateSampleCommand.Create(TMidiPattern(FModel).SampleBank.ObjectID);
      try
        lCreateSampleCommand.SampleLocation := lTreeView.Selected.Text;
        lCreateSampleCommand.LowNote := lNote;
        lCreateSampleCommand.HighNote := lNote;
        lCreateSampleCommand.BaseNote := lNote;

        GCommandQueue.PushCommand(lCreateSampleCommand);
      except
        lCreateSampleCommand.Free;
      end;
    end;
  end;
end;

procedure TMidiPatternGUI.FrameDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  // TODO test if source object is a valid sample
  Accept := True;
end;

procedure TMidiPatternGUI.FrameResize(Sender: TObject);
begin
  // Hmmm..self initialize..
  ZoomFactorY := FZoomFactorY;
end;

procedure TMidiPatternGUI.HandleLoopMarkerMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lUpdateLoopMarkerCommand: TUpdateLoopMarkerCommand;
begin
  lUpdateLoopMarkerCommand := TUpdateLoopMarkerCommand.Create(Self.ObjectID);
  try
    lUpdateLoopMarkerCommand.DataType := FDraggedLoopMarker.DataType;
    lUpdateLoopMarkerCommand.Persist := True;
    lUpdateLoopMarkerCommand.Location := Round(ConvertScreenToTime(X - FOffset));
    dblog('lUpdateLoopMarkerCommand.Location = %d',lUpdateLoopMarkerCommand.Location);
    GCommandQueue.PushCommand(lUpdateLoopMarkerCommand);
  except
    lUpdateLoopMarkerCommand.Free;
  end;
end;

function TMidiPatternGUI.GetEnabled: Boolean;
begin
  Result := TMidiPattern(FModel).Enabled;
end;

procedure TMidiPatternGUI.HandleNoteMouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  lSelectNoteCommand: TSelectNoteCommand;
  lMoveNotesCommand: TMoveNotesCommand;
  lStretchNotesCommand: TStretchNotesCommand;
  lMousePos: TPoint;
begin
  FMouseButton := Button;
  FDragging:= True;
  FOrgNoteX := X;
  FOrgNoteY := Y;
  FMouseNoteX := X;
  FMouseNoteY := Y;

  // Set original values before move
  FDraggedNote.OriginalNoteLength := FDraggedNote.NoteLength;
  FDraggedNote.OriginalNoteLocation := FDraggedNote.NoteLocation;
  FDraggedNote.OriginalNote := FDraggedNote.Note;

  if FNoteAction = naAdjustRight then
  begin
    FDragMode := ndLength;
  end
  else
  begin
    FDragMode := ndMove;
  end;
  case Button of
    mbLeft:
    begin
      if (ssShift in GSettings.Modifier) then
      begin
        FDraggedNote.Selected := True;
      end;

      // First select note
      lSelectNoteCommand := TSelectNoteCommand.Create(Self.ObjectID);
      try
        lSelectNoteCommand.AddMode := (ssShift in GSettings.Modifier);
        lSelectNoteCommand.ObjectID := FDraggedNote.ObjectID;

        GCommandQueue.PushCommand(lSelectNoteCommand);
      except
        lSelectNoteCommand.Free;
      end;

      case FDragMode of
        ndMove:
        begin
          lMoveNotesCommand := TMoveNotesCommand.Create(Self.ObjectID);
          try
            lMoveNotesCommand.Persist := True;
            lMoveNotesCommand.ObjectID := FDraggedNote.ObjectID;
            lMoveNotesCommand.NoteDiff := 0;
            lMoveNotesCommand.NoteLocationDiff := 0;

            GCommandQueue.PushCommand(lMoveNotesCommand);
          except
            lMoveNotesCommand.Free;
          end;
        end;
        ndLength:
        begin
          lStretchNotesCommand := TStretchNotesCommand.Create(Self.ObjectID);
          try
            lStretchNotesCommand.Persist := True;
            lStretchNotesCommand.ObjectID := FDraggedNote.ObjectID;
            lStretchNotesCommand.NoteLengthDiff := 0;

            GCommandQueue.PushCommand(lStretchNotesCommand);
          except
            lStretchNotesCommand.Free;
          end;
        end;
      end;
    end;
    mbRight:
    begin
      GetCursorPos(lMousePos);
      pmNoteMenu.PopUp(lMousePos.X, lMousePos.Y);
    end;
  end;
end;

procedure TMidiPatternGUI.HandleNoteMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FDragging:= False;
  FDraggedNote := nil;
  FNoteAction := naNone;
end;

procedure TMidiPatternGUI.HandleAutomationEditMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  function FindAutomationEvent(X, Y: Integer): TAutomationData;
  var
    lEventX: Integer;
    lEventY: Integer;
    lAutomationData: TAutomationData;
  begin
    Result := nil;

    if Assigned(FSelectedAutomationParameter) then
    begin
      FSelectedAutomationParameter.First;
      while not FSelectedAutomationParameter.Eof do
      begin
        lAutomationData := FSelectedAutomationParameter.CurrentAutomationData;

        lEventX := ConvertTimeToScreen(lAutomationData.Location) + FOffset;
        lEventY := Round(Height - lAutomationData.DataValue * Height);
        if (Abs(X - lEventX) < 6) and (Abs(Y - lEventY) < 6) then
        begin
          Result := FSelectedAutomationParameter.CurrentAutomationData;
          break;
        end;

        FSelectedAutomationParameter.Next;
      end;
    end;
  end;

begin
  FSelectedAutomationEvent := FindAutomationEvent(X, Y);
end;

procedure TMidiPatternGUI.HandleAutomationEditMouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  lEditAutomationDataCommand: TEditAutomationDataCommand;
begin
  if Assigned(FSelectedAutomationEvent) then
  begin
    lEditAutomationDataCommand := TEditAutomationDataCommand.Create(Self.ObjectID);
    try
      lEditAutomationDataCommand.DeviceId := FSelectedAutomationDeviceId;
      lEditAutomationDataCommand.ParameterId := FSelectedAutomationParameterId;
      lEditAutomationDataCommand.Location := Round(ConvertScreenToTime(X - FOffset));
      lEditAutomationDataCommand.DataValue := (Height - Y) / Height;
      lEditAutomationDataCommand.ObjectID := FSelectedAutomationEvent.ObjectID;
      lEditAutomationDataCommand.Persist := False;

      GCommandQueue.PushCommand(lEditAutomationDataCommand);
    except
      lEditAutomationDataCommand.Free;
    end;
  end;
end;

procedure TMidiPatternGUI.HandleAutomationEditMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lCreateAutomationDataCommand: TCreateAutomationDataCommand;
  lEditAutomationDataCommand: TEditAutomationDataCommand;
begin
  if Assigned(FSelectedAutomationEvent) then
  begin
    lEditAutomationDataCommand := TEditAutomationDataCommand.Create(Self.ObjectID);
    try
      lEditAutomationDataCommand.Location := Round(ConvertScreenToTime(X - FOffset));
      lEditAutomationDataCommand.DataValue := (Height - Y) / Height;
      lEditAutomationDataCommand.DeviceId := FSelectedAutomationDeviceId;
      lEditAutomationDataCommand.ParameterId := FSelectedAutomationParameterId;
      lEditAutomationDataCommand.ObjectID := FSelectedAutomationEvent.ObjectID;
      lEditAutomationDataCommand.Persist := True;

      GCommandQueue.PushCommand(lEditAutomationDataCommand);
    except
      lEditAutomationDataCommand.Free;
    end;

    FSelectedAutomationEvent := nil;
  end
  else
  begin
    // TODO check if there already is an automation event on this location
    lCreateAutomationDataCommand := TCreateAutomationDataCommand.Create(Self.ObjectID);
    try
      lCreateAutomationDataCommand.Location := Round(ConvertScreenToTime(X - FOffset));
      lCreateAutomationDataCommand.DataValue := (Height - Y) / Height;
      lCreateAutomationDataCommand.DeviceId := FSelectedAutomationDeviceId;
      lCreateAutomationDataCommand.ParameterId := FSelectedAutomationParameterId;

      GCommandQueue.PushCommand(lCreateAutomationDataCommand);
    except
      lCreateAutomationDataCommand.Free;
    end;
  end;
end;

procedure TMidiPatternGUI.HandleControllerEditMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lControllerEditCommand: TControllerEditCommand;

  function FindControllerEvent(X, Y: Integer): TMidiData;
  var
    lIndex: Integer;
    lEventX: Integer;
    lEventY: Integer;
    lValue: Integer;
    lMidiData: TMidiData;
    lMidiDataList: TMidiDataList;
  begin
    Result := nil;

    if Assigned(FModel.MidiDataList) then
    begin
      lMidiDataList := FModel.MidiDataList;
      lMidiDataList.First;
      while not lMidiDataList.Eof do
      begin
        lMidiData := FModel.MidiDataList.CurrentMidiData;
        if Assigned(lMidiData) then
        begin
          if lMidiData.DataType = mtNoteOn then
          begin
            lValue := lMidiData.DataValue2;
          end
          else if lMidiData.DataType = mtCC then
          begin
            lValue := lMidiData.DataValue1;
          end;
          lEventX := ConvertTimeToScreen(lMidiData.Location) + FOffset;
          lEventY := Round(Height - lValue * Height);
          if (Abs(X - lEventX) < 6) and (Abs(Y - lEventY) < 6) then
          begin
            Result := lMidiData;
            break;
          end;
        end;

        lMidiDataList.Next;
      end;
    end;
  end;

begin
  FSelectedControllerEvent := FindControllerEvent(X, Y);

  if Assigned(FSelectedControllerEvent) then
  begin
    lControllerEditCommand := TControllerEditCommand.Create(Self.ObjectID);
    try
      lControllerEditCommand.ObjectID := FSelectedControllerEvent.ParentObject.ObjectID;
      lControllerEditCommand.ControllerId := FSelectedController;
      lControllerEditCommand.Location := Round(ConvertScreenToTime(X - FOffset));
      lControllerEditCommand.DataValue := (Height - Y) div Height;
      lControllerEditCommand.Persist := True;

      GCommandQueue.PushCommand(lControllerEditCommand);
    except
      lControllerEditCommand.Free;
    end;
  end;
end;

procedure TMidiPatternGUI.HandleControllerEditMouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  lControllerEditCommand: TControllerEditCommand;
begin
  if Assigned(FSelectedControllerEvent) then
  begin
    lControllerEditCommand := TControllerEditCommand.Create(Self.ObjectID);
    try
      lControllerEditCommand.ObjectID := FSelectedControllerEvent.ParentObject.ObjectID;
      lControllerEditCommand.ControllerId := FSelectedController;
      lControllerEditCommand.Location := Round(ConvertScreenToTime(X - FOffset));
      lControllerEditCommand.DataValue := (Height - Y) div Height;
      lControllerEditCommand.Persist := False;

      GCommandQueue.PushCommand(lControllerEditCommand);
    except
      lControllerEditCommand.Free;
    end;
  end;
end;

procedure TMidiPatternGUI.HandleControllerEditMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lControllerCreateCommand: TControllerCreateCommand;
begin
  if not Assigned(FSelectedControllerEvent) then
  begin
    // TODO check if there already is an automation event on this location
    lControllerCreateCommand := TControllerCreateCommand.Create(Self.ObjectID);
    try
      lControllerCreateCommand.ControllerId := FSelectedController;
      lControllerCreateCommand.Location := Round(ConvertScreenToTime(X - FOffset));
      lControllerCreateCommand.DataValue := (Height - Y) div Height;
      lControllerCreateCommand.Persist := True;

      GCommandQueue.PushCommand(lControllerCreateCommand);
    except
      lControllerCreateCommand.Free;
    end;
  end;
end;

procedure TMidiPatternGUI.HandleLoopMarkerMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  lUpdateLoopMarkerCommand: TUpdateLoopMarkerCommand;
begin
  lUpdateLoopMarkerCommand := TUpdateLoopMarkerCommand.Create(Self.ObjectID);
  try
    lUpdateLoopMarkerCommand.DataType := FDraggedLoopMarker.DataType;
    lUpdateLoopMarkerCommand.Persist := False;
    lUpdateLoopMarkerCommand.Location := Round(ConvertScreenToTime(X - FOffset));

    GCommandQueue.PushCommand(lUpdateLoopMarkerCommand);
  except
    lUpdateLoopMarkerCommand.Free;
  end;

  FDraggingLoopMarker:= False;
  FDraggedLoopMarker := nil;
end;

procedure TMidiPatternGUI.HandleNoteMouseMove(Shift: TShiftState; X, Y: Integer);
var
  lMoveNotesCommand: TMoveNotesCommand;
  lStretchNotesCommand: TStretchNotesCommand;
  lNoteLocation: Integer;
  lNoteLengthDiff: Integer;
  lNote: Integer;
begin
  FMouseNoteX := X;
  FMouseNoteY := Y;

  if (FMouseButton = mbLeft) and FDragging then
  begin
    lNoteLocation := QuantizeLocation(ConvertScreenToTime(X - FOffset));
    lNoteLengthDiff := ConvertScreenToTime(X + PIANO_WIDTH - FOrgNoteX);
    lNote := ConvertScreenToNote(Y - FNoteOffset);

    if (FOldQuantizedNote <> lNote) or (FOldQuantizedLocation <> lNoteLocation) or
      (FOldNoteLengthDiff <> lNoteLengthDiff) then
    begin
      case FDragMode of
        ndLength: // Change length of note
        begin
          lStretchNotesCommand := TStretchNotesCommand.Create(Self.ObjectID);
          try
            lStretchNotesCommand.ObjectID := FDraggedNote.ObjectID;
            lStretchNotesCommand.Persist := False;
            lStretchNotesCommand.NoteLengthDiff := lNoteLengthDiff;
            GCommandQueue.PushCommand(lStretchNotesCommand);
          except
            lStretchNotesCommand.Free;
          end;
        end;
        ndMove: // Move note
        begin
          lMoveNotesCommand := TMoveNotesCommand.Create(Self.ObjectID);
          try
            lMoveNotesCommand.ObjectID := FDraggedNote.ObjectID;
            lMoveNotesCommand.Persist := False;
            lMoveNotesCommand.NoteDiff := lNote - FDraggedNote.OriginalNote;
            lMoveNotesCommand.NoteLocationDiff := lNoteLocation - FDraggedNote.OriginalNoteLocation;

            GCommandQueue.PushCommand(lMoveNotesCommand);
          except
            lMoveNotesCommand.Free;
          end;
        end;
      end;
      FOldQuantizedLocation := lNoteLocation;
      FOldQuantizedNote := lNote;
      FOldNoteLengthDiff := lNoteLengthDiff;
    end;
  end;
end;

procedure TMidiPatternGUI.HandleLoopMarkerMouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  lValue: Integer;
begin
  lValue := Round(ConvertScreenToTime(X - FOffset));

  case FDraggedLoopMarker.DataType of
  ltStart:
  begin
    if lValue < 0 then lValue := 0;
    FDraggedLoopMarker.Location := lValue;
    LoopEnd.Location := LoopStart.Location + LoopLength.Location;
  end;
  ltEnd:
  begin
    if lValue < 0 then lValue := 0;
    FDraggedLoopMarker.Location := lValue;
    LoopLength.Location := LoopEnd.Location - LoopStart.Location;
  end;
  ltLength:
  begin
    FDraggedLoopMarker.Location := lValue;
    LoopEnd.Location := LoopStart.Location + lValue;
  end;
  end;
end;

function TMidiNoteGUI.QuantizeLocation(ALocation: Integer): Integer;
begin
  if MidiGridGUI.QuantizeSetting = 0 then
  begin
    Result := ALocation;
  end
  else
  begin
    Result := Round(Trunc(ALocation / MidiGridGUI.QuantizeValue) * MidiGridGUI.QuantizeValue);
  end;
end;

procedure TMidiPatternGUI.SetZoomFactorX(const AValue: Single);
var
  lFramesPerScreenWidth: Single;
begin
  FZoomFactorX := AValue;

  lFramesPerScreenWidth := LoopEnd.Location / (Width - FNoteInfoWidth);

  if FZoomFactorX = 0 then FZoomFactorX := 0.01;
  FZoomFactorToScreenX := ZoomFactorX / lFramesPerScreenWidth;
  FZoomFactorToDataX := lFramesPerScreenWidth / ZoomFactorX;
end;

procedure TMidiPatternGUI.SetZoomFactorY(const AValue: Single);
begin
  FZoomFactorY := AValue;
  if FZoomFactorY <= 0 then FZoomFactorY := 1;
  if FZoomFactorY > 2000 then FZoomFactorY := 2000;
  FZoomFactorToScreenY := (ZoomFactorY / 1000);
  FZoomFactorToDataY := (1000 / ZoomFactorY);
  FZoomNoteHeight := Round((Height / 32) * FZoomFactorToScreenY);
end;

procedure TMidiPatternGUI.HandleZoom(AZoomTimeLeft, AZoomTimeRight: Integer);
begin
  ZoomFactorX := 1000000 / (AZoomTimeRight - AZoomTimeLeft);
  FOffset := 0 - ConvertTimeToScreen(AZoomTimeLeft);

  FIsDirty := True;

  Invalidate;
end;


constructor TMidiPatternGUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIsDirty := False;

  FLoopStart := TLoopMarkerGUI.Create(ObjectID, ltStart);
  FLoopEnd := TLoopMarkerGUI.Create(ObjectID, ltEnd);
  FLoopLength := TLoopMarkerGUI.Create(ObjectID, ltLength);

  FOptionsView := [PianoKeyboard];

  if PianoKeyboard in FOptionsView then
  begin
    FNoteInfoWidth := PIANO_WIDTH;
  end
  else
  begin
    FNoteInfoWidth := 0;
  end;

  FEditMode := emPatternEdit;

  FBitmap := TBitmap.Create;

  FRealCursorPosition:= FLoopStart.Location;
  FRubberBandMode := False;

  FNoteListGUI := TObjectList.Create(True);
  ZoomFactorX := 1000;
  ZoomFactorY := 1000;

  FDragging := False;

  {ChangeControlStyle(Self, [csDisplayDragImage], [], True);}
end;

destructor TMidiPatternGUI.Destroy;
begin
  if Assigned(FNoteListGUI) then
    FNoteListGUI.Free;

  FBitmap.Free;

  FLoopStart.Free;
  FLoopEnd.Free;
  FLoopLength.Free;

  inherited Destroy;
end;

procedure TMidiPatternGUI.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

procedure TMidiPatternGUI.Paint;
var
  x: Integer;
  lTime: Single;
  lTimeSpacing: Single;
  lLastTime: Integer;
  lNewTime: Integer;
  lHighlightLocation: Integer;
  lHighlightNote: Integer;
  lHighlightWidth: Integer;
  lIndex: Integer;
  lNote: TMidiNoteGUI;
  lNoteIndex: Integer;
  lMidiNoteModula: Integer;
  lMidiNoteKey: TKey;
  lAutomationData: TAutomationData;
  lAutomationScreenY: Integer;
  lAutomationScreenX: Integer;
  lNoteColor: TColor;
  lSelectedNoteColer: TColor;
  lNoteOutline: TColor;
  lNoteX1: Integer;
  lNoteY1: Integer;
  lNoteX2: Integer;
  lNoteY2: Integer;
  lControllerToScreenRatio: Single;
  lControllerYLocation: Integer;
begin
  if not Assigned(FModel) then exit;

  if FIsDirty or FForceRedraw then
  begin
    if FIsDirty then
    begin
      FIsDirty := False;
    end;

    if FForceRedraw then
    begin
      FForceRedraw := False;
    end;

    FBitmap.Height := Height;
    FBitmap.Width := Width;
    FBitmap.Canvas.Pen.Width := 1;

    // Draw default background
    if FEditMode = emPatternEdit then
    begin
      FBitmap.Canvas.Brush.Color := clMidigridBackgroundWhite;
      FBitmap.Canvas.Pen.Color := clMidigridBackgroundWhite;
      lSelectedNoteColer := clGreen;
      lNoteColor := clLime;
      lNoteOutline := clBlack;
    end
    else
    begin
      FBitmap.Canvas.Brush.Color := clMidigridBackgroundDark;
      FBitmap.Canvas.Pen.Color := clMidigridBackgroundDark;
      lSelectedNoteColer := clDarkGreen;
      lNoteColor := clDarkLime;
      lNoteOutline := clDarkOutline;
    end;
    FBitmap.Canvas.Rectangle(0, 0, FBitmap.Width, FBitmap.Height);

    // Draw note indictor lines
    FBitmap.Canvas.Pen.Color := clMidigridBackgroundBlack;
    FBitmap.Canvas.Brush.Color := clMidigridBackgroundBlack;
    lMidiNoteModula := 0;
    for lNoteIndex := 0 to 127 do
    begin
      case lMidiNoteModula of
       0: lMidiNoteKey := keyWhite;
       1: lMidiNoteKey := keyBlack;
       2: lMidiNoteKey := keyWhite;
       3: lMidiNoteKey := keyBlack;
       4: lMidiNoteKey := keyWhite;
       5: lMidiNoteKey := keyWhite;
       6: lMidiNoteKey := keyBlack;
       7: lMidiNoteKey := keyWhite;
       8: lMidiNoteKey := keyBlack;
       9: lMidiNoteKey := keyWhite;
      10: lMidiNoteKey := keyBlack;
      11: lMidiNoteKey := keyWhite;
      end;

      if lMidiNoteKey = keyBlack then
      begin
        FBitmap.Canvas.Rectangle(
          PIANO_WIDTH,
          ConvertNoteToScreen(lNoteIndex) + FNoteOffset,
          FBitmap.Width,
          ConvertNoteToScreen(lNoteIndex) + FZoomNoteHeight + FNoteOffset);
      end;

      Inc(lMidiNoteModula);
      if lMidiNoteModula = KEYS_PER_OCTAVE then
      begin
        lMidiNoteModula := 0;
      end;
    end;

    // Draw vertica time indicators
    FBitmap.Canvas.Pen.Color := clGlobalOutline;

    lTimeSpacing := QuantizeValue;
    if lTimeSpacing < 1 then
      lTimeSpacing := 22050;

    {if FZoomFactorX < 50 then
      lTimeSpacing := lTimeSpacing * 2;

    if FZoomFactorX < 25 then
      lTimeSpacing := lTimeSpacing * 2;

    if FZoomFactorX < 12 then
      lTimeSpacing := lTimeSpacing * 2;

    if FZoomFactorX < 6.25 then
      lTimeSpacing := lTimeSpacing * 2;

    if FZoomFactorX < 3.125 then
      lTimeSpacing := lTimeSpacing * 2;

    if FZoomFactorX < 1.5625 then
      lTimeSpacing := lTimeSpacing * 2;}

    if FZoomFactorX < 0.78125 then
      lTimeSpacing := lTimeSpacing * 2;

    if FZoomFactorX < 0.390625 then
      lTimeSpacing := lTimeSpacing * 2;

    lLastTime := -1;
    repeat
      x := ConvertTimeToScreen(Round(lTime)) + FOffset;
      lNewTime := Round(lTime) div Round(GSettings.HalfSampleRate) + 1;
      if x < FBitmap.Width then
      begin
        FBitmap.Canvas.Pen.Color := clGray;
        FBitmap.Canvas.Line(x, 0, x, FBitmap.Height);
        if lLastTime <> lNewTime then
        begin
          FBitmap.Canvas.TextOut(x + 2, 1, Format('%d', [lNewTime]));
        end;
      end;
      lLastTime := lNewTime;
      lTime := lTime + lTimeSpacing;
    until x >= FBitmap.Width;

    // Draw note cursor box
    FBitmap.Canvas.Pen.Color := clCream;
    FBitmap.Canvas.Brush.Color := clCream;
    lHighlightLocation := ConvertTimeToScreen(FNoteHighlightLocation) + FOffset;
    lHighlightNote := ConvertNoteToScreen(FNoteHighlightNote) + FNoteOffset;
    lHighlightWidth := Round(FQuantizeValue * FZoomFactorToScreenX);

    FBitmap.Canvas.Rectangle(
      lHighlightLocation,
      lHighlightNote,
      lHighlightLocation + lHighlightWidth,
      lHighlightNote + FZoomNoteHeight);

    // Draw rubberband selector with dashed outline
    if FRubberBandMode then
    begin
      FBitmap.Canvas.Brush.Style := bsClear;
      FBitmap.Canvas.Pen.Color := clBlack;
      FBitmap.Canvas.Pen.Style := psDash;
      FBitmap.Canvas.Rectangle(FRubberBandSelect);
      FBitmap.Canvas.Brush.Style := bsSolid;
      FBitmap.Canvas.Pen.Style := psSolid;
    end;

    // Draw midi notes
    FBitmap.Canvas.Pen.Color := lNoteOutline;
    FBitmap.Canvas.Brush.Color := lNoteOutline;

    lControllerToScreenRatio := FBitmap.Height / 128;

    for lIndex := 0 to Pred(NoteListGUI.Count) do
    begin
      lNote := TMidiNoteGUI(NoteListGUI[lIndex]);

      lNoteX1 := ConvertTimeToScreen(lNote.NoteLocation) + FOffset;
      lNoteY1 := ConvertNoteToScreen(lNote.Note) + FNoteOffset;
      lNoteX2 := ConvertTimeToScreen(lNote.NoteLocation + lNote.NoteLength) + FOffset;
      lNoteY2 := ConvertNoteToScreen(lNote.Note) + FZoomNoteHeight + FNoteOffset;

      if ((FMouseX >= lNoteX1) and (FMouseX <= lNoteX2) and
         (FMouseY >= lNoteY1) and (FMouseY <= lNoteY2)) or
         (lNote = FDraggedNote) then
      begin
        FBitmap.Canvas.Brush.Color := clWhite;
      end
      else
      begin
        if lNote.Selected then
        begin
          FBitmap.Canvas.Brush.Color := lSelectedNoteColer;
        end
        else
        begin
          FBitmap.Canvas.Brush.Color := lNoteColor;
        end;
      end;

      FBitmap.Canvas.Rectangle(lNoteX1, lNoteY1, lNoteX2, lNoteY2);

      if FSelectedController = MIDI_VELOCITY then
      begin
        FBitmap.Canvas.Pen.Color := clGreen;
        FBitmap.Canvas.Pen.Width := 3;
        lControllerYLocation :=
          Round(FBitmap.Height - lControllerToScreenRatio * lNote.NoteVelocity);

        FBitmap.Canvas.Line(
          lNoteX1,
          lControllerYLocation,
          lNoteX1,
          FBitmap.Height);

        FBitmap.Canvas.Rectangle(
          lNoteX1 - 3,
          lControllerYLocation - 3,
          lNoteX1 + 3,
          lControllerYLocation + 3);

        FBitmap.Canvas.Pen.Width := 1;

      end;

      { note value debug code
      if FZoomNoteHeight > 8 then
      begin
        FBitmap.Canvas.TextOut(
          ConvertTimeToScreen(lNote.NoteLocation) + FOffset,
          ConvertNoteToScreen(lNote.Note) + FNoteOffset,
          Format('%d, %d', [lNote.Note, lNote.NoteLocation]));
      end; }
    end;

    // Draw keyboard
    if PianoKeyboard in FOptionsView then
    begin
      FBitmap.Canvas.Brush.Color := clMidigridBackgroundWhite;
      FBitmap.Canvas.Pen.Color := clMidigridBackgroundWhite;
      FBitmap.Canvas.Clipping := True;
      FBitmap.Canvas.Rectangle(0, 0, PIANO_WIDTH, FBitmap.Height);
      lMidiNoteModula := 0;
      for lNoteIndex := 0 to 127 do
      begin
        case lMidiNoteModula of
         0: lMidiNoteKey := keyWhite;
         1: lMidiNoteKey := keyBlack;
         2: lMidiNoteKey := keyWhite;
         3: lMidiNoteKey := keyBlack;
         4: lMidiNoteKey := keyWhite;
         5: lMidiNoteKey := keyWhite;
         6: lMidiNoteKey := keyBlack;
         7: lMidiNoteKey := keyWhite;
         8: lMidiNoteKey := keyBlack;
         9: lMidiNoteKey := keyWhite;
        10: lMidiNoteKey := keyBlack;
        11: lMidiNoteKey := keyWhite;
        end;

        if lMidiNoteKey = keyBlack then
        begin
          FBitmap.Canvas.Pen.Color := clBlack;
          FBitmap.Canvas.Brush.Color := clBlack;
          FBitmap.Canvas.Rectangle(
            0,
            ConvertNoteToScreen(lNoteIndex) + FNoteOffset,
            PIANO_WIDTH,
            ConvertNoteToScreen(lNoteIndex) + FZoomNoteHeight + FNoteOffset);
        end;

        if lMidiNoteModula in [4, 11] then
        begin
        FBitmap.Canvas.Pen.Color := clBlack;
        FBitmap.Canvas.Brush.Color := clBlack;
        FBitmap.Canvas.Line(
          0,
          ConvertNoteToScreen(lNoteIndex) + FNoteOffset,
          PIANO_WIDTH,
          ConvertNoteToScreen(lNoteIndex) + FNoteOffset);
        end;
        Inc(lMidiNoteModula);
        if lMidiNoteModula = KEYS_PER_OCTAVE then
        begin
          lMidiNoteModula := 0;
        end;
      end;
      FBitmap.Canvas.Line(PIANO_WIDTH, 0, 30, FBitmap.Height);
    end;

    // Draw loopstartmarker
    FBitmap.Canvas.Pen.Width := 2;
    x := Round((LoopStart.Location * FZoomFactorToScreenX) + FOffset + FNoteInfoWidth);
    if x >= FNoteInfoWidth then
    begin
      FBitmap.Canvas.Pen.Color := clRed;
      FBitmap.Canvas.Line(x, 0, x, Height);
      //FBitmap.Canvas.TextOut(x + 10, 10, Format('Start %d', [LoopStart.Location]))
    end;

    // Draw loopendmarker
    x := Round((LoopEnd.Location * FZoomFactorToScreenX) + FOffset + FNoteInfoWidth);
    if x >= FNoteInfoWidth then
    begin
      FBitmap.Canvas.Pen.Color := clRed;
      FBitmap.Canvas.Line(x, 0, x, Height);
      //FBitmap.Canvas.TextOut(x + 10, 10, Format('End %d', [LoopEnd.Location]))
    end;

    // Draw automation data
    if FEditMode = emAutomationEdit then
    begin
      FBitmap.Canvas.Pen.Width := 1;

      if Assigned(FSelectedAutomationParameter) then
      begin
        if FSelectedAutomationParameter.List.Count > 0 then
        begin
          FSelectedAutomationParameter.First;
          lAutomationData := FSelectedAutomationParameter.CurrentAutomationData;

          lAutomationScreenY := Round(Height - lAutomationData.DataValue * Height);
          FBitmap.Canvas.MoveTo(Succ(PIANO_WIDTH), lAutomationScreenY);

          while not FSelectedAutomationParameter.Eof do
          begin
            lAutomationData := FSelectedAutomationParameter.CurrentAutomationData;

            lAutomationScreenY := Round(Height - lAutomationData.DataValue * Height);
            lAutomationScreenX := ConvertTimeToScreen(lAutomationData.Location) + FOffset;
            FBitmap.Canvas.Brush.Color := clRed;
            FBitmap.Canvas.LineTo(
              lAutomationScreenX,
              lAutomationScreenY);

            if (Abs(FMouseX - lAutomationScreenX) < 5) and
              (Abs(FMouseY - lAutomationScreenY) < 5) then
            begin // Mouse over dataselectionpoint
              FBitmap.Canvas.Brush.Color := clLime;
              FBitmap.Canvas.Rectangle(
                lAutomationScreenX - 5,
                lAutomationScreenY - 5,
                lAutomationScreenX + 5,
                lAutomationScreenY + 5);
            end
            else
            begin
              FBitmap.Canvas.Brush.Color := clRed;
              FBitmap.Canvas.Rectangle(
                lAutomationScreenX - 4,
                lAutomationScreenY - 4,
                lAutomationScreenX + 4,
                lAutomationScreenY + 4);
            end;

            FSelectedAutomationParameter.Next;
          end;
          FBitmap.Canvas.LineTo(Width, lAutomationScreenY);
        end
        else
        begin
          // No automation so just draw a straight line
          lAutomationScreenY := Height div 2;
          FBitmap.Canvas.MoveTo(Succ(PIANO_WIDTH), lAutomationScreenY);
          FBitmap.Canvas.LineTo(Width, lAutomationScreenY);
        end;
      end;
    end;
  end;
  Canvas.Draw(0, 0, FBitmap);

  // Draw cursor
  x := Round((FModel.RealCursorPosition * FZoomFactorToScreenX) + FOffset + FNoteInfoWidth);
  if FOldCursorPosition <> x then
  begin
    if x >= FNoteInfoWidth then
    begin
      Canvas.Pen.Color := clBlack;
      Canvas.Line(x, 0, x, Height);
    end;

    FOldCursorPosition := x;
  end;

  inherited Paint;
end;

function TMidiPatternGUI.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TMidiPatternGUI.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

function TMidiPatternGUI.GetObjectOwnerID: string;
begin
  Result := FObjectOwnerID;
end;

procedure TMidiPatternGUI.SetObjectOwnerID(const AObjectOwnerID: string);
begin
  FObjectOwnerID := AObjectOwnerID;
end;

procedure TMidiPatternGUI.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TMidiGridGUI.Update');

  FUpdateSubject := Subject;
  FIsDirty := True;

  DBLog('end TMidiGridGUI.Update');
end;

procedure TMidiPatternGUI.UpdateView(AForceRedraw: Boolean = False);
begin
  FForceRedraw := AForceRedraw;

  if FIsDirty and Assigned(FUpdateSubject) then
  begin
    DiffLists(
      TMidiPattern(FUpdateSubject).NoteList,
      FNoteListGUI,
      @Self.CreateNoteGUI,
      @Self.DeleteNoteGUI);

    FLoopStart.Update(TMidiPattern(FUpdateSubject).LoopStart);
    FLoopEnd.Update(TMidiPattern(FUpdateSubject).LoopEnd);
    FLoopLength.Update(TMidiPattern(FUpdateSubject).LoopLength);

    QuantizeSetting := TMidiPattern(FUpdateSubject).QuantizeSetting;
  end;

  Invalidate;
end;

procedure TMidiPatternGUI.Connect;
begin
  FModel := TMidiPattern(GObjectMapper.GetModelObject(Self.ObjectID));

  FModel.LoopStart.Attach(FLoopStart);
  FModel.LoopEnd.Attach(FLoopEnd);
  FModel.LoopLength.Attach(FLoopLength);
end;

procedure TMidiPatternGUI.Disconnect;
var
  lMidiNoteGUI: TMidiNoteGUI;
  lMidiNote: TMidiNote;
  lIndex: Integer;
begin
  for lIndex := Pred(FNoteListGUI.Count) downto 0 do
  begin
    lMidiNoteGUI := TMidiNoteGUI(FNoteListGUI[lIndex]);

    if Assigned(lMidiNoteGUI) then
    begin
      lMidiNote := TMidiNote(GObjectMapper.GetModelObject(lMidiNoteGUI.ObjectID));
      if Assigned(lMidiNote) then
      begin
        lMidiNote.Detach(lMidiNoteGUI);
        FNoteListGUI.Remove(lMidiNoteGUI);
      end;
    end;
  end;

  FModel.LoopStart.Detach(FLoopStart);
  FModel.LoopEnd.Detach(FLoopEnd);
  FModel.LoopLength.Detach(FLoopLength);
end;

procedure TMidiPatternGUI.CreateNoteGUI(AObjectID: string);
var
  lMidiNote: TMidiNote;
  lMidiNoteGUI: TMidiNoteGUI;
begin
  DBLog('start TMidiGridGUI.CreateNoteGUI');

  lMidiNote := TMidiNote(GObjectMapper.GetModelObject(AObjectID));
  if Assigned(lMidiNote) then
  begin
    lMidiNoteGUI := TMidiNoteGUI.Create(Self.ObjectID);
    lMidiNoteGUI.Note := lMidiNote.Note;
    lMidiNoteGUI.NoteLocation := lMidiNote.NoteLocation;
    lMidiNoteGUI.NoteLength := lMidiNote.NoteLength;
    lMidiNoteGUI.MidiGridGUI := Self;

    FNoteListGUI.Add(lMidiNoteGUI);
    lMidiNote.Attach(lMidiNoteGUI);
  end;

  DBLog('end TMidiGridGUI.CreateNoteGUI');
end;

procedure TMidiPatternGUI.ReleaseNote(Data: PtrInt);
var
  lMidiNoteGUI: TMidiNoteGUI;
begin
  lMidiNoteGUI := TMidiNoteGUI(Data);
  NoteListGUI.Remove(lMidiNoteGUI);

  Invalidate;
end;

procedure TMidiPatternGUI.SetOffset(AValue: Integer);
begin
  FOffset := Round(FZoomFactorToScreenX * LoopEnd.Location / 100 * AValue);

  FIsDirty := True;
end;

procedure TMidiPatternGUI.SetQuantizeSetting(AValue: Integer);
begin
  if FQuantizeSetting = AValue then Exit;
  FQuantizeSetting := AValue;
  case FQuantizeSetting of
  0: FQuantizeValue := -1;
  1: FQuantizeValue := GSettings.HalfSampleRate * 4;
  2: FQuantizeValue := GSettings.HalfSampleRate * 2;
  3: FQuantizeValue := GSettings.HalfSampleRate;
  4: FQuantizeValue := GSettings.HalfSampleRate / 2;
  5: FQuantizeValue := GSettings.HalfSampleRate / 3;
  6: FQuantizeValue := GSettings.HalfSampleRate / 4;
  7: FQuantizeValue := GSettings.HalfSampleRate / 6;
  8: FQuantizeValue := GSettings.HalfSampleRate / 8;
  9: FQuantizeValue := GSettings.HalfSampleRate / 16;
  10: FQuantizeValue := GSettings.HalfSampleRate / 32;
  end;
end;

procedure TMidiPatternGUI.SetSelectedAutomationDeviceId(AValue: string);
begin
  if FSelectedAutomationDeviceId = AValue then Exit;
  FSelectedAutomationDeviceId := AValue;

  FModel.SelectedAutomationDeviceId := AValue;
end;

procedure TMidiPatternGUI.SetSelectedAutomationParameterId(AValue: string);
begin
  if FSelectedAutomationParameterId = AValue then Exit;
  FSelectedAutomationParameterId := AValue;

  FModel.SelectedAutomationParameterId := AValue;

  FSelectedAutomationParameter := FModel.FindAutomationParameter(
    FSelectedAutomationDeviceId,
    FSelectedAutomationParameterId);
end;

procedure TMidiPatternGUI.DeleteNoteGUI(AObjectID: string);
var
  lMidiNoteGUI: TMidiNoteGUI;
  lIndex: Integer;
begin
  DBLog('start TMidiGridGUI.DeleteNoteGUI');

  for lIndex := Pred(FNoteListGUI.Count) downto 0 do
  begin
    lMidiNoteGUI := TMidiNoteGUI(FNoteListGUI[lIndex]);

    if Assigned(lMidiNoteGUI) then
    begin
      if lMidiNoteGUI.ObjectID = AObjectID then
      begin
        Application.QueueAsyncCall(@ReleaseNote, PtrInt(TMidiNoteGUI(lMidiNoteGUI)));
        break;
      end;
    end;
  end;

  DBLog('end TMidiGridGUI.DeleteNoteGUI');
end;

function TMidiPatternGUI.NoteByObjectID(AObjectID: string): TMidiNoteGUI;
var
  lIndex: Integer;
begin
  Result := nil;

  for lIndex := 0 to Pred(NoteListGUI.Count) do
  begin
    if TMidiNoteGUI(NoteListGUI[lIndex]).ObjectID = AObjectID then
    begin
      Result := TMidiNoteGUI(NoteListGUI[lIndex]);
    end;
  end;
end;

procedure TMidiPatternGUI.NoteListByRect(AObjectIDList: TStringList; ARect: TRect);
var
  lIndex: Integer;
  lMidiNoteGUI: TMidiNoteGUI;
  lLeft: Integer;
  lRight: Integer;
  lTop: Integer;
  lBottom: Integer;
begin
  if FRubberBandSelect.Left > FRubberBandSelect.Right then
  begin
    lLeft := FRubberBandSelect.Right;
    lRight := FRubberBandSelect.Left;
  end
  else
  begin
    lLeft := FRubberBandSelect.Left;
    lRight := FRubberBandSelect.Right;
  end;
  if FRubberBandSelect.Top > FRubberBandSelect.Bottom then
  begin
    lTop := FRubberBandSelect.Bottom;
    lBottom := FRubberBandSelect.Top;
  end
  else
  begin
    lTop := FRubberBandSelect.Top;
    lBottom := FRubberBandSelect.Bottom;
  end;

  for lIndex := 0 to Pred(NoteListGUI.Count) do
  begin
    lMidiNoteGUI := TMidiNoteGUI(NoteListGUI[lIndex]);

    if Assigned(lMidiNoteGUI) then
    begin
      if (lLeft < (ConvertTimeToScreen(lMidiNoteGUI.NoteLocation) + FOffset)) and
         (lRight > (ConvertTimeToScreen(lMidiNoteGUI.NoteLocation + lMidiNoteGUI.NoteLength) + FOffset)) and
         (lTop < ConvertNoteToScreen(lMidiNoteGUI.Note) + FNoteOffset) and
         (lBottom > ConvertNoteToScreen(lMidiNoteGUI.Note) + FZoomNoteHeight + FNoteOffset) then
      begin
        AObjectIDList.Add(lMidiNoteGUI.ObjectID);
      end;
    end;
  end;
end;

function TMidiPatternGUI.GetModel: THybridPersistentModel;
begin
  Result := THybridPersistentModel(FModel);
end;

procedure TMidiPatternGUI.SetModel(AModel: THybridPersistentModel);
begin
  FModel := TMidiPattern(AModel);
end;

{
  Convert a screen cursor position to a location in time
}
function TMidiPatternGUI.ConvertScreenToTime(AX: Integer): Integer;
begin
  Result := Round((AX - FNoteInfoWidth) * FZoomFactorToDataX);
end;

{
  Convert location in time to a screen cursor position
}
function TMidiPatternGUI.ConvertTimeToScreen(ATime: Integer): Integer;
begin
  Result := Round(ATime * FZoomFactorToScreenX) + FNoteInfoWidth;
end;

{
  Convert a note to a screen note position
}
function TMidiPatternGUI.ConvertNoteToScreen(ANote: Integer): Integer;
var
  lScale: single;
begin
  lScale := (32 / Height) * FZoomFactorToDataY;

  Result := Round(Height - Round(ANote / lScale));
end;

{
  Convert a screen note position to a note
}
function TMidiPatternGUI.ConvertScreenToNote(AY: Integer): Integer;
var
  lScale: single;
begin
  lScale := (32 / Height) * FZoomFactorToDataY;

  Result := Round((Height - AY) * lScale);
end;

function TMidiPatternGUI.NoteUnderCursor(AX, AY: Integer; var ANoteAction: TNoteAction): TMidiNoteGUI;
var
  lIndex: Integer;
  lNote: TMidiNoteGUI;
  lLeft: Integer;
  lTop: Integer;
  lRight: Integer;
  lBottom: Integer;
begin
  Result := nil;
  FNoteAction := naNone;

  for lIndex := 0 to Pred(FNoteListGUI.Count) do
  begin
    lNote := TMidiNoteGUI(FNoteListGUI[lIndex]);

    lLeft := ConvertTimeToScreen(lNote.NoteLocation) + FOffset;
    lTop :=  ConvertNoteToScreen(lNote.Note) + FNoteOffset;
    lRight := ConvertTimeToScreen(lNote.NoteLocation + lNote.NoteLength) + FOffset;
    lBottom := ConvertNoteToScreen(lNote.Note) + FZoomNoteHeight + FNoteOffset;

    if (lLeft <= AX) and (lTop <= AY) and (lRight >= AX) and (lBottom >= AY) then
    begin
      Result := lNote;
      { Note start adjust not supported yet
      if AX - lLeft < STRETCHNOTE_HOTSPOT then
      begin
        ANoteAction := naAdjustLeft;
      end
      else }if (lRight - AX < STRETCHNOTE_HOTSPOT) and (AX < lRight) then
      begin
        ANoteAction := naAdjustRight;
      end
      else
      begin
        ANoteAction := naDrag;
      end;

      break;
    end;
  end;
end;

procedure TMidiPatternGUI.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  FMouseX := X;
  FMouseY := Y;

  if FEditMode = emPatternEdit then
  begin
    // Moving loopmarker has precedence over notes
    FDraggedLoopMarker := LoopMarkerAt(X, 5);

    if Assigned(FDraggedLoopMarker) then
    begin
      HandleLoopMarkerMouseDown(Button, Shift, X, Y);
    end
    else
    begin
      FDraggedNote := NoteUnderCursor(X, Y, FNoteAction);

      if Assigned(FDraggedNote) then
      begin
        HandleNoteMouseDown(Button, Shift, X, Y);
      end
      else
      begin
        case Button of
          mbLeft:
          begin
            FRubberBandMode := True;
            FRubberBandSelect.TopLeft.X := X;
            FRubberBandSelect.TopLeft.Y := Y;
            FRubberBandSelect.BottomRight.X := X;
            FRubberBandSelect.BottomRight.Y := Y;
          end;

          mbRight:
          begin
            FZoomingMode := True;
            FOldZoomFactorX := FZoomFactorX;
            FOldZoomFactorY := FZoomFactorY;
            FOldLocationOffset := FOffset;
            FOldNoteOffset := FNoteOffset;
            FOldX := X;
            FOldY := Y;
          end;
        end;
      end;
    end;
  end
  else if FEditMode = emAutomationEdit then
  begin
    HandleAutomationEditMouseDown(Button, Shift, X, Y);
  end
  else if FEditMode = emControllerEdit then
  begin
    HandleControllerEditMouseDown(Button, Shift, X, Y);
  end;
end;

procedure TMidiPatternGUI.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if FEditMode = emPatternEdit then
  begin
    if Assigned(FDraggedLoopMarker) then
    begin
      HandleLoopMarkerMouseUp(Button, Shift, X, Y);
    end
    else if Assigned(FDraggedNote) then
    begin
      HandleNoteMouseUp(Button, Shift, X, Y);
    end
    else
    begin
      if FZoomingMode then
        FZoomingMode:= False;

      if FRubberBandMode then
        FRubberBandMode := False;

      Invalidate;
    end;
  end
  else if FEditMode = emAutomationEdit then
  begin
    HandleAutomationEditMouseUp(Button, Shift, X, Y);
  end
  else if FEditMode = emControllerEdit then
  begin
    HandleControllerEditMouseUp(Button, Shift, X, Y);
  end;

  UpdateView(True);
end;

function TMidiPatternGUI.QuantizeLocation(ALocation: Integer): Integer;
begin
  if FModel.QuantizeSetting = 0 then
  begin
    Result := Round(Trunc(ALocation / GSettings.HalfSampleRate) * GSettings.HalfSampleRate);
  end
  else
  begin
    Result := Round(Trunc(ALocation / FModel.QuantizeValue) * FModel.QuantizeValue);
  end;
end;

function TMidiPatternGUI.LoopMarkerAt(X: Integer; AMargin: Single): TLoopMarkerGUI;
begin
  LoopStart.Location := FModel.LoopStart.Value;
  LoopEnd.Location := FModel.LoopEnd.Value;
  LoopLength.Location := FModel.LoopLength.Value;

  Result := nil;
  if Abs(X - ConvertTimeToScreen(LoopStart.Location) - FOffset) < AMargin then
  begin
    Result := LoopStart;
  end
  else
  if Abs(X - ConvertTimeToScreen(LoopEnd.Location) - FOffset) < AMargin then
  begin
    Result := LoopEnd;
  end
  else
  if Abs(X - ConvertTimeToScreen(LoopLength.Location) - FOffset) < AMargin then
  begin
    Result := LoopLength;
  end;
end;

procedure TMidiPatternGUI.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  lNoteAction: TNoteAction;
begin
  inherited MouseMove(Shift, X, Y);

  FMouseX := X;
  FMouseY := Y;

  if FDragging then
  begin
    lNoteAction := FNoteAction;
  end
  else
  begin
    NoteUnderCursor(X, Y, lNoteAction);
  end;
  case lNoteAction of
    naAdjustRight: Screen.Cursor := crSizeWE;
    naAdjustLeft: Screen.Cursor := crSizeWE;
    naDrag: Screen.Cursor := crSize;
  else
    Screen.Cursor := crArrow;
  end;

  if FEditMode = emPatternEdit then
  begin
    if Assigned(FDraggedLoopMarker) then
    begin
      HandleLoopMarkerMouseMove(Shift, X, Y);
    end
    else if Assigned(FDraggedNote) then
    begin
      HandleNoteMouseMove(Shift, X, Y);
    end
    else
    begin
  {   hmmm....why is this code f**king up pattern switching/ lost notes, etc...??


      if FRubberBandMode then
      begin
        if (FRubberBandSelect.BottomRight.X <> X) or
          (FRubberBandSelect.BottomRight.Y <> Y) then
        begin
          FRubberBandSelect.BottomRight.X := X;
          FRubberBandSelect.BottomRight.Y := Y;

          lSelectObjectListCommand := TSelectObjectListCommand.Create(Self.ObjectID);
          try
            lSelectObjectListCommand.AddMode := (ssShift in GSettings.Modifier);
            lSelectObjectListCommand.Persist := False;
            NoteListByRect(lSelectObjectListCommand.ObjectIdList, FRubberBandSelect);

            GCommandQueue.PushCommand(lSelectObjectListCommand);
          except
            lSelectObjectListCommand.Free;
          end;
        end;
      end;
      }
      if FZoomingMode then
      begin
        FOffset:= FOldLocationOffset + (X - FOldX);

        if FOffset > 0 then
        begin
          FOffset := 0;
        end;

        FNoteOffset := FOldNoteOffset + (Y - FOldY);
      end;

      FNoteHighlightLocation := QuantizeLocation(ConvertScreenToTime(X - FOffset));
      FNoteHighlightNote := ConvertScreenToNote(Y - FNoteOffset);
    end;
  end
  else if FEditMode = emAutomationEdit then
  begin
    HandleAutomationEditMouseMove(Shift, X, Y);
  end
  else if FEditMode = emControllerEdit then
  begin
    HandleControllerEditMouseMove(Shift, X, Y);
  end;

  UpdateView(True);
end;

procedure TMidiPatternGUI.DblClick;
var
  lCreateNoteCommand: TCreateNotesCommand;
  lDeleteNoteCommand: TDeleteNotesCommand;
  lControllerCreateCommand: TControllerCreateCommand;
begin
  if Assigned(FDraggedNote) then
  begin
    lDeleteNoteCommand := TDeleteNotesCommand.Create(Self.ObjectID);
    try
      lDeleteNoteCommand.ObjectID := FDraggedNote.ObjectID;

      GCommandQueue.PushCommand(lDeleteNoteCommand);
    except
      lDeleteNoteCommand.Free;
    end;
  end
  else
  begin
    if FEditMode = emPatternEdit then
    begin
      lCreateNoteCommand := TCreateNotesCommand.Create(Self.ObjectID);
      try
        if FQuantizeValue = 0 then
          lCreateNoteCommand.NoteLength := Round(GSettings.HalfSampleRate)
        else
          lCreateNoteCommand.NoteLength := Round(FQuantizeValue);

        lCreateNoteCommand.Note := ConvertScreenToNote(FMouseY - FNoteOffset);
        lCreateNoteCommand.Location := QuantizeLocation(ConvertScreenToTime(FMouseX - FOffset));

        GCommandQueue.PushCommand(lCreateNoteCommand);
      except
        lCreateNoteCommand.Free;
      end;
    end
    else if FEditMode = emControllerEdit then
    begin
      begin
        if not Assigned(FSelectedControllerEvent) then
        begin
          // TODO check if there already is an automation event on this location
          lControllerCreateCommand := TControllerCreateCommand.Create(Self.ObjectID);
          try
            lControllerCreateCommand.ControllerId := FSelectedController;
            lControllerCreateCommand.Location := Round(ConvertScreenToTime(FMouseX - FOffset));
            lControllerCreateCommand.DataValue := (Height - FMouseY) div Height;
            lControllerCreateCommand.Persist := True;

            GCommandQueue.PushCommand(lControllerCreateCommand);
          except
            lControllerCreateCommand.Free;
          end;
        end;
      end;

    end;
  end;

  UpdateView(True);

  inherited DblClick;
end;

procedure TMidiPatternGUI.MouseLeave;
begin
  Screen.Cursor := crArrow;

  inherited MouseLeave;
end;

procedure TMidiPatternGUI.DoOnResize;
begin
  UpdateView(True);

  inherited DoOnResize;
end;

function TMidiPatternGUI.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  ZoomFactorY := ZoomFactorY + 100;

  Invalidate;

  Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

function TMidiPatternGUI.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  ZoomFactorY := ZoomFactorY - 100;

  Invalidate;

  Result := inherited DoMouseWheelUp(Shift, MousePos);
end;

{$R *.lfm}

initialization
  RegisterClass(TMidiPatternGUI);
  RegisterClass(TMidiNoteGUI);
end.

