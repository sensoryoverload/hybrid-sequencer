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
  lclintf, ActnList;

const
  DIVBY220 = 1 / 220.50;
  DIVBY1000 = 1 / 1000;

type
  TMidiGridOptions = set of (PianoKeyboard, DrumMap, MidiChannel, MidiNote);
  TKey = (keyBlack, keyWhite);

  TZoomCallback = procedure(AZoomTimeLeft, AZoomTimeRight: Integer) of object;

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
    constructor Create(AObjectOwner: string);
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce; override;
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
    FObjectID: string;
    FObjectOwnerID: string;
    FModel: TMidiPattern;

    { GUI }
    FLocationOffset: Integer;
    FOldLocationOffset: Integer;

    FNoteOffset: Integer;
    FOldNoteOffset: Integer;
    FNoteInfoWidth: Integer;

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

    FZoomingMode: Boolean;
    FBitmap: TBitmap;
    FBitmapIsDirty: Boolean;

    FRubberBandSelect: TRect;
    FRubberBandMode: Boolean;
    FOptionsView: TMidiGridOptions;

    FDragging: Boolean;
    FDraggedNote: TMidiNoteGUI;
    FDragMode: Short;

    FDraggingLoopMarker: Boolean;
    FDraggedLoopMarker: TLoopMarkerGUI;
    FMouseButton: TMouseButton;

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


    procedure HandleLoopMarkerMouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure HandleLoopMarkerMouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure HandleNoteMouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure HandleNoteMouseMove(Shift: TShiftState; X, Y: Integer);
    procedure HandleLoopMarkerMouseMove(Shift: TShiftState; X, Y: Integer);
    procedure HandleNoteMouseUp(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function LoopMarkerAt(X: Integer; AMargin: Single): TLoopMarkerGUI;
    function QuantizeLocation(ALocation: Integer): Integer;
    function NoteUnderCursor(AX, AY: Integer): TMidiNoteGUI;
    function ConvertTimeToScreen(ATime: Integer): Integer;
    function ConvertScreenToTime(AX: Integer): Integer;
    function ConvertNoteToScreen(ANote: Integer): Integer;
    function ConvertScreenToNote(AY: Integer): Integer;
    procedure ReleaseNote(Data: PtrInt);
    procedure SetZoomFactorX(const AValue: Single);
    procedure SetZoomFactorY(const AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce;
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
    property LocationOffset: Integer read FLocationOffset write FLocationOffset;
    property NoteOffset: Integer read FNoteOffset write FNoteOffset;
    property NoteListGUI: TObjectList read FNoteListGUI write FNoteListGUI;
    property ZoomFactorX: Single read FZoomFactorX write SetZoomFactorX;
    property ZoomFactorY: Single read FZoomFactorY write SetZoomFactorY;
    property RealCursorPosition: Integer read FRealCursorPosition write FRealCursorPosition;
    property CursorAdder: Single read FCursorAdder write FCursorAdder;
    property LoopStart: TLoopMarkerGUI read FLoopStart write FLoopStart;
    property LoopEnd: TLoopMarkerGUI read FLoopEnd write FLoopEnd;
    property LoopLength: TLoopMarkerGUI read FLoopLength write FLoopLength;
    property QuantizeSetting: Integer read FQuantizeSetting write FQuantizeSetting default 1;
    property QuantizeValue: Single read FQuantizeValue write FQuantizeValue default 22050;
    property RootNote: Integer read FRootNote write FRootNote default 0;
    property MidiChannel: Integer read FMidiChannel write FMidiChannel;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure CreateNoteGUI(AObjectID: string);
    procedure DeleteNoteGUI(AObjectID: string);
    function NoteByObjectID(AObjectID: string): TMidiNoteGUI;
    procedure NoteListByRect(AObjectIDList: TStringList; ARect: TRect);
    function GetModel: THybridPersistentModel;
    procedure SetModel(AModel: THybridPersistentModel);
  end;

  TMidigridOverview = class;

  { TMidiNoteOverview }

  TMidiNoteOverview = class(THybridPersistentView)
  private
    FMidiGridOverview: TMidigridOverview;

    { Audio }
    FNoteLocation: Integer; // Which time format ? in samples ??
    FNote: Integer; // 0..127
    FNoteVelocity: Integer; // 0..127
    FNoteLength: Integer;
    FSelected: Boolean;
  public
    procedure Update(Subject: THybridPersistentModel); reintroduce; override;
    property Note: Integer read FNote write FNote;
    property NoteLocation: Integer read FNoteLocation write FNoteLocation;
    property NoteVelocity: Integer read FNoteVelocity write FNoteVelocity;
    property NoteLength: Integer read FNoteLength write FNoteLength;
    property MidiGridOverview: TMidigridOverview read FMidiGridOverview write FMidiGridOverview;
  published
    property Selected: Boolean read FSelected write FSelected;
  end;

  { TMidigridOverview }

  TMidigridOverview = class(TPersistentCustomControl)
  private
    FNoteListGUI: TObjectList;
    FTotalWidth: Integer;
    FZoomBoxWidth: Integer;
    FZoomTimeLeft: Integer;
    FZoomTimeRight: Integer;
    FOldX: Integer;
    FOldY: Integer;
    FMouseX: Integer;

    FZooming: Boolean;
    FZoomingLeft: Boolean;
    FZoomingRight: Boolean;

    FZoomCallback: TZoomCallback;
    FModel: TMidiPattern;

    function CalculateTotalWidth: Integer;
    function ConvertNoteToScreen(ANote: Integer): Integer;
    function ConvertScreenToTime(AX: Integer): Integer;
    function ConvertTimeToScreen(ATime: Integer): Integer;
    procedure CreateOverviewNoteGUI(AObjectID: string);
    procedure DeleteOverviewNoteGUI(AObjectID: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce; override;
    procedure EraseBackground(DC: HDC); override;
    function GetModel: THybridPersistentModel; override;
    procedure SetModel(AModel: THybridPersistentModel); override;
    procedure Connect; override;
    procedure Disconnect; override;
    property ZoomCallback: TZoomCallback write FZoomCallback;
    property Model: THybridPersistentModel read GetModel write SetModel;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  end;


implementation

uses
  utils, appcolors, pattern;

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
begin
  // TODO implement
  {
    Drop samples to midigrid instantely creates a sample in the current samplebank
    for this midigrid.
  }
  writeln('TODO Drop samples to midigrid instantely creates a sample in the current samplebank for this midigrid ');
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

  FBitmapIsDirty := True;
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
    lUpdateLoopMarkerCommand.Location := Round(ConvertScreenToTime(X) * 220.50);

    GCommandQueue.PushCommand(lUpdateLoopMarkerCommand);
  except
    lUpdateLoopMarkerCommand.Free;
  end;
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

  if Width - X < 10 then
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
    lUpdateLoopMarkerCommand.Location := Round(ConvertScreenToTime(X) * 220.50);

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
  lNoteLength: Integer;
  lNote: Integer;
begin
  FMouseNoteX := X;
  FMouseNoteY := Y;

  if (FMouseButton = mbLeft) and FDragging then
  begin
    lNoteLocation := QuantizeLocation(ConvertScreenToTime(X - FLocationOffset));
    lNoteLength := QuantizeLocation(ConvertScreenToTime(X - FOrgNoteX));
    lNote := ConvertScreenToNote(Y - FNoteOffset);

    if (FOldQuantizedNote <> lNote) or (FOldQuantizedLocation <> lNoteLocation) then
    begin
      case FDragMode of
        ndLength: // Change length of note
        begin
          FDraggedNote.NoteLength := FDraggedNote.OriginalNoteLength + lNoteLength;

          lStretchNotesCommand := TStretchNotesCommand.Create(Self.ObjectID);
          try
            lStretchNotesCommand.Persist := False;
            lStretchNotesCommand.NoteLengthDiff := lNoteLength - FDraggedNote.OriginalNoteLength;

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
    end;
  end;
end;

procedure TMidiPatternGUI.HandleLoopMarkerMouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  lValue: Integer;
begin
  lValue := Round(ConvertScreenToTime(X) * 220.50);

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
begin
  FZoomFactorX := AValue;
  if FZoomFactorX <= 0 then FZoomFactorX := 1;
  FZoomFactorToScreenX := (ZoomFactorX / 1000);
  FZoomFactorToDataX := (1000 / ZoomFactorX);
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
  FLocationOffset := 0 - ConvertTimeToScreen(AZoomTimeLeft);

  FBitmapIsDirty := True;
  Invalidate;
end;


constructor TMidiPatternGUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLoopStart := TLoopMarkerGUI.Create(ObjectID, ltStart);
  FLoopEnd := TLoopMarkerGUI.Create(ObjectID, ltEnd);
  FLoopLength := TLoopMarkerGUI.Create(ObjectID, ltLength);

  FOptionsView := [PianoKeyboard];

// TODO Toggle button -----------
  if PianoKeyboard in FOptionsView then
  begin
    FNoteInfoWidth := 30;
  end
  else
  begin
    FNoteInfoWidth := 0;
  end;
// ------------------------------

  FBitmap := TBitmap.Create;
  DoubleBuffered := True;

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
  lTime: Integer;
  lTimeSpacing: Integer;
  lHighlightLocation: Integer;
  lHighlightNote: Integer;
  lHighlightWidth: Integer;
  lIndex: Integer;
  lNote: TMidiNoteGUI;
  lNoteIndex: Integer;
  lMidiNoteModula: Integer;
  lMidiNoteKey: TKey;

begin
  if not Assigned(FModel) then exit;

  if FBitmapIsDirty then
  begin
    FBitmap.Canvas.Clear;
    FBitmap.Height := Height;
    FBitmap.Width := Width;

    // Draw default background
    FBitmap.Canvas.Brush.Color := clMidigridBackgroundWhite;
    FBitmap.Canvas.Pen.Color := clMidigridBackgroundWhite;
    FBitmap.Canvas.Rectangle(0, 0, FBitmap.Width, FBitmap.Height);

    // Draw note indictor lines
    FBitmap.Canvas.Pen.Color := clMidigridBackgroundBlack;
    FBitmap.Canvas.Brush.Color := clMidigridBackgroundBlack;
    for lNoteIndex := 0 to 127 do
    begin
      lMidiNoteModula := lNoteIndex mod 12;
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
          30,
          ConvertNoteToScreen(lNoteIndex) + FNoteOffset,
          FBitmap.Width,
          ConvertNoteToScreen(lNoteIndex) + FZoomNoteHeight + FNoteOffset);
      end;
    end;

    // Draw vertica time indicators
    FBitmap.Canvas.Pen.Color := clGlobalOutline;

    lTimeSpacing := Round(QuantizeValue);
    if lTimeSpacing < 1 then
      lTimeSpacing := 100;

    if FZoomFactorX < 500 then
      lTimeSpacing := lTimeSpacing * 2;

    if FZoomFactorX < 250 then
      lTimeSpacing := lTimeSpacing * 2;

    if FZoomFactorX < 125 then
      lTimeSpacing := lTimeSpacing * 2;

    if FZoomFactorX < 62.5 then
      lTimeSpacing := lTimeSpacing * 2;

    if FZoomFactorX < 31.25 then
      lTimeSpacing := lTimeSpacing * 2;

    if FZoomFactorX < 15.625 then
      lTimeSpacing := lTimeSpacing * 2;

    if FZoomFactorX < 7.8125 then
      lTimeSpacing := lTimeSpacing * 2;

    if FZoomFactorX < 3.90625 then
      lTimeSpacing := lTimeSpacing * 2;

    lTime := lTimeSpacing;
    repeat
      x := ConvertTimeToScreen(lTime) + FLocationOffset;
      FBitmap.Canvas.Pen.Color := clGray;
      FBitmap.Canvas.Line(x, 0, x, FBitmap.Height);
      FBitmap.Canvas.TextOut(x + 2, 1, Format('%d', [lTime div 100 + 1]));
      Inc(lTime, lTimeSpacing);
    until x > FBitmap.Width;

    // Draw note cursor box
    FBitmap.Canvas.Pen.Color := clCream;
    FBitmap.Canvas.Brush.Color := clCream;
    lHighlightLocation := ConvertTimeToScreen(FNoteHighlightLocation) + FLocationOffset;
    lHighlightNote := ConvertNoteToScreen(FNoteHighlightNote) + FNoteOffset;
    lHighlightWidth := Round(FQuantizeValue * FZoomFactorToScreenX);
    FBitmap.Canvas.Rectangle(lHighlightLocation, lHighlightNote,
      lHighlightLocation + lHighlightWidth, lHighlightNote + FZoomNoteHeight);

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
    FBitmap.Canvas.Pen.Color := clBlack;
    FBitmap.Canvas.Brush.Color := clGreen;

    for lIndex := 0 to Pred(NoteListGUI.Count) do
    begin
      lNote := TMidiNoteGUI(NoteListGUI[lIndex]);

      if lNote.Selected then
      begin
        FBitmap.Canvas.Brush.Color := clGreen;
      end
      else
      begin
        FBitmap.Canvas.Brush.Color := clLime;
      end;

      FBitmap.Canvas.Rectangle(
        ConvertTimeToScreen(lNote.NoteLocation) + FLocationOffset,
        ConvertNoteToScreen(lNote.Note) + FNoteOffset,
        ConvertTimeToScreen(lNote.NoteLocation + lNote.NoteLength) + FLocationOffset,
        ConvertNoteToScreen(lNote.Note) + FZoomNoteHeight + FNoteOffset
        );

      if FZoomNoteHeight > 8 then
      begin
        FBitmap.Canvas.TextOut(
          ConvertTimeToScreen(lNote.NoteLocation) + FLocationOffset,
          ConvertNoteToScreen(lNote.Note) + FNoteOffset,
          Format('%d, %d', [lNote.Note, lNote.NoteLocation]));
      end;
    end;

    // Draw keyboard
    if PianoKeyboard in FOptionsView then
    begin
      FBitmap.Canvas.Brush.Color := clMidigridBackgroundWhite;
      FBitmap.Canvas.Pen.Color := clMidigridBackgroundWhite;
      FBitmap.Canvas.Clipping := True;
      FBitmap.Canvas.Rectangle(0, 0, 30, FBitmap.Height);
      for lNoteIndex := 0 to 127 do
      begin
        lMidiNoteModula := lNoteIndex mod 12;
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
            30,
            ConvertNoteToScreen(lNoteIndex) + FZoomNoteHeight + FNoteOffset);
        end;

        if lMidiNoteModula in [4, 11] then
        begin
        FBitmap.Canvas.Pen.Color := clBlack;
        FBitmap.Canvas.Brush.Color := clBlack;
        FBitmap.Canvas.Line(
          0,
          ConvertNoteToScreen(lNoteIndex) + FNoteOffset,
          30,
          ConvertNoteToScreen(lNoteIndex) + FNoteOffset);
        end;
      end;
      FBitmap.Canvas.Line(30, 0, 30, FBitmap.Height);

    end;

    // Draw loopstartmarker
    FBitmap.Canvas.Pen.Width := 2;
    x := Round((LoopStart.Location * FZoomFactorToScreenX) * DIVBY220 + FLocationOffset + FNoteInfoWidth);
    if x >= FNoteInfoWidth then
    begin
      FBitmap.Canvas.Pen.Color := clRed;
      FBitmap.Canvas.Line(x, 0, x, Height);
    end;

    FBitmap.Canvas.TextOut(0, 0, Format('Loopstart : %d calc %d',[FModel.LoopStart.Location, x]));

    // Draw loopendmarker
    x := Round((LoopEnd.Location * FZoomFactorToScreenX) * DIVBY220 + FLocationOffset + FNoteInfoWidth);
    if x >= FNoteInfoWidth then
    begin
      FBitmap.Canvas.Pen.Color := clRed;
      FBitmap.Canvas.Line(x, 0, x, Height);
    end;
    FBitmap.Canvas.Pen.Width := 1;

    FBitmapIsDirty := False;
  end;

  Canvas.Draw(0, 0, FBitmap);

  // Draw cursor
  x := Round((FModel.RealCursorPosition * FZoomFactorToScreenX) / 220.50 + FLocationOffset + FNoteInfoWidth);
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

  DiffLists(
    TMidiPattern(Subject).NoteList,
    FNoteListGUI,
    @Self.CreateNoteGUI,
    @Self.DeleteNoteGUI);

  FLoopStart.Update(TMidiPattern(Subject).LoopStart);
  FLoopEnd.Update(TMidiPattern(Subject).LoopEnd);
  FLoopLength.Update(TMidiPattern(Subject).LoopLength);

  FBitmapIsDirty := True;
  Invalidate;

  DBLog('end TMidiGridGUI.Update');
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

  FBitmapIsDirty := True;
  Invalidate;
end;

procedure TMidiPatternGUI.DeleteNoteGUI(AObjectID: string);
var
  lMidiNoteGUI: TMidiNoteGUI;
  lMidiNote: TMidiNote;
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
      if (lLeft < (ConvertTimeToScreen(lMidiNoteGUI.NoteLocation) + FLocationOffset)) and
         (lRight > (ConvertTimeToScreen(lMidiNoteGUI.NoteLocation + lMidiNoteGUI.NoteLength) + FLocationOffset)) and
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

function TMidiPatternGUI.NoteUnderCursor(AX, AY: Integer): TMidiNoteGUI;
var
  lIndex: Integer;
  lNote: TMidiNoteGUI;
  lLeft: Integer;
  lTop: Integer;
  lRight: Integer;
  lBottom: Integer;
begin
  Result := nil;
  for lIndex := 0 to Pred(FNoteListGUI.Count) do
  begin
    lNote := TMidiNoteGUI(FNoteListGUI[lIndex]);

    lLeft := ConvertTimeToScreen(lNote.NoteLocation) + FLocationOffset;
    lTop :=  ConvertNoteToScreen(lNote.Note) + FNoteOffset;
    lRight := ConvertTimeToScreen(lNote.NoteLocation + lNote.NoteLength) + FLocationOffset;
    lBottom := ConvertNoteToScreen(lNote.Note) + FZoomNoteHeight + FNoteOffset;

    if (lLeft <= AX) and (lTop <= AY) and (lRight >= AX) and (lBottom >= AY) then
    begin
      Result := lNote;
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

  // Moving loopmarker has precedence over notes
  FDraggedLoopMarker := LoopMarkerAt(X, 5);

  if Assigned(FDraggedLoopMarker) then
  begin
    HandleLoopMarkerMouseDown(Button, Shift, X, Y);
  end
  else
  begin
    FDraggedNote := NoteUnderCursor(X, Y);

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
          FOldLocationOffset := FLocationOffset;
          FOldNoteOffset := FNoteOffset;
          FOldX := X;
          FOldY := Y;
        end;
      end;
    end;
  end;
end;

procedure TMidiPatternGUI.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

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
end;

function TMidiPatternGUI.QuantizeLocation(ALocation: Integer): Integer;
begin
  if FQuantizeSetting = 0 then
  begin
    Result := Round(Trunc(ALocation / 100) * 100);
  end
  else
  begin
    Result := Round(Trunc(ALocation / FQuantizeValue) * FQuantizeValue);
  end;
end;

function TMidiPatternGUI.LoopMarkerAt(X: Integer; AMargin: Single): TLoopMarkerGUI;
var
  lLocation: Integer;
begin
  LoopStart.Location := FModel.LoopStart.Location;
  LoopEnd.Location := FModel.LoopEnd.Location;
  LoopLength.Location := FModel.LoopLength.Location;

  Result := nil;

  if Abs(X - ConvertTimeToScreen(LoopStart.Location) * DIVBY220 - (FLocationOffset + FNoteInfoWidth)) < AMargin then
  begin
    Result := LoopStart;
  end
  else
  if Abs(X - ConvertTimeToScreen(LoopEnd.Location) * DIVBY220 - (FLocationOffset + FNoteInfoWidth)) < AMargin then
  begin
    Result := LoopEnd;
  end
  else
  if Abs(X - ConvertTimeToScreen(LoopLength.Location) * DIVBY220 - (FLocationOffset + FNoteInfoWidth)) < AMargin then
  begin
    Result := LoopLength;
  end;
end;

procedure TMidiPatternGUI.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  lSelectObjectListCommand: TSelectObjectListCommand;
begin
  inherited MouseMove(Shift, X, Y);

  FMouseX := X;
  FMouseY := Y;

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
      FLocationOffset:= FOldLocationOffset + (X - FOldX);

      if FLocationOffset > 0 then
      begin
        FLocationOffset := 0;
      end;

      FNoteOffset := FOldNoteOffset + (Y - FOldY);
    end;

    FNoteHighlightLocation := QuantizeLocation(ConvertScreenToTime(X - FLocationOffset));
    FNoteHighlightNote := ConvertScreenToNote(Y - FNoteOffset);
  end;

  // Invalidate here as this one of the few situations that screen updates are
  // requested by the observer and not the subject ie mousemove changes are not always
  // persistent towards the subject.
  FBitmapIsDirty := True;
  //Invalidate;
end;

procedure TMidiPatternGUI.DblClick;
var
  lCreateNoteCommand: TCreateNotesCommand;
  lDeleteNoteCommand: TDeleteNotesCommand;
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
    lCreateNoteCommand := TCreateNotesCommand.Create(Self.ObjectID);
    try
      if FQuantizeValue = 0 then
        lCreateNoteCommand.NoteLength := 100
      else
        lCreateNoteCommand.NoteLength := Round(FQuantizeValue);

      lCreateNoteCommand.Note := ConvertScreenToNote(FMouseY - FNoteOffset);
      lCreateNoteCommand.Location := QuantizeLocation(ConvertScreenToTime(FMouseX - FLocationOffset));

      GCommandQueue.PushCommand(lCreateNoteCommand);
    except
      lCreateNoteCommand.Free;
    end;
  end;

  inherited DblClick;

end;

function TMidiPatternGUI.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  ZoomFactorY := ZoomFactorY + 100;

  FBitmapIsDirty := True;
  Invalidate;

  Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

function TMidiPatternGUI.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  ZoomFactorY := ZoomFactorY - 100;

  FBitmapIsDirty := True;
  Invalidate;

  Result := inherited DoMouseWheelUp(Shift, MousePos);
end;


{$R *.lfm}

{ TMidigridOverview }

constructor TMidigridOverview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FNoteListGUI := TObjectList.Create(True);
end;

destructor TMidigridOverview.Destroy;
begin
  if Assigned(FNoteListGUI) then
    FNoteListGUI.Free;

  inherited Destroy;
end;

procedure TMidigridOverview.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TMidigridOverview.Update');

  DiffLists(
    TMidiPattern(Subject).NoteList,
    FNoteListGUI,
    @Self.CreateOverviewNoteGUI,
    @Self.DeleteOverviewNoteGUI);

  // Determine total width of a notes
  FTotalWidth := CalculateTotalWidth;

  Invalidate;

  DBLog('end TMidigridOverview.Update');
end;

procedure TMidigridOverview.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

function TMidigridOverview.GetModel: THybridPersistentModel;
begin
  Result := THybridPersistentModel(FModel);
end;

procedure TMidigridOverview.SetModel(AModel: THybridPersistentModel);
begin
  FModel := TMidiPattern(AModel);
end;

procedure TMidigridOverview.Connect;
begin
  FModel := TMidiPattern(GObjectMapper.GetModelObject(Self.ObjectID));

  {Create views here from modellist   }
end;

procedure TMidigridOverview.Disconnect;
var
  lIndex: Integer;
  lMidiNoteOverview: TMidiNoteOverview;
  lMidiNote: TMidiNote;
begin
  for lIndex := Pred(FNoteListGUI.Count) downto 0 do
  begin
    lMidiNoteOverview := TMidiNoteOverview(FNoteListGUI[lIndex]);

    if Assigned(lMidiNoteOverview) then
    begin
      lMidiNote := TMidiNote(GObjectMapper.GetModelObject(lMidiNoteOverview.ObjectID));
      if Assigned(lMidiNote) then
      begin
        lMidiNote.Detach(lMidiNoteOverview);
        FNoteListGUI.Remove(lMidiNoteOverview);
      end;
    end;
  end;
end;

procedure TMidigridOverview.Paint;
var
  lIndex: Integer;
  lNote: TMidiNoteOverview;
begin

  // Draw midi notes
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 1;
  for lIndex := 0 to Pred(FNoteListGUI.Count) do
  begin
    lNote := TMidiNoteOverview(FNoteListGUI[lIndex]);

    Canvas.Line(
      ConvertTimeToScreen(lNote.NoteLocation),
      ConvertNoteToScreen(lNote.Note),
      ConvertTimeToScreen(lNote.NoteLocation + lNote.NoteLength),
      ConvertNoteToScreen(lNote.Note)
      );
  end;

  Canvas.Pen.Width := 1;
  Canvas.Rectangle(0, 0, Width, Height);

  Canvas.Pen.Width := 2;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(
    ConvertTimeToScreen(FZoomTimeLeft), 1,
    ConvertTimeToScreen(FZoomTimeRight), Height - 1);

  inherited Paint;
end;

procedure TMidigridOverview.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FOldX := X;
  FOldY := Y;

  if FNoteListGUI.Count > 0 then
  begin
    if (X >= ConvertTimeToScreen(FZoomTimeLeft) - 2) and (X <= ConvertTimeToScreen(FZoomTimeLeft) + 2) then
    begin
      FZoomingLeft := True;
    end
    else if (X >= ConvertTimeToScreen(FZoomTimeRight) - 2) and (X <= ConvertTimeToScreen(FZoomTimeRight) + 2) then
    begin
      FZoomingRight := True;
    end
    else
    begin
      FZooming := True;
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TMidigridOverview.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FZooming := False;
  FZoomingLeft := False;
  FZoomingRight := False;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TMidigridOverview.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  lConstraintZoom: Integer;
begin
  FMouseX := X;

  if FZooming then
  begin
    if Y <= FOldY then
    begin
      lConstraintZoom := 100 - (FOldY - Y);
      if lConstraintZoom > 100 then
        lConstraintZoom := 100;
      if lConstraintZoom < 5 then
        lConstraintZoom := 5;
    end
    else
      lConstraintZoom := 100;

    FZoomBoxWidth := Round((Width / 100) * lConstraintZoom);
    if FZoomBoxWidth < 20 then FZoomBoxWidth := 20;

    FZoomTimeLeft := ConvertScreenToTime(FMouseX - (FZoomBoxWidth div 2));
    if FZoomTimeLeft < 1 then FZoomTimeLeft := 1;

    FZoomTimeRight := ConvertScreenToTime(FMouseX + (FZoomBoxWidth div 2));
    if FZoomTimeRight < 20 then
      FZoomTimeRight := 20;

    if FZoomTimeRight > ConvertScreenToTime(Width) then
      FZoomTimeRight := ConvertScreenToTime(Width);

    if FZoomTimeRight > FZoomTimeLeft then
    begin
      if Assigned(FZoomCallback) then
      begin
        FZoomCallback(FZoomTimeLeft, FZoomTimeRight);
      end;
    end;

    Invalidate;
  end
  else if FZoomingLeft then
  begin
    FZoomTimeLeft := ConvertScreenToTime(X);

    if FZoomTimeLeft >= (FZoomTimeRight - 20) then
      FZoomTimeLeft := FZoomTimeRight - 20;

    if FZoomTimeLeft < 1 then
      FZoomTimeLeft := 1;

    if FZoomTimeRight > ConvertScreenToTime(Width) then
      FZoomTimeRight := ConvertScreenToTime(Width);

    if FZoomTimeRight > FZoomTimeLeft then
    begin
      if Assigned(FZoomCallback) then
      begin
        FZoomCallback(FZoomTimeLeft, FZoomTimeRight);
      end;
    end;

    Invalidate;
  end
  else if FZoomingRight then
  begin
    FZoomTimeRight := ConvertScreenToTime(X);

    if FZoomTimeRight <= (FZoomTimeLeft + 20) then
      FZoomTimeRight := FZoomTimeLeft + 20;

    if FZoomTimeRight > ConvertScreenToTime(Width) then
      FZoomTimeRight := ConvertScreenToTime(Width);

    if FZoomTimeRight > FZoomTimeLeft then
    begin
      if Assigned(FZoomCallback) then
      begin
        FZoomCallback(FZoomTimeLeft, FZoomTimeRight);
      end;
    end;

    Invalidate;
  end;

  inherited MouseMove(Shift, X, Y);
end;

procedure TMidigridOverview.CreateOverviewNoteGUI(AObjectID: string);
var
  lMidiNote: TMidiNote;
  lMidiNoteOverview: TMidiNoteOverview;
begin
  DBLog('start TMidigridOverview.CreateOverviewNoteGUI');

  lMidiNote := TMidiNote(GObjectMapper.GetModelObject(AObjectID));
  if Assigned(lMidiNote) then
  begin
    lMidiNoteOverview := TMidiNoteOverview.Create(Self.ObjectID);
    lMidiNoteOverview.Note := lMidiNote.Note;
    lMidiNoteOverview.NoteLocation := lMidiNote.NoteLocation;
    lMidiNoteOverview.NoteLength := lMidiNote.NoteLength;

    FNoteListGUI.Add(lMidiNoteOverview);
    lMidiNote.Attach(lMidiNoteOverview);
  end;

  DBLog('end TMidigridOverview.CreateOverviewNoteGUI');
end;

procedure TMidigridOverview.DeleteOverviewNoteGUI(AObjectID: string);
var
  lMidiNote: TMidiNote;
  lMidiNoteOverview: TMidiNoteOverview;
  lIndex: Integer;
begin
  DBLog('start TMidigridOverview.DeleteNoteGUI');

  for lIndex := Pred(FNoteListGUI.Count) downto 0 do
  begin
    lMidiNoteOverview := TMidiNoteOverview(FNoteListGUI[lIndex]);

    if Assigned(lMidiNoteOverview) then
    begin
      if lMidiNoteOverview.ObjectID = AObjectID then
      begin
        FNoteListGUI.Remove(lMidiNoteOverview);
        break;
      end;
    end;
  end;

  DBLog('end TMidigridOverview.DeleteNoteGUI');
end;

{
  Convert location in time to a screen cursor position
}
function TMidigridOverview.ConvertTimeToScreen(ATime: Integer): Integer;
begin
  Result := Round(ATime * (Width / FTotalWidth));
end;

{
  Convert location in time to a screen cursor position
}
function TMidigridOverview.ConvertScreenToTime(AX: Integer): Integer;
begin
  Result := Round(AX * (FTotalWidth / Width));
end;

function TMidigridOverview.CalculateTotalWidth: Integer;
var
  lNoteIndex: Integer;
  lMinimalLocation: Integer;
  lMaximalLocation: Integer;
  lNote: TMidiNoteOverview;
begin
  Result := 0;
  for lNoteIndex := 0 to Pred(FNoteListGUI.Count) do
  begin
    lNote := TMidiNoteOverview(FNoteListGUI[lNoteIndex]);
    if lNoteIndex = 0 then
    begin
      lMinimalLocation := lNote.NoteLocation;
      lMaximalLocation := lNote.NoteLocation + lNote.NoteLength;
    end
    else
    begin
      if lNote.NoteLocation < lMinimalLocation then
      begin
        lMinimalLocation := lNote.NoteLocation;
      end;
      if (lNote.NoteLocation + lNote.NoteLength) > lMaximalLocation then
      begin
        lMaximalLocation := lNote.NoteLocation + lNote.NoteLength;
      end;
    end;
  end;
  Result := lMaximalLocation - lMinimalLocation;
end;

{
  Convert a note to a screen note position
}
function TMidigridOverview.ConvertNoteToScreen(ANote: Integer): Integer;
var
  lScale: single;
begin
  lScale := (128 / Height);

  Result := Round(Height - Round(ANote / lScale));
end;

{ TMidiNoteOverview }

procedure TMidiNoteOverview.Update(Subject: THybridPersistentModel);
var
  lNote: TMidiNote;
begin
  DBLog('start TMidiNoteOverview.Update');

  lNote := TMidiNote(Subject);

  if Assigned(lNote) then
  begin
    Selected := lNote.Selected;
    NoteLocation := lNote.NoteLocation;
    Note := lNote.Note;
    NoteVelocity := lNote.NoteVelocity;
    NoteLength := lNote.NoteLength;
  end;

  DBLog('end TMidiNoteOverview.Update');
end;

initialization
  RegisterClass(TMidiPatternGUI);
  RegisterClass(TMidiNoteGUI);
end.

