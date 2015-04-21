{
  Copyright (C) 2015 Robbert Latumahina

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
}
unit midicommand;

{$mode objfpc}{$H+}

interface

uses
    global_command, midi, global, globalconst, Classes;

type
  { TMidiCommand }

  TMidiCommand = class(TCommand)
  private
    FMidiPattern: TMidiPattern;
  public
    procedure Initialize; override;
    procedure Finalize; override;
  end;

  { TControllerEvent }

  TControllerEvent = class(THybridPersistentModel)
  private
    FControllerId: Integer;
    FLocation: Integer;
    FValue: Integer;
    FMapped: Boolean;
    FMidiData: TMidiData;
    FSelected: Boolean;
    function GetValue: Integer;
    procedure SetControllerId(AValue: Integer);
    procedure SetLocation(AValue: Integer);
    procedure SetValue(AValue: Integer);
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean); reintroduce;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Assign(Source: TPersistent); override;
    property MidiData: TMidiData read FMidiData write FMidiData;
    property Mapped: Boolean read FMapped;
  published
    property Value: Integer read GetValue write SetValue;
    property Location: Integer read FLocation write SetLocation;
    property ControllerId: Integer read FControllerId write SetControllerId;
    property Selected: Boolean read FSelected write FSelected;
  end;

  TControllerEditState = (cesBefore, cesChanging, cesAfter);

  { TControllerEditCommand }

  TControllerEditCommand = class(TMidiCommand)
  private
    FControllerDataId: string;
    FControllerId: Integer;
    FLocation: Integer;
    FOldDataValue: Integer;
    FDataValue: Integer;
    FState: TControllerEditState;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  public
    property Location: Integer read FLocation write FLocation;
    property ControllerId: Integer read FControllerId write FControllerId;
    property ControllerDataId: string read FControllerDataId write FControllerDataId;
    property DataValue: Integer read FDataValue write FDataValue;
    property State: TControllerEditState read FState write FState;
  end;

  { TControllerCreateCommand }

  TControllerCreateCommand = class(TMidiCommand)
  private
    FControllerDataId: string;
    FControllerId: Integer;
    FLocation: Integer;
    FDataValue: Integer;
    FOldObjectId: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  public
    property Location: Integer read FLocation write FLocation;
    property ControllerId: Integer read FControllerId write FControllerId;
    property ControllerDataId: string read FControllerDataId write FControllerDataId;
    property DataValue: Integer read FDataValue write FDataValue;
  end;

  TUpdateLoopMarkerCommand = class(TMidiCommand)
  private
    FDataType: TLoopMarkerType;
    FLocation: Integer;
    FOldLocation: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Location: Integer read FLocation write FLocation;
    property DataType: TLoopMarkerType read FDataType write FDataType;
  end;

  { TStretchNotesCommand }

  TStretchNotesCommand = class(TMidiCommand)
  private
    FNoteLengthDiff: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property NoteLengthDiff: Integer read FNoteLengthDiff write FNoteLengthDiff;
  end;

  { TMoveNotesCommand }

  TMoveNotesCommand = class(TMidiCommand)
  private
    FNoteLocationDiff: Integer;
    FNoteDiff: Integer;
    FAction: TMoveCommandAction;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property NoteLocationDiff: Integer read FNoteLocationDiff write FNoteLocationDiff;
    property NoteDiff: Integer read FNoteDiff write FNoteDiff;
    property Action: TMoveCommandAction read FAction write FAction;
  end;

  { TDeleteNotesCommand }

  TDeleteNotesCommand = class(TMidiCommand)
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  end;

  { TCreateNotesCommand }

  TCreateNotesCommand = class(TMidiCommand)
  private
    FOldObjectID: string;
    FNote: Integer;
    FLocation: Integer;
    FNoteLength: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Note: Integer read FNote write FNote;
    property Location: Integer read FLocation write FLocation;
    property NoteLength: Integer read FNoteLength write FNoteLength;
  end;

  { TSelectNoteCommand }

  TSelectNoteCommand = class(TMidiCommand)
  private
    FAddMode: Boolean;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property AddMode: Boolean read FAddMode write FAddMode;
  end;

  { TQuantizeSettingCommand }

  TQuantizeSettingCommand = class(TMidiCommand)
  private
    FQuantizeSetting: Integer;
    FOldQuantizeSetting: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  public
    property QuantizeSetting: Integer read FQuantizeSetting write FQuantizeSetting;
  end;

  { TQuantizeSelectionCommand }

  TQuantizeSelectionCommand = class(TMidiCommand)
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  end;

  { TChangeMidiChannelCommand }

  TChangeMidiChannelCommand = class(TMidiCommand)
  private
    FMidiChannel: Integer;
    FOldMidiChannel: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;

  published
    property MidiChannel: Integer read FMidiChannel write FMidiChannel;
  end;

  { TMuteNotesCommand }

  TMuteNotesCommand = class(TMidiCommand)
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  end;

implementation

uses
  SysUtils, utils;

{ TControllerCreateCommand }

procedure TControllerCreateCommand.DoExecute;
var
  lControllerDataEvent: TControllerEvent;
begin
  FMidiPattern.BeginUpdate;

  DBLog(Format('TControllerCreateCommand.DoExecute Location %d Value %d Controller %d',
    [FLocation, FDataValue, FControllerId]));

  lControllerDataEvent := TControllerEvent.Create(FMidiPattern.ObjectID, MAPPED);
  lControllerDataEvent.Value := FDataValue;
  lControllerDataEvent.Location := FLocation;
  lControllerDataEvent.ControllerId := FControllerId;
  FMidiPattern.ControllerList.Add(lControllerDataEvent);

  if lControllerDataEvent.Mapped then
  begin
    FMidiPattern.MidiDataList.Add(lControllerDataEvent.MidiData);
  end;

  FOldObjectId := lControllerDataEvent.ObjectID;

  FMidiPattern.EndUpdate;
end;

procedure TControllerCreateCommand.DoRollback;
var
  lControllerDataEvent: TControllerEvent;
  lIndex: Integer;
begin
  FMidiPattern.BeginUpdate;

  for lIndex := Pred(FMidiPattern.ControllerList.Count) downto 0 do
  begin
    lControllerDataEvent := TControllerEvent(FMidiPattern.ControllerList[lIndex]);
    if Assigned(lControllerDataEvent) then
    begin
      if lControllerDataEvent.ObjectID = FOldObjectId then
      begin
        if lControllerDataEvent.Mapped then
        begin
          FMidiPattern.MidiDataList.Remove(lControllerDataEvent.MidiData);
        end;

        FMidiPattern.ControllerList.Remove(lControllerDataEvent);

        break;
      end;
    end;
  end;

  FMidiPattern.MidiDataList.IndexList;
  FMidiPattern.EndUpdate;
end;

{ TControllerEvent }

function TControllerEvent.GetValue: Integer;
begin
  Result := FValue;
end;

procedure TControllerEvent.SetControllerId(AValue: Integer);
begin
  FControllerId := AValue;

  if FMapped then
  begin
    FMidiData.DataValue1 := AValue;
  end;
end;

procedure TControllerEvent.SetLocation(AValue: Integer);
begin
  FLocation:= AValue;

  if FMapped then
  begin
    FMidiData.Location := FLocation;
  end;
end;

procedure TControllerEvent.SetValue(AValue: Integer);
begin
  FValue := AValue;

  if FMapped then
  begin
    FMidiData.DataValue2 := FValue;
  end;
end;

constructor TControllerEvent.Create(AObjectOwner: string; AMapped: Boolean);
begin
  inherited Create(AObjectOwner, AMapped);

  FMapped := AMapped;

  if FMapped then
  begin
    FMidiData := TMidiData.Create(Self);
    FMidiData.DataType := mtCC;
    FMidiData.MidiChannel := 1;
  end;

  FSelected := False;
end;

destructor TControllerEvent.Destroy;
begin
  if FMapped then
  begin
    FMidiData.Free;
  end;

  inherited Destroy;
end;

procedure TControllerEvent.Initialize;
begin
  inherited Initialize;
end;

procedure TControllerEvent.Finalize;
begin
  inherited Finalize;
end;

procedure TControllerEvent.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ TMuteNotesCommand }

procedure TMuteNotesCommand.DoExecute;
begin
  //
end;

procedure TMuteNotesCommand.DoRollback;
begin
  //
end;

{ TDeleteNotesCommand }

procedure TDeleteNotesCommand.DoExecute;
var
  i: Integer;
  lMidinote: TMidiNote;
  lMementoNote: TMidiNote;
begin
  DBLog('start TDeleteNotesCommand.DoExecute');

  FMidiPattern.BeginUpdate;

  for i := Pred(FMidiPattern.NoteList.Count) downto 0 do
  begin
    lMidinote := TMidiNote(FMidiPattern.NoteList[i]);

    if lMidinote.Selected or (lMidinote.ObjectID = ObjectID) then
    begin
      lMementoNote := TMidiNote.Create(FMidiPattern.ObjectID, NOT_MAPPED);
      lMementoNote.Note := lMidiNote.Note;
      lMementoNote.NoteLength := lMidiNote.NoteLength;
      lMementoNote.NoteLocation := lMidiNote.NoteLocation;
      lMementoNote.NoteVelocity := lMidiNote.NoteVelocity;
      lMementoNote.ObjectID := lMidiNote.ObjectID;
      lMementoNote.ObjectOwnerID := lMidiNote.ObjectOwnerID;
      Memento.Add(lMementoNote);

      if lMidinote.Mapped then
      begin
        FMidiPattern.MidiDataList.Remove(lMidinote.MidiNoteStart);
        FMidiPattern.MidiDataList.Remove(lMidinote.MidiNoteEnd);
      end;

      FMidiPattern.NoteList.Remove(lMidinote);
    end;
  end;
  FMidiPattern.MidiDataList.IndexList;

  FMidiPattern.EndUpdate;

  DBLog('end TDeleteNotesCommand.DoExecute');
end;

procedure TDeleteNotesCommand.DoRollback;
var
  i: integer;
  lMementoNote: TMidiNote;
  lMidiNote: TMidiNote;
begin
  DBLog('start TDeleteNotesCommand.DoRollback');

  FMidiPattern.BeginUpdate;

  if Memento.Count > 0 then
  begin
    for i := 0 to Pred(Memento.Count) do
    begin
      lMementoNote := TMidiNote(Memento[i]);
      lMidiNote := TMidiNote.Create(FMidiPattern.ObjectID, NOT_MAPPED);
      lMidiNote.Note := lMementoNote.Note;
      lMidiNote.NoteLength := lMementoNote.NoteLength;
      lMidiNote.NoteLocation := lMementoNote.NoteLocation;
      lMidiNote.NoteVelocity := lMementoNote.NoteVelocity;
      lMidiNote.ObjectID := lMementoNote.ObjectID;
      lMidiNote.ObjectOwnerID := lMementoNote.ObjectOwnerID;

      FMidiPattern.NoteList.Add(lMidiNote);

      if lMidinote.Mapped then
      begin
        FMidiPattern.MidiDataList.Add(lMidiNote.MidiNoteStart);
        FMidiPattern.MidiDataList.Add(lMidiNote.MidiNoteEnd);
      end;
    end;
    FMidiPattern.MidiDataList.IndexList;
  end;

  FMidiPattern.EndUpdate;

  DBLog('end TDeleteNotesCommand.DoRollback');
end;

{ TCreateNotesCommand }

procedure TCreateNotesCommand.DoExecute;
var
  lMidiNote: TMidiNote;
begin
  DBLog('start TCreateNotesCommand.DoExecute');

  FMidiPattern.BeginUpdate;

  lMidiNote := TMidiNote.Create(FMidiPattern.ObjectID, MAPPED);
  lMidiNote.Note := Note;
  lMidiNote.NoteLocation := Location;
  lMidiNote.NoteLength := NoteLength;//Round(FMidiPattern.QuantizeValue);
  lMidiNote.NoteVelocity := DEFAULT_NOTE_VELOCITY;

  FMidiPattern.NoteList.Add(lMidiNote);
  if lMidinote.Mapped then
  begin
    FMidiPattern.MidiDataList.Add(lMidiNote.MidiNoteStart);
    FMidiPattern.MidiDataList.Add(lMidiNote.MidiNoteEnd);
  end;

  FOldObjectID := lMidiNote.ObjectID;

  FMidiPattern.MidiDataList.IndexList;

  FMidiPattern.EndUpdate;

  DBLog('end TCreateNotesCommand.DoExecute');
end;

procedure TCreateNotesCommand.DoRollback;
var
  lNoteIndex: Integer;
  lMidiNote: TMidiNote;
begin
  DBLog('start TCreateNotesCommand.DoRollback');

  FMidiPattern.BeginUpdate;

  for lNoteIndex := Pred(FMidiPattern.NoteList.Count) downto 0 do
  begin
    lMidiNote := TMidiNote(FMidiPattern.NoteList[lNoteIndex]);
    if FOldObjectID = lMidiNote.ObjectID then
    begin
      if lMidinote.Mapped then
      begin
        FMidiPattern.MidiDataList.Remove(lMidiNote.MidiNoteStart);
        FMidiPattern.MidiDataList.Remove(lMidiNote.MidiNoteEnd);
      end;

      FMidiPattern.NoteList.Remove(lMidiNote);

      break;
    end;
  end;

  FMidiPattern.MidiDataList.IndexList;
  FMidiPattern.EndUpdate;

  DBLog('end TCreateNotesCommand.DoRollback');
end;


{ TSelectNoteCommand }

procedure TSelectNoteCommand.DoExecute;
var
  lMidiNote: TMidiNote;
  lCurrentNote: TMidiNote;
  lMementoNote: TMidiNote;
  lNoteIndex: Integer;
begin
  DBLog('start TSelectNoteCommand.DoExecute');

  lMidiNote := FMidiPattern.NoteByObjectID(ObjectID);
  lCurrentNote := lMidiNote;

  // If actual selected note is already selected => do not clear other selection
  if Assigned(lMidiNote) then
  begin
    if not lMidiNote.Selected then
    begin
      // Clear selection if not in adding mode 'Shift down'
      if not FAddMode then
      begin
        for lNoteIndex := 0 to Pred(FMidiPattern.NoteList.Count) do
        begin
          lMidinote:= TMidiNote(FMidiPattern.NoteList[lNoteIndex]);
          lMidinote.Selected := False;
          lMidiNote.Notify;
        end;
      end;
    end;
  end;

  // Now select/deselect the current note
  if Assigned(lCurrentNote) then
  begin
    lMementoNote := TMidiNote.Create(FMidiPattern.ObjectID, NOT_MAPPED);
    lMementoNote.ObjectID := lCurrentNote.ObjectID;
    lMementoNote.Selected := lCurrentNote.Selected;
    Memento.Add(lMementoNote);

    if FAddMode then
      lCurrentNote.Selected := not lCurrentNote.Selected
    else
      lCurrentNote.Selected := True;

    lCurrentNote.Notify;
  end;

  DBLog('end TSelectNoteCommand.DoExecute');
end;

procedure TSelectNoteCommand.DoRollback;
var
  lMidiNote: TMidiNote;
  lMementoNote: TMidiNote;
  lIndex: Integer;
begin
  DBLog('start TSelectNoteCommand.DoRollback');

  for lIndex := 0 to Pred(Memento.Count) do
  begin
    lMementoNote := TMidiNote(Memento[lIndex]);

    lMidiNote := FMidiPattern.NoteByObjectID(lMementoNote.ObjectID);

    if Assigned(lMidiNote) then
    begin
      lMidinote.Selected := lMementoNote.Selected;
      lMidiNote.Notify;
    end;
  end;

  DBLog('end TSelectNoteCommand.DoRollback');
end;

{ TMoveNotesCommand }

procedure TMoveNotesCommand.DoExecute;
var
  lMidiNote: TMidiNote;
  lNoteIndex: Integer;
  lMementoNote: TMidiNote;
begin
  DBLog('start TMoveNotesCommand.DoExecute');

  // Move selected notes
  for lNoteIndex := 0 to Pred(FMidiPattern.NoteList.Count) do
  begin
    lMidinote := TMidiNote(FMidiPattern.NoteList[lNoteIndex]);

    if Assigned(lMidinote) then
    begin
      if lMidinote.Selected then
      begin
        if Persist then
        begin
          lMidiNote.OriginalNote := lMidiNote.Note;
          lMidiNote.OriginalNoteLocation := lMidiNote.NoteLocation;

          lMementoNote := TMidiNote.Create(FMidiPattern.ObjectID, NOT_MAPPED);
          lMementoNote.ObjectID := lMidinote.ObjectID;
          lMementoNote.Note := lMidinote.OriginalNote;
          lMementoNote.NoteLength := lMidinote.NoteLength;
          lMementoNote.NoteLocation := lMidinote.OriginalNoteLocation;
          lMementoNote.NoteVelocity := lMidinote.NoteVelocity;
          Memento.Add(lMementoNote);
        end;

        lMidiNote.Note := lMidiNote.OriginalNote + NoteDiff;
        lMidiNote.NoteLocation := lMidiNote.OriginalNoteLocation + NoteLocationDiff;

        // now quantize it
        lMidiNote.NoteLocation := FMidiPattern.QuantizeLocation(lMidiNote.NoteLocation);

        lMidiNote.Notify;
      end;
    end;
  end;

  FMidiPattern.MidiDataList.IndexList;

  FMidiPattern.Notify;

  DBLog('end TMoveNotesCommand.DoExecute');
end;

procedure TMoveNotesCommand.DoRollback;
var
  lMidinote: TMidiNote;
  lMementoMidinote: TMidiNote;
  i: Integer;
begin
  for i := 0 to Pred(Memento.Count) do
  begin
    lMementoMidinote := TMidiNote(Memento[i]);
    lMidinote := FMidiPattern.NoteByObjectID(lMementoMidinote.ObjectID);

    if Assigned(lMidinote) then
    begin
      lMidinote.NoteLocation:= lMementoMidinote.NoteLocation;
      lMidinote.Note := lMementoMidinote.Note;
      lMidinote.Notify;
    end;
  end;
  FMidiPattern.MidiDataList.IndexList;

  FMidiPattern.Notify;
end;

{ TStretchNotesCommand }

procedure TStretchNotesCommand.DoExecute;
var
  j: Integer;
  lMidinote: TMidiNote;
  lMementoNote: TMidiNote;
begin
  for j := 0 to Pred(FMidiPattern.NoteList.Count) do
  begin
    lMidinote:= TMidiNote(FMidiPattern.NoteList[j]);

    if lMidinote.Selected then
    begin
      if Persist then
      begin
        // Store original length
        lMidinote.OriginalNoteLength := lMidinote.NoteLength;

        lMementoNote := TMidiNote.Create(ObjectOwner, NOT_MAPPED);
        lMementoNote.Note := lMidiNote.Note;
        lMementoNote.NoteLength := lMidiNote.NoteLength;
        lMementoNote.NoteLocation := lMidiNote.NoteLocation;
        lMementoNote.NoteVelocity := lMidiNote.NoteVelocity;
        lMementoNote.ObjectID := lMidinote.ObjectID;
        lMementoNote.ObjectOwnerID := lMidinote.ObjectOwnerID;
        Memento.Add(lMementoNote);
      end
      else
      begin
        // Add diff to original length
        lMidinote.NoteLength := lMidinote.OriginalNoteLength + NoteLengthDiff;
        lMidinote.Notify;
      end;
    end;
  end;
  FMidiPattern.MidiDataList.IndexList;
end;


procedure TStretchNotesCommand.DoRollback;
var
  lMidinote: TMidiNote;
  lMementoMidiNote: TMidiNote;
  i: Integer;
begin
  for i := 0 to Pred(Memento.Count) do
  begin
    lMementoMidiNote := TMidiNote(Memento[i]);
    lMidinote := FMidiPattern.NoteByObjectID(lMementoMidiNote.ObjectID);

    if Assigned(lMidinote) then
    begin
      lMidinote.NoteLength:= lMementoMidiNote.NoteLength;
      lMidinote.Notify;
    end;
  end;
  FMidiPattern.MidiDataList.IndexList;
end;

{ TQuantizeSettingCommand }

procedure TQuantizeSettingCommand.DoExecute;
begin
  DBLog('start TQuantizeSettingCommand.DoExecute');

  FMidiPattern.BeginUpdate;

  FOldQuantizeSetting := FMidiPattern.QuantizeSetting;

  FMidiPattern.QuantizeSetting := QuantizeSetting;

  case FMidiPattern.QuantizeSetting of
  0: FMidiPattern.QuantizeValue := -1;
  1: FMidiPattern.QuantizeValue := 22050 * 4;
  2: FMidiPattern.QuantizeValue := 22050 * 2;
  3: FMidiPattern.QuantizeValue := 22050;
  4: FMidiPattern.QuantizeValue := 22050 / 2;
  5: FMidiPattern.QuantizeValue := 22050 / 3;
  6: FMidiPattern.QuantizeValue := 22050 / 4;
  7: FMidiPattern.QuantizeValue := 22050 / 6;
  8: FMidiPattern.QuantizeValue := 22050 / 8;
  9: FMidiPattern.QuantizeValue := 22050 / 16;
  10: FMidiPattern.QuantizeValue := 22050 / 32;
  end;

  FMidiPattern.EndUpdate;

  DBLog('end TQuantizeSettingCommand.DoExecute');
end;

procedure TQuantizeSettingCommand.DoRollback;
begin
  DBLog('start TQuantizeSettingCommand.DoRollback');

  FMidiPattern.BeginUpdate;

  FMidiPattern.QuantizeSetting := FOldQuantizeSetting;

  case FMidiPattern.QuantizeSetting of
  0: FMidiPattern.QuantizeValue := -1;
  1: FMidiPattern.QuantizeValue := 22050 * 4;
  2: FMidiPattern.QuantizeValue := 22050 * 2;
  3: FMidiPattern.QuantizeValue := 22050;
  4: FMidiPattern.QuantizeValue := 22050 / 2;
  5: FMidiPattern.QuantizeValue := 22050 / 3;
  6: FMidiPattern.QuantizeValue := 22050 / 4;
  7: FMidiPattern.QuantizeValue := 22050 / 6;
  8: FMidiPattern.QuantizeValue := 22050 / 8;
  9: FMidiPattern.QuantizeValue := 22050 / 16;
  10: FMidiPattern.QuantizeValue := 22050 / 32;
  end;

  FMidiPattern.EndUpdate;

  DBLog('end TQuantizeSettingCommand.DoRollback');
end;

{ TQuantizeSelectionCommand }

procedure TQuantizeSelectionCommand.DoExecute;
var
  j: Integer;
  lMidinote: TMidiNote;
  lMementoNote: TMidiNote;
begin
  for j := 0 to Pred(FMidiPattern.NoteList.Count) do
  begin
    lMidinote:= TMidiNote(FMidiPattern.NoteList[j]);

    if lMidinote.Selected then
    begin
      if Persist then
      begin
        lMementoNote := TMidiNote.Create(ObjectOwner, NOT_MAPPED);
        lMementoNote.Note := lMidiNote.Note;
        lMementoNote.NoteLength := lMidiNote.NoteLength;
        lMementoNote.NoteLocation := lMidiNote.NoteLocation;
        lMementoNote.NoteVelocity := lMidiNote.NoteVelocity;
        lMementoNote.ObjectID := lMidinote.ObjectID;
        lMementoNote.ObjectOwnerID := lMidinote.ObjectOwnerID;
        Memento.Add(lMementoNote);
      end;

      lMidinote.NoteLocation:= (lMidinote.NoteLocation div FMidiPattern.QuantizeSetting) * FMidiPattern.QuantizeSetting;
      lMidinote.Notify;
    end;
  end;
  FMidiPattern.MidiDataList.IndexList;
end;

procedure TQuantizeSelectionCommand.DoRollback;
var
  lMementoNote: TMidiNote;
  lMidinote: TMidiNote;
  i, j: Integer;
begin
  for i := 0 to Pred(Memento.Count) do
  begin
    lMementoNote := TMidiNote(Memento[i]);
    for j := 0 to Pred(FMidiPattern.NoteList.Count) do
    begin
      lMidinote := TMidiNote(FMidiPattern.NoteList[j]);
      if lMementoNote.ObjectID = lMidinote.ObjectID then
      begin
        lMidinote.NoteLocation := lMementoNote.NoteLocation;
        lMidinote.NoteLength := lMidinote.NoteLength;
        lMidinote.Notify;
        Break;
      end;
    end;
  end;
  FMidiPattern.MidiDataList.IndexList;
end;

{ TChangeMidiChannelCommand }

procedure TChangeMidiChannelCommand.DoExecute;
begin
  DBLog('start TChangeMidiChannelCommand.DoExecute');

  FMidiPattern.BeginUpdate;

  // Store MidiChannel
  FOldMidiChannel := FMidiChannel;
  FMidiPattern.MidiChannel := FMidiChannel;
  DBLog(Format('Change midiChannel to %d',[FMidiPattern.MidiChannel]));

  FMidiPattern.EndUpdate;

  DBLog('end TChangeMidiChannelCommand.DoExecute');
end;

procedure TChangeMidiChannelCommand.DoRollback;
begin
  DBLog('start TChangeMidiChannelCommand.DoRollback');

  FMidiPattern.BeginUpdate;

  FMidiPattern.MidiChannel := FOldMidiChannel;

  FMidiPattern.EndUpdate;

  DBLog('end TChangeMidiChannelCommand.DoRollback');
end;

{ TUpdateLoopMarkerCommand }

procedure TUpdateLoopMarkerCommand.DoExecute;
var
  lQuantize: Integer;
begin
  DBLog('start TUpdateWaveLoopMarkerCommand.DoExecute');

  lQuantize := Round(GSettings.SampleRate / 4);

  if Persist then
  begin
    // Save state
    case FDataType of
    ltStart: FOldLocation := FMidiPattern.LoopStart.Value;
    ltEnd: FOldLocation := FMidiPattern.LoopEnd.Value;
    ltLength: FOldLocation := FMidiPattern.LoopLength.Value;
    end;
  end;

  // Assign
  case FDataType of
  ltStart:
  begin
    if FLocation < 0 then FLocation := 0;
    FMidiPattern.LoopStart.Value := (FLocation div lQuantize) * lQuantize;
    FMidiPattern.LoopEnd.Value :=
      FMidiPattern.LoopStart.Value + FMidiPattern.LoopLength.Value;

    FMidiPattern.LoopEnd.Value := (FMidiPattern.LoopEnd.Value div lQuantize) * lQuantize;
  end;
  ltEnd:
  begin
    if FLocation < 0 then FLocation := 0;
    FMidiPattern.LoopEnd.Value := (FLocation div lQuantize) * lQuantize;

    FMidiPattern.LoopLength.Value :=
      FMidiPattern.LoopEnd.Value - FMidiPattern.LoopStart.Value;

    FMidiPattern.LoopLength.Value := (FMidiPattern.LoopLength.Value div lQuantize) * lQuantize;
  end;
  ltLength:
  begin
    FMidiPattern.LoopLength.Value := (FLocation div lQuantize) * lQuantize;
    FMidiPattern.LoopEnd.Value := FMidiPattern.LoopStart.Value + FLocation;
    FMidiPattern.LoopEnd.Value := (FMidiPattern.LoopEnd.Value div lQuantize) * lQuantize;
  end;
  end;

  // Update observers
  FMidiPattern.Notify;
  FMidiPattern.LoopStart.Notify;
  FMidiPattern.LoopEnd.Notify;
  FMidiPattern.LoopLength.Notify;

  DBLog('end TUpdateWaveLoopMarkerCommand.DoExecute');
end;

procedure TUpdateLoopMarkerCommand.DoRollback;
begin
  DBLog('start TUpdateWaveLoopStartCommand.DoRollback');

  // Retrieve state
  FMidiPattern.LoopStart.Value := FOldLocation;

  // Assign
  case FDataType of
  ltStart: FMidiPattern.LoopStart.Value := FOldLocation;
  ltEnd: FMidiPattern.LoopEnd.Value := FOldLocation;
  ltLength: FMidiPattern.LoopLength.Value := FOldLocation;
  end;

  // Update observers
  FMidiPattern.Notify;
  FMidiPattern.LoopStart.Notify;
  FMidiPattern.LoopEnd.Notify;
  FMidiPattern.LoopLength.Notify;

  DBLog('end TUpdateWaveLoopStartCommand.DoRollback');
end;
{ TMidiCommand }

procedure TMidiCommand.Initialize;
begin
  inherited Initialize;

  FMidiPattern := TMidiPattern(GObjectMapper.GetModelObject(ObjectOwner));
end;

procedure TMidiCommand.Finalize;
begin

  inherited Finalize;
end;

{ TControllerEditCommand }

procedure TControllerEditCommand.DoExecute;
var
  lControllerDataEvent: TControllerEvent;
  lMidiNote: TMidiNote;
  {lIndex: Integer;
  lDiff: Integer;   }
begin
  DBLog('start TControllerEditCommand.DoExecute ' + FControllerDataId);

  if FControllerId = MIDI_VELOCITY then
  begin
    DBLog('Editing velocity');

    lMidiNote := TMidiNote(GObjectMapper.GetModelObject(ObjectID));

    if Assigned(lMidiNote) then
    begin
      FMidiPattern.BeginUpdate;

      case FState of
        cesBefore:
        begin
          FOldDataValue := FDataValue;
        end;
        cesChanging:
        begin
          lMidiNote.NoteVelocity := FDataValue - FOldDataValue;
          lMidiNote.Notify;
        end;
        cesAfter:
        begin
          //
        end;
      end;

      FMidiPattern.EndUpdate;
    end
  end
  else
  begin
    DBLog('Editing controller');

    lControllerDataEvent := TControllerEvent(GObjectMapper.GetModelObject(ObjectID));

    if Assigned(lControllerDataEvent) then
    begin
      FMidiPattern.BeginUpdate;

      case FState of
        cesBefore:
        begin
          FOldDataValue := FDataValue;
        end;
        cesChanging:
        begin
          lControllerDataEvent.Value := FDataValue - FOldDataValue;
          lControllerDataEvent.Notify;
        end;
        cesAfter:
        begin
          //
        end;
      end;

      FMidiPattern.EndUpdate;
    end;
  end;

  DBLog('end TControllerEditCommand.DoExecute ' + FControllerDataId);
end;

procedure TControllerEditCommand.DoRollback;
var
  lControllerDataEvent: TControllerEvent;
  lMidiNote: TMidiNote;
begin
  if FControllerId = MIDI_VELOCITY then
  begin
    lMidiNote := TMidiNote(GObjectMapper.GetModelObject(ObjectID));
    if Assigned(lControllerDataEvent) then
    begin
      FMidiPattern.BeginUpdate;

      FDataValue := FOldDataValue;
      lMidiNote.NoteVelocity := FOldDataValue;
      lMidiNote.Notify;

      FMidiPattern.EndUpdate;
    end;
  end
  else
  begin
    lControllerDataEvent := TControllerEvent(GObjectMapper.GetModelObject(ObjectID));
    if Assigned(lControllerDataEvent) then
    begin
      FMidiPattern.BeginUpdate;

      FDataValue := FOldDataValue;
      lControllerDataEvent.Value := FOldDataValue;
      lControllerDataEvent.Notify;

      FMidiPattern.EndUpdate;
    end;
  end;
end;


end.

