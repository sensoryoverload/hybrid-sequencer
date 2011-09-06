{
  Copyright (C) 2007 Robbert Latumahina

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

  waveform.pas
}
unit midi;

interface

uses
 Classes, SysUtils, Controls, Graphics, LCLType, Forms, ExtCtrls, ctypes, sndfile,
 jacktypes, StdCtrls, Dialogs, Spin, bpm, beattrigger, Utils,
 globalconst, SoundTouchObject, contnrs, global_command, ShellCtrls,
 global, math, pattern, sampler;

const
  MAX_LATENCY = 20000;
  DECIMATED_CACHE_DISTANCE = 64;

type
  TShuffleRefreshEvent = procedure(TrackObject: TObject) of object;

  { Forward declarations }
  TMidiPattern = class;

  { TMidiCommand }

  TMidiCommand = class(TCommand)
  private
    FMidiPattern: TMidiPattern;
  protected
    procedure Initialize; override;
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

  { TSelectObjectListCommand }

  TSelectObjectListCommand = class(TMidiCommand)
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


  { TMidiNote }

  TMidiNote = class(THybridPersistentModel)
  private
    { Audio }
    FNoteLocation: Integer; // Which time format ? in samples ??
    FNote: Integer;
    FNoteVelocity: Integer;
    FNoteLength: Integer;
    FOriginalNoteLocation: Integer;
    FOriginalNote: Integer;
    FOriginalNoteVelocity: Integer;
    FOriginalNoteLength: Integer;
    FSelected: Boolean;
    FMidiNoteStart: TMidiData;
    FMidiNoteEnd: TMidiData;
    function GetNote: Integer;
    procedure SetNote(const AValue: Integer);
    procedure SetNoteLength(const AValue: Integer);
    procedure SetNoteLocation(const AValue: Integer);
    procedure SetNoteVelocity(const AValue: Integer);
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean);
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Assign(Source: TPersistent); override;
    property MidiNoteStart: TMidiData read FMidiNoteStart write FMidiNoteStart;
    property MidiNoteEnd: TMidiData read FMidiNoteEnd write FMidiNoteEnd;
  published
    property Note: Integer read GetNote write SetNote;
    property NoteLocation: Integer read FNoteLocation write SetNoteLocation;
    property NoteVelocity: Integer read FNoteVelocity write SetNoteVelocity;
    property NoteLength: Integer read FNoteLength write SetNoteLength;
    property OriginalNote: Integer read FOriginalNote write FOriginalNote;
    property OriginalNoteLocation: Integer read FOriginalNoteLocation write FOriginalNoteLocation;
    property OriginalNoteVelocity: Integer read FOriginalNoteVelocity write FOriginalNoteVelocity;
    property OriginalNoteLength: Integer read FOriginalNoteLength write FOriginalNoteLength;
    property Selected: Boolean read FSelected write FSelected;
  end;

  { TMidiPattern }

  TMidiPattern = class(TPattern)
  private
    // Data

    FMidiDataList: TMidiDataList;
    FNoteList: TObjectList;
    FQuantizeSetting: Integer;
    FQuantizeValue: Single;
    FEnabled: Boolean;

    // Engine
    FRealCursorPosition: Integer;
    FCursorAdder: Single;
    FMidiDataCursor: TMidiData;
    FBPMScale: Single;

    FMidiBuffer: TMidiBuffer;

    // Private sampler, not a plugin so it's more thightly integrated
    FSample: TSample;
    FSampleBank: TSampleBank;
    FSampleBankEngine: TSampleBankEngine;

    function GetEnabled: Boolean;
    function NoteByObjectID(AObjectID: string): TMidiNote;
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Assign(Source: TPersistent); override;
    function QuantizeLocation(ALocation: Integer): Integer;
    function StartVirtualLocation(ALocation: Integer): TMidiData;

    procedure ProcessInit; override;
    procedure Process(ABuffer: PSingle; AFrameIndex: Integer; AFrameCount: Integer); override;
    procedure ProcessAdvance; override;

    property MidiDataList: TMidiDataList read FMidiDataList write FMidiDataList;
    property Enabled: Boolean read GetEnabled write FEnabled default True;
    property BPMScale: Single read FBPMScale write FBPMScale;
    property RealCursorPosition: Integer read FRealCursorPosition write FRealCursorPosition;
    property CursorAdder: Single read FCursorAdder write FCursorAdder;
    property MidiDataCursor: TMidiData read FMidiDataCursor write FMidiDataCursor;

    {
      Engine
    }
    property MidiBuffer: TMidiBuffer read FMidiBuffer write FMidiBuffer;
    property SampleBankEngine: TSampleBankEngine read FSampleBankEngine write FSampleBankEngine;
  published
    property SampleBank: TSampleBank read FSampleBank write FSampleBank;
    property NoteList: TObjectList read FNoteList write FNoteList;
    property QuantizeSetting: Integer read FQuantizeSetting write FQuantizeSetting default 1;
    property QuantizeValue: Single read FQuantizeValue write FQuantizeValue default 1;
  end;

  TMidiGridEngine = class
  private
  public
  end;


implementation

uses Fx, audiostructure;

constructor TMidiPattern.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  DBLog('start TMidiGrid.Create');

  inherited Create(AObjectOwner, AMapped);

  FOnCreateInstanceCallback := @DoCreateInstance;

  FRealCursorPosition:= LoopStart;

  FNoteList := TObjectList.Create;
  FMidiDataList := TMidiDataList.Create;

  FMidiBuffer := TMidiBuffer.Create;

  FQuantizeSetting := 3;
  FQuantizeValue := 100;

  FSampleBank := TSampleBank.Create(AObjectOwner, AMapped);
  FSampleBankEngine := TSampleBankEngine.Create(GSettings.Frames);

  FSample := TSample.Create(AObjectOwner, AMapped);
  FSample.LoadSample('kick.wav');
  FSample.Initialize;

  FSampleBank.SampleList.Add(FSample);

  FSampleBankEngine.SampleBank := FSampleBank;

  DBLog('KICK.WAV loading');

  DBLog('end TMidiGrid.Create');
end;

destructor TMidiPattern.Destroy;
begin
  FSample.UnloadSample; // TODO Should be done by TSample class itself
  FSample.Free;

  FSampleBankEngine.Free;

  if Assigned(FSampleBank) then
  begin
    FSampleBank.Free;
  end;

  if Assigned(FMidiBuffer) then
    FMidiBuffer.Free;

  if Assigned(FMidiDataList) then
    FMidiDataList.Free;

  if Assigned(FNoteList) then
    FNoteList.Free;

  inherited Destroy;
end;

procedure TMidiPattern.Initialize;
begin
  BeginUpdate;

  Inherited;

  EndUpdate;
end;

function TMidiPattern.QuantizeLocation(ALocation: Integer): Integer;
begin
  DBLog(Format('FQuantizeSetting %d', [FQuantizeSetting]));
  if FQuantizeSetting = 0 then
  begin
    Result := Round(Trunc(ALocation / 100) * 100); ;
  end
  else
  begin
    Result := Round(Trunc(ALocation / FQuantizeValue) * FQuantizeValue);

    DBLog(Format('Floor(ALocation / FQuantizeValue) %d', [Trunc(ALocation / FQuantizeValue)]));
  end;
end;

function TMidiPattern.StartVirtualLocation(ALocation: Integer): TMidiData;
var
  lIndex: Integer;
begin
  Result := nil;
  for lIndex := Pred(FMidiDataList.Count) downto 0 do
  begin
    if TMidiData(FMidiDataList.Items[lIndex]).Location <= ALocation then
    begin
      Result := TMidiData(FMidiDataList.Items[lIndex]);
    end;
  end;
end;

procedure TMidiPattern.ProcessInit;
begin
  // Reset buffer at beginning of callback
  FMidiBuffer.Reset;
  FBPMScale := GAudioStruct.BPMScale;
end;

{
  Process advances cursor
}
procedure TMidiPattern.Process(ABuffer: PSingle; AFrameIndex: Integer; AFrameCount: Integer);
var
  i: Integer;
begin
  {  for i := 0 to Pred(AFrames) do
  begin
    FCursorAdder := FCursorAdder + FBPMScale;
  end;     }

  if (CursorAdder >= LoopEnd) or SyncQuantize then
  begin
    SyncQuantize := False;
    CursorAdder := LoopStart;

    if MidiDataList.Count > 0 then
    begin
      MidiDataList.First;
      MidiDataCursor := TMidiData( MidiDataList.Items[0] );
    end;
  end;

  // Fill MidiBuffer with midi data if found
  if not Updating then
  begin
    if MidiDataList.Count > 0 then
    begin
      while CursorAdder >= MidiDataCursor.Location do
      begin
        // Put event in buffer
        MidiBuffer.WriteEvent(MidiDataCursor, i);  // TODO i ? uninitialized

        if Assigned(MidiDataCursor.Next) then
        begin
          MidiDataCursor := MidiDataCursor.Next
        end
        else
        begin
          break;
        end;
      end;
    end;
  end;

end;

procedure TMidiPattern.ProcessAdvance;
begin
  RealCursorPosition := Round(CursorAdder);
  CursorAdder := CursorAdder + GAudioStruct.BPMScale;
end;

function TMidiPattern.NoteByObjectID(AObjectID: string): TMidiNote;
var
  lIndex: Integer;
begin
  Result := nil;

  for lIndex := 0 to Pred(NoteList.Count) do
  begin
    if TMidiNote(NoteList[lIndex]).ObjectID = AObjectID then
    begin
      Result := TMidiNote(NoteList[lIndex]);
    end;
  end;
end;

function TMidiPattern.GetEnabled: Boolean;
begin
  Result := (FUpdateCount = 0);
end;

procedure TMidiPattern.DoCreateInstance(var AObject: TObject; AClassName: string);
var
  lMidiNote: TMidiNote;
begin
  DBLog('start TMidiGrid.DoCreateInstance');

  // create model Marker
  lMidiNote := TMidiNote.Create(ObjectID, MAPPED);
  lMidiNote.ObjectOwnerID := ObjectID;

  AObject := lMidiNote;

  NoteList.Add(lMidiNote);

  DBLog('end TMidiGrid.DoCreateInstance');
end;

procedure TMidiPattern.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ TMidiNote }

procedure TMidiNote.SetNote(const AValue: Integer);
begin
  FNote := AValue;
  FMidiNoteStart.DataType := mtNoteOn;
  FMidiNoteStart.DataValue1 := FNote;
  FMidiNoteStart.DataValue2 := 127;
  FMidiNoteEnd.DataType := mtNoteOff;
  FMidiNoteEnd.DataValue1 := FNote;
  FMidiNoteEnd.DataValue2 := 0;
end;

function TMidiNote.GetNote: Integer;
begin
  Result := FNote;
end;

procedure TMidiNote.SetNoteLength(const AValue: Integer);
begin
  FNoteLength:= AValue;
  FMidiNoteEnd.Location := (FNoteLocation * 220) + (FNoteLength * 220);
  FMidiNoteStart.Length := FMidiNoteEnd.Location - FMidiNoteStart.Location;
end;

procedure TMidiNote.SetNoteLocation(const AValue: Integer);
begin
  FNoteLocation:= AValue;
  FMidiNoteEnd.Location := (FNoteLocation * 220) + (FNoteLength * 220);

  FMidiNoteStart.Location := FNoteLocation * 220;
  FMidiNoteStart.Length := FMidiNoteEnd.Location - FMidiNoteStart.Location;
end;

procedure TMidiNote.SetNoteVelocity(const AValue: Integer);
begin
  FNoteVelocity:= AValue;
  FMidiNoteStart.DataValue2 := FNoteVelocity;
end;

constructor TMidiNote.Create(AObjectOwner: string; AMapped: Boolean);
begin
  inherited Create(AObjectOwner, AMapped);

  FMidiNoteStart := TMidiData.Create;
  FMidiNoteEnd := TMidiData.Create;

  FSelected:= False;
end;

destructor TMidiNote.Destroy;
begin
  FMidiNoteStart.Free;
  FMidiNoteEnd.Free;

  inherited Destroy;
end;

procedure TMidiNote.Initialize;
begin
  BeginUpdate;

  EndUpdate;
end;

procedure TMidiNote.Finalize;
begin
  //
end;

procedure TMidiNote.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
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

      // Set the midicursor to another not if possible to prevent AV's in the callback
      if (FMidiPattern.MidiDataCursor = lMidinote.MidiNoteStart) or
        (FMidiPattern.MidiDataCursor = lMidinote.MidiNoteEnd) then
      begin
        if FMidiPattern.MidiDataList.Count > 0 then
        begin
          // TODO Make engine for iterating the list
          FMidiPattern.MidiDataCursor := TMidiData(FMidiPattern.MidiDataList[0]);
        end;
      end;
      FMidiPattern.MidiDataList.Remove(lMidinote.MidiNoteStart);
      FMidiPattern.MidiDataList.Remove(lMidinote.MidiNoteEnd);
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
      FMidiPattern.MidiDataList.Add(lMidiNote.MidiNoteStart);
      FMidiPattern.MidiDataList.Add(lMidiNote.MidiNoteEnd);

      FMidiPattern.MidiDataList.IndexList;
    end;
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
  lMidiNote.NoteLength := Round(FMidiPattern.QuantizeValue);
  lMidiNote.NoteVelocity := DEFAULT_NOTE_VELOCITY;
  FMidiPattern.NoteList.Add(lMidiNote);
  FMidiPattern.MidiDataList.Add(lMidiNote.MidiNoteStart);
  FMidiPattern.MidiDataList.Add(lMidiNote.MidiNoteEnd);

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
      writeln(Format('MidiDataList before count %d', [FMidiPattern.MidiDataList.Count]));
      FMidiPattern.MidiDataList.Remove(lMidiNote.MidiNoteStart);
      FMidiPattern.MidiDataList.Remove(lMidiNote.MidiNoteEnd);
      writeln(Format('MidiDataList after count %d', [FMidiPattern.MidiDataList.Count]));

      FMidiPattern.NoteList.Remove(lMidiNote);
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
        lMidinote.NoteLength:= lMidinote.OriginalNoteLength + NoteLengthDiff;
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
  1: FMidiPattern.QuantizeValue := 100 * 4;
  2: FMidiPattern.QuantizeValue := 100 * 2;
  3: FMidiPattern.QuantizeValue := 100;
  4: FMidiPattern.QuantizeValue := 100 / 2;
  5: FMidiPattern.QuantizeValue := 100 / 3;
  6: FMidiPattern.QuantizeValue := 100 / 4;
  7: FMidiPattern.QuantizeValue := 100 / 6;
  8: FMidiPattern.QuantizeValue := 100 / 8;
  9: FMidiPattern.QuantizeValue := 100 / 16;
  10: FMidiPattern.QuantizeValue := 100 / 32;
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
  1: FMidiPattern.QuantizeValue := 100 * 4;
  2: FMidiPattern.QuantizeValue := 100 * 2;
  3: FMidiPattern.QuantizeValue := 100;
  4: FMidiPattern.QuantizeValue := 100 / 2;
  5: FMidiPattern.QuantizeValue := 100 / 3;
  6: FMidiPattern.QuantizeValue := 100 / 4;
  7: FMidiPattern.QuantizeValue := 100 / 6;
  8: FMidiPattern.QuantizeValue := 100 / 8;
  9: FMidiPattern.QuantizeValue := 100 / 16;
  10: FMidiPattern.QuantizeValue := 100 / 32;
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

{ TSelectObjectListCommand }

procedure TSelectObjectListCommand.DoExecute;
var
  lIndex: Integer;
  lMidinote: TMidiNote;
  lMementoNote: TMidiNote;
begin
  if not FAddMode then
  begin
    for lIndex := 0 to Pred(FMidiPattern.NoteList.Count) do
    begin
      lMidinote := TMidiNote(FMidiPattern.NoteList[lIndex]);

      if Assigned(lMidinote) then
      begin
        lMementoNote := TMidiNote.Create(ObjectOwner, NOT_MAPPED);
        lMementoNote.ObjectID := lMidinote.ObjectID;
        lMementoNote.Selected := lMidinote.Selected;
        Memento.Add(lMementoNote);

        lMidinote.Selected := False;
        lMidinote.Notify;
      end;
    end;
  end;

  for lIndex := 0 to Pred(ObjectIdList.Count) do
  begin
    lMidinote := FMidiPattern.NoteByObjectID(ObjectIdList[lIndex]);

    if Assigned(lMidinote) then
    begin
      lMidinote.Selected := True;
      lMidinote.Notify;
    end;
  end;
end;

procedure TSelectObjectListCommand.DoRollback;
var
  lMementoNote: TMidiNote;
  lMidinote: TMidiNote;
  lIndex: Integer;
begin
  for lIndex := 0 to Pred(Memento.Count) do
  begin
    lMementoNote := TMidiNote(Memento[lIndex]);
    lMidinote := FMidiPattern.NoteByObjectID(lMementoNote.ObjectID);

    if Assigned(lMidinote) then
    begin
      lMidinote.Selected := lMementoNote.Selected;
      lMidinote.Notify;
    end;
  end;
end;

{ TChangeMidiChannelCommand }

procedure TChangeMidiChannelCommand.DoExecute;
begin
  DBLog('start TChangeMidiChannelCommand.DoExecute');

  FMidiPattern.BeginUpdate;

  // Store MidiChannel
  FOldMidiChannel := FMidiChannel;
  FMidiPattern.MidiChannel := FMidiChannel;
  writeln(Format('Change midiChannel to %d',[FMidiPattern.MidiChannel]));

  FMidiPattern.EndUpdate;

  DBLog('end TChangeMidiChannelCommand.DoExecute');
end;

procedure TChangeMidiChannelCommand.DoRollback;
begin
  DBLog('start TChangeMidiChannelCommand.DoRollback');

  FMidiPattern.BeginUpdate;

  FMidiPattern.MidiChannel := FOldMidiChannel;
  writeln(Format('Change midiChannel to %d',[FMidiPattern.MidiChannel]));

  FMidiPattern.EndUpdate;

  DBLog('end TChangeMidiChannelCommand.DoRollback');
end;


{ TMidiCommand }

procedure TMidiCommand.Initialize;
begin
  FMidiPattern := TMidiPattern(GObjectMapper.GetModelObject(ObjectOwner));
end;

initialization

finalization
end.

