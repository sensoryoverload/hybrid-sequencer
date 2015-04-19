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
 globalconst, contnrs, global_command, ShellCtrls,
 global, math, pattern, sampler, pluginportmapper, plugin;

const
  MAX_LATENCY = 20000;

type
  { Forward declarations }
  TMidiPattern = class;

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
    FMapped: Boolean;
    function GetNote: Integer;
    procedure SetNote(const AValue: Integer);
    procedure SetNoteLength(const AValue: Integer);
    procedure SetNoteLocation(const AValue: Integer);
    procedure SetNoteVelocity(const AValue: Integer);
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean); reintroduce;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Assign(Source: TPersistent); override;
    property MidiNoteStart: TMidiData read FMidiNoteStart write FMidiNoteStart;
    property MidiNoteEnd: TMidiData read FMidiNoteEnd write FMidiNoteEnd;
    property Mapped: Boolean read FMapped;
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

  TNoteRecord = record
    Mute: Boolean; { mute }
    Solo: Boolean;  { solo }
    MidiChannel: Integer; { -1 = use pattern midichannel, 0..15 = custom midichannel }
    MidiNote: Integer; { midinotevalue out }
  end;

  TMidiPattern = class(TPattern)
  private
    // Data
    FMidiDataList: TMidiDataList;
    FNoteList: TObjectList;
    FControllerList: TObjectList;
    FQuantizeSetting: Integer;
    FQuantizeValue: Single;
    FEnabled: Boolean;
    FNoteRecordList: Array[0..127] of TNoteRecord;

    // Engine
    FMidiDataCursor: TMidiData;
    FLooped: Boolean;

    FMidiBuffer: TMidiBuffer;

    function GetEnabled: Boolean;
    procedure SetQuantizeSetting(AValue: Integer);
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string); override;
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Assign(Source: TPersistent); override;
    function QuantizeLocation(ALocation: Integer): Integer;
    function NoteByObjectID(AObjectID: string): TMidiNote;

    procedure ProcessInit; override;
    procedure Process(ABuffer: PSingle; AFrameIndex: Integer; AFrameCount: Integer); override;
    procedure ProcessAdvance; override;

    function Latency: Integer; reintroduce;
    procedure Flush; override;

    property Enabled: Boolean read GetEnabled write FEnabled default True;
    property MidiDataCursor: TMidiData read FMidiDataCursor write FMidiDataCursor;
    property MidiDataList: TMidiDataList read FMidiDataList write FMidiDataList;

    {
      Engine
    }
    property MidiBuffer: TMidiBuffer read FMidiBuffer write FMidiBuffer;
  published
    property NoteList: TObjectList read FNoteList write FNoteList;
    property ControllerList: TObjectList read FControllerList write FControllerList;
    property QuantizeSetting: Integer read FQuantizeSetting write SetQuantizeSetting default 1;
    property QuantizeValue: Single read FQuantizeValue write FQuantizeValue default 1;
  end;

  TMidiGridEngine = class
  private
  public
  end;


implementation

uses Fx, audiostructure;

constructor TMidiPattern.Create(AObjectOwner: string; AMapped: Boolean = True);
var
  lIndex: Integer;
begin
  DBLog('start TMidiGrid.Create');

  inherited Create(AObjectOwner, AMapped);

  FOnCreateInstanceCallback := @DoCreateInstance;

  for lIndex := Low(FNoteRecordList) to High(FNoteRecordList) do
  begin
    FNoteRecordList[lIndex].Mute := False;
    FNoteRecordList[lIndex].Solo := False;
    FNoteRecordList[lIndex].MidiChannel := -1;
    FNoteRecordList[lIndex].MidiNote := lIndex;
  end;

  FNoteList := TObjectList.Create(True);
  FControllerList := TObjectList.Create(True);
  FMidiDataList := TMidiDataList.Create;

  FMidiBuffer := TMidiBuffer.Create;

  QuantizeSetting := 3;

  LoopStart.Value := 0;
  LoopLength.Value := Round(GSettings.SampleRate * 2);
  LoopEnd.Value := LoopStart.Value + LoopLength.Value;
  FLooped := False;

  DBLog('end TMidiGrid.Create');
end;

destructor TMidiPattern.Destroy;
begin
  if Assigned(FMidiBuffer) then
    FMidiBuffer.Free;

  if Assigned(FMidiDataList) then
    FMidiDataList.Free;

  if Assigned(FNoteList) then
    FNoteList.Free;

  if Assigned(FControllerList) then
    FControllerList.Free;

  inherited Destroy;
end;

procedure TMidiPattern.Initialize;
{var
  lIndex: Integer;
  lIndex2: Integer;
  lAutomationDataList: TAutomationDataList; }
begin
  BeginUpdate;

  Inherited Initialize;

  FMidiDataList.IndexList;

  (*DBLog(Format('AutomationChannelList Count %d', [AutomationChannelList.Count]));

  for lIndex := 0 to Pred(AutomationChannelList.Count) do
  begin
    lAutomationDataList := TAutomationDataList(AutomationChannelList[lIndex]);

    DBLog(Format('AutomationDataList Count %d', [lAutomationDataList.List.Count]));

    for lIndex2 := 0 to Pred(lAutomationDataList.List.Count) do
    begin
      DBLog(Format('AutomationData DataValue %f Location %d', [
        TAutomationData(lAutomationDataList.List[lIndex2]).DataValue,
        TAutomationData(lAutomationDataList.List[lIndex2]).Location]));
    end;
  end;*)

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
    Result := Round(Round(ALocation / FQuantizeValue) * FQuantizeValue);

    DBLog(Format('Floor(ALocation / FQuantizeValue) %d', [Trunc(ALocation / FQuantizeValue)]));
  end;
end;

procedure TMidiPattern.ProcessInit;
begin
  // Reset buffer at beginning of callback
  FMidiBuffer.Reset;
end;

{
  Process advances cursor
}
procedure TMidiPattern.Process(ABuffer: PSingle; AFrameIndex: Integer; AFrameCount: Integer);
var
  i: Integer;

begin
  {
    In the first frame this first midinote of the window has to be found.
    later notes within this window can be found be walking the list
  }
  if Looped then
  begin
    Looped := False;
    {
      Look for first midi event in the window if any
    }
    for i := 0 to Pred(MidiDataList.Count) do
    begin
      if (TMidiData(MidiDataList[i]).Location >= LoopStart.Value) then
      begin
        MidiDataCursor := TMidiData(MidiDataList[i]);
        break;
      end;
    end;
  end;

  {
    Walk the list and push midi events to the midibuffer
  }
  while Assigned(MidiDataCursor) and (MidiDataCursor.Location < PatternCursor)  do
  begin
    {
      Put event in buffer
    }
    if MidiDataCursor.Location >= LoopStart.Value then
    begin
      MidiBuffer.WriteEvent(MidiDataCursor, AFrameIndex);
      DBLog(Format('MidiDataCursor %d Location %d cursor %f buffersize %d ptr %d', [AFrameIndex, MidiDataCursor.Location, PatternCursor, MidiDataList.Count, Integer(MidiDataCursor)]));
    end;

    MidiDataCursor := MidiDataCursor.Next;
  end;
end;

procedure TMidiPattern.ProcessAdvance;
begin
  inherited;
end;

function TMidiPattern.Latency: Integer;
begin
  Result := PluginProcessor.Latency;
end;

procedure TMidiPattern.Flush;
begin
  FMidiBuffer.Reset;
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

procedure TMidiPattern.SetQuantizeSetting(AValue: Integer);
var
  lBeat: Single;
begin
  if FQuantizeSetting = AValue then Exit;
  FQuantizeSetting := AValue;

  lBeat := (GSettings.SampleRate / 2);

  case FQuantizeSetting of
  0: FQuantizeValue := -1;
  1: FQuantizeValue := lBeat * 4;
  2: FQuantizeValue := lBeat * 2;
  3: FQuantizeValue := lBeat;
  4: FQuantizeValue := lBeat / 2;
  5: FQuantizeValue := lBeat / 3;
  6: FQuantizeValue := lBeat / 4;
  7: FQuantizeValue := lBeat / 6;
  8: FQuantizeValue := lBeat / 8;
  9: FQuantizeValue := lBeat / 16;
  10: FQuantizeValue := lBeat / 32;
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

  Inherited DoCreateInstance(AObject, AClassName);

  if AClassName = 'TMidiNote' then
  begin
    // create model Marker
    lMidiNote := TMidiNote.Create(ObjectID, MAPPED);
    lMidiNote.ObjectOwnerID := ObjectID;

    AObject := lMidiNote;

    FNoteList.Add(lMidiNote);

    FMidiDataList.Add(lMidiNote.MidiNoteStart);
    FMidiDataList.Add(lMidiNote.MidiNoteEnd);
  end
  else if AClassName = 'TControllerEvent' then
  begin
    // create model Marker
    lMidiNote := TMidiNote.Create(ObjectID, MAPPED);
    lMidiNote.ObjectOwnerID := ObjectID;

    AObject := lMidiNote;

    FNoteList.Add(lMidiNote);

    FMidiDataList.Add(lMidiNote.MidiNoteStart);
    FMidiDataList.Add(lMidiNote.MidiNoteEnd);
  end;

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

  if FMapped then
  begin
    FMidiNoteStart.DataType := mtNoteOn;
    FMidiNoteStart.DataValue1 := FNote;
    FMidiNoteStart.DataValue2 := 127;
    FMidiNoteEnd.DataType := mtNoteOff;
    FMidiNoteEnd.DataValue1 := FNote;
    FMidiNoteEnd.DataValue2 := 0;
  end;
end;

function TMidiNote.GetNote: Integer;
begin
  Result := FNote;
end;

procedure TMidiNote.SetNoteLength(const AValue: Integer);
begin
  FNoteLength:= AValue;

  if FMapped then
  begin
    FMidiNoteEnd.Location := FNoteLocation + FNoteLength;
    FMidiNoteStart.Length := FMidiNoteEnd.Location - FMidiNoteStart.Location;
  end;
end;

procedure TMidiNote.SetNoteLocation(const AValue: Integer);
begin
  FNoteLocation:= AValue;

  if FMapped then
  begin
    FMidiNoteEnd.Location := FNoteLocation + FNoteLength;

    FMidiNoteStart.Location := FNoteLocation;
    FMidiNoteStart.Length := FMidiNoteEnd.Location - FMidiNoteStart.Location;
  end;
end;

procedure TMidiNote.SetNoteVelocity(const AValue: Integer);
begin
  FNoteVelocity := AValue;

  if FMapped then
  begin
    DBLog('FMidiNoteStart.DataValue2: ' + IntToStr(FNoteVelocity));
    FMidiNoteStart.DataValue2 := FNoteVelocity;
  end;
end;

constructor TMidiNote.Create(AObjectOwner: string; AMapped: Boolean);
begin
  inherited Create(AObjectOwner, AMapped);

  FMapped := AMapped;

  if FMapped then
  begin
    FMidiNoteStart := TMidiData.Create(Self);
    FMidiNoteStart.MidiChannel := 1;
    FMidiNoteEnd := TMidiData.Create(Self);
    FMidiNoteEnd.MidiChannel := 1;
  end;

  FSelected:= False;
end;

destructor TMidiNote.Destroy;
begin
  if FMapped then
  begin
    FMidiNoteStart.Free;
    FMidiNoteEnd.Free;
  end;

  inherited Destroy;
end;

procedure TMidiNote.Initialize;
begin
  //
end;

procedure TMidiNote.Finalize;
begin
  //
end;

procedure TMidiNote.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;


initialization

finalization
end.

