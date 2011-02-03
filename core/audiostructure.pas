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

  audiostructure.pas
}

unit audiostructure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, globalconst, ContNrs, sampler, track, global_command,
  utils, global, midi;

type
  TAudioStructure = class;

  { TModelThread }

  TModelThread = class(TThread)
  private
    procedure Updater;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean);
  end;

  { TAudioStructureCommand }

  TAudioStructureCommand = class(TCommand)
  private
    FAudioStructure: TAudioStructure;
  protected
    procedure Initialize; override;
  end;


  { TDeleteTrackCommand }

  TDeleteTrackCommand = class(TAudioStructureCommand)
  private
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
  end;

  { TCreateTrackCommand }

  TCreateTrackCommand = class(TAudioStructureCommand)
  private
    FSourceType: TFileSourceTypes;
    FSourceLocation: string;
    FPatternName: string;
    FPosition: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property SourceType: TFileSourceTypes read FSourceType write FSourceType;
    property SourceLocation: string read FSourceLocation write FSourceLocation;
    property PatternName: string read FPatternName write FPatternName;
    property Position: Integer read FPosition write FPosition;
  end;

  { TBPMChangeCommand }

  TBPMChangeCommand = class(TAudioStructureCommand)
  private
    FBPM: Single;
    FOldBPM: Single;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property BPM: Single read FBPM write FBPM;
  end;

  { TLoadSessionCommand }

  TLoadSessionCommand = class(TAudioStructureCommand)
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
  end;

  { TSaveSessionCommand }

  TSaveSessionCommand = class(TAudioStructureCommand)
  private
  protected
    procedure DoExecute; override;
  published
  end;

  { TAudioStructure }

  TAudioStructure = class(THybridPersistentModel)
  private
    FTrack: TObjectList;
    FSampler: TSampler;
    FActive: Boolean;
    FModelThread: TModelThread;

    FMainSyncCounter: Integer;
    FMainSyncModula: Integer;
    FMainQuantizeLength: Integer;
    FBPM: Single;
    FMainSampleRate: Single;
    FPlayState: Integer;
    FSelectedBank: TSampleBank;
    FOldSelectedBank: TSampleBank;

    procedure SetBPM(const AValue: Single);
    procedure RecalculateSynchronize;
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
    procedure SetSelectedBank(const AValue: TSampleBank);
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Initialize; override;
    function IndexOfTrack(AObjectID: string): Integer;

    property Active: Boolean read FActive write FActive;
    property Sampler: TSampler read FSampler write FSampler;
    property ModelThread: TModelThread read FModelThread write FModelThread;
    property SelectedBank: TSampleBank read FSelectedBank write SetSelectedBank;
    property OldSelectedBank: TSampleBank read FOldSelectedBank write FOldSelectedBank;
  published
    property Tracks: TObjectList read FTrack write FTrack;
    property MainSyncCounter: Integer read FMainSyncCounter write FMainSyncCounter;
    property MainSyncModula: Integer read FMainSyncModula write FMainSyncModula;
    property BPM: Single read FBPM write SetBPM;
    property PlayState: Integer read FPlayState write FPlayState;
    property MainSampleRate: Single read FMainSampleRate write FMainSampleRate;
    property MainQuantizeLength: Integer read FMainQuantizeLength write FMainQuantizeLength;
  end;

var
  GAudioStruct: TAudioStructure;

implementation

uses
  SimpleJack, sndfile, Pattern, DOM, XMLWrite, XMLRead;

{ TAudioStructure }

constructor TAudioStructure.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  inherited Create(AObjectOwner, AMapped);

  FTrack := TObjectList.create(True);
  FSampler := TSampler.Create(ObjectID, MAPPED);

  FSelectedBank := nil;
  FOldSelectedBank := nil;
//  FModelThread := TModelThread.Create(False);

  FOnCreateInstanceCallback := @DoCreateInstance;

  FMainQuantizeLength := 8; // Syncronize at a 1 bar quantization
  FMainSyncCounter := 0;
  FBPM := 120;
end;

destructor TAudioStructure.Destroy;
begin
//  FModelThread.Terminate;
  FTrack.Free;
  FSampler.Free;

  inherited Destroy;
end;

procedure TAudioStructure.Initialize;
begin
  Notify;
end;

function TAudioStructure.IndexOfTrack(AObjectID: string): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Pred(Tracks.Count) do
  begin
    if TWaveFormTrack(Tracks[i]).ObjectID = AObjectID then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TAudioStructure.SetBPM(const AValue: Single);
begin
  FBPM := AValue;
  if FBPM < 1 then FBPM := 1;

  RecalculateSynchronize;
end;

procedure TAudioStructure.RecalculateSynchronize;
begin
  FMainSyncModula := Round(MainSampleRate * (60 / FBPM)) * FMainQuantizeLength;
end;

procedure TAudioStructure.DoCreateInstance(var AObject: TObject; AClassName: string);
var
  lTrack: TWaveFormTrack;
begin
  DBLog('start TAudioStructure.DoCreateInstance');

  lTrack := TWaveFormTrack.Create(GAudioStruct.ObjectID, MAPPED);
  lTrack.Selected:= True;
  lTrack.Level:= 0;

  Tracks.Add(lTrack);

  AObject := lTrack;

  DBLog('end TAudioStructure.DoCreateInstance');
end;

procedure TAudioStructure.SetSelectedBank(const AValue: TSampleBank);
begin
  FOldSelectedBank := FSelectedBank;
  FSelectedBank := AValue;
end;

{ TModelThread }

procedure TModelThread.Updater;
begin
  // execute commands on audio thread
  if GCommandQueue.CommandQueue.Count > 0 then
  begin;
    //DBLog('start GCommandQueue.ExecuteCommandQueue');
//    GCommandQueue.ExecuteCommandQueue;
    //DBLog('end GCommandQueue.ExecuteCommandQueue');
  end;
end;

procedure TModelThread.Execute;
begin
  while (not Terminated) do
  begin
    // Only update at 1000 ms / 10 ms = about 100 fps
    Sleep(10);
    Synchronize(@Updater);
  end;
end;

constructor TModelThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

{ TDeleteTrackCommand }

procedure TDeleteTrackCommand.DoExecute;
var
  i, j: Integer;
  lMemento: TWaveFormTrack;
  lModelObject: TWaveFormTrack;
begin
  DBLog('start TDeleteTrackCommand.DoExecute');

  FAudioStructure.BeginUpdate;

  // Actually delete track(s)
  for i := 0 to Pred(ObjectIdList.Count) do
  begin
    for j := 0 to Pred(GAudioStruct.Tracks.Count) do
    begin
      if TWaveFormTrack(GAudioStruct.Tracks[j]).ObjectID = ObjectIdList[i] then
      begin
        lModelObject := TWaveFormTrack(GAudioStruct.Tracks[j]);
        lMemento := TWaveFormTrack.Create(lModelObject.ObjectID, False);
        lMemento.ObjectID := ObjectIdList[i];

        { Make ASSIGN work! }
        //lMemento.Assign(TWaveFormTrack(GAudioStruct.Tracks[j]));
        Memento.Add(lMemento);
        GAudioStruct.Tracks.Remove(lModelObject);

        break;
      end;
    end;
  end;

  FAudioStructure.EndUpdate;

  DBLog('end TDeleteTrackCommand.DoExecute');
end;

procedure TDeleteTrackCommand.DoRollback;
var
  lTrack: TWaveFormTrack;
  i: Integer;
begin
  DBLog('start TDeleteTrackCommand.DoRollback');

  FAudioStructure.BeginUpdate;

  for i := 0 to Pred(Memento.Count) do
  begin
    lTrack := TWaveFormTrack.Create(GAudioStruct.ObjectID);
    lTrack.ObjectID := TWaveFormTrack(Memento[i]).ObjectID;

    { TODO Make Assign work! }
    //lTrack.Assign(TWaveFormTrack(Memento[i]));
    GAudioStruct.Tracks.Add(lTrack);

    GObjectMapper.AddMapping(lTrack);
  end;

  FAudioStructure.EndUpdate;

  DBLog('end TDeleteTrackCommand.DoRollback');
end;


{ TCreateTrackCommand }

procedure TCreateTrackCommand.DoExecute;
var
  lTrack: TWaveFormTrack;
  i: Integer;
  lPattern: TPattern;
begin
  DBLog('start TCreateTrackCommand.DoExecute');

  FAudioStructure.BeginUpdate;

  lTrack := TWaveFormTrack.Create(GAudioStruct.ObjectID, MAPPED);
  lTrack.Selected:= True;
  lTrack.Level:= 0;

  GAudioStruct.Tracks.Add(lTrack);

  ObjectIdList.Add(lTrack.ObjectID);

  for i := 0 to Pred(GAudioStruct.Tracks.Count) do
  begin
    TWaveFormTrack(GAudioStruct.Tracks[i]).Selected := (lTrack = TWaveFormTrack(GAudioStruct.Tracks[i]));
  end;

  // Store action about creating a track
  FAudioStructure.EndUpdate;

  case SourceType of
    fsWave:
    begin
      lTrack.BeginUpdate;

      lPattern := TPattern.Create(lTrack.ObjectID);
      lPattern.PatternName := PatternName;
      lPattern.WaveForm.SampleFileName := SourceLocation;
      lPattern.Position := Position;
      lTrack.PatternList.Add(lPattern);
      lTrack.SelectedPattern := lPattern;

      if FileExists(SourceLocation) then
      begin
        lPattern.Initialize;
        lPattern.OkToPlay := True;
      end;
      lTrack.EndUpdate;
      lPattern.Notify;
      lPattern.WaveForm.Notify;
      lPattern.MidiGrid.Notify;
    end;
    fsMIDI:
    begin

    end;
    fsPattern:
    begin

    end;
    fsPlugin:
    begin

    end;
  end;

  DBLog('end TCreateTrackCommand.DoExecute');
end;

procedure TCreateTrackCommand.DoRollback;
var
  i: Integer;
  lTrackIndex: Integer;
begin
  DBLog('start TCreateTrackCommand.DoRollback');

  FAudioStructure.BeginUpdate;

  for i := 0 to Pred(ObjectIdList.Count) do
  begin
    lTrackIndex := GAudioStruct.IndexOfTrack(ObjectIdList[i]);
    if lTrackIndex <> -1 then
    begin

      GAudioStruct.Tracks.Remove(GAudioStruct.Tracks[lTrackIndex]);
    end;
  end;

  FAudioStructure.EndUpdate;

  DBLog('end TCreateTrackCommand.DoRollback');
end;

{ TBPMChangeCommand }

procedure TBPMChangeCommand.DoExecute;
begin
  if Persist then
  begin
    FAudioStructure.BeginUpdate;

    FOldBPM := GAudioStruct.BPM;

    FAudioStructure.EndUpdate;
  end;

  GAudioStruct.BPM := FBPM;
end;

procedure TBPMChangeCommand.DoRollback;
begin
  FAudioStructure.BeginUpdate;

  GAudioStruct.BPM := FOldBPM;

  FAudioStructure.EndUpdate;
end;

{ TLoadSessionCommand }

procedure TLoadSessionCommand.DoExecute;
var
  i: Integer;
  xdoc: TXMLDocument;
  RootNode: TDOMNode;
begin
  FAudioStructure.BeginUpdate;

  for i := Pred(GAudioStruct.Tracks.Count) downto 0 do
  begin
    GAudioStruct.Tracks.Remove(GAudioStruct.Tracks[i]);
  end;
  ReadXMLFile(xDoc, 'teste.xml');
  RootNode := xDoc.DocumentElement.FirstChild;
  if RootNode <> nil then
  begin
    GAudioStruct.LoadFromXML(RootNode);
    GAudioStruct.RecurseNotify(GAudioStruct);
  end;

  FAudioStructure.EndUpdate;
end;

procedure TLoadSessionCommand.DoRollback;
var
  i: Integer;
begin
  FAudioStructure.BeginUpdate;

  for i := Pred(GAudioStruct.Tracks.Count) downto 0 do
  begin
    GAudioStruct.Tracks.Remove(GAudioStruct.Tracks[i]);
  end;

  FAudioStructure.EndUpdate;
end;

{ TSaveSessionCommand }

procedure TSaveSessionCommand.DoExecute;
var
  xdoc: TXMLDocument;
  RootNode: TDOMNode;
begin
  FAudioStructure.BeginUpdate;

  //create a document
  xdoc := TXMLDocument.create;

  //create a root node
  RootNode := xdoc.CreateElement('root');
  Xdoc.Appendchild(RootNode);

  //create a tracks node
  RootNode:= xdoc.DocumentElement;

  // pass xmldocument to iterate function
  // pass mode to iterate (Store,Retrieve) ie. Save to xml or load from xml
  // load from xml should create instance of property objects
  GAudioStruct.SaveToXML(GAudioStruct, 0, RootNode);

  // write to XML
  writeXMLFile(xDoc, 'teste.xml');
  // free memory
  Xdoc.free;

  FAudioStructure.EndUpdate;
end;


{ TAudioStructureCommand }

procedure TAudioStructureCommand.Initialize;
begin
  FAudioStructure := GAudioStruct;
end;

initialization

finalization

end.

