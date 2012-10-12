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
  utils, global, midi, midicontrolmap, fileutil;

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
  public
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

  { TClearSessionCommand }

  TClearSessionCommand = class(TAudioStructureCommand)
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
  end;

  { TAudioStructure }

  TAudioStructure = class(THybridPersistentModel)
  private
    FTrack: TTrackList;
    FActive: Boolean;
    //FModelThread: TModelThread;
    FMainSyncCounter: Single; // 1 bar loop
    FMainSyncLength: Single; // 1 bar length
    FMainSyncSignal: Boolean;
    FMainQuantizeLength: Integer;

    FMainTimeLine: Single;
    FBPM: Single;
    FBPMScale: Single;
    FBPMScaleInv: Single;
    FMainSampleRate: Single;
    FPlayState: Integer;
    FSelectedBank: TSampleBank;
    FOldSelectedBank: TSampleBank;
    FMasterTrack: TTrack;
    FSync: Boolean;

    procedure SetBPM(const AValue: Single);
    procedure RecalculateSynchronize;
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
    procedure SetSelectedBank(const AValue: TSampleBank);
  public
    MainCounter: Double;

    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure ProcessAdvance;
    function Sync: Boolean;
    function IndexOfTrack(AObjectID: string): Integer;

    property Active: Boolean read FActive write FActive;
    //property ModelThread: TModelThread read FModelThread write FModelThread;
    property SelectedBank: TSampleBank read FSelectedBank write SetSelectedBank;
    property OldSelectedBank: TSampleBank read FOldSelectedBank write FOldSelectedBank;
  published
    property Tracks: TTrackList read FTrack write FTrack;
    property MainSyncCounter: Single read FMainSyncCounter write FMainSyncCounter;
    property MainSyncLength: Single read FMainSyncLength write FMainSyncLength;
    property MainSyncSignal: Boolean read FMainSyncSignal write FMainSyncSignal;
    property BPM: Single read FBPM write SetBPM;
    property BPMScale: Single read FBPMScale;
    property BPMScaleInv: Single read FBPMScaleInv;
    property PlayState: Integer read FPlayState write FPlayState;
    property MainSampleRate: Single read FMainSampleRate write FMainSampleRate;
    property MainQuantizeLength: Integer read FMainQuantizeLength write FMainQuantizeLength;
  end;

var
  GAudioStruct: TAudioStructure;

implementation

uses
  SimpleJack, sndfile, Wave, DOM, XMLWrite, XMLRead;

{ TClearSessionCommand }

procedure TClearSessionCommand.DoExecute;
var
  lTrackIndex: Integer;
begin
  FAudioStructure.BeginUpdate;

  for lTrackIndex := Pred(GAudioStruct.Tracks.Count) downto 0 do
  begin
    if GAudioStruct.Tracks[lTrackIndex].TrackType <> ttMaster then
    begin
      GAudioStruct.Tracks.Remove(GAudioStruct.Tracks[lTrackIndex]);
    end;
  end;

  FAudioStructure.EndUpdate;
end;

procedure TClearSessionCommand.DoRollback;
begin
  //
end;

{ TAudioStructure }

constructor TAudioStructure.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  inherited Create(AObjectOwner, AMapped);

  FTrack := TTrackList.create(True);

  FSelectedBank := nil;
  FOldSelectedBank := nil;

  FOnCreateInstanceCallback := @DoCreateInstance;

  FMainQuantizeLength := 44100 * 2; // Syncronize at a 1 bar quantization
  FMainTimeLine := 10000; // Default 1 bar (4 beat) length
  FMainSyncCounter := 0;
  MainCounter := 0; // Should be longer than 1 bar
  MainSyncSignal := True;
  FBPM := 120;
  FSync := True;
end;

destructor TAudioStructure.Destroy;
begin
  FTrack.Free;

  inherited Destroy;
end;

procedure TAudioStructure.Initialize;
begin
  FMasterTrack := TTrack.Create(GAudioStruct.ObjectID, MAPPED);
  FMasterTrack.TrackType := ttMaster;

  GAudioStruct.Tracks.Add(FMasterTrack);

  Notify;
end;

procedure TAudioStructure.Finalize;
begin
  //
end;

procedure TAudioStructure.ProcessAdvance;
begin
  MainCounter += FBPMScale;
  if MainCounter >= MainQuantizeLength then
  begin
    FSync := True;
    MainCounter -= MainQuantizeLength;
  end
  else
  begin
    FSync := False;
  end;
end;

function TAudioStructure.Sync: Boolean;
begin
  Result := FSync;
end;

function TAudioStructure.IndexOfTrack(AObjectID: string): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Pred(Tracks.Count) do
  begin
    if TTrack(Tracks[i]).ObjectID = AObjectID then
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

  FBPMScale := FBPM * DIVIDE_BY_120_MULTIPLIER;
  FBPMScaleInv := 1 / FBPMScale;

  RecalculateSynchronize;
end;

procedure TAudioStructure.RecalculateSynchronize;
begin
  FMainSyncLength := GSettings.SampleRate * 2;
end;

procedure TAudioStructure.DoCreateInstance(var AObject: TObject; AClassName: string);
var
  lTrack: TTrack;
begin
  DBLog('start TAudioStructure.DoCreateInstance');

  lTrack := TTrack.Create(GAudioStruct.ObjectID, MAPPED);
  lTrack.Selected:= True;
  lTrack.LeftLevel:= 0;
  lTrack.RightLevel:= 0;

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
    GCommandQueue.ExecuteCommandQueue;
    //DBLog('end GCommandQueue.ExecuteCommandQueue');
  end;
end;

procedure TModelThread.Execute;
begin
  while (not Terminated) do
  begin
    // Only update at 1000 ms / 40 ms = about 25 fps
    Sleep(40);
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
  lModelObject: TTrack;
begin
  DBLog('start TDeleteTrackCommand.DoExecute');

  FAudioStructure.BeginUpdate;

  // Actually delete track(s)
  for i := 0 to Pred(ObjectIdList.Count) do
  begin
    for j := 0 to Pred(GAudioStruct.Tracks.Count) do
    begin
      lModelObject := TTrack(GAudioStruct.Tracks[j]);

      if lModelObject.ObjectID = ObjectIdList[i] then
      begin
        lModelObject.Active := False;
        lModelObject.Playing := False;

        // Remove from live objectlist
        GAudioStruct.Tracks.Extract(lModelObject);

        GObjectMapper.DeleteMapping(lModelObject.ObjectID);

        Memento.Add(lModelObject);

        break;
      end;
    end;
  end;

  FAudioStructure.EndUpdate;

  DBLog('end TDeleteTrackCommand.DoExecute');
end;

procedure TDeleteTrackCommand.DoRollback;
var
  lTrack: TTrack;
  i: Integer;
begin
  DBLog('start TDeleteTrackCommand.DoRollback');

  FAudioStructure.BeginUpdate;

  for i := 0 to Pred(Memento.Count) do
  begin
    lTrack := TTrack(Memento[i]);

    GObjectMapper.AddMapping(lTrack);
    GAudioStruct.Tracks.Add(lTrack);
  end;

  FAudioStructure.EndUpdate;

  DBLog('end TDeleteTrackCommand.DoRollback');
end;


{ TCreateTrackCommand }

procedure TCreateTrackCommand.DoExecute;
var
  lTrack: TTrack;
  i: Integer;
  lWavePattern: TWavePattern;
  lMidiPattern: TMidiPattern;
begin
  DBLog('start TCreateTrackCommand.DoExecute');

  FAudioStructure.BeginUpdate;

  lTrack := TTrack.Create(GAudioStruct.ObjectID, MAPPED);
  lTrack.Selected:= True;
  lTrack.LeftLevel:= 0;
  lTrack.RightLevel:= 0;

  GAudioStruct.Tracks.Add(lTrack);

  ObjectIdList.Add(lTrack.ObjectID);

  for i := 0 to Pred(GAudioStruct.Tracks.Count) do
  begin
    TTrack(GAudioStruct.Tracks[i]).Selected := (lTrack = TTrack(GAudioStruct.Tracks[i]));
  end;

  // Store action about creating a track
  FAudioStructure.EndUpdate;

  case SourceType of
    fsWave:
    begin
      lTrack.BeginUpdate;

      lWavePattern := TWavePattern.Create(lTrack.ObjectID);
      lWavePattern.Text := ExtractFileNameWithoutExt(PatternName);
      lWavePattern.PatternName := PatternName;
      lWavePattern.WaveFileName := SourceLocation;
      lWavePattern.Position := Position;
      lTrack.PatternList.Add(lWavePattern);
      lTrack.SelectedPattern := lWavePattern;

      if FileExists(SourceLocation) then
      begin
        lWavePattern.Initialize;
      end;
      lTrack.EndUpdate;
      lWavePattern.Notify;
    end;
    fsMIDI:
    begin
      lTrack.BeginUpdate;

      lMidiPattern := TMidiPattern.Create(lTrack.ObjectID);
      lMidiPattern.Text := ExtractFileNameWithoutExt(PatternName);
      lMidiPattern.PatternName := PatternName;
      lMidiPattern.Position := Position;
      lTrack.PatternList.Add(lMidiPattern);
      lTrack.SelectedPattern := lMidiPattern;

      //Nothing to load here..
      {if FileExists(SourceLocation) then
      begin}
        lMidiPattern.Initialize;
      {end;}
      lTrack.EndUpdate;
      lWavePattern.Notify;
    end;
    fsEmpty:
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
  lTrack: TTrack;
begin
  DBLog('start TCreateTrackCommand.DoRollback');

  FAudioStructure.BeginUpdate;

  for i := 0 to Pred(ObjectIdList.Count) do
  begin
    lTrackIndex := GAudioStruct.IndexOfTrack(ObjectIdList[i]);
    if lTrackIndex <> -1 then
    begin
      lTrack := TTrack(GAudioStruct.Tracks[lTrackIndex]);
      lTrack.Active := False;
      lTrack.Playing := False;

      GAudioStruct.Tracks.Remove(lTrack);
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

  FAudioStructure.EndUpdate;

  FAudioStructure.BeginUpdate;

  ReadXMLFile(xDoc, 'teste.xml');
  try
    RootNode := xDoc.DocumentElement.FirstChild;
    if RootNode <> nil then
    begin
      GAudioStruct.LoadFromXML(RootNode);
      GAudioStruct.RecurseNotify(GAudioStruct);
    end;
  finally
    xdoc.Free;
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

