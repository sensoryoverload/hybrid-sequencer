unit patternview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, ComCtrls, StdCtrls,
  global, globalconst, midi, wave, wavepatterncontrolgui, midipatterncontrolgui,
  pluginhostgui, audiostructure, track, contnrs;

type

  TPatternView = class;

  { TTrackSettings }

  TTrackSettings = class(THybridPersistentView)
  private
    FPatternView: TPatternView;
  public
    constructor Create(AObjectOwner: string; TheOwner: TComponent);
    destructor Destroy; override;

    procedure Update(Subject: THybridPersistentModel); override;

    property PatternView: TPatternView read FPatternView write FPatternView;
  end;


  TTrackSettingsList = class(TObjectList)
  private
    function GetTrackSettings(AIndex: Integer): TTrackSettings;
    procedure SetTrackSettings(AIndex: Integer; const Value: TTrackSettings);
  public
    property Items[AIndex: Integer] : TTrackSettings read GetTrackSettings write SetTrackSettings; default;
    function Add(ATrackSettings: TTrackSettings): integer;
  end;

  { TPatternView }

  TPatternView = class(TFrame, IObserver)
    pcEditor: TPageControl;
    tsEffects: TTabSheet;
    tsPattern: TTabSheet;
    procedure pcEditorDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    FObjectOwnerID: string;
    FObjectID: string;
    FModel: THybridPersistentModel;
    FTracks: TTrackSettingsList;

    FWavePatternControlGUI: TWavePatternControlGUI;
    FMidiPatternControlGUI: TMidiPatternControlGUI;
    FPluginProcessorGUI: TPluginProcessorGUI;

    procedure CreateTrackGUI(AObjectID: string);
    procedure DeleteTrackGUI(AObjectID: string);
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string; virtual;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    procedure Update(Subject: THybridPersistentModel); virtual; reintroduce;
    function GetModel: THybridPersistentModel; virtual;
    procedure SetModel(AModel: THybridPersistentModel); virtual;
    procedure Connect;
    procedure Disconnect;

    procedure DoPatternRefreshEvent(TrackObject: TObject);
    procedure UpdateScreen;

    property Tracks: TTrackSettingsList read FTracks write FTracks;
  end;

implementation

uses
  utils;

{ TTrackSettings }

constructor TTrackSettings.Create(AObjectOwner: string; TheOwner: TComponent);
begin
  inherited Create(AObjectOwner);

  FPatternView := TPatternView(TheOwner);
end;

destructor TTrackSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TTrackSettings.Update(Subject: THybridPersistentModel);
begin
  if Subject is TTrack then
  begin
    // Blank page if not a valid pattern in memory anymore
    if PatternView.FMidiPatternControlGUI.MidiPatternGUI.Parent = PatternView.tsPattern then
    begin
      if not Assigned(GObjectMapper.GetModelObject(PatternView.FMidiPatternControlGUI.MidiPatternGUI.ObjectID)) then
      begin
        PatternView.tsPattern.TabVisible := False;
        GSettings.SelectedPattern := nil;
        GSettings.OldSelectedPattern := nil;
      end;
    end
    else if PatternView.FWavePatternControlGUI.WaveGUI.Parent = PatternView.tsPattern then
    begin
      if not Assigned(GObjectMapper.GetModelObject(PatternView.FWavePatternControlGUI.WaveGUI.ObjectID)) then
      begin
        PatternView.tsPattern.TabVisible := False;
        GSettings.SelectedPattern := nil;
        GSettings.OldSelectedPattern := nil;
      end;
    end;
  end
end;

procedure TPatternView.DoPatternRefreshEvent(TrackObject: TObject);
var
  lWavePattern: TWavePattern;
  lMidiPattern: TMidiPattern;
begin
  if not Assigned(TrackObject) then
  begin
    tsPattern.TabVisible := False;
    tsEffects.TabVisible := False;
    if Assigned(GSettings.OldSelectedPattern) then
    begin
      if GSettings.OldSelectedPattern is TWavePattern then
      begin
        lWavePattern := TWavePattern(GSettings.OldSelectedPattern);
        if Assigned(lWavePattern) then
        begin
          lWavePattern.Detach(FWavePatternControlGUI);
          FWavePatternControlGUI.Parent := nil;

          lWavePattern.PluginProcessor.Detach(FPluginProcessorGUI);
        end;
      end
      else if GSettings.OldSelectedPattern is TMidiPattern then
      begin
        lMidiPattern := TMidiPattern(GSettings.OldSelectedPattern);
        if Assigned(lMidiPattern) then
        begin
          lMidiPattern.Detach(FMidiPatternControlGUI);
          FMidiPatternControlGUI.Parent := nil;

          lMidiPattern.PluginProcessor.Detach(FPluginProcessorGUI);
        end;
      end;
    end;
    GSettings.OldSelectedPattern := nil;
    GSettings.SelectedPattern := nil;
  end
  else if TrackObject is TWavePattern then
  begin
    if GSettings.OldSelectedPattern <> GSettings.SelectedPattern then
    begin
      // Detach if old pattern is visible
      if Assigned(GSettings.OldSelectedPattern) then
      begin
        if GSettings.OldSelectedPattern is TMidiPattern then
        begin
          // Last selected pattern of different type;
          // - detach
          // - set parent to new pattern type
          lMidiPattern := TMidiPattern(GSettings.OldSelectedPattern);
          if Assigned(lMidiPattern) then
          begin
            lMidiPattern.Detach(FMidiPatternControlGUI);
            FMidiPatternControlGUI.Parent := nil;

            lMidiPattern.PluginProcessor.Detach(FPluginProcessorGUI);
          end;

          // Attach new pattern
          lWavePattern := TWavePattern(GSettings.SelectedPattern);
          if Assigned(lWavePattern) then
          begin
            lWavePattern.Attach(FWavePatternControlGUI);
            FWavePatternControlGUI.Parent := tsPattern;

            lWavePattern.PluginProcessor.Attach(FPluginProcessorGUI);
          end;
        end
        else if GSettings.OldSelectedPattern is TWavePattern then
        begin
          // Last selected pattern of same type; just detach
          lWavePattern := TWavePattern(GSettings.OldSelectedPattern);
          if Assigned(lWavePattern) then
          begin
            lWavePattern.Detach(FWavePatternControlGUI);
            lWavePattern.PluginProcessor.Detach(FPluginProcessorGUI);
          end;

          // Attach new pattern
          lWavePattern := TWavePattern(GSettings.SelectedPattern);
          if Assigned(lWavePattern) then
          begin
            lWavePattern.Attach(FWavePatternControlGUI);
            lWavePattern.PluginProcessor.Attach(FPluginProcessorGUI);
          end;
        end
        else
        begin
          // unknown pattern
          tsPattern.TabVisible := False;
          tsEffects.TabVisible := False;
        end;
      end
      else
      begin
        if Assigned(GSettings.SelectedPattern) then
        begin
          tsPattern.TabVisible := True;
          tsEffects.TabVisible := True;

          // Attach new pattern
          lWavePattern := TWavePattern(GSettings.SelectedPattern);
          if Assigned(lWavePattern) then
          begin
            lWavePattern.Attach(FWavePatternControlGUI);
            FWavePatternControlGUI.Parent := tsPattern;
            lWavePattern.PluginProcessor.Attach(FPluginProcessorGUI);
          end;
        end
        else
        begin
          tsPattern.TabVisible := False;
          tsEffects.TabVisible := False;
        end;
      end;

      GSettings.OldSelectedPattern := GSettings.SelectedPattern;
    end;
  end
  else if TrackObject is TMidiPattern then
  begin
    if GSettings.OldSelectedPattern <> GSettings.SelectedPattern then
    begin
      // Detach if old pattern is visible
      if Assigned(GSettings.OldSelectedPattern) then
      begin
        if GSettings.OldSelectedPattern is TWavePattern then
        begin
          // Last selected pattern of different type;
          // - detach
          // - set parent to new pattern type
          lWavePattern := TWavePattern(GSettings.OldSelectedPattern);
          if Assigned(lWavePattern) then
          begin
            lWavePattern.Detach(FWavePatternControlGUI);
            FWavePatternControlGUI.Parent := nil;

            lWavePattern.PluginProcessor.Detach(FPluginProcessorGUI);
          end;

          lMidiPattern := TMidiPattern(GSettings.SelectedPattern);
          if Assigned(lMidiPattern) then
          begin
            lMidiPattern.Attach(FMidiPatternControlGUI);
            FMidiPatternControlGUI.Parent := tsPattern;
            lMidiPattern.PluginProcessor.Attach(FPluginProcessorGUI);
          end;

          tsPattern.TabVisible := True;
          tsEffects.TabVisible := True;
        end
        else if GSettings.OldSelectedPattern is TMidiPattern then
        begin
          // Last selected pattern of same type; just detach
          lMidiPattern := TMidiPattern(GSettings.OldSelectedPattern);
          if Assigned(lMidiPattern) then
          begin
            lMidiPattern.Detach(FMidiPatternControlGUI);
            FMidiPatternControlGUI.Parent := nil;

            lMidiPattern.PluginProcessor.Detach(FPluginProcessorGUI);
          end;

          lMidiPattern := TMidiPattern(GSettings.SelectedPattern);
          if Assigned(lMidiPattern) then
          begin
            lMidiPattern.Attach(FMidiPatternControlGUI);
            FMidiPatternControlGUI.Parent := tsPattern;
            lMidiPattern.PluginProcessor.Attach(FPluginProcessorGUI);
          end;
        end
        else
        begin
          // unknown pattern
          tsPattern.TabVisible := False;
          tsEffects.TabVisible := False;
        end;
      end
      else
      begin
        if Assigned(GSettings.SelectedPattern) then
        begin
          lMidiPattern := TMidiPattern(GSettings.SelectedPattern);
          if Assigned(lMidiPattern) then
          begin
            lMidiPattern.Attach(FMidiPatternControlGUI);
            FMidiPatternControlGUI.Parent := tsPattern;
            lMidiPattern.PluginProcessor.Attach(FPluginProcessorGUI);
          end;

          tsPattern.TabVisible := True;
          tsEffects.TabVisible := True;
        end
        else
        begin
          // Should hide
        end;
      end;

      GSettings.OldSelectedPattern := GSettings.SelectedPattern;
    end;
  end;
end;

procedure TPatternView.UpdateScreen;
begin
  // Update patterneditor grid
  if FMidiPatternControlGUI.Parent = tsPattern then
  begin
    if not Assigned(GObjectMapper.GetModelObject(FMidiPatternControlGUI.MidiPatternGUI.ObjectID)) then
    begin
      tsPattern.TabVisible := False;
    end
    else
    begin
      FMidiPatternControlGUI.MidiPatternGUI.Invalidate;
    end;
  end
  else if FWavePatternControlGUI.Parent = tsPattern then
  begin
    if not Assigned(GObjectMapper.GetModelObject(FWavePatternControlGUI.WaveGUI.ObjectID)) then
    begin
      tsPattern.TabVisible := False;
    end
    else
    begin
      FWavePatternControlGUI.WaveGUI.Invalidate;
    end;
  end;
end;

constructor TPatternView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  pcEditor.ActivePage := tsPattern;
  pcEditor.ShowTabs := True;

  FTracks := TTrackSettingsList.create(True);

  FWavePatternControlGUI := TWavePatternControlGUI.Create(Self);
  FWavePatternControlGUI.Parent := nil;
  FWavePatternControlGUI.Align := alClient;

  FMidiPatternControlGUI := TMidiPatternControlGUI.Create(Self);
  FMidiPatternControlGUI.Parent := nil;
  FMidiPatternControlGUI.Align := alClient;

  FPluginProcessorGUI := TPluginProcessorGUI.Create(Self);
  FPluginProcessorGUI.Align := alClient;
  FPluginProcessorGUI.Parent := tsEffects;

  tsPattern.TabVisible := False;
  tsEffects.TabVisible := False;

  pcEditor.ActivePage := tsPattern;
end;

destructor TPatternView.Destroy;
begin
  FTracks.Free;

  inherited Destroy;
end;

function TPatternView.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TPatternView.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

procedure TPatternView.Update(Subject: THybridPersistentModel);
begin
  FObjectID := Subject.ObjectID;

  if Subject is TTrack then
  begin
    // Blank page if not a valid pattern in memory anymore
    if FMidiPatternControlGUI.MidiPatternGUI.Parent = tsPattern then
    begin
      if not Assigned(GObjectMapper.GetModelObject(FMidiPatternControlGUI.MidiPatternGUI.ObjectID)) then
      begin
        tsPattern.TabVisible := False;
      end;
    end
    else if FWavePatternControlGUI.WaveGUI.Parent = tsPattern then
    begin
      if not Assigned(GObjectMapper.GetModelObject(FWavePatternControlGUI.WaveGUI.ObjectID)) then
      begin
        tsPattern.TabVisible := False;
      end;
    end;
  end
  else if Subject is TAudioStructure then
  begin
    // Add/Remove tracks
    DiffLists(
      TAudioStructure(Subject).Tracks,
      FTracks,
      @CreateTrackGUI,
      @DeleteTrackGUI);
  end;
end;

function TPatternView.GetModel: THybridPersistentModel;
begin
  Result := FModel;
end;

procedure TPatternView.SetModel(AModel: THybridPersistentModel);
begin
  FModel := AModel;
end;

procedure TPatternView.Connect;
begin
  // Virtual base method
end;

procedure TPatternView.Disconnect;
begin
  // Virtual base method
end;

function TPatternView.GetObjectOwnerID: string;
begin
  Result := FObjectOwnerID;
end;

procedure TPatternView.SetObjectOwnerID(const AObjectOwnerID: string);
begin
  FObjectOwnerID := AObjectOwnerID;
end;

procedure TPatternView.pcEditorDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  lTabIndex: Integer;
begin
  if Sender Is TPageControl then
  begin
    lTabIndex := TPageControl(Sender).IndexOfTabAt(X, Y);
    if lTabIndex >= 0 then
    begin
      TPageControl(Sender).ActivePageIndex := lTabIndex;
    end;
  end;
end;

procedure TPatternView.CreateTrackGUI(AObjectID: string);
var
  lTrack: TTrack;
  lTrackSettings: TTrackSettings;
begin
  lTrack := TTrack(GObjectMapper.GetModelObject(AObjectID));

  if Assigned(lTrack) then
  begin
    lTrackSettings := TTrackSettings.Create(AObjectID, Self);

    FTracks.Add(lTrackSettings);

    lTrack.Attach(lTrackSettings);
  end;
end;

procedure TPatternView.DeleteTrackGUI(AObjectID: string);
var
  lIndex: Integer;
begin
  for lIndex := Pred(FTracks.Count) downto 0 do
  begin
    if FTracks[lIndex].ObjectID = AObjectID then
    begin
      FTracks.Remove(FTracks[lIndex]);
    end;
  end;
end;

function TTrackSettingsList.GetTrackSettings(AIndex: integer): TTrackSettings;
begin
  result := inherited Items[AIndex] as TTrackSettings;
end;

procedure TTrackSettingsList.SetTrackSettings(AIndex: integer; const Value: TTrackSettings);
begin
  inherited Items[AIndex] := Value;
end;

function TTrackSettingsList.Add(ATrackSettings: TTrackSettings): integer;
begin
  Result := inherited Add(ATrackSettings);
end;

initialization
{$I PatternView.lrs}

end.

