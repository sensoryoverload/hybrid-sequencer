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
    procedure pcEditorChange(Sender: TObject);
    procedure pcEditorDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    FObjectOwnerID: string;
    FObjectID: string;
    FModel: THybridPersistentModel;
    FTracks: TTrackSettingsList;
    FUpdateSubject: THybridPersistentModel;
    FIsDirty: Boolean;

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
    procedure UpdateView(AForceRedraw: Boolean = False);
    function GetModel: THybridPersistentModel; virtual;
    procedure SetModel(AModel: THybridPersistentModel); virtual;
    procedure Connect;
    procedure Disconnect;

    procedure DoPatternRefreshEvent(TrackObject: TObject);

    property Tracks: TTrackSettingsList read FTracks write FTracks;
  end;

implementation

uses
  utils, sessiongrid, pattern;

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
        PatternView.pcEditor.Visible := False;
        PatternView.tsPattern.TabVisible := False;
        GSettings.SelectedObject := nil;
        GSettings.OldSelectedObject := nil;
      end;
    end
    else if PatternView.FWavePatternControlGUI.WaveGUI.Parent = PatternView.tsPattern then
    begin
      if not Assigned(GObjectMapper.GetModelObject(PatternView.FWavePatternControlGUI.WaveGUI.ObjectID)) then
      begin
        PatternView.pcEditor.Visible := False;
        PatternView.tsPattern.TabVisible := False;
        GSettings.SelectedObject := nil;
        GSettings.OldSelectedObject := nil;
      end;
    end;
  end
end;

procedure TPatternView.DoPatternRefreshEvent(TrackObject: TObject);
var
  lWavePattern: TWavePattern;
  lMidiPattern: TMidiPattern;
  lTrack: TTrack;
begin
  if not Assigned(TrackObject) then
  begin
    pcEditor.Visible := False;
    tsPattern.TabVisible := False;
    tsEffects.TabVisible := False;
    if Assigned(GSettings.OldSelectedObject) then
    begin
      if GSettings.OldSelectedObject is TWavePattern then
      begin
        lWavePattern := TWavePattern(GSettings.OldSelectedObject);
        if Assigned(lWavePattern) then
        begin
          lWavePattern.Detach(FWavePatternControlGUI);
          FWavePatternControlGUI.Parent := nil;

          lWavePattern.PluginProcessor.Detach(FPluginProcessorGUI);
        end;
      end
      else if GSettings.OldSelectedObject is TMidiPattern then
      begin
        lMidiPattern := TMidiPattern(GSettings.OldSelectedObject);
        if Assigned(lMidiPattern) then
        begin
          lMidiPattern.Detach(FMidiPatternControlGUI);
          FMidiPatternControlGUI.Parent := nil;

          lMidiPattern.PluginProcessor.Detach(FPluginProcessorGUI);
        end;
      end
      else if GSettings.OldSelectedObject is TTrack then
      begin
        lTrack := TTrack(GSettings.OldSelectedObject);
        if Assigned(lTrack) then
        begin
          lTrack.PluginProcessor.Detach(FPluginProcessorGUI);
        end;
      end;
    end;
    GSettings.OldSelectedObject := nil;
    GSettings.SelectedObject := nil;
  end
  else if TrackObject is TTrack then
  begin
    if GSettings.OldSelectedObject <> GSettings.SelectedObject then
    begin
      pcEditor.Visible := True;
      tsPattern.TabVisible := False;
      tsEffects.TabVisible := True;

      // Detach if old pattern is visible
      if Assigned(GSettings.OldSelectedObject) then
      begin
        if GSettings.OldSelectedObject is TTrack then
        begin
          lTrack := TTrack(GSettings.OldSelectedObject);
          if Assigned(lTrack) then
          begin
            lTrack.PluginProcessor.Detach(FPluginProcessorGUI);
          end;
        end
        else if GSettings.OldSelectedObject is TMidiPattern then
        begin
          lMidiPattern := TMidiPattern(GSettings.OldSelectedObject);
          if Assigned(lMidiPattern) then
          begin
            lMidiPattern.Detach(FMidiPatternControlGUI);
            FMidiPatternControlGUI.Parent := nil;

            lMidiPattern.PluginProcessor.Detach(FPluginProcessorGUI);
          end;
        end
        else if GSettings.OldSelectedObject is TWavePattern then
        begin
          lWavePattern := TWavePattern(GSettings.OldSelectedObject);
          if Assigned(lWavePattern) then
          begin
            lWavePattern.Detach(FWavePatternControlGUI);
            FWavePatternControlGUI.Parent := nil;

            lWavePattern.PluginProcessor.Detach(FPluginProcessorGUI);
          end;
        end;
      end;

      lTrack := TTrack(GSettings.SelectedObject);
      if Assigned(lTrack) then
      begin
        lTrack.PluginProcessor.Attach(FPluginProcessorGUI);
      end;

      GSettings.OldSelectedObject := GSettings.SelectedObject;
    end;
  end
  else if TrackObject is TWavePattern then
  begin
    if GSettings.OldSelectedObject <> GSettings.SelectedObject then
    begin
      // Detach if old pattern is visible
      if Assigned(GSettings.OldSelectedObject) then
      begin
        if GSettings.OldSelectedObject is TTrack then
        begin
          // Last selected pattern of different type;
          // - detach
          // - set parent to new pattern type
          lTrack := TTrack(GSettings.OldSelectedObject);
          if Assigned(lTrack) then
          begin
            lTrack.PluginProcessor.Detach(FPluginProcessorGUI);
          end;

          // Attach new pattern
          lWavePattern := TWavePattern(GSettings.SelectedObject);
          if Assigned(lWavePattern) then
          begin
            lWavePattern.Attach(FWavePatternControlGUI);
            FWavePatternControlGUI.Parent := tsPattern;

            lWavePattern.PluginProcessor.Attach(FPluginProcessorGUI);
          end;

          pcEditor.Visible := True;
          tsPattern.TabVisible := True;
          tsEffects.TabVisible := True;
        end
        else if GSettings.OldSelectedObject is TMidiPattern then
        begin
          // Last selected pattern of different type;
          // - detach
          // - set parent to new pattern type
          lMidiPattern := TMidiPattern(GSettings.OldSelectedObject);
          if Assigned(lMidiPattern) then
          begin
            lMidiPattern.Detach(FMidiPatternControlGUI);
            FMidiPatternControlGUI.Parent := nil;

            lMidiPattern.PluginProcessor.Detach(FPluginProcessorGUI);
          end;

          // Attach new pattern
          lWavePattern := TWavePattern(GSettings.SelectedObject);
          if Assigned(lWavePattern) then
          begin
            lWavePattern.Attach(FWavePatternControlGUI);
            FWavePatternControlGUI.Parent := tsPattern;

            lWavePattern.PluginProcessor.Attach(FPluginProcessorGUI);
          end;
        end
        else if GSettings.OldSelectedObject is TWavePattern then
        begin
          // Last selected pattern of same type; just detach
          lWavePattern := TWavePattern(GSettings.OldSelectedObject);
          if Assigned(lWavePattern) then
          begin
            lWavePattern.Detach(FWavePatternControlGUI);
            lWavePattern.PluginProcessor.Detach(FPluginProcessorGUI);
          end;

          // Attach new pattern
          lWavePattern := TWavePattern(GSettings.SelectedObject);
          if Assigned(lWavePattern) then
          begin
            lWavePattern.Attach(FWavePatternControlGUI);
            lWavePattern.PluginProcessor.Attach(FPluginProcessorGUI);
          end;
        end
        else
        begin
          // unknown pattern
          pcEditor.Visible := False;
          tsPattern.TabVisible := False;
          tsEffects.TabVisible := False;
        end;
      end
      else
      begin
        if Assigned(GSettings.SelectedObject) then
        begin
          pcEditor.Visible := True;
          tsPattern.TabVisible := True;
          tsEffects.TabVisible := True;

          // Attach new pattern
          lWavePattern := TWavePattern(GSettings.SelectedObject);
          if Assigned(lWavePattern) then
          begin
            lWavePattern.Attach(FWavePatternControlGUI);
            FWavePatternControlGUI.Parent := tsPattern;
            lWavePattern.PluginProcessor.Attach(FPluginProcessorGUI);
          end;
        end
        else
        begin
          pcEditor.Visible := False;
          tsPattern.TabVisible := False;
          tsEffects.TabVisible := False;
        end;
      end;

      GSettings.OldSelectedObject := GSettings.SelectedObject;
    end;
  end
  else if TrackObject is TMidiPattern then
  begin
    if GSettings.OldSelectedObject <> GSettings.SelectedObject then
    begin
      // Detach if old pattern is visible
      if Assigned(GSettings.OldSelectedObject) then
      begin
        if GSettings.OldSelectedObject is TTrack then
        begin
          // Last selected pattern of different type;
          // - detach
          // - set parent to new pattern type
          lTrack := TTrack(GSettings.OldSelectedObject);
          if Assigned(lTrack) then
          begin
            lTrack.PluginProcessor.Detach(FPluginProcessorGUI);
          end;

          // Attach new pattern
          lMidiPattern := TMidiPattern(GSettings.SelectedObject);
          if Assigned(lMidiPattern) then
          begin
            lMidiPattern.Attach(FMidiPatternControlGUI);
            FMidiPatternControlGUI.Parent := tsPattern;

            lMidiPattern.PluginProcessor.Attach(FPluginProcessorGUI);
          end;

          pcEditor.Visible := True;
          tsPattern.TabVisible := True;
          tsEffects.TabVisible := True;
        end
        else if GSettings.OldSelectedObject is TWavePattern then
        begin
          // Last selected pattern of different type;
          // - detach
          // - set parent to new pattern type
          lWavePattern := TWavePattern(GSettings.OldSelectedObject);
          if Assigned(lWavePattern) then
          begin
            lWavePattern.Detach(FWavePatternControlGUI);
            FWavePatternControlGUI.Parent := nil;

            lWavePattern.PluginProcessor.Detach(FPluginProcessorGUI);
          end;

          lMidiPattern := TMidiPattern(GSettings.SelectedObject);
          if Assigned(lMidiPattern) then
          begin
            lMidiPattern.Attach(FMidiPatternControlGUI);
            FMidiPatternControlGUI.Parent := tsPattern;
            lMidiPattern.PluginProcessor.Attach(FPluginProcessorGUI);
          end;
        end
        else if GSettings.OldSelectedObject is TMidiPattern then
        begin
          // Last selected pattern of same type; just detach
          lMidiPattern := TMidiPattern(GSettings.OldSelectedObject);
          if Assigned(lMidiPattern) then
          begin
            lMidiPattern.Detach(FMidiPatternControlGUI);
            FMidiPatternControlGUI.Parent := nil;

            lMidiPattern.PluginProcessor.Detach(FPluginProcessorGUI);
          end;

          lMidiPattern := TMidiPattern(GSettings.SelectedObject);
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
          pcEditor.Visible := False;
          tsPattern.TabVisible := False;
          tsEffects.TabVisible := False;
        end;
      end
      else
      begin
        if Assigned(GSettings.SelectedObject) then
        begin
          lMidiPattern := TMidiPattern(GSettings.SelectedObject);
          if Assigned(lMidiPattern) then
          begin
            lMidiPattern.Attach(FMidiPatternControlGUI);
            FMidiPatternControlGUI.Parent := tsPattern;
            lMidiPattern.PluginProcessor.Attach(FPluginProcessorGUI);
          end;

          pcEditor.Visible := True;
          tsPattern.TabVisible := True;
          tsEffects.TabVisible := True;
        end
        else
        begin
          // Should hide
        end;
      end;

      GSettings.OldSelectedObject := GSettings.SelectedObject;
    end;
  end;

  if Assigned(GSettings.SelectedObject) then
  begin
    if GSettings.SelectedObject is TTrack then
    begin
      pcEditor.ActivePage := tsEffects;
    end
    else
    begin
      pcEditor.ActivePageIndex := TPattern(GSettings.SelectedObject).VisibleTabIndex;
    end;
  end;
end;

constructor TPatternView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FIsDirty := False;

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
  FPluginProcessorGUI.Parent := tsEffects;
  FPluginProcessorGUI.Align := alClient;

  FPluginProcessorGUI.OnChangeNodeList := @FMidiPatternControlGUI.PopulateAutomationControls;

  pcEditor.Visible := False;
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
  FUpdateSubject := Subject;

  FIsDirty := True;
end;

procedure TPatternView.UpdateView(AForceRedraw: Boolean = False);
begin
  FObjectID := FUpdateSubject.ObjectID;

  if FIsDirty and Assigned(FUpdateSubject) then
  begin
    if FUpdateSubject is TTrack then
    begin
      // Blank page if not a valid pattern in memory anymore
      if FMidiPatternControlGUI.MidiPatternGUI.Parent = tsPattern then
      begin
        if not Assigned(GObjectMapper.GetModelObject(FMidiPatternControlGUI.MidiPatternGUI.ObjectID)) then
        begin
          pcEditor.Visible := False;
          tsPattern.TabVisible := False;
          tsEffects.TabVisible := False;
        end;
      end
      else if FWavePatternControlGUI.WaveGUI.Parent = tsPattern then
      begin
        if not Assigned(GObjectMapper.GetModelObject(FWavePatternControlGUI.WaveGUI.ObjectID)) then
        begin
          pcEditor.Visible := False;
          tsEffects.TabVisible := False;
          tsPattern.TabVisible := False;
        end;
      end;
    end;
  end;

  // Update patterneditor grid
  if FMidiPatternControlGUI.Parent = tsPattern then
  begin
    if not Assigned(GObjectMapper.GetModelObject(FMidiPatternControlGUI.MidiPatternGUI.ObjectID)) then
    begin
      pcEditor.Visible := False;
      tsPattern.TabVisible := False;
      tsEffects.TabVisible := False;
    end
    else
    begin
      FPluginProcessorGUI.OnChangeNodeList := @FMidiPatternControlGUI.PopulateAutomationControls;
      FMidiPatternControlGUI.MidiPatternGUI.UpdateView(False);
    end;
  end
  else if FWavePatternControlGUI.Parent = tsPattern then
  begin
    if not Assigned(GObjectMapper.GetModelObject(FWavePatternControlGUI.WaveGUI.ObjectID)) then
    begin
      pcEditor.Visible := False;
      tsPattern.TabVisible := False;
      tsEffects.TabVisible := False;
    end
    else
    begin
      FPluginProcessorGUI.OnChangeNodeList := @FWavePatternControlGUI.PopulateAutomationControls;
      FWavePatternControlGUI.WaveGUI.UpdateView(False);
    end;
    FIsDirty := False;
  end;

  FPluginProcessorGUI.UpdateView;
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

procedure TPatternView.pcEditorChange(Sender: TObject);
begin
  // Save active page to midi/wav -pattern so it can be recalled when switching
  // to a previously selected pattern
  if Assigned(GSettings.SelectedObject) then
  begin
    TPattern(GSettings.SelectedObject).VisibleTabIndex := pcEditor.ActivePageIndex;
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

