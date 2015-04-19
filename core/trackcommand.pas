unit trackcommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Track, global_command, globalconst, global;

type
  { TTrackCommand }

  TTrackCommand = class(TCommand)
  private
    FTrack: TTrack;
  public
    procedure Initialize; override;
  end;

  { TSchedulePatternCommand }

  TSchedulePatternCommand = class(TTrackCommand)
  private
    FTrackID: string;
    FScheduledTo: TScheduleType;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property TrackID: string read FTrackID write FTrackID;
    property ScheduledTo: TScheduleType read FScheduledTo write FScheduledTo;
  end;

  { TCreatePatternCommand }

  TCreatePatternCommand = class(TTrackCommand)
  private
    FOldObjectID: string;
    FPatternName: string;
    FPosition: Integer;
    FSourceType: TFileSourceTypes;
    FSourceLocation: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property PatternName: string read FPatternName write FPatternName;
    property Position: Integer read FPosition write FPosition;
    property SourceType: TFileSourceTypes read FSourceType write FSourceType;
    property SourceLocation: string read FSourceLocation write FSourceLocation;
  end;

  { TDeletePatternCommand }

  TDeletePatternCommand = class(TTrackCommand)
  private
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
  end;

  { TRepositonPatternCommand }

  TRepositonPatternCommand = class(TTrackCommand)
  private
    FPosition: Integer;
    FOldPosition: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Position: Integer read FPosition write FPosition;
  end;

  { TMovePatternToTrackCommand }

  TMovePatternToTrackCommand = class(TTrackCommand)
  private
    FPosition: Integer;
    FOldPosition: Integer;
    FSourceTrackID: string;
    FTargetTrackID: string;
    FPatternID: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;

  published
    property Position: Integer read FPosition write FPosition;
    property SourceTrackID: string read FSourceTrackID write FSourceTrackID;
    property TargetTrackID: string read FTargetTrackID write FTargetTrackID;
    property PatternID: string read FPatternID write FPatternID;
  end;

  { TActivateTrackCommand }

  TActivateTrackCommand = class(TTrackCommand)
  private
    FActiveState: Boolean;
    FOldActiveState: Boolean;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;

  published
    property ActiveState: Boolean read FActiveState write FActiveState;
  end;

  { TTrackLevelCommand }

  TTrackLevelCommand = class(TTrackCommand)
  private
    FTrackLevel: Single;
    FOldTrackLevel: Single;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;

  published
    property TrackLevel: Single read FTrackLevel write FTrackLevel;
  end;

  TTrackPanCommand = class(TTrackCommand)
  private
    FPan: Single;
    FOldPan: Single;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;

  published
    property Pan: Single read FPan write FPan;
  end;

  { TTrackLatencyCommand }

  TTrackLatencyCommand = class(TTrackCommand)
  private
    FLatency: Integer;
    FOldLatency: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Latency: Integer read FLatency write FLatency;
  end;

  { TTrackChangeTargetCommand }

  TTrackChangeTargetCommand = class(TTrackCommand)
  private
    FTargetTrackId: string;
    FOldTargetTrackId: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;

  published
    property TargetTrackId: string read FTargetTrackId write FTargetTrackId;
  end;

implementation

uses
  utils, audiostructure, pattern, midi, wave;

{ TTrackLatencyCommand }

procedure TTrackLatencyCommand.DoExecute;
begin
  DBLog('start Execute TTrackLatencyCommand ' + ObjectID);

  FTrack.BeginUpdate;

  if Persist then
  begin
    FOldLatency := FTrack.InternalLatency;
  end;

  FTrack.InternalLatency := FLatency;

  FTrack.EndUpdate;

  DBLog('end Execute TTrackPanCommand');
end;

procedure TTrackLatencyCommand.DoRollback;
begin
  DBLog('start Rollback TTrackLatencyCommand ' + ObjectID);

  FTrack.BeginUpdate;

  if Persist then
  begin
    FTrack.InternalLatency := FOldLatency;
  end;

  FTrack.EndUpdate;

  DBLog('end Rollback TTrackLatencyCommand');
end;

{ TTrackChangeTargetCommand }

procedure TTrackChangeTargetCommand.DoExecute;
var
  lTrack: TTrack;
  lIndex: Integer;
  lPlaying: Boolean;
begin
  DBLog('start Execute TTrackChangeTargetCommand ' + ObjectID);

  FTrack.Playing := False;

  if Persist then
  begin
    FTrack.BeginUpdate;

    // Preserve old state
    FOldTargetTrackId := FTrack.TargetTrackId;

    // Set new state
    FTrack.TargetTrackId := FTargetTrackId;

    if FTargetTrackId <> '' then
    begin
      for lIndex := 0 to Pred(GAudioStruct.Tracks.Count) do
      begin
        lTrack := GAudioStruct.Tracks[lIndex];
        if lTrack.TrackId = FTargetTrackId then
        begin
          DBLog('Assigned target track');
          FTrack.TargetTrack := lTrack;
          FTrack.TargetTrackId := lTrack.TrackId;

          break;
        end;
      end;
    end
    else
    begin
      DBLog('Clearing target track');
      FTrack.TargetTrackId := '';
      FTrack.TargetTrack := nil;
    end;

    FTrack.EndUpdate;
  end;

  FTrack.Playing := True;

  DBLog('end Execute TTrackChangeTargetCommand');
end;

procedure TTrackChangeTargetCommand.DoRollback;
var
  lTrack: TTrack;
  lIndex: Integer;
  lPlaying: Boolean;
begin
  DBLog('start Rollback TTrackChangeTargetCommand ' + ObjectID);

  FTrack.Playing := False;

  if Persist then
  begin
    FTrack.BeginUpdate;

    // Restore old state
    FTrack.TargetTrackId := FOldTargetTrackId;

    if FTrack.TargetTrackId <> '' then
    begin
      for lIndex := 0 to Pred(GAudioStruct.Tracks.Count) do
      begin
        lTrack := GAudioStruct.Tracks[lIndex];
        if lTrack.TrackId = FTrack.TargetTrackId then
        begin
          DBLog('Assigned target track');
          FTrack.TargetTrack := lTrack;
          FTrack.TargetTrackId := lTrack.TrackId;

          break;
        end;
      end;
    end
    else
    begin
      DBLog('Clearing target track');
      FTrack.TargetTrackId := '';
      FTrack.TargetTrack := nil;
    end;

    FTrack.EndUpdate;
  end;

  FTrack.Playing := True;

  DBLog('end Rollback TTrackChangeTargetCommand');
end;

{ TDeletePatternCommand }

procedure TDeletePatternCommand.DoExecute;
var
  lIndex: Integer;
  lPattern: TPattern;
begin
  DBLog('start TDeletePatternCommand.DoExecute');
  for lIndex := 0 to Pred(FTrack.PatternList.Count) do
  begin
    lPattern := FTrack.PatternList[lIndex];

    if lPattern.ObjectID = ObjectID then
    begin
      lPattern.Enabled := False;
      lPattern.Playing := False;
      lPattern.OkToPlay := False;

      FTrack.BeginUpdate;
      FTrack.PatternList.Extract(lPattern);

      GObjectMapper.DeleteMapping(lPattern.ObjectID);

      Memento.Add(lPattern);

      FTrack.EndUpdate;
    end;
  end;
  DBLog('end TDeletePatternCommand.DoExecute');
end;

procedure TDeletePatternCommand.DoRollback;
var
  lPattern: TPattern;
  i: Integer;
begin
  DBLog('start TDeletePatternCommand.DoRollback');

  FTrack.BeginUpdate;

  for i := 0 to Pred(Memento.Count) do
  begin
    lPattern := TPattern(Memento[i]);

    GObjectMapper.AddMapping(lPattern);
    FTrack.PatternList.Add(lPattern);
    lPattern.Enabled := True;
  end;

  FTrack.EndUpdate;

  DBLog('end TDeletePatternCommand.DoRollback');
end;
{ TSchedulePatternCommand }

procedure TSchedulePatternCommand.DoExecute;
var
  lPattern: TPattern;
  lTrack: TTrack;
  lIteratePattern: TPattern;
  i: Integer;
begin
  DBLog('start TSchedulePatternCommand.DoExecute');

  // Only one pattern per track can be scheduled
  lPattern := TPattern(GObjectMapper.GetModelObject(ObjectIdList[0]));
  lTrack := TTrack(GObjectMapper.GetModelObject(lPattern.ObjectOwnerID));
  if Assigned(lTrack) then
  begin
    lTrack.BeginUpdate;

    for i := 0 to Pred(lTrack.PatternList.Count) do
    begin
      lIteratePattern := TPattern(lTrack.PatternList[i]);
      if Assigned(lIteratePattern) then
      begin
        if lPattern.ObjectID = lIteratePattern.ObjectID then
        begin
          lTrack.ScheduledPattern := lIteratePattern;

          lTrack.ScheduledTo := FScheduledTo;

          // Make shure track allow audio
          lTrack.Playing := True;

          lIteratePattern.Scheduled := True;
        end
        else
          lIteratePattern.Scheduled := False;
      end;
    end;

    lTrack.EndUpdate;
  end;

  DBLog('end TSchedulePatternCommand.DoExecute');
end;

procedure TSchedulePatternCommand.DoRollback;
begin
  DBLog('TSchedulePatternCommand.DoRollback');
end;

{ TCreatePatternCommand }

procedure TCreatePatternCommand.DoExecute;
var
  lMidiPattern: TMidiPattern;
  lWavePattern: TWavePattern;

  procedure InitializePattern(APattern: TPattern);
  begin
    APattern.Position := Position;
    APattern.ObjectOwnerID := ObjectOwner;

    FTrack.PatternList.Add(APattern);

    FOldObjectID := APattern.ObjectID;
    FTrack.SelectedPattern := APattern;

    APattern.BeginUpdate;
    APattern.Initialize;
    APattern.Enabled := True;
    APattern.EndUpdate;
  end;

begin
  DBLog('start TCreatePatternCommand.DoExecute');

  FTrack.BeginUpdate;

  // creat model pattern
  case PeekFileType(SourceLocation) of
    fsWave:
    begin
      lWavePattern := TWavePattern.Create(ObjectOwner, MAPPED);
      lWavePattern.WaveFileName := SourceLocation;
      lWavePattern.CalculateLoopMarkers;
      lWavePattern.PatternName := PatternName;

      InitializePattern(lWavePattern);
    end;
    fsMIDI:
    begin;
      lMidiPattern := TMidiPattern.Create(ObjectOwner, MAPPED);
      lMidiPattern.LoadFromFile(SourceLocation);
      lMidiPattern.LoopStart.Value := 0;
      lMidiPattern.LoopEnd.Value := Round(2 * GSettings.SampleRate);

      InitializePattern(lMidiPattern);
    end;
    fsEmpty:
      begin
        case SourceType of
          fsWave:
          begin
            lWavePattern := TWavePattern.Create(ObjectOwner, MAPPED);
            lWavePattern.WaveFileName := SourceLocation;
            lWavePattern.PatternName := PatternName;

            InitializePattern(lWavePattern);
          end;
          fsMIDI:
          begin
            lMidiPattern := TMidiPattern.Create(ObjectOwner, MAPPED);
            lMidiPattern.PatternName := PatternName;

            InitializePattern(lMidiPattern);
          end;
        end;
    end;
  end;
  FTrack.EndUpdate;

  DBLog('end TCreatePatternCommand.DoExecute');
end;

procedure TCreatePatternCommand.DoRollback;
var
  i: Integer;
  lPattern: TPattern;
begin
  DBLog('start TCreatePatternCommand.DoRollback');

  FTrack.BeginUpdate;

  for i := 0 to Pred(FTrack.PatternList.Count) do
  begin
    lPattern := TPattern(FTrack.PatternList[i]);
    if lPattern.ObjectID = FOldObjectID then
    begin
      lPattern.Enabled := False;
      GSettings.OldSelectedObject := nil;

      if FTrack.Playing then
      begin
        FTrack.Playing := False;
        FTrack.PlayingPattern := nil;
        lPattern.Finalize;
      end;

      FTrack.PatternList.Remove(lPattern);

      break;
    end;
  end;

  FTrack.EndUpdate;

  DBLog('end TCreatePatternCommand.DoRollback');
end;

{ TActivateTrackCommand }

procedure TActivateTrackCommand.DoExecute;
begin
  DBLog('start Execute TActivateTrackCommand ' + ObjectID);

  FTrack.BeginUpdate;

  FOldActiveState := FactiveState;
  FTrack.Active := not FTrack.Active;

  FTrack.EndUpdate;

  DBLog('end Execute TActivateTrackCommand');
end;

procedure TActivateTrackCommand.DoRollback;
begin
  DBLog('start Rollback TActivateTrackCommand ' + ObjectID);

  FTrack.BeginUpdate;

  FTrack.Active := FOldActiveState;

  FTrack.EndUpdate;

  DBLog('end Rollback TActivateTrackCommand');
end;

{ TTrackLevelCommand }

procedure TTrackLevelCommand.DoExecute;
begin
  DBLog('start Execute TTrackLevelCommand ' + ObjectID);

  FTrack.Volume := FTrackLevel;

  DBLog(Format('Setting track level %f', [FTrackLevel]));

  if Persist then
  begin
    FTrack.BeginUpdate;

    FOldTrackLevel := FTrackLevel;

    FTrack.EndUpdate;
  end;

  DBLog('end Execute TTrackLevelCommand');
end;


procedure TTrackLevelCommand.DoRollback;
begin
  DBLog('start Rollback TTrackLevelCommand ' + ObjectID);

  if Persist then
  begin
    FTrack.BeginUpdate;

    FTrack.Volume := FOldTrackLevel;

    FTrack.EndUpdate;
  end;

  DBLog('end Rollback TTrackLevelCommand');
end;

procedure TTrackPanCommand.DoExecute;
begin
  DBLog('start Execute TTrackPanCommand ' + ObjectID);

  DBLog(Format('Setting Pan to %f (0=L, 1=R)', [FPan]));

  FTrack.BeginUpdate;

  if Persist then
  begin
    FOldPan := FTrack.Pan;
  end;

  FTrack.Pan := FPan;

  FTrack.EndUpdate;

  DBLog('end Execute TTrackPanCommand');
end;


procedure TTrackPanCommand.DoRollback;
begin
  DBLog('start Rollback TTrackBalanceCommand ' + ObjectID);

  DBLog(Format('Setting Pan to %f (0=L, 1=R)', [FOldPan]));

  FTrack.BeginUpdate;

  if Persist then
  begin
    FTrack.Pan := FOldPan;
  end;

  FTrack.EndUpdate;

  DBLog('end Rollback TTrackBalanceCommand');
end;

{ TRepositonPatternCommand }

procedure TRepositonPatternCommand.DoExecute;
var
  lPattern: TPattern;
begin
  DBLog('start TRepositonPatternCommand.DoExecute ' + ObjectID);

  lPattern := TPattern(GObjectMapper.GetModelObject(ObjectID));

  if Assigned(lPattern) then
  begin
    lPattern.BeginUpdate;

    FOldPosition := lPattern.Position;
    lPattern.Position := FPosition;

    lPattern.EndUpdate;

    FTrack.Notify;
  end;

  DBLog('end TRepositonPatternCommand.DoExecute');
end;

procedure TRepositonPatternCommand.DoRollback;
var
  lPattern: TPattern;
begin
  DBLog('start TRepositonPatternCommand.DoRollback ' + ObjectID);

  lPattern := TPattern(GObjectMapper.GetModelObject(ObjectID));

  if Assigned(lPattern) then
  begin
    lPattern.BeginUpdate;

    lPattern.Position := FOldPosition;

    lPattern.EndUpdate;

    FTrack.Notify;
  end;

  DBLog('end TRepositonPatternCommand.DoRollback');
end;

{ TMovePatternToTrackCommand }

procedure TMovePatternToTrackCommand.DoExecute;
var
  lPattern: TPattern;
  lSourceTrack: TTrack;
  lTargetTrack: TTrack;
  lSchedulePattern: TSchedulePatternCommand;
begin
  DBLog('start TMovePatternToTrackCommand.DoExecute');

  // Get source track
  lSourceTrack := TTrack(GObjectMapper.GetModelObject(SourceTrackID));
  lTargetTrack := TTrack(GObjectMapper.GetModelObject(TargetTrackID));
  lPattern := TPattern(GObjectMapper.GetModelObject(PatternID));

  DBLog(format('lSourceTrack: %s, lTargetTrack: %s, lPattern: %s', [SourceTrackID, TargetTrackID, PatternID]));

  if Assigned(lSourceTrack) and Assigned(lTargetTrack) and Assigned(lPattern) then
  begin
    // Disconnect from source track
    if SourceTrackID <> TargetTrackID then
    begin
      if lPattern.Playing then
      begin
        lSchedulePattern := TSchedulePatternCommand.Create(ObjectID);
        try
          lSchedulePattern.ObjectIdList.Add(lPattern.ObjectID);
          lSchedulePattern.TrackID := lSourceTrack.ObjectID;
          lSchedulePattern.ScheduledTo := stStop;
          lSchedulePattern.Persist := False;
          GCommandQueue.PushCommand(lSchedulePattern);
        except
          lSchedulePattern.Free;
        end;
      end;

      lSourceTrack.PatternList.Extract(lPattern);
      lSourceTrack.Notify;

      // Change pattern owner
      lPattern.ObjectOwnerID := TargetTrackID;
      FOldPosition := lPattern.Position;
      lPattern.Position := FPosition;

      // Connect to target track
      lTargetTrack.PatternList.Add(lPattern);
      lTargetTrack.Notify;
    end;
  end;

  DBLog('end TMovePatternToTrackCommand.DoExecute');
end;

procedure TMovePatternToTrackCommand.DoRollback;
var
  lPattern: TPattern;
  lSourceTrack: TTrack;
  lTargetTrack: TTrack;
begin
  DBLog('start TMovePatternToTrackCommand.DoRollback');

  // Get source track
  lSourceTrack := TTrack(GObjectMapper.GetModelObject(SourceTrackID));
  lTargetTrack := TTrack(GObjectMapper.GetModelObject(TargetTrackID));
  lPattern := TPattern(GObjectMapper.GetModelObject(PatternID));

  if Assigned(lSourceTrack) and Assigned(lTargetTrack) and Assigned(lPattern) then
  begin
    // Update gui's of both tracks
    if SourceTrackID <> TargetTrackID then
    begin
      lTargetTrack.PatternList.Extract(lPattern);
      lTargetTrack.Notify;

      // Change pattern owner
      lPattern.ObjectOwnerID := SourceTrackID;
      lPattern.Position := FOldPosition;

      lSourceTrack.PatternList.Add(lPattern);
      lSourceTrack.Notify;
    end;
  end;

  DBLog('end TMovePatternToTrackCommand.DoRollback');
end;

{ TTrackCommand }

procedure TTrackCommand.Initialize;
begin
  FTrack := TTrack(GObjectMapper.GetModelObject(ObjectOwner));
end;

end.

