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

  trackk.pas
}

unit track;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, globalconst, ContNrs, pattern, jacktypes, utils,
  sndfile, Dialogs, bpm, global_command, global, plugin, pluginhost,
  math;

type
  { TTrack }

  TTrackType = (ttMaster, ttGroup, ttNormal);

  TScheduleType = (stIdle, stStart, stStop, stPause);

  TTrack = class(THybridPersistentModel)
  private
    FPatternList: TObjectList;
    FSelectedPattern: TPattern;
    FPlayingPattern: TPattern;
    FScheduledPattern: TPattern;

    FToTrackID: Integer;   // Push audio to this track => group/master mix buffer
    FFromTrackID: Integer; // Record audio/midi from here

    // Denotes when the track should generate audio, normal track should have
    // data first, after that groups, after that returns and finally master
    // One exception is when recording, track which is recording is processed last
    FProcessPriority: Integer;

    FSelected: Boolean;
    FPitched: Boolean;
    FLeftLevel: Single;
    FRightLevel: Single;
    FLatency: Word;
    FDevValue: shortstring;
    FBooleanStack: Integer;
    FActive: Boolean;
    FRecording: Boolean;
    FVolume: Single;
    FVolumeMultiplier: Single;
    FTrackType: TTrackType;
    FTrackName: string;
    FScheduledTo: TScheduleType;

    //FPluginProcessor: TPluginProcessor;

    // Location where track puts its output samples
    // This is then used by the mixing and routing function to serve this data to
    // master, groups, monitor
    FOutputBuffer: pjack_default_audio_sample_t;
    FPreFadeBuffer: pjack_default_audio_sample_t;

    FAttack_coef: Single;
    FAttack_in_ms: Single;
    FRelease_coef: Single;
    FRelease_in_ms: Single;

    function GetDevValue: shortstring;
    function GetVolume: single;
    procedure SetDevValue(const AValue: shortstring);
    procedure SetLeftLevel(const AValue: Single);
    procedure SetRightLevel(const AValue: Single);
    function GetPlaying: Boolean;
    procedure SetPlaying(const AValue: Boolean);
    procedure SetVolume(const AValue: single);
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Process(ABuffer: PSingle; AFrameCount: Integer);
    function ClearSample: Boolean;
    procedure Assign(Source: TPersistent); override;
    property SelectedPattern: TPattern read FSelectedPattern write FSelectedPattern;
    property PlayingPattern: TPattern read FPlayingPattern write FPlayingPattern;
    property ScheduledPattern: TPattern read FScheduledPattern write FScheduledPattern;
    property OutputBuffer: pjack_default_audio_sample_t read FOutputBuffer write FOutputBuffer;
    property PreFadeBuffer: pjack_default_audio_sample_t read FPreFadeBuffer write FPreFadeBuffer;
    property BooleanStack: Integer read FBooleanStack;
    property Recording: Boolean read FRecording write FRecording;
  published
    property PatternList: TObjectList read FPatternList write FPatternList;
    //property PluginProcessor: TPluginProcessor read FPluginProcessor write FPluginProcessor;
    property LeftLevel: Single read FLeftLevel write SetLeftLevel;
    property RightLevel: Single read FRightLevel write SetRightLevel;
    property Selected: Boolean read FSelected write FSelected;
    property Volume: single read GetVolume write SetVolume;
    property VolumeMultiplier: Single read FVolumeMultiplier write FVolumeMultiplier;
    property Latency: Word read FLatency write FLatency default 0;
    property Playing: Boolean read GetPlaying write SetPlaying;
    property Active: Boolean read FActive write FActive;
    property ToTrackID: Integer read FToTrackID write FToTrackID;
    property DevValue: shortstring read GetDevValue write SetDevValue;
    property TrackType: TTrackType read FTrackType write FTrackType;
    property TrackName: string read FTrackName write FTrackName;
    property ScheduledTo: TScheduleType read FScheduledTo write FScheduledTo;
  end;

  TTrackList = class(TObjectList)
  private
    function GetTrack(AIndex: Integer): TTrack;
    procedure SetTrack(AIndex: Integer; const Value: TTrack);
  public
    property Items[AIndex: Integer] : TTrack read GetTrack write SetTrack; default;
    function Add(ATrack: TTrack): integer;
  end;

  PWaveFormTrack = ^TTrack;

  { TTrackCommand }

  TTrackCommand = class(TCommand)
  private
    FTrack: TTrack;
  protected
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

  TTracksRefreshEvent = procedure (TrackObject: TTrack) of object;

var
  BPMDetect: TBPMDetect;


implementation

uses
  audiostructure, wave, midi;

function TTrackList.Add(ATrack: TTrack): integer;
begin
  Result := inherited Add(ATrack);
end;

function TTrackList.GetTrack(AIndex: integer): TTrack;
begin
  result := inherited Items[aindex] as TTrack;
end;

procedure TTrackList.SetTrack(AIndex: integer; const Value: TTrack);
begin
  inherited Items[AIndex] := Value;
end;

procedure TTrack.SetPlaying(const AValue: Boolean);
begin
  if AValue then
    dec(FBooleanStack)
  else
    inc(FBooleanStack);
end;

procedure TTrack.SetVolume(const AValue: single);
begin
  FVolume := AValue;
  FVolumeMultiplier := FVolume / 100;
end;

procedure TTrack.DoCreateInstance(var AObject: TObject; AClassName: string);
var
  lWavePattern: TWavePattern;
  lMidiPattern: TMidiPattern;
begin
  DBLog('start TWaveFormTrack.DoCreateInstance');

  // create model pattern
  if SameText(UpperCase(AClassName), 'TWAVEPATTERN') then
  begin
    lWavePattern := TWavePattern.Create(ObjectID, MAPPED);

    lWavePattern.ObjectOwnerID := ObjectID;
    AObject := lWavePattern;

    PatternList.Add(lWavePattern);

    FSelectedPattern := lWavePattern;
  end
  else if SameText(UpperCase(AClassName), 'TMIDIPATTERN') then
  begin
    lMidiPattern := TMidiPattern.Create(ObjectID, MAPPED);

    lMidiPattern.ObjectOwnerID := ObjectID;
    AObject := lMidiPattern;

    PatternList.Add(lMidiPattern);

    FSelectedPattern := lMidiPattern;
  end;

  DBLog('end TWaveFormTrack.DoCreateInstance');
end;

procedure TTrack.Initialize;
var
  lWavePattern: TWavePattern;
  lMidiPattern: TMidiPattern;
  i: Integer;
begin
  DBLog('start TWaveFormTrack.Initialize');

  for i := 0 to Pred(PatternList.Count) do
  begin
    if PatternList[i].ClassType = TWavePattern then
    begin
      lWavePattern := TWavePattern(PatternList[i]);

      if FileExists(lWavePattern.WaveFileName) and (not lWavePattern.OkToPlay) then
      begin
        lWavePattern.LoadSample(lWavePattern.WaveFileName);
        lWavePattern.Initialize;
        lWavePattern.OkToPlay := True;
        FSelectedPattern := lWavePattern;

        lWavePattern.Notify;
      end;
    end
    else if PatternList[i].ClassType = TMidiPattern then
    begin
      lMidiPattern := TMidiPattern(PatternList[i]);

      if not lMidiPattern.OkToPlay then
      begin
        lMidiPattern.Initialize;
        lMidiPattern.OkToPlay := True;
        FSelectedPattern := lMidiPattern;

        lMidiPattern.Notify;
      end;
    end;
  end;

  Notify;

  DBLog('end TWaveFormTrack.Initialize');
end;

procedure TTrack.Finalize;
begin
  //
end;

{
  1. Calculate vu meter
  2. ..
  3. ..
}
procedure TTrack.Process(ABuffer: PSingle; AFrameCount: Integer);
var
  i: Integer;
  TempLeftLevel: Single;
  TempRightLevel: Single;
begin
  {for i := 0 to Pred(AFrameCount) do
  begin
    TempLeftLevel := Abs(ABuffer[i] * FVolumeMultiplier);
    if TempLeftLevel > FLeftLevel then
      FLeftLevel := (FAttack_coef * (FLeftLevel - TempLeftLevel)) + TempLeftLevel
    else
      FLeftLevel := (FRelease_coef * (FLeftLevel - TempLeftLevel)) + TempLeftLevel;

    // TODO! Buffer is still in MONO so this code does exactly the same as above
    TempRightLevel := Abs(ABuffer[i] * FVolumeMultiplier);
    if TempRightLevel > FRightLevel then
      FRightLevel := (FAttack_coef * (FRightLevel - TempRightLevel)) + TempRightLevel
    else
      FRightLevel := (FRelease_coef * (FRightLevel - TempRightLevel)) + TempRightLevel;
  end; }

  for i := 0 to Pred(AFrameCount) do
  begin
    // Left
    TempLeftLevel := Abs(ABuffer[i]);
    if TempLeftLevel > FLeftLevel then
    begin
      FLeftLevel := TempLeftLevel;
    end;

    // Right
    TempRightLevel := Abs(ABuffer[i]);
    if TempRightLevel > FRightLevel then
    begin
      FRightLevel := TempRightLevel;
    end;
  end;

  FLeftLevel := FLeftLevel * 0.95;
  FRightLevel := FRightLevel * 0.95;
end;

constructor TTrack.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  DBLog('start TwaveformTrack.Create');

  inherited Create(AObjectOwner, AMapped);

  FOnCreateInstanceCallback := @DoCreateInstance;

  Getmem(FOutputBuffer, 88200);

  FPatternList := TObjectList.create(True);
  ObjectOwnerID := AObjectOwner;

  FBooleanStack:= 1; // Start with off
  FToTrackID := 0; // 0 = master (default)
  FPitched:= False;
  Volume := 100;
  FActive := True;
  FTrackType := ttNormal;

  //FPluginProcessor := TPluginProcessor.Create(GSettings.Frames, AObjectOwner, AMapped);
  //FPluginProcessor.AudioIn.Buffer := FOutputBuffer;

  if Assigned(SelectedPattern) then
  begin
    SelectedPattern.Pitch := 1;
  end;

  FAttack_in_ms := 20;
  FRelease_in_ms := 1000;
  FAttack_coef := power(0.01, 1.0/( FAttack_in_ms * GAudioStruct.MainSampleRate * 0.001));
  FRelease_coef := power(0.01, 1.0/( FRelease_in_ms * GAudioStruct.MainSampleRate * 0.001));

  DBLog('end TwaveformTrack.Create');
end;

destructor TTrack.Destroy;
begin
  DBLog('start TwaveformTrack.Destroy');

  //if Assigned(FPluginProcessor) then
  //  FPluginProcessor.Free;

  if Assigned(FOutputBuffer) then
    Freemem(FOutputBuffer);

  if Assigned(FPatternList) then
    FPatternList.Free;

  inherited Destroy;

  DBLog('end TwaveformTrack.Destroy');
end;

function TTrack.Clearsample: Boolean;
begin
  // Undefined behaviour

  Result := False;
end;

procedure TTrack.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TTrack.SetLeftLevel(const AValue: Single);
begin
  if FLeftLevel > 2 then FLeftLevel := 2;
  if FLeftLevel < 0 then FLeftLevel := 0;
  FLeftLevel := AValue;
end;

procedure TTrack.SetRightLevel(const AValue: Single);
begin
  if FRightLevel > 2 then FRightLevel := 2;
  if FRightLevel < 0 then FRightLevel := 0;
  FRightLevel := AValue;
end;

function TTrack.GetVolume: single;
begin
  Result := FVolume;
end;

function TTrack.GetDevValue: shortstring;
begin
  Result := FDevValue;
end;

procedure TTrack.SetDevValue(const AValue: shortstring);
begin
  FDevValue := AValue;
end;

function TTrack.GetPlaying: Boolean;
begin
  if FBooleanStack <= 0 then
    Result := True
  else
    Result := False;
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
      lWavePattern.PatternName := PatternName;

      InitializePattern(lWavePattern);
    end;
    fsMIDI:
    begin;
      lMidiPattern := TMidiPattern.Create(ObjectOwner, MAPPED);
      lMidiPattern.LoadFromFile(SourceLocation);

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
      if FTrack.Playing then
      begin
        FTrack.Playing := False;
        FTrack.PlayingPattern := nil;
        lPattern.Finalize;
      end;

      FTrack.PatternList.Remove(FTrack.PatternList[i]);

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

initialization
  RegisterClass(TTrack);
  BPMDetect := TBPMDetect.Create(1, 44100);

finalization
  BPMDetect.Free;
  UnRegisterClass(TTrack);

end.

