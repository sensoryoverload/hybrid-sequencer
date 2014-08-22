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
  math, fx;

type
  { TTrack }

  TTrackType = (ttMaster, ttGroup, ttReturn, ttNormal);

  TScheduleType = (stIdle, stStart, stStop, stPause);

  TPatternList = class(TObjectList)
  private
    function GetPattern(AIndex: Integer): TPattern;
    procedure SetPattern(AIndex: Integer; const Value: TPattern);
  public
    property Items[AIndex: Integer] : TPattern read GetPattern write SetPattern; default;
    function Add(APattern: TPattern): integer;
  end;


  TTrack = class(THybridPersistentModel)
  private
    FPatternList: TPatternList;
    FSelectedPattern: TPattern;
    FPlayingPattern: TPattern;
    FScheduledPattern: TPattern;

    FTargetTrackId: string;   // Push audio to this track => group/master mix buffer

    // Denotes when the track should generate audio, normal track should have
    // data first, after that groups, after that returns and finally master
    // One exception is when recording, track which is recording is processed last
    //FProcessPriority: Integer;
    FSelected: Boolean;
    FPitched: Boolean;
    FLeftLevel: Single;
    FRightLevel: Single;
    FLatency: Integer;
    FBooleanStack: Integer;
    FActive: Boolean;
    FRecording: Boolean;
    FTrackId: string;
    // Value between 0 and 1
    FVolume: Single;
    // Value between -1 (Left) and 1 (Right), defaults to 0 (Center)
    FPan: single;
    FLeftPanGain: Single;
    FRightPanGain: Single;

    FVolumeMultiplier: Single;
    FTrackType: TTrackType;
    FTrackName: string;
    FScheduledTo: TScheduleType;

    FPluginProcessor: TPluginProcessor;

    // Location where track puts its output samples
    // This is then used by the mixing and routing function to serve this data to
    // master, groups, monitor
    FOutputBuffer: pjack_default_audio_sample_t;
    FInputBuffer: pjack_default_audio_sample_t;
    FPreFadeBuffer: pjack_default_audio_sample_t;

    FALCBufferL: TAudioRingBuffer;
    FALCBufferR: TAudioRingBuffer;

    FTargetTrack: TTrack;

    FAttack_coef: Single;
    FAttack_in_ms: Single;
    FRelease_coef: Single;
    FRelease_in_ms: Single;

    function GetDelaySmp: Integer;
    function GetLatency: Integer;
    function GetVolume: single;
    procedure SetDelaySmp(AValue: Integer);
    procedure SetPan(AValue: single);
    procedure SetLeftLevel(const AValue: Single);
    procedure SetRightLevel(const AValue: Single);
    function GetPlaying: Boolean;
    procedure SetPlaying(const AValue: Boolean);
    procedure SetVolume(const AValue: single);
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True); reintroduce;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrameCount: Integer);
    function ClearSample: Boolean;
    procedure Assign(Source: TPersistent); override;
    property SelectedPattern: TPattern read FSelectedPattern write FSelectedPattern;
    property PlayingPattern: TPattern read FPlayingPattern write FPlayingPattern;
    property ScheduledPattern: TPattern read FScheduledPattern write FScheduledPattern;
    property OutputBuffer: pjack_default_audio_sample_t read FOutputBuffer write FOutputBuffer;
    property InputBuffer: pjack_default_audio_sample_t read FInputBuffer write FInputBuffer;
    property PreFadeBuffer: pjack_default_audio_sample_t read FPreFadeBuffer write FPreFadeBuffer;
    property BooleanStack: Integer read FBooleanStack;
    property Recording: Boolean read FRecording write FRecording;
    property LeftPanGain: Single read FLeftPanGain;
    property RightPanGain: Single read FRightPanGain;
    property TargetTrack: TTrack read FTargetTrack write FTargetTrack;
    property Latency: Integer read GetLatency write FLatency;
    property DelaySmp: Integer read GetDelaySmp write SetDelaySmp;
  published
    property PatternList: TPatternList read FPatternList write FPatternList;
    property PluginProcessor: TPluginProcessor read FPluginProcessor write FPluginProcessor;
    property LeftLevel: Single read FLeftLevel write SetLeftLevel;
    property RightLevel: Single read FRightLevel write SetRightLevel;
    property Selected: Boolean read FSelected write FSelected;
    property Volume: single read GetVolume write SetVolume;
    property Pan: single read FPan write SetPan;
    property VolumeMultiplier: Single read FVolumeMultiplier write FVolumeMultiplier;
    property Playing: Boolean read GetPlaying write SetPlaying;
    property Active: Boolean read FActive write FActive;
    property TargetTrackId: string read FTargetTrackId write FTargetTrackId;
    property TrackId: string read FTrackId write FTrackId;
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

  TTracksRefreshEvent = procedure (TrackObject: TTrack) of object;

var
  BPMDetect: TBPMDetect;


implementation

uses
  audiostructure, wave, midi;

{ TTrackChangeTargetCommand }

procedure TTrackChangeTargetCommand.DoExecute;
var
  lTrack: TTrack;
  lIndex: Integer;
begin
  DBLog('start Execute TTrackChangeTargetCommand ' + ObjectID);

  FTrack.TargetTrackId := FTargetTrackId;

  if Persist then
  begin
    FTrack.BeginUpdate;

    FOldTargetTrackId := FTargetTrackId;

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

  DBLog('end Execute TTrackChangeTargetCommand');
end;

procedure TTrackChangeTargetCommand.DoRollback;
var
  lTrack: TTrack;
  lIndex: Integer;
begin
  DBLog('start Rollback TTrackChangeTargetCommand ' + ObjectID);

  if Persist then
  begin
    FTrack.BeginUpdate;

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
    if PatternList[i] is TWavePattern then
    begin
      lWavePattern := TWavePattern(PatternList[i]);

      if FileExists(lWavePattern.WaveFileName) and (not lWavePattern.OkToPlay) then
      begin
        lWavePattern.LoadSample(lWavePattern.WaveFileName);
        lWavePattern.Initialize;
        lWavePattern.OkToPlay := True;
        lWavePattern.Enabled := True;
        FSelectedPattern := lWavePattern;

        lWavePattern.Notify;
      end;
    end
    else if PatternList[i] is TMidiPattern then
    begin
      lMidiPattern := TMidiPattern(PatternList[i]);

      if not lMidiPattern.OkToPlay then
      begin
        lMidiPattern.Initialize;
        lMidiPattern.OkToPlay := True;
        lMidiPattern.Enabled := True;
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
procedure TTrack.Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrameCount: Integer);
var
  i: Integer;
  lLeftOffset: Integer;
  lRightOffset: Integer;
  TempLeftLevel: Single;
  TempRightLevel: Single;
begin
  if Active then
  begin
    FPluginProcessor.Process(AMidiBuffer, ABuffer, ABuffer, AFrameCount);

    lLeftOffset := 0;
    lRightOffset := 1;

    for i := 0 to Pred(AFrameCount) do
    begin
      ABuffer[lLeftOffset] := FALCBufferL.Process(ABuffer[lLeftOffset]);
      ABuffer[lRightOffset] := FALCBufferR.Process(ABuffer[lRightOffset]);

      ABuffer[lLeftOffset] := ABuffer[lLeftOffset] * FVolumeMultiplier * FLeftPanGain;
      ABuffer[lRightOffset] := ABuffer[lRightOffset] * FVolumeMultiplier * FRightPanGain;

      // Left
      TempLeftLevel := Abs(ABuffer[lLeftOffset]);
      if TempLeftLevel > FLeftLevel then
      begin
        FLeftLevel := TempLeftLevel;
      end;

      // Right
      TempRightLevel := Abs(ABuffer[lRightOffset]);
      if TempRightLevel > FRightLevel then
      begin
        FRightLevel := TempRightLevel;
      end;

      Inc(lLeftOffset, STEREO);
      Inc(lRightOffset, STEREO);
    end;
  end
  else
  begin
    // Silence both channels
    for i := 0 to Pred(AFrameCount * STEREO) do
    begin
      ABuffer[i] := 0;
    end;
  end;

  // Sum audio to parent track (group, master)

  // Level decay
  FLeftLevel := FLeftLevel * 0.95;
  FRightLevel := FRightLevel * 0.95;
end;

constructor TTrack.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  DBLog('start TwaveformTrack.Create');

  inherited Create(AObjectOwner, AMapped);

  FOnCreateInstanceCallback := @DoCreateInstance;

  FTrackId := ObjectID;

  Getmem(FOutputBuffer, Round(GSettings.SampleRate * STEREO * SizeOf(Single)));
  Getmem(FInputBuffer, Round(GSettings.SampleRate * STEREO * SizeOf(Single)));

  FALCBufferL := TAudioRingBuffer.Create(Round(GSettings.SampleRate));
  FALCBufferL.DelaySmp := 0;
  FALCBufferR := TAudioRingBuffer.Create(Round(GSettings.SampleRate));
  FALCBufferR.Delaysmp := 0;

  FPatternList := TPatternList.create(True);
  ObjectOwnerID := AObjectOwner;

  FBooleanStack:= 1; // Start with off
  FTargetTrackId := ''; // 0 = master (default)
  FPitched:= False;
  Volume := 100;
  Pan := 0; // 0 denotes center position
  FActive := True;
  FTrackType := ttNormal;

  FPluginProcessor := TPluginProcessor.Create(GSettings.Frames, AObjectOwner, AMapped);

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

  if Assigned(FPluginProcessor) then
    FPluginProcessor.Free;

  if Assigned(FOutputBuffer) then
    Freemem(FOutputBuffer);

  if Assigned(FInputBuffer) then
  begin
    Freemem(FInputBuffer);
  end;

  if Assigned(FPatternList) then
    FPatternList.Free;

  if Assigned(FALCBufferL) then
    FALCBufferL.Free;
  if Assigned(FALCBufferR) then
    FALCBufferR.Free;

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

procedure TTrack.SetDelaySmp(AValue: Integer);
begin
  FALCBufferL.DelaySmp := AValue;
  FALCBufferR.DelaySmp := AValue;
end;

function TTrack.GetLatency: Integer;
begin
  if Assigned(FPlayingPattern) then
  begin
    Result := FPlayingPattern.Latency;
  end
  else
  begin
    Result := 0;
  end;

  Result += FPluginProcessor.Latency;
end;

function TTrack.GetDelaySmp: Integer;
begin
  Result := FALCBufferL.DelaySmp;
end;

procedure TTrack.SetPan(AValue: single);
begin
  FPan := AValue;

  FLeftPanGain := (1 - FPan) * (0.7 + 0.2 * FPan);
  FRightPanGain := (1 + FPan) * (0.7 - 0.2 * FPan);

  DBLog(Format('L=%f, R=%f', [FLeftPanGain, FRightPanGain]));
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

  FTrack.Pan := FPan;

  if Persist then
  begin
    FTrack.BeginUpdate;

    FOldPan := FPan;

    FTrack.EndUpdate;
  end;

  DBLog('end Execute TTrackPanCommand');
end;


procedure TTrackPanCommand.DoRollback;
begin
  DBLog('start Rollback TTrackBalanceCommand ' + ObjectID);

  if Persist then
  begin
    FTrack.BeginUpdate;

    FTrack.Pan := FOldPan;

    FTrack.EndUpdate;
  end;

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

function TPatternList.Add(APattern: TPattern): integer;
begin
  Result := inherited Add(APattern);
end;

function TPatternList.GetPattern(AIndex: integer): TPattern;
begin
  result := inherited Items[aindex] as TPattern;
end;

procedure TPatternList.SetPattern(AIndex: integer; const Value: TPattern);
begin
  inherited Items[AIndex] := Value;
end;

initialization
  RegisterClass(TTrack);
  BPMDetect := TBPMDetect.Create(1, 44100);

finalization
  BPMDetect.Free;
  UnRegisterClass(TTrack);

end.

