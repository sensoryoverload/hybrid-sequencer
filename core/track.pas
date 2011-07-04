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
  sndfile, Dialogs, bpm, global_command, global, plugin, pluginhost;

type
  { TTrack }

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
    FLevel: jack_default_audio_sample_t;
    FLatency: Word;
    FDevValue: shortstring;
    FBooleanStack: Integer;
    FActive: Boolean;
    FRecording: Boolean;
    FVolume: Single;
    FVolumeMultiplier: Single;

    //FPluginProcessor: TPluginProcessor;

    // Location where track puts its output samples
    // This is then used by the mixing and routing function to serve this data to
    // master, groups, monitor
    FOutputBuffer: pjack_default_audio_sample_t;
    FPreFadeBuffer: pjack_default_audio_sample_t;

    function GetDevValue: shortstring;
    function GetVolume: single;
    procedure SetDevValue(const AValue: shortstring);
    procedure SetLevel(const AValue: jack_default_audio_sample_t);
    function GetPlaying: Boolean;
    procedure SetPlaying(const AValue: Boolean);
    procedure SetVolume(const AValue: single);
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Initialize; override;
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
    property Level: jack_default_audio_sample_t read FLevel write SetLevel default 0;
    property Selected: Boolean read FSelected write FSelected;
    property Volume: single read GetVolume write SetVolume;
    property VolumeMultiplier: Single read FVolumeMultiplier write FVolumeMultiplier;
    property Latency: Word read FLatency write FLatency default 0;
    property Playing: Boolean read GetPlaying write SetPlaying;
    property Active: Boolean read FActive write FActive;
    property ToTrackID: Integer read FToTrackID write FToTrackID;
    property DevValue: shortstring read GetDevValue write SetDevValue;
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
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property TrackID: string read FTrackID write FTrackID;
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
  FVolume := 1;

  //FPluginProcessor := TPluginProcessor.Create(GSettings.Frames, AObjectOwner, AMapped);
  //FPluginProcessor.AudioIn.Buffer := FOutputBuffer;

  if Assigned(SelectedPattern) then
  begin
    SelectedPattern.Pitch := 1;
  end;

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

procedure TTrack.SetLevel(const AValue: jack_default_audio_sample_t);
begin
  if FLevel > 2 then FLevel := 2;
  if FLevel < 0 then FLevel := 0;
  FLevel:= AValue;
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
          lTrack.Playing := True;
          lIteratePattern.Scheduled := True;
        end
        else
          lIteratePattern.Scheduled := False;
      end;
    end;

    lTrack.EndUpdate;
  end;
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
  lPattern: TPattern;
  lSourceType: string;

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

  DBLog(format('lSourceTrack: %s, lTargetTrack: %s, lPattern: %s', [SourceTrackID, TargetTrackID, PatternID]));

  // Get source track
  lSourceTrack := TTrack(GObjectMapper.GetModelObject(SourceTrackID));
  lTargetTrack := TTrack(GObjectMapper.GetModelObject(TargetTrackID));
  lPattern := TPattern(GObjectMapper.GetModelObject(PatternID));

  if Assigned(lSourceTrack) and Assigned(lTargetTrack) and Assigned(lPattern) then
  begin
    lSourceTrack.PatternList.Extract(lPattern);

    // Update gui's of both tracks
    lSourceTrack.Notify;

    // Change pattern owner
    lPattern.ObjectOwnerID := TargetTrackID;
    lPattern.Position := Position;
    lTargetTrack.PatternList.Add(lPattern);

    lTargetTrack.Notify;
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
    lTargetTrack.PatternList.Extract(lPattern);

    // Update gui's of both tracks
    lTargetTrack.Notify;

    // Change pattern owner
    lPattern.ObjectOwnerID := SourceTrackID;
    lPattern.Position := Position;
    lSourceTrack.PatternList.Add(lPattern);

    lSourceTrack.Notify;
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

