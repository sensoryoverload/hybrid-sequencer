unit wavecommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, global, wave, global_command, globalconst, utils;

type
  { TWaveFormCommand }

  TWaveFormCommand = class(TCommand)
  private
    FWavePattern: TWavePattern;
  public
    procedure Initialize; override;
    procedure Finalize; override;
  end;

  { TAddMarkerCommand }

  TAddMarkerCommand = class(TWaveFormCommand)
  private
    FLocation: Integer;
    FOriginalLocation: Integer;
    FActive: Boolean;
    FSliceType: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Active: Boolean read FActive write FActive;
    property SliceType: Integer read FSliceType write FSliceType;
    property Location: Integer read FLocation write FLocation;
    property OriginalLocation: Integer read FOriginalLocation write FOriginalLocation;
  end;

  { TRemoveMarkerCommand }

  TRemoveMarkerCommand = class(TWaveFormCommand)
  private
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  public
  end;

  { TToggleLockMarkerCommand }

  TToggleLockMarkerCommand = class(TWaveFormCommand)
  private
    FLocked: Boolean;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  public
  end;

  { TUpdateMarkerCommand }

  TUpdateMarkerCommand = class(TWaveFormCommand)
  private
    FLocation: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Location: Integer read FLocation write FLocation;
  end;

  { TUpdateWaveSampleMarkerCommand }

  TUpdateWaveSampleMarkerCommand = class(TWaveFormCommand)
  private
    FDataType: TSampleMarkerType;
    FLocation: Integer;
    FOldLocation: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Location: Integer read FLocation write FLocation;
    property DataType: TSampleMarkerType read FDataType write FDataType;
  end;

  { TUpdateWaveLoopMarkerCommand }

  TUpdateWaveLoopMarkerCommand = class(TWaveFormCommand)
  private
    FDataType: TLoopMarkerType;
    FLocation: Integer;
    FOldLocation: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Location: Integer read FLocation write FLocation;
    property DataType: TLoopMarkerType read FDataType write FDataType;
  end;

  { TPatternDropWaveCommand }

  TPatternDropWaveCommand = class(TWaveFormCommand)
  private
    FFileName: string;
    FOldFileName: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property FileName: string read FFileName write FFileName;
  end;

  { TTogglePitchCommand }

  TTogglePitchCommand = class(TWaveFormCommand)
  private
    FState: Boolean;
    FOldState: Boolean;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property State: Boolean read FState write FState;
  end;

  { TChangePitchCommand }

  TChangePitchCommand = class(TWaveFormCommand)
  private
    FPitch: Single;
    FOldPitch: Single;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Pitch: Single read FPitch write FPitch;
  end;

  { TChangeOriginalBPMCommand }

  TChangeRealBPMCommand = class(TWaveFormCommand)
  private
    FRealBPM: Single;
    FOldRealBPM: Single;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;

  published
    property RealBPM: Single read FRealBPM write FRealBPM;
  end;

  { TChangeThresHoldCommand }

  TChangeThresHoldCommand = class(TWaveFormCommand)
  private
    FThreshold: Integer;
    FOldThreshold: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;

  published
    property Threshold: Integer read FThreshold write FThreshold;
  end;

  { TDoubleLoopLengthCommand }

  TDoubleLoopLengthCommand = class(TWaveFormCommand)
  private
    FOldLoopLength: Integer;
    FOldLoopEnd: Integer;
    FOldSampleEnd: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  end;

  { THalveLoopLengthCommand }

  THalveLoopLengthCommand = class(TWaveFormCommand)
  private
    FOldLoopLength: Integer;
    FOldLoopEnd: Integer;
    FOldSampleEnd: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  end;

  { TChangeLatencyCommand }

  TChangeLatencyCommand = class(TWaveFormCommand)
  private
    FOldValue: Integer;
    FValue: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Latency: Integer read FValue write FValue;
  end;

  { TChangeQuantizeCommand }

  TChangeQuantizeCommand = class(TWaveFormCommand)
  private
    FOldValue: TQuantizeSettings;
    FValue: TQuantizeSettings;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Value: TQuantizeSettings read FValue write FValue;
  end;

  { TChangeStretchAlgoCommand }

  TChangeStretchAlgoCommand = class(TWaveFormCommand)
  private
    FPitchAlgorithm: TPitchAlgorithm;
    FOldPitchAlgorithm: TPitchAlgorithm;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;

  published
    property PitchAlgorithm: TPitchAlgorithm read FPitchAlgorithm write FPitchAlgorithm;
  end;

  { TUpdateThresholdCommand }

  TUpdateThresholdCommand = class(TWaveFormCommand)
  private
    FSensitivity: Single;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Sensitivity: Single read FSensitivity write FSensitivity;
  end;

implementation

function QuantizeLocation(ALocation: Integer; AQuantizeSettings: TQuantizeSettings): Integer;
begin
  case AQuantizeSettings of
    qsNone: Result := ALocation;
    qsBar: Result := (ALocation div QUNATIZE_BAR) * QUNATIZE_BAR;
    qsBeat: Result := (ALocation div QUANTIZE_BEAT) * QUANTIZE_BEAT;
    qs4th: Result := (ALocation div QUANTIZE_4TH) * QUANTIZE_4TH;
    qs8th: Result := (ALocation div QUANTIZE_8TH) * QUANTIZE_8TH;
    qs16th: Result := (ALocation div QUANTIZE_16TH) * QUANTIZE_16TH;
    qs32th: Result := (ALocation div QUANTIZE_32TH) * QUANTIZE_32TH;
  end;
end;

{ TUpdateThresholdCommand }

procedure TUpdateThresholdCommand.DoExecute;
begin
  FWavePattern.Sensitivity := FSensitivity;
  FWavePattern.AutoMarkerProcess(True);
  FWavePattern.Notify;
end;

procedure TUpdateThresholdCommand.DoRollback;
begin
  inherited DoRollback;
end;

{ TChangeQuantizeCommand }

procedure TChangeQuantizeCommand.DoExecute;
begin
  FOldValue := FValue;
  FWavePattern.QuantizeSetting := FValue;
end;

procedure TChangeQuantizeCommand.DoRollback;
begin
  FWavePattern.QuantizeSetting := FOldValue;
end;

{ TChangeLatencyCommand }

procedure TChangeLatencyCommand.DoExecute;
begin
  FOldValue := FValue;
// deprecated  FWavePattern.Latency := FValue;
end;

procedure TChangeLatencyCommand.DoRollback;
begin
// deprecated  FWavePattern.Latency := FOldValue;
end;

{ THalveLoopLengthCommand }

procedure THalveLoopLengthCommand.DoExecute;
begin
  FOldLoopEnd := FWavePattern.LoopEnd.Value;
  FOldLoopLength := FWavePattern.LoopLength.Value;
  FOldSampleEnd := FWavePattern.SampleEnd.Value;

  FWavePattern.LoopLength.Value := FWavePattern.LoopLength.Value div 2;
  FWavePattern.LoopEnd.Value :=
    FWavePattern.LoopStart.Value + FWavePattern.LoopLength.Value;

  FWavePattern.SampleEnd.Value := FWavePattern.SampleStart.Value +
    (FWavePattern.SampleEnd.Value - FWavePattern.SampleStart.Value) div 2;

  FWavePattern.UpdateBPMScale;
  FWavePattern.UpdateSampleScale;

  FWavePattern.Notify;
  FWavePattern.LoopLength.Notify;
  FWavePattern.LoopEnd.Notify;
  FWavePattern.SampleEnd.Notify;
end;

procedure THalveLoopLengthCommand.DoRollback;
begin
  FWavePattern.LoopLength.Value := FOldLoopLength;
  FWavePattern.LoopEnd.Value := FOldLoopEnd;
  FWavePattern.SampleEnd.Value := FOldSampleEnd;

  FWavePattern.UpdateBPMScale;
  FWavePattern.UpdateSampleScale;

  FWavePattern.Notify;
  FWavePattern.LoopLength.Notify;
  FWavePattern.LoopEnd.Notify;
  FWavePattern.SampleEnd.Notify;
end;

{ TDoubleLoopLengthCommand }

procedure TDoubleLoopLengthCommand.DoExecute;
begin
  FOldLoopEnd := FWavePattern.LoopEnd.Value;
  FOldLoopLength := FWavePattern.LoopLength.Value;
  FOldSampleEnd := FWavePattern.SampleEnd.Value;

  FWavePattern.LoopLength.Value := FWavePattern.LoopLength.Value * 2;
  FWavePattern.LoopEnd.Value := FWavePattern.LoopStart.Value + FWavePattern.LoopLength.Value;

  FWavePattern.SampleEnd.Value := FWavePattern.SampleStart.Value +
    (FWavePattern.SampleEnd.Value - FWavePattern.SampleStart.Value) * 2;

  FWavePattern.UpdateBPMScale;
  FWavePattern.UpdateSampleScale;

  FWavePattern.Notify;
  FWavePattern.LoopLength.Notify;
  FWavePattern.LoopEnd.Notify;
  FWavePattern.SampleEnd.Notify;
end;

procedure TDoubleLoopLengthCommand.DoRollback;
begin
  FWavePattern.LoopLength.Value := FOldLoopLength;
  FWavePattern.LoopEnd.Value := FOldLoopEnd;
  FWavePattern.SampleEnd.Value := FOldSampleEnd;

  FWavePattern.UpdateBPMScale;
  FWavePattern.UpdateSampleScale;

  FWavePattern.Notify;
  FWavePattern.LoopLength.Notify;
  FWavePattern.LoopEnd.Notify;
  FWavePattern.SampleEnd.Notify;
end;

{ TAddMarkerCommand }

procedure TAddMarkerCommand.DoExecute;
var
  lMementoMarker: TMarker;
  lMarker: TMarker;
begin
  // Check if there's a marker already
  lMarker := FWavePattern.AddSlice(Location, SLICE_NORMAL, True);
  lMarker.Locked := False;

  if Assigned(lMarker) then
  begin
    lMementoMarker := TMarker.Create(ObjectOwner, NOT_MAPPED);
    lMementoMarker.Active := True;
    lMementoMarker.ObjectID := lMarker.ObjectID;
    lMementoMarker.Location := lMarker.Location;
    lMementoMarker.OrigLocation := lMarker.OrigLocation;
    Memento.Add(lMementoMarker);
  end;

  FWavePattern.SortSlices;
  FWavePattern.Notify;
end;

procedure TAddMarkerCommand.DoRollback;
var
  i, j: Integer;
  lMementoMarker: TMarker;
  lMarker: TMarker;
begin
  for i := 0 to Pred(Memento.Count) do
  begin
    lMementoMarker := TMarker(Memento[i]);

    for j := 0 to Pred(FWavePattern.SliceList.Count) do
    begin
      lMarker := TMarker(FWavePattern.SliceList[j]);

      if lMementoMarker.ObjectID = lMarker.ObjectID then
      begin
        FWavePattern.SliceList.Remove(lMarker);
        break;
      end;
    end;
  end;

  FWavePattern.SortSlices;
  FWavePattern.Notify;
end;

{ TUpdateMarkerCommand }

procedure TUpdateMarkerCommand.DoExecute;
var
  lMementoMarker: TMarker;
  lMarker: TMarker;
begin
  // Check if there's a marker already
  lMarker := TMarker(GObjectMapper.GetModelObject(ObjectID));

  if Assigned(lMarker) then
  begin
    if Persist then
    begin
      lMementoMarker := TMarker.Create(ObjectOwner, NOT_MAPPED);
      lMementoMarker.Active := True;
      lMementoMarker.ObjectID := lMarker.ObjectID;
      lMementoMarker.Location := lMarker.Location;
      lMementoMarker.OrigLocation := lMarker.OrigLocation;
      lMementoMarker.DecayRate := lMarker.DecayRate;
      lMementoMarker.Locked := lMarker.Locked;
      Memento.Add(lMementoMarker);
    end;

    DBLog(Format('Moving slice location from %d to %d',
      [lMarker.Location, Location]));
    lMarker.Location := Location;
    lMarker.Notify;

    FWavePattern.SortSlices;
    FWavePattern.Notify;
  end;
end;


procedure TUpdateMarkerCommand.DoRollback;
var
  i: Integer;
  lMementoMarker: TMarker;
  lMarker: TMarker;
begin
  for i := 0 to Pred(Memento.Count) do
  begin
    lMementoMarker := TMarker(Memento[i]);

    lMarker := TMarker(GObjectMapper.GetModelObject(ObjectID));

    if Assigned(lMarker) then
    begin
      lMarker.Location := lMementoMarker.Location;
      lMarker.Notify;
    end;
  end;

  FWavePattern.SortSlices;
  FWavePattern.Notify;
end;

{ TRemoveMarkerCommand }

procedure TRemoveMarkerCommand.DoExecute;
var
  lMementoMarker: TMarker;
  lMarker: TMarker;
begin
  DBLog('start TRemoveMarkerCommand.DoExecute');

  lMarker := TMarker(GObjectMapper.GetModelObject(ObjectID));

  if Assigned(lMarker) then
  begin
    lMementoMarker := TMarker.Create(ObjectOwner, NOT_MAPPED);
    lMementoMarker.Active := True;
    lMementoMarker.ObjectID := lMarker.ObjectID;
    lMementoMarker.Location := lMarker.Location;
    lMementoMarker.OrigLocation := lMarker.OrigLocation;
    lMementoMarker.DecayRate := lMarker.DecayRate;
    Memento.Add(lMementoMarker);

    // Update observers
    FWavePattern.SliceList.Remove(lMarker);

    FWavePattern.SortSlices;
    FWavePattern.Notify;
  end;

  DBLog('end TRemoveMarkerCommand.DoExecute');
end;


procedure TRemoveMarkerCommand.DoRollback;
var
  i: Integer;
  lMementoMarker: TMarker;
  lMarker: TMarker;
begin
  for i := 0 to Pred(Memento.Count) do
  begin
    lMementoMarker := TMarker(Memento[i]);

    // Check if there's a marker already
    lMarker := FWavePattern.AddSlice(lMementoMarker.Location, SLICE_NORMAL, True);
    lMarker.Active := True;
    lMarker.ObjectID := lMementoMarker.ObjectID;
    lMarker.Location := lMementoMarker.Location;
    lMarker.OrigLocation := lMementoMarker.OrigLocation;
    lMarker.DecayRate := lMementoMarker.DecayRate;
  end;

  FWavePattern.SortSlices;
  FWavePattern.Notify;
end;

{ TToggleLockMarkerCommand }

procedure TToggleLockMarkerCommand.DoExecute;
var
  lMarker: TMarker;
begin
  DBLog('start TToggleLockMarkerCommand.DoExecute');

  lMarker := TMarker(GObjectMapper.GetModelObject(ObjectID));

  if Assigned(lMarker) then
  begin
    lMarker.BeginUpdate;

    // Save state
    FLocked := lMarker.Locked;

    // Toggle
    lMarker.Locked := not lMarker.Locked;

    DBLog(Format('Setting locked = %s', [BoolToStr(lMarker.Locked, True)]));

    // Update observers
    lMarker.EndUpdate;

    FWavePattern.Notify;
  end;

  DBLog('end TToggleLockMarkerCommand.DoExecute');
end;

procedure TToggleLockMarkerCommand.DoRollback;
var
  lMarker: TMarker;
begin
  DBLog('start TToggleLockMarkerCommand.DoRollback');

  lMarker := TMarker(GObjectMapper.GetModelObject(ObjectID));

  if Assigned(lMarker) then
  begin
    lMarker.BeginUpdate;

    // Retrieve state
    lMarker.Locked := FLocked;

    DBLog(Format('Setting locked = %s', [BoolToStr(lMarker.Locked, True)]));

    // Update observers
    lMarker.EndUpdate;

    FWavePattern.Notify;
  end;

  DBLog('end TToggleLockMarkerCommand.DoRollback');
end;

{ TUpdateWaveSampleMarkerCommand }

procedure TUpdateWaveSampleMarkerCommand.DoExecute;
begin
  DBLog('start TUpdateWaveSampleMarkerCommand.DoExecute');

  //if Persist then
  begin
    // Save state
    case FDataType of
    stStart: FOldLocation := FWavePattern.SampleStart.Value;
    stEnd: FOldLocation := FWavePattern.SampleEnd.Value;
    end;
  end;

  // Assign
  case FDataType of
    stStart:
    begin
      if FLocation < 0 then FLocation := 0;
      FWavePattern.SampleStart.Value := QuantizeLocation(FLocation, FWavePattern.QuantizeSetting);
    end;
    stEnd:
    begin
      if FLocation < 0 then FLocation := 0;
      FWavePattern.SampleEnd.Value := QuantizeLocation(FLocation, FWavePattern.QuantizeSetting);
    end;
  end;

  FWavePattern.UpdateSampleScale;
  FWavePattern.UpdateBPMScale;

  // Update observers
  FWavePattern.Notify;
  FWavePattern.SampleStart.Notify;
  FWavePattern.SampleEnd.Notify;

  DBLog('end TUpdateWaveSampleMarkerCommand.DoExecute');
end;

procedure TUpdateWaveSampleMarkerCommand.DoRollback;
begin
  DBLog('start TUpdateWaveSampleMarkerCommand.DoRollback');

  // Assign
  case FDataType of
    stStart: FWavePattern.SampleStart.Value := FOldLocation;
    stEnd: FWavePattern.SampleEnd.Value := FOldLocation;
  end;

  FWavePattern.UpdateSampleScale;
  FWavePattern.UpdateBPMScale;

  // Update observers
  FWavePattern.Notify;
  FWavePattern.SampleStart.Notify;
  FWavePattern.SampleEnd.Notify;

  DBLog('end TUpdateWaveSampleMarkerCommand.DoRollback');
end;

{ TUpdateWaveSampleCommand }

procedure TUpdateWaveLoopMarkerCommand.DoExecute;
begin
  DBLog('start TUpdateWaveLoopMarkerCommand.DoExecute');

  //if Persist then
  begin
    // Save state
    case FDataType of
    ltStart: FOldLocation := FWavePattern.LoopStart.Value;
    ltEnd: FOldLocation := FWavePattern.LoopEnd.Value;
    ltLength: FOldLocation := FWavePattern.LoopLength.Value;
    end;
  end;

  // Assign
  case FDataType of
  ltStart:
  begin
    if FLocation < 0 then FLocation := 0;

    FWavePattern.LoopStart.Value := QuantizeLocation(FLocation, FWavePattern.QuantizeSetting);
  end;
  ltEnd:
  begin
    if FLocation < 0 then FLocation := 0;
    FWavePattern.LoopEnd.Value := QuantizeLocation(FLocation, FWavePattern.QuantizeSetting);
  end;
  ltLength: FWavePattern.LoopLength.Value := QuantizeLocation(FLocation, FWavePattern.QuantizeSetting);
  end;

  FWavePattern.UpdateSampleScale;
  FWavePattern.UpdateBPMScale;

  // Update observers
  FWavePattern.Notify;
  FWavePattern.LoopStart.Notify;
  FWavePattern.LoopEnd.Notify;
  FWavePattern.LoopLength.Notify;

  DBLog('end TUpdateWaveLoopMarkerCommand.DoExecute');
end;

procedure TUpdateWaveLoopMarkerCommand.DoRollback;
begin
  DBLog('start TUpdateWaveLoopStartCommand.DoRollback');

  // Retrieve state
  FWavePattern.LoopStart.Value := FOldLocation;

  // Assign
  case FDataType of
  ltStart: FWavePattern.LoopStart.Value := FOldLocation;
  ltEnd: FWavePattern.LoopEnd.Value := FOldLocation;
  ltLength: FWavePattern.LoopLength.Value := FOldLocation;
  end;

  FWavePattern.UpdateSampleScale;
  FWavePattern.UpdateBPMScale;

  // Update observers
  FWavePattern.Notify;
  FWavePattern.LoopStart.Notify;
  FWavePattern.LoopEnd.Notify;
  FWavePattern.LoopLength.Notify;

  DBLog('end TUpdateWaveLoopStartCommand.DoRollback');
end;

{ TWaveFormCommand }

procedure TWaveFormCommand.Initialize;
begin
  inherited Initialize;

  FWavePattern := TWavePattern(GObjectMapper.GetModelObject(ObjectOwner));
end;

procedure TWaveFormCommand.Finalize;
begin
  inherited Finalize;
end;

{ TPatternDropWaveCommand }

procedure TPatternDropWaveCommand.DoExecute;
begin
  DBLog('start TPatternDropWaveCommand.DoExecute');

  FWavePattern.BeginUpdate;

  FOldFileName := FWavePattern.WaveFileName;
  FWavePattern.WaveFileName := FFileName;
  FWavePattern.Initialize;

  FWavePattern.EndUpdate;

  DBLog('end TPatternDropWaveCommand.DoExecute');
end;

procedure TPatternDropWaveCommand.DoRollback;
begin
  DBLog('start TPatternDropWaveCommand.DoRollback');

  FWavePattern.BeginUpdate;

  FWavePattern.WaveFileName := FFileName;
  FWavePattern.UnLoadSample;
  FWavePattern.Finalize;

  FWavePattern.EndUpdate;

  DBLog('end TPatternDropWaveCommand.DoRollback');
end;

{ TTogglePitchCommand }

procedure TTogglePitchCommand.DoExecute;
begin
  DBLog('start TTogglePitchCommand.DoExecute');

  FWavePattern.BeginUpdate;

  FOldState := FState;
  FWavePattern.Pitched := FState;

  FWavePattern.EndUpdate;

  DBLog('end TTogglePitchCommand.DoExecute');
end;

procedure TTogglePitchCommand.DoRollback;
begin
  DBLog('start TTogglePitchCommand.DoRollback');

  FWavePattern.BeginUpdate;

  FWavePattern.Pitched := FOldState;

  FWavePattern.EndUpdate;

  DBLog('end TTogglePitchCommand.DoRollback');
end;

{ TChangePitchCommand }

procedure TChangePitchCommand.DoExecute;
begin
  DBLog('start TChangePitchCommand.DoExecute');

  FWavePattern.BeginUpdate;

  // Store pitch
  FOldpitch := FWavePattern.Pitch;
  FWavePattern.Pitch := Pitch;

  FWavePattern.EndUpdate;

  DBLog('end TChangePitchCommand.DoExecute');
end;

procedure TChangePitchCommand.DoRollback;
begin
  DBLog('start TChangePitchCommand.DoRollback');

  FWavePattern.BeginUpdate;

  // Restore pitch
  FWavePattern.Pitch := FOldpitch;

  FWavePattern.EndUpdate;

  DBLog('end TChangePitchCommand.DoRollback');
end;

{ TChangeThresHoldCommand }

procedure TChangeThresHoldCommand.DoExecute;
begin
  DBLog('start TChangeThresHoldCommand.DoExecute');

  FWavePattern.BeginUpdate;

  // Store Threshold
  FOldThreshold := FWavePattern.TransientThreshold;
  FWavePattern.TransientThreshold := FThreshold;

  FWavePattern.EndUpdate;

  DBLog('end TChangeThresHoldCommand.DoExecute');
end;

procedure TChangeThresHoldCommand.DoRollback;
begin
  DBLog('start TChangeThresHoldCommand.DoRollback');

  FWavePattern.BeginUpdate;

  // Restore Threshold
  FWavePattern.TransientThreshold := FOldThreshold;

  FWavePattern.EndUpdate;

  DBLog('end TChangeThresHoldCommand.DoRollback');
end;

{ TChangeOriginalBPMCommand }

procedure TChangeRealBPMCommand.DoExecute;
begin
  DBLog('start TChangeRealBPMCommand.DoExecute');

  FWavePattern.BeginUpdate;

  // Store RealBPM
  FOldRealBPM := FRealBPM;
  FWavePattern.RealBPM := FRealBPM;

  DBLog(Format('Set BPM: %f', [FRealBPM]));

  FWavePattern.EndUpdate;

  DBLog('end TChangeRealBPMCommand.DoExecute');
end;

procedure TChangeRealBPMCommand.DoRollback;
begin
  DBLog('start TChangeRealBPMCommand.DoRollback');

  FWavePattern.BeginUpdate;

  // Restore RealBPM
  FWavePattern.RealBPM := FOldRealBPM;

  FWavePattern.EndUpdate;

  DBLog('end TChangeRealBPMCommand.DoRollback');
end;

{ TChangeStretchAlgoCommand }

procedure TChangeStretchAlgoCommand.DoExecute;
begin
  DBLog('start TChangeStretchAlgoCommand.DoExecute');

  FWavePattern.BeginUpdate;

  FOldPitchAlgorithm := FPitchAlgorithm;
  FWavePattern.PitchAlgorithm := FPitchAlgorithm;

  FWavePattern.EndUpdate;

  DBLog('end TChangeStretchAlgoCommand.DoExecute');
end;

procedure TChangeStretchAlgoCommand.DoRollback;
begin
  DBLog('start TChangeStretchAlgoCommand.DoRollback');

  FWavePattern.BeginUpdate;

  FWavePattern.PitchAlgorithm := FOldPitchAlgorithm;

  FWavePattern.EndUpdate;

  DBLog('end TChangeStretchAlgoCommand.DoRollback');
end;

end.

