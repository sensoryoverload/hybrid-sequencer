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
unit wave;

//{$fputype sse}
{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Controls, Graphics, LCLType, Forms, ExtCtrls, ctypes, sndfile,
 jacktypes, StdCtrls, Dialogs, Spin, bpmdetect, beattrigger, Utils,
 globalconst, soundtouch, contnrs, global_command,
 ShellCtrls, global, flqueue, math, ringbuffer, pattern, audiothread,
 audiostructure, smbPitchShift, audioutils, stretcher, determinetransients;

const
  MAX_LATENCY = 20000;
  LOOP_FRACTIONAL_MARGE = 1000;
  MAX_STRETCH = 256;
  MIN_STRETCH = 1 / 256;
  QUNATIZE_BAR = 88200;
  QUANTIZE_BEAT = 22050;
  QUANTIZE_4TH = QUNATIZE_BAR div 4;
  QUANTIZE_8TH = QUNATIZE_BAR div 8;
  QUANTIZE_16TH = QUNATIZE_BAR div 16;
  QUANTIZE_32TH = QUNATIZE_BAR div 32;

type
  TQuantizeSettings = (qsNone, qsBar, qsBeat, qs4th, qs8th, qs16th, qs32th);

  TSliceState = (ssCustom, ssAuto);

  { TWavePattern }
  TWavePattern = class(TPattern)
  private
    { Audio }
    FQuantizeSettings: TQuantizeSettings;
    FSensitivity: Single;
    FWorkBuffer: PSingle;
    FConvertBuffer: PSingle;
    FBufferFrames: Integer;
    FSliceList: TObjectList;
    FCurrentSliceIndex: Integer;
    FCurrentSlice: TMarker;
    FVirtualCursorPosition: Integer;
    FSampleStart: TLoopMarker;
    FSampleEnd: TLoopMarker;
    FBarCount: Integer;
    FBarLength: Integer;
    FDragSlice: Boolean;
    FZooming: Boolean;
    FSelectedSlice: TMarker;
    FRubberbandSelect: Boolean;
    FCursorAdder: Single;
    FSampleCursor: Single;
    FCursorRamp: Single;
    FSampleRate: Single;
    FVolumeDecay: Single;
    FWaveFileName: string;
    FWSOLAStretcher: TSoundTouch;
    FTransientThreshold: Integer;
    FWave: TAudioStream;
    FRealBPM: Single;
    FPatternLength: Integer;
    FBPMscale: Single;
    FBPMscaleOld: Single;
    FSampleScale: Single;
    FSampleScaleOld: Single;
    FSampleScaleInverse: Single;

    FSliceState: TSliceState;
    FSliceCounter: Single;
    FSliceCursor: Single;
    FSliceSynced: Boolean;
    FLastSlice: TMarker;

    FPitchAlgorithm: TPitchAlgorithm;

    FSliceStretcher: TStretcher;

    FInterpolationAlgorithm: TInterpolationAlgorithm;

    function CalculateSampleCursor: Boolean;
    procedure GetSampleAtCursor(ASampleCursor: Single; ASourceBuffer: TAudioStream;
      ATargetBuffer: PSingle; AFrameIndex: Integer; AChannelCount: Integer);
    procedure SetCursorRamp(const AValue: Single);
    procedure SetPitchAlgorithm(AValue: TPitchAlgorithm);
    procedure SetRealBPM(const AValue: Single);
    procedure SetWaveFileName(AValue: string);
    function SliceAt(Location: Integer; Margin: single): TMarker;
    procedure SetTransientThreshold(const AValue: Integer);
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
  public
    CalculatedPitch: Single;
    psBeginLocation: Integer;
    psEndLocation: Integer;
    psLastScaleValue: Single;
    StartingSliceIndex: Integer;
    DivideByRealBPM_Multiplier: Single;
    DivideByCursorRamp_Multiplier: Single;

    lFrames: Longint;
    lAudioPlaying: Boolean;
    lFramePacket: TFrameData;
    FLastSliceIndex: Integer;
    lStartingSliceIndex: Integer;
    buffer_size: Integer;

    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Initialize; override;
    procedure Finalize; override;
    function LoadSample(AFileName: string): boolean;
    procedure UnLoadSample;
    function AddSlice(Location, SliceType: Integer; Active: Boolean; Mode: Integer = 0; Margin: single = 5): TMarker;
    function NextSlice: TMarker;
    function LastSlice: TMarker;
    function FirstSlice: TMarker;
    procedure SortSlices;
    procedure RecalculateWarp;
    function LoadSampleInfo: Boolean;
    procedure CalculateLoopMarkers;
    procedure AutoMarkerProcess(ACalculateStatistics: Boolean = True);
    function GetSliceAt(Location: Integer; AMargin: single): TMarker;
    function WarpedLocation(AStartIndex: Integer; ALocation: single; var AFrameData: TFrameData): Boolean;
    function StartOfWarpLocation(ALocation: single): Integer;
    procedure Flush;
    function Latency: Integer; override;
    procedure UpdateBPMScale;
    procedure UpdateSampleScale;

    procedure ProcessInit; override;
    procedure Process(ABuffer: PSingle; AFrameIndex: Integer; AFrameCount: Integer); override;
    procedure ProcessAdvance; override;

    property TimeStretch: TSoundTouch read FWSOLAStretcher write FWSOLAStretcher;
    property Wave: TAudioStream read FWave write FWave;
    property WorkBuffer: Pjack_default_audio_sample_t read FWorkBuffer write FWorkBuffer;
    property CurrentSlice: TMarker read FCurrentSlice write FCurrentSlice;
    property SelectedSlice: TMarker read FSelectedSlice write FSelectedSlice;
    property VirtualCursorPosition: Integer read FVirtualCursorPosition write FVirtualCursorPosition;
    property CurrentSliceIndex: Integer read FCurrentSliceIndex write FCurrentSliceIndex;
    property CursorAdder: Single read FCursorAdder write FCursorAdder;
    property SampleCursor: Single read FSampleCursor write FSampleCursor;
    property CursorRamp: Single read FCursorRamp write SetCursorRamp default 1.0;
    property BPMscale: Single read FBPMscale write FBPMScale;
    property SliceCounter: Single read FSliceCounter write FSliceCounter;
    property SliceCursor: Single read FSliceCursor write FSliceCursor;
    property SliceState: TSliceState read FSliceState write FSliceState;
    property SampleScaleInverse: Single read FSampleScaleInverse write FSampleScaleInverse;
    property BufferFrames: Integer read FBufferFrames write FBufferFrames;
  published
    property SliceList: TObjectList read FSliceList write FSliceList;
    property SampleStart: TLoopMarker read FSampleStart write FSampleStart;
    property SampleEnd: TLoopMarker read FSampleEnd write FSampleEnd;
    property SampleRate: Single read FSampleRate write FSampleRate;
    property VolumeDecay: Single read FVolumeDecay write FVolumeDecay default 1;
    property TransientThreshold: Integer read FTransientThreshold write SetTransientThreshold;
    property BarLength: Integer read FBarLength write FBarLength;
    property BarCount: Integer read FBarCount write FBarCount;
    property RealBPM: Single read FRealBPM write SetRealBPM;
    property PatternLength: Integer read FPatternLength write FPatternLength;
    property PitchAlgorithm: TPitchAlgorithm read FPitchAlgorithm write SetPitchAlgorithm;
    property WaveFileName: string read FWaveFileName write SetWaveFileName;
    property QuantizeSetting: TQuantizeSettings read FQuantizeSettings write FQuantizeSettings;
    property Sensitivity: Single read FSensitivity write FSensitivity;
  end;


implementation

function SortOnLocation(Item1 : Pointer; Item2 : Pointer) : Integer;
var
  location1, location2 : TMidiData;
begin
  // We start by viewing the object pointers as TSlice objects
  location1 := TMidiData(Item1);
  location2 := TMidiData(Item2);

  // Now compare by location
  if location1.Location > location2.Location then
    Result := 1
  else if location1.Location = location2.Location then
    Result := 0
  else
    Result := -1;
end;

function compareByLocation(Item1 : Pointer; Item2 : Pointer) : Integer;
var
  location1, location2 : TMarker;
begin
  // We start by viewing the object pointers as TSlice objects
  location1 := TMarker(Item1);
  location2 := TMarker(Item2);

  // Now compare by location
  if location1.Location > location2.Location then
    Result := 1
  else if location1.Location = location2.Location then
    Result := 0
  else
    Result := -1;
end;

procedure TWavePattern.SetTransientThreshold(const AValue: Integer);
begin
  FTransientThreshold:= AValue;
  AutoMarkerProcess(True);
  Notify;
end;

procedure TWavePattern.DoCreateInstance(var AObject: TObject; AClassName: string);
var
  lMarker: TMarker;
begin
  DBLog('start TWaveForm.DoCreateInstance');

  // create model pattern
  lMarker := TMarker.Create(ObjectID, MAPPED);

  lMarker.ObjectOwnerID := ObjectID;
  AObject := lMarker;

  SliceList.Add(lMarker);

  DBLog('end TWaveForm.DoCreateInstance');
end;

constructor TWavePattern.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  DBLog('start Twaveform.Create');

  inherited Create(AObjectOwner, AMapped);

  FOnCreateInstanceCallback := @DoCreateInstance;

  FSampleStart := TLoopMarker.Create(AObjectOwner, ltStart);
  FSampleEnd := TLoopMarker.Create(AObjectOwner, ltEnd);

  // Initalize settings
  FDragSlice := False;
  FZooming := False;
  FRubberbandSelect := False;
  FCursorAdder := 0;
  FVolumeDecay := 1;
  FSampleScale := 1;

  FSensitivity := 1.5;

  FSliceList := TObjectList.Create(True);
  FCurrentSliceIndex:= 0;

  FLastSliceIndex := -1;

  FWSOLAStretcher := TSoundTouch.Create;
  FWSOLAStretcher.setChannels(2);
  FWSOLAStretcher.setSampleRate(Round(GSettings.SampleRate));
  FWSOLAStretcher.setSetting(SETTING_SEEKWINDOW_MS, 15);
  FWSOLAStretcher.setSetting(SETTING_SEQUENCE_MS, 40);
  FWSOLAStretcher.setSetting(SETTING_OVERLAP_MS, 8);
  FWSOLAStretcher.setSetting(SETTING_USE_QUICKSEEK, 1);
  FWSOLAStretcher.setPitch(1);

  Pitch := 1;

  PitchAlgorithm := paSliceStretch;
  FInterpolationAlgorithm := iaNone; //iaHermite gives problems;

  FSliceStretcher := TStretcher.Create(Round(GSettings.SampleRate));
  FSliceStretcher.InterpolationAlgorithm := FInterpolationAlgorithm;
  FSliceStretcher.SliceList := FSliceList;
  FSliceStretcher.OverlapLengthMs := 8;
  FSliceStretcher.SeekwindowMs := 15;
  FSliceStretcher.SequencewindowMs := 80;

  Getmem(FWorkBuffer, Round(GSettings.SampleRate * 2));
  Getmem(FConvertBuffer, Round(GSettings.SampleRate) * SizeOf(Single));

  QuantizeSetting := qsBeat;

  DBLog('end Twaveform.Create');
end;

destructor TWavePattern.Destroy;
begin
  if Assigned(FSliceList) then
    FSliceList.Free;
  if Assigned(FWorkBuffer) then
    FreeMem(FWorkBuffer);
  if Assigned(FWSOLAStretcher) then
    FWSOLAStretcher.Free;
  if Assigned(FWave) then
    FWave.Free;
  if Assigned(FSampleStart) then
    FSampleStart.Free;
  if Assigned(FSampleEnd) then
    FSampleEnd.Free;

  FSliceStretcher.Free;

  Freemem(FConvertBuffer);

  inherited Destroy;
end;

procedure TWavePattern.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TWavePattern.Initialize;
begin
  inherited Initialize;

  BeginUpdate;

{  if FWaveFileName <> '' then
  begin
    LoadSample(FWaveFileName);
  end;}

  EndUpdate;
end;

procedure TWavePattern.Finalize;
begin
  UnLoadSample;

  inherited Finalize;
end;

function TWavePattern.LoadSample(AFilename: string): Boolean;
var
  lWaveBuffer: PSingle;
  i, j: Integer;
  SamplesRead: Integer;
  lValue: single;
  lBPMDetect: TBPMDetect;
begin
  DBLog('Start LoadSample: ' + AFilename);

  Result := False;
  DBLog('Loading: ' + AFilename);

  FWave := GAudioStreamListSingleton.CreateStream(AFilename);

  ChannelCount := FWave.ChannelCount;

  if PitchAlgorithm in [paSoundTouch, paSoundTouchEco] then
  begin
    // Always 2 channels
    TimeStretch.setChannels(2);
  end;

  WaveFileName := AFileName;

  if FWave.FrameCount > 0 then
  begin
    // Timestretch latency makes the cursor start at an offset and go paSoundTouch the normal end of the sample

    FBufferFrames := FWave.FrameCount;

    AddSlice(0, SLICE_UNDELETABLE, True);
    AddSlice(FWave.FrameCount, SLICE_UNDELETABLE, True);

    CurrentSlice:= TMarker(SliceList[0]);
    CurrentSliceIndex:= 0;

    // Init default values
    CursorAdder:= 0;

    AutoMarkerProcess(True);

    UpdateSampleScale;
  end;

  Notify;

  DBLog('start TWaveForm.LoadSampleData');
end;

procedure TWavePattern.UnLoadSample;
var
  lMarkerIndex: Integer;
begin
  DBLog('UnLoadSample 2');
  // Removes a sample from the waveform pattern
  for lMarkerIndex := Pred(FSliceList.Count) downto 0 do
  begin
    DBLog('Removing marker: ' + TMarker(FSliceList[lMarkerIndex]).ObjectID);
    FSliceList.Delete(lMarkerIndex);
  end;
end;

function TWavePattern.AddSlice(Location,
  Slicetype: Integer; Active: Boolean; Mode: Integer = 0; Margin: single = 5): TMarker;
var
  lWaveSlice: TMarker;
  lSliceX1: TMarker;
  lSliceX2: TMarker;
  lOrgLocation: Integer;
  i: Integer;
  lSliceFound: Boolean;
begin
  DBLog('start Twaveform.AddSlice');

  Result := nil;
  // If there's already a Slice on this spot then edit that Slice
  // Find Slice at Location and if found, remove and free resources
  lSliceFound := False;
  for i := 0 to Pred(FSliceList.Count) do
  begin
    lWaveSlice := TMarker(FSliceList[i]);
    if Abs(Location - lWaveSlice.Location) < Margin then
    begin
      lSliceFound := True;
      break;
    end;
  end;

  if not lSliceFound then
  begin
    // Calculate where the slice is located on the read unwarped sample
    if FSliceList.Count > 1 then
    begin
      for i := 0 to FSliceList.Count - 2 do
      begin
        if (Location > TMarker(FSliceList[i]).Location) and (Location < TMarker(FSliceList[i + 1]).Location) then
        begin
          lSliceX1:= TMarker(FSliceList.Items[i]);
          lSliceX2:= TMarker(FSliceList.Items[i + 1]);

          lOrgLocation :=
            Round(
            ((Location - lSliceX1.Location) / (lSliceX2.Location - lSliceX1.Location)) *
            (lSliceX2.OrigLocation - lSliceX1.OrigLocation) + lSliceX1.OrigLocation);

          break;
        end;
      end;
    end
    else
    begin
      lOrgLocation := Location;
    end;

    // Place Slice of SliceType in list at Location with state Active
    lWaveSlice := TMarker.Create(ObjectID, MAPPED);
    FSliceList.Add(lWaveSlice);
    lWaveSlice.Location:= Location;
    lWaveSlice.SliceType:= Slicetype;
    lWaveSlice.Active:= Active;
    lWaveSlice.Selected:= False;
    lWaveSlice.DecayRate:= 1;
    lWaveSlice.PitchRate := 1;
    lWaveSlice.OrigLocation:= lOrgLocation;
    lWaveSlice.Locked:= False;

    DBLog('Create Marker at %d', lWaveSlice.Location);

    // Sort it as we are always going from left to right through the list
    Sortslices;

    // Update observers
    Notify;
  end
  else
  begin
    case Mode of
      0:
      begin
        // Slice found, moving location
        FDragSlice := True;
      end;
      1:
      begin
        lWaveSlice.Locked := not lWaveSlice.Locked;
      end;
    end;

    SortSlices;

    // Update observers
    Notify;
  end;

  Result := lWaveSlice;

  DBLog('end Twaveform.AddSlice');
end;

function TWavePattern.SliceAt(Location: Integer; Margin: single): TMarker;
var
  i: Integer;
  lSlice: TMarker;
begin
  // Find Slice at Location
  Result := nil;
  for i := 0 to Pred(FSliceList.Count) do
  begin
    lSlice:= TMarker(FSliceList.Items[i]);
    if Abs(Location - lSlice.Location) < Margin then
      if lSlice.SliceType <> SLICE_UNDELETABLE then
      begin
        Result := lSlice;
        break;
      end;
  end;
end;

procedure TWavePattern.SetRealBPM(const AValue: Single);
begin
  if FRealBPM = AValue then exit;
  FRealBPM := AValue;

  if FRealBPM = 0 then
  begin
    FRealBPM := 120;
  end;
  DivideByRealBPM_Multiplier := 1 / FRealBPM;
end;

procedure TWavePattern.SetWaveFileName(AValue: string);
begin
  if FWaveFileName = AValue then Exit;
  FWaveFileName := AValue;

  LoadSample(FWaveFileName);
end;

procedure TWavePattern.SetCursorRamp(const AValue: Single);
begin
  if FCursorRamp = AValue then exit;
  FCursorRamp := AValue;

  if FCursorRamp = 0 then
  begin
    FCursorRamp := 1;
  end;
end;

procedure TWavePattern.setPitchAlgorithm(AValue: TPitchAlgorithm);
begin
  if FPitchAlgorithm = AValue then Exit;

  case AValue of
  paSoundTouchEco:
    begin
      TimeStretch.setSetting(SETTING_USE_QUICKSEEK, 1);
      TimeStretch.setSetting(SETTING_XCORR_FFT, 0);
      TimeStretch.setSetting(SETTING_SEQUENCE_MS, 40);
      TimeStretch.setSetting(SETTING_SEEKWINDOW_MS, 15);
      TimeStretch.setSetting(SETTING_OVERLAP_MS, 8);
    end;
  paSoundTouch:
    begin
      TimeStretch.setSetting(SETTING_USE_QUICKSEEK, 0);
      TimeStretch.setSetting(SETTING_XCORR_FFT, 1);
      TimeStretch.setSetting(SETTING_SEQUENCE_MS, 40);
      TimeStretch.setSetting(SETTING_SEEKWINDOW_MS, 15);
      TimeStretch.setSetting(SETTING_OVERLAP_MS, 8);
    end;
  else
    begin
      FPitchAlgorithm := paNone;
    end;
  end;

  // Wait until here until setting it as the jack callback might
  // check state earlier
  FPitchAlgorithm := AValue;
end;

procedure TWavePattern.Flush;
begin
  case FPitchAlgorithm of
    paSoundTouch, paSoundTouchEco:
    begin
      TimeStretch.flush;
    end;
  end;
end;

function TWavePattern.Latency: Integer;
begin
  case PitchAlgorithm of
    paSoundTouch, paSoundTouchEco:
    begin
      Result := TimeStretch.latency;
    end;
    paPitched, paSliceStretch:
    begin
      Result := 0;
    end;
  end;

  Result := Result + PluginProcessor.Latency;
end;

function TWavePattern.NextSlice: TMarker;
var
  lMarker: TMarker;
begin
  Result := nil;

  if FCurrentSliceIndex < FSliceList.Count then
    lMarker := TMarker(FSliceList.Items[FCurrentSliceIndex])
  else
    lMarker := TMarker(FSliceList.Last);

  if Assigned(lMarker) then
    Result := lMarker.NextSlice
  else
    Result := nil;
end;

function TWavePattern.LastSlice: TMarker;
begin
  result := TMarker(FSliceList.Last);
end;

function TWavePattern.FirstSlice: TMarker;
begin
  result := TMarker(FSliceList.First);
end;

procedure TWavePattern.Sortslices;
var
  i: Integer;
begin
  // Link all sorted
  FSliceList.Sort(@compareByLocation);
  for i := 0 to FSliceList.Count - 2 do
  begin
    if i = 0 then
    begin
      TMarker(FSliceList.Items[i]).PrevSlice:= nil;
    end;
    if (i + 1) <= FSliceList.Count then
    begin
      TMarker(FSliceList.Items[i]).NextSlice := TMarker(FSliceList.Items[i + 1]);
      TMarker(FSliceList.Items[i + 1]).PrevSlice := TMarker(FSliceList.Items[i]);
    end
    else
      TMarker(FSliceList.Items[i]).NextSlice:= nil;
  end;

  RecalculateWarp;
end;

procedure TWavePattern.RecalculateWarp;
var
  i: Integer;

  function CalculateNominalSliceLength(ALength: Single): Single;
  var
    lBaseNominalLength: Single;
    lCalculatedNominalLength: Single;
  begin
    lBaseNominalLength := GSettings.SampleRate / 8;
    lCalculatedNominalLength := ALength;

    while lCalculatedNominalLength > lBaseNominalLength do
    begin
      lCalculatedNominalLength := lCalculatedNominalLength / 2;
    end;

    Result := lCalculatedNominalLength;
  end;

begin
  for i := 0 to FSliceList.Count - 2 do
  begin
    TMarker(FSliceList.Items[i]).DecayRate :=
    (TMarker(FSliceList.Items[i + 1]).OrigLocation - TMarker(FSliceList.Items[i]).OrigLocation) /
    (TMarker(FSliceList.Items[i + 1]).Location - TMarker(FSliceList.Items[i]).Location);

    TMarker(FSliceList.Items[i]).PitchRate :=
      1 / TMarker(FSliceList.Items[i]).DecayRate;

    TMarker(FSliceList.Items[i]).Length :=
      CalculateNominalSliceLength(
        TMarker(FSliceList.Items[i + 1]).Location -
        TMarker(FSliceList.Items[i]).Location);


    if i = FSliceList.Count - 2 then
    begin
      TMarker(FSliceList.Items[i]).Length :=
        CalculateNominalSliceLength(
          TMarker(FSliceList.Last).Location -
          TMarker(FSliceList.First).Location);
    end;

    DBLog(Format('Decayrate %f modlength %f distance %d', [
        TMarker(FSliceList.Items[i]).DecayRate,
        TMarker(FSliceList.Items[i]).Length,
        TMarker(FSliceList.Items[i + 1]).Location -
        TMarker(FSliceList.Items[i]).Location]));
  end;
end;

function TWavePattern.LoadSampleInfo: Boolean;
begin
  Result := True;
end;

function TWavePattern.GetSliceAt(Location: Integer; AMargin: single): TMarker;
var
  i: Integer;
  lSlice: TMarker;
begin
  Result := TMarker(FSliceList.First);
  for i := 0 to Pred(FSliceList.Count) do
  begin
    lSlice:= TMarker(FSliceList.Items[i]);
    if Abs(Location - lSlice.Location) < AMargin then
    begin
      Result:= lSlice;
      FCurrentSliceIndex:= i;
      break;
    end;
  end;
end;

{
  AStartIndex: The index at which you should start, cached by StartVirtualLocation
  ALocation: Location in the timeline
  AFrameData: Structure returned with info about the sampleframe

}
function TWavePattern.WarpedLocation(AStartIndex: Integer; ALocation: single; var AFrameData: TFrameData): Boolean;
var
  i: Integer;
  lSliceStart: TMarker;
  lSliceEnd: TMarker;
begin
  Result := True;

  AFrameData.Location := ALocation;
  AFrameData.Ramp := 1;

  for i := AStartIndex to FSliceList.Count - 2 do
  begin
    lSliceStart := TMarker(FSliceList.Items[i]);
    lSliceEnd := TMarker(FSliceList.Items[i + 1]);

    if (ALocation >= lSliceStart.Location) and (ALocation < lSliceEnd.Location) then
    begin
      if PitchAlgorithm = paPitched then
      begin
        // This algorithm just cuts off the sound giving gaps between te slices when
        AFrameData.Location :=
          lSliceStart.OrigLocation +
          (Pitch * GAudioStruct.BPMScaleInv * (ALocation - lSliceStart.Location));

        // Return false if cursor past end of slice
        Result := (AFrameData.Location < lSliceEnd.Location);
      end
      else
      begin
        AFrameData.Location :=
          lSliceStart.OrigLocation +
          (lSliceStart.DecayRate * (ALocation - lSliceStart.Location));
      end;

      AFrameData.Ramp := lSliceStart.DecayRate;
      AFrameData.Pitch := lSliceStart.PitchRate;
      AFrameData.Index := i;
      break;
    end;
  end;
end;


{
  Looks for the index of the current slice and can be used to provide the startindex
  for the method VirtualLocation. This prevents a big loop every sample when there are
  lots of warp markers.
}
function TWavePattern.StartOfWarpLocation(ALocation: single): Integer;
var
  i: Integer;
  lSliceStart: TMarker;
  lSliceEnd: TMarker;
begin
  for i := 0 to FSliceList.Count - 2 do
  begin
    lSliceStart := TMarker(FSliceList.Items[i]);
    lSliceEnd := TMarker(FSliceList.Items[i + 1]);

    if (ALocation >= lSliceStart.Location) and (ALocation < lSliceEnd.Location) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TWavePattern.CalculateSampleCursor: Boolean;
begin
  Result := (PatternCursor >= FSampleStart.Value) and (PatternCursor < FSampleEnd.Value);
  if Result then
  begin
    FSampleCursor := (PatternCursor - FSampleStart.Value) * FSampleScale;
  end;
end;

procedure TWavePattern.UpdateSampleScale;
begin
  FSampleScale := FWave.FrameCount / (FSampleEnd.Value - FSampleStart.Value);
  FSampleScaleInverse := (FSampleEnd.Value - FSampleStart.Value) / FWave.FrameCount;

  DBLog(Format('FSampleScale %f FSampleScaleInverse %f', [FSampleScale, FSampleScaleInverse]));
end;

procedure TWavePattern.UpdateBPMScale;
begin
  FBPMscale := GAudioStruct.BPM * DivideByRealBPM_Multiplier;
  if FBPMscale > 100 then
    FBPMscale := 100
  else if FBPMscale < 0.01 then
    FBPMscale := 0.01;
end;

procedure TWavePattern.ProcessInit;
begin
  UpdateBPMScale;

  psBeginLocation := 0;
end;

{
  Get a sample out of the buffer and interpolate it
}
procedure TWavePattern.GetSampleAtCursor(ASampleCursor: Single;
  ASourceBuffer: TAudioStream; ATargetBuffer: PSingle; AFrameIndex: Integer; AChannelCount: Integer);
var
  lFracPosition: Single;
  lBufferOffset: integer;
begin
  lFracPosition := Frac(ASampleCursor);
  lBufferOffset := Round(ASampleCursor{ * AChannelCount});

  if AChannelCount = 1 then
  begin
{    ATargetBuffer[AFrameIndex * 2] := hermite4(
      lFracPosition,
      ifthen(lBufferOffset <= 1, 0, ASourceBuffer[lBufferOffset - 1]),
      ASourceBuffer[lBufferOffset],
      ASourceBuffer[lBufferOffset + 1],
      ASourceBuffer[lBufferOffset + 2]);
    ATargetBuffer[AFrameIndex * 2] := ASourceBuffer[lBufferOffset];}

    // Make dual mono
    ATargetBuffer[AFrameIndex * 2] := ASourceBuffer.Audio(lBufferOffset);
    ATargetBuffer[AFrameIndex * 2 + 1] := ASourceBuffer.Audio(lBufferOffset);
  end
  else
  begin
{    ATargetBuffer[AFrameIndex * 2] := hermite4(
      lFracPosition,
      ifthen(lBufferOffset <= 1, 0, ASourceBuffer[lBufferOffset - 2]),
      ASourceBuffer[lBufferOffset],
      ASourceBuffer[lBufferOffset + 2],
      ASourceBuffer[lBufferOffset + 4]);

    ATargetBuffer[AFrameIndex * 2 + 1] := hermite4(
      lFracPosition,
      ifthen(lBufferOffset + 1 <= 2, 0, ASourceBuffer[lBufferOffset - 1]),
      ASourceBuffer[lBufferOffset + 1],
      ASourceBuffer[lBufferOffset + 3],
      ASourceBuffer[lBufferOffset + 5]);}

    ATargetBuffer[AFrameIndex * 2] := ASourceBuffer.Audio(lBufferOffset);
    // !!!! Todo This should take the right sample instead of the left sample
    ATargetBuffer[AFrameIndex * 2 + 1] := ASourceBuffer.Audio(lBufferOffset);
  end;
end;

procedure TWavePattern.Process(ABuffer: PSingle; AFrameIndex: Integer; AFrameCount: Integer);
var
  lInSample: Boolean;
  lLoopingFramePacket: TFrameData;
begin
  try
    if AFrameIndex = 0 then
    begin
      psLastScaleValue := CursorRamp;
    end;

    lInSample := CalculateSampleCursor;

    // Fetch frame data packet containing warp factor and virtual location in wave data
    if AFrameIndex = 0 then
    begin
      StartingSliceIndex := StartOfWarpLocation(FSampleCursor);
    end;

    if lInSample then
    begin
      if PitchAlgorithm = paSliceStretch then
      begin
        FSliceStretcher.Pitch := Pitch;
        FSliceStretcher.Tempo := GAudioStruct.BPMScale;
        FSliceStretcher.SampleScale := FSampleScale;
        FSliceStretcher.Process(StartingSliceIndex, FSampleCursor, FSliceCounter, FWave, ABuffer, AFrameIndex, FWave.ChannelCount);
      end
      else
      begin
        if WarpedLocation(StartingSliceIndex, FSampleCursor, lFramePacket) then
        begin
          CursorAdder := lFramePacket.Location;
          CursorRamp := lFramePacket.Ramp;

          if Trunc(CursorAdder) < (Wave.FrameCount * FWave.ChannelCount) then
          begin
            GetSampleAtCursor(CursorAdder, FWave, WorkBuffer, AFrameIndex, Wave.ChannelCount);

            // Scale buffer up to now when the scaling-factor has changed or
            // the end of the buffer has been reached.
            if PitchAlgorithm in [paSoundTouch, paSoundTouchEco] then
            begin
              if (CursorRamp <> psLastScaleValue) or
                (FBPMscaleOld <> FBPMscale) or
                (FSampleScaleOld <> FSampleScale) or
                (FLastSliceIndex <> lFramePacket.Index) or
                (AFrameIndex = (AFrameCount - 1)) then
              begin
                psEndLocation := AFrameIndex;
                CalculatedPitch := Pitch * ((1 / (CursorRamp * FBPMscale)) / FSampleScale);
                if CalculatedPitch > MAX_STRETCH then CalculatedPitch := MAX_STRETCH;
                if CalculatedPitch < MIN_STRETCH then CalculatedPitch := MIN_STRETCH;

                // Frames to process this window
                lFrames := (psEndLocation - psBeginLocation) + 1;

                DBLog(Format('CalculatedPitch %f lFrames %d', [CalculatedPitch, lFrames]));

                case PitchAlgorithm of
                  paSoundTouch, paSoundTouchEco:
                  begin
                    TimeStretch.setPitch(CalculatedPitch);
                    TimeStretch.PutSamples(@WorkBuffer[psBeginLocation], lFrames);
                    TimeStretch.ReceiveSamples(@ABuffer[psBeginLocation], lFrames);
                  end;
                end;

                // Remember last change
                psBeginLocation := AFrameIndex;
                psLastScaleValue := CursorRamp;
                FBPMscaleOld := FBPMscale;
                FSampleScaleOld := FSampleScale;
              end;
            end
            else if PitchAlgorithm in [paPitched] then
            begin
              // Copy stereo pair
              ABuffer[AFrameIndex * 2] := WorkBuffer[AFrameIndex * 2];
              ABuffer[AFrameIndex * 2 + 1] := WorkBuffer[AFrameIndex * 2 + 1];
            end;
          end
          else
          begin
            ABuffer[AFrameIndex * 2] := 0;
            ABuffer[AFrameIndex * 2 + 1] := 0;
          end;
        end;
      end;

      FLastSliceIndex := lFramePacket.Index;
    end
    else
    begin
      ABuffer[AFrameIndex * 2] := 0;
      ABuffer[AFrameIndex * 2 + 1] := 0;
    end;
  except
    on e:exception do
    begin
      DBLog(Format('TWavePattern.Process %s FrameIndex: %d' , [e.Message, AFrameIndex]));
    end;
  end;
end;

procedure TWavePattern.ProcessAdvance;
begin
  inherited;

  UpdateBPMScale;

  if Looped then
  begin
    Looped := False;
    FLastSlice := TMarker(SliceList.Last);
    FSliceCounter := 0;
    FSliceSynced := True;
    Flush;
  end;
end;

procedure TWavePattern.CalculateLoopMarkers;
begin
  // Is loop already set ?
  if RealBPM = 0 then
  begin
    RealBPM := 120;
  end
  else
  begin
    if RealBPM < 90 then
    begin
      RealBPM := RealBPM * 2;
      if RealBPM < 90 then
      begin
        RealBPM := RealBPM * 2;
      end;
    end
    else if RealBPM > 190 then
    begin
      RealBPM := RealBPM / 2;
      if RealBPM > 190 then
      begin
        RealBPM := RealBPM / 2;
      end;
    end;
  end;

  // Determine bars
  BarCount := FWave.FrameCount div (2 * FWave.SampleRate);

  // BarCount should be at least 1 else it will appear as a blank pattern
  if BarCount < 1 then
  begin
    BarCount := 1;
  end;

  BarLength := BarCount * FWave.SampleRate * FWave.ChannelCount * 2;

  LoopStart.Value := 0;
  LoopEnd.Value := BarLength;

  LoopLength.Value := LoopEnd.Value - LoopStart.Value;

  SampleStart.Value := 0;
  SampleEnd.Value := LoopEnd.Value;

  //FWave.LoopStart := SampleStart.Value;
  //FWave.LoopEnd := SampleEnd.Value;

  writeln(Format('FWave.Frames %d BarCount %d, BarLength %d, LoopLength.Value %d SampleEnd.Value %d FWave.SampleRate %d FWave.ChannelCount %d',
    [FWave.FrameCount, BarCount, BarLength, LoopLength.Value, SampleEnd.Value, FWave.SampleRate, FWave.ChannelCount]));

  UpdateSampleScale;

  // Update observers
  Notify;
  SampleStart.Notify;
  SampleEnd.Notify;
end;

procedure TWavePattern.AutoMarkerProcess(ACalculateStatistics: Boolean = True);
var
  i: Integer;
  WindowLength: Integer;
  lCurrentSlice: TMarker;
  lFirstSlice: Boolean;
  lDetermineTransients: TDetermineTransients;
  lDoAddSlice: Boolean;
begin
  lFirstSlice := True;
  lCurrentSlice := TMarker(FSliceList[0]);

  WindowLength := 0;

  // First remove old slices
  for i := Pred(FSliceList.Count) downto 0 do
  begin
    if TMarker(FSliceList[i]).SliceType = SLICE_VIRTUAL then
    begin
      FSliceList.Delete(i);
    end;
  end;

  SortSlices;

  lDoAddSlice := True;
  for i := Low(FWave.AudioPeak.TransientMarkers) to High(FWave.AudioPeak.TransientMarkers) do
  begin
    if i > 0 then
    begin
      lDoAddSlice := FWave.AudioPeak.TransientMarkers[i] - FWave.AudioPeak.TransientMarkers[i - 1] > (GSettings.SampleRate * 0.125);
    end;
    if lDoAddSlice then
    begin
      AddSlice(FWave.AudioPeak.TransientMarkers[i], SLICE_VIRTUAL, True);
    end;
  end;
end;

initialization
finalization

end.
