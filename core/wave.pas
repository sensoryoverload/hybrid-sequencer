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

{$fputype sse}

interface

uses
 Classes, SysUtils, Controls, Graphics, LCLType, Forms, ExtCtrls, ctypes, sndfile,
 jacktypes, StdCtrls, Dialogs, Spin, bpmdetect, beattrigger, Utils,
 globalconst, soundtouch, contnrs, global_command,
 ShellCtrls, global, flqueue, math, ringbuffer, pattern,
 audiostructure, smbPitchShift, audioutils, stretcher, determinetransients;

const
  MAX_LATENCY = 20000;
  DECIMATED_CACHE_DISTANCE = 64;
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
  TSliceDirection = (sdIdle, sdForward, sdReverse);

  TRingBufferData = class(TObject)
  private
    FData: Pointer;
    FSize: Integer;
    FInBuffer: Boolean;
  public
    property Data: pointer read FData;
    property Size: Integer read FSize write FSize;
    property InBuffer: Boolean read FInBuffer write FInBuffer;
  end;


  { TDiskWriterThread }

  TDiskWriterThread = class(TThread)
  private
    FFileStream: TFileStream;
    FBufferOffset: Integer;
    FRingBuffer: pjack_ringbuffer_t;
    FTempName: string;
    FBuffer: psingle;
    FBufferSize: Integer;

    SoundFileOut: PSndFile;
    InfoOut: SF_INFO;

    procedure Updater;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean);
    destructor Destroy; override;

    procedure RingbufferWrite(ABuffer: jack_default_audio_sample_t; ASize: Integer);
  end;

  { TDiskReaderThread }

  TDiskReaderThread = class(TThread)
  private
    FFileStream: TFileStream;
    FBufferOffset: Integer;
    FRingBuffer: pjack_ringbuffer_t;
    FTempName: string;
    FBuffer: psingle;
    FBufferSize: Integer;

    SoundFileIn: PSndFile;
    InfoIn: SF_INFO;

    procedure Updater;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean);
    destructor Destroy; override;

    procedure RingbufferReader(ABuffer: jack_default_audio_sample_t; ASize: Integer);
  end;

  { TSliceLooper }

  TSliceLooper = class
  private
    FPitch: Single;
    FSliceEnd: Single;
    FSliceStart: Single;
    FCursor: Single;
    FSliceHalfLength: Integer;
    procedure SetSliceEnd(AValue: Single);
    procedure SetSliceStart(AValue: Single);
  public
    function Process: Single;
    property SliceStart: Single read FSliceStart write SetSliceStart;
    property SliceEnd: Single read FSliceEnd write SetSliceEnd;
    property Cursor: Single read FCursor write FCursor;
    property Pitch: Single read FPitch write FPitch;
  end;

  { TSliceStretcher }

  TSliceStretcher = class
  private
    FPitch: Single;
    FSampleScale: Single;
    FSliceList: TObjectList;
    FSliceStartLocation: single;
    FSliceEndLocation: single;
    FSliceHalfLength: Integer;
    FSliceLastCounter: Single;
    FSliceCursor: Single;
    FSliceLoopModulo: Integer;
    FSliceFadeIn: Single;
    FSliceFadeOut: Single;
    FSliceLooper: TSliceLooper;
    FInterpolationAlgorithm: TInterpolationAlgorithm;
    procedure GetSample(
      ACursor: Single;
      ASourceBuffer: PSingle;
      var ALeftValue: Single;
      var ARightValue: Single;
      AChannelCount: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Process(
      AStartIndex: Integer;
      var ASampleCursor: Single;
      var ASliceCounter: Single;
      ASourceBuffer: PSingle;
      ATargetBuffer: PSingle;
      AFrameIndex: Integer;
      AChannelCount: Integer);
    property InterpolationAlgorithm: TInterpolationAlgorithm read FInterpolationAlgorithm write FInterpolationAlgorithm;
    property SliceList: TObjectList read FSliceList write FSliceList;
    property Pitch: Single read FPitch write FPitch;
    property SampleScale: Single read FSampleScale write FSampleScale;
  end;

  { TWavePattern }
  TWavePattern = class(TPattern)
  private
    { Audio }
    FDecimatedData: PSingle;
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
    FFFTStretcher: TSmbPitchShifter;
    FTransientThreshold: Integer;
    FBufferDataSize: PtrUInt;
    FWave: TWaveFile;
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

//    FDiskWriterThread: TDiskWriterThread;
    FDiskReaderThread: TDiskReaderThread;
    FPitchAlgorithm: TPitchAlgorithm;

    FSliceStretcher: TStretcher; //TSliceStretcher;

    FInterpolationAlgorithm: TInterpolationAlgorithm;

    function CalculateSampleCursor: Boolean;
    procedure GetSampleAtCursor(ASampleCursor: Single; ASourceBuffer: PSingle;
      ATargetBuffer: PSingle; AFrameIndex: Integer; AChannelCount: Integer);
    procedure SetCursorRamp(const AValue: Single);
    procedure SetPitchAlgorithm(AValue: TPitchAlgorithm);
    procedure SetRealBPM(const AValue: Single);
    procedure SetWaveFileName(AValue: string);
    function SliceAt(Location: Integer; Margin: single): TMarker;
    procedure SetTransientThreshold(const AValue: Integer);
    procedure UpdateBPMScale;
    procedure UpdateSampleScale;
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
    lBuffer: PSingle;
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
    function WarpedLocation(AStartIndex: Integer; ALocation: single; var AFrameData: TFrameData; var ALoopingFrameData: TFrameData; ABuffer: PSingle): Boolean;
    function StartOfWarpLocation(ALocation: single): Integer;
    procedure Flush;
    function Latency: Integer; reintroduce;

    procedure ProcessInit; override;
    procedure Process(ABuffer: PSingle; AFrameIndex: Integer; AFrameCount: Integer); override;
    procedure ProcessAdvance; override;

    property TimeStretch: TSoundTouch read FWSOLAStretcher write FWSOLAStretcher;
    property Wave: TWaveFile read FWave write FWave;
    property WorkBuffer: Pjack_default_audio_sample_t read FWorkBuffer write FWorkBuffer;
    property CurrentSlice: TMarker read FCurrentSlice write FCurrentSlice;
    property SelectedSlice: TMarker read FSelectedSlice write FSelectedSlice;
    property DecimatedData: PSingle read FDecimatedData write FDecimatedData;
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
//    property DiskWriterThread: TDiskWriterThread read FDiskWriterThread;
    property DiskReaderThread: TDiskReaderThread read FDiskReaderThread;
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

{ TSliceLooper }

procedure TSliceLooper.SetSliceEnd(AValue: Single);
begin
  FSliceEnd := AValue;

  FSliceHalfLength := Round(FSliceEnd - FSliceStart) div 2;
end;

procedure TSliceLooper.SetSliceStart(AValue: Single);
begin
  FSliceStart := AValue;

  FSliceHalfLength := Round(FSliceEnd - FSliceStart) div 2;
end;

function TSliceLooper.Process: Single;
var
  lSliceLoopModulo: Integer;
begin
  // Ran past end of slice so loop a part of the previous section
  if FSliceHalfLength <> 0 then
  begin
    lSliceLoopModulo := Round(FCursor - FSliceEnd) mod FSliceHalfLength;
    if Odd(Round(FCursor - FSliceEnd) div FSliceHalfLength) then
    begin
      Result := Round(FSliceEnd - FSliceHalfLength) + lSliceLoopModulo;
    end
    else
    begin
      Result := Round(FSliceEnd) - lSliceLoopModulo;
    end;
  end;
  FCursor := FCursor + FPitch;
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

  FWave := TWaveFile.Create(AObjectOwner, True);
  FWave.BufferFormat := bfInterleave;


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

  FFFTStretcher := TSmbPitchShifter.Create;
  FFFTStretcher.FFTFrameSize := 512;
  FFFTStretcher.OverSampling := 8;
  FFFTStretcher.SampleRate := GSettings.SampleRate;
  FFFTStretcher.Initialize;

  PitchAlgorithm := paSliceStretch;
  FInterpolationAlgorithm := {iaNone;//}iaHermite;

  FSliceStretcher := TStretcher.Create(Round(GSettings.SampleRate)); //TSliceStretcher.Create;
  FSliceStretcher.InterpolationAlgorithm := FInterpolationAlgorithm;
  FSliceStretcher.SliceList := FSliceList;
  FSliceStretcher.OverlapLengthMs := 8;
  FSliceStretcher.SeekwindowMs := 15;
  FSliceStretcher.SequencewindowMs := 80;

  Getmem(FWorkBuffer, Round(GSettings.SampleRate * 2));
  Getmem(FConvertBuffer, Round(GSettings.SampleRate) * SizeOf(Single));
//  FDiskWriterThread := TDiskWriterThread.Create(False);
//  FDiskWriterThread.FreeOnTerminate := True;
//  FDiskReaderThread := TDiskReaderThread.Create(False);
//FDiskReaderThread.FreeOnTerminate := True;

  QuantizeSetting := qsBeat;

  DBLog('end Twaveform.Create');
end;

destructor TWavePattern.Destroy;
begin
  if Assigned(FSliceList) then
    FSliceList.Free;
  if Assigned(FWorkBuffer) then
    FreeMem(FWorkBuffer);
  if Assigned(FDecimatedData) then
    Freemem(FDecimatedData);
  if Assigned(FWSOLAStretcher) then
    FWSOLAStretcher.Free;
  if Assigned(FWave) then
    FWave.Free;
{  if Assigned(FDiskWriterThread) then
    FDiskWriterThread.Free;}
//  if Assigned(FDiskReaderThread) then
//    FDiskReaderThread.Free;
  if Assigned(FSampleStart) then
    FSampleStart.Free;
  if Assigned(FSampleEnd) then
    FSampleEnd.Free;

  FFFTStretcher.Free;

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

  if FWaveFileName <> '' then
  begin
    LoadSample(FWaveFileName);
  end;

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
  FWave.BufferFormat := bfInterleave;
  FWave.LoadSample(AFilename);

  ChannelCount := FWave.ChannelCount;

  if PitchAlgorithm in [paSoundTouch, paSoundTouchEco] then
  begin
    // Always 2 channels
    TimeStretch.setChannels(2);
  end;

  WaveFileName := AFileName;

  if Assigned(FDecimatedData) then
    Freemem(FDecimatedData);

  FBufferDataSize:= FWave.DataSize;

  if FBufferDataSize > 0 then
  begin
    // Timestretch latency makes the cursor start at an offset and go paSoundTouch the normal end of the sample

    FBufferDataSize := FBufferDataSize + MAX_LATENCY;

    GetMem(FDecimatedData, FBufferDataSize div DECIMATED_CACHE_DISTANCE);
    FBufferFrames := FWave.Frames;
    DBLog(Format('FramesCount %d', [FBufferFrames]));

    AddSlice(0, SLICE_UNDELETABLE, True);
    AddSlice(FWave.Frames, SLICE_UNDELETABLE, True);

    CurrentSlice:= TMarker(SliceList[0]);
    CurrentSliceIndex:= 0;

    // Init default values
    CursorAdder:= 0;

    // Normalize sample in-place
    NormalizeInMemorySample(TChannel(FWave.ChannelList[0]).Buffer, FWave.ReadCount);

    // Build decimated buffer for fast displaying in gui where
    // a high resolution is not of high importance
    for i := 0 to (FWave.ReadCount div DECIMATED_CACHE_DISTANCE) - 1 do
    begin
      lWaveBuffer := TChannel(FWave.ChannelList[0]).Buffer;
      lValue:= 0;
      for j := 0 to Pred(DECIMATED_CACHE_DISTANCE) do
      begin
        lValue:= lValue + lWaveBuffer[i * DECIMATED_CACHE_DISTANCE + j];
      end;
      DecimatedData[i] := lValue / DECIMATED_CACHE_DISTANCE;
    end;

    // Calculated BPM
    lBPMDetect := TBPMDetect.Create(FWave.ChannelCount, FWave.SampleRate);
    try
      try
        SamplesRead := 0;
        for i := 0 to (FWave.ReadCount div DECIMATED_BLOCK_SAMPLES) - 1 do
        begin
          lBPMDetect.inputSamples(TChannel(FWave.ChannelList[0]).Buffer + i * DECIMATED_BLOCK_SAMPLES, DECIMATED_BLOCK_SAMPLES);
          inc(SamplesRead, DECIMATED_BLOCK_SAMPLES);
        end;
        lBPMDetect.inputSamples(TChannel(FWave.ChannelList[0]).Buffer + SamplesRead, FWave.ReadCount - SamplesRead);
        RealBPM := lBPMDetect.getBpm;
        DBLog('Calculated BPM: ' + FloatToStr(RealBPM));

      except
        on e: exception do
        begin
          DBLog('Error: ' + e.Message);
        end;
      end;
    finally
      lBPMDetect.Free;
    end;

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
    TMarker(FSliceList[lMarkerIndex]).Free;
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
    paFFT:
    begin
      Result := 0;
    end;
    paPitched:
    begin
      Result := 0;
    end;
  end;
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
function TWavePattern.WarpedLocation(AStartIndex: Integer; ALocation: single; var AFrameData: TFrameData; var ALoopingFrameData: TFrameData; ABuffer: PSingle): Boolean;
var
  i: Integer;
  lSliceStart: TMarker;
  lSliceEnd: TMarker;
begin
  Result := True;

  AFrameData.Location := ALocation;
  AFrameData.Ramp := 1;
  ALoopingFrameData.Location := ALocation;
  ALoopingFrameData.Ramp := 1;

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
  FSampleScale := FWave.Frames / (FSampleEnd.Value - FSampleStart.Value);
  FSampleScaleInverse := (FSampleEnd.Value - FSampleStart.Value) / FWave.Frames;

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
  ASourceBuffer: PSingle; ATargetBuffer: PSingle; AFrameIndex: Integer; AChannelCount: Integer);
var
  lFracPosition: Single;
  lBufferOffset: integer;
begin
  lFracPosition := Frac(ASampleCursor);
  lBufferOffset := Round(ASampleCursor * AChannelCount);

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
    ATargetBuffer[AFrameIndex * 2] := ASourceBuffer[lBufferOffset];
    ATargetBuffer[AFrameIndex * 2 + 1] := ASourceBuffer[lBufferOffset];
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

    ATargetBuffer[AFrameIndex * 2] := ASourceBuffer[lBufferOffset];
    ATargetBuffer[AFrameIndex * 2 + 1] := ASourceBuffer[lBufferOffset + 1];
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
      lBuffer := TChannel(Wave.ChannelList[0]).Buffer;

      if PitchAlgorithm = paSliceStretch then
      begin
        FSliceStretcher.Pitch := Pitch;
        FSliceStretcher.Tempo := GAudioStruct.BPMScale;
        FSliceStretcher.SampleScale := FSampleScale;
        FSliceStretcher.Process(StartingSliceIndex, FSampleCursor, FSliceCounter, lBuffer, ABuffer, AFrameIndex, FWave.ChannelCount);
      end
      else
      begin
        if WarpedLocation(StartingSliceIndex, FSampleCursor, lFramePacket, lLoopingFramePacket, lBuffer) then
        begin
          CursorAdder := lFramePacket.Location;
          CursorRamp := lFramePacket.Ramp;

          if Trunc(CursorAdder) < (Wave.Frames * FWave.ChannelCount) then
          begin
            GetSampleAtCursor(CursorAdder, lBuffer, WorkBuffer, AFrameIndex, Wave.ChannelCount);

            // Scale buffer up to now when the scaling-factor has changed or
            // the end of the buffer has been reached.
            if PitchAlgorithm in [paSoundTouch, paSoundTouchEco, paFFT] then
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

                DBLog(Format('CalculatedPitch %f', [CalculatedPitch]));

                case PitchAlgorithm of
                  paSoundTouch, paSoundTouchEco:
                  begin
                    TimeStretch.setPitch(CalculatedPitch);
                    TimeStretch.PutSamples(@WorkBuffer[psBeginLocation], lFrames);
                    TimeStretch.ReceiveSamples(@ABuffer[psBeginLocation], lFrames);
                  end;
                  paFFT:
                  begin
                    FFFTStretcher.Pitch := CalculatedPitch;
                    FFFTStretcher.Process(@WorkBuffer[psBeginLocation], @ABuffer[psBeginLocation], lFrames);
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
  BarCount := FWave.Frames div (2 * FWave.SampleRate);
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

  DBLog(Format('FWave.Frames %d BarCount %d, BarLength %d, LoopLength.Value %d SampleEnd.Value %d FWave.SampleRate %d FWave.ChannelCount %d',
    [FWave.Frames, BarCount, BarLength, LoopLength.Value, SampleEnd.Value, FWave.SampleRate, FWave.ChannelCount]));

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

  lDetermineTransients := TDetermineTransients.Create(Round(GSettings.SampleRate));
  try
    lDetermineTransients.Sensitivity := FSensitivity;
    lDetermineTransients.Process(TChannel(Wave.ChannelList[0]).Buffer, FWave.Frames, FWave.ChannelCount);
    if lDetermineTransients.Transients.Count = 0 then
    begin
      DBLog('no transients');
    end;
    lDoAddSlice := True;
    for i := 0 to Pred(lDetermineTransients.Transients.Count) do
    begin
      if i > 0 then
      begin
        lDoAddSlice := lDetermineTransients.Transients[i] - lDetermineTransients.Transients[i - 1] > (GSettings.SampleRate * 0.125);
      end;
      if lDoAddSlice then
      begin
        AddSlice(lDetermineTransients.Transients[i], SLICE_VIRTUAL, True);
      end;
    end;
  finally
    lDetermineTransients.Free;
  end;
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

    DBLog('Setting locked = %s', BoolToStr(lMarker.Locked, True));

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

    DBLog('Setting locked = %s', BoolToStr(lMarker.Locked, True));

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

{ TDiskWriterThread }

procedure TDiskWriterThread.Updater;
begin
  //DBLog(Format('IO Data written %d', [FBufferOffset]));
end;

procedure TDiskWriterThread.Execute;
var
  lSize: Integer;
begin
  while (not Terminated) do
  begin
    // Write to disk
    lSize := jack_ringbuffer_read_space(FRingBuffer);
    if lSize > 0 then
    begin
      Inc(FBufferOffset, lSize);
      jack_ringbuffer_read(FRingBuffer, @FBuffer[0], lSize);

      sf_write_float(SoundFileOut, @FBuffer[0], lSize);
    end;

    // Update gui
    Synchronize(@Updater);

    // Only update at 1000 ms
    Sleep(1000);
  end;
end;

constructor TDiskWriterThread.Create(CreateSuspended: boolean);
var
  lGUID: TGuid;
begin
  inherited Create(CreateSuspended);

  FBufferSize := 88200;

  FRingBuffer := jack_ringbuffer_create(FBufferSize);
  FBuffer := getmem(FBufferSize);
  FBufferOffset := 0;

  // Create guid
  CreateGUID(lGUID);

  //FTempName := GUIDToString(lGUID) + '.wav';
  FTempName := 'aaa.wav';
  DeleteFile(FTempName);

  InfoOut.channels := 1;
  InfoOut.samplerate := 44100; //Round(GSettings.SampleRate);
  InfoOut.format:=(SF_FORMAT_WAV or SF_FORMAT_FLOAT);

  SoundFileOut := sf_open(StringToPChar(FTempName), SFM_WRITE, InfoOut);

  if SoundFileOut = nil then
  begin
    sf_perror(Nil);
    exit;
  end;
end;

destructor TDiskWriterThread.Destroy;
begin
  sf_close(SoundFileOut);

  jack_ringbuffer_free(FRingBuffer);

  freemem(FBuffer);

  //DeleteFile(FTempName);

  inherited Destroy;
end;

procedure TDiskWriterThread.RingbufferWrite(ABuffer: jack_default_audio_sample_t;
  ASize: Integer);
begin
  jack_ringbuffer_write(FRingBuffer, @ABuffer, ASize);
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

{ TDiskReaderThread }

procedure TDiskReaderThread.Updater;
begin
  DBLog(Format('Streaming from disk: %d', [FBufferOffset]));
end;

procedure TDiskReaderThread.Execute;
var
  lSize: Integer;
begin
  while (not Terminated) do
  begin
    // Only update at 1000 ms
    Sleep(1000);

    // Write to disk
    lSize := jack_ringbuffer_write_space(FRingBuffer);
    if lSize > 0 then
    begin
      Inc(FBufferOffset, lSize);
      FFileStream.Seek(FBufferOffset, soFromBeginning);
      FFileStream.Read(FBuffer, lSize);
      jack_ringbuffer_write(FRingBuffer, @FBuffer[0], lSize);
    end;

    // Update gui
    Synchronize(@Updater);
  end;
end;

constructor TDiskReaderThread.Create(CreateSuspended: boolean);
begin
  FBufferSize := 88200;

  FRingBuffer := jack_ringbuffer_create(FBufferSize);
  FBuffer := getmem(FBufferSize);

  FTempName := 'aaa.wav';

  FBufferOffset := 0;
  FFileStream := TFileStream.Create(FTempName, fmOpenRead);

  inherited Create(CreateSuspended);
end;

destructor TDiskReaderThread.Destroy;
begin
  FFileStream.Free;

  jack_ringbuffer_free(FRingBuffer);

  freemem(FBuffer);

  DeleteFile(FTempName);

  inherited Destroy;
end;

procedure TDiskReaderThread.RingbufferReader(
  ABuffer: jack_default_audio_sample_t; ASize: Integer);
begin
  if jack_ringbuffer_read_space(FRingBuffer) >= ASize then
  begin
    jack_ringbuffer_read(FRingBuffer, @ABuffer, ASize);
  end
  else
  begin
    // Buffer underrun
  end;
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

constructor TSliceStretcher.Create;
begin
  FSliceLooper := TSliceLooper.Create;
end;

destructor TSliceStretcher.Destroy;
begin
  FSliceLooper.Free;

  inherited Destroy;
end;

procedure TSliceStretcher.GetSample(
  ACursor: Single;
  ASourceBuffer: PSingle;
  var ALeftValue: Single;
  var ARightValue: Single;
  AChannelCount: Integer);
var
  lFracPosition: Single;
  lBufferOffset: integer;
begin
  lBufferOffset := Round(ACursor * AChannelCount);

  case FInterpolationAlgorithm of
    iaHermite:
    begin
      lFracPosition := Frac(ACursor);

      if AChannelCount = 1 then
      begin
        ALeftValue := hermite4(
          lFracPosition,
          ifthen(lBufferOffset <= 1, 0, ASourceBuffer[lBufferOffset - 1]),
          ASourceBuffer[lBufferOffset],
          ASourceBuffer[lBufferOffset + 1],
          ASourceBuffer[lBufferOffset + 2]);

        ARightValue := ALeftValue;
      end
      else
      begin
        ALeftValue := hermite4(
          lFracPosition,
          ifthen(lBufferOffset <= 1, 0, ASourceBuffer[lBufferOffset - 2]),
          ASourceBuffer[lBufferOffset],
          ASourceBuffer[lBufferOffset + 2],
          ASourceBuffer[lBufferOffset + 4]);

        ARightValue := hermite4(
          lFracPosition,
          ifthen(lBufferOffset + 1 <= 2, 0, ASourceBuffer[lBufferOffset - 1]),
          ASourceBuffer[lBufferOffset + 1],
          ASourceBuffer[lBufferOffset + 3],
          ASourceBuffer[lBufferOffset + 5]);
      end;
    end;
    iaLinear:
    begin

    end;
    iaNone:
    begin
      if AChannelCount = 1 then
      begin
        ALeftValue := ASourceBuffer[lBufferOffset];
        ARightValue := ASourceBuffer[lBufferOffset];
      end
      else
      begin
        ALeftValue := ASourceBuffer[lBufferOffset];
        ARightValue := ASourceBuffer[lBufferOffset + 1];
      end;
    end;
  end;
end;

procedure TSliceStretcher.Process(
  AStartIndex: Integer;
  var ASampleCursor: Single;
  var ASliceCounter: Single;
  ASourceBuffer: PSingle;
  ATargetBuffer: PSingle;
  AFrameIndex: Integer;
  AChannelCount: Integer);
var
  i: Integer;
  lSliceStart: TMarker;
  lSliceEnd: TMarker;
  lLeftValue: Single;
  lRightValue: Single;
  lLeftValueLooper: Single;
  lRightValueLooper: Single;
  lCursor: Single;
  lLoopingCursor: Single;
begin
  for i := AStartIndex to FSliceList.Count - 2 do
  begin
    lSliceStart := TMarker(FSliceList.Items[i]);
    lSliceEnd := TMarker(FSliceList.Items[i + 1]);

    if (ASampleCursor >= lSliceStart.Location) and (ASampleCursor < lSliceEnd.Location) then
    begin
      // Detect slice synchronize
      ASliceCounter := fmod(ASampleCursor - lSliceStart.Location, lSliceStart.Length);

      // Slicecounter looped so it's a slice sync
      if ASliceCounter < FSliceLastCounter then
      begin
        // Keep last slice looping
        FSliceLooper.SliceStart := FSliceStartLocation;
        FSliceLooper.SliceEnd := FSliceEndLocation;
        FSliceLooper.Cursor := FSliceCursor;
        FSliceLooper.Pitch := FPitch;

        // Start of slice
        FSliceStartLocation :=
          lSliceStart.OrigLocation +
          (lSliceStart.DecayRate * (ASampleCursor - lSliceStart.Location));

        FSliceEndLocation :=
          FSliceStartLocation +
          lSliceStart.Length * FSampleScale * lSliceStart.DecayRate;

        FSliceCursor := FSliceStartLocation;

        FSliceHalfLength := Round(FSliceEndLocation - FSliceStartLocation) div 2;

        FSliceFadeOut := 1;
        FSliceFadeIn :=  0;
      end;

      if FSliceFadeOut > 0 then
      begin
        FSliceFadeOut := FSliceFadeOut - 0.05
      end
      else
      begin
        FSliceFadeOut := 0;
      end;
      if FSliceFadeIn < 1 then
      begin
        FSliceFadeIn := FSliceFadeIn + 0.05;
      end
      else
      begin
        FSliceFadeIn := 1;
      end;

      if (FSliceFadeIn < 1) or (FSliceFadeOut > 0) then
      begin
        lLoopingCursor := FSliceLooper.Process;
      end;

      if FSliceCursor >= FSliceEndLocation then
      begin
        // Ran past end of slice so loop a part of the previous section
        if FSliceHalfLength <> 0 then
        begin
          FSliceLoopModulo := Round(FSliceCursor - FSliceEndLocation) mod FSliceHalfLength;
          if Odd(Round(FSliceCursor - FSliceEndLocation) div FSliceHalfLength) then
          begin
            lCursor := Round(FSliceEndLocation - FSliceHalfLength) + FSliceLoopModulo;
          end
          else
          begin
            lCursor := Round(FSliceEndLocation) - FSliceLoopModulo;
          end;
        end;
      end
      else
      begin
        // Still inside slice so calculate cursor position
        lCursor := FSliceCursor;
      end;

      // Protect against accidental overflow
      if lCursor > TMarker(FSliceList.Last).Location then
      begin
        lCursor := TMarker(FSliceList.Last).Location;
      end
      else if lCursor < TMarker(FSliceList.First).Location then
      begin
        lCursor := TMarker(FSliceList.First).Location;
      end;

      // Increate nominal always playing at samplespeed * pitch
      FSliceCursor := FSliceCursor + Pitch;

      FSliceLastCounter := ASliceCounter;

      // Get normal stream
      GetSample(lCursor, ASourceBuffer, lLeftValue, lRightValue, AChannelCount);

      // Get looping stream
      GetSample(lLoopingCursor, ASourceBuffer, lLeftValueLooper, lRightValueLooper, AChannelCount);

      // Mix both streams together
      ATargetBuffer[AFrameIndex * 2] :=
        lLeftValue * FSliceFadeIn
        +
        lLeftValueLooper * FSliceFadeOut;

      ATargetBuffer[AFrameIndex * 2 + 1] :=
        lRightValue * FSliceFadeIn
        +
        lRightValueLooper * FSliceFadeOut;

      break;
    end;
  end;
end;


initialization
finalization

end.
