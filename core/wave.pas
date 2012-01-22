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

interface

uses
 Classes, SysUtils, Controls, Graphics, LCLType, Forms, ExtCtrls, ctypes, sndfile,
 jacktypes, StdCtrls, Dialogs, Spin, bpmdetect, beattrigger, Utils,
 globalconst, librubberband, soundtouch, contnrs, global_command,
 ShellCtrls, global, multiband_wsola, flqueue, math, ringbuffer, pattern,
 audiostructure;

const
  MAX_LATENCY = 20000;
  DECIMATED_CACHE_DISTANCE = 64;

type
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



  { TWavePattern }
  TWavePattern = class(TPattern)
  private
    { Audio }
    FDecimatedData: PSingle;
    FWorkBuffer: PSingle;
    FBufferFrames: Integer;
    FSliceList: TObjectList;
    FCurrentSliceIndex: Integer;
    FCurrentSlice: TMarker;
    FRealCursorPosition: Integer;
    FVirtualCursorPosition: Integer;
    FLoopStart: TLoopMarker;
    FLoopEnd: TLoopMarker;
    FSampleStart: TLoopMarker;
    FSampleEnd: TLoopMarker;
    FLoopLength: TLoopMarker;
    FBarLength: Integer;
    FDragSlice: Boolean;
    FZooming: Boolean;
    FSelectedSlice: TMarker;
    FRubberbandSelect: Boolean;
    FCursorAdder: Single;
    FCursorReal: Single;
    FCursorRamp: Single;
    FSampleRate: Single;
    FVolumeDecay: Single;
    FWaveFileName: string;
    FTimeStretch: TSoundTouch;
    FTransientThreshold: Integer;
    FBufferDataSize: PtrUInt;
    FWave: TWaveFile;
    FRealBPM: Single;
    FPatternLength: Integer;
    FBPMscale: single;

//    FDiskWriterThread: TDiskWriterThread;
    FDiskReaderThread: TDiskReaderThread;
    MultiWSOLA: TMultiWSOLA;
    FPitchAlgorithm: TPitchAlgorithm;

    procedure SetCursorRamp(const AValue: Single);
    procedure SetPitchAlgorithm(AValue: TPitchAlgorithm);
    procedure SetRealBPM(const AValue: Single);
    function SliceAt(Location: Integer; Margin: single): TMarker;
    procedure SetTransientThreshold(const AValue: Integer);
    procedure UpdateBPMScale;
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
  public
    // Hermite interpolation
    frac_pos,xm1,x0,x1,x2: single;
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
    lStartingSliceIndex: Integer;
    buf_offset: integer;
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
    procedure AutoMarkerProcess(ACalculateStatistics: Boolean = True);
    function GetSliceAt(Location: Integer; AMargin: single): TMarker;
    procedure VirtualLocation(AStartIndex: Integer; ALocation: single; var AFrameData: TFrameData);
    function StartVirtualLocation(ALocation: single): Integer;

    procedure ProcessInit; override;
    procedure Process(ABuffer: PSingle; AFrameIndex: Integer; AFrameCount: Integer); override;
    procedure ProcessAdvance; override;

    property TimeStretch: TSoundTouch read FTimeStretch write FTimeStretch;
    property WSOLA: TMultiWSOLA read MultiWSOLA write MultiWSOLA;
    property Wave: TWaveFile read FWave write FWave;
    property WorkBuffer: Pjack_default_audio_sample_t read FWorkBuffer write FWorkBuffer;
    property CurrentSlice: TMarker read FCurrentSlice write FCurrentSlice;
    property SelectedSlice: TMarker read FSelectedSlice write FSelectedSlice;
    property DecimatedData: PSingle read FDecimatedData write FDecimatedData;
    property RealCursorPosition: Integer read FRealCursorPosition write FRealCursorPosition;
    property VirtualCursorPosition: Integer read FVirtualCursorPosition write FVirtualCursorPosition;
    property CurrentSliceIndex: Integer read FCurrentSliceIndex write FCurrentSliceIndex;
    property CursorAdder: Single read FCursorAdder write FCursorAdder;
    property CursorReal: Single read FCursorReal write FCursorReal default 1.0;
    property CursorRamp: Single read FCursorRamp write SetCursorRamp default 1.0;
    property BufferFrames: Integer read FBufferFrames write FBufferFrames;
//    property DiskWriterThread: TDiskWriterThread read FDiskWriterThread;
    property DiskReaderThread: TDiskReaderThread read FDiskReaderThread;
  published
    property SliceList: TObjectList read FSliceList write FSliceList;
    property LoopStart: TLoopMarker read FLoopStart write FLoopStart;
    property LoopEnd: TLoopMarker read FLoopEnd write FLoopEnd;
    property LoopLength: TLoopMarker read FLoopLength write FLoopLength;
    property SampleStart: TLoopMarker read FSampleStart write FSampleStart;
    property SampleEnd: TLoopMarker read FSampleEnd write FSampleEnd;
    property SampleRate: Single read FSampleRate write FSampleRate;
    property VolumeDecay: Single read FVolumeDecay write FVolumeDecay default 1;
    property TransientThreshold: Integer read FTransientThreshold write SetTransientThreshold;
    property BarLength: Integer read FBarLength write FBarLength;
    property RealBPM: Single read FRealBPM write SetRealBPM;
    property PatternLength: Integer read FPatternLength write FPatternLength;
    property PitchAlgorithm: TPitchAlgorithm read FPitchAlgorithm write SetPitchAlgorithm;
    property WaveFileName: string read FWaveFileName write FWaveFileName;
  end;

  { TWaveFormCommand }

  TWaveFormCommand = class(TCommand)
  private
    FWavePattern: TWavePattern;
  protected
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

  { TUpdateWaveLoopStartCommand }

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

  { TUpdateWaveLoopStartCommand }

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



var
  BeatDetect: TBeatDetector;

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
  BeatDetect.setThresHold(FTransientThreshold / 100);
  AutoMarkerProcess(True);
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

  FLoopStart := TLoopMarker.Create(AObjectOwner, ltStart);
  FLoopEnd := TLoopMarker.Create(AObjectOwner, ltEnd);
  FLoopLength := TLoopMarker.Create(AObjectOwner, ltLength);

  FSampleStart := TLoopMarker.Create(AObjectOwner, ltStart);
  FSampleEnd := TLoopMarker.Create(AObjectOwner, ltEnd);

  FWave := TWaveFile.Create(AObjectOwner, True);
  FWave.BufferFormat := bfInterleave;

  // Initalize settings
  FDragSlice:= False;
  FZooming:= False;
  FRubberbandSelect:= False;
  FCursorAdder:= 0;
  FVolumeDecay:= 1;

  FSliceList := TObjectList.Create(True);
  FCurrentSliceIndex:= 0;

  FTimeStretch := TSoundTouch.Create;
  FTimeStretch.setChannels(1);
  FTimeStretch.setSampleRate(Round(GSettings.SampleRate));

  MultiWSOLA := TMultiWSOLA.Create;
  MultiWSOLA.Samplerate := Round(GSettings.SampleRate);
  MultiWSOLA.Channels := 1;
  MultiWSOLA.Bands := 3;
  MultiWSOLA.QuickSeek := False;
  MultiWSOLA.AntiAliasFilter := False;
  MultiWSOLA.Initialize;

  Pitch := 1;

  PitchAlgorithm := paMultiBandSoundTouchEco;

  Getmem(FWorkBuffer, 88200);

//  FDiskWriterThread := TDiskWriterThread.Create(False);
//  FDiskWriterThread.FreeOnTerminate := True;
//  FDiskReaderThread := TDiskReaderThread.Create(False);
//FDiskReaderThread.FreeOnTerminate := True;

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
  if Assigned(FTimeStretch) then
    FTimeStretch.Free;
  if Assigned(MultiWSOLA) then
    MultiWSOLA.Free;
  if Assigned(FWave) then
    FWave.Free;
  if Assigned(FLoopStart) then
    FLoopStart.Free;
  if Assigned(FLoopEnd) then
    FLoopEnd.Free;
  if Assigned(FLoopLength) then
    FLoopLength.Free;
{  if Assigned(FDiskWriterThread) then
    FDiskWriterThread.Free;}
//  if Assigned(FDiskReaderThread) then
//    FDiskReaderThread.Free;
  if Assigned(FSampleStart) then
    FSampleStart.Free;
  if Assigned(FSampleEnd) then
    FSampleEnd.Free;

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

  FWave.BufferFormat := bfInterleave;
  FWave.LoadSample(AFilename);

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

    AddSlice(0, SLICE_UNDELETABLE, True);
    AddSlice(FWave.Frames, SLICE_UNDELETABLE, True);

    CurrentSlice:= TMarker(SliceList[0]);
    CurrentSliceIndex:= 0;

    // Init default values
    CursorAdder:= 0;

    //NormalizeInMemorySample(TChannel(FWave.ChannelList[0]).Buffer, FWave.ReadCount);

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
          writeln('Error: ' + e.Message);
        end;
      end;
    finally
      lBPMDetect.Free;
    end;
    AutoMarkerProcess(True);
  end;

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
        //RecalculateSlicePosition(lWaveSlice, Location);
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
  GLogger.PushMessage(Format('BPMDetect analysed: %f', [AValue]));
  if FRealBPM = AValue then exit;
  FRealBPM := AValue;

  if FRealBPM = 0 then
  begin
    FRealBPM := 120;
  end;
  DivideByRealBPM_Multiplier := 1 / FRealBPM;
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
  FPitchAlgorithm := AValue;

  case FPitchAlgorithm of
  paSoundTouchEco:
    begin
      TimeStretch.flush;
      TimeStretch.setSetting(SETTING_USE_QUICKSEEK, 1);
      TimeStretch.setSetting(SETTING_USE_AA_FILTER, 1);
      TimeStretch.setPitch(CalculatedPitch);
    end;
  paMultiBandSoundTouchEco:
    begin
      MultiWSOLA.Flush;
      MultiWSOLA.QuickSeek := True;
      MultiWSOLA.AntiAliasFilter := True;
      MultiWSOLA.Pitch := CalculatedPitch;
    end;
  paSoundTouch:
    begin
      TimeStretch.flush;
      TimeStretch.setSetting(SETTING_USE_QUICKSEEK, 0);
      TimeStretch.setSetting(SETTING_USE_AA_FILTER, 1);
      TimeStretch.setPitch(CalculatedPitch);
    end;
  paMultiBandSoundTouch:
    begin
      MultiWSOLA.Flush;
      MultiWSOLA.QuickSeek := False;
      MultiWSOLA.AntiAliasFilter := True;
      MultiWSOLA.Pitch := CalculatedPitch;
    end
  else
    begin
      FPitchAlgorithm := paNone;
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
begin
  for i := 0 to FSliceList.Count - 2 do
  begin
    TMarker(FSliceList.Items[i]).DecayRate :=
    (TMarker(FSliceList.Items[i + 1]).OrigLocation - TMarker(FSliceList.Items[i]).OrigLocation) /
    (TMarker(FSliceList.Items[i + 1]).Location - TMarker(FSliceList.Items[i]).Location);
  end;
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
procedure TWavePattern.VirtualLocation(AStartIndex: Integer; ALocation: single; var AFrameData: TFrameData);
var
  i: Integer;
  lSliceStart: TMarker;
  lSliceEnd: TMarker;
begin
  AFrameData.Location := ALocation;
  AFrameData.Ramp := 1;

  for i := AStartIndex to FSliceList.Count - 2 do
  begin
    lSliceStart := TMarker(FSliceList.Items[i]);
    lSliceEnd := TMarker(FSliceList.Items[i + 1]);

    if (ALocation >= lSliceStart.Location) and (ALocation < lSliceEnd.Location) then
    begin
      AFrameData.Location := lSliceStart.OrigLocation + (lSliceStart.DecayRate * (ALocation - lSliceStart.Location));
      AFrameData.Ramp := lSliceStart.DecayRate;
      break;
    end;
  end;
end;

{
  Looks for the index of the current slice and can be used to provide the startindex
  for the method VirtualLocation. This prevents a big loop every sample when there are
  lots of warp markers.
}
function TWavePattern.StartVirtualLocation(ALocation: single): Integer;
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

procedure TWavePattern.UpdateBPMScale;
begin
  //  FBPMscale := GAudioStruct.BPM / FRealBPM;
  FBPMscale := GAudioStruct.BPM * DivideByRealBPM_Multiplier *
    (FWave.Frames / (FSampleEnd.Location - FSampleStart.Location));
  if FBPMscale > 16 then
    FBPMscale := 16
  else if FBPMscale < 0.1 then
    FBPMscale := 0.1;
end;

procedure TWavePattern.ProcessInit;
begin
  UpdateBPMScale;

  psBeginLocation := 0;
end;

procedure TWavePattern.Process(ABuffer: PSingle; AFrameIndex: Integer; AFrameCount: Integer);
var
  lAvailSamples: longint;
begin
  if AFrameIndex = 0 then
  begin
    psLastScaleValue := CursorRamp;
  end;

  // Synchronize with global quantize track
  if (CursorReal >= LoopEnd.Location) or SyncQuantize then
  begin
    SyncQuantize := False;
    CursorReal := LoopStart.Location;
  end;

  // Fetch frame data packet containing warp factor and virtual location in wave data
  if AFrameIndex = 0 then
  begin
    StartingSliceIndex := StartVirtualLocation(CursorReal);
  end;

  VirtualLocation(StartingSliceIndex, CursorReal, lFramePacket);
  CursorAdder := lFramePacket.Location;
  CursorRamp := lFramePacket.Ramp * FBPMscale;
  if PitchAlgorithm = paPitched then
    CursorRamp := Pitch;

  // Put sound in buffer
  lBuffer := TChannel(Wave.ChannelList[0]).Buffer;
  frac_pos := Frac(CursorAdder);
  buf_offset := (Trunc(CursorAdder) * Wave.ChannelCount);

  if Trunc(CursorAdder) < Wave.Frames then
  begin
    // Interpolate with Hermite interpolation
    if buf_offset <= 0 then
      xm1 := 0
    else
      xm1 := lBuffer[buf_offset - 1];
    x0 := lBuffer[buf_offset];
    x1 := lBuffer[buf_offset + 1];
    x2 := lBuffer[buf_offset + 2];
    WorkBuffer[AFrameIndex] := hermite4(frac_pos, xm1, x0, x1, x2);

    // Scale buffer up to now when the scaling-factor has changed or
    // the end of the buffer has been reached.
    if PitchAlgorithm <> paNone then
    begin
      if (CursorRamp <> psLastScaleValue) or (AFrameIndex = (AFrameCount - 1)) then
      begin
        psEndLocation := AFrameIndex;
        CalculatedPitch := Pitch * (1 / FCursorRamp);

        // Frames to process this window
        lFrames := (psEndLocation - psBeginLocation) + 1;

        case PitchAlgorithm of
          paSoundTouch, paSoundTouchEco:
          begin
            if CursorRamp <> psLastScaleValue then
            begin
              TimeStretch.setPitch(CalculatedPitch);
            end;
            TimeStretch.PutSamples(@WorkBuffer[psBeginLocation], lFrames);
            TimeStretch.ReceiveSamples(@ABuffer[psBeginLocation], lFrames);
          end;
          paMultiBandSoundTouch, paMultiBandSoundTouchEco:
          begin
            if CursorRamp <> psLastScaleValue then
            begin
              WSOLA.Pitch := CalculatedPitch;
            end;
            WSOLA.Process(@WorkBuffer[psBeginLocation], @ABuffer[psBeginLocation], lFrames);
          end;
        end;

        // Remember last change
        psBeginLocation:= AFrameIndex;
        psLastScaleValue:= CursorRamp;
      end;
    end
    else
    begin
      ABuffer[AFrameIndex] := WorkBuffer[AFrameIndex];
    end;
  end;
end;

procedure TWavePattern.ProcessAdvance;
begin
  UpdateBPMScale;

  // Advance cursors for midi and tsPattern
  RealCursorPosition := Round(CursorReal);
  CursorReal := CursorReal + FBPMscale;
end;

{ TWaveFormScrollBox }

procedure TWavePattern.AutoMarkerProcess(ACalculateStatistics: Boolean = True);
var
  i: Integer;
  WindowLength: Integer;
  lCurrentSlice: TMarker;
  lFirstSlice: Boolean;
  lSample: Integer;
begin
  GLogger.PushMessage('StartAutoMarkerProcess');

  lFirstSlice:= True;
  lCurrentSlice:= TMarker(FSliceList[0]);

  WindowLength:= 0;

  if ACalculateStatistics then
  begin
    // First remove old slices
    for i := Pred(FSliceList.Count) downto 0 do
    begin
      if TMarker(FSliceList[i]).SliceType = SLICE_VIRTUAL then
      begin
        FSliceList.Delete(i);
      end;
    end;

    for i:= 0 to (FWave.ReadCount div FWave.ChannelCount - 1) do
    begin
      BeatDetect.AudioProcess(TChannel(Wave.ChannelList[0]).Buffer[i * FWave.ChannelCount]);
      if BeatDetect.BeatPulse then
      begin
        if Assigned(lCurrentSlice) and BeatDetect.TriggerOn then
        begin
          WindowLength:= 0;
        end;
        GLogger.PushMessage(format('slice at %d', [i]));
        lCurrentSlice := AddSlice(i, SLICE_VIRTUAL, True);
        if lFirstSlice then
        begin
          LoopStart.Location:= i;
          lFirstSlice:= False;
        end;
      end;

      if BeatDetect.ReleasePulse then
      begin
        WindowLength:= 0;
      end
      else
        Inc(WindowLength);

    end;
  end;
  // Calculate Loopend as a multiple of the maximum number of bars in the sample

  if RealBPM = 0 then
  begin
    // Assume the sample is cropped to one, two bars, etc
    LoopStart.Location := 0;
    LoopEnd.Location := BufferFrames;
    RealBPM := ((LoopEnd.Location - LoopStart.Location) div FWave.Samplerate) * 60;
    if RealBPM > 200 then
    begin
      repeat
        RealBPM := RealBPM / 2;

      until RealBPM <= 200;
    end;
    if RealBPM < 75 then
    begin
      repeat
        RealBPM := RealBPM * 2;

      until RealBPM >= 75;
    end;

  end;
  BarLength:= Round((4 * (60 / 120{RealBPM})) * FWave.Samplerate);
  {PatternLength := BarLength * FWave.ChannelCount;
  lSample:= BufferFrames - LoopStart.Location;
  // Just guessing...
  while (BarLength) > lSample do
    BarLength:= BarLength div 2;

  LoopStart.Location := 0;
  SampleStart.Location := 0;

  if BarLength > 0 then
  begin
    LoopEnd.Location:= (lSample div BarLength) * BarLength;
  end;  }

  LoopEnd.Location := BarLength;
  SampleEnd.Location := FWave.Frames;

  LoopLength.Location := LoopEnd.Location - LoopStart.Location;

  GLogger.PushMessage('EndAutoMarkerProcess');
end;


{ TAddMarkerCommand }

procedure TAddMarkerCommand.DoExecute;
var
  lMementoMarker: TMarker;
  lMarker: TMarker;
begin
  // Check if there's a marker already
  lMarker := FWavePattern.AddSlice(Location, SLICE_NORMAL, True);
  lMarker.Locked := True;

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

  if Persist then
  begin
    // Save state
    case FDataType of
    stStart: FOldLocation := FWavePattern.SampleStart.Location;
    stEnd: FOldLocation := FWavePattern.SampleEnd.Location;
    end;
  end;

  // Assign
  case FDataType of
    stStart:
    begin
      if FLocation < 0 then FLocation := 0;
      FWavePattern.SampleStart.Location := FLocation;
    end;
    stEnd:
    begin
      if FLocation < 0 then FLocation := 0;
      FWavePattern.SampleEnd.Location := FLocation;
    end;
  end;

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

  // Retrieve state
  FWavePattern.SampleStart.Location := FOldLocation;

  // Assign
  case FDataType of
    stStart: FWavePattern.SampleStart.Location := FOldLocation;
    stEnd: FWavePattern.SampleEnd.Location := FOldLocation;
  end;

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

  if Persist then
  begin
    // Save state
    case FDataType of
    ltStart: FOldLocation := FWavePattern.LoopStart.Location;
    ltEnd: FOldLocation := FWavePattern.LoopEnd.Location;
    ltLength: FOldLocation := FWavePattern.LoopLength.Location;
    end;
  end;

  // Assign
  case FDataType of
  ltStart:
  begin
    if FLocation < 0 then FLocation := 0;
    FWavePattern.LoopStart.Location := FLocation;
  end;
  ltEnd:
  begin
    if FLocation < 0 then FLocation := 0;
    FWavePattern.LoopEnd.Location := FLocation;
  end;
  ltLength: FWavePattern.LoopLength.Location := FLocation;
  end;

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
  FWavePattern.LoopStart.Location := FOldLocation;

  // Assign
  case FDataType of
  ltStart: FWavePattern.LoopStart.Location := FOldLocation;
  ltEnd: FWavePattern.LoopEnd.Location := FOldLocation;
  ltLength: FWavePattern.LoopLength.Location := FOldLocation;
  end;

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
  //writeln(Format('IO Data written %d', [FBufferOffset]));
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
  writeln(Format('Streaming from disk: %d', [FBufferOffset]));
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

  // Store pitch
  FOldpitch := FWavePattern.Pitch;
  FWavePattern.Pitch := Pitch;
  FWavePattern.Notify;

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



initialization

  BeatDetect := TBeatDetector.Create;

finalization
  BeatDetect.Free;

end.
