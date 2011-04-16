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
unit waveform;

interface

uses
 Classes, SysUtils, Controls, Graphics, LCLType, Forms, ExtCtrls, ctypes, sndfile,
 jacktypes, StdCtrls, Dialogs, Spin, bpm, beattrigger, Utils,
 globalconst, rubberband, SoundTouchObject, contnrs, global_command,
 ShellCtrls, global, multiband_wsola, flqueue, math, ringbuffer;

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



  { TWaveForm }
  TWaveForm = class(THybridPersistentModel)
  private
    { Audio }
    FDecimatedData: PSingle;
    FBufferData: PSingle;
    FBufferData2: PSingle;
    FBufferData3: PSingle;
    FWorkBuffer: PSingle;
    FBufferFrames: Integer;
    FSliceList: TObjectList;
    FCurrentSliceIndex: Integer;
    FCurrentSlice: TMarker;
    FRealCursorPosition: Integer;
    FVirtualCursorPosition: Integer;
    FLoopStart: TLoopMarker;
    FLoopEnd: TLoopMarker;
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
    FSampleFileName: string;
    FTimeStretch: TSoundTouch;
    FTransientThreshold: Integer;
    FBufferDataSize: PtrUInt;
    FWave: TWaveFile;
    FRealBPM: Single;
    FPatternLength: Integer;

    FDiskWriterThread: TDiskWriterThread;
    FDiskReaderThread: TDiskReaderThread;
    MultiWSOLA: TMultiWSOLA;
//    FTimePitchScale: RubberbandState;
    FTimePitchOptions: Longint;
    FPitchAlgorithm: TPitchAlgorithm;

    function SliceAt(Location: Integer; Margin: single): TMarker;
    procedure SetTransientThreshold(const AValue: Integer);
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Initialize; override;
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
    property TimeStretch: TSoundTouch read FTimeStretch write FTimeStretch;
    property WSOLA: TMultiWSOLA read MultiWSOLA write MultiWSOLA;
    property Wave: TWaveFile read FWave write FWave;
    property WorkBuffer: Pjack_default_audio_sample_t read FWorkBuffer write FWorkBuffer;
    property CurrentSlice: TMarker read FCurrentSlice write FCurrentSlice;
    property SelectedSlice: TMarker read FSelectedSlice write FSelectedSlice;
    property DecimatedData: PSingle read FDecimatedData write FDecimatedData;
    property BufferData: PSingle read FBufferData write FBufferData;
    property BufferData2: PSingle read FBufferData2 write FBufferData2;
    property BufferData3: PSingle read FBufferData3 write FBufferData3;
//    property TimePitchScale: RubberBandState read FTimePitchScale write FTimePitchScale;
    property RealCursorPosition: Integer read FRealCursorPosition write FRealCursorPosition;
    property VirtualCursorPosition: Integer read FVirtualCursorPosition write FVirtualCursorPosition;
    property CurrentSliceIndex: Integer read FCurrentSliceIndex write FCurrentSliceIndex;
    property CursorAdder: Single read FCursorAdder write FCursorAdder;
    property CursorReal: Single read FCursorReal write FCursorReal default 1.0;
    property CursorRamp: Single read FCursorRamp write FCursorRamp default 1.0;
    property BufferFrames: Integer read FBufferFrames write FBufferFrames;
    property DiskWriterThread: TDiskWriterThread read FDiskWriterThread;
    property DiskReaderThread: TDiskReaderThread read FDiskReaderThread;
  published
    property SliceList: TObjectList read FSliceList write FSliceList;
    property LoopStart: TLoopMarker read FLoopStart write FLoopStart;
    property LoopEnd: TLoopMarker read FLoopEnd write FLoopEnd;
    property LoopLength: TLoopMarker read FLoopLength write FLoopLength;
    property SampleRate: Single read FSampleRate write FSampleRate;
    property VolumeDecay: Single read FVolumeDecay write FVolumeDecay default 1;
    property SampleFileName: string read FSampleFileName write FSampleFileName;
    property TransientThreshold: Integer read FTransientThreshold write SetTransientThreshold;
    property BarLength: Integer read FBarLength write FBarLength;
    property RealBPM: Single read FRealBPM write FRealBPM;
    property PatternLength: Integer read FPatternLength write FPatternLength;
    property PitchAlgorithm: TPitchAlgorithm read FPitchAlgorithm write FPitchAlgorithm;
  end;

  TWaveFormEngine = class
  private
  public
  end;

  { TWaveFormCommand }

  TWaveFormCommand = class(TCommand)
  private
    FWaveForm: TWaveForm;
  protected
    procedure Initialize; override;
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

var
  BeatDetect: TBeatDetector;
  BPMDetect: TBPMDetect;

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

procedure TWaveForm.SetTransientThreshold(const AValue: Integer);
begin
  FTransientThreshold:= AValue;
  BeatDetect.setThresHold(FTransientThreshold / 100);
//  AutoMarkerProcess(True);
end;

procedure TWaveForm.DoCreateInstance(var AObject: TObject; AClassName: string);
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

constructor Twaveform.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  DBLog('start Twaveform.Create');

  inherited Create(AObjectOwner, AMapped);

  FOnCreateInstanceCallback := @DoCreateInstance;

  FLoopStart := TLoopMarker.Create(AObjectOwner, ltStart);
  FLoopEnd := TLoopMarker.Create(AObjectOwner, ltEnd);
  FLoopLength := TLoopMarker.Create(AObjectOwner, ltLength);

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
  FTimeStretch.Channels := 1;
  FTimeStretch.SetSetting(SETTING_USE_QUICKSEEK, 1);
  FTimeStretch.SetSetting(SETTING_USE_AA_FILTER, 0);
  {FTimeStretch.setSetting(SETTING_SEQUENCE_MS, 40);
  FTimeStretch.setSetting(SETTING_SEEKWINDOW_MS, 15);
  FTimeStretch.setSetting(SETTING_OVERLAP_MS, 8);}

  MultiWSOLA := TMultiWSOLA.Create;
  MultiWSOLA.Channels := 1;
  MultiWSOLA.Bands := 4;
  MultiWSOLA.Pitch := 1;
  MultiWSOLA.Initialize;

{  FTimePitchOptions:=
    RubberBandOptionProcessRealTime or
    RubberBandOptionPhaseLaminar or
    RubberBandOptionWindowShort or
    RubberBandOptionPitchHighQuality; }

//  FTimePitchScale := rubberband_new(44100, 1, FTimePitchOptions, 1, 1);
//  rubberband_set_max_process_size(FTimePitchScale, 4096);

  FPitchAlgorithm := paST;

  Getmem(FWorkBuffer, 88200);

  FDiskWriterThread := TDiskWriterThread.Create(False);
//  FDiskWriterThread.FreeOnTerminate := True;
//  FDiskReaderThread := TDiskReaderThread.Create(False);
//FDiskReaderThread.FreeOnTerminate := True;

  DBLog('end Twaveform.Create');
end;

destructor Twaveform.Destroy;
begin
  if Assigned(FSliceList) then
    FSliceList.Free;
  if Assigned(FWorkBuffer) then
    FreeMem(FWorkBuffer);
  if Assigned(FBufferData) then
    Freemem(FBufferData);
  if Assigned(FBufferData2) then
    Freemem(FBufferData2);
  if Assigned(FBufferData3) then
    Freemem(FBufferData3);
  if Assigned(FDecimatedData) then
    Freemem(FDecimatedData);
  if Assigned(FTimeStretch) then
    FTimeStretch.Free;
  if Assigned(MultiWSOLA) then
    MultiWSOLA.Free;
{  if Assigned(FTimePitchScale) then
    rubberband_delete(FTimePitchScale);}
  if Assigned(FWave) then
    FWave.Free;
  if Assigned(FLoopStart) then
    FLoopStart.Free;
  if Assigned(FLoopEnd) then
    FLoopEnd.Free;
  if Assigned(FLoopLength) then
    FLoopLength.Free;
  if Assigned(FDiskWriterThread) then
    FDiskWriterThread.Free;
//  if Assigned(FDiskReaderThread) then
//    FDiskReaderThread.Free;

  inherited Destroy;
end;

procedure TWaveForm.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TWaveForm.Initialize;
begin
  BeginUpdate;

  if FSampleFileName <> '' then
  begin
    LoadSample(FSampleFileName);
  end;

  EndUpdate;
end;

function TWaveForm.LoadSample(AFilename: string): Boolean;
var
  lBuffer: PSingle;
  i, j: Integer;
  SamplesRead: Integer;
  lValue: single;
  lBPMDetect: TBPMDetect;
begin
  DBLog('Start LoadSample: ' + AFilename);

  Result := False;

  FWave.BufferFormat := bfInterleave;
  FWave.LoadSample(AFilename);

  SampleFileName := AFileName;

  if Assigned(FBufferData) then
    Freemem(FBufferData);
  if Assigned(FBufferData2) then
    Freemem(FBufferData2);
  if Assigned(FBufferData3) then
    Freemem(FBufferData3);
  if Assigned(FDecimatedData) then
    Freemem(FDecimatedData);

  FBufferDataSize:= FWave.DataSize;

  if FBufferDataSize > 0 then
  begin
    // Timestretch latency makes the cursor start at an offset and go past the normal end of the sample

    FBufferDataSize := FBufferDataSize + MAX_LATENCY;

    GetMem(FBufferData, FBufferDataSize);
    GetMem(FBufferData2, FBufferDataSize);
    GetMem(FBufferData3, FBufferDataSize);
    GetMem(FDecimatedData, FBufferDataSize div DECIMATED_CACHE_DISTANCE);
    FBufferFrames := FWave.Frames;

    AddSlice(0, SLICE_UNDELETABLE, True);
    AddSlice(FWave.Frames, SLICE_UNDELETABLE, True);

    CurrentSlice:= TMarker(SliceList[0]);
    CurrentSliceIndex:= 0;

    // Init default values
    CursorAdder:= 0;

    NormalizeInMemorySample(BufferData, FWave.ReadCount);

    Move(BufferData^, BufferData2^, FBufferDataSize);
    Move(BufferData^, BufferData3^, FBufferDataSize);
    for i := 0 to (FWave.ReadCount div DECIMATED_CACHE_DISTANCE) - 1 do
    begin
      lBuffer := TChannel(FWave.ChannelList[0]).Buffer;
      lValue:= 0;
      for j := 0 to Pred(DECIMATED_CACHE_DISTANCE) do
      begin
        lValue:= lValue + lBuffer[i * DECIMATED_CACHE_DISTANCE + j];
      end;
      DecimatedData[i]:= lValue / DECIMATED_CACHE_DISTANCE;
    end;

    lBPMDetect := TBPMDetect.Create(FWave.ChannelCount, FWave.SampleRate);
    try
      try
        SamplesRead := 0;
        for i := 0 to (FWave.ReadCount div DECIMATED_BLOCK_SAMPLES) - 1 do
        begin
          lBPMDetect.inputSamples(BufferData3 + i * DECIMATED_BLOCK_SAMPLES, DECIMATED_BLOCK_SAMPLES);
          inc(SamplesRead, DECIMATED_BLOCK_SAMPLES);
        end;
        lBPMDetect.inputSamples(BufferData3 + SamplesRead, FWave.ReadCount - SamplesRead);
        RealBPM:= lBPMDetect.getBpm;
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

    AutoMarkerProcess(False);
  end;

  DBLog('start TWaveForm.LoadSampleData');
end;

procedure TWaveForm.UnLoadSample;
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

function Twaveform.AddSlice(Location,
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

function Twaveform.SliceAt(Location: Integer; Margin: single): TMarker;
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

function Twaveform.NextSlice: TMarker;
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

function TWaveForm.LastSlice: TMarker;
begin
  result := TMarker(FSliceList.Last);
end;

function TWaveForm.FirstSlice: TMarker;
begin
  result := TMarker(FSliceList.First);
end;

procedure Twaveform.Sortslices;
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

procedure TWaveForm.RecalculateWarp;
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

function TWaveForm.GetSliceAt(Location: Integer; AMargin: single): TMarker;
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
procedure TWaveForm.VirtualLocation(AStartIndex: Integer; ALocation: single; var AFrameData: TFrameData);
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
function TWaveForm.StartVirtualLocation(ALocation: single): Integer;
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

{ TWaveFormScrollBox }

procedure TWaveform.AutoMarkerProcess(ACalculateStatistics: Boolean = True);
var
  i: Integer;
  WindowLength: Integer;
  lCurrentSlice: TMarker;
  lFirstSlice: Boolean;
  lSample: Integer;
begin
  lFirstSlice:= True;
  lCurrentSlice:= TMarker(FSliceList[0]);

  WindowLength:= 0;

  if ACalculateStatistics then
  begin
    for i:= 0 to (FWave.ReadCount div FWave.ChannelCount - 1) do
    begin
      BeatDetect.AudioProcess(BufferData2[i * FWave.ChannelCount]);
      if BeatDetect.BeatPulse then
      begin
        if Assigned(lCurrentSlice) and BeatDetect.TriggerOn then
        begin
          WindowLength:= 0;
        end;

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

  if RealBPM = 0 then RealBPM := 120;
  BarLength:= Round((4 * (60 / RealBPM)) * FWave.Samplerate);
  PatternLength := BarLength * FWave.ChannelCount;
  lSample:= BufferFrames - LoopStart.Location;
  // Just guessing...
  while (BarLength) > lSample do
    BarLength:= BarLength div 2;

  LoopStart.Location := 0;
  LoopEnd.Location:= (lSample div BarLength) * BarLength;
  LoopLength.Location := LoopEnd.Location - LoopStart.Location;
end;


{ TAddMarkerCommand }

procedure TAddMarkerCommand.DoExecute;
var
  lMementoMarker: TMarker;
  lMarker: TMarker;
begin
  // Check if there's a marker already
  lMarker := FWaveForm.AddSlice(Location, SLICE_NORMAL, True);

  if Assigned(lMarker) then
  begin
    lMementoMarker := TMarker.Create(ObjectOwner, NOT_MAPPED);
    lMementoMarker.Active := True;
    lMementoMarker.ObjectID := lMarker.ObjectID;
    lMementoMarker.Location := lMarker.Location;
    lMementoMarker.OrigLocation := lMarker.OrigLocation;
    Memento.Add(lMementoMarker);
  end;

  FWaveForm.SortSlices;
  FWaveForm.Notify;
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

    for j := 0 to Pred(FWaveForm.SliceList.Count) do
    begin
      lMarker := TMarker(FWaveForm.SliceList[j]);

      if lMementoMarker.ObjectID = lMarker.ObjectID then
      begin
        FWaveForm.SliceList.Remove(lMarker);
        break;
      end;
    end;
  end;

  FWaveForm.SortSlices;
  FWaveForm.Notify;
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

    FWaveForm.SortSlices;
    FWaveForm.Notify;
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

  FWaveForm.SortSlices;
  FWaveForm.Notify;
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
    FWaveForm.SliceList.Remove(lMarker);

    FWaveForm.SortSlices;
    FWaveForm.Notify;
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
    lMarker := FWaveForm.AddSlice(lMementoMarker.Location, SLICE_NORMAL, True);
    lMarker.Active := True;
    lMarker.ObjectID := lMementoMarker.ObjectID;
    lMarker.Location := lMementoMarker.Location;
    lMarker.OrigLocation := lMementoMarker.OrigLocation;
    lMarker.DecayRate := lMementoMarker.DecayRate;
  end;

  FWaveForm.SortSlices;
  FWaveForm.Notify;
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

    FWaveForm.Notify;
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

    FWaveForm.Notify;
  end;

  DBLog('end TToggleLockMarkerCommand.DoRollback');
end;

{ TUpdateWaveLoopStartCommand }

procedure TUpdateWaveLoopMarkerCommand.DoExecute;
begin
  DBLog('start TUpdateWaveLoopMarkerCommand.DoExecute');

  if Persist then
  begin
    // Save state
    case FDataType of
    ltStart: FOldLocation := FWaveForm.LoopStart.Location;
    ltEnd: FOldLocation := FWaveForm.LoopEnd.Location;
    ltLength: FOldLocation := FWaveForm.LoopLength.Location;
    end;
  end;

  // Assign
  case FDataType of
  ltStart:
  begin
    if FLocation < 0 then FLocation := 0;
    FWaveForm.LoopStart.Location := FLocation;
  end;
  ltEnd:
  begin
    if FLocation < 0 then FLocation := 0;
    FWaveForm.LoopEnd.Location := FLocation;
  end;
  ltLength: FWaveForm.LoopLength.Location := FLocation;
  end;

  // Update observers
  FWaveForm.Notify;
  FWaveForm.LoopStart.Notify;
  FWaveForm.LoopEnd.Notify;
  FWaveForm.LoopLength.Notify;

  DBLog('end TUpdateWaveLoopMarkerCommand.DoExecute');
end;

procedure TUpdateWaveLoopMarkerCommand.DoRollback;
begin
  DBLog('start TUpdateWaveLoopStartCommand.DoRollback');

  // Retrieve state
  FWaveForm.LoopStart.Location := FOldLocation;

  // Assign
  case FDataType of
  ltStart: FWaveForm.LoopStart.Location := FOldLocation;
  ltEnd: FWaveForm.LoopEnd.Location := FOldLocation;
  ltLength: FWaveForm.LoopLength.Location := FOldLocation;
  end;

  // Update observers
  FWaveForm.Notify;
  FWaveForm.LoopStart.Notify;
  FWaveForm.LoopEnd.Notify;
  FWaveForm.LoopLength.Notify;

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
  FWaveForm := TWaveForm(GObjectMapper.GetModelObject(ObjectOwner));
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

initialization

  BeatDetect := TBeatDetector.Create;
  BPMDetect := TBPMDetect.Create(1, 44100);

finalization
  BeatDetect.Free;
  BPMDetect.Free;

end.
