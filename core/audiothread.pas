unit audiothread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sndfile, contnrs;

type
  TArrayOfIntegers = array of Integer;

  { TAudioPeak }

  TAudioPeak = class
  private
    FData: PSingle;
    FDataSampleInfo: SF_INFO;
    FFileHandle: File;

    // Memory block with full wave but decimated by 64 for the gui
    FDecimatedData: PSingle;
    FDecimatedDataCount: Integer;

    FTransientMarkers: TArrayOfIntegers;
    FTransientMarkerCount: Integer;

    FPeakFilename: string;
    FFileHash: string[16];
    FBPM: Single;

    procedure Calculate;
    procedure CalculateBPM;
    procedure CalculateMD5Hash;
    procedure CalculateDecimatedData;
    procedure CalculateAutomaticMarkers;
  public
    function LoadFromFile(AFileName: string): Boolean;
    procedure SaveToFile;
    property Data: PSingle read FData write FData;
    property DataSampleInfo: SF_INFO read FDataSampleInfo write FDataSampleInfo;
    property DecimatedData: PSingle read FDecimatedData;
    property DecimatedDataCount: Integer read FDecimatedDataCount;
    property TransientMarkers: TArrayOfIntegers read FTransientMarkers;
    property PeakFileName: string read FPeakFilename;
    property BPM: Single read FBPM;
  end;

  { TAudioStreamBlock }

  TAudioStreamBlock = class
  private
    FBlockOffset: Integer;
    FBlockOffsetHalf: Integer;
    FSize: Integer;
    procedure SetBlockOffset(AValue: Integer);
    procedure SetSize(AValue: Integer);
  public
    Buffer: PSingle;
    property BlockOffset: Integer read FBlockOffset write SetBlockOffset;
    property BlockOffsetHalf: Integer read FBlockOffsetHalf write FBlockOffsetHalf;
    constructor Create;
    property Size: Integer read FSize write SetSize;
  end;

  TAudioStreamRequestState = (rsIdle, rsRequested, rsReady);

  TAudioStreamPages = array[0..1] of TAudioStreamBlock;

  { TAudioStream }

  TAudioStream = class
  private
    FSampleHandle: PSndFile;
    FSampleInfo: SF_INFO;

    FAudioPeak: TAudioPeak;
    FPeakFilename: String;
    FFilename: string;
    // Page flipping buffer
    FPage: TAudioStreamPages;
    FSize: Integer;
    FActivePage: Integer;
    FPageRequest: TAudioStreamRequestState;
    FRequestedFrameOffset: Integer;
    FBlockOffset: Integer;
    FChannelCount: Integer;
    FFrameCount: Integer;
    FSampleRate: Integer;

    function CursorOutsideBlock(AOffset: Integer): Boolean;
    procedure LoadBlock;
    procedure SetSize(AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    // Return the audio at offset. The function itself will calculate
    // the actual offset in the memory block
    function Audio(AOffset: Integer): Single;

    // Get the pointer to the beginning of the memoryblock with relative offset
    function AudioBlock(AOffset: Integer): PSingle;

    function SetFilename(AValue: string): Boolean;
    property Filename: string read FFilename;
    property BlockOffset: Integer read FBlockOffset;
    property Size: Integer read FSize write SetSize;
    property Page: TAudioStreamPages read FPage;
    property ActivePage: Integer read FActivePage write FActivePage;
    property PageRequest: TAudioStreamRequestState read FPageRequest write FPageRequest;

    property ChannelCount: Integer read FChannelCount;
    property FrameCount: Integer read FFrameCount;
    property SampleRate: Integer read FSampleRate;

    property AudioPeak: TAudioPeak read FAudioPeak;

  end;

  { TAudioStreamListSingleton }

  TAudioStreamListSingleton = class(TThread)
  private
    FStreams: TObjectList;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;

    // Creates a new stream and return its reference
    function CreateStream(AFileName: string): TAudioStream;

    // Destroyes an existing stream
    procedure DestroyStream(AAudioStream: TAudioStream);

    // Actual thread
    procedure Execute; override;
  end;

var
  GAudioStreamListSingleton: TAudioStreamListSingleton;


implementation

uses
  md5, bpmdetect, determinetransients, utils, globalconst, global, BaseUnix;

{ TAudioPeak }

procedure TAudioPeak.Calculate;
begin
  CalculateMD5Hash;
  CalculateDecimatedData;
  CalculateAutomaticMarkers;
  CalculateBPM;
end;

procedure TAudioPeak.CalculateMD5Hash;
begin
  // Calculate MD5 hash
  FFileHash := MD5Print(MD5File(FPeakFilename));
end;

procedure TAudioPeak.CalculateBPM;
var
  i, j: Integer;
  lSamplesRead: Integer;
  lBPMDetect: TBPMDetect;
begin
  // Calculated BPM
  lBPMDetect := TBPMDetect.Create(FDataSampleInfo.channels, FDataSampleInfo.samplerate);
  try
    try
      lSamplesRead := 0;
      for i := 0 to Pred(FDataSampleInfo.frames div DECIMATED_BLOCK_SAMPLES) do
      begin
        lBPMDetect.inputSamples(FData + i * DECIMATED_BLOCK_SAMPLES, DECIMATED_BLOCK_SAMPLES);
        Inc(lSamplesRead, DECIMATED_BLOCK_SAMPLES);
      end;
      lBPMDetect.inputSamples(FData + lSamplesRead, FDataSampleInfo.frames - lSamplesRead);
      FBPM := lBPMDetect.getBpm;
      DBLog('Calculated BPM: ' + FloatToStr(FBPM));

    except
      on e: exception do
      begin
        DBLog('Error: ' + e.Message);
      end;
    end;
  finally
    lBPMDetect.Free;
  end;
end;

procedure TAudioPeak.CalculateDecimatedData;
var
  i, j: Integer;
  lValue: Single;
  lLeftValue: Single;
  lRightValue: Single;
  lOffset: Integer;
begin
  // Calculate decimated version of audio by averaging
  FDecimatedDataCount := FDataSampleInfo.frames * FDataSampleInfo.channels div DECIMATED_CACHE_DISTANCE;
  if Assigned(FDecimatedData) then
  begin
    FreeMem(FDecimatedData);
  end;
  FDecimatedData := GetMem(FDecimatedDataCount * SizeOf(Single));

  if FDataSampleInfo.channels = 1 then
  begin
    lOffset := 0;
    for i := 0 to Pred(FDecimatedDataCount) do
    begin
      lValue := 0;
      for j := 0 to Pred(DECIMATED_CACHE_DISTANCE) do
      begin
        lValue := lValue + FData[lOffset + j];
      end;
      Inc(lOffset, DECIMATED_CACHE_DISTANCE);
      FDecimatedData[i] := lValue / DECIMATED_CACHE_DISTANCE;
  //    writeln(Format('FDecimatedData[%d] = %f', [i, FDecimatedData[i]]));
    end;
  end
  else if FDataSampleInfo.channels = 2 then
  begin
    lOffset := 0;
    for i := 0 to Pred(FDecimatedDataCount) do
    begin
      lLeftValue := 0;
      lRightValue := 0;
      for j := 0 to Pred(DECIMATED_CACHE_DISTANCE) do
      begin
        lLeftValue := lLeftValue + FData[lOffset + j * 2];
        lRightValue := lRightValue + FData[lOffset + j * 2 + 1];
      end;
      Inc(lOffset, DECIMATED_CACHE_DISTANCE);
      FDecimatedData[i] := lLeftValue / DECIMATED_CACHE_DISTANCE;
      FDecimatedData[i + 1] := lRightValue / DECIMATED_CACHE_DISTANCE;
  //    writeln(Format('FDecimatedData[%d] = %f', [i, FDecimatedData[i]]));
    end;
  end;
end;

procedure TAudioPeak.CalculateAutomaticMarkers;
var
  i: Integer;
  WindowLength: Integer;
  lCurrentSlice: TMarker;
  lFirstSlice: Boolean;
  lDetermineTransients: TDetermineTransients;
  lDoAddSlice: Boolean;
  lTransientValue: Integer;
begin
  WindowLength := 0;

  lDetermineTransients := TDetermineTransients.Create(FDataSampleInfo.samplerate);
  try
    lDetermineTransients.Sensitivity := 1.5;
    lDetermineTransients.Process(FData, FDataSampleInfo.frames, FDataSampleInfo.channels);
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
        SetLength(FTransientMarkers, Succ(Length(FTransientMarkers)));
        lTransientValue := lDetermineTransients.Transients[i];
        FTransientMarkers[High(FTransientMarkers)] := lTransientValue;
      end;
    end;
    FTransientMarkerCount := Length(FTransientMarkers);
  finally
    lDetermineTransients.Free;
  end;
end;

function TAudioPeak.LoadFromFile(AFileName: string): Boolean;
var
  lFileStream: TFileStream;
begin
  FPeakFilename := ChangeFileExt(AFilename, '.pk');

  {if FileExists(FPeakFilename) then
  begin
    lFileStream := TFileStream.Create(FPeakFilename, fmOpenRead or fmShareDenyNone);
    try
      lFileStream.ReadBuffer(FBPM, SizeOf(FBPM));
      FFileHash := lFileStream.ReadAnsiString;
      lFileStream.ReadBuffer(FDecimatedDataCount, SizeOf(FDecimatedDataCount));
      lFileStream.ReadBuffer(FDecimatedData, FDecimatedDataCount * SizeOf(Single));
      lFileStream.ReadBuffer(FTransientMarkerCount, SizeOf(Integer));
      lFileStream.ReadBuffer(FTransientMarkers, FTransientMarkerCount * SizeOf(Single));
    finally
      lFileStream.Free;
    end;
  end
  else }
  begin
    Calculate;
    Result := False;
  end;
end;

procedure TAudioPeak.SaveToFile;
var
  lFileStream: TFileStream;
  lFlags: word;
begin
  exit;
  try
    if FPeakFilename <> '' then
    begin
      if FileExists(FPeakFilename) then
      begin
        DeleteFile(FPeakFilename);
      end;

      DBLog('1');
      lFileStream := TFileStream.Create(FPeakFilename, fmCreate);
      DBLog('2');
      try
        lFileStream.WriteBuffer(FBPM, SizeOf(FBPM));
        DBLog('3');
        lFileStream.WriteAnsiString(FFileHash);
        DBLog('4');
        lFileStream.WriteBuffer(FDecimatedDataCount, SizeOf(FDecimatedDataCount));
        DBLog('5');
        lFileStream.WriteBuffer(FDecimatedData[0], FDecimatedDataCount * SizeOf(Single));
        DBLog('6');
        lFileStream.WriteBuffer(FTransientMarkerCount, SizeOf(Integer));
        DBLog('7');
        lFileStream.WriteBuffer(FTransientMarkers[0], FTransientMarkerCount * SizeOf(Integer));
        DBLog('8');
      finally
        DBLog('9');
        lFileStream.Free;
        DBLog('10');
      end;
    end
  except
    on e: exception do
    begin
      DBLog(e.Message);
    end;
  end;
end;

{ TAudioStreamBlock }

procedure TAudioStreamBlock.SetSize(AValue: Integer);
begin
  if FSize=AValue then Exit;
  FSize:=AValue;

  if Assigned(Buffer) then
  begin
    FreeMem(Buffer);
  end;
  Getmem(Buffer, FSize * SizeOf(Single) * STEREO);
end;

procedure TAudioStreamBlock.SetBlockOffset(AValue: Integer);
begin
  if FBlockOffset=AValue then Exit;
  FBlockOffset:=AValue;
end;

constructor TAudioStreamBlock.Create;
begin
  // 1 seconds buffer by default
  Size := Round(GSettings.SampleRate);
end;

{ TAudioStreamListSingleton }

destructor TAudioStreamListSingleton.Destroy;
begin
  FStreams.Free;

  inherited Destroy;
end;

constructor TAudioStreamListSingleton.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);

  FStreams := TObjectList.Create(False);
end;

function TAudioStreamListSingleton.CreateStream(AFileName: string): TAudioStream;
var
  lAudioStream: TAudioStream;
begin
  lAudioStream := TAudioStream.Create;
  if lAudioStream.SetFilename(AFileName) then
  begin
    FStreams.Add(lAudioStream);

    Result := lAudioStream;
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TAudioStreamListSingleton.DestroyStream(AAudioStream: TAudioStream);
begin
  FStreams.Remove(AAudioStream);
end;

procedure TAudioStreamListSingleton.Execute;
var
  i: Integer;
  lAudioStream: TAudioStream;
begin
  while not Terminated do
  begin
    // Let thread sleep a while
    Sleep(250);

    // For each stream do
    for i := 0 to Pred(FStreams.Count) do
    begin
      lAudioStream := TAudioStream(FStreams[i]);

      if Assigned(lAudioStream) then
      begin
        if lAudioStream.PageRequest = rsRequested then
        begin
          lAudioStream.LoadBlock;
          lAudioStream.PageRequest := rsReady
        end;
      end;
    end;
  end;
end;

{ TAudioStream }

procedure TAudioStream.SetSize(AValue: Integer);
var
  i: Integer;
begin
  if FSize=AValue then exit;;
  FSize:=AValue;

  for i := 0 to 1 do
  begin
    FPage[i].Size := FSize;
  end;
end;

constructor TAudioStream.Create;
begin
  inherited Create;

  FAudioPeak := TAudioPeak.Create;

  FPage[0] := TAudioStreamBlock.Create;
  FPage[1] := TAudioStreamBlock.Create;

  FActivePage := 0;
end;

destructor TAudioStream.Destroy;
begin
  sf_close(FSampleHandle);

  FAudioPeak.Free;

  inherited Destroy;
end;

function TAudioStream.CursorOutsideBlock(AOffset: Integer): Boolean;
var
  lSampleOffset: Integer;
begin
  lSampleOffset := AOffset * FChannelCount;

  Result :=
    (lSampleOffset > FPage[FActivePage].BlockOffsetHalf) or
    (lSampleOffset <= FPage[FActivePage].BlockOffset);
end;

// Calculate offset in memory block and return result
function TAudioStream.Audio(AOffset: Integer): Single;
begin
  FRequestedFrameOffset := AOffset;

  if FPageRequest = rsReady then
  begin
    FActivePage := 1 - FActivePage;

    FPageRequest := rsIdle;
  end
  else
  begin
    if CursorOutsideBlock(AOffset) then
    begin
      FRequestedFrameOffset := AOffset;

      // request new page, this should be picked up in the thread and a new block
      // should be loaded and FNewPageRequested should be set to false subsequently
      FPageRequest := rsRequested;
    end;
  end;

  Result := FPage[FActivePage].Buffer[(AOffset - FPage[FActivePage].BlockOffset) * FChannelCount];
end;

function TAudioStream.AudioBlock(AOffset: Integer): PSingle;
begin
  FRequestedFrameOffset := AOffset;

  if FPageRequest = rsReady then
  begin
    DBLog('TAudioStream.Audio, FPageRequest = rsReady');
    FActivePage := 1 - FActivePage;

    FPageRequest := rsIdle;
  end
  else
  begin
    if CursorOutsideBlock(AOffset) then
    begin
      FRequestedFrameOffset := AOffset;

      // request new page, this should be picked up in the thread and a new block
      // should be loaded and FNewPageRequested should be set to false subsequently
      FPageRequest := rsRequested;
    end;
  end;

  Result := @FPage[FActivePage].Buffer[(AOffset - FPage[FActivePage].BlockOffset) * FChannelCount];
  DBLog(Format('TAudioStream.AudioBlock %d BlockOffset %d', [AOffset, FPage[FActivePage].BlockOffset]));
end;

procedure TAudioStream.LoadBlock;
var
  lPage: TAudioStreamBlock;
  lLoadingPage: Integer;
  lReadCount: Integer;
begin
  lLoadingPage := 1 - FActivePage;
  lPage := TAudioStreamBlock(FPage[lLoadingPage]);
  lPage.BlockOffset := FRequestedFrameOffset;
  lPage.BlockOffsetHalf := lPage.BlockOffset + lPage.Size div 2;
  sf_seek(FSampleHandle, lPage.BlockOffset, SEEK_SET);
  lReadCount := sf_read_float(FSampleHandle, lPage.Buffer, lPage.Size * FChannelCount);
  FPageRequest := rsReady;
end;

function TAudioStream.SetFilename(AValue: string): Boolean;
var
  lSampleInfo: SF_INFO;
begin
  if FFilename=AValue then Exit;
  FFilename:=AValue;

  FSampleHandle := sf_open(StringToPChar(FFilename), SFM_READ, lSampleInfo);

  if Assigned(FSampleHandle) then
  begin
    FChannelCount := lSampleInfo.channels;
    FFrameCount := lSampleInfo.frames;
    FSampleRate := lSampleInfo.samplerate;

    if Assigned(FAudioPeak.Data) then
    begin
      FreeMem(FAudioPeak.Data);
    end;
    FAudioPeak.Data := GetMem(lSampleInfo.frames * lSampleInfo.channels * SizeOf(Single));
    sf_read_float(FSampleHandle, FAudioPeak.Data, lSampleInfo.frames * lSampleInfo.channels);

    FAudioPeak.DataSampleInfo := lSampleInfo;
    FAudioPeak.LoadFromFile(FFilename);
    FAudioPeak.SaveToFile;

    Result := True;
  end
  else
  begin
    DBLog(sf_strerror(FSampleHandle));

    Result := False;
  end
end;

initialization
  GAudioStreamListSingleton := TAudioStreamListSingleton.Create(False);
finalization
  GAudioStreamListSingleton.Free;

end.

