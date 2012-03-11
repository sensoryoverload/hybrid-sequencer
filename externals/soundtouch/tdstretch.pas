unit tdstretch;

{$mode objfpc}{$H+}

interface

uses
  sysutils, fifosamplebuffer, fifosamplepipe, mmx, beattrigger, utils;

////////////////////////////////////////////////////////////////////////////////
///
/// Sampled sound tempo changer/time stretch algorithm. Changes the sound tempo
/// while maintaining the original pitch by using a time domain WSOLA-like method
/// with several performance-increasing tweaks.
///
/// Note : MMX/SSE optimized functions reside in separate, platform-specific files
/// 'mmx_optimized.cpp' and 'sse_optimized.cpp'
///
/// Author        : Copyright (c) Olli Parviainen
/// Author e-mail : oparviai 'at' iki.fi
/// SoundTouch WWW: http://www.surina.net/soundtouch
///
////////////////////////////////////////////////////////////////////////////////
//
// Last changed  : $Date$
// File revision : $Revision: 4 $
//
// $Id$
//
////////////////////////////////////////////////////////////////////////////////
//
// License :
//
//  SoundTouch audio processing library
//  Copyright (c) Olli Parviainen
//
//  This library is free software; you can redistribute it and/or
//  modify it under the terms of the GNU Lesser General Public
//  License as published by the Free Software Foundation; either
//  version 2.1 of the License, or (at your option) any later version.
//
//  This library is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public
//  License along with this library; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
////////////////////////////////////////////////////////////////////////////////

const
  FLT_MIN = 1.17549435e-38;

/// Default values for sound processing parameters:
/// Notice that the default parameters are tuned for contemporary popular music
/// processing. For speech processing applications these parameters suit better:
///     #define DEFAULT_SEQUENCE_MS     40
///     #define DEFAULT_SEEKWINDOW_MS   15
///     #define DEFAULT_OVERLAP_MS      8
///

/// Default length of a single processing sequence, in milliseconds. This determines to how
/// long sequences the original sound is chopped in the time-stretch algorithm.
///
/// The larger this value is, the lesser sequences are used in processing. In principle
/// a bigger value sounds better when slowing down tempo, but worse when increasing tempo
/// and vice versa.
///
/// Giving this value for the sequence length sets automatic parameter value
/// according to tempo setting (recommended)
USE_AUTO_SEQUENCE_LEN = 0;
/// Increasing this value reduces computational burden & vice versa.
//#define DEFAULT_SEQUENCE_MS         40
DEFAULT_SEQUENCE_MS = USE_AUTO_SEQUENCE_LEN;



/// Seeking window default length in milliseconds for algorithm that finds the best possible
/// overlapping location. This determines from how wide window the algorithm may look for an
/// optimal joining location when mixing the sound sequences back together.
///
/// The bigger this window setting is, the higher the possibility to find a better mixing
/// position will become, but at the same time large values may cause a "drifting" artifact
/// because consequent sequences will be taken at more uneven intervals.
///
/// If there's a disturbing artifact that sounds as if a constant frequency was drifting
/// around, try reducing this setting.
///
/// Giving this value for the seek window length sets automatic parameter value
/// according to tempo setting (recommended)
  USE_AUTO_SEEKWINDOW_LEN = 0;
/// Increasing this value increases computational burden & vice versa.
//#define DEFAULT_SEEKWINDOW_MS       15
  DEFAULT_SEEKWINDOW_MS = USE_AUTO_SEEKWINDOW_LEN;


/// Overlap length in milliseconds. When the chopped sound sequences are mixed back together,
/// to form a continuous sound stream, this parameter defines over how long period the two
/// consecutive sequences are let to overlap each other.
///
/// This shouldn't be that critical parameter. If you reduce the DEFAULT_SEQUENCE_MS setting
/// by a large amount, you might wish to try a smaller value on this.
///
/// Increasing this value increases computational burden & vice versa.
  DEFAULT_OVERLAP_MS = 8;

  // Adjust tempo param according to tempo, so that variating processing sequence length is used
  // at varius tempo settings, between the given low...top limits
  AUTOSEQ_TEMPO_LOW = 0.25; //0.9;     // auto setting low tempo range (-50%)
  AUTOSEQ_TEMPO_TOP = 4.0; //1.07;     // auto setting top tempo range (+100%)

  // sequence-ms setting values at above low & top tempo
  AUTOSEQ_AT_MIN =    150; //120.0;
  AUTOSEQ_AT_MAX =    25; //30.0;
  AUTOSEQ_K =         ((AUTOSEQ_AT_MAX - AUTOSEQ_AT_MIN) / (AUTOSEQ_TEMPO_TOP - AUTOSEQ_TEMPO_LOW));
  AUTOSEQ_C =         (AUTOSEQ_AT_MIN - (AUTOSEQ_K) * (AUTOSEQ_TEMPO_LOW));

  // seek-window-ms setting values at above low & top tempo
  AUTOSEEK_AT_MIN =   20; // 20.0;
  AUTOSEEK_AT_MAX =   3; //3.0;
  AUTOSEEK_K =        ((AUTOSEEK_AT_MAX - AUTOSEEK_AT_MIN) / (AUTOSEQ_TEMPO_TOP - AUTOSEQ_TEMPO_LOW));
  AUTOSEEK_C =        (AUTOSEEK_AT_MIN - (AUTOSEEK_K) * (AUTOSEQ_TEMPO_LOW));



{*****************************************************************************
 *
 * Constant definitions
 *
 *****************************************************************************}

// Table for the hierarchical mixing position seeking algorithm
const
  _scanOffsets: Array[0..4, 0..23] of Integer = (
    ( 124,  186,  248,  310,  372,  434,  496,  558,  620,  682,  744, 806,
      868,  930,  992, 1054, 1116, 1178, 1240, 1302, 1364, 1426, 1488,   0),
    (-100,  -75,  -50,  -25,   25,   50,   75,  100,    0,    0,    0,   0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,   0),
    ( -20,  -15,  -10,   -5,    5,   10,   15,   20,    0,    0,    0,   0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,   0),
    (  -4,   -3,   -2,   -1,    1,    2,    3,    4,    0,    0,    0,   0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,   0),
    ( 121,  114,   97,  114,   98,  105,  108,   32,  104,   99,  117,  111,
      116,  100,  110,  117,  111,  115,    0,    0,    0,    0,    0,   0));

type
  /// Class that does the time-stretch (tempo change) effect for the processed
  /// sound.

  { TTDStretch }

  TTDStretch = class(TFIFOProcessor)
  private
    channels: Integer;
    sampleReq: Integer;
    tempo: Single;

    pMidBuffer: PSingle;
    pRefMidBuffer: PSingle;
    overlapLength: Integer;
    seekLength: Integer;
    inverseSeekLength: Single;
    seekWindowLength: Integer;
    overlapDividerBits: Integer;
    slopingDivider: Integer;
    nominalSkip: Single;
    skipFract: Single;
    outputBuffer: TFIFOSampleBuffer;
    inputBuffer: TFIFOSampleBuffer;
    bQuickSeek: Boolean;
    sampleRate: Integer;
    sequenceMs: Integer;
    seekWindowMs: Integer;
    overlapMs: Integer;
    bAutoSeqSetting: Boolean;
    bAutoSeekSetting: Boolean;
    beatdetect: TBeatDetector;
    transient: Boolean;

    procedure acceptNewOverlapLength(newOverlapLength: Integer);

    procedure calculateOverlapLength(aoverlapInMsec: Integer);

    function calcCrossCorrStereo(const mixingPos: PSingle; const compare: PSingle): double;
    function calcCrossCorrMono(const mixingPos: PSingle; const compare: PSingle): double;
    function seekBestOverlapPositionStereo(const refPos: PSingle): Integer;
    function seekBestOverlapPositionStereoEfficient(const refPos: PSingle): Integer;
    function seekBestOverlapPositionStereoQuick(const refPos: PSingle): Integer;
    function seekBestOverlapPositionMono(const refPos: PSingle): Integer;
    function seekBestOverlapPositionMonoEfficient(const refPos: PSingle): Integer;
    function seekBestOverlapPositionMonoQuick(const refPos: PSingle): Integer;
    function seekBestOverlapPosition(const refPos: PSingle): Integer;

    procedure overlapStereo(poutput: PSingle; const pinput: PSingle);
    procedure overlapMono(poutput: PSingle; const pinput: PSingle);

    function detectTransient(const refPos: PSingle): Integer;

    procedure clearMidBuffer;
    procedure overlap(poutput: PSingle; const pinput: PSingle; ovlPos: longword);

    procedure precalcCorrReferenceMono;
    procedure precalcCorrReferenceStereo;

    procedure calcSeqParameters;

    /// Changes the tempo of the given sound samples.
    /// Returns amount of samples returned in the "output" buffer.
    /// The maximum amount of samples that can be returned at a time is set by
    /// the 'set_returnBuffer_size' function.

  public
    procedure processSamples;
    constructor Create; override;
    destructor Destroy; override;

    /// Returns the output buffer object
    function getOutput: TFIFOSamplePipe;

    /// Returns the input buffer object
    function getInput: TFIFOSamplePipe;

    /// Sets new target tempo. Normal tempo = 'SCALE', smaller values represent slower
    /// tempo, larger faster tempo.
    procedure setTempo(newTempo: Single);

    /// Returns nonzero if there aren't any samples available for outputting.
    procedure clear; override;

    /// Clears the input buffer
    procedure clearInput;

    /// Sets the number of channels, 1 = mono, 2 = stereo
    procedure setChannels(numChannels: Integer);

    /// Enables/disables the quick position seeking algorithm. Zero to disable,
    /// nonzero to enable
    procedure enableQuickSeek(enable: Boolean);

    /// Returns nonzero if the quick seeking algorithm is enabled.
    function isQuickSeekEnabled: Boolean;

    /// Sets routine control parameters. These control are certain time constants
    /// defining how the sound is stretched to the desired duration.
    //
    /// 'sampleRate' = sample rate of the sound
    /// 'sequenceMS' = one processing sequence length in milliseconds
    /// 'seekwindowMS' = seeking window length for scanning the best overlapping
    ///      position
    /// 'overlapMS' = overlapping length
    procedure setParameters(
      asampleRate: Integer;           ///< Samplerate of sound being processed (Hz)
      asequenceMS: Integer = -1;      ///< Single processing sequence length (ms)
      aseekwindowMS: Integer = -1;    ///< Offset seeking window length (ms)
      aoverlapMS: Integer = -1        ///< Sequence overlapping length (ms)
      );

    /// Get routine control parameters, see setParameters() function.
    /// Any of the parameters to this function can be NULL, in such case corresponding parameter
    /// value isn't returned.
    procedure getParameters(apSampleRate: PInteger; apSequenceMs: PInteger; apSeekWindowMs: PInteger; apOverlapMs: PInteger);

    /// Adds 'numsamples' pcs of samples from the 'samples' memory position into
    /// the input of the object.
    procedure putSamples(
            const samples: PSingle;  ///< Input sample data
            anumSamples: longword                         ///< Number of samples in 'samples' so that one sample
                                                    ///< contains both channels if stereo
            ); override;

    function getInputSampleReq: Integer;
    function getOutputBatchSize: Integer;
    procedure enableTransientDetection(enable: Integer);
  end;

implementation

{*****************************************************************************
 *
 * Implementation of the class 'TDStretch'
 *
 *****************************************************************************}

(*
*)

constructor TTDStretch.Create;
begin
  outputBuffer := TFIFOSampleBuffer.Create(1);
  inputBuffer := TFIFOSampleBuffer.Create(1);

  inherited Create(outputBuffer);

  beatdetect := TBeatDetector.Create;

  beatdetect.setSampleRate(44100);
  beatdetect.setThresHold(0.4);
  beatdetect.setFilterCutOff(2000);

  transient := False;

  bQuickSeek := FALSE;
  channels := 1;

  pMidBuffer := nil;
  overlapLength := 0;

  bAutoSeqSetting := TRUE;
  bAutoSeekSetting := TRUE;

  skipFract := 0;

  setParameters(44100, DEFAULT_SEQUENCE_MS, DEFAULT_SEEKWINDOW_MS, DEFAULT_OVERLAP_MS);
  setTempo(1.0);

  clear;
end;

destructor TTDStretch.Destroy;
begin
  FreeMem(pMidBuffer);

  beatdetect.Free;

  inherited Destroy;
end;

function TTDStretch.getOutput: TFIFOSamplePipe;
begin
  Result := outputBuffer;
end;

function TTDStretch.getInput: TFIFOSamplePipe;
begin
  Result := inputBuffer;
end;

// Sets routine control parameters. These control are certain time constants
// defining how the sound is stretched to the desired duration.
//
// 'sampleRate' = sample rate of the sound
// 'sequenceMS' = one processing sequence length in milliseconds (default = 82 ms)
// 'seekwindowMS' = seeking window length for scanning the best overlapping 
//      position (default = 28 ms)
// 'overlapMS' = overlapping length (default = 12 ms)

procedure TTDStretch.setParameters(aSampleRate, aSequenceMS,
  aSeekWindowMS, aOverlapMS: Integer);
begin
  // accept only positive parameter values - if zero or negative, use old values instead
  if aSampleRate > 0 then Self.sampleRate := aSampleRate;
  if aOverlapMS > 0 then Self.overlapMs := aOverlapMS;

  if aSequenceMS > 0 then
  begin
    Self.sequenceMs := aSequenceMS;
    bAutoSeqSetting := FALSE;
  end
  else if aSequenceMS = 0 then
  begin
    // if zero, use automatic setting
    bAutoSeqSetting := TRUE;
  end;

  if aSeekWindowMS > 0 then
  begin
    Self.seekWindowMs := aSeekWindowMS;
    bAutoSeekSetting := FALSE;
  end
  else if aSeekWindowMS = 0 then
  begin
    // if zero, use automatic setting
    bAutoSeekSetting := TRUE;
  end;

  calcSeqParameters;

  calculateOverlapLength(overlapMs);

  // set tempo to recalculate 'sampleReq'
  setTempo(tempo);
end;

/// Get routine control parameters, see setParameters() function.
/// Any of the parameters to this function can be NULL, in such case corresponding parameter
/// value isn't returned.
procedure TTDStretch.getParameters(apSampleRate, apSequenceMs, apSeekWindowMs, apOverlapMs: PInteger);
begin
  if Assigned(apSampleRate) then
  begin
    apSampleRate^ := sampleRate;
  end;

  if Assigned(apSequenceMs) then
  begin
    if bAutoSeqSetting then
      apSequenceMs^ := USE_AUTO_SEQUENCE_LEN
    else
      apSequenceMs^ := sequenceMs;
  end;

  if Assigned(apSeekWindowMs) then
  begin
    if bAutoSeekSetting then
      apSeekWindowMs^ := USE_AUTO_SEEKWINDOW_LEN
    else
      apSeekWindowMs^ := seekWindowMs;
  end;

  if Assigned(apOverlapMs) then
  begin
    apOverlapMs^ := overlapMs;
  end;
end;

// Overlaps samples in 'midBuffer' with the samples in 'pInput'
procedure TTDStretch.overlapMono(pOutput: PSingle; const pInput: PSingle);
var
  i, itemp: Integer;
  DivByOverlapLength: Single;
begin
  // Multiplication is a bit faster than division
  DivByOverlapLength := 1 / overlapLength;

  for i := 0 to Pred(overlapLength) do
  begin
    itemp := overlapLength - i;
    pOutput[i] := (pInput[i] * i + pMidBuffer[i] * itemp ) * DivByOverlapLength;
  end;
end;

function TTDStretch.detectTransient(const refPos: PSingle): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Pred(seekLength) do
  begin
    if transient then
    begin
      beatdetect.AudioProcess((refPos + i)^);
      if beatdetect.BeatPulse then
      begin
        Result := i;
        break;
      end;
    end;
  end;
end;

procedure TTDStretch.clearMidBuffer;
begin
  FillByte(pMidBuffer^, (2 * sizeof(Single) * overlapLength), 0);
end;

procedure TTDStretch.clearInput;
begin
  inputBuffer.clear;
  clearMidBuffer;
end;

// Clears the sample buffers
procedure TTDStretch.clear;
begin
  outputBuffer.clear;
  clearInput;
end;

// Enables/disables the quick position seeking algorithm. Zero to disable, nonzero
// to enable
procedure TTDStretch.enableQuickSeek(enable: Boolean);
begin
  bQuickSeek := enable;
end;

// Returns nonzero if the quick seeking algorithm is enabled.
function TTDStretch.isQuickSeekEnabled: Boolean;
begin
  Result := bQuickSeek;
end;

// Seeks for the optimal overlap-mixing position.
function TTDStretch.seekBestOverlapPosition(const refPos: PSingle): Integer;
begin
  if channels = 2 then
  begin
    // stereo sound
    if bQuickSeek then
    begin
      Result := seekBestOverlapPositionStereoQuick(refPos);
//      Result := seekBestOverlapPositionStereoEfficient(refPos);
    end
    else
    begin
//      Result := seekBestOverlapPositionStereoQuick(refPos);
      Result := seekBestOverlapPositionStereo(refPos);
    end;
  end
  else
  begin
    // mono sound
    if bQuickSeek then
    begin
      Result := seekBestOverlapPositionMonoQuick(refPos);
//      Result := seekBestOverlapPositionMonoEfficient(refPos);
    end
    else
    begin
//      Result := seekBestOverlapPositionMonoQuick(refPos);
      Result := seekBestOverlapPositionMono(refPos);
    end;
  end;
end;

// Overlaps samples in 'midBuffer' with the samples in 'pInputBuffer' at position
// of 'ovlPos'.
procedure TTDStretch.overlap(pOutput: PSingle; const pInput: PSingle; ovlPos: longword);
begin
  if channels = 2 then
  begin
    // stereo sound
    overlapStereo(pOutput, pInput + 2 * ovlPos);
  end
  else
  begin
    // mono sound.
    overlapMono(pOutput, pInput + ovlPos);
  end;
end;

// Seeks for the optimal overlap-mixing position. The 'stereo' version of the
// routine
//
// The best position is determined as the position where the two overlapped
// sample sequences are 'most alike', in terms of the highest cross-correlation
// value over the overlapping period
function TTDStretch.seekBestOverlapPositionStereo(const refPos: PSingle): Integer;
var
  bestOffs: Integer;
  bestCorr, corr: double;
  i: Integer;
  tmp: double;
begin
  // Slopes the amplitudes of the 'midBuffer' samples
  precalcCorrReferenceStereo;

  bestCorr := FLT_MIN;
  bestOffs := 0;

  // Scans for the best correlation value by testing each possible position
  // over the permitted range.
  for i := 0 to Pred(seekLength) do
  begin
    // Calculates correlation value for the mixing position corresponding
    // to 'i'
    corr := calcCrossCorrStereo(refPos + 2 * i, pRefMidBuffer);
    // heuristic rule to slightly favour values close to mid of the range
    tmp := (2 * i - seekLength) * inverseSeekLength;
    corr := ((corr + 0.1) * (1.0 - 0.25 * tmp * tmp));

    // Checks for the highest correlation value
    if corr > bestCorr then
    begin
      bestCorr := corr;
      bestOffs := i;
    end;
  end;

  Result := bestOffs;
end;

function TTDStretch.seekBestOverlapPositionStereoEfficient(const refPos: PSingle): Integer;
var
  i, j: Integer;
  adder: single;
  main_adder: single;
  position: Integer;
  pRefMidBuffer_iterate: PSingle;
  refPos_iterate: PSingle;
begin
  // Slopes the amplitude of the 'midBuffer' samples
  precalcCorrReferenceStereo;

  main_adder := 99999999;
  for i := 0 to Pred(2 * seekLength) do
  begin
    adder := 0;
    pRefMidBuffer_iterate := pRefMidBuffer;
    refPos_iterate := refPos + i * 2;
    for j := 0 to Pred(2 * overlapLength) do
    begin
      adder :=
        (pRefMidBuffer_iterate^ + (pRefMidBuffer_iterate + 1)^) -
        (refPos_iterate^ + (refPos_iterate + 1)^);

      Inc(pRefMidBuffer_iterate, 2);
      Inc(refPos_iterate, 2);
{      adder +=
        (pRefMidBuffer[j] + pRefMidBuffer[j + 1]) -
        ((refPos + i)[j] + (refPos + i)[j + 1]);}
    end;
    if adder < main_adder then
    begin
      main_adder := adder;
      position := i;
    end;
  end;
  if main_adder = 99999999 then
    Result := 0
  else
    Result := position;
end;

function TTDStretch.seekBestOverlapPositionMonoEfficient(const refPos: PSingle): Integer;
var
  i, j: Integer;
  adder: single;
  main_adder: single;
  position: Integer;
  pRefMidBuffer_iterate: PSingle;
  refPos_iterate: PSingle;
begin
  // Slopes the amplitude of the 'midBuffer' samples
  precalcCorrReferenceMono;

  main_adder := 99999999;
  for i := 0 to Pred(seekLength) do
  begin
    adder := 0;
    pRefMidBuffer_iterate := pRefMidBuffer;
    refPos_iterate := refPos + i;
    for j := 0 to Pred(overlapLength) do
    begin
      adder := pRefMidBuffer_iterate^ - refPos_iterate^;
      Inc(pRefMidBuffer_iterate);
      Inc(refPos_iterate);
    end;
    if adder < main_adder then
    begin
      main_adder := adder;
      position := i;
    end;
  end;

  if main_adder = 99999999 then
    Result := 0
  else
    Result := position;
end;

// Seeks for the optimal overlap-mixing position. The 'stereo' version of the
// routine
//
// The best position is determined as the position where the two overlapped
// sample sequences are 'most alike', in terms of the highest cross-correlation
// value over the overlapping period
function TTDStretch.seekBestOverlapPositionStereoQuick(const refPos: PSingle): Integer;
var
  j: Integer;
  bestOffs: Integer;
  bestCorr, corr: double;
  scanCount, corrOffset, tempOffset: Integer;
  tmp: double;
begin
  // Slopes the amplitude of the 'midBuffer' samples
  precalcCorrReferenceStereo;

  bestCorr := FLT_MIN;
  bestOffs := _scanOffsets[0, 0];
  corrOffset := 0;
  tempOffset := 0;

  // Scans for the best correlation value using four-pass hierarchical search.
  //
  // The look-up table 'scans' has hierarchical position adjusting steps.
  // In first pass the routine searhes for the highest correlation with
  // relatively coarse steps, then rescans the neighbourhood of the highest
  // correlation with better resolution and so on.
  for scanCount := 0 to 4 do
  begin
    j := 0;
    while _scanOffsets[scanCount, j] > 0 do
    begin
      tempOffset := corrOffset + _scanOffsets[scanCount, j];
      if tempOffset >= seekLength then break;

      // Calculates correlation value for the mixing position corresponding
      // to 'tempOffset'
      corr := calcCrossCorrStereo(refPos + 2 * tempOffset, pRefMidBuffer);
      // heuristic rule to slightly favour values close to mid of the range
      tmp := (2 * tempOffset - seekLength) * inverseSeekLength;
      corr := ((corr + 0.1) * (1.0 - 0.25 * tmp * tmp));

      // Checks for the highest correlation value
      if corr > bestCorr then
      begin
        bestCorr := corr;
        bestOffs := tempOffset;
      end;
      j := j + 1;
    end;
    corrOffset := bestOffs;
  end;

  Result := bestOffs;
end;

// Seeks for the optimal overlap-mixing position. The 'mono' version of the
// routine
//
// The best position is determined as the position where the two overlapped
// sample sequences are 'most alike', in terms of the highest cross-correlation
// value over the overlapping period
function TTDStretch.seekBestOverlapPositionMono(const refPos: PSingle): Integer;
var
  bestOffs: Integer;
  bestCorr, corr: double;
  tempOffset: Integer;
  compare: PSingle;
  tmp: double;
begin
  // Slopes the amplitude of the 'midBuffer' samples
  precalcCorrReferenceMono;

  bestCorr := FLT_MIN;
  bestOffs := 0;

  // Scans for the best correlation value by testing each possible position
  // over the permitted range.
  for tempOffset := 0 to Pred(seekLength) do
  begin
    compare := refPos + tempOffset;

    // Calculates correlation value for the mixing position corresponding
    // to 'tempOffset'
    corr := calcCrossCorrMono(pRefMidBuffer, compare);
    // heuristic rule to slightly favour values close to mid of the range
    tmp := (2 * tempOffset - seekLength) * inverseSeekLength;
    corr := ((corr + 0.1) * (1.0 - 0.25 * tmp * tmp));

    // Checks for the highest correlation value
    if corr > bestCorr then
    begin
      bestCorr := corr;
      bestOffs := tempOffset;
    end;
  end;

  Result := bestOffs;
end;


// Seeks for the optimal overlap-mixing position. The 'mono' version of the
// routine
//
// The best position is determined as the position where the two overlapped
// sample sequences are 'most alike', in terms of the highest cross-correlation
// value over the overlapping period
function TTDStretch.seekBestOverlapPositionMonoQuick(const refPos: PSingle): Integer;
var
  j: Integer;
  bestOffs: Integer;
  bestCorr, corr: double;
  scanCount, corrOffset, tempOffset: Integer;
  tmp: double;
begin
  // Slopes the amplitude of the 'midBuffer' samples
  precalcCorrReferenceMono;

  bestCorr := FLT_MIN;
  bestOffs := _scanOffsets[0, 0];
  corrOffset := 0;
  tempOffset := 0;

  // Scans for the best correlation value using four-pass hierarchical search.
  //
  // The look-up table 'scans' has hierarchical position adjusting steps.
  // In first pass the routine searhes for the highest correlation with
  // relatively coarse steps, then rescans the neighbourhood of the highest
  // correlation with better resolution and so on.
  for scanCount := 0 to 4 do
  begin
    j := 0;
    while _scanOffsets[scanCount, j] > 0 do
    begin
      tempOffset := corrOffset + _scanOffsets[scanCount, j];
      if tempOffset >= seekLength then break;

      // Calculates correlation value for the mixing position corresponding
      // to 'tempOffset'
      corr := calcCrossCorrMono(refPos + tempOffset, pRefMidBuffer);
      // heuristic rule to slightly favour values close to mid of the range
      tmp := (2 * tempOffset - seekLength) * inverseSeekLength;
      corr := ((corr + 0.1) * (1.0 - 0.25 * tmp * tmp));

      // Checks for the highest correlation value
      if corr > bestCorr then
      begin
        bestCorr := corr;
        bestOffs := tempOffset;
      end;
      j := j + 1;
    end;
    corrOffset := bestOffs;
  end;

  Result := bestOffs;
end;

/// Calculates processing sequence length according to tempo setting
procedure TTDStretch.calcSeqParameters;
var
  seq, seek: double;

  function CHECK_LIMITS(x, mi, ma: double): double;
  begin
    if x < mi then
      Result := mi
    else if x > ma then
      Result := ma
    else
      Result := x;
  end;

begin
  if bAutoSeqSetting then
  begin
    seq := AUTOSEQ_C + AUTOSEQ_K * tempo;
    seq := CHECK_LIMITS(seq, AUTOSEQ_AT_MAX, AUTOSEQ_AT_MIN);
    sequenceMs := Round(seq);
  end;

  if bAutoSeekSetting then
  begin
    seek := AUTOSEEK_C + AUTOSEEK_K * tempo;
    seek := CHECK_LIMITS(seek, AUTOSEEK_AT_MAX, AUTOSEEK_AT_MIN);
    seekWindowMs := Round(seek);
  end;

  // Update seek window lengths
  seekWindowLength := (sampleRate * sequenceMs) div 1000;
  if seekWindowLength < 2 * overlapLength then
  begin
    seekWindowLength := 2 * overlapLength;
  end;
  seekLength := (sampleRate * seekWindowMs) div 1000;

  inverseSeekLength := 1 / seekLength;
end;

// Sets new target tempo. Normal tempo = 'SCALE', smaller values represent slower 
// tempo, larger faster tempo.
procedure TTDStretch.setTempo(newTempo: Single);
var
  intskip: Integer;
begin
  tempo := newTempo;

  // Calculate new sequence duration
  calcSeqParameters;

  // Calculate ideal skip length (according to tempo value)
  nominalSkip := tempo * (seekWindowLength - overlapLength);
  intskip := Round(nominalSkip);

  // Calculate how many samples are needed in the 'inputBuffer' to
  // process another batch of samples
  //sampleReq = max(intskip + overlapLength, seekWindowLength) + seekLength / 2;
  if intskip + overlapLength > seekWindowLength then
  begin
    sampleReq := intskip + overlapLength + seekLength;
  end
  else
  begin
    sampleReq := seekWindowLength + seekLength;
  end;
end;

// Sets the number of channels, 1 = mono, 2 = stereo
procedure TTDStretch.setChannels(numChannels: Integer);
begin
  if channels = numChannels then exit;

  channels := numChannels;
  inputBuffer.setChannels(channels);
  outputBuffer.setChannels(channels);
end;

// Processes as many processing frames of the samples 'inputBuffer', store
// the result into 'outputBuffer'
procedure TTDStretch.processSamples;
var
  ovlSkip, offset: Integer;
  temp: Integer;
begin
  // Process samples as long as there are enough samples in 'inputBuffer'
  // to form a processing frame.
  while inputBuffer.numSamples >= sampleReq do
  begin
    // If tempo differs from the normal ('SCALE'), scan for the best overlapping
    // position
    offset := seekBestOverlapPosition(inputBuffer.ptrBegin);

    // Mix the samples in the 'inputBuffer' at position of 'offset' with the
    // samples in 'midBuffer' using sliding overlapping
    // ... first partially overlap with the end of the previous sequence
    // (that's in 'midBuffer')
    overlap(outputBuffer.ptrEnd(overlapLength), inputBuffer.ptrBegin, offset);
    outputBuffer.putSamples(overlapLength);

    // ... then copy sequence samples from 'inputBuffer' to output:

    // length of sequence
    temp := (seekWindowLength - 2 * overlapLength);

    // crosscheck that we don't have buffer overflow...
    if (inputBuffer.numSamples < (offset + temp + overlapLength * 2)) then
    begin
      continue;    // just in case, shouldn't really happen
    end;

    outputBuffer.putSamples(inputBuffer.ptrBegin + channels * (offset + overlapLength), temp);

    // Copies the end of the current sequence from 'inputBuffer' to
    // 'midBuffer' for being mixed with the beginning of the next
    // processing sequence and so on
    Move(
      (inputBuffer.ptrBegin + channels * (offset + temp + overlapLength))^,
      pMidBuffer^,
      channels * sizeof(Single) * overlapLength);

    // Remove the processed samples from the input buffer. Update
    // the difference between integer & nominal skip step to 'skipFract'
    // in order to prevent the error from accumulating over time.
    skipFract += nominalSkip;   // real skip size
    ovlSkip := Trunc(skipFract);   // rounded to integer skip
    skipFract -= ovlSkip;       // maintain the fraction part, i.e. real vs. integer skip
    inputBuffer.receiveSamples(ovlSkip);
  end;
end;


// Adds 'numsamples' pcs of samples from the 'samples' memory position into
// the input of the object.
procedure TTDStretch.putSamples(const samples: PSingle; anumSamples: longword);
begin
  // Add the samples into the input buffer
  inputBuffer.putSamples(samples, anumSamples);
  // Process the samples in input buffer
  processSamples;
end;

/// Set new overlap length parameter & reallocate RefMidBuffer if necessary.
procedure TTDStretch.acceptNewOverlapLength(newOverlapLength: Integer);
var
  prevOvl: Integer;
begin
  prevOvl := overlapLength;
  overlapLength := newOverlapLength;

  if overlapLength > prevOvl then
  begin
    FreeMem(pMidBuffer);
    FreeMem(pRefMidBuffer);

    pMidBuffer := GetMem(overlapLength * 2 * sizeof(Single));
    clearMidBuffer;

    pRefMidBuffer := GetMem(2 * sizeof(Single) * overlapLength);
  end;
end;

// Slopes the amplitude of the 'midBuffer' samples so that cross correlation
// is faster to calculate
procedure TTDStretch.precalcCorrReferenceStereo;
var
  i, cnt2: Integer;
  temp: single;
begin
  for i := 0 to Pred(overlapLength) do
  begin
    temp := i * (overlapLength - i);
    cnt2 := i * 2;
    pRefMidBuffer[cnt2] := (pMidBuffer[cnt2] * temp);
    pRefMidBuffer[cnt2 + 1] := (pMidBuffer[cnt2 + 1] * temp);
  end;
end;

// Slopes the amplitude of the 'midBuffer' samples so that cross correlation
// is faster to calculate
procedure TTDStretch.precalcCorrReferenceMono;
var
  i: Integer;
  temp: single;
begin

  for i := 0 to Pred(overlapLength) do
  begin
    temp := i * (overlapLength - i);
    pRefMidBuffer[i] := (pMidBuffer[i] * temp);
  end;
end;

// Overlaps samples in 'midBuffer' with the samples in 'pInput'
procedure TTDStretch.overlapStereo(pOutput: PSingle; const pInput: PSingle);
var
  i: Integer;
  cnt2: Integer;
  fTemp: Single;
  fScale: Single;
  fi: Single;
begin
  fScale := 1.0 / overlapLength;

  for i := 0 to Pred(overlapLength) do
  begin
    fTemp := (overlapLength - i) * fScale;
    fi := i * fScale;
    cnt2 := 2 * i;
    pOutput[cnt2 + 0] := pInput[cnt2 + 0] * fi + pMidBuffer[cnt2 + 0] * fTemp;
    pOutput[cnt2 + 1] := pInput[cnt2 + 1] * fi + pMidBuffer[cnt2 + 1] * fTemp;
  end;
end;

/// Calculates overlapInMsec period length in samples.
procedure TTDStretch.calculateOverlapLength(aoverlapInMsec: Integer);
var
  newOvl: Integer;
begin
  newOvl := (sampleRate * aoverlapInMsec) div 1000;
  if newOvl < 16 then newOvl := 16;

  // must be divisible by 8
  newOvl -= newOvl mod 8;

  acceptNewOverlapLength(newOvl);
end;

function TTDStretch.calcCrossCorrMono(const mixingPos: PSingle; const compare: PSingle): double;
var
  corr: double;
  norm: double;
  i: Integer;
begin
  corr := 0;
  norm := 0;
  for i := 1 to Pred(overlapLength) do
  begin
    corr += mixingPos[i] * compare[i];
    norm += mixingPos[i] * mixingPos[i];
  end;

  if norm < 1e-9 then norm := 1.0;    // to avoid div by zero
  Result := corr / sqrt(norm);
end;


function TTDStretch.calcCrossCorrStereo(const mixingPos: PSingle; const compare: PSingle): double;
var
  corr: double;
  norm: double;
  i, j: Integer;
begin
  corr := 0;
  norm := 0;
  for i := 2 to Pred(2 * overlapLength) do
  begin

    j := i * 2;
    corr += mixingPos[j] * compare[j] +
            mixingPos[j + 1] * compare[j + 1];
    norm += mixingPos[j] * mixingPos[j] +
            mixingPos[j + 1] * mixingPos[j + 1];
  end;

  if norm < 1e-9 then norm := 1.0;    // to avoid div by zero
  Result := corr / sqrt(norm);
end;

/// return nominal input sample requirement for triggering a processing batch
function TTDStretch.getInputSampleReq: Integer;
begin
  Result := Round(nominalSkip);
end;

/// return nominal output sample amount when running a processing batch
function TTDStretch.getOutputBatchSize: Integer;
begin
 	Result := seekWindowLength - overlapLength;
end;

procedure TTDStretch.enableTransientDetection(enable: Integer);
begin
  transient := (enable = 1);
end;

end.