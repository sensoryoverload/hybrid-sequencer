unit soundtouch;

{$mode objfpc}{$H+}

{$fputype sse2}

interface

uses
  sysutils;

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

const
/// Soundtouch library version string
  SOUNDTOUCH_VERSION =         '1.6.1pre';

/// SoundTouch library version id
  SOUNDTOUCH_VERSION_ID =      (10601);

//
// Available setting IDs for the 'setSetting' & 'get_setting' functions:

/// Enable/disable anti-alias filter in pitch transposer (0 = disable)
  SETTING_USE_AA_FILTER =      0;

/// Pitch transposer anti-alias filter length (8 .. 128 taps, default = 32)
  SETTING_AA_FILTER_LENGTH =   1;

/// Enable/disable quick seeking algorithm in tempo changer routine
/// (enabling quick seeking lowers CPU utilization but causes a minor sound
///  quality compromising)
  SETTING_USE_QUICKSEEK =      2;

/// Time-stretch algorithm single processing sequence length in milliseconds. This determines
/// to how long sequences the original sound is chopped in the time-stretch algorithm.
/// See "STTypes.h" or README for more information.
  SETTING_SEQUENCE_MS =        3;

/// Time-stretch algorithm seeking window length in milliseconds for algorithm that finds the
/// best possible overlapping location. This determines from how wide window the algorithm
/// may look for an optimal joining location when mixing the sound sequences back together.
/// See "STTypes.h" or README for more information.
  SETTING_SEEKWINDOW_MS =      4;

/// Time-stretch algorithm overlap length in milliseconds. When the chopped sound sequences
/// are mixed back together, to form a continuous sound stream, this parameter defines over
/// how long period the two consecutive sequences are let to overlap each other.
/// See "STTypes.h" or README for more information.
  SETTING_OVERLAP_MS =         5;


/// Call "getSetting" with this ID to query nominal average processing sequence
/// size in samples. This value tells approcimate value how many input samples
/// SoundTouch needs to gather before it does DSP processing run for the sample batch.
///
/// Notices:
/// - This is read-only parameter, i.e. setSetting ignores this parameter
/// - Returned value is approximate average value, exact processing batch
///   size may wary from time to time
/// - This parameter value is not constant but may change depending on
///   tempo/pitch/rate/samplerate settings.
  SETTING_NOMINAL_INPUT_SEQUENCE =		6;


/// Call "getSetting" with this ID to query nominal average processing output
/// size in samples. This value tells approcimate value how many output samples
/// SoundTouch outputs once it does DSP processing run for a batch of input samples.
///
/// Notices:
/// - This is read-only parameter, i.e. setSetting ignores this parameter
/// - Returned value is approximate average value, exact processing batch
///   size may wary from time to time
/// - This parameter value is not constant but may change depending on
///   tempo/pitch/rate/samplerate settings.
  SETTING_NOMINAL_OUTPUT_SEQUENCE	=	7;


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
  AUTOSEQ_TEMPO_LOW = 0.25;     // auto setting low tempo range (-50%)
  AUTOSEQ_TEMPO_TOP = 4.0;     // auto setting top tempo range (+100%)

  // sequence-ms setting values at above low & top tempo
  AUTOSEQ_AT_MIN =    125.0;
  AUTOSEQ_AT_MAX =    50.0;
  AUTOSEQ_K =         ((AUTOSEQ_AT_MAX - AUTOSEQ_AT_MIN) / (AUTOSEQ_TEMPO_TOP - AUTOSEQ_TEMPO_LOW));
  AUTOSEQ_C =         (AUTOSEQ_AT_MIN - (AUTOSEQ_K) * (AUTOSEQ_TEMPO_LOW));

  // seek-window-ms setting values at above low & top tempo
  AUTOSEEK_AT_MIN =   25.0;
  AUTOSEEK_AT_MAX =   15.0;
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
  TFIFOSampleBuffer = class
  private
    // Raw unaligned buffer memory. 'buffer' is made aligned by pointing it to first
    // 16-byte aligned location of this buffer
    bufferUnaligned: PSingle;

    /// Sample buffer size in bytes
    sizeInBytes: longword;

    /// How many samples are currently in buffer.
    samplesInBuffer: longword;

    /// Channels, 1=mono, 2=stereo.
    fchannels: longword;

    /// Rewind the buffer by moving data from position pointed by 'bufferPos' to real
    /// beginning of the buffer.
    procedure rewind;

    /// Returns current capacity.
    function getCapacity: longword;
  public
    /// Sample buffer.
    buffer: PSingle;

    /// Current position pointer to the buffer. This pointer is increased when samples are
    /// removed from the pipe so that it's necessary to actually rewind buffer (move data)
    /// only new data when is put to the pipe.
    bufferPos: longword;

    /// Constructor
    constructor Create(numChannels: longword     ///< Number of channels, 1=mono, 2=stereo.
                                              ///< Default is stereo.
                      );

    /// destructor
    destructor Destroy; override;

    /// Ensures that the buffer has capacity for at least this many samples.
    procedure ensureCapacity(capacityRequirement: longword);

    /// Returns a pointer to the beginning of the output samples.
    /// This function is provided for accessing the output samples directly.
    /// Please be careful for not to corrupt the book-keeping!
    ///
    /// When using this function to output samples, also remember to 'remove' the
    /// output samples from the buffer by calling the
    /// 'receiveSamples(numSamples)' function
    function ptrBegin: PSingle;

    /// Returns a pointer to the end of the used part of the sample buffer (i.e.
    /// where the new samples are to be inserted). This function may be used for
    /// inserting new samples into the sample buffer directly. Please be careful
    /// not corrupt the book-keeping!
    ///
    /// When using this function as means for inserting new samples, also remember
    /// to increase the sample count afterwards, by calling  the
    /// 'putSamples(numSamples)' function.
    function ptrEnd(
                slackCapacity: longword  ///< How much free capacity (in samples) there _at least_
                                     ///< should be so that the caller can succesfully insert the
                                     ///< desired samples to the buffer. If necessary, the function
                                     ///< grows the buffer size to comply with this requirement.
                ): PSingle;

    /// Adds 'numSamples' pcs of samples from the 'samples' memory position to
    /// the sample buffer.
    procedure putSamples(const samples: PSingle;  ///< Pointer to samples.
                         anumSamples: longword                        ///< Number of samples to insert.
                        );

    /// Adjusts the book-keeping to increase number of samples in the buffer without
    /// copying any actual samples.
    ///
    /// This function is used to update the number of samples in the sample buffer
    /// when accessing the buffer directly with 'ptrEnd' function. Please be
    /// careful though!
    procedure putSamples(anumSamples: longword   ///< Number of samples been inserted.
                         );

    /// Output samples from beginning of the sample buffer. Copies requested samples to
    /// output buffer and removes them from the sample buffer. If there are less than
    /// 'numsample' samples in the buffer, returns all that available.
    ///
    /// \return Number of samples returned.
    function receiveSamples(output: PSingle; ///< Buffer where to copy output samples.
                                maxSamples: longword                  ///< How many samples to receive at max.
                                ): longword;

    /// Adjusts book-keeping so that given number of samples are removed from beginning of the
    /// sample buffer without copying them anywhere.
    ///
    /// Used to reduce the number of samples in the buffer when accessing the sample buffer directly
    /// with 'ptrBegin' function.
    function receiveSamples(maxSamples: longword   ///< Remove this many samples from the beginning of pipe.
                            ): longword;

    /// Returns number of samples currently available.
    function numSamples: longword;

    /// Sets number of channels, 1 = mono, 2 = stereo.
    procedure setChannels(numChannels: longword);

    /// Returns nonzero if there aren't any samples available for outputting.
    function isEmpty: integer;

    /// Clears all the samples.
    procedure clear;
  end;

  /// Class that does the time-stretch (tempo change) effect for the processed
  /// sound.

  { TSoundTouch }

  TSoundTouch = class
  private
    channels: Integer;
    sampleReq: Integer;
    tempo: Single;
    internaltempo: single;
    rate: Single;
    internalrate: Single;

    fSlopecount: Single;
    sPrevSampleL: Single;
    sPrevSampleR: Single;

    /// Virtual pitch parameter. Effective rate & tempo are calculated from these parameters.
    virtualRate: Single;

    /// Virtual pitch parameter. Effective rate & tempo are calculated from these parameters.
    virtualTempo: Single;

    /// Virtual pitch parameter. Effective rate & tempo are calculated from these parameters.
    virtualPitch: Single;

    /// Flag: Has sample rate been set?
    bSrateSet: Boolean;

    pMidBuffer: PSingle;
    pRefMidBuffer: PSingle;
    overlapLength: Integer;
    seekLength: Integer;
    InverseSeekLength: Single;
    seekWindowLength: Integer;
    overlapDividerBits: Integer;
    slopingDivider: Integer;
    nominalSkip: Single;
    skipFract: Single;
    outputBuffer: TFIFOSampleBuffer;
    tempBuffer: TFIFOSampleBuffer;
    inputBuffer: TFIFOSampleBuffer;
    bQuickSeek: Boolean;
    sampleRate: Integer;
    sequenceMs: Integer;
    seekWindowMs: Integer;
    overlapMs: Integer;
    bAutoSeqSetting: Boolean;
    bAutoSeekSetting: Boolean;

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

    /// Sets new target tempo. Normal tempo = 'SCALE', smaller values represent slower
    /// tempo, larger faster tempo.
    procedure setInternalTempo(newTempo: Single);

    // Sets new target iRate. Normal iRate = 1.0, smaller values represent slower
    // iRate, larger faster iRates.
    procedure setInternalRate(newRate: Single);

    procedure acceptNewOverlapLength(newOverlapLength: Integer);
    procedure calcEffectiveRateAndTempo;

    procedure calculateOverlapLength(aoverlapInMsec: Integer);

    function calcCrossCorrStereo(const mixingPos: PSingle; const compare: PSingle): double;
    function calcCrossCorrMono(const mixingPos: PSingle; const compare: PSingle): double;
    procedure resetRegisters;

    function seekBestOverlapPositionStereo(const refPos: PSingle): Integer;
    function seekBestOverlapPositionStereoQuick(const refPos: PSingle): Integer;
    function seekBestOverlapPositionMono(const refPos: PSingle): Integer;
    function seekBestOverlapPositionMonoQuick(const refPos: PSingle): Integer;
    function seekBestOverlapPosition(const refPos: PSingle): Integer;

    procedure overlapStereo(poutput: PSingle; const pinput: PSingle);
    procedure overlapMono(poutput: PSingle; const pinput: PSingle);

    procedure clearMidBuffer;
    procedure overlap(poutput: PSingle; const pinput: PSingle; ovlPos: longword);

    procedure precalcCorrReferenceMono;
    procedure precalcCorrReferenceStereo;

    procedure calcSeqParameters;

    function processTranspose(dest: PSingle; const src: PSingle; anumSamples: longword
      ): longword;
    function transposeMono(dest: PSingle; const src: PSingle;
      anumSamples: longword): longword;
    function transposeStereo(dest: PSingle; const src: PSingle;
      anumSamples: longword): longword;
  public
    procedure processStretch;
    constructor Create;
    destructor Destroy; override;

    /// Returns the output buffer object
    function getOutput: TFIFOSampleBuffer;

    /// Returns the input buffer object
    function getInput: TFIFOSampleBuffer;

    /// Returns nonzero if there aren't any samples available for outputting.
    procedure clear;

    /// Clears the input buffer
    procedure clearInput;

    /// Sets the number of channels, 1 = mono, 2 = stereo
    procedure setChannels(numChannels: Integer);

    /// Enables/disables the quick position seeking algorithm. Zero to disable,
    /// nonzero to enable
    procedure enableQuickSeek(enable: Boolean);

    /// Returns nonzero if the quick seeking algorithm is enabled.
    function isQuickSeekEnabled: Boolean;

    function setSetting(settingId: Integer; value: Integer): Boolean;
    function getSetting(settingId: Integer): Integer;


    /// Adds 'numsamples' pcs of samples from the 'samples' memory position into
    /// the input of the object.
    procedure putSamples(
            samples: PSingle;  ///< Input sample data
            anumSamples: longword                         ///< Number of samples in 'samples' so that one sample
                                                    ///< contains both channels if stereo
            );

    function receiveSamples(samples: PSingle; anumSamples: longword ): longword;
    procedure flush;
    procedure setSampleRate(srate: longword);
    function getInputSampleReq: Integer;
    function getOutputBatchSize: Integer;
    procedure setPitch(newPitch: Single);
    procedure setPitchOctaves(newPitch: Single);
    procedure setPitchSemiTones(newPitch: Single);
    procedure setRate(newRate: Single);
    procedure setTempo(newTempo: Single);
  end;

implementation

/// test if two floating point numbers are equal
function TEST_FLOAT_EQUAL(a, b: Single): Boolean;
begin
  Result := (abs(a - b) < 1e-10);
end;

{*****************************************************************************
 *
 * Implementation of the class 'TSoundTouch'
 *
 *****************************************************************************}
constructor TSoundTouch.Create;
begin
  outputBuffer := TFIFOSampleBuffer.Create(1);
  inputBuffer := TFIFOSampleBuffer.Create(1);
  tempBuffer := TFIFOSampleBuffer.Create(1);

  rate := 0;
  tempo := 0;

  virtualPitch := 1.0;
  virtualRate := 1.0;
  virtualTempo := 1.0;

  calcEffectiveRateAndTempo;

  bQuickSeek := FALSE;
  channels := 1;

  pMidBuffer := nil;
  overlapLength := 0;

  bAutoSeqSetting := TRUE;
  bAutoSeekSetting := TRUE;

  skipFract := 0;

  setParameters(44100, DEFAULT_SEQUENCE_MS, DEFAULT_SEEKWINDOW_MS, DEFAULT_OVERLAP_MS);

  setInternalTempo(1.0);
  setInternalRate(1.0);

  ResetRegisters;

  clear;

  bSrateSet := FALSE;
end;

destructor TSoundTouch.Destroy;
begin
  FreeMem(pMidBuffer);

  inputBuffer.Free;
  outputBuffer.Free;
  tempBuffer.Free;

  inherited Destroy;
end;

function TSoundTouch.getOutput: TFIFOSampleBuffer;
begin
  Result := outputBuffer;
end;

function TSoundTouch.getInput: TFIFOSampleBuffer;
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

procedure TSoundTouch.setParameters(aSampleRate, aSequenceMS,
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
  setInternalTempo(tempo);
end;

/// Get routine control parameters, see setParameters() function.
/// Any of the parameters to this function can be NULL, in such case corresponding parameter
/// value isn't returned.
procedure TSoundTouch.getParameters(apSampleRate, apSequenceMs, apSeekWindowMs, apOverlapMs: PInteger);
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
procedure TSoundTouch.overlapMono(pOutput: PSingle; const pInput: PSingle);
var
  i, itemp: Integer;
  DivByOverlapLength: Single;
begin
  DivByOverlapLength := 1 / overlapLength;

  for i := 0 to Pred(overlapLength) do
  begin
    itemp := overlapLength - i;
    pOutput[i] := (pInput[i] * i + pMidBuffer[i] * itemp ) * DivByOverlapLength;
  end;
end;

procedure TSoundTouch.clearMidBuffer;
begin
  FillByte(pMidBuffer^, (2 * sizeof(Single) * overlapLength), 0);
end;

procedure TSoundTouch.clearInput;
begin
  inputBuffer.clear;
  clearMidBuffer;
end;

// Clears the sample buffers
procedure TSoundTouch.clear;
begin
  outputBuffer.clear;
  clearInput;
end;

// Enables/disables the quick position seeking algorithm. Zero to disable, nonzero
// to enable
procedure TSoundTouch.enableQuickSeek(enable: Boolean);
begin
  bQuickSeek := enable;
end;

// Returns nonzero if the quick seeking algorithm is enabled.
function TSoundTouch.isQuickSeekEnabled: Boolean;
begin
  Result := bQuickSeek;
end;

// Seeks for the optimal overlap-mixing position.
function TSoundTouch.seekBestOverlapPosition(const refPos: PSingle): Integer;
begin
  if channels = 2 then
  begin
    // stereo sound
    if bQuickSeek then
    begin
      Result := seekBestOverlapPositionStereoQuick(refPos);
    end
    else
    begin
      Result := seekBestOverlapPositionStereo(refPos);
    end;
  end
  else
  begin
    // mono sound
    if bQuickSeek then
    begin
      Result := seekBestOverlapPositionMonoQuick(refPos);
    end
    else
    begin
      Result := seekBestOverlapPositionMono(refPos);
    end;
  end;
end;

// Overlaps samples in 'midBuffer' with the samples in 'pInputBuffer' at position
// of 'ovlPos'.
procedure TSoundTouch.overlap(pOutput: PSingle; const pInput: PSingle; ovlPos: longword);
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
function TSoundTouch.seekBestOverlapPositionStereo(const refPos: PSingle): Integer;
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
    tmp := (2 * i - seekLength) * InverseSeekLength;// / seekLength;
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

// Seeks for the optimal overlap-mixing position. The 'stereo' version of the
// routine
//
// The best position is determined as the position where the two overlapped
// sample sequences are 'most alike', in terms of the highest cross-correlation
// value over the overlapping period
function TSoundTouch.seekBestOverlapPositionStereoQuick(const refPos: PSingle): Integer;
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
      tmp := (2 * tempOffset - seekLength) * InverseSeekLength; // / seekLength;
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
function TSoundTouch.seekBestOverlapPositionMono(const refPos: PSingle): Integer;
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
    tmp := (2 * tempOffset - seekLength) * InverseSeekLength;// / seekLength;
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
function TSoundTouch.seekBestOverlapPositionMonoQuick(const refPos: PSingle): Integer;
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
      tmp := (2 * tempOffset - seekLength) * InverseSeekLength; // / seekLength;
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
procedure TSoundTouch.calcSeqParameters;
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
  InverseSeekLength := 1 / seekLength;
end;

// Sets new target tempo. Normal tempo = 'SCALE', smaller values represent slower 
// tempo, larger faster tempo.
procedure TSoundTouch.setInternalTempo(newTempo: Single);
var
  intskip: Integer;
begin
  internaltempo := newTempo;

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
procedure TSoundTouch.setChannels(numChannels: Integer);
begin
  if channels = numChannels then exit;

  channels := numChannels;
  inputBuffer.setChannels(channels);
  outputBuffer.setChannels(channels);
  tempBuffer.setChannels(channels);
end;

// Processes as many processing frames of the samples 'inputBuffer', store
// the result into 'outputBuffer'
procedure TSoundTouch.processStretch;
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
procedure TSoundTouch.putSamples(samples: PSingle; anumSamples: longword);
var
  lInputBuffer: PSingle;
  lOutputBuffer: PSingle;
  sizeTemp: longword;
  lCount: longword;
begin
  // Add the samples into the input buffer
  inputBuffer.putSamples(samples, anumSamples);
  // Process the samples in input buffer
  processStretch;

  // Intermediate buffer size before scaling
  sizeTemp := Round(((outputBuffer.numSamples * channels * SizeOf(Single)) / rate) + 16.0);

  lInputBuffer := GetMem(outputBuffer.numSamples * channels * SizeOf(Single));
  lOutputBuffer := GetMem(sizeTemp);

  lCount := outputBuffer.receiveSamples(lInputBuffer, outputBuffer.numSamples);
  lCount := processTranspose(lOutputBuffer, lInputBuffer, lCount);
  outputBuffer.putSamples(lOutputBuffer, lCount);

  FreeMem(lInputBuffer);
  FreeMem(lOutputBuffer);
end;

// Output frames from beginning of the sample buffer.
// Copies requested frames output buffer and removes them
// from the sample buffer. If there are less than frames()
// samples in the buffer, returns all that available.
function TSoundTouch.receiveSamples(samples: PSingle; anumSamples: longword): longword;
begin
	result := outputBuffer.receiveSamples(samples, anumSamples);
end;

// Flushes the last samples from the processing pipeline to the output.
// Clears also the internal processing buffers.
//
// Note: This function is meant for extracting the last samples of a sound
// stream. This function may introduce additional blank samples in the end
// of the sound stream, and thus it's not recommended to call this function
// in the middle of a sound stream.
procedure TSoundTouch.flush;
var
  i: Integer;
  nOut: longword;
  buff: Array[0..127] of Single;
begin
  //nOut := numSamples;

  FillByte(buff, 128 * sizeof(Single), 0);
  // "Push" the last active samples out from the processing pipeline by
  // feeding blank samples into the processing pipeline until new,
  // processed samples appear in the output (not however, more than
  // 8ksamples in any case)
  for i := Low(buff) to High(buff) do
  begin
    putSamples(buff, 64);
  //  if numSamples <> nOut then
    begin
      break;  // new samples have appeared in the output!
    end;
  end;

  // Clear working buffers
  clear;
  // yet leave the 'tempoChanger' output intouched as that's where the
  // flushed samples are!
end;

// Sets sample rate.
procedure TSoundTouch.setSampleRate(srate: longword);
begin
  bSrateSet := TRUE;
  // set sample rate, leave other tempo changer parameters as they are.
  setParameters(srate);
end;

// Transposes the sample rate of the given samples using linear interpolation.
// Returns the number of samples returned in the "dest" buffer
function TSoundTouch.processTranspose(dest: PSingle; const src: PSingle; anumSamples: longword): longword; inline;
begin
  if Channels = 2 then
  begin
    Result := transposeStereo(dest, src, anumSamples);
  end
  else
  begin
    Result := transposeMono(dest, src, anumSamples);
  end;
end;

procedure TSoundTouch.resetRegisters;
begin
  fSlopeCount := 0;
  sPrevSampleL := 0;
  sPrevSampleR := 0;
end;

// Transposes the sample rate of the given samples using linear interpolation.
// 'Mono' version of the routine. Returns the number of samples returned in
// the "dest" buffer
function TSoundTouch.transposeMono(dest: PSingle; const src: PSingle; anumSamples: longword): longword;
label
  lend;
var
  i, used: longword;
begin
  used := 0;
  i := 0;

  // Process the last sample saved from the previous call first...
  while fSlopeCount <= 1.0 do
  begin
    dest[i] := ((1.0 - fSlopeCount) * sPrevSampleL + fSlopeCount * src[0]);
    i+=1;
    fSlopeCount += internalrate;
  end;
  fSlopeCount -= 1.0;

  if anumSamples > 1 then
  begin
    while True do
    begin
      while fSlopeCount > 1.0 do
      begin
        fSlopeCount -= 1.0;
        used+=1;
        if used >= anumSamples - 1 then
          goto lend;
      end;
      dest[i] := ((1.0 - fSlopeCount) * src[used] + fSlopeCount * src[used + 1]);
      i+=1;
      fSlopeCount += internalrate;
    end;
  end;
lend:
  // Store the last sample for the next round
  sPrevSampleL := src[anumSamples - 1];

  Result := i;
end;

// Transposes the sample rate of the given samples using linear interpolation.
// 'Mono' version of the routine. Returns the number of samples returned in
// the "dest" buffer
function TSoundTouch.transposeStereo(dest: PSingle; const src: PSingle; anumSamples: longword): longword;
label
  lend;
var
  srcPos, i, used: longword;
begin

  if anumSamples = 0 then
  begin
    Result := 0;  // no samples, no work
    exit;
  end;

  used := 0;
  i := 0;

  // Process the last sample saved from the sPrevSampleLious call first...
  while fSlopeCount <= 1.0 do
  begin
    dest[2 * i] := ((1.0 - fSlopeCount) * sPrevSampleL + fSlopeCount * src[0]);
    dest[2 * i + 1] := ((1.0 - fSlopeCount) * sPrevSampleR + fSlopeCount * src[1]);
    i+=1;
    fSlopeCount += internalrate;
  end;
  // now always (iSlopeCount > 1.0f)
  fSlopeCount -= 1.0;

  if anumSamples > 1 then
  begin
    while True do
    begin
      while fSlopeCount > 1.0 do
      begin
        fSlopeCount -= 1.0;
        used+=1;
        if used >= anumSamples - 1 then goto lend;
      end;
      srcPos := 2 * used;

      dest[2 * i] := ((1.0 - fSlopeCount) * src[srcPos]
          + fSlopeCount * src[srcPos + 2]);
      dest[2 * i + 1] := ((1.0 - fSlopeCount) * src[srcPos + 1]
          + fSlopeCount * src[srcPos + 3]);

      i+=1;
      fSlopeCount += internalrate;
    end;
  end;
lend:
  // Store the last sample for the next round
  sPrevSampleL := src[2 * anumSamples - 2];
  sPrevSampleR := src[2 * anumSamples - 1];

  Result := i;
end;


/// Set new overlap length parameter & reallocate RefMidBuffer if necessary.
procedure TSoundTouch.acceptNewOverlapLength(newOverlapLength: Integer);
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
procedure TSoundTouch.precalcCorrReferenceStereo;
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
procedure TSoundTouch.precalcCorrReferenceMono;
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
procedure TSoundTouch.overlapStereo(pOutput: PSingle; const pInput: PSingle);
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
procedure TSoundTouch.calculateOverlapLength(aoverlapInMsec: Integer);
var
  newOvl: Integer;
begin
  newOvl := (sampleRate * aoverlapInMsec) div 1000;
  if newOvl < 16 then newOvl := 16;

  // must be divisible by 8
  newOvl -= newOvl mod 8;

  acceptNewOverlapLength(newOvl);
end;

function TSoundTouch.calcCrossCorrMono(const mixingPos: PSingle; const compare: PSingle): double;
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


function TSoundTouch.calcCrossCorrStereo(const mixingPos: PSingle; const compare: PSingle): double;
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
function TSoundTouch.getInputSampleReq: Integer;
begin
  Result := Round(nominalSkip);
end;

/// return nominal output sample amount when running a processing batch
function TSoundTouch.getOutputBatchSize: Integer;
begin
 	Result := seekWindowLength - overlapLength;
end;

// Sets new rate control value. Normal rate = 1.0, smaller values
// represent slower rate, larger faster rates.
procedure TSoundTouch.setRate(newRate: Single);
begin
  virtualRate := newRate;
  calcEffectiveRateAndTempo;
end;

// Sets new tempo control value. Normal tempo = 1.0, smaller values
// represent slower tempo, larger faster tempo.
procedure TSoundTouch.setTempo(newTempo: Single);
begin
  virtualTempo := newTempo;
  calcEffectiveRateAndTempo;
end;

// Sets new pitch control value. Original pitch = 1.0, smaller values
// represent lower pitches, larger values higher pitch.
procedure TSoundTouch.setPitch(newPitch: Single);
begin
  virtualPitch := newPitch;
  calcEffectiveRateAndTempo;
end;

// (-1.00 .. +1.00)
procedure TSoundTouch.setPitchOctaves(newPitch: Single);
begin
  virtualPitch := exp(0.69314718056 * newPitch);
  calcEffectiveRateAndTempo;
end;

procedure TSoundTouch.setPitchSemiTones(newPitch: Single);
begin
  setPitchOctaves(newPitch / 12.0);
end;

// Sets new target iRate. Normal iRate = 1.0, smaller values represent slower
// iRate, larger faster iRates.
procedure TSoundTouch.setInternalRate(newRate: Single);
begin
  internalrate := newRate;
end;

// Calculates 'effective' rate and tempo values from the
// nominal control values.
procedure TSoundTouch.calcEffectiveRateAndTempo;
var
  oldTempo: Single;
  oldRate: Single;
begin
  oldTempo := tempo;
  oldRate := rate;

  tempo := virtualTempo / virtualPitch;
  rate := virtualPitch * virtualRate;

  if not TEST_FLOAT_EQUAL(rate,oldRate) then
  begin
    setInternalRate(rate);
  end;
  if not TEST_FLOAT_EQUAL(tempo, oldTempo) then
  begin
    setInternalTempo(tempo);
  end;
end;

// Changes a setting controlling the processing system behaviour. See the
// 'SETTING_...' defines for available setting ID's.
function TSoundTouch.setSetting(settingId: Integer; value: Integer): Boolean;
var
  lsampleRate, lsequenceMs, lseekWindowMs, loverlapMs: Integer;
begin
  // read current TSoundTouch routine parameters
  getParameters(@lsampleRate, @lsequenceMs, @lseekWindowMs, @loverlapMs);

  case settingId of
    SETTING_USE_AA_FILTER :
    begin
            // enables / disabless anti-alias filter
            //pRateTransposer.enableAAFilter(value <> 0);
            Result := TRUE;
    end;
    SETTING_AA_FILTER_LENGTH :
    begin
            // sets anti-alias filter length
            //pRateTransposer.getAAFilter.setLength(value);
            Result := TRUE;
    end;
    SETTING_USE_QUICKSEEK :
    begin
            // enables / disables tempo routine quick seeking algorithm
            enableQuickSeek(value <> 0);
            Result := TRUE;
    end;
    SETTING_SEQUENCE_MS:
    begin
            // change time-stretch sequence duration parameter
            setParameters(lsampleRate, value, lseekWindowMs, loverlapMs);
            Result := TRUE;
    end;
    SETTING_SEEKWINDOW_MS:
    begin
            // change time-stretch seek window length parameter
            setParameters(lsampleRate, lsequenceMs, value, loverlapMs);
            Result := TRUE;
    end;
    SETTING_OVERLAP_MS:
    begin
            // change time-stretch overlap length parameter
            setParameters(lsampleRate, lsequenceMs, lseekWindowMs, value);
            Result := TRUE;
    end
    else
    begin
      Result := FALSE;
    end;
  end;
end;

// Reads a setting controlling the processing system behaviour. See the
// 'SETTING_...' defines for available setting ID's.
//
// Returns the setting value.
function TSoundTouch.getSetting(settingId: Integer): Integer;
var
  temp: Integer;
begin
  case settingId of
    SETTING_USE_AA_FILTER :
    begin
      //Result := Integer(pRateTransposer.isAAFilterEnabled);
      Result := 0;
    end;

    SETTING_AA_FILTER_LENGTH :
    begin
      //Result := pRateTransposer.getAAFilter.getLength;
      Result := 0;
    end;

    SETTING_USE_QUICKSEEK :
    begin
      Result := Integer(isQuickSeekEnabled);
    end;

    SETTING_SEQUENCE_MS:
    begin
      getParameters(nil, @temp, nil, nil);
      Result := temp;
    end;

    SETTING_SEEKWINDOW_MS:
    begin
      getParameters(nil, nil, @temp, nil);
      Result := temp;
    end;

    SETTING_OVERLAP_MS:
    begin
      getParameters(nil, nil, nil, @temp);
      Result := temp;
    end;

		SETTING_NOMINAL_INPUT_SEQUENCE :
    begin
			Result := getInputSampleReq;
    end;

		SETTING_NOMINAL_OUTPUT_SEQUENCE :
    begin
			Result := getOutputBatchSize;
    end
    else
    begin
      Result := 0
    end;
  end;
end;

{ TFIFOSampleBuffer }

procedure Tfifosamplebuffer.Rewind;
begin
  if bufferPos <> 0 then
  begin
    Move(ptrBegin^, buffer^, sizeof(Single) * fchannels * samplesInBuffer);
    bufferPos := 0;
  end;
end;

procedure Tfifosamplebuffer.Ensurecapacity(Capacityrequirement: longword);
const
  Boundary = 4096;
  Alignment = 16;
var
  tempUnaligned,
  temp: PSingle;
begin
  if capacityRequirement > getCapacity then
  begin
    // enlarge the buffer in 4kbyte steps (round up to next 4k boundary)
    sizeInBytes := (capacityRequirement * fchannels * sizeof(Single) + Boundary - 1) and not (Boundary - 1);
    assert(sizeInBytes mod 2 = 0);
    tempUnaligned := getmem(sizeInBytes + Alignment);
    if not Assigned(tempUnaligned) then
    begin
       writeln('Could not allocate memory!\n');
    end;
    // Align the buffer to begin at 16byte cache line boundary for optimal performance
    temp := PSingle((longword(tempUnaligned) + Alignment - 1) and not (Alignment - 1));
    if samplesInBuffer > 0 then
    begin
      Move(ptrBegin^, temp^, samplesInBuffer * fchannels * sizeof(Single));
    end;
    Freemem(bufferUnaligned);
    buffer := temp;
    bufferUnaligned := tempUnaligned;
    bufferPos := 0;
  end
  else
  begin
    // simply rewind the buffer (if necessary)
    rewind;
  end;
end;

function Tfifosamplebuffer.Getcapacity: longword;
begin
  Result := sizeInBytes div (fchannels * sizeof(Single));
end;

constructor Tfifosamplebuffer.Create(Numchannels: longword);
begin
  assert(numChannels > 0);
  sizeInBytes := 0; // reasonable initial value
  buffer := nil;
  bufferUnaligned := nil;
  samplesInBuffer := 0;
  bufferPos := 0;
  fchannels := numChannels;
  ensureCapacity(32);

  inherited Create;
end;

destructor Tfifosamplebuffer.Destroy;
begin
  if Assigned(bufferUnaligned) then
    Freemem(bufferUnaligned);

  bufferUnaligned := nil;
  buffer := nil;

  inherited;
end;

function Tfifosamplebuffer.Ptrbegin: PSingle;
begin
  Result := buffer + bufferPos * fchannels;
end;

function Tfifosamplebuffer.Ptrend(
  Slackcapacity: longword): PSingle;
begin
  ensureCapacity(samplesInBuffer + slackCapacity);
  Result := buffer + samplesInBuffer * fchannels;
end;

procedure Tfifosamplebuffer.Putsamples(aNumsamples: longword);
var
  req: longword;
begin
  req := samplesInBuffer + aNumsamples;
  ensureCapacity(req);
  samplesInBuffer += aNumsamples;
end;

procedure Tfifosamplebuffer.Putsamples(
  const Samples: PSingle; aNumsamples: longword);
begin
  Move(samples^, ptrEnd(anumSamples)^, sizeof(Single) * anumSamples * fchannels);
  samplesInBuffer += anumSamples;
end;

function Tfifosamplebuffer.Receivesamples(
  Output: PSingle; Maxsamples: longword): longword;
var
  num: longword;
begin
  if maxSamples > samplesInBuffer then
    num := samplesInBuffer
  else
    num := maxSamples;
  Move(ptrBegin^, output^, fchannels * sizeof(Single) * num);
  Result := receiveSamples(num);
end;

function Tfifosamplebuffer.Receivesamples(Maxsamples: longword): longword;
var
  temp: longword;
begin
  if maxSamples >= samplesInBuffer then
  begin
    temp := samplesInBuffer;
    samplesInBuffer := 0;
    Result := temp;
    exit;
  end;

  samplesInBuffer -= maxSamples;
  bufferPos += maxSamples;

  Result := maxSamples;
end;

function Tfifosamplebuffer.Numsamples: longword;
begin
  Result := samplesInBuffer;
end;

procedure Tfifosamplebuffer.Setchannels(Numchannels: longword);
var
  usedBytes: longword;
begin
  usedBytes := fchannels * samplesInBuffer;
  fchannels := numChannels;
  samplesInBuffer := usedBytes div fchannels;
end;

function Tfifosamplebuffer.Isempty: Integer;
begin
  Result := Integer(samplesInBuffer = 0);
end;

procedure Tfifosamplebuffer.Clear;
begin
  samplesInBuffer := 0;
  bufferPos := 0;
end;

end.