unit soundtouch;

{$mode objfpc}

//////////////////////////////////////////////////////////////////////////////
///
/// SoundTouch - main class for tempo/pitch/rate adjusting routines. 
///
/// Notes:
/// - Initialize the SoundTouch object instance by setting up the sound stream 
///   parameters with functions 'setSampleRate' and 'setChannels', then set 
///   desired tempo/pitch/rate settings with the corresponding functions.
///
/// - The SoundTouch class behaves like a first-in-first-out pipeline: The 
///   samples that are to be processed are fed into one of the pipe by calling
///   function 'putSamples', while the ready processed samples can be read 
///   from the other end of the pipeline with function 'receiveSamples'.
/// 
/// - The SoundTouch processing classes require certain sized 'batches' of 
///   samples in order to process the sound. For this reason the classes buffer 
///   incoming samples until there are enough of samples available for 
///   processing, then they carry out the processing step and consequently
///   make the processed samples available for outputting.
/// 
/// - For the above reason, the processing routines introduce a certain 
///   'latency' between the input and output, so that the samples input to
///   SoundTouch may not be immediately available in the output, and neither 
///   the amount of outputtable samples may not immediately be in direct 
///   relationship with the amount of previously input samples.
///
/// - The tempo/pitch/rate control parameters can be altered during processing.
///   Please notice though that they aren't currently protected by semaphores,
///   so in multi-thread application external semaphore protection may be
///   required.
///
/// - This class utilizes classes 'TDStretch' for tempo change (without modifying
///   pitch) and 'RateTransposer' for changing the playback rate (that is, both 
///   tempo and pitch in the same ratio) of the sound. The third available control 
///   'pitch' (change pitch but maintain tempo) is produced by a combination of
///   combining the two other controls.
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

interface

uses
  tdstretch, ratetransposer, fifosamplepipe, fifosamplebuffer;

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

type

  { TSoundTouch }

  TSoundTouch = class(TFIFOProcessor)
  private
    /// Rate transposer class instance
    pRateTransposer: TRateTransposer;

    /// Time-stretch class instance
    pTDStretch: TTDStretch;

    /// Virtual pitch parameter. Effective rate & tempo are calculated from these parameters.
    virtualRate: Single;

    /// Virtual pitch parameter. Effective rate & tempo are calculated from these parameters.
    virtualTempo: Single;

    /// Virtual pitch parameter. Effective rate & tempo are calculated from these parameters.
    virtualPitch: Single;

    /// Flag: Has sample rate been set?
    bSrateSet: Boolean;

    /// Calculates effective rate & tempo valuescfrom 'virtualRate', 'virtualTempo' and
    /// 'virtualPitch' parameters.
    procedure calcEffectiveRateAndTempo;

  protected
    /// Number of channels
    channels: longword;

    /// Effective 'rate' value calculated from 'virtualRate', 'virtualTempo' and 'virtualPitch'
    rate: Single;

    /// Effective 'tempo' value calculated from 'virtualRate', 'virtualTempo' and 'virtualPitch'
    tempo: Single;

  public
    constructor Create; override;
    destructor Destroy; override;

    /// Get SoundTouch library version string
    function getVersionString: string;

    /// Get SoundTouch library version Id
    function getVersionId: longword;

    /// Sets new rate control value. Normal rate = 1.0, smaller values
    /// represent slower rate, larger faster rates.
    procedure setRate(newRate: Single);

    /// Sets new tempo control value. Normal tempo = 1.0, smaller values
    /// represent slower tempo, larger faster tempo.
    procedure setTempo(newTempo: Single);

    /// Sets new rate control value as a difference in percents compared
    /// to the original rate (-50 .. +100 %)
    procedure setRateChange(newRate: Single);

    /// Sets new tempo control value as a difference in percents compared
    /// to the original tempo (-50 .. +100 %)
    procedure setTempoChange(newTempo: Single);

    /// Sets new pitch control value. Original pitch = 1.0, smaller values
    /// represent lower pitches, larger values higher pitch.
    procedure setPitch(newPitch: Single);

    /// Sets pitch change in octaves compared to the original pitch
    /// (-1.00 .. +1.00)
    procedure setPitchOctaves(newPitch: Single);

    /// Sets pitch change in semi-tones compared to the original pitch
    /// (-12 .. +12)
    procedure setPitchSemiTones(newPitch: Integer); overload;
    procedure setPitchSemiTones(newPitch: Single); overload;

    /// Sets the number of channels, 1 = mono, 2 = stereo
    procedure setChannels(numChannels: longword);

    /// Sets sample rate.
    procedure setSampleRate(srate: longword);

    /// Flushes the last samples from the processing pipeline to the output.
    /// Clears also the internal processing buffers.
    //
    /// Note: This function is meant for extracting the last samples of a sound
    /// stream. This function may introduce additional blank samples in the end
    /// of the sound stream, and thus it's not recommended to call this function
    /// in the middle of a sound stream.
    procedure flush;

    /// Adds 'numSamples' pcs of samples from the 'samples' memory position into
    /// the input of the object. Notice that sample rate _has_to_ be set before
    /// calling this function, otherwise throws a runtime_error exception.
    procedure putSamples(
            const samples: PSingle;  ///< Pointer to sample buffer.
            anumSamples: longword                         ///< Number of samples in buffer. Notice
                                                    ///< that in case of stereo-sound a single sample
                                                    ///< contains data for both channels.
            ); override;

    /// Clears all the samples in the object's output and internal processing
    /// buffers.
    procedure clear; override;

    /// Changes a setting controlling the processing system behaviour. See the
    /// 'SETTING_...' defines for available setting ID's.
    ///
    /// \return 'TRUE' if the setting was succesfully changed
    function setSetting(settingId: Integer;   ///< Setting ID number. see SETTING_... defines.
                    value: Integer        ///< New setting value.
                    ): Boolean;

    /// Reads a setting controlling the processing system behaviour. See the
    /// 'SETTING_...' defines for available setting ID's.
    ///
    /// \return the setting value.
    function getSetting(settingId: Integer    ///< Setting ID number, see SETTING_... defines.
                   ): Integer;

    /// Returns number of samples currently unprocessed.
    function numUnprocessedSamples: longword; virtual;


    /// Other handy functions that are implemented in the ancestor classes (see
    /// classes 'FIFOProcessor' and 'FIFOSamplePipe')
    ///
    /// - receiveSamples() : Use this function to receive 'ready' processed samples from SoundTouch.
    /// - numSamples()     : Get number of 'ready' samples that can be received with
    ///                      function 'receiveSamples()'
    /// - isEmpty()        : Returns nonzero if there aren't any 'ready' samples.
    /// - clear()          : Clears all samples from ready/processing buffers.

    function latency: Integer;
  end;

implementation

/// test if two floating point numbers are equal
function TEST_FLOAT_EQUAL(a, b: Single): Boolean;
begin
  Result := (abs(a - b) < 1e-10);
end;

constructor TSoundTouch.Create;
begin
  inherited Create;

  // Initialize rate transposer and tempo changer instances
  pRateTransposer := TRateTransposer.Create;
  pTDStretch := TTDStretch.Create;

  setOutPipe(pTDStretch);

  rate := 0;
  tempo := 0;

  virtualPitch := 1.0;
  virtualRate := 1.0;
  virtualTempo := 1.0;

  calcEffectiveRateAndTempo;

  channels := 0;
  bSrateSet := FALSE;
end;

destructor TSoundTouch.Destroy;
begin
  pRateTransposer.Free;
  pTDStretch.Free;

  inherited Destroy;
end;

/// Get SoundTouch library version string
function TSoundTouch.getVersionString: string;
begin
  Result := SOUNDTOUCH_VERSION;
end;

/// Get SoundTouch library version Id
function TSoundTouch.getVersionId: longword;
begin
  Result := SOUNDTOUCH_VERSION_ID;
end;

// Sets the number of channels, 1 = mono, 2 = stereo
procedure TSoundTouch.setChannels(numChannels: longword);
begin
  if (numChannels <> 1) and (numChannels <> 2) then
  begin
    // raise Exception.Create('Illegal number of channels');
  end;
  channels := numChannels;
  pRateTransposer.setChannels(numChannels);
  pTDStretch.setChannels(numChannels);
end;

// Sets new rate control value. Normal rate = 1.0, smaller values
// represent slower rate, larger faster rates.
procedure TSoundTouch.setRate(newRate: Single);
begin
  virtualRate := newRate;
  calcEffectiveRateAndTempo;
end;

// Sets new rate control value as a difference in percents compared
// to the original rate (-50 .. +100 %)
procedure TSoundTouch.setRateChange(newRate: Single);
begin
  virtualRate := 1.0 + 0.01 * newRate;
  calcEffectiveRateAndTempo;
end;

// Sets new tempo control value. Normal tempo = 1.0, smaller values
// represent slower tempo, larger faster tempo.
procedure TSoundTouch.setTempo(newTempo: Single);
begin
  virtualTempo := newTempo;
  calcEffectiveRateAndTempo;
end;

// Sets new tempo control value as a difference in percents compared
// to the original tempo (-50 .. +100 %)
procedure TSoundTouch.setTempoChange(newTempo: Single);
begin
  virtualTempo := 1.0 + 0.01 * newTempo;
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

// Sets pitch change in semi-tones compared to the original pitch
// (-12 .. +12)
procedure TSoundTouch.setPitchSemiTones(newPitch: Integer);
begin
  setPitchOctaves(newPitch / 12.0);
end;

procedure TSoundTouch.setPitchSemiTones(newPitch: Single);
begin
  setPitchOctaves(newPitch / 12.0);
end;

// Calculates 'effective' rate and tempo values from the
// nominal control values.
procedure TSoundTouch.calcEffectiveRateAndTempo;
var
  oldTempo: Single;
  oldRate: Single;
  tempoOut: TFIFOSamplePipe;
  transOut: TFIFOSamplePipe;
begin
  oldTempo := tempo;
  oldRate := rate;

  tempo := virtualTempo / virtualPitch;
  rate := virtualPitch * virtualRate;

  if not TEST_FLOAT_EQUAL(rate, oldRate) then
  begin
    pRateTransposer.setRate(rate);
  end;
  if not TEST_FLOAT_EQUAL(tempo, oldTempo) then
  begin
    pTDStretch.setTempo(tempo);
  end;

  // #ifndef SOUNDTOUCH_PREVENT_CLICK_AT_RATE_CROSSOVER

  if rate <= 1 then
  begin
    if output <> pTDStretch then
    begin
      assert(output = pRateTransposer);
      // move samples in the current output buffer to the output of pTDStretch
      tempoOut := pTDStretch.getOutput;
      tempoOut.moveSamples(output);
      // move samples in pitch transposer's store buffer to tempo changer's input
      pTDStretch.moveSamples(pRateTransposer.getStore);

      output := pTDStretch;
    end;
  end
  else
  begin
    if output <> pRateTransposer then
    begin
      assert(output = pTDStretch);
      // move samples in the current output buffer to the output of pRateTransposer
      transOut := pRateTransposer.getOutput;
      transOut.moveSamples(output);
      // move samples in tempo changer's input to pitch transposer's input
      pRateTransposer.moveSamples(pTDStretch.getInput);

      output := pRateTransposer;
    end;
  end;
end;


// Sets sample rate.
procedure TSoundTouch.setSampleRate(srate: longword);
begin
  bSrateSet := TRUE;
  // set sample rate, leave other tempo changer parameters as they are.
  pTDStretch.setParameters(srate);
end;

// Adds 'numSamples' pcs of samples from the 'samples' memory position into
// the input of the object.
procedure TSoundTouch.putSamples(const samples: PSingle; anumSamples: longword);
begin
  if bSrateSet = FALSE then
  begin
    // raise Exception.Create('SoundTouch : Sample rate not defined');
  end
  else if channels = 0 then
  begin
    // raise Exception.Create('SoundTouch : Number of channels not defined');
  end
  // Transpose the rate of the new samples if necessary
  (* Bypass the nominal setting - can introduce a click in sound when tempo/pitch control crosses the nominal value...
  else if rate = 1.0 then
  begin
      // The rate value is same as the original, simply evaluate the tempo changer.
      assert(output = pTDStretch);
      if pRateTransposer.isEmpty = 0 then
      begin
        // yet flush the last samples in the pitch transposer buffer
        // (may happen if 'rate' changes from a non-zero value to zero)
        pTDStretch.moveSamples(pRateTransposer);
      end;
      pTDStretch.putSamples(samples, anumSamples);
  end  *)
  // SOUNDTOUCH_PREVENT_CLICK_AT_RATE_CROSSOVER
  else if rate <= 1.0 then
  begin
    // transpose the rate down, output the transposed sound to tempo changer buffer
    assert(output = pTDStretch);
    pRateTransposer.putSamples(samples, anumSamples);
    pTDStretch.moveSamples(pRateTransposer);
  end
  else
  begin
    // evaluate the tempo changer, then transpose the rate up,
    assert(output = pRateTransposer);
    pTDStretch.putSamples(samples, anumSamples);
    pRateTransposer.moveSamples(pTDStretch);
  end;
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
  nOut := numSamples;

  FillByte(buff, 128 * sizeof(SAMPLETYPE), 0);
  // "Push" the last active samples out from the processing pipeline by
  // feeding blank samples into the processing pipeline until new,
  // processed samples appear in the output (not however, more than
  // 8ksamples in any case)
  for i := Low(buff) to High(buff) do
  begin
    putSamples(buff, 64);
    if numSamples <> nOut then
    begin
      break;  // new samples have appeared in the output!
    end;
  end;

  // Clear working buffers
  pRateTransposer.clear;
  pTDStretch.clearInput;
  // yet leave the 'tempoChanger' output intouched as that's where the
  // flushed samples are!
end;

// Changes a setting controlling the processing system behaviour. See the
// 'SETTING_...' defines for available setting ID's.
function TSoundTouch.setSetting(settingId: Integer; value: Integer): Boolean;
var
  sampleRate, sequenceMs, seekWindowMs, overlapMs: Integer;
begin
  // read current tdstretch routine parameters
  pTDStretch.getParameters(@sampleRate, @sequenceMs, @seekWindowMs, @overlapMs);

  case settingId of
    SETTING_USE_AA_FILTER :
    begin
            // enables / disabless anti-alias filter
            pRateTransposer.enableAAFilter(value <> 0);
            Result := TRUE;
    end;
    SETTING_AA_FILTER_LENGTH :
    begin
            // sets anti-alias filter length
            pRateTransposer.getAAFilter.setLength(value);
            Result := TRUE;
    end;
    SETTING_USE_QUICKSEEK :
    begin
            // enables / disables tempo routine quick seeking algorithm
            pTDStretch.enableQuickSeek(value <> 0);
            Result := TRUE;
    end;
    SETTING_SEQUENCE_MS:
    begin
            // change time-stretch sequence duration parameter
            pTDStretch.setParameters(sampleRate, value, seekWindowMs, overlapMs);
            Result := TRUE;
    end;
    SETTING_SEEKWINDOW_MS:
    begin
            // change time-stretch seek window length parameter
            pTDStretch.setParameters(sampleRate, sequenceMs, value, overlapMs);
            Result := TRUE;
    end;
    SETTING_OVERLAP_MS:
    begin
            // change time-stretch overlap length parameter
            pTDStretch.setParameters(sampleRate, sequenceMs, seekWindowMs, value);
            Result := TRUE;
    end;
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
      Result := Integer(pRateTransposer.isAAFilterEnabled);
    end;

    SETTING_AA_FILTER_LENGTH :
    begin
      Result := pRateTransposer.getAAFilter.getLength;
    end;

    SETTING_USE_QUICKSEEK :
    begin
      Result := Integer(pTDStretch.isQuickSeekEnabled);
    end;

    SETTING_SEQUENCE_MS:
    begin
      pTDStretch.getParameters(nil, @temp, nil, nil);
      Result := temp;
    end;

    SETTING_SEEKWINDOW_MS:
    begin
      pTDStretch.getParameters(nil, nil, @temp, nil);
      Result := temp;
    end;

    SETTING_OVERLAP_MS:
    begin
      pTDStretch.getParameters(nil, nil, nil, @temp);
      Result := temp;
    end;

		SETTING_NOMINAL_INPUT_SEQUENCE :
    begin
			Result := pTDStretch.getInputSampleReq;
    end;

		SETTING_NOMINAL_OUTPUT_SEQUENCE :
    begin
			Result := pTDStretch.getOutputBatchSize;
    end
    else
    begin
      Result := 0
    end;
  end;
end;

// Clears all the samples in the object's output and internal processing
// buffers.
procedure TSoundTouch.clear;
begin
  pRateTransposer.clear;
  pTDStretch.clear;
end;

/// Returns number of samples currently unprocessed.
function TSoundTouch.numUnprocessedSamples: longword;
var
  psp: TFIFOSamplePipe;
begin
  if Assigned(pTDStretch) then
  begin
    psp := pTDStretch.getInput;
    if Assigned(psp) then
    begin
      Result := psp.numSamples;
      exit;
    end;
  end;
  Result := 0;
end;

function TSoundTouch.latency: Integer;
begin
  Result := pTDStretch.getLatency;
end;

end.