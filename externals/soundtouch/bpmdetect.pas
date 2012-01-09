unit bpmdetect;

////////////////////////////////////////////////////////////////////////////////
///
/// Beats-per-minute (BPM) detection routine.
///
/// The beat detection algorithm works as follows:
/// - Use function 'inputSamples' to input a chunks of samples to the class for
///   analysis. It's a good idea to enter a large sound file or stream in smallish
///   chunks of around few kilosamples in order not to extinguish too much RAM memory.
/// - Inputted sound data is decimated to approx 500 Hz to reduce calculation burden,
///   which is basically ok as low (bass) frequencies mostly determine the beat rate.
///   Simple averaging is used for anti-alias filtering because the resulting signal
///   quality isn't of that high importance.
/// - Decimated sound data is enveloped, i.e. the amplitude shape is detected by
///   taking absolute value that's smoothed by sliding average. Signal levels that
///   are below a couple of times the general RMS amplitude level are cut away to
///   leave only notable peaks there.
/// - Repeating sound patterns (e.g. beats) are detected by calculating short-term 
///   autocorrelation function of the enveloped signal.
/// - After whole sound data file has been analyzed as above, the bpm level is 
///   detected by function 'getBpm' that finds the highest peak of the autocorrelation 
///   function, calculates it's precise location and converts this reading to bpm's.
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
  FIFOSampleBuffer,
  PeakFinder;

const
  /// Minimum allowed BPM rate. Used to restrict accepted result above a reasonable limit.
  MIN_BPM = 29;

  /// Maximum allowed BPM rate. Used to restrict accepted result below a reasonable limit.
  MAX_BPM = 200;

  INPUT_BLOCK_SAMPLES = 2048;
  DECIMATED_BLOCK_SAMPLES = 256;

type
  /// Class for calculating BPM rate for audio data.
  TBPMDetect = class
  protected
    /// Auto-correlation accumulator bins.
    xcorr: PSingle;
    
    /// Amplitude envelope sliding average approximation level accumulator
    envelopeAccu: double;

    /// RMS volume sliding average approximation level accumulator
    RMSVolumeAccu: double;

    /// Level below which to cut off signals
    cutCoeff: double;

    /// Accumulator for accounting what proportion of samples exceed cutCoeff level
    aboveCutAccu: double;

    /// Accumulator for total samples to calculate proportion of samples that exceed cutCoeff level
    totalAccu: double;

    /// Sample average counter.
    decimateCount: Integer;

    /// Sample average accumulator for FIFO-like decimation.
    decimateSum: double;

    /// Decimate sound by this coefficient to reach approx. 500 Hz.
    decimateBy: Integer;

    /// Auto-correlation window length
    windowLen: Integer;

    /// Number of channels (1 = mono, 2 = stereo)
    channels: Integer;

    /// sample rate
    sampleRate: Integer;

    /// Beginning of auto-correlation window: Autocorrelation isn't being updated for
    /// the first these many correlation bins.
    windowStart: Integer;
 
    /// FIFO-buffer for decimated processing samples.
    buffer: TFIFOSampleBuffer;

    /// Updates auto-correlation function for given number of decimated samples that 
    /// are read from the internal 'buffer' pipe (samples aren't removed from the pipe 
    /// though).
    procedure updateXCorr(process_samples: Integer      /// How many samples are processed.
                     );

    /// Decimates samples to approx. 500 Hz.
    ///
    /// \return Number of output samples.
    function decimate(dest: PSingle;      ///< Destination buffer
                 src: PSingle; ///< Source sample buffer
                 numsamples: Integer                     ///< Number of source samples.
                 ): Integer;

    /// Calculates amplitude envelope for the buffer of samples.
    /// Result is output to 'samples'.
    procedure calcEnvelope(samples: PSingle;  ///< Pointer to input/output data buffer
                      numsamples: Integer                    ///< Number of samples in buffer
                      );

  public
    /// Constructor.
    constructor Create(numChannels: Integer;  ///< Number of channels in sample data.
              asampleRate: Integer    ///< Sample rate in Hz.
              );

    /// Destructor.
    destructor Destroy; override;

    /// Inputs a block of samples for analyzing: Envelopes the samples and then
    /// updates the autocorrelation estimation. When whole song data has been input
    /// in smaller blocks using this function, read the resulting bpm with 'getBpm' 
    /// function. 
    /// 
    /// Notice that data in 'samples' array can be disrupted in processing.
    procedure inputSamples(samples: PSingle;    ///< Pointer to input/working data buffer
                      numSamples: Integer                            ///< Number of samples in buffer
                      );


    /// Analyzes the results and returns the BPM rate. Use this function to read result
    /// after whole song data has been input to the class by consecutive calls of
    /// 'inputSamples' function.
    ///
    /// \return Beats-per-minute rate, or zero if detection failed.
    function getBpm: Single;
  end;

implementation

const
/// decay constant for calculating RMS volume sliding average approximation
/// (time constant is about 10 sec)
  avgdecay = 0.99986;

/// Normalization coefficient for calculating RMS sliding average approximation.
  avgnorm = (1 - avgdecay);

constructor TBPMDetect.Create(numChannels: Integer; aSampleRate: Integer);
begin
  sampleRate := aSampleRate;
  channels := numChannels;

  decimateSum := 0;
  decimateCount := 0;

  envelopeAccu := 0;

  // Initialize RMS volume accumulator to RMS level of 3000 (out of 32768) that's
  // a typical RMS signal level value for song data. This value is then adapted
  // to the actual level during processing.

  // float samples, scaled to range [-1..+1[
  RMSVolumeAccu := (0.092 * 0.092) / avgnorm;

  cutCoeff := 1.75;
  aboveCutAccu := 0;
  totalAccu := 0;

  // choose decimation factor so that result is approx. 500 Hz
  decimateBy := asampleRate div 500;
  assert(decimateBy > 0);
  assert(INPUT_BLOCK_SAMPLES < decimateBy * DECIMATED_BLOCK_SAMPLES);

  // Calculate window length & starting item according to desired min & max bpms
  windowLen := (60 * asampleRate) div (decimateBy * MIN_BPM);
  windowStart := (60 * asampleRate) div (decimateBy * MAX_BPM);

  assert(windowLen > windowStart);

  // allocate new working objects
  xcorr := GetMem(windowLen * sizeof(Single));
  memset(xcorr, 0, windowLen * sizeof(Single));

  // allocate processing buffer
  buffer := TFIFOSampleBuffer.Create(1);
  // we do processing in mono mode
  buffer.setChannels(1);
  buffer.clear();
end;



destructor TBPMDetect.Destroy;
begin
  FreeMem(xcorr);
  buffer.Free;

  inherited;
end;



/// convert to mono, low-pass filter & decimate to about 500 Hz. 
/// return number of outputted samples.
///
/// Decimation is used to remove the unnecessary frequencies and thus to reduce 
/// the amount of data needed to be processed as calculating autocorrelation 
/// function is a very-very heavy operation.
///
/// Anti-alias filtering is done simply by averaging the samples. This is really a 
/// poor-man's anti-alias filtering, but it's not so critical in this kind of application
/// (it'd also be difficult to design a high-quality filter with steep cut-off at very 
/// narrow band)
function TBPMDetect.decimate(dest: PSingle; src: PSingle; numsamples: Integer): Integer;
var
  count, outcount: Integer;
  lout: double;
  j: Integer;
begin

  assert(channels > 0);
  assert(decimateBy > 0);
  outcount := 0;
  for count := 0 to Pred(numsamples) do
  begin

    // convert to mono and accumulate
    for j := 0 to Pred(channels) do
    begin
      decimateSum += src[j];
    end;
    src += j;

    Inc(decimateCount);
    if decimateCount >= decimateBy then
    begin
      // Store every Nth sample only
      lout := decimateSum / (decimateBy * channels);
      decimateSum := 0;
      decimateCount := 0;
      dest[outcount] := lout;
      Inc(outcount);
    end;
  end;
  Result := outcount;
end;



// Calculates autocorrelation function of the sample history buffer
procedure TBPMDetect.updateXCorr(process_samples: Integer);
var
  offs: Integer;
  pBuffer: PSingle;
  sum: double;
  i: Integer;
begin

  assert(buffer.numSamples() >= (process_samples + windowLen));

  pBuffer := buffer.ptrBegin;
  for offs := windowStart to Pred(windowLen) do
  begin

    sum := 0;
    for i := 0 to Pred(process_samples) do
    begin
      sum += pBuffer[i] * pBuffer[i + offs];    // scaling the sub-result shouldn't be necessary
    end;
//    xcorr[offs] *= xcorr_decay;   // decay 'xcorr' here with suitable coefficients
                                    // if it's desired that the system adapts automatically to
                                    // various bpms, e.g. in processing continouos music stream.
                                    // The 'xcorr_decay' should be a value that's smaller than but
                                    // close to one, and should also depend on 'process_samples' value.

    xcorr[offs] += sum;
  end;
end;


// Calculates envelope of the sample data
procedure TBPMDetect.calcEnvelope(samples: PSingle; numsamples: Integer);
const
  decay = 0.7;               // decay constant for smoothing the envelope
  norm = (1 - decay);
var
  i: Integer;
  lout: double;
  val: double;
  d: double;
begin

  for i := 0 to Pred(numsamples) do
  begin
    // calc average RMS volume
    RMSVolumeAccu *= avgdecay;
    val := abs(samples[i]);
    RMSVolumeAccu += val * val;

    // cut amplitudes that are below cutoff ~2 times RMS volume
    // (we're interested in peak values, not the silent moments)
    val -= cutCoeff * sqrt(RMSVolumeAccu * avgnorm);
    if val > 0 then
    begin
      aboveCutAccu += 1.0;  // sample above threshold
    end
    else
    begin
      val := 0;
    end;

    totalAccu += 1.0;

    // maintain sliding statistic what proportion of 'val' samples is
    // above cutoff threshold
    aboveCutAccu *= 0.99931;  // 2 sec time constant
    totalAccu *= 0.99931;

    if totalAccu > 500 then
    begin
      // after initial settling, auto-adjust cutoff level so that ~8% of
      // values are above the threshold
      d := (aboveCutAccu / totalAccu) - 0.08;
      cutCoeff += 0.001 * d;
    end;

    // smooth amplitude envelope
    envelopeAccu *= decay;
    envelopeAccu += val;
    lout := envelopeAccu * norm;

    samples[i] := lout;
  end;

  // check that cutoff doesn't get too small - it can be just silent sequence!
  if cutCoeff < 1.5 then
  begin
    cutCoeff := 1.5;
  end;
end;



procedure TBPMDetect.inputSamples(samples: PSingle; numSamples: Integer);
var
  decimated: array[0..DECIMATED_BLOCK_SAMPLES] of Single;
  block: Integer;
  decSamples: Integer;
  processLength: Integer;
begin

  // iterate so that max INPUT_BLOCK_SAMPLES processed per iteration
  while numSamples > 0 do
  begin
    if numSamples > INPUT_BLOCK_SAMPLES then
      block := INPUT_BLOCK_SAMPLES
    else
      block := numSamples;

    // decimate. note that converts to mono at the same time
    decSamples := decimate(decimated, samples, block);
    samples += block * channels;
    numSamples -= block;

    // envelope new samples and add them to buffer
    calcEnvelope(decimated, decSamples);
    buffer.putSamples(decimated, decSamples);
  end;

  // when the buffer has enought samples for processing...
  if buffer.numSamples() > windowLen then
  begin

    // how many samples are processed
    processLength := buffer.numSamples() - windowLen;

    // ... calculate autocorrelations for oldest samples...
    updateXCorr(processLength);
    // ... and remove them from the buffer
    buffer.receiveSamples(processLength);
  end;
end;



function TBPMDetect.getBpm: Single;
var
  peakPos: double;
  coeff: double;
  peakFinder: TPeakFinder;
begin
  coeff := 60.0 * (sampleRate / decimateBy);

  // find peak position
  peakFinder := TPeakFinder.Create;
  try
    peakPos := peakFinder.detectPeak(xcorr, windowStart, windowLen);
  finally
    peakFinder.Free;
  end;

  assert(decimateBy <> 0);
  if peakPos < 1e-9 then
  begin
    Result := 0; // detection failed.
    exit;
  end;

  // calculate BPM
  Result := coeff / peakPos;
end;

end.