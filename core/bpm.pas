unit bpm;

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
//  Extended and converted to FreePascal by Robbert Latumahina
//  bpm.pas
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

{$mode Objfpc}{$H+}

interface

uses
  Classes, Sysutils;

const
  // Minimum allowed BPM rate. Used to restrict accepted result above a reasonable limit.
  MIN_BPM = 29;

  // Maximum allowed BPM rate. Used to restrict accepted result below a reasonable limit.
  MAX_BPM = 230;

  INPUT_BLOCK_SAMPLES = 2048;
  DECIMATED_BLOCK_SAMPLES = 256;

  /// decay constant for calculating RMS volume sliding average approximation
  /// (time constant is about 10 sec)
  avgdecay = 0.99986;

  /// Normalization coefficient for calculating RMS sliding average approximation.
  avgnorm = (1 - avgdecay);

type
  SAMPLETYPE = single;

  { TFIFOSampleBuffer }

  TFIFOSampleBuffer = class(TObject)
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
    procedure putSamples(samples: PSingle;  ///< Pointer to samples.
                         numSamples: longword                        ///< Number of samples to insert.
                        );

    /// Adjusts the book-keeping to increase number of samples in the buffer without
    /// copying any actual samples.
    ///
    /// This function is used to update the number of samples in the sample buffer
    /// when accessing the buffer directly with 'ptrEnd' function. Please be
    /// careful though!
    procedure putSamples(numSamples: longword   ///< Number of samples been inserted.
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


  { TPeakFinder }

  TPeakFinder = class(TObject)
  private
    function GetPeakCenter(Data: Psingle; PeakPos: Longint): Single;
  protected
    /// Min, max allowed peak positions within the data vector
    fminPos, fmaxPos: longint;

    /// Calculates the mass center between given vector items.
    function calcMassCenter(data: psingle;      ///< Data vector.
                            firstPos,           ///< Index of first vector item beloging to the peak.
                            lastPos: longint    ///< Index of last vector item beloging to the peak.
                            ): longint;

    /// Finds the data vector index where the monotoniously decreasing signal crosses the
    /// given level.
    function findCrossingLevel(data: psingle;   ///< Data vector.
                            level: single;      ///< Goal crossing level.
                            peakpos,            ///< Peak position index within the data vector.
                            direction: longint  /// Direction where to proceed from the peak: 1 = right, -1 = left.
                            ): longint;

    /// Finds the 'ground' level, i.e. smallest level between two neighbouring peaks, to right-
    /// or left-hand side of the given peak position.
    function findGround(data: psingle;          /// Data vector.
                        peakpos,                /// Peak position index within the data vector.
                        direction: longint      /// Direction where to proceed from the peak: 1 = right, -1 = left.
                        ): longint;

  public
    /// Detect exact peak position of the data vector by finding the largest peak 'hump'
    /// and calculating the mass-center location of the peak hump.
    ///
    /// \return The exact mass-center location of the largest peak hump.
    function detectPeak(data: psingle;    /// Data vector to be analyzed. The data vector has
                                          /// to be at least 'maxPos' items long.
                        minPos,           ///< Min allowed peak location within the vector data.
                        maxPos: longint   ///< Max allowed peak location within the vector data.
                        ): single;
  end;



  { TBPMDetect }
  TBPMDetect = class(TObject)
  private
    /// Auto-correlation accumulator bins.
    xcorr: psingle;

    /// Amplitude envelope sliding average approximation level accumulator
    envelopeAccu: single;

    /// RMS volume sliding average approximation level accumulator
    RMSVolumeAccu: single;

    /// Sample average counter.
    decimateCount: integer;

    /// Sample average accumulator for FIFO-like decimation.
    decimateSum: double;

    /// Level below which to cut off signals
    cutCoeff: double;

    /// Accumulator for accounting what proportion of samples exceed cutCoeff level
    aboveCutAccu: double;

    /// Accumulator for total samples to calculate proportion of samples that exceed cutCoeff level
    totalAccu: double;

    /// Decimate sound by this coefficient to reach approx. 500 Hz.
    decimateBy: integer;

    /// Auto-correlation window length
    windowLen: integer;

    /// Number of channels (1 = mono, 2 = stereo)
    fchannels: integer;

    /// sample rate
    fsampleRate: integer;

    /// Beginning of auto-correlation window: Autocorrelation isn't being updated for
    /// the first these many correlation bins.
    windowStart: integer;

    /// FIFO-buffer for decimated processing samples.
    buffer: TFIFOSampleBuffer;
    
    /// Initialize the class for processing.
    procedure init(NumChannels, SampleRate: integer);

    /// Updates auto-correlation function for given number of decimated samples that
    /// are read from the internal 'buffer' pipe (samples aren't removed from the pipe
    /// though).
    procedure updateXCorr(process_samples: longint);

    /// Decimates samples to approx. 500 Hz.
    ///
    /// \return Number of output samples.
    function decimate(dest, src: PSingle;numsamples: longint): longint;

    /// Calculates amplitude envelope for the buffer of samples.
    /// Result is output to 'samples'.
    procedure calcEnvelope(samples: PSingle; numsamples: longint);
  public
    /// Constructor.
    constructor Create(NumChannels, SampleRate: longint);

    /// Destructor.
    destructor Destroy; override;

    /// Inputs a block of samples for analyzing: Envelopes the samples and then
    /// updates the autocorrelation estimation. When whole song data has been input
    /// in smaller blocks using this function, read the resulting bpm with 'getBpm'
    /// function.
    ///
    /// Notice that data in 'samples' array can be disrupted in processing.
    procedure inputSamples(samples: PSingle; numSamples: longint);


    /// Analyzes the results and returns the BPM rate. Use this function to read result
    /// after whole song data has been input to the class by consecutive calls of
    /// 'inputSamples' function.
    ///
    /// \return Beats-per-minute rate, or zero if detection failed.
    function getBpm: single;
end;

procedure memcpy(dest, source: Pointer; count: Integer);
function memmove(dest, src: Pointer; n: Cardinal): Pointer;
procedure memset(P: Pointer; B: Integer; count: Integer);
function max(x, y: single): single;
function max(x, y: Integer): Integer;

implementation

//    #define max(x, y) (((x) > (y)) ? (x) : (y))
function max(x, y: Integer): Integer;
begin
  if x > y then
    Result := x
  else
    Result := y;
end;

function max(x, y: single): single;
begin
  if x > y then
    Result := x
  else
    Result := y;
end;

procedure memcpy(dest, source: Pointer; count: Integer);

begin
  Move(source^, dest^, count);
end;

function memmove(dest, src: Pointer; n: Cardinal): Pointer;
begin
  Move(src^, dest^, n);
  Result := dest;
end;

procedure memset(P: Pointer; B: Integer; count: Integer);
begin
  FillChar(P^, count, B);
end;

{ TBPMDetect }

procedure Tbpmdetect.Init(Numchannels, Samplerate: Integer);
begin
  self.fsampleRate := Samplerate;

  // choose decimation factor so that result is approx. 500 Hz
  decimateBy := sampleRate div 500;
  assert(decimateBy > 0);
  assert(INPUT_BLOCK_SAMPLES < decimateBy * DECIMATED_BLOCK_SAMPLES);

  // Calculate window length & starting item according to desired min & max bpms
  windowLen := (60 * sampleRate) div (decimateBy * MIN_BPM);
  windowStart := (60 * sampleRate) div (decimateBy * MAX_BPM);
  
  windowLen *= 2;

  assert(windowLen > windowStart);

  // allocate new working objects
  xcorr := getmem(windowLen *  sizeof(single));
  memset(xcorr, 0, windowLen * sizeof(single));

  // we do processing in mono mode
  buffer.setChannels(1);
  buffer.clear;
end;

procedure Tbpmdetect.Updatexcorr(Process_samples: Longint);
var
  offs: longint;
  pBuffer: PSingle;
  sum: double;
  i: longint;
begin
  pBuffer := buffer.ptrBegin;
  for offs := windowStart to windowLen do
  begin
    sum := 0;
    for i := 0 to process_samples - 1 do
    begin
      sum += pBuffer[i] * pBuffer[i + offs];    // scaling the sub-result shouldn't be necessary
    end;
//        xcorr[offs] *= xcorr_decay;   // decay 'xcorr' here with suitable coefficients
                                    // if it's desired that the system adapts automatically to
                                    // various bpms, e.g. in processing continouos music stream.
                                    // The 'xcorr_decay' should be a value that's smaller than but
                                    // close to one, and should also depend on 'process_samples' value.

    xcorr[offs] += single(sum);
  end;
end;

function Tbpmdetect.Decimate(Dest,
  Src: PSingle; Numsamples: Longint): Longint;
var
  count, outcount: longint;
  output: double;
begin
  assert(decimateBy <> 0);
  outcount := 0;
  for count := 0 to numsamples - 1 do
  begin
    decimateSum += src[count];

    inc(decimateCount);
    if decimateCount >= decimateBy then
    begin
      // Store every Nth sample only
      output := double(decimateSum / decimateBy);
      decimateSum := 0;
      decimateCount := 0;
      dest[outcount] := Single(output);
      inc(outcount);
    end;
  end;
  Result := outcount;
end;

procedure Tbpmdetect.Calcenvelope(
  Samples: PSingle; Numsamples: Longint);
const
  decay = 0.7;               // decay constant for smoothing the envelope
  norm = (1 - decay);
var
  i: longint;
  output: double;
  val: single;
  d: double;
begin
  for i := 0 to numsamples - 1 do
  begin
    // calc average RMS volume
    RMSVolumeAccu *= avgdecay;
    val := single(abs(samples[i]));
    RMSVolumeAccu += val * val;

    // cut amplitudes that are below 2 times average RMS volume
    // (we're interested in peak values, not the silent moments)
    val -= 2 * single(sqrt(RMSVolumeAccu * avgnorm));

    //val = (val > 0) ? val : 0;
    if val > 0 then val := val else val := 0;

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
    output := double(envelopeAccu * norm);

    samples[i] := single(output);
  end;
end;

constructor Tbpmdetect.Create(numchannels, samplerate: Longint);
begin
  //xcorr := nil; //NULL;

  buffer := TFIFOSampleBuffer.Create(numchannels);

  decimateSum := 0;
  decimateCount := 0;
  decimateBy := 0;

  self.fsampleRate := sampleRate;
  self.fchannels := numChannels;

  envelopeAccu := 0;
  cutCoeff := 1.75;
  aboveCutAccu := 0;

  // Initialize RMS volume accumulator to RMS level of 3000 (out of 32768) that's
  // a typical RMS signal level value for song data. This value is then adapted
  // to the actual level during processing.

  // float samples, scaled to range [-1..+1[
  RMSVolumeAccu := (0.092 * 0.092) / avgnorm;

  init(numchannels, sampleRate);
end;

destructor Tbpmdetect.Destroy;
begin
  if Assigned(xcorr) then
    Freemem(xcorr);
  buffer.Free;

  inherited;
end;

procedure Tbpmdetect.Inputsamples(
  Samples: PSingle; Numsamples: Longint);
var
  decimated: array[0..DECIMATED_BLOCK_SAMPLES] of Single;
  i: longint;
  processLength: longint;
begin
  // convert from stereo to mono if necessary
  if fchannels = 2 then
  begin
    for i := 0 to numSamples - 1 do
    begin
      samples[i] := (samples[i * 2] + samples[i * 2 + 1]) / 2;
    end;
  end;

  // decimate
  numSamples := decimate(decimated, samples, numSamples);

  // envelope new samples and add them to buffer
  calcEnvelope(decimated, numSamples);
  buffer.putSamples(decimated, numSamples);

  // when the buffer has enought samples for processing...
  if longint(buffer.numSamples) > windowLen then
  begin
    // how many samples are processed
    processLength := buffer.numSamples - windowLen;

    // ... calculate autocorrelations for oldest samples...
    updateXCorr(processLength);
    // ... and remove them from the buffer
    buffer.receiveSamples(processLength);
  end;

end;

function Tbpmdetect.Getbpm: Single;
var
  peakPos: single;
  peakFinder: TPeakFinder;
begin
  peakFinder := TPeakFinder.Create;

  // find peak position
  peakPos := peakFinder.detectPeak(xcorr, windowStart, windowLen);

  assert(decimateBy <> 0);
  if peakPos < 0.0000001 then
    Result := 0.0 // detection failed.
  else
  begin
    // calculate BPM
    Result := 60.0 * ((single(fsampleRate) / single(decimateBy)) / peakPos);
    
    // Correct result
    if Result < 60 then
    begin
      while Result < 60 do
        Result *= 2;
    end
    else
    begin
      while Result > 230 do
        Result /= 2;
    end;
  end;

  if Result = 0 then Result := 120;

  peakFinder.Free;
end;

{ TPeakFinder }

function Tpeakfinder.Calcmasscenter(Data: Psingle; Firstpos,
  Lastpos: Longint): Longint;
var
  i: longint;
  sum: single;
  wsum: single;
begin
  sum := 0;
  wsum := 0;
  i := firstPos;
  while i <= lastPos do
  begin
    sum += single(i * data[i]);
    wsum += data[i];
    inc(i);
  end;
  Result := Round(sum / wsum);
end;

function Tpeakfinder.Findcrossinglevel(Data: Psingle; Level: Single; Peakpos,
  Direction: Longint): Longint;
var
  peaklevel: single;
  pos: longint;
begin

  peaklevel := data[peakpos];
  assert(peaklevel >= level);
  pos := peakpos;
  Result := -1; // not found
  while (pos >= fminPos) and (pos < fmaxPos) do
  begin
    if data[pos + direction] < level then
    begin
      Result := pos;   // crossing found
      break;
    end;
    pos += direction;
  end;
end;

function Tpeakfinder.Findground(Data: Psingle; Peakpos,
  Direction: Longint): Longint;
var
  refvalue: single;
  lowpos: longint;
  pos: longint;
  climb_count: longint;
  delta: single;
  prevpos: longint;

begin

  climb_count := 0;
  refvalue := data[peakpos];
  lowpos := peakpos;

  pos := peakpos;

  while (pos > fminPos) and (pos < fmaxPos) do
  begin

    prevpos := pos;
    pos += direction;

    // calculate derivate
    delta := data[pos] - data[prevpos];
    if delta <= 0 then
    begin
      // going downhill, ok
      if climb_count = 1 then // TODO should be > 0 ??      was = 1
      begin
        dec(climb_count);  // decrease climb count
      end;

      // check if new minimum found
      if data[pos] < refvalue then
      begin
        // new minimum found
        lowpos := pos;
        refvalue := data[pos];
      end;
    end
    else
    begin
      // going uphill, increase climbing counter
      inc(climb_count);
      if climb_count > 5 then
        break;    // we've been climbing too long => it's next uphill => quit
    end;
  end;
  Result := lowpos;
end;

function Tpeakfinder.Detectpeak(Data: Psingle; Minpos,
  Maxpos: Longint): Single;
var
  i: longint;
  peakpos: longint;                 // position of peak level
  peakLevel: single;                // peak level
  peaktmp, tmp: double;
  i1,i2: longint;
  highPeak, peak: double;


begin
  self.fminPos := minPos;
  self.fmaxPos := maxPos;

  // find absolute peak
  peakpos := minPos;
  peakLevel := data[minPos];
  for i := minPos + 1 to maxPos - 1 do
  begin
    if data[i] > peakLevel then
    begin
      peakLevel := data[i];
      peakpos := i;
    end;
  end;

  // Calculate exact location of the highest peak mass center
  highPeak := getPeakCenter(data, peakpos);
  peak := highPeak;

  // Now check if the highest peak were in fact harmonic of the true base beat peak
  // - sometimes the highest peak can be Nth harmonic of the true base peak yet
  // just a slightly higher than the true base
  for i := 2 to 10 do
  begin

      peakpos := round(highPeak / i + 0.5);
      if (peakpos < minPos) then break;

      // calculate mass-center of possible base peak
      peaktmp := getPeakCenter(data, peakpos);

      // now compare to highest detected peak
      i1 := round(highPeak + 0.5);
      i2 := round(peaktmp + 0.5);
      tmp := 2 * (data[i2] - data[i1]) / (data[i2] + data[i1]);
      if abs(tmp) < 0.1 then
      begin
          // The highest peak is harmonic of almost as high base peak,
          // thus use the base peak instead
          peak := peaktmp;
      end;
  end;

  result :=  peak;

end;

function Tpeakfinder.GetPeakCenter(Data: Psingle; PeakPos: Longint): Single;
var
  peakLevel: single;                // peak level
  crosspos1, crosspos2: longint;    // position where the peak 'hump' crosses cutting level
  cutLevel: single;                 // cutting value
  groundLevel: single;              // ground level of the peak
  gp1, gp2: longint;                // bottom positions of the peak 'hump'

begin

  // find ground positions.
  gp1 := findGround(data, peakpos, -1);
  gp2 := findGround(data, peakpos, 1);

  peakLevel := data[peakpos];
  groundLevel := max(data[gp1], data[gp2]);

  if (groundLevel < 0.000001) then
  begin
    Result := 0; // ground level too small => detection failed
  end
  else
  begin
    if ((peakLevel / groundLevel) < 1.3) then
    begin
      Result := 0; // peak less than 30% of the ground level => no good peak detected
    end
    else
    begin
      // calculate 70%-level of the peak
      cutLevel := 0.70 * peakLevel + 0.30 * groundLevel;
      // find mid-level crossings
      crosspos1 := findCrossingLevel(data, cutLevel, peakpos, -1);
      crosspos2 := findCrossingLevel(data, cutLevel, peakpos, 1);

      if (crosspos1 < 0) or (crosspos2 < 0) then
      begin
        Result := 0;   // no crossing, no peak..
      end
      else
        // calculate mass center of the peak surroundings
        Result := calcMassCenter(data, crosspos1, crosspos2);
    end;
  end;
end;

{ TFIFOSampleBuffer }

procedure Tfifosamplebuffer.Rewind;
begin
  if bufferPos <> 0 then
  begin
    memmove(buffer, ptrBegin, sizeof(SAMPLETYPE) * fchannels * samplesInBuffer);
    bufferPos := 0;
  end;
end;

procedure Tfifosamplebuffer.Ensurecapacity(Capacityrequirement: longword);
var
  tempUnaligned,
  temp: PSingle;
begin
  if capacityRequirement > getCapacity then
  begin
    // enlarge the buffer in 4kbyte steps (round up to next 4k boundary)
    //sizeInBytes := (capacityRequirement * fchannels * sizeof(SAMPLETYPE) + 4095) and -4096;
    sizeInBytes := (capacityRequirement * fchannels * sizeof(SAMPLETYPE) + 4096);
    assert(sizeInBytes mod 2 = 0);
    //tempUnaligned := getmem(sizeInBytes div sizeof(SAMPLETYPE) + 16 div sizeof(SAMPLETYPE));
    tempUnaligned := getmem(sizeInBytes);
    if not Assigned(tempUnaligned) then
    begin
       writeln('Could not allocate memory!\n');
    end;
    //temp := Pjack_default_audio_sample_t((longword(tempUnaligned + 15) and -16));
    temp := tempUnaligned;
    MemCpy(temp, ptrBegin, samplesInBuffer * fchannels * sizeof(SAMPLETYPE));
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
  Result := sizeInBytes div (fchannels * sizeof(SAMPLETYPE));
end;

constructor Tfifosamplebuffer.Create(Numchannels: longword);
begin
  if Numchannels = 0 then
    Numchannels := 2;
  sizeInBytes := 0; // reasonable initial value
  buffer := nil;  //new SAMPLETYPE[sizeInBytes / sizeof(SAMPLETYPE)];
  bufferUnaligned := nil;
  samplesInBuffer := 0;
  bufferPos := 0;
  fchannels := numChannels;

  inherited Create;
end;

destructor Tfifosamplebuffer.Destroy;
begin
  if Assigned(bufferUnaligned) then
    Freemem(bufferUnaligned);

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

procedure Tfifosamplebuffer.Putsamples(Numsamples: longword);
var
  req: longword;
begin
  req := samplesInBuffer + numSamples;
  ensureCapacity(req);
  samplesInBuffer += numSamples;
end;

procedure Tfifosamplebuffer.Putsamples(
  Samples: PSingle; Numsamples: longword);
begin
  memcpy(ptrEnd(numSamples), samples, sizeof(SAMPLETYPE) * numSamples * fchannels);
  samplesInBuffer += numSamples;
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
  memcpy(output, ptrBegin, fchannels * sizeof(SAMPLETYPE) * num);
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

