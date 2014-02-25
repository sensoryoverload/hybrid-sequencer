unit fifosamplebuffer;

{$mode objfpc}

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

interface

uses
  Classes, Sysutils, fifosamplepipe;

const
  // Minimum allowed BPM rate. Used to restrict accepted result above a reasonable limit.
  MIN_BPM = 29;

  // Maximum allowed BPM rate. Used to restrict accepted result below a reasonable limit.
  MAX_BPM = 200;

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

  TFIFOSampleBuffer = class(TFIFOSamplePipe)
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
    function ptrBegin: PSingle; override;

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
                        ); override;

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
                                ): longword; override;

    /// Adjusts book-keeping so that given number of samples are removed from beginning of the
    /// sample buffer without copying them anywhere.
    ///
    /// Used to reduce the number of samples in the buffer when accessing the sample buffer directly
    /// with 'ptrBegin' function.
    function receiveSamples(maxSamples: longword   ///< Remove this many samples from the beginning of pipe.
                            ): longword; override;

    /// Returns number of samples currently available.
    function numSamples: longword; override;

    /// Sets number of channels, 1 = mono, 2 = stereo.
    procedure setChannels(numChannels: longword);

    /// Returns nonzero if there aren't any samples available for outputting.
    function isEmpty: integer; override;

    /// Clears all the samples.
    procedure clear; override;
  end;

procedure memcpy(dest, source: Pointer; count: Integer);
function memmove(dest, src: Pointer; n: Cardinal): Pointer;
procedure memset(P: Pointer; B: Integer; count: Integer);
function max(x, y: single): single;
function max(x, y: Integer): Integer;

implementation

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

{ TFIFOSampleBuffer }

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
(*    // enlarge the buffer in 4kbyte steps (round up to next 4k boundary)
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
*)

    sizeInBytes := (capacityRequirement * fchannels * sizeof(Single) + Boundary - 1) and not (Boundary - 1);
    assert(sizeInBytes mod 2 = 0);
    FreeMem(buffer);
    buffer := getmem(sizeInBytes + Alignment);
    if samplesInBuffer > 0 then
    begin
      Move(ptrBegin^, buffer^, samplesInBuffer * fchannels * sizeof(Single));
    end;
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

