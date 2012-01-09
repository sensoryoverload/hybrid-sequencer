unit ratetransposer;

{$mode objfpc}

{$fputype sse2}

////////////////////////////////////////////////////////////////////////////////
///
/// Sample rate transposer. Changes sample rate by using linear interpolation
/// together with anti-alias filtering (first order interpolation with anti-
/// alias filtering should be quite adequate for this application).
///
/// Use either of the derived classes of 'RateTransposerInteger' or
/// 'RateTransposerFloat' for corresponding integer/floating point tranposing
/// algorithm implementation.
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
  aafilter, fifosamplepipe, fifosamplebuffer;

/// A common linear samplerate transposer class.
///
/// Note: Use function "RateTransposer::newInstance()" to create a new class
/// instance instead of the "new" operator; that function automatically
/// chooses a correct implementation depending on if integer or floating
/// arithmetics are to be used.
type
  TRateTransposer = class(TFIFOProcessor)
  protected
    /// Anti-alias filter object
    pAAFilter: TAAFilter;

    fSlopecount: Single;
    sPrevSampleL: Single;
    sPrevSampleR: Single;

    fRate: Single;

    numChannels: Integer;

    /// Buffer for collecting samples to feed the anti-alias filter between
    /// two batches
    storeBuffer: TFIFOSampleBuffer;

    /// Buffer for keeping samples between transposing & anti-alias filter
    tempBuffer: TFIFOSampleBuffer;

    /// Output sample buffer
    outputBuffer: TFIFOSampleBuffer;

    bUseAAFilter: Boolean;

    procedure resetRegisters; virtual;

    function transposeStereo(dest: PSingle;
                         const src: PSingle;
                         anumSamples: longword): longword; virtual;
    function transposeMono(dest: PSingle;
                       const src: PSingle;
                       anumSamples: longword): longword; virtual;
    function transpose(dest: PSingle;
                   const src: PSingle;
                   anumSamples: longword): longword; inline;

    procedure downsample(const src: PSingle;
                    anumSamples: longword);
    procedure upsample(const src: PSingle;
                 anumSamples: longword);

    /// Transposes sample rate by applying anti-alias filter to prevent folding.
    /// Returns amount of samples returned in the "dest" buffer.
    /// The maximum amount of samples that can be returned at a time is set by
    /// the 'set_returnBuffer_size' function.
    procedure processSamples(const src: Psingle;
                        anumSamples: longword);


  public
    constructor Create; override;
    destructor Destroy; override;

    /// Returns the output buffer object
    function getOutput: TFIFOSamplePipe;

    /// Returns the store buffer object
    function getStore: TFIFOSamplePipe;

    /// Return anti-alias filter object
    function getAAFilter: TAAFilter;

    /// Enables/disables the anti-alias filter. Zero to disable, nonzero to enable
    procedure enableAAFilter(newMode: Boolean);

    /// Returns nonzero if anti-alias filter is enabled.
    function isAAFilterEnabled: Boolean;

    /// Sets new target rate. Normal rate = 1.0, smaller values represent slower
    /// rate, larger faster rates.
    procedure setRate(newRate: Single); virtual;

    /// Sets the number of channels, 1 = mono, 2 = stereo
    procedure setChannels(nchannels: Integer);

    /// Adds 'numSamples' pcs of samples from the 'samples' memory position into
    /// the input of the object.
    procedure putSamples(const samples: PSingle; anumSamples: longword); override;

    /// Clears all the samples in the object
    procedure clear; override;

    /// Returns nonzero if there aren't any samples available for outputting.
    function isEmpty: Integer; override;
  end;

implementation

// Constructor
constructor TRateTransposer.Create;
begin
  numChannels := 2;

  /// Output sample buffer
  outputBuffer := TFIFOSampleBuffer.Create(numChannels);

  /// Buffer for keeping samples between transposing & anti-alias filter
  tempBuffer := TFIFOSampleBuffer.Create(numChannels);

  storeBuffer := TFIFOSampleBuffer.Create(numChannels);

  inherited Create(outputBuffer);

  bUseAAFilter := TRUE;
  fRate := 0;

  // Instantiates the anti-alias filter with default tap length
  // of 32
  pAAFilter := TAAFilter.Create(32);

  // Notice: use local function calling syntax for sake of clarity,
  // to indicate the fact that C++ constructor can't call virtual functions.
  resetRegisters;
  setRate(1.0);
end;

destructor TRateTransposer.Destroy;
begin
  pAAFilter.Free;
  storeBuffer.Free;
  tempBuffer.Free;
  outputBuffer.Free;
end;

function TRateTransposer.getOutput: TFIFOSamplePipe;
begin
  Result := outputBuffer;
end;

/// Returns the store buffer object
function TRateTransposer.getStore: TFIFOSamplePipe;
begin
  Result := storeBuffer;
end;

/// Enables/disables the anti-alias filter. Zero to disable, nonzero to enable
procedure TRateTransposer.enableAAFilter(newMode: Boolean);
begin
  bUseAAFilter := newMode;
end;

/// Returns nonzero if anti-alias filter is enabled.
function TRateTransposer.isAAFilterEnabled: Boolean;
begin
  Result := bUseAAFilter;
end;

function TRateTransposer.getAAFilter: TAAFilter;
begin
  Result := pAAFilter;
end;

// Sets new target iRate. Normal iRate = 1.0, smaller values represent slower 
// iRate, larger faster iRates.
procedure TRateTransposer.setRate(newRate: Single);
var
  fCutoff: double;
begin
  fRate := newRate;

  // design a new anti-alias filter
  if newRate > 1.0 then
  begin
      fCutoff := 0.5 / newRate;
  end
  else
  begin
      fCutoff := 0.5 * newRate;
  end;
  pAAFilter.setCutoffFreq(fCutoff);
end;

// Outputs as many samples of the 'outputBuffer' as possible, and if there's
// any room left, outputs also as many of the incoming samples as possible.
// The goal is to drive the outputBuffer empty.
//
// It's allowed for 'output' and 'input' parameters to point to the same
// memory position.
(*
procedure TRateTransposer.flushStoreBuffer;
begin
    if storeBuffer.isEmpty then
    begin
      return;
      exit;
    end;

    outputBuffer.moveSamples(storeBuffer);
end;*)

// Adds 'nSamples' pcs of samples from the 'samples' memory position into
// the input of the object.
procedure TRateTransposer.putSamples(const samples: PSingle; anumSamples: longword);
begin
  processSamples(samples, anumSamples);
end;

// Transposes up the sample rate, causing the observed playback 'rate' of the
// sound to decrease
procedure TRateTransposer.upsample(const src: PSingle; anumSamples: longword);
var
  count, sizeTemp, num: longword;
begin
    // If the parameter 'uRate' value is smaller than 'SCALE', first transpose
    // the samples and then apply the anti-alias filter to remove aliasing.

    // First check that there's enough room in 'storeBuffer' 
    // (+16 is to reserve some slack in the destination buffer)
    sizeTemp := Round(anumSamples / fRate + 16.0);

    // Transpose the samples, store the result into the end of "storeBuffer"
    count := transpose(storeBuffer.ptrEnd(sizeTemp), src, anumSamples);
    storeBuffer.putSamples(count);

    // Apply the anti-alias filter to samples in "store output", output the
    // result to "dest"
    num := storeBuffer.numSamples;
    count := pAAFilter.evaluate(outputBuffer.ptrEnd(num),
        storeBuffer.ptrBegin, num, numChannels);
    outputBuffer.putSamples(count);

    // Remove the processed samples from "storeBuffer"
    storeBuffer.receiveSamples(count);
end;

// Transposes down the sample rate, causing the observed playback 'rate' of the
// sound to increase
procedure TRateTransposer.downsample(const src: PSingle; anumSamples: longword);
var
  count, sizeTemp: longword;
begin
  // If the parameter 'uRate' value is larger than 'SCALE', first apply the
  // anti-alias filter to remove high frequencies (prevent them from folding
  // over the lover frequencies), then transpose.

  // Add the new samples to the end of the storeBuffer
  storeBuffer.putSamples(src, anumSamples);

  // Anti-alias filter the samples to prevent folding and output the filtered
  // data to tempBuffer. Note : because of the FIR filter length, the
  // filtering routine takes in 'filter_length' more samples than it outputs.
  assert(tempBuffer.isEmpty > 0);
  sizeTemp := storeBuffer.numSamples;

  count := pAAFilter.evaluate(tempBuffer.ptrEnd(sizeTemp),
      storeBuffer.ptrBegin, sizeTemp, numChannels);

	if count = 0 then
  begin
    exit;
  end;

  // Remove the filtered samples from 'storeBuffer'
  storeBuffer.receiveSamples(count);

  // Transpose the samples (+16 is to reserve some slack in the destination buffer)
  sizeTemp := Round(anumSamples / fRate + 16.0);
  count := transpose(outputBuffer.ptrEnd(sizeTemp), tempBuffer.ptrBegin, count);
  outputBuffer.putSamples(count);
end;


// Transposes sample rate by applying anti-alias filter to prevent folding. 
// Returns amount of samples returned in the "dest" buffer.
// The maximum amount of samples that can be returned at a time is set by
// the 'set_returnBuffer_size' function.
procedure TRateTransposer.processSamples(const src: PSingle; anumSamples: longword);
var
  count: longword;
  sizeReq: longword;
begin

  if anumSamples = 0 then
  begin
    exit;
  end;
  assert(Assigned(pAAFilter));

  // If anti-alias filter is turned off, simply transpose without applying
  // the filter
  if bUseAAFilter = FALSE then
  begin
    sizeReq := Round(anumSamples / fRate + 1.0);
    count := transpose(outputBuffer.ptrEnd(sizeReq), src, anumSamples);
    outputBuffer.putSamples(count);
    exit;
  end;

  // Transpose with anti-alias filter
  if fRate < 1.0 then
  begin
    upsample(src, anumSamples);
  end
  else
  begin
    downsample(src, anumSamples);
  end;
end;


// Transposes the sample rate of the given samples using linear interpolation. 
// Returns the number of samples returned in the "dest" buffer
function TRateTransposer.transpose(dest: PSingle; const src: PSingle; anumSamples: longword): longword; inline;
begin
  if numChannels = 2 then
  begin
    Result := transposeStereo(dest, src, anumSamples);
  end
  else
  begin
    Result := transposeMono(dest, src, anumSamples);
  end;
end;

// Sets the number of channels, 1 = mono, 2 = stereo
procedure TRateTransposer.setChannels(nChannels: Integer);
begin
  assert(nChannels > 0);
  if numChannels = nChannels then
  begin
    exit;
  end;

  assert((nChannels = 1) or (nChannels = 2));
  numChannels := nChannels;

  storeBuffer.setChannels(numChannels);
  tempBuffer.setChannels(numChannels);
  outputBuffer.setChannels(numChannels);

  // Inits the linear interpolation registers
  resetRegisters;
end;

// Clears all the samples in the object
procedure TRateTransposer.clear;
begin
  outputBuffer.clear;
  storeBuffer.clear;
end;

// Returns nonzero if there aren't any samples available for outputting.
function TRateTransposer.isEmpty: Integer;
var
  res: Integer;
begin
  res := Inherited isEmpty;
  if res = 0 then
    Result := 0
  else
    Result := storeBuffer.isEmpty;
end;

procedure TRateTransposer.resetRegisters;
begin
  fSlopeCount := 0;
  sPrevSampleL := 0;
  sPrevSampleR := 0;
end;

// Transposes the sample rate of the given samples using linear interpolation. 
// 'Mono' version of the routine. Returns the number of samples returned in 
// the "dest" buffer
function TRateTransposer.transposeMono(dest: PSingle; const src: PSingle; anumSamples: longword): longword;
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
    fSlopeCount += fRate;
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
      fSlopeCount += fRate;
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
function TRateTransposer.transposeStereo(dest: PSingle; const src: PSingle; anumSamples: longword): longword;
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
    fSlopeCount += fRate;
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
      fSlopeCount += fRate;
    end;
  end;
lend:
  // Store the last sample for the next round
  sPrevSampleL := src[2 * anumSamples - 2];
  sPrevSampleR := src[2 * anumSamples - 1];

  Result := i;
end;

end.