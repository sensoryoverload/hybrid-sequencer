unit firfilter;

{$mode objfpc}

////////////////////////////////////////////////////////////////////////////////
///
/// General FIR digital filter routines with MMX optimization. 
///
/// Note : MMX optimized functions reside in a separate, platform-specific file, 
/// e.g. 'mmx_win.cpp' or 'mmx_gcc.cpp'
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
  math;

type
  TFIRFilter = class
  protected
    // Number of FIR filter taps
    length: word;
    // Number of FIR filter taps divided by 8
    lengthDiv8: word;

    // Result divider factor in 2^k format
    resultDivFactor: word;

    // Result divider value.
    resultDivider: Single;

    // Memory for filter coefficients
    filterCoeffs: PSingle;

    function evaluateFilterStereo(
      dest: PSingle;
      src: PSingle;
      numSamples: word): word; virtual;
    function evaluateFilterMono(
      dest: PSingle;
      src: PSingle;
      numSamples: word): word; virtual;

  public
    constructor Create;
    destructor Destroy; override;

    /// Applies the filter to the given sequence of samples.
    /// Note : The amount of outputted samples is by value of 'filter_length'
    /// smaller than the amount of input samples.
    ///
    /// \return Number of samples copied to 'dest'.
    function evaluate(
      dest: PSingle;
      src: PSingle;
      numSamples: word;
      numChannels: word): word;

    function getLength: word;

    procedure setCoefficients(
      const coeffs:PSingle;
      newLength: word;
      uResultDivFactor: word); virtual;
  end;

implementation

(*****************************************************************************
 *
 * Implementation of the class 'TFIRFilter'
 *
 *****************************************************************************)

constructor TFIRFilter.Create;
begin
  resultDivFactor := 0;
  resultDivider := 0;
  length := 0;
  lengthDiv8 := 0;
  filterCoeffs := nil;
end;


destructor TFIRFilter.Destroy;
begin
  FreeMem(filterCoeffs);

  inherited Destroy;
end;

// Usual C-version of the filter routine for stereo sound
function TFIRFilter.evaluateFilterStereo(
  dest: PSingle;
  src: PSingle;
  numSamples: word): word;
var
  i, j, lend: word;
  suml, sumr: double;
  dScaler: double;
  ptr: PSingle;
begin
 // when using floating point samples, use a scaler instead of a divider
  // because division is much slower operation than multiplying.
  dScaler := 1.0 / resultDivider;

  assert(length <> 0);
  assert(src <> nil);
  assert(dest <> nil);
  assert(filterCoeffs <> nil);

  lend := 2 * (numSamples - length);

  j := 0;
  while j < lend do
  begin
    suml := 0;
    sumr := 0;
    ptr := src + j;

    i := 0;
    while i < length do
    begin
        // loop is unrolled by factor of 4 here for efficiency
        suml += ptr[2 * i + 0] * filterCoeffs[i + 0] +
                ptr[2 * i + 2] * filterCoeffs[i + 1] +
                ptr[2 * i + 4] * filterCoeffs[i + 2] +
                ptr[2 * i + 6] * filterCoeffs[i + 3];
        sumr += ptr[2 * i + 1] * filterCoeffs[i + 0] +
                ptr[2 * i + 3] * filterCoeffs[i + 1] +
                ptr[2 * i + 5] * filterCoeffs[i + 2] +
                ptr[2 * i + 7] * filterCoeffs[i + 3];
      i += 4;
    end;

    suml *= dScaler;
    sumr *= dScaler;

    dest[j] := suml;
    dest[j + 1] := sumr;

    j += 2;
  end;
  Result := numSamples - length;
end;




// Usual C-version of the filter routine for mono sound
function TFIRFilter.evaluateFilterMono(
  dest: PSingle;
  src: PSingle;
  numSamples: word): word;
var
  i, j, lend: word;
  sum: double;
  dScaler: double;
begin
  // when using floating point samples, use a scaler instead of a divider
  // because division is much slower operation than multiplying.
  dScaler := 1.0 / resultDivider;

  assert(length <> 0);

  lend := numSamples - length;
  for j := 0 to Pred(lend) do
  begin
    sum := 0;
    i := 0;
    while i < length do
    begin
      // loop is unrolled by factor of 4 here for efficiency
      sum += src[i + 0] * filterCoeffs[i + 0] +
             src[i + 1] * filterCoeffs[i + 1] +
             src[i + 2] * filterCoeffs[i + 2] +
             src[i + 3] * filterCoeffs[i + 3];
      i += 4;
    end;

    sum *= dScaler;

    dest[j] := sum;
    //src++;
    src := src + 1;
  end;
  Result := lend;
end;


// Set filter coeffiecients and length.
//
// Throws an exception if filter length isn't divisible by 8
procedure TFIRFilter.setCoefficients(const coeffs: PSingle; newLength: word; uResultDivFactor: word);
begin
    assert(newLength > 0);
{    if (newLength mod 8) > 0 then
      raise Exception.Create('FIR filter length not divisible by 8');  }

    lengthDiv8 := newLength div 8;
    length := lengthDiv8 * 8;
    assert(length = newLength);

    resultDivFactor := uResultDivFactor;
    resultDivider := power(2.0, resultDivFactor);

    FreeMem(filterCoeffs);
    filterCoeffs := GetMem(length * SizeOf(Single));
    Move(coeffs^, filterCoeffs^, length * sizeof(Single));
end;


function TFIRFilter.getLength: word;
begin
  Result := length;
end;



// Applies the filter to the given sequence of samples. 
//
// Note : The amount of outputted samples is by value of 'filter_length' 
// smaller than the amount of input samples.
function TFIRFilter.evaluate(dest: PSingle; src: PSingle; numSamples: word; numChannels: word): word;
begin
  assert((numChannels = 1) or (numChannels = 2));

  assert(length > 0);
  assert(lengthDiv8 * 8 = length);
  if numSamples < length then
  begin
    Result := 0;
    exit;
  end;
  if numChannels = 2 then
  begin
    Result := evaluateFilterStereo(dest, src, numSamples);
  end
  else
  begin
    Result := evaluateFilterMono(dest, src, numSamples);
  end;
end;

end.
