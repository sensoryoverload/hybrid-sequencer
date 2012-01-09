unit aafilter;

{$mode objfpc}

////////////////////////////////////////////////////////////////////////////////
///
/// FIR low-pass (anti-alias) filter with filter coefficient design routine and
/// MMX optimization. 
/// 
/// Anti-alias filter is used to prevent folding of high frequencies when 
/// transposing the sample rate with interpolation.
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
  firfilter;

const
  PI = 3.141592655357989;
  TWOPI = (2 * PI);

type
  TAAFilter = class
  protected
    pFIR: TFIRFilter;

    /// Low-pass filter cut-off frequency, negative = invalid
    cutoffFreq: double;

    /// num of filter taps
    length: word;

    /// Calculate the FIR coefficients realizing the given cutoff-frequency
    procedure  calculateCoeffs;
  public
    constructor Create(len: word);
    destructor Destroy; override;

    /// Sets new anti-alias filter cut-off edge frequency, scaled to sampling
    /// frequency (nyquist frequency = 0.5). The filter will cut off the
    /// frequencies than that.
    procedure setCutoffFreq(newCutoffFreq: double);

    /// Sets number of FIR filter taps, i.e. ~filter complexity
    procedure setLength(newLength: word);

    function getLength: word;

    /// Applies the filter to the given sequence of samples.
    /// Note : The amount of outputted samples is by value of 'filter length'
    /// smaller than the amount of input samples.
    function evaluate(
      dest: PSingle;
      src: PSingle;
      numSamples: word;
      numChannels: word): word;
  end;

implementation

(*****************************************************************************
 *
 * Implementation of the class 'TAAFilter'
 *
 *****************************************************************************)

constructor TAAFilter.Create(len: word);
begin
  pFIR := TFIRFilter.Create;

  cutoffFreq := 0.5;
  setLength(len);
end;



destructor TAAFilter.Destroy;
begin
  pFIR.Free;

  inherited Destroy;
end;

// Sets new anti-alias filter cut-off edge frequency, scaled to
// sampling frequency (nyquist frequency = 0.5).
// The filter will cut frequencies higher than the given frequency.
procedure TAAFilter.setCutoffFreq(newCutoffFreq: double);
begin
  cutoffFreq := newCutoffFreq;
  calculateCoeffs;
end;



// Sets number of FIR filter taps
procedure TAAFilter.setLength(newLength: word);
begin
  length := newLength;
  calculateCoeffs;
end;

// Calculates coefficients for a low-pass FIR filter using Hamming window
procedure TAAFilter.calculateCoeffs;
var
  i: word;
  cntTemp, temp, tempCoeff,h, w: double;
  fc2, wc: double;
  scaleCoeff, sum: double;
  work: pdouble;
  coeffs: psingle;
begin

  assert(length >= 2);
  assert(length mod 4 = 0);
  assert(cutoffFreq >= 0);
  assert(cutoffFreq <= 0.5);

  work := GetMem(length * SizeOf(double));
  coeffs := GetMem(length * SizeOf(single));

  fc2 := 2.0 * cutoffFreq;
  wc := PI * fc2;
  tempCoeff := TWOPI / length;

  sum := 0;
  for i := 0 to Pred(length) do
  begin
      cntTemp := i - (length / 2);

      temp := cntTemp * wc;
      if temp <> 0 then
      begin
          h := fc2 * sin(temp) / temp;                     // sinc function
      end
      else
      begin
          h := 1.0;
      end;
      w := 0.54 + 0.46 * cos(tempCoeff * cntTemp);       // hamming window

      temp := w * h;
      work[i] := temp;

      // calc net sum of coefficients
      sum += temp;
  end;

  // ensure the sum of coefficients is larger than zero
  assert(sum > 0);

  // ensure we've really designed a lowpass filter...
  assert(work[length div 2] > 0);
  assert(work[length div 2 + 1] > -1e-6);
  assert(work[length div 2 - 1] > -1e-6);

  // Calculate a scaling coefficient in such a way that the result can be
  // divided by 16384
  scaleCoeff := 16384.0 / sum;

  for i := 0 to Pred(length) do
  begin
      // scale & round to nearest integer
      temp := work[i] * scaleCoeff;
      if temp >= 0 then
        temp += 0.5
      else
        temp += -0.5;

      // ensure no overfloods
      assert((temp >= -32768) and (temp <= 32767));
      coeffs[i] := temp;
  end;

  // Set coefficients. Use divide factor 14 => divide result by 2^14 = 16384
  pFIR.setCoefficients(coeffs, length, 14);

  FreeMem(work);
  FreeMem(coeffs);
end;


// Applies the filter to the given sequence of samples. 
// Note : The amount of outputted samples is by value of 'filter length' 
// smaller than the amount of input samples.
function TAAFilter.evaluate(dest: PSingle; src: PSingle; numSamples: word; numChannels: word): word;
begin
  Result := pFIR.evaluate(dest, src, numSamples, numChannels);
end;


function TAAFilter.getLength: word;
begin
  Result := pFIR.getLength;
end;

end.