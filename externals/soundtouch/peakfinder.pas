unit peakfinder;

////////////////////////////////////////////////////////////////////////////////
///
/// Peak detection routine. 
///
/// The routine detects highest value on an array of values and calculates the 
/// precise peak location as a mass-center of the 'hump' around the peak value.
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

type
  TPeakFinder = class
  protected
    /// Min, max allowed peak positions within the data vector
    minPos, maxPos: Integer;

    /// Calculates the mass center between given vector items.
    function calcMassCenter(const data: PSingle; ///< Data vector.
                         firstPos: Integer;      ///< Index of first vector item beloging to the peak.
                         lastPos: Integer        ///< Index of last vector item beloging to the peak.
                         ): double;

    /// Finds the data vector index where the monotoniously decreasing signal crosses the
    /// given level.
    function findCrossingLevel(const data: PSingle;  ///< Data vector.
                            level: Single;        ///< Goal crossing level.
                            peakpos: Integer;        ///< Peak position index within the data vector.
                            direction: Integer       /// Direction where to proceed from the peak: 1 = right, -1 = left.
                            ): Integer;

    // Finds real 'top' of a peak hump from neighnourhood of the given 'peakpos'.
    function findTop(const data: PSingle; peakpos: Integer): Integer;


    /// Finds the 'ground' level, i.e. smallest level between two neighbouring peaks, to right- 
    /// or left-hand side of the given peak position.
    function findGround(const data: PSingle;     /// Data vector.
                     peakpos: Integer;           /// Peak position index within the data vector.
                     direction: Integer          /// Direction where to proceed from the peak: 1 = right, -1 = left.
                     ): Integer;

    /// get exact center of peak near given position by calculating local mass of center
    function getPeakCenter(const data: PSingle; peakpos: Integer): double;

  public
    /// Constructor. 
    constructor Create;

    /// Detect exact peak position of the data vector by finding the largest peak 'hump'
    /// and calculating the mass-center location of the peak hump.
    ///
    /// \return The location of the largest base harmonic peak hump.
    function detectPeak(const data: PSingle; /// Data vector to be analyzed. The data vector has
                                        /// to be at least 'maxPos' items long.
                     aminPos: Integer;        ///< Min allowed peak location within the vector data.
                     amaxPos: Integer         ///< Max allowed peak location within the vector data.
                     ): double;
  end;

implementation

uses
  math;

function max(x, y: Single): Single;
begin
  if x > y then
    Result := x
  else
    Result := y;
end;

constructor TPeakFinder.Create;
begin
  minPos := 0;
  maxPos := 0;
end;


// Finds real 'top' of a peak hump from neighnourhood of the given 'peakpos'.
function TPeakFinder.findTop(const data: PSingle; peakpos: Integer): Integer;
var
  i: Integer;
  lstart, lend: Integer;
  refvalue: Single;
begin
  refvalue := data[peakpos];

  // seek within ±10 points
  lstart := peakpos - 10;
  if lstart < minPos then
    lstart := minPos;
  lend := peakpos + 10;
  if lend > maxPos then
    lend := maxPos;

  for i := lstart to Pred(lend) do
  begin
    if data[i] > refvalue then
    begin
      peakpos := i;
      refvalue := data[i];
    end;
  end;

  // failure if max value is at edges of seek range => it's not peak, it's at slope.
  if (peakpos = lstart) or (peakpos = lend) then
    Result := 0
  else
    Result := peakpos;
end;

// Finds 'ground level' of a peak hump by starting from 'peakpos' and proceeding
// to direction defined by 'direction' until next 'hump' after minimum value will 
// begin
function TPeakFinder.findGround(const data: PSingle; peakpos: Integer; direction: Integer): Integer;
var
  lowpos: Integer;
  pos: Integer;
  climb_count: Integer;
  refvalue: Single;
  delta: Single;
  prevpos: Integer;
begin

  climb_count := 0;
  refvalue := data[peakpos];
  lowpos := peakpos;

  pos := peakpos;

  while (pos > minPos) and (pos < maxPos) do
  begin
    prevpos := pos;
    pos += direction;

    // calculate derivate
    delta := data[pos] - data[prevpos];
    if delta <= 0 then
    begin
      // going downhill, ok
      if climb_count > 0 then
      begin
        Dec(climb_count);  // decrease climb count
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
      Inc(climb_count);
      if climb_count > 5 then break;    // we've been climbing too long => it's next uphill => quit
    end;
  end;
  Result := lowpos;
end;

// Find offset where the value crosses the given level, when starting from 'peakpos' and
// proceeds to direction defined in 'direction'
function TPeakFinder.findCrossingLevel(const data: PSingle; level: Single; peakpos: Integer; direction: Integer): Integer;
var
  peaklevel: Single;
  pos: Integer;
begin

  peaklevel := data[peakpos];
  assert(peaklevel >= level);
  pos := peakpos;
  while (pos >= minPos) and (pos < maxPos) do
  begin
    if (data[pos + direction] < level) then
    begin
      Result := pos;   // crossing found
      exit;
    end;
    pos += direction;
  end;
  Result := -1;  // not found
end;


// Calculates the center of mass location of 'data' array items between 'firstPos' and 'lastPos'
function TPeakFinder.calcMassCenter(const data: PSingle; firstPos: Integer; lastPos: Integer): double;
var
  i: Integer;
  sum: Single;
  wsum: Single;
begin

  sum := 0;
  wsum := 0;
  for i := firstPos to Pred(lastPos) do
  begin
    sum += i * data[i];
    wsum += data[i];
  end;

  if wsum < 1e-6 then
  begin
    Result := 0;
  end
  else
  begin
    Result := sum / wsum;
  end;
end;



/// get exact center of peak near given position by calculating local mass of center
function TPeakFinder.getPeakCenter(const data: PSingle; peakpos: Integer): double;
var
  peakLevel: Single;            // peak level
  crosspos1, crosspos2: Integer;   // position where the peak 'hump' crosses cutting level
  cutLevel: Single;             // cutting value
  groundLevel: Single;          // ground level of the peak
  gp1, gp2: Integer;               // bottom positions of the peak 'hump'
begin

  // find ground positions.
  gp1 := findGround(data, peakpos, -1);
  gp2 := findGround(data, peakpos, 1);

  groundLevel := max(data[gp1], data[gp2]);
  peakLevel := data[peakpos];

  if groundLevel < 1e-9 then
  begin
    Result := 0;                // ground level too small => detection failed
    exit;
  end;
  if (peakLevel / groundLevel) < 1.3 then
  begin
    Result := 0;   // peak less than 30% of the ground level => no good peak detected
    exit;
  end;

  // calculate 70%-level of the peak
  cutLevel := 0.70 * peakLevel + 0.30 * groundLevel;
  // find mid-level crossings
  crosspos1 := findCrossingLevel(data, cutLevel, peakpos, -1);
  crosspos2 := findCrossingLevel(data, cutLevel, peakpos, 1);

  if (crosspos1 < 0) or (crosspos2 < 0) then
  begin
    Result := 0;   // no crossing, no peak..
    exit;
  end;

  // calculate mass center of the peak surroundings
  Result := calcMassCenter(data, crosspos1, crosspos2);
end;

function TPeakFinder.detectPeak(const data: PSingle; aminPos: Integer; amaxPos: Integer): double;
var
  i: Integer;
  peakpos: Integer;                // position of peak level
  highPeak, peak: double;
  hp: Integer;
  peaktmp, harmonic: double;
  i1,i2: Integer;
  diff: double;
begin

  Self.minPos := aminPos;
  Self.maxPos := amaxPos;

  // find absolute peak
  peakpos := minPos;
  peak := data[minPos];
  for i := minPos + 1 to Pred(maxPos) do
  begin
    if data[i] > peak then
    begin
      peak := data[i];
      peakpos := i;
    end;
  end;

  // Calculate exact location of the highest peak mass center
  highPeak := getPeakCenter(data, peakpos);
  peak := highPeak;

  // Now check if the highest peak were in fact harmonic of the true base beat peak
  // - sometimes the highest peak can be Nth harmonic of the true base peak yet
  // just a slightly higher than the true base

  // Round?? hp := (highPeak + 0.5);
  hp := Round(highPeak);

  for i := 3 to 9 do
  begin

    harmonic := i * 0.5;
    //peakpos := highPeak / harmonic + 0.5;
    peakpos := Round(highPeak / harmonic);
    if peakpos < minPos then break;
    peakpos := findTop(data, peakpos);   // seek true local maximum index
    if peakpos = 0 then continue;         // no local max here

    // calculate mass-center of possible harmonic peak
    peaktmp := getPeakCenter(data, peakpos);

    // accept harmonic peak if
    // (a) it is found
    // (b) is within ±4% of the expected harmonic interval
    // (c) has at least half x-corr value of the max. peak

    diff := harmonic * peaktmp / highPeak;
    if (diff < 0.96) or (diff > 1.04) then continue;   // peak too afar from expected

    // now compare to highest detected peak
//        i1 = highPeak + 0.5;
    i1 := Round(highPeak);
//        i2 = peaktmp + 0.5;
    i2 := Round(peaktmp);
    if data[i2] >= 0.5 * data[i1] then
    begin
      // The harmonic is at least half as high primary peak,
      // thus use the harmonic peak instead
      peak := peaktmp;
    end;
  end;

  Result := peak;
end;

end.