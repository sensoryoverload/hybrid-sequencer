unit fifosamplepipe;

{$mode objfpc}

////////////////////////////////////////////////////////////////////////////////
///
/// 'FIFOSamplePipe' : An abstract base class for classes that manipulate sound
/// samples by operating like a first-in-first-out pipe: New samples are fed
/// into one end of the pipe with the 'putSamples' function, and the processed
/// samples are received from the other end with the 'receiveSamples' function.
///
/// 'FIFOProcessor' : A base class for classes the do signal processing with 
/// the samples while operating like a first-in-first-out pipe. When samples
/// are input with the 'putSamples' function, the class processes them
/// and moves the processed samples to the given 'output' pipe object, which
/// may be either another processing stage, or a fifo sample buffer object.
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
/// Abstract base class for FIFO (first-in-first-out) sample processing classes.
  TFIFOSamplePipe = class
  public
   /// Returns a pointer to the beginning of the output samples.
    /// This function is provided for accessing the output samples directly. 
    /// Please be careful for not to corrupt the book-keeping!
    ///
    /// When using this function to output samples, also remember to 'remove' the
    /// output samples from the buffer by calling the 
    /// 'receiveSamples(numSamples)' function
    function ptrBegin: PSingle; virtual; abstract;

    /// Adds 'numSamples' pcs of samples from the 'samples' memory position to
    /// the sample buffer.
    procedure putSamples(
      const samples: PSingle;  ///< Pointer to samples.
      anumSamples: longword         ///< Number of samples to insert.
      ); virtual; abstract;

    // Moves samples from the 'other' pipe instance to this instance.
    procedure moveSamples(
      other: TFIFOSamplePipe    ///< Other pipe instance where from the receive the data.
      );

    /// Output samples from beginning of the sample buffer. Copies requested samples to 
    /// output buffer and removes them from the sample buffer. If there are less than 
    /// 'numsample' samples in the buffer, returns all that available.
    ///
    /// \return Number of samples returned.
    function receiveSamples(
      output: PSingle; ///< Buffer where to copy output samples.
      maxSamples: longword                 ///< How many samples to receive at max.
      ): longword; virtual; abstract;

    /// Adjusts book-keeping so that given number of samples are removed from beginning of the 
    /// sample buffer without copying them anywhere. 
    ///
    /// Used to reduce the number of samples in the buffer when accessing the sample buffer directly
    /// with 'ptrBegin' function.
    function receiveSamples(
      maxSamples: longword   ///< Remove this many samples from the beginning of pipe.
      ): longword; virtual; abstract;

    /// Returns number of samples currently available.
    function numSamples: longword; virtual; abstract;

    // Returns nonzero if there aren't any samples available for outputting.
    function isEmpty: Integer; virtual; abstract;

    /// Clears all the samples.
    procedure clear; virtual; abstract;
  end;

/// Base-class for sound processing routines working in FIFO principle. With this base 
/// class it's easy to implement sound processing stages that can be chained together,
/// so that samples that are fed into beginning of the pipe automatically go through 
/// all the processing stages.
///
/// When samples are input to this class, they're first processed and then put to 
/// the FIFO pipe that's defined as output of this class. This output pipe can be
/// either other processing stage or a FIFO sample buffer.
  TFIFOProcessor = class(TFIFOSamplePipe)
  protected
    /// Internal pipe where processed samples are put.
    output: TFIFOSamplePipe;

    /// Sets output pipe.
    procedure setOutPipe(pOutput: TFIFOSamplePipe);

  public
    /// Returns a pointer to the beginning of the output samples.
    /// This function is provided for accessing the output samples directly. 
    /// Please be careful for not to corrupt the book-keeping!
    ///
    /// When using this function to output samples, also remember to 'remove' the
    /// output samples from the buffer by calling the 
    /// 'receiveSamples(numSamples)' function
    function ptrBegin: PSingle; override;

    /// Constructor. Doesn't define output pipe; it has to be set be
    /// 'setOutPipe' function.
    constructor Create; virtual;

    /// Constructor. Configures output pipe.
    constructor Create(
      pOutput: TFIFOSamplePipe   ///< Output pipe.
      ); virtual;

    /// Destructor.
    destructor Destroy; override;

    /// Output samples from beginning of the sample buffer. Copies requested samples to 
    /// output buffer and removes them from the sample buffer. If there are less than 
    /// 'numsample' samples in the buffer, returns all that available.
    ///
    /// \return Number of samples returned.
    function receiveSamples(
      outBuffer: PSingle;       ///< Buffer where to copy output samples.
      maxSamples: longword          ///< How many samples to receive at max.
      ): longword; override;

    /// Adjusts book-keeping so that given number of samples are removed from beginning of the 
    /// sample buffer without copying them anywhere. 
    ///
    /// Used to reduce the number of samples in the buffer when accessing the sample buffer directly
    /// with 'ptrBegin' function.
    function receiveSamples(
      maxSamples: longword   ///< Remove this many samples from the beginning of pipe.
      ): longword; override;

    /// Returns number of samples currently available.
    function numSamples: longword; override;

    /// Returns nonzero if there aren't any samples available for outputting.
    function isEmpty: Integer; override;
  end;

implementation

procedure TFIFOSamplePipe.moveSamples(
  other: TFIFOSamplePipe  ///< Other pipe instance where from the receive the data.
  );
var
  oNumSamples: Integer;
begin
  oNumSamples := other.numSamples;

  putSamples(other.ptrBegin, oNumSamples);
  other.receiveSamples(oNumSamples);
end;

procedure TFIFOProcessor.setOutPipe(pOutput: TFIFOSamplePipe);
begin
  assert(not Assigned(output));
  assert(Assigned(pOutput));
  output := pOutput;
end;

constructor TFIFOProcessor.Create;
begin
  output := nil;
end;

constructor TFIFOProcessor.Create(pOutput: TFIFOSamplePipe);
begin
  output := pOutput;
end;

destructor TFIFOProcessor.Destroy;
begin
  inherited Destroy;
end;

function TFIFOProcessor.ptrBegin: PSingle;
begin
  Result := output.ptrBegin;
end;

function TFIFOProcessor.receiveSamples(
  outBuffer: PSingle;
  maxSamples: longword
  ): longword;
begin
  Result := output.receiveSamples(outBuffer, maxSamples);
end;

function TFIFOProcessor.receiveSamples(
  maxSamples: longword
  ): longword;
begin
  Result := output.receiveSamples(maxSamples);
end;

function TFIFOProcessor.numSamples: longword;
begin
  Result := output.numSamples;
end;

function TFIFOProcessor.isEmpty: Integer;
begin
  Result := output.isEmpty;
end;

end.