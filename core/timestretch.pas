unit timestretch;
{*************************************************************************

->	Converted with C to Pascal Converter 1.0
->	Release: 1.5.11.2008
->	Email: al_gun@ncable.net.au
->	Updates: http://cc.codegear.com/Author/302259
->	Copyright © 2005-2008 Ural Gunaydin, All rights reserved.

*************************************************************************}

interface

uses
	Messages, SysUtils, Classes, mmx, bpm;

// qtractorTimeStretch.cpp
//
(****************************************************************************
   Copyright (C) 2005-2008, rncbc aka Rui Nuno Capela. All rights reserved.

   Adapted and refactored from the SoundTouch cLibrary (L)GPL,
   Copyright (C) 2001-2006, Olli Parviainen.

   This cProgram is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This cProgram is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this cProgram; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

***************************************************************)

//include 'qtractorTimeStretch.h'

// Default values for sound processing parameters.
const
  DEFAULT_SEQUENCE_MS = 82;
  DEFAULT_SEEKWINDOW_MS = 14;
  DEFAULT_OVERLAP_MS = 12;

type
  PPSingle = ^PSingle;

  { TStretcher }
  TStretcher = class(TObject)
  private
  	m_iChannels: word;
  	m_fTempo: single;
  	m_bQuickSeek: boolean;

  	m_iSampleRate: word;
  	m_iSequenceMs: word;
  	m_iSeekWindowMs: word;
  	m_iOverlapMs: word;

  	m_iFramesReq: word;

    m_ppMidBuffer: ppsingle;
  	m_ppRefMidBuffer: ppsingle;
  	m_ppRefMidBufferUnaligned: ppsingle;
  	m_ppFrames: ppsingle;

  	m_iOverlapLength: word;
  	m_iSeekLength: word;
  	m_iSeekWindowLength: word;
  	m_fNominalSkip: single;
  	m_fSkipFract: single;
  	m_outputBuffer: TFIFOSampleBuffer;
  	m_inputBuffer: TFIFOSampleBuffer;
  	m_bMidBufferDirty: Boolean;
  public
    constructor Create(iChannels, iSampleRate: Word);
    destructor Destroy; override;
    procedure setChannels(iChannels: Word);
    function channels: Word;
    procedure setTempo(fTempo: Single);
    function tempo: Single;
    procedure setQuickSeek(bQuickSeek: Boolean);
    function isQuickSeek: Boolean;
    procedure setParameterSampleRate(iSampleRate: word);
    procedure setParameterSequenceMS(iSequenceMs: word);
    procedure setParameterSeekWindowMS(iSeekWindowMs: word);
    procedure setParameterOverlapMS(iOverlapMs: word);
    procedure getParameters(piSampleRate, piSequenceMs, piSeekWindowMs, piOverlapMs:pword);
    procedure clearMidBuffer;
    procedure clearInput;
    procedure clear;
    function seekBestOverlapPosition: Word;
    procedure processFrames;
    procedure putFrames(ppFrames: array of Single; iFrames: Word);
    function receiveFrames(ppFrames: array of Single; iFrames: Word): Word;
    function frames: Word;
    procedure flushInput;
    procedure calcCrossCorrReference;
    procedure calcOverlapLength;
  end;

implementation

// Cross-correlation value calculation over the overlap period.
//
//include <xmmintrin.h>

// SSE detection.
function sse_enabled: Boolean;
begin
	result:= is_sse_cpu;
end;

// Standard (slow) version.
function std_cross_corr(var pV1: PSingle; var pV2: PSingle; iOverlapLength: Word): Double;
var
	i: word;
	dCorr: Double;
begin
	dCorr := 0.0;

	for i := 1 to iOverlapLength - 1 do
		dCorr:= dCorr + pV1[i] * pV2[i];

	result:= dCorr;
end;


//---------------------------------------------------------------------------
// qtractorTimeStretch - Time-stretch (tempo change) effect for processed sound.
//

// Constructor.
constructor TStretcher.Create(iChannels, iSampleRate: Word);
begin 
	setChannels(iChannels);

	m_fTempo := 1.0;
	m_bQuickSeek := false;

	m_bMidBufferDirty := false;
	m_ppMidBuffer := nil;
	m_ppRefMidBuffer := nil;
	m_ppRefMidBufferUnaligned := nil;
	m_ppFrames := nil;

	m_iOverlapLength := 0;

	{if sse_enabled then
		m_pfnCrossCorr := sse_cross_corr
	else
  	m_pfnCrossCorr := @std_cross_corr;}

	setParameterSampleRate(iSampleRate);
end;


// Destructor.
destructor TStretcher.Destroy;
var
	i: word;
begin 
	if Assigned(m_ppFrames) then
	begin 
		for i := 0 to m_iChannels - 1 do 
		begin 
			freemem(m_ppMidBuffer[i]);
			freemem(m_ppRefMidBufferUnaligned[i]);
		end;
		freemem(m_ppMidBuffer);
		freemem(m_ppRefMidBufferUnaligned);
		freemem(m_ppRefMidBuffer);
		freemem(m_ppFrames);
	end;
end;



// Sets the number of channels, 1=mono, 2=stereo.
procedure TStretcher.setChannels(iChannels: Word);
begin 
	if (m_iChannels = iChannels) then 
		Exit;

	m_iChannels := iChannels;
	m_inputBuffer.setChannels(m_iChannels);
	m_outputBuffer.setChannels(m_iChannels);
end;


// Get the assigne number of channels, 1=mono, 2=stereo.
function TStretcher.channels: Word;
begin 
	result := m_iChannels;
end;


// Sets new target tempo; less than 1.0 values represent
// slower tempo, greater than 1.0 represents faster tempo.
procedure TStretcher.setTempo(fTempo: Single);
begin 
	// Set new is tempo scaling.
	m_fTempo := fTempo;

	// Calculate ideal skip length (according to tempo value) 
	m_fNominalSkip := m_fTempo * (m_iSeekWindowLength - m_iOverlapLength);
	m_fSkipFract := 0;

	// Calculate how many samples are needed in the input buffer 
	// to process another batch of samples.
	m_iFramesReq := Round( (m_fNominalSkip + 0.5) + m_iOverlapLength );
	if (m_iFramesReq < m_iSeekWindowLength) then 
		m_iFramesReq := m_iSeekWindowLength;
	m_iFramesReq:= m_iFramesReq + m_iSeekLength;

	clear;

	// These will be enough for most purposes, and
	// shoudl avoid in-the-fly buffer re-allocations...
	m_inputBuffer.ensureCapacity(m_iFramesReq);
	m_outputBuffer.ensureCapacity(m_iFramesReq);
end;

// Get assigned target tempo.
function TStretcher.tempo: Single;
begin 
	result:= m_fTempo;
end;


// Set quick-seek mode (hierachical search).
procedure TStretcher.setQuickSeek(bQuickSeek: Boolean);
begin 
	m_bQuickSeek := bQuickSeek;
end;

// Get quick-seek mode.
function TStretcher.isQuickSeek: Boolean;
begin 
	result:= m_bQuickSeek;
end;

// Sets routine control parameters.
// These control are certain time constants defining
// how the sound is stretched to the desired duration.
//
// iSampleRate = sample rate of the sound.
// iSequenceMs = one processing sequence length in milliseconds.
// iSeekWindowMs = seeking window length for scanning the best
//      overlapping position.
// iOverlapMs = overlapping length.
procedure TStretcher.setParameterSampleRate(iSampleRate: word);
begin
	m_iSampleRate := iSampleRate;
	m_iSeekLength := (m_iSampleRate * m_iSeekWindowMs) div 1000;
	m_iSeekWindowLength := (m_iSampleRate * m_iSequenceMs) div 1000;

	calcOverlapLength;

	// Set tempo to recalculate required frames...
	setTempo(m_fTempo);
end;

procedure TStretcher.setParameterSequenceMS(iSequenceMs: word);
begin
	m_iSequenceMs := iSequenceMs;
	m_iSeekLength := (m_iSampleRate * m_iSeekWindowMs) div 1000;
	m_iSeekWindowLength := (m_iSampleRate * m_iSequenceMs) div 1000;

	calcOverlapLength;

	// Set tempo to recalculate required frames...
	setTempo(m_fTempo);
end;

procedure TStretcher.setParameterSeekWindowMS(iSeekWindowMs: word);
begin
	m_iSeekWindowMs := iSeekWindowMs;
	m_iSeekLength := (m_iSampleRate * m_iSeekWindowMs) div 1000;
	m_iSeekWindowLength := (m_iSampleRate * m_iSequenceMs) div 1000;

	calcOverlapLength;

	// Set tempo to recalculate required frames...
	setTempo(m_fTempo);
end;

procedure TStretcher.setParameterOverlapMS(iOverlapMs: word);
begin
	m_iOverlapMs := iOverlapMs;
	m_iSeekLength := (m_iSampleRate * m_iSeekWindowMs) div 1000;
	m_iSeekWindowLength := (m_iSampleRate * m_iSequenceMs) div 1000;

	calcOverlapLength;

	// Set tempo to recalculate required frames...
	setTempo(m_fTempo);
end;

// Get routine control parameters, see setParameters() function.
// Any of the parameters to this function can be NULL, in such case
// corresponding parameter value isn't returned.
procedure TStretcher.getParameters(piSampleRate, piSequenceMs, piSeekWindowMs, piOverlapMs:pword);
begin
	if Assigned(piSampleRate) then
		piSampleRate^ := m_iSampleRate;

	if Assigned(piSequenceMs) then
		piSequenceMs^ := m_iSequenceMs;

	if Assigned(piSeekWindowMs) then
		piSeekWindowMs^ := m_iSeekWindowMs;

	if Assigned(piOverlapMs) then
		piOverlapMs^ := m_iOverlapMs;
end;


// Clears mid sample frame buffer.
procedure TStretcher.clearMidBuffer;
var
	i: word;
begin 
	if (m_bMidBufferDirty) then
  begin
		for i := 0 to m_iChannels - 1 do
			FillChar(m_ppMidBuffer[i], 0, 2 * m_iOverlapLength * SizeOf(Single));
		m_bMidBufferDirty := false;
	end;
end;


// Clears input and mid sample frame buffers.
procedure TStretcher.clearInput;
begin 
	m_inputBuffer.clear;
	clearMidBuffer;
end;


// Clears all sample frame buffers.
procedure TStretcher.clear;
begin 
	m_outputBuffer.clear;
	m_inputBuffer.clear;

	clearMidBuffer;
end;



// Seeks for the optimal overlap-mixing position.
//
// The best position is determined as the position where
// the two overlapped sample sequences are 'most alike',
// in terms of the highest cross-correlation value over
// the overlapping period.
function TStretcher.seekBestOverlapPosition: Word;
var
   dBestCorr, dCorr: Double;
	iBestOffs, iPrevBestOffs: Word;
	i, iStep: Word;
	iOffs, j, k: integer;
begin 
	
	// Slopes the amplitude of the 'midBuffer' samples
	calcCrossCorrReference;

	dBestCorr := -1e50; // A reasonable lower limit.

	// Scans for the best correlation value by testing each
	// possible position over the permitted range.
	if (m_bQuickSeek) then  
	begin 
		// Hierachical search...
		iPrevBestOffs := (m_iSeekLength + 1) shr 1;
    iBestOffs := iPrevBestOffs;
    iOffs := iBestOffs;
    iStep := 64;
    while iStep > 0 do
		begin
      k := -1;
      while k <= 1 do
			begin
        j := 1;
        while ((j < 4) or (iStep = 64)) do
				begin
					iOffs := iPrevBestOffs + k * j * iStep;
					if (iOffs < 0) or (iOffs >= m_iSeekLength) then
						break;
					for i := 0 to m_iChannels - 1 do
					begin 
						// Calculates correlation value for the mixing
						// position corresponding to iOffs.
{						dCorr := std_cross_corr(
            	m_inputBuffer.ptrBegin(i) + iOffs,
							m_ppRefMidBuffer[i],
  						m_iOverlapLength);
						// Checks for the highest correlation value.
  					if dCorr > dBestCorr then
						begin 
							dBestCorr := dCorr;
							iBestOffs := iOffs;
						end;   }
					end;
          j := j + 1;
        end;
        k:= k + 2;
			end;
			iPrevBestOffs := iBestOffs;

      iStep := iStep shr 2
		end;
	end 
	else 
	begin 
		// Linear search...
		iBestOffs := 0;
		for iOffs := 0 to m_iSeekLength - 1 do
		begin 
			for i := 0 to m_iChannels - 1 do
			begin 
				// Calculates correlation value for the mixing
				// position corresponding to iOffs.
{				dCorr := std_cross_corr(
					m_inputBuffer.ptrBegin(i) + iOffs,
					m_ppRefMidBuffer[i], m_iOverlapLength);
				// Checks for the highest correlation value.}
				if dCorr > dBestCorr then  
				begin 
					dBestCorr := dCorr;
					iBestOffs := iOffs;
				end;
			end;
		end;
	end;

	result:= iBestOffs;
end;


// Processes as many processing frames of the samples
// from input-buffer, store the result into output-buffer.
procedure TStretcher.processFrames;
var
	i: word;
	j, k: word;
	pInput, pOutput: PSingle;
	iSkip, iOffset: word;
	iTemp: Integer;
begin 

	// If mid-buffer is empty, move the first
	// frames of the input stream into it...
	if (not m_bMidBufferDirty) then
  begin
		// Wait until we've got overlapLength samples
		if (m_inputBuffer.numSamples < m_iOverlapLength) then
			Exit;
//		m_inputBuffer.receiveSamples(m_ppMidBuffer, m_iOverlapLength);
		m_bMidBufferDirty := true;
  end;

	// Process frames as long as there are enough in
	// input-buffer to form a processing block.
	while (m_inputBuffer.numSamples >= m_iFramesReq) do
  begin
	
		// If tempo differs from the nominal,
		// scan for the best overlapping position...
		iOffset := seekBestOverlapPosition;

		// Mix the frames in the input-buffer at position of iOffset
		// with the samples in mid-buffer using sliding overlapping;
		// first partially overlap with the end of the previous
		// sequence (that's in the mid-buffer).
		m_outputBuffer.ensureCapacity(m_iOverlapLength);
		// Overlap...
		for i := 0 to m_iChannels - 1 do
    begin
//			pInput := m_inputBuffer.ptrBegin(i);
			pOutput := m_outputBuffer.ptrEnd(i);
      for j := 0 to m_iOverlapLength - 1 do
      begin
				k := m_iOverlapLength - j;
//				pOutput[j] = (pInput[j + iOffset] * j
//					+ m_ppMidBuffer[i][j] * k) div m_iOverlapLength;
			 end;
		end;
		// Commit...
		m_outputBuffer.putSamples(m_iOverlapLength);

		// Then copy sequence samples from input-buffer to output...
		iTemp := (m_iSeekWindowLength - 2 * m_iOverlapLength);
		if (iTemp > 0) then  begin 
			// Temporary mapping...
			for i := 0 to m_iChannels - 1 do
      begin
//				m_ppFrames[i] = m_inputBuffer.ptrBegin(i)
//					+ (iOffset + m_iOverlapLength);
			end;
//			m_outputBuffer.putSamples(m_ppFrames, iTemp);
		 end;

		// Copies the end of the current sequence from input-buffer to 
		// mid-buffer for being mixed with the beginning of the next 
		// processing sequence and so on
		// assert(iOffset + m_iSeekWindowLength <= m_inputBuffer.frames());
{		m_inputBuffer.receiveSamples(m_ppMidBuffer, m_iOverlapLength,
			(iOffset + m_iSeekWindowLength - m_iOverlapLength));       }
		m_bMidBufferDirty := true;

		// Remove the processed samples from the input-buffer. Update
		// the difference between integer & nominal skip step to skip-fract
		// in order to prevent the error from accumulating over time.
		m_fSkipFract:= m_fSkipFract + m_fNominalSkip; // real skip size
		iSkip := integer( m_fSkipFract );     // rounded to integer skip
		// Maintain the fraction part, i.e. real vs. integer skip
		m_fSkipFract:= m_fSkipFract - iSkip;
		m_inputBuffer.receiveSamples(iSkip);
	 end;
 end;


// Adds frames of samples into the input of the object.
procedure TStretcher.putFrames(ppFrames: array of Single; iFrames: Word);
begin 
	// Add the frames into the input buffer.
	m_inputBuffer.putSamples(ppFrames, iFrames);
	// Process the samples in input buffer.
	processFrames;
end;


// Output frames from beginning of the sample buffer.
// Copies requested frames output buffer and removes them
// from the sample buffer. If there are less than frames()
// samples in the buffer, returns all that available.
function TStretcher.receiveFrames(ppFrames: array of Single; iFrames: Word ): Word;
begin 
	result := m_outputBuffer.receiveSamples(ppFrames, iFrames);
end;


// Returns number of frames currently available.
function TStretcher.frames: Word;
begin 
	result := m_outputBuffer.numSamples;
end;


// Flush any last samples that are hiding in the internal processing pipeline.
procedure TStretcher.flushInput;
var
	i: word;
	dummy: array[0..256-1] of Single;
  iFrames: word;
begin 
	if m_bMidBufferDirty then
	begin 
		// Prepare a dummy empty buffer...
//		FillChar( and dummy[0], 0, SizeOf(dummy));
		
{		for i := 0 to m_iChannels - 1 do
			m_ppFrames[i] :=  and dummy[0];}
		// Push the last active frames out from the pipeline
		// by feeding blank samples into processing until
		// new samples appear in the output...
		iFrames := frames;
		for i := 0 to 128 - 1 do 
		begin 
//			putFrames(m_ppFrames, 256);
			// Any new samples appeared in the output?
			if frames > iFrames then
				break;
		end;
	end;

	clearInput;
end;


//---------------------------------------------------------------------------
// Floating point arithmetics specific algorithm implementations.
//

// Slopes the amplitude of the mid-buffer samples
// so that cross correlation is faster to calculate
procedure TStretcher.calcCrossCorrReference;
var
  i, j: word;
  fTemp : Single;
begin 
  for j := 0 to m_iOverlapLength - 1 do
	begin 
		fTemp := Single(j) * Single(m_iOverlapLength - j);
{		for i := 0 to m_iChannels = 1 do
			m_ppRefMidBuffer[i][j] := Single(m_ppMidBuffer[i][j] * fTemp);}
	 end;
end;


// Calculates overlap period length in frames and reallocate ref-mid-buffer.
procedure TStretcher.calcOverlapLength;
var
	i: word;
  iNewOverlapLength,
  iOldOverlapLength: word;
begin 
	// Must be divisible by 8...
//	iNewOverlapLength := (m_iSampleRate * m_iOverlapMs) / 1000;
	if iNewOverlapLength < 16 then 
		iNewOverlapLength := 16;
	iNewOverlapLength := iNewOverlapLength - (iNewOverlapLength mod 8);

  iOldOverlapLength := m_iOverlapLength;
	m_iOverlapLength := iNewOverlapLength;
	if m_iOverlapLength > iOldOverlapLength then
	begin 
//		if m_ppFrames then
		begin 
      for i := 0 to m_iChannels - 1 do
			begin
				freemem(m_ppMidBuffer);
				freemem(m_ppRefMidBufferUnaligned);
			end;
			freemem(m_ppMidBuffer);
			freemem(m_ppRefMidBufferUnaligned);
			freemem(m_ppRefMidBuffer);
			freemem(m_ppFrames);
		end;
		GetMem(m_ppFrames, SizeOf(Single) * m_iChannels);
		GetMem(m_ppMidBuffer, SizeOf(Single) * m_iChannels);
		GetMem(m_ppRefMidBufferUnaligned, SizeOf(Single) * m_iChannels);
		GetMem(m_ppRefMidBuffer, SizeOf(Single) * m_iChannels);
    for i := 0 to m_iChannels - 1 do
		begin
			//new float [2 * m_iOverlapLength];
			// := GetMem(m_ppMidBuffer[i], SizeOf(Single) [2 * m_iOverlapLength];
			{m_ppRefMidBufferUnaligned[i]
				:= new function[2 * m_iOverlapLength + 16 / SizeOfv1: Singlee): Single;;   }
			// Ensure that ref-mid-buffer is aligned
			// to 16 byte boundary for efficiency
			//m_ppRefMidBuffer[i] = (Single )
			//	((((Cardinal) m_ppRefMidBufferUnaligned[i]) + 15) and -16);
		end;
		m_bMidBufferDirty := true;
		clearMidBuffer;
	 end;
 end;

// end of qtractorTimeStretch.cpp

end.

