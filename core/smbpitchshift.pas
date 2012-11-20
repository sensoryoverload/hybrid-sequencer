//*************************************************************************//
//                                                                         //
// NAME: smbPitchShift.pas                                                 //
// VERSION: 1.2                                                            //
// HOME URL: http://www.dspdimension.com                                   //
// KNOWN BUGS: none                                                        //
//                                                                         //
// SYNOPSIS: Routine for doing pitch shifting while maintaining            //
// duration using the Short Time Fourier Transform.                        //
//                                                                         //
// DESCRIPTION: The routine takes a pitchShift factor value which is       //
// between 0.5 (one octave down) and 2. (one octave up). A value of        //
// exactly 1 does not change the pitch. AFrames tells the        //
// routine how many samples in indata[0..AFrames-1] should       //
// be pitch shifted and moved to outdata[0..AFrames-1].          //
// The two buffers can be identical (ie. it can process the data           //
// in-place). fftFrameSize defines the FFT frame size used for the         //
// processing. Typical values are 1024, 2048 and 4096. It may be           //
// any value <= MAX_FRAME_LENGTH but it MUST be a power of 2. osamp        //
// is the STFT oversampling factor which also determines the overlap       //
// between adjacent STFT frames. It should at least be 4 for moderate      //
// scaling ratios. A value of 32 is recommended for best quality.          //
// sampleRate takes the sample rate for the signal in unit Hz, ie. 44100   //
// for 44.1 kHz audio. The data passed to the routine in indata[] should   //
// be in the range [-1.0, 1.0), which is also the output range for the     //
// data, make sure you scale the data accordingly (for 16bit signed        //
// integers you would have to divide (and multiply) by 32768).             //
//                                                                         //
// COPYRIGHT 1999-2009 Stephan M. Bernsee <smb@dspdimension.com>           //
//                                                                         //
//             The Wide Open License (WOL)                                 //
//                                                                         //
// Permission to use, copy, modify, distribute and sell this software      //
// and its documentation for any purpose is hereby granted without fee,    //
// provided that the above copyright notice and this license appear in     //
// all source copies.                                                      //
// THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY   //
// OF ANY KIND. See http://www.dspguru.com/wol.htm for more information.   //
//                                                                         //
//*************************************************************************//

unit smbPitchShift;

{$asmmode intel}

interface

uses
  Math;

const
  M_PI = 3.14159265358979323846;
  MAX_FRAME_LENGTH = 8192;

type

  { TSmbPitchShifter }

  TSmbPitchShifter = class
  private
    FInFIFO: array[0..MAX_FRAME_LENGTH - 1] of Single;
    FOutFIFO: array[0..MAX_FRAME_LENGTH - 1] of Single;
    FFFTworksp: array[0..MAX_FRAME_LENGTH - 1] of Single;

    FLastPhase: array[0..MAX_FRAME_LENGTH div 2] of Single;
    FOverSampling: Longint;
    FPitch: Single;
    FSampleRate: Single;
    FSumPhase: array[0..MAX_FRAME_LENGTH div 2] of Single;
    FOutputAccum: array[0..2 * MAX_FRAME_LENGTH - 1] of Single;
    FAnaFreq: array[0..MAX_FRAME_LENGTH - 1] of Single;
    FAnaMagn: array[0..MAX_FRAME_LENGTH - 1] of Single;
    FSynFreq: array[0..MAX_FRAME_LENGTH - 1] of Single;
    FSynMagn: array[0..MAX_FRAME_LENGTH - 1] of Single;
    FHanningWindow: array[0..MAX_FRAME_LENGTH - 1] of Single;
    FRover: Longint;

    FFFTFrameSize: Longint;
    procedure smbFFT(fftBuffer: PSingle; fftFrameSize, sign: Longint);
    procedure SetFFTFrameSize(AValue: Longint);
    procedure SetOverSampling(AValue: Longint);
    procedure SetPitch(AValue: Single);
    procedure SetSampleRate(AValue: Single);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize;
    procedure Process(indata, outdata: PSingle; AFrames: Longint);
    property Pitch: Single read FPitch write SetPitch;
    property FFTFrameSize: Longint read FFFTFrameSize write SetFFTFrameSize;
    property OverSampling: Longint read FOverSampling write SetOverSampling;
    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

implementation

function squareroot_sse_11bits(x: single): single; forward; inline;
function arctan2x(y, x: double): double; forward; inline;

// Routine smbPitchShift(). See top of file for explanation
// Purpose: doing pitch shifting while maintaining duration using the Short
// Time Fourier Transform.
// Author: (c)1999-2009 Stephan M. Bernsee <smb@dspdimension.com>
procedure TSmbPitchShifter.Process(indata, outdata: PSingle; AFrames: Longint);
var
  magn, phase, tmp, window, real, imag: double;
  freqPerBin, freqPerBin_inverse, expct: double;
  oversampling_inverse: double;
  fftFrameSizeMulOverSampling: double;
  lCos, lSin: Extended;
  i,k, qpd, index, inFifoLatency, stepSize, fftFrameSize2: Longint;
begin
  { set up some handy variables }
  fftFrameSize2 := FFFTFrameSize div 2;
  stepSize := FFFTFrameSize div FOverSampling;
  freqPerBin := FSampleRate / FFFTFrameSize;
  freqPerBin_inverse := 1 / freqPerBin;
  oversampling_inverse := 1 / FOverSampling;
  fftFrameSizeMulOverSampling := 1 / (fftFrameSize2 * FOverSampling);
  expct := 2.0 * Pi * stepSize / FFFTFrameSize;
  inFifoLatency := FFFTFrameSize - stepSize;
  if (FRover = 0) then
    FRover := inFifoLatency;

  { main processing loop }
  for i := 0 to AFrames - 1 do
  begin

    { As long as we have not yet collected enough data just read in }
    FInFIFO[FRover] := indata[i];
    outdata[i] := FOutFIFO[FRover - inFifoLatency];
    Inc(FRover);

    { now we have enough data for processing }
    if FRover >= ffftFrameSize then
    begin
      FRover := inFifoLatency;

      { do windowing and re,im interleave }
      for k := 0 to fftFrameSize -1 do
      begin
        window := -0.5 * cos(2.0 * Pi * k/fftFrameSize) + 0.5;
        FFFTworksp[2 * k] := FInFIFO[k] * window;
        FFFTworksp[2 * k + 1] := 0.0;
      end;

      { ***************** ANALYSIS ******************* }
      { do transform }
      smbFft(@FFFTworksp[0], fftFrameSize, -1);

      { this is the analysis step }
      for k := 0 to fftFrameSize2 - 1 do
      begin

        { de-interlace FFT buffer }
        real := FFFTworksp[2 * k];
        imag := FFFTworksp[2 * k + 1];

        { compute magnitude and phase }
        magn := 2.0 * squareroot_sse_11bits(real * real + imag * imag);
        phase := ArcTan2x(imag, real);

        { compute phase difference }
        tmp := phase - FLastPhase[k];
        FLastPhase[k] := phase;

        { subtract expected phase difference }
        tmp := tmp - k * expct;

        { map delta phase into +/- Pi interval }
        qpd := Trunc(tmp / Pi);
        if qpd >= 0 then
          Inc(qpd, qpd and 1)
        else
          Dec(qpd, qpd and 1);
        tmp := tmp - Pi * qpd;

        { get deviation from bin frequency from the +/- Pi interval }
        tmp := FOverSampling * tmp / (2.0 * Pi);

        { compute the k-th partials' true frequency }
        tmp := k * freqPerBin + tmp * freqPerBin;

        { store magnitude and true frequency in analysis arrays }
        FAnaMagn[k] := magn;
        FAnaFreq[k] := tmp;
      end;

      { ***************** PROCESSING ******************* }
      { this does the actual pitch shifting }
      FillChar(FSynMagn, SizeOf(FSynMagn), 0);
      FillChar(FSynFreq, SizeOf(FSynFreq), 0);
      for k := 0 to fftFrameSize2 - 1 do
      begin
        index := Trunc(k * FPitch);
        if index <= fftFrameSize2 then
        begin
          FSynMagn[index] := FSynMagn[index] + FAnaMagn[k];
          FSynFreq[index] := FAnaFreq[k] * FPitch;
        end
      end;

      { ***************** SYNTHESIS ******************* }
      { this is the synthesis step }
      for k := 0 to fftFrameSize2 - 1 do
      begin

        { get magnitude and true frequency from synthesis arrays }
        magn := FSynMagn[k];
        tmp := FSynFreq[k];

        { subtract bin mid frequency }
        tmp := tmp - k * freqPerBin;

        { get bin deviation from freq deviation }
        tmp := tmp * freqPerBin_inverse;

        { take AOverSampling into account }
        tmp := 2.0 * Pi * tmp * oversampling_inverse;

        { add the overlap phase advance back in }
        tmp := tmp + k * expct;

        { accumulate delta phase to get bin phase }
        FSumPhase[k] := FSumPhase[k] + tmp;
        phase := FSumPhase[k];

        { get real and imag part and re-interleave }
        FFFTworksp[2 * k] := magn * Cos(phase);
        FFFTworksp[2 * k + 1] := magn * Sin(phase);
      end;

      { zero negative frequencies }
      for k := fftFrameSize+2 to 2 * fftFrameSize - 1 do
        FFFTworksp[k] := 0.0;


      { do inverse transform }
      smbFFT(@FFFTworksp[0], fftFrameSize, 1);

      { do windowing and add to output accumulator }
      for k :=0 to fftFrameSize - 1 do
      begin
        window := -0.5 * Cos(2.0 * Pi * k/fftFrameSize) + 0.5;
        FOutputAccum[k] := FOutputAccum[k] +
                           2.0 * window * FFFTworksp[2 * k] *
                           fftFrameSizeMulOverSampling;
      end;
      for k := 0 to stepSize - 1 do
        FOutFIFO[k] := FOutputAccum[k];

      { shift accumulator }
      Move(FOutputAccum[stepSize], FOutputAccum[0],
        ffftFrameSize * sizeof(Single));

      { move input FIFO }
      for k := 0 to inFifoLatency - 1 do
        FInFIFO[k] := FInFIFO[k + stepSize];
    end;
  end;
end;

function arctan2x(y, x: double): double;
const
  M_PI_COEF1 = M_PI / 4;
  M_PI_COEF2 = 3 * M_PI_COEF1;
var
  abs_y: double;
  r: double;
  angle: double;
begin
   abs_y := abs(y) + 1e-10 ;     // kludge to prevent 0/0 condition
   if x >= 0 then
   begin
      r := (x - abs_y) / (x + abs_y);
      angle := M_PI_COEF1 - M_PI_COEF1 * r;
   end
   else
   begin
      r := (x + abs_y) / (abs_y - x);
      angle := M_PI_COEF2 - M_PI_COEF1 * r;
   end;
   if y < 0 then
    result := -angle     // negate if in quad III or IV
   else
    result := angle;
end;

function squareroot_sse_11bits(x: single): single;
var
  z: single;
begin
  asm
    rsqrtss xmm0, x
    rcpss xmm0, xmm0
    movss z, xmm0            // z ~= sqrt(x) to 0.038%
  end;
  Result := z;
end;

function fastroot(i:single;n:integer):single;
var
  l: longint;
begin
  l := longint((@i)^);
  l := l - $3F800000;
  l := l shr (n-1);
  l := l + $3F800000;
  result := single((@l)^);
end;

{ TSmbPitchShifter }

constructor TSmbPitchShifter.Create;
begin
  //
end;

procedure TSmbPitchShifter.Initialize;
var
  i: Integer;
begin
  // Set to default if not set
  if FFFTFrameSize = 0 then
    FFFTFrameSize := 1024;

  if FOverSampling = 0 then
    FOverSampling := 4;

  if FSampleRate = 0 then
    FSampleRate := 44100;

  if FPitch = 0 then
    FPitch := 1;

  { initialize our static arrays }
  FillChar(FInFIFO, SizeOf(FInFIFO), 0);
  FillChar(FOutFIFO, SizeOf(FOutFIFO), 0);
  FillChar(FFFTworksp, SizeOf(FFFTworksp), 0);
  FillChar(FLastPhase, SizeOf(FLastPhase), 0);
  FillChar(FSumPhase, SizeOf(FSumPhase), 0);
  FillChar(FOutputAccum, SizeOf(FOutputAccum), 0);
  FillChar(FAnaFreq, SizeOf(FAnaFreq), 0);
  FillChar(FAnaMagn, SizeOf(FAnaMagn), 0);

  FRover := 0;

  for i := 0 to Pred(FFFTFrameSize) do
  begin
    FHanningWindow[i] := -0.5*cos(2.0*M_PI*i/FFFTFrameSize)+0.5;
  end;

end;

procedure TSmbPitchShifter.smbFFT(fftBuffer: PSingle; fftFrameSize, sign: Longint);

// FFT routine, (C)1996 S.M.Bernsee. Sign = -1 is FFT, 1 is iFFT (inverse)
// Fills fftBuffer[0..2*fftFrameSize-1] with the Fourier transform of the
// time domain data in fftBuffer[0..2*fftFrameSize-1]. The FFT array takes
// and returns the cosine and sine parts in an interleaved manner, ie.
// fftBuffer[0] = cosPart[0], fftBuffer[1] = sinPart[0], asf. fftFrameSize
// must be a power of 2. It expects a complex input signal (see footnote 2),
// ie. when working with 'common' audio signals our input signal has to be
// passed as beginin[0],0.,in[1],0.,in[2],0.,...end asf. In that case, the
// transform of the frequencies of interest is in fftBuffer[0...fftFrameSize].

var
  wr, wi, arg, temp: Single;
  p1, p2: PSingle;
  tr, ti, ur, ui: Single;
  p1r, p1i, p2r, p2i: PSingle;
  i, bitm, j, le, le2, k: Longint;
begin
  i := 2;
  while i < 2 * fftFrameSize - 2 do
  begin
    bitm := 2;
    j := 0;
    while bitm < 2 * fftFrameSize do
    begin
      if (i and bitm) <> 0 then
        Inc(j);
      j := j shl 1;
      bitm := bitm shl 1;
    end;
    if i < j then
    begin
      p1 := fftBuffer + i;
      p2 := fftBuffer + j;
      temp := p1^;
      p1^ := p2^;
      p2^ := temp;
      Inc(p1);
      Inc(p2);
      temp := p1^;
      p1^ := p2^;
      p2^ := temp;
    end;
    Inc(i, 2);
  end;

  le := 2;
  for k := 0 to Trunc(Ln(fftFrameSize)/Ln(2.0) + 0.5) - 1 do
  begin
    le := le shl 1;
    le2 := le shr 1;
    ur := 1.0;
    ui := 0.0;
    arg := Pi / (le2 shr 1);
    wr := Cos(arg);
    wi := sign * Sin(arg);
    j := 0;
    while j < le2 do
    begin
      p1r := fftBuffer + j;
      p1i := p1r + 1;
      p2r := p1r + le2;
      p2i := p2r + 1;
      i := j;
      while i < 2 * fftFrameSize do
      begin
        tr := p2r^ * ur - p2i^ * ui;
        ti := p2r^ * ui + p2i^ * ur;
        p2r^ := p1r^ - tr;
        p2i^ := p1i^ - ti;
        p1r^ := p1r^ + tr;
        p1i^ := p1i^ + ti;
        Inc(p1r, le);
        Inc(p1i, le);
        Inc(p2r, le);
        Inc(p2i, le);
        Inc(i, le);
      end;
      tr := ur * wr - ui * wi;
      ui := ur * wi + ui * wr;
      ur := tr;
      Inc(j, 2);
    end
  end
end;

// 12/12/02, smb
//
// PLEASE NOTE:
//
// There have been some reports on domain errors when the atan2() function
// was used as in the above code. Usually, a domain error should not
// interrupt the program flow (maybe except in Debug mode) but rather be
// handled "silently" and a global variable should be set according to
// this error. However, on some occasions people ran into this kind of
// scenario, so a replacement atan2() function is provided here.
//
// If you are experiencing domain errors and your program stops, simply
// replace all instances of atan2() with calls to the smbAtan2()
// function below.

function smbAtan2(x, y: Double): Double;
var
  signx: Double;
begin
  if x > 0.0 then
    signx := 1.0
  else
    signx := -1.0;

  if x = 0.0 then
    Result := 0.0
  else if y = 0.0 then
    Result := signx * Pi / 2.0
  else
    Result := ArcTan2(x, y);
end;

procedure TSmbPitchShifter.SetFFTFrameSize(AValue: Longint);
begin
  if FFFTFrameSize = AValue then Exit;
  FFFTFrameSize := AValue;
end;

procedure TSmbPitchShifter.SetOverSampling(AValue: Longint);
begin
  if FOverSampling = AValue then Exit;
  FOverSampling := AValue;
end;

procedure TSmbPitchShifter.SetPitch(AValue: Single);
begin
  if FPitch = AValue then Exit;
  FPitch := AValue;
end;

procedure TSmbPitchShifter.SetSampleRate(AValue: Single);
begin
  if FSampleRate = AValue then Exit;
  FSampleRate := AValue;
end;

destructor TSmbPitchShifter.Destroy;
begin
  //

  inherited Destroy;
end;

end.

