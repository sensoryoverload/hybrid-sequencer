// Object Pascal Port...

unit SincFIR;

(* Windowed Sinc FIR Generator
   Bob Maling (BobM.DSP@gmail.com)
   Contributed to musicdsp.org Source Code Archive
   Last Updated: April 12, 2005
   Translated to Object Pascal by Christian-W. Budde

   Usage:
   Lowpass:wsfirLP(H, WindowType, CutOff)
   Highpass:wsfirHP(H, WindowType, CutOff)
   Bandpass:wsfirBP(H, WindowType, LowCutOff, HighCutOff)
   Bandstop:wsfirBS(H, WindowType, LowCutOff, HighCutOff)

   where:
   H (TDoubleArray): empty filter coefficient table (SetLength(H,DesiredLength)!)
   WindowType (TWindowType): wtBlackman, wtHanning, wtHamming
   CutOff (double): cutoff (0 < CutOff < 0.5, CutOff = f/fs)
   --> for fs=48kHz and cutoff f=12kHz, CutOff = 12k/48k => 0.25

   LowCutOff (double):low cutoff (0 < CutOff < 0.5, CutOff = f/fs)
   HighCutOff (double):high cutoff (0 < CutOff < 0.5, CutOff = f/fs)

   Windows included here are Blackman, Blackman-Harris, Gaussian, Hanning
   and Hamming.*)

interface

uses Math;

type TDoubleArray = array of Double;
     TWindowType = (wtBlackman, wtHanning, wtHamming, wtBlackmanHarris,
                    wtGaussian); // Window type contstants

// Function prototypes
procedure wsfirLP(var H : TDoubleArray; const WindowType : TWindowType; const CutOff : Double);
procedure wsfirHP(var H : TDoubleArray; const WindowType : TWindowType; const CutOff : Double);
procedure wsfirBS(var H : TDoubleArray; const WindowType : TWindowType; const LowCutOff, HighCutOff : Double);
procedure wsfirBP(var H : TDoubleArray; const WindowType : TWindowType; const LowCutOff, HighCutOff : Double);
procedure genSinc(var Sinc : TDoubleArray; const CutOff : Double);
procedure wGaussian(var W : TDoubleArray);
procedure wBlackmanHarris(var W : TDoubleArray);
procedure wBlackman(var W : TDoubleArray);
procedure wHanning(var W : TDoubleArray);
procedure wHamming(var W : TDoubleArray);

implementation

// Generate lowpass filter
// This is done by generating a sinc function and then windowing it
procedure wsfirLP(var H : TDoubleArray; const WindowType : TWindowType; const CutOff : Double);
begin
genSinc(H, CutOff);     // 1. Generate Sinc function
case WindowType of  // 2. Generate Window function -> lowpass filter!
  wtBlackman: wBlackman(H);
  wtHanning: wHanning(H);
  wtHamming: wHamming(H);
  wtGaussian: wGaussian(H);
  wtBlackmanHarris: wBlackmanHarris(H);
end;
end;

// Generate highpass filter
// This is done by generating a lowpass filter and then spectrally inverting it
procedure wsfirHP(var H : TDoubleArray; const WindowType : TWindowType; const CutOff : Double);
var
  i : Integer;
begin
  wsfirLP(H, WindowType, CutOff); // 1. Generate lowpass filter

  // 2. Spectrally invert (negate all samples and add 1 to center sample) lowpass filter
  // = delta[n-((N-1)/2)] - h[n], in the time domain
  for i:=0 to Length(H)-1 do
    H[i] := H[i] * -1.0;

  H[(Length(H)-1) div 2] := H[(Length(H)-1) div 2] + 1.0;
end;

// Generate bandstop filter
// This is done by generating a lowpass and highpass filter and adding them
procedure wsfirBS(var H : TDoubleArray; const WindowType : TWindowType; const LowCutOff, HighCutOff : Double);
var
  i: Integer;
  H2: TDoubleArray;
begin
  SetLength(H2,Length(H));

  // 1. Generate lowpass filter at first (low) cutoff frequency
  wsfirLP(H, WindowType, LowCutOff);

  // 2. Generate highpass filter at second (high) cutoff frequency
  wsfirHP(H2, WindowType, HighCutOff);

  // 3. Add the 2 filters together
  for i:=0 to Length(H)-1
    do H[i]:=H[i]+H2[i];

  SetLength(H2,0);
end;

// Generate bandpass filter
// This is done by generating a bandstop filter and spectrally inverting it
procedure wsfirBP(var H : TDoubleArray; const WindowType : TWindowType; const LowCutOff, HighCutOff : Double);
var i : Integer;
begin
  wsfirBS(H, WindowType, LowCutOff, HighCutOff); // 1. Generate bandstop filter

  // 2. Spectrally invert (negate all samples and add 1 to center sample) lowpass filter
  // = delta[n-((N-1)/2)] - h[n], in the time domain
  for i:=0 to Length(H)-1
    do H[i]:=H[i]*-1.0;
  H[(Length(H)-1) div 2]:=H[(Length(H)-1) div 2]+1.0;
end;

// Generate sinc function - used for making lowpass filter
procedure genSinc(var Sinc : TDoubleArray; const Cutoff : Double);
var i,j  : Integer;
    n,k  : Double;
begin
j:=Length(Sinc)-1;
k:=1/j;
// Generate sinc delayed by (N-1)/2
  for i:=0 to j do
    if (i=j div 2)
     then Sinc[i]:=2.0*Cutoff
     else
      begin
       n:=i-j/2.0;
       Sinc[i]:=sin(2.0*PI*Cutoff*n)/(PI*n);
      end;
end;

// Generate window function (Gaussian)
procedure wGaussian(var W : TDoubleArray);
var i,j : Integer;
    k   : Double;
begin
  j:=Length(W)-1;
  k:=1/j;
  for i:=0 to j
    do W[i]:=W[i]*(exp(-5.0/(sqr(j))*(2*i-j)*(2*i-j)));
end;

// Generate window function (Blackman-Harris)
procedure wBlackmanHarris(var W : TDoubleArray);
var i,j : Integer;
    k   : Double;
begin
  j:=Length(W)-1;
  k:=1/j;
  for i:=0 to j
    do W[i]:=W[i]*(0.35875-0.48829*cos(2*PI*(i+0.5)*k)+0.14128*cos(4*PI*(i+0.5)*k)-0.01168*cos(6*PI*(i+0.5)*k));
end;

// Generate window function (Blackman)
procedure wBlackman(var W : TDoubleArray);
var i,j : Integer;
    k   : Double;
begin
  j:=Length(W)-1;
  k:=1/j;
  for i:=0 to j
    do W[i]:=W[i]*(0.42-(0.5*cos(2*PI*i*k))+(0.08*cos(4*PI*i*k)));
end;

// Generate window function (Hanning)
procedure wHanning(var W : TDoubleArray);
var i,j : Integer;
    k   : Double;
begin
  j:=Length(W)-1;
  k:=1/j;
  for i:=0 to j
    do W[i]:=W[i]*(0.5*(1.0-cos(2*PI*i*k)));
end;

// Generate window function (Hamming)
procedure wHamming(var W : TDoubleArray);
var i,j : Integer;
    k   : Double;
begin
  j:=Length(W)-1;
  k:=1/j;
  for i:=0 to j
    do W[i]:=W[i]*(0.54-(0.46*cos(2*PI*i*k)));
end;

end.
