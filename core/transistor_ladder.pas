unit transistor_ladder;

//// LICENSE TERMS: Copyright 2012 Teemu Voipio
//
// You can use this however you like for pretty much any purpose,
// as long as you don't claim you wrote it. There is no warranty.
//
// Distribution of substantial portions of this code in source form
// must include this copyright notice and list of conditions.
//
// Adapted to the Pascal language by Robbert Latumahina

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

const
  M_PI = 3.1415926535897932384;

type

  { TTransistorLadder }

  TTransistorLadder = class
  private
    // input delay and state for member variables
    z1: double;
    cutoff: double;
    resonance: double;
    s: array[0..3] of double;
  public
    constructor Create;
    procedure Reset;

    // q: resonance in the range [0..1]
    procedure set_q(const q: double);

    // fc: frequency in the range [0..1]
    procedure set_frequency(const fc: double);

    function Process(const x: double): double;
  end;

implementation

// tanh(x)/x approximation, flatline at very high inputs
// so might not be safe for very large feedback gains
// [limit is 1/15 so very large means ~15 or +23dB]
function tanhXdX(x: double): double;
var
  a: double;
begin
  a := x * x;

  // IIRC I got this as Pade-approx for tanh(sqrt(x))/sqrt(x)
  Result := ((a + 105)*a + 945) / ((15*a + 420)*a + 945);
end;

constructor TTransistorLadder.Create;
begin
  Reset;
end;

procedure TTransistorLadder.Reset;
begin
  s[0] := 0;
  s[1] := 0;
  s[2] := 0;
  s[3] := 0;
end;

procedure TTransistorLadder.set_q(const q: double);
begin
  resonance := q;
end;

procedure TTransistorLadder.set_frequency(const fc: double);
begin
  cutoff := fc * 0.5;
end;

// cutoff as normalized frequency (eg 0.5 = Nyquist)
// resonance from 0 to 1, self-oscillates at settings over 0.9
function TTransistorLadder.process(const x: double): double;
var
  ih: double;
  zi: double;
  f: double;
  r: double;
  t0, t1, t2, t3, t4: double;
  g0, g1, g2, g3: double;
  f3, f2, f1, f0: double;
  y3: double;
  xx: double;
  y0, y1, y2: double;
begin
  // tuning and feedback
  f := tan(M_PI * cutoff);
  r := (40.0/9.0) * resonance;

  // input with half delay, for non-linearities
  ih := 0.5 * (x + zi);
  zi := x;

  // evaluate the non-linear gains
  t0 := tanhXdX(ih - r * s[3]);
  t1 := tanhXdX(s[0]);
  t2 := tanhXdX(s[1]);
  t3 := tanhXdX(s[2]);
  t4 := tanhXdX(s[3]);

  // g# the denominators for solutions of individual stages
  g0 := 1 / (1 + f*t1);
  g1 := 1 / (1 + f*t2);
  g2 := 1 / (1 + f*t3);
  g3 := 1 / (1 + f*t4);

  // f# are just factored out of the feedback solution
  f3 := f*t3*g3;
  f2 := f*t2*g2*f3;
  f1 := f*t1*g1*f2;
  f0 := f*t0*g0*f1;

  // solve feedback
  y3 := (g3*s[3] + f3*g2*s[2] + f2*g1*s[1] + f1*g0*s[0] + f0*x) / (1 + r*f0);

  // then solve the remaining outputs (with the non-linear gains here)
  xx := t0*(x - r*y3);
  y0 := t1*g0*(s[0] + f*xx);
  y1 := t2*g1*(s[1] + f*y0);
  y2 := t3*g2*(s[2] + f*y1);

  // update state
  s[0] += 2*f * (xx - y0);
  s[1] += 2*f * (y0 - y1);
  s[2] += 2*f * (y1 - y2);
  s[3] += 2*f * (y2 - t4*y3);

  Result := y3;
end;

end.

