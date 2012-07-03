unit diode_ladder_filter;

// This code is released under the MIT license (see below).
//
// The MIT License
// 
// Copyright (c) 2012 Dominique Wurtz (www.blaukraut.info)
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.


// Emulation of Diode ladder lowpass filter as found in Roland TB303 or EMS VCS3
// Version 0.1 (04/03/2012)

interface

uses
  math;

const
  M_PI = 3.14159265358979323846;

type

  { TDiodeLadderFilter }

  TDiodeLadderFilter = class
  private
    FK, FA: double;
    z: array[0..4] of double; // filter memory (4 integrators plus 1st order HPF)
    ah, bh: double; // feedback HPF coeffs
    FFc, FQ: double;

    function clip(const x: double): double;
  public

    constructor Create;
    procedure Reset;

    // q: resonance in the range [0..1]
    procedure set_q(const q: double);

    // fc: frequency in the range [0..1]
    procedure set_frequency(const fc: double);

    // fc: normalized cutoff frequency in the range [0..1] => 0 HZ .. Nyquist
    procedure set_feedback_hpf_cutoff(const fc: double);

    function Process(const x: double): double;
  end;

implementation

constructor TDiodeLadderFilter.Create;
begin
  set_feedback_hpf_cutoff(0);
	z[0] := 0;
	z[1] := 0;
	z[2] := 0;
	z[3] := 0;
	z[4] := 0;
	set_q(0);
end;

procedure TDiodeLadderFilter.Reset;
begin
  if FK < 17 then
  begin
    set_feedback_hpf_cutoff(0);
	  z[0] := 0;
	  z[1] := 0;
	  z[2] := 0;
	  z[3] := 0;
	  z[4] := 0;
	  set_q(0);
  end;
end;

procedure TDiodeLadderFilter.set_feedback_hpf_cutoff(const fc: double);
var
  lK: double;
begin
	lK := fc * M_PI;
	ah := (lK - 2) / (lK + 2);
	bh := 2 / (lK + 2);
end;

// q: resonance in the range [0..1]
procedure TDiodeLadderFilter.set_q(const q: double);
begin
  if FQ = q then Exit;

  FQ := q;
  FK := 20 * q;
	FA := 1 + 0.5*FK; // resonance gain compensation
end;

procedure TDiodeLadderFilter.set_frequency(const fc: double);
begin
  if fc = FFc then exit;

  FFc := fc;


end;

// x: input signal
// fc: normalized cutoff frequency in the range [0..1] => 0 HZ .. Nyquist
function TDiodeLadderFilter.Process(const x: double): double;
var
  g, s, y5, y4, y3, y2, y1, y0: double;
  a, ainv, a2, b, b2, c, g0, s0: double;
begin
  a := M_PI * (FFc * 0.5); // PI is Nyquist frequency
  a := 2 * tan(0.5*a); // dewarping, not required with 2x oversampling
  ainv := 1/a;
  a2 := a*a;
  b := 2*a + 1;
  b2 := b*b;
  c := 1 / (2*a2*a2 - 4*a2*b2 + b2*b2);
  g0 := 2*a2*a2*c;
  g := g0 * bh;

  // current state
  s0 := (a2*a*z[0] + a2*b*z[1] + z[2]*(b2 - 2*a2)*a + z[3]*(b2 - 3*a2)*b) * c;
  s := bh*s0 - z[4];

  // solve feedback loop (linear)
  y5 := (g*x + s) / (1 + g*FK);

  // input clipping
  y0 := clip(x - FK*y5);
  y5 := g*y0 + s;

  // compute integrator outputs
  y4 := g0*y0 + s0;
  y3 := (b*y4 - z[3]) * ainv;
  y2 := (b*y3 - a*y4 - z[2]) * ainv;
  y1 := (b*y2 - a*y3 - z[1]) * ainv;

  // update filter state
  z[0] += 4*a*(y0 - y1 + y2);
  z[1] += 2*a*(y1 - 2*y2 + y3);
  z[2] += 2*a*(y2 - 2*y3 + y4);
  z[3] += 2*a*(y3 - 2*y4);
  z[4] := bh*y4 + ah*y5;

  Result := FA*y4;
end;

function TDiodeLadderFilter.clip(const x: double): double;
begin
	Result := x / (1 + abs(x));
end;

end.