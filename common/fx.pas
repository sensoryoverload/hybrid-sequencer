{
  Copyright (C) 2009 Robbert Latumahina

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  fx.pas
}

unit fx;

{
lTrack.FXFilter.Resonance:= Random(500) / 550;
lTrack.FXFilter.Frequency:= Random(22050);
}

{$mode objfpc}{$H+}
{$asmmode intel}
interface

uses
  Classes, SysUtils, plugin, midi;
  
type

  { TDummyFilter }

  TDummyFilter = class(TInternalPlugin)
  public
    constructor Create(AObjectOwnerID: string);
    procedure Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer); override;
  end;

  { TDecimateFX }

  // Bit/Samplerate -reducer
  // bits: 1..32
  // rate: 0..1 (1 is original samplerate)
  TDecimateFX = class(TInternalPlugin)
  private
    m: longint;
    y,
    cnt,
    rate: single;
  public
    constructor Create(AObjectOwnerID: string);
    function Decimate(i: single): single;
    procedure Init(bits: integer; shrate: single);
    procedure Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer); override;
  end;

  { TMoogFilter }

  TMoogFilter = class(TInternalPlugin)
  private
    fA   : array[1..5] of single;
    fOld : single;
    fQ   : single;
    f2vg : single;
    fAcr : single;
    fF   : single;
    fFs  : single;
    procedure SetFrequency(v:single);
    procedure SetQ(v:single);
    procedure SetFS(v:single);
    procedure FreqCalc;
  public
    constructor Create(AObjectOwnerID: string);
    function Process(const I : Single):Single;
    procedure Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer); override;
    property Frequency: single read fF write SetFrequency;
    property SampleRate: single read fFS write SetFS;
    property Resonance: single read fQ write SetQ;
  end;
  
  TLFOType = (lfoTriangle, lfoSine);

  TLFO = class(TInternalPlugin)
  private
    iSpeed     : Integer;
    fSpeed     : Single;
    fMax, fMin : Single;
    fValue     : Single;
    fPos       : Integer;
    fType      : TLFOType;
    fScale     : Single;
    fPosMul    : Single;
    fHalfScale : Single;
    function GetValue:Single;
    procedure SetType(tt: TLFOType);
    procedure SetMin(v:Single);
    procedure SetMax(v:Single);
    procedure SetSpeed(v:Single);
  public
    { Public declarations }
    constructor Create(AObjectOwnerID: string);
  published
    property Value:Single read GetValue;
    property Speed:Single read FSpeed Write SetSpeed;
    property Min:Single read FMin write SetMin;
    property Max:Single read FMax Write SetMax;
    property LFO:TLFOType read fType Write SetType;
  end;

  function f_Abs(f:Single):Single; assembler;
  function Tanh2_pas1(x:Single):Single;
  function Tanh2_pas2(x:Single):Single;
  function Tanh2(x:Single):Single;  assembler;

  
implementation

{ TDecimateFX }

function TDecimateFX.Decimate(i: single): single;
begin
  cnt:= cnt + rate;
  if cnt > 1 then
  begin
    cnt:= cnt - 1;
    y:= round(i * m) / m;
  end;
  result:= y;
end;

procedure TDecimateFX.Init(bits: integer; shrate: single);
begin
  m:= 1 shl (bits - 1);
  cnt:= 1;
  rate:= shrate;
end;

procedure TDecimateFX.Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer);
var
  i: Integer;
begin
  for i := 0 to Pred(AFrames) do
  begin
    ABuffer[i] := Decimate(ABuffer[i]);
  end;
end;

constructor TDecimateFX.Create(AObjectOwnerID: string);
begin
  inherited Create(AObjectOwnerID);
  
  // Default to this
  Init(16, 44100);
end;

const i2      : Double = 40000;
      i2v     : Double = 1/20000;
      noise   : Double = 1E-10;
      noi     : Double = 1E-10*((1.0/$10000) / $10000);  // 2^-32
      mTwo    : Single = -2;
      c3      : Single =  3;
      c6      : Single =  6;
      c12     : Single = 12;
      c24     : Single = 24;

var ipi : Double;

function f_Abs(f:Single):Single; assembler;
asm
 fld f.Single
 fabs
end;

function Tanh2_pas1(x:Single):Single;
var a : Single;
begin
 a:=f_abs(x);
 a:=a*(12+a*(6+a*(3+a)));
 if (x<0)
  then Result:=-a/(a+24)
  else Result:= a/(a+24);
end;

function Tanh2_pas2(x:Single):Single;
var   a,b:Single;
begin
// x:=x*2;

 a:=f_Abs(x);
 b:=12+a*(6+a*(3+a));
 Result:=(x*b)/(a*b+24);
end;

function Tanh2(x:Single):Single;  assembler;
asm
 fld x
 fld st(0)
 fabs       // a
 fld c3
 fadd st(0),st(1)
 fmul st(0),st(1)
 fadd c6
 fmul st(0),st(1)
 fadd c12          // b
 fmul st(2),st(0)  // x*b
 fmulp st(1),st(0)  // a*b
 fadd c24
 fdivp st(1),st(0)
end;


constructor TMoogFilter.Create(AObjectOwnerID: string);
begin
 inherited Create(AObjectOwnerID);
 fQ:=1;
 fF:=1000;
 fFS:=44100;
 fA[1]:=0;
 fA[2]:=0;
 fA[3]:=0;
 fA[4]:=0;
 fA[5]:=0;
 fOld:=0;
end;

procedure TMoogFilter.FreqCalc;
var fFc  : Double;
    fFcr : Double;
begin
 fFc:=  fF/fFs;
 // frequency & amplitude correction
 fFcr := 1.8730*(fFc*fFc*fFc) + 0.4955*(fFc*fFc) - 0.6490*fFc + 0.9988;
 fAcr := -3.9364*(fFc*fFc) + 1.8409*fFc + 0.9968;
 f2vg := i2*(1-exp(-ipi*fFcr*fFc)); // Filter Tuning
end;

procedure TMoogFilter.SetFrequency(v:single);
begin
 if fFS<=0 then raise exception.create('Sample Rate Error!');
 if v<>fF then
  begin
   fF:=v;
   FreqCalc;
  end;
end;

procedure TMoogFilter.SetFS(v:single);
begin
 if fFS<=0 then raise exception.create('Sample Rate Error!');
 if v<>fFs then
  begin
   fFs:=v;
   FreqCalc;
  end;
end;

procedure TMoogFilter.SetQ(v:single);
begin
 if v<>fQ then
  begin
   if v > 1
    then fQ:=1
    else if v < 0
     then fQ:=0
     else fQ:=v;
  end;
end;

function TMoogFilter.Process(const I : Single):Single;
begin
 // cascade of 4 1st order sections
 fA[1]:=fA[1]+f2vg*(Tanh2_pas2((I+(noise*Random)-2*fQ*fAcr*fOld)*i2v)-Tanh2_pas2(fA[1]*i2v));
// fA[1]:=fA[1]+(f2vg*(Tanh2((I+(noise*Random)-2*fQ*fOld*fAcr)*i2v)-Tanh2(fA[1]*i2v)));
 fA[2]:=fA[2]+f2vg*(Tanh2_pas2(fA[1]*i2v)-Tanh2_pas2(fA[2]*i2v));
 fA[3]:=fA[3]+f2vg*(Tanh2_pas2(fA[2]*i2v)-Tanh2_pas2(fA[3]*i2v));
 fA[4]:=fA[4]+f2vg*(Tanh2_pas2(fA[3]*i2v)-Tanh2_pas2(fA[4]*i2v));

 // 1/2-sample delay for phase compensation
 fOld:=fA[4]+fA[5];
 fA[5]:=fA[4];

 // oversampling
 fA[1]:=fA[1]+f2vg*(Tanh2_pas2((-2*fQ*fAcr*fOld)*i2v)-Tanh2(fA[1]*i2v));
 fA[2]:=fA[2]+f2vg*(Tanh2_pas2(fA[1]*i2v)-Tanh2_pas2(fA[2]*i2v));
 fA[3]:=fA[3]+f2vg*(Tanh2_pas2(fA[2]*i2v)-Tanh2_pas2(fA[3]*i2v));
 fA[4]:=fA[4]+f2vg*(Tanh2_pas2(fA[3]*i2v)-Tanh2_pas2(fA[4]*i2v));

 fOld:=fA[4]+fA[5];
 fA[5]:=fA[4];

 Result:=fOld;
end;

procedure TMoogFilter.Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer);
var
  i: Integer;
begin
  for i := 0 to Pred(AFrames) do
  begin
    ABuffer[i] := Process(ABuffer[i]);
  end;
end;


constructor TLFO.Create(AObjectOwnerID: string);
begin
  inherited Create(AObjectOwnerID);

  Speed:=100;
  fSpeed:=100;
  fMax:=1;
  fMin:=0;
  fValue:=0;
  fPos:=0;
  iSpeed:=Round($100000000/fSpeed);
  fType:=lfoTriangle;
  fScale:=fMax-((fMin+fMax)/2);
end;

procedure TLFO.SetType(tt: TLFOType);
begin
  fType:=tt;
  if fType = lfoSine then
    begin
     fPosMul:=(Sqrt(fScale*2))/$80000000;
     fHalfScale:=(Sqrt(fScale*2))/2;
    end
  else
    begin
     fPosMul:=fScale/$80000000;
    end;
end;

procedure TLFO.SetMin(v: Single);
begin
  fMin:=v;
  fScale:=fMax-((fMin+fMax)/2);
end;

procedure TLFO.SetMax(v: Single);
begin
  fMax:=v;
  fScale:=fMax-((fMin+fMax)/2);
end;

function TLFO.GetValue:Single;
begin
  if fType = lfoSine then
    begin
     Result:=Abs(fPos*fPosMul)-fHalfScale;
     Result:=Result*(fHalfScale*2-Abs(Result))*2;
     Result:=Result+((fMin+fMax)/2);
    end
  else
    begin
      Result:=Abs(fPos*(2*fPosMul))+fMin;
    end;
  fPos:=fPos+iSpeed;
end;

procedure TLFO.SetSpeed(v:Single);
begin
  fSpeed:=v;
  iSpeed:=Round($100000000/fSpeed);
end;

{ TDummyFilter }

constructor TDummyFilter.Create(AObjectOwnerID: string);
begin
  Inherited Create(AObjectOwnerID);
end;

procedure TDummyFilter.Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer);
var
  i: Integer;
begin
  for i := 0 to Pred(AFrames) do
  begin
    ABuffer[i] := ABuffer[i];
  end;
end;

begin
 ipi:=4*arctan(1);
end.

