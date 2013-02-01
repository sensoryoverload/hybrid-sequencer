unit plugin_moog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, plugin, global_command, fx;

type
  { TMoogFilter }

  TMoogFilter = class(TPluginNode)
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
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
      AOutputBuffer: PSingle; AFrames: Integer); override;
  published
    property Frequency: single read fF write SetFrequency;
    property SampleRate: single read fFS write SetFS;
    property Resonance: single read fQ write SetQ;
  end;

implementation

const i2      : Double = 40000;
      i2v     : Double = 1/20000;
      noise   : Double = 1E-10;

var ipi : Double;

{ TMoogFilter }

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

procedure TMoogFilter.Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
  AOutputBuffer: PSingle; AFrames: Integer);
var
  i: Integer;
begin
  for i := 0 to Pred(AFrames) do
  begin
    AOutputBuffer[i] := Process(AInputBuffer[i]);
  end;
end;


begin
  ipi:=4*arctan(1);
end.

