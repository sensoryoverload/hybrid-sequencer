unit freereverb;
{
       Unit: Reverb for either KOL or VCL Delphi and Freepascal 1.9.x,
             probably Kylix.
    purpose: Based on the Freeverb (C++) design by Jezar[at]dreampoint.co.uk
     Author: Thaddy de Koning, thaddy[at]thaddy.com
  Copyright: Original in C++ ©2000, Jezar
             Delphi and Freepascal version ©2003,
             Basm  added ©2004, Thaddy de Koning
             Use as you like, copyrighted freeware.

    Remarks: Removed original skip for interleave.
             Comb and Allpass filter processing rewritten in BASM.
             It is as faithfull a translation as I could manage.
             Sound is exactly the same as FreeVerb 3.
             I have writen it to be as portable as my Pascals go
             Although not tested, it should work in BCB6+ and Kylix too.
             It works with Delphi 6/7 and Freepascal 1.9.X under both linux
             and windows.
             There are no dependencies on the windows unit.
}

interface

uses
  Classes;

{$asmmode intel}
{$ALIGN 8}
//
// Reverb model tuning values, taken from original algoritm by Jezar
//

const
  Kdenorm:single =1.0e-23;

  numcombs = 8;
  numallpasses = 4;
  muted = 0;
  fixedgain = 0.015;
  scalewet = 3;
  scaledry = 2;
  scaledamp = 0.4;
  scaleroom = 0.28;
  offsetroom = 0.7;
  initialroom = 0.5;
  initialdamp = 0.5;
  initialwet = 1 / scalewet;
  initialdry = 1 / scaledry;
  initialwidth = 1;
  initialmode = 0;
  freezemode = 0.5;
  stereospread = 23;
  // These values assume 44.1KHz sample rate
  // they will probably be OK for 48KHz sample rate
  // but would need scaling for 96KHz (or other) sample rates.
  // The values were obtained by listening tests.
  combtuningL1 = 1116;
  combtuningR1 = 1116 + stereospread;
  combtuningL2 = 1188;
  combtuningR2 = 1188 + stereospread;
  combtuningL3 = 1277;
  combtuningR3 = 1277 + stereospread;
  combtuningL4 = 1356;
  combtuningR4 = 1356 + stereospread;
  combtuningL5 = 1422;
  combtuningR5 = 1422 + stereospread;
  combtuningL6 = 1491;
  combtuningR6 = 1491 + stereospread;
  combtuningL7 = 1557;
  combtuningR7 = 1557 + stereospread;
  combtuningL8 = 1617;
  combtuningR8 = 1617 + stereospread;
  allpasstuningL1 = 556;
  allpasstuningR1 = 556 + stereospread;
  allpasstuningL2 = 441;
  allpasstuningR2 = 441 + stereospread;
  allpasstuningL3 = 341;
  allpasstuningR3 = 341 + stereospread;
  allpasstuningL4 = 225;
  allpasstuningR4 = 225 + stereospread;

  // Allpass filter class declaration
type
  TAllpass = class(TObject)
  private
    feedback: Single;
    buffer: psingle;
    bufsize,
    bufidx: integer;
  public
    constructor Create(Buffersize: integer); virtual;
    destructor Destroy; override;
    function process(const input: Single): Single; register; inline; assembler;
    procedure mute;
    procedure setfeedback(Value: Single);
    function getfeedback: Single;
  end;

  // Comb filter class declaration
  Tcomb = class(TObject)
  private
    feedback,
    filterstore,
    damp1,
    damp2: Single;
    buffer:psingle;
    bufsize,
    bufidx: integer;
    fTube: boolean;
  public
    constructor Create(Buffersize: integer); virtual;
    destructor Destroy; override;
    function process(const input: Single): Single; register; inline; assembler;
    procedure mute;
    procedure setdamp(Value: Single);
    function getdamp: Single;
    procedure setfeedback(Value: Single);
    function getfeedback: Single;
  end;


  // Reverb model class declaration
  //

  { TReverb }

  TReverb = class(TObject)
  private
    Fsamplerate: Single;
    Isband: boolean;
    Freq,
    FQual,
    Ftype: Single;
    gain,
    roomsize,
    roomsize1,
    damp,
    damp1,
    wet,
    wet1,
    wet2,
    dry,
    Width,
    mode: Single;

    // Comb filters
    combL: array [0..numcombs - 1] of TComb;
    combR: array [0..numcombs - 1] of TComb;

    // Allpass filters
    allpassL: array [0..numallpasses - 1] of TAllpass;
    allpassR: array [0..numallpasses - 1] of TAllpass;
  protected
    procedure update;
  public
    constructor Create;
    destructor Destroy; override;
    procedure mute;
    procedure process(
      input: psingle;
      output: psingle;
      numsamples: integer);
    procedure setroomsize(Value: Single);
    function getroomsize: Single;
    procedure setdamp(Value: Single);
    function getdamp: Single;
    procedure setwet(Value: Single);
    function getwet: Single;
    procedure setdry(Value: Single);
    function getdry: Single;
    procedure setwidth(Value: Single);
    function getwidth: Single;
    procedure setmode(Value: Single);
    function getmode: Single;
  end;

implementation

constructor TAllpass.Create(Buffersize: integer);
begin
  inherited Create;
  Bufsize := Buffersize * SizeOf(Single) * 2;
  Buffer := AllocMem(BufSize);
  bufidx := 0;
end;

destructor TAllPAss.destroy;
begin
  freemem(Buffer);
  inherited;
end;

function Tallpass.getfeedback: Single;
begin
  Result := Feedback;
end;

procedure Tallpass.mute;
var
  i: integer;
begin
  Fillchar(Buffer^,Bufsize, 0);
end;

(*function Tallpass.process(input: Single): Single;
begin
  //if input < Kdenorm then input := 0;
  Buffer[Bufidx] := ((Buffer[Bufidx] - Input) * feedback) + Input;
  if Bufidx < BufSize then
    Inc(Bufidx)
  else
    Bufidx := 0;
end;*)
{ I really don't know if this is all as fast as can be,
  but it beats Delphi's compiler generated code hands down,
  Thaddy}
function Tallpass.process(const input: Single): Single;
asm
  mov  ecx, [eax].buffer                   // buffer start in ecx
  mov  edx, [eax].Bufidx                   // buffer index in edx
  fld  input

  // This checks for very small values that can cause a processor
  // to switch in extra precision mode, which is expensive.
  // Since such small values are irrelevant to audio, avoid this.
  // The code is equivalent to the C inline macro by Jezar
  // This is the same spot where the original C macro appears
  test dword ptr [ecx+edx], $7F800000      // test if denormal
  jnz @Normal
  mov dword ptr [ecx+edx], 0               // if so, zero out
@normal:

  fld  [ecx+edx].Single                    // load current sample from buffer
  fsub st(0), st(1)                        // subtract input sample
  // NOT fsub, because delphi 7 translates that into fsubp!
  fxch                                    // this is a zero cycle operant,
                                          // just renames the stack internally
  fmul [eax].feedback                     // multiply stored sample with feedback
  fadd input                              // and add the input
  fstp [ecx + edx].Single;                // store at the current sample pos
  add  edx, 4                             // increment sample position
  cmp  edx, [eax].BufSize;                // are we at end of buffer?
  jb   @OK
  xor  edx, edx                           // if so, reset buffer index
@OK:
  mov  [eax].bufidx, edx                  // and store new index,
                                          // result already in st(0),
                                          // hence the fxch
end;

procedure Tallpass.setfeedback(Value: Single);
begin
  Feedback := Value;
end;

constructor TComb.Create(Buffersize: integer);
begin
  inherited Create;
  BufSize := Buffersize * SizeOf(Single) * 2;
  Buffer:= AllocMem(BufSize);
  filterstore := 0;
  bufidx := 0;
end;

{ Tcomb }
destructor Tcomb.destroy;
begin
  freemem(Buffer);
  inherited;
end;

function Tcomb.getdamp: Single;
begin
  Result := damp1;
end;

function Tcomb.getfeedback: Single;
begin
  Result := Feedback;
end;

procedure Tcomb.mute;
var
  i: integer;
begin
  Fillchar(Buffer^,BufSize,0);
end;

(*function Tcomb.process(input: Single): Single;
var
  Temp : Single;
begin
  //if input < Kdenorm then input := 0;
  Result := buffer[bufidx];
  Temp := Result * feedback + input;
  //if filterStore < Kdenorm then filterStore := 0;
  Result := Result * damp1 + filterStore * damp2;
  filterStore := Temp;
end;*)
{ I really don't know if this is all as fast as can be,
  but it beats Delphi's compiler generated code hands down,
  Thaddy}

function Tcomb.process(const input: Single): Single;
asm
  mov   ecx, [eax].Buffer                        // buffer start in ecx
  mov   edx, [eax].Bufidx                        // buffer index in edx

  // This checks for very small values that can cause a processor
  // to switch in extra precision mode, which is expensive.
  // Since such small values are irrelevant to audio, avoid this.
  // This is the same spot where the original C macro appears
  test  dword ptr [ecx+edx], $7F800000           // test if denormal
  jnz   @Normal
  mov   dword ptr [ecx+edx], 0                   // if so, zero out
@normal:

  fld   [ecx+edx].Single;                        // load sample from buffer
  fld   st(0)                                    // duplicate on the stack
  fmul  [eax].damp2                              // multiply with damp2
  fld   [eax].filterstore;                       // load stored filtered sample
  fmul  [eax].damp1                              // multiply with damp1
  faddp
//  fadd  Kdenorm
  fst   [eax].filterstore                        // store it back

  // This checks for very small values that can cause a processor
  // to switch in extra precision mode, which is expensive.
  // Since such small values are irrelevant to audio, avoid this.
  // This is the same spot where the original C macro appears
  test  dword ptr [eax].filterstore, $7F800000   // test if denormal
  jnz   @Normal2
  mov   dword ptr [eax].filterstore, 0           // if so, zero out
@normal2:

  fmul  [eax].feedback                           // multiply with feedback
  fadd  input                                    // and add to input sample
//  fadd  Kdenorm
  fstp  [ecx+edx].Single                         // store at current buffer pos
  add   edx, 4                                   // update buffer index
  cmp   edx, [eax].BufSize;                      // end of buffer reached?
  jb    @OK
  xor   edx, edx                                 // if so, reset buffer index
@OK:
  mov  [eax].bufidx, edx                         // and store new index.
                                                 // result already in st(0),
                                                 // hence duplicate
end;


procedure Tcomb.setdamp(Value: Single);
begin
  damp1 := Value;
  damp2 := 1 - Value;
end;

procedure Tcomb.setfeedback(Value: Single);
begin
  Feedback := Value;
end;

constructor TReverb.Create;
begin
  inherited;

  CombL[0] := Tcomb.Create(combtuningL1);
  CombR[0] := Tcomb.Create(combtuningR1);
  CombL[1] := Tcomb.Create(combtuningL2);
  CombR[1] := Tcomb.Create(combtuningR2);
  CombL[2] := Tcomb.Create(combtuningL3);
  CombR[2] := Tcomb.Create(combtuningR3);
  CombL[3] := Tcomb.Create(combtuningL4);
  CombR[3] := Tcomb.Create(combtuningR4);
  CombL[4] := Tcomb.Create(combtuningL5);
  CombR[4] := Tcomb.Create(combtuningR5);
  CombL[5] := Tcomb.Create(combtuningL6);
  CombR[5] := Tcomb.Create(combtuningR6);
  CombL[6] := Tcomb.Create(combtuningL7);
  CombR[6] := Tcomb.Create(combtuningR7);
  CombL[7] := Tcomb.Create(combtuningL8);
  CombR[7] := Tcomb.Create(combtuningR8);
  AllpassL[0] := TAllpass.Create(allpasstuningL1);
  AllpassR[0] := TAllpass.Create(allpasstuningR1);
  AllpassL[1] := TAllpass.Create(allpasstuningL2);
  AllpassR[1] := TAllpass.Create(allpasstuningR2);
  AllpassL[2] := TAllpass.Create(allpasstuningL3);
  AllpassR[2] := TAllpass.Create(allpasstuningR3);
  AllpassL[3] := TAllpass.Create(allpasstuningL4);
  AllpassR[3] := TAllpass.Create(allpasstuningR4);
  // Set default values
  allpassL[0].setfeedback(0.5);
  allpassR[0].setfeedback(0.5);
  allpassL[1].setfeedback(0.5);
  allpassR[1].setfeedback(0.5);
  allpassL[2].setfeedback(0.5);
  allpassR[2].setfeedback(0.5);
  allpassL[3].setfeedback(0.5);
  allpassR[3].setfeedback(0.5);
  setwet(initialwet);
  setroomsize(initialroom);
  setdry(initialdry);
  setdamp(initialdamp);
  setwidth(initialwidth);
  setmode(initialmode);
  mute;
end;

destructor TReverb.Destroy;
var
  i: integer;
begin
  for i := 0 to Pred(numallpasses) do
  begin
    allpassL[i].Free;
    allpassR[i].Free;
  end;
  for i := 0 to Pred(numcombs) do
  begin
    CombR[i].Free;
    CombL[i].Free;
  end;
  inherited Destroy;
end;

function TReverb.getdamp: Single;
begin
  Result := damp / scaledamp;
end;

function TReverb.getdry: Single;
begin
  Result := dry / scaledry;
end;

function TReverb.getmode: Single;
begin
  if mode >= freezemode then
    Result := 1
  else
    Result := 0;
end;

function TReverb.getroomsize: Single;
begin
  Result := (roomsize - offsetroom) / scaleroom;
end;

function TReverb.getwet: Single;
begin
  Result := wet / scalewet;
end;

function TReverb.getwidth: Single;
begin
  Result := Width;
end;

procedure TReverb.mute;
var
  i: integer;
begin
  if getmode >= freezemode then exit;

  for i := 0 to numcombs - 1 do
  begin
    combL[i].mute;
    combR[i].mute;
  end;
  for i := 0 to numallpasses - 1 do
  begin
    allpassL[i].mute;
    allpassR[i].mute;
  end;
end;

procedure TReverb.process(
  input: psingle;
  output: psingle;
  numsamples: integer);
var
  OutL, OutR: Single;
  i, j: integer;
  offsetL, offsetR: Integer;
  mixinput: single;
begin
  for i := 0 to Numsamples - 1 do
  begin
    offsetL := i * 2;
    offsetR := i * 2 + 1;
    outL := 0;
    outR := 0;
    mixinput := (Input[offsetL] + Input[offsetR]) * gain;
    // Accumulate comb filters in parallel
    for j := 0 to numcombs - 1 do
    begin
      outL := OutL + combL[j].process(mixinput);
      outR := OutR + combR[j].process(mixinput);
    end;
    // Feed through allpasses in series
    for j := 0 to numallpasses - 1 do
    begin
      outL := allpassL[j].process(outL);
      outR := allpassR[j].process(outR);
    end;
    // Calculate output REPLACING anything already there
    Output[offsetL] := outL * wet1 + outR * wet2 + Input[offsetL] * dry;
    Output[offsetR] := outR * wet1 + outL * wet2 + Input[offsetR] * dry;
  end;
end;

procedure TReverb.setdamp(Value: Single);
begin
  damp := Value * scaledamp;
  update;
end;

procedure TReverb.setdry(Value: Single);
begin
  dry := Value * scaledry;
  Update;
end;

procedure TReverb.setmode(Value: Single);
begin
  mode := Value;
  update;
end;

procedure TReverb.setroomsize(Value: Single);
begin
  roomsize := (Value * scaleroom) + offsetroom;
  update;
end;

procedure TReverb.setwet(Value: Single);
begin
  wet := Value * scalewet;
  update;
end;

procedure TReverb.setwidth(Value: Single);
begin
  Width := Value;
  update;
end;

procedure TReverb.update;
var
  i: integer;
begin
  // Recalculate internal values after parameter change
  wet1 := wet * (Width / 2 + 0.5);
  wet2 := wet * ((1 - Width) / 2);
  if mode >= freezemode then
  begin
    roomsize1 := 1;
    damp1 := 0;
    gain := muted;
  end
  else
  begin
    roomsize1 := roomsize;
    damp1 := damp;
    gain := fixedgain;
  end;
  for i := 0 to numcombs - 1 do
  begin
    combL[i].setfeedback(roomsize1);
    combR[i].setfeedback(roomsize1);
  end;
  for i := 0 to numcombs - 1 do
  begin
    combL[i].setdamp(damp1);
    combR[i].setdamp(damp1);
  end;
end;

end.
