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
  Classes, SysUtils, plugin, global_command, math;
  
type

  { TAudioRingBuffer }

  TAudioRingBuffer = class
  private
    FDelayMs: Integer;
    FDelaySmp: Integer;
    FWritePtr: Integer;
    FReadPtr: Integer;
    FBuffer: PSingle;
    FBufferSize: Integer;
    FChannels: Integer;
    FSampleRate: Integer;
    procedure SetDelayMs(AValue: Integer);
    procedure SetDelaySmp(AValue: Integer);
  public
    constructor Create(ASampleRate: Integer; AChannels: Integer);
    destructor Destroy; override;
    function Process(input: Single): Single;
    property DelayMs: Integer read FDelayMs write SetDelayMs;
    property DelaySmp: Integer read FDelaySmp write SetDelaySmp;
  end;


  { TFIFOAudioBuffer }

  TFIFOAudioBuffer = class
  private
    FBuffer: PSingle;
    FChannelCount: Integer;
    FDelay: Integer;
    FDelaySamples: Integer;
    FLength: Integer;
    FBeginPtr: Integer;
    FEndPtr: Integer;
    FSampleRate: Integer;
    procedure SetChannelCount(AValue: Integer);
    procedure SetDelay(AValue: Integer);
    procedure SetSampleRate(AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    // Push samples in the internal buffer
    procedure PutSamples(ABuffer: PSingle; ANumSamples: Integer);
    function ReceiveSamples(ABuffer: PSingle; ANumSamples: Integer): Integer;
    procedure Process(ABuffer: PSingle; ANumSamples: Integer);

    // Set the delay in
    property Delay: Integer read FDelay write SetDelay;
    property SampleRate: Integer read FSampleRate write SetSampleRate;
    property ChannelCount: Integer read FChannelCount write SetChannelCount;
  end;

  { TDummyFilter }

  TDummyFilter = class(TInternalNode)
  public
    constructor Create(AObjectOwnerID: string);
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); override;
  end;

  TLFOType = (lfoTriangle, lfoSine);

  TLFO = class(TInternalNode)
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
  function saturate(x: single; t: single): single;

implementation

var RandSeed:Integer=$DEAD;
const
  c3      : Single =  3;
  c6      : Single =  6;
  c12     : Single = 12;
  c24     : Single = 24;

{ TAudioRingBuffer }

procedure TAudioRingBuffer.SetDelayMs(AValue: Integer);
begin
  if FDelayMs = AValue then Exit;
  FDelayMs := AValue;
end;

procedure TAudioRingBuffer.SetDelaySmp(AValue: Integer);
begin
  if FDelaySmp = AValue then Exit;
  FDelaySmp := AValue;

  FWritePtr := FDelaySmp * FChannels;
  FReadPtr := 0;
end;

constructor TAudioRingBuffer.Create(ASampleRate: Integer; AChannels: Integer);
begin
  FChannels := AChannels;
  FSampleRate := FSampleRate;
  FBufferSize := ASampleRate * AChannels;
  FBuffer := Getmem(FBufferSize * SizeOf(Single));

  // Zero delay by default
  FWritePtr := 0;
  FReadPtr := 0;
end;

destructor TAudioRingBuffer.Destroy;
begin
  FreeMem(FBuffer);

  inherited Destroy;
end;

function TAudioRingBuffer.Process(input: Single): Single;
begin
  FBuffer[FWritePtr] := input;
  Result := FBuffer[FReadPtr];
  Inc(FWritePtr, FChannels);
  if FWritePtr > FBufferSize then
  begin
    Dec(FWritePtr, FBufferSize);
  end;
  Inc(FReadPtr, FChannels);
  if FReadPtr > FBufferSize then
  begin
    Dec(FReadPtr, FBufferSize);
  end;
end;

{ TFIFOAudioBuffer }

{
  Sets the length of the buffer
}
procedure TFIFOAudioBuffer.SetDelay(AValue: Integer);
begin
  if AValue = 0 then
    FDelay := 1
  else
    FDelay := AValue;

  FDelaySamples := Round(FSampleRate / (1000 / FDelay));
end;

procedure TFIFOAudioBuffer.SetChannelCount(AValue: Integer);
begin
  if FChannelCount = AValue then Exit;
  FChannelCount := AValue;
end;

procedure TFIFOAudioBuffer.SetSampleRate(AValue: Integer);
begin
  if FSampleRate = AValue then Exit;
  FSampleRate := AValue;
end;

constructor TFIFOAudioBuffer.Create;
begin
  inherited Create;

  FSampleRate := 44100;
  FLength := 88200;
  FBeginPtr := 0;
  FEndPtr := 0;

  FBuffer := GetMem(SizeOf(Single) * FLength);
end;

destructor TFIFOAudioBuffer.Destroy;
begin
  FreeMem(FBuffer);

  inherited Destroy;
end;

{
  Pushes an amount of samples to the internal buffer
}
procedure TFIFOAudioBuffer.PutSamples(ABuffer: PSingle; ANumSamples: Integer);
var
  i: Integer;
begin
  // Check if there is capacity to move ANumSamples into the buffer
  if ANumSamples * 2 * FChannelCount < FLength then
  begin
    for i := Pred(ANumSamples * FChannelCount) downto 0 do
    begin
      FBuffer[FDelaySamples - ANumSamples + i] := FBuffer[i];
    end;
    for i := 0 to Pred(ANumSamples * FChannelCount) do
    begin
      FBuffer[i] := ABuffer[i];
    end;
//    Move(FBuffer, FBuffer[FDelaySamples - ANumSamples], FDelaySamples * SizeOf(Single));
//    Move(ABuffer, FBuffer, ANumSamples * SizeOf(Single));
//    Inc(FBeginPtr, ANumSamples);
  end
  else
  begin
    raise Exception.Create('TFIFOAudioBuffer: Buffer overflow');
  end;
end;

{
  Fetch an amount of samples from the internal buffer
}
function TFIFOAudioBuffer.ReceiveSamples(ABuffer: PSingle; ANumSamples: Integer
  ): Integer;
var
  i: Integer;
begin
  for i := 0 to Pred(ANumSamples * FChannelCount) do
  begin
    ABuffer[i] := FBuffer[FDelaySamples - ANumSamples + i];
  end;
  //Move(FBuffer[FDelaySamples - ANumSamples], ABuffer, ANumSamples * SizeOf(Single));
end;

procedure TFIFOAudioBuffer.Process(ABuffer: PSingle; ANumSamples: Integer);
begin
  // Put samples in one end of the pipe
  PutSamples(ABuffer, ANumSamples);

  // Get samples from the other end of the pipe
  ReceiveSamples(ABuffer, ANumSamples);
end;



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

procedure TDummyFilter.Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer);
var
  i: Integer;
begin
  for i := 0 to Pred(AFrames) do
  begin
    ABuffer[i] := ABuffer[i];
  end;
end;

function sigmoid(x: single): single;
begin
  if (abs(x) < 1) then
    Result := x * (1.5 - 0.5 * x * x)
  else
    if x > 0 then
      Result := 1
    else
      Result := -1;
end;

function saturate(x: single; t: single): single;
begin
  if (abs(x) < t) then
    Result := x
  else
  begin
    if (x > 0) then
      Result := t + (1-t) * sigmoid((x - t) / ((1 - t) * 1.5))
    else
      Result := -(t + (1-t) * sigmoid((-x - t) / ((1 - t) * 1.5)));
  end;
end;

begin

end.

