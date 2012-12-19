unit plugin_decimate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, plugin, global_command;

type
  { TDecimateFX }

  // Bit/Samplerate -reducer
  // bits: 1..32
  // rate: 0..1 (1 is original samplerate)
  TDecimateFX = class(TPluginNode)
  private
    FBits: longint;
    y,
    FRateSkip,
    FSampleRate: single;
    procedure SetBits(AValue: Integer);
    procedure SetSampleRate(AValue: Single);
  public
    constructor Create(AObjectOwnerID: string);
    function Decimate(i: single): single;
    procedure Init(ABits: integer; ASampleRate: single);
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); override;
  published
    property Bits: Integer read FBits write SetBits;
    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

implementation

{ TDecimateFX }

function TDecimateFX.Decimate(i: single): single;
begin
  FRateSkip:= FRateSkip + FSampleRate;
  if FRateSkip > 1 then
  begin
    FRateSkip:= FRateSkip - 1;
    y:= round(i * FBits) / FBits;
  end;
  result:= y;
end;

procedure TDecimateFX.Init(ABits: integer; ASampleRate: single);
begin
  FBits:= 1 shl (ABits - 1);
  FRateSkip:= 1;
  FSampleRate:= ASampleRate;
end;

procedure TDecimateFX.Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer);
var
  i: Integer;
begin
  for i := 0 to Pred(AFrames) do
  begin
    ABuffer[i] := Decimate(ABuffer[i]);
  end;
end;

procedure TDecimateFX.SetBits(AValue: Integer);
begin
  if FBits = AValue then Exit;
  FBits := AValue;

  Init(FBits, FSampleRate);
end;

procedure TDecimateFX.SetSampleRate(AValue: Single);
begin
  if FSampleRate = AValue then Exit;
  FSampleRate := AValue;

  Init(FBits, FSampleRate);
end;

constructor TDecimateFX.Create(AObjectOwnerID: string);
begin
  inherited Create(AObjectOwnerID);

  // Default to this
  Init(16, 44100);
end;


end.

