unit plugin_decimate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, plugin, global_command;

type
  TDecimateParameter = (dpSampleRate, dpBits);

  { TPluginDecimate }

  // Bit/Samplerate -reducer
  // bits: 1..32
  // rate: 0..1 (1 is original samplerate)
  TPluginDecimate = class(TPluginNode)
  private
    FBits: longint;
    y,
    FRateSkip,
    FSampleRate: single;
    procedure SetBits(AValue: Integer);
    procedure SetBitsFloat(AValue: single);
    procedure SetSampleRate(AValue: Single);
  public
    constructor Create(AObjectOwnerID: string; AMapped: Boolean = True); reintroduce;
    function Decimate(i: single): single;
    procedure Init(ABits: integer; ASampleRate: single);
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
      AOutputBuffer: PSingle; AFrames: Integer); override;
    procedure Instantiate; override;
  published
    property Bits: Integer read FBits write SetBits;
    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

  { TDecimateCommand }

  TDecimateCommand = class(TCommand)
  private
    FDecimate: TPluginDecimate;
  public
    procedure Initialize; override;
  end;

  { TDecimateParameterCommand }

  TDecimateParameterCommand = class(TDecimateCommand)
  private
    FOldValue: Variant;
    FValue: Variant;
    FParameter: TDecimateParameter;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Value: Variant read FValue write FValue;
    property Parameter: TDecimateParameter read FParameter write FParameter;
  end;

implementation

uses
  global;

{ TDecimateParameterCommand }

procedure TDecimateParameterCommand.DoExecute;
begin
  FDecimate.BeginUpdate;

  case FParameter of
    dpBits:
    begin
      FOldValue := FDecimate.Bits;
      FDecimate.Bits := FValue;
    end;
    dpSampleRate:
    begin
      FOldValue := FDecimate.SampleRate;
      FDecimate.SampleRate := FValue;
    end;
  end;

  FDecimate.EndUpdate;
end;

procedure TDecimateParameterCommand.DoRollback;
begin
  FDecimate.BeginUpdate;

  case FParameter of
    dpBits:
    begin
      FDecimate.Bits := FOldValue;
    end;
    dpSampleRate:
    begin
      FDecimate.SampleRate := FOldValue;
    end;
  end;

  FDecimate.EndUpdate;
end;

{ TDecimateCommand }

procedure TDecimateCommand.Initialize;
begin
  FDecimate := TPluginDecimate(GObjectMapper.GetModelObject(ObjectID));
end;

{ TPluginDecimate }

function TPluginDecimate.Decimate(i: single): single;
begin
  FRateSkip := FRateSkip + FSampleRate;
  if FRateSkip > 1 then
  begin
    FRateSkip := FRateSkip - 1;
    y := round(i * FBits) / FBits;
  end;
  result := y;
end;

procedure TPluginDecimate.Init(ABits: integer; ASampleRate: single);
begin
  FBits := 1 shl (ABits - 1);
  FRateSkip := 1;
  FSampleRate:= ASampleRate;
end;

procedure TPluginDecimate.Process(AMidiBuffer: TMidiBuffer;
  AInputBuffer: PSingle; AOutputBuffer: PSingle; AFrames: Integer);
var
  i: Integer;
begin
  for i := 0 to Pred(AFrames) do
  begin
    AOutputBuffer[i] := Decimate(AInputBuffer[i]);
  end;
end;

procedure TPluginDecimate.Instantiate;
begin
  CreatePortParameter('Bits', 1, 32, True, True, False, True, False, False, 32, FBits, @SetBitsFloat);
  CreatePortParameter('Samplerate', 0, 1, True, True, False, True, False, False, 1, FSampleRate, @SetSampleRate);
end;

procedure TPluginDecimate.SetBits(AValue: Integer);
begin
  if FBits = AValue then Exit;
  FBits := AValue;

  Init(FBits, FSampleRate);
end;

procedure TPluginDecimate.SetBitsFloat(AValue: single);
begin
  Bits := Round(AValue);
end;

procedure TPluginDecimate.SetSampleRate(AValue: Single);
begin
  if FSampleRate = AValue then Exit;
  FSampleRate := AValue;

  Init(FBits, FSampleRate);
end;

constructor TPluginDecimate.Create(AObjectOwnerID: string; AMapped: Boolean = True);
begin
  inherited Create(AObjectOwnerID, AMapped);

  // Default to this
  Init(16, 44100);
end;


end.

