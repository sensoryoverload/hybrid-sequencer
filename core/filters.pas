unit filters;

{$mode objfpc}{$H+}

{$fputype SSE}

interface

uses
  Classes, SysUtils, globalconst, baseengine, audioutils, diode_ladder_filter,
  transistor_ladder;

const
  divby6 = 1 / 6;

type
  TModSource = (
    msLFO1,
    msLFO2,
    msLFO3,
    msAmpEnvelope,
    msFilterEnvelope,
    msPitchEnvelope,
    msEnvelopeFollower,
    msOscillator1,
    msOscillator2,
    msOscillator3,
    msOscillatorMix,
    msFilterOutput,
    msAmpOutput,
    msVelocity,
    msNote);

var
  ModSourceDescr: array[0..14] of string = (
    'LFO1',
    'LFO2',
    'LFO3',
    'AmpEnvelope',
    'FilterEnvelope',
    'PitchEnvelope',
    'EnvelopeFollower',
    'Oscillator1',
    'Oscillator2',
    'Oscillator3',
    'OscillatorMix',
    'FilterOutput',
    'AmpOutput',
    'Velocity',
    'Note');

const p4=1.0e-24;

type
  TBaseFilterType = class(THybridPersistentModel)

  end;

  TEnumFilterTypes = (ftLowpass, ftHighpass, ftBandpass, ftBandreject, ftMoog, ft303, ftTransistorLadder);

  { TFilter }

  TFilter = class(THybridPersistentModel)
  private
    FFrequency: Single;
    FFrequencyInternal: Single;
    FResonance: Single;
    FFreqModSource: TModSource;
    FFreqModAmount: Single;
    FEnvelopeAmount: Single;
    FFilterType: TEnumFilterTypes;
    FActive: Boolean;
    FSmoothCutoff: TParamSmooth;
    procedure SetActive(const AValue: Boolean);
    procedure SetFilterType(const AValue: TEnumFilterTypes);
    procedure SetFrequency(AValue: Single);
  protected
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
    property FrequencyInternal: Single read FFrequencyInternal;
  published
    property Frequency: Single read FFrequency write SetFrequency; // 0..1
    property Resonance: Single read FResonance write FResonance; // 0..1 ?
    property FreqModSource: TModSource read FFreqModSource write FFreqModSource;
    property FreqModAmount: single read FFreqModAmount write FFreqModAmount;
    property EnvelopeAmount: single read FEnvelopeAmount write FEnvelopeAmount;
    property FilterType: TEnumFilterTypes read FFilterType write SetFilterType;
    property Active: Boolean read FActive write SetActive;
  end;


  { TBaseFilterTypeEngine }

  TBaseFilterTypeEngine = class(TBaseEngine)
  private
    FFilter: TFilter;
    FLevel: Single;
    procedure SetFilter(const AValue: TFilter);
  public
    // Descendant should use Initialize to change coeffecients, pre-calculations, etc
    function Process(I: Single): Single; virtual; abstract;
    property Filter: TFilter read FFilter write SetFilter;
    property Level: Single read FLevel;
  end;

  { TFilterEngine }

  TFilterEngine = class
  private
    FFilterType: TEnumFilterTypes;
    procedure SetFilterType(const AValue: TEnumFilterTypes);
  public
    procedure Initialize;
    property FilterType: TEnumFilterTypes read FFilterType write SetFilterType;
  end;

  { TLP24DB }

  TLP24DB = class(TBaseFilterTypeEngine)
  private
    FFrequency: Single;
    FResonance: Single;
    FSmoothCutoff: TParamSmooth;
    t, t2, t3, x, k, p, r,
    y1, y2, y3, y4,
    oldx, oldy1, oldy2, oldy3,
    divbysamplerate: Single;
    _kd: Single;
    procedure SetFrequency(AValue: Single);
    procedure SetResonance(AValue: Single);
  public
    constructor Create(AFrames: Integer); override;
    destructor Destroy; override;
    function Process(I: Single): Single; override;
    procedure Calc;
    procedure Initialize; override;
    property Frequency: Single read FFrequency write SetFrequency; // 0..1
    property Resonance: Single read FResonance write SetResonance; // 0..1 ?
  end;

  {

    TDspFilter: Filter taken from DiscoDSP Highlife Open Source Sampler

  }
  TDspFilter = class(TBaseFilterTypeEngine)
  private
    // Filtertypes
    FDiodeLadderFilter: TDiodeLadderFilter;
    FLP24DB: TLP24DB;
    FTransistorLadderFilter: TTransistorLadder;

    FFrequency: Single;
    FResonance: Single;
    FSmoothCutoff: TParamSmooth;
    b0,b1,b2,b3,b4: single;
    t1,t2,t3: single;
    f,p,q: single;

    t, x, k, r,
    y1, y2, y3, y4,
    oldx, oldy1, oldy2, oldy3,
    _kd: Single;
    procedure SetFrequency(AValue: Single);
    procedure SetResonance(AValue: Single);
  public
    constructor Create(AFrames: Integer); override;
    destructor Destroy; override;
    function Process(AInput: Single): Single; override;
    procedure Calc;
    procedure Initialize; override;
    property Frequency: Single read FFrequency write SetFrequency; // 0..1
    property Resonance: Single read FResonance write SetResonance; // 0..1 ?
  end;

  {
    Taken and converted from the Open303 project
  }

implementation

uses
  fx;

{ TDspFilter }

procedure TDspFilter.SetFrequency(AValue: Single);
begin
  FFrequency := FSmoothCutoff.Process(AValue);
  Calc;
end;

procedure TDspFilter.SetResonance(AValue: Single);
begin
  FResonance := AValue;
  Calc;
end;

constructor TDspFilter.Create(AFrames: Integer);
begin
  inherited Create(AFrames);

  FSmoothCutoff := TParamSmooth.Create;

  FDiodeLadderFilter := TDiodeLadderFilter.Create;
  FLP24DB := TLP24DB.Create(AFrames);
  FTransistorLadderFilter := TTransistorLadder.Create;
end;

destructor TDspFilter.Destroy;
begin
  FSmoothCutoff.Free;

  FDiodeLadderFilter.Free;
  FLP24DB.Free;
  FTransistorLadderFilter.Free;

  inherited Destroy;
end;

function TDspFilter.Process(AInput: Single): Single;
begin
  // Keep between valid range!
  if AInput > 0.999 then AInput := 0.999;
  if AInput < -0.999 then AInput := -0.999;

  // filter work
  case FFilter.FilterType of
    ftLowpass, ftHighpass, ftBandpass, ftBandreject:
    begin
      AInput -= q * b4;
      t1 := b1 + p4;
      b1 := (AInput + b0) * p - b1 * f;
      t2 := b2 + p4;
      b2 := (b1 + t1) * p - b2 * f;
      t1 := b3 + p4;
      b3 := (b2 + t2) * p - b3 * f;
      b4 := (b3 + t1) * p - b4 * f;

      // update
      b0 := AInput;

      if FFilter.FilterType = ftLowpass then
        Result := b4                     // lowpass output
      else if FFilter.FilterType = ftHighpass then
        Result := AInput - b4            // hipass output
      else if FFilter.FilterType = ftBandpass then
        Result := 3 * (b3 - b4)          // bandpass output
      else if FFilter.FilterType = ftBandreject then
        Result := 3 * (b3 - b4) - AInput // band reject
      else
        Result := 0;
    end;
    ftMoog:
    begin
      Result := FLP24DB.Process(AInput);
    end;
    ft303:
    begin
      Result := FDiodeLadderFilter.Process(AInput);
    end;
    ftTransistorLadder:
    begin
      Result := FTransistorLadderFilter.Process(AInput);
    end;
  end;
end;

procedure TDspFilter.Calc;
var
  lInternalFrequency: single;
  lInternalResonance: single;
begin
  lInternalFrequency := FFrequency;
  lInternalResonance := FResonance;

  if ModAmount > 0 then
  begin
    lInternalFrequency := lInternalFrequency + Modifier^ * log_approx(ModAmount);
  end;

  if lInternalFrequency > 0.999 then lInternalFrequency := 0.999;
  if lInternalFrequency < 0.001 then lInternalFrequency := 0.001;
  if lInternalResonance > 0.999 then lInternalResonance := 0.999;
  if lInternalResonance < 0.1 then lInternalResonance := 0.1;

  case FFilter.FilterType of
    ftLowpass, ftHighpass, ftBandpass, ftBandreject:
    begin
      // filter coeffs
      q := 1 - lInternalFrequency;
      p := lInternalFrequency + 0.8 * lInternalFrequency * q;
      f := p + p - 1;
      q := 0.89 * lInternalResonance * (1 + 0.5 * q * (1 - q + 5.6 * q * q));
    end;
    ftMoog:
    begin
      FLP24DB.Frequency := lInternalFrequency;
      FLP24DB.Resonance := lInternalResonance;
    end;
    ft303:
    begin
      FDiodeLadderFilter.set_frequency(lInternalFrequency);
      FDiodeLadderFilter.set_q(lInternalResonance);
    end;
    ftTransistorLadder:
    begin
      FTransistorLadderFilter.set_frequency(lInternalFrequency);
      FTransistorLadderFilter.set_q(lInternalResonance);
    end;
  end;
end;

procedure TDspFilter.Initialize;
begin
  inherited Initialize;

  case FFilter.FilterType of
    ftLowpass, ftHighpass, ftBandpass, ftBandreject:
    begin
      b0 := 0;
      b1 := 0;
      b2 := 0;
      b3 := 0;
      b4 := 0;
      t1 := 0;
      t2 := 0;
      t3 := 0;
      f := 0;
      p := 0;
      q := 0;
    end;
    ftMoog:
    begin
      FLP24DB.Initialize;
      {y1 := 0;
      y2 := 0;
      y3 := 0;
      y4 := 0;
      oldx := 0;
      oldy1 := 0;
      oldy2 := 0;
      oldy3 := 0;
      _kd := 1E-20; }
    end;
    ft303:
    begin
      FDiodeLadderFilter.Reset;
    end;
    ftTransistorLadder:
    begin
      FTransistorLadderFilter.Reset;
    end;
  end;

  Calc;
end;

{ TFilterEngine }

procedure TFilterEngine.SetFilterType(const AValue: TEnumFilterTypes);
begin
  if FFilterType = AValue then exit;
  FFilterType := AValue;

  Initialize;
end;

procedure TFilterEngine.Initialize;
begin
  //
end;

{ TBaseFilterTypeEngine }

procedure TBaseFilterTypeEngine.SetFilter(const AValue: TFilter);
begin
  if FFilter = AValue then exit;
  FFilter := AValue;

  Initialize;
end;

procedure TLP24DB.SetFrequency(AValue: Single);
begin
  if FFrequency = AValue then Exit;
  FFrequency := AValue;

  Calc;
end;

procedure TLP24DB.SetResonance(AValue: Single);
begin
  if FResonance = AValue then Exit;
  FResonance := AValue;

  Calc;
end;

procedure TLP24DB.Calc;
begin
  p := FFrequency * (1.8 - 0.8 * FFrequency);
  k := p + p - 1.0;
  t := (1.0 - p) * 1.386249;
  t2 := 12.0 + t * t;
  t3 := 6.0 * t;
  r := FResonance * (t2 + t3) / (t2 - t3);
end;

constructor TLP24DB.Create(AFrames: Integer);
begin
  inherited Create(AFrames);

  y1 := 0;
  y2 := 0;
  y3 := 0;
  y4 := 0;
  oldx := 0;
  oldy1 := 0;
  oldy2 := 0;
  oldy3 := 0;
  _kd := 1E-20;
end;

destructor TLP24DB.Destroy;
begin

  inherited Destroy;
end;


procedure TLP24DB.Initialize;
begin
  inherited Initialize;
  // Move some pre-calculation stuff from process to here
  // pre-calculation should only be done when changing parameters
  y1 := 0;
  y2 := 0;
  y3 := 0;
  y4 := 0;
  oldx := 0;
  oldy1 := 0;
  oldy2 := 0;
  oldy3 := 0;
  _kd := 1E-20;

  divbysamplerate := 1 / SampleRate;
end;

function TLP24DB.Process(I: Single): Single;
begin
  // Lowpass stage
  x := I - r * y4;
  y1:= x  * p + oldx * p - k * y1;
  y2:= y1 * p + oldy1 * p - k * y2;
  y3:= y2 * p + oldy2 * p - k * y3;
  y4:= y3 * p + oldy3 * p - k * y4;
  y4 := y4 - ((y4 * y4 * y4) * divby6);
  oldx := x;
  oldy1 := y1 +_kd;
  oldy2 := y2 +_kd;
  oldy3 := y3 +_kd;
  Result := y4;
end;

procedure TFilter.SetFilterType(const AValue: TEnumFilterTypes);
begin
  if FFilterType = AValue then exit;
  FFilterType := AValue;
end;

procedure TFilter.SetFrequency(AValue: Single);
begin
  if FFrequency = AValue then exit;
  FFrequency := AValue;

  FFrequencyInternal := log_approx(AValue);
end;

procedure TFilter.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then exit;
  FActive := AValue;
end;

constructor TFilter.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  inherited Create(AObjectOwner, AMapped);

  Resonance := 0;
  Frequency := 1;
end;

destructor TFilter.Destroy;
begin
  FSmoothCutoff.Free;

  inherited Destroy;
end;

procedure TFilter.Initialize;
begin
  Active := True;

  FFilterType := ftLowpass;

  Notify;
end;

procedure TFilter.Finalize;
begin
  //
end;

initialization

finalization

end.

