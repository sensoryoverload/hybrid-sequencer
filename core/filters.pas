unit filters;

{$mode objfpc}{$H+}

{$fputype SSE}

interface

uses
  Classes, SysUtils, globalconst, baseengine;

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

type
  TBaseFilterType = class(THybridPersistentModel)

  end;

  TEnumFilterTypes = (ftMoog1, ftMoog2, ftRBJ, ft24DB);

  { TFilter }

  TFilter = class(THybridPersistentModel)
  private
    FFrequency: Single;
    FResonance: Single;
    FFreqModSource: TModSource;
    FFreqModAmount: Single;
    FResoModSource: TModSource;
    FResoModAMount: Single;
    FFilterType: TEnumFilterTypes;
    FActive: Boolean;
    procedure SetActive(const AValue: Boolean);
    procedure SetFilterType(const AValue: TEnumFilterTypes);
  protected
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    procedure Initialize; override;
    procedure Finalize; override;
  published
    property Frequency: Single read FFrequency write FFrequency; // 0..20000
    property Resonance: Single read FResonance write FResonance; // 0..1 ?
    property FreqModSource: TModSource read FFreqModSource write FFreqModSource;
    property FreqModAmount: single read FFreqModAmount write FFreqModAmount;
    property ResoModSource: TModSource read FResoModSource write FResoModSource;
    property ResoModAmount: single read FResoModAmount write FResoModAmount;
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
    //FFilter: TBaseFilter;
    //FLevel: single;
    t, t2, t3, x, f, k, p, r, y1, y2, y3, y4, oldx, oldy1, oldy2, oldy3,  divbysamplerate: Single;
    _kd: Single;
    procedure SetFrequency(AValue: Single);
    procedure SetResonance(AValue: Single);
    procedure Calc;
  public
    constructor Create(AFrames: Integer); override;
    function Process(I: Single): Single; override;
    procedure Initialize; override;
    property Frequency: Single read FFrequency write SetFrequency; // 0..20000
    property Resonance: Single read FResonance write SetResonance; // 0..1 ?
  end;

implementation

uses
  fx;

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
  if Frequency > 20000 then Frequency := 20000;
  if Frequency < 30 then Frequency := 30;

  f := (Frequency + Frequency) * divbysamplerate;
  p := f * (1.8 - 0.8 * f);
  k := p + p - 1.0;
  t := (1.0 - p) * 1.386249;
  t2 := 12.0 + t * t;
  t3 := 6.0 * t;
  r := Resonance * (t2 + t3) / (t2 - t3);
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
  // Keep between valid range!
  if i > 1 then i := 1;
  if i < -1 then i := -1;

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

procedure TFilter.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then exit;
  FActive := AValue;
end;

constructor TFilter.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  inherited Create(AObjectOwner, AMapped);

  Resonance := 0.5;
  Frequency := 1000;
end;

procedure TFilter.Initialize;
begin
  Active := True;

  Notify;
end;

procedure TFilter.Finalize;
begin
  //
end;

initialization

finalization

end.

