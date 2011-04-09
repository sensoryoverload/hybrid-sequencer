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

  sampler.pas
}

unit sampler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, globalconst, jacktypes, ContNrs, global_command, global, midi,
  sndfile, jack, plugin, midiport, samplestreamprovider;

type
  TModSource = (
    msLFO1,
    msLFO2,
    msLFO3,
    msEnvelope1,
    msEnvelope2,
    msEnvelopeFollower,
    msOscillator1,
    msOscillator2,
    msOscillatorMix,
    msFilterOutput,
    msAmpOutput,
    msVelocity,
    msNote);

  { TBaseEngine - A really simple base class }

  TBaseEngine = class
  private
    FSampleRate: single;
  public
    constructor Create; virtual;
    procedure Initialize; virtual;
  end;

  { TEnvelope }

  TEnvelope = class(THybridPersistentModel)
  private
    // Time in seconds to reach value 1 from 0 after NOTE ON
    FAttack: single;
    // Time in seconds to reach sustain level from 1
    FDecay: single;
    // Sustain level between 0 and 1
    FSustain: single;
    // Time in seconds for the level to drop to 0 after NOTE OFF
    FRelease: single;
    // Loop envelope active state (ie LFO'ish)
    FLoopActive: Boolean;
  public
    procedure Initialize; override;
  published
    property Attack: single read FAttack write FAttack;
    property Decay: single read FDecay write FDecay;
    property Sustain: single read FSustain write FSustain;
    property Release: single read FRelease write FRelease;
    property LoopActive: Boolean read FLoopActive write FLoopActive;
  end;

  TEnvelopeState = (esStart, esAttack, esDecay, esSustain, esRelease, esEnd);
  TEnvelopeNoteEvent = (nsNone, nsNoteOn, nsNoteOff);

  { TEnvelopeEngine }

  TEnvelopeEngine = class(TBaseEngine)
  private
    FState: TEnvelopeState;
    FAdder: single;
    FLevel: single;
    FEnvelope: TEnvelope;
    procedure SetEnvelope(const AValue: TEnvelope);
  public
    procedure Initialize; override;
    procedure Process;
    procedure NoteOn;
    procedure NoteOff;
    property Envelope: TEnvelope read FEnvelope write SetEnvelope;
    property Level: single read FLevel;
    property State: TEnvelopeState read FState write FState;
  end;

  TLFOWaveform = (lwSin, lwSaw, lwTri, lwSqr);

  { TLFO }

  TLFO = class(THybridPersistentModel)
  private
    // Width of the phase before bringing down the level to 0, defaults to 50 (halfcycle)
    FPhaseWidth: Integer;
    // Each cycle in Hz ie 0.5Hz / 500Hz
    FRate: Single;
    // waveform types
    FWaveform: TLFOWaveform;
    // Time in seconds to reach full strength at level 1
    FAttack: Integer;
    // Starting point 0..99
    FPhase: Integer;

    FKeyDepth: Single;
    FActive: Boolean;

    FModSource: TModSource;
    FModAmount: single;
  public
    procedure Initialize; override;
  published
    property Rate: Single read FRate write FRate;
    property Waveform: TLFOWaveform read FWaveform write FWaveform;
    property Attack: Integer read FAttack write FAttack;
    property Phase: Integer read FPhase write FPhase;
    property PhaseWidth: Integer read FPhaseWidth write FPhaseWidth;
    property KeyDepth: Single read FKeyDepth write FKeyDepth;
    property Active: Boolean read FActive write FActive;

    property ModSource: TModSource read FModSource write FModSource;
    property ModAmount: single read FModAmount write FModAmount;
  end;

  TLFOState = (lsIdle, lsRunning);
  TLFODirection = (ldUp, ldDown);

  { TLFOEngine }

  TLFOEngine = class(TBaseEngine)
  private
    FState: TLFOState;
    FAdder: single;
    FLevel: single;
    FLFO: TLFO;
    FOldWaveForm: TLFOWaveform;
    FPosition: single;
    FPositionAdder: single;
    procedure SetLFO(const AValue: TLFO);
  public
    constructor Create; override;
    procedure Initialize; override;
    procedure Process;
    procedure Sync;
    property LFO: TLFO read FLFO write SetLFO;
    property Level: single read FLevel;
    property State: TLFOState read FState write FState;
  end;

  { TFilter }

  TFilter = class(THybridPersistentModel)
  private
    FFrequency: single;
    FModAmount: single;
    FModSource: TModSource;
    FSampleRate: single;
    FResonance: single;
    procedure SetFrequency(AValue: single);
    procedure SetSampleRate(AValue: single);
    procedure SetResonance(AValue: single);
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    procedure Initialize; override;
  published
    property Frequency: single read FFrequency write SetFrequency;
    property SampleRate: single read FSampleRate write SetSampleRate;
    property Resonance: single read FResonance write SetResonance;

    property ModSource: TModSource read FModSource write FModSource;
    property ModAmount: single read FModAmount write FModAmount;
  end;


  { TFilterEngine }

const
  i2      : Double = 40000;
  i2v     : Double = 1/20000;
  noise   : Double = 1E-10;
  noi     : Double = 1E-10*((1.0/$10000) / $10000);  // 2^-32
  mTwo    : Single = -2;
  c3      : Single =  3;
  c6      : Single =  6;
  c12     : Single = 12;
  c24     : Single = 24;

type
  TFilterEngine = class(TBaseEngine)
  private
    FA   : array[1..5] of single;
    FFilter: TFilter;
    FOld : single;
    F2vg : single;
    FAcr : single;
    FIpi : Double;
    procedure FreqCalc;
    procedure SetFilter(const AValue: TFilter);
  public
    constructor Create;
    function Process(const I : Single):Single;
    procedure Initialize; override;
    property Filter: TFilter read FFilter write SetFilter;
  end;

  { TEnvelopeFollower }

  TEnvelopeFollower = class(THybridPersistentModel)
  private
    FAttack: single;
    FModAmount: single;
    FModSource: TModSource;
    FRelease: single;
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    procedure Initialize; override;
  published
    property Attack: single read FAttack write FAttack;
    property Release: single read FRelease write FRelease;

    property ModSource: TModSource read FModSource write FModSource;
    property ModAmount: single read FModAmount write FModAmount;
  end;

  { TEnvelopeFollowerEngine }

  TEnvelopeFollowerEngine = class(TBaseEngine)
  private
    FRel: single;
    FAtt: single;
    FEnvIn: single;
    FEnvOut: single;

    FEnvelopeFollower: TEnvelopeFollower;
    procedure SetEnvelopeFollower(const AValue: TEnvelopeFollower);
  public
    function Process(const I : Single):Single;
    procedure Initialize; override;
    property EnvelopeFollower: TEnvelopeFollower read FEnvelopeFollower write SetEnvelopeFollower;
  end;

  { TAmplifier }

  TAmplifier = class(THybridPersistentModel)
  private
    // Adjust level -24dB .. +24dB
    FAmplify: single;

    FModAmount: single;
    FModSource: TModSource;
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    procedure Initialize; override;
  published
    property Amplify: single read FAmplify write FAmplify;
    property ModSource: TModSource read FModSource write FModSource;
    property ModAmount: single read FModAmount write FModAmount;
  end;

  { TAmplifierEngine }

  TAmplifierEngine = class(TBaseEngine)
  private
    FAmplifier: TAmplifier;
    procedure SetAmplifier(const AValue: TAmplifier);
  public
    function Process(const I : Single):Single;
    procedure Initialize; override;
    property Amplifier: TAmplifier read FAmplifier write SetAmplifier;
  end;

const
  k1Div24lowerBits = 1 / (1 shl 24);

  WFStrings: array[0..4] of string = ('triangle','sinus', 'sawtooth', 'square', 'exponent');

type
  TOscWaveform = (triangle, sinus, sawtooth, square, exponent);

  { TOscillator }

  TOscillator = class(THybridPersistentModel)
  private
    FStartPhase: dword;
    FWaveForm: TOscWaveform;
    procedure SetStartPhase(const AValue: dword);
    procedure SetWaveForm(const Value: TOscWaveform);
  public
    function WaveformName:String;
    property WaveForm: TOscWaveform read FWaveForm write SetWaveForm;
    property StartPhase: dword read FStartPhase write SetStartPhase;

  end;

  { TOscillatorEngine }

  TOscillatorEngine = class(TBaseEngine)
  private
    FRate: single;
    FTable: array[0..256] of Single; // 1 more for linear interpolation
    FPhase,
    FInc: dword;
    FOscillator: TOscillator;

    procedure SetOscillator(const AValue: TOscillator);
    procedure CalculateWaveform(AWaveform: TOscWaveform);
    procedure SetRate(const AValue: single);
  public
    // increments the phase and outputs the new LFO value.
    // return the new LFO value between [-1;+1]
    function Process:Single;
    procedure Initialize; override;
    procedure Sync;
    property Rate: single read FRate write SetRate;
    property Oscillator: TOscillator read FOscillator write SetOscillator;
  end;

  { TSample }

  TSample = class(THybridPersistentModel)
  private
    FWaveData: pjack_default_audio_sample_t;
    FSampleLocation: string;
    FSampleStart: Integer;
    FSampleEnd: Integer;
    FSampleReverse: Boolean;
    FSampleChannels: Integer;
    FSampleLevel: Single;
    FSamplePan: Single;

    FOsc1: TOscillator;
    FOsc2: TOscillator;
    FOsc3: TOscillator;

    FPitchEnvelope: TEnvelope;
    FPitchEnvelopeDepth: Single;
    FPitchLFODepth: Single;
    FPitchKeyDepth: Single;
    FPitchSemiTone: Single;
    FPitchDetune: Single;

    FAmpEnvelope: TEnvelope;
    FAmpEnvelopeDepth: Single;
    FAmpLFODepth: Single;
    FAmpKeyDepth: Single;
    FAmpOn: Boolean;

    FSaturateDrive: Single;
    FSaturateOn: Boolean;

    FFilter: TFilter;
    FFilterEnvelope: TEnvelope;
    FFilterEnvelopeDepth: Single;
    FFilterLFODepth: Single;
    FFilterKeyDepth: Single;
    FFilterOn: Boolean;
    FFilterCutoff: Single;
    FFilterResonance: Single;

    FLFO1: TLFO;
    FLFO2: TLFO;
    FLFO3: TLFO;

    FKey: Integer;
    FVoiceCount: Integer; // The number of voice instances there should be
    FSelected: Boolean;

    { Internal variables}
    FBufferData: PSingle;
    FBufferDataSize: Integer;
    FWave: TWaveFile;
    FPatternLength: Integer;
    FSampleInfo: SF_INFO;
    FSampleHandle: PSndFile;
    lData: PSingle;

    FSampleName: string;

    FPitchScaleFactor: Single;
    procedure SetPitchDetune(const AValue: Single);
    procedure SetPitchSemiTone(const AValue: Single);
  protected
    procedure RecalculatePitchFactor;
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Initialize; override;
    function LoadSample(AFileName: string): boolean;
    property PitchScaleFactor: Single read FPitchScaleFactor;

    procedure UnloadSample;
    property WaveData: pjack_default_audio_sample_t read FWaveData write FWaveData;
    property Wave: TWaveFile read FWave write FWave;
  published
    property SampleStart: Integer read FSampleStart write FSampleStart;
    property SampleEnd: Integer read FSampleEnd write FSampleEnd;
    property SampleReverse: Boolean read FSampleReverse write FSampleReverse;
    property SampleChannels: Integer read FSampleChannels write FSampleChannels;
    property SampleLevel: Single read FSampleLevel write FSampleLevel;
    property SamplePan: Single read FSamplePan write FSamplePan;

    property Osc1: TOscillator read FOsc1 write FOsc1;
    property Osc2: TOscillator read FOsc2 write FOsc2;
    property Osc3: TOscillator read FOsc3 write FOsc3;
    property PitchEnvelope: TEnvelope read FPitchEnvelope write FPitchEnvelope;
    property PitchEnvelopeDepth: Single read FPitchEnvelopeDepth write FPitchEnvelopeDepth;
    property PitchLFODepth: Single read FPitchLFODepth write FPitchLFODepth;
    property PitchKeyDepth: Single read FPitchKeyDepth write FPitchKeyDepth;
    property PitchSemiTone: Single read FPitchSemiTone write SetPitchSemiTone;
    property PitchDetune: Single read FPitchDetune write SetPitchDetune;

    property AmpEnvelope: TEnvelope read FAmpEnvelope write FAmpEnvelope;
    property AmpEnvelopeDepth: Single read FAmpEnvelopeDepth write FAmpEnvelopeDepth;
    property AmpLFODepth: Single read FAmpLFODepth write FAmpLFODepth;
    property AmpKeyDepth: Single read FAmpKeyDepth write FAmpKeyDepth;
    property AmpOn: Boolean read FAmpOn write FAmpOn;

    property SaturateDrive: Single read FSaturateDrive write FSaturateDrive;
    property SaturateOn: Boolean read FSaturateOn write FSaturateOn;

    property FilterEnvelope: TEnvelope read FFilterEnvelope write FFilterEnvelope;
    property FilterEnvelopeDepth: Single read FFilterEnvelopeDepth write FFilterEnvelopeDepth;
    property FilterLFODepth: Single read FFilterLFODepth write FFilterLFODepth;
    property FilterKeyDepth: Single read FFilterKeyDepth write FFilterKeyDepth;
    property FilterOn: Boolean read FFilterOn write FFilterOn;
    property FilterCutoff: Single read FFilterCutoff write FFilterCutoff;
    property FilterResonance: Single read FFilterResonance write FFilterResonance;

    property LFO1: TLFO read FLFO1 write FLFO1;
    property LFO2: TLFO read FLFO2 write FLFO2;
    property LFO3: TLFO read FLFO3 write FLFO3;

    property Key: Integer read FKey write FKey;
    property VoiceCount: Integer read FVoiceCount write FVoiceCount;
    property Selected: Boolean read FSelected write FSelected;

    property SampleName: string read FSampleName write FSampleName;
    property SampleLocation: string read FSampleLocation write FSampleLocation;
  end;

  { TSampleBank }

  TSampleBank = class(THybridPersistentModel)
  private
    FBankName: string;
    FMidiChannel: Integer; // Needed??
    FSampleList: TObjectList; // type TSampleEngine
    FSelected: Boolean;
    FSelectedSample: TSample;
    procedure AssignBank(Source: TSampleBank);
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer);
    procedure Assign(Source:TPersistent); override;
    property Selected: Boolean read FSelected write FSelected;
    property SelectedSample: TSample read FSelectedSample write FSelectedSample;
  published
    property SampleList: TObjectList read FSampleList write FSampleList;
    property BankName: string read FBankName write FBankName;
    property MidiChannel: Integer read FMidiChannel write FMidiChannel;
  end;



  { TPluginSampleBank }

  TPluginSampleBank = class(TInternalPlugin)
  private
  public
    constructor Create(AObjectOwnerID: string);
    procedure Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer); override;
  end;

  { * Manages Banks, Samples * }

  { TSampler }

  TSampler = class(THybridPersistentModel)
  private
    FBankList: TObjectList;

    FSelectedBank: TSampleBank;
    FTestSignal: Single;
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Assign(Source: TPersistent); override;
    function Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer): Integer;
    property SelectedBank: TSampleBank read FSelectedBank write FSelectedBank;
  published
    property BankList: TObjectList read FBankList write FBankList;
  end;

  { TSampleCommand }

  TSampleCommand = class(TCommand)
  private
    FSample: TSample;
    FSampleBank: TSampleBank;
  protected
    procedure Initialize; override;
  end;

  { TFilterCutoffCommand }

  TFilterCutoffCommand = class(TSampleCommand)
  private
    FOldCutoffValue: Single;
    FCutoffValue: Single;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property CutoffValue: Single read FCutoffValue write FCutoffValue;
  end;

  { TSampleBankCommand }

  TSampleBankCommand = class(TCommand)
  private
    FSampleBank: TSampleBank;
  protected
    procedure Initialize; override;
  end;

  { TSelectSampleCommand }

  TSelectSampleCommand = class(TSampleBankCommand)
  private
    FOldSelectedID: string;
    FSelectedID: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property SelectedID: string read FSelectedID write FSelectedID;
  end;

  { TSamplerCommand }

  TSamplerCommand = class(TCommand)
  private
    FSampler: TSampler;
  protected
    procedure Initialize; override;
  end;

  { TChangeSelectedBankCommand }

  TChangeSelectedBankCommand = class(TSamplerCommand)
  private
    FSelectedObjectID: string;
    FOldSelectedObjectID: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property SelectedObjectID: string read FSelectedObjectID write FSelectedObjectID;
  end;

  { TDeleteBankCommand }

  TDeleteBankCommand = class(TSamplerCommand)
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  end;

  { TCreateBankCommand }

  TCreateBankCommand = class(TSamplerCommand)
  private
    FOldObjectID: string;
    FBankName: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property BankName: string read FBankName write FBankName;
  end;

  { TDeleteSampleCommand }

  TDeleteSampleCommand = class(TSampleBankCommand)
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  end;

  { TCreateSampleCommand }

  TCreateSampleCommand = class(TSampleBankCommand)
  private
    FOldObjectID: string;
    FSampleLocation: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property SampleLocation: string read FSampleLocation write FSampleLocation;
  end;

  {
    All the classes, methods and variables to actually render the audio are located
    here. Things like the current playing position in a sample. Classes do not descent
    from TPersistent as they are never stored, just instantiated on the heap.
  }

  { TSampleVoice - Internal playing voice for a TSample structure }

  { TSampleVoiceEngine }

  TSampleVoiceEngine = class(TBaseEngine)
  private
    FSample: TSample;
    FSamplePosition: single;
    FFilterEnvelope: TEnvelopeEngine;
    FFilterEnvelopeLevel: single;

    FAmpEnvelope: TEnvelopeEngine;
    FAmpEnvelopeLevel: single;

    FPitchEnvelope: TEnvelopeEngine;
    FPitchEnvelopeLevel: single;

    FOsc1: TOscillatorEngine;
    FOsc2: TOscillatorEngine;
    FOsc3: TOscillatorEngine;

    FLFO1: TLFOEngine;
    FLFO2: TLFOEngine;
    FLFO3: TLFOEngine;

    FLFOPhase: single;
    FNote: Integer;
    FRunning: Boolean;
    FNoteOnOffset: Integer;

    // Start the NOTE OFF phase of a note; handle note release ADSR etc
    FStopVoice: Boolean;

    procedure SetSample(const AValue: TSample);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer);
    procedure NoteOn(ANote: Integer; ARelativeLocation: Integer);
    procedure NoteOff;

    function RunningNote: Integer;
    property Running: Boolean read FRunning write FRunning;
    property Note: Integer read FNote write FNote;
  published
    property Sample: TSample read FSample write SetSample;
  end;

  { TSampleEngine }

  TSampleEngine = class(TBaseEngine)
  private
    FSample: TSample;
    FSampleVoiceEngineList: TObjectList;
    FMidiDataList: TMidiDataList;
    FLastMidiDataIndex: Integer;
    FInternalMidiIndex: Integer;
    FMidiStartIndex: Integer;
    FMidiEndIndex: Integer;

    FFrameOffsetLow: Integer;
    FFrameOffsetHigh: Integer;

    FOldRealCursorPosition: Integer;

    function GetMidiEvent: TMidiData;
    procedure SetSample(const AValue: TSample);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer);

    property Sample: TSample read FSample write SetSample;
  end;

  { TSampleBankEngine }

  TSampleBankEngine = class(TBaseEngine)
  private
    FSampleBank: TSampleBank;
    FSampleEngineList: TObjectList;
    FMidiDataList: TMidiDataList;

    procedure SetSampleBank(const AValue: TSampleBank);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer);

    property SampleBank: TSampleBank read FSampleBank write SetSampleBank;
  end;

  { TSamplerEngine }

  TSamplerEngine = class(TBaseEngine)
  private
    FSampler: TSampler;
    FSampleBankEngineList: TObjectList;

    procedure SetSampler(const AValue: TSampler);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Process(ATrackList: TObjectList; ABuffer: PSingle; AFrames: Integer);

    property Sampler: TSampler read FSampler write SetSampler;
  end;

implementation

uses
  audiostructure, utils, fx;

{ TBaseEngine }

constructor TBaseEngine.Create;
begin
  writeln(Format('Creating class: %s', [Self.ClassName]));

{  // Descendants initalize samplerate at constructor call
  Initialize;}
end;

{
  Retrieve samplerate setting
}
procedure TBaseEngine.Initialize;
begin
  writeln(Format('Initializing class: %s', [Self.ClassName]));

  FSamplerate := GSettings.SampleRate;
end;

{ TOscillatorEngine }

procedure TOscillatorEngine.SetOscillator(const AValue: TOscillator);
begin
  if FOscillator = AValue then exit;
  FOscillator := AValue;

  Initialize;
end;

function TOscillatorEngine.Process: Single;
var
  i: integer;
  frac: Single;
begin
  // the 8 MSB are the index in the table in the range 0-255
  i := Fphase shr 24;
  // and the 24 LSB are the fractionnal part
  frac := (Fphase and $00FFFFFF) * k1Div24lowerBits;
  // increment the phase for the next tick
  Fphase := FPhase + Finc; // the phase overflows itself
  Result := Ftable[i] * (1-frac) + Ftable[i+1] * frac; // linear interpolation
end;


procedure TOscillatorEngine.Initialize;
begin
  inherited Initialize;

  FPhase := 0;
  CalculateWaveform(sawtooth);
  Rate := 1000;
end;

procedure TOscillatorEngine.CalculateWaveform(AWaveform: TOscWaveform);
var
  i: Integer;
begin
  case AWaveform of
    sinus:
    begin
      for i:=0 to 256 do
      begin
        FTable[i] := sin(2*pi*(i/256));
      end;
    end;
    triangle:
    begin
      for i:=0 to 63 do
      begin
        FTable[i] := i / 64;
        FTable[i+64] :=(64-i) / 64;
        FTable[i+128] := - i / 64;
        FTable[i+192] := - (64-i) / 64;
      end;
      FTable[256] := 0;
    end;
    sawtooth:
    begin
      for i:=0 to 255 do
      begin
        FTable[i] := 2*(i/255) - 1;
      end;
      FTable[256] := -1;
    end;
    square:
    begin
      for i := 0 to 127 do
      begin
        FTable[i]     :=  1;
        FTable[i+128] := -1;
      end;
      FTable[256] := 1;
    end;
    exponent:
    begin
      // symetric exponent similar to triangle
      for i:=0 to 127 do
      begin
        FTable[i] := 2 * ((exp(i/128) - 1) / (exp(1) - 1)) - 1  ;
        FTable[i+128] := 2 * ((exp((128-i)/128) - 1) / (exp(1) - 1)) - 1  ;
      end;
      FTable[256] := -1;
    end;
  end;
end;

procedure TOscillatorEngine.SetRate(const AValue: single);
begin
  GLogger.PushMessage(Format('Note value: %f', [AValue]));

  if FRate = AValue then exit;
  FRate := AValue;
  // the rate in Hz is converted to a phase increment with the following formula
  // f[ inc = (256*rate/samplerate) * 2^24]
  Finc := round((256 * Frate / Fsamplerate) * (1 shl 24));
end;

procedure TOscillatorEngine.Sync;
begin
  FPhase := 0;
end;

{ TOscillator }

procedure TOscillator.SetStartPhase(const AValue: dword);
begin
  if FStartPhase = AValue then exit;
  FStartPhase := AValue;
end;

procedure TOscillator.SetWaveForm(const Value: TOscWaveForm);
var
  i: integer;
begin
  FWaveForm := Value;
end;

function TOscillator.WaveformName: String;
begin
  result := WFStrings[Ord(Fwaveform)];
end;

{ TAmplifierEngine }

procedure TAmplifierEngine.SetAmplifier(const AValue: TAmplifier);
begin
  if FAmplifier = AValue then exit;
  FAmplifier := AValue;

  Initialize;
end;

function TAmplifierEngine.Process(const I: Single): Single;
begin
  Result := I;
end;

procedure TAmplifierEngine.Initialize;
begin
  inherited Initialize;
end;

{ TAmplifier }

constructor TAmplifier.Create(AObjectOwner: string; AMapped: Boolean);
begin
  inherited Create(AObjectOwner, AMapped);

end;

procedure TAmplifier.Initialize;
begin
  Notify;
end;

{ TEnvelopeFollowerEngine }

procedure TEnvelopeFollowerEngine.SetEnvelopeFollower(
  const AValue: TEnvelopeFollower);
begin
  if FEnvelopeFollower = AValue then exit;
  FEnvelopeFollower := AValue;

  Initialize;
end;

function TEnvelopeFollowerEngine.Process(const I: Single): Single;
begin
  // get your data into 'input'
  FEnvIn := abs(I);

  if FEnvOut < FEnvIn then
  begin
    FEnvOut := FEnvIn + FAtt * (FEnvOut - FEnvIn);
  end
  else
  begin
    FEnvOut := FEnvIn + FRel * (FEnvOut - FEnvIn);
  end;

  // envOut now contains the envelope
  Result := FEnvOut;
end;

procedure TEnvelopeFollowerEngine.Initialize;
begin
  inherited Initialize;

  // Attack and Release is in seconds
  FAtt := exp(-1.0 / (FSampleRate * FEnvelopeFollower.Attack));
  FRel := exp(-1.0 / (FSampleRate * FEnvelopeFollower.Release));

  FEnvOut := 0.0;
end;

{ TEnvelopeFollower }

constructor TEnvelopeFollower.Create(AObjectOwner: string; AMapped: Boolean);
begin
  inherited Create(AObjectOwner, AMapped);

  //
end;

procedure TEnvelopeFollower.Initialize;
begin
  FAttack := 0;
  FRelease := 1;
end;

{ TLFOEngine }

procedure TLFOEngine.SetLFO(const AValue: TLFO);
begin
  if FLFO = AValue then exit;
  FLFO := AValue;

  Initialize;
end;

constructor TLFOEngine.Create;
begin
  inherited Create;

end;

procedure TLFOEngine.Initialize;
begin
  inherited Initialize;

  case FLFO.Waveform of
  lwSaw:
    begin
      FLevel := 1;
      FAdder := FLFO.Rate / FSampleRate;
    end;
  lwSin: // TODO this is a tri not a sin!
    begin
      FLevel := 1;
      FAdder := (FLFO.Rate / FSampleRate) * 2;
    end;
  lwSqr:
    begin
      FPositionAdder := (FLFO.Rate / FSampleRate);
    end;
  lwTri:
    begin
      FPositionAdder := (FLFO.Rate / FSampleRate);
    end;
  end;
end;

procedure TLFOEngine.Process;
begin
  case FLFO.Waveform of
  lwSaw:
    begin
      FLevel := FLevel - FAdder;
      if FLevel < 0 then
      begin
        FLevel := 1;
      end;
    end;
  lwSin:
    begin

    end;
  lwSqr:
    begin
      //if FPosition > ;
    end;
  lwTri:
    begin

    end;
  end;
end;

{
  Resynchronize to starting point
}
procedure TLFOEngine.Sync;
begin
  //
end;

{ TEnvelopeEngine }

procedure TEnvelopeEngine.SetEnvelope(const AValue: TEnvelope);
begin
  if FEnvelope = AValue then exit;
  FEnvelope := AValue;

  Initialize;
end;

procedure TEnvelopeEngine.Initialize;
begin
  inherited Initialize;

  FLevel := 0;
  FState := esStart;
end;

procedure TEnvelopeEngine.Process;
begin

  case FState of
  esStart:
    begin
      FAdder := 1 / (FSampleRate * FEnvelope.Attack);
      FState := esAttack;
    end;
  esAttack:
    begin
      FLevel := FLevel + FAdder;
      if FLevel >= 1 then
      begin
        FAdder := 1 / (FSampleRate * FEnvelope.Decay);
        FState := esDecay;
      end;
    end;
  esDecay:
    begin
      FLevel := FLevel - FAdder;
      if FLevel <= FEnvelope.Sustain then
      begin
        FAdder := 0;
        FState := esSustain;
      end;
    end;
  esSustain:
    begin
      // Do nothing, level is stable here
    end;
  esRelease:
    begin
      FLevel := FLevel - FAdder;
      if FLevel <= 0 then
      begin
        if FEnvelope.LoopActive then
        begin
          // Start again
          FState := esAttack;
        end
        else
        begin
          FState := esEnd;
        end;
      end;
      FAdder := 1 / (FSampleRate * FEnvelope.Decay);
    end;
  esEnd:
    begin
      // Stay here at the end, this should be a signal to put the voice engine
      // back in the voice pool
    end;
  end;
end;

procedure TEnvelopeEngine.NoteOn;
begin
  FAdder := 1 / (FSampleRate * FEnvelope.Attack);
  FState := esAttack;
end;

procedure TEnvelopeEngine.NoteOff;
begin
  FAdder := 1 / (FSampleRate * FEnvelope.Release);
  FState := esRelease;
end;

{ TSampler }

constructor TSampler.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  inherited Create(AObjectOwner, AMapped);

  FBankList := TObjectList.create(True);
end;

destructor TSampler.Destroy;
begin
  FBankList.Free;

  inherited Destroy;
end;

procedure TSampler.Initialize;
begin
  Notify;
end;

procedure TSampler.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

function TSampler.Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer): Integer;
var
  lBankIndex: Integer;
begin
  for lBankIndex := 0 to Pred(FBankList.Count) do
  begin
    TSampleBank(FBankList[lBankIndex]).Process(AMidiGrid, ABuffer, AFrames);
  end;
end;

{ TSample }

procedure TSample.SetPitchSemiTone(const AValue: Single);
begin
  FPitchSemiTone := AValue;

  RecalculatePitchFactor;
end;

procedure TSample.RecalculatePitchFactor;
begin
  FPitchScaleFactor := 1;
end;

procedure TSample.SetPitchDetune(const AValue: Single);
begin
  FPitchDetune := AValue;

  RecalculatePitchFactor;
end;

constructor TSample.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  inherited Create(AObjectOwner, AMapped);

  // Init oscillators
  FOsc1 := TOscillator.Create(ObjectID);
  FOsc2 := TOscillator.Create(ObjectID);
  FOsc3 := TOscillator.Create(ObjectID);

  // Init envelopes
  FAmpEnvelope := TEnvelope.Create(ObjectID);
  FFilterEnvelope := TEnvelope.Create(ObjectID);
  FPitchEnvelope := TEnvelope.Create(ObjectID);

  // Init LFO
  FLFO1 := TLFO.Create(ObjectID);
  FLFO2 := TLFO.Create(ObjectID);
  FLFO3 := TLFO.Create(ObjectID);

  FPitchSemiTone := 1;
  FPitchDetune := 0;

  FPitchScaleFactor := 1;

  // Default voice count (6 note polyphonic)
  FVoiceCount := 6;
end;

destructor TSample.Destroy;
begin
  FOsc1.Free;
  FOsc2.Free;
  FOsc3.Free;
  FAmpEnvelope.Free;
  FFilterEnvelope.Free;
  FPitchEnvelope.Free;
  FLFO1.Free;
  FLFO2.Free;
  FLFO3.Free;

  inherited Destroy;
end;

procedure TSample.Initialize;
begin
  Notify;
end;

function TSample.LoadSample(AFileName: string): boolean;
var
  lFilename: pchar;
  lChannelIndex: Integer;
  lChannelSize: Integer;
  lChannelItems: Integer;
  lBufferIndex: Integer;
  lBuffer: PSingle;
  i, j: Integer;
  SamplesRead: Integer;
  lValue: single;
begin
  writeln('start TSample.LoadSample');
  DBLog('start TSample.LoadSample');

  FWave := GSampleStreamProvider.LoadSample(AFileName);

  FSampleName := AFileName;
  FSampleLocation := ExtractFileName(AFileName);

  DBLog(Format('FWave.ChannelList.Count %d', [FWave.ChannelList.Count]));

  DBLog('end TSample.LoadSample');
end;

procedure TSample.UnloadSample;
begin
  if Assigned(FWave) then
  begin
    FWave.UnloadSample;
  end;
end;

{ TSampleBank }

procedure TSampleBank.AssignBank(Source: TSampleBank);
begin
  Self.ObjectID := Source.ObjectID;
  Self.ObjectOwnerID := Source.ObjectOwnerID;
end;

constructor TSampleBank.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  inherited Create(AObjectOwner, AMapped);

  FSampleList := TObjectList.create(True);
end;

destructor TSampleBank.Destroy;
begin
  FSampleList.Free;

  inherited Destroy;
end;

procedure TSampleBank.Initialize;
begin
  Notify;
end;

{
  Play all samples 'FSampleList' of the TSampleBank
}
procedure TSampleBank.Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer);
var
  buffer: ^byte;
  lFrameOffsetLow: Integer;
  lFrameOffsetHigh: Integer;
  lRelativeLocation: Integer;
  lIndex: Integer;
  lMidiData: TMidiData;
  lSampleIndex: Integer;
begin

  for lSampleIndex := 0 to Pred(FSampleList.Count) do
  begin
    TSampleEngine(FSampleList[lSampleIndex]).Process(AMidiGrid, ABuffer, AFrames);
  end;

end;

procedure TSampleBank.Assign(Source: TPersistent);
begin
  if Source is TSampleBank then
    AssignBank(Source as TSampleBank)
  else if Source = nil then
  begin
  end
  else
    inherited Assign(Source);
end;


{ TDeleteBankCommand }

procedure TDeleteBankCommand.DoExecute;
var
  lMementoBank: TSampleBank;
  lBank: TSampleBank;
  lSampler: TSampler;
  lBankIndex: Integer;
begin
  DBLog('start TDeleteBankCommand.DoExecute');

  FSampler.BeginUpdate;

  for lBankIndex := 0 to Pred(FSampler.BankList.Count) do
  begin
    lBank := TSampleBank(FSampler.BankList[lBankIndex]);

    if Assigned(lBank) then
    begin

      if lBank.Selected then
      begin

        DBLog('Deleting selected bank: %s', lBank.ObjectID);

        lMementoBank := TSampleBank.Create(ObjectOwner, NOT_MAPPED);
        lMementoBank.Assign(lBank);
        lMementoBank.ObjectOwnerID := ObjectOwner;
        Memento.Add(lMementoBank);

        FSampler.BankList.Remove(lBank);
      end;
    end;
  end;

  FSampler.EndUpdate;

  DBLog('end TDeleteBankCommand.DoExecute');
end;

procedure TDeleteBankCommand.DoRollback;
var
  lBank: TSampleBank;
  lMementoBank: TSampleBank;
  lMementoIndex: Integer;
begin
  DBLog('start TDeleteBankCommand.DoRollback');

  // First find object owner in sampler list
  for lMementoIndex := 0 to Pred(Memento.Count) do
  begin
    lMementoBank := TSampleBank(Memento[lMementoIndex]);
    lBank := TSampleBank.Create(ObjectOwner);
    lBank.Assign(lMementoBank);
    FSampler.BankList.Add(lBank);
  end;
  FSampler.Notify;

  DBLog('end TDeleteBankCommand.DoRollback');
end;

{ TCreateBankCommand }

procedure TCreateBankCommand.DoExecute;
var
  lBank: TSampleBank;
  lMementoBank: TSampleBank;
begin
  DBLog('start TCreateBankCommand.DoExecute');

  FSampler.BeginUpdate;

  // Create bank
  lBank := TSampleBank.Create(ObjectOwner);
  lBank.ObjectOwnerID := FSampler.ObjectID;
  lBank.BankName := FBankName;

  FSampler.BankList.Add(lBank);
  FSampler.SelectedBank := lBank;

  GAudioStruct.SelectedBank := lBank;

  // Create memento
  FOldObjectID := lBank.ObjectID;

  // update view
  FSampler.EndUpdate;

  DBLog('end TCreateBankCommand.DoExecute');
end;

procedure TCreateBankCommand.DoRollback;
var
  lMementoBank: TSampleBank;
  lMementoIndex: Integer;
  lBank: TSampleBank;
  lBankIndex: Integer;
begin
  DBLog('start TCreateBankCommand.DoRollback');

  // Search for bank where ObjectID same as Memento.ObjectID
  for lBankIndex := Pred(FSampler.BankList.Count) downto 0 do
  begin
    lBank := TSampleBank(FSampler.BankList[lBankIndex]);
    if lBank.ObjectID = FOldObjectID then
    begin
      DBLog('Found bank, deleting...');
      FSampler.BeginUpdate;
      FSampler.BankList.Remove(lBank);
      FSampler.EndUpdate;
      break;
    end;
  end;

  if FSampler.BankList.Count > 0 then
  begin
    GAudioStruct.SelectedBank := TSampleBank(FSampler.BankList[0]);
  end
  else
  begin
    GAudioStruct.SelectedBank := nil;
  end;


  DBLog('end TCreateBankCommand.DoRollback');
end;

{ TDeleteSampleCommand }

procedure TDeleteSampleCommand.DoExecute;
var
  lMementoSample: TSample;
  lSample: TSample;
  lSampleIndex: Integer;
  lBank: TSampleBank;
begin
  DBLog('start TDeleteSampleCommand.DoExecute');

  if Assigned(GAudioStruct.SelectedBank) then
  begin
    lBank := TSampleBank(GObjectMapper.GetModelObject(GAudioStruct.SelectedBank.ObjectID));

    if Assigned(lBank) then
    begin

      for lSampleIndex := 0 to Pred(lBank.SampleList.Count) do
      begin
        lSample := TSample(lBank.SampleList[lSampleIndex]);

        if lSample.Selected then
        begin
          lMementoSample := TSample.Create(lBank.ObjectID, NOT_MAPPED);
          lMementoSample.Assign(lSample);
          lMementoSample.ObjectOwnerID := ObjectOwner;
          lMementoSample.ObjectID := ObjectID;
          Memento.Add(lMementoSample);

          lBank.SampleList.Remove(lSample);
        end;
      end;
    end;
  end;

  DBLog('end TDeleteSampleCommand.DoExecute');
end;


procedure TDeleteSampleCommand.DoRollback;
var
  lSample: TSample;
  lMementoSample: TSample;
  lMementoIndex: Integer;
  lBank: TSampleBank;
begin
  DBLog('start TDeleteSampleCommand.DoRollback');

  // First find object owner in sampler list
  for lMementoIndex := 0 to Pred(Memento.Count) do
  begin
    lMementoSample := TSample(Memento[lMementoIndex]);

    lBank := TSampleBank(GObjectMapper.GetModelObject(lMementoSample.ObjectOwnerID));
    if Assigned(lBank) then
    begin
      lSample := TSample.Create(ObjectOwner, MAPPED);
      lSample.Assign(lMementoSample);

      lBank.SampleList.Add(lSample);
    end;
  end;

  DBLog('end TDeleteSampleCommand.DoRollback');
end;

{ TCreateSampleCommand }

procedure TCreateSampleCommand.DoExecute;
var
  lSample: TSample;
  lBank: TSampleBank;
begin
  DBLog('start TCreateSampleCommand.DoExecute');

  if Assigned(GAudioStruct.SelectedBank) then
  begin
    lBank := GAudioStruct.SelectedBank;

    if Assigned(lBank) then
    begin
      lBank.BeginUpdate;

      lSample := TSample.Create(lBank.ObjectID);
      lSample.LoadSample(SampleLocation);

      FOldObjectID := lSample.ObjectID;

      lBank.SampleList.Add(lSample);

      DBLog('Add sample to samplelist: %s', lSample.ObjectID);

      DBLog(Format('lBank.SampleList.Count %d', [lBank.SampleList.Count]));

      lBank.EndUpdate;
    end;
  end;

  DBLog('end TCreateSampleCommand.DoExecute');
end;

procedure TCreateSampleCommand.DoRollback;
var
  lSample: TSample;
  lSampleIndex: Integer;
  lBank: TSampleBank;
begin
  DBLog('start TCreateSampleCommand.DoRollback');

  lBank := TSampleBank(GObjectMapper.GetModelObject(ObjectOwner));

  if Assigned(lBank) then
  begin

    DBLog('Try finding sample...');
    // Search for sample where ObjectID same as Memento.ObjectID
    for lSampleIndex := Pred(lBank.SampleList.Count) downto 0 do
    begin
      lSample := TSample(lBank.SampleList[lSampleIndex]);
      if lSample.ObjectID = FOldObjectID then
      begin
        DBLog('Found sample, deleting...' + lSample.ObjectID);

        lBank.BeginUpdate;
        lBank.SampleList.Remove(lSample);
        lBank.EndUpdate;
        break;
      end;
    end;
  end;

  DBLog('end TCreateSampleCommand.DoRollback');
end;

{ TChangeSelectedBankCommand }

procedure TChangeSelectedBankCommand.DoExecute;
var
  lBankIndex: Integer;
begin
  DBLog('start TChangeSelectedBankCommand.DoExecute');

  FSampler.BeginUpdate;

  // Create memento
  FOldSelectedObjectID := GAudioStruct.SelectedBank.ObjectID;
  GAudioStruct.SelectedBank := TSampleBank(GObjectMapper.GetModelObject(SelectedObjectID));
  GAudioStruct.Sampler.SelectedBank := TSampleBank(GObjectMapper.GetModelObject(SelectedObjectID));

  for lBankIndex := 0 to Pred(FSampler.BankList.Count) do
  begin
    if Assigned(FSampler.BankList[lBankIndex]) then
    begin
      TSampleBank(FSampler.BankList[lBankIndex]).Selected :=
        (TSampleBank(FSampler.BankList[lBankIndex]).ObjectID = SelectedObjectID);
    end;
  end;

  // update view
  FSampler.EndUpdate;

  DBLog('end TChangeSelectedBankCommand.DoExecute');
end;



procedure TChangeSelectedBankCommand.DoRollback;
var
  lBank: TSampleBank;
  lBankIndex: Integer;
begin
  DBLog('start TChangeSelectedBankCommand.DoRollback');

  FSampler.BeginUpdate;

  GAudioStruct.SelectedBank := TSampleBank(GObjectMapper.GetModelObject(FOldSelectedObjectID));
  GAudioStruct.Sampler.SelectedBank := TSampleBank(GObjectMapper.GetModelObject(FOldSelectedObjectID));

  for lBankIndex := 0 to Pred(FSampler.BankList.Count) do
  begin
    if Assigned(FSampler.BankList[lBankIndex]) then
    begin
      TSampleBank(FSampler.BankList[lBankIndex]).Selected :=
        (TSampleBank(FSampler.BankList[lBankIndex]).ObjectID = FOldSelectedObjectID);
    end;
  end;

  FSampler.EndUpdate;

  DBLog('end TChangeSelectedBankCommand.DoRollback');
end;

{ TEnvelope }

procedure TEnvelope.Initialize;
begin
  Attack := 0.1;
  Decay := 1;
  Sustain := 1;
  Release := 1;

  Notify;
end;

{ TLFO }

procedure TLFO.Initialize;
begin
  Notify;
end;

{ TSampleBankCommand }

procedure TSampleBankCommand.Initialize;
begin
  FSampleBank := TSampleBank(GObjectMapper.GetModelObject(ObjectOwner));
end;

{ TSamplerCommand }

procedure TSamplerCommand.Initialize;
begin
  FSampler := TSampler(GObjectMapper.GetModelObject(ObjectOwner));
end;

{ TPluginSampleBank }

constructor TPluginSampleBank.Create(AObjectOwnerID: string);
begin
  //
end;

procedure TPluginSampleBank.Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer);
begin
  //
end;

{ TSampleCommand }

procedure TSampleCommand.Initialize;
begin
  FSampleBank := TSampleBank(GObjectMapper.GetModelObject(ObjectOwner));
  FSample := TSample(GObjectMapper.GetModelObject(ObjectID));
end;

{ TFilterCutoffCommand }

procedure TFilterCutoffCommand.DoExecute;
begin
  DBLog('start TFilterCutoffCommand.DoExecute');

  FSample.BeginUpdate;

  FOldCutoffValue := FSample.FilterCutoff;
  FSample.FilterCutoff := FCutoffValue;

  FSample.EndUpdate;

  DBLog('end TFilterCutoffCommand.DoExecute');
end;

procedure TFilterCutoffCommand.DoRollback;
begin
  DBLog('start TFilterCutoffCommand.DoRollback');

  FSample.BeginUpdate;

  FSample.FilterCutoff := FOldCutoffValue;

  FSample.EndUpdate;

  DBLog('end TFilterCutoffCommand.DoRollback');
end;

{ TSelectSampleCommand }

procedure TSelectSampleCommand.DoExecute;
var
  lSampleIndex: Integer;
begin
  FSampleBank.BeginUpdate;

  for lSampleIndex := 0 to Pred(FSampleBank.SampleList.Count) do
  begin
    TSample(FSampleBank.SampleList[lSampleIndex]).BeginUpdate;

    TSample(FSampleBank.SampleList[lSampleIndex]).Selected :=
      (TSample(FSampleBank.SampleList[lSampleIndex]).ObjectID = FSelectedID);

    TSample(FSampleBank.SampleList[lSampleIndex]).EndUpdate;
  end;

  FOldSelectedID := FSelectedID;

  FSampleBank.EndUpdate;
end;

procedure TSelectSampleCommand.DoRollback;
var
  lSampleIndex: Integer;
begin
  FSampleBank.BeginUpdate;

  for lSampleIndex := 0 to Pred(FSampleBank.SampleList.Count) do
  begin
    TSample(FSampleBank.SampleList[lSampleIndex]).BeginUpdate;

    TSample(FSampleBank.SampleList[lSampleIndex]).Selected :=
      (TSample(FSampleBank.SampleList[lSampleIndex]).ObjectID = FOldSelectedID);

    TSample(FSampleBank.SampleList[lSampleIndex]).EndUpdate;
  end;

  FSampleBank.EndUpdate;
end;

{ TSampleEngine }

function TSampleEngine.GetMidiEvent: TMidiData;
begin
  if FMidiDataList.Count > 0 then
  begin
    if FInternalMidiIndex < FMidiEndIndex then
    begin
      Result := TMidiData(FMidiDataList[FInternalMidiIndex]);
      Inc(FInternalMidiIndex);
    end
    else
    begin
      Result := nil;
    end;
  end
  else
  begin
    Result := nil;
  end;

end;

procedure TSampleEngine.SetSample(const AValue: TSample);
var
  lSampleVoice: TSampleVoiceEngine;
  lVoiceIndex: Integer;
begin
  FSample := AValue;

  // Set default number of voices for sample
  writeln(Format('Number of voices %d', [FSample.VoiceCount]));

  for lVoiceIndex := 0 to Pred(FSample.VoiceCount) do
  begin
    writeln(Format('voice %d', [lVoiceIndex]));
    lSampleVoice := TSampleVoiceEngine.Create;
    lSampleVoice.Sample := FSample;

    lSampleVoice.Initialize;

    FSampleVoiceEngineList.Add(lSampleVoice);

    writeln(Format('Add SampleVoiceEngine for %s, count %d',
      [lSampleVoice.Sample.SampleName, FSampleVoiceEngineList.Count]));
  end;
end;

constructor TSampleEngine.Create;
begin
  inherited Create;

  FInternalMidiIndex := 0;
  FOldRealCursorPosition := -1;
  FLastMidiDataIndex := 0;

  FSampleVoiceEngineList := TObjectList.create(True);
end;

destructor TSampleEngine.Destroy;
begin
  FSampleVoiceEngineList.Free;

  inherited Destroy;
end;

procedure TSampleEngine.Initialize;
begin
  inherited Initialize;

end;

procedure TSampleEngine.Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer);
var
  lVoiceIndex: Integer;
  lVoice: TSampleVoiceEngine;

  lMidiBuffer: TMidiBuffer;
  lMidiEvent: TMidiEvent;
  lMidiBufferIndex: Integer;
begin
  lMidiBuffer := AMidiGrid.MidiBuffer;
  lMidiBuffer.Seek(0);

  if lMidiBuffer.Count > 0 then
  begin
    for lMidiBufferIndex := 0 to Pred(lMidiBuffer.Count) do
    begin
      // Shortcut for current event in buffer
      lMidiEvent := lMidiBuffer.ReadEvent;

      if lMidiEvent.DataType = mtNoteOff then
      begin
        {
          Are there any running voices which are to receive a NOTE OFF?
        }
        for lVoiceIndex := 0 to Pred(FSampleVoiceEngineList.Count) do
        begin
          lVoice := TSampleVoiceEngine(FSampleVoiceEngineList[lVoiceIndex]);

          if lVoice.Running then
          begin
            if lMidiEvent.DataType = mtNoteOff then
            begin
              if lMidiEvent.DataValue1 = lVoice.Note then
              begin
                GLogger.PushMessage(Format('Note off: %d, Offset %d', [lMidiEvent.DataValue1, lMidiEvent.RelativeOffset]));

                lVoice.NoteOff;
                break;
              end;
            end;
          end;
        end;
      end
      else if lMidiEvent.DataType = mtNoteOn then
      begin
        {
          Are there any empty voice slots to receive NOTE ON's?
        }
        for lVoiceIndex := 0 to Pred(FSampleVoiceEngineList.Count) do
        begin
          lVoice := TSampleVoiceEngine(FSampleVoiceEngineList[lVoiceIndex]);

          if not lVoice.Running then
          begin
            GLogger.PushMessage(Format('Start Note: %d, Offset %d', [lMidiEvent.DataValue1, lMidiEvent.RelativeOffset]));

            // Start note at location 'RelativeOffset'
            lVoice.NoteOn(lMidiEvent.DataValue1, lMidiEvent.RelativeOffset);
            break;
          end;
        end;
      end
      else if lMidiEvent.DataType = mtCC then
      begin
        // TODO
      end
      else if lMidiEvent.DataType = mtVelocity then
      begin
        // TODO
      end;
    end;
  end;

  // Play all assigned voices
  for lVoiceIndex := 0 to Pred(FSampleVoiceEngineList.Count) do
  begin
    lVoice := TSampleVoiceEngine(FSampleVoiceEngineList[lVoiceIndex]);

    if lVoice.Running then
    begin
      lVoice.Process(AMidiGrid, ABuffer, AFrames);
    end;
  end;
end;

{ TSampleVoice - Initializes the voice with the newly assigned TSample }

procedure TSampleVoiceEngine.SetSample(const AValue: TSample);
begin
  FSample := AValue;
end;

constructor TSampleVoiceEngine.Create;
begin
  FOsc1 := TOscillatorEngine.Create;
  FOsc2 := TOscillatorEngine.Create;
  FOsc3 := TOscillatorEngine.Create;
  FFilterEnvelope := TEnvelopeEngine.Create;
  FAmpEnvelope := TEnvelopeEngine.Create;
  FPitchEnvelope := TEnvelopeEngine.Create;
  FLFO1 := TLFOEngine.Create;
  FLFO2 := TLFOEngine.Create;
  FLFO3 := TLFOEngine.Create;

  FLFOPhase := 0;
  FRunning := False;
  FSamplePosition := 0;
  FNote := -1;
  FNoteOnOffset := 0;
  FStopVoice := False;
end;

destructor TSampleVoiceEngine.Destroy;
begin
  FOsc1.Free;
  FOsc2.Free;
  FOsc3.Free;
  FFilterEnvelope.Free;
  FAmpEnvelope.Free;
  FPitchEnvelope.Free;
  FLFO1.Free;
  FLFO2.Free;
  FLFO3.Free;

  inherited Destroy;
end;

procedure TSampleVoiceEngine.Initialize;
begin
  inherited Initialize;

  if Assigned(FSample) then
  begin
    FOsc1.Oscillator := FSample.Osc1;
    FOsc2.Oscillator := FSample.Osc2;
    FOsc3.Oscillator := FSample.Osc3;
    FFilterEnvelope.Envelope := FSample.FilterEnvelope;
    FAmpEnvelope.Envelope := FSample.AmpEnvelope;
    FPitchEnvelope.Envelope := FSample.PitchEnvelope;
    FLFO1.LFO := FSample.LFO1;
    FLFO2.LFO := FSample.LFO2;
    FLFO3.LFO := FSample.LFO3;
  end;
  FLFOPhase := 0;
  FRunning := False;
  FSamplePosition := 0;
  FNote := -1;
  FNoteOnOffset := 0;
  FStopVoice := False;

end;

procedure TSampleVoiceEngine.Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer);
var
  i: Integer;
  lSample: single;
  lChannel: TChannel;
begin
  if Assigned(FSample.Wave.ChannelList) then
  begin
    if FSample.Wave.ChannelCount > 0 then
    begin
      // Hmm still in MONO (TODO)
      lChannel := TChannel(FSample.Wave.ChannelList[0]);

      if Assigned(lChannel) then
      begin
        for i := FNoteOnOffset to Pred(AFrames) do
        begin
          // Calculate synth voice here
          if (Round(FSamplePosition) >= 0) and (Round(FSamplePosition) < FSample.Wave.Frames) then
          begin
            ABuffer[i] := TChannel(FSample.Wave.ChannelList[0]).Buffer[Round(FSamplePosition)];
          end
          else
          begin
            ABuffer[i] := 0;

            FRunning := False;
          end;

          lSample := ABuffer[i];
          // External audio in Envelope follower

          // Oscillatorbank
          lSample := FOsc1.Process;
          lSample := lSample + FOsc2.Process;
          lSample := lSample + FOsc3.Process;
{
          // LFO's
          FLFO1.Process;
          FLFO2.Process;
          FLFO3.Process;

          // ADSR Pitch
          FPitchEnvelope.Process;
          FPitchEnvelopeLevel := FPitchEnvelope.Level;

          // Pitch


          // ADSR Filter
          FFilterEnvelope.Process;
          FFilterEnvelopeLevel := FFilterEnvelope.Level;

          // Filter

          // ADSR Amplifier
          FAmpEnvelope.Process;
          FAmpEnvelopeLevel := FAmpEnvelope.Level;

          // Amplifier

          // FX

}
          ABuffer[i] := lSample;

          {TODO PitchSemiTone should be replaced with a pitchscale factor}
          if (FSamplePosition + FSample.PitchScaleFactor) < FSample.Wave.Frames then
          begin
            FSamplePosition := FSamplePosition + FSample.PitchScaleFactor;
          end
          else
          begin
            // Single shot
            //FSamplePosition := FSample.Wave.Frames;
            FRunning := False;

            // Loop
            //FSamplePosition := 0;
          end;



        end;

        // Next iteration, start from the beginning
        FNoteOnOffset := 0;
      end;
    end;
  end;
end;

{
  Reinitialize start state
}
procedure TSampleVoiceEngine.NoteOn(ANote: Integer; ARelativeLocation: Integer);
begin
  GLogger.PushMessage(Format('Notevalue: %d', [ANote]));
  FRunning := True;
  FSamplePosition := 0;

  FPitchEnvelope.NoteOn;
  FFilterEnvelope.NoteOn;
  FAmpEnvelope.NoteOn;

  FNoteOnOffset := ARelativeLocation;

  FNote := ANote;

  FOsc1.Rate := GNoteToFreq[ANote];
  FOsc2.Rate := GNoteToFreq[ANote] - 1;
  FOsc3.Rate := GNoteToFreq[ANote] + 1;
end;

procedure TSampleVoiceEngine.NoteOff;
begin
  FStopVoice := True;

  FPitchEnvelope.NoteOff;
  FFilterEnvelope.NoteOff;
  FAmpEnvelope.NoteOff;
end;

function TSampleVoiceEngine.RunningNote: Integer;
begin
  Result := FNote;
end;

{ TSampleBankEngine }

procedure TSampleBankEngine.SetSampleBank(const AValue: TSampleBank);
var
  lSampleEngineIndex: Integer;
  lSampleEngine: TSampleEngine;
begin
  FSampleBank := AValue;

  // Destroy running sample engines
  for lSampleEngineIndex := Pred(FSampleEngineList.Count) downto 0 do
  begin
    FSampleEngineList[lSampleEngineIndex].Free;
  end;

  // Now recreate the sample-engines
  for lSampleEngineIndex := 0 to Pred(FSampleBank.SampleList.Count) do
  begin
    lSampleEngine := TSampleEngine.Create;
    lSampleEngine.Sample := TSample(FSampleBank.SampleList[lSampleEngineIndex]);

    FSampleEngineList.Add(lSampleEngine);

    DBLog(Format('TSampleBankEngine.SetSampleBank: set sampleengine %s', [lSampleEngine.Sample.SampleName]));
  end;
end;

constructor TSampleBankEngine.Create;
begin
  inherited Create;

  FSampleEngineList := TObjectList.Create(True);
end;

destructor TSampleBankEngine.Destroy;
begin
  FSampleEngineList.Free;

  inherited Destroy;
end;

procedure TSampleBankEngine.Initialize;
begin
  inherited Initialize;

end;

procedure TSampleBankEngine.Process(AMidiGrid: TMidiGrid; ABuffer: PSingle;
  AFrames: Integer);
var
  lSampleEngineIndex: Integer;
begin
  for lSampleEngineIndex := 0 to Pred(FSampleEngineList.Count) do
  begin
    // Listens to AMidiGrid midi buffer and mixes samples to ABuffer for a length of
    // AFrames
    TSampleEngine(FSampleEngineList[lSampleEngineIndex]).Process(AMidiGrid, ABuffer, AFrames);
  end;
end;

{ TSamplerEngine }

procedure TSamplerEngine.SetSampler(const AValue: TSampler);
var
  i: Integer;
  lSampleBankEngine: TSampleBankEngine;
begin
  FSampler := AValue;

  for i := 0 to Pred(FSampler.BankList.Count) do
  begin
    lSampleBankEngine := TSampleBankEngine.Create;
    lSampleBankEngine.SampleBank := TSampleBank(FSampler.BankList[i]);

    FSampleBankEngineList.Add(lSampleBankEngine);
  end;
end;

constructor TSamplerEngine.Create;
begin
  FSampleBankEngineList := TObjectList.create(True);
end;

destructor TSamplerEngine.Destroy;
begin
  FSampleBankEngineList.Free;

  inherited Destroy;
end;

procedure TSamplerEngine.Initialize;
begin
  inherited Initialize;

end;

{ This method iterates through all tracks and renders all available midi to just one
  output buffer : ABuffer }
procedure TSamplerEngine.Process(ATrackList: TObjectList; ABuffer: PSingle;
  AFrames: Integer);
var
  lTrackIndex: Integer;
begin
  for lTrackIndex := 0 to Pred(ATrackList.Count) do
  begin

  end;
end;

constructor TFilterEngine.Create;
begin
  inherited Create;

  FA[1] := 0;
  FA[2] := 0;
  FA[3] := 0;
  FA[4] := 0;
  FA[5] := 0;
  FOld := 0;
end;

procedure TFilterEngine.FreqCalc;
var
  lFc  : Double;
  lFcr : Double;
begin
  lFc :=  FFilter.Frequency / FFilter.SampleRate;
  // frequency & amplitude correction
  lFcr := 1.8730 * (lFc * lFc * lFc) + 0.4955 * (lFc * lFc) - 0.6490 * lFc + 0.9988;
  FAcr := -3.9364 * (lFc * lFc) + 1.8409 * lFc + 0.9968;
  F2vg := i2 * (1 - exp(-FIpi * lFcr * lFc)); // Filter Tuning
end;

procedure TFilterEngine.SetFilter(const AValue: TFilter);
begin
  if FFilter = AValue then exit;
  FFilter := AValue;

  Initialize;
end;

constructor TFilter.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  inherited Create(AObjectOwner, AMapped);

  FResonance := 1;
  FFrequency := 1000;
  FSampleRate := 44100;
end;

procedure TFilter.Initialize;
begin
  Notify;
end;

procedure TFilter.SetFrequency(AValue: single);
begin
  if FSampleRate <= 0 then raise exception.create('Sample Rate Error!');
  if AValue <> FFrequency then
  begin
    FFrequency := AValue;
  end;
end;

procedure TFilter.SetSampleRate(AValue: single);
begin
  if FSampleRate <= 0 then raise exception.create('Sample Rate Error!');
  if AValue <> FSampleRate then
  begin
    FSampleRate := AValue;
  end;
end;

procedure TFilter.SetResonance(AValue: single);
begin
  if AValue <> FResonance then
  begin
    if AValue > 1 then
      FResonance := 1
    else if AValue < 0 then
      FResonance := 0
    else
      FResonance := AValue;
  end;
end;

function TFilterEngine.Process(const I : Single):Single;
begin
 // cascade of 4 1st order sections
 FA[1]:=FA[1]+F2vg*(Tanh2_pas2((I+(noise*Random)-2*FFilter.Resonance*FAcr*FOld)*i2v)-Tanh2_pas2(FA[1]*i2v));
// FA[1]:=FA[1]+(F2vg*(Tanh2((I+(noise*Random)-2*fQ*FOld*FAcr)*i2v)-Tanh2(FA[1]*i2v)));
 FA[2]:=FA[2]+F2vg*(Tanh2_pas2(FA[1]*i2v)-Tanh2_pas2(FA[2]*i2v));
 FA[3]:=FA[3]+F2vg*(Tanh2_pas2(FA[2]*i2v)-Tanh2_pas2(FA[3]*i2v));
 FA[4]:=FA[4]+F2vg*(Tanh2_pas2(FA[3]*i2v)-Tanh2_pas2(FA[4]*i2v));

 // 1/2-sample delay for phase compensation
 FOld:=FA[4]+FA[5];
 FA[5]:=FA[4];

 // oversampling
 FA[1]:=FA[1]+F2vg*(Tanh2_pas2((-2*FFilter.Resonance*FAcr*FOld)*i2v)-Tanh2(FA[1]*i2v));
 FA[2]:=FA[2]+F2vg*(Tanh2_pas2(FA[1]*i2v)-Tanh2_pas2(FA[2]*i2v));
 FA[3]:=FA[3]+F2vg*(Tanh2_pas2(FA[2]*i2v)-Tanh2_pas2(FA[3]*i2v));
 FA[4]:=FA[4]+F2vg*(Tanh2_pas2(FA[3]*i2v)-Tanh2_pas2(FA[4]*i2v));

 FOld:=FA[4]+FA[5];
 FA[5]:=FA[4];

 Result:=FOld;
end;

procedure TFilterEngine.Initialize;
begin
  inherited Initialize;

  FreqCalc;
end;

end.

