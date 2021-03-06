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
  Classes, SysUtils, globalconst, jacktypes, ContNrs, global_command, global,
  sndfile, jack, plugin, midiport, samplestreamprovider, filters, baseengine,
  variants, math;

const
  DENORMAL_KILLER = 1E-20;


type

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
    FLoop: Boolean;
    // Active
    FActive: Boolean;
    procedure SetActive(const AValue: Boolean);
    procedure SetAttack(const AValue: single);
    procedure SetDecay(const AValue: single);
    procedure SetLoop(const AValue: Boolean);
    procedure SetRelease(const AValue: single);
  public
    procedure Initialize; override;
    procedure Finalize; override;
  published
    property Attack: single read FAttack write SetAttack;
    property Decay: single read FDecay write SetDecay;
    property Sustain: single read FSustain write FSustain;
    property Release: single read FRelease write SetRelease;
    property Loop: Boolean read FLoop write SetLoop;
    property Active: Boolean read FActive write SetActive;
  end;

var
  EnvelopeStateDescr: array[0..5] of string = ('Start','Attack', 'Decay', 'Sustain', 'Release', 'End');

type
  TEnvelopeState = (esStart, esAttack, esDecay, esSustain, esRelease, esEnd);
  TEnvelopeNoteEvent = (nsNone, nsNoteOn, nsNoteOff);

  {
  }

  { TEnvelopeEngine }

  TEnvelopeEngine = class(TBaseEngine)
  private
    FState: TEnvelopeState;
    FAttackAdder: single;
    FDecayAdder: single;
    FReleaseAdder: single;
    FLevel: single;
    FEnvelope: TEnvelope;
    FFrameCounter: Integer;
    procedure SetEnvelope(const AValue: TEnvelope);
  public
    procedure Initialize; override;
    procedure Process;
    procedure NoteOn;
    procedure NoteOff;
    property Envelope: TEnvelope read FEnvelope write SetEnvelope;
    property Level: single read FLevel;
    property State: TEnvelopeState read FState;
    property FrameCounter: Integer read FFrameCounter;
  end;

  TLFOWaveform = (lwSin, lwSaw, lwTri, lwSqr);

  { TLFO }

  TLFO = class(THybridPersistentModel)
  private
    // Each cycle in Hz ie 0.5Hz / 500Hz
    FRate: Single;
    // waveform types
    FWaveform: TLFOWaveform;
    // Time in seconds to reach full strength at level 1
    FAttack: Integer;
    // Starting point 0..99
    FPhase: Integer;
  public
    procedure Initialize; override;
  published
    property Rate: Single read FRate write FRate;
    property Waveform: TLFOWaveform read FWaveform write FWaveform;
    property Attack: Integer read FAttack write FAttack;
    property Phase: Integer read FPhase write FPhase;
  end;

  { TEnvelopeFollower }

  TEnvelopeFollower = class(THybridPersistentModel)
  private
    FAttack: single;
    FModAmount: single;
    FModSource: TModSource;
    FRelease: single;
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True); reintroduce;
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
    FLevel: single;

    FEnvelopeFollower: TEnvelopeFollower;
    procedure SetEnvelopeFollower(const AValue: TEnvelopeFollower);
  public
    function Process(const I : Single):Single; inline;
    procedure Initialize; override;
    property EnvelopeFollower: TEnvelopeFollower read FEnvelopeFollower write SetEnvelopeFollower;
    property Level: single read FLevel write FLevel;
  end;

  { TAmplifier }

  TAmplifier = class(THybridPersistentModel)
  private
    // Adjust level -24dB .. +24dB
    FAmplify: single;

    FModAmount: single;
    FModSource: TModSource;
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True); reintroduce;
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
    function Process(const I : Single):Single; inline;
    procedure Initialize; override;
    property Amplifier: TAmplifier read FAmplifier write SetAmplifier;
  end;

const
  WFStrings: array[0..5] of string = ('sinus', 'triangle', 'sawtooth', 'square', 'exponent', 'off');
  LUT_SIZE = 4096 * 4;

type
  TOscWaveform = (triangle, sinus, sawtooth, square, exponent, off);
  TOscMode = (omOsc, omLfo);

  { TOscillator }

  TOscillator = class(THybridPersistentModel)
  private
    FStartPhase: dword;
    FWaveForm: TOscWaveform;
    FPitch: Single;
    FModSource: TModSource;
    FModAmount: Single;
    FInternalLevel: Single;
    FActive: Boolean;
    FMode: TOscMode;
    FLevel: single;
    FPulseWidth: single;
    procedure SetActive(const AValue: Boolean);
    procedure SetLevel(const AValue: Single);
    procedure SetModAmount(const AValue: Single);
    procedure SetMode(AValue: TOscMode);
    procedure SetModSource(const AValue: TModSource);
    procedure SetPitch(const AValue: Single);
    procedure SetPulseWidth(AValue: single);
    procedure SetStartPhase(const AValue: dword);
    procedure SetWaveForm(const Value: TOscWaveform);
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True); override;
    function WaveformName:String;
    procedure Initialize; override;
    procedure Finalize; override;
    property WaveForm: TOscWaveform read FWaveForm write SetWaveForm;
    property StartPhase: dword read FStartPhase write SetStartPhase;
    property Pitch: Single read FPitch write SetPitch;
    property ModSource: TModSource read FModSource write SetModSource;
    property ModAmount: Single read FModAmount write SetModAmount;
    property Level: Single read FInternalLevel write SetLevel;
    property Active: Boolean read FActive write SetActive;
    property Mode: TOscMode read FMode write SetMode;
    property PulseWidth: single read FPulseWidth write SetPulseWidth;
  end;

  { TOscillatorWaveformCache }

  TOscillatorWaveformCache = class
  public
    Table: array[Low(TOscWaveform)..High(TOscWaveform), 0..LUT_SIZE + 1] of Single; // 1 more for linear interpolation
    procedure Initialize;
  end;

  { TOscillatorEngine }

  TOscillatorEngine = class(TBaseEngine)
  private
    FRate: single;
    FPhase,
    FInc: single;
    FOscillator: TOscillator;
    FPulseWidth: Integer;
    FSyncOscillator: TOscillatorEngine;
    FLevel: single; // Last process value store
    FDivBySamplerate: single;
    FSynchronized: Boolean;

    procedure SetOscillator(const AValue: TOscillator);
    procedure SetRate(const AValue: single);
  public
    // increments the phase and outputs the new LFO value.
    // return the new LFO value between [-1;+1]
    function Process:Single; inline;
    procedure Initialize; override;
    procedure Sync(APhase: single);
    property Synchronized: Boolean read FSynchronized write FSynchronized;
    property SyncOscillator: TOscillatorEngine read FSyncOscillator write FSyncOscillator;
    property Rate: single read FRate write SetRate;
    property Phase: single read FPhase write FPhase;
    property Level: single read FLevel;
    property Oscillator: TOscillator read FOscillator write SetOscillator;
  end;

  { TSample }

  TSample = class(THybridPersistentModel)
  private
    FGlobalLevelInternal: Single;
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
    FAmpEnvelope: TEnvelope;
    FFilterEnvelope: TEnvelope;

    FGlobalLevel: Single;

    FFilter: TFilter;

    FLFO1: TOscillator;
    FLFO2: TOscillator;
    FLFO3: TOscillator;

    FKey: Integer;
    FLowNote: Integer;
    FHighNote: Integer;
    FVoiceCount: Integer; // The number of voice instances there should be
    FSelected: Boolean;

    { Internal variables}
    FWave: TWaveFile;
    FHasSample: Boolean;


    FSampleName: string;

    FPitchScaleFactor: Single;
    procedure SetGlobalLevel(AValue: Single);
  protected
    procedure RecalculatePitchFactor;
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True); reintroduce;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
    function LoadSample(AFileName: string): boolean;
    property PitchScaleFactor: Single read FPitchScaleFactor;

    procedure UnloadSample;
    property WaveData: pjack_default_audio_sample_t read FWaveData write FWaveData;
    property Wave: TWaveFile read FWave write FWave;
    property HasSample: Boolean read FHasSample write FHasSample;
    property GlobalLevelInternal: Single read FGlobalLevelInternal;
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
    property AmpEnvelope: TEnvelope read FAmpEnvelope write FAmpEnvelope;
    property FilterEnvelope: TEnvelope read FFilterEnvelope write FFilterEnvelope;

    property Filter: TFilter read FFilter write FFilter;

    property LFO1: TOscillator read FLFO1 write FLFO1;
    property LFO2: TOscillator read FLFO2 write FLFO2;
    property LFO3: TOscillator read FLFO3 write FLFO3;

    property GlobalLevel: Single read FGlobalLevel write SetGlobalLevel;

    property Key: Integer read FKey write FKey;
    property LowNote: Integer read FLowNote write FLowNote;
    property HighNote: Integer read FHighNote write FHighNote;
    property VoiceCount: Integer read FVoiceCount write FVoiceCount;
    property Selected: Boolean read FSelected write FSelected;

    property SampleName: string read FSampleName write FSampleName;
    property SampleLocation: string read FSampleLocation write FSampleLocation;
  end;

  { TSampleBank }

  TSampleBank = class(TPluginNode)
  private
    FBankName: string;
    FMidiChannel: Integer; // Needed??
    FSampleList: TObjectList; // type TSampleEngine
    FSampleEngineList: TObjectList;
    FSelected: Boolean;
    FSelectedSample: TSample;
    procedure AssignBank(Source: TSampleBank);
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
    procedure RefreshEngine;
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
      AOutputBuffer: PSingle; AFrames: Integer); override;
    function GetLatency: Integer; override;
    procedure SetLatency(AValue: Integer); override;
    procedure Assign(Source:TPersistent); override;
    property Selected: Boolean read FSelected write FSelected;
    property SelectedSample: TSample read FSelectedSample write FSelectedSample;
  published
    property SampleList: TObjectList read FSampleList write FSampleList;
    property BankName: string read FBankName write FBankName;
    property MidiChannel: Integer read FMidiChannel write FMidiChannel;
  end;

  { TSampleCommand }

  TSampleCommand = class(TCommand)
  private
    FSample: TSample;
    FSampleBank: TSampleBank;
  public
    procedure Initialize; override;
  end;

  TSampleParameter = (
    spOSC1_Pitch,
    spOSC1_Waveform,
    spOSC1_ModSource,
    spOSC1_ModAmount,
    spOSC1_Level,
    spOSC1_PulseWidth,

    spOSC2_Pitch,
    spOSC2_Waveform,
    spOSC2_ModSource,
    spOSC2_ModAmount,
    spOSC2_Level,
    spOSC2_PulseWidth,

    spOSC3_Pitch,
    spOSC3_Waveform,
    spOSC3_ModSource,
    spOSC3_ModAmount,
    spOSC3_Level,
    spOSC3_PulseWidth,

    spFilter_Cutoff,
    spFilter_Cutoff_ModSource,
    spFilter_Cutoff_ModAmount,
    spFilter_Resonance,
    spFilter_Envelope_Amount,
    spFilter_Active,
    spFilter_Type,

    spFilterEnv_Attack,
    spFilterEnv_Decay,
    spFilterEnv_Sustain,
    spFilterEnv_Release,

    spAmplifierEnv_Attack,
    spAmplifierEnv_Decay,
    spAmplifierEnv_Sustain,
    spAmplifierEnv_Release,

    spPitchEnv_Attack,
    spPitchEnv_Decay,
    spPitchEnv_Sustain,
    spPitchEnv_Release,

    spLFO1_Rate,
    spLFO1_Waveform,
    spLFO2_Rate,
    spLFO2_Waveform,
    spLFO3_Rate,
    spLFO3_Waveform,

    spFeedback,
    spGlobal_Level,
    spLow_Note,
    spHigh_Note,
    spBase_Note
  );

  { TSampleParameterCommand }

  TSampleParameterCommand = class(TSampleCommand)
  private
    FOldValue: Variant;
    FValue: Variant;
    FParameter: TSampleParameter;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Value: Variant read FValue write FValue;
    property Parameter: TSampleParameter read FParameter write FParameter;
  end;

  { TSampleBankCommand }

  TSampleBankCommand = class(TCommand)
  private
    FSampleBank: TSampleBank;
  public
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

  { TDeleteSampleCommand }

  TDeleteSampleCommand = class(TSampleBankCommand)
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  end;

  { TCreateSampleCommand }

  TCreateSampleCommand = class(TSampleBankCommand)
  private
    FBaseNote: Integer;
    FHighNote: Integer;
    FLowNote: Integer;
    FOldObjectID: string;
    FSampleLocation: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property SampleLocation: string read FSampleLocation write FSampleLocation;
    property LowNote: Integer read FLowNote write FLowNote;
    property HighNote: Integer read FHighNote write FHighNote;
    property BaseNote: Integer read FBaseNote write FBaseNote;
  end;

  TSampleEngine = class;

  {
    All the classes, methods and variables to actually render the audio are located
    here. Things like the current playing position in a sample. Classes do not descent
    from TPersistent as they are never stored, just instantiated on the heap.
  }

  { TSampleVoice - Internal playing voice for a TSample structure }

  { TSampleVoiceEngine }

  TSampleVoiceEngine = class(TBaseEngine)
  private
    FInternalBuffer: PSingle;

    FSample: TSample;
    FSampleEngine: TSampleEngine;
    FSamplePosition: single;

    FFilterEnvelopeEngine: TEnvelopeEngine;

    FAmpEnvelopeEngine: TEnvelopeEngine;

    FPitchEnvelopeEngine: TEnvelopeEngine;

    FOsc1Engine: TOscillatorEngine;
    FOsc2Engine: TOscillatorEngine;
    FOsc3Engine: TOscillatorEngine;

    FFilterEngine: TDspFilter;

    FLFO1Engine: TOscillatorEngine;
    FLFO2Engine: TOscillatorEngine;
    FLFO3Engine: TOscillatorEngine;

    FLFOPhase: single;
    FNote: Integer;
    FRunning: Boolean;
    FLength: single;
    FCutoffKeytracking: single;
    FVelocity: single;
    FNoteToPitch: single;

    // Start the NOTE OFF phase of a note; handle note release ADSR etc
    FStopVoice: Boolean;

    procedure SetSample(const AValue: TSample);
    procedure SetSampleEngine(AValue: TSampleEngine);
  protected
    function GetSourceAmountPtr(AModSource: TModSource): PSingle;
  public
    constructor Create(AFrames: Integer); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Process(AInputBuffer: PSingle; AOutputBuffer: PSingle; AFrameIndex: Integer);
    procedure NoteOn(ANote: Integer; ALength: Single; AVelocity: Single);
    procedure NoteOff;

    function RunningNote: Integer;
    property Running: Boolean read FRunning write FRunning;
    property AmpEnvelopeEngine: TEnvelopeEngine read FAmpEnvelopeEngine;
    property Note: Integer read FNote write FNote;
    property InternalBuffer: PSingle read FInternalBuffer;
    property SampleEngine: TSampleEngine read FSampleEngine write SetSampleEngine;
    property FilterEngine: TDspFilter read FFilterEngine write FFilterEngine;
    property LFO1Engine: TOscillatorEngine read FLFO1Engine write FLFO1Engine;
    property LFO2Engine: TOscillatorEngine read FLFO2Engine write FLFO2Engine;
    property LFO3Engine: TOscillatorEngine read FLFO3Engine write FLFO3Engine;
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
    FMidiEndIndex: Integer;
    FLFO1Engine: TOscillatorEngine;
    FLFO2Engine: TOscillatorEngine;

    FOldRealCursorPosition: Integer;

    function GetMidiEvent: TMidiData;
    procedure SetSample(const AValue: TSample);
  public
    constructor Create(AFrames: Integer); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
      AOutputBuffer: PSingle; AFrames: Integer);
    property Sample: TSample read FSample write SetSample;
    property LFO1Engine: TOscillatorEngine read FLFO1Engine write FLFO1Engine;
    property LFO2Engine: TOscillatorEngine read FLFO2Engine write FLFO2Engine;
  end;

var
  GWaveformTable: TOscillatorWaveformCache;

implementation

uses
  audiostructure, utils, fx, audioutils;

function SortVoicePriority(Item1 : Pointer; Item2 : Pointer) : Integer;
var
  lVoicePriority1, lVoicePriority2 : TSampleVoiceEngine;
begin
  // We start by viewing the object pointers as TSampleVoiceEngine objects
  lVoicePriority1 := TSampleVoiceEngine(Item1);
  lVoicePriority2 := TSampleVoiceEngine(Item2);

  // Now compare by voice priority. The longer a voice runs the sooner the voice get stolen
  if lVoicePriority1.AmpEnvelopeEngine.FrameCounter > lVoicePriority2.AmpEnvelopeEngine.FrameCounter then
    Result := 1
  else if lVoicePriority1.AmpEnvelopeEngine.FrameCounter = lVoicePriority2.AmpEnvelopeEngine.FrameCounter then
    Result := 0
  else
    Result := -1;
end;

{ TOscillatorWaveformCache }

procedure TOscillatorWaveformCache.Initialize;
var
  i: Integer;
begin
  for i := 0 to LUT_SIZE + 1 do
  begin
    Table[sinus, i] := sin(2*pi*(i/LUT_SIZE));
  end;

  for i := 0 to Pred(LUT_SIZE div 4) do
  begin
    Table[triangle, i] := i / (LUT_SIZE * 0.25);
    Table[triangle, i+Round(LUT_SIZE * 0.25)] := ((LUT_SIZE * 0.25)-i) / (LUT_SIZE * 0.25);
    Table[triangle, i+Round(LUT_SIZE * 0.5)] := - i / (LUT_SIZE * 0.25);
    Table[triangle, i+Round(LUT_SIZE * 0.75)] := - ((LUT_SIZE * 0.25)-i) / (LUT_SIZE * 0.25);
  end;
  Table[triangle, LUT_SIZE] := 0;
  Table[triangle, LUT_SIZE + 1] := 1 / (LUT_SIZE * 0.25);

  for i:= 0 to LUT_SIZE - 1 do
  begin
    Table[sawtooth, i] := 2 * (i / LUT_SIZE) - 1;
  end;
  Table[sawtooth, LUT_SIZE] := -1;
  Table[sawtooth, LUT_SIZE + 1] := 2 * (1 / LUT_SIZE) - 1;

  for i := 0 to LUT_SIZE div 2 do
  begin
    Table[square, i]     :=  1;
    Table[square, i + Round(LUT_SIZE * 0.5)] := -1;
  end;
  Table[square, LUT_SIZE] := 1;
  Table[square, LUT_SIZE + 1] := 1;

  // symetric exponent similar to triangle
  for i:=0 to Pred(LUT_SIZE div 2) do
  begin
    Table[exponent, i] := 2 * ((exp(i / (LUT_SIZE / 2)) - 1) / (exp(1) - 1)) - 1  ;
    Table[exponent, i + (LUT_SIZE div 2)] := 2 * ((exp(((LUT_SIZE / 2) - i) / (LUT_SIZE / 2)) - 1) / (exp(1) - 1)) - 1  ;
  end;
  Table[exponent, LUT_SIZE] := -1;
  Table[exponent, LUT_SIZE + 1] := 2 * ((exp(1 / (LUT_SIZE / 2)) - 1) / (exp(1) - 1)) - 1  ;;
end;


{ TSampleParameterCommand }

procedure TSampleParameterCommand.DoExecute;
begin
  DBLog('start TSampleParameterCommand.DoExecute - ' + VarToStr(FValue));

  FSample.BeginUpdate;

  case FParameter of
    // Osc 1
    spOSC1_Pitch:
    begin
      FOldValue := FSample.Osc1.Pitch;
      FSample.Osc1.Pitch := FValue;
    end;
    spOSC1_Waveform:
    begin
      FOldValue := FSample.Osc1.WaveForm;
      FSample.Osc1.WaveForm := FValue;
    end;
    spOSC1_ModSource:
    begin
      FOldValue := FSample.Osc1.ModSource;
      FSample.Osc1.ModSource := FValue;
    end;
    spOSC1_ModAmount:
    begin
      FOldValue := FSample.Osc1.ModAmount;
      FSample.Osc1.ModAmount := FValue;
    end;
    spOSC1_Level:
    begin
      FOldValue := FSample.Osc1.Level;
      FSample.Osc1.Level := FValue;
    end;
    spOSC1_PulseWidth:
    begin
      FOldValue := FSample.Osc1.PulseWidth;
      FSample.Osc1.PulseWidth := FValue;
    end;

    // Osc 2
    spOSC2_Pitch:
    begin
      FOldValue := FSample.Osc2.Pitch;
      FSample.Osc2.Pitch := FValue;
    end;
    spOSC2_Waveform:
    begin
      FOldValue := FSample.Osc2.WaveForm;
      FSample.Osc2.WaveForm := FValue;
    end;
    spOSC2_ModSource:
    begin
      FOldValue := FSample.Osc2.ModSource;
      FSample.Osc2.ModSource := FValue;
    end;
    spOSC2_ModAmount:
    begin
      FOldValue := FSample.Osc2.ModAmount;
      FSample.Osc2.ModAmount := FValue;
    end;
    spOSC2_Level:
    begin
      FOldValue := FSample.Osc2.Level;
      FSample.Osc2.Level := FValue;
    end;
    spOSC2_PulseWidth:
    begin
      FOldValue := FSample.Osc2.PulseWidth;
      FSample.Osc2.PulseWidth := FValue;
    end;

    // Osc 3
    spOSC3_Pitch:
    begin
      FOldValue := FSample.Osc3.Pitch;
      FSample.Osc3.Pitch := FValue;
    end;
    spOSC3_Waveform:
    begin
      FOldValue := FSample.Osc3.WaveForm;
      FSample.Osc3.WaveForm := FValue;
    end;
    spOSC3_ModSource:
    begin
      FOldValue := FSample.Osc3.ModSource;
      FSample.Osc3.ModSource := FValue;
    end;
    spOSC3_ModAmount:
    begin
      FOldValue := FSample.Osc3.ModAmount;
      FSample.Osc3.ModAmount := FValue;
    end;
    spOSC3_Level:
    begin
      FOldValue := FSample.Osc3.Level;
      FSample.Osc3.Level := FValue;
    end;
    spOSC3_PulseWidth:
    begin
      FOldValue := FSample.Osc3.PulseWidth;
      FSample.Osc3.PulseWidth := FValue;
    end;

    // Filter
    spFilter_Cutoff:
    begin
      FOldValue := FSample.Filter.Frequency;
      FSample.Filter.Frequency := FValue;
    end;
    spFilter_Cutoff_ModSource:
    begin
      FOldValue := FSample.Filter.FreqModSource;
      FSample.Filter.FreqModSource := FValue;
    end;
    spFilter_Cutoff_ModAmount:
    begin
      FOldValue := FSample.Filter.FreqModAmount;
      FSample.Filter.FreqModAmount := FValue;
    end;
    spFilter_Resonance:
    begin
      FOldValue := FSample.Filter.Resonance;
      FSample.Filter.Resonance := FValue;
    end;
    spFilter_Envelope_Amount:
    begin
      FOldValue := FSample.Filter.EnvelopeAmount;
      FSample.Filter.EnvelopeAmount := FValue;
    end;
    spFilter_Active:
    begin
      FOldValue := FSample.Filter.Active;
      FSample.Filter.Active := FValue;
    end;
    spFilter_Type:
    begin
      FOldValue := FSample.Filter.FilterType;
      FSample.Filter.FilterType := FValue;
    end;

    // Filter ADSR
    spFilterEnv_Attack:
    begin
      FOldValue := FSample.FilterEnvelope.Attack;
      FSample.FilterEnvelope.Attack := FValue;
    end;
    spFilterEnv_Decay:
    begin
      FOldValue := FSample.FilterEnvelope.Decay;
      FSample.FilterEnvelope.Decay := FValue;
    end;
    spFilterEnv_Sustain:
    begin
      FOldValue := FSample.FilterEnvelope.Sustain;
      FSample.FilterEnvelope.Sustain := FValue;
    end;
    spFilterEnv_Release:
    begin
      FOldValue := FSample.FilterEnvelope.Release;
      FSample.FilterEnvelope.Release := FValue;
    end;

    // Amplifier ADSR
    spAmplifierEnv_Attack:
    begin
      FOldValue := FSample.AmpEnvelope.Attack;
      FSample.AmpEnvelope.Attack := FValue;
    end;
    spAmplifierEnv_Decay:
    begin
      FOldValue := FSample.AmpEnvelope.Decay;
      FSample.AmpEnvelope.Decay := FValue;
    end;
    spAmplifierEnv_Sustain:
    begin
      FOldValue := FSample.AmpEnvelope.Sustain;
      FSample.AmpEnvelope.Sustain := FValue;
    end;
    spAmplifierEnv_Release:
    begin
      FOldValue := FSample.AmpEnvelope.Release;
      FSample.AmpEnvelope.Release := FValue;
    end;

    // Pitch ADSR
    spPitchEnv_Attack:
    begin
      FOldValue := FSample.PitchEnvelope.Attack;
      FSample.PitchEnvelope.Attack := FValue;
    end;
    spPitchEnv_Decay:
    begin
      FOldValue := FSample.PitchEnvelope.Decay;
      FSample.PitchEnvelope.Decay := FValue;
    end;
    spPitchEnv_Sustain:
    begin
      FOldValue := FSample.PitchEnvelope.Sustain;
      FSample.PitchEnvelope.Sustain := FValue;
    end;
    spPitchEnv_Release:
    begin
      FOldValue := FSample.PitchEnvelope.Release;
      FSample.PitchEnvelope.Release := FValue;
    end;

    // LFO
    spLFO1_Rate:
    begin
      FOldValue := FSample.LFO1.Pitch;
      FSample.LFO1.Pitch := FValue;
    end;
    spLFO1_Waveform:
    begin
      FOldValue := FSample.LFO1.Waveform;
      FSample.LFO1.Waveform := FValue;
    end;
    spLFO2_Rate:
    begin
      FOldValue := FSample.LFO2.Pitch;
      FSample.LFO2.Pitch := FValue;
    end;
    spLFO2_Waveform:
    begin
      FOldValue := FSample.LFO2.Waveform;
      FSample.LFO2.Waveform := FValue;
    end;
    spLFO3_Rate:
    begin
      FOldValue := FSample.LFO3.Pitch;
      FSample.LFO3.Pitch := FValue;
    end;
    spLFO3_Waveform:
    begin
      FOldValue := FSample.LFO3.Waveform;
      FSample.LFO3.Waveform := FValue;
    end;

    // Globals
    spGlobal_Level:
    begin
      FOldValue := FSample.GlobalLevel;
      FSample.GlobalLevel := FValue;
    end;
    spLow_Note:
    begin
      FOldValue := FSample.LowNote;
      FSample.LowNote := FValue;
    end;
    spHigh_Note:
    begin
      FOldValue := FSample.HighNote;
      FSample.HighNote := FValue;
    end;
    spBase_Note:
    begin
      FOldValue := FSample.Key;
      FSample.Key := FValue;
    end;
  end;

  FSample.EndUpdate;

  DBLog('end TSampleParameterCommand.DoExecute');
end;

procedure TSampleParameterCommand.DoRollback;
begin
  DBLog('start TSampleParameterCommand.DoRollback');

  FSample.BeginUpdate;

  case FParameter of
    // OSC1
    spOSC1_Pitch:
    begin
      FSample.Osc1.Pitch := FOldValue;
    end;
    spOSC1_Waveform:
    begin
      FSample.Osc1.WaveForm := FOldValue;
    end;
    spOSC1_ModSource:
    begin
      FSample.Osc1.ModSource := FOldValue;
    end;
    spOSC1_ModAmount:
    begin
      FSample.Osc1.ModAmount := FOldValue;
    end;
    spOSC1_Level:
    begin
      FSample.Osc1.Level := FOldValue;
    end;
    spOSC1_PulseWidth:
    begin
      FSample.Osc1.PulseWidth := FOldValue;
    end;

    // OSC2
    spOSC2_Pitch:
    begin
      FSample.Osc2.Pitch := FOldValue;
    end;
    spOSC2_Waveform:
    begin
      FSample.Osc2.WaveForm := FOldValue;
    end;
    spOSC2_ModSource:
    begin
      FSample.Osc2.ModSource := FOldValue;
    end;
    spOSC2_ModAmount:
    begin
      FSample.Osc2.ModAmount := FOldValue;
    end;
    spOSC2_Level:
    begin
      FSample.Osc2.Level := FOldValue;
    end;
    spOSC2_PulseWidth:
    begin
      FSample.Osc2.PulseWidth := FOldValue;
    end;

    // OSC3
    spOSC3_Pitch:
    begin
      FSample.Osc3.Pitch := FOldValue;
    end;
    spOSC3_Waveform:
    begin
      FSample.Osc3.Waveform := FOldValue;
    end;
    spOSC3_ModSource:
    begin
      FSample.Osc3.ModSource := FOldValue;
    end;
    spOSC3_ModAmount:
    begin
      FSample.Osc3.ModAmount := FOldValue;
    end;
    spOSC3_Level:
    begin
      FSample.Osc3.Level := FOldValue;
    end;
    spOSC3_PulseWidth:
    begin
      FSample.Osc3.PulseWidth := FOldValue;
    end;

    // Filter Cutoff
    spFilter_Cutoff:
    begin
      FSample.Filter.Frequency := FOldValue;
    end;
    spFilter_Cutoff_ModSource:
    begin
      FSample.Filter.FreqModSource := FOldValue;
    end;
    spFilter_Cutoff_ModAmount:
    begin
      FSample.Filter.FreqModAmount := FOldValue;
    end;
    spFilter_Active:
    begin
      FSample.Filter.Active := FOldValue;
    end;
    spFilter_Type:
    begin
      FSample.Filter.FilterType := FOldValue;
    end;

    // Filter Resonance
    spFilter_Resonance:
    begin
      FSample.Filter.Resonance := FOldValue;
    end;
    spFilter_Envelope_Amount:
    begin
      FSample.Filter.EnvelopeAmount := FOldValue;
    end;

    // Filter ADSR
    spFilterEnv_Attack:
    begin
      FSample.FilterEnvelope.Attack := FOldValue;
    end;
    spFilterEnv_Decay:
    begin
      FSample.FilterEnvelope.Decay := FOldValue;
    end;
    spFilterEnv_Sustain:
    begin
      FSample.FilterEnvelope.Sustain := FOldValue;
    end;
    spFilterEnv_Release:
    begin
      FSample.FilterEnvelope.Release := FOldValue;
    end;

    // Amplifier ADSR
    spAmplifierEnv_Attack:
    begin
      FSample.AmpEnvelope.Attack := FOldValue;
    end;
    spAmplifierEnv_Decay:
    begin
      FSample.AmpEnvelope.Decay := FOldValue;
    end;
    spAmplifierEnv_Sustain:
    begin
      FSample.AmpEnvelope.Sustain := FOldValue;
    end;
    spAmplifierEnv_Release:
    begin
      FSample.AmpEnvelope.Release := FOldValue;
    end;

    // Pitch ADSR
    spPitchEnv_Attack:
    begin
      FSample.PitchEnvelope.Attack := FOldValue;
    end;
    spPitchEnv_Decay:
    begin
      FSample.PitchEnvelope.Decay := FOldValue;
    end;
    spPitchEnv_Sustain:
    begin
      FSample.PitchEnvelope.Sustain := FOldValue;
    end;
    spPitchEnv_Release:
    begin
      FSample.PitchEnvelope.Release := FOldValue;
    end;

    // LFO
    spLFO1_Rate:
    begin
      FSample.LFO1.Pitch := FOldValue;
    end;
    spLFO1_Waveform:
    begin
      FSample.LFO1.Waveform := FOldValue;
    end;
    spLFO2_Rate:
    begin
      FSample.LFO2.Pitch := FOldValue;
    end;
    spLFO2_Waveform:
    begin
      FSample.LFO2.Waveform := FOldValue;
    end;
    spLFO3_Rate:
    begin
      FSample.LFO3.Pitch := FOldValue;
    end;
    spLFO3_Waveform:
    begin
      FSample.LFO3.Waveform := FOldValue;
    end;

    // Globals
    spGlobal_Level:
    begin
      FSample.GlobalLevel := FOldValue;
    end;
    spLow_Note:
    begin
      FSample.LowNote := FOldValue;
    end;
    spHigh_Note:
    begin
      FSample.HighNote := FOldValue;
    end;
    spBase_Note:
    begin
      FSample.Key := FOldValue;
    end;
  end;

  FSample.EndUpdate;

  DBLog('end TSampleParameterCommand.DoRollback');
end;

{ TOscillatorEngine }

procedure TOscillatorEngine.SetOscillator(const AValue: TOscillator);
begin
  if FOscillator = AValue then exit;
  FOscillator := AValue;

  Initialize;
end;

function TOscillatorEngine.Process: Single; inline;
var
  i: word;
  lMod: word;
  lFrac: Single;
  lAddSub: Single;
  xm1, x0, x1, x2: single;
begin
  if FOscillator.Active and
    (FOscillator.Level > DENORMAL_KILLER) and
    (FOscillator.WaveForm <> off) then
  begin
    i := Round(Fphase);
    lFrac :=  Frac(FPhase);

    if AllowModifier then
    begin
      lAddSub := Finc + (Modifier^ * FOscillator.ModAmount * 64);
    end
    else
    begin
      lAddSub := Finc;
    end;

    // Limit the maximum else it'll run outside the array
    if lAddSub > LUT_SIZE - 1 then
    begin
      lAddSub := LUT_SIZE;
    end;
    Fphase := FPhase + lAddSub;
    if FPhase > LUT_SIZE then
    begin
      FPhase := FPhase - LUT_SIZE;

      // Hardsync linked oscillator
      if Assigned(FSyncOscillator) then
      begin
        //FSyncOscillator.Sync(FPhase);
      end;

    end;

    if FOscillator.WaveForm = square then
    begin
      // PulseWidth varies from 0..1, scaled to tablewidth 1-50%
      if i < (FOscillator.PulseWidth * (LUT_SIZE * 0.5)) then
      begin
        Result := 1;
      end
      else
      begin
        Result := -1;
      end;
    end;

{   // Hermite interpolation

    xm1 := GWaveformTable.Table[FOscillator.WaveForm, i - 1];
    x0 := GWaveformTable.Table[FOscillator.WaveForm, i];
    x1 := GWaveformTable.Table[FOscillator.WaveForm, i + 1];
    x2 := GWaveformTable.Table[FOscillator.WaveForm, i + 2];
    Result := hermite4(lFrac, xm1, x0, x1, x2) * FOscillator.Level;}

    // Linear interpolation
    x0 := GWaveformTable.Table[FOscillator.WaveForm, i];
    x1 := GWaveformTable.Table[FOscillator.WaveForm, i + 1];
    Result := x0 * (1 - lFrac) + x1 * lFrac;
    Result := Result * FOscillator.Level;

    FLevel := Result;
  end
  else
  begin
    Result := 0;
  end;
end;


procedure TOscillatorEngine.Initialize;
begin
  inherited Initialize;

  FDivBySamplerate := 1 / SampleRate;
  FPulseWidth := 1;
  FPhase := 0;
end;

procedure TOscillatorEngine.SetRate(const AValue: single);
begin
  if FRate = AValue then exit;
  FRate := AValue;

  if FOscillator.FMode = omOsc then
  begin
    Finc := LUT_SIZE * Frate * FDivBySamplerate;
  end
  else
  begin
    Finc := log_approx4(Frate) * LUT_SIZE * 2048 * FDivBySamplerate;
  end;
end;

procedure TOscillatorEngine.Sync(APhase: single);
begin
  FPhase := APhase;
end;

{ TOscillator }

procedure TOscillator.SetStartPhase(const AValue: dword);
begin
  if FStartPhase = AValue then exit;
  FStartPhase := AValue;
end;

procedure TOscillator.SetPitch(const AValue: Single);
begin
  if FPitch = AValue then exit;
  FPitch := AValue;
end;

procedure TOscillator.SetPulseWidth(AValue: single);
begin
  if FPulseWidth = AValue then Exit;
  FPulseWidth := AValue;
end;

procedure TOscillator.SetModAmount(const AValue: Single);
begin
  if FModAmount = AValue then exit;
  FModAmount := AValue;
end;

procedure TOscillator.SetMode(AValue: TOscMode);
begin
  if FMode = AValue then Exit;
  FMode := AValue;
end;

procedure TOscillator.SetModSource(const AValue: TModSource);
begin
  if FModSource = AValue then exit;
  FModSource := AValue;
end;

procedure TOscillator.SetLevel(const AValue: Single);
begin
  if FInternalLevel = AValue then exit;
  FInternalLevel := AValue;
  FLevel := log_approx(FInternalLevel);
end;

procedure TOscillator.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then exit;
  FActive := AValue;
end;

procedure TOscillator.SetWaveForm(const Value: TOscWaveForm);
begin
  FWaveForm := Value;
end;

constructor TOscillator.Create(AObjectOwner: string; AMapped: Boolean);
begin
  inherited Create(AObjectOwner, AMapped);

  FPitch := 1;
  FModAmount := 0;
  FInternalLevel := 1;
  FActive := True;
  FMode := omOsc;
end;

function TOscillator.WaveformName: String;
begin
  result := WFStrings[Ord(Fwaveform)];
end;

procedure TOscillator.Initialize;
begin
  Notify;
end;

procedure TOscillator.Finalize;
begin
  //
end;

{ TAmplifierEngine }

procedure TAmplifierEngine.SetAmplifier(const AValue: TAmplifier);
begin
  if FAmplifier = AValue then exit;
  FAmplifier := AValue;

  Initialize;
end;

function TAmplifierEngine.Process(const I: Single): Single; inline;
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

function TEnvelopeFollowerEngine.Process(const I: Single): Single; inline;
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
  FLevel := Result;
end;

procedure TEnvelopeFollowerEngine.Initialize;
begin
  inherited Initialize;

  // Attack and Release is in seconds
  FAtt := exp(-1.0 / (Samplerate * FEnvelopeFollower.Attack));
  FRel := exp(-1.0 / (Samplerate * FEnvelopeFollower.Release));

  FEnvOut := 0.0;

  FLevel := 1;
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
  FRelease := 0.00001;
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
  Inc(FFrameCounter);

  case FState of
  esStart:
    begin
      FLevel := 0;
    end;
  esAttack:
    begin
      FLevel := FLevel + FAttackAdder;
      if FLevel >= 1 then
      begin
        FLevel := 1;

        FState := esDecay;
      end;
    end;
  esDecay:
    begin
      FLevel := FLevel - FDecayAdder;
      if FLevel <= FEnvelope.Sustain then
      begin
        if FLevel > 0 then
        begin
          FLevel := FEnvelope.Sustain;
          FState := esSustain;
        end
        else
        begin
          FLevel := 0;
          FState := esEnd;
        end;
      end;
    end;
  esSustain:
    begin
      //
    end;
  esRelease:
    begin
      FLevel := FLevel - FReleaseAdder;
      if FLevel < 0.01 then
      begin
        FLevel := 0;
        FState := esEnd;
      end;
    end;
  esEnd:
    begin
      // Stay here at the end, this should be a signal to put the voice engine
      // back in the voice pool
      FLevel := 0;

      // Set max steal priority
      FFrameCounter := High(Integer);
    end;
  end;

  if FLevel < DENORMAL_KILLER then
  begin
    FLevel := DENORMAL_KILLER;
  end;
end;

procedure TEnvelopeEngine.NoteOn;
begin
  begin
    FAttackAdder :=
      1 / (Samplerate * log_approx( FEnvelope.Attack ) * 3);
  end;

  // Time to decrease to sustain level
  if FEnvelope.Decay = 0 then
  begin
    FState := esSustain;
    FLevel := FEnvelope.Sustain;
  end
  else
  begin
    FDecayAdder :=
      (1 - FEnvelope.Sustain) /
      (Samplerate * log_approx( FEnvelope.Decay ) * 3);
  end;

  FFrameCounter := 0;
  FState := esAttack;
end;

procedure TEnvelopeEngine.NoteOff;
begin
  if FLevel = 0 then
  begin
    FState := esEnd;
  end
  else
  begin
    if FEnvelope.Release = 0 then
    begin
      FLevel := 0;
      FState := esEnd;
    end
    else
    begin
      FReleaseAdder :=
        FLevel /
        (Samplerate * log_approx( FEnvelope.Release ) * 5);
    end;

    FState := esRelease;
  end;
end;

{ TSample }

procedure TSample.SetGlobalLevel(AValue: Single);
begin
  if FGlobalLevel = AValue then Exit;
  FGlobalLevel := AValue;

  FGlobalLevelInternal := log_approx(AValue);
end;

procedure TSample.RecalculatePitchFactor;
begin
  //FPitchScaleFactor := 1;
end;

constructor TSample.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  inherited Create(AObjectOwner, AMapped);

  // create oscillators
  FOsc1 := TOscillator.Create(ObjectID);
  FOsc2 := TOscillator.Create(ObjectID);
  FOsc3 := TOscillator.Create(ObjectID);

  // create envelopes
  FAmpEnvelope := TEnvelope.Create(ObjectID);
  FFilterEnvelope := TEnvelope.Create(ObjectID);
  FPitchEnvelope := TEnvelope.Create(ObjectID);

  // create Filter
  FFilter := TFilter.Create(ObjectID);

  // create LFO
  FLFO1 := TOscillator.Create(ObjectID);
  FLFO2 := TOscillator.Create(ObjectID);
  FLFO3 := TOscillator.Create(ObjectID);

  // init oscillators
  FOsc1.Initialize;
  FOsc1.Level := 1;
  FOsc1.Pitch := 12;
  FOsc1.WaveForm := sawtooth;
  FOsc2.Initialize;
  FOsc2.Level := 0;
  FOsc2.Pitch := 12;
  FOsc2.WaveForm := off;
  FOsc3.Initialize;
  FOsc3.Level := 0;
  FOsc3.Pitch := 12;
  FOsc3.WaveForm := off;

  // Init envelopes
  FAmpEnvelope.Initialize;
  FFilterEnvelope.Initialize;
  FPitchEnvelope.Initialize;

  // Init Filter
  FFilter.FilterType := ftBandreject;
  FFilter.Initialize;

  // Init LFO
  FLFO1.Initialize;
  FLFO1.Mode := omLfo;
  FLFO1.Pitch := 0.1;
  FLFO1.WaveForm := sinus;
  FLFO2.Initialize;
  FLFO2.Mode := omLfo;
  FLFO2.Pitch := 0.1;
  FLFO2.WaveForm := off;
  FLFO3.Initialize;
  FLFO3.Mode := omLfo;
  FLFO3.Pitch := 0.1;
  FLFO3.WaveForm := off;

  // Globals
  LowNote := 0;
  HighNote := 127;
  Key := 48;
  GlobalLevel := 1;

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
  FFilter.Free;

  inherited Destroy;
end;

procedure TSample.Initialize;
begin
  Notify;
end;

procedure TSample.Finalize;
begin
  //
end;

function TSample.LoadSample(AFileName: string): boolean;
begin
  DBLog('start TSample.LoadSample');

  Result := True;
  try
    FWave := GSampleStreamProvider.LoadSample(AFileName);

    FSampleName := AFileName;
    FSampleLocation := ExtractFileName(AFileName);
  except
    Result := False;
    raise;
  end;

  FHasSample := True;

  if Assigned(FWave) then
  begin
    if Assigned(FWave.ChannelList) then
    begin
      if FWave.ChannelCount > 0 then
      begin
        // Hmm still in MONO (TODO)
        if Assigned(TChannel(FWave.ChannelList[0])) then
        begin
          FHasSample := True;
        end;
      end;
    end;
  end;

  FPitchScaleFactor := 1 / FWave.ChannelCount;

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

procedure TSampleBank.DoCreateInstance(var AObject: TObject; AClassName: string
  );
var
  lSample: TSample;
begin
  if SameText(AClassName, 'TSAMPLE') then
  begin
    lSample := TSample.Create(ObjectID, MAPPED);
    lSample.ObjectOwnerID := ObjectID;

    AObject := lSample;

    SampleList.Add(lSample);

    RefreshEngine;
  end;
end;

constructor TSampleBank.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  inherited Create(AObjectOwner, AMapped);

  FSampleList := TObjectList.create(False);
  FSampleEngineList := TObjectList.Create(True);

  FOnCreateInstanceCallback := @DoCreateInstance;
end;

destructor TSampleBank.Destroy;
begin
  FSampleEngineList.Free;

  if Assigned(FSampleList) then
  begin
    FSampleList.Clear;
    FSampleList.Free;
  end;

  inherited Destroy;
end;

procedure TSampleBank.Initialize;
var
  lSampleEngineIndex: Integer;
begin
  for lSampleEngineIndex := 0 to Pred(FSampleEngineList.Count) do
  begin
    TSampleEngine(FSampleEngineList[lSampleEngineIndex]).Initialize;
  end;

  Notify;
end;

procedure TSampleBank.Finalize;
begin
  //
end;

{
  Play all samples 'FSampleList' of the TSampleBank
}
procedure TSampleBank.Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
  AOutputBuffer: PSingle; AFrames: Integer);
var
  lSampleEngineIndex: Integer;
begin
  FillByte(AOutputBuffer[0], AFrames * STEREO * SizeOf(Single), 0);

  for lSampleEngineIndex := 0 to Pred(FSampleEngineList.Count) do
  begin
    // Listens to AMidiPattern midi buffer and mixes samples to ABuffer for a length of
    TSampleEngine(FSampleEngineList[lSampleEngineIndex]).Process(AMidiBuffer,
      AInputBuffer, AOutputBuffer, AFrames);
  end;
end;

function TSampleBank.GetLatency: Integer;
begin
  Result := 0;
end;

procedure TSampleBank.SetLatency(AValue: Integer);
begin

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


procedure TSampleBank.RefreshEngine;
var
  lSampleEngineIndex: Integer;
  lSampleIndex: Integer;
  lSampleEngine: TSampleEngine;
  lFound: Boolean;
begin
  // Destroy running sample engines not in samplebank anymore
  for lSampleEngineIndex := Pred(FSampleEngineList.Count) downto 0 do
  begin
    lFound := False;

    for lSampleIndex := 0 to Pred(FSampleList.Count) do
    begin
      if TSample(FSampleList[lSampleIndex]).ObjectID =
        TSampleEngine(FSampleEngineList[lSampleEngineIndex]).Sample.ObjectID then
      begin
        lFound := True;
        break;
      end;
    end;

    if not lFound then
    begin
      FSampleEngineList[lSampleEngineIndex].Free;
    end;
  end;

  // Now create the sample-engines for new samples
  for lSampleIndex := 0 to Pred(FSampleList.Count) do
  begin
    lFound := False;

    for lSampleEngineIndex := 0 to Pred(FSampleEngineList.Count) do
    begin
      if TSample(FSampleList[lSampleIndex]).ObjectID =
        TSampleEngine(FSampleEngineList[lSampleEngineIndex]).Sample.ObjectID then
      begin
        lFound := True;
        break;
      end;
    end;

    if not lFound then
    begin
      lSampleEngine := TSampleEngine.Create(Frames);
      lSampleEngine.Sample := TSample(FSampleList[lSampleIndex]);
      lSampleEngine.Initialize;

      FSampleEngineList.Add(lSampleEngine);
    end;
  end;
end;

{ TDeleteSampleCommand }

procedure TDeleteSampleCommand.DoExecute;
var
  lMementoSample: TSample;
  lSample: TSample;
  lSampleIndex: Integer;
begin
  DBLog('start TDeleteSampleCommand.DoExecute');

  if Assigned(FSampleBank) then
  begin

    for lSampleIndex := 0 to Pred(FSampleBank.SampleList.Count) do
    begin
      lSample := TSample(FSampleBank.SampleList[lSampleIndex]);

      if lSample.Selected then
      begin
        lMementoSample := TSample.Create(FSampleBank.ObjectID, NOT_MAPPED);
        lMementoSample.Assign(lSample);
        lMementoSample.ObjectOwnerID := ObjectOwner;
        lMementoSample.ObjectID := ObjectID;
        Memento.Add(lMementoSample);

        FSampleBank.SampleList.Remove(lSample);
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
begin
  DBLog('start TCreateSampleCommand.DoExecute');

  if Assigned(FSampleBank) then
  begin
    FSampleBank.BeginUpdate;

    lSample := TSample.Create(FSampleBank.ObjectID);
    if SampleLocation <> '' then
    begin
      lSample.LoadSample(SampleLocation);
    end
    else
    begin
      lSample.SampleLocation := '- new -';
    end;

    lSample.Initialize;

    lSample.LowNote := FLowNote;
    lSample.HighNote := FHighNote;
    lSample.Key := FBaseNote;

    FOldObjectID := lSample.ObjectID;

    FSampleBank.SampleList.Add(lSample);

    FSampleBank.RefreshEngine;

    DBLog(Format('Add sample to samplelist: %s', [lSample.ObjectID]));

    DBLog(Format('lBank.SampleList.Count %d', [FSampleBank.SampleList.Count]));

    FSampleBank.EndUpdate;
  end;

  DBLog('end TCreateSampleCommand.DoExecute');
end;

procedure TCreateSampleCommand.DoRollback;
var
  lSample: TSample;
  lSampleIndex: Integer;
begin
  DBLog('start TCreateSampleCommand.DoRollback');

  if Assigned(FSampleBank) then
  begin

    DBLog('Try finding sample...');
    // Search for sample where ObjectID same as Memento.ObjectID
    for lSampleIndex := Pred(FSampleBank.SampleList.Count) downto 0 do
    begin
      lSample := TSample(FSampleBank.SampleList[lSampleIndex]);
      if lSample.ObjectID = FOldObjectID then
      begin
        DBLog('Found sample, deleting...' + lSample.ObjectID);

        FSampleBank.BeginUpdate;
        FSampleBank.SampleList.Remove(lSample);
        FSampleBank.EndUpdate;
        break;
      end;
    end;
  end;

  DBLog('end TCreateSampleCommand.DoRollback');
end;

{ TEnvelope }

procedure TEnvelope.SetAttack(const AValue: single);
begin
  if FAttack = AValue then exit;
  FAttack := AValue;
  if FAttack < 0.01 then
  begin
    FAttack :=  0.01;
  end;
end;

procedure TEnvelope.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then exit;
  FActive := AValue;
end;

procedure TEnvelope.SetDecay(const AValue: single);
begin
  if FDecay = AValue then exit;
  FDecay := AValue;
  if FDecay < 0.01 then
  begin
    FDecay :=  0.01;
  end;
end;

procedure TEnvelope.SetLoop(const AValue: Boolean);
begin
  if FLoop = AValue then exit;
  FLoop := AValue;
end;

procedure TEnvelope.SetRelease(const AValue: single);
begin
  if FRelease = AValue then exit;
  FRelease := AValue;
  if FRelease < 0.01 then
  begin
    FRelease :=  0.01;
  end;
end;

procedure TEnvelope.Initialize;
begin
  Attack := 0.001;
  Decay := 0.3;
  Sustain := 0;
  Release := 0.001;

  Active := True;

  Notify;
end;

procedure TEnvelope.Finalize;
begin
  //
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

{ TSampleCommand }

procedure TSampleCommand.Initialize;
begin
  FSampleBank := TSampleBank(GObjectMapper.GetModelObject(ObjectOwner));
  FSample := TSample(GObjectMapper.GetModelObject(ObjectID));
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
  DBLog(Format('Number of voices %d', [FSample.VoiceCount]));

  for lVoiceIndex := 0 to Pred(FSample.VoiceCount) do
  begin
    DBLog(Format('voice %d', [lVoiceIndex]));
    lSampleVoice := TSampleVoiceEngine.Create(Frames);
    lSampleVoice.Sample := FSample;
    lSampleVoice.SampleEngine := Self;

    lSampleVoice.Initialize;

    FSampleVoiceEngineList.Add(lSampleVoice);

    DBLog(Format('Add SampleVoiceEngine for %s, count %d',
      [lSampleVoice.Sample.SampleName, FSampleVoiceEngineList.Count]));
  end;
end;

constructor TSampleEngine.Create(AFrames: Integer);
begin
  inherited Create(AFrames);

  FLFO1Engine := TOscillatorEngine.Create(Frames);
  FLFO2Engine := TOscillatorEngine.Create(Frames);

  FInternalMidiIndex := 0;
  FOldRealCursorPosition := -1;
  FLastMidiDataIndex := 0;

  FSampleVoiceEngineList := TObjectList.create(True);
end;

destructor TSampleEngine.Destroy;
begin
  FSampleVoiceEngineList.Free;
  FLFO1Engine.Free;
  FLFO2Engine.Free;

  inherited Destroy;
end;

procedure TSampleEngine.Initialize;
begin
  inherited Initialize;

  FLFO1Engine.Oscillator := FSample.LFO1;
  FLFO1Engine.Oscillator.PulseWidth := 1;
  FLFO1Engine.AllowModifier := False;
  FLFO2Engine.Oscillator := FSample.LFO2;
  FLFO2Engine.Oscillator.PulseWidth := 1;
  FLFO2Engine.AllowModifier := False;
end;

procedure TSampleEngine.Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
  AOutputBuffer: PSingle; AFrames: Integer);
var
  lVoiceIndex: Integer;
  lVoice: TSampleVoiceEngine;
  lStealVoice: Boolean;
  lMidiEvent: TMidiEvent;
  lMidiBufferIndex: Integer;
  lIndex: Integer;
  lLeftAdder: Integer;
  lRightAdder: Integer;
  lValue: Single;
begin
  try
    lLeftAdder := 0;
    lRightAdder := 1;

    for lIndex := 0 to Pred(AFrames) do
    begin
      if AMidiBuffer.Count > 0 then
      begin
        AMidiBuffer.Seek(0);
        for lMidiBufferIndex := 0 to Pred(AMidiBuffer.Count) do
        begin
          // Shortcut for current event in buffer
          lMidiEvent := AMidiBuffer.ReadEvent;
          if lIndex = lMidiEvent.RelativeOffset then
          begin
            if lMidiEvent.DataType = mtNoteOn then
            begin
              // Are there any empty voice slots to receive NOTE ON's?
              if (lMidiEvent.DataValue1 >= FSample.Lownote) and
                (lMidiEvent.DataValue1 <= FSample.HighNote) then
              begin
                lStealVoice := True;

                for lVoiceIndex := 0 to Pred(FSampleVoiceEngineList.Count) do
                begin
                  lVoice := TSampleVoiceEngine(FSampleVoiceEngineList[lVoiceIndex]);

                  if (lVoice.Running and (lMidiEvent.DataValue1 = lVoice.Note))
                     or (not lVoice.Running) then
                  begin
                    // Take voice when te same note value or steal
                    lVoice.NoteOn(lMidiEvent.DataValue1, lMidiEvent.Length, lMidiEvent.DataValue2);

                    FLFO1Engine.Rate := FSample.LFO1.Pitch;
                    FLFO2Engine.Rate := FSample.LFO2.Pitch;

                    lStealVoice := False;
                    break;
                  end;
                end;

                if lStealVoice then
                begin
                  // Sort just before stealing, only needed when stealing
                  // TODO hmm maybe a performance penalty sorting?
                  FSampleVoiceEngineList.Sort(@SortVoicePriority);

                  lVoice := TSampleVoiceEngine(FSampleVoiceEngineList[Pred(FSampleVoiceEngineList.Count)]);
                  lVoice.NoteOn(lMidiEvent.DataValue1, lMidiEvent.Length, lMidiEvent.DataValue2);

                  FLFO1Engine.Rate := FSample.LFO1.Pitch;
                  FLFO2Engine.Rate := FSample.LFO2.Pitch;
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
      end;

      // Global LFO's
      FLFO1Engine.Process;
      FLFO2Engine.Process;

      // Per Voice processing
      if FSample.GlobalLevel > DENORMAL_KILLER then
      begin
        for lVoiceIndex := 0 to Pred(FSampleVoiceEngineList.Count) do
        begin
          lVoice := TSampleVoiceEngine(FSampleVoiceEngineList[lVoiceIndex]);

          if lVoice.Running then
          begin
            lVoice.Process(AInputBuffer, AOutputBuffer, lIndex);
            lValue := lVoice.InternalBuffer[lIndex] * FSample.GlobalLevelInternal;
          end
          else
          begin
            lValue := 0;
          end;

          // Mono to stereo
          AOutputBuffer[lLeftAdder] := AOutputBuffer[lLeftAdder] + lValue;
          AOutputBuffer[lRightAdder] := AOutputBuffer[lRightAdder] + lValue;
        end;
      end;

      // Increase iterators
      Inc(lLeftAdder, STEREO);
      Inc(lRightAdder, STEREO);
    end;
  except
    on e: exception do
    begin
      // Do nothing and let the engine run on
{      DBLog('Sampler exception: ' + e.Message);

      // Most likely the filters excepted so just reset them
      for lVoiceIndex := 0 to Pred(FSampleVoiceEngineList.Count) do
      begin
        lVoice := TSampleVoiceEngine(FSampleVoiceEngineList[lVoiceIndex]);
        lVoice.FilterEngine.Initialize;
        lVoice.FilterEngine.Calc;
      end;}
    end;
  end;
end;

{ TSampleVoice - Initializes the voice with the newly assigned TSample }

procedure TSampleVoiceEngine.SetSample(const AValue: TSample);
begin
  FSample := AValue;
end;

procedure TSampleVoiceEngine.SetSampleEngine(AValue: TSampleEngine);
begin
  if FSampleEngine = AValue then Exit;
  FSampleEngine := AValue;
end;

constructor TSampleVoiceEngine.Create(AFrames: Integer);
begin
  inherited Create(AFrames);

  FInternalBuffer := GetMem(Frames * SizeOf(Single));

  FOsc1Engine := TOscillatorEngine.Create(Frames);
  FOsc2Engine := TOscillatorEngine.Create(Frames);
  FOsc3Engine := TOscillatorEngine.Create(Frames);
  FFilterEnvelopeEngine := TEnvelopeEngine.Create(Frames);
  FAmpEnvelopeEngine := TEnvelopeEngine.Create(Frames);
  FPitchEnvelopeEngine := TEnvelopeEngine.Create(Frames);
  FLFO1Engine := TOscillatorEngine.Create(Frames);
  FLFO2Engine := TOscillatorEngine.Create(Frames);
  FLFO3Engine := TOscillatorEngine.Create(Frames);
  FFilterEngine := TDspFilter.Create(Frames);

  FLFOPhase := 0;
  FRunning := False;
  FSamplePosition := 0;
  FNote := -1;
  FStopVoice := False;
end;

destructor TSampleVoiceEngine.Destroy;
begin
  FOsc1Engine.Free;
  FOsc2Engine.Free;
  FOsc3Engine.Free;
  FFilterEnvelopeEngine.Free;
  FAmpEnvelopeEngine.Free;
  FPitchEnvelopeEngine.Free;
  FLFO1Engine.Free;
  FLFO2Engine.Free;
  FLFO3Engine.Free;
  FFilterEngine.Free;

  FreeMem(FInternalBuffer);

  inherited Destroy;
end;

procedure TSampleVoiceEngine.Initialize;
begin
  inherited Initialize;

  if Assigned(FSample) then
  begin
    FOsc1Engine.Oscillator := FSample.Osc1;
    FOsc1Engine.Modifier := GetSourceAmountPtr(FSample.Osc1.ModSource);
    FOsc2Engine.Oscillator := FSample.Osc2;
    FOsc1Engine.Modifier := GetSourceAmountPtr(FSample.Osc2.ModSource);
    FOsc3Engine.Oscillator := FSample.Osc3;
    FOsc3Engine.Modifier := GetSourceAmountPtr(FSample.Osc3.ModSource);
    FFilterEnvelopeEngine.Envelope := FSample.FilterEnvelope;
    FAmpEnvelopeEngine.Envelope := FSample.AmpEnvelope;
    FPitchEnvelopeEngine.Envelope := FSample.PitchEnvelope;
    FLFO1Engine.Oscillator := FSample.LFO1;
    FLFO1Engine.Oscillator.PulseWidth := 1;
    FLFO1Engine.Modifier := GetSourceAmountPtr(FSample.FLFO1.ModSource);
    FLFO2Engine.Oscillator := FSample.LFO2;
    FLFO2Engine.Oscillator.PulseWidth := 1;
    FLFO2Engine.Modifier := GetSourceAmountPtr(FSample.FLFO2.ModSource);
    FLFO3Engine.Oscillator := FSample.LFO3;
    FLFO3Engine.Oscillator.PulseWidth := 1;
    FLFO3Engine.Modifier := GetSourceAmountPtr(FSample.FLFO3.ModSource);
    FFilterEngine.Filter := FSample.Filter;
    FFilterEngine.Modifier := GetSourceAmountPtr(FSample.Filter.FreqModSource);
    FFilterEngine.Filter.FilterType := FSample.Filter.FilterType;

    // Hardsync 1 & 2
    FOsc2Engine.SyncOscillator := FOsc3Engine;
  end;
  FLFOPhase := 0;
  FRunning := False;
  FSamplePosition := 0;
  FNote := -1;
  FStopVoice := False;
end;

procedure TSampleVoiceEngine.Process(AInputBuffer: PSingle; AOutputBuffer: PSingle; AFrameIndex: Integer);
var
  lSample, lSampleA, lSampleB, lSampleC: single;
begin
  // ADSR Amplifier
  FAmpEnvelopeEngine.Process;

  if FAmpEnvelopeEngine.State = esEnd then
  begin
    FRunning := False;

    lSample := DENORMAL_KILLER;
  end
  else
  begin
    // ADSR
    FPitchEnvelopeEngine.Process;
    FFilterEnvelopeEngine.Process;

    // independant running voice LFO
    FLFO3Engine.Process;

    // Calculate synth voice here
    if FSample.HasSample then
    begin
      if (Round(FSamplePosition) >= 0) and (Round(FSamplePosition) < FSample.Wave.Frames) then
      begin
        FInternalBuffer[AFrameIndex] := TChannel(FSample.Wave.ChannelList[0]).Buffer[Round(FSamplePosition)];
      end
      else
      begin
        FInternalBuffer[AFrameIndex] := 0;
      end;
    end;

    // Oscillatorbank
    if FSample.HasSample and (FOsc1Engine.Oscillator.WaveForm <> off) then
    begin
      lSampleA := FInternalBuffer[AFrameIndex] * FOsc1Engine.Oscillator.Level;
    end
    else
    begin
      lSampleA := FOsc1Engine.Process;
    end;
    lSampleB := FOsc2Engine.Process;
    lSampleC := FOsc3Engine.Process;

    // Mix and overdrive oscillators
    lSample := lSampleA + lSampleB + lSampleC;

    // Filter
    if FFilterEngine.Filter.Active then
    begin
      if (FFilterEnvelopeEngine.Level > DENORMAL_KILLER) and
        (FFilterEngine.Filter.EnvelopeAmount > DENORMAL_KILLER) then
      begin
        FFilterEngine.Frequency :=
          FCutoffKeytracking + // Key tracking
          FFilterEngine.Filter.FrequencyInternal + // knob position
          FFilterEnvelopeEngine.Level * FFilterEngine.Filter.EnvelopeAmount; // filter envelope
      end
      else
      begin
        FFilterEngine.Frequency :=
          FCutoffKeytracking + // Key tracking
          FFilterEngine.Filter.FrequencyInternal; // knob position
      end;

      FFilterEngine.Resonance := FFilterEngine.Filter.Resonance;

      lSample := FFilterEngine.Process(lSample);
    end;

    // Amplifier
    if FAmpEnvelopeEngine.Envelope.Active and
      (FAmpEnvelopeEngine.Level > DENORMAL_KILLER) then
    begin
      // Amp & Overdrive
      lSample := lSample * FAmpEnvelopeEngine.Level;
    end
    else
    begin
      lSample := DENORMAL_KILLER;
    end;
  end;

  FInternalBuffer[AFrameIndex] := lSample;

  if FSample.HasSample then
  begin
    if (FSamplePosition + FNoteToPitch) < FSample.Wave.Frames then
    begin
      FSamplePosition := FSamplePosition + FNoteToPitch;
    end;
  end;

  // Virtual note off when FLength < 0
  FLength := FLength - GAudioStruct.BPMScale;
  if FLength < 0 then
  begin
    NoteOff;
  end;

  // Debug statistics
  //DBLog(Format('IntLevel: %g', [FAmpEnvelopeEngine.FLevel]));
  {DBLog(
    Format('IntLevel: %g, Attack: %g Adr: %g,Decay: %g Adr: %g, Sustain: %g, Release: %g Adr: %g  ',
    [FAmpEnvelopeEngine.FLevel,
    FAmpEnvelopeEngine.Envelope.Attack,
    FAmpEnvelopeEngine.FAttackAdder,
    FAmpEnvelopeEngine.Envelope.Decay,
    FAmpEnvelopeEngine.FDecayAdder,
    FAmpEnvelopeEngine.Envelope.Sustain,
    FAmpEnvelopeEngine.Envelope.Release,
    FAmpEnvelopeEngine.FReleaseAdder])); }
end;

{
  Reinitialize start state
}
procedure TSampleVoiceEngine.NoteOn(ANote: Integer; ALength: Single; AVelocity: Single);

  function ClampNote(ANote: Integer): Integer;
  begin
    if ANote > 127 then
      Result := 127
    else if ANote < 0 then
      Result := 0
    else
      Result := ANote;
  end;

begin
  FRunning := True;
  FSamplePosition := 0;
  FStopVoice := False;

  FPitchEnvelopeEngine.NoteOn;
  FFilterEnvelopeEngine.NoteOn;
  FAmpEnvelopeEngine.NoteOn;

  FNote := ANote;

  // Initialize with note length and decrease each iteration of a frame. This is
  // like a pre allocated note off. When using loops this will play the note paSoundTouch
  // the end loop marker.
  FLength := ALength;

  FOsc1Engine.Rate := GNoteToFreq[ClampNote(ANote + Round(FSample.Osc1.Pitch))];
  FOsc2Engine.Rate := GNoteToFreq[ClampNote(ANote + Round(FSample.Osc2.Pitch))] - 1;
  FOsc3Engine.Rate := GNoteToFreq[ClampNote(ANote + Round(FSample.Osc3.Pitch))] + 1;

  FLFO1Engine.Rate := FSample.LFO1.Pitch;
  FLFO2Engine.Rate := FSample.LFO2.Pitch;
  FLFO3Engine.Rate := FSample.LFO3.Pitch;

  FFilterEngine.Modifier := GetSourceAmountPtr(FSample.Filter.FreqModSource);
  FOsc1Engine.Modifier := GetSourceAmountPtr(FSample.Osc1.ModSource);
  FOsc2Engine.Modifier := GetSourceAmountPtr(FSample.Osc2.ModSource);
  FOsc3Engine.Modifier := GetSourceAmountPtr(FSample.Osc3.ModSource);

  // Scale keytracking from 0..12543.8539514160 => 0..1
  FNoteToPitch :=
    (GNoteToFreq[ClampNote(ANote + Round(FSample.Osc1.Pitch))] /
    GNoteToFreq[ClampNote(FSample.Key)]) *
    FSample.PitchScaleFactor;

  // Log scale 0..1
  FCutoffKeytracking := GNoteToFreq[ClampNote(ANote)] / GNoteToFreq[127];
  FVelocity := AVelocity;

  FFilterEngine.ModAmount := FFilterEngine.Filter.FreqModAmount;
end;

procedure TSampleVoiceEngine.NoteOff;
begin
  if not FStopVoice then
  begin
    FPitchEnvelopeEngine.NoteOff;
    FFilterEnvelopeEngine.NoteOff;
    FAmpEnvelopeEngine.NoteOff;
    FStopVoice := True;
  end;
end;

function TSampleVoiceEngine.RunningNote: Integer;
begin
  Result := FNote;
end;

function TSampleVoiceEngine.GetSourceAmountPtr(AModSource: TModSource): PSingle;
begin
  case AModSource of
    msLFO1:
    begin
      Result := @SampleEngine.LFO1Engine.Level;
    end;
    msLFO2:
    begin
      Result := @SampleEngine.LFO2Engine.Level;
    end;
    msLFO3:
    begin
      Result := @FLFO3Engine.Level;
    end;
    msAmpEnvelope:
    begin
      Result := @FAmpEnvelopeEngine.Level;
    end;
    msFilterEnvelope:
    begin
      Result := @FFilterEnvelopeEngine.Level;
    end;
    msPitchEnvelope:
    begin
      Result := @FPitchEnvelopeEngine.Level;
    end;
    msOscillator1:
    begin
      Result := @FOsc1Engine.Level;
    end;
    msOscillator2:
    begin
      Result := @FOsc2Engine.Level;
    end;
    msOscillator3:
    begin
      Result := @FOsc3Engine.Level;
    end;
    msOscillatorMix:
    begin
      Result := @SampleEngine.LFO1Engine.Level; // dummy
    end;
    msFilterOutput:
    begin
      Result := @FFilterEngine.Level;
    end;
    msAmpOutput:
    begin
      Result := @SampleEngine.LFO1Engine.Level; // dummy;
    end;
    msVelocity:
    begin
      Result := @FVelocity; // dummy;
    end;
    msNote:
    begin
      Result := @FCutoffKeytracking; // dummy;
    end;
  end;
end;

initialization
  GWaveformTable := TOscillatorWaveformCache.Create;
  GWaveformTable.Initialize;

finalization
  GWaveformTable.Free;

end.

