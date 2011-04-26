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
  sndfile, jack, plugin, midiport, samplestreamprovider, filters, baseengine,
  variants;

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
    FLoopActive: Boolean;
    procedure SetAttack(const AValue: single);
    procedure SetDecay(const AValue: single);
    procedure SetRelease(const AValue: single);
  public
    procedure Initialize; override;
  published
    property Attack: single read FAttack write SetAttack;
    property Decay: single read FDecay write SetDecay;
    property Sustain: single read FSustain write FSustain;
    property Release: single read FRelease write SetRelease;
    property LoopActive: Boolean read FLoopActive write FLoopActive;
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
    procedure SetEnvelope(const AValue: TEnvelope);
  public
    procedure Initialize; override;
    procedure Process;
    procedure NoteOn;
    procedure NoteOff;
    property Envelope: TEnvelope read FEnvelope write SetEnvelope;
    property Level: single read FLevel;
    property State: TEnvelopeState read FState;
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
    constructor Create(AFrames: Integer); override;
    procedure Initialize; override;
    procedure Process;
    procedure Sync;
    property LFO: TLFO read FLFO write SetLFO;
    property Level: single read FLevel;
    property State: TLFOState read FState write FState;
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
    FPitch: Single;
    FModAmount: Single;
    procedure SetModAmount(const AValue: Single);
    procedure SetPitch(const AValue: Single);
    procedure SetStartPhase(const AValue: dword);
    procedure SetWaveForm(const Value: TOscWaveform);
  public
    function WaveformName:String;
    procedure Initialize; override;
    property WaveForm: TOscWaveform read FWaveForm write SetWaveForm;
    property StartPhase: dword read FStartPhase write SetStartPhase;
    property Pitch: Single read FPitch write SetPitch;
    property ModAmount: Single read FModAmount write SetModAmount;

  end;

  { TOscillatorEngine }

  TOscillatorEngine = class(TBaseEngine)
  private
    FRate: single;
    FTable: array[0..256] of Single; // 1 more for linear interpolation
    FPhase,
    FInc: dword;
    FOscillator: TOscillator;
    FLevel: single; // Last process value store

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
    property Level: single read FLevel;
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

    FAmpEnvelope: TEnvelope;
    FAmpEnvelopeDepth: Single;

    FSaturateDrive: Single;
    FSaturateOn: Boolean;

    FFilter: TFilter;
    FFilterEnvelope: TEnvelope;
    FFilterEnvelopeDepth: Single;

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

    property AmpEnvelope: TEnvelope read FAmpEnvelope write FAmpEnvelope;
    property AmpEnvelopeDepth: Single read FAmpEnvelopeDepth write FAmpEnvelopeDepth;

    property SaturateDrive: Single read FSaturateDrive write FSaturateDrive;
    property SaturateOn: Boolean read FSaturateOn write FSaturateOn;

    property FilterEnvelope: TEnvelope read FFilterEnvelope write FFilterEnvelope;
    property FilterEnvelopeDepth: Single read FFilterEnvelopeDepth write FFilterEnvelopeDepth;
    property Filter: TFilter read FFilter write FFilter;

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
//    FParameter: TSampeParameter;
  protected
    procedure Initialize; override;
  end;

  TSampleParameter = (
    spOSC1_Pitch,
    spOSC1_ModAmount,
    spOSC2_Pitch,
    spOSC2_ModAmount,
    spOSC3_Pitch,
    spOSC3_ModAmount,
    spFilter_Cutoff,
    spFilter_Resonance
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
    FInternalBuffer: PSingle;

    FSample: TSample;
    FSamplePosition: single;
    FFilterEnvelopeEngine: TEnvelopeEngine;
    FFilterEnvelopeLevel: single;

    FAmpEnvelopeEngine: TEnvelopeEngine;
    FAmpEnvelopeLevel: single;

    FPitchEnvelopeEngine: TEnvelopeEngine;
    FPitchEnvelopeLevel: single;

    FOsc1Engine: TOscillatorEngine;
    FOsc2Engine: TOscillatorEngine;
    FOsc3Engine: TOscillatorEngine;

    FFilterEngine: TMoog2FilterEngine;

    FLFO1Engine: TLFOEngine;
    FLFO2Engine: TLFOEngine;
    FLFO3Engine: TLFOEngine;

    FLFOPhase: single;
    FNote: Integer;
    FRunning: Boolean;
    FNoteOnOffset: Integer;
    FLength: single;

    // Start the NOTE OFF phase of a note; handle note release ADSR etc
    FStopVoice: Boolean;

    procedure SetSample(const AValue: TSample);
  protected
    function GetSourceAmountPtr(AModSource: TModSource): PSingle;
  public
    constructor Create(AFrames: Integer);
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Process(AMidiGrid: TMidiGrid; AInputBuffer: PSingle; AFrames: Integer);
    procedure NoteOn(ANote: Integer; ARelativeLocation: Integer; ALength: Single);
    procedure NoteOff;

    function RunningNote: Integer;
    property Running: Boolean read FRunning write FRunning;
    property Note: Integer read FNote write FNote;
    property InternalBuffer: PSingle read FInternalBuffer;
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
    constructor Create(AFrames: Integer);
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
    constructor Create(AFrames: Integer);
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
    constructor Create(AFrames: Integer);
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Process(ATrackList: TObjectList; ABuffer: PSingle; AFrames: Integer);

    property Sampler: TSampler read FSampler write SetSampler;
  end;

implementation

uses
  audiostructure, utils, fx;

{ TSampleParameterCommand }

procedure TSampleParameterCommand.DoExecute;
begin
  DBLog('start TSampleParameterCommand.DoExecute');

  FSample.BeginUpdate;

  case FParameter of
    spOSC1_Pitch:
    begin
      FOldValue := FSample.Osc1.Pitch;
      FSample.Osc1.Pitch := FValue;
    end;
    spOSC1_ModAmount:
    begin
      FOldValue := FSample.Osc1.ModAmount;
      FSample.Osc1.ModAmount := FValue;
    end;
    spOSC2_Pitch:
    begin
      FOldValue := FSample.Osc2.Pitch;
      FSample.Osc2.Pitch := FValue;
    end;
    spOSC2_ModAmount:
    begin
      FOldValue := FSample.Osc2.ModAmount;
      FSample.Osc2.ModAmount := FValue;
    end;
    spOSC3_Pitch:
    begin
      FOldValue := FSample.Osc3.Pitch;
      FSample.Osc3.Pitch := FValue;
    end;
    spOSC3_ModAmount:
    begin
      FOldValue := FSample.Osc3.ModAmount;
      FSample.Osc3.ModAmount := FValue;
    end;
    spFilter_Cutoff:
    begin
      FOldValue := FSample.Filter.Frequency;
      FSample.Filter.Frequency := FValue;
    end;
    spFilter_Resonance:
    begin
      FOldValue := FSample.Filter.Resonance;
      FSample.Filter.Resonance := FValue;
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
    spOSC1_Pitch:
    begin
      FSample.Osc1.Pitch := FOldValue;
    end;
    spOSC1_ModAmount:
    begin
      FSample.Osc1.ModAmount := FOldValue;
    end;
    spOSC2_Pitch:
    begin
      FSample.Osc2.Pitch := FOldValue;
    end;
    spOSC2_ModAmount:
    begin
      FSample.Osc2.ModAmount := FOldValue;
    end;
    spOSC3_Pitch:
    begin
      FSample.Osc3.Pitch := FOldValue;
    end;
    spOSC3_ModAmount:
    begin
      FSample.Osc3.ModAmount := FOldValue;
    end;
    spFilter_Cutoff:
    begin
      FSample.Filter.Frequency := FOldValue;
    end;
    spFilter_Resonance:
    begin
      FSample.Filter.Resonance := FOldValue;
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

  FLevel := Result;
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
//  GLogger.PushMessage(Format('Note value: %f', [AValue]));

  if FRate = AValue then exit;
  FRate := AValue;
  // the rate in Hz is converted to a phase increment with the following formula
  // f[ inc = (256*rate/samplerate) * 2^24]
  Finc := round((256 * Frate / Samplerate) * (1 shl 24));
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

procedure TOscillator.SetPitch(const AValue: Single);
begin
  if FPitch = AValue then exit;
  FPitch := AValue;
end;

procedure TOscillator.SetModAmount(const AValue: Single);
begin
  if FModAmount = AValue then exit;
  FModAmount := AValue;
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

procedure TOscillator.Initialize;
begin
  {FPitch := 0;
  FModAmount := 0;

  Notify;}
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
  FAtt := exp(-1.0 / (Samplerate * FEnvelopeFollower.Attack));
  FRel := exp(-1.0 / (Samplerate * FEnvelopeFollower.Release));

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

constructor TLFOEngine.Create(AFrames: Integer);
begin
  inherited Create(AFrames);

end;

procedure TLFOEngine.Initialize;
begin
  inherited Initialize;

  case FLFO.Waveform of
  lwSaw:
    begin
      FLevel := 1;
      FAdder := FLFO.Rate / Samplerate;
    end;
  lwSin: // TODO this is a tri not a sin!
    begin
      FLevel := 1;
      FAdder := (FLFO.Rate / Samplerate) * 2;
    end;
  lwSqr:
    begin
      FPositionAdder := (FLFO.Rate / Samplerate);
    end;
  lwTri:
    begin
      FPositionAdder := (FLFO.Rate / Samplerate);
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

  FAttackAdder := 1 / (Samplerate * FEnvelope.Attack);
  FDecayAdder := 1 / (Samplerate * FEnvelope.Decay);
  FReleaseAdder := 1 / (Samplerate * FEnvelope.Release);

  FLevel := 0;
  FState := esStart;
end;

procedure TEnvelopeEngine.Process;
begin
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
        FState := esDecay;
      end;
    end;
  esDecay:
    begin
      FLevel := FLevel - FDecayAdder;
      if FLevel <= FEnvelope.Sustain then
      begin
        FState := esSustain;
      end;
    end;
  esSustain:
    begin
      // Do nothing, level is stable here
    end;
  esRelease:
    begin
      FLevel := FLevel - FReleaseAdder;
      if FLevel <= 0 then
      begin
        FState := esEnd;
      end;
    end;
  esEnd:
    begin
      // Stay here at the end, this should be a signal to put the voice engine
      // back in the voice pool
      FLevel := 0;
    end;
  else
    FLevel := 0;
  end;

  if FLevel < 0 then FLevel := 0;
  if FLevel > 2 then FLevel := 2;
end;

procedure TEnvelopeEngine.NoteOn;
begin
  FAttackAdder := 1 / (Samplerate * FEnvelope.Attack);
  FDecayAdder := 1 / (Samplerate * FEnvelope.Decay);
  FLevel := 0;
  FState := esAttack;
end;

procedure TEnvelopeEngine.NoteOff;
begin
  FReleaseAdder := 1 / (Samplerate * FEnvelope.Release);
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

procedure TSample.RecalculatePitchFactor;
begin
  FPitchScaleFactor := 1;
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

  // Init Filter
  FFilter := TFilter.Create(ObjectID);

  // Init LFO
  FLFO1 := TLFO.Create(ObjectID);
  FLFO2 := TLFO.Create(ObjectID);
  FLFO3 := TLFO.Create(ObjectID);

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

  FOsc1.Initialize;
  FOsc2.Initialize;
  FOsc3.Initialize;

  // Init envelopes
  FAmpEnvelope.Initialize;
  FFilterEnvelope.Initialize;
  FPitchEnvelope.Initialize;

  // Init Filter

  FFilter.FilterType := ft24DB;
  FFilter.Initialize;

  // Init LFO
  FLFO1.Initialize;
  FLFO2.Initialize;
  FLFO3.Initialize;

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

procedure TEnvelope.SetAttack(const AValue: single);
begin
  if FAttack = AValue then exit;
  FAttack := AValue;

  // Please no div by zero
  if FAttack = 0 then
  begin
    FAttack := 0.01;
  end;
end;

procedure TEnvelope.SetDecay(const AValue: single);
begin
  if FDecay = AValue then exit;
  FDecay := AValue;

  // Please no div by zero
  if FDecay = 0 then
  begin
    FDecay := 0.01;
  end;
end;

procedure TEnvelope.SetRelease(const AValue: single);
begin
  if FRelease = AValue then exit;
  FRelease := AValue;

  // Please no div by zero
  if FRelease = 0 then
  begin
    FRelease := 0.01;
  end;
end;

procedure TEnvelope.Initialize;
begin
  Attack := 0.05;
  Decay := 0.3;
  Sustain := 0.1;
  Release := 1.5;

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
    lSampleVoice := TSampleVoiceEngine.Create(Frames);
    lSampleVoice.Sample := FSample;

    lSampleVoice.Initialize;

    FSampleVoiceEngineList.Add(lSampleVoice);

    writeln(Format('Add SampleVoiceEngine for %s, count %d',
      [lSampleVoice.Sample.SampleName, FSampleVoiceEngineList.Count]));
  end;
end;

constructor TSampleEngine.Create(AFrames: Integer);
begin
  inherited Create(AFrames);

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
  lBufferIndex: Integer;
  lSampleAdd: Single;
  lSampleMul: Single;

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
{        for lVoiceIndex := 0 to Pred(FSampleVoiceEngineList.Count) do
        begin
          lVoice := TSampleVoiceEngine(FSampleVoiceEngineList[lVoiceIndex]);

          if lVoice.Running then
          begin
            if lMidiEvent.DataValue1 = lVoice.Note then
            begin
              lVoice.NoteOff;
              break;
            end;
          end;
        end;    }
      end
      else if lMidiEvent.DataType = mtNoteOn then
      begin
        {
          Are there any empty voice slots to receive NOTE ON's?
        }
        for lVoiceIndex := 0 to Pred(FSampleVoiceEngineList.Count) do
        begin
          lVoice := TSampleVoiceEngine(FSampleVoiceEngineList[lVoiceIndex]);

          {if lVoice.Running and (lMidiEvent.DataValue1 = lVoice.Note) then
          begin
            // Retrigger note when te same note value
            lVoice.NoteOn(lMidiEvent.DataValue1, lMidiEvent.RelativeOffset, lMidiEvent.Length);
            break;
          end
          else}
          if not lVoice.Running then
          begin
            // Start note at location 'RelativeOffset'
            lVoice.NoteOn(lMidiEvent.DataValue1, lMidiEvent.RelativeOffset, lMidiEvent.Length);
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

    lVoice.Process(AMidiGrid, ABuffer, AFrames);
  end;

  // Mix all voices into buffer
  for lVoiceIndex := 0 to Pred(FSampleVoiceEngineList.Count) do
  begin
    lVoice := TSampleVoiceEngine(FSampleVoiceEngineList[lVoiceIndex]);

    for lBufferIndex := 0 to Pred(Frames) do
    begin
      lSampleAdd := ABuffer[lBufferIndex] + lVoice.InternalBuffer[lBufferIndex];
      lSampleMul := ABuffer[lBufferIndex] * lVoice.InternalBuffer[lBufferIndex];
      ABuffer[lBufferIndex] := lSampleAdd - lSampleMul;
    end;
  end;
end;

{ TSampleVoice - Initializes the voice with the newly assigned TSample }

procedure TSampleVoiceEngine.SetSample(const AValue: TSample);
begin
  FSample := AValue;
end;

constructor TSampleVoiceEngine.Create(AFrames: Integer);
begin
  inherited Create(AFrames);

  FInternalBuffer := GetMem(Frames * SizeOf(Single));

  GLogger.PushMessage(Format('FInternalBuffer %d', [Frames * SizeOf(Single)]));

  FOsc1Engine := TOscillatorEngine.Create(Frames);
  FOsc2Engine := TOscillatorEngine.Create(Frames);
  FOsc3Engine := TOscillatorEngine.Create(Frames);
  FFilterEnvelopeEngine := TEnvelopeEngine.Create(Frames);
  FAmpEnvelopeEngine := TEnvelopeEngine.Create(Frames);
  FPitchEnvelopeEngine := TEnvelopeEngine.Create(Frames);
  FLFO1Engine := TLFOEngine.Create(Frames);
  FLFO2Engine := TLFOEngine.Create(Frames);
  FLFO3Engine := TLFOEngine.Create(Frames);
  FFilterEngine := TMoog2FilterEngine.Create(Frames);

  FLFOPhase := 0;
  FRunning := False;
  FSamplePosition := 0;
  FNote := -1;
  FNoteOnOffset := 0;
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
var
  lBufferIndex: Integer;
begin
  inherited Initialize;

  if Assigned(FSample) then
  begin
    FOsc1Engine.Oscillator := FSample.Osc1;
    FOsc2Engine.Oscillator := FSample.Osc2;
    FOsc3Engine.Oscillator := FSample.Osc3;
    FFilterEnvelopeEngine.Envelope := FSample.FilterEnvelope;
    FAmpEnvelopeEngine.Envelope := FSample.AmpEnvelope;
    FPitchEnvelopeEngine.Envelope := FSample.PitchEnvelope;
    FLFO1Engine.LFO := FSample.LFO1;
    FLFO2Engine.LFO := FSample.LFO2;
    FLFO3Engine.LFO := FSample.LFO3;
    FFilterEngine.Filter := FSample.Filter;
    FFilterEngine.Filter.FilterType := FSample.Filter.FilterType;
  end;
  FLFOPhase := 0;
  FRunning := False;
  FSamplePosition := 0;
  FNote := -1;
  FNoteOnOffset := 0;
  FStopVoice := False;
end;

procedure TSampleVoiceEngine.Process(AMidiGrid: TMidiGrid; AInputBuffer: PSingle; AFrames: Integer);
var
  i: Integer;
  lSample, lSampleA, lSampleB, lSampleC: single;
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
          if FLength <= 0 then
          begin
            NoteOff;
          end;

          // Calculate synth voice here
          if (Round(FSamplePosition) >= 0) and (Round(FSamplePosition) < FSample.Wave.Frames) then
          begin
            FInternalBuffer[i] := TChannel(FSample.Wave.ChannelList[0]).Buffer[Round(FSamplePosition)];
          end
          else
          begin
            FInternalBuffer[i] := 0;
          end;

          lSample := FInternalBuffer[i];

          // External audio in Envelope follower
//          FEnvelopeFollower.Process(AInputBuffer[i]);

          // Oscillatorbank
          lSampleA := FOsc1Engine.Process;
          lSampleB := FOsc2Engine.Process;
          lSampleC := FOsc3Engine.Process;
          lSample := lSampleA + lSampleB - (lSampleA * lSampleB);
          lSample := lSample +  lSampleC - (lSample * lSampleC);

          // LFO's
          {FLFO1Engine.Process;
          FLFO2Engine.Process;
          FLFO3Engine.Process;}

          // ADSR Pitch
          {FPitchEnvelopeEngine.Process;
          FPitchEnvelopeLevel := FPitchEnvelopeEngine.Level;}

          // Pitch


          // ADSR Filter
          FFilterEnvelopeEngine.Process;

          // Filter
          FFilterEngine.Frequency := FFilterEngine.Filter.Frequency * FFilterEnvelopeEngine.Level;
          FFilterEngine.Resonance := FFilterEngine.Filter.Resonance * 0.01;
          lSample := FFilterEngine.Process(lSample);

          // ADSR Amplifier
          FAmpEnvelopeEngine.Process;

          // Amplifier
          lSample := lSample * FAmpEnvelopeEngine.Level;

          if FAmpEnvelopeEngine.State = esEnd then
          begin
            FRunning := False;
          end;

          // FX

          FInternalBuffer[i] := lSample;

          {TODO PitchSemiTone should be replaced with a pitchscale factor}
          if (FSamplePosition + FSample.PitchScaleFactor) < FSample.Wave.Frames then
          begin
            FSamplePosition := FSamplePosition + FSample.PitchScaleFactor;
          end
          else
          begin
            // Single shot
            //FSamplePosition := FSample.Wave.Frames;

            // Loop
            //FSamplePosition := 0;
          end;

          // Virtual note of when FLength <= 0
          FLength := FLength - GAudioStruct.BPMAdder;
        end;

        {GLogger.PushMessage(Format('FFilter %f, FFilter.Filter.Frequency %f, FFilterEnvelope %f',
        [FFilterEngine.Frequency, FFilterEngine.Filter.Frequency, FFilterEnvelopeEngine.Level]));}

        // Next iteration, start from the beginning
        FNoteOnOffset := 0;
      end;
    end;
  end;
end;

{
  Reinitialize start state
}
procedure TSampleVoiceEngine.NoteOn(ANote: Integer; ARelativeLocation: Integer; ALength: Single);

  function ClampNote(ANote: Integer): Integer;
  begin
    if ANote > 127 then
      Result := 127
    else if ANote < 0 then
      Result := 0;
  end;

begin
  FRunning := True;
  FSamplePosition := 0;

  FPitchEnvelopeEngine.NoteOn;
  FFilterEnvelopeEngine.NoteOn;
  FAmpEnvelopeEngine.NoteOn;

  FNoteOnOffset := ARelativeLocation;

  FNote := ANote;

  // Initialize with note length and decrease each iteration of a frame. This is
  // like a pre allocated note off. When using loops this will play the note past
  // the end loop marker.
  FLength := ALength;

  FOsc1Engine.Rate := GNoteToFreq[ClampNote(ANote + Round(FSample.Osc1.Pitch))];
  FOsc2Engine.Rate := GNoteToFreq[ClampNote(ANote + Round(FSample.Osc2.Pitch))] - 1;
  FOsc3Engine.Rate := GNoteToFreq[ClampNote(ANote + Round(FSample.Osc3.Pitch))] + 1;
{  FOsc1Engine.Rate := GNoteToFreq[ANote{ + Round(FSample.Osc1.Pitch)}];
  FOsc2Engine.Rate := GNoteToFreq[ANote{ + Round(FSample.Osc2.Pitch)}] - 1;
  FOsc3Engine.Rate := GNoteToFreq[ANote{ + Round(FSample.Osc3.Pitch)}] + 1;}
end;

procedure TSampleVoiceEngine.NoteOff;
begin
  FStopVoice := True;

  FPitchEnvelopeEngine.NoteOff;
  FFilterEnvelopeEngine.NoteOff;
  FAmpEnvelopeEngine.NoteOff;
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
    lSampleEngine := TSampleEngine.Create(Frames);
    lSampleEngine.Sample := TSample(FSampleBank.SampleList[lSampleEngineIndex]);

    FSampleEngineList.Add(lSampleEngine);

    DBLog(Format('TSampleBankEngine.SetSampleBank: set sampleengine %s', [lSampleEngine.Sample.SampleName]));
  end;
end;

constructor TSampleBankEngine.Create(AFrames: Integer);
begin
  inherited Create(AFrames);

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
    lSampleBankEngine := TSampleBankEngine.Create(Frames);
    lSampleBankEngine.SampleBank := TSampleBank(FSampler.BankList[i]);

    FSampleBankEngineList.Add(lSampleBankEngine);
  end;
end;

constructor TSamplerEngine.Create(AFrames: Integer);
begin
  inherited Create(AFrames);

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

function TSampleVoiceEngine.GetSourceAmountPtr(AModSource: TModSource): PSingle;
begin
  case AModSource of
    msLFO1:
    begin
      Result := @FLFO1Engine.Level;
    end;
    msLFO2:
    begin
      Result := @FLFO2Engine.Level;
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
    msEnvelopeFollower:
    begin
      Result := nil;
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
      Result := nil;
    end;
    msFilterOutput:
    begin
      Result := @FFilterEngine.Level;
    end;
    msAmpOutput:
    begin
      ;
    end;
    msVelocity:
    begin
      ;
    end;
    msNote:
    begin
      ;
    end;
  end;
end;

end.

