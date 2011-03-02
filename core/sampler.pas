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

  { TEnvelope }

  TEnvelope = class(THybridPersistentModel)
  private
    FAttack: Integer;
    FDecay: Integer;
    FSustain: Integer;
    FRelease: Integer;
  public
    procedure Initialize; override;
  published
    property Attack: Integer read FAttack write FAttack;
    property Decay: Integer read FDecay write FDecay;
    property Sustain: Integer read FSustain write FSustain;
    property Release: Integer read FRelease write FRelease;
  end;

  { TLFO }

  TLFO = class(THybridPersistentModel)
  private
    FRate: Single;
    FWaveform: Integer;
    FAttack: Integer;
    FKeyDepth: Single;
    FIsON: Boolean;
  public
    procedure Initialize; override;
  published
    property Rate: Single read FRate write FRate;
    property Waveform: Integer read FWaveform write FWaveform;
    property Attack: Integer read FAttack write FAttack;
    property KeyDepth: Single read FKeyDepth write FKeyDepth;
    property IsON: Boolean read FIsON write FIsON;
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

    FFilterEnvelope: TEnvelope;
    FFilterEnvelopeDepth: Single;
    FFilterLFODepth: Single;
    FFilterKeyDepth: Single;
    FFilterOn: Boolean;
    FFilterCutoff: Single;
    FFilterResonance: Single;

    FLFO: TLFO;

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

    property LFO: TLFO read FLFO write FLFO;

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

  { TBaseEngine - A really simple base class }

  TBaseEngine = class
  end;

  { TSampleVoice - Internal playing voice for a TSample structure }

  TSampleVoiceEngine = class(TBaseEngine)
  private
    FSample: TSample;
    FSamplePosition: single;
    FFilterEnvelopePosition: single;
    FAmpEnvelopePosition: single;
    FPitchEnvelopePosition: single;
    FLFOPhase: single;
    FNote: Integer;
    FRunning: Boolean;
    FNoteOnOffset: Integer;

    procedure SetSample(const AValue: TSample);
  public
    constructor Create;
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
    procedure Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer);

    property MidiDataList: TMidiDataList read FMidiDataList write FMidiDataList;
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
    procedure Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer);

    property MidiDataList: TMidiDataList read FMidiDataList write FMidiDataList;
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
    procedure Process(ATrackList: TObjectList; ABuffer: PSingle; AFrames: Integer);

    property Sampler: TSampler read FSampler write SetSampler;
  end;

implementation

uses
  audiostructure, utils;

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
  i, j: Integer;
  lBankIndex: Integer;
begin
  for lBankIndex := 0 to Pred(FBankList.Count) do
  begin
    TSampleBank(FBankList[lBankIndex]).Process(AMidiGrid, ABuffer, AFrames);
  end;
  (*
  j := 0;
  for i := 0 to Pred(AMidiGrid.MidiDataList.Count) do
  begin
    if (TMidiData(AMidiGrid.MidiDataList[i]).Location >= AMidiGrid.RealCursorPosition) and
      (TMidiData(AMidiGrid.MidiDataList[i]).Location < AMidiGrid.RealCursorPosition + AFrames) then
    begin
      // Found start of midistream
      j := i;
      break;
    end;
  end;

  for i := 0 to AFrames - 1 do
  begin
    ABuffer[i]:= ABuffer[i] * FTestSignal;
    if AMidiGrid.MidiDataList.Count > 0 then
    begin

      while (j < AMidiGrid.MidiDataList.Count) and (TMidiData(AMidiGrid.MidiDataList[j]).Location = i + 0{AMidiGrid.RealCursorPosition}) do
      begin
        //FTestSignal := 1;
        // Create/Destroy not threads
        case TMidiData(AMidiGrid.MidiDataList[j]).DataType of
          mtNoteOn:
          begin
            FTestSignal := 1;
          end;
          mtNoteOff:
          begin
            FTestSignal := 0;
          end;
          mtBankSelect:
          begin

          end;
        end;
        Inc(j);
      end;
    end;
  end;
  *)
  Result := round(FTestSignal);
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

  // Init envelopes
  FAmpEnvelope := TEnvelope.Create(ObjectID);
  FFilterEnvelope := TEnvelope.Create(ObjectID);
  FPitchEnvelope := TEnvelope.Create(ObjectID);

  // Init LFO
  FLFO := TLFO.Create(ObjectID);

  FPitchSemiTone := 1;
  FPitchDetune := 0;

  FPitchScaleFactor := 1;

  // Default voice count (6 note polyphonic)
  FVoiceCount := 6;
end;

destructor TSample.Destroy;
begin
  FAmpEnvelope.Free;
  FFilterEnvelope.Free;
  FPitchEnvelope.Free;
  FLFO.Free;

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
  (*
  // Only process when not in state change
  if AMidiGrid.Enabled and (AMidiGrid.MidiDataList.Count > 0) then
  begin
    lFrameOffsetLow := ((AMidiGrid.RealCursorPosition div AFrames) * AFrames);
    lFrameOffsetHigh := ((AMidiGrid.RealCursorPosition div AFrames) * AFrames) + AFrames;

    while (not AMidiGrid.MidiDataList.Eof) and
      (AMidiGrid.MidiDataList.CurrentMidiData.Location < lFrameOffsetHigh) do
    begin
      lMidiData := AMidiGrid.MidiDataList.CurrentMidiData;

      if AMidiGrid.RealCursorPosition > lMidiData.Location then
      begin
        lRelativeLocation := 0
      end
      else
      begin
        lRelativeLocation := lMidiData.Location mod AFrames;
      end;
      {
        push note to engine which in turn looks if this note is already playing
        if so then reset the sample engine to start = 0
        if note is a note off then end the note and deregister as playing

        if not playing then start engine from start = 0
      }

{
      buffer := jack_midi_event_reserve(AMidiOutBuf, lRelativeLocation, 3);
      if Assigned(buffer) then
      begin
        case lMidiData.DataType of
          mtNoteOn:
          begin
  			    buffer[0] := $90 + APattern.MidiChannel;	{ note on }
  			    buffer[1] := lMidiData.DataValue1;
            buffer[2] := lMidiData.DataValue2;		{ velocity }
          end;
          mtNoteOff:
          begin
    				buffer[0] := $80 + APattern.MidiChannel;;	{ note off }
    				buffer[1] := lMidiData.DataValue1;
    				buffer[2] := 0;		{ velocity }
          end;
          mtBankSelect:
          begin

          end;
        end;
      end
      else
      begin
        ATrack.DevValue := 'jackmidi buffer allocation failed';
      end;
}
//      ATrack.DevValue := Format('Play midi at location %d', [lMidiData.Location]);

      AMidiGrid.MidiDataList.Next;
    end;
  end;       *)


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
    lSampleVoice := TSampleVoiceEngine.Create;
    lSampleVoice.Sample := FSample;

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

procedure TSampleEngine.Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer);
var
  lLocalCursorRatio: Single;
  lLocalCursorAdder: Single;
  lLocalMidiDataCursor: TMidiData;

  lVoiceIndex: Integer;
  lVoice: TSampleVoiceEngine;

  lMidiEvent: TMidiData;
  lFrame: Integer;
  lMidiBufferIndex: Integer;
  lRelativeLocation: Integer;
begin
  GLogger.PushMessage(Format('Midi cursor pos: %f', [AMidiGrid.CursorAdder]));

  //  Loop through aframes
  // increase the local cursor
  lLocalCursorAdder := AMidiGrid.CursorAdder;
  lLocalCursorRatio := AMidiGrid.BPMScale;
  lLocalMidiDataCursor := AMidiGrid.MidiDataCursor;

  if Assigned(lLocalMidiDataCursor) then
  begin
    for lFrame := 0 to Pred(AFrames) do
    begin
      // Parse all mididata where cursor is past mididata.location
      while lLocalCursorAdder >= lLocalMidiDataCursor.Location do
      begin
        // Parse midi event (start/stop note)

        // Is this note already playing? Yes...now locate the voice
        for lVoiceIndex := 0 to Pred(FSampleVoiceEngineList.Count) do
        begin
          lVoice := TSampleVoiceEngine(FSampleVoiceEngineList[lVoiceIndex]);

          if lVoice.Running then
          begin
            if (lLocalMidiDataCursor.DataValue1 = lVoice.Note) then
            begin
              // Start or restart note at location 'lFrame'
              lVoice.NoteOn(lLocalMidiDataCursor.DataValue1, lFrame);
            end;
          end
          else
          begin
            lVoice.NoteOn(lLocalMidiDataCursor.DataValue1, lFrame);
          end;
        end;

        // Advance to next midi event if available
        if Assigned(lLocalMidiDataCursor.Next) then
        begin
          lLocalMidiDataCursor := lLocalMidiDataCursor.Next;
        end;
      end;

      lLocalCursorAdder += lLocalCursorRatio;
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


//------------------------------------------------------------------------------

(*
  // First clear buffer as it has uninitialized junk inside.
  for lFrame := 0 to Pred(AFrames) do
  begin
    ABuffer[lFrame] := 0;
  end;

  FFrameOffsetLow := ((AMidiGrid.RealCursorPosition div AFrames) * AFrames);
  FFrameOffsetHigh := ((AMidiGrid.RealCursorPosition div AFrames) * AFrames) + AFrames;

  FMidiStartIndex := AMidiGrid.MidiDataList.FrameFirstIndex(FFrameOffsetLow);
  FMidiEndIndex := AMidiGrid.MidiDataList.FrameLastIndex(FFrameOffsetHigh);

  // Missed last note, re process it
  if Succ(FLastMidiDataIndex) < FMidiStartIndex then
  begin;
    FInternalMidiIndex := FMidiStartIndex;
  end
  else
  begin
    // did we wrap around to the beginning of buffer
    if AMidiGrid.RealCursorPosition < FOldRealCursorPosition then
    begin
      FInternalMidiIndex := FMidiStartIndex;
    end
    else
    begin
      FInternalMidiIndex := FLastMidiDataIndex;
    end;
  end;

  // First assign new mididata to running notes
  // Let's if there's notes in the buffer
  for lMidiBufferIndex := FInternalMidiIndex to FMidiEndIndex do
  begin
    lMidiEvent := GetMidiEvent;
    if Assigned(lMidiEvent) then
    begin
      lRelativeLocation := lMidiEvent.Location mod AFrames;

      // Is this note already playing? Yes...now locate the voice
      for lVoiceIndex := 0 to Pred(FSampleVoiceEngineList.Count) do
      begin
        lVoice := TSampleVoiceEngine(FSampleVoiceEngineList[lVoiceIndex]);

        if lVoice.Running then
        begin
          if (lMidiEvent.DataValue1 = lVoice.Note) then
          begin
            lVoice.NoteOn(lMidiEvent.DataValue1, lRelativeLocation);
          end;
        end
        else
        begin
          lVoice.NoteOn(lMidiEvent.DataValue1, lRelativeLocation);
        end;
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

  FLastMidiDataIndex := FInternalMidiIndex;

  FOldRealCursorPosition := AMidiGrid.RealCursorPosition;
*)
end;

{ TSampleVoice - Initializes the voice with the newly assigned TSample }

procedure TSampleVoiceEngine.SetSample(const AValue: TSample);
begin
  FSample := AValue;
end;

constructor TSampleVoiceEngine.Create;
begin
  FAmpEnvelopePosition := 0;
  FFilterEnvelopePosition := 0;
  FPitchEnvelopePosition := 0;
  FLFOPhase := 0;
  FRunning := False;
  FSamplePosition := 0;
  FNote := -1;
  FNoteOnOffset := 0;
end;

procedure TSampleVoiceEngine.Process(AMidiGrid: TMidiGrid; ABuffer: PSingle; AFrames: Integer);
var
  i: Integer;
  lChannel: TChannel;
begin
  if Assigned(FSample.Wave.ChannelList) then
  begin
    if FSample.Wave.ChannelCount > 0 then
    begin
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

          {TODO PitchSemiTone should be replaced with a pitchscale factor}
          if FSamplePosition < FSample.Wave.Frames then
          begin
            FSamplePosition := FSamplePosition + FSample.PitchScaleFactor;
          end
          else
          begin
            // Single shot
            //FSamplePosition := FSample.Wave.Frames;
            FRunning := False;

            // Loop
            //FSamplePosition := 0;;
          end;
        end;

        // Next iteration, start from the beginning
        FNoteOnOffset := 0;
      end;
    end;
  end;
end;

procedure TSampleVoiceEngine.NoteOn(ANote: Integer; ARelativeLocation: Integer);
begin
  FRunning := True;
  FSamplePosition := 0;
  FNoteOnOffset := ARelativeLocation;

  FNote := ANote;
end;

procedure TSampleVoiceEngine.NoteOff;
begin
  FRunning := False;
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
    lSampleEngine.MidiDataList := FMidiDataList;
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

end.

