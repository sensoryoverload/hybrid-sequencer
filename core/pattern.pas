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

  pattern.pas
}

unit pattern;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, waveform, midi, graphics, global_command,
  globalconst, global, fx, plugin, pluginhost, sampler;

type

  { TPattern }

  TPattern = class(THybridPersistentModel)
  private
    FWaveForm: TWaveForm;
    FMidiGrid: TMidiGrid;
    FPatternColor: TColor;
    FPitched: Boolean;
    FPosition: Integer; // Vertical position in the patterngrid
    FText: string;
    FSyncQuantize: Boolean;
    FOkToPlay: Boolean;
    FPitch: Single;
    FRootNote: Integer;
    FMidiChannel: Integer;
    FPlaying: Boolean;
    FScheduled: Boolean;
    FPatternLength: Longint;
    FPatternName: string;
    FPluginProcessor: TPluginProcessor;
    FFilter: TDecimateFX;
    FFilter2: TMoogFilter;

    // Private sampler, not a plugin so it's more thightly integrated
    FLocalSampleBank: TSampleBank;
    FLocalSampleBankEngine: TSampleBankEngine;

    FSample: TSample;

    procedure SetOkToPlay(const AValue: Boolean);
    procedure SetPatternColor(const AValue: TColor);
    procedure SetPosition(const AValue: Integer);
    procedure SetText(const AValue: string);
    procedure Setpitch(const Avalue: Single);
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure RecalculateSynchronize;
    procedure Initialize; override;

    property LocalSampleBankEngine: TSampleBankEngine read FLocalSampleBankEngine write FLocalSampleBankEngine;
    property PatternColor: TColor read FPatternColor write SetPatternColor;

  published
    property WaveForm: TWaveForm read FWaveForm write FWaveForm;
    property MidiGrid: TMidiGrid read FMidiGrid write FMidiGrid;
    property LocalSampleBank: TSampleBank read FLocalSampleBank write FLocalSampleBank;
    property PluginProcessor: TPluginProcessor read FPluginProcessor write FPluginProcessor;
    property SyncQuantize: Boolean read FSyncQuantize write FSyncQuantize;
    property Position: Integer read FPosition write SetPosition;
    property Text: string read FText write SetText;
    property OkToPlay: Boolean read FOkToPlay write SetOkToPlay;
    property Pitch: Single read FPitch write SetPitch default 1;
    property Pitched: Boolean read FPitched write FPitched default False;
    property RootNote: Integer read FRootNote write FRootNote default 0;
    property MidiChannel: Integer read FMidiChannel write FMidiChannel;
    property Playing: Boolean read FPlaying write FPlaying default False;
    property Scheduled: Boolean read FScheduled write FScheduled default False;
    property PatternLength: Longint read FPatternLength write FPatternLength;
    property PatternName: string read FPatternName write FPatternName;
  end;

  { TPatternCommand }

  TPatternCommand = class(TCommand)
  private
    FPattern: TPattern;
  protected
    procedure Initialize; override;
  end;

  { TPatternDropWaveCommand }

  TPatternDropWaveCommand = class(TPatternCommand)
  private
    FFileName: string;
    FOldFileName: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property FileName: string read FFileName write FFileName;
  end;

  { TTogglePitchCommand }

  TTogglePitchCommand = class(TPatternCommand)
  private
    FState: Boolean;
    FOldState: Boolean;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property State: Boolean read FState write FState;
  end;

  { TChangePitchCommand }

  TChangePitchCommand = class(TPatternCommand)
  private
    FPitch: Single;
    FOldPitch: Single;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Pitch: Single read FPitch write FPitch;
  end;

  { TChangeOriginalBPMCommand }

  TChangeRealBPMCommand = class(TPatternCommand)
  private
    FRealBPM: Single;
    FOldRealBPM: Single;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;

  published
    property RealBPM: Single read FRealBPM write FRealBPM;
  end;

  { TChangeMidiChannelCommand }

  TChangeMidiChannelCommand = class(TPatternCommand)
  private
    FMidiChannel: Integer;
    FOldMidiChannel: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;

  published
    property MidiChannel: Integer read FMidiChannel write FMidiChannel;
  end;

  { TChangeThresHoldCommand }

  TChangeThresHoldCommand = class(TPatternCommand)
  private
    FThreshold: Integer;
    FOldThreshold: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;

  published
    property Threshold: Integer read FThreshold write FThreshold;
  end;

  { TChangeStretchAlgoCommand }

  TChangeStretchAlgoCommand = class(TPatternCommand)
  private
    FAlgoType: Boolean;
    FOldAlgoType: Boolean;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;

  published
    property AlgoType: Boolean read FAlgoType write FAlgoType;
  end;

implementation

uses
  utils;

{ TPattern }

procedure TPattern.SetPatternColor(const AValue: TColor);
begin
  FPatternColor := AValue;
end;

procedure TPattern.SetOkToPlay(const AValue: Boolean);
begin
  FOkToPlay := AValue;
end;

procedure TPattern.SetPosition(const AValue: Integer);
begin
  FPosition := AValue;
end;

procedure TPattern.SetText(const AValue: string);
begin
  FText := AValue;
end;

procedure TPattern.Setpitch(const Avalue: Single);
begin
  if Avalue > 8 then
    FPitch := 8
  else if Avalue < 0.1 then
    FPitch := 0.1
  else
    FPitch := Avalue;
end;

procedure TPattern.DoCreateInstance(var AObject: TObject; AClassName: string);
begin
  DBLog('start TPattern.DoCreateInstance');

  DBLog('end TPattern.DoCreateInstance');
end;


procedure TPattern.Initialize;
begin
  Self.WaveForm.Initialize;
  Self.MidiGrid.Initialize;

  Notify;
end;

constructor TPattern.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  DBLog('start TPattern.Create');

  inherited Create(AObjectOwner, AMapped);

  FOnCreateInstanceCallback := @DoCreateInstance;

  FMidiGrid := TMidiGrid.Create(AObjectOwner, AMapped);
  FWaveForm := TWaveForm.Create(AObjectOwner, AMapped);

  FLocalSampleBank := TSampleBank.Create(AObjectOwner, AMapped);
  FLocalSampleBankEngine := TSampleBankEngine.Create(GSettings.Frames);

  FSample := TSample.Create(AObjectOwner, AMapped);
  FSample.LoadSample('kick.wav');
  FSample.Initialize;

  FLocalSampleBank.SampleList.Add(FSample);

  FLocalSampleBankEngine.SampleBank := FLocalSampleBank;

  DBLog('KICK.WAV loading');

  FPluginProcessor := TPluginProcessor.Create(GSettings.Frames, AObjectOwner, AMapped);
  {
  FFilter := TDecimateFX.Create(FPluginProcessor.ObjectID);
  FFilter.PluginName := 'DecimateFX';
  FFilter.Init(16, 44100);
  FFilter2 := TMoogFilter.Create(FPluginProcessor.ObjectID);
  FFilter2.PluginName := 'MoogFilter';
  FFilter2.Frequency := 20000;
  FFilter2.Resonance := 0.0;

  FPluginProcessor.InsertNode(FFilter, FPluginProcessor.AudioOut, FPluginProcessor.AudioIn);
  FPluginProcessor.InsertNode(FFilter2, FFilter, FPluginProcessor.AudioIn);
  }

  RecalculateSynchronize;

  FOkToPlay := False;

  DBLog('end TPattern.Create');
end;

destructor TPattern.Destroy;
begin
  FPluginProcessor.Free;
  //FFilter.Free;
  //FFilter2.Free;

  FWaveForm.Free;
  FMidiGrid.Free;

  FLocalSampleBankEngine.Free;

  FSample.UnloadSample; // TODO Should be done by TSample class itself
  FSample.Free;
  FLocalSampleBank.Free;

  inherited Destroy;
end;

procedure TPattern.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TPattern.RecalculateSynchronize;
begin
  //FPatternLength := Round(WaveForm.SampleRate * (60 / FRealBPM)) * 4; // TODO choose next multiple of 4
end;

{ TPatternDropWaveCommand }

procedure TPatternDropWaveCommand.DoExecute;
begin
  DBLog('start TPatternDropWaveCommand.DoExecute');

  FPattern.BeginUpdate;

  FOldFileName := FPattern.WaveForm.SampleFileName;
  FPattern.WaveForm.SampleFileName := FFileName;
  FPattern.Initialize;
  FPattern.OkToPlay := True;

  FPattern.EndUpdate;

  DBLog('end TPatternDropWaveCommand.DoExecute');
end;

procedure TPatternDropWaveCommand.DoRollback;
begin
  DBLog('start TPatternDropWaveCommand.DoRollback');

  FPattern.BeginUpdate;

  FPattern.OkToPlay := False;
  FPattern.WaveForm.SampleFileName := FFileName;
  FPattern.WaveForm.UnLoadSample;

  FPattern.EndUpdate;

  DBLog('end TPatternDropWaveCommand.DoRollback');
end;

{ TTogglePitchCommand }

procedure TTogglePitchCommand.DoExecute;
begin
  DBLog('start TTogglePitchCommand.DoExecute');

  FPattern.BeginUpdate;

  FOldState := FState;
  FPattern.Pitched := FState;

  FPattern.EndUpdate;

  DBLog('end TTogglePitchCommand.DoExecute');
end;

procedure TTogglePitchCommand.DoRollback;
begin
  DBLog('start TTogglePitchCommand.DoRollback');

  FPattern.BeginUpdate;

  FPattern.Pitched := FOldState;

  FPattern.EndUpdate;

  DBLog('end TTogglePitchCommand.DoRollback');
end;

{ TChangePitchCommand }

procedure TChangePitchCommand.DoExecute;
begin
  DBLog('start TChangePitchCommand.DoExecute');

  // Store pitch
  FOldpitch := Pitch;
  FPattern.Pitch := Pitch;
  FPattern.Notify;

  DBLog('end TChangePitchCommand.DoExecute');
end;

procedure TChangePitchCommand.DoRollback;
begin
  DBLog('start TChangePitchCommand.DoRollback');

  FPattern.BeginUpdate;

  // Restore pitch
  FPattern.Pitch := FOldpitch;

  FPattern.EndUpdate;

  DBLog('end TChangePitchCommand.DoRollback');
end;

{ TChangeThresHoldCommand }

procedure TChangeThresHoldCommand.DoExecute;
begin
  DBLog('start TChangeThresHoldCommand.DoExecute');

  FPattern.BeginUpdate;

  // Store Threshold
  FOldThreshold := FPattern.WaveForm.TransientThreshold;

  FPattern.EndUpdate;

  DBLog('end TChangeThresHoldCommand.DoExecute');
end;

procedure TChangeThresHoldCommand.DoRollback;
begin
  DBLog('start TChangeThresHoldCommand.DoRollback');

  FPattern.BeginUpdate;

  // Restore Threshold
  FPattern.WaveForm.TransientThreshold := FOldThreshold;

  FPattern.EndUpdate;

  DBLog('end TChangeThresHoldCommand.DoRollback');
end;

{ TChangeOriginalBPMCommand }

procedure TChangeRealBPMCommand.DoExecute;
begin
  DBLog('start TChangeRealBPMCommand.DoExecute');

  FPattern.BeginUpdate;

  // Store RealBPM
  FOldRealBPM := FRealBPM;
  FPattern.WaveForm.RealBPM := FRealBPM;

  FPattern.EndUpdate;

  DBLog('end TChangeRealBPMCommand.DoExecute');
end;

procedure TChangeRealBPMCommand.DoRollback;
begin
  DBLog('start TChangeRealBPMCommand.DoRollback');

  FPattern.BeginUpdate;

  // Restore RealBPM
  FPattern.WaveForm.RealBPM := FOldRealBPM;

  FPattern.EndUpdate;

  DBLog('end TChangeRealBPMCommand.DoRollback');
end;

{ TChangeStretchAlgoCommand }

procedure TChangeStretchAlgoCommand.DoExecute;
begin
  DBLog('start TChangeStretchAlgoCommand.DoExecute');

  FPattern.BeginUpdate;

  FOldAlgoType := FAlgoType;
  FPattern.Pitched := FAlgoType;

  FPattern.EndUpdate;

  DBLog('end TChangeStretchAlgoCommand.DoExecute');
end;

procedure TChangeStretchAlgoCommand.DoRollback;
begin
  DBLog('start TChangeStretchAlgoCommand.DoRollback');

  FPattern.BeginUpdate;

  FPattern.Pitched := FOldAlgoType;

  FPattern.EndUpdate;

  DBLog('end TChangeStretchAlgoCommand.DoRollback');
end;

{ TPatternCommand }

procedure TPatternCommand.Initialize;
begin
  FPattern := TPattern(GObjectMapper.GetModelObject(ObjectOwner));
end;

{ TChangeMidiChannelCommand }

procedure TChangeMidiChannelCommand.DoExecute;
begin
  DBLog('start TChangeMidiChannelCommand.DoExecute');

  FPattern.BeginUpdate;

  // Store MidiChannel
  FOldMidiChannel := FMidiChannel;
  FPattern.MidiChannel := FMidiChannel;
  writeln(Format('Change midiChannel to %d',[FPattern.MidiChannel]));

  FPattern.EndUpdate;

  DBLog('end TChangeMidiChannelCommand.DoExecute');
end;

procedure TChangeMidiChannelCommand.DoRollback;
begin
  DBLog('start TChangeMidiChannelCommand.DoRollback');

  FPattern.BeginUpdate;

  FPattern.MidiChannel := FOldMidiChannel;
  writeln(Format('Change midiChannel to %d',[FPattern.MidiChannel]));

  FPattern.EndUpdate;

  DBLog('end TChangeMidiChannelCommand.DoRollback');
end;

end.

