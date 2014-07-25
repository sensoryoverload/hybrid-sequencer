unit plugin_bassline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, plugin, global_command, tb303, global, globalconst, utils,
  audiostructure;

const
  PLUGIN_NAME_BASSLINE = 'Bassline';
  DENORMAL_KILLER = 1E-20;

type
  TTbParameter = (tbPitch, tbCutoff, tbReso, tbEnvMod, tbDecay, tbAccent, tbWave,
    tbOverdrive);

  { TPluginBassline }

  TPluginBassline = class(TPluginNode)
  private
    FTB303: TTB303;
    FLength: Single;
    FPlaying: Boolean;
    FOverDrive: Single;
    FLatency: Integer;
    function GetPitch: Single;
    function GetCutoff: Single;
    function GetReso: Single;
    function GetEnvMod: Single;
    function GetDecay: Single;
    function GetAccent: Single;
    function GetWave: Integer;
    function GetOverDrive: Single;
    procedure SetPitch(AValue: Single);
    procedure SetCutoff(AValue: Single);
    procedure SetReso(AValue: Single);
    procedure SetEnvMod(AValue: Single);
    procedure SetDecay(AValue: Single);
    procedure SetAccent(AValue: Single);
    procedure SetWave(AValue: Integer);
    procedure SetOverDrive(AValue: Single);
  public
    constructor Create(AObjectOwnerID: string; AMapped: Boolean = True); reintroduce;
    destructor Destroy; override;
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
      AOutputBuffer: PSingle; AFrames: Integer); override;
    function GetLatency: Integer; override;
    procedure SetLatency(AValue: Integer); override;
    procedure Instantiate; override;
    procedure UpdateParameters; override;
  published
    property EnvMod: Single read GetEnvMod write SetEnvMod;
    property Pitch: Single read GetPitch write SetPitch;
    property Accent: Single read GetAccent write SetAccent;
    property Cutoff: Single read GetCutoff write SetCutoff;
    property Decay: Single read GetDecay write SetDecay;
    property Reso: Single read GetReso write SetReso;
    property Wave: Integer read GetWave write SetWave;
    property OverDrive: Single read GetOverDrive write SetOverDrive;
  end;


  { TBasslineCommand }

  TBasslineCommand = class(TCommand)
  private
    FBassline: TPluginBassline;
  public
    procedure Initialize; override;
  end;

  { TBasslineParameterCommand }

  TBasslineParameterCommand = class(TBasslineCommand)
  private
    FOldValue: Variant;
    FValue: Variant;
    FParameter: TTbParameter;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Value: Variant read FValue write FValue;
    property Parameter: TTbParameter read FParameter write FParameter;
  end;


implementation

uses
  fx;

{ TBasslineParameterCommand }

procedure TBasslineParameterCommand.DoExecute;
begin
  FBassline.BeginUpdate;

  case FParameter of
    tbPitch:
    begin
      FOldValue := FBassline.Pitch;
      FBassline.Pitch := FValue;
    end;
    tbCutoff:
    begin
      FOldValue := FBassline.Cutoff;
      FBassline.Cutoff := FValue;
    end;
    tbReso:
    begin
      FOldValue := FBassline.Reso;
      FBassline.Reso := FValue;
    end;
    tbEnvMod:
    begin
      FOldValue := FBassline.EnvMod;
      FBassline.EnvMod := FValue;
    end;
    tbDecay:
    begin
      FOldValue := FBassline.Decay;
      FBassline.Decay := FValue;
    end;
    tbAccent:
    begin
      FOldValue := FBassline.Accent;
      FBassline.Accent := FValue;
    end;
    tbWave:
    begin
      FOldValue := FBassline.Wave;
      FBassline.Wave := FValue;
    end;
    tbOverdrive:
    begin
      FOldValue := FBassline.OverDrive;
      FBassline.OverDrive := FValue;
    end;
  end;

  FBassline.EndUpdate;
end;

procedure TBasslineParameterCommand.DoRollback;
begin
  FBassline.BeginUpdate;

  case FParameter of
    tbPitch:
    begin
      FBassline.Pitch := FOldValue;
    end;
    tbCutoff:
    begin
      FBassline.Cutoff := FOldValue;
    end;
    tbReso:
    begin
      FBassline.Reso := FOldValue;
    end;
    tbEnvMod:
    begin
      FBassline.EnvMod := FOldValue;
    end;
    tbDecay:
    begin
      FBassline.Decay := FOldValue;
    end;
    tbAccent:
    begin
      FBassline.Accent := FOldValue;
    end;
    tbWave:
    begin
      FBassline.Wave := FOldValue;
    end;
    tbOverdrive:
    begin
      FBassline.OverDrive := FOldValue;
    end;
  end;

  FBassline.EndUpdate;
end;

{ TBasslineCommand }

procedure TBasslineCommand.Initialize;
begin
  FBassline := TPluginBassline(GObjectMapper.GetModelObject(ObjectID));
end;

{ TPluginBassline }

procedure TPluginBassline.Process(AMidiBuffer: TMidiBuffer;
  AInputBuffer: PSingle; AOutputBuffer: PSingle; AFrames: Integer);
var
  i: Integer;
  lOutput: single;
  lOffsetL: Integer;
  lOffsetR: Integer;
  lMidiEvent: TMidiEvent;
  lMidiBufferIndex: Integer;
begin
  Inherited;

  if AMidiBuffer.Count > 0 then
  begin
    DBLog(Format('AMidiBuffer.Count %d', [AMidiBuffer.Count]));
  end;
  lOffsetL := 0;
  lOffsetR := 1;
  for i := 0 to Pred(AFrames) do
  begin
    if AMidiBuffer.Count > 0 then
    begin
      AMidiBuffer.Seek(0);
      for lMidiBufferIndex := 0 to Pred(AMidiBuffer.Count) do
      begin
        // Shortcut for current event in buffer
        lMidiEvent := AMidiBuffer.ReadEvent;
        if i = lMidiEvent.RelativeOffset then
        begin
          if lMidiEvent.DataType = mtNoteOn then
          begin
            FLength := lMidiEvent.Length;
            FPlaying := True;
            FTB303.NoteOn(lMidiEvent.DataValue1, lMidiEvent.DataValue2 > 100, False);
          end
        end;
      end;
    end;

    if FLength < 10 then
    begin
      if FPlaying then
      begin
        FPlaying := False;
        FTB303.NoteOff;
      end;
    end
    else
    begin
      FLength := FLength - GAudioStruct.BPMScale;
    end;
    lOutput :=  FTB303.Process * FOverDrive;
    if lOutput > DENORMAL_KILLER then
    begin
      lOutput := tanh2(lOutput);
    end;
    AOutputBuffer[lOffsetL] := lOutput;
    AOutputBuffer[lOffsetR] := lOutput;
    Inc(lOffsetL, 2);
    Inc(lOffsetR, 2);
  end;
end;

function TPluginBassline.GetLatency: Integer;
begin
  Result := FLatency;
end;

procedure TPluginBassline.SetLatency(AValue: Integer);
begin
  FLatency := AValue;
end;

procedure TPluginBassline.Instantiate;
begin
  CreatePortParameter('Cutoff', 0, 1, True, True, False, True, False, False, 1, FTB303.Cutoff, @FTB303.setcut);
  CreatePortParameter('Resonance', 0, 1, True, True, False, True, False, False, 1, FTB303.Resonance, @FTB303.setres);
  CreatePortParameter('Envelope', 0, 1, True, True, False, True, False, False, 1, FTB303.EnvMod, @FTB303.setenvmod);
  CreatePortParameter('Decay', 0, 1, True, True, False, True, False, False, 1, FTB303.EnvDecay, @FTB303.setenvdec);
  CreatePortParameter('Accent', 0, 1, True, True, False, True, False, False, 1, FTB303.AccAmt, @FTB303.setaccamt);
  CreatePortParameter('Wave', 0, 1, True, True, True, True, False, False, 1, FTB303.Waveform, @FTB303.setwaveform);
  CreatePortParameter('Overdrive', 1, 20, True, True, False, True, False, False, 1, FOverDrive, @SetOverDrive);

  inherited;
end;

procedure TPluginBassline.UpdateParameters;
begin
  FTB303.Cutoff := InputControls[0].Value;
  FTB303.Resonance := InputControls[1].Value;
  FTB303.EnvMod := InputControls[2].Value;
  FTB303.EnvDecay := InputControls[3].Value;
  FTB303.AccAmt := InputControls[4].Value;
  FTB303.Waveform := Round(InputControls[5].Value);
  FOverDrive := InputControls[6].Value;
end;

function TPluginBassline.GetPitch: Single;
begin
  Result := 1; //FTB303.getPitch;
end;

function TPluginBassline.GetCutoff: Single;
begin
  Result := FTB303.Cutoff;
end;

function TPluginBassline.GetReso: Single;
begin
  Result := FTB303.Resonance;
end;

function TPluginBassline.GetEnvMod: Single;
begin
  Result := FTB303.EnvMod;
end;

function TPluginBassline.GetDecay: Single;
begin
  Result := FTB303.EnvDecay;
end;

function TPluginBassline.GetAccent: Single;
begin
  Result := FTB303.AccAmt;
end;

function TPluginBassline.GetWave: Integer;
begin
  Result := FTB303.Waveform;
end;

function TPluginBassline.GetOverDrive: Single;
begin
  Result := FOverDrive;
end;

procedure TPluginBassline.SetPitch(AValue: Single);
begin
  //FTB303.setPitch(AValue);
end;

procedure TPluginBassline.SetCutoff(AValue: Single);
begin
  FTB303.Cutoff := AValue;
end;

procedure TPluginBassline.SetReso(AValue: Single);
begin
  FTB303.Resonance := AValue;
end;

procedure TPluginBassline.SetEnvMod(AValue: Single);
begin
  FTB303.EnvMod := AValue;
end;

procedure TPluginBassline.SetDecay(AValue: Single);
begin
  FTB303.EnvDecay := AValue;
end;

procedure TPluginBassline.SetAccent(AValue: Single);
begin
  FTB303.AccAmt := AValue;
end;

procedure TPluginBassline.SetWave(AValue: Integer);
begin
  FTB303.Waveform := AValue;
end;

procedure TPluginBassline.SetOverDrive(AValue: Single);
begin
  FOverDrive := AValue;
end;

constructor TPluginBassline.Create(AObjectOwnerID: string; AMapped: Boolean = True);
begin
  inherited Create(AObjectOwnerID, AMapped);

  FOverDrive := 1;

  PluginName := PLUGIN_NAME_BASSLINE;

  FTB303 := TTB303.Create(GSettings.SampleRate);
end;

destructor TPluginBassline.Destroy;
begin
  FTB303.Free;

  inherited Destroy;
end;


end.

