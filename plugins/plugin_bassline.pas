unit plugin_bassline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, plugin, global_command, tb303, global, globalconst, utils,
  audiostructure;

type
  TTbParameter = (tbPitch, tbCutoff, tbReso, tbEnvMod, tbDecay, tbAccent);

  { TPluginBassline }

  TPluginBassline = class(TPluginNode)
  private
    FTB303: TTB303;
    FLength: Single;
    FPlaying: Boolean;
    function GetPitch: Single;
    function GetCutoff: Single;
    function GetReso: Single;
    function GetEnvMod: Single;
    function GetDecay: Single;
    function GetAccent: Single;
    procedure SetPitch(AValue: Single);
    procedure SetCutoff(AValue: Single);
    procedure SetReso(AValue: Single);
    procedure SetEnvMod(AValue: Single);
    procedure SetDecay(AValue: Single);
    procedure SetAccent(AValue: Single);
  public
    constructor Create(AObjectOwnerID: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); override;
  published
    property EnvMod: Single read GetEnvMod write SetEnvMod;
    property Pitch: Single read GetPitch write SetPitch;
    property Accent: Single read GetAccent write SetAccent;
    property Cutoff: Single read GetCutoff write SetCutoff;
    property Decay: Single read GetDecay write SetDecay;
    property Reso: Single read GetReso write SetReso;
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
  end;

  FBassline.EndUpdate;
end;

{ TBasslineCommand }

procedure TBasslineCommand.Initialize;
begin
  FBassline := TPluginBassline(GObjectMapper.GetModelObject(ObjectID));
end;

{ TPluginBassline }

procedure TPluginBassline.Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer);
var
  i: Integer;
  lOutput: single;
  lMidiEvent: TMidiEvent;
  lMidiBufferIndex: Integer;

  procedure ProcessInternal;
  begin
    while (i < lMidiEvent.RelativeOffset) and (i < AFrames) do
    begin
      lOutput := FTB303.Process;
      ABuffer[i * 2] := lOutput;
      ABuffer[i * 2 + 1] := lOutput;

      Inc(i);
    end;
  end;

begin
  AMidiBuffer.Seek(0);
  for i := 0 to Pred(AFrames) do
  begin
    if AMidiBuffer.Count > 0 then
    begin
      for lMidiBufferIndex := 0 to Pred(AMidiBuffer.Count) do
      begin
        // Shortcut for current event in buffer
        lMidiEvent := AMidiBuffer.ReadEvent;
        if i = lMidiEvent.RelativeOffset then
        begin
          if lMidiEvent.DataType = mtNoteOff then
          begin
//            FTB303.NoteOff;
          end
          else if lMidiEvent.DataType = mtNoteOn then
          begin
            FLength := lMidiEvent.Length;
            FPlaying := True;
            DBLog(format('Note on %d Length %d Offset %d', [
              lMidiEvent.DataValue1, lMidiEvent.Length, lMidiEvent.RelativeOffset]));
            FTB303.NoteOn(lMidiEvent.DataValue1, False, False);
          end
          else if lMidiEvent.DataType = mtVelocity then
          begin
          end;

        end;
      end;
    end;

    if FLength < 1000 then
    begin
      if FPlaying then
      begin
        FPlaying := False;
        DBLog(format('Note off %d Offset %d', [
          lMidiEvent.DataValue1, lMidiEvent.RelativeOffset]));
        FTB303.NoteOff;
      end;
    end
    else
    begin
      FLength := FLength - GAudioStruct.BPMScale;
    end;
    lOutput := FTB303.Process;
    ABuffer[i * 2] := lOutput;
    ABuffer[i * 2 + 1] := lOutput;
  end;

  exit;
  AMidiBuffer.Seek(0);
  i := 0;
  if AMidiBuffer.Count > 0 then
  begin
    for lMidiBufferIndex := 0 to Pred(AMidiBuffer.Count) do
    begin
      // Shortcut for current event in buffer
      lMidiEvent := AMidiBuffer.ReadEvent;

      ProcessInternal;

      if lMidiEvent.DataType = mtNoteOff then
      begin
        FTB303.NoteOff;
      end
      else if lMidiEvent.DataType = mtNoteOn then
      begin
        FTB303.NoteOff;
        FTB303.NoteOn(lMidiEvent.DataValue1, False, False);
      end
      else if lMidiEvent.DataType = mtVelocity then
      begin
      end;
    end;

    ProcessInternal;
  end
  else
  begin
    for i := 0 to Pred(AFrames) do
    begin
      lOutput := FTB303.Process;
      ABuffer[i * 2] := lOutput;
      ABuffer[i * 2 + 1] := lOutput;
    end;
  end;

//  FTB303.process(ABuffer, ABuffer, AFrames);
end;

function TPluginBassline.GetPitch: Single;
begin
//  Result := FTB303.getPitch;
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

constructor TPluginBassline.Create(AObjectOwnerID: string; AMapped: Boolean = True);
begin
  inherited Create(AObjectOwnerID, AMapped);

  FTB303 := TTB303.Create(GSettings.SampleRate);
end;

destructor TPluginBassline.Destroy;
begin
  FTB303.Free;

  inherited Destroy;
end;


end.

