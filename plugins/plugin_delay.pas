unit plugin_delay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, plugin, global_command, global, globalconst, pluginhost, fx;

type
  TDelayParameter = (
    dpLeftDelay,
    dpRightDelay,
    dpLeftFeedback,
    dpRightFeedback,
    dpLeftPanning,
    dpRightPanning);

  { TPluginDelay }

  TPluginDelay = class(TPluginNode)
  private
    FLeftDelay: single;
    FLeftFeedback: single;
    FLeftPanning: single;
    FLeftPanGainL: single;
    FLeftPanGainR: single;
    FRightDelay: single;
    FRightFeedback: single;
    FRightPanning: single;
    FRightPanGainL: single;
    FRightPanGainR: single;

    FLeftDelayBuffer: TAudioRingBuffer;
    FRightDelayBuffer: TAudioRingBuffer;
    procedure SetLeftPanning(AValue: single);
    procedure SetPanning(APanValue: Single; var ALeftPan, ARightPan: single);
    procedure SetRightPanning(AValue: single);
  public
    constructor Create(AObjectOwnerID: string; AMapped: Boolean = True); override;
    destructor Destroy; override;
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
      AOutputBuffer: PSingle; AFrames: Integer); override;
    procedure Instantiate; override;
    procedure UpdateParameters; override;
  published
    property LeftDelay: single read FLeftDelay write FLeftDelay;
    property LeftFeedback: single read FLeftFeedback write FLeftFeedback;
    property LeftPanning: single read FLeftPanning write SetLeftPanning;
    property RightDelay: single read FRightDelay write FRightDelay;
    property RightFeedback: single read FRightFeedback write FRightFeedback;
    property RightPanning: single read FRightPanning write SetRightPanning;
  end;

  { TDelayCommand }

  TDelayCommand = class(TCommand)
  private
    FOldValue: Variant;
    FValue: Variant;
    FPluginDelay: TPluginDelay;
    FParameter: TDelayParameter;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  public
    procedure Initialize; override;
    property Value: Variant read FValue write FValue;
    property Parameter: TDelayParameter read FParameter write FParameter;
  end;

implementation

{ TBasePluginCommand }

procedure TDelayCommand.Initialize;
begin
   FPluginDelay := TPluginDelay(GObjectMapper.GetModelObject(ObjectOwner));
end;

{ TDelayCommand }

procedure TDelayCommand.DoExecute;
begin
  FPluginDelay.BeginUpdate;

  case FParameter of
    dpLeftDelay:
    begin;
      FOldValue := FPluginDelay.LeftDelay;
      FPluginDelay.LeftDelay := FValue;
    end;
  end;

  FPluginDelay.EndUpdate;
end;

procedure TDelayCommand.DoRollback;
begin
  FPluginDelay.BeginUpdate;

  case FParameter of
    dpLeftDelay:
    begin;
      FPluginDelay.LeftDelay := FOldValue;
    end;
  end;

  FPluginDelay.EndUpdate;
end;


{ TPluginDelay }

procedure TPluginDelay.SetLeftPanning(AValue: single);
begin
  if FLeftPanning=AValue then Exit;
  FLeftPanning:=AValue;

  SetPanning(FLeftPanning, FLeftPanGainL, FLeftPanGainR);
end;

procedure TPluginDelay.SetRightPanning(AValue: single);
begin
  if FRightPanning=AValue then Exit;
  FRightPanning:=AValue;

  SetPanning(FRightPanning, FRightPanGainL, FRightPanGainR);
end;

constructor TPluginDelay.Create(AObjectOwnerID: string; AMapped: Boolean);
begin
  inherited;

  FLeftDelay := 1000;
  FLeftFeedback := 0.5;
  FLeftPanning := 0;
  FLeftDelayBuffer := TAudioRingBuffer.Create(Round(GSettings.SampleRate), STEREO);
  FLeftDelayBuffer.DelayMs := Round(FLeftDelay);

  FRightDelay := 1000;
  FRightFeedback := 0.5;
  FRightPanning := 0;
  FRightDelayBuffer := TAudioRingBuffer.Create(Round(GSettings.SampleRate), STEREO);
  FRightDelayBuffer.DelayMs := Round(FRightDelay);
end;

destructor TPluginDelay.Destroy;
begin
  FLeftDelayBuffer.Free;
  FRightDelayBuffer.Free;

  inherited Destroy;
end;

procedure TPluginDelay.Process(AMidiBuffer: TMidiBuffer;
  AInputBuffer: PSingle; AOutputBuffer: PSingle;
  AFrames: Integer);
var
  lIndex: Integer;
  lLeftOffset: Integer;
  lRightOffset: Integer;
  lLeftSample: Single;
  lRightSample: Single;
begin
  lLeftOffset := 0;
  lRightOffset := 1;
  for lIndex := 0 to Pred(AFrames) do
  begin
    lLeftSample :=
      FLeftDelayBuffer.Process(AInputBuffer[lLeftOffset]) +   // Put input into delaybuffer
      AInputBuffer[lLeftOffset];                              // Mix dry signal

    lRightSample :=
      FRightDelayBuffer.Process(AInputBuffer[lRightOffset]) + // Put input into delaybuffer
      AInputBuffer[lRightOffset];                             // Mix dry signal

    AOutputBuffer[lLeftOffset] := lLeftSample;
    AOutputBuffer[lRightOffset] := lRightSample;

    Inc(lLeftOffset, STEREO);
    Inc(lRightOffset, STEREO);
  end;
end;

procedure TPluginDelay.Instantiate;
begin
  CreatePortParameter('Left delay', 0, 1, True, True, False, True, False, False, 1, LeftDelay, nil);
  CreatePortParameter('Left feedback', 0, 1, True, True, False, True, False, False, 1, LeftFeedback, nil);
  CreatePortParameter('Left panning', 0, 1, True, True, False, True, False, False, 1, LeftPanning, nil);
  CreatePortParameter('Right delay', 0, 1, True, True, False, True, False, False, 1, RightDelay, nil);
  CreatePortParameter('Right feedback', 0, 1, True, True, False, True, False, False, 1, RightFeedback, nil);
  CreatePortParameter('Right panning', 0, 1, True, True, True, True, False, False, 1, RightPanning, nil);

  inherited;
end;

procedure TPluginDelay.UpdateParameters;
begin
  LeftDelay := InputControls[0].Value;
  LeftFeedback := InputControls[1].Value;
  LeftPanning := InputControls[2].Value;
  RightDelay := InputControls[3].Value;
  RightFeedback := InputControls[4].Value;
  RightPanning := Round(InputControls[5].Value);
end;

procedure TPluginDelay.SetPanning(APanValue: Single; var ALeftPan, ARightPan: single);
begin
  ALeftPan := (1 - APanValue) * (0.7 + 0.2 * APanValue);
  ARightPan := (1 + APanValue) * (0.7 - 0.2 * APanValue);
end;

end.

