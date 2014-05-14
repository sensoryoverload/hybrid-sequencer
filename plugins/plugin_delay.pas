unit plugin_delay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, plugin, global_command, global, globalconst, pluginhost, fx;

type
  TDelayParameter = (
    dpDelay1,
    dpDelay2,
    dpFeedback1,
    dpFeedback2,
    dpPanning1,
    dpPanning2);

  { TPluginDelay }

  TPluginDelay = class(TPluginNode)
  private
    FDelay1: single;
    FFeedback1: single;
    FPanning1: single;
    FPanGainL1: single;
    FPanGainR1: single;
    FDelay2: single;
    FFeedback2: single;
    FPanning2: single;
    FPanGainL2: single;
    FPanGainR2: single;

    FDelayBufferL1: TAudioRingBuffer;
    FDelayBufferR1: TAudioRingBuffer;
    FDelayBufferL2: TAudioRingBuffer;
    FDelayBufferR2: TAudioRingBuffer;
    procedure SetPanning1(AValue: single);
    procedure SetPanning(APanValue: Single; var ALeftPan, ARightPan: single);
    procedure SetPanning2(AValue: single);
  public
    constructor Create(AObjectOwnerID: string; AMapped: Boolean = True); override;
    destructor Destroy; override;
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
      AOutputBuffer: PSingle; AFrames: Integer); override;
    procedure Instantiate; override;
    procedure UpdateParameters; override;
  published
    property Delay1: single read FDelay1 write FDelay1;
    property Feedback1: single read FFeedback1 write FFeedback1;
    property Panning1: single read FPanning1 write SetPanning1;
    property Delay2: single read FDelay2 write FDelay2;
    property Feedback2: single read FFeedback2 write FFeedback2;
    property Panning2: single read FPanning2 write SetPanning2;
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
    dpDelay1:
    begin;
      FOldValue := FPluginDelay.Delay1;
      FPluginDelay.Delay1 := FValue;
    end;
  end;

  FPluginDelay.EndUpdate;
end;

procedure TDelayCommand.DoRollback;
begin
  FPluginDelay.BeginUpdate;

  case FParameter of
    dpDelay1:
    begin;
      FPluginDelay.Delay1 := FOldValue;
    end;
  end;

  FPluginDelay.EndUpdate;
end;


{ TPluginDelay }

procedure TPluginDelay.SetPanning1(AValue: single);
begin
  if FPanning1=AValue then Exit;
  FPanning1:=AValue;

  SetPanning(FPanning1, FPanGainL1, FPanGainR1);
end;

procedure TPluginDelay.SetPanning2(AValue: single);
begin
  if FPanning2=AValue then Exit;
  FPanning2:=AValue;

  SetPanning(FPanning2, FPanGainL2, FPanGainR2);
end;

constructor TPluginDelay.Create(AObjectOwnerID: string; AMapped: Boolean);
begin
  inherited;

  FDelay1 := 1000;
  FFeedback1 := 0.5;
  FPanning1 := 0;
  FDelayBufferL1 := TAudioRingBuffer.Create(Round(GSettings.SampleRate));
  FDelayBufferL1.DelayMs := Round(FDelay1);
  FDelayBufferR1 := TAudioRingBuffer.Create(Round(GSettings.SampleRate));
  FDelayBufferR1.DelayMs := Round(FDelay1);

  FDelay2 := 1000;
  FFeedback2 := 0.5;
  FPanning2 := 0;
  FDelayBufferL2 := TAudioRingBuffer.Create(Round(GSettings.SampleRate));
  FDelayBufferL2.DelayMs := Round(FDelay2);
  FDelayBufferR2 := TAudioRingBuffer.Create(Round(GSettings.SampleRate));
  FDelayBufferR2.DelayMs := Round(FDelay2);
end;

destructor TPluginDelay.Destroy;
begin
  FDelayBufferL1.Free;
  FDelayBufferR1.Free;
  FDelayBufferL2.Free;
  FDelayBufferR2.Free;

  inherited Destroy;
end;

procedure TPluginDelay.Process(AMidiBuffer: TMidiBuffer;
  AInputBuffer: PSingle; AOutputBuffer: PSingle;
  AFrames: Integer);
var
  lIndex: Integer;
  lOffsetL: Integer;
  lOffsetR: Integer;
  lSampleL1: Single;
  lSampleR1: Single;
  lSampleL2: Single;
  lSampleR2: Single;
begin
  lOffsetL := 0;
  lOffsetR := 1;
  for lIndex := 0 to Pred(AFrames) do
  begin
    // Channel 1 - Left
    lSampleL1 :=
      FDelayBufferL1.Process(AInputBuffer[lOffsetL] +   // Put input into delaybuffer
      lSampleL1 * FFeedback1) +                         // Feed output in input
      AInputBuffer[lOffsetL];                           // Mix dry signal

    // Channel 1 - Right
    lSampleR1 :=
      FDelayBufferR2.Process(AInputBuffer[lOffsetR] +   // Put input into delaybuffer
      lSampleR1 * FFeedback1) +                         // Feed output in input
      AInputBuffer[lOffsetR];                           // Mix dry signal

    // Channel 2 - Left
    lSampleL2 :=
      FDelayBufferL2.Process(AInputBuffer[lOffsetL] +   // Put input into delaybuffer
      lSampleL2 * FFeedback2) +                         // Feed output in input
      AInputBuffer[lOffsetL];                           // Mix dry signal

    // Channel 2 - Right
    lSampleR2 :=
      FDelayBufferR2.Process(AInputBuffer[lOffsetR] +   // Put input into delaybuffer
      lSampleR2 * FFeedback2) +                         // Feed output in input
      AInputBuffer[lOffsetR];                           // Mix dry signal

    AOutputBuffer[lOffsetL] := lSampleL1 + lSampleL2;
    AOutputBuffer[lOffsetR] := lSampleR1 + lSampleR2;

    Inc(lOffsetL, STEREO);
    Inc(lOffsetR, STEREO);
  end;
end;

procedure TPluginDelay.Instantiate;
begin
  CreatePortParameter('Left delay', 0, 1, True, True, False, True, False, False, 1, Delay1, nil);
  CreatePortParameter('Left feedback', 0, 1, True, True, False, True, False, False, 1, Feedback1, nil);
  CreatePortParameter('Left panning', 0, 1, True, True, False, True, False, False, 1, Panning1, nil);
  CreatePortParameter('Right delay', 0, 1, True, True, False, True, False, False, 1, Delay2, nil);
  CreatePortParameter('Right feedback', 0, 1, True, True, False, True, False, False, 1, Feedback2, nil);
  CreatePortParameter('Right panning', 0, 1, True, True, True, True, False, False, 1, Panning2, nil);

  inherited;
end;

procedure TPluginDelay.UpdateParameters;
begin
  Delay1 := InputControls[0].Value;
  Feedback1 := InputControls[1].Value;
  Panning1 := InputControls[2].Value;
  Delay2 := InputControls[3].Value;
  Feedback2 := InputControls[4].Value;
  Panning2 := Round(InputControls[5].Value);
end;

procedure TPluginDelay.SetPanning(APanValue: Single; var ALeftPan, ARightPan: single);
begin
  ALeftPan := (1 - APanValue) * (0.7 + 0.2 * APanValue);
  ARightPan := (1 + APanValue) * (0.7 - 0.2 * APanValue);
end;

end.

