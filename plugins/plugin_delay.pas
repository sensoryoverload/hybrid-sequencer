unit plugin_delay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, plugin, global_command, global, globalconst, pluginhost, fx;

type
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
    FDryWet: single;
    FDry: single;
    FWet: single;

    FSampleL1: Single;
    FSampleR1: Single;
    FSampleL2: Single;
    FSampleR2: Single;

    FDelayBufferL1: TAudioRingBuffer;
    FDelayBufferR1: TAudioRingBuffer;
    FDelayBufferL2: TAudioRingBuffer;
    FDelayBufferR2: TAudioRingBuffer;

    procedure SetDelay1(AValue: single);
    procedure SetDelay2(AValue: single);
    procedure SetDryWet(AValue: single);
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
    property Delay1: single read FDelay1 write SetDelay1;
    property Feedback1: single read FFeedback1 write FFeedback1;
    property Panning1: single read FPanning1 write SetPanning1;
    property Delay2: single read FDelay2 write SetDelay2;
    property Feedback2: single read FFeedback2 write FFeedback2;
    property Panning2: single read FPanning2 write SetPanning2;
    property DryWet: single read FDryWet write SetDryWet;
  end;

implementation

{ TPluginDelay }

procedure TPluginDelay.SetPanning1(AValue: single);
begin
  SetPanning(FPanning1, FPanGainL1, FPanGainR1);
end;

procedure TPluginDelay.SetDelay1(AValue: single);
begin
  FDelayBufferL1.DelayMs := Round(AValue);
  FDelayBufferR1.DelayMs := Round(AValue);
end;

procedure TPluginDelay.SetDelay2(AValue: single);
begin
  FDelayBufferL2.DelayMs := Round(AValue);
  FDelayBufferR2.DelayMs := Round(AValue);
end;

procedure TPluginDelay.SetDryWet(AValue: single);
begin
  SetPanning(AValue, FDry, FWet);
end;

procedure TPluginDelay.SetPanning2(AValue: single);
begin
  SetPanning(FPanning2, FPanGainL2, FPanGainR2);
end;

constructor TPluginDelay.Create(AObjectOwnerID: string; AMapped: Boolean);
begin
  inherited;

  PluginName := 'Delay';

  FDelayBufferL1 := TAudioRingBuffer.Create(Round(GSettings.SampleRate));
  FDelayBufferL1.DelayMs := Round(FDelay1);
  FDelayBufferR1 := TAudioRingBuffer.Create(Round(GSettings.SampleRate));
  FDelayBufferR1.DelayMs := Round(FDelay1);
  Delay1 := 1000;
  Feedback1 := 0.1;
  Panning1 := 0;

  FDelayBufferL2 := TAudioRingBuffer.Create(Round(GSettings.SampleRate));
  FDelayBufferL2.DelayMs := Round(FDelay2);
  FDelayBufferR2 := TAudioRingBuffer.Create(Round(GSettings.SampleRate));
  FDelayBufferR2.DelayMs := Round(FDelay2);
  Delay2 := 1000;
  Feedback2 := 0.1;
  Panning2 := 0;

  DryWet := 0;

  FSampleL1 := 0;
  FSampleR1 := 0;
  FSampleL2 := 0;
  FSampleR2 := 0;
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
  lInputL: Single;
  lInputR: Single;
begin
  inherited;

  lOffsetL := 0;
  lOffsetR := 1;

  for lIndex := 0 to Pred(AFrames) do
  begin
    lInputL := AInputBuffer[lOffsetL];
    lInputR := AInputBuffer[lOffsetR];

    // Channel 1 - Left
    FSampleL1 :=
      FDelayBufferL1.Process(lInputL +   // Put input into delaybuffer
      FSampleL1 * FFeedback1);           // Feed output in input

    // Channel 1 - Right
    FSampleR1 :=
      FDelayBufferR1.Process(lInputR +   // Put input into delaybuffer
      FSampleR1 * FFeedback1);           // Feed output in input

    // Channel 2 - Left
    FSampleL2 :=
      FDelayBufferL2.Process(lInputL +   // Put input into delaybuffer
      FSampleL2 * FFeedback2);           // Feed output in input

    // Channel 2 - Right
    FSampleR2 :=
      FDelayBufferR2.Process(lInputR +   // Put input into delaybuffer
      FSampleR2 * FFeedback2);           // Feed output in input

    AOutputBuffer[lOffsetL] :=
      lInputL * FDry +
      (FSampleL1 * FPanGainL1 + FSampleL2 * FPanGainL2) * FWet;
    AOutputBuffer[lOffsetR] :=
      lInputR * FDry +
      (FSampleR1 * FPanGainR1 + FSampleR2 * FPanGainR2) * FWet;

    Inc(lOffsetL, STEREO);
    Inc(lOffsetR, STEREO);
  end;
end;

procedure TPluginDelay.Instantiate;
begin
  {
    ACaption: string;
    ALowerBound: Single;
    AUpperBound: Single;
    AIsBoundedAbove: Boolean;
    AIsBoundedBelow: Boolean;
    AIsInteger: Boolean;
    AIsLogarithmic: Boolean;
    AIsSampleRate: Boolean;
    AIsToggled: Boolean;
    ADefaultValue: Single;
    AValue: Single;
    ASetValue: TSingleParameter
  }
  CreatePortParameter('Delay 1', 0, 1000, True, True, True, False, False, False, 1000, Delay1, nil);
  CreatePortParameter('Feedback 1', 0, 1, True, True, False, False, False, False, 0, Feedback1, nil);
  CreatePortParameter('Panning 1', -1, 1, True, True, True, False, False, False, 0, Panning1, nil);
  CreatePortParameter('Delay 2', 0, 1000, True, True, True, False, False, False, 1000, Delay2, nil);
  CreatePortParameter('Feedback 2', 0, 1, True, True, False, False, False, False, 0, Feedback2, nil);
  CreatePortParameter('Panning 2', -1, 1, True, True, True, False, False, False, 0, Panning2, nil);
  CreatePortParameter('DryWet ratio', -1, 1, True, True, True, False, False, False, 0, DryWet, nil);
end;

procedure TPluginDelay.UpdateParameters;
begin
  Delay1 := InputControls[0].Value;
  Feedback1 := InputControls[1].Value;
  Panning1 := InputControls[2].Value;
  Delay2 := InputControls[3].Value;
  Feedback2 := InputControls[4].Value;
  Panning2 := Round(InputControls[5].Value);
  DryWet := InputControls[6].Value;
end;

procedure TPluginDelay.SetPanning(APanValue: Single; var ALeftPan, ARightPan: single);
begin
  ALeftPan := (1 - APanValue) * (0.7 + 0.2 * APanValue);
  ARightPan := (1 + APanValue) * (0.7 - 0.2 * APanValue);
end;

procedure Register;
begin

end;

end.

