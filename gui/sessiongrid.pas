unit sessiongrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Controls, globalconst, track, LCLType,
  BGRABitmap, BGRABitmapTypes, Graphics, ActnList, Menus, contnrs,
  Forms, pattern, ExtCtrls, dialcontrol;

const
  TRACK_WIDTH = 100;
  PATTERN_HEIGHT = 15;
  TRACK_CONTROL_HEIGHT = 150;

type
  TDroppedFileEvent = procedure(ADroppedFile: string) of object;

  TSessionGrid = class;

  {
    FlyWeight pattern based glyph class
  }

  { TBaseFlyWeight }

  TBaseFlyWeight = class
  private
    FHeight: Integer;
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
  public
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
  end;

  { TPatternDrawCursor }

  TPatternDrawCursor = class
    class procedure Render(X, Y: Integer; ACanvas: TCanvas; APattern: TPattern);
  end;

  {
    TPatternFlyWeight
  }
  TPatternFlyWeight = class(TBaseFlyWeight)
  public
    class procedure Render(X, Y: Integer; ABGRABitmap: TBGRABitmap; APattern: TPattern); virtual;
  end;

  {
    TMidiPatternFlyWeight

      Renders the state
  }

  TMidiPatternFlyWeight = class(TPatternFlyWeight)
  public
    class procedure Render(X, Y: Integer; ABGRABitmap: TBGRABitmap; APattern: TPattern); override;
  end;

  {
    TWavePatternFlyWeight

      Renders the state
  }

  TWavePatternFlyWeight = class(TPatternFlyWeight)
  public
    class procedure Render(X, Y: Integer; ABGRABitmap: TBGRABitmap; APattern: TPattern); override;
  end;

  {
    TNullPatternFlyWeight

      Renders the state
  }

  TNullPatternFlyWeight = class(TPatternFlyWeight)
  public
    class procedure Render(X, Y: Integer; ABGRABitmap: TBGRABitmap; APattern: TPattern); override;
  end;

  {
    TTrackView

      Gets synchronized by the model
  }

  TTrackView = class(THybridPersistentView)
  private
    FTrackControls: TPanel;
    FVolumeFader: TVolumeControl;
    FActiveSwitch: TToggleControl;

    FTop: Integer;
    FHeight: Integer;
    FLeft: Integer;
    FWidth: Integer;

    FMidiPatternFlyWeight: TMidiPatternFlyWeight;
    FWavePatternFlyWeight: TWavePatternFlyWeight;
    FNullPatternFlyWeight: TNullPatternFlyWeight;

    FSessionGrid: TSessionGrid;
    procedure ActiveSwitchChange(Sender: TObject);
    procedure LevelChange(Sender: TObject);
    procedure LevelStartChange(Sender: TObject);
    procedure SetSessionGrid(AValue: TSessionGrid);
  protected
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  public
    constructor Create(AObjectOwner: string; TheOwner: TComponent);
    destructor Destroy; override;

    procedure Update(Subject: THybridPersistentModel); override;
    procedure Render(AX, AY: Integer; ABGRABitmap: TBGRABitmap);
    procedure RenderCursor(ACanvas: TCanvas);
    function PatternAtMouseXY(AX, AY: Integer): TPattern;

    property SessionGrid: TSessionGrid read FSessionGrid write SetSessionGrid;
    property TrackControls: TPanel read FTrackControls;
    property VolumeFader: TVolumeControl read FVolumeFader;

    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
  end;

  {
    TTrackViewList
  }

  TTrackViewList = class(TObjectList)
  private
    function GetTrackView(AIndex: Integer): TTrackView;
    procedure SetTrackView(AIndex: Integer; const Value: TTrackView);
  public
    property Items[AIndex: Integer] : TTrackView read GetTrackView write SetTrackView; default;
    function Add(ATrackView: TTrackView): integer;
  end;


  {
    Drag position (track index, position)
  }
  TDragPosition = record
    TrackID: string;
    Position: Integer;
    XOffset: Integer;
    YOffset: Integer;
  end;


  { TSessionGrid }

  TSessionGrid = class(TPersistentPanel{TPersistentCustomControl})
  private
    FBGRABitmap: TBGRABitmap;
    FDragStart: TDragPosition;
    FDragDrop: TDragPosition;
    FDraggedPattern: TPatternFlyWeight;
    FOnPatternRefreshGUI: TPatternRefreshGUIEvent;
    FSelectedPattern: TPattern;
    FScrollIndex: Integer;
    FTrackViewList: TTrackViewList;
    FVisiblePatternCount: Integer;
    FJustDrawCursors: Boolean;
    FDragging: Boolean;
    FMouseDownX: Integer;
    FMouseDownY: Integer;
    FLastMouseX: Integer;
    FLastMouseY: Integer;
    FPopupMenu: TPopupMenu;
    FMouseX: Integer;
    FMouseY: Integer;
    FMouseDownL: Boolean;
    FMouseDownR: Boolean;
    FMenuCreateMidiPattern: TMenuItem;
    FMenuDeletePattern: TMenuItem;
    FMenuCreateTrack: TMenuItem;
    FMenuDeleteTrack: TMenuItem;
    FActionList: TActionList;
    FActionCreateMidiPattern: TAction;
    FActionDeletePattern: TAction;
    FActionCreateTrack: TAction;
    FActionDeleteTrack: TAction;

    procedure DrawCursors(ACanvas: TCanvas);
    procedure DrawTrackList(ABGRABitmap: TBGRABitmap);
    procedure GetDragPosition(X, Y: Integer; var ADragPosition: TDragPosition);
    procedure DoCreateMidiPattern(Sender: TObject);
    procedure DoDeletePattern(Sender: TObject);
    procedure DoCreateTrack(Sender: TObject);
    procedure DoDeleteTrack(Sender: TObject);
    function GetTrack(X, Y: Integer): TTrack;
    function GetPatternState(ATrack: TTrack; Y: Integer): TPattern;
    procedure CreateTrackGUI(AObjectID: string);
    procedure DeleteTrackGUI(AObjectID: string);
    function GetTrackView(AX, AY: Integer): TTrackView;
    procedure SetScrollIndex(AValue: Integer);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure ReSize; override;
    // Scrolling vertically through the patterns
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update(Subject: THybridPersistentModel); reintroduce; override;
    procedure EraseBackground(DC: HDC); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function PatternAtMouseXY(X, Y: Integer): TPattern;

    property OnPatternRefreshGUI: TPatternRefreshGUIEvent read FOnPatternRefreshGUI write FOnPatternRefreshGUI;
    property VisiblePatternCount: Integer read FVisiblePatternCount write FVisiblePatternCount;
    property ScrollIndex: Integer read FScrollIndex write SetScrollIndex;
    property BGRABitmap: TBGRABitmap read FBGRABitmap write FBGRABitmap;
    property JustDrawCursors: Boolean read FJustDrawCursors write FJustDrawCursors;
    property TrackViewList: TTrackViewList read FTrackViewList;
  end;

implementation

uses
  ComCtrls, Utils, audiostructure, midi, wave, global_command,
  global, FileUtil;

{ TPatternDrawCursor }

class procedure TPatternDrawCursor.Render(X, Y: Integer; ACanvas: TCanvas;
  APattern: TPattern);
var
  lCursor: Integer;
begin
  lCursor := Round(APattern.PatternCursor * (TRACK_WIDTH / APattern.LoopEnd.Value));

  ACanvas.Pen.Color := clRed;
  ACanvas.Line(
    X + lCursor,
    Y,
    X + lCursor,
    Y + PATTERN_HEIGHT);
end;

{ TNullPatternFlyWeight }

class procedure TNullPatternFlyWeight.Render(X, Y: Integer;
  ABGRABitmap: TBGRABitmap; APattern: TPattern);
begin
  ABGRABitmap.Rectangle(
    X,
    Y,
    X + TRACK_WIDTH,
    Y + PATTERN_HEIGHT + 1,
    ColorToBGRA(clGray), ColorToBGRA(clLtGray), dmset);
end;

{ TPatternFlyWeight }

class procedure TPatternFlyWeight.Render(X, Y: Integer;
  ABGRABitmap: TBGRABitmap; APattern: TPattern);
var
  pts: array of TPointF;
  lTrimmedPatternName: string;
begin
  ABGRABitmap.Rectangle(
    X,
    Y,
    X + TRACK_WIDTH,
    Y + PATTERN_HEIGHT,
    ColorToBGRA(clBlue), ColorToBGRA(clYellow), dmset);

  ABGRABitmap.DrawVertLine(X + 15, Y, Y + PATTERN_HEIGHT, ColorToBGRA(clBlue));

  if Assigned(APattern) then
  begin
    if APattern.Playing then
    begin
      ABGRABitmap.FillRectAntialias(
        X + 4,
        Y + 4,
        X + 11,
        Y + 11, ColorToBGRA(clBlue));
    end
    else
    begin
      SetLength(pts,3);
      pts[0] := PointF(X + 12, Y + 7);
      pts[1] := PointF(X + 4, Y + 3);
      pts[2] := PointF(X + 4, Y + 11);
      ABGRABitmap.FillPolyAntialias(pts, ColorToBGRA(clBlue));
    end;

    lTrimmedPatternName := RightStr(APattern.PatternName, TRACK_WIDTH div 8);
    ABGRABitmap.FontHeight := 12;
    ABGRABitmap.TextOut(
      X + 24, Y + 1,
      Format('%s', [lTrimmedPatternName]),
      ColorToBGRA(clBtnText));
  end;
end;

{ TWavePatternFlyWeight }

class procedure TWavePatternFlyWeight.Render(X, Y: Integer;
  ABGRABitmap: TBGRABitmap; APattern: TPattern);
var
  pts: array of TPointF;
  lTrimmedPatternName: string;
begin
  ABGRABitmap.Rectangle(
    X,
    Y,
    X + TRACK_WIDTH,
    Y + PATTERN_HEIGHT,
    ColorToBGRA(clBlue), ColorToBGRA(clYellow), dmset);

  ABGRABitmap.DrawVertLine(X + 15, Y, Y + PATTERN_HEIGHT, ColorToBGRA(clBlue));

  if Assigned(APattern) then
  begin
    if APattern.Playing then
    begin
      ABGRABitmap.FillRectAntialias(
        X + 4,
        Y + 4,
        X + 11,
        Y + 11, ColorToBGRA(clBlue));
    end
    else
    begin
      SetLength(pts,3);
      pts[0] := PointF(X + 12, Y + 7);
      pts[1] := PointF(X + 4, Y + 3);
      pts[2] := PointF(X + 4, Y + 11);
      ABGRABitmap.FillPolyAntialias(pts, ColorToBGRA(clBlue));
    end;

    lTrimmedPatternName := RightStr(APattern.PatternName, TRACK_WIDTH div 8);
    ABGRABitmap.FontHeight := 12;
    ABGRABitmap.TextOut(
      X + 24, Y + 1,
      Format('%s', [lTrimmedPatternName]),
      ColorToBGRA(clBtnText));
  end;
end;

{ TMidiPatternFlyWeight }

class procedure TMidiPatternFlyWeight.Render(X, Y: Integer;
  ABGRABitmap: TBGRABitmap; APattern: TPattern);
var
  pts: array of TPointF;
  lTrimmedPatternName: string;
  lColor: TColor;
begin
  // Give selected pattern a white color
  if Assigned(APattern) and (APattern = GSettings.SelectedPattern) then
  begin
    lColor := clWhite;
  end
  else
  begin
    lColor := clYellow;
  end;
  ABGRABitmap.Rectangle(
    X,
    Y,
    X + TRACK_WIDTH,
    Y + PATTERN_HEIGHT,
    ColorToBGRA(clBlue), ColorToBGRA(lColor), dmset);

  ABGRABitmap.DrawVertLine(X + 15, Y, Y + PATTERN_HEIGHT, ColorToBGRA(clBlue));

  if APattern.Playing then
  begin
    ABGRABitmap.FillRectAntialias(
      X + 4,
      Y + 4,
      X + 11,
      Y + 11, ColorToBGRA(clBlue));
  end
  else
  begin
    SetLength(pts,3);
    pts[0] := PointF(X + 12, Y + 7);
    pts[1] := PointF(X + 4, Y + 3);
    pts[2] := PointF(X + 4, Y + 11);
    ABGRABitmap.FillPolyAntialias(pts, ColorToBGRA(clBlue));
  end;

  lTrimmedPatternName := RightStr(APattern.PatternName, TRACK_WIDTH div 8);
  ABGRABitmap.FontHeight := 12;
  ABGRABitmap.TextOut(
    X + 24, Y + 1,
    Format('%s', [lTrimmedPatternName]),
    ColorToBGRA(clBtnText));
end;

function TTrackViewList.Add(ATrackView: TTrackView): integer;
begin
  Result := inherited Add(ATrackView);
end;

function TTrackViewList.GetTrackView(AIndex: integer): TTrackView;
begin
  result := inherited Items[AIndex] as TTrackView;
end;

procedure TTrackViewList.SetTrackView(AIndex: integer; const Value: TTrackView);
begin
  inherited Items[AIndex] := Value;
end;

{ TTrackView }

procedure TTrackView.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TTrackView.Update');

  // SetLevel
  FVolumeFader.Position := TTrack(Subject).Volume;

  // SetActive
  FActiveSwitch.SwitchedOn := TTrack(Subject).Active;

  // SetPan

  // SetInput

  // SetOutput

  FSessionGrid.Invalidate;

  DBLog('end TTrackView.Update');
end;

procedure TTrackView.Render(AX, AY: Integer; ABGRABitmap: TBGRABitmap);
var
  lIndex: Integer;
  lListIndex: Integer;
  lPattern: TPattern;
  lFound: Boolean;
begin
  ABGRABitmap.FillRect(Left, Top, Left + TRACK_WIDTH, ABGRABitmap.Height, ColorToBGRA(clLtGray), dmSet);
  ABGRABitmap.Rectangle(Left, Top, Left + TRACK_WIDTH, ABGRABitmap.Height, ColorToBGRA(clGray), dmSet);

  { todo sort patternlist on position, better performance when
    able to break out of the loop}
  for lIndex := 0 to Pred(FSessionGrid.VisiblePatternCount) do
  begin
    lFound := False;

    for lListIndex := 0 to Pred(TTrack(Model).PatternList.Count) do
    begin
      lPattern := TPattern(TTrack(Model).PatternList[lListIndex]);

      if lPattern.Position = lIndex + FSessionGrid.ScrollIndex then
      begin
        lFound := True;
        break;
      end;
    end;

    if lFound then
    begin
      FMidiPatternFlyWeight.Render(Left, lIndex * PATTERN_HEIGHT, ABGRABitmap, lPattern);
    end
    else
    begin
      FNullPatternFlyWeight.Render(Left, lIndex * PATTERN_HEIGHT, ABGRABitmap, nil);
    end;
  end;
end;

procedure TTrackView.RenderCursor(ACanvas: TCanvas);
var
  lIndex: Integer;
  lListIndex: Integer;
  lPattern: TPattern;
  lFound: Boolean;
begin
  for lIndex := 0 to Pred(FSessionGrid.VisiblePatternCount) do
  begin
    lFound := False;

    for lListIndex := 0 to Pred(TTrack(Model).PatternList.Count) do
    begin
      lPattern := TPattern(TTrack(Model).PatternList[lListIndex]);

      if (lPattern.Position = lIndex + FSessionGrid.ScrollIndex) and
        lPattern.Playing then
      begin
        lFound := True;
        TPatternDrawCursor.Render(Left, lIndex * PATTERN_HEIGHT, ACanvas, lPattern);
        break;
      end;
    end;

    if lFound then
    begin
      break;
    end;
  end;
end;

function TTrackView.PatternAtMouseXY(AX, AY: Integer): TPattern;
var
  lPosition: Integer;
  lTrack: TTrack;
  lIndex: Integer;
begin
  Result := nil;

  lPosition := AY div PATTERN_HEIGHT;
  if lPosition < FSessionGrid.VisiblePatternCount then
  begin
    lTrack := TTrack(Model);
    for lIndex := 0 to Pred(lTrack.PatternList.Count) do
    begin
      if TPattern(lTrack.PatternList[lIndex]).Position = lPosition then
      begin
        Result := TPattern(lTrack.PatternList[lIndex]);
        break;
      end;
    end;
  end;
end;

procedure TTrackView.SetLeft(AValue: Integer);
begin
  if FLeft = AValue then Exit;
  FLeft := AValue;

  FTrackControls.Left := FLeft;
end;

procedure TTrackView.SetHeight(AValue: Integer);
begin
  if FHeight = AValue then Exit;
  FHeight := AValue;

  FTrackControls.Top := FTop;
end;

procedure TTrackView.SetTop(AValue: Integer);
begin
  if FTop = AValue then Exit;
  FTop := AValue;

  FTrackControls.Top := FSessionGrid.Height - FTrackControls.Height;
end;

procedure TTrackView.SetWidth(AValue: Integer);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;

  FTrackControls.Width := FWidth;
end;

procedure TTrackView.SetSessionGrid(AValue: TSessionGrid);
begin
  if FSessionGrid = AValue then Exit;
  FSessionGrid := AValue;

  FTrackControls.Parent := FSessionGrid;
end;

constructor TTrackView.Create(AObjectOwner: string; TheOwner: TComponent);
begin
  inherited Create(AObjectOwner);

  FSessionGrid := TSessionGrid(TheOwner);

  FTrackControls := TPanel.Create(FSessionGrid);
  FTrackControls.Width := TRACK_WIDTH;
  FTrackControls.Height := 150;
  FTrackControls.Left := 0;
  FTrackControls.Top := FSessionGrid.Height - FTrackControls.Height;
  FTrackControls.BevelOuter := bvNone;
  FTrackControls.Parent := FSessionGrid;

  FVolumeFader := TVolumeControl.Create(FTrackControls);
  FVolumeFader.Height := 150;
  FVolumeFader.Width := 20;
  FVolumeFader.Left := 5;
  FVolumeFader.Top := 0;
  FVolumeFader.OnChange := @LevelChange;
  FVolumeFader.OnStartChange := @LevelStartChange;
  FVolumeFader.Parent := FTrackControls;

  FActiveSwitch := TToggleControl.Create(FTrackControls);
  FActiveSwitch.Left := 30;
  FActiveSwitch.Top := 5;
  FActiveSwitch.CaptionOff := 'Off';
  FActiveSwitch.CaptionOn := 'On';
  FActiveSwitch.OnChange := @ActiveSwitchChange;
  FActiveSwitch.Parent := FTrackControls;

  FMidiPatternFlyWeight := TMidiPatternFlyWeight.Create;
  FWavePatternFlyWeight := TWavePatternFlyWeight.Create;
  FNullPatternFlyWeight := TNullPatternFlyWeight.Create;
end;

destructor TTrackView.Destroy;
begin
  FMidiPatternFlyWeight.Free;
  FWavePatternFlyWeight.Free;
  FNullPatternFlyWeight.Free;

  FTrackControls.Free;

  inherited Destroy;
end;

procedure TTrackView.LevelChange(Sender: TObject);
var
  lTrackLevelCommand: TTrackLevelCommand;
begin
  lTrackLevelCommand := TTrackLevelCommand.Create(ObjectID);
  try
    lTrackLevelCommand.Persist := False;
    lTrackLevelCommand.TrackLevel := FVolumeFader.Position;

    GCommandQueue.PushCommand(lTrackLevelCommand);
  except
    lTrackLevelCommand.Free;
  end;
end;

procedure TTrackView.LevelStartChange(Sender: TObject);
var
  lTrackLevelCommand: TTrackLevelCommand;
begin
  lTrackLevelCommand := TTrackLevelCommand.Create(ObjectID);
  try
    lTrackLevelCommand.Persist := True;
    lTrackLevelCommand.TrackLevel := FVolumeFader.Position;

    GCommandQueue.PushCommand(lTrackLevelCommand);
  except
    lTrackLevelCommand.Free;
  end;
end;

procedure TTrackView.ActiveSwitchChange(Sender: TObject);
var
  lActivateTrack: TActivateTrackCommand;
begin
  // Send Channel On/Off command
  lActivateTrack := TActivateTrackCommand.Create(ObjectID);
  try
    lActivateTrack.ActiveState := FActiveSwitch.SwitchedOn;

    GCommandQueue.PushCommand(lActivateTrack);
  except
    lActivateTrack.Free;
  end;
end;

{ TSessionGrid }

constructor TSessionGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csAcceptsControls];

  FBGRABitmap := TBGRABitmap.Create(Width, Height);

  ReSize;

  FDraggedPattern := TPatternFlyWeight.Create;
  FSelectedPattern := nil;

  FTrackViewList := TTrackViewList.Create(True);

  FVisiblePatternCount := 10;

  FActionList := TActionList.Create(nil);

  FActionCreateMidiPattern := TAction.Create(nil);
  FActionCreateMidiPattern.Enabled := True;
  FActionCreateMidiPattern.Caption := 'Create MIDI Pattern';
  FActionCreateMidiPattern.ActionList := FActionList;
  FActionCreateMidiPattern.OnExecute := @DoCreateMidiPattern;

  FActionDeletePattern := TAction.Create(nil);
  FActionDeletePattern.Enabled := True;
  FActionDeletePattern.Caption := 'Delete Pattern';
  FActionDeletePattern.ActionList := FActionList;
  FActionDeletePattern.OnExecute := @DoDeletePattern;

  FActionCreateTrack := TAction.Create(nil);
  FActionCreateTrack.Enabled := True;
  FActionCreateTrack.Caption := 'Create track';
  FActionCreateTrack.ActionList := FActionList;
  FActionCreateTrack.OnExecute := @DoCreateTrack;

  FActionDeleteTrack := TAction.Create(nil);
  FActionDeleteTrack.Enabled := True;
  FActionDeleteTrack.Caption := 'Delete track';
  FActionDeleteTrack.ActionList := FActionList;
  FActionDeleteTrack.OnExecute := @DoDeleteTrack;

  FPopupMenu := TPopupMenu.Create(nil);

  FMenuCreateMidiPattern := TMenuItem.Create(nil);
  FMenuCreateTrack := TMenuItem.Create(nil);
  FMenuDeletePattern := TMenuItem.Create(nil);
  FMenuDeleteTrack := TMenuItem.Create(nil);

  FPopupMenu.Items.Add(FMenuCreateMidiPattern);
  FMenuCreateMidiPattern.Action := FActionCreateMidiPattern;

  FPopupMenu.Items.Add(FMenuDeletePattern);
  FMenuDeletePattern.Action := FActionDeletePattern;

  FPopupMenu.Items.Add(FMenuCreateTrack);
  FMenuCreateTrack.Action := FActionCreateTrack;

  FPopupMenu.Items.Add(FMenuDeleteTrack);
  FMenuDeleteTrack.Action := FActionDeleteTrack;
end;

destructor TSessionGrid.Destroy;
begin
  FMenuCreateMidiPattern.Free;
  FPopupMenu.Free;

  FActionCreateMidiPattern.Free;
  FActionList.Free;

  FTrackViewList.Free;

  FDraggedPattern.Free;

  FBGRABitmap.Free;

  inherited Destroy;
end;

{
  This update method has the function of analysing what actually changed based on the
  subject classtype and subsequently only updates what's necessary.
}
procedure TSessionGrid.Update(Subject: THybridPersistentModel);
begin
  if Subject is TAudioStructure then
  begin
    // Add/Remove tracks
    DiffLists(
      TAudioStructure(Subject).Tracks,
      FTrackViewList,
      @CreateTrackGUI,
      @DeleteTrackGUI);
  end
  else if Subject is TTrack then
  begin
    // Add/Remove patterns
  end
  else if Subject is TPattern then
  begin
    if Subject is TMidiPattern then
    begin
      // Changed MIDI notes
    end
    else if Subject is TWavePattern then
    begin
      // Changed waveforms
    end;
  end;

  Invalidate;
end;

procedure TSessionGrid.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

procedure TSessionGrid.Paint;
begin
  if FJustDrawCursors then
  begin
    FJustDrawCursors := False;

    FBGRABitmap.Draw(Canvas, 0, 0, False);

    DrawCursors(Canvas);
  end
  else
  begin
    FBGRABitmap.FillRect(0, 0, Width, Height, ColorToBGRA(clLtGray), dmSet);

    // Draw the tracks
    DrawTrackList(FBGRABitmap);

    // Draw dragging
    if FDragging then
    begin
      FDraggedPattern.Render(
        FMouseX - FDragStart.XOffset,
        FMouseY - FDragStart.YOffset ,
        FBGRABitmap, FSelectedPattern);
    end;

    FBGRABitmap.Draw(Canvas, 0, 0, False);

    DrawCursors(Canvas);
  end;
end;

procedure TSessionGrid.DrawCursors(ACanvas: TCanvas);
var
  lIndex: Integer;
begin
  if not Assigned(Model) then exit;

  for lIndex := 0 to Pred(FTrackViewList.Count) do
  begin
    FTrackViewList[lIndex].RenderCursor(ACanvas);
  end;
end;

procedure TSessionGrid.DrawTrackList(ABGRABitmap: TBGRABitmap);
var
  lIndex: Integer;
begin
  if not Assigned(Model) then exit;

  for lIndex := 0 to Pred(FTrackViewList.Count) do
  begin
    FTrackViewList[lIndex].Render(0, 0, ABGRABitmap);
  end;
end;

procedure TSessionGrid.GetDragPosition(X, Y: Integer; var ADragPosition: TDragPosition);
var
  lIndex: Integer;
  lPosition: Integer;
begin
  ADragPosition.TrackID := '';
  ADragPosition.Position := -1;

  for lIndex := 0 to Pred(FTrackViewList.Count) do
  begin
    if (X >= FTrackViewList[lIndex].Left) and
      (X < FTrackViewList[lIndex].Left + FTrackViewList[lIndex].Width) then
    begin
      lPosition := Y div PATTERN_HEIGHT;

      if lPosition < FVisiblePatternCount then
      begin
        ADragPosition.TrackID := FTrackViewList[lIndex].ObjectID;
        ADragPosition.Position := lPosition;
        ADragPosition.XOffset := X - FTrackViewList[lIndex].Left;
        ADragPosition.YOffset := Y - lPosition * PATTERN_HEIGHT;
      end;

      break;
    end;
  end;
end;

procedure TSessionGrid.DoCreateMidiPattern(Sender: TObject);
var
  lCreateMidiPattern: TCreatePatternCommand;
  lTrack: TTrack;
begin
  lTrack := GetTrack(FMouseX, FMouseY);

  if Assigned(lTrack) then
  begin
    lCreateMidiPattern := TCreatePatternCommand.Create(lTrack.ObjectID);
    try
      lCreateMidiPattern.PatternName := '- midi -';
      lCreateMidiPattern.Position := (FMouseY div PATTERN_HEIGHT) + ScrollIndex;
      lCreateMidiPattern.SourceType := fsMidi;

      GCommandQueue.PushCommand(lCreateMidiPattern);
    except
      lCreateMidiPattern.Free;
    end;

    Invalidate;
  end;
end;

procedure TSessionGrid.DoDeletePattern(Sender: TObject);
var
  lDeletePatternCommand: TDeletePatternCommand;
  lTrack: TTrack;
begin
  lTrack := GetTrack(FMouseX, FMouseY);
  if Assigned(lTrack) then
  begin
    if Assigned(FSelectedPattern) then
    begin
      lDeletePatternCommand := TDeletePatternCommand.Create(lTrack.ObjectID);
      try
        lDeletePatternCommand.ObjectID := FSelectedPattern.ObjectID;

        GCommandQueue.PushCommand(lDeletePatternCommand);
      except
        lDeletePatternCommand.Free;
      end;

      Invalidate;
    end;
  end;
end;

procedure TSessionGrid.DoCreateTrack(Sender: TObject);
var
  lCreateTrack: TCreateTrackCommand;
begin
  lCreateTrack := TCreateTrackCommand.Create(GAudioStruct.ObjectID);
  try
    lCreateTrack.SourceType := fsEmpty;

    GCommandQueue.PushCommand(lCreateTrack);
  except
    on e: Exception do
    begin
      DBLog('HybridError: ' + e.Message);
      lCreateTrack.Free;
    end;
  end;

  Invalidate;
end;

procedure TSessionGrid.DoDeleteTrack(Sender: TObject);
var
  lTrack: TTrack;
  lDeleteTrackCommand: TDeleteTrackCommand;
begin
  lTrack := GetTrack(FMouseX, FMouseY);

  if Assigned(lTrack) then
  begin
    lDeleteTrackCommand := TDeleteTrackCommand.Create(GAudioStruct.ObjectID);
    try
      lDeleteTrackCommand.ObjectIdList.Add(lTrack.ObjectID);

      GCommandQueue.PushCommand(lDeleteTrackCommand);
    except
      GCommandQueue.Free;
    end;
  end;
end;

{
  Get track under mouse if any
}
function TSessionGrid.GetTrack(X, Y: Integer): TTrack;
var
  lTrackView: TTrackView;
begin
  Result := nil;

  if not Assigned(Model) then exit;

  lTrackView := GetTrackView(X, Y);
  if Assigned(lTrackView) then
  begin
    Result := TTrack(lTrackView.Model);
  end;
end;

{
  Get trackview under mouse if any
}
function TSessionGrid.GetTrackView(AX, AY: Integer): TTrackView;
var
  lIndex: Integer;
begin
  Result := nil;

  if not Assigned(Model) then exit;

  for lIndex := 0 to Pred(FTrackViewList.Count) do
  begin
    if (AX >= FTrackViewList[lIndex].Left) and
      (AX < (FTrackViewList[lIndex].Left + FTrackViewList[lIndex].Width)) then
    begin
      Result := FTrackViewList[lIndex];

      break;
    end;
  end;
end;

{
  Get pattern under mouse cursor if any
}
function TSessionGrid.GetPatternState(ATrack: TTrack; Y: Integer): TPattern;
var
  lIndex: Integer;
  lPattern: TPattern;
begin
  Result := nil;

  if not Assigned(Model) then exit;

  for lIndex := 0 to Pred(ATrack.PatternList.Count) do
  begin
    lPattern := TPattern(ATrack.PatternList[lIndex]);

    if Trunc(Y / PATTERN_HEIGHT) = lPattern.Position then
    begin
      Result := TPattern(ATrack.PatternList[lIndex]);

      break;
    end;
  end;
end;

procedure TSessionGrid.CreateTrackGUI(AObjectID: string);
var
  lTrack: TTrack;
  lTrackState: TTrackView;
begin
  lTrack := TTrack(GObjectMapper.GetModelObject(AObjectID));

  if Assigned(lTrack) then
  begin
    lTrackState := TTrackView.Create(AObjectID, Self);
    lTrackState.Left := FTrackViewList.Count * TRACK_WIDTH;
    lTrackState.Width := TRACK_WIDTH;

    FTrackViewList.Add(lTrackState);

    lTrack.Attach(lTrackState);
  end;
end;

procedure TSessionGrid.DeleteTrackGUI(AObjectID: string);
var
  lIndex: Integer;
begin
  for lIndex := Pred(FTrackViewList.Count) downto 0 do
  begin
    if FTrackViewList[lIndex].ObjectID = AObjectID then
    begin
      FTrackViewList.Remove(FTrackViewList[lIndex]);
    end;
  end;
end;

procedure TSessionGrid.SetScrollIndex(AValue: Integer);
begin
  if FScrollIndex = AValue then Exit;

  FScrollIndex := AValue;

  Invalidate;
end;

{
  Detects if a file is dropped from the treeview and gives back
  the filename to the parent control through the DroppedFileEvent
}
procedure TSessionGrid.DragDrop(Source: TObject; X, Y: Integer);
var
  lTreeView: TTreeView;
  lTrack: TTrack;
  lCreatePattern: TCreatePatternCommand;
begin
  if Source is TTreeView then
  begin
    lTreeView := TTreeView(Source);

    // Create new pattern when there's is not pattern under mousecursor else nothing
    lTrack := GetTrack(X, Y);

    if Assigned(lTrack) then
    begin
      lCreatePattern := TCreatePatternCommand.Create(lTrack.ObjectID);
      try
        lCreatePattern.SourceLocation := TTreeFolderData(lTreeView.Selected.Data).Path;
        lCreatePattern.PatternName := ExtractFileNameWithoutExt(TTreeFolderData(lTreeView.Selected.Data).Path);
        lCreatePattern.Position := (Y div PATTERN_HEIGHT) + ScrollIndex;

        GCommandQueue.PushCommand(lCreatePattern);
      except
        lCreatePattern.Free;
      end;
    end;
  end;

  inherited DragDrop(Source, X, Y);
end;

function TSessionGrid.PatternAtMouseXY(X, Y: Integer): TPattern;
var
  lIndex: Integer;
begin
  Result := nil;
  for lIndex := 0 to Pred(FTrackViewList.Count) do
  begin
    if (X >= FTrackViewList[lIndex].Left) and
      (X < FTrackViewList[lIndex].Left + FTrackViewList[lIndex].Width) then
    begin
      Result := FTrackViewList[lIndex].PatternAtMouseXY(X, Y);
      if Assigned(Result) then
      begin
        break;
      end;
    end;
  end;
end;

procedure TSessionGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FMouseDownL := (Button = mbLeft);
  FMouseDownR := (Button = mbRight);

  FMouseX := X;
  FMouseY := Y;

  // Set dragdrop init state here and detect dragdrop in mousemove
  FMouseDownX := X;
  FMouseDownY := Y;

  GetDragPosition(X, Y, FDragStart);

  FSelectedPattern := PatternAtMouseXY(X, Y);

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TSessionGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  lTrack: TTrack;
  lPattern: TPattern;
  lMovePatternToTrackCommand: TMovePatternToTrackCommand;
  lRepositonPatternCommand: TRepositonPatternCommand;
  lSchedulePattern: TSchedulePatternCommand;
begin
  FMouseDownL := False;
  FMouseDownR := False;

  FMouseX := X;
  FMouseY := Y;

  GetDragPosition(X, Y, FDragDrop);

  {
    From a sessiongrid's view you only click on tracks or empty space.
    When a track is clicked control is handed over to the track's mouse handler
  }
  lTrack := GetTrack(FMouseX, FMouseY);

  if Button = mbRight then
  begin
    // Activate popup menu
    if Assigned(lTrack) then
    begin
      // Track found so hide pattern specific actions
      FActionCreateTrack.Visible := False;
      FActionCreateMidiPattern.Visible := True;
      FActionDeletePattern.Visible := True;
      FActionDeleteTrack.Visible := True;
    end
    else
    begin
      // No track found so show track specific actions
      FActionCreateTrack.Visible := True;
      FActionCreateMidiPattern.Visible := False;
      FActionDeletePattern.Visible := False;
      FActionDeleteTrack.Visible := False;
    end;

    FPopupMenu.PopUp;
  end
  else if Button = mbLeft then
  begin
    if FDragging then
    begin
      if FDragStart.TrackID <> FDragDrop.TrackID then
      begin
        if (FDragStart.TrackID <> '') and (FDragDrop.TrackID <> '') then
        begin
          // Send pattern move command
          lMovePatternToTrackCommand := TMovePatternToTrackCommand.Create(Self.ObjectID);
          try
            lMovePatternToTrackCommand.SourceTrackID := FDragStart.TrackID;
            lMovePatternToTrackCommand.TargetTrackID := FDragDrop.TrackID;
            lMovePatternToTrackCommand.PatternID := FSelectedPattern.ObjectID;
            lMovePatternToTrackCommand.Position := FDragDrop.Position;

            GCommandQueue.PushCommand(lMovePatternToTrackCommand);
          except
            lMovePatternToTrackCommand.Free;
          end;
        end;
      end
      else
      begin
        if FDragDrop.Position <> FDragStart.Position then
        begin
          lRepositonPatternCommand := TRepositonPatternCommand.Create(Self.ObjectID);
          try
            lRepositonPatternCommand.ObjectID := FSelectedPattern.ObjectID;
            lRepositonPatternCommand.Position := FDragDrop.Position;

            GCommandQueue.PushCommand(lRepositonPatternCommand);
          except
            lRepositonPatternCommand.Free;
          end;
        end;
      end;
    end
    else
    begin
      // Set focus and let child controls handle it
      if Assigned(lTrack) then
      begin
        lPattern := GetPatternState(lTrack, FMouseY);
        if Assigned(lPattern) then
        begin
          // This means that the leftmost 15 pixels has been clicked
          // ie. The play/stop button
          if FDragStart.XOffset < 15 then
          begin
            if lPattern.Playing then
            begin
              lSchedulePattern := TSchedulePatternCommand.Create(ObjectID);
              try
                lSchedulePattern.ObjectIdList.Add(lPattern.ObjectID);
                lSchedulePattern.TrackID := lTrack.ObjectID;
                lSchedulePattern.ScheduledTo := stStop;
                lSchedulePattern.Persist := False;
                GCommandQueue.PushCommand(lSchedulePattern);
              except
                lSchedulePattern.Free;
              end;
            end
            else
            begin
              lSchedulePattern := TSchedulePatternCommand.Create(ObjectID);
              try
                lSchedulePattern.ObjectIdList.Add(lPattern.ObjectID);
                lSchedulePattern.TrackID := lTrack.ObjectID;
                lSchedulePattern.ScheduledTo := stStart;
                lSchedulePattern.Persist := False;
                GCommandQueue.PushCommand(lSchedulePattern);
              except
                lSchedulePattern.Free;
              end;
            end;
          end
          else
          begin
            if Assigned(FSelectedPattern) then
            begin
              GSettings.SelectedPattern := lPattern;
              if Assigned(FOnPatternRefreshGUI) then
              begin
                FOnPatternRefreshGUI(GSettings.SelectedPattern);
              end;
            end;
          end;
        end
        else
        begin
          // Empty slot clicked so hide pattern view
          if Assigned(FOnPatternRefreshGUI) then
          begin
            FOnPatternRefreshGUI(nil);
          end;
        end;
      end
      else
      begin

      end;
    end;
  end;

  FDragging := False;
  FSelectedPattern := nil;

  Invalidate;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TSessionGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  FMouseX := X;
  FMouseY := Y;

  // Detect start of dragging
  if FMouseDownL and (not FDragging) and Assigned(FSelectedPattern) then
  begin
    FDragging :=
      (abs(FMouseDownX - X) > 5) or (abs(FMouseDownY - Y) > 5);
  end;

  Invalidate;

  FLastMouseX := X;
  FLastMouseY := Y;

  inherited MouseMove(Shift, X, Y);
end;

procedure TSessionGrid.DblClick;
var
  lTrack: TTrack;
  lCreateMidiPattern: TCreatePatternCommand;
  lCreateTrack: TCreateTrackCommand;
begin
  // Create new pattern when there's is not pattern under mousecursor else nothing
  lTrack := GetTrack(FMouseX, FMouseY);

  if Assigned(lTrack) then
  begin
    lCreateMidiPattern := TCreatePatternCommand.Create(lTrack.ObjectID);
    try
      lCreateMidiPattern.PatternName := '- midi -';
      lCreateMidiPattern.Position := (FMouseY div PATTERN_HEIGHT) + ScrollIndex;
      lCreateMidiPattern.SourceType := fsMidi;

      GCommandQueue.PushCommand(lCreateMidiPattern);
    except
      lCreateMidiPattern.Free;
    end;
  end
  else
  begin
    lCreateTrack := TCreateTrackCommand.Create(GAudioStruct.ObjectID);
    try
      lCreateTrack.SourceType := fsEmpty;

      GCommandQueue.PushCommand(lCreateTrack);
    except
      lCreateTrack.Free;
    end;
  end;

  Invalidate;

  inherited DblClick;
end;

procedure TSessionGrid.ReSize;
var
  lIndex: Integer;

begin
  inherited;

  if Assigned(FBGRABitmap) then
  begin
    FBGRABitmap.Free;
  end;
  FBGRABitmap := TBGRABitmap.Create(Width, Height);

  FVisiblePatternCount := (Height - TRACK_CONTROL_HEIGHT) div PATTERN_HEIGHT;

  if Assigned(FTrackViewList) then
  begin
    for lIndex := 0 to Pred(FTrackViewList.Count) do
    begin
      FTrackViewList[lIndex].TrackControls.Top :=
        Height - FTrackViewList[lIndex].TrackControls.Height;
    end;
  end;
end;

function TSessionGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  ScrollIndex := ScrollIndex + 1;

  Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

function TSessionGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  ScrollIndex := ScrollIndex - 1;

  Result := inherited DoMouseWheelUp(Shift, MousePos);
end;

procedure TSessionGrid.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);

  Accept := True;
end;

end.

