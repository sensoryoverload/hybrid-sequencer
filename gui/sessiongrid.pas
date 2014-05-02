unit sessiongrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Controls, globalconst, track, LCLType,
  BGRABitmap, BGRABitmapTypes, Graphics, ActnList, Menus, contnrs,
  Forms, pattern, StdCtrls, ExtCtrls, dialcontrol;

const
  TRACK_WIDTH = 100;
  PATTERN_HEIGHT = 15;
  TRACK_CONTROL_HEIGHT = 150;

type
  TMode = (mSelect, mRename);

  TDroppedFilefEvent = procedure(ADroppedFile: string) of object;

  TSessionGrid = class;

  {
    FlyWeight pattern based glyph class
  }

  { TBaseFlyWeight }

  TBaseFlyWeight = class
  private
    FHeight: integer;
    FLeft: integer;
    FTop: integer;
    FWidth: integer;
  public
    property Left: integer read FLeft write FLeft;
    property Top: integer read FTop write FTop;
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
  end;

  { TPatternDrawCursor }

  TPatternDrawCursor = class
    class procedure Render(X, Y: integer; ACanvas: TCanvas; APattern: TPattern);
  end;

  {
    TPatternFlyWeight
  }
  TPatternFlyWeight = class(TBaseFlyWeight)
  public
    class procedure Render(X, Y: integer; ABGRABitmap: TBGRABitmap;
      APattern: TPattern); virtual;
  end;

  {
    TMidiPatternFlyWeight

      Renders the state
  }

  TMidiPatternFlyWeight = class(TPatternFlyWeight)
  public
    class procedure Render(X, Y: integer; ABGRABitmap: TBGRABitmap;
      APattern: TPattern); override;
  end;

  {
    TWavePatternFlyWeight

      Renders the state
  }

  TWavePatternFlyWeight = class(TPatternFlyWeight)
  public
    class procedure Render(X, Y: integer; ABGRABitmap: TBGRABitmap;
      APattern: TPattern); override;
  end;

  {
    TNullPatternFlyWeight

      Renders the state
  }

  TNullPatternFlyWeight = class(TPatternFlyWeight)
  public
    class procedure Render(X, Y: integer; ABGRABitmap: TBGRABitmap;
      APattern: TPattern); override;
  end;

  {
    TTrackView

      Gets synchronized by the model
  }

  TTrackView = class(THybridPersistentView)
  private
    FUpdateSubject: THybridPersistentModel;
    FIsDirty: boolean;
    FTrackType: TTrackType;

    FTrackControls: TPanel;
    FVolumeFader: TVolumeControl;
    FPanControl: TParameterControl;
    FTarget: TComboBox;
    FActiveSwitch: TToggleControl;

    FTop: integer;
    FHeight: integer;
    FLeft: integer;
    FWidth: integer;

    FMidiPatternFlyWeight: TMidiPatternFlyWeight;
    FWavePatternFlyWeight: TWavePatternFlyWeight;
    FNullPatternFlyWeight: TNullPatternFlyWeight;

    FSessionGrid: TSessionGrid;
    procedure ActiveSwitchChange(Sender: TObject);
    procedure LevelChange(Sender: TObject);
    procedure LevelStartChange(Sender: TObject);
    procedure BalanceChange(Sender: TObject);
    procedure BalanceStartChange(Sender: TObject);
    procedure SetSessionGrid(AValue: TSessionGrid);
    procedure DoOnTrackClick(Sender: TObject);
    procedure DoOnChangeTarget(Sender: TObject);
  protected
    procedure SetHeight(AValue: integer);
    procedure SetLeft(AValue: integer);
    procedure SetTop(AValue: integer);
    procedure SetWidth(AValue: integer);
  public
    constructor Create(AObjectOwner: string; TheOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure Update(Subject: THybridPersistentModel); override;
    procedure UpdateView(AForceRedraw: boolean = False); override;
    procedure Render(AX, AY: integer; ABGRABitmap: TBGRABitmap);
    procedure RenderCursor(ACanvas: TCanvas);
    function PatternAtMouseXY(AX, AY: integer): TPattern;

    property SessionGrid: TSessionGrid read FSessionGrid write SetSessionGrid;
    property TrackControls: TPanel read FTrackControls;
    property VolumeFader: TVolumeControl read FVolumeFader;

    property Left: integer read FLeft write SetLeft;
    property Top: integer read FTop write SetTop;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;

    property TrackType: TTrackType read FTrackType write FTrackType;
  end;

  {
    TTrackViewList
  }

  TTrackViewList = class(TObjectList)
  private
    function GetTrackView(AIndex: integer): TTrackView;
    procedure SetTrackView(AIndex: integer; const Value: TTrackView);
  public
    property Items[AIndex: integer]: TTrackView read GetTrackView write SetTrackView;
      default;
    function Add(ATrackView: TTrackView): integer;
  end;


  {
    Drag position (track index, position)
  }
  TDragPosition = record
    TrackID: string;
    Position: integer;
    XOffset: integer;
    YOffset: integer;
  end;


  { TSessionGrid }

  TSessionGrid = class({TPersistentPanel}TPersistentCustomControl)
  private
    FUpdateSubject: THybridPersistentModel;
    FIsDirty: boolean;
    FBGRABitmap: TBGRABitmap;
    FDragStart: TDragPosition;
    FDragDrop: TDragPosition;
    FDraggedPattern: TPatternFlyWeight;
    FOnPatternRefreshGUI: TPatternRefreshGUIEvent;
    FSelectedPattern: TPattern;
    FScrollIndex: integer;
    FTrackViewList: TTrackViewList;
    FTrackMasterCount: integer;
    FTrackGroupCount: integer;
    FTrackNormalCount: integer;
    FTrackReturnCount: integer;
    FVisiblePatternCount: integer;
    FJustDrawCursors: boolean;
    FDragging: boolean;
    FMouseDownX: integer;
    FMouseDownY: integer;
    FLastMouseX: integer;
    FLastMouseY: integer;
    FPopupMenu: TPopupMenu;
    FMouseX: integer;
    FMouseY: integer;
    FMouseDownL: boolean;
    FMouseDownR: boolean;
    FMode: TMode;
    FTempPatternName: string;
    FRenameCursorPosition: integer;
    FMenuRenamePattern: TMenuItem;
    FMenuCreateMidiPattern: TMenuItem;
    FMenuDeletePattern: TMenuItem;
    FMenuCreateTrack: TMenuItem;
    FMenuCreateGroupTrack: TMenuItem;
    FMenuCreateReturnTrack: TMenuItem;
    FMenuCreateMasterTrack: TMenuItem;
    FMenuDeleteTrack: TMenuItem;
    FActionList: TActionList;
    FActionRenamePattern: TAction;
    FActionCreateMidiPattern: TAction;
    FActionDeletePattern: TAction;
    FActionCreateTrack: TAction;
    FActionCreateGroupTrack: TAction;
    FActionCreateReturnTrack: TAction;
    FActionCreateMasterTrack: TAction;
    FActionDeleteTrack: TAction;

    procedure CalculateTrackOffsets;
    procedure CreateTrackType(ATrackType: TTrackType);
    procedure DrawCursors(ACanvas: TCanvas);
    procedure DrawTrackList(ABGRABitmap: TBGRABitmap);
    procedure GetDragPosition(X, Y: integer; var ADragPosition: TDragPosition);
    procedure DoRenamePattern(Sender: TObject);
    procedure OnUpdateRenamePattern(Sender: TObject);
    procedure DoCreateMidiPattern(Sender: TObject);
    procedure DoDeletePattern(Sender: TObject);
    procedure DoCreateTrack(Sender: TObject);
    procedure DoCreateGroupTrack(Sender: TObject);
    procedure DoCreateReturnTrack(Sender: TObject);
    procedure DoCreateMasterTrack(Sender: TObject);
    procedure DoDeleteTrack(Sender: TObject);
    function GetTrack(X, Y: integer): TTrack;
    function GetPatternState(ATrack: TTrack; Y: integer): TPattern;
    procedure CreateTrackGUI(AObjectID: string);
    procedure DeleteTrackGUI(AObjectID: string);
    function GetTrackView(AX, AY: integer): TTrackView;
    procedure HandleKeyDownRenameMode(var Key: word; Shift: TShiftState);
    procedure HandleKeyDownSelectMode(var Key: word; Shift: TShiftState);
    procedure SetScrollIndex(AValue: integer);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure DblClick; override;
    procedure ReSize; override;
    // Scrolling vertically through the patterns
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean; override;
    procedure DragOver(Source: TObject; X, Y: integer; State: TDragState;
      var Accept: boolean); override;
    //    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    //    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update(Subject: THybridPersistentModel); reintroduce; override;
    procedure UpdateView(AForceRedraw: boolean = False); override;
    procedure EraseBackground(DC: HDC); override;
    procedure DragDrop(Source: TObject; X, Y: integer); override;
    function PatternAtMouseXY(X, Y: integer): TPattern;
    procedure SelectTrack(ASelected: TTrackView);

    property OnPatternRefreshGUI: TPatternRefreshGUIEvent
      read FOnPatternRefreshGUI write FOnPatternRefreshGUI;
    property VisiblePatternCount: integer read FVisiblePatternCount
      write FVisiblePatternCount;
    property ScrollIndex: integer read FScrollIndex write SetScrollIndex;
    property BGRABitmap: TBGRABitmap read FBGRABitmap write FBGRABitmap;
    property JustDrawCursors: boolean read FJustDrawCursors write FJustDrawCursors;
    property TrackViewList: TTrackViewList read FTrackViewList;
  end;

implementation

uses
  ComCtrls, Utils, audiostructure, midi, wave, global_command,
  global, FileUtil, renamepatterngui;

{ TPatternDrawCursor }

class procedure TPatternDrawCursor.Render(X, Y: integer; ACanvas: TCanvas;
  APattern: TPattern);
var
  lCursor: integer;
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

class procedure TNullPatternFlyWeight.Render(X, Y: integer;
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

class procedure TPatternFlyWeight.Render(X, Y: integer; ABGRABitmap: TBGRABitmap;
  APattern: TPattern);
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
      SetLength(pts, 3);
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

class procedure TWavePatternFlyWeight.Render(X, Y: integer;
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
      SetLength(pts, 3);
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

class procedure TMidiPatternFlyWeight.Render(X, Y: integer;
  ABGRABitmap: TBGRABitmap; APattern: TPattern);
var
  pts: array of TPointF;
  lTrimmedPatternName: string;
  lColor: TColor;
begin
  // Give selected pattern a white color
  if Assigned(APattern) and (APattern = GSettings.SelectedObject) then
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
    SetLength(pts, 3);
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
  Result := inherited Items[AIndex] as TTrackView;
end;

procedure TTrackViewList.SetTrackView(AIndex: integer; const Value: TTrackView);
begin
  inherited Items[AIndex] := Value;
end;

{ TTrackView }

procedure TTrackView.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TTrackView.Update');

  FUpdateSubject := Subject;
  FIsDirty := True;


  DBLog('end TTrackView.Update');
end;

procedure TTrackView.UpdateView(AForceRedraw: boolean = False);
var
  lIndex: Integer;
  lMasterIndex: Integer;
  lGroupIndex: Integer;
  lTrack: TTrack;
begin
  if (FIsDirty or AForceRedraw) and Assigned(FUpdateSubject) then
  begin
    FVolumeFader.LevelLeft := TTrack(FUpdateSubject).LeftLevel;
    FVolumeFader.LevelRight := TTrack(FUpdateSubject).RightLevel;

    // SetLevel
    FVolumeFader.Position := TTrack(FUpdateSubject).Volume;

    // SetActive
    FActiveSwitch.SwitchedOn := TTrack(FUpdateSubject).Active;

    // SetBalance
    FPanControl.Value := TTrack(FUpdateSubject).Pan;

    // SetInput

    // SetOutput
    if TrackType <> ttMaster then
    begin
      lMasterIndex := 0;
      lGroupIndex := 0;
      FTarget.Clear;
      FTarget.Items.AddObject('None', nil);

      for lIndex := 0 to Pred(FSessionGrid.TrackViewList.Count) do
      begin
        lTrack := TTrack(FSessionGrid.TrackViewList[lIndex].Model);

        case lTrack.TrackType of
          ttMaster:
          begin
            Inc(lMasterIndex);

            // Do not feedback on own track
            if Self <> FSessionGrid.TrackViewList[lIndex] then
            begin
              FTarget.Items.AddObject(Format('Master %d', [lMasterIndex]), lTrack);
            end;
          end;
          ttGroup:
          begin
            Inc(lGroupIndex);

            // Do not feedback on own track
            if Self <> FSessionGrid.TrackViewList[lIndex] then
            begin
              FTarget.Items.AddObject(Format('Group %d', [lGroupIndex]), lTrack);
            end;
          end;
        end;
      end;

      // If not assigned then look for item 'None'
      if TTrack(FUpdateSubject).TargetTrackId = '' then
      begin
        for lIndex := 0 to Pred(FTarget.Items.Count) do
        begin
          lTrack := TTrack(FTarget.Items.Objects[lIndex]);

          if not Assigned(lTrack) then
          begin
            FTarget.ItemIndex := lIndex;

            break;
          end;
        end;
      end
      else
      begin
        // Look for an item the targettrack points to
        for lIndex := 0 to Pred(FTarget.Items.Count) do
        begin
          lTrack := TTrack(FTarget.Items.Objects[lIndex]);

          if Assigned(lTrack) then
          begin
            if lTrack.TrackId = TTrack(FUpdateSubject).TargetTrackId then
            begin
              FTarget.ItemIndex := lIndex;

              break;
            end;
          end;
        end;
      end;
    end
    else
    begin
      FTarget.Visible := False;
    end;

    FSessionGrid.Invalidate;
  end;
end;

procedure TTrackView.Render(AX, AY: integer; ABGRABitmap: TBGRABitmap);
var
  lIndex: integer;
  lListIndex: integer;
  lPattern: TPattern;
  lFound: boolean;
begin
  ABGRABitmap.FillRect(Left, Top, Left + TRACK_WIDTH, ABGRABitmap.Height,
    ColorToBGRA(clLtGray), dmSet);
  ABGRABitmap.Rectangle(Left, Top, Left + TRACK_WIDTH, ABGRABitmap.Height,
    ColorToBGRA(clGray), dmSet);

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
  lIndex: integer;
  lListIndex: integer;
  lPattern: TPattern;
  lFound: boolean;
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

function TTrackView.PatternAtMouseXY(AX, AY: integer): TPattern;
var
  lPosition: integer;
  lTrack: TTrack;
  lIndex: integer;
begin
  Result := nil;

  lPosition := AY div PATTERN_HEIGHT + FSessionGrid.ScrollIndex;
  if AY div PATTERN_HEIGHT < FSessionGrid.VisiblePatternCount then
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

procedure TTrackView.SetLeft(AValue: integer);
begin
  if FLeft = AValue then
    Exit;
  FLeft := AValue;

  FTrackControls.Left := FLeft;
end;

procedure TTrackView.SetHeight(AValue: integer);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;

  FTrackControls.Top := FTop;
end;

procedure TTrackView.SetTop(AValue: integer);
begin
  if FTop = AValue then
    Exit;
  FTop := AValue;

  FTrackControls.Top := FSessionGrid.Height - FTrackControls.Height;
end;

procedure TTrackView.SetWidth(AValue: integer);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;

  FTrackControls.Width := FWidth;
end;

procedure TTrackView.SetSessionGrid(AValue: TSessionGrid);
begin
  if FSessionGrid = AValue then
    Exit;
  FSessionGrid := AValue;

  FTrackControls.Parent := FSessionGrid;
end;

procedure TTrackView.DoOnTrackClick(Sender: TObject);
begin
  if Assigned(SessionGrid.OnPatternRefreshGUI) then
  begin
    GSettings.SelectedObject := Self.Model;
    SessionGrid.SelectTrack(Self);
    SessionGrid.OnPatternRefreshGUI(GSettings.SelectedObject);
  end;
end;

procedure TTrackView.DoOnChangeTarget(Sender: TObject);
var
  lTrackChangeTargetCommand: TTrackChangeTargetCommand;
  lTrack: TTrack;
begin
  if FTarget.ItemIndex <> -1 then
  begin
    lTrackChangeTargetCommand := TTrackChangeTargetCommand.Create(ObjectID);
    try
      lTrackChangeTargetCommand.Persist := True;

      lTrack := TTrack(FTarget.Items.Objects[FTarget.ItemIndex]);
      if Assigned(lTrack) then
      begin
        lTrackChangeTargetCommand.TargetTrackId := lTrack.ObjectID;
      end
      else
      begin
        lTrackChangeTargetCommand.TargetTrackId := '';
      end;

      GCommandQueue.PushCommand(lTrackChangeTargetCommand);
    except
      lTrackChangeTargetCommand.Free;
    end;
  end;
end;

constructor TTrackView.Create(AObjectOwner: string; TheOwner: TComponent);
begin
  DBLog('start TTrackView.Create');

  inherited Create(AObjectOwner);

  FIsDirty := False;

  FSessionGrid := TSessionGrid(TheOwner);

  FTrackControls := TPanel.Create(FSessionGrid);
  FTrackControls.Width := TRACK_WIDTH;
  FTrackControls.Height := 200;
  FTrackControls.Left := 0;
  FTrackControls.Top := FSessionGrid.Height - FTrackControls.Height;
  FTrackControls.BevelOuter := bvRaised;
  FTrackControls.Parent := FSessionGrid;
  FTrackControls.OnClick := @DoOnTrackClick;

  FTarget := TComboBox.Create(FTrackControls);
  FTarget.Top := 3;
  FTarget.Left := 4;
  FTarget.Height := 12;
  FTarget.ItemHeight := 12;
  FTarget.Width := FTrackControls.Width - 9;
  FTarget.Parent := FTrackControls;
  FTarget.Style := csDropDownList;
  FTarget.Sorted := True;
  FTarget.ItemIndex := 0;
  FTarget.OnChange := @DoOnChangeTarget;

  FPanControl := TParameterControl.Create(FTrackControls);
  FPanControl.Orientation := oBalance;
  FPanControl.Height := 10;
  FPanControl.Width := 50;
  FPanControl.Top := 28;
  FPanControl.Left := 4;
  FPanControl.Caption := 'Balance';
  FPanControl.Min := -1;
  FPanControl.Max := 1;
  FPanControl.Value := 0;
  FPanControl.ShowValue := False;
  FPanControl.OnStartChange := @BalanceStartChange;
  FPanControl.OnChange := @BalanceChange;
  FPanControl.Parent := FTrackControls;

  FVolumeFader := TVolumeControl.Create(FTrackControls);
  FVolumeFader.Height := 140;
  FVolumeFader.Width := 20;
  FVolumeFader.Left := 4;
  FVolumeFader.Top := 50;
  FVolumeFader.OnChange := @LevelChange;
  FVolumeFader.OnStartChange := @LevelStartChange;
  FVolumeFader.Parent := FTrackControls;

  FActiveSwitch := TToggleControl.Create(FTrackControls);
  FActiveSwitch.Left := 30;
  FActiveSwitch.Top := 50;
  FActiveSwitch.Width := 30;
  FActiveSwitch.Height := 30;
  FActiveSwitch.CaptionOff := 'Off';
  FActiveSwitch.CaptionOn := 'On';
  FActiveSwitch.FontSize := 10;
  FActiveSwitch.FontStyle := [fsBold];
  FActiveSwitch.OnChange := @ActiveSwitchChange;
  FActiveSwitch.Parent := FTrackControls;

  FMidiPatternFlyWeight := TMidiPatternFlyWeight.Create;
  FWavePatternFlyWeight := TWavePatternFlyWeight.Create;
  FNullPatternFlyWeight := TNullPatternFlyWeight.Create;

  DBLog('end TTrackView.Create');
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

procedure TTrackView.BalanceChange(Sender: TObject);
var
  lTrackBalanceCommand: TTrackPanCommand;
begin
  lTrackBalanceCommand := TTrackPanCommand.Create(ObjectID);
  try
    lTrackBalanceCommand.Persist := False;
    lTrackBalanceCommand.Pan := FPanControl.Value;

    GCommandQueue.PushCommand(lTrackBalanceCommand);
  except
    lTrackBalanceCommand.Free;
  end;
end;

procedure TTrackView.BalanceStartChange(Sender: TObject);
var
  lTrackBalanceCommand: TTrackPanCommand;
begin
  lTrackBalanceCommand := TTrackPanCommand.Create(ObjectID);
  try
    lTrackBalanceCommand.Persist := True;
    lTrackBalanceCommand.Pan := FPanControl.Value;

    GCommandQueue.PushCommand(lTrackBalanceCommand);
  except
    lTrackBalanceCommand.Free;
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

  FIsDirty := False;

  FMode := mSelect;

  ControlStyle := ControlStyle + [csAcceptsControls];

  DoubleBuffered := True;

  FBGRABitmap := TBGRABitmap.Create(Width, Height);

  ReSize;

  FDraggedPattern := TPatternFlyWeight.Create;
  FSelectedPattern := nil;

  FTrackViewList := TTrackViewList.Create(True);
  FTrackNormalCount := 0;
  FTrackGroupCount := 0;
  FTrackReturnCount := 0;
  FTrackMasterCount := 0;

  FVisiblePatternCount := 10;

  FActionList := TActionList.Create(Self);

  FActionRenamePattern := TAction.Create(FActionList);
  FActionRenamePattern.Enabled := True;
  FActionRenamePattern.Caption := 'Rename Pattern';
  FActionRenamePattern.ActionList := FActionList;
  FActionRenamePattern.OnExecute := @DoRenamePattern;
  FActionRenamePattern.OnUpdate := @OnUpdateRenamePattern;
  FActionRenamePattern.ShortCut := ShortCut(VK_R, [ssCtrl]);

  FActionCreateMidiPattern := TAction.Create(FActionList);
  FActionCreateMidiPattern.Enabled := True;
  FActionCreateMidiPattern.Caption := 'Create MIDI Pattern';
  FActionCreateMidiPattern.ActionList := FActionList;
  FActionCreateMidiPattern.OnExecute := @DoCreateMidiPattern;

  FActionDeletePattern := TAction.Create(FActionList);
  FActionDeletePattern.Enabled := True;
  FActionDeletePattern.Caption := 'Delete Pattern';
  FActionDeletePattern.ActionList := FActionList;
  FActionDeletePattern.OnExecute := @DoDeletePattern;

  FActionCreateTrack := TAction.Create(FActionList);
  FActionCreateTrack.Enabled := True;
  FActionCreateTrack.Caption := 'Create track';
  FActionCreateTrack.ActionList := FActionList;
  FActionCreateTrack.OnExecute := @DoCreateTrack;

  FActionCreateGroupTrack := TAction.Create(FActionList);
  FActionCreateGroupTrack.Enabled := True;
  FActionCreateGroupTrack.Caption := 'Create grouptrack';
  FActionCreateGroupTrack.ActionList := FActionList;
  FActionCreateGroupTrack.OnExecute := @DoCreateGroupTrack;

  FActionCreateMasterTrack := TAction.Create(FActionList);
  FActionCreateMasterTrack.Enabled := True;
  FActionCreateMasterTrack.Caption := 'Create mastertrack';
  FActionCreateMasterTrack.ActionList := FActionList;
  FActionCreateMasterTrack.OnExecute := @DoCreateMasterTrack;

  FActionCreateReturnTrack := TAction.Create(FActionList);
  FActionCreateReturnTrack.Enabled := True;
  FActionCreateReturnTrack.Caption := 'Create returntrack';
  FActionCreateReturnTrack.ActionList := FActionList;
  FActionCreateReturnTrack.OnExecute := @DoCreateReturnTrack;

  FActionDeleteTrack := TAction.Create(FActionList);
  FActionDeleteTrack.Enabled := True;
  FActionDeleteTrack.Caption := 'Delete track';
  FActionDeleteTrack.ActionList := FActionList;
  FActionDeleteTrack.OnExecute := @DoDeleteTrack;

  FPopupMenu := TPopupMenu.Create(Self);

  FMenuRenamePattern := TMenuItem.Create(FPopupMenu);
  FMenuCreateMidiPattern := TMenuItem.Create(FPopupMenu);
  FMenuCreateTrack := TMenuItem.Create(FPopupMenu);
  FMenuCreateGroupTrack := TMenuItem.Create(FPopupMenu);
  FMenuCreateReturnTrack := TMenuItem.Create(FPopupMenu);
  FMenuCreateMasterTrack := TMenuItem.Create(FPopupMenu);
  FMenuDeletePattern := TMenuItem.Create(FPopupMenu);
  FMenuDeleteTrack := TMenuItem.Create(FPopupMenu);

  FPopupMenu.Items.Add(FMenuRenamePattern);
  FMenuRenamePattern.Action := FActionRenamePattern;

  FPopupMenu.Items.Add(FMenuCreateMidiPattern);
  FMenuCreateMidiPattern.Action := FActionCreateMidiPattern;

  FPopupMenu.Items.Add(FMenuDeletePattern);
  FMenuDeletePattern.Action := FActionDeletePattern;

  FPopupMenu.Items.Add(FMenuCreateTrack);
  FMenuCreateTrack.Action := FActionCreateTrack;

  FPopupMenu.Items.Add(FMenuCreateGroupTrack);
  FMenuCreateGroupTrack.Action := FActionCreateGroupTrack;

  FPopupMenu.Items.Add(FMenuCreateReturnTrack);
  FMenuCreateReturnTrack.Action := FActionCreateReturnTrack;

  FPopupMenu.Items.Add(FMenuCreateMasterTrack);
  FMenuCreateMasterTrack.Action := FActionCreateMasterTrack;

  FPopupMenu.Items.Add(FMenuDeleteTrack);
  FMenuDeleteTrack.Action := FActionDeleteTrack;
end;

destructor TSessionGrid.Destroy;
begin
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
  FUpdateSubject := Subject;

  FIsDirty := True;
end;

procedure TSessionGrid.UpdateView(AForceRedraw: boolean = False);
var
  lIndex: integer;
begin
  if FIsDirty and Assigned(FUpdateSubject) then
  begin
    FIsDirty := False;

    if FUpdateSubject is TAudioStructure then
    begin
      // Add/Remove tracks
      DiffLists(
        TAudioStructure(FUpdateSubject).Tracks,
        FTrackViewList, @CreateTrackGUI, @DeleteTrackGUI);
    end
    else if FUpdateSubject is TTrack then
    begin
      // Add/Remove patterns
    end
    else if FUpdateSubject is TPattern then
    begin
      if FUpdateSubject is TMidiPattern then
      begin
        // Changed MIDI notes
      end
      else if FUpdateSubject is TWavePattern then
      begin
        // Changed waveforms
      end;
    end;

    Invalidate;
  end;

  for lIndex := 0 to Pred(FTrackViewList.Count) do
  begin
    TTrackView(FTrackViewList[lIndex]).UpdateView;
  end;
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
        FMouseY - FDragStart.YOffset,
        FBGRABitmap, FSelectedPattern);
    end;

    FBGRABitmap.Draw(Canvas, 0, 0, False);

    DrawCursors(Canvas);
  end;
end;

procedure TSessionGrid.DrawCursors(ACanvas: TCanvas);
var
  lIndex: integer;
begin
  if not Assigned(Model) then
    exit;

  for lIndex := 0 to Pred(FTrackViewList.Count) do
  begin
    FTrackViewList[lIndex].RenderCursor(ACanvas);
  end;
end;

procedure TSessionGrid.DrawTrackList(ABGRABitmap: TBGRABitmap);
var
  lIndex: integer;
begin
  if not Assigned(Model) then
    exit;

  for lIndex := 0 to Pred(FTrackViewList.Count) do
  begin
    FTrackViewList[lIndex].Render(0, 0, ABGRABitmap);
  end;
end;

procedure TSessionGrid.GetDragPosition(X, Y: integer; var ADragPosition: TDragPosition);
var
  lIndex: integer;
  lPosition: integer;
begin
  ADragPosition.TrackID := '';
  ADragPosition.Position := -1;

  for lIndex := 0 to Pred(FTrackViewList.Count) do
  begin
    if (X >= FTrackViewList[lIndex].Left) and
      (X < FTrackViewList[lIndex].Left + FTrackViewList[lIndex].Width) then
    begin
      lPosition := Y div PATTERN_HEIGHT;

      if Y div PATTERN_HEIGHT < FVisiblePatternCount then
      begin
        ADragPosition.TrackID := FTrackViewList[lIndex].ObjectID;
        ADragPosition.Position := lPosition + ScrollIndex;
        ADragPosition.XOffset := X - FTrackViewList[lIndex].Left;
        ADragPosition.YOffset := Y - lPosition * PATTERN_HEIGHT;
      end;

      break;
    end;
  end;
end;

procedure TSessionGrid.DoRenamePattern(Sender: TObject);
var
  lRenamePattern: TFmRenamePattern;
begin
  FMode := mRename;

  lRenamePattern := TFmRenamePattern.Create(nil);
  try
    lRenamePattern.PatternName := FSelectedPattern.PatternName;
    if lRenamePattern.ShowModal = mrOk then
    begin
      FSelectedPattern.PatternName := lRenamePattern.PatternName;
    end;
  finally
    lRenamePattern.Free;
  end;
end;

procedure TSessionGrid.OnUpdateRenamePattern(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FSelectedPattern);
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

procedure TSessionGrid.CreateTrackType(ATrackType: TTrackType);
var
  lCreateTrack: TCreateTrackCommand;
begin
  lCreateTrack := TCreateTrackCommand.Create(GAudioStruct.ObjectID);
  try
    lCreateTrack.SourceType := fsEmpty;
    lCreateTrack.TrackType := ATrackType;

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

procedure TSessionGrid.DoCreateTrack(Sender: TObject);
begin
  CreateTrackType(ttNormal);
end;

procedure TSessionGrid.DoCreateGroupTrack(Sender: TObject);
begin
  CreateTrackType(ttGroup);
end;

procedure TSessionGrid.DoCreateReturnTrack(Sender: TObject);
begin
  CreateTrackType(ttReturn);
end;

procedure TSessionGrid.DoCreateMasterTrack(Sender: TObject);
begin
  CreateTrackType(ttMaster);
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
function TSessionGrid.GetTrack(X, Y: integer): TTrack;
var
  lTrackView: TTrackView;
begin
  Result := nil;

  if not Assigned(Model) then
    exit;

  lTrackView := GetTrackView(X, Y);
  if Assigned(lTrackView) then
  begin
    Result := TTrack(lTrackView.Model);
  end;
end;

{
  Get trackview under mouse if any
}
function TSessionGrid.GetTrackView(AX, AY: integer): TTrackView;
var
  lIndex: integer;
begin
  Result := nil;

  if not Assigned(Model) then
    exit;

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
function TSessionGrid.GetPatternState(ATrack: TTrack; Y: integer): TPattern;
var
  lIndex: integer;
  lPattern: TPattern;
begin
  Result := nil;

  if not Assigned(Model) then
    exit;

  for lIndex := 0 to Pred(ATrack.PatternList.Count) do
  begin
    lPattern := TPattern(ATrack.PatternList[lIndex]);

    if Y div PATTERN_HEIGHT + ScrollIndex = lPattern.Position then
    begin
      Result := TPattern(ATrack.PatternList[lIndex]);

      break;
    end;
  end;
end;

procedure TSessionGrid.CalculateTrackOffsets;
var
  lIndex: integer;
  lTrackState: TTrackView;
  lTrackNormalIndex: Integer;
  lTrackGroupIndex: Integer;
  lTrackReturnIndex: Integer;
  lTrackMasterIndex: Integer;
begin
  FTrackNormalCount := 0;
  FTrackGroupCount := 0;
  FTrackReturnCount := 0;
  FTrackMasterCount := 0;

  for lIndex := 0 to Pred(FTrackViewList.Count) do
  begin
    lTrackState := FTrackViewList[lIndex];
    case lTrackState.TrackType of
      ttNormal: Inc(FTrackNormalCount);
      ttGroup: Inc(FTrackGroupCount);
      ttReturn: Inc(FTrackReturnCount);
      ttMaster: Inc(FTrackMasterCount);
    end;
  end;

  lTrackNormalIndex := 0;
  lTrackGroupIndex := 0;
  lTrackReturnIndex := 0;
  lTrackMasterIndex := 0;
  for lIndex := 0 to Pred(FTrackViewList.Count) do
  begin
    lTrackState := FTrackViewList[lIndex];
    case lTrackState.TrackType of
      ttNormal:
      begin
        lTrackState.Left := lTrackNormalIndex * TRACK_WIDTH;
        Inc(lTrackNormalIndex);

        lTrackState.FActiveSwitch.CaptionOff := IntToStr(lTrackNormalIndex);
        lTrackState.FActiveSwitch.CaptionOn := IntToStr(lTrackNormalIndex);
        lTrackState.FActiveSwitch.Color := clYellow;
      end;
      ttGroup:
      begin
        lTrackState.Left := Self.Width -
          ((FTrackMasterCount +
          FTrackReturnCount +
          FTrackGroupCount) * TRACK_WIDTH) +
          lTrackGroupIndex * TRACK_WIDTH;
        Inc(lTrackGroupIndex);

        lTrackState.FActiveSwitch.CaptionOff := IntToStr(lTrackGroupIndex);
        lTrackState.FActiveSwitch.CaptionOn := IntToStr(lTrackGroupIndex);
        lTrackState.FActiveSwitch.Color := clLime;
      end;
      ttReturn:
      begin
        lTrackState.Left := Self.Width -
          ((FTrackMasterCount +
          FTrackReturnCount) * TRACK_WIDTH) +
          lTrackReturnIndex * TRACK_WIDTH;
        Inc(lTrackReturnIndex);

        lTrackState.FActiveSwitch.CaptionOff := IntToStr(lTrackReturnIndex);
        lTrackState.FActiveSwitch.CaptionOn := IntToStr(lTrackReturnIndex);
        lTrackState.FActiveSwitch.Color := clGreen;
      end;
      ttMaster:
      begin
        lTrackState.Left := (Self.Width -
          (FTrackMasterCount * TRACK_WIDTH)) +
          lTrackMasterIndex * TRACK_WIDTH;
        Inc(lTrackMasterIndex);

        lTrackState.FActiveSwitch.CaptionOff := IntToStr(lTrackMasterIndex);
        lTrackState.FActiveSwitch.CaptionOn := IntToStr(lTrackMasterIndex);
        lTrackState.FActiveSwitch.Color := clRed;
      end;
    end;
  end;

end;

procedure TSessionGrid.CreateTrackGUI(AObjectID: string);
var
  lTrack: TTrack;
  lTrackState: TTrackView;
begin
  DBLog('start TSessionGrid.CreateTrackGUI');

  lTrack := TTrack(GObjectMapper.GetModelObject(AObjectID));

  if Assigned(lTrack) then
  begin
    lTrackState := TTrackView.Create(AObjectID, Self);
    lTrackState.TrackType := lTrack.TrackType;
    lTrackState.Width := TRACK_WIDTH;

    FTrackViewList.Add(lTrackState);

    lTrack.Attach(lTrackState);

    CalculateTrackOffsets;
  end;

  DBLog('end TSessionGrid.CreateTrackGUI');
end;

procedure TSessionGrid.DeleteTrackGUI(AObjectID: string);
var
  lIndex: integer;
begin
  for lIndex := Pred(FTrackViewList.Count) downto 0 do
  begin
    if FTrackViewList[lIndex].ObjectID = AObjectID then
    begin
      FTrackViewList.Remove(FTrackViewList[lIndex]);
    end;
  end;

  CalculateTrackOffsets;
end;

procedure TSessionGrid.SetScrollIndex(AValue: integer);
begin
  if FScrollIndex = AValue then
    Exit;

  FScrollIndex := AValue;

  Invalidate;
end;

{
  Detects if a file is dropped from the treeview and gives back
  the filename to the parent control through the DroppedFileEvent
}
procedure TSessionGrid.DragDrop(Source: TObject; X, Y: integer);
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
        lCreatePattern.PatternName :=
          ExtractFileNameWithoutExt(TTreeFolderData(lTreeView.Selected.Data).Path);
        lCreatePattern.Position := (Y div PATTERN_HEIGHT) + ScrollIndex;

        GCommandQueue.PushCommand(lCreatePattern);
      except
        lCreatePattern.Free;
      end;
    end;
  end;

  inherited DragDrop(Source, X, Y);
end;

function TSessionGrid.PatternAtMouseXY(X, Y: integer): TPattern;
var
  lIndex: integer;
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

procedure TSessionGrid.SelectTrack(ASelected: TTrackView);
var
  lTrackIndex: integer;
begin
  if not Assigned(ASelected) then
  begin
    // Something other than a track is clicked, so unselect all tracks
    for lTrackIndex := 0 to Pred(FTrackViewList.Count) do
    begin
      FTrackViewList[lTrackIndex].TrackControls.BevelInner := bvRaised;
      FTrackViewList[lTrackIndex].TrackControls.Invalidate;
    end;
  end
  else
  begin
    // A track has been clicked on, so unselect all but the clicked track
    for lTrackIndex := 0 to Pred(FTrackViewList.Count) do
    begin
      if FTrackViewList[lTrackIndex].ObjectID = ASelected.ObjectID then
      begin
        FTrackViewList[lTrackIndex].TrackControls.BevelInner := bvLowered;
        GSettings.SelectedObject := FTrackViewList[lTrackIndex].Model;
      end
      else
      begin
        FTrackViewList[lTrackIndex].TrackControls.BevelInner := bvRaised;
      end;

      FTrackViewList[lTrackIndex].TrackControls.Invalidate;
    end;
  end;
end;

procedure TSessionGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  SetFocus;

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

procedure TSessionGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
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
              GSettings.SelectedObject := lPattern;

              // Unselect all tracks
              SelectTrack(nil);

              if Assigned(FOnPatternRefreshGUI) then
              begin
                FOnPatternRefreshGUI(GSettings.SelectedObject);
              end;
            end;
          end;
        end
        else
        begin
          // Unselect all tracks
          SelectTrack(nil);

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

procedure TSessionGrid.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  FMouseX := X;
  FMouseY := Y;

  // Detect start of dragging
  if FMouseDownL and (not FDragging) and Assigned(FSelectedPattern) then
  begin
    FDragging :=
      (abs(FMouseDownX - X) > 5) or (abs(FMouseDownY - Y) > 5);
  end;

  if FMouseDownL then
  begin
    Invalidate;
  end;

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
      lCreateTrack.TrackType := ttNormal;

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
  lIndex: integer;

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

    CalculateTrackOffsets;
  end;
end;

function TSessionGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): boolean;
begin
  ScrollIndex := ScrollIndex + 1;

  Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

function TSessionGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean;
begin
  ScrollIndex := ScrollIndex - 1;

  Result := inherited DoMouseWheelUp(Shift, MousePos);
end;

procedure TSessionGrid.DragOver(Source: TObject; X, Y: integer;
  State: TDragState; var Accept: boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);

  Accept := True;
end;

procedure TSessionGrid.HandleKeyDownSelectMode(var Key: word; Shift: TShiftState);
begin
  // Scroll through grid, changing te selected pattern at the bottom
end;

procedure TSessionGrid.HandleKeyDownRenameMode(var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_LEFT:
    begin
      if FRenameCursorPosition > 0 then
      begin
        Dec(FRenameCursorPosition);
      end;
    end;
    VK_RIGHT:
    begin
      if FRenameCursorPosition < 10 then
      begin
        Inc(FRenameCursorPosition);
      end;
    end;
    VK_RETURN:
    begin
      FMode := mSelect;
    end;
    VK_ESCAPE:
    begin
      FMode := mSelect;
      FSelectedPattern.PatternName := FTempPatternName;
    end
    else
    begin
    end;
  end;
end;

       (*
procedure TSessionGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  {case FMode of
    mSelect: HandleKeyDownSelectMode(Key, Shift);
    mRename: HandleKeyDownRenameMode(Key, Shift);
  end;}

  inherited KeyDown(Key, Shift);
end;

procedure TSessionGrid.KeyPress(var Key: Char);
var
  lTemp: string;
begin
  if Assigned(FSelectedPattern) then
  begin
    {if ((Key >= 'A') and (Key <= 'Z')) or
        ((Key >= 'a') and (Key <= 'z')) or
        ((Key >= '0') and (Key <= '9')) then
    begin
      lTemp := FSelectedPattern.PatternName;
      lTemp[FRenameCursorPosition] := Key;
      FSelectedPattern.PatternName := lTemp;
    end
    else}
    begin
      inherited KeyPress(Key);
    end;
  end
end; *)

end.
