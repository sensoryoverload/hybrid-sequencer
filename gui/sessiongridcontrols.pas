unit sessiongridcontrols;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, Forms, ExtCtrls, Math, Spin,
  StdCtrls, ShellCtrls, ComCtrls, DateUtils, LResources, BGRABitmap, BGRABitmapTypes,
  contnrs, dialcontrol, Menus;


type
  TBaseControlLite = class;

  { TControlLiteContainer }

  TControlLiteContainer = class
  private
    FCanvas: TBGRABitmap;
    FControls: TObjectList;
    FHeight: Integer;
    FLeft: Integer;
    FOnClick: TNotifyEvent;
    FParent: TWinControl;
    FSelected: Boolean;
    FTop: Integer;
    FWidth: Integer;
    FInvalidated: Boolean;
    FFocusedControl: TBaseControlLite;

    function FindControl(X, Y: Integer): TBaseControlLite;
    function GetClientOrigin: TPoint;
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AChild: TBaseControlLite);
    procedure Paint;
    procedure Invalidate;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);

    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
    property Selected: Boolean read FSelected write FSelected;
    property ClientOrigin: TPoint read GetClientOrigin;

    property Controls: TObjectList read FControls write FControls;
    property Canvas: TBGRABitmap read FCanvas write FCanvas;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;

    property Parent: TWinControl read FParent write FParent;
    property Invalidated: Boolean read FInvalidated;
  end;

  { TBaseControlLite }

  TBaseControlLite = class
  private
    FCanvas: TBGRABitmap;
    FHeight: Integer;
    FLeft: Integer;
    FParent: TControlLiteContainer;
    FTop: Integer;
    FVisible: Boolean;
    FWidth: Integer;
    FInvalidated: Boolean;
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure ResizeCanvas;
  public
    constructor Create(AOwner: TControlLiteContainer); virtual;
    destructor Destroy; override;
    procedure Paint; virtual;
    procedure Invalidate;

    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
    property Canvas: TBGRABitmap read FCanvas write FCanvas;
    property Invalidated: Boolean read FInvalidated write FInvalidated;
    property Visible: Boolean read FVisible write FVisible;
    property Parent: TControlLiteContainer read FParent write FParent;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
  end;

  TOrientationLite = (oHorizontalLite, oVerticalLite, oBalanceLite);

  { TParameterControlLite }

  TParameterControlLite = class(TBaseControlLite)
  private
    FBoolean: Boolean;
    FMax: Single;
    FMidiMappingMode: Boolean;
    FMin: Single;
    FOrientation: TOrientationLite;
    FSize: Integer;
    FValue: Single;
    FScreenValue: Integer;
    FCaption: string;
    FOnStartChange: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FChanging: Boolean;
    FScaleValueToScreen: Single;
    FScaleScreenToValue: Single;

    procedure SetCaption(AValue: string);
    procedure SetMax(AValue: Single);
    procedure SetMin(AValue: Single);
    procedure SetOrientation(AValue: TOrientationLite);
    procedure SetSize(AValue: Integer);
    procedure SetValue(const AValue: Single);
    procedure UpdateScreenValue(X, Y: Integer);
  protected
    procedure UpdateScale;
  public
    constructor Create(AOwner : TControlLiteContainer); override;
    procedure Paint; override;
    property MidiMappingMode: Boolean read FMidiMappingMode write FMidiMappingMode;
  published
    property Value: Single read FValue write SetValue;
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property Caption: string read FCaption write SetCaption;
    property ShowValue: Boolean read FBoolean write FBoolean;
    property Orientation: TOrientationLite read FOrientation write SetOrientation;
    property Size: Integer read FSize write SetSize;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnStartChange: TNotifyEvent read FOnStartChange write FOnStartChange;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  end;

  { TToggleControlLite }

  TToggleControlLite = class(TBaseControlLite{, IFeedBack})
  private
    FColor: TColor;
    FFontSize: Integer;
    FFontStyle: TFontStyles;
    FSwitchedOn: Boolean;
    FLastSwitchedOn: Boolean;
    FCaption: string;
    FCaptionOn: string;
    FCaptionOff: string;
    FOnChange: TNotifyEvent;
    procedure SetCaptionOff(const AValue: string);
    procedure SetCaptionOn(const AValue: string);
    procedure SetColor(AValue: TColor);
    procedure SetSwitchedOn(const AValue: Boolean);
  public
    constructor Create(AOwner : TControlLiteContainer); override;
    procedure Paint; override;
    procedure UpdateControl;
    property FontSize: Integer read FFontSize write FFontSize;
    property FontStyle: TFontStyles read FFontStyle write FFontStyle;
  published
    property SwitchedOn: Boolean read FSwitchedOn write SetSwitchedOn;
    property CaptionOn: string read FCaptionOn write SetCaptionOn;
    property CaptionOff: string read FCaptionOff write SetCaptionOff;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Color: TColor read FColor write SetColor;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
  end;

  { TVolumeControlLite }

  TVolumeControlLite = class(TBaseControlLite{, IFeedBack})
  private
    FPosition: Single;
    FY: Integer;
    FFaderMoving: Boolean;
    FRange: Integer;

    // This points to a location where the present audio level is located
    FChannelCount: Byte;
    FChannelLevel: TChannel;
    FOnChange: TNotifyEvent;
    FOnStartChange: TNotifyEvent;
    FOnEndChange: TNotifyEvent;
    function GetLevelLeft: single;
    function GetLevelRight: single;
    function GetVolumeMultiplier: Single;
    procedure SetLevelLeft(const AValue: single);
    procedure SetLevelRight(const AValue: single);
    procedure SetPosition(const AValue: Single);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner : TControlLiteContainer); override;
    procedure Paint; override;
    procedure Update; reintroduce;
    procedure UpdateControl;
    property Position: Single read FPosition write SetPosition;
    property VolumeMultiplier: Single read GetVolumeMultiplier;
    property LevelLeft: single read GetLevelLeft write SetLevelLeft;
    property LevelRight: single read GetLevelRight write SetLevelRight;
    property ChannelCount: Byte read FChannelCount write FChannelCount;
    property ChannelLevel: TChannel read FChannelLevel write FChannelLevel;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnStartChange: TNotifyEvent read FOnStartChange write FOnStartChange;
    property OnEndChange: TNotifyEvent read FOnEndChange write FOnEndChange;
  end;

  { TListSelectLite }

  TListSelectLite = class(TBaseControlLite)
  private
    FItems: TStringList;
    FItemIndex: Integer;
    FPopupMenu: TPopupMenu;
    FOnChange: TNotifyEvent;
    procedure SetItemIndex(AValue: Integer);
  protected
    procedure DoClick(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
  public
    constructor Create(AOwner : TControlLiteContainer); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Items: TStringList read FItems write FItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TControlLiteContainer }

function TControlLiteContainer.FindControl(X, Y: Integer): TBaseControlLite;
var
  lIndex: Integer;
  lControl: TBaseControlLite;
begin
  Result := nil;

  for lIndex := 0 to Pred(FControls.Count) do
  begin
    lControl := TBaseControlLite(FControls[lIndex]);
    if Assigned(lControl.Parent) then
    begin
      if (lControl.Left < X) and (lControl.Left + lControl.width > X) and
        (lControl.Top < Y) and (lControl.Top + lControl.Height > Y) then
      begin
        Result := lControl;
        break;
      end;
    end;
  end;
end;

function TControlLiteContainer.GetClientOrigin: TPoint;
begin
  if Assigned(FParent) then
  begin
    Result := FParent.ClientOrigin;
  end
  else
  begin
    Result := Point(0, 0);
  end;
end;

procedure TControlLiteContainer.SetHeight(AValue: Integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
  Invalidate;
end;

procedure TControlLiteContainer.SetLeft(AValue: Integer);
begin
  if FLeft=AValue then Exit;
  FLeft:=AValue;
  Invalidate;
end;

procedure TControlLiteContainer.SetTop(AValue: Integer);
begin
  if FTop=AValue then Exit;
  FTop:=AValue;
  Invalidate;
end;

procedure TControlLiteContainer.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  Invalidate;
end;

constructor TControlLiteContainer.Create;
begin
  FControls := TObjectList.create(True);

  FFocusedControl := nil;

  FSelected := False;

  FInvalidated := False;
end;

destructor TControlLiteContainer.Destroy;
begin
  FControls.Free;

  inherited Destroy;
end;

procedure TControlLiteContainer.Add(AChild: TBaseControlLite);
begin
  FControls.Add(AChild);
end;

procedure TControlLiteContainer.Paint;
var
  lIndex: Integer;
  lControl: TBaseControlLite;
begin
  if FInvalidated then
  begin
    FCanvas.FillRect(FLeft + 1, FTop + 1, FLeft + FWidth - 1, FTop + FHeight - 1, ColorToBGRA(clLtGray), dmSet);
    if FSelected then
    begin
      FCanvas.Rectangle(FLeft, FTop, FLeft + FWidth, FTop + FHeight, clWhite);
    end
    else
    begin
      FCanvas.Rectangle(FLeft, FTop, FLeft + FWidth, FTop + FHeight, RGBToColor(100, 100, 100));
    end;
  end;

  for lIndex := 0 to Pred(FControls.Count) do
  begin
    lControl := TBaseControlLite(FControls[lIndex]);
    if Assigned(lControl.Parent) then
    begin
      if lControl.Invalidated then
      begin
        lControl.Paint;
        lControl.Invalidated := False;
        FCanvas.PutImage(FLeft + lControl.Left, FTop + lControl.Top, lControl.Canvas, dmSet);
      end;
    end;
  end;

  FInvalidated := False;
end;

procedure TControlLiteContainer.Invalidate;
var
  lIndex: Integer;
begin
  FInvalidated := True;

  for lIndex := 0 to Pred(FControls.Count) do
  begin
    TBaseControlLite(FControls[lIndex]).Invalidate;
  end;
end;

procedure TControlLiteContainer.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FFocusedControl := FindControl(X - FLeft, Y - FTop);
  if Assigned(FFocusedControl) then
  begin
    FFocusedControl.MouseDown(
      Button,
      Shift,
      X - FFocusedControl.Left - FLeft,
      Y - FFocusedControl.Top - FTop);
  end;

  // Container clicked
  if Assigned(FOnClick) then
  begin
    FOnClick(Self);
  end;

  FSelected := True;
end;

procedure TControlLiteContainer.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FFocusedControl) then
  begin
    FFocusedControl.MouseUp(
      Button,
      Shift,
      X - FFocusedControl.Left - FLeft,
      Y - FFocusedControl.Top - FTop);
    FFocusedControl := nil;
  end;
end;

procedure TControlLiteContainer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FFocusedControl) then
  begin
    FFocusedControl.MouseMove(Shift, X - FFocusedControl.Left - FLeft, Y - FFocusedControl.Top - FTop);
  end;
end;

{ TBaseControlLite }

procedure TBaseControlLite.SetHeight(AValue: Integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;

  ResizeCanvas;
end;

procedure TBaseControlLite.SetLeft(AValue: Integer);
begin
  if FLeft=AValue then Exit;
  FLeft:=AValue;

  ResizeCanvas;
end;

procedure TBaseControlLite.SetTop(AValue: Integer);
begin
  if FTop=AValue then Exit;
  FTop:=AValue;

  ResizeCanvas;
end;

procedure TBaseControlLite.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;

  ResizeCanvas;
end;

procedure TBaseControlLite.ResizeCanvas;
begin
  if Assigned(FCanvas) then
  begin
    FCanvas.Free;
  end;
  FCanvas := TBGRABitmap.Create(FWidth, FHeight);
end;

constructor TBaseControlLite.Create(AOwner: TControlLiteContainer);
begin
  AOwner.Add(Self);

  FWidth := 0;
  FHeight := 0;

  ResizeCanvas;

  FVisible := True;
end;

destructor TBaseControlLite.Destroy;
begin
  if Assigned(FCanvas) then
  begin
    FCanvas.Free;
  end;

  inherited Destroy;
end;

procedure TBaseControlLite.Paint;
begin
  FInvalidated := False;
end;

procedure TBaseControlLite.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  // Virtual
end;

procedure TBaseControlLite.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  // Virtual
end;

procedure TBaseControlLite.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  // Virtual
end;

procedure TBaseControlLite.Invalidate;
begin
  FInvalidated := True;
end;

{ TParameterControlLite }

procedure TParameterControlLite.SetValue(const AValue: Single);
begin
  if FValue = AValue then exit;
  FValue := AValue;

  if (FOrientation = oHorizontalLite) or (FOrientation = oBalanceLite) then
  begin
    FScreenValue := Round(FScaleValueToScreen * (FValue - FMin));
  end
  else
  begin
    FScreenValue := Round(FScaleValueToScreen * (FValue - FMin));
  end;

  Invalidate;
end;

procedure TParameterControlLite.SetMax(AValue: Single);
begin
  if FMax = AValue then exit;
  FMax := AValue;

  UpdateScale;
  Invalidate;
end;

procedure TParameterControlLite.SetCaption(AValue: string);
begin
  if FCaption = AValue then Exit;
  FCaption := AValue;
  Invalidate;
end;

procedure TParameterControlLite.SetMin(AValue: Single);
begin
  if FMin = AValue then exit;
  FMin := AValue;
  UpdateScale;
  Invalidate;
end;

procedure TParameterControlLite.SetOrientation(AValue: TOrientationLite);
begin
  if FOrientation = AValue then exit;
  FOrientation := AValue;
  if (Orientation = oHorizontalLite) or (Orientation = oBalanceLite) then
  begin
    Height := 13;
    Width := Size;
  end
  else
  begin
    Width := 13;
    Height := Size;
  end;
  Invalidate;
end;

procedure TParameterControlLite.SetSize(AValue: Integer);
begin
  if FSize = AValue then Exit;
  FSize := AValue;
  Invalidate;
end;

constructor TParameterControlLite.Create(AOwner: TControlLiteContainer);
begin
  inherited Create(AOwner);

  Size := 100;
  ShowValue := True;
  FValue := 0.0000001; // Autoinitialize

  Canvas.FontStyle := [fsBold];

  FChanging := False;
end;

procedure TParameterControlLite.Paint;
var
  lCaption: string;
begin
  if Orientation = oBalanceLite then
  begin
    Canvas.Rectangle(0, 0, Width, Height, ColorToBGRA(clBlack), ColorToBGRA(clLtGray), dmSet);
    Canvas.Rectangle(Width div 2, 0, FScreenValue, Height, ColorToBGRA(clBlack), ColorToBGRA(clGray), dmSet);
    Canvas.DrawVertLine(Width div 2, 0, Height, ColorToBGRA(clGray));
    Canvas.FontHeight := Height - 1;
    if ShowValue then
    begin
      lCaption := FCaption + ' ' + FormatFloat('#.#', FValue)
    end
    else
    begin
      lCaption := FCaption
    end;
    Canvas.TextOut(1, 0, lCaption, ColorToBGRA(ColorToRGB(clBtnText)));
  end
  else if Orientation = oHorizontalLite then
  begin
    Canvas.Rectangle(0, 0, FScreenValue, Height, ColorToBGRA(clBlack), ColorToBGRA(clGray), dmSet);
    Canvas.Rectangle(FScreenValue, 0, Width, Height, ColorToBGRA(clBlack), ColorToBGRA(clLtGray), dmSet);
    Canvas.FontHeight := Height - 1;
    if ShowValue then
    begin
      lCaption := FCaption + ' ' + FormatFloat('#.#', FValue)
    end
    else
    begin
      lCaption := FCaption
    end;
    Canvas.TextOut(1, 0, lCaption, ColorToBGRA(ColorToRGB(clBtnText)));
  end
  else if Orientation = oVerticalLite then
  begin
    Canvas.Rectangle(0, 0, Width, FScreenValue, ColorToBGRA(clBlack), ColorToBGRA(clGray), dmSet);
    Canvas.Rectangle(0, FScreenValue, Width, Height, ColorToBGRA(clBlack), ColorToBGRA(clLtGray), dmSet);
    Canvas.FontHeight := Width - 2;
    if ShowValue then
    begin
      lCaption := FCaption + ' ' + FormatFloat('#.#', FValue)
    end
    else
    begin
      lCaption := FCaption
    end;
    Canvas.TextOut(1, 1, lCaption, ColorToBGRA(ColorToRGB(clBtnText)));
  end;
end;

procedure TParameterControlLite.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FChanging := True;

  UpdateScreenValue(X, Y);

  if Assigned(FOnStartChange) then
  begin
    FOnStartChange(Self);
  end;

  Invalidate;
end;

procedure TParameterControlLite.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  UpdateScreenValue(X, Y);

  Invalidate;

  FChanging := False;
end;

procedure TParameterControlLite.UpdateScreenValue(X, Y: Integer);
begin
  if FChanging then
  begin
    if (Orientation = oHorizontalLite) or (Orientation = oBalanceLite) then
    begin
      if X > Pred(FWidth) then
      begin
        FScreenValue := Pred(FWidth);
      end
      else if X < 0 then
      begin
        FScreenValue := 0;
      end
      else
      begin
        FScreenValue := X;
      end;

      FValue := FScaleScreenToValue * FScreenValue + FMin;
    end
    else if Orientation = oVerticalLite then
    begin
      if Y > Height then
      begin
        FScreenValue := Height;
      end
      else if Y < 0 then
      begin
        FScreenValue := 0;
      end
      else
      begin
        FScreenValue := Y;
      end;

      FValue := FScaleScreenToValue * FScreenValue + FMin;
    end;
  end;
end;

procedure TParameterControlLite.UpdateScale;
begin
  if (Orientation = oHorizontalLite) or (Orientation = oBalanceLite) then
  begin
    FScaleScreenToValue := (FMax - FMin) / FWidth;
    FScaleValueToScreen := FWidth / (FMax - FMin);
  end
  else
  begin
    FScaleScreenToValue := (FMax - FMin) / Height;
    FScaleValueToScreen := Height / (FMax - FMin);
  end;
end;

procedure TParameterControlLite.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FChanging then
  begin
    UpdateScreenValue(X, Y);

    if Assigned(FOnChange) then
    begin
      FOnChange(Self);
    end;

    Invalidate;
  end;
end;

{ TToggleControlLite }

procedure TToggleControlLite.SetCaptionOff(const AValue: string);
begin
  if FCaptionOff = AValue then exit;
  FCaptionOff:= AValue;
  FCaption:= FCaptionOff;
  Invalidate;
end;

procedure TToggleControlLite.SetCaptionOn(const AValue: string);
begin
  if FCaptionOn = AValue then exit;
  FCaptionOn:= AValue;
  Invalidate;
end;

procedure TToggleControlLite.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  Invalidate;
end;

procedure TToggleControlLite.SetSwitchedOn(const AValue: Boolean);
begin
  if FSwitchedOn = AValue then exit;
  FSwitchedOn := AValue;
  Invalidate;
end;

constructor TToggleControlLite.Create(AOwner: TControlLiteContainer);
begin
  inherited Create(AOwner);

  FColor := clYellow;

  Width := 40;
  Height := 20;

  FFontStyle := [];
  FFontSize := 8;

  FSwitchedOn := False;
  FCaption := 'Off';
end;

procedure TToggleControlLite.Paint;
begin
  Canvas.FillRect(0, 0, Width, Height, ColorToBGRA(clGray), dmSet);
  Canvas.FontHeight := FFontSize;
  Canvas.FontStyle := FFontStyle;

  if FSwitchedOn then
    FCaption := FCaptionOn
  else
    FCaption := FCaptionOff;

  // Outline color
  Canvas.PenStyle := psSolid;

  if FSwitchedOn then
    Canvas.Rectangle(0, 0, Width, Height, ColorToBGRA(clBlack), ColorToBGRA(FColor), dmSet)
  else
    Canvas.Rectangle(0, 0, Width, Height, ColorToBGRA(clBlack), ColorToBGRA(clLtGray), dmSet);

  Canvas.TextOut(
    (Width shr 1) - (Canvas.TextSize(FCaption).cx shr 1),
    1,
    FCaption,
    ColorToBGRA(ColorToRGB(clBtnText)));
end;

procedure TToggleControlLite.UpdateControl;
begin
  Invalidate;
end;

procedure TToggleControlLite.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FLastSwitchedOn:= FSwitchedOn;

  if Assigned(FOnChange) then
    FOnChange(Self);

  Invalidate;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TToggleControlLite.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  // Return to original state if not in control at mouseup
  if (X < 0) or (X > Width) or (Y < 0) or (Y > Height) then
  begin
    FSwitchedOn:= FLastSwitchedOn;
    Invalidate;
  end;

  inherited MouseUp(Button, Shift, X, Y);
end;

function TVolumeControlLite.GetVolumeMultiplier: Single;
begin
  Result:= FPosition * 0.01; { FPosition / 100}
end;

function TVolumeControlLite.GetLevelLeft: single;
begin
  Result := ChannelLevel[0];
end;

function TVolumeControlLite.GetLevelRight: single;
begin
  Result := ChannelLevel[1];
end;

procedure TVolumeControlLite.SetLevelLeft(const AValue: single);
begin
  if AValue <> ChannelLevel[0] then
  begin
    ChannelLevel[0] := AValue;
    Invalidate;
  end;
end;

procedure TVolumeControlLite.SetLevelRight(const AValue: single);
begin
  if AValue <> ChannelLevel[1] then
  begin
    ChannelLevel[1] := AValue;
    Invalidate;
  end;
end;

procedure TVolumeControlLite.SetPosition(const AValue: Single);
begin
  if FPosition <> AValue then
  begin
    FPosition := AValue;
    Invalidate;
  end;
end;

constructor TVolumeControlLite.Create(AOwner: TControlLiteContainer);
var
  i: byte;
begin
  inherited Create(AOwner);

  Width := 30;

  ChannelCount := 2;
  for i := 0 to 7 do
  begin
    ChannelLevel[i] := 0;
  end;

  FPosition:= 75;
  FRange := 100;
  FFaderMoving:= False;
end;

procedure TVolumeControlLite.Paint;
const
  FADER_HEIGHT = 24;
  FADER_HEIGHT_HALF = FADER_HEIGHT div 2;
var
  HeightScale: Integer;
  i: Integer;
  ChannelOffset: Integer;
begin
  // Draws the background
  Canvas.FillRect(0, 0, Width, Height, clLtGray);

  // Draw Levels for all channels
  for i := 0 to ChannelCount - 1 do
  begin
    ChannelOffset := i * 5 + 11;
    HeightScale:= Round(Height * ChannelLevel[i]);

    // In the RED!
    Canvas.FillRect(ChannelOffset, 1, ChannelOffset + 4, 20, clRed);

    // You've been warned
    Canvas.FillRect(ChannelOffset, 21, ChannelOffset + 4, 40, clYellow);

    // Behaving signal
    Canvas.FillRect(ChannelOffset, 41, ChannelOffset + 4, Height - 1, clLime);

    // Signal level
    Canvas.FillRect(ChannelOffset, 1, ChannelOffset + 4, Height - HeightScale, clLtGray);
  end;

  Canvas.FillRect(4, 0, 6, Height, RGBToColor(50, 50, 50));

  // Draw FaderHandle
  FY := Round(Height - FADER_HEIGHT_HALF - (FPosition * ((Height - FADER_HEIGHT) * DIVBY100)));
  Canvas.RoundRect(
    0, FY - FADER_HEIGHT_HALF, 10, FY + FADER_HEIGHT_HALF,
    4, 4,
    ColorToBGRA(RGBToColor(50, 50, 50)), ColorToBGRA(RGBToColor(255, 255, 255)));

  Canvas.FillRect(0, FY - 1, 10, FY + 1, RGBToColor(50, 50, 50));
end;

procedure TVolumeControlLite.Update;
begin
  Invalidate;
end;

procedure TVolumeControlLite.UpdateControl;
begin
  Invalidate;
end;

procedure TVolumeControlLite.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Assigned(FOnStartChange) then
    FOnStartChange(Self);

  FFaderMoving := True;

  Invalidate;
end;

procedure TVolumeControlLite.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if Assigned(FOnEndChange) then
    FOnEndChange(Self);

  FFaderMoving := False;

  Invalidate;
end;

procedure TVolumeControlLite.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  if FFaderMoving then
  begin
    FPosition:= FRange - (Y * (FRange / Height));
    if FPosition < 0 then
      FPosition:= 0;
    if FPosition > 100 then
      FPosition:= 100;

    if Assigned(FOnChange) then
      FOnChange(Self);

    Invalidate;
  end
end;

{ TListSelectLite }

procedure TListSelectLite.SetItemIndex(AValue: Integer);
begin
  if FItemIndex <> AValue then
  begin
    FItemIndex := AValue;

    Invalidate;
  end;
end;

procedure TListSelectLite.DoClick(Sender: TObject);
var
  lItemIndex: Integer;
begin
  lItemIndex := (Sender as TMenuItem).Tag;
  if FItemIndex <> lItemIndex then
  begin
    FItemIndex := lItemIndex;
    if Assigned(FOnChange) then
    begin
      FOnChange(Self);
    end;
  end;

  FPopupMenu.Close;

  Invalidate;
end;

procedure TListSelectLite.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  lItemIndex: Integer;
  lMenuItem: TMenuItem;
begin
  FPopupMenu.Items.Clear;
  for lItemIndex := 0 to Pred(FItems.Count) do
  begin
    lMenuItem := TMenuItem.Create(FPopupMenu);
    lMenuItem.Tag := lItemIndex;
    lMenuItem.OnClick := @DoClick;
    lMenuItem.Caption := FItems[lItemIndex];
    FPopupMenu.Items.Add(lMenuItem);
  end;

  if Assigned(FParent) then
  begin
    FPopupMenu.PopUp(
      FParent.ClientOrigin.X + Parent.Left + Self.Left,
      FParent.ClientOrigin.Y + Parent.Top + Self.Top + LISTITEM_HEIGHT);
  end
  else
  begin
    writeln('Parent not assigned');
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TListSelectLite.Paint;
begin
  Height := LISTITEM_HEIGHT;
  Canvas.Rectangle(
    0,
    0,
    Width,
    Height,
    ColorToBGRA(clBlack),
    ColorToBGRA(clLtGray),
    dmSet);

  if FVisible then
  begin
    if FItemIndex < FItems.Count then
    begin
      if FItemIndex <> -1 then
      begin
        Canvas.FontHeight := LISTITEM_HEIGHT - 2;
        Canvas.TextOut(
          1,
          1,
          FItems[FItemIndex],
          ColorToBGRA(ColorToRGB(clBtnText)));
      end;
    end;
  end;
end;

constructor TListSelectLite.Create(AOwner: TControlLiteContainer);
begin
  inherited Create(AOwner);

  FItems := TStringList.Create;
  FPopupMenu := TPopupMenu.Create(nil);

  Width := 50;
  Height := LISTITEM_HEIGHT;

  FCanvas.FontStyle := [fsBold];

  FInvalidated := False;

  FItemIndex := -1;
end;

destructor TListSelectLite.Destroy;
begin
  FItems.Free;
  FPopupMenu.Free;

  inherited Destroy;
end;

end.

