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

  dialcontrol.pas
}

unit dialcontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, Forms, ExtCtrls, Math, Spin,
  StdCtrls, ShellCtrls, ComCtrls, DateUtils, LResources;

const
  M_PI = 3.14159265358979323846;


Type
  PPSingle = ^PSingle;

  IFeedBack = interface
    procedure UpdateControl;
  end;

  { TDialControl }

  TDialControl = class(TCustomControl, IFeedBack)
  private
    FDialMoving: Boolean;

    FStartingInternalValue: Single;
    FOldInternalValue: Single;
    FInternalValue: Single;

    FAngle: Single;
    FOldAngle: Single;

    FRange: Single;
    FLowest: Single;
    FHighest: Single;
    FOldY: Integer;
    FY: Integer;

    FCaption: string;

    FScaleInternalValueToExternalValue: Single;
    FScaleExternalValueToInternalValue: Single;

    FOnChange: TNotifyEvent;
    FOnEndChange: TNotifyEvent;
    FOnStartChange: TNotifyEvent;
    FEnabled: Boolean;

    procedure CalcInternals(X, Y: Longint);
    procedure SetHighest(const AValue: Single);
    procedure SetLowest(const AValue: Single);
    function GetValue: Single;
    procedure SetValue(const AValue: Single);
  protected
    procedure Initialize;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure UpdateControl;
  published
    property Value: Single read GetValue write SetValue;
    property Caption: string read FCaption write FCaption;
    property Lowest: Single read FLowest write SetLowest;
    property Highest: Single read FHighest write SetHighest;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnEndChange: TNotifyEvent read FOnEndChange write FOnEndChange;
    property OnStartChange: TNotifyEvent read FOnStartChange write FOnStartChange;
    property Enabled: Boolean read FEnabled write FEnabled;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    property Color;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  end;

  { TButtonControl }

  { TToggleControl }

  TToggleControl = class(TCustomControl, IFeedBack)
  private
    FSwitchedOn: Boolean;
    FLastSwitchedOn: Boolean;
    FCaption: string;
    FCaptionOn: string;
    FCaptionOff: string;
    FCaptionWidth: Integer;
    FOnChange: TNotifyEvent;
    procedure SetCaptionOff(const AValue: string);
    procedure SetCaptionOn(const AValue: string);
    procedure SetSwitchedOn(const AValue: Boolean);

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure UpdateControl;
  published
    property SwitchedOn: Boolean read FSwitchedOn write SetSwitchedOn;
    property CaptionOn: string read FCaptionOn write SetCaptionOn;
    property CaptionOff: string read FCaptionOff write SetCaptionOff;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Color;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
  end;

  { TValueControl }

  TValueControl = class(TCustomSpinEdit, IFeedBack)
  private
    FOffset: Integer;
    FOriginalValue: Integer;
    FLastValue: Integer;
    FOldY: Integer;
    FDragging: Boolean;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure UpdateControl;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  published
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderSpacing;
    property Color;
    property Constraints;
    property Enabled;
    property Font;
    property Increment;
    property MaxValue;
    property MinValue;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    Property OnKeyDown;
    property OnKeyPress;
    Property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnUTF8KeyPress;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Value;
    property Visible;
  end;

  { TFloatSpinEditControl }

  TFloatSpinEditControl = class(TCustomFloatSpinEdit, IFeedBack)
  private
    FOffset: single;
    FOriginalValue: single;
    FLastValue: single;
    FOldY: Integer;
    FDragging: Boolean;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure UpdateControl;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  published
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderSpacing;
    property Color;
    property Constraints;
    property Enabled;
    property Font;
    property Increment;
    property MaxValue;
    property MinValue;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    Property OnKeyDown;
    property OnKeyPress;
    Property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnUTF8KeyPress;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Value;
    property Visible;
  end;

  { TTimeControl }

  TTimeControl = class(TPanel)
  private
    FValue: Integer;
    FBar: Integer;
    FBeat: Integer;
    FFrac: Integer;
    FCaption: string;
    FOnChange: TNotifyEvent;
    FLabel: TLabel;
    FSampleRate: Integer;
    FSpinEditBar: TValueControl;
    FSpinEditBeat: TValueControl;
    FSpinEditFrac: TValueControl;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  published
    property Bar: Integer read FBar write FBar;
    property Beat: Integer read FBeat write FBeat;
    property Frac: Integer read FFrac write FFrac;
    property SampleRate: Integer read FSampleRate write FSampleRate;
    property Caption: string read FCaption write FCaption;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  end;

  TChannel = Array[0..7] of Single;

  { TVolumeControl }

  TVolumeControl = class(TCustomControl, IFeedBack)
  private
    FTimer: TTimer;
    FPosition: Single;
    FY: Integer;
    FFaderMoving: Boolean;
    FPeakHold: Integer;
    FVolumeMultiplier: Single;
    FRange: Integer;

    // This points to a location where the present audio level is located
    FLevelLeft: Single;
    FLevelRight: Single;
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
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure Update;
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

  { TFilteredShellTreeView }

  TFilteredShellTreeView = class(TCustomShellTreeView)
  private
    FMask: string;
    procedure SetMask(const AValue: string);
  protected
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function  PopulateTreeNodeWithFiles(
      ANode: TTreeNode; ANodePath: string): Boolean;
  published
    { TCustomTreeView properties }
    property Align;
    property Anchors;
    property AutoExpand;
    property BorderSpacing;
    //property BiDiMode;
    property BackgroundColor;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property Enabled;
    property ExpandSignType;
    property Font;
    //property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property RowSelect;
    property ScrollBars;
    property SelectionColor;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property ToolTips;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectionChanged;
    property OnShowHint;
    property OnUTF8KeyPress;
    property Options;
    property TreeLineColor;
    property TreeLinePenStyle;
    property ExpandSignColor;
    { TCustomShellTreeView properties }
    property ObjectTypes;
    property ShellListView;
    property Mask: string read FMask write SetMask;
  end;

  { TSplitter }

  TCollapseSplitter = class(TCustomSplitter)
  published
    property Align;
    property Anchors;
    property AutoSnap;
    property Beveled;
    property Color;
    property Constraints;
    property Cursor;
    property Height;
    property MinSize;
    property OnCanResize;
    property OnChangeBounds;
    property OnMoved;
    property OnDblClick;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ResizeAnchor;
    property ResizeStyle;
    property ShowHint;
    property Visible;
    property Width;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('HybridComponentPack', [TDialControl, TTimeControl,
    TValueControl, TToggleControl, TVolumeControl, TFilteredShellTreeView,
    TCollapseSplitter, TFloatSpinEditControl]);
end;

function AngleOfLine(x1, y1, x2, y2: Integer): Single;
var
  dx: Single;
  dy: Single;
begin
  dy := y2 - y1;
  dx := x2 - x1;


  if (dx = 0.0) then // Special case, vertical line
  begin
    if (dy > 0.0) then
      result := 0.0
    else
      result := 180.0;
  end
  else
  begin

    if dy = 0.0 then // Special case, horizontal line
    begin
      if dx > 0.0 then
        result := 90.0
      else
        result := 270.0;
    end
    else
    begin
      if dx > 0.0 then
        result := 90.0 - ArcTan(dy/dx) * (180 / M_PI)
      else if dx < 0.0 then
        result := 270.0 - ArcTan(dy/dx) * (180 / M_PI);
    end;
  end
end;

function LineFromAngle(XOrg, YOrg: Integer; AAngle: Single; ARadius: Single): TPoint;
var
  lPoint: TPoint;
  lRadians: Single;
begin
  lRadians := degtorad(AAngle);
  lPoint.X := XOrg - Round(ARadius * cos(lRadians));
  lPoint.Y := YOrg - round(ARadius * sin(lRadians));
  Result := lPoint;
end;

procedure TDialControl.CalcInternals(X, Y: Longint);
begin
  if FRange = 0 then
    FRange := 0.001;

  FInternalValue := FStartingInternalValue + (FOldY - FY);
  if FInternalValue < 0 then FInternalValue := 0;
  if FInternalValue > 300 then FInternalValue := 300;

  if FInternalValue <> FOldInternalValue then
  begin
    if Assigned(FOnChange) then
    begin
      FOnChange(Self);
    end;
    FOldInternalValue := FInternalValue;
  end;
end;

procedure TDialControl.SetLowest(const AValue: Single);
begin
  FLowest := AValue;
  FRange := FHighest - FLowest;

  if FRange = 0 then
    FRange := 0.001;

  Initialize;
end;

procedure TDialControl.SetHighest(const AValue: Single);
begin
  FHighest := AValue;
  FRange := FHighest - FLowest;

  if FRange = 0 then
    FRange := 0.001;

  Initialize;
end;

function TDialControl.GetValue: Single;
begin
  Result := FInternalValue * FScaleInternalValueToExternalValue;
end;

procedure TDialControl.SetValue(const AValue: Single);
begin
  if FRange = 0 then
    FRange := 0.001;

  FInternalValue := AValue * FScaleExternalValueToInternalValue;

  Repaint;
end;

procedure TDialControl.Initialize;
begin
  FScaleInternalValueToExternalValue := FRange / 300;
  FScaleExternalValueToInternalValue := 300 / FRange;
end;

constructor TDialControl.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csDisplayDragImage];

  ParentColor := True;

  Constraints.MinHeight := 60;
  Constraints.MaxHeight := 60;
  Constraints.MinWidth := 40;
  Constraints.MaxWidth := 40;
  Width := 40;
  Height := 60;
  Left := 0;
  Top := 0;
  FAngle := 180;
  FLowest := 0;
  FHighest := 1;
  Enabled := True;

  DoubleBuffered:= False;

  FDialMoving:= False;
end;

destructor TDialControl.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
end;

procedure TDialControl.EraseBackground(DC: HDC);
begin
  // Uncomment this to enable default background erasing
  inherited EraseBackground(DC);
end;

procedure TDialControl.Paint;
var
  Bitmap: TBitmap;
  lPos: Integer;
  lStr: string;
  lPoint: TPoint;
begin
  Bitmap := TBitmap.Create;
  try
    // Initializes the Bitmap Size
    Bitmap.Height := Height;
    Bitmap.Width := Width;

    Bitmap.Canvas.AntialiasingMode:= amOn;
    Bitmap.Canvas.Font.Size:= 7;

    // Draws the background
    Bitmap.Canvas.Brush.Color := Color;
    Bitmap.Canvas.FillRect(0, 0, Width, Height);

    if FEnabled then
      Bitmap.Canvas.Pen.Color := clBlue
    else
      Bitmap.Canvas.Pen.Color := clGray;

    Bitmap.Canvas.Pen.Width := 3;

    FAngle := FInternalValue;

    lPoint := LineFromAngle(Width div 2, Height div 2, FAngle - 60, 12);
    Bitmap.Canvas.Line(Width div 2, Height div 2, lPoint.X, lPoint.Y);

    Bitmap.Canvas.Arc(7, 17, 33, 43, -880, 4640);
    Bitmap.Canvas.Font.Color := clBlue;

    lPos := 20 - (Bitmap.Canvas.TextWidth(FCaption) div 2);
    Bitmap.Canvas.TextOut(lPos, 2, FCaption);

    lStr := IntToStr(Round(Value));
    lPos := 20 - (Bitmap.Canvas.TextWidth(lStr) div 2);
    Bitmap.Canvas.TextOut(lPos, 45, lStr);

    Canvas.Draw(0, 0, Bitmap);
  finally
    Bitmap.Free;
  end;

  inherited Paint;
end;

procedure TDialControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if FEnabled then
  begin

    if Assigned(FOnStartChange) then
      FOnStartChange(Self);

    FY := Y + Top;
    FOldY := FY;

    FStartingInternalValue := FInternalValue;

    CalcInternals(X, FY);

    Paint;

    FDialMoving := True;
  end;
end;

procedure TDialControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  FStartingInternalValue := FInternalValue;

  if Assigned(FOnEndChange) then
    FOnEndChange(Self);

  FDialMoving := False;
end;

procedure TDialControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  FY := Y + Top;

  if FDialMoving then
  begin
    CalcInternals(X, FY);

    Paint;
  end
end;

procedure TDialControl.UpdateControl;
begin
  Repaint;
end;

{ TTimeControl }

constructor TTimeControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csDisplayDragImage];

  Color := clBtnFace;
  Height:= 22;
  AutoSize:= True;

  FLabel:= TLabel.Create(AOwner);
  FLabel.Align:= alLeft;
  FLabel.Parent:= Self;
  FLabel.Top:= 3;

  FSpinEditBar:= TValueControl.Create(AOwner);
  FSpinEditBar.Align:= alLeft;
  FSpinEditBar.Parent:= Self;

  FSpinEditBeat:= TValueControl.Create(AOwner);
  FSpinEditBeat.Align:= alLeft;
  FSpinEditBeat.Parent:= Self;

  FSpinEditFrac:= TValueControl.Create(AOwner);
  FSpinEditFrac.Align:= alLeft;
  FSpinEditFrac.Parent:= Self;

  Height:= 12;
end;

destructor TTimeControl.Destroy;
begin
  FLabel.Free;
  FSpinEditBar.Free;
  FSpinEditBeat.Free;
  FSpinEditFrac.Free;

  inherited Destroy;
end;

procedure TTimeControl.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

procedure TTimeControl.Paint;
begin
  inherited Paint;
end;

procedure TTimeControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TTimeControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TTimeControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

{ TValueControl }

constructor TValueControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csDisplayDragImage];

  ParentColor := True;

  FDragging:= False;
end;

destructor TValueControl.Destroy;
begin
  inherited Destroy;
end;

procedure TValueControl.UpdateControl;
begin
  Repaint;
end;

procedure TValueControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FDragging:= True;
  FOldY:= Y;
  FOriginalValue:= Value;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TValueControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FDragging:= False;
  FOffset:= 0;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TValueControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FDragging then
  begin
    FOffset:= (FOldY - Y) div 10;
    Value:= FOriginalValue + FOffset;
    FLastValue:= FOffset;

    // Execute OnChange Handler if declared
    if Assigned(OnChange) then
    begin
      OnChange(Self);
    end;
  end;

  inherited MouseMove(Shift, X, Y);
end;


{ TToggleControl }

procedure TToggleControl.SetCaptionOff(const AValue: string);
begin
  FCaptionOff:= AValue;
  FCaption:= FCaptionOff;
end;

procedure TToggleControl.SetCaptionOn(const AValue: string);
begin
  FCaptionOn:= AValue;
end;

procedure TToggleControl.SetSwitchedOn(const AValue: Boolean);
begin
  FSwitchedOn := AValue;
  Repaint;
end;

constructor TToggleControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ParentColor := True;

  ControlStyle := ControlStyle + [csDisplayDragImage];

  Constraints.MinHeight:= 14;
  Constraints.MaxHeight:= 14;
  Width:= 40;

  FSwitchedOn:= False;
  FCaption:= 'Off';
end;

destructor TToggleControl.Destroy;
begin
  inherited Destroy;
end;

procedure TToggleControl.EraseBackground(DC: HDC);
begin
  //inherited EraseBackground(DC);
end;

procedure TToggleControl.Paint;
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font.Size:= 7;

    if FSwitchedOn then
      FCaption := FCaptionOn
    else
      FCaption := FCaptionOff;

    FCaptionWidth := Canvas.TextWidth(FCaption);

    // Initializes the Bitmap Size
    Bitmap.Height := Height;
    Bitmap.Width := Width;

    // Switched on/off state color
    if FSwitchedOn then
      Bitmap.Canvas.Brush.Color := clLime
    else
      Bitmap.Canvas.Brush.Color := Color;

    // Outline color
    Bitmap.Canvas.Pen.Style:= psSolid;
    Bitmap.Canvas.Pen.Color:= clBlack;

    Bitmap.Canvas.Rectangle(0, 0, Width, Height);
    Bitmap.Canvas.TextOut((Width shr 1) - (FCaptionWidth shr 1), 1, FCaption);

    Canvas.Draw(0, 0, Bitmap);
  finally
    Bitmap.Free;
  end;

  //inherited Paint;
end;

procedure TToggleControl.UpdateControl;
begin
  Repaint;
end;

procedure TToggleControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FLastSwitchedOn:= FSwitchedOn;

  if Assigned(FOnChange) then
    FOnChange(Self);

  Invalidate;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TToggleControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
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

function TVolumeControl.GetVolumeMultiplier: Single;
begin
  Result:= FPosition / 100;
end;

function TVolumeControl.GetLevelLeft: single;
begin
  Result := ChannelLevel[0];
end;

function TVolumeControl.GetLevelRight: single;
begin
  Result := ChannelLevel[1];
end;

procedure TVolumeControl.SetLevelLeft(const AValue: single);
begin
  ChannelLevel[0] := AValue;
end;

procedure TVolumeControl.SetLevelRight(const AValue: single);
begin
  ChannelLevel[1] := AValue;
end;

procedure TVolumeControl.SetPosition(const AValue: Single);
begin
  FPosition := AValue;
  Repaint;
end;

constructor TVolumeControl.Create(AOwner: TComponent);
var
  i: byte;
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csDisplayDragImage];

  ParentColor := True;

  Width:= 20;
  DoubleBuffered:= False;

  ChannelCount := 2;
  for i := 0 to 7 do
  begin
    ChannelLevel[i] := 0;
  end;

  FPosition:= 75;
  FRange := 100;
  FFaderMoving:= False;
end;

destructor TVolumeControl.Destroy;
begin

  inherited Destroy;
end;

procedure TVolumeControl.EraseBackground(DC: HDC);
begin
  // Uncomment this to enable default background erasing
  //inherited EraseBackground(DC);
end;

procedure TVolumeControl.Paint;
var
  Millimeter: Integer;
  MillimeterStep: Single;
  MillimeterNumber: Integer;
  HeightScale: Integer;
  i: byte;
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    // Initializes the Bitmap Size
    Bitmap.Height := Height;
    Bitmap.Width := Width;

    // Draws the background
    Bitmap.Canvas.Pen.Color := Parent.Color;
    Bitmap.Canvas.Rectangle(0, 0, Width, Height);

    // Draw Levels for all channels
    for i := 0 to ChannelCount - 1 do
    begin
      HeightScale:= Round(Height * ChannelLevel[i]);
      Bitmap.Canvas.Brush.Color:= clRed;
      Bitmap.Canvas.FillRect(i * 5, Height - HeightScale, i * 5 + 4, Height);

      Bitmap.Canvas.Brush.Color:= clLtGray;
      Bitmap.Canvas.FillRect(i * 5, 0, i * 5 + 4, Height - HeightScale);
    end;

    // Draw Scale
    Bitmap.Canvas.Pen.Color := clBlack;
    MillimeterNumber:= 20;
    MillimeterStep:= Height / MillimeterNumber;
    for Millimeter:= 0 to Pred(MillimeterNumber) do
    begin
      Bitmap.Canvas.Line(
        12, Round(Millimeter * MillimeterStep) + 2,
        15, Round(Millimeter * MillimeterStep) + 2);
    end;
    Bitmap.Canvas.Brush.Color:= clBlue;

    // Draw FaderHandle

    FY := Round(Bitmap.Height - (FPosition * (Bitmap.Height / FRange)));
    Bitmap.Canvas.FillRect(0, FY - 2, 10, FY + 2);
    Bitmap.Canvas.TextOut(0, 0, Format('%f', [FPosition]));
    Canvas.Draw(0, 0, Bitmap);
  finally
    Bitmap.Free;
  end;

  inherited Paint;
end;

procedure TVolumeControl.Update;
begin
  Repaint;
end;

procedure TVolumeControl.UpdateControl;
begin
  Repaint;
end;

procedure TVolumeControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Assigned(FOnStartChange) then
    FOnStartChange(Self);

  FFaderMoving := True;
end;

procedure TVolumeControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if Assigned(FOnEndChange) then
    FOnEndChange(Self);

  FFaderMoving := False;
end;

procedure TVolumeControl.MouseMove(Shift: TShiftState; X, Y: Integer);
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

    Paint;
  end
end;

{ TFilteredShellTreeView }

procedure TFilteredShellTreeView.SetMask(const AValue: string);
begin
  FMask := AValue;
  PopulateWithBaseFiles;
end;

{ Returns true if at least one item was added, false otherwise }
function TFilteredShellTreeView.PopulateTreeNodeWithFiles(
  ANode: TTreeNode; ANodePath: string): Boolean;
var
  i: Integer;
  Files: TStringList;
  NewNode: TTreeNode;
begin
  Files := TStringList.Create;
  try
    GetFilesInDir(ANodePath, AllFilesMask, ObjectTypes, Files);

    Result := Files.Count > 0;

    for i := 0 to Files.Count - 1 do
    begin
      NewNode := Items.AddChildObject(ANode, Files.Strings[i], nil); //@Files.Strings[i]);
      NewNode.HasChildren := Files.Objects[i] <> nil; // This marks if the node is a directory
    end;
  finally
    Files.Free;
  end;
end;


constructor TFilteredShellTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csDisplayDragImage];
end;

destructor TFilteredShellTreeView.Destroy;
begin
  inherited Destroy;
end;

{ TFloatSpinEditControl }

constructor TFloatSpinEditControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csDisplayDragImage];

  FDragging:= False;
end;

destructor TFloatSpinEditControl.Destroy;
begin
  inherited Destroy;
end;

procedure TFloatSpinEditControl.UpdateControl;
begin
  Repaint;
end;

procedure TFloatSpinEditControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging:= True;
  FOldY:= Y;
  FOriginalValue:= Value;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TFloatSpinEditControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging:= False;
  FOffset:= 0;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TFloatSpinEditControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FDragging then
  begin
    FOffset:= (FOldY - Y) / 20;
    Value:= FOriginalValue + FOffset;
    FLastValue:= FOffset;

    // Execute OnChange Handler if declared
    if Assigned(OnChange) then
    begin
      OnChange(Self);
    end;
  end;

  inherited MouseMove(Shift, X, Y);
end;

initialization
  //{$I hybrid.lrs}

end.

