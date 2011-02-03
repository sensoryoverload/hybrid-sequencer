unit dialcontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, Forms, ExtCtrls, Math, utils;

const
  M_PI = 3.14159265358979323846;


Type
  { TDialControl }

  TDialControl = class(TCustomControl)
  private
    FDialMoving: Boolean;
    FValue: Single;
    FLastValue: Single;
    FCaption: string;
    FAngle: Single;
    FLowest: Single;
    FHighest: Single;
    FOnChange: TNotifyEvent;

    procedure CalcInternals(X, Y: Longint);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  published
    property Value: Single read FValue write FValue;
    property Caption: string read FCaption write FCaption;
    property Lowest: Single read FLowest write FLowest;
    property Highest: Single read FHighest write FHighest;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('HybridComponentPack', [TDialControl]);
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
  FAngle := AngleOfLine(Width div 2, Height div 2, X, Y);
  if FAngle > 330 then FAngle := 330;
  if FAngle < 30 then FAngle := 30;

  // Normalize to 0..100
  FValue := ((360 - FAngle) - 30);

  // Scale to Lowest..Highest
  FValue := FValue * ((FHighest - FLowest) / 300);

  if FLastValue <> FValue then
  begin
    if Assigned(FOnChange) then
    begin
      FOnChange(Self);
    end;
  end;
  FLastValue := FValue;
end;

constructor TDialControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 40;
  Height := 60;
  Left := 0;
  Top := 0;
  FAngle := 180;

  DoubleBuffered:= False;
  FValue:= 0;
  FDialMoving:= False;
end;

destructor TDialControl.Destroy;
begin

  inherited Destroy;
end;

procedure TDialControl.EraseBackground(DC: HDC);
begin
  // Uncomment this to enable default background erasing
  //inherited EraseBackground(DC);
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

    // Draws the background
    Bitmap.Canvas.Brush.Color := Parent.Color;
    Bitmap.Canvas.FillRect(0, 0, Width, Height);

    Bitmap.Canvas.Pen.Color := clBlue;
    Bitmap.Canvas.Pen.Width := 2;

    lPoint := LineFromAngle(Width div 2, Height div 2, 270 - FAngle, 15);
    Bitmap.Canvas.Line(Width div 2, Height div 2, lPoint.X, lPoint.Y);

    Bitmap.Canvas.Arc(5, 15, 35, 45, -880, 4640);
    Bitmap.Canvas.Font.Color := clBlue;

    lPos := 20 - (Bitmap.Canvas.TextWidth(FCaption) div 2);
    Bitmap.Canvas.TextOut(lPos, 2, FCaption);

    lStr := IntToStr(Round(FValue));
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
  CalcInternals(X, Y);
  Paint;

  FDialMoving := True;
end;

procedure TDialControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  FDialMoving := False;
end;

procedure TDialControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  if FDialMoving then
  begin
    CalcInternals(X, Y);
    Paint;
  end
end;

{
ATan2(x1-x2,y1-y2)
}
end.

