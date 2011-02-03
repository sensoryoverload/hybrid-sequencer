unit peakmetercontrol;

{$mode objfpc}{$H+}

interface

(*uses
  Classes, SysUtils, Controls, Graphics, LCLType, Forms, ExtCtrls, jacktypes;

Type
  { TVolumeControl }

  TVolumeControl = class(TCustomControl)
  private
    FTimer: TTimer;
    FPosition: Single;
    FFaderMoving: Boolean;
    FPeakHold: Integer;
    FVolumeMultiplier: Single;
    
    // This points to a location where the present audio level is located
    FLevel: jack_default_audio_sample_t;
    function GetVolumeMultiplier: Single;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    property Position: Single read FPosition write FPosition;
    property VolumeMultiplier: Single read GetVolumeMultiplier;
    property Level: jack_default_audio_sample_t read FLevel write FLevel;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  end;

procedure Register;*)

implementation

(*uses utils;

procedure Register;
begin
  RegisterComponents('VolumeControl', [TVolumeControl]);
end;

function TVolumeControl.GetVolumeMultiplier: Single;
begin
  Result:= FPosition / 100;
end;

constructor TVolumeControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  Width:= 20;
  DoubleBuffered:= False;
  FLevel:= 0;
  FPosition:= 75;
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
  
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    FLevel :=

    // Initializes the Bitmap Size
    Bitmap.Height := Parent.Height;
    Bitmap.Width := Width;

    // Draws the background
    Bitmap.Canvas.Pen.Color := Parent.Color;
    Bitmap.Canvas.Rectangle(0, 0, Width, Height);

    HeightScale:= Round(Height * FLevel);
    Bitmap.Canvas.Brush.Color:= clRed;
    Bitmap.Canvas.FillRect(0, Height - HeightScale, 10, Height);

    Bitmap.Canvas.Brush.Color:= clLtGray;
    Bitmap.Canvas.FillRect(0, 0, 10, Height - HeightScale);

    Bitmap.Canvas.Pen.Color := clBlack;
    MillimeterNumber:= 20;
    MillimeterStep:= Height / MillimeterNumber;
    for Millimeter:= 0 to Pred(MillimeterNumber) do
    begin
      Bitmap.Canvas.Line(
        12, Round(Millimeter * MillimeterStep) + 2,
        15, Round(Millimeter * MillimeterStep) + 2);
    end;
    HeightScale:= Round((Height / 100) * FPosition);
    Bitmap.Canvas.Brush.Color:= clBlue;
    Bitmap.Canvas.FillRect(0, Height - HeightScale - 2, 10, Height - HeightScale + 2);
    
    Canvas.Draw(0, 0, Bitmap);
  finally
    Bitmap.Free;
  end;

  inherited Paint;
end;

procedure TVolumeControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  
  FFaderMoving := True;
end;

procedure TVolumeControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  
  FFaderMoving := False;
end;

procedure TVolumeControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  
  if FFaderMoving then
  begin
    FPosition:= Height - (Y * (100 / Height));
    if FPosition < 0 then
      FPosition:= 0;
    if FPosition > 100 then
      FPosition:= 100;
    Paint;
  end
end;
*)

end.

