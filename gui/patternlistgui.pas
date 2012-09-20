unit patternlistgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Controls, ContNrs, LCLType, Graphics, track,
  globalconst;

type
  { TPatternListControl }

  TPatternListControl = class(TCustomControl)
  private
    FTrack: TTrack;



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
    property Align;
    property Anchors;
    property Constraints;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
  end;

implementation

{ TPatternListControl }

procedure TPatternListControl.SetCaptionOff(const AValue: string);
begin
  FCaptionOff:= AValue;
  FCaption:= FCaptionOff;
end;

procedure TPatternListControl.SetCaptionOn(const AValue: string);
begin
  FCaptionOn:= AValue;
end;

procedure TPatternListControl.SetSwitchedOn(const AValue: Boolean);
begin
  FSwitchedOn := AValue;
  Invalidate;
end;

constructor TPatternListControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ParentColor := True;

  ControlStyle := ControlStyle + [csDisplayDragImage];


end;

destructor TPatternListControl.Destroy;
begin
  inherited Destroy;
end;

procedure TPatternListControl.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

procedure TPatternListControl.Paint;
var
  Bitmap: TBitmap;
  PatternsInControl: Integer;
  PatternIndex: Integer;
begin
  Bitmap := TBitmap.Create;
  try
    for PatternIndex := 0 to Pred(PatternsInControl) do
    begin

    end;

    Bitmap.Canvas.Font.Size:= 7;

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

procedure TPatternListControl.UpdateControl;
begin
  Invalidate;
end;

procedure TPatternListControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FLastSwitchedOn:= FSwitchedOn;

  if Assigned(FOnChange) then
    FOnChange(Self);

  Invalidate;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TPatternListControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
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

end.

