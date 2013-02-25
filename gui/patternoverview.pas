unit patternoverview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, globalconst, global, LCLType, Controls, Graphics;

type
  TWaveZoomCallback = procedure(ALeftPercentage, ARightPercentage: Single) of object;

  { TPatternOverview }

  TPatternOverview = class(TPersistentCustomControl)
  private
    FTotalWidth: Integer;
    FZoomBoxWidth: Integer;
    FZoomBoxLeft: Integer;
    FZoomBoxOldLeft: Integer;
    FZoomBoxRight: Integer;
    FOldX: Integer;
    FOldY: Integer;
    FMouseX: Integer;

    FZooming: Boolean;
    FZoomingLeft: Boolean;
    FZoomingRight: Boolean;

    FZoomCallback: TWaveZoomCallback;
    FModel: THybridPersistentModel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update(Subject: THybridPersistentModel); override;
    procedure EraseBackground(DC: HDC); override;
    function GetModel: THybridPersistentModel; override;
    procedure SetModel(AModel: THybridPersistentModel); override;
    procedure Connect; override;
    procedure Disconnect; override;
    property ZoomCallback: TWaveZoomCallback write FZoomCallback;
    property Model: THybridPersistentModel read GetModel write SetModel;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  end;

implementation

{ TPatternOverview }

constructor TPatternOverview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FZoomBoxLeft := 0;
  FZoomBoxRight := Width;
  FZoomBoxWidth := Width;
end;

procedure TPatternOverview.Update(Subject: THybridPersistentModel);
begin
  Invalidate;
end;

procedure TPatternOverview.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

function TPatternOverview.GetModel: THybridPersistentModel;
begin
  Result := THybridPersistentModel(FModel);
end;

procedure TPatternOverview.SetModel(AModel: THybridPersistentModel);
begin
  FModel := AModel;
end;

procedure TPatternOverview.Connect;
begin
  FModel := GObjectMapper.GetModelObject(Self.ObjectID);
end;

procedure TPatternOverview.Disconnect;
begin
  inherited Disconnect;
end;

procedure TPatternOverview.Paint;
begin
  Canvas.Brush.Color := clLtGray;
  Canvas.Pen.Width := 1;
  Canvas.Rectangle(0, 0, Width, Height);

  Canvas.pen.Width := clBlack;
  Canvas.Pen.Width := 2;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(FZoomBoxLeft, 1, FZoomBoxRight, Height - 1);
end;

procedure TPatternOverview.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FOldX := X;
  FOldY := Y;

  if Button = mbLeft then
  begin
    if X < FZoomBoxLeft + 3 then
    begin
      FZoomingLeft := True;
    end
    else if X > FZoomBoxRight - 3 then
    begin
      FZoomingRight := True;
    end
    else
    begin
      FZooming := True;

      FZoomBoxOldLeft := FZoomBoxLeft;
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TPatternOverview.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FZooming := False;
  FZoomingLeft := False;
  FZoomingRight := False;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TPatternOverview.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  lTryLeftLocation: Integer;
  lTryRightLocation: Integer;
  lDelta: Integer;
begin
  if FZooming  then
  begin
    lDelta := X - FOldX;
    lTryLeftLocation := FZoomBoxOldLeft + lDelta;
    lTryRightLocation := FZoomBoxOldLeft + lDelta + FZoomBoxWidth;

    // Clamp to left side
    if lTryLeftLocation < 1 then
    begin
      FZoomBoxLeft := 1;
      FZoomBoxRight := FZoomBoxWidth;
    end
    // Clamp to right side
    else if lTryRightLocation >= Width then
    begin
      FZoomBoxLeft := Width - FZoomBoxWidth;
      FZoomBoxRight := Width;
    end
    // Freely move
    else
    begin
      FZoomBoxLeft := lTryLeftLocation;
      FZoomBoxRight := lTryRightLocation;
    end;
  end
  else if FZoomingLeft then
  begin
    FZoomBoxLeft := X;

    if FZoomBoxLeft >= (FZoomBoxRight - 5) then
      FZoomBoxLeft := FZoomBoxRight - 5;

    // Clamp to left side
    if FZoomBoxLeft < 1 then
      FZoomBoxLeft := 1;

    FZoomBoxWidth := FZoomBoxRight - FZoomBoxLeft;
  end
  else if FZoomingRight then
  begin
    FZoomBoxRight := X;

    if FZoomBoxRight <= (FZoomBoxLeft + 5) then
      FZoomBoxRight := FZoomBoxLeft + 5;

    // Clamp to right side
    if FZoomBoxRight > Width then
      FZoomBoxRight := Width;

    FZoomBoxWidth := FZoomBoxRight - FZoomBoxLeft;
  end;

  if FZooming or FZoomingLeft or FZoomingRight then
  begin
    if FZoomBoxRight > FZoomBoxLeft then
    begin
      if Assigned(FZoomCallback) then
      begin
        FZoomCallback((100 / Width) * FZoomBoxLeft, (100 / Width) * FZoomBoxRight);

        Invalidate;
      end;
    end;

  end;

  inherited MouseMove(Shift, X, Y);
end;

end.

