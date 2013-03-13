unit patternoverview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, globalconst, global, LCLType, Controls, Graphics, contnrs;

type
  TWaveZoomCallback = procedure(ALeftPercentage, ARightPercentage: Single) of object;

  { TPatternOverview }

  TPatternOverview = class(TPersistentCustomControl)
  private
    FTotalWidth: Integer;
    FWidth: Integer;
    FZoomBoxWidth: Integer;
    FZoomBoxLeft: Integer;
    FZoomBoxOldLeft: Integer;
    FZoomBoxRight: Integer;
    FOldX: Integer;
    FOldY: Integer;

    FZooming: Boolean;
    FZoomingLeft: Boolean;
    FZoomingRight: Boolean;

    FZoomCallback: TWaveZoomCallback;
    FModel: THybridPersistentModel;
    procedure SetWidth(AValue: Integer);
  public
    procedure Update(Subject: THybridPersistentModel); override;
    procedure EraseBackground(DC: HDC); override;
    procedure Connect; override;
    procedure Disconnect; override;
    property ZoomCallback: TWaveZoomCallback write FZoomCallback;
    property Width: Integer read FWidth write SetWidth;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  end;

  { TMidiPatternOverview }

  TMidiPatternOverview = class(TPatternOverview)
  private
    FNoteListGUI: TObjectList;
    function ConvertNoteToScreen(ANote: Integer): Integer;
    function ConvertScreenToTime(AX: Integer): Integer;
    function ConvertTimeToScreen(ATime: Integer): Integer;
  protected
    procedure Paint; override;
  public
    procedure Update(Subject: THybridPersistentModel); override;
  end;

  TWaveFrame = record
    LowValue: Single;
    HighValue: Single;
  end;

  TWaveData = Array of TWaveFrame;

  { TWavePatternOverview }

  TWavePatternOverview = class(TPatternOverview)
  private
    FWaveImage: TWaveData;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); override;
    procedure Disconnect; override;
  end;

implementation

uses
  midi, wave;

{ TWavePatternOverview }

procedure TWavePatternOverview.Paint;
var
  lIndex: Integer;
  lChannelScale: Single;
begin
  lChannelScale := Height / 2;

  // Draw
  Canvas.Brush.Color := clLtGray;
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 1;
  Canvas.Rectangle(0, 0, Width, Height);

  Canvas.Brush.Color := clGray;
  Canvas.Pen.Color := clGray;
  Canvas.Pen.Width := 1;

  for lIndex := 0 to Pred(Width) do
  begin
    Canvas.Line(
      lIndex,
      Round(lChannelScale + FWaveImage[lIndex].LowValue * lChannelScale),
      lIndex,
      Round(lChannelScale + FWaveImage[lIndex].HighValue * lChannelScale));
  end;

  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clBlack;
  Canvas.Rectangle(0, 0, Width, Height);

  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 2;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(FZoomBoxLeft + 1, 1, FZoomBoxRight, Height);
end;

constructor TWavePatternOverview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetLength(FWaveImage, 0);
end;

destructor TWavePatternOverview.Destroy;
begin
  SetLength(FWaveImage, 0);

  inherited Destroy;
end;

procedure TWavePatternOverview.Update(Subject: THybridPersistentModel);
var
  lFrac: Integer;
  lLow: Single;
  lHigh: Single;
  lValue: Single;
  lIndex: Integer;
  lFracIndex: Integer;
  FWavePattern: TWavePattern;
begin
  FWavePattern := TWavePattern(Subject);

  SetLength(FWaveImage, Width);

  lFrac := FWavePattern.Wave.Frames div Width;
  for lIndex := 0 to Pred(FWavePattern.Wave.Frames div lFrac) do
  begin
    lLow := 1000;
    lHigh := -1000;
    for lFracIndex := 0 to Pred(lFrac) do
    begin
      lValue := TChannel(FWavePattern.Wave.ChannelList[0]).Buffer[lIndex * lFrac + lFracIndex];
      if lValue < lLow then
      begin
        lLow := lValue;
      end
      else if lValue > lHigh then
      begin
        lHigh := lValue;
      end;
    end;

    FWaveImage[lIndex].LowValue := lLow;
    FWaveImage[lIndex].HighValue := lHigh;
  end;

  Invalidate;
end;

procedure TWavePatternOverview.Disconnect;
begin
  inherited Disconnect;
end;

{ TMidiPatternOverview }

procedure TMidiPatternOverview.Update(Subject: THybridPersistentModel);
begin
  FTotalWidth :=
    TMidiPattern(Subject).LoopEnd.Value - TMidiPattern(Subject).LoopStart.Value;

  Invalidate;
end;

{
  Convert location in time to a screen cursor position
}
function TMidiPatternOverview.ConvertTimeToScreen(ATime: Integer): Integer;
begin
  Result := Round(ATime * (Width / FTotalWidth));
end;

{
  Convert location in time to a screen cursor position
}
function TMidiPatternOverview.ConvertScreenToTime(AX: Integer): Integer;
begin
  Result := Round(AX * (FTotalWidth / Width));
end;

{
  Convert a note to a screen note position
}
function TMidiPatternOverview.ConvertNoteToScreen(ANote: Integer): Integer;
var
  lScale: single;
begin
  lScale := (128 / Height);

  Result := Round(Height - Round(ANote / lScale));
end;

procedure TMidiPatternOverview.Paint;
var
  lIndex: Integer;
  lNote: TMidiNote;
begin
  Canvas.Brush.Color := clLtGray;
  Canvas.Pen.Width := 1;
  Canvas.Rectangle(0, 0, Width, Height);

  // Draw midi notes
  Canvas.Pen.Color := clGray;
  Canvas.Pen.Width := 1;
  for lIndex := 0 to Pred(TMidiPattern(FModel).NoteList.Count) do
  begin
    lNote := TMidiNote(TMidiPattern(FModel).NoteList[lIndex]);

    Canvas.Line(
      ConvertTimeToScreen(lNote.NoteLocation),
      ConvertNoteToScreen(lNote.Note),
      ConvertTimeToScreen(lNote.NoteLocation + lNote.NoteLength),
      ConvertNoteToScreen(lNote.Note)
      );
  end;

  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 2;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(FZoomBoxLeft + 1, 1, FZoomBoxRight, Height);
end;

{ TPatternOverview }

procedure TPatternOverview.SetWidth(AValue: Integer);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;

  FZoomBoxLeft := 0;
  FZoomBoxRight := Width;
  FZoomBoxWidth := FZoomBoxRight - FZoomBoxLeft;

  inherited Width := AValue;
end;

procedure TPatternOverview.Update(Subject: THybridPersistentModel);
begin
  Invalidate;
end;

procedure TPatternOverview.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
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
  Canvas.Rectangle(FZoomBoxLeft + 1, 1, FZoomBoxRight, Height);
end;

procedure TPatternOverview.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FOldX := X;
  FOldY := Y;

  if Button = mbLeft then
  begin
    if X < FZoomBoxLeft + 5 then
    begin
      FZoomingLeft := True;
    end
    else if X > FZoomBoxRight - 5 then
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
    if lTryLeftLocation < 0 then
    begin
      FZoomBoxLeft := 0;
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
    if FZoomBoxLeft < 0 then
      FZoomBoxLeft := 0;

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

