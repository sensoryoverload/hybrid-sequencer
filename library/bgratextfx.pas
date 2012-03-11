unit BGRATextFX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmapTypes, Types;

type

  { TBGRATextEffect }

  TBGRATextEffect = class
  private
    function GetHeight: integer;
    function GetWidth: integer;
  protected
    FTextMask: TBGRACustomBitmap;
    FShadowRadius: integer;
    FOutlineMask, FShadowMask : TBGRACustomBitmap;
    FWidth,FHeight: integer;
    FOffset: TPoint;
    procedure DrawMaskMulticolored(ADest: TBGRACustomBitmap; AMask: TBGRACustomBitmap; X,Y: Integer; const AColors: array of TBGRAPixel);
    procedure DrawMask(ADest: TBGRACustomBitmap; AMask: TBGRACustomBitmap; X,Y: Integer; AColor: TBGRAPixel);
    procedure DrawMask(ADest: TBGRACustomBitmap; AMask: TBGRACustomBitmap; X,Y: Integer; ATexture: IBGRAScanner);
  public
    constructor Create(AText: string; Font: TFont; Antialiasing: boolean);
    procedure ApplySphere;
    procedure ApplyVerticalCylinder;
    procedure ApplyHorizontalCylinder;
    procedure Draw(ADest: TBGRACustomBitmap; X,Y: integer; AColor: TBGRAPixel);
    procedure Draw(ADest: TBGRACustomBitmap; X,Y: integer; ATexture: IBGRAScanner);
    procedure Draw(ADest: TBGRACustomBitmap; X, Y: integer; AColor: TBGRAPixel; AAlign: TAlignment);
    procedure Draw(ADest: TBGRACustomBitmap; X, Y: integer; ATexture: IBGRAScanner; AAlign: TAlignment);
    procedure DrawMulticolored(ADest: TBGRACustomBitmap; X,Y: integer; const AColors: array of TBGRAPixel);
    procedure DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; AColor: TBGRAPixel);
    procedure DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; ATexture: IBGRAScanner);
    procedure DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; AColor: TBGRAPixel; AAlign: TAlignment);
    procedure DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; ATexture: IBGRAScanner; AAlign: TAlignment);
    procedure DrawShadow(ADest: TBGRACustomBitmap; X,Y,Radius: integer; AColor: TBGRAPixel);
    procedure DrawShadow(ADest: TBGRACustomBitmap; X,Y,Radius: integer; AColor: TBGRAPixel; AAlign: TAlignment);
    destructor Destroy; override;
    property TextMask: TBGRACustomBitmap read FTextMask;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
  end;

function TextShadow(AWidth,AHeight: Integer; AText: String; AFontHeight: Integer; ATextColor,AShadowColor: TBGRAPixel;
    AOffSetX,AOffSetY: Integer; ARadius: Integer = 0; AFontStyle: TFontStyles = []; AFontName: String = 'Default'; AShowText: Boolean = True): TBGRACustomBitmap;

implementation

uses BGRAGradientScanner, BGRAText, GraphType;

procedure BGRAReplace(var Destination: TBGRACustomBitmap; Temp: TObject);
begin
  Destination.Free;
  Destination := Temp as TBGRACustomBitmap;
end;

function TextShadow(AWidth,AHeight: Integer; AText: String; AFontHeight: Integer; ATextColor,AShadowColor: TBGRAPixel;
  AOffSetX,AOffSetY: Integer; ARadius: Integer = 0; AFontStyle: TFontStyles = []; AFontName: String = 'Default'; AShowText: Boolean = True): TBGRACustomBitmap;
var
  bmpOut,bmpSdw: TBGRACustomBitmap; OutTxtSize: TSize; OutX,OutY: Integer;
begin
  bmpOut:= BGRABitmapFactory.Create(AWidth,AHeight);
  bmpOut.FontAntialias:= True;
  bmpOut.FontHeight:= AFontHeight;
  bmpOut.FontStyle:= AFontStyle;
  bmpOut.FontName:= AFontName;

  OutTxtSize:= bmpOut.TextSize(AText);
  OutX:= Round(AWidth/2) - Round(OutTxtSize.cx/2);
  OutY:= Round(AHeight/2) - Round(OutTxtSize.cy/2);

  bmpSdw:= BGRABitmapFactory.Create(OutTxtSize.cx+2*ARadius,OutTxtSize.cy+2*ARadius);
  bmpSdw.FontAntialias:= True;
  bmpSdw.FontHeight:= AFontHeight;
  bmpSdw.FontStyle:= AFontStyle;
  bmpSdw.FontName:= AFontName;

  bmpSdw.TextOut(ARadius,ARadius,AText,AShadowColor);
  BGRAReplace(bmpSdw,bmpSdw.FilterBlurRadial(ARadius,rbFast));
  bmpOut.PutImage(OutX+AOffSetX-ARadius,OutY+AOffSetY-ARadius,bmpSdw,dmDrawWithTransparency);
  bmpSdw.Free;

  if AShowText = True then bmpOut.TextOut(OutX,OutY,AText,ATextColor);

  Result:= bmpOut;
end;

{ TBGRATextEffect }

function TBGRATextEffect.GetHeight: integer;
begin
  result := FHeight;
end;

function TBGRATextEffect.GetWidth: integer;
begin
  result := FWidth;
end;

procedure TBGRATextEffect.DrawMaskMulticolored(ADest: TBGRACustomBitmap;
  AMask: TBGRACustomBitmap; X, Y: Integer; const AColors: array of TBGRAPixel);
var
  scan: TBGRASolidColorMaskScanner;
  xb,yb,startX,numColor: integer;
  p0,p: PBGRAPixel;
  emptyCol: boolean;
begin
  if (AMask = nil) or (length(AColors)=0) then exit;
  if (length(AColors)=0) then
  begin
    DrawMask(ADest,AMask,X,Y,AColors[0]);
    exit;
  end;
  scan := TBGRASolidColorMaskScanner.Create(AMask,Point(-X,-Y),AColors[0]);
  numColor := 0;
  startX := -1;
  p0 := AMask.data;
  for xb := 0 to AMask.Width-1 do
  begin
    p := p0;
    emptyCol := true;
    for yb := AMask.Height-1 downto 0 do
    begin
      if (p^<>BGRABlack) then
      begin
        emptyCol := false;
        break;
      end;
      inc(p, AMask.Width);
    end;
    if not emptyCol then
    begin
      if startX=-1 then
        startX := xb;
    end else
    begin
      if startX<>-1 then
      begin
        ADest.FillRect(X+startX,Y,X+xb,Y+AMask.Height,scan,dmDrawWithTransparency);
        inc(numColor);
        if numColor = length(AColors) then
          numColor := 0;
        scan.Color := AColors[numColor];
        startX := -1;
      end;
    end;
    inc(p0);
  end;
  if startX<>-1 then
    ADest.FillRect(X+startX,Y,X+AMask.Width,Y+AMask.Height,scan,dmDrawWithTransparency);
  scan.Free;
end;

procedure TBGRATextEffect.DrawMask(ADest: TBGRACustomBitmap; AMask: TBGRACustomBitmap; X,
  Y: Integer; AColor: TBGRAPixel);
var
  scan: TBGRACustomScanner;
begin
  if AMask = nil then exit;
  scan := TBGRASolidColorMaskScanner.Create(AMask,Point(-X,-Y),AColor);
  ADest.FillRect(X,Y,X+AMask.Width,Y+AMask.Height,scan,dmDrawWithTransparency);
  scan.Free;
end;

procedure TBGRATextEffect.DrawMask(ADest: TBGRACustomBitmap; AMask: TBGRACustomBitmap; X,
  Y: Integer; ATexture: IBGRAScanner);
var
  scan: TBGRACustomScanner;
begin
  if AMask = nil then exit;
  scan := TBGRATextureMaskScanner.Create(AMask,Point(-X,-Y),ATexture);
  ADest.FillRect(X,Y,X+AMask.Width,Y+AMask.Height,scan,dmDrawWithTransparency);
  scan.Free;
end;

procedure TBGRATextEffect.Draw(ADest: TBGRACustomBitmap; X, Y: integer;
  AColor: TBGRAPixel; AAlign: TAlignment);
begin
  Case AAlign of
  taLeftJustify: Draw(ADest,X,Y,AColor);
  taRightJustify: Draw(ADest,X-Width,Y,AColor);
  taCenter: Draw(ADest,X-Width div 2,Y,AColor);
  end;
end;

procedure TBGRATextEffect.Draw(ADest: TBGRACustomBitmap; X, Y: integer;
  ATexture: IBGRAScanner; AAlign: TAlignment);
begin
  Case AAlign of
  taLeftJustify: Draw(ADest,X,Y,ATexture);
  taRightJustify: Draw(ADest,X-Width,Y,ATexture);
  taCenter: Draw(ADest,X-Width div 2,Y,ATexture);
  end;
end;

constructor TBGRATextEffect.Create(AText: string; Font: TFont;
  Antialiasing: boolean);
var temp: TBGRACustomBitmap;
    size: TSize;
    p: PBGRAPixel;
    n: integer;
    alpha: byte;
    sizeX: integer;
    onePixel: integer;
begin
  size := BGRAOriginalTextSize(Font,Antialiasing,AText);
  if (size.cx = 0) or (size.cy = 0) then
  begin
    size := BGRATextSize(Font,Antialiasing,'Hg');
    FWidth := 0;
    FHeight := size.cy;
    FOffset := Point(0,0);
    exit;
  end;

  sizeX := size.cx+size.cy;
  if Antialiasing then
  begin
    sizeX := (sizeX + FontAntialiasingLevel-1);
    sizeX -= sizeX mod FontAntialiasingLevel;
    OnePixel := FontAntialiasingLevel;
  end else
    OnePixel := 1;
  FOffset := Point(-size.cy div 2,-OnePixel); //include overhang

  temp := BGRABitmapFactory.Create(sizeX, size.cy+2*OnePixel,clBlack);
  temp.Canvas.Font := Font;
  temp.Canvas.Font.Height := Font.Height*OnePixel;
  temp.Canvas.Font.Color := clWhite;
  temp.Canvas.Brush.Style := bsClear;
  temp.Canvas.TextOut(-FOffset.X, -FOffset.Y, AText);

  if Antialiasing then
  begin
    FWidth := round(size.cx/FontAntialiasingLevel);
    FHeight := round(size.cy/FontAntialiasingLevel);
    FOffset := Point(round(FOffset.X/FontAntialiasingLevel),round(FOffset.Y/FontAntialiasingLevel));

    FTextMask := temp.Resample(round(temp.width/FontAntialiasingLevel),round(temp.Height/FontAntialiasingLevel),rmSimpleStretch);
    BGRAReplace(FTextMask,FTextMask.FilterNormalize(False));
    temp.Free;
  end
  else
  begin
    FWidth := size.cx;
    FHeight := size.cy;

    FTextMask := temp;
    p := FTextMask.data;
    for n := FTextMask.NbPixels-1 downto 0 do
    begin
      alpha := GammaExpansionTab[P^.green] shr 8;
      p^.green := alpha;
      p^.red := alpha;
      p^.blue := alpha;
    end;
  end;
end;

procedure TBGRATextEffect.ApplySphere;
var sphere: TBGRACustomBitmap;
begin
  if FTextMask = nil then exit;
  FreeAndNil(FOutlineMask);
  FreeAndNil(FShadowMask);
  FShadowRadius := 0;
  sphere := FTextMask.FilterSphere;
  FTextMask.Fill(BGRABlack);
  FTextMask.PutImage(0,0,sphere,dmDrawWithTransparency);
  sphere.Free;
end;

procedure TBGRATextEffect.ApplyVerticalCylinder;
begin
  if FTextMask = nil then exit;
  FreeAndNil(FOutlineMask);
  FreeAndNil(FShadowMask);
  FShadowRadius := 0;
  BGRAReplace(FTextMask,FTextMask.FilterCylinder);
end;

procedure TBGRATextEffect.ApplyHorizontalCylinder;
begin
  if FTextMask = nil then exit;
  FreeAndNil(FOutlineMask);
  FreeAndNil(FShadowMask);
  FShadowRadius := 0;
  BGRAReplace(FTextMask,FTextMask.RotateCW);
  BGRAReplace(FTextMask,FTextMask.FilterCylinder);
  BGRAReplace(FTextMask,FTextMask.RotateCCW);
end;

procedure TBGRATextEffect.Draw(ADest: TBGRACustomBitmap; X, Y: integer;
  AColor: TBGRAPixel);
begin
  if FTextMask = nil then exit;
  DrawMask(ADest,FTextMask,X+FOffset.X,Y+FOffset.Y,AColor);
end;

procedure TBGRATextEffect.Draw(ADest: TBGRACustomBitmap; X, Y: integer;
  ATexture: IBGRAScanner);
begin
  if FTextMask = nil then exit;
  DrawMask(ADest,FTextMask,X+FOffset.X,Y+FOffset.Y,ATexture);
end;

procedure TBGRATextEffect.DrawMulticolored(ADest: TBGRACustomBitmap; X, Y: integer;
  const AColors: array of TBGRAPixel);
begin
  if FTextMask = nil then exit;
  DrawMaskMulticolored(ADest,FTextMask,X+FOffset.X,Y+FOffset.Y,AColors);
end;

procedure TBGRATextEffect.DrawOutline(ADest: TBGRACustomBitmap; X, Y: integer;
  AColor: TBGRAPixel);
begin
  if FTextMask = nil then exit;
  if FOutlineMask = nil then
  begin
    FOutlineMask := FTextMask.FilterContour;
    FOutlineMask.LinearNegative;
  end;
  DrawMask(ADest,FOutlineMask,X+FOffset.X,Y+FOffset.Y,AColor);
end;

procedure TBGRATextEffect.DrawOutline(ADest: TBGRACustomBitmap; X, Y: integer;
  ATexture: IBGRAScanner);
begin
  if FTextMask = nil then exit;
  if FOutlineMask = nil then
  begin
    FOutlineMask := FTextMask.FilterContour;
    FOutlineMask.LinearNegative;
  end;
  DrawMask(ADest,FOutlineMask,X+FOffset.X,Y+FOffset.Y,ATexture);
end;

procedure TBGRATextEffect.DrawOutline(ADest: TBGRACustomBitmap; X, Y: integer;
  AColor: TBGRAPixel; AAlign: TAlignment);
begin
  Case AAlign of
  taLeftJustify: DrawOutline(ADest,X,Y,AColor);
  taRightJustify: DrawOutline(ADest,X-Width,Y,AColor);
  taCenter: DrawOutline(ADest,X-Width div 2,Y,AColor);
  end;
end;

procedure TBGRATextEffect.DrawOutline(ADest: TBGRACustomBitmap; X, Y: integer;
  ATexture: IBGRAScanner; AAlign: TAlignment);
begin
  Case AAlign of
  taLeftJustify: DrawOutline(ADest,X,Y,ATexture);
  taRightJustify: DrawOutline(ADest,X-Width,Y,ATexture);
  taCenter: DrawOutline(ADest,X-Width div 2,Y,ATexture);
  end;
end;

procedure TBGRATextEffect.DrawShadow(ADest: TBGRACustomBitmap; X, Y,Radius: integer;
  AColor: TBGRAPixel);
begin
  if Radius = 0 then
  begin
    Draw(ADest,X,Y,AColor);
    exit;
  end;
  if FTextMask = nil then exit;
  if FShadowRadius <> Radius then
  begin
    FShadowRadius := Radius;
    FreeAndNil(FShadowMask);
    FShadowMask := BGRABitmapFactory.Create(FTextMask.Width+Radius*2,FTextMask.Height+Radius*2,BGRABlack);
    FShadowMask.PutImage(Radius,Radius,FTextMask,dmSet);
    BGRAReplace(FShadowMask, FShadowMask.FilterBlurRadial(Radius,rbFast));
  end;
  DrawMask(ADest,FShadowMask,X-Radius+FOffset.X,Y-Radius+FOffset.Y,AColor)
end;

procedure TBGRATextEffect.DrawShadow(ADest: TBGRACustomBitmap; X, Y,
  Radius: integer; AColor: TBGRAPixel; AAlign: TAlignment);
begin
  Case AAlign of
  taLeftJustify: DrawShadow(ADest,X,Y,Radius,AColor);
  taRightJustify: DrawShadow(ADest,X-Width,Y,Radius,AColor);
  taCenter: DrawShadow(ADest,X-Width div 2,Y,Radius,AColor);
  end;
end;

destructor TBGRATextEffect.Destroy;
begin
  FShadowMask.free;
  textMask.Free;
  FOutlineMask.Free;
  inherited Destroy;
end;

end.

