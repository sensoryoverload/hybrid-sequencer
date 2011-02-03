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

  plugingui.pas
}

unit plugingui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Controls, globalconst, utils, Graphics, plugin;

type
    { TPluginNodeGUI }

  {
    Visible part of an audio/midi -plugin/instrument.
    Contains the name of the plugin and the inter-plugin connections.
    Clicking the object sets the plugin parameters visible in a side panel.
  }

  TPluginNodeGUI = class(TPersistentCustomControl)
  private
    FPluginName: string;
    FCaptionWidth: Integer;
    FOnChange: TNotifyEvent;
    FSelected: Boolean;
    FOnSelect: TNotifyEvent;
    FOnConnection: TInterConnectCallback;
    FXLocation: Integer;
    FYLocation: Integer;
    FMouseX: Integer;
    FMouseY: Integer;
    FOldMouseX: Integer;
    FOldMouseY: Integer;
    FLeftMouseDown: Boolean;
    function GetXLocation: Integer;
    function GetYLocation: Integer;
    procedure SetPluginName(const AValue: string);
    procedure SetXLocation(const AValue: Integer);
    procedure SetYLocation(const AValue: Integer);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure UpdateControl;
    procedure Update(Subject: THybridPersistentModel); reintroduce; override;
    property XLocation: Integer read GetXLocation write SetXLocation;
    property YLocation: Integer read GetYLocation write SetYLocation;
    property OnConnection: TInterConnectCallback read FOnConnection write FOnConnection;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  published
    property Selected: Boolean read FSelected write FSelected;
    property PluginName: string read FPluginName write SetPluginName;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  end;

implementation

{ TSampleSelectControl }

procedure TPluginNodeGUI.SetPluginName(const AValue: string);
begin
  FPluginName := AValue;
  Width := (Length(FPluginName) * 7);
end;

function TPluginNodeGUI.GetXLocation: Integer;
begin
  Result := Left;
end;

function TPluginNodeGUI.GetYLocation: Integer;
begin
  Result := Top;
end;

procedure TPluginNodeGUI.SetXLocation(const AValue: Integer);
begin
  FXLocation := AValue;
  Left := FXLocation - (Width div 2);
end;

procedure TPluginNodeGUI.SetYLocation(const AValue: Integer);
begin
  FYLocation := AValue;
  Top := FYLocation - (Height div 2);;
end;

constructor TPluginNodeGUI.Create(AOwner: TComponent);
begin
  DBLog('start TPluginNodeGUI.Create');

  inherited Create(AOwner);

  FLeftMouseDown := False;
  FMouseX := 0;
  FOldMouseX := 0;
  FMouseY := 0;
  FOldMouseY := 0;

  ControlStyle := ControlStyle + [csDisplayDragImage];

  {Constraints.MinWidth := 50;
  Constraints.MaxWidth := 50;
  Constraints.MinHeight := 30;
  Constraints.MaxHeight := 30;}
  Width := 50;
  Height := 30;

  DBLog('end TPluginNodeGUI.Create');
end;

destructor TPluginNodeGUI.Destroy;
begin
  inherited Destroy;
end;

procedure TPluginNodeGUI.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

procedure TPluginNodeGUI.Paint;
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font.Size:= 7;

    // Initializes the Bitmap Size
    Bitmap.Height := Height;
    Bitmap.Width := Width;

    // Outline color
    Bitmap.Canvas.Pen.Style:= psSolid;
    Bitmap.Canvas.Pen.Color:= clBlack;

    Bitmap.Canvas.Brush.Color := clLtGray;
    Bitmap.Canvas.Rectangle(0, 0, Width, Height);

    Bitmap.Canvas.Brush.Color := clBlue;

    FCaptionWidth := Canvas.TextWidth(FPluginName) + 4;

    Bitmap.Canvas.TextOut((Width shr 1) - (FCaptionWidth shr 1), 1, FPluginName);

    Canvas.Draw(0, 0, Bitmap);
  finally
    Bitmap.Free;
  end;

  inherited Paint;
end;

procedure TPluginNodeGUI.UpdateControl;
begin
  Repaint;
end;

procedure TPluginNodeGUI.Update(Subject: THybridPersistentModel);
begin
  Caption := TPluginNode(Subject).PluginName;
  {FPluginName := TPluginNode(Subject).PluginName;
  FXLocation := TPluginNode(Subject).XLocation;
  FYLocation := TPluginNode(Subject).YLocation;}
end;

procedure TPluginNodeGUI.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  case Button of
    mbLeft:
    begin
      FLeftMouseDown := True;
      FOldMouseX := X;
      FOldMouseY := Y;

      if Assigned(FOnSelect) then
        FOnSelect(Self);
    end;
    mbRight:
    begin
      // Start Connect
      if Assigned(FOnConnection) then
      begin
        FOnConnection(ObjectID, 'PORT1')
      end;
    end;
  end;
end;

procedure TPluginNodeGUI.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  FLeftMouseDown := False;

  FMouseX := FOldMouseX;
  FMouseY := FOldMouseY;

  Repaint;
end;

procedure TPluginNodeGUI.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  if FLeftMouseDown then
  begin
    FMouseX := X;
    FMouseY := Y;

    Left := Left + (FMouseX - FOldMouseX);
    Top := Top + (FMouseY - FOldMouseY);
  end;
end;


end.

