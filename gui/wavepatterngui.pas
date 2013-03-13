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

  wavepatterngui.pas
}

unit wavepatterngui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, wave, wavegui, graphics,
  track, LCLType, globalconst, global_command, global, pattern, ComCtrls,
  imglist, LCLintf, Menus, PatternGUI;

type

  { TWavePatternGUI }

  TWavePatternGUI = class(TPatternGUI)
  private
    FModel: TWavePattern;

    bmp: TBitmap;
    FCursorPosition: Integer;
    FOldCursorPosition: Integer;
    FCacheIsDirty: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce; override;
    procedure UpdateGUI;
    procedure RecalculateSynchronize;
    procedure ChangeZoomX(Sender: TObject);
    procedure Connect; override;
    procedure Disconnect; override;
    procedure Invalidate; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;

    function GetModel: THybridPersistentModel; override;
    procedure SetModel(AModel: THybridPersistentModel); override;

    property CursorPosition: Integer read FCursorPosition write FCursorPosition;
    property CacheIsDirty: Boolean read FCacheIsDirty write FCacheIsDirty;
//    property Model: TWavePattern read FWavePattern write FWavePattern;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
                    var Accept: Boolean); override;
    procedure MyEndDrag(Sender, Target: TObject; X, Y: Integer);
    function GetDragImages: TDragImageList; override;
  end;


implementation

uses
  utils;

{ TWavePatternGUI }

constructor TWavePatternGUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  bmp := TBitmap.Create;
  FCacheIsDirty := True;

  FCursorPosition := 0;
  Height := 15;
  RecalculateSynchronize;

  OnEndDrag := @MyEndDrag;

  {ChangeControlStyle(Self, [csDisplayDragImage], [], True); }
end;

destructor TWavePatternGUI.Destroy;
begin
  bmp.Free;

  inherited;
end;

procedure TWavePatternGUI.UpdateGUI;
begin
  Invalidate;
end;

procedure TWavePatternGUI.RecalculateSynchronize;
begin
  //FPatternLength := Round(WaveForm.SampleRate * (60 / FRealBPM)) * 4; // TODO choose next multiple of 4
end;

procedure TWavePatternGUI.ChangeZoomX(Sender: TObject);
begin
  // Zooming is GUI stuff and only needs to update itself
end;

procedure TWavePatternGUI.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

(*
procedure TMidiPatternGUI.Paint;
var
  lCursorPos: Integer;
begin
  //if FCacheIsDirty then
  begin
    bmp.Canvas.Clear;

    Top := Position;
    Width := Parent.Width;
    bmp.Height := Height;
    bmp.Width := Width;

    bmp.Canvas.Brush.Color := clLtGray;
    bmp.Canvas.Rectangle(0, 0, 15, Height);

    bmp.Canvas.Pen.Color := clGray;
    bmp.Canvas.Brush.Color := clGray;

    if FModel.Playing then
    begin
      bmp.Canvas.Rectangle(3, 3, 12, Height - 3);
    end
    else
    begin
      bmp.Canvas.Polygon([Point(3, 2), Point(12, Height div 2), Point(3, Height - 3)]);
    end;

    bmp.Canvas.Brush.Color := clLtGray;
    bmp.Canvas.Clipping := False;
    bmp.Canvas.Rectangle(15, 0, Width, Height);

    bmp.Canvas.TextOut(17, 2, Text);
    FCacheIsDirty := False;
  end;

  Canvas.Draw(0, 0, bmp);

  // Draw live pattern cursor
  lCursorPos := Round(Width * (FCursorPosition / PatternLength));
  if FOldCursorPosition <> lCursorPos then
  begin
    Canvas.Pen.Color := clRed;
    Canvas.Line(lCursorPos, 0, lCursorPos, Height);
    Canvas.Pen.Color := clLtGray;

    FOldCursorPosition := lCursorPos;
  end;

  inherited Paint;
end;
*)

procedure TWavePatternGUI.Paint;
var
  lCursorPos: Integer;
begin
  if FCacheIsDirty then
  begin
    bmp.Canvas.Clear;

    Top := Position;
    Width := Parent.Width;
    bmp.Height := Height;
    bmp.Width := Width;

    bmp.Canvas.Brush.Color := clLtGray;
    bmp.Canvas.Rectangle(0, 0, 15, Height);

    bmp.Canvas.Pen.Color := clGray;
    bmp.Canvas.Brush.Color := clGray;

    if FModel.Playing then
    begin
      bmp.Canvas.Rectangle(3, 3, 12, Height - 3);
    end
    else
    begin
      bmp.Canvas.Polygon([Point(3, 2), Point(12, Height div 2), Point(3, Height - 3)]);
    end;

    bmp.Canvas.Brush.Color := clLtGray;
    bmp.Canvas.Clipping := False;
    bmp.Canvas.Rectangle(15, 0, Width, Height);

    bmp.Canvas.TextOut(17, 2, Text);
    FCacheIsDirty := False;
  end;

  Canvas.Draw(0, 0, bmp);

  // Draw live pattern cursor
  lCursorPos := Round(Width * (FCursorPosition / PatternLength));
  if FOldCursorPosition <> lCursorPos then
  begin
    Canvas.Pen.Color := clRed;
    Canvas.Line(lCursorPos, 0, lCursorPos, Height);
    Canvas.Pen.Color := clLtGray;

    FOldCursorPosition := lCursorPos;
  end;

  inherited Paint;
end;

procedure TWavePatternGUI.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  lSchedulePattern: TSchedulePatternCommand;
begin
  if Button = mbLeft then
  begin
    Self.BeginDrag(False, 15);

    // TODO Playing pattern should be scheduler
    if (X - Self.Left) < 15 then
    begin
      Scheduled := True;
      lSchedulePattern := TSchedulePatternCommand.Create(ObjectID);
      try
        lSchedulePattern.ObjectIdList.Add(Self.ObjectID);
        lSchedulePattern.TrackID := Self.ObjectOwnerID;
        lSchedulePattern.Persist := False;
        GCommandQueue.PushCommand(lSchedulePattern);
      except
        on e:exception do
        begin
          DBLog('Internal error: ' + e.message);
          lSchedulePattern.Free;
        end;
      end;
    end
    else
    begin
      Selected := True;
    end;

    GSettings.SelectedObject := Self;

    FCacheIsDirty := True;

    //DoPatternRefreshGUI;
  end
  else if Button = mbRight then
  begin
    //
  end;


  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TWavePatternGUI.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FCacheIsDirty := True;

  inherited MouseUp(Button, Shift, X, Y);

  //DoPatternRefreshGUI;
end;

procedure TWavePatternGUI.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

procedure TWavePatternGUI.DragDrop(Source: TObject; X, Y: Integer);
var
  lTreeView: TTreeView;
  lDropWave: TPatternDropWaveCommand;
begin
  inherited DragDrop(Source, X, Y);

  if Source is TTreeView then
  begin
    lTreeView := TTreeView(Source);
    {
      If Source is wav file then create pattern
      else if Source is pattern then move pattern (or copy with Ctrl held)
    }
    lDropWave := TPatternDropWaveCommand.Create(ObjectID);
    try
      lDropWave.FileName := TTreeFolderData(lTreeView.Selected.Data).Path;
      Text := ExtractFileName(lDropWave.FileName);

      GCommandQueue.PushCommand(lDropWave);
    except
      lDropWave.Free;
    end;
  end;

  FCacheIsDirty := True;

  Invalidate;
end;

function TWavePatternGUI.GetModel: THybridPersistentModel;
begin
  Result := THybridPersistentModel(FModel);
end;

procedure TWavePatternGUI.SetModel(AModel: THybridPersistentModel);
begin
  FModel := TWavePattern(AModel);
end;


procedure TWavePatternGUI.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);

  Accept := True;
end;

// Pattern has moved location inside or outside of track
procedure TWavePatternGUI.MyEndDrag(Sender, Target: TObject; X, Y: Integer);
var
  P:Tpoint;
  C: TControl;
begin
  C := FindDragTarget(P, False);
  if Assigned(C) then
  begin
    GetCursorPos(P);
    C.DragDrop(Self, X, Y);
  end;
end;


function TWavePatternGUI.GetDragImages: TDragImageList;
begin
  Result := inherited GetDragImages;
  if Assigned(bmp) then
  begin
    if not Assigned(Result) then
      Result := TDragImageList.Create(nil);

    Result.Clear;
    Result.Height := Height;
    Result.Width := Width;
    Result.Add(bmp, nil);
    Result.SetDragImage(0, Width div 2, Height div 2);
  end;
end;

procedure TWavePatternGUI.Invalidate;
begin
  inherited Invalidate;
end;

procedure TWavePatternGUI.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TPatternGUI.Update');

  Position := FModel.Position;
  PatternLength := FModel.LoopEnd.Value;  //todo Use global patternlength
//  PatternControls.RealBPM := Model.WavePattern.RealBPM;
//  Text := ExtractFileName(TPattern(Subject).WavePattern.SampleFileName);

  DBLog('end TPatternGUI.Update');
end;

{ TCreateGUICommand }

procedure TWavePatternGUI.Connect;
begin
  DBLog('start TWavePatternGUI.Connect');


  DBLog('end TWavePatternGUI.Connect');
end;

procedure TWavePatternGUI.Disconnect;
begin
  DBLog('start TWavePatternGUI.Disconnect');


  DBLog('end TWavePatternGUI.Disconnect');
end;

initialization
  RegisterClass(TWavePatternGUI);

end.
