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

  ptterngui.pas
}

unit patterngui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, waveform, waveformgui, midi, midigui, graphics,
  track, LCLType, globalconst, global_command, global, pattern, ComCtrls, patterngui2,
  imglist, LCLintf;

type

  { TPatternGUI }

  TPatternGUI = class(TPersistentCustomControl)
  private
    FModel: TPattern;

    FPatternControls: TPatternControls;
    FPatternColor: TColor;
    FPitched: Boolean;
    FPosition: Integer; // Vertical position in the patterngrid
    FText: string;
    FSyncQuantize: Boolean;
    FOkToPlay: Boolean;
    FPitch: Single;
    FRealBPM: Single;
    FRootNote: Integer;
    FMidiChannel: Integer;
    FPlaying: Boolean;
    FScheduled: Boolean;
    FSelected: Boolean;
    FPatternLength: Longint;
    FOnTracksRefreshGUI: TTracksRefreshGUIEvent;
    bmp: TBitmap;
    FCursorPosition: Integer;
    FOldCursorPosition: Integer;
    FCacheIsDirty: Boolean;

    procedure SetPatternColor(const AValue: TColor);
    procedure SetPosition(const AValue: Integer);
    procedure SetText(const AValue: string);
    procedure Setpitch(const Avalue: Single);

  protected
    procedure DoTrackRefreshGUI;
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
    property Model: TPattern read FModel write FModel;
    property SyncQuantize: Boolean read FSyncQuantize write FSyncQuantize;
    property Position: Integer read FPosition write SetPosition;
    property PatternColor: TColor read FPatternColor write SetPatternColor;
    property Text: string read FText write SetText;
    property OkToPlay: Boolean read FOkToPlay write FOkToPlay;
    property Pitch: Single read FPitch write SetPitch default 1;
    property Pitched: Boolean read FPitched write FPitched default False;
    property RealBPM: Single read FRealBPM write FRealBPM default 120;
    property RootNote: Integer read FRootNote write FRootNote default 0;
    property MidiChannel: Integer read FMidiChannel write FMidiChannel;
    property Playing: Boolean read FPlaying write FPlaying default False;
    property Scheduled: Boolean read FScheduled write FScheduled default False;
    property Selected: Boolean read FSelected write FSelected;
    property PatternLength: Longint read FPatternLength write FPatternLength;
    property OnTracksRefreshGUI: TTracksRefreshGUIEvent read FOnTracksRefreshGUI write FOnTracksRefreshGUI;
    property PatternControls: TPatternControls read FPatternControls write FPatternControls;
    property CursorPosition: Integer read FCursorPosition write FCursorPosition;
    property CacheIsDirty: Boolean read FCacheIsDirty write FCacheIsDirty;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
                    var Accept: Boolean); override;
    procedure DoEndDrag(Sender, Target: TObject; X, Y: Integer);
    function GetDragImages: TDragImageList; override;
  end;


implementation

uses
  utils;

{ TPatternGUI }

procedure TPatternGUI.SetPatternColor(const AValue: TColor);
begin
  FPatternColor := AValue;
  Repaint;
end;

procedure TPatternGUI.SetPosition(const AValue: Integer);
begin
  FPosition := AValue;
end;

procedure TPatternGUI.SetText(const AValue: string);
begin
  FText := AValue;
end;

procedure TPatternGUI.Setpitch(const Avalue: Single);
begin
  if Avalue > 8 then
    FPitch := 8
  else if Avalue < 0.1 then
    FPitch := 0.1
  else
    FPitch := Avalue;
end;

procedure TPatternGUI.DoTrackRefreshGUI;
begin
  if Assigned(FOnTracksRefreshGUI) then
    FOnTracksRefreshGUI(Self);
end;

constructor TPatternGUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  bmp := TBitmap.Create;
  FPatternControls := TPatternControls.Create(Self);
  FCacheIsDirty := True;

  FCursorPosition := 0;
  Height := 15;
  FRealBPM := 120;
  RecalculateSynchronize;

  FOkToPlay := False;

  OnEndDrag := @DoEndDrag;

  ChangeControlStyle(Self, [csDisplayDragImage], [], True);
end;

destructor TPatternGUI.Destroy;
begin
  bmp.Free;
  FPatternControls.Free;

  inherited;
end;

procedure TPatternGUI.UpdateGUI;
begin
  Repaint;
end;

procedure TPatternGUI.RecalculateSynchronize;
begin
  //FPatternLength := Round(WaveForm.SampleRate * (60 / FRealBPM)) * 4; // TODO choose next multiple of 4
end;

procedure TPatternGUI.ChangeZoomX(Sender: TObject);
begin
  // Zooming is GUI stuff and only needs to update itself

    {GAudioStruct.SelectedTrack.SelectedPattern.WaveForm.Repaint;}
end;

procedure TPatternGUI.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

procedure TPatternGUI.Paint;
var
  lCursorPos: Integer;
begin
  if FCacheIsDirty then
  begin
    bmp.Canvas.Clear;

    Top := FPosition;
    Width := Parent.Width;
    bmp.Height := Height;
    bmp.Width := Width;

    if FPlaying then
    begin
      bmp.Canvas.Brush.Color := clGreen
    end
    else
    begin
      if Scheduled then
        bmp.Canvas.Brush.Color := clYellow
      else
        bmp.Canvas.Brush.Color := clBlue
    end;

    bmp.Canvas.Clipping := False;
    bmp.Canvas.Rectangle(0, 0, 15, Height);

    bmp.Canvas.Brush.Color := clLtGray;
    bmp.Canvas.Clipping := False;
    bmp.Canvas.Rectangle(15, 0, Width, Height);

    bmp.Canvas.Font.Color := clRed;
    bmp.Canvas.TextOut(3, 2, 'P');
    bmp.Canvas.TextOut(17, 2, Text);
    FCacheIsDirty := False;
  end;

  Canvas.Draw(0, 0, bmp);

  // Draw live pattern cursor
  lCursorPos := Round(Width * (FCursorPosition / FPatternLength));
  if FOldCursorPosition <> lCursorPos then
  begin
    Canvas.Pen.Color := clRed;
    Canvas.Line(lCursorPos, 0, lCursorPos, Height);
    Canvas.Pen.Color := clLtGray;

    FOldCursorPosition := lCursorPos;
  end;

  inherited Paint;
end;

procedure TPatternGUI.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  lSchedulePattern: TSchedulePatternCommand;
begin
  if Button = mbLeft then
  begin
    Self.BeginDrag(False, 5);
  end;

  // TODO Playing pattern should be scheduler
  if (X - Self.Left) < 15 then
  begin
    Scheduled := True;
    writeln('SCHEDULE');
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

  GSettings.SelectedPatternGUI := Self;

  FCacheIsDirty := True;

  DoTrackRefreshGUI;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TPatternGUI.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FCacheIsDirty := True;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TPatternGUI.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

procedure TPatternGUI.DragDrop(Source: TObject; X, Y: Integer);
var
  lTreeView: TTreeView;
  lDropWave: TPatternDropWaveCommand;
begin
  DBLog('start TPatternGUI.DragDrop ' + ObjectID);

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

  DBLog('end TPatternGUI.DragDrop');
end;


procedure TPatternGUI.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);

  Accept := True;
end;

// Pattern has moved location inside or outside of track
procedure TPatternGUI.DoEndDrag(Sender, Target: TObject; X, Y: Integer);
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


function TPatternGUI.GetDragImages: TDragImageList;
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

procedure TPatternGUI.Invalidate;
begin
  inherited Invalidate;

  FPatternControls.Invalidate;
end;

procedure TPatternGUI.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TPatternGUI.Update');

  PatternControls.Update(Subject);

  Position := Model.Position;
  PatternLength := Model.MidiPattern.LoopEnd;  //todo Use global patternlength
  PatternControls.RealBPM := Model.WavePattern.RealBPM;
  Text := ExtractFileName(TPattern(Subject).WavePattern.SampleFileName);
  writeln(inttostr(PatternLength));
  DBLog('end TPatternGUI.Update');
end;

{ TCreateGUICommand }

procedure TPatternGUI.Connect;
begin
  DBLog('start TPatternGUI.Connect');

  Model.Attach(Self);
  PatternControls.ModelObject := ModelObject;
  PatternControls.Model := Model;
  PatternControls.ObjectID := ObjectID;
  PatternControls.ObjectOwnerID := ObjectOwnerID;
  PatternControls.Connect;

  DBLog('end TPatternGUI.Connect');
end;

procedure TPatternGUI.Disconnect;
begin
  DBLog('start TPatternGUI.Disconnect');

  PatternControls.Disconnect;       // DEBUG
  Model.Detach(Self);

  DBLog('end TPatternGUI.Disconnect');
end;

initialization
  RegisterClass(TPatternGUI);

end.
