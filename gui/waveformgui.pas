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

  waveformgui.pas
}

unit waveformgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLType, Graphics, globalconst, global, jacktypes,
  ComCtrls, pattern, global_command, waveform, utils, ContNrs;

const
  DECIMATED_CACHE_DISTANCE = 64;

type
  { TSimpleWaveForm }

  TSimpleWaveForm = class(TCustomControl)
  private
    FData: PJack_default_audio_sample_t;
    FZoom: single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    property Data: PJack_default_audio_sample_t read FData write FData;
    property Zoom: single read FZoom write FZoom;
  protected
  published
  end;

  { TMarkerGUI }

  TMarkerGUI = class(THybridPersistentView)
  private
    FLocation: Integer;
    FOriginalLocation: Integer;
    FModel: TMarker;
    FSelected: Boolean;
    FLocked: Boolean;
    FSliceType: Integer;
    FDecayRate: single;
    FNextSlice: TMarkerGUI;         // Points to next slice to the right or nil if last
    FPrevSlice: TMarkerGUI;         // Points to next slice to the right or nil if last
  public
    procedure Update(Subject: THybridPersistentModel); reintroduce; override;
    property Selected: Boolean read FSelected write FSelected;
    property Locked: Boolean read FLocked write FLocked;
    property Location: Integer read FLocation write FLocation;
    property OriginalLocation: Integer read FOriginalLocation write FOriginalLocation;
    property Model: TMarker read FModel write FModel;
    property SliceType: Integer read FSliceType write FSliceType;
    property DecayRate: single read FDecayRate write FDecayRate;
    property NextSlice: TMarkerGUI read FNextSlice write FNextSlice;
    property PrevSlice: TMarkerGUI read FPrevSlice write FPrevSlice;
  end;

  { TLoopMarkerGUI }

  TLoopMarkerGUI = class(THybridPersistentView)
  private
    FDataType: TLoopMarkerType;
    FLocation: Integer;
    FModel: TLoopMarker;
  public
    constructor Create(AObjectOwner: string; ADataType: TLoopMarkerType);
    procedure Update(Subject: THybridPersistentModel); reintroduce; override;
    property Model: TLoopMarker read FModel write FModel;
    property DataType: TLoopMarkerType read FDataType write FDataType;
    property Location: Integer read FLocation write FLocation;
  end;

  { TWaveFormGUI }
  TWaveFormGUI = class(TPersistentCustomControl)
  private
    { GUI }
    FZoomFactorX: Single;
    FZoomFactorY: Single;
    FZoomFactorToScreen: Single;
    FZoomFactorToData: Single;
    FOriginalZoomFactorX: Single;
    FOffset: Integer;
    FOldOffset: Integer;
    FOldX: Integer;
    FOriginalOffsetX: Integer;
    FOriginalOffsetY: Integer;
    { Audio }
    FData: PJack_default_audio_sample_t;
    FDecimatedData: PJack_default_audio_sample_t;
    FSliceListGUI: TObjectList;
    FCurrentSliceIndex: Integer;
    FRealCursorPosition: Integer;
    FVirtualCursorPosition: Integer;
    FLoopStart: TLoopMarkerGUI;
    FLoopEnd: TLoopMarkerGUI;
    FLoopLength: TLoopMarkerGUI;
    FBarLength: Integer;
    FDragSlice: Boolean;
    FZooming: Boolean;
    FSelectedSlice: TMarkerGUI;
    FSelectedLoopMarkerGUI: TLoopMarkerGUI;
    FRubberbandX1,
    FRubberbandX2: Integer;
    FRubberbandSelect: Boolean;
    FCursorAdder: Single;
    FCursorReal: Single;
    FCursorRamp: Single;
    FSampleRate: Single;
    FVolumeDecay: Single;
    FReadCount: Integer;
    FSampleFileName: string;
    FTransientThreshold: Integer;
    FModel: TWaveForm;
    FBitmap: TBitmap;
    FCacheIsDirty: Boolean;
    FOldCursorPosition: Integer;

    procedure RecalculateWarp;
    procedure SetTransientThreshold(const AValue: Integer);
    procedure SetZoomFactorX(const AValue: Single);
    procedure SetZoomFactorY(const AValue: Single);
    procedure Sortslices;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce; override;
    procedure Connect; override;
    procedure Disconnect; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function LoopMarkerAt(Location: Integer; AMargin: Single): TLoopMarkerGUI;
    function NextSlice: TMarkerGUI;
    function GetSliceAt(Location: Integer; AMargin: Single): TMarkerGUI;
    property Data: PJack_default_audio_sample_t read FData write FData;
    property DecimatedData: PJack_default_audio_sample_t read FDecimatedData write FDecimatedData;
    property RealCursorPosition: Integer read FRealCursorPosition write FRealCursorPosition;
    property VirtualCursorPosition: Integer read FVirtualCursorPosition write FVirtualCursorPosition;
    property LoopStart: TLoopMarkerGUI read FLoopStart write FLoopStart;
    property LoopEnd: TLoopMarkerGUI read FLoopEnd write FLoopEnd;
    property LoopLength: TLoopMarkerGUI read FLoopLength write FLoopLength;
    property SliceListGUI: TObjectList read FSliceListGUI write FSliceListGUI;
    property CursorReal: Single read FCursorReal write FCursorReal default 1.0;
    property CursorRamp: Single read FCursorRamp write FCursorRamp default 1.0;
    property SampleRate: Single read FSampleRate write FSampleRate;
    property VolumeDecay: Single read FVolumeDecay write FVolumeDecay default 1;
    property ReadCount: Integer read FReadCount write FReadCount;
    property SampleFileName: string read FSampleFileName write FSampleFileName;
    property TransientThreshold: Integer read FTransientThreshold write SetTransientThreshold;
    property BarLength: Integer read FBarLength write FBarLength;
    property Model: TWaveForm read FModel write FModel;
    property CacheIsDirty: Boolean read FCacheIsDirty write FCacheIsDirty;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
                    var Accept: Boolean); override;
    procedure CreateMarkerGUI(AObjectID: string);
    procedure DeleteMarkerGUI(AObjectID: string);
  published
    property ZoomFactorX: Single read FZoomFactorX write SetZoomFactorX;
    property ZoomFactorY: Single read FZoomFactorY write SetZoomFactorY;
  end;

implementation

function compareByLocation(Item1 : Pointer; Item2 : Pointer) : Integer;
var
  location1, location2 : TMarkerGUI;
begin
  // We start by viewing the object pointers as TSlice objects
  location1 := TMarkerGUI(Item1);
  location2 := TMarkerGUI(Item2);

  // Now compare by location
  if location1.Location > location2.Location then
    Result := 1
  else if location1.Location = location2.Location then
    Result := 0
  else
    Result := -1;
end;

{ TSimpleWaveForm }

constructor Tsimplewaveform.Create(Aowner: Tcomponent);
begin
  inherited Create(Aowner);

  FZoom := 4;
end;

destructor Tsimplewaveform.Destroy;
begin
  inherited Destroy;
end;

procedure Tsimplewaveform.Erasebackground(Dc: Hdc);
begin
  //inherited Erasebackground(Dc);
end;

procedure Tsimplewaveform.Paint;
var
  bmp: TBitmap;
  screenloop: integer;
  zeroline: integer;
begin
  bmp := TBitmap.Create;
  try
    bmp.Height := Height;
    bmp.Width := Width;
    zeroline := Height div 2;

    bmp.Canvas.Pen.Color := clBlack;
    bmp.Canvas.Clipping := False;
    bmp.Canvas.Rectangle(0, 0, Width, Height);

    if FData <> nil then
    begin
      bmp.Canvas.Pen.Color := clBlack;
      bmp.Canvas.Line(0, zeroline, Width, zeroline);

      bmp.Canvas.Pen.Color := clBlue;
      bmp.Canvas.MoveTo(0, zeroline);

      for ScreenLoop := 0 to Pred(bmp.Width) do
        bmp.Canvas.LineTo(ScreenLoop, Round(FData[Round(ScreenLoop * FZoom)] * zeroline) + zeroline);
    end;
    Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;

  inherited Paint;
end;

procedure TWaveFormGUI.SetTransientThreshold(const AValue: Integer);
begin
  FTransientThreshold:= AValue;
//  BeatDetect.setThresHold(FTransientThreshold / 100);
//  AutoMarkerProcess(True);
end;

procedure TWaveFormGUI.SetZoomFactorX(const AValue: Single);
begin
  FZoomFactorX := AValue;
  if FZoomFactorX <= 2 then FZoomFactorX := 2;
  FZoomFactorToScreen:= (ZoomFactorX / 1000);
  FZoomFactorToData:= (1000 / ZoomFactorX);

  FCacheIsDirty := True;
end;

procedure TWaveFormGUI.SetZoomFactorY(const AValue: Single);
begin
  FCacheIsDirty := True;
end;

constructor TWaveFormGUI.Create(Aowner: Tcomponent);
begin
  inherited Create(Aowner);

  FLoopStart := TLoopMarkerGUI.Create(ObjectID, ltStart);
  FLoopEnd := TLoopMarkerGUI.Create(ObjectID, ltEnd);
  FLoopLength := TLoopMarkerGUI.Create(ObjectID, ltLength);

  FBitmap := TBitmap.Create;

  // Initalize settings
  FOffset:= 0;
  FData:= nil;
  FDecimatedData:= nil;
  FZoomFactorX:= 2;
  FZoomFactorY:= 3;
  FDragSlice:= False;
  FZooming:= False;
  FRubberbandSelect:= False;
  FCursorAdder:= 0;
  FVolumeDecay:= 1;
  FCacheIsDirty := True;

  FSliceListGUI := TObjectList.Create(True);
  FCurrentSliceIndex:= 0;

  ChangeControlStyle(Self, [csDisplayDragImage], [], True);
end;

destructor TWaveFormGUI.Destroy;
begin
  FSliceListGUI.Free;

  FBitmap.Free;

  FLoopStart.Free;
  FLoopEnd.Free;
  FLoopLength.Free;

  inherited Destroy;
end;

procedure TWaveFormGUI.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TWaveFormGUI.Update');

  DiffLists(
    TWaveForm(Subject).SliceList,
    SliceListGUI,
    @CreateMarkerGUI,
    @DeleteMarkerGUI);

  FLoopStart.Update(TWaveForm(Subject).LoopStart);
  FLoopEnd.Update(TWaveForm(Subject).LoopEnd);
  FLoopLength.Update(TWaveForm(Subject).LoopLength);

  Sortslices;

  Invalidate;

  DBLog('end TWaveFormGUI.Update');
end;

procedure TWaveFormGUI.Connect;
begin
  writeln('start TWaveFormGUI.Connect');

  FLoopStart.ObjectID := Model.LoopStart.ObjectID;
  Model.LoopStart.Attach(FLoopStart);

  FLoopEnd.ObjectID := Model.LoopEnd.ObjectID;
  Model.LoopEnd.Attach(FLoopEnd);

  FLoopLength.ObjectID := Model.LoopLength.ObjectID;
  Model.LoopLength.Attach(FLoopLength);

  writeln('end TWaveFormGUI.Connect');
end;

procedure TWaveFormGUI.Disconnect;
begin
  Model.LoopStart.Detach(FLoopStart);
  Model.LoopEnd.Detach(FLoopEnd);
  Model.LoopLength.Detach(FLoopLength);
end;

procedure TWaveFormGUI.EraseBackground(DC: HDC);
begin
  // Uncomment this to enable default background erasing
  inherited EraseBackground(DC);
end;

procedure TWaveFormGUI.Paint;
var
  ChannelLoop: Integer;
  ScreenLoop: Integer;
  SliceLoop: Integer;
  SliceX: Integer;
  SliceX2: Integer;
  PositionInData1: Single;
  PositionInData2: Single;
  ChannelScreenOffset: Integer;
  ChannelHeight: Integer;
  ChannelZeroLine: Integer;
  TrackHeight: Integer;
  TempRubberbandX: Integer;
  Adder: Single;
  AdderFactor: Single;
  DataValue: Single;
  MaxValue: Single;
  MinValue: Single;
  SubSampleLoop: Integer;
  RedrawRect: TRect;
  Ramp: Single;
  TimeMarker: Integer;
  TimeMarkerSpacing: Integer;
  TimeMarkerLocation: Integer;
  lClipRect: TRect;
begin
  if FCacheIsDirty then
  begin
    FBitmap.Canvas.Clear;

    TrackHeight := Parent.Height;

    // Initializes the Bitmap Size
    FBitmap.Height := TrackHeight;
    FBitmap.Width := Width;

    FBitmap.Canvas.Brush.Color:= clLtGray;

    FBitmap.Canvas.Pen.Color := clBlack;
    FBitmap.Canvas.Clipping := False;
    FBitmap.Canvas.Rectangle(0, 0, Width, TrackHeight);

    if (TChannel(Model.Wave.ChannelList[0]).Buffer <> nil) and (Model.Wave.Frames > 0) then
    begin
      // Bound parameters
      FOffset := GSettings.CursorPosition;

      if FZoomFactorX <= 2 then FZoomFactorX := 2;
      if FZoomFactorY <= 0 then FZoomFactorY := 1;
      FZoomFactorToScreen:= FZoomFactorX / 1000;
      FZoomFactorToData:= 1000 / FZoomFactorX;

      // Draw Selected Area
      if FRubberbandSelect then
      begin
        FBitmap.Canvas.Brush.Color:= clYellow;
        if FRubberbandX1 > FRubberbandX2 then
        begin
          // Swap if x1 > x2
          TempRubberbandX := FRubberbandX1;
          FRubberbandX1 := FRubberbandX2;
          FRubberbandX2 := TempRubberbandX;
        end;
        FBitmap.Canvas.FillRect(Round(FRubberbandX1 * FZoomFactorToScreen - FOffset), 0,
          Round(FRubberbandX2 * FZoomFactorToScreen - FOffset), TrackHeight);
        FBitmap.Canvas.Brush.Color:= clWhite;
      end;

      ChannelHeight := FBitmap.Height div Model.Wave.ChannelCount;
      for ChannelLoop := 0 to Pred(Model.Wave.ChannelCount) do
      begin
        FBitmap.Canvas.Pen.Color := clBlack;
        FBitmap.Canvas.Line(0, ChannelHeight * ChannelLoop, Width, ChannelHeight * ChannelLoop);

        // First point
        ChannelScreenOffset := ChannelLoop * ChannelHeight + ChannelHeight shr 1;
        ChannelZeroLine := ChannelHeight div Model.Wave.ChannelCount;
        FBitmap.Canvas.Pen.Color := clBlue;
        FBitmap.Canvas.Pen.Width := 1;
        FBitmap.Canvas.MoveTo(0, ChannelScreenOffset);

        for SliceLoop := 0 to SliceListGUI.Count - 2 do
        begin
          AdderFactor := TMarkerGUI(SliceListGUI[SliceLoop]).DecayRate;

          SliceX := Round(TMarkerGUI(SliceListGUI[SliceLoop]).Location * FZoomFactorToScreen - FOffset);
          SliceX2 := Round(TMarkerGUI(SliceListGUI[SliceLoop + 1]).Location * FZoomFactorToScreen - FOffset);

          Adder := TMarkerGUI(SliceListGUI[SliceLoop]).OriginalLocation * FZoomFactorToScreen - FOffset;

          // Only render data within the view
          if (SliceX < Width) and (SliceX2 > 0) then
          begin
            for ScreenLoop := SliceX to Pred(SliceX2) do
            begin
              PositionInData1 := (FOffset + Adder) * FZoomFactorToData;
              PositionInData2 := (FOffset + Adder + AdderFactor) * FZoomFactorToData;
              MinValue:= 100;
              MaxValue:= -100;
              if (ScreenLoop <= Width) and (ScreenLoop >= 0) then
              begin
                if FZoomFactorToData > 50 then
                begin
                  // Subsampling sample values when zoomed in
                  DataValue := Model.DecimatedData[Round(PositionInData1 * Model.Wave.ChannelCount + ChannelLoop) div DECIMATED_CACHE_DISTANCE];
                  for SubSampleLoop := Round(PositionInData1) to Round(PositionInData2) - 1 do
                  begin
                    DataValue := Model.DecimatedData[(SubSampleLoop * Model.Wave.ChannelCount + ChannelLoop) div DECIMATED_CACHE_DISTANCE];
                    if DataValue < MinValue then MinValue := DataValue;
                    if DataValue > MaxValue then MaxValue := DataValue;
                  end;
                  FBitmap.Canvas.Line(ScreenLoop, Round(MinValue * FZoomFactorY * ChannelZeroLine) + ChannelScreenOffset,
                    ScreenLoop, Round(MaxValue * FZoomFactorY * ChannelZeroLine) + ChannelScreenOffset);
                end
                else
                begin
                  // Pixelview
                  DataValue := Model.DecimatedData[(Round(PositionInData1) * Model.Wave.ChannelCount + ChannelLoop) div DECIMATED_CACHE_DISTANCE];
                  if PositionInData1 < Model.Wave.ReadCount then
                  begin
                    FBitmap.Canvas.LineTo(ScreenLoop, Round(DataValue * FZoomFactorY * ChannelZeroLine) + ChannelScreenOffset);
                  end;
                end;
              end;
              Adder := Adder + AdderFactor;
            end;
          end;
        end;

        FBitmap.Canvas.Pen.Color := clLime;
        FBitmap.Canvas.MoveTo(0, ChannelScreenOffset);
      end;
    end;

    for SliceLoop := 0 to Pred(SliceListGUI.Count) do
    begin
      SliceX := Round(TMarkerGUI(SliceListGUI[SliceLoop]).Location * FZoomFactorToScreen - FOffset);

      // SliceMarker
      case TMarkerGUI(SliceListGUI[SliceLoop]).SliceType of
      SLICE_UNDELETABLE: Canvas.Pen.Color := clYellow;
      SLICE_NORMAL: Canvas.Pen.Color := clRed;
      SLICE_VIRTUAL: Canvas.Pen.Color := clGray;
      end;

      if TMarkerGUI(SliceListGUI[SliceLoop]).Selected then
        FBitmap.Canvas.Pen.Color := clGreen;

      FBitmap.Canvas.Line(SliceX, TrackHeight - 12, SliceX, 0);
      if TMarkerGUI(SliceListGUI[SliceLoop]).Locked then
      begin
        FBitmap.Canvas.Brush.Color := clLime;
        FBitmap.Canvas.FillRect(SliceX - 4, TrackHeight - 12, SliceX + 4, TrackHeight - 4);
      end
      else
      begin
        FBitmap.Canvas.Brush.Color := clLtGray;
        FBitmap.Canvas.Rectangle(SliceX - 4, TrackHeight - 12, SliceX + 4, TrackHeight - 4);
      end;

{      Ramp := TMarkerGUI(SliceListGUI[SliceLoop]).DecayRate;
      FBitmap.Canvas.TextOut(SliceX, 5, Format('Ramp %f',[Ramp]));
      FBitmap.Canvas.TextOut(SliceX, 15, Format('Location %d',[TMarkerGUI(SliceListGUI[SliceLoop]).Location]));
      FBitmap.Canvas.TextOut(
        Round(TMarkerGUI(SliceListGUI[SliceLoop]).Location * FZoomFactorToScreen - FOffset),
        ChannelHeight - 10, Format('OrigLocation %d',[TMarkerGUI(SliceListGUI[SliceLoop]).OriginalLocation]));}
    end;

    // Draw measurements
    FBitmap.Canvas.Pen.Color := clWhite;
    TimeMarkerSpacing := Round(FZoomFactorToScreen * 22100) ;
    for TimeMarker := 0 to LoopEnd.Location div 22100  do
    begin
      TimeMarkerLocation:= Round((TimeMarker * TimeMarkerSpacing) - FOffset);
      FBitmap.Canvas.Line(TimeMarkerLocation, 0, TimeMarkerLocation, TrackHeight);
    end;


    SliceX := Round((LoopStart.Location) * FZoomFactorToScreen - FOffset);
    FBitmap.Canvas.Pen.Color := clRed;
    FBitmap.Canvas.Pen.Width:= 3;
    FBitmap.Canvas.Line(SliceX, 0, SliceX, TrackHeight);
//    FBitmap.Canvas.TextOut(SliceX, 15, Format('LoopStart %d',[LoopStart.Location]));

    SliceX := Round((LoopEnd.Location) * FZoomFactorToScreen - FOffset);
    FBitmap.Canvas.Pen.Color := clRed;
    FBitmap.Canvas.Line(SliceX, 0, SliceX, TrackHeight);
//    FBitmap.Canvas.TextOut(SliceX, 15, Format('LoopEnd %d',[LoopEnd.Location]));

    FCacheIsDirty := False;
  end;

  Canvas.Draw(0, 0, FBitmap);

  // Draw cursor
  SliceX := Round((Model.RealCursorPosition) * FZoomFactorToScreen - FOffset);
  if FOldCursorPosition <> SliceX then
  begin
    Canvas.Pen.Color := clBlack;
    Canvas.Line(SliceX, 0, SliceX, TrackHeight);

    FOldCursorPosition := SliceX;
  end;

  inherited Paint;
end;

procedure TWaveFormGUI.Mousedown(Button: Tmousebutton; Shift: Tshiftstate; X,
  Y: Integer);
var
  XRelative: Integer;
  i: Integer;
  lMode: Integer;
  lMargin: single;
  lMoveMarkerCommand: TUpdateMarkerCommand;
  lRemoveMarkerCommand: TRemoveMarkerCommand;
  lAddMarkerCommand: TAddMarkerCommand;
  lToggleLockCommand: TToggleLockMarkerCommand;
  lUpdateWaveLoopMarkerCommand: TUpdateWaveLoopMarkerCommand;
begin
  lMargin := 5 * (1000 / ZoomFactorX);
  XRelative:= Round((FOffset + X) * (1000 / ZoomFactorX));

  if (Height - Y) < 10 then
    lMode := 1
  else
    lMode := 0;

  case Button of
  mbLeft:
  begin
    case GSettings.EditMode of
    emEdit:
    begin
      FSelectedLoopMarkerGUI := LoopMarkerAt(XRelative, lMargin);

      if Assigned(FSelectedLoopMarkerGUI) then
      begin
        lUpdateWaveLoopMarkerCommand := TUpdateWaveLoopMarkerCommand.Create(Self.ObjectID);
        try
          lUpdateWaveLoopMarkerCommand.ObjectID := FSelectedLoopMarkerGUI.ObjectID;
          lUpdateWaveLoopMarkerCommand.DataType := FSelectedLoopMarkerGUI.DataType;
          lUpdateWaveLoopMarkerCommand.Persist := True;
          lUpdateWaveLoopMarkerCommand.Location := XRelative;

          GCommandQueue.PushCommand(lUpdateWaveLoopMarkerCommand);
        except
          lUpdateWaveLoopMarkerCommand.Free;
        end;
      end
      else
      begin
        FSelectedSlice := GetSliceAt(XRelative, lMargin);

        if Assigned(FSelectedSlice) then
        begin
          FSelectedSlice.Selected:= True;

          case lMode of
            1:
            begin
              lToggleLockCommand := TToggleLockMarkerCommand.Create(Self.ObjectID);
              try
                lToggleLockCommand.ObjectID := FSelectedSlice.ObjectID;

                GCommandQueue.PushCommand(lToggleLockCommand);
              except
                lToggleLockCommand.Free;
              end;
            end;
            0:
            begin
              if FSelectedSlice.Locked then
              begin
                FDragSlice := True;

                lMoveMarkerCommand := TUpdateMarkerCommand.Create(Self.ObjectID);
                try
                  lMoveMarkerCommand.ObjectID := FSelectedSlice.ObjectID;
                  lMoveMarkerCommand.Location := FSelectedSlice.Location;
                  lMoveMarkerCommand.Persist := True;

                  GCommandQueue.PushCommand(lMoveMarkerCommand);
                except
                  lMoveMarkerCommand.Free;
                end;
              end;
            end;
          end;
        end
        else
        begin
          lAddMarkerCommand := TAddMarkerCommand.Create(Self.ObjectID);
          try
            lAddMarkerCommand.Location := XRelative;
            lAddMarkerCommand.Persist := True;

            GCommandQueue.PushCommand(lAddMarkerCommand);
          except
            lAddMarkerCommand.Free;
          end;
        end;
      end;
    end;
    emRubberbandSelect:
    begin
      // Rubbeband selectmode
      FRubberbandSelect := True;
      FRubberbandX1 := XRelative;
    end;
    emSelect:
    begin
      // Deselect all before add/edit
      for i:= 0 to Pred(SliceListGUI.Count) do
        TMarkerGUI(SliceListGUI.Items[i]).Selected:= False;

      FSelectedSlice:= GetSliceAt(XRelative, lMargin);
      FSelectedSlice.Selected:= True;
    end;
    else
    end;
  end;
  mbRight:
    begin
      FSelectedSlice := GetSliceAt(XRelative, lMargin);

      if not Assigned(FSelectedSlice) then
      begin
        FOriginalZoomFactorX := FZoomFactorX;
        FOriginalOffsetX := X;
        FOriginalOffsetY := Y;
        FOldOffset:= FOffset;
        FOldX:= X;
        FZooming := True;
      end;

      if Assigned(FSelectedSlice) then
      begin
        if not FSelectedSlice.Locked then
        begin
          lRemoveMarkerCommand := TRemoveMarkerCommand.Create(Self.ObjectID);
          try
            lRemoveMarkerCommand.ObjectID := FSelectedSlice.ObjectID;
            lRemoveMarkerCommand.Persist := True;

            GCommandQueue.PushCommand(lRemoveMarkerCommand);
          except
            lRemoveMarkerCommand.Free;
          end;
        end;
      end;
    end;
  end;

  FCacheIsDirty := True;
  Invalidate;

  inherited Mousedown(Button, Shift, X, Y);
end;

procedure TWaveFormGUI.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  XRelative: Integer;
  lMoveMarkerCommand: TUpdateMarkerCommand;
  lUpdateWaveLoopMarkerCommand: TUpdateWaveLoopMarkerCommand;
begin
  XRelative:= Round((FOffset + X) * (1000 / ZoomFactorX));

  if Assigned(FSelectedLoopMarkerGUI) then
  begin
    lUpdateWaveLoopMarkerCommand := TUpdateWaveLoopMarkerCommand.Create(Self.ObjectID);
    try
      lUpdateWaveLoopMarkerCommand.DataType := FSelectedLoopMarkerGUI.DataType;
      lUpdateWaveLoopMarkerCommand.Persist := False;
      lUpdateWaveLoopMarkerCommand.Location := XRelative;

      GCommandQueue.PushCommand(lUpdateWaveLoopMarkerCommand);
    except
      lUpdateWaveLoopMarkerCommand.Free;
    end;

    FSelectedLoopMarkerGUI := nil;
  end
  else
  if FDragSlice then
  begin
    // Update model with last slice location before end drag slice
    // do not persist as this is done BEFORE a change
    lMoveMarkerCommand := TUpdateMarkerCommand.Create(Self.ObjectID);
    try
      lMoveMarkerCommand.ObjectID := FSelectedSlice.ObjectID;
      lMoveMarkerCommand.Location := XRelative;
      lMoveMarkerCommand.Persist := False;

      GCommandQueue.PushCommand(lMoveMarkerCommand);
    except
      lMoveMarkerCommand.Free;
    end;

    FDragSlice:= False;
  end;

  if FZooming then
    FZooming:= False;

  if FRubberbandSelect then
    FRubberbandSelect:= False;

  FCacheIsDirty := True;
  Invalidate;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TWaveFormGUI.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  XRelative: Integer;
begin
  XRelative:= Round((FOffset + X) * (1000 / ZoomFactorX));

  if Assigned(FSelectedLoopMarkerGUI) then
  begin
    FSelectedLoopMarkerGUI.Location := XRelative;
  end
  else
  if FDragSlice then
  begin
    if Assigned(FSelectedSlice) then
    begin
      if FSelectedSlice.Locked then
      begin
        if (XRelative > FSelectedSlice.PrevSlice.Location) and (XRelative < FSelectedSlice.NextSlice.Location) then
        begin
          FSelectedSlice.Location := XRelative;
          Sortslices;
        end;
      end;
    end;
  end
  else
  if FZooming then
  begin
    FZoomFactorX:= FOriginalZoomFactorX + ((FOriginalOffsetY - Y) / 5);
    if FZoomFactorX < 0.3 then FZoomFactorX := 0.3;
    FOffset:= FOldOffset + (X - FOldX);
  end
  else
  if FRubberbandSelect then
  begin
    XRelative:= Round((FOffset + X) * (1000 / ZoomFactorX));
    FRubberbandX2:= XRelative;
  end;

  inherited MouseMove(Shift, X, Y);

  FCacheIsDirty := True;
  Invalidate;
end;

procedure TWaveFormGUI.DragDrop(Source: TObject; X, Y: Integer);
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
    lDropWave := TPatternDropWaveCommand.Create(Self.ObjectID);
    try
      lDropWave.FileName := lTreeView.Selected.Text;

      GCommandQueue.PushCommand(lDropWave);
    except
      lDropWave.Free;
    end;
  end;
end;

procedure TWaveFormGUI.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);

  Accept := True;
end;


function TwaveformGUI.NextSlice: TMarkerGUI;
var
  lMarker: TMarkerGUI;
begin
  Result := nil;

  if FCurrentSliceIndex < SliceListGUI.Count then
    lMarker := TMarkerGUI(SliceListGUI[FCurrentSliceIndex])
  else
    lMarker := TMarkerGUI(SliceListGUI.Last);

  if Assigned(lMarker) then
    Result := lMarker.NextSlice
  else
    Result := nil;
end;

procedure TWaveFormGUI.Sortslices;
var
  i: Integer;
begin
  // Link all sorted
  FSliceListGUI.Sort(@compareByLocation);
  for i := 0 to FSliceListGUI.Count - 2 do
  begin
    if i = 0 then
    begin
      TMarkerGUI(FSliceListGUI[i]).PrevSlice:= nil;
    end;
    if (i + 1) <= FSliceListGUI.Count then
    begin
      TMarkerGUI(FSliceListGUI[i]).NextSlice:= TMarkerGUI(FSliceListGUI[i + 1]);
      TMarkerGUI(FSliceListGUI[i + 1]).PrevSlice:= TMarkerGUI(FSliceListGUI[i]);
    end
    else
      TMarkerGUI(FSliceListGUI[i]).NextSlice:= nil;
  end;

  RecalculateWarp;
end;

procedure TWaveFormGUI.RecalculateWarp;
var
  i: Integer;
begin
  for i := 0 to FSliceListGUI.Count - 2 do
  begin
    TMarkerGUI(FSliceListGUI[i]).DecayRate :=
      (TMarkerGUI(FSliceListGUI[i + 1]).OriginalLocation - TMarkerGUI(FSliceListGUI[i]).OriginalLocation) /
      (TMarkerGUI(FSliceListGUI[i + 1]).Location - TMarkerGUI(FSliceListGUI[i]).Location);
  end;

  // Just initialize the last marker as it's not valid otherwise
  TMarkerGUI(FSliceListGUI[FSliceListGUI.Count - 1]).DecayRate := 1;

  CacheIsDirty := True;
end;

function TWaveFormGUI.GetSliceAt(Location: Integer; AMargin: Single): TMarkerGUI;
var
  i: Integer;
  lSlice: TMarkerGUI;
begin
  Result := nil;

  for i := 0 to Pred(SliceListGUI.Count) do
  begin
    lSlice := TMarkerGUI(SliceListGUI[i]);

    if Abs(Location - lSlice.Location) < AMargin then
    begin
      if lSlice.SliceType <> SLICE_UNDELETABLE then
      begin
        Result:= lSlice;
        FCurrentSliceIndex:= i;
        break;
      end;
    end;
  end;
end;

function TWaveFormGUI.LoopMarkerAt(Location: Integer; AMargin: Single): TLoopMarkerGUI;
begin
  LoopStart.Location := Model.LoopStart.Location;
  LoopEnd.Location := Model.LoopEnd.Location;
  LoopLength.Location := Model.LoopLength.Location;

  Result := nil;

  if Abs(Location - LoopStart.Location) < AMargin then
  begin
    Result := LoopStart;
  end
  else
  if Abs(Location - LoopEnd.Location) < AMargin then
  begin
    Result := LoopEnd;
  end
  else
  if Abs(Location - LoopLength.Location) < AMargin then
  begin
    Result := LoopLength;
  end;
end;

procedure TWaveFormGUI.CreateMarkerGUI(AObjectID: string);
var
  lMarker: TMarker;
  lMarkerGUI: TMarkerGUI;
begin
  DBLog('start TWaveFormGUI.CreateMarkerGUI ' + AObjectID);

  lMarker := TMarker(GObjectMapper.GetModelObject(AObjectID));

  lMarkerGUI := TMarkerGUI.Create(ObjectID);
  lMarkerGUI.ObjectID := AObjectID;
  lMarkerGUI.Location := lMarker.Location;
  lMarkerGUI.OriginalLocation := lMarker.OrigLocation;
  lMarkerGUI.Selected := lMarker.Selected;
  lMarkerGUI.Locked := lMarker.Locked;
  lMarkerGUI.DecayRate := lMarker.DecayRate;
  lMarkerGUI.Model := lMarker;
  lMarker.Attach(lMarkerGUI);

  FSliceListGUI.Add(lMarkerGUI);

  DBLog('end TWaveFormGUI.CreateMarkerGUI');
end;

procedure TWaveFormGUI.DeleteMarkerGUI(AObjectID: string);
var
  lMarker: TMarker;
  lMarkerGUI: TMarkerGUI;
  lIndex: Integer;
begin
  DBLog('start TWaveFormGUI.DeleteMarkerGUI : ' + AObjectID);

  lMarker := TMarker(GObjectMapper.GetModelObject(AObjectID));

  for lIndex := Pred(FSliceListGUI.Count) downto 0 do
  begin
    lMarkerGUI := TMarkerGUI(FSliceListGUI[lIndex]);

    if lMarkerGUI.ObjectID = AObjectID then
    begin
      FSliceListGUI.Remove(lMarkerGUI);
      break;
    end;
  end;

  DBLog('end TWaveFormGUI.DeleteMarkerGUI');
end;

{ TLoopMarkerGUI }

constructor TLoopMarkerGUI.Create(AObjectOwner: string; ADataType: TLoopMarkerType);
begin
  inherited Create(AObjectOwner);

  FDataType := ADataType;
end;

procedure TLoopMarkerGUI.Update(Subject: THybridPersistentModel);
begin
  Self.Location := TLoopMarker(Subject).Location;
end;

{ TMarkerGUI }

procedure TMarkerGUI.Update(Subject: THybridPersistentModel);
begin
  Self.ObjectID := TMarker(Subject).ObjectID;
  Self.Selected := TMarker(Subject).Selected;
  Self.Location := TMarker(Subject).Location;
  Self.OriginalLocation := TMarker(Subject).OrigLocation;
  Self.DecayRate := TMarker(Subject).DecayRate;
  Self.SliceType := TMarker(Subject).SliceType;
  Self.Locked := TMarker(Subject).Locked;
end;

initialization
  RegisterClass(TWaveFormGUI);

end.

