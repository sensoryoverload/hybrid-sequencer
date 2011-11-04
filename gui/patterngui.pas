unit patterngui;
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

  patterngui.pas
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, graphics,
  track, LCLType, globalconst, global_command, global, pattern, ComCtrls,
  imglist, LCLintf, Menus;

type

  { TPatternGUI }

  TPatternGUI = class(TPersistentCustomControl)
  private
    {FModel: TPattern;  }
    FPatternColor: TColor;

    FPosition: Integer; // Vertical position in the patterngrid
    FText: string;
    FSyncQuantize: Boolean;
    FOkToPlay: Boolean;
    FPlaying: Boolean;
    FScheduled: Boolean;
    FSelected: Boolean;
    FPatternLength: Longint;
    FPatternRefreshGUI: TPatternRefreshGUIEvent;

    procedure SetPatternColor(const AValue: TColor);
    procedure SetPosition(const AValue: Integer);
    procedure SetText(const AValue: string);

  protected
    procedure DoPatternRefreshGUI;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce; override;
    procedure RecalculateSynchronize;
    procedure Connect; override;
    procedure Disconnect; override;
    {function GetModel: THybridPersistentModel; reintroduce; override;
    procedure SetModel(AModel: THybridPersistentModel); reintroduce; override;}

    property SyncQuantize: Boolean read FSyncQuantize write FSyncQuantize;
    property Position: Integer read FPosition write SetPosition;
    property PatternColor: TColor read FPatternColor write SetPatternColor;
    property Text: string read FText write SetText;
    property OkToPlay: Boolean read FOkToPlay write FOkToPlay;
    property Playing: Boolean read FPlaying write FPlaying default False;
    property Scheduled: Boolean read FScheduled write FScheduled default False;
    property Selected: Boolean read FSelected write FSelected;
    property PatternLength: Longint read FPatternLength write FPatternLength;
    property OnPatternRefreshGUI: TPatternRefreshGUIEvent read FPatternRefreshGUI write FPatternRefreshGUI;
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

procedure TPatternGUI.DoPatternRefreshGUI;
begin
  if Assigned(FPatternRefreshGUI) then
    FPatternRefreshGUI(Self);
end;

constructor TPatternGUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Height := 15;
  RecalculateSynchronize;

  FOkToPlay := False;

  ChangeControlStyle(Self, [csDisplayDragImage], [], True);
end;

destructor TPatternGUI.Destroy;
begin

  inherited;
end;

procedure TPatternGUI.RecalculateSynchronize;
begin
  //FPatternLength := Round(WaveForm.SampleRate * (60 / FRealBPM)) * 4; // TODO choose next multiple of 4
end;

procedure TPatternGUI.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TPatternGUI.Update');

  Position := TPattern(Subject).Position;
  PatternLength := TPattern(Subject).LoopEnd;  //todo Use global patternlength
//  PatternControls.RealBPM := Model.WavePattern.RealBPM;
//  Text := ExtractFileName(TPattern(Subject).WavePattern.SampleFileName);
  writeln(inttostr(PatternLength));
  DBLog('end TPatternGUI.Update');
end;

{ TCreateGUICommand }

procedure TPatternGUI.Connect;
begin
  DBLog('start TPatternGUI.Connect');

  //Pattern.Attach(Self);

  DBLog('end TPatternGUI.Connect');
end;

procedure TPatternGUI.Disconnect;
begin
  DBLog('start TPatternGUI.Disconnect');

  //Pattern.Detach(Self);

  DBLog('end TPatternGUI.Disconnect');
end;

{function TPatternGUI.GetModel: THybridPersistentModel;
begin
  Result := THybridPersistentModel(FModel);
end;

procedure TPatternGUI.SetModel(AModel: THybridPersistentModel);
begin
  FModel := TPattern(AModel);
end;}

initialization
  RegisterClass(TPatternGUI);

end.
