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

  pattern.pas
}

unit pattern;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, graphics, global_command,
  globalconst, global, pluginhost, fx;

type

  { TPattern }

  TPattern = class(THybridPersistentModel)
  private
    FPatternColor: TColor;
    FPatternCursor: Double;
    FPatternCursorOld: Double;
    FRealCursorPosition: Integer;
    FPitched: Boolean;
    FPosition: Integer; // Vertical position in the patterngrid
    FText: string;
    FSyncQuantize: Boolean;
    FOkToPlay: Boolean;
    FPitch: Single;
    FRootNote: Integer;
    FMidiChannel: Integer;
    FPlaying: Boolean;
    FScheduled: Boolean;
    FPatternLength: Longint;
    FPatternName: string;
    FFileName: string; // The name of the xml file
    FPluginProcessor: TPluginProcessor;
    FFilter: TDecimateFX;
    FFilter2: TMoogFilter;

    FLoopStart: TLoopMarker;
    FLoopEnd: TLoopMarker;
    FLoopLength: TLoopMarker;
    FLooped: Boolean;

    procedure SetOkToPlay(const AValue: Boolean);
    procedure SetPatternColor(const AValue: TColor);
    procedure SetPosition(const AValue: Integer);
    procedure SetText(const AValue: string);
    procedure Setpitch(const Avalue: Single);
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Initialize; override;
    procedure Finalize; override;
    property PatternColor: TColor read FPatternColor write SetPatternColor;
    property PatternCursor: Double read FPatternCursor write FPatternCursor;
    property RealCursorPosition: Integer read FRealCursorPosition write FRealCursorPosition;

    procedure ProcessInit; virtual; abstract;
    procedure Process(ABuffer: PSingle; AFrameIndex: Integer; AFrameCount: Integer); virtual;
    procedure ProcessAdvance; virtual;
  published
    property PluginProcessor: TPluginProcessor read FPluginProcessor write FPluginProcessor;
    property SyncQuantize: Boolean read FSyncQuantize write FSyncQuantize;
    property Position: Integer read FPosition write SetPosition;
    property Text: string read FText write SetText;
    property OkToPlay: Boolean read FOkToPlay write SetOkToPlay;
    property Pitch: Single read FPitch write SetPitch default 1;
    property Pitched: Boolean read FPitched write FPitched default False;
    property RootNote: Integer read FRootNote write FRootNote default 0;
    property MidiChannel: Integer read FMidiChannel write FMidiChannel;
    property Playing: Boolean read FPlaying write FPlaying default False;
    property Scheduled: Boolean read FScheduled write FScheduled default False;
    property PatternLength: Longint read FPatternLength write FPatternLength;
    property PatternName: string read FPatternName write FPatternName;
    property FileName: string read FFileName write FFileName;
    property LoopStart: TLoopMarker read FLoopStart write FLoopStart;
    property LoopEnd: TLoopMarker read FLoopEnd write FLoopEnd;
    property LoopLength: TLoopMarker read FLoopLength write FLoopLength;
    property Looped: Boolean read FLooped write FLooped;
  end;

  { TPatternCommand }

  TPatternCommand = class(TCommand)
  private
    FPattern: TPattern;
  protected
    procedure Initialize; override;
  end;

  { TUpdateLoopMarkerCommand }
(*
  TUpdateLoopMarkerCommand = class(TPatternCommand)
  private
    FDataType: TLoopMarkerType;
    FLocation: Integer;
    FOldLocation: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Location: Integer read FLocation write FLocation;
    property DataType: TLoopMarkerType read FDataType write FDataType;
  end;
*)
  { TLoadPatternCommand }

  TLoadPatternCommand = class(TPatternCommand)
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
  end;

  { TSavePatternCommand }

  TSavePatternCommand = class(TPatternCommand)
  private
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
  end;

implementation

uses
  utils, DOM, XMLWrite, XMLRead, audiostructure;
(*
{ TUpdateLoopMarkerCommand }

procedure TUpdateLoopMarkerCommand.DoExecute;
begin
  DBLog('start TUpdateWaveLoopMarkerCommand.DoExecute');

  if Persist then
  begin
    // Save state
    case FDataType of
    ltStart: FOldLocation := FPattern.LoopStart.Location;
    ltEnd: FOldLocation := FPattern.LoopEnd.Location;
    ltLength: FOldLocation := FPattern.LoopLength.Location;
    end;
  end;

  // Assign
  case FDataType of
  ltStart:
  begin
    if FLocation < 0 then FLocation := 0;
    FPattern.LoopStart.Location := FLocation;
  end;
  ltEnd:
  begin
    if FLocation < 0 then FLocation := 0;
    FPattern.LoopEnd.Location := FLocation;
  end;
  ltLength: FPattern.LoopLength.Location := FLocation;
  end;

  // Update observers
  FPattern.Notify;
  FPattern.LoopStart.Notify;
  FPattern.LoopEnd.Notify;
  FPattern.LoopLength.Notify;

  DBLog('end TUpdateWaveLoopMarkerCommand.DoExecute');
end;

procedure TUpdateLoopMarkerCommand.DoRollback;
begin
  DBLog('start TUpdateWaveLoopStartCommand.DoRollback');

  // Retrieve state
  FPattern.LoopStart.Location := FOldLocation;

  // Assign
  case FDataType of
  ltStart: FPattern.LoopStart.Location := FOldLocation;
  ltEnd: FPattern.LoopEnd.Location := FOldLocation;
  ltLength: FPattern.LoopLength.Location := FOldLocation;
  end;

  // Update observers
  FPattern.Notify;
  FPattern.LoopStart.Notify;
  FPattern.LoopEnd.Notify;
  FPattern.LoopLength.Notify;

  DBLog('end TUpdateWaveLoopStartCommand.DoRollback');
end;
*)
{ TSavePatternCommand }

procedure TSavePatternCommand.DoExecute;
var
  xdoc: TXMLDocument;
  RootNode: TDOMNode;
begin
  FPattern.BeginUpdate;

  //create a document
  xdoc := TXMLDocument.create;

  //create a root node
  RootNode := xdoc.CreateElement('root');
  Xdoc.Appendchild(RootNode);

  //create a pattern node
  RootNode:= xdoc.DocumentElement;

  // pass xmldocument to iterate function
  // pass mode to iterate (Store,Retrieve) ie. Save to xml or load from xml
  // load from xml should create instance of property objects
  FPattern.SaveToXML(FPattern, 0, RootNode);

  // write to XML
  writeXMLFile(xDoc, FPattern.FileName);

  // free memory
  Xdoc.free;

  FPattern.EndUpdate;
end;

procedure TSavePatternCommand.DoRollback;
begin
  inherited DoRollback;
end;

{ TLoadPatternCommand }

procedure TLoadPatternCommand.DoExecute;
var
  xdoc: TXMLDocument;
  RootNode: TDOMNode;
begin
  FPattern.BeginUpdate;

  { TODO
    erase current pattern
  }

  ReadXMLFile(xDoc, FPattern.FileName);
  RootNode := xDoc.DocumentElement.FirstChild;
  if RootNode <> nil then
  begin
    FPattern.LoadFromXML(RootNode);
    FPattern.RecurseNotify(FPattern);
  end;

  FPattern.EndUpdate;
end;

procedure TLoadPatternCommand.DoRollback;
begin
  FPattern.BeginUpdate;

  {for i := Pred(GAudioStruct.Tracks.Count) downto 0 do
  begin
    GAudioStruct.Tracks.Remove(GAudioStruct.Tracks[i]);
  end; }

  FPattern.EndUpdate;
end;

{ TPattern }

procedure TPattern.SetPatternColor(const AValue: TColor);
begin
  FPatternColor := AValue;
end;

procedure TPattern.SetOkToPlay(const AValue: Boolean);
begin
  FOkToPlay := AValue;
end;

procedure TPattern.SetPosition(const AValue: Integer);
begin
  FPosition := AValue;
end;

procedure TPattern.SetText(const AValue: string);
begin
  FText := AValue;
end;

procedure TPattern.Setpitch(const Avalue: Single);
begin
  if Avalue > 8 then
    FPitch := 8
  else if Avalue < 0.1 then
    FPitch := 0.1
  else
    FPitch := Avalue;
end;

procedure TPattern.DoCreateInstance(var AObject: TObject; AClassName: string);
begin
  DBLog('start TPattern.DoCreateInstance');

  DBLog('end TPattern.DoCreateInstance');
end;


procedure TPattern.Initialize;
begin
  OkToPlay := True;

  Notify;
end;

procedure TPattern.Finalize;
begin
  // Stop playing
  OkToPlay := False;

  Notify;
end;

procedure TPattern.Process(ABuffer: PSingle; AFrameIndex: Integer;
  AFrameCount: Integer);
begin
  if (AFrameIndex = 0) or FLooped then
  begin
    FLooped := False;
  end;

  inherited;
end;

constructor TPattern.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  DBLog('start TPattern.Create');

  inherited Create(AObjectOwner, AMapped);

  FOnCreateInstanceCallback := @DoCreateInstance;

  FPluginProcessor := TPluginProcessor.Create(GSettings.Frames, AObjectOwner, AMapped);

  FFilter := TDecimateFX.Create(FPluginProcessor.ObjectID);
  FFilter.PluginName := 'DecimateFX';
  FFilter.Init(16, 44100);
  FFilter2 := TMoogFilter.Create(FPluginProcessor.ObjectID);
  FFilter2.PluginName := 'MoogFilter';
  FFilter2.Frequency := 20000;
  FFilter2.Resonance := 0.0;

  FPluginProcessor.InsertNode(FFilter, FPluginProcessor.AudioOut, FPluginProcessor.AudioIn);
  FPluginProcessor.InsertNode(FFilter2, FFilter, FPluginProcessor.AudioIn);


  FOkToPlay := False;

  // Defaults
  FLoopStart := TLoopMarker.Create(AObjectOwner, ltStart);
  FLoopEnd := TLoopMarker.Create(AObjectOwner, ltEnd);
  FLoopLength := TLoopMarker.Create(AObjectOwner, ltLength);

  FLoopStart.Value := 0;
  FLoopEnd.Value := Round(44100 * 4);

  FLooped := False;

  DBLog('end TPattern.Create');
end;

destructor TPattern.Destroy;
begin
  FPluginProcessor.Free;
  FFilter.Free;
  FFilter2.Free;
  if Assigned(FLoopStart) then
    FLoopStart.Free;
  if Assigned(FLoopEnd) then
    FLoopEnd.Free;
  if Assigned(FLoopLength) then
    FLoopLength.Free;

  inherited Destroy;
end;

procedure TPattern.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TPattern.ProcessAdvance;
begin
  // Advance cursor
  FPatternCursor := FPatternCursor + GAudioStruct.BPMScale;
  if FSyncQuantize then
  begin
    FPatternCursor := FLoopStart.Value + fmod(FPatternCursor - FLoopStart.Value, GAudioStruct.MainQuantizeLength);
    FSyncQuantize := False;
  end
  else if FPatternCursor >= FLoopEnd.Value then
  begin
    FPatternCursor := FPatternCursor - FLoopLength.Value;
    FLooped := True;
  end;

  RealCursorPosition := Round(PatternCursor);

  FFilter2.Frequency := ((RealCursorPosition div 20) mod 15000) + 100;
end;

{ TPatternCommand }

procedure TPatternCommand.Initialize;
begin
  FPattern := TPattern(GObjectMapper.GetModelObject(ObjectOwner));
end;

end.

