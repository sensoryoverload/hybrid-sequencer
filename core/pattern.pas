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
  globalconst, global;

type

  { TPattern }

  TPattern = class(THybridPersistentModel)
  private
    FPatternColor: TColor;
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
    {FPluginProcessor: TPluginProcessor;
    FFilter: TDecimateFX;
    FFilter2: TMoogFilter;}

    FLoopStart: Longint;
    FLoopEnd: Longint;


    procedure SetOkToPlay(const AValue: Boolean);
    procedure SetPatternColor(const AValue: TColor);
    procedure SetPosition(const AValue: Integer);
    procedure SetText(const AValue: string);
    procedure Setpitch(const Avalue: Single);
    procedure SetLoopEnd(const AValue: Longint);
    procedure SetLoopStart(const AValue: Longint);
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure RecalculateSynchronize;
    procedure Initialize; override;
    procedure Finalize; override;
    property PatternColor: TColor read FPatternColor write SetPatternColor;

    procedure ProcessInit; virtual; abstract;
    procedure Process(ABuffer: PSingle; AFrameIndex: Integer; AFrameCount: Integer); virtual; abstract;
    procedure ProcessAdvance; virtual; abstract;
  published
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
    property LoopStart: Longint read FLoopStart write SetLoopStart;
    property LoopEnd: Longint read FLoopEnd write SetLoopEnd;
  end;

  { TPatternCommand }

  TPatternCommand = class(TCommand)
  private
    FPattern: TPattern;
  protected
    procedure Initialize; override;
  end;

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
  utils, DOM, XMLWrite, XMLRead;

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

constructor TPattern.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  DBLog('start TPattern.Create');

  inherited Create(AObjectOwner, AMapped);

  FOnCreateInstanceCallback := @DoCreateInstance;

  //FPluginProcessor := TPluginProcessor.Create(GSettings.Frames, AObjectOwner, AMapped);
  {
  FFilter := TDecimateFX.Create(FPluginProcessor.ObjectID);
  FFilter.PluginName := 'DecimateFX';
  FFilter.Init(16, 44100);
  FFilter2 := TMoogFilter.Create(FPluginProcessor.ObjectID);
  FFilter2.PluginName := 'MoogFilter';
  FFilter2.Frequency := 20000;
  FFilter2.Resonance := 0.0;

  FPluginProcessor.InsertNode(FFilter, FPluginProcessor.AudioOut, FPluginProcessor.AudioIn);
  FPluginProcessor.InsertNode(FFilter2, FFilter, FPluginProcessor.AudioIn);
  }

  RecalculateSynchronize;

  FOkToPlay := False;

  // Defaults
  FLoopStart:= 0;
  FLoopEnd:= Round(44100 * 4);

  DBLog('end TPattern.Create');
end;

destructor TPattern.Destroy;
begin
  //FPluginProcessor.Free;
  //FFilter.Free;
  //FFilter2.Free;


  inherited Destroy;
end;

procedure TPattern.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TPattern.RecalculateSynchronize;
begin
  //FPatternLength := Round(WavePattern.SampleRate * (60 / FRealBPM)) * 4; // TODO choose next multiple of 4
end;

procedure TPattern.SetLoopEnd(const AValue: Longint);
begin
  if FLoopEnd = AValue then exit;
  FLoopEnd := AValue;
end;

procedure TPattern.SetLoopStart(const AValue: Longint);
begin
  if FLoopStart = AValue then exit;
  FLoopStart := AValue;


end;


{ TPatternCommand }

procedure TPatternCommand.Initialize;
begin
  FPattern := TPattern(GObjectMapper.GetModelObject(ObjectOwner));
end;

end.

