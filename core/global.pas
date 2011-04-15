{
  Copyright (C) 2007 Robbert Latumahina

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

  global.pas
}
unit global;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ContNrs, globalconst, jacktypes, sndfile;

type
  TShuffleRefreshEvent = procedure(TrackObject: TObject) of object;

  TShuffle = class(TObject)
  public
    x: Integer;
    oldx: Integer;
    step: Integer;
    trackobject: TObject;
  end;

  // TMidiData is the real data to be send to plugins, outputs, etc
  // It is generated at loadtime from objects defined in XML
  // These objects contain more information about the mididata and should be the
  // source of the abstracted mididata.
  TMidiData = class
  public
    Location: Integer; // Location in samples
    DataType: Integer;   // Note, CC, NRPN
    DataValue1: byte;
    DataValue2: byte;
    MidiChannel: byte;
    RelativeOffset: Integer;
    Length: Integer; // Notelength

    Next: TMidiData; // Point to next in list
  end;

  TMidiEvent = record
    Location: Integer; // Location in samples
    DataType: Integer;   // Note, CC, NRPN
    DataValue1: byte;
    DataValue2: byte;
    MidiChannel: byte;
    RelativeOffset: Integer;
    Length: Integer; // Notelength
  end;

  { TObjectMapper }

  TObjectMapper = class(THybridPersistent)
  private
    FMaps: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMapping(AObject: TObject);
    procedure DeleteMapping(AGUID: string);
    procedure SetModelObjectID(AObject: TObject; AObjectID: string);
    function GetModelObject(AGUID: string): TObject;

    property Maps: TObjectList read FMaps write FMaps;
  end;

  { TSettings }

  TSettings = Class(THybridPersistent)
  private
    FCursorPosition: Integer;
    FSelectedTrackGUI: TObject;
    FOldSelectedTrackGUI: TObject;
    FSelectedPatternGUI: TObject;
    FOldSelectedPatternGUI: TObject;
    FLastPath: string;
    FEditMode: Byte;
    FModifier: TShiftState;
    FFrames: Integer;
    FOnShuffleRefresh: TShuffleRefreshEvent;
    FIDCounter: Integer;
    FEscapeAction: Boolean;
    FSampleRate: single;
    FSampleMap: string;
    procedure SetFrames(const AValue: Integer);
    procedure SetModifier(const AValue: TShiftState);
    procedure SetSelectedPatternGUI(const AValue: TObject);
  public

    constructor Create;
    destructor Destroy; override;
    function NextID: Integer;
    procedure Update(Subject: THybridPersistentModel);
    property CursorPosition: Integer read FCursorPosition write FCursorPosition;
    property SelectedTrackGUI: TObject read FSelectedTrackGUI write FSelectedTrackGUI;
    property OldSelectedTrackGUI: TObject read FOldSelectedTrackGUI write FOldSelectedTrackGUI;
    property SelectedPatternGUI: TObject read FSelectedPatternGUI write SetSelectedPatternGUI;
    property OldSelectedPatternGUI: TObject read FOldSelectedPatternGUI write FOldSelectedPatternGUI;
    property EditMode: Byte read FEditMode write FEditMode;
    property Modifier: TShiftState read FModifier write SetModifier;
    property Frames: Integer read FFrames write SetFrames;
    property OnShuffleRefresh: TShuffleRefreshEvent read FOnShuffleRefresh write FOnShuffleRefresh;
    property EscapeAction: Boolean read FEscapeAction write FEscapeAction default false;
    property SampleRate: single read FSampleRate write FSampleRate;
    property SampleMap: string read FSampleMap write FSampleMap;
  end;

  { TChannel }

  TChannel = Class(TObject)
  private
    FBufferSize: Integer;
  public
    Buffer: PSingle;

    destructor Destroy; override;
    property BufferSize: Integer read FBufferSize write FBufferSize;
  end;

  { TChannelList }

  TChannelList = class (TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TChannel);
    function GetObject (Index: Integer): TChannel;
  public
    function Add (Obj: TChannel): Integer;
    procedure Insert (Index: Integer; Obj: TChannel);
    property Items [Index: Integer]: TChannel read GetObject write SetObject; default;
  end;

  { TWaveFile }

  TBufferFormat = (bfInterleave, bfSplit);

  TWaveFile = Class(THybridPersistent)
  private
    FChannelList: TObjectList;
    FFileName: string;
    FSampleRate: Integer;
    FBufferFormat: TBufferFormat;
    FFrames: Integer;
    FReadCount: Integer;
    FDataSize: Integer;
    FChannelCount: Integer;
    FData: PSingle;
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    function LoadSample(AFileName: string): Boolean;
    procedure UnloadSample;
    property ChannelList: TObjectList read FChannelList write FChannelList;
    property SampleRate: Integer read FSampleRate write FSampleRate;
    property FileName: string read FFileName write FFileName;
    property BufferFormat: TBufferFormat read FBufferFormat write FBufferFormat;
    property Frames: Integer read FFrames write FFrames;
    property ReadCount: Integer read FReadCount write FReadCount;
    property DataSize: Integer read FDataSize write FDataSize;
    property ChannelCount: Integer read FChannelCount write FChannelCount;
  end;

var
  GObjectMapper: TObjectMapper;
  GSettings: TSettings;

implementation

uses
  utils;

{ TObjectMapper }

constructor TObjectMapper.Create;
begin
  FMaps := TObjectList.Create(False);
end;

destructor TObjectMapper.Destroy;
begin
  Maps.Free;

  inherited Destroy;
end;

procedure TObjectMapper.AddMapping(AObject: TObject);
begin
  DBLog('Add model to mapping: ' + THybridPersistentModel(AObject).ObjectID);

  FMaps.Add(AObject);
end;

procedure TObjectMapper.SetModelObjectID(AObject: TObject; AObjectID: string);
var
  lIndex: Integer;
begin
  lIndex := FMaps.IndexOf(AObject);
  if lIndex <> -1 then
  begin
    THybridPersistentModel(FMaps[lIndex]).ObjectID := AObjectID;
  end;
end;

procedure TObjectMapper.DeleteMapping(AGUID: string);
var
  lIndex: Integer;
begin
  // Always go from high to low when deleting elements
  for lIndex := Pred(FMaps.Count) downto 0 do
  begin
    if THybridPersistentModel(FMaps[lIndex]).ObjectID = AGUID then
    begin
      FMaps.Delete(lIndex);
      break;
    end;
  end;
end;

function TObjectMapper.GetModelObject(AGUID: string): TObject;
var
  lIndex: Integer;
begin
  Result := nil;

  if AGUID = '' then
  begin
    DBLog('Internal error: empty AGUID in TObjectMapper.GetModelObject(AGUID: string)');
  end
  else
  begin
    for lIndex := 0 to Pred(FMaps.Count) do
    begin
      if THybridPersistentModel(FMaps[lIndex]).ObjectID = AGUID then
      begin
        Result := FMaps[lIndex];
        break;
      end;
    end;
  end;

  if Result = nil then
  begin
    DBLog('Model not found! %s', AGUID);
  end;
end;

{ TGSettings }

procedure TSettings.SetModifier(const AValue: TShiftState);
begin
  FModifier:= AValue;
{  TShiftStateEnum = (ssShift, ssAlt, ssCtrl,
    ssLeft, ssRight, ssMiddle, ssDouble,
    // Extra additions
    ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum,
    ssScroll,ssTriple,ssQuad,ssExtra1,ssExtra2);}
end;

procedure TSettings.SetSelectedPatternGUI(const AValue: TObject);
begin
  FSelectedPatternGUI := AValue;
end;

procedure TSettings.SetFrames(const AValue: Integer);
begin
  FFrames := AValue;
end;

constructor TSettings.Create;
begin
  //inherited Create;

  FIDCounter:= 0;
  CursorPosition := 0;
  //RecalculateSynchronize;
end;

destructor TSettings.Destroy;
begin

  inherited Destroy;
end;

function TSettings.NextID: Integer;
begin
  Inc(FIDCounter);
  Result := FIDCounter;
end;

procedure TSettings.Update(Subject: THybridPersistentModel);
begin
  //inherited UpdateGUI;
end;

{ TObjectLoader }

{ TWaveFile }

constructor TWaveFile.Create(AObjectOwner: string; AMapped: Boolean);
var
  lChannelIndex: Integer;
  lChannel: TChannel;
begin
  inherited Create(AObjectOwner);

  FBufferFormat := bfInterleave;

  FChannelList := TChannelList.Create(True);

  for lChannelIndex := 0 to 7 do
  begin
    lChannel := TChannel.Create;
    FChannelList.Add(lChannel);
  end;
end;

destructor TWaveFile.Destroy;
begin
  FChannelList.Free;

  inherited Destroy;
end;

function TWaveFile.LoadSample(AFileName: string): Boolean;
var
  lFilename: pchar;
  lChannelIndex: Integer;
  lChannelSize: Integer;
  lChannelItems: Integer;
  lBufferIndex: Integer;
  lBuffer: PSingle;
  i, j: Integer;
  SamplesRead: Integer;
  lSampleHandle: PSndFile;
  lValue: single;
  lSampleInfo: SF_INFO;
begin
  Result := False;

  lFilename:= StringToPChar(AFilename);
  lSampleHandle := sf_open(lFilename, SFM_READ, lSampleInfo);
  try
    if not Assigned(lSampleHandle) then
    begin
      DBLog(sf_strerror(lSampleHandle));
      Result := False;
    end
    else
    begin
      FDataSize := lSampleInfo.frames * lSampleInfo.channels * SizeOf(Single);
      FChannelCount := lSampleInfo.channels;
      FFrames := lSampleInfo.frames;
      FSampleRate := lSampleInfo.samplerate;

      if Assigned(FData) then
      begin
        Freemem(FData);
      end;

      GetMem(FData, FDataSize + 1000);
      FReadCount := sf_read_float(lSampleHandle, FData, FDataSize);
      lChannelSize := (FReadCount div FChannelCount) * SizeOf(Single);
      lChannelItems := FReadCount div FChannelCount;

      if FReadCount = 0 then
      begin
        // Something went wrong
      end
      else
      begin
        case FBufferFormat of
        bfSplit:
          begin
            (* Split channel version *)
            for lChannelIndex := 0 to Pred(FChannelCount) do
            begin
              TChannel(FChannelList[lChannelIndex]).Buffer := GetMem(lChannelSize);
            end;

            for lChannelIndex := 0 to Pred(FChannelCount) do
            begin
              lBuffer := TChannel(FChannelList[lChannelIndex]).Buffer;
              TChannel(FChannelList[lChannelIndex]).BufferSize := FDataSize;

              for lBufferIndex := 0 to Pred(lChannelItems) do
              begin
                lBuffer[lBufferIndex] := FData[lBufferIndex * FChannelCount + lChannelIndex];
              end;
            end;
          end;
        bfInterleave:
          begin;
            (* Interleaved channel version *)
            TChannel(FChannelList[0]).Buffer := FData;
            TChannel(FChannelList[0]).BufferSize := FDataSize;
          end;
        end;
      end;

      Result := True;
    end

  finally
    sf_close(lSampleHandle);
  end;
end;


procedure TWaveFile.UnloadSample;
begin
  //
end;

{ TChannel }

destructor TChannel.Destroy;
begin
  if Assigned(Buffer) then
    FreeMem(Buffer);

  inherited Destroy;
end;

{ TChannelList }

procedure TChannelList.SetObject(Index: Integer; Item: TChannel);
begin
  inherited Items[Index] := Item;
end;

function TChannelList.GetObject(Index: Integer): TChannel;
begin
  Result := inherited Items[Index] as TChannel;
end;

function TChannelList.Add(Obj: TChannel): Integer;
begin
  Result := inherited Add( Obj );
end;

procedure TChannelList.Insert(Index: Integer; Obj: TChannel);
begin
  inherited Insert( Index, Obj );
end;

initialization
  GObjectMapper := TObjectMapper.Create;
  GSettings := TSettings.Create;

finalization
  GObjectMapper.Free;
  GSettings.Free;

end.

