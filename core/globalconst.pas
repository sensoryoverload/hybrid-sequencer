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

  globalconst.pas
}

unit globalconst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, LMessages, Controls, Forms, jacktypes, ContNrs,
  typinfo, variants, DOM, XMLWrite, XMLRead;

const
  MAX_LATENCY = 20000;
  DECIMATED_CACHE_DISTANCE = 64;

  // Slice types
  SLICE_UNDELETABLE = 666;
  SLICE_NORMAL = 555;
  SLICE_VIRTUAL = 444;

  // Edit modes
  emEdit = 0;
  emRubberbandSelect = 1;
  emSelect = 2;

  // Note drag modes
  ndLength = 0;
  ndMove = 1;

  crFull = 1;
  crCursors = 2;

  psStop = 0;
  psPlay = 1;
  psPause = 2;

  ctDelete = 0;
  ctChangeLength = 1;
  ctChangeLengthStart = 2;
  ctChangeLengthStop = 3;

  ctChangeLocation = 4;
  ctChangeLocationStart = 5;
  ctChangeLocationStop = 6;

  ctFocus = 7;

  ptIdle = 0;
  ptSchedule = 1;
  ptPlay = 2;

  APP_NAME = 'Hybrid';
  APP_VERSION = '0.2';

  DEFAULT_NOTE_VELOCITY = 100;
  NOT_MAPPED = False;
  MAPPED = True;

  LM_SYNCMESSAGE = LM_USER + 1;

  mtNoteOn = 0;
  mtNoteOff = 1;
  mtProgramChange = 2;
  mtBankSelect = 3;
  mtCC = 4;
  mtVelocity = 5;

type
  TInterConnectCallback = procedure(AObjectID: string; AParameter: string) of object;

  TOperation = (
    opUpdate,
    opCreate,
    opDestroy,
    opCreateMidi,
    opDestroyMidi,
    opCreateWaveform,
    opDestroyWaveform);

  TMoveCommandAction = (
    maStart,
    maMove,
    maFinalize);

  TLoopMarkerType = (ltStart, ltEnd, ltLength);

  TSampleMarkerType = (stStart, stEnd);

  TStoreType = (stDefinition, stObject, stDatabase, stXML);

  TIterateState = (isStore, isRetrieve);

  //TMidiTypes = (mtNoteOn, mtNoteOff, mtProgramChange, mtBankSelect, mtCC, mtVelocity);

  TFileSourceTypes = (fsTrack, fsEmpty, fsWave, fsMIDI, fsPlugin);

  TPitchAlgorithm = (paNone, paSoundTouchEco, paSoundTouch, paFFT, paRubberband, paPitched);

  TSerializeAction = (saRetrieveProperties, saInitilizeObjects);

  PPSingle = ^PSingle;

  TCurveType = (ctLinear, ctLogarithmic);

  IMidiControllable = interface
    function GetLow: single;
    function GetHigh: single;
    function GetCurveType: TCurveType;
  end;

  THybridPersistentModel = class;

  { TObjectList }

  ISubject = interface;

  IObserver = interface['{38AEACE2-2EFB-4EA3-A540-23BD70D4FEF8}']
    procedure Update(Subject: THybridPersistentModel);
    procedure Connect;
    procedure Disconnect;
    function GetModel: THybridPersistentModel;
    procedure SetModel(AModel: THybridPersistentModel);
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    property ObjectID: string read GetObjectID write SetObjectID;
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property Model: THybridPersistentModel read GetModel write SetModel;
  end;

  ISubject = interface['{7F0461A3-E078-49F3-B9A4-9FB7840F86DA}']
    procedure Attach(Observer: IObserver);
    procedure Detach(Observer: IObserver);
    procedure Notify;
    procedure Initialize;
  end;

  { THybridPersistent }

  THybridPersistent = class(TInterfacedPersistent)
  private
    FObjectOwnerID: string;
    FObjectID: string;
    FModel: THybridPersistentModel;
    FObjectOwner: TObject;
    function GetObjectID: string;
    procedure SetObjectID(const AValue: string);
  public
    constructor Create(AObjectOwner: string);
    destructor Destroy; override;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure Assign(Source: TPersistent); override;
    property Model: THybridPersistentModel read FModel write FModel;
    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
    property ObjectID: string read GetObjectID write SetObjectID;
    property ObjectOwnerID: string read FObjectOwnerID write FObjectOwnerID;
  end;

  TCreateInstanceCallback = procedure (var ACallbackObject: TObject; AClassName: string) of object;

  { THybridPersistentModel }

  THybridPersistentModel = class(THybridPersistent, ISubject)
  private
  protected
    FObservers: TInterfaceList;
    FUpdateCount: Integer;
    FClassType: string;
    FOnCreateInstanceCallback: TCreateInstanceCallback;
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function Updating: Boolean;
    procedure Attach(AObserver: IObserver);
    procedure Detach(AObserver: IObserver);
    procedure Notify;
    procedure Initialize; virtual; abstract;
    procedure Finalize; virtual; abstract;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToXML(pVisitor: THybridPersistentModel; ALevel: Integer; AXMLNode: TDOMNode);
    procedure SaveToFile(AXMLFileName: string);
    procedure LoadFromXML(AXMLNode: TDOMNode);
    procedure LoadFromFile(AXMLFileName: string);
    procedure RecurseNotify(pVisitor: THybridPersistentModel);
    property OnCreateInstanceCallback: TCreateInstanceCallback read FOnCreateInstanceCallback write FOnCreateInstanceCallback;
  published
    property ClassType: string read FClassType;
  end;

  { THybridPersistentView }

  THybridPersistentView = class(THybridPersistent, IObserver)
  private
  public
    constructor Create(AObjectOwner: string);
    destructor Destroy; override;
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string; virtual;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property ObjectID: string read GetObjectID write SetObjectID;
    procedure Update(Subject: THybridPersistentModel); virtual;
    function GetModel: THybridPersistentModel; virtual;
    procedure SetModel(AModel: THybridPersistentModel); virtual;
    procedure Connect; override;
    procedure Disconnect; override;
  end;

  THybridPersistentClass = class of THybridPersistent;


  { TPersistentCustomControl }

  TPersistentCustomControl = class(TCustomControl, IObserver)
  private
    FObjectOwnerID: string;
    FObjectID: string;
    FModel: THybridPersistentModel;
    FObjectOwner: TObject;
  public
    destructor Destroy; override;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure Update(Subject: THybridPersistentModel); reintroduce; virtual;
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string; virtual;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property ObjectID: string read GetObjectID write SetObjectID;
    property Model: THybridPersistentModel read FModel write FModel;
    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
    function GetModel: THybridPersistentModel; virtual;
    procedure SetModel(AModel: THybridPersistentModel); virtual;
  end;

  { TPersistentGraphicControl }

  TPersistentGraphicControl = class(TGraphicControl, IObserver)
  private
    FObjectOwnerID: string;
    FObjectID: string;
    FModel: THybridPersistentModel;
    FObjectOwner: TObject;
  public
    destructor Destroy; override;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure Update(Subject: THybridPersistentModel); reintroduce; virtual;
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string; virtual;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property ObjectID: string read GetObjectID write SetObjectID;
    property Model: THybridPersistentModel read FModel write FModel;
    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
    function GetModel: THybridPersistentModel;
    procedure SetModel(AModel: THybridPersistentModel);
  end;

  { TPersistentScrollBox }

  TPersistentScrollBox = class(TScrollBox, IObserver)
  private
    FObjectOwnerID: string;
    FObjectID: string;
    FModel: THybridPersistentModel;
    FObjectOwner: TObject;
  public
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce; virtual;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string; virtual;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property ObjectID: string read GetObjectID write SetObjectID;
    property Model: THybridPersistentModel read FModel write FModel;
    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
    function GetModel: THybridPersistentModel;
    procedure SetModel(AModel: THybridPersistentModel);
  end;

  { TPersistentFrame }

  TPersistentFrame = class(TFrame, IObserver)
  private
    FObjectOwnerID: string;
    FObjectID: string;
    FModel: THybridPersistentModel;
    FObjectOwner: TObject;
  public
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce; virtual;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string; virtual;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property ObjectID: string read GetObjectID write SetObjectID;
    property Model: THybridPersistentModel read FModel write FModel;
    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
    function GetModel: THybridPersistentModel;
    procedure SetModel(AModel: THybridPersistentModel);
  end;

  TPersistentObjectList = class(TObjectList)
  private
    FObjectID: string;
  public
    property ObjectID: string read FObjectID write FObjectID;
  end;


  TShuffleRefreshEvent = procedure(TrackObject: TObject) of object;
  TTracksRefreshGUIEvent = procedure(TrackObject: TObject) of object;
  TPatternRefreshGUIEvent = procedure(TrackObject: TObject) of object;

  { TFrameData }

  TFrameData =  record
    Data: Single;      // Value of sample frame, usually -1 to +1
    Location: Single;  // Virtual location due to warping
    Ramp: Single;      // Current warp ramp
    Pitch: Single;     // Pitch rate for slice
  end;

  { TLoopMarker }

  TLoopMarker = class(THybridPersistentModel)
  private
    FDataType: TLoopMarkerType;
    FLocation: Integer;
  public
    constructor Create(AObjectOwner: string; ADataType: TLoopMarkerType);
    procedure Initialize; override;
    procedure Finalize; override;
  published
    property DataType: TLoopMarkerType read FDataType write FDataType;
    property Location: Integer read FLocation write FLocation;
  end;

  { TSampleMarker }

  TSampleMarker = class(THybridPersistentModel)
  private
    FDataType: TSampleMarkerType;
    FLocation: Integer;
  public
    constructor Create(AObjectOwner: string; ADataType: TSampleMarkerType);
    procedure Initialize; override;
    procedure Finalize; override;
  published
    property DataType: TSampleMarkerType read FDataType write FDataType;
    property Location: Integer read FLocation write FLocation;
  end;

  { TMarker }

  TMarker = class(THybridPersistentModel)
  private
    FLocation: Integer;          // Location of Slice-startpoint
    FSliceType: Integer;         // See const above
    FActive: Boolean;            // On/Off
    FDecayRate: Single;          // Rate of decay
    FPitchRate: Single;          // Nominal pitch
    FNextSlice: TMarker;         // Points to next slice to the right or nil if last
    FPrevSlice: TMarker;         // Points to next slice to the right or nil if last
    FSelected: Boolean;          // When true it'll be used in batch editting/processing
    FOrigLocation: Integer;      // Original Location property of slice (to calculate warp)
    FLocked: Boolean;            // Locks OrigLocation and makes marker warpable
  public
    procedure Initialize; override;
    procedure Finalize; override;
    property NextSlice: TMarker read FNextSlice write FNextSlice;
    property PrevSlice: TMarker read FPrevSlice write FPrevSlice;
  published
    property Selected: Boolean read FSelected write FSelected;
    property Locked: Boolean read FLocked write FLocked;
    property Location: Integer read FLocation write FLocation;          // Location of Slice-startpoint
    property OrigLocation: Integer read FOrigLocation write FOrigLocation;
    property SliceType: Integer read FSliceType write FSliceType;
    property Active: Boolean read FActive write FActive;
    property DecayRate: Single read FDecayRate write FDecayRate;
    property PitchRate: Single read FPitchRate write FPitchRate;
  end;

  PSlice = ^TSlice;
  TSlice = record
    Location: Integer;          // Location of Slice-startpoint
    SliceType: Integer;         // See const above
    Active: Boolean;            // On/Off
    DecayRate: Single;          // Rate of decay
    NextSlice: Pointer;         // Points to next slice to the right or nil if last
    PrevSlice: Pointer;         // Points to next slice to the right or nil if last
    Selected: Boolean;          // When true it'll be used in batch editting/processing
    OrigLocation: Integer;      // Original Location property of slice (to calculate warp)
    Locked: Boolean;            // Locks OrigLocation and makes marker warpable

    // Implement stack of standard audio plugins per slice
    // Automation track possible
    FXBitRateReducer: Boolean;  // FX implemented
    FXBufferReverse: Boolean;   // inline effect in process TODO
    FXPitchShifter: Boolean;    // scale value
    FXDistort: Boolean;         // Saturate/Hard clipping
    FXFlanger: Boolean;         // speed, depth, feedback
    FXGater: Boolean;           // speed, length, startphase
    FXDelay: Boolean;           // length in ms, feedback
    FXRetrigger: Boolean;       // speed, decay
    FXShuffler: Boolean;        // inline effect in process TODO
    FXFilter: Boolean;          // Moog filter
  end;

  { TWaveData }

  TWaveData = class(TObject)
  private
    FChannels: Integer;
    FSamplerate: Integer;
    FFrames: Integer;
    FFrameSize: Integer;
    FFilename: string;
    FData: PJack_default_audio_sample_t;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadData(ASource: PJack_default_audio_sample_t; ASize: Integer);
    procedure UnLoadData;
    property Channels: Integer read FChannels write FChannels default 0;
    property Samplerate: Integer read FSamplerate write FSamplerate default 44100;
    property Frames: Integer read FFrames write FFrames default 0;
    property FrameSize: Integer read FFrameSize write FFrameSize default 2;
    property Data: PJack_default_audio_sample_t read FData write FData;
    property FileName: string read FFilename write FFilename;
  end;

  { TLoopMarkerGUI }

  TLoopMarkerGUI = class(THybridPersistentView)
  private
    FDataType: TLoopMarkerType;
    FLocation: Integer;
    FLoopMarker: TLoopMarker;
  public
    constructor Create(AObjectOwner: string; ADataType: TLoopMarkerType);
    procedure Update(Subject: THybridPersistentModel); reintroduce; override;
    property LoopMarker: TLoopMarker read FLoopMarker write FLoopMarker;
    property DataType: TLoopMarkerType read FDataType write FDataType;
    property Location: Integer read FLocation write FLocation;
  end;

  { TSampleMarkerGUI }

  TSampleMarkerGUI = class(THybridPersistentView)
  private
    FDataType: TSampleMarkerType;
    FLocation: Integer;
    FSampleMarker: TSampleMarker;
  public
    constructor Create(AObjectOwner: string; ADataType: TSampleMarkerType);
    procedure Update(Subject: THybridPersistentModel); reintroduce; override;
    property SampleMarker: TSampleMarker read FSampleMarker write FSampleMarker;
    property DataType: TSampleMarkerType read FDataType write FDataType;
    property Location: Integer read FLocation write FLocation;
  end;

  TDiffCallback = procedure (AObjectID: string) of object;

procedure ChangeControlStyle(AControl: TControl; const AInclude: TControlStyle; const AExclude: TControlStyle = []; Recursive: Boolean = True);
procedure DiffLists(AModelList, AViewList: TObjectList; ACreateProc, ADestroyProc: TDiffCallback);
function PeekFileType(AFileName: string): TFileSourceTypes;

implementation

uses
  global, utils, Base64;

procedure ChangeControlStyle(AControl: TControl; const AInclude: TControlStyle; const AExclude: TControlStyle = []; Recursive: Boolean = True);
{var
  I: Integer;}
begin
  {AControl.ControlStyle := AControl.ControlStyle + AInclude - AExclude;
  if Recursive and (AControl is TWinControl) then
    for I := 0 to TWinControl(AControl).ControlCount - 1 do
      ChangeControlStyle(TWinControl(AControl).Controls[I], AInclude, AExclude, True);}
end;

procedure DiffLists(AModelList, AViewList: TObjectList; ACreateProc, ADestroyProc: TDiffCallback);
var
  lModelIndex: Integer;
  lModelFound: Boolean;
  lViewIndex: Integer;
  lViewFound: Boolean;
  lViewIntf: IObserver;
begin
  DBLog('start DiffLists');
  try
    if Assigned(AModelList) and Assigned(AViewList) and
      Assigned(ACreateProc) and Assigned(ADestroyProc) then
    begin
      // First look for missing observer on the client side
      for lModelIndex := 0 to Pred(AModelList.Count) do
      begin
        lViewFound := False;

        for lViewIndex := 0 to Pred(AViewList.Count) do
        begin
          lViewIntf := (AViewList[lViewIndex] as IObserver);

          if lViewIntf.ObjectID = THybridPersistentModel(AModelList[lModelIndex]).ObjectID then
          begin
            lViewFound := True;
            break;
          end;
        end;

        // Create observer by callback
        if not lViewFound then
        begin
          DBLog('start ACreateProc %s', THybridPersistent(AModelList[lModelIndex]).ObjectID);
          ACreateProc(THybridPersistent(AModelList[lModelIndex]).ObjectID);
          DBLog('end ACreateProc');
        end;
      end;

      // Now look for missing subjects on the model side as to destroy them
      for lViewIndex := Pred(AViewList.Count) downto 0 do
      begin
        lViewIntf := (AViewList[lViewIndex] as IObserver);

        lModelFound := False;

        for lModelIndex := 0 to Pred(AModelList.Count) do
        begin
          if lViewIntf.ObjectID = THybridPersistentModel(AModelList[lModelIndex]).ObjectID then
          begin
            lModelFound := True;
            break;
          end;
        end;

        // Delete observer by callback
        if not lModelFound then
        begin
          DBLog('start ADestroyProc %s', lViewIntf.ObjectID);
          ADestroyProc(lViewIntf.ObjectID);
          DBLog('end ADestroyProc');
        end;
      end;
    end;

  except
    on e: exception do
    begin
      DumpExceptionCallStack(e);
      DBLog('DiffLists error: ' + e.Message);
    end;
  end;
  DBLog('end DiffLists');
end;

function PeekFileType(AFileName: string): TFileSourceTypes;
var
  xdoc: TXMLDocument;
  RootNode: TDOMNode;
  lProperties: TDOMNode;
begin
  Result := fsEmpty;

  if SameText(Uppercase(ExtractFileExt(AFileName)), '.WAV') then
  begin
    Result := fsWave;
  end
  else if SameText(Uppercase(ExtractFileExt(AFileName)), '.XML') then
  begin
    ReadXMLFile(xDoc, AFileName);
    try
      RootNode := xDoc.DocumentElement.FirstChild;
      if RootNode <> nil then
      begin
        lProperties := RootNode.FirstChild;
        while Assigned(lProperties) do
        begin
          if SameText(lProperties.NodeName, 'CLASSTYPE') then
          begin
            if SameText(lProperties.NodeValue, 'TMIDIPATTERN') then
            begin
              Result := fsMIDI;
            end
            else if SameText(lProperties.NodeValue, 'TWAVEPATTERN') then
            begin
              Result := fsWave;
            end;
            Break;
          end;

          lProperties := lProperties.NextSibling;
        end;
      end;

    finally
      xDoc.Free;
    end;
  end;
end;

procedure CopyObjectToObject(Source, Dest: TObject);
var
  TypInfo: PTypeInfo;
  PropList: TPropList;
  PropCount, i: integer;
  Value: variant;
begin
  TypInfo := Source.ClassInfo;
  PropCount := GetPropList(TypInfo, tkAny, @PropList);
  for i := 0 to PropCount - 1 do
  begin
    Value := GetPropValue (Source, PropList [i]^.Name);
    SetPropValue (Dest, PropList [i]^.Name, Value);
  end;
end;

function GenerateGuidAndRegister(Sender: TObject): string;
var
  lGUID: TGuid;
  lGUIDAsString: string;
begin
  // Create guid
  CreateGUID(lGUID);
  lGUIDAsString:= GUIDToString(lGUID);

  // And register in mapping
  GObjectMapper.AddMapping(Sender);
  Result := lGUIDAsString;
end;

{ THybridPersistent }

function THybridPersistent.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure THybridPersistent.SetObjectID(const AValue: string);
begin
  FObjectID := AValue;
end;

constructor THybridPersistent.Create(AObjectOwner: string);
begin
  FObjectOwnerID := AObjectOwner;
end;

destructor THybridPersistent.Destroy;
begin
  inherited Destroy;
end;

procedure THybridPersistent.Connect;
begin
  Model := GObjectMapper.GetModelObject(ObjectID);
end;

procedure THybridPersistent.Disconnect;
begin
  //
end;

procedure THybridPersistent.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ TPersistentCustomControl }

destructor TPersistentCustomControl.Destroy;
begin
  if Assigned(FModel) then
  begin
    FModel.Detach(Self);
  end;

  inherited Destroy;
end;

procedure TPersistentCustomControl.Connect;
begin
  // Virtual base method
end;

procedure TPersistentCustomControl.Disconnect;
begin
  // Virtual base method
end;

procedure TPersistentCustomControl.Update(Subject: THybridPersistentModel);
begin
   // Virtual base method
end;

function TPersistentCustomControl.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TPersistentCustomControl.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

function TPersistentCustomControl.GetObjectOwnerID: string;
begin
  Result := FObjectOwnerID;
end;

procedure TPersistentCustomControl.SetObjectOwnerID(const AObjectOwnerID: string
  );
begin
  FObjectOwnerID := AObjectOwnerID;
end;

function TPersistentCustomControl.GetModel: THybridPersistentModel;
begin
  Result := FModel;
end;

procedure TPersistentCustomControl.SetModel(AModel: THybridPersistentModel);
begin
  FModel := AModel;
end;

{ TPersistentScrollBox }

destructor TPersistentScrollBox.Destroy;
begin
  if Assigned(FModel) then
  begin
    FModel.Detach(Self);
  end;

  inherited Destroy;
end;

procedure TPersistentScrollBox.Update(Subject: THybridPersistentModel);
begin
  // Virtual base method
end;

procedure TPersistentScrollBox.Connect;
begin
  Model := GObjectMapper.GetModelObject(ObjectID);
end;

procedure TPersistentScrollBox.Disconnect;
begin
  // Virtual base method
end;

function TPersistentScrollBox.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TPersistentScrollBox.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

function TPersistentScrollBox.GetObjectOwnerID: string;
begin
  Result := FObjectOwnerID;
end;

procedure TPersistentScrollBox.SetObjectOwnerID(const AObjectOwnerID: string);
begin
  FObjectOwnerID := AObjectOwnerID;
end;

function TPersistentScrollBox.GetModel: THybridPersistentModel;
begin
  Result := FModel;
end;

procedure TPersistentScrollBox.SetModel(AModel: THybridPersistentModel);
begin
  FModel := AModel;
end;

{ TWaveData }

constructor TWaveData.Create;
begin
  //
end;

destructor TWaveData.Destroy;
begin
  inherited Destroy;
end;

procedure TWaveData.LoadData(ASource: PJack_default_audio_sample_t;
  ASize: Integer);
begin
  if Assigned(FData) then
    Freemem(FData);

  FData := Getmem(Frames * FrameSize * Channels);
end;

procedure TWaveData.UnLoadData;
begin
  //
end;

{ THybridPersistentModel }

constructor THybridPersistentModel.Create(AObjectOwner: string; AMapped: Boolean
  );
var
  lGUID: TGuid;
begin
  inherited Create(AObjectOwner);

  FClassType := ClassName;

  if AMapped then
  begin
    FObjectOwner := GObjectMapper.GetModelObject(AObjectOwner);

    // Create guid
    CreateGUID(lGUID);
    FObjectID:= GUIDToString(lGUID);

    // And register in mapping
    GObjectMapper.AddMapping(Self);
  end;

  FUpdateCount := 0;
  FObservers := TInterfaceList.Create;
end;

destructor THybridPersistentModel.Destroy;
begin
  FObservers.Free;

  GObjectMapper.DeleteMapping(ObjectID);

  inherited Destroy;
end;

procedure THybridPersistentModel.Attach(AObserver: IObserver);
begin
  DBLog(Format('start %s.Attach', [Self.ClassName]));

  FObservers.Add(AObserver);
  AObserver.Model := Self;
  AObserver.ObjectID := Self.ObjectID;
  AObserver.ObjectOwnerID := Self.ObjectOwnerID;

  Notify;

  DBLog(Format('end %s.Attach', [Self.ClassName]));
end;

procedure THybridPersistentModel.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure THybridPersistentModel.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then Notify;
end;

function THybridPersistentModel.Updating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure THybridPersistentModel.Detach(AObserver: IObserver);
begin
  DBLog(Format('start %s.Detach', [Self.ClassName]));

  { not sure if this would break existing code, it shoudn't.. }
  //AObserver.Disconnect;
  AObserver.ObjectID := '';
  AObserver.ObjectOwnerID := '';{}

  FObservers.Remove(AObserver);

  DBLog(Format('end %s.Detach', [Self.ClassName]));
end;

procedure THybridPersistentModel.Notify;
var
  i: Integer;
begin
  DBLog(Format('start %s.Notify', [Self.ClassName]));

  if FObservers <> nil then
  begin
    for i := 0 to Pred(FObservers.Count) do
    begin
      (FObservers[i] as IObserver).Update(Self);
    end;
  end;

  DBLog(Format('end %s.Notify', [Self.ClassName]));
end;

procedure THybridPersistentModel.Assign(Source: TPersistent);
begin
  {if Source is Self.ClassType then Assign(Source)
  else }inherited Assign(Source);
end;

procedure THybridPersistentModel.LoadFromXML(AXMLNode: TDOMNode);
var
  lPropList: TPropList;
  lPropInfo: PPropInfo;
  lPropType: PTypeInfo;
  lPropCount: Integer;
  lModelObject: THybridPersistentModel;
  lVisited: TObject;
  PropName: string;
  lPropXMLNode: TDOMNode;
  i: Integer;
  lPropFound: Boolean;
  lPropertyType: string;
begin
  DBLog('start THybridPersistentModel.LoadFromXML ' + ClassName);

  if AXMLNode = nil then Exit; // Stops if reached a leaf

  lPropCount := GetPropList(Self.ClassInfo, tkAny, @lPropList);

  // Goes to the child node
  lPropXMLNode := AXMLNode;

  WriteLn(lPropXMLNode.NodeValue);

  // Processes all child nodes
  while lPropXMLNode <> nil do
  begin
    // Find property by name, should be easier than this i think
    lPropFound := False;
    for i := 0 to Pred(lPropCount) do
    begin
      lPropInfo := lPropList[i];
      lPropType := lPropInfo^.PropType;

      PropName := lPropInfo^.Name;

      if PropName = lPropXMLNode.NodeName then
      begin
        lPropFound := True;
        break;
      end;
    end;

    if lPropFound then
    begin
      lPropertyType := TDOMElement(lPropXMLNode).GetAttribute('DataType');
      writeln('Reading Class: ' + lPropertyType);

      if lPropType^.Kind = tkMethod then
      begin
        Continue;
      end
      else if lPropType^.Kind = tkClass then
      begin
        lVisited := GetObjectProp(Self, PropName);
        if Assigned(lVisited) then
        begin
          writeln('PropName ' + PropName);

          // Is property an owner list item
          if (lVisited is TObjectList) then
          begin
            lModelObject := nil;

            // If so first create instance before going into it
            if Assigned(FOnCreateInstanceCallback) then
            begin
              FOnCreateInstanceCallback(lModelObject, lPropertyType);
            end;

            if Assigned(lModelObject) then
            begin
              // Now set all properties
              lModelObject.LoadFromXML(lPropXMLNode.FirstChild);
            end;
          end
          else if (lVisited is THybridPersistentModel) then
          begin
            lVisited := GetObjectProp(Self, PropName);

            if Assigned(lVisited) then
            begin
              if (lVisited is THybridPersistentModel) then
              begin
                // Now set all properties
                THybridPersistentModel(lVisited).LoadFromXML(lPropXMLNode.FirstChild);
              end;
            end;
            writeln('recurse THybridPersistentModel');
          end;
        end;
      end
      else
      begin
        writeln(Format('%s.%s = %s', [ClassName, PropName, lPropXMLNode.Attributes[0].NodeValue]));
        case lPropType^.Kind of
          // All integer properties...
          tkInteger, tkChar, tkSet, tkWChar:
          begin
            SetOrdProp(Self, PropName, StrToInt(lPropXMLNode.Attributes[0].NodeValue));
          end;
          tkEnumeration:
          begin
            SetEnumProp(Self, PropName, lPropXMLNode.Attributes[0].NodeValue);
          end;
          // Floating point properties...
          tkFloat:
          begin
            SetFloatProP(Self, PropName, StrToFloat(lPropXMLNode.Attributes[0].NodeValue));
          end;
          // String properties...
          tkLString, tkString, tkAString:
          begin
            // Do not overwrite Name as this has to be unique and this is provided
            // by the compiler
            if CompareText(PropName, 'Name') <> 0 then
            begin
              SetStrProp(Self, PropName, lPropXMLNode.Attributes[0].NodeValue);
            end;
          end;
        end;
      end;
    end;

    lPropXMLNode := lPropXMLNode.NextSibling;
  end;

  // Initialize after setting all object properties
  Initialize;

  DBLog('end THybridPersistentModel.LoadFromXML ' + ClassName);
end;

procedure THybridPersistentModel.LoadFromFile(AXMLFileName: string);
var
  xdoc: TXMLDocument;
  RootNode: TDOMNode;
begin
  BeginUpdate;

  ReadXMLFile(xDoc, AXMLFileName);
  try
    RootNode := xDoc.DocumentElement.FirstChild;
    if RootNode <> nil then
    begin
      LoadFromXML(RootNode);
      RecurseNotify(Self);
    end;
  finally
    xdoc.Free;
  end;

  EndUpdate;
end;

procedure THybridPersistentModel.SaveToXML(pVisitor: THybridPersistentModel; ALevel: Integer; AXMLNode: TDOMNode);
var
  lPropXMLNode: TDOMNode;
  i, j: integer;
  lVisited: TObject;
  lObjectList: TObjectList;
  PropName, PropValue: string;
  lPropList: TPropList;
  lPropInfo: PPropInfo;
  lPropType: PTypeInfo;
  lPropCount: Integer;
  lLevelSpaces: string;
begin
  Inc(ALevel, 3);

  lLevelSpaces := '';
  for i := 0 to Pred(ALevel) do
    lLevelSpaces := lLevelSpaces + ' ';

  lPropCount := GetPropList(Self.ClassInfo, tkAny, @lPropList);

  for i := 0 to Pred(lPropCount) do
  begin
    lPropInfo := lPropList[i];
    lPropType := lPropInfo^.PropType;

    PropName := lPropInfo^.Name;
    PropValue := GetPropValue(Self, PropName);

    if lPropType^.Kind = tkMethod then
    begin
      Continue;
    end
    else if lPropType^.Kind = tkClass then
    begin
      lVisited := GetObjectProp(Self, PropName);
      if Assigned(lVisited) then
      begin
        if (lVisited is TObjectList) then
        begin
          lObjectList := TObjectList(lVisited);

          for j := 0 to Pred(lObjectList.Count) do
          begin
            if (lObjectList[j] is THybridPersistentModel) then
            begin
              lPropXMLNode := AXMLNode.OwnerDocument.CreateElement(PropName);
              TDOMElement(lPropXMLNode).SetAttribute('DataType', THybridPersistentModel(lObjectList[j]).ClassName);
              TDOMElement(lPropXMLNode).SetAttribute('Kind', 'Iterate');
              {TDOMElement(lPropXMLNode).SetAttribute('ObjectOwnerID', THybridPersistentModel(lObjectList[j]).ObjectOwnerID);
              TDOMElement(lPropXMLNode).SetAttribute('ObjectID', THybridPersistentModel(lObjectList[j]).ObjectID);}

              THybridPersistentModel(lObjectList[j]).SaveToXML(pVisitor, ALevel, lPropXMLNode);
              AXMLNode.Appendchild(lPropXMLNode);
            end;
          end;
        end
        else if (lVisited is THybridPersistentModel) then
        begin
          lPropXMLNode := AXMLNode.OwnerDocument.CreateElement(PropName);
          TDOMElement(lPropXMLNode).SetAttribute('DataType', THybridPersistentModel(lVisited).ClassName);
          TDOMElement(lPropXMLNode).SetAttribute('Kind', 'Recurse');
          {TDOMElement(lPropXMLNode).SetAttribute('ObjectOwnerID', THybridPersistentModel(lVisited).ObjectOwnerID);
          TDOMElement(lPropXMLNode).SetAttribute('ObjectID', THybridPersistentModel(lVisited).ObjectID);}

          THybridPersistentModel(lVisited).SaveToXML(pVisitor, ALevel, lPropXMLNode);
          AXMLNode.Appendchild(lPropXMLNode);
        end;
      end;
    end
    else
    begin
      lPropXMLNode := AXMLNode.OwnerDocument.CreateElement(PropName);
      TDOMElement(lPropXMLNode).SetAttribute('Value', PropValue);
      TDOMElement(lPropXMLNode).SetAttribute('DataType', lPropType^.Name);

      AXMLNode.Appendchild(lPropXMLNode);
    end;
  end;
end;

procedure THybridPersistentModel.SaveToFile(AXMLFileName: string);
var
  xdoc: TXMLDocument;
  RootNode: TDOMNode;
begin
  BeginUpdate;

  //create a document
  xdoc := TXMLDocument.create;

  //create a root node
  RootNode := xdoc.CreateElement('root');
  Xdoc.Appendchild(RootNode);

  //create a Self node
  RootNode:= xdoc.DocumentElement;

  // pass xmldocument to iterate function
  // pass mode to iterate (Store,Retrieve) ie. Save to xml or load from xml
  // load from xml should create instance of property objects
  SaveToXML(Self, 0, RootNode);

  // write to XML
  writeXMLFile(xDoc, AXMLFileName);
  // free memory
  Xdoc.free;

  EndUpdate;
end;

procedure THybridPersistentModel.RecurseNotify(pVisitor: THybridPersistentModel);
var
  i, j: integer;
  lVisit: TObject;
  lVisitObjectList: TObjectList;
  PropName: string;
  lPropList: TPropList;
  lPropInfo: PPropInfo;
  lPropType: PTypeInfo;
  lPropCount: Integer;
begin
  lPropCount := GetPropList(Self.ClassInfo, tkAny, @lPropList);

  for i := 0 to Pred(lPropCount) do
  begin
    lPropInfo := lPropList[i];
    lPropType := lPropInfo^.PropType;

    PropName := lPropInfo^.Name;

    if lPropType^.Kind = tkMethod then
    begin
      Continue;
    end
    else if lPropType^.Kind = tkClass then
    begin
      lVisit := GetObjectProp(Self, PropName);
      if Assigned(lVisit) then
      begin
        if (lVisit is TObjectList) then
        begin
          lVisitObjectList := TObjectList(lVisit);

          for j := 0 to Pred(lVisitObjectList.Count) do
          begin
            if (lVisitObjectList[j] is THybridPersistentModel) then
            begin
              DBLog('Initialize: %s', THybridPersistentModel(lVisitObjectList[j]).ClassName);
              THybridPersistentModel(lVisitObjectList[j]).Notify;
              THybridPersistentModel(lVisitObjectList[j]).RecurseNotify(pVisitor);
            end;
          end;
        end
        else if (lVisit is THybridPersistentModel) then
        begin
          DBLog('Initialize: %s', THybridPersistentModel(lVisit).ClassName);
          THybridPersistentModel(lVisit).Notify;
          THybridPersistentModel(lVisit).RecurseNotify(pVisitor);
        end;
      end;
    end;
  end;
end;

{ THybridPersistentView }

function THybridPersistentView.GetObjectOwnerID: string;
begin
  Result := FObjectOwnerID;
end;

procedure THybridPersistentView.SetObjectOwnerID(const AObjectOwnerID: string);
begin
  FObjectOwnerID := AObjectOwnerID;
end;

constructor THybridPersistentView.Create(AObjectOwner: string);
begin
  inherited Create(AObjectOwner);
end;

destructor THybridPersistentView.Destroy;
begin
  {if Assigned(FModel) then
  begin
    FModel.Detach(Self);
  end;}

  inherited Destroy;
end;

function THybridPersistentView.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure THybridPersistentView.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

procedure THybridPersistentView.Update(Subject: THybridPersistentModel);
begin
  FObjectID := Subject.ObjectID;
end;

function THybridPersistentView.GetModel: THybridPersistentModel;
begin
  Result := FModel;
end;

procedure THybridPersistentView.SetModel(AModel: THybridPersistentModel);
begin
  FModel := AModel;
end;

procedure THybridPersistentView.Connect;
begin
  // Virtual base method
end;

procedure THybridPersistentView.Disconnect;
begin
  // Virtual base method
end;

{ TLoopMarker }

constructor TLoopMarker.Create(AObjectOwner: string; ADataType: TLoopMarkerType);
begin
  inherited Create(AObjectOwner, False);

  FDataType := ADataType;
end;

procedure TLoopMarker.Initialize;
begin
  Notify;
end;

procedure TLoopMarker.Finalize;
begin
  //
end;

{ TSampleMarker }

constructor TSampleMarker.Create(AObjectOwner: string; ADataType: TSampleMarkerType);
begin
  inherited Create(AObjectOwner, False);

  FDataType := ADataType;
end;

procedure TSampleMarker.Initialize;
begin
  Notify;
end;

procedure TSampleMarker.Finalize;
begin
  //
end;

{ TPersistentPanel }

destructor TPersistentFrame.Destroy;
begin
  if Assigned(FModel) then
  begin
    FModel.Detach(Self);
  end;

  inherited Destroy;
end;

procedure TPersistentFrame.Update(Subject: THybridPersistentModel);
begin
  // Virtual base method
end;

procedure TPersistentFrame.Connect;
begin
  Model := GObjectMapper.GetModelObject(ObjectID);
end;

procedure TPersistentFrame.Disconnect;
begin
  // Virtual base method
end;

function TPersistentFrame.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TPersistentFrame.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

function TPersistentFrame.GetObjectOwnerID: string;
begin
  Result := FObjectOwnerID;
end;

procedure TPersistentFrame.SetObjectOwnerID(const AObjectOwnerID: string);
begin
  FObjectOwnerID := AObjectOwnerID;
end;

function TPersistentFrame.GetModel: THybridPersistentModel;
begin
  Result := FModel;
end;

procedure TPersistentFrame.SetModel(AModel: THybridPersistentModel);
begin
  FModel := AModel;
end;


{ TMarker }

procedure TMarker.Initialize;
begin
  Notify;
end;

procedure TMarker.Finalize;
begin
  //
end;

{ TPersistentGraphicControl }

destructor TPersistentGraphicControl.Destroy;
begin
  if Assigned(FModel) then
  begin
    FModel.Detach(Self);
  end;

  inherited Destroy;
end;

procedure TPersistentGraphicControl.Connect;
begin
  // Virtual base class
end;

procedure TPersistentGraphicControl.Disconnect;
begin
  // Virtual base class
end;

procedure TPersistentGraphicControl.Update(Subject: THybridPersistentModel);
begin
  // Virtual base class
end;

function TPersistentGraphicControl.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TPersistentGraphicControl.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

function TPersistentGraphicControl.GetObjectOwnerID: string;
begin
  Result := FObjectOwnerID;
end;

procedure TPersistentGraphicControl.SetObjectOwnerID(
  const AObjectOwnerID: string);
begin
  FObjectOwnerID := AObjectOwnerID;
end;

function TPersistentGraphicControl.GetModel: THybridPersistentModel;
begin
  Result := FModel;
end;

procedure TPersistentGraphicControl.SetModel(AModel: THybridPersistentModel);
begin
  FModel := AModel;
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

{ TSampleMarkerGUI }

constructor TSampleMarkerGUI.Create(AObjectOwner: string; ADataType: TSampleMarkerType);
begin
  inherited Create(AObjectOwner);

  FDataType := ADataType;
end;

procedure TSampleMarkerGUI.Update(Subject: THybridPersistentModel);
begin
  Self.Location := TSampleMarker(Subject).Location;
end;


end.

