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

  plugin.pas
}

unit plugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ContNrs, globalconst, utils, global_command, global,
  ladspaloader, ladspa, Menus;

type
  TAutomationDataList = class;

  TSingleParameter = procedure (ASingle: Single) of object;

  TPortParameter = class(THybridPersistentModel)
  public
    PortRangeHint: LongWord;
    Caption: string;
    UpperBound: Single;
    LowerBound: Single;
    IsBoundedAbove: Boolean;
    IsBoundenBelow: Boolean;
    IsLogarithmic: Boolean;
    IsSampleRate: Boolean;
    IsInteger: Boolean;
    IsToggled: Boolean;
    DefaultValue: Single;
    Value: Single;
    AutomationDataList: TAutomationDataList;
  end;

  TArrayOfSingle = Array of Single;
  TArrayOfPortParameter = Array of TPortParameter;
  TArrayOfPSingle = Array of PSingle;

  TPluginType = (
    ptIO, ptSampler, ptDistortion, ptFlanger, ptFilter, ptDecimate, ptReverb,
    ptBassline, ptLADSPA);

  TPluginNodeType = (pntSource, pntSink, pntPlugin);

  TApplyProc = procedure of Object;

  TBasePlugin = class(TObject)
  public
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); virtual; abstract;
  end;

  { TPluginParameter }

  TPluginParameter = class
    Name: string;
    Hint: string;
  end;

  { TPluginParameterList }

  TPluginParameterList = class(TObjectList)
  protected
    function GetPluginParameters(I: Integer): TPluginParameter;
    procedure SetPluginParameters(I: Integer; APluginParameter: TPluginParameter);
  public
    property PluginParameters[I: Integer]: TPluginParameter read GetPluginParameters write SetPluginParameters;
  end;

  { TPluginNode }

  TPluginNode = class(THybridPersistentModel)
  private
    FActive: Boolean;
    FPopulateAutomationDevices: TPopulateAutomationDevices;
    FPortList: TObjectList;
    FParameterList: TObjectList;
    FInputBuffer: PSingle;
    FOutputBuffer: PSingle;
    FInputControlCount: Integer;
    FOutputControlCount: Integer;
    FInputControls: TArrayOfPortParameter;
    FOutputControls: TArrayOfSingle;

    FInputChannelCount: Integer;
    FOutputChannelCount: Integer;
    FInputChannels: TArrayOfPSingle;
    FOutputChannels: TArrayOfPSingle;

    FFrames: Integer;
    FNodeType: TPluginNodeType;
    FPluginName: string;
    FPluginType: TPluginType;
    FChannels: Integer;
    FSequenceNr: Integer;
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
  public
    constructor Create(AObjectOwnerID: string; AMapped: Boolean = True);  reintroduce; virtual;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Instantiate; virtual;
    procedure Activate; virtual;
    procedure Deactivate; virtual;
    procedure Clean; virtual;
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle; AOutputBuffer: PSingle; AFrames: Integer); virtual;
    procedure Clear;
    procedure UpdateParameters; virtual;

    function CreatePortParameter(
      ACaption: string;
      ALowerBound: Single;
      AUpperBound: Single;
      AIsBoundedAbove: Boolean;
      AIsBoundedBelow: Boolean;
      AIsInteger: Boolean;
      AIsLogarithmic: Boolean;
      AIsSampleRate: Boolean;
      AIsToggled: Boolean;
      ADefaultValue: Single;
      AValue: Single;
      ASetValue: TSingleParameter
      ): Integer;

    function PortParameterByName(ACaption: string): TPortParameter;

    property OnPopulateAutomationDevices: TPopulateAutomationDevices
      read FPopulateAutomationDevices write FPopulateAutomationDevices;

    property PortList: TObjectList read FPortList write FPortList;
    property NodeType: TPluginNodeType read FNodeType write FNodeType;
    property InputBuffer: psingle read FInputBuffer write FInputBuffer;
    property OutputBuffer: psingle read FOutputBuffer write FOutputBuffer;

    property InputControlCount: Integer read FInputControlCount write FInputControlCount;
    property OutputControlCount: Integer read FOutputControlCount write FOutputControlCount;
    property InputControls: TArrayOfPortParameter read FInputControls write FInputControls;
    property OutputControls: TArrayOfSingle read FOutputControls write FOutputControls;

    property InputChannelCount: Integer read FInputChannelCount write FInputChannelCount;
    property OutputChannelCount: Integer read FOutputChannelCount write FOutputChannelCount;
    property InputChannels: TArrayOfPSingle read FInputChannels write FInputChannels;
    property OutputChannels: TArrayOfPSingle read FOutputChannels write FOutputChannels;

    property Active: Boolean read FActive write FActive;
  published
    property PluginName: string read FPluginName write FPluginName;
    property PluginType: TPluginType read FPluginType write FPluginType;
    property Frames: Integer read FFrames write FFrames;
    property SequenceNr: Integer read FSequenceNr write FSequenceNr;
    property Channels: Integer read FChannels write FChannels;
  end;

  { TScriptNode }

  TScriptNode = class(TPluginNode)
  public
    constructor Create(AObjectOwnerID: string; AMapped: Boolean = True); reintroduce;
    destructor Destroy; override;
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle; AOutputBuffer: PSingle; AFrames: Integer); override;
  end;

  { TPluginLADSPA }

  TPluginLADSPA = class(TPluginNode)
  private
    FLadspaLoadedPluginItem: TLadspaLoadedPluginItem;
    FPluginInstance: LADSPA_Handle;
    FPluginDescriptor: PLADSPA_Descriptor;
    FUniqueID: Integer;
  public
    procedure Instantiate; override;
    procedure Activate; override;
    procedure Deactivate; override;
    procedure Clean; override;
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
      AOutputBuffer: PSingle; AFrames: Integer); override;
  published
    property UniqueID: Integer read FUniqueID write FUniqueID;
  end;

  { TLADSPACommand }

  TLADSPACommand = class(TCommand)
  private
    FOldValue: Variant;
    FValue: Variant;
    FModel: TPluginLADSPA;
    FParameter: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  public
    procedure Initialize; override;
    property Value: Variant read FValue write FValue;
    property Parameter: Integer read FParameter write FParameter;
  end;

  { TMementoNode }

  TMementoNode = class(TPluginNode)
  public
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
      AOutputBuffer: PSingle; AFrames: Integer); override;
  end;

  { TInternalNode }

  TInternalNode = class(TPluginNode)
  end;

  { TExternalNode }

  TExternalNode = class(TInternalNode)
  private
  public
    constructor Create(AObjectOwnerID: string); reintroduce;
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
      AOutputBuffer: PSingle; AFrames: Integer); override;
  end;

  { TAutomationDataList }

  TAutomationDataList = class(THybridPersistentModel)
  private
    FList: TObjectList;
    FLastIndex: Integer;
    FIndex: Integer;
    FPlugin: TPluginNode;
    FPluginParameter: TPortParameter;
    FDeviceId: string;
    FParameterId: string;
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True); reintroduce;
    destructor Destroy; override;
    procedure Initialize; override;
    function FrameFirstIndex(ALocation: Integer): Integer;
    function FrameLastIndex(ALocation: Integer): Integer;
    function CurrentAutomationData: TAutomationData;
    function NextAutomationData: TAutomationData;
    function PreviousAutomationData: TAutomationData;
    procedure Next;
    procedure First;
    function Eof: Boolean;
    function Bof: Boolean;

    procedure IndexList;
    procedure AddAutomation(AAutomationData :TAutomationData);
    procedure DeleteAutomation(AAutomationData :TAutomationData);

    property LastIndex: Integer read FLastIndex write FLastIndex;
    property Index: Integer read FIndex write FIndex;

    property Plugin: TPluginNode read FPlugin write FPlugin;
    property PluginParameter: TPortParameter read FPluginParameter write FPluginParameter;
  published
    property DeviceId: string read FDeviceId write FDeviceId;
    property ParameterId: string read FParameterId write FParameterId;
    property List: TObjectList read FList write FList;
  end;

  TMenuItemObjectType = (miotNone, miotDevice, miotDeviceParameter);

  { TMenuItemObject }

  TMenuItemObject = class(TMenuItem)
  private
    FDeviceId: string;
    FObjectId: string;
    FObjectType: TMenuItemObjectType;
    FParameterId: string;
    FPlugin: TPluginNode;
    FPluginParameter: TPortParameter;
  public
    property ObjectId: string read FObjectId write FObjectId;
    property ObjectType: TMenuItemObjectType read FObjectType write FObjectType;
    property DeviceId: string read FDeviceId write FDeviceId;
    property ParameterId: string read FParameterId write FParameterId;
    property Plugin: TPluginNode read FPlugin write FPlugin;
    property PluginParameter: TPortParameter read FPluginParameter write FPluginParameter;
  end;

implementation

uses
  pluginhost;

{ TLADSPACommand }

procedure TLADSPACommand.DoExecute;
begin
  if Assigned(FModel) then
  begin
    FOldValue := FModel.InputControls[FParameter].Value;
    FModel.InputControls[FParameter].Value := FValue;

    FModel.Notify;
  end;
end;

procedure TLADSPACommand.DoRollback;
begin
  if Assigned(FModel) then
  begin
    FModel.InputControls[FParameter].Value := FOldValue;

    FModel.Notify;
  end;
end;

procedure TLADSPACommand.Initialize;
begin
  FModel := TPluginLADSPA(GObjectMapper.GetModelObject(ObjectID));
end;

{ TPluginCatalog }

procedure TPluginParameterList.SetPluginParameters(I: Integer; APluginParameter: TPluginParameter);
begin
  Items[i] := APluginParameter;
end;

function TPluginParameterList.GetPluginParameters(I: Integer): TPluginParameter;
begin
  Result := TPluginParameter( Items[i] );
end;

{ TPlugin }

procedure TPluginLADSPA.Instantiate;
var
  lPortIndex: Integer;
  lPortDescriptor: LADSPA_PortDescriptor;
  lPortRangeHint: LADSPA_PortRangeHint;
  lPortName: PChar;
  lControl: PSingle;
  lPortParameter: TPortParameter;
  lDefaultValue: Single;
  lLowerBound: Single;
  lUpperBound: Single;
begin
  if FUniqueID = 0 then
  begin
    raise Exception.Create('Plugin ID 0 does not exist, did you forget to initialize it?');
  end;

  FLadspaLoadedPluginItem := GLadspaPluginFactory.LoadPlugin(FUniqueID);
  FPluginDescriptor := FLadspaLoadedPluginItem.LadspaDescriptor;

  if Assigned(FPluginDescriptor) then
  begin
    FPluginInstance := FPluginDescriptor^.instantiate(FPluginDescriptor,
      Round(GSettings.SampleRate));
  end;

  FInputChannelCount := 0;
  FOutputChannelCount := 0;

  // if there is just 1 audio input, the AInputBuffer should be mixed to mono
  // or 1 channel should be droppen from AInputBuffer (faster but less quality)
  for lPortIndex := 0 to Pred(FPluginDescriptor^.PortCount) do
  begin
    lPortDescriptor := FPluginDescriptor^.PortDescriptors[lPortIndex];
    lPortRangeHint := FPluginDescriptor^.PortRangeHints[lPortIndex];
    lPortName := FPluginDescriptor^.PortNames[lPortIndex];

    if LADSPA_IS_PORT_INPUT(lPortDescriptor) and LADSPA_IS_PORT_AUDIO(lPortDescriptor) then
    begin
      Inc(FInputChannelCount);
      SetLength(FInputChannels, FInputChannelCount);
      FInputChannels[Pred(FInputChannelCount)] :=
        GetMem(FFrames * STEREO * SizeOf(Single));

      FPluginDescriptor^.connect_port(
        FPluginInstance,
        lPortIndex,
        FInputChannels[Pred(FInputChannelCount)]);
    end
    else if LADSPA_IS_PORT_OUTPUT(lPortDescriptor) and LADSPA_IS_PORT_AUDIO(lPortDescriptor) then
    begin
      Inc(FOutputChannelCount);
      SetLength(FOutputChannels, FOutputChannelCount);
      FOutputChannels[Pred(FOutputChannelCount)] :=
        GetMem(FFrames * STEREO * SizeOf(Single));

      FPluginDescriptor^.connect_port(
        FPluginInstance,
        lPortIndex,
        FOutputChannels[Pred(FOutputChannelCount)]);
    end
    else if LADSPA_IS_PORT_INPUT(lPortDescriptor) and LADSPA_IS_PORT_CONTROL(lPortDescriptor) then
    begin
      Inc(FInputControlCount);
      SetLength(FInputControls, FInputControlCount);

      FInputControls[Pred(FInputControlCount)] := TPortParameter.Create(Self.ObjectID);
      lPortParameter := FInputControls[Pred(FInputControlCount)];

      if Assigned(OnPopulateAutomationDevices) then
      begin
        OnPopulateAutomationDevices(Self.ObjectID, lPortParameter.ObjectID, paaInsert);
      end;

      lLowerBound := 0;
      if LADSPA_IS_HINT_BOUNDED_BELOW(lPortRangeHint.HintDescriptor) then
      begin
        lLowerBound := lPortRangeHint.LowerBound;
        if LADSPA_IS_HINT_SAMPLE_RATE(lPortRangeHint.HintDescriptor) then
        begin
          lLowerBound := lLowerBound * GSettings.SampleRate;
        end;
      end;

      lUpperBound := 1;
      if LADSPA_IS_HINT_BOUNDED_ABOVE(lPortRangeHint.HintDescriptor) then
      begin
        lUpperBound := lPortRangeHint.UpperBound;
        if LADSPA_IS_HINT_SAMPLE_RATE(lPortRangeHint.HintDescriptor) then
        begin
          lUpperBound := lUpperBound * GSettings.SampleRate;
        end
      end;


      lDefaultValue := 0;
      case (lPortRangeHint.HintDescriptor AND LADSPA_HINT_DEFAULT_MASK) of
      	LADSPA_HINT_DEFAULT_MINIMUM:
        begin
      		lDefaultValue := lPortParameter.LowerBound;
    		end;
    		LADSPA_HINT_DEFAULT_LOW:
        begin
    			if LADSPA_IS_HINT_LOGARITHMIC(lPortRangeHint.HintDescriptor) then
          begin
    				lDefaultValue := exp(ln(lLowerBound) * 0.75 + ln(lUpperBound) * 0.25);
          end
          else
          begin
    				lDefaultValue := (lLowerBound * 0.75 + lUpperBound * 0.25);
    			end;
    		end;
    		LADSPA_HINT_DEFAULT_MIDDLE:
        begin
          if LADSPA_IS_HINT_LOGARITHMIC(lPortRangeHint.HintDescriptor) then
          begin
    				lDefaultValue := sqrt(lLowerBound * lUpperBound);
    			end
          else
          begin
    				lDefaultValue := (lLowerBound + lUpperBound) * 0.5;
    			end;
        end;
    		LADSPA_HINT_DEFAULT_HIGH:
        begin
          if LADSPA_IS_HINT_LOGARITHMIC(lPortRangeHint.HintDescriptor) then
          begin
      			lDefaultValue := exp(ln(lLowerBound) * 0.25 + ln(lUpperBound) * 0.75);
          end
          else
          begin
    				lDefaultValue := (lLowerBound * 0.25 + lUpperBound * 0.75);
          end;
        end;
    		LADSPA_HINT_DEFAULT_MAXIMUM:
    		begin
          lDefaultValue := lUpperBound;
    		end;
    		LADSPA_HINT_DEFAULT_0:
    		begin
          lDefaultValue := 0;
    		end;
    		LADSPA_HINT_DEFAULT_1:
    		begin
          lDefaultValue := 1;
    		end;
    		LADSPA_HINT_DEFAULT_100:
        begin
          lDefaultValue := 100;
    		end;
    		LADSPA_HINT_DEFAULT_440:
        begin
          lDefaultValue := 440;
    		end;
      end;

      lPortParameter.Caption := lPortName;
      lPortParameter.LowerBound := lLowerBound;
      lPortParameter.UpperBound := lUpperBound;
      lPortParameter.IsBoundedAbove := LADSPA_IS_HINT_BOUNDED_ABOVE(lPortRangeHint.HintDescriptor);
      lPortParameter.IsBoundenBelow := LADSPA_IS_HINT_BOUNDED_BELOW(lPortRangeHint.HintDescriptor);
      lPortParameter.IsInteger := LADSPA_IS_HINT_INTEGER(lPortRangeHint.HintDescriptor);
      lPortParameter.IsLogarithmic := LADSPA_IS_HINT_LOGARITHMIC(lPortRangeHint.HintDescriptor);
      lPortParameter.IsSampleRate := LADSPA_IS_HINT_SAMPLE_RATE(lPortRangeHint.HintDescriptor);
      lPortParameter.IsToggled := LADSPA_IS_HINT_TOGGLED(lPortRangeHint.HintDescriptor);
      lPortParameter.DefaultValue := lDefaultValue;
      lPortParameter.Value := lDefaultValue;

      FPluginDescriptor^.connect_port(
        FPluginInstance,
        lPortIndex,
        @lPortParameter.Value);
    end
    else if LADSPA_IS_PORT_OUTPUT(lPortDescriptor) and LADSPA_IS_PORT_CONTROL(lPortDescriptor) then
    begin
      Inc(FOutputControlCount);
      SetLength(FOutputControls, FOutputControlCount);
      lControl := @FOutputControls[Pred(FOutputControlCount)];
      lControl^ := 0; //initialize with default

      FPluginDescriptor^.connect_port(
        FPluginInstance,
        lPortIndex,
        @FOutputControls[Pred(FOutputControlCount)]);
    end;
  end;
end;

procedure TPluginLADSPA.Activate;
begin
  inherited Activate;

  if Assigned(FPluginDescriptor^.activate) then
  begin
    FPluginDescriptor^.activate(FPluginInstance);
  end;
end;

procedure TPluginLADSPA.Deactivate;
begin
  if Assigned(FPluginDescriptor^.deactivate) then
  begin
    FPluginDescriptor^.deactivate(FPluginInstance);
  end;

  inherited Deactivate;
end;

procedure TPluginLADSPA.Clean;
var
  lIndex: Integer;
begin
  for lIndex := 0 to Pred(FInputChannelCount) do
  begin
    FreeMem(FInputChannels[lIndex]);
  end;
  SetLength(FInputChannels, 0);
  FInputChannelCount := 0;

  for lIndex := 0 to Pred(FOutputChannelCount) do
  begin
    FreeMem(FOutputChannels[lIndex]);
  end;
  SetLength(FOutputChannels, 0);
  FOutputChannelCount := 0;

  if Assigned(FPluginInstance) then
  begin
    FPluginDescriptor^.cleanup(FPluginInstance);
  end;
end;

procedure TPluginLADSPA.Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
  AOutputBuffer: PSingle; AFrames: Integer);
begin
  case FInputChannelCount of
  1: ConvertBufferStereoToMono(AInputBuffer, FInputChannels[0], AFrames);
  2: SplitStereoToDualMono(AInputBuffer, FInputChannels[0], FInputChannels[1], AFrames);
  end;

  // Just trap any problems :)
  try
    FPluginDescriptor^.run(FPluginInstance, AFrames);
  except
  end;

  case FOutputChannelCount of
  1: ConvertBufferMonoToStereo(FOutputChannels[0], AOutputBuffer, AFrames);
  2: CombineDualMonoToStereo(FOutputChannels[0], FOutputChannels[1], AOutputBuffer, AFrames);
  end;
end;

{ TPluginNode }

constructor TPluginNode.Create(AObjectOwnerID: string; AMapped: Boolean = True);
var
  i: Integer;
  lPluginProcessor: TPluginProcessor;
begin
  Inherited Create(AObjectOwnerID);

  FOnCreateInstanceCallback := @DoCreateInstance;

  lPluginProcessor := TPluginProcessor(GObjectMapper.GetModelObject(AObjectOwnerID));

  FFrames := lPluginProcessor.Frames;
  FChannels := 2;
  FInputBuffer := GetMem(FFrames * SizeOf(Single) * FChannels);
  FOutputBuffer := GetMem(FFrames * SizeOf(Single) * FChannels);

  FParameterList := TObjectList.Create(False);

  FActive := True;

  for i := 0 to Pred(FFrames * FChannels) do
  begin
    FInputBuffer[i] := 0;
    FOutputBuffer[i] := 0;
  end;
end;

destructor TPluginNode.Destroy;
begin
  DBLog('start TPluginNode.Destroy: ' + ClassName);

  FParameterList.Free;
  FreeMem(FInputBuffer);
  FreeMem(FOutputBuffer);

  inherited Destroy;

  DBLog('end TPluginNode.Destroy: ' + ClassName);
end;

procedure TPluginNode.DoCreateInstance(var AObject: TObject; AClassName: string);
begin
  DBLog('start TPluginNode.DoCreateInstance');

  DBLog('end TPluginNode.DoCreateInstance');
end;

procedure TPluginNode.Initialize;
begin
  Notify;
end;

procedure TPluginNode.Finalize;
begin
  //
end;

procedure TPluginNode.Instantiate;
var
  lIndex: Integer;
begin
  if Assigned(OnPopulateAutomationDevices) then
  begin
    for lIndex := 0 to Pred(FInputControlCount) do
    begin
      OnPopulateAutomationDevices(Self.ObjectID, FInputControls[lIndex].ObjectID, paaInsert);
    end;
  end;
end;

procedure TPluginNode.Activate;
begin
  Active := True;
end;

procedure TPluginNode.Deactivate;
begin
  Active := False;
end;

procedure TPluginNode.Clean;
begin
  //
end;

procedure TPluginNode.Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
  AOutputBuffer: PSingle; AFrames: Integer);
begin
  UpdateParameters;
end;

procedure TPluginNode.Clear;
var
  i: Integer;
begin
  for i := 0 to Pred(FFrames) do
  begin
    FInputBuffer[i] := 0;
  end;
end;

procedure TPluginNode.UpdateParameters;
begin
  // To be override;
end;

function TPluginNode.CreatePortParameter(
  ACaption: string;
  ALowerBound: Single;
  AUpperBound: Single;
  AIsBoundedAbove: Boolean;
  AIsBoundedBelow: Boolean;
  AIsInteger: Boolean;
  AIsLogarithmic: Boolean;
  AIsSampleRate: Boolean;
  AIsToggled: Boolean;
  ADefaultValue: Single;
  AValue: Single;
  ASetValue: TSingleParameter
  ): Integer;
var
  lPortParameter: TPortParameter;
begin
  Inc(FInputControlCount);
  SetLength(FInputControls, FInputControlCount);

  FInputControls[Pred(FInputControlCount)] := TPortParameter.Create(Self.ObjectID);
  lPortParameter := FInputControls[Pred(FInputControlCount)];
  lPortParameter.Caption := ACaption;
  lPortParameter.LowerBound := ALowerBound;
  lPortParameter.UpperBound := AUpperBound;
  lPortParameter.IsBoundedAbove := AIsBoundedAbove;
  lPortParameter.IsBoundenBelow := AIsBoundedBelow;
  lPortParameter.IsInteger := AIsInteger;
  lPortParameter.IsLogarithmic := AIsLogarithmic;
  lPortParameter.IsSampleRate := AIsSampleRate;
  lPortParameter.IsToggled := AIsToggled;
  lPortParameter.DefaultValue := ADefaultValue;
  lPortParameter.Value := AValue;
end;

function TPluginNode.PortParameterByName(ACaption: string): TPortParameter;
var
  lIndex: Integer;
begin
  Result := nil;

  for lIndex := 0 to Pred(FInputControlCount) do
  begin
    if SameText(FInputControls[lIndex].Caption, ACaption) then
    begin
      Result := FInputControls[lIndex];
      Break;
    end;
  end;
end;

{ TExternalNode }

constructor TExternalNode.Create(AObjectOwnerID: string);
begin
  //
end;

procedure TExternalNode.Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
  AOutputBuffer: PSingle; AFrames: Integer);
begin
  //
end;

{ TMementoNode }

procedure TMementoNode.Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
  AOutputBuffer: PSingle; AFrames: Integer);
begin
  // Virtual base method
end;

{ TScriptNode }

constructor TScriptNode.Create(AObjectOwnerID: string; AMapped: Boolean);
begin
  inherited Create(AObjectOwnerID, AMapped);

end;

destructor TScriptNode.Destroy;
begin

  inherited Destroy;
end;

procedure TScriptNode.Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
  AOutputBuffer: PSingle; AFrames: Integer);
begin
  //
end;

{ TAutomationDataList }

function SortOnAutomationLocation(Item1, Item2: Pointer): Integer;
begin
  if (TAutomationData(Item1).Location < TAutomationData(Item2).Location) then
  begin
    result := -1
  end
  else
  begin
    if (TAutomationData(Item1).Location > TAutomationData(Item2).Location) then
      result := 1
    else
      result := 0;
  end;
end;

procedure TAutomationDataList.DoCreateInstance(var AObject: TObject; AClassName: string);
var
  lAutomationData: TAutomationData;
begin
  lAutomationData := TAutomationData.Create(ObjectID);
  lAutomationData.ObjectOwnerID := ObjectOwnerID;

  AObject := lAutomationData;

  FList.Add(lAutomationData);
end;

constructor TAutomationDataList.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  inherited Create(AObjectOwner, AMapped);

  FOnCreateInstanceCallback := @DoCreateInstance;

  FList := TObjectList.Create(False);
end;

destructor TAutomationDataList.Destroy;
begin
  FList.Free;

  inherited Destroy;
end;

procedure TAutomationDataList.Initialize;
var
  lPlugin: TPluginNode;
  lPluginParameter: TPortParameter;
  lIndex: Integer;
begin
  inherited Initialize;

  IndexList;

  //lPlugin := TPluginNode(GObjectMapper.GetModelObject(FDeviceId));
  //lPluginParameter := TPortParameter(GObjectMapper.GetModelObject(FPortId));

  DBLog('TAutomationDataList.Initialize;');
  for lIndex := 0 to Pred(FList.Count) do
  begin
    dblog(Format('datavalue %f location %d',
      [TAutomationData(FList[lIndex]).DataValue,
      TAutomationData(FList[lIndex]).Location]));
  end;
end;

{
  Return index of first element within window
}
function TAutomationDataList.FrameFirstIndex(ALocation: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Pred(FList.Count) do
  begin
    if TAutomationData(FList.Items[i]).Location >= ALocation then
    begin
      Result := i;
      break;
    end;
  end;
end;

{
  Return index of last element within window
}
function TAutomationDataList.FrameLastIndex(ALocation: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := Pred(FList.Count) downto 0 do
  begin
    if TAutomationData(FList.Items[i]).Location < ALocation then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TAutomationDataList.CurrentAutomationData: TAutomationData;
begin
  if FIndex < FList.Count then
  begin
    Result := TAutomationData(FList.Items[FIndex]);
  end
  else
    Result := TAutomationData(FList.Last)
end;

function TAutomationDataList.NextAutomationData: TAutomationData;
begin
  if Succ(FIndex) < FList.Count then
  begin
    Result := TAutomationData(FList.Items[Succ(FIndex)]);
  end
  else
    Result := TAutomationData(FList.Last)
end;

function TAutomationDataList.PreviousAutomationData: TAutomationData;
begin
  if FIndex > 0 then
  begin
    Result := TAutomationData(FList.Items[Pred(FIndex)]);
  end
  else
    Result := TAutomationData(FList.First)
end;

procedure TAutomationDataList.Next;
begin
  FIndex := FIndex + 1;
end;

function TAutomationDataList.Eof: Boolean;
begin
  Result := FIndex >= FList.Count;
end;

function TAutomationDataList.Bof: Boolean;
begin
  Result := FIndex <= 0;
end;

{
  This method sorts the list on location starting low ending high
  After that it will also be linked into a linked list
}
procedure TAutomationDataList.IndexList;
var
  i: Integer;
begin
  FList.Sort(@SortOnAutomationLocation);

  for i := 0 to FList.Count - 2 do
  begin
    TAutomationData(FList.Items[i]).Next := TAutomationData(FList.Items[i + 1]);
  end;

  // Initialize last one with nil
  if FList.Count > 0 then
  begin
    TAutomationData(FList.Items[FList.Count - 1]).Next := nil;
  end;
end;

procedure TAutomationDataList.AddAutomation(AAutomationData: TAutomationData);
begin
  FList.Add(AAutomationData);

  IndexList;
end;

procedure TAutomationDataList.DeleteAutomation(AAutomationData: TAutomationData
  );
var
  lIndex: Integer;
begin
  lIndex := FList.IndexOf(AAutomationData);
  if lIndex <> -1 then
  begin
    TAutomationData(FList[lIndex]).Free;
    FList.Delete(lIndex);
  end;

  IndexList;
end;

procedure TAutomationDataList.First;
begin
  FIndex := 0;
end;

end.

