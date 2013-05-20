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
  ladspaloader, ladspa, math;

type
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
  end;

  TArrayOfSingle = Array of Single;
  TArrayOfPortParameter = Array of TPortParameter;

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
    FPortList: TObjectList;
    FParameterList: TObjectList;
    FReturnBuffer: PSingle;
    FMidiBuffer: TMidiBuffer;
    FInputBuffer: PSingle;
    FOutputBuffer: PSingle;
    FInputControlCount: Integer;
    FOutputControlCount: Integer;
    FInputControls: TArrayOfPortParameter;
    FOutputControls: TArrayOfSingle;
    FFrames: Integer;
    FNodeType: TPluginNodeType;
    FPluginName: string;
    FPluginType: TPluginType;
    FChannels: Integer;
    FSequenceNr: Integer;
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
  public
    constructor Create(AObjectOwnerID: string; AMapped: Boolean = True); virtual;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Instantiate; virtual;
    procedure Activate; virtual;
    procedure Deactivate; virtual;
    procedure Clean; virtual;
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle; AOutputBuffer: PSingle; AFrames: Integer); virtual; abstract;
    procedure Clear;
    property PortList: TObjectList read FPortList write FPortList;
    property NodeType: TPluginNodeType read FNodeType write FNodeType;
    property InputBuffer: psingle read FInputBuffer write FInputBuffer;
    property OutputBuffer: psingle read FOutputBuffer write FOutputBuffer;
    property InputControlCount: Integer read FInputControlCount write FInputControlCount;
    property OutputControlCount: Integer read FOutputControlCount write FOutputControlCount;
    property InputControls: TArrayOfPortParameter read FInputControls write FInputControls;
    property OutputControls: TArrayOfSingle read FOutputControls write FOutputControls;
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
    constructor Create(AObjectOwnerID: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle; AOutputBuffer: PSingle; AFrames: Integer); override;
  end;

  { TPluginLADSPA }

  TPluginLADSPA = class(TPluginNode)
  private
    FLadspaLoadedPluginItem: TLadspaLoadedPluginItem;
    FPluginInstance: LADSPA_Handle;
    FPluginDescriptor: PLADSPA_Descriptor;
    FFirstRun: Boolean;
    FInputChannelCount: Integer;
    FOutputChannelCount: Integer;
    FInputChannels: Array of PSingle;
    FOutputChannels: Array of PSingle;
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
    constructor Create(AObjectOwnerID: string);
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
      AOutputBuffer: PSingle; AFrames: Integer); override;
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
  lMinValue: Single;
  lMaxValue: Single;
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

      lMinValue := 0;
      if LADSPA_IS_HINT_BOUNDED_BELOW(lPortRangeHint.HintDescriptor) then
      begin
        if LADSPA_IS_HINT_SAMPLE_RATE(lPortRangeHint.HintDescriptor) then
        begin
          lMinValue := lPortParameter.LowerBound * GSettings.SampleRate;
        end
        else
        begin
          lMinValue := lPortRangeHint.LowerBound;
        end;
      end;

      lMaxValue := 1;
      if LADSPA_IS_HINT_BOUNDED_ABOVE(lPortRangeHint.HintDescriptor) then
      begin
        if LADSPA_IS_HINT_SAMPLE_RATE(lPortRangeHint.HintDescriptor) then
        begin
          lMaxValue := lPortParameter.UpperBound * GSettings.SampleRate;
        end
        else
        begin
          lMaxValue := lPortRangeHint.UpperBound;
        end;
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
    				lDefaultValue := exp(ln(lMinValue) * 0.75 + ln(lMaxValue) * 0.25);
          end
          else
          begin
    				lDefaultValue := (lMinValue * 0.75 + lMaxValue * 0.25);
    			end;
    		end;
    		LADSPA_HINT_DEFAULT_MIDDLE:
        begin
          if LADSPA_IS_HINT_LOGARITHMIC(lPortRangeHint.HintDescriptor) then
          begin
    				lDefaultValue := sqrt(lMinValue * lMaxValue);
    			end
          else
          begin
    				lDefaultValue := (lMinValue + lMaxValue) * 0.5;
    			end;
        end;
    		LADSPA_HINT_DEFAULT_HIGH:
        begin
          if LADSPA_IS_HINT_LOGARITHMIC(lPortRangeHint.HintDescriptor) then
          begin
      			lDefaultValue := exp(ln(lMinValue) * 0.25 + ln(lMaxValue) * 0.75);
          end
          else
          begin
    				lDefaultValue := (lMinValue * 0.25 + lMaxValue * 0.75);
          end;
        end;
    		LADSPA_HINT_DEFAULT_MAXIMUM:
    		begin
          lDefaultValue := lMaxValue;
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
      lPortParameter.LowerBound := lMinValue;
      lPortParameter.UpperBound := lMaxValue;
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
begin
  //
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

procedure TPluginNode.Clear;
var
  i: Integer;
begin
  for i := 0 to Pred(FFrames) do
  begin
    FInputBuffer[i] := 0;
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

end.

