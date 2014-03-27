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

  pluginhost.pas
}

unit pluginhost;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ContNrs, globalconst, global_command, global, plugin, utils,
  sampler;

type
  { TPluginManager }

  TPluginManager = class(TObject)
  private
    FPluginList: TObjectList;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure Discover;
  published
    property PluginList: TObjectList read FPluginList;
  end;

  { TPluginProcessor }

  TPluginProcessor = class(THybridPersistentModel)
  private
    FBuffer: PSingle;
    FEnabled: Boolean;
    FNodeList: TObjectList;
    FFrames: Integer;
    FChannels: Integer;
    FPopulateAutomationDevices: TPopulateAutomationDevices;
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
    procedure SortPlugins;
  public
    constructor Create(AFrames: Integer; AObjectOwner: string; AMapped: Boolean = True); reintroduce;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
      AOutputBuffer: PSingle; AFrames: Integer);
    procedure InsertNode(ANode: TPluginNode);
    procedure RemoveNode(ANode: TPluginNode);
    property Buffer: PSingle read FBuffer write FBuffer;
    property OnPopulateAutomationDevices: TPopulateAutomationDevices
      read FPopulateAutomationDevices write FPopulateAutomationDevices;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property NodeList: TObjectList read FNodeList write FNodeList;
    property Frames: Integer read FFrames write FFrames;
  end;

  { TPluginProcessorCommand }

  TPluginProcessorCommand = class(TCommand)
  private
    FPluginProcessor: TPluginProcessor;
  protected
    procedure Initialize; override;
  end;

  { TDeleteNodesCommand }

  TDeleteNodesCommand = class(TPluginProcessorCommand)
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  end;

  { TInsertNodeCommand }

  TInsertNodeCommand = class(TPluginProcessorCommand)
  private
    FSequenceID: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  public
    property SequenceID: string read FSequenceID write FSequenceID;
  end;

  { TCreateNodesCommand }

  TCreateNodesCommand = class(TPluginProcessorCommand)
  private
    FUniqueId: Integer;
    FSequenceNr: Integer;
    FPluginName: string;
    FPluginType: TPluginType;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property SequenceNr: Integer read FSequenceNr write FSequenceNr;
    property PluginName: string read FPluginName write FPluginName;
    property UniqueId: Integer read FUniqueId write FUniqueId;
    property PluginType: TPluginType read FPluginType write FPluginType;
  end;

  { TOrderNodesCommand }

  TOrderNodesCommand = class(TPluginProcessorCommand)
  private
    FPluginOrderList: string;
    FOldPluginOrderList: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property PluginOrderList: string read FPluginOrderList write FPluginOrderList;
  end;

implementation

uses
  plugin_distortion,
  plugin_decimate,
  plugin_moog,
  plugin_freeverb,
  plugin_bassline;

function SortOnSequenceNr(Item1 : Pointer; Item2 : Pointer) : Integer;
var
  lSequenceNr1, lSequenceNr2 : TPluginNode;
begin
  // We start by viewing the object pointers as TPluginNode objects
  lSequenceNr1 := TPluginNode(Item1);
  lSequenceNr2 := TPluginNode(Item2);

  // Now compare by sequencenr
  if lSequenceNr1.SequenceNr > lSequenceNr2.SequenceNr then
    Result := 1
  else if lSequenceNr1.SequenceNr = lSequenceNr2.SequenceNr then
    Result := 0
  else
    Result := -1;
end;

{ TOrderNodesCommand }

procedure TOrderNodesCommand.DoExecute;
var
  lOrderList: TStringList;
  lOldOrderList: TStringList;
  lIndex: Integer;
  lIndexInt: Integer;
  lPlugin: TPluginNode;
begin
  lOldOrderList := TStringList.Create;
  try
    for lIndex := 0 to Pred(FPluginProcessor.NodeList.Count) do
    begin
      lPlugin := TPluginNode(FPluginProcessor.NodeList[lIndex]);

      lOldOrderList.Values[lPlugin.ObjectID] := IntToStr(lPlugin.SequenceNr);
    end;

    FOldPluginOrderList := lOldOrderList.Text;
  finally
    lOldOrderList.Free;
  end;

  lOrderList := TStringList.Create;
  try
    lOrderList.Text := FPluginOrderList;
    for lIndex := 0 to Pred(lOrderList.Count) do
    begin
      for lIndexInt := 0 to Pred(FPluginProcessor.NodeList.Count) do
      begin
        lPlugin := TPluginNode(FPluginProcessor.NodeList[lIndexInt]);
        if lOrderList.Names[lIndex] = lPlugin.ObjectID then
        begin
          lPlugin.SequenceNr := StrToInt(lOrderList.Values[lOrderList.Names[lIndex]]);
          break;
        end;
      end;
    end;
  finally
    lOrderList.Free;
  end;

  FPluginProcessor.SortPlugins;

  FPluginProcessor.Notify;
end;

procedure TOrderNodesCommand.DoRollback;
var
  lOldOrderList: TStringList;
  lIndex: Integer;
  lIndexInt: Integer;
  lPlugin: TPluginNode;
begin
  lOldOrderList := TStringList.Create;
  try
    lOldOrderList.Text := FOldPluginOrderList;
    for lIndex := 0 to Pred(lOldOrderList.Count) do
    begin
      for lIndexInt := 0 to Pred(FPluginProcessor.NodeList.Count) do
      begin
        lPlugin := TPluginNode(FPluginProcessor.NodeList[lIndexInt]);
        if lOldOrderList.Names[lIndex] = lPlugin.ObjectID then
        begin
          lPlugin.SequenceNr :=
            StrToInt(lOldOrderList.Values[lOldOrderList.Names[lIndex]]);
        end;
      end;
    end;
  finally
    lOldOrderList.Free;
  end;

  FPluginProcessor.SortPlugins;

  FPluginProcessor.Notify;
end;

{ TInsertNodeCommand }

procedure TInsertNodeCommand.DoExecute;
begin
  //
end;

procedure TInsertNodeCommand.DoRollback;
begin
  //
end;

procedure TPluginProcessor.DoCreateInstance(var AObject: TObject; AClassName: string);
var
  lPluginNode: TPluginNode;

  procedure AddPluginToNodeList(APluginNode: TPluginNode);
  begin
    AObject := APluginNode;

    FNodeList.Add(APluginNode);

    APluginNode.OnPopulateAutomationDevices := FPopulateAutomationDevices;
    APluginNode.ObjectOwnerID := ObjectID;
  end;

begin
  DBLog('start TPluginProcessor.DoCreateInstance');

  if AClassName = 'TScriptNode' then
  begin
    lPluginNode := TScriptNode.Create(ObjectID, MAPPED);
  end
  else if AClassName = 'TPluginLADSPA' then
  begin
    lPluginNode := TPluginLADSPA.Create(ObjectID, MAPPED);
  end
  else if AClassName = 'TExternalNode' then
  begin
    lPluginNode := TExternalNode.Create(ObjectID);
  end
  else if AClassName = 'TPluginBassline' then
  begin
    lPluginNode := TPluginBassline.Create(ObjectID, MAPPED);
  end
  else if AClassName = 'TSampleBank' then
  begin
    lPluginNode := TPluginBassline.Create(ObjectID, MAPPED);
  end
  else
  begin
    raise Exception.CreateFmt(
      'PluginProcessor: Error loading unknown plugin format',
      [AClassName]);
  end;

  if Assigned(lPluginNode) then
  begin
    AddPluginToNodeList(lPluginNode);
    SortPlugins;
  end;

  DBLog('end TPluginProcessor.DoCreateInstance');
end;

{
  Sort plugins based on their SequenceNr which is the plugins' location in the
  signal chain.
}
procedure TPluginProcessor.SortPlugins;
begin
  FNodeList.Sort(@SortOnSequenceNr);
end;

{ TPluginProcessor }

constructor TPluginProcessor.Create(AFrames: Integer; AObjectOwner: string; AMapped: Boolean = True);
begin
  DBLog('start TPluginProcessor.Create');

  inherited Create(AObjectOwner, AMapped);

  FOnCreateInstanceCallback := @DoCreateInstance;

  FNodeList := TObjectList.create(False);

  FFrames := AFrames;
  FChannels := 2;
  FBuffer := GetMem(FFrames * SizeOf(Single) * FChannels);
  FEnabled := True;

  DBLog('end TPluginProcessor.Create');
end;

destructor TPluginProcessor.Destroy;
begin
  DBLog('start TPluginProcessor.Destroy');

  FreeMem(FBuffer);

  FNodeList.Free;

  inherited Destroy;

  DBLog('end TPluginProcessor.Destroy');
end;

procedure TPluginProcessor.Initialize;
var
  lIndex: Integer;
begin
  for lIndex := 0 to Pred(FNodeList.Count) do
  begin
    TPluginNode(FNodeList[lIndex]).Instantiate;
    TPluginNode(FNodeList[lIndex]).Activate;
  end;
end;

procedure TPluginProcessor.Finalize;
begin
  //
end;

procedure TPluginProcessor.Process(AMidiBuffer: TMidiBuffer;
  AInputBuffer: PSingle; AOutputBuffer: PSingle; AFrames: Integer);
var
  lIndex: Integer;
  lPreviousOutputBuffer: PSingle;
begin
  if FEnabled then
  begin
    if FNodeList.Count > 0 then
    begin
      // Process the complete chain
      for lIndex := 0 to Pred(FNodeList.Count) do
      begin
        if TPluginNode(FNodeList[lIndex]).Active then
        begin
          if lIndex = 0 then
          begin
            TPluginNode(FNodeList[lIndex]).Process(
              AMidiBuffer,
              AInputBuffer,
              TPluginNode(FNodeList[lIndex]).OutputBuffer,
              AFrames);
          end
          else
          begin
            TPluginNode(FNodeList[lIndex]).Process(
              AMidiBuffer,
              lPreviousOutputBuffer,
              TPluginNode(FNodeList[lIndex]).OutputBuffer,
              AFrames);
          end;

          lPreviousOutputBuffer := TPluginNode(FNodeList[lIndex]).OutputBuffer;
        end;
      end;

      // Pull audio from pluginchain
      Move(TPluginNode(FNodeList.Last).OutputBuffer^, AOutputBuffer^, AFrames * sizeof(single) * FChannels);
    end;
  end
  else
  begin
    FillByte(AOutputBuffer^, AFrames * SizeOf(Single) * STEREO, 0);
  end;
end;

procedure TPluginProcessor.InsertNode(ANode: TPluginNode);
begin
  DBLog('start TPluginProcessor.InsertNode');

  BeginUpdate;

  FNodeList.Add(ANode);

  EndUpdate;

  DBLog('end TPluginProcessor.InsertNode');
end;

procedure TPluginProcessor.RemoveNode(ANode: TPluginNode);
begin
  DBLog('start TPluginProcessor.RemoveNode');

  BeginUpdate;

  FNodeList.Extract(ANode);

  EndUpdate;

  DBLog('end TPluginProcessor.RemoveNode');
end;

{ TDeleteNodesCommand }

procedure TDeleteNodesCommand.DoExecute;
var
  i: Integer;
  lPluginNode: TPluginNode;
  lMementoNode: TMementoNode;
begin
  DBLog('start TDeleteNodesCommand.DoExecute');

  FPluginProcessor.Enabled := False;

  FPluginProcessor.BeginUpdate;

  for i := Pred(FPluginProcessor.NodeList.Count) downto 0 do
  begin
    lPluginNode := TPluginNode(FPluginProcessor.NodeList[i]);

    if lPluginNode.ObjectID = ObjectID then
    begin
      lMementoNode := TMementoNode.Create(FPluginProcessor.ObjectID);
      lMementoNode.ObjectID := lPluginNode.ObjectID;
      lMementoNode.ObjectOwnerID := lPluginNode.ObjectOwnerID;

      lPluginNode.Active := False;
      lPluginNode.Deactivate;
      lPluginNode.Clean;

      FPluginProcessor.NodeList.Remove(lPluginNode);
    end;
  end;

  FPluginProcessor.SortPlugins;

  FPluginProcessor.EndUpdate;

  FPluginProcessor.Enabled := True;

  DBLog('end TDeleteNodesCommand.DoExecute');
end;

procedure TDeleteNodesCommand.DoRollback;
var
  i: integer;
  lPluginNode: TPluginNode;
  lMementoNode: TMementoNode;
begin
  DBLog('start TDeleteNodesCommand.DoRollback');

  if Memento.Count > 0 then
  begin
    FPluginProcessor.BeginUpdate;

    for i := 0 to Pred(Memento.Count) do
    begin
      lMementoNode := TMementoNode(Memento[i]);
      lPluginNode := TPluginNode.Create(FPluginProcessor.ObjectID, NOT_MAPPED);
      lPluginNode.ObjectID := lMementoNode.ObjectID;
      lPluginNode.ObjectOwnerID := lMementoNode.ObjectOwnerID;

      FPluginProcessor.NodeList.Add(lPluginNode);
    end;

    FPluginProcessor.SortPlugins;

    FPluginProcessor.EndUpdate;
  end;

  DBLog('end TDeleteNodesCommand.DoRollback');
end;

{ TCreateNodesCommand }

procedure TCreateNodesCommand.DoExecute;
var
  lPluginDistortion: TPluginDistortion;
  lPluginFreeverb: TPluginFreeverb;
  lPluginBassline: TPluginBassline;
  lPluginDecimate: TPluginDecimate;
  lPluginLadspa: TPluginLADSPA;
  lSampleBank: TSampleBank;

  procedure AddPluginToNodeList(APluginNode: TPluginNode);
  begin
    APluginNode.OnPopulateAutomationDevices := FPluginProcessor.OnPopulateAutomationDevices;

    FPluginProcessor.NodeList.Add(APluginNode);
    APluginNode.Instantiate;
    APluginNode.Activate;
  end;

begin
  DBLog('start TCreateNodesCommand.DoExecute');

  FPluginProcessor.BeginUpdate;

  case FPluginType of
    ptDistortion:
    begin
      lPluginDistortion := TPluginDistortion.Create(FPluginProcessor.ObjectID, MAPPED);
      lPluginDistortion.PluginName := FPluginName;
      lPluginDistortion.PluginType := ptDistortion;
      lPluginDistortion.SequenceNr := FPluginProcessor.NodeList.Count;

      AddPluginToNodeList(lPluginDistortion);

      ObjectIdList.Add(lPluginDistortion.ObjectID);
    end;
    ptSampler:
    begin
      lSampleBank := TSampleBank.Create(FPluginProcessor.ObjectID, MAPPED);
      lSampleBank.PluginName := 'Sampler';
      lSampleBank.PluginType := ptSampler;
      lSampleBank.SequenceNr := FPluginProcessor.NodeList.Count;

      AddPluginToNodeList(lSampleBank);

      ObjectIdList.Add(lSampleBank.ObjectID);
    end;
    ptReverb:
    begin
      lPluginFreeverb := TPluginFreeverb.Create(FPluginProcessor.ObjectID, MAPPED);
      lPluginFreeverb.PluginName := FPluginName;
      lPluginFreeverb.PluginType := ptReverb;
      lPluginFreeverb.SequenceNr := FPluginProcessor.NodeList.Count;

      AddPluginToNodeList(lPluginFreeverb);

      ObjectIdList.Add(lPluginFreeverb.ObjectID);
    end;
    ptBassline:
    begin
      lPluginBassline := TPluginBassline.Create(FPluginProcessor.ObjectID, MAPPED);
      lPluginBassline.PluginType := ptBassline;
      lPluginBassline.SequenceNr := FPluginProcessor.NodeList.Count;

      AddPluginToNodeList(lPluginBassline);

      ObjectIdList.Add(lPluginBassline.ObjectID);
    end;
    ptDecimate:
    begin
      lPluginDecimate := TPluginDecimate.Create(FPluginProcessor.ObjectID, MAPPED);
      lPluginDecimate.PluginName := FPluginName;
      lPluginDecimate.PluginType := ptDecimate;
      lPluginDecimate.SequenceNr := FPluginProcessor.NodeList.Count;

      AddPluginToNodeList(lPluginDecimate);

      ObjectIdList.Add(lPluginDecimate.ObjectID);
    end;
    ptLADSPA:
    begin
      lPluginLadspa := TPluginLADSPA.Create(FPluginProcessor.ObjectID, MAPPED);
      lPluginLadspa.PluginName := FPluginName;
      lPluginLadspa.PluginType := ptLADSPA;
      lPluginLadspa.SequenceNr := FPluginProcessor.NodeList.Count;
      lPluginLadspa.UniqueID := FUniqueId;

      ObjectIdList.Add(lPluginLadspa.ObjectID);

      AddPluginToNodeList(lPluginLadspa);
    end;
  end;

  FPluginProcessor.SortPlugins;

  FPluginProcessor.EndUpdate;

  DBLog('end TCreateNodesCommand.DoExecute');
end;

procedure TCreateNodesCommand.DoRollback;
var
  lPluginNode: TPluginNode;
  lMementoIndex, lNodeIndex: Integer;
begin
  DBLog('start TCreateNodesCommand.DoRollback');

  FPluginProcessor.Enabled := False;

  FPluginProcessor.BeginUpdate;

  for lMementoIndex := 0 to Pred(ObjectIdList.Count) do
  begin
    for lNodeIndex := Pred(FPluginProcessor.NodeList.Count) downto 0 do
    begin
      lPluginNode := TPluginNode(FPluginProcessor.NodeList[lNodeIndex]);
      if ObjectIdList[lMementoIndex] = lPluginNode.ObjectID then
      begin
        lPluginNode.Active := False;
        lPluginNode.Deactivate;
        lPluginNode.Clean;

        FPluginProcessor.NodeList.Remove(lPluginNode);
      end;
    end;
  end;

  FPluginProcessor.SortPlugins;

  FPluginProcessor.EndUpdate;

  FPluginProcessor.Enabled := True;

  DBLog('end TCreateNodesCommand.DoRollback');
end;

{ TPluginManager }

constructor TPluginManager.Create;
begin
  FPluginList := TObjectList.create(True);
end;

destructor TPluginManager.Destroy;
begin
  FPluginList.Free;

  inherited Destroy;
end;

procedure TPluginManager.Discover;
begin
  // Fill a list with all discovered plugins on the system

  // Discover Ladspa plugins and add to list

  // Discover LV2 plugins and add to list

  // Discover DSSI plugins and add to list

  // Discover Internal plugins and add to list (hardcoded in source)

end;

{ TPluginProcessorCommand }

procedure TPluginProcessorCommand.Initialize;
begin
  FPluginProcessor := TPluginProcessor(GObjectMapper.GetModelObject(ObjectOwner));
end;

end.

