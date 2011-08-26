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
  Classes, SysUtils, ContNrs, globalconst, global_command, global, plugin, utils;

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

  { TInterConnect }

  TInterConnect = class(THybridPersistentModel)
  private
    FToPluginNode: string;
    FToPluginNodePort: string;
    FFromPluginNode: string;
    FFromPluginNodePort: string;
    function GetIsReady: Boolean;
  public
    procedure Initialize; override;
    procedure Finalize; override;
    property IsReady: Boolean read GetIsReady;
  published
    // Connection outputs audio or midi to pluginnode input (parent)
    property ToPluginNode: string read FToPluginNode write FToPluginNode;
    property ToPluginNodePort: string read FToPluginNodePort write FToPluginNodePort;
    // Connection input takes audio or midi from pluginnode output (child)
    property FromPluginNode: string read FFromPluginNode write FFromPluginNode;
    property FromPluginNodePort: string read FFromPluginNodePort write FFromPluginNodePort;
  end;

  { TPluginProcessor }

  TPluginProcessor = class(THybridPersistentModel)
  private
    FAudioOut: TPluginAudioOut;
    FAudioIn: TPluginAudioIn;
    FConnectionList: TObjectList;
    FBuffer: PSingle;
    FEnabled: Boolean;
    FNodeList: TObjectList;
    FFrames: Integer;
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
  public
    constructor Create(AFrames: Integer; AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Initialize; override;
    function Execute(AFrames: Integer; ABuffer: PSingle): PSingle;
    procedure AddChild(AParentNode, ANode: TPluginNode);
    procedure InsertNode(ANode, AParentNode, AChildNode: TPluginNode);
    procedure RemoveNode(ANode, AParentNode: TPluginNode);
    function FindNodeByID(ANodeID: string): TPluginNode;
    function FindConnection(APluginId1, APluginId2: TPluginNode): TInterConnect;
    procedure Clear;
    property Buffer: PSingle read FBuffer write FBuffer;
    property AudioOut: TPluginAudioOut read FAudioOut write FAudioOut;
    property AudioIn: TPluginAudioIn read FAudioIn write FAudioIn;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property NodeList: TObjectList read FNodeList write FNodeList;
    property ConnectionList: TObjectList read FConnectionList write FConnectionList;
    property Frames: Integer read FFrames write FFrames;
  end;

  { TPluginPluginProcessorCommand }

  TPluginPluginProcessorCommand = class(TCommand)
  private
    FPluginProcessor: TPluginProcessor;
  protected
    procedure Initialize; override;
  end;

  { TDeleteNodesCommand }

  TDeleteNodesCommand = class(TPluginPluginProcessorCommand)
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  end;

  { TCreateNodesCommand }

  TCreateNodesCommand = class(TPluginPluginProcessorCommand)
  private
    FXLocation: Integer;
    FYLocation: Integer;
    FPluginName: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property XLocation: Integer read FXLocation write FXLocation;
    property YLocation: Integer read FYLocation write FYLocation;
    property PluginName: string read FPluginName write FPluginName;
  end;

  { TCreateConnectionCommand }

  TCreateConnectionCommand = class(TPluginPluginProcessorCommand)
  private
    FFromPluginNode: string;
    FToPluginNode: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  public
    property FromPluginNode: string read FFromPluginNode write FFromPluginNode;
    property ToPluginNode: string read FToPluginNode write FToPluginNode;
  end;

  { TDeleteConnectionCommand }

  TDeleteConnectionCommand = class(TPluginPluginProcessorCommand)
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  end;

implementation

procedure TPluginProcessor.DoCreateInstance(var AObject: TObject; AClassName: string);
var
  lPluginNode: TPluginNode;
  lInterConnect: TInterConnect;
begin
  DBLog('start TPluginProcessor.DoCreateInstance');

  writeln('Construction: ' + AClassName);

  if AClassName = 'TScriptPlugin' then
  begin
    lPluginNode := TScriptPlugin.Create(ObjectID, MAPPED);
    lPluginNode.ObjectOwnerID := ObjectID;
    NodeList.Add(lPluginNode);
    AObject := lPluginNode;
  end
  else if AClassName = 'TLADSPAPlugin' then
  begin
    lPluginNode := TLADSPAPlugin.Create(ObjectID, MAPPED);
    lPluginNode.ObjectOwnerID := ObjectID;
    NodeList.Add(lPluginNode);
    AObject := lPluginNode;
  end
  else if AClassName = 'TPluginExternal' then
  begin
    lPluginNode := TPluginExternal.Create(ObjectID);
    lPluginNode.ObjectOwnerID := ObjectID;
    NodeList.Add(lPluginNode);
    AObject := lPluginNode;
  end
  else if AClassName = 'TInterConnect' then
  begin
    lInterConnect := TInterConnect.Create(ObjectID, MAPPED);
    lInterConnect.ObjectOwnerID := ObjectID;
    ConnectionList.Add(lInterConnect);
    AObject := lInterConnect;
  end
  else
  begin
    // Should raise some error/exception or just let go?..
  end;

  DBLog('end TPluginProcessor.DoCreateInstance');
end;

{ TPluginProcessor }

constructor TPluginProcessor.Create(AFrames: Integer; AObjectOwner: string; AMapped: Boolean = True);
begin
  DBLog('start TPluginProcessor.Create');

  inherited Create(AObjectOwner, AMapped);

  FOnCreateInstanceCallback := @DoCreateInstance;

  FNodeList := TObjectList.create(False);
  FConnectionList := TObjectList.create(True);

  FFrames := AFrames;

  FAudioOut := TPluginAudioOut.Create(ObjectID);
  FAudioOut.PluginName := 'AudioOut';

  FAudioIn := TPluginAudioIn.Create(ObjectID);
  FAudioIn.PluginName := 'AudioIn';

  FBuffer := GetMem(FFrames * SizeOf(Single));

  AddChild(FAudioOut, FAudioIn);

  DBLog('end TPluginProcessor.Create');
end;

destructor TPluginProcessor.Destroy;
begin
  DBLog('start TPluginProcessor.Destroy');

  FAudioOut.Free;
  FAudioIn.Free;
  FreeMem(FBuffer);

  FNodeList.Free;
  FConnectionList.Free;

  inherited Destroy;

  DBLog('end TPluginProcessor.Destroy');
end;

procedure TPluginProcessor.Initialize;
var
  lNodeIndex: Integer;
begin
  BeginUpdate;

  writeln('Should connect all nodes here!');

  // First search node which is not a child of another node
  for lNodeIndex := 0 to Pred(FNodeList.Count) do
  begin
    //for lChildIndex := 0 to Pred(FNodeList.Count) do
    writeln(TPluginNode(FNodeList[lNodeIndex]).PluginName + ' Childs ' + IntTostr(TPluginNode(FNodeList[lNodeIndex]).Childs.Count));
  end;

  EndUpdate;
end;

function TPluginProcessor.Execute(AFrames: Integer; ABuffer: PSingle): PSingle;
begin
  Move(ABuffer[0], FAudioIn.Buffer[0], AFrames * sizeof(single));

  Result := FAudioOut.Execute(AFrames);

  Move(FAudioOut.MixBuffer[0], FBuffer[0], AFrames * sizeof(single));
end;

procedure TPluginProcessor.AddChild(AParentNode, ANode: TPluginNode);
var
  lInterConnect: TInterConnect;
begin
  DBLog('start TPluginProcessor.AddChild');

  BeginUpdate;

  AParentNode.Childs.Add(ANode);

  lInterConnect := TInterConnect.Create(Self.ObjectID, MAPPED);
  lInterConnect.FromPluginNode := ANode.ObjectID;
  lInterConnect.ToPluginNode := AParentNode.ObjectID;
  FConnectionList.Add(lInterConnect);

  EndUpdate;

  DBLog('end TPluginProcessor.AddChild');
end;

procedure TPluginProcessor.InsertNode(ANode, AParentNode, AChildNode: TPluginNode);
var
  lInterConnect: TInterConnect;
  lFoundConnection: TInterConnect;
begin
  DBLog('start TPluginProcessor.InsertNode');

  BeginUpdate;

  DBLog(Format('Disconnect %s from %s', [AChildNode.ClassName, AParentNode.ClassName]));
  AParentNode.Childs.Extract(AChildNode);

  DBLog(Format('Add %s to %s', [ANode.ClassName, AParentNode.ClassName]));
  AParentNode.Childs.Add(ANode);

  DBLog(Format('Connect %s to %s', [AChildNode.ClassName, ANode.ClassName]));
  ANode.Childs.Add(AChildNode);

  FNodeList.Add(ANode);

  lFoundConnection := FindConnection(AParentNode, AChildNode);
  if Assigned(lFoundConnection) then
  begin
    lFoundConnection.ToPluginNode := AParentNode.ObjectID;
    lFoundConnection.FromPluginNode := ANode.ObjectID;
  end;

  lInterConnect := TInterConnect.Create(Self.ObjectID, MAPPED);
  lInterConnect.ToPluginNode := ANode.ObjectID;
  lInterConnect.FromPluginNode := AChildNode.ObjectID;
  FConnectionList.Add(lInterConnect);

  EndUpdate;

  DBLog('end TPluginProcessor.InsertNode');
end;

procedure TPluginProcessor.RemoveNode(ANode, AParentNode: TPluginNode);
var
  lChildIndex: Integer;
begin
  DBLog('start TPluginProcessor.RemoveNode');

  BeginUpdate;

  for lChildIndex := 0 to Pred(ANode.Childs.Count) do
  begin
    AParentNode.Childs.Add(ANode.Childs[lChildIndex]);
  end;

  for lChildIndex := 0 to Pred(ANode.Childs.Count) do
  begin
    ANode.Childs.Remove(ANode);
  end;

  FNodeList.Extract(ANode);

  EndUpdate;

  DBLog('end TPluginProcessor.RemoveNode');
end;

function TPluginProcessor.FindNodeByID(ANodeID: string): TPluginNode;
begin
  // traverse tree to find node, simple and small structure so no performance problems
  Result := nil;
end;

function TPluginProcessor.FindConnection(APluginId1, APluginId2: TPluginNode
  ): TInterConnect;
var
  i: Integer;
begin
  Result := nil;

  // First try left => right connected
  for i := 0 to Pred(FConnectionList.Count) do
  begin
    if (TInterConnect(FConnectionList[i]).FromPluginNode = APluginId1.ObjectID) and
        (TInterConnect(FConnectionList[i]).ToPluginNode = APluginId2.ObjectID) then
    begin
      Result := TInterConnect(FConnectionList[i]);
    end;
  end;

  // Not found now try left <= right connected
  if not Assigned(Result) then
  begin
    for i := 0 to Pred(FConnectionList.Count) do
    begin
      if (TInterConnect(FConnectionList[i]).FromPluginNode = APluginId2.ObjectID) and
          (TInterConnect(FConnectionList[i]).ToPluginNode = APluginId1.ObjectID) then
      begin
        Result := TInterConnect(FConnectionList[i]);
      end;
    end;
  end;
end;

procedure TPluginProcessor.Clear;
begin
  FAudioOut.ApplyToAll(@FAudioOut.Clear);
end;

{ TDeleteNodesCommand }

procedure TDeleteNodesCommand.DoExecute;
var
  i: Integer;
  lPluginNode: TPluginNode;
  lMementoNode: TMementoPlugin;
begin
  DBLog('start TDeleteNodesCommand.DoExecute');

  FPluginProcessor.BeginUpdate;

  for i := Pred(FPluginProcessor.NodeList.Count) downto 0 do
  begin
    lPluginNode := TPluginNode(FPluginProcessor.NodeList[i]);

    if lPluginNode.Selected or (lPluginNode.ObjectID = ObjectID) then
    begin
      lMementoNode := TMementoPlugin.Create(FPluginProcessor.ObjectID);
      lMementoNode.ObjectID := lPluginNode.ObjectID;
      lMementoNode.ObjectOwnerID := lPluginNode.ObjectOwnerID;
    end;
  end;

  FPluginProcessor.EndUpdate;

  DBLog('end TDeleteNodesCommand.DoExecute');
end;

procedure TDeleteNodesCommand.DoRollback;
var
  i: integer;
  lPluginNode: TPluginNode;
  lMementoNode: TMementoPlugin;
begin
  DBLog('start TDeleteNodesCommand.DoRollback');

  if Memento.Count > 0 then
  begin
    FPluginProcessor.BeginUpdate;

    for i := 0 to Pred(Memento.Count) do
    begin
      lMementoNode := TMementoPlugin(Memento[i]);
      lPluginNode := TPluginNode.Create(FPluginProcessor.ObjectID, NOT_MAPPED);
      lPluginNode.ObjectID := lMementoNode.ObjectID;
      lPluginNode.ObjectOwnerID := lMementoNode.ObjectOwnerID;

      FPluginProcessor.NodeList.Add(lPluginNode);
    end;

    FPluginProcessor.EndUpdate;
  end;

  DBLog('end TDeleteNodesCommand.DoRollback');
end;

{ TCreateNodesCommand }

procedure TCreateNodesCommand.DoExecute;
var
  lPluginNode: TPluginNode;
begin
  DBLog('start TCreateNodesCommand.DoExecute');

  FPluginProcessor.BeginUpdate;

  lPluginNode := TPluginNode.Create(FPluginProcessor.ObjectID, MAPPED);
  lPluginNode.PluginName := FPluginName;
  lPluginNode.XLocation := XLocation;
  lPluginNode.YLocation := YLocation;

  FPluginProcessor.NodeList.Add(lPluginNode);

  ObjectIdList.Add(lPluginNode.ObjectID);

  FPluginProcessor.EndUpdate;

  DBLog('end TCreateNodesCommand.DoExecute');
end;

procedure TCreateNodesCommand.DoRollback;
var
  lPluginNode: TPluginNode;
  lMementoIndex, lNodeIndex: Integer;
begin
  DBLog('start TCreateNodesCommand.DoRollback');

  FPluginProcessor.BeginUpdate;

  for lMementoIndex := 0 to Pred(ObjectIdList.Count) do
  begin
    for lNodeIndex := Pred(FPluginProcessor.NodeList.Count) downto 0 do
    begin
      lPluginNode := TPluginNode(FPluginProcessor.NodeList[lNodeIndex]);
      if ObjectIdList[lMementoIndex] = lPluginNode.ObjectID then
      begin
        FPluginProcessor.NodeList.Remove(lPluginNode);
      end;
    end;
  end;

  FPluginProcessor.EndUpdate;

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

{ TPluginPluginProcessorCommand }

procedure TPluginPluginProcessorCommand.Initialize;
begin
  FPluginProcessor := TPluginProcessor(GObjectMapper.GetModelObject(ObjectOwner));
end;

{ TCreateConnectionCommand }

procedure TCreateConnectionCommand.DoExecute;
var
  lInterConnect: TInterConnect;
  lParent, lChild: TPluginNode;
begin
  DBLog('start TCreateConnectionCommand.DoExecute');

  FPluginProcessor.BeginUpdate;

  lInterConnect := TInterConnect.Create(FPluginProcessor.ObjectID, MAPPED);
  lInterConnect.FromPluginNode := FFromPluginNode;
  lInterConnect.ToPluginNode := FToPluginNode;
  ObjectID := lInterConnect.ObjectID;

  FPluginProcessor.ConnectionList.Add(lInterConnect);

  lChild := TPluginNode(GObjectMapper.GetModelObject(FFromPluginNode));
  lParent := TPluginNode(GObjectMapper.GetModelObject(FToPluginNode));

  if Assigned(lParent) and Assigned(lChild) then
  begin
    DBLog(Format('Connect child %s (%s) to parent %s (%s)',
      [lChild.PluginName, lChild.ObjectID, lParent.PluginName, lParent.ObjectID]));

    lParent.Childs.Add(lChild);
  end;

  FPluginProcessor.EndUpdate;

  DBLog('end TCreateConnectionCommand.DoExecute');
end;

procedure TCreateConnectionCommand.DoRollback;
var
  lInterConnect: TInterConnect;
  lConnectionIndex: Integer;
  lParent, lChild: TPluginNode;
begin
  DBLog('start TCreateConnectionCommand.DoRollback');

  FPluginProcessor.BeginUpdate;

  // Remove plugin audio connection
  lParent := TPluginNode(GObjectMapper.GetModelObject(FromPluginNode));
  lChild := TPluginNode(GObjectMapper.GetModelObject(FToPluginNode));
  if Assigned(lParent) and Assigned(lChild) then
  begin
    lParent.Childs.Extract(lChild);
  end;

  for lConnectionIndex := Pred(FPluginProcessor.ConnectionList.Count) downto 0 do
  begin
    lInterConnect := TInterConnect(FPluginProcessor.ConnectionList[lConnectionIndex]);
    if lInterConnect.ObjectID = ObjectID then
    begin
      FPluginProcessor.ConnectionList.Remove(lInterConnect);
      break;
    end;
  end;

  FPluginProcessor.EndUpdate;

  DBLog('end TCreateConnectionCommand.DoRollback');
end;

{ TDeleteConnectionCommand }

procedure TDeleteConnectionCommand.DoExecute;
begin
  //
end;

procedure TDeleteConnectionCommand.DoRollback;
begin
  //
end;

{ TInterConnect }

function TInterConnect.GetIsReady: Boolean;
begin
  Result := ((FromPluginNode <> '') and (FromPluginNodePort <> '') and
    (ToPluginNode <> '') and (ToPluginNodePort <> ''));
end;

procedure TInterConnect.Initialize;
begin
  Notify;
end;

procedure TInterConnect.Finalize;
begin
  //
end;

end.

