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

  pluginhostgui.pas
}

unit pluginhostgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, globalconst, global,
  plugin, utils, Controls, LCLType, Graphics, contnrs, pluginhost, plugingui;

type
  TInterConnectPort = record
    PluginID: string;
    Parameter: string;
  end;

  { TInterConnectGUI }

  TInterConnectGUI = class(THybridPersistentView)
  private
    FToPluginNode: string;
    FToPluginNodePort: string;
    FFromPluginNode: string;
    FFromPluginNodePort: string;
    function GetIsReady: Boolean;
  public
    procedure Update(Subject: THybridPersistentModel); reintroduce;
    property IsReady: Boolean read GetIsReady;
    // Connection outputs audio or midi to pluginnode input (parent)
    property ToPluginNode: string read FToPluginNode write FToPluginNode;
    property ToPluginNodePort: string read FToPluginNodePort write FToPluginNodePort;
    // Connection input takes audio or midi from pluginnode output (child)
    property FromPluginNode: string read FFromPluginNode write FFromPluginNode;
    property FromPluginNodePort: string read FFromPluginNodePort write FFromPluginNodePort;

  end;

  { TPluginProcessorGUI }

  TPluginProcessorGUI = class(TFrame, IObserver)
    gbPlugin: TGroupBox;
    sbPluginGraph: TScrollBox;
    procedure sbPluginGraphDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure sbPluginGraphDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    { private declarations }
    FAudioOutGUI: TPluginNodeGUI;
    FAudioInGUI: TPluginNodeGUI;

    FObjectOwnerID: string;
    FObjectID: string;
    FModelObject: TObject;
    FObjectOwner: TObject;
    FModel: TPluginProcessor;

    FNodeListGUI: TObjectList;
    FConnectionListGUI: TObjectList;
    FTempConnection: TInterConnectGUI;
    FBusyConnecting: Boolean;
  protected
    function NodeByObjectID(AObjectID: string): TPluginNodeGUI;
    function NodeByName(APluginName: string): TPluginNodeGUI;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure Connect; virtual;
    procedure CreateNodeGUI(AObjectID: string);
    procedure DeleteNodeGUI(AObjectID: string);
    procedure CreateConnectionGUI(AObjectID: string);
    procedure DeleteConnectionGUI(AObjectID: string);
    procedure DoConnection(AObjectID: string; AParameter: string);
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string; virtual;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property ObjectID: string read GetObjectID write SetObjectID;
    property Model: TPluginProcessor read FModel write FModel;
    property NodeListGUI: TObjectList read FNodeListGUI write FNodeListGUI;
    property ConnectionListGUI: TObjectList read FConnectionListGUI write FConnectionListGUI;
    property ModelObject: TObject read FModelObject write FModelObject;
    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
    property AudioOutGUI: TPluginNodeGUI read FAudioOutGUI write FAudioOutGUI;
    property AudioInGUI: TPluginNodeGUI read FAudioInGUI write FAudioInGUI;
  end;

implementation

uses
  global_command, ComCtrls;

procedure TPluginProcessorGUI.sbPluginGraphDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  lTreeView: TTreeView;
  lCreateNodesCommand: TCreateNodesCommand;
begin
  if Source is TTreeView then
  begin
    lTreeView := TTreeView(Source);
    { TODO Check format
      If Source is pluginname then create plugin command
    }

    lCreateNodesCommand := TCreateNodesCommand.Create(ObjectID);
    try
      lCreateNodesCommand.XLocation := X;
      lCreateNodesCommand.YLocation := Y;
      lCreateNodesCommand.PluginName := lTreeView.Selected.Text;

      GCommandQueue.PushCommand(lCreateNodesCommand);
    except
      lCreateNodesCommand.Free;
    end;
  end;
end;

procedure TPluginProcessorGUI.sbPluginGraphDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

function TPluginProcessorGUI.NodeByObjectID(AObjectID: string): TPluginNodeGUI;
var
  lIndex: Integer;
begin
  Result := nil;

  for lIndex := 0 to Pred(NodeListGUI.Count) do
  begin
    if TPluginNodeGUI(NodeListGUI[lIndex]).ObjectID = AObjectID then
    begin
      Result := TPluginNodeGUI(NodeListGUI[lIndex]);
    end;
  end;
end;

function TPluginProcessorGUI.NodeByName(APluginName: string): TPluginNodeGUI;
var
  lIndex: Integer;
begin
  Result := nil;

  for lIndex := 0 to Pred(NodeListGUI.Count) do
  begin
    if TPluginNodeGUI(NodeListGUI[lIndex]).PluginName = APluginName then
    begin
      Result := TPluginNodeGUI(NodeListGUI[lIndex]);
    end;
  end;
end;

constructor TPluginProcessorGUI.Create(AOwner: TComponent);
begin
  DBLog('start TPluginProcessorGUI.Create');

  inherited Create(AOwner);

  FNodeListGUI := TObjectList.create(True);
  FConnectionListGUI := TObjectList.create(True);
  FTempConnection := TInterConnectGUI.Create(Self.ObjectID);

  FAudioInGUI := TPluginNodeGUI.Create(Self);
  FAudioInGUI.Parent := sbPluginGraph;
  FAudioInGUI.XLocation := 50;
  FAudioInGUI.YLocation := 50;
  FAudioInGUI.OnConnection := @DoConnection;

  FAudioOutGUI := TPluginNodeGUI.Create(Self);
  FAudioOutGUI.Parent := sbPluginGraph;
  FAudioOutGUI.XLocation := sbPluginGraph.Width - 50;
  FAudioOutGUI.YLocation := 50;
  FAudioOutGUI.OnConnection := @DoConnection;

  DBLog('end TPluginProcessorGUI.Create');
end;

destructor TPluginProcessorGUI.Destroy;
begin
  DBLog('start TPluginProcessorGUI.Destroy');

  FTempConnection.Free;
  FNodeListGUI.Free;
  FConnectionListGUI.Free;

  FAudioInGUI.Free;
  FAudioOutGUI.Free;

  inherited Destroy;

  DBLog('end TPluginProcessorGUI.Destroy');
end;

procedure TPluginProcessorGUI.Update(Subject: THybridPersistentModel);
var
  i: Integer;
begin
  DBLog('start TPluginProcessorGUI.Update');

  // Create or Delete pluginnodes
  DBLog('DiffLists NodeListGUI');

  DiffLists(
    TPluginProcessor(Subject).NodeList,
    NodeListGUI,
    @CreateNodeGUI,
    @DeleteNodeGUI);

  // Create or Delete connections between pluginnodes
  DBLog('DiffLists ConnectionListGUI');

  DiffLists(
    TPluginProcessor(Subject).ConnectionList,
    ConnectionListGUI,
    @CreateConnectionGUI,
    @DeleteConnectionGUI);

  DBLog('end TPluginProcessorGUI.Update');
end;

procedure TPluginProcessorGUI.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

procedure TPluginProcessorGUI.Paint;
var
  i: Integer;
  lPoints: array of TPoint;
  lFromPluginNode: TPluginNodeGUI;
  lToPluginNode: TPluginNodeGUI;
  lBitmap: TBitmap;

  function GetObjectFromList(AObjectID: string): TPluginNodeGUI;
  var
    i: Integer;
  begin
    Result := nil;
    if AObjectID = FAudioInGUI.ObjectID then
    begin
      Result := FAudioInGUI;
      exit;
    end;

    if AObjectID = FAudioOutGUI.ObjectID then
    begin
      Result := FAudioOutGUI;
      exit;
    end;

    for i := 0 to Pred(FNodeListGUI.Count) do
    begin
      if AObjectID = TPluginNodeGUI(FNodeListGUI[i]).ObjectID then
      begin
        Result := TPluginNodeGUI(FNodeListGUI[i]);
        break;
      end;
    end;
  end;

begin
  lBitmap := TBitmap.Create;
  try
    lBitmap.Height := Height;
    lBitmap.Width := Width;
    lBitmap.Canvas.Clear;
    lBitmap.Canvas.Pen.Color := clGreen;
    lBitmap.Canvas.Brush.Color := clGradientActiveCaption;
    lBitmap.Canvas.Rectangle(0, 0, lBitmap.Width, lBitmap.Height);

    SetLength(lPoints, 4);
    for i := 0 to Pred(FConnectionListGUI.Count) do
    begin
      lFromPluginNode := GetObjectFromList(TInterConnectGUI(FConnectionListGUI[i]).FromPluginNode);
      lToPluginNode := GetObjectFromList(TInterConnectGUI(FConnectionListGUI[i]).ToPluginNode);

      if Assigned(lFromPluginNode) and Assigned(lToPluginNode) then
      begin
        lPoints[0].X := lFromPluginNode.Left + lFromPluginNode.Width;
        lPoints[0].Y := lFromPluginNode.Top + (lFromPluginNode.Height div 2);
        lPoints[1].X := lFromPluginNode.Left + lFromPluginNode.Width + 100;
        lPoints[1].Y := lFromPluginNode.Top + (lFromPluginNode.Height div 2);
        lPoints[2].X := lToPluginNode.Left - 100;
        lPoints[2].Y := lToPluginNode.Top + (lToPluginNode.Height div 2);
        lPoints[3].X := lToPluginNode.Left;
        lPoints[3].Y := lToPluginNode.Top + (lToPluginNode.Height div 2);
        lBitmap.Canvas.PolyBezier(lPoints);
      end;
    end;

    if FBusyConnecting then
    begin
      if Assigned(FTempConnection) then
      begin
        if (FTempConnection.FromPluginNode <> '') and (FTempConnection.ToPluginNode <> '') then
        begin
          lFromPluginNode := GetObjectFromList(FTempConnection.FromPluginNode);
          lToPluginNode := GetObjectFromList(FTempConnection.ToPluginNode);

          if Assigned(lFromPluginNode) and Assigned(lToPluginNode) then
          begin
            lPoints[0].X := lFromPluginNode.Left + lFromPluginNode.Width;
            lPoints[0].Y := lFromPluginNode.Top + (lFromPluginNode.Height div 2);
            lPoints[1].X := lFromPluginNode.Left + lFromPluginNode.Width + 100;
            lPoints[1].Y := lFromPluginNode.Top + (lFromPluginNode.Height div 2);
            lPoints[2].X := lToPluginNode.Left - 100;
            lPoints[2].Y := lToPluginNode.Top + (lToPluginNode.Height div 2);
            lPoints[3].X := lToPluginNode.Left;
            lPoints[3].Y := lToPluginNode.Top + (lToPluginNode.Height div 2);
            lBitmap.Canvas.PolyBezier(lPoints);
          end;
        end;
      end;
    end;
    sbPluginGraph.Canvas.Draw(0, 0, lBitmap);
  finally
    lBitmap.Free;
  end;

  inherited Paint;
end;


procedure TPluginProcessorGUI.Connect;
begin
  ModelObject := GObjectMapper.GetModelObject(ObjectID);

  TPluginProcessor(ModelObject).AudioIn.Attach(FAudioInGUI);
  FAudioInGUI.ObjectID := TPluginProcessor(Model).AudioIn.ObjectID;
  FAudioInGUI.PluginName := TPluginProcessor(Model).AudioIn.PluginName;

  TPluginProcessor(ModelObject).AudioOut.Attach(FAudioOutGUI);
  FAudioOutGUI.ObjectID := TPluginProcessor(Model).AudioOut.ObjectID;
  FAudioOutGUI.PluginName := TPluginProcessor(Model).AudioOut.PluginName;
end;

procedure TPluginProcessorGUI.CreateNodeGUI(AObjectID: string);
var
  lPluginNode: TPluginNode;
  lPluginNodeGUI: TPluginNodeGUI;
begin
  DBLog('start TPluginProcessorGUI.CreateNodeGUI ' + AObjectID);

  lPluginNode := TPluginNode(GObjectMapper.GetModelObject(AObjectID));
  if Assigned(lPluginNode) then
  begin
    lPluginNodeGUI := TPluginNodeGUI.Create(Self);
    lPluginNodeGUI.ObjectID := AObjectID;
    lPluginNodeGUI.ObjectOwnerID := Self.ObjectID;
    lPluginNodeGUI.ModelObject := lPluginNode;
    lPluginNodeGUI.PluginName := lPluginNode.PluginName;
    lPluginNodeGUI.XLocation := lPluginNode.XLocation;
    lPluginNodeGUI.YLocation := lPluginNode.YLocation;
    lPluginNodeGUI.Parent := Self.sbPluginGraph;
    lPluginNodeGUI.OnConnection := @DoConnection;

    FNodeListGUI.Add(lPluginNodeGUI);
    lPluginNode.Attach(lPluginNodeGUI);
  end;

  DBLog('end TPluginProcessorGUI.CreateNodeGUI');
end;

procedure TPluginProcessorGUI.DeleteNodeGUI(AObjectID: string);
var
  lPluginNodeGUI: TPluginNodeGUI;
  lIndex: Integer;
begin
  DBLog('start TPluginProcessorGUI.DeleteNodeGUI ' + AObjectID);

  for lIndex := Pred(FNodeListGUI.Count) downto 0 do
  begin
    lPluginNodeGUI := TPluginNodeGUI(FNodeListGUI[lIndex]);

    if lPluginNodeGUI.ObjectID = AObjectID then
    begin
      FNodeListGUI.Remove(lPluginNodeGUI);
    end;
  end;

  DBLog('end TPluginProcessorGUI.DeleteNodeGUI');
end;

procedure TPluginProcessorGUI.CreateConnectionGUI(AObjectID: string);
var
  lInterConnect: TInterConnect;
  lInterConnectGUI: TInterConnectGUI;
begin
  DBLog('start TPluginProcessorGUI.CreateConnectionGUI ' + AObjectID);

  lInterConnect := TInterConnect(GObjectMapper.GetModelObject(AObjectID));
  if Assigned(lInterConnect) then
  begin
    lInterConnectGUI := TInterConnectGUI.Create(Self.ObjectID);
    lInterConnectGUI.ObjectID := AObjectID;
    lInterConnectGUI.ObjectOwnerID := Self.ObjectID;
    lInterConnectGUI.ModelObject := lInterConnect;
    lInterConnectGUI.FromPluginNode := lInterConnect.FromPluginNode;
    lInterConnectGUI.ToPluginNode := lInterConnect.ToPluginNode;

    FConnectionListGUI.Add(lInterConnectGUI);
    lInterConnect.Attach(lInterConnectGUI);

    Invalidate;
  end;

  DBLog('end TPluginProcessorGUI.CreateConnectionGUI');
end;

procedure TPluginProcessorGUI.DeleteConnectionGUI(AObjectID: string);
var
  lInterConnectGUI: TInterConnect;
  lIndex: Integer;
begin
  DBLog('start TPluginProcessorGUI.DeleteConnectionGUI ' + AObjectID);

  for lIndex := Pred(FConnectionListGUI.Count) downto 0 do
  begin
    lInterConnectGUI := TInterConnect(FConnectionListGUI[lIndex]);

    if lInterConnectGUI.ObjectID = AObjectID then
    begin
      FConnectionListGUI.Remove(lInterConnectGUI);
      break;
    end;
  end;

  Invalidate;

  DBLog('end TPluginProcessorGUI.DeleteConnectionGUI');
end;

procedure TPluginProcessorGUI.DoConnection(AObjectID: string; AParameter: string);
var
  lCreateConnectionCommand: TCreateConnectionCommand;
begin
  // Start connection
  if not FBusyConnecting then
  begin
    FTempConnection.FromPluginNode := AObjectID;
    FTempConnection.FromPluginNodePort := AParameter;

    DBLog('Start plugin connect %s', AObjectID);

    FBusyConnecting := True;
  end
  else
  // End connection
  begin
    FTempConnection.ToPluginNode := AObjectID;
    FTempConnection.ToPluginNodePort := AParameter;

    DBLog('End plugin connect %s', AObjectID);

    // Create connection command here
    lCreateConnectionCommand := TCreateConnectionCommand.Create(Self.ObjectID);
    try
      lCreateConnectionCommand.FromPluginNode := FTempConnection.FromPluginNode;
      lCreateConnectionCommand.ToPluginNode := FTempConnection.ToPluginNode;

      GCommandQueue.PushCommand(lCreateConnectionCommand);
    except
      lCreateConnectionCommand.Free;
    end;

    FBusyConnecting := False;
  end;
end;

function TPluginProcessorGUI.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TPluginProcessorGUI.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

function TPluginProcessorGUI.GetObjectOwnerID: string;
begin
  Result := FObjectOwnerID;
end;

procedure TPluginProcessorGUI.SetObjectOwnerID(const AObjectOwnerID: string);
begin
  FObjectOwnerID := AObjectOwnerID;
end;

{ TInterConnectGUI }

function TInterConnectGUI.GetIsReady: Boolean;
begin
  Result := ((FFromPluginNode <> '') and (FFromPluginNodePort <> '') and
    (FToPluginNode <> '') and (FToPluginNodePort <> ''));
end;

procedure TInterConnectGUI.Update(Subject: THybridPersistentModel);
begin
  FFromPluginNode := TInterConnect(Subject).FromPluginNode;
  FToPluginNode := TInterConnect(Subject).ToPluginNode;
  FFromPluginNodePort := TInterConnect(Subject).FromPluginNodePort;
  FToPluginNodePort := TInterConnect(Subject).ToPluginNodePort;
end;

initialization
  {$I pluginhostgui.lrs}

end.

