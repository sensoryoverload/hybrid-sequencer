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
  plugin, utils, Controls, LCLType, Graphics, ExtCtrls, contnrs, pluginhost,
  pluginnodegui, bankgui, sampler, ladspaloader;

type
  { TPluginProcessorGUI }

  TPluginProcessorGUI = class(TFrame, IObserver)
    pnlPlugin: TPanel;
    procedure pnlPluginDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure pnlPluginDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    { private declarations }
    FUpdateSubject: THybridPersistentModel;
    FIsDirty: Boolean;

    FAudioOutGUI: TGenericPluginGUI;
    FAudioInGUI: TGenericPluginGUI;

    FOnChangeNodeList: TNotifyEvent;

    FObjectOwnerID: string;
    FObjectID: string;
    FObjectOwner: TObject;
    FModel: THybridPersistentModel;

    FNodeListGUI: TObjectList;
    procedure SortPlugins(Sender: TObject);
    procedure UpdatePluginOrder;
  protected
    procedure DoChangeNodeList;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce;
    procedure UpdateView(AForceRedraw: Boolean = False);
    procedure EraseBackground(DC: HDC); override;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure ReleasePlugin(Data: PtrInt);
    procedure CreatePluginNodeGUI(AObjectID: string);
    procedure DeletePluginNodeGUI(AObjectID: string);
    function GetModel: THybridPersistentModel;
    procedure SetModel(AModel: THybridPersistentModel);
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string; virtual;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property ObjectID: string read GetObjectID write SetObjectID;
    property Model: THybridPersistentModel read FModel write FModel;
    property NodeListGUI: TObjectList read FNodeListGUI write FNodeListGUI;
    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
    property AudioOutGUI: TGenericPluginGUI read FAudioOutGUI write FAudioOutGUI;
    property AudioInGUI: TGenericPluginGUI read FAudioInGUI write FAudioInGUI;
    property OnChangeNodeList: TNotifyEvent read FOnChangeNodeList write FOnChangeNodeList;
  end;

implementation

uses
  global_command, ComCtrls,
  plugin_distortion, plugin_distortion_gui,
  plugin_freeverb, plugin_freeverb_gui,
  plugin_bassline, plugin_bassline_gui,
  plugin_decimate, plugin_decimate_gui;

function SortOnSequenceNr(Item1 : Pointer; Item2 : Pointer) : Integer;
var
  lSequenceNr1, lSequenceNr2 : TGenericPluginGUI;
begin
  // We start by viewing the object pointers as TGenericPluginGUI objects
  lSequenceNr1 := TGenericPluginGUI(Item1);
  lSequenceNr2 := TGenericPluginGUI(Item2);

  // Now compare by sequencenr
  if lSequenceNr1.SequenceNr > lSequenceNr2.SequenceNr then
    Result := 1
  else if lSequenceNr1.SequenceNr = lSequenceNr2.SequenceNr then
    Result := 0
  else
    Result := -1;
end;

function SortOnScreenOrder(Item1 : Pointer; Item2 : Pointer) : Integer;
var
  lLeft1, lLeft2 : TGenericPluginGUI;
begin
  // We start by viewing the object pointers as TGenericPluginGUI objects
  lLeft1 := TGenericPluginGUI(Item1);
  lLeft2 := TGenericPluginGUI(Item2);

  // Now compare by sequencenr
  if lLeft1.Left > lLeft2.Left then
    Result := 1
  else if lLeft1.Left = lLeft2.Left then
    Result := 0
  else
    Result := -1;
end;

procedure TPluginProcessorGUI.pnlPluginDragDrop(Sender, Source: TObject; X,
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
      lCreateNodesCommand.SequenceNr := FNodeListGUI.Count;

      if Assigned(lTreeView.Selected.Data)  then
      begin
        lCreateNodesCommand.PluginType := ptLADSPA;
        lCreateNodesCommand.PluginName :=
          TTreeViewPluginInfo(lTreeView.Selected.Data).Caption;
        lCreateNodesCommand.UniqueId :=
          TTreeViewPluginInfo(lTreeView.Selected.Data).UniqueId;
      end
      else if SameText(lTreeView.Selected.Text, 'sampler') then
      begin
        lCreateNodesCommand.PluginType := ptSampler;
      end
      else if SameText(lTreeView.Selected.Text, 'distortion') then
      begin
        lCreateNodesCommand.PluginType := ptDistortion;
      end
      else if SameText(lTreeView.Selected.Text, 'reverb') then
      begin
        lCreateNodesCommand.PluginType := ptReverb;
      end
      else if SameText(lTreeView.Selected.Text, 'bassline') then
      begin
        lCreateNodesCommand.PluginType := ptBassline;
      end
      else if SameText(lTreeView.Selected.Text, 'bitreducer') then
      begin
        lCreateNodesCommand.PluginType := ptDecimate;
      end;

      GCommandQueue.PushCommand(lCreateNodesCommand);
    except
      lCreateNodesCommand.Free;
    end;
  end;
end;

procedure TPluginProcessorGUI.pnlPluginDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

constructor TPluginProcessorGUI.Create(AOwner: TComponent);
begin
  DBLog('start TPluginProcessorGUI.Create');

  inherited Create(AOwner);

  FIsDirty := False;

  Height := 143;

  FNodeListGUI := TObjectList.create(True);

  DBLog('end TPluginProcessorGUI.Create');
end;

destructor TPluginProcessorGUI.Destroy;
begin
  DBLog('start TPluginProcessorGUI.Destroy');

  FNodeListGUI.Free;

  inherited Destroy;

  DBLog('end TPluginProcessorGUI.Destroy');
end;

procedure TPluginProcessorGUI.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TPluginProcessorGUI.Update');

  FUpdateSubject := Subject;
  FIsDirty := True;

  DBLog('end TPluginProcessorGUI.Update');
end;

procedure TPluginProcessorGUI.UpdateView(AForceRedraw: Boolean = False);
var
  lIndex: Integer;
begin
  if FIsDirty and Assigned(FUpdateSubject) then
  begin
    FIsDirty := False;

    // Create or Delete pluginnodes
    DBLog('DiffLists NodeListGUI');

    DiffLists(
      TPluginProcessor(FUpdateSubject).NodeList,
      NodeListGUI,
      @CreatePluginNodeGUI,
      @DeletePluginNodeGUI);

    UpdatePluginOrder;

    DoChangeNodeList;
  end;

  for lIndex := 0 to Pred(FNodeListGUI.Count) do
  begin
    if not (FNodeListGUI[lIndex] is TBankView) then
    begin
      TGenericPluginGUI(FNodeListGUI[lIndex]).UpdateView;
    end;
  end;
end;

procedure TPluginProcessorGUI.UpdatePluginOrder;
var
  lIndex: Integer;
  lOffsetAdder: Integer;
  lPluginGUI: TGenericPluginGUI;
begin
  // Retrieve pluginorder from model
  for lIndex := 0 to Pred(FNodeListGUI.Count) do
  begin
    lPluginGUI := TGenericPluginGUI(FNodeListGUI[lIndex]);
    lPluginGUI.SequenceNr := TPluginNode(lPluginGUI.Model).SequenceNr;
  end;

  // Sort view order on sequencenr
  FNodeListGUI.Sort(@SortOnSequenceNr);

  // Now set the plugin gui in order from left to right
  lOffsetAdder := 0;
  for lIndex := 0 to Pred(FNodeListGUI.Count) do
  begin
    lPluginGUI := TGenericPluginGUI(FNodeListGUI[lIndex]);

    lPluginGUI.Left := lOffsetAdder;
    lOffsetAdder := lOffsetAdder + lPluginGUI.Width;
  end;
end;

procedure TPluginProcessorGUI.DoChangeNodeList;
begin
  if Assigned(FOnChangeNodeList) then
  begin
    FOnChangeNodeList(Self);
  end;
end;

procedure TPluginProcessorGUI.SortPlugins(Sender: TObject);
var
  lIndex: Integer;
  lPluginGUI: TGenericPluginGUI;
  lOrderNodesCommand: TOrderNodesCommand;
  lOrderList: TStringList;
begin
  // Sort the nodelist on the components' left property
  FNodeListGUI.Sort(@SortOnScreenOrder);

  lOrderList := TStringList.Create;
  try
    for lIndex := 0 to Pred(FNodeListGUI.Count) do
    begin
      lPluginGUI := TGenericPluginGUI(FNodeListGUI[lIndex]);

      lPluginGUI.SequenceNr := lIndex;

      lOrderList.Values[lPluginGUI.ObjectID] := IntToStr(lPluginGUI.SequenceNr);
    end;

    // Send command to pluginhost
    lOrderNodesCommand := TOrderNodesCommand.Create(ObjectID);
    try
      lOrderNodesCommand.PluginOrderList := lOrderList.Text;
      GCommandQueue.PushCommand(lOrderNodesCommand);
    except
      lOrderNodesCommand.Free;
    end;
  finally
    lOrderList.Free;
  end;
end;

procedure TPluginProcessorGUI.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

procedure TPluginProcessorGUI.Connect;
begin
  Model := GObjectMapper.GetModelObject(ObjectID);
end;

procedure TPluginProcessorGUI.Disconnect;
var
  lIndex: Integer;
  lPluginNodeGUI: TGenericPluginGUI;
begin
  for lIndex := Pred(FNodeListGUI.Count) downto 0 do
  begin
    lPluginNodeGUI := TGenericPluginGUI(FNodeListGUI[lIndex]);

    TPluginNode(lPluginNodeGUI.Model).Detach(lPluginNodeGUI);
    FNodeListGUI.Remove(lPluginNodeGUI);
  end;
end;

procedure TPluginProcessorGUI.CreatePluginNodeGUI(AObjectID: string);
var
  lPluginNode: TPluginNode;
  lPluginNodeGUI: TGenericPluginGUI;
  lSampleBankGUI: TBankView;
  lPluginDistortionGUI: TPluginDistortionGUI;
  lPluginFreeverbGUI: TPluginFreeverbGUI;
  lPluginBasslineGUI: TPluginBasslineGUI;
  lPluginDecimateGUI: TPluginDecimateGUI;
begin
  DBLog('start TPluginProcessorGUI.CreateNodeGUI ' + AObjectID);

  lPluginNode := TPluginNode(GObjectMapper.GetModelObject(AObjectID));
  if Assigned(lPluginNode) then
  begin
    case lPluginNode.PluginType of
    ptIO:
    begin
      lPluginNodeGUI := TGenericPluginGUI.Create(nil);
      lPluginNodeGUI.PluginProcessorGui := Self;
      lPluginNodeGUI.ObjectID := AObjectID;
      lPluginNodeGUI.ObjectOwnerID := Self.ObjectID;
      lPluginNodeGUI.Model := lPluginNode;
      lPluginNodeGUI.PluginName := lPluginNode.PluginName;
      lPluginNodeGUI.Align := alNone;
      lPluginNodeGUI.Parent := pnlPlugin;
      lPluginNodeGUI.Width := 100;
      lPluginNodeGUI.OnStopDragging := @SortPlugins;

      FNodeListGUI.Add(lPluginNodeGUI);
      lPluginNode.Attach(lPluginNodeGUI);
    end;
    ptSampler:
    begin
      lSampleBankGUI := TBankView.Create(nil);
      lSampleBankGUI.PluginProcessorGui := Self;
      lSampleBankGUI.ObjectID := AObjectID;
      lSampleBankGUI.ObjectOwnerID := Self.ObjectID;
      lSampleBankGUI.Model := TSampleBank(lPluginNode);
      lSampleBankGUI.PluginName := lPluginNode.PluginName;
      lSampleBankGUI.Align := alNone;
      lSampleBankGUI.Parent := pnlPlugin;
      lSampleBankGUI.Top := 0;
      lSampleBankGUI.Left := 0;;
      lSampleBankGUI.Width := 955;
      lSampleBankGUI.OnStopDragging := @SortPlugins;

      FNodeListGUI.Add(lSampleBankGUI);
      TSampleBank(lPluginNode).Attach(lSampleBankGUI);
    end;
    ptDistortion:
    begin
      lPluginDistortionGUI := TPluginDistortionGUI.Create(nil);
      lPluginDistortionGUI.PluginProcessorGui := Self;
      lPluginDistortionGUI.ObjectID := AObjectID;
      lPluginDistortionGUI.ObjectOwnerID := Self.ObjectID;
      lPluginDistortionGUI.Model := TPluginDistortion(lPluginNode);
      lPluginDistortionGUI.PluginName := lPluginNode.PluginName;
      lPluginDistortionGUI.Width := 100;
      lPluginDistortionGUI.Align := alNone;
      lPluginDistortionGUI.Parent := pnlPlugin;
      lPluginDistortionGUI.OnStopDragging := @SortPlugins;

      FNodeListGUI.Add(lPluginDistortionGUI);
      TPluginDistortion(lPluginNode).Attach(lPluginDistortionGUI);
    end;
    ptReverb:
    begin
      lPluginFreeverbGUI := TPluginFreeverbGUI.Create(nil);
      lPluginFreeverbGUI.PluginProcessorGui := Self;
      lPluginFreeverbGUI.ObjectID := AObjectID;
      lPluginFreeverbGUI.ObjectOwnerID := Self.ObjectID;
      lPluginFreeverbGUI.Model := TPluginFreeverb(lPluginNode);
      lPluginFreeverbGUI.PluginName := lPluginNode.PluginName;
      lPluginFreeverbGUI.Align := alNone;
      lPluginFreeverbGUI.Parent := pnlPlugin;
      lPluginFreeverbGUI.Width := 100;
      lPluginFreeverbGUI.OnStopDragging := @SortPlugins;

      FNodeListGUI.Add(lPluginFreeverbGUI);
      TPluginFreeverb(lPluginNode).Attach(lPluginFreeverbGUI);
    end;
    ptBassline:
    begin
      lPluginBasslineGUI := TPluginBasslineGUI.Create(nil);
      lPluginBasslineGUI.PluginProcessorGui := Self;
      lPluginBasslineGUI.ObjectID := AObjectID;
      lPluginBasslineGUI.ObjectOwnerID := Self.ObjectID;
      lPluginBasslineGUI.Model := TPluginBassline(lPluginNode);
      lPluginBasslineGUI.PluginName := lPluginNode.PluginName;
      lPluginBasslineGUI.Align := alNone;
      lPluginBasslineGUI.Parent := pnlPlugin;
      lPluginBasslineGUI.Width := 100;
      lPluginBasslineGUI.OnStopDragging := @SortPlugins;

      FNodeListGUI.Add(lPluginBasslineGUI);
      TPluginBassline(lPluginNode).Attach(lPluginBasslineGUI);
    end;
    ptDecimate:
    begin
      lPluginDecimateGUI := TPluginDecimateGUI.Create(nil);
      lPluginDecimateGUI.PluginProcessorGui := Self;
      lPluginDecimateGUI.ObjectID := AObjectID;
      lPluginDecimateGUI.ObjectOwnerID := Self.ObjectID;
      lPluginDecimateGUI.Model := TPluginDecimate(lPluginNode);
      lPluginDecimateGUI.PluginName := lPluginNode.PluginName;
      lPluginDecimateGUI.Align := alNone;
      lPluginDecimateGUI.Parent := pnlPlugin;
      lPluginDecimateGUI.Width := 100;
      lPluginDecimateGUI.OnStopDragging := @SortPlugins;

      FNodeListGUI.Add(lPluginDecimateGUI);
      TPluginDecimate(lPluginNode).Attach(lPluginDecimateGUI);
    end;
    ptLADSPA:
    begin
      lPluginNodeGUI := TGenericPluginGUI.Create(nil);
      lPluginNodeGUI.PluginProcessorGui := Self;
      lPluginNodeGUI.ObjectID := AObjectID;
      lPluginNodeGUI.ObjectOwnerID := Self.ObjectID;
      lPluginNodeGUI.Model := lPluginNode;
      lPluginNodeGUI.PluginName := lPluginNode.PluginName;
      lPluginNodeGUI.Align := alNone;
      lPluginNodeGUI.Parent := pnlPlugin;
      lPluginNodeGUI.OnStopDragging := @SortPlugins;

      FNodeListGUI.Add(lPluginNodeGUI);
      lPluginNode.Attach(lPluginNodeGUI);
    end;
    end;
  end;

  DBLog('end TPluginProcessorGUI.CreateNodeGUI');
end;

procedure TPluginProcessorGUI.DeletePluginNodeGUI(AObjectID: string);
var
  lPluginNodeGUI: TGenericPluginGUI;
  lSamplerNodeGUI: TBankView;
  lIndex: Integer;
begin
  DBLog('start TPluginProcessorGUI.DeleteNodeGUI ' + AObjectID);

  for lIndex := Pred(FNodeListGUI.Count) downto 0 do
  begin
    // TBankView has a different ancestor so handle this special case
    if FNodeListGUI[lIndex] is TBankView then
    begin
      lSamplerNodeGUI := TBankView(FNodeListGUI[lIndex]);

      if lSamplerNodeGUI.ObjectID = AObjectID then
      begin
        TPluginNode(lSamplerNodeGUI.Model).Detach(lSamplerNodeGUI);
        FNodeListGUI.Remove(lSamplerNodeGUI);
      end;
    end
    else
    begin
      lPluginNodeGUI := TGenericPluginGUI(FNodeListGUI[lIndex]);

      if lPluginNodeGUI.ObjectID = AObjectID then
      begin
        TPluginNode(lPluginNodeGUI.Model).Detach(lPluginNodeGUI);
        FNodeListGUI.Remove(lPluginNodeGUI);
      end;
    end;
  end;

  DBLog('end TPluginProcessorGUI.DeleteNodeGUI');
end;

function TPluginProcessorGUI.GetModel: THybridPersistentModel;
begin
  Result := FModel;
end;

procedure TPluginProcessorGUI.SetModel(AModel: THybridPersistentModel);
begin
  FModel := AModel;
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

procedure TPluginProcessorGUI.ReleasePlugin(Data: PtrInt);
var
  lDeleteNodesCommand: TDeleteNodesCommand;
begin
  lDeleteNodesCommand := TDeleteNodesCommand.Create(TGenericPluginGUI(Data).ObjectOwnerID);
  try
    lDeleteNodesCommand.ObjectID := TGenericPluginGUI(Data).ObjectID;

    GCommandQueue.PushCommand(lDeleteNodesCommand);
  except
    lDeleteNodesCommand.Free;
  end;
end;


initialization
  {$I pluginhostgui.lrs}

end.

