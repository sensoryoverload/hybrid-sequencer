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
  pluginnodegui, bankgui, sampler;

type
  { TPluginProcessorGUI }

  TPluginProcessorGUI = class(TFrame, IObserver)
    gbPlugin: TGroupBox;
    pnlPlugin: TPanel;
    procedure pnlPluginDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure pnlPluginDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    { private declarations }
    FAudioOutGUI: TGenericPluginGUI;
    FAudioInGUI: TGenericPluginGUI;

    FObjectOwnerID: string;
    FObjectID: string;
    FObjectOwner: TObject;
    FModel: THybridPersistentModel;

    FNodeListGUI: TObjectList;
  protected
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce;
    procedure EraseBackground(DC: HDC); override;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure CreateNodeGUI(AObjectID: string);
    procedure DeleteNodeGUI(AObjectID: string);
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
  end;

implementation

uses
  global_command, ComCtrls,
  plugin_distortion, plugin_distortion_gui,
  plugin_freeverb, plugin_freeverb_gui,
  plugin_bassline, plugin_bassline_gui,
  plugin_decimate, plugin_decimate_gui;

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
      lCreateNodesCommand.SequenceNr := 0;
      lCreateNodesCommand.PluginName := lTreeView.Selected.Text;
      if SameText(lTreeView.Selected.Text, 'sampler') then
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

  // Create or Delete pluginnodes
  DBLog('DiffLists NodeListGUI');

  DiffLists(
    TPluginProcessor(Subject).NodeList,
    NodeListGUI,
    @CreateNodeGUI,
    @DeleteNodeGUI);

  DBLog('end TPluginProcessorGUI.Update');
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
begin
  //
end;

procedure TPluginProcessorGUI.CreateNodeGUI(AObjectID: string);
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
      lPluginNodeGUI.ObjectID := AObjectID;
      lPluginNodeGUI.ObjectOwnerID := Self.ObjectID;
      lPluginNodeGUI.Model := lPluginNode;
      lPluginNodeGUI.PluginName := lPluginNode.PluginName;
      lPluginNodeGUI.Parent := pnlPlugin;
      lPluginNodeGUI.Width := 50;
      lPluginNodeGUI.Align := alLeft;

      if Odd(FNodeListGUI.Count) then
      begin
        lPluginNodeGUI.Color := clRed;
      end
      else
      begin
        lPluginNodeGUI.Color := clBlue;
      end;

      FNodeListGUI.Add(lPluginNodeGUI);
      lPluginNode.Attach(lPluginNodeGUI);
    end;
    ptSampler:
    begin
      lSampleBankGUI := TBankView.Create(nil);
      lSampleBankGUI.ObjectID := AObjectID;
      lSampleBankGUI.ObjectOwnerID := Self.ObjectID;
      lSampleBankGUI.Model := TSampleBank(lPluginNode);
//      lSampleBankGUI.PluginName := lPluginNode.PluginName;
      lSampleBankGUI.Parent := pnlPlugin;
      lSampleBankGUI.Align := alLeft;

      FNodeListGUI.Add(lSampleBankGUI);
      TSampleBank(lPluginNode).Attach(lSampleBankGUI);
    end;
    ptDistortion:
    begin
      lPluginDistortionGUI := TPluginDistortionGUI.Create(nil);
      lPluginDistortionGUI.ObjectID := AObjectID;
      lPluginDistortionGUI.ObjectOwnerID := Self.ObjectID;
      lPluginDistortionGUI.Model := TPluginDistortion(lPluginNode);
      lPluginDistortionGUI.PluginName := lPluginNode.PluginName;
      lPluginDistortionGUI.Parent := pnlPlugin;
      lPluginDistortionGUI.Width := 100;
      lPluginDistortionGUI.Align := alLeft;

      FNodeListGUI.Add(lPluginDistortionGUI);
      TPluginDistortion(lPluginNode).Attach(lPluginDistortionGUI);
    end;
    ptReverb:
    begin
      lPluginFreeverbGUI := TPluginFreeverbGUI.Create(nil);
      lPluginFreeverbGUI.ObjectID := AObjectID;
      lPluginFreeverbGUI.ObjectOwnerID := Self.ObjectID;
      lPluginFreeverbGUI.Model := TPluginFreeverb(lPluginNode);
      lPluginFreeverbGUI.PluginName := lPluginNode.PluginName;
      lPluginFreeverbGUI.Parent := pnlPlugin;
      lPluginFreeverbGUI.Width := 100;
      lPluginFreeverbGUI.Align := alLeft;

      FNodeListGUI.Add(lPluginFreeverbGUI);
      TPluginFreeverb(lPluginNode).Attach(lPluginFreeverbGUI);
    end;
    ptBassline:
    begin
      lPluginBasslineGUI := TPluginBasslineGUI.Create(nil);
      lPluginBasslineGUI.ObjectID := AObjectID;
      lPluginBasslineGUI.ObjectOwnerID := Self.ObjectID;
      lPluginBasslineGUI.Model := TPluginBassline(lPluginNode);
      lPluginBasslineGUI.PluginName := lPluginNode.PluginName;
      lPluginBasslineGUI.Parent := pnlPlugin;
      lPluginBasslineGUI.Width := 100;
      lPluginBasslineGUI.Align := alLeft;

      FNodeListGUI.Add(lPluginBasslineGUI);
      TPluginBassline(lPluginNode).Attach(lPluginBasslineGUI);
    end;
    ptDecimate:
    begin
      lPluginDecimateGUI := TPluginDecimateGUI.Create(nil);
      lPluginDecimateGUI.ObjectID := AObjectID;
      lPluginDecimateGUI.ObjectOwnerID := Self.ObjectID;
      lPluginDecimateGUI.Model := TPluginDecimate(lPluginNode);
      lPluginDecimateGUI.PluginName := lPluginNode.PluginName;
      lPluginDecimateGUI.Parent := pnlPlugin;
      lPluginDecimateGUI.Width := 100;
      lPluginDecimateGUI.Align := alLeft;

      FNodeListGUI.Add(lPluginDecimateGUI);
      TPluginDecimate(lPluginNode).Attach(lPluginDecimateGUI);
    end;
    end;
  end;

  DBLog('end TPluginProcessorGUI.CreateNodeGUI');
end;

procedure TPluginProcessorGUI.DeleteNodeGUI(AObjectID: string);
var
  lPluginNodeGUI: TGenericPluginGUI;
  lIndex: Integer;
begin
  DBLog('start TPluginProcessorGUI.DeleteNodeGUI ' + AObjectID);

  for lIndex := Pred(FNodeListGUI.Count) downto 0 do
  begin
    lPluginNodeGUI := TGenericPluginGUI(FNodeListGUI[lIndex]);

    if lPluginNodeGUI.ObjectID = AObjectID then
    begin
      TPluginNode(lPluginNodeGUI.Model).Detach(lPluginNodeGUI);
      FNodeListGUI.Remove(lPluginNodeGUI);
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


initialization
  {$I pluginhostgui.lrs}

end.

