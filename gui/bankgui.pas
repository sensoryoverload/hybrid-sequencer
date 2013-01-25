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

  bankgui.pas
}

unit bankgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, globalconst,
  sampler, samplegui, Controls, Contnrs, global, utils, Graphics, LCLType,
  StdCtrls, DBGrids, Grids, Menus, ActnList, pluginnodegui;

type

  { TBankView }

  TBankView = class(TGenericPluginGUI)
    actDeleteSample: TAction;
    actNewSample: TAction;
    alSampleSelect: TActionList;
    gbSampleSelect: TGroupBox;
    lbSampleSelector: TListBox;
    miNewSample: TMenuItem;
    miDeleteSample: TMenuItem;
    pmSampleSelect: TPopupMenu;
    procedure actDeleteSampleExecute(Sender: TObject);
    procedure actNewSampleExecute(Sender: TObject);
    procedure lbSampleSelectorClick(Sender: TObject);
    procedure lbSampleSelectorDblClick(Sender: TObject);
    procedure lbSampleSelectorDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbSampleSelectorDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure sbSamplesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure sbSamplesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    { private declarations }
    FPluginName: string;
    FSampleListGUI: TObjectList;
    FSelectedSampleGUI: TSampleView;
    FOldSelectedSample: string;
    FSampleView: TSampleView;

    procedure CreateSampleGUI(AObjectID: string);
    procedure DeleteSampleGUI(AObjectID: string);
    procedure DoChangeSelectedSample(Subject: THybridPersistentModel);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
    procedure Update(Subject: THybridPersistentModel); override;
    property SampleListGUI: TObjectList read FSampleListGUI write FSampleListGUI;
    property PluginName: string read FPluginName write FPluginName;
  end;

implementation

uses
  DialControl, ComCtrls, global_command;

{ TBankView }

constructor TBankView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSampleListGUI := TObjectList.Create;

  FSampleView := TSampleView.Create(Self);
  FSampleView.Enabled := False;
  FSampleView.EnableControls := False;
  FSampleView.Align := alClient;
  FSampleView.Parent := Self;
end;

destructor TBankView.Destroy;
begin
  if Assigned(FSampleListGUI) then
    FSampleListGUI.Free;

  inherited Destroy;
end;

procedure TBankView.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TBankView.Update');

  DiffLists(
    TSampleBank(Subject).SampleList,
    FSampleListGUI,
    @CreateSampleGUI,
    @DeleteSampleGUI);

  DoChangeSelectedSample(Subject);

  DBLog('end TBankView.Update');
end;

procedure TBankView.Connect;
begin
  //
end;

procedure TBankView.Disconnect;
var
  lSampleIndex: Integer;
  lSample: TSample;
  lSampleSelectControl: TSampleSelectControl;
  lOldSelectedSample: TSample;
begin
  // Detach all sampleselect controls
  lbSampleSelector.Clear;
  for lSampleIndex := Pred(FSampleListGUI.Count) downto 0 do
  begin
    lSampleSelectControl := TSampleSelectControl(FSampleListGUI[lSampleIndex]);
    lSample := TSample(GObjectMapper.GetModelObject(lSampleSelectControl.ObjectID));

    lSample.Detach(lSampleSelectControl);
    FSampleListGUI.Remove(lSampleSelectControl);
  end;
  FSampleListGUI.Clear;

  // Detach the sampleview if attached (ie controls are active)
  if FOldSelectedSample <> '' then
  begin
    lOldSelectedSample := TSample(GObjectMapper.GetModelObject(FOldSelectedSample));

    if Assigned(lOldSelectedSample) then
    begin
      lOldSelectedSample.Detach(FSampleView);
    end;
  end;
end;

procedure TBankView.sbSamplesDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  lTreeView: TTreeView;
  lCreateSampleCommand: TCreateSampleCommand;
begin
  if Source is TTreeView then
  begin
    lTreeView := TTreeView(Source);

    DBLog('lTreeView.Selected.Text: ' + lTreeView.Selected.Text);

    // Get tbank object
    lCreateSampleCommand := TCreateSampleCommand.Create(ObjectID);
    try
      lCreateSampleCommand.SampleLocation := lTreeView.Selected.Text;
      lCreateSampleCommand.BaseNote := 48;
      lCreateSampleCommand.LowNote := 0;
      lCreateSampleCommand.HighNote := 127;

      GCommandQueue.PushCommand(lCreateSampleCommand);
    except
      lCreateSampleCommand.Free;
    end;
  end;
end;

procedure TBankView.lbSampleSelectorDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True; // TODO True if valid sample
end;

procedure TBankView.lbSampleSelectorDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  lTreeView: TTreeView;
  lCreateSampleCommand: TCreateSampleCommand;
begin
  if Source is TTreeView then
  begin
    lTreeView := TTreeView(Source);

    DBLog('lTreeView.Selected.Text: ' + lTreeView.Selected.Text);

    // Get tbank object
    lCreateSampleCommand := TCreateSampleCommand.Create(ObjectID);
    lCreateSampleCommand.BaseNote := 48;
    lCreateSampleCommand.LowNote := 0;
    lCreateSampleCommand.HighNote := 127;
    try
      lCreateSampleCommand.SampleLocation := lTreeView.Selected.Text;

      GCommandQueue.PushCommand(lCreateSampleCommand);
    except
      lCreateSampleCommand.Free;
    end;
  end;
end;

procedure TBankView.lbSampleSelectorClick(Sender: TObject);
var
  lSelectedObjectID: string;
  lIndex: Integer;
  lSelectSampleCommand: TSelectSampleCommand;
begin
  // Search for selected (clicked) sample in listbox
  for lIndex := 0 to Pred(TListBox(Sender).Count) do
  begin
    if TListBox(Sender).Selected[lIndex] then
    begin
      lSelectedObjectID := TSampleSelectControl(TListBox(Sender).Items.Objects[lIndex]).ObjectID;
      break;
    end;
  end;

  if FOldSelectedSample <> lSelectedObjectID then
  begin
    // Command will toggle all selected samplecontrols to false and set this one
    // true; Also changes viewed sample editor
    lSelectSampleCommand := TSelectSampleCommand.Create(Self.ObjectID);
    try
      lSelectSampleCommand.ObjectID := lSelectedObjectID;
      lSelectSampleCommand.SelectedID := lSelectedObjectID;
      lSelectSampleCommand.Persist := True;

      GCommandQueue.PushCommand(lSelectSampleCommand);
    except
      lSelectSampleCommand.Free;
    end;
  end;
end;

procedure TBankView.actNewSampleExecute(Sender: TObject);
var
  lCreateSampleCommand: TCreateSampleCommand;
begin
  // Get tbank object
  lCreateSampleCommand := TCreateSampleCommand.Create(ObjectID);
  try
    lCreateSampleCommand.SampleLocation := '';
    lCreateSampleCommand.BaseNote := 48;
    lCreateSampleCommand.LowNote := 0;
    lCreateSampleCommand.HighNote := 127;

    GCommandQueue.PushCommand(lCreateSampleCommand);
  except
    lCreateSampleCommand.Free;
  end;
end;

procedure TBankView.actDeleteSampleExecute(Sender: TObject);
begin
  // Delete sample
end;

procedure TBankView.lbSampleSelectorDblClick(Sender: TObject);
begin
  // Create new sample if click on empty slot

  // Delete sample if clicked on sample => dialog Yes/Cancel
end;

procedure TBankView.sbSamplesDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := True; // TODO True if valid sample
end;

procedure TBankView.CreateSampleGUI(AObjectID: string);
var
  lSampleSelectControl: TSampleSelectControl;
  lSample: TSample;
begin
  DBLog('start TBankView.CreateSampleGUI');

  // Get state from server
  lSample := TSample(GObjectMapper.GetModelObject(AObjectID));
  if Assigned(lSample) then
  begin
    lSampleSelectControl := TSampleSelectControl.Create(Self.ObjectID);
    lSampleSelectControl.ObjectID := AObjectID;
    lSampleSelectControl.ObjectOwnerID := lSample.ObjectOwnerID;
    lSampleSelectControl.Caption := lSample.SampleLocation;
    lSampleSelectControl.SampleLocation := lSample.SampleLocation;
    lSampleSelectControl.Selected := lSample.Selected;
    lSampleSelectControl.SampleView := FSampleView;

    FSampleListGUI.Add(lSampleSelectControl);

    if lbSampleSelector.Items.IndexOfObject(lSampleSelectControl) = -1 then
    begin
      lbSampleSelector.Items.AddObject(lSampleSelectControl.SampleLocation, lSampleSelectControl);
    end;
  end;

  DBLog('end TBankView.CreateSampleGUI');
end;

procedure TBankView.DeleteSampleGUI(AObjectID: string);
var
  lSampleSelectControl: TSampleSelectControl;
  lIndex: Integer;
begin
  DBLog('start TBankView.DeleteSampleGUI');

  FSelectedSampleGUI := nil;

  for lIndex := Pred(FSampleListGUI.Count) downto 0 do
  begin
    lSampleSelectControl := TSampleSelectControl(FSampleListGUI[lIndex]);

    if lSampleSelectControl.ObjectID = AObjectID then
    begin
      lbSampleSelector.Items.Delete(lbSampleSelector.Items.IndexOfObject(lSampleSelectControl));

      FSampleListGUI.Remove(lSampleSelectControl);
      break;
    end;
  end;

  DBLog('end TBankView.DeleteSampleGUI');
end;

procedure TBankView.DoChangeSelectedSample(Subject: THybridPersistentModel);
var
  lOldSelectedSample: TSample;
  lNewSelectedSample: TSample;
  lSampleIndex: Integer;
  lListBoxIndex: Integer;
  lNewObjectID: string;
begin
  DBLog('start TBankView.DoChangeSelectedSample');

  // Set listbox focus to selected sample (by TCommand from Model)
  for lSampleIndex := 0 to Pred(TSampleBank(Subject).SampleList.Count) do
  begin
    if TSample(TSampleBank(Subject).SampleList[lSampleIndex]).Selected then
    begin
      lNewObjectID := TSample(TSampleBank(Subject).SampleList[lSampleIndex]).ObjectID;

      for lListBoxIndex := 0 to Pred(lbSampleSelector.Count) do
      begin
        lbSampleSelector.Selected[lListBoxIndex] :=
          (TSampleSelectControl(lbSampleSelector.Items.Objects[lListBoxIndex]).ObjectID =
          lNewObjectID);
      end;

      break;
    end;
  end;

  if lNewObjectID <> '' then
  begin
    for lSampleIndex := 0 to Pred(FSampleListGUI.Count) do
    begin
      TSampleSelectControl(FSampleListGUI[lSampleIndex]).Selected :=
        (TSampleSelectControl(FSampleListGUI[lSampleIndex]).ObjectID = lNewObjectID);
    end;
  end;

  // Can be unassigned when first used
  if FOldSelectedSample <> '' then
  begin
    lOldSelectedSample := TSample(GObjectMapper.GetModelObject(FOldSelectedSample));

    if Assigned(lOldSelectedSample) then
    begin
      // Release last sample connection
      lOldSelectedSample.Detach(FSampleView);
    end;
  end;

  lNewSelectedSample := TSample(GObjectMapper.GetModelObject(lNewObjectID));

  if Assigned(lNewSelectedSample) then
  begin
    FSampleView.ObjectID := lNewObjectID;
    FSampleView.ObjectOwnerID := ObjectID;

    // Attach new view
    lNewSelectedSample.Attach(FSampleView);
    if not FSampleView.Enabled then
    begin
      FSampleView.EnableControls := True;
      FSampleView.Enabled := True;
    end;
    FOldSelectedSample := lNewSelectedSample.ObjectID;
  end
  else
  begin
    FSampleView.EnableControls := False;
    FSampleView.Enabled := False;
  end;

  DBLog('end TBankView.DoChangeSelectedSample');
end;

initialization
  {$I bankgui.lrs}

end.

