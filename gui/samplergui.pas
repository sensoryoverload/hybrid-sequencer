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

  samplergui.pas
}

unit samplergui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, ComCtrls, ExtCtrls,
  dialcontrol, contnrs, Controls, global_command, globalconst,
  ShellCtrls, Menus, Dialogs, ActnList, sampler, bankgui, global, samplegui;

type
  { TSamplerGUI }

  TSamplerGUI = class(TFrame, IObserver)
    gbStructure: TGroupBox;
    miDeleteBank: TMenuItem;
    pnlStructure: TPanel;
    pmStructure: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    tvTreeStructure: TTreeView;
    procedure miDeleteBankClick(Sender: TObject);
    procedure tvTreeStructureClick(Sender: TObject);
    procedure tvTreeStructureDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvTreeStructureDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    { private declarations }
    FObjectOwnerID: string;
    FObjectID: string;
    FModelObject: TObject;
    FObjectOwner: TObject;

    FBankView: TBankView;
    FSelectedBank: TSampleBank;
    FSelectedSample: TSample;
    FBankListGUI: TObjectList;

    procedure ChangeSelectedBankUpdate(Subject: THybridPersistentModel);
    procedure CreateBankGUI(AObjectID: string);
    procedure DeleteBankGUI(AObjectID: string);
  public
    { public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); reintroduce;
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    property ObjectID: string read GetObjectID write SetObjectID;
    {
      Stuff to create, load, save, etc sampler stuff....
    }
    property ObjectOwnerID: string read FObjectOwnerID write FObjectOwnerID;
    property Model: TObject read FModelObject write FModelObject;
    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
    property BankListGUI: TObjectList read FBankListGUI write FBankListGUI;
    property SelectedBank: TSampleBank read FSelectedBank write FSelectedBank;
    property SelectedSample: TSample read FSelectedSample write FSelectedSample;
  published
  end;


implementation

uses utils, audiostructure;

{ TSamplerGUI }

procedure TSamplerGUI.tvTreeStructureDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  lTreeView: TTreeView;
  lCreateBankCommand: TCreateBankCommand;
begin
  if Source is TTreeView then
  begin
    lTreeView := TTreeView(Source);

    DBLog(Format('%s', [lTreeView.Selected.Text]));

    // When import are audio files then create a new bank with inside it all the dragged files
    lCreateBankCommand:= TCreateBankCommand.Create(ObjectID);
    try
      lCreateBankCommand.BankName := lTreeView.Selected.Text;

      GCommandQueue.PushCommand(lCreateBankCommand);
    except
      lCreateBankCommand.Free;
    end;
  end;
end;

procedure TSamplerGUI.miDeleteBankClick(Sender: TObject);
var
  lDeleteBankCommand: TDeleteBankCommand;
begin
  // Show 'Are you sure' form
  if MessageDlg(APP_NAME, 'Delete bank?', mtWarning, mbYesNo, 0) = mrYes then
  begin
    lDeleteBankCommand:= TDeleteBankCommand.Create(ObjectID);
    try
      GCommandQueue.PushCommand(lDeleteBankCommand);
    except
      on e: Exception do
      begin
        DBLog('Command create failed: ' + e.Message);
        lDeleteBankCommand.Free;
      end;
    end;
  end;
end;

procedure TSamplerGUI.tvTreeStructureClick(Sender: TObject);
var
  lSelectedBank: TSampleBank;
  lBankSelectControl: TBankSelectControl;
  lIndex: Integer;
  lChangeSelectedBankCommand: TChangeSelectedBankCommand;
begin

  lSelectedBank := TBankSelectControl(tvTreeStructure.Selected.Data).SampleBank;
  if Assigned(lSelectedBank) then
  begin
    if (lSelectedBank <> FSelectedBank) or (BankListGUI.Count = 1) then
    begin
      for lIndex := 0 to Pred(BankListGUI.Count) do
      begin
        lBankSelectControl := TBankSelectControl(BankListGUI[lIndex]);
        if lBankSelectControl.ObjectID = lSelectedBank.ObjectID then
        begin
          lChangeSelectedBankCommand := TChangeSelectedBankCommand.Create(ObjectID);
          try
            lChangeSelectedBankCommand.SelectedObjectID := lSelectedBank.ObjectID;

            GCommandQueue.PushCommand(lChangeSelectedBankCommand);
          except
            lChangeSelectedBankCommand.Free;
          end;
        end;
      end;

      DBLog('Change selected bank');
      // Detach bank view before Attaching another bank view
      FSelectedBank := lSelectedBank;
    end;
  end;
end;

procedure TSamplerGUI.tvTreeStructureDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TSamplerGUI.CreateBankGUI(AObjectID: string);
var
  lBankSelectControl: TBankSelectControl;
  lSampleBank: TSampleBank;
  lCurrentNode: TTreeNode;
begin
  DBLog('start TSamplerGUI.CreateBankGUI');

  lSampleBank := TSampleBank(GObjectMapper.GetModelObject(AObjectID));
  if Assigned(lSampleBank) then
  begin
    lBankSelectControl := TBankSelectControl.Create(nil);
    lBankSelectControl.SampleBank := lSampleBank;
    lBankSelectControl.ObjectID := AObjectID;
    lBankSelectControl.ObjectOwnerID := Self.ObjectID;

    FBankListGUI.Add(lBankSelectControl);

    FBankView.Bank := lSampleBank;
    lSampleBank.Attach(FBankView);

    GAudioStruct.SelectedBank := lSampleBank;
    if tvTreeStructure.Items.Count = 0 then
    begin
      lCurrentNode := tvTreeStructure.Items.AddObjectFirst(nil, lSampleBank.BankName, lBankSelectControl);
    end
    else
    begin
      lCurrentNode := tvTreeStructure.Items.AddObject(tvTreeStructure.Items.GetLastNode, lSampleBank.BankName, lBankSelectControl);
    end;

    if Assigned(lCurrentNode) then
    begin
      tvTreeStructure.Selected := lCurrentNode;
    end;
  end;

  Invalidate;

  DBLog('end TSamplerGUI.CreateBankGUI');
end;

procedure TSamplerGUI.DeleteBankGUI(AObjectID: string);
var
  lBankSelectControl: TBankSelectControl;
  lIndex: Integer;
  lNode: TTreeNode;
begin
  DBLog('start TSamplerGUI.DeleteBankGUI: ' + AObjectID);

  for lIndex := Pred(BankListGUI.Count) downto 0 do
  begin
    lBankSelectControl := TBankSelectControl(FBankListGUI[lIndex]);
    if lBankSelectControl.ObjectID = AObjectID then
    begin
      DBLog('Deleting BankGUI: ' + lBankSelectControl.ObjectID);

      lNode := tvTreeStructure.Items.FindNodeWithData(lBankSelectControl);
      if Assigned(lNode) then
      begin
        tvTreeStructure.Items.Delete(lNode);
      end;

      FBankListGUI.Remove(lBankSelectControl);
    end;
  end;

  Invalidate;

  DBLog('end TSamplerGUI.DeleteBankGUI');
end;

(*
procedure TSamplerGUI.CreateBankGUI(AObjectID: string);
var
  lSampleBankView: TBankView;
  lSampleBank: TSampleBank;
  lCurrentNode: TTreeNode;
begin
  DBLog('start TSamplerGUI.CreateBankGUI');

  lSampleBank := TSampleBank(GObjectMapper.GetModelObject(AObjectID));
  if Assigned(lSampleBank) then
  begin
    lSampleBankView := TBankView.Create(nil);
    lSampleBankView.Bank := lSampleBank;
    lSampleBankView.Parent := gbEngine;
    lSampleBankView.ObjectID := AObjectID;
    lSampleBankView.ObjectOwnerID := Self.ObjectID;
    lSampleBank.Attach(lSampleBankView);

    FBankListGUI.Add(lSampleBankView);

    GAudioStruct.SelectedBank := lSampleBank;
    if tvTreeStructure.Items.Count = 0 then
    begin
      lCurrentNode := tvTreeStructure.Items.AddObjectFirst(nil, lSampleBank.BankName, lSampleBank);
    end
    else
    begin
      lCurrentNode := tvTreeStructure.Items.AddObject(tvTreeStructure.Items.GetLastNode, lSampleBank.BankName, lSampleBank);
    end;

    if Assigned(lCurrentNode) then
    begin
      tvTreeStructure.Selected := lCurrentNode;
    end;
  end;

  Invalidate;

  DBLog('end TSamplerGUI.CreateBankGUI');
end;

procedure TSamplerGUI.DeleteBankGUI(AObjectID: string);
var
  lSampleBankView: TBankView;
  lSampleBank: TSampleBank;
  lIndex: Integer;
  lNode: TTreeNode;
begin
  DBLog('start TSamplerGUI.DeleteBankGUI: ' + AObjectID);

  for lIndex := Pred(BankListGUI.Count) downto 0 do
  begin
    lSampleBankView := TBankView(FBankListGUI[lIndex]);
    if lSampleBankView.ObjectID = AObjectID then
    begin
      DBLog('Deleting BankGUI: ' + lSampleBankView.ObjectID);
      lSampleBank := lSampleBankView.Bank;
      lNode := tvTreeStructure.Items.FindNodeWithData(lSampleBank);
      if Assigned(lNode) then
      begin
        tvTreeStructure.Items.Delete(lNode);
      end;

      FBankListGUI.Remove(lSampleBankView);
    end;
  end;

  Invalidate;

  DBLog('end TSamplerGUI.DeleteBankGUI');
end; *)

constructor TSamplerGUI.Create(AOwner: TComponent);
begin
  inherited;

  FBankView := TBankView.Create(nil);
  FBankView.Parent := Self;

  FBankListGUI := TObjectList.Create;

  ChangeControlStyle(Self, [csDisplayDragImage], [], True);
end;

destructor TSamplerGUI.Destroy;
begin
  if Assigned(FBankListGUI) then
    FBankListGUI.Free;

  if Assigned(FBankView) then
    FBankView.Free;

  inherited;
end;

procedure TSamplerGUI.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TSamplerGUI.Update');

  DiffLists(
    TSampler(Subject).BankList,
    FBankListGUI,
    @CreateBankGUI,
    @DeleteBankGUI);

  ChangeSelectedBankUpdate(Subject);

  DBLog('end TSamplerGUI.Update');
end;

function TSamplerGUI.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TSamplerGUI.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

{ TChangeSelectedBankUpdate }

procedure TSamplerGUI.ChangeSelectedBankUpdate(Subject: THybridPersistentModel);
var
  lBankSelectControl: TBankSelectControl;
  lSampleSelectControl: TSampleSelectControl;
  lSampleIndex: Integer;
  lIndex: Integer;
begin
  DBLog('start ChangeSelectedBankUpdate');

  if FBankListGUI.Count > 0 then
  begin
    if Assigned(GAudioStruct.SelectedBank) then
    begin
      for lIndex := 0 to Pred(FBankListGUI.Count) do
      begin
        lBankSelectControl := TBankSelectControl(FBankListGUI[lIndex]);

        if Assigned(lBankSelectControl) then
        begin
          if lBankSelectControl.SampleBank = GAudioStruct.SelectedBank then
          begin
            if Assigned(lBankSelectControl.SampleBank) then
            begin
              tvTreeStructure.Selected := tvTreeStructure.Items.FindNodeWithData(lBankSelectControl);

              // First detach old bank
              if Assigned(GAudioStruct.OldSelectedBank) then
              begin
                for lSampleIndex := 0 to Pred(FBankView.SampleListGUI.Count) do
                begin
                  //TSample(FBankView.Bank.SampleList[lSampleIndex]).Detach(FBankView.SampleListGUI[lSampleIndex]);
                  //TSampleSelectControl(FBankView.SampleListGUI[lSampleIndex]).Model;
                end;

                GAudioStruct.OldSelectedBank.Detach(FBankView);
              end;

              // Now attach newly selected bank
              GAudioStruct.SelectedBank.Attach(FBankView);
              FBankView.Bank := GAudioStruct.SelectedBank;
            end;
          end;
        end;
      end;
    end;
  end;

  DBLog('end ChangeSelectedBankUpdate');
end;

initialization
  {$I samplergui.lrs}

end.

