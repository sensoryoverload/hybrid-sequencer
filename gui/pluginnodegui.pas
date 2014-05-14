unit pluginnodegui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, StdCtrls, ValEdit,
  ExtCtrls, Menus, ActnList, globalconst, plugin, dialcontrol, global_command,
  global, utils;

type
  TNotifyStopDragging = procedure(Sender: TObject; ADropLocation: Integer) of object;

  { TGenericPluginGUI }

  TGenericPluginGUI = class(TFrame, IObserver)
    acDeletePlugin: TAction;
    acEnabledPlugin: TAction;
    ActionList1: TActionList;
    miEnabled: TMenuItem;
    miDelete: TMenuItem;
    pnlTop: TPanel;
    pnlControls: TPanel;
    PopupMenu1: TPopupMenu;
    procedure acDeletePluginExecute(Sender: TObject);
    procedure acDeletePluginUpdate(Sender: TObject);
    procedure acEnabledPluginExecute(Sender: TObject);
    procedure acEnabledPluginUpdate(Sender: TObject);
  private
    { private declarations }
    FUpdateSubject: THybridPersistentModel;
    FObjectOwnerID: string;
    FObjectID: string;
    FModel: THybridPersistentModel;
    FOnStopDragging: TNotifyEvent;
    FPluginName: string;
    FParameterList: TStringList;
    FPluginProcessorGui: TComponent;
    FSequenceNr: Integer;
    FDragging: Boolean;
    FOldDragPosition: Integer;
    procedure DoParameterChange(Sender: TObject);
    procedure DoParameterStartChange(Sender: TObject);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(Subject: THybridPersistentModel); virtual; reintroduce;
    procedure UpdateView(AForceRedraw: Boolean = False);
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string; virtual;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    function GetModel: THybridPersistentModel; virtual;
    procedure SetModel(AModel: THybridPersistentModel); virtual;
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property ObjectID: string read GetObjectID write SetObjectID;
    property PluginName: string read FPluginName write FPluginName;
    property Model: THybridPersistentModel read FModel write FModel;
    property SequenceNr: Integer read FSequenceNr write FSequenceNr;
    property OnStopDragging: TNotifyEvent read FOnStopDragging write FOnStopDragging;
    property PluginProcessorGui: TComponent read FPluginProcessorGui write FPluginProcessorGui;
  end;

implementation

uses
  pluginhost, pluginhostgui;

{ TGenericPluginGUI }

constructor TGenericPluginGUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FParameterList := TStringList.Create;

  FDragging := False;
  pnlTop.OnMouseDown := @DoMouseDown;
  pnlTop.OnMouseUp := @DoMouseUp;
  pnlTop.OnMouseMove := @DoMouseMove;
end;

destructor TGenericPluginGUI.Destroy;
begin
  FParameterList.Free;

  inherited Destroy;
end;

procedure TGenericPluginGUI.Update(Subject: THybridPersistentModel);
begin
  FUpdateSubject := Subject;
end;

procedure TGenericPluginGUI.UpdateView(AForceRedraw: Boolean = False);
var
  lParameterIndex: Integer;
  lPlugin: TPluginNode;
begin
  if Assigned(FModel) then
  begin
    lPlugin := TPluginNode(FModel);

    for lParameterIndex := 0 to Pred(lPlugin.InputControlCount) do
    begin
      TParameterControl(FParameterList.Objects[lParameterIndex]).Value :=
        lPlugin.InputControls[lParameterIndex].Value;
    end;

    Width := (lPlugin.InputControlCount div 8) * 100 + 100;
    SequenceNr := lPlugin.SequenceNr;
    pnlTop.Caption := lPlugin.PluginName;
  end;

  Invalidate;
end;

procedure TGenericPluginGUI.Connect;
var
  lParameterIndex: Integer;
  lPlugin: TPluginNode;
  lPluginParameter: TParameterControl;
begin
  lPlugin := TPluginNode(FModel);

  // Dynamically drop components on the form
  for lParameterIndex := 0 to Pred(lPlugin.InputControlCount) do
  begin
    lPluginParameter := TParameterControl.Create(Self);
    lPluginParameter.Tag := Integer(lParameterIndex);
    lPluginParameter.OnChange := @DoParameterChange;
    lPluginParameter.OnStartChange := @DoParameterStartChange;
    lPluginParameter.Caption := lPlugin.InputControls[lParameterIndex].Caption;
    lPluginParameter.Min := lPlugin.InputControls[lParameterIndex].LowerBound;
    lPluginParameter.Max := lPlugin.InputControls[lParameterIndex].UpperBound;
    lPluginParameter.Left := 10 + (lParameterIndex div 10) * 90;
    lPluginParameter.Width := 80;
    lPluginParameter.Height := 10;
    lPluginParameter.Top := (lParameterIndex mod 8) * 20 + 30;
    lPluginParameter.Parent := pnlControls;

    FParameterList.AddObject(IntToStr(lParameterIndex), lPluginParameter);
  end;
end;

procedure TGenericPluginGUI.Disconnect;
begin
  // Delete all components from the form
end;

procedure TGenericPluginGUI.DoParameterStartChange(Sender: TObject);
var
  lGenericCommand: TGenericCommand;
begin
  lGenericCommand := TGenericCommand.Create(Self.ObjectID);
  try
    lGenericCommand.Parameter := Integer(TParameterControl(Sender).Tag);
    lGenericCommand.MidiLearn := TParameterControl(Sender).MidiMappingMode;
    lGenericCommand.ObjectID := Self.ObjectID;
    lGenericCommand.Value := TParameterControl(Sender).Value;
    lGenericCommand.Persist := True;

    GCommandQueue.PushCommand(lGenericCommand);
  except
    lGenericCommand.Free;
  end;
end;

procedure TGenericPluginGUI.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDragging := True;

    BringToFront;

    FOldDragPosition := X;
  end;
end;

procedure TGenericPluginGUI.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FDragging then
  begin
    FDragging := False;

    if Assigned(FOnStopDragging) then
    begin
      FOnStopDragging(Self);
    end;
  end;
end;

procedure TGenericPluginGUI.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FDragging then
  begin
    Self.Left := Self.Left + (X - FOldDragPosition);
  end;
end;

procedure TGenericPluginGUI.acDeletePluginExecute(Sender: TObject);
begin
  Application.QueueAsyncCall(@TPluginProcessorGUI(FPluginProcessorGui).ReleasePlugin, PtrInt(Self));
end;

procedure TGenericPluginGUI.acDeletePluginUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := True;
end;

procedure TGenericPluginGUI.acEnabledPluginExecute(Sender: TObject);
begin
  //todo
end;

procedure TGenericPluginGUI.acEnabledPluginUpdate(Sender: TObject);
begin
  // todo
  (Sender as TAction).Enabled := True;
end;

procedure TGenericPluginGUI.DoParameterChange(Sender: TObject);
var
  lGenericCommand: TGenericCommand;
begin
  lGenericCommand := TGenericCommand.Create(Self.ObjectID);
  try
    lGenericCommand.Parameter := Integer(TParameterControl(Sender).Tag);
    lGenericCommand.MidiLearn := TParameterControl(Sender).MidiMappingMode;
    lGenericCommand.ObjectID := Self.ObjectID;
    lGenericCommand.Value := TParameterControl(Sender).Value;
    lGenericCommand.Persist := False;

    GCommandQueue.PushCommand(lGenericCommand);
  except
    lGenericCommand.Free;
  end;
end;

function TGenericPluginGUI.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TGenericPluginGUI.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

function TGenericPluginGUI.GetObjectOwnerID: string;
begin
  Result := FObjectOwnerID;
end;

procedure TGenericPluginGUI.SetObjectOwnerID(const AObjectOwnerID: string);
begin
  FObjectOwnerID := AObjectOwnerID;
end;

function TGenericPluginGUI.GetModel: THybridPersistentModel;
begin
  Result := FModel;
end;

procedure TGenericPluginGUI.SetModel(AModel: THybridPersistentModel);
begin
  FModel := AModel;
end;

initialization
  {$I pluginnodegui.lrs}

end.

