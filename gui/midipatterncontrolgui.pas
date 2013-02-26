unit midipatterncontrolgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, ExtCtrls, StdCtrls,
  PairSplitter, globalconst, midipatterngui, midi, midigui, sampler, samplegui,
  bankgui, global, patternoverview;

type
{ TMidiPatternControlGUI }

  TMidiPatternControlGUI = class(TFrame, IObserver)
    cbMidiChannel: TComboBox;
    cbQuantize: TComboBox;
    lblMidiChannel: TLabel;
    lblQuantize: TLabel;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    pnlMidiGrid: TPanel;
    pnlMidiSettings: TPanel;

    procedure cbQuantizeChange(Sender: TObject);
    procedure cbSampleBankSelecterChange(Sender: TObject);
    procedure cbMidiChannelChange(Sender: TObject);
  private
    { private declarations }
    FMidiPatternGUI: TMidiPatternGUI;
    FMidiOverview: TMidiPatternOverview;

    FRootNote: Integer;
    FMidiChannel: Integer;

    FConnected: Boolean;
    FObjectOwnerID: string;
    FObjectID: string;
    FModel: TMidiPattern;
    FObjectOwner: TObject;
  protected
    procedure DoMidiZoom(ALeftPercentage, ARightPercentage: single);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure Update(Subject: THybridPersistentModel); reintroduce;

    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string; virtual;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    function GetModel: THybridPersistentModel;
    procedure SetModel(AModel: THybridPersistentModel);
    property Connected: Boolean read FConnected;
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property ObjectID: string read GetObjectID write SetObjectID;
    property MidiPatternGUI: TMidiPatternGUI read FMidiPatternGUI write FMidiPatternGUI;
    property RootNote: Integer read FRootNote write FRootNote default 0;
    property MidiChannel: Integer read FMidiChannel write FMidiChannel;
    property Model: THybridPersistentModel read GetModel write SetModel;
    property ObjectOwner: TObject read FObjectOwner write FObjectOwner;
  end;

implementation

uses utils, global_command, track;

{ TMidiPatternControlGUI }

constructor TMidiPatternControlGUI.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);

  FConnected := False;

  FMidiPatternGUI := TMidiPatternGUI.Create(nil);

  cbQuantize.Items.Add('None');
  cbQuantize.Items.Add('4 bars');
  cbQuantize.Items.Add('2 bars');
  cbQuantize.Items.Add('1 bar');
  cbQuantize.Items.Add('1/2');
  cbQuantize.Items.Add('1/3');
  cbQuantize.Items.Add('1/4');
  cbQuantize.Items.Add('1/6');
  cbQuantize.Items.Add('1/8');
  cbQuantize.Items.Add('1/16');
  cbQuantize.Items.Add('1/32');

  cbQuantize.ItemIndex := 3;

  cbMidiChannel.Items.Add('1');
  cbMidiChannel.Items.Add('2');
  cbMidiChannel.Items.Add('3');
  cbMidiChannel.Items.Add('4');
  cbMidiChannel.Items.Add('5');
  cbMidiChannel.Items.Add('6');
  cbMidiChannel.Items.Add('7');
  cbMidiChannel.Items.Add('8');
  cbMidiChannel.Items.Add('9');
  cbMidiChannel.Items.Add('10');
  cbMidiChannel.Items.Add('11');
  cbMidiChannel.Items.Add('12');
  cbMidiChannel.Items.Add('13');
  cbMidiChannel.Items.Add('14');
  cbMidiChannel.Items.Add('15');
  cbMidiChannel.Items.Add('16');

  pnlMidiGrid.Align := alNone;
  pnlMidiGrid.Align := alClient;

  FMidiPatternGUI.Align := alClient;
  FMidiPatternGUI.Parent := nil;
  FMidiPatternGUI.Parent := pnlMidiGrid;

  FMidiOverview := TMidiPatternOverview.Create(nil);
  FMidiOverview.ZoomCallback := @DoMidiZoom;
  FMidiOverview.Width := 100;
  FMidiOverview.Height := 20;
  FMidiOverview.Left := 5;
  FMidiOverview.Top := 1;
  FMidiOverview.Parent := pnlMidiSettings;

  DoMidiZoom(0, 100);
end;

destructor TMidiPatternControlGUI.Destroy;
begin
  FMidiPatternGUI.Free;
  FMidiOverview.Free;

  inherited Destroy;
end;

procedure TMidiPatternControlGUI.Connect;
begin
  if Assigned(FModel) then
  begin
    FModel.Attach(FMidiPatternGUI);

    FModel.Attach(FMidiOverview);

    FMidiPatternGUI.ZoomFactorX := 1000;
    FMidiPatternGUI.ZoomFactorY := 1000;
    FMidiPatternGUI.QuantizeSetting := FModel.QuantizeSetting;
    FMidiPatternGUI.MidiChannel := FModel.MidiChannel;

    FConnected := True;
  end;
end;

procedure TMidiPatternControlGUI.Disconnect;
begin
  if Assigned(FModel) then
  begin
    FMidiOverview.Disconnect;
    FModel.Detach(FMidiOverview);

    FMidiPatternGUI.Disconnect;
    FModel.Detach(FMidiPatternGUI);

    FConnected := False;
  end;
end;

procedure TMidiPatternControlGUI.DoMidiZoom(ALeftPercentage,
  ARightPercentage: single);
begin
  FMidiPatternGUI.ZoomFactorX := 100 / (ARightPercentage - ALeftPercentage);
  FMidiPatternGUI.Offset := - Round(ALeftPercentage);

  FMidiPatternGUI.Invalidate;
end;

procedure TMidiPatternControlGUI.Update(Subject: THybridPersistentModel);
begin
  DBLog('start TPatternControls.Update');

  if TTrack(Subject.ObjectOwner).PatternList.IndexOf(Subject) = -1 then
  begin
    //Parent := nil;
  end;

  if cbMidiChannel.ItemIndex <> TMidiPattern(Subject).MidiChannel then
  begin
    cbMidiChannel.ItemIndex := TMidiPattern(Subject).MidiChannel;
  end;

  if cbQuantize.ItemIndex <> TMidiPattern(Subject).QuantizeSetting then
  begin
    cbQuantize.ItemIndex :=  TMidiPattern(Subject).QuantizeSetting;
  end;

  DBLog('end TPatternControls.Update');
end;

function TMidiPatternControlGUI.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TMidiPatternControlGUI.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

function TMidiPatternControlGUI.GetObjectOwnerID: string;
begin
  Result := FObjectOwnerID;
end;

procedure TMidiPatternControlGUI.SetObjectOwnerID(const AObjectOwnerID: string
  );
begin
  FObjectOwnerID := AObjectOwnerID;
end;

function TMidiPatternControlGUI.GetModel: THybridPersistentModel;
begin
  Result := THybridPersistentModel(FModel);
end;

procedure TMidiPatternControlGUI.SetModel(AModel: THybridPersistentModel);
begin
  FModel := TMidiPattern(AModel);
end;

procedure TMidiPatternControlGUI.cbQuantizeChange(Sender: TObject);
var
  lQuantizeSettingCommand: TQuantizeSettingCommand;
begin

  if Assigned(FMidiPatternGUI) then
  begin
    FMidiPatternGUI.QuantizeSetting := cbQuantize.ItemIndex;

    case FMidiPatternGUI.QuantizeSetting of
    0: FMidiPatternGUI.QuantizeValue := -1;
    1: FMidiPatternGUI.QuantizeValue := 22050 * 4;
    2: FMidiPatternGUI.QuantizeValue := 22050 * 2;
    3: FMidiPatternGUI.QuantizeValue := 22050;
    4: FMidiPatternGUI.QuantizeValue := 22050 / 2;
    5: FMidiPatternGUI.QuantizeValue := 22050 / 3;
    6: FMidiPatternGUI.QuantizeValue := 22050 / 4;
    7: FMidiPatternGUI.QuantizeValue := 22050 / 6;
    8: FMidiPatternGUI.QuantizeValue := 22050 / 8;
    9: FMidiPatternGUI.QuantizeValue := 22050 / 16;
    10: FMidiPatternGUI.QuantizeValue := 22050 / 32;
    end;
  end;

  lQuantizeSettingCommand := TQuantizeSettingCommand.Create(FObjectID);
  try
    lQuantizeSettingCommand.QuantizeSetting := cbQuantize.ItemIndex;

    GCommandQueue.PushCommand(lQuantizeSettingCommand);

  except
    lQuantizeSettingCommand.Free;
  end;
end;

procedure TMidiPatternControlGUI.cbSampleBankSelecterChange(Sender: TObject);
begin
  // TODO change used bank
end;

procedure TMidiPatternControlGUI.cbMidiChannelChange(Sender: TObject);
var
  lChangeMidiChannelCommand: TChangeMidiChannelCommand;
begin
  lChangeMidiChannelCommand := TChangeMidiChannelCommand.Create(FObjectID);
  try
    lChangeMidiChannelCommand.MidiChannel := cbMidiChannel.ItemIndex;

    GCommandQueue.PushCommand(lChangeMidiChannelCommand);
  except
    lChangeMidiChannelCommand.Free;
  end;
end;

initialization
  {$I midipatterncontrolgui.lrs}

end.

