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

  pattern.pas
}

unit pattern;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, graphics, global_command,
  globalconst, global, pluginhost, fx, plugin, contnrs;

type
  { TAutomationDevice }

  TAutomationDevice = class(THybridPersistentModel)
  private
    FParameterList: TObjectList;
    FDeviceId: string;
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True); reintroduce;
    destructor Destroy; override;
  published
    property DeviceId: string read FDeviceId write FDeviceId;
    property ParameterList: TObjectList read FParameterList write FParameterList;
  end;

  { TPattern }

  TPattern = class(THybridPersistentModel)
  private
    FChannelCount: Integer;
    FAutomationChannelList: TObjectList;
    FEnabled: Boolean;
    FPatternColor: TColor;
    FPatternCursor: Double;
    FRealCursorPosition: Integer;
    FLatency: Integer;
    FPitched: Boolean;
    FPosition: Integer; // Vertical position in the patterngrid
    FSelectedAutomationDeviceId: string;
    FSelectedAutomationParameterId: string;
    FText: string;
    FSyncQuantize: Boolean;
    FOkToPlay: Boolean;
    FPitch: Single;
    FPitchInv: Single;
    FRootNote: Integer;
    FMidiChannel: Integer;
    FPlaying: Boolean;
    FScheduled: Boolean;
    FPatternLength: Longint;
    FPatternName: string;
    FFileName: string; // The name of the xml file
    FPluginProcessor: TPluginProcessor;
    FVisibleTabIndex: Integer;

    FLoopStart: TLoopMarker;
    FLoopEnd: TLoopMarker;
    FLoopLength: TLoopMarker;
    FLooped: Boolean;

    procedure SetLoopEnd(AValue: TLoopMarker);
    procedure SetLoopLength(AValue: TLoopMarker);
    procedure SetLoopStart(AValue: TLoopMarker);
    procedure SetOkToPlay(const AValue: Boolean);
    procedure SetPatternColor(const AValue: TColor);
    procedure SetPosition(const AValue: Integer);
    procedure SetText(const AValue: string);
    procedure Setpitch(const Avalue: Single);
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string); virtual;
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True); reintroduce;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Initialize; override;
    procedure Finalize; override;
{    function FindAutomationParameter(APlugin: TPluginNode;
        APluginParameter: TPortParameter): TAutomationDataList;}
    function FindAutomationParameter(APluginId: string;
      APluginParameterId: string): TAutomationDataList;
    procedure DoPopulateAutomationDevices(ADeviceId: string; AParameterId: string;
      AAction: TPopulateAutomationAction);

    property PatternColor: TColor read FPatternColor write SetPatternColor;
    property PatternCursor: Double read FPatternCursor write FPatternCursor;
    property RealCursorPosition: Integer read FRealCursorPosition write FRealCursorPosition;

    procedure ProcessInit; virtual; abstract;
    procedure Process(ABuffer: PSingle; AFrameIndex: Integer; AFrameCount: Integer); virtual;
    procedure ProcessAdvance; virtual;
    procedure ProcessAutomation; virtual;
    function Latency: Integer; virtual;

    property ChannelCount: Integer read FChannelCount write FChannelCount;
    property PitchInv: Single read FPitchInv write FPitchInv;
    property VisibleTabIndex: Integer read FVisibleTabIndex write FVisibleTabIndex;
  published
    property PluginProcessor: TPluginProcessor read FPluginProcessor write FPluginProcessor;
    property AutomationChannelList: TObjectList read FAutomationChannelList write FAutomationChannelList;
    property SyncQuantize: Boolean read FSyncQuantize write FSyncQuantize;
    property Position: Integer read FPosition write SetPosition;
    property Text: string read FText write SetText;
    property OkToPlay: Boolean read FOkToPlay write SetOkToPlay;
    property Pitch: Single read FPitch write SetPitch default 1;
    property Pitched: Boolean read FPitched write FPitched default False;
    property RootNote: Integer read FRootNote write FRootNote default 0;
    property MidiChannel: Integer read FMidiChannel write FMidiChannel;
    property Playing: Boolean read FPlaying write FPlaying default False;
    property Scheduled: Boolean read FScheduled write FScheduled default False;
    property PatternLength: Longint read FPatternLength write FPatternLength;
    property PatternName: string read FPatternName write FPatternName;
    property FileName: string read FFileName write FFileName;
    property LoopStart: TLoopMarker read FLoopStart write SetLoopStart;
    property LoopEnd: TLoopMarker read FLoopEnd write SetLoopEnd;
    property LoopLength: TLoopMarker read FLoopLength write SetLoopLength;
    property Looped: Boolean read FLooped write FLooped;
    property Enabled: Boolean read FEnabled write FEnabled;
    property SelectedAutomationDeviceId: string read FSelectedAutomationDeviceId write FSelectedAutomationDeviceId;
    property SelectedAutomationParameterId: string read FSelectedAutomationParameterId write FSelectedAutomationParameterId;
  end;

  { TPatternCommand }

  TPatternCommand = class(TCommand)
  private
    FPattern: TPattern;
  public
    procedure Initialize; override;
  end;

  { TCreateAutomationDataCommand }

  TCreateAutomationDataCommand = class(TPatternCommand)
  private
    FLocation: Integer;
    FDeviceId: string;
    FParameterId: string;
    FDataValue: Single;
    FStoredObjectId: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  public
    property Location: Integer read FLocation write FLocation;
    property DeviceId: string read FDeviceId write FDeviceId;
    property ParameterId: string read FParameterId write FParameterId;
    property DataValue: Single read FDataValue write FDataValue;
  end;

  { TEditAutomationDataCommand }

  TEditAutomationDataCommand = class(TPatternCommand)
  private
    FLocation: Integer;
    FDeviceId: string;
    FOldLocation: Integer;
    FOldDataValue: Single;
    FParameterId: string;
    FDataValue: Single;
    FStoredObjectId: string;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  public
    property Location: Integer read FLocation write FLocation;
    property DeviceId: string read FDeviceId write FDeviceId;
    property ParameterId: string read FParameterId write FParameterId;
    property DataValue: Single read FDataValue write FDataValue;
  end;

  { TUpdateLoopMarkerCommand }
(*
  TUpdateLoopMarkerCommand = class(TPatternCommand)
  private
    FDataType: TLoopMarkerType;
    FLocation: Integer;
    FOldLocation: Integer;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Location: Integer read FLocation write FLocation;
    property DataType: TLoopMarkerType read FDataType write FDataType;
  end;
*)
  { TLoadPatternCommand }

  TLoadPatternCommand = class(TPatternCommand)
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
  end;

  { TSavePatternCommand }

  TSavePatternCommand = class(TPatternCommand)
  private
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
  end;

implementation

uses
  utils, DOM, XMLWrite, XMLRead, audiostructure;

{ TAutomationDevice }

constructor TAutomationDevice.Create(AObjectOwner: string; AMapped: Boolean);
begin
  FParameterList := TObjectList.Create(False);
end;

destructor TAutomationDevice.Destroy;
var
  lIndex: Integer;
begin
  for lIndex := 0 to Pred(FParameterList.Count) do
  begin
    FParameterList[lIndex].Free;
  end;
  FParameterList.Free;

  inherited Destroy;
end;

{ TEditAutomationDataCommand }

procedure TEditAutomationDataCommand.DoExecute;
var
  lAutomationDataList: TAutomationDataList;
begin
  FPattern.BeginUpdate;

  lAutomationDataList := FPattern.FindAutomationParameter(FDeviceId, FParameterId);
  lAutomationDataList.First;
  while not lAutomationDataList.Eof do
  begin
    if lAutomationDataList.CurrentAutomationData.ObjectID = ObjectID then
    begin
      FOldLocation :=
        lAutomationDataList.CurrentAutomationData.Location;

      FOldDataValue :=
        lAutomationDataList.CurrentAutomationData.DataValue;
      FStoredObjectId := ObjectID;

      if (
           (FLocation > lAutomationDataList.PreviousAutomationData.Location) and
           (FLocation < lAutomationDataList.NextAutomationData.Location)
         )
         or
         (
           (lAutomationDataList.Index = 0) and
           (FLocation < lAutomationDataList.NextAutomationData.Location)
         )
         or
         (
           (lAutomationDataList.Index = Pred(lAutomationDataList.List.Count)) and
           (FLocation > lAutomationDataList.PreviousAutomationData.Location)
         ) then
      begin
        lAutomationDataList.CurrentAutomationData.Location := FLocation;
        lAutomationDataList.CurrentAutomationData.DataValue := FDataValue;
        break;
      end;
    end;

    lAutomationDataList.Next;
  end;

  FPattern.EndUpdate;
end;

procedure TEditAutomationDataCommand.DoRollback;
var
  lAutomationDataList: TAutomationDataList;
begin
  lAutomationDataList := FPattern.FindAutomationParameter(FDeviceId, FParameterId);
  lAutomationDataList.First;
  while not lAutomationDataList.Eof do
  begin
    if lAutomationDataList.CurrentAutomationData.ObjectID = FStoredObjectId then
    begin
      lAutomationDataList.CurrentAutomationData.Location := FOldLocation;
      lAutomationDataList.CurrentAutomationData.DataValue := FOldDataValue;
      break;
    end;

    lAutomationDataList.Next;
  end;
end;

{ TCreateAutomationDataCommand }

procedure TCreateAutomationDataCommand.DoExecute;
var
  lAutomationData: TAutomationData;
  lAutomationParameter: TAutomationDataList;
begin
  FPattern.BeginUpdate;

  // First find if there is already a automation track
  lAutomationParameter := FPattern.FindAutomationParameter(FDeviceId, FParameterId);

  // if not create one
  if not Assigned(lAutomationParameter) then
  begin
    lAutomationParameter := TAutomationDataList.Create(FPattern.ObjectID);
  end;

  // create automationpoint on found/created automationtrack
  lAutomationData := TAutomationData.Create(lAutomationParameter.ObjectID);
  lAutomationData.Location := FLocation;
  lAutomationData.DataValue := FDataValue;

  FStoredObjectId := lAutomationData.ObjectID;

  lAutomationParameter.AddAutomation(lAutomationData);

  FPattern.EndUpdate;
end;

procedure TCreateAutomationDataCommand.DoRollback;
var
  lAutomationDataList: TAutomationDataList;
begin
  FPattern.BeginUpdate;

  lAutomationDataList := FPattern.FindAutomationParameter(FDeviceId, FParameterId);

  if Assigned(lAutomationDataList) then
  begin
    lAutomationDataList.First;
    while not lAutomationDataList.Eof do
    begin
      if lAutomationDataList.CurrentAutomationData.ObjectID =
        FStoredObjectId then
      begin
        lAutomationDataList.DeleteAutomation(
          lAutomationDataList.CurrentAutomationData);

        break;
      end;

      lAutomationDataList.Next;
    end;
  end;

  FPattern.EndUpdate;
end;

{ TSavePatternCommand }

procedure TSavePatternCommand.DoExecute;
var
  xdoc: TXMLDocument;
  RootNode: TDOMNode;
begin
  FPattern.BeginUpdate;

  //create a document
  xdoc := TXMLDocument.create;

  //create a root node
  RootNode := xdoc.CreateElement('root');
  Xdoc.Appendchild(RootNode);

  //create a pattern node
  RootNode:= xdoc.DocumentElement;

  // pass xmldocument to iterate function
  // pass mode to iterate (Store,Retrieve) ie. Save to xml or load from xml
  // load from xml should create instance of property objects
  FPattern.SaveToXML(FPattern, 0, RootNode);

  // write to XML
  writeXMLFile(xDoc, FPattern.FileName);

  // free memory
  Xdoc.free;

  FPattern.EndUpdate;
end;

procedure TSavePatternCommand.DoRollback;
begin
  inherited DoRollback;
end;

{ TLoadPatternCommand }

procedure TLoadPatternCommand.DoExecute;
var
  xdoc: TXMLDocument;
  RootNode: TDOMNode;
begin
  FPattern.BeginUpdate;

  { TODO
    erase current pattern
  }

  ReadXMLFile(xDoc, FPattern.FileName);
  RootNode := xDoc.DocumentElement.FirstChild;
  if RootNode <> nil then
  begin
    FPattern.LoadFromXML(RootNode);
    FPattern.RecurseNotify(FPattern);
  end;

  FPattern.EndUpdate;
end;

procedure TLoadPatternCommand.DoRollback;
begin
  FPattern.BeginUpdate;

  {for i := Pred(GAudioStruct.Tracks.Count) downto 0 do
  begin
    GAudioStruct.Tracks.Remove(GAudioStruct.Tracks[i]);
  end; }

  FPattern.EndUpdate;
end;

{ TPattern }

procedure TPattern.SetPatternColor(const AValue: TColor);
begin
  FPatternColor := AValue;
end;

procedure TPattern.SetOkToPlay(const AValue: Boolean);
begin
  FOkToPlay := AValue;
end;

procedure TPattern.SetLoopEnd(AValue: TLoopMarker);
begin
  if FLoopEnd = AValue then Exit;
  FLoopEnd := AValue;
end;

procedure TPattern.SetLoopLength(AValue: TLoopMarker);
begin
  if FLoopLength = AValue then Exit;
  FLoopLength := AValue;
end;

procedure TPattern.SetLoopStart(AValue: TLoopMarker);
begin
  if FLoopStart = AValue then Exit;
  FLoopStart := AValue;
end;

procedure TPattern.SetPosition(const AValue: Integer);
begin
  FPosition := AValue;
end;

procedure TPattern.SetText(const AValue: string);
begin
  FText := AValue;
end;

procedure TPattern.Setpitch(const Avalue: Single);
begin
  if Avalue > 2 then
    FPitch := 2
  else if Avalue < 0.5 then
    FPitch := 0.5
  else
    FPitch := Avalue;

  FPitchInv := 1 / FPitch;
end;

procedure TPattern.DoCreateInstance(var AObject: TObject; AClassName: string);
var
  lAutomationDataList: TAutomationDataList;
begin
  DBLog('start TPattern.DoCreateInstance');

  if AClassName = 'TAutomationDataList' then
  begin
    lAutomationDataList := TAutomationDataList.Create(ObjectID);
    lAutomationDataList.ObjectOwnerID := ObjectID;

    AutomationChannelList.Add(lAutomationDataList);

    AObject := lAutomationDataList;
  end;

  DBLog('end TPattern.DoCreateInstance');
end;

procedure TPattern.Initialize;
var
  lIndex: Integer;
  lIndex2: Integer;
  lAutomationDataList: TAutomationDataList;
  lAutomationData: TAutomationData;
begin
  OkToPlay := True;

  DBLog(Format('AutomationChannelList Count %d', [AutomationChannelList.Count]));

  for lIndex := 0 to Pred(AutomationChannelList.Count) do
  begin
    lAutomationDataList := TAutomationDataList(AutomationChannelList[lIndex]);

    // Reconnect to plugins
    lAutomationDataList.Plugin :=
      FPluginProcessor.FindDeviceById(lAutomationDataList.DeviceId);

    lAutomationDataList.PluginParameter :=
      FPluginProcessor.FindParameterByDeviceAndParameterId(
        lAutomationDataList.DeviceId,
        lAutomationDataList.ParameterId);

    DBLog(Format('AutomationDataList Count %d', [lAutomationDataList.List.Count]));

    for lIndex2 := 0 to Pred(lAutomationDataList.List.Count) do
    begin
      DBLog(Format('AutomationData DataValue %f Location %d', [
        TAutomationData(lAutomationDataList.List[lIndex2]).DataValue,
        TAutomationData(lAutomationDataList.List[lIndex2]).Location]));
    end;
  end;

  Notify;
end;

procedure TPattern.Finalize;
begin
  // Stop playing
  OkToPlay := False;

  Notify;
end;

function TPattern.FindAutomationParameter(APluginId: string;
  APluginParameterId: string): TAutomationDataList;
var
  lParameterIndex: Integer;
  lAutomationParameter: TAutomationDataList;
begin
  Result := nil;

  for lParameterIndex := 0 to Pred(FAutomationChannelList.Count) do
  begin
    lAutomationParameter := TAutomationDataList(FAutomationChannelList[lParameterIndex]);
    DBLog(format('Search automation parameter %s = %s, %s = %s?', [
    lAutomationParameter.DeviceId, APluginId, lAutomationParameter.ParameterId, APluginParameterId]));

    if (lAutomationParameter.DeviceId = APluginId) and
      (lAutomationParameter.ParameterId = APluginParameterId) then
    begin
      DBLog(format('Found automation parameter %s = %s, %s = %s?', [
        lAutomationParameter.DeviceId, APluginId, lAutomationParameter.ParameterId, APluginParameterId]));

      Result := lAutomationParameter;
      break;
    end;
  end;
end;

{
  This method will given to the PluginProcessor and it will fire
  for each inserted or deleted parameter
}
procedure TPattern.DoPopulateAutomationDevices(ADeviceId: string;
  AParameterId: string; AAction: TPopulateAutomationAction);
var
  lAutomationParameter: TAutomationDataList;
  lPlugin: TPluginNode;
  lPluginParameter: TPortParameter;
begin
  if not Loading then
  begin;
    lPlugin := TPluginNode(GObjectMapper.GetModelObject(ADeviceId));
    lPluginParameter := TPortParameter(GObjectMapper.GetModelObject(AParameterId));

    // Create/delete automationdatalist based on incoming parameters
    case AAction of
    paaInsert:
      begin
        lAutomationParameter := TAutomationDataList.Create(Self.ObjectID);

        lAutomationParameter.Plugin := lPlugin;
        lAutomationParameter.DeviceId := lPlugin.PluginName;
        lAutomationParameter.PluginParameter := lPluginParameter;
        lAutomationParameter.ParameterId := lPluginParameter.Caption;
        FAutomationChannelList.Add(lAutomationParameter);
      end;
    paaDelete:
      begin
        lAutomationParameter := FindAutomationParameter(ADeviceId, AParameterId);
        if Assigned(lAutomationParameter) then
        begin
          FAutomationChannelList.Extract(lAutomationParameter);
          lAutomationParameter.Free;
        end;
      end;
    end;
  end;
end;

procedure TPattern.Process(ABuffer: PSingle; AFrameIndex: Integer;
  AFrameCount: Integer);
begin
  if (AFrameIndex = 0) or FLooped then
  begin
    FLooped := False;
  end;
end;

constructor TPattern.Create(AObjectOwner: string; AMapped: Boolean = True);
begin
  DBLog('start TPattern.Create');

  inherited Create(AObjectOwner, AMapped);

  FOnCreateInstanceCallback := @DoCreateInstance;

  FAutomationChannelList := TObjectList.create(False);

  FPluginProcessor := TPluginProcessor.Create(GSettings.Frames, AObjectOwner, AMapped);
  FPluginProcessor.OnPopulateAutomationDevices := @DoPopulateAutomationDevices;

  FOkToPlay := False;

  // Defaults
  FLoopStart := TLoopMarker.Create(AObjectOwner, ltStart);
  FLoopEnd := TLoopMarker.Create(AObjectOwner, ltEnd);
  FLoopLength := TLoopMarker.Create(AObjectOwner, ltLength);

  FLoopStart.Value := 0;
  FLoopEnd.Value := 0;

  FLooped := False;
  FEnabled := True;

  FChannelCount := 1; // Default

  FVisibleTabIndex := 0;

  DBLog('end TPattern.Create');
end;

destructor TPattern.Destroy;
var
  lIndex: Integer;
begin
  FPluginProcessor.Free;

  for lIndex := 0 to Pred(FAutomationChannelList.Count) do
  begin
    FAutomationChannelList[lIndex].Free;
  end;
  FAutomationChannelList.Free;

  if Assigned(FLoopStart) then
    FLoopStart.Free;
  if Assigned(FLoopEnd) then
    FLoopEnd.Free;
  if Assigned(FLoopLength) then
    FLoopLength.Free;

  inherited Destroy;
end;

procedure TPattern.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{
  Advance cursor
  Todo: when a pattern switch is done, the cursor stops
  and continues when switched to the original pattern.
  This would start this pattern possibly halfway it's length
  and that would normally not be desired. Problem is that
  simply resetting it to 0 would put the pattern out of sync
  with other tracks. Not sure this makes any sense but it
  has to be tried.
}
procedure TPattern.ProcessAdvance;
begin
  FPatternCursor := FPatternCursor + GAudioStruct.BPMScale;
  if FPatternCursor > FLoopEnd.Value then
  begin
    FPatternCursor := FPatternCursor - (FLoopEnd.Value - FLoopStart.Value);
    FLooped := True;
  end;

  RealCursorPosition := Round(PatternCursor);
end;

procedure TPattern.ProcessAutomation;
var
  lIndex: Integer;
  lFirstAutomationData: TAutomationData;
  lLeftIndex: Integer;
  lLeftAutomationData: TAutomationData;
  lLeftLocation: Integer;
  lLeftValue: Single;
  lRightIndex: Integer;
  lRightAutomationData: TAutomationData;
  lRightLocation: Integer;
  lRightValue: Single;
  lCalculatedValue: Single;
  lPlugin: TPluginNode;
  lPortIndex: Integer;
  lPort: TPortParameter;
  lAutomationParameter: TAutomationDataList;
  lParameterIndex: Integer;
begin
  {
    ipv door alle plugins te itereren is het efficient om door de
    automationlists te itereren.
  }
  for lParameterIndex := 0 to Pred(FAutomationChannelList.Count) do
  begin
    lAutomationParameter := TAutomationDataList(FAutomationChannelList[lParameterIndex]);

    //  This reference could be much more efficient when stored in parameterlist
    lPlugin := lAutomationParameter.Plugin;

    // Only set automation when there are automation events
    if lAutomationParameter.List.Count > 0 then
    begin
      if Assigned(lPlugin) then
      begin
        for lPortIndex := 0 to Pred(lPlugin.InputControlCount) do
        begin
          lPort := lPlugin.InputControls[lPortIndex];

          if lAutomationParameter.PluginParameter = lPort then
          begin
            // Cursor before first automation event so just use that value
            lFirstAutomationData := TAutomationData(lAutomationParameter.List[0]);
            if FPatternCursor < lFirstAutomationData.Location then
            begin
              lCalculatedValue :=
                lFirstAutomationData.DataValue * (lport.UpperBound - lPort.LowerBound);

              lPort.Value := lCalculatedValue;

              break;
            end
            else
            begin
              // Find previous and next automation data points and do a linear interpolate.
              lLeftIndex := 0;
              for lIndex := 0 to Pred(lAutomationParameter.List.Count) do
              begin
                if TAutomationData(lAutomationParameter.List[lIndex]).Location > FPatternCursor then
                begin
                  if lIndex = 0 then
                  begin
                    lLeftIndex := 0
                  end
                  else
                  begin
                    lLeftIndex := Pred(lIndex);
                  end;

                  break;
                end;
              end;

              lRightIndex := Succ(lLeftIndex);

              if lRightIndex > Pred(lAutomationParameter.List.Count) then
              begin
                lRightIndex := Pred(lAutomationParameter.List.Count);
              end;

              lLeftAutomationData := TAutomationData(lAutomationParameter.List[lLeftIndex]);
              lLeftLocation := lLeftAutomationData.Location;

              lRightAutomationData := TAutomationData(lAutomationParameter.List[lRightIndex]);
              lRightLocation := lRightAutomationData.Location;

              if (lLeftLocation < FPatternCursor) and (lRightLocation > FPatternCursor) then
              begin
                lLeftValue :=
                  lLeftAutomationData.DataValue * (lport.UpperBound - lPort.LowerBound);

                lRightValue :=
                  lRightAutomationData.DataValue * (lport.UpperBound - lPort.LowerBound);

                if lRightLocation - lLeftLocation <> 0 then
                begin
                  if lRightLocation <> lLeftLocation then
                  begin
                    // Linear interpolate to find the actual automation value at the cursor
                    lCalculatedValue :=
                      lLeftValue + (lRightValue - lLeftValue) *
                      (
                        (FPatternCursor - lLeftLocation)
                        /
                        (lRightLocation - lLeftLocation)
                      );
                  end
                  else
                  begin
                    lCalculatedValue := lPort.DefaultValue;
                  end;

                  lPort.Value := lCalculatedValue;

                  //dblog(format('value %f', [lPort.Value^]));

                  dblog(format('Cursor %f Leftvalue %f rightvalue %f leftlocation %d rightlocation %d value %f', [
                    FPatternCursor,
                    lLeftValue,
                    lRightValue,
                    lLeftLocation,
                    lRightLocation,
                    lPort.Value]));
                end;

                break;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TPattern.Latency: Integer;
begin
  Result := 0;
end;

{ TPatternCommand }

procedure TPatternCommand.Initialize;
begin
  FPattern := TPattern(GObjectMapper.GetModelObject(ObjectOwner));
end;

end.

