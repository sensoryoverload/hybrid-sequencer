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

  plugin.pas
}

unit plugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ContNrs, globalconst, utils, global_command, global,
  ladspaloader;

type
  TPluginNodeType = (pntSource, pntSink, pntPlugin);

  TApplyProc = procedure of Object;

  TBasePlugin = class(TObject)
  public
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); virtual; abstract;
  end;

  { TPluginParameter }

  TPluginParameter = class
    Name: string;
    Hint: string;
  end;

  { TPluginParameterList }

  TPluginParameterList = class(TObjectList)
  protected
    function GetPluginParameters(I: Integer): TPluginParameter;
    procedure SetPluginParameters(I: Integer; APluginParameter: TPluginParameter);
  public
    property PluginParameters[I: Integer]: TPluginParameter read GetPluginParameters write SetPluginParameters;
  end;

  { TPluginNode }

  TPluginNode = class(THybridPersistentModel)
  private
    FChilds: TObjectList; // multiple TPluginNodes
    FPortList: TObjectList;
    FParent: TPluginNode;
    FMixBuffer: PSingle;
    FParameterList: TObjectList;
    FReturnBuffer: PSingle;
    FMidiBuffer: TMidiBuffer;
    FBuffer: PSingle;
    FFrames: Integer;
    FNodeType: TPluginNodeType;
    FPluginName: string;
    FCached: Boolean;
    FSelected: Boolean;
  protected
    procedure DoCreateInstance(var AObject: TObject; AClassName: string);
  public
    constructor Create(AObjectOwnerID: string; AMapped: Boolean = True);
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Instantiate; virtual;
    procedure Activate; virtual;
    procedure Deactivate; virtual;
    procedure Clean; virtual;
    procedure ApplyToAll(AApplyProc: TApplyProc);
    function Execute(AFrames: Integer): PSingle;
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); virtual; abstract;
    procedure Clear;
    property MixBuffer: PSingle read FMixBuffer write FMixBuffer;
    property PortList: TObjectList read FPortList write FPortList;
    property Parent: TPluginNode read FParent write FParent;
    property NodeType: TPluginNodeType read FNodeType write FNodeType;
    property Buffer: psingle read FBuffer write FBuffer;
    property Childs: TObjectList read FChilds write FChilds;
  published
    property PluginName: string read FPluginName write FPluginName;
    property Frames: Integer read FFrames write FFrames;
    property Selected: Boolean read FSelected write FSelected;
  end;

  { TScriptNode }

  TScriptNode = class(TPluginNode)
  private
    constructor Create(AObjectOwnerID: string; AMapped: Boolean = True);
  public
    destructor Destroy; override;
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); override;
  end;

  { TLADSPANode }

  TLADSPANode = class(TPluginNode)
  public
    procedure Instantiate; override;
    procedure Activate; override;
    procedure Deactivate; override;
    procedure Clean; override;
    procedure LoadByID(AId: Integer);
    procedure LoadByName(AName: string);
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); override;
  end;

  { TMementoNode }

  TMementoNode = class(TPluginNode)
  public
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); override;
  end;

  { TInternalNode }

  TInternalNode = class(TPluginNode)
  end;

  { TExternalNode }

  TExternalNode = class(TInternalNode)
  private
  public
    constructor Create(AObjectOwnerID: string);
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); override;
  end;

  { TAudioOutNode }

  TAudioOutNode = class(TInternalNode)
  private
  public
    constructor Create(AObjectOwnerID: string);
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); override;
  end;

  { TAudioInNode }

  TAudioInNode = class(TInternalNode)
  private
  public
    constructor Create(AObjectOwnerID: string);
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); override;
  end;

  { TMidiOutNode }

  TMidiOutNode = class(TInternalNode)
  private
  public
    constructor Create(AObjectOwnerID: string);
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); override;
  end;

  { TMidiInNode }

  TMidiInNode = class(TInternalNode)
  private
  public
    constructor Create(AObjectOwnerID: string);
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); override;
  end;

implementation

uses
  pluginhost;

{ TPluginCatalog }

procedure TPluginParameterList.SetPluginParameters(I: Integer; APluginParameter: TPluginParameter);
begin
  Items[i] := APluginParameter;
end;

function TPluginParameterList.GetPluginParameters(I: Integer): TPluginParameter;
begin
  Result := TPluginParameter( Items[i] );
end;

{ TPlugin }

procedure TLADSPANode.Instantiate;
begin
  //  FLADSPA.Instantiate
end;

procedure TLADSPANode.Activate;
begin
  inherited Activate;

//  FLADSPA.Activate
end;

procedure TLADSPANode.Deactivate;
begin
  inherited Deactivate;
end;

procedure TLADSPANode.Clean;
begin
  //  FLADSPA.Clean
end;

procedure TLADSPANode.LoadByID(AId: Integer);
begin
  // Load by LADSPA ID, these should be unique
end;

procedure TLADSPANode.LoadByName(AName: string);
begin
  // GLadspaPluginFactory.Discover;
end;

procedure TLADSPANode.Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer);
begin
  //  FLADSPA.Run
end;

{ TAudioOutNode }

constructor TAudioOutNode.Create(AObjectOwnerID: string);
begin
  inherited Create(AObjectOwnerID);

  GObjectMapper.SetModelObjectID(Self, '{C83219B8-4ABC-4570-A65F-DDC31E61BE15}');

  NodeType := pntSink;
end;

procedure TAudioOutNode.Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer);
begin
  //
end;

{ TMidiInNode }

constructor TMidiInNode.Create(AObjectOwnerID: string);
begin
  inherited Create(AObjectOwnerID);

  GObjectMapper.SetModelObjectID(Self, '{55D760C9-C10F-4BF1-82F7-49A25CA3C03E}');

  NodeType := pntSource;
end;

procedure TMidiInNode.Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer);
begin
  //
end;

{ TMidiOutNode }

constructor TMidiOutNode.Create(AObjectOwnerID: string);
begin
  inherited Create(AObjectOwnerID);

  GObjectMapper.SetModelObjectID(Self, '{1294039B-A480-418F-9AE0-BE9C2A15755D}');

  NodeType := pntSink;
end;

procedure TMidiOutNode.Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer);
var
  i: Integer;
begin
  for i := 0 to Pred(AFrames) do
  begin
    FBuffer[i] := FMixBuffer[i];
  end;
end;

{ TAudioInNode }

constructor TAudioInNode.Create(AObjectOwnerID: string);
begin
  inherited Create(AObjectOwnerID);

  GObjectMapper.SetModelObjectID(Self, '{0485096E-A098-48FB-8D84-792936163D0D}');
  NodeType := pntSource;
end;

procedure TAudioInNode.Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer);
var
  i: Integer;
begin
  // Just copy from FBuffer as this is the external input and just to be safe, should not
  // be referenced.
  for i := 0 to Pred(AFrames) do
  begin
    FMixBuffer[i] := FBuffer[i];
  end;
end;

{ TPluginNode }

constructor TPluginNode.Create(AObjectOwnerID: string; AMapped: Boolean = True);
var
  i: Integer;
  lPluginProcessor: TPluginProcessor;
begin
  Inherited Create(AObjectOwnerID);

  FOnCreateInstanceCallback := @DoCreateInstance;

  lPluginProcessor := TPluginProcessor(GObjectMapper.GetModelObject(AObjectOwnerID));
  FFrames := lPluginProcessor.Frames;

  FChilds := TObjectList.Create(False);
  FCached := False;
  FMixBuffer := GetMem(FFrames * SizeOf(Single));
  FBuffer := GetMem(FFrames * SizeOf(Single));

  FParameterList := TObjectList.Create(False);

  for i := 0 to Pred(FFrames) do
  begin
    FMixBuffer[i] := 0;
    FBuffer[i] := 0;
  end;
end;

destructor TPluginNode.Destroy;
begin
  DBLog('start TPluginNode.Destroy: ' + ClassName);

  FParameterList.Free;
  FreeMem(FMixBuffer);

  {
    TAudioInNode takes it's audio buffer from TWaveForm, TWaveFormTrack and as such should not
    free this buffer. An other alternative would be to copy the buffer.
  }
  if not ClassNameIs('TPluginAudioIn') then
  begin
    FreeMem(FBuffer);
  end;

  FChilds.Free;

  inherited Destroy;

  DBLog('end TPluginNode.Destroy: ' + ClassName);
end;

procedure TPluginNode.DoCreateInstance(var AObject: TObject; AClassName: string);
begin
  DBLog('start TPluginNode.DoCreateInstance');

  DBLog('end TPluginNode.DoCreateInstance');
end;

procedure TPluginNode.Initialize;
begin
  Notify;
end;

procedure TPluginNode.Finalize;
begin
  //
end;

procedure TPluginNode.Instantiate;
begin
  //
end;

procedure TPluginNode.Activate;
begin
  //
end;

procedure TPluginNode.Deactivate;
begin
  //
end;

procedure TPluginNode.Clean;
begin
  //
end;

function TPluginNode.Execute(AFrames: Integer): PSingle;
var
  lChildIndex: Integer;
  lIndex: Integer;
  lChildNode: TPluginNode;
begin
  // Clear mixbuffer each call
  FillByte(FMixBuffer[0], AFrames * SizeOf(Single), 0);

  // First recurse into childs
  if FChilds.Count > 0 then
  begin
    for lChildIndex := 0 to Pred(FChilds.Count) do
    begin
      lChildNode := TPluginNode(FChilds[lChildIndex]);

      if Assigned(lChildNode) then
      begin
        FReturnBuffer := lChildNode.Execute(AFrames);

        // Mix each child into buffer
        for lIndex := 0 to Pred(AFrames) do
        begin
          FMixBuffer[lIndex] += FReturnBuffer[lIndex];
        end;
      end;
    end;

    // Scale mixbuffer
    for lIndex := 0 to Pred(AFrames) do
    begin
      FMixBuffer[lIndex] /= FChilds.Count;
    end;
  end;

  // Apply plugin to mixed childs
  Process(FMidiBuffer, FMixBuffer, AFrames);

  FCached := True;

  Result := FMixBuffer;
end;

procedure TPluginNode.Clear;
var
  i: Integer;
begin
  FCached := False;

  for i := 0 to Pred(FFrames) do
  begin
    FMixBuffer[i] := 0;
  end;
end;

procedure TPluginNode.ApplyToAll(AApplyProc: TApplyProc);
var
  lChildIndex: Integer;
  lChildNode: TPluginNode;
begin
  // First recurse into childs
  for lChildIndex := 0 to Pred(FChilds.Count) do
  begin
    lChildNode := TPluginNode(FChilds[lChildIndex]);

    if Assigned(lChildNode) then
    begin
      lChildNode.ApplyToAll(AApplyProc);
    end;
  end;

  // Apply plugin to mixed childs
  if Assigned(AApplyProc) then
  begin
    AApplyProc();
  end;
end;


{ TExternalNode }

constructor TExternalNode.Create(AObjectOwnerID: string);
begin
  //
end;

procedure TExternalNode.Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer);
begin
  //
end;

{ TMementoNode }

procedure TMementoNode.Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer);
begin
  // Virtual base method
end;

{ TScriptNode }

constructor TScriptNode.Create(AObjectOwnerID: string; AMapped: Boolean);
begin
  inherited Create(AObjectOwnerID, AMapped);

end;

destructor TScriptNode.Destroy;
begin

  inherited Destroy;
end;

procedure TScriptNode.Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle;
  AFrames: Integer);
begin
  //
end;

end.

