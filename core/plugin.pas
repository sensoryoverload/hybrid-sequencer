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
  TPluginType = (ptIO, ptSampler, ptDistortion, ptFlanger, ptFilter, ptDecimate, ptReverb, ptBassline);

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
    FPortList: TObjectList;
    FParameterList: TObjectList;
    FReturnBuffer: PSingle;
    FMidiBuffer: TMidiBuffer;
    FBuffer: PSingle;
    FFrames: Integer;
    FNodeType: TPluginNodeType;
    FPluginName: string;
    FPluginType: TPluginType;
    FChannels: Integer;
    FSequenceNr: Single;
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
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); virtual; abstract;
    procedure Clear;
    property PortList: TObjectList read FPortList write FPortList;
    property NodeType: TPluginNodeType read FNodeType write FNodeType;
    property Buffer: psingle read FBuffer write FBuffer;
  published
    property PluginName: string read FPluginName write FPluginName;
    property PluginType: TPluginType read FPluginType write FPluginType;
    property Frames: Integer read FFrames write FFrames;
    property SequenceNr: Single read FSequenceNr write FSequenceNr;
    property Channels: Integer read FChannels write FChannels;
  end;

  { TScriptNode }

  TScriptNode = class(TPluginNode)
  public
    constructor Create(AObjectOwnerID: string; AMapped: Boolean = True);
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
  FChannels := 2;
  FBuffer := GetMem(FFrames * SizeOf(Single) * FChannels);

  FParameterList := TObjectList.Create(False);

  for i := 0 to Pred(FFrames * FChannels) do
  begin
    FBuffer[i] := 0;
  end;
end;

destructor TPluginNode.Destroy;
begin
  DBLog('start TPluginNode.Destroy: ' + ClassName);

  FParameterList.Free;
  FreeMem(FBuffer);

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

procedure TPluginNode.Clear;
var
  i: Integer;
begin
  for i := 0 to Pred(FFrames) do
  begin
    FBuffer[i] := 0;
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

