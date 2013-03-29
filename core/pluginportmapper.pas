unit pluginportmapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, globalconst, sampler;

type
  {
    Publishes and maps the plugin's ports as models to who you can attach a view
    per parameter.

    Ports can hardcoded like the sampler native to this project and dynamic like
    LADSPA plugin where ports are discoverd and enumerated.

    It is possible to write a PortMapper per LADSPA plugin, this would enable you
    to code a custom gui view with for example metering.
  }

  { TBasePluginPortMapper }

  TBasePluginPortMapper = class(THybridPersistentModel)
  private
    function GetParameter(Index: Integer): Integer;
    procedure SetParameter(Index: Integer; AValue: Integer);
  public
    property Parameter[Index: Integer]: Integer read GetParameter write SetParameter;
  published
    function GetVersion: string; virtual;
    function GetProductName: string; virtual;
    procedure GetParameterList(AParameterList: TStringList); virtual;
  end;

  TSamplerPlugin = class(TBasePluginPortMapper)
  public
  end;

implementation

{ TBasePluginPortMapper }

function TBasePluginPortMapper.GetParameter(Index: Integer): Integer;
begin

end;

procedure TBasePluginPortMapper.SetParameter(Index: Integer; AValue: Integer);
begin

end;

function TBasePluginPortMapper.GetVersion: string;
begin
  Result := '1';
end;

function TBasePluginPortMapper.GetProductName: string;
begin
  Result := 'Base';
end;

procedure TBasePluginPortMapper.GetParameterList(AParameterList: TStringList);
begin
  // Implement in descendant
end;

end.

