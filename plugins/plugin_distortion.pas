unit plugin_distortion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, plugin, global_command, global, pluginhost;

type
  TDistortionParameter = (dpDrive);

  { TPluginDistortion }

  TPluginDistortion = class(TPluginNode)
  private
    FDrive: single;
  public
    constructor Create(AObjectOwnerID: string; AMapped: Boolean = True);
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); override;
  published
    property Drive: single read FDrive write FDrive;
  end;

  { TDistortionCommand }

  TDistortionCommand = class(TCommand)
  private
    FOldValue: Variant;
    FValue: Variant;
    FPluginDistortion: TPluginDistortion;
    FParameter: TDistortionParameter;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  public
    procedure Initialize; override;
    property Value: Variant read FValue write FValue;
    property Parameter: TDistortionParameter read FParameter write FParameter;
  end;

implementation

uses
  fx;

{ TBasePluginCommand }

procedure TDistortionCommand.Initialize;
begin
   FPluginDistortion := TPluginDistortion(GObjectMapper.GetModelObject(ObjectOwner));
end;

{ TDistortionCommand }

procedure TDistortionCommand.DoExecute;
begin
  FPluginDistortion.BeginUpdate;

  case FParameter of
    dpDrive:
    begin;
      FOldValue := FPluginDistortion.Drive;
      FPluginDistortion.Drive := FValue;
    end;
  end;

  FPluginDistortion.EndUpdate;
end;

procedure TDistortionCommand.DoRollback;
begin
  FPluginDistortion.BeginUpdate;

  case FParameter of
    dpDrive:
    begin;
      FPluginDistortion.Drive := FOldValue;
    end;
  end;

  FPluginDistortion.EndUpdate;
end;


{ TPluginDistortion }

constructor TPluginDistortion.Create(AObjectOwnerID: string; AMapped: Boolean);
begin
  Inherited Create(AObjectOwnerID, AMapped);

  FDrive := 1;
end;

procedure TPluginDistortion.Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle;
  AFrames: Integer);
var
  lIndex: Integer;
begin
  for lIndex := 0 to Pred(AFrames * Channels) do
  begin
    ABuffer[lIndex] := tanh2(ABuffer[lIndex] * FDrive);
  end;
end;


end.

