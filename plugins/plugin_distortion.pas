unit plugin_distortion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, plugin, global_command, global, pluginhost;

type

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
    FOldDrive: Single;
    FDrive: Single;
    FPluginDistortion: TPluginDistortion;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  public
    procedure Initialize; override;
    property Drive: single read FDrive write FDrive;
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

  FOldDrive := FDrive;

  FPluginDistortion.Drive := FDrive;

  FPluginDistortion.EndUpdate;
end;

procedure TDistortionCommand.DoRollback;
begin
  FPluginDistortion.BeginUpdate;

  FDrive := FOldDrive;

  FPluginDistortion.Drive := FOldDrive;

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

