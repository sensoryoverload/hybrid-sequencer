unit plugin_freeverb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, plugin, global_command, freereverb;

type
  { TFreeverb }

  TFreeverb = class(TPluginNode)
  private
    FReverb: TReverb;
  public
    constructor Create(AObjectOwnerID: string);
    destructor Destroy; override;
    procedure Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer); override;
  published
  end;

implementation

{ TFreeverb }

procedure TFreeverb.Process(AMidiBuffer: TMidiBuffer; ABuffer: PSingle; AFrames: Integer);
begin
  FReverb.process(ABuffer, ABuffer, AFrames);
end;

constructor TFreeverb.Create(AObjectOwnerID: string);
begin
  inherited Create(AObjectOwnerID);

  FReverb := TReverb.Create;
end;

destructor TFreeverb.Destroy;
begin
  FReverb.Free;

  inherited Destroy;
end;


end.

