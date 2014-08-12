unit plugin_freeverb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, plugin, global_command, freereverb, global, globalconst;

type
  TReverbParameter = (rpRoomSize, rpDamp, rpWidth, rpDry, rpWet, rpMode, rpPreDelay);

  { TPluginFreeverb }

  TPluginFreeverb = class(TPluginNode)
  private
    FReverb: TReverb;
    FLatency: Integer;
    function GetDamp: Single;
    function GetDry: Single;
    function GetMode: Single;
    function GetRoomSize: Single;
    function GetWet: Single;
    function GetWidth: Single;
    function GetPreDelay: Single;
    procedure SetDamp(AValue: Single);
    procedure SetDry(AValue: Single);
    procedure SetMode(AValue: Single);
    procedure SetRoomSize(AValue: Single);
    procedure SetWet(AValue: Single);
    procedure SetWidth(AValue: Single);
    procedure SetPreDelay(AValue: Single);
  public
    constructor Create(AObjectOwnerID: string; AMapped: Boolean = True); reintroduce;
    destructor Destroy; override;
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
        AOutputBuffer: PSingle; AFrames: Integer); override;
    function GetLatency: Integer; override;
    procedure SetLatency(AValue: Integer); override;
  published
    property RoomSize: Single read GetRoomSize write SetRoomSize;
    property Damp: Single read GetDamp write SetDamp;
    property Width: Single read GetWidth write SetWidth;
    property Dry: Single read GetDry write SetDry;
    property Wet: Single read GetWet write SetWet;
    property Mode: Single read GetMode write SetMode;
    property PreDelay: Single read GetPreDelay write SetPreDelay;
  end;


  { TFreeverbCommand }

  TFreeverbCommand = class(TCommand)
  private
    FFreeverb: TPluginFreeverb;
  public
    procedure Initialize; override;
  end;

  { TFreeverbParameterCommand }

  TFreeverbParameterCommand = class(TFreeverbCommand)
  private
    FOldValue: Variant;
    FValue: Variant;
    FParameter: TReverbParameter;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  published
    property Value: Variant read FValue write FValue;
    property Parameter: TReverbParameter read FParameter write FParameter;
  end;


implementation

{ TFreeverbParameterCommand }

procedure TFreeverbParameterCommand.DoExecute;
begin
  FFreeverb.BeginUpdate;

  case FParameter of
    rpDamp:
    begin
      FOldValue := FFreeverb.Damp;
      FFreeverb.Damp := FValue;
    end;
    rpDry:
    begin
      FOldValue := FFreeverb.Dry;
      FFreeverb.Dry := FValue;
    end;
    rpMode:
    begin
      FOldValue := FFreeverb.Mode;
      FFreeverb.Mode := FValue;
    end;
    rpRoomSize:
    begin
      FOldValue := FFreeverb.RoomSize;
      FFreeverb.RoomSize := FValue;
    end;
    rpWet:
    begin
      FOldValue := FFreeverb.Wet;
      FFreeverb.Wet := FValue;
    end;
    rpWidth:
    begin
      FOldValue := FFreeverb.Width;
      FFreeverb.Width := FValue;
    end;
    rpPreDelay:
    begin
      FOldValue := FFreeverb.PreDelay;
      FFreeverb.PreDelay := FValue;
    end;
  end;

  FFreeverb.EndUpdate;
end;

procedure TFreeverbParameterCommand.DoRollback;
begin
  FFreeverb.BeginUpdate;

  case FParameter of
    rpDamp:
    begin
      FFreeverb.Damp := FOldValue;
    end;
    rpDry:
    begin
      FFreeverb.Dry := FOldValue;
    end;
    rpMode:
    begin
      FFreeverb.Mode := FOldValue;
    end;
    rpRoomSize:
    begin
      FFreeverb.RoomSize := FOldValue;
    end;
    rpWet:
    begin
      FFreeverb.Wet := FOldValue;
    end;
    rpWidth:
    begin
      FFreeverb.Width := FOldValue;
    end;
    rpPreDelay:
    begin
      FFreeverb.PreDelay := FOldValue;
    end;
  end;

  FFreeverb.EndUpdate;
end;

{ TFreeverbCommand }

procedure TFreeverbCommand.Initialize;
begin
  FFreeverb := TPluginFreeverb(GObjectMapper.GetModelObject(ObjectID));
end;

{ TPluginFreeverb }

procedure TPluginFreeverb.Process(AMidiBuffer: TMidiBuffer;
  AInputBuffer: PSingle; AOutputBuffer: PSingle; AFrames: Integer);
begin
  FReverb.process(AInputBuffer, AOutputBuffer, AFrames);
end;

function TPluginFreeverb.GetLatency: Integer;
begin
  Result := FLatency;
end;

procedure TPluginFreeverb.SetLatency(AValue: Integer);
begin
  FLatency := AValue;
end;

function TPluginFreeverb.GetDamp: Single;
begin
  Result := FReverb.getdamp;
end;

function TPluginFreeverb.GetDry: Single;
begin
  Result := FReverb.getdry;
end;

function TPluginFreeverb.GetMode: Single;
begin
  Result := FReverb.getmode;
end;

function TPluginFreeverb.GetRoomSize: Single;
begin
  Result := FReverb.getroomsize;
end;

function TPluginFreeverb.GetWet: Single;
begin
  Result := FReverb.getwet;
end;

function TPluginFreeverb.GetWidth: Single;
begin
  Result := FReverb.getwidth;
end;

function TPluginFreeverb.GetPreDelay: Single;
begin
  Result := FReverb.getpredelay;
end;

procedure TPluginFreeverb.SetDamp(AValue: Single);
begin
  FReverb.setdamp(AValue);
end;

procedure TPluginFreeverb.SetDry(AValue: Single);
begin
  FReverb.setdry(AValue);
end;

procedure TPluginFreeverb.SetMode(AValue: Single);
begin
  FReverb.setmode(AValue);
end;

procedure TPluginFreeverb.SetRoomSize(AValue: Single);
begin
  FReverb.setroomsize(AValue);
end;

procedure TPluginFreeverb.SetWet(AValue: Single);
begin
  FReverb.setwet(AValue);
end;

procedure TPluginFreeverb.SetWidth(AValue: Single);
begin
  FReverb.setwidth(AValue);
end;

procedure TPluginFreeverb.SetPreDelay(AValue: Single);
begin
  FReverb.setpredelay(AValue);
end;

constructor TPluginFreeverb.Create(AObjectOwnerID: string; AMapped: Boolean = True);
begin
  inherited Create(AObjectOwnerID, AMapped);

  FReverb := TReverb.Create;
end;

destructor TPluginFreeverb.Destroy;
begin
  FReverb.Free;

  inherited Destroy;
end;


end.

