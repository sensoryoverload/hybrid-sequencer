unit ladspaloader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ShellCtrls,
  StdCtrls, ladspa, dynlibs;

type
  TLadspaPluginLib = class;

  TInstantiate = function(Descriptor: PLADSPA_Descriptor; SampleRate: LongWord) : LADSPA_Handle; cdecl;
  TConnect_port = procedure(Instance: LADSPA_Handle; Port: LongWord; DataLocation: PLADSPA_Data); cdecl;
  TActivate = procedure(Instance: LADSPA_Handle); cdecl;
  TRun = procedure(Instance: LADSPA_Handle; SampleCount: LongWord); cdecl;
  TRun_adding = procedure(Instance: LADSPA_Handle; SampleCount: LongWord); cdecl;
  TSet_run_adding_gain = procedure(Instance: LADSPA_Handle; Gain: LADSPA_Data); cdecl;
  TDeactivate = procedure(Instance: LADSPA_Handle); cdecl;
  TCleanup = procedure(Instance: LADSPA_Handle); cdecl;
  TDescriptorFunction = function(index : LongWord): PLADSPA_Descriptor; cdecl;

  { TLadspaDescriptor }

  TLadspaDescriptor = class
  private
    FLadspaDescriptor: PLADSPA_Descriptor;
  public
    instantiate: TInstantiate;
    connect_port: TConnect_port;
    aactivate: TActivate;
    run: TRun;
    run_adding: TRun_adding;
    set_run_adding_gain: TSet_run_adding_gain;
    ddeactivate: TDeactivate;
    cleanup: TCleanup;
    DescriptorFunction: LADSPA_DescriptorFunction;

    constructor Create(ALadspaPluginLib: TLadspaPluginLib);

    property LadspaDescriptor: PLADSPA_Descriptor read FLadspaDescriptor write FLadspaDescriptor;
  end;

  { TLadspaPluginLib }

  TLadspaPluginLib = class
  public
    FileName: string;
    FLadspaLibHandle: TLibHandle;
    FLadspaDescriptor: TLadspaDescriptor;
    constructor Create(ALadspaLibHandle: TLibHandle);

    property LadspaLibHandle: TLibHandle read FLadspaLibHandle write FLadspaLibHandle;
    property LadspaDescriptor: TLadspaDescriptor read FLadspaDescriptor write FLadspaDescriptor;
  end;

  TBasePlugin = class
    //procedure Run(AFrames: Integer); virtual; abstract;
  end;

  { TLadspaPlugin }

  TLadspaPlugin = class(TBasePlugin)
  private
  public
    FileName: string;
    PluginDescriptor: TLadspaDescriptor;
    PluginInstance: LADSPA_Handle;
    constructor Create(APluginFileName: string);
    destructor Destroy; override;
    procedure Activate;
    procedure Deactivate;
    procedure Process(AFrames: Integer);
  end;

  { TLadspaPluginFactory }

  TLadspaPluginFactory = class
  private
    FPluginList: TStringList;
  public
    instantiate: TInstantiate;
    cleanup: TCleanup;

    constructor Create;
    destructor Destroy; override;

    function LoadPlugin(AFileName: string): TLadspaDescriptor;
    function UnloadPlugin(AFileName: string): Boolean;
  end;

(*
  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;
    ShellListView1: TShellListView;
    ShellTreeView1: TShellTreeView;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShellListView1DblClick(Sender: TObject);
  private
    FAmpPlugin: TLadspaPlugin;

    procedure FreeDLL;
    procedure InitDLL(AFile: string);
    { private declarations }
  public
    { public declarations }
    property Memo: TMemo read Memo1;
  end;
*)

var
  GLadspaPluginFactory: TLadspaPluginFactory;

implementation

{ TLadspaPluginLib }

constructor TLadspaPluginLib.Create(ALadspaLibHandle: TLibHandle);
begin
  FLadspaLibHandle := ALadspaLibHandle;

  FLadspaDescriptor := TLadspaDescriptor.Create(Self)
end;


{ TLadspaDescriptor }

constructor TLadspaDescriptor.Create(ALadspaPluginLib: TLadspaPluginLib);
var
  lIterateDescriptors: Integer;
  lLADSPA_Handle: LADSPA_Handle;
begin
  DescriptorFunction := LADSPA_DescriptorFunction(GetProcAddress(ALadspaPluginLib.LadspaLibHandle, 'ladspa_descriptor'));

  lIterateDescriptors := 0;
  while Assigned(DescriptorFunction(lIterateDescriptors)) do
  begin
    FLadspaDescriptor := DescriptorFunction(lIterateDescriptors);

(*    if Assigned(FLadspaDescriptor) then
    begin
      lLADSPA_Handle := FLadspaDescriptor^.instantiate(FLadspaDescriptor, 44100);
      if Assigned(lLADSPA_Handle) then
      begin
        // Is function implemented, not always so
        if Assigned( FLadspaDescriptor^.activate ) then
        begin
          FLadspaDescriptor^.activate(lLADSPA_Handle);
        end;

        try
          // This function should be there but make sure that buffers and ports are allocated
          if Assigned( FLadspaDescriptor^.run ) then
          begin
            FLadspaDescriptor^.run(lLADSPA_Handle, 100);
          end;
        finally
        end;

        // Is function implemented, not always so
        if Assigned( FLadspaDescriptor^.deactivate ) then
        begin
          FLadspaDescriptor^.deactivate(lLADSPA_Handle);
        end;

        FLadspaDescriptor^.cleanup(lLADSPA_Handle);
      end;
    end;
    *)


    Inc(lIterateDescriptors);
  end;
end;

constructor TLadspaPlugin.Create(APluginFileName: string);
begin
  PluginDescriptor := GLadspaPluginFactory.LoadPlugin(APluginFileName);

  if Assigned(PluginDescriptor) then
  begin
    PluginInstance := PluginDescriptor.LadspaDescriptor^.instantiate(PluginDescriptor.LadspaDescriptor, 44100);
  end;
end;

destructor TLadspaPlugin.Destroy;
begin
  if Assigned(PluginInstance) then
  begin
    PluginDescriptor.LadspaDescriptor^.cleanup(PluginInstance);
  end;

  inherited Destroy;
end;

procedure TLadspaPlugin.Activate;
begin
  if Assigned(PluginDescriptor.LadspaDescriptor^.activate) then
  begin
    PluginDescriptor.LadspaDescriptor^.activate(PluginInstance);
  end;
end;

procedure TLadspaPlugin.Deactivate;
begin
  if Assigned(PluginDescriptor.LadspaDescriptor^.deactivate) then
  begin
    PluginDescriptor.LadspaDescriptor^.deactivate(PluginInstance);
  end;
end;

procedure TLadspaPlugin.Process(AFrames: Integer);
begin
  PluginDescriptor.LadspaDescriptor^.run(PluginInstance, 44100);
end;

{ TLadspaPluginFactory }

constructor TLadspaPluginFactory.Create;
begin
  FPluginList := TStringList.Create;
end;

destructor TLadspaPluginFactory.Destroy;
begin
  FPluginList.Free;

  inherited Destroy;
end;

function TLadspaPluginFactory.LoadPlugin(AFileName: string): TLadspaDescriptor;
var
  lLibIndex: Integer;
  lLadspaLibHandle: TLibHandle;
  lLadspaPluginLib: TLadspaPluginLib;
begin
  lLibIndex := FPluginList.IndexOf(AFileName);

  if lLibIndex <> -1 then
  begin
    Result := TLadspaPluginLib(FPluginList[lLibIndex]).LadspaDescriptor;
  end
  else
  begin
    lLadspaLibHandle := LoadLibrary(AFileName);

    if lLadspaLibHandle <> 0 then
    begin
      lLadspaPluginLib := TLadspaPluginLib.Create(lLadspaLibHandle);
      try
        lLadspaPluginLib.FileName := AFileName;

        FPluginList.AddObject(AFileName, lLadspaPluginLib);

        Result := lLadspaPluginLib.LadspaDescriptor;
      finally
      end;
    end;
  end;
end;

function TLadspaPluginFactory.UnloadPlugin(AFileName: string): Boolean;
var
  lLibIndex: Integer;
  lLadspaPluginLib: TLadspaPluginLib;
begin
  lLibIndex := FPluginList.IndexOf(AFileName);

  if lLibIndex <> -1 then
  begin
    lLadspaPluginLib := TLadspaPluginLib(FPluginList[lLibIndex]);

    FreeLibrary(lLadspaPluginLib.LadspaLibHandle);
    lLadspaPluginLib.LadspaLibHandle := 0;

    lLadspaPluginLib.Free;
  end;
end;

(*
{ TForm1 }

procedure TForm1.ShellListView1DblClick(Sender: TObject);
var
  lFileName: string;
  lLadspa_Descriptor: PLADSPA_Descriptor;
  i: Integer;

  function HasProperty(AProperty, AFlag: longword): string;
  begin
    if (AProperty and AFlag) = AFlag then
    begin
      Result := 'True';
    end
    else
    begin
      Result := 'False';
    end;
  end;

begin
  if lFileName <> (ShellTreeView1.GetSelectedNodePath + ShellListView1.Selected.Caption) then
  begin
    lFileName := ShellTreeView1.GetSelectedNodePath + ShellListView1.Selected.Caption;
    if Assigned(FAmpPlugin) then
    begin
      FAmpPlugin.Free;
    end;
    FAmpPlugin := TLadspaPlugin.Create(lFileName);

    lLadspa_Descriptor := FAmpPlugin.PluginDescriptor.FLadspaDescriptor;

    Memo1.Lines.Clear;
    Memo1.Lines.Add(Format('Properties: %d', [lLadspa_Descriptor^.Properties]));
    Memo1.Lines.Add(Format('LADSPA_PROPERTY_REALTIME: %s', [HasProperty(lLadspa_Descriptor^.Properties, LADSPA_PROPERTY_REALTIME)]));
    Memo1.Lines.Add(Format('LADSPA_PROPERTY_INPLACE_BROKEN: %s', [HasProperty(lLadspa_Descriptor^.Properties, LADSPA_PROPERTY_REALTIME)]));
    Memo1.Lines.Add(Format('LADSPA_PROPERTY_HARD_RT_CAPABLE: %s', [HasProperty(lLadspa_Descriptor^.Properties, LADSPA_PROPERTY_HARD_RT_CAPABLE)]));

    Memo1.Lines.Add(Format('UniqueID: %d', [lLadspa_Descriptor^.UniqueID]));
    Memo1.Lines.Add(Format('Label0: %s', [lLadspa_Descriptor^.Label0]));
    Memo1.Lines.Add(Format('Copyright: %s', [lLadspa_Descriptor^.Copyright]));
    Memo1.Lines.Add(Format('Maker: %s', [lLadspa_Descriptor^.Maker]));
    Memo1.Lines.Add(Format('Name: %s', [lLadspa_Descriptor^.Name]));
    Memo1.Lines.Add(Format('PortCount: %d', [lLadspa_Descriptor^.PortCount]));
    Memo1.Lines.Add('---------------------------------------------------');
    for i := 0 to Pred(lLadspa_Descriptor^.PortCount) do
    begin
      Memo1.Lines.Add(Format('PortNames: %s', [lLadspa_Descriptor^.PortNames[i]]));
      Memo1.Lines.Add(Format('LADSPA_PORT_INPUT: %s', [HasProperty(lLadspa_Descriptor^.PortDescriptors[i], LADSPA_PORT_INPUT)]));
      Memo1.Lines.Add(Format('LADSPA_PORT_OUTPUT: %s', [HasProperty(lLadspa_Descriptor^.PortDescriptors[i], LADSPA_PORT_OUTPUT)]));
      Memo1.Lines.Add(Format('LADSPA_PORT_CONTROL: %s', [HasProperty(lLadspa_Descriptor^.PortDescriptors[i], LADSPA_PORT_CONTROL)]));
      Memo1.Lines.Add(Format('LADSPA_PORT_AUDIO: %s', [HasProperty(lLadspa_Descriptor^.PortDescriptors[i], LADSPA_PORT_AUDIO)]));

      Memo1.Lines.Add(Format('LADSPA_HINT_BOUNDED_BELOW: %s', [HasProperty(lLadspa_Descriptor^.PortRangeHints[i].HintDescriptor, LADSPA_HINT_BOUNDED_BELOW)]));
      Memo1.Lines.Add(Format('LADSPA_HINT_BOUNDED_ABOVE: %s', [HasProperty(lLadspa_Descriptor^.PortRangeHints[i].HintDescriptor, LADSPA_HINT_BOUNDED_ABOVE)]));
      Memo1.Lines.Add(Format('LADSPA_HINT_TOGGLED: %s', [HasProperty(lLadspa_Descriptor^.PortRangeHints[i].HintDescriptor, LADSPA_HINT_TOGGLED)]));
      Memo1.Lines.Add(Format('LADSPA_HINT_SAMPLE_RATE: %s', [HasProperty(lLadspa_Descriptor^.PortRangeHints[i].HintDescriptor, LADSPA_HINT_SAMPLE_RATE)]));
      Memo1.Lines.Add(Format('LADSPA_HINT_LOGARITHMIC: %s', [HasProperty(lLadspa_Descriptor^.PortRangeHints[i].HintDescriptor, LADSPA_HINT_LOGARITHMIC)]));
      Memo1.Lines.Add(Format('LADSPA_HINT_INTEGER: %s', [HasProperty(lLadspa_Descriptor^.PortRangeHints[i].HintDescriptor, LADSPA_HINT_INTEGER)]));
      Memo1.Lines.Add(Format('LADSPA_HINT_DEFAULT_MASK: %s', [HasProperty(lLadspa_Descriptor^.PortRangeHints[i].HintDescriptor, LADSPA_HINT_DEFAULT_MASK)]));
      Memo1.Lines.Add(Format('LADSPA_HINT_DEFAULT_NONE: %s', [HasProperty(lLadspa_Descriptor^.PortRangeHints[i].HintDescriptor, LADSPA_HINT_DEFAULT_NONE)]));
      Memo1.Lines.Add(Format('LADSPA_HINT_DEFAULT_MINIMUM: %s', [HasProperty(lLadspa_Descriptor^.PortRangeHints[i].HintDescriptor, LADSPA_HINT_DEFAULT_MINIMUM)]));
      Memo1.Lines.Add(Format('LADSPA_HINT_DEFAULT_LOW: %s', [HasProperty(lLadspa_Descriptor^.PortRangeHints[i].HintDescriptor, LADSPA_HINT_DEFAULT_LOW)]));
      Memo1.Lines.Add(Format('LADSPA_HINT_DEFAULT_MIDDLE: %s', [HasProperty(lLadspa_Descriptor^.PortRangeHints[i].HintDescriptor, LADSPA_HINT_DEFAULT_MIDDLE)]));
      Memo1.Lines.Add(Format('LADSPA_HINT_DEFAULT_HIGH: %s', [HasProperty(lLadspa_Descriptor^.PortRangeHints[i].HintDescriptor, LADSPA_HINT_DEFAULT_HIGH)]));
      Memo1.Lines.Add(Format('LADSPA_HINT_DEFAULT_MAXIMUM: %s', [HasProperty(lLadspa_Descriptor^.PortRangeHints[i].HintDescriptor, LADSPA_HINT_DEFAULT_MAXIMUM)]));
      Memo1.Lines.Add(Format('LADSPA_HINT_DEFAULT_0: %s', [HasProperty(lLadspa_Descriptor^.PortRangeHints[i].HintDescriptor, LADSPA_HINT_DEFAULT_0)]));
      Memo1.Lines.Add(Format('LADSPA_HINT_DEFAULT_1: %s', [HasProperty(lLadspa_Descriptor^.PortRangeHints[i].HintDescriptor, LADSPA_HINT_DEFAULT_1)]));
      Memo1.Lines.Add(Format('LADSPA_HINT_DEFAULT_100: %s', [HasProperty(lLadspa_Descriptor^.PortRangeHints[i].HintDescriptor, LADSPA_HINT_DEFAULT_100)]));
    end;


  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  lFileName: string;
begin
  ShellTreeView1.Root := '/usr/lib/ladspa/';
  ShellListView1.Root := ShellTreeView1.Root;

  lFileName := '/usr/lib/ladspa/analogue_osc_1416.so';
  FAmpPlugin := TLadspaPlugin.Create(lFileName);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeDLL;
end;

procedure TForm1.InitDLL(AFile: string);
begin

end;

procedure TForm1.FreeDLL;
begin
//  if LadspaPluginHandle <> 0 then FreeLibrary(LadspaPluginHandle);
end;
*)

initialization
  GLadspaPluginFactory := TLadspaPluginFactory.Create;
finalization
  GLadspaPluginFactory.Free;

end.

