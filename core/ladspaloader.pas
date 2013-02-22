unit ladspaloader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ShellCtrls,
  StdCtrls, contnrs, ladspa, dynlibs;

const
  LADPSA_PATH = 'LADPSA_PATH';

type
  TLadspaLoadedPluginItem = class
    LadspaDescriptor: PLADSPA_Descriptor;
  end;

  TLadspaPluginCatalogItem = class
  public
    // Plugin developer
    Maker: string;
    // Unique plugin identifier
    UniqueId: Integer;
    // Plugin name
    Name: string;
    // Plugin full file path
    FileName: string;
    // Plugin index, there can be more than 1 plugin per pluginobject (.so)
    PluginIndex: Integer;
    // Number of input/output audio buffers
    AudioInCount: Integer;
    AudioOutCount: Integer;
  end;

  { TLadspaPluginCatalog }

  TLadspaPluginCatalog = class
  private
    FLoadedPlugins: TStringList;
    FPluginList: TStringList;
    procedure Discover;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadPlugin(APluginId: Integer): TLadspaLoadedPluginItem;
    property LoadedPlugins: TStringList read FLoadedPlugins write FLoadedPlugins;
    property PluginList: TStringList read FPluginList write FPluginList;
  end;

var
  GLadspaPluginFactory: TLadspaPluginCatalog;

implementation

procedure TLadspaPluginCatalog.Discover;
var
  i, j: Integer;
  lEnvironmentVars: TStringList;
  lLadspaPaths: TStringList;
  lFullPathAndFilename: string;
  lSearchRec: TSearchRec;
  lAttributes: Integer;
  lAudioOutPortCount: Integer;
  lAudioInPortCount: Integer;

  lLibHandle: TLibHandle;
  lDescriptorFunction: LADSPA_DescriptorFunction;
  lLadspaDescriptor: PLADSPA_Descriptor;
  lPortDescriptor: LADSPA_PortDescriptor;
  lIterateDescriptor: Integer;
  lNewPluginDescriptor: TLadspaPluginCatalogItem;
begin
  // Iterate found paths
  lEnvironmentVars := TStringList.Create;
  lLadspaPaths := TStringList.Create;
  try
    for i := 1 to GetEnvironmentVariableCount do
    begin
      lEnvironmentVars.Add(GetEnvironmentString(i));
    end;

    // Retrieve set paths
    if lEnvironmentVars.IndexOfName(LADPSA_PATH) <> -1 then
    begin
      lLadspaPaths.Delimiter := ';';
      lLadspaPaths.DelimitedText := lEnvironmentVars.Values[LADPSA_PATH];
    end
    else
    begin
      // Not set so use the default locations
      lLadspaPaths.Add('/usr/lib/ladspa');
      lLadspaPaths.Add('/usr/local/lib/ladspa');
      lLadspaPaths.Add('~/.ladspa');
    end;

    // Iterate plugin paths
    for i := 0 to Pred(lLadspaPaths.Count) do
    begin
      if DirectoryExists(lLadspaPaths[i]) then
      begin
        // Iterate folder and store references
        lAttributes := faAnyFile;
        if FindFirst(IncludeTrailingPathDelimiter(lLadspaPaths[i]) + '*.so',
          lAttributes, lSearchRec) = 0 then
        begin
          repeat
            lFullPathAndFilename := IncludeTrailingPathDelimiter(lLadspaPaths[i]) + lSearchRec.Name;
            lLibHandle := LoadLibrary(lFullPathAndFilename);

            lDescriptorFunction := LADSPA_DescriptorFunction(GetProcAddress(lLibHandle, 'ladspa_descriptor'));

            lIterateDescriptor := 0;
            while Assigned(lDescriptorFunction(lIterateDescriptor)) do
            begin
              lLadspaDescriptor := lDescriptorFunction(lIterateDescriptor);

              lNewPluginDescriptor := TLadspaPluginCatalogItem.Create;
              lNewPluginDescriptor.Maker := lLadspaDescriptor^.Maker;
              lNewPluginDescriptor.UniqueId := lLadspaDescriptor^.UniqueID;
              lNewPluginDescriptor.PluginIndex := lIterateDescriptor;
              lNewPluginDescriptor.Name := lLadspaDescriptor^.Label0;
              lNewPluginDescriptor.FileName := lFullPathAndFilename;

              lAudioInPortCount := 0;
              lAudioOutPortCount := 0;

              // Discover audio with inputs AND outputs.
              for j := 0 to Pred(lLadspaDescriptor^.PortCount) do
              begin
                lPortDescriptor := lLadspaDescriptor^.PortDescriptors[j];
                Inc(lAudioInPortCount, Integer(LADSPA_IS_PORT_INPUT(lPortDescriptor) and LADSPA_IS_PORT_AUDIO(lPortDescriptor)));
                Inc(lAudioOutPortCount, Integer(LADSPA_IS_PORT_OUTPUT(lPortDescriptor) and LADSPA_IS_PORT_AUDIO(lPortDescriptor)));
              end;

              if (lAudioInPortCount > 0) and (lAudioOutPortCount > 0) then
              begin
                lNewPluginDescriptor.AudioInCount := lAudioInPortCount;
                lNewPluginDescriptor.AudioOutCount := lAudioOutPortCount;
              end;

              // Only add the plugin when it at least meets a few requirements
              if (lAudioInPortCount >= 1) and (lAudioInPortCount <= 2) and
                (lAudioOutPortCount >= 1) and (lAudioOutPortCount <= 2) then
              begin
                FPluginList.AddObject(IntToStr(lLadspaDescriptor^.UniqueID), lNewPluginDescriptor);
              end;

              Inc(lIterateDescriptor);
            end;

            FreeLibrary(lLibHandle);
          until FindNext(lSearchRec) <> 0;
          FindClose(lSearchRec);
        end;
      end;
    end;
  finally
    lLadspaPaths.Free;
    lEnvironmentVars.Free;
  end;
end;

constructor TLadspaPluginCatalog.Create;
begin
  FLoadedPlugins := TStringList.Create;
  FLoadedPlugins.Sorted := True;
  FPluginList := TStringList.Create;
  FPluginList.Sorted := True;
end;

destructor TLadspaPluginCatalog.Destroy;
var
  lIndex: Integer;
begin
  for lIndex := 0 to Pred(FLoadedPlugins.Count) do
  begin
    if Assigned(FLoadedPlugins.Objects[lIndex]) then
    begin
      TLadspaLoadedPluginItem(FLoadedPlugins.Objects[lIndex]).Free;
    end;
  end;

  for lIndex := 0 to Pred(FPluginList.Count) do
  begin
    if Assigned(FPluginList.Objects[lIndex]) then
    begin
      TLadspaPluginCatalogItem(FPluginList.Objects[lIndex]).Free;
    end;
  end;

  FLoadedPlugins.Free;
  FPluginList.Free;

  inherited Destroy;
end;

function TLadspaPluginCatalog.LoadPlugin(APluginId: Integer): TLadspaLoadedPluginItem;
var
  lLibIndex: Integer;
  lPluginIndex: Integer;
  lLibHandle: TLibHandle;
  lPluginCatalogItem: TLadspaPluginCatalogItem;
  lPluginItem: TLadspaLoadedPluginItem;
  lDescriptorFunction: LADSPA_DescriptorFunction;
begin
  lLibIndex := FLoadedPlugins.IndexOf(IntToStr(APluginId));

  if lLibIndex <> -1 then
  begin
    Result := TLadspaLoadedPluginItem(FLoadedPlugins.Objects[lLibIndex]);
  end
  else
  begin
    lPluginIndex := FPluginList.IndexOf(IntToStr(APluginId));
    if lPluginIndex <> -1 then
    begin
      lPluginCatalogItem := TLadspaPluginCatalogItem(FPluginList.Objects[lPluginIndex]);

      lLibHandle := LoadLibrary(lPluginCatalogItem.FileName);

      if lLibHandle <> 0 then
      begin
        lDescriptorFunction := LADSPA_DescriptorFunction(GetProcAddress(lLibHandle, 'ladspa_descriptor'));

        Result := TLadspaLoadedPluginItem.Create;
        Result.LadspaDescriptor := lDescriptorFunction(lPluginCatalogItem.PluginIndex);
        FLoadedPlugins.AddObject(IntToStr(APluginId), Result);
      end;
    end;
  end;
end;

initialization
  GLadspaPluginFactory := TLadspaPluginCatalog.Create;
  GLadspaPluginFactory.Discover;

finalization
  GLadspaPluginFactory.Free;

end.

