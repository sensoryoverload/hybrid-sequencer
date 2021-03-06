{
  Copyright (C) 2007 Robbert Latumahina

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

  global.pas
}
unit global;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ContNrs, globalconst, jacktypes, sndfile,
  XMLConf, Menus, jack, transport;

type
  // TMidiData is the real data to be send to plugins, outputs, etc
  // It is generated at loadtime from objects defined in XML
  // These objects contain more information about the mididata and should be the
  // source of the abstracted mididata.
  TMidiData = class
  private
    FParentObject: THybridPersistentModel;
  public
    Location: Integer; // Location in samples
    DataType: Integer;   // Note, CC, NRPN
    DataValue1: byte;
    DataValue2: byte;
    MidiChannel: byte;
    RelativeOffset: Integer;
    Length: Integer; // Notelength

    Next: TMidiData; // Point to next in list
    constructor Create(AParentObject: THybridPersistentModel);
    property ParentObject: THybridPersistentModel read FParentObject write FParentObject;
  end;

  { TAutomationData }

  TAutomationData = class(THybridPersistentModel)
  private
    FDataValue: single;
    FLocation: Integer;
  public
    Next: TAutomationData; // Point to next in list
    RelativeOffset: Integer;
    procedure Initialize; override;
  published
    property Location: Integer read FLocation write FLocation; // Location in samples
    property DataValue: single read FDataValue write FDataValue;
  end;

  TMidiEvent = class
    Location: Integer; // Location in samples
    DataType: Integer;   // Note, CC, NRPN
    DataValue1: byte;
    DataValue2: byte;
    MidiChannel: byte;
    RelativeOffset: Integer;
    Length: Integer; // Notelength
  end;

  { TObjectMapper }

  TObjectMapper = class(THybridPersistent)
  private
    FMaps: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMapping(AObject: TObject);
    procedure DeleteMapping(AGUID: string);
    procedure SetModelObjectID(AObject: THybridPersistentModel; AObjectID: string);
    function GetModelObject(AGUID: string): THybridPersistentModel;

    property Maps: TStringList read FMaps write FMaps;
  end;

  { TSettings }

  TSettings = Class(THybridPersistent)
  private
    FCursorPosition: Integer;
    FSelectedTrack: TObject;
    FOldSelectedTrackGUI: TObject;
    FSelectedPatternGUI: TObject;
    FOldSelectedPatternGUI: TObject;
    FEditMode: Byte;
    FModifier: TShiftState;
    FFrames: Integer;
    FIDCounter: Integer;
    FEscapeAction: Boolean;
    FSampleRate: single;
    FHalfSampleRate: single;
    FSampleMap: string;
    FMapToVisible: Boolean;
    procedure SetFrames(const AValue: Integer);
    procedure SetModifier(const AValue: TShiftState);
    procedure SetSampleRate(AValue: single);
    procedure SetSelectedPatternGUI(const AValue: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function Load(AFileLocation: string): Boolean;
    function Save(AFileLocation: string): Boolean;
    function NextID: Integer;
    procedure Update(Subject: THybridPersistentModel);
    property CursorPosition: Integer read FCursorPosition write FCursorPosition;
    property SelectedTrack: TObject read FSelectedTrack write FSelectedTrack;
    property OldSelectedTrackGUI: TObject read FOldSelectedTrackGUI write FOldSelectedTrackGUI;
    property SelectedObject: TObject read FSelectedPatternGUI write SetSelectedPatternGUI;
    property OldSelectedObject: TObject read FOldSelectedPatternGUI write FOldSelectedPatternGUI;
    property EditMode: Byte read FEditMode write FEditMode;
    property Modifier: TShiftState read FModifier write SetModifier;
    property Frames: Integer read FFrames write SetFrames;
    property EscapeAction: Boolean read FEscapeAction write FEscapeAction default false;
    property SampleRate: single read FSampleRate write SetSampleRate;
    property HalfSampleRate: single read FHalfSampleRate write FHalfSampleRate;
    property SampleMap: string read FSampleMap write FSampleMap;
    property MapToVisible: Boolean read FMapToVisible write FMapToVisible;
  end;

  { TChannel }

  TChannel = Class(TObject)
  private
    FBufferSize: Integer;
  public
    Buffer: PSingle;

    destructor Destroy; override;
    property BufferSize: Integer read FBufferSize write FBufferSize;
  end;

  { TChannelList }

  TChannelList = class (TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TChannel);
    function GetObject (Index: Integer): TChannel;
  public
    function Add (Obj: TChannel): Integer;
    procedure Insert (Index: Integer; Obj: TChannel);
    property Items [Index: Integer]: TChannel read GetObject write SetObject; default;
  end;

  { TWaveFile }

  TBufferFormat = (bfInterleave, bfSplit);

  TWaveFile = Class(THybridPersistent)
  private
    FChannelList: TObjectList;
    FFileName: string;
    FSampleRate: Integer;
    FBufferFormat: TBufferFormat;
    FFrames: Integer;
    FReadCount: Integer;
    FDataSize: Integer;
    FChannelCount: Integer;
    FData: PSingle;
  public
    constructor Create(AObjectOwner: string; AMapped: Boolean = True);
    destructor Destroy; override;
    function LoadSample(AFileName: string): Boolean;
    procedure UnloadSample;
    property ChannelList: TObjectList read FChannelList write FChannelList;
    property SampleRate: Integer read FSampleRate write FSampleRate;
    property FileName: string read FFileName write FFileName;
    property BufferFormat: TBufferFormat read FBufferFormat write FBufferFormat;
    property Frames: Integer read FFrames write FFrames;
    property ReadCount: Integer read FReadCount write FReadCount;
    property DataSize: Integer read FDataSize write FDataSize;
    property ChannelCount: Integer read FChannelCount write FChannelCount;
  end;

  TEditMode = (emPatternEdit, emAutomationEdit, emControllerEdit, emVelocityEdit);

  { TJackAudio }

  TJackAudio = class
  private
    FProcess: JackProcessCallback;
    FPordId: Integer;
    function GetFrames: Integer;
    function GetSamplerate: Integer;
  public
    midi_input_port : ^jack_port_t;
    midi_output_port : ^jack_port_t;
    audio_input_port_left : ^jack_port_t;
    audio_input_port_right : ^jack_port_t;
    audio_output_port_left : ^jack_port_t;
    audio_output_port_right : ^jack_port_t;
    client: ^jack_client_t;

    constructor Create;
    destructor Destroy; override;
    procedure Initialize;
    function GeneratePordId: Integer;

    property Process: JackProcessCallback read FProcess write FProcess;
    property Samplerate: Integer read GetSamplerate;
    property Frames: Integer read GetFrames;
  end;



var
  GObjectMapper: TObjectMapper;
  GSettings: TSettings;
  GJackAudio: TJackAudio;

implementation

uses
  utils, xmlread, xmlwrite, dom, BaseUnix, md5, bpmdetect, determinetransients;

{ TJackAudio }


function srate(nframes : jack_nframes_t ; arg : pointer): longint; cdecl;
begin
	//
end;

procedure jack_shutdown(arg: pointer); cdecl;
begin
  exit;
end;

function TJackAudio.GetFrames: Integer;
begin
  Result := jack_get_buffer_size(client);
end;

function TJackAudio.GetSamplerate: Integer;
begin
  Result := jack_get_sample_rate(client);
end;

constructor TJackAudio.Create;
begin
  client := jack_client_open('HybridSequencer', JackNullOption, nil);
  if not assigned(client) then
  begin
    writeln('Error creating jack client!');
    Halt(1);
  end;

  FPordId := 0;
end;

destructor TJackAudio.Destroy;
begin
  sleep(100);
  jack_transport_stop(client);

  sleep(100);
  jack_deactivate(client);

  sleep(100);
  jack_client_close(client);

  sleep(100);

  inherited Destroy;
end;

procedure TJackAudio.Initialize;
var
  input_ports: ppchar;
  output_ports: ppchar;
  lIndex: Integer;
begin
  midi_input_port := jack_port_register (client, 'midi_in', JACK_DEFAULT_MIDI_TYPE, Longword(JackPortIsInput), 0);
  midi_output_port := jack_port_register (client, 'midi_out', JACK_DEFAULT_MIDI_TYPE, Longword(JackPortIsOutput), 0);
  audio_input_port_left := jack_port_register (client, 'audio_in_left', JACK_DEFAULT_AUDIO_TYPE, Longword(JackPortIsInput), 0);
  audio_input_port_right := jack_port_register (client, 'audio_in_right', JACK_DEFAULT_AUDIO_TYPE, Longword(JackPortIsInput), 0);
  audio_output_port_left := jack_port_register (client, 'audio_out_left', JACK_DEFAULT_AUDIO_TYPE, Longword(JackPortIsOutput), 0);
  audio_output_port_right := jack_port_register (client, 'audio_out_right', JACK_DEFAULT_AUDIO_TYPE, Longword(JackPortIsOutput), 0);

  jack_on_shutdown(client, @jack_shutdown, nil);
  jack_set_sample_rate_callback(client, @srate, nil);
  writeln(format('jack_set_process_callback %d', [jack_set_process_callback(client, FProcess, nil)]));

  if jack_activate(client) = 1 then
  begin
	  writeln('cannot activate client');
    halt(1);
  end;

  input_ports := jack_get_ports(client, nil, nil, (Longword(JackPortIsPhysical) or Longword(JackPortIsOutput)));
  if not Assigned(input_ports) then
  begin
    writeln('no physical capture ports.');
  end
  else
  begin
    lIndex := 0;
    while Assigned(input_ports[lIndex]) do
    begin
      writeln(Format('Input port: "%s"', [input_ports[lIndex]]));

      Inc(lIndex);
    end;

    {if jack_connect(client, input_ports[0], jack_port_name(audio_input_port)) <> 0 then
    begin
      writeln('cannot connect input ports');
    end;}
  end;

  output_ports := jack_get_ports(client, nil, nil, (Longword(JackPortIsPhysical) or Longword(JackPortIsInput)));
  if not Assigned(output_ports) then
  begin
    writeln('no physical playback ports.');
  end
  else
  begin
    lIndex := 0;
    while Assigned(output_ports[lIndex]) do
    begin
      writeln(Format('Output port: "%s"', [output_ports[lIndex]]));

      Inc(lIndex);
    end;

    if jack_connect(client, jack_port_name(audio_output_port_left), output_ports[0]) <> 0 then
    begin
      writeln('cannot connect output port left');
    end;
    if jack_connect(client, jack_port_name(audio_output_port_right), output_ports[1]) <> 0 then
    begin
      writeln('cannot connect output port right');
    end;
  end;

  jack_transport_start(client);
end;

function TJackAudio.GeneratePordId: Integer;
begin
  Inc(FPordId);

  Result := FPordId;
end;

{ TAutomationData }

procedure TAutomationData.Initialize;
begin
  BeginUpdate;

  Inherited Initialize;

  EndUpdate;
end;

{ TMidiData }

constructor TMidiData.Create(AParentObject: THybridPersistentModel);
begin
  DBLog('TMidiData.Create');

  FParentObject := AParentObject;
end;

{ TObjectMapper }

constructor TObjectMapper.Create;
begin
  FMaps := TStringList.Create;
  FMaps.Sorted := True;
end;

destructor TObjectMapper.Destroy;
begin
  Maps.Free;

  inherited Destroy;
end;

procedure TObjectMapper.AddMapping(AObject: TObject);
begin
  FMaps.AddObject(THybridPersistentModel(AObject).ObjectID, AObject);
end;

procedure TObjectMapper.SetModelObjectID(AObject: THybridPersistentModel; AObjectID: string);
var
  lIndex: Integer;
begin
  lIndex := FMaps.IndexOfObject(AObject);
  if lIndex <> -1 then
  begin
    THybridPersistentModel(FMaps.Objects[lIndex]).ObjectID := AObjectID;
  end;
end;

procedure TObjectMapper.DeleteMapping(AGUID: string);
var
  lIndex: Integer;
begin
  lIndex := FMaps.IndexOf(AGUID);
  if lIndex <> -1 then
  begin
    FMaps.Delete(lIndex);
  end;
end;

function TObjectMapper.GetModelObject(AGUID: string): THybridPersistentModel;
var
  lIndex: Integer;
begin
  Result := nil;

  if AGUID <> '' then
  begin
    lIndex := FMaps.IndexOf(AGUID);
    if lIndex <> -1 then
    begin
      Result := THybridPersistentModel(FMaps.Objects[lIndex]);
    end;
  end;
end;

{ TGSettings }

procedure TSettings.SetModifier(const AValue: TShiftState);
begin
  FModifier:= AValue;
{  TShiftStateEnum = (ssShift, ssAlt, ssCtrl,
    ssLeft, ssRight, ssMiddle, ssDouble,
    // Extra additions
    ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum,
    ssScroll,ssTriple,ssQuad,ssExtra1,ssExtra2);}
end;

procedure TSettings.SetSampleRate(AValue: single);
begin
  if FSampleRate = AValue then Exit;
  FSampleRate := AValue;
  FHalfSampleRate := AValue / 2;
end;

procedure TSettings.SetSelectedPatternGUI(const AValue: TObject);
begin
  FSelectedPatternGUI := AValue;
end;

procedure TSettings.SetFrames(const AValue: Integer);
begin
  FFrames := AValue;
end;

constructor TSettings.Create;
begin
  FIDCounter := 0;
  CursorPosition := 0;
  FMapToVisible := False;
end;

destructor TSettings.Destroy;
begin

  inherited Destroy;
end;

function TSettings.Load(AFileLocation: string): Boolean;
var
  lXMLConfig: TXMLConfig;
begin
  if FileExists(AFileLocation) then
  begin
    lXMLConfig := TXMLConfig.Create(nil);
    try
      lXMLConfig.Filename := AFileLocation;

      GSettings.SampleMap := lXMLConfig.GetValue('samplemap', '');
    finally
      lXMLConfig.Free;
    end;
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

function TSettings.Save(AFileLocation: string): Boolean;
var
  lXMLConfig: TXMLConfig;
begin
  lXMLConfig := TXMLConfig.Create(nil);
  try
    lXMLConfig.Filename := AFileLocation;
    lXMLConfig.SetValue('samplemap', GSettings.SampleMap);
    lXMLConfig.Flush;
  finally
    lXMLConfig.Free;
  end;
end;

function TSettings.NextID: Integer;
begin
  Inc(FIDCounter);
  Result := FIDCounter;
end;

procedure TSettings.Update(Subject: THybridPersistentModel);
begin
  //
end;

{ TWaveFile }

constructor TWaveFile.Create(AObjectOwner: string; AMapped: Boolean);
var
  lChannelIndex: Integer;
  lChannel: TChannel;
begin
  inherited Create(AObjectOwner);

  FBufferFormat := bfInterleave;

  FChannelList := TObjectList.Create(True);

  for lChannelIndex := 0 to 7 do
  begin
    lChannel := TChannel.Create;
    FChannelList.Add(lChannel);
  end;
end;

destructor TWaveFile.Destroy;
begin
  FChannelList.Free;

  inherited Destroy;
end;

function TWaveFile.LoadSample(AFileName: string): Boolean;
var
  lFilename: pchar;
  lChannelIndex: Integer;
  lChannelSize: Integer;
  lChannelItems: Integer;
  lBufferIndex: Integer;
  lBuffer: PSingle;
  lSampleHandle: PSndFile;
  lSampleInfo: SF_INFO;
begin
  Result := False;

  lFilename:= StringToPChar(AFilename);
  lSampleHandle := sf_open(lFilename, SFM_READ, lSampleInfo);
  try
    if not Assigned(lSampleHandle) then
    begin
      DBLog(sf_strerror(lSampleHandle));
      Result := False;
    end
    else
    begin
      FDataSize := lSampleInfo.frames * lSampleInfo.channels * SizeOf(Single);
      FChannelCount := lSampleInfo.channels;
      FFrames := lSampleInfo.frames;
      FSampleRate := lSampleInfo.samplerate;

      if Assigned(FData) then
      begin
        Freemem(FData);
      end;

      GetMem(FData, FDataSize * 4 + 1000);
      try
        FReadCount := sf_read_float(lSampleHandle, FData, FDataSize);

      except
        on e: exception do
        begin
          Result := False;

          raise;
        end;
      end;
      lChannelSize := (FReadCount div FChannelCount) * SizeOf(Single);
      lChannelItems := FReadCount div FChannelCount;

      if FReadCount = 0 then
      begin
        // Something went wrong
      end
      else
      begin
        case FBufferFormat of
        bfSplit:
          begin
            (* Split channel version *)
            for lChannelIndex := 0 to Pred(FChannelCount) do
            begin
              TChannel(FChannelList[lChannelIndex]).Buffer := GetMem(lChannelSize);
            end;

            for lChannelIndex := 0 to Pred(FChannelCount) do
            begin
              lBuffer := TChannel(FChannelList[lChannelIndex]).Buffer;
              TChannel(FChannelList[lChannelIndex]).BufferSize := FDataSize;

              for lBufferIndex := 0 to Pred(lChannelItems) do
              begin
                lBuffer[lBufferIndex] := FData[lBufferIndex * FChannelCount + lChannelIndex];
              end;
            end;
          end;
        bfInterleave:
          begin;
            (* Interleaved channel version *)
            TChannel(FChannelList[0]).Buffer := FData;
            TChannel(FChannelList[0]).BufferSize := FDataSize;
          end;
        end;
      end;

      Result := True;
    end

  finally
    sf_close(lSampleHandle);
  end;
end;


procedure TWaveFile.UnloadSample;
begin
  //
end;

{ TChannel }

destructor TChannel.Destroy;
begin
  if Assigned(Buffer) then
    FreeMem(Buffer);

  inherited Destroy;
end;

{ TChannelList }

procedure TChannelList.SetObject(Index: Integer; Item: TChannel);
begin
  inherited Items[Index] := Item;
end;

function TChannelList.GetObject(Index: Integer): TChannel;
begin
  Result := inherited Items[Index] as TChannel;
end;

function TChannelList.Add(Obj: TChannel): Integer;
begin
  Result := inherited Add( Obj );
end;

procedure TChannelList.Insert(Index: Integer; Obj: TChannel);
begin
  inherited Insert( Index, Obj );
end;

initialization
  GJackAudio := TJackAudio.Create;
  GObjectMapper := TObjectMapper.Create;
  GSettings := TSettings.Create;
  GSettings.Load('config.xml');

finalization
  GJackAudio.Free;
  GObjectMapper.Free;
  GSettings.Save('config.xml');
  GSettings.Free;

end.

