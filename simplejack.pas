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

  simplejack.pas
}
unit simplejack;

{$mode Objfpc}{$H+}{$INLINE+}

interface

uses
  Classes, Sysutils, Lresources, Forms, LCLProc, Controls, Graphics, Dialogs,
  StdCtrls, jack, midiport, jacktypes, ExtCtrls, Math, sndfile, wave, Spin,
  ContNrs, transport, FileCtrl, PairSplitter, Utils, ComCtrls, GlobalConst,
  Menus, ActnList, dialcontrol, bpm,
  TypInfo, FileUtil, global_command, LCLType, LCLIntf,
  ShellCtrls, Grids, TrackGUI, wavegui, global, track, pattern,
  audiostructure, midigui, mapmonitor, syncobjs, eventlog,
  midi, db, aboutgui, global_scriptactions, plugin, pluginhostgui,
  ringbuffer, optionsgui, wavepatterncontrolgui, midipatterncontrolgui,
  wavepatterngui, midipatterngui, patterngui, sampler;

const
  DIVIDE_BY_120_MULTIPLIER = 1 / 120;
  DIV_BY_24 = 1 /24;

type

  TMidiEvent = record
    // No function yet as midi-events are processed as fast as possible
    // all depending on samplerate, midithread priority and rate
    TimeStamp: single;
    // Size of 'Data' buffer as these are the packets delivered by Jack
    Size: Integer;
    Data: ^Byte;
  end;

  TMainApp = class;

  { TMidiMessage }

  TMidiMessage = class(TObject)
  public
    // No function yet as midi-events are processed as fast as possible
    // all depending on samplerate, midithread priority and rate
    Time: longword;
    DataValue1: Byte;
    DataValue2: Byte;
    DataValue3: Byte;

    constructor Create(AJackMidiEvent: pjack_midi_event_t);
    destructor Destroy; override;
  end;

  {

    TMIDIThread

      Receives midi data from the jack callback function. Here it will map a midi
      controller # to a model parameter.

  }

  TMIDIThread = class(TThread)
  private
    FRingBuffer: pjack_ringbuffer_t;
    FBufferSize: Integer;
    FMidiController: string;
    procedure Updater;
    procedure DoMidiCallback(var AMidiController: string);
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean);
    destructor Destroy; override;
    function PushMidiMessage(AJackMidiEvent: pjack_midi_event_t): Boolean;
    function PopMidiMessage(AJackMidiEvent: TMidiMessage): Boolean;
  end;


  { TMainApp }
  TMainApp = class(Tform, IObserver)
    acPlay: TAction;
    acStop: TAction;
    acPause: TAction;
    acRedo: TAction;
    acAbout: TAction;
    acUndo: TAction;
    alGlobalActions: TActionList;
    LeftSplitter: TCollapseSplitter;
    BottomSplitter: TCollapseSplitter;
    DialControl1: TDialControl;
    ilGlobalImages: TImageList;
    MainMenu1: TMainMenu;
    HelpMenu: TMenuItem;
    MenuItem3: TMenuItem;
    miCreateTrack: TMenuItem;
    pnlToolbar: TPanel;
    pnlVarious: TPanel;
    pnlBottom: TPanel;
    pupScrollBox: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SavePattern: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    LoadMenu: TMenuItem;
    LoadSession: TMenuItem;
    pnlFileManager: TPanel;
    SaveMenu: TMenuItem;
    ControlMenu: TMenuItem;
    OptionMenu: TMenuItem;
    SaveSession: TMenuItem;
    SaveTrack: TMenuItem;
    gbTrackDetail: Tgroupbox;
    pnlTop: Tpanel;
    Sbtracks: Tscrollbox;
    ScreenUpdater: TTimer;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    tbPlay: TToolButton;
    tbStop: TToolButton;
    tbPause: TToolButton;
    tbUndo: TToolButton;
    tbRedo: TToolButton;
    TreeView1: TTreeView;
    procedure acAboutExecute(Sender: TObject);
    procedure acPauseExecute(Sender: TObject);
    procedure acPlayExecute(Sender: TObject);
    procedure acRedoExecute(Sender: TObject);
    procedure acRedoUpdate(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure acUndoExecute(Sender: TObject);
    procedure acUndoUpdate(Sender: TObject);
    procedure btnCompileClick(Sender: TObject);
    procedure btnCreateTrackClick(Sender: TObject);
    procedure cbPitchedChange(Sender: TObject);
    procedure DialControl1StartChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LeftSplitterDblClick(Sender: TObject);
    procedure DialControl1Change(Sender: TObject);
    procedure FileListBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure gbOutputClick(Sender: TObject);
    procedure LoadSessionClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure miCreateTrackClick(Sender: TObject);
    procedure SavePatternClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure OptionMenuClick(Sender: TObject);
    procedure rgEditModeClick(Sender: TObject);
    procedure SaveSessionClick(Sender: TObject);
    procedure SaveTrackClick(Sender: TObject);
    procedure sbTracksDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure sbTracksDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure sbTracksResize(Sender: TObject);
    procedure ScreenUpdaterTimer(Sender: TObject);
    procedure Formdestroy(Sender: Tobject);
    procedure Formcreate(Sender: Tobject);
    procedure Btndeletetrackclick(Sender: Tobject);
    procedure ShuffleProc(Sender: Tobject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Collapsed(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Deletion(Sender: TObject; Node: TTreeNode);
    procedure TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure UpdateTrackControls(Sender: TObject);
  private
    { private Declarations }
    FShuffleList: TObjectList;
    Tracks: TObjectList;
    FSimpleWaveForm: TSimpleWaveForm;
    FOutputWaveform: Boolean;
    FMappingMonitor: TfmMappingMonitor;
    FObjectID: string;
    FObjectOwnerID: string;
    FLowPriorityInterval: Integer;
    FMediumPriorityInterval: Integer;
    FHighPriorityInterval: Integer;
    FWavePatternControlGUI: TWavePatternControlGUI;
    FMidiPatternControlGUI: TMidiPatternControlGUI;
    FNoJackMode: Boolean;
    FModel: TAudioStructure;

    procedure CustomExceptionHandler(Sender: TObject; E: Exception);
    procedure ReleaseTrack(Data: PtrInt);
    function TrackExists(AObjectID: string): Boolean;
    function IndexOfTrack(AObjectId: string): Integer;
    procedure DoTracksRefreshEvent(TrackObject: TObject);
    procedure DoPatternRefreshEvent(TrackObject: TObject);
    function ShuffleByObject(TrackObject: TObject): TShuffle;
    procedure UpdateTracks(TrackObject: TTrack);
    procedure DeleteShuffleByObject(TrackObject: TObject);
    procedure ArrangeShuffleObjects;

    function CreateTrack(AFileLocation: string; ATrackType: Integer = 0): TTrackGUI;
    procedure CreateTrackGUI(AObjectID: string);
    procedure DeleteTrackGUI(AObjectID: string);

    function HasSubFolder(const Directory: string): Boolean;
    procedure LoadTreeDirectory;
    procedure AddSubFolders(const Directory: string;
      ParentNode: TTreeNode);

    procedure LoadGlobalSession;
    procedure SaveGlobalSession;
  protected
  public
    { public Declarations }
    procedure Update(Subject: THybridPersistentModel); reintroduce;
    procedure Connect;
    procedure Disconnect;
    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    function GetObjectOwnerID: string; virtual;
    procedure SetObjectOwnerID(const AObjectOwnerID: string);
    property ObjectOwnerID: string read GetObjectOwnerID write SetObjectOwnerID;
    property ObjectID: string read GetObjectID write SetObjectID;
    property MappingMonitor: TfmMappingMonitor read FMappingMonitor write FMappingMonitor;
    function GetModel: THybridPersistentModel;
    procedure SetModel(AModel: THybridPersistentModel);
    property Model: THybridPersistentModel read GetModel write SetModel;
  end;

var
  MainApp: TMainApp;

  midi_input_port : ^jack_port_t;
  midi_output_port : ^jack_port_t;
  audio_input_port : ^jack_port_t;
  audio_output_port_left : ^jack_port_t;
  audio_output_port_right : ^jack_port_t;
	client : ^jack_client_t;
  note_frqs : array[0..127] of jack_default_audio_sample_t;
  note : byte;
  last_note:byte;
  ramp : longint;
  note_on : jack_default_audio_sample_t;
  buffer_allocate2: ^jack_default_audio_sample_t;
  incoming_bpm: single;
  lastsyncposition: single;
  sync_counter:Integer;
  samplerate:single;
  CurrentSlice: TMarker;
  transport_pos : jack_position_t;
  MIDIThread: TMIDIThread;
  attack_coef: Single;
  attack_in_ms: Single;
  release_coef: Single;
  release_in_ms: Single;
  FShowMapping: Boolean;

  FCriticalSection: TCriticalSection;

implementation

uses
  fx, librubberband;

function DumpCallStack: string;
var
  I: Longint;
  prevbp: Pointer;
  CallerFrame,
  CallerAddress,
  bp: Pointer;
  Report: string;
const
  MaxDepth = 20;
begin
  Report := '';
  bp := get_frame;
  // This trick skip SendCallstack item
  // bp:= get_caller_frame(get_frame);
  try
    prevbp := bp - 1;
    I := 0;
    while bp > prevbp do begin
       CallerAddress := get_caller_addr(bp);
       CallerFrame := get_caller_frame(bp);
       if (CallerAddress = nil) then
         Break;
       Report := Report + BackTraceStrFunc(CallerAddress) + LineEnding;
       Inc(I);
       if (I >= MaxDepth) or (CallerFrame = nil) then
         Break;
       prevbp := bp;
       bp := CallerFrame;
     end;
   except
     { prevent endless dump if an exception occured }
   end;
  Result := Report;
end;

function DumpExceptionCallStack(E: Exception): string;
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  Result := Report;
end;



function compareByLocation(Item1 : Pointer; Item2 : Pointer) : Integer;
var
  location1, location2 : TShuffle;
begin
  // We start by viewing the object pointers as TShuffle objects
  location1 := TShuffle(TTrackGUI(Item1).Shuffle);
  location2 := TShuffle(TTrackGUI(Item2).Shuffle);

  // Now compare by location
  if location1.x > location2.x then
    Result := 1
  else if location1.x = location2.x then
    Result := 0
  else
    Result := -1;
end;

{ TMainApp }

procedure calc_note_frqs(srate : jack_default_audio_sample_t);
var
  i : integer;
begin
	for i := 0 to 127 do
	begin
		note_frqs[i] := 440 * power(2, ((jack_default_audio_sample_t(i) - 69.0) / 12.0));// / srate;
  end;
end;

function srate(nframes : jack_nframes_t ; arg : pointer): longint; cdecl;
begin
	calc_note_frqs(jack_default_audio_sample_t(nframes));
end;

function process_midi_buffer(AMidiPattern: TMidiPattern; AMidiOutBuf: pointer; AFrames: Integer; ATrack: TTrack): Integer;
var
  buffer: ^byte;
  lFrameOffsetLow: Integer;
  lFrameOffsetHigh: Integer;
  lRelativeLocation: Integer;
  lMidiData: TMidiData;
begin

  // Only process when not in state change
  if AMidiPattern.Enabled and (AMidiPattern.MidiDataList.Count > 0) then
  begin
    lFrameOffsetLow := ((AMidiPattern.RealCursorPosition div AFrames) * AFrames);
    lFrameOffsetHigh := ((AMidiPattern.RealCursorPosition div AFrames) * AFrames) + AFrames;

    while (AMidiPattern.MidiDataList.CurrentMidiData.Location < lFrameOffsetHigh) and
      (not AMidiPattern.MidiDataList.Eof) do
    begin
      lMidiData := AMidiPattern.MidiDataList.CurrentMidiData;

      if AMidiPattern.RealCursorPosition > lMidiData.Location then
      begin
        lRelativeLocation := 0
      end
      else
      begin
        lRelativeLocation := lMidiData.Location mod AFrames;
      end;

      buffer := jack_midi_event_reserve(AMidiOutBuf, lRelativeLocation, 3);
      if Assigned(buffer) then
      begin
        case lMidiData.DataType of
          mtNoteOn:
          begin
  			    buffer[0] := $90 + AMidiPattern.MidiChannel;	{ note on }
  			    buffer[1] := lMidiData.DataValue1;
            buffer[2] := lMidiData.DataValue2;		{ velocity }
          end;
          mtNoteOff:
          begin
    				buffer[0] := $80 + AMidiPattern.MidiChannel;;	{ note off }
    				buffer[1] := lMidiData.DataValue1;
    				buffer[2] := 0;		{ velocity }
          end;
          mtBankSelect:
          begin

          end;
        end;
      end
      else
      begin
        ATrack.DevValue := 'jackmidi buffer allocation failed';
      end;

      AMidiPattern.MidiDataList.Next;
    end;
  end;

  Result := 0
end;

procedure jack_shutdown(arg: pointer); cdecl;
begin
  exit;
end;

function process(nframes: jack_nframes_t; arg:pointer): longint; cdecl;
var
  i, j : integer;
  midi_in_buf : pointer;
  midi_out_buf : pointer;
  output_left : ^jack_default_audio_sample_t;
  output_right : ^jack_default_audio_sample_t;
  input : ^jack_default_audio_sample_t;
	in_event : jack_midi_event_t;
  event_index : jack_nframes_t;
  event_count : jack_nframes_t;
  transport_state : jack_transport_state_t;
  lTrack: TTrack;
  TempLevel: jack_default_audio_sample_t;
  lPlayingPattern: TPattern;
  buffer_size: Integer;
  sync_frame: Integer;
begin

  buffer_size := nframes * SizeOf(Single);

  // Get pgPattern-input and pgAudio-output buffers
  midi_in_buf := jack_port_get_buffer(midi_input_port, nframes);
  midi_out_buf := jack_port_get_buffer(midi_output_port, nframes);
	output_left := jack_port_get_buffer(audio_output_port_left, nframes);
	output_right := jack_port_get_buffer(audio_output_port_right, nframes);
	input := jack_port_get_buffer(audio_input_port, nframes);

  if not GAudioStruct.Active then
  begin
    for i := 0 to Pred(nframes) do
    begin
      output_left[i] := 0;
      output_right[i] := 0;
    end;

    exit;
  end;

  jack_midi_clear_buffer(midi_out_buf);

  // Query BPM from transport
  transport_state := jack_transport_query(client, @transport_pos);

  // Default to no-sync
  sync_frame := -1;

  // Detect at which frame a sync should be done
  for i := 0 to Pred(nframes) do
  begin
    GAudioStruct.MainSyncCounter :=
      GAudioStruct.MainSyncCounter + GAudioStruct.BPMScale;

    if GAudioStruct.MainSyncCounter >= GAudioStruct.MainSyncLength then
    begin
      GAudioStruct.MainSyncCounter :=
        GAudioStruct.MainSyncCounter - GAudioStruct.MainSyncLength;

      sync_frame := i;
    end;
  end;

  // Get number of pending pgPattern-events
	event_count := jack_midi_get_event_count(midi_in_buf, nframes);
	event_index := 0;

	jack_midi_event_get(@in_event, midi_in_buf, 0, nframes);

  // Silence y'all!
  for i := 0 to Pred(nframes) do
  begin
    output_left[i] := 0;
    output_right[i] := 0;

    // Detect MIDI-events per sample
		if (in_event.time = i) and (event_index < event_count) then
    begin
			if in_event.buffer^ and $f0 = $90 then
      begin
				// note on
				note := (in_event.buffer + 1)^;
				note_on := 1.0;
      end
			else if in_event.buffer^ and $f0 = $80 then
      begin
				// note off
				note := in_event.buffer^ + 1;
				note_on := 0.0;
      end
			else if in_event.buffer^ and $f0 = $F8 then
      begin
        // Calculate average bpm
        GAudioStruct.BPM := (GAudioStruct.BPM + ((samplerate * DIV_BY_24) / (lastsyncposition - sync_counter)) * 60) * 0.5;
      end
			else if in_event.buffer^ and $f0 = $FA then
      begin
				// clock start
      end
			else if in_event.buffer^ and $f0 = $FB then
      begin
				// clock continue
      end
			else if in_event.buffer^ and $f0 = $FC then
      begin
				// clock stop
      end;

      {
        Push midi messages directly to the midithread to handle recording of notes,
        midi controller mapping, etc
      }
      MIDIThread.PushMidiMessage(@in_event);

      if note <> 0 then
        last_note := note;
        
			inc( event_index );
			if event_index < event_count then
				jack_midi_event_get(@in_event, midi_in_buf, event_index, nframes);
    end;

    inc(sync_counter);
  end;

  for j := 0 to Pred(GAudioStruct.Tracks.Count) do
  begin
    lTrack := TTrack(GAudioStruct.Tracks.Items[j]);
    if Assigned(lTrack) then
    begin

       // Increment cursor regardless if audible or not
      if Assigned(lTrack.PlayingPattern) then
      begin
        lTrack.PlayingPattern.ProcessInit;

        if lTrack.Playing then
        begin
          // Send midi pattern to jack buffer
          if lTrack.PlayingPattern is TMidiPattern then
          begin
            process_midi_buffer(TMidiPattern(lTrack.PlayingPattern), midi_out_buf, nframes, lTrack);
          end;
        end;
      end;

      for i := 0 to Pred(nframes) do
      begin

        if lTrack.Playing then
        begin
          // Synchronize section
          if i = sync_frame then
          begin
            if Assigned(lTrack.ScheduledPattern) then
            begin
              if Assigned(lTrack.PlayingPattern) then
              begin
                lTrack.PlayingPattern.Playing := False;
              end;
              lTrack.PlayingPattern := lTrack.ScheduledPattern;

              if lTrack.PlayingPattern is TWavePattern then
              begin;
                TWavePattern(lTrack.PlayingPattern).TimeStretch.Flush;
              end
              else if lTrack.PlayingPattern is TMidiPattern then
              begin
                if TMidiPattern(lTrack.PlayingPattern).MidiDataList.Count > 0 then
                begin
                  TMidiPattern(lTrack.PlayingPattern).MidiDataList.First;
                  TMidiPattern(lTrack.PlayingPattern).MidiDataCursor :=
                    TMidiData( TMidiPattern(lTrack.PlayingPattern).MidiDataList.Items[0] );
                end;
              end;

              lTrack.PlayingPattern.Playing := True;
              lTrack.PlayingPattern.Scheduled := False;
              lTrack.PlayingPattern.SyncQuantize := True;
              lTrack.ScheduledPattern := nil;
            end;
          end;

          if Assigned(lTrack.PlayingPattern) then
          begin
            lPlayingPattern := lTrack.PlayingPattern;

            if lPlayingPattern.OkToPlay then
            begin
              lPlayingPattern.Process(lTrack.OutputBuffer, i, nframes);
              lPlayingPattern.ProcessAdvance;
            end;
          end;
        end;
      end;
    end;
  end;

  for j := 0 to Pred(GAudioStruct.Tracks.Count) do
  begin
    lTrack := TTrack(GAudioStruct.Tracks.Items[j]);
    if Assigned(lTrack) then
    begin

      lPlayingPattern := lTrack.PlayingPattern;
      if Assigned(lPlayingPattern) then
      begin
        if lPlayingPattern.OkToPlay then
        begin

          if lTrack.Playing then
          begin

            if lTrack.Active then
            begin
              { TODO Not switched on
                lPlayingPattern.WavePattern.DiskWriterThread.RingbufferWrite(input[0], nframes);
              }
              if lPlayingPattern is TMidiPattern then
              begin
                FillByte(lTrack.OutputBuffer[0], buffer_size, 0);
                TMidiPattern(lPlayingPattern).SampleBankEngine.Process( TMidiPattern(lPlayingPattern).MidiBuffer,
                  lTrack.OutputBuffer, nframes);
              end;

              // 1. Execute per pattern plugins
  {            lPlayingPattern.PluginProcessor.Execute(nframes, lPlayingPattern.WavePattern.BufferData2);

              // 2. Execute per track plugins
              lTrack.PluginProcessor.Execute(nframes, lPlayingPattern.PluginProcessor.Buffer);

              // 3. Apply tracksettings (Level, Mute, ...) to track output buffer
              for i := 0 to Pred(nframes) do
              begin
                lTrack.OutputBuffer[i] :=
                  (lTrack.PluginProcessor.Buffer[i] + input[i]) * lTrack.VolumeMultiplier * 1.5;
              end;          }

              // Mix to the jack out only when audible
              if lTrack.VolumeMultiplier > DENORMAL_KILLER then
              begin
                for i := 0 to Pred(nframes) do
                begin
                  lTrack.OutputBuffer[i] := lTrack.OutputBuffer[i] * lTrack.VolumeMultiplier;
                  output_left[i] := output_left[i] + lTrack.OutputBuffer[i];
                  output_right[i] := output_right[i] + lTrack.OutputBuffer[i];
                end;
              end;

              // Copy to track output (JUST FOR DEBUGGING, use MasterOut )
              {Move(lTrack.OutputBuffer[0], output_left[0], buffer_size);
              Move(lTrack.OutputBuffer[0], output_right[0], buffer_size);}
            end;
          end;
        end;
      end;

      // Should visible levels be calculated here?? Maybe the mainthread...
      // Save's a lot of multiplications
      for i := 0 to Pred(nframes) do
      begin
        TempLevel := Abs(lTrack.OutputBuffer[i] * lTrack.VolumeMultiplier);
        if TempLevel > lTrack.Level then
          lTrack.Level := (attack_coef * (lTrack.Level - TempLevel)) + TempLevel
        else
          lTrack.Level := (release_coef * (lTrack.Level - TempLevel)) + TempLevel;
      end;
    end;
  end;
  //------- End effects section ----------------------------------------

  // Move to displaybuffer
  if GAudioStruct.Tracks.Count > 0 then
  begin
    lTrack := TTrack(GAudioStruct.Tracks.Items[0]);
    if lTrack.Playing then
    begin
      move(output_left[0], buffer_allocate2[0], buffer_size);
    end;
  end;

  Result := 0;
end;

{ TMidiMessage }

constructor TMidiMessage.Create(AJackMidiEvent: pjack_midi_event_t);
begin
  inherited Create;

  DataValue1 := AJackMidiEvent^.buffer^;
  DataValue2 := (AJackMidiEvent^.buffer + 1)^;
  DataValue3 := (AJackMidiEvent^.buffer + 2)^;
  Time := AJackMidiEvent^.time;
end;

destructor TMidiMessage.Destroy;
begin

  inherited Destroy;
end;

procedure TMainApp.CustomExceptionHandler(Sender: TObject; E: Exception);
begin
  {DumpExceptionCallStack(E);}
  {DumpCallStack;}
  {Halt; // End of program execution   }
end;

procedure TMainApp.acStopExecute(Sender: TObject);
var
  i: Integer;
  lWavePattern: TWavePattern;
  lMidiPattern: TMidiPattern;
begin
  // Stop
  GAudioStruct.PlayState:= psStop;
  for i := 0 to Pred(GAudioStruct.Tracks.Count) do
  begin
    TTrack(GAudioStruct.Tracks[i]).Playing:= False;
    if Assigned(TTrack(GAudioStruct.Tracks[i]).PlayingPattern) then
    begin
      if TTrack(GAudioStruct.Tracks[i]).PlayingPattern is TWavePattern then
      begin
        lWavePattern := TWavePattern(TTrack(GAudioStruct.Tracks[i]).PlayingPattern);
        lWavePattern.CursorAdder := lWavePattern.LoopStart.Location;
      end
      else if TTrack(GAudioStruct.Tracks[i]).PlayingPattern is TMidiPattern then
      begin
        lMidiPattern := TMidiPattern(TTrack(GAudioStruct.Tracks[i]).PlayingPattern);
        lMidiPattern.CursorAdder := lMidiPattern.LoopStart.Location;
      end
    end;
  end;
end;

procedure TMainApp.acUndoExecute(Sender: TObject);
var
  lCommand: TCommand;
  lTrimIndex: Integer;
begin
  // Undo

  // Make shure we're in a valid region
  if (GHistoryIndex > -1) and (GHistoryIndex < GHistoryQueue.Count) then
  begin
    lCommand := TCommand(GHistoryQueue[GHistoryIndex]);
    if Assigned(lCommand) then
    begin
      try
        DBLog('Rolling back command class: %s', lCommand.ClassName);
        lCommand.Initialize;
        lCommand.Rollback;
        lCommand.Finalize;

        for lTrimIndex := Pred(GHistoryQueue.Count) downto GHistoryIndex do
        begin
          DBLog('Deleting history: %d start',lTrimIndex);
          GHistoryQueue.Delete(lTrimIndex);
          DBLog('Deleting history: %d end',lTrimIndex);
        end;

        Dec(GHistoryIndex);
      except
        on e:exception do
        begin
          DBLog(Format('Internal error at acUndoExecute: class: %s, %s, %s ',
            [lCommand.ClassName, DumpExceptionCallStack(e), DumpCallStack]));
          lCommand.Free;
        end;
      end;
    end;
  end;
end;

procedure TMainApp.acUndoUpdate(Sender: TObject);
begin
  tbUndo.Enabled := ((GHistoryIndex > -1) and (GHistoryIndex < GHistoryQueue.Count));
end;

procedure TMainApp.btnCompileClick(Sender: TObject);
{var
  lLine: Integer;
  lMessage: string;}
begin
  DBLog('start Compile');

  // Link editor to the scriptengine
{  FPascalScript.Script := ScriptEditor.Lines;

  if FPascalScript.Compile then
  begin
    for lLine := 0 to Pred(FPascalScript.CompilerMessageCount) do
    begin
      lMessage := lMessage + FPascalScript.CompilerMessages[lLine].MessageToString + #13#10;
    end;
  end
  else
  begin
    lMessage := 'Compile failed!';
  end;

  ScriptMessages.Lines.Text := lMessage;}

  DBLog('end Compile');
end;

procedure TMainApp.btnCreateTrackClick(Sender: TObject);
var
  lCommandCreateTrack: TCreateTrackCommand;
begin
  lCommandCreateTrack := TCreateTrackCommand.Create(GAudioStruct.ObjectID);
  try
    GCommandQueue.PushCommand(lCommandCreateTrack);

  except
    lCommandCreateTrack.Free;
  end;
End;

procedure TMainApp.acPlayExecute(Sender: TObject);
var
  i: Integer;
begin
  // Play
  GAudioStruct.PlayState:= psPlay;
  GAudioStruct.Active := True;

  for i := 0 to Pred(GAudioStruct.Tracks.Count) do
  begin
    TTrack(GAudioStruct.Tracks[i]).Playing := True;
  end;
end;

procedure TMainApp.acRedoExecute(Sender: TObject);
var
  lCommand: TCommand;
begin
  // Redo

  // Make shure we're in a valid region
  if (GHistoryIndex > -1) and (GHistoryIndex < GHistoryQueue.Count) then
  begin
    lCommand:= TCommand(GHistoryQueue[GHistoryIndex]);
    if Assigned(lCommand) then
    begin
      lCommand.Initialize;
      lCommand.Execute;
      lCommand.Finalize;
      Inc(GHistoryIndex);
    end;
  end;
end;

procedure TMainApp.acRedoUpdate(Sender: TObject);
begin
  tbRedo.Enabled := False;//((GHistoryIndex > -1) and (GHistoryIndex <= GHistoryQueue.Count));
end;

procedure TMainApp.acPauseExecute(Sender: TObject);
begin
  // Pause
  GAudioStruct.PlayState:= psPause;
end;

procedure TMainApp.acAboutExecute(Sender: TObject);
begin
  //
end;

procedure TMainApp.cbPitchedChange(Sender: TObject);
begin
  {if Assigned(GAudioStruct.SelectedTrack) then
  begin
    //   Put command object
    GAudioStruct.SelectedTrack.SelectedPattern.Pitched := cbPitched.Checked;
  end;}
end;

procedure TMainApp.DialControl1StartChange(Sender: TObject);
var
  lBPMChangeCommand: TBPMChangeCommand;
begin
  lBPMChangeCommand := TBPMChangeCommand.Create(MainApp.ObjectID);
  lBPMChangeCommand.BPM := DialControl1.Value;
  lBPMChangeCommand.Persist := True;

  GCommandQueue.PushCommand(lBPMChangeCommand);
end;

procedure TMainApp.FormResize(Sender: TObject);
begin
  Invalidate;
end;

procedure TMainApp.FormShow(Sender: TObject);
begin
  if not Assigned(MIDIThread) then
  begin
    MIDIThread:= TMIDIThread.Create(False);
    MIDIThread.FreeOnTerminate:= True;
  end;
end;

procedure TMainApp.LeftSplitterDblClick(Sender: TObject);
begin
  if gbTrackDetail.Width < 30 then
    gbTrackDetail.Width := 250
  else
    gbTrackDetail.Width := 0;
end;

procedure TMainApp.DialControl1Change(Sender: TObject);
var
  lBPMChangeCommand: TBPMChangeCommand;
begin
  lBPMChangeCommand := TBPMChangeCommand.Create(MainApp.ObjectID);
  lBPMChangeCommand.BPM := DialControl1.Value;
  lBPMChangeCommand.Persist := False;

  GCommandQueue.PushCommand(lBPMChangeCommand);
end;

procedure TMainApp.FileListBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  BeginDrag(False, 5);
end;


procedure TMainApp.FormDropFiles(Sender: TObject; const FileNames: array of String
  );
var P:Tpoint;
begin
  DBLog(FileNames[0]);
  GetCursorPos(P);
  DBLog(FindDragTarget(P, False).Name);
end;

procedure TMainApp.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  case key of
    VK_ESCAPE:
    begin
      GSettings.EscapeAction:= True;
    end;
  end;

  GSettings.Modifier:= Shift;
end;

procedure TMainApp.FormKeyPress(Sender: TObject; var Key: char);
begin
//  MessageDlg('test', Key, mtWarning, [mbOK], 'test');
end;

procedure TMainApp.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // No modifiers active when key is up
  GSettings.Modifier:= [];
end;

procedure TMainApp.gbOutputClick(Sender: TObject);
begin
  FOutputWaveform := not FOutputWaveForm;
end;

procedure TMainApp.LoadSessionClick(Sender: TObject);
begin
  LoadGlobalSession;
end;

procedure TMainApp.MenuItem1Click(Sender: TObject);
begin

end;

procedure TMainApp.MenuItem2Click(Sender: TObject);
begin
  //
  Application.Terminate;
end;

procedure TMainApp.miCreateTrackClick(Sender: TObject);
begin
  CreateTrack('', 2);
end;

procedure TMainApp.SavePatternClick(Sender: TObject);
begin

end;

procedure TMainApp.MenuItem4Click(Sender: TObject);
var
  lAbout: TfmAbout;
begin
  lAbout := TfmAbout.Create(nil);
  try
    lAbout.ShowModal;
  finally
    lAbout.Free;
  end;
end;

procedure TMainApp.OptionMenuClick(Sender: TObject);
var
  lFmOptions: TfmOptions;
begin

  lFmOptions := TfmOptions.Create(nil);
  try
    lFmOptions.Settings := GSettings;

    if lFmOptions.ShowModal = mrOK then
    begin

    end;
  finally
    lFmOptions.Free;
  end;

end;

procedure TMainApp.rgEditModeClick(Sender: TObject);
begin
  GSettings.EditMode := 0;
end;

procedure TMainApp.SaveSessionClick(Sender: TObject);
begin
  SaveGlobalSession;
end;

procedure TMainApp.SaveTrackClick(Sender: TObject);
begin

end;

procedure TMainApp.sbTracksDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  lTreeView: TTreeView;
begin
  DBLog('start TMainApp.sbTracksDragDrop');

  if Source is TTreeView then
  begin
    lTreeView := TTreeView(Source);
    CreateTrack(TTreeFolderData(lTreeView.Selected.Data).Path, 0);
  end;

  Invalidate;

  DBLog('end TMainApp.sbTracksDragDrop');
end;

procedure TMainApp.sbTracksDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  // TODO can be wav, track or
  Accept := True;
end;

procedure TMainApp.sbTracksResize(Sender: TObject);
var
  i: Integer;
begin
  for i:= 0 to Pred(GAudioStruct.Tracks.Count) do
  begin
    TTrackGUI(Tracks.Items[i]).Height := Sbtracks.Height;
  end;
end;

procedure TMainApp.ScreenUpdaterTimer(Sender: TObject);
var
  i, j: Integer;
  lTrack: TTrack;
  lMidiPatternGUI: TMidiPatternGUI;
  lWavePatternGUI: TWavePatternGUI;
begin
  Application.ProcessMessages;
  try
    FSimpleWaveForm.Invalidate;

    acUndoUpdate(Self);
    acRedoUpdate(Self);

    // Handle update of objects
    if FHighPriorityInterval = 0 then
    begin
      MainApp.ArrangeShuffleObjects;
    end;

      // Update object mapping
    if FShowMapping then
    begin
      if FLowPriorityInterval = 0 then
      begin
        MainApp.MappingMonitor.UpdateGrid;
      end;
    end;

    // Update session grid
    for i := 0 to Pred(MainApp.Tracks.Count) do
    begin
      lTrack := TTrackGUI(MainApp.Tracks[i]).Track;

      if Assigned(lTrack) then
      begin
        TTrackGUI(MainApp.Tracks[i]).vcLevel.LevelLeft := lTrack.Level;
        TTrackGUI(MainApp.Tracks[i]).vcLevel.LevelRight := lTrack.Level;
        TTrackGUI(MainApp.Tracks[i]).vcLevel.Invalidate;

        if Assigned(lTrack.PlayingPattern) then
        begin
          for j := 0 to Pred(TTrackGUI(MainApp.Tracks[i]).PatternListGUI.Count) do
          begin
            if TTrackGUI(MainApp.Tracks[i]).PatternListGUI[j] is TMidiPatternGUI then
            begin
              lMidiPatternGUI := TMidiPatternGUI(TTrackGUI(MainApp.Tracks[i]).PatternListGUI[j]);

              if lMidiPatternGUI.ObjectID = lTrack.PlayingPattern.ObjectID then
              begin
                lMidiPatternGUI.CursorPosition := TMidiPattern(lTrack.PlayingPattern).RealCursorPosition;
                lMidiPatternGUI.CacheIsDirty := True;
              end;
            end
            else if TTrackGUI(MainApp.Tracks[i]).PatternListGUI[j] is TWavePatternGUI then
            begin
              lWavePatternGUI := TWavePatternGUI(TTrackGUI(MainApp.Tracks[i]).PatternListGUI[j]);

              if lWavePatternGUI.ObjectID = lTrack.PlayingPattern.ObjectID then
              begin
                lWavePatternGUI.CursorPosition := TWavePattern(lTrack.PlayingPattern).RealCursorPosition;
                lWavePatternGUI.CacheIsDirty := True;
              end;
            end;
          end;
        end;
      end;

      TTrackGUI(MainApp.Tracks[i]).Invalidate;
    end;

    // Update patterneditor grid
    {if Assigned(GSettings.SelectedPatternGUI) then
    begin
      if GSettings.SelectedPatternGUI is TMidiPatternGUI then
      begin
        FMidiPatternControlGUI.MidiPatternGUI.CacheIsDirty := True;
        FMidiPatternControlGUI.MidiPatternGUI.Invalidate;
      end
      else if GSettings.SelectedPatternGUI is TWavePatternGUI then
      begin
        FWavePatternControlGUI.WavePatternGUI.CacheIsDirty := True;
        FWavePatternControlGUI.WavePatternGUI.Invalidate;
      end;
    end; }
    if pnlBottom = FMidiPatternControlGUI.Parent then
    begin
      FMidiPatternControlGUI.MidiPatternGUI.CacheIsDirty := True;
      FMidiPatternControlGUI.MidiPatternGUI.Invalidate;
    end
    else if pnlBottom = FWavePatternControlGUI.Parent then
    begin
      FWavePatternControlGUI.WavePatternGUI.CacheIsDirty := True;
      FWavePatternControlGUI.WavePatternGUI.Invalidate;
    end;

    Inc(FLowPriorityInterval);
    if FLowPriorityInterval > 10 then
      FLowPriorityInterval := 0;
    Inc(FMediumPriorityInterval);
    if FMediumPriorityInterval > 5 then
      FMediumPriorityInterval := 0;
    Inc(FHighPriorityInterval);
    if FHighPriorityInterval > 1 then
      FHighPriorityInterval := 0;

  except
    on e:exception do
    begin
      writeln('Hybrid error: ' + e.Message);
    end;
  end;
end;

procedure TMainApp.Formdestroy(Sender: Tobject);
var
   i: Integer;
begin
  ScreenUpdater.Enabled := False;

  if not FNoJackMode then
  begin
    sleep(100);
    jack_transport_stop(client);

    sleep(100);
    jack_deactivate(client);

    sleep(100);
	  jack_client_close(client);

    sleep(100);
  end;
  if Assigned(GAudioStruct) then
  begin
    GAudioStruct.Detach(MainApp);

    for i:= 0 to Pred(GAudioStruct.Tracks.Count) do
      TTrack(GAudioStruct.Tracks.Items[i]).Playing := False;
  end;

  if Assigned(buffer_allocate2) then
    Freemem(buffer_allocate2);

  if Assigned(FShuffleList) then
    FShuffleList.Free;

  if Assigned(FSimpleWaveForm) then
    FSimpleWaveForm.Free;

  if Assigned(Tracks) then
    Tracks.Free;

  if Assigned(FMappingMonitor) then
    FMappingMonitor.Free;

  if Assigned(GAudioStruct) then
    GAudioStruct.Free;

  FWavePatternControlGUI.Free;
  FMidiPatternControlGUI.Free;
End;

procedure TMainApp.Formcreate(Sender: Tobject);
var
  input_ports: ppchar;
  output_ports: ppchar;
begin
  Application.OnException := @CustomExceptionHandler;

  FNoJackMode := FindCmdLineSwitch('nojack', ['/', '-'], True);

  MainApp.DoubleBuffered := True;
  Sbtracks.DoubleBuffered := True;

  Tracks:= TObjectList.create(True);

  LoadTreeDirectory;

  FWavePatternControlGUI := TWavePatternControlGUI.Create(nil);
  FMidiPatternControlGUI := TMidiPatternControlGUI.Create(nil);

  if not FNoJackMode then
  begin
    client := jack_client_open('loopbox', JackNullOption, nil);
	  if not assigned(client) then
    begin
      DBLog('Error creating jack client!');
      Halt(1);
    end;

	  midi_input_port := jack_port_register (client, 'midi_in', JACK_DEFAULT_MIDI_TYPE, Longword(JackPortIsInput), 0);
	  midi_output_port := jack_port_register (client, 'midi_out', JACK_DEFAULT_MIDI_TYPE, Longword(JackPortIsOutput), 0);
	  audio_input_port := jack_port_register (client, 'audio_in', JACK_DEFAULT_AUDIO_TYPE, Longword(JackPortIsInput), 0);
	  audio_output_port_left := jack_port_register (client, 'audio_out_left', JACK_DEFAULT_AUDIO_TYPE, Longword(JackPortIsOutput), 0);
	  audio_output_port_right := jack_port_register (client, 'audio_out_right', JACK_DEFAULT_AUDIO_TYPE, Longword(JackPortIsOutput), 0);

	  calc_note_frqs(jack_get_sample_rate (client));

    DBLog('Samplerate: ' + IntToStr(Round(jack_get_sample_rate (client))));

    samplerate:= jack_get_sample_rate (client);
    jack_on_shutdown(client, @jack_shutdown, nil);
	  jack_set_sample_rate_callback(client, @srate, nil);
	  DBLog(format('jack_set_process_callback %d', [jack_set_process_callback(client, @process, nil)]));

	  if jack_activate(client) = 1 then
    begin
		  DBLog('cannot activate client');
      halt(1);
    end;

    DBLog('start autoconnect');
    input_ports := jack_get_ports(client, nil, nil, (Longword(JackPortIsPhysical) or Longword(JackPortIsOutput)));
    if not Assigned(input_ports) then
    begin
      writeln('no physical capture ports.');
    end
    else
    begin
      {if jack_connect(client, input_ports[0], jack_port_name(audio_input_port)) <> 0 then
      begin
        writeln('cannot connect input ports');
      end;}
    end;

    output_ports := jack_get_ports(client, nil, nil, (Longword(JackPortIsPhysical) or Longword(JackPortIsInput)));
    if not Assigned(output_ports) then
    begin
      DBLog('no physical playback ports.');
    end
    else
    begin
      if jack_connect(client, jack_port_name(audio_output_port_left), output_ports[0]) <> 0 then
      begin
        DBLog('cannot connect output ports');
      end;
      if jack_connect(client, jack_port_name(audio_output_port_right), output_ports[1]) <> 0 then
      begin
        DBLog('cannot connect output ports');
      end;
    end;
    DBLog('end autoconnect');

    jack_transport_start(client);

    GSettings.SampleRate := samplerate;
    GSettings.Frames := jack_get_buffer_size(client);
  end
  else
  begin
    GSettings.SampleRate := 44100;
    GSettings.Frames := 512;
  end;


  GAudioStruct := TAudioStructure.Create('{D6DDECB0-BA12-4448-BBAE-3A96EEC90BFB}', MAPPED);
  GAudioStruct.Initialize;
  GAudioStruct.MainSampleRate := samplerate;
  GAudioStruct.BPM := 120;

  attack_in_ms := 20;
  release_in_ms := 1000;
  attack_coef := power(0.01, 1.0/( attack_in_ms * GAudioStruct.MainSampleRate * 0.001));
  release_coef := power(0.01, 1.0/( release_in_ms * GAudioStruct.MainSampleRate * 0.001));

  note := 0;
  ramp := 0;
  FOutputWaveform:= False;

  Getmem(buffer_allocate2, 200000 * SizeOf(jack_default_audio_sample_t));

  FShuffleList := TObjectList.create(False);
  FShuffleList.Sort(@compareByLocation);

  FSimpleWaveForm := TSimpleWaveForm.Create(Self);
  FSimpleWaveForm.Data := buffer_allocate2;
  FSimpleWaveForm.Top := 0;
  FSimpleWaveForm.Left := 0;
  FSimpleWaveForm.Width := pnlVarious.Width;
  FSimpleWaveForm.Align := alClient;
  FSimpleWaveForm.Parent := pnlVarious;
  
  FMappingMonitor := TfmMappingMonitor.Create(Self);
  FMappingMonitor.Maps := GObjectMapper.Maps;
  if FShowMapping then
  begin
    FMappingMonitor.Show;
  end;

  GAudioStruct.Attach(MainApp);

  {ChangeControlStyle(Self, [csDisplayDragImage], [], True);}

  ScreenUpdater.Interval := 40;
  ScreenUpdater.Enabled := True;

  pnlVarious.Width := 0;
End;

procedure TMainApp.Btndeletetrackclick(Sender: Tobject);
var
  lCommandDeleteTrack: TDeleteTrackCommand;
  lTrackIndex: Integer;
begin
  if Assigned(GSettings.SelectedTrackGUI) then
  begin
    lCommandDeleteTrack := TDeleteTrackCommand.Create(GAudioStruct.ObjectID);
    try
      for lTrackIndex := 0 to Pred(Tracks.Count) do
      begin
        if Assigned(Tracks[lTrackIndex]) then
        begin
          if TTrackGUI(Tracks[lTrackIndex]).Selected then
          begin
            lCommandDeleteTrack.ObjectIdList.Add(TTrackGUI(GSettings.SelectedTrackGUI).ObjectID);
          end;
        end;
      end;
      GCommandQueue.PushCommand(lCommandDeleteTrack);
    except
      lCommandDeleteTrack.Free;
    end;
  end;
End;

procedure TMainApp.ShuffleProc(Sender: Tobject);
begin
  ArrangeShuffleObjects;
end;

procedure TMainApp.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  //
end;

procedure TMainApp.TreeView1Collapsed(Sender: TObject; Node: TTreeNode);
begin
  Node.ImageIndex := 17;
end;

procedure TMainApp.TreeView1Deletion(Sender: TObject; Node: TTreeNode);
var
  TreeFolderData: TTreeFolderData;
begin
  TreeFolderData := TTreeFolderData(Node.Data);
  if TreeFolderData <> nil then
    TreeFolderData.Free;
end;

{
  When dragsource is
    tpattern : save pattern
    ttrack : save track
}
procedure TMainApp.TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  lPattern: TPattern;
  lTrack: TTrack;
  lSavePatternCommand: TSavePatternCommand;
  lSavePatternDialog: TSaveDialog;
begin
  if Source is TWavePatternGUI then
  begin
    lPattern := TWavePattern(TWavePatternGUI(Source).Model);
    if Assigned(lPattern) then
    begin
      {
        create save pattern command
      }
      lSavePatternDialog :=  TSaveDialog.Create(nil);
      try
        lSavePatternDialog.FileName := lPattern.FileName;
        if lSavePatternDialog.Execute then
        begin
          lPattern.FileName := lSavePatternDialog.FileName;

          lSavePatternCommand := TSavePatternCommand.Create(lPattern.ObjectID);

          GCommandQueue.PushCommand(lSavePatternCommand);
        end;
      finally
        lSavePatternDialog.Free;
      end;
    end;
  end
  else if Source is TTrackGUI then
  begin
    lTrack := TTrackGUI(Source).Track;
  end;
end;

procedure TMainApp.TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Source is TWavePatternGUI then
  begin
    Accept := True;
  end
  else if Source is TTrackGUI then
  begin
    Accept := True;
  end
  else
  begin
    Accept := False;
  end;
end;

procedure TMainApp.TreeView1Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  TreeFolderData: TTreeFolderData;
begin
  if TObject(Node.Data) is TTreeFolderData then
  begin
    TreeFolderData := TTreeFolderData(Node.Data);
    if not TreeFolderData.Opened then
    begin
      TreeFolderData.Opened := True;
      AddSubFolders(TreeFolderData.Path, Node);
    end;
    Node.ImageIndex := 17;
  end;
end;

procedure TMainApp.TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    TreeView1.BeginDrag(False);
  end;

end;

procedure TMainApp.UpdateTrackControls(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Pred(Tracks.Count) do
  begin

    if Sender <> Tracks[i] then
    begin

    end;
  end;
  Sbtracks.Invalidate;
end;

function TMainApp.TrackExists(AObjectID: string): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to Pred(Tracks.Count) do
  begin
    if TTrackGUI(Tracks[i]).ObjectID = AObjectID then
    begin
      Result := True;
    end;
  end;
end;

function TMainApp.IndexOfTrack(AObjectId: string): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Tracks.Count - 1 do
  begin
    if TTrackGUI(Tracks[i]).ObjectID = AObjectID then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TMainApp.DoTracksRefreshEvent(TrackObject: TObject);
var
  lTrackIndex: Integer;
begin
  if not Assigned(TrackObject) then
  begin
    // TODO Should be more intelligence in here...

    FMidiPatternControlGUI.Parent := nil;
    FWavePatternControlGUI.Parent := nil;
    GSettings.OldSelectedPatternGUI := nil;
    GSettings.SelectedPatternGUI := nil;
  end
  else if TrackObject is TTrackGUI then
  begin
    GSettings.SelectedTrackGUI := TrackObject;
    for lTrackIndex := 0 to Pred(Tracks.Count) do
    begin
      if ssCtrl in GSettings.Modifier then
        TTrackGUI(Tracks[lTrackIndex]).Selected := not TTrackGUI(Tracks[lTrackIndex]).Selected
      else
        TTrackGUI(Tracks[lTrackIndex]).Selected := (TrackObject = Tracks[lTrackIndex]);
    end;

    if GSettings.OldSelectedTrackGUI <> GSettings.SelectedTrackGUI then
    begin
      GSettings.OldSelectedTrackGUI := GSettings.SelectedTrackGUI;
    end;
  end;
end;

procedure TMainApp.DoPatternRefreshEvent(TrackObject: TObject);
var
  lWavePattern: TWavePattern;
  lMidiPattern: TMidiPattern;
begin
  if not Assigned(TrackObject) then
  begin
    // TODO Should be more intelligence in here...

    FMidiPatternControlGUI.Parent := nil;
    FWavePatternControlGUI.Parent := nil;
    GSettings.OldSelectedPatternGUI := nil;
    GSettings.SelectedPatternGUI := nil;
  end
  else if TrackObject is TWavePatternGUI then
  begin
    if GSettings.OldSelectedPatternGUI <> GSettings.SelectedPatternGUI then
    begin
      // Detach if old pattern is visible
      if Assigned(GSettings.OldSelectedPatternGUI) then
      begin
        if GSettings.OldSelectedPatternGUI is TMidiPatternGUI then
        begin
          // Last selected pattern of different type;
          // - detach
          // - set parent to new pattern type
          lMidiPattern := TMidiPattern(GObjectMapper.GetModelObject(TMidiPatternGUI(GSettings.OldSelectedPatternGUI).ObjectID));
          if Assigned(lMidiPattern) then
          begin
            FMidiPatternControlGUI.Disconnect;
            lMidiPattern.Detach(FMidiPatternControlGUI);
          end;

          FMidiPatternControlGUI.Parent := nil;
          FWavePatternControlGUI.Align := alClient;
          FWavePatternControlGUI.Parent := pnlBottom;

          // Attach new pattern
          lWavePattern := TWavePattern(GObjectMapper.GetModelObject(TWavePatternGUI(GSettings.SelectedPatternGUI).ObjectID));
          if Assigned(lWavePattern) then
          begin
            lWavePattern.Attach(FWavePatternControlGUI);
            FWavePatternControlGUI.Connect;
          end;
        end
        else
        begin
          // Last selected pattern of same type; just detach
          lWavePattern := TWavePattern(GObjectMapper.GetModelObject(TWavePatternGUI(GSettings.OldSelectedPatternGUI).ObjectID));
          if Assigned(lWavePattern) then
          begin
            FWavePatternControlGUI.Disconnect;
            lWavePattern.Detach(FWavePatternControlGUI);
          end;

          // Attach new pattern
          lWavePattern := TWavePattern(GObjectMapper.GetModelObject(TWavePatternGUI(GSettings.SelectedPatternGUI).ObjectID));
          if Assigned(lWavePattern) then
          begin
            lWavePattern.Attach(FWavePatternControlGUI);
            FWavePatternControlGUI.Connect;
          end;
        end;
      end
      else
      begin
        if Assigned(GSettings.SelectedPatternGUI) then
        begin
          FWavePatternControlGUI.Align := alClient;
          FWavePatternControlGUI.Parent := pnlBottom;

          // Attach new pattern
          lWavePattern := TWavePattern(GObjectMapper.GetModelObject(TWavePatternGUI(GSettings.SelectedPatternGUI).ObjectID));
          if Assigned(lWavePattern) then
          begin
            lWavePattern.Attach(FWavePatternControlGUI);
            FWavePatternControlGUI.Connect;
          end;
        end
        else
        begin
          FWavePatternControlGUI.Parent := nil;
        end;
      end;

      GSettings.OldSelectedPatternGUI := GSettings.SelectedPatternGUI;
    end;
  end
  else if TrackObject is TMidiPatternGUI then
  begin
    if GSettings.OldSelectedPatternGUI <> GSettings.SelectedPatternGUI then
    begin
      // Detach if old pattern is visible
      if Assigned(GSettings.OldSelectedPatternGUI) then
      begin
        if GSettings.OldSelectedPatternGUI is TWavePatternGUI then
        begin
          // Last selected pattern of different type;
          // - detach
          // - set parent to new pattern type
          lWavePattern := TWavePattern(GObjectMapper.GetModelObject(TWavePatternGUI(GSettings.OldSelectedPatternGUI).ObjectID));
          if Assigned(lWavePattern) then
          begin
            FWavePatternControlGUI.Disconnect;
            lWavePattern.Detach(FWavePatternControlGUI);
          end;

          FWavePatternControlGUI.Parent := nil;
          FMidiPatternControlGUI.Align := alClient;
          FMidiPatternControlGUI.Parent := pnlBottom;

          lMidiPattern := TMidiPattern(GObjectMapper.GetModelObject(TMidiPatternGUI(GSettings.SelectedPatternGUI).ObjectID));
          if Assigned(lMidiPattern) then
          begin
            lMidiPattern.Attach(FMidiPatternControlGUI);
            FMidiPatternControlGUI.Connect;
          end;
        end
        else
        begin
          // Last selected pattern of same type; just detach
          lMidiPattern := TMidiPattern(GObjectMapper.GetModelObject(TMidiPatternGUI(GSettings.OldSelectedPatternGUI).ObjectID));
          if Assigned(lMidiPattern) then
          begin
            FMidiPatternControlGUI.Disconnect;
            lMidiPattern.Detach(FMidiPatternControlGUI);
          end;

          lMidiPattern := TMidiPattern(GObjectMapper.GetModelObject(TMidiPatternGUI(GSettings.SelectedPatternGUI).ObjectID));
          if Assigned(lMidiPattern) then
          begin
            lMidiPattern.Attach(FMidiPatternControlGUI);
            FMidiPatternControlGUI.Connect;
          end;
        end;
      end
      else
      begin
        if Assigned(GSettings.SelectedPatternGUI) then
        begin
          FMidiPatternControlGUI.Align := alClient;
          FMidiPatternControlGUI.Parent := pnlBottom;

          lMidiPattern := TMidiPattern(GObjectMapper.GetModelObject(TMidiPatternGUI(GSettings.SelectedPatternGUI).ObjectID));
          if Assigned(lMidiPattern) then
          begin
            lMidiPattern.Attach(FMidiPatternControlGUI);
            FMidiPatternControlGUI.Connect;
          end;
        end
        else
        begin
          FMidiPatternControlGUI.Parent := nil;
        end;
      end;

      GSettings.OldSelectedPatternGUI := GSettings.SelectedPatternGUI;
    end;
  end;
end;

procedure TMainApp.UpdateTracks(TrackObject: TTrack);
var
  i, j: Integer;
begin
  for i := 0 to Pred(GAudioStruct.Tracks.Count) do
  begin
    if Assigned(GAudioStruct.Tracks.Items[i]) then
    begin
      if TrackObject = TTrack(GAudioStruct.Tracks.Items[i]) then
      begin
        TTrack(GAudioStruct.Tracks.Items[i]).Selected:= True;
      end
      else
      begin
        TTrack(GAudioStruct.Tracks.Items[i]).Selected:= False;
      end;

      for j := 0 to Pred(TTrack(GAudioStruct.Tracks.Items[i]).PatternList.Count) do
      begin
//         TWavePattern(TTrack(GAudioStruct.Tracks.Items[i]).PatternList[j]).Repaint;
      end;
    end;
  end;
  for i := 0 to Pred(GAudioStruct.Tracks.Count) do
  begin
    TTrackGUI(Tracks.Items[i]).Repaint;
  end;
end;

procedure TMainApp.DeleteShuffleByObject(TrackObject: TObject);
var
  i: Integer;
begin
  for i:= 0 to Pred(FShuffleList.Count) do
  begin
    if TrackObject = TShuffle(FShuffleList.Items[i]).trackobject then
    begin
      FShuffleList.Delete(i);
      break;
    end;
  end;
end;

function TMainApp.ShuffleByObject(TrackObject: TObject): TShuffle;
var
  i: Integer;
begin
  Result := nil;
  for i:= 0 to Pred(FShuffleList.Count) do
  begin
    if TrackObject = TShuffle(FShuffleList.Items[i]).trackobject then
    begin
      Result := TShuffle(FShuffleList.Items[i]);
      break;
    end;
  end;
end;

procedure TMainApp.ArrangeShuffleObjects;
var
  i: Integer;
  lDiff: Single;
  Location: Integer;
  lLeftOffset: Integer;
  lTrack: TTrackGUI;
begin

  if Assigned(GSettings.SelectedTrackGUI) then
  begin
    FShuffleList.Clear;
    for i:= 0 to Pred(MainApp.Tracks.Count) do
    begin
      FShuffleList.Add(MainApp.Tracks[i]);
    end;
    FShuffleList.Sort(@compareByLocation);
    FShuffleList.Pack;
    lLeftOffset:= 0;

    // Target Locations
    for i := 0 to Pred(FShuffleList.Count) do
    begin
      lTrack := TTrackGUI(FShuffleList[i]);

      if TTrackGUI(GSettings.SelectedTrackGUI) <> lTrack then
        lTrack.Shuffle.x := lLeftOffset
      else
        if not TTrackGUI(GSettings.SelectedTrackGUI).IsShuffling then
          lTrack.Shuffle.x := lLeftOffset;

      Inc(lLeftOffset, lTrack.Width);
    end;

    // Intermediate floating locations
    for i := 0 to Pred(FShuffleList.Count) do
    begin
      lTrack := TTrackGUI(FShuffleList[i]);

      if TTrackGUI(GSettings.SelectedTrackGUI) <> lTrack then
      begin
        if lTrack.Shuffle.step >= 10 then
          lTrack.Shuffle.oldx := lTrack.Shuffle.x;

        lDiff:= lTrack.Shuffle.x - lTrack.Shuffle.oldx;

        // End reached!
        if lDiff = 0 then
        begin
          lTrack.Shuffle.step:= 0;
          lTrack.Shuffle.oldx:= lTrack.Shuffle.x;
          Location:= lTrack.Shuffle.x;
        end
        else
        begin
          Location:= lTrack.Shuffle.oldx + Round(lTrack.Shuffle.step * (lDiff / 10));
        end;

        if lTrack.Left <> Location then
          lTrack.Left:= Location;

        Inc(lTrack.Shuffle.step, 2);
      end
      else
      begin
        if not TTrackGUI(GSettings.SelectedTrackGUI).IsShuffling then
        begin
          if lTrack.Left <> lTrack.Shuffle.x then
            lTrack.Left:= lTrack.Shuffle.x;
        end;
      end;
    end;
  end;
end;

function TMainApp.CreateTrack(AFileLocation: string; ATrackType: Integer = 0): TTrackGUI;
var
  lCreateTrack: TCreateTrackCommand;
begin
  lCreateTrack := TCreateTrackCommand.Create(GAudioStruct.ObjectID);
  try
    case ATrackType of
      0:
      begin
        lCreateTrack.SourceType := fsWave;
        lCreateTrack.SourceLocation := AFileLocation;
        lCreateTrack.PatternName := ExtractFileNameWithoutExt(AFileLocation);
      end;
      1:
      begin
        lCreateTrack.SourceType := fsMIDI;
        lCreateTrack.SourceLocation := AFileLocation;
        lCreateTrack.PatternName := ExtractFileNameWithoutExt(AFileLocation);
      end;
      2:
      begin
        lCreateTrack.SourceType := fsEmpty;
      end;
    end;

    GCommandQueue.PushCommand(lCreateTrack);
  except
    on e: Exception do
    begin
      DBLog('HybridError: ' + e.Message);
      lCreateTrack.Free;
    end;
  end;
end;

procedure TMainApp.ReleaseTrack(Data: PtrInt);
var
  lTrackGUI: TTrackGUI;
begin

  //also like this but for patterns

  lTrackGUI := TTrackGUI(Data);
  lTrackGUI.Parent := nil;
  Tracks.Remove(lTrackGUI);

  DoTracksRefreshEvent(nil);
end;

procedure TMainApp.CreateTrackGUI(AObjectID: string);
var
  lTrackGUI: TTrackGUI;
  lTrack: TTrack;
  lTrackTotalWidth: Integer;
  lTrackIndex: Integer;
begin
  DBLog('start TMainApp.CreateTrackGUI: ' + AObjectID);

  lTrack := TTrack(GObjectMapper.GetModelObject(AObjectID));

  // Create track with remote ObjectID
  lTrackGUI := TTrackGUI.Create(nil);
  lTrackGUI.Parent := MainApp.Sbtracks;
  lTrackGUI.Height := Sbtracks.Height;
  lTrackGUI.OnUpdateTrackControls := @UpdateTrackControls;
  lTrackGUI.OnTracksRefreshGUI := @DoTracksRefreshEvent;
  lTrackGUI.OnPatternRefreshGUI := @DoPatternRefreshEvent;
  case lTrack.TrackType of
  ttNormal: lTrackGUI.Align := alNone;
  ttMaster: lTrackGUI.Align := alRight;
  ttGroup: lTrackGUI.Align := alNone;
  end;

  lTrackGUI.Track := lTrack;
  Tracks.Add(lTrackGUI);

  lTrack.Attach(lTrackGUI);

  // Calculate x, y for track inside sbTracks
  lTrackTotalWidth := 0;
  for lTrackIndex := 0 to Pred(Tracks.Count) do
    Inc(lTrackTotalWidth, TTrackGUI(Tracks.Items[lTrackIndex]).Width);

  lTrackGUI.Left:= lTrackTotalWidth - lTrackGUI.Width;

  lTrackGUI.Shuffle.trackobject := lTrackGUI;
  lTrackGUI.Shuffle.x := lTrackGUI.Left;
  lTrackGUI.Shuffle.oldx:= lTrackGUI.Shuffle.x;
  lTrackGUI.Shuffle.step:= 0;
  GSettings.SelectedTrackGUI := lTrackGUI;

  for lTrackIndex := 0 to Pred(Tracks.Count) do
  begin
    if Tracks.Items[lTrackIndex] <> lTrackGUI then
      TTrackGUI(Tracks.Items[lTrackIndex]).Selected:= False;
  end;

  DBLog('end TMainApp.CreateTrackGUI ' + lTrackGUI.ObjectID);
end;


procedure TMainApp.DeleteTrackGUI(AObjectID: string);
var
  lIndex: Integer;
  lTrackGUI: TTrackGUI;
begin
  DBLog('start TMainApp.DeleteTrackGUI');

  for lIndex := Pred(Tracks.Count) downto 0 do
  begin
    lTrackGUI := TTrackGUI(Tracks[lIndex]);
    if lTrackGUI.ObjectID = AObjectID then
    begin
      GSettings.SelectedPatternGUI := nil;

      Application.QueueAsyncCall(@ReleaseTrack, PtrInt(lTrackGUI));
    end;
  end;

  DBLog('end TMainApp.DeleteTrackGUI');
end;

function TMainApp.HasSubFolder(const Directory: string): Boolean;
var
  SearchRec: TSearchRec;
  Attributes: Integer;
begin
  // Do a peek to see if the given folder has at least one subfolder
  Result := False;
  Attributes := faAnyFile;
  if FindFirst(IncludeTrailingPathDelimiter(Directory) + '*',
    Attributes, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) > 0 then
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          Result := True;
          Break;
        end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

procedure TMainApp.LoadTreeDirectory;
var
  RootNode: TTreeNode;
  lFilterNode: TTreeNode;
  TreeFolderData: TTreeFolderData;
begin
  TreeView1.Items.BeginUpdate;
  try
    TreeView1.Items.Clear;
    TreeView1.SortType := stText;
    RootNode := TreeView1.Items.Add(nil, 'Root');
    RootNode.ImageIndex := 17;
    TreeFolderData := TTreeFolderData.Create(PathDelim);
    TreeFolderData.Opened := True;
    RootNode.Data := TreeFolderData;

    // Add FileTree
    AddSubFolders(ExtractFilePath(Application.ExeName), RootNode);

    // Add plugins
    lFilterNode := TreeView1.Items.Add(RootNode, 'Plugins');
    lFilterNode.ImageIndex := 17;
    lFilterNode := TreeView1.Items.AddChild(lFilterNode, 'BitReducer');
    lFilterNode := TreeView1.Items.AddChild(lFilterNode, 'Moog filter');

    // Add presets (TODO should be added to above per plugin)

    RootNode.Expand(False);
  finally
    TreeView1.Items.EndUpdate;
  end;
end;

procedure TMainApp.AddSubFolders(const Directory: string;
  ParentNode: TTreeNode);
var
  SearchRec: TSearchRec;
  Attributes: Integer;
  NewNode: TTreeNode;
  TreeFolderData: TTreeFolderData;
begin
  TreeView1.Items.BeginUpdate;
  try
    // First, delete any subfolders from the parent node.
    if ParentNode <> nil then
      ParentNode.DeleteChildren;

    Attributes := faAnyFile;
    if FindFirst(IncludeTrailingPathDelimiter(Directory) + '*',
      Attributes, SearchRec) = 0 then
    begin
      repeat
        //if (SearchRec.Attr and faDirectory) > 0 then
        begin

          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          begin
            if SameText(ExtractFileExt(SearchRec.Name), '.wav') or
              SameText(ExtractFileExt(SearchRec.Name), '.xml') or
              ((SearchRec.Attr and faDirectory) > 0) then
            begin

              NewNode := TreeView1.Items.AddChild(ParentNode, SearchRec.Name);
              if (SearchRec.Attr and faDirectory) > 0 then
                NewNode.ImageIndex := 17
              else
              begin
                NewNode.ImageIndex := -1;
              end;
              TreeFolderData := TTreeFolderData.Create(
                IncludeTrailingPathDelimiter(Directory) + SearchRec.Name);
              NewNode.Data := TreeFolderData;

              if HasSubFolder(TreeFolderData.Path) then
              begin
                TreeFolderData.HasSubFolders := True;
                // Add a fake child so the + appears
                TreeView1.Items.AddChild(newNode, '').ImageIndex := 17;
              end
            end;
          end;
        end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
  finally
    TreeView1.Items.EndUpdate;
  end;
end;

procedure TMainApp.LoadGlobalSession;
var
  lLoadSession: TLoadSessionCommand;
begin
  lLoadSession := TLoadSessionCommand.Create('');
  try
    GCommandQueue.PushCommand(lLoadSession);
  except
    on e: Exception do
    begin
      DBLog('HybridError: ' + e.Message);
      lLoadSession.Free;
    end;
  end;
end;

procedure TMainApp.SaveGlobalSession;
var
  lSaveSession: TSaveSessionCommand;
begin
  lSaveSession := TSaveSessionCommand.Create('');
  try
    GCommandQueue.PushCommand(lSaveSession);
  except
    on e: Exception do
    begin
      DBLog('HybridError: ' + e.Message);
      lSaveSession.Free;
    end;
  end;;
end;

procedure TMainApp.Update(Subject: THybridPersistentModel);
begin
  DBLog('MainApp.Update');

  DiffLists(
    TAudioStructure(Subject).Tracks,
    Tracks,
    @CreateTrackGUI,
    @DeleteTrackGUI);

  DialControl1.Value := GAudioStruct.BPM;
end;

procedure TMainApp.Connect;
begin
  //
end;

procedure TMainApp.Disconnect;
begin
  //
end;

function TMainApp.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TMainApp.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

function TMainApp.GetObjectOwnerID: string;
begin
  Result := FObjectOwnerID;
end;

procedure TMainApp.SetObjectOwnerID(const AObjectOwnerID: string);
begin
  FObjectOwnerID := AObjectOwnerID;
end;

function TMainApp.GetModel: THybridPersistentModel;
begin
  Result := THybridPersistentModel(FModel);
end;

procedure TMainApp.SetModel(AModel: THybridPersistentModel);
begin
  FModel := TAudioStructure(AModel);
end;

{ TMIDIThread }

{
  This is a method executed in the main thread but called by the midi thread.
  It's function is to update the gui with new/altered midi data etc.
}
procedure TMIDIThread.Updater;
var
  lMidiEvent: TMidiMessage;
  lGenericCommand: TSampleParameterCommand;
  lMidiMap: TMidiMap;
  lMidiMapIndex: Integer;
begin
  while jack_ringbuffer_read_space(FRingBuffer) > 0 do
  begin
    // Pop midi event from queue
    jack_ringbuffer_read(FRingBuffer, @lMidiEvent, SizeOf(TMidiMessage));

    // Encode unique midievent id
    FMidiController := IntToStr(lMidiEvent.DataValue1 * 1000000 + lMidiEvent.DataValue2 * 1000);

    // Get mapping for controller and send an model change
    lMidiMapIndex := GCommandQueue.MidiMappingTable.IndexOf(FMidiController);
    if lMidiMapIndex <> -1 then
    begin
      lMidiMap := TMidiMap(GCommandQueue.MidiMappingTable.Objects[lMidiMapIndex]);
      lGenericCommand := TSampleParameterCommand.Create(lMidiMap.ObjectOwnerID);
      try
        if GSettings.MapToVisible and (GSettings.SelectedPatternGUI is TMidiPatternGUI) then
        begin
          // Mapped to visible pattern
          lGenericCommand.ObjectID := TMidiPatternGUI(GSettings.SelectedPatternGUI).ObjectID;
        end
        else
        begin
          // Mapped to any pattern
          lGenericCommand.ObjectID := lMidiMap.ObjectID;
        end;

        lGenericCommand.Parameter := TSampleParameter(lMidiMap.Parameter);
        lGenericCommand.MidiLearn := False;
        lGenericCommand.Value := lMidiMap.Scale * lMidiEvent.DataValue3;
        lGenericCommand.Persist := False;

        // Send to commandqueue
        GCommandQueue.PushCommand(lGenericCommand);
      except
        lGenericCommand.Free;
      end;
    end;

    // Now dispose of received object
    lMidiEvent.Free;
  end;
end;

{
  This is a callback event which gets called by the mvc-controller if a command has
  been received that has MidiLearn = True. The controller calls this methods to
  retrieve the last used midicontroller.
}
procedure TMIDIThread.DoMidiCallback(var AMidiController: string);
begin
  AMidiController := FMidiController;
end;

{
  Read at regular intervals the midi ringbuffer containing incoming midi notes
}
procedure TMIDIThread.Execute;
begin
  while (not Terminated) do
  begin
    // Only update at 1000 ms / 40 ms = about 25 fps
    sleep(40);

    Synchronize(@Updater);
  end;
end;

constructor TMIDIThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);

  FBufferSize := 2048;

  GCommandQueue.MidiCallback := @DoMidiCallback;

  FRingBuffer := jack_ringbuffer_create(FBufferSize);
end;

destructor TMIDIThread.Destroy;
begin
  jack_ringbuffer_free(FRingBuffer);

  inherited Destroy;
end;

function TMIDIThread.PushMidiMessage(AJackMidiEvent: pjack_midi_event_t): Boolean;
var
  lMidiMessage: TMidiMessage;
begin
  Result := False;
  if jack_ringbuffer_write_space(FRingBuffer) > SizeOf(TMidiMessage) then
  begin
    lMidiMessage := TMidiMessage.Create(AJackMidiEvent);
    try
      jack_ringbuffer_write(FRingBuffer, @lMidiMessage, SizeOf(TMidiMessage));
      Result := True;
    except
      lMidiMessage.Free;
    end;
  end;
end;

function TMIDIThread.PopMidiMessage(AJackMidiEvent: TMidiMessage): Boolean;
begin
	Result := False;
  jack_ringbuffer_read(FRingBuffer, @AJackMidiEvent, SizeOf(TMidiMessage));
end;

initialization
  {$I simplejack.lrs}
  FCriticalSection := TCriticalSection.Create;

  FLogging := FindCmdLineSwitch('logging', ['/', '-'], True);
  FShowMapping := FindCmdLineSwitch('mapping', ['/', '-'], True);

finalization
  FCriticalSection.Free;
end.

