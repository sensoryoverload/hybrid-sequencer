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
  ShellCtrls, Grids, wavegui, global, track, pattern,
  audiostructure, midigui, mapmonitor, syncobjs, eventlog,
  midi, db, aboutgui, global_scriptactions, plugin, pluginhostgui,
  ringbuffer, optionsgui, wavepatterngui, midipatterngui, patterngui, sampler,
  sessiongrid, patternview, ladspaloader;

const
  DIVIDE_BY_120_MULTIPLIER = 1 / 120;
  DIV_BY_24 = 1 /24;
  STEREO = 2;
  MONO = 1;

type
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
    acSaveLiveSetAs: TAction;
    acSaveLiveSet: TAction;
    acOpenLiveSet: TAction;
    acExit: TAction;
    acCloseLiveSet: TAction;
    acOptions: TAction;
    acUndo: TAction;
    alGlobalActions: TActionList;
    LeftSplitter: TCollapseSplitter;
    BottomSplitter: TCollapseSplitter;
    DialControl1: TDialControl;
    ilGlobalImages: TImageList;
    MainMenu1: TMainMenu;
    HelpMenu: TMenuItem;
    miOptionsDialog: TMenuItem;
    miCloseLiveSet: TMenuItem;
    miView: TMenuItem;
    miExit: TMenuItem;
    miSaveLiveSetAs: TMenuItem;
    miFile: TMenuItem;
    miOpenLiveSet: TMenuItem;
    miSaveLiveSet: TMenuItem;
    miCreateTrack: TMenuItem;
    pcCPU_Load: TParameterControl;
    pnlToolbar: TPanel;
    pnlVarious: TPanel;
    pnlBottom: TPanel;
    pupScrollBox: TPopupMenu;
    SaveDialog1: TSaveDialog;
    miAbout: TMenuItem;
    miTools: TMenuItem;
    miOptions: TMenuItem;
    miEdit: TMenuItem;
    pnlTop: Tpanel;
    ScreenUpdater: TTimer;
    ScrollBar1: TScrollBar;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    tbPlay: TToolButton;
    tbStop: TToolButton;
    tbPause: TToolButton;
    tbUndo: TToolButton;
    tbRedo: TToolButton;
    TreeView1: TTreeView;
    procedure acAboutExecute(Sender: TObject);
    procedure acAboutUpdate(Sender: TObject);
    procedure acCloseLiveSetExecute(Sender: TObject);
    procedure acCloseLiveSetUpdate(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acExitUpdate(Sender: TObject);
    procedure acOpenLiveSetExecute(Sender: TObject);
    procedure acOpenLiveSetUpdate(Sender: TObject);
    procedure acOptionsExecute(Sender: TObject);
    procedure acOptionsUpdate(Sender: TObject);
    procedure acPauseExecute(Sender: TObject);
    procedure acPlayExecute(Sender: TObject);
    procedure acRedoExecute(Sender: TObject);
    procedure acRedoUpdate(Sender: TObject);
    procedure acSaveLiveSetAsExecute(Sender: TObject);
    procedure acSaveLiveSetAsUpdate(Sender: TObject);
    procedure acSaveLiveSetExecute(Sender: TObject);
    procedure acSaveLiveSetUpdate(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure acUndoExecute(Sender: TObject);
    procedure acUndoUpdate(Sender: TObject);
    procedure DialControl1StartChange(Sender: TObject);
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
    procedure miOpenLiveSetClick(Sender: TObject);
    procedure ScreenUpdaterTimer(Sender: TObject);
    procedure Formdestroy(Sender: Tobject);
    procedure Formcreate(Sender: Tobject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure TreeView1AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
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
  private
    { private Declarations }
    FUpdateSubject: THybridPersistentModel;
    FIsDirty: Boolean;
    FSessionGrid: TSessionGrid;
    FPatternView: TPatternView;
    FSimpleWaveForm: TSimpleWaveForm;
    FOutputWaveform: Boolean;
    FMappingMonitor: TfmMappingMonitor;
    FObjectID: string;
    FObjectOwnerID: string;
    FLowPriorityInterval: Integer;
    FMediumPriorityInterval: Integer;
    FHighPriorityInterval: Integer;
    FNoJackMode: Boolean;
    FModel: TAudioStructure;

    procedure CustomExceptionHandler(Sender: TObject; E: Exception);
    procedure UpdateTracks(TrackObject: TTrack);

    function HasSubFolder(const Directory: string): Boolean;
    procedure LoadTreeDirectory;
    procedure AddSubFolders(const Directory: string;
      ParentNode: TTreeNode);

    procedure LoadGlobalSession(ALiveSetName: string);
    procedure SaveGlobalSession(ALiveSetName: string);
    procedure ClearGlobalSession;
  protected
  public
    { public Declarations }
    procedure Update(Subject: THybridPersistentModel); reintroduce;
    procedure UpdateView(AForceRedraw: Boolean = False);
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
  new_fmod: single = 0;
  last_fmod: single = MaxFloat;

  FCriticalSection: TCriticalSection;

implementation

uses
  fx;

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

{ TMainApp }


function process_midi_buffer(AMidiPattern: TMidiPattern; AMidiOutBuf: pointer; AFrames: Integer; ATrack: TTrack): Integer;
var
  buffer: ^byte;
//  lFrameOffsetLow: Integer;
  lFrameOffsetHigh: Integer;
  lRelativeLocation: Integer;
  lMidiData: TMidiData;
begin

  // Only process when not in state change
  if AMidiPattern.Enabled and (AMidiPattern.MidiDataList.Count > 0) then
  begin
//    lFrameOffsetLow := ((AMidiPattern.RealCursorPosition div AFrames) * AFrames);
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
    				buffer[0] := $80 + AMidiPattern.MidiChannel;	{ note off }
    				buffer[1] := lMidiData.DataValue1;
    				buffer[2] := 0;		{ velocity }
          end;
          mtBankSelect:
          begin

          end;
          mtCC:
          begin
    				buffer[0] := $B0 + AMidiPattern.MidiChannel;	{ cc }
    				buffer[1] := lMidiData.DataValue1;
    				buffer[2] := lMidiData.DataValue2;
          end;
        end;
      end
      else
      begin
        DBLog('jackmidi buffer allocation failed');
      end;

      AMidiPattern.MidiDataList.Next;
    end;
  end;

  Result := 0
end;

function process(nframes: jack_nframes_t; arg:pointer): longint; cdecl;
var
  i, j : integer;
  midi_in_buf : pointer;
  midi_out_buf : pointer;
  output_left : ^jack_default_audio_sample_t;
  output_right : ^jack_default_audio_sample_t;
  input_left : ^jack_default_audio_sample_t;
  input_right : ^jack_default_audio_sample_t;
	in_event : jack_midi_event_t;
  event_index : jack_nframes_t;
  event_count : jack_nframes_t;
  transport_state : jack_transport_state_t;
  lTrack: TTrack;
  lPlayingPattern: TPattern;
  buffer_size: Integer;
  lMidiBuffer: TMidiBuffer;
begin
  Result := 0;

  GAudioStruct.OrderedTrackPriority;

  buffer_size := nframes * SizeOf(Single) * STEREO;

  midi_in_buf := jack_port_get_buffer(GJackAudio.midi_input_port, nframes);
  midi_out_buf := jack_port_get_buffer(GJackAudio.midi_output_port, nframes);
	output_left := jack_port_get_buffer(GJackAudio.audio_output_port_left, nframes);
	output_right := jack_port_get_buffer(GJackAudio.audio_output_port_right, nframes);
	input_left := jack_port_get_buffer(GJackAudio.audio_input_port_left, nframes);
	input_right := jack_port_get_buffer(GJackAudio.audio_input_port_right, nframes);

  // Silence when not active
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
  transport_state := jack_transport_query(GJackAudio.client, @transport_pos);

  // Get number of pending pgPattern-events
	event_count := jack_midi_get_event_count(midi_in_buf, nframes);
	event_index := 0;

	jack_midi_event_get(@in_event, midi_in_buf, 0, nframes);

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

    for j := 0 to Pred(GAudioStruct.Tracks.Count) do
    begin
      lTrack := TTrack(GAudioStruct.Tracks.Items[j]);

      if (i = 0) and Assigned(lTrack.PlayingPattern) then
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

      if lTrack.Playing then
      begin
        // Synchronize section
        if GAudioStruct.Sync then
        begin
          if Assigned(lTrack.ScheduledPattern) then
          begin
            if Assigned(lTrack.PlayingPattern) then
            begin
              lTrack.PlayingPattern.Playing := False;
            end;
            lTrack.PlayingPattern := lTrack.ScheduledPattern;

            lTrack.PlayingPattern.PatternCursor := lTrack.PlayingPattern.LoopStart.Value;

            if lTrack.PlayingPattern is TWavePattern then
            begin
              TWavePattern(lTrack.PlayingPattern).Flush;
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

            if lTrack.ScheduledTo = stStart then
            begin
              lTrack.PlayingPattern.Playing := True;
            end
            else if lTrack.ScheduledTo = stStop then
            begin
              lTrack.PlayingPattern.Playing := False;
            end;

            lTrack.ScheduledTo := stIdle;
            lTrack.PlayingPattern.Scheduled := False;
            lTrack.PlayingPattern.SyncQuantize := True;

            lTrack.ScheduledPattern := nil;
          end;
        end;

        // Play current pattern of current track
        if Assigned(lTrack.PlayingPattern) then
        begin
          lPlayingPattern := lTrack.PlayingPattern;

          if lPlayingPattern.OkToPlay and lPlayingPattern.Playing then
          begin
            lPlayingPattern.Process(lTrack.OutputBuffer, i, nframes);
          end;

          lPlayingPattern.ProcessAdvance;
        end;
      end;
    end;

    // Increment timeline cursor regardless if audible or not
    GAudioStruct.ProcessAdvance;
  end;

  // Clear summing buffers
  for j := 0 to Pred(GAudioStruct.TrackOrder.Count) do
  begin
    lTrack := TTrack(GAudioStruct.TrackOrder.Objects[j]);
    if Assigned(lTrack) then
    begin
      if lTrack.TrackType in [ttGroup, ttMaster] then
      begin
        FillByte(lTrack.InputBuffer[0], buffer_size, 0);
        FillByte(lTrack.OutputBuffer[0], buffer_size, 0);
      end;
    end;
  end;

  // Plugin section, runs freely from sequencer timeline
  for j := Pred(GAudioStruct.TrackOrder.Count) downto 0 do
  begin
    lTrack := TTrack(GAudioStruct.TrackOrder.Objects[j]);

    if Assigned(lTrack) then
    begin
      lMidiBuffer := nil;

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
                lPlayingPattern.WavePattern.DiskWriterThread.RingbufferWrite(input_left[0], nframes);
              }
              lPlayingPattern.ProcessAutomation;

              if lPlayingPattern is TMidiPattern then
              begin
                FillByte(lTrack.OutputBuffer[0], buffer_size, 0);

                lMidiBuffer := TMidiPattern(lPlayingPattern).MidiBuffer;

                // Effects chain
                lPlayingPattern.PluginProcessor.Process(
                  lMidiBuffer,
                  lTrack.OutputBuffer,
                  lTrack.OutputBuffer,
                  nframes);
              end
              else
              begin
                // Effects chain
                lPlayingPattern.PluginProcessor.Process(
                  nil,
                  lTrack.OutputBuffer,
                  lTrack.OutputBuffer,
                  nframes);
              end;
            end;
          end;
        end;
      end;

      if lTrack.TrackType = ttGroup then
      begin
        CopyBuffer(lTrack.InputBuffer, lTrack.OutputBuffer, nframes, STEREO);
      end;

      // Mix audio into target audio buffer
      if lTrack.TrackType <> ttMaster then
      begin
        // Track plugin/volume/panning processing
        lTrack.Process(lMidiBuffer, lTrack.OutputBuffer, nframes);

        // Only mix if there is a target configured
        if Assigned(lTrack.TargetTrack) then
        begin
          MixToBuffer(lTrack.OutputBuffer, lTrack.TargetTrack.InputBuffer, nframes, STEREO);
        end;
      end
      else
      begin
        // Master just copies
        CopyBuffer(lTrack.InputBuffer, lTrack.OutputBuffer, nframes, STEREO);

        // Track plugin/volume/panning processing
        lTrack.Process(lMidiBuffer, lTrack.OutputBuffer, nframes);

        // Mix into output 1 (left) and 2 (right)
        if lTrack.TrackType = ttMaster then
        begin
          SplitStereoToDualMono(lTrack.OutputBuffer, output_left, output_right, nframes);
        end;
      end;

      // Flush midi buffer as this the end of the processing line
      if Assigned(lMidiBuffer) then
      begin
        lMidiBuffer.Reset;
      end;
    end;
  end;

  GAudioStruct.Process;

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
  DumpExceptionCallStack(E);
  DumpCallStack;
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
        lWavePattern.CursorAdder := lWavePattern.LoopStart.Value;
      end
      else if TTrack(GAudioStruct.Tracks[i]).PlayingPattern is TMidiPattern then
      begin
        lMidiPattern := TMidiPattern(TTrack(GAudioStruct.Tracks[i]).PlayingPattern);
        lMidiPattern.PatternCursor := lMidiPattern.LoopStart.Value;
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

        // Update views
        Self.UpdateView(True);
        FSessionGrid.UpdateView(True);
        FPatternView.UpdateView(True);

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

procedure TMainApp.acSaveLiveSetAsExecute(Sender: TObject);
var
  lSaveDialog: TSaveDialog;
  lSave: Boolean;
begin
  lSaveDialog := TSaveDialog.Create(nil);
  try
    lSaveDialog.InitialDir := ExtractFilePath(ParamStr(0));
    lSaveDialog.DefaultExt := '.hls';
    lSaveDialog.Filter := 'Hybrid Live Set|*.hls';
    if lSaveDialog.Execute then
    begin
      if FileExists(lSaveDialog.FileName) then
      begin
        lSave := (MessageDlg('', Format('"%s" already exists. Overwrite it?', [lSaveDialog.FileName]), mtConfirmation, mbYesNoCancel, 0) = mrYes);
      end;

      // save
      if lSave then
      begin
        if SameText(ExtractFileNameWithoutExt(lSaveDialog.FileName), lSaveDialog.FileName) then
        begin
          lSaveDialog.FileName := lSaveDialog.FileName + '.hls';
        end;
        GAudioStruct.LiveSetName := lSaveDialog.FileName;

        SaveGlobalSession(lSaveDialog.FileName);
      end;
    end;
  finally
    lSaveDialog.Free;
  end;
end;

procedure TMainApp.acSaveLiveSetAsUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TMainApp.acSaveLiveSetExecute(Sender: TObject);
var
  lSaveDialog: TSaveDialog;
  lSave: Boolean;
begin
  lSave := True;
  if SameText(GAudioStruct.LiveSetName, '') then
  begin
    lSaveDialog := TSaveDialog.Create(nil);
    try
      lSaveDialog.InitialDir := ExtractFilePath(ParamStr(0));
      lSaveDialog.DefaultExt := '.hls';
      lSaveDialog.Filter := 'Hybrid Live Set|*.hls';
      if lSaveDialog.Execute then
      begin
        GAudioStruct.LiveSetName := lSaveDialog.FileName;
        if FileExists(lSaveDialog.FileName) then
        begin
          // replace
          lSave := (MessageDlg('', Format('"%s" already exists. Overwrite it?', [lSaveDialog.FileName]), mtConfirmation, mbYesNoCancel, 0) = mrYes);
        end;
      end;
    finally
      lSaveDialog.Free;
    end;
  end;

  if lSave then
  begin
    SaveGlobalSession(GAudioStruct.LiveSetName);
  end;
end;

procedure TMainApp.acSaveLiveSetUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TMainApp.acPauseExecute(Sender: TObject);
begin
  // Pause
  GAudioStruct.PlayState:= psPause;
end;

procedure TMainApp.acAboutExecute(Sender: TObject);
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

procedure TMainApp.acAboutUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TMainApp.acCloseLiveSetExecute(Sender: TObject);
begin
  ClearGlobalSession;
end;

procedure TMainApp.acCloseLiveSetUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TMainApp.acExitExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TMainApp.acExitUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TMainApp.acOpenLiveSetExecute(Sender: TObject);
var
  lOpenDialog: TOpenDialog;
begin
  lOpenDialog := TOpenDialog.Create(nil);
  try
    lOpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
    lOpenDialog.Filter := 'Hybrid Live Set|*.hls';
    if lOpenDialog.Execute then
    begin
      LoadGlobalSession(lOpenDialog.FileName);
    end;
  finally
    lOpenDialog.Free;
  end;
end;

procedure TMainApp.acOpenLiveSetUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TMainApp.acOptionsExecute(Sender: TObject);
var
  lFmOptions: TfmOptions;
begin

  lFmOptions := TfmOptions.Create(nil);
  try
    lFmOptions.Settings := GSettings;

    if lFmOptions.ShowModal = mrOK then
    begin
      GSettings.Save('config.xml');
      LoadTreeDirectory;
    end;
  finally
    lFmOptions.Free;
  end;

end;

procedure TMainApp.acOptionsUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
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
  if TreeView1.Width < 30 then
    TreeView1.Width := 250
  else
    TreeView1.Width := 0;
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

procedure TMainApp.miOpenLiveSetClick(Sender: TObject);
var
  lOpenDialog: TOpenDialog;
begin
  lOpenDialog := TOpenDialog.Create(nil);
  try
    if lOpenDialog.Execute then
    begin

    end;
  finally
    lOpenDialog.Free;
  end;
end;

procedure TMainApp.ScreenUpdaterTimer(Sender: TObject);
begin
  Application.ProcessMessages;
  try
    acUndoUpdate(Self);
    acRedoUpdate(Self);

    // Update object mapping
    if FShowMapping then
    begin
      if FLowPriorityInterval = 0 then
      begin
        MainApp.MappingMonitor.UpdateGrid;
      end;
    end
    else
    begin
      FSimpleWaveForm.Invalidate;
    end;

    // Display cpu usage in percentages
    if not FNoJackMode then
    begin
      pcCPU_Load.Value := jack_cpu_load(GJackAudio.client);
    end;

    // Update views
    Self.UpdateView;
    FSessionGrid.UpdateView;
    FPatternView.UpdateView;

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
      DBLog('Hybrid error: ' + e.Message);
    end;
  end;
end;

procedure TMainApp.Formdestroy(Sender: Tobject);
var
   i: Integer;
begin
  ScreenUpdater.Enabled := False;

  if Assigned(GAudioStruct) then
  begin
    GAudioStruct.Detach(MainApp);

    for i:= 0 to Pred(GAudioStruct.Tracks.Count) do
      TTrack(GAudioStruct.Tracks.Items[i]).Playing := False;
  end;

  if Assigned(buffer_allocate2) then
    Freemem(buffer_allocate2);

  if Assigned(FSessionGrid) then
    FSessionGrid.Free;

  if Assigned(FMappingMonitor) then
    FMappingMonitor.Free;

  if Assigned(GAudioStruct) then
    GAudioStruct.Free;
End;

procedure TMainApp.Formcreate(Sender: Tobject);
var
  input_ports: ppchar;
  output_ports: ppchar;
  lIndex: Integer;
begin
  DBLog('start TMainApp.FormCreate');

  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);

  Application.OnException := @CustomExceptionHandler;

  FIsDirty := False;

  FNoJackMode := FindCmdLineSwitch('nojack', ['/', '-'], True);

  LoadTreeDirectory;

  GAudioStruct := TAudioStructure.Create('{D6DDECB0-BA12-4448-BBAE-3A96EEC90BFB}', MAPPED);
  GAudioStruct.Initialize;
  GAudioStruct.MainSampleRate := samplerate;
  GAudioStruct.BPM := 120;

  FPatternView := TPatternView.Create(Self);
  FPatternView.Parent := pnlBottom;
  FPatternView.Align := alClient;
  GAudioStruct.Attach(FPatternView);

  FSessionGrid := TSessionGrid.Create(nil);
  FSessionGrid.Parent := pnlTop;
  FSessionGrid.Align := alClient;
  FSessionGrid.OnPatternRefreshGUI := @FPatternView.DoPatternRefreshEvent;
  GAudioStruct.Attach(FSessionGrid);

  if not FNoJackMode then
  begin
    samplerate:= GJackAudio.Samplerate;

    GJackAudio.Process := @process;
    GJackAudio.Initialize;
    GSettings.SampleRate := GJackAudio.Samplerate;;
    GSettings.Frames := GJackAudio.Frames;
  end
  else
  begin
    GSettings.SampleRate := 44100;
    GSettings.Frames := 512;
  end;

  attack_in_ms := 20;
  release_in_ms := 1000;
  attack_coef := power(0.01, 1.0/( attack_in_ms * GAudioStruct.MainSampleRate * 0.001));
  release_coef := power(0.01, 1.0/( release_in_ms * GAudioStruct.MainSampleRate * 0.001));

  note := 0;
  ramp := 0;
  FOutputWaveform:= False;

  Getmem(buffer_allocate2, 200000 * SizeOf(jack_default_audio_sample_t));

  FMappingMonitor := TfmMappingMonitor.Create(Self);
  FMappingMonitor.Maps := GObjectMapper.Maps;
  if FShowMapping then
  begin
    FMappingMonitor.Show;
    FMappingMonitor.Width := pnlVarious.Width;
    FMappingMonitor.Align := alClient;
    FMappingMonitor.Parent := pnlVarious;
  end
  else
  begin
    FSimpleWaveForm := TSimpleWaveForm.Create(Self);
    FSimpleWaveForm.Data := buffer_allocate2;
    FSimpleWaveForm.Top := 0;
    FSimpleWaveForm.Left := 0;
    FSimpleWaveForm.Width := pnlVarious.Width;
    FSimpleWaveForm.Align := alClient;
    FSimpleWaveForm.Parent := pnlVarious;
  end;

  GAudioStruct.Attach(MainApp);

  ScreenUpdater.Interval := 50;
  ScreenUpdater.Enabled := True;

  pnlVarious.Width := 0;

  DBLog('end TMainApp.FormCreate');
End;

procedure TMainApp.ScrollBar1Change(Sender: TObject);
begin
  FSessionGrid.ScrollIndex := ScrollBar1.Position;
end;

procedure TMainApp.TreeView1AdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
begin
  if Odd(Node.Index) then
  begin
    State := [cdsDefault];
  end
  else
  begin
    State := [cdsGrayed];
  end;
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
  end;
end;

procedure TMainApp.TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Source is TWavePatternGUI then
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

procedure TMainApp.UpdateTracks(TrackObject: TTrack);
var
  i: Integer;
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
    end;
  end;
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
  lFilterRootNode: TTreeNode;
  lIndex: Integer;
  TreeFolderData: TTreeFolderData;
  lTreeViewPluginInfo: TTreeViewPluginInfo;
begin
  TreeView1.Items.BeginUpdate;
  try
    TreeView1.Items.Clear;
    TreeView1.SortType := stText;
    RootNode := TreeView1.Items.Add(nil, 'Files');
    RootNode.ImageIndex := 17;
    TreeFolderData := TTreeFolderData.Create(PathDelim);
    TreeFolderData.Opened := True;
    RootNode.Data := TreeFolderData;

    // Add FileTree
    if GSettings.SampleMap = '' then
    begin
      AddSubFolders(ExtractFilePath(Application.ExeName), RootNode);
    end
    else
    begin
      AddSubFolders(GSettings.SampleMap, RootNode);
    end;

    // Add plugins
    lFilterRootNode := TreeView1.Items.Add(RootNode, 'Native Plugins');
    lFilterRootNode.ImageIndex := 17;
    TreeView1.Items.AddChild(lFilterRootNode, 'Reverb');
    TreeView1.Items.AddChild(lFilterRootNode, 'External');
    TreeView1.Items.AddChild(lFilterRootNode, 'Delay');
    TreeView1.Items.AddChild(lFilterRootNode, 'Bassline');
    TreeView1.Items.AddChild(lFilterRootNode, 'Sampler');
    TreeView1.Items.AddChild(lFilterRootNode, 'Distortion');
    TreeView1.Items.AddChild(lFilterRootNode, 'BitReducer');
    TreeView1.Items.AddChild(lFilterRootNode, 'Moog filter');

    lFilterRootNode := TreeView1.Items.Add(RootNode, 'LADSPA Plugins');
    lFilterRootNode.ImageIndex := 17;
    for lIndex := 0 to Pred(GLadspaPluginFactory.PluginList.Count) do
    begin
      lTreeViewPluginInfo := TTreeViewPluginInfo.Create;
      lTreeViewPluginInfo.UniqueId :=
        TLadspaPluginCatalogItem(GLadspaPluginFactory.PluginList.Objects[lIndex]).UniqueId;
      lTreeViewPluginInfo.Caption :=
        TLadspaPluginCatalogItem(GLadspaPluginFactory.PluginList.Objects[lIndex]).Name;

      TreeView1.Items.AddChildObject(lFilterRootNode,
        TLadspaPluginCatalogItem(GLadspaPluginFactory.PluginList.Objects[lIndex]).Name,
        lTreeViewPluginInfo);
    end;

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
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          if SameText(ExtractFileExt(SearchRec.Name), '.wav') or
            SameText(ExtractFileExt(SearchRec.Name), '.xml') or
            ((SearchRec.Attr and faDirectory) > 0) then
          begin

            NewNode := TreeView1.Items.AddChild(ParentNode, SearchRec.Name);
            if (SearchRec.Attr and faDirectory) > 0 then
            begin
              NewNode.ImageIndex := 17
            end
            else if SameText(ExtractFileExt(SearchRec.Name), '.wav') then
            begin
              NewNode.ImageIndex := 19;
            end
            else if SameText(ExtractFileExt(SearchRec.Name), '.xml') then
            begin
              NewNode.ImageIndex := 20;
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
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
  finally
    TreeView1.Items.EndUpdate;
  end;
end;

procedure TMainApp.LoadGlobalSession(ALiveSetName: string);
var
  lLoadSession: TLoadSessionCommand;
begin
  lLoadSession := TLoadSessionCommand.Create('');
  try
    lLoadSession.LiveSetName := ALiveSetName;

    GCommandQueue.PushCommand(lLoadSession);
  except
    on e: Exception do
    begin
      DBLog('HybridError: ' + e.Message);
      lLoadSession.Free;
    end;
  end;
end;

procedure TMainApp.SaveGlobalSession(ALiveSetName: string);
var
  lSaveSession: TSaveSessionCommand;
begin
  lSaveSession := TSaveSessionCommand.Create('');
  try
    lSaveSession.LiveSetName := ALiveSetName;

    GCommandQueue.PushCommand(lSaveSession);
  except
    on e: Exception do
    begin
      DBLog('HybridError: ' + e.Message);
      lSaveSession.Free;
    end;
  end;
end;

procedure TMainApp.ClearGlobalSession;
var
  lClearSession: TClearSessionCommand;
begin
  lClearSession := TClearSessionCommand.Create('');
  try
    GCommandQueue.PushCommand(lClearSession);
  except
    on e: Exception do
    begin
      DBLog('HybridError: ' + e.Message);
      lClearSession.Free;
    end;
  end;;
end;

procedure TMainApp.Update(Subject: THybridPersistentModel);
begin
  DBLog('MainApp.Update');

  FUpdateSubject := Subject;
  FIsDirty := True;
end;

procedure TMainApp.UpdateView(AForceRedraw: Boolean = False);
begin
  if AForceRedraw then
  begin
    FIsDirty := True;
  end;

  if FIsDirty and Assigned(FUpdateSubject) then
  begin
    FIsDirty := False;

    DialControl1.Value := GAudioStruct.BPM;
  end;
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
        if GSettings.MapToVisible and (GSettings.SelectedObject is TMidiPatternGUI) then
        begin
          // Mapped to visible pattern
          lGenericCommand.ObjectID := TMidiPatternGUI(GSettings.SelectedObject).ObjectID;
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

