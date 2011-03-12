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
  StdCtrls, jack, midiport, jacktypes, ExtCtrls, Math, sndfile, waveform, Spin,
  ContNrs, transport, FileCtrl, PairSplitter, Utils, ComCtrls, GlobalConst,
  Menus, ActnList, dialcontrol, bpm, samplergui, SoundTouchObject,
  Laz_XMLStreaming, Laz_DOM, Laz_XMLCfg, SynEdit, SynHighlighterPas,
  SynGutterBase, SynGutterMarks, SynGutterLineNumber, SynGutterChanges,
  SynGutter, SynGutterCodeFolding, rubberband,
  TypInfo, FileUtil, global_command, LCLType, LCLIntf,
  ShellCtrls, Grids, TrackGUI, waveformgui, global, track, pattern,
  audiostructure, midigui, patterngui, mapmonitor, syncobjs, eventlog,
  midi, db, aboutgui, global_scriptactions, plugin, pluginhostgui,
  ringbuffer, lua, optionsgui;

type
  TTestFunction = function (Param1: Double;
                            Data: String): Longint of object;

  TMidiEvent = record
    // No function yet as midi-events are processed as fast as possible
    // all depending on samplerate, midithread priority and rate
    TimeStamp: single;
    // Size of 'Data' buffer as these are the packets delivered by Jack
    Size: Integer;
    Data: ^Byte;
  end;

  TMainApp = class;

  { TMIDIThread }

  TMIDIThread = class(TThread)
  private
    procedure Updater;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean);
  end;


  { TMainApp }
  TMainApp = class(Tform, IObserver)
    acPlay: TAction;
    acStop: TAction;
    acPause: TAction;
    acRedo: TAction;
    acAbout: TAction;
    acNewScriptAction: TAction;
    acDeleteScriptAction: TAction;
    acSaveScriptActionAs: TAction;
    alScriptActions: TActionList;
    acUndo: TAction;
    alGlobalActions: TActionList;
    btnDeleteTrack: TButton;
    btnCompile: TButton;
    btnExecute: TButton;
    btnNewScriptAction: TButton;
    btnDeleteScriptAction: TButton;
    btnSaveScriptActionAs: TButton;
    cbScriptActions: TComboBox;
    edtScriptActionName: TEdit;
    gbScriptControl: TGroupBox;
    gbScriptEditor: TGroupBox;
    LeftSplitter: TCollapseSplitter;
    BottomSplitter: TCollapseSplitter;
    DialControl1: TDialControl;
    gbFX: TGroupBox;
    gbMasterTempo: TGroupBox;
    ilGlobalImages: TImageList;
    MainMenu1: TMainMenu;
    HelpMenu: TMenuItem;
    MenuItem4: TMenuItem;
    pcPattern: TPageControl;
    ScriptMessages: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    LoadMenu: TMenuItem;
    LoadSession: TMenuItem;
    Panel2: TPanel;
    pnlFileManager: TPanel;
    rbMaster: TRadioButton;
    rbMidiSync: TRadioButton;
    rgEditMode: TRadioGroup;
    SaveMenu: TMenuItem;
    ControlMenu: TMenuItem;
    OptionMenu: TMenuItem;
    SaveSession: TMenuItem;
    SaveTrack: TMenuItem;
    sbFXBitRate: TScrollBar;
    sbFXSampleRate: TScrollBar;
    Splitter1: TSplitter;
    Splitter5: TSplitter;
    gbTrackDetail: Tgroupbox;
    Panel1: Tpanel;
    Sbtracks: Tscrollbox;
    stBitrate: TStaticText;
    stSampleRate: TStaticText;
    ScriptEditor: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    ScreenUpdater: TTimer;
    tsSampler: TTabSheet;
    tsFX: TTabSheet;
    tsMonitor: TTabSheet;
    tsScripter: TTabSheet;
    tsPattern: TTabSheet;
    ToolBar1: TToolBar;
    tbPlay: TToolButton;
    tbStop: TToolButton;
    tbPause: TToolButton;
    tbUndo: TToolButton;
    tbRedo: TToolButton;
    TreeView1: TTreeView;
    procedure acAboutExecute(Sender: TObject);
    procedure acDeleteScriptActionExecute(Sender: TObject);
    procedure acNewScriptActionExecute(Sender: TObject);
    procedure acPauseExecute(Sender: TObject);
    procedure acPlayExecute(Sender: TObject);
    procedure acRedoExecute(Sender: TObject);
    procedure acRedoUpdate(Sender: TObject);
    procedure acSaveScriptActionAsExecute(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure acUndoExecute(Sender: TObject);
    procedure acUndoUpdate(Sender: TObject);
    procedure BottomSplitterDblClick(Sender: TObject);
    procedure btnCompileClick(Sender: TObject);
    procedure btnCreateTrackClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
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
    procedure spnMidiChannelChange(Sender: TObject);
    procedure Formdestroy(Sender: Tobject);
    procedure Formcreate(Sender: Tobject);
    procedure Btndeletetrackclick(Sender: Tobject);
    procedure ShuffleProc(Sender: Tobject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Collapsed(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Deletion(Sender: TObject; Node: TTreeNode);
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
    FSamplerGUI: TSamplerGUI;
    FMappingMonitor: TfmMappingMonitor;
    FObjectID: string;
    FObjectOwnerID: string;
    FLowPriorityInterval: Integer;
    FMediumPriorityInterval: Integer;
    FHighPriorityInterval: Integer;

    function TrackExists(AObjectID: string): Boolean;
    function IndexOfTrack(AObjectId: string): Integer;
    procedure DoTracksRefreshEvent(TrackObject: TObject);
    function ShuffleByObject(TrackObject: TObject): TShuffle;
    procedure UpdateTracks(TrackObject: TWaveFormTrack);
    procedure DeleteShuffleByObject(TrackObject: TObject);
    procedure ArrangeShuffleObjects;

    function CreateTrack(AFileLocation: string; ATrackType: Integer = 0): TTrack;
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

    function GetObjectID: string;
    procedure SetObjectID(AObjectID: string);
    property ObjectID: string read GetObjectID write SetObjectID;
    property MappingMonitor: TfmMappingMonitor read FMappingMonitor write FMappingMonitor;
    property ObjectOwnerID: string read FObjectOwnerID write FObjectOwnerID;
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
  CB_TimeBuffer: psingle;
  FShowMapping: Boolean;

  FCriticalSection: TCriticalSection;

implementation

uses
  fx;

function compareByLocation(Item1 : Pointer; Item2 : Pointer) : Integer;
var
  location1, location2 : TShuffle;
begin
  // We start by viewing the object pointers as TShuffle objects
  location1 := TShuffle(TTrack(Item1).Shuffle);
  location2 := TShuffle(TTrack(Item2).Shuffle);

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

function process_midi_buffer(APattern: TPattern; AMidiOutBuf: pointer; AFrames: Integer; ATrack: TWaveFormTrack): Integer;
var
  buffer: ^byte;
  lFrameOffsetLow: Integer;
  lFrameOffsetHigh: Integer;
  lRelativeLocation: Integer;
  lIndex: Integer;
  lMidiData: TMidiData;
begin

  // Only process when not in state change
  if APattern.MidiGrid.Enabled and (APattern.MidiGrid.MidiDataList.Count > 0) then
  begin
    lFrameOffsetLow := ((APattern.MidiGrid.RealCursorPosition div AFrames) * AFrames);
    lFrameOffsetHigh := ((APattern.MidiGrid.RealCursorPosition div AFrames) * AFrames) + AFrames;

    while (APattern.MidiGrid.MidiDataList.CurrentMidiData.Location < lFrameOffsetHigh) and
      (not APattern.MidiGrid.MidiDataList.Eof) do
    begin
      lMidiData := APattern.MidiGrid.MidiDataList.CurrentMidiData;

      if APattern.MidiGrid.RealCursorPosition > lMidiData.Location then
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
  			    buffer[0] := $90 + APattern.MidiChannel;	{ note on }
  			    buffer[1] := lMidiData.DataValue1;
            buffer[2] := lMidiData.DataValue2;		{ velocity }
          end;
          mtNoteOff:
          begin
    				buffer[0] := $80 + APattern.MidiChannel;;	{ note off }
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

      APattern.MidiGrid.MidiDataList.Next;
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
  i, j, k : integer;
  midi_in_buf : pointer;
  midi_out_buf : pointer;
  output_left : ^jack_default_audio_sample_t;
  output_right : ^jack_default_audio_sample_t;
  input : ^jack_default_audio_sample_t;
  midi_event: TMidiEvent;
	in_event : jack_midi_event_t;
  event_index : jack_nframes_t;
  event_count : jack_nframes_t;
  transport_state : jack_transport_state_t;
  lTrack: TWaveFormTrack;
  TempLevel: jack_default_audio_sample_t;
  CalculatedPitch: Single;
  BPMscale: Single;
  GlobalBPMscale: Single;
  psBeginLocation: Integer;
  psEndLocation: Integer;
  psLastScaleValue: Single;
  lFrames: Longint;
  lPlayingPattern: TPattern;
  lAudioPlaying: Boolean;
  lBuffer: PSingle;
  lFramePacket: TFrameData;
  lStartingSliceIndex: Integer;

  // Hermite interpolation
  frac_pos,xm1,x0,x1,x2: single;

  buf_offset: integer;
  buffer_size: Integer;
begin
  if not GAudioStruct.Active then
    exit;

  buffer_size := nframes * SizeOf(Single);

  // Get pgPattern-input and pgAudio-output buffers
  midi_in_buf := jack_port_get_buffer(midi_input_port, nframes);
  midi_out_buf := jack_port_get_buffer(midi_output_port, nframes);
	output_left := jack_port_get_buffer(audio_output_port_left, nframes);
	output_right := jack_port_get_buffer(audio_output_port_right, nframes);
	input := jack_port_get_buffer(audio_input_port, nframes);

  jack_midi_clear_buffer(midi_out_buf);

  // Get number of pending pgPattern-events
	event_count := jack_midi_get_event_count(midi_in_buf, nframes);
	event_index := 0;
 
  // Query BPM from transport
  transport_state := jack_transport_query(client, @transport_pos);

  // Make sure that the main synchronize counter is valid
  if GAudioStruct.MainSyncCounter >= (MaxInt shr 1) then
  begin
    GAudioStruct.MainSyncCounter := (MaxInt shr 1) - GAudioStruct.MainSyncCounter;
  end;

  if event_count > 1 then
  begin
		for i := 0 to event_count - 1 do
			jack_midi_event_get(@in_event, midi_in_buf, i, nframes);
  end;
  
	jack_midi_event_get(@in_event, midi_in_buf, 0, nframes);

  // Silence y'all!
  for i := 0 to Pred(nframes) do
  begin
    output_left[i] := 0;
    output_right[i] := 0;

    // Detect MIDI-events per sample
    // Implement Midi-mapping right here!
		if (in_event.time = i) and (event_index < event_count) then
    begin
			if in_event.buffer^ and $f0 = $90 then
      begin
				// note on
				note := in_event.buffer^ + 1;
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
				// pgPattern clock

        // Calculate average bpm
        GAudioStruct.BPM := (GAudioStruct.BPM + ((samplerate / 24) / (lastsyncposition - sync_counter)) * 60) / 2;
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
      
      if note <> 0 then
        last_note := note;
        
			inc( event_index );
			if event_index < event_count then
				jack_midi_event_get(@in_event, midi_in_buf, event_index, nframes);
    end;

    inc(sync_counter);
  end;

  GlobalBPMscale := GAudioStruct.BPM / 120;

  for j := 0 to Pred(GAudioStruct.Tracks.Count) do
  begin
    lTrack := TWaveFormTrack(GAudioStruct.Tracks.Items[j]);

     // Increment cursor regardless if audible or not
    if Assigned(lTrack.PlayingPattern) then
    begin
      if lTrack.PlayingPattern.WaveForm.RealBPM = 0 then
        lTrack.PlayingPattern.WaveForm.RealBPM := 120;

      BPMscale := GAudioStruct.BPM / NoDivByZero(lTrack.PlayingPattern.WaveForm.RealBPM);
      if BPMscale > 16 then
        BPMscale := 16
      else if BPMscale < 0.1 then
        BPMscale := 0.1;
    end;

    psBeginLocation:= 0;

    if Assigned(lTrack.PlayingPattern) then
    begin
      // Reset buffer at beginning of callback
      lTrack.PlayingPattern.MidiGrid.MidiBuffer.Reset;

      lTrack.PlayingPattern.MidiGrid.BPMScale := GlobalBPMscale;

      if lTrack.Playing then
      begin
        // Send midi pattern to jack buffer
        process_midi_buffer(lTrack.PlayingPattern, midi_out_buf, nframes, lTrack);
      end;
    end;



    for i := 0 to Pred(nframes) do
    begin

      if lTrack.Playing then
      begin
        // Synchronize section
        if (GAudioStruct.MainSyncCounter + i) mod GAudioStruct.MainSyncModula = 0 then
        begin

          if Assigned(lTrack.ScheduledPattern) then
          begin
            if Assigned(lTrack.PlayingPattern) then
            begin
              lTrack.PlayingPattern.Playing := False;
            end;
            lTrack.PlayingPattern := lTrack.ScheduledPattern;
            lTrack.PlayingPattern.WaveForm.TimeStretch.Flush;

            if lTrack.PlayingPattern.MidiGrid.MidiDataList.Count > 0 then
            begin
              lTrack.PlayingPattern.MidiGrid.MidiDataList.First;
              lTrack.PlayingPattern.MidiGrid.MidiDataCursor :=
                TMidiData( lTrack.PlayingPattern.MidiGrid.MidiDataList.Items[0] );
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
            lPlayingPattern.WaveForm.WorkBuffer[i] := 0;

            if i = 0 then
            begin
              psLastScaleValue := lPlayingPattern.WaveForm.CursorRamp;
            end;

            // Synchronize with global quantize track
            if (lPlayingPattern.WaveForm.CursorReal >= lPlayingPattern.WaveForm.LoopEnd.Location) or lPlayingPattern.SyncQuantize then
            begin
              lPlayingPattern.SyncQuantize := False;
              lPlayingPattern.WaveForm.CursorReal := lPlayingPattern.WaveForm.LoopStart.Location;
            end;

            if (lPlayingPattern.MidiGrid.CursorAdder >= lPlayingPattern.MidiGrid.LoopEnd) or lPlayingPattern.SyncQuantize then
            begin
              lPlayingPattern.SyncQuantize := False;
              lPlayingPattern.MidiGrid.CursorAdder := lPlayingPattern.MidiGrid.LoopStart;

              if lPlayingPattern.MidiGrid.MidiDataList.Count > 0 then
              begin
                lPlayingPattern.MidiGrid.MidiDataList.First;
                lPlayingPattern.MidiGrid.MidiDataCursor := TMidiData( lPlayingPattern.MidiGrid.MidiDataList.Items[0] );
              end;
            end;

            // Fetch frame data packet containing warp factor and virtual location in wave data
            if i = 0 then
            begin
              lStartingSliceIndex := lPlayingPattern.WaveForm.StartVirtualLocation(lPlayingPattern.WaveForm.CursorReal);
            end;
            lPlayingPattern.WaveForm.VirtualLocation(lStartingSliceIndex, lPlayingPattern.WaveForm.CursorReal, lFramePacket);
            lPlayingPattern.WaveForm.CursorAdder := lFramePacket.Location;
            lPlayingPattern.WaveForm.CursorRamp := lFramePacket.Ramp * BPMscale;

            if lPlayingPattern.WaveForm.PitchAlgorithm = paPitched then
              lPlayingPattern.WaveForm.CursorRamp := lPlayingPattern.Pitch;

            // Put sound in buffer, interpolate with Hermite4 function
            lBuffer := TChannel(lPlayingPattern.WaveForm.Wave.ChannelList[0]).Buffer;

            frac_pos := Frac(lPlayingPattern.WaveForm.CursorAdder);
            buf_offset := (Trunc(lPlayingPattern.WaveForm.CursorAdder) * lPlayingPattern.WaveForm.Wave.ChannelCount);

            if buf_offset <= 0 then
              xm1 := 0
            else
              xm1 := lBuffer[buf_offset - 1];

            x0 := lBuffer[buf_offset];
            x1 := lBuffer[buf_offset + 1];
            x2 := lBuffer[buf_offset + 2];
            lPlayingPattern.WaveForm.WorkBuffer[i] := hermite4(frac_pos, xm1, x0, x1, x2);

            if lTrack.Active then
            begin
              // Scale buffer up to now when the scaling-factor has changed or
              // the end of the buffer has been reached.
              if lPlayingPattern.WaveForm.PitchAlgorithm <> paNone then
              begin
                if (lPlayingPattern.WaveForm.CursorRamp <> psLastScaleValue) or (i = (nframes - 1)) then
                begin
                  psEndLocation := i;
                  CalculatedPitch := lPlayingPattern.Pitch * (1 / NoDivByZero(lPlayingPattern.WaveForm.CursorRamp));
                  if CalculatedPitch < 0.062 then CalculatedPitch := 0.062;
                  if CalculatedPitch > 16 then CalculatedPitch := 16;

                  // Frames to process this window
                  lFrames := (psEndLocation - psBeginLocation) + 1;

                  for k := 0 to Pred(lFrames) do
                    CB_TimeBuffer[k] := lPlayingPattern.WaveForm.WorkBuffer[psBeginLocation + k];

                  case lPlayingPattern.WaveForm.PitchAlgorithm of
                    paST:
                    begin
                      lPlayingPattern.WaveForm.TimeStretch.Pitch := CalculatedPitch;
                      lPlayingPattern.WaveForm.TimeStretch.PutSamples(CB_TimeBuffer, lFrames);
                      lPlayingPattern.WaveForm.TimeStretch.ReceiveSamples(CB_TimeBuffer, lFrames);
                    end;
                    paMultiST:
                    begin
                      lPlayingPattern.WaveForm.WSOLA.Pitch := CalculatedPitch;
                      lPlayingPattern.WaveForm.WSOLA.Process(CB_TimeBuffer, CB_TimeBuffer, lFrames);
                    end;
                    paRubberband:
                    begin
                      {rubberband_set_pitch_scale(lPlayingPattern.WaveForm.TimePitchScale, CalculatedPitch);
                      rubberband_process(lPlayingPattern.WaveForm.TimePitchScale, @CB_TimeBuffer, lFrames, 0);
                      rubberband_retrieve(lPlayingPattern.WaveForm.TimePitchScale, @CB_TimeBuffer, lFrames);}
                    end;
                  end;

                  for k := 0 to Pred(lFrames) do
                    lPlayingPattern.WaveForm.BufferData2[psBeginLocation + k] := CB_TimeBuffer[k];

                  // Remember last change
                  psBeginLocation:= i;
                  psLastScaleValue:= lPlayingPattern.WaveForm.CursorRamp;
                end;
              end
              else
              begin
                lPlayingPattern.WaveForm.BufferData2[i] := lPlayingPattern.WaveForm.WorkBuffer[i];
              end;
            end;

// debug; clear waveform buffer to listen to midi plugin
lPlayingPattern.WaveForm.BufferData2[i] := 0;

            // Fill MidiBuffer with midi data if found
            while (lPlayingPattern.MidiGrid.CursorAdder >= lPlayingPattern.MidiGrid.MidiDataCursor.Location) do
            begin
              // Put event in buffer
              lPlayingPattern.MidiGrid.MidiBuffer.WriteEvent(lPlayingPattern.MidiGrid.MidiDataCursor, i);

              if Assigned(lPlayingPattern.MidiGrid.MidiDataCursor.Next) then
              begin
                lPlayingPattern.MidiGrid.MidiDataCursor :=
                  lPlayingPattern.MidiGrid.MidiDataCursor.Next
              end
              else
              begin
                break;
              end;
            end;

            // Advance cursors for midi and tsPattern
            lPlayingPattern.WaveForm.RealCursorPosition := Round(lPlayingPattern.WaveForm.CursorReal);
            lPlayingPattern.WaveForm.CursorReal := lPlayingPattern.WaveForm.CursorReal + BPMscale;

            lPlayingPattern.MidiGrid.RealCursorPosition := Round(lPlayingPattern.MidiGrid.CursorAdder);
            lPlayingPattern.MidiGrid.CursorAdder := lPlayingPattern.MidiGrid.CursorAdder + GlobalBPMscale;
          end;
        end;
      end;
    end;
  end;

  for j := 0 to Pred(GAudioStruct.Tracks.Count) do
  begin
    lTrack := TWaveFormTrack(GAudioStruct.Tracks.Items[j]);

    FillByte(lTrack.OutputBuffer[0], buffer_size, 0);

    lPlayingPattern := lTrack.PlayingPattern;
    if Assigned(lPlayingPattern) then
    begin
      if lPlayingPattern.OkToPlay then
      begin

        if lTrack.Playing then
        begin

          if lTrack.Active then
          begin
            lPlayingPattern.WaveForm.DiskWriterThread.RingbufferWrite(input[0], nframes);

            {lPlayingPattern.LocalSampleBank.Process(lPlayingPattern.MidiGrid,
              lTrack.OutputBuffer, nframes); }
            lPlayingPattern.LocalSampleBankEngine.Process(lPlayingPattern.MidiGrid,
              lTrack.OutputBuffer, nframes);

            // 1. Execute per pattern plugins
{            lPlayingPattern.PluginProcessor.Execute(nframes, lPlayingPattern.WaveForm.BufferData2);

            // 2. Execute per track plugins
            lTrack.PluginProcessor.Execute(nframes, lPlayingPattern.PluginProcessor.Buffer);

            // 3. Apply tracksettings (Level, Mute, ...) to track output buffer
            for i := 0 to Pred(nframes) do
            begin
              lTrack.OutputBuffer[i] :=
                (lTrack.PluginProcessor.Buffer[i] + input[i]) * lTrack.VolumeMultiplier * 1.5;
            end;          }

            // Mix to the jack out
            for i := 0 to Pred(nframes) do
            begin
              output_left[i] := output_left[i] + lTrack.OutputBuffer[i];
              output_right[i] := output_right[i] + lTrack.OutputBuffer[i];
            end;

            // Copy to track output (JUST FOR DEBUGGING, use MasterOut )
            {Move(lTrack.OutputBuffer[0], output_left[0], buffer_size);
            Move(lTrack.OutputBuffer[0], output_right[0], buffer_size);}
          end;
        end;
      end;
    end;

    // TODO when switching playing pattern calculate the division count
    for i := 0 to Pred(nframes) do
    begin
      output_left[i] /= GAudioStruct.Tracks.Count;
      output_right[i] /= GAudioStruct.Tracks.Count;
    end;

    for i := 0 to Pred(nframes) do
    begin
      // clamp tsPattern to prevent fpu errors
      if lTrack.OutputBuffer[i] > 1 then lTrack.OutputBuffer[i] := 1;
      if lTrack.OutputBuffer[i] < -1 then lTrack.OutputBuffer[i] := -1;

      TempLevel := Abs(lTrack.OutputBuffer[i] * lTrack.VolumeMultiplier * 1.5);
      if TempLevel > lTrack.Level then
        lTrack.Level := (attack_coef * (lTrack.Level - TempLevel)) + TempLevel
      else
        lTrack.Level := (release_coef * (lTrack.Level - TempLevel)) + TempLevel;
    end;
  end;
  //------- End effects section ----------------------------------------

  {case GSettings.PlayState of
    psPlay:
    begin}
      GAudioStruct.MainSyncCounter := GAudioStruct.MainSyncCounter + nframes;
    {end;
    psStop:;
    psPause:;
  end;}

  // Move to displaybuffer
  if GAudioStruct.Tracks.Count > 0 then
  begin
    lTrack := TWaveFormTrack(GAudioStruct.Tracks.Items[0]);
    if lTrack.Playing then
    begin
      move(output_left[0], buffer_allocate2[0], buffer_size);
    end;
  end;

  Result := 0;
end;


procedure TMainApp.acStopExecute(Sender: TObject);
var
  i: Integer;
begin
  // Stop
  GAudioStruct.PlayState:= psStop;
  for i := 0 to Pred(GAudioStruct.Tracks.Count) do
  begin
    TWaveFormTrack(GAudioStruct.Tracks[i]).Playing:= False;
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
    lCommand:= TCommand(GHistoryQueue[GHistoryIndex]);
    if Assigned(lCommand) then
    begin
      try
        DBLog('Rolling back command class: %s', lCommand.ClassName);
        lCommand.Rollback;

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
          DBLog(Format('Internal error at acUndoExecute: %s, class: %s', [e.Message, lCommand.ClassName]));
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

procedure TMainApp.BottomSplitterDblClick(Sender: TObject);
begin
  if pcPattern.Height < 30 then
    pcPattern.Height := 200
  else
    pcPattern.Height := 0;
end;

procedure TMainApp.btnCompileClick(Sender: TObject);
var
  lLine: Integer;
  lMessage: string;
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


procedure TMainApp.btnExecuteClick(Sender: TObject);
var
  Meth: TTestFunction;
begin
  DBLog('start btnExecuteClick');


  DBLog('end btnExecuteClick');
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
    TWaveFormTrack(GAudioStruct.Tracks[i]).Playing := True;
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
      lCommand.Execute;
      Inc(GHistoryIndex);
    end;
  end;
end;

procedure TMainApp.acRedoUpdate(Sender: TObject);
begin
  tbRedo.Enabled := False;//((GHistoryIndex > -1) and (GHistoryIndex <= GHistoryQueue.Count));
end;

procedure TMainApp.acSaveScriptActionAsExecute(Sender: TObject);
var
  lAction: THybridAction;
begin
  lAction := THybridAction.Create(nil);
  lAction.Script.Text := ScriptEditor.Lines.Text;
  lAction.ActionList := alScriptActions;

  // Add to ComboBox selector
  cbScriptActions.AddItem(lAction.Caption, lAction);
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

procedure TMainApp.acDeleteScriptActionExecute(Sender: TObject);
begin
  //
end;

procedure TMainApp.acNewScriptActionExecute(Sender: TObject);
begin
  //alScriptActions.Actions[]
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
  GSettings.EditMode := rgEditMode.ItemIndex;
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
    TTrack(Tracks.Items[i]).Height := Sbtracks.Height;
  end;
end;

procedure TMainApp.ScreenUpdaterTimer(Sender: TObject);
var
  i, j: Integer;
  lTrack: TWaveFormTrack;
  lPatternGUI: TPatternGUI;
begin
  try
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

    for i := 0 to Pred(MainApp.Tracks.Count) do
    begin
      lTrack := TWaveFormTrack(TTrack(MainApp.Tracks[i]).ModelObject);

      if Assigned(lTrack) then
      begin
        TTrack(MainApp.Tracks[i]).vcLevel.LevelLeft := lTrack.Level;
        TTrack(MainApp.Tracks[i]).vcLevel.LevelRight := lTrack.Level;
        TTrack(MainApp.Tracks[i]).vcLevel.Invalidate;

        for j := 0 to Pred(TTrack(MainApp.Tracks[i]).PatternListGUI.Count) do
        begin
          lPatternGUI := TPatternGUI(TTrack(MainApp.Tracks[i]).PatternListGUI[j]);

          if Assigned(lPatternGUI) and Assigned(lTrack.PlayingPattern) then
          begin
            if lPatternGUI.ObjectID = lTrack.PlayingPattern.ObjectID then
            begin
              lPatternGUI.CursorPosition := lTrack.PlayingPattern.MidiGrid.RealCursorPosition;
              lPatternGUI.CacheIsDirty := True;
            end;
          end;
        end;
      end;

      TTrack(MainApp.Tracks[i]).Invalidate;
    end;

    if Assigned(GSettings.SelectedPatternGUI) then
    begin
      TPatternGUI(GSettings.SelectedPatternGUI).PatternControls.MidiGridGUI.Invalidate;
      TPatternGUI(GSettings.SelectedPatternGUI).PatternControls.WaveFormGUI.Invalidate;
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

    Application.ProcessMessages;

  except
    on e:exception do
    begin
      writeln('Hybrid error: ' + e.Message);
    end;
  end;
end;

procedure TMainApp.spnMidiChannelChange(Sender: TObject);
begin
  // Change pgPattern-channel for track
end;

procedure TMainApp.Formdestroy(Sender: Tobject);
var
   i: Integer;
begin
  ScreenUpdater.Enabled := False;

  sleep(100);
  jack_transport_stop(client);

  sleep(100);
  jack_deactivate(client);

  sleep(100);
	jack_client_close(client);

  sleep(100);

  if Assigned(GAudioStruct) then
  begin
    GAudioStruct.Detach(MainApp);

    for i:= 0 to Pred(GAudioStruct.Tracks.Count) do
      TWaveFormTrack(GAudioStruct.Tracks.Items[i]).Playing := False;
  end;

  GAudioStruct.Sampler.Detach(FSamplerGUI);

  if Assigned(buffer_allocate2) then
    Freemem(buffer_allocate2);

  if Assigned(FSamplerGUI) then
    FSamplerGUI.Free;

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

  FreeMem(CB_TimeBuffer);
End;

procedure TMainApp.Formcreate(Sender: Tobject);
var
  input_port: jack_port_t;
  output_port: jack_port_t;
  input_ports: ppchar;
  output_ports: ppchar;
begin
  Getmem(CB_TimeBuffer, 144100);

  MainApp.DoubleBuffered := True;
  Sbtracks.DoubleBuffered := True;

  Tracks:= TObjectList.create(True);

  LoadTreeDirectory;

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
  GSettings.SampleRate := samplerate;
  GSettings.Frames := jack_get_buffer_size(client);
  GAudioStruct := TAudioStructure.Create('{D6DDECB0-BA12-4448-BBAE-3A96EEC90BFB}', MAPPED);
  GAudioStruct.MainSampleRate := samplerate;
  GAudioStruct.BPM:= 120;

  attack_in_ms := 20;
  release_in_ms := 1000;
  attack_coef := power(0.01, 1.0/( attack_in_ms * GAudioStruct.MainSampleRate * 0.001));
  release_coef := power(0.01, 1.0/( release_in_ms * GAudioStruct.MainSampleRate * 0.001));

	writeln(format('jack_set_process_callback %d', [jack_set_process_callback(client, @process, nil)]));

  jack_on_shutdown(client, @jack_shutdown, nil);

	jack_set_sample_rate_callback(client, @srate, nil);

  note := 0;
  ramp := 0;
  FOutputWaveform:= False;
  
	if jack_activate(client) = 1 then
  begin
		DBLog('cannot activate client');
    halt(1);
  end;

  writeln('start autoconnect');
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
    writeln('no physical playback ports.');
  end
  else
  begin
    if jack_connect(client, jack_port_name(audio_output_port_left), output_ports[0]) <> 0 then
    begin
      writeln('cannot connect output ports');
    end;
    if jack_connect(client, jack_port_name(audio_output_port_right), output_ports[1]) <> 0 then
    begin
      writeln('cannot connect output ports');
    end;
  end;
  writeln('end autoconnect');

  jack_transport_start(client);

  Getmem(buffer_allocate2, 200000 * SizeOf(jack_default_audio_sample_t));

  FShuffleList := TObjectList.create(False);
  FShuffleList.Sort(@compareByLocation);

  {FSimpleWaveForm := TSimpleWaveForm.Create(Self);
  FSimpleWaveForm.Data := buffer_allocate2;
  FSimpleWaveForm.Top := 0;
  FSimpleWaveForm.Left := 0;
  FSimpleWaveForm.Width := pgMonitor.Width;
  FSimpleWaveForm.Align := alClient;
  FSimpleWaveForm.Parent := pgMonitor;}
  
  FSamplerGUI := TSamplerGUI.Create(nil);
  FSamplerGUI.ObjectOwnerID := MainApp.ObjectID;
  FSamplerGUI.ObjectID := GAudioStruct.Sampler.ObjectID;
  GAudioStruct.Sampler.Attach(FSamplerGUI);
  FSamplerGUI.Parent := tsSampler;

  FMappingMonitor := TfmMappingMonitor.Create(Self);
  FMappingMonitor.Maps := GObjectMapper.Maps;
  if FShowMapping then
  begin
    FMappingMonitor.Show;
  end;

  GAudioStruct.Attach(MainApp);
  MainApp.ObjectID := GAudioStruct.ObjectID;

  ChangeControlStyle(Self, [csDisplayDragImage], [], True);

  ScreenUpdater.Interval := 40;
  ScreenUpdater.Enabled := True;
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
          if TTrack(Tracks[lTrackIndex]).Selected then
          begin
            lCommandDeleteTrack.ObjectIdList.Add(TTrack(GSettings.SelectedTrackGUI).ObjectID);
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
// TODO Custom splitter manager
    DBLog(inttostr(i));
      TTrack(Tracks[i]).PanelSends.Height := (Sender as TTrack).PanelSends.Height;

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
    if TTrack(Tracks[i]).ObjectID = AObjectID then
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
    if TTrack(Tracks[i]).ObjectID = AObjectID then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TMainApp.DoTracksRefreshEvent(TrackObject: TObject);
var
  lPatternGUI: TPatternGUI;
  lTrackGUI: TTrack;
  lTrackIndex: Integer;
begin
  if TrackObject is TTrack then
  begin;
    GSettings.SelectedTrackGUI := TrackObject;
    for lTrackIndex := 0 to Pred(Tracks.Count) do
    begin
      if ssCtrl in GSettings.Modifier then
        TTrack(Tracks[lTrackIndex]).Selected := not TTrack(Tracks[lTrackIndex]).Selected
      else
        TTrack(Tracks[lTrackIndex]).Selected := (TrackObject = Tracks[lTrackIndex]);
    end;


    if GSettings.OldSelectedTrackGUI <> GSettings.SelectedTrackGUI then
    begin
      writeln(format('%d, %d', [Integer(GSettings.OldSelectedTrackGUI), Integer(GSettings.SelectedTrackGUI)]));
      GSettings.OldSelectedTrackGUI := GSettings.SelectedTrackGUI;

      lTrackGUI := TTrack(GSettings.SelectedTrackGUI);
      if Assigned(lTrackGUI) then
      begin
        writeln('Assigned(lTrackGUI)');
        lPatternGUI := lTrackGUI.SelectedPattern;
        if Assigned(lPatternGUI) then
        begin
          writeln('Assigned(lPatternGUI)');
          lPatternGUI.PatternControls.Parent := nil;
          lPatternGUI.PatternControls.Parent := tsPattern;
        end
        else
        begin
          if Assigned(lTrackGUI.PatternListGUI) then
          begin
            if lTrackGUI.PatternListGUI.Count > 0 then
            begin
            end;
          end;
        end;
      end;
    end;
  end
  else if TrackObject is TPatternGUI then
  begin
    if GSettings.OldSelectedPatternGUI <> GSettings.SelectedPatternGUI then
    begin
      GSettings.OldSelectedPatternGUI := GSettings.SelectedPatternGUI;

      lPatternGUI := TPatternGUI(GSettings.SelectedPatternGUI);
      if Assigned(lPatternGUI) then
      begin
        lPatternGUI.PatternControls.Parent := nil;
        lPatternGUI.PatternControls.Parent := tsPattern;
      end;
    end;
  end;
end;

procedure TMainApp.UpdateTracks(TrackObject: TWaveFormTrack);
var
  i, j: Integer;
begin
  for i := 0 to Pred(GAudioStruct.Tracks.Count) do
  begin
    if Assigned(GAudioStruct.Tracks.Items[i]) then
    begin
      if TrackObject = TWaveFormTrack(GAudioStruct.Tracks.Items[i]) then
      begin
        TWaveFormTrack(GAudioStruct.Tracks.Items[i]).Selected:= True;
      end
      else
      begin
        TWaveFormTrack(GAudioStruct.Tracks.Items[i]).Selected:= False;
      end;

      for j := 0 to Pred(TWaveFormTrack(GAudioStruct.Tracks.Items[i]).PatternList.Count) do
      begin
//         TWaveForm(TWaveFormTrack(GAudioStruct.Tracks.Items[i]).PatternList[j]).Repaint;
      end;
    end;
  end;
  for i := 0 to Pred(GAudioStruct.Tracks.Count) do
  begin
    TTrack(Tracks.Items[i]).Repaint;
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
  lTrack: TTrack;
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
      lTrack := TTrack(FShuffleList[i]);

      if TTrack(GSettings.SelectedTrackGUI) <> lTrack then
        lTrack.Shuffle.x := lLeftOffset
      else
        if not TTrack(GSettings.SelectedTrackGUI).IsShuffling then
          lTrack.Shuffle.x := lLeftOffset;

      Inc(lLeftOffset, lTrack.Width);
    end;

    // Intermediate floating locations
    for i := 0 to Pred(FShuffleList.Count) do
    begin
      lTrack := TTrack(FShuffleList[i]);

      if TTrack(GSettings.SelectedTrackGUI) <> lTrack then
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
        if not TTrack(GSettings.SelectedTrackGUI).IsShuffling then
        begin
          if lTrack.Left <> lTrack.Shuffle.x then
            lTrack.Left:= lTrack.Shuffle.x;
        end;
      end;
    end;
  end;
end;

function TMainApp.CreateTrack(AFileLocation: string; ATrackType: Integer = 0): TTrack;
var
  lCreateTrack: TCreateTrackCommand;
begin
  lCreateTrack := TCreateTrackCommand.Create(GAudioStruct.ObjectID);
  try
    lCreateTrack.SourceType := fsWave;
    lCreateTrack.SourceLocation := AFileLocation;
    lCreateTrack.PatternName := ExtractFileNameWithoutExt(AFileLocation);

    GCommandQueue.PushCommand(lCreateTrack);
  except
    on e: Exception do
    begin
      DBLog('HybridError: ' + e.Message);
      lCreateTrack.Free;
    end;
  end;
end;

procedure TMainApp.CreateTrackGUI(AObjectID: string);
var
  lTrackGUI: TTrack;
  lTrackTotalWidth: Integer;
  lTrackIndex: Integer;
begin
  DBLog('start TMainApp.CreateTrackGUI: ' + AObjectID);

  // Create track with remote ObjectID
  lTrackGUI := TTrack.Create(nil);
  lTrackGUI.Parent := MainApp.Sbtracks;
  lTrackGUI.OnUpdateTrackControls := @UpdateTrackControls;
  lTrackGUI.OnTracksRefreshGUI := @DoTracksRefreshEvent;

  lTrackGUI.SetObjectID(AObjectID);
  lTrackGUI.ModelObject := GObjectMapper.GetModelObject(AObjectID);
  Tracks.Add(lTrackGUI);

  // Calculate x, y for track inside sbTracks
  lTrackTotalWidth := 0;
  for lTrackIndex := 0 to Pred(Tracks.Count) do
    Inc(lTrackTotalWidth, TTrack(Tracks.Items[lTrackIndex]).Width);

  lTrackGUI.Left:= lTrackTotalWidth - lTrackGUI.Width;

  lTrackGUI.Shuffle.trackobject := lTrackGUI;
  lTrackGUI.Shuffle.x := lTrackGUI.Left;
  lTrackGUI.Shuffle.oldx:= lTrackGUI.Shuffle.x;
  lTrackGUI.Shuffle.step:= 0;
  GSettings.SelectedTrackGUI := lTrackGUI;

  for lTrackIndex := 0 to Pred(Tracks.Count) do
  begin
    if Tracks.Items[lTrackIndex] <> lTrackGUI then
      TTrack(Tracks.Items[lTrackIndex]).Selected:= False;
  end;

  TWaveFormTrack(lTrackGUI.ModelObject).Attach(lTrackGUI);

  MainApp.Sbtracks.Invalidate;

  DBLog('end TMainApp.CreateTrackGUI ' + lTrackGUI.ObjectID);
end;


procedure TMainApp.DeleteTrackGUI(AObjectID: string);
var
  lIndex: Integer;
begin
  DBLog('start TMainApp.DeleteTrackGUI');

  for lIndex := Pred(Tracks.Count) downto 0 do
  begin
    if TTrack(Tracks[lIndex]).ObjectID = AObjectID then
    begin
      GSettings.SelectedPatternGUI := nil;
      Tracks.Remove(Tracks[lIndex]);
    end;
  end;

  MainApp.Sbtracks.Invalidate;

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
    AddSubFolders('/home/robbert/dev/sources/jack/'{PathDelim}, RootNode);

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
            if SameText(ExtractFileExt(SearchRec.Name), '.wav') or ((SearchRec.Attr and faDirectory) > 0) then
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

function TMainApp.GetObjectID: string;
begin
  Result := FObjectID;
end;

procedure TMainApp.SetObjectID(AObjectID: string);
begin
  FObjectID := AObjectID;
end;

{ TMIDIThread }

{
  This is a method executed in the main thread but called by the midi thread.
  It's function is to update the gui with new/altered midi data etc.
}
procedure TMIDIThread.Updater;
begin
  //dblog('midi thread: '+char(random(100)));
end;

{
  Read at regular intervals the midi ringbuffer containing incoming midi notes
}
procedure TMIDIThread.Execute;
begin
  while (not Terminated) do
  begin
    // Only update at 1000 ms / 100 ms = about 10 fps
    Sleep(100);
    Synchronize(@Updater);
  end;
end;

constructor TMIDIThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
end;

initialization
  {$I simplejack.lrs}
  FCriticalSection := TCriticalSection.Create;

  FLogging := FindCmdLineSwitch('logging', ['/', '-'], True);
  FShowMapping := FindCmdLineSwitch('mapping', ['/', '-'], True);

finalization
  FCriticalSection.Free;
end.

