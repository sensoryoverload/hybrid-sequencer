unit plugin_external;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, plugin, global_command, global, globalconst, pluginhost, fx,
  jack, jacktypes, midiport, utils, math;

type
  { TPluginExternal }

  TPluginExternal = class(TPluginNode)
  private
    input_ports: ppchar;
    output_ports: ppchar;

    midi_output_port : ^jack_port_t;
    audio_input_port_left : ^jack_port_t;
    audio_input_port_right : ^jack_port_t;
  public
    constructor Create(AObjectOwnerID: string; AMapped: Boolean = True); override;
    destructor Destroy; override;
    procedure Process(AMidiBuffer: TMidiBuffer; AInputBuffer: PSingle;
      AOutputBuffer: PSingle; AFrames: Integer); override;
    procedure Instantiate; override;
    procedure Activate; override;
    procedure Deactivate; override;
    procedure UpdateParameters; override;
  published
  end;

implementation

{ TPluginExternal }

constructor TPluginExternal.Create(AObjectOwnerID: string; AMapped: Boolean);
begin
  inherited;

  PluginName := 'External';
end;

destructor TPluginExternal.Destroy;
begin
  //

  inherited Destroy;
end;

procedure TPluginExternal.Process(AMidiBuffer: TMidiBuffer;
  AInputBuffer: PSingle; AOutputBuffer: PSingle;
  AFrames: Integer);
var
  lInputLeft: ^jack_default_audio_sample_t;
  lInputRight: ^jack_default_audio_sample_t;
  lMidiOutBuf : pointer;
  buffer: ^byte;
  i: Integer;
  lOffsetL: Integer;
  lOffsetR: Integer;
  lMidiEvent: TMidiEvent;
  lMidiBufferIndex: Integer;
begin
  inherited;

  if not Assigned(AMidiBuffer) then
  begin
    lOffsetL := 0;
    lOffsetR := 1;
    for i := 0 to Pred(AFrames) do
    begin
      AOutputBuffer[lOffsetL] := 0;
      AOutputBuffer[lOffsetR] := 0;
      Inc(lOffsetL, 2);
      Inc(lOffsetR, 2);
    end;
    exit;
  end;

  lInputLeft := jack_port_get_buffer(audio_input_port_left, AFrames);
  lInputRight := jack_port_get_buffer(audio_input_port_right, AFrames);
  lMidiOutBuf := jack_port_get_buffer(midi_output_port, AFrames);

  // This should be called each proces cycle before "jack_midi_event_reserve"
  jack_midi_clear_buffer(lMidiOutBuf);

  if AMidiBuffer.Count > 0 then
  begin
    DBLog(Format('AMidiBuffer.Count %d', [AMidiBuffer.Count]));
  end;
  lOffsetL := 0;
  lOffsetR := 1;
  for i := 0 to Pred(AFrames) do
  begin
    if AMidiBuffer.Count > 0 then
    begin
      AMidiBuffer.Seek(0);
      for lMidiBufferIndex := 0 to Pred(AMidiBuffer.Count) do
      begin
        // Shortcut for current event in buffer
        lMidiEvent := AMidiBuffer.ReadEvent;
        if i = lMidiEvent.RelativeOffset then
        begin
          buffer := jack_midi_event_reserve(lMidiOutBuf, i, 3);
          if Assigned(buffer) then
          begin
            case lMidiEvent.DataType of
              mtNoteOn:
              begin
                DBLog(Format('Note %d Velocity %d MidiChannel %d',
                  [lMidiEvent.DataValue1, lMidiEvent.DataValue2, lMidiEvent.MidiChannel]));

      			    buffer[0] := $90 + lMidiEvent.MidiChannel;	{ note on }
      			    buffer[1] := lMidiEvent.DataValue1;
                buffer[2] := lMidiEvent.DataValue2;		{ velocity }
              end;
              mtNoteOff:
              begin
        				buffer[0] := $80 + lMidiEvent.MidiChannel;	{ note off }
        				buffer[1] := lMidiEvent.DataValue1;
        				buffer[2] := 0;		{ velocity }
              end;
              mtBankSelect:
              begin

              end;
              mtCC:
              begin
        				buffer[0] := $B0 + lMidiEvent.MidiChannel;	{ cc }
        				buffer[1] := lMidiEvent.DataValue1;
        				buffer[2] := lMidiEvent.DataValue2;
              end;
            end;
          end
          else
          begin
            DBLog('jackmidi buffer allocation failed');
          end;
        end;
      end;
    end;

    // Get audio from jack input
    AOutputBuffer[lOffsetL] := lInputLeft[i];
    AOutputBuffer[lOffsetR] := lInputRight[i];
    Inc(lOffsetL, 2);
    Inc(lOffsetR, 2);
  end;
end;

procedure TPluginExternal.Instantiate;
begin
  //
end;

procedure TPluginExternal.Activate;
begin
  inherited Activate;

  // create input
  audio_input_port_left := jack_port_register(GJackAudio.client, 'external_audio_in_left', JACK_DEFAULT_AUDIO_TYPE, Longword(JackPortIsInput), 0);
  if not Assigned(audio_input_port_left) then
  begin
    DBLog('audio_input_port_left not registered');
  end;

  audio_input_port_right := jack_port_register(GJackAudio.client, 'external_audio_in_right', JACK_DEFAULT_AUDIO_TYPE, Longword(JackPortIsInput), 0);
  if not Assigned(audio_input_port_right) then
  begin
    DBLog('audio_input_port_right not registered');
  end;

  // create output
  midi_output_port := jack_port_register(GJackAudio.client, 'external_midi_out', JACK_DEFAULT_MIDI_TYPE, Longword(JackPortIsOutput), 0);
  if not Assigned(midi_output_port) then
  begin
    DBLog('midi_output_port not registered');
  end;
end;

procedure TPluginExternal.Deactivate;
var
  lError: Integer;
begin
  lError := jack_port_unregister(GJackAudio.client, audio_input_port_left);
  if lError <> 0 then
  begin
    DBLog('unregister audio_input_port_left failed');
  end
  else
  begin
    DBLog('unregister audio_input_port_left succes');
  end;
  lError := jack_port_unregister(GJackAudio.client, audio_input_port_right);
  if lError <> 0 then
  begin
    DBLog('unregister audio_input_port_right failed');
  end
  else
  begin
    DBLog('unregister audio_input_port_right succes');
  end;
  lError := jack_port_unregister(GJackAudio.client, midi_output_port);
  if lError <> 0 then
  begin
    DBLog('unregister midi_output_port failed');
  end
  else
  begin
    DBLog('unregister midi_output_port succes');
  end;

  inherited Deactivate;
end;

procedure TPluginExternal.UpdateParameters;
begin
end;

procedure Register;
begin

end;

end.

