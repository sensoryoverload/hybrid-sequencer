unit plugin_external;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, plugin, global_command, global, globalconst, pluginhost, fx,
  jack, jacktypes, midiport;

type
  { TPluginExternal }

  TPluginExternal = class(TPluginNode)
  private
    input_ports: ppchar;
    output_ports: ppchar;

    midi_input_port : ^jack_port_t;
    midi_output_port : ^jack_port_t;
    audio_input_port : ^jack_port_t;
    audio_output_port_left : ^jack_port_t;
    audio_output_port_right : ^jack_port_t;
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

(*
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
end;*)

constructor TPluginExternal.Create(AObjectOwnerID: string; AMapped: Boolean);
begin
  inherited;

  PluginName := 'External';
end;

destructor TPluginExternal.Destroy;
begin

  inherited Destroy;
end;

procedure TPluginExternal.Process(AMidiBuffer: TMidiBuffer;
  AInputBuffer: PSingle; AOutputBuffer: PSingle;
  AFrames: Integer);
var
  lIndex: Integer;
  lInputL: Single;
  lInputR: Single;
  buffer: ^byte;
  i: Integer;
  lOutput: single;
  lOffsetL: Integer;
  lOffsetR: Integer;
  lMidiEvent: TMidiEvent;
  lMidiBufferIndex: Integer;
begin
  inherited;

  if AMidiBuffer.Count > 0 then
  begin
    //DBLog(Format('AMidiBuffer.Count %d', [AMidiBuffer.Count]));
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
          buffer := jack_midi_event_reserve(midi_output_port, i, 3);
          if Assigned(buffer) then
          begin
            case lMidiEvent.DataType of
              mtNoteOn:
              begin
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
            //DBLog('jackmidi buffer allocation failed');
          end;
        end;
      end;
    end;

    //get audio from jack input

    AOutputBuffer[lOffsetL] := lOutput;
    AOutputBuffer[lOffsetR] := lOutput;
    Inc(lOffsetL, 2);
    Inc(lOffsetR, 2);
  end;
end;

procedure TPluginExternal.Instantiate;
begin
  (* This would only be needed when other jack clients are also running
     Connecting to other clients should be done by other utilities

  // discover inputs
  input_ports := jack_get_ports(client, nil, nil, (Longword(JackPortIsInput)));

  // discover outputs
  output_ports := jack_get_ports(client, nil, nil, (Longword(JackPortIsOutput)));
  *)

  // create input
  audio_input_port := jack_port_register(GJackAudio.client, 'audio_in', JACK_DEFAULT_AUDIO_TYPE, Longword(JackPortIsInput), 0);

  // create output
  midi_output_port := jack_port_register(GJackAudio.client, 'midi_out', JACK_DEFAULT_MIDI_TYPE, Longword(JackPortIsOutput), 0);
end;

procedure TPluginExternal.Activate;
begin
  inherited Activate;

  (* This would only be needed when other jack clients are also running
     Connecting to other clients should be done by other utilities

  // Connect to selected outputs
  if jack_connect(client, jack_port_name(audio_output_port_left), output_ports[0]) <> 0 then
  begin
    DBLog('cannot connect output ports');
  end;

  // Connect to selected inputs
  if jack_connect(client, jack_port_name(audio_output_port_left), input_ports[0]) <> 0 then
  begin
    DBLog('cannot connect input ports');
  end;
  *)
end;

procedure TPluginExternal.Deactivate;
begin
  // Disconnect from selected outputs
  // Disconnect from selected inputs

  inherited Deactivate;
end;

procedure TPluginExternal.UpdateParameters;
begin
end;

procedure Register;
begin

end;

end.

