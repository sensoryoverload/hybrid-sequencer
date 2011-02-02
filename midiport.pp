
unit midiport;
interface
uses jacktypes, baseunix;
{
  Automatically converted by H2Pas 1.0.0 from midiport.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    jack
    midiport.h
}

  const
    External_library='jack'; {Setup as you need}


{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  {
      Copyright (C) 2004 Ian Esten
      
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
  
   }
{$ifndef __JACK_MIDIPORT_H}
{$define __JACK_MIDIPORT_H}  
{ C++ extern C conditionnal removed }
//{$include <jack/types.h>}
//{$include <stdlib.h>}
  {* Type for raw event data contained in @ref jack_midi_event_t.  }

  type

    jack_midi_data_t = byte;
    Pjack_midi_data_t  = ^jack_midi_data_t;

    {* A Jack MIDI event.  }
    _jack_midi_event = record
      time : jack_nframes_t;      {*< Sample index at which event is valid  }
      size : size_t;              {*< Number of bytes of data in \a buffer  }
      buffer : ^jack_midi_data_t; {*< Raw MIDI data  }
    end;
    jack_midi_event_t = _jack_midi_event;
    Pjack_midi_event_t  = ^jack_midi_event_t;

  { Get number of events in a port buffer.
   *
   * @param port_buffer Port buffer from which to retrieve event.
   * @param nframes Number of valid frames this cycle.
   * @return number of events inside @a port_buffer
    }

  function jack_midi_get_event_count(port_buffer:pointer; nframes:jack_nframes_t):jack_nframes_t;cdecl;external External_library name 'jack_midi_get_event_count';

  {* Get a MIDI event from an event port buffer.
   * 
   * Jack MIDI is normalised, the MIDI event returned by this function is
   * guaranteed to be a complete MIDI event (the status byte will always be
   * present, and no realtime events will interspered with the event).
   *
   * @param event Event structure to store retrieved event in.
   * @param port_buffer Port buffer from which to retrieve event.
   * @param event_index Index of event to retrieve.
   * @param nframes Number of valid frames this cycle.
   * @return 0 on success, ENODATA if buffer is empty.
    }
  function jack_midi_event_get(event:pjack_midi_event_t; port_buffer:pointer; event_index:jack_nframes_t; nframes:jack_nframes_t):longint;cdecl;external External_library name 'jack_midi_event_get';

  {* Clear an event buffer.
   * 
   * This should be called at the beginning of each process cycle before calling
   * @ref jack_midi_event_reserve or @ref jack_midi_event_write. This
   * function may not be called on an input port's buffer.
   *
   * @param port_buffer Port buffer to clear (must be an output port buffer).
   * @param nframes Number of valid frames this cycle.
    }
  procedure jack_midi_clear_buffer(port_buffer:pointer);cdecl;external External_library name 'jack_midi_clear_buffer';

  {* Get the size of the largest event that can be stored by the port.
   *
   * This function returns the current space available, taking into account
   * events already stored in the port.
   *
   * @param port_buffer Port buffer to check size of.
    }
  function jack_midi_max_event_size(port_buffer:pointer; nframes:jack_nframes_t):size_t;cdecl;external External_library name 'jack_midi_max_event_size';

  {* Allocate space for an event to be written to an event port buffer.
   *
   * Clients are to write the actual event data to be written starting at the
   * pointer returned by this function. Clients must not write more than
   * @a data_size bytes into this buffer.  Clients must write normalised
   * MIDI data to the port - no running status and no (1-byte) realtime
   * messages interspersed with other messages (realtime messages are fine
   * when they occur on their own, like other messages).
   *
   * @param port_buffer Buffer to write event to.
   * @param time Sample offset of event.
   * @param data_size Length of event's raw data in bytes.
   * @param nframes Number of valid frames this event.
   * @return Pointer to the beginning of the reserved event's data buffer, or
   * NULL on error (ie not enough space).
    }
  function jack_midi_event_reserve(port_buffer:pointer; time:jack_nframes_t; data_size:size_t):Pjack_midi_data_t;cdecl;external External_library name 'jack_midi_event_reserve';

  {* Write an event into an event port buffer.
   *
   * This function is simply a wrapper for @ref jack_midi_event_reserve
   * which writes the event data into the space reserved in the buffer.
   * The same restrictions on the MIDI data apply.
   * 
   * @param port_buffer Buffer to write event to.
   * @param time Sample offset of event.
   * @param data Message data to be written.
   * @param data_size Length of @a data in bytes.
   * @param nframes Number of valid frames this event.
   * @return 0 on success, ENOBUFS if there's not enough space in buffer for event.
    }
(* Const before type ignored *)
  function jack_midi_event_write(port_buffer:pointer; time:jack_nframes_t; data:pjack_midi_data_t; data_size:size_t; nframes:jack_nframes_t):longint;cdecl;external External_library name 'jack_midi_event_write';

  {* Get the number of events that could not be written to @a port_buffer.
   *
   * This function returning a non-zero value implies @a port_buffer is full.
   * Currently the only way this can happen is if events are lost on port mixdown.
   *
   * @param port_buffer Port to receive count for.
   * @param nframes Number of valid frames this cycle.
   * @returns Number of events that could not be written to @a port_buffer.
    }
  function jack_midi_get_lost_event_count(port_buffer:pointer; nframes:jack_nframes_t):jack_nframes_t;cdecl;external External_library name 'jack_midi_get_lost_event_count';

{ C++ end of extern C conditionnal removed }
{$endif}
  { __JACK_MIDIPORT_H  }

implementation


end.
