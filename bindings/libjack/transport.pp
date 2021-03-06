unit transport;
interface
uses jacktypes, baseunix;
{
  Automatically converted by H2Pas 1.0.0 from transport.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    jack
    transport.h
}

  const
    External_library='jack'; {Setup as you need}

  Type
  Pjack_client_t  = ^jack_client_t;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  {
      Copyright (C) 2002 Paul Davis
      Copyright (C) 2003 Jack O'Quin
      
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
  
      $Id: transport.h 951 2006-05-15 21:32:08Z pbd $
   }

  type
  {*
   * Transport states.
    }
    
     { the order matters for binary compatibility  }
     jack_transport_state_t =
       {*< Transport halted  }
     (JackTransportStopped := 0,
       {*< Transport playing  }
      JackTransportRolling := 1,
       {*< For OLD_TRANSPORT, now ignored  }
      JackTransportLooping := 2,
       {*< Waiting for sync ready  }
      JackTransportStarting := 3
     );

     jack_unique_t = longword;
  {*< Unique ID (opaque)  }
  {*
   * Optional struct jack_position_t fields.
    }

     jack_position_bits_t =
       {*< Bar, Beat, Tick  }
      (JackPositionBBT := $10,
       {*< External timecode  }
       JackPositionTimecode := $20,
       {*< Frame offset of BBT information  }
       JackBBTFrameOffset := $40,
       {*< audio frames per video frame  }
       JackAudioVideoRatio := $80,
       {*< frame offset of first video frame  }
       JackVideoFrameOffset := $100);
  {* all valid position bits  }

  const
    //(((JackPositionBBT or JackPositionTimecode) or JackBBTFrameOffset) or JackAudioVideoRatio) or JackVideoFrameOffset;
    JACK_POSITION_MASK = $1F0;

  {*
   * Struct for transport position information.
    }

  type

     jack_position_t = record
          { these four cannot be set from clients: the server sets them  }
          {*< unique ID  }
          unique_1 : jack_unique_t;
          {*< monotonic, free-rolling  }
          usecs : jack_time_t;
          {*< current frame rate (per second)  }
          frame_rate : jack_nframes_t;
          {*< frame number, always present  }
          frame : jack_nframes_t;
          {*< which other fields are valid  }
          valid : jack_position_bits_t;
          { JackPositionBBT fields:  }
          {*< current bar  }
          bar : longint;
          {*< current beat-within-bar  }
          beat : longint;
          {*< current tick-within-beat  }
          tick : longint;
          bar_start_tick : double;
          {*< time signature "numerator"  }
          beats_per_bar : single;
          {*< time signature "denominator"  }
          beat_type : single;
          { JackPositionTimecode fields:	(EXPERIMENTAL: could change)  }
          ticks_per_beat : double;
          beats_per_minute : double;
          {*< current time in seconds  }
          frame_time : double;
          {*< next sequential frame_time
  					     (unless repositioned)  }
          next_time : double;
          { JackBBTFrameOffset fields:  }
          {*< frame offset for the BBT fields
  					     (the given bar, beat, and tick
  					     values actually refer to a time
  					     frame_offset frames before the
  					     start of the cycle), should
  					     be assumed to be 0 if
  					     JackBBTFrameOffset is not
  					     set. If JackBBTFrameOffset is
  					     set and this value is zero, the BBT
  					     time refers to the first frame of this
  					     cycle. If the value is positive,
  					     the BBT time refers to a frame that
  					     many frames before the start of the
  					     cycle.  }
          bbt_offset : jack_nframes_t;
          {*< number of audio frames
  					     per video frame. Should be assumed
  					     zero if JackAudioVideoRatio is not
  					     set. If JackAudioVideoRatio is set
  					     and the value is zero, no video
  					     data exists within the JACK graph  }
          { JACK video positional data (experimental)  }
          audio_frames_per_video_frame : single;
          {*< audio frame at which the first video
  					     frame in this cycle occurs. Should
  					     be assumed to be 0 if JackVideoFrameOffset
  					     is not set. If JackVideoFrameOffset is
  					     set, but the value is zero, there is
  					     no video frame within this cycle.  }
          video_offset : jack_nframes_t;
          { For binary compatibility, new fields should be allocated from
           * this padding area with new valid bits controlling access, so
           * the existing structure size and offsets are preserved.  }
          padding : array[0..9] of longint;
          { When (unique_1 == unique_2) the contents are consistent.  }
          {*< unique ID  }
          unique_2 : jack_unique_t;
       end;

  Pjack_position_t  = ^jack_position_t;
  {*
   * Called by the timebase master to release itself from that
   * responsibility.
   *
   * If the timebase master releases the timebase or leaves the JACK
   * graph for any reason, the JACK engine takes over at the start of
   * the next process cycle.  The transport state does not change.  If
   * rolling, it continues to play, with frame numbers as the only
   * available position information.
   *
   * @see jack_set_timebase_callback
   *
   * @param client the JACK client structure.
   *
   * @return 0 on success, otherwise a non-zero error code.
    }

  function jack_release_timebase(client:pjack_client_t):longint;cdecl;external External_library name 'jack_release_timebase';

  {*
   * Prototype for the @a sync_callback defined by slow-sync clients.
   * When the client is active, this callback is invoked just before
   * process() in the same thread.  This occurs once after registration,
   * then subsequently whenever some client requests a new position, or
   * the transport enters the ::JackTransportStarting state.  This
   * realtime function must not wait.
   *
   * The transport @a state will be:
   *
   *   - ::JackTransportStopped when a new position is requested;
   *   - ::JackTransportStarting when the transport is waiting to start;
   *   - ::JackTransportRolling when the timeout has expired, and the
   *   position is now a moving target.
   *
   * @param state current transport state.
   * @param pos new transport position.
   * @param arg the argument supplied by jack_set_sync_callback().
   *
   * @return TRUE (non-zero) when ready to roll.
    }
  type

     JackSyncCallback = function (state:jack_transport_state_t; pos:pjack_position_t; arg:pointer):longint;cdecl;
  {*
   * Register (or unregister) as a slow-sync client, one that cannot
   * respond immediately to transport position changes.
   *
   * The @a sync_callback will be invoked at the first available
   * opportunity after its registration is complete.  If the client is
   * currently active this will be the following process cycle,
   * otherwise it will be the first cycle after calling jack_activate().
   * After that, it runs according to the ::JackSyncCallback rules.
   * Clients that don't set a @a sync_callback are assumed to be ready
   * immediately any time the transport wants to start.
   *
   * @param client the JACK client structure.
   * @param sync_callback is a realtime function that returns TRUE when
   * the client is ready.  Setting @a sync_callback to NULL declares that
   * this client no longer requires slow-sync processing.
   * @param arg an argument for the @a sync_callback function.
   *
   * @return 0 on success, otherwise a non-zero error code.
    }

  function jack_set_sync_callback(client:pjack_client_t; sync_callback:JackSyncCallback; arg:pointer):longint;cdecl;external External_library name 'jack_set_sync_callback';

  {*
   * Set the timeout value for slow-sync clients.
   *
   * This timeout prevents unresponsive slow-sync clients from
   * completely halting the transport mechanism.  The default is two
   * seconds.  When the timeout expires, the transport starts rolling,
   * even if some slow-sync clients are still unready.  The @a
   * sync_callbacks of these clients continue being invoked, giving them
   * a chance to catch up.
   *
   * @see jack_set_sync_callback
   *
   * @param client the JACK client structure.
   * @param timeout is delay (in microseconds) before the timeout expires.
   *
   * @return 0 on success, otherwise a non-zero error code.
    }
  function jack_set_sync_timeout(client:pjack_client_t; timeout:jack_time_t):longint;cdecl;external External_library name 'jack_set_sync_timeout';

  {*
   * Prototype for the @a timebase_callback used to provide extended
   * position information.  Its output affects all of the following
   * process cycle.  This realtime function must not wait.
   *
   * This function is called immediately after process() in the same
   * thread whenever the transport is rolling, or when any client has
   * requested a new position in the previous cycle.  The first cycle
   * after jack_set_timebase_callback() is also treated as a new
   * position, or the first cycle after jack_activate() if the client
   * had been inactive.
   *
   * The timebase master may not use its @a pos argument to set @a
   * pos->frame.  To change position, use jack_transport_reposition() or
   * jack_transport_locate().  These functions are realtime-safe, the @a
   * timebase_callback can call them directly.
   *
   * @param state current transport state.
   * @param nframes number of frames in current period.
   * @param pos address of the position structure for the next cycle; @a
   * pos->frame will be its frame number.  If @a new_pos is FALSE, this
   * structure contains extended position information from the current
   * cycle.  If TRUE, it contains whatever was set by the requester.
   * The @a timebase_callback's task is to update the extended
   * information here.
   * @param new_pos TRUE (non-zero) for a newly requested @a pos, or for
   * the first cycle after the @a timebase_callback is defined.
   * @param arg the argument supplied by jack_set_timebase_callback().
    }
  type

     JackTimebaseCallback = procedure (state:jack_transport_state_t; nframes:jack_nframes_t; pos:pjack_position_t; new_pos:longint; arg:pointer);cdecl;
  {*
   * Register as timebase master for the JACK subsystem.
   *
   * The timebase master registers a callback that updates extended
   * position information such as beats or timecode whenever necessary.
   * Without this extended information, there is no need for this
   * function.
   *
   * There is never more than one master at a time.  When a new client
   * takes over, the former @a timebase_callback is no longer called.
   * Taking over the timebase may be done conditionally, so it fails if
   * there was a master already.
   *
   * @param client the JACK client structure.
   * @param conditional non-zero for a conditional request.
   * @param timebase_callback is a realtime function that returns
   * position information.
   * @param arg an argument for the @a timebase_callback function.
   *
   * @return
   *   - 0 on success;
   *   - EBUSY if a conditional request fails because there was already a
   *   timebase master;
   *   - other non-zero error code.
    }

  function jack_set_timebase_callback(client:pjack_client_t; conditional:longint; timebase_callback:JackTimebaseCallback; arg:pointer):longint;cdecl;external External_library name 'jack_set_timebase_callback';

  {*
   * Reposition the transport to a new frame number.
   *
   * May be called at any time by any client.  The new position takes
   * effect in two process cycles.  If there are slow-sync clients and
   * the transport is already rolling, it will enter the
   * ::JackTransportStarting state and begin invoking their @a
   * sync_callbacks until ready.  This function is realtime-safe.
   *
   * @see jack_transport_reposition, jack_set_sync_callback
   * 
   * @param client the JACK client structure.
   * @param frame frame number of new transport position.
   *
   * @return 0 if valid request, non-zero otherwise.
    }
  function jack_transport_locate(client:pjack_client_t; frame:jack_nframes_t):longint;cdecl;external External_library name 'jack_transport_locate';

  {*
   * Query the current transport state and position.
   *
   * This function is realtime-safe, and can be called from any thread.
   * If called from the process thread, @a pos corresponds to the first
   * frame of the current cycle and the state returned is valid for the
   * entire cycle.
   *
   * @param client the JACK client structure.
   * @param pos pointer to structure for returning current transport
   * position; @a pos->valid will show which fields contain valid data.
   * If @a pos is NULL, do not return position information.
   *
   * @return Current transport state.
    }
  function jack_transport_query(client:pjack_client_t; pos:pjack_position_t):jack_transport_state_t;cdecl;external External_library name 'jack_transport_query';

  {*
   * Return an estimate of the current transport frame,
   * including any time elapsed since the last transport
   * positional update.
   *
   * @param client the JACK client structure
    }
  function jack_get_current_transport_frame(client:pjack_client_t):jack_nframes_t;cdecl;external External_library name 'jack_get_current_transport_frame';

  {*
   * Request a new transport position.
   *
   * May be called at any time by any client.  The new position takes
   * effect in two process cycles.  If there are slow-sync clients and
   * the transport is already rolling, it will enter the
   * ::JackTransportStarting state and begin invoking their @a
   * sync_callbacks until ready.  This function is realtime-safe.
   *
   * @see jack_transport_locate, jack_set_sync_callback
   * 
   * @param client the JACK client structure.
   * @param pos requested new transport position.
   *
   * @return 0 if valid request, EINVAL if position structure rejected.
    }
  function jack_transport_reposition(client:pjack_client_t; pos:pjack_position_t):longint;cdecl;external External_library name 'jack_transport_reposition';

  {*
   * Start the JACK transport rolling.
   *
   * Any client can make this request at any time.  It takes effect no
   * sooner than the next process cycle, perhaps later if there are
   * slow-sync clients.  This function is realtime-safe.
   *
   * @see jack_set_sync_callback
   *
   * @param client the JACK client structure.
    }
  procedure jack_transport_start(client:pjack_client_t);cdecl;external External_library name 'jack_transport_start';

  {*
   * Stop the JACK transport.
   *
   * Any client can make this request at any time.  It takes effect on
   * the next process cycle.  This function is realtime-safe.
   *
   * @param client the JACK client structure.
    }
  procedure jack_transport_stop(client:pjack_client_t);cdecl;external External_library name 'jack_transport_stop';

  {********************************************************************
   * The following interfaces are DEPRECATED.  They are only provided
   * for compatibility with the earlier JACK transport implementation.
   ******************************************************************** }
  {*
   * Optional struct jack_transport_info_t fields.
   *
   * @see jack_position_bits_t.
    }

  type
     jack_transport_bits_t =
      {*< Transport state  }
     (JackTransportState := $1,
      {*< Frame number  }
      JackTransportPosition := $2,
      {*< Loop boundaries (ignored)  }
      JackTransportLoop := $4,
      {*< SMPTE (ignored)  }
      JackTransportSMPTE := $8,
      {*< Bar, Beat, Tick  }
      JackTransportBBT := $10);
      
  {*
   * Deprecated struct for transport position information.
   *
   * @deprecated This is for compatibility with the earlier transport
   * interface.  Use the jack_position_t struct, instead.
    }
  { these two cannot be set from clients: the server sets them  }
  {*< current frame rate (per second)  }
  {*< monotonic, free-rolling  }
  {*< which fields are legal to read  }
  {*< SMPTE offset (from frame 0)  }
  {*< 29.97, 30, 24 etc.  }

     jack_transport_info_t = record
          frame_rate : jack_nframes_t;
          usecs : jack_time_t;
          valid : jack_transport_bits_t;
          transport_state : jack_transport_state_t;
          frame : jack_nframes_t;
          loop_start : jack_nframes_t;
          loop_end : jack_nframes_t;
          smpte_offset : longint;
          smpte_frame_rate : single;
          bar : longint;
          beat : longint;
          tick : longint;
          bar_start_tick : double;
          beats_per_bar : single;
          beat_type : single;
          ticks_per_beat : double;
          beats_per_minute : double;
       end;

  Pjack_transport_info_t  = ^jack_transport_info_t;

  {*
   * Gets the current transport info structure (deprecated).
   *
   * @param client the JACK client structure.
   * @param tinfo current transport info structure.  The "valid" field
   * describes which fields contain valid data.
   *
   * @deprecated This is for compatibility with the earlier transport
   * interface.  Use jack_transport_query(), instead.
   *
   * @pre Must be called from the process thread.
    }

  procedure jack_get_transport_info(client:pjack_client_t; tinfo:pjack_transport_info_t);cdecl;external External_library name 'jack_get_transport_info';

  {*
   * Set the transport info structure (deprecated).
   *
   * @deprecated This function still exists for compatibility with the
   * earlier transport interface, but it does nothing.  Instead, define
   * a ::JackTimebaseCallback.
    }
  procedure jack_set_transport_info(client:pjack_client_t; tinfo:pjack_transport_info_t);cdecl;external External_library name 'jack_set_transport_info';

implementation

end.
