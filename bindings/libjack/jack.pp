unit jack;
interface
uses jacktypes, jackthread, baseunix;
{
  Automatically converted by H2Pas 1.0.0 from jack.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    jack
    jack.h
}

  const
    External_library='jack'; {Setup as you need}

  Type
  Pjack_port_t  = ^jack_port_t;
  Pjack_status_t  = ^jack_status_t;

  Tfunc = procedure (_para1:Pchar);
  Pfunc = ^Tfunc;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  {
      Copyright (C) 2001 Paul Davis
      Copyright (C) 2004 Jack O'Quin
      
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
  
      $Id: jack.h 895 2005-05-11 02:06:47Z pbd $
   }

  {*
   * Note: More documentation can be found in jack/types.h.
    }
  {*
   * Open an external client session with a JACK server.  This interface
   * is more complex but more powerful than jack_client_new().  With it,
   * clients may choose which of several servers to connect, and control
   * whether and how to start the server automatically, if it was not
   * already running.  There is also an option for JACK to generate a
   * unique client name, when necessary.
   *
   * @param client_name of at most jack_client_name_size() characters.
   * The name scope is local to each server.  Unless forbidden by the
   * @ref JackUseExactName option, the server will modify this name to
   * create a unique variant, if needed.
   *
   * @param options formed by OR-ing together @ref JackOptions bits.
   * Only the @ref JackOpenOptions bits are allowed.
   *
   * @param status (if non-NULL) an address for JACK to return
   * information from the open operation.  This status word is formed by
   * OR-ing together the relevant @ref JackStatus bits.
   *
   *
   * <b>Optional parameters:</b> depending on corresponding [@a options
   * bits] additional parameters may follow @a status (in this order).
   *
   * @arg [@ref JackServerName] <em>(char *) server_name</em> selects
   * from among several possible concurrent server instances.  Server
   * names are unique to each user.  If unspecified, use "default"
   * unless \$JACK_DEFAULT_SERVER is defined in the process environment.
   *
   * @return Opaque client handle if successful.  If this is NULL, the
   * open operation failed, @a *status includes @ref JackFailure and the
   * caller is not a JACK client.
    }

  function jack_client_open(client_name:pchar; options:jack_options_t; status:pjack_status_t; args:array of const):Pjack_client_t;cdecl;external External_library name 'jack_client_open';

  function jack_client_open(client_name:pchar; options:jack_options_t; status:pjack_status_t):Pjack_client_t;cdecl;external External_library name 'jack_client_open';

  {*
   * Attempt to become an external client of the Jack server.
   *
   * JACK is evolving a mechanism for automatically starting the server
   * when needed.  As a transition, jack_client_new() only does this
   * when \$JACK_START_SERVER is defined in the environment of the
   * calling process.  In the future this will become normal behavior.
   * For full control of this feature, use jack_client_open(), instead.
   * In either case, defining \$JACK_NO_START_SERVER disables this
   * feature.
   *
   * @param client_name of at most jack_client_name_size() characters.
   * If this name is already in use, the request fails.
   *
   * @return Opaque client handle if successful, otherwise NULL.
   *
   * @note Failure generally means that the JACK server is not running.
   * If there was some other problem, it will be reported via the @ref
   * jack_error_callback mechanism.  Use jack_client_open() and check
   * the @a status parameter for more detailed information.
    }
  function jack_client_new(client_name:pchar):Pjack_client_t;cdecl;external External_library name 'jack_client_new';

  {*
   * Disconnects an external client from a JACK server.
   *
   * @return 0 on success, otherwise a non-zero error code
    }
  function jack_client_close(client:pjack_client_t):longint;cdecl;external External_library name 'jack_client_close';

  {*
   * @return the maximum number of characters in a JACK client name
   * including the final NULL character.  This value is a constant.
    }
  function jack_client_name_size:longint;cdecl;external External_library name 'jack_client_name_size';

  {*
   * @return pointer to actual client name.  This is useful when @ref
   * JackUseExactName is not specified on open and @ref
   * JackNameNotUnique status was returned.  In that case, the actual
   * name will differ from the @a client_name requested.
    }
  function jack_get_client_name(client:pjack_client_t):Pchar;cdecl;external External_library name 'jack_get_client_name';

  {*
   * Load an internal client into the Jack server.
   *
   * Internal clients run inside the JACK server process.  They can use
   * most of the same functions as external clients.  Each internal
   * client must declare jack_initialize() and jack_finish() entry
   * points, called at load and unload times.  See inprocess.c for an
   * example of how to write an internal client.
   *
   * @deprecated Please use jack_internal_client_load().
   *
   * @param client_name of at most jack_client_name_size() characters.
   *
   * @param load_name of a shared object file containing the code for
   * the new client.
   *
   * @param load_init an arbitary string passed to the jack_initialize()
   * routine of the new client (may be NULL).
   *
   * @return 0 if successful.
    }

  function jack_internal_client_new(client_name:pchar; load_name:pchar; load_init:pchar):longint;cdecl;external External_library name 'jack_internal_client_new';

  {*
   * Remove an internal client from a JACK server.
   *
   * @deprecated Please use jack_internal_client_load().
    }
  procedure jack_internal_client_close(client_name:pchar);cdecl;external External_library name 'jack_internal_client_close';

  {*
   * @param client pointer to JACK client structure.
   *
   * Check if the JACK subsystem is running with -R (--realtime).
   *
   * @return 1 if JACK is running realtime, 0 otherwise
    }
  function jack_is_realtime(client:pjack_client_t):longint;cdecl;external External_library name 'jack_is_realtime';

  {* 
   * @param client pointer to JACK client structure.
   * @param function The jack_shutdown function pointer.
   * @param arg The arguments for the jack_shutdown function.
   *
   * Register a function (and argument) to be called if and when the
   * JACK server shuts down the client thread.  The function must
   * be written as if it were an asynchonrous POSIX signal
   * handler --- use only async-safe functions, and remember that it
   * is executed from another thread.  A typical function might
   * set a flag or write to a pipe so that the rest of the
   * application knows that the JACK client thread has shut
   * down.
   *
   * NOTE: clients do not need to call this.  It exists only
   * to help more complex clients understand what is going
   * on.  It should be called before jack_client_activate().
    }
  procedure jack_on_shutdown(client:pjack_client_t; shutdown_callback:JackThreadShutdownCallback; arg:pointer);cdecl;external External_library name 'jack_on_shutdown';

  {*
   * Tell the Jack server to call @a process_callback whenever there is
   * work be done, passing @a arg as the second argument.
   *
   * The code in the supplied function must be suitable for real-time
   * execution.  That means that it cannot call functions that might
   * block for a long time.  This includes malloc, free, printf,
   * pthread_mutex_lock, sleep, wait, poll, select, pthread_join,
   * pthread_cond_wait, etc, etc.  See
   * http://jackit.sourceforge.net/docs/design/design.html#SECTION00411000000000000000
   * for more information.
   *
   * @return 0 on success, otherwise a non-zero error code, causing JACK
   * to remove that client from the process() graph.
    }
  function jack_set_process_callback(client:pjack_client_t; process_callback:JackProcessCallback; arg:pointer):longint;cdecl;external External_library name 'jack_set_process_callback';

  {*
   * Tell JACK to call @a thread_init_callback once just after
   * the creation of the thread in which all other callbacks 
   * will be handled.
   *
   * The code in the supplied function does not need to be
   * suitable for real-time execution.
   *
   * @return 0 on success, otherwise a non-zero error code, causing JACK
   * to remove that client from the process() graph.
    }
  function jack_set_thread_init_callback(client:pjack_client_t; thread_init_callback:JackThreadInitCallback; arg:pointer):longint;cdecl;external External_library name 'jack_set_thread_init_callback';

  {*
   * Tell the Jack server to call @a freewheel_callback
   * whenever we enter or leave "freewheel" mode, passing @a
   * arg as the second argument. The first argument to the
   * callback will be non-zero if JACK is entering freewheel
   * mode, and zero otherwise.
   *
   * @return 0 on success, otherwise a non-zero error code.
    }
  function jack_set_freewheel_callback(client:pjack_client_t; freewheel_callback:JackFreewheelCallback; arg:pointer):longint;cdecl;external External_library name 'jack_set_freewheel_callback';

  {*
   * Start/Stop JACK's "freewheel" mode.
   *
   * When in "freewheel" mode, JACK no longer waits for
   * any external event to begin the start of the next process
   * cycle. 
   *
   * As a result, freewheel mode causes "faster than realtime"
   * execution of a JACK graph. If possessed, real-time
   * scheduling is dropped when entering freewheel mode, and
   * if appropriate it is reacquired when stopping.
   * 
   * IMPORTANT: on systems using capabilities to provide real-time
   * scheduling (i.e. Linux kernel 2.4), if onoff is zero, this function
   * must be called from the thread that originally called jack_activate(). 
   * This restriction does not apply to other systems (e.g. Linux kernel 2.6 
   * or OS X).
   * 
   * @param client pointer to JACK client structure
   * @param onoff  if non-zero, freewheel mode starts. Otherwise
   *                  freewheel mode ends.
   *
   * @return 0 on success, otherwise a non-zero error code.
    }
  function jack_set_freewheel(client:pjack_client_t; onoff:longint):longint;cdecl;external External_library name 'jack_set_freewheel';

  {*
   * Change the buffer size passed to the @a process_callback.
   *
   * This operation stops the JACK engine process cycle, then calls all
   * registered @a bufsize_callback functions before restarting the
   * process cycle.  This will cause a gap in the audio flow, so it
   * should only be done at appropriate stopping points.
   *
   * @see jack_set_buffer_size_callback()
   *
   * @param client pointer to JACK client structure.
   * @param nframes new buffer size.  Must be a power of two.
   *
   * @return 0 on success, otherwise a non-zero error code
    }
  function jack_set_buffer_size(client:pjack_client_t; nframes:jack_nframes_t):longint;cdecl;external External_library name 'jack_set_buffer_size';

  {*
   * Tell JACK to call @a bufsize_callback whenever the size of the the
   * buffer that will be passed to the @a process_callback is about to
   * change.  Clients that depend on knowing the buffer size must supply
   * a @a bufsize_callback before activating themselves.
   *
   * @param client pointer to JACK client structure.
   * @param bufsize_callback function to call when the buffer size changes.
   * @param arg argument for @a bufsize_callback.
   *
   * @return 0 on success, otherwise a non-zero error code
    }
  function jack_set_buffer_size_callback(client:pjack_client_t; bufsize_callback:JackBufferSizeCallback; arg:pointer):longint;cdecl;external External_library name 'jack_set_buffer_size_callback';

  {*
   * Tell the Jack server to call @a srate_callback whenever the system
   * sample rate changes.
   *
   * @return 0 on success, otherwise a non-zero error code
    }
  function jack_set_sample_rate_callback(client:pjack_client_t; srate_callback:JackSampleRateCallback; arg:pointer):longint;cdecl;external External_library name 'jack_set_sample_rate_callback';

  {*
   * Tell the JACK server to call @a registration_callback whenever a
   * port is registered or unregistered, passing @a arg as a parameter.
   *
   * @return 0 on success, otherwise a non-zero error code
    }
  function jack_set_port_registration_callback(_para1:Pjack_client_t; registration_callback:JackPortRegistrationCallback; arg:pointer):longint;cdecl;external External_library name 'jack_set_port_registration_callback';

  {*
   * Tell the JACK server to call @a graph_callback whenever the
   * processing graph is reordered, passing @a arg as a parameter.
   *
   * @return 0 on success, otherwise a non-zero error code
    }
  function jack_set_graph_order_callback(_para1:Pjack_client_t; graph_callback:JackGraphOrderCallback; _para3:pointer):longint;cdecl;external External_library name 'jack_set_graph_order_callback';

  {*
   * Tell the JACK server to call @a xrun_callback whenever there is a
   * xrun, passing @a arg as a parameter.
   *
   * @return 0 on success, otherwise a non-zero error code
    }
  function jack_set_xrun_callback(_para1:Pjack_client_t; xrun_callback:JackXRunCallback; arg:pointer):longint;cdecl;external External_library name 'jack_set_xrun_callback';

  {*
   * Tell the Jack server that the program is ready to start processing
   * audio.
   *
   * @return 0 on success, otherwise a non-zero error code
    }
  function jack_activate(client:pjack_client_t):longint;cdecl;external External_library name 'jack_activate';

  {*
   * Tell the Jack server to remove this @a client from the process
   * graph.  Also, disconnect all ports belonging to it, since inactive
   * clients have no port connections.
   *
   * @return 0 on success, otherwise a non-zero error code
    }
  function jack_deactivate(client:pjack_client_t):longint;cdecl;external External_library name 'jack_deactivate';

  {*
   * Create a new port for the client. This is an object used for moving
   * data of any type in or out of the client.  Ports may be connected
   * in various ways.
   *
   * Each port has a short name.  The port's full name contains the name
   * of the client concatenated with a colon (:) followed by its short
   * name.  The jack_port_name_size() is the maximum length of this full
   * name.  Exceeding that will cause the port registration to fail and
   * return NULL.
   *
   * All ports have a type, which may be any non-NULL and non-zero
   * length string, passed as an argument.  Some port types are built
   * into the JACK API, currently only JACK_DEFAULT_AUDIO_TYPE.
   *
   * @param client pointer to JACK client structure.
   * @param port_name non-empty short name for the new port (not
   * including the leading @a "client_name:").
   * @param port_type port type name.  If longer than
   * jack_port_type_size(), only that many characters are significant.
   * @param flags @ref JackPortFlags bit mask.
   * @param buffer_size must be non-zero if this is not a built-in @a
   * port_type.  Otherwise, it is ignored.
   *
   * @return jack_port_t pointer on success, otherwise NULL.
    }
  function jack_port_register(client:pjack_client_t; port_name:pchar; port_type:pchar; flags:dword; buffer_size:dword):Pjack_port_t;cdecl;external External_library name 'jack_port_register';

  {* 
   * Remove the port from the client, disconnecting any existing
   * connections.
   *
   * @return 0 on success, otherwise a non-zero error code
    }
  function jack_port_unregister(_para1:Pjack_client_t; _para2:Pjack_port_t):longint;cdecl;external External_library name 'jack_port_unregister';

  {*
   * This returns a pointer to the memory area associated with the
   * specified port. For an output port, it will be a memory area
   * that can be written to; for an input port, it will be an area
   * containing the data from the port's connection(s), or
   * zero-filled. if there are multiple inbound connections, the data
   * will be mixed appropriately.  
   *
   * FOR OUTPUT PORTS ONLY
   * ---------------------
   * You may cache the value returned, but only between calls to
   * your "blocksize" callback. For this reason alone, you should
   * either never cache the return value or ensure you have
   * a "blocksize" callback and be sure to invalidate the cached
   * address from there.
    }
  function jack_port_get_buffer(_para1:Pjack_port_t; _para2:jack_nframes_t):pointer;cdecl;external External_library name 'jack_port_get_buffer';

  {*
   * @return the full name of the jack_port_t (including the @a
   * "client_name:" prefix).
   *
   * @see jack_port_name_size().
    }
  function jack_port_name(port:pjack_port_t):Pchar;cdecl;external External_library name 'jack_port_name';

  {*
   * @return the short name of the jack_port_t (not including the @a
   * "client_name:" prefix).
   *
   * @see jack_port_name_size().
    }
  function jack_port_short_name(port:pjack_port_t):Pchar;cdecl;external External_library name 'jack_port_short_name';

  {*
   * @return the @ref JackPortFlags of the jack_port_t.
    }
  function jack_port_flags(port:pjack_port_t):longint;cdecl;external External_library name 'jack_port_flags';

  {*
   * @return the @a port type, at most jack_port_type_size() characters
   * including a final NULL.
    }
  function jack_port_type(port:pjack_port_t):Pchar;cdecl;external External_library name 'jack_port_type';

  {* 
   * @return TRUE if the jack_port_t belongs to the jack_client_t.
    }
  function jack_port_is_mine(_para1:Pjack_client_t; port:pjack_port_t):longint;cdecl;external External_library name 'jack_port_is_mine';

  {* 
   * @return number of connections to or from @a port.
   *
   * @pre The calling client must own @a port.
    }
  function jack_port_connected(port:pjack_port_t):longint;cdecl;external External_library name 'jack_port_connected';

  {*
   * @return TRUE if the locally-owned @a port is @b directly connected
   * to the @a port_name.
   *
   * @see jack_port_name_size()
    }
(* Const before type ignored *)
  function jack_port_connected_to(port:pjack_port_t; port_name:pchar):longint;cdecl;external External_library name 'jack_port_connected_to';

  {*
   * @return a null-terminated array of full port names to which the @a
   * port is connected.  If none, returns NULL.
   *
   * The caller is responsible for calling free(3) on any non-NULL
   * returned value.
   *
   * @param port locally owned jack_port_t pointer.
   *
   * @see jack_port_name_size(), jack_port_get_all_connections()
    }(* Const before type ignored *)
  function jack_port_get_connections(port:pjack_port_t):PPchar;cdecl;external External_library name 'jack_port_get_connections';

  {*
   * @return a null-terminated array of full port names to which the @a
   * port is connected.  If none, returns NULL.
   *
   * The caller is responsible for calling free(3) on any non-NULL
   * returned value.
   *
   * This differs from jack_port_get_connections() in two important
   * respects:
   *
   *     1) You may not call this function from code that is
   *          executed in response to a JACK event. For example,
   *          you cannot use it in a GraphReordered handler.
   *
   *     2) You need not be the owner of the port to get information
   *          about its connections. 
   *
   * @see jack_port_name_size()
    }

  function jack_port_get_all_connections(client:pjack_client_t; port:pjack_port_t):PPchar;cdecl;external External_library name 'jack_port_get_all_connections';

  {*
   * A client may call this on a pair of its own ports to 
   * semi-permanently wire them together. This means that
   * a client that wants to direct-wire an input port to
   * an output port can call this and then no longer
   * have to worry about moving data between them. Any data
   * arriving at the input port will appear automatically
   * at the output port.
   *
   * The 'destination' port must be an output port. The 'source'
   * port must be an input port. Both ports must belong to
   * the same client. You cannot use this to tie ports between
   * clients. That is what a connection is for.
   *
   * @return 0 on success, otherwise a non-zero error code
    }
  function jack_port_tie(src:pjack_port_t; dst:pjack_port_t):longint;cdecl;external External_library name 'jack_port_tie';

  {*
   * This undoes the effect of jack_port_tie(). The port
   * should be same as the 'destination' port passed to
   * jack_port_tie().
   *
   * @return 0 on success, otherwise a non-zero error code
    }
  function jack_port_untie(port:pjack_port_t):longint;cdecl;external External_library name 'jack_port_untie';

  {*
   * A client may call this function to prevent other objects
   * from changing the connection status of a port. The port
   * must be owned by the calling client.
   *
   * @return 0 on success, otherwise a non-zero error code
    }
  function jack_port_lock(_para1:Pjack_client_t; _para2:Pjack_port_t):longint;cdecl;external External_library name 'jack_port_lock';

  {*
   * This allows other objects to change the connection status of a port.
   *
   * @return 0 on success, otherwise a non-zero error code
    }
  function jack_port_unlock(_para1:Pjack_client_t; _para2:Pjack_port_t):longint;cdecl;external External_library name 'jack_port_unlock';

  {* 
   * @return the time (in frames) between data being available or
   * delivered at/to a port, and the time at which it arrived at or is
   * delivered to the "other side" of the port.  E.g. for a physical
   * audio output port, this is the time between writing to the port and
   * when the signal will leave the connector.  For a physical audio
   * input port, this is the time between the sound arriving at the
   * connector and the corresponding frames being readable from the
   * port.
    }
  function jack_port_get_latency(port:pjack_port_t):jack_nframes_t;cdecl;external External_library name 'jack_port_get_latency';

  {*
   * The maximum of the sum of the latencies in every
   * connection path that can be drawn between the port and other
   * ports with the @ref JackPortIsTerminal flag set.
    }
  function jack_port_get_total_latency(_para1:Pjack_client_t; port:pjack_port_t):jack_nframes_t;cdecl;external External_library name 'jack_port_get_total_latency';

  {*
   * The port latency is zero by default. Clients that control
   * physical hardware with non-zero latency should call this
   * to set the latency to its correct value. Note that the value
   * should include any systemic latency present "outside" the
   * physical hardware controlled by the client. For example,
   * for a client controlling a digital audio interface connected
   * to an external digital converter, the latency setting should
   * include both buffering by the audio interface *and* the converter. 
    }
  procedure jack_port_set_latency(_para1:Pjack_port_t; _para2:jack_nframes_t);cdecl;external External_library name 'jack_port_set_latency';

  {*
   *
    }
  function jack_recompute_total_latencies(_para1:Pjack_client_t):longint;cdecl;external External_library name 'jack_recompute_total_latencies';

  {*
   * Modify a port's short name.  May be called at any time.  If the
   * resulting full name (including the @a "client_name:" prefix) is
   * longer than jack_port_name_size(), it will be truncated.
   *
   * @return 0 on success, otherwise a non-zero error code.
    }
  function jack_port_set_name(port:pjack_port_t; port_name:pchar):longint;cdecl;external External_library name 'jack_port_set_name';

  {*
   * If @ref JackPortCanMonitor is set for this @a port, turn input
   * monitoring on or off.  Otherwise, do nothing.
    }
  function jack_port_request_monitor(port:pjack_port_t; onoff:longint):longint;cdecl;external External_library name 'jack_port_request_monitor';

  {*
   * If @ref JackPortCanMonitor is set for this @a port_name, turn input
   * monitoring on or off.  Otherwise, do nothing.
   *
   * @return 0 on success, otherwise a non-zero error code.
   *
   * @see jack_port_name_size()
    }
  function jack_port_request_monitor_by_name(client:pjack_client_t; port_name:pchar; onoff:longint):longint;cdecl;external External_library name 'jack_port_request_monitor_by_name';

  {*
   * If @ref JackPortCanMonitor is set for a port, this function turns
   * on input monitoring if it was off, and turns it off if only one
   * request has been made to turn it on.  Otherwise it does nothing.
   *
   * @return 0 on success, otherwise a non-zero error code
    }
  function jack_port_ensure_monitor(port:pjack_port_t; onoff:longint):longint;cdecl;external External_library name 'jack_port_ensure_monitor';

  {*
   * @return TRUE if input monitoring has been requested for @a port.
    }
  function jack_port_monitoring_input(port:pjack_port_t):longint;cdecl;external External_library name 'jack_port_monitoring_input';

  {*
   * Establish a connection between two ports.
   *
   * When a connection exists, data written to the source port will
   * be available to be read at the destination port.
   *
   * @pre The port types must be identical.
   *
   * @pre The @ref JackPortFlags of the @a source_port must include @ref
   * JackPortIsOutput.
   *
   * @pre The @ref JackPortFlags of the @a destination_port must include
   * @ref JackPortIsInput.
   *
   * @return 0 on success, EEXIST if the connection is already made,
   * otherwise a non-zero error code
    }
  function jack_connect(_para1:Pjack_client_t; source_port:pchar; destination_port:pchar):longint;cdecl;external External_library name 'jack_connect';

  {*
   * Remove a connection between two ports.
   *
   * @pre The port types must be identical.
   *
   * @pre The @ref JackPortFlags of the @a source_port must include @ref
   * JackPortIsOutput.
   *
   * @pre The @ref JackPortFlags of the @a destination_port must include
   * @ref JackPortIsInput.
   *
   * @return 0 on success, otherwise a non-zero error code
    }
  function jack_disconnect(_para1:Pjack_client_t; source_port:pchar; destination_port:pchar):longint;cdecl;external External_library name 'jack_disconnect';

  {*
   * Perform the same function as jack_disconnect() using port handles
   * rather than names.  This avoids the name lookup inherent in the
   * name-based version.
   *
   * Clients connecting their own ports are likely to use this function,
   * while generic connection clients (e.g. patchbays) would use
   * jack_disconnect().
    }
  function jack_port_disconnect(_para1:Pjack_client_t; _para2:Pjack_port_t):longint;cdecl;external External_library name 'jack_port_disconnect';

  {*
   * @return the maximum number of characters in a full JACK port name
   * including the final NULL character.  This value is a constant.
   *
   * A port's full name contains the owning client name concatenated
   * with a colon (:) followed by its short name and a NULL
   * character.
    }
  function jack_port_name_size:longint;cdecl;external External_library name 'jack_port_name_size';

  {*
   * @return the maximum number of characters in a JACK port type name
   * including the final NULL character.  This value is a constant.
    }
  function jack_port_type_size:longint;cdecl;external External_library name 'jack_port_type_size';

  {*
   * @return the sample rate of the jack system, as set by the user when
   * jackd was started.
    }
  function jack_get_sample_rate(_para1:Pjack_client_t):jack_nframes_t;cdecl;external External_library name 'jack_get_sample_rate';

  {*
   * @return the current maximum size that will ever be passed to the @a
   * process_callback.  It should only be used *before* the client has
   * been activated.  This size may change, clients that depend on it
   * must register a @a bufsize_callback so they will be notified if it
   * does.
   *
   * @see jack_set_buffer_size_callback()
    }
  function jack_get_buffer_size(_para1:Pjack_client_t):jack_nframes_t;cdecl;external External_library name 'jack_get_buffer_size';

  {*
   * @param port_name_pattern A regular expression used to select 
   * ports by name.  If NULL or of zero length, no selection based 
   * on name will be carried out.
   * @param type_name_pattern A regular expression used to select 
   * ports by type.  If NULL or of zero length, no selection based 
   * on type will be carried out.
   * @param flags A value used to select ports by their flags.  
   * If zero, no selection based on flags will be carried out.
   *
   * @return a NULL-terminated array of ports that match the specified
   * arguments.  The caller is responsible for calling free(3) any
   * non-NULL returned value.
   *
   * @see jack_port_name_size(), jack_port_type_size()
    }
  function jack_get_ports(_para1:Pjack_client_t; port_name_pattern:pchar; type_name_pattern:pchar; flags:dword):PPchar;cdecl;external External_library name 'jack_get_ports';

  {*
   * @return address of the jack_port_t named @a port_name.
   *
   * @see jack_port_name_size()
    }
  function jack_port_by_name(_para1:Pjack_client_t; port_name:pchar):Pjack_port_t;cdecl;external External_library name 'jack_port_by_name';

  {*
   * @return address of the jack_port_t of a @a port_id.
    }
  function jack_port_by_id(client:pjack_client_t; port_id:jack_port_id_t):Pjack_port_t;cdecl;external External_library name 'jack_port_by_id';

  {*
   * Old-style interface to become the timebase for the entire JACK
   * subsystem.
   *
   * @deprecated This function still exists for compatibility with the
   * earlier transport interface, but it does nothing.  Instead, see
   * transport.h and use jack_set_timebase_callback().
   *
   * @return ENOSYS, function not implemented.
    }
  function jack_engine_takeover_timebase(_para1:Pjack_client_t):longint;cdecl;external External_library name 'jack_engine_takeover_timebase';

  {*
   * @return the time in frames that has passed since the JACK server
   * began the current process cycle.
    }
  function jack_frames_since_cycle_start(_para1:Pjack_client_t):jack_nframes_t;cdecl;external External_library name 'jack_frames_since_cycle_start';

  {*
   * @return an estimate of the current time in frames.  This is a
   * running counter, no significance should be attached to its value,
   * but it can be compared to a previously returned value.
    }
  function jack_frame_time(_para1:Pjack_client_t):jack_nframes_t;cdecl;external External_library name 'jack_frame_time';

  {*
   * @return the frame_time after the last processing of the graph
   * this is only to be used from the process callback. 
   *
   * This function can be used to put timestamps generated by 
   * jack_frame_time() in correlation to the current process cycle.
    }
  function jack_last_frame_time(client:pjack_client_t):jack_nframes_t;cdecl;external External_library name 'jack_last_frame_time';

  {*
   * @return the current CPU load estimated by JACK.  This is a running
   * average of the time it takes to execute a full process cycle for
   * all clients as a percentage of the real time available per cycle
   * determined by the buffer size and sample rate.
    }
  function jack_cpu_load(client:pjack_client_t):double;cdecl;external External_library name 'jack_cpu_load';

  {*
   * @return the pthread ID of the thread running the JACK client side
   * code.
    }
  function jack_client_thread_id(_para1:Pjack_client_t):pthread_t;cdecl;external External_library name 'jack_client_thread_id';

  {*
   * Display JACK error message.
   *
   * Set via jack_set_error_function(), otherwise a JACK-provided
   * default will print @a msg (plus a newline) to stderr.
   *
   * @param msg error message text (no newline at end).
    }

  var
     jack_error_callback : procedure (msg:pchar);cvar;external;
  {*
   * Set the @ref jack_error_callback for error message display.
   *
   * The JACK library provides two built-in callbacks for this purpose:
   * default_jack_error_callback() and silent_jack_error_callback().
    }

  procedure jack_set_error_function(func:Pfunc);cdecl;external External_library name 'jack_set_error_function';

implementation

end.
