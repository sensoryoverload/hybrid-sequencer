unit jacktypes;
interface

//uses libc;
uses BaseUnix;

{
  Automatically converted by H2Pas 1.0.0 from types.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    jack
    types.h
}

  const
    External_library='jack'; {Setup as you need}

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
  
      $Id: types.h 945 2006-05-04 15:14:45Z pbd $
   }

//{$include <inttypes.h>}

  type

     jack_shmsize_t = longint;
  {*
   * Type used to represent sample frame counts.
    }

     jack_nframes_t = longword;
  {*
   * Maximum value that can be stored in jack_nframes_t
    }
  { This should be UINT32_MAX, but
  					   C++ has a problem with that.  }

  const
     JACK_MAX_FRAMES = 4294967295;     
  {*
   * Type used to represent the value of free running
   * monotonic clock with units of microseconds.
    }

  type

     jack_time_t = qword;
  {*
   *  Maximum size of @a load_init string passed to an internal client
   *  jack_initialize() function via jack_internal_client_load().
    }

  const
     JACK_LOAD_INIT_LIMIT = 1024;     
  {*
   *  jack_intclient_t is an opaque type representing a loaded internal
   *  client.  You may only access it using the API provided in @ref
   *  intclient.h "<jack/intclient.h>".
    }

  type

     jack_intclient_t = qword;
  {*
   *  jack_port_t is an opaque type.  You may only access it using the
   *  API provided.
    }
    jack_port_t = pointer;//_jack_port;
  {*
   *  jack_client_t is an opaque type.  You may only access it using the
   *  API provided.
    }
      jack_client_t = pointer;//_jack_client
  {*
   *  Ports have unique ids. A port registration callback is the only
   *  place you ever need to know their value.
    }

     jack_port_id_t = longword;
  {*
   * Prototype for the client supplied function that is called 
   * by the engine anytime there is work to be done.
   *
   * @pre nframes == jack_get_buffer_size()
   * @pre nframes == pow(2,x)
   *
   * @param nframes number of frames to process
   * @param arg pointer to a client supplied structure
   *
   * @return zero on success, non-zero on error
    }
     JackProcessCallback = function (nframes:jack_nframes_t; arg:pointer):longint;cdecl;
  {*
   * Prototype for the client supplied function that is called 
   * once after the creation of the thread in which other
   * callbacks will be made. Special thread characteristics
   * can be set from this callback, for example. This is a
   * highly specialized callback and most clients will not
   * and should not use it.
   *
   * @param arg pointer to a client supplied structure
   *
   * @return void
    }
     JackThreadInitCallback = procedure (arg:pointer);cdecl;

     JackThreadShutdownCallback = procedure (arg:pointer);cdecl;
  {*
   * Prototype for the client supplied function that is called 
   * whenever the processing graph is reordered.
   *
   * @param arg pointer to a client supplied structure
   *
   * @return zero on success, non-zero on error
    }
     JackGraphOrderCallback = function (arg:pointer):longint;cdecl;
  {*
   * Prototype for the client-supplied function that is called whenever
   * an xrun has occured.
   *
   * @see jack_get_xrun_delayed_usecs()
   *
   * @param arg pointer to a client supplied structure
   *
   * @return zero on success, non-zero on error
    }
     JackXRunCallback = function (arg:pointer):longint;cdecl;
  {*
   * Prototype for the @a bufsize_callback that is invoked whenever the
   * JACK engine buffer size changes.  Although this function is called
   * in the JACK process thread, the normal process cycle is suspended
   * during its operation, causing a gap in the audio flow.  So, the @a
   * bufsize_callback can allocate storage, touch memory not previously
   * referenced, and perform other operations that are not realtime
   * safe.
   *
   * @param nframes buffer size
   * @param arg pointer supplied by jack_set_buffer_size_callback().
   *
   * @return zero on success, non-zero on error
    }
     JackBufferSizeCallback = function (nframes:jack_nframes_t; arg:pointer):longint;cdecl;
  {*
   * Prototype for the client supplied function that is called 
   * when the engine sample rate changes.
   *
   * @param nframes new engine sample rate
   * @param arg pointer to a client supplied structure
   *
   * @return zero on success, non-zero on error
    }
     JackSampleRateCallback = function (nframes:jack_nframes_t; arg:pointer):longint;cdecl;
  {*
   * Prototype for the client supplied function that is called 
   * whenever a port is registered or unregistered.
   *
   * @param arg pointer to a client supplied structure
    }
     JackPortRegistrationCallback = procedure (port:jack_port_id_t; _para2:longint; arg:pointer);cdecl;
  {*
   * Prototype for the client supplied function that is called 
   * whenever jackd starts or stops freewheeling.
   *
   * @param starting non-zero if we start starting to freewheel, zero otherwise
   * @param arg pointer to a client supplied structure
    }
     JackFreewheelCallback = procedure (starting:longint; arg:pointer);cdecl;
  {*
   * Used for the type argument of jack_port_register() for default
   * audio and midi ports.
    }

  const
     JACK_DEFAULT_AUDIO_TYPE = '32 bit float mono audio';     
     JACK_DEFAULT_MIDI_TYPE = '8 bit raw midi';     
  {*
   * For convenience, use this typedef if you want to be able to change
   * between float and double. You may want to typedef sample_t to
   * jack_default_audio_sample_t in your application.
    }

  type

     jack_default_audio_sample_t = single;
     Pjack_default_audio_sample_t = ^jack_default_audio_sample_t;
  {*
   *  A port has a set of flags that are formed by AND-ing together the
   *  desired values from the list below. The flags "JackPortIsInput" and
   *  "JackPortIsOutput" are mutually exclusive and it is an error to use
   *  them both.
    }
  {*
        * if JackPortIsInput is set, then the port can receive
        * data.
         }
  {*
        * if JackPortIsOutput is set, then data can be read from
        * the port.
         }
  {*
        * if JackPortIsPhysical is set, then the port corresponds
        * to some kind of physical I/O connector.
         }
  {*
        * if JackPortCanMonitor is set, then a call to
        * jack_port_request_monitor() makes sense.
        *
        * Precisely what this means is dependent on the client. A typical
        * result of it being called with TRUE as the second argument is
        * that data that would be available from an output port (with
        * JackPortIsPhysical set) is sent to a physical output connector
        * as well, so that it can be heard/seen/whatever.
        * 
        * Clients that do not control physical interfaces
        * should never create ports with this bit set.
         }
  {*
        * JackPortIsTerminal means:
        *
        *	for an input port: the data received by the port
        *                    will not be passed on or made
        *		           available at any other port
        *
        * for an output port: the data available at the port
        *                    does not originate from any other port
        *
        * Audio synthesizers, I/O hardware interface clients, HDR
        * systems are examples of clients that would set this flag for
        * their ports.
         }
     JackPortFlags =
      (JackPortIsInput := $1,
       JackPortIsOutput := $2,
       JackPortIsPhysical := $4,
        JackPortCanMonitor := $8,
       JackPortIsTerminal := $10);

  {*
   *  @ref jack_options_t bits
    }
  {*
        * Null value to use when no option bits are needed.
         }
  {*
        * Do not automatically start the JACK server when it is not
        * already running.  This option is always selected if
        * \$JACK_NO_START_SERVER is defined in the calling process
        * environment.
         }
  {*
        * Use the exact client name requested.  Otherwise, JACK
        * automatically generates a unique one, if needed.
         }
  {*
        * Open with optional <em>(char *) server_name</em> parameter.
         }
  {*
        * Load internal client from optional <em>(char *)
        * load_name</em>.  Otherwise use the @a client_name.
         }
  {*
        * Pass optional <em>(char *) load_init</em> string to the
        * jack_initialize() entry point of an internal client.
         }
     JackOptions =
      (JackNullOption := $00,
       JackNoStartServer := $01,
       JackUseExactName := $02,
       JackServerName := $04,
       JackLoadName := $08,
       JackLoadInit := $10
       );

  {* Valid options for opening an external client.  }

  const
     JackOpenOptions = $7; //(JackServerName or JackNoStartServer) or JackUseExactName;
  {* Valid options for loading an internal client.  }
     JackLoadOptions = $1A; //(JackLoadInit or JackLoadName) or JackUseExactName;
  {*
   *  Options for several JACK operations, formed by OR-ing together the
   *  relevant @ref JackOptions bits.
    }

  type

     jack_options_t = JackOptions;
  {*
   *  @ref jack_status_t bits
    }
  {*
        * Overall operation failed.
         }
  {*
        * The operation contained an invalid or unsupported option.
         }
  {*
        * The desired client name was not unique.  With the @ref
        * JackUseExactName option this situation is fatal.  Otherwise,
        * the name was modified by appending a dash and a two-digit
        * number in the range "-01" to "-99".  The
        * jack_get_client_name() function will return the exact string
        * that was used.  If the specified @a client_name plus these
        * extra characters would be too long, the open fails instead.
         }
  {*
        * The JACK server was started as a result of this operation.
        * Otherwise, it was running already.  In either case the caller
        * is now connected to jackd, so there is no race condition.
        * When the server shuts down, the client will find out.
         }
  {*
        * Unable to connect to the JACK server.
         }
  {*
        * Communication error with the JACK server.
         }
  {*
        * Requested client does not exist.
         }
  {*
        * Unable to load internal client
         }
  {*
        * Unable to initialize client
         }
  {*
        * Unable to access shared memory
         }
  {*
        * Client's protocol version does not match
         }
     JackStatus = (JackFailure := $01,JackInvalidOption := $02,
       JackNameNotUnique := $04,JackServerStarted := $08,
       JackServerFailed := $10,JackServerError := $20,
       JackNoSuchClient := $40,JackLoadFailure := $80,
       JackInitFailure := $100,JackShmFailure := $200,
       JackVersionError := $400);

  {*
   *  Status word returned from several JACK operations, formed by
   *  OR-ing together the relevant @ref JackStatus bits.
    }

     jack_status_t = JackStatus;

const
  JACK_PORT_NAME_SIZE = 256;
  JACK_PORT_TYPE_SIZE = 32;


(* The relatively low value of this constant reflects the fact that
 * JACK currently only knows about *2* port types.  (May 2006)
 *
 * Further, the 4 covers:
 *   - a single non-negotiated audio format
 *   - music data (ie. MIDI)
 *   - video
 *   - one other
 *
 * which is probably enough for more than just the foreseeable future.
 *)
const
  JACK_MAX_PORT_TYPES =4;
  JACK_AUDIO_PORT_TYPE =0;
  JACK_MIDI_PORT_TYPE =1;

// these should probably go somewhere else, but not in <jack/types.h> */
  JACK_CLIENT_NAME_SIZE =33;

(* JACK shared memory segments are limited to MAX_INT32, they can be
 * shared between 32-bit and 64-bit clients.
 *)
const
  JACK_SHM_MAX = High(longword);


(* Port type structure.
 *
 *  (1) One for each port type is part of the engine's jack_control_t
 *  shared memory structure.
 *
 *  (2) One for each port type is appended to the engine's
 *  jack_client_connect_result_t response.  The client reads them into
 *  its local memory, using them to attach the corresponding shared
 *  memory segments.
 *)
type
 jack_port_type_id_t = longint;
  jack_client_id_t = longword;
  type

     jack_shmtype_t = (shm_POSIX := 1,shm_SYSV := 2);

     jack_shm_registry_index_t = smallint;


_jack_port_type_info = record

    ptype_id : jack_port_type_id_t;
    type_name : array[0..JACK_PORT_TYPE_SIZE] of char;

    (* If == 1, then a buffer to handle nframes worth of data has
     * sizeof(jack_default_audio_sample_t) * nframes bytes.
     *
     * If > 1, the buffer allocated for input mixing will be
     * this value times sizeof(jack_default_audio_sample_t)
     * * nframes bytes in size.  For non-audio data types,
     * it may have a different value.
     *
     * If < 0, the value should be ignored, and buffer_size
     * should be used.
     *)
     buffer_scale_factor : longint;

    (* ignored unless buffer_scale_factor is < 0. see above *)
     buffer_size : jack_shmsize_t;

     shm_registry_index :jack_shm_registry_index_t;

     zero_buffer_offset :jack_shmsize_t;

    end;

  jack_port_type_info_t = _jack_port_type_info;

//* Allocated by the engine in shared memory. */
  _jack_port_shared = record

    ptype_id : jack_port_type_id_t;	//* index into port type array */
    offset : jack_shmsize_t;	//* buffer offset in shm segment */
    id : jack_port_id_t;	//* index into engine port array */
    {enum} 	     flags:JackPortFlags;
    name : array[0..JACK_CLIENT_NAME_SIZE+JACK_PORT_NAME_SIZE] of char;
    client_id : jack_client_id_t;	//* who owns me */

    {volatile} latency : jack_nframes_t;
    {volatile} total_latency: jack_nframes_t;
    {volatile} monitor_requests:byte;

    has_mixdown:char; //* port has a mixdown function */
    in_use:char;
    locked:char;
   end;

jack_port_shared_t = _jack_port_shared;

 _jack_port_functions = record

    (* Function to initialize port buffer. Cannot be NULL.
     * NOTE: This must take a buffer rather than jack_port_t as it is called
     * in jack_engine_place_buffers() before any port creation.
     * A better solution is to make jack_engine_place_buffers to be type-specific,
     * but this works.
     *)
    //void (*buffer_init)(void *buffer, size_t size);

    (* Function to mixdown multiple inputs to a buffer.  Can be NULL,
     * indicating that multiple input connections are not legal for
     * this data type.
     *)
    //void (*mixdown)(jack_port_t *, jack_nframes_t);

  end;

jack_port_functions_t = _jack_port_functions;


///* Allocated by the client in local memory. */
_jack_port = record
    //void                    **client_segment_base;
    //void                     *mix_buffer;
    //jack_port_type_info_t    *type_info; /* shared memory type info */
    //struct _jack_port_shared *shared;	 /* corresponding shm struct */
    //struct _jack_port        *tied;	 /* locally tied source port */
    //jack_port_functions_t    fptr;
    //pthread_mutex_t          connection_lock;
    //JSList                   *connections;
end;

//jack_port_t = _jack_port;

{/**
 * Get port functions.
 * @param ptid port type id.
 *
 * @return pointer to port type functions or NULL if port type is unknown.
 */}
{/*const*/}
//function jack_get_port_functions(ptid : jack_port_type_id_t ):jack_port_functions_t;





(*  Inline would be cleaner, but it needs to be fast even in
 *  non-optimized code.  jack_output_port_buffer() only handles output
 *  ports.  jack_port_buffer() works for both input and output ports.
 *)


//#define jack_port_buffer(p) \
//  ((void *) ((p)->mix_buffer? (p)->mix_buffer: \
//   *(p)->client_segment_base + (p)->shared->offset))
//#define jack_output_port_buffer(p) \
//  ((void *) (*(p)->client_segment_base + (p)->shared->offset))
//*)

//SHM

  const
     MAX_SERVERS = 8;
  { generally about 16 per server  }
     MAX_SHM_ID = 256;
  { maximum length of server name  }
     JACK_SERVER_NAME_SIZE = 256;
  { shm magic number: "JACK"  }
     JACK_SHM_MAGIC = $4a41434b;
  { NULL SHM index  }
     JACK_SHM_NULL_INDEX = -(1);
  { pseudo SHM index for registry  }
     JACK_SHM_REGISTRY_INDEX = -(2);
  { On Mac OS X, SHM_NAME_MAX is the maximum length of a shared memory
   * segment name (instead of NAME_MAX or PATH_MAX as defined by the
   * standard).
    }

  type

     shm_name_t = char;

     jack_shm_id_t = longint;

  { SHM type  }
  { shared memory type  }
  { POSIX shared memory  }
  { System V shared memory  }


  {*
   * A structure holding information about shared memory allocated by
   * JACK. this persists across invocations of JACK, and can be used by
   * multiple JACK servers.  It contains no pointers and is valid across
   * address spaces.
   *
   * The registry consists of two parts: a header including an array of
   * server names, followed by an array of segment registry entries.
    }
  { process ID  }

     _jack_shm_server = record
          pid : pid_t;
          name : array[0..(JACK_SERVER_NAME_SIZE)-1] of char;
       end;
     jack_shm_server_t = _jack_shm_server;
  { magic number  }
  { JACK protocol version  }
  { shm type  }
  { total registry segment size  }
  { size of header  }
  { size of registry entry  }
  { current server array  }

     _jack_shm_header = record
          magic : longword;
          protocol : word;
          _type : jack_shmtype_t;
          size : jack_shmsize_t;
          hdr_len : jack_shmsize_t;
          entry_len : jack_shmsize_t;
          server : array[0..(MAX_SERVERS)-1] of jack_shm_server_t;
       end;
     jack_shm_header_t = _jack_shm_header;
  { offset into the registry  }
  { PID that created shm segment  }
  { for POSIX unattach  }
  { API specific, see above  }

     _jack_shm_registry = record
          index : jack_shm_registry_index_t;
          allocator : pid_t;
          size : jack_shmsize_t;
          id : jack_shm_id_t;
       end;
     jack_shm_registry_t = _jack_shm_registry;
  { was #define dname def_expr }
  function JACK_SHM_REGISTRY_SIZE : longint;
      { return type might be wrong }

  {*
   * a structure holding information about shared memory
   * allocated by JACK. this version is valid only
   * for a given address space. It contains a pointer
   * indicating where the shared memory has been
   * attached to the address space.
    }
  { offset into the registry  }
  { address where attached  }

  type

     _jack_shm_info = record
          index : jack_shm_registry_index_t;
          attached_at : pointer;
       end;
     jack_shm_info_t = _jack_shm_info;


    Type
    //Pchar  = ^char;
    //PPchar = ^PChar;
    Pjack_shm_info_t  = ^jack_shm_info_t;
    Pjack_shm_registry_index_t  = ^jack_shm_registry_index_t;
  { utility functions used only within JACK  }

  procedure jack_shm_copy_from_registry(_para1:Pjack_shm_info_t; _para2:jack_shm_registry_index_t);cdecl;external External_library name 'jack_shm_copy_from_registry';

  procedure jack_shm_copy_to_registry(_para1:Pjack_shm_info_t; _para2:Pjack_shm_registry_index_t);cdecl;external External_library name 'jack_shm_copy_to_registry';

  procedure jack_release_shm_info(_para1:jack_shm_registry_index_t);cdecl;external External_library name 'jack_release_shm_info';

(* error
static inline char* jack_shm_addr (jack_shm_info_t* si) {
 in declarator_list *)
(* error
	return si->attached_at;
 in declarator_list *)
(* error
}
  { here beginneth the API  }
in declaration at line 98 *)
(* Const before type ignored *)
    procedure jack_unregister_server(server_name:pchar);cdecl;external External_library name 'jack_unregister_server';

(* Const before type ignored *)
    function jack_initialize_shm(server_name:pchar):longint;cdecl;external External_library name 'jack_initialize_shm';

    function jack_cleanup_shm:longint;cdecl;external External_library name 'jack_cleanup_shm';

    function jack_shmalloc(size:jack_shmsize_t; result:pjack_shm_info_t):longint;cdecl;external External_library name 'jack_shmalloc';

    procedure jack_release_shm(_para1:Pjack_shm_info_t);cdecl;external External_library name 'jack_release_shm';

    procedure jack_destroy_shm(_para1:Pjack_shm_info_t);cdecl;external External_library name 'jack_destroy_shm';

    function jack_attach_shm(_para1:Pjack_shm_info_t):longint;cdecl;external External_library name 'jack_attach_shm';

    function jack_resize_shm(_para1:Pjack_shm_info_t; size:jack_shmsize_t):longint;cdecl;external External_library name 'jack_resize_shm';

implementation



  { was #define dname def_expr }
  function JACK_SHM_REGISTRY_SIZE : longint;
      { return type might be wrong }
      begin
         JACK_SHM_REGISTRY_SIZE:=(sizeof(jack_shm_header_t))+((sizeof(jack_shm_registry_t))*MAX_SHM_ID);
      end;

end.
