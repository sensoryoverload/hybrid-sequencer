
unit engine;
interface

{
  Automatically converted by H2Pas 1.0.0 from engine.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    jack
    engine.h
}

    const
      External_library='jack'; {Setup as you need}

    Type
    Pchar  = ^char;
    Pjack_driver_desc_t  = ^jack_driver_desc_t;
    Pjack_engine_t  = ^jack_engine_t;
    Pjack_port_internal_t  = ^jack_port_internal_t;
    Pjack_request_t  = ^jack_request_t;
    PJSList  = ^JSList;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  { -*- mode: c; c-file-style: "bsd"; -*-  }
  {
      Copyright (C) 2001-2003 Paul Davis
      
      This program is free software; you can redistribute it and/or modify
      it under the terms of the GNU General Public License as published by
      the Free Software Foundation; either version 2 of the License, or
      (at your option) any later version.
  
      This program is distributed in the hope that it will be useful,
      but WITHOUT ANY WARRANTY; without even the implied warranty of
      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      GNU General Public License for more details.
  
      You should have received a copy of the GNU General Public License
      along with this program; if not, write to the Free Software
      Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
  
      $Id: engine.h 978 2006-06-20 22:15:52Z joq $
   }
{$ifndef __jack_engine_h__}
{$define __jack_engine_h__}  
{$include <jack/jack.h>}
{$include <jack/internal.h>}
{$include <jack/driver_interface.h>}

  type
     _jack_driver = record
         {undefined structure}
       end;

     _jack_client_internal = record
         {undefined structure}
       end;

     _jack_port_internal = record
         {undefined structure}
       end;

  { Structures is allocated by the engine in local memory to keep track
   * of port buffers and connections. 
    }

     jack_port_buffer_info_t = record
          shm_info : ^jack_shm_info_t;
          offset : jack_shmsize_t;
       end;
  { The engine keeps an array of these in its local memory.  }

     _jack_port_internal = record
          shared : ^_jack_port_shared;
          connections : ^JSList;
          buffer_info : ^jack_port_buffer_info_t;
       end;
     jack_port_internal_t = _jack_port_internal;
  { The engine's internal port type structure.  }
  { only lock within server  }
  { list of free buffers  }
  { jack_buffer_info_t array  }

     _jack_port_buffer_list = record
          lock : pthread_mutex_t;
          freelist : ^JSList;
          info : ^jack_port_buffer_info_t;
       end;
     jack_port_buffer_list_t = _jack_port_buffer_list;

  const
     JACKD_WATCHDOG_TIMEOUT = 5000;     
  { The main engine structure in local memory.  }
  { these are "callbacks" made by the driver backend  }
  { "private" sections starts here  }
  { engine serialization -- use precedence for deadlock avoidance  }
  { precedes client_lock  }
  { Time to wait for clients in msecs.  Used when jackd is run
       * without realtime priority enabled.  }
  { info on the shm segment containing this->control  }
  { address-space local port buffer and segment info, 
         indexed by the port type_id 
       }
(* Const before type ignored *)
  { these lists are protected by `client_lock'  }
(* error 
#define JACK_ENGINE_ROLLING_COUNT 32
{$ifdef JACK_USE_MACH_THREADS}
  { specific resources for server/client real-time thread communication  }
{$endif}
 in member_list *)

  type
     _jack_engine = record
       end;

  { public functions  }
(* Const before type ignored *)

  function jack_engine_new(real_time:longint; real_time_priority:longint; do_mlock:longint; do_unlock:longint; server_name:pchar; 
             temporary:longint; verbose:longint; client_timeout:longint; port_max:dword; waitpid:pid_t; 
             frame_time_offset:jack_nframes_t; drivers:pJSList):^jack_engine_t;cdecl;external External_library name 'jack_engine_new';

  procedure jack_engine_delete(_para1:Pjack_engine_t);cdecl;external External_library name 'jack_engine_delete';

  function jack_run(engine:pjack_engine_t):longint;cdecl;external External_library name 'jack_run';

  function jack_wait(engine:pjack_engine_t):longint;cdecl;external External_library name 'jack_wait';

  function jack_engine_load_driver(engine:pjack_engine_t; driver_desc:pjack_driver_desc_t; driver_params:pJSList):longint;cdecl;external External_library name 'jack_engine_load_driver';

  procedure jack_dump_configuration(engine:pjack_engine_t; take_lock:longint);cdecl;external External_library name 'jack_dump_configuration';

  { private engine functions  }
  procedure jack_engine_reset_rolling_usecs(engine:pjack_engine_t);cdecl;external External_library name 'jack_engine_reset_rolling_usecs';

  function internal_client_request(ptr:pointer; request:pjack_request_t):longint;cdecl;external External_library name 'internal_client_request';

  function jack_get_fifo_fd(engine:pjack_engine_t; which_fifo:dword):longint;cdecl;external External_library name 'jack_get_fifo_fd';


    var
       clock_source : jack_timer_type_t;cvar;external;

  function jack_client_internal_by_id(engine:pjack_engine_t; id:jack_client_id_t):^jack_client_internal_t;cdecl;external External_library name 'jack_client_internal_by_id';

(* error 
static inline void jack_lock_graph (jack_engine_t* engine) {
 in declarator_list *)
(* error 
	pthread_mutex_lock (&engine->client_lock);
 in declarator_list *)
(* error 
}
in declaration at line 186 *)
(* error 
	return pthread_mutex_trylock (&engine->client_lock);
 in declarator_list *)
(* error 
}
in declaration at line 192 *)
(* error 
	pthread_mutex_unlock (&engine->client_lock);
 in declarator_list *)
(* error 
}
in declaration at line 198 *)
(* error 
}
    { Internal port handling interfaces for JACK engine.  }
in declaration at line 203 *)
    procedure jack_port_registration_notify(_para1:Pjack_engine_t; _para2:jack_port_id_t; _para3:longint);cdecl;external External_library name 'jack_port_registration_notify';

    procedure jack_port_release(engine:pjack_engine_t; _para2:Pjack_port_internal_t);cdecl;external External_library name 'jack_port_release';

    procedure jack_sort_graph(engine:pjack_engine_t);cdecl;external External_library name 'jack_sort_graph';

{$endif}
    { __jack_engine_h__  }

implementation


end.
