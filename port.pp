unit port;
interface
(*
    Copyright (C) 2001 Paul Davis
    
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

    $Id: port.h 998 2006-11-22 19:55:56Z joq $
*)

uses pthreads, libc;

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
  JACK_SHM_MAX = High(uint32_t);


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
 jack_port_type_id_t = int32_t;


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
     buffer_scale_factor : int32_t;

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
    {volatile} monitor_requests:uint8_t;

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

{/**
 * Get port functions.
 * @param ptid port type id.
 *
 * @return pointer to port type functions or NULL if port type is unknown.
 */}
{/*const*/}
function jack_get_port_functions(ptid : jack_port_type_id_t ):jack_port_functions_t;


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

jack_port_t = _jack_port;
(*  Inline would be cleaner, but it needs to be fast even in
 *  non-optimized code.  jack_output_port_buffer() only handles output
 *  ports.  jack_port_buffer() works for both input and output ports.
 *)
(*
#define jack_port_buffer(p) \
  ((void *) ((p)->mix_buffer? (p)->mix_buffer: \
   *(p)->client_segment_base + (p)->shared->offset))
#define jack_output_port_buffer(p) \
  ((void *) (*(p)->client_segment_base + (p)->shared->offset))
*)

implementation
end.
