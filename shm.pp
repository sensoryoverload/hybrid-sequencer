
unit shm;
interface
uses types;
{
  Automatically converted by H2Pas 1.0.0 from shm.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    jack
    shm.h
}

    const
      External_library='jack'; {Setup as you need}


{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{$ifndef __jack_shm_h__}
{$define __jack_shm_h__}  
//{$include <limits.h>}
//{$include <sys/types.h>}
//{$include <jack/types.h>}
  { maximum concurrent servers  }

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
{$ifdef USE_POSIX_SHM}
{$ifndef SHM_NAME_MAX}

  const
     SHM_NAME_MAX = NAME_MAX;     
{$endif}

  type

     shm_name_t = char;

     jack_shm_id_t = shm_name_t;
{$else}
  { System V SHM  }

  type

     jack_shm_id_t = longint;
{$endif}
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
          magic : uint32_t;
          protocol : uint16_t;
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

{$endif}
    { __jack_shm_h__  }

implementation

  { was #define dname def_expr }
  function JACK_SHM_REGISTRY_SIZE : longint;
      { return type might be wrong }
      begin
         JACK_SHM_REGISTRY_SIZE:=(sizeof(jack_shm_header_t))+((sizeof(jack_shm_registry_t))*MAX_SHM_ID);
      end;


end.
