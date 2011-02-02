
  const
    External_library='jack'; {Setup as you need}

  Type

  Pjack_client_t  = ^jack_client_t;
  Pjack_port_t  = ^jack_port_t;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



{$ifndef __jack_h__}





(*  before  ignored *)

  function jack_client_open(client_name:pchar; options:jack_options_t; var status:jack_status_t; args:array of const):^jack_client_t;cdecl;external External_library name 'jack_client_open';

  function jack_client_open(client_name:pchar; options:jack_options_t; var status:jack_status_t):^jack_client_t;cdecl;external External_library name 'jack_client_open';


(*  before  ignored *)
  function jack_client_new(client_name:pchar):^jack_client_t;cdecl;external External_library name 'jack_client_new';


  function jack_client_close(var client:jack_client_t):longint;cdecl;external External_library name 'jack_client_close';


  function jack_client_name_size:longint;cdecl;external External_library name 'jack_client_name_size';


  function jack_get_client_name(var client:jack_client_t):^char;cdecl;external External_library name 'jack_get_client_name';


(*  before  ignored *)
(*  before  ignored *)
(*  before  ignored *)
  function jack_internal_client_new(client_name:pchar; load_name:pchar; load_init:pchar):longint;cdecl;external External_library name 'jack_internal_client_new';


(*  before  ignored *)
  procedure jack_internal_client_close(client_name:pchar);cdecl;external External_library name 'jack_internal_client_close';


  function jack_is_realtime(var client:jack_client_t):longint;cdecl;external External_library name 'jack_is_realtime';


  procedure jack_on_shutdown(var client:jack_client_t; _function:procedure (var arg:pointer); var arg:pointer);cdecl;external External_library name 'jack_on_shutdown';


  function jack_set_process_callback(var client:jack_client_t; process_callback:JackProcessCallback; var arg:pointer):longint;cdecl;external External_library name 'jack_set_process_callback';


  function jack_set_thread_init_callback(var client:jack_client_t; thread_init_callback:JackThreadInitCallback; var arg:pointer):longint;cdecl;external External_library name 'jack_set_thread_init_callback';


  function jack_set_freewheel_callback(var client:jack_client_t; freewheel_callback:JackFreewheelCallback; var arg:pointer):longint;cdecl;external External_library name 'jack_set_freewheel_callback';


  function jack_set_freewheel(var client:jack_client_t; onoff:longint):longint;cdecl;external External_library name 'jack_set_freewheel';


  function jack_set_buffer_size(var client:jack_client_t; nframes:jack_nframes_t):longint;cdecl;external External_library name 'jack_set_buffer_size';


  function jack_set_buffer_size_callback(var client:jack_client_t; bufsize_callback:JackBufferSizeCallback; var arg:pointer):longint;cdecl;external External_library name 'jack_set_buffer_size_callback';


  function jack_set_sample_rate_callback(var client:jack_client_t; srate_callback:JackSampleRateCallback; var arg:pointer):longint;cdecl;external External_library name 'jack_set_sample_rate_callback';


  function jack_set_port_registration_callback(_para1:Pjack_client_t; registration_callback:JackPortRegistrationCallback; var arg:pointer):longint;cdecl;external External_library name 'jack_set_port_registration_callback';


  function jack_set_graph_order_callback(_para1:Pjack_client_t; graph_callback:JackGraphOrderCallback; _para3:pointer):longint;cdecl;external External_library name 'jack_set_graph_order_callback';


  function jack_set_xrun_callback(_para1:Pjack_client_t; xrun_callback:JackXRunCallback; var arg:pointer):longint;cdecl;external External_library name 'jack_set_xrun_callback';


  function jack_activate(var client:jack_client_t):longint;cdecl;external External_library name 'jack_activate';


  function jack_deactivate(var client:jack_client_t):longint;cdecl;external External_library name 'jack_deactivate';


(*  before  ignored *)
(*  before  ignored *)
  function jack_port_register(var client:jack_client_t; port_name:pchar; port_type:pchar; flags:dword; buffer_size:dword):^jack_port_t;cdecl;external External_library name 'jack_port_register';


  function jack_port_unregister(_para1:Pjack_client_t; _para2:Pjack_port_t):longint;cdecl;external External_library name 'jack_port_unregister';


  function jack_port_get_buffer(_para1:Pjack_port_t; _para2:jack_nframes_t):pointer;cdecl;external External_library name 'jack_port_get_buffer';


(*  before  ignored *)
(*  before  ignored *)
  function jack_port_name(var port:jack_port_t):^char;cdecl;external External_library name 'jack_port_name';


(*  before  ignored *)
(*  before  ignored *)
  function jack_port_short_name(var port:jack_port_t):^char;cdecl;external External_library name 'jack_port_short_name';


(*  before  ignored *)
  function jack_port_flags(var port:jack_port_t):longint;cdecl;external External_library name 'jack_port_flags';


(*  before  ignored *)
(*  before  ignored *)
  function jack_port_type(var port:jack_port_t):^char;cdecl;external External_library name 'jack_port_type';


(*  before  ignored *)
(*  before  ignored *)
  function jack_port_is_mine(_para1:Pjack_client_t; var port:jack_port_t):longint;cdecl;external External_library name 'jack_port_is_mine';


(*  before  ignored *)
  function jack_port_connected(var port:jack_port_t):longint;cdecl;external External_library name 'jack_port_connected';


(*  before  ignored *)
(*  before  ignored *)
  function jack_port_connected_to(var port:jack_port_t; port_name:pchar):longint;cdecl;external External_library name 'jack_port_connected_to';

(*  before  ignored *)
(*  before  ignored *)
  function jack_port_get_connections(var port:jack_port_t):^^char;cdecl;external External_library name 'jack_port_get_connections';

(*  before  ignored *)
(*  before  ignored *)
(*  before  ignored *)
  function jack_port_get_all_connections(var client:jack_client_t; var port:jack_port_t):^^char;cdecl;external External_library name 'jack_port_get_all_connections';


  function jack_port_tie(var src:jack_port_t; var dst:jack_port_t):longint;cdecl;external External_library name 'jack_port_tie';


  function jack_port_untie(var port:jack_port_t):longint;cdecl;external External_library name 'jack_port_untie';


  function jack_port_lock(_para1:Pjack_client_t; _para2:Pjack_port_t):longint;cdecl;external External_library name 'jack_port_lock';


  function jack_port_unlock(_para1:Pjack_client_t; _para2:Pjack_port_t):longint;cdecl;external External_library name 'jack_port_unlock';


  function jack_port_get_latency(var port:jack_port_t):jack_nframes_t;cdecl;external External_library name 'jack_port_get_latency';


  function jack_port_get_total_latency(_para1:Pjack_client_t; var port:jack_port_t):jack_nframes_t;cdecl;external External_library name 'jack_port_get_total_latency';


  procedure jack_port_set_latency(_para1:Pjack_port_t; _para2:jack_nframes_t);cdecl;external External_library name 'jack_port_set_latency';


  function jack_recompute_total_latencies(_para1:Pjack_client_t):longint;cdecl;external External_library name 'jack_recompute_total_latencies';


(*  before  ignored *)
  function jack_port_set_name(var port:jack_port_t; port_name:pchar):longint;cdecl;external External_library name 'jack_port_set_name';


  function jack_port_request_monitor(var port:jack_port_t; onoff:longint):longint;cdecl;external External_library name 'jack_port_request_monitor';


(*  before  ignored *)
  function jack_port_request_monitor_by_name(var client:jack_client_t; port_name:pchar; onoff:longint):longint;cdecl;external External_library name 'jack_port_request_monitor_by_name';


  function jack_port_ensure_monitor(var port:jack_port_t; onoff:longint):longint;cdecl;external External_library name 'jack_port_ensure_monitor';


  function jack_port_monitoring_input(var port:jack_port_t):longint;cdecl;external External_library name 'jack_port_monitoring_input';


(*  before  ignored *)
(*  before  ignored *)
  function jack_connect(_para1:Pjack_client_t; source_port:pchar; destination_port:pchar):longint;cdecl;external External_library name 'jack_connect';


(*  before  ignored *)
(*  before  ignored *)
  function jack_disconnect(_para1:Pjack_client_t; source_port:pchar; destination_port:pchar):longint;cdecl;external External_library name 'jack_disconnect';


  function jack_port_disconnect(_para1:Pjack_client_t; _para2:Pjack_port_t):longint;cdecl;external External_library name 'jack_port_disconnect';


  function jack_port_name_size:longint;cdecl;external External_library name 'jack_port_name_size';


  function jack_port_type_size:longint;cdecl;external External_library name 'jack_port_type_size';


  function jack_get_sample_rate(_para1:Pjack_client_t):jack_nframes_t;cdecl;external External_library name 'jack_get_sample_rate';


  function jack_get_buffer_size(_para1:Pjack_client_t):jack_nframes_t;cdecl;external External_library name 'jack_get_buffer_size';


(*  before  ignored *)
(*  before  ignored *)
(*  before  ignored *)
  function jack_get_ports(_para1:Pjack_client_t; port_name_pattern:pchar; type_name_pattern:pchar; flags:dword):^^char;cdecl;external External_library name 'jack_get_ports';


(*  before  ignored *)
  function jack_port_by_name(_para1:Pjack_client_t; port_name:pchar):^jack_port_t;cdecl;external External_library name 'jack_port_by_name';


  function jack_port_by_id(var client:jack_client_t; port_id:jack_port_id_t):^jack_port_t;cdecl;external External_library name 'jack_port_by_id';


  function jack_engine_takeover_timebase(_para1:Pjack_client_t):longint;cdecl;external External_library name 'jack_engine_takeover_timebase';


(*  before  ignored *)
  function jack_frames_since_cycle_start(_para1:Pjack_client_t):jack_nframes_t;cdecl;external External_library name 'jack_frames_since_cycle_start';


(*  before  ignored *)
  function jack_frame_time(_para1:Pjack_client_t):jack_nframes_t;cdecl;external External_library name 'jack_frame_time';


(*  before  ignored *)
  function jack_last_frame_time(var client:jack_client_t):jack_nframes_t;cdecl;external External_library name 'jack_last_frame_time';


  function jack_cpu_load(var client:jack_client_t):double;cdecl;external External_library name 'jack_cpu_load';


  function jack_client_thread_id(_para1:Pjack_client_t):pthread_t;cdecl;external External_library name 'jack_client_thread_id';


(*  before  ignored *)

    var
       jack_error_callback : procedure (msg:pchar);cvar;external;

(*  before  ignored *)

  procedure jack_set_error_function(func:procedure (_para1:Pchar));cdecl;external External_library name 'jack_set_error_function';

{ C++ end of extern C conditionnal removed }
{$endif}


