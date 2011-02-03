
unit driver;
interface

{
  Automatically converted by H2Pas 1.0.0 from driver.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    jack
    driver.h
}

  const
    External_library='jack'; {Setup as you need}

  Type
  P_jack_driver  = ^_jack_driver;
  P_jack_driver_nt  = ^_jack_driver_nt;
  P_jack_engine  = ^_jack_engine;
  Pchar  = ^char;
  Pjack_driver_nt_t  = ^jack_driver_nt_t;
  Pjack_driver_t  = ^jack_driver_t;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  {
      Copyright (C) 2001 Paul Davis 
  
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
  
      $Id: driver.h 945 2006-05-04 15:14:45Z pbd $
   }
{$ifndef __jack_driver_h__}
{$define __jack_driver_h__}  
{$include <pthread.h>}
{$include <jack/types.h>}
{$include <jack/port.h>}
{$include <jack/driver_interface.h>}

  type

     gain_t = double;

     channel_t = dword;

     ClockSyncStatus = (Lock := $1,NoLock := $2,Sync := $4,NoSync := $8
       );

     ClockSyncListenerFunction = procedure (_para1:channel_t; _para2:ClockSyncStatus; _para3:pointer);cdecl;

     ClockSyncListener = record
          id : dword;
          _function : ClockSyncListenerFunction;
          arg : pointer;
       end;
     _jack_engine = record
         {undefined structure}
       end;

     _jack_driver = record
         {undefined structure}
       end;


     JackDriverAttachFunction = function (_para1:P_jack_driver; _para2:P_jack_engine):longint;cdecl;

     JackDriverDetachFunction = function (_para1:P_jack_driver; _para2:P_jack_engine):longint;cdecl;

     JackDriverReadFunction = function (_para1:P_jack_driver; nframes:jack_nframes_t):longint;cdecl;

     JackDriverWriteFunction = function (_para1:P_jack_driver; nframes:jack_nframes_t):longint;cdecl;

     JackDriverNullCycleFunction = function (_para1:P_jack_driver; nframes:jack_nframes_t):longint;cdecl;

     JackDriverStopFunction = function (_para1:P_jack_driver):longint;cdecl;

     JackDriverStartFunction = function (_para1:P_jack_driver):longint;cdecl;

     JackDriverBufSizeFunction = function (_para1:P_jack_driver; nframes:jack_nframes_t):longint;cdecl;
  { 
     Call sequence summary:
  
       1) engine loads driver via runtime dynamic linking
  	 - calls jack_driver_load
  	 - we call dlsym for "driver_initialize" and execute it
       2) engine attaches to driver
       3) engine starts driver
       4) driver runs its own thread, calling
           while () 
            driver->wait ();
  	  driver->engine->run_cycle ()
           
       5) engine stops driver
       6) engine detaches from driver
       7) engine calls driver `finish' routine
  
       Note that stop/start may be called multiple times in the event of an
       error return from the `wait' function.
   }
  { The _jack_driver structure fields are included at the beginning of
     each driver-specific structure using the JACK_DRIVER_DECL macro,
     which is defined below.  The comments that follow describe each
     common field.
    
     The driver should set this to be the interval it expects to elapse
     between returning from the `wait' function. if set to zero, it
     implies that the driver does not expect regular periodic wakeups.
  
      jack_time_t period_usecs;
  
  
     The driver should set this within its "wait" function to indicate
     the UST of the most recent determination that the engine cycle
     should run. it should not be set if the "extra_fd" argument of
     the wait function is set to a non-zero value.
  
      jack_time_t last_wait_ust;
  
  
     These are not used by the driver.  They should not be written to or
     modified in any way
   
      void *handle;
      struct _jack_internal_client *internal_client;
  
     This should perform any cleanup associated with the driver. it will
     be called when jack server process decides to get rid of the
     driver. in some systems, it may not be called at all, so the driver
     should never rely on a call to this. it can set it to NULL if
     it has nothing do do.
  
      void (*finish)(struct _jack_driver *);
  
  
     The JACK engine will call this when it wishes to attach itself to
     the driver. the engine will pass a pointer to itself, which the driver
     may use in anyway it wishes to. the driver may assume that this
     is the same engine object that will make `wait' calls until a
     `detach' call is made.
  
      JackDriverAttachFunction attach;
  
  
     The JACK engine will call this when it is finished using a driver.
  
      JackDriverDetachFunction detach;
  
  
     The JACK engine will call this when it wants to wait until the 
     driver decides that its time to process some data. the driver returns
     a count of the number of audioframes that can be processed. 
  
     it should set the variable pointed to by `status' as follows:
  
     zero: the wait completed normally, processing may begin
     negative: the wait failed, and recovery is not possible
     positive: the wait failed, and the driver stopped itself.
  	       a call to `start' will return the driver to	
  	       a correct and known state.
  
     the driver should also fill out the `delayed_usecs' variable to
     indicate any delay in its expected periodic execution. for example,
     if it discovers that its return from poll(2) is later than it
     expects it to be, it would place an estimate of the delay
     in this variable. the engine will use this to decide if it 
     plans to continue execution.
  
      JackDriverWaitFunction wait;
  
  
     The JACK engine will call this to ask the driver to move
     data from its inputs to its output port buffers. it should
     return 0 to indicate successful completion, negative otherwise. 
   
     This function will always be called after the wait function (above).
  
      JackDriverReadFunction read;
  
  
     The JACK engine will call this to ask the driver to move
     data from its input port buffers to its outputs. it should
     return 0 to indicate successful completion, negative otherwise. 
   
     this function will always be called after the read function (above).
  
      JackDriverWriteFunction write;
  
  
     The JACK engine will call this after the wait function (above) has
     been called, but for some reason the engine is unable to execute
     a full "cycle". the driver should do whatever is necessary to
     keep itself running correctly, but cannot reference ports
     or other JACK data structures in any way.
  
      JackDriverNullCycleFunction null_cycle;
  
      
     The engine will call this when it plans to stop calling the `wait'
     function for some period of time. the driver should take
     appropriate steps to handle this (possibly no steps at all).
     NOTE: the driver must silence its capture buffers (if any)
     from within this function or the function that actually
     implements the change in state.
  
      JackDriverStopFunction stop;
  
  
     The engine will call this to let the driver know that it plans
     to start calling the `wait' function on a regular basis. the driver
     should take any appropriate steps to handle this (possibly no steps
     at all). NOTE: The driver may wish to silence its playback buffers
     (if any) from within this function or the function that actually
     implements the change in state.
     
      JackDriverStartFunction start;
  
     The engine will call this to let the driver know that some client
     has requested a new buffer size.  The stop function will be called
     prior to this, and the start function after this one has returned.
  
      JackDriverBufSizeFunction bufsize;
   }
  { define the fields here...  }(* error 
#define JACK_DRIVER_DECL \
  { expand the macro  }
 in member_list *)

     _jack_driver = record
       end;
     jack_driver_t = _jack_driver;

     JackDriverDescFunction = jack_driver_desc_t;

  procedure jack_driver_init(_para1:Pjack_driver_t);cdecl;external External_library name 'jack_driver_init';

  procedure jack_driver_release(_para1:Pjack_driver_t);cdecl;external External_library name 'jack_driver_release';

  function jack_driver_load(argc:longint; argv:Ppchar):^jack_driver_t;cdecl;external External_library name 'jack_driver_load';

  procedure jack_driver_unload(_para1:Pjack_driver_t);cdecl;external External_library name 'jack_driver_unload';

  {***************************
   *** Non-Threaded Drivers ***
   *************************** }
  { 
     Call sequence summary:
  
       1) engine loads driver via runtime dynamic linking
  	 - calls jack_driver_load
  	 - we call dlsym for "driver_initialize" and execute it
           - driver_initialize calls jack_driver_nt_init
       2) nt layer attaches to driver
       3) nt layer starts driver
       4) nt layer runs a thread, calling
           while () 
             driver->nt_run_ctcle();
           
       5) nt layer stops driver
       6) nt layer detaches driver
       7) engine calls driver `finish' routine which calls jack_driver_nt_finish
  
       Note that stop/start may be called multiple times in the event of an
       error return from the `wait' function.
  
  
   }

  type
     _jack_driver_nt = record
         {undefined structure}
       end;


     JackDriverNTAttachFunction = function (_para1:P_jack_driver_nt):longint;cdecl;

     JackDriverNTDetachFunction = function (_para1:P_jack_driver_nt):longint;cdecl;

     JackDriverNTStopFunction = function (_para1:P_jack_driver_nt):longint;cdecl;

     JackDriverNTStartFunction = function (_para1:P_jack_driver_nt):longint;cdecl;

     JackDriverNTBufSizeFunction = function (_para1:P_jack_driver_nt; nframes:jack_nframes_t):longint;cdecl;

     JackDriverNTRunCycleFunction = function (_para1:P_jack_driver_nt):longint;cdecl;
(* error 
#define JACK_DRIVER_NT_DECL \
 in member_list *)

     _jack_driver_nt = record
       end;
     jack_driver_nt_t = _jack_driver_nt;

  procedure jack_driver_nt_init(driver:pjack_driver_nt_t);cdecl;external External_library name 'jack_driver_nt_init';

  procedure jack_driver_nt_finish(driver:pjack_driver_nt_t);cdecl;external External_library name 'jack_driver_nt_finish';

{$endif}
  { __jack_driver_h__  }

implementation


end.
