
unit hardware;
interface

{
  Automatically converted by H2Pas 1.0.0 from hardware.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    jack
    hardware.h
}

  const
    External_library='jack'; {Setup as you need}

  Type
  P_jack_hardware  = ^_jack_hardware;
  Pjack_port_t  = ^jack_port_t;
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
  
      $Id: hardware.h 350 2003-03-20 20:49:23Z pbd $
   }
{$ifndef __jack_hardware_h__}
{$define __jack_hardware_h__}  
{$include <jack/types.h>}

  type

     SampleClockMode = (AutoSync,WordClock,ClockMaster);

     Capabilities = (Cap_HardwareMonitoring := $1,Cap_AutoSync := $2,
       Cap_WordClock := $4,Cap_ClockMaster := $8,
       Cap_ClockLockReporting := $10,Cap_HardwareMetering := $20
       );
     _jack_hardware = record
         {undefined structure}
       end;


     JackHardwareReleaseFunction = procedure (_para1:P_jack_hardware);cdecl;

     JackHardwareSetInputMonitorMaskFunction = function (_para1:P_jack_hardware; _para2:dword):longint;cdecl;

     JackHardwareChangeSampleClockFunction = function (_para1:P_jack_hardware; _para2:SampleClockMode):longint;cdecl;

     JackHardwareGetHardwarePeak = function (port:pjack_port_t; frames:jack_nframes_t):double;cdecl;

     JackHardwareGetHardwarePower = function (port:pjack_port_t; frames:jack_nframes_t):double;cdecl;

     _jack_hardware = record
          capabilities : dword;
          input_monitor_mask : dword;
          change_sample_clock : JackHardwareChangeSampleClockFunction;
          set_input_monitor_mask : JackHardwareSetInputMonitorMaskFunction;
          release : JackHardwareReleaseFunction;
          get_hardware_peak : JackHardwareGetHardwarePeak;
          get_hardware_power : JackHardwareGetHardwarePower;
          private : pointer;
       end;
     jack_hardware_t = _jack_hardware;

  function jack_hardware_new:^jack_hardware_t;cdecl;external External_library name 'jack_hardware_new';

{$endif}
  { __jack_hardware_h__  }

implementation


end.
