{
  Copyright (C) 2009 Robbert Latumahina

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

  rubberband.pas


  Based on the Rubberband library header:

    Rubber Band
    An audio time-stretching and pitch-shifting library.
    Copyright 2007-2008 Chris Cannam.
}


unit librubberband;

interface

  const
    External_library = 'rubberband'; {Setup as you need}

  Type
  PSingle  = ^single;
  PPSingle = ^PSingle;
//{$IFDEF FPC}
//{$PACKRECORDS C}
//{$ENDIF}


  { -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4:  }
  {
      Rubber Band
      An audio time-stretching and pitch-shifting library.
      Copyright 2007-2008 Chris Cannam.
      
      This program is free software; you can redistribute it and/or
      modify it under the terms of the GNU General Public License as
      published by the Free Software Foundation; either version 2 of the
      License, or (at your option) any later version.  See the file
      COPYING included with this distribution for more information.
   }
//{$ifndef _RUBBERBAND_C_API_H_}
//{$define _RUBBERBAND_C_API_H_}

  const
     RUBBERBAND_VERSION = '1.2.0-gpl';     
     RUBBERBAND_API_MAJOR_VERSION = 2;     
     RUBBERBAND_API_MINOR_VERSION = 0;     
  {*
   * This is a C-linkage interface to the Rubber Band time stretcher.
   * 
   * This is a wrapper interface: the primary interface is in C++ and is
   * defined and documented in RubberBandStretcher.h.  The library
   * itself is implemented in C++, and requires C++ standard library
   * support even when using the C-linkage API.
   *
   * Please see RubberBandStretcher.h for documentation.
   *
   * If you are writing to the C++ API, do not include this header.
    }

  const
     //RubberBandOption = (
       RubberBandOptionProcessOffline = $00000000;
       RubberBandOptionProcessRealTime = $00000001;
       RubberBandOptionStretchElastic = $00000000;
       RubberBandOptionStretchPrecise = $00000010;
       RubberBandOptionTransientsCrisp = $00000000;
       RubberBandOptionTransientsMixed = $00000100;
       RubberBandOptionTransientsSmooth = $00000200;
       RubberBandOptionPhaseLaminar = $00000000;
       RubberBandOptionPhaseIndependent = $00002000;
       RubberBandOptionThreadingAuto = $00000000;
       RubberBandOptionThreadingNever = $00010000;
       RubberBandOptionThreadingAlways = $00020000;
       RubberBandOptionWindowStandard = $00000000;
       RubberBandOptionWindowShort = $00100000;
       RubberBandOptionWindowLong = $00200000;
       RubberBandOptionFormantShifted = $00000000;
       RubberBandOptionFormantPreserved = $01000000;
       RubberBandOptionPitchHighQuality = $00000000;
       RubberBandOptionPitchHighSpeed = $02000000;
       RubberBandOptionPitchHighConsistency = $04000000;
     //  );

  type
     RubberBandOptions = longint;
     RubberBandState_ = record
         {undefined structure}
       end;


  RubberBandState = ^RubberBandState_;

  function rubberband_new(sampleRate:dword; channels:dword; options:RubberBandOptions; initialTimeRatio:double; initialPitchScale:double):RubberBandState;cdecl;external External_library name 'rubberband_new';

  procedure rubberband_delete(_para1:RubberBandState);cdecl;external External_library name 'rubberband_delete';

  procedure rubberband_reset(_para1:RubberBandState);cdecl;external External_library name 'rubberband_reset';

  procedure rubberband_set_time_ratio(_para1:RubberBandState; ratio:double);cdecl;external External_library name 'rubberband_set_time_ratio';

  procedure rubberband_set_pitch_scale(_para1:RubberBandState; scale:double);cdecl;external External_library name 'rubberband_set_pitch_scale';

  function rubberband_get_time_ratio(_para1:RubberBandState):double;cdecl;external External_library name 'rubberband_get_time_ratio';

  function rubberband_get_pitch_scale(_para1:RubberBandState):double;cdecl;external External_library name 'rubberband_get_pitch_scale';

  function rubberband_get_latency(_para1:RubberBandState):dword;cdecl;external External_library name 'rubberband_get_latency';

  procedure rubberband_set_transients_option(_para1:RubberBandState; options:RubberBandOptions);cdecl;external External_library name 'rubberband_set_transients_option';

  procedure rubberband_set_phase_option(_para1:RubberBandState; options:RubberBandOptions);cdecl;external External_library name 'rubberband_set_phase_option';

  procedure rubberband_set_formant_option(_para1:RubberBandState; options:RubberBandOptions);cdecl;external External_library name 'rubberband_set_formant_option';

  procedure rubberband_set_pitch_option(_para1:RubberBandState; options:RubberBandOptions);cdecl;external External_library name 'rubberband_set_pitch_option';

  procedure rubberband_set_expected_input_duration(_para1:RubberBandState; samples:dword);cdecl;external External_library name 'rubberband_set_expected_input_duration';

  function rubberband_get_samples_required(_para1:RubberBandState):dword;cdecl;external External_library name 'rubberband_get_samples_required';

  procedure rubberband_set_max_process_size(_para1:RubberBandState; samples:dword);cdecl;external External_library name 'rubberband_set_max_process_size';

  procedure rubberband_study(_para1:RubberBandState; input:PPSingle; samples:dword; final:longint);cdecl;external External_library name 'rubberband_study';

  procedure rubberband_process(_para1:RubberBandState; input:PPSingle; samples:dword; final:longint);cdecl;external External_library name 'rubberband_process';

  function rubberband_available(_para1:RubberBandState):longint;cdecl;external External_library name 'rubberband_available';

  function rubberband_retrieve(_para1:RubberBandState; output:PPSingle; samples:dword):dword;cdecl;external External_library name 'rubberband_retrieve';

  function rubberband_get_channel_count(_para1:RubberBandState):dword;cdecl;external External_library name 'rubberband_get_channel_count';

  procedure rubberband_calculate_stretch(_para1:RubberBandState);cdecl;external External_library name 'rubberband_calculate_stretch';

  procedure rubberband_set_debug_level(_para1:RubberBandState; level:longint);cdecl;external External_library name 'rubberband_set_debug_level';

  procedure rubberband_set_default_debug_level(level:longint);cdecl;external External_library name 'rubberband_set_default_debug_level';

//{$endif}

implementation


end.
