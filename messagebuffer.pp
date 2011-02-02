
unit messagebuffer;
interface

{
  Automatically converted by H2Pas 1.0.0 from messagebuffer.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    jack
    messagebuffer.h
}

    const
      External_library='jack'; {Setup as you need}

    Type
    Pchar  = ^char;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  {
   * messagebuffer.h -- realtime-safe message interface for jackd.
   *
   *  This function is included in libjack so backend drivers can use
   *  it, *not* for external client processes.  The VERBOSE() and
   *  MESSAGE() macros are realtime-safe.
    }
  {
   *  Copyright (C) 2004 Rui Nuno Capela, Steve Harris
   *  
   *  This program is free software; you can redistribute it and/or modify
   *  it under the terms of the GNU Lesser General Public License as published by
   *  the Free Software Foundation; either version 2.1 of the License, or
   *  (at your option) any later version.
   *  
   *  This program is distributed in the hope that it will be useful,
   *  but WITHOUT ANY WARRANTY; without even the implied warranty of
   *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   *  GNU Lesser General Public License for more details.
   *  
   *  You should have received a copy of the GNU Lesser General Public License
   *  along with this program; if not, write to the Free Software 
   *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
   *
   *  $Id: messagebuffer.h 843 2004-12-13 17:20:39Z trutkin $
    }
{$ifndef __jack_messagebuffer_h__}
{$define __jack_messagebuffer_h__}  
(* error 
#define MESSAGE(fmt,args...) jack_messagebuffer_add(fmt , ##args)
in define line 32 *)
(* error 
#define VERBOSE(engine,fmt,args...)	\
in define line 35 *)

    procedure jack_messagebuffer_init;cdecl;external External_library name 'jack_messagebuffer_init';

    procedure jack_messagebuffer_exit;cdecl;external External_library name 'jack_messagebuffer_exit';

(* Const before type ignored *)
    procedure jack_messagebuffer_add(fmt:pchar; args:array of const);cdecl;external External_library name 'jack_messagebuffer_add';

    procedure jack_messagebuffer_add(fmt:pchar);cdecl;external External_library name 'jack_messagebuffer_add';

{$endif}
    { __jack_messagebuffer_h__  }

implementation


end.
