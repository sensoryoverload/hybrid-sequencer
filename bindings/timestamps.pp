
unit timestamps;
interface

{
  Automatically converted by H2Pas 1.0.0 from timestamps.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    jack
    timestamps.h
}

  const
    External_library='jack'; {Setup as you need}

  Type
  Pchar  = ^char;
  PFILE  = ^FILE;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  {
      Copyright (C) 2002 Paul Davis
      
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
  
      $Id: timestamps.h 264 2002-11-01 15:41:08Z pbd $
   }
{$ifndef __jack_timestamps_h__}
{$define __jack_timestamps_h__}  
{$include <stdio.h>}
{ C++ extern C conditionnal removed }

  procedure jack_init_timestamps(howmany:dword);cdecl;external External_library name 'jack_init_timestamps';

(* Const before type ignored *)
  procedure jack_timestamp(what:pchar);cdecl;external External_library name 'jack_timestamp';

  procedure jack_dump_timestamps(out:pFILE);cdecl;external External_library name 'jack_dump_timestamps';

  procedure jack_reset_timestamps;cdecl;external External_library name 'jack_reset_timestamps';

{ C++ end of extern C conditionnal removed }
{$endif}
  { __jack_timestamps_h__  }

implementation


end.
