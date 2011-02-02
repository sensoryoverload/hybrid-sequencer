
unit pool;
interface

{
  Automatically converted by H2Pas 1.0.0 from pool.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    jack
    pool.h
}

  const
    External_library='jack'; {Setup as you need}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  {
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
  
      $Id: pool.h 11 2001-11-13 04:32:31Z pbd $
   }
{$ifndef __jack_pool_h__}
{$define __jack_pool_h__}  
{$include <sys/types.h>}

  function jack_pool_alloc(bytes:size_t):pointer;cdecl;external External_library name 'jack_pool_alloc';

  procedure jack_pool_release(_para1:pointer);cdecl;external External_library name 'jack_pool_release';

{$endif}
  { __jack_pool_h__  }

implementation


end.
