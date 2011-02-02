
unit statistics;
interface

{
  Automatically converted by H2Pas 1.0.0 from statistics.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    jack
    statistics.h
}

  const
    External_library='jack'; {Setup as you need}

  Type
  Pjack_client_t  = ^jack_client_t;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  {
   *  Copyright (C) 2004 Rui Nuno Capela, Lee Revell
   *  
   *  This program is free software; you can redistribute it and/or
   *  modify it under the terms of the GNU Lesser General Public License
   *  as published by the Free Software Foundation; either version 2.1
   *  of the License, or (at your option) any later version.
   *  
   *  This program is distributed in the hope that it will be useful,
   *  but WITHOUT ANY WARRANTY; without even the implied warranty of
   *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   *  Lesser General Public License for more details.
   *  
   *  You should have received a copy of the GNU Lesser General Public
   *  License along with this program; if not, write to the Free
   *  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   *  02111-1307, USA.
   *
   *  $Id: statistics.h 860 2004-12-31 23:57:21Z joq $
    }
{$ifndef __statistics_h__}
{$define __statistics_h__}  
{ C++ extern C conditionnal removed }
{$include <jack/types.h>}
  {*
   * @return the maximum delay reported by the backend since
   * startup or reset.  When compared to the period size in usecs, this
   * can be used to estimate the ideal period size for a given setup.
    }

  function jack_get_max_delayed_usecs(client:pjack_client_t):double;cdecl;external External_library name 'jack_get_max_delayed_usecs';

  {*
   * @return the delay in microseconds due to the most recent XRUN
   * occurrence.  This probably only makes sense when called from a @ref
   * JackXRunCallback defined using jack_set_xrun_callback().
    }
  function jack_get_xrun_delayed_usecs(client:pjack_client_t):double;cdecl;external External_library name 'jack_get_xrun_delayed_usecs';

  {*
   * Reset the maximum delay counter.  This would be useful
   * to estimate the effect that a change to the configuration of a running
   * system (e.g. toggling kernel preemption) has on the delay
   * experienced by JACK, without having to restart the JACK engine.
    }
  procedure jack_reset_max_delayed_usecs(client:pjack_client_t);cdecl;external External_library name 'jack_reset_max_delayed_usecs';

{ C++ end of extern C conditionnal removed }
{$endif}
  { __statistics_h__  }

implementation


end.
