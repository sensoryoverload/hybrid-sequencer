unit uslv2values;

interface

uses
  uslv2value, uslv2types;

const
  External_library='slv2'; {Setup as you need}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  { SLV2
   * Copyright (C) 2007 Dave Robillard <http://drobilla.net>
   *  
   * This library is free software; you can redistribute it and/or modify it
   * under the terms of the GNU General Public License as published by the Free
   * Software Foundation; either version 2 of the License, or (at your option)
   * any later version.
   *
   * This library is distributed in the hope that it will be useful, but WITHOUT
   * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   * for more details.
   *
   * You should have received a copy of the GNU General Public License along
   * with this program; if not, write to the Free Software Foundation, Inc.,
   * 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
    }


{ C++ extern C conditionnal removed }
  {* \defgroup values Collections of values
   *
   * SLV2Values is an ordered collection of typed values which is fast for random
   * access by index (i.e. a fancy array).
   *
   * @
    }
  {* Allocate a new, empty SLV2Values
    }

  function slv2_values_new:SLV2Values;cdecl;external External_library name 'slv2_values_new';

  {* Get the number of elements in a string list.
    }
  function slv2_values_size(list:SLV2Values):dword;cdecl;external External_library name 'slv2_values_size';

  {* Get a string from a string list at the given index.
   *
   * @return the element at \a index, or NULL if index is out of range.
   *
   * Time = O(1)
    }
  function slv2_values_get_at(list:SLV2Values; index:dword):SLV2Value;cdecl;external External_library name 'slv2_values_get_at';

  {* Return whether \a list contains \a string.
   *
   * Time = O(n)
    }
  function slv2_values_contains(list:SLV2Values; value:SLV2Value):boolean;cdecl;external External_library name 'slv2_values_contains';

  {* Free a string list.
    }
  procedure slv2_values_free(_para1:SLV2Values);cdecl;external External_library name 'slv2_values_free';

implementation


end.
