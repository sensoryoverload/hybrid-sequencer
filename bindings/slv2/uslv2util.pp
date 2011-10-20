
unit util;
interface

{
  Automatically converted by H2Pas 1.0.0 from util.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    slv2
    util.h
}

  const
    External_library='slv2'; {Setup as you need}

  Type
  Pchar  = ^char;
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
{$ifndef __SLV2_UTIL_H__}
{$define __SLV2_UTIL_H__}  
{$include <stdarg.h>}
{ C++ extern C conditionnal removed }
  {* \defgroup util Utility functions
   *
   * @
    }
  {* Convert a full URI (eg file://foo/bar/baz.ttl) to a local path (e.g. /foo/bar/baz.ttl).
   *
   * Return value is shared and must not be deleted by caller.
   * \return \a uri converted to a path, or NULL on failure (URI is not local).
    }
(* Const before type ignored *)
(* Const before type ignored *)

  function slv2_uri_to_path(uri:pchar):^char;cdecl;external External_library name 'slv2_uri_to_path';

  {* Append \a suffix to \a *dst, reallocating \a dst as necessary.
   *
   * \a dst will (possibly) be freed, it must be dynamically allocated with malloc
   * or NULL.
    }
(* Const before type ignored *)
  procedure slv2_strappend(dst:Ppchar; suffix:pchar);cdecl;external External_library name 'slv2_strappend';

  {* Join all arguments into one string.
   *
   * Arguments are not modified, return value must be free()'d.
    }
(* Const before type ignored *)
  function slv2_strjoin(first:pchar; args:array of const):^char;cdecl;external External_library name 'slv2_strjoin';

  function slv2_strjoin(first:pchar):^char;cdecl;external External_library name 'slv2_strjoin';

(* Const before type ignored *)
  function slv2_vstrjoin(first:Ppchar; args_list:va_list):^char;cdecl;external External_library name 'slv2_vstrjoin';

  {* @  }
{ C++ end of extern C conditionnal removed }
{$endif}
  { __SLV2_UTIL_H__  }

implementation


end.
