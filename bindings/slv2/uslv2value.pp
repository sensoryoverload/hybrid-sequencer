unit uslv2value;

interface

uses
  slv2types;

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

//{$include <slv2/types.h>}
  {* \addtogroup data
   * @
    }
  {* Free an SLV2Value.
    }

  procedure slv2_value_free(val:SLV2Value);cdecl;external External_library name 'slv2_value_free';

  {* Return whether two values are equivalent.
    }
  function slv2_value_equals(value:SLV2Value; other:SLV2Value):boolean;cdecl;external External_library name 'slv2_value_equals';

  {* Return this value as a Turtle/SPARQL token.
   * Examples:
   * 	<http://example.org/foo>
   * 	doap:name
   * 	"this is a string"
   * 	1.0
   * 	1
   *
   * 	Returned string is newly allocation and must be freed by caller.
    }
  function slv2_value_get_turtle_token(value:SLV2Value):pchar;cdecl;external External_library name 'slv2_value_get_turtle_token';

  {* Return whether the value is a URI (resource).
   *
   * Time = O(1)
    }
  function slv2_value_is_uri(value:SLV2Value):boolean;cdecl;external External_library name 'slv2_value_is_uri';

  {* Return this value as a URI string, e.g. "http://example.org/foo".
   * 
   * Valid to call only if slv2_value_is_uri(\a value) or
   * slv2_value_is_qname(\a value) returns true.
   * Returned value is owned by \a value and must not be freed by caller.
   * 
   * Time = O(1)
    }
(* Const before type ignored *)
  function slv2_value_as_uri(value:SLV2Value):pchar;cdecl;external External_library name 'slv2_value_as_uri';

{$if 0}
  {* Return whether the value is a QName ("qualified name", a prefixed URI).
   *
   * A QName will return true for both this, and slv2_value_is_uri.
   * slv2_value_as_uri and slv2_value_as_qname will both return appropriately.
   *
   * Time = O(1)
    }

  function slv2_value_is_qname(value:SLV2Value):boolean;cdecl;external External_library name 'slv2_value_is_qname';

  {* Return this value as a QName string, e.g. "lv2:Plugin".
   * 
   * Valid to call only if slv2_value_is_qname(\a value) returns true.
   * Returned value is owned by \a value and must not be freed by caller.
   * 
   * Time = O(1)
    }
(* Const before type ignored *)
  function slv2_value_as_qname(value:SLV2Value):pchar;cdecl;external External_library name 'slv2_value_as_qname';

{$endif}
  {* Return whether this value is a literal (i.e. not a URI).
   *
   * Returns true if \a value is a string or numeric value.
   *
   * Time = O(1)
    }

  function slv2_value_is_literal(value:SLV2Value):boolean;cdecl;external External_library name 'slv2_value_is_literal';

  {* Return whether this value is a string literal.
   *
   * Returns true if \a value is a string (but not  numeric) value.
   *
   * Time = O(1)
    }
  function slv2_value_is_string(value:SLV2Value):boolean;cdecl;external External_library name 'slv2_value_is_string';

  {* Return whether this value is a string literal.
   *
   * Time = O(1)
    }
(* Const before type ignored *)
  function slv2_value_as_string(value:SLV2Value):pchar;cdecl;external External_library name 'slv2_value_as_string';

  {* Return whether this value is a decimal literal.
   *
   * Time = O(1)
    }
  function slv2_value_is_float(value:SLV2Value):boolean;cdecl;external External_library name 'slv2_value_is_float';

  {* Return \a value as a float.
   * 
   * Valid to call only if slv2_value_is_float(\a value) or
   * slv2_value_is_int(\a value) returns true.
   *
   * Time = O(1)
    }
  function slv2_value_as_float(value:SLV2Value):single;cdecl;external External_library name 'slv2_value_as_float';

  {* Return whether this value is an integer literal.
   * 
   * Time = O(1)
    }
  function slv2_value_is_int(value:SLV2Value):boolean;cdecl;external External_library name 'slv2_value_is_int';

  {* Return \a value as an integer.
   * 
   * Valid to call only if slv2_value_is_int(\a value) returns true.
   *
   * Time = O(1)
    }
  function slv2_value_as_int(value:SLV2Value):longint;cdecl;external External_library name 'slv2_value_as_int';


implementation


end.
