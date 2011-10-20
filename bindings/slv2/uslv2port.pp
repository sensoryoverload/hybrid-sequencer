unit uslv2port;

interface

uses
  uslv2types, {slv2plugin, slv2port,} uslv2values;

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

{* \addtogroup data
   * @
    }
  {* Port equivalent to slv2_plugin_get_value.
   *
   * Time = Query
    }
(* Const before type ignored *)

  function slv2_port_get_value(plugin:SLV2Plugin; port:SLV2Port; _property:pchar):SLV2Values;cdecl;external External_library name 'slv2_port_get_value';

  {* Port equivalent to slv2_plugin_get_properties.
   *
   * Time = Query
    }
  function slv2_port_get_properties(plugin:SLV2Plugin; port:SLV2Port):SLV2Values;cdecl;external External_library name 'slv2_port_get_properties';

  {* Port equivalent to slv2_plugin_get_hints.
   *
   * Time = Query
    }
  function slv2_port_get_hints(plugin:SLV2Plugin; port:SLV2Port):SLV2Values;cdecl;external External_library name 'slv2_port_get_hints';

  {* Get the symbol of a port given the index.
   *
   * The 'symbol' is a short string, a valid C identifier.
   * Returned string must be free()'d by caller.
   *
   * \return NULL when index is out of range
   *
   * Time = Query
    }
  function slv2_port_get_symbol(plugin:SLV2Plugin; port:SLV2Port):pchar;cdecl;external External_library name 'slv2_port_get_symbol';

  {* Get the name of a port.
   *
   * This is guaranteed to return the untranslated name (the doap:name in the
   * data file without a language tag).  Returned value must be free()'d by
   * the caller.
   *
   * Time = Query
    }
  function slv2_port_get_name(plugin:SLV2Plugin; port:SLV2Port):pchar;cdecl;external External_library name 'slv2_port_get_name';

  {* Get the class (input/output, data type, rate...) of a port.
   *
   * Time = Query
    }
  function slv2_port_get_class(plugin:SLV2Plugin; port:SLV2Port):SLV2PortClass;cdecl;external External_library name 'slv2_port_get_class';

  {* Get the default value of a port.
   *
   * Only valid for ports with a data type of lv2:float.
   *
   * Time = Query
    }
  function slv2_port_get_default_value(plugin:SLV2Plugin; port:SLV2Port):single;cdecl;external External_library name 'slv2_port_get_default_value';

  {* Get the minimum value of a port.
   *
   * Only valid for ports with a data type of lv2:float.
   *
   * Time = Query
    }
  function slv2_port_get_minimum_value(plugin:SLV2Plugin; port:SLV2Port):single;cdecl;external External_library name 'slv2_port_get_minimum_value';

  {* Get the maximum value of a port.
   *
   * Only valid for ports with a data type of lv2:float.
   *
   * Time = Query
    }
  function slv2_port_get_maximum_value(plugin:SLV2Plugin; port:SLV2Port):single;cdecl;external External_library name 'slv2_port_get_maximum_value';


implementation


end.
