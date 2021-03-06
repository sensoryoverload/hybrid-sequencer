unit uslv2plugins;

interface

uses
    uslv2types, uslv2plugin;

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
{$ifndef __SLV2_PLUGINS_H__}
{$define __SLV2_PLUGINS_H__}  
{ C++ extern C conditionnal removed }
  {* \defgroup plugins Collections of plugins
   * 
   * These functions work with lists of plugins which come from an
   * SLV2World.  These lists contain only a weak reference to an LV2 plugin
   * in the Model.
   *
   * @
    }
  {* Free a plugin list.
   *
   * Freeing a plugin list does not destroy the plugins it contains (plugins
   * are owned by the world).  \a list is invalid after this call.
    }

  procedure slv2_plugins_free(world:SLV2World; list:SLV2Plugins);cdecl;external External_library name 'slv2_plugins_free';

  {* Get the number of plugins in the list.
    }
  function slv2_plugins_size(list:SLV2Plugins):dword;cdecl;external External_library name 'slv2_plugins_size';

  {* Get a plugin from the list by URI.
   *
   * Return value is shared (stored in \a list) and must not be freed or
   * modified by the caller in any way.
   *
   * Time = O(log2(n))
   * 
   * \return NULL if plugin with \a url not found in \a list.
    }
(* Const before type ignored *)
  function slv2_plugins_get_by_uri(list:SLV2Plugins; uri:pchar):SLV2Plugin;cdecl;external External_library name 'slv2_plugins_get_by_uri';

  {* Get a plugin from the list by index.
   *
   * \a index has no significance other than as an index into this list.
   * Any \a index not less than slv2_list_get_length(list) will return NULL,
   * so all plugins in a list can be enumerated by repeated calls
   * to this function starting with \a index = 0.
   *
   * Time = O(1)
   *
   * \return NULL if \a index out of range.
    }
  function slv2_plugins_get_at(list:SLV2Plugins; index:dword):SLV2Plugin;cdecl;external External_library name 'slv2_plugins_get_at';

  {* @  }
{ C++ end of extern C conditionnal removed }
{$endif}
  { __SLV2_PLUGINS_H__  }

implementation


end.
