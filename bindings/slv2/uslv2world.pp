unit uslv2world;

interface

uses
  uslv2plugins, uslv2pluginclasses, uslv2types{, librdf};

const
  External_library='slv2'; {Setup as you need}

Type
  SLV2PluginFunc = function (_para1:SLV2Plugin):boolean;

  Pchar  = ^char;
//  Plibrdf_world  = ^librdf_world;
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
  {* \defgroup world Library
   * 
   * The "world" represents all library state, and the data found in bundles'
   * manifest.ttl (ie it is an in-memory index of all things LV2 found).
   * Plugins (and plugin extensions) and the LV2 specification (and LV2
   * extensions) itself can be queried from the world for use.
   *
   * Normal hosts which just want to easily load plugins by URI are strongly
   * recommended to simply call \ref slv2_world_load_all to find all installed
   * data in the recommended way.
   *
   * Normal hosts should NOT have to refer to bundles directly under normal
   * circumstances.  However, functions are provided to load individual bundles
   * explicitly, intended for hosts which depend on a specific bundle
   * (which is shipped with the application).
   *
   * @
    }
  {* Initialize a new, empty world.
    }

  function slv2_world_new:SLV2World;cdecl;external External_library name 'slv2_world_new';

  {* Initialize a new, empty world, using an existing Redland context.
    }
// Ugh..needs LibRDF bindings  function slv2_world_new_using_rdf_world(world:plibrdf_world):SLV2World;cdecl;external External_library name 'slv2_world_new_using_rdf_world';

  {* Destroy the world, mwahaha.
   *
   * NB: Destroying the world will leave dangling references in any plugin lists,
   * plugins, etc.  Do not destroy the world until you are finished with all
   * objects that came from it.
    }
  procedure slv2_world_free(world:SLV2World);cdecl;external External_library name 'slv2_world_free';

  {* Load all installed LV2 bundles on the system.
   *
   * This is the recommended way for hosts to load LV2 data.  It does the most
   * reasonable thing to find all installed plugins, extensions, etc. on the
   * system.  The environment variable LV2_PATH may be used to set the
   * directories inside which this function will look for bundles.  Otherwise
   * a sensible, standard default will be used.
   *
   * Use of other functions for loading bundles is \em highly discouraged
   * without a special reason to do so - use this one.
   *
   * Time = Query
    }
  procedure slv2_world_load_all(world:SLV2World);cdecl;external External_library name 'slv2_world_load_all';

  {* Load a specific bundle.
   *
   * \arg bundle_uri A fully qualified URI to the bundle directory,
   * with the trailing slash, eg. file:///usr/lib/lv2/someBundle/
   *
   * Normal hosts should not use this function.
   *
   * Hosts should \b never attach any long-term significance to bundle paths
   * as there are no guarantees they will remain consistent whatsoever.
   * Plugins (and other things) are identified by URIs, \b not bundle or
   * file names.
   *
   * This function should only be used by apps which ship with a special
   * bundle (which it knows exists at some path because the bundle is
   * shipped with the application).
   *
   * Time = Query
    }
(* Const before type ignored *)
  procedure slv2_world_load_bundle(world:SLV2World; bundle_uri:pchar);cdecl;external External_library name 'slv2_world_load_bundle';

  {* Get the parent of all other plugin classes, lv2:Plugin.
   *
   * Time = O(1)
    }
  function slv2_world_get_plugin_class(world:SLV2World):SLV2PluginClass;cdecl;external External_library name 'slv2_world_get_plugin_class';

  {* Return a list of all found plugin classes.
   *
   * Returned list is owned by world and must not be freed by the caller.
   * 
   * Time = O(1)
    }
  function slv2_world_get_plugin_classes(world:SLV2World):SLV2PluginClasses;cdecl;external External_library name 'slv2_world_get_plugin_classes';

  {* Return a list of all found plugins.
   *
   * The returned list contains just enough references to query
   * or instantiate plugins.  The data for a particular plugin will not be
   * loaded into memory until a call to an slv2_plugin_* function results in
   * a query (at which time the data is cached with the SLV2Plugin so future
   * queries are very fast).
   *
   * Returned list must be freed by user with slv2_plugins_free.  The contained
   * plugins are owned by \a world and must not be freed by caller.
   *
   * Time = O(1)
    }
  function slv2_world_get_all_plugins(world:SLV2World):SLV2Plugins;cdecl;external External_library name 'slv2_world_get_all_plugins';

  {* Return a list of found plugins filtered by a user-defined filter function.
   *
   * All plugins currently found in \a world that return true when passed to
   * \a include (a pointer to a function which takes an SLV2Plugin and returns
   * a bool) will be in the returned list.
   *
   * Returned list must be freed by user with slv2_plugins_free.  The contained
   * plugins are owned by \a world and must not be freed by caller.
   *
   * Time = O(n * Time(include))
    }
  function slv2_world_get_plugins_by_filter(world:SLV2World; include: SLV2PluginFunc):SLV2Plugins;cdecl;external External_library name 'slv2_world_get_plugins_by_filter';

  {* Return a list of found plugins in a given class.
   *
   * Returned list must be freed by user with slv2_plugins_free.  The contained
   * plugins are owned by \a world and must not be freed by caller.
   *
   * Time = O(n)
    }

  function slv2_world_get_plugins_by_class(world:SLV2World; plugin_class:SLV2PluginClass):SLV2Plugins;cdecl;external External_library name 'slv2_world_get_plugins_by_class';

  {* Get plugins filtered by a user-defined SPARQL query.
   *
   * This is much faster than using slv2_world_get_plugins_by_filter with a
   * filter function which calls the various slv2_plugin_* functions.
   *
   * \param query A valid SPARQL query which SELECTs a single variable, which
   * should match the URI of plugins to be loaded.
   *
   * \b Example: Get all plugins with at least 1 audio input and output:
  <tt> \verbatim
  PREFIX : <http://lv2plug.in/ontology#>
  SELECT DISTINCT ?plugin WHERE 
      ?plugin  :port  [ a :AudioPort; a :InputPort ] ;
               :port  [ a :AudioPort; a :OutputPort ] .
  
  \endverbatim </tt>
   *
   * Returned plugins contain a reference to this world, world must not be
   * destroyed until plugins are finished with.
    }

  function slv2_world_get_plugins_by_query(world:SLV2World; query:pchar):SLV2Plugins;cdecl;external External_library name 'slv2_world_get_plugins_by_query';


implementation


end.
