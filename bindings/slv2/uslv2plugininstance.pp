
unit uslv2plugininstance;
interface

uses
  ulv2, uslv2plugin, uslv2port;

{
  Automatically converted by H2Pas 1.0.0 from plugininstance.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    slv2
    plugininstance.h
}

    const
      External_library='slv2'; {Setup as you need}

    Type
    PLV2_Host_Feature  = ^LV2_Host_Feature;
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


  {* \defgroup lib Plugin library access
   *
   * An SLV2Instance is an instantiated SLV2Plugin (ie a loaded dynamic
   * library).  These functions interact with the binary library code only,
   * they do not read data files in any way.
   * 
   * @
    }

  type

     SLV2InstanceImpl = _InstanceImpl;
  {* Instance of a plugin.
   *
   * The LV2 descriptor and handle of this are exposed to allow inlining of
   * performance critical functions like slv2_instance_run (which are exposed
   * in lv2.h anyway).  The remaining implementation details are
   * in the opaque pimpl member.
    }
(* Const before type ignored *)
  {/< Private implementation }

     _Instance = record
          lv2_descriptor : ^LV2_Descriptor;
          lv2_handle : LV2_Handle;
          pimpl : SLV2InstanceImpl;
       end;
     SLV2Instance = ^_Instance;
  {* Instantiate a plugin.
   *
   * The returned object represents shared library objects loaded into memory,
   * it must be cleaned up with slv2_instance_free when no longer
   * needed.
   * 
   * \a plugin is not modified or directly referenced by the returned object
   * (instances store only a copy of the plugin's URI).
   * 
   * \a host_features NULL-terminated array of features the host supports.
   * NULL may be passed if the host supports no additional features (unlike
   * the LV2 specification - SLV2 takes care of it).
   *
   * \return NULL if instantiation failed.
    }
(* Const before type ignored *)

  function slv2_plugin_instantiate(plugin:SLV2Plugin; sample_rate:uint32_t; host_features:PpLV2_Host_Feature):SLV2Instance;cdecl;external External_library name 'slv2_plugin_instantiate';

  {* Free a plugin instance.
   *
   * \a instance is invalid after this call.
    }
  procedure slv2_instance_free(instance:SLV2Instance);cdecl;external External_library name 'slv2_instance_free';

  {* Get the URI of the plugin which \a instance is an instance of.
   *
   * Returned string is shared and must not be modified or deleted.
    }
(* error 
static inline const char*
 in declarator_list *)

(* error 
	assert(instance->lv2_descriptor);
 in declarator_list *)

(* error 
	return instance->lv2_descriptor->URI;
 in declarator_list *)

(* error 

  {* Connect a port to a data location.
   *
   * This may be called regardless of whether the plugin is activated,
   * activation and deactivation does not destroy port connections.
    }
in declaration at line 113 *)

(* error 
	assert(instance->lv2_descriptor);
 in declarator_list *)

(* error 
	assert(instance->lv2_descriptor->connect_port);
 in declarator_list *)

(* error
	instance->lv2_descriptor->connect_port
*)
(*
 error
	instance->lv2_descriptor->connect_port
 in declarator_list *)

(* error 
		(instance->lv2_handle, port_index, data_location);
 in declarator_list *)
(* error 
}
    {* Activate a plugin instance.
     *
     * This resets all state information in the plugin, except for port data
     * locations (as set by slv2_instance_connect_port).  This MUST be called
     * before calling slv2_instance_run.
      }
in declaration at line 131 *)
(* error 
	assert(instance->lv2_descriptor);
 in declarator_list *)
(* error 
	if (instance->lv2_descriptor->activate)
 in declarator_list *)
(* error 
}
    {* Run \a instance for \a sample_count frames.
     *
     * If the hint lv2:hardRtCapable is set for this plugin, this function is
     * guaranteed not to block.
      }
in declaration at line 148 *)

	assert(instance->lv2_descriptor);
 in declarator_list

	assert(instance->lv2_handle),

	assert(instance->lv2_descriptor->run);
 in declarator_list
 in declarator_list

	instance->lv2_descriptor->run(instance->lv2_handle, sample_count);

	instance->lv2_descriptor->run(instance->lv2_handle, sample_count);

	instance->lv2_descriptor->run(instance->lv2_handle, sample_count);
 in declarator_list
 in declarator_list

}
    {* Deactivate a plugin instance.
     *
     * Note that to run the plugin after this you must activate it, which will
     * reset all state information (except port connections).
      }


	assert(instance->lv2_descriptor);
 in declarator_list

	assert(instance->lv2_handle);
 in declarator_list

	if (instance->lv2_descriptor->deactivate)
 in declarator_list

}
    {* Get the LV2_Descriptor of the plugin instance.
     *
     * Normally hosts should not need to access the LV2_Descriptor directly,
     * use the slv2_instance_* functions.
     *
     * The returned descriptor is shared and must not be deleted.
      }


	assert(instance->lv2_descriptor);


	return instance->lv2_descriptor;


}
    {* Get the LV2_Handle of the plugin instance.
     *
     * Normally hosts should not need to access the LV2_Handle directly,
     * use the slv2_instance_* functions.
     * 
     * The returned handle is shared and must not be deleted.
      }


	assert(instance->lv2_descriptor);

	return instance->lv2_handle;



implementation


end.
