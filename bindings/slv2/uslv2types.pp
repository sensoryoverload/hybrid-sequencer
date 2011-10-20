unit uslv2types;

interface

uses
  BaseUnix;

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

  {* Class (direction and type) of a port
   *
   * Note that ports may be of other classes not listed here, this is just
   * to make the most common case simple.  Use slv2_port_get_value(p, "rdf:type")
   * if you need further class information.
    }
  {*< One input float per block  }
  {*< One output float per block  }
  {*< One input float per frame  }
  {*< One output float per frame  }
  {*< MIDI input (LL extension)  }
  {*< MIDI output (LL extension)  }

  type
   uint32_t = cuint32;

   _SLV2PortClass = (SLV2_UNKNOWN_PORT_CLASS,SLV2_CONTROL_INPUT,
     SLV2_CONTROL_OUTPUT,SLV2_AUDIO_INPUT,
     SLV2_AUDIO_OUTPUT,SLV2_MIDI_INPUT,SLV2_MIDI_OUTPUT
     );
   SLV2PortClass = _SLV2PortClass;
{* The format of a URI string.
 *
 * Full URI: http://example.org/foo
 * QName: lv2:Plugin
  }

   _SLV2URIType = (SLV2_URI,SLV2_QNAME);
   SLV2URIType = _SLV2URIType;
{* A port on a plugin.  Opaque, but valid to compare to NULL.  }

   SLV2Port = pointer;
{* A plugin.  Opaque, but valid to compare to NULL.  }

   SLV2Plugin = pointer;
{* A collection of plugins.  Opaque, but valid to compare to NULL.  }

   SLV2Plugins = pointer;
{* The world.  Opaque, but valid to compare to NULL.  }

   SLV2World = pointer;
{* A plugin class.  Opaque, but valid to compare to NULL.  }

   SLV2PluginClass = pointer;
{* A collection of plugin classes.  Opaque, but valid to compare to NULL.  }

   SLV2PluginClasses = pointer;
{* A typed value  }

   SLV2Value = pointer;
{* A collection of typed values.  }

   SLV2Values = pointer;

implementation

end.
