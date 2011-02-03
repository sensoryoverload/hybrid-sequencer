
unit driver_interface;
interface

{
  Automatically converted by H2Pas 1.0.0 from driver_interface.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    jack
    driver_interface.h
}

  const
    External_library='jack'; {Setup as you need}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  {
      Copyright (C) 2003 Bob Ham <rah@bash.sh>
      
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
  
   }
{$ifndef __jack_driver_interface_h__}
{$define __jack_driver_interface_h__}  
{ C++ extern C conditionnal removed }
{$include <limits.h>}
{$include <jack/jack.h>}

  const
     JACK_DRIVER_NAME_MAX = 15;     
     JACK_DRIVER_PARAM_NAME_MAX = 15;     
     JACK_DRIVER_PARAM_STRING_MAX = 63;     
  {* Driver parameter types  }

  type

     jack_driver_param_type_t = (JackDriverParamInt := 1,JackDriverParamUInt,
       JackDriverParamChar,JackDriverParamString,
       JackDriverParamBool);
  {* Driver parameter value  }

     jack_driver_param_value_t = record
         case longint of
            0 : ( ui : uint32_t );
            1 : ( i : int32_t );
            2 : ( c : char );
            3 : ( str : array[0..(JACK_DRIVER_PARAM_STRING_MAX+1)-1] of char );
         end;
  {* A driver parameter descriptor  }
  {*< The parameter's name  }
  {*< The parameter's character (for getopt, etc)  }
  {*< The parameter's type  }
  {*< The parameter's (default) value  }
  {*< A short (~30 chars) description for the user  }
  {*< A longer description for the user  }

     jack_driver_param_desc_t = record
          name : array[0..(JACK_DRIVER_NAME_MAX+1)-1] of char;
          character : char;
          _type : jack_driver_param_type_t;
          value : jack_driver_param_value_t;
          short_desc : array[0..63] of char;
          long_desc : array[0..1023] of char;
       end;
  {* A driver parameter  }

     jack_driver_param_t = record
          character : char;
          value : jack_driver_param_value_t;
       end;
  {* A struct for describing a jack driver  }
  {*< The driver's canonical name  }
  {*< The filename of the driver's shared object file  }
  {*< The number of parameters the driver has  }
  {*< An array of parameter descriptors  }

     jack_driver_desc_t = record
          name : array[0..(JACK_DRIVER_NAME_MAX+1)-1] of char;
          file : array[0..(PATH_MAX+1)-1] of char;
          nparams : uint32_t;
          params : ^jack_driver_param_desc_t;
       end;
{ C++ end of extern C conditionnal removed }
{$endif}
  { __jack_driver_interface_h__  }

implementation


end.
