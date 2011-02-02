
unit varargs;
interface

{
  Automatically converted by H2Pas 1.0.0 from varargs.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    jack
    varargs.h
}

    const
      External_library='jack'; {Setup as you need}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  {
   *  Copyright (C) 2004 Jack O'Quin
   *  
   *  This program is free software; you can redistribute it and/or modify
   *  it under the terms of the GNU Lesser General Public License as published by
   *  the Free Software Foundation; either version 2.1 of the License, or
   *  (at your option) any later version.
   *  
   *  This program is distributed in the hope that it will be useful,
   *  but WITHOUT ANY WARRANTY; without even the implied warranty of
   *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   *  GNU Lesser General Public License for more details.
   *  
   *  You should have received a copy of the GNU Lesser General Public License
   *  along with this program; if not, write to the Free Software 
   *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
   *
   *  $Id: varargs.h 807 2004-11-29 17:54:35Z joq $
    }
{$ifndef __jack_varargs_h__}
{$define __jack_varargs_h__}  
{ C++ extern C conditionnal removed }
  { variable argument structure  }
  { server name  }
  { load module name  }
  { initialization string  }

  type

     jack_varargs_t = record
          server_name : ^char;
          load_name : ^char;
          load_init : ^char;
       end;
(* error 
static inline void
 in declarator_list *)
(* error 
	va->server_name = jack_default_server_name ();
 in declarator_list *)
(* error 
}
  { initialize default settings  }
in declaration at line 46 *)
(* error 
	if ((options & JackServerName)) {
 in declarator_list *)
(* error 
		char *sn = va_arg(ap, char *);
 in declarator_list *)
(* error 
			va->server_name = sn;
 in declarator_list *)
(* error 
	}
in declaration at line 54 *)
(* error 
	if ((options & JackLoadInit))
(* error 
		va->load_init = va_arg(ap, char *);
 in declarator_list *)
 in declarator_list *)
(* error 
}
{ C++ end of extern C conditionnal removed }
{$endif}
    { __jack_varargs_h__  }

implementation


end.
