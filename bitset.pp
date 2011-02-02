
unit bitset;
interface

{
  Automatically converted by H2Pas 1.0.0 from bitset.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    jack
    bitset.h
}

    const
      External_library='jack'; {Setup as you need}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  {
   * bitset.h -- some simple bit vector set operations.
   *
   * This is useful for sets of small non-negative integers.  There are
   * some obvious set operations that are not implemented because I
   * don't need them right now.
   *
   * These functions represent sets as arrays of unsigned 32-bit
   * integers allocated on the heap.  The first entry contains the set
   * cardinality (number of elements allowed), followed by one or more
   * words containing bit vectors.
   *
   *  $Id: bitset.h 864 2005-01-03 00:15:31Z joq $
    }
  {
   *  Copyright (C) 2005 Jack O'Quin
   *
   *  This program is free software; you can redistribute it and/or
   *  modify it under the terms of the GNU General Public License as
   *  published by the Free Software Foundation; either version 2 of the
   *  License, or (at your option) any later version.
   *  
   *  This program is distributed in the hope that it will be useful,
   *  but WITHOUT ANY WARRANTY; without even the implied warranty of
   *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   *  General Public License for more details.
   *  
   *  You should have received a copy of the GNU General Public License
   *  along with this program; if not, write to the Free Software
   *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
    }
{$ifndef __bitset_h__}
{$define __bitset_h__}  
{$include <inttypes.h>			/* POSIX standard fixed-size types */}
{$include <assert.h>			/* `#define NDEBUG' to disable */}
  { On some 64-bit machines, this implementation may be slightly
   * inefficient, depending on how compilers allocate space for
   * uint32_t.  For the set sizes I currently need, this is acceptable.
   * It should not be hard to pack the bits better, if that becomes
   * worthwhile.
    }

  type

     _bitset_word_t = uint32_t;

     bitset_t = _bitset_word_t;
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function WORD_SIZE(cardinality : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function BYTE_SIZE(cardinality : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function WORD_INDEX(element : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  function BIT_INDEX(element : longint) : element;  

(* error 
static inline void
(* error 
bitset_add(bitset_t set, unsigned int element)
 in declarator_list *)
 in declarator_list *)
(* error 
	set[WORD_INDEX(element)] |= (1 << BIT_INDEX(element));
 in declarator_list *)
(* error 
}
in declaration at line 64 *)
(* error 
	memcpy(to_set, from_set, BYTE_SIZE(to_set[0]));
 in declarator_list *)
(* error 
	memcpy(to_set, from_set, BYTE_SIZE(to_set[0]));
 in declarator_list *)

      var
 : memcpy;
(* error 
}
in declaration at line 72 *)
         set : ^assert;cvar;public;
(* error 
	*set[0] = cardinality;
in declaration at line 74 *)
(* error 
}
in declaration at line 81 *)
(* error 
		*set = (bitset_t) 0;
in declaration at line 82 *)
(* error 
	}
in declaration at line 89 *)
(* error 
	_bitset_word_t result = 0;
 in declarator_list *)
(* error 
	int nwords = WORD_SIZE(set[0]);
 in declarator_list *)
(* error 
	for (i = 1; i < nwords; i++) {
 in declarator_list *)
(* error 
	for (i = 1; i < nwords; i++) {
in declaration at line 92 *)
(* error 
	for (i = 1; i < nwords; i++) {
in declaration at line 93 *)
(* error 
	}
in declaration at line 95 *)
(* error 
}
in declaration at line 101 *)
(* error 
	return (0 != (set[WORD_INDEX(element)] & (1<<BIT_INDEX(element))));
 in declarator_list *)
(* error 
}
in declaration at line 108 *)
(* error 
	set[WORD_INDEX(element)] &= ~(1<<BIT_INDEX(element));
in declaration at line 109 *)
(* error 
}
{$endif}
    { __bitset_h__  }

implementation

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function WORD_SIZE(cardinality : longint) : longint;
    begin
       WORD_SIZE:=1+((cardinality+31)/32);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function BYTE_SIZE(cardinality : longint) : longint;
    begin
       BYTE_SIZE:=(WORD_SIZE(cardinality))*(sizeof(_bitset_word_t));
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function WORD_INDEX(element : longint) : longint;
    begin
       WORD_INDEX:=1+(element/32);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  function BIT_INDEX(element : longint) : element;
    begin
       BIT_INDEX:=element(@(037));
    end;


end.
