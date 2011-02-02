
unit memops;
interface

{
  Automatically converted by H2Pas 1.0.0 from memops.h
  The following command line parameters were used:
    -DecCpPTv
    -l
    jack
    memops.h
}

    const
      External_library='jack'; {Setup as you need}

    Type
    Pchar  = ^char;
    Pdither_state_t  = ^dither_state_t;
    Pjack_default_audio_sample_t  = ^jack_default_audio_sample_t;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  {
      Copyright (C) 1999-2000 Paul Davis 
  
      This program is free software; you can redistribute it and/or modify
      it under the terms of the GNU General Public License as published by
      the Free Software Foundation; either version 2 of the License, or
      (at your option) any later version.
  
      This program is distributed in the hope that it will be useful,
      but WITHOUT ANY WARRANTY; without even the implied warranty of
      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      GNU General Public License for more details.
  
      You should have received a copy of the GNU General Public License
      along with this program; if not, write to the Free Software
      Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
  
      $Id: memops.h 945 2006-05-04 15:14:45Z pbd $
   }
{$ifndef __jack_memops_h__}
{$define __jack_memops_h__}  
{$include <jack/types.h>}

  type

     DitherAlgorithm = (None,Rectangular,Triangular,Shaped);

  const
     DITHER_BUF_SIZE = 8;     
     DITHER_BUF_MASK = 7;     

  type

     dither_state_t = record
          depth : dword;
          rm1 : double;
          idx : dword;
          e : array[0..(DITHER_BUF_SIZE)-1] of double;
       end;

  procedure sample_move_d32u24_sSs(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_d32u24_sSs';

  procedure sample_move_d32u24_sS(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_d32u24_sS';

  procedure sample_move_d24_sSs(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_d24_sSs';

  procedure sample_move_d24_sS(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_d24_sS';

  procedure sample_move_d16_sSs(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_d16_sSs';

  procedure sample_move_d16_sS(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_d16_sS';

  procedure sample_move_dither_rect_d32u24_sSs(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_rect_d32u24_sSs';

  procedure sample_move_dither_rect_d32u24_sS(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_rect_d32u24_sS';

  procedure sample_move_dither_tri_d32u24_sSs(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_tri_d32u24_sSs';

  procedure sample_move_dither_tri_d32u24_sS(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_tri_d32u24_sS';

  procedure sample_move_dither_shaped_d32u24_sSs(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_shaped_d32u24_sSs';

  procedure sample_move_dither_shaped_d32u24_sS(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_shaped_d32u24_sS';

  procedure sample_move_dither_rect_d24_sSs(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_rect_d24_sSs';

  procedure sample_move_dither_rect_d24_sS(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_rect_d24_sS';

  procedure sample_move_dither_tri_d24_sSs(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_tri_d24_sSs';

  procedure sample_move_dither_tri_d24_sS(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_tri_d24_sS';

  procedure sample_move_dither_shaped_d24_sSs(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_shaped_d24_sSs';

  procedure sample_move_dither_shaped_d24_sS(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_shaped_d24_sS';

  procedure sample_move_dither_rect_d16_sSs(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_rect_d16_sSs';

  procedure sample_move_dither_rect_d16_sS(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_rect_d16_sS';

  procedure sample_move_dither_tri_d16_sSs(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_tri_d16_sSs';

  procedure sample_move_dither_tri_d16_sS(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_tri_d16_sS';

  procedure sample_move_dither_shaped_d16_sSs(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_shaped_d16_sSs';

  procedure sample_move_dither_shaped_d16_sS(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_move_dither_shaped_d16_sS';

  procedure sample_move_dS_s32u24s(dst:pjack_default_audio_sample_t; src:pchar; nsamples:dword; src_skip:dword);cdecl;external External_library name 'sample_move_dS_s32u24s';

  procedure sample_move_dS_s32u24(dst:pjack_default_audio_sample_t; src:pchar; nsamples:dword; src_skip:dword);cdecl;external External_library name 'sample_move_dS_s32u24';

  procedure sample_move_dS_s24s(dst:pjack_default_audio_sample_t; src:pchar; nsamples:dword; src_skip:dword);cdecl;external External_library name 'sample_move_dS_s24s';

  procedure sample_move_dS_s24(dst:pjack_default_audio_sample_t; src:pchar; nsamples:dword; src_skip:dword);cdecl;external External_library name 'sample_move_dS_s24';

  procedure sample_move_dS_s16s(dst:pjack_default_audio_sample_t; src:pchar; nsamples:dword; src_skip:dword);cdecl;external External_library name 'sample_move_dS_s16s';

  procedure sample_move_dS_s16(dst:pjack_default_audio_sample_t; src:pchar; nsamples:dword; src_skip:dword);cdecl;external External_library name 'sample_move_dS_s16';

  procedure sample_merge_d16_sS(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_merge_d16_sS';

  procedure sample_merge_d32u24_sS(dst:pchar; src:pjack_default_audio_sample_t; nsamples:dword; dst_skip:dword; state:pdither_state_t);cdecl;external External_library name 'sample_merge_d32u24_sS';

(* error 
static __inline__ void
(* error 
sample_merge (jack_default_audio_sample_t *dst, jack_default_audio_sample_t *src, unsigned long cnt)
(* error 
sample_merge (jack_default_audio_sample_t *dst, jack_default_audio_sample_t *src, unsigned long cnt)
 in declarator_list *)
 in declarator_list *)
 in declarator_list *)
(* error 
	while (cnt--) {
 in declarator_list *)
(* error 
		*dst += *src;
 in declarator_list *)
(* error 
		dst++;
in declaration at line 86 *)
(* error 
		src++;
in declaration at line 87 *)
(* error 
	}
in declaration at line 95 *)
(* error 
}
in declaration at line 98 *)
    procedure memcpy_fake(dst:pchar; src:pchar; src_bytes:dword; foo:dword; bar:dword);cdecl;external External_library name 'memcpy_fake';

    procedure memcpy_interleave_d16_s16(dst:pchar; src:pchar; src_bytes:dword; dst_skip_bytes:dword; src_skip_bytes:dword);cdecl;external External_library name 'memcpy_interleave_d16_s16';

    procedure memcpy_interleave_d24_s24(dst:pchar; src:pchar; src_bytes:dword; dst_skip_bytes:dword; src_skip_bytes:dword);cdecl;external External_library name 'memcpy_interleave_d24_s24';

    procedure memcpy_interleave_d32_s32(dst:pchar; src:pchar; src_bytes:dword; dst_skip_bytes:dword; src_skip_bytes:dword);cdecl;external External_library name 'memcpy_interleave_d32_s32';

    procedure merge_memcpy_interleave_d16_s16(dst:pchar; src:pchar; src_bytes:dword; dst_skip_bytes:dword; src_skip_bytes:dword);cdecl;external External_library name 'merge_memcpy_interleave_d16_s16';

    procedure merge_memcpy_interleave_d24_s24(dst:pchar; src:pchar; src_bytes:dword; dst_skip_bytes:dword; src_skip_bytes:dword);cdecl;external External_library name 'merge_memcpy_interleave_d24_s24';

    procedure merge_memcpy_interleave_d32_s32(dst:pchar; src:pchar; src_bytes:dword; dst_skip_bytes:dword; src_skip_bytes:dword);cdecl;external External_library name 'merge_memcpy_interleave_d32_s32';

    procedure merge_memcpy_d16_s16(dst:pchar; src:pchar; src_bytes:dword; foo:dword; bar:dword);cdecl;external External_library name 'merge_memcpy_d16_s16';

    procedure merge_memcpy_d32_s32(dst:pchar; src:pchar; src_bytes:dword; foo:dword; bar:dword);cdecl;external External_library name 'merge_memcpy_d32_s32';

{$endif}
    { __jack_memops_h__  }

implementation


end.
