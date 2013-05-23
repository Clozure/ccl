/*   Copyright (C) 2005-2009 Clozure Associates and contributors  */
/*   This file is part of Clozure CL.    */

/*   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public  */
/*   License , known as the LLGPL and distributed with Clozure CL as the  */
/*   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,  */
/*   which is distributed with Clozure CL as the file "LGPL".  Where these  */
/*   conflict, the preamble takes precedence.    */

/*   Clozure CL is referenced in the preamble as the "LIBRARY."  */

/*   The LLGPL is also available online at  */
/*   http://opensource.franz.com/preamble.html  */


		
	include(lisp.s)
	_beginfile
	
        .align 2
define(`_spentry',`ifdef(`__func_name',`_endfn',`')
	.p2align 3
	_exportfn(_SP$1)
	.line  __line__
')

             
define(`_endsubp',`
	_endfn(_SP$1)
#  __line__ 
')

define(`jump_builtin',`
	ref_nrs_value(builtin_functions,%fname)
	set_nargs($2)
	vrefr(%fname,%fname,$1)
	jump_fname()
')

        

_spentry(bad_funcall)
Xspentry_start:         
	.globl C(bad_funcall)	
__(tra(C(bad_funcall)))
	__(uuo_error_not_callable)
_endsubp(bad_funcall)
	
/* %arg_z has overflowed by one bit.  Make a bignum with 2 (32-bit) digits.  */
	
_spentry(fix_overflow)
C(fix_one_bit_overflow):	
	__(movq $two_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(`',aligned_bignum_size(2)))
	__(unbox_fixnum(%arg_z,%imm0))
	__(movq $0xe000000000000000,%imm1)
	__(mov %temp0,%arg_z)
	__(xorq %imm1,%imm0)
	__(movq %imm0,misc_data_offset(%arg_z))
	__(ret)	
_endsubp(fix_overflow)


/* Make a lisp integer (fixnum or two-digit bignum) from the signed  */
/* 64-bit value in %imm0.   */

_spentry(makes64)
	__(movq %imm0,%imm1)
	__(shlq $fixnumshift,%imm1)
	__(movq %imm1,%arg_z)
	__(sarq $fixnumshift,%imm1)
	__(cmpq %imm1,%imm0)
	__(jz 0f)
	__(movd %imm0,%mm0)
	__(movq $two_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(2)))
	__(movq %mm0,misc_data_offset(%arg_z))
0:	__(repret)
_endsubp(makes64)	

        				

/* %imm1:%imm0 constitute a signed integer, almost certainly a bignum.  */
/* Make a lisp integer out of those 128 bits ..   */
	
_startfn(C(makes128))
	
        /*  We're likely to have to make a bignum out of the integer in %imm1 and  */
        /*  %imm0. We'll need to use %imm0 and %imm1 to cons the bignum, and  */
        /*  will need to do some arithmetic (determining significant bigits)  */
        /*  on %imm0 and %imm1 in order to know how large that bignum needs to be.  */
        /*  Cache %imm0 and %imm1 in %mm0 and %mm1.   */
   
	__(movd %imm0,%mm0)
	__(movd %imm1,%mm1)
	
        /* If %imm1 is just a sign extension of %imm0, make a 64-bit signed integer.   */
	
	__(sarq $63,%imm0) 
	__(cmpq %imm0,%imm1)
	__(movd %mm0,%imm0)
	__(je _SPmakes64)
	
        /* Otherwise, if the high 32 bits of %imm1 are a sign-extension of the  */
        /* low 32 bits of %imm1, make a 3-digit bignum.  If the upper 32 bits  */
        /* of %imm1 are significant, make a 4 digit bignum   */
	
	__(movq %imm1,%imm0)
	__(shlq $32,%imm0)
	__(sarq $32,%imm0)
	__(cmpq %imm0,%imm1)
	__(jz 3f)
	__(mov $four_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(4)))
	__(movq %mm0,misc_data_offset(%arg_z))
	__(movq %mm1,misc_data_offset+8(%arg_z))
	__(ret)
3:	__(mov $three_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(3)))
	__(movq %mm0,misc_data_offset(%arg_z))
	__(movd %mm1,misc_data_offset+8(%arg_z))
	__(ret)
_endfn

        
/* %imm1:%imm0 constitute an unsigned integer, almost certainly a bignum.  */
/* Make a lisp integer out of those 128 bits ..  */
	
_startfn(C(makeu128))
	
        /* We're likely to have to make a bignum out of the integer in %imm1 and  */
        /* %imm0. We'll need to use %imm0 and %imm1 to cons the bignum, and  */
        /* will need to do some arithmetic (determining significant bigits)  */
        /* on %imm0 and %imm1 in order to know how large that bignum needs to be.  */
        /* Cache %imm0 and %imm1 in %mm0 and %mm1.   */

        /* If the high word is 0, make an unsigned-byte 64 ... 	  */
	
	__(testq %imm1,%imm1)
	__(jz _SPmakeu64)
	
	__(movd %imm0,%mm0)
	__(movd %imm1,%mm1)

	__(js 5f)		/* Sign bit set in %imm1. Need 5 digits   */
	__(bsrq %imm1,%imm0)
	__(rcmpb(%imm0_b,$31))
	__(jae 4f)		/* Some high bits in %imm1.  Need 4 digits   */
	__(testl %imm1_l,%imm1_l)
	__(movd %mm0,%imm0)
	__(jz _SPmakeu64)
	
	/* Need 3 digits   */
	
	__(movq $three_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(3)))
	__(movq %mm0,misc_data_offset(%arg_z))
	__(movd %mm1,misc_data_offset+8(%arg_z))
	__(ret)
4:	__(movq $four_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(4)))
	__(jmp 6f)
5:	__(movq $five_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(5)))
6:	__(movq %mm0,misc_data_offset(%arg_z))
	__(movq %mm0,misc_data_offset+8(%arg_z))
	__(ret)
_endfn

_spentry(misc_ref)
	__(movb $tagmask,%imm0_b)
	__(andb %arg_y_b,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne 0f)
	__(testb $fixnummask,%arg_z_b)
	__(jne 1f)
	__(movq misc_header_offset(%arg_y),%imm0)
        __(xorb %imm0_b,%imm0_b)
	__(shrq $num_subtag_bits-fixnumshift,%imm0)
	__(cmpq %imm0,%arg_z)
	__(jae 2f)
	__(movb misc_subtag_offset(%arg_y),%imm1_b)
        __(jmp C(misc_ref_common))
        
0:      __(uuo_error_reg_not_tag(Rarg_y,tag_misc))
1:      __(uuo_error_reg_not_fixnum(Rarg_z))
2:      __(uuo_error_vector_bounds(Rarg_z,Rarg_y))        
_endsubp(misc_ref)
	
/* %imm1.b = subtag, %arg_y = uvector, %arg_z = index.  */
/* Bounds/type-checking done in caller  */
	
_startfn(C(misc_ref_common))
	__(movzbl %imm1_b,%imm1_l)
        __(lea local_label(misc_ref_jmp)(%rip),%imm2)
	__(jmp *(%imm2,%imm1,8))
	.p2align 3
local_label(misc_ref_jmp):	
	/* 00-0f   */
	.quad local_label(misc_ref_invalid) /* 00 even_fixnum   */
	.quad local_label(misc_ref_invalid) /* 01 imm_1   */
	.quad local_label(misc_ref_invalid) /* 02 imm_2   */
	.quad local_label(misc_ref_invalid) /* 03 cons   */
	.quad local_label(misc_ref_invalid) /* 04 tra_0   */
	.quad local_label(misc_ref_invalid) /* 05 nodeheader_0   */
	.quad local_label(misc_ref_invalid) /* 06 nodeheader_1   */
	.quad local_label(misc_ref_invalid) /* 07 immheader_0   */
	.quad local_label(misc_ref_invalid) /* 08 odd_fixnum   */
	.quad local_label(misc_ref_invalid) /* 09 immheader_1   */
	.quad local_label(misc_ref_invalid) /* 0a immheader_2   */
	.quad local_label(misc_ref_invalid) /* 0b nil   */
	.quad local_label(misc_ref_invalid) /* 0c tra_1   */
	.quad local_label(misc_ref_invalid) /* 0d misc   */
	.quad local_label(misc_ref_invalid) /* 0e symbol   */
	.quad local_label(misc_ref_invalid) /* 0f function   */
	/* 10-1f   */
	.quad local_label(misc_ref_invalid) /* 10 even_fixnum   */
	.quad local_label(misc_ref_invalid) /* 11 imm_1   */
	.quad local_label(misc_ref_invalid) /* 12 imm_2   */
	.quad local_label(misc_ref_invalid) /* 13 cons   */
	.quad local_label(misc_ref_invalid) /* 14 tra_0   */
	.quad local_label(misc_ref_node) /* 15 symbol_vector   */
	.quad local_label(misc_ref_node) /* 16 ratio   */
	.quad local_label(misc_ref_invalid) /* 17 immheader_0   */
	.quad local_label(misc_ref_invalid) /* 18 odd_fixnum   */
	.quad local_label(misc_ref_u32)	/* 19 bignum   */
	.quad local_label(misc_ref_u64) /* 1a macptr   */
	.quad local_label(misc_ref_invalid) /* 1b nil   */
	.quad local_label(misc_ref_invalid) /* 1c tra_1   */
	.quad local_label(misc_ref_invalid) /* 1d misc   */
	.quad local_label(misc_ref_invalid) /* 1e symbol   */
	.quad local_label(misc_ref_invalid) /* 1f function   */
	/* 20-2f   */
	.quad local_label(misc_ref_invalid) /* 20 even_fixnum   */
	.quad local_label(misc_ref_invalid) /* 21 imm_1   */
	.quad local_label(misc_ref_invalid) /* 22 imm_2   */
	.quad local_label(misc_ref_invalid) /* 23 cons   */
	.quad local_label(misc_ref_invalid) /* 24 tra_0   */
	.quad local_label(misc_ref_node) /* 25 catch_frame   */
	.quad local_label(misc_ref_node) /* 26 complex   */
	.quad local_label(misc_ref_invalid) /* 27 immheader_0   */
	.quad local_label(misc_ref_invalid) /* 28 odd_fixnum   */
	.quad local_label(misc_ref_u32)	/* 29 double_float   */
	.quad local_label(misc_ref_u64)  /* 2a dead_macptr   */
	.quad local_label(misc_ref_invalid) /* 2b nil   */
	.quad local_label(misc_ref_invalid) /* 2c tra_1   */
	.quad local_label(misc_ref_invalid) /* 2d misc   */
	.quad local_label(misc_ref_invalid) /* 2e symbol   */
	.quad local_label(misc_ref_invalid) /* 2f function   */
	/* 30-3f   */
	.quad local_label(misc_ref_invalid) /* 30 even_fixnum   */
	.quad local_label(misc_ref_invalid) /* 31 imm_1   */
	.quad local_label(misc_ref_invalid) /* 32 imm_2   */
	.quad local_label(misc_ref_invalid) /* 33 cons   */
	.quad local_label(misc_ref_invalid) /* 34 tra_0   */
	.quad local_label(misc_ref_node) /* 35 hash_vector   */
	.quad local_label(misc_ref_node) /* 36 struct   */
	.quad local_label(misc_ref_invalid) /* 37 immheader_0   */
	.quad local_label(misc_ref_invalid) /* 38 odd_fixnum   */
	.quad local_label(misc_ref_u32)	/* 39 xcode_vector   */
	.quad local_label(misc_ref_invalid) /* 3a immheader_2   */
	.quad local_label(misc_ref_invalid) /* 3b nil   */
	.quad local_label(misc_ref_invalid) /* 3c tra_1   */
	.quad local_label(misc_ref_invalid) /* 3d misc   */
	.quad local_label(misc_ref_invalid) /* 3e symbol   */
	.quad local_label(misc_ref_invalid) /* 3f function   */
	/* 40-4f   */
	.quad local_label(misc_ref_invalid) /* 40 even_fixnum   */
	.quad local_label(misc_ref_invalid) /* 41 imm_1   */
	.quad local_label(misc_ref_invalid) /* 42 imm_2   */
	.quad local_label(misc_ref_invalid) /* 43 cons   */
	.quad local_label(misc_ref_invalid) /* 44 tra_0   */
	.quad local_label(misc_ref_node) /* 45 pool   */
	.quad local_label(misc_ref_node) /* 46 istruct   */
	.quad local_label(misc_ref_invalid) /* 47 immheader_0   */
	.quad local_label(misc_ref_invalid) /* 48 odd_fixnum   */
	.quad local_label(misc_ref_invalid) /* 49 immheader_1   */
	.quad local_label(misc_ref_invalid) /* 4a immheader_2   */
	.quad local_label(misc_ref_invalid) /* 4b nil   */
	.quad local_label(misc_ref_invalid) /* 4c tra_1   */
	.quad local_label(misc_ref_invalid) /* 4d misc   */
	.quad local_label(misc_ref_invalid) /* 4e symbol   */
	.quad local_label(misc_ref_invalid) /* 4f function   */
	/* 50-5f   */
	.quad local_label(misc_ref_invalid) /* 50 even_fixnum   */
	.quad local_label(misc_ref_invalid) /* 51 imm_1   */
	.quad local_label(misc_ref_invalid) /* 52 imm_2   */
	.quad local_label(misc_ref_invalid) /* 53 cons   */
	.quad local_label(misc_ref_invalid) /* 54 tra_0   */
	.quad local_label(misc_ref_node) /* 55 weak   */
	.quad local_label(misc_ref_node) /* 56 value_cell   */
	.quad local_label(misc_ref_invalid) /* 57 immheader_0   */
	.quad local_label(misc_ref_invalid) /* 58 odd_fixnum   */
	.quad local_label(misc_ref_invalid) /* 59 immheader_1   */
	.quad local_label(misc_ref_invalid) /* 5a immheader_2   */
	.quad local_label(misc_ref_invalid) /* 5b nil   */
	.quad local_label(misc_ref_invalid) /* 5c tra_1   */
	.quad local_label(misc_ref_invalid) /* 5d misc   */
	.quad local_label(misc_ref_invalid) /* 5e symbol   */
	.quad local_label(misc_ref_invalid) /* 5f function   */
	/* 60-6f   */
	.quad local_label(misc_ref_invalid) /* 60 even_fixnum   */
	.quad local_label(misc_ref_invalid) /* 61 imm_1   */
	.quad local_label(misc_ref_invalid) /* 62 imm_2   */
	.quad local_label(misc_ref_invalid) /* 63 cons   */
	.quad local_label(misc_ref_invalid) /* 64 tra_0   */
	.quad local_label(misc_ref_node) /* 65 package   */
	.quad local_label(misc_ref_node) /* 66 xfunction   */
	.quad local_label(misc_ref_invalid) /* 67 immheader_0   */
	.quad local_label(misc_ref_invalid) /* 68 odd_fixnum   */
	.quad local_label(misc_ref_invalid) /* 69 immheader_1   */
	.quad local_label(misc_ref_invalid) /* 6a immheader_2   */
	.quad local_label(misc_ref_invalid) /* 6b nil   */
	.quad local_label(misc_ref_invalid) /* 6c tra_1   */
	.quad local_label(misc_ref_invalid) /* 6d misc   */
	.quad local_label(misc_ref_invalid) /* 6e symbol   */
	.quad local_label(misc_ref_invalid) /* 6f function   */
	/* 70-7f   */
	.quad local_label(misc_ref_invalid) /* 70 even_fixnum   */
	.quad local_label(misc_ref_invalid) /* 71 imm_1   */
	.quad local_label(misc_ref_invalid) /* 72 imm_2   */
	.quad local_label(misc_ref_invalid) /* 73 cons   */
	.quad local_label(misc_ref_invalid) /* 74 tra_0   */
	.quad local_label(misc_ref_node) /* 75 slot_vector   */
	.quad local_label(misc_ref_node) /* 76 lock   */
	.quad local_label(misc_ref_invalid) /* 77 immheader_0   */
	.quad local_label(misc_ref_invalid) /* 78 odd_fixnum   */
	.quad local_label(misc_ref_invalid) /* 79 immheader_1   */
	.quad local_label(misc_ref_invalid) /* 7a immheader_2   */
	.quad local_label(misc_ref_invalid) /* 7b nil   */
	.quad local_label(misc_ref_invalid) /* 7c tra_1   */
	.quad local_label(misc_ref_invalid) /* 7d misc   */
	.quad local_label(misc_ref_invalid) /* 7e symbol   */
	.quad local_label(misc_ref_invalid) /* 7f function   */
	/* 80-8f   */
	.quad local_label(misc_ref_invalid) /* 80 even_fixnum   */
	.quad local_label(misc_ref_invalid) /* 81 imm_1   */
	.quad local_label(misc_ref_invalid) /* 82 imm_2   */
	.quad local_label(misc_ref_invalid) /* 83 cons   */
	.quad local_label(misc_ref_invalid) /* 84 tra_0   */
	.quad local_label(misc_ref_node) /* 85 lisp_thread   */
	.quad local_label(misc_ref_node) /* 86 instance   */
	.quad local_label(misc_ref_invalid) /* 87 immheader_0   */
	.quad local_label(misc_ref_invalid) /* 88 odd_fixnum   */
	.quad local_label(misc_ref_invalid) /* 89 immheader_1   */
	.quad local_label(misc_ref_invalid) /* 8a immheader_2   */
	.quad local_label(misc_ref_invalid) /* 8b nil   */
	.quad local_label(misc_ref_invalid) /* 8c tra_1   */
	.quad local_label(misc_ref_invalid) /* 8d misc   */
	.quad local_label(misc_ref_invalid) /* 8e symbol   */
	.quad local_label(misc_ref_invalid) /* 8f function   */
	/* 90-9f   */
	.quad local_label(misc_ref_invalid) /* 90 even_fixnum   */
	.quad local_label(misc_ref_invalid) /* 91 imm_1   */
	.quad local_label(misc_ref_invalid) /* 92 imm_2   */
	.quad local_label(misc_ref_invalid) /* 93 cons   */
	.quad local_label(misc_ref_invalid) /* 94 tra_0   */
	.quad local_label(misc_ref_function) /* 95 function_vector   */
	.quad local_label(misc_ref_invalid) /* 96 nodeheader_1   */
	.quad local_label(misc_ref_invalid) /* 97 immheader_0   */
	.quad local_label(misc_ref_invalid) /* 98 odd_fixnum   */
	.quad local_label(misc_ref_invalid) /* 99 immheader_1   */
	.quad local_label(misc_ref_invalid) /* 9a immheader_2   */
	.quad local_label(misc_ref_invalid) /* 9b nil   */
	.quad local_label(misc_ref_invalid) /* 9c tra_1   */
	.quad local_label(misc_ref_invalid) /* 9d misc   */
	.quad local_label(misc_ref_invalid) /* 9e symbol   */
	.quad local_label(misc_ref_invalid) /* 9f function   */
	/* a0-af   */
	.quad local_label(misc_ref_invalid) /* a0 even_fixnum   */
	.quad local_label(misc_ref_invalid) /* a1 imm_1   */
	.quad local_label(misc_ref_invalid) /* a2 imm_2   */
	.quad local_label(misc_ref_invalid) /* a3 cons   */
	.quad local_label(misc_ref_invalid) /* a4 tra_0   */
	.quad local_label(misc_ref_node) /* a5 arrayH   */
	.quad local_label(misc_ref_node) /* a6 vectorH   */
	.quad local_label(misc_ref_s16)	/* a7 s16   */
	.quad local_label(misc_ref_invalid) /* a8 odd_fixnum   */
	.quad local_label(misc_ref_invalid) /* a9 immheader_1   */
	.quad local_label(misc_ref_invalid) /* aa immheader_2   */
	.quad local_label(misc_ref_invalid) /* ab nil   */
	.quad local_label(misc_ref_invalid) /* ac tra_1   */
	.quad local_label(misc_ref_invalid) /* ad misc   */
	.quad local_label(misc_ref_invalid) /* ae symbol   */
	.quad local_label(misc_ref_invalid) /* af function   */
	/* b0-bf   */
	.quad local_label(misc_ref_invalid) /* b0 even_fixnum   */
	.quad local_label(misc_ref_invalid) /* b1 imm_1   */
	.quad local_label(misc_ref_invalid) /* b2 imm_2   */
	.quad local_label(misc_ref_invalid) /* b3 cons   */
	.quad local_label(misc_ref_invalid) /* b4 tra_0   */
	.quad local_label(misc_ref_invalid) /* b5 nodeheader_0   */
	.quad local_label(misc_ref_node) /* b6 simple_vector   */
	.quad local_label(misc_ref_u16) /* b7 immheader_0   */
	.quad local_label(misc_ref_invalid) /* b8 odd_fixnum   */
	.quad local_label(misc_ref_invalid) /* b9 immheader_1   */
	.quad local_label(misc_ref_invalid) /* ba immheader_2   */
	.quad local_label(misc_ref_invalid) /* bb nil   */
	.quad local_label(misc_ref_invalid) /* bc tra_1   */
	.quad local_label(misc_ref_invalid) /* bd misc   */
	.quad local_label(misc_ref_invalid) /* be symbol   */
	.quad local_label(misc_ref_invalid) /* bf function   */
	/* c0-cf   */
	.quad local_label(misc_ref_invalid) /* c0 even_fixnum   */
	.quad local_label(misc_ref_invalid) /* c1 imm_1   */
	.quad local_label(misc_ref_invalid) /* c2 imm_2   */
	.quad local_label(misc_ref_invalid) /* c3 cons   */
	.quad local_label(misc_ref_invalid) /* c4 tra_0   */
	.quad local_label(misc_ref_invalid) /* c5 nodeheader_0   */
	.quad local_label(misc_ref_invalid) /* c6 nodeheader_1   */
	.quad local_label(misc_ref_string) /* c7 simple_base_string   */
	.quad local_label(misc_ref_invalid) /* c8 odd_fixnum   */
	.quad local_label(misc_ref_new_string) /* c9 new_string_1   */
	.quad local_label(misc_ref_fixnum_vector) /* ca fixnum_vector   */
	.quad local_label(misc_ref_invalid) /* cb nil   */
	.quad local_label(misc_ref_invalid) /* cc tra_1   */
	.quad local_label(misc_ref_invalid) /* cd misc   */
	.quad local_label(misc_ref_invalid) /* ce symbol   */
	.quad local_label(misc_ref_invalid) /* cf function   */
	/* d0-df   */
	.quad local_label(misc_ref_invalid) /* d0 even_fixnum   */
	.quad local_label(misc_ref_invalid) /* d1 imm_1   */
	.quad local_label(misc_ref_invalid) /* d2 imm_2   */
	.quad local_label(misc_ref_invalid) /* d3 cons   */
	.quad local_label(misc_ref_invalid) /* d4 tra_0   */
	.quad local_label(misc_ref_invalid) /* d5 nodeheader_0   */
	.quad local_label(misc_ref_invalid) /* d6 nodeheader_1   */
	.quad local_label(misc_ref_s8)	/* d7 s8   */
	.quad local_label(misc_ref_invalid) /* d8 odd_fixnum   */
	.quad local_label(misc_ref_s32)	/* d9 s32   */
	.quad local_label(misc_ref_s64)	/* da s64   */
	.quad local_label(misc_ref_invalid) /* db nil   */
	.quad local_label(misc_ref_invalid) /* dc tra_1   */
	.quad local_label(misc_ref_invalid) /* dd misc   */
	.quad local_label(misc_ref_invalid) /* de symbol   */
	.quad local_label(misc_ref_invalid) /* df function   */
	/* e0-ef   */
	.quad local_label(misc_ref_invalid) /* e0 even_fixnum   */
	.quad local_label(misc_ref_invalid) /* e1 imm_1   */
	.quad local_label(misc_ref_invalid) /* e2 imm_2   */
	.quad local_label(misc_ref_invalid) /* e3 cons   */
	.quad local_label(misc_ref_invalid) /* e4 tra_0   */
	.quad local_label(misc_ref_invalid) /* e5 nodeheader_0   */
	.quad local_label(misc_ref_invalid) /* e6 nodeheader_1   */
	.quad local_label(misc_ref_u8)	/* e7 u8   */
	.quad local_label(misc_ref_invalid) /* e8 odd_fixnum   */
	.quad local_label(misc_ref_u32)	/* e9 u32   */
	.quad local_label(misc_ref_u64) /* ea u64   */
	.quad local_label(misc_ref_invalid) /* eb nil   */
	.quad local_label(misc_ref_invalid) /* ec tra_1   */
	.quad local_label(misc_ref_invalid) /* ed misc   */
	.quad local_label(misc_ref_invalid) /* ee symbol   */
	.quad local_label(misc_ref_invalid) /* ef function   */
	/* f0-ff   */
	.quad local_label(misc_ref_invalid) /* f0 even_fixnum   */
	.quad local_label(misc_ref_invalid) /* f1 imm_1   */
	.quad local_label(misc_ref_invalid) /* f2 imm_2   */
	.quad local_label(misc_ref_invalid) /* f3 cons   */
	.quad local_label(misc_ref_invalid) /* f4 tra_0   */
	.quad local_label(misc_ref_invalid) /* f5 nodeheader_0   */
	.quad local_label(misc_ref_invalid) /* f6 nodeheader_1   */
	.quad local_label(misc_ref_bit_vector) /* f7 bitvector   */
	.quad local_label(misc_ref_invalid) /* f8 odd_fixnum   */
	.quad local_label(misc_ref_single_float_vector) /* f9 single_float   */
	.quad local_label(misc_ref_double_float_vector) /* fa double_float   */
	.quad local_label(misc_ref_invalid) /* fb nil   */
	.quad local_label(misc_ref_invalid) /* fc tra_1   */
	.quad local_label(misc_ref_invalid) /* fd misc   */
	.quad local_label(misc_ref_invalid) /* fe symbol   */
	.quad local_label(misc_ref_invalid) /* ff function   */
	
	
	/* Node vector.  Functions are funny: the first  N words  */
	/* are treated as (UNSIGNED-BYTE 64), where N is the low  */
	/* 32 bits of the first word.  */
	
local_label(misc_ref_function):		
	__(movl misc_data_offset(%arg_y),%imm0_l)
	__(shl $fixnumshift,%imm0)
	__(rcmpq(%arg_z,%imm0))
	__(jb local_label(misc_ref_u64))
local_label(misc_ref_node):
	__(movq misc_data_offset(%arg_y,%arg_z),%arg_z)
	__(ret)
local_label(misc_ref_u64):
	__(movq misc_data_offset(%arg_y,%arg_z),%imm0)
	__(jmp _SPmakeu64)
local_label(misc_ref_double_float_vector):
	__(movsd misc_data_offset(%arg_y,%arg_z),%fp1)
	__(movq $double_float_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,double_float.size))
	__(movsd %fp1,double_float.value(%arg_z))
	__(ret)
local_label(misc_ref_fixnum_vector):	
	__(movq misc_data_offset(%arg_y,%arg_z),%imm0)
        __(box_fixnum(%imm0,%arg_z))
        __(ret)
local_label(misc_ref_s64):	
	__(movq misc_data_offset(%arg_y,%arg_z),%imm0)
	__(jmp _SPmakes64)
local_label(misc_ref_u32):
	__(movq %arg_z,%imm0)
	__(shr $1,%imm0)
	__(movl misc_data_offset(%arg_y,%imm0),%imm0_l)
	__(box_fixnum(%imm0,%arg_z))
	__(ret)
local_label(misc_ref_s32):
	__(movq %arg_z,%imm0)
	__(shr $1,%imm0)
	__(movslq misc_data_offset(%arg_y,%imm0),%imm0)
	__(box_fixnum(%imm0,%arg_z))
	__(ret)
local_label(misc_ref_single_float_vector):
	__(movq %arg_z,%imm0)
	__(shr $1,%imm0)
	__(movsd misc_data_offset(%arg_y,%imm0),%fp1)
	__(movd %fp1,%imm0_l)
	__(shl $32,%imm0)
	__(lea subtag_single_float(%imm0),%arg_z)
	__(ret)
local_label(misc_ref_u8):
	__(movq %arg_z,%imm0)
	__(shr $3,%imm0)
	__(movzbl misc_data_offset(%arg_y,%imm0),%imm0_l)
	__(box_fixnum(%imm0,%arg_z))
	__(ret)
local_label(misc_ref_s8):	
	__(movq %arg_z,%imm0)
	__(shr $3,%imm0)
	__(movsbq misc_data_offset(%arg_y,%imm0),%imm0)
	__(box_fixnum(%imm0,%arg_z))
	__(ret)
local_label(misc_ref_string):
	__(movq %arg_z,%imm0)
	__(shr $3,%imm0)
	__(movzbl misc_data_offset(%arg_y,%imm0),%imm0_l)
	__(shlq $charcode_shift,%imm0)
	__(leaq subtag_character(%imm0),%arg_z)
	__(ret)
local_label(misc_ref_new_string):
	__(movq %arg_z,%imm0)
	__(shr $1,%imm0)
	__(movl misc_data_offset(%arg_y,%imm0),%imm0_l)
	__(shlq $charcode_shift,%imm0)
	__(leaq subtag_character(%imm0),%arg_z)
	__(ret)        
local_label(misc_ref_u16):	
	__(movq %arg_z,%imm0)
	__(shrq $2,%imm0)
	__(movzwl misc_data_offset(%arg_y,%imm0),%imm0_l)
	__(box_fixnum(%imm0,%arg_z))
	__(ret)
local_label(misc_ref_s16):	
	__(movq %arg_z,%imm0)
	__(shrq $2,%imm0)
	__(movswq misc_data_offset(%arg_y,%imm0),%imm0)
	__(box_fixnum(%imm0,%arg_z))
	__(ret)
local_label(misc_ref_bit_vector):
	__(unbox_fixnum(%arg_z,%imm0))
	__(btq %imm0,misc_data_offset(%arg_y))
	__(setc %imm0_b)
	__(movzbl %imm0_b,%imm0_l)
	__(imull $fixnumone,%imm0_l,%arg_z_l)
	__(ret)
local_label(misc_ref_invalid):
	__(movq $XBADVEC,%arg_x)
	__(set_nargs(3))
	__(jmp _SPksignalerr)
_endfn(C(misc_ref_common))

/* like misc_ref, only the boxed subtag is in arg_x.   */
					
_spentry(subtag_misc_ref)
	__(movb $tagmask,%imm0_b)
	__(andb %arg_y_b,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne 0f)
	__(testb $fixnummask,%arg_z_b)
	__(jne 1f)
        __(movq misc_header_offset(%arg_y),%imm0)
        __(xorb %imm0_b,%imm0_b)
	__(shrq $num_subtag_bits-fixnumshift,%imm0)
	__(cmpq %imm0,%arg_z)
	__(jae 2f)
	__(unbox_fixnum(%arg_x,%imm1))
	__(jmp C(misc_ref_common))
0:      __(uuo_error_reg_not_tag(Rarg_y,tag_misc))
1:      __(uuo_error_reg_not_fixnum(Rarg_z))
2:      __(uuo_error_vector_bounds(Rarg_z,Rarg_y))
                        
_endsubp(subtag_misc_ref)

_spentry(subtag_misc_set)
	__(movb $tagmask,%imm0_b)
	__(andb %arg_x_b,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne 0f)
	__(testb $fixnummask,%arg_y_b)
	__(jne 1f)
	__(movq misc_header_offset(%arg_x),%imm0)
        __(xorb %imm0_b,%imm0_b)
	__(shrq $num_subtag_bits-fixnumshift,%imm0)
	__(cmpq %imm0,%arg_y)
	__(jae 2f)
	__(unbox_fixnum(%temp0,%imm1))
	__(jmp C(misc_set_common))
0:      __(uuo_error_reg_not_tag(Rarg_x,tag_misc))
1:      __(uuo_error_reg_not_fixnum(Rarg_y))
2:      __(uuo_error_vector_bounds(Rarg_y,Rarg_x))                        
_endsubp(subtag_misc_set)

_spentry(misc_set)
	__(movb $tagmask,%imm0_b)
	__(andb %arg_x_b,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne 0f)
	__(testb $fixnummask,%arg_y_b)
	__(jne 1f)
	__(movq misc_header_offset(%arg_x),%imm0)
        __(xorb %imm0_b,%imm0_b)
	__(shrq $num_subtag_bits-fixnumshift,%imm0)
	__(cmpq %imm0,%arg_y)
	__(jae 2f)
	__(movb misc_subtag_offset(%arg_x),%imm1_b)
	__(jmp C(misc_set_common))
	
0:      __(uuo_error_reg_not_tag(Rarg_x,tag_misc))
1:      __(uuo_error_reg_not_fixnum(Rarg_y))
2:      __(uuo_error_vector_bounds(Rarg_y,Rarg_x))                        
_endsubp(misc_set)
		
_startfn(C(misc_set_common))
	__(movzbl %imm1_b,%imm1_l)
        __(lea local_label(misc_set_jmp)(%rip),%imm2)
	__(jmp *(%imm2,%imm1,8))
	.p2align 3
local_label(misc_set_jmp):		
	/* 00-0f   */
	.quad local_label(misc_set_invalid) /* 00 even_fixnum   */
	.quad local_label(misc_set_invalid) /* 01 imm_1   */
	.quad local_label(misc_set_invalid) /* 02 imm_2   */
	.quad local_label(misc_set_invalid) /* 03 cons   */
	.quad local_label(misc_set_invalid) /* 04 tra_0   */
	.quad local_label(misc_set_invalid) /* 05 nodeheader_0   */
	.quad local_label(misc_set_invalid) /* 06 nodeheader_1   */
	.quad local_label(misc_set_invalid) /* 07 immheader_0   */
	.quad local_label(misc_set_invalid) /* 08 odd_fixnum   */
	.quad local_label(misc_set_invalid) /* 09 immheader_1   */
	.quad local_label(misc_set_invalid) /* 0a immheader_2   */
	.quad local_label(misc_set_invalid) /* 0b nil   */
	.quad local_label(misc_set_invalid) /* 0c tra_1   */
	.quad local_label(misc_set_invalid) /* 0d misc   */
	.quad local_label(misc_set_invalid) /* 0e symbol   */
	.quad local_label(misc_set_invalid) /* 0f function   */
	/* 10-1f   */
	.quad local_label(misc_set_invalid)	/* 10 even_fixnum   */
	.quad local_label(misc_set_invalid) /* 11 imm_1   */
	.quad local_label(misc_set_invalid) /* 12 imm_2   */
	.quad local_label(misc_set_invalid) /* 13 cons   */
	.quad local_label(misc_set_invalid)	/* 14 tra_0   */
	.quad _SPgvset /* 15 symbol_vector   */
	.quad _SPgvset /* 16 ratio   */
	.quad local_label(misc_set_invalid) /* 17 immheader_0   */
	.quad local_label(misc_set_invalid)	/* 18 odd_fixnum   */
	.quad local_label(misc_set_u32)	/* 19 bignum   */
	.quad local_label(misc_set_u64) /* 1a macptr   */
	.quad local_label(misc_set_invalid) /* 1b nil   */
	.quad local_label(misc_set_invalid)	/* 1c tra_1   */
	.quad local_label(misc_set_invalid)	/* 1d misc   */
	.quad local_label(misc_set_invalid)	/* 1e symbol   */
	.quad local_label(misc_set_invalid)	/* 1f function   */
	/* 20-2f   */
	.quad local_label(misc_set_invalid)	/* 20 even_fixnum   */
	.quad local_label(misc_set_invalid) /* 21 imm_1   */
	.quad local_label(misc_set_invalid) /* 22 imm_2   */
	.quad local_label(misc_set_invalid) /* 23 cons   */
	.quad local_label(misc_set_invalid)	/* 24 tra_0   */
	.quad _SPgvset /* 25 catch_frame   */
	.quad _SPgvset /* 26 complex   */
	.quad local_label(misc_set_invalid) /* 27 immheader_0   */
	.quad local_label(misc_set_invalid)	/* 28 odd_fixnum   */
	.quad local_label(misc_set_u32)	/* 29 double_float   */
	.quad local_label(misc_set_u64)  /* 2a dead_macptr   */
	.quad local_label(misc_set_invalid) /* 2b nil   */
	.quad local_label(misc_set_invalid)	/* 2c tra_1   */
	.quad local_label(misc_set_invalid)	/* 2d misc   */
	.quad local_label(misc_set_invalid)	/* 2e symbol   */
	.quad local_label(misc_set_invalid)	/* 2f function   */
	/* 30-3f   */
	.quad local_label(misc_set_invalid)	/* 30 even_fixnum   */
	.quad local_label(misc_set_invalid) /* 31 imm_1   */
	.quad local_label(misc_set_invalid) /* 32 imm_2   */
	.quad local_label(misc_set_invalid) /* 33 cons   */
	.quad local_label(misc_set_invalid)	/* 34 tra_0   */
	.quad _SPgvset /* 35 hash_vector   */
	.quad _SPgvset /* 36 struct   */
	.quad local_label(misc_set_invalid) /* 37 immheader_0   */
	.quad local_label(misc_set_invalid)	/* 38 odd_fixnum   */
	.quad local_label(misc_set_u32)	/* 39 xcode_vector   */
	.quad local_label(misc_set_invalid)  /* 3a immheader_2   */
	.quad local_label(misc_set_invalid) /* 3b nil   */
	.quad local_label(misc_set_invalid)	/* 3c tra_1   */
	.quad local_label(misc_set_invalid)	/* 3d misc   */
	.quad local_label(misc_set_invalid)	/* 3e symbol   */
	.quad local_label(misc_set_invalid)	/* 3f function   */
	/* 40-4f   */
	.quad local_label(misc_set_invalid)	/* 40 even_fixnum   */
	.quad local_label(misc_set_invalid) /* 41 imm_1   */
	.quad local_label(misc_set_invalid) /* 42 imm_2   */
	.quad local_label(misc_set_invalid) /* 43 cons   */
	.quad local_label(misc_set_invalid)	/* 44 tra_0   */
	.quad _SPgvset /* 45 pool   */
	.quad _SPgvset /* 46 istruct   */
	.quad local_label(misc_set_invalid) /* 47 immheader_0   */
	.quad local_label(misc_set_invalid)	/* 48 odd_fixnum   */
	.quad local_label(misc_set_invalid)	/* 49 immheader_1   */
	.quad local_label(misc_set_invalid)  /* 4a immheader_2   */
	.quad local_label(misc_set_invalid) /* 4b nil   */
	.quad local_label(misc_set_invalid)	/* 4c tra_1   */
	.quad local_label(misc_set_invalid)	/* 4d misc   */
	.quad local_label(misc_set_invalid)	/* 4e symbol   */
	.quad local_label(misc_set_invalid)	/* 4f function   */
	/* 50-5f   */
	.quad local_label(misc_set_invalid)	/* 50 even_fixnum   */
	.quad local_label(misc_set_invalid) /* 51 imm_1   */
	.quad local_label(misc_set_invalid) /* 52 imm_2   */
	.quad local_label(misc_set_invalid) /* 53 cons   */
	.quad local_label(misc_set_invalid)	/* 54 tra_0   */
	.quad _SPgvset /* 55 weak   */
	.quad _SPgvset /* 56 value_cell   */
	.quad local_label(misc_set_invalid) /* 57 immheader_0   */
	.quad local_label(misc_set_invalid)	/* 58 odd_fixnum   */
	.quad local_label(misc_set_invalid)	/* 59 immheader_1   */
	.quad local_label(misc_set_invalid)  /* 5a immheader_2   */
	.quad local_label(misc_set_invalid) /* 5b nil   */
	.quad local_label(misc_set_invalid)	/* 5c tra_1   */
	.quad local_label(misc_set_invalid)	/* 5d misc   */
	.quad local_label(misc_set_invalid)	/* 5e symbol   */
	.quad local_label(misc_set_invalid)	/* 5f function   */
	/* 60-6f   */
	.quad local_label(misc_set_invalid)	/* 60 even_fixnum   */
	.quad local_label(misc_set_invalid) /* 61 imm_1   */
	.quad local_label(misc_set_invalid) /* 62 imm_2   */
	.quad local_label(misc_set_invalid) /* 63 cons   */
	.quad local_label(misc_set_invalid)	/* 64 tra_0   */
	.quad _SPgvset /* 65 package   */
	.quad _SPgvset /* 66 xfunction   */
	.quad local_label(misc_set_invalid) /* 67 immheader_0   */
	.quad local_label(misc_set_invalid)	/* 68 odd_fixnum   */
	.quad local_label(misc_set_invalid)	/* 69 immheader_1   */
	.quad local_label(misc_set_invalid)  /* 6a immheader_2   */
	.quad local_label(misc_set_invalid) /* 6b nil   */
	.quad local_label(misc_set_invalid)	/* 6c tra_1   */
	.quad local_label(misc_set_invalid)	/* 6d misc   */
	.quad local_label(misc_set_invalid)	/* 6e symbol   */
	.quad local_label(misc_set_invalid)	/* 6f function   */
	/* 70-7f   */
	.quad local_label(misc_set_invalid)	/* 70 even_fixnum   */
	.quad local_label(misc_set_invalid) /* 71 imm_1   */
	.quad local_label(misc_set_invalid) /* 72 imm_2   */
	.quad local_label(misc_set_invalid) /* 73 cons   */
	.quad local_label(misc_set_invalid)	/* 74 tra_0   */
	.quad _SPgvset /* 75 slot_vector   */
	.quad _SPgvset /* 76 lock   */
	.quad local_label(misc_set_invalid) /* 77 immheader_0   */
	.quad local_label(misc_set_invalid)	/* 78 odd_fixnum   */
	.quad local_label(misc_set_invalid)	/* 79 immheader_1   */
	.quad local_label(misc_set_invalid)  /* 7a immheader_2   */
	.quad local_label(misc_set_invalid) /* 7b nil   */
	.quad local_label(misc_set_invalid)	/* 7c tra_1   */
	.quad local_label(misc_set_invalid)	/* 7d misc   */
	.quad local_label(misc_set_invalid)	/* 7e symbol   */
	.quad local_label(misc_set_invalid)	/* 7f function   */
	/* 80-8f   */
	.quad local_label(misc_set_invalid)	/* 80 even_fixnum   */
	.quad local_label(misc_set_invalid) /* 81 imm_1   */
	.quad local_label(misc_set_invalid) /* 82 imm_2   */
	.quad local_label(misc_set_invalid) /* 83 cons   */
	.quad local_label(misc_set_invalid)	/* 84 tra_0   */
	.quad _SPgvset /* 85 lisp_thread   */
	.quad _SPgvset /* 86 instance   */
	.quad local_label(misc_set_invalid) /* 87 immheader_0   */
	.quad local_label(misc_set_invalid)	/* 88 odd_fixnum   */
	.quad local_label(misc_set_invalid)	/* 89 immheader_1   */
	.quad local_label(misc_set_invalid)  /* 8a immheader_2   */
	.quad local_label(misc_set_invalid) /* 8b nil   */
	.quad local_label(misc_set_invalid)	/* 8c tra_1   */
	.quad local_label(misc_set_invalid)	/* 8d misc   */
	.quad local_label(misc_set_invalid)	/* 8e symbol   */
	.quad local_label(misc_set_invalid)	/* 8f function   */
	/* 90-9f   */
	.quad local_label(misc_set_invalid)	/* 90 even_fixnum   */
	.quad local_label(misc_set_invalid) /* 91 imm_1   */
	.quad local_label(misc_set_invalid) /* 92 imm_2   */
	.quad local_label(misc_set_invalid) /* 93 cons   */
	.quad local_label(misc_set_invalid)	/* 94 tra_0   */
	.quad local_label(misc_set_function) /* 95 function_vector   */
	.quad local_label(misc_set_invalid) /* 96 nodeheader_1   */
	.quad local_label(misc_set_invalid) /* 97 immheader_0   */
	.quad local_label(misc_set_invalid)	/* 98 odd_fixnum   */
	.quad local_label(misc_set_invalid)	/* 99 immheader_1   */
	.quad local_label(misc_set_invalid)  /* 9a immheader_2   */
	.quad local_label(misc_set_invalid) /* 9b nil   */
	.quad local_label(misc_set_invalid)	/* 9c tra_1   */
	.quad local_label(misc_set_invalid)	/* 9d misc   */
	.quad local_label(misc_set_invalid)	/* 9e symbol   */
	.quad local_label(misc_set_invalid)	/* 9f function   */
	/* a0-af   */
	.quad local_label(misc_set_invalid)	/* a0 even_fixnum   */
	.quad local_label(misc_set_invalid) /* a1 imm_1   */
	.quad local_label(misc_set_invalid) /* a2 imm_2   */
	.quad local_label(misc_set_invalid) /* a3 cons   */
	.quad local_label(misc_set_invalid)	/* a4 tra_0   */
	.quad _SPgvset /* a5 arrayH   */
	.quad _SPgvset /* a6 vectorH   */
	.quad local_label(misc_set_s16)	/* a7 s16   */
	.quad local_label(misc_set_invalid)	/* a8 odd_fixnum   */
	.quad local_label(misc_set_invalid)	/* a9 immheader_1   */
	.quad local_label(misc_set_invalid)  /* aa immheader_2   */
	.quad local_label(misc_set_invalid) /* ab nil   */
	.quad local_label(misc_set_invalid)	/* ac tra_1   */
	.quad local_label(misc_set_invalid)	/* ad misc   */
	.quad local_label(misc_set_invalid)	/* ae symbol   */
	.quad local_label(misc_set_invalid)	/* af function   */
	/* b0-bf   */
	.quad local_label(misc_set_invalid)	/* b0 even_fixnum   */
	.quad local_label(misc_set_invalid) /* b1 imm_1   */
	.quad local_label(misc_set_invalid) /* b2 imm_2   */
	.quad local_label(misc_set_invalid) /* b3 cons   */
	.quad local_label(misc_set_invalid)	/* b4 tra_0   */
	.quad local_label(misc_set_invalid) /* b5 nodeheader_0   */
	.quad _SPgvset /* b6 simple_vector   */
	.quad local_label(misc_set_u16) /* b7 immheader_0   */
	.quad local_label(misc_set_invalid)	/* b8 odd_fixnum   */
	.quad local_label(misc_set_invalid)	/* b9 immheader_1   */
	.quad local_label(misc_set_invalid) /* ba immheader_2   */
	.quad local_label(misc_set_invalid) /* bb nil   */
	.quad local_label(misc_set_invalid)	/* bc tra_1   */
	.quad local_label(misc_set_invalid)	/* bd misc   */
	.quad local_label(misc_set_invalid)	/* be symbol   */
	.quad local_label(misc_set_invalid)	/* bf function   */
	/* c0-cf   */
	.quad local_label(misc_set_invalid)	/* c0 even_fixnum   */
	.quad local_label(misc_set_invalid) /* c1 imm_1   */
	.quad local_label(misc_set_invalid) /* c2 imm_2   */
	.quad local_label(misc_set_invalid) /* c3 cons   */
	.quad local_label(misc_set_invalid)	/* c4 tra_0   */
	.quad local_label(misc_set_invalid) /* c5 nodeheader_0   */
	.quad local_label(misc_set_invalid) /* c6 nodeheader_1   */
	.quad local_label(misc_set_string) /* c7 simple_base_string   */
	.quad local_label(misc_set_invalid)	/* c8 odd_fixnum   */
	.quad local_label(misc_set_new_string)	/* c9 new_strin   */
	.quad local_label(misc_set_fixnum_vector)  /* ca fixnum_vector   */
	.quad local_label(misc_set_invalid) /* cb nil   */
	.quad local_label(misc_set_invalid)	/* cc tra_1   */
	.quad local_label(misc_set_invalid)	/* cd misc   */
	.quad local_label(misc_set_invalid)	/* ce symbol   */
	.quad local_label(misc_set_invalid)	/* cf function   */
	/* d0-df   */
	.quad local_label(misc_set_invalid)	/* d0 even_fixnum   */
	.quad local_label(misc_set_invalid) /* d1 imm_1   */
	.quad local_label(misc_set_invalid) /* d2 imm_2   */
	.quad local_label(misc_set_invalid) /* d3 cons   */
	.quad local_label(misc_set_invalid)	/* d4 tra_0   */
	.quad local_label(misc_set_invalid) /* d5 nodeheader_0   */
	.quad local_label(misc_set_invalid) /* d6 nodeheader_1   */
	.quad local_label(misc_set_s8)	/* d7 s8   */
	.quad local_label(misc_set_invalid)	/* d8 odd_fixnum   */
	.quad local_label(misc_set_s32)	/* d9 s32   */
	.quad local_label(misc_set_s64)	/* da s64   */
	.quad local_label(misc_set_invalid) /* db nil   */
	.quad local_label(misc_set_invalid)	/* dc tra_1   */
	.quad local_label(misc_set_invalid)	/* dd misc   */
	.quad local_label(misc_set_invalid)	/* de symbol   */
	.quad local_label(misc_set_invalid)	/* df function   */
	/* e0-ef   */
	.quad local_label(misc_set_invalid)	/* e0 even_fixnum   */
	.quad local_label(misc_set_invalid) /* e1 imm_1   */
	.quad local_label(misc_set_invalid) /* e2 imm_2   */
	.quad local_label(misc_set_invalid) /* e3 cons   */
	.quad local_label(misc_set_invalid)	/* e4 tra_0   */
	.quad local_label(misc_set_invalid) /* e5 nodeheader_0   */
	.quad local_label(misc_set_invalid) /* e6 nodeheader_1   */
	.quad local_label(misc_set_u8)	/* e7 u8   */
	.quad local_label(misc_set_invalid)	/* e8 odd_fixnum   */
	.quad local_label(misc_set_u32)	/* e9 u32   */
	.quad local_label(misc_set_u64) /* ea u64   */
	.quad local_label(misc_set_invalid) /* eb nil   */
	.quad local_label(misc_set_invalid)	/* ec tra_1   */
	.quad local_label(misc_set_invalid)	/* ed misc   */
	.quad local_label(misc_set_invalid)	/* ee symbol   */
	.quad local_label(misc_set_invalid)	/* ef function   */
	/* f0-ff   */
	.quad local_label(misc_set_invalid)	/* f0 even_fixnum   */
	.quad local_label(misc_set_invalid) /* f1 imm_1   */
	.quad local_label(misc_set_invalid) /* f2 imm_2   */
	.quad local_label(misc_set_invalid) /* f3 cons   */
	.quad local_label(misc_set_invalid)	/* f4 tra_0   */
	.quad local_label(misc_set_invalid) /* f5 nodeheader_0   */
	.quad local_label(misc_set_invalid) /* f6 nodeheader_1   */
	.quad local_label(misc_set_bit_vector) /* f7 bitvector   */
	.quad local_label(misc_set_invalid)	/* f8 odd_fixnum   */
	.quad local_label(misc_set_single_float_vector) /* f9 single_float   */
	.quad local_label(misc_set_double_float_vector) /* fa double_float   */
	.quad local_label(misc_set_invalid) /* fb nil   */
	.quad local_label(misc_set_invalid)	/* fc tra_1   */
	.quad local_label(misc_set_invalid)	/* fd misc   */
	.quad local_label(misc_set_invalid)	/* fe symbol   */
	.quad local_label(misc_set_invalid)	/* ff function   */

local_label(misc_set_function):			
	/* Functions are funny: the first  N words  */
	/* are treated as (UNSIGNED-BYTE 64), where N is the low  */
	/* 32 bits of the first word.   */
	__(movl misc_data_offset(%arg_x),%imm0_l)
	__(shl $fixnumshift,%imm0)
	__(rcmpq(%arg_y,%imm0))
	__(jae _SPgvset)
local_label(misc_set_u64):
	__(movq $~(target_most_positive_fixnum << fixnumshift),%imm0)
	__(testq %arg_z,%imm0)
	__(movq %arg_z,%imm0)
	__(jne 1f)
	__(sarq $fixnumshift,%imm0)
	__(jmp 9f)
1:	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movb misc_subtag_offset(%arg_z),%imm0_b)
	__(cmpb $subtag_bignum,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movq misc_header_offset(%arg_z),%imm0)
	__(cmpq $three_digit_bignum_header,%imm0)
	__(je 3f)
	__(cmpq $two_digit_bignum_header,%imm0)
	__(jne local_label(misc_set_bad))
	__(movq misc_data_offset(%arg_z),%imm0)
	__(testq %imm0,%imm0)
	__(js local_label(misc_set_bad))
	__(jmp 9f)
3:	__(movq misc_data_offset(%arg_z),%imm0)
	__(cmpl $0,misc_data_offset+8(%arg_z))
	__(jne local_label(misc_set_bad))
9:	__(movq %imm0,misc_data_offset(%arg_x,%arg_y))
	__(ret)
local_label(misc_set_fixnum_vector):
	__(movq %arg_z,%imm0)
	__(sarq $fixnumshift,%imm0)
	__(testb $fixnummask,%arg_z_b)
	__(jne local_label(misc_set_bad))
	__(movq %imm0,misc_data_offset(%arg_x,%arg_y))
	__(ret)	
local_label(misc_set_s64):
	__(movq %arg_z,%imm0)
	__(sarq $fixnumshift,%imm0)
	__(testb $fixnummask,%arg_z_b)
	__(je 9f)
1:	__(movb %arg_z_b,%imm0_b)
	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movb misc_subtag_offset(%arg_z),%imm0_b)
	__(cmpb $subtag_bignum,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movq misc_header_offset(%arg_z),%imm0)
	__(cmpq $two_digit_bignum_header,%imm0)
	__(movq misc_data_offset(%arg_z),%imm0)
	__(jne local_label(misc_set_bad))
9:	__(movq %imm0,misc_data_offset(%arg_x,%arg_y))
	__(ret)	
local_label(misc_set_bad):
	__(movq %arg_z,%arg_y)
	__(movq %arg_x,%arg_z)
	__(movq $XNOTELT,%arg_x)
	__(set_nargs(3))
	__(jmp _SPksignalerr)
local_label(misc_set_double_float_vector):	
	__(extract_lisptag(%arg_z,%imm0))
	__(cmpb $tag_misc,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movb misc_subtag_offset(%arg_z),%imm0_b)
	__(cmpb $subtag_double_float,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movq double_float.value(%arg_z),%imm0)
	__(movq %imm0,misc_dfloat_offset(%arg_x,%arg_y))
	__(ret)
local_label(misc_set_s32):	
	__(movq %arg_z,%imm0)
	__(movq %arg_y,%imm1)
	__(shlq $64-(32+fixnumshift),%imm0)
	__(shrq $1,%imm1)
	__(sarq $64-(32+fixnumshift),%imm0)
	__(cmpq %imm0,%arg_z)
	__(jne local_label(misc_set_bad))
	__(testb $fixnummask,%arg_z_b)
	__(jne local_label(misc_set_bad))
	__(shr $fixnumshift,%imm0)
	__(movl %imm0_l,misc_data_offset(%arg_x,%imm1))
	__(ret)
local_label(misc_set_single_float_vector):
	__(cmpb $tag_single_float,%arg_z_b)
	__(movq %arg_z,%imm0)
	__(movq %arg_y,%imm1)
	__(jne local_label(misc_set_bad))
	__(shrq $1,%imm1)
	__(shr $32,%imm0)
	__(movl %imm0_l,misc_data_offset(%arg_x,%imm1))
	__(ret)
local_label(misc_set_u32):
	__(movq %arg_y,%imm1)	
	__(movq $~(0xffffffff<<fixnumshift),%imm0)
	__(shrq $1,%imm1)
	__(testq %imm0,%arg_z)
	__(jne local_label(misc_set_bad))
	__(unbox_fixnum(%arg_z,%imm0))
	__(movl %imm0_l,misc_data_offset(%arg_x,%imm1))
	__(ret)
local_label(misc_set_bit_vector):	
	__(testq $~fixnumone,%arg_z)
	__(jne local_label(misc_set_bad))
	__(unbox_fixnum(%arg_y,%imm0))
	__(testb %arg_z_b,%arg_z_b)
	__(je local_label(misc_set_clr_bit))
local_label(misc_set_set_bit):	
	__(btsq %imm0,misc_data_offset(%arg_x))
	__(ret)
local_label(misc_set_clr_bit):	
	__(btrq %imm0,misc_data_offset(%arg_x))
	__(ret)
local_label(misc_set_u8):	
	__(testq $~(0xff<<fixnumshift),%arg_z)
	__(jne local_label(misc_set_bad))
	__(movq %arg_y,%imm1)
	__(unbox_fixnum(%arg_z,%imm0))
	__(shrq $3,%imm1)
	__(movb %imm0_b,misc_data_offset(%arg_x,%imm1))
	__(ret)
local_label(misc_set_s8):
	__(movq %arg_z,%imm0)
	__(shlq $64-(8+fixnumshift),%imm0)	
	__(sarq $64-(8+fixnumshift),%imm0)
	__(cmpq %arg_z,%imm0)
	__(jne local_label(misc_set_bad))
	__(testb $fixnummask,%arg_z_b)
	__(jne local_label(misc_set_bad))
	__(movq %arg_y,%imm1)
	__(shrq $fixnumshift,%imm0)
	__(shrq $3,%imm1)
	__(movb %imm0_b,misc_data_offset(%arg_x,%imm1))
	__(ret)
local_label(misc_set_string):
	__(cmpb $subtag_character,%arg_z_b)
	__(movq %arg_z,%imm0)
	__(jne local_label(misc_set_bad))
	__(movq %arg_y,%imm1)
	__(shrq $charcode_shift,%imm0)
	__(shrq $3,%imm1)
	__(movb %imm0_b,misc_data_offset(%arg_x,%imm1))
	__(ret)
local_label(misc_set_new_string):
	__(cmpb $subtag_character,%arg_z_b)
	__(movq %arg_z,%imm0)
	__(jne local_label(misc_set_bad))
	__(movq %arg_y,%imm1)
	__(shrq $charcode_shift,%imm0)
	__(shrq $1,%imm1)
	__(movl %imm0_l,misc_data_offset(%arg_x,%imm1))
	__(ret)        
local_label(misc_set_s16):	
	__(movq %arg_z,%imm0)
	__(movq %arg_y,%imm1)
	__(shlq $64-(16+fixnumshift),%imm0)	
	__(shrq $2,%imm1)
	__(sarq $64-(16+fixnumshift),%imm0)
	__(cmpq %arg_z,%imm0)
	__(jne local_label(misc_set_bad))
	__(testb $fixnummask,%arg_z_b)
	__(jne local_label(misc_set_bad))
	__(shrq $fixnumshift,%imm0)
	__(movw %imm0_w,misc_data_offset(%arg_x,%imm1))
	__(ret)
local_label(misc_set_u16):
	__(movq %arg_y,%imm1)
	__(testq $~(0xffff<<fixnumshift),%arg_z)
	__(jne local_label(misc_set_bad))
	__(shrq $2,%imm1)
	__(unbox_fixnum(%arg_z,%imm0))
	__(movw %imm0_w,misc_data_offset(%arg_x,%imm1))
	__(ret)
local_label(misc_set_invalid):
	__(push $XSETBADVEC)
	__(set_nargs(4))
	__(jmp _SPksignalerr)
_endfn(C(misc_set_common))
	
/* ret1valn returns "1 multiple value" when a called function does not   */
/* return multiple values.  Its presence on the stack (as a return address)   */
/* identifies the stack frame to code which returns multiple values.   */

_spentry(Fret1valn)
	.globl C(ret1valn)
__(tra(C(ret1valn)))
        __(movq (%rsp),%ra0)
        __(movq %arg_z,(%rsp))
	__(set_nargs(1))
	__(jmpq *%ra0)
_endsubp(Fret1valn)
	

_spentry(nvalret)
	.globl C(nvalret)			
C(nvalret):	
	__(ref_global(ret1val_addr,%temp1))
	__(cmpq lisp_frame.savera0(%rbp),%temp1)
	__(je 1f)
	__(testl %nargs,%nargs)
	__(movl $nil_value,%arg_z_l)
	__(cmovneq -node_size(%rsp,%nargs_q),%arg_z)
	__(leaveq)
        __(ret)

	
/* actually need to return values ; always need to copy   */
1:	__(leaq 2*node_size(%rbp),%imm1)
	__(movq (%imm1),%ra0)
	__(addq $node_size,%imm1)
	__(movq 0(%rbp),%rbp)
	__(leaq (%rsp,%nargs_q),%temp0)
	__(xorl %imm0_l,%imm0_l)
	__(jmp 3f)
2:	__(movq -node_size(%temp0),%temp1)
	__(subq $node_size,%temp0)
	__(addl $node_size,%imm0_l)
	__(movq %temp1,-node_size(%imm1))
	__(subq $node_size,%imm1)
3:	__(cmpl %imm0_l,%nargs)  ;
	__(jne 2b)
	__(movq %imm1,%rsp)
	__(jmp *%ra0)	
_endsubp(nvalret)
	
_spentry(jmpsym)
	__(jump_fname())
_endsubp(jmpsym)

_spentry(jmpnfn)
	__(movq %temp0,%fn)
	__(jmp *%fn)
_endsubp(jmpnfn)

_spentry(funcall)
	__(do_funcall())
_endsubp(funcall)

_spentry(mkcatch1v)
	__(nMake_Catch(0))
	__(ret)
_endsubp(mkcatch1v)

_spentry(mkunwind)
	__(movq $undefined,%arg_z)
	__(Make_Catch(fixnumone))
	__(jmp *%ra0)
_endsubp(mkunwind)
        
/* this takes a return address in %ra0; it's "new" in that it does the
   double binding of *interrupt-level* out-of-line */
_spentry(nmkunwind)
	__(movq rcontext(tcr.tlb_pointer),%arg_x)
        __(movq INTERRUPT_LEVEL_BINDING_INDEX(%arg_x),%arg_y)
	__(push %arg_y)
	__(push $INTERRUPT_LEVEL_BINDING_INDEX)
	__(push rcontext(tcr.db_link))
	__(movq %rsp,rcontext(tcr.db_link))
	__(movq $-1<<fixnumshift,INTERRUPT_LEVEL_BINDING_INDEX(%arg_x))
	__(movq $undefined,%arg_z)
	__(Make_Catch(fixnumone))
        __(movq %arg_y,%arg_z)
        __(jmp _SPbind_interrupt_level)
_endsubp(nmkunwind)

_spentry(mkcatchmv)
	__(nMake_Catch(fixnumone))
	__(ret)
_endsubp(mkcatchmv)
        
_spentry(throw)
	__(movq rcontext(tcr.catch_top),%imm1)
	__(xorl %imm0_l,%imm0_l)
	__(movq (%rsp,%nargs_q),%temp0)	/* temp0 = tag   */
	__(jmp local_label(_throw_test))
local_label(_throw_loop):
	__(cmpq %temp0,catch_frame.catch_tag(%imm1))
	__(je local_label(_throw_found))
	__(movq catch_frame.link(%imm1),%imm1)
	__(addq $fixnum_one,%imm0)
local_label(_throw_test):
	__(testq %imm1,%imm1)
	__(jne local_label(_throw_loop))
        __(push %ra0)
	__(uuo_error_reg_not_tag(Rtemp0,subtag_catch_frame))
        __(pop %ra0)
	__(jmp _SPthrow)
local_label(_throw_found):	
	__(testb $fulltagmask,catch_frame.mvflag(%imm1))
	__(jne local_label(_throw_multiple))
	__(testl %nargs,%nargs)
	__(movl $nil_value,%arg_z_l)
	__(je local_label(_throw_one_value))
	__(movq -node_size(%rsp,%nargs_q),%arg_z)
	__(add %nargs_q,%rsp)
local_label(_throw_one_value):
	__(lea local_label(_threw_one_value)(%rip),%ra0)
	__(jmp _SPnthrow1value)
__(tra(local_label(_threw_one_value)))
	__(movq rcontext(tcr.catch_top),%temp0)
	__(movq catch_frame.db_link(%temp0),%imm0)
	__(movq rcontext(tcr.db_link),%imm1)
	__(cmpq %imm0,%imm1)
	__(jz local_label(_threw_one_value_dont_unbind))
	__(lea local_label(_threw_one_value_dont_unbind)(%rip),%ra0)
        __(push %ra0)
	__(jmp _SPunbind_to)
__(tra(local_label(_threw_one_value_dont_unbind)))
	__(movq catch_frame.rbp(%temp0),%rbp)
	__(movq catch_frame.foreign_sp(%temp0),%imm0)
	__(movq catch_frame.xframe(%temp0),%imm1)
        __(movq %imm0,rcontext(tcr.foreign_sp))
	__(movq %imm1,rcontext(tcr.xframe))
	__(movq catch_frame.rsp(%temp0),%rsp)
	__(movq catch_frame.link(%temp0),%imm1)
	__(movq catch_frame._save0(%temp0),%save0)
	__(movq catch_frame._save1(%temp0),%save1)
	__(movq catch_frame._save2(%temp0),%save2)
	__ifndef(`TCR_IN_GPR')
	__(movq catch_frame._save3(%temp0),%save3)
	__endif
	__(movq %imm1,rcontext(tcr.catch_top))
	__(movq catch_frame.pc(%temp0),%ra0)
	__(lea -(tsp_frame.fixed_overhead+fulltag_misc)(%temp0),%imm1)
	__(movq (%imm1),%imm1)
        __(movq %imm1,rcontext(tcr.save_tsp))
        __(movq %imm1,rcontext(tcr.next_tsp))
	__(jmp *%ra0)
local_label(_throw_multiple):
	__(lea local_label(_threw_multiple)(%rip),%ra0)
	__(jmp _SPnthrowvalues)
__(tra(local_label(_threw_multiple)))
	__(movq rcontext(tcr.catch_top),%temp0)
	__(movq catch_frame.db_link(%temp0),%imm0)
	__(movq rcontext(tcr.db_link),%imm1)
	__(cmpq %imm0,%imm1)
	__(je local_label(_threw_multiple_dont_unbind))
	__(leaq local_label(_threw_multiple_dont_unbind)(%rip),%ra0)
        __(push %ra0)
	__(jmp _SPunbind_to)
__(tra(local_label(_threw_multiple_dont_unbind)))
	/* Copy multiple values from the current %rsp to the target %rsp   */
	__(lea (%rsp,%nargs_q),%imm0)
	__(movq catch_frame.rsp(%temp0),%imm1)
	__(jmp local_label(_threw_multiple_push_test))
local_label(_threw_multiple_push_loop):
	__(subq $node_size,%imm0)
	__(subq $node_size,%imm1)
	__(movq (%imm0),%arg_z)
	__(movq %arg_z,(%imm1))
local_label(_threw_multiple_push_test):		
	__(cmpq %imm0,%rsp)
	__(jne local_label(_threw_multiple_push_loop))
	/* target %rsp is now in %imm1   */
	__(movq catch_frame.rbp(%temp0),%rbp)
	__(movq catch_frame.foreign_sp(%temp0),%imm0)
        __(movq %imm0,rcontext(tcr.foreign_sp))        
	__(movq catch_frame.xframe(%temp0),%imm0)
	__(movq %imm0,rcontext(tcr.xframe))
	__(movq %imm1,%rsp)
	__(movq catch_frame.link(%temp0),%imm1)		
	__(movq catch_frame._save0(%temp0),%save0)
	__(movq catch_frame._save1(%temp0),%save1)
	__(movq catch_frame._save2(%temp0),%save2)
	__ifndef(`TCR_IN_GPR')
	__(movq catch_frame._save3(%temp0),%save3)
	__endif
	__(movq %imm1,rcontext(tcr.catch_top))
	__(movq catch_frame.pc(%temp0),%ra0)
	__(lea -(tsp_frame.fixed_overhead+fulltag_misc)(%temp0),%imm1)
	__(movq (%imm1),%imm1)
        __(movq %imm1,rcontext(tcr.save_tsp))
        __(movq %imm1,rcontext(tcr.next_tsp))
	__(jmp *%ra0)
_endsubp(throw)

/* This takes N multiple values atop the vstack.   */
_spentry(nthrowvalues)
	__(movb $1,rcontext(tcr.unwinding))
local_label(_nthrowv_nextframe):
	__(subq $fixnumone,%imm0)
	__(js local_label(_nthrowv_done))
	__(movd %imm0,%mm1)
	__(movq rcontext(tcr.catch_top),%temp0)
	__(movq catch_frame.link(%temp0),%imm1)
	__(movq catch_frame.db_link(%temp0),%imm0)
	__(movq %imm1,rcontext(tcr.catch_top))
	__(cmpq %imm0,rcontext(tcr.db_link))
	__(jz local_label(_nthrowv_dont_unbind))
	__(push %ra0)
	__(leaq local_label(_nthrowv_back_from_unbind)(%rip),%ra0)
        __(push %ra0)
	__(jmp _SPunbind_to)
__(tra(local_label(_nthrowv_back_from_unbind)))

	__(pop %ra0)
local_label(_nthrowv_dont_unbind):
	__(cmpb $unbound_marker,catch_frame.catch_tag(%temp0))
	__(je local_label(_nthrowv_do_unwind))
/* A catch frame.  If the last one, restore context from there.   */
	__(movd %mm1,%imm0)
	__(testq %imm0,%imm0)	/* last catch frame ?   */
	__(jne local_label(_nthrowv_skip))
	__(movq catch_frame.xframe(%temp0),%save0)
	__(movq %save0,rcontext(tcr.xframe))
	__(leaq (%rsp,%nargs_q),%save1)
	__(movq catch_frame.rsp(%temp0),%save2)
	__(movq %nargs_q,%save0)
	__(jmp local_label(_nthrowv_push_test))
local_label(_nthrowv_push_loop):
	__(subq $node_size,%save1)
	__(subq $node_size,%save2)
	__(movq (%save1),%temp1)
	__(movq %temp1,(%save2))
local_label(_nthrowv_push_test):
	__(subq $node_size,%save0)
	__(jns local_label(_nthrowv_push_loop))
	__(movq catch_frame.xframe(%temp0),%save0)
	__(movq %save0,rcontext(tcr.xframe))
	__(movq %save2,%rsp)
	__(movq catch_frame.rbp(%temp0),%rbp)
	__ifndef(`TCR_IN_GPR')
	__(movq catch_frame._save3(%temp0),%save3)
	__endif
	__(movq catch_frame._save2(%temp0),%save2)
	__(movq catch_frame._save1(%temp0),%save1)
	__(movq catch_frame._save0(%temp0),%save0)
	__(movq catch_frame.foreign_sp(%temp0),%stack_temp)
        __(movq %stack_temp,rcontext(tcr.foreign_sp))        
local_label(_nthrowv_skip):	
	__(movq -(tsp_frame.fixed_overhead+fulltag_misc)(%temp0),%imm1)
        __(movq %imm1,rcontext(tcr.save_tsp))        
        __(movq %imm1,rcontext(tcr.next_tsp))
	__(movd %mm1,%imm0)
	__(jmp local_label(_nthrowv_nextframe))
local_label(_nthrowv_do_unwind):	
/* This is harder.  Call the cleanup code with the multiple values and   */
/* nargs, the throw count, and the caller's return address in a temp  */
/* stack frame.   */
	__(leaq (%rsp,%nargs_q),%save1)
	__(push catch_frame._save0(%temp0))
	__(push catch_frame._save1(%temp0))
	__(push catch_frame._save2(%temp0))
	__ifndef(`TCR_IN_GPR')
	__(push catch_frame._save3(%temp0))
	__endif
	__(push catch_frame.pc(%temp0))
	__(movq catch_frame.rbp(%temp0),%rbp)
        __(movq catch_frame.xframe(%temp0),%stack_temp)
	__(movq catch_frame.rsp(%temp0),%arg_x)
        __(movq %stack_temp,rcontext(tcr.xframe))
	__(movq catch_frame.foreign_sp(%temp0),%stack_temp)
        __(movq %stack_temp,rcontext(tcr.foreign_sp))        
	/* Discard the catch frame, so we can build a temp frame   */
	__(movq -(tsp_frame.fixed_overhead+fulltag_misc)(%temp0),%imm1)
        __(movq %imm1,rcontext(tcr.save_tsp))
        __(movq %imm1,rcontext(tcr.next_tsp))
	/* tsp overhead, nargs, throw count, ra0   */
	__(dnode_align(%nargs_q,(tsp_frame.fixed_overhead+(3*node_size)),%imm0))
	__(TSP_Alloc_Var(%imm0,%imm1))

	__(movq %nargs_q,(%imm1))
	__(movq %ra0,node_size(%imm1))
	__(movq %mm1,node_size*2(%imm1))
	__(leaq node_size*3(%imm1),%imm1)
	__(jmp local_label(_nthrowv_tpushtest))
local_label(_nthrowv_tpushloop):
	__(movq -node_size(%save1),%temp0)
	__(subq $node_size,%save1)
	__(movq %temp0,(%imm1))
	__(addq $node_size,%imm1)
local_label(_nthrowv_tpushtest):
	__(subl $node_size,%nargs)
	__(jns local_label(_nthrowv_tpushloop))
	__(pop %xfn)
	__ifndef(`TCR_IN_GPR')
	__(pop %save3)
	__endif
	__(pop %save2)
	__(pop %save1)
	__(pop %save0)
	__(movq %arg_x,%rsp)
/* Ready to call cleanup code. set up tra, jmp to %xfn   */
	__(leaq local_label(_nthrowv_called_cleanup)(%rip),%ra0)
        __(push %ra0)
	__(movb $0,rcontext(tcr.unwinding))
	__(jmp *%xfn)
__(tra(local_label(_nthrowv_called_cleanup)))

	__(movb $1,rcontext(tcr.unwinding))
	__(movq rcontext(tcr.save_tsp),%imm1)
	__(movq tsp_frame.data_offset+(0*node_size)(%imm1),%nargs_q)
	__(movq tsp_frame.data_offset+(1*node_size)(%imm1),%ra0)
	__(movq tsp_frame.data_offset+(2*node_size)(%imm1),%mm1)
	__(movq %nargs_q,%imm0)
	__(addq $tsp_frame.fixed_overhead+(node_size*3),%imm1)
	__(jmp local_label(_nthrowv_tpoptest))
local_label(_nthrowv_tpoploop):	
	__(push (%imm1))
	__(addq $node_size,%imm1)
local_label(_nthrowv_tpoptest):	
	__(subq $node_size,%imm0)
	__(jns local_label(_nthrowv_tpoploop))
	__(movq rcontext(tcr.save_tsp),%imm1)
	__(movq (%imm1),%imm1)
        __(movq %imm1,rcontext(tcr.save_tsp))
        __(movq %imm1,rcontext(tcr.next_tsp))
	__(movd %mm1,%imm0)
	__(jmp local_label(_nthrowv_nextframe))
local_label(_nthrowv_done):
	__(movb $0,rcontext(tcr.unwinding))
	__(check_pending_interrupt(%imm0))
local_label(_nthrowv_return):	
	__(jmp *%ra0)	
_endsubp(nthrowvalues)

/* This is a (slight) optimization.  When running an unwind-protect,  */
/* save the single value and the throw count in the tstack frame.  */
/* Note that this takes a single value in arg_z.  */
	
_spentry(nthrow1value)
	__(movb $1,rcontext(tcr.unwinding))
local_label(_nthrow1v_nextframe):
	__(subq $fixnumone,%imm0)
	__(js local_label(_nthrow1v_done))
	__(movd %imm0,%mm1)
	__(movq rcontext(tcr.catch_top),%temp0)
	__(movq catch_frame.link(%temp0),%imm1)
	__(movq catch_frame.db_link(%temp0),%imm0)
	__(movq %imm1,rcontext(tcr.catch_top))
	__(cmpq %imm0,rcontext(tcr.db_link))
	__(jz local_label(_nthrow1v_dont_unbind))
	__(push %ra0)
	__(leaq local_label(_nthrow1v_back_from_unbind)(%rip),%ra0)
        __(push %ra0)
	__(jmp _SPunbind_to)
__(tra(local_label(_nthrow1v_back_from_unbind)))

	__(pop %ra0)
local_label(_nthrow1v_dont_unbind):
	__(cmpb $unbound_marker,catch_frame.catch_tag(%temp0))
	__(je local_label(_nthrow1v_do_unwind))
/* A catch frame.  If the last one, restore context from there.   */
	__(movd %mm1,%imm0)
	__(testq %imm0,%imm0)	/* last catch frame ?   */
	__(jne local_label(_nthrow1v_skip))
	__(movq catch_frame.xframe(%temp0),%save0)
	__(movq %save0,rcontext(tcr.xframe))
	__(leaq (%rsp,%nargs_q),%save1)
	__(movq catch_frame.xframe(%temp0),%save0)
	__(movq %save0,rcontext(tcr.xframe))
	__(movq catch_frame.rsp(%temp0),%rsp)
	__(movq catch_frame.rbp(%temp0),%rbp)
	__ifndef(`TCR_IN_GPR')
	__(movq catch_frame._save3(%temp0),%save3)
	__endif
	__(movq catch_frame._save2(%temp0),%save2)
	__(movq catch_frame._save1(%temp0),%save1)
	__(movq catch_frame._save0(%temp0),%save0)
	__(movq catch_frame.foreign_sp(%temp0),%stack_temp)
        __(movq %stack_temp,rcontext(tcr.foreign_sp))        
local_label(_nthrow1v_skip):	
	__(movq -(tsp_frame.fixed_overhead+fulltag_misc)(%temp0),%imm1)
        __(movq %imm1,rcontext(tcr.save_tsp))
        __(movq %imm1,rcontext(tcr.next_tsp))        
	__(movd %mm1,%imm0)
	__(jmp local_label(_nthrow1v_nextframe))
local_label(_nthrow1v_do_unwind):
	
/* This is harder, but not as hard (not as much BLTing) as the  */
/* multiple-value case.  */
	
	__(movq catch_frame.xframe(%temp0),%save0)
	__(movq %save0,rcontext(tcr.xframe))
	__(movq catch_frame._save0(%temp0),%save0)
	__(movq catch_frame._save1(%temp0),%save1)
	__(movq catch_frame._save2(%temp0),%save2)
	__ifndef(`TCR_IN_GPR')
	__(movq catch_frame._save3(%temp0),%save3)
	__endif
	__(movq catch_frame.pc(%temp0),%xfn)
	__(movq catch_frame.rbp(%temp0),%rbp)
	__(movq catch_frame.rsp(%temp0),%rsp)
	__(movq catch_frame.foreign_sp(%temp0),%stack_temp)
        __(movq %stack_temp,rcontext(tcr.foreign_sp))        
	/* Discard the catch frame, so we can build a temp frame   */
	__(movq -(tsp_frame.fixed_overhead+fulltag_misc)(%temp0),%imm1)
        __(movq %imm1,rcontext(tcr.save_tsp))
        __(movq %imm1,rcontext(tcr.next_tsp))        
	__(TSP_Alloc_Fixed((3*node_size),%imm1))
	__(addq $tsp_frame.fixed_overhead,%imm1)
	__(movq %ra0,(%imm1))
	__(movq %mm1,node_size*1(%imm1))
	__(movq %arg_z,node_size*2(%imm1))
/* Ready to call cleanup code. set up tra, jmp to %xfn   */
	__(leaq local_label(_nthrow1v_called_cleanup)(%rip),%ra0)
	__(movb $0,rcontext(tcr.unwinding))
        __(push %ra0)
	__(jmp *%xfn)
__(tra(local_label(_nthrow1v_called_cleanup)))

	__(movb $1,rcontext(tcr.unwinding))
	__(movq rcontext(tcr.save_tsp),%imm1)
	__(movq tsp_frame.data_offset+(0*node_size)(%imm1),%ra0)
	__(movq tsp_frame.data_offset+(1*node_size)(%imm1),%mm1)
	__(movq tsp_frame.data_offset+(2*node_size)(%imm1),%arg_z)

	__(movq (%imm1),%imm1)
        __(movq %imm1,rcontext(tcr.save_tsp))
        __(movq %imm1,rcontext(tcr.next_tsp))        
	__(movd %mm1,%imm0)
	__(jmp local_label(_nthrow1v_nextframe))
local_label(_nthrow1v_done):
	__(movb $0,rcontext(tcr.unwinding))
	__(check_pending_interrupt(%imm0))
local_label(_nthrow1v_return):	
	__(jmp *%ra0)	
_endsubp(nthrow1value)

/* This never affects the symbol's vcell   */
/* Non-null symbol in arg_y, new value in arg_z           */
	
_spentry(bind)
	__(movq symbol.binding_index(%arg_y),%temp0)
	__(cmpq rcontext(tcr.tlb_limit),%temp0)
	__(jb 0f)
	__(push %temp0)
	__(tlb_too_small())
0:	__(testq %temp0,%temp0)
	__(jz 9f)
	__(movq rcontext(tcr.tlb_pointer),%temp1)
	__(push (%temp1,%temp0))
	__(push %temp0)
	__(push rcontext(tcr.db_link))
	__(movq %rsp,rcontext(tcr.db_link))
	__(movq %arg_z,(%temp1,%temp0))
	__(jmp *%ra0)
9:	
	__(movq %arg_y,%arg_z)
	__(movq $XSYMNOBIND,%arg_y)
	__(set_nargs(2))
        __(push %ra0)
	__(jmp _SPksignalerr)	
_endsubp(bind)

/* arg_z = symbol: bind it to its current value  */
	
_spentry(bind_self)
	__(movq symbol.binding_index(%arg_z),%temp0)
	__(cmpq rcontext(tcr.tlb_limit),%temp0)
	__(jb 0f)
	__(push %temp0)
	__(tlb_too_small())
0:	__(testq %temp0,%temp0)
	__(jz 9f)
	__(movq rcontext(tcr.tlb_pointer),%temp1)
	__(cmpb $no_thread_local_binding_marker,(%temp0,%temp1))
	__(jz 2f)
	__(push (%temp1,%temp0))
	__(push %temp0)
	__(push rcontext(tcr.db_link))
	__(movq %rsp,rcontext(tcr.db_link))
	__(jmp *%ra0)
2:	__(movq symbol.vcell(%arg_z),%arg_y)
	__(push (%temp1,%temp0))
	__(push %temp0)
	__(push rcontext(tcr.db_link))
	__(movq %arg_y,(%temp1,%temp0))
	__(movq %rsp,rcontext(tcr.db_link))
	__(jmp *%ra0)
9:	__(movq $XSYMNOBIND,%arg_y)
	__(set_nargs(2))
        __(push %ra0)
	__(jmp _SPksignalerr)
_endsubp(bind_self)

_spentry(bind_nil)
	__(movq symbol.binding_index(%arg_z),%temp0)
	__(cmpq rcontext(tcr.tlb_limit),%temp0)
	__(jb 0f)
	__(push %temp0)
	__(tlb_too_small())
0:	__(testq %temp0,%temp0)
	__(jz 9f)
	__(movq rcontext(tcr.tlb_pointer),%temp1)
	__(push (%temp1,%temp0))
	__(push %temp0)
	__(push rcontext(tcr.db_link))
	__(movq %rsp,rcontext(tcr.db_link))
	__(movq $nil_value,(%temp1,%temp0))
	__(jmp *%ra0)
9:	__(movq $XSYMNOBIND,%arg_y)
	__(set_nargs(2))
        __(push %ra0)
	__(jmp _SPksignalerr)
_endsubp(bind_nil)

_spentry(bind_self_boundp_check)
	__(movq symbol.binding_index(%arg_z),%temp0)
	__(cmpq rcontext(tcr.tlb_limit),%temp0)
	__(jb 0f)
	__(push %temp0)
	__(tlb_too_small())
0:	__(testq %temp0,%temp0)
	__(jz 9f)
	__(movq rcontext(tcr.tlb_pointer),%temp1)
	__(cmpb $no_thread_local_binding_marker,(%temp1,%temp0))
	__(je 2f)
	__(cmpb $unbound_marker,(%temp1,%temp0))
	__(je 8f)
	__(push (%temp1,%temp0))
	__(push %temp0)
	__(push rcontext(tcr.db_link))
	__(movq %rsp,rcontext(tcr.db_link))
	__(jmp *%ra0)
2:	__(movq symbol.vcell(%arg_z),%arg_y)
	__(cmpb $unbound_marker,%arg_y_b)
	__(jz 8f)
	__(push (%temp1,%temp0))
	__(push %temp0)
	__(push rcontext(tcr.db_link))
	__(movq %rsp,rcontext(tcr.db_link))
	__(movq %arg_y,(%temp1,%temp0))
	__(jmp *%ra0)
8:	__(push %ra0)
        __(uuo_error_reg_unbound(Rarg_z))
	
9:	__(movq $XSYMNOBIND,%arg_y)
	__(set_nargs(2))
        __(push %ra0)
	__(jmp _SPksignalerr)
_endsubp(bind_self_boundp_check)

_spentry(conslist)
	__(movl $nil_value,%arg_z_l)
	__(testl %nargs,%nargs)
	__(jmp 2f)
1:	__(pop %arg_y)
	__(Cons(%arg_y,%arg_z,%arg_z))
	__(subl $node_size,%nargs)
2:	__(jnz 1b)
	__(jmp *%ra0)		
_endsubp(conslist)

/* do list*: last arg in arg_z, all others pushed, nargs set to #args pushed.  */
/* Cons, one cons cell at at time.  Maybe optimize this later.  */
	
_spentry(conslist_star)
	__(testl %nargs,%nargs)
	__(jmp 2f)
1:	__(pop %arg_y)
	__(Cons(%arg_y,%arg_z,%arg_z))
	__(subl $node_size,%nargs)
2:	__(jnz 1b)
	__(jmp *%ra0)		
_endsubp(conslist_star)

/* We always have to create a tsp frame (even if nargs is 0), so the compiler   */
/* doesn't get confused.   */
_spentry(stkconslist)
	__(movq %nargs_q,%imm1)
	__(addq %imm1,%imm1)
	__(movl $nil_value,%arg_z_l)
	__(dnode_align(%imm1,tsp_frame.fixed_overhead,%imm1))
	__(TSP_Alloc_Var(%imm1,%imm0))
	__(addq $fulltag_cons,%imm0)
	__(testl %nargs,%nargs)
	__(jmp 2f)
1:	__(pop %temp0)
	__(_rplaca(%imm0,%temp0))
	__(_rplacd(%imm0,%arg_z))
	__(movq %imm0,%arg_z)
	__(add $cons.size,%imm0)
	__(subl $node_size,%nargs)
2:	__(jne 1b)
	__(jmp *%ra0)
_endsubp(stkconslist)

/* do list*: last arg in arg_z, all others vpushed,   */
/*	nargs set to #args vpushed.  */
	
_spentry(stkconslist_star)
	__(movq %nargs_q,%imm1)
	__(addq %imm1,%imm1)
	__(dnode_align(%imm1,tsp_frame.fixed_overhead,%imm1))
	__(TSP_Alloc_Var(%imm1,%imm0))
	__(addq $fulltag_cons,%imm0)
	__(testl %nargs,%nargs)
	__(jmp 2f)
1:	__(pop %temp0)
	__(_rplaca(%imm0,%temp0))
	__(_rplacd(%imm0,%arg_z))
	__(movq %imm0,%arg_z)
	__(addq $cons.size,%imm0)
	__(subl $node_size,%nargs)
2:	__(jne 1b)
	__(jmp *%ra0)
_endsubp(stkconslist_star)

/* Make a stack-consed simple-vector out of the NARGS objects   */
/*	on top of the vstack; return it in arg_z.  */
	
_spentry(mkstackv)
	__(dnode_align(%nargs_q,tsp_frame.fixed_overhead+node_size,%imm1))
	__(TSP_Alloc_Var(%imm1,%temp0))
	__(movl %nargs,%imm0_l)
	__(shlq $(num_subtag_bits-fixnumshift),%imm0)
	__(movb $subtag_simple_vector,%imm0_b)
	__(movq %imm0,(%temp0))
	__(leaq fulltag_misc(%temp0),%arg_z)
	__(testl %nargs,%nargs)
	__(leaq misc_data_offset(%arg_z,%nargs_q),%imm1)
	__(jmp 2f)
1:	__(pop -node_size(%imm1))
	__(subl $node_size,%nargs)
	__(leaq -node_size(%imm1),%imm1)
2:	__(jne 1b)
	__(jmp *%ra0)	
_endsubp(mkstackv)

	
        .globl C(egc_write_barrier_start)
C(egc_write_barrier_start):
/*  */
/* The function pc_luser_xp() - which is used to ensure that suspended threads  */
/* are suspended in a GC-safe way - has to treat these subprims (which implement  */
/* the EGC write-barrier) specially.  Specifically, a store that might introduce  */
/* an intergenerational reference (a young pointer stored in an old object) has  */
/* to "memoize" that reference by setting a bit in the global "refbits" bitmap.  */
/* This has to happen atomically, and has to happen atomically wrt GC.  */

/* Note that updating a word in a bitmap is itself not atomic, unless we use  */
/* interlocked loads and stores.  */



/* For RPLACA and RPLACD, things are fairly simple: regardless of where we are  */
/* in the function, we can do the store (even if it's already been done) and  */
/* calculate whether or not we need to set the bit out-of-line.  (Actually  */
/* setting the bit needs to be done atomically, unless we're sure that other  */
/* threads are suspended.)  */
/* We can unconditionally set the suspended thread's RIP to the return address.  */

	
_spentry(rplaca)
        .globl C(egc_rplaca)
C(egc_rplaca):
        /* pc_luser_xp() expects the store to be the first instruction here */
	__(_rplaca(%arg_y,%arg_z))
        __(rcmpq(%arg_z,%arg_y))
        __(ja 1f)
0:      __(repret)
1:      __(movq %arg_y,%imm0)
        __(subq lisp_global(ref_base),%imm0)
        __(shrq $dnode_shift,%imm0)
        __(movq %imm0,%imm1)
        __(cmpq lisp_global(oldspace_dnode_count),%imm0)
        __(jae 2f)
        __(ref_global(refbits,%temp0))
        __(xorb $63,%imm0_b)
        __(lock)
        __(btsq %imm0,(%temp0))
        __(ref_global(ephemeral_refidx,%temp0))
        __(shrq $8,%imm0)
        __(xorb $63,%imm0_b)
        __(lock)
        __(btsq %imm0,(%temp0))
2:      __(cmpq lisp_global(managed_static_dnodes),%imm1)
        __(jae 0b)
        __(ref_global(managed_static_refbits,%temp0))
        __(xorb $63,%imm1_b)
        __(lock)
        __(btsq %imm1,(%temp0))
        __(ret)
_endsubp(rplaca)

_spentry(rplacd)
        .globl C(egc_rplacd)
C(egc_rplacd):          
        /* pc_luser_xp() expects the store to be the first instruction here */
	__(_rplacd(%arg_y,%arg_z))
        __(rcmpq(%arg_z,%arg_y))
        __(ja 1f)
0:      __(repret)
1:      __(movq %arg_y,%imm0)
        __(subq lisp_global(ref_base),%imm0)
        __(shrq $dnode_shift,%imm0)
        __(movq %imm0,%imm1)
        __(cmpq lisp_global(oldspace_dnode_count),%imm0)
        __(jae 2f)
        __(ref_global(refbits,%temp0))
        __(xorb $63,%imm0_b)
        __(lock)
        __(btsq %imm0,(%temp0))
        __(ref_global(ephemeral_refidx,%temp0))
        __(shrq $8,%imm0)
        __(xorb $63,%imm0_b)
        __(lock)
        __(btsq %imm0,(%temp0))
2:      __(cmpq lisp_global(managed_static_dnodes),%imm1)
        __(jae 0b)
        __(ref_global(managed_static_refbits,%temp0))
        __(xorb $63,%imm1_b)
        __(lock)
        __(btsq %imm1,(%temp0))
        __(ret)        
_endsubp(rplacd)

/* Storing into a gvector can be handled the same way as storing into a CONS.  */


_spentry(gvset)
        .globl C(egc_gvset)
C(egc_gvset):
        /* pc_luser_xp() expects the store to be the first instruction here */
	__(movq %arg_z,misc_data_offset(%arg_x,%arg_y))
        __(rcmpq(%arg_z,%arg_x))
        __(ja 1f)
0:      __(repret)
1:      __(lea misc_data_offset(%arg_x,%arg_y),%imm0)
        __(subq lisp_global(ref_base),%imm0)
        __(shrq $dnode_shift,%imm0)
        __(movq %imm0,%imm1)
        __(cmpq lisp_global(oldspace_dnode_count),%imm0)
        __(jae 2f)
        __(ref_global(refbits,%temp0))
        __(xorb $63,%imm0_b)
        __(lock)
        __(btsq %imm0,(%temp0))
        __(ref_global(ephemeral_refidx,%temp0))
        __(shrq $8,%imm0)
        __(xorb $63,%imm0_b)
        __(lock)
        __(btsq %imm0,(%temp0))
2:      __(cmpq lisp_global(managed_static_dnodes),%imm1)
        __(jae 0b)
        __(ref_global(managed_static_refbits,%temp0))
        __(xorb $63,%imm1_b)
        __(lock)
        __(btsq %imm1,(%temp0))        
        __(ret)                
_endsubp(gvset)

/* This is a special case of storing into a gvector: if we need to  */
/* memoize the store, record the address of the hash-table vector  */
/* in the refmap, as well.  */
        

_spentry(set_hash_key)
        .globl C(egc_set_hash_key)
C(egc_set_hash_key):  
        /* pc_luser_xp() expects the store to be the first instruction here */
	__(movq %arg_z,misc_data_offset(%arg_x,%arg_y))
        __(rcmpq(%arg_z,%arg_x))
        __(ja 1f)
0:      __(repret)
1:      __(lea misc_data_offset(%arg_x,%arg_y),%imm0)
        __(subq lisp_global(ref_base),%imm0)
        __(shrq $dnode_shift,%imm0)
        __(movq %imm0,%imm1)
        __(cmpq lisp_global(oldspace_dnode_count),%imm0)
        __(jae 2f)
        __(ref_global(refbits,%temp0))
        __(xorb $63,%imm0_b)
        __(lock)
        __(btsq %imm0,(%temp0))
        __(ref_global(ephemeral_refidx,%temp0))
        __(shrq $8,%imm0)
        __(xorb $63,%imm0_b)
        __(lock)
        __(btsq %imm0,(%temp0))
        /* Now memoize the address of the hash vector   */
        __(ref_global(refbits,%temp0))
        __(movq %arg_x,%imm0)
        __(subq lisp_global(ref_base),%imm0)
        __(shrq $dnode_shift,%imm0)
        __(xorb $63,%imm0_b)
        __(lock)
        __(btsq %imm0,(%temp0))
        __(ref_global(ephemeral_refidx,%temp0))
        __(shrq $8,%imm0)
        __(xorb $63,%imm0_b)
        __(lock)
        __(btsq %imm0,(%temp0))
2:      __(cmpq lisp_global(managed_static_dnodes),%imm1)
        __(jae 0b)
        __(ref_global(managed_static_refbits,%temp0))
        __(xorb $63,%imm1_b)
        __(lock)
        __(btsq %imm1,(%temp0))
        /* Now memoize the address of the hash vector   */
        __(movq %arg_x,%imm0)
        __(subq lisp_global(ref_base),%imm0)
        __(shrq $dnode_shift,%imm0)
        __(xorb $63,%imm0_b)
        __(lock)
        __(btsq %imm0,(%temp0))
        __(ret)
_endsubp(set_hash_key)

/* This is a little trickier: if this is interrupted, we need to know  */
/* whether or not the STORE-CONDITIONAL (cmpxchgq) has won or not.    */
/* If we're interrupted   before the PC has reached the "success_test" label,   */
/* repeat (luser the PC back to store_node_conditional_retry.)  If we're at that  */
/* label with the Z flag set, we won and (may) need to memoize.  */

_spentry(store_node_conditional)
        .globl C(egc_store_node_conditional)
C(egc_store_node_conditional):
	__(unbox_fixnum(%temp0,%imm1))
        .globl C(egc_store_node_conditional_retry)
C(egc_store_node_conditional_retry):      
0:      __(movq %arg_y,%imm0)
	__(lock)
        __(cmpxchgq %arg_z,(%arg_x,%imm1))
        .globl C(egc_store_node_conditional_success_test)
C(egc_store_node_conditional_success_test):
	__(jne 9f)
        __(lea (%arg_x,%imm1),%imm0)
        __(subq lisp_global(ref_base),%imm0)
        __(shrq $dnode_shift,%imm0)
        __(movq %imm0,%imm1)
        __(cmpq lisp_global(oldspace_dnode_count),%imm0)
        __(ref_global(refbits,%temp1))
        __(jae 2f)
        __(xorb $63,%imm0_b)
        __(lock)
        __(btsq %imm0,(%temp1))
        __(shrq $8,%imm0)
        __(ref_global(ephemeral_refidx,%temp1))
        __(xorb $63,%imm0_b)
        __(lock)
        __(btsq %imm0,(%temp1))
2:      __(cmpq lisp_global(managed_static_dnodes),%imm1)
        __(jae 8f)
        __(ref_global(managed_static_refbits,%temp1))
        __(xorb $63,%imm1_b)
        __(lock)
        __(btsq %imm1,(%temp1))        
	.globl C(egc_store_node_conditional_success_end)
C(egc_store_node_conditional_success_end):
8:      __(movl $t_value,%arg_z_l)
	__(ret)
9:	__(movl $nil_value,%arg_z_l)
	__(ret)
_endsubp(store_node_conditional)
				
	_spentry(set_hash_key_conditional)
        .globl C(egc_set_hash_key_conditional)
C(egc_set_hash_key_conditional):
        .globl C(egc_set_hash_key_conditional_retry)
C(egc_set_hash_key_conditional_retry):          
	__(unbox_fixnum(%temp0,%imm1))
0:	__(movq %arg_y,%imm0)
	__(lock)
        __(cmpxchgq %arg_z,(%arg_x,%imm1))
        .globl C(egc_set_hash_key_conditional_success_test)
C(egc_set_hash_key_conditional_success_test):
	__(jne 9f)
        __(lea (%arg_x,%imm1),%imm0)
        __(subq lisp_global(ref_base),%imm0)
        __(shrq $dnode_shift,%imm0)
        __(movq %imm0,%imm1)
        __(cmpq lisp_global(oldspace_dnode_count),%imm0)
        __(ref_global(refbits,%temp1))
        __(jae 2f)
        __(xorb $63,%imm0_b)
        __(lock)
        __(btsq %imm0,(%temp1))
        __(shrq $8,%imm0)
        __(xorb $63,%imm0_b)
        __(ref_global(ephemeral_refidx,%temp1))
        __(lock)
        __(btsq %imm0,(%temp1))
        /* Now memoize the address of the hash vector   */
        __(movq %arg_x,%imm0)
        __(subq lisp_global(ref_base),%imm0)
        __(ref_global(refbits,%temp1))
        __(shrq $dnode_shift,%imm0)
        __(xorb $63,%imm0_b)
        __(lock)
        __(btsq %imm0,(%temp1))
        __(shrq $8,%imm0)
        __(xorb $63,%imm0_b)
        __(ref_global(ephemeral_refidx,%temp1))
        __(lock)
        __(btsq %imm0,(%temp1))
2:      __(cmpq lisp_global(managed_static_dnodes),%imm1)
        __(jae 8f)
        __(ref_global(managed_static_refbits,%temp1))
        __(xorb $63,%imm1_b)
        __(lock)
        __(btsq %imm1,(%temp1))
        /* Now memoize the address of the hash vector   */
        __(movq %arg_x,%imm0)
        __(subq lisp_global(ref_base),%imm0)
        __(shrq $dnode_shift,%imm0)
        __(xorb $63,%imm0_b)
        __(lock)
        __(btsq %imm0,(%temp1))
        .globl C(egc_write_barrier_end)
C(egc_write_barrier_end):
8:      __(movl $t_value,%arg_z_l)
	__(ret)
9:	__(movl $nil_value,%arg_z_l)
	__(ret)
_endsubp(set_hash_key_conditional)

	


_spentry(setqsym)
	__(btq $sym_vbit_const,symbol.flags(%arg_y))
	__(jae _SPspecset)
	__(movq %arg_y,%arg_z)
	__(movq $XCONST,%arg_y)
	__(set_nargs(2))
	__(jmp _SPksignalerr)
_endsubp(setqsym)

_spentry(progvsave)
	/* Error if arg_z isn't a proper list.  That's unlikely,  */
	/* but it's better to check now than to crash later.  */
	
	__(compare_reg_to_nil(%arg_z))
	__(movq %arg_z,%arg_x)	/* fast   */
	__(movq %arg_z,%temp1)	/* slow   */
	__(je 9f)		/* Null list is proper   */
0:
	__(extract_lisptag(%arg_x,%imm0))
	__(cmpb $tag_list,%imm0_b)
	__(jne 8f)
	__(compare_reg_to_nil(%arg_x))
	__(je 9f)
	__(_cdr(%arg_x,%temp0))	/* (null (cdr fast)) ?   */
	__(compare_reg_to_nil(%temp0))
	__(je 9f)
	__(extract_lisptag(%temp0,%imm0))
	__(cmpb $tag_list,%imm0_b)
	__(jne 8f)
	__(_cdr(%temp0,%arg_x))
	__(_cdr(%temp1,%temp1))
	__(cmpq %temp1,%arg_x)
	__(jne 0b)

8:	__(movq $XIMPROPERLIST,%arg_y)
	__(set_nargs(2))
	__(jmp _SPksignalerr)
9:	/* Whew 	  */

        /* Next, determine the length of arg_y.  We   */
	/* know that it's a proper list.   */
	__(movq $-fixnumone,%imm0)
	__(movq %arg_y,%arg_x)
1:	__(compare_reg_to_nil(%arg_x))
	__(_cdr(%arg_x,%arg_x))
	__(leaq fixnumone(%imm0),%imm0)
	__(jne 1b)
	
	/* imm0 is now (boxed) triplet count.  */
	/* Determine word count, add 1 (to align), and make room.  */
	/*  if count is 0, make an empty tsp frame and exit   */
	__(testq %imm0,%imm0)
	__(jne 2f)
	__(TSP_Alloc_Fixed(2*node_size,%imm0))
	__(ret)
2:	__(movq %imm0,%imm1)
	__(add %imm1,%imm1)
	__(add %imm0,%imm1)
	__(dnode_align(%imm1,tsp_frame.fixed_overhead+node_size,%imm1))
	__(TSP_Alloc_Var(%imm1,%temp0))
	__(movq %imm0,(%temp0))
	__(movq rcontext(tcr.db_link),%temp1)
3:	__(movl $unbound_marker,%temp0_l)
	__(compare_reg_to_nil(%arg_z))
	__(cmovneq cons.car(%arg_z),%temp0)
	__(cmovneq cons.cdr(%arg_z),%arg_z)
	__(_car(%arg_y,%arg_x))
	__(_cdr(%arg_y,%arg_y))
	__(movq symbol.binding_index(%arg_x),%arg_x)
	__(cmp rcontext(tcr.tlb_limit),%arg_x)
	__(jb 4f)
	__(push %arg_x)
	__(tlb_too_small())
4:	__(movq rcontext(tcr.tlb_pointer),%imm0)
	__(subq $binding.size,%imm1)
	__(compare_reg_to_nil(%arg_y))
	__(movq %arg_x,binding.sym(%imm1))
	__(push (%imm0,%arg_x))
	__(pop binding.val(%imm1))
	__(movq %temp0,(%imm0,%arg_x))
	__(movq %temp1,binding.link(%imm1))
	__(movq %imm1,%temp1)
	__(jne 3b)
	__(movq %temp1,rcontext(tcr.db_link))
	__(ret)
_endsubp(progvsave)

/* Allocate node objects on the temp stack, immediate objects on the foreign  */
/* stack. (The caller has to know which stack to discard a frame from.)  */
/* %arg_y = boxed element-count, %arg_z = boxed subtype  */
	
_spentry(stack_misc_alloc)
	__(movq $~(((1<<56)-1)<<fixnumshift),%temp0)
	__(testq %temp0,%arg_y)
	__(jne local_label(stack_misc_alloc_not_u56))
	__(unbox_fixnum(%arg_z,%imm0))
	__(movq %arg_y,%temp0)
	__(shl $num_subtag_bits-fixnumshift,%temp0)
	__(orq %temp0,%imm0)		/* %imm0 now = header   */
	__(movb $fulltagmask,%imm1_b)
	__(andb %imm0_b,%imm1_b)
	__(cmpb $fulltag_nodeheader_0,%imm1_b)
	__(je local_label(stack_misc_alloc_node))
	__(cmpb $fulltag_nodeheader_1,%imm1_b)
	__(je local_label(stack_misc_alloc_node))
	__(cmpb $ivector_class_64_bit,%imm1_b)
	__(jz local_label(stack_misc_alloc_64))
	__(cmpb $ivector_class_32_bit,%imm1_b)
	__(jz local_label(stack_misc_alloc_32))
	__(unbox_fixnum(%arg_y,%imm1))
	/* ivector_class_other_bit: 16, 8, or 1 ...   */
	__(cmpb $subtag_bit_vector,%imm0_b)
	__(jne local_label(stack_misc_alloc_8))
	__(addq $7,%imm1)
	__(shrq $3,%imm1)
	__(jmp local_label(stack_misc_alloc_alloc_ivector))
local_label(stack_misc_alloc_8):	
	__(cmpb $subtag_simple_base_string,%imm0_b)
	__(jb local_label(stack_misc_alloc_16))
	__(unbox_fixnum(%arg_y,%imm1))
	__(jmp local_label(stack_misc_alloc_alloc_ivector))
local_label(stack_misc_alloc_16):	
	__(unbox_fixnum(%arg_y,%imm1))
	__(shlq %imm1)
	__(jmp local_label(stack_misc_alloc_alloc_ivector))
local_label(stack_misc_alloc_32):
	/* 32-bit ivector   */
	__(unbox_fixnum(%arg_y,%imm1))
	__(shlq $2,%imm1)
	__(jmp local_label(stack_misc_alloc_alloc_ivector))
local_label(stack_misc_alloc_64):
	/* 64-bit ivector 	  */
	__(movq %arg_y,%imm1)
local_label(stack_misc_alloc_alloc_ivector):	
	__(dnode_align(%imm1,tsp_frame.fixed_overhead+node_size,%imm1))
	__(cmpq $tstack_alloc_limit,%imm1)
	__(ja local_label(stack_misc_alloc_heap_alloc_ivector))
        __ifdef(`WINDOWS')
         __(windows_cstack_probe(%imm1,%temp0))
        __endif
        __(movq rcontext(tcr.foreign_sp),%stack_temp) 
	__(movd %stack_temp,%temp1)
        __(subq %imm1,rcontext(tcr.foreign_sp))
        __(movq rcontext(tcr.foreign_sp),%temp0)
0:	__(movapd %fpzero,-dnode_size(%temp1))
	__(subq $dnode_size,%temp1)
	__(cmpq %temp1,%temp0)
	__(jnz 0b)	
	__(movq %stack_temp,(%temp0))
        __(movq %rbp,csp_frame.save_rbp(%temp0))
	__(movq %imm0,csp_frame.fixed_overhead(%temp0))
	__(leaq csp_frame.fixed_overhead+fulltag_misc(%temp0),%arg_z)
	__(ret)
local_label(stack_misc_alloc_heap_alloc_ivector):
        __(movq rcontext(tcr.foreign_sp),%imm1)
        __(subq $dnode_size,rcontext(tcr.foreign_sp))
        __(movq rcontext(tcr.foreign_sp),%imm0)
	__(movq %imm1,(%imm0))
	__(jmp _SPmisc_alloc)	
local_label(stack_misc_alloc_node):
	__(movq %arg_y,%imm1)
	__(dnode_align(%imm1,tsp_frame.fixed_overhead+node_size,%imm1))
	__(cmpq $tstack_alloc_limit,%imm1)
	__(ja local_label(stack_misc_alloc_heap_alloc_gvector))
	__(TSP_Alloc_Var(%imm1,%temp0))
	__(movq %imm0,(%temp0))
	__(leaq fulltag_misc(%temp0),%arg_z)
	__(ret)
local_label(stack_misc_alloc_heap_alloc_gvector):	
	__(TSP_Alloc_Fixed(0,%imm0))
	__(jmp _SPmisc_alloc)	
		
local_label(stack_misc_alloc_not_u56):				
        __(movl $XARRLIMIT,%arg_x_l)
        __(set_nargs(3))
        __(jmp _SPksignalerr)
_endsubp(stack_misc_alloc)

/* subtype (boxed, of course) is pushed, followed by nargs bytes worth of   */
/* initial-contents.  Note that this can be used to cons any type of initialized   */
/* node-header'ed misc object (symbols, closures, ...) as well as vector-like   */
/* objects.   */
_spentry(gvector)
        __(subl $node_size,%nargs)
	__(movq (%rsp,%nargs_q),%imm0)	/* boxed subtype   */
	__(sarq $fixnumshift,%imm0)
	__(movq %nargs_q,%imm1)
	__(shlq $num_subtag_bits-word_shift,%imm1)
	__(orq %imm1,%imm0)
	__(dnode_align(%nargs_q,node_size,%imm1))
	__(Misc_Alloc(%arg_z))
	__(movq %nargs_q,%imm1)
	__(jmp 2f)
1:	__(movq %temp0,misc_data_offset(%arg_z,%imm1))
2:	__(subq $node_size,%imm1)
	__(pop %temp0)	/* Note the intentional fencepost:  */
			/* discard the subtype as well.  */
	__(jge 1b)
	__(jmp *%ra0)
_endsubp(gvector)

_spentry(mvpass)
	__(hlt)
_endsubp(mvpass)



_spentry(nthvalue)
	__(hlt)
_endsubp(nthvalue)

_spentry(values)
        __(movq (%temp0),%ra0)
	__(ref_global(ret1val_addr,%imm1))
	__(cmpq %imm1,%ra0)
	__(movl $nil_value,%arg_z_l)
	__(je 0f)
	__(testl %nargs,%nargs)
	__(cmovneq -node_size(%rsp,%nargs_q),%arg_z)
	__(movq %temp0,%rsp)
	__(ret)
0:	__(movq 8(%temp0),%ra0)
        __(addq $2*node_size,%temp0)
	__(lea (%rsp,%nargs_q),%imm0)
	__(jmp 2f)
1:	__(subq $node_size,%imm0)
	__(movq (%imm0),%temp1)
	__(subq $node_size,%temp0)
	__(movq %temp1,(%temp0))
2:	__(cmpq %imm0,%rsp)
	__(jne 1b)
	__(movq %temp0,%rsp)
	__(jmp *%ra0)	
_endsubp(values)

_spentry(default_optional_args)
	__(hlt)
_endsubp(default_optional_args)

_spentry(opt_supplied_p)
	__(hlt)
_endsubp(opt_supplied_p)

_spentry(lexpr_entry)
	__(hlt)
_endsubp(lexpr_entry)
	
_spentry(heap_rest_arg)
	__(push_argregs())
        __(movq %next_method_context,%arg_y)
	__(movl %nargs,%imm1_l)
	__(testl %imm1_l,%imm1_l)
	__(movl $nil_value,%arg_z_l)
	__(jmp 2f)
	.p2align 4
1:	__(pop %temp1)
	__(Cons(%temp1,%arg_z,%arg_z))
	__(subl $node_size,%imm1_l)
2:	__(jg 1b)
	__(push %arg_z)
        __(movq %arg_y,%next_method_context)
	__(jmp *%ra0)		
_endsubp(heap_rest_arg)

/* %imm0 contains the number of fixed args ; make an &rest arg out of the others   */
_spentry(req_heap_rest_arg)
	__(push_argregs())
        __(movq %next_method_context,%arg_y)
	__(movl %nargs,%imm1_l)
	__(subl %imm0_l,%imm1_l)
	__(movl $nil_value,%arg_z_l)
	__(jmp 2f)
	.p2align 4
1:	__(pop %temp1)
	__(Cons(%temp1,%arg_z,%arg_z))
	__(subl $node_size,%imm1_l)
2:	__(jg 1b)
	__(push %arg_z)
        __(movq %arg_y,%next_method_context)
	__(jmp *%ra0)		
_endsubp(req_heap_rest_arg)

/* %imm0 bytes of stuff has already been pushed	  */
/* make an &rest arg out of any others   */
_spentry(heap_cons_rest_arg)
	__(movl %nargs,%imm1_l)
	__(subl %imm0_l,%imm1_l)
        __(movq %next_method_context,%arg_y)
	__(movl $nil_value,%arg_z_l)
	__(jmp 2f)
	.p2align 4
1:	__(pop %temp1)
	__(Cons(%temp1,%arg_z,%arg_z))
	__(subl $node_size,%imm1_l)
2:	__(jg 1b)
	__(push %arg_z)
        __(movq %arg_y,%next_method_context)
	__(jmp *%ra0)		
_endsubp(heap_cons_rest_arg)

_spentry(simple_keywords)
	__(xorl %imm0_l,%imm0_l)
	__(push_argregs())
	__(jmp _SPkeyword_bind)
_endsubp(simple_keywords)

_spentry(keyword_args)
	__(push_argregs())
	__(jmp _SPkeyword_bind)
_endsubp(keyword_args)

/* There are %nargs words of arguments on the stack; %imm0 contains the number  */
/* of non-keyword args pushed.  It's possible that we never actually got  */
/* any keyword args, which would make things much simpler.   */

/* On entry, temp1 contains a fixnum with bits indicating whether   */
/* &allow-other-keys and/or &rest was present in the lambda list.  */
/* Once we get here, we can use the arg registers.  */

define(`keyword_flags_aok_bit',`fixnumshift')
define(`keyword_flags_unknown_keys_bit',`fixnumshift+1')
define(`keyword_flags_rest_bit',`fixnumshift+2')
define(`keyword_flags_seen_aok_bit',`fixnumshift+3')        
	
_spentry(keyword_bind)
	__(movl %nargs,%imm1_l)
	__(subq %imm0,%imm1)
	__(jbe local_label(no_keyword_values))
	__(btq $word_shift,%imm1)
	__(jnc local_label(even))
	__(movl $nil_value,%arg_z_l)
	__(movq %imm1,%nargs_q)
	__(testl %nargs,%nargs)
	__(jmp 1f)
0:	__(pop %arg_y)
	__(Cons(%arg_y,%arg_z,%arg_z))
	__(subl $node_size,%nargs)
1:	__(jnz 0b)
	__(movl $XBADKEYS,%arg_y_l)
	__(set_nargs(2))
	__(jmp _SPksignalerr)
	/* Now that we're sure that we have an even number of keywords and values  */
	/* (in %imm1), copy all pairs to the temp stack   */
local_label(even):
	/* Get the keyword vector into arg_x, and its length into arg_y.  */
	__(movl function_data_offset(%fn),%imm0_l)
	__(movq function_data_offset(%fn,%imm0,node_size),%arg_x)
	__(vector_length(%arg_x,%arg_y))
        __(testq %arg_y,%arg_y)
        __(jne 1f)
        __(btq $keyword_flags_aok_bit,%temp1)
        __(jnc 1f)

        __(btq $keyword_flags_rest_bit,%temp1)
        __(jc 0f)
        __(addq %imm1,%rsp)
0:      
        __(jmp *%ra0)
1:      
       	__(lea tsp_frame.fixed_overhead(%imm1),%arg_z)
	__(TSP_Alloc_Var(%arg_z,%imm0))
2:	__(subq $node_size,%arg_z)
	__(pop (%arg_z))
	__(cmpq %arg_z,%imm0)
	__(jne 2b)
	/* Push arg_y pairs of NILs.   */
	__(movq %arg_y,%imm0)
	__(jmp 4f)
3:	__(push $nil_value)
	__(push $nil_value)
4:	__(subq $fixnumone,%arg_y)
	__(jge 3b)
	/* Push the %saveN registers, so that we can use them in this loop   */
	/* Also, borrow %arg_y for a bit */
	__(push %arg_y)
	__(push %save2)
	__(push %save1)
	__(push %save0)
	__(leaq 4*node_size(%rsp,%imm0,2),%save0)
	/* %save0 points to the 0th value/supplied-p pair   */
	__(leaq (%arg_z,%imm1),%save1)
	/* %save1 is the end of the provided keyword/value pairs (the old %tsp).   */
	__(movq %imm0,%save2)
	/* %save2 is the length of the keyword vector   */
5:	__(movq (%arg_z),%arg_y)	/* %arg_y is current keyword   */
	__(xorl %imm0_l,%imm0_l)
        __(cmpq $nrs.kallowotherkeys,%arg_y)
        __(jne local_label(next_keyvect_entry))
        __(btsq $keyword_flags_seen_aok_bit,%temp1)
        __(jc local_label(next_keyvect_entry))
        __(cmpb $fulltag_nil,node_size(%arg_z))
	__(je local_label(next_keyvect_entry))
	__(btsq $keyword_flags_aok_bit,%temp1)
	__(jmp local_label(next_keyvect_entry))
6:	__(cmpq misc_data_offset(%arg_x,%imm0),%arg_y)
	__(jne 7f)
	/* Got a match; have we already seen this keyword ?   */
	__(negq %imm0)
	__(cmpb $fulltag_nil,-node_size*2(%save0,%imm0,2))
	__(jne 9f)	/* already seen keyword, ignore this value   */
	__(movq node_size(%arg_z),%arg_y)
	__(movq %arg_y,-node_size(%save0,%imm0,2))
	__(movl $t_value,-node_size*2(%save0,%imm0,2))
	__(jmp 9f)
7:	__(addq $node_size,%imm0)
local_label(next_keyvect_entry):	
	__(cmpq %imm0,%save2)
	__(jne 6b)
	/* Didn't match anything in the keyword vector. Is the keyword  */
	/* :allow-other-keys ?   */
	__(cmpq $nrs.kallowotherkeys,%arg_y)
	__(je 9f)               /* :allow-other-keys is never "unknown" */
8:	__(btsq $keyword_flags_unknown_keys_bit,%temp1)
9:	__(addq $dnode_size,%arg_z)
	__(cmpq %arg_z,%save1)
	__(jne 5b)
	__(pop %save0)
	__(pop %save1)
	__(pop %save2)
	__(pop %arg_y)
	/* If the function takes an &rest arg, or if we got an unrecognized  */
	/* keyword and don't allow that, copy the incoming keyword/value  */
	/* pairs from the temp stack back to the value stack   */
	__(btq $keyword_flags_rest_bit,%temp1)
	__(jc 1f)
	__(btq $keyword_flags_unknown_keys_bit,%temp1)
	__(jnc 0f)
	__(btq $keyword_flags_aok_bit,%temp1)
	__(jnc 1f)
	/* pop the temp frame   */
0:	__(discard_temp_frame(%imm1))
	__(jmp *%ra0)
	/* Copy the keyword/value pairs from the tsp back to sp, either because  */
	/* the function takes an &rest arg or because we need to signal an  */
	/* "unknown keywords" error   */
1:	__(movq rcontext(tcr.save_tsp),%arg_z)
	__(mov (%arg_z),%arg_y)
	__(jmp 3f)
2:	__(push (%arg_z))
	__(push node_size(%arg_z))
3:	__(addq $dnode_size,%arg_z)
	__(cmpq %arg_z,%arg_y)
	__(jne 2b)
	__(discard_temp_frame(%imm0))
	__(btq $keyword_flags_unknown_keys_bit,%temp1)
	__(jnc 9f)
	__(btq $keyword_flags_aok_bit,%temp1)
	__(jc 9f)
	/* Signal an "unknown keywords" error   */
	__(movq %imm1,%nargs_q)
	__(testl %nargs,%nargs)
        __(movl $nil_value,%arg_z_l)
	__(jmp 5f)
4:	__(pop %arg_y)
	__(Cons(%arg_y,%arg_z,%arg_z))
	__(subl $node_size,%nargs)
5:	__(jnz 4b)
	__(movl $XBADKEYS,%arg_y_l)
	__(set_nargs(2))
        __(push %ra0)
	__(jmp _SPksignalerr)
9:	__(jmp *%ra0)
	
/* No keyword values were provided.  Access the keyword vector (which is the 0th  */
/*  constant in %fn), determine its length N, and push N	pairs of NILs.   */
/* N could be 0 ...  */
	
local_label(no_keyword_values):		
	__(movl function_data_offset(%fn),%imm0_l)
	__(movq function_data_offset(%fn,%imm0,node_size),%arg_x)
	__(movl $nil_value,%arg_z_l)
	__(vector_length(%arg_x,%arg_y))
	__(jmp 1f)
0:	__(push %arg_z)
	__(push %arg_z)
1:	__(subq $fixnumone,%arg_y)
	__(jge 0b)
	__(jmp *%ra0)		
_endsubp(keyword_bind)



_spentry(ksignalerr)
	__(movq $nrs.errdisp,%fname)
	__(jump_fname)	
_endsubp(ksignalerr)

_spentry(stack_rest_arg)
	__(xorl %imm0_l,%imm0_l)
	__(push_argregs())
	__(jmp _SPstack_cons_rest_arg)
_endsubp(stack_rest_arg)

_spentry(req_stack_rest_arg)
	__(push_argregs())
	__(jmp _SPstack_cons_rest_arg)
_endsubp(req_stack_rest_arg)

_spentry(stack_cons_rest_arg)
	__(movl %nargs,%imm1_l)
	__(subl %imm0_l,%imm1_l)
	__(movl $nil_value,%arg_z_l)
	__(jle 2f)	/* empty list ; make an empty TSP frame   */
	__(addq %imm1,%imm1)
	__(cmpq $(tstack_alloc_limit-dnode_size),%imm1)
	__(ja 3f)	/* make empty frame, then heap-cons   */
	__(dnode_align(%imm1,tsp_frame.fixed_overhead,%imm0))
	__(TSP_Alloc_Var(%imm0,%temp1))
	__(addq $fulltag_cons,%temp1)
1:	__(pop %arg_x)
	__(_rplacd(%temp1,%arg_z))
	__(_rplaca(%temp1,%arg_x))
	__(movq %temp1,%arg_z)
	__(addq $cons.size,%temp1)
	__(subq $dnode_size,%imm1)
	__(jne 1b)
	__(push %arg_z)
	__(jmp *%ra0)
	
/* Length 0, make empty frame  */
	
2:
	__(TSP_Alloc_Fixed(0,%temp1))
	__(push %arg_z)
	__(jmp *%ra0)
	
/* Too big to stack-cons, but make an empty frame before heap-consing  */
	
3:		
	__(TSP_Alloc_Fixed(0,%temp1))
	__(jmp _SPheap_cons_rest_arg)
_endsubp(stack_cons_rest_arg)



_spentry(getxlong)
_endsubp(getxlong)

/* Have to be a little careful here: the caller may or may not have pushed  */
/*   an empty frame, and we may or may not have needed one.  We can't easily  */
/*   tell whether or not a frame will be needed (if the caller didn't reserve  */
/*   a frame, whether or not we need one depends on the length of the list  */
/*   in arg_z.  So, if the caller didn't push a frame, we do so ; once everything's  */
/*   been spread, we discard the reserved frame (regardless of who pushed it)  */
/*   if all args fit in registers.   */
_spentry(spreadargz)
	__(testl %nargs,%nargs)
	__(jne 0f)
	__(push $reserved_frame_marker)
	__(push $reserved_frame_marker)
0:	__(movq %arg_z,%arg_y)	/* save in case of error   */
	__(xorl %imm0_l,%imm0_l)
	__(compare_reg_to_nil(%arg_z))
	__(je 2f)
1:	__(extract_fulltag(%arg_z,%imm1))
	__(cmpb $fulltag_cons,%imm1_b)
	__(jne 9f)
        __(_car(%arg_z,%arg_x))
	__(_cdr(%arg_z,%arg_z))
	__(addl $node_size,%imm0_l)
	__(cmpl $call_arguments_limit<<fixnumshift, %imm0_l)
        __(jae 8f)
	__(compare_reg_to_nil(%arg_z))
	__(push %arg_x)
	__(jne 1b)
2:	__(addl %imm0_l,%nargs)
	__(jne 4f)
3:	__(addq $2*node_size,%rsp)
	__(jmp *%ra0)
4:	__(cmpl $1*node_size,%nargs)
	__(pop %arg_z)
	__(je 3b)
	__(cmpl $2*node_size,%nargs)
	__(pop %arg_y)
	__(je 3b)
	__(cmpl $3*node_size,%nargs)
	__(pop %arg_x)
	__(je 3b)
	__(jmp *%ra0)
/* Discard everything that's been pushed already, complain   */

8:     	__(lea (%rsp,%imm0),%rsp)
	__(movq %arg_y,%arg_z)	/* recover original   */
	__(movq $XTMINPS,%arg_y)
	__(set_nargs(2))
        __(push %ra0)
	__(jmp _SPksignalerr)
/* Discard everything that's been pushed already, complain   */
9:	__(lea (%rsp,%imm0),%rsp)
	__(movq %arg_y,%arg_z)	/* recover original   */
	__(movq $XNOSPREAD,%arg_y)
	__(set_nargs(2))
        __(push %ra0)
	__(jmp _SPksignalerr)
_endsubp(spreadargz)

/* Caller built it's own frame when it was entered.  If all outgoing args  */
/* are in registers, we can discard that frame; otherwise, we copy outgoing  */
/* relative to it and restore %rbp/%ra0   */
_spentry(tfuncallgen)
	__(cmpl $nargregs*node_size,%nargs)
	__(jbe 9f)
	__(lea -nargregs*node_size(%rsp,%nargs_q),%imm0)
	__(xorl %imm1_l,%imm1_l)
	/* We can use %ra0 as a temporary here, since the real return address  */
	/* is on the stack   */
0:	__(movq -node_size(%imm0),%ra0)
	__(movq %ra0,-node_size(%rbp,%imm1))
	__(subq $node_size,%imm0)
	__(subq $node_size,%imm1)
	__(cmpq %imm0,%rsp)
	__(jne 0b)
	__(lea (%rbp,%imm1),%rsp)
	__(movq 8(%rbp),%ra0)
	__(movq (%rbp),%rbp)
        __(pushq %ra0)
	__(do_funcall())
        /* All args in regs; exactly the same as the tfuncallvsp case   */
9:		
	__(leave)
	__(do_funcall())
_endsubp(tfuncallgen)

/* Some args were pushed; move them down in the frame   */
_spentry(tfuncallslide)
	__(lea -nargregs*node_size(%rsp,%nargs_q),%imm0)
	__(xorl %imm1_l,%imm1_l)
	/* We can use %ra0 as a temporary here, since the real return address  */
	/* is on the stack   */
0:	__(movq -node_size(%imm0),%ra0)
	__(movq %ra0,-node_size(%rbp,%imm1))
	__(subq $node_size,%imm0)
	__(subq $node_size,%imm1)
	__(cmpq %imm0,%rsp)
	__(jne 0b)
	__(lea (%rbp,%imm1),%rsp)
	__(movq 8(%rbp),%ra0)
	__(movq (%rbp),%rbp)
        __(push %ra0)
	__(do_funcall())	
_endsubp(tfuncallslide)

/* No args were pushed; recover saved context & do funcall 	  */
_spentry(tfuncallvsp)
	__(leave)
	__(do_funcall())
_endsubp(tfuncallvsp)

_spentry(tcallsymgen)
	__(cmpl $nargregs*node_size,%nargs)
	__(jbe 9f)
	__(lea -nargregs*node_size(%rsp,%nargs_q),%imm0)
	__(xorl %imm1_l,%imm1_l)
	/* We can use %ra0 as a temporary here, since the real return address  */
	/* is on the stack   */
0:	__(movq -node_size(%imm0),%ra0)
	__(movq %ra0,-node_size(%rbp,%imm1))
	__(subq $node_size,%imm0)
	__(subq $node_size,%imm1)
	__(cmpq %imm0,%rsp)
	__(jne 0b)
	__(lea (%rbp,%imm1),%rsp)
	__(movq 8(%rbp),%ra0)
	__(movq (%rbp),%rbp)
        __(pushq %ra0)
	__(jump_fname())
/* All args in regs; exactly the same as the tcallsymvsp case   */
9:		
	__(leave)
	__(jump_fname())
_endsubp(tcallsymgen)

_spentry(tcallsymslide)
	__(lea -nargregs*node_size(%rsp,%nargs_q),%imm0)
	__(xorl %imm1_l,%imm1_l)
	/* We can use %ra0 as a temporary here, since the real return address  */
	/* is on the stack   */
0:	__(movq -node_size(%imm0),%ra0)
	__(movq %ra0,-node_size(%rbp,%imm1))
	__(subq $node_size,%imm0)
	__(subq $node_size,%imm1)
	__(cmpq %imm0,%rsp)
	__(jne 0b)
	__(lea (%rbp,%imm1),%rsp)
	__(movq 8(%rbp),%ra0)
	__(movq 0(%rbp),%rbp)
        __(pushq %ra0)
	__(jump_fname())
_endsubp(tcallsymslide)

_spentry(tcallsymvsp)
	__(leave)
	__(jump_fname())
_endsubp(tcallsymvsp)

_spentry(tcallnfngen)
	__(cmpl $nargregs*node_size,%nargs)
	__(jbe 9f)
	__(lea -nargregs*node_size(%rsp,%nargs_q),%imm0)
	__(xorl %imm1_l,%imm1_l)
	/* We can use %ra0 as a temporary here, since the real return address  */
	/* is on the stack   */
0:	__(movq -node_size(%imm0),%ra0)
	__(movq %ra0,-node_size(%rbp,%imm1))
	__(subq $node_size,%imm0)
	__(subq $node_size,%imm1)
	__(cmpq %imm0,%rsp)
	__(jne 0b)
	__(movq %temp0,%fn)
	__(lea (%rbp,%imm1),%rsp)
	__(movq lisp_frame.savera0(%rbp),%ra0)
	__(movq lisp_frame.backlink(%rbp),%rbp)
        __(pushq %ra0)
	__(jmp *%fn)
/* All args in regs; exactly the same as the tcallnfnvsp case   */
9:		
	__(movq %temp0,%fn)
	__(leave)
	__(jmp *%fn)
_endsubp(tcallnfngen)

_spentry(tcallnfnslide)
	__(lea -nargregs*node_size(%rsp,%nargs_q),%imm0)
	__(xorl %imm1_l,%imm1_l)
	/* We can use %ra0 as a temporary here, since the real return address  */
	/* is on the stack   */
0:	__(movq -node_size(%imm0),%ra0)
	__(movq %ra0,-node_size(%rbp,%imm1))
	__(subq $node_size,%imm0)
	__(subq $node_size,%imm1)
	__(cmpq %imm0,%rsp)
	__(jne 0b)
	__(movq %temp0,%fn)
	__(lea (%rbp,%imm1),%rsp)
	__(movq lisp_frame.savera0(%rbp),%ra0)
	__(movq lisp_frame.backlink(%rbp),%rbp)
        __(pushq %ra0)
	__(jmp *%fn)
_endsubp(tcallnfnslide)

_spentry(tcallnfnvsp)
	__(movq %temp0,%fn)
	__(leave)
	__(jmp *%fn)
_endsubp(tcallnfnvsp)


/* Make a "raw" area on the foreign stack, stack-cons a macptr to point to it,   */
/*   and return the macptr.  Size (in bytes, boxed) is in arg_z on entry; macptr  */
/*   in arg_z on exit.   */
_spentry(makestackblock)
	__(unbox_fixnum(%arg_z,%imm0))
	__(dnode_align(%imm0,tsp_frame.fixed_overhead+macptr.size,%imm0))
	__(cmpq $tstack_alloc_limit,%imm0)
	__(jae 1f)
        __ifdef(`WINDOWS')
         __(windows_cstack_probe(%imm0,%arg_z))
        __endif
        __(movq rcontext(tcr.foreign_sp),%imm1)
        __(subq %imm0,rcontext(tcr.foreign_sp))
        __(movq rcontext(tcr.foreign_sp),%arg_z)
	__(movq %imm1,(%arg_z))
        __(movq %rbp,csp_frame.save_rbp(%arg_z))
	__(lea macptr.size+tsp_frame.fixed_overhead(%arg_z),%imm0)
	__(movq $macptr_header,tsp_frame.fixed_overhead(%arg_z))
	__(addq $fulltag_misc+tsp_frame.fixed_overhead,%arg_z)
	__(movq %imm0,macptr.address(%arg_z))
	__(movsd %fpzero,macptr.domain(%arg_z))
	__(movsd %fpzero,macptr.type(%arg_z))
	__(ret)
1:	__(movq rcontext(tcr.foreign_sp),%imm1)
        __(subq $dnode_size,rcontext(tcr.foreign_sp))
        __(movq rcontext(tcr.foreign_sp),%imm0)
	__(movq %imm1,(%imm0))
        __(movq %rbp,csp_frame.save_rbp(%imm0))
	__(set_nargs(1))
	__(movq $nrs.new_gcable_ptr,%fname)
	__(jump_fname())
_endsubp(makestackblock)

_spentry(makestackblock0)
	__(unbox_fixnum(%arg_z,%imm0))
	__(dnode_align(%imm0,tsp_frame.fixed_overhead+macptr.size,%imm0))
	__(cmpq $tstack_alloc_limit,%imm0)
	__(jae 9f)
        __ifdef(`WINDOWS')
         __(windows_cstack_probe(%imm0,%arg_z))
        __endif        
        __(movq rcontext(tcr.foreign_sp),%imm1)
        __(subq %imm0,rcontext(tcr.foreign_sp))
        __(movq rcontext(tcr.foreign_sp),%arg_z)
	__(movq %imm1,(%arg_z))
        __(movq %rbp,csp_frame.save_rbp(%arg_z))
	__(lea macptr.size+tsp_frame.fixed_overhead(%arg_z),%imm0)
	__(movq $macptr_header,tsp_frame.fixed_overhead(%arg_z))
	__(addq $fulltag_misc+tsp_frame.fixed_overhead,%arg_z)
	__(movq %imm0,macptr.address(%arg_z))
	__(movsd %fpzero,macptr.domain(%arg_z))
	__(movsd %fpzero,macptr.type(%arg_z))
	__(jmp 2f)
1:	__(movapd %fpzero,(%imm0))
	__(addq $dnode_size,%imm0)
2:	__(cmpq %imm0,%imm1)
	__(jne 1b)		
	__(repret)
9:	__(movq rcontext(tcr.foreign_sp),%imm1)
        __(subq $dnode_size,rcontext(tcr.foreign_sp))
        __(movq rcontext(tcr.foreign_sp),%imm0)
	__(movq %imm1,(%imm0))
        __(movq %rbp,csp_frame.save_rbp(%imm0))
	__(set_nargs(1))
	__(movq $nrs.new_gcable_ptr,%fname)
	__(jump_fname())
_endsubp(makestackblock0)

_spentry(makestacklist)
        __(movq $((1<<63)|fixnummask),%imm0)
        __(testq %imm0,%arg_y)
        __(jne 9f)
	__(movq %arg_y,%imm0)
	__(addq %imm0,%imm0)
	__(rcmpq(%imm0,$tstack_alloc_limit))
	__(movl $nil_value,%temp1_l) 
	__(jae 2f)
	__(addq $tsp_frame.fixed_overhead,%imm0)
	__(TSP_Alloc_Var(%imm0,%temp0))
	__(addq $fulltag_cons,%temp0)
	__(jmp 1f)
0:	__(_rplaca(%temp0,%arg_z))
	__(_rplacd(%temp0,%temp1))
	__(movq %temp0,%temp1)
	__(addq $cons.size,%temp0)
1:	__(subq $fixnumone,%arg_y)
	__(jge 0b)
	__(movq %temp1,%arg_z)
	__(ret)
2:	__(TSP_Alloc_Fixed(0,%imm0))
	__(jmp 4f)
3:	__(Cons(%arg_z,%temp1,%temp1))
4:	__(subq $fixnumone,%arg_y)				
	__(jge 3b)
	__(movq %temp1,%arg_z)
	__(ret)
9:      __(uuo_error_reg_not_type(Rarg_y,error_object_not_unsigned_byte))
_endsubp(makestacklist)

/* subtype (boxed) vpushed before initial values. (Had better be a   */
/* node header subtag.) Nargs set to count of things vpushed. 	  */
_spentry(stkgvector)
	__(lea -fixnum_one(%nargs_q),%imm0)
	__(lea (%rsp,%imm0),%arg_x)
	__(movq %imm0,%arg_y)
	__(shlq $num_subtag_bits-fixnumshift,%imm0)
	__(movq (%arg_x), %imm1)
	__(shrq $fixnumshift,%imm1)
	__(orq %imm1,%imm0)	/* imm0 = header, %arg_y = unaligned size   */
	__(dnode_align(%arg_y,(tsp_frame.fixed_overhead+node_size),%imm1))
	__(TSP_Alloc_Var(%imm1,%arg_z))
	__(movq %imm0,(%arg_z))
	__(addq $fulltag_misc,%arg_z)
	__(lea -node_size(%nargs_q),%imm0)
	__(jmp 2f)
1:	__(pop misc_data_offset(%arg_z,%imm0))
2:	__(subq $node_size,%imm0)
	__(jge 1b)
	__(addq $node_size,%rsp)
	__(jmp *%ra0)	
_endsubp(stkgvector)

_spentry(misc_alloc)
	__(movq $~(((1<<56)-1)<<fixnumshift),%imm0)
	__(testq %imm0,%arg_y)
	__(jne local_label(misc_alloc_not_u56))
	__(unbox_fixnum(%arg_z,%imm0))
	__(movq %arg_y,%temp0)
	__(shl $num_subtag_bits-fixnumshift,%temp0)
	__(orq %temp0,%imm0)		/* %imm0 now = header   */
	__(movb $fulltagmask,%imm1_b)
	__(andb %imm0_b,%imm1_b)
	__(cmpb $fulltag_nodeheader_0,%imm1_b)
	__(je local_label(misc_alloc_64))
	__(cmpb $fulltag_nodeheader_1,%imm1_b)
	__(je local_label(misc_alloc_64))
	__(cmpb $ivector_class_64_bit,%imm1_b)
	__(jz local_label(misc_alloc_64))
	__(cmpb $ivector_class_32_bit,%imm1_b)
	__(jz local_label(misc_alloc_32))
	__(unbox_fixnum(%arg_y,%imm1))
	/* ivector_class_other_bit: 16, 8, or 1 ...   */
	__(cmpb $subtag_bit_vector,%imm0_b)
	__(jne local_label(misc_alloc_8))
	__(addq $7,%imm1)
	__(shrq $3,%imm1)
	__(jmp local_label(misc_alloc_alloc_vector))
local_label(misc_alloc_8):	
	__(cmpb $subtag_simple_base_string,%imm0_b)
	__(jae local_label(misc_alloc_alloc_vector))
local_label(misc_alloc_16):	
	__(shlq %imm1)
	__(jmp local_label(misc_alloc_alloc_vector))
local_label(misc_alloc_32):
	/* 32-bit ivector   */
	__(unbox_fixnum(%arg_y,%imm1))
	__(shlq $2,%imm1)
	__(jmp local_label(misc_alloc_alloc_vector))
local_label(misc_alloc_64):
	/* 64-bit ivector or gvector 	  */
	__(movq %arg_y,%imm1)
local_label(misc_alloc_alloc_vector):	
	__(dnode_align(%imm1,node_size,%imm1))
        __(ref_global(tenured_area,%arg_x))
        __(cmpq area.low(%arg_x),%imm1)
        __(ja local_label(misc_alloc_large))
	__(Misc_Alloc(%arg_z))
	__(ret)
local_label(misc_alloc_not_u56):
        __(movl $XARRLIMIT,%arg_x_l)
        __(set_nargs(3))
        __(jmp _SPksignalerr)
local_label(misc_alloc_large):
        /* If we tried to subtract %imm1 from tcr.allocptr, it
           might become negative ; we treat addresses as being unsigned,
           so that negative value would look like a very large unsigned
           value and we'd think that the allocation succeeded.
           If we reach this point, we're trying to allocate something
           very large indeed.  Depending on the platform, that's anywhere
           from hundreds of GB to hundreds of TB.  Someday, it might be
           worth trying that (using a special "large allocation" UUO);
           for now, it's probably safe to just report that a memory
           allocation attempt failed.
        */
        __(movq $XMEMFULL,%arg_z)
        __(set_nargs(1))
        __(jmp _SPksignalerr)
_endsubp(misc_alloc)


_startfn(C(destbind1))
	/* Save entry %rsp in case of error   */
	__(movd %rsp,%mm0)
	/* Extract required arg count.   */
	__(movzbl %nargs_b,%imm0_l)
        __(testl %imm0_l,%imm0_l)
	__(je local_label(opt))		/* skip if no required args   */
local_label(req_loop):	
	__(compare_reg_to_nil(%arg_reg))
	__(je local_label(toofew))
	__(extract_lisptag(%arg_reg,%imm1))
	__(cmpb $tag_list,%imm1_b)
	__(jne local_label(badlist))
	__(subb $1,%imm0_b)
	__(pushq cons.car(%arg_reg))
	__(_cdr(%arg_reg,%arg_reg))
	__(jne local_label(req_loop))
local_label(opt):	
	__(movw %nargs_w,%imm0_w)
	__(shrw $8,%imm0_w)
	__(je local_label(rest_keys))
	__(btl $initopt_bit,%nargs)
	__(jc local_label(opt_supp))
	/* 'simple' &optionals:	 no supplied-p, default to nil.   */
local_label(simple_opt_loop):
	__(compare_reg_to_nil(%arg_reg))
	__(je local_label(default_simple_opt))
	__(extract_lisptag(%arg_reg,%imm1))
	__(cmpb $tag_list,%imm1_b)
	__(jne local_label(badlist))
	__(subb $1,%imm0_b)
	__(pushq cons.car(%arg_reg))
	__(_cdr(%arg_reg,%arg_reg))
	__(jne local_label(simple_opt_loop))
	__(jmp local_label(rest_keys))
local_label(default_simple_opt):
	__(subb $1,%imm0_b)
	__(pushq $nil_value)
	__(jne local_label(default_simple_opt))
	__(jmp local_label(rest_keys))
local_label(opt_supp):
	__(extract_lisptag(%arg_reg,%imm1))
	__(compare_reg_to_nil(%arg_reg))
	__(je local_label(default_hard_opt))
	__(cmpb $tag_list,%imm1_b)
	__(jne local_label(badlist))
	__(subb $1,%imm0_b)
	__(pushq cons.car(%arg_reg))
	__(_cdr(%arg_reg,%arg_reg))
	__(push $t_value)
	__(jne local_label(opt_supp))
	__(jmp local_label(rest_keys))
local_label(default_hard_opt):
	__(subb $1,%imm0_b)
	__(push $nil_value)
	__(push $nil_value)
	__(jne local_label(default_hard_opt))	
local_label(rest_keys):	
	__(btl $restp_bit,%nargs)
	__(jc local_label(have_rest))
	__(btl $keyp_bit,%nargs)
	__(jc local_label(have_keys))
	__(compare_reg_to_nil(%arg_reg))
	__(jne local_label(toomany))
	__(jmp *%ra0)
local_label(have_rest):
	__(pushq %arg_reg)
	__(btl $keyp_bit,%nargs)
	__(jc local_label(have_keys))
	__(jmp *%ra0)		
	/* Ensure that arg_reg contains a proper,even-length list.  */
	/* Insist that its length is <= 512 (as a cheap circularity check.)   */
local_label(have_keys):
	__(movw $256,%imm0_w)
	__(movq %arg_reg,%arg_y)
local_label(count_keys_loop):	
	__(compare_reg_to_nil(%arg_y))
	__(je local_label(counted_keys))
	__(subw $1,%imm0_w)
	__(jl local_label(toomany))
	__(extract_lisptag(%arg_y,%imm1))
	__(cmpb $tag_list,%imm1_b)
	__(jne local_label(badlist))
	__(_cdr(%arg_y,%arg_y))
	__(extract_fulltag(%arg_y,%imm1))
	__(cmpb $fulltag_cons,%imm1_b)
	__(jne local_label(badlist))
	__(_cdr(%arg_y,%arg_y))
	__(jmp local_label(count_keys_loop))
local_label(counted_keys):		
	/* We've got a proper, even-length list of key/value pairs in  */
	/* arg_reg. For each keyword var in the lambda-list, push a pair  */
	/* of NILs on the vstack.   */
	
	__(movl %nargs,%imm1_l)
	__(shrl $16,%imm1_l)
	__(movzbl %imm1_b,%imm0_l)
	__(movq %rsp,%arg_y)
	__(jmp local_label(push_pair_test))	
local_label(push_pair_loop):
	__(push $nil_value)
	__(push $nil_value)
local_label(push_pair_test):	
	__(subb $1,%imm1_b)
	__(jge local_label(push_pair_loop))
	/* Push the %saveN registers, so that we can use them in this loop   */
	/* Also, borrow %arg_z */
	__(push %save0)
	__(push %save1)
	__(push %save2)
	__(push %arg_z)
	/* save0 points to the 0th value/supplied-p pair   */
	__(movq %arg_y,%save0)
	/* save1 is the length of the keyword vector   */
	__(vector_length(%arg_x,%save1))
	/* save2 is the current keyword   */
	/* arg_z is the value of the current keyword   */
	__(xorl %imm0_l,%imm0_l)	/* count unknown keywords seen   */
local_label(match_keys_loop):
	__(compare_reg_to_nil(%arg_reg))
	__(je local_label(matched_keys))
	__(_car(%arg_reg,%save2))
	__(_cdr(%arg_reg,%arg_reg))
	__(_car(%arg_reg,%arg_z))
	__(_cdr(%arg_reg,%arg_reg))
	__(xorl %arg_y_l,%arg_y_l)
	__(jmp local_label(match_test))
local_label(match_loop):
	__(cmpq misc_data_offset(%arg_x,%arg_y),%save2)
	__(je local_label(matched))
	__(addq $node_size,%arg_y)
local_label(match_test):
	__(cmpq %arg_y,%save1)
	__(jne local_label(match_loop))
	/* No match.  Note unknown keyword, check for :allow-other-keys   */
	__(addl $1,%imm0_l)
	__(cmpq $nrs.kallowotherkeys,%save2)
	__(jne local_label(match_keys_loop))
	__(subl $1,%imm0_l)
	__(btsl $seen_aok_bit,%nargs)
	__(jc local_label(match_keys_loop))
	/* First time we've seen :allow-other-keys.  Maybe set aok_bit.   */
	__(compare_reg_to_nil(%arg_z))
	__(je local_label(match_keys_loop))
	__(btsl $aok_bit,%nargs)
	__(jmp local_label(match_keys_loop))
	/* Got a match.  Worry about :allow-other-keys here, too.   */
local_label(matched):
	__(negq %arg_y)
	__(cmpb $fulltag_nil,-node_size*2(%save0,%arg_y,2))
	__(jne local_label(match_keys_loop))
	__(movq %arg_z,-node_size(%save0,%arg_y,2))
	__(movl $t_value,-node_size*2(%save0,%arg_y,2))
	__(cmpq $nrs.kallowotherkeys,%save2)
	__(jne local_label(match_keys_loop))
	__(btsl $seen_aok_bit,%nargs)
	__(jnc local_label(match_keys_loop))
	__(compare_reg_to_nil(%arg_z))
	__(je local_label(match_keys_loop))
	__(btsl $aok_bit,%nargs)
	__(jmp local_label(match_keys_loop))
local_label(matched_keys):		
	__(pop %arg_z)
	__(pop %save2)
	__(pop %save1)
	__(pop %save0)
	__(testl %imm0_l,%imm0_l)
	__(je local_label(keys_ok)) 
	__(btl $aok_bit,%nargs)
	__(jnc local_label(badkeys))
local_label(keys_ok):	
	__(jmp *%ra0)
	/* Some unrecognized keywords.  Complain generically about   */
	/* invalid keywords.   */
local_label(badkeys):
	__(movq $XBADKEYS,%arg_y)
	__(jmp local_label(destructure_error))
local_label(toomany):
	__(movq $XCALLTOOMANY,%arg_y)
	__(jmp local_label(destructure_error))
local_label(toofew):
	__(movq $XCALLTOOFEW,%arg_y)
	__(jmp local_label(destructure_error))
local_label(badlist):
	__(movq $XCALLNOMATCH,%arg_y)
	/* jmp local_label(destructure_error)   */
local_label(destructure_error):
	__(movd %mm0,%rsp)		/* undo everything done to the stack   */
	__(movq %whole_reg,%arg_z)
	__(set_nargs(2))
        __(push %ra0)
	__(jmp _SPksignalerr)
_endfn(C(destbind1))	

_spentry(macro_bind)
	__(movq %arg_reg,%whole_reg)
	__(extract_lisptag(%arg_reg,%imm0))
	__(cmpb $tag_list,%imm0_b)
	__(jne 1f)
	__(_cdr(%arg_reg,%arg_reg))
	__(jmp C(destbind1))
1:	__(movq $XCALLNOMATCH,%arg_y)
	__(movq %whole_reg,%arg_z)
	__(set_nargs(2))
        __(push %ra0)        
	__(jmp _SPksignalerr)
_endsubp(macro_bind)

_spentry(destructuring_bind)
	__(movq %arg_reg,%whole_reg)
	__(jmp C(destbind1))
_endsubp(destructuring_bind)

_spentry(destructuring_bind_inner)
	__(movq %arg_z,%whole_reg)
	__(jmp C(destbind1))
_endsubp(destructuring_bind_inner)

	


_spentry(vpopargregs)
_endsubp(vpopargregs)

/* If arg_z is an integer, return in imm0 something whose sign  */
/* is the same as arg_z's.  If not an integer, error.   */
_spentry(integer_sign)
	__(testb $tagmask,%arg_z_b)
	__(movq %arg_z,%imm0)
	__(je 8f)
	__(extract_typecode(%arg_z,%imm0))
	__(cmpb $subtag_bignum,%imm0_b)
	__(jne 9f)
	__(getvheader(%arg_z,%imm0))
	__(shr $num_subtag_bits,%imm0)
	__(movslq misc_data_offset-4(%arg_z,%imm0,4),%imm0)
8:	__(repret)
9:	__(uuo_error_reg_not_type(Rarg_z,error_object_not_integer))
_endsubp(integer_sign)

/* "slide" nargs worth of values up the stack.  IMM0 contains   */
/* the difference between the current RSP and the target.   */
_spentry(mvslide)
	__(movl %nargs,%imm1_l)
	__(lea (%rsp,%nargs_q),%temp0)
	__(testq %imm1,%imm1)
	__(lea (%temp0,%imm0),%imm0)
	__(je 2f)
1:	
	__(subq $node_size,%temp0)
	__(movq (%temp0),%temp1)
	__(subq $node_size,%imm0)
	__(movq %temp1,(%imm0))
	__(subq $node_size,%imm1)
	__(jne 1b)
2:	__(movq %imm0,%rsp)
	__(jmp *%ra0)	
_endsubp(mvslide)

_spentry(save_values)
	__(movq rcontext(tcr.save_tsp),%imm1)
/* common exit: nargs = values in this set, imm1 = ptr to tsp before call to save_values   */
local_label(save_values_to_tsp):
	__(movq rcontext(tcr.save_tsp),%arg_x)
	__(dnode_align(%nargs_q,tsp_frame.fixed_overhead+(2*node_size),%imm0)) /* count, link   */
	__(TSP_Alloc_Var(%imm0,%arg_z))
	__(movq rcontext(tcr.save_tsp),%imm0)
	__(movq %imm1,(%imm0))
	__(movq %nargs_q,(%arg_z))
	__(movq %arg_x,node_size(%arg_z))
	__(leaq 2*node_size(%arg_z,%nargs_q),%arg_y)
	__(leaq (%rsp,%nargs_q),%imm0)
	__(cmpq %imm0,%rsp)
	__(jmp 2f)
1:	__(subq $node_size,%imm0)
	__(movq (%imm0),%arg_z)
	__(subq $node_size,%arg_y)
	__(cmpq %imm0,%rsp)
	__(movq %arg_z,(%arg_y))
2:	__(jne 1b)
	__(add %nargs_q,%rsp)
	__(jmp *%ra0)			
_endsubp(save_values)

/* Add the multiple values that are on top of the vstack to the set  */
/* saved in the top tsp frame, popping them off of the vstack in the  */
/* process.  It is an error (a bad one) if the TSP contains something  */
/* other than a previously saved set of multiple-values.  */
/* Since adding to the TSP may cause a new TSP segment to be allocated,  */
/* each add_values call adds another linked element to the list of  */
/* values. This makes recover_values harder.   */
_spentry(add_values)
	__(testl %nargs,%nargs)
	__(movq rcontext(tcr.save_tsp),%imm1)
	__(movq (%imm1),%imm1)
	__(jne local_label(save_values_to_tsp))
	__(jmp *%ra0)
_endsubp(add_values)

/* push the values in the value set atop the sp, incrementing nargs.  */
/* Discard the tsp frame; leave values atop the sp.   */
	
_spentry(recover_values)
	/* First, walk the segments reversing the pointer to previous  */
	/* segment pointers Can tell the end because that previous  */
	/* segment pointer is the prev tsp pointer   */
	__(movq rcontext(tcr.save_tsp),%temp1)
	__(movq %temp1,%arg_x)	/* current segment   */
	__(movq %temp1,%arg_y)	/* last segment   */
	__(movq tsp_frame.backlink(%temp1),%arg_z)	/* previous tsp   */
local_label(walkloop):
	__(movq tsp_frame.fixed_overhead+node_size(%arg_x),%temp0)
	__(cmpq %temp0,%arg_z)	/* last segment ?   */
	__(movq %arg_y,tsp_frame.fixed_overhead+node_size(%arg_x))
	__(movq %arg_x,%arg_y)	/* last segment <- current segment   */
	__(movq %temp0,%arg_x)	/* current segment <- next segment   */
	__(jne local_label(walkloop))

	/* the final segment pointer is now in %arg_y  */
	/* walk backwards, pushing values on the stack and incrementing %nargs   */
local_label(pushloop):
	__(movq tsp_frame.data_offset(%arg_y),%imm0)	/* nargs in segment   */
	__(testq %imm0,%imm0)
	__(leaq tsp_frame.data_offset+(2*node_size)(%arg_y,%imm0),%temp0)
	__(leaq (%nargs_q,%imm0),%nargs_q)
	__(jmp 2f)
1:	__(pushq -node_size(%temp0))
	__(subq $node_size,%temp0)
	__(subq $fixnum_one,%imm0)
2:	__(jne 1b)
	__(cmpq %arg_y,%temp1)
	__(movq tsp_frame.data_offset+node_size(%arg_y),%arg_y)
	__(jne local_label(pushloop))
	__(movq (%temp1),%temp1)
        __(movq %temp1,rcontext(tcr.save_tsp))
        __(movq %temp1,rcontext(tcr.next_tsp))        
	__(jmp *%ra0)		
_endsubp(recover_values)

/* Exactly like recover_values, but it's necessary to reserve an outgoing  */
/* frame if any values (which will be used as outgoing arguments) will  */
/* wind up on the stack.  We can assume that %nargs contains 0 (and  */
/* that no other arguments have been pushed) on entry.   */
                
_spentry(recover_values_for_mvcall)
	/* First, walk the segments reversing the pointer to previous  */
	/* segment pointers Can tell the end because that previous  */
	/* segment pointer is the prev tsp pointer   */
        __(xorl %nargs,%nargs)
	__(movq rcontext(tcr.save_tsp),%temp1)
	__(movq %temp1,%arg_x)	/* current segment   */
	__(movq %temp1,%arg_y)	/* last segment   */
	__(movq tsp_frame.backlink(%temp1),%arg_z)	/* previous tsp   */
local_label(walkloop_mvcall):
	__(movq tsp_frame.fixed_overhead+node_size(%arg_x),%temp0)
        __(addq tsp_frame.data_offset(%arg_x),%nargs_q)	
	__(cmpq %temp0,%arg_z)	/* last segment ?   */
	__(movq %arg_y,tsp_frame.fixed_overhead+node_size(%arg_x))
	__(movq %arg_x,%arg_y)	/* last segment <- current segment   */
	__(movq %temp0,%arg_x)	/* current segment <- next segment   */
	__(jne local_label(walkloop_mvcall))

        __(cmpl $nargregs*node_size,%nargs)
        __(jbe local_label(pushloop_mvcall))
        __(push $reserved_frame_marker)
        __(push $reserved_frame_marker)

	/* the final segment pointer is now in %arg_y  */
	/* walk backwards, pushing values on the stack and incrementing %nargs   */
local_label(pushloop_mvcall):
	__(movq tsp_frame.data_offset(%arg_y),%imm0)	/* nargs in segment   */
	__(testq %imm0,%imm0)
	__(leaq tsp_frame.data_offset+(2*node_size)(%arg_y,%imm0),%temp0)
	__(jmp 2f)
1:	__(pushq -node_size(%temp0))
	__(subq $node_size,%temp0)
	__(subq $fixnum_one,%imm0)
2:	__(jne 1b)
	__(cmpq %arg_y,%temp1)
	__(movq tsp_frame.data_offset+node_size(%arg_y),%arg_y)
	__(jne local_label(pushloop_mvcall))
	__(movq (%temp1),%temp1)
        __(movq %temp1,rcontext(tcr.save_tsp))
        __(movq %temp1,rcontext(tcr.next_tsp))        
	__(jmp *%ra0)		
_endsubp(recover_values_for_mvcall)
        				
_spentry(reset)
	__(hlt)
_endsubp(reset)



_spentry(misc_alloc_init)
	__(push %rbp)
	__(movq %rsp,%rbp)
	__(push %arg_z)
	__(movq %arg_y,%arg_z)
	__(movq %arg_x,%arg_y)
	__(lea local_label(misc_alloc_init_back)(%rip),%ra0)
        __(push %ra0)
	__(jmp _SPmisc_alloc)
__(tra(local_label(misc_alloc_init_back)))
	__(pop %arg_y)
	__(leave)
	__(movq $nrs.init_misc,%fname)
	__(set_nargs(2))
	__(jump_fname())	
_endsubp(misc_alloc_init)

_spentry(stack_misc_alloc_init)
	__(push %rbp)
	__(movq %rsp,%rbp)
	__(push %arg_z)
	__(movq %arg_y,%arg_z)
	__(movq %arg_x,%arg_y)
	__(lea local_label(stack_misc_alloc_init_back)(%rip),%ra0)
        __(push %ra0)
	__(jmp _SPstack_misc_alloc)
__(tra(local_label(stack_misc_alloc_init_back)))
	__(pop %arg_y)
	__(leave)
	__(movq $nrs.init_misc,%fname)
	__(set_nargs(2))
	__(jump_fname())	
_endsubp(stack_misc_alloc_init)



	.globl C(popj)
_spentry(popj)
C(popj):
	__(leave)
        __(ret)
_endsubp(popj)



_spentry(getu64)
	__(movq $~(target_most_positive_fixnum << fixnumshift),%imm0)
	__(testq %arg_z,%imm0)
	__(movq %arg_z,%imm0)
	__(jne 1f)
	__(sarq $fixnumshift,%imm0)
	__(ret)
1:	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne 9f)
	__(movb misc_subtag_offset(%arg_z),%imm0_b)
	__(cmpb $subtag_bignum,%imm0_b)
	__(jne 9f)
	__(movq misc_header_offset(%arg_z),%imm0)
	__(cmpq $three_digit_bignum_header,%imm0)
	__(je 3f)
	__(cmpq $two_digit_bignum_header,%imm0)
	__(jne 9f)
	__(movq misc_data_offset(%arg_z),%imm0)
	__(testq %imm0,%imm0)
	__(js 9f)
	__(repret)
3:	__(movq misc_data_offset(%arg_z),%imm0)
	__(cmpl $0,misc_data_offset+8(%arg_z))
	__(jne 9f)
	__(repret)
9:	__(uuo_error_reg_not_type(Rarg_z,error_object_not_u64))
_endsubp(getu64)

_spentry(gets64)
	__(movq %arg_z,%imm0)
	__(sarq $fixnumshift,%imm0)
	__(testb $fixnummask,%arg_z_b)
	__(je 8f)
1:	__(movb %arg_z_b,%imm0_b)
	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne 9f)
	__(movb misc_subtag_offset(%arg_z),%imm0_b)
	__(cmpb $subtag_bignum,%imm0_b)
	__(jne 9f)
	__(movq misc_header_offset(%arg_z),%imm0)
	__(cmpq $two_digit_bignum_header,%imm0)
	__(movq misc_data_offset(%arg_z),%imm0)
	__(jne 9f)
8:	__(repret)
9:	__(uuo_error_reg_not_type(Rarg_z,error_object_not_s64))
_endsubp(gets64)

_spentry(makeu64)
	__(movq %imm0,%imm1)
	__(shlq $fixnumshift+1,%imm1)
	__(movq %imm1,%arg_z)	/* Tagged as a fixnum, 2x    */
	__(shrq $fixnumshift+1,%imm1)
	__(shrq %arg_z)
	__(cmpq %imm0,%imm1)
	__(je 9f)
	__(testq %imm0,%imm0)
	__(movd %imm0,%mm0)
	__(js 3f)
	/* Make a 2-digit bignum.   */
	__(movl $two_digit_bignum_header,%imm0_l)
	__(movl $aligned_bignum_size(2),%imm1_l)
	__(Misc_Alloc(%arg_z))
	__(movq %mm0,misc_data_offset(%arg_z))
	__(ret)
3:	__(movl $three_digit_bignum_header,%imm0_l)
	__(movl $aligned_bignum_size(3),%imm1_l)
	__(Misc_Alloc(%arg_z))
	__(movq %mm0,misc_data_offset(%arg_z))
9:	__(repret)
_endsubp(makeu64)

/* on entry: arg_z = symbol.  On exit, arg_z = value (possibly  */
/* unbound_marker), arg_y = symbol   */
_spentry(specref)
	__(movq symbol.binding_index(%arg_z),%imm0)
        __(xorl %imm2_l,%imm2_l) 
	__(cmp rcontext(tcr.tlb_limit),%imm0)
	__(movq rcontext(tcr.tlb_pointer),%imm1)
	__(movq %arg_z,%arg_y)
	__(cmovael %imm2_l,%imm0_l)
	__(movq (%imm1,%imm0),%arg_z)
	__(cmpb $no_thread_local_binding_marker,%arg_z_b)
	__(cmoveq symbol.vcell(%arg_y),%arg_z)
	__(ret)
_endsubp(specref)

/* arg_y = special symbol, arg_z = new value.           */
_spentry(specset)
	__(movq symbol.binding_index(%arg_y),%imm0)
	__(cmp rcontext(tcr.tlb_limit),%imm0)
	__(movq rcontext(tcr.tlb_pointer),%imm1)
	__(jae 1f)
	__(movq (%imm1,%imm0),%arg_x)
	__(cmpb $no_thread_local_binding_marker,%arg_x_b)
	__(je 1f)
	__(movq %arg_z,(%imm1,%imm0))
	__(ret)
1:	__(lea fulltag_misc-fulltag_symbol(%arg_y),%arg_x)
	__(movq $1<<fixnumshift,%arg_y)
	__(jmp _SPgvset)
_endsubp(specset)

_spentry(specrefcheck)
	__(movq symbol.binding_index(%arg_z),%imm0)
        __(xorl %imm2_l,%imm2_l)
	__(cmp rcontext(tcr.tlb_limit),%imm0)
	__(movq rcontext(tcr.tlb_pointer),%imm1)
	__(movq %arg_z,%arg_y)
	__(cmovaeq %imm2,%imm0)
	__(movq (%imm1,%imm0),%arg_z)
	__(cmpb $no_thread_local_binding_marker,%arg_z_b)
	__(cmoveq symbol.vcell(%arg_y),%arg_z)
	__(cmpb $unbound_marker,%arg_z_b)
	__(je 9f)
        __(repret)
9:      __(uuo_error_reg_unbound(Rarg_y))
_endsubp(specrefcheck)

_spentry(restoreintlevel)
_endsubp(restoreintlevel)

_spentry(makes32)
	__(hlt)
_endsubp(makes32)

_spentry(makeu32)
	__(hlt)
_endsubp(makeu32)

_spentry(gets32)
	__(hlt)
_endsubp(gets32)

_spentry(getu32)
	__(hlt)
_endsubp(getu32)


_spentry(mvpasssym)
_endsubp(mvpasssym)


_spentry(unbind)
	__(movq rcontext(tcr.db_link),%imm1)
	__(movq rcontext(tcr.tlb_pointer),%arg_x)
	__(movq binding.sym(%imm1),%temp1)
	__(movq binding.val(%imm1),%arg_y)
	__(movq binding.link(%imm1),%imm1)
	__(movq %arg_y,(%arg_x,%temp1))
	__(movq %imm1,rcontext(tcr.db_link))
	__(ret)	
_endsubp(unbind)

_spentry(unbind_n)
	__(movq rcontext(tcr.db_link),%imm1)
	__(movq rcontext(tcr.tlb_pointer),%arg_x)
1:		
	__(movq binding.sym(%imm1),%temp1)
	__(movq binding.val(%imm1),%arg_y)
	__(movq binding.link(%imm1),%imm1)
	__(movq %arg_y,(%arg_x,%temp1))
	__(subq $1,%imm0)
	__(jne 1b)
	__(movq %imm1,rcontext(tcr.db_link))
	__(ret)	
_endsubp(unbind_n)

_spentry(unbind_to)
	__(movq rcontext(tcr.db_link),%imm1)
	__(movq rcontext(tcr.tlb_pointer),%arg_x)
1:		
	__(movq binding.sym(%imm1),%temp1)
	__(movq binding.val(%imm1),%arg_y)
	__(movq binding.link(%imm1),%imm1)
	__(movq %arg_y,(%arg_x,%temp1))
	__(cmpq %imm1,%imm0)
	__(jne 1b)
	__(movq %imm1,rcontext(tcr.db_link))
	__(ret)	
_endsubp(unbind_to)


/* Bind CCL::*INTERRUPT-LEVEL* to 0.  If its value had been negative, check   */
/* for pending interrupts after doing so.   */
	
_spentry(bind_interrupt_level_0)
	__(movq rcontext(tcr.tlb_pointer),%temp1)
	__(cmpq $0,INTERRUPT_LEVEL_BINDING_INDEX(%temp1))
	__(push INTERRUPT_LEVEL_BINDING_INDEX(%temp1))
	__(push $INTERRUPT_LEVEL_BINDING_INDEX)
	__(push rcontext(tcr.db_link))
	__(movq %rsp,rcontext(tcr.db_link))
	__(movq $0,INTERRUPT_LEVEL_BINDING_INDEX(%temp1))
	__(js 1f)
0:	__(jmp *%ra0)
	/* Interrupt level was negative; interrupt may be pending   */
1:	__(check_pending_enabled_interrupt(2f))
2:	__(jmp *%ra0)
_endsubp(bind_interrupt_level_0)
	

/* Bind CCL::*INTERRUPT-LEVEL* to the fixnum -1.  (This has the effect  */
/* of disabling interrupts.)   */

_spentry(bind_interrupt_level_m1)
	__(movq rcontext(tcr.tlb_pointer),%temp1)
	__(push INTERRUPT_LEVEL_BINDING_INDEX(%temp1))
	__(push $INTERRUPT_LEVEL_BINDING_INDEX)
	__(push rcontext(tcr.db_link))
	__(movq %rsp,rcontext(tcr.db_link))
	__(movq $-1<<fixnumshift,INTERRUPT_LEVEL_BINDING_INDEX(%temp1))
	__(jmp *%ra0)
_endsubp(bind_interrupt_level_m1)

/* Bind CCL::*INTERRUPT-LEVEL* to the value in arg_z.  If that value's 0,  */
/* do what _SPbind_interrupt_level_0 does   */
_spentry(bind_interrupt_level)
	__(testq %arg_z,%arg_z)
	__(movq rcontext(tcr.tlb_pointer),%temp1)
	__(jz _SPbind_interrupt_level_0)
	__(push INTERRUPT_LEVEL_BINDING_INDEX(%temp1))
	__(push $INTERRUPT_LEVEL_BINDING_INDEX)
	__(push rcontext(tcr.db_link))
	__(movq %rsp,rcontext(tcr.db_link))
	__(movq %arg_z,INTERRUPT_LEVEL_BINDING_INDEX(%temp1))
	__(jmp *%ra0)
_endsubp(bind_interrupt_level)

/* Unbind CCL::*INTERRUPT-LEVEL*.  If the value changes from negative to  */
/* non-negative, check for pending interrupts.    */
	
_spentry(unbind_interrupt_level)
        __(btq $TCR_FLAG_BIT_PENDING_SUSPEND,rcontext(tcr.flags))
	__(movq rcontext(tcr.db_link),%imm1)
	__(movq rcontext(tcr.tlb_pointer),%arg_x)
	__(movq INTERRUPT_LEVEL_BINDING_INDEX(%arg_x),%imm0)
        __(jc 5f)
0:      __(testq %imm0,%imm0)
	__(movq binding.val(%imm1),%temp0)
	__(movq binding.link(%imm1),%imm1)
	__(movq %temp0,INTERRUPT_LEVEL_BINDING_INDEX(%arg_x))
 	__(movq %imm1,rcontext(tcr.db_link))
	__(js 3f)
2:	__(repret)
3:	__(testq %temp0,%temp0)
	__(js 2b)
	__(check_pending_enabled_interrupt(4f))
4:	__(repret)
5:       /* Missed a suspend request; force suspend now if we're restoring
          interrupt level to -1 or greater */
        __(cmpq $-2<<fixnumshift,%imm0)
        __(jne 0b)
	__(movq binding.val(%imm1),%temp0)
        __(cmpq %imm0,%temp0)
        __(je 0b)
        __(movq $-1<<fixnumshift,INTERRUPT_LEVEL_BINDING_INDEX(%arg_x))
        __(suspend_now())
        __(jmp 0b)
_endsubp(unbind_interrupt_level)

	
_spentry(progvrestore)
	__(movq rcontext(tcr.save_tsp),%imm0)
	__(movq tsp_frame.backlink(%imm0),%imm0) /* ignore .SPnthrowXXX values frame   */
	__(movq tsp_frame.data_offset(%imm0),%imm0)
	__(shrq $fixnumshift,%imm0)
	__(jne _SPunbind_n)
	__(repret)
_endsubp(progvrestore)
	

/* %arg_z <- %arg_y + %arg_z.  Do the fixnum case - including overflow -  */
/* inline.  Call out otherwise.   */
_spentry(builtin_plus)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(addq %arg_y,%arg_z)
	__(jo C(fix_one_bit_overflow))
	__(repret)
1:	__(jump_builtin(_builtin_plus,2))
_endsubp(builtin_plus)
	

/* %arg_z <- %arg_z - %arg_y.  Do the fixnum case - including overflow -  */
/*  inline.  Call out otherwise.   */
_spentry(builtin_minus)			
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(xchgq %arg_y,%arg_z)
	__(subq %arg_y,%arg_z)
	__(jo C(fix_one_bit_overflow))
	__(repret)
1:	__(jump_builtin(_builtin_minus,2))
_endsubp(builtin_minus)

/* %arg_z <- %arg_z * %arg_y.  Do the fixnum case - including overflow -  */
/* inline.  Call out otherwise.   */
_spentry(builtin_times)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 2f)
	__(unbox_fixnum(%arg_z,%imm0))
	/* 128-bit fixnum result in %imm1:%imm0. Overflow set if %imm1  */
	/* is significant   */
	__(imul %arg_y)
	__(jo 1f)
	__(mov %imm0,%arg_z)
	__(ret)
1:	__(unbox_fixnum(%arg_z,%imm0))
	__(unbox_fixnum(%arg_y,%imm1))
	__(imul %imm1)
	__(jmp C(makes128))
2:	__(jump_builtin(_builtin_times,2))
_endsubp(builtin_times)

_spentry(builtin_div)
	__(jump_builtin(_builtin_div,2))

/* %arg_z <- (= %arg_y %arg_z).	  */
_spentry(builtin_eq)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpq(%arg_z,%arg_y))
	__(condition_to_boolean(e,%imm0,%arg_z))
	__(ret)
1:	__(jump_builtin(_builtin_eq,2))
_endsubp(builtin_eq)
	
/* %arg_z <- (/= %arg_y %arg_z).	  */
_spentry(builtin_ne)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpq(%arg_z,%arg_y))
	__(condition_to_boolean(ne,%imm0,%arg_z))
	__(ret)
1:	__(jump_builtin(_builtin_ne,2))
_endsubp(builtin_ne)
	
/* %arg_z <- (> %arg_y %arg_z).	  */
_spentry(builtin_gt)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpq(%arg_y,%arg_z))
	__(condition_to_boolean(g,%imm0,%arg_z))
	__(ret)
1:	__(jump_builtin(_builtin_gt,2))
_endsubp(builtin_gt)

/* %arg_z <- (>= %arg_y %arg_z).	  */
_spentry(builtin_ge)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpq(%arg_y,%arg_z))
	__(condition_to_boolean(ge,%imm0,%arg_z))
	__(ret)
1:	__(jump_builtin(_builtin_ge,2))
_endsubp(builtin_ge)
	
/* %arg_z <- (< %arg_y %arg_z).	  */
_spentry(builtin_lt)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpq(%arg_y,%arg_z))
	__(condition_to_boolean(l,%imm0,%arg_z))
	__(ret)
1:	__(jump_builtin(_builtin_lt,2))
_endsubp(builtin_lt)

/* %arg_z <- (<= %arg_y %arg_z).   */
_spentry(builtin_le)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpq(%arg_y,%arg_z))
	__(condition_to_boolean(le,%imm0,%arg_z))
	__(ret)
1:	__(jump_builtin(_builtin_le,2))
_endsubp(builtin_le)

_spentry(builtin_eql)
0:      __(cmpq %arg_y,%arg_z)
	__(je 8f)
	/* Not EQ.  Could only possibly be EQL if both are tag-misc  */
	/* and both have the same subtag   */
	__(extract_lisptag(%arg_y,%imm0))
	__(extract_lisptag(%arg_z,%imm1))
	__(cmpb $tag_misc,%imm0_b)
	__(jne 9f)
	__(cmpb %imm0_b,%imm1_b)
	__(jne 9f)
	__(extract_subtag(%arg_y,%imm0))
	__(extract_subtag(%arg_z,%imm1))
	__(cmpb %imm0_b,%imm1_b)
	__(jne 9f)
        __(cmpb $subtag_macptr,%imm0_b)
        __(je 1f)
        __(cmpb $subtag_double_float,%imm0_b)
        __(jne 2f)
1:      __(movq misc_data_offset(%arg_y),%imm0)
        __(cmpq misc_data_offset(%arg_z),%imm0)
        __(movl $t_value,%arg_z_l)
        __(movl $nil_value,%imm0_l)
        __(cmovnel %imm0_l,%arg_z_l)
        __(ret)
2:      __(cmpb $subtag_ratio,%imm0_b)
        __(je 3f)
        __(cmpb $subtag_complex,%imm0_b)
        __(jne 5f)
3:      __(pushq %rbp)
        __(movq %rsp,%rsp)
        __(pushq ratio.denom(%arg_y))
        __(pushq ratio.denom(%arg_z))
        __(movq ratio.numer(%arg_y),%arg_y)
        __(movq ratio.numer(%arg_z),%arg_z)
        __(lea 4f(%rip),%ra0)
        __(pushq %ra0)
        __(jmp 0b)
__(tra(4))
        __(compare_reg_to_nil(%arg_z))
        __(popq %arg_z)
        __(popq %arg_y)
        __(popq %rbp)
        __(je 9f)
        __(jmp 0b)
5:      __(cmpb $subtag_bignum,%imm0_b)
        __(jne 9f)
        __(getvheader(%arg_y,%imm2))
        __(cmpq misc_header_offset(%arg_z),%imm2)
        __(jne 9f)
        __(shrq $num_subtag_bits,%imm2)
        __(xorl %imm1_l,%imm1_l)
6:      __(movl misc_data_offset(%arg_y,%imm1,4),%imm0_l)
        __(cmpl misc_data_offset(%arg_z,%imm1,4),%imm0_l)
        __(jne 9f)
        __(addq $1,%imm1)
        __(cmpq %imm1,%imm2)
        __(jne 6b)
8:	__(movl $t_value,%arg_z_l)
	__(ret)
9:	__(movl $nil_value,%arg_z_l)
	__(ret)	
_endsubp(builtin_eql)

_spentry(builtin_length)
	__(extract_lisptag(%arg_z,%imm0))
	__(cmpb $tag_list,%imm0_b)
	__(jz 2f)
	__(cmpb $tag_misc,%imm0_b)
	__(jnz 8f)
	__(extract_subtag(%arg_z,%imm0))
	__(rcmpb(%imm0_b,$min_vector_subtag))
	__(jb 8f)
	__(je 1f)
	/* (simple-array * (*))   */
	__(movq %arg_z,%arg_y)
	__(vector_length(%arg_y,%arg_z))
	__(ret)
1:	/* vector header   */
	__(movq vectorH.logsize(%arg_z),%arg_z)
	__(ret)
2:	/* list.  Maybe null, maybe dotted or circular.   */
	__(movq $-fixnumone,%imm2)
	__(movq %arg_z,%temp0)	/* fast pointer   */
	__(movq %arg_z,%temp1)  /* slow pointer   */
3:	__(extract_lisptag(%temp0,%imm0))	
	__(compare_reg_to_nil(%temp0))
	__(leaq fixnumone(%imm2),%imm2)
	__(je 9f)
	__(cmpb $tag_list,%imm0_b)
	__(jne 8f)
	__(extract_lisptag(%temp1,%imm1))
	__(testb $fixnumone,%imm2_b)
	__(_cdr(%temp0,%temp0))
	__(je 3b)
	__(cmpb $tag_list,%imm1_b)
	__(jne 8f)
	__(_cdr(%temp1,%temp1))
	__(cmpq %temp0,%temp1)
	__(jne 3b)
8:	
	__(jump_builtin(_builtin_length,1))
9:	
	__(movq %imm2,%arg_z)
	__(ret)		
_endsubp(builtin_length)

	
_spentry(builtin_seqtype)
	__(extract_lisptag(%arg_z,%imm0))
	__(cmpb $tag_list,%imm0_b)
	__(jz 1f)
	__(cmpb $tag_misc,%imm0_b)
	__(jne 2f)
	__(movb misc_subtag_offset(%arg_z),%imm0_b)
	__(rcmpb(%imm0_b,$min_vector_subtag))
	__(jb 2f)
	__(movl $nil_value,%arg_z_l)
	__(ret)
1:	__(movl $t_value,%arg_z_l)
	__(ret)
2:	
	__(jump_builtin(_builtin_seqtype,1))
_endsubp(builtin_seqtype)

_spentry(builtin_assq)
	__(cmpb $fulltag_nil,%arg_z_b)
	__(jz 5f)
1:	__(movb $tagmask,%imm0_b)
	__(andb %arg_z_b,%imm0_b)
	__(cmpb $tag_list,%imm0_b)
	__(jnz 2f)
	__(_car(%arg_z,%arg_x))
	__(_cdr(%arg_z,%arg_z))
	__(cmpb $fulltag_nil,%arg_x_b)
	__(jz 4f)
	__(movb $tagmask,%imm0_b)
	__(andb %arg_x_b,%imm0_b)
	__(cmpb $tag_list,%imm0_b)
	__(jnz 3f)
	__(_car(%arg_x,%temp0))
	__(cmpq %temp0,%arg_y)
	__(jnz 4f)
	__(movq %arg_x,%arg_z)
	__(ret)
4:	__(cmpb $fulltag_nil,%arg_z_b)
5:	__(jnz 1b)
	__(repret)
2:      __(uuo_error_reg_not_list(Rarg_z))
3:      __(uuo_error_reg_not_list(Rarg_x))        
_endsubp(builtin_assq)	

_spentry(builtin_memq)
	__(cmpb $fulltag_nil,%arg_z_b)
	__(jmp 3f)
1:	__(movb $tagmask,%imm0_b)
	__(andb %arg_z_b,%imm0_b)
	__(cmpb $tag_list,%imm0_b)
	__(jnz 2f)
	__(_car(%arg_z,%arg_x))
	__(_cdr(%arg_z,%temp0))
	__(cmpq %arg_x,%arg_y)
	__(jz 4f)
	__(cmpb $fulltag_nil,%temp0_b)
	__(movq %temp0,%arg_z)
3:	__(jnz 1b)
4:	__(repret)				
2:      __(uuo_error_reg_not_list(Rarg_z))
_endsubp(builtin_memq)

        __ifdef(`X8664')
logbitp_max_bit = 61
        __else
logbitp_max_bit = 30
        __endif
	
_spentry(builtin_logbitp)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jnz 1f)
	__(unbox_fixnum(%arg_y,%imm0))
        __(movl $logbitp_max_bit-1+fixnumshift,%imm1_l)
        __(js 1f)               /* bit number negative */
	__(addb $fixnumshift,%imm0_b)
	__(cmpq $logbitp_max_bit<<fixnumshift,%arg_y)
	__(cmovael %imm1_l,%imm0_l)
	__(bt %imm0,%arg_z)
	__(condition_to_boolean(b,%imm0,%arg_z))
	__(ret)
1:	__(jump_builtin(_builtin_logbitp,2))
_endsubp(builtin_logbitp)

_spentry(builtin_logior)
	__(movb %arg_y_b,%imm0_b)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(orq %arg_y,%arg_z)
	__(ret)
1:	
	__(jump_builtin(_builtin_logior,2))
		
_endsubp(builtin_logior)

_spentry(builtin_logand)
	__(movb %arg_y_b,%imm0_b)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(andq %arg_y,%arg_z)
	__(ret)
1:		
	__(jump_builtin(_builtin_logand,2))
_endsubp(builtin_logand)

_spentry(builtin_negate)
	__(testb $fixnummask,%arg_z_b)
	__(jne 1f)
	__(negq %arg_z)
	__(jo C(fix_one_bit_overflow))
	__(repret)
1:		
	__(jump_builtin(_builtin_negate,1))	
_endsubp(builtin_negate)

_spentry(builtin_logxor)
	__(movb %arg_y_b,%imm0_b)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(xorq %arg_y,%arg_z)
	__(ret)
1:		
	__(jump_builtin(_builtin_logxor,2))
_endsubp(builtin_logxor)


_spentry(builtin_aset1)
	__(extract_typecode(%arg_x,%imm0))
	__(box_fixnum(%imm0,%temp0))
	__(cmpb $min_vector_subtag,%imm0_b)
	__(ja _SPsubtag_misc_set)
	__(jump_builtin(_builtin_aset1,3))
_endsubp(builtin_aset1)


_spentry(builtin_ash)
	__(movb %arg_y_b,%imm0_b)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 9f)
	__(unbox_fixnum(%arg_y,%imm1))
	__(unbox_fixnum(%arg_z,%imm0))
	/* Z flag set if zero ASH shift count   */
	__(jnz 1f)
	__(movq %arg_y,%arg_z)	/* shift by 0   */
	__(ret)
1:	__(jns 3f)
	__(rcmpq(%imm0,$-63))
	__(jg 2f)
	__(sar $63,%imm1)
	__(box_fixnum(%imm1,%arg_z))
	__(ret)
2:	/* Right-shift by small fixnum   */
	__(negb %imm0_b)
	__(movzbl %imm0_b,%ecx)
	__(sar %cl,%imm1)
	__(box_fixnum(%imm1,%arg_z))
	__(ret)
3:      /* Left shift by fixnum. We cant shift by more than 63 bits, though  */
	/* shifting by 64 is actually easy.   */
	__(rcmpq(%imm0,$64))
	__(jg 9f)
	__(jne 4f)
	/* left-shift by 64-bits exactly   */
	__(xorl %imm0_l,%imm0_l)
	__(jmp C(makes128))
4:	/* left-shift by 1..63 bits.  Safe to move shift count to %rcx/%cl   */
	__(movzbl %imm0_b,%ecx)	 /* zero-extending mov   */
	__(movq %imm1,%imm0)
	__(sarq $63,%imm1)
	__(js 5f)
	__(shld %cl,%imm0,%imm1)
	__(shl %cl,%imm0)
	__(jmp C(makes128))
5:	__(shld %cl,%imm0,%imm1)
	__(shl %cl,%imm0)
	__(jmp C(makes128))
9:	
	__(jump_builtin(_builtin_ash,2))
_endsubp(builtin_ash)

_spentry(builtin_aref1)
	__(extract_typecode(%arg_y,%imm0))
	__(cmpb $min_vector_subtag,%imm0_b)
	__(box_fixnum_no_flags(%imm0,%arg_x))
	__(ja _SPsubtag_misc_ref)
	__(jump_builtin(_builtin_aref1,2))
_endsubp(builtin_aref1)

/* Arg_z is either a MACPTR containing the function address or a boxed fixnum.  */
/*   %imm0.b (aka %al) contains the number (0-7) of args passed in FP regs.  */
/*   On entry, the foreign stack contains a frame containing at least 8 words:  */

/*   * -> aligned on 16-byte boundary  */
/*  *backlink	<-	foreign %rsp		  */
/*   unused  */
/*   scalar arg 0		passed in %rdi  */
/*   scalar arg 1         passed in %rsi  */
/*   scalar arg 2		passed in %rdx  */
/*   scalar arg 3		passed in %rcx  */
/*   scalar arg 4		passed in %r8  */
/*   scalar arg 5		passed in %r9  */
/*  *address of first memory arg  */
/*   ...  */
/*   possible scratch space  */
/*  *previous %rsp value  */

/*   Any floating-point args will have been loaded into %xmm0-%xmm7 by the caller.  */
/*   When this returns, the foreign %rsp will contain its previous value, and  */
/*   the function result will be in %rax (and possibly %rdx) or %xmm0 (+ %xmm1).  */

        .globl C(ffcall_return)
_spentry(ffcall)
LocalLabelPrefix`'ffcall:                
        /* Unbox %arg_z.  It's either a fixnum or macptr (or bignum) ;
          if not a fixnum, get the first word */
        __(unbox_fixnum(%arg_z,%imm1))
	__(testb $fixnummask,%arg_z_b)
        __(je 0f)
        __(movq macptr.address(%arg_z),%imm1)
0:              
	/* Save lisp registers   */
        __(push %rbp)
	__(movq %rsp,%rbp)
	__(push %temp0)
	__(push %temp1)
	__(push %temp2)
	__(push %arg_x)
	__(push %arg_y)
	__(push %arg_z)
	__(push %fn)
	__ifndef(`TCR_IN_GPR')
	__(push %save3)  
	__endif
	__(push %save2)
	__(push %save1)
	__(push %save0)       /* 10 or 11 registers pushed after %rbp */
	__(movq %rsp,rcontext(tcr.save_vsp))
        __(movq %rbp,rcontext(tcr.save_rbp))
	__(movq $TCR_STATE_FOREIGN,rcontext(tcr.valence))
        __(movq rcontext(tcr.foreign_sp),%rsp)
        __ifdef(`WINDOWS')
	__(stmxcsr rcontext(tcr.lisp_mxcsr))
        __else
	__(movq $0,rcontext(tcr.ffi_exception))
        __endif
	__(emms)
        __ifdef(`WINDOWS')
	__(ldmxcsr rcontext(tcr.foreign_mxcsr))
        __endif
	__(movq (%rsp),%rbp)
	__ifdef(`TCR_IN_GPR')
	/* Preserve TCR pointer */
	__(movq %rcontext_reg, %csave0)
	__endif
LocalLabelPrefix`'ffcall_setup: 
	__(addq $2*node_size,%rsp)
        __(movq %imm1,%r11)
        __ifdef(`WINDOWS')
         /* Leave 0x20 bytes of register spill area on stack */
         __(movq (%rsp),%carg0)
         __(movq 8(%rsp),%carg1)
         __(movq 16(%rsp),%carg2)
         __(movq 24(%rsp),%carg3)
        __else
	 __(pop %carg0)
	 __(pop %carg1)
	 __(pop %carg2)
	 __(pop %carg3)
	 __(pop %carg4)
	 __(pop %carg5)
	__endif
LocalLabelPrefix`'ffcall_setup_end: 
LocalLabelPrefix`'ffcall_call:
	__(call *%r11)
C(ffcall_return):               
LocalLabelPrefix`'ffcall_call_end:               
	__ifdef(`WINDOWS')
	__(add $0x20,%rsp)
	__endif
	__(movq %rbp,%rsp)
	__ifdef(`TCR_IN_GPR')
	__(movq %csave0, %rcontext_reg)
	__endif
	__(movq %rsp,rcontext(tcr.foreign_sp))
	__ifndef(`TCR_IN_GPR')
	__(clr %save3)
	__endif
	__(clr %save2)
	__(clr %save1)
	__(clr %save0)
	__(clr %arg_z)
	__(clr %arg_y)
	__(clr %arg_x)
	__(clr %temp2)
	__(clr %temp1)
	__(clr %temp0)
	__(clr %fn)
	__(pxor %fpzero,%fpzero)

        __ifndef(`WINDOWS')
	/* If we got a floating-point exception during the ff-call,
	   our handler will have set a flag, preserved lisp's MXCSR,
	   and resumed execution with fp exceptions masked. */
	__(btrq $TCR_FLAG_BIT_FOREIGN_FPE,rcontext(tcr.flags))
	__(jnc 1f)
        __endif
        __(cmpb $0,C(bogus_fp_exceptions)(%rip))
        __(je 0f)
        __(movl %arg_x_l,rcontext(tcr.ffi_exception))
        __(jmp 1f)
0:      __(stmxcsr rcontext(tcr.ffi_exception))
        __ifndef(`WINDOWS')
	__(ldmxcsr rcontext(tcr.lisp_mxcsr)) /* preserved by the handler */
        __endif
1:      __(movq rcontext(tcr.save_vsp),%rsp)
        __(movq rcontext(tcr.save_rbp),%rbp)
	__(movq $TCR_STATE_LISP,rcontext(tcr.valence))
	__(pop %save0)
	__(pop %save1)
	__(pop %save2)
	__ifndef(`TCR_IN_GPR')
	__(pop %save3)
	__endif
	__(pop %fn)
	__(pop %arg_z)
	__(pop %arg_y)
	__(pop %arg_x)
	__(pop %temp2)
	__(pop %temp1)
        __ifdef(`WINDOWS')
	__(ldmxcsr rcontext(tcr.lisp_mxcsr))
        __endif
	__(check_pending_interrupt(%temp0))
	__(pop %temp0)
        __(leave)
	__ifdef(`DARWIN')
	__(btrq $TCR_FLAG_BIT_FOREIGN_EXCEPTION,rcontext(tcr.flags))
	__(jc 0f)
	__endif
	__(ret)
	__ifdef(`DARWIN')
0:
	/* Unboxed foreign exception (likely an NSException) in %imm0. */
	/* Box it, then signal a lisp error. */
	__(movq %imm0,%imm2)
	__(movq $macptr_header,%rax)
	__(Misc_Alloc_Fixed(%arg_z,macptr.size))
	__(movq %imm2,macptr.address(%arg_z))
	__(movq $XFOREIGNEXCEPTION,%arg_y)
	__(set_nargs(2))
	__(jmp _SPksignalerr)
	__endif
        __ifdef(`DARWIN')        
        /* Handle exceptions, for ObjC 2.0 */
LocalLabelPrefix`'ffcallLandingPad:      
        __(movq %rax,%save1)
        __(cmpq $1,%rdx)
        __(je 1f)
        __(movq %rax,%rdi)
LocalLabelPrefix`'ffcallUnwindResume:            
       	__(call *lisp_global(unwind_resume))
LocalLabelPrefix`'ffcallUnwindResume_end:         
1:      __(movq %save1,%rdi)
LocalLabelPrefix`'ffcallBeginCatch:              
        __(call *lisp_global(objc_2_begin_catch))
LocalLabelPrefix`'ffcallBeginCatch_end:          
        __(movq (%rax),%save1) /* indirection is necessary because we don't provide type info in lsda */
LocalLabelPrefix`'ffcallEndCatch:                
        __(call *lisp_global(objc_2_end_catch))
LocalLabelPrefix`'ffcallEndCatch_end:            
	__(ref_global(get_tcr,%rax))
	__(movq $1,%rdi)
	__(call *%rax)
	__(btsq $TCR_FLAG_BIT_FOREIGN_EXCEPTION,tcr.flags(%rax))
	__(movq %save1,%rax)
	__(jmp LocalLabelPrefix`'ffcall_call_end)
LocalLabelPrefix`'ffcall_end:   
        __endif
_endsubp(ffcall)

        __ifdef(`DARWIN')
	.section __DATA,__gcc_except_tab
GCC_except_table0:
	.align 3
LLSDA1:
	.byte	0xff	/* @LPStart format (omit) */
	.byte	0x0	/* @TType format (absolute) */
	.byte	0x4d	/* uleb128 0x4d; @TType base offset */
	.byte	0x3	/* call-site format (udata4) */
	.byte	0x41	/* uleb128 0x41; Call-site table length */
	
	.long Lffcall_setup-Lffcall	/* region 0 start */
	.long Lffcall_setup_end-Lffcall_setup	/* length */
	.long	0x0	/* landing pad */
	.byte	0x0	/* uleb128 0x0; action */
        
	.long Lffcall_call-Lffcall	/* region 1 start */
	.long Lffcall_call_end-Lffcall_call	/* length */
	.long LffcallLandingPad-Lffcall	/* landing pad */
	.byte	0x1	/* uleb128 0x1; action */
        
	.long LffcallUnwindResume-Lffcall	/* region 2 start */
	.long LffcallUnwindResume_end-LffcallUnwindResume	/* length */
	.long	0x0	/* landing pad */
	.byte	0x0	/* uleb128 0x0; action */
	
	.long LffcallBeginCatch-Lffcall	/* region 3 start */
	.long LffcallBeginCatch_end-LffcallBeginCatch	/* length */
	.long 0	/* landing pad */
	.byte	0x0	/* uleb128 0x0; action */
        
	.long LffcallEndCatch-Lffcall
	.long LffcallEndCatch_end-LffcallEndCatch	/* length */
	.long	0x0	/* landing pad */
	.byte	0x0	/* uleb128 0x0; action */
	.byte	0x1	/* Action record table */
	.byte	0x0
	.align 3
	.quad	0       /* _OBJC_EHTYPE_$_NSException */
        .text
        __endif

_spentry(ffcall_return_registers)
LocalLabelPrefix`'ffcall_return_registers:                
        /* Unbox %arg_z.  It's either a fixnum or macptr (or bignum) ;
          if not a fixnum, get the first word */
        __(unbox_fixnum(%arg_z,%imm1))
	__(testb $fixnummask,%arg_z_b)
        __(je 0f)
        __(movq macptr.address(%arg_z),%imm1)
0:              
	/* Save lisp registers   */
        __(push %rbp)
        __(movq %rsp,%rbp)
	__(push %temp0)
	__(push %temp1)
	__(push %temp2)
	__(push %arg_x)
	__(push %arg_y)
	__(push %arg_z)
	__ifndef(`TCR_IN_GPR')
	__(push %save3)
	__endif
	__(push %save2)
	__(push %save1)
	__(push %save0)
        __(movq macptr.address(%arg_y),%csave0)  /* %rbx non-volatile */
	__(push %fn)
	__(movq %rsp,rcontext(tcr.save_vsp))
        __(movq %rbp,rcontext(tcr.save_rbp))
	__(movq $TCR_STATE_FOREIGN,rcontext(tcr.valence))
	__(movq $0,rcontext(tcr.ffi_exception))
        __(movq rcontext(tcr.foreign_sp),%rsp)
	__(emms)
	__(movq (%rsp),%rbp)
	__ifdef(`TCR_IN_GPR')
	/* Preserve TCR pointer */
	__(movq %rcontext_reg, %csave1)
	__endif
        __(movq %imm1,%r11)
LocalLabelPrefix`'ffcall_return_registers_setup: 
	__(addq $2*node_size,%rsp)
	__(pop %carg0)
	__(pop %carg1)
	__(pop %carg2)
	__(pop %carg3)
	__ifdef(`WINDOWS')
	__(sub $0x20, %rsp) /* Make room for arg register spill */
	__else
	__(pop %carg4)
	__(pop %carg5)
	__endif
LocalLabelPrefix`'ffcall_return_registers_setup_end: 
LocalLabelPrefix`'ffcall_return_registers_call:
	__(call *%r11)
LocalLabelPrefix`'ffcall_return_registers_call_end:
	__ifdef(`WINDOWS')
	__(add $0x20, %rsp)
	__endif
        __(movq %rax,(%csave0))
        __(movq %rdx,8(%csave0))
        __(movsd %xmm0,16(%csave0))
        __(movsd %xmm1,24(%csave0))
	__(movq %rbp,%rsp)
	__ifdef(`TCR_IN_GPR')
	__(movq %csave1, %rcontext_reg)
	__endif
	__(movq %rsp,rcontext(tcr.foreign_sp))        
	__ifndef(`TCR_IN_GPR')
	__(clr %save3)
	__endif
	__(clr %save2)
	__(clr %save1)
	__(clr %save0)
	__(clr %arg_z)
	__(clr %arg_y)
	__(clr %arg_x)
	__(clr %temp2)
	__(clr %temp1)
	__(clr %temp0)
	__(clr %fn)
	__(pxor %fpzero,%fpzero)
	/* Check for fp exceptions as in .SPffcall, above. */
	__(btrq $TCR_FLAG_BIT_FOREIGN_FPE,rcontext(tcr.flags))
	__(jnc 1f)
        __(cmpb $0,C(bogus_fp_exceptions)(%rip))
        __(je 0f)
        __(movl %arg_x_l,rcontext(tcr.ffi_exception))
        __(jmp 1f)
0:      __(stmxcsr rcontext(tcr.ffi_exception))
	__(ldmxcsr rcontext(tcr.lisp_mxcsr))
1:      __(movq rcontext(tcr.save_vsp),%rsp)
        __(movq rcontext(tcr.save_rbp),%rbp)
	__(movq $TCR_STATE_LISP,rcontext(tcr.valence))
	__(pop %fn)
	__(pop %save0)
	__(pop %save1)
	__(pop %save2)
	__ifndef(`TCR_IN_GPR')
	__(pop %save3)
	__endif
	__(pop %arg_z)
	__(pop %arg_y)
	__(pop %arg_x)
	__(pop %temp2)
	__(pop %temp1)
	__(check_pending_interrupt(%temp0))
	__(pop %temp0)
        __(leave)
	__ifdef(`DARWIN')
	__(btrq $TCR_FLAG_BIT_FOREIGN_EXCEPTION,rcontext(tcr.flags))
	__(jc 0f)
	__endif
        __(ret)
	__ifdef(`DARWIN')
0:
	/* Unboxed foreign exception (likely an NSException) in %imm0. */
	/* Box it, then signal a lisp error. */
	__(movq %imm0,%imm2)
	__(movq $macptr_header,%rax)
	__(Misc_Alloc_Fixed(%arg_z,macptr.size))
	__(movq %imm2,macptr.address(%arg_z))
	__(movq $XFOREIGNEXCEPTION,%arg_y)
	__(set_nargs(2))
	__(jmp _SPksignalerr)
	__endif
        __ifdef(`DARWIN')        
        /* Handle exceptions, for ObjC 2.0 */
LocalLabelPrefix`'ffcall_return_registersLandingPad:      
        __(movq %rax,%save1)
        __(cmpq $1,%rdx)
        __(je 1f)
        __(movq %rax,%rdi)
LocalLabelPrefix`'ffcall_return_registersUnwindResume:            
       	__(call *lisp_global(unwind_resume))
LocalLabelPrefix`'ffcall_return_registersUnwindResume_end:         
1:      __(movq %save1,%rdi)
LocalLabelPrefix`'ffcall_return_registersBeginCatch:              
        __(call *lisp_global(objc_2_begin_catch))
LocalLabelPrefix`'ffcall_return_registersBeginCatch_end:          
        __(movq (%rax),%save1) /* indirection is necessary because we don't provide type info in lsda */
LocalLabelPrefix`'ffcall_return_registersEndCatch:                
        __(call *lisp_global(objc_2_end_catch))
LocalLabelPrefix`'ffcall_return_registersEndCatch_end:            
	__(ref_global(get_tcr,%rax))
	__(movq $1,%rdi)
	__(call *%rax)
	__(btsq $TCR_FLAG_BIT_FOREIGN_EXCEPTION,tcr.flags(%rax))
	__(movq %save1,%rax)
	__(jmp LocalLabelPrefix`'ffcall_return_registers_call_end)
LocalLabelPrefix`'ffcall_return_registers_end:   
        __endif
_endsubp(ffcall_returning_registers)

        __ifdef(`DARWIN')
	.section __DATA,__gcc_except_tab
GCC_except_table1:
	.align 3
LLSDA2:
	.byte	0xff	/* @LPStart format (omit) */
	.byte	0x0	/* @TType format (absolute) */
	.byte	0x4d	/* uleb128 0x4d; @TType base offset */
	.byte	0x3	/* call-site format (udata4) */
	.byte	0x41	/* uleb128 0x41; Call-site table length */
	
	.long Lffcall_return_registers_setup-Lffcall_return_registers	/* region 0 start */
	.long Lffcall_return_registers_setup_end-Lffcall_return_registers_setup	/* length */
	.long	0x0	/* landing pad */
	.byte	0x0	/* uleb128 0x0; action */
        
	.long Lffcall_return_registers_call-Lffcall_return_registers	/* region 1 start */
	.long Lffcall_return_registers_call_end-Lffcall_return_registers_call	/* length */
	.long Lffcall_return_registersLandingPad-Lffcall_return_registers	/* landing pad */
	.byte	0x1	/* uleb128 0x1; action */
        
	.long Lffcall_return_registersUnwindResume-Lffcall_return_registers	/* region 2 start */
	.long Lffcall_return_registersUnwindResume_end-Lffcall_return_registersUnwindResume	/* length */
	.long	0x0	/* landing pad */
	.byte	0x0	/* uleb128 0x0; action */
	
	.long Lffcall_return_registersBeginCatch-Lffcall_return_registers	/* region 3 start */
	.long Lffcall_return_registersBeginCatch_end-Lffcall_return_registersBeginCatch	/* length */
	.long 0	/* landing pad */
	.byte	0x0	/* uleb128 0x0; action */
        
	.long Lffcall_return_registersEndCatch-Lffcall_return_registers
	.long Lffcall_return_registersEndCatch_end-Lffcall_return_registersEndCatch	/* length */
	.long	0x0	/* landing pad */
	.byte	0x0	/* uleb128 0x0; action */
	.byte	0x1	/* Action record table */
	.byte	0x0
	.align 3
	.quad	0       /* _OBJC_EHTYPE_$_NSException */
        .text
        __endif
                
_spentry(syscall)
	/* Save lisp registers   */
	__(push %rbp)
	__(movq %rsp,%rbp)
	__(push %temp0)
	__(push %temp1)
	__(push %temp2)
	__(push %arg_x)
	__(push %arg_y)
	__(push %arg_z)
        __ifndef(`TCR_IN_GPR')
	 __(push %save3)
        __endif
	__(push %save2)
	__(push %save1)
	__(push %save0)
	__(push %fn)
	__(movq %rsp,rcontext(tcr.save_vsp))
        __(movq %rbp,rcontext(tcr.save_rbp))
	__(movq $TCR_STATE_FOREIGN,rcontext(tcr.valence))
        __(movq rcontext(tcr.foreign_sp),%rsp)
	__(emms)
	__(movq (%rsp),%rbp)
	__(addq $2*node_size,%rsp)
        __ifdef(`TCR_IN_GPR')
         __(movq %rcontext_reg,%csave0)
        __endif
        __ifdef(`WINDOWS')
         __(pop %carg0)
         __(pop %carg1)
         __(pop %carg2)
         __(pop %carg3)
         __(subq $0x20,%rsp)
         __(orq $-1,%cret)
         __(addq $0x20,%rsp)
        __else
	 __(unbox_fixnum(%arg_z,%rax))
	 __(pop %rdi)
	 __(pop %rsi)
	 __(pop %rdx)
	 __(pop %r10)		/*  syscalls take 4th param in %r10, not %rcx   */
	 __(pop %r8)
	 __(pop %r9)
	 __(syscall)
         __ifdef(`SYSCALL_SETS_CARRY_ON_ERROR')
          __(jnc 0f)
          __(negq %rax)
0:      
         __endif
        __endif
        __ifdef(`TCR_IN_GPR')
         __(movq %csave0,%rcontext_reg)
        __endif
	__(movq %rbp,%rsp)
	__(movq %rsp,rcontext(tcr.foreign_sp))
        __ifndef(`TCR_IN_GPR')
	 __(clr %save3)
        __endif
	__(clr %save2)
	__(clr %save1)
	__(clr %save0)
	__(clr %arg_z)
	__(clr %arg_y)
	__(clr %arg_x)
	__(clr %temp2)
	__(clr %temp1)
	__(clr %temp0)
	__(clr %fn)
	__(pxor %fpzero,%fpzero)
	__(movq rcontext(tcr.save_vsp),%rsp)
        __(movq rcontext(tcr.save_rbp),%rbp)
	__(movq $TCR_STATE_LISP,rcontext(tcr.valence))
	__(pop %fn)
	__(pop %save0)
	__(pop %save1)
	__(pop %save2)
        __ifndef(`TCR_IN_GPR')
	 __(pop %save3)
        __endif
	__(pop %arg_z)
	__(pop %arg_y)
	__(pop %arg_x)
	__(pop %temp2)
	__(pop %temp1)
	__(check_pending_interrupt(%temp0))
	__(pop %temp0)
        __(leave)
	__(ret)
_endsubp(syscall)		

/* We need to reserve a frame here if (a) nothing else was already pushed and (b) */
/*   we push something (e.g., more than 3 args in the lexpr) 	  */
_spentry(spread_lexprz)
	new_local_labels()
	__(movq (%arg_z),%imm0)
	__(testl %nargs,%nargs) /* anything pushed by caller ? */
        __(leaq node_size(%arg_z,%imm0),%imm1)
        __(jne 0f)              /* yes, caller has already created frame. */
        __(cmpw $(nargregs*node_size),%imm0_w) /* will we push anything ? */
        __(jbe 0f)
        __(push $reserved_frame_marker)
        __(push $reserved_frame_marker)
0:      __(addw %imm0_w,%nargs_w)
        __(cmpw $(nargregs*node_size),%imm0_w)
        __(jae 9f)
        __(cmpw $(2*node_size),%imm0_w)
        __(je 2f)
        __(testw %imm0_w,%imm0_w)
        __(jne 1f)
        /* lexpr count was 0; vpop the args that */
        /* were pushed by the caller */
        __(testl %nargs,%nargs)
        __(je local_label(all_args_popped))
        __(pop %arg_z)
local_label(maybe_pop_yx):              
        __(cmpl $(1*node_size),%nargs)
        __(je local_label(all_args_popped))
        __(pop %arg_y)
        __(cmpl $(2*node_size),%nargs)
        __(je local_label(all_args_popped))
local_label(pop_arg_x):         
        __(pop %arg_x)
local_label(all_args_popped):   
        /* If all args fit in registers but some were pushed */
        /* by the caller, discard the reserved frame that the caller */
        /* pushed.         */
        __(cmpw %imm0_w,%nargs_w)
        __(je local_label(go))
        __(cmpl $(nargregs*node_size),%nargs)
        __(ja local_label(go))
        __(addq $(2*node_size),%rsp)
local_label(go):        
        __(jmp *%ra0)        
	/* vpush args from the lexpr until we have only */
	/* three left, then assign them to arg_x, arg_y, */
	/* and arg_z. */ 
8:      __(cmpw $(4*node_size),%imm0_w)
        __(lea -1*node_size(%imm0),%imm0)
        __(push -node_size(%imm1))
        __(lea -1*node_size(%imm1),%imm1)
9:      __(jne 8b)
        __(movq -node_size*1(%imm1),%arg_x)
        __(movq -node_size*2(%imm1),%arg_y)
        __(movq -node_size*3(%imm1),%arg_z)
        __(jmp *%ra0)

	/* lexpr count is two: set arg_y, arg_z from the */
	/* lexpr, maybe vpop arg_x */
2:      __(cmpl $(2*node_size),%nargs)
        __(movq -node_size*1(%imm1),%arg_y)
        __(movq -node_size*2(%imm1),%arg_z)
        __(jne local_label(pop_arg_x))
        __(jmp *%ra0)
	/* lexpr count is one: set arg_z from the lexpr, */
	/* maybe vpop arg_y, arg_x  */
1:      __(movq -node_size*1(%imm1),%arg_z)
        __(jmp local_label(maybe_pop_yx))
_endsubp(spread_lexprz)
	



/* Callback index in %r11 */
_spentry(callback)
	__(push %rbp)
	__(movq %rsp,%rbp)
	/* C scalar args   */
	__(push %carg0)	/* -8(%rbp)   */
	__(push %carg1)
	__(push %carg2)
	__(push %carg3)
	__ifndef(`WINDOWS')
	__(push %carg4)
	__(push %carg5)
	__endif
	/* FP arg regs   */
	__ifdef(`WINDOWS')
	__(subq $4*8,%rsp)
	__(movq %xmm0,3*8(%rsp))	/* -40(%rbp) */
	__(movq %xmm1,2*8(%rsp))
	__(movq %xmm2,1*8(%rsp))
	__(movq %xmm3,0*8(%rsp))
	__else
	__(subq $8*8,%rsp)
	__(movq %xmm0,7*8(%rsp))	/* -56(%rbp) */
	__(movq %xmm1,6*8(%rsp))
	__(movq %xmm2,5*8(%rsp))
	__(movq %xmm3,4*8(%rsp))
	__(movq %xmm4,3*8(%rsp))
	__(movq %xmm5,2*8(%rsp))
	__(movq %xmm6,1*8(%rsp))
	__(movq %xmm7,0*8(%rsp))
	__endif
	__ifndef(`WINDOWS')
	__endif
        /* Save caller's mxcsr */
        __(subq $16,%rsp)
        __(stmxcsr (%rsp))
        __(andb $~mxcsr_all_exceptions,(%rsp))
	/* C NVRs   */
	__(push %csave0)
	__(push %csave1)
	__(push %csave2)
	__(push %csave3)
	__(push %csave4)
	__ifdef(`WINDOWS')
	__(push %csave5)
	__(push %csave6)
	__endif
	__(push %rbp)
	__(movq %r11,%csave0)
        __ifdef(`HAVE_TLS')
	 /* TCR initialized for lisp ?   */
	 __ifndef(`TCR_IN_GPR') /* FIXME */
	 __(movq %fs:current_tcr@TPOFF,%rax)
	 __(testq %rax,%rax)
	 __(jne 1f)
	 __endif
        __endif
	__(ref_global(get_tcr,%rax))
	__(movq $1,%carg0)
	__ifdef(`WINDOWS')
	__(sub $0x20, %rsp)
	__endif
	__(call *%rax)
	__ifdef(`WINDOWS')
	__(add $0x20, %rsp)
        __endif
        __ifdef(`TCR_IN_GPR')
	__(movq %rax, %rcontext_reg)
	__endif	
1:	/* Align foreign stack for lisp   */
        __(pushq rcontext(tcr.save_rbp)) /* mark cstack frame's "owner" */
	__(pushq rcontext(tcr.foreign_sp))
	/* init lisp registers   */
	__(movq %csave0,%rax)
	__(movq %rsp,rcontext(tcr.foreign_sp))
	__ifndef(`TCR_IN_GPR')
	__(clr %save3)
	__endif
	__(clr %save2)
	__(clr %save1)
	__(clr %save0)
	__(clr %arg_z)
	__(clr %arg_y)
	__(clr %arg_x)
	__(clr %temp2)
	__(clr %temp1)
	__(clr %temp0)
	__(clr %fn)
	__(pxor %fpzero,%fpzero)
	__(movq rcontext(tcr.save_vsp),%rsp)
	__(box_fixnum(%rax,%arg_y))
	__(movq %rbp,%arg_z)
        __(movq rcontext(tcr.save_rbp),%rbp)
	__(movq $TCR_STATE_LISP,rcontext(tcr.valence))
        __(movq (%rsp),%save0)
        __(movq 8(%rsp),%save1)
        __(movq 16(%rsp),%save2)
        __ifndef(`TCR_IN_GPR')
         __(movq 24(%rsp),%save3)
        __endif
	__(ldmxcsr rcontext(tcr.lisp_mxcsr))
	__(movq $nrs.callbacks,%fname)
	__(lea local_label(back_from_callback)(%rip),%ra0)
	__(set_nargs(2))
        __(push %ra0)
	__(jump_fname())
__(tra(local_label(back_from_callback)))
	__(movq %rsp,rcontext(tcr.save_vsp))
        __(movq %rbp,rcontext(tcr.save_rbp))
        __(movq rcontext(tcr.foreign_sp),%rsp)
	__(stmxcsr rcontext(tcr.lisp_mxcsr))
	__(movq $TCR_STATE_FOREIGN,rcontext(tcr.valence))
	__(emms)
	__(pop rcontext(tcr.foreign_sp))
        __(addq $node_size,%rsp)
        __(ldmxcsr rcontext(tcr.foreign_mxcsr))
	__(pop %rbp)
	__ifdef(`WINDOWS')
	__(pop %csave6)
	__(pop %csave5)
	__endif
	__(pop %csave4)
	__(pop %csave3)
	__(pop %csave2)
	__(pop %csave1)
	__(pop %csave0)
        __(ldmxcsr (%rsp))
        __(addq $16,%rsp)
	__(movq -8(%rbp),%rax)
        __(movq -16(%rbp),%rdx)
	__(movq -24(%rbp),%xmm0)
        __(movq -32(%rbp),%xmm1)
	__(leave)
	__(ret)		
_endsubp(callback)

/* arg_x = array, arg_y = i, arg_z = j. Typecheck everything.
   We don't know whether the array is alleged to be simple or
   not, and don't know anythng about the element type.  */
        	
_spentry(aref2)
        __(testb $fixnummask,%arg_y_b)
        __(jne 0f)
        
        __(testb $fixnummask,%arg_z_b)
        __(jne 1f)
        __(extract_typecode(%arg_x,%imm0))
        __(cmpb $subtag_arrayH,%imm0_b)
        __(jne 2f)
        __(cmpq $2<<fixnumshift,arrayH.rank(%arg_x))
        __(jne 2f)
        __(cmpq arrayH.dim0(%arg_x),%arg_y)
        __(jae 3f)
        __(movq arrayH.dim0+node_size(%arg_x),%imm0)
        __(cmpq %imm0,%arg_z)
        __(jae 4f)
        __(unbox_fixnum(%imm0,%imm0))
        __(mulq %arg_y)         /* imm0 <- imm0 * arg_y */
        __(addq %imm0,%arg_z)
        __(movq %arg_x,%arg_y)
6:      __(addq arrayH.displacement(%arg_y),%arg_z)
        __(movq arrayH.data_vector(%arg_y),%arg_y)
        __(extract_subtag(%arg_y,%imm1))
        __(cmpb $subtag_vectorH,%imm1_b) 
        __(ja C(misc_ref_common))
        __(jmp 6b)
0:      __(uuo_error_reg_not_fixnum(Rarg_y))
1:      __(uuo_error_reg_not_fixnum(Rarg_z))
2:      __(uuo_error_reg_not_type(Rarg_x,error_object_not_array_2d))
3:      __(uuo_error_array_bounds(Rarg_y,Rarg_x))
4:      __(uuo_error_array_bounds(Rarg_z,Rarg_x))
        
_endsubp(aref2)

/* %temp0 = array, %arg_x = i,%arg_y = j, %arg_z = k */
_spentry(aref3)
        __(testb $fixnummask,%arg_x_b)
        __(jne 0f)
        __(testb $fixnummask,%arg_y_b)
        __(jne 1f)
        __(testb $fixnummask,%arg_z_b)
        __(jne 2f)
        __(extract_typecode(%temp0,%imm0))
        __(cmpb $subtag_arrayH,%imm0_b)
        __(jne 3f)
        __(cmpq $3<<fixnumshift,arrayH.rank(%temp0))
        __(jne 3f)
        __(cmpq arrayH.dim0(%temp0),%arg_x)
        __(jae 5f)
        __(movq arrayH.dim0+node_size(%temp0),%imm0)
        __(cmpq %imm0,%arg_y)
        __(jae 6f)
        __(unbox_fixnum(%imm0,%imm0))
        __(movq arrayH.dim0+(node_size*2)(%temp0),%imm1)
        __(cmpq %imm1,%arg_z)
        __(jae 7f)
        __(unbox_fixnum(%imm1,%imm1))
        __(imulq %imm1,%arg_y)
        __(mulq %imm1)
        __(imulq %imm0,%arg_x)
        __(addq %arg_x,%arg_z)
        __(addq %arg_y,%arg_z)
        __(movq %temp0,%arg_y)
8:      __(addq arrayH.displacement(%arg_y),%arg_z)
        __(movq arrayH.data_vector(%arg_y),%arg_y)
        __(extract_subtag(%arg_y,%imm1))
        __(cmpb $subtag_vectorH,%imm1_b)
        __(ja C(misc_ref_common))
        __(jmp 8b)
0:      __(uuo_error_reg_not_fixnum(Rarg_x))
1:      __(uuo_error_reg_not_fixnum(Rarg_y))	
2:      __(uuo_error_reg_not_fixnum(Rarg_z))
3:      __(uuo_error_reg_not_type(Rtemp0,error_object_not_array_3d))
5:      __(uuo_error_array_bounds(Rarg_x,Rtemp0))
6:      __(uuo_error_array_bounds(Rarg_y,Rtemp0))
7:      __(uuo_error_array_bounds(Rarg_z,Rtemp0))
        
_endsubp(aref3)
        
/* As with aref2, but temp0 = array, arg_x = i, arg_y = j, arg_z = new_value */
_spentry(aset2)
        __(testb $fixnummask,%arg_x_b)
        __(jne 0f)
        __(testb $fixnummask,%arg_y_b)
        __(jne 1f)
        __(extract_typecode(%temp0,%imm0))
        __(cmpb $subtag_arrayH,%imm0_b)
        __(jne 2f)
        __(cmpq $2<<fixnumshift,arrayH.rank(%temp0))
        __(jne 2f)
        __(cmpq arrayH.dim0(%temp0),%arg_x)
        __(jae 4f)
        __(movq arrayH.dim0+node_size(%temp0),%imm0)
        __(cmpq %imm0,%arg_y)
        __(jae 5f)
        __(unbox_fixnum(%imm0,%imm0))
        __(mulq %arg_x)         /* imm0 <- imm0 * arg_x */
        __(addq %imm0,%arg_y)
        __(movq %temp0,%arg_x)
6:      __(addq arrayH.displacement(%arg_x),%arg_y)
        __(movq arrayH.data_vector(%arg_x),%arg_x)
        __(extract_subtag(%arg_x,%imm1))
        __(cmpb $subtag_vectorH,%imm1_b)
        __(ja C(misc_set_common))
        __(jmp 6b)
0:      __(uuo_error_reg_not_fixnum(Rarg_x))
1:      __(uuo_error_reg_not_fixnum(Rarg_y))
2:      __(uuo_error_reg_not_type(Rtemp0,error_object_not_array_2d))
4:      __(uuo_error_array_bounds(Rarg_x,Rtemp0))
5:      __(uuo_error_array_bounds(Rarg_y,Rtemp0))
_endsubp(aset2)

/* %temp1 = array, %temp0 = i, %arg_x = j, %arg_y = k, %arg_y = newval. */

_spentry(aset3)
        __(testb $fixnummask,%temp0_b)
        __(jne 0f)
        __(testb $fixnummask,%arg_x_b)
        __(jne 1f)
        __(testb $fixnummask,%arg_y_b)
        __(jne 2f)
        __(extract_typecode(%temp1,%imm0))
        __(cmpb $subtag_arrayH,%imm0_b)
        __(jne 3f)
        __(cmpq $3<<fixnumshift,arrayH.rank(%temp1))
        __(jne 3f)
        __(cmpq arrayH.dim0(%temp1),%temp0)
        __(jae 5f)
        __(movq arrayH.dim0+node_size(%temp1),%imm0)
        __(cmpq %imm0,%arg_x)
        __(jae 6f)
        __(unbox_fixnum(%imm0,%imm0))
        __(movq arrayH.dim0+(node_size*2)(%temp1),%imm1)
        __(cmpq %imm1,%arg_y)
        __(jae 7f)
        __(unbox_fixnum(%imm1,%imm1))
        __(imulq %imm1,%arg_x)
        __(mulq %imm1)
        __(imulq %imm0,%temp0)
        __(addq %temp0,%arg_y)
        __(addq %arg_x,%arg_y)
        __(movq %temp1,%arg_x)
8:      __(addq arrayH.displacement(%arg_x),%arg_y)
        __(movq arrayH.data_vector(%arg_x),%arg_x)
        __(extract_subtag(%arg_x,%imm1))
        __(cmpb $subtag_vectorH,%imm1_b)
        __(ja C(misc_set_common))
        __(jmp 8b)
	
0:      __(uuo_error_reg_not_fixnum(Rtemp0))
1:      __(uuo_error_reg_not_fixnum(Rarg_x))
2:      __(uuo_error_reg_not_fixnum(Rarg_y))
3:      __(uuo_error_reg_not_type(Rtemp1,error_object_not_array_3d))
5:      __(uuo_error_array_bounds(Rtemp0,Rtemp1))
6:      __(uuo_error_array_bounds(Rarg_x,Rtemp1))
6:      __(uuo_error_array_bounds(Rarg_x,Rtemp1))
7:      __(uuo_error_array_bounds(Rarg_y,Rtemp1))
	
_endsubp(aset3)

        


/* Prepend all but the first five (4 words of code, inner fn) and last   */
/* (lfbits) elements of %fn to the "arglist".   */
	
_spentry(call_closure)
        new_local_labels()
        __(subq $fulltag_function-fulltag_misc,%fn)
        __(vector_length(%fn,%imm0))
	
        __(subq $6<<fixnumshift,%imm0)  /* imm0 = inherited arg count   */
        __(lea (%nargs_q,%imm0),%imm1)
        __(cmpl $nargregs<<fixnumshift,%imm1_l)
        __(jna local_label(regs_only))
        __(pop %ra0)
        __(cmpl $nargregs<<fixnumshift,%nargs)
        __(jna local_label(no_insert))
	
/* Some arguments have already been pushed.  Push imm0's worth   */
/* of NILs, copy those arguments that have already been vpushed from   */
/* the old TOS to the new, then insert all of the inerited args   */
/* and go to the function.  */
	
        __(movq %imm0,%imm1)
local_label(push_nil_loop):     
        __(push $nil_value)
        __(sub $fixnumone,%imm1)
        __(jne local_label(push_nil_loop))
	
/* Need to use arg regs as temporaries here.    */
        __(movq %rsp,%temp1)
        __(push %arg_z)
        __(push %arg_y)
        __(push %arg_x)
        __(lea 3*node_size(%rsp,%imm0),%arg_x)
        __(lea -nargregs<<fixnumshift(%nargs_q),%arg_y)
local_label(copy_already_loop): 
        __(movq (%arg_x),%arg_z)
        __(addq $fixnumone,%arg_x)
        __(movq %arg_z,(%temp1))
        __(addq $fixnumone,%temp1)
        __(subq $fixnumone,%arg_y)
        __(jne local_label(copy_already_loop))
	
        __(movl $5<<fixnumshift,%imm1_l) /* skip code, new fn   */
local_label(insert_loop):               
        __(movq misc_data_offset(%fn,%imm1),%arg_z)
        __(addq $node_size,%imm1)
        __(addl $fixnum_one,%nargs)
        __(subq $node_size,%arg_x)
        __(movq %arg_z,(%arg_x))
        __(subq $fixnum_one,%imm0)
        __(jne local_label(insert_loop))

        /* Recover the argument registers, pushed earlier   */
        __(pop %arg_x)
        __(pop %arg_y)
        __(pop %arg_z)
        __(jmp local_label(go))

/* Here if nothing was pushed by the caller.  If we're  */
/* going to push anything, we have to reserve a stack  */
/* frame first. (We'll need to push something if the  */
/* sum of %nargs and %imm0 is greater than nargregs)   */
	
local_label(no_insert):
        __(lea (%nargs_q,%imm0),%imm1)
        __(cmpq $nargregs<<fixnumshift,%imm1)
        __(jna local_label(no_insert_no_frame))
        /* Reserve space for a stack frame   */
        __(push $reserved_frame_marker)
        __(push $reserved_frame_marker)
local_label(no_insert_no_frame):        
	/* nargregs or fewer args were already vpushed.   */
	/* if exactly nargregs, vpush remaining inherited vars.   */
        __(cmpl $nargregs<<fixnumshift,%nargs)
        __(movl $5<<fixnumshift,%imm1_l) /* skip code, new fn   */
        __(leaq 5<<fixnumshift(%imm0),%temp1)
        __(jnz local_label(set_regs))
local_label(vpush_remaining):  
        __(push misc_data_offset(%fn,%imm1))
        __(addq $node_size,%imm1)
        __(addl $fixnumone,%nargs)
        __(subq $node_size,%imm0)
        __(jnz local_label(vpush_remaining))
        __(jmp local_label(go))
local_label(set_regs):
	/* if nargs was > 1 (and we know that it was < 3), it must have   */
	/* been 2.  Set arg_x, then vpush the remaining args.   */
        __(cmpl $fixnumone,%nargs)
        __(jle local_label(set_y_z))
local_label(set_arg_x): 
        __(subq $node_size,%temp1)
        __(movq misc_data_offset(%fn,%temp1),%arg_x)
        __(addl $fixnumone,%nargs)
        __(subq $fixnumone,%imm0)
        __(jne local_label(vpush_remaining))
        __(jmp local_label(go))
	/* Maybe set arg_y or arg_z, preceding args   */
local_label(set_y_z):
        __(jne local_label(set_arg_z))
	/* Set arg_y, maybe arg_x, preceding args   */
local_label(set_arg_y): 
        __(subq $node_size,%temp1)
        __(movq misc_data_offset(%fn,%temp1),%arg_y)
        __(addl $fixnumone,%nargs)
        __(subq $fixnum_one,%imm0)
        __(jnz local_label(set_arg_x))
        __(jmp local_label(go))
local_label(set_arg_z): 
        __(subq $node_size,%temp1)
        __(movq misc_data_offset(%fn,%temp1),%arg_z)
        __(addl $fixnumone,%nargs)
        __(subq $fixnum_one,%imm0)
        __(jne local_label(set_arg_y))
local_label(go):        
        __(movq misc_data_offset+(4*node_size)(%fn),%fn)
        __(push %ra0)
        __(jmp *%fn)
local_label(regs_only):
        __(leaq 5<<fixnumshift(%imm0),%temp1)
        __(testl %nargs,%nargs)
        __(jne local_label(some_args))
        __(cmpw $node_size,%imm0)
        __(movq misc_data_offset-node_size(%fn,%temp1),%arg_z)
        __(je local_label(rgo))
        __(cmpw $2*node_size,%imm0)
        __(movq misc_data_offset-(node_size*2)(%fn,%temp1),%arg_y)
        __(je local_label(rgo))
        __(movq misc_data_offset-(node_size*3)(%fn,%temp1),%arg_x)
local_label(rgo):
        __(addw %imm0_w,%nargs_w)
        __(jmp *misc_data_offset+(4*node_size)(%fn))
local_label(some_args):         
        __(cmpl $2*node_size,%nargs)
        __(jz local_label(rtwo))
        /* One arg was passed, could be one or two inherited args */
        __(cmpw $node_size,%imm0)
        __(movq misc_data_offset-node_size(%fn,%temp1),%arg_y)
        __(je local_label(rgo))
        __(movq misc_data_offset-(node_size*2)(%fn,%temp1),%arg_x)
        __(jmp local_label(rgo))
local_label(rtwo):     
        __(movq misc_data_offset-node_size(%fn,%temp1),%arg_x)
        __(jmp local_label(rgo))
_endsubp(call_closure)
                                        
        
_spentry(poweropen_callbackX)
_endsubp(poweropen_callbackX)
	
	
_spentry(poweropen_ffcallX)
_endsubp(poweropen_ffcallX)
        	
_spentry(poweropen_syscall)
_endsubp(poweropen_syscall)

_spentry(eabi_ff_call)
_endsubp(eabi_ff_call)

_spentry(eabi_callback)
_endsubp(eabi_callback)


/* Unused, and often not used on PPC either  */
_spentry(callbuiltin)
	__(hlt)
_endsubp(callbuiltin)

_spentry(callbuiltin0)
	__(hlt)
_endsubp(callbuiltin0)

_spentry(callbuiltin1)
	__(hlt)
_endsubp(callbuiltin1)

_spentry(callbuiltin2)
	__(hlt)
_endsubp(callbuiltin2)

_spentry(callbuiltin3)
	__(hlt)
_endsubp(callbuiltin3)
	
_spentry(restorefullcontext)
	__(hlt)
_endsubp(restorefullcontext)

_spentry(savecontextvsp)
	__(hlt)
_endsubp(savecontextvsp)

_spentry(savecontext0)
	__(hlt)
_endsubp(savecontext0)

_spentry(restorecontext)
	__(hlt)
_endsubp(restorecontext)

_spentry(stkconsyz)
	__(hlt)
_endsubp(stkconsyz)

_spentry(stkvcell0)
	__(hlt)
_endsubp(stkvcell0)

_spentry(stkvcellvsp)
	__(hlt)
_endsubp(stkvcellvsp)

_spentry(breakpoint)
        __(hlt)
_endsubp(breakpoint)


        __ifdef(`DARWIN')
        .if 1
	.globl  C(lisp_objc_personality)
C(lisp_objc_personality):
	jmp *lisp_global(objc_2_personality)
	
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$12,LECIE1-LSCIE1
	.long L$set$12	/* Length of Common Information Entry */
LSCIE1:
	.long	0x0	/* CIE Identifier Tag */
	.byte	0x1	/* CIE Version */
	.ascii "zPLR\0"	/* CIE Augmentation */
	.byte	0x1	/* uleb128 0x1; CIE Code Alignment Factor */
	.byte	0x78	/* sleb128 -8; CIE Data Alignment Factor */
	.byte	0x10	/* CIE RA Column */
	.byte	0x7
	.byte	0x9b
	.long	_lisp_objc_personality+4@GOTPCREL
	.byte	0x10	/* LSDA Encoding (pcrel) */
	.byte	0x10	/* FDE Encoding (pcrel) */
	.byte	0xc	/* DW_CFA_def_cfa */
	.byte	0x7	/* uleb128 0x7 */
	.byte	0x8	/* uleb128 0x8 */
	.byte	0x90	/* DW_CFA_offset, column 0x10 */
	.byte	0x1	/* uleb128 0x1 */
	.align 3
LECIE1:
        .globl _SPffcall.eh
_SPffcall.eh:
        .long LEFDEffcall-LSFDEffcall
LSFDEffcall:      
        .long LSFDEffcall-EH_frame1 /* FDE CIE offset */
        .quad Lffcall-. /* FDE Initial Location */
        .quad Lffcall_end-Lffcall /* FDE address range */
        .byte 8 /* uleb128 0x8; Augmentation size */
        .quad LLSDA1-.           /* Language Specific Data Area */
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long Lffcall_setup-Lffcall
	.byte	0xe	/* DW_CFA_def_cfa_offset */
	.byte	0x10	/* uleb128 0x10 */
	.byte	0x86	/* DW_CFA_offset, column 0x6 */
	.byte	0x2	/* uleb128 0x2 */
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long Lffcall_setup_end-Lffcall_setup
	.byte	0xd	/* DW_CFA_def_cfa_register */
	.byte	0x6	/* uleb128 0x6 */
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long Lffcall_call_end-Lffcall_call
	.byte	0x83	/* DW_CFA_offset, column 0x3 */
	.byte	0x3	/* uleb128 0x3 */
	.align 3
LEFDEffcall:
        .globl _SPffcall_return_registers.eh
_SPffcall_return_registers.eh:
        .long LEFDEffcall_return_registers-LSFDEffcall_return_registers
LSFDEffcall_return_registers:      
        .long LSFDEffcall_return_registers-EH_frame1 /* FDE CIE offset */
        .quad Lffcall_return_registers-. /* FDE Initial Location */
        .quad Lffcall_return_registers_end-Lffcall_return_registers /* FDE address range */
        .byte 8 /* uleb128 0x8; Augmentation size */
        .quad LLSDA2-.           /* Language Specific Data Area */
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long Lffcall_return_registers_setup-Lffcall_return_registers
	.byte	0xe	/* DW_CFA_def_cfa_offset */
	.byte	0x10	/* uleb128 0x10 */
	.byte	0x86	/* DW_CFA_offset, column 0x6 */
	.byte	0x2	/* uleb128 0x2 */
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long Lffcall_return_registers_setup_end-Lffcall_return_registers_setup
	.byte	0xd	/* DW_CFA_def_cfa_register */
	.byte	0x6	/* uleb128 0x6 */
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long Lffcall_return_registers_call_end-Lffcall_return_registers_call
	.byte	0x83	/* DW_CFA_offset, column 0x3 */
	.byte	0x3	/* uleb128 0x3 */
	.align 3
LEFDEffcall_return_registers:
        .text
        .endif
        __endif
        
_spentry(unused_5)
        __(hlt)
Xspentry_end:           
_endsubp(unused_5)
        
        .data
        .globl C(spentry_start)
        .globl C(spentry_end)
C(spentry_start):       .quad Xspentry_start
C(spentry_end):         .quad Xspentry_end
