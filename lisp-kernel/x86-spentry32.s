	include(lisp.s)
	_beginfile

	.align 2
define(`_spentry',`ifdef(`__func_name',`_endfn',`')
        .p2align 3
        _exportfn(_SP$1)
')

define(`_endsubp',`
        _endfn(_SP$1)
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

/* %arg_z has overflowed by one bit.  Make a bignum with 1 (32-bit) digit. */
_spentry(fix_overflow)
C(fix_one_bit_overflow):
        __(movl $one_digit_bignum_header,%imm0)
	__(movd %imm0,%mm0)
        __(Misc_Alloc_Fixed(`',aligned_bignum_size(1)))
        __(unbox_fixnum(%arg_z,%imm0))
	__(xor $0xc0000000,%imm0)
        __(mov %temp0,%arg_z)
        __(movl %imm0,misc_data_offset(%arg_z))
        __(ret)
_endsubp(fix_overflow)

/* %arg_y = vector, %arg_z = unscaled-idx */
_spentry(misc_ref)
	__(mov %arg_y,%imm0)
	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne 0f)
	__(testb $fixnummask,%arg_z_b)
	__(jne 1f)
	__(movl misc_header_offset(%arg_y),%imm0)
	__(xorb %imm0_b,%imm0_b)
	__(shrl $num_subtag_bits-fixnumshift,%imm0)
	__(cmpl %imm0,%arg_z)
	__(jae 2f)
	__(movb misc_subtag_offset(%arg_y),%imm0_b)
	__(jmp C(misc_ref_common))

0:	__(uuo_error_reg_not_tag(Rarg_y,tag_misc))
1:	__(uuo_error_reg_not_fixnum(Rarg_z))
2:	__(uuo_error_vector_bounds(Rarg_z,Rarg_y))
_endsubp(misc_ref)

/* %imm0_b = subtag, %arg_y = vector, %arg_z = index. */
/* Bounds/type-checking done in caller. */
_startfn(C(misc_ref_common))
	__(movzbl %imm0_b,%imm0)
	__(leal local_label(misc_ref_jmp)(,%imm0,4),%imm0)
	__(jmp *(%imm0))
	.p2align 2
local_label(misc_ref_jmp):
	/* 00-0f */
        .long local_label(misc_ref_invalid) /* 00 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 01 cons  */
        .long local_label(misc_ref_invalid) /* 02 nodeheader  */
        .long local_label(misc_ref_invalid) /* 03 imm  */
        .long local_label(misc_ref_invalid) /* 04 odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 05 tra  */
        .long local_label(misc_ref_invalid) /* 06 misc  */
        .long local_label(misc_ref_u32) /* 07 bignum  */
        .long local_label(misc_ref_invalid) /* 08 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 09 cons  */
        .long local_label(misc_ref_node) /* 0a ratio  */
        .long local_label(misc_ref_invalid) /* 0b imm  */
        .long local_label(misc_ref_invalid) /* 0c odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 0d tra  */
        .long local_label(misc_ref_invalid) /* 0e misc  */
        .long local_label(misc_ref_u32) /* 0f single_float  */
        /* 10-1f  */
        .long local_label(misc_ref_invalid) /* 10 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 11 cons  */
        .long local_label(misc_ref_invalid) /* 12 nodeheader  */
        .long local_label(misc_ref_invalid) /* 13 imm  */
        .long local_label(misc_ref_invalid) /* 14 odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 15 tra  */
        .long local_label(misc_ref_invalid) /* 16 misc  */
        .long local_label(misc_ref_u32) /* 17 double_float  */
        .long local_label(misc_ref_invalid) /* 18 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 19 cons  */
        .long local_label(misc_ref_node) /* 1a complex  */
        .long local_label(misc_ref_invalid) /* 1b imm  */
        .long local_label(misc_ref_invalid) /* 1c odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 1d tra  */
        .long local_label(misc_ref_invalid) /* 1e misc  */
        .long local_label(misc_ref_u32) /* 1f macptr  */
        /* 20-2f  */
        .long local_label(misc_ref_invalid) /* 20 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 21 cons  */
        .long local_label(misc_ref_node) /* 22 catch_frame  */
        .long local_label(misc_ref_invalid) /* 23 imm  */
        .long local_label(misc_ref_invalid) /* 24 odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 25 tra  */
        .long local_label(misc_ref_invalid) /* 26 misc  */
        .long local_label(misc_ref_u32) /* 27 dead_macptr  */
        .long local_label(misc_ref_invalid) /* 28 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 29 cons  */
        .long local_label(misc_ref_function) /* 2a function  */
        .long local_label(misc_ref_invalid) /* 2b imm  */
        .long local_label(misc_ref_invalid) /* 2c odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 2d tra  */
        .long local_label(misc_ref_invalid) /* 2e misc  */
        .long local_label(misc_ref_invalid) /* 2f immheader  */
        /* 30-3f  */
        .long local_label(misc_ref_invalid) /* 30 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 31 cons  */
        .long local_label(misc_ref_node) /* 32 basic_stream  */
        .long local_label(misc_ref_invalid) /* 33 imm  */
        .long local_label(misc_ref_invalid) /* 34 odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 35 tra  */
        .long local_label(misc_ref_invalid) /* 36 misc  */
        .long local_label(misc_ref_invalid) /* 37 immheader  */
        .long local_label(misc_ref_invalid) /* 38 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 39 cons  */
        .long local_label(misc_ref_node) /* 3a symbol  */
        .long local_label(misc_ref_invalid) /* 3b imm  */
        .long local_label(misc_ref_invalid) /* 3c odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 3d tra  */
        .long local_label(misc_ref_invalid) /* 3e misc  */
        .long local_label(misc_ref_u32) /* 3f xcode_vector  */
        /* 40-4f  */
        .long local_label(misc_ref_invalid) /* 40 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 41 cons  */
        .long local_label(misc_ref_node) /* 42 lock  */
        .long local_label(misc_ref_invalid) /* 43 imm  */
        .long local_label(misc_ref_invalid) /* 44 odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 45 tra  */
        .long local_label(misc_ref_invalid) /* 46 misc  */
        .long local_label(misc_ref_invalid) /* 47 immheader  */
        .long local_label(misc_ref_invalid) /* 48 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 49 cons  */
        .long local_label(misc_ref_node) /* 4a hash_vector  */
        .long local_label(misc_ref_invalid) /* 4b imm  */
        .long local_label(misc_ref_invalid) /* 4c odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 4d tra  */
        .long local_label(misc_ref_invalid) /* 4e misc  */
        .long local_label(misc_ref_invalid) /* 4f immheader  */
        /* 50-5f  */
        .long local_label(misc_ref_invalid) /* 50 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 51 cons  */
        .long local_label(misc_ref_node) /* 52 pool  */
        .long local_label(misc_ref_invalid) /* 53 imm  */
        .long local_label(misc_ref_invalid) /* 54 odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 55 tra  */
        .long local_label(misc_ref_invalid) /* 56 misc  */
        .long local_label(misc_ref_invalid) /* 57 immheader  */
        .long local_label(misc_ref_invalid) /* 58 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 59 cons  */
        .long local_label(misc_ref_node) /* 5a weak  */
        .long local_label(misc_ref_invalid) /* 5b imm  */
        .long local_label(misc_ref_invalid) /* 5c odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 5d tra  */
        .long local_label(misc_ref_invalid) /* 5e misc  */
        .long local_label(misc_ref_invalid) /* 5f immheader  */
        /* 60-6f  */
        .long local_label(misc_ref_invalid) /* 60 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 61 cons  */
        .long local_label(misc_ref_node) /* 62 package  */
        .long local_label(misc_ref_invalid) /* 63 imm  */
        .long local_label(misc_ref_invalid) /* 64 odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 65 tra  */
        .long local_label(misc_ref_invalid) /* 66 misc  */
        .long local_label(misc_ref_invalid) /* 67 immheader  */
        .long local_label(misc_ref_invalid) /* 68 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 69 cons  */
        .long local_label(misc_ref_node) /* 6a slot_vector  */
        .long local_label(misc_ref_invalid) /* 6b imm  */
        .long local_label(misc_ref_invalid) /* 6c odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 6d tra  */
        .long local_label(misc_ref_invalid) /* 6e misc  */
        .long local_label(misc_ref_invalid) /* 6f immheader  */
        /* 70-7f  */
        .long local_label(misc_ref_invalid) /* 70 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 71 cons  */
        .long local_label(misc_ref_node) /* 72 instance  */
        .long local_label(misc_ref_invalid) /* 73 imm  */
        .long local_label(misc_ref_invalid) /* 74 odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 75 tra  */
        .long local_label(misc_ref_invalid) /* 76 misc  */
        .long local_label(misc_ref_invalid) /* 77 immheader  */
        .long local_label(misc_ref_invalid) /* 78 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 79 cons  */
        .long local_label(misc_ref_node) /* 7a struct  */
        .long local_label(misc_ref_invalid) /* 7b imm  */
        .long local_label(misc_ref_invalid) /* 7c odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 7d tra  */
        .long local_label(misc_ref_invalid) /* 7e misc  */
        .long local_label(misc_ref_invalid) /* 7f immheader  */
        /* 80-8f  */
        .long local_label(misc_ref_invalid) /* 80 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 81 cons  */
        .long local_label(misc_ref_node) /* 82 istruct  */
        .long local_label(misc_ref_invalid) /* 83 imm  */
        .long local_label(misc_ref_invalid) /* 84 odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 85 tra  */
        .long local_label(misc_ref_invalid) /* 86 misc  */
        .long local_label(misc_ref_invalid) /* 87 immheader  */
        .long local_label(misc_ref_invalid) /* 88 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 89 cons  */
        .long local_label(misc_ref_node) /* 8a value_cell  */
        .long local_label(misc_ref_invalid) /* 8b imm  */
        .long local_label(misc_ref_invalid) /* 8c odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 8d tra  */
        .long local_label(misc_ref_invalid) /* 8e misc  */
        .long local_label(misc_ref_invalid) /* 8f immheader  */
        /* 90-9f  */
        .long local_label(misc_ref_invalid) /* 90 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 91 cons  */
        .long local_label(misc_ref_node) /* 92 xfunction  */
        .long local_label(misc_ref_invalid) /* 93 imm  */
        .long local_label(misc_ref_invalid) /* 94 odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 95 tra  */
        .long local_label(misc_ref_invalid) /* 96 misc  */
        .long local_label(misc_ref_invalid) /* 97 immheader  */
        .long local_label(misc_ref_invalid) /* 98 even_fixnum  */
        .long local_label(misc_ref_invalid) /* 99 cons  */
        .long local_label(misc_ref_node) /* 9a arrayH  */
        .long local_label(misc_ref_invalid) /* 9b imm  */
        .long local_label(misc_ref_invalid) /* 9c odd_fixnum  */
        .long local_label(misc_ref_invalid) /* 9d tra  */
        .long local_label(misc_ref_invalid) /* 9e misc  */
        .long local_label(misc_ref_invalid) /* 9f immheader  */
        /* a0-af  */
        .long local_label(misc_ref_invalid) /* a0 even_fixnum  */
        .long local_label(misc_ref_invalid) /* a1 cons  */
        .long local_label(misc_ref_node) /* a2 vectorH  */
        .long local_label(misc_ref_invalid) /* a3 imm  */
        .long local_label(misc_ref_invalid) /* a4 odd_fixnum  */
        .long local_label(misc_ref_invalid) /* a5 tra  */
        .long local_label(misc_ref_invalid) /* a6 misc  */
        .long local_label(misc_ref_single_float_vector) /* a7 sf_vector  */
        .long local_label(misc_ref_invalid) /* a8 even_fixnum  */
        .long local_label(misc_ref_invalid) /* a9 cons  */
        .long local_label(misc_ref_node) /* aa simple_vector  */
        .long local_label(misc_ref_invalid) /* ab imm  */
        .long local_label(misc_ref_invalid) /* ac odd_fixnum  */
        .long local_label(misc_ref_invalid) /* ad tra  */
        .long local_label(misc_ref_invalid) /* ae misc  */
        .long local_label(misc_ref_u32) /* af u32  */
        /* b0-bf  */
        .long local_label(misc_ref_invalid) /* b0 even_fixnum  */
        .long local_label(misc_ref_invalid) /* b1 cons  */
        .long local_label(misc_ref_invalid) /* b2 nodeheader  */
        .long local_label(misc_ref_invalid) /* b3 imm  */
        .long local_label(misc_ref_invalid) /* b4 odd_fixnum  */
        .long local_label(misc_ref_invalid) /* b5 tra  */
        .long local_label(misc_ref_invalid) /* b6 misc  */
        .long local_label(misc_ref_s32) /* b7 s32  */
        .long local_label(misc_ref_invalid) /* b8 even_fixnum  */
        .long local_label(misc_ref_invalid) /* b9 cons  */
        .long local_label(misc_ref_invalid) /* ba nodeheader  */
        .long local_label(misc_ref_invalid) /* bb imm  */
        .long local_label(misc_ref_invalid) /* bc odd_fixnum  */
        .long local_label(misc_ref_invalid) /* bd tra  */
        .long local_label(misc_ref_invalid) /* be misc  */
        .long local_label(misc_ref_fixnum_vector) /* bf fixnum_vector  */
        /* c0-cf  */
        .long local_label(misc_ref_invalid) /* c0 even_fixnum  */
        .long local_label(misc_ref_invalid) /* c1 cons  */
        .long local_label(misc_ref_invalid) /* c2 nodeheader  */
        .long local_label(misc_ref_invalid) /* c3 imm  */
        .long local_label(misc_ref_invalid) /* c4 odd_fixnum  */
        .long local_label(misc_ref_invalid) /* c5 tra  */
        .long local_label(misc_ref_invalid) /* c6 misc  */
        .long local_label(misc_ref_string) /* c7 simple_base_string  */
        .long local_label(misc_ref_invalid) /* c8 even_fixnum  */
        .long local_label(misc_ref_invalid) /* c9 cons  */
        .long local_label(misc_ref_invalid) /* ca nodeheader  */
        .long local_label(misc_ref_invalid) /* cb imm  */
        .long local_label(misc_ref_invalid) /* cc odd_fixnum  */
        .long local_label(misc_ref_invalid) /* cd tra  */
        .long local_label(misc_ref_invalid) /* ce misc  */
        .long local_label(misc_ref_u8) /* cf u8  */
        /* d0-df  */
        .long local_label(misc_ref_invalid) /* d0 even_fixnum  */
        .long local_label(misc_ref_invalid) /* d1 cons  */
        .long local_label(misc_ref_invalid) /* d2 nodeheader  */
        .long local_label(misc_ref_invalid) /* d3 imm  */
        .long local_label(misc_ref_invalid) /* d4 odd_fixnum  */
        .long local_label(misc_ref_invalid) /* d5 tra  */
        .long local_label(misc_ref_invalid) /* d6 misc  */
        .long local_label(misc_ref_s8)      /* d7 s8  */
        .long local_label(misc_ref_invalid) /* d8 even_fixnum  */
        .long local_label(misc_ref_invalid) /* d9 cons  */
        .long local_label(misc_ref_invalid) /* da nodeheader  */
        .long local_label(misc_ref_invalid) /* db imm  */
        .long local_label(misc_ref_invalid) /* dc odd_fixnum  */
        .long local_label(misc_ref_invalid) /* dd tra  */
        .long local_label(misc_ref_invalid) /* de misc  */
        .long local_label(misc_ref_invalid) /* df immheader  */
        /* e0-ef  */
        .long local_label(misc_ref_invalid) /* e0 even_fixnum  */
        .long local_label(misc_ref_invalid) /* e1 cons  */
        .long local_label(misc_ref_invalid) /* e2 nodeheader  */
        .long local_label(misc_ref_invalid) /* e3 imm  */
        .long local_label(misc_ref_invalid) /* e4 odd_fixnum  */
        .long local_label(misc_ref_invalid) /* e5 tra  */
        .long local_label(misc_ref_invalid) /* e6 misc  */
        .long local_label(misc_ref_u16) /* e7 u16  */
        .long local_label(misc_ref_invalid) /* e8 even_fixnum  */
        .long local_label(misc_ref_invalid) /* e9 cons  */
        .long local_label(misc_ref_invalid) /* ea nodeheader  */
        .long local_label(misc_ref_invalid) /* eb imm  */
        .long local_label(misc_ref_invalid) /* ec odd_fixnum  */
        .long local_label(misc_ref_invalid) /* ed tra  */
        .long local_label(misc_ref_invalid) /* ee misc  */
        .long local_label(misc_ref_s16) /* ef s16  */
        /* f0-ff  */
        .long local_label(misc_ref_invalid) /* f0 even_fixnum  */
        .long local_label(misc_ref_invalid) /* f1 cons  */
        .long local_label(misc_ref_invalid) /* f2 nodeheader  */
        .long local_label(misc_ref_invalid) /* f3 imm  */
        .long local_label(misc_ref_invalid) /* f4 odd_fixnum  */
        .long local_label(misc_ref_invalid) /* f5 tra  */
        .long local_label(misc_ref_invalid) /* f6 misc  */
        .long local_label(misc_ref_double_float_vector) /* f7 df vector  */
        .long local_label(misc_ref_invalid) /* f8 even_fixnum  */
        .long local_label(misc_ref_invalid) /* f9 cons  */
        .long local_label(misc_ref_invalid) /* fa nodeheader  */
        .long local_label(misc_ref_invalid) /* fb imm  */
        .long local_label(misc_ref_invalid) /* fc odd_fixnum  */
        .long local_label(misc_ref_invalid) /* fd tra  */
        .long local_label(misc_ref_invalid) /* fe misc  */
        .long local_label(misc_ref_bit_vector) /* ff bit_vector  */

/* Functions are funny.  The first N words are treated as */
/* (UNSIGNED-BYTE 32), where N is the low 16 bits of the first word. */

local_label(misc_ref_function):
	__(movzwl misc_data_offset(%arg_y), %imm0)
	/* XXX bootstrapping */
	__(btr $15,%imm0)
	__(jnc 0f)
	__(movl $0xffffff00,%temp0)
	__(andl misc_header_offset(%arg_y),%temp0)
	__(shr $num_subtag_bits-fixnumshift,%temp0)
	__(shl $fixnumshift,%imm0)
	__(subl %imm0,%temp0)
	__(movl %temp0,%imm0)
	__(shr $fixnumshift,%imm0)
0:	
	__(shl $fixnumshift,%imm0)
	__(rcmpl(%arg_z,%imm0))
	__(jb local_label(misc_ref_u32))
local_label(misc_ref_node):
	__(movl misc_data_offset(%arg_y,%arg_z),%arg_z)
	__(ret)
local_label(misc_ref_u32):
	__(movl misc_data_offset(%arg_y,%arg_z),%imm0)
	__(jmp _SPmakeu32)
local_label(misc_ref_s32):
	__(movl misc_data_offset(%arg_y,%arg_z),%imm0)
	__(jmp _SPmakes32)
local_label(misc_ref_single_float_vector):
	__(movss misc_data_offset(%arg_y,%arg_z),%fp1)
	__(movl $single_float_header,%imm0)
	__(movd %imm0,%mm0)
	__(Misc_Alloc_Fixed(%arg_z,single_float.size))
	__(movss %fp1,single_float.value(%arg_z))
	__(ret)
local_label(misc_ref_double_float_vector):
	__(movsd misc_dfloat_offset(%arg_y,%arg_z,2),%fp1)
	__(movl $double_float_header,%imm0)
	__(movd %imm0,%mm0)
	__(Misc_Alloc_Fixed(%arg_z,double_float.size))
	__(movsd %fp1,double_float.value(%arg_z))
	__(ret)
local_label(misc_ref_fixnum_vector):
	__(movl misc_data_offset(%arg_y,%arg_z),%imm0)
	__(box_fixnum(%imm0,%arg_z))
	__(ret)
local_label(misc_ref_u8):
	__(movl %arg_z,%imm0)
	__(shr $2,%imm0)
	__(movzbl misc_data_offset(%arg_y,%imm0),%imm0)
	__(box_fixnum(%imm0,%arg_z))
	__(ret)
local_label(misc_ref_s8):
	__(movl %arg_z,%imm0)
	__(shr $2,%imm0)
	__(movsbl misc_data_offset(%arg_y,%imm0),%imm0)
	__(box_fixnum(%imm0,%arg_z))
	__(ret)
local_label(misc_ref_string):
	__(movl %arg_z,%imm0)
	__(movl misc_data_offset(%arg_y,%imm0),%imm0)
	__(shll $charcode_shift,%imm0)
	__(leal subtag_character(%imm0),%arg_z)
	__(ret)
local_label(misc_ref_u16):
	__(movl %arg_z,%imm0)
	__(shrl $1,%imm0)
	__(movzwl misc_data_offset(%arg_y,%imm0),%imm0)
	__(box_fixnum(%imm0,%arg_z))
	__(ret)
local_label(misc_ref_s16):
	__(movl %arg_z,%imm0)
	__(shrl $1,%imm0)
	__(movswl misc_data_offset(%arg_y,%imm0),%imm0)
	__(box_fixnum(%imm0,%arg_z))
	__(ret)
local_label(misc_ref_bit_vector):
	__(unbox_fixnum(%arg_z,%imm0))
	__(btl %imm0,misc_data_offset(%arg_y))
	__(setc %imm0_b)
	__(movzbl %imm0_b,%imm0)
	__(box_fixnum(%imm0,%arg_z))
	__(ret)
local_label(misc_ref_invalid):
	__(pop %temp1)	/* return addr */
	__(push $reserved_frame_marker)
	__(push $reserved_frame_marker)
	__(push $XBADVEC)
	__(push %temp1)
	__(set_nargs(3))
	__(jmp _SPksignalerr)
_endfn(C(misc_ref_common))

/* Like misc_ref, only the boxed subtag is in temp0. */
_spentry(subtag_misc_ref)
	__(mov %arg_y,%imm0)
	__(and $tagmask,%imm0)
	__(cmp $tag_misc,%imm0)
	__(jne 0f)
	__(testb $fixnummask,%arg_z_b)
	__(jne 1f)
	__(movl misc_header_offset(%arg_y),%imm0)
	__(xorb %imm0_b,%imm0_b)
	__(shrl $num_subtag_bits-fixnumshift,%imm0)
	__(cmp %imm0,%arg_z)
	__(jae 2f)
	__(unbox_fixnum(%temp0,%imm0))
	__(jmp C(misc_ref_common))
0:	__(uuo_error_reg_not_tag(Rarg_y,tag_misc))
1:	__(uuo_error_reg_not_fixnum(Rarg_z))
2:	__(uuo_error_vector_bounds(Rarg_z,Rarg_y))
_endsubp(subtag_misc_ref)

/* Like misc_set, only the boxed subtag is in temp1. */
_spentry(subtag_misc_set)
	__(mov %temp0,%imm0)
	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne 0f)
	__(mov %arg_y,%imm0)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(movl misc_header_offset(%temp0),%imm0)
	__(xorb %imm0_b,%imm0_b)
	__(shrl $num_subtag_bits-fixnumshift,%imm0)
	__(cmpl %imm0,%arg_y)
	__(jae 2f)
	__(unbox_fixnum(%temp1,%imm0))
	__(jmp C(misc_set_common))
0:	__(uuo_error_reg_not_tag(Rtemp0,tag_misc))
1:	__(uuo_error_reg_not_fixnum(Rarg_y))
2:	__(uuo_error_vector_bounds(Rarg_y,Rtemp0))
_endsubp(subtag_misc_set)

/* %temp0 = vector, %arg_y = unscaled-idx, %arg_z = val */
_spentry(misc_set)
	__(mov %temp0,%imm0)
	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne 0f)
	__(test $fixnummask,%arg_y)
	__(jne 1f)
	__(movl misc_header_offset(%temp0),%imm0)
	__(xorb %imm0_b,%imm0_b)
	__(shrl $num_subtag_bits-fixnumshift,%imm0)
	__(cmpl %imm0,%arg_y)
	__(jae 2f)
	__(xorl %imm0,%imm0)
	__(movb misc_subtag_offset(%temp0),%imm0_b)
	__(jmp C(misc_set_common))
0:	__(uuo_error_reg_not_tag(Rtemp0,tag_misc))
1:	__(uuo_error_reg_not_fixnum(Rarg_y))
2:	__(uuo_error_vector_bounds(Rarg_y,Rtemp0))
_endsubp(misc_set)

/* imm0_b = subtag, %temp0 = vector, %arg_y = index, %arg_z = value */
_startfn(C(misc_set_common))
	__(movzbl %imm0_b,%imm0)
	__(leal local_label(misc_set_jmp)(,%imm0,4),%imm0)
	__(jmp *(%imm0))
	.p2align 2
local_label(misc_set_jmp):
	/* 00-0f */
        .long local_label(misc_set_invalid) /* 00 even_fixnum  */
        .long local_label(misc_set_invalid) /* 01 cons  */
        .long local_label(misc_set_invalid) /* 02 nodeheader  */
        .long local_label(misc_set_invalid) /* 03 imm  */
        .long local_label(misc_set_invalid) /* 04 odd_fixnum  */
        .long local_label(misc_set_invalid) /* 05 tra  */
        .long local_label(misc_set_invalid) /* 06 misc  */
        .long local_label(misc_set_u32) /* 07 bignum  */
        .long local_label(misc_set_invalid) /* 08 even_fixnum  */
        .long local_label(misc_set_invalid) /* 09 cons  */
        .long _SPgvset /* 0a ratio  */
        .long local_label(misc_set_invalid) /* 0b imm  */
        .long local_label(misc_set_invalid) /* 0c odd_fixnum  */
        .long local_label(misc_set_invalid) /* 0d tra  */
        .long local_label(misc_set_invalid) /* 0e misc  */
        .long local_label(misc_set_u32) /* 0f single_float  */
        /* 10-1f  */
        .long local_label(misc_set_invalid) /* 10 even_fixnum  */
        .long local_label(misc_set_invalid) /* 11 cons  */
        .long local_label(misc_set_invalid) /* 12 nodeheader  */
        .long local_label(misc_set_invalid) /* 13 imm  */
        .long local_label(misc_set_invalid) /* 14 odd_fixnum  */
        .long local_label(misc_set_invalid) /* 15 tra  */
        .long local_label(misc_set_invalid) /* 16 misc  */
        .long local_label(misc_set_u32) /* 17 double_float  */
        .long local_label(misc_set_invalid) /* 18 even_fixnum  */
        .long local_label(misc_set_invalid) /* 19 cons  */
        .long _SPgvset /* 1a complex  */
        .long local_label(misc_set_invalid) /* 1b imm  */
        .long local_label(misc_set_invalid) /* 1c odd_fixnum  */
        .long local_label(misc_set_invalid) /* 1d tra  */
        .long local_label(misc_set_invalid) /* 1e misc  */
        .long local_label(misc_set_u32) /* 1f macptr  */
        /* 20-2f  */
        .long local_label(misc_set_invalid) /* 20 even_fixnum  */
        .long local_label(misc_set_invalid) /* 21 cons  */
        .long _SPgvset /* 22 catch_frame  */
        .long local_label(misc_set_invalid) /* 23 imm  */
        .long local_label(misc_set_invalid) /* 24 odd_fixnum  */
        .long local_label(misc_set_invalid) /* 25 tra  */
        .long local_label(misc_set_invalid) /* 26 misc  */
        .long local_label(misc_set_u32) /* 27 dead_macptr  */
        .long local_label(misc_set_invalid) /* 28 even_fixnum  */
        .long local_label(misc_set_invalid) /* 29 cons  */
        .long local_label(misc_set_function) /* 2a function  */
        .long local_label(misc_set_invalid) /* 2b imm  */
        .long local_label(misc_set_invalid) /* 2c odd_fixnum  */
        .long local_label(misc_set_invalid) /* 2d tra  */
        .long local_label(misc_set_invalid) /* 2e misc  */
        .long local_label(misc_set_invalid) /* 2f immheader  */
        /* 30-3f  */
        .long local_label(misc_set_invalid) /* 30 even_fixnum  */
        .long local_label(misc_set_invalid) /* 31 cons  */
        .long _SPgvset /* 32 basic_stream  */
        .long local_label(misc_set_invalid) /* 33 imm  */
        .long local_label(misc_set_invalid) /* 34 odd_fixnum  */
        .long local_label(misc_set_invalid) /* 35 tra  */
        .long local_label(misc_set_invalid) /* 36 misc  */
        .long local_label(misc_set_invalid) /* 37 immheader  */
        .long local_label(misc_set_invalid) /* 38 even_fixnum  */
        .long local_label(misc_set_invalid) /* 39 cons  */
        .long _SPgvset /* 3a symbol  */
        .long local_label(misc_set_invalid) /* 3b imm  */
        .long local_label(misc_set_invalid) /* 3c odd_fixnum  */
        .long local_label(misc_set_invalid) /* 3d tra  */
        .long local_label(misc_set_invalid) /* 3e misc  */
        .long local_label(misc_set_u32) /* 3f xcode_vector  */
        /* 40-4f  */
        .long local_label(misc_set_invalid) /* 40 even_fixnum  */
        .long local_label(misc_set_invalid) /* 41 cons  */
        .long _SPgvset /* 42 lock  */
        .long local_label(misc_set_invalid) /* 43 imm  */
        .long local_label(misc_set_invalid) /* 44 odd_fixnum  */
        .long local_label(misc_set_invalid) /* 45 tra  */
        .long local_label(misc_set_invalid) /* 46 misc  */
        .long local_label(misc_set_invalid) /* 47 immheader  */
        .long local_label(misc_set_invalid) /* 48 even_fixnum  */
        .long local_label(misc_set_invalid) /* 49 cons  */
        .long _SPgvset /* 4a hash_vector  */
        .long local_label(misc_set_invalid) /* 4b imm  */
        .long local_label(misc_set_invalid) /* 4c odd_fixnum  */
        .long local_label(misc_set_invalid) /* 4d tra  */
        .long local_label(misc_set_invalid) /* 4e misc  */
        .long local_label(misc_set_invalid) /* 4f immheader  */
        /* 50-5f  */
        .long local_label(misc_set_invalid) /* 50 even_fixnum  */
        .long local_label(misc_set_invalid) /* 51 cons  */
        .long _SPgvset /* 52 pool  */
        .long local_label(misc_set_invalid) /* 53 imm  */
        .long local_label(misc_set_invalid) /* 54 odd_fixnum  */
        .long local_label(misc_set_invalid) /* 55 tra  */
        .long local_label(misc_set_invalid) /* 56 misc  */
        .long local_label(misc_set_invalid) /* 57 immheader  */
        .long local_label(misc_set_invalid) /* 58 even_fixnum  */
        .long local_label(misc_set_invalid) /* 59 cons  */
        .long _SPgvset /* 5a weak  */
        .long local_label(misc_set_invalid) /* 5b imm  */
        .long local_label(misc_set_invalid) /* 5c odd_fixnum  */
        .long local_label(misc_set_invalid) /* 5d tra  */
        .long local_label(misc_set_invalid) /* 5e misc  */
        .long local_label(misc_set_invalid) /* 5f immheader  */
        /* 60-6f  */
        .long local_label(misc_set_invalid) /* 60 even_fixnum  */
        .long local_label(misc_set_invalid) /* 61 cons  */
        .long _SPgvset /* 62 package  */
        .long local_label(misc_set_invalid) /* 63 imm  */
        .long local_label(misc_set_invalid) /* 64 odd_fixnum  */
        .long local_label(misc_set_invalid) /* 65 tra  */
        .long local_label(misc_set_invalid) /* 66 misc  */
        .long local_label(misc_set_invalid) /* 67 immheader  */
        .long local_label(misc_set_invalid) /* 68 even_fixnum  */
        .long local_label(misc_set_invalid) /* 69 cons  */
        .long _SPgvset /* 6a slot_vector  */
        .long local_label(misc_set_invalid) /* 6b imm  */
        .long local_label(misc_set_invalid) /* 6c odd_fixnum  */
        .long local_label(misc_set_invalid) /* 6d tra  */
        .long local_label(misc_set_invalid) /* 6e misc  */
        .long local_label(misc_set_invalid) /* 6f immheader  */
        /* 70-7f  */
        .long local_label(misc_set_invalid) /* 70 even_fixnum  */
        .long local_label(misc_set_invalid) /* 71 cons  */
        .long _SPgvset /* 72 instance  */
        .long local_label(misc_set_invalid) /* 73 imm  */
        .long local_label(misc_set_invalid) /* 74 odd_fixnum  */
        .long local_label(misc_set_invalid) /* 75 tra  */
        .long local_label(misc_set_invalid) /* 76 misc  */
        .long local_label(misc_set_invalid) /* 77 immheader  */
        .long local_label(misc_set_invalid) /* 78 even_fixnum  */
        .long local_label(misc_set_invalid) /* 79 cons  */
        .long _SPgvset /* 7a struct  */
        .long local_label(misc_set_invalid) /* 7b imm  */
        .long local_label(misc_set_invalid) /* 7c odd_fixnum  */
        .long local_label(misc_set_invalid) /* 7d tra  */
        .long local_label(misc_set_invalid) /* 7e misc  */
        .long local_label(misc_set_invalid) /* 7f immheader  */
        /* 80-8f  */
        .long local_label(misc_set_invalid) /* 80 even_fixnum  */
        .long local_label(misc_set_invalid) /* 81 cons  */
        .long _SPgvset /* 82 istruct  */
        .long local_label(misc_set_invalid) /* 83 imm  */
        .long local_label(misc_set_invalid) /* 84 odd_fixnum  */
        .long local_label(misc_set_invalid) /* 85 tra  */
        .long local_label(misc_set_invalid) /* 86 misc  */
        .long local_label(misc_set_invalid) /* 87 immheader  */
        .long local_label(misc_set_invalid) /* 88 even_fixnum  */
        .long local_label(misc_set_invalid) /* 89 cons  */
        .long _SPgvset /* 8a value_cell  */
        .long local_label(misc_set_invalid) /* 8b imm  */
        .long local_label(misc_set_invalid) /* 8c odd_fixnum  */
        .long local_label(misc_set_invalid) /* 8d tra  */
        .long local_label(misc_set_invalid) /* 8e misc  */
        .long local_label(misc_set_invalid) /* 8f immheader  */
        /* 90-9f  */
        .long local_label(misc_set_invalid) /* 90 even_fixnum  */
        .long local_label(misc_set_invalid) /* 91 cons  */
        .long _SPgvset /* 92 xfunction  */
        .long local_label(misc_set_invalid) /* 93 imm  */
        .long local_label(misc_set_invalid) /* 94 odd_fixnum  */
        .long local_label(misc_set_invalid) /* 95 tra  */
        .long local_label(misc_set_invalid) /* 96 misc  */
        .long local_label(misc_set_invalid) /* 97 immheader  */
        .long local_label(misc_set_invalid) /* 98 even_fixnum  */
        .long local_label(misc_set_invalid) /* 99 cons  */
        .long _SPgvset /* 9a arrayH  */
        .long local_label(misc_set_invalid) /* 9b imm  */
        .long local_label(misc_set_invalid) /* 9c odd_fixnum  */
        .long local_label(misc_set_invalid) /* 9d tra  */
        .long local_label(misc_set_invalid) /* 9e misc  */
        .long local_label(misc_set_invalid) /* 9f immheader  */
        /* a0-af  */
        .long local_label(misc_set_invalid) /* a0 even_fixnum  */
        .long local_label(misc_set_invalid) /* a1 cons  */
        .long _SPgvset /* a2 vectorH  */
        .long local_label(misc_set_invalid) /* a3 imm  */
        .long local_label(misc_set_invalid) /* a4 odd_fixnum  */
        .long local_label(misc_set_invalid) /* a5 tra  */
        .long local_label(misc_set_invalid) /* a6 misc  */
        .long local_label(misc_set_single_float_vector) /* a7 sf_vector  */
        .long local_label(misc_set_invalid) /* a8 even_fixnum  */
        .long local_label(misc_set_invalid) /* a9 cons  */
        .long _SPgvset /* aa simple_vector  */
        .long local_label(misc_set_invalid) /* ab imm  */
        .long local_label(misc_set_invalid) /* ac odd_fixnum  */
        .long local_label(misc_set_invalid) /* ad tra  */
        .long local_label(misc_set_invalid) /* ae misc  */
        .long local_label(misc_set_u32) /* af u32  */
        /* b0-bf  */
        .long local_label(misc_set_invalid) /* b0 even_fixnum  */
        .long local_label(misc_set_invalid) /* b1 cons  */
        .long local_label(misc_set_invalid) /* b2 nodeheader  */
        .long local_label(misc_set_invalid) /* b3 imm  */
        .long local_label(misc_set_invalid) /* b4 odd_fixnum  */
        .long local_label(misc_set_invalid) /* b5 tra  */
        .long local_label(misc_set_invalid) /* b6 misc  */
        .long local_label(misc_set_s32) /* b7 s32  */
        .long local_label(misc_set_invalid) /* b8 even_fixnum  */
        .long local_label(misc_set_invalid) /* b9 cons  */
        .long local_label(misc_set_invalid) /* ba nodeheader  */
        .long local_label(misc_set_invalid) /* bb imm  */
        .long local_label(misc_set_invalid) /* bc odd_fixnum  */
        .long local_label(misc_set_invalid) /* bd tra  */
        .long local_label(misc_set_invalid) /* be misc  */
        .long local_label(misc_set_fixnum_vector) /* bf fixnum_vector  */
        /* c0-cf  */
        .long local_label(misc_set_invalid) /* c0 even_fixnum  */
        .long local_label(misc_set_invalid) /* c1 cons  */
        .long local_label(misc_set_invalid) /* c2 nodeheader  */
        .long local_label(misc_set_invalid) /* c3 imm  */
        .long local_label(misc_set_invalid) /* c4 odd_fixnum  */
        .long local_label(misc_set_invalid) /* c5 tra  */
        .long local_label(misc_set_invalid) /* c6 misc  */
        .long local_label(misc_set_string) /* c7 simple_base_string  */
        .long local_label(misc_set_invalid) /* c8 even_fixnum  */
        .long local_label(misc_set_invalid) /* c9 cons  */
        .long local_label(misc_set_invalid) /* ca nodeheader  */
        .long local_label(misc_set_invalid) /* cb imm  */
        .long local_label(misc_set_invalid) /* cc odd_fixnum  */
        .long local_label(misc_set_invalid) /* cd tra  */
        .long local_label(misc_set_invalid) /* ce misc  */
        .long local_label(misc_set_u8) /* cf u8  */
        /* d0-df  */
        .long local_label(misc_set_invalid) /* d0 even_fixnum  */
        .long local_label(misc_set_invalid) /* d1 cons  */
        .long local_label(misc_set_invalid) /* d2 nodeheader  */
        .long local_label(misc_set_invalid) /* d3 imm  */
        .long local_label(misc_set_invalid) /* d4 odd_fixnum  */
        .long local_label(misc_set_invalid) /* d5 tra  */
        .long local_label(misc_set_invalid) /* d6 misc  */
        .long local_label(misc_set_s8)      /* d7 s8  */
        .long local_label(misc_set_invalid) /* d8 even_fixnum  */
        .long local_label(misc_set_invalid) /* d9 cons  */
        .long local_label(misc_set_invalid) /* da nodeheader  */
        .long local_label(misc_set_invalid) /* db imm  */
        .long local_label(misc_set_invalid) /* dc odd_fixnum  */
        .long local_label(misc_set_invalid) /* dd tra  */
        .long local_label(misc_set_invalid) /* de misc  */
        .long local_label(misc_set_invalid) /* df immheader  */
        /* e0-ef  */
        .long local_label(misc_set_invalid) /* e0 even_fixnum  */
        .long local_label(misc_set_invalid) /* e1 cons  */
        .long local_label(misc_set_invalid) /* e2 nodeheader  */
        .long local_label(misc_set_invalid) /* e3 imm  */
        .long local_label(misc_set_invalid) /* e4 odd_fixnum  */
        .long local_label(misc_set_invalid) /* e5 tra  */
        .long local_label(misc_set_invalid) /* e6 misc  */
        .long local_label(misc_set_u16) /* e7 u16  */
        .long local_label(misc_set_invalid) /* e8 even_fixnum  */
        .long local_label(misc_set_invalid) /* e9 cons  */
        .long local_label(misc_set_invalid) /* ea nodeheader  */
        .long local_label(misc_set_invalid) /* eb imm  */
        .long local_label(misc_set_invalid) /* ec odd_fixnum  */
        .long local_label(misc_set_invalid) /* ed tra  */
        .long local_label(misc_set_invalid) /* ee misc  */
        .long local_label(misc_set_s16) /* ef s16  */
        /* f0-ff  */
        .long local_label(misc_set_invalid) /* f0 even_fixnum  */
        .long local_label(misc_set_invalid) /* f1 cons  */
        .long local_label(misc_set_invalid) /* f2 nodeheader  */
        .long local_label(misc_set_invalid) /* f3 imm  */
        .long local_label(misc_set_invalid) /* f4 odd_fixnum  */
        .long local_label(misc_set_invalid) /* f5 tra  */
        .long local_label(misc_set_invalid) /* f6 misc  */
        .long local_label(misc_set_double_float_vector) /* f7 df vector  */
        .long local_label(misc_set_invalid) /* f8 even_fixnum  */
        .long local_label(misc_set_invalid) /* f9 cons  */
        .long local_label(misc_set_invalid) /* fa nodeheader  */
        .long local_label(misc_set_invalid) /* fb imm  */
        .long local_label(misc_set_invalid) /* fc odd_fixnum  */
        .long local_label(misc_set_invalid) /* fd tra  */
        .long local_label(misc_set_invalid) /* fe misc  */
        .long local_label(misc_set_bit_vector) /* ff bit_vector  */

local_label(misc_set_function):
	/* Functions are funny: the first N words are treated as */
	/* (UNSIGNED-BYTE 32), where N is the low 16 bits of the first word. */
	__(movzwl misc_data_offset(%temp0),%imm0)
	/* XXX bootstrapping */
	__(btr $15,%imm0)
	__(jnc 0f)
	__(movl $0xffffff00,%temp1)
	__(andl misc_header_offset(%temp0),%temp1)
	__(shr $num_subtag_bits-fixnumshift,%temp1)
	__(shl $fixnumshift,%imm0)
	__(subl %imm0,%temp1)
	__(movl %temp1,%imm0)
	__(shr $fixnumshift,%imm0)
0:
	__(shl $fixnumshift,%imm0)
	__(rcmpl(%arg_y,%imm0))
	__(jae _SPgvset)
local_label(misc_set_u32):
	/* Either a non-negative fixnum, a positive one-digit bignum, or */
	/* a two-digit bignum whose sign-digit is 0 is OK. */
	__(movl $~(target_most_positive_fixnum <<fixnumshift),%imm0)
	__(test %arg_z,%imm0)
	__(movl %arg_z,%imm0)
	__(jne 1f)
	__(sarl $fixnumshift,%imm0)
	__(jmp 9f)
1:	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movb misc_subtag_offset(%arg_z),%imm0_b)
	__(cmpb $subtag_bignum,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movl misc_header_offset(%arg_z),%imm0)
	__(cmpl $two_digit_bignum_header,%imm0)
	__(je 3f)
	__(cmpl $one_digit_bignum_header,%imm0)
	__(jne local_label(misc_set_bad))
	__(movl misc_data_offset(%arg_z),%imm0)
	__(testl %imm0,%imm0)
	__(js local_label(misc_set_bad))
	__(jmp 9f)
3:	__(movl misc_data_offset(%arg_z),%imm0)
	__(cmpl $0,misc_data_offset+4(%arg_z))
	__(jne local_label(misc_set_bad))
9:	__(movl %imm0,misc_data_offset(%temp0,%arg_y))
	__(ret)
local_label(misc_set_s32):
	__(unbox_fixnum(%arg_z,%imm0))
	__(testb $fixnummask,%arg_z_b)
	__(je 9f)
1:	__(movb %arg_z_b,%imm0_b)
	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movl misc_header_offset(%arg_z),%imm0)
	__(cmpl $one_digit_bignum_header,%imm0)
	__(jne local_label(misc_set_bad))
	__(movl misc_data_offset(%arg_z),%imm0)
9:	__(movl %imm0,misc_data_offset(%temp0,%arg_y))
	__(ret)
local_label(misc_set_bad):
	__(movl %arg_z,%arg_y)
	__(movl %temp0,%arg_z)
	__(pop %temp1)	/* return addr */
	__(push $reserved_frame_marker)
	__(push $reserved_frame_marker)
	__(push $XNOTELT)
	__(push %temp1)
	__(set_nargs(3))
	__(jmp _SPksignalerr)
local_label(misc_set_single_float_vector):
	__(extract_lisptag(%arg_z,%imm0))
	__(cmpb $tag_misc,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movb misc_subtag_offset(%arg_z),%imm0_b)
	__(cmpb $subtag_single_float,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movl single_float.value(%arg_z),%imm0)
	__(movl %imm0,misc_data_offset(%temp0,%arg_y))
	__(ret)
local_label(misc_set_double_float_vector):
	__(extract_lisptag(%arg_z,%imm0))
	__(cmpb $tag_misc,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movb misc_subtag_offset(%arg_z),%imm0_b)
	__(cmpb $subtag_double_float,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movsd double_float.value(%arg_z),%fp0)
	__(movsd %fp0,misc_dfloat_offset(%temp0,%arg_y,2))
	__(ret)
local_label(misc_set_fixnum_vector):
	__(unbox_fixnum(%arg_z,%imm0))
	__(testb $fixnummask,%arg_z_b)
	__(jne local_label(misc_set_bad))
	__(movl %imm0,misc_data_offset(%temp0,%arg_y))
	__(ret)
local_label(misc_set_u8):
	__(testl $~(0xff<<fixnumshift),%arg_z)
	__(jne local_label(misc_set_bad))
	__(unbox_fixnum(%arg_y,%imm0))
	__(movl %arg_z,%arg_y)
	__(shll $8-fixnumshift,%arg_z)
	__(movb %arg_z_bh,misc_data_offset(%temp0,%imm0))
	__(movl %arg_y,%arg_z)
	__(ret)
local_label(misc_set_s8):
	__(movl %arg_z,%imm0)
	__(shll $32-(8+fixnumshift),%imm0)
	__(sarl $32-(8+fixnumshift),%imm0)
	__(cmpl %arg_z,%imm0)
	__(jne local_label(misc_set_bad))
	__(testb $fixnummask,%arg_z_b)
	__(jne local_label(misc_set_bad))
	__(unbox_fixnum(%arg_y,%imm0))
	__(movl %arg_z,%arg_z)
	__(shll $8-fixnumshift,%arg_z)
	__(movb %arg_z_bh,misc_data_offset(%temp0,%imm0))
	__(movl %arg_y,%arg_z)
	__(ret)
local_label(misc_set_string):
	__(cmpb $subtag_character,%arg_z_b)
	__(jne local_label(misc_set_bad))
	__(movl %arg_z,%imm0)
	__(shrl $charcode_shift,%imm0)
	__(movl %imm0,misc_data_offset(%temp0,%arg_y))
	__(ret)
local_label(misc_set_u16):
	__(testl $~(0xffff<<fixnumshift),%arg_z)
	__(jne local_label(misc_set_bad))
	__(movl %arg_y,%imm0)
	__(shrl $1,%imm0)
	__(mark_as_imm(%temp1))
	__(unbox_fixnum(%arg_z,%temp1))
	__(movw %temp1_w,misc_data_offset(%temp0,%imm0))
	__(mark_as_node(%temp1))
	__(ret)
local_label(misc_set_s16):
	__(movl %arg_z,%imm0)
	__(shll $32-(16+fixnumshift),%imm0)
	__(sarl $32-(16+fixnumshift),%imm0)
	__(cmpl %arg_z,%imm0)
	__(jne local_label(misc_set_bad))
	__(testb $fixnummask,%arg_z_b)
	__(jne local_label(misc_set_bad))
	__(movl %arg_y,%imm0)
	__(shrl $1,%imm0)
	__(mark_as_imm(%temp1))
	__(unbox_fixnum(%arg_z,%temp1))
	__(movw %temp1_w,misc_data_offset(%temp0,%imm0))
	__(mark_as_node(%temp1))
	__(ret)
local_label(misc_set_bit_vector):
	__(testl $~fixnumone,%arg_z)
	__(jne local_label(misc_set_bad))
	__(unbox_fixnum(%arg_y,%imm0))
	__(testb %arg_z_b,%arg_z_b)
	__(je local_label(misc_set_clr_bit))
local_label(misc_set_set_bit):
	__(btsl %imm0,misc_data_offset(%temp0))
	__(ret)
local_label(misc_set_clr_bit):
	__(btrl %imm0,misc_data_offset(%temp0))
	__(ret)
local_label(misc_set_invalid):
	__(pop %temp1)	/* return addr */
	__(push $reserved_frame_marker)
	__(push $reserved_frame_marker)
	__(push $XSETBADVEC)
	__(push %temp0)
	__(push %temp1)
	__(set_nargs(4))
	__(jmp _SPksignalerr)
_endfn(C(misc_set_common))

_spentry(Fret1valn)
	.globl C(ret1valn)
__(tra(C(ret1valn)))
        __(mov (%esp),%ra0)
        __(mov %arg_z,(%esp))
	__(set_nargs(1))
	__(jmp *%ra0)
_endsubp(Fret1valn)

_spentry(nvalret)
	.globl C(nvalret)
C(nvalret):
	__(ref_global(ret1val_addr,%temp0))
	__(cmpl lisp_frame.savera0(%ebp),%temp0)
	__(je 1f)
	__(test %nargs,%nargs)
	__(movl $nil_value,%arg_z)
	__(cmovnel -node_size(%esp,%nargs),%arg_z)
	__(leave)
	__(ret)

/* actually need to return values; always need to copy. */
1:	__(lea 2*node_size(%ebp),%imm0)
	__(pushl (%imm0))
	__(movl 0(%ebp),%ebp)
	__(addl $node_size,%imm0)
	__(lea node_size(%esp,%nargs),%temp0)
	__(xorl %arg_y,%arg_y)
	__(jmp 3f)
2:	__(movl -node_size(%temp0),%arg_z)
	__(subl $node_size,%temp0)
	__(addl $node_size,%arg_y)
	__(movl %arg_z,-node_size(%imm0))
	__(subl $node_size,%imm0)
3:	__(cmpl %arg_y,%nargs)
	__(jne 2b)
	__(pop %ra0)
	__(movl %imm0,%esp)
	__(jmp *%ra0)
_endsubp(nvalret)

_spentry(jmpsym)
	__(jump_fname())
_endsubp(jmpsym)

_spentry(jmpnfn)
	__(mov %temp0,%fn)
	__(jmp *%fn)
_endsubp(jmpnfn)

_spentry(funcall)
	__(do_funcall())
_endsubp(funcall)

/* Make a lisp integer (fixnum or one-digit bignum) from the value in %imm0 */
_spentry(makes32)
	__(imull $fixnumone,%imm0,%arg_z)	/* result is fixnum-tagged */
	__(jno 0f)				/* but may have overflowed */
	__(movd %imm0,%mm1)
	__(movl $one_digit_bignum_header,%imm0)
	__(movd %imm0,%mm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(1)))
	__(movd %mm1,misc_data_offset(%arg_z))
0:	__(repret)
_endsubp(makes32)

/* Make a lisp integer out of the unboxed 64-bit word in %mm0. */
/* This is a little clumsy, but the alternative requires callers to */
/* have already marked %edx as an imm reg (or else store it in memory
/* somewhere), and I'm nervous about */
/* splitting up the mark-as-imm/mark-as-node between two separate */
/* pieces of code. */
_spentry(makes64)
        __(movq %mm0,%mm2)
        __(pshufw $0x4e,%mm0,%mm1)      /* swap hi/lo halves */
        __(psrad $31,%mm0)      /* propagate sign */
        __(pcmpeqd %mm0,%mm1)	/* all ones if equal */
        __(movd %mm1,%imm0)
        __(cmpb $-1,%imm0_b)    /* upper half just sign extension? */
        __(jne 1f)
        __(movd %mm2,%imm0)
	__(jmp _SPmakes32)
1:      __(movl $two_digit_bignum_header,%imm0)
        __(movd %imm0,%mm0)
        __(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(2)))
        __(movq %mm2,misc_data_offset(%arg_z))
        __(ret)
_endsubp(makes64)

_spentry(syscall)
	/* Save lisp registers */
	__(push %ebp)
	__(movl %esp,%ebp)
	__(push %temp0)
        __(push %temp1)
        __(push %arg_y)
        __(push %arg_z)
        __(push %fn)
	__(movl %esp,rcontext(tcr.save_vsp))
	__(movl %ebp,rcontext(tcr.save_ebp))
	__(movl $TCR_STATE_FOREIGN,rcontext(tcr.valence))
	__(movl rcontext(tcr.foreign_sp),%esp)
	__(emms)
	__(pop %ebp)		/* backlink */
        __(lea 15(%esp),%edx)
        __(andl $-16,%edx)
        __(movl %edx,%esp)
	__(unbox_fixnum(%arg_z,%eax))	/* syscall number */
	__(movl $local_label(back_from_sysenter),%edx)
	__(push %edx)
	__(movl %esp,%ecx)
	__(sysenter)
local_label(back_from_sysenter):
	__(jnc 0f)
	__(neg %eax)
0:	
	__(movl %ebp,%esp)
	__(movl %esp,rcontext(tcr.foreign_sp))
	__(clr %arg_z)
	__(clr %arg_y)
	__(clr %temp1)
	__(clr %temp0)
	__(clr %fn)
	__(pxor %fpzero,%fpzero)
	__(movl rcontext(tcr.save_vsp),%esp)
	__(movl rcontext(tcr.save_ebp),%ebp)
	__(movl $TCR_STATE_LISP,rcontext(tcr.valence))
        __(pop %fn)
        __(pop %arg_z)
        __(pop %arg_y)
        __(pop %temp1)
	__(check_pending_interrupt(%temp0))
	__(pop %temp0)
	__(leave)
	__(ret)
_endsubp(syscall)

/* Make system call that returns a doubleword result in %edx:%eax and */
/* copy the result into %mm0. */
_spentry(syscall2)
	/* Save lisp registers */
	__(push %ebp)
	__(movl %esp,%ebp)
	__(push %temp0)
        __(push %temp1)
        __(push %arg_y)
        __(push %arg_z)
        __(push %fn)
	__(movl %esp,rcontext(tcr.save_vsp))
	__(movl %ebp,rcontext(tcr.save_ebp))
	__(movl $TCR_STATE_FOREIGN,rcontext(tcr.valence))
	__(movl rcontext(tcr.foreign_sp),%esp)
	__(emms)
	__(pop %ebp)		/* backlink */
        __(lea 15(%esp),%edx)
        __(andl $-16,%edx)
        __(movl %edx,%esp)
	__(unbox_fixnum(%arg_z,%eax))	/* syscall number */
	__(pushl $local_label(back_from_syscall))
	__(int $0x80)
local_label(back_from_syscall):
	__(jnc 0f)
	__(neg %eax)
	__(movl $-1,%edx)
0:
	/* just use memory rather than screwing around with */
	/* movd %eax,%mm0, movd %edx,%mm1, psllq $32,%mm1, por %mm1,%mm0 */
	__(push %edx)
	__(push %eax)
	__(movq (%esp),%mm0)
	__(movl %ebp,%esp)
	__(movl %esp,rcontext(tcr.foreign_sp))
	__(clr %arg_z)
	__(clr %arg_y)
	__(clr %temp1)
	__(clr %temp0)
	__(clr %fn)
	__(pxor %fpzero,%fpzero)
	__(movl rcontext(tcr.save_vsp),%esp)
	__(movl rcontext(tcr.save_ebp),%ebp)
	__(movl $TCR_STATE_LISP,rcontext(tcr.valence))
        __(pop %fn)
        __(pop %arg_z)
        __(pop %arg_y)
        __(pop %temp1)
	__(check_pending_interrupt(%temp0))
	__(pop %temp0)
	__(leave)
	__(ret)
_endsubp(syscall2)


_spentry(mkcatch1v)
	__(nMake_Catch(0))
	__(ret)
_endsubp(mkcatch1v)

_spentry(mkunwind)
	__(movl $undefined,%arg_z)
	__(Make_Catch(fixnumone))
	__(jmp *%ra0)
_endsubp(mkunwind)

/* this takes a return address in %ra0; it's "new" in that it does the */
/*   double binding of *interrupt-level* out-of-line */
_spentry(nmkunwind)
	__(movl rcontext(tcr.tlb_pointer),%arg_z)
        __(movl INTERRUPT_LEVEL_BINDING_INDEX(%arg_z),%arg_y)
	__(push %arg_y)
	__(push $INTERRUPT_LEVEL_BINDING_INDEX)
	__(push rcontext(tcr.db_link))
	__(movl %esp,rcontext(tcr.db_link))
	__(movl $-1<<fixnumshift,INTERRUPT_LEVEL_BINDING_INDEX(%arg_z))
	__(movl $undefined,%arg_z)
	/* %arg_z = tag, %xfn (%temp1) = pc */
	__(Make_Catch(fixnumone))
	__(movl %arg_y,%arg_z)
        __(jmp _SPbind_interrupt_level)
_endsubp(nmkunwind)

_spentry(mkcatchmv)
	__(nMake_Catch(fixnumone))
	__(ret)
_endsubp(mkcatchmv)

_spentry(throw)
	__(movl rcontext(tcr.catch_top),%imm0)
	__(movl (%esp,%nargs),%arg_y)	/* arg_y = tag   */
	__(movd %nargs,%mm0)
	__(xorl %temp1,%temp1)
	__(jmp local_label(_throw_test))
local_label(_throw_loop):
	__(cmpl %arg_y,catch_frame.catch_tag(%imm0))
	__(je local_label(_throw_found))
	__(movl catch_frame.link(%imm0),%imm0)
	__(addl $fixnum_one,%temp1)
local_label(_throw_test):
	__(test %imm0,%imm0)
	__(jne local_label(_throw_loop))
        __(push %ra0)
	__(uuo_error_reg_not_tag(Rarg_y,subtag_catch_frame))
        __(pop %ra0)
	__(jmp _SPthrow)
local_label(_throw_found):
	__(testb $fulltagmask,catch_frame.mvflag(%imm0))
	__(movl %temp1,%imm0)
	__(movd %mm0,%nargs)
	__(jne local_label(_throw_multiple))
	__(movl $nil_value,%arg_z)
	__(test %nargs,%nargs)
	__(je local_label(_throw_one_value))
	__(movl -node_size(%esp,%nargs),%arg_z)
	__(add %nargs,%esp)
local_label(_throw_one_value):
	__(movl $local_label(_threw_one_value),%ra0)
	__(jmp _SPnthrow1value)
__(tra(local_label(_threw_one_value)))
	__(movl rcontext(tcr.catch_top),%arg_y)
	__(movl catch_frame.db_link(%arg_y),%imm0)
	__(cmpl %imm0,rcontext(tcr.db_link))
	__(jz local_label(_threw_one_value_dont_unbind))
	__(push $local_label(_threw_one_value_dont_unbind))
	__(jmp _SPunbind_to)	/* preserves registers */
__(tra(local_label(_threw_one_value_dont_unbind)))
	__(movl catch_frame.ebp(%arg_y),%ebp)
	__(movl catch_frame.foreign_sp(%arg_y),%imm0)
        __(movl %imm0,rcontext(tcr.foreign_sp))
	__(movl catch_frame.xframe(%arg_y),%imm0)
	__(movl %imm0,rcontext(tcr.xframe))
	__(movl catch_frame.esp(%arg_y),%esp)
	__(movl catch_frame.link(%arg_y),%imm0)
	__(movl %imm0,rcontext(tcr.catch_top))
	__(lea -(tsp_frame.fixed_overhead+fulltag_misc)(%arg_y),%imm0)
	__(movl (%imm0),%imm0)
        __(movl %imm0,rcontext(tcr.save_tsp))
        __(movl %imm0,rcontext(tcr.next_tsp))
	__(movl catch_frame.pc(%arg_y),%ra0)
	__(jmp *%ra0)
local_label(_throw_multiple):
	__(movl $local_label(_threw_multiple),%ra0)
	__(jmp _SPnthrowvalues)
__(tra(local_label(_threw_multiple)))
	__(movl rcontext(tcr.catch_top),%arg_y)
	__(movl catch_frame.db_link(%arg_y),%imm0)
	__(cmpl %imm0,rcontext(tcr.db_link))
	__(je local_label(_threw_multiple_dont_unbind))
	__(push $local_label(_threw_multiple_dont_unbind))
	__(jmp _SPunbind_to)	/* preserves registers */
__(tra(local_label(_threw_multiple_dont_unbind)))
	/* Copy multiple values from the current %esp to the target %esp   */
	__(lea (%esp,%nargs),%imm0)
	__(movd %nargs,%mm0)	/* nargs is aka temp1 */
	__(movl catch_frame.esp(%arg_y),%temp1)
	__(jmp local_label(_threw_multiple_push_test))
local_label(_threw_multiple_push_loop):
	__(subl $node_size,%imm0)
	__(subl $node_size,%temp1)
	__(movl (%imm0),%arg_z)
	__(movl %arg_z,(%temp1))
local_label(_threw_multiple_push_test):
	__(cmpl %imm0,%esp)
	__(jne local_label(_threw_multiple_push_loop))
	/* target %esp is now in %temp1   */
	__(movl catch_frame.ebp(%arg_y),%ebp)
	__(movl catch_frame.foreign_sp(%arg_y),%imm0)
        __(movl %imm0,rcontext(tcr.foreign_sp))        
	__(movl catch_frame.xframe(%arg_y),%imm0)
	__(movl %imm0,rcontext(tcr.xframe))
	__(movl %temp1,%esp)
	__(movl catch_frame.link(%arg_y),%temp1)
	__(movl %temp1,rcontext(tcr.catch_top))
	__(movd %mm0,%nargs)
	__(lea -(tsp_frame.fixed_overhead+fulltag_misc)(%arg_y),%imm0)
	__(movl catch_frame.pc(%arg_y),%ra0)
	__(movl (%imm0),%imm0)
        __(movl %imm0,rcontext(tcr.save_tsp))
        __(movl %imm0,rcontext(tcr.next_tsp))
	__(jmp *%ra0)
_endsubp(throw)

	/* This takes N multiple values atop the vstack.   */
_spentry(nthrowvalues)
	__(movb $1,rcontext(tcr.unwinding))
	__(movl %ra0,rcontext(tcr.save0)) /* %ra0 (aka %temp0) to spill area */
local_label(_nthrowv_nextframe):
	__(subl $fixnumone,%imm0)
	__(js local_label(_nthrowv_done))
	__(movd %imm0,%mm1)
	__(movl rcontext(tcr.catch_top),%temp0)
	__(movl catch_frame.link(%temp0),%imm0)
	__(movl %imm0,rcontext(tcr.catch_top))
	__(movl catch_frame.db_link(%temp0),%imm0)
	__(cmpl %imm0,rcontext(tcr.db_link))
	__(jz local_label(_nthrowv_dont_unbind))
	__(push %temp1)
	__(push %temp0)
	__(push $local_label(_nthrowv_back_from_unbind))
	__(jmp _SPunbind_to)
__(tra(local_label(_nthrowv_back_from_unbind)))
	__(pop %temp0)
	__(pop %temp1)
local_label(_nthrowv_dont_unbind):
	__(cmpb $unbound_marker,catch_frame.catch_tag(%temp0))
	__(je local_label(_nthrowv_do_unwind))
/* A catch frame.  If the last one, restore context from there.   */
	__(movd %mm1,%imm0)
	__(test %imm0,%imm0)	/* last catch frame ?   */
	__(jne local_label(_nthrowv_skip))
	__(movl catch_frame.xframe(%temp0),%arg_y)
	__(movl %arg_y,rcontext(tcr.xframe))
	__(lea (%esp,%nargs),%arg_y)
	__(movl catch_frame.esp(%temp0),%arg_z)
	__(movd %nargs,%mm2)
	__(jmp local_label(_nthrowv_push_test))
local_label(_nthrowv_push_loop):
	__(subl $node_size,%arg_y)
	__(subl $node_size,%arg_z)
	__(movd (%arg_y),%mm0)
	__(movd %mm0,(%arg_z))
local_label(_nthrowv_push_test):
	__(subl $node_size,%nargs)
	__(jns local_label(_nthrowv_push_loop))
	__(movd %mm2,%nargs)
	__(movl catch_frame.xframe(%temp0),%arg_y)
	__(movl %arg_y,rcontext(tcr.xframe))
	__(movl %arg_z,%esp)
	__(movl catch_frame.ebp(%temp0),%ebp)
	__(movd catch_frame.foreign_sp(%temp0),%stack_temp)
        __(movd %stack_temp,rcontext(tcr.foreign_sp))        
local_label(_nthrowv_skip):	
	__(movl -(tsp_frame.fixed_overhead+fulltag_misc)(%temp0),%imm0)
        __(movl %imm0,rcontext(tcr.save_tsp))
        __(movl %imm0,rcontext(tcr.next_tsp))
	__(movd %mm1,%imm0)
	__(jmp local_label(_nthrowv_nextframe))
local_label(_nthrowv_do_unwind):	
/* This is harder.  Call the cleanup code with the multiple values and   */
/* nargs, the throw count, and the caller's return address in a temp  */
/* stack frame.   */
	__(leal (%esp,%nargs),%arg_y)
	__(push catch_frame.pc(%temp0))
	__(movl catch_frame.ebp(%temp0),%ebp)
        __(movd catch_frame.xframe(%temp0),%stack_temp)
        __(movd %stack_temp,rcontext(tcr.xframe))
	__(movl catch_frame.esp(%temp0),%arg_z)
	__(movd catch_frame.foreign_sp(%temp0),%stack_temp)
        __(movd %stack_temp,rcontext(tcr.foreign_sp))        
	/* Discard the catch frame, so we can build a temp frame   */
	__(movl -(tsp_frame.fixed_overhead+fulltag_misc)(%temp0),%imm0)
        __(movl %imm0,rcontext(tcr.save_tsp))
        __(movl %imm0,rcontext(tcr.next_tsp))
	__(movd %temp1,%mm2) /* save %nargs */
	/* tsp overhead, nargs, throw count, ra0   */
	__(dnode_align(%nargs,(tsp_frame.fixed_overhead+(3*node_size)),%imm0))
	__(movl %imm0,%temp1)
	__(TSP_Alloc_Var(%temp1,%imm0))
	__(movd %mm2,%temp1) /* aka %nargs */

	__(movl %nargs,(%imm0))
	__(movl rcontext(tcr.save0),%ra0)
	__(movl %ra0,node_size(%imm0))
	__(movd %mm1,node_size*2(%imm0))
	__(leal node_size*3(%imm0),%imm0)
	__(jmp local_label(_nthrowv_tpushtest))
local_label(_nthrowv_tpushloop):
	__(movl -node_size(%arg_y),%temp0)
	__(subl $node_size,%arg_y)
	__(movl %temp0,(%imm0))
	__(addl $node_size,%imm0)
local_label(_nthrowv_tpushtest):
	__(subl $node_size,%nargs)
	__(jns local_label(_nthrowv_tpushloop))
	__(pop %xfn)	/* aka %temp1/%nargs */
	__(movl %arg_z,%esp)
/* Ready to call cleanup code. set up tra, jmp to %xfn   */
	__(push $local_label(_nthrowv_called_cleanup))
	__(movb $0,rcontext(tcr.unwinding))
	__(jmp *%xfn)
__(tra(local_label(_nthrowv_called_cleanup)))

	__(movb $1,rcontext(tcr.unwinding))
	__(movl rcontext(tcr.save_tsp),%imm0)
	__(movl tsp_frame.data_offset+(0*node_size)(%imm0),%nargs)
	__(movl tsp_frame.data_offset+(1*node_size)(%imm0),%ra0)
	__(movl %ra0,rcontext(tcr.save0))
	__(movd tsp_frame.data_offset+(2*node_size)(%imm0),%mm1)
	__(movd %nargs,%mm2)
	__(addl $tsp_frame.fixed_overhead+(node_size*3),%imm0)
	__(jmp local_label(_nthrowv_tpoptest))
local_label(_nthrowv_tpoploop):	
	__(push (%imm0))
	__(addl $node_size,%imm0)
local_label(_nthrowv_tpoptest):	
	__(subl $node_size,%nargs)
	__(jns local_label(_nthrowv_tpoploop))
	__(movd %mm2,%nargs)
	__(movl rcontext(tcr.save_tsp),%imm0)
	__(movl (%imm0),%imm0)
        __(movl %imm0,rcontext(tcr.save_tsp))
        __(movl %imm0,rcontext(tcr.next_tsp))
	__(movd %mm1,%imm0)
	__(jmp local_label(_nthrowv_nextframe))
local_label(_nthrowv_done):
	__(movb $0,rcontext(tcr.unwinding))
	__(check_pending_interrupt(%imm0))
local_label(_nthrowv_return):
	__(movl rcontext(tcr.save0),%ra0)
	__(movss %fpzero,rcontext(tcr.save0))
	__(jmp *%ra0)	
_endsubp(nthrowvalues)

/* This is a (slight) optimization.  When running an unwind-protect,  */
/* save the single value and the throw count in the tstack frame.  */
/* Note that this takes a single value in arg_z.  */

_spentry(nthrow1value)
	__(movb $1,rcontext(tcr.unwinding))
local_label(_nthrow1v_nextframe):
	__(subl $fixnumone,%imm0)
	__(js local_label(_nthrow1v_done))
	__(movd %imm0,%mm0)
	__(movl rcontext(tcr.catch_top),%temp1)
	__(movl catch_frame.link(%temp1),%imm0)
	__(movl %imm0,rcontext(tcr.catch_top))
	__(movl catch_frame.db_link(%temp1),%imm0)
	__(cmpl %imm0,rcontext(tcr.db_link))
	__(jz local_label(_nthrow1v_dont_unbind))
	__(push %temp1)
	__(push %temp0)
	__(push %arg_z)
	__(push `$'local_label(_nthrow1v_back_from_unbind))
	__(jmp _SPunbind_to)
__(tra(local_label(_nthrow1v_back_from_unbind)))
	__(pop %arg_z)
	__(pop %temp0)
	__(pop %temp1)
local_label(_nthrow1v_dont_unbind):
	__(cmpb $unbound_marker,catch_frame.catch_tag(%temp1))
	__(je local_label(_nthrow1v_do_unwind))
/* A catch frame.  If the last one, restore context from there. */
	__(movd %mm0,%imm0)
	__(test %imm0,%imm0)	/* last catch frame? */
	__(jne local_label(_nthrow1v_skip))
	__(movl catch_frame.xframe(%temp1),%arg_y)
	__(movl %arg_y,rcontext(tcr.xframe))
	__(movl catch_frame.esp(%temp1),%esp)
	__(movl catch_frame.ebp(%temp1),%ebp)
	__(movd catch_frame.foreign_sp(%temp1),%stack_temp)
	__(movd %stack_temp,rcontext(tcr.foreign_sp))
local_label(_nthrow1v_skip):
	__(movl -(tsp_frame.fixed_overhead+fulltag_misc)(%temp1),%imm0)
	__(movl %imm0,rcontext(tcr.save_tsp))
	__(movl %imm0,rcontext(tcr.next_tsp))
	__(movd %mm0,%imm0)
	__(jmp local_label(_nthrow1v_nextframe))
local_label(_nthrow1v_do_unwind):
/* This is harder, but not as hard (not as much BLTing) as the */
/* multiple-value case. */
	__(movl catch_frame.xframe(%temp1),%arg_y)
	__(movl %arg_y,rcontext(tcr.xframe))
	__(movl catch_frame.ebp(%temp1),%ebp)
	__(movl catch_frame.esp(%temp1),%esp)
	__(movd catch_frame.foreign_sp(%temp1),%stack_temp)
	__(movd %stack_temp,rcontext(tcr.foreign_sp))
	/* Discard the catch frame so we can build a temp frame. */
	__(movl -(tsp_frame.fixed_overhead+fulltag_misc)(%temp1),%imm0)
	__(movl %imm0,rcontext(tcr.save_tsp))
	__(movl %imm0,rcontext(tcr.next_tsp))
	__(movl catch_frame.pc(%temp1),%xfn) /* xfn is temp1 */
	__(TSP_Alloc_Fixed((3*node_size),%imm0))
	__(addl $tsp_frame.fixed_overhead,%imm0)
	__(movl %ra0,(%imm0))
	__(movd %mm0,node_size*1(%imm0))
	__(movl %arg_z,node_size*2(%imm0))
/* Ready to call cleanup code.  Set up tra, jmp to %xfn. */
	__(push $local_label(_nthrow1v_called_cleanup))
	__(movb $0,rcontext(tcr.unwinding))
	__(jmp *%xfn)
__(tra(local_label(_nthrow1v_called_cleanup)))
	__(movb $1,rcontext(tcr.unwinding))
	__(movl rcontext(tcr.save_tsp),%imm0)
	__(movl tsp_frame.data_offset+(0*node_size)(%imm0),%ra0)
	__(movd tsp_frame.data_offset+(1*node_size)(%imm0),%mm0)
	__(movl tsp_frame.data_offset+(2*node_size)(%imm0),%arg_z)
	__(movl (%imm0),%imm0)
	__(movl %imm0,rcontext(tcr.save_tsp))
	__(movl %imm0,rcontext(tcr.next_tsp))
	__(movd %mm0,%imm0)
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
	__(movl symbol.binding_index(%arg_y),%imm0)
	__(cmpl rcontext(tcr.tlb_limit),%imm0)
	__(jb 0f)
	__(push %imm0)
	__(tlb_too_small())
0:	__(test %imm0,%imm0)
	__(jz 9f)
	__(movl rcontext(tcr.tlb_pointer),%temp1)
	__(push (%temp1,%imm0))
	__(push %imm0)
	__(push rcontext(tcr.db_link))
	__(movl %esp,rcontext(tcr.db_link))
	__(movl %arg_z,(%temp1,%imm0))
	__(jmp *%ra0)
9:	
	__(movl %arg_y,%arg_z)
	__(movl $XSYMNOBIND,%arg_y)
	__(set_nargs(2))
        __(push %ra0)
	__(jmp _SPksignalerr)
_endsubp(bind)

/* arg_z = symbol: bind it to its current value  */

_spentry(bind_self)
	__(movl symbol.binding_index(%arg_z),%imm0)
	__(cmpl rcontext(tcr.tlb_limit),%imm0)
	__(jb 0f)
	__(push %imm0)
	__(tlb_too_small())
0:	__(test %imm0,%imm0)
	__(jz 9f)
	__(movl rcontext(tcr.tlb_pointer),%temp1)
	__(cmpb $no_thread_local_binding_marker,(%temp1,%imm0))
	__(jz 2f)
	__(push (%temp1,%imm0))
	__(push %imm0)
	__(push rcontext(tcr.db_link))
	__(movl %esp,rcontext(tcr.db_link))
	__(jmp *%ra0)
2:	__(movl symbol.vcell(%arg_z),%arg_y)
	__(push (%temp1,%imm0))
	__(push %imm0)
	__(push rcontext(tcr.db_link))
	__(movl %arg_y,(%temp1,%imm0))
	__(movl %esp,rcontext(tcr.db_link))
	__(jmp *%ra0)
9:	__(movl $XSYMNOBIND,%arg_y)
	__(set_nargs(2))
        __(push %ra0)
	__(jmp _SPksignalerr)
_endsubp(bind_self)

_spentry(bind_nil)
	__(movl symbol.binding_index(%arg_z),%imm0)
	__(cmpl rcontext(tcr.tlb_limit),%imm0)
	__(jb 0f)
	__(push %imm0)
	__(tlb_too_small())
0:	__(test %imm0,%imm0)
	__(jz 9f)
	__(movl rcontext(tcr.tlb_pointer),%temp1)
	__(push (%temp1,%imm0))
	__(push %imm0)
	__(push rcontext(tcr.db_link))
	__(movl %esp,rcontext(tcr.db_link))
	__(movl $nil_value,(%temp1,%imm0))
	__(jmp *%ra0)
9:	__(movl $XSYMNOBIND,%arg_y)
	__(set_nargs(2))
        __(push %ra0)
	__(jmp _SPksignalerr)
_endsubp(bind_nil)

_spentry(bind_self_boundp_check)
	__(movl symbol.binding_index(%arg_z),%imm0)
	__(cmpl rcontext(tcr.tlb_limit),%imm0)
	__(jb 0f)
	__(push %imm0)
	__(tlb_too_small())
0:	__(test %imm0,%imm0)
	__(jz 9f)
	__(movl rcontext(tcr.tlb_pointer),%temp1)
	__(cmpb $no_thread_local_binding_marker,(%temp1,%imm0))
	__(je 2f)
	__(cmpb $unbound_marker,(%temp1,%imm0))
	__(je 8f)
	__(push (%temp1,%imm0))
	__(push %imm0)
	__(push rcontext(tcr.db_link))
	__(movl %esp,rcontext(tcr.db_link))
	__(jmp *%ra0)
2:	__(movl symbol.vcell(%arg_z),%arg_y)
	__(cmpl $unbound_marker,%arg_y)
	__(jz 8f)
	__(push (%temp1,%imm0))
	__(push %imm0)
	__(push rcontext(tcr.db_link))
	__(movl %esp,rcontext(tcr.db_link))
	__(movl %arg_y,(%temp1,%imm0))
	__(jmp *%ra0)
8:	__(push %ra0)
        __(uuo_error_reg_unbound(Rarg_z))
	
9:	__(movl $XSYMNOBIND,%arg_y)
	__(set_nargs(2))
        __(push %ra0)
	__(jmp _SPksignalerr)
_endsubp(bind_self_boundp_check)

_spentry(conslist)
	__(movl %nargs,%imm0)
	__(movl %temp0,%temp1)	/* have to use temp0 for consing */
	__(movl $nil_value,%arg_z)
	__(test %imm0,%imm0)
	__(jmp 2f)
1:	__(pop %arg_y)
	__(Cons(%arg_y,%arg_z,%arg_z))
	__(subl $node_size,%imm0)
2:	__(jnz 1b)
	__(jmp *%temp1)
_endsubp(conslist)

/* do list*: last arg in arg_z, all others pushed, nargs set to #args pushed.  */
/* Cons, one cons cell at at time.  Maybe optimize this later.  */

_spentry(conslist_star)
	__(movl %nargs,%imm0)
	__(test %imm0,%imm0)
	__(movl %ra0,%temp1)
	__(jmp 2f)
1:	__(pop %arg_y)
	__(Cons(%arg_y,%arg_z,%arg_z))
	__(subl $node_size,%imm0)
2:	__(jnz 1b)
	__(jmp *%temp1)
_endsubp(conslist_star)

/* We always have to create a tsp frame (even if nargs is 0), so the compiler */
/* doesn't get confused. */
_spentry(stkconslist)
	__(movl $nil_value,%arg_z)
C(stkconslist_common):               
	__(movl %ra0,rcontext(tcr.save0))
	__(movd %nargs,%mm0)
	__(movl %nargs,%temp0)
	__(addl %temp0,%temp0)
	__(dnode_align(%temp0,tsp_frame.fixed_overhead,%temp0))
	__(TSP_Alloc_Var(%temp0,%imm0))
	__(addl $fulltag_cons,%imm0)
	__(test %nargs,%nargs)
	__(jmp 2f)
1:	__(pop %arg_y)
	__(_rplaca(%imm0,%arg_y))
	__(_rplacd(%imm0,%arg_z))
	__(movl %imm0,%arg_z)
	__(add $cons.size,%imm0)
	__(subl $node_size,%nargs)
2:	__(jne 1b)
	__(movl rcontext(tcr.save0),%ra0)
	__(movl $0,rcontext(tcr.save0))
	__(jmp *%ra0)
_endsubp(stkconslist)

/* do list*: last arg in arg_z, all others vpushed,   */
/*	nargs set to #args vpushed.  */

_spentry(stkconslist_star)
        __(jmp C(stkconslist_common))
_endsubp(stkconslist_star)


/* Make a stack-consed simple-vector out of the NARGS objects   */
/*	on top of the vstack; return it in arg_z.  */

_spentry(mkstackv)
	__(dnode_align(%nargs,tsp_frame.fixed_overhead+node_size,%imm0))
	__(TSP_Alloc_Var(%imm0,%arg_y))
	__(movl %nargs,%imm0)
	__(shll $(num_subtag_bits-fixnumshift),%imm0)
	__(movb $subtag_simple_vector,%imm0_b)
	__(movl %imm0,(%arg_y))
	__(leal fulltag_misc(%arg_y),%arg_z)
	__(test %nargs,%nargs)
	__(leal misc_data_offset(%arg_z,%nargs),%imm0)
	__(jmp 2f)
1:	__(pop -node_size(%imm0))
	__(subl $node_size,%nargs)
	__(leal -node_size(%imm0),%imm0)
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
	__(rcmpl(%arg_z,%arg_y))
	__(_rplaca(%arg_y,%arg_z))
	__(ja 1f)
0:	__(repret)
1:	__(movl %arg_y,%imm0)
	__(subl lisp_global(ref_base),%imm0)
	__(shrl $dnode_shift,%imm0)
	__(cmpl lisp_global(oldspace_dnode_count),%imm0)
	__(jae 0b)
	__(ref_global(refbits,%temp0))
	__(xorb $31,%imm0_b)
	__(lock)
	__(btsl %imm0,(%temp0))
	__(ret)
_endsubp(rplaca)

_spentry(rplacd)
        .globl C(egc_rplacd)
C(egc_rplacd):
	__(rcmpl(%arg_z,%arg_y))
	__(_rplacd(%arg_y,%arg_z))
	__(ja 1f)
0:	__(repret)
1:	__(movl %arg_y,%imm0)
	__(subl lisp_global(ref_base),%imm0)
	__(shrl $dnode_shift,%imm0)
	__(cmpl lisp_global(oldspace_dnode_count),%imm0)
	__(jae 0b)
	__(ref_global(refbits,%temp0))
	__(xorb $31,%imm0_b)
	__(lock)
	__(btsl %imm0,(%temp0))
	__(ret)
_endsubp(rplacd)

/* Storing into a gvector can be handled the same way as storing into a CONS. */
/* args (src, unscaled-idx, val) in temp0, arg_y, arg_z */
_spentry(gvset)
        .globl C(egc_gvset)
C(egc_gvset):
	__(movl %arg_z,misc_data_offset(%temp0,%arg_y))
	__(rcmpl(%arg_z,%temp0))
	__(ja 1f)
0:	__(repret)
1:	__(lea misc_data_offset(%temp0,%arg_y),%imm0)
	__(subl lisp_global(ref_base),%imm0)
	__(shrl $dnode_shift,%imm0)
	__(cmpl lisp_global(oldspace_dnode_count),%imm0)
	__(jae 0b)
	__(ref_global(refbits,%temp1))
	__(xorb $31,%imm0_b)
	__(lock)
	__(btsl %imm0,(%temp1))
	__(ret)
_endsubp(gvset)

/* This is a special case of storing into a gvector: if we need to  */
/* memoize the store, record the address of the hash-table vector  */
/* in the refmap, as well.  */

_spentry(set_hash_key)
        .globl C(egc_set_hash_key)
C(egc_set_hash_key):
	__(movl %arg_z,misc_data_offset(%temp0,%arg_y))
	__(rcmpl(%arg_z,%temp0))
	__(ja 1f)
0:	__(repret)
1:	__(lea misc_data_offset(%temp0,%arg_y),%imm0)
	__(subl lisp_global(ref_base),%imm0)
	__(shrl $dnode_shift,%imm0)
	__(cmpl lisp_global(oldspace_dnode_count),%imm0)
	__(jae 0b)
	__(ref_global(refbits,%temp1))
	__(xorb $31,%imm0_b)
	__(lock)
	__(btsl %imm0,(%temp1))
	/* Now memoize the address of the hash vector */
	__(movl %temp0,%imm0)
	__(subl lisp_global(ref_base),%imm0)
	__(shrl $dnode_shift,%imm0)
	__(xorb $31,%imm0_b)
	__(lock)
	__(btsl %imm0,(%temp1))
	__(ret)
_endsubp(set_hash_key)

/* This is a little trickier: if this is interrupted, we need to know  */
/* whether or not the STORE-CONDITIONAL (cmpxchgq) has won or not.    */
/* If we're interrupted   before the PC has reached the "success_test" label, */
/* repeat (luser the PC back to store_node_conditional_retry.)  If
	we're at that */
/* label with the Z flag set, we won and (may) need to memoize.  */

/* %temp0 = offset, %temp1 = object, %arg_y = old, %arg_z = new */
_spentry(store_node_conditional)
        .globl C(egc_store_node_conditional)
C(egc_store_node_conditional):
	__(subl $misc_data_offset*fixnumone,%temp0) /* undo pre-added offset */
	__(sarl $fixnumshift,%temp0)	/* will be fixnum-tagged */
        .globl C(egc_store_node_conditional_retry)
C(egc_store_node_conditional_retry):      
0:	__(cmpl %arg_y,misc_data_offset(%temp1,%temp0))
	__(movl misc_data_offset(%temp1,%temp0),%imm0)
	__(jne 3f)
	__(lock)
	__(cmpxchgl %arg_z,misc_data_offset(%temp1,%temp0))
	.globl C(egc_store_node_conditional_success_test)
C(egc_store_node_conditional_success_test):
	__(jne 0b)
	__(leal misc_data_offset(%temp1,%temp0),%imm0)
	__(subl lisp_global(ref_base),%imm0)
	__(shrl $dnode_shift,%imm0)
	__(cmpl lisp_global(oldspace_dnode_count),%imm0)
	__(jae 2f)
	__(ref_global(refbits,%arg_y))
	__(xorb $31,%imm0_b)
	__(lock)
	__(btsl %imm0,(%arg_y))
        .globl C(egc_store_node_conditional_success_end)
C(egc_store_node_conditional_success_end):
2:	__(movl $t_value,%arg_z)
	__(ret)
3:	__(movl $nil_value,%arg_z)
	__(ret)
_endsubp(store_node_conditional)

	/* %temp0 = offset, %temp1 = object, %arg_y = old, %arg_z = new */
_spentry(set_hash_key_conditional)
        .globl C(egc_set_hash_key_conditional)
C(egc_set_hash_key_conditional):
	__(subl $misc_data_offset*fixnumone,%temp0) /* undo pre-added offset */
	__(sarl $fixnumshift,%temp0)	/* will be fixnum-tagged */
        .globl C(egc_set_hash_key_conditional_retry)
C(egc_set_hash_key_conditional_retry):          
0:	__(cmpl %arg_y,misc_data_offset(%temp1,%temp0))
	__(movl misc_data_offset(%temp1,%temp0),%imm0)
	__(jne 3f)
	__(lock)
	__(cmpxchgl %arg_z,misc_data_offset(%temp1,%temp0))
	.globl C(egc_set_hash_key_conditional_success_test)
C(egc_set_hash_key_conditional_success_test):
	__(jne 0b)
	__(leal misc_data_offset(%temp1,%temp0),%imm0)
	__(subl lisp_global(ref_base),%imm0)
	__(shrl $dnode_shift,%imm0)
	__(cmpl lisp_global(oldspace_dnode_count),%imm0)
	__(jae 2f)
	__(ref_global(refbits,%arg_y))
	__(xorb $31,%imm0_b)
	__(lock)
	__(btsl %imm0,(%arg_y))
	/* Now memoize the address of the hash vector */
	__(movl %temp1,%imm0)
	__(subl lisp_global(ref_base),%imm0)
	__(shrl $dnode_shift,%imm0)
	__(xorb $31,%imm0_b)
	__(lock)
	__(btsl %imm0,(%arg_y))
        .globl C(egc_write_barrier_end)
C(egc_write_barrier_end):
2:	__(movl $t_value,%arg_z)
	__(ret)
3:	__(movl $nil_value,%arg_z)
	__(ret)
_endsubp(store_node_conditional)

_spentry(setqsym)
	__(bt $sym_vbit_const,symbol.flags(%arg_y))
	__(jae _SPspecset)
	__(mov %arg_y,%arg_z)
	__(mov $XCONST,%arg_y)
	__(set_nargs(2))
	__(jmp _SPksignalerr)
_endsubp(setqsym)

_spentry(progvsave)
	__(push %arg_y)
	
	/* Error if arg_z isn't a proper list.  That's unlikely,  */
	/* but it's better to check now than to crash later.  */
	
	__(compare_reg_to_nil(%arg_z))
	__(movl %arg_z,%temp0)	/* fast   */
	__(movl %arg_z,%temp1)	/* slow   */
	__(je 9f)		/* Null list is proper   */
0:
	__(extract_lisptag(%temp0,%imm0))
	__(cmpb $tag_list,%imm0_b)
	__(jne 8f)
	__(compare_reg_to_nil(%temp0))
	__(je 9f)
	__(_cdr(%temp0,%arg_y))	/* (null (cdr fast)) ?   */
	__(compare_reg_to_nil(%arg_y))
	__(je 9f)
	__(extract_lisptag(%arg_y,%imm0))
	__(cmpb $tag_list,%imm0_b)
	__(jne 8f)
	__(_cdr(%arg_y,%temp0))
	__(_cdr(%temp1,%temp1))
	__(cmpl %temp1,%temp0)
	__(jne 0b)

8:	__(add $node_size,%esp)	/* discard pushed arg_y */
	__(movl $XIMPROPERLIST,%arg_y)
	__(set_nargs(2))
	__(jmp _SPksignalerr)
9:	/* Whew 	  */

        /* Next, determine the length of arg_y.  We   */
	/* know that it's a proper list.   */
	__(pop %arg_y)
	
	__(movl $-fixnumone,%imm0)
	__(movl %arg_y,%temp0)
1:	__(compare_reg_to_nil(%temp0))
	__(_cdr(%temp0,%temp0))
	__(leal fixnumone(%imm0),%imm0)
	__(jne 1b)
	
	/* imm0 is now (boxed) triplet count.  */
	/* Determine word count, add 1 (to align), and make room.  */
	/*  if count is 0, make an empty tsp frame and exit   */
	__(testl %imm0,%imm0)
	__(jne 2f)
	__(TSP_Alloc_Fixed(2*node_size,%imm0))
	__(ret)
2:	__(movl %imm0,%temp1)
	__(add %imm0,%imm0)
	__(add %temp1,%imm0)
	__(dnode_align(%imm0,tsp_frame.fixed_overhead+node_size,%imm0))
	__(TSP_Alloc_Var(%imm0,%temp0))
	__(movl %temp1,(%temp0))
	__(movd rcontext(tcr.db_link),%mm0)
3:	__(movl $unbound_marker,%temp0)
	__(compare_reg_to_nil(%arg_z))
	__(cmovnel cons.car(%arg_z),%temp0)
	__(cmovnel cons.cdr(%arg_z),%arg_z)
	__(_car(%arg_y,%temp1))
	__(_cdr(%arg_y,%arg_y))
	__(movl symbol.binding_index(%temp1),%temp1)
	__(cmp rcontext(tcr.tlb_limit),%temp1)
	__(jb 4f)
	__(push %temp1)
	__(tlb_too_small())
4:	__(push %arg_z)
	__(movl rcontext(tcr.tlb_pointer),%arg_z)
	__(subl $binding.size,%imm0)
	__(movl %temp1,binding.sym(%imm0))
	__(push (%arg_z,%temp1))
	__(pop binding.val(%imm0))
	__(movl %temp0,(%arg_z,%temp1))
	__(pop %arg_z)
	__(movd %mm0,binding.link(%imm0))
	__(movd %imm0,%mm0)
	__(compare_reg_to_nil(%arg_y))
	__(jne 3b)
	__(movd %mm0,rcontext(tcr.db_link))
	__(ret)
_endsubp(progvsave)

/* Allocate node objects on the temp stack, immediate objects on the foreign  */
/* stack. (The caller has to know which stack to discard a frame from.)  */
/* %arg_y = boxed element-count, %arg_z = boxed subtype  */

_spentry(stack_misc_alloc)
	__(testl $~(((1<<24)-1)<<fixnumshift),%arg_y)
	__(jne local_label(stack_misc_alloc_not_u24))
	__(unbox_fixnum(%arg_z,%imm0))
	__(mov %arg_y,%temp0)
	__(shl $num_subtag_bits-fixnumshift,%temp0)
	__(or %temp0,%imm0)	/* %imm0 now = header */
	__(movd %imm0,%mm0)	/* cache header in %mm0 */
	__(andb $fulltagmask,%imm0_b)
	__(cmpb $fulltag_nodeheader,%imm0_b)
	__(je local_label(stack_misc_alloc_node))
	__(movd %mm0,%imm0)
	__(cmpb $max_32_bit_ivector_subtag,%imm0_b)
	__(jbe local_label(stack_misc_alloc_32))
	__(cmpb $max_8_bit_ivector_subtag,%imm0_b)
	__(jbe local_label(stack_misc_alloc_8))
	__(cmpb $max_16_bit_ivector_subtag,%imm0_b)
	__(jbe local_label(stack_misc_alloc_16))
	__(cmpb $subtag_double_float_vector,%imm0_b)
	__(jne local_label(stack_misc_alloc_1))
	/* double-float vector case */
	__(imul $2,%arg_y,%imm0)
	__(jmp local_label(stack_misc_alloc_alloc_ivector))
local_label(stack_misc_alloc_1):
	__(unbox_fixnum(%arg_y,%imm0))
	__(addl $7,%imm0)
	__(shrl $3,%imm0)
	__(jmp local_label(stack_misc_alloc_alloc_ivector))
local_label(stack_misc_alloc_8):
	__(unbox_fixnum(%arg_y,%imm0))
	__(jmp local_label(stack_misc_alloc_alloc_ivector))
local_label(stack_misc_alloc_16):
	__(unbox_fixnum(%arg_y,%imm0))
	__(shl $1,%imm0)
	__(jmp local_label(stack_misc_alloc_alloc_ivector))
local_label(stack_misc_alloc_32):
	__(mov %arg_y,%imm0)
local_label(stack_misc_alloc_alloc_ivector):
	/* byte count in %imm0 */
	__(dnode_align(%imm0,tsp_frame.fixed_overhead+node_size,%imm0))
	__(cmpl $tstack_alloc_limit,%imm0)
	__(ja local_label(stack_misc_alloc_heap_alloc_ivector))
        __ifdef(`WINDOWS')
         __(windows_cstack_probe(%imm0,%temp1))
        __endif
	__(movd rcontext(tcr.foreign_sp),%stack_temp)
	__(movd %stack_temp,%temp1)
	__(subl %imm0,rcontext(tcr.foreign_sp))
	__(movl rcontext(tcr.foreign_sp),%temp0)
0:	__(movsd %fpzero,-dnode_size(%temp1))
	__(subl $dnode_size,%temp1)
	__(cmpl %temp1,%temp0)
	__(jnz 0b)
	__(movd %stack_temp,(%temp0))
	__(movl %ebp,csp_frame.save_ebp(%temp0))
	__(movd %mm0,csp_frame.fixed_overhead(%temp0))
	__(lea csp_frame.fixed_overhead+fulltag_misc(%temp0),%arg_z)
	__(ret)
local_label(stack_misc_alloc_heap_alloc_ivector):
	__(movd rcontext(tcr.foreign_sp),%stack_temp)
	__(subl $dnode_size,rcontext(tcr.foreign_sp))
	__(movl rcontext(tcr.foreign_sp),%imm0)
	__(movd %stack_temp,(%imm0))
	__(jmp _SPmisc_alloc)
local_label(stack_misc_alloc_node):
	__(movl %arg_y,%imm0)
	__(dnode_align(%imm0,tsp_frame.fixed_overhead+node_size,%imm0))
	__(cmpl $tstack_alloc_limit,%imm0)
	__(ja local_label(stack_misc_alloc_heap_alloc_gvector))
	__(TSP_Alloc_Var(%imm0,%temp1))
	__(movd %mm0,(%temp1))
	__(leal fulltag_misc(%temp1),%arg_z)
	__(ret)
local_label(stack_misc_alloc_heap_alloc_gvector):
	__(TSP_Alloc_Fixed(0,%imm0))
	__(jmp _SPmisc_alloc)

local_label(stack_misc_alloc_not_u24):
	__(uuo_error_reg_not_type(Rarg_y,error_object_not_unsigned_byte_24))
_endsubp(stack_misc_alloc)

/* subtype (boxed, of course) is pushed, followed by nargs bytes worth of */
/* initial-contents.  Note that this can be used to cons any type of */
/* initialized node-header'ed misc object (symbols, closures, ...) */
/* as well as vector-like objects. */
_spentry(gvector)
	__(subl $node_size,%nargs)	/* off by one in x862-%gvector */
	__(movl (%esp,%nargs),%imm0)	/* boxed subtype */
	__(sarl $fixnumshift,%imm0)
	__(movl %nargs,%arg_z)
	__(shll $num_subtag_bits-word_shift,%arg_z)
	__(orl %arg_z,%imm0)
	__(movd %imm0,%mm0)
	__(dnode_align(%nargs,node_size,%imm0))
	__(push %ra0)	/* aka %temp0, can't be live while consing */
	__(Misc_Alloc(%arg_z))
	__(pop %ra0)
	__(movl %nargs,%imm0)
	__(jmp 2f)
1:	__(movl %arg_y,misc_data_offset(%arg_z,%imm0))
2:	__(subl $node_size,%imm0)
	__(pop %arg_y)	/* Note the intentional fencepost: */
			/* discard the subtype as well. */
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
	__(movl (%temp0),%arg_y)	/* return address */
	__(ref_global(ret1val_addr,%imm0))
	__(movl $nil_value,%arg_z)
	__(cmpl %imm0,%arg_y)
	__(je 0f)
	__(test %nargs,%nargs)
	__(cmovne -node_size(%esp,%nargs),%arg_z)
	__(movl %temp0,%esp)
	__(ret)
0:	__(movl 4(%temp0),%arg_y)
        __(addl $2*node_size,%temp0)
	__(lea (%esp,%nargs),%imm0)
	__(movd %nargs,%mm0)
	__(jmp 2f)
1:	__(subl $node_size,%imm0)
	__(movl (%imm0),%temp1)
	__(subl $node_size,%temp0)
	__(movl %temp1,(%temp0))
2:	__(cmp %imm0,%esp)
	__(jne 1b)
	__(movl %temp0,%esp)
	__(movd %mm0,%nargs)
	__(jmp *%arg_y)

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
	__(movl %temp0,%arg_y)
	__(movl %nargs,%imm0)
	__(testl %imm0,%imm0)
	__(movl $nil_value,%arg_z)
	__(jmp 2f)
	.p2align 4
1:	__(pop %temp1)
	__(Cons(%temp1,%arg_z,%arg_z))
	__(subl $node_size,%imm0)
2:	__(jg 1b)
	__(push %arg_z)
	__(movl %arg_y,%temp0)
	__(jmp *%ra0)

_endsubp(heap_rest_arg)

/* %imm0 contains the number of fixed args; make an &rest arg out of */
/* the others. */
_spentry(req_heap_rest_arg)
	__(push_argregs())
	__(movd %nargs,%mm0)
	__(subl %imm0,%nargs)
	__(movl %nargs,%imm0)
	__(movl %temp0,%temp1)
	__(movl $nil_value,%arg_z)
	__(jmp 2f)
	.p2align 4
1:	__(pop %arg_y)
	__(Cons(%arg_y,%arg_z,%arg_z))
	__(subl $node_size,%imm0)
2:	__(jg 1b)
	__(push %arg_z)
	__(movl %temp1,%temp0)
	__(movd %mm0,%nargs)
	__(jmp *%ra0)
_endsubp(req_heap_rest_arg)

/* %imm0 bytes of stuff has already been pushed	  */
/* make an &rest arg out of any others   */
_spentry(heap_cons_rest_arg)
	__(movd %nargs,%mm0)
	__(subl %imm0,%nargs)
	__(movl %nargs,%imm0)
	__(movl $nil_value,%arg_z)
	__(movl %ra0,%arg_y)	/* temp0 can't be live while consing */
	__(jmp 2f)		/* (did I mention that already?) */
	.p2align 4
1:	__(pop %temp1)
	__(Cons(%temp1,%arg_z,%arg_z))
	__(subl $node_size,%imm0)
2:	__(jg 1b)
	__(push %arg_z)
	__(movd %mm0,%nargs)
	__(jmp *%arg_y)
_endsubp(heap_cons_rest_arg)

_spentry(simple_keywords)
	__(xor %imm0,%imm0)
	__(push_argregs())
	__(jmp _SPkeyword_bind)
_endsubp(simple_keywords)

_spentry(keyword_args)
	__(push_argregs())
	__(jmp _SPkeyword_bind)
_endsubp(keyword_args)

/* There are %nargs words of arguments on the stack; %imm0 contains the */
/* number of non-keyword args pushed.  It's possible that we never actually */
/* got any keyword args, which would make things much simpler. */

/* On entry, the upper half of %temp1 (aka %nargs) contains some bits */
/* indicating whether &allow-other-keys and/or &rest was present in the */
/* lambda list. */

/* Once we get here, we can use the arg registers. */

/* N.B.: %ra0 is %temp0, and must not be clobbered. */

define(`keyword_flags_aok_bit',`16')
define(`keyword_flags_unknown_keys_bit',`17')
define(`keyword_flags_rest_bit',`18')
define(`keyword_flags_seen_aok_bit',`19')

_spentry(keyword_bind)
	__(movl %temp1,rcontext(tcr.unboxed0))	/* save keyword flags */
	__(movzwl %nargs_w,%nargs)
	__(movl %nargs,%arg_z)
	__(subl %imm0,%arg_z)
	__(jbe local_label(no_keyword_values))
	__(btl $word_shift,%arg_z)
	__(jnc local_label(even))
	__(movl $nil_value,%arg_y)
	__(movl %arg_z,%nargs)
	__(test %nargs,%nargs)
	__(movl %ra0,rcontext(tcr.save0))	/* save temp0 while consing */
	__(jmp 1f)
0:	__(pop %arg_z)
	__(Cons(%arg_z,%arg_y,%arg_y))
	__(subl $node_size,%nargs)
1:	__(jnz 0b)
	__(movl rcontext(tcr.save0),%ra0)
	__(movapd %fpzero,rcontext(tcr.save0))
	__(movl %arg_y,%arg_z)
	__(movl $XBADKEYS,%arg_y)
	__(set_nargs(2))
	__(jmp _SPksignalerr)

	/* Now that we're sure that we have an even number of */
	/* keywords and values (in %arg_z), move the pairs over */
	/* to the temp stack. */
local_label(even):
	__(lea tsp_frame.fixed_overhead(%arg_z),%arg_y)
	__(TSP_Alloc_Var(%arg_y,%imm0))
2:	__(subl $node_size,%arg_y)
	__(pop (%arg_y))
	__(cmpl %arg_y,%imm0)
	__(jne 2b)

	/* Get the keyword vector into %arg_y, and its length into %imm0. */
	/* Push %imm0 pairs of NILs (representing value, supplied-p) */
	/* for each declared keyword. */
	__(movzwl misc_data_offset(%fn),%imm0)
	/* XXX bootstrapping */
	__(btr $15,%imm0)
	__(jnc 0f)
	__(vector_length(%fn,%arg_y))
	__(box_fixnum(%imm0,%imm0))
	__(subl %imm0,%arg_y)
	__(movl %arg_y,%imm0)
	__(shrl $fixnumshift,%imm0)
0:
	__(movl misc_data_offset(%fn,%imm0,node_size),%arg_y)
	__(vector_length(%arg_y,%imm0))
	__(jmp 4f)
3:	__(push $nil_value)
	__(push $nil_value)
4:	__(subl $fixnumone,%imm0)
	__(jge 3b)

	/* We can now push %ra0 (aka %temp0) and %nargs (aka %temp1) */
	/* in order to get a couple more registers to work with. */
	__(push %ra0)
	__(push %nargs)

	/* At this point we have: */
	/* number of supplied keywords and values in %arg_z */
	/* keyword vector in %arg_y */
	__(vector_length(%arg_y,%imm0))
	__(push %imm0)		/* count of declared keywords */
	__(push %arg_z)		/* count of supplied keys and values */

	/* For each declared keyword, iterate over the supplied k/v pairs */
	/* to see if it's supplied and what the value is. */
	/* checking to see if any */
	/* key-value pairs were unexpectedly supplied. */

	__(movl rcontext(tcr.save_tsp),%temp0)
	__(addl $2*node_size,%temp0) /* skip frame overhead */
	/* %temp0: top of tstack (skipping frame overhead) */
	__(lea 4*node_size(%esp,%imm0,2),%temp1)
	/* %temp1: word above 0th value/supplied-p pair on vstack */
	/* %arg_y: keyword vector */
	__(xorl %imm0,%imm0)
	/* %imm0: index */
	/* %arg_z: temporary */

	/* Iterate over supplied k/v pairs on tstack.  See if key is */
	/* in the keyword vector.  Copy value and set supplied-p on */
	/* vstack if found. */

local_label(tstack_loop):
	__(movl (%temp0,%imm0,2),%arg_z)	/* keyword */
	__(push %imm0)
	__(xorl %imm0,%imm0)
	__(cmpl $nrs.kallowotherkeys,%arg_z)
	__(jne local_label(next_keyvect_entry))
	__(btsl $keyword_flags_seen_aok_bit,rcontext(tcr.unboxed0))
	__(jc local_label(next_keyvect_entry))
	__(push %imm0)
	__(movl 4(%esp),%imm0)
	__(cmpl $nil_value,node_size(%temp0,%imm0,2))
	__(pop %imm0)
	__(je local_label(next_keyvect_entry))
	__(btsl $keyword_flags_aok_bit,rcontext(tcr.unboxed0))
	__(jmp local_label(next_keyvect_entry))
	/* loop through keyword vector */
6:	__(cmpl misc_data_offset(%arg_y,%imm0),%arg_z)
	__(jne 7f)
	/* Got a match; have we already seen this keyword? */
	__(negl %imm0)
	__(cmpl $nil_value,-node_size*2(%temp1,%imm0,2))
	__(jne 9f)	/* seen it, ignore this value */
	__(movl (%esp),%arg_z)
	__(lea (%temp0,%arg_z,2),%arg_z)
	__(movl node_size(%arg_z),%arg_z) /* value for this key */
	__(movl %arg_z,-node_size(%temp1,%imm0,2))
	__(movl $t_value,-node_size*2(%temp1,%imm0,2))
	__(jmp 9f)
7:	__(addl $node_size,%imm0)
local_label(next_keyvect_entry):
	__(cmpl %imm0,8(%esp))
	__(jne 6b)
	/* Didn't match anything in the keyword vector.  Is the keyword */
	/* :allow-other-keys? */
	__(cmpl $nrs.kallowotherkeys,%arg_z)
	__(je 9f)	/* :allow-other-keys is never "unknown" */
8:	__(btsl $keyword_flags_unknown_keys_bit,rcontext(tcr.unboxed0))
9:	__(pop %imm0)
	__(addl $fixnumone,%imm0)
	__(movl %imm0,%arg_z)
	__(shll $1,%arg_z)	/* pairs of tstack words */
	__(cmpl %arg_z,0(%esp))
	__(jne local_label(tstack_loop))

	__(pop %imm0)	/* count of supplied keys and values */
	__(addl $node_size,%esp)
	__(pop %nargs)
	__(pop %ra0)

	/* If the function takes an &rest arg, or if we got an unrecognized */
	/* keyword and don't allow that, copy the incoming k/v pairs from */
	/* the temp stack back to the value stack. */
	__(btl $keyword_flags_rest_bit,rcontext(tcr.unboxed0))
	__(jc 1f)
	__(btl $keyword_flags_unknown_keys_bit,rcontext(tcr.unboxed0))
	__(jnc 0f)
	__(btl $keyword_flags_aok_bit,rcontext(tcr.unboxed0))
	__(jnc 1f)
	/* pop the tstack frame */
0:	__(discard_temp_frame(%imm0))
	__(jmp *%ra0)

	/* Copy the k/v pairs from the tstack back to the value stack, */
	/* either because the function takes an &rest arg or because */
	/* we need to signal an "unknown keywords" error. */
1:	__(movl rcontext(tcr.save_tsp),%arg_z)
	__(mov (%arg_z),%arg_y)
	__(jmp 3f)
2:	__(push (%arg_z))
	__(push node_size(%arg_z))
3:	__(addl $dnode_size,%arg_z)
	__(cmpl %arg_z,%arg_y)
	__(jne 2b)
	__(discard_temp_frame(%arg_z))
	__(btl $keyword_flags_unknown_keys_bit,rcontext(tcr.unboxed0))
	__(jnc 9f)
	__(btl $keyword_flags_aok_bit,rcontext(tcr.unboxed0))
	__(jc 9f)
	/* Signal an "unknown keywords" error */
	__(movl %imm0,%nargs)
	__(movl $nil_value,%arg_z)
	__(test %nargs,%nargs)
	__(movl %ra0,rcontext(tcr.save0))
	__(jmp 5f)
4:	__(pop %arg_y)
	__(Cons(%arg_y,%arg_z,%arg_z))
	__(subl $node_size,%nargs)
5:	__(jnz 4b)
	__(movl $XBADKEYS,%arg_y)
	__(set_nargs(2))
	__(movl rcontext(tcr.save0),%ra0)
	__(movl $0,rcontext(tcr.save0))
	__(jmp _SPksignalerr)
9:	__(jmp *%ra0)

/* No keyword value were provided.  Access the keyword vector (which is the */
/* 0th constant in %fn), determine its length N, and push N pairs of NILs. */
/* N could be 0... */

local_label(no_keyword_values):
	__(movzwl misc_data_offset(%fn),%imm0)
	/* XXX bootstrapping */
	__(btr $15,%imm0)
	__(jnc 9f)
	__(vector_length(%fn,%arg_y))
	__(box_fixnum(%imm0,%imm0))
	__(subl %imm0,%arg_y)
	__(movl %arg_y,%imm0)
	__(shrl $fixnumshift,%imm0)
9:
	__(movl misc_data_offset(%fn,%imm0,node_size),%arg_y)
	__(vector_length(%arg_y,%arg_z))
	__(movl $nil_value,%imm0)
	__(jmp 1f)
0:	__(push %imm0)
	__(push %imm0)
1:	__(subl $fixnumone,%arg_z)
	__(jge 0b)
	__(jmp *%ra0)
_endsubp(keyword_bind)

/* Normally, we'd just set %fname (aka %temp0) and do */
/* jump_fname().  Sometimes, though, %temp0 is being used */
/* as %ra0, and I'm not sure that it's going to be safe to */
/* clobber that.  (Note that nil-relative symbols aren't going */
/* get moved around by the GC, so we can get away with putting */
/* '%err-disp in %imm0.) */
_spentry(ksignalerr)
	__(mov $nrs.errdisp,%imm0)
	__(mov symbol.fcell(%imm0),%fn)
	__(jump_fn)
_endsubp(ksignalerr)

_spentry(stack_rest_arg)
	__(xorl %imm0,%imm0)
	__(push_argregs())
	__(jmp _SPstack_cons_rest_arg)
_endsubp(stack_rest_arg)

_spentry(req_stack_rest_arg)
	__(push_argregs())
	__(jmp _SPstack_cons_rest_arg)
_endsubp(req_stack_rest_arg)

_spentry(stack_cons_rest_arg)
	__(movd %nargs,%mm2)
	__(movl %temp0,rcontext(tcr.save0))
	__(subl %imm0,%temp1)
	__(movl $nil_value,%arg_z)
	__(jle 2f)	/* empty list; make an empty TSP frame */
	__(addl %temp1,%temp1)
	__(cmpl $(tstack_alloc_limit-dnode_size),%temp1)
	__(ja 3f)	/* make empty frame, then heap-cons */
	__(dnode_align(%temp1,tsp_frame.fixed_overhead,%imm0))
	__(TSP_Alloc_Var(%imm0,%temp0))
	__(addl $fulltag_cons,%temp0)
1:	__(pop %arg_y)
	__(_rplacd(%temp0,%arg_z))
	__(_rplaca(%temp0,%arg_y))
	__(movl %temp0,%arg_z)
	__(addl $cons.size,%temp0)
	__(subl $dnode_size,%temp1)
	__(jne 1b)
	__(push %arg_z)
	__(movd %mm2,%nargs)
	__(movl rcontext(tcr.save0),%temp0)
	__(movss %fpzero,rcontext(tcr.save0))
	__(jmp *%temp0)
/* Length 0, make empty frame */
2:
	__(TSP_Alloc_Fixed(0,%temp0))
	__(push %arg_z)
	__(movd %mm2,%nargs)
	__(movl rcontext(tcr.save0),%temp0)
	__(movss %fpzero,rcontext(tcr.save0))
	__(jmp *%temp0)
/* Too big to stack-cons, but make an empty frame before heap-consing */
	__(TSP_Alloc_Fixed(0,%temp0))
	__(movd %mm2,%nargs)
	__(movl rcontext(tcr.save0),%temp0)
	__(movss %fpzero,rcontext(tcr.save0))
	__(jmp _SPheap_cons_rest_arg)
_endsubp(stack_cons_rest_arg)

_spentry(getxlong)
	__(hlt)
_endsubp(getxlong)

/* Have to be a little careful here: the caller may or may not have pushed  */
/* an empty frame, and we may or may not have needed one.  We can't easily  */
/* tell whether or not a frame will be needed (if the caller didn't reserve  */
/* a frame, whether or not we need one depends on the length of the list  */
/* in arg_z.  So, if the caller didn't push a frame, we do so; once */
/* everything's been spread, we discard the reserved frame (regardless of
/* who pushed it) if all args fit in registers.   */

/* xxx preserve temp1 somehow? cf. comment in x862-invoke-fn */
_spentry(spreadargz)
	__(test %nargs,%nargs)
	__(jne 0f)
	__(push $reserved_frame_marker)
	__(push $reserved_frame_marker)
0:	__(movl %arg_z,rcontext(tcr.save0))	/* save in case of error */
	__(movd %nargs,%mm0)	/* now we can use %temp1 */
	__(xorl %nargs,%nargs)
	__(cmpl $nil_value,%arg_z)
	__(je 2f)
1:	__(extract_fulltag(%arg_z,%imm0))
	__(cmpb $fulltag_cons,%imm0_b)
	__(jne 9f)
	__(_car(%arg_z,%arg_y))
	__(_cdr(%arg_z,%arg_z))
	__(add $node_size,%nargs)
	__(cmpl $call_arguments_limit<<fixnumshift,%nargs)
	__(jae 8f)
	__(push %arg_y)
	__(cmpl $nil_value,%arg_z)
	__(jne 1b)
2:	__(movd %mm0,%imm0)
	__(addl %imm0,%nargs)
	__(jne 4f)
3:	__(addl $2*node_size,%esp)
	__(movl $0,rcontext(tcr.save0))
	__(jmp *%ra0)
4:	__(pop %arg_z)
	__(cmp $1*node_size,%nargs)
	__(je 3b)
	__(pop %arg_y)
	__(cmp $2*node_size,%nargs)
	__(je 3b)
	__(movl $0,rcontext(tcr.save0))
	__(jmp *%ra0)
/* Discard everything that's been pushed already, complain */
8:	__(lea (%esp,%nargs),%esp)
	__(movl rcontext(tcr.save0),%arg_z) /* recover original */
	__(movl $0,rcontext(tcr.save0))
	__(movl $XTMINPS,%arg_y)
	__(set_nargs(2))
	__(push %ra0)
	__(jmp _SPksignalerr)
9:	__(lea (%esp,%nargs),%esp)
	__(movl rcontext(tcr.save0),%arg_z) /* recover original */
	__(movl $0,rcontext(tcr.save0))
	__(movl $XNOSPREAD,%arg_y)
	__(set_nargs(2))
	__(push %ra0)
	__(jmp _SPksignalerr)
_endsubp(spreadargz)


/* Caller built its own frame when it was entered.  If all outgoing args  */
/* are in registers, we can discard that frame; otherwise, we copy outgoing  */
/* relative to it and restore %rbp/%ra0   */
_spentry(tfuncallgen)
	__(cmpl $nargregs*node_size,%nargs)
	__(jbe 9f)
	__(lea -nargregs*node_size(%esp,%nargs),%imm0)
	__(movl %temp0,rcontext(tcr.save0))
	__(movd %nargs,%mm0)
	__(xorl %temp1,%temp1)
	/* We can use %ra0 as a temporary here, since the real return address */
	/* is on the stack   */
0:	__(movl -node_size(%imm0),%ra0)
	__(movl %ra0,-node_size(%ebp,%temp1))
	__(subl $node_size,%imm0)
	__(subl $node_size,%temp1)
	__(cmpl %imm0,%esp)
	__(jne 0b)
	__(lea (%ebp,%temp1),%esp)
	__(movl 4(%ebp),%ra0)
	__(movl (%ebp),%ebp)
        __(pushl %ra0)
	__(movd %mm0,%nargs)
	__(movl rcontext(tcr.save0),%temp0)
	__(movss %fpzero,rcontext(tcr.save0))
	__(do_funcall())
        /* All args in regs; exactly the same as the tfuncallvsp case   */
9:		
	__(leave)
	__(do_funcall())

_endsubp(tfuncallgen)

/* Some args were pushed; move them down in the frame   */
_spentry(tfuncallslide)
	__(lea -nargregs*node_size(%esp,%nargs),%imm0)
	__(movd %nargs,%mm0)
	__(xorl %temp1,%temp1)
	__(movl %temp0,rcontext(tcr.save0))
0:	__(movl -node_size(%imm0),%temp0)
	__(movl %temp0,-node_size(%ebp,%temp1))
	__(subl $node_size,%imm0)
	__(subl $node_size,%temp1)
	__(cmpl %imm0,%esp)
	__(jne 0b)
	__(lea (%ebp,%temp1),%esp)
	__(push 4(%ebp))	/* return address */
	__(movl (%ebp),%ebp)
	__(movd %mm0,%nargs)
	__(movl rcontext(tcr.save0),%temp0)
	__(movss %fpzero,rcontext(tcr.save0))
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
	__(lea -nargregs*node_size(%esp,%nargs),%imm0)
	__(movd %nargs,%mm0)
	__(movl %temp0,rcontext(tcr.save0))
	__(xorl %temp1,%temp1)	/* aka nargs */
0:	__(movl -node_size(%imm0),%temp0)
	__(movl %temp0,-node_size(%ebp,%temp1))
	__(subl $node_size,%imm0)
	__(subl $node_size,%temp1)
	__(cmpl %imm0,%esp)
	__(jne 0b)
	__(lea (%ebp,%temp1),%esp)
	__(movl 4(%ebp),%temp0)
	__(movl (%ebp),%ebp)
	__(push %temp0)
	__(movl rcontext(tcr.save0),%temp0)
	__(movss %fpzero,rcontext(tcr.save0))
	__(movd %mm0,%nargs)
	__(jump_fname())
/* All args in regs; exactly the same as the tcallsymvsp case. */
9:
	__(leave)
	__(jump_fname())
_endsubp(tcallsymgen)

_spentry(tcallsymslide)
	__(movl %ebp,%imm0)
	__(subl %nargs,%imm0)
	__(addl $nargregs*node_size,%imm0)	/* new tos */
	__(push %imm0)
	__(push %arg_y)
	__(push %arg_z)
	__(push %nargs)
	__(lea (4-nargregs)*node_size(%esp,%nargs),%arg_y) /* src ptr */
	__(movl %ebp,%imm0) /* dst ptr */
	__(subl $fixnumone*nargregs,%nargs)
	__(jmp 1f)
0:	__(subl $node_size,%arg_y)
	__(movl (%arg_y),%arg_z)
	__(subl $node_size,%imm0)
	__(movl %arg_z,(%imm0))
1:	__(subl $fixnumone,%nargs)
	__(jge 0b)
	__(pop %nargs)
	__(pop %arg_z)
	__(pop %arg_y)
	__(pop %esp)
	__(push node_size(%ebp))
	__(movl 0(%ebp),%ebp)
	__(jump_fname)
_endsubp(tcallsymslide)

_spentry(tcallsymvsp)
	__(leave)
	__(jump_fname())
_endsubp(tcallsymvsp)

_spentry(tcallnfngen)
	__(cmpl $nargregs*node_size,%nargs)
	__(jbe 9f)
	__(lea -nargregs*node_size(%esp,%nargs),%imm0)
	__(movd %nargs,%mm0)	/* stash nargs aka temp1 */
	__(xorl %temp1,%temp1)
	__(movl %temp0,rcontext(tcr.save0))
	/* It's OK to use %ra0 (%temp0) as an temp here, since the */
	/* real return address is on the stack. */
0:	__(movl -node_size(%imm0),%ra0)
	__(movl %ra0,-node_size(%ebp,%temp1))
	__(subl $node_size,%imm0)
	__(subl $node_size,%temp1)
	__(cmpl %imm0,%esp)
	__(jne 0b)
	__(movl rcontext(tcr.save0),%fn)
	__(movss %fpzero,rcontext(tcr.save0))
	__(lea (%ebp,%temp1),%esp)
	__(movl lisp_frame.savera0(%ebp),%ra0)
	__(movl lisp_frame.backlink(%ebp),%ebp)
	__(push %ra0)
	__(movd %mm0,%nargs)
	__(jmp *%fn)
9:	/* All args in regs; exactly the same as the tcallnfnvsp case */
	__(movl %temp0,%fn)
	__(leave)
	__(jmp *%fn)
_endsubp(tcallnfngen)

_spentry(tcallnfnslide)
	__(lea -nargregs*node_size(%esp,%nargs),%imm0)
	__(movd %nargs,%mm0)	/* save nargs aka temp1 */
	__(xorl %temp1,%temp1)
	__(movl %temp0,rcontext(tcr.save0))
	/* We can use %ra0 as a temporary here, since the real return address */
	/* is on the stack   */
0:	__(movl -node_size(%imm0),%ra0)
	__(movl %ra0,-node_size(%ebp,%temp1))
	__(subl $node_size,%imm0)
	__(subl $node_size,%temp1)
	__(cmpl %imm0,%esp)
	__(jne 0b)
	__(movl rcontext(tcr.save0),%fn)
	__(lea (%ebp,%temp1),%esp)
	__(movl lisp_frame.savera0(%ebp),%ra0)
	__(movl lisp_frame.backlink(%ebp),%ebp)
        __(push %ra0)
	__(movapd %fpzero,rcontext(tcr.save0))
	__(movd %mm0,%nargs)
	__(jmp *%fn)
_endsubp(tcallnfnslide)

_spentry(tcallnfnvsp)
	__(mov %temp0,%fn)
	__(leave)
	__(jmp *%fn)
_endsubp(tcallnfnvsp)

/* Make a "raw" area on the foreign stack, stack-cons a macptr to point */
/* to it, and return the macptr.  Size (in bytes, boxed) is in arg_z */
/* on entry; macptr in arg_z on exit. */
_spentry(makestackblock)
        __(check_cstack_alignment())
	__(unbox_fixnum(%arg_z,%imm0))
	__(dnode_align(%imm0,tsp_frame.fixed_overhead+macptr.size,%imm0))
	__(cmpl $tstack_alloc_limit,%imm0)
	__(jae 1f)
        __ifdef(`WINDOWS')
         __(windows_cstack_probe(%imm0,%arg_z))
        __endif
	__(movd rcontext(tcr.foreign_sp),%mm0)
	__(subl %imm0,rcontext(tcr.foreign_sp))
	__(movl rcontext(tcr.foreign_sp),%arg_z)
	__(movd %mm0,(%arg_z))
	__(movl %ebp,csp_frame.save_ebp(%arg_z))
	__(lea macptr.size+tsp_frame.fixed_overhead(%arg_z),%imm0)
	__(movl $macptr_header,tsp_frame.fixed_overhead(%arg_z))
	__(addl $fulltag_misc+tsp_frame.fixed_overhead,%arg_z)
	__(movl %imm0,macptr.address(%arg_z))
	__(movss %fpzero,macptr.domain(%arg_z))
	__(movss %fpzero,macptr.type(%arg_z))
	__(ret)
1:	__(movd rcontext(tcr.foreign_sp),%mm0)
	__(subl $dnode_size,rcontext(tcr.foreign_sp))
	__(movl rcontext(tcr.foreign_sp),%imm0)
	__(movd %mm0,(%imm0))
	__(movl %ebp,csp_frame.save_ebp(%imm0))
	__(set_nargs(1))
	__(movl $nrs.new_gcable_ptr,%fname)
	__(jump_fname())
_endsubp(makestackblock)

_spentry(makestackblock0)
	__(unbox_fixnum(%arg_z,%imm0))
	__(dnode_align(%imm0,tsp_frame.fixed_overhead+macptr.size,%imm0))
	__(cmpl $tstack_alloc_limit,%imm0)
	__(jae 9f)
        __ifdef(`WINDOWS')
         __(windows_cstack_probe(%imm0,%temp0))
        __endif
        __(movl rcontext(tcr.foreign_sp),%temp0)
        __(subl %imm0,rcontext(tcr.foreign_sp))
        __(movl rcontext(tcr.foreign_sp),%arg_z)
	__(movl %temp0,(%arg_z))
	__(movl %ebp,csp_frame.save_ebp(%arg_z))
	__(lea macptr.size+tsp_frame.fixed_overhead(%arg_z),%imm0)
	__(movl $macptr_header,tsp_frame.fixed_overhead(%arg_z))
	__(addl $fulltag_misc+tsp_frame.fixed_overhead,%arg_z)
	__(movl %imm0,macptr.address(%arg_z))
	__(movss %fpzero,macptr.domain(%arg_z))
	__(movss %fpzero,macptr.type(%arg_z))
	__(jmp 2f)
1:	__(movsd %fpzero,(%imm0))
	__(addl $dnode_size,%imm0)
2:	__(cmpl %imm0,%temp0)
	__(jne 1b)
	__(repret)
9:	__(movd rcontext(tcr.foreign_sp),%mm0)
        __(subl $dnode_size,rcontext(tcr.foreign_sp))
        __(movl rcontext(tcr.foreign_sp),%imm0)
	__(movd %mm0,(%imm0))
	__(movl %ebp,csp_frame.save_ebp(%imm0))
	__(set_nargs(1))
	__(movl $nrs.new_gcable_ptr,%fname)
	__(jump_fname())
_endsubp(makestackblock0)

_spentry(makestacklist)
	__(test %arg_y,%arg_y)
        __(js 9f)
	__(movl %arg_y,%imm0)
        __(testb $fixnummask,%imm0_b)
        __(jne 9f)
	__(addl %imm0,%imm0)
	__(rcmpl(%imm0,$tstack_alloc_limit))
	__(movl $nil_value,%temp1) 
	__(jae 2f)
	__(addl $tsp_frame.fixed_overhead,%imm0)
	__(TSP_Alloc_Var(%imm0,%temp0))
	__(addl $fulltag_cons,%temp0)
	__(jmp 1f)
0:	__(_rplaca(%temp0,%arg_z))
	__(_rplacd(%temp0,%temp1))
	__(movl %temp0,%temp1)
	__(addl $cons.size,%temp0)
1:	__(subl $fixnumone,%arg_y)
	__(jge 0b)
	__(movl %temp1,%arg_z)
	__(ret)
2:	__(TSP_Alloc_Fixed(0,%imm0))
	__(jmp 4f)
3:	__(Cons(%arg_z,%temp1,%temp1))
4:	__(subl $fixnumone,%arg_y)				
	__(jge 3b)
	__(movl %temp1,%arg_z)
	__(ret)
9:      __(uuo_error_reg_not_type(Rarg_y,error_object_not_unsigned_byte))
_endsubp(makestacklist)

/* subtype (boxed) vpushed before initial values. (Had better be a */
/* node header subtag.)  Nargs set to count of things vpushed. */
_spentry(stkgvector)
	__(movl -fixnumone(%esp,%nargs),%imm0)	/* boxed subtag */
	__(shrl $fixnumshift,%imm0)
	__(leal -fixnumone(%nargs),%arg_z)
	__(movl %arg_z,%arg_y)
	__(shll $num_subtag_bits-fixnumshift,%arg_z)
	__(orl %arg_z,%imm0)	/* imm0 = header, %arg_y = unaligned size */
	__(movd %imm0,%mm0)
	__(dnode_align(%arg_y,(tsp_frame.fixed_overhead+node_size),%imm0))
	__(TSP_Alloc_Var(%imm0,%arg_z))
	__(movd %mm0,(%arg_z))
	__(addl $fulltag_misc,%arg_z)
	__(lea -node_size(%nargs),%imm0)
	__(jmp 2f)
1:	__(pop misc_data_offset(%arg_z,%imm0))
2:	__(subl $node_size,%imm0)
	__(jge 1b)
	__(addl $node_size,%esp)
	__(jmp *%ra0)
_endsubp(stkgvector)

/* Allocate a fulltag-misc object. */
/* arg_y = boxed element count, arg_z = subtag (boxed, of course) */
_spentry(misc_alloc)
	__(testl $~(((1<<24)-1)<<fixnumshift),%arg_y)
	__(jne local_label(misc_alloc_not_u24))
	__(unbox_fixnum(%arg_z,%imm0))
	__(mov %arg_y,%temp0)
	__(shl $num_subtag_bits-fixnumshift,%temp0)
	__(or %temp0,%imm0)	/* %imm0 now = header */
	__(movd %imm0,%mm0)	/* Misc_Alloc wants header in %mm0 */
	__(andb $fulltagmask,%imm0_b)
	__(cmpb $fulltag_nodeheader,%imm0_b)
	__(je local_label(misc_alloc_32))
	__(movd %mm0,%imm0)
	__(cmpb $max_32_bit_ivector_subtag,%imm0_b)
	__(jbe local_label(misc_alloc_32))
	__(cmpb $max_8_bit_ivector_subtag,%imm0_b)
	__(jbe local_label(misc_alloc_8))
	__(cmpb $max_16_bit_ivector_subtag,%imm0_b)
	__(jbe local_label(misc_alloc_16))
	__(cmpb $subtag_double_float_vector,%imm0_b)
	__(jne local_label(misc_alloc_1))
	/* double-float vector case */
	__(imul $2,%arg_y,%imm0)
	__(jmp local_label(misc_alloc_alloc_vector))
local_label(misc_alloc_1):
	__(unbox_fixnum(%arg_y,%imm0))
	__(addl $7,%imm0)
	__(shrl $3,%imm0)
	__(jmp local_label(misc_alloc_alloc_vector))
local_label(misc_alloc_8):
	__(unbox_fixnum(%arg_y,%imm0))
	__(jmp local_label(misc_alloc_alloc_vector))
local_label(misc_alloc_16):
	__(unbox_fixnum(%arg_y,%imm0))
	__(shl $1,%imm0)
	__(jmp local_label(misc_alloc_alloc_vector))
local_label(misc_alloc_32):
	__(movl %arg_y,%imm0)
local_label(misc_alloc_alloc_vector):
	__(dnode_align(%imm0,node_size,%imm0))
	__(Misc_Alloc(%arg_z))
	__(ret)
local_label(misc_alloc_not_u24):
	__(uuo_error_reg_not_type(Rarg_y,error_object_not_unsigned_byte_24))
_endsubp(misc_alloc)

/* N.B. arg count word in %imm0, not %nargs */
/* no %whole_reg;  it's in rcontext(tcr.save0) */
/* %arg_reg is %temp1, key vector in %arg_y */ 
_startfn(C(destbind1))
	__(movl %ra0,rcontext(tcr.save1))
	/* Save entry %esp in case of error   */
	__(movd %esp,%mm0)
	/* Save arg count word */
	__(movd %imm0,%mm1)
	/* Extract required arg count.   */
        __(testb %imm0_b,%imm0_b)
	__(je local_label(opt))		/* skip if no required args   */
	__(movzbl %imm0_b,%imm0)
local_label(req_loop):	
	__(compare_reg_to_nil(%arg_reg))
	__(je local_label(toofew))
	__(movb $fulltagmask,%imm0_bh)
	__(andb %arg_reg_b,%imm0_bh)
	__(cmpb $fulltag_cons,%imm0_bh)
	__(jne local_label(badlist))
	__(subb $1,%imm0_b)
	__(pushl cons.car(%arg_reg))
	__(_cdr(%arg_reg,%arg_reg))
	__(jne local_label(req_loop))
	__(movd %mm1,%imm0)
local_label(opt):
        __(movb %imm0_bh,%imm0_b)
	__(testb %imm0_b,%imm0_b)
	__(je local_label(rest_keys))
	__(btl $initopt_bit,%imm0)
	__(jc local_label(opt_supp))
	/* 'simple' &optionals:	 no supplied-p, default to nil.   */
local_label(simple_opt_loop):
	__(compare_reg_to_nil(%arg_reg))
	__(je local_label(default_simple_opt))
	__(movb $fulltagmask,%imm0_bh)
	__(andb %arg_reg_b,%imm0_bh)
	__(cmpb $fulltag_cons,%imm0_bh)
	__(jne local_label(badlist))
	__(subb $1,%imm0_b)
	__(pushl cons.car(%arg_reg))
	__(_cdr(%arg_reg,%arg_reg))
	__(jne local_label(simple_opt_loop))
	__(jmp local_label(rest_keys))
local_label(default_simple_opt):
	__(subb $1,%imm0_b)
	__(pushl $nil_value)
	__(jne local_label(default_simple_opt))
	__(jmp local_label(rest_keys))
local_label(opt_supp):
	__(movb $fulltagmask,%imm0_bh)
	__(andb %arg_reg_b,%imm0_bh)
	__(compare_reg_to_nil(%arg_reg))
	__(je local_label(default_hard_opt))
	__(cmpb $fulltag_cons,%imm0_bh)
	__(jne local_label(badlist))
	__(subb $1,%imm0_b)
	__(pushl cons.car(%arg_reg))
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
	__(btl $restp_bit,%imm0)
	__(jc local_label(have_rest))
	__(btl $keyp_bit,%imm0)
	__(jc local_label(have_keys))
	__(compare_reg_to_nil(%arg_reg))
	__(jne local_label(toomany))
	__(movss %fpzero,rcontext(tcr.save0))
	__(jmp *%ra0)
local_label(have_rest):
	__(pushl %arg_reg)
	__(btl $keyp_bit,%imm0)
	__(jc local_label(have_keys))
	__(movss %fpzero,rcontext(tcr.save0))
	__(jmp *%ra0)
	/* Ensure that arg_reg contains a proper,even-length list.  */
	/* Insist that its length is <= 512 (as a cheap circularity check.)   */
local_label(have_keys):
	__(movb $255,%imm0_b)
	__(push %arg_reg)
	__(push %arg_z)
	__(xorl %arg_z,%arg_z)
local_label(count_keys_loop):
	__(compare_reg_to_nil(%arg_reg))
	__(je local_label(counted_keys))
	__(subb $1,%imm0_b)
	__(jb local_label(toomany))
	__(movb $fulltagmask,%arg_z_bh)
	__(andb %arg_reg_b,%arg_z_bh)
 	__(cmpb $fulltag_cons,%arg_z_bh)
	__(jne local_label(badlist))
	__(_cdr(%arg_reg,%arg_reg))
        __(compare_reg_to_nil(%arg_reg))
        __(je local_label(badlist))
	__(movb $fulltagmask,%arg_z_bh)
	__(andb %arg_reg_b,%arg_z_bh)
	__(cmpb $fulltag_cons,%arg_z_bh)
	__(jne local_label(badlist))
	__(_cdr(%arg_reg,%arg_reg))
	__(jmp local_label(count_keys_loop))
local_label(counted_keys):		
	/* We've got a proper, even-length list of key/value pairs in  */
	/* arg_reg. For each keyword var in the lambda-list, push a pair  */
	/* of NILs on the vstack.   */
	__(pop %arg_z)
	__(pop %arg_reg)
	__(movd %mm1,%imm0)
	__(shrl $16,%imm0)
	__(movzbl %imm0_b,%imm0)
	__(movl %esp,rcontext(tcr.unboxed0))	/* 0th value/supplied-p pair */
	__(jmp local_label(push_pair_test))
local_label(push_pair_loop):
	__(push $nil_value)
	__(push $nil_value)
local_label(push_pair_test):	
	__(subb $1,%imm0_b)
	__(jge local_label(push_pair_loop))
	__(push %temp0)	/* keyword */
	__(push %arg_z) /* value */
	__(vector_length(%arg_y,%imm0))
	__(push %arg_reg)
	__(push %imm0)	/* keyword vector length */
	__(movd %mm1,%imm0)
	__(movl $0,rcontext(tcr.unboxed1)) /* count of unknown keywords seen */
local_label(match_keys_loop):
	__(movl 4(%esp),%arg_reg)
	__(compare_reg_to_nil(%arg_reg))
	__(je local_label(matched_keys))
	__(_car(%arg_reg,%temp0))
	__(_cdr(%arg_reg,%arg_reg))
	__(_car(%arg_reg,%arg_z))
	__(_cdr(%arg_reg,%arg_reg))
	__(movl %arg_reg,4(%esp))
	__(xorl %temp1,%temp1)
	__(jmp local_label(match_test))
local_label(match_loop):
	__(cmpl misc_data_offset(%arg_y,%temp1),%temp0)
	__(je local_label(matched))
	__(addl $node_size,%temp1)
local_label(match_test):
	__(cmpl %temp1,(%esp))	/* compare index, keyword vector length */
	__(jne local_label(match_loop))
	/* No match.  Note unknown keyword, check for :allow-other-keys   */
	__(addl $1,rcontext(tcr.unboxed1))
	__(cmpl $nrs.kallowotherkeys,%temp0)
	__(jne local_label(match_keys_loop))
	__(subl $1,rcontext(tcr.unboxed1))
	__(btsl $seen_aok_bit,%imm0)
	__(jc local_label(match_keys_loop))
	/* First time we've seen :allow-other-keys.  Maybe set aok_bit.   */
	__(compare_reg_to_nil(%arg_z))
	__(je local_label(match_keys_loop))
	__(btsl $aok_bit,%imm0)
	__(jmp local_label(match_keys_loop))
	/* Got a match.  Worry about :allow-other-keys here, too.   */
local_label(matched):
	__(negl %temp1)
	__(shll $1,%temp1)
	__(addl rcontext(tcr.unboxed0),%temp1)
	__(cmpl $nil_value,-node_size*2(%temp1))
	__(jne local_label(match_keys_loop))
	__(movl %arg_z,-node_size(%temp1))
	__(movl $t_value,-node_size*2(%temp1))
	__(cmpl $nrs.kallowotherkeys,%temp0)
	__(jne local_label(match_keys_loop))
	__(btsl $seen_aok_bit,%imm0)
	__(jnc local_label(match_keys_loop))
	__(compare_reg_to_nil(%arg_z))
	__(je local_label(match_keys_loop))
	__(btsl $aok_bit,%imm0)
	__(jmp local_label(match_keys_loop))
local_label(matched_keys):	
	__(cmpl $0,rcontext(tcr.unboxed1))	/* any unknown keys seen? */
	__(je local_label(keys_ok))
	__(btl $aok_bit,%imm0)
	__(jnc local_label(badkeys))
local_label(keys_ok):
	__(addl $(3*node_size),%esp)
	__(pop %ra0)
	__(movss %fpzero,rcontext(tcr.save0))
	__(jmp *%ra0)
	/* Some unrecognized keywords.  Complain generically about   */
	/* invalid keywords.   */
local_label(badkeys):
	__(movl $XBADKEYS,%arg_y)
	__(jmp local_label(destructure_error))
local_label(toomany):
	__(movl $XCALLTOOMANY,%arg_y)
	__(jmp local_label(destructure_error))
local_label(toofew):
	__(movl $XCALLTOOFEW,%arg_y)
	__(jmp local_label(destructure_error))
local_label(badlist):
	__(movl $XCALLNOMATCH,%arg_y)
local_label(destructure_error):
	__(movd %mm0,%esp)		/* undo everything done to the stack */
	__(movl rcontext(tcr.save0),%arg_z)	/* %whole_reg */
	__(movss %fpzero,rcontext(tcr.save0))
	__(set_nargs(2))
	__(push %ra0)
	__(jmp _SPksignalerr)
_endfn(C(destbind1))

_spentry(macro_bind)
	__(movl %arg_reg,rcontext(tcr.save0))	/* %whole_reg */
	__(extract_fulltag(%arg_reg,%imm0))
	__(cmpb $fulltag_cons,%imm0_b)
	__(jne 1f)
	__(_cdr(%arg_reg,%arg_reg))
	__(jmp C(destbind1))
1:	__(movl $XCALLNOMATCH,%arg_y)
	__(movl rcontext(tcr.save0),%arg_z)
	__(movss %fpzero,rcontext(tcr.save0))
	__(set_nargs(2))
        __(push %ra0)        
	__(jmp _SPksignalerr)

_endsubp(macro_bind)

_spentry(destructuring_bind)
	__(movl %arg_reg,rcontext(tcr.save0))	/* %whole_reg */
	__(jmp C(destbind1))
_endsubp(destructuring_bind)

_spentry(destructuring_bind_inner)
	__(movl %arg_z,rcontext(tcr.save0))	/* %whole_reg */
	__(jmp C(destbind1))
_endsubp(destructuring_bind_inner)

_spentry(vpopargregs)
	__(hlt)
_endsubp(vpopargregs)

/* If arg_z is an integer, return in imm0 something whose sign  */
/* is the same as arg_z's.  If not an integer, error.   */
_spentry(integer_sign)
	__(mov %arg_z,%imm0)
	__(testb $tagmask,%arg_z_b)
	__(je 8f)
	__(extract_typecode(%arg_z,%imm0))
	__(cmpb $subtag_bignum,%imm0_b)
	__(jne 9f)
	__(getvheader(%arg_z,%imm0))
	__(shr $num_subtag_bits,%imm0)
	__(movl misc_data_offset-4(%arg_z,%imm0,4),%imm0)
8:	__(repret)
9:	__(uuo_error_reg_not_type(Rarg_z,error_object_not_integer))
_endsubp(integer_sign)

/* "slide" nargs worth of values up the stack.  imm0 contains */
/* the difference between the current stack pointer and the target. */
_spentry(mvslide)
	__(movd %nargs,%mm0)
	__(lea (%esp,%nargs),%arg_y)
	__(lea (%arg_y,%imm0),%imm0)
	__(test %nargs,%nargs)
	__(je 2f)
1:
	__(subl $node_size,%arg_y)
	__(movl (%arg_y),%arg_z)
	__(subl $node_size,%imm0)
	__(movl %arg_z,(%imm0))
	__(subl $node_size,%nargs)
	__(jne 1b)
2:	__(movl %imm0,%esp)
	__(movd %mm0,%nargs)
	__(jmp *%ra0)
_endsubp(mvslide)

_spentry(save_values)
	__(movd rcontext(tcr.save_tsp),%mm1)
/* common exit: nargs = values in this set, mm1 = ptr to tsp before call to save_values   */
local_label(save_values_to_tsp):
	__(movl %ra0,rcontext(tcr.save0))
	__(movl rcontext(tcr.save_tsp),%temp0)
	__(dnode_align(%nargs,tsp_frame.fixed_overhead+(2*node_size),%imm0)) /* count, link   */
	__(TSP_Alloc_Var(%imm0,%arg_z))
	__(movl rcontext(tcr.save_tsp),%imm0)
	__(movd %mm1,(%imm0))
	__(movl %nargs,(%arg_z))
	__(movl %temp0,node_size(%arg_z))
	__(leal 2*node_size(%arg_z,%nargs),%arg_y)
	__(leal (%esp,%nargs),%imm0)
	__(cmpl %imm0,%esp)
	__(jmp 2f)
1:	__(subl $node_size,%imm0)
	__(movl (%imm0),%arg_z)
	__(subl $node_size,%arg_y)
	__(cmpl %imm0,%esp)
	__(movl %arg_z,(%arg_y))
2:	__(jne 1b)
	__(addl %nargs,%esp)
	__(movl rcontext(tcr.save0),%ra0)
	__(movl $0,rcontext(tcr.save0))
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
	/* do we need to preserve imm0? */
	__(test %nargs,%nargs)
	__(movl rcontext(tcr.save_tsp),%imm0)
	__(movl (%imm0),%imm0)
	__(movd %imm0,%mm1)	/* for the benefit of save_values_to_tsp */
	__(jne local_label(save_values_to_tsp))
	__(jmp *%ra0)
_endsubp(add_values)

/* push the values in the value set atop the sp, incrementing nargs.  */
/* Discard the tsp frame; leave values atop the sp.   */
_spentry(recover_values)
	__(movl %ra0,rcontext(tcr.save0)) /* temp0 */
	__(movd %nargs,%mm0)		  /* temp1 */
	/* First, walk the segments reversing the pointer to previous  */
	/* segment pointers Can tell the end because that previous  */
	/* segment pointer is the prev tsp pointer   */
	__(movl rcontext(tcr.save_tsp),%temp1)
	__(movl %temp1,%temp0)	/* current segment   */
	__(movl %temp1,%arg_y)	/* last segment   */
	__(movl tsp_frame.backlink(%temp1),%arg_z)	/* previous tsp   */
local_label(walkloop):
	__(movl tsp_frame.fixed_overhead+node_size(%temp0),%imm0)
	__(cmpl %imm0,%arg_z)	/* last segment ?   */
	__(movl %arg_y,tsp_frame.fixed_overhead+node_size(%temp0))
	__(movl %temp0,%arg_y)	/* last segment <- current segment   */
	__(movl %imm0,%temp0)	/* current segment <- next segment   */
	__(jne local_label(walkloop))

	__(movl %temp1,%arg_z)
	__(movd %mm0,%nargs)
	/* the final segment pointer is now in %arg_y  */
	/* walk backwards, pushing values on the stack and incrementing %nargs   */
local_label(pushloop):
	__(movl tsp_frame.data_offset(%arg_y),%imm0)	/* nargs in segment   */
	__(test %imm0,%imm0)
	__(leal tsp_frame.data_offset+(2*node_size)(%arg_y,%imm0),%temp0)
	__(leal (%nargs,%imm0),%nargs)
	__(jmp 2f)
1:	__(push -node_size(%temp0))
	__(subl $node_size,%temp0)
	__(subl $fixnum_one,%imm0)
2:	__(jne 1b)
	__(cmpl %arg_y,%arg_z)
	__(movl tsp_frame.data_offset+node_size(%arg_y),%arg_y)
	__(jne local_label(pushloop))
	__(movl (%arg_z),%arg_z)
        __(movl %arg_z,rcontext(tcr.save_tsp))
        __(movl %arg_z,rcontext(tcr.next_tsp))
	__(movl rcontext(tcr.save0),%ra0)
	__(movl $0,rcontext(tcr.save0))
	__(jmp *%ra0)		
_endsubp(recover_values)

/* Exactly like recover_values, but it's necessary to reserve an outgoing  */
/* frame if any values (which will be used as outgoing arguments) will  */
/* wind up on the stack.  We can assume that %nargs contains 0 (and  */
/* that no other arguments have been pushed) on entry.   */

_spentry(recover_values_for_mvcall)
	__(movl %ra0,rcontext(tcr.save0)) /* temp0 */
	/* First, walk the segments reversing the pointer to previous  */
	/* segment pointers Can tell the end because that previous  */
	/* segment pointer is the prev tsp pointer   */
	__(xorl %nargs,%nargs)
	__(push %nargs)
	__(movl rcontext(tcr.save_tsp),%temp1)
	__(movl %temp1,%temp0)	/* current segment   */
	__(movl %temp1,%arg_y)	/* last segment   */
	__(movl tsp_frame.backlink(%temp1),%arg_z)	/* previous tsp   */
local_label(walkloop_mvcall):
	__(movl tsp_frame.data_offset(%temp0),%imm0)
	__(addl %imm0,(%esp))
	__(movl tsp_frame.fixed_overhead+node_size(%temp0),%imm0)
	__(cmpl %imm0,%arg_z)	/* last segment ?   */
	__(movl %arg_y,tsp_frame.fixed_overhead+node_size(%temp0))
	__(movl %temp0,%arg_y)	/* last segment <- current segment   */
	__(movl %imm0,%temp0)	/* current segment <- next segment   */
	__(jne local_label(walkloop_mvcall))

	__(movl %temp1,%arg_z)
	__(pop %nargs)

	__(cmpl $nargregs*node_size,%nargs)
	__(jbe local_label(pushloop_mvcall))
	__(push $reserved_frame_marker)
	__(push $reserved_frame_marker)

	/* the final segment pointer is now in %arg_y  */
	/* walk backwards, pushing values on the stack and incrementing %nargs*/
local_label(pushloop_mvcall):
	__(movl tsp_frame.data_offset(%arg_y),%imm0)	/* nargs in segment */
	__(test %imm0,%imm0)
	__(leal tsp_frame.data_offset+(2*node_size)(%arg_y,%imm0),%temp0)
	__(jmp 2f)
1:	__(push -node_size(%temp0))
	__(subl $node_size,%temp0)
	__(subl $fixnum_one,%imm0)
2:	__(jne 1b)
	__(cmpl %arg_y,%arg_z)
	__(movl tsp_frame.data_offset+node_size(%arg_y),%arg_y)
	__(jne local_label(pushloop_mvcall))
	__(movl (%arg_z),%arg_z)
        __(movl %arg_z,rcontext(tcr.save_tsp))
        __(movl %arg_z,rcontext(tcr.next_tsp))
	__(movl rcontext(tcr.save0),%ra0)
	__(movl $0,rcontext(tcr.save0))
	__(jmp *%ra0)		
_endsubp(recover_values_for_mvcall)

_spentry(reset)
	__(hlt)
_endsubp(reset)

/* temp0 = element-count, arg_y = subtag, arg_z = initval */
_spentry(misc_alloc_init)
	__(push %ebp)
	__(movl %esp,%ebp)
	__(push %arg_z)
	__(movl %arg_y,%arg_z)
	__(movl %temp0,%arg_y)
	__(push $local_label(misc_alloc_init_back))
	__(jmp _SPmisc_alloc)
__(tra(local_label(misc_alloc_init_back)))
	__(pop %arg_y)
	__(leave)
	__(movl $nrs.init_misc,%fname)
	__(set_nargs(2))
	__(jump_fname())
_endsubp(misc_alloc_init)

/* %temp1 = element-count, %arg_y = subtag, %arg_z = initial-value */        
_spentry(stack_misc_alloc_init)
	__(push %ebp)
        __(movl %esp,%ebp)
        __(push %arg_z)
        __(movl %arg_y,%arg_z)
        __(movl %temp1,%arg_y)
        __(pushl $local_label(stack_misc_alloc_init_back))
        __(jmp _SPstack_misc_alloc)
__(tra(local_label(stack_misc_alloc_init_back)))
        __(popl %arg_y)
	__(leave)
	__(movl $nrs.init_misc,%fname)
	__(set_nargs(2))
	__(jump_fname())
_endsubp(stack_misc_alloc_init)

	.globl C(popj)
_spentry(popj)
C(popj):
	__(leave)
        __(ret)
_endsubp(popj)

/* arg_z should be of type (signed-byte 64) */
/* return unboxed value in mm0 */
_spentry(gets64)
	__(testb $fixnummask,%arg_z_b)
	__(jne 1f)
        __(unbox_fixnum(%arg_z,%imm0))
        __(movd %imm0,%mm0)
        __(jns 8f)
        /* get sign into upper half of %mm0 */
        __(pcmpeqd %mm1,%mm1)   /* all ones */
        __(psllq $32,%mm1)
        __(por %mm1,%mm0)
        __(ret)
1:      __(movb %arg_z_b,%imm0_b)
        __(andb $tagmask,%imm0_b)
        __(cmpb $tag_misc,%imm0_b)
        __(jne 9f)
        __(movl misc_header_offset(%arg_z),%imm0)
        __(cmpb $subtag_bignum,%imm0_b)
        __(jne 9f)
        __(cmpl $two_digit_bignum_header,%imm0)
        __(ja 9f)
        __(movd misc_data_offset(%arg_z),%mm0)
	__(jne 8f)
	__(movq misc_data_offset(%arg_z),%mm0)
8:      __(repret)
9:      __(uuo_error_reg_not_type(Rarg_z,error_object_not_s64))
_endsubp(gets64)

/* arg_z should be of type (unsigned-byte 64) */
/* return unboxed value in mm0 */
_spentry(getu64)
	__(movl $~(target_most_positive_fixnum << fixnumshift),%imm0)
	__(testl %arg_z,%imm0)
	__(movl %arg_z,%imm0)
	__(jne 1f)
	__(sarl $fixnumshift,%imm0)
	__(movd %imm0,%mm0)
	__(ret)
1:	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne 9f)
	__(movl misc_header_offset(%arg_z),%imm0)
	__(cmpb $subtag_bignum,%imm0_b)
	__(jne 9f)
	__(cmpl $three_digit_bignum_header,%imm0)
	__(ja 9f)
	__(je 3f)
	__(cmpl $two_digit_bignum_header,%imm0)
	__(je 2f)
	/* must be a one digit bignum */
	__(movl misc_data_offset(%arg_z),%imm0)
	__(test %imm0,%imm0)
	__(js 9f)
	__(movd %imm0,%mm0)
	__(ret)
2: 	__(movl misc_data_offset+4(%arg_z),%imm0)
	__(testl %imm0,%imm0)
	__(js 9f)
	__(movq misc_data_offset(%arg_z),%mm0)
	__(ret)
3:	__(movl misc_data_offset(%arg_z),%imm0)
	__(cmpl $0,misc_data_offset+8(%arg_z))
	__(jne 9f)
	__(movq misc_data_offset(%arg_z),%mm0)
	__(repret)
9:	__(uuo_error_reg_not_type(Rarg_z,error_object_not_u64))
_endsubp(getu64)

/* Make unsigned integer from value in mm0 */
_spentry(makeu64)
	__(movq %mm0,%mm1)
	__(psrlq $32,%mm0)
	__(movd %mm0,%imm0)
	__(test %imm0,%imm0)
	__(js 3f)
	__(jnz 2f)
	__(movd %mm1,%imm0)
	__(cmpl $target_most_positive_fixnum,%imm0)
	__(ja 1f)
	__(box_fixnum(%imm0,%arg_z))
	__(ret)
1:	/* maybe make a 1 digit bignum */
	__(test %imm0,%imm0)
	__(js 2f)
	__(movl $one_digit_bignum_header,%imm0)
	__(movd %imm0,%mm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(1)))
	__(movd %mm1,misc_data_offset(%arg_z))
	__(ret)
	/* make a 2 digit bignum */
2:	__(movl $two_digit_bignum_header,%imm0)
	__(movd %imm0,%mm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(2)))
	__(movq %mm1,misc_data_offset(%arg_z))
	__(ret)
	/* make a 3 digit bignum */
3:	__(movl $three_digit_bignum_header,%imm0)
	__(movd %imm0,%mm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(3)))
	__(movq %mm1,misc_data_offset(%arg_z))
	__(ret)
_endsubp(makeu64)

/* on entry: arg_z = symbol.  On exit, arg_z = value (possibly */
/* unbound_marker), arg_y = symbol */
_spentry(specref)
	__(movl symbol.binding_index(%arg_z),%imm0)
	__(cmp rcontext(tcr.tlb_limit),%imm0)
	__(movl rcontext(tcr.tlb_pointer),%temp1)
	__(movl %arg_z,%arg_y)
	__(jae 1f)
	__(movl (%temp1,%imm0),%arg_z)
	__(cmpb $no_thread_local_binding_marker,%arg_z_b)
	__(cmovel symbol.vcell(%arg_y),%arg_z)
	__(ret)
1:      __(movl symbol.vcell(%arg_y),%arg_z)
        __(ret)
_endsubp(specref)

/* arg_y = special symbol, arg_z = new value. */
_spentry(specset)
	__(movl symbol.binding_index(%arg_y),%imm0)
	__(cmp rcontext(tcr.tlb_limit),%imm0)
	__(movl rcontext(tcr.tlb_pointer),%temp1)
	__(jae 1f)
	__(movl (%temp1,%imm0),%temp0)
	__(cmpb $no_thread_local_binding_marker,%temp0_b)
	__(je 1f)
	__(movl %arg_z,(%temp1,%imm0))
	__(ret)
1:	__(movl %arg_y,%temp0)
	__(movl $1<<fixnumshift,%arg_y)
	__(jmp _SPgvset)
_endsubp(specset)

_spentry(specrefcheck)
	__(mov %arg_z,%arg_y)
	__(movl symbol.binding_index(%arg_z),%imm0)
	__(cmp rcontext(tcr.tlb_limit),%imm0)
	__(jae 7f)
	__(movl rcontext(tcr.tlb_pointer),%temp1)
	__(movl (%temp1,%imm0),%arg_z)
	__(cmpb $no_thread_local_binding_marker,%arg_z_b)
	__(cmovel symbol.vcell(%arg_y),%arg_z)
	__(cmpb $unbound_marker,%arg_z_b)
	__(je 9f)
8:	__(repret)
7:	__(movl symbol.vcell(%arg_y),%arg_z)
	__(cmpb $unbound_marker,symbol.vcell(%arg_y))
	__(je 9f)
	__(repret)
9:	__(uuo_error_reg_unbound(Rarg_y))
_endsubp(specrefcheck)

_spentry(restoreintlevel)
	__(hlt)
_endsubp(restoreintlevel)

/* Make a lisp integer from the unsigned value in imm0 */
_spentry(makeu32)
	__(cmpl $target_most_positive_fixnum,%imm0)
	__(ja 0f)	/* need to make a bignum */
	__(box_fixnum(%imm0,%arg_z))
	__(ret)
0:	__(movd %imm0,%mm1)
	__(test %imm0,%imm0)
	__(js 1f)
	__(movl $one_digit_bignum_header,%imm0)
	__(movd %imm0,%mm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(1)))
	__(movd %mm1,misc_data_offset(%arg_z))
	__(ret)
1:	__(movl $two_digit_bignum_header,%imm0)
	__(movd %imm0,%mm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(2)))
	__(movd %mm1,misc_data_offset(%arg_z))
	__(ret)
_endsubp(makeu32)

/* arg_z is of type (signed-byte 32) */
/* return unboxed value in %imm0 */
_spentry(gets32)
	__(testb $fixnummask,%arg_z_b)
	__(jne 1f)
	__(unbox_fixnum(%arg_z,%imm0))
	__(ret)
1:	__(movb %arg_z_b,%imm0_b)
	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne 9f)
	__(movb misc_subtag_offset(%arg_z),%imm0_b)
	__(cmpb $subtag_bignum,%imm0_b)
	__(jne 9f)
	__(movl misc_header_offset(%arg_z),%imm0)
	__(cmpl $one_digit_bignum_header,%imm0)
	__(jne 9f)
	__(movl misc_data_offset(%arg_z),%imm0)
	__(ret)
9:	__(uuo_error_reg_not_type(Rarg_z,error_object_not_signed_byte_32))
_endsubp(gets32)

/* arg_z is of type (unsigned-byte 32) */
/* return unboxed value in %imm0 */
_spentry(getu32)
	__(movl $~(target_most_positive_fixnum << fixnumshift),%imm0)
	__(testl %arg_z,%imm0)
	__(movl %arg_z,%imm0)
	__(jne 1f)
	__(sarl $fixnumshift,%imm0)
	__(ret)
1:	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne 9f)
	__(movb misc_subtag_offset(%arg_z),%imm0_b)
	__(cmpb $subtag_bignum,%imm0_b)
	__(jne 9f)
	__(movl misc_header_offset(%arg_z),%imm0)
	__(cmpl $two_digit_bignum_header,%imm0)
	__(je 2f)
	__(cmpl $one_digit_bignum_header,%imm0)
	__(jne 9f)
	__(movl misc_data_offset(%arg_z),%imm0)
	__(ret)
2:	__(movl misc_data_offset(%arg_z),%imm0)
	__(cmpl $0,misc_data_offset+4(%arg_z))
	__(jne 9f)
	__(ret)
9:	__(uuo_error_reg_not_type(Rarg_z,error_object_not_unsigned_byte_32))
_endsubp(getu32)

_spentry(mvpasssym)
	__(hlt)
_endsubp(mvpasssym)

/* don't smash arg_z */
_spentry(unbind)
	__(push %arg_z)
	__(movl rcontext(tcr.db_link),%imm0)
	__(movl rcontext(tcr.tlb_pointer),%arg_z)
	__(movl binding.sym(%imm0),%temp0)
	__(movl binding.val(%imm0),%arg_y)
	__(movl binding.link(%imm0),%imm0)
	__(movl %arg_y,(%arg_z,%temp0))
	__(movl %imm0,rcontext(tcr.db_link))
	__(pop %arg_z)
	__(ret)
_endsubp(unbind)

_spentry(unbind_n)
	__(push %temp1)		/* preserve temp1/nargs */
	__(push %arg_z)
	__(xorl %arg_z,%arg_z)
	__(movl rcontext(tcr.db_link),%temp1)
	__(movl rcontext(tcr.tlb_pointer),%arg_z)
1:		
	__(movl binding.sym(%temp1),%temp0)
	__(movl binding.val(%temp1),%arg_y)
	__(movl binding.link(%temp1),%temp1)
	__(movl %arg_y,(%arg_z,%temp0))
	__(decl %imm0)
	__(jne 1b)
	__(movl %temp1,rcontext(tcr.db_link))
	__(pop %arg_z)
	__(pop %temp1)
	__(ret)	
_endsubp(unbind_n)

_spentry(unbind_to)
	__(push %arg_y)
	__(push %arg_z)
	__(push %temp0)
	__(push %temp1)
	
	__(movl rcontext(tcr.db_link),%temp0)
	__(movl rcontext(tcr.tlb_pointer),%arg_z)
1:
	__(movl binding.sym(%temp0),%temp1)
	__(movl binding.val(%temp0),%arg_y)
	__(movl binding.link(%temp0),%temp0)
	__(movl %arg_y,(%arg_z,%temp1))
	__(cmpl %temp0,%imm0)
	__(jne 1b)
	__(movl %temp0,rcontext(tcr.db_link))

	__(pop %temp1)
	__(pop %temp0)
	__(pop %arg_z)
	__(pop %arg_y)
	__(ret)
_endsubp(unbind_to)

_spentry(bind_interrupt_level_0)
	__(movl rcontext(tcr.tlb_pointer),%arg_y)
	__(cmpl $0,INTERRUPT_LEVEL_BINDING_INDEX(%arg_y))
	__(push INTERRUPT_LEVEL_BINDING_INDEX(%arg_y))
	__(push $INTERRUPT_LEVEL_BINDING_INDEX)
	__(push rcontext(tcr.db_link))
	__(movl %esp,rcontext(tcr.db_link))
	__(movl $0,INTERRUPT_LEVEL_BINDING_INDEX(%arg_y))
	__(js 1f)
0:	__(jmp *%ra0)
	/* Interrupt level was negative; interrupt may be pending */
1:	__(check_pending_enabled_interrupt(2f))
2:	__(jmp *%ra0)
_endsubp(bind_interrupt_level_0)

/* Bind CCL::*INTERRUPT-LEVEL* to the fixnum -1.  (This has the effect  */
/* of disabling interrupts.)   */
_spentry(bind_interrupt_level_m1)
	__(movl rcontext(tcr.tlb_pointer),%arg_y)
	__(push INTERRUPT_LEVEL_BINDING_INDEX(%arg_y))
	__(push $INTERRUPT_LEVEL_BINDING_INDEX)
	__(push rcontext(tcr.db_link))
	__(movl %esp,rcontext(tcr.db_link))
	__(movl $-1<<fixnumshift,INTERRUPT_LEVEL_BINDING_INDEX(%arg_y))
	__(jmp *%ra0)
_endsubp(bind_interrupt_level_m1)

/* Bind CCL::*INTERRUPT-LEVEL* to the value in arg_z.  If that value's 0, */
/* do what _SPbind_interrupt_level_0 does. */
_spentry(bind_interrupt_level)
	__(test %arg_z,%arg_z)
	__(jz _SPbind_interrupt_level_0)
	__(movl rcontext(tcr.tlb_pointer),%arg_y)
	__(push INTERRUPT_LEVEL_BINDING_INDEX(%arg_y))
	__(push $INTERRUPT_LEVEL_BINDING_INDEX)
	__(push rcontext(tcr.db_link))
	__(movl %esp,rcontext(tcr.db_link))
	__(movl %arg_z,INTERRUPT_LEVEL_BINDING_INDEX(%arg_y))
	__(jmp *%ra0)
_endsubp(bind_interrupt_level)

/* Unbind CCL::*INTERRUPT-LEVEL*.  If the value changes from negative to */
/* non-negative, check for pending interrupts. */
_spentry(unbind_interrupt_level)
	__(btl $TCR_FLAG_BIT_PENDING_SUSPEND,rcontext(tcr.flags))
	__(movl rcontext(tcr.tlb_pointer),%arg_y)
	__(movl INTERRUPT_LEVEL_BINDING_INDEX(%arg_y),%imm0)
	__(jc 5f)
0:	__(test %imm0,%imm0)
	__(movl rcontext(tcr.db_link),%imm0)
	__(movl binding.val(%imm0),%temp0)
	__(movl binding.link(%imm0),%imm0)
	__(movl %temp0,INTERRUPT_LEVEL_BINDING_INDEX(%arg_y))
	__(movl %imm0,rcontext(tcr.db_link))
	__(js 3f)
2:	__(repret)
3:	__(test %temp0,%temp0)
	__(js 2b)
	__(check_pending_enabled_interrupt(4f))
4:	__(repret)
5:       /* Missed a suspend request; force suspend now if we're restoring
          interrupt level to -1 or greater */
        __(cmpl $-2<<fixnumshift,%imm0)
        __(jne 0b)
	__(movl rcontext(tcr.db_link),%temp0)
	__(movl binding.val(%temp0),%temp0)
        __(cmpl %imm0,%temp0)
        __(je 0b)
        __(movl $-1<<fixnumshift,INTERRUPT_LEVEL_BINDING_INDEX(%arg_y))
        __(suspend_now())
        __(jmp 0b)
_endsubp(unbind_interrupt_level)

_spentry(progvrestore)
	__(movl rcontext(tcr.save_tsp),%imm0)
	__(movl tsp_frame.backlink(%imm0),%imm0) /* ignore .SPnthrowXXX values frame   */
	__(movl tsp_frame.data_offset(%imm0),%imm0)
	__(shrl $fixnumshift,%imm0)
	__(jne _SPunbind_n)
	__(repret)
_endsubp(progvrestore)

/* %arg_z <- %arg_y + %arg_z.  Do the fixnum case - including overflow -  */
/* inline.  Call out otherwise.   */
_spentry(builtin_plus)
	__(movl %arg_y,%imm0)
	__(orl %arg_z,%imm0)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(addl %arg_y,%arg_z)
	__(jo C(fix_one_bit_overflow))
	__(repret)
1:	__(jump_builtin(_builtin_plus,2))
_endsubp(builtin_plus)

/* %arg_z <- %arg_y - %arg_z.  Do the fixnum case - including overflow -  */
/*  inline.  Call out otherwise.   */
_spentry(builtin_minus)
	__(movl %arg_y,%imm0)
	__(orl %arg_z,%imm0)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(xchgl %arg_y,%arg_z)
	__(subl %arg_y,%arg_z)
	__(jo C(fix_one_bit_overflow))
	__(repret)
1:	__(jump_builtin(_builtin_minus,2))
_endsubp(builtin_minus)

/* %arg_z -< arg_y * arg_z. */
/* Do the fixnum case---including overflow---inline.  Call out otherwise. */
_spentry(builtin_times)
	__(movl %arg_y,%imm0)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 2f)
	__(unbox_fixnum(%arg_z,%imm0))
	/* 32-bit fixnum result in %imm0.  Overflow set if it doesn't fit. */
	__(imul %arg_y,%imm0)
	__(jo 1f)
	__(movl %imm0,%arg_z)
	__(ret)
1:	__(unbox_fixnum(%arg_z,%eax))
	__(mark_as_imm(%edx))
	__(unbox_fixnum(%arg_y,%edx))
	__(imul %edx)
        __(movd %eax,%mm0)
        __(movd %edx,%mm1)
        __(mark_as_node(%edx))
        __(psllq $32,%mm1)
        __(por %mm1,%mm0)
        __(jmp _SPmakes64)
2:	__(jump_builtin(_builtin_times,2))
_endsubp(builtin_times)

_spentry(builtin_div)
	__(jump_builtin(_builtin_div,2))

/* %arg_z <- (= %arg_y %arg_z).	  */
_spentry(builtin_eq)
	__(movl %arg_y,%imm0)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpl(%arg_z,%arg_y))
	__(condition_to_boolean(e,%imm0,%arg_z))
	__(ret)
1:	__(jump_builtin(_builtin_eq,2))
_endsubp(builtin_eq)

/* %arg_z <- (/= %arg_y %arg_z).	  */
_spentry(builtin_ne)
	__(movl %arg_y,%imm0)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpl(%arg_z,%arg_y))
	__(condition_to_boolean(ne,%imm0,%arg_z))
	__(ret)
1:	__(jump_builtin(_builtin_ne,2))
_endsubp(builtin_ne)

/* %arg_z <- (> %arg_y %arg_z).	  */
_spentry(builtin_gt)
	__(movl %arg_y,%imm0)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpl(%arg_y,%arg_z))
	__(condition_to_boolean(g,%imm0,%arg_z))
	__(ret)
1:	__(jump_builtin(_builtin_gt,2))
_endsubp(builtin_gt)

/* %arg_z <- (>= %arg_y %arg_z).	  */
_spentry(builtin_ge)
	__(movl %arg_y,%imm0)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpl(%arg_y,%arg_z))
	__(condition_to_boolean(ge,%imm0,%arg_z))
	__(ret)
1:	__(jump_builtin(_builtin_ge,2))
_endsubp(builtin_ge)

/* %arg_z <- (< %arg_y %arg_z).	  */
_spentry(builtin_lt)
	__(movl %arg_y,%imm0)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpl(%arg_y,%arg_z))
	__(condition_to_boolean(l,%imm0,%arg_z))
	__(ret)
1:	__(jump_builtin(_builtin_lt,2))
_endsubp(builtin_lt)

/* %arg_z <- (<= %arg_y %arg_z).   */
_spentry(builtin_le)
	__(movl %arg_y,%imm0)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpl(%arg_y,%arg_z))
	__(condition_to_boolean(le,%imm0,%arg_z))
	__(ret)
1:	__(jump_builtin(_builtin_le,2))
_endsubp(builtin_le)

_spentry(builtin_eql)
	__(cmpl %arg_y,%arg_z)
	__(je 1f)
	/* Not EQ.  Could only possibly be EQL if both are tag-misc  */
	/* and both have the same subtag. */
	__(movl %arg_y,%imm0)
	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(jne 2f)
	__(movb %arg_z_b,%imm0_bh)
	__(andb $tagmask,%imm0_bh)
	__(cmpb %imm0_bh,%imm0_b)
	__(jne 2f)
	__(extract_subtag(%arg_y,%imm0_b))
	__(extract_subtag(%arg_z,%imm0_bh))
	__(cmpb %imm0_b,%imm0_bh)
	__(jne 2f)
	__(jump_builtin(_builtin_eql,2))
1:	__(movl $t_value,%arg_z)
	__(ret)
2:	__(movl $nil_value,%arg_z)
	__(ret)
_endsubp(builtin_eql)

_spentry(builtin_length)
	__(extract_fulltag(%arg_z,%imm0))
	__(cmpl $tag_list,%imm0)
	__(jz 2f)
	__(andl $tagmask,%imm0)
	__(cmpl $tag_misc,%imm0)
	__(jnz 8f)
	__(extract_subtag(%arg_z,%imm0_b))
	__(rcmpb(%imm0_b,$min_vector_subtag))
	__(jb 8f)
	__(je 1f)
	/* (simple-array * (*)) */
	__(movl %arg_z,%arg_y)
	__(vector_length(%arg_y,%arg_z))
	__(ret)
1:	/* vector header */
	__(movl vectorH.logsize(%arg_z),%arg_z)
	__(ret)
2:	/* list.  Maybe null, maybe dotted or circular. */
	__(movl $-fixnumone,%arg_y)
	__(movl %arg_z,%temp0)	/* fast pointer */
	__(movl %arg_z,%temp1)  /* slow pointer */
3:	__(movb %temp0_b,%al)
	__(andb $fulltagmask,%al)
	__(addl $fixnumone,%arg_y)
	__(compare_reg_to_nil(%temp0))
	__(je 9f)
	__(cmpb $fulltag_cons,%al)
	__(jne 8f)
	__(movb %temp1_b,%ah)
	__(andb $fulltagmask,%ah)
	__(_cdr(%temp0,%temp0))
	__(testl $fixnumone,%arg_y)
	__(je 3b)
	__(cmpb $fulltag_cons,%ah)
	__(jne 8f)
	__(_cdr(%temp1,%temp1))
	__(cmpl %temp0,%temp1)
	__(jne 3b)
8:
	__(jump_builtin(_builtin_length,1))
9:
	__(movl %arg_y,%arg_z)
	__(ret)
_endsubp(builtin_length)

_spentry(builtin_seqtype)
	__(extract_fulltag(%arg_z,%imm0))
	__(cmpb $fulltag_cons,%imm0_b)
	__(jz 1f)
	__(cmpb $tag_misc,%imm0_b)
	__(jne 2f)
	__(movb misc_subtag_offset(%arg_z),%imm0_b)
	__(rcmpb(%imm0_b,$min_vector_subtag))
	__(jb 2f)
	__(movl $nil_value,%arg_z)
	__(ret)
1:	__(movl $t_value,%arg_z)
	__(ret)
2:
	__(jump_builtin(_builtin_seqtype,1))
_endsubp(builtin_seqtype)

_spentry(builtin_assq)
	__(cmpl $nil_value,%arg_z)
	__(je 5f)
1:	__(movl %arg_z,%imm0)
	__(andb $fulltagmask,%imm0_b)
	__(cmpb $fulltag_cons,%imm0_b)
	__(jne 2f)
	__(_car(%arg_z,%temp0))
	__(_cdr(%arg_z,%arg_z))
	__(cmpl $nil_value,%temp0)
	__(je 4f)
	__(movl %temp0,%imm0)
	__(andb $fulltagmask,%imm0_b)
	__(cmpb $fulltag_cons,%imm0_b)
	__(jne 3f)
	__(_car(%temp0,%temp1))
	__(cmpl %temp1,%arg_y)
	__(jne 4f)
	__(movl %temp0,%arg_z)
	__(ret)
4:	__(cmpl $nil_value,%arg_z)
5:	__(jnz 1b)
	__(repret)
2:	__(uuo_error_reg_not_list(Rarg_z))
3:	__(uuo_error_reg_not_list(Rtemp0))
_endsubp(builtin_assq)

_spentry(builtin_memq)
	__(cmpl $nil_value,%arg_z)
	__(jmp 3f)
1:	__(movb $fulltagmask,%imm0_b)
	__(andb %arg_z_b,%imm0_b)
	__(cmpb $fulltag_cons,%imm0_b)
	__(jne 2f)
	__(_car(%arg_z,%temp1))
	__(_cdr(%arg_z,%temp0))
	__(cmpl %temp1,%arg_y)
	__(jz 4f)
	__(cmpl $nil_value,%temp0)
	__(movl %temp0,%arg_z)
3:	__(jnz 1b)
4:	__(repret)
2:	__(uuo_error_reg_not_list(Rarg_z))
_endsubp(builtin_memq)

logbitp_max_bit = 30

_spentry(builtin_logbitp)
	/* Call out unless: both args fixnums, arg_y in `0, logbitp_max_bit) */
	__(movl %arg_z,%imm0)
	__(orl %arg_y,%imm0)
	__(testb $fixnummask,%imm0_b)
	__(jnz 1f)
	__(unbox_fixnum(%arg_y,%imm0))
	__(js 1f)	/* bit number negative */
	__(addb $fixnumshift,%imm0_b)
	__(cmpl $logbitp_max_bit<<fixnumshift,%arg_y)
	__(jb 2f)
	__(movl $logbitp_max_bit-1+fixnumshift,%imm0)
2:	__(bt %imm0,%arg_z)
	__(condition_to_boolean(b,%imm0,%arg_z))
	__(ret)
1:	__(jump_builtin(_builtin_logbitp,2))
_endsubp(builtin_logbitp)

_spentry(builtin_logior)
	__(movl %arg_y,%imm0)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(orl %arg_y,%arg_z)
	__(ret)
1:
	__(jump_builtin(_builtin_logior,2))
_endsubp(builtin_logior)

_spentry(builtin_logand)
	__(movl %arg_y,%imm0)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(andl %arg_y,%arg_z)
	__(ret)
1:
	__(jump_builtin(_builtin_logand,2))
_endsubp(builtin_logand)

_spentry(builtin_negate)
	__(testb $fixnummask,%arg_z_b)
	__(jne 1f)
	__(negl %arg_z)
	__(jo C(fix_one_bit_overflow))
	__(repret)
1:
	__(jump_builtin(_builtin_negate,1))
_endsubp(builtin_negate)

_spentry(builtin_logxor)
	__(movl %arg_y,%imm0)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(xorl %arg_y,%arg_z)
	__(ret)
1:
	__(jump_builtin(_builtin_logxor,2))
_endsubp(builtin_logxor)

/* temp0 = vector, arg_y = index, arg_z = newval */
_spentry(aset1)
	__(extract_typecode(%temp0,%imm0))
	__(box_fixnum(%imm0,%temp1))
	__(cmpb $min_vector_subtag,%imm0_b)
	__(ja _SPsubtag_misc_set)
	/* push frame... */
	__(pop %temp1)
	__(push $reserved_frame_marker)
	__(push $reserved_frame_marker)
	__(push %temp0)
	__(push %temp1)
	/* and fall through... */
_endsubp(aset1)

_spentry(builtin_aset1)
	__(jump_builtin(_builtin_aset1,3))
_endsubp(builtin_aset1)

_spentry(builtin_ash)
	__(movl %arg_y,%imm0)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 9f)
	__(unbox_fixnum(%arg_z,%imm0))
	/* Z flag set if zero ASH shift count */
	__(jnz 1f)
	__(movl %arg_y,%arg_z) /* shift by 0 */
	__(ret)
1:	__(jns 3f)
	__(rcmpl(%imm0,$-31))
	__(jg 2f)
	__(unbox_fixnum(%arg_y,%imm0))
	__(sar $31,%imm0)
	__(box_fixnum(%imm0,%arg_z))
	__(ret)
2:	/* Right-shift by small fixnum */
	__(negb %imm0_b)
	__(movzbl %imm0_b,%ecx)
	__(unbox_fixnum(%arg_y,%imm0))
	__(sar %cl,%imm0)
	__(box_fixnum(%imm0,%arg_z))
	__(ret)
3:	/* Left shift by fixnum.  We can't shift by more than 31 bits, */
	/* though shifting by 32 is actually easy. */
	__(rcmpl(%imm0,$32))
	__(jg 9f)
	__(jne 4f)
	/* left-shift by 32 bits exactly */
	__(unbox_fixnum(%arg_y,%imm0))
        __(movd %imm0,%mm0)
        __(psllq $32,%mm0)
        __(jmp _SPmakes64)
4:	/* left-shift by 1..31 bits. Safe to move shift count to %cl */
	__(movd %imm0,%mm1)     /* shift count */
        __(unbox_fixnum(%arg_y,%imm0))
        __(movd %imm0,%mm0)
        __(sarl $31,%imm0)      /* propagate sign */
        __(movd %imm0,%mm2)
        __(pshufw $0x4e,%mm2,%mm2) /* swap hi/lo halves */
        __(por %mm2,%mm0)
        __(psllq %mm1,%mm0)
        __(jmp _SPmakes64)
9:
	__(jump_builtin(_builtin_ash,2))
_endsubp(builtin_ash)

_spentry(builtin_aref1)
	__(extract_typecode(%arg_y,%imm0))
	__(box_fixnum_no_flags(%imm0,%temp0))
	__(cmpb $min_vector_subtag,%imm0_b)
	__(ja _SPsubtag_misc_ref)
	__(jump_builtin(_builtin_aref1,2))
_endsubp(builtin_aref1)

/* Maybe check the x87 tag word to see if st(0) is valid and pop it */
/* if so.  This might allow us to avoid having to have a priori */
/* knowledge of whether a foreign function returns a floating-point result. */
/* backlink to saved %esp, below */
/* arg n-1 */
/* arg n-2 */
/* ... */
/* arg 0 */
/* space for alignment */
/* previous %esp */

/*
 * Note that we assume that the lisp registers are in the default
 * state here:  that is, tcr.node_regs_mask has its default value,
 * and the DF is clear.
 */
_spentry(ffcall)
LocalLabelPrefix`'ffcall:
	__(unbox_fixnum(%arg_z,%imm0))
	__(testb $fixnummask,%arg_z_b)
	__(je 0f)
	__(movl macptr.address(%arg_z),%imm0)
0:
	/* Save lisp registers. */
	__(push %ebp)
	__(mov %esp,%ebp)
        __(push %temp0) 	 	 
        __(push %temp1) 	 	 
        __(push %arg_y) 	 	 
        __(push %arg_z) 	 	 
        __(push %fn)         
        __ifdef(`WIN32_ES_HACK')
         __(movl rcontext(tcr.linear),%ebx)
        __endif
	__(movl %esp,rcontext(tcr.save_vsp))
	__(movl %ebp,rcontext(tcr.save_ebp))
	__(movl $TCR_STATE_FOREIGN,rcontext(tcr.valence))
	__(movl rcontext(tcr.foreign_sp),%esp)
	__(stmxcsr rcontext(tcr.lisp_mxcsr))
	__(emms)
	__(ldmxcsr rcontext(tcr.foreign_mxcsr))
	__(movl (%esp),%ebp)
LocalLabelPrefix`'ffcall_setup:
        __(lea 15(%esp),%ecx)
        __(andl $-16,%ecx)
        __(movl %ecx,%esp)
/*	__(addl $node_size,%esp) */
        __ifdef(`WIN32_ES_HACK')
         __(push %ds)
         __(pop %es)
        __endif
LocalLabelPrefix`'ffcall_call:
	__(call *%eax)
	__ifdef(`WIN32_ES_HACK')
         __(movw tcr.ldt_selector(%ebx),%rcontext_reg)
        __endif
LocalLabelPrefix`'ffcall_call_end:
	__(movl %ebp,%esp)
	__(movl %esp,rcontext(tcr.foreign_sp))
        /* The high word of a 64-bit result would be in %edx right now.
           There doesn't seem to be any other good place to put this,
           though %edx is often undefined at this point. */
        __(mov %edx,rcontext(tcr.unboxed1))
	__(clr %arg_z)
	__(clr %arg_y)
	__(clr %temp1)
	__(clr %temp0)
	__(clr %fn)
	__(pxor %fpzero,%fpzero)
	__(cmpb $0,C(bogus_fp_exceptions))
	__(je 0f)
	__(movl %arg_z,rcontext(tcr.ffi_exception))
	__(jmp 1f)
0:
	__ifdef(`SSE2_MATH_LIB')
	__(stmxcsr rcontext(tcr.ffi_exception))
	__else
	__(fnstsw rcontext(tcr.ffi_exception))
	__(fnclex)
	__endif
	__(movl rcontext(tcr.save_vsp),%esp)
	__(movl rcontext(tcr.save_ebp),%ebp)
	__(movl $TCR_STATE_LISP,rcontext(tcr.valence))
        __(pop %fn) 	 	 
        __(pop %arg_z) 	 	 
        __(pop %arg_y) 	 	 
        __(pop %temp1) 
       	__(ldmxcsr rcontext(tcr.lisp_mxcsr))
	__(check_pending_interrupt(%temp0))
        __(pop %temp0)
	__(leave)
	__(ret)
	/* need to deal with NSExceptions and Objc-2.0 execptions */
_endsubp(ffcall)

_spentry(ffcall_return_registers)
	__(hlt)
_endsubp(ffcall_return_registers)

/* We need to reserve a frame here if (a) nothing else was already pushed
/* and (b) we push something (e.g., more than 2 args in the lexpr) */
_spentry(spread_lexprz)
	new_local_labels()
	__(movl (%arg_z),%imm0)	/* lexpr count */
        __(leal node_size(%arg_z,%imm0),%arg_y)
	__(movd %arg_y,%mm1)
	__(test %nargs,%nargs) /* anything pushed by caller ? */
        __(jne 0f)              /* yes, caller has already created frame. */
        __(cmpl $(nargregs*node_size),%imm0) /* will we push anything ? */
        __(jbe 0f)
        __(push $reserved_frame_marker)
        __(push $reserved_frame_marker)
0:      __(addl %imm0,%nargs)
        __(cmpl $(1*node_size),%imm0)
        __(ja 2f)
	__(je 1f)
        /* lexpr count was 0; vpop the args that */
        /* were pushed by the caller */
        __(test %nargs,%nargs)
        __(je local_label(all_args_popped))
        __(pop %arg_z)
local_label(maybe_pop_y):
        __(cmpl $(1*node_size),%nargs)
        __(je local_label(all_args_popped))
        __(pop %arg_y)
local_label(all_args_popped):   
        /* If all args fit in registers but some were pushed */
        /* by the caller, discard the reserved frame that the caller */
        /* pushed.         */
        __(cmpl %imm0,%nargs)
        __(je local_label(go))
        __(cmpl $(nargregs*node_size),%nargs)
        __(ja local_label(go))
        __(addl $(2*node_size),%esp)
local_label(go):
        __(jmp *%ra0)

	/* lexpr count is two or more: vpush args from the lexpr until */
	/* we have only two left, then assign them to arg_y and arg_z */
2:	__(cmpl $(2*node_size),%imm0)
	__(je local_label(push_loop_end))
local_label(push_loop):
	__(lea -1*node_size(%imm0),%imm0)
	__(push -node_size(%arg_y))
	__(lea -1*node_size(%arg_y),%arg_y)
	__(cmpl $(2*node_size),%imm0)
	__(jne 2b)
local_label(push_loop_end):
        __(movl -node_size*2(%arg_y),%arg_z)
	__(movl -node_size*1(%arg_y),%arg_y)
        __(jmp *%ra0)
	/* lexpr count is one: set arg_z from the lexpr, */
	/* maybe vpop arg_y  */
1:      __(movl -node_size*1(%arg_y),%arg_z)
        __(jmp local_label(maybe_pop_y))
_endsubp(spread_lexprz)

_spentry(callback)
	__(push %ebp)
	__(movl %esp,%ebp)
	/* C scalar args are already on the stack. */
	/* arg word 0 at 8(%ebp), word 1 at 12(%ebp), etc. */

	/* %eax is passed to us via the callback trampoline.
	   bits 0-22: callback index
	   bit 23: flag, set if we need to discard hidden arg on return
		   (ignored when upper 8 bits are non-zero)
	   bits 24-31: arg words to discard on return (_stdcall for win32) */
	
        /* Reserve some space for results, relative to the
           current %ebp.  We may need quite a bit of it. */
        __(subl $20,%esp)
        __(movl $0,-16(%ebp)) /* No FP result */
	__(btl $23,%eax)      /* set CF if we need to discard hidden arg */
	__(pushfl)	      /* and save for later */
        __(movl %eax,%ecx)    /* extract args-discard count */
        __(shrl $24,%ecx)
        __(andl $0x007fffff,%eax) /* callback index */
        __(movl %ecx,-20(%ebp))
        /* If the C stack is 16-byte aligned by convention,
           it should still be, and this'll be a NOP. */
        __(andl $~15,%esp)
	/* C NVRs */
	__(push %edi)
	__(push %esi)
	__(push %ebx)
	__(push %ebp)
	__(box_fixnum(%eax,%esi))	/* put callback index in arg_y */
        __(cmpb $0,C(rcontext_readonly))
        __(jne 0f)
	__(ref_global(get_tcr,%eax))
	__(subl $12,%esp)		/* alignment */
	__(push $1)			/* stack now 16-byte aligned */
	__(call *%eax)
	__(addl $16,%esp)		/* discard arg, alignment words */
	/* linear TCR addr now in %eax */
	ifdef(`WINDOWS',`
	',`
	__(movw tcr.ldt_selector(%eax), %rcontext_reg)
	')
0:      

        /* ebp is 16-byte aligned, and we've pushed 4 words.  Make
          sure that when we push old foreign_sp, %esp will be 16-byte
          aligned again */
        __(subl $8,%esp)
        __(pushl rcontext(tcr.save_ebp))  /* mark cstack frame's "owner" */
 	__(push rcontext(tcr.foreign_sp))
	__(movl %esp,rcontext(tcr.foreign_sp))
	__(clr %arg_z)
	/* arg_y contains callback index */
	__(clr %temp1)
	__(clr %temp0)
	__(clr %fn)
	__(pxor %fpzero,%fpzero)
	__(movl rcontext(tcr.save_vsp),%esp)
	__(movl %ebp,%arg_z)
	__(movl rcontext(tcr.save_ebp),%ebp)
	__(movl $TCR_STATE_LISP,rcontext(tcr.valence))
	__(stmxcsr rcontext(tcr.foreign_mxcsr))
	__(andb $~mxcsr_all_exceptions,rcontext(tcr.foreign_mxcsr))
	__(ldmxcsr rcontext(tcr.lisp_mxcsr))
	__(movl $nrs.callbacks,%fname)
        __(check_cstack_alignment())
	__(push $local_label(back_from_callback))
	__(set_nargs(2))
	__(jump_fname())
__(tra(local_label(back_from_callback)))
	__(movl %esp,rcontext(tcr.save_vsp))
	__(movl %ebp,rcontext(tcr.save_ebp))
	__(movl $TCR_STATE_FOREIGN,rcontext(tcr.valence))
	__(movl rcontext(tcr.foreign_sp),%esp)
	__(stmxcsr rcontext(tcr.lisp_mxcsr))
	__(emms)
	__(pop rcontext(tcr.foreign_sp))
        __(addl $12,%esp)       /* discard alignment padding */
        __(ldmxcsr rcontext(tcr.foreign_mxcsr))
        __ifdef(`WIN32_ES_HACK')
         __(push %ds)
         __(pop %es)
        __endif
	__(pop %ebp)
	__(pop %ebx)
	__(pop %esi)
	__(pop %edi)
        __(movl -12(%ebp),%ecx) /* magic value for ObjC bridge */
        __(cmpb $1,-16(%ebp))
        __(jae 1f)
	__(movl -8(%ebp),%eax)
        __(movl -4(%ebp),%edx)
        __ifdef(`WIN_32')
	 __(cmpl $0,-20(%ebp))
         __(jne local_label(winapi_return))
	__endif
        /* since we aligned the stack after pushing flags, we're not
           really sure where %esp is relative to where flags were saved.
           We do know where the saved flags are relative to %ebp, so use
           that to establish %esp before the popfl.
        */
        __(lea -24(%ebp),%esp)
	__(popfl)	/* flags from bt way back when */
	__(jc local_label(discard_first_arg))
	__(leave)
	__(ret)
1:      __(jne 2f)
        /* single float return in x87 */
        __(flds -8(%ebp))
        __ifdef(`WIN_32')
	 __(cmpl $0,-20(%ebp))
         __(jne local_label(winapi_return))
        __endif
        __(leave)
	__(ret)
2:      /* double-float return in x87 */
        __(fldl -8(%ebp))
        __ifdef(`WIN_32')
	 __(cmpl $0,-20(%ebp))
         __(jne local_label(winapi_return))
        __endif
        __(leave)
	__(ret)
        __ifdef(`WIN_32')
local_label(winapi_return):
	  __(movl -20(%ebp),%ecx)
	  __(leave)
         /* %ecx is non-zero and contains count of arg words to pop */
          __(popl -4(%esp,%ecx,4))
          __(leal -4(%esp,%ecx,4),%esp)
          __(ret)
        __endif
local_label(discard_first_arg):
	__(leave)
	__(ret $4)
_endsubp(callback)

/* temp0 = array, arg_y = i, arg_z = j. Typecheck everything.
   We don't know whether the array is alleged to be simple or
   not, and don't know anythng about the element type.  */

_spentry(aref2)
        __(testl $fixnummask,%arg_y)
        __(jne 0f)
	__(testb $fixnummask,%arg_z_b)
        __(jne 1f)
	__(extract_typecode(%temp0,%imm0))
        __(cmpb $subtag_arrayH,%imm0_b)
        __(jne 2f)
        __(cmpl $2<<fixnumshift,arrayH.rank(%temp0))
        __(jne 2f)
	__(cmpl arrayH.dim0(%temp0),%arg_y)
        __(jae 3f)
	__(movl arrayH.dim0+node_size(%temp0),%imm0)
        __(cmpl %imm0,%arg_z)
        __(jae 4f)
	__(sarl $fixnumshift,%imm0)
        __(imull %arg_y,%imm0)
        __(addl %imm0,%arg_z)
        __(movl %temp0,%arg_y)
	__(xorl %temp1,%temp1)
6:      __(addl arrayH.displacement(%arg_y),%arg_z)
        __(movl arrayH.data_vector(%arg_y),%arg_y)
        __(extract_subtag(%arg_y,%imm0_b))
        __(cmpb $subtag_vectorH,%imm0_b)
        __(ja C(misc_ref_common))
        __(jmp 6b)
0:	__(uuo_error_reg_not_fixnum(Rarg_y))
1:	__(uuo_error_reg_not_fixnum(Rarg_z))
2:      __(uuo_error_reg_not_type(Rtemp0,error_object_not_array_2d))
3:	__(uuo_error_array_bounds(Rarg_y,Rtemp0))
4:	__(uuo_error_array_bounds(Rarg_z,Rtemp0))

_endsubp(aref2)

/* Like aref2, but temp1 = array, temp0 = i, arg_y = j, arg_z = k */
_spentry(aref3)
	__(testb $fixnummask,%temp0_b)
	__(jne 0f)
	__(testl $fixnummask,%arg_y)
	__(jne 1f)
	__(testb $fixnummask,%arg_z_b)
	__(jne 2f)
	__(extract_typecode(%temp1,%imm0))
	__(cmpb $subtag_arrayH,%imm0_b)
	__(jne 3f)
	__(cmpl $3<<fixnumshift,arrayH.rank(%temp1))
	__(jne 3f)
	__(cmpl arrayH.dim0(%temp1),%temp0)
	__(jae 4f)
	__(movl arrayH.dim0+node_size(%temp1),%imm0)
	__(cmpl %imm0,%arg_y)
	__(jae 5f)
	__(cmpl arrayH.dim0+(node_size*2)(%temp1),%arg_z)
	__(jae 6f)
	/* index computation: k + dim2 * (j + dim1 * i) */
	/* (plus minor fussing for fixnum scaling) */
	__(sarl $fixnumshift,%imm0)
	__(imull %imm0,%temp0)
	__(addl %arg_y,%temp0)
	__(movl arrayH.dim0+(node_size*2)(%temp1),%imm0)
	__(sarl $fixnumshift,%imm0)
	__(imull %imm0,%temp0)
	__(addl %temp0,%arg_z)
	__(movl %temp1,%arg_y)
8:	__(addl arrayH.displacement(%arg_y),%arg_z)
	__(movl arrayH.data_vector(%arg_y),%arg_y)
	__(extract_subtag(%arg_y,%imm0_b))
	__(cmpb $subtag_vectorH,%imm0_b)
	__(ja C(misc_ref_common))
	__(jmp 8b)
0:	__(uuo_error_reg_not_fixnum(Rtemp0))
1:	__(uuo_error_reg_not_fixnum(Rarg_y))
2:	__(uuo_error_reg_not_fixnum(Rarg_z))
3:	__(uuo_error_reg_not_type(Rtemp1,error_object_not_array_3d))
4:	__(uuo_error_array_bounds(Rtemp0,Rtemp1))
5:	__(uuo_error_array_bounds(Rarg_y,Rtemp1))
6:	__(uuo_error_array_bounds(Rarg_z,Rtemp1))
_endsubp(aref3)

/* As with aref2, but temp1 = array, temp0 = i, arg_y = j, arg_z = new_value */
_spentry(aset2)
        __(testb $fixnummask,%temp0_b)
        __(jne 0f)
	__(testl $fixnummask,%arg_y)
        __(jne 1f)
	__(extract_typecode(%temp1,%imm0))
        __(cmpb $subtag_arrayH,%imm0_b)
        __(jne 2f)
        __(cmpl $2<<fixnumshift,arrayH.rank(%temp1))
        __(jne 2f)
	__(cmpl arrayH.dim0(%temp1),%temp0)
        __(jae 3f)
	__(movl arrayH.dim0+node_size(%temp1),%imm0)
        __(cmpl %imm0,%arg_y)
        __(jae 4f)
	__(sarl $fixnumshift,%imm0)
        __(imull %temp0,%imm0)
        __(addl %imm0,%arg_y)
        __(movl %temp1,%temp0)
	__(xorl %temp1,%temp1)
6:      __(addl arrayH.displacement(%temp0),%arg_y)
        __(movl arrayH.data_vector(%temp0),%temp0)
        __(extract_subtag(%temp0,%imm0_b))
        __(cmpb $subtag_vectorH,%imm0_b)
        __(ja C(misc_set_common))
        __(jmp 6b)
0:	__(uuo_error_reg_not_fixnum(Rtemp0))
1:	__(uuo_error_reg_not_fixnum(Rarg_y))
2:      __(uuo_error_reg_not_type(Rtemp1,error_object_not_array_2d))
3:	__(uuo_error_array_bounds(Rtemp0,Rtemp1))
4:	__(uuo_error_array_bounds(Rarg_y,Rtemp1))
_endsubp(aset2)

/* temp1 = array, (%esp) = i, temp0 = j, arg_y = k, arg_z = newval */
_spentry(aset3)
	__(testb $fixnummask,(%esp))
	__(jne 0f)
	__(testb $fixnummask,%temp0_b)
	__(jne 1f)
	__(testl $fixnummask,%arg_y)
	__(jne 2f)
	__(extract_typecode(%temp1,%imm0))
	__(cmpb $subtag_arrayH,%imm0_b)
	__(jne 3f)
	__(cmpl $3<<fixnumshift,arrayH.rank(%temp1))
	__(jne 3f)
	__(movl arrayH.dim0(%temp1),%imm0)
	__(cmpl %imm0,(%esp))	/* i on stack */
	__(jae 4f)
	__(movl arrayH.dim0+node_size(%temp1),%imm0)
	__(cmpl %imm0,%temp0)
	__(jae 5f)
	__(cmpl arrayH.dim0+(node_size*2)(%temp1),%arg_y)
	__(jae 6f)
	/* index computation: k + dim2 * (j + dim1 * i) */
	/* (plus minor fussing for fixnum scaling) */
	__(sarl $fixnumshift,%imm0)
	__(imull (%esp),%imm0)	/* i on stack */
	__(addl %imm0,%temp0)
	__(addl $node_size,%esp)
	__(movl arrayH.dim0+(node_size*2)(%temp1),%imm0)
	__(sarl $fixnumshift,%imm0)
	__(imull %imm0,%temp0)
	__(addl %temp0,%arg_y)
	__(movl %temp1,%temp0)
8:	__(addl arrayH.displacement(%temp0),%arg_y)
	__(movl arrayH.data_vector(%temp0),%temp0)
	__(extract_subtag(%temp0,%imm0_b))
	__(cmpb $subtag_vectorH,%imm0_b)
	__(ja C(misc_set_common))
	__(jmp 8b)
0:	__(pop %temp0)	/* supplied i */
	__(uuo_error_reg_not_fixnum(Rtemp0))
1:	__(uuo_error_reg_not_fixnum(Rtemp0))
2:	__(uuo_error_reg_not_fixnum(Rarg_y))
3:	__(uuo_error_reg_not_type(Rtemp1,error_object_not_array_3d))
4:	__(pop %imm0)	/* supplied i is on stack */
	__(uuo_error_array_bounds(Rimm0,Rtemp1))
5:	__(uuo_error_array_bounds(Rtemp0,Rtemp1))
6:	__(uuo_error_array_bounds(Rarg_y,Rtemp1))
_endsubp(aset3)

/* Prepend all but the first seven (6 words of code & other immediate data,
/* plus inner fn) and last (lfbits) elements of %fn to the "arglist". */
_spentry(call_closure)
	new_local_labels()
	__(vector_length(%fn,%imm0))
	__(subl $8<<fixnumshift,%imm0)	/* imm0 = inherited arg count */
	__(lea (%nargs,%imm0),%temp0)
	__(cmpl $nargregs<<fixnumshift,%temp0)
	__(jna local_label(regs_only))	/* either: 1 arg, 1 inherited, or */
					/* no args, 2 inherited */
	__(pop rcontext(tcr.save0))		/* save return address */
	__(cmpl $nargregs<<fixnumshift,%nargs)
	__(jna local_label(no_insert))

/* Some arguments have already been pushed.  Push %imm0's worth */
/* of NILs, copy those arguments that have already been vpushed from */
/* the old TOS to the new, then insert all of the inherited args */
/* and go to the function. */

	__(mov %imm0,%temp0)
local_label(push_nil_loop):
	__(push $nil_value)
	__(sub $fixnumone,%temp0)
	__(jne local_label(push_nil_loop))

/* Need to use arg regs as temporaries.  Stash them in the spill area. */
	__(movl %arg_y,rcontext(tcr.save1))
	__(movl %arg_z,rcontext(tcr.save2))

	__(leal (%esp,%imm0),%temp0)	/* start of already-pushed args */
	__(leal -nargregs<<fixnumshift(%nargs),%arg_y) /* args pushed */
	__(movd %imm0,%mm0)	/* save inherited arg count */
	__(xorl %imm0,%imm0)
local_label(copy_already_loop):
	__(movl (%temp0,%imm0),%arg_z)
	__(movl %arg_z,(%esp,%imm0))
	__(addl $fixnumone,%imm0)
	__(cmpl %imm0,%arg_y)
	__(jne local_label(copy_already_loop))

	__(lea -node_size(%temp0,%imm0),%arg_y)	/* start of args on stack */
	__(movl $7<<fixnumshift,%temp0)	/* skip code, new fn */
	__(movd %mm0,%imm0)
local_label(insert_loop):
	__(movl misc_data_offset(%fn,%temp0),%arg_z)
	__(addl $node_size,%temp0)
	__(addl $fixnumone,%nargs)
	__(movl %arg_z,(%arg_y))
	__(subl $node_size,%arg_y)
	__(subl $fixnumone,%imm0)
	__(jne local_label(insert_loop))

	/* Recover arg regs, saved earlier */
	__(movl rcontext(tcr.save1),%arg_y)
	__(movl rcontext(tcr.save2),%arg_z)
	__(jmp local_label(go))
	
/* Here if no args were pushed by the caller. */
/* cases: */
/* no args, more than two inherited args */
/* a single arg in arg_z, more than one inherited arg */
/* two args in arg_y and arg_z, some number of inherited args */

/* Therefore, we're always going to have to push something (the sum of */
/* %nargs and %imm0 will always be greater than $nargregs), and */
/* we will have to reserve space for a stack frame. */
/* The 0 args, 2 inherited case and the 1 arg, 1 inherited case get */
/* handled at local_label(regs_ony). */
	
local_label(no_insert):
	/* Reserve space for a stack frame */
	__(push $reserved_frame_marker)
	__(push $reserved_frame_marker)
	__(lea 7<<fixnumshift(%imm0),%temp0)	/* last inherited arg */
	__(rcmpl(%nargs,$fixnumone))
	__(je local_label(set_arg_y))
	__(jb local_label(set_y_z))
	/* %nargs = $nargregs (i.e., 2), vpush remaining inherited vars. */

local_label(vpush_remaining):
	__(movl $7<<fixnumshift,%temp0)
local_label(vpush_remaining_loop):
	__(push misc_data_offset(%fn,%temp0))
	__(add $node_size,%temp0)
	__(add $fixnumone,%nargs)
	__(sub $node_size,%imm0)
	__(jnz local_label(vpush_remaining_loop))
	__(jmp local_label(go))
	
local_label(set_arg_y):
	/* one arg in arg_z.  set arg_y and vpush remaining inherited args */
	__(subl $node_size,%temp0)
	__(movl misc_data_offset(%fn,%temp0),%arg_y)
	__(addl $fixnumone,%nargs)
	__(subl $fixnumone,%imm0)
	__(jmp local_label(vpush_remaining))
local_label(set_y_z):
	__(subl $node_size,%temp0)
	__(movl misc_data_offset(%fn,%temp0),%arg_z)
	__(addl $fixnumone,%nargs)
	__(subl $fixnumone,%imm0)
	__(jmp local_label(set_arg_y))

local_label(go):
	__(movl misc_data_offset+(6*node_size)(%fn),%fn)
	__(push rcontext(tcr.save0))	/* restore return addr */
	__(movapd %fpzero,rcontext(tcr.save0))	/* clear out spill area */
	__(jmp *%fn)
local_label(regs_only):
	__(lea 7<<fixnumshift(%imm0),%temp0)
	__(test %nargs,%nargs)
	__(jne local_label(one_arg))
	/* no args passed, two inherited args */
	__(movl misc_data_offset-node_size(%fn,%temp0),%arg_z)
	__(cmpl $node_size,%imm0)
	__(je local_label(rgo))
	__(movl misc_data_offset-(node_size*2)(%fn,%temp0),%arg_y)
local_label(rgo):
	__(addl %imm0,%nargs)
	__(jmp *misc_data_offset+(6*node_size)(%fn))
local_label(one_arg):
	/* one arg was passed, so there's one inherited arg */
	__(movl misc_data_offset-node_size(%fn,%temp0),%arg_y)
	__(jmp local_label(rgo))
_endsubp(call_closure)

_spentry(poweropen_callbackX)
	__(hlt)
_endsubp(poweropen_callbackX)

_spentry(poweropen_ffcallX)
	__(hlt)
_endsubp(poweropen_ffcallX)

_spentry(eabi_ff_call)
	__(hlt)
_endsubp(eabi_ff_call)

_spentry(eabi_callback)
	__(hlt)
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

_spentry(unused_6)
        __(hlt)
Xspentry_end:
_endsubp(unused_6)
        .data
        .globl C(spentry_start)
        .globl C(spentry_end)
C(spentry_start):       .long Xspentry_start
C(spentry_end):         .long Xspentry_end
        
