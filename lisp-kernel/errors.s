/*   Copyright (C) 1994-2001 Digitool, Inc */
/*   This file is part of OpenMCL. */
 
/*   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public */
/*   License , known as the LLGPL and distributed with OpenMCL as the */
/*   file "LICENSE".  The LLGPL consists of a preamble and the LGPL, */
/*   which is distributed with OpenMCL as the file "LGPL".  Where these */
/*   conflict, the preamble takes precedence. */
 
/*   OpenMCL is referenced in the preamble as the "LIBRARY." */
 
/*   The LLGPL is also available online at */
/*   http://opensource.franz.com/preamble.html */



	

error_reg_errnum = 0		/* "real" (typically negative) error number is in RB */
error_udf = 1
error_udf_call = 2
error_throw_tag_missing = 3
error_alloc_failed = 4
error_stack_overflow = 5
error_excised_function_call = 6
error_too_many_values = 7
error_propagate_suspend = 10
error_interrupt = 11
error_suspend = 12
error_suspend_all = 13
error_resume = 14
error_resume_all = 15					
error_cant_call = 17
        
error_type_error = 128

define([__type_error_counter__],128)
define([def_type_error],[
error_object_not_$1 = __type_error_counter__
        define([__type_error_counter__],eval(__type_error_counter__+1))])

	def_type_error(array)
	def_type_error(bignum)
	def_type_error(fixnum)
	def_type_error(character)
	def_type_error(integer)
	def_type_error(list)
	def_type_error(number)
	def_type_error(sequence)
	def_type_error(simple_string)
	def_type_error(simple_vector)
	def_type_error(string)
	def_type_error(symbol)
	def_type_error(macptr)
	def_type_error(real)
	def_type_error(cons)
	def_type_error(unsigned_byte)
	def_type_error(radix)
	def_type_error(float)
	def_type_error(rational)
	def_type_error(ratio)
	def_type_error(short_float)
	def_type_error(double_float)
	def_type_error(complex)
	def_type_error(vector)
	def_type_error(simple_base_string)
	def_type_error(function)
	def_type_error(unsigned_byte_16)
	def_type_error(unsigned_byte_8)
	def_type_error(unsigned_byte_32)
	def_type_error(signed_byte_32)
	def_type_error(signed_byte_16)
	def_type_error(signed_byte_8)	
	def_type_error(base_character)
	def_type_error(bit)
	def_type_error(unsigned_byte_24)
	def_type_error(u64)
	def_type_error(s64)
        def_type_error(unsigned_byte_56)
        def_type_error(simple_array_double_float_2d)
        def_type_error(simple_array_single_float_2d)
        def_type_error(mod_char_code_limit)
        def_type_error(array_2d)
        def_type_error(array_3d)
        def_type_error(array_t)
        def_type_error(array_bit)
        def_type_error(array_s8)
        def_type_error(array_u8)
        def_type_error(array_s16)
        def_type_error(array_u16)
        def_type_error(array_s32)
        def_type_error(array_u32)
        def_type_error(array_s64)
        def_type_error(array_u64)
        def_type_error(array_fixnum)
        def_type_error(array_single_float)
        def_type_error(array_double_float)
        def_type_error(array_char)
        def_type_error(array_t_2d)
        def_type_error(array_bit_2d)
        def_type_error(array_s8_2d)
        def_type_error(array_u8_2d)
        def_type_error(array_s16_2d)
        def_type_error(array_u16_2d)
        def_type_error(array_s32_2d)
        def_type_error(array_u32_2d)
        def_type_error(array_s64_2d)
        def_type_error(array_u64_2d)
        def_type_error(array_fixnum_2d)
        def_type_error(array_single_float_2d)
        def_type_error(array_double_float_2d)
        def_type_error(array_char_2d)
        def_type_error(simple_array_t_2d)
        def_type_error(simple_array_bit_2d)
        def_type_error(simple_array_s8_2d)
        def_type_error(simple_array_u8_2d)
        def_type_error(simple_array_s16_2d)
        def_type_error(simple_array_u16_2d)
        def_type_error(simple_array_s32_2d)
        def_type_error(simple_array_u32_2d)
        def_type_error(simple_array_s64_2d)
        def_type_error(simple_array_u64_2d)
        def_type_error(simple_array_fixnum_2d)
        def_type_error(simple_array_char_2d)
        def_type_error(array_t_3d)
        def_type_error(array_bit_3d)
        def_type_error(array_s8_3d)
        def_type_error(array_u8_3d)
        def_type_error(array_s16_3d)
        def_type_error(array_u16_3d)
        def_type_error(array_s32_3d)
        def_type_error(array_u32_3d)
        def_type_error(array_s64_3d)
        def_type_error(array_u64_3d)
        def_type_error(array_fixnum_3d)
        def_type_error(array_single_float_3d)
        def_type_error(array_double_float_3d)
        def_type_error(array_char_3d)
        def_type_error(simple_array_t_3d)
        def_type_error(simple_array_bit_3d)
        def_type_error(simple_array_s8_3d)
        def_type_error(simple_array_u8_3d)
        def_type_error(simple_array_s16_3d)
        def_type_error(simple_array_u16_3d)
        def_type_error(simple_array_s32_3d)
        def_type_error(simple_array_u32_3d)
        def_type_error(simple_array_s64_3d)
        def_type_error(simple_array_u64_3d)
        def_type_error(simple_array_fixnum_3d)
        def_type_error(simple_array_single_float_3d)
        def_type_error(simple_array_double_float_3d)
        def_type_error(simple_array_char_3d)
        
	
/* These are the "old" error constants that %ERR-DISP understands */

define([deferr],[
$1 = $2<<fixnumshift])


	deferr(XVUNBND,1)
	deferr(XBADVEC,2)
	deferr(XTMINPS,3)
	deferr(XNEINPS,4)
	deferr(XWRNGINP,5)
	deferr(XFUNBND,6)
	deferr(XSETBADVEC,7)
	deferr(XCOERCE,8)
	deferr(XWRONGSYS,9)
	deferr(XNOMEM,10)
	deferr(XOPENIMAGE,11)
	deferr(XNOTFUN,13)
	deferr(XNOCTAG,33)
	deferr(XNOFPU,36)
	deferr(XBADTOK,49)
	deferr(XFLOVFL,64)
	deferr(XDIVZRO,66)
	deferr(XFLDZRO,66)
	deferr(XMEMFULL,76)
	deferr(XARRLIMIT,77)
	deferr(XSTKOVER,75)
	deferr(XFLEXC,98)
	deferr(XMFULL,-41)

	deferr(XARROOB,112)
	deferr(XCONST,115)
	deferr(XNOSPREAD,120)
	deferr(XFASLVERS,121)
	deferr(XNOTFASL,122)
	deferr(XUDFCALL,123)
	deferr(XWRONGIMAGE,124)

	deferr(XNOPKG,130)
	deferr(XBADFASL,132)
	deferr(XSYMACC,135)
	deferr(XEXPRTC,136)
	deferr(XNDIMS,148)
	deferr(XNARGS,150)
	deferr(XBADKEYS,153)
	deferr(XWRONGTYPE,157)
	deferr(XBADSTRUCT,158)
	deferr(XSTRUCTBOUNDS,159)
	deferr(XCALLNOTLAMBDA,160)
	deferr(XTEMPFLT,161)
	deferr(XCALLTOOMANY,167)
	deferr(XCALLTOOFEW,168)
	deferr(XCALLNOMATCH,169)
	deferr(XIMPROPERLIST,170)
	deferr(XNOFILLPTR,171)
	deferr(XMALADJUST,172)
	deferr(XACCESSNTH,173)
	deferr(XNOTELT,174)
	deferr(XSGEXHAUSTED,175)
	deferr(XSGNARGS,176)
	deferr(XTOOMANYVALUES,177)
        deferr(XSYMNOBIND,178)
	deferr(XFOREIGNEXCEPTION,200)

error_FPU_exception_double = 1024
error_FPU_exception_short = 1025
error_memory_full = 2048
