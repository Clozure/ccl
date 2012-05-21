/*   Copyright (C) 2005-2009 Clozure Associates  */
/*   This file is part of Clozure CL.    */
 
/*   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public  */
/*   License , known as the LLGPL and distributed with Clozure CL as the  */
/*   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,  */
/*   which is distributed with Clozure CL as the file "LGPL".  Where these  */
/*   conflict, the preamble takes precedence.    */
 
/*   Clozure CL is referenced in the preamble as the "LIBRARY."  */
 
/*   The LLGPL is also available online at  */
/*   http://opensource.franz.com/preamble.html  */



        
/* Indices in %builtin-functions%  */
	
_builtin_plus = 0	/* +-2   */
_builtin_minus = 1	/* --2   */
_builtin_times = 2	/* *-2   */
_builtin_div = 3	/* /-2   */
_builtin_eq = 4		/* =-2   */
_builtin_ne = 5		/* /-2   */
_builtin_gt = 6		/* >-2   */
_builtin_ge = 7		/* >=-2   */
_builtin_lt = 8		/* <-2   */
_builtin_le = 9		/* <=-2   */
_builtin_eql = 10	/* eql   */
_builtin_length = 11	/* length   */
_builtin_seqtype = 12	/* sequence-type   */
_builtin_assq = 13	/* assq   */
_builtin_memq = 14	/* memq   */
_builtin_logbitp = 15	/* logbitp   */
_builtin_logior = 16	/* logior-2   */
_builtin_logand = 17	/* logand-2   */
_builtin_ash = 18	/* ash   */
_builtin_negate = 19	/* %negate   */
_builtin_logxor = 20	/* logxor-2   */
_builtin_aref1 = 21	/* %aref1   */
_builtin_aset1 = 22	/* %aset1   */
	

ifdef(`X8664',`
	include(x86-constants64.s)
',`
	include(x86-constants32.s)
')						

/* registers, as used in destructuring-bind/macro-bind   */
ifdef(`X8664',`
define(`whole_reg',`temp1')
define(`arg_reg',`temp0')
define(`keyvect_reg',`arg_x')
',`
define(`arg_reg',`temp1')
define(`arg_reg_b',`temp1_b')
define(`keyvect_reg',`arg_y')
')

define(`initopt_bit',`24')
define(`keyp_bit',`25') /*  note that keyp can be true even when 0 keys.   */
define(`aok_bit',`26')
define(`restp_bit',`27')
define(`seen_aok_bit',`28')        
        
	
	
		
define(`TCR_STATE_FOREIGN',1)
define(`TCR_STATE_LISP',0)
define(`TCR_STATE_EXCEPTION_WAIT',2)
define(`TCR_STATE_EXCEPTION_RETURN',4)

tstack_alloc_limit = 0xffff
	
mxcsr_ie_bit = 0                /* invalid */
mxcsr_de_bit = 1                /* denorm */        
mxcsr_ze_bit = 2
mxcsr_oe_bit = 3
mxcsr_ue_bit = 4
mxcsr_pe_bit = 5
num_mxcsr_exception_bits = 6

mxcsr_all_exceptions = ((1<<num_mxcsr_exception_bits)-1)

TCR_FLAG_BIT_FOREIGN = fixnum_shift
TCR_FLAG_BIT_AWAITING_PRESET = (fixnum_shift+1)	
TCR_FLAG_BIT_ALT_SUSPEND = (fixnumshift+2)
TCR_FLAG_BIT_PROPAGATE_EXCEPTION = (fixnumshift+3)
TCR_FLAG_BIT_SUSPEND_ACK_PENDING = (fixnumshift+4)
TCR_FLAG_BIT_PENDING_EXCEPTION = (fixnumshift+5)
TCR_FLAG_BIT_FOREIGN_EXCEPTION = (fixnumshift+6)
TCR_FLAG_BIT_PENDING_SUSPEND = (fixnumshift+7)        
TCR_FLAG_BIT_FOREIGN_FPE = (fixnumshift+8)        

CF_BIT = 0
DF_BIT = 10
