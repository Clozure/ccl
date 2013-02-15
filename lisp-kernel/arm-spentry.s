/* Copyright (C) 2010 Clozure Associates */
/* This file is part of Clozure CL.   */

/* Clozure CL is licensed under the terms of the Lisp Lesser GNU Public */
/* License , known as the LLGPL and distributed with Clozure CL as the */
/* file "LICENSE".  The LLGPL consists of a preamble and the LGPL, */
/* which is distributed with Clozure CL as the file "LGPL".  Where these */
/* conflict, the preamble takes precedence.   */

/* Clozure CL is referenced in the preamble as the "LIBRARY." */

/* The LLGPL is also available online at */
/* http://opensource.franz.com/preamble.html */



	include(lisp.s)
	_beginfile
	.align 2
	.arm
	.syntax unified

local_label(start):
define(`_spentry',`ifdef(`__func_name',`_endfn',`')
	_startfn(_SP$1)
L__SP$1:                        
	.line  __line__
')


define(`_endsubp',`
	_endfn(_SP$1)
# __line__
')


	

define(`jump_builtin',`
	ref_nrs_value(fname,builtin_functions)
	set_nargs($2)
	vrefr(fname,fname,$1)
	jump_fname()
')

/* Set the _function.entrypoint locative in nfn - which pointed here -
   to the address of the first instruction in the _function.codevector.
   This must be the first ARM subprim. */

_spentry(fix_nfn_entrypoint)
        __(build_lisp_frame(imm0))
        __(vpush1(arg_z))
        __(ldr arg_z,[nfn,#_function.codevector])
        __(add lr,arg_z,#misc_data_offset)
        __(str lr,[nfn,#_function.entrypoint])
        __(vpop1(arg_z))
        __(restore_lisp_frame(imm0))
        __(jump_nfn())

        
_spentry(builtin_plus)
	__(test_two_fixnums(arg_y,arg_z,imm0))
	__(bne 1f)
	__(adds arg_z,arg_y,arg_z)
	__(bxvc lr)
	__(b _SPfix_overflow)
1:
	__(jump_builtin(_builtin_plus,2))
        
_spentry(builtin_minus)
	__(test_two_fixnums(arg_y,arg_z,imm0))
	__(bne 1f)
	__(subs arg_z,arg_y,arg_z)
	__(bxvc lr)
	__(b _SPfix_overflow)
1:
	__(jump_builtin(_builtin_minus,2))

_spentry(builtin_times)
	__(test_two_fixnums(arg_y,arg_z,imm0))
	__(bne 1f)
	__(unbox_fixnum(imm2,arg_z))
        __(unbox_fixnum(imm0,arg_y))
	__(smull imm0,imm1,imm2,imm0)
	__(b _SPmakes64)

1: __(jump_builtin(_builtin_times,2))

_spentry(builtin_div)
        __(jump_builtin(_builtin_div,2))

_spentry(builtin_eq)
	__(test_two_fixnums(arg_y,arg_z,imm0))
	__(bne 1f)
	__(cmp arg_y,arg_z)
	__(mov arg_z,#nil_value)
	__(addeq arg_z,arg_z,#t_offset)
	__(bx lr)        
1:
	__(jump_builtin(_builtin_eq,2))
                        
_spentry(builtin_ne)
	__(test_two_fixnums(arg_y,arg_z,imm0))
	__(bne 1f)
	__(cmp arg_y,arg_z)
	__(mov arg_z,#nil_value)
	__(addne arg_z,arg_z,#t_offset)
	__(bx lr)
1:
	__(jump_builtin(_builtin_ne,2))

_spentry(builtin_gt)
	__(test_two_fixnums(arg_y,arg_z,imm0))
	__(bne 1f)
	__(cmp arg_y,arg_z)
	__(mov arg_z,#nil_value)
	__(addgt arg_z,arg_z,#t_offset)
	__(bx lr)
1:
	__(jump_builtin(_builtin_gt,2))

_spentry(builtin_ge)
	__(test_two_fixnums(arg_y,arg_z,imm0))
	__(bne 1f)
	__(cmp arg_y,arg_z)
	__(mov arg_z,#nil_value)
	__(addge arg_z,arg_z,#t_offset)
	__(bx lr)
1:
	__(jump_builtin(_builtin_ge,2))

_spentry(builtin_lt)
	__(test_two_fixnums(arg_y,arg_z,imm0))
	__(bne 1f)
	__(cmp arg_y,arg_z)
	__(mov arg_z,#nil_value)
	__(addlt arg_z,arg_z,#t_offset) 
	__(bx lr)
1:
	__(jump_builtin(_builtin_lt,2))

_spentry(builtin_le)
	__(test_two_fixnums(arg_y,arg_z,imm0))
	__(bne 1f)
	__(cmp arg_y,arg_z)
	__(mov arg_z,#nil_value)
	__(addle arg_z,arg_z,#t_offset)
	__(bx lr)
1:
	__(jump_builtin(_builtin_le,2))

_spentry(builtin_eql)
0:      __(cmp arg_y,arg_z)
        __(beq 8f)
        __(extract_fulltag(imm0,arg_y))
        __(extract_fulltag(imm1,arg_z))
        __(cmp imm0,imm1)
        __(bne 9f)
        __(cmp imm0,#fulltag_misc)
        __(bne 9f)
        __(extract_subtag(imm0,arg_y))
        __(extract_subtag(imm1,arg_z))
        __(cmp imm0,imm1)
        __(bne 9f)
        __(cmp imm0,#subtag_macptr)
        __(cmpne imm0,#subtag_single_float)
        __(bne 1f)
        __(ldr imm0,[arg_y,#misc_data_offset])
        __(ldr imm1,[arg_z,#misc_data_offset])
        __(cmp imm0,imm1)
        __(mov arg_z,#nil_value)
        __(addeq arg_z,arg_z,#t_offset)
        __(bx lr)
1:      __(cmp imm0,#subtag_double_float)
        __(bne 2f)
        __(ldr imm0,[arg_y,#misc_dfloat_offset])
        __(ldr imm1,[arg_z,#misc_dfloat_offset])
        __(cmp imm0,imm1)
        __(ldreq imm0,[arg_y,#misc_dfloat_offset+node_size])
        __(ldreq imm1,[arg_z,#misc_dfloat_offset+node_size])
        __(cmpeq imm0,imm1)
        __(mov arg_z,#nil_value)
        __(addeq arg_z,arg_z,#t_offset)
        __(bx lr)
2:      __(cmp imm0,#subtag_ratio)
        __(cmpne imm0,#subtag_complex)
        __(bne 3f)
        __(ldr temp0,[arg_y,#ratio.denom])
        __(ldr temp1,[arg_z,#ratio.denom])
        __(stmdb vsp!,{temp0,temp1})
        __(ldr arg_y,[arg_y,#ratio.numer])
        __(ldr arg_z,[arg_z,#ratio.numer])
        __(build_lisp_frame(imm0))
        __(bl 0b)
        __(cmp arg_z,#nil_value)
        __(restore_lisp_frame(imm0))
        __(ldmia vsp!,{arg_z,arg_y})
        __(bne 0b)
        __(mov arg_z,#nil_value)
        __(bx lr)
3:      __(cmp imm0,#subtag_bignum)
        __(bne 9f)
        __(getvheader(imm0,arg_y))
        __(getvheader(imm1,arg_z))
        __(cmp imm0,imm1)
        __(bne 9f)
        __(header_length(temp0,imm0))
        __(mov imm2,#misc_data_offset)
4:      __(ldr imm0,[arg_y,imm2])
        __(ldr imm1,[arg_z,imm2])
        __(cmp imm0,imm1)
        __(bne 9f)
        __(add imm2,imm2,#node_size)
        __(subs temp0,temp0,#fixnumone)
        __(bne 4b)                
8:      __(mov arg_z,#nil_value)
        __(add arg_z,arg_z,#t_offset)
        __(bx lr)
9:      __(mov arg_z,#nil_value)
        __(bx lr)
        
_spentry(builtin_length)
        __(extract_typecode(imm0,arg_z))
        __(cmp imm0,#min_vector_subtag)
        __(ldreq arg_z,[arg_z,#vectorH.logsize])
        __(bxeq lr)
        __(blo 1f)
        __(vector_length(arg_z,arg_z,imm0))
        __(bx lr)
1:      __(cmp imm0,#tag_list)
        __(bne 8f)
        __(mov temp2,#-1<<fixnum_shift)
        __(mov temp0,arg_z) /* fast pointer  */
        __(mov temp1,arg_z) /* slow pointer  */
2:      __(cmp temp0,#nil_value)
        __(add temp2,temp2,#fixnumone)
        __(beq 9f)
        __(extract_lisptag(imm0,temp0))
        __(cmp imm0,#tag_list)
        __(bne 8f)
        __(_cdr(temp0,temp0))
        __(tst temp2,#fixnumone)
        __(beq 2b)
        __(_cdr(temp1,temp1))
        __(cmp temp1,temp0)
        __(bne 2b)
8: 
        __(jump_builtin(_builtin_length,1))
9:      __(mov arg_z,temp2)
        __(bx lr)       

_spentry(builtin_seqtype)
        __(extract_typecode(imm0,arg_z))
        __(cmp imm0,#min_vector_subtag)
        __(movge arg_z,#nil_value)
        __(bxge lr)
        __(cmp imm0,#tag_list)
        __(moveq arg_z,#nil_value)
        __(addeq arg_z,arg_z,#t_offset)
        __(bxeq lr)
        __(jump_builtin(_builtin_seqtype,1))

/* This is usually inlined these days */
_spentry(builtin_assq)
        __(b 2f)
1:      __(trap_unless_list(arg_z,imm0))
        __(_car(arg_x,arg_z))
        __(_cdr(arg_z,arg_z))
        __(cmp arg_x,#nil_value)
        __(beq 2f)
        __(trap_unless_list(arg_x,imm0))
        __(_car(temp0,arg_x))
        __(cmp temp0,arg_y)
        __(bne 2f)
        __(mov arg_z,arg_x)
        __(bx lr)
2:      __(cmp arg_z,#nil_value)
        __(bne 1b)
        __(bx lr)
 
_spentry(builtin_memq)
        __(cmp arg_z,nil_value)
        __(b 2f)
1:      __(trap_unless_list(arg_z,imm0))
        __(_car(arg_x,arg_z))
        __(_cdr(temp0,arg_z))
        __(cmp arg_x,arg_y)
        __(bxeq lr)
        __(cmp temp0,nil_value)
        __(mov arg_z,temp0)
2:      __(bne 1b)
        __(bx lr)

_spentry(builtin_logbitp)
/* Call out unless both fixnums,0 <=  arg_y < logbitp_max_bit  */
        __(test_two_fixnums(arg_y,arg_z,imm0))
        __(bne 1f)
        __(cmp arg_y,#(nbits_in_word-fixnumshift)<<fixnumshift)
        __(bhs 1f)
        __(unbox_fixnum(imm0,arg_y))
        __(mov imm1,#fixnum1)
        __(tst arg_z,imm1,lsl imm0)
        __(mov arg_z,#nil_value)
        __(addne arg_z,arg_z,#t_offset)
        __(bx lr)
1:
        __(jump_builtin(_builtin_logbitp,2))

_spentry(builtin_logior)
        __(orr imm0,arg_y,arg_z)
        __(test_fixnum(imm0))
        __(moveq arg_z,imm0)
        __(bxeq lr)
        __(jump_builtin(_builtin_logior,2))

_spentry(builtin_logand)
        __(test_two_fixnums(arg_y,arg_z,imm0))
        __(andeq arg_z,arg_y,arg_z)
        __(bxeq lr)
        __(jump_builtin(_builtin_logand,2))
          
_spentry(builtin_ash)
        __(test_two_fixnums(arg_y,arg_z,imm0))
        __(bne 9f)
        __(cmp arg_z,#0)
        __(bgt 1f)
        __(moveq arg_z,arg_y)
        __(bxeq lr)
        /* Shift right */
        __(unbox_fixnum(imm2,arg_z))
        __(rsb imm2,imm2,#0)
        __(cmp imm2,#32)
        __(movge imm2,#31)
        __(mov arg_z,#-fixnumone)
        __(and arg_z,arg_z,arg_y,asr imm2)
        __(bx lr)
        /* shift left */
1:      __(unbox_fixnum(imm0,arg_y))
        __(unbox_fixnum(imm2,arg_z))
        __(cmp imm2,#32)
        __(moveq imm1,imm0)
        __(moveq imm0,#0)
        __(beq _SPmakes64)
        __(bgt 9f)
        __(rsb imm1,imm2,#32)
        __(mov imm1,imm0,asr imm1)
        __(mov imm0,imm0,lsl imm2)
        __(b _SPmakes64)
9:  
        __(jump_builtin(_builtin_ash,2))
                                	
_spentry(builtin_negate)
        __(test_fixnum(arg_z))
        __(bne 1f)
        __(rsbs arg_z,arg_z,#0)
        __(bxvc lr)
        __(b _SPfix_overflow)
1:
        __(jump_builtin(_builtin_negate,1))
 
_spentry(builtin_logxor)
        __(test_two_fixnums(arg_y,arg_z,imm0))
        __(eoreq arg_z,arg_y,arg_z)
        __(bxeq lr)
        __(jump_builtin(_builtin_logxor,2))

_spentry(builtin_aref1)
        __(extract_typecode(imm0,arg_y))
        __(cmp imm0,#min_vector_subtag)
        __(box_fixnum(arg_x,imm0))
        __(bgt _SPsubtag_misc_ref)
        __(jump_builtin(_builtin_aref1,2))

_spentry(builtin_aset1)
        __(extract_typecode(imm0,arg_x))
        __(cmp imm0,#min_vector_subtag)
        __(box_fixnum(temp0,imm0))
        __(bgt _SPsubtag_misc_set)
        __(jump_builtin(_builtin_aset1,3))
                	

	/*  Call nfn if it's either a symbol or function */
_spentry(funcall)
	__(funcall_nfn())

/* Subprims for catch, throw, unwind_protect.  */


_spentry(mkcatch1v)
	__(mov imm2,#0)
	__(mkcatch())
	__(bx lr)


_spentry(mkcatchmv)
	__(mov imm2,#fixnum_one)
	__(mkcatch())
	__(bx lr)

_spentry(mkunwind)
        __(mov imm2,#-fixnumone)
        __(mov imm1,#INTERRUPT_LEVEL_BINDING_INDEX)
        __(ldr temp0,[rcontext,#tcr.tlb_pointer])
        __(ldr arg_y,[temp0,#INTERRUPT_LEVEL_BINDING_INDEX])
        __(ldr imm0,[rcontext,#tcr.db_link])
        __(vpush1(arg_y))
        __(vpush1(imm1))
        __(vpush1(imm0))
        __(str imm2,[temp0,#INTERRUPT_LEVEL_BINDING_INDEX])
        __(str vsp,[rcontext,#tcr.db_link])
        __(mov arg_z,#unbound_marker)
        __(mov imm2,#fixnum_one)
        __(mkcatch())
        __(mov arg_z,arg_y)
        __(b _SPbind_interrupt_level)
        

/* This never affects the symbol's vcell  */
/* Non-null symbol in arg_y, new value in arg_z          */
_spentry(bind)
	__(ldr imm1,[arg_y,#symbol.binding_index])
	__(ldr imm0,[rcontext,#tcr.tlb_limit])
	__(cmp imm0,imm1)
        __(bhi 1f)
	__(uuo_tlb_too_small(al,imm1))
1:              
	__(cmp imm1,#0)
	__(ldr imm2,[rcontext,#tcr.tlb_pointer])
	__(ldr imm0,[rcontext,#tcr.db_link])
	__(ldr temp1,[imm2,imm1])
	__(beq 9f)
	__(vpush1(temp1))
	__(vpush1(imm1))
	__(vpush1(imm0))
	__(str arg_z,[imm2,imm1])
	__(str vsp,[rcontext,#tcr.db_link])
	__(bx lr)
9:
	__(mov arg_z,arg_y)
	__(mov arg_y,#XSYMNOBIND)
	__(set_nargs(2))
	__(b _SPksignalerr)

_spentry(conslist)
	__(mov arg_z,#nil_value)
	__(cmp nargs,#0)
	__(b 2f) 
1:
	__(vpop1(arg_y))
	__(Cons(arg_z,arg_y,arg_z))
	__(subs nargs,nargs,#fixnum_one)
2:
	__(bne 1b)
	__(bx lr)

/* do list*: last arg in arg_z, all others vpushed, nargs set to #args vpushed.  */
/* Cons, one cons cell at at time.  Maybe optimize this later.  */

_spentry(conslist_star)
	__(cmp nargs,#0)
	__(b 2f) 
1:
	__(vpop1(arg_y))
	__(Cons(arg_z,arg_y,arg_z))
	__(subs nargs,nargs,fixnum_one)
2:
	__(bne 1b)
	__(bx lr)

_spentry(makes32)
	__(adds imm1,imm0,imm0)
	__(addsvc arg_z,imm1,imm1)
	__(bxvc lr)
	__(movc16(imm1,one_digit_bignum_header))
	__(Misc_Alloc_Fixed(arg_z,imm1,aligned_bignum_size(1)))
	__(str imm0,[arg_z,#misc_data_offset])
	__(bx lr)

/* Construct a lisp integer out of the 32-bit unsigned value in imm0 */


_spentry(makeu32)
	__(tst imm0,#0xe0000000)
	__(box_fixnum(arg_z,imm0))
	__(bxeq lr)
	__(tst imm0,#0x80000000)
	__(bne 2f)
	__(movc16(imm1,one_digit_bignum_header))
	__(Misc_Alloc_Fixed(arg_z,imm1,aligned_bignum_size(1)))
	__(str imm0,[arg_z,#misc_data_offset])
	__(bx lr)
2:              
	__(movc16(imm1,two_digit_bignum_header))
	__(Misc_Alloc_Fixed(arg_z,imm1,aligned_bignum_size(2)))
	__(str imm0,[arg_z,#misc_data_offset])
	__(bx lr)


/* arg_z has overflowed (by one bit) as the result of an addition or
   subtraction. */
/* Make a bignum out of it. */

_spentry(fix_overflow)
	__(unbox_fixnum(imm0,arg_z))
	__(eor imm0,imm0,#0xc0000000)
	__(b _SPmakes32)



/*  Construct a lisp integer out of the 64-bit unsigned value in */
/*           imm0 (low 32 bits) and imm1 (high 32 bits) */
	
_spentry(makeu64)
	__(cmp imm1,#0)
	__(beq _SPmakeu32)
	__(blt 3f)
	__(movc16(imm2,two_digit_bignum_header))
	__(Misc_Alloc_Fixed(arg_z,imm2,aligned_bignum_size(2)))
	__(str imm0,[arg_z,#misc_data_offset])
	__(str imm1,[arg_z,#misc_data_offset+4])
	__(bx lr)
3:              
	__(movc16(imm2,three_digit_bignum_header))
	__(Misc_Alloc_Fixed(arg_z,imm2,aligned_bignum_size(3)))
	__(str imm0,[arg_z,#misc_data_offset])
	__(str imm1,[arg_z,#misc_data_offset+4])
	__(bx lr)

/*  Construct a lisp integer out of the 64-bit signed value in */
/*        imm0 (low 32 bits) and imm1 (high 32 bits). */
_spentry(makes64)
	__(cmp imm1,imm0,asr #31) /* is imm1 sign extension of imm0 ? */
	__(beq _SPmakes32)        /* forget imm1 if so */
	__(movc16(imm2,two_digit_bignum_header))
	__(Misc_Alloc_Fixed(arg_z,imm2,aligned_bignum_size(2)))
	__(str imm0,[arg_z,#misc_data_offset])
	__(str imm1,[arg_z,#misc_data_offset+4])
	__(bx lr)






/* funcall nfn, returning multiple values if it does.  */
_spentry(mvpass)
        __(cmp nargs,#node_size*nargregs)
        __(mov imm1,vsp)
	__(subgt imm1,imm1,#node_size*nargregs)
	__(addgt imm1,imm1,nargs)
	__(build_lisp_frame(imm0,imm1))
	__(adr lr,C(ret1valn))
	__(mov fn,#0)
	__(funcall_nfn())

/* ret1valn returns "1 multiple value" when a called function does not  */
/* return multiple values.  Its presence on the stack (as a return address)  */
/* identifies the stack frame to code which returns multiple values.  */

_exportfn(C(ret1valn))
	__(restore_lisp_frame(imm0))
	__(vpush1(arg_z))
	__(set_nargs(1))
	__(bx lr)

/* Come here to return multiple values when  */
/* the caller's context isn't saved in a lisp_frame.  */
/* lr, fn valid; temp0 = entry vsp  */

_spentry(values)
local_label(return_values):  
	__(ref_global(imm0,ret1val_addr))
	__(mov arg_z,#nil_value)
	__(cmp imm0,lr)
	__(beq 3f)
	__(cmp nargs,#fixnum_one)
	__(add imm0,nargs,vsp)
	__(ldrge arg_z,[imm0,#-node_size])
	__(mov vsp,temp0)
	__(bx lr)


/* Return multiple values to real caller.  */
3:
	__(ldr lr,[sp,#lisp_frame.savelr])
	__(add imm1,nargs,vsp)
	__(ldr imm0,[sp,#lisp_frame.savevsp])
	__(ldr fn,[sp,#lisp_frame.savefn])
	__(cmp imm1,imm0) /* a fairly common case  */
	__(discard_lisp_frame())
	__(bxeq lr) /* already in the right place  */
	__(cmp nargs,#fixnum_one) /* sadly, a very common case  */
	__(bne 4f)
	__(ldr arg_z,[vsp,#0])
	__(mov vsp,imm0)
	__(vpush1(arg_z))
	__(bx lr)
4:
	__(blt 6f)
	__(mov temp1,#fixnum_one)
5:
	__(cmp temp1,nargs)
	__(add temp1,temp1,#fixnum_one)
	__(ldr arg_z,[imm1,#-node_size]!)
	__(push1(arg_z,imm0))
	__(bne 5b)
6:
	__(mov vsp,imm0)
	__(bx lr)


/* Come here with saved context on top of stack.  */
_spentry(nvalret)
	.globl C(nvalret)
C(nvalret): 
	__(ldr lr,[sp,#lisp_frame.savelr])
	__(ldr temp0,[sp,#lisp_frame.savevsp])
	__(ldr fn,[sp,#lisp_frame.savefn])
	__(discard_lisp_frame())
	__(b local_label(return_values))                         

/* Caller has pushed tag and 0 or more values; nargs = nvalues.  */
/* Otherwise, process unwind-protects and throw to indicated catch frame.  */

                
 _spentry(throw)
        __(ldr temp0,[rcontext, #tcr.catch_top])
        __(mov imm0,#0) /* count intervening catch/unwind-protect frames.  */
        __(cmp temp0,#0)
        __(ldr temp2,[vsp,nargs])
        __(beq local_label(_throw_tag_not_found))
local_label(_throw_loop):
        __(ldr temp1,[temp0,#catch_frame.catch_tag])
        __(cmp temp2,temp1)
        __(ldrne temp0,[temp0,#catch_frame.link])
        __(beq C(_throw_found))
        __(cmp temp0,#0)
        __(add imm0,imm0,#fixnum_one)
        __(bne local_label(_throw_loop))
local_label(_throw_tag_not_found):
        __(uuo_error_no_throw_tag(al,temp2))
        __(str temp2,[vsp,nargs])
        __(b _SPthrow)

/* This takes N multiple values atop the vstack.  */
_spentry(nthrowvalues)
        __(mov imm1,#1)
        __(mov temp2,imm0)
        __(str imm1,[rcontext,#tcr.unwinding])
        __(b C(nthrownv))

/* This is a (slight) optimization.  When running an unwind-protect, */
/* save the single value and the throw count in the tstack frame. */
/* Note that this takes a single value in arg_z.  */
_spentry(nthrow1value)
        __(mov imm1,#1)
        __(mov temp2,imm0)
        __(str imm1,[rcontext,#tcr.unwinding])
        __(b C(nthrow1v))


/* arg_z = symbol: bind it to its current value          */
 _spentry(bind_self)
        __(ldr imm1,[arg_z,#symbol.binding_index])
        __(ldr imm0,[rcontext,#tcr.tlb_limit])
        __(cmp imm1,#0)
        __(beq 9f)
        __(cmp imm0,imm1)
        __(bhi 1f)
        __(uuo_tlb_too_small(al,imm1))
1:              
        __(ldr temp2,[rcontext,#tcr.tlb_pointer])
        __(ldr imm0,[rcontext,#tcr.db_link])
        __(ldr temp1,[temp2,imm1])
        __(cmp temp1,#no_thread_local_binding_marker)
        __(movne temp0,temp1)
        __(ldreq temp0,[arg_z,#symbol.vcell])
        __(vpush1(temp1))   /* old tlb contents */
        __(vpush1(imm1))    /* tlb index */
        __(vpush1(imm0))
        __(str temp0,[temp2,imm1])
        __(str vsp,[rcontext,#tcr.db_link])
        __(bx lr)
9:      __(mov arg_y,#XSYMNOBIND)
        __(set_nargs(2))
        __(b _SPksignalerr)

/* Bind symbol in arg_z to NIL                 */
_spentry(bind_nil)
        __(mov arg_y,arg_z)
        __(mov arg_z,#nil_value)
        __(b _SPbind)

/* Bind symbol in arg_z to its current value;  trap if symbol is unbound */
_spentry(bind_self_boundp_check)
        __(ldr imm1,[arg_z,#symbol.binding_index])
        __(ldr imm0,[rcontext,#tcr.tlb_limit])
        __(cmp imm1,#0)
        __(beq 9f)
        __(cmp imm0,imm1)
        __(bhi 1f)
        __(uuo_tlb_too_small(al,imm1))
1:              
        __(ldr temp2,[rcontext,#tcr.tlb_pointer])
        __(ldr imm0,[rcontext,#tcr.db_link])
        __(ldr temp1,[temp2,imm1])
        __(cmp temp1,#no_thread_local_binding_marker)
        __(movne temp0,temp1)
        __(ldreq temp0,[arg_z,#symbol.vcell])
        __(cmp temp0,#unbound_marker)
        __(bne 2f)
        __(uuo_error_unbound(al,arg_z))
2:              
        __(vpush1(temp1))   /* old tlb contents */
        __(vpush1(imm1))    /* tlb index */
        __(vpush1(imm0))
        __(str temp0,[temp2,imm1])
        __(str vsp,[rcontext,#tcr.db_link])
        __(bx lr)
9:      __(mov arg_y,#XSYMNOBIND)
        __(set_nargs(2))
        __(b _SPksignalerr)

 
/* The function pc_luser_xp() - which is used to ensure that suspended threads */
/* are suspended in a GC-safe way - has to treat these subprims (which  */
/* implement the EGC write-barrier) specially.  Specifically, a store that */
/* might introduce an intergenerational reference (a young pointer stored  */
/* in an old object) has to "memoize" that reference by setting a bit in  */
/* the global "refbits" bitmap. */
/* This has to happen atomically, and has to happen atomically wrt GC. */
/* Note that updating a word in a bitmap is itself not atomic, unless we use */
/* interlocked loads and stores. */


/* For RPLACA and RPLACD, things are fairly simple: regardless of where we  */
/* are in the function, we can do the store (even if it's already been done)  */
/* and calculate whether or not we need to set the bit out-of-line.  (Actually */
/* setting the bit needs to be done atomically, unless we're sure that other */
/* threads are suspended.) */
/* We can unconditionally set the suspended thread's PC to its LR. */

        .globl C(egc_write_barrier_start)
_spentry(rplaca)
C(egc_write_barrier_start):     
        __(cmp arg_z,arg_y)
        __(_rplaca(arg_y,arg_z))
        __(bxlo lr)
        __(ref_global(temp0,ref_base))
        __(sub imm0,arg_y,temp0)
        __(mov imm0,imm0,lsr #dnode_shift)
        __(ref_global(imm1,oldspace_dnode_count))
        __(cmp imm0,imm1)
        __(bxhs lr)
        __(and imm2,imm0,#31)
        __(mov imm1,#0x80000000)
        __(mov imm1,imm1,lsr imm2)
        __(mov imm0,imm0,lsr #bitmap_shift)
        __(ref_global(temp0,refbits))
        __(add temp0,temp0,imm0,lsl #word_shift)
        __(ldr imm2,[temp0])
        __(tst imm2,imm1)
        __(bxne lr)
0:      __(ldrex imm2,[temp0])
        __(orr imm2,imm2,imm1)
        __(strex imm0,imm2,[temp0])
        __(cmp imm0,#0)
        __(bne 0b)        
        __(bx lr)


        .globl C(egc_rplacd)
_spentry(rplacd)
C(egc_rplacd):
        __(cmp arg_z,arg_y)
        __(_rplacd(arg_y,arg_z))
        __(bxlo lr)
        __(ref_global(temp0,ref_base))
        __(sub imm0,arg_y,temp0)
        __(mov imm0,imm0,lsr #dnode_shift)
        __(ref_global(imm1,oldspace_dnode_count))
        __(cmp imm0,imm1)
        __(bxhs lr)
        __(and imm2,imm0,#31)
        __(mov imm1,#0x80000000)
        __(mov imm1,imm1,lsr imm2)
        __(mov imm0,imm0,lsr #bitmap_shift)
        __(ref_global(temp0,refbits))
        __(add temp0,temp0,imm0,lsl #word_shift)
        __(ldr imm2,[temp0])
        __(tst imm2,imm1)
        __(bxne lr)
0:      __(ldrex imm2,[temp0])
        __(orr imm2,imm2,imm1)
        __(strex imm0,imm2,[temp0])
        __(cmp imm0,#0)
        __(bne 0b)        
        __(bx lr)
	

/* Storing into a gvector can be handled the same way as storing into a CONS. */

	.globl C(egc_gvset)
_spentry(gvset)
C(egc_gvset):
        __(cmp arg_z,arg_x)
	__(add imm0,arg_y,#misc_data_offset)
	__(str arg_z,[arg_x,imm0])
        __(bxlo lr)               
        __(add imm0,imm0,arg_x)
        __(ref_global(temp0,ref_base))
        __(sub imm0,imm0,temp0)
        __(mov imm0,imm0,lsr #dnode_shift)
        __(ref_global(imm1,oldspace_dnode_count))
        __(cmp imm0,imm1)
        __(bxhs lr)
        __(and imm2,imm0,#31)
        __(mov imm1,#0x80000000)
        __(mov imm1,imm1,lsr imm2)
        __(mov imm0,imm0,lsr #bitmap_shift)
        __(ref_global(temp0,refbits))
        __(add temp0,temp0,imm0,lsl #word_shift)
        __(ldr imm2,[temp0])
        __(tst imm2,imm1)
        __(bxne lr)	
0:      __(ldrex imm2,[temp0])
        __(orr imm2,imm2,imm1)
        __(strex imm0,imm2,[temp0])
        __(cmp imm0,#0)
        __(bne 0b)        
        __(bx lr)

        
/* This is a special case of storing into a gvector: if we need to memoize  */
/* the store, record the address of the hash-table vector in the refmap,  */
/* as well. */
        .globl C(egc_set_hash_key)        
_spentry(set_hash_key)
C(egc_set_hash_key):
        __(cmp arg_z,arg_x)
	__(add imm0,arg_y,#misc_data_offset)
	__(str arg_z,[arg_x,imm0])
        __(bxlo lr)
        __(add imm0,imm0,arg_x)
        __(ref_global(temp0,ref_base))
        __(sub imm0,imm0,temp0)
        __(mov imm0,imm0,lsr #dnode_shift)
        __(ref_global(imm1,oldspace_dnode_count))
        __(cmp imm0,imm1)
        __(bxhs lr)
        __(and imm2,imm0,#31)
        __(mov imm1,#0x80000000)
        __(mov imm1,imm1,lsr imm2)
        __(mov imm0,imm0,lsr #bitmap_shift)
        __(ref_global(temp0,refbits))
        __(add temp0,temp0,imm0,lsl #word_shift)
        __(ldr imm2,[temp0])
        __(tst imm2,imm1)
        __(bxne lr)
0:      __(ldrex imm2,[temp0])
        __(orr imm2,imm2,imm1)
        __(strex imm0,imm2,[temp0])
        __(cmp imm0,#0)
        __(bne 0b)        
/* Now need to ensure that the hash table itself is in the refmap; we
   know that it's in bounds, etc. */
        __(ref_global(temp0,ref_base))
        __(sub imm0,arg_x,temp0)
        __(mov imm0,imm0,lsr #dnode_shift)
        __(and imm2,imm0,#31)
        __(mov imm1,#0x80000000)
        __(mov imm1,imm1,lsr imm2)
        __(mov imm0,imm0,lsr #bitmap_shift)
        __(ref_global(temp0,refbits))
        __(add temp0,temp0,imm0,lsl #word_shift)
        __(ldr imm2,[temp0])
        __(tst imm2,imm1)
        __(bxne lr)
1:      __(ldrex imm2,[temp0])
        __(orr imm2,imm2,imm1)
        __(strex imm0,imm2,[temp0])
        __(cmp imm0,#0)
        __(bne 1b)        
        __(bx lr)
        

/*
   Interrupt handling (in pc_luser_xp()) notes: 
   If we are in this function and before the test which follows the
   conditional (at egc_store_node_conditional), or at that test
   and cr0`eq' is clear, pc_luser_xp() should just let this continue
   (we either haven't done the store conditional yet, or got a
   possibly transient failure.)  If we're at that test and the
   cr0`EQ' bit is set, then the conditional store succeeded and
   we have to atomically memoize the possible intergenerational
   reference.  Note that the local labels 4 and 5 are in the
   body of the next subprim (and at or beyond 'egc_write_barrier_end').

   N.B: it's not possible to really understand what's going on just
   by the state of the cr0`eq' bit.  A transient failure in the
   conditional stores that handle memoization might clear cr0`eq'
   without having completed the memoization.
*/

            .globl C(egc_store_node_conditional)
            .globl C(egc_write_barrier_end)
_spentry(store_node_conditional)
C(egc_store_node_conditional):
        __(vpop1(temp0))
         
1:      __(unbox_fixnum(imm2,temp0))
        __(add imm2,imm2,arg_x)
        __(ldrex temp1,[imm2])
        __(cmp temp1,arg_y)
        __(bne 5f)
        __(strex imm0,arg_z,[imm2])
        .globl C(egc_store_node_conditional_test)
C(egc_store_node_conditional_test): 
        __(cmp imm0,#0)
        __(bne 1b)
        __(cmp arg_z,arg_x)
        __(blo 4f)

        __(ref_global(imm0,ref_base))
        __(ref_global(imm1,oldspace_dnode_count))
        __(sub imm0,imm2,imm0)
        __(mov imm0,imm0,lsr #dnode_shift)
        __(cmp imm0,imm1)
        __(bhs 4f)
        __(and imm1,imm0,#31)
        __(mov arg_x,#0x80000000)
        __(mov imm1,arg_x,lsr imm1)
        __(ref_global(temp0,refbits))
        __(mov imm0,imm0,lsr #bitmap_shift)
        __(add temp0,temp0,imm0,lsl #word_shift)
        __(ldr imm2,[temp0])
        __(tst imm2,imm1)
        __(bxne lr)
2:      __(ldrex imm2,[temp0])
        __(orr imm2,imm2,imm1)
        __(strex imm0,imm2,[temp0])
        .globl C(egc_set_hash_key_conditional_test)
C(egc_set_hash_key_conditional_test): 
        __(cmp imm0,#0)
        __(bne 2b)
        __(b 4f)
 
/* arg_z = new value, arg_y = expected old value, arg_x = hash-vector,
    vsp`0' = (boxed) byte-offset 
    Interrupt-related issues are as in store_node_conditional, but
    we have to do more work to actually do the memoization.*/
_spentry(set_hash_key_conditional)
        .globl C(egc_set_hash_key_conditional)
C(egc_set_hash_key_conditional):
        __(vpop1(imm1))
        __(unbox_fixnum(imm1,imm1))
0:      __(add imm2,arg_x,imm1)
        __(ldrex temp1,[imm2])
        __(cmp temp1,arg_y)
        __(bne 5f)
        __(strex imm0,arg_z,[imm2])
        __(cmp imm0,#0)
        __(bne 0b)
        __(cmp arg_z,arg_x)
        __(blo 4f)
        __(ref_global(temp0,ref_base))
        __(sub imm0,imm2,temp0)
        __(mov imm0,imm0,lsr #dnode_shift)
        __(ref_global(imm1,oldspace_dnode_count))
        __(cmp imm0,imm1)
        __(bhs 4f)
        __(and imm2,imm0,#31)
        __(mov imm1,#0x80000000)
        __(mov imm1,imm1,lsr imm2)
        __(mov imm0,imm0,lsr #bitmap_shift)
        __(ref_global(temp0,refbits))
        __(add temp0,temp0,imm0,lsl #word_shift)
        __(ldr imm2,[temp0])
        __(tst imm2,imm1)
        __(bxne lr)
1:      __(ldrex imm2,[temp0])
        __(orr imm2,imm2,imm1)
        __(strex imm0,imm2,[temp0])
        __(cmp imm0,#0)
        __(bne 1b)        
/* Now need to ensure that the hash table itself is in the refmap; we
   know that it's in bounds, etc. */
        __(ref_global(temp0,ref_base))
        __(sub imm0,arg_x,temp0)
        __(mov imm0,imm0,lsr #dnode_shift)
        __(and imm2,imm0,#31)
        __(mov imm1,#0x80000000)
        __(mov imm1,imm1,lsr imm2)
        __(mov imm0,imm0,lsr #bitmap_shift)
        __(ref_global(temp0,refbits))
        __(add temp0,temp0,imm0,lsl #word_shift)
        __(ldr imm2,[temp0])
        __(tst imm2,imm1)
        __(bxne lr)
1:      __(ldrex imm2,[temp0])
        __(orr imm2,imm2,imm1)
        __(strex imm0,imm2,[temp0])
        __(cmp imm0,#0)
        __(bne 1b)        
C(egc_write_barrier_end):
4:      __(mov arg_z,#nil_value)
        __(add arg_z,arg_z,#t_offset)
        __(bx lr)
5:      __(_clrex(arg_z))
        __(mov arg_z,#nil_value)
        __(bx lr)




	
/* We always have to create a stack frame (even if nargs is 0), so the compiler  */
/* doesn't get confused.  */
_spentry(stkconslist)
        __(mov arg_z,#nil_value)
C(stkconslist_star):           
        __(mov temp2,nargs,lsl #1)
        __(add temp2,temp2,#node_size)
        __(mov imm0,temp2,lsl #num_subtag_bits-word_shift)
        __(add temp2,temp2,#node_size)
        __(orr imm0,imm0,#subtag_u32_vector)
        __(stack_allocate_zeroed_ivector(imm0,temp2))
        __(mov imm0,#subtag_simple_vector)
        __(strb imm0,[sp,#0])
        __(add imm1,sp,#dnode_size+fulltag_cons)
        __(cmp nargs,#0)
        __(b 4f)
1:      __(vpop1(temp0))
        __(_rplaca(imm1,temp0))
        __(_rplacd(imm1,arg_z))
        __(mov arg_z,imm1)
        __(add imm1,imm1,#cons.size)
        __(subs nargs,nargs,#node_size)
4:
        __(bne 1b)
        __(bx lr)
 
/* do list*: last arg in arg_z, all others vpushed,  */
/* nargs set to #args vpushed.  */
_spentry(stkconslist_star)
        __(b C(stkconslist_star))

/* Make a stack-consed simple-vector out of the NARGS objects  */
/* on top of the vstack; return it in arg_z.  */
_spentry(mkstackv)
        __(dnode_align(imm1,nargs,node_size))
        __(mov imm0,nargs,lsl #num_subtag_bits-fixnumshift)
        __(orr imm0,imm0,#subtag_u32_vector)
        __(stack_allocate_zeroed_ivector(imm0,imm1))
        __(mov imm0,#subtag_simple_vector)
        __(strb imm0,[sp,#0])
        __(add arg_z,sp,#fulltag_misc)
        __(add imm0,arg_z,#misc_data_offset)
        __(add imm1,imm0,nargs)
        __(b 4f)
3:      __(vpop1(arg_y))
        __(str arg_y,[imm1,#-node_size]!)
        __(sub nargs,nargs,#node_size)
4:      __(cmp nargs,#0)
        __(bne 3b)
        __(bx lr)
	
_spentry(setqsym)
        __(ldr imm0,[arg_y,#symbol.flags])
        __(tst imm0,#sym_vbit_const_mask)
        __(beq _SPspecset)
        __(mov arg_z,arg_y)
        __(mov arg_y,#XCONST)
        __(set_nargs(2))
        __(b _SPksignalerr)



_spentry(progvsave)
        __(b (C(progvsave)))
 
	
/* Allocate a uvector on the  stack.  (Push a frame on the stack and  */
/* heap-cons the object if there's no room on the stack.)  */
_spentry(stack_misc_alloc)
        __(tst arg_y,#unsigned_byte_24_mask)
        __(beq 1f)
        __(mov arg_x,#XARRLIMIT)
        __(set_nargs(3))
        __(b _SPksignalerr)
1:              
        __(unbox_fixnum(imm2,arg_z))
        __(extract_fulltag(imm1,imm2))
        __(cmp imm1,#fulltag_nodeheader)
        __(bne 1f)
        __(dnode_align(imm1,arg_y,node_size))
        __(mov imm0,#subtag_u32_vector)
        __(orr imm0,imm0,arg_y,lsl #num_subtag_bits-fixnumshift)
        __(b 9f)
1:      __(mov imm0,arg_y,lsl #num_subtag_bits-fixnumshift)
        __(orr imm0,imm0,arg_z,lsr #fixnumshift)
        __(cmp arg_z,#max_32_bit_ivector_subtag<<fixnumshift)
        __(movle imm1,arg_y)
        __(ble 8f)
        __(cmp arg_z,#max_8_bit_ivector_subtag<<fixnumshift)
        __(movle imm1,arg_y,lsr #fixnumshift)
        __(ble 8f)
        __(cmp arg_z,#max_16_bit_ivector_subtag<<fixnumshift)
        __(movle imm1,arg_y,lsr #1)
        __(ble 8f)
        __(cmp arg_z,#subtag_double_float_vector<<fixnumshift)
        __(moveq imm1,arg_y,lsl #1)
        __(addeq imm1,imm1,#node_size)
        __(addne imm1,arg_y,#7<<fixnumshift)
        __(movne imm1,imm1,lsr#3+fixnumshift)
8:      __(dnode_align(imm1,imm1,node_size))
9:      
        __(ldr temp0,[rcontext,tcr.cs_limit])
        __(sub temp1,sp,imm1)
        __(cmp temp1,temp0)
        __(bls stack_misc_alloc_no_room)
        __(mov temp0,#stack_alloc_marker)
        __(mov temp1,sp)
        __(stack_allocate_zeroed_ivector(imm0,imm1))
        __(add arg_z,sp,#fulltag_misc)
        __(strb imm2,[sp])
        __(stmdb sp!,{temp0,temp1})
        __(bx lr)




/* subtype (boxed, of course) is vpushed, followed by nargs bytes worth of  */
/* initial-contents.  Note that this can be used to cons any type of initialized  */
/* node-header'ed misc object (symbols, closures, ...) as well as vector-like  */
/* objects.  */

_spentry(gvector)
        __(sub nargs,nargs,#node_size)
        __(ldr arg_z,[vsp,nargs])
        __(unbox_fixnum(imm0,arg_z))
        __(orr imm0,imm0,nargs,lsl #num_subtag_bits-fixnum_shift)
        __(dnode_align(imm1,nargs,node_size))
        __(Misc_Alloc(arg_z,imm0,imm1))
        __(mov imm1,nargs)
        __(add imm2,imm1,#misc_data_offset)
        __(b 2f)
1:
        __(str temp0,[arg_z,imm2])
2:
        __(sub imm1,imm1,#node_size)
        __(cmp imm1,#0)
        __(sub imm2,imm2,#node_size)
        __(vpop1(temp0))        /* Note the intentional fencepost: */
                                /* discard the subtype as well.  */
        __(bge 1b)
        __(bx lr)

_spentry(fitvals)
        __(subs imm0,imm0,nargs)
        __(mov imm1,#nil_value)
        __(sublt vsp,vsp,imm0)
        __(bxlt lr)
        __(b 2f)
1:
        __(subs imm0,imm0,#node_size)
        __(vpush1(imm1))	
        __(add nargs,nargs,#node_size)
2:
        __(bne 1b)
        __(bx lr)


_spentry(nthvalue)
        __(add imm0,vsp,nargs)
        __(ldr imm1,[imm0,#0])
        __(cmp imm1,nargs) /*  do unsigned compare:  if (n < 0) => nil.  */
        __(mov arg_z,#nil_value)
        __(rsb imm1,imm1,#0)
        __(sub imm1,imm1,#node_size)
        __(ldrlo arg_z,[imm0,imm1])
        __(add vsp,imm0,#node_size)
        __(bx lr)

/* Provide default (NIL) values for &optional arguments; imm0 is  */
/* the (fixnum) upper limit on the total of required and &optional  */
/* arguments.  nargs is preserved, all arguments wind up on the  */
/* vstack.  */
_spentry(default_optional_args)
        __(vpush_argregs())
        __(cmp nargs,imm0)
        __(mov arg_z,#nil_value)
        __(mov imm1,nargs)
        __(bxhs lr)
1: 
        __(add imm1,imm1,#fixnum_one)
        __(cmp imm1,imm0)
        __(vpush1(arg_z))
        __(bne 1b)
        __(bx lr)

/* Indicate whether &optional arguments were actually supplied.  nargs  */
/* contains the actual arg count (minus the number of required args);  */
/* imm0 contains the number of &optional args in the lambda list.  */
/* Note that nargs may be > imm0 if &rest/&key is involved.  */
_spentry(opt_supplied_p)
        __(mov imm1,#0)
        __(mov arg_x,#nil_value)
        __(add arg_x,arg_x,#t_offset)        
1:     
        /* (vpush (< imm1 nargs))  */
        __(cmp imm1,nargs)
        __(add imm1,imm1,#fixnumone)
        __(subeq arg_x,arg_x,#t_offset)
        __(vpush1(arg_x))
        __(cmp imm1,imm0)
        __(bne 1b)
        __(bx lr)

/* Cons a list of length nargs  and vpush it.  */
/* Use this entry point to heap-cons a simple &rest arg.  */
_spentry(heap_rest_arg)
        __(vpush_argregs())
        __(movs imm1,nargs)
        __(mov arg_z,#nil_value)
        __(b 2f)
1:
        __(vpop1(arg_y))
        __(Cons(arg_z,arg_y,arg_z))
        __(subs imm1,imm1,#fixnum_one)
2:
        __(bne 1b)
        __(vpush1(arg_z))
        __(bx lr)

 
/* And this entry point when the argument registers haven't yet been  */
/* vpushed (as is typically the case when required/&rest but no  */
/* &optional/&key.)  */
_spentry(req_heap_rest_arg)
        __(vpush_argregs())
        __(subs imm1,nargs,imm0)
        __(mov arg_z,#nil_value)
        __(b 2f)
1:
        __(vpop1(arg_y))
        __(Cons(arg_z,arg_y,arg_z))
        __(subs imm1,imm1,#fixnum_one)
2:
        __(bgt 1b)
        __(vpush1(arg_z))
        __(bx lr)

/* Here where argregs already pushed */
_spentry(heap_cons_rest_arg)
        __(subs imm1,nargs,imm0)
        __(mov arg_z,#nil_value)
        __(b 2f)
1:
        __(vpop1(arg_y))
        __(Cons(arg_z,arg_y,arg_z))
        __(subs imm1,imm1,#fixnum_one)
2:
        __(bgt 1b)
        __(vpush1(arg_z))
        __(bx lr)


_spentry(check_fpu_exception)
        __(fmrx imm0,fpscr)
        __(mov imm2,imm0)
        __(ldr imm1,[rcontext,#tcr.lisp_fpscr])
        __(ands imm0,imm0,imm1,lsr #8)
        __(bxeq lr)
        __(bic imm2,imm2,#0xff)
        __(fmxr fpscr,imm2)
        __(build_lisp_frame(imm2))
        __(mov imm2,#34<<fixnumshift)
        __(movc16(imm1,make_header(33,subtag_u32_vector)))
        __(stack_allocate_ivector(imm1,imm2))
        __(add arg_z,sp,#fulltag_misc)
        __(str imm0,[arg_z,#misc_data_offset])
        __(add imm0,sp,#dnode_size)
        __(fstmiad imm0,{d0-d15})
        __(ldr imm1,[lr,#-12])
        __(uuo_error_fpu_exception(al,arg_z,imm1))
        __(add imm0,sp,#dnode_size)
        __(fldmiad imm0,{d0-d15})
        __(add sp,sp,#34<<fixnumshift)
        __(return_lisp_frame(imm0))

_spentry(discard_stack_object)
        new_local_labels()        
        __(ldr imm0,[sp,#0])
        __(cmp imm0,#stack_alloc_marker)
        __(ldreq sp,[sp,#node_size])
        __(bxeq lr)
        __(cmp imm0,#lisp_frame_marker)
        __(extract_fulltag(imm1,imm0))
        __(addeq sp,sp,#lisp_frame.size)
        __(bxeq lr)
        __(cmp imm1,#fulltag_immheader)
        __(and imm1,imm0,#subtag_mask)
        __(bic imm0,imm0,#subtag_mask)
        __(beq local_label(ivector))
local_label(word):
        __(mov imm0,imm0,lsr #num_subtag_bits-word_shift)
local_label(out):       
        __(dnode_align(imm0,imm0,node_size))
        __(add sp,sp,imm0)
        __(bx lr)
local_label(ivector):      
        __(cmp imm1,#max_32_bit_ivector_subtag)
        __(bls local_label(word))        
        __(cmp imm1,#max_8_bit_ivector_subtag)
        __(movls imm0,imm0,lsr #num_subtag_bits)
        __(bls local_label(out))
        __(cmp imm1,#max_16_bit_ivector_subtag)
        __(movls imm0,imm0,lsr #num_subtag_bits-1)
        __(bls local_label(out))
        __(cmp imm1,#subtag_bit_vector)
        __(moveq imm0,imm0,lsr #num_subtag_bits)
        __(addeq imm0,imm0,#7)
        __(moveq imm0,imm0,lsr #3)
        __(beq local_label(out))
        /* The infamous 'stack-consed double-float vector' case */
        __(mov imm0,imm0,lsr #num_subtag_bits-dnode_shift)
        __(b local_label(out))


	
/* Signal an error synchronously, via %ERR-DISP.  */
/* If %ERR-DISP isn't fbound, it'd be nice to print a message  */
/* on the C runtime stderr.  */
 
_spentry(ksignalerr)
        __(ref_nrs_symbol(fname,errdisp,imm0))
        __(jump_fname)

/* As in the heap-consed cases, only stack-cons the &rest arg  */
_spentry(stack_rest_arg)
        __(mov imm0,#0)
        __(vpush_argregs())
        __(b _SPstack_cons_rest_arg)

_spentry(req_stack_rest_arg)
        __(vpush_argregs())
        __(b _SPstack_cons_rest_arg)

_spentry(stack_cons_rest_arg)
        __(subs imm1,nargs,imm0)
        __(mov arg_z,#nil_value)
        __(ble 2f)  /* always temp-push something.  */
        __(mov temp0,imm1)
        __(add imm1,imm1,imm1)
        __(add imm1,imm1,#node_size)
        __(dnode_align(imm0,imm1,node_size))
        __(mov imm1,imm1,lsl #num_subtag_bits-fixnumshift)
        __(orr imm1,imm1,#subtag_u32_vector)
        __(sub arg_x,sp,imm0)
        __(ldr arg_y,[rcontext,#tcr.cs_limit])
        __(cmp arg_x,arg_y)
        __(blo 3f)
        __(stack_allocate_zeroed_ivector(imm1,imm0))
        __(mov imm0,#subtag_simple_vector)
        __(strb imm0,[sp])
        __(add imm0,sp,#dnode_size+fulltag_cons)
1:
        __(subs temp0,temp0,#fixnumone)
        __(vpop1(arg_x))
        __(_rplacd(imm0,arg_z))
        __(_rplaca(imm0,arg_x))
        __(mov arg_z,imm0)
        __(add imm0,imm0,#cons.size)
        __(bne 1b)
        __(vpush1(arg_z))
        __(bx lr)
2:
        __(movc16(imm0,make_header(1,subtag_u32_vector)))
        __(mov imm1,#0)
        __(stmdb sp!,{imm0,imm1})
        __(vpush1(arg_z))
        __(bx lr)
3:
        __(mov arg_z,#stack_alloc_marker)
        __(mov arg_y,sp)
        __(stmdb sp!,{arg_z,arg_y})
        __(b _SPheap_cons_rest_arg)

	
/* Prepend all but the first three (entrypoint, closure code, fn) and last two  */
/* (function name, lfbits) elements of nfn to the "arglist".  */
/* functions which take "inherited arguments" work consistently  */
/* even in cases where no closure object is created.  */
_spentry(call_closure)        
        __(cmp nargs,nargregs<<fixnumshift)
        __(vector_length(imm0,nfn,imm0))
        __(sub imm0,imm0,#5<<fixnumshift) /* imm0 = inherited arg count  */
        __(ble local_label(no_insert))
        /* Some arguments have already been vpushed.  Vpush imm0's worth  */
        /* of NILs, copy those arguments that have already been vpushed from  */
        /* the old TOS to the new, then insert all of the inerited args  */
        /* and go to the function.  */
        __(vpush_all_argregs())
        __(mov arg_x,imm0)
        __(mov arg_y,#nil_value)
local_label(push_nil_loop):
        __(subs arg_x,arg_x,#fixnumone)
        __(vpush1(arg_y))
        __(bne local_label(push_nil_loop))
        __(add arg_y,vsp,imm0)
        __(mov imm1,#0)
local_label(copy_already_loop): 
        __(ldr arg_x,[arg_y,imm1])
        __(str arg_x,[vsp,imm1])
        __(add imm1,imm1,#fixnumone)
        __(cmp imm1,nargs)
        __(bne local_label(copy_already_loop))
        __(mov imm1,#misc_data_offset+(3<<fixnumshift))
        __(add arg_y,vsp,nargs)
        __(add arg_y,arg_y,imm0)
local_label(insert_loop):
        __(subs imm0,imm0,#fixnumone)
        __(ldr fname,[nfn,imm1])
        __(add imm1,imm1,#fixnumone)
        __(add nargs,nargs,#fixnumone)
        __(push1(fname,arg_y))
        __(bne local_label(insert_loop))
        __(vpop_all_argregs())
        __(b local_label(go))
local_label(no_insert):
/* nargregs or fewer args were already vpushed.  */
/* if exactly nargregs, vpush remaining inherited vars.  */
        __(cmp nargs,#nargregs<<fixnumshift)
        __(add imm1,imm0,#misc_data_offset+(3<<fixnumshift))
        __(bne local_label(set_regs))
local_label(vpush_remaining):
        __(mov imm1,#misc_data_offset+(3<<fixnumshift))
local_label(vpush_remaining_loop):              
        __(ldr fname,[nfn,imm1])
        __(add imm1,imm1,#fixnum_one)
        __(vpush1(fname))
        __(subs imm0,imm0,#fixnum_one)
        __(add nargs,nargs,#fixnum_one)
        __(bne  local_label(vpush_remaining_loop))
        __(b local_label(go))
local_label(set_regs):
        /* if nargs was > 1 (and we know that it was < 3), it must have  */
        /* been 2.  Set arg_x, then vpush the remaining args.  */
        __(cmp nargs,#fixnumone)
        __(ble local_label(set_y_z))
local_label(set_arg_x):
        __(subs imm0,imm0,#fixnum_one)
        __(sub imm1,imm1,#fixnum_one)
        __(ldr arg_x,[nfn,imm1])
        __(add nargs,nargs,#fixnum_one)
        __(bne local_label(vpush_remaining))
        __(b local_label(go))
        /* Maybe set arg_y or arg_z, preceding args  */
local_label(set_y_z):
        __(cmp nargs,#fixnumone)
        __(bne local_label(set_arg_z))
        /* Set arg_y, maybe arg_x, preceding args  */
local_label(set_arg_y):
        __(subs imm0,imm0,fixnum_one)
        __(sub imm1,imm1,#fixnum_one)
        __(ldr arg_y,[nfn,imm1])
        __(add nargs,nargs,#fixnum_one)
        __(bne local_label(set_arg_x))
        __(b local_label(go))
local_label(set_arg_z):
        __(subs imm0,imm0,#fixnum_one)
        __(sub imm1,imm1,#fixnum_one)
        __(ldr arg_z,[nfn,imm1])
        __(add nargs,nargs,#fixnum_one)
        __(bne local_label(set_arg_y))
 
local_label(go):
        __(vrefr(nfn,nfn,2))
        __(ldr pc,[nfn,#_function.entrypoint])


/* Everything up to the last arg has been vpushed, nargs is set to  */
/* the (boxed) count of things already pushed.  */
/* On exit, arg_x, arg_y, arg_z, and nargs are set as per a normal  */
/* function call (this may require vpopping a few things.)  */
/* ppc2-invoke-fn assumes that temp1 is preserved here.  */
_spentry(spreadargz)
        __(extract_lisptag(imm1,arg_z))
        __(cmp arg_z,#nil_value) 
        __(mov imm0,#0)
        __(mov arg_y,arg_z)  /*  save in case of error  */
        __(beq 2f)
1:
        __(cmp imm1,#tag_list)
        __(bne 3f)
        __(_car(arg_x,arg_z))
        __(_cdr(arg_z,arg_z))
        __(cmp arg_z,#nil_value)
        __(extract_lisptag(imm1,arg_z))
        __(vpush1(arg_x))
        __(add imm0,imm0,#fixnum_one)
        __(bne 1b)
2:
        __(adds  nargs,nargs,imm0)
        __(bxeq lr)
        __(vpop_argregs_nz)
        __(bx lr)
	
        /*  Discard whatever's been vpushed already, complain.  */
3: 
        __(add vsp,vsp,imm0)
        __(mov arg_z,arg_y)  /* recover original arg_z  */
        __(mov arg_y,#XNOSPREAD)
        __(set_nargs(2))
        __(b _SPksignalerr)

/* Tail-recursively funcall temp0.  */
/* Pretty much the same as the tcallsym* cases above.  */
_spentry(tfuncallgen)
        __(cmp nargs,#nargregs<<fixnumshift)
        __(ldr lr,[sp,#lisp_frame.savelr])
        __(ldr fn,[sp,#lisp_frame.savefn])
        __(ble 2f)
        __(ldr imm0,[sp,#lisp_frame.savevsp])
        __(discard_lisp_frame())
        /* can use temp0 as a temporary  */
        __(sub imm1,nargs,#nargregs<<fixnumshift)
        __(add imm1,imm1,vsp)
1:
        __(ldr temp0,[imm1,#-node_size]!)
        __(cmp imm1,vsp)
        __(push1(temp0,imm0))
        __(bne 1b)
        __(mov vsp,imm0)
        __(funcall_nfn())
2:
        __(ldr vsp,[sp,#lisp_frame.savevsp])
        __(discard_lisp_frame())
        __(funcall_nfn())


/* Some args were vpushed.  Slide them down to the base of  */
/* the current frame, then do funcall.  */
_spentry(tfuncallslide)
        __(ldr fn,[sp,#lisp_frame.savefn])
        __(ldr imm0,[sp,#lisp_frame.savevsp])
        __(ldr lr,[sp,#lisp_frame.savelr])
        __(discard_lisp_frame())
        /* can use temp0 as a temporary  */
        __(sub imm1,nargs,#nargregs<<fixnumshift)
        __(add imm1,imm1,vsp)
1:
        __(ldr temp0,[imm1,#-node_size]!)
        __(cmp imm1,vsp)
        __(push1(temp0,imm0))
        __(bne 1b)
        __(mov vsp,imm0)
        __(funcall_nfn())


_spentry(jmpsym)
        __(jump_fname)

/* Tail-recursively call the (known symbol) in fname.  */
/* In the general case, we don't know if any args were  */
/* vpushed or not.  If so, we have to "slide" them down  */
/* to the base of the frame.  If not, we can just restore  */
/* vsp, lr, fn from the saved lisp frame on the control stack.  */
_spentry(tcallsymgen)
        __(cmp nargs,#nargregs<<fixnumshift)
        __(ldr lr,[sp,#lisp_frame.savelr])
        __(ldr fn,[sp,#lisp_frame.savefn])
        __(ble 2f)

        __(ldr imm0,[sp,#lisp_frame.savevsp])
        __(discard_lisp_frame())
        /* can use nfn (= temp2) as a temporary  */
        __(sub imm1,nargs,#nargregs<<fixnumshift)
        __(add imm1,imm1,vsp)
1:
        __(ldr temp2,[imm1,#-node_size]!)
        __(cmp imm1,vsp)
        __(push1(temp2,imm0))
        __(bne 1b)
        __(mov vsp,imm0)
        __(jump_fname)
  
2:  
        __(ldr vsp,[sp,#lisp_frame.savevsp])
        __(discard_lisp_frame())
        __(jump_fname)


/* Some args were vpushed.  Slide them down to the base of  */
/* the current frame, then do funcall.  */
_spentry(tcallsymslide)
        __(ldr lr,[sp,#lisp_frame.savelr])
        __(ldr fn,[sp,#lisp_frame.savefn])
        __(ldr imm0,[sp,#lisp_frame.savevsp])
        __(discard_lisp_frame())
        /* can use nfn (= temp2) as a temporary  */
        __(sub imm1,nargs,#nargregs<<fixnumshift)
        __(add imm1,imm1,vsp)
1:
        __(ldr temp2,[imm1,#-node_size]!)
        __(cmp imm1,vsp)
        __(push1(temp2,imm0))
        __(bne 1b)
        __(mov vsp,imm0)
        __(jump_fname)


/* Tail-recursively call the function in nfn.  */
/* Pretty much the same as the tcallsym* cases above.  */
_spentry(tcallnfngen)
        __(cmp nargs,#nargregs<<fixnumshift)
        __(bgt _SPtcallnfnslide)
        __(restore_lisp_frame(imm0))
        __(jump_nfn())
         
/* Some args were vpushed.  Slide them down to the base of  */
/* the current frame, then do funcall.  */
_spentry(tcallnfnslide)
        __(ldr lr,[sp,#lisp_frame.savelr])
        __(ldr fn,[sp,#lisp_frame.savefn])
        __(ldr imm0,[sp,#lisp_frame.savevsp])
        __(discard_lisp_frame())
        /* Since we have a known function, can use fname as a temporary.  */
        __(sub imm1,nargs,#nargregs<<fixnumshift)
        __(add imm1,imm1,vsp)
1:
        __(ldr fname,[imm1,#-node_size]!)
        __(cmp imm1,vsp)
        __(push1(fname,imm0))
        __(bne 1b)
        __(mov vsp,imm0)
        __(jump_nfn())


/* Reference index arg_z of a misc-tagged object (arg_y).  */
/* Note that this conses in some cases.  Return a properly-tagged  */
/* lisp object in arg_z.  Do type and bounds-checking.  */

_spentry(misc_ref)
        __(trap_unless_fulltag_equal(arg_y,fulltag_misc,imm0))
        __(trap_unless_fixnum(arg_z))
        __(vector_length(imm0,arg_y,imm1))
        __(cmp arg_z,imm0)
        __(blo 1f)
        __(uuo_error_vector_bounds(al,arg_z,arg_y))
1:              
        __(extract_lowbyte(imm1,imm1)) /* imm1 = subtag  */
        __(b C(misc_ref_common)) 

/* like misc_ref, only the boxed subtag is in arg_x.  */

_spentry(subtag_misc_ref)
        __(trap_unless_fulltag_equal(arg_y,fulltag_misc,imm0))
        __(trap_unless_fixnum(arg_z))
        __(vector_length(imm0,arg_y,imm1))
        __(cmp arg_z,imm0)
        __(blo 1f)
        __(uuo_error_vector_bounds(al,arg_z,arg_y))
1:              
        __(unbox_fixnum(imm1,arg_x))
        __(b C(misc_ref_common))


/* Make a "raw" area on the temp stack, stack-cons a macptr to point to it,  */
/* and return the macptr.  Size (in bytes, boxed) is in arg_z on entry; macptr */
/* in arg_z on exit.  */
_spentry(makestackblock)
        __(unbox_fixnum(imm1,arg_z))
        __(dnode_align(imm1,imm1,0))
        __(add imm1,imm1,#node_size)
        __(add imm0,imm1,#node_size)
        __(sub imm2,sp,imm0)
        __(ldr temp0,[rcontext,#tcr.cs_limit])
        __(cmp imm2,temp0)
        __(mov temp0,sp)
        __(bls 1f)
        __(mov imm1,imm1,lsl #num_subtag_bits)
        __(orr imm1,imm1,#subtag_u8_vector)
        __(stack_allocate_ivector(imm1,imm0))
        __(add temp1,sp,#dnode_size)
        __(movc16(imm1,make_header(macptr.element_count,subtag_macptr)))
        __(str imm1,[sp,#-macptr.size]!)
        __(add arg_z,sp,#fulltag_misc)
        __(str temp1,[arg_z,#macptr.address])
        __(mov imm0,#0)
        __(mov imm1,#stack_alloc_marker)
        __(str imm0,[arg_z,#macptr.type])
        __(str imm0,[arg_z,#macptr.domain])
        __(stmdb sp!,{imm1,temp0})
        __(bx lr)

        /* Too big. Heap cons a gcable macptr  */
1:
        __(mov imm1,#stack_alloc_marker)
        __(stmdb sp!,{imm1,temp0})
        __(set_nargs(1))
        __(ref_nrs_symbol(fname,new_gcable_ptr,imm0))
        __(jump_fname())

/* As above, only set the block's contents to 0.  */
_spentry(makestackblock0)
        __(unbox_fixnum(imm1,arg_z))
        __(dnode_align(imm1,imm1,0))
        __(add imm1,imm1,#node_size)
        __(add imm0,imm1,#node_size)
        __(sub imm2,sp,imm0)
        __(ldr temp0,[rcontext,#tcr.cs_limit])
        __(cmp imm2,temp0)
        __(mov temp0,sp)
        __(bls 1f)
        __(mov imm1,imm1,lsl #num_subtag_bits)
        __(orr imm1,imm1,#subtag_u8_vector)
        __(stack_allocate_zeroed_ivector(imm1,imm0))
        __(add temp1,sp,#dnode_size)
        __(movc16(imm1,make_header(macptr.element_count,subtag_macptr)))
        __(str imm1,[sp,#-macptr.size]!)
        __(add arg_z,sp,#fulltag_misc)
        __(str temp1,[arg_z,#macptr.address])
        __(mov imm0,#0)
        __(mov imm1,#stack_alloc_marker)
        __(str imm0,[arg_z,#macptr.type])
        __(str imm0,[arg_z,#macptr.domain])
        __(stmdb sp!,{imm1,temp0})
        __(bx lr)
	
        /* Too big. Heap cons a gcable macptr  */
1:
        __(mov imm1,#stack_alloc_marker)
        __(stmdb sp!,{imm1,temp0})
        __(mov arg_y,arg_z) /* save block size  */
        __(mov arg_z,#nil_value) /* clear-p arg to %new-gcable-ptr  */
        __(add arg_z,arg_z,#t_offset)
        __(set_nargs(2))
        __(ref_nrs_symbol(fname,new_gcable_ptr,imm0))
        __(jump_fname())

/* Make a list of length arg_y (boxed), initial-element arg_z (boxed) on  */
/* the tstack.  Return the list in arg_z.  */
_spentry(makestacklist)
        __(add imm0,arg_y,arg_y)
        __(mov imm1,imm0,lsl #num_subtag_bits-fixnumshift)
        __(add imm1,imm1,#1<<num_subtag_bits)
        __(orr imm1,imm1,#subtag_u32_vector)
        __(add imm0,imm0,#dnode_size)
        __(ldr temp0,[rcontext,#tcr.cs_limit])
        __(sub imm2,sp,imm0)
        __(cmp imm2,temp0)
        __(bls 4f)
        __(stack_allocate_zeroed_ivector(imm1,imm0))
        __(mov imm0,#subtag_simple_vector)
        __(strb imm0,[sp,#0])
        __(add imm2,sp,#dnode_size+fulltag_cons)
        __(movs imm1,arg_y)
        __(mov arg_y,arg_z)
        __(mov arg_z,#nil_value)
        __(b 3f)
2:
        __(_rplacd(imm2,arg_z))
        __(_rplaca(imm2,arg_y))
        __(mov arg_z,imm2)
        __(add imm2,imm2,#cons.size)
        __(subs imm1,imm1,#fixnumone)
3:
        __(bne 2b)
        __(bx lr)
4:
        __(movc16(imm0,make_header(1,subtag_u32_vector)))
        __(str imm0,[sp,#-8]!)
        __(movs imm1,arg_y) /* count  */
        __(mov arg_y,arg_z) /* initial value  */
        __(mov arg_z,#nil_value) /* result  */
        __(b 6f)
5:
        __(Cons(arg_z,arg_y,arg_z))
        __(subs imm1,imm1,#fixnumone)
6:
        __(bne 5b)
        __(bx lr)

/* subtype (boxed) vpushed before initial values. (Had better be a  */
/* node header subtag.) Nargs set to count of things vpushed.  */

_spentry(stkgvector)
        __(sub imm0,nargs,#fixnumone)
        __(ldr temp0,[vsp,imm0])
        __(dnode_align(temp1,imm0,node_size))
        __(mov imm1,imm0,lsl #num_subtag_bits-fixnumshift)
        __(orr imm1,imm1,#subtag_u32_vector)
        __(sub temp2,sp,imm1)
        __(ldr arg_x,[rcontext,#tcr.cs_limit])
        __(cmp temp2,arg_x)       
        __(mov temp2,sp)
        __(mov arg_x,#stack_alloc_marker)
        __(bls 3f)
        __(stack_allocate_zeroed_ivector(imm1,temp1))
        __(unbox_fixnum(imm1,temp0))
        __(strb imm1,[sp])
        __(add arg_z,sp,#fulltag_misc)
        __(add imm0,sp,nargs)
        __(stmdb sp!,{arg_x,temp2})
        __(b 2f)
1:
        __(vpop1(temp0))
        __(push1(temp0,imm0))
2:      __(subs nargs,nargs,#fixnumone)
        __(bne 1b)
        __(add vsp,vsp,#fixnumone)
        __(bx lr)
3:      /* Have to heap-cons. */        
        __(stmdb sp!,{arg_x,temp2})
        __(vpush1(nargs))
        __(mov arg_y,nargs)
        __(mov arg_z,temp0)
        __(build_lisp_frame(imm0))
        __(bl _SPmisc_alloc)
        __(restore_lisp_frame(imm0))
        __(vpop1(nargs))
        __(add imm0,nargs,#misc_data_offset)
        __(b 5f)
4:      __(vpop1(temp0))
        __(subs imm0,imm0,#fixnumone)
        __(str temp0,[arg_z,imm0])
5:      __(subs nargs,nargs,#fixnumone)
        __(bne 4b)
        __(add vsp,vsp,#fixnumone)
        __(bx lr)
        
/* Allocate a "fulltag_misc" object.  On entry, arg_y contains the element  */
/* count (boxed) and  arg_z contains the subtag (boxed).  Both of these   */
/* parameters must be "reasonable" (the  subtag must be valid, the element  */
/* count must be of type (unsigned-byte 24)/(unsigned-byte 56).   */
/* On exit, arg_z contains the (properly tagged) misc object; it'll have a  */
/* proper header on it and its contents will be 0.   imm0 contains   */
/* the object's header (fulltag = fulltag_immheader or fulltag_nodeheader.)  */

_spentry(misc_alloc)
        __(tst arg_y,#unsigned_byte_24_mask)
        __(bne 9f)
        __(unbox_fixnum(imm0,arg_z))
        __(orr imm0,imm0,arg_y,lsl #num_subtag_bits-fixnumshift)
        __(extract_fulltag(imm1,imm0))
        __(cmp imm1,#fulltag_nodeheader)
        __(mov imm2,arg_y)      /* imm2 = logical size in bytes */
        __(beq 1f)
        __(unbox_fixnum(imm1,arg_z))
        __(cmp imm1,#max_32_bit_ivector_subtag)
        __(ble 1f)
        __(mov imm2,arg_y,lsr #2)
        __(cmp imm1,#max_8_bit_ivector_subtag)
        __(ble 1f)
        __(mov imm2,arg_y,lsr #1)
        __(cmp imm1,#max_16_bit_ivector_subtag)
        __(ble 1f)
        __(mov imm2,arg_y,lsl #1)
        __(add imm2,imm2,#node_size)
        __(cmp imm1,#subtag_double_float_vector)
        __(beq 1f)
        __(add imm2,arg_y,#7<<fixnumshift)
        __(mov imm2,imm2,lsr #3+fixnumshift)
        /* imm2 now = byte count.  Add 4 for header, 7 to align, then clear */
        /* low three bits.  */
1:
        __(dnode_align(imm2,imm2,node_size))
        __(Misc_Alloc(arg_z,imm0,imm2))
        __(bx lr)
9:
        __(mov arg_y,#XARRLIMIT)
        __(set_nargs(3))
        __(b _SPksignalerr)



_spentry(atomic_incf_node)
        __(build_lisp_frame(imm0))
        __(add lr,arg_y,arg_z,asr #fixnumshift)
0:      __(ldrex arg_z,[lr])
        __(add arg_z,arg_z,arg_x)
        __(strex imm0,arg_z,[lr])
        __(cmp imm0,#0)
        __(bne 0b)
       /* Return this way, to get something else in the lr */
        __(restore_lisp_frame(imm0))
        __(bx lr)
        
_spentry(unused1)

_spentry(unused2)

/* vpush the values in the value set atop the stack, incrementing nargs.  */

define(`mvcall_older_value_set',`node_size')
define(`mvcall_younger_value_set',`node_size+4')
        

_spentry(recover_values)
        __(add temp0,sp,#dnode_size)
        /* Find the oldest set of values by walking links from the newest */
0:              
        __(ldr temp1,[temp0,#mvcall_older_value_set])
        __(cmp temp1,#0)
        __(movne temp0,temp1)
        __(bne 0b)
1:      __(ldr imm0,[temp0])
        __(header_length(imm0,imm0))
        __(subs imm0,imm0,#2<<fixnumshift)
        __(add temp1,temp0,#node_size+8)
        __(add temp1,temp1,imm0)
        __(b 3f)
2:      __(subs imm0,imm0,#fixnumone)        
        __(ldr arg_z,[temp1,#-node_size]!)
        __(vpush1(arg_z))
        __(add nargs,nargs,#fixnumone)
3:      __(bne 2b)
        __(ldr temp0,[temp0,#mvcall_younger_value_set])
        __(cmp temp0,#0)
        __(bne 1b)
        __(ldr sp,[sp,#node_size])
        __(bx lr)


/* If arg_z is an integer, return in imm0 something whose sign  */
/* is the same as arg_z's.  If not an integer, error.  */
_spentry(integer_sign)
        __(test_fixnum(arg_z))
        __(moveq imm0,arg_z)
        __(bxeq lr)
        __(extract_typecode(imm0,arg_z))
        __(cmp imm0,#subtag_bignum)
        __(beq 1f)
        __(uuo_error_reg_not_xtype(al,arg_z,xtype_integer))
1:              
        __(getvheader(imm1,arg_z))
        __(header_length(imm0,imm1)) /* boxed length = scaled size  */
        __(add imm0,imm0,#misc_data_offset-4) /* bias, less 1 element  */
        __(ldr imm0,[arg_z,imm0])
        __(cmp imm0,#0)
        __(movge imm0,#1)
        __(movlt imm0,#-1)
        __(bx lr)


/* like misc_set, only pass the (boxed) subtag in temp0  */
_spentry(subtag_misc_set)
        __(trap_unless_fulltag_equal(arg_x,fulltag_misc,imm0))
        __(trap_unless_fixnum(arg_y))
        __(vector_length(imm0,arg_x,imm1))
        __(cmp arg_y,imm0)
        __(blo 1f)
        __(uuo_error_vector_bounds(al,arg_y,arg_x))
1:              
        __(unbox_fixnum(imm1,temp0))
        __(b C(misc_set_common))



/* misc_set (vector index newval).  Pretty damned similar to  */
/* misc_ref, as one might imagine.  */

_spentry(misc_set)
        __(trap_unless_fulltag_equal(arg_x,fulltag_misc,imm0))
        __(trap_unless_fixnum(arg_y))
        __(vector_length(imm0,arg_x,imm1))
        __(cmp arg_y,imm0)
        __(blo 1f)
        __(uuo_error_vector_bounds(al,arg_y,arg_x))
1:              
        __(extract_lowbyte(imm1,imm1))
        __(b C(misc_set_common))

/* "spread" the lexpr in arg_z.  */
/* ppc2-invoke-fn assumes that temp1 is preserved here.  */
_spentry(spread_lexprz)
        __(ldr imm0,[arg_z,#0])
        __(add imm1,arg_z,imm0)
        __(add nargs,nargs,imm0)
        __(add imm1,imm1,#node_size)
        __(cmp imm0,#3<<fixnumshift)
        __(bge 9f)
        __(cmp imm0,#2<<fixnumshift)
        __(beq 2f)
        __(cmp imm0,#0)
        __(bne 1f)
/* lexpr count was 0; vpop the arg regs that  */
/* were vpushed by the caller  */
        __(cmp nargs,#0)
        __(bxeq lr)
        __(vpop_argregs_nz)
        __(bx lr)

/* vpush args from the lexpr until we have only  */
/* three left, then assign them to arg_x, arg_y,  */
/* and arg_z.  */
8:
        __(cmp imm0,#4<<fixnumshift)
        __(sub imm0,imm0,#fixnumone)
        __(ldr arg_z,[imm1,#-node_size]!)
        __(vpush1(arg_z))
9:
        __(bne 8b)
        __(ldr arg_x,[imm1,#-node_size*1])
        __(ldr arg_y,[imm1,#-node_size*2])
        __(ldr arg_z,[imm1,#-node_size*3])
        __(bx lr)

/* lexpr count is two: set arg_y, arg_z from the  */
/* lexpr, maybe vpop arg_x  */
2:
        __(cmp nargs,#2<<fixnumshift)
        __(ldr arg_y,[imm1,#-node_size*1])
        __(ldr arg_z,[imm1,#-node_size*2])
        __(bxeq lr)  /* return if (new) nargs = 2  */
        __(vpop1(arg_x))
        __(bx lr)

/* lexpr count is one: set arg_z from the lexpr,  */
/* maybe vpop arg_y, arg_x  */
1: 
        __(cmp nargs,#2<<fixnumshift)
        __(ldr arg_z,[imm1,#-node_size])
        __(bxlt lr)  /* return if (new) nargs < 2  */
        __(vpop1(arg_y))
        __(bxeq lr)  /* return if (new) nargs = 2  */
        __(vpop1(arg_x))
        __(bx lr)


_spentry(reset)
        __(nop)
        __(ref_nrs_value(temp0,toplcatch))
        __(mov temp1,#XSTKOVER)
        __(vpush1(temp0))
        __(vpush1(temp1))
        __(set_nargs(1))
        __(b _SPthrow)


/* "slide" nargs worth of values up the vstack.  IMM0 contains  */
/* the difference between the current VSP and the target.  */
_spentry(mvslide)
        __(cmp nargs,#0)
        __(mov temp1,nargs)
        __(add imm1,vsp,nargs)
        __(add imm1,imm1,imm0)
        __(add imm0,vsp,nargs)
        __(beq 2f)
1:
        __(subs temp1,temp1,#1<<fixnumshift)
        __(ldr temp0,[imm0,#-node_size]!)
        __(str temp0,[imm1,#-node_size]!)
        __(bne 1b)
2:
        __(mov vsp,imm1)
        __(bx lr)

                      
_spentry(save_values)
        __(mov temp1,#0)
        __(mov arg_x,sp)
local_label(save_values_to_tsp):
        __(add imm1,nargs,#node_size*2)
        __(dnode_align(imm0,imm1,node_size))
        __(mov imm1,imm1,lsl #num_subtag_bits-fixnumshift)
        __(orr imm1,imm1,#subtag_u32_vector)
        __(stack_allocate_zeroed_ivector(imm1,imm0))
        __(cmp temp1,$0)
        __(mov imm1,#subtag_simple_vector)
        __(mov arg_y,#stack_alloc_marker)
        __(strb imm1,[sp])
        __(mov temp0,sp)
        __(stmdb sp!,{arg_y,arg_x})
        __(str temp1,[temp0,#mvcall_older_value_set])
        __(strne temp0,[temp1,#mvcall_younger_value_set])
        __(add temp0,temp0,#node_size+8)
        __(mov imm0,#0)
        __(b 2f)
1:      __(vpop1(temp1))
        __(str temp1,[temp0],#node_size)
        __(add imm0,imm0,#node_size)
2:      __(cmp imm0,nargs)
        __(bne 1b)
        __(bx lr)
        
_spentry(add_values)
        __(cmp nargs,#0)
        __(ldr arg_x,[sp,#node_size])
        __(bxeq lr)
        __(add sp,sp,#dnode_size)
        __(mov temp1,sp)
        __(b local_label(save_values_to_tsp))


/* Like misc_alloc (a LOT like it, since it does most of the work), but takes  */
/* an initial-value arg in arg_z, element_count in arg_x, subtag in arg_y.  */
/* Calls out to %init-misc, which does the rest of the work.  */

_spentry(misc_alloc_init)
        __(build_lisp_frame(imm0))
        __(mov fn,#0)
        __(mov temp2,arg_z)  /* initval  */
        __(mov arg_z,arg_y)  /* subtag  */
        __(mov arg_y,arg_x)  /* element-count  */
        __(bl _SPmisc_alloc)
        __(restore_lisp_frame(imm0))
        __(mov arg_y,temp2)
initialize_vector:              
        __(ref_nrs_symbol(fname,init_misc,imm0))
        __(set_nargs(2))
        __(jump_fname())

/* As in stack_misc_alloc above, only with a non-default initial-value.  */
/* Note that this effectively inlines _SPstack_misc_alloc. */                
 
_spentry(stack_misc_alloc_init)
        __(tst arg_x,#unsigned_byte_24_mask)
        __(beq 1f)
        __(uuo_error_reg_not_xtype(al,arg_x,xtype_unsigned_byte_24))
1:              
        __(unbox_fixnum(imm0,arg_y))
        __(extract_fulltag(imm1,imm0))
        __(cmp imm1,#fulltag_nodeheader)
        __(bne stack_misc_alloc_init_ivector)
        __(dnode_align(imm1,arg_x,node_size))
        __(ldr temp1,[rcontext,#tcr.cs_limit])
        __(sub temp0,sp,imm1)
        __(cmp temp0,temp1)
        __(bls stack_misc_alloc_init_no_room)
        __(mov imm0,#subtag_u32_vector)
        __(orr imm0,imm0,arg_x,lsl #num_subtag_bits-fixnumshift)
        __(mov temp0,#stack_alloc_marker)
        __(mov temp1,sp)
        __(stack_allocate_zeroed_ivector(imm0,imm1))
        __(unbox_fixnum(imm0,arg_y))
        __(strb imm0,[sp])
        __(mov arg_y,arg_z)
        __(add arg_z,sp,#fulltag_misc)
        __(stmdb sp!,{temp0,temp1})
        __(b initialize_vector)

 
_spentry(popj)
        .globl C(popj)
C(popj):
        __(return_lisp_frame(imm0))



/* Divide the 64 bit unsigned integer in imm0 (low) and imm1 (high) by
   the 32-bit unsigned integer in imm2; return the quotient in
   imm0:imm1 and remainder in imm2.  We pretty much have to do this
   as an ff call; even if we wrote the code ourselves, we'd have to
   enter foreign context to use as many imm regs as we'd need.
   Moral: don't do integer division on the ARM.
*/
        .globl C(__aeabi_uldivmod)        
_spentry(udiv64by32)
        __(cmp imm2,#0)
        __(bne 1f)
        __(build_lisp_frame(imm2))
        __(bl _SPmakeu64)
        __(mov arg_y,#XDIVZRO)
        __(mov nargs,#2<<fixnumshift)
        __(restore_lisp_frame(imm0))
        __(b _SPksignalerr)
1:              
        __(stmdb vsp!,{arg_z,arg_y,arg_x,temp0,temp1,temp2})
        __(str vsp,[rcontext,#tcr.save_vsp])
        __(mov arg_z,rcontext)
        __(ldr arg_y,[rcontext,#tcr.last_lisp_frame])
        __(build_lisp_frame(r3))
        __(str sp,[arg_z,#tcr.last_lisp_frame])
        __(str allocptr,[arg_z,#tcr.save_allocptr])
        __(mov r3,#TCR_STATE_FOREIGN)
        __(str r3,[arg_z,#tcr.valence])
        __(mov r3,#0)
        __(bl C(__aeabi_uldivmod))
        __(mov rcontext,arg_z)
        __(str arg_y,[rcontext,#tcr.last_lisp_frame])
        __(mov allocptr,#VOID_ALLOCPTR)
        __(mov fn,#0)
        __(mov temp2,#0)
        __(mov temp1,#0)
        __(mov temp0,#0)
        __(mov arg_x,#TCR_STATE_LISP)
        __(str arg_x,[rcontext,#tcr.valence])
        __(ldr allocptr,[rcontext,#tcr.save_allocptr])
        __(ldm vsp!,{arg_z,arg_y,arg_x,temp0,temp1,temp2})
        __(ldr fn,[sp,#lisp_frame.savefn])
        __(ldr lr,[sp,#lisp_frame.savelr])
        __(discard_lisp_frame())
        __(bx lr)


/* arg_z should be of type (UNSIGNED-BYTE 64);  */
/* return high 32 bits in imm1, low 32 bits in imm0 */


_spentry(getu64)
        __(test_fixnum(arg_z))
        __(bne 1f)
        __(unbox_fixnum(imm0,arg_z))
        __(movs imm1,imm0,asr #31)
        __(bxeq lr)
0:              
        __(uuo_error_reg_not_xtype(al,arg_z,xtype_u64))
1:
        __(extract_typecode(imm0,arg_z))
        __(cmp imm0,#subtag_bignum)
        __(bne 0b)
        __(movc16(imm1,two_digit_bignum_header))
        __(getvheader(imm0,arg_z))
        __(cmp imm0,imm1)
        __(bne 2f)
        __(vrefr(imm0,arg_z,0))
        __(vrefr(imm1,arg_z,1))
        __(cmp imm1,#0)
        __(bxge lr)
        __(uuo_error_reg_not_xtype(al,arg_z,xtype_u64))
2:      __(movc16(imm1,three_digit_bignum_header))
        __(cmp imm0,imm1)
        __(bne 3f)
        __(vrefr(imm2,arg_z,2))
        __(cmp imm2,#0)
        __(vrefr(imm1,arg_z,1))
        __(vrefr(imm0,arg_z,0))
        __(bxeq lr)
        __(uuo_error_reg_not_xtype(al,arg_z,xtype_u64))
3:      __(movc16(imm1,one_digit_bignum_header))
        __(cmp imm0,imm1)
        __(bne 0b)
        __(vrefr(imm0,arg_z,0))
        __(mov imm1,#0)
        __(cmp imm0,#0)
        __(bxgt lr)
        __(b 0b)
        __
         
/* arg_z should be of type (SIGNED-BYTE 64);  */
/*    return high 32 bits  in imm1, low 32 bits in imm0  */

_spentry(gets64)
        __(test_fixnum(arg_z))
        __(moveq imm0,arg_z,asr #fixnumshift)
        __(moveq imm1,imm0,asr #31)
        __(bxeq lr)
        __(mov imm2,#0)
        __(extract_lisptag(imm0,arg_z))
        __(cmp imm0,#tag_misc)
        __(movc16(imm1,one_digit_bignum_header))
        __(ldreq imm2,[arg_z,#misc_header_offset])
        __(cmp imm1,imm2)
        __(bne 0f)
        __(vrefr(imm0,arg_z,0))
        __(mov imm1,imm0,asr #31)
        __(bx lr)
0:     
        __(movc16(imm1,two_digit_bignum_header))
        __(cmp imm1,imm2)
        __(beq 1f)
        __(uuo_error_reg_not_xtype(al,arg_z,xtype_s64))
1:              
        __(vrefr(imm1,arg_z,1))
        __(vrefr(imm0,arg_z,0))
        __(bx lr)


/* on entry: arg_z = symbol.  On exit, arg_z = value (possibly */
/* unbound_marker), arg_y = symbol, imm1 = symbol.binding-index  */
_spentry(specref)
        __(ldr imm1,[arg_z,#symbol.binding_index])
        __(ldr imm0,[rcontext,#tcr.tlb_limit])
        __(cmp imm1,imm0)
        __(ldr temp0,[rcontext,#tcr.tlb_pointer])
        __(mov arg_y,arg_z)
        __(movhs imm1,#0)
        __(ldr arg_z,[temp0,imm1])
        __(cmp arg_z,#no_thread_local_binding_marker)
        __(ldreq arg_z,[arg_y,#symbol.vcell])
        __(bx lr)

_spentry(specrefcheck)
        __(ldr imm1,[arg_z,#symbol.binding_index])
        __(ldr imm0,[rcontext,#tcr.tlb_limit])
        __(cmp imm1,imm0)
        __(movhs imm1,#0)
        __(ldr imm0,[rcontext,#tcr.tlb_pointer])
        __(mov arg_y,arg_z)
        __(ldr arg_z,[imm0,imm1])
        __(cmp arg_z,#no_thread_local_binding_marker)
        __(ldreq arg_z,[arg_y,#symbol.vcell])
        __(cmp arg_z,#unbound_marker)
        __(bxne lr)
        __(uuo_error_unbound(al,arg_y))
        __(bx lr)

/* arg_y = special symbol, arg_z = new value.          */
_spentry(specset)
        __(ldr imm1,[arg_y,#symbol.binding_index])
        __(ldr imm0,[rcontext,#tcr.tlb_limit])
        __(ldr imm2,[rcontext,#tcr.tlb_pointer])
        __(cmp imm1,imm0)
        __(movge imm1,#0)
        __(ldr temp1,[imm2,imm1])
        __(cmp temp1,#no_thread_local_binding_marker)
        __(strne arg_z,[imm2,imm1])
        __(bxne lr)
        __(mov arg_x,arg_y)
        __(mov arg_y,#symbol.vcell-misc_data_offset)
        __(b _SPgvset)


	
/* Construct a lisp integer out of the 32-bit signed value in imm0 */
/* arg_z should be of type (SIGNED-BYTE 32); return unboxed result in imm0 */

_spentry(gets32)
        __(test_fixnum(arg_z))
        __(moveq imm0,arg_z,asr #fixnumshift)
        __(bxeq lr)
        __(extract_lisptag(imm0,arg_z))
        __(cmp imm0,#tag_misc)
        __(beq 1f)
        __(uuo_error_reg_not_xtype(al,arg_z,xtype_s32))
1:              
        __(getvheader(imm0,arg_z))
        __(movc16(imm1,one_digit_bignum_header))
        __(cmp imm0,imm1)
        __(beq 2f)
        __(uuo_error_reg_not_xtype(al,arg_z,xtype_s32))
2:              
        __(vrefr(imm0,arg_z,0))
        __(bx lr)        


/*  */
/* arg_z should be of type (UNSIGNED-BYTE 32); return unboxed result in imm0 */
/*  */

_spentry(getu32)
        __(test_fixnum(arg_z))
        __(moveq imm0,arg_z,asr #fixnumshift)
        __(movseq imm1,imm0,asr #31)
        __(bxeq lr)
        __(movc16(imm1,one_digit_bignum_header))
        __(extract_lisptag(imm0,arg_z))
        __(cmp imm0,#tag_misc)
        __(beq 1f)
        __(uuo_error_reg_not_xtype(al,arg_z,xtype_u32))
1:              
        __(getvheader(imm0,arg_z))
        __(cmp imm0,imm1)
        __(ldreq imm0,[arg_z,#misc_data_offset])
        __(beq 7f)
        __(movc16(imm1,two_digit_bignum_header))
        __(cmp imm0,imm1)
        __(ldreq imm0,[arg_z,#misc_data_offset])
        __(ldreq imm1,[arg_z,#misc_data_offset+4])
        __(cmpeq imm1,#0)
        __(bxeq lr)
        __(uuo_error_reg_not_xtype(al,arg_z,xtype_u32))
7:              
        __(movs imm1,imm0,asr #31)
        __(bxeq lr)
        __(uuo_error_reg_not_xtype(al,arg_z,xtype_u32))


/* */
/* As per mvpass above, but in this case fname is known to be a */
/* symbol. */

_spentry(mvpasssym)
        __(cmp nargs,#node_size*nargregs)
        __(mov imm1,vsp)
	__(subgt imm1,imm1,#node_size*nargregs)
	__(addgt imm1,imm1,nargs)
	__(build_lisp_frame(imm0,imm1))
        __(ref_global(lr,ret1val_addr,imm0))
        __(mov fn,#0)
        __(jump_fname())

_spentry(unbind)
        __(ldr imm1,[rcontext,#tcr.db_link])
        __(ldr temp0,[rcontext,#tcr.tlb_pointer])   
        __(ldr imm0,[imm1,#binding.sym])
        __(ldr temp1,[imm1,#binding.val])
        __(ldr imm1,[imm1,#binding.link])
        __(str temp1,[temp0,imm0])
        __(str imm1,[rcontext,#tcr.db_link])
        __(bx lr)

/* Clobbers imm1,temp0,arg_x, arg_y */        
_spentry(unbind_n)
        __(ldr imm1,[rcontext,#tcr.db_link])
        __(ldr arg_x,[rcontext,#tcr.tlb_pointer])
1:      __(ldr temp0,[imm1,#binding.sym])
        __(ldr arg_y,[imm1,#binding.val])
        __(ldr imm1,[imm1,#binding.link])
        __(subs imm0,imm0,#1)
        __(str arg_y,[arg_x,temp0])
        __(bne 1b)
        __(str imm1,[rcontext,#tcr.db_link])
        __(bx lr)

/* */
/* Clobbers imm1,temp0,arg_x, arg_y */

_spentry(unbind_to)
        do_unbind_to(imm1,temp1,arg_x,arg_y)
        __(bx lr)
 

 
/* */
/* Restore the special bindings from the top of the tstack,  */
/* leaving the tstack frame allocated.  */
/* Note that there might be 0 saved bindings, in which case  */
/* do nothing.  */
/* Note also that this is -only- called from an unwind-protect  */
/* cleanup form, and that .SPnthrowXXX is keeping one or more  */
/* values in a frame on top of the tstack.  */
/*  */
                         
_spentry(progvrestore)
        __(skip_stack_vector(imm0,imm1,sp))
        __(ldr imm0,[imm0,#lisp_frame.size+(9*8)+node_size]) /* 7*8 = size of saved FPR vector, with header */
        __(cmp imm0,#0)
        __(unbox_fixnum(imm0,imm0))
        __(bne _SPunbind_n)
        __(bx lr)

/* Bind CCL::*INTERRUPT-LEVEL* to 0.  If its value had been negative, check  */
/* for pending interrupts after doing so.  */
_spentry(bind_interrupt_level_0)
        __(ldr temp1,[rcontext,#tcr.tlb_pointer])
        __(ldr temp0,[temp1,#INTERRUPT_LEVEL_BINDING_INDEX])
        __(ldr imm0,[rcontext,#tcr.db_link])
        __(cmp temp0,#0)
        __(mov imm1,#INTERRUPT_LEVEL_BINDING_INDEX)
        __(vpush1(temp0))
        __(vpush1(imm1))
        __(vpush1(imm0))
        __(mov imm0,#0)
        __(str imm0,[temp1,#INTERRUPT_LEVEL_BINDING_INDEX])
        __(str vsp,[rcontext,#tcr.db_link])
        __(bxeq lr)
        __(ldrlt temp0,[rcontext,#tcr.interrupt_pending])
        __(cmp temp0,#0)
        __(bxle lr)
        __(uuo_interrupt_now(al))
        __(bx lr)
	
/* Bind CCL::*INTERRUPT-LEVEL* to the fixnum -1.  (This has the effect */
/* of disabling interrupts.)  */
_spentry(bind_interrupt_level_m1)
        __(mov imm2,#-fixnumone)
        __(mov imm1,#INTERRUPT_LEVEL_BINDING_INDEX)
        __(ldr temp1,[rcontext,#tcr.tlb_pointer])
        __(ldr temp0,[temp1,#INTERRUPT_LEVEL_BINDING_INDEX])
        __(ldr imm0,[rcontext,#tcr.db_link])
        __(vpush1(temp0))
        __(vpush1(imm1))
        __(vpush1(imm0))
        __(str imm2,[temp1,#INTERRUPT_LEVEL_BINDING_INDEX])
        __(str vsp,[rcontext,tcr.db_link])
        __(bx lr)
	

/* Bind CCL::*INTERRUPT-LEVEL* to the value in arg_z.  If that value's 0, */
/* do what _SPbind_interrupt_level_0 does  */
_spentry(bind_interrupt_level)
        __(cmp arg_z,#0)
        __(mov imm1,#INTERRUPT_LEVEL_BINDING_INDEX)
        __(ldr temp1,[rcontext,#tcr.tlb_pointer])
        __(ldr temp0,[temp1,#INTERRUPT_LEVEL_BINDING_INDEX])
        __(ldr imm0,[rcontext,#tcr.db_link])
        __(beq _SPbind_interrupt_level_0)
        __(vpush1(temp0))
        __(vpush1(imm1))
        __(vpush1(imm0))
        __(str arg_z,[temp1,INTERRUPT_LEVEL_BINDING_INDEX])
        __(str vsp,[rcontext,#tcr.db_link])
        __(bx lr)

/* Unbind CCL::*INTERRUPT-LEVEL*.  If the value changes from negative to */
/* non-negative, check for pending interrupts.  This is often called in */
/* a context where nargs is significant, so save and restore nargs around */
/* any interrupt polling  */
         
_spentry(unbind_interrupt_level)
        __(ldr imm0,[rcontext,#tcr.flags])
        __(ldr temp2,[rcontext,#tcr.tlb_pointer])
        __(tst imm0,#1<<TCR_FLAG_BIT_PENDING_SUSPEND)
        __(ldr imm0,[rcontext,#tcr.db_link])
        __(ldr temp0,[temp2,#INTERRUPT_LEVEL_BINDING_INDEX])
        __(bne 5f)
0:      
        __(ldr temp1,[imm0,#binding.val])
        __(ldr imm0,[imm0,#binding.link])
        __(str temp1,[temp2,#INTERRUPT_LEVEL_BINDING_INDEX])
        __(str imm0,[rcontext,#tcr.db_link])
        __(cmp temp0,#0)
        __(bxge lr)
        __(cmp temp1,#0)
        __(bxlt lr)
        __(check_enabled_pending_interrupt(imm0,1f))
1:              
        __(bx lr)
5:       /* Missed a suspend request; force suspend now if we're restoring
          interrupt level to -1 or greater */
        __(cmp temp0,#-2<<fixnumshift)
        __(bne 0b)
        __(ldr imm0,[imm1,#binding.val])
        __(cmp imm0,temp0)
        __(beq 0b)
        __(mov imm0,#1<<fixnumshift)
        __(str imm0,[temp2,INTERRUPT_LEVEL_BINDING_INDEX])
        __(suspend_now())
        __(b 0b)
 
 
/* arg_x = array, arg_y = i, arg_z = j. Typecheck everything.
    We don't know whether the array is alleged to be simple or
   not, and don't know anythng about the element type.  */
_spentry(aref2)
        __(trap_unless_fixnum(arg_y))
        __(trap_unless_fixnum(arg_z))
        __(extract_typecode(imm2,arg_x))
        __(cmp imm2,#subtag_arrayH)
        __(ldreq imm1,[arg_x,#arrayH.rank])
        __(cmpeq imm1,#2<<fixnumshift)
        __(beq 1f)
        __(uuo_error_reg_not_xtype(al,arg_x,xtype_array2d))
1:              
        /* It's a 2-dimensional array.  Check bounds */
        __(ldr imm0,[arg_x,#arrayH.dim0])
        __(cmp arg_y,imm0)
        __(blo 2f)
        __(mov temp0,#0)
        __(uuo_error_array_axis_bounds(al,arg_y,temp0,arg_x))
2:              
        __(ldr imm0,[arg_x,#arrayH.dim0+node_size])
        __(cmp arg_z,imm0)
        __(blo 3f)
        __(mov temp0,#fixnumone)
        __(uuo_error_array_axis_bounds(al,arg_z,temp0,arg_x))
3:              
        __(unbox_fixnum(imm0,imm0))
	__(mla arg_z,arg_y,imm0,arg_z)
        /* arg_z is now row-major-index; get data vector and
           add in possible offset */
        __(mov arg_y,arg_x)
0:      __(ldr imm0,[arg_y,#arrayH.displacement])
        __(ldr arg_y,[arg_y,#arrayH.data_vector])
        __(extract_subtag(imm1,arg_y))
        __(cmp imm1,#subtag_vectorH)
        __(add arg_z,arg_z,imm0)
        __(bgt C(misc_ref_common))
        __(b 0b)
 
/* temp0 = array, arg_x = i, arg_y = j, arg_z = k */
_spentry(aref3)
        __(trap_unless_fixnum(arg_x))
        __(trap_unless_fixnum(arg_y))
        __(trap_unless_fixnum(arg_z))
        __(extract_typecode(imm2,temp0))
	__(mov imm1,#0)
        __(cmp imm2,#subtag_arrayH)
        __(ldreq imm1,[temp0,#arrayH.rank])
        __(cmp imm1,#3<<fixnumshift)
        __(beq 1f)
        __(uuo_error_reg_not_xtype(al,temp0,xtype_array3d))
1:              
        /* It's a 3-dimensional array.  Check bounds */
        __(ldr imm2,[temp0,arrayH.dim0+(node_size*2)])
        __(ldr imm1,[temp0,#arrayH.dim0+node_size])
        __(ldr imm0,[temp0,#arrayH.dim0])
        __(cmp arg_z,imm2)
        __(blo 2f)
        __(mov imm0,#2<<fixnumshift)
        __(uuo_error_array_axis_bounds(al,arg_z,imm0,temp0))
2:              
        __(cmp arg_y,imm1)
        __(blo 3f)
        __(mov imm0,#fixnumone)
        __(uuo_error_array_axis_bounds(al,arg_y,imm0,temp0))
3:              
        __(cmp arg_x,imm0)
        __(blo 4f)
        __(mov imm0,#0<<fixnumshift)
        __(uuo_error_array_axis_bounds(al,arg_x,imm0,temp0))
4:              
        __(unbox_fixnum(imm2,imm2))
        __(unbox_fixnum(imm1,imm1))
	/* (+ (* i dim1 dim2) (* j dim2) k) */
	__(mul imm1,imm2,imm1)
	__(mla imm2,arg_y,imm2,arg_z)	/* imm2 now a fixnum */
	__(mla arg_z,arg_x,imm1,imm2)
        __(mov arg_y,temp0)
0:      __(ldr arg_x,[arg_y,#arrayH.displacement])
        __(ldr arg_y,[arg_y,#arrayH.data_vector])
        __(extract_subtag(imm1,arg_y))
        __(cmp imm1,#subtag_vectorH)
        __(add arg_z,arg_x,arg_z)
        __(bgt C(misc_ref_common))
        __(b 0b)




/* As for aref2 above, but temp0 = array, arg_x = i, arg_y = j, arg_z = newval */
_spentry(aset2)
        __(extract_typecode(imm0,temp0))
        __(cmp imm0,#subtag_arrayH)
        __(ldreq imm0,[temp0,#arrayH.rank])
        __(cmpeq imm0,#2<<fixnumshift)
        __(beq 1f)
        __(uuo_error_reg_not_xtype(al,temp0,xtype_array2d))
1:              
        __(trap_unless_fixnum(arg_x))
        __(trap_unless_fixnum(arg_y))
        /* It's a 2-dimensional array.  Check bounds */
        __(ldr imm0,[temp0,#arrayH.dim0])
        __(cmp arg_x,imm0)
        __(blo 2f)
        __(mov imm0,#0)
        __(uuo_error_array_axis_bounds(al,arg_x,imm0,temp0))
2:              
        __(ldr imm0,[temp0,#arrayH.dim0+node_size])
        __(cmp arg_y,imm0)
        __(blo 3f)
        __(mov imm0,#1<<fixnumshift)
        __(uuo_error_array_axis_bounds(al,arg_y,imm0,temp0))
3:              
        __(unbox_fixnum(imm0,imm0))
	__(mla arg_y,arg_x,imm0,arg_y)
        /* arg_y is now row-major-index; get data vector and
           add in possible offset */
        __(mov arg_x,temp0)
0:      __(ldr imm0,[arg_x,#arrayH.displacement])
        __(ldr arg_x,[arg_x,#arrayH.data_vector])
        __(extract_subtag(imm1,arg_x))
        __(cmp imm1,#subtag_vectorH)
        __(add arg_y,arg_y,imm0)
        __(bgt C(misc_set_common))
        __(b 0b)

                 
/* temp1 = array, temp0 = i, arg_x = j, arg_y = k, arg_z = new */        
_spentry(aset3)
        __(extract_typecode(imm0,temp1))
        __(cmp imm0,#subtag_arrayH)
        __(ldreq imm0,[temp1,#arrayH.rank])
        __(cmpeq imm0,#3<<fixnumshift)
        __(beq 1f)
        __(uuo_error_reg_not_xtype(al,temp1,xtype_array3d))
1:              
        __(trap_unless_fixnum(temp0))
        __(trap_unless_fixnum(arg_x))
        __(trap_unless_fixnum(arg_y))
        /* It's a 3-dimensional array.  Check bounds */
        __(ldr imm2,[temp1,#arrayH.dim0+(node_size*2)])
        __(ldr imm1,[temp1,#arrayH.dim0+node_size])
        __(ldr imm0,[temp1,#arrayH.dim0])
        __(cmp arg_y,imm2)
        __(blo 2f)
        __(mov imm0,#2<<fixnumshift)
        __(uuo_error_array_axis_bounds(al,arg_y,imm0,temp1))
2:              
        __(cmp arg_x,imm1)
        __(blo 3f)
        __(mov imm0,#1<<fixnumshift)
        __(uuo_error_array_axis_bounds(al,arg_x,imm0,temp1))
3:              
        __(cmp temp0,imm0)
        __(blo 4f)
        __(mov imm0,#0)
        __(uuo_error_array_axis_bounds(al,temp0,imm0,temp1))
4:              
	__(unbox_fixnum(imm1,imm1))
	__(unbox_fixnum(imm2,imm2))
	/* (+ (* i dim1 dim2) (* j dim2) k) */
	__(mul imm1,imm2,imm1)
	__(mla imm2,arg_x,imm2,arg_y)	/* imm2 now a fixnum */
	__(mla arg_y,temp0,imm1,imm2)
        __(mov arg_x,temp1)
0:      __(ldr temp0,[arg_x,#arrayH.displacement])
        __(ldr arg_x,[arg_x,#arrayH.data_vector])
        __(extract_subtag(imm1,arg_x))
        __(cmp imm1,#subtag_vectorH)
        __(add arg_y,arg_y,temp0)
        __(bgt C(misc_set_common))
        __(b 0b)


/* Treat the last (- nargs imm0) values on the vstack as keyword/value  */
/* pairs.  There'll be arg_z keyword arguments.  arg_y contains flags  */
/* that indicate whether &allow-other-keys was specified and whether  */
/* or not to leave the keyword/value pairs on the vstack for an &rest  */
/* argument.  Element 2 of the function in fn contains a vector of keyword.  */
/* If the number of arguments is greater than imm0, the difference must  */
/* be even.  */
/* All arg regs have been vpushed and the calling function has built a */
/* stack frame.  next_method_context must be preserved, as must the incoming */
/* key/value pairs and their number if we're going to make an &rest arg. */
           

define(`keyword_flags',`arg_y')
define(`key_value_count',`arg_z')

define(`keyword_flag_allow_other_keys',`(fixnumone<<0)')
define(`keyword_flag_seen_allow_other_keys',`(fixnumone<<1)')
define(`keyword_flag_rest',`(fixnumone<<2)')
define(`keyword_flag_unknown_keyword_seen',`(fixnumone<<3)')
define(`keyword_flag_current_aok',`(fixnumone<<4)')

_spentry(keyword_bind)
        new_local_labels()        
        __(subs key_value_count,nargs,imm0)
        __(movmi key_value_count,#0)
        __(tst key_value_count,#fixnumone)
        __(bne local_label(odd_keywords))
        __(mov imm1,key_value_count,lsl #num_subtag_bits-fixnumshift)
        __(orr imm1,imm1,subtag_u32_vector)
        __(add imm0,key_value_count,#dnode_size) /* we know  count is even */
        __(stack_allocate_zeroed_ivector(imm1,imm0))
        __(mov imm0,#subtag_simple_vector)
        __(strb imm0,[sp])
        /* Copy key/value pairs in reverse order from the vstack to
           the gvector we just created on the cstack. */
        __(add imm0,vsp,key_value_count) /* src, predecrement */
        __(add imm1,sp,#node_size)       /* dest, postincrement */
        __(mov temp2,key_value_count)
        __(b 1f)
0:      __(ldr arg_x,[imm0,#-node_size]!)
        __(str arg_x,[imm1],#node_size)
1:      __(subs temp2,temp2,#fixnumone)
        __(bge 0b)
        /* Discard the key/value pairs from the vstack. */
        __(add vsp,vsp,key_value_count)
        __(ldr temp2,[fn,#misc_data_offset+(2*node_size)])
        __(getvheader(imm0,temp2))
        __(mov imm0,imm0,lsr #num_subtag_bits)
        __(mov temp0,vsp)
        __(mov imm1,#nil_value)
        /* Push a pair of NILs (value, supplied-p) for each defined keyword */
        __(b 3f)
2:      __(vpush1(imm1))
        __(vpush1(imm1))
3:      __(subs imm0,imm0,#1)
        __(bge 2b)
        /* Save nargs and temp1 so that we can use them in the loop(s) */
        __(stmdb vsp!,{imm2,temp1})
        /* For each provided key/value pair: if the key is :allow-other-keys
           and that hasn't been seen before, note that it's been seen and
           if the value is non-nil set the allow-other-keys bit in flags.
           Then search for the key in the defined keys vector.  If it's
           not found, note that an undefined keyword was seen by setting
           a bit in keyword_flags ; if it is found, use its position to
           index the table of value/supplied-p pairs that we pushed above.
           If the supplied-p var is already set, do nothing; otherwise,
           set the supplied-p var and value.
           When done, signal an error if we got an unknown keyword, or
           either copy the supplied key/value pairs back to the vstack
           if we're going to cons an &rest arg or discard them if we aren't.
        */
        __(mov imm2,#0)
        __(b local_label(nextvalpairtest))
local_label(nextvalpairloop):   
        __(add temp1,sp,#4)
        __(ldr temp1,[temp1,imm2])
        __(ref_nrs_symbol(imm1,kallowotherkeys,imm1))
        __(cmp temp1,imm1)
        __(orreq keyword_flags,keyword_flags,#keyword_flag_current_aok)
        __(tsteq keyword_flags,#keyword_flag_seen_allow_other_keys)
        __(bne local_label(current_key_allow_other_keys_handled))
        __(orr keyword_flags,keyword_flags,#keyword_flag_seen_allow_other_keys)
        /* Fortunately, we know what the keyword is.  Need to check the
           value here, and don't have a lot of free registers ... */
        __(add temp1,sp,#8)
        __(ldr temp1,[temp1,imm2])
        __(cmp temp1,#nil_value)
        __(orrne keyword_flags,keyword_flags,#keyword_flag_allow_other_keys)
        __(mov temp1,imm1)      /* from comparison above */
local_label(current_key_allow_other_keys_handled):
        __(getvheader(imm0,temp2))
        __(header_length(arg_x,imm0))
        __(add imm0,arg_x,#misc_data_offset)
        __(b local_label(defined_keyword_compare_test))
local_label(defined_keyword_compare_loop):      
        __(ldr arg_x,[temp2,imm0])
        __(cmp arg_x,temp1)
        __(subeq imm0,imm0,#misc_data_offset)
        __(beq local_label(defined_keyword_found))
local_label(defined_keyword_compare_test):      
        __(sub imm0,imm0,#node_size)
        __(cmp imm0,#misc_data_offset)
        __(bge local_label(defined_keyword_compare_loop))
        /* keyword wasn't defined.  Note that ... */
        __(tst keyword_flags,#keyword_flag_current_aok)
        __(bicne keyword_flags,#keyword_flag_current_aok)
        __(orreq keyword_flags,keyword_flags,#keyword_flag_unknown_keyword_seen)
        __(b local_label(nextkeyvalpairnext))
local_label(defined_keyword_found):     
        __(sub imm0,temp0,imm0,lsl #1)
        __(ldr arg_x,[imm0,#-8])
        __(cmp arg_x,#nil_value) /* seen this keyword yet ? */
        __(bne local_label(nextkeyvalpairnext))
        __(add arg_x,arg_x,#t_offset)
        __(str arg_x,[imm0,#-8])
        __(add temp1,sp,#8)
        __(ldr temp1,[temp1,imm2])
        __(str temp1,[imm0,#-4])
local_label(nextkeyvalpairnext):
        __(add imm2,imm2,#8)
local_label(nextvalpairtest):   
        __(cmp imm2,key_value_count)
        __(bne local_label(nextvalpairloop))
        __(ldmia vsp!,{imm2,temp1})
        /* If unknown keywords and that's not allowed, signal error.
           Otherwise, discard the stack-consed vector and return,
           possibly after having copied the vector's contents back
           to the vstack so that an &rest arg can be constructed.
        */
        __(tst keyword_flags,#keyword_flag_unknown_keyword_seen)
        __(beq 0f)
        __(tst keyword_flags,#keyword_flag_allow_other_keys)
        __(beq local_label(badkeys))
0:      __(tst keyword_flags,#keyword_flag_rest)
        __(beq local_label(discard_stack_vector))
        __(mov imm0,#0)
        __(add temp2,sp,#node_size)
        __(b 2f)
1:      __(ldr arg_x,[temp2],#node_size)
        __(vpush1(arg_x))
        __(add imm0,imm0,#fixnumone)
2:      __(cmp imm0,key_value_count)
        __(bne 1b)
local_label(discard_stack_vector):      
        __(add key_value_count,key_value_count,#dnode_size)
        __(add sp,sp,key_value_count)
        __(bx lr)               /* it's finally over ! */

local_label(badkeys):   /* Disturbingly similar to the &rest case */
        __(mov nargs,#0)
        __(add temp2,sp,#node_size)
        __(mov vsp,temp0)
        __(b 1f)
0:      __(ldr arg_x,[temp2],#node_size)
        __(vpush1(arg_x))
        __(add nargs,nargs,#fixnumone)
1:      __(cmp nargs,key_value_count)
        __(bne 0b)
        /* Lose the stack vector */
        __(add key_value_count,key_value_count,#dnode_size)
        __(add sp,sp,key_value_count)
local_label(error_exit):                
        __(bl _SPconslist)
        __(mov arg_y,#XBADKEYS)
        __(set_nargs(2))
        __(b _SPksignalerr)
local_label(odd_keywords):       
        __(mov nargs,key_value_count)
        __(b local_label(error_exit))

        .globl C(__aeabi_uidivmod)                
_spentry(udiv32)
        __(cmp imm1,#0)
        __(bne 1f)
        __(build_lisp_frame(imm1))
        __(bl _SPmakeu32)
        __(mov arg_y,#XDIVZRO)
        __(mov nargs,#2<<fixnumshift)
        __(restore_lisp_frame(imm0))
        __(b _SPksignalerr)
1:              
        __(stmdb vsp!,{arg_z,arg_y,arg_x,temp0,temp1,temp2})
        __(str vsp,[rcontext,#tcr.save_vsp])
        __(mov arg_z,rcontext)
        __(ldr arg_y,[rcontext,#tcr.last_lisp_frame])
        __(build_lisp_frame(r3))
        __(str sp,[arg_z,#tcr.last_lisp_frame])
        __(str allocptr,[arg_z,#tcr.save_allocptr])
        __(mov r3,#TCR_STATE_FOREIGN)
        __(str r3,[arg_z,#tcr.valence])
        __(mov r3,#0)
        __(bl C(__aeabi_uidivmod))
        __(mov rcontext,arg_z)
        __(str arg_y,[rcontext,#tcr.last_lisp_frame])
        __(mov allocptr,#VOID_ALLOCPTR)
        __(mov fn,#0)
        __(mov temp2,#0)
        __(mov temp1,#0)
        __(mov temp0,#0)
        __(mov arg_x,#TCR_STATE_LISP)
        __(str arg_x,[rcontext,#tcr.valence])
        __(ldr allocptr,[rcontext,#tcr.save_allocptr])
        __(ldm vsp!,{arg_z,arg_y,arg_x,temp0,temp1,temp2})
        __(ldr fn,[sp,#lisp_frame.savefn])
        __(ldr lr,[sp,#lisp_frame.savelr])
        __(discard_lisp_frame())
        __(bx lr)

_spentry(sdiv32)
        __(cmp imm1,#0)
        __(bne 1f)
        __(build_lisp_frame(imm1))
        __(bl _SPmakes32)
        __(mov arg_y,#XDIVZRO)
        __(mov nargs,#2<<fixnumshift)
        __(restore_lisp_frame(imm0))
        __(b _SPksignalerr)
1:              
        __(stmdb vsp!,{arg_z,arg_y,arg_x,temp0,temp1,temp2})
        __(str vsp,[rcontext,#tcr.save_vsp])
        __(mov arg_z,rcontext)
        __(ldr arg_y,[rcontext,#tcr.last_lisp_frame])
        __(build_lisp_frame(r3))
        __(str sp,[arg_z,#tcr.last_lisp_frame])
        __(str allocptr,[arg_z,#tcr.save_allocptr])
        __(mov r3,#TCR_STATE_FOREIGN)
        __(str r3,[arg_z,#tcr.valence])
        __(mov r3,#0)
        __(bl C(__aeabi_idivmod))
        __(mov rcontext,arg_z)
        __(str arg_y,[rcontext,#tcr.last_lisp_frame])
        __(mov allocptr,#VOID_ALLOCPTR)
        __(mov fn,#0)
        __(mov temp2,#0)
        __(mov temp1,#0)
        __(mov temp0,#0)
        __(mov arg_x,#TCR_STATE_LISP)
        __(str arg_x,[rcontext,#tcr.valence])
        __(ldr allocptr,[rcontext,#tcr.save_allocptr])
        __(ldm vsp!,{arg_z,arg_y,arg_x,temp0,temp1,temp2})
        __(ldr fn,[sp,#lisp_frame.savefn])
        __(ldr lr,[sp,#lisp_frame.savelr])
        __(discard_lisp_frame())
        __(bx lr)
        

_spentry(eabi_ff_callhf)
        __(add imm0,sp,#8)
        __(fldmfdd imm0,{d0-d7})
        __(ldmia sp,{imm0-imm1})
        __(sub imm0,imm0,#(16<<num_subtag_bits))
        __(add imm2,sp,#16<<2)
        __(stm imm2,{imm0-imm1})
        __(mov sp,imm2)
_spentry(eabi_ff_call)
        __(ldr arg_y,[rcontext,#tcr.last_lisp_frame])
        __(stmdb vsp!,{arg_y,arg_x,temp0,temp1,temp2})
        __(str vsp,[rcontext,#tcr.save_vsp])
/* There's a u32 vector on top of the stack ; its first data word points
   to the previous stack object.  The 4 words at the bottom of the vector
   are reserved for a lisp frame, which we construct carefully ... */
        __(mov imm0,#lisp_frame_marker)
        __(mov imm1,#0)
        __(ldr temp0,[sp,#4])
        __(sub temp0,temp0,#lisp_frame.size)
        __(str imm0,[temp0,#lisp_frame.marker])
        __(ldr imm0,[sp,#0])        
        __(str imm1,[temp0,#lisp_frame.savefn])
        __(str imm1,[temp0,#lisp_frame.savelr])
        __(sub imm0,imm0,#(lisp_frame.size/4)<<num_subtag_bits)
        __(str vsp,[temp0,#lisp_frame.savevsp])
        __(str imm0,[sp,#0])
        __(str lr,[temp0,#lisp_frame.savelr])
        __(str fn,[temp0,#lisp_frame.savefn])
        __(str allocptr,[rcontext,#tcr.save_allocptr])
        __(str temp0,[rcontext,#tcr.last_lisp_frame])
        __(mov temp0,rcontext)
        __(test_fixnum(arg_z))
        __(moveq imm1,arg_z,asr #fixnumshift)
        __(ldrne imm1,[arg_z,#misc_data_offset])
        __(mov imm0,#TCR_STATE_FOREIGN)
        __(str imm0,[rcontext,#tcr.valence])
        __(mov r4,imm1)
        __(add sp,sp,#dnode_size)
        __(ldmia sp!,{r0,r1,r2,r3})
        __(blx r4)
        __(adr temp1,1f)
        __(fldd double_float_zero,[temp1])
        __(mov temp1,#0)
        __(mov temp2,#0)
        __(mov arg_z,#0)
        __(mov arg_y,#0)
        __(mov arg_x,#0)
        __(mov fn,#0)
        __(mov allocptr,#VOID_ALLOCPTR)
        __(mov rcontext,temp0)
        __(ldr sp,[rcontext,#tcr.last_lisp_frame])
        __(str fn,[rcontext,#tcr.valence])
        __(ldr allocptr,[rcontext,#tcr.save_allocptr])
        __(restore_lisp_frame(temp0))
        __(ldmia vsp!,{arg_y,arg_x,temp0,temp1,temp2})
        __(str arg_y,[rcontext,#tcr.last_lisp_frame])
        __(check_pending_interrupt(temp2))
        __(bx lr)
        .align 3
1:      .long 0
        .long 0                
        

_spentry(debind)
        new_local_labels()
        __(mov temp0,vsp)
        __(mov temp1,arg_z)
        __(ands imm0,nargs,#0xff)
        __(mov arg_y,#nil_value)
        __(b local_label(req_test))
local_label(req_loop):  
        __(cmp arg_reg,#nil_value)
        __(extract_lisptag(imm1,arg_reg))
        __(beq local_label(toofew))
        __(cmp imm1,#tag_list)
        __(bne local_label(badlist))
        __(subs imm0,imm0,#1)
        __(_car(arg_x,arg_reg))
        __(_cdr(arg_reg,arg_reg))
        __(vpush1(arg_x))
local_label(req_test):
        __(bne local_label(req_loop))
        __(mov imm0,#0xff)
        __(ands imm0,imm0,nargs,lsr #8)
        __(beq local_label(rest_keys))
        __(tst nargs,#mask_initopt)
        __(bne local_label(opt_supp))
	/* 'simple' &optionals:	 no supplied-p, default to nil.   */
local_label(simple_opt_loop):
        __(cmp arg_reg,#nil_value)
        __(extract_lisptag(imm1,arg_reg))
        __(beq local_label(default_simple_opt))
        __(cmp imm1,#tag_list)
        __(bne local_label(badlist))
        __(subs imm0,imm0,#1)
        __(_car(arg_x,arg_reg))
        __(_cdr(arg_reg,arg_reg))
        __(vpush1(arg_x))
        __(bne local_label(simple_opt_loop))
        __(b local_label(rest_keys))
local_label(default_simple_opt):        
        __(subs imm0,imm0,#1)
        __(vpush1(arg_y))
        __(bne local_label(default_simple_opt))
        __(b local_label(rest_keys))
local_label(opt_supp):   
        __(cmp arg_reg,#nil_value)
        __(extract_lisptag(imm1,arg_reg))
        __(beq local_label(default_hard_opt))
        __(cmp imm1,#tag_list)
        __(bne local_label(badlist))
        __(subs imm0,imm0,#1)
        __(_car(arg_x,arg_reg))
        __(_cdr(arg_reg,arg_reg))
        __(vpush1(arg_x))
        __(add arg_x,arg_y,#t_offset)
        __(vpush1(arg_x))
        __(bne local_label(opt_supp))
        __(b local_label(rest_keys))
local_label(default_hard_opt):  
        __(subs imm0,imm0,#1)
        __(vpush1(arg_y))
        __(vpush1(arg_y))
        __(bne local_label(default_hard_opt))
local_label(rest_keys): 
        __(tst nargs,#mask_restp)
        __(bne local_label(have_rest))
        __(tst nargs,#mask_keyp)
        __(bne local_label(have_keys))
        __(cmp arg_reg,#nil_value)
        __(bne local_label(toomany))
        __(bx lr)
local_label(have_rest): 
        __(vpush1(arg_reg))
        __(tst nargs,#mask_keyp)
        __(bne local_label(have_keys))
        __(bx lr)
local_label(have_keys): 
        __(mov imm0,#256)
        __(mov arg_y,arg_reg)
local_label(count_keys_loop):   
        __(cmp arg_y,#nil_value)
        __(beq local_label(counted_keys))
        __(subs imm0,imm0,#1)
        __(bmi local_label(toomany))
        __(extract_lisptag(imm1,arg_y))
        __(cmp imm1,#tag_list)
        __(bne local_label(badlist))
        __(_cdr(arg_y,arg_y))
        __(cmp arg_y,#nil_value)
        __(extract_lisptag(imm1,arg_y))
        __(beq local_label(badkeys))
        __(cmp imm1,#tag_list)
        __(bne local_label(badlist))
        __(_cdr(arg_y,arg_y))
        __(b local_label(count_keys_loop))
local_label(counted_keys):      
	/* We've got a proper, even-length list of key/value pairs in  */
	/* arg_reg. For each keyword var in the lambda-list, push a pair  */
	/* of NILs on the vstack.  (We've also cdred down arg_y until it */
        /* contains NIL.) */
        __(mov imm0,#0xff)
        __(ands imm0,imm0,nargs,lsr #16)
        __(mov imm1,vsp)
        __(b local_label(push_pair_test))
local_label(push_pair_loop):
        __(subs imm0,imm0,#1)
        __(vpush1(arg_y))
        __(vpush1(arg_y))
local_label(push_pair_test):    
        __(bne local_label(push_pair_loop))
        __(b local_label(provided_key_loop))
        
local_label(next_provided_key): 
        __(_car(arg_x,arg_reg))
        __(ref_nrs_symbol(imm0,kallowotherkeys,imm0))
        __(cmp arg_x,imm0)
        __(bne local_label(not_aok))
        __(orr nargs,nargs,#mask_aok_this)
        __(tst nargs,#mask_aok_seen)
        __(bne local_label(not_aok))
        __(_cdr(arg_x,arg_reg))
        __(_car(arg_x,arg_x))
        __(orr nargs,nargs,#mask_aok_seen)
        __(cmp arg_x,#nil_value)
        __(orrne nargs,nargs,#mask_aok)
        __(_car(arg_x,arg_reg))
local_label(not_aok):   
        __(getvheader(imm0,keyvect_reg))
        __(header_length(arg_y,imm0))
        __(add imm0,arg_y,#misc_data_offset)
        __(b local_label(match_key_test))
local_label(match_key_loop):    
        __(ldr arg_y,[keyvect_reg,imm0])
        __(cmp arg_x,arg_y)
        __(bne local_label(match_key_test))
        __(sub imm0,imm0,#misc_data_offset)
        __(sub imm0,imm1,imm0,lsl #1)
        __(ldr arg_y,[imm0,#-2*node_size])
        __(cmp arg_y,#nil_value)
        __(bne local_label(provided_key_done))
        __(_cdr(arg_x,arg_reg))
        __(_car(arg_x,arg_x))
        __(str arg_x,[imm0,#-node_size])
        __(mov arg_x,#nil_value)
        __(add arg_x,arg_x,#t_offset)
        __(str arg_x,[imm0,#-2*node_size])
        __(b local_label(provided_key_done))
local_label(match_key_test):    
        __(sub imm0,imm0,#node_size)
        __(cmp imm0,#misc_data_offset)
        __(bge local_label(match_key_loop))
        __(tst nargs,#mask_aok_this)
        __(bic nargs,nargs,#mask_aok_this)
        __(orreq nargs,nargs,#mask_unknown_keyword_seen)
local_label(provided_key_done): 
        __(_cdr(arg_reg,arg_reg))
        __(_cdr(arg_reg,arg_reg))
local_label(provided_key_loop):
        __(cmp arg_reg,#nil_value)
        __(bne local_label(next_provided_key))
        __(tst nargs,#mask_unknown_keyword_seen)
        __(bxeq lr)
        __(tst nargs,#mask_aok)
        __(bxne lr)
local_label(badkeys):
        __(mov arg_y,#XBADKEYS)
        __(b local_label(destructure_error))
local_label(toomany):   
        __(mov arg_y,#XCALLTOOMANY)
        __(b local_label(destructure_error))
local_label(toofew):    
        __(mov arg_y,#XCALLTOOFEW)
        __(b local_label(destructure_error))
local_label(badlist):   
        __(mov arg_y,#XCALLNOMATCH)
local_label(destructure_error): 
        __(mov vsp,temp0)
        __(mov arg_z,temp1)        
        __(set_nargs(2))
        __(b _SPksignalerr)
        
_spentry(eabi_callback)
        __(stmdb sp!,{r0,r1,r2,r3})
        __(mov r0,sp)
        __(sub sp,sp,#2*node_size) /* room for result */
        __(fstmdbd sp!,{d0-d7})
        __(stmdb sp!,{r4,r5,r6,r7,r8,r9,r10,r11,r12,lr})
        __(mov r4,r0)
        __(box_fixnum(r5,r12))
        __(ref_global(r12,get_tcr,r0))
        __(mov r0,#1)
        __(blx r12)
        __(mov rcontext,r0)
        __(tst sp,#4)
        __(mov imm2,sp)
        __(strne imm2,[sp,#-4]!)
        __(streq imm2,[sp,#-8]!)
        __(ldr imm2,[rcontext,#tcr.last_lisp_frame])
        __(sub imm0,imm2,sp)
        __(add imm0,imm0,#node_size)
        __(mov imm0,imm0,lsl #num_subtag_bits-word_shift)
        __(orr imm0,imm0,#subtag_u32_vector)
        __(stmdb sp!,{imm0,imm2})
        __(push_foreign_fprs())
        __(adr imm0,1f)
        __(fldd double_float_zero,[imm0])
        __(mov arg_x,#0)
        __(mov temp0,#0)
        __(mov temp1,#0)
        __(mov temp2,#0)
        __(mov allocptr,#VOID_ALLOCPTR)
        __(mov fn,#0)
        __(ldr vsp,[rcontext,#tcr.save_vsp])
        __(mov imm0,#TCR_STATE_LISP)
        __(str imm0,[rcontext,#tcr.valence])
        __(ldr allocptr,[rcontext,#tcr.save_allocptr]) 
        __(set_nargs(2))
        __(ref_nrs_symbol(fname,callbacks,imm0))
        __(ldr nfn,[fname,#symbol.fcell])
        __(ldr lr,[nfn,#_function.entrypoint])
        __(blx lr)
        __(str vsp,[rcontext,#tcr.save_vsp])
        __(ldr imm1,[sp,#(9*8)+4])
        __(str imm1,[rcontext,#tcr.last_lisp_frame])
        __(str allocptr,[rcontext,#tcr.save_allocptr])
        __(mov imm0,#TCR_STATE_FOREIGN)
        __(str imm0,[rcontext,#tcr.valence])
        __(pop_foreign_fprs())
        __(ldr sp,[sp,#node_size*2])   /* drop the ivector that hides foreign stack contents and restore (possibly misaligned) sp */
        __(ldmia sp!,{r4,r5,r6,r7,r8,r9,r10,r11,r12,lr})
        __(add sp,sp,#8*8)
        __(fldd d0,[sp,#0])
        __(ldmia sp!,{r0,r1})
        __(add sp,sp,#4*node_size)
        __(bx lr)
        .align 3
1:      
        .long 0
        .long 0        
                       
/*  EOF, basically  */
	
_startfn(C(misc_ref_common))
        __(add pc,pc,imm1,lsl #2)
        __(nop)

local_label(misc_ref_jmp):          
	/* 00-0f  */
	__(b local_label(misc_ref_invalid)) /* 00 even_fixnum  */
	
	__(b local_label(misc_ref_invalid)) /* 01 cons  */
	__(b local_label(misc_ref_node))    /* 02 pseudofunction */
	__(b local_label(misc_ref_invalid)) /* 03 imm  */
	__(b local_label(misc_ref_invalid)) /* 04 odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 05 nil  */
	__(b local_label(misc_ref_invalid)) /* 06 misc  */
	__(b local_label(misc_ref_u32)) /* 07 bignum  */
	__(b local_label(misc_ref_invalid)) /* 08 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 09 cons  */
	__(b local_label(misc_ref_node)) /* 0a ratio  */
	__(b local_label(misc_ref_invalid)) /* 0b imm  */
	__(b local_label(misc_ref_invalid)) /* 0c odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 0d nil  */
	__(b local_label(misc_ref_invalid)) /* 0e misc  */
	__(b local_label(misc_ref_u32)) /* 0f single_float  */
	/* 10-1f  */
	__(b local_label(misc_ref_invalid)) /* 10 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 11 cons  */
	__(b local_label(misc_ref_invalid)) /* 12 nodeheader  */
	__(b local_label(misc_ref_invalid)) /* 13 imm  */
	__(b local_label(misc_ref_invalid)) /* 14 odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 15 nil  */
	__(b local_label(misc_ref_invalid)) /* 16 misc  */
	__(b local_label(misc_ref_u32)) /* 17 double_float  */
	__(b local_label(misc_ref_invalid)) /* 18 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 19 cons  */
	__(b local_label(misc_ref_node)) /* 1a complex  */
	__(b local_label(misc_ref_invalid)) /* 1b imm  */
	__(b local_label(misc_ref_invalid)) /* 1c odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 1d nil  */
	__(b local_label(misc_ref_invalid)) /* 1e misc  */
	__(b local_label(misc_ref_u32)) /* 1f macptr  */
	/* 20-2f  */
	__(b local_label(misc_ref_invalid)) /* 20 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 21 cons  */
	__(b local_label(misc_ref_node)) /* 22 catch_frame  */
	__(b local_label(misc_ref_invalid)) /* 23 imm  */
	__(b local_label(misc_ref_invalid)) /* 24 odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 25 nil  */
	__(b local_label(misc_ref_invalid)) /* 26 misc  */
	__(b local_label(misc_ref_u32)) /* 27 dead_macptr  */
	__(b local_label(misc_ref_invalid)) /* 28 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 29 cons  */
	__(b local_label(misc_ref_node)) /* 2a function  */
	__(b local_label(misc_ref_invalid)) /* 2b imm  */
	__(b local_label(misc_ref_invalid)) /* 2c odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 2d nil  */
	__(b local_label(misc_ref_invalid)) /* 2e misc  */
	__(b local_label(misc_ref_u32)) /* 2f code_vector  */
	/* 30-3f  */
	__(b local_label(misc_ref_invalid)) /* 30 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 31 cons  */
	__(b local_label(misc_ref_node)) /* 32 lisp_thread  */
	__(b local_label(misc_ref_invalid)) /* 33 imm  */
	__(b local_label(misc_ref_invalid)) /* 34 odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 35 nil  */
	__(b local_label(misc_ref_invalid)) /* 36 misc  */
	__(b local_label(misc_ref_u32)) /* 37 creole  */
	__(b local_label(misc_ref_invalid)) /* 38 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 39 cons  */
	__(b local_label(misc_ref_node)) /* 3a symbol  */
	__(b local_label(misc_ref_invalid)) /* 3b imm  */
	__(b local_label(misc_ref_invalid)) /* 3c odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 3d nil  */
	__(b local_label(misc_ref_invalid)) /* 3e misc  */
	__(b local_label(misc_ref_u32)) /* 3f xcode_vector  */
	/* 40-4f  */
	__(b local_label(misc_ref_invalid)) /* 40 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 41 cons  */
	__(b local_label(misc_ref_node)) /* 42 lock  */
	__(b local_label(misc_ref_invalid)) /* 43 imm  */
	__(b local_label(misc_ref_invalid)) /* 44 odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 45 nil  */
	__(b local_label(misc_ref_invalid)) /* 46 misc  */
	__(b local_label(misc_ref_invalid)) /* 47 immheader  */
	__(b local_label(misc_ref_invalid)) /* 48 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 49 cons  */
	__(b local_label(misc_ref_node)) /* 4a hash_vector  */
	__(b local_label(misc_ref_invalid)) /* 4b imm  */
	__(b local_label(misc_ref_invalid)) /* 4c odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 4d nil  */
	__(b local_label(misc_ref_invalid)) /* 4e misc  */
	__(b local_label(misc_ref_invalid)) /* 4f immheader  */
	/* 50-5f  */
	__(b local_label(misc_ref_invalid)) /* 50 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 51 cons  */
	__(b local_label(misc_ref_node)) /* 52 pool  */
	__(b local_label(misc_ref_invalid)) /* 53 imm  */
	__(b local_label(misc_ref_invalid)) /* 54 odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 55 nil  */
	__(b local_label(misc_ref_invalid)) /* 56 misc  */
	__(b local_label(misc_ref_invalid)) /* 57 immheader  */
	__(b local_label(misc_ref_invalid)) /* 58 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 59 cons  */
	__(b local_label(misc_ref_node)) /* 5a weak  */
	__(b local_label(misc_ref_invalid)) /* 5b imm  */
	__(b local_label(misc_ref_invalid)) /* 5c odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 5d nil  */
	__(b local_label(misc_ref_invalid)) /* 5e misc  */
	__(b local_label(misc_ref_invalid)) /* 5f immheader  */
	/* 60-6f  */
	__(b local_label(misc_ref_invalid)) /* 60 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 61 cons  */
	__(b local_label(misc_ref_node)) /* 62 package  */
	__(b local_label(misc_ref_invalid)) /* 63 imm  */
	__(b local_label(misc_ref_invalid)) /* 64 odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 65 nil  */
	__(b local_label(misc_ref_invalid)) /* 66 misc  */
	__(b local_label(misc_ref_invalid)) /* 67 immheader  */
	__(b local_label(misc_ref_invalid)) /* 68 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 69 cons  */
	__(b local_label(misc_ref_node)) /* 6a slot_vector  */
	__(b local_label(misc_ref_invalid)) /* 6b imm  */
	__(b local_label(misc_ref_invalid)) /* 6c odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 6d nil  */
	__(b local_label(misc_ref_invalid)) /* 6e misc  */
	__(b local_label(misc_ref_invalid)) /* 6f immheader  */
	/* 70-7f  */
	__(b local_label(misc_ref_invalid)) /* 70 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 71 cons  */
	__(b local_label(misc_ref_node)) /* 72 instance  */
	__(b local_label(misc_ref_invalid)) /* 73 imm  */
	__(b local_label(misc_ref_invalid)) /* 74 odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 75 nil  */
	__(b local_label(misc_ref_invalid)) /* 76 misc  */
	__(b local_label(misc_ref_invalid)) /* 77 immheader  */
	__(b local_label(misc_ref_invalid)) /* 78 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 79 cons  */
	__(b local_label(misc_ref_node)) /* 7a struct  */
	__(b local_label(misc_ref_invalid)) /* 7b imm  */
	__(b local_label(misc_ref_invalid)) /* 7c odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 7d nil  */
	__(b local_label(misc_ref_invalid)) /* 7e misc  */
	__(b local_label(misc_ref_invalid)) /* 7f immheader  */
	/* 80-8f  */
	__(b local_label(misc_ref_invalid)) /* 80 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 81 cons  */
	__(b local_label(misc_ref_node)) /* 82 istruct  */
	__(b local_label(misc_ref_invalid)) /* 83 imm  */
	__(b local_label(misc_ref_invalid)) /* 84 odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 85 nil  */
	__(b local_label(misc_ref_invalid)) /* 86 misc  */
	__(b local_label(misc_ref_invalid)) /* 87 immheader  */
	__(b local_label(misc_ref_invalid)) /* 88 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 89 cons  */
	__(b local_label(misc_ref_node)) /* 8a value_cell  */
	__(b local_label(misc_ref_invalid)) /* 8b imm  */
	__(b local_label(misc_ref_invalid)) /* 8c odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 8d nil  */
	__(b local_label(misc_ref_invalid)) /* 8e misc  */
	__(b local_label(misc_ref_invalid)) /* 8f immheader  */
	/* 90-9f  */
	__(b local_label(misc_ref_invalid)) /* 90 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 91 cons  */
	__(b local_label(misc_ref_node)) /* 92 xfunction  */
	__(b local_label(misc_ref_invalid)) /* 93 imm  */
	__(b local_label(misc_ref_invalid)) /* 94 odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 95 nil  */
	__(b local_label(misc_ref_invalid)) /* 96 misc  */
	__(b local_label(misc_ref_invalid)) /* 97 immheader  */
	__(b local_label(misc_ref_invalid)) /* 98 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 99 cons  */
	__(b local_label(misc_ref_node)) /* 9a arrayN  */
	__(b local_label(misc_ref_invalid)) /* 9b imm  */
	__(b local_label(misc_ref_invalid)) /* 9c odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* 9d nil  */
	__(b local_label(misc_ref_invalid)) /* 9e misc  */
	__(b local_label(misc_ref_invalid)) /* 9f immheader  */
	/* a0-af  */
	__(b local_label(misc_ref_invalid)) /* a0 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* a1 cons  */
	__(b local_label(misc_ref_node)) /* a2 vectorH  */
	__(b local_label(misc_ref_invalid)) /* a3 imm  */
	__(b local_label(misc_ref_invalid)) /* a4 odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* a5 nil  */
	__(b local_label(misc_ref_invalid)) /* a6 misc  */
	__(b local_label(misc_ref_single_float_vector)) /* a7 sf_vector  */
	__(b local_label(misc_ref_invalid)) /* a8 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* a9 cons  */
	__(b local_label(misc_ref_node)) /* aa simple_vector  */
	__(b local_label(misc_ref_invalid)) /* ab imm  */
	__(b local_label(misc_ref_invalid)) /* ac odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* ad nil  */
	__(b local_label(misc_ref_invalid)) /* ae misc  */
	__(b local_label(misc_ref_u32)) /* af u32  */
	/* b0-bf  */
	__(b local_label(misc_ref_invalid)) /* b0 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* b1 cons  */
	__(b local_label(misc_ref_invalid)) /* b2 nodeheader  */
	__(b local_label(misc_ref_invalid)) /* b3 imm  */
	__(b local_label(misc_ref_invalid)) /* b4 odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* b5 nil  */
	__(b local_label(misc_ref_invalid)) /* b6 misc  */
	__(b local_label(misc_ref_s32)) /* b7 s32  */
	__(b local_label(misc_ref_invalid)) /* b8 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* b9 cons  */
	__(b local_label(misc_ref_invalid)) /* ba nodeheader  */
	__(b local_label(misc_ref_invalid)) /* bb imm  */
	__(b local_label(misc_ref_invalid)) /* bc odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* bd nil  */
	__(b local_label(misc_ref_invalid)) /* be misc  */
	__(b local_label(misc_ref_fixnum_vector)) /* bf fixnum_vector  */
	/* c0-cf  */
	__(b local_label(misc_ref_invalid)) /* c0 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* c1 cons  */
	__(b local_label(misc_ref_invalid)) /* c2 nodeheader  */
	__(b local_label(misc_ref_invalid)) /* c3 imm  */
	__(b local_label(misc_ref_invalid)) /* c4 odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* c5 nil  */
	__(b local_label(misc_ref_invalid)) /* c6 misc  */
	__(b local_label(misc_ref_new_string)) /* c7 new_string  */
	__(b local_label(misc_ref_invalid)) /* c8 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* c9 cons  */
	__(b local_label(misc_ref_invalid)) /* ca nodeheader  */
	__(b local_label(misc_ref_invalid)) /* cb imm  */
	__(b local_label(misc_ref_invalid)) /* cc odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* cd nil  */
	__(b local_label(misc_ref_invalid)) /* ce misc  */
	__(b local_label(misc_ref_u8)) /* cf u8  */
	/* d0-df  */
	__(b local_label(misc_ref_invalid)) /* d0 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* d1 cons  */
	__(b local_label(misc_ref_invalid)) /* d2 nodeheader  */
	__(b local_label(misc_ref_invalid)) /* d3 imm  */
	__(b local_label(misc_ref_invalid)) /* d4 odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* d5 nil  */
	__(b local_label(misc_ref_invalid)) /* d6 misc  */
	__(b local_label(misc_ref_s8))      /* d7 s8  */
	__(b local_label(misc_ref_invalid)) /* d8 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* d9 cons  */
	__(b local_label(misc_ref_invalid)) /* da nodeheader  */
	__(b local_label(misc_ref_invalid)) /* db imm  */
	__(b local_label(misc_ref_invalid)) /* dc odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* dd nil  */
	__(b local_label(misc_ref_invalid)) /* de misc  */
	__(b local_label(misc_ref_old_string)) /* df (old)subtag_simple_base_string  */
	/* e0-ef  */
	__(b local_label(misc_ref_invalid)) /* e0 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* e1 cons  */
	__(b local_label(misc_ref_invalid)) /* e2 nodeheader  */
	__(b local_label(misc_ref_invalid)) /* e3 imm  */
	__(b local_label(misc_ref_invalid)) /* e4 odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* e5 nil  */
	__(b local_label(misc_ref_invalid)) /* e6 misc  */
	__(b local_label(misc_ref_u16)) /* e7 u16  */
	__(b local_label(misc_ref_invalid)) /* e8 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* e9 cons  */
	__(b local_label(misc_ref_invalid)) /* ea nodeheader  */
	__(b local_label(misc_ref_invalid)) /* eb imm  */
	__(b local_label(misc_ref_invalid)) /* ec odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* ed nil  */
	__(b local_label(misc_ref_invalid)) /* ee misc  */
	__(b local_label(misc_ref_s16)) /* ef s16  */
	/* f0-ff  */
	__(b local_label(misc_ref_invalid)) /* f0 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* f1 cons  */
	__(b local_label(misc_ref_invalid)) /* f2 nodeheader  */
	__(b local_label(misc_ref_invalid)) /* f3 imm  */
	__(b local_label(misc_ref_invalid)) /* f4 odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* f5 nil  */
	__(b local_label(misc_ref_invalid)) /* f6 misc  */
	__(b local_label(misc_ref_double_float_vector)) /* f7 df vector  */
	__(b local_label(misc_ref_invalid)) /* f8 even_fixnum  */
	__(b local_label(misc_ref_invalid)) /* f9 cons  */
	__(b local_label(misc_ref_invalid)) /* fa nodeheader  */
	__(b local_label(misc_ref_invalid)) /* fb imm  */
	__(b local_label(misc_ref_invalid)) /* fc odd_fixnum  */
	__(b local_label(misc_ref_invalid)) /* fd nil  */
	__(b local_label(misc_ref_invalid)) /* fe misc  */
	__(b local_label(misc_ref_bit_vector)) /* ff bit_vector  */

local_label(misc_ref_node):        
	/* A node vector.  */
	__(add imm0,arg_z,#misc_data_offset)
	__(ldr  arg_z,[arg_y,imm0])
	__(bx lr)
local_label(misc_ref_single_float_vector):        
	__(add imm0,arg_z,misc_data_offset)
	__(movc16(imm1,single_float_header))
	__(ldr imm0,[arg_y,imm0])
	__(Misc_Alloc_Fixed(arg_z,imm1,single_float.size))
	__(str imm0,[arg_z,#single_float.value])
	__(bx lr)
local_label(misc_ref_new_string):        
	__(add imm0,arg_z,#misc_data_offset)
	__(ldr imm0,[arg_y,imm0])
	__(mov arg_z,imm0,lsl #charcode_shift)
	__(orr arg_z,arg_z,#subtag_character)
	__(bx lr)
local_label(misc_ref_s32):        
	__(add imm0,arg_z,#misc_data_offset)
	__(ldr imm0,[arg_y,imm0])
	__(b _SPmakes32)
local_label(misc_ref_fixnum_vector):    
	__(add imm0,arg_z,#misc_data_offset)
	__(ldr imm0,[arg_y,imm0])
	__(box_fixnum(arg_z,imm0))
	__(bx lr)        
local_label(misc_ref_u32):        
	__(add imm0,arg_z,#misc_data_offset)
	__(ldr imm0,[arg_y,imm0])
	__(b _SPmakeu32)
local_label(misc_ref_double_float_vector):      
	__(mov imm2,arg_z,lsl #1)
	__(add imm2,imm2,#misc_dfloat_offset)
	__(ldrd imm0,imm1,[arg_y,imm2])
	__(movc16(imm2,double_float_header))
	__(Misc_Alloc_Fixed(arg_z,imm2,double_float.size))
	__(strd imm0,imm1,[arg_z,#double_float.value])
	__(bx lr)
local_label(misc_ref_bit_vector):
	__(mov imm1,#nbits_in_word-1)
	__(and imm1,imm1,arg_z,lsr #2)
	__(mov imm2,#1)
	__(mov imm2,imm2,lsl imm1)
	__(mov imm0,arg_z,lsr #5+fixnumshift)
	__(mov imm0,imm0,lsl #2)
	__(add imm0,imm0,#misc_data_offset)
	__(mov arg_z,#0)
	__(ldr imm0,[arg_y,imm0])
	__(tst imm0,imm2)
	__(addne arg_z,arg_z,#fixnumone)
	__(bx lr)
local_label(misc_ref_s8):       
	__(mov imm0,arg_z,lsr #2)
	__(add imm0,imm0,#misc_data_offset)
	__(ldsb imm0,[arg_y,imm0])
	__(box_fixnum(arg_z,imm0))
	__(bx lr)
local_label(misc_ref_u8):       
	__(mov imm0,arg_z,lsr #2)
	__(add imm0,imm0,#misc_data_offset)
	__(ldrb imm0,[arg_y,imm0])
	__(box_fixnum(arg_z,imm0))
	__(bx lr)
local_label(misc_ref_old_string):          
	__(mov imm0,arg_z,lsr #2)
	__(add imm0,imm0,#misc_data_offset)
	__(ldrb imm0,[arg_y,imm0])
	__(mov arg_z,imm0,lsl #charcode_shift)
	__(orr arg_z,arg_z,#subtag_character)
	__(bx lr)
local_label(misc_ref_u16):        
	__(mov imm0,arg_z,lsr #1)     
	__(add imm0,imm0,#misc_data_offset)
	__(ldrh imm0,[arg_y,imm0])
	__(box_fixnum(arg_z,imm0))
	__(bx lr)
local_label(misc_ref_s16):             
	__(mov imm0,arg_z,lsr #1)     
	__(add imm0,imm0,#misc_data_offset)
	__(ldrsh imm0,[arg_y,imm0])
	__(box_fixnum(arg_z,imm0))
	__(bx lr)
local_label(misc_ref_invalid):
	__(mov arg_x,#XBADVEC)
	__(set_nargs(3))
	__(b _SPksignalerr)        
_endfn
        
_startfn(C(misc_set_common))
        __(add pc,pc,imm1,lsl #2)
        __(nop)
local_label(misc_set_jmp):             
	/* 00-0f  */
	__(b local_label(misc_set_invalid)) /* 00 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 01 cons  */
	__(b _SPgvset)                      /* 02 pseudofunction  */
	__(b local_label(misc_set_invalid)) /* 03 imm  */
	__(b local_label(misc_set_invalid)) /* 04 odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 05 nil  */
	__(b local_label(misc_set_invalid)) /* 06 misc  */
	__(b local_label(misc_set_u32)) /* 07 bignum  */
	__(b local_label(misc_set_invalid)) /* 08 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 09 cons  */
	__(b _SPgvset) /* 0a ratio  */
	__(b  local_label(misc_set_invalid)) /* 0b imm  */
	__(b local_label(misc_set_invalid)) /* 0c odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 0d nil  */
	__(b local_label(misc_set_invalid)) /* 0e misc  */
	__(b local_label(misc_set_u32)) /* 0f single_float  */
	/* 10-1f  */
	__(b local_label(misc_set_invalid)) /* 10 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 11 cons  */
	__(b local_label(misc_set_invalid)) /* 12 nodeheader  */
	__(b local_label(misc_set_invalid)) /* 13 imm  */
	__(b local_label(misc_set_invalid)) /* 14 odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 15 nil  */
	__(b local_label(misc_set_invalid)) /* 16 misc  */
	__(b local_label(misc_set_u32)) /* 17 double_float  */
	__(b local_label(misc_set_invalid)) /* 18 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 19 cons  */
	__(b _SPgvset) /* 1a complex  */
	__(b  local_label(misc_set_invalid)) /* 1b imm  */
	__(b local_label(misc_set_invalid)) /* 1c odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 1d nil  */
	__(b local_label(misc_set_invalid)) /* 1e misc  */
	__(b local_label(misc_set_u32)) /* 1f macptr  */
	/* 20-2f  */
	__(b local_label(misc_set_invalid)) /* 20 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 21 cons  */
	__(b _SPgvset) /* 22 catch_frame  */
	__(b  local_label(misc_set_invalid)) /* 23 imm  */
	__(b local_label(misc_set_invalid)) /* 24 odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 25 nil  */
	__(b local_label(misc_set_invalid)) /* 26 misc  */
	__(b local_label(misc_set_u32)) /* 27 dead_macptr  */
	__(b local_label(misc_set_invalid)) /* 28 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 29 cons  */
	__(b _SPgvset) /* 2a function  */
	__(b  local_label(misc_set_invalid)) /* 2b imm  */
	__(b local_label(misc_set_invalid)) /* 2c odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 2d nil  */
	__(b local_label(misc_set_invalid)) /* 2e misc  */
	__(b local_label(misc_set_u32)) /* 2f code_vector  */
	/* 30-3f  */
	__(b local_label(misc_set_invalid)) /* 30 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 31 cons  */
	__(b _SPgvset) /* 32 lisp_thread  */
	__(b  local_label(misc_set_invalid)) /* 33 imm  */
	__(b local_label(misc_set_invalid)) /* 34 odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 35 nil  */
	__(b local_label(misc_set_invalid)) /* 36 misc  */
	__(b local_label(misc_set_u32)) /* 37 creole  */
	__(b local_label(misc_set_invalid)) /* 38 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 39 cons  */
	__(b _SPgvset) /* 3a symbol  */
	__(b  local_label(misc_set_invalid)) /* 3b imm  */
	__(b local_label(misc_set_invalid)) /* 3c odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 3d nil  */
	__(b local_label(misc_set_invalid)) /* 3e misc  */
	__(b local_label(misc_set_u32)) /* 3f xcode_vector  */
	/* 40-4f  */
	__(b local_label(misc_set_invalid)) /* 40 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 41 cons  */
	__(b _SPgvset) /* 42 lock  */
	__(b  local_label(misc_set_invalid)) /* 43 imm  */
	__(b local_label(misc_set_invalid)) /* 44 odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 45 nil  */
	__(b local_label(misc_set_invalid)) /* 46 misc  */
	__(b local_label(misc_set_invalid)) /* 47 immheader  */
	__(b local_label(misc_set_invalid)) /* 48 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 49 cons  */
	__(b _SPgvset) /* 4a hash_vector  */
	__(b  local_label(misc_set_invalid)) /* 4b imm  */
	__(b local_label(misc_set_invalid)) /* 4c odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 4d nil  */
	__(b local_label(misc_set_invalid)) /* 4e misc  */
	__(b local_label(misc_set_invalid)) /* 4f immheader  */
	/* 50-5f  */
	__(b local_label(misc_set_invalid)) /* 50 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 51 cons  */
	__(b _SPgvset) /* 52 pool  */
	__(b  local_label(misc_set_invalid)) /* 53 imm  */
	__(b local_label(misc_set_invalid)) /* 54 odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 55 nil  */
	__(b local_label(misc_set_invalid)) /* 56 misc  */
	__(b local_label(misc_set_invalid)) /* 57 immheader  */
	__(b local_label(misc_set_invalid)) /* 58 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 59 cons  */
	__(b _SPgvset) /* 5a weak  */
	__(b  local_label(misc_set_invalid)) /* 5b imm  */
	__(b local_label(misc_set_invalid)) /* 5c odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 5d nil  */
	__(b local_label(misc_set_invalid)) /* 5e misc  */
	__(b local_label(misc_set_invalid)) /* 5f immheader  */
	/* 60-6f  */
	__(b local_label(misc_set_invalid)) /* 60 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 61 cons  */
	__(b _SPgvset) /* 62 package  */
	__(b  local_label(misc_set_invalid)) /* 63 imm  */
	__(b local_label(misc_set_invalid)) /* 64 odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 65 nil  */
	__(b local_label(misc_set_invalid)) /* 66 misc  */
	__(b local_label(misc_set_invalid)) /* 67 immheader  */
	__(b local_label(misc_set_invalid)) /* 68 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 69 cons  */
	__(b _SPgvset) /* 6a slot_vector  */
	__(b  local_label(misc_set_invalid)) /* 6b imm  */
	__(b local_label(misc_set_invalid)) /* 6c odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 6d nil  */
	__(b local_label(misc_set_invalid)) /* 6e misc  */
	__(b local_label(misc_set_invalid)) /* 6f immheader  */
	/* 70-7f  */
	__(b local_label(misc_set_invalid)) /* 70 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 71 cons  */
	__(b _SPgvset) /* 72 instance  */
	__(b  local_label(misc_set_invalid)) /* 73 imm  */
	__(b local_label(misc_set_invalid)) /* 74 odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 75 nil  */
	__(b local_label(misc_set_invalid)) /* 76 misc  */
	__(b local_label(misc_set_invalid)) /* 77 immheader  */
	__(b local_label(misc_set_invalid)) /* 78 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 79 cons  */
	__(b _SPgvset) /* 7a struct  */
	__(b  local_label(misc_set_invalid)) /* 7b imm  */
	__(b local_label(misc_set_invalid)) /* 7c odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 7d nil  */
	__(b local_label(misc_set_invalid)) /* 7e misc  */
	__(b local_label(misc_set_invalid)) /* 7f immheader  */
	/* 80-8f  */
	__(b local_label(misc_set_invalid)) /* 80 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 81 cons  */
	__(b _SPgvset) /* 82 istruct  */
	__(b  local_label(misc_set_invalid)) /* 83 imm  */
	__(b local_label(misc_set_invalid)) /* 84 odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 85 nil  */
	__(b local_label(misc_set_invalid)) /* 86 misc  */
	__(b local_label(misc_set_invalid)) /* 87 immheader  */
	__(b local_label(misc_set_invalid)) /* 88 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 89 cons  */
	__(b _SPgvset) /* 8a value_cell  */
	__(b  local_label(misc_set_invalid)) /* 8b imm  */
	__(b local_label(misc_set_invalid)) /* 8c odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 8d nil  */
	__(b local_label(misc_set_invalid)) /* 8e misc  */
	__(b local_label(misc_set_invalid)) /* 8f immheader  */
	/* 90-9f  */
	__(b local_label(misc_set_invalid)) /* 90 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 91 cons  */
	__(b _SPgvset) /* 92 xfunction  */
	__(b  local_label(misc_set_invalid)) /* 93 imm  */
	__(b local_label(misc_set_invalid)) /* 94 odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 95 nil  */
	__(b local_label(misc_set_invalid)) /* 96 misc  */
	__(b local_label(misc_set_invalid)) /* 97 immheader  */
	__(b local_label(misc_set_invalid)) /* 98 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* 99 cons  */
	__(b _SPgvset) /* 9a arrayH  */
	__(b  local_label(misc_set_invalid)) /* 9b imm  */
	__(b local_label(misc_set_invalid)) /* 9c odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* 9d nil  */
	__(b local_label(misc_set_invalid)) /* 9e misc  */
	__(b local_label(misc_set_invalid)) /* 9f immheader  */
	/* a0-af  */
	__(b local_label(misc_set_invalid)) /* a0 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* a1 cons  */
	__(b _SPgvset) /* a2 vectorH  */
	__(b  local_label(misc_set_invalid)) /* a3 imm  */
	__(b local_label(misc_set_invalid)) /* a4 odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* a5 nil  */
	__(b local_label(misc_set_invalid)) /* a6 misc  */
	__(b local_label(misc_set_single_float_vector)) /* a7 sf vector  */
	__(b local_label(misc_set_invalid)) /* a8 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* a9 cons  */
	__(b _SPgvset) /* aa vectorH  */
	__(b  local_label(misc_set_invalid)) /* ab imm  */
	__(b local_label(misc_set_invalid)) /* ac odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* ad nil  */
	__(b local_label(misc_set_invalid)) /* ae misc  */
	__(b local_label(misc_set_u32)) /* af u32  */
	/* b0-bf  */
	__(b local_label(misc_set_invalid)) /* b0 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* b1 cons  */
	__(b local_label(misc_set_invalid)) /* b2 node  */
	__(b local_label(misc_set_invalid)) /* b3 imm  */
	__(b local_label(misc_set_invalid)) /* b4 odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* b5 nil  */
	__(b local_label(misc_set_invalid)) /* b6 misc  */
	__(b local_label(misc_set_s32)) /* b7 s32  */
	__(b local_label(misc_set_invalid)) /* b8 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* b9 cons  */
	__(b local_label(misc_set_invalid)) /* ba nodeheader  */
	__(b local_label(misc_set_invalid)) /* bb imm  */
	__(b local_label(misc_set_invalid)) /* bc odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* bd nil  */
	__(b local_label(misc_set_invalid)) /* be misc  */
	__(b local_label(misc_set_fixnum_vector)) /* bf fixnum_vector  */
	/* c0-cf  */
	__(b local_label(misc_set_invalid)) /* c0 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* c1 cons  */
	__(b local_label(misc_set_invalid)) /* c2 nodeheader  */
	__(b local_label(misc_set_invalid)) /* c3 imm  */
	__(b local_label(misc_set_invalid)) /* c4 odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* c5 nil  */
	__(b local_label(misc_set_invalid)) /* c6 misc  */
	__(b local_label(misc_set_new_string)) /* c7 new_string  */
	__(b local_label(misc_set_invalid)) /* c8 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* c9 cons  */
	__(b local_label(misc_set_invalid)) /* ca nodeheader  */
	__(b local_label(misc_set_invalid)) /* cb imm  */
	__(b local_label(misc_set_invalid)) /* cc odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* cd nil  */
	__(b local_label(misc_set_invalid)) /* ce misc  */
	__(b local_label(misc_set_u8)) /* cf u8  */
	/* d0-df  */
	__(b local_label(misc_set_invalid)) /* d0 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* d1 cons  */
	__(b local_label(misc_set_invalid)) /* d2 nodeheader  */
	__(b local_label(misc_set_invalid)) /* d3 imm  */
	__(b local_label(misc_set_invalid)) /* d4 odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* d5 nil  */
	__(b local_label(misc_set_invalid)) /* d6 misc  */
	__(b local_label(misc_set_s8)) /* d7 s8  */
	__(b local_label(misc_set_invalid)) /* d8 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* d9 cons  */
	__(b local_label(misc_set_invalid)) /* da nodeheader  */
	__(b local_label(misc_set_invalid)) /* db imm  */
	__(b local_label(misc_set_invalid)) /* dc odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* dd nil  */
	__(b local_label(misc_set_invalid)) /* de misc  */
	__(b local_label(misc_set_old_string)) /* df (old) simple_base_string  */
	/* e0-ef  */
	__(b local_label(misc_set_invalid)) /* e0 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* e1 cons  */
	__(b local_label(misc_set_invalid)) /* e2 nodeheader  */
	__(b local_label(misc_set_invalid)) /* e3 imm  */
	__(b local_label(misc_set_invalid)) /* e4 odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* e5 nil  */
	__(b local_label(misc_set_invalid)) /* e6 misc  */
	__(b local_label(misc_set_u16)) /* e7 u16  */
	__(b local_label(misc_set_invalid)) /* e8 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* e9 cons  */
	__(b local_label(misc_set_invalid)) /* ea nodeheader  */
	__(b local_label(misc_set_invalid)) /* eb imm  */
	__(b local_label(misc_set_invalid)) /* ec odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* ed nil  */
	__(b local_label(misc_set_invalid)) /* ee misc  */
	__(b local_label(misc_set_s16)) /* ef s16  */
	/* f0-ff  */
	__(b local_label(misc_set_invalid)) /* f0 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* f1 cons  */
	__(b local_label(misc_set_invalid)) /* f2 nodeheader  */
	__(b local_label(misc_set_invalid)) /* f3 imm  */
	__(b local_label(misc_set_invalid)) /* f4 odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* f5 nil  */
	__(b local_label(misc_set_invalid)) /* f6 misc  */
	__(b local_label(misc_set_double_float_vector)) /* f7 df vector  */
	__(b local_label(misc_set_invalid)) /* f8 even_fixnum  */
	__(b local_label(misc_set_invalid)) /* f9 cons  */
	__(b local_label(misc_set_invalid)) /* fa nodeheader  */
	__(b local_label(misc_set_invalid)) /* fb imm  */
	__(b local_label(misc_set_invalid)) /* fc odd_fixnum  */
	__(b local_label(misc_set_invalid)) /* fd nil  */
	__(b local_label(misc_set_invalid)) /* fe misc  */
	__(b local_label(misc_set_bit_vector)) /* ff bit_vector  */

local_label(misc_set_u32):        
	/* Either a non-negative fixnum, a positive one-digit bignum, */
	/* or a two-digit bignum whose sign-digit is 0 is ok.  */
	__(add imm0,arg_y,#misc_data_offset)
	__(test_fixnum(arg_z))
	__(bne local_label(set_not_fixnum_u32))
	__(tst arg_z,#0x80000000)
	__(bne local_label(set_bad))
	__(unbox_fixnum(imm1,arg_z))
local_label(set_set32):         
	__(str imm1,[arg_x,imm0])
	__(bx lr)
local_label(set_not_fixnum_u32):
	__(extract_lisptag(imm1,arg_z))
	__(cmp imm1,#tag_misc)
	__(bne local_label(set_bad))
	__(movc16(imm2,one_digit_bignum_header))
	__(getvheader(imm1,arg_z))
	__(cmp imm1,imm2)
	__(bne local_label(set_not_1_digit_u32))
	__(ldr imm1,[arg_z,#misc_data_offset])
	__(cmp imm1,#0)
	__(bge local_label(set_set32))
	__(b local_label(set_bad))
local_label(set_not_1_digit_u32):
	__(movc16(imm2,two_digit_bignum_header))
	__(cmp imm1,imm2)
	__(bne local_label(set_bad))
	__(vrefr(imm2,arg_z,1))
	__(vrefr(imm1,arg_z,0))
	__(cmp imm2,#0)
	__(beq local_label(set_set32))
local_label(set_bad):
	/* arg_z does not match the array-element-type of arg_x.  */
	__(mov arg_y,arg_z)
	__(mov arg_z,arg_x)
	__(mov arg_x,#XNOTELT)
	__(set_nargs(3))
	__(b _SPksignalerr)
local_label(misc_set_fixnum_vector):   
	__(add imm0,arg_y,#misc_data_offset)
	__(test_fixnum(arg_z))
	__(bne local_label(set_bad))
	__(unbox_fixnum(imm1,arg_z))
	__(str imm1,[arg_x,imm0])
	__(bx lr)
local_label(misc_set_new_string):   
	__(add imm0,arg_y,#misc_data_offset)
	__(extract_lowbyte(imm2,arg_z))
	__(cmp imm2,#subtag_character)
	__(bne local_label(set_bad))
	__(unbox_character(imm1,arg_z))
	__(str imm1,[arg_x,imm0])
	__(bx lr)
local_label(misc_set_s32):
	__(add imm0,arg_y,#misc_data_offset)
	__(test_fixnum(arg_z))
	__(moveq imm1,arg_z,asr #fixnumshift)
	__(beq local_label(set_set32))
	__(extract_lisptag(imm2,arg_z))
	__(cmp imm2,#tag_misc)
	__(bne local_label(set_bad))
	__(movc16(imm1,one_digit_bignum_header))
	__(getvheader(imm2,arg_z))
	__(cmp imm2,imm1)
	__(vrefr(imm1,arg_z,0))
	__(beq local_label(set_set32))
	__(b local_label(set_bad))
local_label(misc_set_single_float_vector):
	__(add imm0,arg_y,#misc_data_offset)
	__(extract_typecode(imm2,arg_z))
	__(cmp imm2,#subtag_single_float)
	__(bne local_label(set_bad))
	__(ldr imm1,[arg_z,#single_float.value])
	__(str imm1,[arg_x,imm0])
	__(bx lr)
local_label(misc_set_u8):               
	__(mov imm0,arg_y,lsr #2)
	__(add imm0,imm0,#misc_data_offset)
	__(mov imm2,#~(0xff<<fixnumshift))
	__(tst arg_z,imm2)
	__(bne local_label(set_bad))
	__(unbox_fixnum(imm1,arg_z))
	__(strb imm1,[arg_x,imm0])
	__(bx lr)
local_label(misc_set_old_string):
	__(mov imm0,arg_y,lsr #2)
	__(add imm0,imm0,#misc_data_offset)
	__(extract_lowbyte(imm2,arg_z))
	__(cmp imm2,#subtag_character)
	__(unbox_character(imm1,arg_z))
	__(bne local_label(set_bad))
	__(strb imm1,[arg_x,imm0])
	__(bx lr)
local_label(misc_set_s8):
	__(mov imm0,arg_y,lsr #2)
	__(add imm0,imm0,#misc_data_offset)
	__(test_fixnum(arg_z))
	__(bne local_label(set_bad))
	__(unbox_fixnum(imm1,arg_z))
	__(mov imm2,imm1,lsl #32-8)
	__(cmp imm1,imm2,asr #32-8)
	__(bne local_label(set_bad))
	__(strb imm1,[arg_x,imm0])
	__(bx lr)
local_label(misc_set_u16):         
	__(mov imm0,arg_y,lsr #1)
	__(add imm0,imm0,#misc_data_offset)
	__(test_fixnum(arg_z))
	__(bne local_label(set_bad))
	__(unbox_fixnum(imm1,arg_z))
	__(mov imm2,imm1,lsl #16)
	__(cmp imm1,imm2,lsr #16)
	__(bne local_label(set_bad))
	__(strh imm1,[arg_x,imm0])
	__(bx lr)
local_label(misc_set_s16):
	__(mov imm0,arg_y,lsr #1)
	__(add imm0,imm0,#misc_data_offset)
	__(test_fixnum(arg_z))
	__(bne local_label(set_bad))
	__(unbox_fixnum(imm1,arg_z))
	__(mov imm2,imm1,lsl #16)
	__(cmp imm1,imm2,asr #16)
	__(bne local_label(set_bad))
	__(strh imm1,[arg_x,imm0])
	__(bx lr)
local_label(misc_set_bit_vector):
	__(bics imm0,arg_z,#fixnumone)
	__(bne local_label(set_bad))
	__(mov imm2,#31)
	__(and imm2,imm2,arg_y,lsr #2)
	__(mov imm1,#1)
	__(mov imm1,imm1,lsl imm2)
	__(mov imm0,arg_y,lsr #fixnumshift+5)
	__(mov imm0,imm0,lsl #2)
	__(add imm0,imm0,#misc_data_offset)
	__(cmp arg_z,#0)
	__(ldr imm2,[arg_x,imm0])
	__(orrne imm2,imm2,imm1)
	__(biceq imm2,imm2,imm1)
	__(str imm2,[arg_x,imm0])
	__(bx lr)

local_label(misc_set_double_float_vector):
	__(extract_subtag(imm2,arg_z))
	__(cmp imm2,#subtag_double_float)
	__(bne local_label(set_bad))
	__(ldrd imm0,imm1,[arg_z,#misc_dfloat_offset])
	__(mov imm2,arg_y,lsl #1)
	__(add imm2,imm2,#misc_dfloat_offset)
	__(strd imm0,imm1,[arg_x,imm2])
	__(bx lr)
local_label(misc_set_invalid):  
	__(mov temp0,#XSETBADVEC)        
	__(set_nargs(4))
	__(vpush1(temp0))
	__(b _SPksignalerr)                

        
/* temp0: (stack-consed) target catch frame, imm0: count of intervening  */
/* frames. If target isn't a multiple-value receiver, discard extra values */
/* (less hair, maybe.)  */
_startfn(C(_throw_found))
        new_local_labels()
        __(ldr imm1,[temp0,#catch_frame.mvflag])
        __(cmp imm1,#0)
        __(mov fn,#0)
        __(add imm1,vsp,nargs)
        __(add imm1,imm1,#-node_size)
        __(bne local_label(throw_all_values))
        __(cmp nargs,#0)
        __(moveq imm1,#nil_value)
        __(set_nargs(1))
        __(streq imm1,[vsp,#-node_size]!)
        __(movne vsp,imm1)
local_label(throw_all_values):  
        __(bl _SPnthrowvalues) 
        __(ldr temp0,[rcontext,#tcr.catch_top])
        __(ldr imm1,[rcontext,#tcr.db_link])
        __(ldr imm0,[temp0,#catch_frame.db_link])
        __(cmp imm0,imm1)
        __(blne _SPunbind_to)
        __(ldr temp1,[temp0,#catch_frame.mvflag])
        __(ldr imm0,[temp0,#catch_frame.xframe])        
        __(ldr imm1,[temp0,#catch_frame.last_lisp_frame])
        __(cmp temp1,#0)
        __(str imm0,[rcontext,#tcr.xframe])
        __(str imm1,[rcontext,#tcr.last_lisp_frame])
        __(add imm0,vsp,nargs)
        __(sub sp,temp0,#fulltag_misc)
        __(ldr imm1,[sp,#catch_frame.size+lisp_frame.savevsp])
        __(ldreq arg_z,[imm0,#-node_size])
        __(beq local_label(throw_pushed_values))
        __(movs arg_x,nargs)
        __(b local_label(throw_push_test))
local_label(throw_push_loop):
        __(subs arg_x,arg_x,#fixnumone)
        __(ldr arg_y,[imm0,#-node_size]!)
        __(push1(arg_y,imm1))
local_label(throw_push_test):   
        __(bne local_label(throw_push_loop))
local_label(throw_pushed_values):
        __(mov vsp,imm1)
        __(ldr imm0,[temp0,#catch_frame.link])
        __(str imm0,[rcontext,#tcr.catch_top])
        __(ldr fn,[sp,#catch_frame.size+lisp_frame.savefn])
        __(ldr lr,[sp,#catch_frame.size+lisp_frame.savelr])
        __(add sp,sp,#catch_frame.size+lisp_frame.size)
        __(pop_lisp_fprs())
        __(bx lr)
_endfn(C(_throw_found))        

_startfn(C(nthrow1v))
        new_local_labels()
local_label(_nthrow1v_nextframe):
        __(subs temp2,temp2,#fixnum_one)
        __(ldr temp0,[rcontext,#tcr.catch_top])
        __(ldr imm1,[rcontext,#tcr.db_link])
        __(set_nargs(1))
        __(blt local_label(_nthrow1v_done))
        __(ldr arg_y,[temp0,#catch_frame.link])
        __(ldr imm0,[temp0,#catch_frame.db_link])
        __(cmp imm0,imm1)
        __(str arg_y,[rcontext,#tcr.catch_top])
        __(ldr arg_y,[temp0,#catch_frame.xframe])
        __(str arg_y,[rcontext,#tcr.xframe])
        __(beq local_label(_nthrow1v_dont_unbind))
        __(do_unbind_to(imm1,temp1,arg_x,arg_y))
local_label(_nthrow1v_dont_unbind):
        __(ldr temp1,[temp0,#catch_frame.catch_tag])
        __(cmp temp1,#unbound_marker)  /* unwind-protect ?  */
        __(sub sp,temp0,#fulltag_misc)
        __(beq local_label(_nthrow1v_do_unwind))
        /* A catch frame.  If the last one, restore context from there.  */
        __(cmp temp2,#0)
        __(ldreq vsp,[sp,#catch_frame.size+lisp_frame.savevsp])
        __(add sp,sp,#catch_frame.size+lisp_frame.size)
        __(pop_lisp_fprs())
        __(b local_label(_nthrow1v_nextframe))
local_label(_nthrow1v_do_unwind):
        /* This is harder, but not as hard (not as much BLTing) as the  */
        /* multiple-value case.  */
        /* Save our caller's LR and FN in the csp frame created by the unwind-  */
        /* protect.  (Clever, eh ?)  */
        __(add sp,sp,#catch_frame.size)
        /* We used to use a swp instruction to exchange the lr with
        the lisp_frame.savelr field of the lisp frame that temp0 addresses.
        Multicore ARMv7 machines include the ability to disable the swp
        instruction, and some Linux kernels do so and emulate the instruction.
        There seems to be evidence that they sometimes do so incorrectly,
        so we stopped using swp.
        pc_luser_xp() needs to do some extra work if the thread is interrupted
        in the midst of the three-instruction sequence at
	swap_lr_lisp_frame_temp0.
        */
        __(mov imm1,#0)
        __(mov temp0,sp)
        __(mov imm0,#3<<num_subtag_bits)
        __(orr imm0,imm0,#subtag_simple_vector)
        __(stmdb sp!,{imm0,imm1,arg_z,temp2})
        .globl C(swap_lr_lisp_frame_temp0)
        .globl C(swap_lr_lisp_frame_temp0_end)
        /* This instruction sequence needs support from pc_luser_xp() */
C(swap_lr_lisp_frame_temp0):            
        __(ldr imm0,[temp0,#lisp_frame.savelr])
        __(str lr,[temp0,#lisp_frame.savelr])
        __(mov lr,imm0)
C(swap_lr_lisp_frame_temp0_end):            
        __(ldr nfn,[temp0,#lisp_frame.savefn])
        __(str fn,[temp0,#lisp_frame.savefn])
        __(ldr vsp,[temp0,#lisp_frame.savevsp])
        __(mov fn,nfn)
        __(add temp0,temp0,#lisp_frame.size)
        __(restore_lisp_fprs(temp0))
        __(str imm1,[rcontext,#tcr.unwinding])
        __(blx lr)
        __(mov imm1,#1)
        __(ldr arg_z,[sp,#8])
        __(str imm1,[rcontext,#tcr.unwinding])
        __(ldr temp2,[sp,#12])
        __(add sp,sp,#4*node_size)
        __(restore_lisp_frame(imm0))
        __(discard_lisp_fprs())
        __(b local_label(_nthrow1v_nextframe))
local_label(_nthrow1v_done):
        __(mov imm0,#0)
        __(str imm0,[rcontext,#tcr.unwinding])
        /* nargs has an undefined value here, so we can clobber it while */
        /* polling for a deferred interrupt  */
        __(check_pending_interrupt(nargs))
        __(bx lr)
_endfn        

_startfn(C(nthrownv))
        new_local_labels()
local_label(nthrownv_nextframe):
        __(subs temp2,temp2,#fixnum_one)
        __(ldr temp0,[rcontext,#tcr.catch_top])
        __(ldr imm1,[rcontext,#tcr.db_link])
        __(blt local_label(nthrownv_done))
        __(ldr arg_y,[temp0,#catch_frame.link])
        __(ldr imm0,[temp0,#catch_frame.db_link])
        __(cmp imm0,imm1)
        __(str arg_y,[rcontext,#tcr.catch_top])
        __(ldr arg_y,[temp0,#catch_frame.xframe])
        __(str arg_y,[rcontext,#tcr.xframe])
        __(beq local_label(nthrownv_dont_unbind))
        __(do_unbind_to(imm1,temp1,arg_x,arg_y))
local_label(nthrownv_dont_unbind):
        __(ldr temp1,[temp0,#catch_frame.catch_tag])
        __(cmp temp1,#unbound_marker)  /* unwind-protect ?  */
        __(sub sp,temp0,#fulltag_misc)
        __(beq local_label(nthrownv_do_unwind))
        __(cmp temp2,#0)
/* A catch frame.  If the last one, restore context from there.  */
	__(bne local_label(nthrownv_skip))
        __(ldr imm0,[sp,#catch_frame.size+lisp_frame.savevsp])
        __(add imm1,vsp,nargs)
        __(movs arg_z,nargs)
        __(b local_label(nthrownv_push_test))
local_label(nthrownv_push_loop):        
        __(subs arg_z,arg_z,#fixnumone)
        __(ldr temp1,[imm1,#-node_size]!)
        __(push1(temp1,imm0))
local_label(nthrownv_push_test):        
        __(bne local_label(nthrownv_push_loop))
        __(mov vsp,imm0)
local_label(nthrownv_skip):     
        __(add sp,sp,#catch_frame.size+lisp_frame.size)
        __(pop_lisp_fprs())          
        __(b local_label(nthrownv_nextframe))                
local_label(nthrownv_do_unwind):
        __(ldr arg_x,[temp0,#catch_frame.xframe])
        __(ldr arg_z,[temp0,#catch_frame.last_lisp_frame])
        __(sub sp,temp0,#fulltag_misc)
        __(str arg_x,[rcontext,#tcr.xframe])
        __(str arg_z,[rcontext,#tcr.last_lisp_frame])
        __(add sp,sp,#catch_frame.size)
        __(add imm1,nargs,#node_size)
        __(mov arg_z,sp)
        __(dnode_align(imm0,imm1,node_size))
        __(mov imm1,imm1,lsl #num_subtag_bits-fixnumshift)
        __(orr imm1,imm1,#subtag_u32_vector)
        __(stack_allocate_zeroed_ivector(imm1,imm0))
        __(mov imm0,#subtag_simple_vector)
        __(strb imm0,[sp])
        __(str temp2,[sp,#node_size])
        __(add temp2,sp,#dnode_size)
        __(add temp2,temp2,nargs)
        __(add temp1,vsp,nargs)
        __(b local_label(nthrownv_tpushtest))
local_label(nthrownv_tpushloop):        
        __(ldr temp0,[temp1,#-node_size]!)
        __(push1(temp0,temp2))
local_label(nthrownv_tpushtest):        
        __(subs nargs,nargs,#fixnumone)
        __(bge local_label(nthrownv_tpushloop))
        __(mov imm1,#0)
        /* This instruction sequence needs support from pc_luser_xp() */
        .globl C(swap_lr_lisp_frame_arg_z)
        .globl C(swap_lr_lisp_frame_arg_z_end)
C(swap_lr_lisp_frame_arg_z):                   
        __(ldr imm0,[arg_z,#lisp_frame.savelr])
        __(str lr,[arg_z,#lisp_frame.savelr])
        __(mov lr,imm0)
C(swap_lr_lisp_frame_arg_z_end):                   
        __(ldr nfn,[arg_z,#lisp_frame.savefn])
        __(str fn,[arg_z,#lisp_frame.savefn])
        __(ldr vsp,[arg_z,#lisp_frame.savevsp])
        __(add arg_z,arg_z,#lisp_frame.size)
        __(restore_lisp_fprs(arg_z))
        __(str imm1,[rcontext,#tcr.unwinding])
        __(mov fn,nfn)
        __(blx lr)
        __(mov imm1,#1)
        __(str imm1,[rcontext,#tcr.unwinding])
        __(ldr imm0,[sp])
        __(header_length(imm0,imm0))
        __(subs nargs,imm0,#node_size)
        __(add imm0,imm0,#node_size)
        __(add temp0,sp,imm0)
        __(mov imm0,nargs)
        __(add arg_z,temp0,#node_size)
        __(bic arg_z,arg_z,#fulltagmask)
        __(b local_label(nthrownv_tpoptest))
local_label(nthrownv_tpoploop):  
        __(subs imm0,imm0,#node_size)        
        __(vpush1(temp2))
local_label(nthrownv_tpoptest):  
        __(ldr temp2,[temp0,#-node_size]!)
        __(bne local_label(nthrownv_tpoploop))
        __(mov sp,arg_z)
        __(ldr fn,[sp,#lisp_frame.savefn])
        __(ldr lr,[sp,#lisp_frame.savelr])
        __(discard_lisp_frame())
        __(discard_lisp_fprs())
        __(b local_label(nthrownv_nextframe))
local_label(nthrownv_done):     
        __(mov imm0,#0)
        __(str imm0,[rcontext,#tcr.unwinding])
        __(check_pending_interrupt(imm1))
        __(bx lr)
_endfn                

_startfn(C(progvsave))        
        /* Error if arg_z isn't a proper list.  That's unlikely, */
        /* but it's better to check now than to crash later. */
        __(cmp arg_z,#nil_value)
        __(mov arg_x,arg_z) /* fast  */
        __(mov temp1,arg_z) /* slow  */
        __(beq 9f)  /* Null list is proper  */
0: 
        __(trap_unless_list(arg_x,imm0))
        __(_cdr(temp2,arg_x)) /* (null (cdr fast)) ?  */
        __(trap_unless_list(temp2,imm0,cr0))
        __(cmp temp2,#nil_value)
        __(_cdr(arg_x,temp2))
        __(beq 9f)
        __(_cdr(temp1,temp1))
        __(cmp arg_x,temp1)
        __(bne 0b)
        __(mov arg_y,#XIMPROPERLIST)
        __(set_nargs(2))
        __(b _SPksignalerr)
9:      /* Whew   */
 
        /* Next, determine the length of arg_y.  We  */
        /* know that it's a proper list.  */
        __(mov imm0,#0)
        __(mov arg_x,arg_y)
1:
        __(cmp arg_x,#nil_value)
        __(addne imm0,imm0,#node_size)
        __(_cdr(arg_x,arg_x))
        __(bne 1b)
        /* imm0 is now (boxed) triplet count.  */
        /* Determine word count, add 1 (to align), and make room.  */
        /* if count is 0, make an empty tsp frame and exit  */
        __(cmp imm0,#0)
        __(add imm1,imm0,imm0,lsl #1)
        __(add imm1,imm1,#node_size) /* Room for binding count */
        __(dnode_align(imm2,imm1,node_size))
        __(bne 2f)
        __(movc16(imm0,make_header(1,subtag_simple_vector)))
        __(mov imm1,#0)
        __(stmdb sp!,{imm0,imm1})
        __(b 9f)
2:
        __(orr imm1,imm1,fixnumone) /* force odd */
        __(mov imm1,imm1,lsl #num_subtag_bits-fixnumshift)
        __(orr imm1,imm1,#subtag_u32_vector)
        __(mov temp1,sp)
        __(stack_allocate_zeroed_ivector(imm1,imm2))
        __(mov imm1,#subtag_simple_vector)
        __(strb imm1,[sp])
        __(str imm0,[sp,#node_size])
        __(ldr imm1,[rcontext,#tcr.db_link])
3:      __(_car(temp0,arg_y))
        __(ldr imm0,[temp0,#symbol.binding_index])
        __(ldr imm2,[rcontext,#tcr.tlb_limit])
        __(_cdr(arg_y,arg_y))
        __(cmp imm2,imm0)
        __(bhi 4f)
        __(uuo_tlb_too_small(al,imm0))
4:              
        __(ldr arg_x,[rcontext,#tcr.tlb_pointer])
        __(ldr temp0,[arg_x,imm0])
        __(cmp arg_z,#nil_value)
        __(mov temp2,#unbound_marker)
        __(ldrne temp2,[arg_z,#cons.car])
        __(_cdr(arg_z,arg_z))
        __(cmp arg_y,#nil_value)
        __(push1(temp0,temp1))
        __(push1(imm0,temp1))
        __(push1(imm1,temp1))
        __(mov imm1,temp1)
        __(str temp2,[arg_x,imm0])
        __(bne 3b)
        __(str imm1,[rcontext,#tcr.db_link])
9:              
        __(mov arg_z,#unbound_marker)
        __(mov imm2,#fixnum_one)
        __(mkcatch())        
        __(bx lr)
_endfn                                
               
/* Too large to safely fit on tstack.  Heap-cons the vector, but make  */
/* sure that there's an empty tsp frame to keep the compiler happy.  */
_startfn(stack_misc_alloc_no_room)
        __(mov imm0,#stack_alloc_marker)
        __(mov imm1,sp)
        __(stmdb sp!,{imm0,imm1})
        __(b _SPmisc_alloc)
_endfn        
_startfn(stack_misc_alloc_init_no_room)
/* Too large to safely fit on tstack.  Heap-cons the vector, but make  */
/* sure that there's an empty tsp frame to keep the compiler happy.  */
        __(mov imm0,#stack_alloc_marker)
        __(mov imm1,sp)
        __(stmdb sp!,{imm0,imm1})
        __(b _SPmisc_alloc_init)
_endfn        
_startfn(stack_misc_alloc_init_ivector)
        __(mov imm0,arg_x,lsl #num_subtag_bits-fixnumshift)
        __(orr imm0,imm0,arg_y,lsr #fixnumshift)
        __(cmp arg_y,#max_32_bit_ivector_subtag<<fixnumshift)
        __(movle imm1,arg_x)
        __(ble 8f)
        __(cmp arg_y,#max_8_bit_ivector_subtag<<fixnumshift)
        __(movle imm1,arg_x,lsr #fixnumshift)
        __(ble 8f)
        __(cmp arg_y,#max_16_bit_ivector_subtag<<fixnumshift)
        __(movle imm1,arg_x,lsr #1)
        __(ble 8f)
        __(cmp arg_y,#subtag_double_float)
        __(moveq imm1,arg_x,lsl #1)
        __(addeq imm1,imm1,#node_size)
        __(addne imm1,arg_x,#7<<fixnumshift)
        __(movne imm1,imm1,lsr#3+fixnumshift)
8:      __(dnode_align(imm1,imm1,node_size))
        __(ldr temp0,[rcontext,#tcr.cs_limit])
        __(sub temp1,sp,imm1)
        __(cmp temp1,temp0)
        __(bls stack_misc_alloc_init_no_room)
        __(mov temp0,#stack_alloc_marker)
        __(mov temp1,sp)
        __(stack_allocate_zeroed_ivector(imm0,imm1))
        __(mov arg_y,arg_z)
        __(add arg_z,sp,#fulltag_misc)
        __(stmdb sp!,{temp0,temp1})
        __(b initialize_vector)
_endfn        
/* This is called from a lisp-style context and calls a lisp function. */
/* This does the moral equivalent of */
/*   (loop  */
/*	(let* ((fn (%function_on_top_of_lisp_stack))) */
/*	  (if fn */
/*           (catch %toplevel-catch% */
/*	       (funcall fn)) */
/*            (return nil)))) */

_startfn(toplevel_loop)
        __(build_lisp_frame(imm0))
	__(b local_label(test))
local_label(loop):
	__(ref_nrs_value(arg_z,toplcatch))
	__(bl _SPmkcatch1v)
	__(b local_label(test))	/* cleanup address, not really a branch */
        __(ldr nfn,[vsp,#0])
	__(set_nargs(0))
        __(bl _SPfuncall)
	__(mov arg_z,#nil_value)
	__(mov imm0,#fixnum_one)
	__(bl _SPnthrow1value)
local_label(test):
        __(ldr nfn,[vsp,#0])
        __(cmp nfn,#nil_value)
	__(bne local_label(loop))
        __(return_lisp_frame(imm0))
	_endfn


/* This gets called with R0 pointing to the current TCR. */
/* r1 is 0 if we want to start the whole thing rolling, */
/* non-zero if we want to reset the current process */
/* by throwing to toplevel */

	.globl _SPreset
_exportfn(C(start_lisp))
        __(stmdb sp!,{r4,r5,r6,r7,r8,r9,r10,r11,r12,lr})
        __(mov rcontext,r0)
        __(mov r0,sp)
        __(tst sp,#4)
        __(strne r0,[sp,#-4]!)
        __(streq r0,[sp,#-8]!)
        __(mov arg_z,#0)
        __(mov arg_y,#0)
        __(mov arg_x,#0)
        __(mov temp0,#0)
        __(mov temp1,#0)
        __(mov temp2,#0)
        __(mov allocptr,#VOID_ALLOCPTR)
        __(mov fn,#0)
        __(ldr vsp,[rcontext,#tcr.save_vsp])
        __(ldr imm2,[rcontext,#tcr.last_lisp_frame])
        __(sub imm0,imm2,sp)
        __(add imm0,imm0,#node_size)
        __(mov imm0,imm0,lsl #num_subtag_bits-word_shift)
        __(orr imm0,imm0,#subtag_u32_vector)
        __(stmdb sp!,{imm0,imm2})
        __(push_foreign_fprs())
        __(adr imm0,1f)
        __(fldd double_float_zero,[imm0])
        __(mov imm0,#TCR_STATE_LISP)
        __(str imm0,[rcontext,#tcr.valence])
        __(ldr allocptr,[rcontext,#tcr.save_allocptr])
        __(bl toplevel_loop)
        __(ldr imm1,[sp,#(9*8)+4])
        __(mov imm0,#TCR_STATE_FOREIGN)
        __(str imm1,[rcontext,#tcr.last_lisp_frame])
        __(str imm0,[rcontext,#tcr.valence])
        __(pop_foreign_fprs())
        __(add sp,sp,#2*node_size)
        __(mov imm0,#nil_value)
        __(ldr sp,[sp])
        __(ldmia sp!,{r4,r5,r6,r7,r8,r9,r10,r11,r12,lr})
        __(bx lr)
_endfn
        
        .align 3
1:
        .long 0
        .long 0

/* This gets called with r0 = the current thread's TCR.  Should
   call RESTORE-LISP-POINTERS and return 0 if it returns normally
   and non-0 if it throws. */
        
_exportfn(C(init_lisp))
        new_local_labels()
        new_macro_labels()
        __(stmdb sp!,{r4,r5,r6,r7,r8,r9,r10,r11,r12,lr})
        __(mov rcontext,r0)
        __(mov r0,sp)
        __(tst sp,#4)
        __(strne r0,[sp,#-4]!)
        __(streq r0,[sp,#-8]!)
        __(mov arg_z,#0)
        __(mov arg_y,#0)
        __(mov arg_x,#0)
        __(mov temp0,#0)
        __(mov temp1,#0)
        __(mov temp2,#0)
        __(mov allocptr,#VOID_ALLOCPTR)
        __(mov fn,#0)
        __(ldr vsp,[rcontext,#tcr.save_vsp])
        __(ldr imm2,[rcontext,#tcr.last_lisp_frame])
        __(sub imm0,imm2,sp)
        __(add imm0,imm0,#node_size)
        __(mov imm0,imm0,lsl #num_subtag_bits-word_shift)
        __(orr imm0,imm0,#subtag_u32_vector)
        __(stmdb sp!,{imm0,imm2})
        __(push_foreign_fprs())
        __(adr imm0,1b)
        __(fldd double_float_zero,[imm0])
        __(mov imm0,#TCR_STATE_LISP)
        __(str imm0,[rcontext,#tcr.valence])
        __(ldr allocptr,[rcontext,#tcr.save_allocptr])
        __(ref_nrs_function(nfn,restore_lisp_pointers))
        __(extract_subtag(imm0,nfn))
        __(cmp imm0,#subtag_function)
        __(bne local_label(fail))
        __(ref_nrs_value(arg_z,toplcatch))
        __(bl _SPmkcatch1v)
        __(b local_label(fail)) /* cleanup address */
        __(ref_nrs_function(nfn,restore_lisp_pointers))
        __(set_nargs(0))
        __(bl _SPfuncall)
        __(mov arg_z,#0)
        __(mov imm0,#fixnum_one)
        __(bl _SPnthrow1value)
        __(b local_label(done))
local_label(fail):      
        __(mov arg_z,#fixnum_one)
local_label(done):                     
        __(ldr imm1,[sp,#(9*8)+4])
        __(mov imm0,#TCR_STATE_FOREIGN)
        __(str imm1,[rcontext,#tcr.last_lisp_frame])
        __(str imm0,[rcontext,#tcr.valence])
        __(pop_foreign_fprs())
        __(add sp,sp,#2*node_size)
        __(unbox_fixnum(imm0,arg_z))
        __(ldr sp,[sp])
        __(ldmia sp!,{r4,r5,r6,r7,r8,r9,r10,r11,r12,lr})
        __(bx lr)
_endfn

/* Called (on something other than the initial thread) with r0=tcr,
   r1 = a (void *) arg.  Returns a null pointer/0, but declared to return
   void.
*/                
_exportfn(C(os_main))
        new_local_labels()
        new_macro_labels()
        __(stmdb sp!,{r4,r5,r6,r7,r8,r9,r10,r11,r12,lr})
        __(mov rcontext,r0)
        __(mov r0,sp)
        __(tst sp,#4)
        __(strne r0,[sp,#-4]!)
        __(streq r0,[sp,#-8]!)
        __(mov arg_z,#0)
        __(mov arg_y,#0)
        __(mov arg_x,#0)
        __(mov temp0,#0)
        __(mov temp1,#0)
        __(mov temp2,#0)
        __(mov allocptr,#VOID_ALLOCPTR)
        __(mov fn,#0)
        __(ldr vsp,[rcontext,#tcr.save_vsp])
        __(ldr imm2,[rcontext,#tcr.last_lisp_frame])
        __(sub imm0,imm2,sp)
        __(add imm0,imm0,#node_size)
        __(mov imm0,imm0,lsl #num_subtag_bits-word_shift)
        __(orr imm0,imm0,#subtag_u32_vector)
        __(stmdb sp!,{imm0,imm2})
        __(push_foreign_fprs())
        __(adr imm0,1b)
        __(fldd double_float_zero,[imm0])
        __(mov imm0,#TCR_STATE_LISP)
        __(str imm0,[rcontext,#tcr.valence])
        __(ldr allocptr,[rcontext,#tcr.save_allocptr])
        __(movc16(imm0,make_header(macptr.element_count,subtag_macptr)))
        __(stmdb sp!,{imm0,imm1,arg_z,arg_y})
        __(add arg_z,sp,#fulltag_misc)
        __(ref_nrs_function(nfn,os_init_function))
        __(set_nargs(1))
        __(bl _SPfuncall)
        __(add sp,sp,#16)
        __(ldr imm1,[sp,#(9*8)+4])
        __(mov imm0,#TCR_STATE_FOREIGN)
        __(str imm1,[rcontext,#tcr.last_lisp_frame])
        __(str imm0,[rcontext,#tcr.valence])
        __(pop_foreign_fprs())
        __(add sp,sp,#2*node_size)
        __(mov imm0,#nil_value)
        __(ldr sp,[sp])
        __(ldmia sp!,{r4,r5,r6,r7,r8,r9,r10,r11,r12,lr})
        __(bx lr)
_endfn
                                
        .data
        .globl C(sptab)
        .globl C(sptab_end)
        new_local_labels()
C(sptab):
        .long local_label(start)
C(sptab_end):   
        .long local_label(end)
local_label(start):                     
        .long _SPfix_nfn_entrypoint /* must be first */
        .long _SPbuiltin_plus
        .long _SPbuiltin_minus
        .long _SPbuiltin_times
        .long _SPbuiltin_div
        .long _SPbuiltin_eq
        .long _SPbuiltin_ne
        .long _SPbuiltin_gt
        .long _SPbuiltin_ge
        .long _SPbuiltin_lt
        .long _SPbuiltin_le
        .long _SPbuiltin_eql
        .long _SPbuiltin_length
        .long _SPbuiltin_seqtype
        .long _SPbuiltin_assq
        .long _SPbuiltin_memq
        .long _SPbuiltin_logbitp
        .long _SPbuiltin_logior
        .long _SPbuiltin_logand
        .long _SPbuiltin_ash
        .long _SPbuiltin_negate
        .long _SPbuiltin_logxor
        .long _SPbuiltin_aref1
        .long _SPbuiltin_aset1
        .long _SPfuncall
        .long _SPmkcatch1v
        .long _SPmkcatchmv
        .long _SPmkunwind
        .long _SPbind
        .long _SPconslist
        .long _SPconslist_star
        .long _SPmakes32
        .long _SPmakeu32
        .long _SPfix_overflow
        .long _SPmakeu64
        .long _SPmakes64
        .long _SPmvpass
        .long _SPvalues
        .long _SPnvalret
        .long _SPthrow
        .long _SPnthrowvalues
        .long _SPnthrow1value
        .long _SPbind_self
        .long _SPbind_nil
        .long _SPbind_self_boundp_check
        .long _SPrplaca
        .long _SPrplacd
        .long _SPgvset
        .long _SPset_hash_key
        .long _SPstore_node_conditional
        .long _SPset_hash_key_conditional
        .long _SPstkconslist
        .long _SPstkconslist_star
        .long _SPmkstackv
        .long _SPsetqsym
        .long _SPprogvsave
        .long _SPstack_misc_alloc
        .long _SPgvector
        .long _SPfitvals
        .long _SPnthvalue
        .long _SPdefault_optional_args
        .long _SPopt_supplied_p
        .long _SPheap_rest_arg
        .long _SPreq_heap_rest_arg
        .long _SPheap_cons_rest_arg
        .long _SPcheck_fpu_exception
        .long _SPdiscard_stack_object
        .long _SPksignalerr
        .long _SPstack_rest_arg
        .long _SPreq_stack_rest_arg
        .long _SPstack_cons_rest_arg
        .long _SPcall_closure        
        .long _SPspreadargz
        .long _SPtfuncallgen
        .long _SPtfuncallslide
        .long _SPjmpsym
        .long _SPtcallsymgen
        .long _SPtcallsymslide
        .long _SPtcallnfngen
        .long _SPtcallnfnslide
        .long _SPmisc_ref
        .long _SPsubtag_misc_ref
        .long _SPmakestackblock
        .long _SPmakestackblock0
        .long _SPmakestacklist
        .long _SPstkgvector
        .long _SPmisc_alloc
        .long _SPatomic_incf_node
        .long _SPunused1
        .long _SPunused2
        .long _SPrecover_values
        .long _SPinteger_sign
        .long _SPsubtag_misc_set
        .long _SPmisc_set
        .long _SPspread_lexprz
        .long _SPreset
        .long _SPmvslide
        .long _SPsave_values
        .long _SPadd_values
        .long _SPmisc_alloc_init
        .long _SPstack_misc_alloc_init
        .long _SPpopj
        .long _SPudiv64by32
        .long _SPgetu64
        .long _SPgets64
        .long _SPspecref
        .long _SPspecrefcheck
        .long _SPspecset
        .long _SPgets32
        .long _SPgetu32
        .long _SPmvpasssym
        .long _SPunbind
        .long _SPunbind_n
        .long _SPunbind_to
        .long _SPprogvrestore
        .long _SPbind_interrupt_level_0
        .long _SPbind_interrupt_level_m1
        .long _SPbind_interrupt_level
        .long _SPunbind_interrupt_level
        .long _SParef2
        .long _SParef3
        .long _SPaset2
        .long _SPaset3
        .long _SPkeyword_bind
        .long _SPudiv32
        .long _SPsdiv32
        .long _SPeabi_ff_call
        .long _SPdebind
        .long _SPeabi_callback
        .long _SPeabi_ff_callhf
local_label(end):       
        	_endfile
