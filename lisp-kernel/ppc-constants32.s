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


define([rcontext],[r13])
        
nbits_in_word = 32
nbits_in_byte = 8
ntagbits = 3	/* But only 2 are significant to lisp */
nlisptagbits = 2
nfixnumtagbits = 2
num_subtag_bits = 8
fixnumshift = 2
fixnum_shift = 2
fulltagmask = 7
tagmask = 3
fixnummask = 3
ncharcodebits = 24              /* arguably, we're only using the low 8 */
charcode_shift = nbits_in_word-ncharcodebits
word_shift = 2
node_size = 4
dnode_size = 8
dnode_align_bits = 3
dnode_shift = dnode_align_bits
bitmap_shift = 5


fixnumone = (1<<fixnumshift)
fixnum_one = fixnumone
fixnum1 = fixnumone


/* Tags. */
/* There are two-bit tags and three-bit tags. */
/* A FULLTAG is the value of the low three bits of a tagged object. */
/* A TAG is the value of the low two bits of a tagged object. */
/* A TYPECODE is either a TAG or the value of a "tag-misc" objects header-byte. */

/* There are 4 primary TAG values.  Any object which lisp can "see" can be classified */
/* by its TAG.  (Some headers have FULLTAGS that are congruent modulo 4 with the */
/* TAGS of other objects, but lisp can't "see" headers.) */


tag_fixnum = 0	/* All fixnums, whether odd or even */
tag_list = 1	/* Conses and NIL */
tag_misc = 2	/* Heap-consed objects other than lists: vectors, symbols, functions, floats ... */
tag_imm = 3	/* Immediate-objects: characters, UNBOUND, other markers. */


/*  And there are 8 FULLTAG values.  Note that NIL has its own FULLTAG (congruent mod 4 to tag-list), */
/*  that FULLTAG-MISC is > 4 (so that code-vector entry-points can be branched to, since the low */
/*  two bits of the PC are ignored) and that both FULLTAG-MISC and FULLTAG-IMM have header fulltags */
/*  that share the same TAG. */
/*  Things that walk memory (and the stack) have to be careful to look at the FULLTAG of each */
/*  object that they see. */


fulltag_even_fixnum = 0	/* I suppose EVENP/ODDP might care; nothing else does. */
fulltag_cons = 1	/* a real (non_null) cons.  Shares TAG with fulltag_nil. */
fulltag_nodeheader = 2	/* Header of heap_allocated object that contains lisp_object pointers */
fulltag_imm = 3	/* a "real" immediate object.  Shares TAG with fulltag_immheader. */
fulltag_odd_fixnum = 4	/* */
fulltag_nil = 5	/* NIL and nothing but.  (Note that there]s still a hidden NILSYM.) */
fulltag_misc = 6	/* Pointer "real" tag_misc object.  Shares TAG with fulltag_nodeheader. */
fulltag_immheader = 7	/* Header of heap-allocated object that contains unboxed data. */

nil_value = 0x00003015+LOWMEM_BIAS
misc_bias = fulltag_misc
cons_bias = tag_list        

/* Functions are of (conceptually) unlimited size. */
	_struct(_function,-misc_bias)
	 _node(header)
	 _node(codevector)
	_ends

	_struct(tsp_frame,0)
	 _node(backlink)
	 _node(type)
	 _struct_label(fixed_overhead)
	 _struct_label(data_offset)
	_ends

/* Order of CAR and CDR doesn]t seem to matter much - there aren]t */
/* too many tricks to be played with predecrement/preincrement addressing. */
/* Keep them in the confusing MCL 3.0 order, to avoid confusion. */
	_struct(cons,-cons_bias)
	 _node(cdr)
	 _node(car)
	_ends
	
misc_header_offset = -fulltag_misc
misc_subtag_offset = misc_header_offset+3		/* low byte of header */
misc_data_offset = misc_header_offset+4		/* first word of data */
misc_dfloat_offset = misc_header_offset+8		/* double-floats are doubleword-aligned */

max_64_bit_constant_index = ((0x7fff + misc_dfloat_offset)>>3)
max_32_bit_constant_index = ((0x7fff + misc_data_offset)>>2)
max_16_bit_constant_index = ((0x7fff + misc_data_offset)>>1)
max_8_bit_constant_index = (0x7fff + misc_data_offset)
max_1_bit_constant_index = ((0x7fff + misc_data_offset)<<5)

/* T is almost adjacent to NIL: since NIL is a misaligned CONS, it spans */
/* two doublewords.  The arithmetic difference between T and NIL is */
/* such that the least-significant bit and exactly one other bit is */
/* set in the result. */

t_offset = (8+(8-fulltag_nil)+fulltag_misc)
t_value = nil_value+t_offset

/* The order in which various header values are defined is significant in several ways: */
/* 1) Numeric subtags precede non-numeric ones; there are further orderings among numeric subtags. */
/* 2) All subtags which denote CL arrays are preceded by those that don]t, */
/*    with a further ordering which requires that (< header-arrayH header-vectorH ,@all-other-CL-vector-types) */
/* 3) The element-size of ivectors is determined by the ordering of ivector subtags. */
/* 4) All subtags are >= fulltag-immheader . */

define([define_subtag],[
subtag_$1 = $2|($3<<ntagbits)])
	
define([define_imm_subtag],[
	define_subtag($1,fulltag_immheader,$2)])

	
define([define_node_subtag],[
	define_subtag($1,fulltag_nodeheader,$2)])

		
/*Immediate subtags. */
	define_subtag(character,fulltag_imm,9)
	define_subtag(unbound,fulltag_imm,6)
        define_subtag(illegal,fulltag_imm,10)
	define_subtag(go_tag,fulltag_imm,12)
	define_subtag(block_tag,fulltag_imm,24)
	define_subtag(vsp_protect,fulltag_imm,7)
        define_subtag(no_thread_local_binding,fulltag_imm,30)
unbound_marker = subtag_unbound
undefined = unbound_marker
illegal_marker = subtag_illegal
no_thread_local_binding_marker = subtag_no_thread_local_binding
/*Numeric subtags. */

	define_imm_subtag(bignum,0)
min_numeric_subtag = subtag_bignum

	define_node_subtag(ratio,1)
max_rational_subtag = subtag_ratio

	define_imm_subtag(single_float,1)
	define_imm_subtag(double_float,2)
min_float_subtag = subtag_single_float
max_float_subtag = subtag_double_float
max_real_subtag = subtag_double_float

	define_node_subtag(complex,3)
max_numeric_subtag = subtag_complex


/* CL array types.  There are more immediate types than node types; all CL array subtags must be > than */
/* all non-CL-array subtags.  So we start by defining the immediate subtags in decreasing order, starting */
/* with that subtag whose element size isn]t an integral number of bits and ending with those whose */
/* element size - like all non-CL-array fulltag-immheader types - is 32 bits. */

	define_imm_subtag(bit_vector,31)
	define_imm_subtag(double_float_vector,30)
	define_imm_subtag(s16_vector,29)
	define_imm_subtag(u16_vector,28)
min_16_bit_ivector_subtag = subtag_u16_vector
max_16_bit_ivector_subtag = subtag_s16_vector
	define_imm_subtag(s8_vector,26)
	define_imm_subtag(u8_vector,25)
min_8_bit_ivector_subtag = subtag_u8_vector
max_8_bit_ivector_subtag = fulltag_immheader|(27<<ntagbits)
        define_imm_subtag(simple_base_string,24)
        define_imm_subtag(fixnum_vector,23)
	define_imm_subtag(s32_vector,22)
	define_imm_subtag(u32_vector,21)
	define_imm_subtag(single_float_vector,20)
max_32_bit_ivector_subtag = fulltag_immheader|(24<<ntagbits)
min_cl_ivector_subtag = subtag_single_float_vector


	define_node_subtag(vectorH,20)
	define_node_subtag(arrayH,19)
	define_node_subtag(simple_vector,21)
min_vector_subtag = subtag_vectorH
min_array_subtag = subtag_arrayH

/* So, we get the remaining subtags (n: (n > max-numeric-subtag) & (n < min-array-subtag)) */
/* for various immediate/node object types. */

	define_imm_subtag(macptr,3)
min_non_numeric_imm_subtag = subtag_macptr

	define_imm_subtag(dead_macptr,4)
	define_imm_subtag(code_vector,5)
	define_imm_subtag(creole,6)

max_non_array_imm_subtag = (18<<ntagbits)|fulltag_immheader

	define_node_subtag(catch_frame,4)
	define_node_subtag(function,5)
	define_node_subtag(basic_stream,6)
	define_node_subtag(symbol,7)
	define_node_subtag(lock,8)
	define_node_subtag(hash_vector,9)
	define_node_subtag(pool,10)
	define_node_subtag(weak,11)
	define_node_subtag(package,12)
	define_node_subtag(slot_vector,13)
	define_node_subtag(instance,14)
	define_node_subtag(struct,15)
	define_node_subtag(istruct,16)
	define_node_subtag(value_cell,17)
        define_node_subtag(xfunction,18)
max_non_array_node_subtag = (18<<ntagbits)|fulltag_immheader
	
/* The objects themselves look something like this: */
	_structf(ratio)
	 _node(numer)
	 _node(denom)
	_endstructf

	_structf(single_float)
	 _word(value)
	_endstructf

	_structf(double_float)
	 _word(pad)
	 _dword(value)
	_endstructf

	_structf(symbol)
	 _node(pname)
	 _node(vcell)
	 _node(fcell)
	 _node(package_predicate)
	 _node(flags)
         _node(plist)
         _node(binding_index)
	_endstructf

	_structf(catch_frame)
	 _node(catch_tag)	/* #<unbound> -> unwind-protect, else catch */
	 _node(link)		/* backpointer to previous catch frame */
	 _node(mvflag)		/* 0 if single-valued catch, fixnum 1 otherwise */
	 _node(csp)		/* pointer to lisp_frame on csp */
	 _node(db_link)		/* head of special-binding chain */
	 _field(regs,8*node_size)	/* save7-save0 */
	 _node(xframe)		/* exception frame chain */
	 _node(tsp_segment)	/* maybe someday; padding for now */
	_endstructf

	_structf(macptr)
	 _node(address)
         _node(domain)
         _node(type)
	_endstructf

	_structf(vectorH)
	 _node(logsize)
	 _node(physsize)
	 _node(data_vector)
	 _node(displacement)
	 _node(flags)
	_endstructf

        _structf(arrayH)
         _node(rank)
         _node(physsize)
         _node(data_vector)
         _node(displacement)
         _node(flags)
         _struct_label(dim0)
        _endstructf
        
	
        
	_struct(c_frame,0)	/* PowerOpen ABI C stack frame */
	 _node(backlink)
	 _node(crsave)
	 _node(savelr)
	 _field(unused, 8)
	 _node(savetoc)
	 _struct_label(params)
         _node(param0)
         _node(param1)
         _node(param2)
         _node(param3)
         _node(param4)
         _node(param5)
         _node(param6)
         _node(param7)
	 _struct_label(minsiz)
	_ends


	_struct(eabi_c_frame,0)
	 _word(backlink) 
	 _word(savelr)
	 _word(param0)
	 _word(param1)
	 _word(param2)
	 _word(param3)
	 _word(param4)
	 _word(param5)
	 _word(param6)
	 _word(param7)
	 _struct_label(minsiz)
	_ends

	/* For entry to variable-argument-list functions */
/*	  (e.g., via callback) */
	_struct(varargs_eabi_c_frame,0)
	 _word(backlink)
	 _word(savelr)
	 _struct_label(va_list)
	 _word(flags)		/* gpr count byte, fpr count byte, padding */
	 _word(overflow_arg_area)
	 _word(reg_save_area)
	 _field(padding,4)
	 _struct_label(regsave)
	 _field(gp_save,8*node_size)
	 _field(fp_save,8*8)
	 _word(old_backlink)
	 _word(old_savelr)
	 _struct_label(incoming_stack_args)
	_ends
        	
	_struct(lisp_frame,0)
	 _node(backlink) 
	 _node(savefn)	
	 _node(savelr)	
	 _node(savevsp)	
	_ends

	_struct(vector,-fulltag_misc)
	 _node(header)
	 _struct_label(data)
	_ends

        _struct(binding,0)
         _node(link)
         _node(sym)
         _node(val)
        _ends



symbol_extra = symbol.size-fulltag_misc
	
	_struct(nrs,nil_value-fulltag_nil)
	 _struct_pad(fulltag_nil)
	 _field(nilptr,16-fulltag_nil)

	 _struct_pad(fulltag_misc)
	 _struct_label(tsym)
	 _struct_pad(symbol_extra)	/* t */

	 _struct_pad(fulltag_misc)
	 _struct_label(nilsym)
	 _struct_pad(symbol_extra)	/* nil */

	 _struct_pad(fulltag_misc)
	 _struct_label(errdisp)
	 _struct_pad(symbol_extra)	/* %err-disp */

	 _struct_pad(fulltag_misc)
	 _struct_label(cmain)
	 _struct_pad(symbol_extra)	/* cmain */

	 _struct_pad(fulltag_misc)
	 _struct_label(eval)
	 _struct_pad(symbol_extra)	/* eval */
 
	 _struct_pad(fulltag_misc)
	 _struct_label(appevalfn)
	 _struct_pad(symbol_extra)	/* apply-evaluated-function */

	 _struct_pad(fulltag_misc)
	 _struct_label(error)
	 _struct_pad(symbol_extra)	/* error */

	 _struct_pad(fulltag_misc)
	 _struct_label(defun)
	 _struct_pad(symbol_extra)	/* %defun */

	 _struct_pad(fulltag_misc)
	 _struct_label(defvar)
	 _struct_pad(symbol_extra)	/* %defvar */

	 _struct_pad(fulltag_misc)
	 _struct_label(defconstant)
	 _struct_pad(symbol_extra)	/* %defconstant */

	 _struct_pad(fulltag_misc)
	 _struct_label(macrosym)
	 _struct_pad(symbol_extra)	/* %macro */

	 _struct_pad(fulltag_misc)
	 _struct_label(kernelrestart)
	 _struct_pad(symbol_extra)	/* %kernel-restart */

	 _struct_pad(fulltag_misc)
	 _struct_label(package)
	 _struct_pad(symbol_extra)	/* *package* */

	 _struct_pad(fulltag_misc)
	 _struct_label(total_bytes_freed)		/* *total-bytes-freed* */
	 _struct_pad(symbol_extra)

	 _struct_pad(fulltag_misc)
	 _struct_label(kallowotherkeys)
	 _struct_pad(symbol_extra)	/* allow-other-keys */

	 _struct_pad(fulltag_misc)
	 _struct_label(toplcatch)
	 _struct_pad(symbol_extra)	/* %toplevel-catch% */

	 _struct_pad(fulltag_misc)
	 _struct_label(toplfunc)
	 _struct_pad(symbol_extra)	/* %toplevel-function% */

	 _struct_pad(fulltag_misc)
	 _struct_label(callbacks)
	 _struct_pad(symbol_extra)	/* %pascal-functions% */

	 _struct_pad(fulltag_misc)
	 _struct_label(allmeteredfuns)
	 _struct_pad(symbol_extra)	/* *all-metered-functions* */

	 _struct_pad(fulltag_misc)
	 _struct_label(total_gc_microseconds)		/* *total-gc-microseconds* */
	 _struct_pad(symbol_extra)

	 _struct_pad(fulltag_misc)
	 _struct_label(builtin_functions)		/* %builtin-functions% */
	 _struct_pad(symbol_extra)                

	 _struct_pad(fulltag_misc)
	 _struct_label(udf)
	 _struct_pad(symbol_extra)	/* %unbound-function% */

	 _struct_pad(fulltag_misc)
	 _struct_label(init_misc)
	 _struct_pad(symbol_extra)	/* %init-misc */

	 _struct_pad(fulltag_misc)
	 _struct_label(macro_code)
	 _struct_pad(symbol_extra)	/* %macro-code% */

	 _struct_pad(fulltag_misc)
	 _struct_label(closure_code)
	 _struct_pad(symbol_extra)      /* %closure-code% */

       	 _struct_pad(fulltag_misc)
	 _struct_label(new_gcable_ptr) /* %new-gcable-ptr */
	 _struct_pad(symbol_extra)
	
       	 _struct_pad(fulltag_misc)
	 _struct_label(gc_event_status_bits)
	 _struct_pad(symbol_extra)	/* *gc-event-status-bits* */

	 _struct_pad(fulltag_misc)
	 _struct_label(post_gc_hook)
	 _struct_pad(symbol_extra)	/* *post-gc-hook* */

	 _struct_pad(fulltag_misc)
	 _struct_label(handlers)
	 _struct_pad(symbol_extra)	/* %handlers% */


	 _struct_pad(fulltag_misc)
	 _struct_label(all_packages)
	 _struct_pad(symbol_extra)	/* %all-packages% */

	 _struct_pad(fulltag_misc)
	 _struct_label(keyword_package)
	 _struct_pad(symbol_extra)	/* *keyword-package* */

	 _struct_pad(fulltag_misc)
	 _struct_label(finalization_alist)
	 _struct_pad(symbol_extra)	/* %finalization-alist% */

	 _struct_pad(fulltag_misc)
	 _struct_label(foreign_thread_control)
	 _struct_pad(symbol_extra)	/* %foreign-thread-control */

	_ends

define([def_header],[
$1 = ($2<<num_subtag_bits)|$3])

	def_header(single_float_header,single_float.element_count,subtag_single_float)
	def_header(double_float_header,double_float.element_count,subtag_double_float)
	def_header(one_digit_bignum_header,1,subtag_bignum)
	def_header(two_digit_bignum_header,2,subtag_bignum)
	def_header(three_digit_bignum_header,3,subtag_bignum)
	def_header(symbol_header,symbol.element_count,subtag_symbol)
	def_header(value_cell_header,1,subtag_value_cell	)
	def_header(macptr_header,macptr.element_count,subtag_macptr)
	def_header(vectorH_header,vectorH.element_count,subtag_vectorH)

	include(errors.s)

/* Symbol bits that we care about */
sym_vbit_bound = (0+fixnum_shift)
sym_vbit_bound_mask = (1<<sym_vbit_bound)
sym_vbit_const = (1+fixnum_shift)
sym_vbit_const_mask = (1<<sym_vbit_const)

	_struct(area,0)
	 _node(pred) 
	 _node(succ) 
	 _node(low) 
	 _node(high) 
	 _node(active) 
	 _node(softlimit) 
	 _node(hardlimit) 
	 _node(code) 
	 _node(markbits) 
	 _node(ndwords) 
	 _node(older) 
	 _node(younger) 
	 _node(h) 
	 _node(sofprot) 
	 _node(hardprot) 
	 _node(owner) 
	 _node(refbits) 
	 _node(nextref) 
	_ends


/* This is only referenced by c->lisp code that needs to save/restore C NVRs in a TSP frame. */
	_struct(c_reg_save,0)
	 _node(tsp_link)	/* backpointer */
	 _node(tsp_mark)	/* frame type */
	 _node(save_fpscr)	/* for Cs FPSCR */
	 _field(save_gprs,19*4) /* r13-r31 */
	 _dword(save_fp_zero)	/* for fp_zero */
	 _dword(save_fps32conv)
         _field(save_fprs,13*8)
	_ends


TCR_BIAS = 0
/* TCR_BIAS = 0x7000 */
        
/*  Thread context record. */

	_struct(tcr,-TCR_BIAS)
	 _node(prev)		/* in doubly-linked list */
	 _node(next)		/* in doubly-linked list */
	 _node(lisp_fpscr)	/* lisp thread's fpscr (in low word) */
	 _node(lisp_fpscr_low)
	 _node(db_link)		/* special binding chain head */
	 _node(catch_top)	/* top catch frame */
	 _node(save_vsp)	/* VSP when in foreign code */
	 _node(save_tsp)	/* TSP when in foreign code */
	 _node(cs_area)		/* cstack area pointer */
	 _node(vs_area)		/* vstack area pointer */
	 _node(ts_area)		/* tstack area pointer */
	 _node(cs_limit)	/* cstack overflow limit */
	 _node(bytes_consed_high)
	 _node(bytes_consed_low)
	 _node(log2_allocation_quantum)
	 _node(interrupt_pending)
	 _node(xframe)		/* per-thread exception frame list */
	 _node(errno_loc)	/* per-thread  errno location */
	 _node(ffi_exception)	/* fpscr exception bits from ff-call */
	 _node(osid)		/* OS thread id */
         _node(valence)		/* odd when in foreign code */
	 _node(foreign_exception_status)
	 _node(native_thread_info)
	 _node(native_thread_id)
	 _node(last_allocptr)
	 _node(save_allocptr)
	 _node(save_allocbase)
	 _node(reset_completion)
	 _node(activate)
         _node(suspend_count)
         _node(suspend_context)
	 _node(pending_exception_context)
	 _node(suspend)		/* semaphore for suspension notify */
	 _node(resume)		/* sempahore for resumption notify */
	 _node(flags)      
	 _node(gc_context)
         _node(termination_semaphore)
         _node(unwinding)
         _node(tlb_limit)
         _node(tlb_pointer)     /* Consider using tcr+N as tlb_pointer */
	 _node(shutdown_count)
         _node(safe_ref_address)
	_ends

TCR_FLAG_BIT_FOREIGN = fixnum_shift
TCR_FLAG_BIT_AWAITING_PRESET = (fixnum_shift+1)
TCR_FLAG_BIT_ALT_SUSPEND = (fixnumshift+2)
TCR_FLAG_BIT_PROPAGATE_EXCEPTION = (fixnumshift+3)
TCR_FLAG_BIT_SUSPEND_ACK_PENDING = (fixnumshift+4)
TCR_FLAG_BIT_PENDING_EXCEPTION = (fixnumshift+5)
TCR_FLAG_BIT_FOREIGN_EXCEPTION = (fixnumshift+6)
TCR_FLAG_BIT_PENDING_SUSPEND = (fixnumshift+7)        
	
r0 = 0
r1 = 1
r2 = 2
r3 = 3
r4 = 4
r5 = 5
r6 = 6
r7 = 7
r8 = 8
r9 = 9
r10 = 10
r11 = 11
r12 = 12
r13 = 13
r14 = 14
r15 = 15
r16 = 16
r17 = 17
r18 = 18
r19 = 19
r20 = 20
r21 = 21
r22 = 22
r23 = 23
r24 = 24
r25 = 25
r26 = 26
r27 = 27
r28 = 28
r29 = 29
r30 = 30
r31 = 31

/* Lisp code keeps 0.0 in fp_zero */
define([fp_zero],[f31])   /* a non-volatile reg as far as FFI is concerned. */
define([fp_s32conv],[f30])   /* for s32->fp conversion */
	
/* registers, as used in destrucuring-bind/macro-bind */

define([whole_reg],[temp1])
define([arg_reg],[temp3])
define([keyvect_reg],[temp2])
define([mask_req_start],[24])
define([mask_req_width],[8])
define([mask_opt_start],[16])
define([mask_opt_width],[8])
define([mask_key_start],[8])
define([mask_key_width],[8])
define([mask_initopt],[7])
define([mask_keyp],[6]) /*  note that keyp can be true even when 0 keys. */
define([mask_aok],[5])
define([mask_restp],[4])

ifdef([DARWIN],[
	define([STACK_ALIGN],16)
	define([STACK_ALIGN_MASK],15)
],[
	define([STACK_ALIGN],8)
	define([STACK_ALIGN_MASK],7)
])

define([TCR_STATE_FOREIGN],1)
define([TCR_STATE_LISP],0)
define([TCR_STATE_EXCEPTION_WAIT],2)
define([TCR_STATE_EXCEPTION_RETURN],4)

define([RESERVATION_DISCHARGE],0x2004)

lisp_globals_limit = (0x3010+(LOWMEM_BIAS))
        
INTERRUPT_LEVEL_BINDING_INDEX = fixnumone
