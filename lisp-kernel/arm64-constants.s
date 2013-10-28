/*   Copyright (C) 2003-2009, Clozure Associates. */
/*   Copyright (C) 1994-2001 Digitool, Inc */
/*   This file is part of Clozure CL. */

/*   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public */
/*   License , known as the LLGPL and distributed with Clozure CL as the */
/*   file "LICENSE".  The LLGPL consists of a preamble and the LGPL, */
/*   which is distributed with Clozure CL as the file "LGPL".  Where these */
/*   conflict, the preamble takes precedence. */

/*   Clozure CL is referenced in the preamble as the "LIBRARY." */

/*   The LLGPL is also available online at */
/*   http://opensource.franz.com/preamble.html */

define(`imm0',`x0')  
define(`imm1',`x1')
define(`imm2',`x2')
define(`imm3',`x3')
define(`imm4',`x4')
define(`imm5',`x5') define(`nargs',`x5')
define(`rnil',`x6')
define(`rt',`x7')
/* If we need this, should point to _SPcall_closure.  Used in FFI. */
define(`rclosure_call',`x8')
        
define(`temp3',`x9')
define(`temp2',`x10')
define(`temp1',`x11')
define(`temp0',`x12')
define(`arg_x',`x13')                
define(`arg_y',`x14')                
define(`arg_z',`x15')                
define(`save0',`x16')
define(`save1',`x17')
define(`save2',`x18')
define(`save3',`x19')
define(`save4',`x20')
define(`save5',`x21')
define(`save6',`x22')
define(`save7',`x23')
        
define(`loc_pc',`x24')
define(`vsp',`x25')
define(`allocptr',`x26')
define(`allocbase',`x27')                
define(`rcontext',`x28')

define(`lr',`x30')        
define(`fname',`temp3')
define(`nfn',`temp2')

define(`vzero',`q31')
        
nbits_in_word = 64
nbits_in_byte = 8
ntagbits = 8
nlisptagbits = 8
nfixnumtagbits = 8
nlowtagbits = 0      
num_subtag_bits = 8
fixnumshift = 0
fixnum_shift = 0
fulltagmask = 15
tagmask = 7
fixnummask = 7
ncharcodebits = 8
charcode_shift = 8
word_shift = 3
node_size = 8
dnode_size = 16
dnode_mask = (dnode_size-1)        
dnode_align_bits = 4
dnode_shift = dnode_align_bits        
bitmap_shift = 6
        
fixnumone = 1
fixnum_one = fixnumone
fixnum1 = fixnumone

/* The ARM64 can (and generally is) configured to ignore the
   top 8 bits of a 64-bit address, allowing us to use those
   bits as tags. */

tag_shift = 56
                        
/* 56-bit fixnums have the sign-extension of bit 55 in their
   high 8 bits. */

tag_positive_fixnum = 0         /* non-negative *
tag_negative_fixnum = 0xff

/* If we do addition or subtraction on a pair of (SIGNED-BYTE 56) values,
   the result may overflow by one bit.  The GC should ignore such things ;
   we can't do too much with them besides boxing them as bignums ... */

tag_overflowed_positive_fixnum = 1
tag_overflowed_negative_fixnum = 0xfe
                                
           
/* lists have their 6 most significant bits (58-63) clear and bit 57
   set. */
list_leading_zero_bits = 6        
tag_list = 2
tag_nil = 3

/* leaf node objects (characters, single-floats, other markers) have their
   top 3 bits clear and bit 60 set. */
        
imm_tag_mask = 0x10
tag_single_float = (imm_tag_mask | 0)
tag_character = (imm_tag_mask | 1)
tag_unbound = (imm_tag_mask | 2)
unbound_marker = (tag_unbound << tag_shift)
tag_slot_unbound = (imm_tag_mask | 3)
slot_unbound_marker = (tag_slot_unbound << tag_shift)
tag_no_thread_local_binding = (imm_tag | 4)
no_thread_local_binding_marker = (tag_no_thread_local_binding << tag_shift)
tag_illegal = (imm_tag | 5)
illegal_marker = (tag_illegal << tag_shift)  
tag_stack_alloc = (tag_imm | 6)
stack_alloc_marker = (tag_stack_alloc << tag_shift)                      

/* Everything else is either (a) the tag of a uvector header, which
   is the first word in a non-CONS allocated object or (b) a pointer
   to a uvector, where the tag of the pointer encodes the type of the
   uvector.  A header's tag top 2 bits are #b10 ;  a uvector reference's
   top 2 bits are #b10.  A uvector reference or header with bit 5 set describes
   a gvector (one whose contents are nodes.) */

gvector_tag_bit = 5        
gvector_tag_mask = (1<<gvecor_tag_bit)        
uvector_ref = 0x40
uvector_header = 0x80
uvector_mask = (uvector_header | uvector_ref)
cl_ivector_tag_bit = 0        
cl_ivector_mask = (1<<cl_ivector_tag_bit)
cl_ivector_ref = (uvector_ref | cl_ivector_mask)  
cl_ivector_ref_mask = (uvector_mask | gvector_tag_mask | cl_ivector_mask)
        
define(`define_uvector',`
tag_$1 = (uvector_ref | ($2))
$1_header = (uvector_header | ($2))
        ')
define(`define_ivector',`define_uvector($1,($2<<1))')
define(`define_cl_ivector',`define_uvector($1,($2<<1)|1)')        
define(`define_gvector',`define_uvector($1,($2|gvector_tag_mask))')

define_cl_ivector(bit_vector,0)
define_cl_ivector(s8_vector,1)
define_cl_ivector(u8_vector,2)                
define_cl_ivector(s16_vector,3)
define_cl_ivector(u16_vector,4)
define_cl_ivector(s32_vector,8)
define_cl_ivector(u32_vector,9)
define_ivector(xcode_vector,10)        
define_cl_ivector(single_float_vector,10)
define_ivector(double_float,11)
define_cl_ivector(simple_string,11)
define_cl_ivector(s64_vector,12)
define_cl_ivector(u64_vector,13)
define_ivector(macptr,14)        
define_cl_ivector(fixnum_vector,14)  
define_ivector(dead_macptr,15)              
define_cl_ivector(double_float_vector,15)

min_8_bit_ivector_header = s8_vector_header
min_16_bit_ivector_header = s16_vector_header
min_32_bit_ivector_header = s32_vector_header
min_64_bit_ivector_header = s64_vector_header

define_gvector(ratio,0)
define_gvector(complex,1)
define_gvector(function,2)
define_gvector(symbol,3)
define_gvector(catch_frame,4)
define_gvector(basic_stream,5)                                        
define_gvector(lock,6)
define_gvector(hash_vector,7)
define_gvector(pool,8)
define_gvector(weak,9)
define_gvector(package,10)
define_gvector(slot_vector,11)
define_gvector(instance,12)
define_gvector(struct,13)
define_gvector(istruct,14)
define_gvector(value_cell,15)
define_gvector(xfunction,16)
define_gvector(simple_vector,28)
define_gvector(vectorH,29)
define_gvector(arrayH,31)        
			
misc_bias = -node_size
cons_bias = misc_bias
function_bias = misc_bias
t_value = (0x3000+fulltag_misc)	
define(`t_offset',-symbol.size)
	
misc_header_offset = node_bias
misc_data_offset = 0
misc_header_byte_offset = (node_bias + (node_size-1))       /* high byte of header */
misc_dfloat_offset = 0		/* double-floats are doubleword-aligned */



	
max_64_bit_constant_index = 0x400
max_32_bit_constant_index = 0x400
max_16_bit_constant_index = 0x400
max_8_bit_constant_index = 0x400
max_1_bit_constant_index = 0


	
/* The objects themselves look something like this: */
	
/* Order of CAR and CDR doesn't seem to matter much - there aren't */
/* too many tricks to be played with predecrement/preincrement addressing. */
/* Keep them in the confusing MCL 3.0 order, to avoid confusion. */
	_struct(cons,cons_bias)
	 _node(cdr)
	 _node(car)
	_ends
	
	_structf(ratio)
	 _node(numer)
	 _node(denom)
	_endstructf
	
	_structf(double_float)
         _word(val_low)
	 _word(value)
	_endstructf
	
	_structf(macptr)
	 _node(address)
         _node(domain)
         _node(type)
	_endstructf
	
/* Functions are of (conceptually) unlimited size. */
	_struct(_function,function_bias)
         _struct_label(entrypoint)
	_ends




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
         _node(_save0)
         _node(_save1)
         _node(_save2)
         _node(_save3)
         _node(_save4)
         _node(_save5)
         _node(_save6)
         _node(_save7)
	 _node(link)		/* backpointer to previous catch frame */
	 _node(mvflag)		/* 0 if single-valued catch, fixnum 1 otherwise */
	 _node(db_link)		/* head of special-binding chain */
	 _node(xframe)		/* exception frame chain */
	 _node(last_lisp_frame) /* from TCR */
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
        
	_struct(lisp_frame,0)
	 _node(savevsp)	
	 _node(savelr)	
	_ends

	_struct(vector,misc_bias)
	 _node(header)
	 _struct_label(data)
	_ends

        _struct(binding,0)
         _node(link)
         _node(sym)
         _node(val)
        _ends


/* Nilreg-relative globals.  Talking the assembler into doing something reasonable here */
/* is surprisingly hard. */


nrs_origin = node_size
nrs_symbol_fulltag = node_size
define(`nilsym',`nil')        
lisp_globals_limit = -node_size
        
        include(lisp_globals.s)
        
	

define(`def_header',`
$1 = ($2<<num_subtag_bits)|$3')

	def_header(two_digit_bignum_header,2,bignum_header)
	def_header(three_digit_bignum_header,3,bignum_header)
	def_header(four_digit_bignum_header,4,bignum_header)
	def_header(five_digit_bignum_header,5,subtag_bignum)        

	include(errors.s)

/* Symbol bits that we care about */
sym_vbit_bound = (0)
sym_vbit_bound_mask = (1<<sym_vbit_bound)
sym_vbit_const = (1)
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




TCR_BIAS = 0
	
/*  Thread context record. */

	_struct(tcr,0)
	 _node(prev)		/* in doubly-linked list */
	 _node(next)		/* in doubly-linked list */
         _node(single_float_convert) /* xxxf0 */
	 _word(lisp_fpscr)	/* lisp thread's fpscr (in low word) */
	 _word(lisp_fpscr_low)
	 _node(db_link)		/* special binding chain head */
	 _node(catch_top)	/* top catch frame */
	 _node(save_vsp)	/* VSP when in foreign code */
	 _node(save_tsp)	/* TSP when in foreign code */
	 _node(cs_area)		/* cstack area pointer */
	 _node(vs_area)		/* vstack area pointer */
	 _node(ts_area)		/* tstack area pointer */
	 _node(cs_limit)	/* cstack overflow limit */
	 _word(bytes_consed_high)
	 _word(bytes_consed_low)
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
         _word(flags_pad)
	 _word(flags)      
	 _node(gc_context)
         _node(termination_semaphore)
         _node(unwinding)
         _node(tlb_limit)
         _node(tlb_pointer)     /* Consider using tcr+N as tlb_pointer */
	 _node(shutdown_count)
         _node(safe_ref_address)
	_ends

TCR_FLAG_BIT_FOREIGN = 0
TCR_FLAG_BIT_AWAITING_PRESET = 1
TCR_FLAG_BIT_ALT_SUSPEND = 2
TCR_FLAG_BIT_PROPAGATE_EXCEPTION = 3
TCR_FLAG_BIT_SUSPEND_ACK_PENDING = 4
TCR_FLAG_BIT_PENDING_EXCEPTION = 5
TCR_FLAG_BIT_FOREIGN_EXCEPTION = 6
TCR_FLAG_BIT_PENDING_SUSPEND = 7        


nil_value = (0x3000+symbol.size+fulltag_misc+(LOWMEM_BIAS))
        	
define(`RESERVATION_DISCHARGE',(0x2008+(LOWMEM_BIAS)))


        
INTERRUPT_LEVEL_BINDING_INDEX = fixnumone
        
/* Condition bits, not to be confused with condition codes (which
   depend on them.) */
        
nzvc_n = 8
nzvc_z = 4
nzvc_v = 2
nzvc_c = 1                                                
