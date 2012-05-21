define(`eax_l',`eax')
define(`ecx_l',`ecx')
define(`edx_l',`edx')
define(`ebx_l',`ebx')
define(`esi_l',`esi')
define(`edi_l',`edi')

define(`eax_b',`al')
define(`ecx_b',`cl')
define(`edx_b',`dl')
define(`ebx_b',`bl')

define(`imm0',`eax')
	define(`imm0_l',`eax')
	define(`imm0_w',`ax')
	define(`imm0_b',`al')
	define(`imm0_bh',`ah')
	define(`Rimm0',`0')

define(`temp0',`ecx')
	define(`temp0_l',`ecx')
	define(`temp0_w',`cx')
	define(`temp0_b',`cl')
	define(`temp0_bh',`ch')
	define(`Rtemp0',`1')

define(`temp1',`edx')
	define(`temp1_l',`edx')
	define(`temp1_w',`dx')
	define(`temp1_b',`dl')
	define(`temp1_bh',`dh')
	define(`Rtemp1',`2')

define(`arg_z',`ebx')
	define(`arg_z_l',`ebx')
	define(`arg_z_w',`bx')
	define(`arg_z_b',`bl')
	define(`arg_z_bh',`bh')
	define(`Rarg_z',`3')

define(`arg_y',`esi')
	define(`Rarg_y',`6')

define(`fn',`edi')
	define(`Rfn',`7')

define(`rcontext_reg',`fs')
	
define(`rcontext',`%rcontext_reg:$1')

define(`fname',`temp0')
define(`allocptr',`temp0')

define(`nargs',`temp1')
define(`nargs_w',`temp1_w')

define(`ra0',`temp0')
define(`xfn',`temp1')

define(`allocptr',`temp0')
define(`stack_temp',`mm7')

define(`fp0',`xmm0')		
define(`fp1',`xmm1')		
define(`fp2',`xmm2')		
define(`fp3',`xmm3')		
define(`fp4',`xmm4')		
define(`fp5',`xmm5')		
define(`fp6',`xmm6')		
define(`fp7',`xmm7')		
define(`fpzero',`fp7')

nbits_in_word = 32
nbits_in_byte = 8
ntagbits = 3
nlisptagbits = 2
nfixnumtagbits = 2
num_subtag_bits = 8
subtag_shift = num_subtag_bits
fixnumshift = 2
fixnum_shift = 2
fulltagmask = 7
tagmask = 3
fixnummask = 3
ncharcodebits = 8
charcode_shift = 8
word_shift = 2
node_size = 4
dnode_size = 8
dnode_align_bits = 3
dnode_shift = dnode_align_bits        
bitmap_shift = 5

fixnumone = (1<<fixnumshift)
fixnum_one = fixnumone
fixnum1 = fixnumone

nargregs = 2

tag_fixnum = 0
tag_list = 1
tag_misc = 2
tag_imm = 3

fulltag_even_fixnum = 0
fulltag_cons = 1
fulltag_nodeheader = 2
fulltag_imm = 3
fulltag_odd_fixnum = 4
fulltag_tra = 5
fulltag_misc = 6
fulltag_immheader = 7

define(`define_subtag',`subtag_$1 = ($2 | ($3 << ntagbits))')
define(`define_imm_subtag',`define_subtag($1,fulltag_immheader,$2)')
define(`define_node_subtag',`define_subtag($1,fulltag_nodeheader,$2)')

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

define_node_subtag(arrayH,19)
define_node_subtag(vectorH,20)
define_node_subtag(simple_vector,21)
min_vector_subtag = subtag_vectorH
min_array_subtag = subtag_arrayH

define_imm_subtag(macptr,3)
min_non_numeric_imm_subtag = subtag_macptr
define_imm_subtag(dead_macptr,4)
define_imm_subtag(xcode_vector,7)

define_subtag(unbound,fulltag_imm,6)
unbound_marker = subtag_unbound
undefined = unbound_marker
define_subtag(character,fulltag_imm,9)
define_subtag(slot_unbound,fulltag_imm,10)
slot_unbound_marker = subtag_slot_unbound
define_subtag(illegal,fulltag_imm,11)
illegal = subtag_illegal
define_subtag(reserved_frame,fulltag_imm,29)
reserved_frame_marker = subtag_reserved_frame
define_subtag(no_thread_local_binding,fulltag_imm,30)
no_thread_local_binding_marker = subtag_no_thread_local_binding
define_subtag(function_boundary_marker,fulltag_imm,31)
function_boundary_marker = subtag_function_boundary_marker

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

misc_header_offset = -fulltag_misc
misc_subtag_offset = misc_header_offset
misc_data_offset = misc_header_offset+node_size
misc_dfloat_offset = misc_header_offset+8

nil_value = ((0x13000 + fulltag_cons)+(LOWMEM_BIAS))
t_value = ((0x13008 + fulltag_misc)+(LOWMEM_BIAS))
t_offset = (t_value-nil_value)
misc_bias = fulltag_misc
cons_bias = fulltag_cons

	_struct(cons,-cons_bias)
         _node(cdr)
         _node(car)
        _ends

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

	_structf(macptr)
         _node(address)
         _node(domain)
         _node(type)
        _endstructf

	_structf(catch_frame)
	 _node(catch_tag)  /* #<unbound> -> unwind-protect, else catch */
	 _node(link)	   /* backpointer to previous catch frame */
	 _node(mvflag)     /* 0 if single-valued catch, fixnum 1 otherwise */
	 _node(esp)	   /* saved lisp esp */
	 _node(ebp)	   /* saved lisp ebp */
	 _node(foreign_sp) /* necessary? */
	 _node(db_link)	   /* head of special-binding chain */
	 _node(xframe)	   /* exception frame chain */
	 _node(pc)	   /* TRA of catch exit or cleanup form */
	_endstructf

	_struct(_function,-misc_bias)
         _node(header)
         _node(codevector)
        _ends

        _struct(tsp_frame,0)
         _node(backlink)
         _node(save_ebp)
         _struct_label(fixed_overhead)
         _struct_label(data_offset)
        _ends

	_struct(csp_frame,0)
         _node(backlink)
         _node(save_ebp)
         _struct_label(fixed_overhead)
         _struct_label(data_offset)
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
	 _node(backlink) 
	 _node(savera0)	
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

nrs_origin = 0x13008+(LOWMEM_BIAS)
nrs_symbol_fulltag = fulltag_misc        
lisp_globals_limit = (0x13000+LOWMEM_BIAS)
	
        include(lisp_globals.s)


define(`def_header',`$1 = ($2<<num_subtag_bits)|$3')

def_header(single_float_header,single_float.element_count,subtag_single_float)
def_header(double_float_header,double_float.element_count,subtag_double_float)
def_header(one_digit_bignum_header,1,subtag_bignum)
def_header(two_digit_bignum_header,2,subtag_bignum)
def_header(three_digit_bignum_header,3,subtag_bignum)
def_header(symbol_header,symbol.element_count,subtag_symbol)
def_header(value_cell_header,1,subtag_value_cell)
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

ifdef(`WIN_32',`
TCR_BIAS = 0xe10 + (4 * 30)	/* last 34 words of TlsSlots */
',`
TCR_BIAS = 0
')

ifdef(`WIN_32',`
        _struct(tcr,TCR_BIAS)
         _node(linear)          /* our linear (non-segment-based) address. */
	 _word(aux)		/* pointer to tcr_aux struct, see below */
         _node(valence)         /* odd when in foreign code */
         _word(node_regs_mask)
         _node(save_allocbase)
         _node(save_allocptr)
         _node(last_allocptr)
         _node(catch_top)       /* top catch frame */
         _node(db_link)         /* special binding chain head */
         _node(tlb_limit)
         _node(tlb_pointer)     /* Consider using tcr+N as tlb_pointer */
         _node(ffi_exception)   /* mxcsr exception bits from ff-call */
         _node(foreign_sp)      /* Saved foreign SP when in lisp code */
         _node(interrupt_pending)
	 _node(next_method_context)
         _node(next_tsp)
         _node(safe_ref_address)
         _node(save_tsp)        /* TSP when in foreign code */
         _node(save_vsp)        /* VSP when in foreign code */
         _node(save_ebp)        /* lisp EBP when in foreign code */
         _node(ts_area)         /* tstack area pointer */
         _node(vs_area)         /* vstack area pointer */
         _node(xframe)          /* per-thread exception frame list */
         _node(unwinding)
         _node(flags)      
	 _node(foreign_mxcsr)
         _word(lisp_mxcsr)
	 _word(pending_exception_context)
	 _word(unboxed0)
	 _word(unboxed1)
	 _node(save0)		/* spill area for node registers... */
	 _node(save1)	 	/* ...must be 16-byte aligned */
	 _node(save2)
	 _node(save3)
        _ends

	_struct(tcr_aux,0)
         _word(bytes_allocated)
         _word(bytes_allocated_high)
         _node(cs_area)         /* cstack area pointer */
         _node(cs_limit)        /* cstack overflow limit */
         _node(log2_allocation_quantum)
         _node(errno_loc)       /* per-thread  errno location */
         _node(osid)            /* OS thread id */
         _node(foreign_exception_status)
         _node(native_thread_info)
         _node(native_thread_id)
         _node(reset_completion)
         _node(activate)
         _node(gc_context)
         _node(termination_semaphore)
         _node(shutdown_count)
         _node(suspend_count)
         _node(suspend_context)
         _node(suspend)         /* semaphore for suspension notify */
         _node(resume)          /* sempahore for resumption notify */
         _word(allocated)
         _word(pending_io_info)
         _word(io_datum)
         _node(next)            /* in doubly-linked list */
         _node(prev)            /* in doubly-linked list */
	_ends
',`
/*  Thread context record.  */

        _struct(tcr,TCR_BIAS)
         _node(next)            /* in doubly-linked list */
         _node(prev)            /* in doubly-linked list */
         _word(node_regs_mask)
         _node(linear)          /* our linear (non-segment-based) address. */
	 _node(save0)		/* spill area for node registers (16-byte aligned ) */
	 _node(save1)
	 _node(save2)
	 _node(save3)
         _node(save_ebp)        /* lisp EBP when in foreign code */
         _word(lisp_mxcsr)
         _word(foreign_mxcsr)   
         _node(db_link)         /* special binding chain head */
         _node(catch_top)       /* top catch frame */
         _node(save_vsp)        /* VSP when in foreign code */
         _node(save_tsp)        /* TSP when in foreign code */
         _node(foreign_sp)      /* Saved foreign SP when in lisp code */
         _node(cs_area)         /* cstack area pointer */
         _node(vs_area)         /* vstack area pointer */
         _node(ts_area)         /* tstack area pointer */
         _node(cs_limit)        /* cstack overflow limit */
         _word(bytes_allocated)
         _word(bytes_consed_high)
         _node(log2_allocation_quantum)
         _node(interrupt_pending)
         _node(xframe)          /* per-thread exception frame list */
         _node(errno_loc)       /* per-thread  errno location */
         _node(ffi_exception)   /* mxcsr exception bits from ff-call */
         _node(osid)            /* OS thread id */
         _node(valence)         /* odd when in foreign code */
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
         _node(suspend)         /* semaphore for suspension notify */
         _node(resume)          /* sempahore for resumption notify */
         _node(flags)      
         _node(gc_context)
         _node(termination_semaphore)
         _node(unwinding)
         _node(tlb_limit)
         _node(tlb_pointer)     /* Consider using tcr+N as tlb_pointer */
         _node(shutdown_count)
         _node(next_tsp)
         _node(safe_ref_address)
	 _word(ldt_selector)
	 _word(scratch_mxcsr)
	 _word(unboxed0)
	 _word(unboxed1)
	 _node(next_method_context)
	 _word(save_eflags)
         _word(allocated)
         _word(pending_io_info)
         _word(io_datum)
        _ends
')

        _struct(win32_context,0)
	 _field(ContextFlags, 4)
	 _field(Dr0, 4)
	 _field(Dr1, 4)
	 _field(Dr2, 4)
	 _field(Dr3, 4)
	 _field(Dr6, 4)
	 _field(Dr7, 4)
	 _struct_label(FloatSave)
	 _field(ControlWord, 4);
	 _field(StatusWord, 4)
	 _field(TagWord, 4)
	 _field(ErrorOffset, 4)
	 _field(ErrorSelector, 4)
	 _field(DataOffset, 4)
	 _field(DataSelector, 4)
         _field(RegisterArea, 80)
	 _field(Cr0NpxState, 4)
        
	 _field(SegGs, 4)
	 _field(SegFs, 4)
	 _field(SegEs, 4)
	 _field(SegDs, 4)
	 _field(Edi, 4)
	 _field(Esi, 4)
	 _field(Ebx, 4)
	 _field(Edx, 4)
	 _field(Ecx, 4)
	 _field(Eax, 4)
	 _field(Ebp, 4)
	 _field(Eip, 4)
	 _field(SegCs, 4)
	 _field(EFlags, 4)
	 _field(Esp, 4)
	 _field(SegSs, 4)
         _struct_label(ExtendedRegisters)
         _struct_pad(24)
         _field(MXCSR,4)
         _struct_pad(132) /* (- 160 28) */
         _field(Xmm0,16)
         _field(Xmm1,16)
         _field(Xmm2,16)
         _field(Xmm3,16)
         _field(Xmm4,16)
         _field(Xmm5,16)
         _field(Xmm6,16)
         _field(Xmm7,16)
         _struct_pad(224)
         _ends
        
target_most_positive_fixnum = 536870911
target_most_negative_fixnum = -536870912
call_arguments_limit = 0x10000

        
INTERRUPT_LEVEL_BINDING_INDEX = fixnumone


ifdef(`DARWIN',`
c_stack_16_byte_aligned = 1
',`
c_stack_16_byte_aligned = 0
')                
