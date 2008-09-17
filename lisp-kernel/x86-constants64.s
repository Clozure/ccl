/*   Copyright (C) 2005 Clozure Associates  */
/*   This file is part of OpenMCL.    */

/*   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public  */
/*   License , known as the LLGPL and distributed with OpenMCL as the  */
/*   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,  */
/*   which is distributed with OpenMCL as the file "LGPL".  Where these  */
/*   conflict, the preamble takes precedence.    */
 
/*   OpenMCL is referenced in the preamble as the "LIBRARY."  */
 
/*   The LLGPL is also available online at  */
/*   http://opensource.franz.com/preamble.html  */


/* Register usage.  This is certainly a little short of  */
/* immediate registers; we can maybe use the low bits  */
/* of mmx or xmm registers to hold immediate values and  */
/* do some unboxed arithmetic.   */


/*

	Register usage in C calling conventions differ between
	Darwin/Linux/FreeBSD (which use the AMD-defined ABI) and
	Windows64 (which uses something else).  The good news is that
	Win64 did away with the cdecl/stdcall/fastcall madness, there
	is only one ABI left.  Here's a rundown.

	AMD64^Wx86-64 ABI:
	 * Integer and pointer function arguments passed (from left to
	right) in RDI, RSI, RDX, RCX, R8 and R9
	 * FP arguments are passed in XMM0..XMM7
	 * rest is passed on stack
	 * return value in RAX
	 * Callee must preserve RBP, RBX, R12..R15, MXCSR control bits
	 * On function entry, x87 mode and DF clear is assumed
	 * [RSP]..[RSP-128] must not be touched by signal handlers

	Win64 ABI:
	 * Integer and pointers passed in RCX, RDX, R8, R9
	 * FP passed in XMM0..XMM3
	 * rest is passed on stack
	 * Return value in RAX or XMM0
	 * Caller (!) responsible for creating and cleaning stack space for
	spilling integer registers
	 * Callee must preserve RBP, RBX, RSI, RDI, R12..R15, XMM6..XMM15

	Both want their stack pointers to be 16 byte aligned on call,
	equivalent to 8 byte offset after call due to pushed return address.
	
	http://msdn2.microsoft.com/en-us/library/zthk2dkh(VS.80).aspx
	http://www.tortall.net/projects/yasm/manual/html/objfmt-win64-exception.html
	http://www.x86-64.org/documentation/abi.pdf


	Lisp register usage:

	Clozure CL renames the physical registers, giving them names
	based on their usage. An overview:

	imm0..imm2
	temp0..temp2
	save0..save3
	arg_x, arg_y, arg_z
	fn

	On top of that, further mappings are defined:

	fname, next_method_context: 	temp0
        nargs:				imm2
        ra0:				temp2
        xfn:				temp1
        allocptr:			temp0
        stack_temp:			mm7	
	
	x86-64 ABI mapping:
	
	imm0..imm2:		RAX, RDX, RCX
	temp0..temp2:		RBX, R9, R10
	save0..save3:		R15, R14, R12, R11
	arg_x, arg_y, arg_z:	R8, RDI, RSI
        fn:			R13
        rcontext_reg:		GS

	Win64 specifics:
        rcontext_reg:		R11
	
*/
	

/* Redefining these standard register names - with the same _l, _w, _b suffixes  */
/*  used in lispy symbolic names - allows us to play Stupid M4 Tricks in macros  */
			
define([rax_l],[eax])
define([rax_w],[ax])
define([rax_b],[al])
define([rbx_l],[ebx])
define([rbx_w],[bx])
define([rbx_b],[bl])
define([rcx_l],[ecx])
define([rcx_w],[cx])
define([rdx_l],[edx])
define([rdx_w],[dx])					
define([rdx_b],[dl])							
define([rsi_l],[esi])
define([rsi_w],[si])				
define([rsi_b],[sil])
define([rdi_l],[edo])
define([rdi_w],[di])				
define([rdi_b],[dil])
define([r8_l],[r8d])
define([r8_w],[r8w])					
define([r8_b],[r8b])							
define([r9_l],[r9d])
define([r9_w],[r9w])					
define([r9_b],[r9b])							
define([r10_l],[r10d])
define([r10_w],[r10w])					
define([r10_b],[r10b])							
define([r10_l],[r11d])
define([r11_w],[r11w])					
define([r11_b],[r11b])							
define([r12_l],[r12d])
define([r12_w],[r12w])					
define([r12_b],[r12b])							
define([r13_l],[r13d])
define([r13_w],[r13w])					
define([r13_b],[r13b])							
define([r14_l],[r14d])
define([r14_w],[r14w])					
define([r14_b],[r14b])							
define([r15_l],[r15d])
define([r15_w],[r15w])					
define([r15_b],[r15b])							

/* Registers when using Lisp calling conventions */
	
define([imm0],[rax]) 
	define([imm0_l],[eax])
	define([imm0_w],[ax])
	define([imm0_b],[al])
	define([Rimm0],[0])
	
define([temp0],[rbx])
	define([temp0_l],[ebx])
	define([temp0_w],[bx])
	define([temp0_b],[bl])
	define([Rtemp0],[3])

define([imm2],[rcx])
	define([imm2_l],[ecx])
	define([imm2_w],[cx])
	define([imm2_b],[cl])
	define([Rimm2],[1])
	
define([imm1],[rdx])
	define([imm1_l],[edx])
	define([imm1_w],[dx])
	define([imm1_b],[dl])
	define([Rimm1],[2])
	
define([arg_z],[rsi])
	define([arg_z_l],[esi])
	define([arg_z_w],[si])
	define([arg_z_b],[sil])
	define([Rarg_z],[6])

define([arg_y],[rdi])
	define([arg_y_l],[edi])
	define([arg_y_w],[di])
	define([arg_y_b],[dil])
	define([Rarg_y],[7])

define([arg_x],[r8])
	define([arg_x_l],[r8d])
	define([arg_x_w],[r8w])
	define([arg_x_b],[r8b])
	define([Rarg_x],[8])

define([temp1],[r9])
	define([temp1_l],[r9d])
	define([temp1_w],[r9w])
	define([temp1_b],[r9b])
	define([Rtemp1],[9])

define([temp2],[r10])
	define([temp2_l],[r10d])
	define([temp2_w],[r10w])
	define([temp2_x_b],[r10b])
	define([Rtemp2],[10])
	
define([save3],[r11])		
	define([save3_l],[r11d])
	define([save3_w],[r11w])
	define([save3_b],[r11b])
	define([Rsave3],[11])
	
define([save2],[r12])
	define([save2_l],[r12d])
	define([save2_w],[r12w])
	define([save2_b],[r12b])
	define([Rsave2],[12])
	
define([fn],[r13])		/* some addressing restrictions   */
	define([fn_l],[r13d])
	define([fn_w],[r13w])
	define([fn_b],[r13b])
	define([Rfn],[13])
	
define([save1],[r14])
	define([save1_l],[r14d])
	define([save1_w],[r14w])
	define([save1_b],[r14b])
	define([Rsave1],[14])
		
define([save0],[r15])
	define([save0_l],[r15d])
	define([save0_w],[r15w])
	define([save0_b],[r15b])
	define([Rsave0],[15])	


ifdef([WINDOWS],[
/* We keep the TCR pointer in r11 */
	define([rcontext_reg], r11)
	define([rcontext],[$1(%rcontext_reg)])
],[
/* The TCR can be accessed relative to %gs   */
	define([rcontext_reg],[gs])
	define([rcontext],[%rcontext_reg:$1])
])
define([fname],[temp0])
define([next_method_context],[temp0])
define([nargs_b],[imm2_b])	
define([nargs_w],[imm2_w])
define([nargs_q],[imm2])
define([nargs],[imm2_l])
define([ra0],[temp2])        
						
define([xfn],[temp1])

define([allocptr],[temp0])		
define([stack_temp],[mm7])

		
define([fp0],[xmm0])		
define([fp1],[xmm1])		
define([fp2],[xmm2])		
define([fp3],[xmm3])		
define([fp4],[xmm4])		
define([fp5],[xmm5])		
define([fp6],[xmm6])		
define([fp7],[xmm7])		
define([fp8],[xmm8])		
define([fp9],[xmm9])		
define([fp10],[xmm10])		
define([fp11],[xmm11])		
define([fp12],[xmm12])		
define([fp13],[xmm13])		
define([fp14],[xmm14])		
define([fp15],[xmm15])		
define([fpzero],[fp15])

/* Registers when running with native C calling conventions */

define([cret],[rax]) 
	define([cret_l],[eax])
	define([cret_w],[ax])
	define([cret_b],[al])
	define([Rcret],[0])
	
define([ctemp0],[r10])
	define([ctemp0_l],[r10d])
	define([ctemp0_w],[r10w])
	define([ctemp0_b],[r10b])
	define([Rctemp0],[10])
	
define([ctemp1],[r11])		
	define([ctemp1_l],[r11d])
	define([ctemp1_w],[r11w])
	define([ctemp1_b],[r11b])
	define([Rctemp1],[11])
	
define([csave0],[rbx])
	define([csave0_l],[ebx])
	define([csave0_w],[bx])
	define([csave0_b],[bl])
	define([Rcsave0],[3])

define([csave1],[r12])
	define([csave1_l],[r12d])
	define([csave1_w],[r12w])
	define([csave1_b],[r12b])
	define([Rcsave1],[12])
	
define([csave2],[r13])
	define([csave2_l],[r13d])
	define([csave2_w],[r13w])
	define([csave2_b],[r13b])
	define([Rcsave2],[13])
	
define([csave3],[r14])
	define([csave3_l],[r14d])
	define([csave3_w],[r14w])
	define([csave3_b],[r14b])
	define([Rcsave3],[14])
		
define([csave4],[r15])
	define([csave4_l],[r15d])
	define([csave4_w],[r15w])
	define([csave4_b],[r15b])
	define([Rcsave4],[15])	

ifdef([WINDOWS],[

define([carg0],[rcx])
	define([carg0_l],[ecx])
	define([carg0_w],[cx])
	define([carg0_b],[cl])
	define([Rcarg0],[1])
	
define([carg1],[rdx])
	define([carg1_l],[edx])
	define([carg1_w],[dx])
	define([carg1_b],[dl])
	define([Rcarg1],[2])
	
define([carg2],[r8])
	define([carg2_l],[r8d])
	define([carg2_w],[r8w])
	define([carg2_b],[r8b])
	define([Rcarg2],[8])

define([carg3],[r9])
	define([carg3_l],[r9d])
	define([carg3_w],[r9w])
	define([carg3_b],[r9b])
	define([Rcarg3],[9])

define([csave5],[rsi])
	define([csave5_l],[esi])
	define([csave5_w],[si])
	define([csave5_b],[sil])
	define([csave5_z],[6])

define([csave6],[rdi])
	define([csave6_l],[edi])
	define([csave6_w],[di])
	define([csave6_b],[dil])
	define([Rcsave6],[7])

],[
	
define([carg0],[rdi])
	define([carg0_l],[edi])
	define([carg0_w],[di])
	define([carg0_b],[dil])
	define([Rcarg0],[7])

define([carg1],[rsi])
	define([carg1_l],[esi])
	define([carg1_w],[si])
	define([carg1_b],[sil])
	define([carg1_z],[6])

define([carg2],[rdx])
	define([carg2_l],[edx])
	define([carg2_w],[dx])
	define([carg2_b],[dl])
	define([Rcarg2],[2])
	
define([carg3],[rcx])
	define([carg3_l],[ecx])
	define([carg3_w],[cx])
	define([carg3_b],[cl])
	define([Rcarg3],[1])
	
define([carg4],[r8])
	define([carg4_l],[r8d])
	define([carg4_w],[r8w])
	define([carg4_b],[r8b])
	define([Rcarg4],[8])

define([carg5],[r9])
	define([carg5_l],[r9d])
	define([carg5_w],[r9w])
	define([carg5_b],[r9b])
	define([Rcarg5],[9])	
])
	
nbits_in_word = 64
nbits_in_byte = 8
ntagbits = 4
nlisptagbits = 3
nfixnumtagbits = 3
nlowtagbits = 2        
num_subtag_bits = 8
subtag_shift = num_subtag_bits	
fixnumshift = 3
fixnum_shift = 3
fulltagmask = 15
tagmask = 7
fixnummask = 7
ncharcodebits = 8
charcode_shift = 8
word_shift = 3
node_size = 8
dnode_size = 16
dnode_align_bits = 4
dnode_shift = dnode_align_bits        
bitmap_shift = 6
        
fixnumone = (1<<fixnumshift)
fixnum_one = fixnumone
fixnum1 = fixnumone

nargregs = 3
nsaveregs = 4	
                

tag_fixnum = 0
tag_imm_0 = 1		/* subtag_single_float ONLY   */
tag_imm_1 = 2		/* subtag_character, internal markers   */
tag_list = 3		/* fulltag_cons or NIL   */
tag_tra = 4		/* tagged return_address   */
tag_misc = 5		/* random uvector   */
tag_symbol = 6	        /* non-null symbol   */
tag_function = 7	/* function entry point   */

tag_single_float = tag_imm_0
		
fulltag_even_fixnum = 0
fulltag_imm_0 = 1		/* subtag_single_float (ONLY)   */
fulltag_imm_1 = 2		/* subtag_character (mostly)   */
fulltag_cons = 3
fulltag_tra_0 = 4		/* tagged return address   */
fulltag_nodeheader_0 = 5
fulltag_nodeheader_1 = 6
fulltag_immheader_0 = 7	
fulltag_odd_fixnum = 8
fulltag_immheader_1 = 9
fulltag_immheader_2 = 10
fulltag_nil = 11
fulltag_tra_1 = 12
fulltag_misc = 13
fulltag_symbol = 14
fulltag_function = 15

define([define_subtag],[
subtag_$1 = ($2 | ($3 << ntagbits))
])
	

define_subtag(arrayH,fulltag_nodeheader_0,10)
define_subtag(vectorH,fulltag_nodeheader_1,10)
define_subtag(simple_vector,fulltag_nodeheader_1,11)
min_vector_subtag = subtag_vectorH
min_array_subtag = subtag_arrayH
        
	
ivector_class_64_bit = fulltag_immheader_2
ivector_class_32_bit = fulltag_immheader_1
ivector_class_other_bit = fulltag_immheader_0

define_subtag(fixnum_vector,ivector_class_64_bit,12)
define_subtag(s64_vector,ivector_class_64_bit,13)
define_subtag(u64_vector,ivector_class_64_bit,14)
define_subtag(double_float_vector,ivector_class_64_bit,15)

define_subtag(simple_base_string,ivector_class_32_bit,12)
define_subtag(s32_vector,ivector_class_32_bit,13)
define_subtag(u32_vector,ivector_class_32_bit,14)
define_subtag(single_float_vector,ivector_class_32_bit,15)
	
define_subtag(s16_vector,ivector_class_other_bit,10)
define_subtag(u16_vector,ivector_class_other_bit,11)
define_subtag(s8_vector,ivector_class_other_bit,13)
define_subtag(u8_vector,ivector_class_other_bit,14)
define_subtag(bit_vector,ivector_class_other_bit,15)


/* There's some room for expansion in non-array ivector space.   */
define_subtag(macptr,ivector_class_64_bit,1)
define_subtag(dead_macptr,ivector_class_64_bit,2)
define_subtag(bignum,ivector_class_32_bit,1)
define_subtag(double_float,ivector_class_32_bit,2)
define_subtag(xcode_vector,ivector_class_32_bit,3)

        
/* Note the difference between (e.g) fulltag_function - which  */
/* defines what the low 4 bytes of a function pointer look like -  */
/* and subtag_function - which describes what the subtag byte  */
/* in a function header looks like.  (Likewise for fulltag_symbol  */
/* and subtag_symbol)  */
		

define_subtag(symbol,fulltag_nodeheader_0,1)
define_subtag(catch_frame,fulltag_nodeheader_0,2)
define_subtag(hash_vector,fulltag_nodeheader_0,3)
define_subtag(pool,fulltag_nodeheader_0,4)
define_subtag(weak,fulltag_nodeheader_0,5)
define_subtag(package,fulltag_nodeheader_0,6)
define_subtag(slot_vector,fulltag_nodeheader_0,7)
define_subtag(basic_stream,fulltag_nodeheader_0,8)
define_subtag(function,fulltag_nodeheader_0,9)
	
define_subtag(ratio,fulltag_nodeheader_1,1)
define_subtag(complex,fulltag_nodeheader_1,2)
define_subtag(struct,fulltag_nodeheader_1,3)
define_subtag(istruct,fulltag_nodeheader_1,4)
define_subtag(value_cell,fulltag_nodeheader_1,5)
define_subtag(xfunction,fulltag_nodeheader_1,6)
define_subtag(lock,fulltag_nodeheader_1,7)
define_subtag(instance,fulltag_nodeheader_1,8)
	
			
nil_value = (0x13000+fulltag_nil)
t_value = (0x13020+fulltag_symbol)
misc_bias = fulltag_misc
cons_bias = fulltag_cons
define([t_offset],(t_value-nil_value))
	
misc_header_offset = -fulltag_misc
misc_data_offset = misc_header_offset+node_size /* first word of data    */
misc_subtag_offset = misc_header_offset       /* low byte of header   */
misc_dfloat_offset = misc_data_offset		/* double-floats are doubleword-aligned   */
function_header_offset = -fulltag_function
function_data_offset = function_header_offset+node_size	

define_subtag(single_float,fulltag_imm_0,0)


define_subtag(character,fulltag_imm_1,0)
                	
define_subtag(unbound,fulltag_imm_1,1)
unbound_marker = subtag_unbound
undefined = unbound_marker
define_subtag(slot_unbound,fulltag_imm_1,2)
slot_unbound_marker = subtag_slot_unbound
define_subtag(illegal,fulltag_imm_1,3)
illegal_marker = subtag_illegal
define_subtag(no_thread_local_binding,fulltag_imm_1,4)
no_thread_local_binding_marker = subtag_no_thread_local_binding
define_subtag(reserved_frame,fulltag_imm_1,5)
reserved_frame_marker = subtag_reserved_frame
define_subtag(function_boundary_marker,fulltag_imm_1,15)                        

	


	
/* The objects themselves look something like this:   */
	
/* Order of CAR and CDR doesn]t seem to matter much - there aren't   */
/* too many tricks to be played with predecrement/preincrement addressing.   */
/* Keep them in the confusing MCL 3.0 order, to avoid confusion.   */
	_struct(cons,-cons_bias)
	 _node(cdr)
	 _node(car)
	_ends
	
	_structf(ratio)
	 _node(numer)
	 _node(denom)
	_endstructf
	
	_structf(double_float)
	 _word(value)
         _word(val_low)
	_endstructf
	
	_structf(macptr)
	 _node(address)
         _node(domain)
         _node(type)
	_endstructf
	
/* Functions are of (conceptually) unlimited size.  */
	
	_struct(_function,-misc_bias)
	 _node(header)
	 _node(codevector)
	_ends

	_struct(tsp_frame,0)
	 _node(backlink)
	 _node(save_rbp)
	 _struct_label(fixed_overhead)
	 _struct_label(data_offset)
	_ends

	_struct(csp_frame,0)
	 _node(backlink)
	 _node(save_rbp)
	 _struct_label(fixed_overhead)
	 _struct_label(data_offset)
	_ends
        


	_structf(symbol,-fulltag_symbol)
	 _node(pname)
	 _node(vcell)
	 _node(fcell)
	 _node(package_predicate)
	 _node(flags)
         _node(plist)
         _node(binding_index)
	_endstructf

	_structf(catch_frame)
	 _node(catch_tag)	/* #<unbound> -> unwind-protect, else catch   */
	 _node(link)		/* backpointer to previous catch frame   */
	 _node(mvflag)		/* 0 if single-valued catch, fixnum 1 otherwise   */
	 _node(rsp)		/* saved lisp sp   */
	 _node(rbp)		/* saved lisp rbp   */
	 _node(foreign_sp)      /* necessary ?    */
	 _node(db_link)		/* head of special-binding chain   */
	 _node(_save3)
	 _node(_save2)
	 _node(_save1)
	 _node(_save0)
	 _node(xframe)		/* exception frame chain   */
	 _node(pc)		/* TRA of catch exit or cleanup form   */
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
        	
        
	_struct(c_frame,0)	/* PowerOpen ABI C stack frame   */
	 _node(backlink)
	 _node(crsave)
	 _node(savelr)
	 _field(unused, 16)
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

	/* For entry to variable-argument-list functions   */
	/* (e.g., via callback)   */
	_struct(varargs_eabi_c_frame,0)
	 _word(backlink)
	 _word(savelr)
	 _struct_label(va_list)
	 _word(flags)		/* gpr count byte, fpr count byte, padding   */
	 _word(overflow_arg_area)
	 _word(reg_save_area)
	 _field(padding,4)
	 _struct_label(regsave)
	 _field(gp_save,8*4)
	 _field(fp_save,8*8)
	 _word(old_backlink)
	 _word(old_savelr)
	 _struct_label(incoming_stack_args)
	_ends
        	
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


/* Nilreg-relative globals.  Talking the assembler into doing  */
/* something reasonable here  */
/* is surprisingly hard.   */

symbol_extra = symbol.size-fulltag_symbol

	
	_struct(nrs,0x13020)
	 _struct_pad(fulltag_symbol)
	 _struct_label(tsym)
	 _struct_pad(symbol_extra)	/* t    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(nil)
	 _struct_pad(symbol_extra)	/* nil    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(errdisp)
	 _struct_pad(symbol_extra)	/* %err-disp    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(cmain)
	 _struct_pad(symbol_extra)	/* cmain    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(eval)
	 _struct_pad(symbol_extra)	/* eval    */
 
	 _struct_pad(fulltag_symbol)
	 _struct_label(appevalfn)
	 _struct_pad(symbol_extra)	/* apply-evaluated-function    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(error)
	 _struct_pad(symbol_extra)	/* error    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(defun)
	 _struct_pad(symbol_extra)	/* %defun    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(defvar)
	 _struct_pad(symbol_extra)	/* %defvar    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(defconstant)
	 _struct_pad(symbol_extra)	/* %defconstant    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(macrosym)
	 _struct_pad(symbol_extra)	/* %macro    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(kernelrestart)
	 _struct_pad(symbol_extra)	/* %kernel-restart    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(package)
	 _struct_pad(symbol_extra)	/* *package*    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(total_bytes_freed)		/* *total-bytes-freed*   */
	 _struct_pad(symbol_extra)

	 _struct_pad(fulltag_symbol)
	 _struct_label(kallowotherkeys)
	 _struct_pad(symbol_extra)	/* allow-other-keys    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(toplcatch)
	 _struct_pad(symbol_extra)	/* %toplevel-catch%    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(toplfunc)
	 _struct_pad(symbol_extra)	/* %toplevel-function%    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(callbacks)
	 _struct_pad(symbol_extra)	/* %pascal-functions%    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(allmeteredfuns)
	 _struct_pad(symbol_extra)	/* *all-metered-functions*    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(total_gc_microseconds)		/* *total-gc-microseconds*   */
	 _struct_pad(symbol_extra)

	 _struct_pad(fulltag_symbol)
	 _struct_label(builtin_functions)		/* %builtin-functions%   */
	 _struct_pad(symbol_extra)                

	 _struct_pad(fulltag_symbol)
	 _struct_label(udf)
	 _struct_pad(symbol_extra)	/* %unbound-function%    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(init_misc)
	 _struct_pad(symbol_extra)	/* %init-misc   */

	 _struct_pad(fulltag_symbol)
	 _struct_label(macro_code)
	 _struct_pad(symbol_extra)	/* %macro-code%   */

	 _struct_pad(fulltag_symbol)
	 _struct_label(closure_code)
	 _struct_pad(symbol_extra)      /* %closure-code%   */

       	 _struct_pad(fulltag_symbol)
	 _struct_label(new_gcable_ptr) /* %new-gcable-ptr   */
	 _struct_pad(symbol_extra)
	
       	 _struct_pad(fulltag_symbol)
	 _struct_label(gc_event_status_bits)
	 _struct_pad(symbol_extra)	/* *gc-event-status-bits*    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(post_gc_hook)
	 _struct_pad(symbol_extra)	/* *post-gc-hook*    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(handlers)
	 _struct_pad(symbol_extra)	/* %handlers%    */


	 _struct_pad(fulltag_symbol)
	 _struct_label(all_packages)
	 _struct_pad(symbol_extra)	/* %all-packages%    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(keyword_package)
	 _struct_pad(symbol_extra)	/* *keyword-package*    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(finalization_alist)
	 _struct_pad(symbol_extra)	/* %finalization-alist%    */

	 _struct_pad(fulltag_symbol)
	 _struct_label(foreign_thread_control)
	 _struct_pad(symbol_extra)	/* %foreign-thread-control    */

	_ends

define([def_header],[
$1 = ($2<<num_subtag_bits)|$3])

	def_header(double_float_header,2,subtag_double_float)
	def_header(two_digit_bignum_header,2,subtag_bignum)
	def_header(three_digit_bignum_header,3,subtag_bignum)
	def_header(four_digit_bignum_header,4,subtag_bignum)
	def_header(five_digit_bignum_header,5,subtag_bignum)        
	def_header(symbol_header,symbol.element_count,subtag_symbol)
	def_header(value_cell_header,1,subtag_value_cell	)
	def_header(macptr_header,macptr.element_count,subtag_macptr)
	def_header(vectorH_header,vectorH.element_count,subtag_vectorH)

	include(errors.s)

/* Symbol bits that we care about  */
	
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



TCR_BIAS = 0
		
/*  Thread context record.  */

	_struct(tcr,TCR_BIAS)
	 _node(prev)		/* in doubly-linked list   */
	 _node(next)		/* in doubly-linked list   */
         _node(single_float_convert)
	 _node(linear)		/* our linear (non-segment-based) address.   */
         _node(save_rbp)        /* lisp RBP when in foreign code    */
	 _word(lisp_mxcsr)
	 _word(foreign_mxcsr)	
	 _node(db_link)		/* special binding chain head   */
	 _node(catch_top)	/* top catch frame   */
	 _node(save_vsp)	/* VSP when in foreign code   */
	 _node(save_tsp)	/* TSP when in foreign code   */
	 _node(foreign_sp)	/* Saved foreign SP when in lisp code   */
	 _node(cs_area)		/* cstack area pointer   */
	 _node(vs_area)		/* vstack area pointer   */
	 _node(ts_area)		/* tstack area pointer   */
	 _node(cs_limit)	/* cstack overflow limit   */
	 _word(bytes_consed_low)
	 _word(bytes_consed_high)
	 _node(log2_allocation_quantum)
	 _node(interrupt_pending)
	 _node(xframe)		/* per-thread exception frame list   */
	 _node(errno_loc)	/* per-thread  errno location   */
	 _node(ffi_exception)	/* mxcsr exception bits from ff-call   */
	 _node(osid)		/* OS thread id   */
         _node(valence)		/* odd when in foreign code 	  */
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
	 _node(suspend)		/* semaphore for suspension notify   */
	 _node(resume)		/* sempahore for resumption notify   */
	 _node(flags)      
	 _node(gc_context)
         _node(termination_semaphore)
         _node(unwinding)
         _node(tlb_limit)
         _node(tlb_pointer)     /* Consider using tcr+N as tlb_pointer   */
	 _node(shutdown_count)
         _node(next_tsp)
         _node(safe_ref_address)
	_ends

        _struct(win64_context,0)
         _field(P1Home, 8)
         _field(P2Home, 8)
         _field(P3Home, 8)
         _field(P4Home, 8)
         _field(P5Home, 8)
         _field(P6Home, 8)
         _field(ContextFlags, 4)
         _field(MxCsr, 4)
         _field(SegCs, 2)
         _field(SegDs, 2)
         _field(SegEs, 2)
         _field(SegFs, 2)
         _field(SegGs, 2)
         _field(SegSs, 2)
         _field(EFlags, 4)
         _field(Dr0, 8)
         _field(Dr1, 8)
         _field(Dr2, 8)
         _field(Dr3, 8)
         _field(Dr6, 8)
         _field(Dr7, 8)
         _field(Rax, 8)
         _field(Rcx, 8)
         _field(Rdx, 8)
         _field(Rbx, 8)
         _field(Rsp, 8)
         _field(Rbp, 8)
         _field(Rsi, 8)
         _field(Rdi, 8)
         _field(R8, 8)
         _field(R9, 8)
         _field(R10, 8)
         _field(R11, 8)
         _field(R12, 8)
         _field(R13, 8)
         _field(R14, 8)
         _field(R15, 8)
         _field(Rip, 8)
         _struct_label(fpstate)
         _field(Header, 32)
         _field(Legacy, 128)
         _field(Xmm0, 16)
         _field(Xmm1, 16)        
         _field(Xmm2, 16)        
         _field(Xmm3, 16)        
         _field(Xmm4, 16)        
         _field(Xmm5, 16)        
         _field(Xmm6, 16)        
         _field(Xmm7, 16)        
         _field(Xmm8, 16)        
         _field(Xmm9, 16)        
         _field(Xmm10, 16)        
         _field(Xmm11, 16)        
         _field(Xmm12, 16)        
         _field(Xmm13, 16)        
         _field(Xmm14, 16)        
         _field(Xmm15, 16)
         _field(__pad, 96)
         _field(VectorRegister, 416)
         _field(VectorControl, 8)
         _field(DebugControl, 8)
         _field(LastBranchToRip, 8)
         _field(LastBranchFromRip, 8)
         _field(LastExceptionToRip, 8)
         _field(LastExceptionFromRip, 8)
 _ends

	
TCR_FLAG_BIT_FOREIGN = fixnum_shift
TCR_FLAG_BIT_AWAITING_PRESET = (fixnum_shift+1)	
TCR_FLAG_BIT_ALT_SUSPEND = (fixnumshift+2)
TCR_FLAG_BIT_PROPAGATE_EXCEPTION = (fixnumshift+3)
TCR_FLAG_BIT_SUSPEND_ACK_PENDING = (fixnumshift+4)
TCR_FLAG_BIT_PENDING_EXCEPTION = (fixnumshift+5)
TCR_FLAG_BIT_FOREIGN_EXCEPTION = (fixnumshift+6)
TCR_FLAG_BIT_PENDING_SUSPEND = (fixnumshift+7)        
	
target_most_positive_fixnum = 1152921504606846975
target_most_negative_fixnum = -1152921504606846976


lisp_globals_limit = 0x13000
        
INTERRUPT_LEVEL_BINDING_INDEX = fixnumone

c_stack_16_byte_aligned = 1
        	
		        
                
