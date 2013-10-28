/*   Copyright (C) 2012 Clozure Associates */
/*   This file is part of Clozure CL.  */

/*   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public */
/*   License , known as the LLGPL and distributed with Clozure CL as the */
/*   file "LICENSE".  The LLGPL consists of a preamble and the LGPL, */
/*   which is distributed with Clozure CL as the file "LGPL".  Where these */
/*   conflict, the preamble takes precedence.   */

/*   Clozure CL is referenced in the preamble as the "LIBRARY." */

/*   The LLGPL is also available online at */
/*   http://opensource.franz.com/preamble.html */

define(`gprval',`substr($1,1)')
define(`gpr32',``w'gprval($1)')
define(`gpr64',``x'gprval($1)')                        

/* dnode_align(dest,src,delta) */
        define(`dnode_align',`
        __(add $1,$2,#$3+(dnode_size-1))
        __(bic $1,$1,#((1<<dnode_align_bits)-1))
')

define(`make_header',`
        __(mov $1,#($3<<tag_shift))
        __(add $1,$1,#$2)
        ')
        
/* Load a 16-bit constant into $1 */
define(`movc16',`
        __(mov $1,#$2)
        ')

define(`_clrex',`
        __(clrex)
        ')        

define(`extract_tag',`
        __(lsr $2,$1,#tag_shift)
        ')

/* Set $1 to $2, with bit 55 sign-sextended into bits 56-63.  If
   $2 is a fixnum, $1 and $2 will be =. */        
define(`sign_extend_value',`
        __(sbfx $2,$1,#0,#56)
        ')
        
/* Set $2 to 0 iff $1 is a fixnum, to an arbitrary bit pattern otherwise. */
define(`test_fixnum',`
        __(sign_extend_value($2,$1))
        __(eor $2,$1,$2)
        ')
        
                        
define(`branch_if_not_fixnum',`
        __(test_fixnum($3,$1))
        __(cbnz $3,$2)
        ')

define(`branch_if_fixnum',`
        __(test_fixnum($3,$1))
        __(cbz $3,$2)
        ')

define(`branch_if_list',`
        __(clz $3,$1)
        __(sub $3,$3,#list_leading_zero_bits)
        __(cbz $3,$2)
        ')

define(`branch_if_not_list',`
        __(clz $3,$1)
        __(sub $3,$3,#list_leading_zero_bits)
        __(cbz $3,$2)
        ')
                
                
define(`branch_if_negative',`
        __(tbnz $1,#63,$2)
        ')
        
define(`lisp_boolean',`
        __(csel $1,rt,rnil,$2)
        ')
                
define(`test_two_fixnums',`
        __(test_fixnum($3,$1))
        __(test_fixnum($4,$2))
        __(orr $3,$3,$4)
        ')
        	

define(`extract_header',`
	__(ldr $1,[$2,#misc_header_offset])
	')


define(`unbox_character',`
        __(clear_tag($1,$2))
        ')
                

define(`push1',`
        __(str $1,[$2,#-node_size]!)
	')
	
define(`pop1',`
        __(ldr $1,[$2],#node_size)
	')
	
define(`vpush1',`
	__(push1($1,vsp))
	')
	
define(`vpop1',`
	__(pop1($1,vsp))
	')
	
		
define(`unlink',`
	__(ldr($1,0($1)))
 ')

	
define(`set_nargs',`
	__(mov nargs,#($1<<3))
	')
	

	

define(`vref32',`
        __(ldr gpr32($1),[$2,#$2<<2])
	')
        
	
define(`vrefr',`
        __(vref32($1,$2,$3))
	')


        
                	
define(`getvheader',`
	__(ldr $1,[$2,#vector.header])
	')
	
	
	/* "Length" is fixnum element count */
define(`header_length',`
        __(ubfx $1,$2,#0,#56)
        ')

define(`header_size',`
        __(header_length($1,$2))
        ')
        

define(`vector_length',`
	__(getvheader($3,$2))
	__(header_length($1,$3))
	')

	
define(`ref_global',`
        __(mov $1,#lisp_globals.$2)
	__(ldr $1,[rnil,$1])
')


define(`ref_nrs_value',`
	__(ldr $1,[rnil,#((nrs.$2)+(symbol.vcell))])
')

define(`ref_nrs_function',`
	__(ldr $1,[rnil,#((nrs.$2)+(symbol.fcell))])
')
        
define(`ref_nrs_symbol',`
        __(add $1,rnil,#$2)
        ')
	


	/* vpop argregs - nargs is known to be non-zero */
define(`vpop_argregs_nz',`
        new_macro_labels()
        __(cmp nargs,#node_size*2)
        __(vpop1(arg_z))
        __(blo macro_label(done))
        __(vpop1(arg_y))
        __(beq macro_label(done))
        __(vpop1(arg_x))
macro_label(done):              
        ')

define(`vpop_argregs',`
        new_macro_labels()
        __(cbz nargs,macro_label(done))
        __(vpop_argregs_nz())
macro_label(done):      
        ')
        
define(`vpush_argregs_nz',`
        new_macro_labels()
        __(cmp nargs,#2<<fixnumshift)
        __(bls macro_label(notx))
        __(vpush1(arg_x))
macro_label(notx):      
        __(bne macro_label(justz))
        __(vpush1(arg_y))
macro_label(justz):
        __(vpush1(arg_z))
        ')
        
define(`vpush_argregs',`
	new_macro_labels()
        __(cbz nargs,macro_label(done))
        __(vpush_argregs_nz())
macro_label(done):
')

define(`vpush_all_argregs',`
        __(vpush1(arg_x))
        __(vpush1(arg_y))
        __(vpush1(arg_z))
        ')

define(`vpop_all_argregs',`
        __(vpop1(arg_z))
        __(vpop1(arg_y))
        __(vpop1(arg_x))
        ')
                        
                

/* $1 = value for lisp_frame.savevsp */                
define(`build_lisp_frame',`
        __(stp ifelse($1,`',vsp,$1),lr,[sp,#-(2*node_size)]!)
')

define(`restore_lisp_frame',`
        __(ldp vsp,lr,[sp],#2*node_size)
        ')

define(`return_lisp_frame',`
        __(restore_lisp_frame())
        __(ret)
        ')
        
define(`discard_lisp_frame',`
	__(add sp,sp,#lisp_frame.size)
	')
	
	
define(`_car',`
        __(ldr $1,[$2,#cons.car])
')
	
define(`_cdr',`
        __(ldr $1,[$2,#cons.cdr])
	')
	
define(`_rplaca',`
        __(str $2,[$1,#cons.car])
	')
	
define(`_rplacd',`
        __(str $2,[$1,#cons.cdr])
	')



define(`trap_unless_list',`
        new_macro_labels()
        __(clz $2,$1)
        __(cmp $2,#list_leading_zero_bits)
        __(beq local_label(ok))
        __(uuo_error_reg_not_lisptag($1,tag_list))
')

define(`trap_unless_fixnum',`
        __(new_macro_labels())
        __(branch_if_fixnum($1,macro_label(ok),$2))
        __(uuo_error_reg_not_lisptag($1,tag_fixnum))
macro_label(ok):        
        ')
                
        
/* "jump" to the code-vector of the function in nfn. */
define(`jump_nfn',`
        __(br nfn)
')

/* "call the  function in nfn. */
define(`call_nfn',`
        __(blr nfn)
')
	

/* "jump" to the function in fnames function cell. */
define(`jump_fname',`
	__(ldr nfn,[fname,#symbol.fcell])
	__(jump_nfn())
')

/* call the function in fnames function cell. */
define(`call_fname',`
	__(ldr nfn,[fname,#symbol.fcell])
	__(call_nfn())
')

define(`funcall_nfn',`
        new_macro_labels()
        __(extract_tag(imm0,nfn))
        __(cmp imm0,#tag_function)
        __(beq local_label(go))
        __(cmp imm0,#tag_symbol)
        __(beq macro_label(symbol))
        __(uuo_error_not_callable(nfn))
macro_label(symbol):            
        __(mov fname,nfn)
        __(ldr nfn,[fname,#symbol.fcell])
macro_label(go):      
        __(jump_nfn())

')

/* Save the non-volatile FPRs (d8-d15) in a stack-allocated vector.
*/        
   
define(`push_foreign_fprs',`
        __(make_header($1,9,double_float_vector_header))
        __(stp $1,xzr,[sp,#-10<<3]!)
        __(stp d8,d9,[sp,#16])
        __(stp d10,d11,[sp,#32])
        __(stp d12,d14,[sp,#48])
        __(stp d14,d15,[sp,#64])
')

/* Save the lisp non-volatile FPRs. These are exactly the same as the foreign
   FPRs. */
define(`push_lisp_fprs',`
        __(make_header($1,9,double_float_vector_header))
        __(stp $1,xzr,[sp,#-10<<3]!)
        __(stp d8,d9,[sp,#16])
        __(stp d10,d11,[sp,#32])
        __(stp d12,d14,[sp,#48])
        __(stp d14,d15,[sp,#64])
')
        
/* Pop the non-volatile FPRs (d8-d15) from the stack-consed vector
   on top of the stack and discard the vector. */
define(`pop_foreign_fprs',`
        __(ldp d8,d9,[sp,#16])
        __(ldp d10,d11,[sp,#32])
        __(ldp d12,d14,[sp,#48])
        __(ldp d14,d15,[sp,#64])
        __(add sp,sp,#10<<3)
')

/* Pop the lisp non-volatile FPRs */        
define(`pop_lisp_fprs',`
        __(ldp d8,d9,[sp,#16])
        __(ldp d10,d11,[sp,#32])
        __(ldp d12,d14,[sp,#48])
        __(ldp d14,d15,[sp,#64])
        __(add sp,sp,#10<<3)
')

/* Reload the non-volatile lisp FPRs (d8-d15) from the stack-consed vector
   on top of the stack, leaving the vector in place.   */
define(`restore_lisp_fprs',`
        __(ldp d8,d9,[sp,#16])
        __(ldp d10,d11,[sp,#32])
        __(ldp d12,d14,[sp,#48])
        __(ldp d14,d15,[sp,#64])
')                

/* discard the stack-consed vector which contains a set of 8 non-volatile
   FPRs. */
define(`discard_lisp_fprs',`
        __(add sp,sp,#10*8)
')                        
        
define(`mkcatch',`
        new_macro_labels()
        __(push_lisp_fprs(imm0))
	__(build_lisp_frame())
        __(make_header(imm1,catch_frame.element_count,catch_frame_header))
        __(mov imm2,#catch_frame.element_count<<word_shift)
        __(dnode_align(imm2,imm2,node_size))
        __(stack_allocate_zeroed_vector(imm0,imm1,imm2,#tag_catch_frame))
        __(ldr temp1,[rcontext,#tcr.last_lisp_frame])
	__(ldr imm1,[rcontext,#tcr.catch_top])
        /* imm2 is mvflag */
        /* arg_z is tag */
        __(ldr arg_x,[rcontext,#tcr.db_link])
        __(ldr temp0,[rcontext,#tcr.xframe])
        __(str arg_z,[imm0,#catch_frame.catch_tag])
        __(str imm1,[imm0,#catch_frame.link])
        __(str imm2,[imm0,#catch_frame.mvflag])
        __(str arg_x,[imm0,#catch_frame.db_link])
        __(str temp0,[imm0,#catch_frame.xframe])
        __(stp save0,save1,[imm0,#catch_frame.save0])
        __(stp save2,save3,[sp,#catch_frame.save2])
        __(stp save4,save5,[sp,#catch_frame.save4])
        __(stp save6,save7,[sp,#catch_frame/save6])
        __(str imm0,[rcontext,#tcr.catch_top])
        __(add lr,lr,#4)
')	




define(`stack_align',`((($1)+STACK_ALIGN_MASK)&~STACK_ALIGN_MASK)')

define(`clear_alloc_tag',`
        __(bic allocptr,allocptr,#dnode_mask)
')

define(`clear_header_element_count',`
        __(ubfm $1,$2,#56,#63)
        ')

define(`clear_tag',`
        __(ubfm $1,$2,#0,#55)
        ')
        
        
define(`Cons',`
       	new_macro_labels()
        __(sub allocptr,allocptr,#cons.size-node_size)
        __(cmp allocptr,allocbase)
        __(bhi macro_label(ok))
        __(uuo_alloc_trap())
macro_label(ok):                
        __(str $3,[allocptr,#cons.cdr])
        __(str $2,[allocptr,#cons.car])
        __(orr $1,allocptr,#tag_cons<<tag_shift)
	__(clear_alloc_tag())
')


/* This is probably only used once or twice in the entire kernel, but */
/* I wanted a place to describe the constraints on the mechanism. */

/* Those constaints are (not surprisingly) similar to those which apply */
/* to cons cells, except for the fact that the header (and any length */
/* field that might describe large arrays) has to have been stored in */
/* the object if the trap has succeeded on entry to the GC.  It follows */
/* that storing the register containing the header must immediately */
/* follow the allocation trap (and an auxiliary length register must */
/* be stored immediately after the header.)  Successfully falling */
/* through the trap must emulate any header initialization: it would */
/* be a bad idea to have allocptr pointing to a zero header ... */



/* Parameters: 

 $1 = dest reg 
 $2 = header
 $3 = register containing size in bytes.  (We're going to subtract 
 node_size from this; do it in the macro body, rather than force the
 (1 ?) caller to do it. 
 $4 = tag byte
 */


define(`Misc_Alloc',`
        new_macro_labels()
	__(sub $3,$3,#node_size)
	__(sub allocptr,allocptr,$3)
        __(cmp allocptr,allocbase)
        __(bhi macro_label(ok))
        __(uuo_alloc_trap())
macro_label(ok):                
	__(str $2,[allocptr,#misc_header_offset])
        __(orr $1,allocptr,$4,lsl #tag_shift)
	__(clear_alloc_tag())
')

/*  Parameters $1, $2, $4 as above; $3 = physical size constant. */
define(`Misc_Alloc_Fixed',`
        new_macro_labels()
        __(sub allocptr,allocptr,#$3-node_size)
        __(cmp allocptr,allocbase)
        __(bhi macro_label(ok))
        __(uuo_alloc_trap())
macro_label(ok):                
	__(str $2,[allocptr,#misc_header_offset])
        __(orr $1,allocptr,$4,lsl #tag_shift)
	__(clear_alloc_tag())
')

/* Stack-allocate an ivector; $1 = header_lengthder, $2 = dnode-aligned
   size in bytes. */
define(`stack_allocate_ivector',`
        __(str $1,[sp,-$2]!)
        ')

        

                        
                        
/* Stack-allocate a uvector (other than a smallish ivector) and return
   a tagged pointer to it in $1.
   $2 = header, $3 = dnode-aligned size in bytes, $4 = tag).  
   Both $2 and $3 are modified here. 
   We need to make sure that the right thing happens if we're
   interrupted in the middle of the zeroing loop. */
define(`stack_allocate_zeroed_vector',`
       new_macro_labels()
macro_label(loop):              
        __(subs $3,$3,#dnode_size)
        __(str vzero,[sp,#-dnode_size]!)
        __(bne macro_label(loop))
        __(str $2,[sp,#0])
        __(add $1,sp,#node_size)
        __(orr $1,$1,$4)
        ')
   

define(`check_enabled_pending_interrupt',`
        __(ldr $1,[rcontext,#tcr.interrupt_pending])
        __(cmp $1,0)
        __(ble $2)
        __(uuo_interrupt_now())
        ')
        
define(`check_pending_interrupt',`
	new_macro_labels()
        __(ldr $1,[rcontext,#tcr.tlb_pointer])
	__(ldr $1,[$1,$INTERRUPT_LEVEL_BINDING_INDEX])
        __(cmp $1,#0)
        __(blt macro_label(done))
        __(check_enabled_pending_interrupt($1,macro_label(done)))
macro_label(done):
')

/* $1 = ndigits.  Assumes 4-byte digits */        
define(`aligned_bignum_size',`((~(dnode_size-1)&(node_size+(dnode_size-1)+(4*$1))))')

define(`suspend_now',`
	__(uuo_suspend_now())
')

/* $3 points to a uvector header.  Set $1 to the first dnode-aligned address */
/* beyond the uvector, using imm regs $1,$2,$4, and $5 as temporaries. */
define(`skip_stack_vector',`
        new_macro_labels()
        __(ldr $1,[$3])
        __(extract_tag($2,$1))
        __(header_size($1,$1))
        __(cmp $2,#min_64_bit_ivector_header)
        __(bge local_label(bits64))
        __(cmp $2,#min_32_bit_ivector_header)
        __(bge local_label(bits32))
        __(cmp $2,#min_16_bit_ivector_header)
        __(bge local_label(bits16))
        __(cmp $2,#min_8_bit_ivector_header)
        __(bge local_label(bytes))
        __(add $1,$1,#7)
        __(lsr $1,$1,#3)
        __(b local_label(bytes))
local_label(bits64):    
        __(lsl $1,$1,#1)
local_label(bits32):
        __(lsl $1,$1,#1)
local_label(bits16):    
        __(lsl $1,$1,#1)        
macro_label(bytes):
        __(dnode_align($1,$1,node_size))
        __(add $1,$1,$3)
        ')

/* This may need to be inlined.  $1=link, $2=saved sym idx, $3 = tlb, $4 = value */
define(`do_unbind_to',`
        __(ldr $1,[rcontext,#tcr.db_link])
        __(ldr $3,[rcontext,#tcr.tlb_pointer])
1:      __(ldr $2,[$1,#binding.sym])
        __(ldr $4,[$1,#binding.val])
        __(ldr $1,[$1,#binding.link])
        __(cmp imm0,$1)
        __(str $4,[$3,$2])
        __(bne 1b)
        __(str $1,[rcontext,#tcr.db_link])
        ')                

