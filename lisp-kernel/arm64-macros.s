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

define(`make_header',`(($1<<num_subtag_bits)|($2&subtag_mask))')
        
/* Load a 16-bit constant into $1 */
define(`movc16',`
        __(mov $1,#$2)
        ')

define(`_clrex',`
        __(clrex)
        ')        

        
define(`branch_if_not_fixnum',`
        __(tbnz gpr32($1),#fixnum_clr_bit,$2)
        ')

define(`branch_if_fixnum',`
        __(tbz gpr32($1),#fixnum_clr_bit,$2)
        ')

define(`branch_if_list',`
        __(tbnz gpr32($1),#list_set_bit,$2)
        ')

define(`branch_if_not_list',`
        __(tbz gpr32($1),#list_set_bit,$2)
        ')                        
                
define(`branch_if_negative',`
        __(tbnz $1,#63,$2)
        ')
        
define(`lisp_boolean',`
        __(csel $1,rt,rnil,$2)
        ')
                
define(`test_two_fixnums',`
        __(orr $3,$1,$2)
        __(test_fixnum($3))
        ')
        	
define(`extract_fulltag',`
        __(and $1,$2,#fulltagmask)
        ')

define(`extract_lisptag',`
        __(and $1,$2,#tagmask)
        ')

define(`extract_lisptag_',`
        __(ands $1,$1,#tagmask)
        ')

define(`extract_subtag',`
        __(ldrb $1,[$2,#misc_subtag_offset])
	')

                               
define(`extract_lowbyte',`
        __(and $1,$2,#((1<<num_subtag_bits)-1))
        ')

define(`extract_header',`
	__(ldr $1,[$2,#misc_header_offset])
	')

define(`extract_typecode',`
       	new_macro_labels()
        __(extract_lisptag($1,$2))
        __(cmp $1,#tag_misc)
        __(b.ne macro_label(not_misc))
        __(ldrb $1,[$2,#misc_subtag_offset])
macro_label(not_misc):          
        ')

define(`box_fixnum',`
        __(lsl $1,$2, #fixnumshift)
	')

define(`unbox_fixnum',`	
	__(asr $1,$2, #fixnumshift)
	')

define(`unbox_character',`
        __(lsr $1,$2, #charcode_shift)
        ')
                
define(`loaddf',`
	__(lfd $1,dfloat.value($2))')
	
define(`storedf',`
	__(stfd $1,dfloat.value($2))
	')

define(`push1',`
        __(str $1,[$2,#-node_size]!)
	')
	
	/* Generally not a great idea. */
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
	__(mov nargs,#($1)<<fixnumshift)
	')
	
define(`bitclr',`
	__(rlwinm $1,$2,0,0x1f&((31-($3))+1),0x1f&((31-($3))-1))
	')
	

define(`vref32',`
        __(ldr $1,[$2,#misc_data_offset+(($3)<<2)])
	')
        
define(`vref16',`/* dest,src,n*/
	__(lhz $1,misc_data_offset+(($3)<<1)($2))
	')
	
define(`vrefr',`
        __(vref32($1,$2,$3))
	')


        
                	
define(`getvheader',`
	__(ldr $1,[$2,#vector.header])
	')
	
	
	/* "Length" is fixnum element count */
define(`header_length',`
        __(bic $1,$2,#subtag_mask)
        __(mov $1,$1,lsr #num_subtag_bits-fixnumshift)
        ')



define(`vector_length',`
	__(getvheader($3,$2))
	__(header_length($1,$3))
	')

	
define(`ref_global',`
        __(mov ifelse($3,`',$1,$3),#nil_value)
	__(ldr $1,[ifelse($3,`',$1,$3),#lisp_globals.$2])
')


define(`ref_nrs_value',`
        __(mov $1,#nil_value)
	__(ldr $1,[$1,#((nrs.$2)+(symbol.vcell))])
')

define(`ref_nrs_function',`
        __(mov $1,#nil_value)
	__(ldr $1,[$1,#((nrs.$2)+(symbol.fcell))])
')
        
define(`ref_nrs_symbol',`
        __(movc16($3,nrs.$2))
        __(add $1,$3,#nil_value)
        ')
	
define(`set_nrs_value',`
	__(str($1,((nrs.$2)+(symbol.vcell))(0)))
')


	/* vpop argregs - nargs is known to be non-zero */
define(`vpop_argregs_nz',`
        __(cmp nargs,#node_size*2)
        __(vpop1(arg_z))
        __(ldrhs arg_y,[vsp],#node_size)
        __(ldrhi arg_x,[vsp],#node_size)
        ')

                
	/* vpush argregs */
define(`vpush_argregs',`
	new_macro_labels()
        __(cmp nargs,#0)
        __(beq macro_label(done))
        __(cmp nargs,#node_size*2)
        __(strhi arg_x,[vsp,#-node_size]!)
        __(strhs arg_y,[vsp,#-node_size]!)
        __(str arg_z,[vsp,#-node_size]!)
macro_label(done):
')

define(`vpush_all_argregs',`
        __(stmdb vsp!,{arg_z,arg_y,arg_x})
        ')

define(`vpop_all_argregs',`
        __(ldmia vsp!,{arg_z,arg_y,arg_x})
        ')
                        
                

/* $1 = value for lisp_frame.savevsp */                
define(`build_lisp_frame',`
        __(stp ifelse($1,`',vsp,$1),lr,[sp,#-(2*node_size)]!)
')

/* This has the odd side effect of loading lisp_frame_marker into
   the arg/temp/imm reg $1.  I think that that's probably better
   than adjusting sp and loading the other regs ; it'd be good
   to say (at interrupt time) that there's either a lisp frame
   on the stack or there isn't. */
define(`restore_lisp_frame',`
        __(ldm sp!,{$1,vsp,fn,lr})
        ')

define(`return_lisp_frame',`
        __(ldm sp!,{$1,vsp,fn,pc})
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


define(`trap_unless_lisptag_equal',`
       	new_macro_labels()
	__(extract_lisptag($3,$1))
        __(cmp $3,#$2)
        __(beq macro_label(ok))
	__(uuo_error_reg_not_lisptag(al,$3,$2))
macro_label(ok):                
')

define(`trap_unless_list',`
       	new_macro_labels()
        __(tbnz $1,#0,macro_label(ok))
        __(uuo_error_reg_not_tag($1,tag_list))
macro_label(ok):        
')

define(`trap_unless_fixnum',`
        __(new_macro_labels())
        __(test_fixnum($1))
        __(beq macro_label(ok))
        __(uuo_error_reg_not_lisptag(al,$1,tag_fixnum))
macro_label(ok):        
        ')
                
define(`trap_unless_fulltag_equal',`
        new_macro_labels()
	__(extract_fulltag($3,$1))
        __(cmp $3,#$2)
        __(beq macro_label(ok))
        __(uuo_error_reg_not_fulltag(al,$1,$2))
macro_label(ok):        
')
	
define(`trap_unless_typecode_equal',`
        new_macro_labels()
        __(extract_typecode($3,$1))
        __(cmp $3,#$2)
        __(beq macro_label(ok))
        __(uuo_error_reg_not_xtype(al,$2))
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
        __(extract_lisptag(imm0,nfn))
        __(cmp imm0,#tag_callable)
        __(b.ne macro_label(not_callable))
        __(tbnz nfn,#callable_function_bit,macro_label(go))
        __(mov fname,nfn)
        __(ldr nfn,[fname,#symbol.fcell])
macro_label(go):      
        __(jump_nfn())
macro_label(not_callable):              
        __(uuo_error_not_callable(nfn))

')

/* Save the non-volatile FPRs (d8-d15) in a stack-allocated vector.
   Clobber d7.  Note that d7/s14 wind up looking like denormalized
   floats (we effectively load a vector header into d7.)
*/        
   
define(`push_foreign_fprs',`
        __(b macro_label(next))
        .align 3
macro_label(data):      
        .long make_header(8,subtag_double_float_vector)
        .long 0
macro_label(next):
        __(fldd d7,[pc,#-16])
        __(fstmfdd sp!,{d7-d15})
')

/* Save the lisp non-volatile FPRs. These are exactly the same as the foreign
   FPRs. */
define(`push_lisp_fprs',`
        new_macro_labels()
        __(b macro_label(next))
macro_label(data):      
        .long make_header(8,subtag_double_float_vector)
macro_label(next):
        __(flds single_float_zero,[pc,#-12])
        __(fstmfdd sp!,{d7-d15})
        __(fcpys single_float_zero,s15)
')
        
/* Pop the non-volatile FPRs (d8-d15) from the stack-consed vector
   on top of the stack.  This loads the vector header
   into d7 as a side-effect. */
define(`pop_foreign_fprs',`
        __(fldmfdd sp!,{d7-d15})
')

/* Pop the lisp non-volatile FPRs */        
define(`pop_lisp_fprs',`
        __(fldmfdd sp!,{d7-d15})
        __(fcpys single_float_zero,s15)
')

/* Reload the non-volatile lisp FPRs (d8-d15) from the stack-consed vector
   on top of the stack, leaving the vector in place.  d7 winds up with
   a denormalized float in it, if anything cares. */
define(`restore_lisp_fprs',`
        __(fldmfdd $1,{d7-d15})
        __(fcpys single_float_zero,s15)
')                

/* discard the stack-consed vector which contains a set of 8 non-volatile
   FPRs. */
define(`discard_lisp_fprs',`
        __(add sp,sp,#9*8)
')                        
        
define(`mkcatch',`
        new_macro_labels()
        __(push_lisp_fprs())
	__(build_lisp_frame(imm0))
        __(movc16(imm0,make_header(catch_frame.element_count,subtag_u32_vector)))
        __(mov imm1,#catch_frame.element_count<<word_shift)
        __(dnode_align(imm1,imm1,node_size))
        __(stack_allocate_zeroed_ivector(imm0,imm1))
        __(movc16(imm0,make_header(catch_frame.element_count,subtag_catch_frame)))
        __(movs temp2,fn)
        __(ldrne temp2,[temp2,_function.codevector])
        __(ldr temp1,[rcontext,#tcr.last_lisp_frame])
	__(ldr imm1,[rcontext,#tcr.catch_top])
        /* imm2 is mvflag */
        /* arg_z is tag */
        __(ldr arg_x,[rcontext,#tcr.db_link])
        __(ldr temp0,[rcontext,#tcr.xframe])
        __(stmia sp,{imm0,imm1,imm2,arg_z,arg_x,temp0,temp1,temp2})
        __(add imm0,sp,#fulltag_misc)
        __(str imm0,[rcontext,#tcr.catch_top])
        __(add lr,lr,#4)
')	




define(`stack_align',`((($1)+STACK_ALIGN_MASK)&~STACK_ALIGN_MASK)')

define(`clear_alloc_tag',`
        __(bic allocptr,allocptr,#fulltagmask)
')

define(`Cons',`
       	new_macro_labels()
        __(add allocptr,allocptr,#-cons.size+fulltag_cons)
        __(ldr allocbase,[rcontext,#tcr.save_allocbase])
        __(cmp allocptr,allocbase)
        __(bhi macro_label(ok))
        __(uuo_alloc_trap(al))
macro_label(ok):                
        __(str $3,[allocptr,#cons.cdr])
        __(str $2,[allocptr,#cons.car])
        __(mov $1,allocptr)
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



/* Parameters: */

/* $1 = dest reg */
/* $2 = header.
/* $3 = register containing size in bytes.  (We're going to subtract */
/* fulltag_misc from this; do it in the macro body, rather than force the
/* (1 ?) caller to do it. */


define(`Misc_Alloc',`
        new_macro_labels()
	__(sub $3,$3,#fulltag_misc)
	__(sub allocptr,allocptr,$3)
        __(ldr allocbase,[rcontext,#tcr.save_allocbase])
        __(cmp allocptr,allocbase)
        __(bhi macro_label(ok))
        __(uuo_alloc_trap(al))
macro_label(ok):                
	__(str $2,[allocptr,#misc_header_offset])
	__(mov $1,allocptr)
	__(clear_alloc_tag())
')

/*  Parameters $1, $2 as above; $3 = physical size constant. */
define(`Misc_Alloc_Fixed',`
        new_macro_labels()
        __(add allocptr,allocptr,#(-$3)+fulltag_misc)
        __(ldr allocbase,[rcontext,#tcr.save_allocbase])
        __(cmp allocptr,allocbase)
        __(bhi macro_label(ok))
        __(uuo_alloc_trap(al))
macro_label(ok):                
	__(str $2,[allocptr,#misc_header_offset])
	__(mov $1,allocptr)
	__(clear_alloc_tag())
')

/* Stack-allocate an ivector; $1 = header, $0 = dnode-aligned
   size in bytes. */
define(`stack_allocate_ivector',`
        __(str $1,[sp,-$2]!)
        ')
        
                        
/* Stack-allocate an ivector and zero its contents; caller may
   change subtag of header after it's zeroed. 
   $1 = header (tagged as subtag_u32_vector until zeroed), $2 = dnode-
   aligned size in bytes).  Both $1 and $2 are modified here. */
define(`stack_allocate_zeroed_ivector',`
       new_macro_labels()
        __(str $1,[sp,-$2]!)
        __(mov $1,#0)
        __(add $2,sp,$2)
        __(b macro_label(test))
macro_label(loop):      
        __(str $1,[$2])
macro_label(test):                      
        __(sub $2,#dnode_size)
        __(cmp $2,sp)
        __(str $1,[$2,#node_size])
        __(bne macro_label(loop))
        ')
   

define(`check_enabled_pending_interrupt',`
        __(ldr $1,[rcontext,#tcr.interrupt_pending])
        __(cmp $1,0)
        __(ble $2)
        __(uuo_interrupt_now(al))
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
	__(uuo_suspend_now(al))
')

/* $3 points to a uvector header.  Set $1 to the first dnode-aligned address */
/* beyond the uvector, using imm regs $1 and $2 as temporaries. */
define(`skip_stack_vector',`
        new_macro_labels()
        __(ldr $1,[$3])
        __(extract_fulltag($2,$1))        
        __(cmp $2,#fulltag_immheader)
        __(extract_lowbyte($2,$1))
        __(mov $1,$1,lsr #num_subtag_bits)
        __(moveq $1,$1,lsl #2)
        __(beq macro_label(bytes))
        __(cmp $2,#max_32_bit_ivector_subtag)
        __(movle $1,$1,lsl #2)
        __(ble macro_label(bytes))
        __(cmp $2,#max_8_bit_ivector_subtag)
        __(ble macro_label(bytes))
        __(cmp $2,#max_16_bit_ivector_subtag)
        __(movle $1,$1,lsl #1)
        __(ble macro_label(bytes))
        __(cmp $2,subtag_double_float_vector)
        __(moveq $1,$1,lsl #3)
        __(addeq $1,$1,#4)
        __(beq macro_label(bytes))
        __(add $1,$1,#7)
        __(mov $1,$1,lsr #3)
macro_label(bytes):     
        __(add $1,$1,#node_size+(dnode_size-1))
        __(bic $1,$1,#fulltagmask)
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

