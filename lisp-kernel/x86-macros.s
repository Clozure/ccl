/*
 * Copyright 2005-2009 Clozure Associates
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/* Try to make macros follow GAS/ATT conventions, where source precedes  */
/* destination.  */

define(`lisp_global',`lisp_globals.$1')
                        		
define(`ref_global',`
	__(mov lisp_global($1),$2)
')

define(`set_global',`
	__(mov $1,lisp_global($2))
')

define(`ref_nrs_value',`
	__(mov nrs.$1+symbol.vcell,$2)
')
	
define(`set_nrs_value',`
	__(mov $1,nrs.$2+symbol.vcell)
')
							
define(`unbox_fixnum',`
	__(mov $1,$2)
	__(sar `$'fixnumshift,$2)
')

define(`box_fixnum',`
        __(imul `$'fixnumone,$1,$2)
')	


/* box_fixnum, with no effect on flags */
define(`box_fixnum_no_flags',`
        __(lea (,$1,fixnumone),$2)
')


/* Zero $3 bytes worth of dnodes, starting at offset $2 relative  */
/* to the base register $1.  */


ifdef(`DarwinAssembler',`
	.macro zero_dnodes
	.if $2
	ifdef(`X8664',`
	__(movapd %fpzero,$1($0))
	',`
	__(movsd %fpzero,$1($0))
	')
	__(zero_dnodes $0,$1+dnode_size,$2-dnode_size)
	.endif
	.endmacro
',`
	.macro zero_dnodes base,disp,nbytes
	.ifgt \nbytes
	ifdef(`X8664',`
        movapd %fpzero,\disp(\base)
	',`
	movsd %fpzero,\disp(\base)
	')
	zero_dnodes \base,"\disp+dnode_size","\nbytes-dnode_size"
	.endif
	.endm
')	


/* Allocate $1+dnode_size zeroed bytes on the tstack, using $2 as a temp  */
/* reg.  */

ifdef(`X8632',`
define(`TSP_Alloc_Fixed',`
	define(`TSP_Alloc_Size',`((($1+node_size) & ~(dnode_size-1))+dnode_size)')
	__(subl `$'TSP_Alloc_Size,rcontext(tcr.next_tsp))
	__(movd rcontext(tcr.save_tsp),%stack_temp)
	__(movl rcontext(tcr.next_tsp),$2)
	zero_dnodes $2,0,TSP_Alloc_Size
	__(movd %stack_temp,($2))
	__(movl %ebp,tsp_frame.save_ebp($2))
	__(movl $2,rcontext(tcr.save_tsp))
	undefine(`TSP_Alloc_Size')
')',`
define(`TSP_Alloc_Fixed',`
	define(`TSP_Alloc_Size',`((($1+node_size) & ~(dnode_size-1))+dnode_size)')
	__(subq `$'TSP_Alloc_Size,rcontext(tcr.next_tsp))
        __(movq rcontext(tcr.save_tsp),%stack_temp)
        __(movq rcontext(tcr.next_tsp),$2)
	zero_dnodes $2,0,TSP_Alloc_Size
	__(movq %stack_temp,($2))
        __(movq %rbp,tsp_frame.save_rbp($2))
        __(movq $2,rcontext(tcr.save_tsp))
	undefine(`TSP_Alloc_Size')
')')

/* $1 = size (dnode-aligned, including tsp overhead, $2 scratch.  */
/* Modifies both $1 and $2; on exit, $2 = new_tsp+tsp_overhead, $1 = old tsp  */

ifdef(`X8632',`
define(`TSP_Alloc_Var',`
        new_macro_labels()
        __(subl $1,rcontext(tcr.next_tsp))
        __(movd rcontext(tcr.save_tsp),%stack_temp)
        __(movl rcontext(tcr.next_tsp),$2)
        __(jmp macro_label(test))
macro_label(loop):
        __(movsd %fpzero,0($2))
        __(addl $dnode_size,$2)
macro_label(test):
        __(subl $dnode_size,$1)
        __(jge macro_label(loop))
        __(movl rcontext(tcr.next_tsp),$2)
        __(movd %stack_temp,$1)
        __(movl $1,($2))
	__(movl %ebp,tsp_frame.save_ebp($2))
        __(movl $2,rcontext(tcr.save_tsp))
        __(addl $dnode_size,$2)
')',`
define(`TSP_Alloc_Var',`
	new_macro_labels()
        subq $1,rcontext(tcr.next_tsp)
        __(movq rcontext(tcr.save_tsp),%stack_temp)
        __(movq rcontext(tcr.next_tsp),$2)
	__(jmp macro_label(test))
macro_label(loop):
	__(movapd %fpzero,0($2))
	__(addq $dnode_size,$2)
macro_label(test):	
	__(subq $dnode_size,$1)
	__(jge macro_label(loop))
        __(movq rcontext(tcr.next_tsp),$2)
	__(movq %stack_temp,$1)
	__(movq $1,($2))
        __(movq %rbp,tsp_frame.save_rbp($2))
        __(movq $2,rcontext(tcr.save_tsp))
	__(addq $dnode_size,$2)
')')
	
	
ifdef(`X8632',`
define(`Allocate_Catch_Frame',`
        TSP_Alloc_Fixed(catch_frame.size,$1)
        __(movl `$'(catch_frame.element_count<<subtag_shift)|subtag_catch_frame,dnode_size($1))
        __(addl `$'dnode_size+fulltag_misc,$1)
')',`
define(`Allocate_Catch_Frame',`
	TSP_Alloc_Fixed(catch_frame.size,$1)
	__(movq `$'(catch_frame.element_count<<subtag_shift)|subtag_catch_frame,dnode_size($1))
	__(addq `$'dnode_size+fulltag_misc,$1)
')')

/* %arg_z = tag,  %xfn = pc, $1 = mvflag 	  */

ifdef(`X8632',`
define(`Make_Catch',`
	Allocate_Catch_Frame(%imm0)
	__(movd rcontext(tcr.catch_top),%mm0)
	__(movd rcontext(tcr.db_link),%mm1)
	__(movl %arg_z,catch_frame.catch_tag(%imm0))
	__(movd %mm0,catch_frame.link(%imm0))
	__(movl `$'$1,catch_frame.mvflag(%imm0))
	__(movd rcontext(tcr.xframe),%mm0)
        __(movd rcontext(tcr.nfp),%mm2)
	__(movl %esp,catch_frame.esp(%imm0))
	__(movl %ebp,catch_frame.ebp(%imm0))
        __(movd rcontext(tcr.foreign_sp),%stack_temp)
	__(movd %stack_temp,catch_frame.foreign_sp(%imm0))
	__(movd %mm1,catch_frame.db_link(%imm0))
	__(movd %mm0,catch_frame.xframe(%imm0))
        __(movd %mm2,catch_frame.nfp(%imm0))
	__(movl %xfn,catch_frame.pc(%imm0))
	__(movl %imm0,rcontext(tcr.catch_top))
')',`
define(`Make_Catch',`
	Allocate_Catch_Frame(%imm2)
	__(movq rcontext(tcr.catch_top),%imm0)
	__(movq rcontext(tcr.db_link),%imm1)
	__(movq %arg_z,catch_frame.catch_tag(%imm2))
	__(movq %imm0,catch_frame.link(%imm2))
	__(movq `$'$1,catch_frame.mvflag(%imm2))
	__(movq rcontext(tcr.xframe),%imm0)
	__(movq %rsp,catch_frame.rsp(%imm2))
	__(movq %rbp,catch_frame.rbp(%imm2))
        __(movq rcontext(tcr.foreign_sp),%stack_temp)
	__(movq %imm1,catch_frame.db_link(%imm2))
        __(movq rcontext(tcr.nfp),%imm1)       
	__(movq %imm0,catch_frame.xframe(%imm2))
	__(movq %stack_temp,catch_frame.foreign_sp(%imm2))
        __(movq %imm1,catch_frame.nfp(%imm2))
	__(movq %xfn,catch_frame.pc(%imm2))
	__(movq %imm2,rcontext(tcr.catch_top))
')')	

ifdef(`X8632',`
define(`nMake_Catch',`
	Allocate_Catch_Frame(%imm0)
	__(movd rcontext(tcr.catch_top),%mm0)
	__(movd rcontext(tcr.db_link),%mm1)
	__(movl %arg_z,catch_frame.catch_tag(%imm0))
	__(movd %mm0,catch_frame.link(%imm0))
	__(movl %esp,catch_frame.esp(%imm0))
	__(addl $node_size,catch_frame.esp(%imm0))
	__(movl `$'$1,catch_frame.mvflag(%imm0))
	__(movd rcontext(tcr.xframe),%mm0)
        __(movd rcontext(tcr.nfp),%mm2)
	__(movl %ebp,catch_frame.ebp(%imm0))
        __(movd rcontext(tcr.foreign_sp),%stack_temp)
	__(movd %mm1,catch_frame.db_link(%imm0))
	__(movd %mm0,catch_frame.xframe(%imm0))
        __(movd %mm2,catch_frame.nfp(%imm0))
	__(movd %stack_temp,catch_frame.foreign_sp(%imm0))
	__(movl %xfn,catch_frame.pc(%imm0))
	__(movl %imm0,rcontext(tcr.catch_top))
')',`	
define(`nMake_Catch',`
	Allocate_Catch_Frame(%imm2)
	__(movq rcontext(tcr.catch_top),%imm0)
	__(movq rcontext(tcr.db_link),%imm1)
	__(movq %arg_z,catch_frame.catch_tag(%imm2))
	__(movq %imm0,catch_frame.link(%imm2))
        __(lea node_size(%rsp),%imm0)
	__(movq `$'$1,catch_frame.mvflag(%imm2))
	__(movq %imm0,catch_frame.rsp(%imm2))
	__(movq rcontext(tcr.xframe),%imm0)
        __(movq rcontext(tcr.nfp),%mm0)
	__(movq %rbp,catch_frame.rbp(%imm2))
        __(movq rcontext(tcr.foreign_sp),%stack_temp)
	__(movq %imm1,catch_frame.db_link(%imm2))
	__(movq %imm0,catch_frame.xframe(%imm2))
        __(movq %mm0,catch_frame.nfp(%imm2))
	__(movq %stack_temp,catch_frame.foreign_sp(%imm2))
	__(movq %xfn,catch_frame.pc(%imm2))
	__(movq %imm2,rcontext(tcr.catch_top))
')')	
        	
	
/* Consing can get interrupted (either by PROCESS-INTERRUPT or by GC  */
/* activity in some other thread; if it is interrupted, the interrupting  */
/* process needs to be able to determine what is going on well enough  */
/* to be able to either back out of the attempt or finish the job.  */
/* That requires that we use easily recogninized instruction sequences  */
/* and follow certain conventions when consing (either in the kernel  */
/* or in compiled code.)  (One of those conventions involves using  */
/* %allocptr = %temp0 as a freepointer; when consing, %temp0 can not  */
/* contain a live value.)  */
/* Making a CONS cell is a little simpler than making a uvector.  */

/* $1=new_car,$2=new_cdr,$3=dest   */

ifdef(`X8632',`
define(`Cons',`
	new_macro_labels()
/* The instructions where tcr.save_allocptr is tagged are difficult  */
/* to interrupt; the interrupting code has to recognize and possibly  */
/* emulate the instructions in between   */
        __(subl $cons.size-fulltag_cons,rcontext(tcr.save_allocptr))
        __(movl rcontext(tcr.save_allocptr),%allocptr)
        __(rcmpl(%allocptr,rcontext(tcr.save_allocbase)))
        __(ja macro_label(no_trap))
        uuo_alloc()
macro_label(no_trap):
        __(andb $~fulltagmask,rcontext(tcr.save_allocptr))
/* Easy to interrupt now that tcr.save_allocptr is not tagged as a cons    */
        __(movl $2,cons.cdr(%allocptr))
        __(movl $1,cons.car(%allocptr))
        ifelse($3,`',`',`
         __(movl %allocptr,$3)
        ')
')',`

define(`Cons',`
	new_macro_labels()
/* The instructions where tcr.save_allocptr is tagged are difficult  */
/* to interrupt; the interrupting code has to recognize and possibly  */
/* emulate the instructions in between   */
	__(subq $cons.size-fulltag_cons,rcontext(tcr.save_allocptr))
	__(movq rcontext(tcr.save_allocptr),%allocptr)
	__(rcmpq(%allocptr,rcontext(tcr.save_allocbase)))
	__(ja macro_label(no_trap))
	uuo_alloc()
macro_label(no_trap):	
	__(andb $~fulltagmask,rcontext(tcr.save_allocptr))
/* Easy to interrupt now that tcr.save_allocptr is not tagged as a cons    */
	__(movq $2,cons.cdr(%allocptr))
	__(movq $1,cons.car(%allocptr))
	ifelse($3,`',`',`
	 __(movq %allocptr,$3)
	')
')')

ifdef(`X8632',`
/* Header in %mm0, size in bytes in %imm0.  We bash %imm0. */
define(`Misc_Alloc',`
	__(sub `$'fulltag_misc,%imm0)
	Misc_Alloc_Internal($1)
')',`
/* Header in %imm0, size in bytes in %imm1.  We bash %imm1. */
define(`Misc_Alloc',`
	__(subq `$'fulltag_misc,%imm1)
	Misc_Alloc_Internal($1)
')')

/* Here Be Monsters: we have to treat some/all of this instruction   */
/* sequence atomically, as soon as tcr.save_allocptr becomes tagged.  */
                
ifdef(`X8632',`
define(`Misc_Alloc_Internal',`                  
        new_macro_labels()
        __(subl %imm0,rcontext(tcr.save_allocptr))
        __(movl rcontext(tcr.save_allocptr),%allocptr)
        __(cmpl rcontext(tcr.save_allocbase),%allocptr)
        __(ja macro_label(no_trap))
        uuo_alloc()
macro_label(no_trap):   
        __(movd %mm0,misc_header_offset(%allocptr))
        __(andb $~fulltagmask,rcontext(tcr.save_allocptr))
/* Now that tcr.save_allocptr is untagged, it is easier to be interrupted   */
        ifelse($1,`',`',`
         __(mov %allocptr,$1)
        ')
')',`	
define(`Misc_Alloc_Internal',`			
	new_macro_labels()
	__(subq %imm1,rcontext(tcr.save_allocptr))
	__(movq rcontext(tcr.save_allocptr),%allocptr)
	__(rcmpq(%allocptr,rcontext(tcr.save_allocbase)))
	__(ja macro_label(no_trap))
	uuo_alloc()
macro_label(no_trap):	
	__(movq %imm0,misc_header_offset(%allocptr))
	__(andb $~fulltagmask,rcontext(tcr.save_allocptr))
/* Now that tcr.save_allocptr is untagged, it is easier to be interrupted   */
	ifelse($1,`',`',`
	 __(mov %allocptr,$1)
	')
')')

ifdef(`X8632',`
define(`Misc_Alloc_Fixed',`
	__(mov `$'$2-fulltag_misc,%imm0)
	Misc_Alloc_Internal($1)
')',`
define(`Misc_Alloc_Fixed',`
	__(movq `$'$2-fulltag_misc,%imm1)
	Misc_Alloc_Internal($1)
')')					

define(`vrefr',`
	__(mov misc_data_offset+($3<<word_shift)($2),$1)
')	

define(`jump_fn',`
	__(jmp *%fn)
')
			
define(`jump_fname',`
	__(mov symbol.fcell(%fname),%fn)
	jump_fn()
')	

ifdef(`X8632',`
define(`set_nargs',`
	__(xorl %nargs,%nargs)
	__(addl `$'$1<<fixnumshift,%nargs)
')',`
define(`set_nargs',`
        ifelse(eval($1>15),1,`
        __(movl `$'$1<<fixnumshift,%nargs)
        ',`
        __(xorl %nargs,%nargs)
        ifelse(eval($1),0,`',`
        __(addl `$'$1<<fixnumshift,%nargs)
        ')')')
')

/* $1 = ndigits.  Assumes 4-byte digits           */
define(`aligned_bignum_size',`((~(dnode_size-1)&(node_size+(dnode_size-1)+(4*$1))))')
	

define(`_car',`
	__(mov cons.car($1),$2)
')	

define(`_rplaca',`
	__(mov $2,cons.car($1))
')	
		
define(`_cdr',`
	__(mov cons.cdr($1),$2)
')

define(`_rplacd',`
	__(mov $2,cons.cdr($1))
')	
		
	
	
ifdef(`X8632',`
define(`tra',`
        .p2align 3
	.long 0
	.byte 0
$1:	
')',`
define(`tra',`
        .p2align 3
	ifelse($2,`',`
	.long 0
	',`
	.long $1-$2
	')
$1:	
')')

ifdef(`X8632',`
define(`do_funcall',`
        new_macro_labels()
        extract_fulltag(%temp0,%imm0)
        __(cmpb $fulltag_misc,%imm0_b)
        __(jne macro_label(bad))
        __(cmpb $subtag_function,misc_subtag_offset(%temp0))
        __(jne macro_label(maybe_symbol))
        __(mov %temp0,%fn)
        __(jmp *%fn)
macro_label(maybe_symbol):
        __(cmpb $subtag_symbol,misc_subtag_offset(%temp0))
        __(jne macro_label(bad))
        /* %fname == %temp0 */
        __(mov symbol.fcell(%fname),%fn)
        __(jmp *%fn)
macro_label(bad):
        __(uuo_error_not_callable)
')',`
define(`do_funcall',`
	new_macro_labels()
	__(movb %temp0_b,%imm0_b)
	__(andb $fulltagmask,%imm0_b)
	__(cmpb $fulltag_symbol,%imm0_b)
	/* %fname == %temp0   */
	__(cmovgq %temp0,%fn)
	jl macro_label(bad)
	__(cmoveq symbol.fcell(%fname),%fn)
	__(jmp *%fn)
macro_label(bad):		
	__(uuo_error_not_callable)
')')

define(`getvheader',`
        __(mov misc_header_offset($1),$2)
')

/* "Size" is unboxed element-count.  $1 (header) and $2 (dest) should  */
/*    both be immediate registers   */
define(`header_size',`
        __(mov $1,$2)
        __(shr $num_subtag_bits,$2)
')

/* $2 (length) is fixnum element-count.   */
define(`header_length',`
        __(mov $~255,$2)
        __(and $1,$2)
        __(shr $num_subtag_bits-fixnumshift,$2)
')

/* $1 = vector, $2 = header, $3 = dest   */
define(`vector_size',`                                 
        __(getvheader($1,$2))
        __(header_size($2,$3))
')

/* $1 = vector, $2 = dest   */
define(`vector_length',`                                 
        __(mov $~255,$2)
        __(and misc_header_offset($1),$2)
        __(shr $num_subtag_bits-fixnumshift,$2)
')
                
/* GAS/ATT comparison arg order drives me nuts   */
define(`rcmpq',`
	__(cmpq $2,$1)
')

define(`rcmpl',`
	__(cmpl $2,$1)
')	

define(`rcmpw',`
	__(cmpw $2,$1)
')	

define(`rcmpb',`
	__(cmpb $2,$1)
')		


define(`condition_to_boolean',`
        __(movl `$'t_value,$2_l)
        __(lea (-t_offset)($2),$3)
        __(cmov$1l $2_l,$3_l)
')

ifdef(`X8632',`
define(`compare_reg_to_nil',`
	__(cmp $nil_value,$1)
')',`
define(`compare_reg_to_nil',`
	__(cmpb $fulltag_nil,$1_b)
')')

ifdef(`X8632',`
define(`extract_lisptag',`
	__(movl $1,$2)
	__(and `$'tagmask,$2)
')',`
define(`extract_lisptag',`
	__(movzbl $1_b,$2_l)
	__(andb `$'tagmask,$2_b)
')')

								
define(`extract_fulltag',`
	__(movzbl $1_b,$2_l)
	__(andb `$'fulltagmask,$2_b)
')

ifdef(`X8664',`
define(`extract_subtag',`
	__(movzbl misc_subtag_offset($1),$2_l)
')
',`
define(`extract_subtag',`
	__(movb misc_subtag_offset($1),$2)
')')        
                


ifdef(`X8632',`
define(`extract_typecode',`
	new_macro_labels()
	__(mov $1,$2)
	__(andl $tagmask,$2)
	__(cmpb $tag_misc,$2_b)
	__(jne macro_label(done))
	__(movb misc_subtag_offset($1),$2_b)
macro_label(done):
')',`
define(`extract_typecode',`
	new_macro_labels()
	__(movzbl $1_b,$2_l)
	__(andb $tagmask,$2_b)
	__(cmpb $tag_misc,$2_b)
	__(jne macro_label(done))
	__(movb misc_subtag_offset($1),$2_b)
macro_label(done):
')')

/* dnode_align(src,delta,dest)  */

define(`dnode_align',`
        __(lea ($2+(dnode_size-1))($1),$3)
	__(andb $~(dnode_size-1),$3_b)
')

ifdef(`X8632',`
define(`push_argregs',`
	new_macro_labels()
	/* xxx hack alert: when the compiler calls a keyword subprim */
	/* (SPsimple_keywords, SPkeyword_args, SP_keyword_bind) */
	/* it puts some flags in the upper half of %temp1, which
	/* is %nargs.  We use the cmpw here to avoid seeing those flags. */
	__(cmpw `$'1*node_size,%nargs_w)
	__(jb macro_label(done))
	__(je macro_label(z))
	__(push %arg_y)
macro_label(z):
	__(push %arg_z)
macro_label(done):
')',`
define(`push_argregs',`
	new_macro_labels()
	__(testl %nargs,%nargs)
	__(jz macro_label(done))
	__(cmpl `$'2*node_size,%nargs)
	__(je macro_label(yz))
	__(jb macro_label(z))
	__(push %arg_x)
macro_label(yz):
	__(push %arg_y)
macro_label(z):
	__(push %arg_z)
macro_label(done):
')')	


/* $1 = ndigits.  Assumes 4-byte digits           */
define(`aligned_bignum_size',`((~(dnode_size-1)&(node_size+(dnode_size-1)+(4*$1))))')

define(`discard_temp_frame',`
	__(mov rcontext(tcr.save_tsp),$1)
	__(mov ($1),$1)
	__(mov $1,rcontext(tcr.save_tsp))
	__(mov $1,rcontext(tcr.next_tsp))
')

ifdef(`X8632',`	
define(`check_pending_enabled_interrupt',`
	__(btrl `$'31,rcontext(tcr.interrupt_pending))
	__(jnc $1)
	interrupt_now()
')',`
define(`check_pending_enabled_interrupt',`
	__(btrq `$'63,rcontext(tcr.interrupt_pending))
	__(jnc $1)
	interrupt_now()
')')
	
/* $1 = scratch register, used to access tcr.tlb_pointer.  An interrupt  */
/*   should be taken if interrupts are enabled and the most significant  */
/*   bit of tcr.interrupt_pending is set.  If we take the interrupt, we  */
/*   test and clear the pending bit.  */

ifdef(`X8632',`
define(`check_pending_interrupt',`
	new_macro_labels()
	__(movl rcontext(tcr.tlb_pointer),$1)
	__(cmpl `$'0,INTERRUPT_LEVEL_BINDING_INDEX($1))
	__(js macro_label(done))
	check_pending_enabled_interrupt(macro_label(done))
macro_label(done):
')',`
define(`check_pending_interrupt',`
	new_macro_labels()
	__(movq rcontext(tcr.tlb_pointer),$1)
	__(cmpq `$'0,INTERRUPT_LEVEL_BINDING_INDEX($1))
	__(js macro_label(done))
	check_pending_enabled_interrupt(macro_label(done))
macro_label(done):
')')

/*  On AMD hardware (at least), a one-byte RET instruction should be */
/*  prefixed with a REP prefix if it (a) is the target of a  */
/*  branch or (b) immediately follows a conditional branch not taken. */
define(`repret',`
        __(.byte 0xf3)
        __(ret)
')

ifdef(`X8632',`
define(`regnum',`ifelse($1, `%eax', `0',
       $1, `%ecx', `1',
       $1, `%edx', `2',
       $1, `%ebx', `3',
       $1, `%esp', `4',
       $1, `%ebp', `5',
       $1, `%esi', `6',
       $1, `%edi', `7',
	"unknown register")dnl
')

define(`mark_as_node', `
	__(xorl $1,$1)
        __(orb `$'(1<<regnum($1)), rcontext(tcr.node_regs_mask))
')

define(`mark_as_imm',`
        __(andb `$'~(1<<regnum($1)), rcontext(tcr.node_regs_mask))
')
')

define(`check_cstack_alignment',`
        new_macro_labels()
        __(testb `$'7,rcontext(tcr.foreign_sp))
        __(je macro_label(done))
        __(hlt)
macro_label(done):
')

        __ifdef(`WINDOWS')
define(`windows_cstack_probe',`
        new_macro_labels()
        __(cmp `$'0x1000,$1)
        __(jb macro_label(done))
        __(mov rcontext(tcr.foreign_sp),$2)
        __(orl `$'0,-0x1000($2))
        __(cmp `$'0x2000,$1)
        __(jb macro_label(done))
        __(orl `$'0,-0x2000($2))
        __(cmp `$'0x3000,$1)
        __(jb macro_label(done))
        __(orl `$'0,-0x3000($2))
        __(cmp `$'0x4000,$1)
        __(jb macro_label(done))
        __(orl `$'0,-0x4000($2))
        __(cmp `$'0x5000,$1)
        __(jb macro_label(done))
        __(orl `$'0,-0x5000($2))
        __(cmp `$'0x6000,$1)
        __(jb macro_label(done))
        __(orl `$'0,-0x6000($2))
        __(cmp `$'0x7000,$1)
        __(jb macro_label(done))
        __(orl `$'0,-0x7000($2))
        __(cmp `$'0x8000,$1)
        __(jb macro_label(done))
        __(orl `$'0,-0x8000($2))
        __(cmp `$'0x9000,$1)
        __(jb macro_label(done))
        __(orl `$'0,-0x9000($2))
        __(cmp `$'0xa000,$1)
        __(jb macro_label(done))
        __(orl `$'0,-0xa000($2))
        __(cmp `$'0xb000,$1)
        __(jb macro_label(done))
        __(orl `$'0,-0xb000($2))
        __(cmp `$'0xc000,$1)
        __(jb macro_label(done))
        __(orl `$'0,-0xc000($2))
        __(cmp `$'0xd000,$1)
        __(jb macro_label(done))
        __(orl `$'0,-0xd000($2))
        __(cmp `$'0xe000,$1)
        __(jb macro_label(done))
        __(orl `$'0,-0xe000($2))
        __(cmp `$'0xf000,$1)
        __(jb macro_label(done))
        __(orl `$'0,-0xf000($2))
macro_label(done):      
')
        __endif
        
/* If $1 is a  an ivector typecode, copy $1 to $2 else set $2 to 0.
   $1 and $2 should be byte registers, typically %imm0_b and %imm0_bh */  
define(`ivector_typecode_p',`
        __ifdef(`X8664')
        __(movl $1,$3)
        __(andl $fulltagmask,$3)
        __(movl $((1<<fulltag_immheader_0)|(1<<fulltag_immheader_1)|(1<<fulltag_immheader_2)),$2)
        __(bt $3,$2)
        __(movl $1,$2)
        __(movl $`0',$3)
        __(cmovael $3,$2)
        __else
        new_macro_labels()
        __(movb $1,$2)
        __(andb $fulltagmask,$2)
        __(cmpb $fulltag_immheader,$2)
        __(movb $1,$2)
        __(je macro_label(done))
        __(movb `$'0,$2)
macro_label(done):
        __endif
        ')
        
                        
