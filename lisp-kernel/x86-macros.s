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


/* Try to make macros follow GAS/ATT conventions, where source precedes  */
/* destination.  */

define([lisp_global],[lisp_globals.$1])
                        		
define([ref_global],[
	mov lisp_global($1),$2
])

define([set_global],[
	mov $1,lisp_global($2)
])

define([ref_nrs_value],[
	mov nrs.$1+symbol.vcell,$2
])
	
define([set_nrs_value],[
	mov $1,nrs.$2+symbol.vcell
])
							
define([unbox_fixnum],[
	mov $1,$2
	sar [$]fixnumshift,$2
])

define([box_fixnum],[
        imulq [$]fixnumone,$1,$2
])	


/* box_fixnum, with no effect on flags */
define([box_fixnum_no_flags],[
        leaq (,$1,8),$2
])
                                
define([save_node_regs],[
	push %arg_z
	push %arg_y
	push %arg_x
	push %temp0
	push %temp1
	push %temp2
	push %save0
	push %save1
	push %save2
	push %save3
	push %ra0
	push %fn
])

/* This needs to be done before we transition back to the lisp stack  */
/* from the foreign stack.   */
		
define([zero_node_regs],[
	xor %fn,%fn
	mov %fn,%ra0
	mov %fn,%save3
	mov %fn,%save2
	mov %fn,%save1
	mov %fn,%save0
	mov %fn,%temp2
	mov %fn,%temp1
	mov %fn,%temp0
	mov %fn,%arg_x
	mov %fn,%arg_y
	mov %fn,arg_z
])	
define([restore_node_regs],[
	pop %fn
	pop %ra0
	pop %save3
	pop %save2
	pop %save1
	pop %save0
	pop %temp2
	pop %temp1
	pop %temp0
	pop %arg_x
	pop %arg_y
	pop %arg_z
])	

/* Zero $3 bytes worth of dnodes, starting at offset $2 relative  */
/* to the base register $1.  */


ifdef([DarwinAssembler],[
	.macro zero_dnodes
	.if $2
	movapd %fpzero,$1($0)
	zero_dnodes $0,$1+dnode_size,$2-dnode_size
	.endif
	.endmacro
],[
	.macro zero_dnodes base,disp,nbytes
	.ifgt \nbytes
	movapd %fpzero,\disp(\base)
	zero_dnodes \base,"\disp+dnode_size","\nbytes-dnode_size"
	.endif
	.endm
])	


/* Allocate $1+dnode_size zeroed bytes on the tstack, using $2 as a temp  */
/* reg.  */
	
define([TSP_Alloc_Fixed],[
	define([TSP_Alloc_Size],[((($1+node_size) & ~(dnode_size-1))+dnode_size)])
	subq [$]TSP_Alloc_Size,%rcontext:tcr.next_tsp
        movq %rcontext:tcr.save_tsp,%stack_temp
        movq %rcontext:tcr.next_tsp,$2
	zero_dnodes $2,0,TSP_Alloc_Size
	movq %stack_temp,($2)
        movq %rbp,tsp_frame.save_rbp($2)
        movq $2,%rcontext:tcr.save_tsp
	undefine([TSP_Alloc_Size])
])

/* $1 = size (dnode-aligned, including tsp overhead, $2 scratch.  */
/* Modifies both $1 and $2; on exit, $2 = new_tsp+tsp_overhead, $1 = old tsp  */
	
define([TSP_Alloc_Var],[
	new_macro_labels()
        subq $1,%rcontext:tcr.next_tsp
        movq %rcontext:tcr.save_tsp,%stack_temp
        movq %rcontext:tcr.next_tsp,$2
	jmp macro_label(test)
macro_label(loop):
	movapd %fpzero,0($2)
	addq $dnode_size,$2
macro_label(test):	
	subq $dnode_size,$1
	jge macro_label(loop)
        movq %rcontext:tcr.next_tsp,$2
	movd %stack_temp,$1
	movq $1,($2)
        movq %rbp,tsp_frame.save_rbp($2)
        movq $2,%rcontext:tcr.save_tsp
	addq $dnode_size,$2
])
	
	

define([Allocate_Catch_Frame],[
	TSP_Alloc_Fixed(catch_frame.size,$1)
	movq [$](catch_frame.element_count<<subtag_shift)|subtag_catch_frame,dnode_size($1)
	addq [$]dnode_size+fulltag_misc,$1
])

/* %arg_z = tag,  %xfn = pc, $1 = mvflag 	  */
	
define([Make_Catch],[
	Allocate_Catch_Frame(%imm2)
	movq %rcontext:tcr.catch_top,%imm0
	movq %rcontext:tcr.db_link,%imm1
	movq %arg_z,catch_frame.catch_tag(%imm2)
	movq %imm0,catch_frame.link(%imm2)
	movq [$]$1,catch_frame.mvflag(%imm2)
	movq %rcontext:tcr.xframe,%imm0
	movq %rsp,catch_frame.rsp(%imm2)
	movq %rbp,catch_frame.rbp(%imm2)
        movq %rcontext:tcr.foreign_sp,%stack_temp
	movq %imm1,catch_frame.db_link(%imm2)
	movq %save3,catch_frame._save3(%imm2)
	movq %save2,catch_frame._save2(%imm2)
	movq %save1,catch_frame._save1(%imm2)
	movq %save0,catch_frame._save0(%imm2)
	movq %imm0,catch_frame.xframe(%imm2)
	movq %stack_temp,catch_frame.foreign_sp(%imm2)
	movq %xfn,catch_frame.pc(%imm2)
	movq %imm2,%rcontext:tcr.catch_top
])	

define([nMake_Catch],[
	Allocate_Catch_Frame(%imm2)
	movq %rcontext:tcr.catch_top,%imm0
	movq %rcontext:tcr.db_link,%imm1
	movq %arg_z,catch_frame.catch_tag(%imm2)
	movq %imm0,catch_frame.link(%imm2)
        lea node_size(%rsp),%imm0
	movq [$]$1,catch_frame.mvflag(%imm2)
	movq %imm0,catch_frame.rsp(%imm2)
	movq %rcontext:tcr.xframe,%imm0
	movq %rbp,catch_frame.rbp(%imm2)
        movq %rcontext:tcr.foreign_sp,%stack_temp
	movq %imm1,catch_frame.db_link(%imm2)
	movq %save3,catch_frame._save3(%imm2)
	movq %save2,catch_frame._save2(%imm2)
	movq %save1,catch_frame._save1(%imm2)
	movq %save0,catch_frame._save0(%imm2)
	movq %imm0,catch_frame.xframe(%imm2)
	movq %stack_temp,catch_frame.foreign_sp(%imm2)
	movq %xfn,catch_frame.pc(%imm2)
	movq %imm2,%rcontext:tcr.catch_top
])	
        	
	
/* Consing can get interrupted (either by PROCESS-INTERRUPT or by GC  */
/* activity in some other thread; if it's interrupted, the interrupting  */
/* process needs to be able to determine what's going on well enough  */
/* to be able to either back out of the attempt or finish the job.  */
/* That requires that we use easily recogninized instruction sequences  */
/* and follow certain conventions when consing (either in the kernel  */
/* or in compiled code.)  (One of those conventions involves using  */
/* %allocptr = %temp0 as a freepointer; when consing, %temp0 can't  */
/* contain a live value.)  */
/* Making a CONS cell is a little simpler than making a uvector.  */

/* $1=new_car,$2=new_cdr,$3=dest   */
define([Cons],[
	new_macro_labels()
/* The instructions where tcr.save_allocptr is tagged are difficult  */
/* to interrupt; the interrupting code has to recognize and possibly  */
/* emulate the instructions in between   */
	subq $cons.size-fulltag_cons,%rcontext:tcr.save_allocptr
	movq %rcontext:tcr.save_allocptr,%allocptr
	rcmpq(%allocptr,%rcontext:tcr.save_allocbase)
	jg macro_label(no_trap)
	uuo_alloc()
macro_label(no_trap):	
	andb $~fulltagmask,%rcontext:tcr.save_allocptr
/* Easy to interrupt now that tcr.save_allocptr isn't tagged as a cons    */
	movq $2,cons.cdr(%allocptr)
	movq $1,cons.car(%allocptr)
	ifelse($3,[],[],[
	 movq %allocptr,$3
	])
])

/* The header has to be in %imm0, and the physical size in bytes has  */
/*  to be in %imm1. We bash %imm1.   */

define([Misc_Alloc],[
	subq [$]fulltag_misc,%imm1
	Misc_Alloc_Internal($1)
])

define([Misc_Alloc_Internal],[			
/* Here Be Monsters: we have to treat some/all of this instruction   */
/* sequence atomically, as soon as tcr.save_allocptr becomes tagged.  */
                
	new_macro_labels()
	subq %imm1,%rcontext:tcr.save_allocptr
	movq %rcontext:tcr.save_allocptr,%allocptr
	rcmpq(%allocptr,%rcontext:tcr.save_allocbase)
	jg macro_label(no_trap)
	uuo_alloc()
macro_label(no_trap):	
	movq %imm0,misc_header_offset(%allocptr)
	andb $~fulltagmask,%rcontext:tcr.save_allocptr
/* Now that tcr.save_allocptr is untagged, it's easier to be interrupted   */
	ifelse($1,[],[],[
	 mov %allocptr,$1
	])
])
	
define([Misc_Alloc_Fixed],[
	movq [$]$2-fulltag_misc,%imm1
	Misc_Alloc_Internal($1)
])					

define([vrefr],[
	mov misc_data_offset+($3<<word_shift)($2),$1
])	

define([jump_fn],[
	jmpq *%fn
])
			
define([jump_fname],[
	mov symbol.fcell(%fname),%fn
	jump_fn()
])	
	
define([set_nargs],[
	movw [$]$1<<fixnumshift,%nargs
])

/* $1 = ndigits.  Assumes 4-byte digits           */
define([aligned_bignum_size],[((~(dnode_size-1)&(node_size+(dnode_size-1)+(4*$1))))])
	

define([_car],[
	movq cons.car($1),$2
])	

define([_rplaca],[
	movq $2,cons.car($1)
])	
		
define([_cdr],[
	movq cons.cdr($1),$2
])

define([_rplacd],[
	movq $2,cons.cdr($1)
])	
		
	
	
define([tra],[
        .p2align 3
	ifelse($2,[],[
	.long 0
	],[
	.long $1-$2
	])
$1:	
])
				
define([do_funcall],[
	new_macro_labels()
	movb %temp0_b,%imm0_b
	andb $fulltagmask,%imm0_b
	cmpb $fulltag_symbol,%imm0_b
	/* %fname == %temp0   */
	cmovgq %temp0,%fn
	jl macro_label(bad)
	cmoveq symbol.fcell(%fname),%fn
	jmp *%fn
macro_label(bad):		
	__(uuo_error_not_callable)
])

define([getvheader],[
        movq misc_header_offset($1),$2
])

/* "Size" is unboxed element-count.  $1 (header) and $2 (dest) should  */
/*    both be immediate registers   */
define([header_size],[
        movq $1,$2
        shr $num_subtag_bits,$2
])

/* $2 (length) is fixnum element-count.   */
define([header_length],[
        movq $~255,$2
        andq $1,$2
        shr $num_subtag_bits-fixnumshift,$2
])

/* $1 = vector, $2 = header, $3 = dest   */
define([vector_size],[                                 
        getvheader($1,$2)
        header_size($2,$3)
])

/* $1 = vector, $2 = dest   */
define([vector_length],[                                 
        movq $~255,$2
        andq misc_header_offset($1),$2
        shr $num_subtag_bits-fixnumshift,$2
])
                
/* GAS/ATT comparison arg order drives me nuts   */
define([rcmpq],[
	cmpq $2,$1
])

define([rcmpl],[
	cmpl $2,$1
])	

define([rcmpw],[
	cmpw $2,$1
])	

define([rcmpb],[
	cmpb $2,$1
])		


define([condition_to_boolean],[
        movl [$]t_value,$2_l
        lea (-t_offset)($2),$3
        cmov$1l $2_l,$3_l
])

define([compare_reg_to_nil],[
	cmpb $fulltag_nil,$1_b
])		
	
define([extract_lisptag],[
	movzbl $1_b,$2_l
	andb [$]tagmask,$2_b
])

								
define([extract_fulltag],[
	movzbl $1_b,$2_l
	andb [$]fulltagmask,$2_b
])

define([extract_subtag],[
	movb misc_subtag_offset($1),$2
])

define([extract_typecode],[
	new_macro_labels()
	movzbl $1_b,$2_l
	andb $tagmask,$2_b
	cmpb $tag_misc,$2_b
	jne macro_label(done)
	movb misc_subtag_offset($1),$2_b
macro_label(done):	
])

/* dnode_align(src,delta,dest)  */

        define([dnode_align],[
        lea ($2+(dnode_size-1))($1),$3
	andb $~(dnode_size-1),$3_b
])
	
define([push_argregs],[
	new_macro_labels()
	testw %nargs,%nargs
	jz macro_label(done)
	cmpw [$]2*node_size,%nargs
	je macro_label(yz)
	jb macro_label(z)
	push %arg_x
macro_label(yz):
	push %arg_y
macro_label(z):
	push %arg_z
macro_label(done):
])	


/* $1 = ndigits.  Assumes 4-byte digits           */
define([aligned_bignum_size],[((~(dnode_size-1)&(node_size+(dnode_size-1)+(4*$1))))])

define([discard_temp_frame],[
	movq %rcontext:tcr.save_tsp,$1
	movq ($1),$1
        movq $1,%rcontext:tcr.save_tsp
        movq $1,%rcontext:tcr.next_tsp

])	

define([check_pending_enabled_interrupt],[
	btrq [$]63,%rcontext:tcr.interrupt_pending
	jnc,pt $1
	interrupt_now()
])
	
/* $1 = scratch register, used to access tcr.tlb_pointer.  An interrupt  */
/*   should be taken if interrupts are enabled and the most significant  */
/*   bit of tcr.interrupt_pending is set.  If we take the interrupt, we  */
/*   test and clear the pending bit.  */

define([check_pending_interrupt],[
	new_macro_labels()
	movq %rcontext:tcr.tlb_pointer,$1
	cmpq [$]0,INTERRUPT_LEVEL_BINDING_INDEX($1)
	js,pt macro_label(done)
	check_pending_enabled_interrupt(macro_label(done))
macro_label(done):
])

/* This should only be called from a foreign context; it should be */
/* assumed to bash all non-volatile C registers.  And of course it's */
/* ugly, awful, non-portable, and slow.  %rdi should point to the */
/* linear address that %gs should be made to address (tcr or pthread data) */
        			
ifdef([DARWIN_GS_HACK],[
define([set_gs_base],[
        ifelse($1,[],[
        ],[
        movq $1,%rdi
        ])
        movl [$]0x3000003,%eax
        syscall
])

/* %gs addresses the tcr.  Make it address pthread data before running */
/* foreign code */        
        
define([set_foreign_gs_base],[
        set_gs_base([%rcontext:tcr.osid])
])

/* %gs addresses the tcr.  Get the linear address of the tcr and */
/* copy it to $1 */

define([save_tcr_linear],[
        movq %rcontext:tcr.linear,$1
]) 
	
])

/*  On AMD hardware (at least), a one-byte RET instruction should be */
/*  prefixed with a REP prefix if it (a) is the target of a  */
/*  branch or (b) immediately follows a conditional branch not taken. */
define([repret],[
        .byte 0xf3
         ret
])
                                
        
