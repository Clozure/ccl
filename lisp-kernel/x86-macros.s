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
	__(mov lisp_global($1),$2)
])

define([set_global],[
	__(mov $1,lisp_global($2))
])

define([ref_nrs_value],[
	__(mov nrs.$1+symbol.vcell,$2)
])
	
define([set_nrs_value],[
	__(mov $1,nrs.$2+symbol.vcell)
])
							
define([unbox_fixnum],[
	__(mov $1,$2)
	__(sar [$]fixnumshift,$2)
])

define([box_fixnum],[
        __(imulq [$]fixnumone,$1,$2)
])	


/* box_fixnum, with no effect on flags */
define([box_fixnum_no_flags],[
        __(leaq (,$1,8),$2)
])
                                

/* Zero $3 bytes worth of dnodes, starting at offset $2 relative  */
/* to the base register $1.  */


ifdef([DarwinAssembler],[
	.macro zero_dnodes
	.if $2
	__(movapd %fpzero,$1($0))
	__(zero_dnodes $0,$1+dnode_size,$2-dnode_size)
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
	__(subq [$]TSP_Alloc_Size,%rcontext:tcr.next_tsp)
        __(movq %rcontext:tcr.save_tsp,%stack_temp)
        __(movq %rcontext:tcr.next_tsp,$2)
	zero_dnodes $2,0,TSP_Alloc_Size
	__(movq %stack_temp,($2))
        __(movq %rbp,tsp_frame.save_rbp($2))
        __(movq $2,%rcontext:tcr.save_tsp)
	undefine([TSP_Alloc_Size])
])

/* $1 = size (dnode-aligned, including tsp overhead, $2 scratch.  */
/* Modifies both $1 and $2; on exit, $2 = new_tsp+tsp_overhead, $1 = old tsp  */
	
define([TSP_Alloc_Var],[
	new_macro_labels()
        subq $1,%rcontext:tcr.next_tsp
        __(movq %rcontext:tcr.save_tsp,%stack_temp)
        __(movq %rcontext:tcr.next_tsp,$2)
	__(jmp macro_label(test))
macro_label(loop):
	__(movapd %fpzero,0($2))
	__(addq $dnode_size,$2)
macro_label(test):	
	__(subq $dnode_size,$1)
	__(jge macro_label(loop))
        __(movq %rcontext:tcr.next_tsp,$2)
	__(movd %stack_temp,$1)
	__(movq $1,($2))
        __(movq %rbp,tsp_frame.save_rbp($2))
        __(movq $2,%rcontext:tcr.save_tsp)
	__(addq $dnode_size,$2)
])
	
	

define([Allocate_Catch_Frame],[
	TSP_Alloc_Fixed(catch_frame.size,$1)
	__(movq [$](catch_frame.element_count<<subtag_shift)|subtag_catch_frame,dnode_size($1))
	__(addq [$]dnode_size+fulltag_misc,$1)
])

/* %arg_z = tag,  %xfn = pc, $1 = mvflag 	  */
	
define([Make_Catch],[
	Allocate_Catch_Frame(%imm2)
	__(movq %rcontext:tcr.catch_top,%imm0)
	__(movq %rcontext:tcr.db_link,%imm1)
	__(movq %arg_z,catch_frame.catch_tag(%imm2))
	__(movq %imm0,catch_frame.link(%imm2))
	__(movq [$]$1,catch_frame.mvflag(%imm2))
	__(movq %rcontext:tcr.xframe,%imm0)
	__(movq %rsp,catch_frame.rsp(%imm2))
	__(movq %rbp,catch_frame.rbp(%imm2))
        __(movq %rcontext:tcr.foreign_sp,%stack_temp)
	__(movq %imm1,catch_frame.db_link(%imm2))
	__(movq %save3,catch_frame._save3(%imm2))
	__(movq %save2,catch_frame._save2(%imm2))
	__(movq %save1,catch_frame._save1(%imm2))
	__(movq %save0,catch_frame._save0(%imm2))
	__(movq %imm0,catch_frame.xframe(%imm2))
	__(movq %stack_temp,catch_frame.foreign_sp(%imm2))
	__(movq %xfn,catch_frame.pc(%imm2))
	__(movq %imm2,%rcontext:tcr.catch_top)
])	

define([nMake_Catch],[
	Allocate_Catch_Frame(%imm2)
	__(movq %rcontext:tcr.catch_top,%imm0)
	__(movq %rcontext:tcr.db_link,%imm1)
	__(movq %arg_z,catch_frame.catch_tag(%imm2))
	__(movq %imm0,catch_frame.link(%imm2))
        __(lea node_size(%rsp),%imm0)
	__(movq [$]$1,catch_frame.mvflag(%imm2))
	__(movq %imm0,catch_frame.rsp(%imm2))
	__(movq %rcontext:tcr.xframe,%imm0)
	__(movq %rbp,catch_frame.rbp(%imm2))
        __(movq %rcontext:tcr.foreign_sp,%stack_temp)
	__(movq %imm1,catch_frame.db_link(%imm2))
	__(movq %save3,catch_frame._save3(%imm2))
	__(movq %save2,catch_frame._save2(%imm2))
	__(movq %save1,catch_frame._save1(%imm2))
	__(movq %save0,catch_frame._save0(%imm2))
	__(movq %imm0,catch_frame.xframe(%imm2))
	__(movq %stack_temp,catch_frame.foreign_sp(%imm2))
	__(movq %xfn,catch_frame.pc(%imm2))
	__(movq %imm2,%rcontext:tcr.catch_top)
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
	__(subq $cons.size-fulltag_cons,%rcontext:tcr.save_allocptr)
	__(movq %rcontext:tcr.save_allocptr,%allocptr)
	__(rcmpq(%allocptr,%rcontext:tcr.save_allocbase))
	__(jg macro_label(no_trap))
	uuo_alloc()
macro_label(no_trap):	
	__(andb $~fulltagmask,%rcontext:tcr.save_allocptr)
/* Easy to interrupt now that tcr.save_allocptr isn't tagged as a cons    */
	__(movq $2,cons.cdr(%allocptr))
	__(movq $1,cons.car(%allocptr))
	ifelse($3,[],[],[
	 __(movq %allocptr,$3)
	])
])

/* The header has to be in %imm0, and the physical size in bytes has  */
/*  to be in %imm1. We bash %imm1.   */

define([Misc_Alloc],[
	__(subq [$]fulltag_misc,%imm1)
	Misc_Alloc_Internal($1)
])

define([Misc_Alloc_Internal],[			
/* Here Be Monsters: we have to treat some/all of this instruction   */
/* sequence atomically, as soon as tcr.save_allocptr becomes tagged.  */
                
	new_macro_labels()
	__(subq %imm1,%rcontext:tcr.save_allocptr)
	__(movq %rcontext:tcr.save_allocptr,%allocptr)
	__(rcmpq(%allocptr,%rcontext:tcr.save_allocbase))
	__(jg macro_label(no_trap))
	uuo_alloc()
macro_label(no_trap):	
	__(movq %imm0,misc_header_offset(%allocptr))
	__(andb $~fulltagmask,%rcontext:tcr.save_allocptr)
/* Now that tcr.save_allocptr is untagged, it's easier to be interrupted   */
	ifelse($1,[],[],[
	 __(mov %allocptr,$1)
	])
])
	
define([Misc_Alloc_Fixed],[
	__(movq [$]$2-fulltag_misc,%imm1)
	Misc_Alloc_Internal($1)
])					

define([vrefr],[
	__(mov misc_data_offset+($3<<word_shift)($2),$1)
])	

define([jump_fn],[
	__(jmpq *%fn)
])
			
define([jump_fname],[
	__(mov symbol.fcell(%fname),%fn)
	jump_fn()
])	
	
define([set_nargs],[
        ifelse(eval($1>15),1,[
        __(movl [$]$1<<fixnumshift,%nargs_l)
        ],[
        __(xorl %nargs_l,%nargs_l)
        ifelse(eval($1),0,[],[
        __(addl [$]$1<<fixnumshift,%nargs_l)
        ])])])
        


/* $1 = ndigits.  Assumes 4-byte digits           */
define([aligned_bignum_size],[((~(dnode_size-1)&(node_size+(dnode_size-1)+(4*$1))))])
	

define([_car],[
	__(movq cons.car($1),$2)
])	

define([_rplaca],[
	__(movq $2,cons.car($1))
])	
		
define([_cdr],[
	__(movq cons.cdr($1),$2)
])

define([_rplacd],[
	__(movq $2,cons.cdr($1))
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
])

define([getvheader],[
        __(movq misc_header_offset($1),$2)
])

/* "Size" is unboxed element-count.  $1 (header) and $2 (dest) should  */
/*    both be immediate registers   */
define([header_size],[
        __(movq $1,$2)
        __(shr $num_subtag_bits,$2)
])

/* $2 (length) is fixnum element-count.   */
define([header_length],[
        __(movq $~255,$2)
        __(andq $1,$2)
        __(shr $num_subtag_bits-fixnumshift,$2)
])

/* $1 = vector, $2 = header, $3 = dest   */
define([vector_size],[                                 
        getvheader($1,$2)
        header_size($2,$3)
])

/* $1 = vector, $2 = dest   */
define([vector_length],[                                 
        __(movq $~255,$2)
        __(andq misc_header_offset($1),$2)
        __(shr $num_subtag_bits-fixnumshift,$2)
])
                
/* GAS/ATT comparison arg order drives me nuts   */
define([rcmpq],[
	__(cmpq $2,$1)
])

define([rcmpl],[
	__(cmpl $2,$1)
])	

define([rcmpw],[
	__(cmpw $2,$1)
])	

define([rcmpb],[
	__(cmpb $2,$1)
])		


define([condition_to_boolean],[
        __(movl [$]t_value,$2_l)
        __(lea (-t_offset)($2),$3)
        __(cmov$1l $2_l,$3_l)
])

define([compare_reg_to_nil],[
	__(cmpb $fulltag_nil,$1_b)
])		
	
define([extract_lisptag],[
	__(movzbl $1_b,$2_l)
	__(andb [$]tagmask,$2_b)
])

								
define([extract_fulltag],[
	__(movzbl $1_b,$2_l)
	__(andb [$]fulltagmask,$2_b)
])

define([extract_subtag],[
	__(movb misc_subtag_offset($1),$2)
])

define([extract_typecode],[
	new_macro_labels()
	__(movzbl $1_b,$2_l)
	__(andb $tagmask,$2_b)
	__(cmpb $tag_misc,$2_b)
	__(jne macro_label(done))
	__(movb misc_subtag_offset($1),$2_b)
macro_label(done):	
])

/* dnode_align(src,delta,dest)  */

        define([dnode_align],[
        __(lea ($2+(dnode_size-1))($1),$3)
	__(andb $~(dnode_size-1),$3_b)
])
	
define([push_argregs],[
	new_macro_labels()
	__(testw %nargs,%nargs)
	__(jz macro_label(done))
	__(cmpw [$]2*node_size,%nargs)
	__(je macro_label(yz))
	__(jb macro_label(z))
	__(push %arg_x)
macro_label(yz):
	__(push %arg_y)
macro_label(z):
	__(push %arg_z)
macro_label(done):
])	


/* $1 = ndigits.  Assumes 4-byte digits           */
define([aligned_bignum_size],[((~(dnode_size-1)&(node_size+(dnode_size-1)+(4*$1))))])

define([discard_temp_frame],[
	__(movq %rcontext:tcr.save_tsp,$1)
	__(movq ($1),$1)
        __(movq $1,%rcontext:tcr.save_tsp)
        __(movq $1,%rcontext:tcr.next_tsp)

])	

define([check_pending_enabled_interrupt],[
	__(btrq [$]63,%rcontext:tcr.interrupt_pending)
	__(jnc,pt $1)
	interrupt_now()
])
	
/* $1 = scratch register, used to access tcr.tlb_pointer.  An interrupt  */
/*   should be taken if interrupts are enabled and the most significant  */
/*   bit of tcr.interrupt_pending is set.  If we take the interrupt, we  */
/*   test and clear the pending bit.  */

define([check_pending_interrupt],[
	new_macro_labels()
	__(movq %rcontext:tcr.tlb_pointer,$1)
	__(cmpq [$]0,INTERRUPT_LEVEL_BINDING_INDEX($1))
	__(js,pt macro_label(done))
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
        __(movq $1,%rdi)
        ])
        __(movl [$]0x3000003,%eax)
        __(syscall)
])

/* %gs addresses the tcr.  Make it address pthread data before running */
/* foreign code */        
        
define([set_foreign_gs_base],[
        set_gs_base([%rcontext:tcr.osid])
])

/* %gs addresses the tcr.  Get the linear address of the tcr and */
/* copy it to $1 */

define([save_tcr_linear],[
        __(movq %rcontext:tcr.linear,$1)
]) 
	
])

/*  On AMD hardware (at least), a one-byte RET instruction should be */
/*  prefixed with a REP prefix if it (a) is the target of a  */
/*  branch or (b) immediately follows a conditional branch not taken. */
define([repret],[
        __(.byte 0xf3)
        __(ret)
])
                                
        
