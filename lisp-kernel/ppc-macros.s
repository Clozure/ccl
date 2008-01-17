/*   Copyright (C) 1994-2001 Digitool, Inc */
/*   This file is part of OpenMCL.  */

/*   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public */
/*   License , known as the LLGPL and distributed with OpenMCL as the */
/*   file "LICENSE".  The LLGPL consists of a preamble and the LGPL, */
/*   which is distributed with OpenMCL as the file "LGPL".  Where these */
/*   conflict, the preamble takes precedence.   */

/*   OpenMCL is referenced in the preamble as the "LIBRARY." */

/*   The LLGPL is also available online at */
/*   http://opensource.franz.com/preamble.html */

/* The assembler has to do the arithmetic here:	 the expression */
/*   may not be evaluable by m4. */
define([lwi],[ifdef([DARWIN],[
	.if ((($2) & 0xffff8000) == 0xffff8000)
	 li $1,($2)
	.elseif ((($2) & 0xffff8000) == 0)
	 li $1,$2
	.else
	 lis $1,(($2)>>16)
	 .if (($2) & 0xffff) <> 0
	  ori $1,$1,(($2) & 0xffff)
	 .endif
	.endif],[
	.ifeq (($2) & 0xffff8000)-0xffff8000
	 li $1,$2
	.else
	 .ifeq (($2) & 0xffff8000)
	  li $1,$2
	 .else
	  lis $1,($2>>16)
	  .ifne ($2 & 0xffff)
	   ori $1,$1,$2 & 0xffff
	  .endif
	 .endif
	.endif
])])

ifdef([PPC64],[
        define([clrrri],[
        clrrdi $@
        ])       
        define([clrlri],[
        clrldi $@
        ])
        define([clrlri_],[
        clrldi. $@
        ])
        define([ldr],[
        ld $@
        ])
        define([ldrx],[
        ldx $@
        ])
        define([ldru],[
        ldu $@
        ])
        define([str],[
        std $@
        ])
        define([strx],[
        stdx $@
        ])
        define([stru],[
        stdu $@
        ])
        define([strux],[
        stdux $@
        ])	
        define([cmpr],[
        cmpd $@
        ])
        define([cmpri],[
        cmpdi $@
        ])
        define([cmplr],[
        cmpld $@
        ])
        define([cmplri],[
        cmpldi $@
        ])
        define([trlge],[
        tdlge $@
        ])
        define([trllt],[
        tdllt $@
        ])
        define([trlt],[
        tdlt $@
        ])
	define([trlle],[
	tdlle $@
	])
        define([treqi],[
        tdeqi $@
        ])
        define([trnei],[
        tdnei $@
        ])
        define([trgti],[
        tdgti $@
        ])
        define([srari],[
        sradi $@
        ])
        define([srri],[
        srdi $@
        ])
        define([srr],[
        srd $@
        ])
        define([slri],[
        sldi $@
        ])
        define([lrarx],[
        ldarx $@
        ])
        define([strcx],[
        stdcx. $@
        ])
        define([load_highbit],[
        lis $1,0x8000
        sldi $1,$1,32
        ])
        define([extract_bit_shift_count],[
        clrldi $1,$2,64-bitmap_shift
        ])
        define([alloc_trap],[
        tdlt allocptr,allocbase
        ])
        define([mullr],[
        mulld $@
        ])
],[
        define([clrrri],[
        clrrwi $@
        ])
        define([clrlri],[
        clrlwi $@
        ])
        define([clrlri_],[
        clrlwi. $@
        ])
        define([ldr],[
        lwz $@
        ])
        define([ldrx],[
        lwzx $@
        ])
        define([ldru],[
        lwzu $@
        ])
        define([str],[
        stw $@
        ])
        define([strx],[
        stwx $@
        ])
        define([stru],[
        stwu $@
        ])
        define([strux],[
        stwux $@
        ])
        define([cmpr],[
        cmpw $@
        ])
        define([cmpri],[
        cmpwi $@
        ])
        define([cmplr],[
        cmplw $@
        ])
        define([cmplri],[
        cmplwi $@
        ])
        define([trlge],[
        twlge $@
        ])
        define([trllt],[
        twllt $@
        ])
        define([trlt],[
        twlt $@
        ])
        define([trlle],[
        twlle $@
        ])       
        define([treqi],[
        tweqi $@
        ])
        define([trnei],[
        twnei $@
        ])
        define([trgti],[
        twgti $@
        ])
        define([srari],[
        srawi $@
        ])
        define([srri],[
        srwi $@
        ])
        define([srr],[
        srw $@
        ])
        define([slri],[
        slwi $@
        ])
        define([lrarx],[
        lwarx $@
        ])
        define([strcx],[
        stwcx. $@
        ])
        define([load_highbit],[
        lis $1,0x8000
        ])
        define([extract_bit_shift_count],[
        clrlwi $1,$2,32-bitmap_shift
        ])
        define([alloc_trap],[
        twllt allocptr,allocbase
        ])
        define([mullr],[
        mullw $@
        ])
])

/* dnode_align(dest,src,delta) */
        define([dnode_align],[
        la $1,($3+(dnode_size-1))($2)
        clrrri($1,$1,dnode_align_bits)
])

define([extract_fulltag],[
	clrlri($1,$2,nbits_in_word-ntagbits)
        ])

define([extract_lisptag],[
	clrlri($1,$2,nbits_in_word-nlisptagbits)
        ])

define([extract_lisptag_],[
	clrlri_($1,$2,nbits_in_word-nlisptagbits)
        ])

define([extract_subtag],[
	lbz $1,misc_subtag_offset($2)])

ifdef([PPC64],[
define([extract_lowtag],[
        clrldi $1,$2,nbits_in_word-nlowtagbits
])
define([trap_unless_lowtag_equal],[
        clrldi $3,$1,nbits_in_word-nlowtagbits
        tdnei $3,$2
])                
        ])
                               
define([extract_lowbyte],[
        clrlri($1,$2,nbits_in_word-num_subtag_bits)
        ])

define([extract_header],[
	ldr($1,misc_header_offset($2))])


ifdef([PPC64],[
define([extract_typecode],[
	new_macro_labels()
	extract_fulltag($1,$2)
	cmpdi cr0,$1,fulltag_misc
	extract_lisptag($1,$1)
	bne cr0,macro_label(not_misc)
	extract_subtag($1,$2)
macro_label(not_misc):
])],[	
define([extract_typecode],[
	new_macro_labels()
	extract_lisptag($1,$2)
	cmpwi cr0,$1,tag_misc
	bne cr0,macro_label(not_misc)
	extract_subtag($1,$2)
macro_label(not_misc):
])])

define([box_fixnum],[
	slri($1,$2,fixnumshift)])

define([unbox_fixnum],[	
	srari($1,$2,fixnumshift)])

define([loaddf],[
	lfd $1,dfloat.value($2)])
	
define([storedf],[
	stfd $1,dfloat.value($2)])

define([push],[
	stru($1,-node_size($2))])
	
	/* Generally not a great idea. */
define([pop],[
	ldr($1,0($2))
	la $2,node_size($2)])
	
define([vpush],[
	push($1,vsp)])
	
define([vpop],[
	pop($1,vsp)])
	
		
define([unlink],[
	ldr($1,0($1))
 ])

	
define([set_nargs],[
	lwi(nargs,($1)<<fixnumshift)])
	
define([bitclr],[
	rlwinm $1,$2,0,0x1f&((31-($3))+1),0x1f&((31-($3))-1)])
	

define([vref32],[
	lwz $1,misc_data_offset+(($3)<<2)($2)])
        
define([vref16],[/* dest,src,n*/
	lhz $1,misc_data_offset+(($3)<<1)($2)])
	
ifdef([PPC64],[
        define([vref64],[
        ld $1,misc_data_offset+(($3)<<3)($2)])

        define([vrefr],[
        vref64($1,$2,$3)])
],[
        define([vrefr],[
        vref32($1,$2,$3)])
])
        
                	
define([getvheader],[
	ldr($1,vector.header($2))])
	
	/* Size is unboxed element count */
define([header_size],[
	srri($1,$2,num_subtag_bits)])
	
	/* "Length" is fixnum element count */
define([header_length],[
ifdef([PPC64],[
        rldicr $1,$2,nbits_in_word-(num_subtag_bits-nfixnumtagbits),63-nfixnumtagbits
        clrldi $1,$1,(num_subtag_bits-nfixnumtagbits)
        ],[               
	rlwinm $1,$2,nbits_in_word-(num_subtag_bits-nfixnumtagbits),(num_subtag_bits-nfixnumtagbits),31-nfixnumtagbits
        ])
])        


define([vector_size],[
	getvheader(ifelse($3.[],$1,$3),$2)
	header_size($1,ifelse($3.[],$1,$3))])
	
define([vector_length],[
	getvheader($3,$2)
	header_length($1,$3)])

	
define([ref_global],[
	ldr($1,lisp_globals.$2(0))
])

define([set_global],[
	str($1,lisp_globals.$2(0))
])

define([ref_nrs_value],[
	ldr($1,((nrs.$2)+(symbol.vcell))(0))
])
	
define([set_nrs_value],[
	str($1,((nrs.$2)+(symbol.vcell))(0))
])

define([extract_unsigned_byte_bits],[
ifdef([PPC64],[
        rldicr $1,$2,64-fixnumshift,63-$3
],[                
        rlwinm $1,$2,0,32-fixnumshift,31-($3+fixnumshift)
])        
])

define([extract_unsigned_byte_bits_],[
ifdef([PPC64],[
        rldicr. $1,$2,64-fixnumshift,63-$3
],[                
        rlwinm. $1,$2,0,32-fixnumshift,31-($3+fixnumshift)
])        
])

	/* vpop argregs - nargs is known to be non-zero */
define([vpop_argregs_nz],[
	new_macro_labels()
	cmplri(cr1,nargs,node_size*2)
	vpop(arg_z)
	blt cr1,macro_label(l0)
	vpop(arg_y)
	bne cr1,macro_label(l0)
	vpop(arg_x)
macro_label(l0):])

                
	/* vpush argregs */
define([vpush_argregs],[
	new_macro_labels()
	cmplri(cr0,nargs,0)
	cmplri(cr1,nargs,node_size*2)
	beq cr0,macro_label(done)
	blt cr1,macro_label(z)
	beq cr1,macro_label(yz)
	vpush(arg_x)
macro_label(yz):
	vpush(arg_y)
macro_label(z):
	vpush(arg_z)
macro_label(done):
])

define([create_lisp_frame],[
	stru(sp,-lisp_frame.size(sp))
])

                
define([build_lisp_frame],[
	create_lisp_frame()
	str(ifelse($1,[],fn,$1),lisp_frame.savefn(sp))
	str(ifelse($2,[],loc_pc,$2),lisp_frame.savelr(sp))
	str(ifelse($3,[],vsp,$3),lisp_frame.savevsp(sp))
])

        	
define([discard_lisp_frame],[
	la sp,lisp_frame.size(sp)])
	
	
define([_car],[
	ldr($1,cons.car($2))
])
	
define([_cdr],[
	ldr($1,cons.cdr($2))])
	
define([_rplaca],[
	str($2,cons.car($1))])
	
define([_rplacd],[
	str($2,cons.cdr($1))])

define([vpush_saveregs],[
	vpush(save7)
	vpush(save6)
	vpush(save5)
	vpush(save4)
	vpush(save3)
	vpush(save2)
	vpush(save1)
	vpush(save0)])
	
define([restore_saveregs],[
	ldr(save0,node_size*0($1))
	ldr(save1,node_size*1($1))
	ldr(save2,node_size*2($1))
	ldr(save3,node_size*3($1))
	ldr(save4,node_size*4($1))
	ldr(save5,node_size*5($1))
	ldr(save6,node_size*6($1))
	ldr(save7,node_size*7($1))
])

define([vpop_saveregs],[
	restore_saveregs(vsp)
	la vsp,node_size*8(vsp)
])

define([trap_unless_lisptag_equal],[
	extract_lisptag($3,$1)
	trnei($3,$2)
])

ifdef([PPC64],[
define([trap_unless_list],[
	new_macro_labels()
	cmpdi ifelse($3,$3,cr0),$1,nil_value
	extract_fulltag($2,$1)
	beq ifelse($3,$3,cr0),macro_label(is_list)
	tdnei $2,fulltag_cons
macro_label(is_list):	

])],[	
define([trap_unless_list],[
	trap_unless_lisptag_equal($1,tag_list,$2)
])
])

define([trap_unless_fulltag_equal],[
	extract_fulltag($3,$1)
	trnei($3,$2)
])
	
define([trap_unless_typecode_equal],[
        extract_typecode($3,$1)
        trnei($3,$2)
])
        
/* "jump" to the code-vector of the function in nfn. */
define([jump_nfn],[
	ldr(temp0,_function.codevector(nfn))
	mtctr temp0
	bctr
])

/* "call the code-vector of the function in nfn. */
define([call_nfn],[
	ldr(temp0,_function.codevector(nfn))
	mtctr temp0
	bctrl
])
	

/* "jump" to the function in fnames function cell. */
define([jump_fname],[
	ldr(nfn,symbol.fcell(fname))
	jump_nfn()
])

/* call the function in fnames function cell. */
define([call_fname],[
	ldr(nfn,symbol.fcell(fname))
	call_nfn()
])

define([do_funcall],[
	new_macro_labels()
	extract_fulltag(imm0,temp0)
	cmpri(imm0,fulltag_misc)
	mr nfn,temp0
	bne- macro_label(bad)
	extract_subtag(imm0,temp0)
	cmpri(imm0,subtag_function)
	cmpri(cr1,imm0,subtag_symbol)
        bne cr0,macro_label(_sym)
        jump_nfn()
macro_label(_sym):             
	mr fname,temp0
	bne cr1,macro_label(bad)
	jump_fname()
macro_label(bad):
	uuo_interr(error_cant_call,temp0)
])	

define([mkcatch],[
	mflr loc_pc
	ldr(imm0,tcr.catch_top(rcontext))
	lwz imm1,0(loc_pc) /* a forward branch to the catch/unwind cleanup */
	rlwinm imm1,imm1,0,6,29	/* extract LI */
	add loc_pc,loc_pc,imm1
	build_lisp_frame(fn,loc_pc,vsp)
	sub loc_pc,loc_pc,imm1
	la loc_pc,4(loc_pc)	/* skip over the forward branch */
	mtlr loc_pc
	lwi(imm4,(catch_frame.element_count<<num_subtag_bits)|subtag_catch_frame)
	ldr(imm3,tcr.xframe(rcontext))
	ldr(imm1,tcr.db_link(rcontext))
	TSP_Alloc_Fixed_Unboxed(catch_frame.size)
	la nargs,tsp_frame.data_offset+fulltag_misc(tsp)
        str(imm4,catch_frame.header(nargs))
	str(arg_z,catch_frame.catch_tag(nargs))
	str(imm0,catch_frame.link(nargs))
	str(imm2,catch_frame.mvflag(nargs))
	str(sp,catch_frame.csp(nargs))
	str(imm1,catch_frame.db_link(nargs))
        str(first_nvr,catch_frame.regs+0*node_size(nargs))
        str(second_nvr,catch_frame.regs+1*node_size(nargs))
        str(third_nvr,catch_frame.regs+2*node_size(nargs))
        str(fourth_nvr,catch_frame.regs+3*node_size(nargs))
        str(fifth_nvr,catch_frame.regs+4*node_size(nargs))
        str(sixth_nvr,catch_frame.regs+5*node_size(nargs))
        str(seventh_nvr,catch_frame.regs+6*node_size(nargs))
        str(eighth_nvr,catch_frame.regs+7*node_size(nargs))
	str(imm3,catch_frame.xframe(nargs))
	str(rzero,catch_frame.tsp_segment(nargs))
	Set_TSP_Frame_Boxed()
	str(nargs,tcr.catch_top(rcontext))
        li nargs,0

])	

define([restore_catch_nvrs],[
        ldr(first_nvr,catch_frame.regs+(node_size*0)($1))
        ldr(second_nvr,catch_frame.regs+(node_size*1)($1))
        ldr(third_nvr,catch_frame.regs+(node_size*2)($1))
        ldr(fourth_nvr,catch_frame.regs+(node_size*3)($1))
        ldr(fifth_nvr,catch_frame.regs+(node_size*4)($1))
        ldr(sixth_nvr,catch_frame.regs+(node_size*5)($1))
        ldr(seventh_nvr,catch_frame.regs+(node_size*6)($1))
        ldr(eighth_nvr,catch_frame.regs+(node_size*7)($1))
])               

define([DCBZL],[
	.long (31<<26)+(1<<21)+($1<<16)+($2<<11)+(1014<<1)
])
	
define([check_stack_alignment],[
	new_macro_labels()
	andi. $1,sp,STACK_ALIGN_MASK
	beq+ macro_label(stack_ok)
	.long 0
macro_label(stack_ok):
])

define([stack_align],[((($1)+STACK_ALIGN_MASK)&~STACK_ALIGN_MASK)])

define([clear_alloc_tag],[
	clrrri(allocptr,allocptr,ntagbits)
])

/* If the GC interrupts the current thread (after the trap), it needs */
/*   to ensure that the cons cell that's been "reserved" stays reserved */
/*   (e.g. the tagged allocptr has to be treated as a node.)  If that */
/*   reserved cons cell gets tenured, the car and cdr are of a generation */
/*   that's at least as old (so memoization isn't an issue.) */

/*   More generally, if the GC interrupts a thread when allocptr is */
/*   tagged as a cons: */

/*    a) if the trap hasn't been taken (yet), the GC should force the */
/*       thread to resume in such a way that the trap will be taken ; */
/*       the segment allocator should worry about allocating the object. */

/*    b) If the trap has been taken, allocptr is treated as a node as */
/*       described above.  Allocbase is made to point to the base of the */
/*       cons cell, so that the thread's next allocation attempt will */
/*       invoke the segment allocator. */
	
define([Cons],[
	la allocptr,(-cons.size+fulltag_cons)(allocptr)
        alloc_trap()
	str($3,cons.cdr(allocptr))
	str($2,cons.car(allocptr))
	mr $1,allocptr
	clear_alloc_tag()
])


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
/* $2 = header.  (For now, assume that this always encodes length ; */
/* that may change with "large vector" support.) */
/* $3 = register containing size in bytes.  (We're going to subtract */
/* fulltag_misc from this; do it in the macro body, rather than force the
/* (1 ?) caller to do it. */


define([Misc_Alloc],[
	la $3,-fulltag_misc($3)
	sub allocptr,allocptr,$3
        alloc_trap()
	str($2,misc_header_offset(allocptr))
	mr $1,allocptr
	clear_alloc_tag()
])

/*  Parameters $1, $2 as above; $3 = physical size constant. */
define([Misc_Alloc_Fixed],[
	la allocptr,(-$3)+fulltag_misc(allocptr)
        alloc_trap()
	str($2,misc_header_offset(allocptr))
	mr $1,allocptr
	clear_alloc_tag()
])


/*  Zero $3 bytes worth of doublewords, starting at offset $2 relative */
/* to the base register $1. */


ifdef([DARWIN],[
	.macro zero_doublewords
	.if $2
	stfd fp_zero,$1($0)
	zero_doublewords $0,$1+8,$2-8
	.endif
	.endmacro
])

ifdef([LINUX],[
	.macro zero_doublewords base,disp,nbytes
	.if \nbytes
	stfd fp_zero,\disp(\base)
	zero_doublewords \base,\disp+8,\nbytes-8
	.endif
	.endm
])	

define([Set_TSP_Frame_Unboxed],[
	str(tsp,tsp_frame.type(tsp))
])

define([Set_TSP_Frame_Boxed],[
	str(rzero,tsp_frame.type(tsp))
])
		
/* A newly allocated TSP frame is always "raw" (has non-zero type, indicating */
/* that it doesn't contain tagged data. */

define([TSP_Alloc_Fixed_Unboxed],[
	stru(tsp,-($1+tsp_frame.data_offset)(tsp))
	Set_TSP_Frame_Unboxed()
])

define([TSP_Alloc_Fixed_Unboxed_Zeroed],[
	TSP_Alloc_Fixed_Unboxed($1)
	zero_doublewords tsp,tsp_frame.fixed_overhead,$1
])

define([TSP_Alloc_Fixed_Boxed],[
	TSP_Alloc_Fixed_Unboxed_Zeroed($1)
	Set_TSP_Frame_Boxed()
])


        
	

/* This assumes that the backpointer points  to the first byte beyond */
/* each frame.  If we allow segmented tstacks, that constraint might */
/* complicate  their implementation. */
/* We don't need to know the size of the frame (positive or negative, */
/* with or without header).  $1 and $2 are temp registers, $3 is an */
/* optional CR field. */


/* Handle the general case, where the frame might be empty */
define([Zero_TSP_Frame],[
	new_macro_labels()
	la $1,tsp_frame.size-8(tsp)
	ldr($2,tsp_frame.backlink(tsp))
	la $2,-8($2)
	b macro_label(zero_tsp_test)
macro_label(zero_tsp_loop):
	stfdu fp_zero,8($1)
macro_label(zero_tsp_test):	
	cmpr(ifelse($3,[],[cr0],$3),$1,$2)
	bne ifelse($3,[],[cr0],$3),macro_label(zero_tsp_loop)
])

/* Save some branching when we know that the frame can't be empty.*/
define([Zero_TSP_Frame_nz],[
	new_macro_labels()
	la $1,tsp_frame.size-8(tsp)
	ldr($2,tsp_frame.backlink(tsp))
	la $2,-8($2)
macro_label(zero_tsp_loop):
	stfdu fp_zero,8($1)
	cmpr(ifelse($3,[],[cr0],$3),$1,$2)
	bne ifelse($3,[],[cr0],$3),macro_label(zero_tsp_loop)
])
	
/* $1 = 8-byte-aligned size, positive.  $2 (optiional) set */
/* to negated size. */
define([TSP_Alloc_Var_Unboxed],[
	neg ifelse($2,[],$1,$2),$1
	strux(tsp,tsp,ifelse($2,[],$1,$2))
	Set_TSP_Frame_Unboxed()
])

define([TSP_Alloc_Var_Boxed],[
	TSP_Alloc_Var_Unboxed($1)
	Zero_TSP_Frame($1,$2)
	Set_TSP_Frame_Boxed()
])		


define([TSP_Alloc_Var_Boxed_nz],[
	TSP_Alloc_Var_Unboxed($1)
	Zero_TSP_Frame_nz($1,$2)
	Set_TSP_Frame_Boxed()
])		

define([check_pending_interrupt],[
	new_macro_labels()
        ldr(nargs,tcr.tlb_pointer(rcontext))
	ldr(nargs,INTERRUPT_LEVEL_BINDING_INDEX(nargs))
	cmpri(ifelse($1,[],[cr0],$1),nargs,0)
	blt ifelse($1,[],[cr0],$1),macro_label(done)
	bgt ifelse($1,[],[cr0],$1),macro_label(trap)
	ldr(nargs,tcr.interrupt_pending(rcontext))
macro_label(trap):
	trgti(nargs,0)
macro_label(done):
])

/* $1 = ndigits.  Assumes 4-byte digits */        
define([aligned_bignum_size],[((~(dnode_size-1)&(node_size+(dnode_size-1)+(4*$1))))])

define([suspend_now],[
	uuo_interr(error_propagate_suspend,rzero)
])
