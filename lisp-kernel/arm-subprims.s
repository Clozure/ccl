/*   Copyright (C) 2010 Clozure Associates */
/*   This file is part of Clozure CL.  */

/*   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public */
/*   License , known as the LLGPL and distributed with Clozure CL as the */
/*   file "LICENSE".  The LLGPL consists of a preamble and the LGPL, */
/*   which is distributed with Clozure CL as the file "LGPL".  Where these */
/*   conflict, the preamble takes precedence.   */

/*   Clozure CL is referenced in the preamble as the "LIBRARY." */

/*   The LLGPL is also available online at */
/*   http://opensource.franz.com/preamble.html */


        .syntax unified
        .arm
        
	include(lisp.s)
	_beginfile

	.globl _SPmkcatch1v
	.globl _SPnthrow1value


/* This is called from a lisp-style context and calls a lisp function. */
/* This does the moral equivalent of */
/*   (loop  */
/*	(let* ((fn (%function_on_top_of_lisp_stack))) */
/*	  (if fn */
/*           (catch %toplevel-catch% */
/*	       (funcall fn)) */
/*            (return nil)))) */

_exportfn(toplevel_loop)
        __(build_lisp_frame(imm0))
	__(b local_label(test))
local_label(loop):
	__(ref_nrs_value(arg_z,toplcatch))
	__(bl _SPmkcatch1v)     /* preserves nfn, at the moment */
	__(b local_label(test))	/* cleanup address, not really a branch */
        __(ldr nfn,[vsp,#0])
	__(set_nargs(0))
        __(bl _SPfuncall)
	__(mov arg_z,#nil_value)
	__(mov imm0,fixnum_one)
	__(bl _SPnthrow1value)
local_label(test):
        __(ldr nfn,[vsp,#0])
        __(cmp nfn,#nil_value)
	__(bne local_label(loop))
        __(return_lisp_frame(imm0))
	_endfn


/* This gets called with R0 pointing to the current TCR. */
/* r1 is 0 if we want to start the whole thing rolling, */
/* non-zero if we want to reset the current process */
/* by throwing to toplevel */

	.globl _SPreset
_exportfn(C(start_lisp))
        __(stmdb sp!,{r4,r5,r6,r7,r8,r9,r10,r11,r12,lr})
        __(mov rcontext,r0)
        __(mov arg_z,#0)
        __(mov arg_y,#0)
        __(mov arg_x,#0)
        __(mov temp0,#0)
        __(mov temp1,#0)
        __(mov temp2,#0)
        __(mov allocptr,#VOID_ALLOCPTR)
        __(mov fn,#0)
        __(ldr vsp,[rcontext,#tcr.save_vsp])
        __(ldr imm2,[rcontext,#tcr.last_lisp_frame])
        __(sub imm0,imm2,sp)
        __(add imm0,imm0,#node_size)
        __(mov imm0,imm0,lsl #num_subtag_bits-word_shift)
        __(orr imm0,imm0,#subtag_u32_vector)
        __(stmdb sp!,{imm0,imm2})
        __(mov imm0,#TCR_STATE_LISP)
        __(str imm0,[rcontext,#tcr.valence])
        __(ldr allocptr,[rcontext,#tcr.save_allocptr])
        __(bl toplevel_loop)
        __(ldmia sp!,{imm0,imm1})
        __(mov imm0,#TCR_STATE_FOREIGN)
        __(str imm1,[rcontext,#tcr.last_lisp_frame])
        __(str imm0,[rcontext,#tcr.valence])
        __(mov imm0,#nil_value)
        __(ldmia sp!,{r4,r5,r6,r7,r8,r9,r10,r11,r12,lr})
        __(bx lr)

_exportfn(_SPsp_end)
        __(nop)
	_endfile

