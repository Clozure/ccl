/*   Copyright (C) 2005 Clozure Associates*/
/*   This file is part of OpenMCL.  */

/*   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public*/
/*   License , known as the LLGPL and distributed with OpenMCL as the*/
/*   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,*/
/*   which is distributed with OpenMCL as the file "LGPL".  Where these*/
/*   conflict, the preamble takes precedence.  */

/*   OpenMCL is referenced in the preamble as the "LIBRARY."*/

/*   The LLGPL is also available online at*/
/*   http://opensource.franz.com/preamble.html*/


	include(lisp.s)
	_beginfile

	.globl _SPmkcatch1v
	.globl _SPnthrow1value


/* This is called from a c-style context and calls a lisp function.*/
/* This does the moral equivalent of*/
/*   (loop */
/*	(let* ((fn (%function_on_top_of_lisp_stack)))*/
/*	  (if fn*/
/*            (catch %toplevel-catch%*/
/*	       (funcall fn))*/
/*            (return nil))))*/


_exportfn(toplevel_loop)
	__(push %rbp)
	__(movq %rsp,%rbp)
	/* Switch to the lisp stack */
        __(push $0)
        __(push $0)
	__(movq %rsp,%rcontext:tcr.foreign_sp)
	__(movq %rcontext:tcr.save_vsp,%rsp)
	__(push $0)
	__(movq %rsp,%rbp)
        
        __(TSP_Alloc_Fixed(0,%temp0))
        __(movsd %fpzero,tsp_frame.save_rbp(%temp0)) /* sentinel */
	__(jmp local_label(test))
local_label(loop):
	__(ref_nrs_value(toplcatch,%arg_z))
	__(leaq local_label(back_from_catch)(%rip),%ra0)
	__(leaq local_label(test)(%rip),%xfn)
        __(push %ra0)
	__(jmp _SPmkcatch1v)
__(tra(local_label(back_from_catch)))
	__(movq %arg_x,%temp0)
	__(leaq local_label(back_from_funcall)(%rip),%ra0)
        __(push %ra0)
	__(set_nargs(0))
	__(jmp _SPfuncall)
__(tra(local_label(back_from_funcall)))
	__(movl $fixnumone,%imm0_l)
	__(leaq local_label(test)(%rip),%ra0)
	__(jmp _SPnthrow1value)	
__(tra(local_label(test)))
	__(movq 8(%rbp),%arg_x)
	__(cmpq $nil_value,%arg_x)
	__(jnz local_label(loop))
local_label(back_to_c):
        __(discard_temp_frame(%imm0))
	__(movq %rcontext:tcr.foreign_sp,%rsp)
        __(addq $dnode_size,%rsp)
	__(movq %rsp,%rbp)
	__(leave)
	__(ret)

/* This is called from C code when a thread (including the initial thread) */
/* starts execution.  (Historically, it also provided a primitive way of */
/* "resettting" a thread in the event of catastrophic failure, but this */
/* hasn't worked in a long time.) */
/* For compatibility with PPC code, this is called with the first foreign */
/* argument pointing to the thread's TCR and the second foreign argument */
/*  being a Boolean which indicates whether the thread should try to */
/* "reset" itself or start running lisp code.  Both of these arguments */
/* are currently ignored (the TCR is maintained in a segment register and */
/*  the reset/panic code doesn't work ...) */
   
_exportfn(C(start_lisp))
	__(push %rbp)
	__(movq %rsp,%rbp)
	__(push %rbx)
	__(push %r12)
	__(push %r13)
	__(push %r14)
	__(push %r15)
        __ifdef([DARWIN_GS_HACK])
         __(set_gs_base())
        __endif
	__(sub $8,%rsp)	/* %rsp is now 16-byte aligned  */
	/* Put harmless values in lisp node registers  */
	__(clr %arg_z)
	__(clr %arg_y)
	__(clr %arg_x)
	__(clr %temp0)
	__(clr %temp1)
	__(clr %temp1)
	__(clr %fn)
	__(clr %ra0)
	__(clr %save0)
	__(clr %save1)
	__(clr %save2)
	__(clr %save3)
	__(pxor %fpzero,%fpzero)	/* fpzero = 0.0[d0] */
        __(stmxcsr %rcontext:tcr.foreign_mxcsr)
        __(andb $~mxcsr_all_exceptions,%rcontext:tcr.foreign_mxcsr)
        __(ldmxcsr %rcontext:tcr.lisp_mxcsr)
	__(movq $TCR_STATE_LISP,%rcontext:tcr.valence)
	__(call toplevel_loop)
	__(movq $TCR_STATE_FOREIGN,%rcontext:tcr.valence)
	__(emms)
	__(addq $8,%rsp)	/* discard alignment word */
	__(pop %r15)
	__(pop %r14)
	__(pop %r13)
	__(pop %r12)
	__(pop %rbx)
        __(ldmxcsr %rcontext:tcr.foreign_mxcsr)
        __ifdef([DARWIN_GS_HACK])
         __(set_foreign_gs_base)
        __endif
	__(movl $nil_value,%eax)
	__(leave)
	__(ret)
_endfn
		
