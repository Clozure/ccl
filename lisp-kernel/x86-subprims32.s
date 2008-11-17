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
Xsubprims_start:        	
	__(push %ebp)
	__(movl %esp,%ebp)
	/* Switch to the lisp stack */
	__(movl %esp,rcontext(tcr.foreign_sp))
	__(movl rcontext(tcr.save_vsp),%esp)
	__(push $0)
	__(mov %esp,%ebp)
	__(cmpl $0,C(GCDebug))
	__(je 1f)
        __(ref_global(initial_tcr,%imm0))
        __(cmpl rcontext(tcr.linear),%imm0)
        __(jne 1f)
	__(clr %imm0)
	__(uuo_error_gc_trap)
1:
	__(jmp local_label(test))
local_label(loop):
	__(ref_nrs_value(toplcatch,%arg_z))
	__(movl [$]local_label(back_from_catch),%ra0)
	__(movl [$]local_label(test),%xfn)
        __(push %ra0)
	__(jmp _SPmkcatch1v)
__(tra(local_label(back_from_catch)))
	__(movl %arg_y,%temp0)
	__(pushl [$]local_label(back_from_funcall))
	__(set_nargs(0))
	__(jmp _SPfuncall)
__(tra(local_label(back_from_funcall)))
	__(movl $fixnumone,%imm0)
	__(movl [$]local_label(test),%ra0)
	__(jmp _SPnthrow1value)
__(tra(local_label(test)))
	__(movl 4(%ebp),%arg_y)
	__(cmpl $nil_value,%arg_y)
	__(jnz local_label(loop))
local_label(back_to_c):
	__(movl rcontext(tcr.foreign_sp),%esp)
	__(movl %esp,%ebp)
	__(leave)
	__(ret)

/* This is called from C code when a thread (including the initial thread) */
/* starts execution.  (Historically, it also provided a primitive way of */
/* "resettting" a thread in the event of catastrophic failure, but this */
/* hasn't worked in a long time.) */
/* For compatibility with PPC code, this is called with the first foreign */
/* argument pointing to the thread's TCR and the second foreign argument */
/*  being a Boolean which indicates whether the thread should try to */
/* "reset" itself or start running lisp code. */
/* The reset/panic code doesn't work. */

_exportfn(C(start_lisp))
	__(push %ebp)
	__(movl %esp, %ebp)
	__(push %edi)
	__(push %esi)
	__(push %ebx)
	__(mov 8(%ebp), %ebx)	/* get tcr */
        __(cmpb $0,C(rcontext_readonly))
        __(jne 0f)
        __(movw tcr.ldt_selector(%ebx), %rcontext_reg)
0:              
        __(movl 8(%ebp),%eax)
        __(cmpl rcontext(tcr.linear),%eax)
        __(je 1f)
        __(hlt)
1:              
        .if c_stack_16_byte_aligned
	__(sub $12, %esp) 	/* stack now 16-byte aligned */
        .else
        __(andl $~15,%esp)
        .endif
	__(clr %arg_z)
	__(clr %arg_y)	
	__(clr %temp0)
	__(clr %temp1)
	__(clr %fn)
	__(pxor %fpzero, %fpzero)
	__(stmxcsr rcontext(tcr.foreign_mxcsr))
	__(andb $~mxcsr_all_exceptions,rcontext(tcr.foreign_mxcsr))
        __(ldmxcsr rcontext(tcr.lisp_mxcsr))
	__(movl $TCR_STATE_LISP, rcontext(tcr.valence))
	__(call toplevel_loop)
	__(movl $TCR_STATE_FOREIGN, rcontext(tcr.valence))
	__(emms)
        __(leal -3*node_size(%ebp),%esp)
	__(pop %ebx)
	__(pop %esi)
	__(pop %edi)
	__(ldmxcsr rcontext(tcr.foreign_mxcsr))
        __ifdef([WIN32_ES_HACK])
         __(push %ds)
         __(pop %es)
        __endif
	__(movl $nil_value, %eax)
	__(leave)
	__(ret)
Xsubprims_end:           
_endfn

        .data
        .globl C(subprims_start)
        .globl C(subprims_end)
C(subprims_start):      .long Xsubprims_start
C(subprims_end):        .long Xsubprims_end
        .text


