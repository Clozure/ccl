/*   Copyright (C) 2005 Clozure Associates */
/*   This file is part of OpenMCL.   */
 
/*   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public */
/*   License , known as the LLGPL and distributed with OpenMCL as the */
/*   file "LICENSE".  The LLGPL consists of a preamble and the LGPL, */
/*   which is distributed with OpenMCL as the file "LGPL".  Where these */
/*   conflict, the preamble takes precedence.   */
 
/*   OpenMCL is referenced in the preamble as the "LIBRARY." */
 
/*   The LLGPL is also available online at */
/*   http://opensource.franz.com/preamble.html */


	

	include(lisp.s)

	_beginfile

_exportfn(C(current_stack_pointer))
	__(movl %esp,%eax)
	__(ret)
_endfn
                        
_exportfn(C(count_leading_zeros))
	__(bsr 4(%esp),%eax)
	__(xor $31,%eax)
	__(ret)
_endfn

_exportfn(C(noop))
	__(ret)
_endfn

_exportfn(C(set_mxcsr))
        __(ldmxcsr 4(%esp))
        __(ret)
_endfn
	
_exportfn(C(get_mxcsr))
        __(push $0)
        __(stmxcsr (%esp))
        __(pop %eax)
        __(ret)
_endfn

_exportfn(C(save_fp_context))
_endfn
        
_exportfn(C(restore_fp_context))
_endfn                        

/*  Atomically store new in *p, if *p == old. */
/*  Return actual old value. */
/* natural store_conditional(natural *p, natural old, natural new) */
_exportfn(C(store_conditional))
	__(movl 12(%esp),%edx)	/* new */
	__(movl 8(%esp),%eax)	/* old */
	__(movl 4(%esp),%ecx)	/* ptr */
	__(lock)
        __(cmpxchgl %edx,(%ecx))
	__(cmovne %edx,%eax)
	__(ret)
_endfn

/*	Atomically store val in *p; return previous *p */
/*	of *%rdi. */
/* signed_natural atomic_swap(signed_natural *p, signed_natural val) */
_exportfn(C(atomic_swap))
	__(movl 8(%esp),%eax)
	__(movl 4(%esp),%edx)
	__(lock)
        __(xchg %eax,(%edx))
	__(ret)
_endfn

/*      Logior the value in *p with mask (presumably a */
/*	bitmask with exactly 1 bit set.)  Return non-zero if any of */
/*	the bits in that bitmask were already set. */
/* natural atomic_ior(natural *p, natural mask) */
_exportfn(C(atomic_ior))
	__(movl 4(%esp),%edx)	/* ptr */
0:	__(movl (%edx),%eax)
	__(movl %eax,%ecx)
	__(orl 8(%esp),%ecx)
	__(lock)
        __(cmpxchg %ecx,(%edx))
        __(jnz 0b)
	__(andl 8(%esp),%eax)
	__(ret)
_endfn
        
        
/* Logand the value in *p with mask (presumably a bitmask with exactly 1 */
/* bit set.)  Return the value now in *p (for some value of "now"). */
/* natural atomic_and(natural *p, natural mask) */
_exportfn(C(atomic_and))
	__(movl 4(%esp),%edx)
0:	__(movl (%edx),%eax)
	__(movl %eax,%ecx)
	__(and 8(%esp),%ecx)
	__(lock)
        __(cmpxchg %ecx,(%edx))
        __(jnz 0b)
	__(movl %ecx,%eax)
	__(ret)
_endfn


        __ifdef([DARWIN])
_exportfn(C(pseudo_sigreturn))
        __(hlt)
        __(jmp C(pseudo_sigreturn))
_endfn
        __endif    

/* int cpuid (int code, int *pebx, int *pecx, int *pedx)  */
_exportfn(C(cpuid))
	__(push %ebx)		/* %ebx is non-volatile */
	__(push %esi)		/* ditto here */
	__(movl 12(%esp),%eax)
        __(xorl %ecx,%ecx)
	__(cpuid)
	__(movl 16(%esp),%esi)
	__(movl %ebx,(%esi))
	__(movl 20(%esp),%esi)
	__(movl %ecx,(%esi))
	__(movl 24(%esp),%esi)
	__(movl %edx,(%esi))
	__(pop %esi)
	__(pop %ebx)
	__(ret)
_endfn

/* switch_to_foreign_stack(new_sp, func, arg_0, arg_1, arg_2, arg_3)  */
/*   Not fully general, but should get us off of the signal stack */
/* Beware: on Darwin, GDB can get very confused by this code, and
   doesn't really get unconfused until the target function - the
   handler - has built its stack frame */
/* The lone caller of this function actually doesn't pass arg_3.
   On platforms where the C stack must be 16-byte aligned, pushing
   4 words (arg_0-arg_3) helps make it aligned before the return
   address is (re-)pushed.
   On Linux, there are severe constraints on what the top of stack
   can look like when rt_sigreturn (the code at the return address)
   runs, and there aren't any constraints on stack alignment, so
   we don't push "arg_3" on the new stack.*/
_exportfn(C(switch_to_foreign_stack))
        __(addl $4,%esp)        /* discard return address, on wrong stack */
        __(pop %edi)            /* new esp */
        __(pop %esi)            /* handler */
        __(pop %eax)            /* arg_0 */
        __(pop %ebx)            /* arg_1 */
        __(pop %ecx)            /* arg_2 */
        __ifndef([LINUX])        
        __(pop %edx)            /* arg_3 */
        __endif
        __(mov %edi,%esp)
        __(pop %edi)            /* Return address pushed by caller */
        __ifndef([LINUX])
        __(push %edx)
        __endif
        __(push %ecx)
        __(push %ebx)
        __(push %eax)
        __(push %edi)           /* On some platforms, we don't really return */
        __(jmp *%esi)
_endfn

_exportfn(C(get_vector_registers))
	__(ret)
_endfn

_exportfn(C(put_vector_registers))
	__(ret)
_endfn				

        __ifdef([WIN_32])
_exportfn(C(restore_windows_context))
Xrestore_windows_context_start:
        __(movl 12(%esp),%edx)  /* old valence */
        __(movl 8(%esp),%eax)   /* tcr */
        __(movw tcr.ldt_selector(%eax), %rcontext_reg)
        __(movl 4(%esp),%ecx)   /* context */
        __(movl $0,rcontext(tcr.pending_exception_context))
        __(frstor win32_context.FloatSave(%ecx))
        /* Windows doesn't bother to align the context, so use
          'movupd' here */
        __(movupd win32_context.Xmm0(%ecx),%xmm0)
        __(movupd win32_context.Xmm1(%ecx),%xmm1)
        __(movupd win32_context.Xmm2(%ecx),%xmm2)
        __(movupd win32_context.Xmm3(%ecx),%xmm3)
        __(movupd win32_context.Xmm4(%ecx),%xmm4)
        __(movupd win32_context.Xmm5(%ecx),%xmm5)
        __(movupd win32_context.Xmm6(%ecx),%xmm6)
        __(movupd win32_context.Xmm7(%ecx),%xmm7)
        __(ldmxcsr win32_context.MXCSR(%ecx))
        __(movl win32_context.Ebp(%ecx),%ebp)
        __(movl win32_context.Edi(%ecx),%edi)
        __(movl win32_context.Esi(%ecx),%esi)
        __(movl win32_context.Edx(%ecx),%edx)
        __(movl win32_context.Ebx(%ecx),%ebx)
        __(movl win32_context.Eax(%ecx),%eax)
        __(movl win32_context.Esp(%ecx),%esp)
        __(pushl win32_context.Eip(%ecx))
Xrestore_windows_context_load_rcx:                
        __(movl win32_context.Ecx(%ecx),%ecx)
Xrestore_windows_context_iret:            
        __(ret)
Xrestore_windows_context_end:             
        __(nop)
_endfn
	
_exportfn(C(windows_switch_to_foreign_stack))
        __(pop %eax)
        __(pop %ebx)            /* new %esp */
        __(pop %ecx)            /* handler */
        __(pop %edx)            /* arg */
        __(movl %ebx,%esp)
        __(subl $0x10,%esp)
        __(movl %edx,(%esp))
        __(push %eax)
        __(jmp *%ecx)
_endfn        

        .data
        .globl C(restore_windows_context_start)
        .globl C(restore_windows_context_end)
        .globl C(restore_windows_context_load_rcx)
        .globl C(restore_windows_context_iret)
C(restore_windows_context_start):  .long Xrestore_windows_context_start
C(restore_windows_context_end): .long Xrestore_windows_context_end
C(restore_windows_context_load_rcx):  .long Xrestore_windows_context_load_rcx
C(restore_windows_context_iret): .long Xrestore_windows_context_iret
        .text
        
        __ifdef([WIN32_ES_HACK])
/* Something that we shouldn't return to */
_exportfn(C(windows_halt))
        __(hlt)
_endfn         
        __endif
_exportfn(C(ensure_safe_for_string_operations))
        __ifdef([WIN32_ES_HACK])
        __(movw %es,%ax)
        __(movw %ds,%dx)
        __(cmpw %ax,%dx)
        __(jne 9f)
0:      __(movw %dx,%es)
        __endif
        __(cld)        
	__(ret)
        __ifdef ([WIN32_ES_HACK])
9:      __(hlt)
        __(jmp 0b)
        __endif
_endfn                                       
        __endif
        _endfile

