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
/* Also: do this function and its caller observe ia32 stack-alignment
   constraints, whatever they are ? */       
_exportfn(C(switch_to_foreign_stack))
        __(addl $4,%esp)        /* discard return address, on wrong stack */
        __(pop %edi)            /* new esp */
        __(pop %esi)            /* handler */
        __(pop %eax)            /* arg_0 */
        __(pop %ebx)            /* arg_1 */
        __(pop %ecx)            /* arg_2 */
        __(pop %edx)            /* arg_3 */
        __(mov %edi,%esp)
        __(pop %edi)            /* Return address pushed by caller */
        __(push %edx)
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

        __ifdef([WIN32])
_exportfn(C(restore_windows_context))
Xrestore_windows_context_start:
        __(hlt)
Xrestore_windows_context_load_rcx:                
        __(nop)
Xrestore_windows_context_iret:            
        __(iretl)
Xrestore_windows_context_end:             
        __(nop)
_endfn
	
_exportfn(C(windows_switch_to_foreign_stack))
        __(pop %eax)
        __(pop %ecx)            /* new %esp */
        __(pop %edx)            /* handler */
        __(pop %edx)            /* arg */
        __(movl %ecx,%esp)
        __(subl $0x10,%esp)
        __(movl %edx,(%esp))
        __(push %eax)
        __(jmp *%edx)
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

        __endif
	_endfile

