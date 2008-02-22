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

/* Flush %rsi cache lines, starting at address in %rdi.  Each line is */
/*   assumed to be %rdx bytes wide. */
_exportfn(C(flush_cache_lines))
	__(cmpq $0,%rsi)
	__(jmp 2f)
1:	__(clflush (%rdi))
	__(addq %rdx,%rdi)
	__(subq $1,%rsi)
2:	__(jg 1b)	
	__(repret)
_endfn

_exportfn(C(current_stack_pointer))
	__(movq %rsp,%rax)
	__(ret)
_endfn

_exportfn(C(touch_page))
        __(movq %rdi,(%rdi))
        __(movq $0,(%rdi))
        __(movl $1,%eax)
        .globl C(touch_page_end)
C(touch_page_end):	
        __(ret)
                        
_exportfn(C(count_leading_zeros))
	__(bsrq %rdi,%rax)
	__(xorq $63,%rax)
	__(ret)
_endfn

_exportfn(C(noop))
	__(retq)
_endfn

_exportfn(C(set_mxcsr))
        __(pushq %rdi)
        __(ldmxcsr (%rsp))
        __(addq $8,%rsp)
        __(ret)
_endfn
	
_exportfn(C(get_mxcsr))
        __(pushq $0)
        __(stmxcsr (%rsp))
        __(popq %rax)
        __(ret)
_endfn

_exportfn(C(save_fp_context))
_endfn
        
_exportfn(C(restore_fp_context))
_endfn                        

/*  Atomically store new value (%rdx) in *%rdi, if old value == %rsi. */
/*  Return actual old value. */
_exportfn(C(store_conditional))
	__(mov %rsi,%rax)
	__(lock) 
        __(cmpxchgq %rdx,(%rdi))
	__(cmovne %rdx,%rax)
	__(ret)	
_endfn

/*	Atomically store new_value(%rsi) in *%rdi ;  return previous contents */
/*	of *%rdi. */

_exportfn(C(atomic_swap))
	__(lock) 
        __(xchg %rsi,(%rdi))
	__(mov %rsi,%rax)
	__(ret)
_endfn

/*        Logior the value in *%rdi with the value in %rsi (presumably a */
/*	bitmask with exactly 1 bit set.)  Return non-zero if any of */
/*	the bits in that bitmask were already set. */
_exportfn(C(atomic_ior))
0:	__(movq (%rdi),	%rax)
	__(movq %rax,%rcx)
	__(orq %rsi,%rcx)
	__(lock)
        __(cmpxchg %rcx,(%rdi))
        __(jnz 0b)
	__(andq %rsi,%rax)
	__(ret)
_endfn
        
        
/* Logand the value in *rdi with the value in rsi (presumably a bitmask with exactly 1 */
/* bit set.)  Return the value now in *rdi (for some value of "now" */

_exportfn(C(atomic_and))
0:	__(movq (%rdi),	%rax)
	__(movq %rax,%rcx)
	__(and %rsi,%rcx)
	__(lock)
        __(cmpxchg %rcx,(%rdi))
        __(jnz 0b)
	__(movq %rcx,%rax)
	__(ret)
_endfn


        __ifdef([DARWIN])
_exportfn(C(pseudo_sigreturn))
        __(hlt)
        __(jmp C(pseudo_sigreturn))
_endfn
        __endif                        

/* int cpuid (int code, int *pebx, int *pecx, int *pedx)  */
/* UNIX	          %rdi,     %rsi,      %rdx,      %rcx    	     */
/* WIN		  %ecx,     %rdx,      %r8,       %r9 */    
_exportfn(C(cpuid))
	__ifdef([WINDOWS])
	__(pushq %r8)		/* pecx */
	__(pushq %r9)		/* pedx */
	__(movq %rdx, %rsi)     /* pebx */
	__(pushq %rbx)		/* %rbx is non-volatile */
	__(xorq %rax, %rax)
	__(movl %ecx,%eax)
	__else
	__(pushq %rdx)		/* pecx */
	__(pushq %rcx)		/* pedx */
	__(pushq %rbx)		/* %rbx is non-volatile */
	__(movq %rdi,%rax)
	__endif
        __(xorl %ecx,%ecx)
	__(cpuid)
	__(movl %ebx,(%rsi))
	__(popq %rbx)
	__(popq %rsi)           /* recover pedx */
	__(movl %edx,(%rsi))
	__(popq %rsi)		/* recover pecx */
	__(movl %ecx,(%rsi))
	__(ret)
_endfn

/* switch_to_foreign_stack(new_sp, func, arg_0, arg_1, arg_2, arg_3)  */
/*   Not fully general, but should get us off of the signal stack */
_exportfn(C(switch_to_foreign_stack))
	__(movq %rdi,%rsp)
	__(movq %rsi,%rax)
	__(movq %rdx,%rdi)
	__(movq %rcx,%rsi)
	__(movq %r8,%rdx)
	__(movq %r9,%rcx)
	__(jmp *%rax)
_endfn

_exportfn(C(freebsd_sigreturn))
	__(movl $417,%eax)	/* SYS_sigreturn */
	__(syscall)				
_exportfn(C(get_vector_registers))
_endfn

_exportfn(C(put_vector_registers))
_endfn				
	
        
        __ifdef([DARWIN_GS_HACK])
/* Check (in and ugly, non-portable way) to see if %gs is addressing
   pthreads data.  If it was, return 0; otherwise, assume that it's
   addressing a lisp tcr and set %gs to point to the tcr's tcr.osid,
   then return 1. */
	
thread_signature = 0x54485244 /* 'THRD' */
	
_exportfn(C(ensure_gs_pthread))
        __(cmpl $thread_signature,%gs:0)
        __(movl $0,%eax)
        __(je 9f)
        __(movq %gs:tcr.osid,%rdi)
        __(movl $0x3000003,%eax)
        __(syscall)
        __(movl $1,%eax)
9:      __(repret)
_endfn

        /* Ensure that %gs addresses the linear address in %rdi */
        /* This incidentally returns the segment selector .*/
_exportfn(C(set_gs_address))
        __(movl $0x3000003,%eax)
        __(syscall)
        __(ret)
_endfn
        __endif		
	_endfile
