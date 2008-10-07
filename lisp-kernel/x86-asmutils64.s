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

/* Flush %carg1 cache lines, starting at address in %carg0.  Each line is */
/*   assumed to be %carg2 bytes wide. */
_exportfn(C(flush_cache_lines))
	__(cmpq $0,%carg1)
	__(jmp 2f)
1:	__(clflush (%carg0))
	__(addq %carg2,%carg0)
	__(subq $1,%carg1)
2:	__(jg 1b)	
	__(repret)
_endfn

_exportfn(C(current_stack_pointer))
	__(movq %rsp,%cret)
	__(ret)
_endfn

_exportfn(C(touch_page))
        __(movq %carg0,(%carg0))
        __(movq $0,(%carg0))
        __(movl $1,%cret_l)
        .globl C(touch_page_end)
C(touch_page_end):	
        __(ret)
                        
_exportfn(C(count_leading_zeros))
	__(bsrq %carg0,%cret)
	__(xorq $63,%cret)
	__(ret)
_endfn

_exportfn(C(noop))
	__(retq)
_endfn

_exportfn(C(set_mxcsr))
        __(pushq %carg0)
        __(ldmxcsr (%rsp))
        __(addq $8,%rsp)
        __(ret)
_endfn
	
_exportfn(C(get_mxcsr))
        __(pushq $0)
        __(stmxcsr (%rsp))
        __(popq %cret)
        __(ret)
_endfn

_exportfn(C(save_fp_context))
_endfn
        
_exportfn(C(restore_fp_context))
_endfn                        

/*  Atomically store new value (%carg2) in *%carg0, if old value == %carg1. */
/*  Return actual old value. */
_exportfn(C(store_conditional))
	__(mov %carg1,%cret)
	__(lock) 
        __(cmpxchgq %carg2,(%carg0))
	__(cmovne %carg2,%cret)
	__(ret)	
_endfn

/*	Atomically store new_value(%carg1) in *%carg0 ;  return previous contents */
/*	of *%carg0. */

_exportfn(C(atomic_swap))
	__(lock) 
        __(xchg %carg1,(%carg0))
	__(mov %carg1,%cret)
	__(ret)
_endfn

/*        Logior the value in *%carg0 with the value in %carg1 (presumably a */
/*	bitmask with exactly 1 bit set.)  Return non-zero if any of */
/*	the bits in that bitmask were already set. */
_exportfn(C(atomic_ior))
0:	__(movq (%carg0),%cret)
	__(movq %cret,%carg2)
	__(orq %carg1,%carg2)
	__(lock)
        __(cmpxchg %carg2,(%carg0))
        __(jnz 0b)
	__(andq %carg1,%cret)
	__(ret)
_endfn
        
        
/* Logand the value in *carg0 with the value in carg1 (presumably a bitmask with exactly 1 */
/* bit set.)  Return the value now in *carg0 (for some value of "now" */

_exportfn(C(atomic_and))
0:	__(movq (%carg0),%cret)
	__(movq %cret,%carg2)
	__(and %carg1,%carg2)
	__(lock)
        __(cmpxchg %carg2,(%carg0))
        __(jnz 0b)
	__(movq %carg2,%cret)
	__(ret)
_endfn


        __ifdef([DARWIN])
_exportfn(C(pseudo_sigreturn))
        __(hlt)
        __(jmp C(pseudo_sigreturn))
_endfn
        __endif                        

/* int cpuid (natural code, natural *pebx, natural *pecx, natural *pedx)  */
_exportfn(C(cpuid))
	__(pushq %carg2)
	__(pushq %carg3)
	__(movq %carg1, %ctemp0)
	__(pushq %rbx)		/* non-volatile reg, clobbered by CPUID */
	__(movq %carg0, %rax)
        __(xorq %rcx,%rcx)
	__(cpuid)
	__(movq %rbx,(%ctemp0))
	__(popq %rbx)
	__(popq %ctemp0)           /* recover pedx */
	__(movq %rdx,(%ctemp0))
	__(popq %ctemp0)		/* recover pecx */
	__(movq %rcx,(%ctemp0))
	__(ret)
_endfn

/* switch_to_foreign_stack(new_sp, func, arg_0, arg_1, arg_2, arg_3)  */
/*   Not fully general, but should get us off of the signal stack */
_exportfn(C(switch_to_foreign_stack))
	__ifdef([WINDOWS])
	__(movq 8(%rsp), %ctemp0)
	__(movq 16(%rsp), %ctemp1)
	__endif
	__(movq %carg0,%rsp)
	__(movq %carg1,%rax)
	__(movq %carg2,%carg0)
	__(movq %carg3,%carg1)
	__ifdef([WINDOWS])
	__(movq %ctemp0, %carg2)
	__(movq %ctemp1, %carg3)
	__else
	__(movq %carg4,%carg2)
	__(movq %carg5,%carg3)
	__endif
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
/* Check (in an ugly, non-portable way) to see if %gs is addressing
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

        __ifdef([WIN_64])
/* %rcx = CONTEXT, %rdx = tcr, %r8 = old_valence.  This pretty
   much has to be uninterruptible */        
_exportfn(C(restore_windows_context))
Xrestore_windows_context_start: 	
        __(subq $0x38,%rsp)
        __(xorl %eax,%eax)
        __(movq %r8,tcr.valence(%rdx))
        __(movq %rax,tcr.pending_exception_context(%rdx))
        __(fxrstor win64_context.fpstate(%rcx))
        __(movapd win64_context.Xmm0(%rcx),%xmm0)
        __(movapd win64_context.Xmm1(%rcx),%xmm1)
        __(movapd win64_context.Xmm2(%rcx),%xmm2)
        __(movapd win64_context.Xmm3(%rcx),%xmm3)
        __(movapd win64_context.Xmm4(%rcx),%xmm4)
        __(movapd win64_context.Xmm5(%rcx),%xmm5)
        __(movapd win64_context.Xmm6(%rcx),%xmm6)
        __(movapd win64_context.Xmm7(%rcx),%xmm7)
        __(movapd win64_context.Xmm8(%rcx),%xmm8)
        __(movapd win64_context.Xmm9(%rcx),%xmm9)
        __(movapd win64_context.Xmm10(%rcx),%xmm10)
        __(movapd win64_context.Xmm11(%rcx),%xmm11)
        __(movapd win64_context.Xmm12(%rcx),%xmm12)
        __(movapd win64_context.Xmm13(%rcx),%xmm13)
        __(movapd win64_context.Xmm14(%rcx),%xmm14)
        __(movapd win64_context.Xmm15(%rcx),%xmm15)
        __(ldmxcsr win64_context.MxCsr(%rcx))
        __(movw win64_context.SegSs(%rcx),%ax)
        __(movw %ax,0x20(%rsp))
        __(movq win64_context.Rsp(%rcx),%rax)
        __(movq %rax,0x18(%rsp))
        __(movl win64_context.EFlags(%rcx),%eax)
        __(movl %eax,0x10(%rsp))
        __(movw win64_context.SegCs(%rcx),%ax)
        __(movw %ax,8(%rsp))
        __(movq win64_context.Rip(%rcx),%rax)
        __(movq %rax,(%rsp))
        __(movq win64_context.Rax(%rcx),%rax)
        __(movq win64_context.Rbx(%rcx),%rbx)
        __(movq win64_context.Rdx(%rcx),%rdx)
        __(movq win64_context.Rdi(%rcx),%rdi)
        __(movq win64_context.Rsi(%rcx),%rsi)
        __(movq win64_context.Rbp(%rcx),%rbp)
        __(movq win64_context.R8(%rcx),%r8)
        __(movq win64_context.R9(%rcx),%r9)
        __(movq win64_context.R10(%rcx),%r10)
        __(movq win64_context.R11(%rcx),%r11)
        __(movq win64_context.R12(%rcx),%r12)
        __(movq win64_context.R13(%rcx),%r13)
        __(movq win64_context.R14(%rcx),%r14)
        __(movq win64_context.R15(%rcx),%r15)
Xrestore_windows_context_load_rcx:                
        __(movq win64_context.Rcx(%rcx),%rcx)
Xrestore_windows_context_iret:            
        __(iretq)
Xrestore_windows_context_end:             
        __(nop)
_endfn
	
_exportfn(C(windows_switch_to_foreign_stack))
        __(pop %rax)
        __(lea -0x20(%rcx),%rsp)
        __(push %rax)
        __(movq %r8,%rcx)
        __(jmp *%rdx)
_endfn        

        .data
        .globl C(restore_windows_context_start)
        .globl C(restore_windows_context_end)
        .globl C(restore_windows_context_load_rcx)
        .globl C(restore_windows_context_iret)
C(restore_windows_context_start):  .quad Xrestore_windows_context_start
C(restore_windows_context_end): .quad Xrestore_windows_context_end
C(restore_windows_context_load_rcx):  .quad Xrestore_windows_context_load_rcx
C(restore_windows_context_iret): .quad Xrestore_windows_context_iret
        .text

/* Something that we shouldn't return to */
_exportfn(C(windows_halt))
        __(hlt)
_endfn         
_exportfn(C(ensure_safe_for_string_operations))
        __(cld)
        __(ret)
_endfn                                       
        __endif
	_endfile
