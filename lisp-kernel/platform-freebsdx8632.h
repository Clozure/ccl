/*
   Copyright (C) 2010 Clozure Associates
   Copyright (C) 1994-2001 Digitool, Inc
   This file is part of Clozure CL.  

   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
   License , known as the LLGPL and distributed with Clozure CL as the
   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
   which is distributed with Clozure CL as the file "LGPL".  Where these
   conflict, the preamble takes precedence.  

   Clozure CL is referenced in the preamble as the "LIBRARY."

   The LLGPL is also available online at
   http://opensource.franz.com/preamble.html
*/

#define WORD_SIZE 32
#define PLATFORM_OS PLATFORM_OS_FREEBSD
#define PLATFORM_CPU PLATFORM_CPU_X86
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_32

typedef struct __ucontext ExceptionInformation;

#define MAXIMUM_MAPPABLE_MEMORY (1U<<30)
#define IMAGE_BASE_ADDRESS 0x30000000

#include "lisptypes.h"
#include "x86-constants32.h"

#define REG_EDI 5
#define REG_ESI 6
#define REG_EBP 7
#define REG_ISP 8
#define REG_EBX 9
#define REG_EDX 10
#define REG_ECX 11
#define REG_EAX 12
#define REG_EIP 15
#define REG_EFL 17
#define REG_ESP 18

#include <sys/types.h>
#include <machine/npx.h>
#define xpGPRvector(x) ((natural *)(&((x)->uc_mcontext)))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define eflags_register(xp) xpGPR(xp,Iflags)
#define xpPC(x) xpGPR(x,Iip)
#define xpMMXreg(x,n) *((natural *)(&(((struct savexmm *)(&(x)->uc_mcontext.mc_fpstate))->sv_fp[n])))
#define xpXMMregs(x)(&(((struct savexmm *)(&(x)->uc_mcontext.mc_fpstate))->sv_xmm[0]))
#define xpMXCSR(x) ((struct savefpu *)((x)->uc_mcontext.mc_fpstate)->sv_env.en_mxcsr)
extern void freebsd_sigreturn(ExceptionInformation *);
#define SIGNUM_FOR_INTN_TRAP SIGBUS
#define IS_MAYBE_INT_TRAP(info,xp) ((xp->uc_mcontext.mc_trapno == T_PROTFLT) && ((xp->uc_mcontext.mc_err & 7) == 2))
#define IS_PAGE_FAULT(info,xp) (xp->uc_mcontext.mc_trapno == T_PAGEFLT)
#define SIGRETURN(context) freebsd_sigreturn(context)

#define AVX_CONTEXT_PRESENT(xp) ((xp)->uc_mcontext.mc_trapno & 4)
#define AVX_CONTEXT_PTR(xp) (((xp)->uc_mcontext.mc_fpstate[130]))
#define AVX_CONTEXT_SIZE(xp) ((natural)((xp)->uc_mcontext.mc_fpstate[131]))

#include "os-freebsd.h"
