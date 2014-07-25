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
#define PLATFORM_OS PLATFORM_OS_SOLARIS
#define PLATFORM_CPU PLATFORM_CPU_X86
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_32

typedef struct ucontext ExceptionInformation;

#define MAXIMUM_MAPPABLE_MEMORY (1U<<30)
#define IMAGE_BASE_ADDRESS 0x10000000

#include "lisptypes.h"
#include "x86-constants32.h"

#include <sys/regset.h>
#include <limits.h>
#define REG_EAX EAX
#define REG_EBX EBX
#define REG_ECX ECX
#define REG_EDX EDX
#define REG_ESI ESI
#define REG_EDI EDI
#define REG_EBP EBP
#define REG_ESP UESP    /* Maybe ... ESP is often 0, but who knows why ? */
#define REG_EFL EFL
#define REG_EIP EIP

#define xpGPRvector(x) ((x)->uc_mcontext.gregs)
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define xpPC(x) xpGPR(x,Iip)
#define eflags_register(xp) xpGPR(xp,Iflags)
#define xpXMMregs(x)(&((x)->uc_mcontext.fpregs.fp_reg_set.fpchip_state.xmm[0]))
#define xpMMXreg(x,n)*(natural *)(&(((struct fnsave_state *)(&(((x)->uc_mcontext.fpregs))))->f_st[n]))
#define xmMXCSR(x) ((x)->uc_mcontext.fpregs.fp_reg_set.fpchip_state.mxcsr)
#define SIGNUM_FOR_INTN_TRAP SIGSEGV
#define IS_MAYBE_INT_TRAP(info,xp) ((xpGPR(xp,TRAPNO)==0xd)&&((xpGPR(xp,ERR)&7)==2))
#define IS_PAGE_FAULT(info,xp) (xpGPR(xp,TRAPNO)==0xe)
#define SIGRETURN(context) setcontext(context)

#include "os-solaris.h"
