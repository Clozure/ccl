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

#define WORD_SIZE 64
#define PLATFORM_OS PLATFORM_OS_DARWIN
#define PLATFORM_CPU PLATFORM_CPU_X86
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_64


#include <sys/signal.h>
#include <sys/ucontext.h>

/* ucontext/mcontext stuff; saner if OS >= 10.5 */
typedef mcontext_t MCONTEXT_T;
typedef ucontext_t ExceptionInformation;
#define UC_MCONTEXT(UC) UC->uc_mcontext

#include "standard-inttypes.h"

#define MAXIMUM_MAPPABLE_MEMORY (512L<<30L)
#define IMAGE_BASE_ADDRESS 0x300000000000L

#include "lisptypes.h"
#include "x86-constants64.h"

#define REG_RAX 0
#define REG_RBX 1
#define REG_RCX 2
#define REG_RDX 3
#define REG_RDI 4
#define REG_RSI 5
#define REG_RBP 6
#define REG_RSP 7
#define REG_R8 8
#define REG_R9 9
#define REG_R10 10
#define REG_R11 11
#define REG_R12 12
#define REG_R13 13
#define REG_R14 14
#define REG_R15 15
#define REG_RIP 16
#define REG_RFL 17

extern void darwin_sigreturn(ExceptionInformation *,unsigned);

/* xp accessors, sigreturn stuff */
#define DarwinSigReturn(context) do {\
    darwin_sigreturn(context, 0x1e);                 \
    Bug(context,"sigreturn returned");\
  } while (0)

#define xpGPRvector(x) ((natural *)(&(UC_MCONTEXT(x)->__ss)))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define xpPC(x) (xpGPR(x,Iip))
#define eflags_register(xp) xpGPR(xp,Iflags)
#define xpFPRvector(x) ((natural *)(&(UC_MCONTEXT(x)->__fs.__fpu_xmm0)))
#define xpMMXvector(x) (&(UC_MCONTEXT(x)->__fs.__fpu_stmm0))
/* Note that this yields only the lower half of the MMX reg on x8632 */
#define xpMMXreg(x,n) *(natural *)&(xpMMXvector(x)[n])
#define xpMXCSR(x) (UC_MCONTEXT(x)->__fs.__fpu_mxcsr)
#define SIGNUM_FOR_INTN_TRAP SIGSEGV /* Not really, but our Mach handler fakes that */
#define IS_MAYBE_INT_TRAP(info,xp) ((UC_MCONTEXT(xp)->__es.__trapno == 0xd) && (((UC_MCONTEXT(xp)->__es.__err)&7)==2))
#define IS_PAGE_FAULT(info,xp) (UC_MCONTEXT(xp)->__es.__trapno == 0xe)
/* The x86 version of sigreturn just needs the context argument; the
   hidden, magic "flavor" argument that sigtramp uses is ignored. */
#define SIGRETURN(context) DarwinSigReturn(context)


#include "os-darwin.h"


#define SEPARATE_ALTSTACK 1
