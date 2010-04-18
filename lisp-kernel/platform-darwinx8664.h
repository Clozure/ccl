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

/* xp accessors, sigreturn stuff */
#define DARWIN_USE_PSEUDO_SIGRETURN 1
#define DarwinSigReturn(context) do {\
    darwin_sigreturn(context);\
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
#define SIGNUM_FOR_INTN_TRAP SIGSEGV /* Not really, but our Mach handler fakes that */
#define IS_MAYBE_INT_TRAP(info,xp) ((UC_MCONTEXT(xp)->__es.__trapno == 0xd) && (((UC_MCONTEXT(xp)->__es.__err)&7)==2))
#define IS_PAGE_FAULT(info,xp) (UC_MCONTEXT(xp)->__es.__trapno == 0xe)
/* The x86 version of sigreturn just needs the context argument; the
   hidden, magic "flavor" argument that sigtramp uses is ignored. */
#define SIGRETURN(context) DarwinSigReturn(context)

#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/machine/thread_state.h>
#include <mach/machine/thread_status.h>

pthread_mutex_t *mach_exception_lock;

#include "os-darwin.h"
