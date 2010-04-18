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

#define WORD_SIZE 3
#define PLATFORM_OS PLATFORM_OS_DARWIN
#define PLATFORM_CPU PLATFORM_CPU_PPC
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_32

#include <sys/signal.h>
#include <sys/ucontext.h>

/* ucontext/mcontext stuff; saner if OS >= 10.5 */
typedef ucontext_t ExceptionInformation;
typedef mcontext_t MCONTEXT_T;
#define UC_MCONTEXT(UC) UC->uc_mcontext

#include "standard-inttypes.h"

#define MAXIMUM_MAPPABLE_MEMORY ((1U<<31)-2*heap_segment_size)
#define IMAGE_BASE_ADDRESS 0x04000000

#include "lisptypes.h"
#include "ppc-constants32.h"

/* xp accessors.  Logically identical on darwinppc32/64. */
#define xpGPRvector(x) (&(UC_MCONTEXT(x)->__ss.__r0))
#define xpGPR(x,gprno) ((xpGPRvector(x))[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (UInt32)(new)
#define xpPC(x) (*((pc*) &(UC_MCONTEXT(x)->__ss.__srr0)))
#define set_xpPC(x,new) (xpPC(x) = (pc)(new))
#define xpLR(x) (*((pc*)(&(UC_MCONTEXT(x)->__ss.__lr))))
#define xpCTR(x) (*(pc*)(&(UC_MCONTEXT(x)->__ss.__ctr)))
#define xpXER(x) (UC_MCONTEXT(x)->__ss.__xer)
#define xpCCR(x) (UC_MCONTEXT(x)->__ss.__cr)
#define xpMSR(x) (UC_MCONTEXT(x)->__ss.__srr1)
#define xpDSISR(x) (UC_MCONTEXT(x)->__es.__dsisr)
#define xpDAR(x) (UC_MCONTEXT(x)->__es.__dar)
#define xpTRAP(x) (UC_MCONTEXT(x)->__es.__exception)
#define xpFPSCR(x) (UC_MCONTEXT(x)->__fs.__fpscr)
#define xpFPRvector(x) (UC_MCONTEXT(x)->__fs.__fpregs)
#define xpFPR(x,fprno) (xpFPRvector(x)[fprno])

/* Late versions of OSX 10.2 had a bug in 32-bit exception handling;
   machine context wasn't restored correctly if it wasn't modified
   by the exception handler.  DarwinSigReturn() was a macro that 
   tried to work around this.
*/
#define DarwinSigReturn(x)

/* On some platforms, we may need to do something more than returning
   from a signal handler in order to return to the kernel and let it
   restore context.  On DarwinPPC, that's not a factor.
*/
#define SIGRETURN(context)

#include "os-darwin.h"
