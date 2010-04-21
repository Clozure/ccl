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
#define PLATFORM_OS PLATFORM_OS_LINUX
#define PLATFORM_CPU PLATFORM_CPU_PPC
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_32

typedef struct ucontext ExceptionInformation;

#include "standard-inttypes.h"


#define MAXIMUM_MAPPABLE_MEMORY (1U<<30)
#define IMAGE_BASE_ADDRESS 0x31000000

#include "lisptypes.h"
#include "ppc-constants32.h"

/* xp accessors.  Logically identical on linuxppc32/64. */
#define XP_PTREGS(x) ((x)->uc_mcontext.regs)
#define xpGPRvector(x) ((natural *)(XP_PTREGS(x)))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define xpPC(x) (*((pc*)(&(XP_PTREGS(x)->nip))))
#define set_xpPC(x,new) (xpPC(x) = (pc)(new))
#define xpLR(x) (*((pc*)(&(XP_PTREGS(x)->link))))
#define xpCTR(x) (*(pc*)(&(XP_PTREGS(x)->ctr)))
#define xpXER(x) (XP_PTREGS(x)->xer)
#define xpCCR(x) (XP_PTREGS(x)->ccr)
#define xpMSR(x) (XP_PTREGS(x)->msr)
#define xpDSISR(x) (XP_PTREGS(x)->dsisr)
#define xpDAR(x) (XP_PTREGS(x)->dar)
#define xpTRAP(x) (XP_PTREGS(x)->trap)
#define xpFPSCR(x) (XP_PTREGS(x)->gpr[PT_FPSCR])
#define xpFPRvector(x) ((double *)(&(XP_PTREGS(x)->gpr[PT_FPR0])))
#define xpFPR(x,fprno) (xpFPRvector(x)[fprno])

/* 
   Work around a Darwin G5 bug (present in OSX 10.2.7, 10.2.8, and later
   versions.  See platform-darwinppc*.h for details
*/
#define DarwinSigReturn(context)
#define SIGRETURN(context)

#include "os-linux.h"
