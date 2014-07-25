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
#define PLATFORM_CPU PLATFORM_CPU_ARM
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_32

typedef struct ucontext ExceptionInformation;

#define MAXIMUM_MAPPABLE_MEMORY (3<<29)
#define IMAGE_BASE_ADDRESS 0x10000000

#include "lisptypes.h"
#include "arm-constants.h"

/* xp accessors */
#define xpGPRvector(x) ((natural *)&((x)->uc_mcontext.arm_r0))
#define xpGPR(x,gprno) (xpGPRvector(x))[gprno]
#define xpPC(x) (*((pc*)(&(xpGPR(x,15)))))
#define xpLR(x) (*((pc*)(&(xpGPR(x,14)))))
#define xpPSR(x) ((x)->uc_mcontext.arm_cpsr)
#define xpFaultAddress(x) ((x)->uc_mcontext.fault_address)
#define xpTRAP(x) ((x)->uc_mcontext.trap_no)
#define xpERROR(x) ((x)->uc_mcontext.error_code)
#define xpFaultStatus(x) xpERROR(x)

#define DarwinSigReturn(context)
#define SIGRETURN(context)

#include "os-linux.h"

#define PROTECT_CSTACK 1

