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
#define PLATFORM_OS PLATFORM_OS_WINDOWS
#define PLATFORM_CPU PLATFORM_CPU_X86
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_32

#include <windows.h>
typedef CONTEXT ExceptionInformation;

#include "windows-inttypes.h"


#define MAXIMUM_MAPPABLE_MEMORY (1U<<30)
#define IMAGE_BASE_ADDRESS 0x04000000

#include "lisptypes.h"
#include "x86-constants32.h"

#define xpGPRvector(x) ((DWORD *)(&(x)->Edi))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define xpPC(x) xpGPR(x,Iip)
#define eflags_register(xp) xp->EFlags
#define xpFPRvector(x) ((natural *)(&(x->ExtendedRegisters[10*16])))
#define xpMMXreg(x,n)  (*((u64_t *)(&(x->FloatSave.RegisterArea[10*(n)]))))
#define xpMXCSRptr(x) (DWORD *)(&(x->ExtendedRegisters[24]))

#ifdef SOLARIS
#define SIGNUM_FOR_INTN_TRAP SIGSEGV
#ifdef X8664
#define IS_MAYBE_INT_TRAP(info,xp) ((xpGPR(xp,REG_TRAPNO)==0xd)&&((xpGPR(xp,REG_ERR)&7)==2))
#define IS_PAGE_FAULT(info,xp) (xpGPR(xp,REG_TRAPNO)==0xe)
#else
#define IS_MAYBE_INT_TRAP(info,xp) ((xpGPR(xp,TRAPNO)==0xd)&&((xpGPR(xp,ERR)&7)==2))
#define IS_PAGE_FAULT(info,xp) (xpGPR(xp,TRAPNO)==0xe)
#endif
#define SIGRETURN(context) setcontext(context)
#endif

#include "os-windows.h"
