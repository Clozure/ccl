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
#define PLATFORM_OS PLATFORM_OS_WINDOWS
#define PLATFORM_CPU PLATFORM_CPU_X86
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_64

#include <windows.h>

typedef CONTEXT ExceptionInformation;

#include "windows-inttypes.h"

/* Supposedly, the high-end version of Vista allow 128GB of pageable memory */
#define MAXIMUM_MAPPABLE_MEMORY (512LL<<30LL)
#define IMAGE_BASE_ADDRESS 0x100000000LL

#include "lisptypes.h"
#include "x86-constants64.h"

#define xpGPRvector(x) ((DWORD64 *)(&(x)->Rax))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define xpPC(x) xpGPR(x,Iip)
#define eflags_register(xp) xp->EFlags
#define xpMXCSRptr(x) (DWORD *)(&(x->MxCsr))
#define xpMXCSR(x) ((x)->MxCsr)

#define SIGNUM_FOR_INTN_TRAP SIGSEGV /* Also fake */
#define IS_MAYBE_INT_TRAP(info,xp) \
  ((info->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) &&       \
   (info->ExceptionInformation[0]==0) &&                       \
   (info->ExceptionInformation[1]==(ULONG_PTR)(-1L)))
#define IS_PAGE_FAULT(info,xp) (1)
#define SIGRETURN(context)      /* for now */

#include "os-windows.h"
