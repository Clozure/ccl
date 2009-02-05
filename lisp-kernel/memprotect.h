/*
   Copyright (C) 1994-2001 Digitool, Inc
   This file is part of OpenMCL.  

   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
   License , known as the LLGPL and distributed with OpenMCL as the
   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
   which is distributed with OpenMCL as the file "LGPL".  Where these
   conflict, the preamble takes precedence.  

   OpenMCL is referenced in the preamble as the "LIBRARY."

   The LLGPL is also available online at
   http://opensource.franz.com/preamble.html
*/

#ifndef __memprotect_h__
#define __memprotect_h__



#include "lisptypes.h"
#ifdef PPC
#include "ppc-constants.h"
#endif
#include <signal.h>
#ifndef WINDOWS
#include <ucontext.h>
#endif

#ifdef WINDOWS
#define MAP_FAILED ((void *)(-1))

#define MEMPROTECT_NONE PAGE_NOACCESS
#define MEMPROTECT_RO   PAGE_READONLY
#define MEMPROTECT_RW   PAGE_READWRITE
#define MEMPROTECT_RX   PAGE_EXECUTE_READ
#define MEMPROTECT_RWX  PAGE_EXECUTE_READWRITE

#else

#define MEMPROTECT_NONE PROT_NONE
#define MEMPROTECT_RO   PROT_READ
#define MEMPROTECT_RW   (PROT_READ|PROT_WRITE)
#define MEMPROTECT_RX   (PROT_READ|PROT_EXEC)
#define MEMPROTECT_RWX  (PROT_READ|PROT_WRITE|PROT_EXEC)
#ifndef MAP_GROWSDOWN
#define MAP_GROWSDOWN (0)
#endif


#endif

LogicalAddress
ReserveMemoryForHeap(LogicalAddress want, natural totalsize);

int
CommitMemory (LogicalAddress start, natural len);

void
UnCommitMemory (LogicalAddress start, natural len);

LogicalAddress
MapMemory(LogicalAddress addr, natural nbytes, int protection);

LogicalAddress
MapMemoryForStack(natural nbytes);

int
UnMapMemory(LogicalAddress addr, natural nbytes);

int
ProtectMemory(LogicalAddress, natural);

int
UnProtectMemory(LogicalAddress, natural);

int
MapFile(LogicalAddress addr, natural pos, natural nbytes, int permissions, int fd);

typedef enum {
  kNotProtected,		/* At least not at the moment. */
  kVSPsoftguard,
  kSPsoftguard,
  kTSPsoftguard,
  kSPhardguard,			/* Touch one and die. */
  kVSPhardguard,
  kTSPhardguard,
  kHEAPsoft,			/* Uninitialized page in the heap */
  kHEAPhard,			/* The end-of-the-line in the heap */
  /* Phony last entry. */
  kNumProtectionKinds
  } lisp_protection_kind;

typedef
struct protected_area {
  struct protected_area *next;
  BytePtr start;                /* first byte (page-aligned) that might be protected */
  BytePtr end;                  /* last byte (page-aligned) that could be protected */
  unsigned nprot;               /* Might be 0 */
  unsigned protsize;            /* number of bytes to protect */
  lisp_protection_kind why;
} protected_area, *protected_area_ptr;


/* Various functions that try to respond to a protection violation */
typedef 
  OSStatus (protection_handler)(ExceptionInformation *, protected_area_ptr, BytePtr);

protection_handler 
  do_spurious_wp_fault,
  do_soft_stack_overflow,
  do_hard_stack_overflow,
  do_tenured_space_write,
  do_heap_soft_probe,
  do_heap_hard_probe;

extern protection_handler
  *protection_handlers[];


void
exception_cleanup(void);


  
#endif /* __memprotect_h__ */
