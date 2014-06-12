/*
   Copyright (C) 2009 Clozure Associates
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

#ifndef __lisp__
#define __lisp__

/* a platform-specific header file is pre-included via -include */

#ifndef LOWMEM_BIAS
#define LOWMEM_BIAS (0)
#endif


#include "macros.h"

extern int page_size, log2_page_size;

static inline natural
_align_to_power_of_2(natural n, unsigned power)
{
  natural align = (1<<power) -1;

  return (n+align) & ~align;
}

#define align_to_power_of_2(n,p) _align_to_power_of_2(((natural)(n)),p)

static inline natural
_truncate_to_power_of_2(natural n, unsigned power)
{
  return n & ~((1<<power) -1);
}

#define truncate_to_power_of_2(n,p) _truncate_to_power_of_2((natural)(n),p)

LispObj start_lisp(TCR*, LispObj);

size_t
ensure_stack_limit(size_t);

char *
print_lisp_object(LispObj);

#include "kernel-globals.h"

#define PLATFORM_WORD_SIZE_32 0
#define PLATFORM_WORD_SIZE_64 64
#define PLATFORM_CPU_PPC (0<<3)
#define PLATFORM_CPU_SPARC (1<<3)
#define PLATFORM_CPU_X86 (2<<3)
#define PLATFORM_CPU_ARM (3<<3)
#define PLATFORM_OS_VXWORKS 0
#define PLATFORM_OS_LINUX 1
#define PLATFORM_OS_SOLARIS 2
#define PLATFORM_OS_DARWIN 3
#define PLATFORM_OS_FREEBSD 4
#define PLATFORM_OS_WINDOWS 5
#define PLATFORM_OS_ANDROID 6



#define PLATFORM (PLATFORM_OS|PLATFORM_CPU|PLATFORM_WORD_SIZE)

#ifdef WINDOWS
Boolean check_for_embedded_image (wchar_t *);
#else
Boolean check_for_embedded_image (char *);
#endif
natural xStackSpace();
void init_threads(void *, TCR *);

#ifdef WINDOWS
void wperror(char *);
#endif

void ensure_static_conses(ExceptionInformation *, TCR *,natural);

#include <stdio.h>

extern FILE *dbgout;

#define RESERVE_FOR_LISP 1
#define ON_ALTSTACK 2
#define RESTART_SYSCALLS 4

extern void
install_signal_handler(int signo, void *handler, unsigned flags);

extern void make_dynamic_heap_executable(void *, void *);
extern void xMakeDataExecutable(BytePtr, natural);
extern void lower_heap_start(BytePtr, area*);

#endif /* __lisp__ */
