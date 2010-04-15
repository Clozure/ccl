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
#define PLATFORM_OS PLATFORM_OS_DARWIN
#define PLATFORM_CPU PLATFORM_CPU_X86
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_32

/* ucontext/mcontext stuff; saner if OS >= 10.5 */
#include <sys/signal.h>
#include <sys/ucontext.h>

#define UC_MCONTEXT(UC) UC->uc_mcontext
typedef mcontext_t MCONTEXT_T;
typedef ucontext_t ExceptionInformation;

#include "standard-inttypes.h"

#define MAXIMUM_MAPPABLE_MEMORY ((1U<<31)-2*heap_segment_size)
#define IMAGE_BASE_ADDRESS 0x04000000

#include "lisptypes.h"
#include "x86-constants32.h"
