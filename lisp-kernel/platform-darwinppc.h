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
