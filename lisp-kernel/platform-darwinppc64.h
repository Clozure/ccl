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


/* ucontext/mcontext stuff; saner if OS >= 10.5 */
typedef struct ucontext64 ExceptionInformation;
typedef struct mcontext64 MCONTEXT_T;
#define UC_MCONTEXT(UC) UC->uc_mcontext

/* Define "standard" C integer types.  There are lots of standards; we
   basically want to define signed/unsigned 8/16/32/64-bit integer
   types (s8_t, u32_t) with names that we can use consistently in
   this code.  (We may or may not actually use them consistently.)
 */

#include "standard-inttypes.h"
