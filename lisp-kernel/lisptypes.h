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

#ifndef __lisptypes__
#define __lisptypes__


#if WORD_SIZE == 64
typedef u64_t LispObj;
typedef u64_t natural;
typedef s64_t signed_natural;
typedef u64_t unsigned_of_pointer_size;
#else
typedef u32_t LispObj;
typedef u32_t natural;
typedef s32_t signed_natural;
typedef u32_t unsigned_of_pointer_size;
#endif


typedef u32_t lisp_char_code;

typedef int OSStatus, OSErr;
#define noErr ((OSErr) 0)
typedef int Boolean;
typedef void *LogicalAddress;
typedef char *Ptr, *BytePtr, *StringPtr;
typedef unsigned int UInt32;



#define true 1
#define false 0

#endif /*__lisptypes__ */
