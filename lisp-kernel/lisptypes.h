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

#ifndef __lisptypes__
#define __lisptypes__

#include <sys/types.h>
#define WORD_SIZE 32
#ifdef PPC64
#undef WORD_SIZE
#define WORD_SIZE 64
#endif
#ifdef X8664
#undef WORD_SIZE
#define WORD_SIZE 64
#endif


#ifdef WINDOWS
#include <windows.h>
typedef long long s64_t;
typedef unsigned long long u64_t;
typedef signed long s32_t;
typedef unsigned long u32_t;
typedef signed short s16_t;
typedef unsigned short u16_t;
typedef signed char s8_t;
typedef unsigned char u8_t;
#else

#include <stdint.h>

#ifdef SOLARIS
/* Solaris doesn't laugh and play like the other children */
typedef int64_t s64_t;
typedef uint64_t u64_t;
typedef int32_t s32_t;
typedef uint32_t u32_t;
typedef int16_t s16_t;
typedef uint16_t u16_t;
typedef int8_t s8_t;
typedef uint8_t u8_t;
#else
typedef int64_t s64_t;
typedef u_int64_t u64_t;
typedef int32_t s32_t;
typedef u_int32_t u32_t;
typedef int16_t s16_t;
typedef u_int16_t u16_t;
typedef int8_t s8_t;
typedef u_int8_t u8_t;
#endif
#endif

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


#ifdef DARWIN
#include <sys/signal.h>
#include <sys/ucontext.h>

#ifdef PPC
#if WORD_SIZE == 64
#ifdef _STRUCT_UCONTEXT64
typedef _STRUCT_UCONTEXT64 ExceptionInformation;
typedef _STRUCT_MCONTEXT64 *MCONTEXT_T;
#else /* _STRUCT_UCONTEXT64 */
typedef struct ucontext64 ExceptionInformation;
typedef struct mcontext64 *MCONTEXT_T;
#endif /* _STRUCT_UCONTEXT64 */
#define UC_MCONTEXT(UC) UC->uc_mcontext64
#else /* WORD_SIZE */
#ifdef _STRUCT_UCONTEXT
typedef _STRUCT_UCONTEXT ExceptionInformation;
typedef _STRUCT_MCONTEXT *MCONTEXT_T;
#else
typedef struct ucontext ExceptionInformation;
typedef struct mcontext *MCONTEXT_T;
#endif
#define UC_MCONTEXT(UC) UC->uc_mcontext
#endif /* WORD_SIZE */



#endif /* PPC */

#ifdef X8664
/* Broken <i386/ucontext.h> in xcode 2.4 */
#ifndef _STRUCT_MCONTEXT64 /* A guess at what'll be defined when this is fixed */
struct mcontext64 {
	x86_exception_state64_t	__es;
	x86_thread_state64_t 	__ss;	
	x86_float_state64_t	__fs;
};

typedef struct mcontext64 *MCONTEXT_T;
typedef ucontext64_t ExceptionInformation;
#define UC_MCONTEXT(UC) UC->uc_mcontext64
#define __rax rax
#define __fpu_mxcsr fpu_mxcsr
#define __fpu_xmm0 fpu_xmm0
#define __rsp rsp
#define __faultvaddr faultvaddr
#define __err err
#define __rip rip
#define __rsi rsi
#define __rdi rdi
#define __rdx rdx
#define __rcx rcx
#define __r8 r8
#define __rflags rflags
#else
typedef mcontext_t MCONTEXT_T;
typedef ucontext_t ExceptionInformation;
#define UC_MCONTEXT(UC) UC->uc_mcontext
#endif /* _STRUCT_MCONTEXT64 */
#endif /* X86_64 */

#ifdef X8632
/* Assume rational <i386/ucontext.h> */
/* Sadly, we can't make that assumption, since Apple renamed things
   for Leopard. Yow!  Are we standards-compliant yet ? */
/* In the long term, we probably want to use the leopard-compliant
   names (with leading __ prefixes).  In the shorter term, we want
   kernels compiled on Leopard to run on Tiger (and not reference
   foo$UNIX2003 and similar nonsense, and that means getting the old
   names (without leading __ prefixes.)  Confused yet ? */

/* #if STILL_SUPPORT_TIGER */
#define __ss ss
#define __ds ds
#define __es es
#define __cs cs
#define __fs fs
#define __gs gs
#define __eax eax
#define __esp esp
#define __eip eip
#define __eflags eflags
#define __fpu_xmm0 fpu_xmm0
#define __fpu_mxcsr fpu_mxcsr
#define __fpu_stmm0 fpu_stmm0
#define __err err
#define __faultvaddr faultvaddr
/* #endif STILL_SUPPORT_TIGER */

#define UC_MCONTEXT(UC) UC->uc_mcontext
typedef mcontext_t MCONTEXT_T;
typedef ucontext_t ExceptionInformation;
#endif

#endif /* #ifdef DARWIN */

#ifdef LINUX
typedef struct ucontext ExceptionInformation;
#endif

#ifdef FREEBSD
typedef struct __ucontext ExceptionInformation;
#endif

#ifdef SOLARIS
typedef struct ucontext ExceptionInformation;
#endif

#ifdef WINDOWS
typedef CONTEXT ExceptionInformation;
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
