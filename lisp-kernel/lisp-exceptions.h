/*
 * Copyright 1994-2009 Clozure Associates
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef __lisp_exceptions_h__
#define __lisp_exceptions_h__ 1


#include <stdlib.h>
#include "memprotect.h"
#include "gc.h"

#ifdef WINDOWS
#include <windows.h>
#endif

typedef enum {
  kDebugger,
  kContinue,
  kExit
} ErrAction;


#ifdef WINDOWS
typedef EXCEPTION_RECORD siginfo_t;  /* Not even close to being the right thing to do */
#endif


void
zero_page(BytePtr);

void
zero_heap_segment(BytePtr);

extern protected_area_ptr AllProtectedAreas;

protected_area_ptr find_protected_area(BytePtr);

OSStatus
lisp_Debugger(ExceptionInformation *, siginfo_t *, int, Boolean, char *, ...);

OSStatus
handle_protection_violation(ExceptionInformation *, siginfo_t *, TCR *, int);

protected_area_ptr 
new_protected_area(BytePtr, BytePtr, lisp_protection_kind, natural, Boolean);

void
unprotect_area_prefix(protected_area_ptr, size_t);

void
protect_area_prefix(protected_area_ptr, size_t);

void
protect_area(protected_area_ptr);


Boolean
resize_dynamic_heap(BytePtr, natural);

OSStatus
PMCL_exception_handler(int, ExceptionInformation *, TCR *, siginfo_t *, int);

TCR*
get_tcr(Boolean);

ErrAction
error_action( void );

void
install_pmcl_exception_handlers(void);

void
unprotect_all_areas(void);

void
exception_cleanup(void);

void
exception_init();


#define debug_entry_exception 0
#define debug_entry_bug -1
#define debug_entry_dbg -2

#ifdef WINDOWS
#define ALLOW_EXCEPTIONS(context) // blank stare for now
#else
#define ALLOW_EXCEPTIONS(context) \
pthread_sigmask(SIG_SETMASK, (sigset_t *)(&context->uc_sigmask), NULL);
#endif

void
Fatal(StringPtr, StringPtr);

void fatal_oserr(StringPtr, OSErr);

void
non_fatal_error( char * );

void Bug(ExceptionInformation *, const char *format_string, ...);
void FBug(ExceptionInformation *, const char *format_string, ...);
signed_natural gc_from_xp(ExceptionInformation *, signed_natural);
signed_natural purify_from_xp(ExceptionInformation *, signed_natural);
signed_natural impurify_from_xp(ExceptionInformation *, signed_natural);



void
adjust_exception_pc(ExceptionInformation *, int);

size_t
symbol_name( unsigned, char *, size_t );


size_t
exception_fn_name( ExceptionInformation *, int, char *, size_t );



#ifdef PPC
#include "ppc-exceptions.h"
#endif

#ifdef X86
#include "x86-exceptions.h"
#endif

#ifdef ARM
#include "arm-exceptions.h"
#endif

#ifdef ARM64
#include "arm64-exceptions.h"
#endif

#ifdef DARWIN
void darwin_exception_init(TCR *tcr);
#endif

void thread_signal_setup(void);
void suspend_other_threads(Boolean);
void resume_other_threads(Boolean);
void reset_lisp_process(ExceptionInformation *);
void terminate_lisp(void);

#endif /* __lisp_exceptions_h__ */

