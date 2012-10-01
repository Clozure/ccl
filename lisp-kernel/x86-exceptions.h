/*
   Copyright (C) 2005-2009 Clozure Associates
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

#ifndef X86_EXCEPTIONS_H
#define X86_EXCEPTIONS_H 1

typedef u8_t opcode, *pc;

void switch_to_foreign_stack(void*, ...);

#define INTN_OPCODE 0xcd

#define UUO_GC_TRAP    0xc4
#define UUO_ALLOC_TRAP 0xc5
#define UUO_DEBUG_TRAP 0xca
#define UUO_DEBUG_TRAP_WITH_STRING 0xcd
#define UUO_WATCH_TRAP 0xce
  #define WATCH_TRAP_FUNCTION_WATCH 0
  #define WATCH_TRAP_FUNCTION_UNWATCH 1

#define XUUO_OPCODE_0 0x0f
#define XUUO_OPCODE_1 0x0b

#define XUUO_TLB_TOO_SMALL 1
#define XUUO_INTERRUPT_NOW 2
#define XUUO_SUSPEND_NOW 3
#define XUUO_INTERRUPT 4
#define XUUO_SUSPEND 5
#define XUUO_SUSPEND_ALL 6
#define XUUO_RESUME 7
#define XUUO_RESUME_ALL 8
#define XUUO_KILL 9
#define XUUO_ALLOCATE_LIST 10

int callback_to_lisp (TCR *tcr, LispObj callback_macptr, ExceptionInformation *xp,
		      natural arg1, natural arg2, natural arg3, natural arg4,
		      natural arg5);


void
pc_luser_xp(ExceptionInformation*, TCR*, signed_natural*);


typedef enum {
  ID_unrecognized_alloc_instruction,
  ID_load_allocptr_reg_from_tcr_save_allocptr_instruction,
  ID_compare_allocptr_reg_to_tcr_save_allocbase_instruction,
  ID_branch_around_alloc_trap_instruction,
  ID_alloc_trap_instruction,
  ID_set_allocptr_header_instruction,
  ID_clear_tcr_save_allocptr_tag_instruction
} alloc_instruction_id;

/* sigaltstack isn't thread-specific on The World's Most Advanced OS */
#ifdef WINDOWS
#undef USE_SIGALTSTACK
#else
#define USE_SIGALTSTACK 1
#endif

#ifdef USE_SIGALTSTACK
void setup_sigaltstack(area *);
#ifdef DARWIN
/* Apple's sigaltstack mechanism is still broken; a thread is considered
to be running on the alternate signal stack until a signal handler called
on the altstack returns.  (OSX is too advanced to look at the damned
stack pointer ...)  We can reset a thread's notion of whether or not it's
on the altstack by calling sigreturn with a second argument of #x80000000
(the first argument is ignored in this case.)

This is supported thru 10.8.1; hopefully, it'll be ignored if Apple ever
fixes their sigaltstack implementation.

Recall that in 10.4 and earlier, Apple's sigaltstack implementation tried
to make all threads use the same alternate stack (whatever one was most
recently specified in a call to sigaltstack()), so this nonsense is actually
a step forward.  A sysctl was supported thru 10.6.8 (at least) to revert
to this behavior, which was presumably useful to people who wanted to see
what would happen if multiple threads ran signal handlers on the same stack
at the same time.

(Whenever I rant about this sort of thing, I feel like a political satirist
during the Bush administration: every day new material is written for you,
and it's much better than anything that you could invent yourself.)
*/
#define ResetAltStack() darwin_sigreturn(NULL, 0x80000000)
#endif
#endif

#ifndef ResetAltStack
#define ResetAltStack()
#endif

extern natural get_mxcsr();
extern void set_mxcsr(natural);
void enable_fp_exceptions(void);

void callback_for_gc_notification(ExceptionInformation *xp, TCR *tcr);



#endif /* X86_EXCEPTIONS_H */

