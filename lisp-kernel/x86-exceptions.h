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





/* Please go away. */
#ifdef DARWIN_GS_HACK
extern Boolean ensure_gs_pthread(void);
extern void set_gs_address(void *);
#endif


/* sigaltstack isn't thread-specific on The World's Most Advanced OS */
#ifdef DARWIN
#undef USE_SIGALTSTACK
#else
#ifdef WINDOWS
#undef USE_SIGALTSTACK
#else
#define USE_SIGALTSTACK 1
#endif
#endif

#ifdef USE_SIGALTSTACK
void setup_sigaltstack(area *);
#endif

/* recognizing the function associated with a tagged return address */
/* now involves recognizinig an "(lea (@ disp (% rip)) (% rn))" */
/* instruction at the tra */

#define RECOVER_FN_FROM_RIP_LENGTH 7 /* the instruction is 7 bytes long */
#define RECOVER_FN_FROM_RIP_DISP_OFFSET 3 /* displacement word is 3 bytes in */
#define RECOVER_FN_FROM_RIP_WORD0 0x8d4c /* 0x4c 0x8d, little-endian */
#define RECOVER_FN_FROM_RIP_BYTE2 0x2d  /* third byte of opcode */

extern natural get_mxcsr();
extern void set_mxcsr(natural);
void enable_fp_exceptions(void);

#ifdef X8632
/* The 32-bit immediate value in the instruction
 * "(mov ($ 0x12345678) (% fn))" at a tagged return address
 * refers to the associated function.
 */
#define RECOVER_FN_OPCODE 0xbf
#define RECOVER_FN_LENGTH 5
#endif

void callback_for_gc_notification(ExceptionInformation *xp, TCR *tcr);

#endif /* X86_EXCEPTIONS_H */

