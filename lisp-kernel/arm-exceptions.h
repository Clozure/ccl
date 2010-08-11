/*
   Copyright (C) 2010 Clozure Associates
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
#define UUO_MASK 0x0ff000f0

#define IS_UUO(i) (((i) & UUO_MASK) == 0x07f000f0)
/* If an instruction is a UUO its format is determined by the low 4 bits */
#define UUO_FORMAT(i) ((i)&0xf)

#define UUO_UNARY_field(uuo) (((uuo)>>12)&0xff)
#define UUOA_field(uuo)      (((uuo)>>8) &0x0f)

#define uuo_format_nullary          0 /* 12 bits of code */
#define uuo_format_unary            1 /* 8 bits of info - NOT type info - 4-bit reg */
#define uuo_format_error_lisptag    2 /* 2 bits of lisptag info, 4-bit reg */
#define uuo_format_error_fulltag    3 /* 3 bits of fulltag info, 4 bit reg */

#define uuo_format_error_xtype      4 /* 8 bits of extended type/subtag info, 4 bit reg */
#define uuo_format_binary           7 /* 4 bits of code, r1, r0 */
#define uuo_format_nullary_error    8 /* nullary, call out to lisp */
#define uuo_format_unary_error      9 /* like unary, but call out to lisp */
#define uuo_format_cerror_lisptag  10 /* continuable, lisptag, reg */
#define uuo_format_cerror_fulltag  11 /* continuable, fulltag, reg */
#define uuo_format_cerror_xtype    12 /* continuable, xtype, reg */  
#define uuo_format_kernel_service  13 /* 8 bits of info */      
#define uuo_format_ternary         14 /* slot-unbound only */
#define uuo_format_binary_error    15 /* binary format, call out to lisp */




typedef u_int32_t opcode, *pc;

Boolean
handle_uuo(ExceptionInformation *, siginfo_t *, opcode);



int
callback_for_trap (LispObj, ExceptionInformation *, natural, natural, int*);

natural
register_codevector_contains_pc (natural, pc);

int
callback_to_lisp (LispObj, ExceptionInformation *, natural, natural, int*);

OSStatus
handle_trap(ExceptionInformation *, opcode, pc, siginfo_t *);


/* */

#define RN_field(i) (((i)>>16)&0xf)
#define RD_field(i) (((i)>>12)&0xf)
#define RM_field(i) ((i)&0xf)

#define IS_SUB_RM_FROM_ALLOCPTR(i)   (((i)&0x0ffff000) == 0x004cc000)
#define IS_SUB_FROM_ALLOCPTR(i)      (((i)&0x0ffff000) == 0x024cc000)
#define IS_SUB_LO_FROM_ALLOCPTR(i)   (((i)&0x0fffff00) == 0x024cc000)
#define IS_SUB_HI_FROM_ALLOCPTR(i)   (IS_SUB_FROM_ALLOCPTR(i) && \
                                     !(IS_SUB_LOW_FROM_ALLOCPTR(i)))
#define IS_LOAD_RD_FROM_ALLOCBASE(i) (((i)&0x0fff0fff) == \
                                      ( 0x05930000 | offsetof(tcr,allocbase)))
#define IS_COMPARE_ALLOCPTR_TO_RM(i) (((i)&0x0fffff0) == 0x0140c000)
#define IS_ALLOC_TRAP(i) (((i)&0x0fffffff) == 0x07f000f0)
#define IS_SET_ALLOCPTR_HEADER_RD(i) (((i)&0x0fff0fff) == \
                                      (0x050c0000 | (- misc_header_offset)))
/* The 1 here - and the 3 in the following definition - are based on
   the tagged offsets of cars and cdrs.  Fix these definitions of that ever
   changes ... */
#define IS_SET_ALLOCPTR_CDR_RD(i)    (((i)&0x0fff0fff) == 0x050c0001)
#define IS_SET_ALLOCPTR_CAR_RD(i)    (((i)&0x0fff0fff) == 0x058c0003)
#define IS_SET_ALLOCPTR_RESULT_RD(i) (((i)&0x0fff0fff) == 0x01a0000c)
#define IS_CLR_ALLOCPTR_TAG(i)       (((i)&0x0fffffff) == 0x03ccc007)


#define IS_GC_TRAP(i)                (((i)&0x0fffffff) == 0x07f002f0)
#define IS_DEBUG_TRAP(i)             (((i)&0x0fffffff) == 0x07f003f0)
#define IS_DEFERRED_INTERRUPT(i)     (((i)&0x0fffffff) == 0x07f004f0)
#define IS_DEFERRED_SUSPEND(i)       (((i)&0x0fffffff) == 0x07f005f0)

#define PSEUDO_SIGRETURN_UUO         (0xe7fffef1)

OSStatus
handle_error(ExceptionInformation *, unsigned, unsigned, int*);

typedef char* vector_buf;

void put_altivec_registers(vector_buf);
void get_altivec_registers(vector_buf);


int altivec_available;

#ifdef DARWIN
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/machine/thread_state.h>
#include <mach/machine/thread_status.h>

#endif



#ifdef DARWIN
#define SIGNAL_FOR_PROCESS_INTERRUPT SIGUSR1
#endif
#ifdef LINUX
#define SIGNAL_FOR_PROCESS_INTERRUPT SIGPWR
#endif



Boolean
extend_tcr_tlb(TCR *, ExceptionInformation *,  unsigned);

void 
pc_luser_xp(ExceptionInformation *, TCR *, signed_natural *);

#define codevec_hdr_p(value) ((value) == 0)

#ifdef __GNUC__
static __inline__ natural
ror(natural val, natural count) __attribute__((always_inline));

static __inline__ natural
ror(natural val,natural count)
{
  natural result;
  __asm__ __volatile__("ror %[result],%[val],%[count]"
                       :[result] "=r" (result)
                       :[val] "r" (val),
                        [count] "r" (count));
  return result;
}
#else
extern natural ror(natural, natural);
#endif

#ifdef DARWIN
#undef USE_SIGALTSTACK
#else
#define USE_SIGALTSTACK 1
#endif

#ifdef USE_SIGALTSTACK
void
invoke_handler_on_main_stack(int, siginfo_t*, ExceptionInformation *, void *, void*);
#endif

#ifdef USE_SIGALTSTACK
#define ALTSTACK(handler) altstack_ ## handler
#else
#define ALTSTACK(handler) handler
#endif
