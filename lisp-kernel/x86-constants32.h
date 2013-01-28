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

#include "x86-constants.h"

/* Indicies of GPRs in the mcontext component of a ucontext */
#define Iimm0  REG_EAX
#define Iarg_z REG_EBX
#define Itemp0 REG_ECX
#define Itemp1 REG_EDX
#define Ifn    REG_EDI
#define Iarg_y REG_ESI
#define Iesp   REG_ESP
#define Iebp   REG_EBP
#define Ieip   REG_EIP
#define Iflags REG_EFL

#define Isp Iesp
#define Iip Ieip
#define Iallocptr Itemp0
#define Ira0 Itemp0
#define Inargs Itemp1
#define Ixfn Itemp1
#define Ifp Iebp

/* MMX register offsets from where mm0 is found in uc_mcontext.fs */
#define Imm0 0
#define Imm1 1

#define nbits_in_word 32
#define log2_nbits_in_word 5
#define nbits_in_byte 8
#define ntagbits 3
#define nlisptagbits 2
#define nfixnumtagbits 2
#define num_subtag_bits 8
#define fixnumshift 2
#define fixnum_shift 2
#define fulltagmask 7
#define tagmask  3
#define fixnummask 3
#define subtagmask ((1<<num_subtag_bits)-1)
#define ncharcodebits 8
#define charcode_shift 8
#define node_size 4
#define node_shift 2
#define nargregs 2

#define tag_fixnum 0
#define tag_list 1
#define tag_misc 2
#define tag_imm 3

#define fulltag_even_fixnum 0
#define fulltag_cons 1
#define fulltag_nodeheader 2
#define fulltag_imm 3
#define fulltag_odd_fixnum 4
#define fulltag_tra 5
#define fulltag_misc 6
#define fulltag_immheader 7

#define SUBTAG(tag,subtag) ((tag) | ((subtag) << ntagbits))
#define IMM_SUBTAG(subtag) SUBTAG(fulltag_immheader,(subtag))
#define NODE_SUBTAG(subtag) SUBTAG(fulltag_nodeheader,(subtag))

#define subtag_bignum IMM_SUBTAG(0)
#define min_numeric_subtag subtag_bignum
#define subtag_ratio NODE_SUBTAG(1)
#define max_rational_subtag subtag_ratio
#define subtag_single_float IMM_SUBTAG(1)
#define subtag_double_float IMM_SUBTAG(2)
#define min_float_subtag subtag_single_float
#define max_float_subtag subtag_double_float
#define max_real_subtag subtag_double_float
#define subtag_complex NODE_SUBTAG(3)
#define max_numeric_subtag subtag_complex

#define subtag_bit_vector IMM_SUBTAG(31)
#define subtag_double_float_vector IMM_SUBTAG(30)
#define subtag_s16_vector IMM_SUBTAG(29)
#define subtag_u16_vector IMM_SUBTAG(28)
#define min_16_bit_ivector_subtag subtag_u16_vector
#define max_16_bit_ivector_subtag subtag_s16_vector

/* subtag 27 unused*/
#define subtag_s8_vector IMM_SUBTAG(26)
#define subtag_u8_vector IMM_SUBTAG(25)
#define min_8_bit_ivector_subtag subtag_u8_vector
#define max_8_bit_ivector_subtag IMM_SUBTAG(27)

#define subtag_simple_base_string IMM_SUBTAG(24)
#define subtag_fixnum_vector IMM_SUBTAG(23)
#define subtag_s32_vector IMM_SUBTAG(22)
#define subtag_u32_vector IMM_SUBTAG(21)
#define subtag_single_float_vector IMM_SUBTAG(20)
#define max_32_bit_ivector_subtag IMM_SUBTAG(24)
#define min_cl_ivector_subtag subtag_single_float_vector

#define subtag_vectorH NODE_SUBTAG(20)
#define subtag_arrayH NODE_SUBTAG(19)
#define subtag_simple_vector NODE_SUBTAG(21)    /*  Only one such subtag */
#define min_vector_subtag subtag_vectorH
#define min_array_subtag subtag_arrayH

#define subtag_macptr IMM_SUBTAG(3)
#define min_non_numeric_imm_subtag subtag_macptr

#define subtag_dead_macptr IMM_SUBTAG(4)
#define subtag_code_vector IMM_SUBTAG(5)
#define subtag_creole IMM_SUBTAG(6)

#define max_non_array_imm_subtag ((19<<ntagbits)|fulltag_immheader)

#define subtag_catch_frame NODE_SUBTAG(4)
#define subtag_function NODE_SUBTAG(5)
#define subtag_basic_stream NODE_SUBTAG(6)
#define subtag_symbol NODE_SUBTAG(7)
#define subtag_lock NODE_SUBTAG(8)
#define subtag_hash_vector NODE_SUBTAG(9)
#define subtag_pool NODE_SUBTAG(10)
#define subtag_weak NODE_SUBTAG(11)
#define subtag_package NODE_SUBTAG(12)
#define subtag_slot_vector NODE_SUBTAG(13)
#define subtag_instance NODE_SUBTAG(14)
#define subtag_struct NODE_SUBTAG(15)
#define subtag_istruct NODE_SUBTAG(16)
#define max_non_array_node_subtag ((19<<ntagbits)|fulltag_immheader)

#define subtag_unbound SUBTAG(fulltag_imm, 6)
#define unbound_marker subtag_unbound
#define undefined subtag_unbound
#define unbound subtag_unbound
#define subtag_character SUBTAG(fulltag_imm, 9)
#define slot_unbound SUBTAG(fulltag_imm, 10)
#define slot_unbound_marker slot_unbound
#define subtag_illegal SUBTAG(fulltag_imm,11)
#define illegal_marker subtag_illegal
#define subtag_forward_marker SUBTAG(fulltag_imm,28)
#define subtag_reserved_frame  SUBTAG(fulltag_imm,29)
#define reserved_frame_marker subtag_reserved_frame
#define subtag_no_thread_local_binding SUBTAG(fulltag_imm,30)
#define no_thread_local_binding_marker subtag_no_thread_local_binding
#define subtag_function_boundary_marker SUBTAG(fulltag_imm,31)
#define function_boundary_marker subtag_function_boundary_marker

/* The 32-bit immediate value in the instruction
 * "(mov ($ 0x12345678) (% fn))" at a tagged return address
 * refers to the associated function.
 */
#define RECOVER_FN_OPCODE 0xbf
#define RECOVER_FN_LENGTH 5


typedef struct double_float {
    LispObj header;
    LispObj pad;
    LispObj value_low;
    LispObj value_high;
} double_float;

typedef struct single_float {
    LispObj header;
    LispObj value;
} single_float;


typedef struct lisp_frame {
    struct lisp_frame *backlink;
    LispObj tra;
    LispObj xtra;		/* if tra is nvalretn */
} lisp_frame;

typedef struct exception_callback_frame {
    struct lisp_frame *backlink;
    LispObj tra;		/* ALWAYS 0 FOR AN XCF */
    LispObj nominal_function;   /* the current function at the time of the exception */
    LispObj relative_pc;        /* Boxed byte offset within actual function */
    LispObj containing_uvector;	/* the uvector that contains the relative PC or NIL */
    LispObj xp;			/* exception context */
    LispObj ra0;		/* value of ra0 from context */
    LispObj foreign_sp;		/* foreign sp at the time that exception occurred */
    LispObj prev_xframe;	/* so %apply-in-frame can unwind it */
    LispObj pc_low;		/* fixnum low half of absolute pc */
    LispObj pc_high;		/* and the high half */
} xcf;


typedef struct catch_frame {
    LispObj header;
    LispObj catch_tag;
    LispObj link;
    LispObj mvflag;
    LispObj esp;
    LispObj ebp;
    LispObj foreign_sp;
    LispObj db_link;
    LispObj xframe;
    LispObj pc;
} catch_frame;

#define catch_frame_element_count ((sizeof(catch_frame)/sizeof(LispObj))-1)
#define catch_frame_header make_header(subtag_catch_frame,catch_frame_element_count)

/* 
   All exception frames in a thread are linked together 
 */
typedef struct xframe_list {
  ExceptionInformation *curr;
  natural node_regs_mask;
  struct xframe_list *prev;
} xframe_list;

#define fixnum_bitmask(n)  (1<<((n)+fixnumshift))






#include "lisp-errors.h"

#ifdef DARWIN
#include <architecture/i386/sel.h>
#else
typedef unsigned short sel_t;   /* for now */
#endif

/*
 * bits correspond to reg encoding used in instructions
 *   7   6   5   4   3   2   1   0
 *  edi esi ebp esp ebx edx ecx eax
 */

#define X8632_DEFAULT_NODE_REGS_MASK 0xce

#ifdef WIN_32
/* TCR is in the last 34 words of NtCurrentTeb()->TlsSlots[] */
#define TCR_BIAS (0xe10 + 30 * sizeof(natural))

typedef struct tcr {
  struct tcr *linear;
  struct tcr_aux *aux;
  signed_natural valence;	/* odd when in foreign code */
  natural node_regs_mask;	/* bit set means register contains node */
  char *save_allocbase;
  char *save_allocptr;
  char *last_allocptr;
  LispObj catch_top;            /* top catch frame */
  special_binding *db_link;     /* special binding chain head */
  natural tlb_limit;
  LispObj *tlb_pointer;
  LispObj ffi_exception;        /* fpscr bits from ff-call */
  LispObj *foreign_sp;
  signed_natural interrupt_pending;     /* pending interrupt flag */
  LispObj next_method_context;  /* used in lieu of register */
  LispObj *next_tsp;
  void *safe_ref_address;
  LispObj *save_tsp;		/* TSP when in foreign code */
  LispObj *save_vsp;		/* VSP when in foreign code */
  LispObj *save_fp;		/* EBP when in foreign code */
  struct area *ts_area;		/* tstack area pointer */
  struct area *vs_area;		/* vstack area pointer */
  xframe_list *xframe;		/* exception-frame linked list */
  signed_natural unwinding;
  natural flags;
  natural foreign_mxcsr;
  natural lisp_mxcsr;
  ExceptionInformation *pending_exception_context;
  natural unboxed0;		/* unboxed scratch locations */
  natural unboxed1;
  LispObj save0;		/* spill area for node registers: */
  LispObj save1;		/*  it must be 16-byte aligned */
  LispObj save2;
  LispObj save3;
} TCR;

struct tcr_aux {
  unsigned long long bytes_allocated;
  struct area *cs_area;		/* cstack area pointer */
  LispObj cs_limit;		/* stack overflow limit */
  natural log2_allocation_quantum;      /* for per-thread consing */
  int *errno_loc;               /* per-thread (?) errno location */
  LispObj osid;                 /* OS thread id */
  signed_natural foreign_exception_status; /* non-zero -> call lisp_exit_hook */
  void *native_thread_info;	/* platform-dependent */
  void *native_thread_id;	/* mach_thread_t, pid_t, etc. */
  void *reset_completion;
  void *activate;
  ExceptionInformation *gc_context;
  void *termination_semaphore;
  natural shutdown_count;
  natural save_eflags;
  sel_t ldt_selector;
  signed_natural suspend_count;
  ExceptionInformation *suspend_context;
  void *suspend;                /* suspension semaphore */
  void *resume;                 /* resumption semaphore */
  void *allocated;
  void *pending_io_info;
  void *io_datum;
  struct tcr *next;
  struct tcr *prev;
};
#else
#define TCR_BIAS 0

typedef struct tcr {
  struct tcr *next;
  struct tcr *prev;
  natural node_regs_mask; /* bit set means correspnding reg contains node */
  struct tcr *linear;
  /* this spill area must be 16-byte aligned */
  LispObj save0;		/* spill area for node registers */
  LispObj save1;
  LispObj save2;
  LispObj save3;
  LispObj *save_fp;		/* EBP when in foreign code */
  u32_t lisp_mxcsr;
  u32_t foreign_mxcsr;
  special_binding *db_link;     /* special binding chain head */
  LispObj catch_top;            /* top catch frame */
  LispObj *save_vsp;		  /* VSP when in foreign code */
  LispObj *save_tsp;		  /* TSP when in foreign code */
  LispObj *foreign_sp;
  struct area *cs_area;		/* cstack area pointer */
  struct area *vs_area;		/* vstack area pointer */
  struct area *ts_area;		/* tstack area pointer */
  LispObj cs_limit;			/* stack overflow limit */
  natural bytes_allocated;
  natural bytes_consed_high;
  natural log2_allocation_quantum;      /* for per-thread consing */
  signed_natural interrupt_pending;     /* pending interrupt flag */
  xframe_list *xframe;	  /* exception-frame linked list */
  int *errno_loc;               /* per-thread (?) errno location */
  LispObj ffi_exception;        /* fpscr bits from ff-call */
  LispObj osid;                 /* OS thread id */
  signed_natural valence;	  /* odd when in foreign code */
  signed_natural foreign_exception_status; /* non-zero -> call lisp_exit_hook */
  void *native_thread_info;		     /* platform-dependent */
  void *native_thread_id;	/* mach_thread_t, pid_t, etc. */
  char *last_allocptr;
  char *save_allocptr;
  char *save_allocbase;
  void *reset_completion;
  void *activate;
  signed_natural suspend_count;
  ExceptionInformation *suspend_context;
  ExceptionInformation *pending_exception_context;
  void *suspend;                /* suspension semaphore */
  void *resume;                 /* resumption semaphore */
  natural flags;
  ExceptionInformation *gc_context;
  void *termination_semaphore;
  signed_natural unwinding;
  natural tlb_limit;
  LispObj *tlb_pointer;
  natural shutdown_count;
  LispObj *next_tsp;
  void *safe_ref_address;
  sel_t ldt_selector;
  natural scratch_mxcsr;
  natural unboxed0;
  natural unboxed1;
  LispObj next_method_context; /* used in lieu of register */
  natural save_eflags;
  void *allocated;
  void *pending_io_info;
  void *io_datum;
} TCR;
#endif

/* expansion of "TCR_AUX(tcr)": TCR_AUX(tcr) */

#define nil_value ((0x13000 + (fulltag_cons))+(LOWMEM_BIAS))
#define t_value ((0x13008 + (fulltag_misc))+(LOWMEM_BIAS))
#define t_offset (t_value-nil_value)
#define misc_header_offset -fulltag_misc
#define misc_data_offset misc_header_offset + node_size

typedef struct {
  natural Eip;
  natural Cs;                   /* in low 16 bits */
  natural EFlags;
} ia32_iret_frame;

#define heap_segment_size 0x00010000
#define log2_heap_segment_size 16

#ifndef EFL_DF
#define EFL_DF 1024
#endif

#define ABI_VERSION_MIN 1039
#define ABI_VERSION_CURRENT 1039
#define ABI_VERSION_MAX 1039
