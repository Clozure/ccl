/* offsets into uc_mcontext.ss */
#ifdef DARWIN
#define REG_EAX 0
#define REG_EBX 1
#define REG_ECX 2
#define REG_EDX 3
#define REG_EDI 4
#define REG_ESI 5
#define REG_EBP 6
#define REG_ESP 7
#define REG_EFL 9
#define REG_EIP 10
#endif

#ifdef WINDOWS
/* Offsets relative to _CONTEXT.Edi */
#define REG_EDI 0
#define REG_ESI 1
#define REG_EBX 2
#define REG_EDX 3
#define REG_ECX 4
#define REG_EAX 5
#define REG_EBP 6
#define REG_EIP 7
#define REG_EFL 9
#define REG_ESP 10
#endif

#ifdef FREEBSD
#define REG_EDI 5
#define REG_ESI 6
#define REG_EBP 7
#define REG_ISP 8
#define REG_EBX 9
#define REG_EDX 10
#define REG_ECX 11
#define REG_EAX 12
#define REG_EIP 15
#define REG_EFL 17
#define REG_ESP 18
#endif

#ifdef SOLARIS
#include <sys/regset.h>
#include <limits.h>
#define REG_EAX EAX
#define REG_EBX EBX
#define REG_ECX ECX
#define REG_EDX EDX
#define REG_ESI ESI
#define REG_EDI EDI
#define REG_EBP EBP
#define REG_ESP UESP    /* Maybe ... ESP is often 0, but who knows why ? */
#define REG_EFL EFL
#define REG_EIP EIP
#endif

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

typedef struct cons {
    LispObj cdr;
    LispObj car;
} cons;

typedef struct lispsymbol {
    LispObj header;
    LispObj pname;
    LispObj vcell;
    LispObj fcell;
    LispObj package_predicate;
    LispObj flags;
    LispObj plist;
    LispObj binding_index;
} lispsymbol;

typedef struct ratio {
    LispObj header;
    LispObj numer;
    LispObj denom;
} ratio;

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

typedef struct macptr {
    LispObj header;
    LispObj address;
    LispObj class;
    LispObj type;
} macptr;

typedef struct xmacptr {
    LispObj header;
    LispObj address;
    LispObj class;
    LispObj type;
    LispObj flags;
    LispObj link;
} xmacptr;

typedef struct special_binding {
    struct special_binding *link;
    struct lispsymbol *sym;
    LispObj value;
} special_binding;

typedef struct lisp_frame {
    struct lisp_frame *backlink;
    LispObj tra;
    LispObj xtra;		/* if tra is nvalretn */
} lisp_frame;

typedef struct exception_callback_frame {
    struct lisp_frame *backlink;
    LispObj tra;		/* ALWAYS 0 FOR AN XCF */
    LispObj nominal_function;   /* the current function at the time of the exception */
    LispObj relative_pc;        /* Boxed byte offset within actual function or absolute address */
    LispObj containing_uvector;	/* the uvector that contains the relative PC or NIL */
    LispObj xp;			/* exception context */
    LispObj ra0;		/* value of ra0 from context */
    LispObj foreign_sp;		/* foreign sp at the time that exception occurred */
    LispObj prev_xframe;	/* so %apply-in-frame can unwind it */
} xcf;

/* The GC (at least) needs to know what a
   package looks like, so that it can do GCTWA. */
typedef struct package {
    LispObj header;
    LispObj itab; 		/* itab and etab look like (vector (fixnum . fixnum) */
    LispObj etab;
    LispObj used;
    LispObj used_by;
    LispObj names;
    LispObj shadowed;
} package;

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

/* 
  The GC (at least) needs to know about hash-table-vectors and their flag bits.
*/

typedef struct hash_table_vector_header {
  LispObj header;
  LispObj link;                 /* If weak */
  LispObj flags;                /* a fixnum; see below */
  LispObj gc_count;             /* gc-count kernel global */
  LispObj free_alist;           /* preallocated conses for finalization_alist */
  LispObj finalization_alist;   /* key/value alist for finalization */
  LispObj weak_deletions_count; /* incremented when GC deletes weak pair */
  LispObj hash;                 /* backpointer to hash-table */
  LispObj deleted_count;        /* number of deleted entries [not maintained if lock-free] */
  LispObj count;                /* number of valid entries [not maintained if lock-free] */
  LispObj cache_idx;            /* index of last cached pair */
  LispObj cache_key;            /* value of last cached key */
  LispObj cache_value;          /* last cached value */
  LispObj size;                 /* number of entries in table */
  LispObj size_reciprocal;      /* shifted reciprocal of size */
} hash_table_vector_header;

/*
  Bits (masks) in hash_table_vector.flags:
*/

/* GC should track keys when addresses change */ 
#define nhash_track_keys_mask fixnum_bitmask(28) 

/* GC should set when nhash_track_keys_bit & addresses change */
#define nhash_key_moved_mask  fixnum_bitmask(27) 

/* weak on key or value (need new "weak both" encoding.) */
#define nhash_weak_mask       fixnum_bitmask(12)

/* weak on value */
#define nhash_weak_value_mask fixnum_bitmask(11)

/* finalizable */
#define nhash_finalizable_mask fixnum_bitmask(10)

/* keys frozen, i.e. don't clobber keys, only values */
#define nhash_keys_frozen_mask fixnum_bitmask(9)

/* Lfun bits */

#define lfbits_nonnullenv_mask fixnum_bitmask(0)
#define lfbits_keys_mask fixnum_bitmask(1)
#define lfbits_restv_mask fixnum_bitmask(7)
#define lfbits_optinit_mask fixnum_bitmask(14)
#define lfbits_rest_mask fixnum_bitmask(15)
#define lfbits_aok_mask fixnum_bitmask(16)
#define lfbits_lap_mask fixnum_bitmask(23)
#define lfbits_trampoline_mask fixnum_bitmask(24)
#define lfbits_evaluated_mask fixnum_bitmask(25)
#define lfbits_cm_mask fixnum_bitmask(26)         /* combined_method */
#define lfbits_nextmeth_mask fixnum_bitmask(26)   /* or call_next_method with method_mask */
#define lfbits_gfn_mask fixnum_bitmask(27)        /* generic_function */
#define lfbits_nextmeth_with_args_mask fixnum_bitmask(27)   /* or call_next_method_with_args with method_mask */
#define lfbits_method_mask fixnum_bitmask(28)     /* method function */
/* PPC only but want it defined for xcompile */
#define lfbits_noname_mask fixnum_bitmask(29)

/*
  known values of an "extended" (gcable) macptr's flags word:
*/

typedef enum {
  xmacptr_flag_none = 0,        /* Maybe already disposed by Lisp */
  xmacptr_flag_recursive_lock,  /* recursive-lock */
  xmacptr_flag_ptr,             /* malloc/free */
  xmacptr_flag_rwlock,          /* read/write lock */
  xmacptr_flag_semaphore        /* semaphore */
} xmacptr_flag;

/* Creole */

#define doh_quantum 400
#define doh_block_slots ((doh_quantum >> 2) - 3)

typedef struct doh_block {
  struct doh_block *link;
  unsigned size;
  unsigned free;
  LispObj data[doh_block_slots];
} doh_block, *doh_block_ptr;

#define population_weak_list (0<<fixnum_shift)
#define population_weak_alist (1<<fixnum_shift)
#define population_termination_bit (16+fixnum_shift)
#define population_type_mask ((1<<population_termination_bit)-1)

#define gc_retain_pages_bit fixnum_bitmask(0)
#define gc_integrity_check_bit fixnum_bitmask(2)
#define egc_verbose_bit fixnum_bitmask(3)
#define gc_verbose_bit fixnum_bitmask(4)
#define gc_allow_stack_overflows_bit fixnum_bitmask(5)
#define gc_postgc_pending fixnum_bitmask(26)

#include "lisp-errors.h"

#ifdef DARWIN
#include <architecture/i386/sel.h>
#else
typedef unsigned short sel_t;   /* for now */
#endif

#define TCR_BIAS 0

/*
 * bits correspond to reg encoding used in instructions
 *   7   6   5   4   3   2   1   0
 *  edi esi ebp esp ebx edx ecx eax
 */

#define X8632_DEFAULT_NODE_REGS_MASK 0xce

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
  LispObj *save_ebp;		/* EBP when in foreign code */
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
  void *last_allocptr;
  void *save_allocptr;
  void *save_allocbase;
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

#define nil_value ((0x13000 + (fulltag_cons))+(LOWMEM_BIAS))
#define t_value ((0x13008 + (fulltag_misc))+(LOWMEM_BIAS))
#define t_offset (t_value-nil_value)
#define misc_header_offset -fulltag_misc
#define misc_data_offset misc_header_offset + node_size

#define heap_segment_size 0x00010000
#define log2_heap_segment_size 16

#ifndef EFL_DF
#define EFL_DF 1024
#endif
