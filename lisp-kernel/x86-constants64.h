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

#ifdef DARWIN
#define REG_RAX 0
#define REG_RBX 1
#define REG_RCX 2
#define REG_RDX 3
#define REG_RDI 4
#define REG_RSI 5
#define REG_RBP 6
#define REG_RSP 7
#define REG_R8 8
#define REG_R9 9
#define REG_R10 10
#define REG_R11 11
#define REG_R12 12
#define REG_R13 13
#define REG_R14 14
#define REG_R15 15
#define REG_RIP 16
#define REG_RFL 17
#endif

#ifdef FREEBSD
#define REG_RDI 1
#define REG_RSI 2
#define REG_RDX 3
#define REG_RCX 4
#define REG_R8 5
#define REG_R9 6
#define REG_RAX 7
#define REG_RBX 8
#define REG_RBP 9
#define REG_R10 10
#define REG_R11 11
#define REG_R12 12
#define REG_R13 13
#define REG_R14 14
#define REG_R15 15
#define REG_RIP 20
#define REG_RFL 22
#define REG_RSP 23
#endif

#ifdef WIN_64
/* DWORD64 indices in &(CONTEXT->Rax) */
#define REG_RAX     0
#define REG_RCX     1
#define REG_RDX     2
#define REG_RBX     3
#define REG_RSP     4
#define REG_RBP     5
#define REG_RSI     6
#define REG_RDI     7
#define REG_R8      8
#define REG_R9      9
#define REG_R10     10
#define REG_R11     11
#define REG_R12     12
#define REG_R13     13
#define REG_R14     14
#define REG_R15     15
#define REG_RIP     16
#endif

/* Define indices of the GPRs in the mcontext component of a ucontext */
#define Itemp0      REG_RBX
#define Iarg_y      REG_RDI
#define Iarg_x      REG_R8
#define Iarg_z      REG_RSI
#define Isave3      REG_R11
#define Isave2      REG_R12
#define Isave1      REG_R14
#define Isave0      REG_R15
#define Itemp2        REG_R10
#define Ifn         REG_R13
#define Irbp        REG_RBP
#define Iimm0       REG_RAX
#define Iimm1       REG_RDX
#define Iimm2       REG_RCX
#define Itemp1      REG_R9
#define Isp         REG_RSP
#define Iip         REG_RIP
#if defined(LINUX) || defined(WINDOWS)
#define Iflags      REG_EFL
#endif

#if defined(SOLARIS) || defined(FREEBSD) || defined(DARWIN)
#define Iflags      REG_RFL
#endif


#define Iallocptr Itemp0
#define Ira0 Itemp2
#define Inargs Iimm2
#define Ixfn Itemp1
#define Ifp Irbp


#define nbits_in_word 64L
#define log2_nbits_in_word 6L
#define nbits_in_byte 8L
#define ntagbits 4L
#define nlisptagbits 3L
#define nfixnumtagbits 2L
#define num_subtag_bits 8L
#define fixnumshift 3L
#define fixnum_shift 3L
#define fulltagmask 15L
#define tagmask	 7L
#define fixnummask 3
#define subtagmask ((1L<<num_subtag_bits)-1L)
#define ncharcodebits 8L
#define charcode_shift 8L
#define node_size 8L
#define node_shift 3L
#define nargregs 3L

#define tag_fixnum 0L
#define tag_imm_0 1L		/* subtag_single_float ONLY */
#define tag_imm_1 2L		/* subtag_character, internal markers */
#define tag_list 3L		/* subtag_cons or NIL */
#define tag_tra 4L		/* tagged return_address */
#define tag_misc 5L		/* random uvector */
#define tag_symbol 6L	        /* non-null symbol */
#define tag_function 7L	/* function entry point */

#define fulltag_even_fixnum 0L
#define fulltag_imm_0 1L
#define fulltag_imm_1 2L
#define fulltag_cons 3L
#define fulltag_tra_0 4L
#define fulltag_nodeheader_0 5L
#define fulltag_nodeheader_1 6L
#define fulltag_immheader_0 7L
#define fulltag_odd_fixnum 8L
#define fulltag_immheader_1 9L
#define fulltag_immheader_2 10L
#define fulltag_nil 11L
#define fulltag_tra_1 12L
#define fulltag_misc 13L
#define fulltag_symbol 14L
#define fulltag_function 15L

#define SUBTAG(tag,subtag) ((tag) | ((subtag) << ntagbits))
#define subtag_arrayH SUBTAG(fulltag_nodeheader_0,10L)
#define subtag_vectorH SUBTAG(fulltag_nodeheader_1,10L)
#define subtag_simple_vector SUBTAG(fulltag_nodeheader_1,11L)
#define min_vector_subtag subtag_vectorH	

#define ivector_class_64_bit fulltag_immheader_2
#define ivector_class_32_bit fulltag_immheader_1
#define ivector_class_other_bit fulltag_immheader_0


#define subtag_fixnum_vector SUBTAG(ivector_class_64_bit,12L)
#define subtag_s64_vector SUBTAG(ivector_class_64_bit,13L)
#define subtag_u64_vector SUBTAG(ivector_class_64_bit,14L)
#define subtag_double_float_vector SUBTAG(ivector_class_64_bit,15L)

#define subtag_simple_base_string SUBTAG(ivector_class_32_bit,12L)
#define subtag_s32_vector SUBTAG(ivector_class_32_bit,13L)
#define subtag_u32_vector SUBTAG(ivector_class_32_bit,14L)
#define subtag_single_float_vector SUBTAG(ivector_class_32_bit,15L)

#define subtag_s16_vector SUBTAG(ivector_class_other_bit,10L)
#define subtag_u16_vector SUBTAG(ivector_class_other_bit,11L)
#define subtag_s8_vector SUBTAG(ivector_class_other_bit,13L)
#define subtag_u8_vector SUBTAG(ivector_class_other_bit,14L)
#define subtag_bit_vector SUBTAG(ivector_class_other_bit,15L)
/* min_8_bit_ivector_subtag is the old 8-bit simple_base_string */
#define min_8_bit_ivector_subtag SUBTAG(ivector_class_other_bit,12L)

/* There's some room for expansion in non-array ivector space. */
#define subtag_macptr SUBTAG(ivector_class_64_bit,1)
#define subtag_dead_macptr SUBTAG(ivector_class_64_bit,2)
#define subtag_bignum SUBTAG(ivector_class_32_bit,0)
#define subtag_double_float SUBTAG(ivector_class_32_bit,1)
#define subtag_xcode_vector SUBTAG(ivector_class_32_bit,2)

/* Note the difference between (e.g) fulltag_function - which
   defines what the low 4 bytes of a function pointer look like -
   and subtag_function - which describes what the subtag byte
   in a function header looks like.  (Likewise for fulltag_symbol
   and subtag_symbol)
*/		

#define subtag_symbol SUBTAG(fulltag_nodeheader_0,1)
#define subtag_catch_frame SUBTAG(fulltag_nodeheader_0,2)
#define subtag_hash_vector SUBTAG(fulltag_nodeheader_0,3)
#define subtag_pool SUBTAG(fulltag_nodeheader_0,4)
#define subtag_weak SUBTAG(fulltag_nodeheader_0,5)
#define subtag_package SUBTAG(fulltag_nodeheader_0,6)
#define subtag_slot_vector SUBTAG(fulltag_nodeheader_0,7)
#define subtag_basic_stream SUBTAG(fulltag_nodeheader_0,8)
#define subtag_function SUBTAG(fulltag_nodeheader_0,9)

#define subtag_ratio SUBTAG(fulltag_nodeheader_1,1)
#define subtag_complex SUBTAG(fulltag_nodeheader_1,2)
#define subtag_struct SUBTAG(fulltag_nodeheader_1,3)
#define subtag_istruct SUBTAG(fulltag_nodeheader_1,4)
#define subtag_value_cell SUBTAG(fulltag_nodeheader_1,5)
#define subtag_xfunction SUBTAG(fulltag_nodeheader_1,6)
#define subtag_lock SUBTAG(fulltag_nodeheader_1,7)
#define subtag_instance SUBTAG(fulltag_nodeheader_1,8)



#define nil_value ((0x13000+fulltag_nil)+(LOWMEM_BIAS))
#define t_value ((0x13020+fulltag_symbol)+(LOWMEM_BIAS))
#define misc_bias fulltag_misc
#define cons_bias fulltag_cons

	
#define misc_header_offset -fulltag_misc
#define misc_subtag_offset misc_header_offset       /* low byte of header */
#define misc_data_offset misc_header_offset+node_size	/* first word of data */
#define misc_dfloat_offset misc_header_offset		/* double-floats are doubleword-aligned */

#define subtag_single_float SUBTAG(fulltag_imm_0,0)
#define subtag_character SUBTAG(fulltag_imm_1,0)

#define subtag_unbound SUBTAG(fulltag_imm_1,1)
#define unbound_marker subtag_unbound
#define undefined unbound_marker
#define unbound unbound_marker
#define subtag_slot_unbound SUBTAG(fulltag_imm_1,2)
#define slot_unbound_marker subtag_slot_unbound
#define slot_unbound slot_unbound_marker
#define subtag_illegal SUBTAG(fulltag_imm_1,3)
#define illegal_marker subtag_illegal
#define subtag_no_thread_local_binding SUBTAG(fulltag_imm_1,4)
#define no_thread_local_binding_marker subtag_no_thread_local_binding
#define subtag_reserved_frame  SUBTAG(fulltag_imm_1,5)
#define reserved_frame_marker subtag_reserved_frame
#define subtag_forward_marker SUBTAG(fulltag_imm_1,6)

#define function_boundary_marker SUBTAG(fulltag_imm_1,15)	

/* The objects themselves look something like this: */

/*  Order of CAR and CDR doesn't seem to matter much - there aren't */
/*  too many tricks to be played with predecrement/preincrement addressing. */
/*  Keep them in the confusing MCL 3.0 order, to avoid confusion. */

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
  LispObj value;
} double_float;


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
  LispObj xtra;			/* if tra is nvalretn */
} lisp_frame;

/* These are created on the lisp stack by the exception callback mechanism,
   but nothing ever returns to them.  (At the very least, nothing -should-
   try to return to them ...).
*/
typedef struct exception_callback_frame {
  struct lisp_frame *backlink;
  LispObj tra;                  /* ALWAYS 0 FOR AN XCF */
  LispObj nominal_function;     /* the current function at the time of the exception */
  LispObj relative_pc;          /* Boxed byte offset within actual
                                   function or absolute address */
  LispObj containing_uvector;   /* the uvector that contains the relative PC or NIL */
  LispObj xp;                   /* exception context */
  LispObj ra0;                  /* value of ra0 from context */
  LispObj foreign_sp;           /* foreign sp at the time that exception occurred */
  LispObj prev_xframe;          /* so %apply-in-frame can unwind it */
} xcf;


/* The GC (at least) needs to know what a
   package looks like, so that it can do GCTWA. */
typedef struct package {
  LispObj header;
  LispObj itab;			/* itab and etab look like (vector (fixnum . fixnum) */
  LispObj etab;
  LispObj used;
  LispObj used_by;
  LispObj names;
  LispObj shadowed;
} package;

/*
  The GC also needs to know what a catch_frame looks like.
*/

typedef struct catch_frame {
  LispObj header;
  LispObj catch_tag;
  LispObj link;
  LispObj mvflag;
  LispObj csp;
  LispObj db_link;
  LispObj regs[4];
  LispObj xframe;
  LispObj tsp_segment;
} catch_frame;

#define catch_frame_element_count ((sizeof(catch_frame)/sizeof(LispObj))-1)
#define catch_frame_header make_header(subtag_catch_frame,catch_frame_element_count)


/* 
  All exception frames in a thread are linked together 
  */
typedef struct xframe_list {
  ExceptionInformation *curr;
  struct xframe_list *prev;
} xframe_list;

#define fixnum_bitmask(n)  (1LL<<((n)+fixnumshift))

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
  Bits (masks)  in hash_table_vector.flags:
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



#define TCR_BIAS (0x0)

typedef struct tcr {
  struct tcr* next;
  struct tcr* prev;
  struct {
    u32_t tag;
    float f;
  } single_float_convert;
  struct tcr* linear;
  LispObj *save_fp;            /* RBP when in foreign code */
  u32_t lisp_mxcsr;
  u32_t foreign_mxcsr;
  special_binding* db_link;	/* special binding chain head */
  LispObj catch_top;		/* top catch frame */
  LispObj* save_vsp;  /* VSP when in foreign code */
  LispObj* save_tsp;  /* TSP when in foreign code */
  LispObj* foreign_sp;
  struct area* cs_area; /* cstack area pointer */
  struct area* vs_area; /* vstack area pointer */
  struct area* ts_area; /* tstack area pointer */
  LispObj cs_limit;		/* stack overflow limit */
  natural bytes_allocated;
  natural log2_allocation_quantum;      /* for per-thread consing */
  signed_natural interrupt_pending;	/* pending interrupt flag */
  xframe_list* xframe; /* exception-frame linked list */
  int* errno_loc;		/* per-thread (?) errno location */
  LispObj ffi_exception;	/* fpscr bits from ff-call */
  LispObj osid;			/* OS thread id */
  signed_natural valence;			/* odd when in foreign code */
  signed_natural foreign_exception_status;	/* non-zero -> call lisp_exit_hook */
  void* native_thread_info;	/* platform-dependent */
  void* native_thread_id;	/* mach_thread_t, pid_t, etc. */
  void* last_allocptr;
  void* save_allocptr;
  void* save_allocbase;
  void* reset_completion;
  void* activate;
  signed_natural suspend_count;
  ExceptionInformation* suspend_context;
  ExceptionInformation* pending_exception_context;
  void* suspend;		/* suspension semaphore */
  void* resume;			/* resumption semaphore */
  natural flags;
  ExceptionInformation* gc_context;
  void* termination_semaphore;
  signed_natural unwinding;
  natural tlb_limit;
  LispObj* tlb_pointer;
  natural shutdown_count;
  LispObj* next_tsp;
  void *safe_ref_address;
  void *pending_io_info;
  void *io_datum;
} TCR;

#define t_offset (t_value-nil_value)

typedef struct {
  natural Rip;
  natural Cs;                   /* in low 16 bits */
  natural Rflags;               /* in low 32 bits */
  natural Rsp;
  natural Ss;                   /* in low 16 bits*/
} x64_iret_frame;

/* 
  These were previously global variables.  There are lots of implicit
  assumptions about the size of a heap segment, so they might as well
  be constants.
*/

#define heap_segment_size 0x00020000L
#define log2_heap_segment_size 17L

