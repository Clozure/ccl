/*
   Copyright (C) 2003-2009, Clozure Associates.
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

#ifndef __constants64__
#define __constants64__ 1

#define rcontext 2

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

#define lowtagmask 3L
#define lowtag_mask lowtagmask

#define lowtag_primary 0L
#define lowtag_imm 1L
#define lowtag_immheader 2L
#define lowtag_nodeheader 3L

#define tag_fixnum 0L

#define fulltag_even_fixnum 0L
#define fulltag_imm_0 1L
#define fulltag_immheader_0 2L
#define fulltag_nodeheader_0 3L
#define fulltag_cons 4L
#define fulltag_imm_1 5L
#define fulltag_immheader_1 6L
#define fulltag_nodeheader_1 7L
#define fulltag_odd_fixnum 8L
#define fulltag_imm_2 9L
#define fulltag_immheader_2 10L
#define fulltag_nodeheader_2 11L
#define fulltag_misc 12L
#define fulltag_imm_3 13L
#define fulltag_immheader_3 14L
#define fulltag_nodeheader_3 15L

#define SUBTAG(tag,subtag) ((tag) | ((subtag) << ntagbits))
#define cl_array_subtag_mask 0x80L
#define CL_ARRAY_SUBTAG(tag,subtag) (cl_array_subtag_mask | (SUBTAG(tag,subtag)))

#define subtag_arrayH CL_ARRAY_SUBTAG(fulltag_nodeheader_1,0L)
#define subtag_vectorH CL_ARRAY_SUBTAG(fulltag_nodeheader_2,0L)
#define subtag_simple_vector CL_ARRAY_SUBTAG(fulltag_nodeheader_3,0L)
#define min_vector_subtag subtag_vectorH	

#define ivector_class_64_bit fulltag_immheader_3
#define ivector_class_32_bit fulltag_immheader_2
#define ivector_class_other_bit fulltag_immheader_1
#define ivector_class_8_bit fulltag_immheader_0

#define subtag_s64_vector CL_ARRAY_SUBTAG(ivector_class_64_bit,1)
#define subtag_u64_vector CL_ARRAY_SUBTAG(ivector_class_64_bit,2)
#define subtag_fixnum_vector CL_ARRAY_SUBTAG(ivector_class_64_bit,3)
#define subtag_double_float_vector CL_ARRAY_SUBTAG(ivector_class_64_bit,4)
#define subtag_s32_vector CL_ARRAY_SUBTAG(ivector_class_32_bit,1)
#define subtag_u32_vector CL_ARRAY_SUBTAG(ivector_class_32_bit,2)
#define subtag_single_float_vector CL_ARRAY_SUBTAG(ivector_class_32_bit,3)
#define subtag_simple_base_string CL_ARRAY_SUBTAG(ivector_class_32_bit,5)
#define subtag_s16_vector CL_ARRAY_SUBTAG(ivector_class_other_bit,1)
#define subtag_u16_vector CL_ARRAY_SUBTAG(ivector_class_other_bit,2)
#define subtag_bit_vector CL_ARRAY_SUBTAG(ivector_class_other_bit,7)
#define subtag_s8_vector CL_ARRAY_SUBTAG(ivector_class_8_bit,1)
#define subtag_u8_vector CL_ARRAY_SUBTAG(ivector_class_8_bit,2)

/* There's some room for expansion in non-array ivector space. */
#define subtag_macptr SUBTAG(ivector_class_64_bit,1)
#define subtag_dead_macptr SUBTAG(ivector_class_64_bit,2)
#define subtag_code_vector SUBTAG(ivector_class_32_bit,0)
#define subtag_xcode_vector SUBTAG(ivector_class_32_bit,1)
#define subtag_bignum SUBTAG(ivector_class_32_bit,2)
#define subtag_double_float SUBTAG(ivector_class_32_bit,3)


/*
 Size doesn't matter for non-CL-array gvectors; I can't think of a good
 reason to classify them in any particular way.  Let's put funcallable
 things in the first slice by themselves, though it's not clear that
 that helps FUNCALL much.
*/
#define gvector_funcallable fulltag_nodeheader_0
	
#define subtag_function SUBTAG(gvector_funcallable,0)
#define subtag_symbol SUBTAG(gvector_funcallable,1)
#define subtag_catch_frame SUBTAG(fulltag_nodeheader_1,0)
#define subtag_basic_stream SUBTAG(fulltag_nodeheader_1,1)
#define subtag_lock SUBTAG(fulltag_nodeheader_1,2)
#define subtag_hash_vector SUBTAG(fulltag_nodeheader_1,3)
#define subtag_pool SUBTAG(fulltag_nodeheader_1,4)
#define subtag_weak SUBTAG(fulltag_nodeheader_1,5)
#define subtag_package SUBTAG(fulltag_nodeheader_1,6)

#define subtag_slot_vector SUBTAG(fulltag_nodeheader_2,0)
#define subtag_instance SUBTAG(fulltag_nodeheader_2,1)
#define subtag_struct SUBTAG(fulltag_nodeheader_2,2)
#define subtag_istruct SUBTAG(fulltag_nodeheader_2,3)
#define subtag_value_cell SUBTAG(fulltag_nodeheader_2,4)
#define subtag_xfunction SUBTAG(fulltag_nodeheader_2,5)

#define subtag_ratio SUBTAG(fulltag_nodeheader_3,0)
#define subtag_complex SUBTAG(fulltag_nodeheader_3,1)



#define nil_value (0x3000+fulltag_misc+sizeof(struct lispsymbol)+(LOWMEM_BIAS))
#define t_value (0x3000+fulltag_misc+(LOWMEM_BIAS))	
#define misc_bias fulltag_misc
#define cons_bias fulltag_cons

	
#define misc_header_offset -fulltag_misc
#define misc_subtag_offset misc_header_offset+7       /* low byte of header */
#define misc_data_offset misc_header_offset+8		/* first word of data */
#define misc_dfloat_offset misc_header_offset		/* double-floats are doubleword-aligned */

#define subtag_single_float SUBTAG(fulltag_imm_0,0)

#define subtag_go_tag SUBTAG(fulltag_imm_1,2) /* deprecated */
#define subtag_block_tag SUBTAG(fulltag_imm_1,3) /* deprecated */

#define subtag_character SUBTAG(fulltag_imm_1,0)

#define subtag_unbound SUBTAG(fulltag_imm_3,0)
#define unbound_marker subtag_unbound
#define undefined unbound_marker
#define unbound unbound_marker
#define subtag_slot_unbound SUBTAG(fulltag_imm_3,1)
#define slot_unbound_marker subtag_slot_unbound
#define slot_unbound slot_unbound_marker
#define subtag_illegal SUBTAG(fulltag_imm_3,2)
#define illegal_marker subtag_illegal
#define subtag_no_thread_local_binding SUBTAG(fulltag_imm_3,3)
#define no_thread_local_binding_marker subtag_no_thread_local_binding        
#define subtag_forward_marker SUBTAG(fulltag_imm_3,7)
	
#define max_64_bit_constant_index ((0x7fff + misc_dfloat_offset)>>3)
#define max_32_bit_constant_index ((0x7fff + misc_data_offset)>>2)
#define max_16_bit_constant_index ((0x7fff + misc_data_offset)>>1)
#define max_8_bit_constant_index (0x7fff + misc_data_offset)
#define max_1_bit_constant_index ((0x7fff + misc_data_offset)<<5)


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
  

typedef struct eabi_c_frame {
  struct eabi_c_frame *backlink;
  unsigned savelr;
  LispObj params[8];
} eabi_c_frame;

/* PowerOpen ABI C frame */

typedef struct c_frame {
  struct c_frame *backlink;
  natural crsave;
  natural savelr;
  natural unused[2];
  natural savetoc;		/* Used with CFM (and on Linux.) */
  natural params[8];		/* Space for callee to save r3-r10 */
} c_frame;

typedef struct lisp_frame {
  struct lisp_frame *backlink;
  LispObj savefn;
  LispObj savelr;
  LispObj savevsp;
} lisp_frame;

typedef struct special_binding {
  struct special_binding *link;
  struct lispsymbol *sym;
  LispObj value;
} special_binding;

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
  LispObj regs[8];
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
    float f;
    u_int32_t tag;
  } single_float_convert;
  union {
    double d;
    struct {u_int32_t h, l;} words;
  } lisp_fpscr;			/* lisp thread's fpscr (in low word) */
  special_binding* db_link;	/* special binding chain head */
  LispObj catch_top;		/* top catch frame */
  LispObj* save_vsp;  /* VSP when in foreign code */
  LispObj* save_tsp;  /* TSP when in foreign code */
  struct area* cs_area; /* cstack area pointer */
  struct area* vs_area; /* vstack area pointer */
  struct area* ts_area; /* tstack area pointer */
  LispObj cs_limit;		/* stack overflow limit */
  natural bytes_allocated;
  natural log2_allocation_quantum;      /* for per-tread consing */
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
  void *safe_ref_address;
} TCR;

#define t_offset -(sizeof(lispsymbol))

/* 
  These were previously global variables.  There are lots of implicit
  assumptions about the size of a heap segment, so they might as well
  be constants.
*/

#define heap_segment_size 0x00020000L
#define log2_heap_segment_size 17L

#endif

