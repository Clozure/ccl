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

#define TCR_FLAG_BIT_FOREIGN fixnumshift
#define TCR_FLAG_BIT_AWAITING_PRESET (fixnumshift+1)
#define TCR_FLAG_BIT_ALT_SUSPEND (fixnumshift+2)
#define TCR_FLAG_BIT_PROPAGATE_EXCEPTION (fixnumshift+3)
#define TCR_FLAG_BIT_SUSPEND_ACK_PENDING (fixnumshift+4)
#define TCR_FLAG_BIT_PENDING_EXCEPTION (fixnumshift+5)
#define TCR_FLAG_BIT_FOREIGN_EXCEPTION (fixnumshift+6)
#define TCR_FLAG_BIT_PENDING_SUSPEND (fixnumshift+7)
#define TCR_FLAG_BIT_FOREIGN_FPE (fixnumshift+8)

#define TCR_STATE_FOREIGN (1)
#define TCR_STATE_LISP    (0)
#define TCR_STATE_EXCEPTION_WAIT (2)
#define TCR_STATE_EXCEPTION_RETURN (4)

#define dnode_size (node_size*2)
#define dnode_shift (node_shift+1)

#define INTERRUPT_LEVEL_BINDING_INDEX (1)

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
  LispObj count;                /* number of valid entries */
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
#define lfbits_noname_mask fixnum_bitmask(29)


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

typedef struct {
  natural w0;
  natural w1;
} dnode;
