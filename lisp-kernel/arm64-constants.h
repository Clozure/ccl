/* SPDX-License-Identifier: Apache-2.0 */

/*
 * This stuff must match the contents of arm64-arch.lisp and arm64-arm.lisp
 * in ccl:compiler;ARM64;
 *
 * N.B.: This file in included in both .c and .s files.
 */

#ifdef __ASSEMBLER__
#  define DEFCONST(name, val) name = val
#else
#  define DEFCONST(name, val) enum { name = (val) };
#endif

DEFCONST(nbits_in_word, 64)
DEFCONST(log2_nbits_in_word, 6)
DEFCONST(nbits_in_byte, 8)
DEFCONST(ntagbits, 4)
DEFCONST(nlisptagbits, 3)
DEFCONST(nfixnumtagbits, 3)
DEFCONST(num_subtag_bits, 8)
DEFCONST(fixnumshift, 3)
DEFCONST(fixnum_shift, 3)
DEFCONST(fulltagmask, 15)
DEFCONST(tagmask, 7)
DEFCONST(fixnummask, 3)
DEFCONST(subtagmask, ((1<<num_subtag_bits)-1))
DEFCONST(ncharcodebits, 8)
DEFCONST(charcode_shift, 8)
DEFCONST(node_size, 8)
DEFCONST(dnode_size, 16)
DEFCONST(node_shift, 3)
DEFCONST(nargregs, 3)
DEFCONST(nsaveregs, 4)

/* registers */
#ifdef __ASSEMBLER__
imm0 .req x0
imm1 .req x1
imm2 .req x2
imm3 .req x3
imm4 .req x4
imm5 .req x6
nargs .req x6
fn .req x7
/* x8 tbd */
arg_w .req x9
arg_x .req x10
arg_y .req x11
arg_z .req x12
temp0 .req x13
temp1 .req x14
temp2 .req x15
temp3 .req x16
temp4 .req x17
next_method_context .req temp1
nfn .req temp2
fname .req temp3
/* x18 reserved */
save0 .req x19
save1 .req x20
save2 .req x21
save3 .req x22
rnil .req x23
tsp .req x24
vsp .req x25
allocptr .req x26
allocbase .req x27
rcontext .req x28
#endif

DEFCONST(tag_fixnum,       0b000)
DEFCONST(tag_single_float, 0b001)
DEFCONST(tag_imm,          0b010)
DEFCONST(tag_list,         0b011)
DEFCONST(tag_4,            0b100)
DEFCONST(tag_5,            0b101)
DEFCONST(tag_nodeheader,   0b110)
DEFCONST(tag_7,            0b111)

DEFCONST(fulltag_even_fixnum,  0b0000)
DEFCONST(fulltag_single_float, 0b0001)
DEFCONST(fulltag_imm_0,        0b0010)
DEFCONST(fulltag_cons,         0b0011)
DEFCONST(fulltag_misc,         0b0100)
DEFCONST(fulltag_immheader_0,  0b0101)
DEFCONST(fulltag_nodeheader_0, 0b0110)
DEFCONST(fulltag_symbol,       0b0111)
DEFCONST(fulltag_odd_fixnum,   0b1000)
DEFCONST(fulltag_reserved,     0b1001)
DEFCONST(fulltag_imm_1,        0b1010)
DEFCONST(fulltag_nil,          0b1011)
DEFCONST(fulltag_immheader_1,  0b1100)
DEFCONST(fulltag_immheader_2,  0b1101)
DEFCONST(fulltag_nodeheader_1, 0b1110)
DEFCONST(fulltag_function,     0b1111)

DEFCONST(misc_bias, fulltag_misc)
DEFCONST(cons_bias, fulltag_cons)
DEFCONST(misc_header_offset, -fulltag_misc)
DEFCONST(misc_data_offset, misc_header_offset + node_size)
DEFCONST(misc_subtag_offset, misc_header_offset)

#define SUBTAG(fulltag, val) ((fulltag) | ((val) << ntagbits))

DEFCONST(subtag_arrayH, SUBTAG(fulltag_nodeheader_0, 10))
DEFCONST(subtag_vectorH, SUBTAG(fulltag_nodeheader_1, 10))
DEFCONST(subtag_simple_vector, SUBTAG(fulltag_nodeheader_1, 11))

DEFCONST(ivector_class_64_bit, fulltag_immheader_2)
DEFCONST(ivector_class_32_bit, fulltag_immheader_1)
DEFCONST(ivector_class_other_bit, fulltag_immheader_0)

DEFCONST(subtag_complex_single_float_vector, SUBTAG(ivector_class_64_bit, 11))
DEFCONST(subtag_fixnum_vector, SUBTAG(ivector_class_64_bit, 12))
DEFCONST(subtag_s64_vector, SUBTAG(ivector_class_64_bit, 13))
DEFCONST(subtag_u64_vector, SUBTAG(ivector_class_64_bit, 14))
DEFCONST(subtag_double_float_vector, SUBTAG(ivector_class_64_bit, 15))

DEFCONST(subtag_simple_base_string, SUBTAG(ivector_class_32_bit, 12))
DEFCONST(subtag_s32_vector, SUBTAG(ivector_class_32_bit, 13))
DEFCONST(subtag_u32_vector, SUBTAG(ivector_class_32_bit, 14))
DEFCONST(subtag_single_float_vector, SUBTAG(ivector_class_32_bit, 15))

DEFCONST(subtag_complex_double_float_vector, SUBTAG(ivector_class_other_bit,9))
DEFCONST(min_cl_ivector_subtag, subtag_complex_double_float_vector)
DEFCONST(subtag_s16_vector, SUBTAG(ivector_class_other_bit, 9))
DEFCONST(subtag_u16_vector, SUBTAG(ivector_class_other_bit, 11))
/* missing 12 was an 8-bit simple base string */
DEFCONST(subtag_s8_vector, SUBTAG(ivector_class_other_bit, 13))
DEFCONST(subtag_u8_vector, SUBTAG(ivector_class_other_bit, 14))
DEFCONST(subtag_bit_vector, SUBTAG(ivector_class_other_bit, 15))

DEFCONST(subtag_macptr, SUBTAG(ivector_class_64_bit, 1))
DEFCONST(subtag_dead_macptr, SUBTAG(ivector_class_64_bit, 2))

DEFCONST(subtag_bignum, SUBTAG(ivector_class_32_bit, 1))
DEFCONST(subtag_double_float, SUBTAG(ivector_class_32_bit, 2))
DEFCONST(subtag_xcode_vector, SUBTAG(ivector_class_32_bit, 3))
DEFCONST(subtag_complex_single_float, SUBTAG(ivector_class_32_bit, 4))
DEFCONST(subtag_complex_double_float, SUBTAG(ivector_class_32_bit, 5))
DEFCONST(subtag_code_vector, SUBTAG(ivector_class_32_bit, 6))

DEFCONST(subtag_symbol, SUBTAG(fulltag_nodeheader_0, 1))
DEFCONST(subtag_catch_frame, SUBTAG(fulltag_nodeheader_0, 2))
DEFCONST(subtag_hash_vector, SUBTAG(fulltag_nodeheader_0, 3))
DEFCONST(subtag_pool, SUBTAG(fulltag_nodeheader_0, 4))
DEFCONST(subtag_weak, SUBTAG(fulltag_nodeheader_0, 5))
DEFCONST(subtag_package, SUBTAG(fulltag_nodeheader_0, 6))
DEFCONST(subtag_slot_vector, SUBTAG(fulltag_nodeheader_0, 7))
DEFCONST(subtag_basic_stream, SUBTAG(fulltag_nodeheader_0, 8))
DEFCONST(subtag_function, SUBTAG(fulltag_nodeheader_0, 9))

DEFCONST(subtag_ratio, SUBTAG(fulltag_nodeheader_1, 1))
DEFCONST(subtag_complex, SUBTAG(fulltag_nodeheader_1, 2))
DEFCONST(subtag_struct, SUBTAG(fulltag_nodeheader_1, 3))
DEFCONST(subtag_istruct, SUBTAG(fulltag_nodeheader_1, 4))
DEFCONST(subtag_value_cell, SUBTAG(fulltag_nodeheader_1, 5))
DEFCONST(subtag_xfunction, SUBTAG(fulltag_nodeheader_1, 6))
DEFCONST(subtag_lock, SUBTAG(fulltag_nodeheader_1, 7))
DEFCONST(subtag_instance, SUBTAG(fulltag_nodeheader_1, 8))

DEFCONST(subtag_character, SUBTAG(fulltag_imm_0, 0))

DEFCONST(subtag_unbound, SUBTAG(fulltag_imm_1, 1))
DEFCONST(unbound_marker, subtag_unbound)
DEFCONST(undefined, unbound_marker)
DEFCONST(subtag_slot_unbound, SUBTAG(fulltag_imm_1, 2))
DEFCONST(slot_unbound_marker, subtag_slot_unbound)
DEFCONST(subtag_illegal, SUBTAG(fulltag_imm_1, 3))
DEFCONST(illegal_marker, subtag_illegal)
DEFCONST(subtag_no_thread_local_binding, SUBTAG(fulltag_imm_1, 4))
DEFCONST(no_thread_local_binding_marker, subtag_no_thread_local_binding)
DEFCONST(subtag_lisp_frame_marker, SUBTAG(fulltag_imm_1, 5))
DEFCONST(lisp_frame_marker, subtag_lisp_frame_marker)


/* struct definitions */

#ifdef __ASSEMBLER__
/*
 * A struct definition generates a set of assembler equates: for each field,
 * STRUCT.field is the byte offset of that field from a suitably tagged
 * pointer, and STRUCT.size is the total size.  Example:
 *
 *      _struct cons, -cons_bias
 *      _node cdr
 *      _node car
 *      _ends
 *
 * yields cons.cdr = -cons_bias, cons.car = -cons_bias + node_size, and
 * cons.size = 2 * node_size.
 *
 * Keep this working on both clang and GNU as.
 */

.macro _struct name, bias=0
        .set _struct_org, \bias
        .set _struct_base, _struct_org

        .macro _field fld, sz                 /* generic, explicit size */
                .set \name\().\fld, _struct_org
                .set _struct_org, _struct_org + \sz
        .endm
        .macro _rfield fld, sz                /* predecrement */
                .set _struct_org, _struct_org - \sz
                .set \name\().\fld, _struct_org
        .endm
        .macro _halfword fld
                .set \name\().\fld, _struct_org
                .set _struct_org, _struct_org + 2
        .endm
        .macro _word fld
                .set \name\().\fld, _struct_org
                .set _struct_org, _struct_org + 4
        .endm
        .macro _dword fld
                .set \name\().\fld, _struct_org
                .set _struct_org, _struct_org + 8
        .endm
        .macro _node fld
                .set \name\().\fld, _struct_org
                .set _struct_org, _struct_org + node_size
        .endm
        .macro _rnode fld
                .set _struct_org, _struct_org - node_size
                .set \name\().\fld, _struct_org
        .endm
        .macro _endstructf
                .set \name\().element_count, ((_struct_org - node_size) - _struct_base) / node_size
                _ends
        .endm
        .macro _ends
                .set \name\().size, _struct_org - _struct_base
                .purgem _field
                .purgem _rfield
                .purgem _halfword
                .purgem _word
                .purgem _dword
                .purgem _node
                .purgem _rnode
                .purgem _endstructf
                .purgem _ends
        .endm
.endm
/* Fixed-size lisp object: one-word header, accessed via a fulltag_misc
   pointer.  Also defines STRUCT.element_count (see _endstructf). */
.macro _structf name, bias=-misc_bias
        _struct \name, \bias
        _node header
.endm
#endif

#ifdef __ASSEMBLER__
_struct cons, -cons_bias
  _node cdr
  _node car
_ends

_structf ratio
  _node numer
  _node denom
_endstructf

_structf double_float
  _word value
  _word val_low
_endstructf

_structf complex_single_float
  _word realpart
  _word imagpart
_endstructf

_structf complex_double_float
_node pad
  _field realpart, 8
  _field imagpart, 8
_endstructf

_structf macptr
  _node address
  _node domain
  _node type
_endstructf
#endif

DEFCONST(bigit_size, 4)
DEFCONST(two_digit_bignum_header, ((2<<num_subtag_bits)|subtag_bignum))

#define aligned_bignum_size(ndigits)                                    \
  ((node_size + (bigit_size*(ndigits)) + (dnode_size-1)) & ~(dnode_size-1))

/* thread context record struct */
#ifdef __ASSEMBLER__
_struct tcr
  _node next            /* in doubly-linked list   */
  _node prev            /* in doubly-linked list   */
  _node single_float_convert
  _node linear          /* our linear  non-segment-based address.   */
  _node save_fp         /* lisp RBP when in foreign code    */
  _word lisp_mxcsr
  _word foreign_mxcsr
  _node db_link         /* special binding chain head   */
  _node catch_top       /* top catch frame   */
  _node save_vsp        /* VSP when in foreign code   */
  _node save_tsp        /* TSP when in foreign code   */
  _node foreign_sp      /* Saved foreign SP when in lisp code   */
  _node cs_area         /* cstack area pointer   */
  _node vs_area         /* vstack area pointer   */
  _node ts_area         /* tstack area pointer   */
  _node cs_limit        /* cstack overflow limit   */
  _dword bytes_consed
  _node log2_allocation_quantum
  _node interrupt_pending
  _node xframe          /* per-thread exception frame list   */
  _node errno_loc       /* per-thread  errno location   */
  _node ffi_exception   /* mxcsr exception bits from ff-call   */
  _node osid            /* OS thread id   */
  _node valence         /* odd when in foreign code       */
  _node foreign_exception_status
  _node native_thread_info
  _node native_thread_id
  _node last_allocptr
  _node save_allocptr
  _node save_allocbase
  _node reset_completion
  _node activate
  _node suspend_count
  _node suspend_context
  _node pending_exception_context
  _node suspend         /* semaphore for suspension notify   */
  _node resume          /* sempahore for resumption notify   */
  _node flags
  _node gc_context
  _node termination_semaphore
  _node unwinding
  _node tlb_limit
  _node tlb_pointer     /* Consider using tcr+N as tlb_pointer   */
  _node shutdown_count
  _node next_tsp
  _node safe_ref_address
  _node pending_io_info
  _node io_datum
  _node nfp
_ends
#else
typedef struct tcr {
  struct tcr *next;
  struct tcr *prev;
  struct {
    uint32_t tag;
    float f;
  } single_float_convert;
  struct tcr* linear;
  LispObj *save_fp;            /* RBP when in foreign code */
  uint32_t lisp_mxcsr;
  uint32_t foreign_mxcsr;
  special_binding* db_link;     /* special binding chain head */
  LispObj catch_top;            /* top catch frame */
  LispObj *save_vsp;  /* VSP when in foreign code */
  LispObj *save_tsp;  /* TSP when in foreign code */
  LispObj *foreign_sp;
  struct area *cs_area; /* cstack area pointer */
  struct area *vs_area; /* vstack area pointer */
  struct area *ts_area; /* tstack area pointer */
  LispObj cs_limit;             /* stack overflow limit */
  natural bytes_allocated;
  natural log2_allocation_quantum;      /* for per-thread consing */
  signed_natural interrupt_pending;     /* pending interrupt flag */
  xframe_list *xframe; /* exception-frame linked list */
  int *errno_loc;               /* per-thread (?) errno location */
  LispObj ffi_exception;        /* fpscr bits from ff-call */
  LispObj osid;                 /* OS thread id */
  signed_natural valence;                       /* odd when in foreign code */
  signed_natural foreign_exception_status;      /* non-zero -> call lisp_exit_hook */
  void *native_thread_info;     /* platform-dependent */
  void *native_thread_id;       /* mach_thread_t, pid_t, etc. */
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
  void *pending_io_info;
  void *io_datum;
  void *nfp;
} TCR;
#endif
