/* SPDX-License-Identifier: Apache-2.0 */

/*
 * This stuff must match the contents of arm64-arch.lisp and arm64-arm.lisp
 * in ccl:compiler;ARM64;
 *
 * N.B.: This file in included in both .c and .s files.
 */
#pragma once

#ifndef __ASSEMBLER__
#include "lisptypes.h"
#endif

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
DEFCONST(fixnummask, 7)
DEFCONST(subtagmask, ((1<<num_subtag_bits)-1))
DEFCONST(ncharcodebits, 8)
DEFCONST(charcode_shift, 8)
DEFCONST(node_size, 8)
#undef dnode_size
DEFCONST(dnode_size, 16)
DEFCONST(node_shift, 3)
DEFCONST(nargregs, 3)
DEFCONST(nsaveregs, 4)

DEFCONST(call_arguments_limit, 0x10000)
DEFCONST(heap_segment_size, 0x20000)
DEFCONST(log2_heap_segment_size, 17)
// XXX - This is not going to work on macOS
DEFCONST(STATIC_BASE_ADDRESS, 0x03fff000)

/* lisp names for registers */
#ifdef __ASSEMBLER__
imm0 .req x0
imm1 .req x1
imm2 .req x2
imm3 .req x3
imm4 .req x4
imm5 .req x5
nargs .req x6
fn .req x7
arg_w .req x8
arg_x .req x9
arg_y .req x10
arg_z .req x11
temp0 .req x12
temp1 .req x13
temp2 .req x14
temp3 .req x15
temp4 .req x16
temp5 .req x17
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

/* register numbers */
DEFCONST(Rimm0, 0)
DEFCONST(Rimm1, 1)
DEFCONST(Rimm2, 2)
DEFCONST(Rimm3, 3)
DEFCONST(Rimm4, 4)
DEFCONST(Rimm5, 5)
DEFCONST(Rnargs, 6)
DEFCONST(Rfn, 7)
DEFCONST(Rarg_w, 8)
DEFCONST(Rarg_x, 9)
DEFCONST(Rarg_y, 10)
DEFCONST(Rarg_z, 11)
DEFCONST(Rtemp0, 12)
DEFCONST(Rtemp1, 13)
DEFCONST(Rtemp2, 14)
DEFCONST(Rtemp3, 15)
DEFCONST(Rtemp4, 16)
DEFCONST(Rtemp5, 17)
DEFCONST(Rnext_method_context, Rtemp1)
DEFCONST(Rnfn, Rtemp2)
DEFCONST(Rfname, Rtemp3)
/* x18 reserved */
DEFCONST(Rsave0, 19)
DEFCONST(Rsave1, 20)
DEFCONST(Rsave2, 21)
DEFCONST(Rsave3, 22)
DEFCONST(Rrnil, 23)
DEFCONST(Rtsp, 24)
DEFCONST(Rvsp, 25)
DEFCONST(Rallocptr, 26)
DEFCONST(Rallocbase, 27)
DEFCONST(Rrcontext, 28)
DEFCONST(Rfp, 29)
DEFCONST(Rlr, 30)
DEFCONST(Rsp, 31)
DEFCONST(Rzr, 31)

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
DEFCONST(fulltag_immheader_0,  0b0100)
DEFCONST(fulltag_immheader_1,  0b0101)
DEFCONST(fulltag_nodeheader_0, 0b0110)
DEFCONST(fulltag_symbol,       0b0111)
#ifndef __ASSEMBLER__
/* gc.h:96 and plsym.c both test this name with #ifdef, not #if defined-value:
 * x86-constants64.h:96 defines fulltag_symbol as a real preprocessor macro, so
 * those tests work there.  A DEFCONST makes an ENUM member, which #ifdef cannot
 * see, so arm64 silently compiled the fulltag_misc branch of is_symbol_fulltag --
 * and symbols are fulltag 7 here, so the predicate never matched anything.
 *
 * Consequence, gdb-confirmed: on a full GC, GCTWA (gc-common.c:1768/1804) neither
 * rescued worthy package-itab symbols nor scrubbed dead ones to unbound_marker, so
 * compact_dynamic_heap forwarded itab references to unmarked symbols == heap
 * corruption after the first full GC following heavy interning (an in-image
 * compile-file will do it).  Bug() backtrace was compact_dynamic_heap ->
 * node_forwarding_address with tag_n 7.
 *
 * The self-referential define keeps the enum as the VALUE while making the NAME
 * visible to #ifdef.  It also heals plsym.c:24 describe_symbol.  fulltag_symbol is
 * the only DEFCONST name the preprocessor tests anywhere in kernel C (checked by
 * comm of the DEFCONST names against the ifdef names). */
#define fulltag_symbol fulltag_symbol
#endif
DEFCONST(fulltag_odd_fixnum,   0b1000)
DEFCONST(fulltag_reserved,     0b1001)
DEFCONST(fulltag_imm_1,        0b1010)
DEFCONST(fulltag_nil,          0b1011)
DEFCONST(fulltag_misc,         0b1100)
DEFCONST(fulltag_immheader_2,  0b1101)
DEFCONST(fulltag_nodeheader_1, 0b1110)
DEFCONST(fulltag_15,           0b1111)

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
DEFCONST(subtag_s16_vector, SUBTAG(ivector_class_other_bit, 10))
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
DEFCONST(unbound, unbound_marker)
DEFCONST(undefined, unbound_marker)
DEFCONST(subtag_slot_unbound, SUBTAG(fulltag_imm_1, 2))
DEFCONST(slot_unbound_marker, subtag_slot_unbound)
DEFCONST(slot_unbound, subtag_slot_unbound)
DEFCONST(subtag_illegal, SUBTAG(fulltag_imm_1, 3))
DEFCONST(illegal_marker, subtag_illegal)
DEFCONST(subtag_no_thread_local_binding, SUBTAG(fulltag_imm_1, 4))
DEFCONST(no_thread_local_binding_marker, subtag_no_thread_local_binding)
DEFCONST(subtag_lisp_frame_marker, SUBTAG(fulltag_imm_1, 5))
DEFCONST(lisp_frame_marker, subtag_lisp_frame_marker)


/*
 * The generic C constants (special_binding, hash_table_vector_header, the
 * TCR flag bits, INTERRUPT_LEVEL_BINDING_INDEX, ...) that the struct tcr below
 * and much of the C kernel depend on.  x86/ppc reach constants.h through their
 * C-only arch headers; because this header is shared by the assembler too, we
 * pull it in ourselves, guarded out of the assembler pass.
 */
#ifndef __ASSEMBLER__
#include "constants.h"
#define fixnum_bitmask(n)  (1LL<<((n)+fixnumshift))
#endif

/* struct definitions */

#ifdef __ASSEMBLER__
/*
 * A struct definition generates a set of assembler equates: for each field,
 * name.field is the byte offset of that field from a suitably tagged
 * pointer, and name.size is the total size.  Example:
 *
 *      _struct cons, -cons_bias
 *      _node cdr
 *      _node car
 *      _ends
 *
 * yields cons.cdr = -cons_bias, cons.car = -cons_bias + node_size, and
 * cons.size = 2 * node_size.
 *
 * This needs to work on both clang and GNU as.
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
        .macro _struct_label fld
                .set \name\().\fld, _struct_org
        .endm
        .macro _struct_pad n                  /* advance without a label */
                .set _struct_org, _struct_org + \n
        .endm
        .macro _endstructf
                .set \name\().element_count, \
                     ((_struct_org - node_size) - _struct_base) / node_size
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
                .purgem _struct_label
                .purgem _struct_pad
                .purgem _endstructf
                .purgem _ends
        .endm
.endm
/* Fixed-size lisp object: one-word header, accessed via a fulltag_misc
   pointer.  Also defines name.element_count (see _endstructf). */
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

/* A symbol has its own tag, but is otherwise a miscobj */
_structf symbol, -fulltag_symbol
  _node pname
  _node vcell
  _node fcell
  _node package_predicate
  _node flags
  _node plist
  _node binding_index
_endstructf

_struct _function, -fulltag_misc
 _node header
 _node code_vector
_ends

_structf vectorH
 _node logsize
 _node physsize
 _node data_vector
 _node displacement
 _node flags
_endstructf

_structf arrayH
 _node rank
 _node physsize
 _node data_vector
 _node displacement
 _node flags
 _struct_label dim0
_endstructf

_structf macptr
  _node address
  _node domain
  _node type
_endstructf

INTERRUPT_LEVEL_BINDING_INDEX = (1 << fixnumshift)

_struct binding
 _node link
 _node sym
 _node val
_ends

_struct tsp_frame
 _node backlink
 _node type
 _struct_label fixed_overhead
 _struct_label data_offset
_ends

_struct lisp_frame
 _node marker
 _node savevsp
 _node savefn
 _node savelr
_ends

_structf catch_frame
 _node catch_tag                /* #<unbound> -> unwind-protect, else catch */
 _node link                     /* backpointer to previous catch frame */
 _node mvflag                   /* 0 if single-valued catch, else fixnum 1 */
 _node db_link                  /* head of special-binding chain */
 _field regs, 4*node_size       /* save0 through save3 */
 _node xframe                   /* exception frame chain */
 _node nfp
_endstructf

_struct c_frame
 _node header
 _node savedsp
 _struct_label params
_ends

TCR_STATE_LISP = 0
TCR_STATE_FOREIGN = 1
#endif

DEFCONST(two_digit_bignum_header, ((2<<num_subtag_bits)|subtag_bignum))
DEFCONST(three_digit_bignum_header, ((3<<num_subtag_bits)|subtag_bignum))
DEFCONST(four_digit_bignum_header, ((4<<num_subtag_bits)|subtag_bignum))
/* bignum digits are 32 bits even though they could be 64 bits */
DEFCONST(bigit_size, 4)
#define aligned_bignum_size(ndigits) \
  ((node_size + (bigit_size*(ndigits)) + (dnode_size-1)) & ~(dnode_size-1))

#if !defined(__ASSEMBLER__)
typedef struct xframe_list {
  ExceptionInformation *curr;
  struct xframe_list *prev;
} xframe_list;
#endif

#ifdef __ASSEMBLER__

/* Symbol bits that we care about */
sym_vbit_bound = (0+fixnumshift)
sym_vbit_bound_mask = (1<<sym_vbit_bound)
sym_vbit_const = (1+fixnumshift)
sym_vbit_const_mask = (1<<sym_vbit_const)

_struct area
  _node pred
  _node succ
  _node low
  _node high
  _node active
  _node softlimit
  _node hardlimit
   _node code
  _node markbits
  _node ndwords
  _node older
  _node younger
  _node h
  _node sofprot
  _node hardprot
  _node owner
  _node refbits
  _node nextref
_ends

/* thread context record struct */
_struct tcr
  _node next            /* in doubly-linked list   */
  _node prev            /* in doubly-linked list   */
  _node db_link         /* special binding chain head   */
  _node catch_top       /* top catch frame   */
  _node last_lisp_frame
  _node save_vsp        /* VSP when in foreign code   */
  _node save_tsp        /* TSP when in foreign code   */
  _node cs_area         /* cstack area pointer   */
  _node vs_area         /* vstack area pointer   */
  _node ts_area         /* tstack area pointer   */
  _node cs_limit        /* cstack overflow limit   */
  _dword bytes_allocated
  _node log2_allocation_quantum
  _node interrupt_pending
  _node xframe          /* per-thread exception frame list   */
  _node errno_loc       /* per-thread  errno location   */
  _node foreign_fpsr    /* fpsr exception bits on return from ff-call */
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
  _node safe_ref_address
  _node io_datum /* Darwin: Mach thread exception port */
  _node nfp
  _field spare, 20*node_size
  _field sptab, 256*node_size   /* subprims table (see arm64-spentry.s) */
_ends

.if tcr.spare - 336
.error "tcr.spare moved; sync tcr.spare in arm64-arch.lisp"
.endif
.if tcr.sptab - 496
.error "tcr.sptab moved; sync tcr.sptab in arm64-arch.lisp"
.endif
.if tcr.size - 2544
.error "tcr.size changed; re-check arm64-arch.lisp"
.endif

#else

#define TCR_BIAS 0

typedef struct tcr {
  struct tcr *next;
  struct tcr *prev;
  special_binding* db_link;     /* special binding chain head */
  LispObj catch_top;            /* top catch frame */
  LispObj *last_lisp_frame; /* top frame on cstack when in foreign code */
  LispObj *save_vsp;  /* VSP when in foreign code */
  LispObj *save_tsp;  /* TSP when in foreign code */
  struct area *cs_area; /* cstack area pointer */
  struct area *vs_area; /* vstack area pointer */
  struct area *ts_area; /* tstack area pointer */
  LispObj cs_limit;             /* stack overflow limit */
  natural bytes_allocated;
  natural log2_allocation_quantum;      /* for per-thread consing */
  signed_natural interrupt_pending;     /* pending interrupt flag */
  xframe_list *xframe; /* exception-frame linked list */
  int *errno_loc;               /* per-thread (?) errno location */
  LispObj foreign_fpsr;        /* fpsr bits from ff-call */
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
  void *safe_ref_address;
  void *io_datum;               /* Darwin: Mach thread exception port */
  void *nfp;
  LispObj spare[20];
  LispObj sptab[256];           /* subprims table (see arm64-spentry.s) */
} TCR;

typedef struct lisp_frame {
  LispObj marker;
  LispObj savevsp;
  LispObj savefn;
  LispObj savelr;
} lisp_frame;

#include <stddef.h>
#include <assert.h>
/*
 * Try to detect struct tcr layout changes that require corresponding
 * udpates in arm64-arch.lisp.
 */
static_assert(offsetof(TCR, spare) == 336,
               "TCR.spare changed; update tcr.spare in arm64-arch.lisp");
static_assert(offsetof(TCR, sptab) == 496,
               "TCR.sptab changed; update tcr.sptab in arm64-arch.lisp");
static_assert(sizeof(TCR) == 2544,
               "sizeof(TCR) changed; update arm64-arch.lisp");

#define ABI_VERSION_MIN 1
#define ABI_VERSION_CURRENT 1046
#define ABI_VERSION_MAX 1046

#endif
