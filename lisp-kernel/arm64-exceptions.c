/* SPDX-License-Identifier: Apache-2.0 */

/* PPC64 LINE-PORT (source: vendor/ccl/lisp-kernel/ppc-exceptions.c)
 *
 * Exception/trap handling for Matt Emerson's upstream ARM64 (low-tag)
 * design; the missing arm64-exceptions.o in linuxarm64/Makefile.
 * Tree pin: upstream-arm64-tip @ d71a5ad.
 *
 * PORT-NOTE — deviations from the PPC64 source (each tagged inline):
 *  1. Trap encoding: PPC tw/twi/td/tdi conditional traps + UUO major
 *     opcodes are replaced by the `udf #imm16' UUO scheme.  Namespace
 *     source: compiler/ARM64/arm64-asm.lisp:435-450 (Matt's active
 *     layer; lead ruling 2026-07-10 — arm64-uuo.s's hlt scheme is
 *     2012-WIP legacy and is NOT decoded here).
 *     ARM64 trap sites have already BRANCHED (conditional traps become
 *     b.cond around an unconditional udf), so is_conditional_trap()
 *     and PPC's TO-field condition re-evaluation have no analog here.
 *  2. Signal mechanics (sigaltstack, SIGILL routing) follow
 *     arm-exceptions.c (ARM-family Linux reference) where PPC has no
 *     analog; such blocks are tagged ARM64-DEVIATION with line cites.
 *  3. Alloc-sequence emulation: the inline allocator is
 *     arm64-macros.s:36-69 (Cons/Misc_Alloc/Misc_Alloc_Fixed): a
 *     multi-instruction sub/cmp/b.hi/udf sequence, so pc_luser_xp uses
 *     arm-exceptions.c's classify/restart approach (arm-exceptions.c:
 *     1624-1698) rather than PPC's two-instruction back-decode.
 *  4. No FE0/FE1 MSR bits, no PR_SET_FPEXC prctl on AArch64:
 *     enable/disable_fp_exceptions are empty (arm-exceptions.c:55-66).
 *  5. TCR layout: arm64-constants.h C-side struct (W4-D20: where
 *     arm64-arch.lisp disagrees, constants.h is followed).
 *
 * Register numbers: map authority is compiler/ARM64/arm64-asm.lisp:183-215
 * (Matt's active layer; lead ruling 2026-07-10): imm5=x5 is DISTINCT from
 * nargs=x6; all other assignments as in arm64-constants.h's `.req' block
 * (arg_z=x12, temp0=x13, rnil=x23, tsp=x24, ...).  arm64-constants.h has
 * exactly one stale line (`imm5 .req x6'); arm64-constants.s's m4 map is
 * wholly stale (rnil=x6, high-tag residue).  See
 * drafts/arm64-exceptions-report.md.
 */

#include "lisp.h"                 /* ppc-exceptions.c:17 */
#include "lisp-errors.h"          /* error_* trap codes (shared table) */
#include "lisp-exceptions.h"      /* ppc-exceptions.c:18 */
#include "lisp_globals.h"         /* ppc-exceptions.c:19 */
#include <ctype.h>                /* ppc-exceptions.c:20 */
#include <stdio.h>                /* ppc-exceptions.c:21 */
#include <stddef.h>               /* ppc-exceptions.c:22 */
#include <string.h>               /* ppc-exceptions.c:23 */
#include <stdarg.h>               /* ppc-exceptions.c:24 */
#include <errno.h>                /* ppc-exceptions.c:25 */
#include <stdio.h>                /* ppc-exceptions.c:26 */
#ifdef LINUX                      /* ppc-exceptions.c:27-31 */
#include <strings.h>
#include <sys/mman.h>
#endif

#include "threads.h"              /* ppc-exceptions.c:49 */

/* ------------------------------------------------------------------ */
/* lisp_globals.h grew a real ARM64 branch @ 93d72a0 (nil-anchored via
 * the runtime lisp_nil) -- the fixed-address PROPOSED shim that lived
 * here is retired; lisp_globals.h owns lisp_global/nrs_symbol now. */

/* PROPOSED: canonical NIL/T addresses (spentry-A-alloc-numbers.s:96-101;
 * derivation: Matt's compiler/ARM64/arm64-arch.lisp canonical-nil-value/
 * canonical-t-value offsets; absolute base 0x13000 provisional). */
#ifndef nil_value
#define nil_value ((LispObj)(0x13000+fulltag_nil+(LOWMEM_BIAS)))
#endif
#ifndef t_offset
#define t_offset ((0x13020+fulltag_symbol)-(0x13000+fulltag_nil))
#endif
#ifndef t_value
#define t_value (nil_value+t_offset)
#endif

/* ------------------------------------------------------------------ */
/* C-side register numbers — authority: arm64-constants.h R* numbers
 * @ pin 115b7aa (map unified upstream at 01d73c3). */
enum {
  imm0 = 0,
  imm1 = 1,
  imm2 = 2,
  imm3 = 3,
  imm4 = 4,
  imm5 = 5,        /* arm64-asm.lisp: imm5=x5, DISTINCT from nargs */
  nargs = 6,       /* arm64-asm.lisp: nargs=x6 */
  fn = 7,          /* fn=x7 */
  arg_w = 8,       /* renumbered @ upstream 01d73c3 (map unified) */
  arg_x = 9,
  arg_y = 10,
  arg_z = 11,
  temp0 = 12,
  temp1 = 13,
  temp2 = 14, nfn = 14,
  temp3 = 15, fname = 15,
  temp4 = 16,
  temp5 = 17,      /* NEW boxed temp @ 01d73c3 */
  save0 = 19,
  save1 = 20,
  save2 = 21,
  save3 = 22,
  rnil = 23,
  tsp = 24,        /* Matt's design HAS a tsp register */
  vsp = 25,
  allocptr = 26,
  allocbase = 27,
  rcontext = 28
  /* Rlr(30)/Rsp(31) are now provided by his arm64-constants.h DEFCONST
     register table (RECONCILED 556aebe8: our duplicates dropped; our code's
     Rlr/Rsp references resolve to his, same values).  AArch64 SP is not one
     of regs[0..30]; Rsp=31 is the selector for stack-register arguments. */
};

/* ------------------------------------------------------------------ */
/* PROPOSED C-side struct overlays (ratify with Matt; arm64-constants.h
 * has no C struct block yet).  lisp_frame moved to
 * platform-linuxarm64.h (albt.c needs it too).
 *
 * catch_frame, laid on the UNTAGGED uvector base (untag(tcr->catch_top);
 * header @0, first data slot @8: fulltag_misc = 4 + misc_data_offset = 4).
 * Field order: spentry-C-bind-catch-throw.s:165-177 _structf catch_frame
 * (PPC64 layout, regs[] sized to this design's nsaveregs = 4). */
typedef struct catch_frame {
  LispObj header;
  LispObj catch_tag;              /* unbound_marker => unwind-protect  */
  LispObj link;                   /* previous catch frame              */
  LispObj mvflag;
  LispObj csp;                    /* saved control-stack lisp_frame    */
  LispObj db_link;
  LispObj regs[4];                /* save0..save3                      */
  LispObj xframe;
  LispObj nfp;
} catch_frame;

/* Forward declarations (Matt's arm64-exceptions.h is empty; PPC keeps
 * these in ppc-exceptions.h). */
OSStatus handle_error(ExceptionInformation *, unsigned, unsigned,
                      unsigned, pc);
Boolean extend_tcr_tlb(TCR *, ExceptionInformation *, unsigned);
void pc_luser_xp(ExceptionInformation *, TCR *, signed_natural *);
void adjust_exception_pc(ExceptionInformation *, int);
OSStatus handle_uuo(ExceptionInformation *, opcode, pc, siginfo_t *);
void callback_for_trap(LispObj, ExceptionInformation *, pc, natural,
                       natural, natural);
void callback_to_lisp(LispObj, ExceptionInformation *, natural, natural,
                      natural, natural, natural);

/* ------------------------------------------------------------------ */
/* udf #imm16 UUO decode.  CANON: Matt's lisp-kernel/arm64-uuo.s after
 * the c9e7ffb renumber (pin 6b6540e) -- misc moved to format 0 (info
 * must never be all-0 because udf #0 is the code-vector sentinel),
 * unary infos renumbered and given udf/udf_call, binary fleshed out,
 * and all type errors moved to a new wrong_type format 3.
 *
 * A64 UDF encoding: the permanently-undefined space, instruction word
 * 0x0000IIII (top 16 bits zero, imm16 in the low 16).  Linux/AArch64
 * delivers it as SIGILL.
 *
 * imm16 low 2 BITS = format:
 *   0 misc: 14 bits of info in 15:2, NEVER all-0.  His values 1-8
 *     (1 alloc, 2 gc, 3 debug, 4 interrupt_now, 5 suspend_now,
 *     6 too_few_args, 7 too_many_args, 8 wrong_number_of_args).
 *     PROPOSED extension (arm64-globals-proposed.s uuo_interr; ratify):
 *     info bit 13 set = "interr": bits 12:5 = errors.s errnum,
 *     bits 4:0 = register.
 *   1 unary: reg in bits 6:2, 9 bits of info in 15:7 (0 not_callable,
 *     1 no_throw_tag, 2 unbound, 3 udf, 4 udf_call, 5 tlb_too_small).
 *   2 binary: ra in 6:2, rb in 11:7, 4 bits of info in 15:12
 *     (0 vector_bounds, 1 array_bounds, 2 integer_divide_by_zero,
 *     3 eep_unresolved, 4 fpu_exception, 5 array_rank, 6 array_flags,
 *     7 two_registers = extra register pair for the preceding uuo).
 *   3 wrong_type: reg in 6:2, continuable flag in bit 7, expected
 *     xtype code in 15:8.
 */
#define IS_UUO(i)     (((i) & 0xffff0000) == 0)
#define UUO_IMM16(i)  ((i) & 0xffff)

#define uuo_format_misc       0
#define uuo_format_unary      1
#define uuo_format_binary     2
#define uuo_format_wrong_type 3

#define UUO_FORMAT(imm16)      ((imm16) & 3)
#define UUO_UNARY_GPR(imm16)   (((imm16) >> 2) & 0x1f)
#define UUO_UNARY_INFO(imm16)  (((imm16) >> 7) & 0x1ff)
#define UUO_BINARY_RA(imm16)   (((imm16) >> 2) & 0x1f)
#define UUO_BINARY_RB(imm16)   (((imm16) >> 7) & 0x1f)
#define UUO_BINARY_INFO(imm16) (((imm16) >> 12) & 0xf)
#define UUO_MISC_INFO(imm16)   (((imm16) >> 2) & 0x3fff)
#define UUO_WT_GPR(imm16)         (((imm16) >> 2) & 0x1f)
#define UUO_WT_CONTINUABLE(imm16) (((imm16) >> 7) & 1)
#define UUO_WT_XTYPE(imm16)       (((imm16) >> 8) & 0xff)
/* PROPOSED interr extension fields (misc info bit 13 set) */
#define UUO_MISC_IS_INTERR(mi) (((mi) >> 13) & 1)
#define UUO_INTERR_ERRNUM(mi)  (((mi) >> 5) & 0xff)
#define UUO_INTERR_GPR(mi)     ((mi) & 0x1f)

/* unary infos (arm64-uuo.s unary_info_*) */
#define uuo_unary_not_callable  0
#define uuo_unary_no_throw_tag  1
#define uuo_unary_unbound       2
#define uuo_unary_udf           3
#define uuo_unary_udf_call      4
#define uuo_unary_tlb_too_small 5
#define uuo_unary_slot_unbound  6   /* patch 0052 (PROPOSED); three-register
                                       error -- see doc/porting/arm64.md */
#define uuo_unary_apply_macro   7   /* patch 0055: funcalled a macro/
                                       special-operator name; gpr = fname */

/* binary infos (arm64-uuo.s binary_info_*) */
#define uuo_binary_vector_bounds  0
#define uuo_binary_array_bounds   1
#define uuo_binary_int_div_zero   2
#define uuo_binary_eep_unresolved 3
#define uuo_binary_fpu_exception  4
#define uuo_binary_array_rank     5
#define uuo_binary_array_flags    6
#define uuo_binary_two_registers  7

/* misc infos (arm64-uuo.s uuo_misc macros; 0 is invalid) */
#define uuo_misc_alloc          1
#define uuo_misc_gc_trap        2
#define uuo_misc_debug_trap     3
#define uuo_misc_interrupt_now  4
#define uuo_misc_suspend_now    5
#define uuo_misc_too_few        6
#define uuo_misc_too_many       7
#define uuo_misc_wrong_number   8

/* Whole-instruction forms tested for directly:
   udf #((info<<2) | uuo_format_misc). */
#define ALLOC_TRAP_INSTRUCTION         (0x00000004)  /* uuo_alloc = udf #4  */
#define GC_TRAP_INSTRUCTION            (0x00000008)  /* uuo_gc_trap         */
#define DEBUG_TRAP_INSTRUCTION         (0x0000000c)  /* uuo_debug_trap      */
#define DEFERRED_INTERRUPT_INSTRUCTION (0x00000010)  /* uuo_interrupt_now   */

/* ------------------------------------------------------------------ */
/* Inline-allocator instruction matchers.
 *
 * The allocation sequence is arm64-macros.s:36-69 (Cons / Misc_Alloc /
 * Misc_Alloc_Fixed):
 *     sub  allocptr, allocptr, {#imm | Xm}
 *     cmp  allocptr, allocbase
 *     b.hi 1f
 *     udf  #3   (uuo_alloc = misc 0; assembler-validated 2026-07-11)
 * 1:  str  <header|cdr>, [allocptr, #misc_header_offset|#cons.cdr]
 *    (str  <car>, [allocptr, #cons.car])
 *     mov  <dest>, allocptr
 *     bic  allocptr, allocptr, #fulltagmask
 *
 * The matchers below hardcode A64 encodings the same way
 * arm-exceptions.h:79-101 hardcodes ARM32 ones; each carries its
 * encoding derivation so the lead can verify against an assembler.
 *
 * allocptr = x26, allocbase = x27 (arm64-constants.h).
 */

/* SUB (immediate): sf=1 op=1 S=0 100010 sh imm12 Rn Rd
   sub x26, x26, #imm12 [, LSL #12]  → 0xd100035a | sh<<22 | imm12<<10 */
#define IS_SUB_IMM_FROM_ALLOCPTR(i) (((i) & 0xff8003ff) == 0xd100035a)
#define SUB_IMM_FIELD(i) ((natural)(((i) >> 10) & 0xfff) << ((((i) >> 22) & 1) ? 12 : 0))

/* SUB (shifted register): sf=1 op=1 S=0 01011 00 0 Rm 000000 Rn Rd
   sub x26, x26, Xm  → 0xcb00035a | Rm<<16 */
#define IS_SUB_RM_FROM_ALLOCPTR(i) (((i) & 0xffe0ffff) == 0xcb00035a)
#define RM_field(i) (((i) >> 16) & 0x1f)

/* SUBS xzr (cmp): cmp x26, x27 → 0xeb1b035f */
#define IS_COMPARE_ALLOCPTR_TO_ALLOCBASE(i) ((i) == 0xeb1b035f)

/* B.cond: 0101 0100 imm19 0 cond; b.hi .+8 → imm19=2, cond=0b1000 */
#define IS_BRANCH_AROUND_ALLOC_TRAP(i) ((i) == 0x54000048)

#define IS_ALLOC_TRAP(i) ((i) == ALLOC_TRAP_INSTRUCTION)

/* STUR (64-bit): 1111 1000 000 imm9 00 Rn Rt
   header store: stur Xt, [x26, #misc_header_offset(-4)]
     imm9 = -4 & 0x1ff = 0x1fc → 0xf8000000|0x1fc<<12|26<<5 = 0xf81fc340 */
#define IS_SET_ALLOCPTR_HEADER_RD(i) (((i) & 0xffffffe0) == 0xf81fc340)
/* cons.cdr = -cons_bias = -3 (arm64-constants.h cons struct):
     stur Xt, [x26, #-3] → imm9 = 0x1fd → 0xf81fd340 */
#define IS_SET_ALLOCPTR_CDR_RD(i)    (((i) & 0xffffffe0) == 0xf81fd340)
/* cons.car = -cons_bias + node_size = +5:
     stur Xt, [x26, #5]  → imm9 = 5 → 0xf8005340 */
#define IS_SET_ALLOCPTR_CAR_RD(i)    (((i) & 0xffffffe0) == 0xf8005340)
#define RD_field(i) ((i) & 0x1f)

/* MOV Xd, x26 = ORR Xd, xzr, x26: 0xaa1a03e0 | Rd */
#define IS_SET_ALLOCPTR_RESULT_RD(i) (((i) & 0xffffffe0) == 0xaa1a03e0)

/* bic allocptr, allocptr, #fulltagmask assembles as
   AND x26, x26, #0xfffffffffffffff0 (N=1 immr=60 imms=59):
   0x92000000|1<<22|60<<16|59<<10|26<<5|26 = 0x927cef5a */
#define IS_CLR_ALLOCPTR_TAG(i)       ((i) == 0x927cef5a)

/* Alloc-sequence classification (ARM64-DEVIATION: arm-exceptions.h:103-111,
   needed because the ARM-family alloc sequence is multi-instruction). */
typedef enum {
  ID_unrecognized_alloc_instruction,
  ID_adjust_allocptr_instruction,
  ID_compare_allocptr_to_allocbase_instruction,
  ID_branch_around_alloc_trap_instruction,
  ID_alloc_trap_instruction,
  ID_finish_allocation
} alloc_instruction_id;

/* ------------------------------------------------------------------ */

#ifndef SA_NODEFER               /* ppc-exceptions.c:35-37 */
#define SA_NODEFER 0
#endif

/* ARM64-DEVIATION (arm-exceptions.c:55-66): no FE0/FE1, no PR_SET_FPEXC
   on AArch64; FP exceptions are untrapped by default (FPCR trap enables
   are IMPDEF and generally RES0 on shipping cores).  PPC's prctl-based
   enable/disable_fp_exceptions (ppc-exceptions.c:57-84) reduce to no-ops. */
void
enable_fp_exceptions()
{
}

void
disable_fp_exceptions()
{
}

/*
  Handle exceptions.
*/

extern LispObj lisp_nil;          /* ppc-exceptions.c:91 */

extern natural lisp_heap_gc_threshold;      /* ppc-exceptions.c:93 */
extern Boolean grow_dynamic_area(natural);  /* ppc-exceptions.c:94 */

int
page_size = 4096;                 /* ppc-exceptions.c:101-102 */

int
log2_page_size = 12;              /* ppc-exceptions.c:104-105 */

/*
  If the PC is pointing to an allocation trap, the previous instruction
  must have decremented allocptr.  Return the non-zero amount by which
  allocptr was decremented.
  (ppc-exceptions.c:111-140; instruction shapes per arm64-macros.s:36-69:
   the sub is 3 instructions before the udf — sub/cmp/b.hi/udf.)
*/
signed_natural
allocptr_displacement(ExceptionInformation *xp)
{
  pc program_counter = xpPC(xp);
  opcode instr = *program_counter, prev_instr;

  if (IS_ALLOC_TRAP(instr)) {   /* ppc-exceptions.c:122 */
    prev_instr = program_counter[-3];  /* sub, cmp, b.hi, udf */

    if (IS_SUB_RM_FROM_ALLOCPTR(prev_instr)) {  /* ppc:123-129 (subf) */
      /* Misc_Alloc: the size register still holds (size - fulltag_misc) */
      return ((signed_natural) xpGPR(xp, RM_field(prev_instr)));
    }
    if (IS_SUB_IMM_FROM_ALLOCPTR(prev_instr)) { /* ppc:130-136 (addi -) */
      return ((signed_natural) SUB_IMM_FIELD(prev_instr));
    }
    Bug(xp, "Can't determine allocation displacement");  /* ppc:137 */
  }
  return 0;                      /* ppc-exceptions.c:139 */
}


/*
  A cons cell's been successfully allocated, but the allocptr's
  still tagged (as fulltag_cons, of course.)  Emulate any instructions
  that might follow the allocation (stores to the car or cdr, an
  assignment to the "result" gpr) that take place while the allocptr's
  tag is non-zero, advancing over each such instruction.  When we're
  done, the cons cell will be allocated and initialized, the result
  register will point to it, the allocptr will be untagged, and
  the PC will point past the instruction that clears the allocptr's
  tag.  (ppc-exceptions.c:143-193; ARM64 instruction shapes from
  arm64-macros.s Cons macro; same structure as arm-exceptions.c:148-172.)
*/
void
finish_allocating_cons(ExceptionInformation *xp)
{
  pc program_counter = xpPC(xp);
  opcode instr;
  LispObj cur_allocptr = xpGPR(xp, allocptr);
  cons *c = (cons *)ptr_from_lispobj(untag(cur_allocptr));

  while (1) {
    instr = *program_counter++;

    if (IS_CLR_ALLOCPTR_TAG(instr)) {       /* ppc:166-170 */
      xpGPR(xp, allocptr) = untag(cur_allocptr);
      xpPC(xp) = program_counter;
      return;
    } else if (IS_SET_ALLOCPTR_CAR_RD(instr)) {  /* ppc:173-174 */
      c->car = xpGPR(xp, RD_field(instr));
    } else if (IS_SET_ALLOCPTR_CDR_RD(instr)) {  /* ppc:176-177 */
      c->cdr = xpGPR(xp, RD_field(instr));
    } else if (IS_SET_ALLOCPTR_RESULT_RD(instr)) {  /* ppc:179-190 */
      xpGPR(xp, RD_field(instr)) = cur_allocptr;
    } else {
      Bug(xp, "Unexpected instruction following cons alloc trap at " LISP ":",
          (LispObj)(program_counter - 1));  /* arm-exceptions.c:203 idiom */
    }
  }
}

/*
  We were interrupted in the process of allocating a uvector; we
  survived the allocation trap, and allocptr is tagged as fulltag_misc.
  Emulate any instructions which store a header into the uvector,
  assign the value of allocptr to some other register, and clear
  allocptr's tag.  Don't expect/allow any other instructions in
  this environment.  (ppc-exceptions.c:195-232.)
*/
void
finish_allocating_uvector(ExceptionInformation *xp)
{
  pc program_counter = xpPC(xp);
  opcode instr;
  LispObj cur_allocptr = xpGPR(xp, allocptr);

  while (1) {
    instr = *program_counter++;
    if (IS_CLR_ALLOCPTR_TAG(instr)) {       /* ppc:213-217 */
      xpGPR(xp, allocptr) = untag(cur_allocptr);
      xpPC(xp) = program_counter;
      return;
    }
    if (IS_SET_ALLOCPTR_HEADER_RD(instr)) { /* ppc:218-220 */
      header_of(cur_allocptr) = xpGPR(xp, RD_field(instr));
    } else if (IS_SET_ALLOCPTR_RESULT_RD(instr)) { /* ppc:221-230 */
      xpGPR(xp, RD_field(instr)) = cur_allocptr;
    } else {
      Bug(xp, "Unexpected instruction following uvector alloc trap at " LISP ":",
          (LispObj)(program_counter - 1));
    }
  }
}


Boolean
allocate_object(ExceptionInformation *xp,
                natural bytes_needed,
                signed_natural disp_from_allocptr,
                TCR *tcr)
{
  area *a = active_dynamic_area;   /* ppc-exceptions.c:235-282 */

  /* Maybe do an EGC */
  if (a->older && lisp_global(OLDEST_EPHEMERAL)) {   /* ppc:244 */
    if (((a->active)-(a->low)) >= a->threshold) {
      gc_from_xp(xp, 0L);
    }
  }

  /* Life is pretty simple if we can simply grab a segment
     without extending the heap.
  */
  if (new_heap_segment(xp, bytes_needed, false, tcr, NULL)) {  /* ppc:253 */
    xpGPR(xp, allocptr) += disp_from_allocptr;
    return true;
  }

  /* It doesn't make sense to try a full GC if the object
     we're trying to allocate is larger than everything
     allocated so far.
  */
  if ((lisp_global(HEAP_END)-lisp_global(HEAP_START)) > bytes_needed) { /* ppc:266 */
    untenure_from_area(tenured_area); /* force a full GC */
    gc_from_xp(xp, 0L);
  }

  /* Try again, growing the heap if necessary */
  if (new_heap_segment(xp, bytes_needed, true, tcr, NULL)) {   /* ppc:272 */
    xpGPR(xp, allocptr) += disp_from_allocptr;
    return true;
  }

  return false;
}

#ifndef XNOMEM                    /* ppc-exceptions.c:284-286 */
#define XNOMEM 10
#endif

void
update_bytes_allocated(TCR* tcr, void *cur_allocptr)
{                                 /* ppc-exceptions.c:288-298 */
  BytePtr
    last = (BytePtr) tcr->last_allocptr,
    current = (BytePtr) cur_allocptr;
  if (last && (cur_allocptr != ((void *)VOID_ALLOCPTR))) {
    tcr->bytes_allocated += last-current;
  }
  tcr->last_allocptr = 0;
}

void
lisp_allocation_failure(ExceptionInformation *xp, TCR *tcr, natural bytes_needed)
{                                 /* ppc-exceptions.c:300-309 */
  /* Couldn't allocate the object.  If it's smaller than some arbitrary
     size (say 128K bytes), signal a "chronically out-of-memory" condition;
     else signal a "allocation request failed" condition.
  */
  xpGPR(xp,allocptr) = xpGPR(xp,allocbase) = VOID_ALLOCPTR;
  handle_error(xp, bytes_needed < (128<<10) ? XNOMEM : error_alloc_failed, 0, 0, xpPC(xp));
}

/*
  Allocate a large list, where "large" means "large enough to
  possibly trigger the EGC several times if this was done
  by individually allocating each CONS."  The number of
  conses in question is in arg_z; on successful return,
  the list will be in arg_z.  (ppc-exceptions.c:311-351.)
*/

Boolean
allocate_list(ExceptionInformation *xp, TCR *tcr)
{
  natural
    nconses = (unbox_fixnum(xpGPR(xp,arg_z))),
    bytes_needed = (nconses << dnode_shift);
  LispObj
    prev = lisp_nil,
    current,
    initial = xpGPR(xp,arg_y);

  if (nconses == 0) {
    /* Silly case */
    xpGPR(xp,arg_z) = lisp_nil;
    xpGPR(xp,allocptr) = lisp_nil;
    return true;
  }
  update_bytes_allocated(tcr, (void *)(void *) tcr->save_allocptr);
  if (allocate_object(xp,bytes_needed,(-bytes_needed)+fulltag_cons,tcr)) {
    /* ppc:338-343: after allocate_object, allocptr is tagged fulltag_cons
       and points (tagged) at the FIRST (lowest-addressed) cons.  Chain
       upward: deref(current,0) is the CDR (cons.cdr = untag+0 in Matt's
       layout, arm64-constants.h cons struct — same slot order as PPC64),
       deref(current,1) the CAR. */
    for (current = xpGPR(xp,allocptr);
         nconses;
         prev = current, current+= dnode_size, nconses--) {
      deref(current,0) = prev;      /* cdr */
      deref(current,1) = initial;   /* car */
    }
    xpGPR(xp,arg_z) = prev;
    xpGPR(xp,arg_y) = xpGPR(xp,allocptr);
    xpGPR(xp,allocptr)-=fulltag_cons;
  } else {
    lisp_allocation_failure(xp,tcr,bytes_needed);
  }
  return true;
}

OSStatus
handle_alloc_trap(ExceptionInformation *xp, TCR *tcr)
{                                 /* ppc-exceptions.c:353-414 */
  natural cur_allocptr, bytes_needed = 0;
  signed_natural disp = 0;
  unsigned allocptr_tag;

  cur_allocptr = xpGPR(xp,allocptr);
  allocptr_tag = fulltag_of(cur_allocptr);

  switch (allocptr_tag) {
  case fulltag_cons:              /* ppc:368-371 */
    bytes_needed = sizeof(cons);
    disp = -sizeof(cons) + fulltag_cons;
    break;

  case fulltag_even_fixnum:       /* ppc:373-375 */
  case fulltag_odd_fixnum:
    break;

  case fulltag_misc:              /* ppc:377-395 */
    /* On PPC this decoded the previous (subf/addi) instruction inline;
       here the same decode lives in allocptr_displacement() (the sub is
       3 instructions back per arm64-macros.s Misc_Alloc). */
    disp = -allocptr_displacement(xp);
    if (disp) {
      bytes_needed = (-disp) + fulltag_misc;
      break;
    }
    /* else fall thru */
  default:                        /* ppc:396-397 */
    return -1;
  }

  if (bytes_needed) {             /* ppc:400-412 */
    update_bytes_allocated(tcr,((BytePtr)(cur_allocptr-disp)));
    if (allocate_object(xp, bytes_needed, disp, tcr)) {
      adjust_exception_pc(xp,4);  /* resume at the str after the udf */
      return 0;
    }
    lisp_allocation_failure(xp,tcr,bytes_needed);
    return -1;
  }
  return -1;
}

natural gc_deferred = 0, full_gc_deferred = 0;  /* ppc-exceptions.c:416 */

signed_natural
flash_freeze(TCR *tcr, signed_natural param)
{                                 /* ppc-exceptions.c:418-422 */
  return 0;
}

OSStatus
handle_gc_trap(ExceptionInformation *xp, TCR *tcr)
{                                 /* ppc-exceptions.c:424-557 */
  LispObj
    selector = xpGPR(xp,imm0),
    arg = xpGPR(xp,imm1);
  area *a = active_dynamic_area;
  Boolean egc_was_enabled = (a->older != NULL);
  natural gc_previously_deferred = gc_deferred;


  switch (selector) {
  case GC_TRAP_FUNCTION_EGC_CONTROL:   /* ppc:436-439 */
    egc_control(arg != 0, a->active);
    xpGPR(xp,arg_z) = lisp_nil + (egc_was_enabled ? t_offset : 0);
    break;

  case GC_TRAP_FUNCTION_CONFIGURE_EGC: /* ppc:441-446 */
    a->threshold = unbox_fixnum(xpGPR(xp, arg_x));
    g1_area->threshold = unbox_fixnum(xpGPR(xp, arg_y));
    g2_area->threshold = unbox_fixnum(xpGPR(xp, arg_z));
    xpGPR(xp,arg_z) = lisp_nil+t_offset;
    break;

  case GC_TRAP_FUNCTION_SET_LISP_HEAP_THRESHOLD:  /* ppc:448-455 */
    if (((signed_natural) arg) > 0) {
      lisp_heap_gc_threshold =
        align_to_power_of_2((arg-1) +
                            (heap_segment_size - 1),
                            log2_heap_segment_size);
    }
    /* fall through */
  case GC_TRAP_FUNCTION_GET_LISP_HEAP_THRESHOLD:  /* ppc:456-458 */
    xpGPR(xp, imm0) = lisp_heap_gc_threshold;
    break;

  case GC_TRAP_FUNCTION_USE_LISP_HEAP_THRESHOLD:  /* ppc:460-471 */
    /*  Try to put the current threshold in effect.  This may
        need to disable/reenable the EGC. */
    untenure_from_area(tenured_area);
    resize_dynamic_heap(a->active,lisp_heap_gc_threshold);
    if (egc_was_enabled) {
      if ((a->high - a->active) >= a->threshold) {
        tenure_to_area(tenured_area);
      }
    }
    xpGPR(xp, imm0) = lisp_heap_gc_threshold;
    break;

  case GC_TRAP_FUNCTION_ENSURE_STATIC_CONSES:     /* ppc:473-475 */
    ensure_static_conses(xp,tcr,32768);
    break;

  case GC_TRAP_FUNCTION_FLASH_FREEZE:  /* ppc:477-486 */
    untenure_from_area(tenured_area);
    gc_like_from_xp(xp,flash_freeze,0);
    a->active = (BytePtr) align_to_power_of_2(a->active, log2_page_size);
    tenured_area->static_dnodes = area_dnode(a->active, a->low);
    if (egc_was_enabled) {
      tenure_to_area(tenured_area);
    }
    xpGPR(xp, imm0) = tenured_area->static_dnodes << dnode_shift;
    break;

  default:                        /* ppc:488-551 */
    update_bytes_allocated(tcr, (void *) ptr_from_lispobj(xpGPR(xp, allocptr)));

    if (selector == GC_TRAP_FUNCTION_IMMEDIATE_GC) {
      if (!full_gc_deferred) {
        gc_from_xp(xp, 0L);
        break;
      }
      /* Tried to do a full GC when gc was disabled.  That failed,
         so try full GC now */
      selector = GC_TRAP_FUNCTION_GC;
    }

    if (egc_was_enabled) {
      egc_control(false, (BytePtr) a->active);
    }
    gc_from_xp(xp, 0L);
    if (gc_deferred > gc_previously_deferred) {
      full_gc_deferred = 1;
    } else {
      full_gc_deferred = 0;
    }
    if (selector > GC_TRAP_FUNCTION_GC) {
      if (selector & GC_TRAP_FUNCTION_IMPURIFY) {
        impurify_from_xp(xp, 0L);
        lisp_global(OLDSPACE_DNODE_COUNT) = 0;
        gc_from_xp(xp, 0L);
      }
      if (selector & GC_TRAP_FUNCTION_PURIFY) {
        purify_from_xp(xp, 0L);
        lisp_global(OLDSPACE_DNODE_COUNT) = 0;
        gc_from_xp(xp, 0L);
      }
      if (selector & GC_TRAP_FUNCTION_SAVE_APPLICATION) {
        OSErr err;
        extern OSErr save_application(unsigned, Boolean);
        TCR *tcr = TCR_FROM_TSD(xpGPR(xp, rcontext));
        area *vsarea = tcr->vs_area;

        nrs_TOPLFUNC.vcell = *((LispObj *)(vsarea->high)-1);
        err = save_application(arg, egc_was_enabled);
        if (err == noErr) {
          _exit(0);
        }
        fatal_oserr(": save_application", err);
      }
      switch (selector) {

      case GC_TRAP_FUNCTION_FREEZE:   /* ppc:538-542 */
        a->active = (BytePtr) align_to_power_of_2(a->active, log2_page_size);
        tenured_area->static_dnodes = area_dnode(a->active, a->low);
        xpGPR(xp, imm0) = tenured_area->static_dnodes << dnode_shift;
        break;
      default:
        break;
      }
    }

    if (egc_was_enabled) {
      egc_control(true, NULL);
    }
    break;

  }

  adjust_exception_pc(xp,4);      /* ppc:555 */
  return 0;
}


void
signal_stack_soft_overflow(ExceptionInformation *xp, unsigned reg)
{                                 /* ppc-exceptions.c:561-573 */
  /* The cstack just overflowed.  Force the current thread's
     control stack to do so until all stacks are well under their overflow
     limits.
  */
  handle_error(xp, error_stack_overflow, reg, 0,  xpPC(xp));
}

/*
  Lower (move toward 0) the "end" of the soft protected area associated
  with a by a page, if we can.
*/

void
adjust_soft_protection_limit(area *a)
{                                 /* ppc-exceptions.c:575-592 */
  char *proposed_new_soft_limit = a->softlimit - 4096;
  protected_area_ptr p = a->softprot;

  if (proposed_new_soft_limit >= (p->start+16384)) {
    p->end = proposed_new_soft_limit;
    p->protsize = p->end-p->start;
    a->softlimit = proposed_new_soft_limit;
  }
  protect_area(p);
}

void
restore_soft_stack_limit(unsigned stkreg)
{                                 /* ppc-exceptions.c:594-616 */
  area *a;
  TCR *tcr = get_tcr(true);

  switch (stkreg) {
  case Rsp:  /* ARM64-DEVIATION: PPC used sp=r1; AArch64 SP is not a
                numbered GPR, selector Rsp=31 (see enum above). */
    a = tcr->cs_area;
    if ((a->softlimit - 4096) > (a->hardlimit + 16384)) {
      a->softlimit -= 4096;
    }
    tcr->cs_limit = (LispObj)ptr_to_lispobj(a->softlimit);
    break;
  case vsp:
    a = tcr->vs_area;
    adjust_soft_protection_limit(a);
    break;
  case tsp:
    a = tcr->ts_area;
    adjust_soft_protection_limit(a);
  }
}

/* Maybe this'll work someday.  We may have to do something to
   make the thread look like it's not handling an exception */
void
reset_lisp_process(ExceptionInformation *xp)
{                                 /* ppc-exceptions.c:618-633 */
  TCR *tcr = TCR_FROM_TSD(xpGPR(xp,rcontext));
  catch_frame *last_catch = (catch_frame *) ptr_from_lispobj(untag(tcr->catch_top));

  tcr->save_allocptr = (void *) ptr_from_lispobj(xpGPR(xp, allocptr));
  tcr->save_allocbase = (void *) ptr_from_lispobj(xpGPR(xp, allocbase));

  tcr->save_vsp = (LispObj *) ptr_from_lispobj(((lisp_frame *)ptr_from_lispobj(last_catch->csp))->savevsp);
  tcr->save_tsp = (LispObj *) ptr_from_lispobj((LispObj) ptr_to_lispobj(last_catch)) - (2*node_size); /* account for TSP header */

  start_lisp(tcr, 1);
}


void
platform_new_heap_segment(ExceptionInformation *xp, TCR *tcr, BytePtr low, BytePtr high)
{                                 /* ppc-exceptions.c:636-642 */
  tcr->last_allocptr = (void *)high;
  xpGPR(xp,allocptr) = (LispObj) high;
  xpGPR(xp,allocbase) = (LispObj) low;
}


void
update_area_active (area **aptr, BytePtr value)
{                                 /* ppc-exceptions.c:645-659 */
  area *a = *aptr;
  for (; a; a = a->older) {
    if ((a->low <= value) && (a->high >= value)) break;
  };
  if (a == NULL) Bug(NULL, "Can't find active area");
  a->active = value;
  *aptr = a;

  for (a = a->younger; a; a = a->younger) {
    a->active = a->high;
  }
}

LispObj *
tcr_frame_ptr(TCR *tcr)
{                                 /* ppc-exceptions.c:661-676 */
  ExceptionInformation *xp;
  LispObj *bp = NULL;

  if (tcr->pending_exception_context)
    xp = tcr->pending_exception_context;
  else {
    xp = tcr->suspend_context;
  }
  if (xp) {
    bp = (LispObj *) xpSP(xp);  /* ARM64-DEVIATION: xpGPR(xp,sp) → xpSP */
  }
  return bp;
}

void
normalize_tcr(ExceptionInformation *xp, TCR *tcr, Boolean is_other_tcr)
{                                 /* ppc-exceptions.c:678-733 */
  void *cur_allocptr = NULL;
  LispObj freeptr = 0;

  if (xp) {
    if (is_other_tcr) {
      pc_luser_xp(xp, tcr, NULL);
      freeptr = xpGPR(xp, allocptr);
      if (fulltag_of(freeptr) == 0){
        cur_allocptr = (void *) ptr_from_lispobj(freeptr);
      }
    }
    update_area_active((area **)&tcr->cs_area, (BytePtr) ptr_from_lispobj(xpSP(xp)));
    update_area_active((area **)&tcr->vs_area, (BytePtr) ptr_from_lispobj(xpGPR(xp, vsp)));
    update_area_active((area **)&tcr->ts_area, (BytePtr) ptr_from_lispobj(xpGPR(xp, tsp)));
  } else {
    /* In ff-call. */
    cur_allocptr = (void *) (tcr->save_allocptr);
    update_area_active((area **)&tcr->vs_area, (BytePtr) tcr->save_vsp);
    update_area_active((area **)&tcr->ts_area, (BytePtr) tcr->save_tsp);
    /* ARM64-DEVIATION (16m41): PPC's "No need to update cs_area" comment does
       NOT carry over.  It is true there because ppc-gc.c:1022 walks a backlink
       CHAIN, which self-terminates wherever it starts; our walk is linear over
       [active, high) and asserts it lands on high, so a stale active means it
       starts in dead stack.  16m40/16m41 observed exactly that: a thread with
       valence=1 (foreign) and an active left over from an earlier exception,
       walked into C frames and strode 1.8e16 words off a spilled 0.9d0.
       tcr.last_lisp_frame is the boundary the ff-call spentries record (see
       the protocol note in spentry-E-ffi.s); this is where the GC reads it. */
    update_area_active((area **)&tcr->cs_area, (BytePtr) tcr->last_lisp_frame);
  }


  tcr->save_allocptr = tcr->save_allocbase = (void *)VOID_ALLOCPTR;
  if (cur_allocptr) {
    update_bytes_allocated(tcr, cur_allocptr);
    if (freeptr) {
      xpGPR(xp, allocptr) = VOID_ALLOCPTR;
      xpGPR(xp, allocbase) = VOID_ALLOCPTR;
    }
  }
}

TCR *gc_tcr = NULL;               /* ppc-exceptions.c:735 */

/* Suspend and "normalize" other tcrs, then call a gc-like function
   in that context.  Resume the other tcrs, then return what the
   function returned */

signed_natural
gc_like_from_xp(ExceptionInformation *xp,
                signed_natural(*fun)(TCR *, signed_natural),
                signed_natural param)
{                                 /* ppc-exceptions.c:741-800 */
  TCR *tcr = TCR_FROM_TSD(xpGPR(xp, rcontext)), *other_tcr;
  int result;
  signed_natural inhibit;

  suspend_other_threads(true);
  inhibit = (signed_natural)(lisp_global(GC_INHIBIT_COUNT));
  if (inhibit != 0) {
    if (inhibit > 0) {
      lisp_global(GC_INHIBIT_COUNT) = (LispObj)(-inhibit);
    }
    resume_other_threads(true);
    gc_deferred++;
    return 0;
  }
  gc_deferred = 0;

  gc_tcr = tcr;

  xpGPR(xp, allocptr) = VOID_ALLOCPTR;
  xpGPR(xp, allocbase) = VOID_ALLOCPTR;

  normalize_tcr(xp, tcr, false);


  for (other_tcr = tcr->next; other_tcr != tcr; other_tcr = other_tcr->next) {
    if (other_tcr->pending_exception_context) {
      other_tcr->gc_context = other_tcr->pending_exception_context;
    } else if (other_tcr->valence == TCR_STATE_LISP) {
      other_tcr->gc_context = other_tcr->suspend_context;
    } else {
      /* no pending exception, didn't suspend in lisp state:
         must have executed a synchronous ff-call.
      */
      other_tcr->gc_context = NULL;
    }
    normalize_tcr(other_tcr->gc_context, other_tcr, true);
  }



  result = fun(tcr, param);

  other_tcr = tcr;
  do {
    other_tcr->gc_context = NULL;
    other_tcr = other_tcr->next;
  } while (other_tcr != tcr);

  gc_tcr = NULL;

  resume_other_threads(true);

  return result;

}



/* Returns #bytes freed by invoking GC */

signed_natural
gc_from_tcr(TCR *tcr, signed_natural param)
{                                 /* ppc-exceptions.c:804-826 */
  area *a;
  BytePtr oldfree, newfree;
  BytePtr oldend, newend;

  a = active_dynamic_area;
  oldend = a->high;
  oldfree = a->active;
  gc(tcr, param);
  newfree = a->active;
  newend = a->high;
  return ((oldfree-newfree)+(newend-oldend));
}

signed_natural
gc_from_xp(ExceptionInformation *xp, signed_natural param)
{                                 /* ppc-exceptions.c:828-835 */
  signed_natural status = gc_like_from_xp(xp, gc_from_tcr, param);

  freeGCptrs();
  return status;
}

signed_natural
purify_from_xp(ExceptionInformation *xp, signed_natural param)
{                                 /* ppc-exceptions.c:837-841 */
  return gc_like_from_xp(xp, purify, param);
}

signed_natural
impurify_from_xp(ExceptionInformation *xp, signed_natural param)
{                                 /* ppc-exceptions.c:843-847 */
  return gc_like_from_xp(xp, impurify, param);
}


protection_handler
 * protection_handlers[] = {      /* ppc-exceptions.c:854-863 */
   do_spurious_wp_fault,
   do_soft_stack_overflow,
   do_soft_stack_overflow,
   do_soft_stack_overflow,
   do_hard_stack_overflow,
   do_hard_stack_overflow,
   do_hard_stack_overflow
   };


Boolean
is_write_fault(ExceptionInformation *xp, siginfo_t *info)
{                                 /* ppc-exceptions.c:866-925 */
  /* ppc:869-885: use the siginfo.  Linux delivers write-protection
     faults as SIGSEGV with SEGV_ACCERR in the low bits of si_code.
     ARM64-DEVIATION: the non-siginfo fallback read PPC's DSISR bit 25 /
     TRAP=0x300 (ppc:886-895); AArch64's mcontext has no fault-status
     register (the ESR lives in an optional esr_context extension that
     older kernels omit), so there is no register fallback — Linux
     sigaction with SA_SIGINFO always supplies info. */
  if (info) {
    return ((info->si_signo == SIGSEGV) &&
            ((info->si_code & 0xff) == (SEGV_ACCERR & 0xff)));
  }
  Bug(xp, "is_write_fault: no siginfo");
  return false;
}

static OSStatus pv_cold_load_fatal(ExceptionInformation *xp, BytePtr addr,
                                   Boolean is_write);

OSStatus
handle_protection_violation(ExceptionInformation *xp, siginfo_t *info, TCR *tcr, int old_valence)
{                                 /* ppc-exceptions.c:927-974 */
  BytePtr addr;
  protected_area_ptr area;
  protection_handler *handler;
  extern Boolean touch_page(void *);
  extern void touch_page_end(void);

  if (info) {
    addr = (BytePtr)(info->si_addr);
  } else {
    /* ARM64-DEVIATION: PPC read xpDAR (ppc:939); AArch64 mcontext
       carries the fault address directly. */
    addr = (BytePtr) ((natural) (xpFaultAddress(xp)));
  }

  if (addr && (addr == tcr->safe_ref_address)) {  /* ppc:942-947 */
    adjust_exception_pc(xp,4);

    xpGPR(xp,imm0) = 0;
    return 0;
  }

  if (xpPC(xp) == (pc)touch_page) {               /* ppc:949-953 */
    xpGPR(xp,imm0) = 0;
    xpPC(xp) = (pc)touch_page_end;
    return 0;
  }


  if (is_write_fault(xp,info)) {                  /* ppc:956-969 */
    area = find_protected_area(addr);
    if (area != NULL) {
      handler = protection_handlers[area->why];
      return handler(xp, area, addr);
    } else {
      if ((addr >= readonly_area->low) &&
          (addr < readonly_area->active)) {
        UnProtectMemory((LogicalAddress)(truncate_to_power_of_2(addr,log2_page_size)),
                        page_size);
        return 0;
      }
    }
  }
  if (old_valence == TCR_STATE_LISP) {            /* ppc:970-972 */
    LispObj cmain = nrs_CMAIN.vcell;
    /* Cold-load routing (same test as handle_uuo): calling back into a
       not-yet-real error system spins forever on its own faults. */
    if (!((fulltag_of(cmain) == fulltag_misc) &&
          (header_subtag(header_of(cmain)) == subtag_macptr))) {
      return pv_cold_load_fatal(xp, addr, is_write_fault(xp,info));
    }
    callback_for_trap(cmain, xp, (pc)xpPC(xp), SIGBUS, (natural)addr, is_write_fault(xp,info));
  }
  return -1;
}





OSStatus
do_hard_stack_overflow(ExceptionInformation *xp, protected_area_ptr area, BytePtr addr)
{                                 /* ppc-exceptions.c:980-988 */
#ifdef SUPPORT_PRAGMA_UNUSED
#pragma unused(area,addr)
#endif
  reset_lisp_process(xp);
  return -1;
}

extern area*
allocate_vstack(natural useable);       /* This is in "pmcl-kernel.c" */

extern area*
allocate_tstack(natural useable);       /* This is in "pmcl-kernel.c" */

/* ppc-exceptions.c:996-1009 (catch_frame_p), 1065-1081
   (find_non_catch_frame_from_xp), 1083-1095 (db_link_chain_in_area_p)
   are EXTEND_VSTACK-only (not compiled on PPC64 either); not ported. */

/* ppc-exceptions.c:1011-1035 (unwind_protect_cleanup_frame_p,
   lexpr_entry_frame_p) are PPC-shaped backlink-frame helpers with no
   callers outside the EXTEND_VSTACK lane (not compiled on PPC64
   either); ARM32 shipped without them.  Not ported. */

Boolean
lisp_frame_p(lisp_frame *spPtr)
{                                 /* arm-exceptions.c:955-958 --
                                     ARM64-DEVIATION: the ARM-family
                                     MARKER frame identifies itself; no
                                     PPC backlink/savefn heuristics
                                     (ppc:1037-1049). */
  return (spPtr->marker == lisp_frame_marker);
}


int ffcall_overflow_count = 0;    /* ppc-exceptions.c:1052 */


/* Note: CURRENT_VS (CURRENT_TS) is always either the area containing
  the current value of VSP (TSP) or an older area.  */

OSStatus
do_vsp_overflow (ExceptionInformation *xp, BytePtr addr)
{                                 /* ppc-exceptions.c:1103-1112 */
  TCR* tcr = TCR_FROM_TSD(xpGPR(xp, rcontext));
  area *a = tcr->vs_area;
  protected_area_ptr vsp_soft = a->softprot;
  unprotect_area(vsp_soft);
  signal_stack_soft_overflow(xp,vsp);
  return 0;
}


OSStatus
do_tsp_overflow (ExceptionInformation *xp, BytePtr addr)
{                                 /* ppc-exceptions.c:1115-1124 */
  TCR* tcr = TCR_FROM_TSD(xpGPR(xp, rcontext));
  area *a = tcr->ts_area;
  protected_area_ptr tsp_soft = a->softprot;
  unprotect_area(tsp_soft);
  signal_stack_soft_overflow(xp,tsp);
  return 0;
}

OSStatus
do_soft_stack_overflow(ExceptionInformation *xp, protected_area_ptr prot_area, BytePtr addr)
{                                 /* ppc-exceptions.c:1126-1141 */
  /* Trying to write into a guard page on the vstack or tstack.
     Allocate a new stack segment, emulate stwu and stwux for the TSP, and
     signal an error_stack_overflow condition.
      */
  lisp_protection_kind which = prot_area->why;
  Boolean on_TSP = (which == kTSPsoftguard);

  if (on_TSP) {
    return do_tsp_overflow(xp, addr);
   } else {
    return do_vsp_overflow(xp, addr);
   }
}

OSStatus
do_spurious_wp_fault(ExceptionInformation *xp, protected_area_ptr area, BytePtr addr)
{                                 /* ppc-exceptions.c:1143-1150 */
#ifdef SUPPORT_PRAGMA_UNUSED
#pragma unused(xp,area,addr)
#endif
  return -1;
}

/* ppc-exceptions.c:1153-1214 (comment block + is_ephemeral_node_store):
   NOT ported.  is_ephemeral_node_store has no callers in
   ppc-exceptions.c (dead code), and its body is stw-instruction
   emulation specific to the PPC ISA. */

OSStatus
handle_sigfpe(ExceptionInformation *xp, TCR *tcr)
{
  /* ppc-exceptions.c:1222-1235 zeroed the FPSCR, re-enabled FP traps
     via prctl and back-scanned for the offending PPC FPU opcode
     (handle_fpux_binop).  ARM64-DEVIATION (arm-exceptions.c:1017-1021):
     AArch64 FP exceptions are untrapped (FPCR trap enables generally
     RES0 on shipping cores), so a synchronous SIGFPE can only come
     from integer division; unhandled here (falls through to the
     debugger path in signal_handler, as on ARM32). */
  return -1;
}

OSStatus
handle_unimplemented_instruction(ExceptionInformation *xp,
                                 opcode instruction,
                                 TCR *tcr)
{
  /* ppc-exceptions.c:1242-1272 emulated the optional PPC fsqrt/fsqrts
     instructions.  ARM64-DEVIATION (arm-exceptions.c:1024-1031): no
     analogous optional instructions to emulate; fsqrt is base A64. */
  return -1;
}

OSStatus
PMCL_exception_handler(int xnum,
                       ExceptionInformation *xp,
                       TCR *tcr,
                       siginfo_t *info,
                       int old_valence)
{                                 /* ppc-exceptions.c:1274-1316 */
  OSStatus status = -1;
  pc program_counter;
  opcode instruction = 0;


  program_counter = xpPC(xp);

  if ((xnum == SIGILL) | (xnum == SIGTRAP)) {     /* ppc:1288-1290 */
    instruction = *program_counter;
  }

  if (((xnum == SIGILL) || (xnum == SIGTRAP)) &&
      IS_ALLOC_TRAP(instruction)) {               /* ppc:1292-1293 */
    status = handle_alloc_trap(xp, tcr);
  } else if ((xnum == SIGSEGV) ||
             (xnum == SIGBUS)) {                  /* ppc:1294-1296 */
    status = handle_protection_violation(xp, info, tcr, old_valence);
  } else if (xnum == SIGFPE) {                    /* ppc:1297-1298 */
    status = handle_sigfpe(xp, tcr);
  } else if ((xnum == SIGILL) || (xnum == SIGTRAP)) {  /* ppc:1299-1308 */
    if (instruction == GC_TRAP_INSTRUCTION) {
      status = handle_gc_trap(xp, tcr);
    } else if (IS_UUO(instruction)) {
      /* All remaining traps are udf UUOs; PPC's separate
         is_conditional_trap()/handle_trap() path (ppc:1304-1305) folds
         into handle_uuo because ARM64 trap sites branch around an
         unconditional udf (see PORT-NOTE 1). */
      status = handle_uuo(xp, instruction, program_counter, info);
    } else {
      status = handle_unimplemented_instruction(xp,instruction,tcr);
    }
  } else if (xnum == SIGNAL_FOR_PROCESS_INTERRUPT) {   /* ppc:1309-1313 */
    tcr->interrupt_pending = 0;
    /* ARM64-DEVIATION: PPC passed the magic TRI_instruction(TO_GT,nargs,0)
       word as the "trap" argument so lisp's trap-decode recognizes a
       process-interrupt (ppc:1311); the ARM64 marker is the
       take-deferred-interrupt udf. */
    callback_for_trap(nrs_CMAIN.vcell, xp, 0, DEFERRED_INTERRUPT_INSTRUCTION, 0, 0);
    status = 0;
  }

  return status;
}

void
adjust_exception_pc(ExceptionInformation *xp, int delta)
{                                 /* ppc-exceptions.c:1318-1322 */
  xpPC(xp) += (delta >> 2);
}

/* ppc-exceptions.c:1325-1362 (handle_fpux_binop): NOT ported — it
   back-scans for PPC FPU major opcodes to classify a trapped FP
   exception; AArch64 FP exceptions are untrapped (see handle_sigfpe). */

/* Cold-load fatal diagnostic: name the symbol in a trap register and
   die loudly.  Used when a uuo fires before the lisp error system
   exists (nrs_CMAIN/nrs_ERRDISP not yet macptrs) -- the alternative is
   lisp_Debugger, which spins on EOF under a detached boot.

   Layout formulas (OBSERVED in the 16m3 image, cited per doctrine 8):
   symbol fulltag = fulltag_symbol (7); symbol.pname = [sym + 1]
   (= tagged - fulltag_symbol + node_size, arm64-arch.lisp symptr:
   pname is slot 0 after the header).  pname is a misc-tagged string:
   header at [pname - fulltag_misc], element count = header >> 8
   (num_subtag_bits), 32-bit code points from [pname - fulltag_misc +
   node_size]. */
static void
uuo_describe_symbol(LispObj sym)
{
  if (fulltag_of(sym) == fulltag_symbol) {
    LispObj pname = *(LispObj *)((sym - fulltag_symbol) + node_size);
    if (fulltag_of(pname) == fulltag_misc) {
      natural header = header_of(pname);
      natural len = header >> num_subtag_bits;
      unsigned *chars = (unsigned *)((pname - fulltag_misc) + node_size);
      natural i;
      fprintf(dbgout, " symbol ");
      for (i = 0; (i < len) && (i < 256); i++) {
        unsigned c = chars[i];
        fputc(((c >= 0x20) && (c < 0x7f)) ? (int)c : '?', dbgout);
      }
    } else {
      fprintf(dbgout, " symbol with unreadable pname (0x%lx)",
              (unsigned long)pname);
    }
  } else {
    fprintf(dbgout, " non-symbol value 0x%lx", (unsigned long)sym);
  }
}

/* Caller context (boot-16m5b): when the named symbol is %ERR-DISP the
   interesting fact is the ERROR being signalled, not the udf itself —
   .SPksignalerr jumps through the %err-disp fcell with the errnum in
   arg_z and the culprit lr.  Dump the call frame so each boot names
   its own wall (register names per Matt's arm64 map). */
static void
cold_load_dump_frame(ExceptionInformation *xp)
{
  fprintf(dbgout,
          "  lr 0x%lx  nargs 0x%lx  vsp 0x%lx  tsp 0x%lx  sp 0x%lx\n"
          "  imm0-2 0x%lx 0x%lx 0x%lx\n"
          "  arg_w..z(x8-11) 0x%lx 0x%lx 0x%lx 0x%lx  fn(x7) 0x%lx\n"
          "  temp0-5(x12-17) 0x%lx 0x%lx 0x%lx 0x%lx 0x%lx 0x%lx\n",
          (unsigned long)xpGPR(xp, 30), (unsigned long)xpGPR(xp, 6),
          (unsigned long)xpGPR(xp, 25), (unsigned long)xpGPR(xp, 24),
          (unsigned long)xpSP(xp),
          (unsigned long)xpGPR(xp, 0), (unsigned long)xpGPR(xp, 1),
          (unsigned long)xpGPR(xp, 2),
          (unsigned long)xpGPR(xp, 8), (unsigned long)xpGPR(xp, 9),
          (unsigned long)xpGPR(xp, 10), (unsigned long)xpGPR(xp, 11),
          (unsigned long)xpGPR(xp, 7),
          (unsigned long)xpGPR(xp, 12), (unsigned long)xpGPR(xp, 13),
          (unsigned long)xpGPR(xp, 14), (unsigned long)xpGPR(xp, 15),
          (unsigned long)xpGPR(xp, 16), (unsigned long)xpGPR(xp, 17));
  fflush(dbgout);
}

static OSStatus
uuo_cold_load_fatal(ExceptionInformation *xp, pc where, opcode the_uuo,
                    const char *what, unsigned gpr)
{
  fprintf(dbgout, "\nFATAL (cold load, no lisp error system): %s --", what);
  uuo_describe_symbol(xpGPR(xp, gpr));
  fprintf(dbgout, "\n  at pc 0x%lx, uuo 0x%08x, x%u = 0x%lx\n",
          (unsigned long)(natural)where, the_uuo, gpr,
          (unsigned long)xpGPR(xp, gpr));
  cold_load_dump_frame(xp);
  _exit(157);
  return -1;                      /* not reached */
}

/* Protection-violation flavor of the cold-load fatal (16m5o): before this,
   an unhandled SEGV during cold load called back into a lisp error system
   that doesn't exist yet, and the callback's own fault made a silent
   recursive-signal 100%-CPU spin — every such wall cost a gdb session to
   even NAME.  Same routing rule as handle_uuo: if cmain isn't a real
   macptr yet, die loudly on dbgout instead of calling back. */
static OSStatus
pv_cold_load_fatal(ExceptionInformation *xp, BytePtr addr, Boolean is_write)
{
  fprintf(dbgout,
          "\nFATAL (cold load, no lisp error system): unhandled %s fault\n"
          "  at pc 0x%lx, fault address 0x%lx\n",
          is_write ? "write" : "read",
          (unsigned long)(natural)xpPC(xp), (unsigned long)(natural)addr);
  cold_load_dump_frame(xp);
  _exit(157);
  return -1;                      /* not reached */
}

/*
  UUO dispatch.  Combines ppc-exceptions.c's handle_uuo (:1364-1446)
  and the unconditional-trap semantics of handle_trap (:1552-1673),
  since every ARM64 trap is a udf UUO (PORT-NOTE 1).

  Callback routing follows PPC:
   - errnum-style errors → %err-disp (handle_error → nrs_ERRDISP), as
     PPC handle_uuo's UUO_INTERR default case (ppc:1415-1417).
   - register/tag/argument-count decode left to lisp → cmain with the
     raw trap word, as PPC handle_trap's cmain case (ppc:1657-1670).
  Any path that would call back into lisp before the error system
  exists routes to uuo_cold_load_fatal instead (named-symbol print +
  loud exit) -- the boot-bringup diagnosis loop reads dbgout.
*/
OSStatus
handle_uuo(ExceptionInformation *xp, opcode the_uuo, pc where, siginfo_t *info)
{
  unsigned imm16 = UUO_IMM16(the_uuo);
  unsigned format = UUO_FORMAT(imm16);
  LispObj cmain = nrs_CMAIN.vcell;              /* ppc:1558 */
  TCR *tcr = TCR_FROM_TSD(xpGPR(xp, rcontext)); /* ppc:1559 */
  Boolean cmain_is_macptr =
    ((fulltag_of(cmain) == fulltag_misc) &&
     (header_subtag(header_of(cmain)) == subtag_macptr)); /* ppc:1657-1658 */

  OSStatus status = -1;

  int bump = 4;                   /* ppc:1377 */

  switch (format) {

  case uuo_format_unary: {
    unsigned gpr  = UUO_UNARY_GPR(imm16);
    unsigned uinfo = UUO_UNARY_INFO(imm16);

    switch (uinfo) {
    case uuo_unary_not_callable:
      /* funcall target not a function/symbol -> fixed errnum. */
      status = handle_error(xp, error_cant_call, gpr, 0, where);
      if (status) {
        status = uuo_cold_load_fatal(xp, where, the_uuo,
                                     "called object not callable", gpr);
      }
      break;

    case uuo_unary_apply_macro:
      /* Funcalled a symbol naming a macro/special operator: the fcell's
         2-element simple-vector was jumped through (slot 0 = %macro-code%,
         whose single instruction is this UUO).  gpr = fname.  Lisp turns
         it into $XNOTFUN (call-special-operator-or-macro, a subclass of
         undefined-function) with the argument list from the frame. */
      status = handle_error(xp, error_apply_macro_or_special, gpr, 0, where);
      if (status) {
        status = uuo_cold_load_fatal(xp, where, the_uuo,
                                     "funcalled a macro or special-operator "
                                     "name", gpr);
      }
      break;

    case uuo_unary_no_throw_tag:
      status = handle_error(xp, error_throw_tag_missing, gpr, 0, where);
      if (status) {
        status = uuo_cold_load_fatal(xp, where, the_uuo,
                                     "throw tag missing", gpr);
      }
      break;

    case uuo_unary_udf:
    case uuo_unary_udf_call:
      /* Undefined function referenced/called; gpr = fname (errors.s:
         error_udf=1, error_udf_call=2; PPC routes both to %err-disp). */
      status = handle_error(xp,
                            (uinfo == uuo_unary_udf) ? error_udf
                                                     : error_udf_call,
                            gpr, 0, where);
      if (status) {
        status = uuo_cold_load_fatal(xp, where, the_uuo,
                                     "undefined function", gpr);
      } else if (uinfo == uuo_unary_udf_call) {
        /* If lisp's returned from an undefined-function call, it's put
           a code vector in the xp's PC.  Don't advance the PC
           (ppc:1424-1429; lisp side = arm64-error-signal.lisp
           handle-udf-call). */
        bump = 0;
      }
      break;

    case uuo_unary_tlb_too_small:
      /* ppc:1645-1655: twlle tlb-limit,index -> extend the tcr's tlb.
         ARM64-DEVIATION: the udf encodes only the INDEX register; the
         subprim reloads tcr.tlb_limit itself (arm-exceptions.c:
         1160-1166 does the same). */
      if (extend_tcr_tlb(tcr, xp, gpr)) {
        status = 0;
        break;
      }
      status = -1;
      break;

    case uuo_unary_unbound:
      /* PPC signalled unbound via a conditional trap on unbound_marker
         decoded by lisp (cmain path, ppc:1657-1670); gpr = register
         holding the symbol. */
      if (cmain_is_macptr) {
        callback_for_trap(cmain, xp, where, (natural) the_uuo, 0, 0);
        status = 0;
      } else {
        status = uuo_cold_load_fatal(xp, where, the_uuo,
                                     "unbound variable", gpr);
      }
      break;

    case uuo_unary_slot_unbound: {
      /* Three-register error (doc/porting/arm64.md "Errors that need
         three registers").  The primary UUO names the SLOT VECTOR; the
         companion at where[1] is a binary uuo_extra_registers carrying
         (index, dest).  Lisp needs all three: the slot vector and index
         to find the slot definition, and dest because CL's SLOT-UNBOUND
         may RETURN a value that becomes the slot reference's value.  So
         hand lisp BOTH words and resume past BOTH instructions -- the
         companion is data and is never executed. */
      opcode companion = where[1];
      unsigned cimm16 = UUO_IMM16(companion);

      if ((!IS_UUO(companion)) ||
          (UUO_FORMAT(cimm16) != uuo_format_binary) ||
          (UUO_BINARY_INFO(cimm16) != uuo_binary_two_registers)) {
        /* The emit site must place the companion adjacently; without it
           index and dest are unrecoverable, so this is never a
           continuable situation. */
        status = uuo_cold_load_fatal(xp, where, the_uuo,
                                     "slot-unbound uuo with no "
                                     "uuo_extra_registers companion", gpr);
        bump = 0;
        break;
      }
      if (cmain_is_macptr) {
        callback_for_trap(cmain, xp, where, (natural) the_uuo,
                          (natural) companion, 0);
        status = 0;
        bump = 8;               /* primary + companion */
      } else {
        status = uuo_cold_load_fatal(xp, where, the_uuo,
                                     "unbound slot", gpr);
      }
      break;
    }

    default:
      status = -1;
      bump = 0;
      break;
    }
    break;
  }

  case uuo_format_binary:
    /* ra = one operand reg, rb = the other; lisp decodes reg pair +
       info from the raw trap word (PPC's twlge index,limit traps,
       ppc:1657-1670).  All assigned infos are error reports, so the
       cmain route covers 0-6; 7 (two_registers) extends a PRECEDING
       uuo and should never fault on its own. */
    if (UUO_BINARY_INFO(imm16) != uuo_binary_two_registers) {
      if (cmain_is_macptr) {
        callback_for_trap(cmain, xp, where, (natural) the_uuo, 0, 0);
        status = 0;
      } else {
        status = uuo_cold_load_fatal(xp, where, the_uuo,
                                     "binary trap (see uuo bits 15:12)",
                                     UUO_BINARY_RA(imm16));
      }
    } else {
      status = -1;
      bump = 0;
    }
    break;

  case uuo_format_wrong_type:
    /* reg in 6:2, continuable bit 7, expected xtype in 15:8; lisp
       decodes reg + expected type from the raw trap word (PPC's twnei
       tag traps, ppc:1657-1670). */
    if (cmain_is_macptr) {
      callback_for_trap(cmain, xp, where, (natural) the_uuo, 0, 0);
      status = 0;
    } else {
      status = uuo_cold_load_fatal(xp, where, the_uuo,
                                   "wrong type (expected xtype in uuo bits 15:8)",
                                   UUO_WT_GPR(imm16));
    }
    break;

  case uuo_format_misc: {
    unsigned mi = UUO_MISC_INFO(imm16);

    if (mi == 0) {
      /* udf #0 is the code-vector start sentinel (arm64-uuo.s:20-22);
         executing one is never legitimate.  Loud failure. */
      status = -1;
      bump = 0;
      break;
    }

    if (UUO_MISC_IS_INTERR(mi)) {
      /* PROPOSED uuo_interr extension (arm64-globals-proposed.s;
         ratify): PPC uuo_interr(errnum, reg), ppc:1387-1419. */
      unsigned errnum = UUO_INTERR_ERRNUM(mi);
      unsigned gpr = UUO_INTERR_GPR(mi);

      if ((errnum == error_stack_overflow) && (gpr == Rsp)) {
        /* Failed control-stack overflow check (spentry-C savecontext*
           emits uuo_interr error_stack_overflow, sp).  PPC
           handle_trap's "trllt RA==sp" yellow-zone logic,
           ppc:1564-1629, ported whole. */
        area
          *CS_area = tcr->cs_area,
          *VS_area = tcr->vs_area;

        natural
          current_SP = xpSP(xp),        /* ppc:1599 xpGPR(sp) */
          current_VSP = xpGPR(xp,vsp);  /* ppc:1600 */

        if (current_SP  < (natural) (CS_area->hardlimit)) { /* ppc:1602 */
          /* If we are not in soft overflow mode yet, assume that the
             user has set the soft overflow size very small and try to
             continue on another thread before throwing to toplevel */
          if ((tcr->cs_limit == CS_OVERFLOW_FORCE_LIMIT)) {
            reset_lisp_process(xp);
          }
        } else {
          if (tcr->cs_limit == CS_OVERFLOW_FORCE_LIMIT) { /* ppc:1610 */
            /* If the control stack pointer is at least 4K away from its
               soft limit and the value stack pointer is at least 4K away
               from its soft limit, stop trapping.  Else keep trapping. */
            if ((current_SP > (natural) ((CS_area->softlimit)+4096)) &&
                (current_VSP > (natural) ((VS_area->softlimit)+4096))) {
              protected_area_ptr vs_soft = VS_area->softprot;
              if (vs_soft->nprot == 0) {
                protect_area(vs_soft);
              }
              tcr->cs_limit = ptr_to_lispobj(CS_area->softlimit);
            }
          } else {
            tcr->cs_limit = ptr_to_lispobj(CS_area->hardlimit); /* ppc:1623 */
            signal_stack_soft_overflow(xp, Rsp);
          }
        }
        status = 0;                     /* ppc:1628-1629 */
        break;
      }

      {
        /* Kernel-service + generic errnum dispatch, PPC handle_uuo
           UUO_INTERR (ppc:1387-1419). */
        TCR *target = (TCR *)xpGPR(xp,arg_z);  /* ppc:1389 */
        status = 0;
        switch (errnum) {
        case error_propagate_suspend:   /* ppc:1392-1393 */
          break;
        case error_interrupt:           /* ppc:1394-1396 */
          xpGPR(xp,imm0) = (LispObj) raise_thread_interrupt(target);
          break;
        case error_suspend:             /* ppc:1397-1399 */
          xpGPR(xp,imm0) = (LispObj) lisp_suspend_tcr(target);
          break;
        case error_suspend_all:         /* ppc:1400-1402 */
          lisp_suspend_other_threads();
          break;
        case error_resume:              /* ppc:1403-1405 */
          xpGPR(xp,imm0) = (LispObj) lisp_resume_tcr(target);
          break;
        case error_resume_all:          /* ppc:1406-1408 */
          lisp_resume_other_threads();
          break;
        case error_kill:                /* ppc:1409-1411 */
          xpGPR(xp,imm0) = (LispObj)kill_tcr(target);
          break;
        case error_allocate_list:       /* ppc:1412-1414 */
          allocate_list(xp,get_tcr(true));
          break;
        default:                        /* ppc:1415-1417 */
          status = handle_error(xp, errnum, gpr, 0,  where);
          break;
        }
      }
      break;
    }

    switch (mi) {
      /* misc 0 (alloc trap) and misc 1 (GC trap) are dispatched by
         PMCL_exception_handler before we get here, as on PPC
         (ppc:1292/1300). */

    case uuo_misc_debug_trap:
      /* ppc:1640-1644 QUIET_LISP_BREAK_INSTRUCTION; same shape as
         arm-exceptions.c:1177-1182. */
      adjust_exception_pc(xp, bump);
      bump = 0;
      lisp_Debugger(xp, info, debug_entry_dbg, false, "Lisp Breakpoint");
      status = 0;
      break;

    case uuo_misc_interrupt_now:
      /* ppc:1659-1668: the explicit take-deferred-interrupt trap:
         reset interrupt level/pending, then tell cmain. */
      if (cmain_is_macptr) {
        TCR_INTERRUPT_LEVEL(tcr) = 0;
        tcr->interrupt_pending = 0;
        callback_for_trap(cmain, xp, where, (natural) the_uuo, 0, 0);
        status = 0;
      }
      break;

    case uuo_misc_suspend_now:
      /* ppc:1391-1393 (error_propagate_suspend does nothing but
         resume). */
      status = 0;
      break;

    case uuo_misc_too_few:
    case uuo_misc_too_many:
    case uuo_misc_wrong_number:
      /* nargs checks: lisp decodes the trap word via cmain, as with
         PPC conditional nargs traps (ppc:1657-1670). */
      if (cmain_is_macptr) {
        callback_for_trap(cmain, xp, where, (natural) the_uuo, 0, 0);
        status = 0;
      } else {
        status = uuo_cold_load_fatal(xp, where, the_uuo,
                                     "wrong argument count", nargs);
      }
      break;

    default:
      status = -1;
      bump = 0;
      break;
    }
    break;
  }
  }

  if ((!status) && bump) {        /* ppc:1442-1444 */
    adjust_exception_pc(xp, bump);
  }
  return status;
}

natural
register_codevector_contains_pc (natural lisp_function, pc where)
{                                 /* ppc-exceptions.c:1448-1463 */
  natural code_vector, size;

  /* A function is an ordinary miscobj (fulltag_misc + subtag_function;
     fulltag_function removed, patch 0055); codevector is slot 0
     (deref(fn,1)) as on PPC64. */
  if ((fulltag_of(lisp_function) == fulltag_misc) &&
      (header_subtag(header_of(lisp_function)) == subtag_function)) {
    code_vector = deref(lisp_function, 1);
    size = header_element_count(header_of(code_vector)) << 2;
    if ((untag(code_vector) < (natural)where) &&
        ((natural)where < (code_vector + size)))
      return(code_vector);
  }

  return(0);
}

/* Callback to lisp to handle a trap. Need to translate the
   PC (where) into one of two forms of pairs:

   1. If PC is in fn or nfn's code vector, use the register number
      of fn or nfn and the index into that function's code vector.
   2. Otherwise use 0 and the pc itself
   (ppc-exceptions.c:1465-1489)
*/
void
callback_for_trap (LispObj callback_macptr, ExceptionInformation *xp, pc where,
                   natural arg1, natural arg2, natural arg3)
{
  natural code_vector = register_codevector_contains_pc(xpGPR(xp, fn), where);
  unsigned register_number = fn;
  natural index = (natural)where;

  if (code_vector == 0) {
    register_number = nfn;
    code_vector = register_codevector_contains_pc(xpGPR(xp, nfn), where);
  }
  if (code_vector == 0)
    register_number = 0;
  else
    /* misc_data_offset = -4 relative to the tagged code vector in
       Matt's layout (arm64-constants.h:143-144: fulltag_misc 12,
       data = header + node_size). */
    index = ((natural)where - (code_vector + misc_data_offset)) >> 2;
  callback_to_lisp(callback_macptr, xp, register_number, index, arg1, arg2, arg3);
}

void
callback_to_lisp (LispObj callback_macptr, ExceptionInformation *xp,
                  natural arg1, natural arg2, natural arg3, natural arg4, natural arg5)
{                                 /* ppc-exceptions.c:1491-1535 */
  natural  callback_ptr;
  area *a;

  TCR *tcr = TCR_FROM_TSD(xpGPR(xp, rcontext));

  /* Put the active stack pointer where .SPcallback expects it */
  a = tcr->cs_area;
  a->active = (BytePtr) ptr_from_lispobj(xpSP(xp)); /* ppc:1502 xpGPR(sp) */

  /* Copy globals from the exception frame to tcr */
  tcr->save_allocptr = (void *)ptr_from_lispobj(xpGPR(xp, allocptr));
  tcr->save_allocbase = (void *)ptr_from_lispobj(xpGPR(xp, allocbase));
  tcr->save_vsp = (LispObj*) ptr_from_lispobj(xpGPR(xp, vsp));
  tcr->save_tsp = (LispObj*) ptr_from_lispobj(xpGPR(xp, tsp));



  /* Call back.
     Lisp will handle trampolining through some code that
     will push lr/fn & pc/nfn stack frames for backtrace.
  */
  callback_ptr = ((macptr *)ptr_from_lispobj(untag(callback_macptr)))->address;
  UNLOCK(lisp_global(EXCEPTION_LOCK), tcr);
  ((void (*)())callback_ptr) (xp, arg1, arg2, arg3, arg4, arg5);
  LOCK(lisp_global(EXCEPTION_LOCK), tcr);



  /* Copy GC registers back into exception frame */
  xpGPR(xp, allocbase) = (LispObj) ptr_to_lispobj(tcr->save_allocbase);
  xpGPR(xp, allocptr) = (LispObj) ptr_to_lispobj(tcr->save_allocptr);
}

area *
allocate_no_stack (natural size)
{                                 /* ppc-exceptions.c:1537-1545 */
#ifdef SUPPORT_PRAGMA_UNUSED
#pragma unused(size)
#endif

  return (area *) NULL;
}


/* ppc-exceptions.c:1552-1673 (handle_trap): folded into handle_uuo
   above — the conditional-trap decode (:1591-1655) has no ARM64 analog
   (trap sites branch around an unconditional udf), the cstack
   yellow-zone logic lives in handle_uuo's error_stack_overflow case,
   the tlb case in uuo_unary_tlb_too_small, the LISP_BREAK cases in
   uuo_misc_debug_trap, and the cmain fallback in the raw-uuo
   callback branches.
   ppc-exceptions.c:1676-1692 (scan_for_instr) and :1701-1734
   (is_conditional_trap): PPC-ISA-specific helpers with no callers
   here; not ported. */


void non_fatal_error( char *msg )
{                                 /* ppc-exceptions.c:1695-1699 */
  fprintf( dbgout, "Non-fatal error: %s.\n", msg );
  fflush( dbgout );
}

OSStatus
handle_error(ExceptionInformation *xp, unsigned errnum, unsigned rb, unsigned continuable, pc where)
{                                 /* ppc-exceptions.c:1736-1749 */
  LispObj   errdisp = nrs_ERRDISP.vcell;

  if ((fulltag_of(errdisp) == fulltag_misc) &&
      (header_subtag(header_of(errdisp)) == subtag_macptr)) {
    /* errdisp is a macptr, we can call back to lisp */
    callback_for_trap(errdisp, xp, where, errnum, rb, continuable);
    return(0);
    }

  return(-1);
}


/* ===========================================================================
 * Exception-lock plumbing, signal handlers, pc_luser_xp, installation.
 * ppc-exceptions.c:1758-2333 (fns 56-70 of the inventory), completed by the
 * lead after the drafting agent's session ended at fn 55.
 * ===========================================================================
 */

int
prepare_to_wait_for_exception_lock(TCR *tcr, ExceptionInformation *context)
{                                 /* ppc-exceptions.c:1758-1769, verbatim */
  int old_valence = tcr->valence;

  tcr->pending_exception_context = context;
  tcr->valence = TCR_STATE_EXCEPTION_WAIT;

  ALLOW_EXCEPTIONS(context);      /* lisp-exceptions.h:100-108 */
  return old_valence;
}

void
wait_for_exception_lock_in_handler(TCR *tcr,
                                   ExceptionInformation *context,
                                   xframe_list *xf)
{                                 /* ppc-exceptions.c:1771-1786, verbatim */
  LOCK(lisp_global(EXCEPTION_LOCK), tcr);
  xf->curr = context;
  xf->prev = tcr->xframe;
  tcr->xframe = xf;
  tcr->pending_exception_context = NULL;
  tcr->valence = TCR_STATE_FOREIGN;
}

void
unlock_exception_lock_in_handler(TCR *tcr)
{                                 /* ppc-exceptions.c:1788-1797, verbatim */
  tcr->pending_exception_context = tcr->xframe->curr;
  tcr->xframe = tcr->xframe->prev;
  tcr->valence = TCR_STATE_EXCEPTION_RETURN;
  UNLOCK(lisp_global(EXCEPTION_LOCK), tcr);
}

/* If an interrupt is pending on exception exit, try to ensure that the
   thread sees it as soon as it's able to run. */
void
raise_pending_interrupt(TCR *tcr)
{                                 /* ppc-exceptions.c:1803-1809, verbatim */
  if (TCR_INTERRUPT_LEVEL(tcr) > 0) {
    pthread_kill((pthread_t)ptr_from_lispobj(tcr->osid),
                 SIGNAL_FOR_PROCESS_INTERRUPT);
  }
}

void
exit_signal_handler(TCR *tcr, int old_valence, natural old_last_lisp_frame)
{                                 /* ppc-exceptions.c:1811-1820 + the boundary */
  sigset_t mask;
  sigfillset(&mask);

  pthread_sigmask(SIG_SETMASK, &mask, NULL);
  tcr->valence = old_valence;
  tcr->pending_exception_context = NULL;
  /* ARM64-DEVIATION (16m41): restore the lisp<->foreign cstack boundary the
     handler moved.  PPC has no such field because its cstack walk is a
     backlink chain (see normalize_tcr); on a marker/linear walk the boundary
     is the only thing that keeps a foreign-valence thread's walk out of C
     frames.  Same shape as the ARM-family handler exit. */
  tcr->last_lisp_frame = old_last_lisp_frame;
}

void
signal_handler(int signum, siginfo_t *info, ExceptionInformation *context)
{                                 /* ppc-exceptions.c:1823-1866 */
  TCR *tcr;
  int old_valence;
  natural old_last_lisp_frame;
  xframe_list xframe_link;

  tcr = (TCR *) get_interrupt_tcr(false);

  /* The signal handler's entered with all signals (notably the
     thread_suspend signal) blocked.  Don't allow any other signals
     (notably the thread_suspend signal) to preempt us until we've
     set the TCR's xframe slot to include the current exception
     context.  (ppc:1832-1838.)

     16m41 CORRECTION: the note here said ARM32's tcr->last_lisp_frame save
     "does NOT port -- Matt's tcr has no last_lisp_frame field".  That was
     false at this pin (arm64-constants.h:470 asm / :531 C), and dropping the
     save left the field permanently 0, which is what normalize_tcr's ff-call
     branch then had nothing to read.  While this handler runs, the thread's
     lisp-owned cstack ends at the faulting SP, so that is the boundary; the
     old value is restored in exit_signal_handler. */
  old_last_lisp_frame = tcr->last_lisp_frame;
  tcr->last_lisp_frame = (natural)ptr_to_lispobj(xpSP(context));

  old_valence = prepare_to_wait_for_exception_lock(tcr, context);

  if (tcr->flags & (1 << TCR_FLAG_BIT_PENDING_SUSPEND)) {
    CLR_TCR_FLAG(tcr, TCR_FLAG_BIT_PENDING_SUSPEND);
    pthread_kill(pthread_self(), thread_suspend_signal);
  }

  wait_for_exception_lock_in_handler(tcr, context, &xframe_link);
  if ((noErr != PMCL_exception_handler(signum, context, tcr, info,
                                       old_valence))) {
    char msg[512];
    /* 16m40: NAME THE INSTRUCTION, not just its address.
       "Unhandled exception 5 at <pc>" is a SIGILL/SIGTRAP whose word this
       handler already looked at and could not decode -- so the one fact
       that identifies the bug is the word itself, and it was the one fact
       the message omitted.  16m39/16m40 each spent a boot recovering it by
       hand.  That trick only works while the pc is in the IMAGE, which is
       mapped at a fixed address; for code the RESIDENT compiler generated
       the pc is in the dynamic heap and differs every run, so there is no
       second boot that can read it.  Hence: print it here.
       Decoded per arm64-uuo.s: a udf is `#imm16' with the top 16 bits
       zero, format in imm16 1:0; a brk is 0xd42xxxx0 and is NOT a uuo on
       this architecture (arm64-uuo.s:11) -- seeing one means a placeholder
       trap reached the kernel. */
    opcode faulting = 0;
    char insn_desc[160];
    insn_desc[0] = 0;
    if ((signum == SIGILL) || (signum == SIGTRAP)) {
      faulting = *(xpPC(context));
      if (IS_UUO(faulting)) {
        unsigned imm16 = UUO_IMM16(faulting);
        snprintf(insn_desc, sizeof(insn_desc),
                 ", insn 0x%08x = udf #0x%x (uuo format %d, UNDECODED)",
                 faulting, imm16, UUO_FORMAT(imm16));
      } else if ((faulting & 0xffe0001f) == 0xd4200000) {
        snprintf(insn_desc, sizeof(insn_desc),
                 ", insn 0x%08x = brk #0x%x -- NOT a uuo on arm64; this is a "
                 "PLACEHOLDER trap that should be a udf",
                 faulting, (faulting >> 5) & 0xffff);
      } else {
        snprintf(insn_desc, sizeof(insn_desc),
                 ", insn 0x%08x (neither udf nor brk)", faulting);
      }
    }
    snprintf(msg, sizeof(msg),
             "Unhandled exception %d at 0x%lx%s, context->regs at #x%lx",
             signum, (natural)xpPC(context), insn_desc,
             (natural)xpGPRvector(context));
    if (lisp_Debugger(context, info, signum, false, msg)) {
      SET_TCR_FLAG(tcr, TCR_FLAG_BIT_PROPAGATE_EXCEPTION);
    }
  }

  unlock_exception_lock_in_handler(tcr);

  /* This thread now looks like a thread that was suspended while
     executing lisp code.  If some other thread gets the exception
     lock and GCs, the context (this thread's suspend_context) will
     be updated.  (ppc:1858-1863) */
  exit_signal_handler(tcr, old_valence, old_last_lisp_frame);
  raise_pending_interrupt(tcr);
}

/*
  If it looks like we're in the middle of an atomic operation, make
  it seem as if that operation is either complete or hasn't started
  yet.  (ppc-exceptions.c:1868-1897 comment; cases (a)/(b)/(c)/(e)
  below.  PPC case (d) -- stmw to the vsp -- is PPC32-only and does
  not port.)
*/

extern opcode
  egc_rplaca, egc_rplaca_did_store,          /* spentry-D (window half 1) */
  egc_rplacd, egc_rplacd_did_store, egc_rplacd_end,
  egc_gvset, egc_gvset_did_store,            /* spentry-B (window half 2) */
  egc_set_hash_key, egc_set_hash_key_did_store,
  egc_store_node_conditional, egc_store_node_conditional_test,
  egc_set_hash_key_conditional, egc_set_hash_key_conditional_test,
  egc_write_barrier_end;

/* The tsp-frame "raw" mark: `str tsp, [tsp, #tsp_frame.type]' (type == 8;
   spentry-A tsp_frame equates / arm64-macros.s TSP frame discipline).
   STR (unsigned offset, 64-bit): 0xF9000000 | (8>>3)<<10 | tsp<<5 | tsp,
   tsp = x24 (arm64-asm.lisp:215). */
#define MARK_TSP_FRAME_INSTRUCTION 0xF9000718

/* Marker lisp-frame creation + slot stores (the drafts' canonical build
   order: sub sp,sp,#32; str marker@0; str vsp@8; str fn@16; str lr@24 --
   spentry-A misc_alloc_init:661-666 et al.).
   SUB sp,sp,#32: 0xD1000000 | (32<<10) | (31<<5) | 31 = 0xD10083FF.
   STR Xt,[sp,#imm]: 0xF9000000 | (imm>>3)<<10 | (31<<5) | Rt. */
#define CREATE_LISP_FRAME_INSTRUCTION 0xD10083FF
#define IS_STR_TO_SP(i) (((i) & 0xFFC003E0) == 0xF90003E0)
#define STR_TO_SP_DISP(i) ((((i) >> 10) & 0xfff) << 3)

void
pc_luser_xp(ExceptionInformation *xp, TCR *tcr, signed_natural *alloc_disp)
{                                 /* ppc-exceptions.c:1900-2130 */
  pc program_counter = xpPC(xp);
  opcode instr = *program_counter;
  lisp_frame *frame = (lisp_frame *)ptr_from_lispobj(xpSP(xp));
  LispObj cur_allocptr = xpGPR(xp, allocptr);
  int allocptr_tag = fulltag_of(cur_allocptr);

  /* (e) EGC write-barrier subprims.  ARM64-DEVIATION (window shape only):
     PPC brackets ALL barrier subprims in one contiguous
     [egc_write_barrier_start, egc_write_barrier_end) region; here the
     family is split across spentry-D (rplaca/rplacd) and spentry-B
     (gvset/set-hash-key/conditionals), so we test the two per-file
     windows.  Case logic and memoization are ppc:1911-1979 verbatim.
     Conditional-store "did it store?" test: PPC reads CR0.EQ
     (xpCCR & 0x20000000); the drafts' ll/sc loops leave the stxr status
     in w17 (= temp5 after the 01d73c3 renumber; the uniform status register):
     status != 0 means not-stored/will-retry. */
  if (((program_counter >= &egc_rplaca) &&
       (program_counter < &egc_rplacd_end)) ||
      ((program_counter >= &egc_gvset) &&
       (program_counter < &egc_write_barrier_end))) {
    LispObj *ea = 0, val = 0, root = 0;
    bitvector refbits = (bitvector)(lisp_global(REFBITS));
    Boolean need_check_memo = true, need_memoize_root = false;

    if (program_counter >= &egc_set_hash_key_conditional) {
      if ((program_counter < &egc_set_hash_key_conditional_test) ||
          ((program_counter == &egc_set_hash_key_conditional_test) &&
           ((xpGPR(xp, temp5) & 0xffffffff) != 0))) {
        return;
      }
      root = xpGPR(xp, arg_x);
      ea = (LispObj *)(root + unbox_fixnum(xpGPR(xp, temp0)));
      need_memoize_root = true;
    } else if (program_counter >= &egc_store_node_conditional) {
      if ((program_counter < &egc_store_node_conditional_test) ||
          ((program_counter == &egc_store_node_conditional_test) &&
           ((xpGPR(xp, temp5) & 0xffffffff) != 0))) {
        /* The conditional store either hasn't been attempted yet, or
           has failed.  No need to adjust the PC, or do memoization. */
        return;
      }
      ea = (LispObj *)(xpGPR(xp, arg_x) + unbox_fixnum(xpGPR(xp, temp0)));
      xpGPR(xp, arg_z) = t_value;
    } else if (program_counter >= &egc_set_hash_key) {
      if (program_counter < &egc_set_hash_key_did_store) {
        return;
      }
      root = xpGPR(xp, arg_x);
      val = xpGPR(xp, arg_z);
      ea = (LispObj *)(root + xpGPR(xp, arg_y) + misc_data_offset);
      need_memoize_root = true;
    } else if (program_counter >= &egc_gvset) {
      if (program_counter < &egc_gvset_did_store) {
        return;
      }
      ea = (LispObj *)(xpGPR(xp, arg_x) + xpGPR(xp, arg_y) + misc_data_offset);
      val = xpGPR(xp, arg_z);
    } else if (program_counter >= &egc_rplacd) {
      if (program_counter < &egc_rplacd_did_store) {
        return;
      }
      ea = (LispObj *)untag(xpGPR(xp, arg_y));       /* cdr @ untag+0 */
      val = xpGPR(xp, arg_z);
    } else {                      /* egc_rplaca */
      if (program_counter < &egc_rplaca_did_store) {
        return;
      }
      ea = ((LispObj *)untag(xpGPR(xp, arg_y))) + 1; /* car @ untag+8 */
      val = xpGPR(xp, arg_z);
    }
    if (need_check_memo) {        /* ppc:1964-1976 verbatim */
      natural bitnumber = area_dnode(ea, lisp_global(REF_BASE));
      if ((bitnumber < lisp_global(OLDSPACE_DNODE_COUNT)) &&
          ((LispObj)ea < val)) {
        atomic_set_bit(refbits, bitnumber);
        atomic_set_bit(global_refidx, bitnumber >> 8);
        if (need_memoize_root) {
          bitnumber = area_dnode(root, lisp_global(REF_BASE));
          atomic_set_bit(refbits, bitnumber);
          atomic_set_bit(global_refidx, bitnumber >> 8);
        }
      }
    }
    /* Barrier subprims are leaves: returning to LR skips the remaining
       asm barrier (the memoization just happened here).  ppc:1977 */
    set_xpPC(xp, xpLR(xp));
    return;
  }

  /* (b) marking a newly-allocated TSP frame as containing "raw" data.
     ppc:1983-1990 */
  if (instr == MARK_TSP_FRAME_INSTRUCTION) {
    LispObj tsp_val = xpGPR(xp, tsp);

    ((LispObj *)ptr_from_lispobj(tsp_val))[1] = tsp_val;
    adjust_exception_pc(xp, 4);
    return;
  }

  /* (a) storing into a newly-allocated lisp frame on the stack.
     ppc:1992-2032, re-derived for the marker frame: PPC detects a std
     to sp with a CREATE_LISP_FRAME (stdu) 1-3 instructions back and
     zeroes the not-yet-stored slots so the GC never sees garbage in a
     fresh frame.  The ARM64 marker frame is built sub sp,sp,#32 then
     marker@0 / savevsp@8 / savefn@16 / savelr@24 in that order
     (drafts' canonical sequence).  PROPOSED: compiler-emitted frame
     builds must keep this store order (flagged in the report; ARM32
     never had this window because its stmdb build was atomic). */
  if (IS_STR_TO_SP(instr) &&
      ((program_counter[-1] == CREATE_LISP_FRAME_INSTRUCTION) ||
       (program_counter[-2] == CREATE_LISP_FRAME_INSTRUCTION) ||
       (program_counter[-3] == CREATE_LISP_FRAME_INSTRUCTION))) {
    natural disp = STR_TO_SP_DISP(instr);

    if (disp < lisp_frame_size) {
      /* Slots at and above `disp' haven't been stored yet: zero them
         (the interrupted store re-executes on resume).  If even the
         marker hasn't landed, plant it so the frame is walkable. */
      if (disp == 0) {
        frame->marker = lisp_frame_marker;
      }
      if (disp <= 8) {
        frame->savevsp = 0;
      }
      if (disp <= 16) {
        frame->savefn = 0;
      }
      frame->savelr = 0;          /* disp <= 24 always true here */
      return;
    }
  }

  /* (c) consing / uvector allocation.  ppc:2034-2094 verbatim (the
     alloc-sequence decode lives in allocptr_displacement /
     finish_allocating_* above). */
  if (allocptr_tag != tag_fixnum) {
    signed_natural disp = allocptr_displacement(xp);

    if (disp) {
      /* Being architecturally "at" the alloc trap doesn't tell us
         whether the thread has committed to taking the trap.  Make the
         allocptr valid; the interrupt handler undoes this (interrupt
         case) or the trap is re-taken (GC case).  ppc:2036-2076 */
      if (alloc_disp) {
        *alloc_disp = disp;
        xpGPR(xp, allocptr) += disp;
      } else {
        update_bytes_allocated(tcr,
                               (void *)ptr_from_lispobj(cur_allocptr + disp));
        xpGPR(xp, allocbase) = VOID_ALLOCPTR;
        xpGPR(xp, allocptr) = VOID_ALLOCPTR - disp;
      }
    } else {
      /* Already past the alloc trap: finish allocating the object. */
      if (allocptr_tag == fulltag_cons) {
        finish_allocating_cons(xp);
      } else {
        if (allocptr_tag == fulltag_misc) {
          finish_allocating_uvector(xp);
        } else {
          Bug(xp, "what's being allocated here ?");
        }
      }
      xpGPR(xp, allocptr) = xpGPR(xp, allocbase) = VOID_ALLOCPTR;
    }
    return;
  }

  /* PPC's INIT_CATCH_FRAME partial-init back-out (ppc:2096-2110) is NOT
     ported yet: the ARM64 catch-frame build sequence (spentry-C
     build_catch_lisp_frame + catch push) has no settled single
     detection instruction, and the compiler side doesn't emit catch
     frames yet.  Risk window = interrupting a thread mid-catch-push;
     revisit when Matt's vinsns build catch frames (report item).
     PPC's stmw case (ppc:2112-2123) is PPC32-only; not ported. */
}

void
interrupt_handler(int signum, siginfo_t *info, ExceptionInformation *context)
{                                 /* ppc-exceptions.c:2132-2181 */
  TCR *tcr = get_interrupt_tcr(false);
  if (tcr) {
    if (TCR_INTERRUPT_LEVEL(tcr) < 0) {
      tcr->interrupt_pending = 1 << fixnumshift;
    } else {
      LispObj cmain = nrs_CMAIN.vcell;

      if ((fulltag_of(cmain) == fulltag_misc) &&
          (header_subtag(header_of(cmain)) == subtag_macptr)) {
        /* This thread can (allegedly) take an interrupt now.  It's
           tricky to do that if we're executing foreign code.  If we're
           unwinding the stack, we also want to defer the interrupt.
           ppc:2144-2151 */
        if ((tcr->valence != TCR_STATE_LISP) ||
            (tcr->unwinding != 0)) {
          TCR_INTERRUPT_LEVEL(tcr) = (1 << fixnumshift);
        } else {
          xframe_list xframe_link;
          int old_valence;
          signed_natural disp = 0;
          /* 16m41: same boundary save/restore as signal_handler. */
          natural old_last_lisp_frame = tcr->last_lisp_frame;

          tcr->last_lisp_frame = (natural)ptr_to_lispobj(xpSP(context));
          pc_luser_xp(context, tcr, &disp);
          old_valence = prepare_to_wait_for_exception_lock(tcr, context);
          wait_for_exception_lock_in_handler(tcr, context, &xframe_link);
          PMCL_exception_handler(signum, context, tcr, info, old_valence);
          if (disp) {
            xpGPR(context, allocptr) -= disp;
          }
          unlock_exception_lock_in_handler(tcr);
          exit_signal_handler(tcr, old_valence, old_last_lisp_frame);
        }
      }
    }
  }
}

void
install_signal_handler(int signo, void *handler, unsigned flags)
{                                 /* ppc-exceptions.c:2186-2207, verbatim */
  struct sigaction sa;
  int err;

  sa.sa_sigaction = (void *)handler;
  sigfillset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO;

  if (flags & RESTART_SYSCALLS)
    sa.sa_flags |= SA_RESTART;
  if (flags & RESERVE_FOR_LISP) {
    extern sigset_t user_signals_reserved;
    sigaddset(&user_signals_reserved, signo);
  }

  err = sigaction(signo, &sa, NULL);
  if (err) {
    perror("sigaction");
    exit(1);
  }
}

void
install_pmcl_exception_handlers()
{                                 /* ppc-exceptions.c:2210-2226 */
  extern int no_sigtrap;
  /* udf raises SIGILL on aarch64-linux; brk (the drafts' remaining
     placeholders + the debugger entry) raises SIGTRAP. */
  install_signal_handler(SIGILL, (void *)signal_handler, RESERVE_FOR_LISP);
  if (no_sigtrap != 1) {
    install_signal_handler(SIGTRAP, (void *)signal_handler, RESERVE_FOR_LISP);
  }
  install_signal_handler(SIGBUS, (void *)signal_handler, RESERVE_FOR_LISP);
  install_signal_handler(SIGSEGV, (void *)signal_handler, RESERVE_FOR_LISP);
  install_signal_handler(SIGFPE, (void *)signal_handler, RESERVE_FOR_LISP);

  install_signal_handler(SIGNAL_FOR_PROCESS_INTERRUPT,
                         (void *)interrupt_handler, RESERVE_FOR_LISP);
  signal(SIGPIPE, SIG_IGN);
}

void
thread_kill_handler(int signum, siginfo_t *info, ExceptionInformation *xp)
{                                 /* ppc-exceptions.c:2229-2255, verbatim */
  TCR *tcr = get_tcr(false);
  area *a;
  sigset_t mask;

  sigemptyset(&mask);

  if (tcr) {
    tcr->valence = TCR_STATE_FOREIGN;
    a = tcr->vs_area;
    if (a) {
      a->active = a->high;
    }
    a = tcr->ts_area;
    if (a) {
      a->active = a->high;
    }
    a = tcr->cs_area;
    if (a) {
      a->active = a->high;
    }
  }

  pthread_sigmask(SIG_SETMASK, &mask, NULL);
  pthread_exit(NULL);
}

void
thread_signal_setup()
{                                 /* ppc-exceptions.c:2258-2268, verbatim */
  thread_suspend_signal = SIG_SUSPEND_THREAD;
  thread_kill_signal = SIG_KILL_THREAD;

  install_signal_handler(thread_suspend_signal,
                         (void *)suspend_resume_handler,
                         RESERVE_FOR_LISP | RESTART_SYSCALLS);
  install_signal_handler(thread_kill_signal, (void *)thread_kill_handler,
                         RESERVE_FOR_LISP);
}

void
unprotect_all_areas()
{                                 /* ppc-exceptions.c:2272-2280, verbatim */
  protected_area_ptr p;

  for (p = AllProtectedAreas, AllProtectedAreas = NULL; p; p = p->next) {
    unprotect_area(p);
  }
}

/*
  The tlb-too-small trap (udf unary-misc sub 2) carries the INDEX
  register; extend the tcr's tlb so the index is in bounds, filling new
  pages with no_thread_local_binding_marker.  ppc-exceptions.c:2282-2321,
  3-arg signature per ARM32 (arm-exceptions.c:2014 -- the drafts' binding
  subprims reload tcr.tlb_limit from the tcr each time, so there is no
  live limit REGISTER to update, unlike PPC's twlle shape).
*/
Boolean
extend_tcr_tlb(TCR *tcr, ExceptionInformation *xp, unsigned idx_regno)
{
  unsigned
    index = (unsigned)(xpGPR(xp, idx_regno)),
    old_limit = tcr->tlb_limit,
    new_limit = align_to_power_of_2(index + 1, 12),
    new_bytes = new_limit - old_limit;
  LispObj
    *old_tlb = tcr->tlb_pointer,
    *new_tlb = realloc(old_tlb, new_limit),
    *work;

  if (new_tlb == NULL) {
    return false;
  }

  work = (LispObj *)((BytePtr)new_tlb + old_limit);

  while (new_bytes) {
    *work++ = no_thread_local_binding_marker;
    new_bytes -= sizeof(LispObj);
  }
  tcr->tlb_pointer = new_tlb;
  tcr->tlb_limit = new_limit;
  return true;
}

void
exception_init()
{                                 /* ppc-exceptions.c:2327-2331, verbatim */
  install_pmcl_exception_handlers();
}
