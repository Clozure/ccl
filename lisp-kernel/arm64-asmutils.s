/* SPDX-License-Identifier: Apache-2.0 */

#include "arm64-constants.h"
#include "arm64-macros.s" /* pulls arm64-uuo.s @ 115b7aa */

/*
 * PPC64 LINE-PORT (source: vendor/ccl/lisp-kernel/ppc-asmutils.s)
 *
 * The missing arm64-asmutils.o in Matt's linuxarm64/Makefile (ASMOBJ).
 * Logic from ppc-asmutils.s; AArch64 mechanics from the ARM-family
 * analog arm-asmutils.s (in Matt's tree) where PPC differs -- such
 * blocks carry ARM64-DEVIATION tags. Plain GNU as + cpp, same as the
 * spentry-* drafts.
 *
 * PORT-NOTE / inventory vs the two sources:
 * - zero_cache_lines (ppc:24): NOT ported -- its only caller
 * (memory.c:492) is inside a PPC-only branch (dcbz page-zeroing).
 * - flush_cache_lines: A64 has userspace cache maintenance; the
 * canonical dc cvau / ic ivau loop replaces PPC's dcbf/icbi and
 * ARM32's __ARM_NR_cacheflush syscall. Signature kept from PPC
 * (base, nlines, line_size) to match xMakeDataExecutable's PPC-shaped
 * call (pmcl-kernel.c:2212-2221 needs an ARM64 branch upstream --
 * flagged in the gc report, open question 7).
 * - set/get/zero_fpscr (ppc:86-116): PPC's one FPSCR splits into
 * FPCR (control) + FPSR (status); we export fpcr/fpsr pairs AND
 * keep PPC-named wrappers reading the STATUS reg (they're what the
 * FP-exception POLLING model reads -- Matt 2026-07-11 mail).
 * - save/restore_fp_context, put/get_vector_registers (ppc:118-198,
 * 280+): ARM32 ships these as no-op stubs (arm-asmutils.s:202-213);
 * same here, with the same rationale (the C signal path gets FP
 * state from the mcontext, not from these).
 * - store_conditional / atomic_swap / atomic_ior / atomic_and
 * (ppc:202-258): ldxr/stxr loops, status in w9 (a C-volatile scratch
 * -- NOT the Lisp w17 convention; this file runs on the C side).
 * atomic_swap_acquire + release_spin_lock added from the ARM32 set
 * (arm-asmutils.s:135-158) -- thread_manager.c uses them.
 * - pseudo_sigreturn (ppc:270): trivial trap-return marker, ARM32
 * shape (arm-asmutils.s:196-199).
 * - rt_sigprocmask / call_handler_on_main_stack / __aeabi_uldivmod
 * (ARM32-only: android syscall shim, arm-exceptions.c helper, EABI
 * division ABI): NOT ported -- no ARM64 caller.
 * - dmb/dsb/isb helpers (arm-asmutils.s:255-266): kept.
 *
 * AAPCS64: args x0-x7, result x0; x9-x15 volatile scratch.
 */

        .text

/* Flush the data cache to the point of unification and invalidate the
 * instruction cache: base = x0, nlines = x1, line_size = x2.
 * ppc-asmutils.s:37-54 (dcbf/isync/icbi loops collapse into one A64
 * loop; ARM64-DEVIATION: A64 allows EL0 dc cvau/ic ivau -- no syscall,
 * unlike ARM32's __ARM_NR_cacheflush (arm-asmutils.s:25-40)). */
        .globl C(flush_cache_lines)
C(flush_cache_lines):
        mul     x1, x1, x2              /* total bytes                  */
        add     x1, x0, x1              /* end                          */
        mov     x9, x0
1:      dc      cvau, x9                /* clean D to PoU               */
        add     x9, x9, x2
        cmp     x9, x1
        b.lo    1b
        dsb     ish
        mov     x9, x0
2:      ic      ivau, x9                /* invalidate I to PoU          */
        add     x9, x9, x2
        cmp     x9, x1
        b.lo    2b
        dsb     ish
        isb
        ret

/* ppc-asmutils.s:58-65 / arm-asmutils.s:42-49: dirty a page (write a
 * word, write it back to 0), return 1. The C protection-fault path
 * compares the faulting PC against [touch_page, touch_page_end). */
        .globl C(touch_page)
        .globl C(touch_page_end)
C(touch_page):
        str     x0, [x0]
        mov     x9, #0
        str     x9, [x0]
        mov     x0, #1
C(touch_page_end):
        ret

/* ppc-asmutils.s:68-70 */
        .globl C(current_stack_pointer)
C(current_stack_pointer):
        mov     x0, sp
        ret

/* ppc-asmutils.s:73-79 (cntlzw) -> A64 clz.  ARM64-DEVIATION: 64-bit
 * clz (callers pass natural). */
        .globl C(count_leading_zeros)
C(count_leading_zeros):
        clz     x0, x0
        ret

/* ppc-asmutils.s:82-83 */
        .globl C(noop)
C(noop):
        ret

/* PPC's one FPSCR splits into FPCR (control) / FPSR (status).
 * ppc-asmutils.s:86-116; polled-FP model per Matt 2026-07-11. */
        .globl C(get_fpscr)             /* PPC name: reads STATUS       */
C(get_fpscr):
        mrs     x0, fpsr
        ret
        .globl C(set_fpscr)             /* PPC name: writes CONTROL     */
C(set_fpscr):
        msr     fpcr, x0
        ret
        .globl C(zero_fpscr)            /* clear accrued status         */
C(zero_fpscr):
        msr     fpsr, xzr
        ret
        .globl C(get_fpcr)
C(get_fpcr):
        mrs     x0, fpcr
        ret
        .globl C(set_fpcr)
C(set_fpcr):
        msr     fpcr, x0
        ret
        .globl C(get_fpsr)
C(get_fpsr):
        mrs     x0, fpsr
        ret
        .globl C(set_fpsr)
C(set_fpsr):
        msr     fpsr, x0
        ret

/* ARM32 ships these as stubs (arm-asmutils.s:202-213): the signal path
 * reads FP state from the mcontext. Same here. */
        .globl C(save_fp_context)
C(save_fp_context):
        ret
        .globl C(restore_fp_context)
C(restore_fp_context):
        ret
        .globl C(put_vector_registers)
C(put_vector_registers):
        ret
        .globl C(get_vector_registers)
C(get_vector_registers):
        ret

/* Atomically store new (x2) in *x0 if old == expected (x1); return the
 * actual old value. ppc-asmutils.s:202-216 (lrarx/strcx) -> ldxr/stxr;
 * status in w9 (C-side scratch). */
        .globl C(store_conditional)
C(store_conditional):
1:      ldxr    x3, [x0]
        cmp     x3, x1
        b.ne    2f
        stxr    w9, x2, [x0]
        cbnz    w9, 1b
        dmb     ish
        mov     x0, x3
        ret
2:      clrex                           /* abandon the reservation      */
        mov     x0, x3
        ret

/* Atomically store x1 in *x0; return the old value.
 * ppc-asmutils.s:219-229. */
        .globl C(atomic_swap)
C(atomic_swap):
1:      ldxr    x3, [x0]
        stxr    w9, x1, [x0]
        cbnz    w9, 1b
        dmb     ish
        mov     x0, x3
        ret

/* atomic_swap with acquire semantics + spin-lock release: ARM32 set
 * (arm-asmutils.s:135-158); thread_manager.c's spin locks use them. */
        .globl C(atomic_swap_acquire)
C(atomic_swap_acquire):
1:      ldaxr   x3, [x0]
        stxr    w9, x1, [x0]
        cbnz    w9, 1b
        mov     x0, x3
        ret
        .globl C(release_spin_lock)
C(release_spin_lock):
        stlr    xzr, [x0]
        ret

/* Logically OR x1 into *x0; return the new value.
 * ppc-asmutils.s:232-244. */
        .globl C(atomic_ior)
C(atomic_ior):
1:      ldxr    x3, [x0]
        orr     x3, x3, x1
        stxr    w9, x3, [x0]
        cbnz    w9, 1b
        dmb     ish
        mov     x0, x3
        ret

/* Logically AND x1 into *x0; return the new value.
 * ppc-asmutils.s:247-258. */
        .globl C(atomic_and)
C(atomic_and):
1:      ldxr    x3, [x0]
        and     x3, x3, x1
        stxr    w9, x3, [x0]
        cbnz    w9, 1b
        dmb     ish
        mov     x0, x3
        ret

/* FP exceptions are POLLED on arm64 (Matt 2026-07-11); the PPC
 * prctl-based enable/disable (ppc-asmutils.s:260-267) are no-ops, as on
 * ARM32 (arm-asmutils.s:186-193). The C-side pair in
 * arm64-exceptions.c are the ones actually linked; these asm names are
 * NOT exported to avoid duplicate symbols. */

/* ppc-asmutils.s:270-277 / arm-asmutils.s:196-199: a recognizable
 * do-nothing marker the exception path can resume through. */
        .globl C(pseudo_sigreturn)
C(pseudo_sigreturn):
        ret

/* Barrier helpers (ARM32 set, arm-asmutils.s:255-266). */
        .globl C(dmb)
C(dmb):
        dmb     ish
        ret
        .globl C(dsb)
C(dsb):
        dsb     ish
        ret
        .globl C(isb)
C(isb):
        isb
        ret

        .end
