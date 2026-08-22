/* SPDX-License-Identifier: Apache-2.0 */

#include "arm64-constants.h"
#include "arm64-uuo.s"
#include "arm64-asm.h"
#include "arm64-lisp-globals.s"

.macro note_function_start name
#if !defined(__APPLE__)
        /* mark the symbol as a function for ELF platforms */
        .type \name, %function
#endif
.endm

.macro note_function_end name
#if !defined(__APPLE__)
        /* record function size for ELF platforms */
        .size \name, . - \name
#endif
.endm

.macro spentry name
        .text
        .p2align 2
        .global _SP\name
        note_function_start _SP\name
_SP\name:
.endm

.macro endsp name
        note_function_end _SP\name
.endm

.macro clear_allocptr_tag
        bic allocptr, allocptr, #fulltagmask
.endm

.macro Cons dest, car, cdr
        sub allocptr, allocptr, #(cons.size - fulltag_cons)
        cmp allocptr, allocbase
        b.hi .Linit\@
        uuo_alloc
.Linit\@:
        stur \cdr, [allocptr, #cons.cdr]
        stur \car, [allocptr, #cons.car]
        mov \dest, allocptr
        clear_allocptr_tag
.endm

// dest: a node register for the newly allocated object
// header: an unboxed register with the desired header
// size: an unboxed register with desired size in bytes
.macro Misc_Alloc dest, header, size
        sub \size, \size, #fulltag_misc
        sub allocptr, allocptr, \size
        cmp allocptr, allocbase
        b.hi .Linit\@
        uuo_alloc
.Linit\@:
        stur \header, [allocptr, #misc_header_offset]
        mov \dest, allocptr
        clear_allocptr_tag
.endm

.macro Misc_Alloc_Fixed dest, header, sizeconst
        sub allocptr, allocptr, #(\sizeconst - fulltag_misc)
        cmp allocptr, allocbase
        b.hi .Linit\@
        uuo_alloc
.Linit\@:
        stur \header, [allocptr, #misc_header_offset]
        mov \dest, allocptr
        clear_allocptr_tag
.endm

// Round a RUNTIME byte count up to a dnode (16-byte) boundary: \dest :=
// (\src + \delta + dnode_size-1) & ~(dnode_size-1).  \delta is a bare
// immediate expression (any fixed overhead to fold in before rounding);
// pass 0 for a plain round-up.  \dest and \src may be the same register.
// ppc-macros.s dnode_align.  For a count known at assembly time, fold it
// into the immediate directly, or use aligned_bignum_size() (arm64-
// constants.h) for bignums -- both do the same rounding at cpp time.
.macro dnode_align dest, src, delta
        add \dest, \src, #((\delta) + (dnode_size - 1))
        and \dest, \dest, #~(dnode_size - 1)
.endm

/*
 * Temp-stack (tsp) frame allocation.  Canonical home for every TSP
 * allocator; call sites should use these rather than open-coding a frame.
 *
 * A tsp frame is [backlink][type][data...]; the tstack grows DOWN, so tsp
 * points at the backlink (lowest address of the frame) and the data area
 * begins at tsp+tsp_frame.fixed_overhead.
 *
 * GC SAFETY.  The collector walks the tstack from the live tsp register
 * upward (arm64-gc.c mark_tstack_area): at each frame it follows *tsp as
 * the backlink and, iff tsp[1] (type) == 0, scans the data area as boxed
 * nodes.  So at EVERY instruction boundary the frame that tsp points at
 * must already have (a) a valid backlink and (b) a non-zero type, UNLESS
 * its data area is fully node-clean.  tstack memory is otherwise full of
 * arbitrary junk, so a frame must never become visible half-built.
 *
 * We keep that invariant WITHOUT pc_luser_xp help, two ways:
 *
 *   Fixed size (immediate): a single pre-index stp writes the backlink and
 *   a non-zero type AND decrements tsp in one instruction -- there is no
 *   instant where tsp points at an un-formed frame.  (The transfer reg and
 *   the base+writeback reg must differ -- stp tsp,tsp,[tsp,...]! is
 *   CONSTRAINED UNPREDICTABLE -- hence old tsp is parked in \tmp first.)
 *
 *   Variable size (register): build the whole frame BELOW the still-live
 *   tsp (invisible to the collector), then publish it with a single
 *   `mov tsp, scratch'.  A GC on either side of that mov sees a complete
 *   frame; nothing is ever half-published.
 *
 * A non-zero type marks the frame "raw" (GC skips it).  We use the saved
 * old tsp as a handy non-zero value; there's nothing else special about it.
 */

// Fixed unboxed frame.  \nbytes is an immediate data size; the whole frame
// (\nbytes + fixed_overhead) must fit the pre-index imm range (<= 504, i.e.
// \nbytes <= 488) -- larger fixed frames must use the variable form.
.macro TSP_Alloc_Fixed_Unboxed nbytes, tmp
        mov \tmp, tsp
        stp \tmp, \tmp, [tsp, #-(\nbytes + tsp_frame.fixed_overhead)]!
.endm

// Fixed boxed frame: push raw, zero the data area while still raw (a GC
// there skips the frame), then flip the type to boxed LAST.
.macro TSP_Alloc_Fixed_Boxed nbytes, tmp
        TSP_Alloc_Fixed_Unboxed \nbytes, \tmp
        .set _tspab_off, tsp_frame.fixed_overhead
        .rept (\nbytes) / node_size
        str xzr, [tsp, #_tspab_off]
        .set _tspab_off, _tspab_off + node_size
        .endr
        str xzr, [tsp, #tsp_frame.type]
.endm

// Mark the current (topmost) tsp frame boxed / unboxed.
.macro Set_TSP_Frame_Boxed
        str xzr, [tsp, #tsp_frame.type]         // zero => GC scans data as nodes
.endm
.macro Set_TSP_Frame_Unboxed
        str tsp, [tsp, #tsp_frame.type]         // non-zero => GC skips the frame
.endm

// Variable unboxed frame.  \size is a register: 16-aligned total byte count
// INCLUDING tsp_frame.fixed_overhead.  \scratch is clobbered.
.macro TSP_Alloc_Var_Unboxed size, scratch
        sub \scratch, tsp, \size
        stp tsp, tsp, [\scratch]                // backlink + raw type, below live tsp
        mov tsp, \scratch                       // publish the finished frame
.endm

// Variable boxed frame.  \size (register, 16-aligned, includes fixed
// overhead) is CLOBBERED (reused as the zeroing cursor); \scratch too.  The
// leading test lets the data area be empty (\size == fixed_overhead).
.macro TSP_Alloc_Var_Boxed size, scratch
        sub \scratch, tsp, \size
        str tsp, [\scratch, #tsp_frame.backlink]
        add \size, \scratch, #tsp_frame.fixed_overhead  // cursor := first data word
.Ltspvb\@:
        cmp \size, tsp                          // end := old tsp (== base + size)
        b.hs .Ltspvbdone\@
        str xzr, [\size], #node_size
        b .Ltspvb\@
.Ltspvbdone\@:
        str xzr, [\scratch, #tsp_frame.type]    // boxed, still unpublished
        mov tsp, \scratch                       // publish zeroed boxed frame
.endm

// As TSP_Alloc_Var_Boxed, but the caller guarantees a non-empty data area
// (\size > fixed_overhead), so the zeroing runs at least once: drop the
// leading test for a do-while.
.macro TSP_Alloc_Var_Boxed_nz size, scratch
        sub \scratch, tsp, \size
        str tsp, [\scratch, #tsp_frame.backlink]
        add \size, \scratch, #tsp_frame.fixed_overhead
.Ltspvbnz\@:
        str xzr, [\size], #node_size
        cmp \size, tsp
        b.lo .Ltspvbnz\@
        str xzr, [\scratch, #tsp_frame.type]
        mov tsp, \scratch
.endm

// Pop the topmost tsp frame.
.macro TSP_Unlink
        ldr tsp, [tsp, #tsp_frame.backlink]
.endm

.macro extract_header dest, miscobj
        ldur \dest, [\miscobj, #misc_header_offset]
.endm

/*
 * Check for interrupts.  Clobbers nargs: should use a passed-in scratch reg.
 */
.macro check_pending_interrupt
        ldr nargs, [rcontext, #tcr.tlb_pointer]
        ldr nargs, [nargs, #INTERRUPT_LEVEL_BINDING_INDEX]
        cmp nargs, #0
        b.lt .Ldone\@           // interrupts are disabled
        b.gt .Ltrap\@           // a deferred interrupt is waiting: take it
        ldr nargs, [rcontext, #tcr.interrupt_pending]
        cbz nargs, .Ldone\@     // skip if no interrupt pending
.Ltrap\@:
        uuo_interrupt_now
.Ldone\@:
.endm

/* value stack push/pop  */
.macro vpush1 reg
        str \reg, [vsp, #-node_size]!
.endm
.macro vpop1 reg
        ldr \reg, [vsp], #node_size
.endm

/* n is a plain constant; the macro boxes it as a fixnum into nargs */
.macro set_nargs n
        mov nargs, #((\n)<<fixnumshift)
.endm

/* pop a lisp frame off the control stack */
.macro discard_lisp_frame
        add sp, sp, #lisp_frame.size
.endm
