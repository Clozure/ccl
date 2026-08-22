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

/*
 * Round src (a register) + extra (a constant expression) up to the
 * nearest dnode boundary.
 *
 * dest = (src + add + dnode_size - 1) & ~(dnode_size - 1)
 */
.macro dnode_align dest, src, extra=0
        add \dest, \src, #((\extra) + (dnode_size - 1))
        and \dest, \dest, #~(dnode_size - 1)
.endm

/*
 * Temp-stack (tsp) frame allocation.  Always use these macros to
 * create tsp frames.
 *
 * A tsp frame is [backlink][type][data...]; the tstack grows down, so
 * tsp points at the backlink (lowest address of the frame) and the
 * data area begins at tsp+tsp_frame.fixed_overhead.
 *
 * When the type field is non-zero, that means that the frame contains
 * raw, unboxed data.  The gc will skip over that data.  When the type
 * field is 0, the contents of the frame are treated as nodes that the
 * gc will scan.
 *
 * Because the gc walks the temp stack (see mark_tstack_area), it's
 * essential for the tsp to point to a valid frame at all times: it
 * can't ever appear half-built.
 *
 * We maintain that invariant in two ways.
 *
 *   For a fixed-size frame, where the size is specified with an
 *   immediate value, a single pre-indexed stp writes the backlink and
 *   a non-zero type and decrements tsp in one instruction -- there is
 *   no instant where tsp points at an un-formed frame.  (In case you
 *   were wondering, the transfer register and the base+writeback
 *   register must differ: stp tsp,tsp,[tsp,...]! is CONSTRAINED
 *   UNPREDICTABLE.  This is why the old tsp is saved in \tmp first.)
 *
 *   For a variable-size frame, where the size is specified with a
 *   register, we build the whole frame below the still-live tsp
 *   (invisible to the gc), then make it visible with a single `mov
 *   tsp, scratch'.  A GC on either side of that mov sees a complete
 *   frame; nothing is ever half-published.
 *
 * When initializing the type field of a tsp frame, we will sometimes
 * use the old tsp as a handy non-zero value to mark it as a raw
 * frame.  There's nothing otherwise special about it.
 */

/*
 * Allocate a fixed-size tsp frame.  nbytes is a constant specifying
 * how large the data area in the frame should be, and tmp is a
 * register to hold a copy of the tsp.  Note that the whole frame
 * (nbytes + fixed_overhead) has to fit into the pre-index imm range
 * (<= 504; in other words, nbytes must be <= 488).
 */
.macro TSP_Alloc_Fixed_Unboxed nbytes, tmp
        mov \tmp, tsp
        stp \tmp, \tmp, [tsp, #-(\nbytes + tsp_frame.fixed_overhead)]!
.endm

/*
 * Mark the current (topmost) tsp frame boxed or unboxed.
 */
.macro Set_TSP_Frame_Boxed
        str xzr, [tsp, #tsp_frame.type] // zero => contains nodes
.endm
.macro Set_TSP_Frame_Unboxed
        str tsp, [tsp, #tsp_frame.type] // non-zero => raw data
.endm

/*
 * Allocate a small frame for nodes.  nbytes must be a multiple of
 * node_size, and tmp is a register that will hold a copy of tsp.
 *
 * This works by allocating a raw frame, zeroing it, and then setting
 * the type field of the frame to 0 (nodes).
 */
#define TSP_FIXED_BOXED_MAX_NODES 8
.macro TSP_Alloc_Fixed_Boxed nbytes, tmp
        .if ((\nbytes) % node_size) != 0
        .error "nbytes must be a multiple of node_size"
        .endif
        .if ((\nbytes) / node_size) > TSP_FIXED_BOXED_MAX_NODES
        .error "frame too large: use TSP_Alloc_Var_Boxed"
        .endif
        TSP_Alloc_Fixed_Unboxed \nbytes, \tmp
        .set _tspab_off\@, tsp_frame.fixed_overhead
        .rept (\nbytes) / node_size
        str xzr, [tsp, #_tspab_off\@]
        .set _tspab_off\@, _tspab_off\@ + node_size
        .endr
        Set_TSP_Frame_Boxed
.endm

/*
 * Variable-size unboxed frame.  size is a register, which must be dnode
 * aligned and already include tsp_frame.fixed_overhead.  scratch is a
 * register which will be clobbered.
 */
.macro TSP_Alloc_Var_Unboxed size, scratch
        sub \scratch, tsp, \size // space for frame
        stp tsp, tsp, [\scratch] // backlink + raw type, below live tsp
        mov tsp, \scratch        // publish the finished frame
.endm

/*
 * Variable-size boxed frame.  size is a register, which must be dnode
 * aligned and already include tsp_frame.fixed_overhead; SIZE WILL BE
 * CLOBBERED.  scratch is another register which will be clobbered too.
 *
 * The zeroing is a guarded do-while: a leading test lets the data area
 * be empty (size == fixed_overhead), and the loop body itself carries
 * only one branch per word.
 */
.macro TSP_Alloc_Var_Boxed size, scratch
        sub \scratch, tsp, \size // make room
        str tsp, [\scratch, #tsp_frame.backlink]
        // size is now a cursor
        add \size, \scratch, #tsp_frame.fixed_overhead  // first data word
        cmp \size, tsp          // empty? (size == fixed_overhead)
        b.hs .Ldone\@
.Lloop\@:
        str xzr, [\size], #node_size
        cmp \size, tsp
        b.lo .Lloop\@
.Ldone\@:
        str xzr, [\scratch, #tsp_frame.type]  // set boxed, still not visible
        mov tsp, \scratch                     // make boxed frame visible
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
