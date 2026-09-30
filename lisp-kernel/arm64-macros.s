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

/* Load the C global lisp_nil into dest (then usually ldr dest,[dest]).
 * Darwin/arm64 forbids text relocations from `ldr Rd, =sym`; use ADRP. */
.macro load_addr_of_lisp_nil dest
#if defined(__APPLE__)
        adrp    \dest, C(lisp_nil)@PAGE
        add     \dest, \dest, C(lisp_nil)@PAGEOFF
#else
        ldr     \dest, =C(lisp_nil)
#endif
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
 * can't ever appear half-built.  Building a frame must also look
 * atomic to an interrupt: process-interrupt runs lisp code on this
 * thread's tstack, starting at the interrupted tsp.
 *
 * We maintain that invariant in two ways.
 *
 *   1. For a fixed-size frame, where the size is specified with an
 *   immediate value, a single pre-indexed stp writes the backlink and
 *   a non-zero type and decrements tsp in one instruction -- there is
 *   no instant where tsp points at an un-formed frame.  (In case you
 *   were wondering, the transfer register and the base+writeback
 *   register must differ: stp tsp,tsp,[tsp,...]! is CONSTRAINED
 *   UNPREDICTABLE.  This is why the old tsp is saved in \tmp first.)
 *   If the stp faults on the tstack guard page, the writeback doesn't
 *   happen, so tsp is unchanged.
 *
 *   2. For a variable-size frame, where the size is specified with a
 *   register, we move tsp first and then store the backlink and type:
 *
 *       mov  scratch, tsp
 *       sub  tsp, tsp, size
 *       stp  scratch, scratch, [tsp]
 *
 *   We need the sub and stp to appear as an atomic operation to the
 *   gc and to other interrupts, because between the sub and the stp,
 *   tsp points at a frame whose backlink and type are junk.  The
 *   function rollback_tsp_frame_push() in the lisp kernel recognizes
 *   this exact instruction sequence.  When a thread is between the
 *   sub and the stp, it undoes the sub, and backs the pc up to it;
 *   the push starts over when the thread resumes.
 *
 *   pc_luser_xp() does this for a thread that is suspended for gc,
 *   interrupted, or waiting for the exception lock when another thread
 *   runs the gc.  handle_protection_violation() does it when the stp
 *   faults (typically on the guard page), before any lisp code runs
 *   on the faulting thread.  Once rolled back, the pc is at the sub,
 *   so a second rollback is a no-op.
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
        // tsp_frame.backlink = tsp
        // tsp_frame.type = tsp (non-zero value to mark frame as raw)
        stp \tmp, \tmp, [tsp, #-(\nbytes + tsp_frame.fixed_overhead)]!
.endm

/*
 * Mark the current (topmost) tsp frame as boxed, which indicates to
 * the gc that it should scan the contents of the frame.  Frames are
 * born raw via the allocators above, so there's no
 * Set_TSP_Frame_Unboxed counterpart.
 */
.macro Set_TSP_Frame_Boxed
        str xzr, [tsp, #tsp_frame.type] // zero means contains nodes
.endm

/*
 * Allocate a small frame for nodes.  nbytes must be a multiple of
 * node_size; tmp is a scratch register (it holds the old tsp during the
 * push, then serves as the zeroing cursor).
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
        .if ((\nbytes) / node_size) > 0
        // zero the data area: a cursor walks up from the first data word,
        // storing xzr with post-indexed addressing (same insn each time)
        add \tmp, tsp, #tsp_frame.fixed_overhead
        .rept (\nbytes) / node_size
        str xzr, [\tmp], #node_size
        .endr
        .endif
        Set_TSP_Frame_Boxed
.endm

/*
 * Variable-size unboxed frame.  size is a register, which must be dnode
 * aligned and already include tsp_frame.fixed_overhead; it is preserved.
 * scratch is a register which is left holding the old tsp.
 */
.macro TSP_Alloc_Var_Unboxed size, scratch
        mov \scratch, tsp
        sub tsp, tsp, \size           // space for the frame
        stp \scratch, \scratch, [tsp] // backlink + raw type
.endm

/*
 * Variable-size boxed frame.  size is a register, which must be dnode
 * aligned and already include tsp_frame.fixed_overhead; SIZE WILL BE
 * CLOBBERED.  scratch is a register which is left holding the old tsp.
 *
 * Like TSP_Alloc_Fixed_Boxed, this allocates a raw frame, zeroes it,
 * and only then marks it boxed.  The frame is live while it is being
 * zeroed, so it must stay raw until the data is clean.
 *
 * The zeroing is a guarded do-while: a leading test lets the data area
 * be empty (size == fixed_overhead), and the loop body itself carries
 * only one branch per word.
 */
.macro TSP_Alloc_Var_Boxed size, scratch
        TSP_Alloc_Var_Unboxed \size, \scratch
        // size is now a cursor
        add \size, tsp, #tsp_frame.fixed_overhead  // first data word
        cmp \size, \scratch     // empty? (size == fixed_overhead)
        b.hs .Ldone\@
.Lloop\@:
        str xzr, [\size], #node_size
        cmp \size, \scratch
        b.lo .Lloop\@
.Ldone\@:
        Set_TSP_Frame_Boxed
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

/*
 * Build a lisp frame on the control stack, saving the current vsp,
 * fn, and lr there.  This two-stp sequence is the sole sanctioned way
 * to build a lisp frame: pc_luser_xp recognizes this sequence so that
 * if a thread is suspended between the two stps, it will zero the
 * not-yet-stored savefn and savelr slots in the frame.  Compare with
 * the save-lisp-context vinsns.
 *
 * tmp is a register to contain the lisp frame marker.
 */
.macro build_lisp_frame tmp
        mov \tmp, #lisp_frame_marker
        stp \tmp, vsp, [sp, #-lisp_frame.size]!
        stp fn, lr, [sp, #lisp_frame.savefn]
.endm

/* pop a lisp frame off the control stack */
.macro discard_lisp_frame
        add sp, sp, #lisp_frame.size
.endm
