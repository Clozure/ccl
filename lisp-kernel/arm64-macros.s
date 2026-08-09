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
