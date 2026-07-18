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
        b.hi .Lcons\@
        uuo_alloc
.Lcons\@:
        str \cdr, [allocptr, #cons.cdr]
        str \car, [allocptr, #cons.car]
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
        b.hi .Lmalloc\@
        uuo_alloc
.Lmalloc\@:
        str \header, [allocptr, #misc_header_offset]
        mov \dest, allocptr
        clear_allocptr_tag
        .endm

        .macro Misc_Alloc_Fixed dest, header, sizeconst
        sub allocptr, allocptr, #(\sizeconst - fulltag_misc)
        cmp allocptr, allocbase
        b.hi .Lmaf\@
        uuo_alloc
.Lmaf\@:
        str \header, [allocptr, #misc_header_offset]
        mov \dest, allocptr
        clear_allocptr_tag
        .endm

        .macro extract_header dest, miscobj
        ldur \dest, [\miscobj, #misc_header_offset]
        .endm
