/* SPDX-License-Identifier: Apache-2.0 */

/* Mach-O wants a leading underscore; ELF doesn't. */
#if defined(__APPLE__)
#define C(name) _##name
#else
#define C(name) name
#endif

        .macro spentry name
        .text
        .p2align 4
        .global _SP\name
#if !defined(__APPLE__)
        /* mark the symbol as a function for ELF platforms */
        .type _SP\name, %function
#endif
_SP\name:
        .endm

        .macro  endsp name
#if !defined(__APPLE__)
        /* record function size for ELF platforms */
        .size _SP\name, . - _SP\name
#endif
        .endm        

        .macro uuo_alloc_trap
        udf #1
        .endm

        .macro clear_allocptr_tag
        bic allocptr, allocptr, #fulltagmask
        .endm

        .macro Cons dest, car, cdr
        sub allocptr, allocptr, #(cons.size - fulltag_cons)
        cmp allocptr, allocbase
        b.hi 1f
        uuo_alloc_trap
1:      str \cdr, [allocptr, #cons.cdr]
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
        b.hi 1f
        uuo_alloc_trap
1:      str \header, [allocptr, #misc_header_offset]
        mov \dest, allocptr
        clear_allocptr_tag
        .endm

        .macro Misc_Alloc_Fixed dest, header, sizeconst
        sub allocptr, allocptr, #(\sizeconst - fulltag_misc)
        cmp allocptr, allocbase
        b.hi 1f
        uuo_alloc_trap
1:      str \header, [allocptr, #misc_header_offset]
        mov \dest, allocptr
        clear_allocptr_tag
        .endm
