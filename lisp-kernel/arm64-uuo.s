/* SPDX-License-Identifier: Apache-2.0 */

/*
 * On arm64, we implement UUOs with the udf instruction.
 * The upper 16 bits of a udf instruction are 0; the lower 16
 * are an immediate.
 *
 * The udf instruction is architecturally undefined.  It will always
 * generate an undefined instruction exception (which will appear
 * as SIGILL).
 *
 * I prefer udf to attempting to use hlt or brk.  I can imagine using
 * brk in some special cases.
 */

/*
 * NOTA BENE: udf #0 must remain reserved because it is used
 * as a sentinel instruction at the start of a code vector.
 */

/*
 * The low 2 bits of the udf operand define the format of the remaining
 * upper 14 bits.  Maybe it would be better to use 3 bits for the format.
 */

// This doesn't need to be 100% reserved, but a uuo can't be all 0.
uuo_format_reserved = 0
uuo_format_unary = 1   // bits 6:2 encode a register; 9 bits of info in 15:7 
  unary_info_lisptag = 0
  unary_info_fulltag = 1
  unary_info_subtag = 2
  unary_info_xtype = 3
  unary_info_not_callable = 4
  unary_info_no_throw_tag = 5
  unary_info_tlb_too_small = 6
  unary_info_error_unbound = 7
uuo_format_binary = 2  // ra in 6:2, rb in 11:7, 4 bits of info in 15:12
  binary_info_vector_bounds = 0
  binary_info_tbd1 = 1
  binary_info_tbd2 = 2
  binary_info_tbd3 = 3
  binary_info_tbd4 = 4
  binary_info_tbd5 = 5
  binary_info_tbd6 = 6
  binary_info_tbd7 = 7
uuo_format_misc = 3    // arbitrary info in upper 14 bits

        // misc format
        .macro uuo_misc info
        udf #((\info) << 2 | uuo_format_misc)
        .endm

        .macro uuo_alloc
        uuo_misc 0
        .endm

        .macro uuo_gc_trap
        uuo_misc 1
        .endm

        .macro uuo_debug_trap
        uuo_misc 2
        .endm

        .macro uuo_interrupt_now
        uuo_misc 3
        .endm

        .macro uuo_suspend_now
        uuo_misc 4
        .endm

        .macro uuo_too_few_args
        uuo_misc 5
        .endm

        .macro uuo_too_many_args
        uuo_misc 6
        .endm

        .macro uuo_wrong_number_of_args
        uuo_misc 7
        .endm

        // unary format
        .macro uuo_unary reg, info
        udf # ((\info) << 7 | R\reg << 2 | uuo_format_unary)
        .endm

        .macro uuo_error_reg_not_lisptag reg, lisptag
        uuo_unary \reg, (\lisptag << 3 | unary_info_lisptag)
        .endm

        .macro uuo_error_reg_not_fulltag reg, fulltag
        uuo_unary \reg, (\fulltag << 3 | unary_info_fulltag)
        .endm

        // I think this is going to be inadequate: xtype needs a
        // bigger range (probably 8 bits at least).
        .macro uuo_error_reg_not_xtype reg, xtype
        uuo_unary \reg, (\xtype << 3 | unary_info_xtype)
        .endm

        .macro uuo_error_reg_not_callable reg
        uuo_unary \reg, unary_info_not_callable
        .endm

        .macro uuo_error_no_throw_tag reg
        uuo_unary \reg, unary_info_no_throw_tag
        .endm

        .macro uuo_error_tlb_too_small reg
        uuo_unary \reg, unary_info_tlb_too_small
        .endm

        .macro uuo_error_unbound reg
        uuo_unary \reg, unary_info_unbound
        .endm

        // binary format
        .macro uuo_binary ra, rb, info=0
        udf # ((\info) << 12 | (R\rb) << 7 | (R\ra) << 2 | uuo_format_binary)
        .endm

        .macro uuo_error_vector_bounds ra, rb
        uuo_binary \ra, \rb
        .endm
