/* SPDX-License-Identifier: Apache-2.0 */

/*
 * On arm64, we implement UUOs with the udf instruction.  The upper 16
 * bits of a udf instruction are 0; the lower 16 are an immediate.
 *
 * The udf instruction is architecturally undefined.  It will always
 * generate an undefined instruction exception (which will appear as
 * SIGILL).
 *
 * The hlt instruction could probably work here, or maybe brk, but udf
 * doesn't carry any extra semantic intent, so it seems like the best
 * choice.
 */

/*
 * The low 2 bits of the udf operand define the format of the
 * remaining upper 14 bits.
 *
 * NOTA BENE: udf #0 must remain reserved because it is used as a
 * sentinel instruction at the start of a code vector.
 */

/*
 * Very important: a uuo_format_misc UUO cannot be all 0.
 * 14 bits of info in 15:2 (must not be all zero)
 */
uuo_format_misc = 0

/*
 * Non-type single-register errors
 * reg in 6:2, 9 bits of info in 15:7
 */
uuo_format_unary = 1
  unary_info_not_callable = 0
  unary_info_no_throw_tag = 1
  unary_info_unbound = 2
  unary_info_udf = 3
  unary_info_udf_call = 4
  unary_info_tlb_too_small = 5

/*
 * Two-register errors
 * ra in 6:2, rb in 11:7, 4 bits of info in 15:12
 */
uuo_format_binary = 2
  binary_info_vector_bounds = 0
  binary_info_array_bounds = 1
  binary_info_integer_divide_by_zero = 2
  binary_info_eep_unresolved = 3
  binary_info_fpu_exception = 4
  binary_info_array_rank = 5
  binary_info_array_flags = 6
  binary_info_two_registers = 7  // extra register pair for preceding uuo

/*
 * All type errors: "this register doesn't hold the expected type"
 * reg in 6:2, continuable flag in 7, expected type code in 15:8
 *
 * The continuable flag means that the handler should attempt to make
 * the error continuable (by directly updating the register with the
 * user-supplied new value).
 *
 */
uuo_format_wrong_type = 3

        // misc format
        .macro uuo_misc info
        .if \info == 0
        .error "uuo_misc bits cannot be all 0"
        .endif
        udf #((\info) << 2 | uuo_format_misc)
        .endm

        .macro uuo_alloc
        uuo_misc 1
        .endm

        .macro uuo_gc_trap
        uuo_misc 2
        .endm

        .macro uuo_debug_trap
        uuo_misc 3
        .endm

        .macro uuo_interrupt_now
        uuo_misc 4
        .endm

        .macro uuo_suspend_now
        uuo_misc 5
        .endm

        .macro uuo_too_few_args
        uuo_misc 6
        .endm

        .macro uuo_too_many_args
        uuo_misc 7
        .endm

        .macro uuo_wrong_number_of_args
        uuo_misc 8
        .endm

        // unary format
        .macro uuo_unary reg, info
        udf # ((\info) << 7 | R\reg << 2 | uuo_format_unary)
        .endm

        .macro uuo_error_reg_not_callable reg
        uuo_unary \reg, unary_info_not_callable
        .endm

        .macro uuo_error_no_throw_tag reg
        uuo_unary \reg, unary_info_no_throw_tag
        .endm

        .macro uuo_error_unbound reg
        uuo_unary \reg, unary_info_unbound
        .endm

        .macro uuo_error_udf reg
        uuo_unary \reg, unary_info_udf
        .endm

        .macro uuo_error_udf_call reg
        uuo_unary \reg, unary_info_udf_call
        .endm

        .macro uuo_error_tlb_too_small reg
        uuo_unary \reg, unary_info_tlb_too_small
        .endm

        // binary format
        .macro uuo_binary ra, rb, info=0
        udf # ((\info) << 12 | (R\rb) << 7 | (R\ra) << 2 | uuo_format_binary)
        .endm

        .macro uuo_error_vector_bounds ra, rb
        uuo_binary \ra, \rb, binary_info_vector_bounds
        .endm

        .macro uuo_error_array_bounds ra, rb
        uuo_binary \ra, \rb, binary_info_array_bounds
        .endm

        .macro uuo_extra_registers ra, rb
        uuo_binary \ra, \rb, binary_info_two_registers
        .endm

        // wrong_type format
        .macro uuo_wrong_type reg, code, cflag=0
        udf # (\code << 8 | \cflag << 7 | R\reg << 2 | uuo_format_wrong_type)
        .endm

        // Keep these in sync with the values in arm64-arch.lisp.
        xtype_integer  = 0x18
        xtype_s64 = 0x28
        xtype_u64 = 0x38
        xtype_s32 = 0x48
        xtype_u32 = 0x58
        xtype_s16 = 0x68
        xtype_u16 = 0x78
        xtype_s8 = 0x88
        xtype_u8 = 0x98
        xtype_bit = 0xa8
        xtype_rational = 0xb8
        xtype_real = 0xc8
        xtype_number = 0xd8
        xtype_cons = 0xe8

        xtype_char_code = 0x10
        xtype_unsigned_byte_24 = 0x20
        xtype_array2d = 0x30
        xtype_array3d = 0x40
        xtype_null = 0x50

        .macro uuo_error_reg_not_lisptag reg, lisptag
        uuo_wrong_type \reg, \lisptag, 0
        .endm
        .macro uuo_cerror_reg_not_lisptag reg, lisptag
        uuo_wrong_type \reg, \lisptag, 1
        .endm

        .macro uuo_error_reg_not_fulltag reg, fulltag
        uuo_wrong_type \reg, \fulltag, 0
        .endm
        .macro uuo_cerror_reg_not_fulltag reg, fulltag
        uuo_wrong_type \reg, \fulltag, 1
        .endm

        .macro uuo_error_reg_not_xtype reg, xtype
        uuo_wrong_type \reg, \xtype, 0
        .endm
        .macro uuo_cerror_reg_not_xtype reg, xtype
        uuo_wrong_type \reg, \xtype, 1
        .endm
