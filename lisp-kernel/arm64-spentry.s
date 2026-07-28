/* SPDX-License-Identifier: Apache-2.0 */

#include "arm64-constants.h"
#include "arm64-macros.s"

/*
 * The fixnum in arg_z {over,under}flowed by one bit as the result
 * of an addition or subtraction.  Make a bignum out of it.
 */
spentry fix_overflow
C(fix_one_bit_overflow):
        asr imm0, arg_z, #fixnumshift
        eor imm0, imm0, #0xe000000000000000
        mov imm1, #two_digit_bignum_header
        Misc_Alloc_Fixed arg_z, imm1, aligned_bignum_size(2)
        str imm0, [arg_z, #misc_data_offset]
        ret
endsp fix_overflow
        
/*
 * There's a signed 64-bit value in imm0.  Make a Lisp integer
 * from it (either a fixnum or a two-digit bignum).
 */
spentry makes64
        sbfx imm1, imm0, #0, #(nbits_in_word - nfixnumtagbits)
        cmp imm0, imm1
        b.ne 1f
        lsl arg_z, imm0, #fixnumshift
        ret
1:      mov imm1, #two_digit_bignum_header
        Misc_Alloc_Fixed arg_z, imm1, aligned_bignum_size(2)
        stur imm0, [arg_z, #misc_data_offset]
        ret
endsp makes64

/*
 * There's an unsigned 64-bit value in imm0.  Make a Lisp integer
 * from it (either a fixnum or a two- or three-digit bignum).
 */
spentry makeu64
        tst imm0, #(0xf << 60)
        b.ne 1f
        // Top 4 bits clear means the value is an (unsigned-byte 60)
        // and therefore fits in the (signed-byte 61) fixnum range.
        lsl arg_z, imm0, #fixnumshift
        ret
1:      mov imm1, #two_digit_bignum_header
        mov imm2, #aligned_bignum_size(2)
        tbz imm0, #63, 2f       // bit 63 clear: fits in 2 digits
        // need three digits after all
        mov imm1, #three_digit_bignum_header
        mov imm2, #aligned_bignum_size(3)
2:      Misc_Alloc arg_z, imm1, imm2
        stur imm0, [arg_z, #misc_data_offset]
        // If there is a third digit, it is implicity 0 because alloc
        // returns zero-filled memory.
        ret
endsp makeu64

/*
 * There's a miscobj reference in arg_y.  Reference index arg_z of
 * said miscobj, and return a properly-tagged lisp object in arg_z.
 * Do type and bounds checking.
 */

// Obviously this is not complete. It's just for playing with the uuos.
spentry misc_ref_upstream_sketch
        and imm0, arg_y, #fulltagmask
        cmp imm0, #fulltag_misc
        bne 0f
        tst arg_z, #fixnummask
        bne 1f
        ldur imm0, [arg_y, #misc_header_offset]
        ubfx imm0, arg_y, #8, #56
        lsr imm1, arg_z, #fixnumshift
        cmp imm0, imm1
        b.hi 2f
        b C(misc_ref_common)
0:      uuo_error_reg_not_fulltag arg_y, fulltag_misc
1:      uuo_error_reg_not_lisptag arg_z, tag_fixnum
2:      uuo_error_vector_bounds arg_z, arg_y
endsp misc_ref_upstream_sketch

C(misc_ref_common):
        ret

/*
 * A read-only vector of subprim addresses.  The loader fixes up the
 * symbol entries at load-time; init_arm_tcr_sptab() then copies the
 * whole block into each thread's tcr->sptab.  Calling a subprim is
 * then
 *
 *      (ldr x0 (:@ rcontext (:$ (+ tcr.sptab (ash n word-shift)))))
 *      (blr x0)
 *
 * Slots not yet implemented hold 0.  The entries follow the 32-bit
 * ARM order, omitting a few subprims that aren't useful on arm64,
 * but the order doesn't really matter.
 *
 */
        .global C(sptab)
        .global C(sptab_end)
        .section RELRO
        .p2align 3
C(sptab):
        .quad 0 // 0 SPbuiltin_plus
        .quad 0 // 1 SPbuiltin_minus
        .quad 0 // 2 SPbuiltin_times
        .quad 0 // 3 SPbuiltin_div
        .quad 0 // 4 SPbuiltin_eq
        .quad 0 // 5 SPbuiltin_ne
        .quad 0 // 6 SPbuiltin_gt
        .quad 0 // 7 SPbuiltin_ge
        .quad 0 // 8 SPbuiltin_lt
        .quad 0 // 9 SPbuiltin_le
        .quad 0 // 10 SPbuiltin_eql
        .quad 0 // 11 SPbuiltin_length
        .quad 0 // 12 SPbuiltin_seqtype
        .quad 0 // 13 SPbuiltin_assq
        .quad 0 // 14 SPbuiltin_memq
        .quad 0 // 15 SPbuiltin_logbitp
        .quad 0 // 16 SPbuiltin_logior
        .quad 0 // 17 SPbuiltin_logand
        .quad 0 // 18 SPbuiltin_ash
        .quad 0 // 19 SPbuiltin_negate
        .quad 0 // 20 SPbuiltin_logxor
        .quad 0 // 21 SPbuiltin_aref1
        .quad 0 // 22 SPbuiltin_aset1
        .quad 0 // 23 SPfuncall
        .quad 0 // 24 SPmkcatch1v
        .quad 0 // 25 SPmkcatchmv
        .quad 0 // 26 SPmkunwind
        .quad 0 // 27 SPbind
        .quad 0 // 28 SPconslist
        .quad 0 // 29 SPconslist_star
        .quad 0 // 30 SPmakes32
        .quad 0 // 31 SPmakeu32
        .quad _SPfix_overflow // 32
        .quad _SPmakeu64 // 33
        .quad _SPmakes64 // 34
        .quad 0 // 35 SPmvpass
        .quad 0 // 36 SPvalues
        .quad 0 // 37 SPnvalret
        .quad 0 // 38 SPthrow
        .quad 0 // 39 SPnthrowvalues
        .quad 0 // 40 SPnthrow1value
        .quad 0 // 41 SPbind_self
        .quad 0 // 42 SPbind_nil
        .quad 0 // 43 SPbind_self_boundp_check
        .quad 0 // 44 SPrplaca
        .quad 0 // 45 SPrplacd
        .quad 0 // 46 SPgvset
        .quad 0 // 47 SPset_hash_key
        .quad 0 // 48 SPstore_node_conditional
        .quad 0 // 49 SPset_hash_key_conditional
        .quad 0 // 50 SPstkconslist
        .quad 0 // 51 SPstkconslist_star
        .quad 0 // 52 SPmkstackv
        .quad 0 // 53 SPsetqsym
        .quad 0 // 54 SPprogvsave
        .quad 0 // 55 SPstack_misc_alloc
        .quad 0 // 56 SPgvector
        .quad 0 // 57 SPfitvals
        .quad 0 // 58 SPnthvalue
        .quad 0 // 59 SPdefault_optional_args
        .quad 0 // 60 SPopt_supplied_p
        .quad 0 // 61 SPheap_rest_arg
        .quad 0 // 62 SPreq_heap_rest_arg
        .quad 0 // 63 SPheap_cons_rest_arg
        .quad 0 // 64 SPcheck_fpu_exception
        .quad 0 // 65 SPdiscard_stack_object
        .quad 0 // 66 SPksignalerr
        .quad 0 // 67 SPstack_rest_arg
        .quad 0 // 68 SPreq_stack_rest_arg
        .quad 0 // 69 SPstack_cons_rest_arg
        .quad 0 // 70 SPcall_closure
        .quad 0 // 71 SPspreadargz
        .quad 0 // 72 SPtfuncallgen
        .quad 0 // 73 SPtfuncallslide
        .quad 0 // 74 SPjmpsym
        .quad 0 // 75 SPtcallsymgen
        .quad 0 // 76 SPtcallsymslide
        .quad 0 // 77 SPtcallnfngen
        .quad 0 // 78 SPtcallnfnslide
        .quad _SPmisc_ref // 79
        .quad 0 // 80 SPsubtag_misc_ref
        .quad 0 // 81 SPmakestackblock
        .quad 0 // 82 SPmakestackblock0
        .quad 0 // 83 SPmakestacklist
        .quad 0 // 84 SPstkgvector
        .quad 0 // 85 SPmisc_alloc
        .quad 0 // 86 SPatomic_incf_node
        .quad 0 // 87 SPrecover_values
        .quad 0 // 88 SPinteger_sign
        .quad 0 // 89 SPsubtag_misc_set
        .quad 0 // 90 SPmisc_set
        .quad 0 // 91 SPspread_lexprz
        .quad 0 // 92 SPreset
        .quad 0 // 93 SPmvslide
        .quad 0 // 94 SPsave_values
        .quad 0 // 95 SPadd_values
        .quad 0 // 96 SPmisc_alloc_init
        .quad 0 // 97 SPstack_misc_alloc_init
        .quad 0 // 98 SPpopj
        .quad 0 // 99 SPgetu64
        .quad 0 // 100 SPgets64
        .quad 0 // 101 SPspecref
        .quad 0 // 102 SPspecrefcheck
        .quad 0 // 103 SPspecset
        .quad 0 // 104 SPgets32
        .quad 0 // 105 SPgetu32
        .quad 0 // 106 SPmvpasssym
        .quad 0 // 107 SPunbind
        .quad 0 // 108 SPunbind_n
        .quad 0 // 109 SPunbind_to
        .quad 0 // 110 SPprogvrestore
        .quad 0 // 111 SPbind_interrupt_level_0
        .quad 0 // 112 SPbind_interrupt_level_m1
        .quad 0 // 113 SPbind_interrupt_level
        .quad 0 // 114 SPunbind_interrupt_level
        .quad 0 // 115 SParef2
        .quad 0 // 116 SParef3
        .quad 0 // 117 SPaset2
        .quad 0 // 118 SPaset3
        .quad 0 // 119 SPkeyword_bind
        .quad 0 // 120 SPffcall
        .quad 0 // 121 SPdebind
        .quad 0 // 122 SPcallback
C(sptab_end):
