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
spentry misc_ref
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
endsp misc_ref

C(misc_ref_common):
        ret
