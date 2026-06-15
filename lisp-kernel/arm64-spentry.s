/* SPDX-License-Identifier: Apache-2.0 */

#include "arm64-constants.h"
#include "arm64-macros.s"

/*
 * The fixnum in arg_z {over,under}flowed by one bit as the result
 * of an addition or subtraction.  Make a bignum out of it.
 */
spentry fix_overflow
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
        str imm0, [arg_z, misc_data_offset]
        ret
endsp makes64
