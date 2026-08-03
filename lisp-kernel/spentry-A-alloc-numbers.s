/* SPDX-License-Identifier: Apache-2.0 */

#include "arm64-constants.h"
#include "arm64-macros.s"   /* pulls arm64-uuo.s @ 115b7aa */
#include "arm64-globals-proposed.s"

/*
 * Cluster A: alloc-numbers subprims
 * Ported from vendor/ccl/lisp-kernel/ppc-spentry.s (PPC64 branch) to
 * Matt Emerson's upstream ARM64 low-tag design.
 *
 * 20 subprims (per CLAUDE.md upstream-port task): stack_misc_alloc,
 * makestackblock, makestacklist, misc_alloc, misc_alloc_init, integer_sign,
 * builtin_div, getu64, gets64, makeu64, makeu128, makes128, specref,
 * specrefcheck, makes32, makeu32, gets32, getu32, stack_misc_alloc_init,
 * makestackblock0.
 *
 * makes64/fix_overflow are Matt's OWN already-real examples in his
 * arm64-spentry.s (not owned by this cluster).  gets32/getu32 exist in our
 * own high-tag arm64-spentry.s; makes32/makeu32 do not (derived from PPC64
 * only).
 *
 * PORT-NOTE: the cross-cutting infrastructure this file's error paths and
 * tail-jumps depend on is now PROPOSED (ratify with Matt) and shared with
 * the sibling clusters:
 * (a) NRS/global-relative addressing: arm64-globals-proposed.s
 *     (rnil-relative ref_nrs_symbol/ref_nrs_value, indices from the vendor
 *     lisp_globals.s nrs order) -- same idiom as spentry-C/D.
 * (b) error/trap signalling: the `udf #imm16' UUO scheme (canonical:
 *     arm64-asm.lisp:435-450; namespace doc = PROPOSED-CONSTANTS block
 *     below, BINDING for all clusters) plus the _SPksignalerr subprim
 *     (spentry-D-call-builtins.s:488, real body).
 */

/* PROPOSED-CONSTANTS (ratify with Matt) -- derived from PPC64 struct/header
 * definitions using the SAME _struct/_structf macro conventions already
 * present in arm64-constants.h; NOT invented.  Cited per-field below. */

/* tsp_frame: ppc-constants64.s:228-233 (backlink, type, then fixed_overhead
 * and data_offset alias the SAME offset -- Matt's _struct macro block in
 * arm64-constants.h has no _struct_label primitive, so these are plain .set
 * equates rather than routed through the struct-generator macros). */
.set tsp_frame.backlink, 0
.set tsp_frame.type, 8
.set tsp_frame.fixed_overhead, 16
.set tsp_frame.data_offset, 16
.set tsp_frame.size, 16
/* ppc-constants.s:171 "(UNSIGNED-BYTE 16), one less than TSTACK_SOFTPROT" */
.set tstack_alloc_limit, 0xffff

/* lisp_frame: Matt's ARM-family MARKER frame, NOT PPC's backlink frame.
 * Ground truth: his popj vinsn (compiler/ARM64/arm64-vinsns.lisp:61-67)
 * does ldp fn,lr,[sp,#16] / ldr vsp,[sp,#8] ("ignore marker") / add sp,#32,
 * and arm64-constants.h:177-178 defines subtag_lisp_frame_marker.  Layout
 * matches ARM32 (arm-constants.s:374-379): marker,savevsp,savefn,savelr. */
.set lisp_frame.marker, 0
.set lisp_frame.savevsp, 8
.set lisp_frame.savefn, 16
.set lisp_frame.savelr, 24
.set lisp_frame.size, 32

/* symbol: field order ppc-constants64.s:237-245, but biased by
 * -fulltag_symbol: Matt's design gives symbols their OWN pointer tag
 * (arm64-constants.h:90 fulltag_symbol=0b0111; arm64-arch.lisp:196
 * misc-symbol-offset = node_size - fulltag_symbol), so slot n of a
 * symbol-tagged pointer is at (n+1)*node_size - fulltag_symbol.
 * (Was wrongly -misc_bias=-4; caught in the D-repair sibling sweep.
 * These odd offsets are the ledger's "symbol.vcell=9" item.) */
.set symbol.header, (0*node_size - fulltag_symbol)
.set symbol.pname, (1*node_size - fulltag_symbol)
.set symbol.vcell, (2*node_size - fulltag_symbol)
.set symbol.fcell, (3*node_size - fulltag_symbol)
.set symbol.package_predicate, (4*node_size - fulltag_symbol)
.set symbol.flags, (5*node_size - fulltag_symbol)
.set symbol.plist, (6*node_size - fulltag_symbol)
.set symbol.binding_index, (7*node_size - fulltag_symbol)
.set symbol.size, 64

/* _function: slot order ppc-constants64.s:223-226 (codevector = slot 0),
 * biased by -fulltag_misc: a function is an ordinary miscobj
 * (fulltag_function removed, patch 0055; codevector offset -7 -> -4). */
.set _function.header, misc_header_offset
.set _function.codevector, misc_data_offset
.set _function.size, 16

/* bignum headers not already in arm64-constants.h (two/three/four_digit_
 * bignum_header ARE already defined there).  Derived via the same
 * def_header(name,count,subtag) formula as ppc-constants.s:330-333/
 * ppc-constants32.s:388, using macptr.element_count/subtag_bignum which
 * arm64-constants.h already defines for real. */
.set one_digit_bignum_header, (1 << num_subtag_bits) | subtag_bignum
.set five_digit_bignum_header, (5 << num_subtag_bits) | subtag_bignum
.set macptr_header, (macptr.element_count << num_subtag_bits) | subtag_macptr

/* %builtin-functions% vector index (ppc-constants.s:131; arch-independent
 * lisp-level index, same table spentry-D's jump_builtin uses). */
.set _builtin_div, 3

/* Lisp error selector for _SPksignalerr (errors.s:195 deferr(XARRLIMIT,77):
 * deferr makes a BOXED fixnum, same convention as spentry-C:66-75). */
.set XARRLIMIT, (77 << fixnumshift)

/* UUO / trap encodings: Matt's OWN lisp-kernel/arm64-uuo.s @ 115b7aa
 * (2e10ffb "Sketch out a revised way to encode and write UUOs"),
 * included above -- 2-bit format in the udf imm16 (0 reserved: udf #0 is
 * the code-vector start sentinel; 1 unary reg+info; 2 binary ra+rb+info;
 * 3 misc).  His macros are invoked directly below with register NAMES
 * (they encode via the R* numbers in arm64-constants.h).
 * PROPOSED on top (arm64-globals-proposed.s): uuo_interr (misc bit-13
 * flag) for PPC's errnum-carrying traps.
 * xtype VALUES: his arm64-uuo.s (included above) defines the full set
 * (xtype_integer = 0x18 ... xtype_cons = 0xe8, "Keep these in sync with
 * the values in arm64-arch.lisp"); *arm64-xtype-specifiers*
 * (arm64-trap-support.lisp:215) decodes that numbering and the
 * expected-type field is 8 bits wide.  16m41: ARM32-numbered local
 * .sets here (integer=4, s64=8, ...) SHADOWED his values, so unboxing
 * traps reported wrong expected types. */

/* Construct a lisp integer out of the 32-bit signed value in imm0.
 * ported from ppc-spentry.s:6762-6774 (PPC64 branch only: box_fixnum and
 * return -- a (signed-byte 32) always fits in Matt's 61-bit-magnitude
 * fixnum, so the PPC32-only bignum-overflow path in the same subprim is not
 * reachable on a 64-bit target and is not ported). */
spentry makes32
        lsl     arg_z, imm0, #fixnumshift
        ret
endsp makes32

/* Construct a lisp integer out of the 32-bit unsigned value in imm0.
 * ported from ppc-spentry.s:6780-6783 (PPC64 branch): an (unsigned-byte 32)
 * also always fits Matt's 61-bit-magnitude fixnum. */
spentry makeu32
        lsl     arg_z, imm0, #fixnumshift
        ret
endsp makeu32

/* Construct a lisp integer out of the unsigned 128-bit value in imm0 (high
 * 64 bits) : imm1 (low 64 bits) -- PPC64's "imm0:imm1" register-pair
 * comment convention, high-part-first.
 * ported from ppc-spentry.s:6621-6660 (PPC64 branch), logic re-derived from
 * first principles (minimal-digit unsigned-bignum normalization: keep
 * digits up through the highest nonzero one; append one more zero digit iff
 * that digit's own top bit is set) rather than transliterating PPC's
 * per-32-bit-half CR-probe sequence, which relies on POWER's multiple
 * parallel condition-register fields with no 1:1 AArch64 (single NZCV)
 * analog.  rotldi byte-swaps dropped throughout: AArch64 is little-endian,
 * so a direct 64-bit store already places digit pairs in the correct
 * byte order with no swap needed (PPC64/CCL historically ran big-endian). */
spentry makeu128
        cbz     imm0, 6f                       /* whole value fits in imm1 alone */
        /* GC SAFETY (Matt, 2026-07-11 mail): scratch must be an IMM reg,
         * never a node reg like temp0 -- an unboxed value there has an
         * arbitrary tag the GC could see. */
        lsr     imm3, imm0, #32
        cbnz    imm3, 2f                       /* digit3 (imm0's high half) != 0 */
        /* digit3 == 0: 3 or 4 digits, decided by imm0's bit31 (== digit2's
         * own sign bit, since digit3==0 means imm0 < 2^32). */
        lsr     imm3, imm0, #31
        cbnz    imm3, 1f
        mov     imm2, #three_digit_bignum_header
        Misc_Alloc_Fixed arg_z, imm2, aligned_bignum_size(3)
        str     imm1, [arg_z, #misc_data_offset]
        str     w0,   [arg_z, #(misc_data_offset + 8)]   /* w0 = low 32 bits of imm0 */
        ret
1:      mov     imm2, #four_digit_bignum_header
        Misc_Alloc_Fixed arg_z, imm2, aligned_bignum_size(4)
        str     imm1, [arg_z, #misc_data_offset]
        str     imm0, [arg_z, #(misc_data_offset + 8)]
        ret
2:      /* digit3 != 0: 4 or 5 digits, decided by imm0's sign (bit63). */
        cmp     imm0, #0
        b.ge    1b
        mov     imm2, #five_digit_bignum_header
        Misc_Alloc_Fixed arg_z, imm2, aligned_bignum_size(5)
        str     imm1, [arg_z, #misc_data_offset]
        str     imm0, [arg_z, #(misc_data_offset + 8)]
        str     xzr,  [arg_z, #(misc_data_offset + 16)]
        ret
6:      mov     imm0, imm1
        b       _SPmakeu64
endsp makeu128

/* Construct a lisp integer out of the signed 128-bit value in imm0 (high 64
 * bits) : imm1 (low 64 bits).
 * ported from ppc-spentry.s:6667-6693 (PPC64 branch).  Unlike makeu128, a
 * signed value never needs a 5th padding digit: a kept top digit's own
 * bit31 already correctly encodes the sign, since the whole point of
 * dropping a would-be digit is only valid when it's pure sign-extension of
 * the one below -- which is exactly the "imm0 == sign_extend(imm1)" /
 * "imm0 fits signed32" tests below.  rotldi byte-swaps dropped (see
 * makeu128 comment -- little-endian AArch64 needs none). */
spentry makes128
        asr     imm2, imm1, #63
        cmp     imm2, imm0
        b.eq    2f
        sxtw    imm3, w0                        /* sign-extend low 32 bits of imm0 */
        cmp     imm3, imm0
        b.eq    1f
        mov     imm2, #four_digit_bignum_header
        Misc_Alloc_Fixed arg_z, imm2, aligned_bignum_size(4)
        str     imm1, [arg_z, #misc_data_offset]
        str     imm0, [arg_z, #(misc_data_offset + 8)]
        ret
1:      mov     imm2, #three_digit_bignum_header
        Misc_Alloc_Fixed arg_z, imm2, aligned_bignum_size(3)
        str     imm1, [arg_z, #misc_data_offset]
        str     w3,   [arg_z, #(misc_data_offset + 8)]
        ret
2:      mov     imm0, imm1
        b       _SPmakes64
endsp makes128

/* makeu64: Matt's own implementation landed upstream @ 115b7aa
 * (arm64-spentry.s spentry makeu64 -- GC-safe, imm regs only); ours is
 * CEDED to his and removed (it also had the temp0 GC-safety bug he
 * flagged in the 2026-07-11 mail). */

/* arg_z should be (unsigned-byte 64); return unboxed value in imm0.
 * ported from ppc-spentry.s:6437-6462 (PPC64 branch).  A 2-digit bignum's
 * combined 64-bit value (digits stored little-endian, direct 64-bit load,
 * no PPC-style rotldi swap needed) must be nonneg; a 3-digit bignum's 3rd
 * (most-significant) 32-bit digit must be exactly 0. */
spentry getu64
        and     imm1, arg_z, #tagmask
        cmp     imm1, #tag_fixnum
        b.ne    1f
        asr     imm0, arg_z, #fixnumshift
        cmp     arg_z, #0
        b.ge    8f
        b       9f
1:      and     imm1, arg_z, #fulltagmask
        cmp     imm1, #fulltag_misc
        b.ne    9f
        ldrb    w1, [arg_z, #misc_subtag_offset]  /* ldrb needs a W reg (imm1=x1) */
        cmp     imm1, #subtag_bignum
        b.ne    9f
        ldr     imm1, [arg_z, #misc_header_offset]
        ldr     imm0, [arg_z, #misc_data_offset]
        cmp     imm1, #two_digit_bignum_header
        b.eq    2f
        cmp     imm1, #three_digit_bignum_header
        b.ne    9f
        ldr     w2, [arg_z, #(misc_data_offset + 8)]   /* 3rd digit must be 0 */
        cbnz    w2, 9f
        b       8f
2:      cmp     imm0, #0
        b.lt    9f
8:      ret
9:      /* ppc-spentry.s:6446 uuo_interr(error_object_not_u64,arg_z) -> the
         * udf xtype trap (fmt 4; same xtype our own tree uses here). */
        uuo_error_reg_not_xtype arg_z, xtype_u64
endsp getu64

/* arg_z should be (signed-byte 64); return unboxed value in imm0.
 * ported from ppc-spentry.s:6502-6514 (PPC64 branch).  A 2-digit bignum's
 * combined 64-bit value IS the signed result directly (any bit pattern is
 * valid for signed-64, no extra range check needed, unlike getu64). */
spentry gets64
        and     imm1, arg_z, #tagmask
        cmp     imm1, #tag_fixnum
        asr     imm0, arg_z, #fixnumshift
        b.eq    8f
        and     imm2, arg_z, #fulltagmask
        cmp     imm2, #fulltag_misc
        b.ne    9f
        ldrb    w2, [arg_z, #misc_subtag_offset]  /* ldrb needs a W reg (imm2=x2) */
        cmp     imm2, #subtag_bignum
        b.ne    9f
        ldr     imm1, [arg_z, #misc_header_offset]
        ldr     imm0, [arg_z, #misc_data_offset]
        cmp     imm1, #two_digit_bignum_header
        b.ne    9f
8:      ret
9:      /* ppc-spentry.s:6532 uuo_interr(error_object_not_s64,arg_z) */
        uuo_error_reg_not_xtype arg_z, xtype_s64
endsp gets64

/* arg_z should be (signed-byte 32); return unboxed value in imm0.
 * ported from ppc-spentry.s:6804-6813 (PPC64 branch), reimplemented per the
 * function's stated contract ("return unboxed result in imm0") rather than
 * transliterated literally: the vendored PPC64 branch's last step reads
 * `box_fixnum(imm0,arg_z)`, which would leave imm0 BOXED, contradicting the
 * subprim's own header comment and every other get* subprim's convention --
 * almost certainly a `box_fixnum`/`unbox_fixnum` transcription slip in the
 * vendored source (flag for a future PPC64-diff audit).  Implemented here as
 * the evident intent: arg_z is (signed-byte 32) iff unboxing it round-trips
 * through a 32-bit sign-extend. */
spentry gets32
        and     imm0, arg_z, #tagmask
        asr     imm1, arg_z, #fixnumshift
        cmp     imm0, #tag_fixnum
        b.ne    9f
        sxtw    imm2, w1
        cmp     imm2, imm1
        b.ne    9f
        mov     imm0, imm1
        ret
9:      /* ppc-spentry.s:6827 uuo_interr(error_object_not_signed_byte_32,
         * arg_z) */
        uuo_error_reg_not_xtype arg_z, xtype_s32
endsp gets32

/* arg_z should be (unsigned-byte 32); return unboxed value in imm0.
 * ported from ppc-spentry.s:6833-6857.  Reimplemented digit-wise (a plain
 * 32-bit LDR zero-extends into the 64-bit register, giving exactly the
 * unsigned digit magnitude with no sign ambiguity) rather than PPC64's
 * paired 64-bit vrefr reads, which fold digit-pair endianness assumptions
 * that don't apply on little-endian AArch64. one_digit_bignum_header is a
 * PROPOSED constant above (ppc-constants32.s:388 -- 64-bit targets normally
 * never need it since a lone 32-bit digit always fits a fixnum, but getu32
 * itself is not #ifdef(PPC64)-split in the vendored source, so it defensively
 * accepts one). */
spentry getu32
        and     imm1, arg_z, #tagmask
        asr     imm0, arg_z, #fixnumshift
        cmp     imm1, #tag_fixnum
        b.ne    1f
        cmp     imm0, #0
        b.lt    9f
        lsr     imm2, imm0, #32
        cbnz    imm2, 9f
        ret
1:      and     imm1, arg_z, #fulltagmask
        cmp     imm1, #fulltag_misc
        b.ne    9f
        ldrb    w1, [arg_z, #misc_subtag_offset]  /* ldrb needs a W reg (imm1=x1) */
        cmp     imm1, #subtag_bignum
        b.ne    9f
        ldr     imm1, [arg_z, #misc_header_offset]
        ldr     w0, [arg_z, #misc_data_offset]        /* digit0, zero-extended */
        cmp     imm1, #one_digit_bignum_header
        b.eq    8f
        cmp     imm1, #two_digit_bignum_header
        b.ne    9f
        ldr     w2, [arg_z, #(misc_data_offset + 4)]  /* digit1 must be 0 */
        cbnz    w2, 9f
8:      ret
9:      /* ppc-spentry.s:6857 uuo_interr(error_object_not_unsigned_byte_32,
         * arg_z) */
        uuo_error_reg_not_xtype arg_z, xtype_u32
endsp getu32

/* On entry arg_z = symbol.  On exit arg_z = value (possibly unbound_marker),
 * arg_y = symbol, imm3 = symbol.binding_index.
 * ported from ppc-spentry.s:6697-6708 (direct, unmodified logic port --
 * symbol.binding_index stores an already byte-scaled offset into the TCR's
 * thread-local binding area, indexed here with a raw non-scaled add, exactly
 * as PPC64's ldrx/cmpr do). */
spentry specref
        ldr     imm3, [arg_z, #symbol.binding_index]
        ldr     imm0, [rcontext, #tcr.tlb_limit]
        cmp     imm3, imm0
        ldr     imm2, [rcontext, #tcr.tlb_pointer]
        mov     arg_y, arg_z
        b.ge    1f
        ldr     arg_z, [imm2, imm3]
        cmp     arg_z, #no_thread_local_binding_marker
        b.ne    9f
1:      ldr     arg_z, [arg_y, #symbol.vcell]
9:      ret
endsp specref

/* As specref, but traps if the resulting value is unbound_marker.
 * ported from ppc-spentry.s:6711-6723. */
spentry specrefcheck
        ldr     imm3, [arg_z, #symbol.binding_index]
        ldr     imm0, [rcontext, #tcr.tlb_limit]
        cmp     imm3, imm0
        ldr     imm2, [rcontext, #tcr.tlb_pointer]
        mov     arg_y, arg_z
        b.ge    1f
        ldr     arg_z, [imm2, imm3]
        cmp     arg_z, #no_thread_local_binding_marker
        b.ne    2f
1:      ldr     arg_z, [arg_y, #symbol.vcell]
2:      cmp     arg_z, #unbound_marker
        b.ne    9f
        /* ppc-spentry.s:6722 treqi(arg_z,unbound_marker): the equality is
         * already established by the b.ne above, so trap unconditionally.
         * The uuo register operand is the SYMBOL (handler contract,
         * arm64-exceptions.c uuo_unary_unbound: gpr = symbol) — here the
         * symbol is in arg_y; arg_z holds the loaded unbound_marker.
         * (spentry-C's binding path passes arg_z because THERE the value
         * went to temp0 and arg_z still holds the symbol.) */
        uuo_error_unbound arg_y         /* macro fixed upstream @ c9e7ffb */
9:      ret
endsp specrefcheck

/* arg_z is a fixnum or bignum.  Returns (in imm0): arg_z unchanged if a
 * fixnum; else +1/-1 per the sign of the bignum's most-significant 32-bit
 * digit (this is an internal digit-sign probe, not CL SIGNUM).
 * ported from ppc-spentry.s:3882-3904. */
spentry integer_sign
        and     imm1, arg_z, #fulltagmask
        and     imm0, arg_z, #tagmask
        cmp     imm1, #fulltag_misc
        b.ne    1f
        ldrb    w0, [arg_z, #misc_subtag_offset]  /* ldrb needs a W reg (imm0=x0) */
1:      cmp     imm0, #tag_fixnum
        b.ne    2f
        mov     imm0, arg_z
        ret
2:      cmp     imm0, #subtag_bignum
        b.eq    3f
        /* ppc-spentry.s:3904 uuo_interr(error_object_not_integer,arg_z) */
        uuo_error_reg_not_xtype arg_z, xtype_integer
3:      ldr     imm0, [arg_z, #misc_header_offset]
        lsr     imm0, imm0, #num_subtag_bits      /* raw bigit (32-bit digit) count */
        lsl     imm0, imm0, #2                     /* * bigit_size(4) -> byte offset past last bigit */
        add     imm0, imm0, #(misc_data_offset - 4)
        ldr     w1, [arg_z, imm0]                  /* w1 = last (most-significant) 32-bit bigit */
        cmp     w1, #0
        mov     imm0, #1
        b.ge    9f
        mov     imm0, #-1
9:      ret
endsp integer_sign

/* No inline fixnum fast path -- division always dispatches to the Lisp
 * builtin.  ported from ppc-spentry.s:5578-5579: jump_builtin(_builtin_div,2)
 * expands to ref_nrs_value(fname,builtin_functions); set_nargs(2);
 * vrefr(fname,fname,_builtin_div); jump_fname().  Same expansion as
 * spentry-D-call-builtins.s's jump_builtin macro (D:122-130). */
spentry builtin_div
        ref_nrs_value fname, builtin_functions  /* ppc-spentry.s:38 */
        mov     nargs, #(2 << fixnumshift)      /* ppc-spentry.s:39 set_nargs(2) */
        ldr     fname, [fname, #(misc_data_offset + _builtin_div * node_size)] /* ppc:40 vrefr */
        ldr     nfn, [fname, #symbol.fcell]     /* ppc:41 jump_fname            */
        ldr     temp0, [nfn, #_function.codevector]
        br      temp0
endsp builtin_div

/* Allocate a "fulltag_misc" object.  arg_y = element count (boxed fixnum,
 * unsigned), arg_z = subtag (boxed fixnum whose raw value is the subtag
 * byte).  On exit arg_z = the tagged object (header set, contents zero);
 * imm0 = the header word used.
 * ported from ppc-spentry.s:3438-3480 (PPC64 branch).  Matt's ARM64 shares
 * PPC64's fixnumshift=3 == log2(node_size=8) identity, so "arg_y tagged"
 * already equals raw_count*node_size for the node/64-bit-per-element
 * classes with no extra shift needed -- the same trick PPC64 exploits.
 * AArch64's single NZCV flags register (vs. POWER's several parallel CR
 * fields) means the parallel cr1..cr5 compares are serialized below into an
 * if/elif cascade; the VALUES/arithmetic are unchanged. */
spentry misc_alloc
        /* GC SAFETY: imm scratch, not temp0 (see makeu128 note). */
        lsr     imm4, arg_y, #59                  /* bounds: raw count fits unsigned-byte-56 */
        cbnz    imm4, 9f
        asr     imm0, arg_z, #fixnumshift          /* imm0 = raw subtag byte */
        lsl     imm2, arg_y, #(num_subtag_bits - fixnumshift)
        orr     imm0, imm2, imm0                   /* imm0 = header word */
        and     imm2, imm0, #fulltagmask
        and     imm3, imm0, #subtagmask            /* imm3 = subtag byte (count bits live at 8+, so compare the MASKED subtag, matching PPC's cmp on the boxed arg_z) */
        mov     imm1, arg_y
        cmp     imm2, #fulltag_nodeheader_0
        b.eq    1f
        cmp     imm2, #fulltag_nodeheader_1
        b.eq    1f
        cmp     imm2, #ivector_class_64_bit
        b.eq    1f
        cmp     imm3, #subtag_complex_double_float_vector
        b.eq    3f
        lsr     imm1, imm1, #1
        cmp     imm2, #ivector_class_32_bit
        b.eq    1f
        cmp     imm3, #subtag_bit_vector
        b.eq    2f
        lsr     imm1, imm1, #1
        /* Matt's scheme has no ivector_class_8_bit (only 64/32/other,
         * arm64-constants.h:112-114); within class other_bit the 8-bit
         * subtags are s8/u8 >= subtag_s8_vector, below that s16/u16
         * (x86-spentry64.s:2977-2981 misc_alloc, the same tag scheme). */
        cmp     imm3, #subtag_s8_vector
        b.lt    1f                                 /* 16-bit: n*2 stands */
        lsr     imm1, imm1, #1                     /* 8-bit: n*1 */
1:      add     imm1, imm1, #(node_size + dnode_size - 1)   /* dnode_align(imm1,imm1,node_size) */
        and     imm1, imm1, #0xfffffffffffffff0
        Misc_Alloc arg_z, imm0, imm1
        ret
2:      add     imm1, arg_y, #(7 << fixnumshift)
        lsr     imm1, imm1, #(3 + fixnumshift)
        b       1b
3:      add     imm1, arg_y, arg_y
        b       1b
9:      /* ppc-spentry.s:3477-3480: li arg_x,XARRLIMIT; set_nargs(3);
         * b _SPksignalerr (real subprim: spentry-D-call-builtins.s:488). */
        mov     arg_x, #XARRLIMIT               /* ppc:3478 li arg_x,XARRLIMIT  */
        mov     nargs, #(3 << fixnumshift)      /* ppc:3479 set_nargs(3)        */
        b       _SPksignalerr                   /* ppc:3480                     */
endsp misc_alloc

/* Allocate a uvector on the tstack (push a tsp frame and heap-cons the
 * object via misc_alloc if there's no room).  Same (arg_y=count,
 * arg_z=subtag) convention and byte-count arithmetic as misc_alloc above.
 * ported from ppc-spentry.s:1025-1069 (PPC64 branch): dnode_align + a
 * tstack_alloc_limit check, then TSP_Alloc_Var_Boxed_nz (real tsp=x24
 * register, PPC discipline) with heap-cons fallback via TSP_Alloc_Fixed_
 * Unboxed(0) + tail to misc_alloc. */
spentry stack_misc_alloc
        asr     imm0, arg_z, #fixnumshift
        lsl     imm2, arg_y, #(num_subtag_bits - fixnumshift)
        orr     imm0, imm2, imm0
        and     imm2, imm0, #fulltagmask
        and     imm3, imm0, #subtagmask            /* masked subtag byte (see misc_alloc note) */
        mov     imm1, arg_y
        cmp     imm2, #fulltag_nodeheader_0
        b.eq    1f
        cmp     imm2, #fulltag_nodeheader_1
        b.eq    1f
        cmp     imm2, #ivector_class_64_bit
        b.eq    1f
        cmp     imm3, #subtag_complex_double_float_vector
        b.eq    6f
        lsr     imm1, imm1, #1
        cmp     imm2, #ivector_class_32_bit
        b.eq    1f
        cmp     imm3, #subtag_bit_vector
        b.eq    5f
        lsr     imm1, imm1, #1
        cmp     imm3, #subtag_s8_vector            /* no 8_bit class: 8-bit subtags are >= s8 within class other (see misc_alloc note) */
        b.lt    1f
        lsr     imm1, imm1, #1
1:      /* imm1 = byte count; dnode_align(imm1,imm1,tsp_frame.fixed_overhead
         * +node_size) -- the total tsp allocation (frame header + object
         * header + data), ppc-spentry.s:1058. */
        add     imm1, imm1, #(tsp_frame.fixed_overhead + node_size + (dnode_size - 1))
        and     imm1, imm1, #0xfffffffffffffff0
        mov     imm3, #tstack_alloc_limit
        cmp     imm1, imm3
        b.ge    9f
        /* TSP_Alloc_Var_Boxed_nz(imm1): push a new tsp frame of size imm1,
         * zero its data area, mark it boxed (type=0).  "_nz": imm1 always
         * includes the fixed frame overhead, so the frame can never be
         * empty -- ppc-macros.s:695-704,721-725. */
        mov     temp4, tsp                        /* old tsp -> backlink */
        sub     tsp, tsp, imm1
        str     temp4, [tsp, #tsp_frame.backlink]
        mov     temp0, tsp
        add     temp1, tsp, imm1
        sub     temp1, temp1, #8
7:      str     xzr, [temp0, #8]!
        cmp     temp0, temp1
        b.ne    7b
        str     xzr, [tsp, #tsp_frame.type]
        str     imm0, [tsp, #tsp_frame.data_offset]
        add     arg_z, tsp, #(tsp_frame.data_offset + fulltag_misc)
        ret
5:      /* bit-vector: byte_count = (arg_y + 7<<fixnumshift) >> (3+fixnumshift) */
        add     imm1, arg_y, #(7 << fixnumshift)
        lsr     imm1, imm1, #(3 + fixnumshift)
        b       1b
6:      /* complex-double-float-vector: byte_count = arg_y + arg_y */
        add     imm1, arg_y, arg_y
        b       1b
9:      /* Too large for the tstack: push one empty UNBOXED tsp frame
         * (TSP_Alloc_Fixed_Unboxed(0), ppc-spentry.s:1068 -- type=self,
         * nonzero, so GC skips it) so the compiler's balancing discard-
         * temp-frame still has a frame to pop, then heap-cons via
         * misc_alloc instead; arg_y/arg_z are unchanged, matching
         * misc_alloc's own (count, subtag) calling convention. */
        mov     temp4, tsp
        sub     tsp, tsp, #tsp_frame.data_offset
        str     temp4, [tsp, #tsp_frame.backlink]
        str     tsp,   [tsp, #tsp_frame.type]
        b       _SPmisc_alloc
endsp stack_misc_alloc

/* arg_z = size in bytes (boxed fixnum).  Allocate a macptr-tagged block of
 * that size on the tstack (or heap-cons a gcable macptr via %new-gcable-ptr
 * if it won't fit).
 * ported from ppc-spentry.s:3297-3321.  macptr.address/domain/type offsets
 * come from arm64-constants.h's already-real _structf macptr; macptr_header
 * is a PROPOSED constant above. */
spentry makestackblock
        asr     imm0, arg_z, #fixnumshift
        add     imm0, imm0, #(tsp_frame.fixed_overhead + macptr.size + (dnode_size - 1))
        and     imm0, imm0, #0xfffffffffffffff0
        mov     imm1, #tstack_alloc_limit
        cmp     imm0, imm1
        b.ge    1f
        /* TSP_Alloc_Var_Unboxed(imm0): push a new tsp frame, leave it
         * "raw"/unboxed (type=self, nonzero) -- ppc-macros.s:708-712. */
        mov     temp4, tsp
        sub     tsp, tsp, imm0
        str     temp4, [tsp, #tsp_frame.backlink]
        str     tsp,   [tsp, #tsp_frame.type]
        mov     imm0, #macptr_header
        add     imm1, tsp, #(tsp_frame.data_offset + macptr.size)
        str     imm0, [tsp, #tsp_frame.data_offset]
        add     arg_z, tsp, #(tsp_frame.data_offset + fulltag_misc)
        str     imm1, [arg_z, #macptr.address]
        str     xzr,  [arg_z, #macptr.domain]
        str     xzr,  [arg_z, #macptr.type]
        ret
1:      /* Too big: push one empty unboxed tsp frame, then heap-cons via
         * %new-gcable-ptr (ppc-spentry.s:3317-3321). */
        mov     temp4, tsp
        sub     tsp, tsp, #tsp_frame.data_offset
        str     temp4, [tsp, #tsp_frame.backlink]
        str     tsp,   [tsp, #tsp_frame.type]
        mov     nargs, #(1 << fixnumshift)      /* ppc:3319 set_nargs(1)          */
        ref_nrs_symbol fname, new_gcable_ptr    /* ppc:3320 li fname,nrs.new_gcable_ptr */
        ldr     nfn, [fname, #symbol.fcell]     /* ppc:3321 jump_fname()          */
        ldr     temp0, [nfn, #_function.codevector]
        br      temp0
endsp makestackblock

/* arg_y = length (boxed fixnum), arg_z = initial-element (boxed).  Return a
 * fresh list of that length on the tstack, cell by cell (or heap-cons via
 * Cons if it won't fit).  Fully real -- no missing constants/ABI needed.
 * ported from ppc-spentry.s:3351-3388.  rnil used directly for Matt's ARM64
 * dedicated nil register in place of PPC64's `li reg,nil_value` absolute
 * load. */
spentry makestacklist
        add     imm0, arg_y, arg_y
        mov     imm3, #((tstack_alloc_limit + 1) - cons.size)
        cmp     imm0, imm3
        add     imm0, imm0, #tsp_frame.fixed_overhead
        b.ge    3f
        /* TSP_Alloc_Var_Boxed(imm0): push frame, zero its data area (may be
         * empty when arg_y=0, hence the leading compare instead of the "_nz"
         * do-while form), mark boxed -- ppc-macros.s:681-692,714-718. */
        mov     temp4, tsp
        sub     tsp, tsp, imm0
        str     temp4, [tsp, #tsp_frame.backlink]
        mov     temp0, tsp
        add     temp1, tsp, imm0
        sub     temp1, temp1, #8
1:      cmp     temp0, temp1
        b.eq    2f
        str     xzr, [temp0, #8]!
        b       1b
2:      str     xzr, [tsp, #tsp_frame.type]
        mov     imm1, arg_y                       /* count */
        cmp     imm1, #0
        mov     arg_y, arg_z                       /* initial value */
        mov     arg_z, rnil                         /* result so far */
        ldr     imm2, [tsp, #tsp_frame.backlink]
        sub     imm2, imm2, #(tsp_frame.fixed_overhead - fulltag_cons)
        b       10f
4:      sub     imm1, imm1, #(1 << fixnumshift)
        cmp     imm1, #0
        str     arg_z, [imm2, #cons.cdr]
        str     arg_y, [imm2, #cons.car]
        mov     arg_z, imm2
        sub     imm2, imm2, #cons.size
10:     b.ne    4b
        ret
3:      /* Too big for the tstack: push one empty BOXED (zeroed) tsp frame
         * (TSP_Alloc_Fixed_Boxed(0), ppc-spentry.s:3377), then heap-cons
         * cell by cell via Cons. */
        mov     temp4, tsp
        sub     tsp, tsp, #tsp_frame.data_offset
        str     temp4, [tsp, #tsp_frame.backlink]
        str     xzr,   [tsp, #tsp_frame.type]
        mov     imm1, arg_y
        mov     arg_y, arg_z
        mov     arg_z, rnil
        /* Loop test POST-Cons: Matt's Cons macro does `cmp allocptr,
         * allocbase` and CLOBBERS NZCV (the spentry-D:404 class; the
         * pre-Cons cmp here span an infinite loop, 16m5n).  Entry test
         * added -- the old code fell into 11f on the stale size-check
         * flags. */
        cmp     imm1, #0
        b       11f
6:      Cons    arg_z, arg_y, arg_z
        subs    imm1, imm1, #(1 << fixnumshift)
11:     b.ne    6b
        ret
endsp makestacklist

/* On entry: arg_x = element count, arg_y = subtag, arg_z = initial value
 * (all boxed).  Allocate via misc_alloc, then tail-call %init-misc% to fill
 * the contents with the initial value.
 * ported from ppc-spentry.s:5231-5247.  lisp_frame is a plain native-SP call
 * frame (PROPOSED-CONSTANTS above); AArch64 needs no PPC-style mflr/mtlr
 * dance since x30 (lr) is directly readable/writable, so build/discard_
 * lisp_frame collapse to a plain sub/str.../ldr.../add sp sequence. */
spentry misc_alloc_init
        /* 16m41 PARITY NOTE: this twin keeps PPC's temp0 park and is CORRECT,
         * but only because _SPmisc_alloc preserves temp0 -- registers survive
         * the allocation trap, and the initval is a node, so a GC there
         * relocates it in place.  The TSTACK twin below could not keep it:
         * _SPstack_misc_alloc uses temp0 as its frame-zeroing cursor.  If
         * misc_alloc ever grows a temp0 use, this breaks the same way. */
        sub     sp, sp, #lisp_frame.size
        mov     temp4, #lisp_frame_marker
        str     temp4, [sp, #lisp_frame.marker]
        str     vsp,   [sp, #lisp_frame.savevsp]
        str     fn,    [sp, #lisp_frame.savefn]
        str     x30,   [sp, #lisp_frame.savelr]
        mov     fn, xzr
        mov     temp0, arg_z                       /* initval */
        mov     arg_z, arg_y                        /* subtag */
        mov     arg_y, arg_x                         /* element-count */
        bl      _SPmisc_alloc
        ldr     x30, [sp, #lisp_frame.savelr]
        ldr     fn,  [sp, #lisp_frame.savefn]
        ldr     vsp, [sp, #lisp_frame.savevsp]
        add     sp, sp, #lisp_frame.size
        mov     arg_y, temp0                    /* ppc:5246 mr arg_y,temp0        */
        mov     nargs, #(2 << fixnumshift)      /* ppc:5245 set_nargs(2)          */
        ref_nrs_symbol fname, init_misc         /* ppc:5244 li fname,nrs.init_misc */
        ldr     nfn, [fname, #symbol.fcell]     /* ppc:5247 jump_fname()          */
        ldr     temp0, [nfn, #_function.codevector]
        br      temp0
endsp misc_alloc_init

/* As misc_alloc_init above, but allocates on the tstack via
 * stack_misc_alloc.  ported from ppc-spentry.s:5251-5267. */
spentry stack_misc_alloc_init
        /* 16m41 BUG FIX (regression stage 11, DYNAMIC-EXTENT.13/14): PPC parks
         * the initval in temp0 across the alloc (ppc:5266 mr arg_y,temp0) and
         * that does NOT port -- our _SPstack_misc_alloc uses temp0 as the
         * ZEROING CURSOR for the new tsp frame (`mov temp0,tsp' / `str xzr,
         * [temp0,#8]!'), so the initval came back as a raw tstack address and
         * init_misc type-errored.  ARM64-DEVIATION: park it on the VSTACK
         * instead, which no callee can clobber and which the GC scans as a node
         * (unlike a register whose safety depends on the callee's clobber set --
         * see the note on the heap twin above).  The push precedes savevsp so an
         * unwind restores a vsp that still covers the parked word. */
        str     arg_z, [vsp, #-node_size]!      /* park the initval */
        sub     sp, sp, #lisp_frame.size
        mov     temp4, #lisp_frame_marker
        str     temp4, [sp, #lisp_frame.marker]
        str     vsp,   [sp, #lisp_frame.savevsp]
        str     fn,    [sp, #lisp_frame.savefn]
        str     x30,   [sp, #lisp_frame.savelr]
        mov     fn, xzr
        mov     arg_z, arg_y                        /* subtag */
        mov     arg_y, arg_x                         /* element-count */
        bl      _SPstack_misc_alloc
        ldr     x30, [sp, #lisp_frame.savelr]
        ldr     fn,  [sp, #lisp_frame.savefn]
        ldr     vsp, [sp, #lisp_frame.savevsp]
        add     sp, sp, #lisp_frame.size
        ldr     arg_y, [vsp], #node_size        /* initval back (ppc:5266 used temp0) */
        mov     nargs, #(2 << fixnumshift)      /* ppc:5265 set_nargs(2)          */
        ref_nrs_symbol fname, init_misc         /* ppc:5264 li fname,nrs.init_misc */
        ldr     nfn, [fname, #symbol.fcell]     /* ppc:5267 jump_fname()          */
        ldr     temp0, [nfn, #_function.codevector]
        br      temp0
endsp stack_misc_alloc_init

/* As makestackblock above, but zero the block's contents.
 * arg_z = size in bytes (boxed fixnum).  Allocate a zeroed macptr-tagged
 * block on the tstack (or heap-cons a gcable macptr with clear-p=T).
 * ported from ppc-spentry.s:3324-3348 (PPC64 branch).  Parity sibling of
 * makestackblock (above, line 545); differences: (1) Zero_TSP_Frame after
 * alloc, (2) only macptr.domain zeroed explicitly (not .type -- already zero
 * from the frame-zero pass), (3) too-big path passes 2 args (size, t=clear). */
spentry makestackblock0
        asr     imm0, arg_z, #fixnumshift
        add     imm0, imm0, #(tsp_frame.fixed_overhead + macptr.size + (dnode_size - 1))
        and     imm0, imm0, #0xfffffffffffffff0
        mov     imm1, #tstack_alloc_limit
        cmp     imm0, imm1
        b.ge    makestackblock0_too_big
        /* TSP_Alloc_Var_Unboxed(imm0): push a new tsp frame, leave it
         * "raw"/unboxed (type=self, nonzero) -- ppc-macros.s:708-712. */
        mov     temp4, tsp
        sub     tsp, tsp, imm0
        str     temp4, [tsp, #tsp_frame.backlink]
        str     tsp,   [tsp, #tsp_frame.type]
        /* Zero_TSP_Frame(imm0, imm1): zero from tsp+data_offset through
         * old_tsp-8 inclusive.  ppc-macros.s:681-692. */
        add     imm0, tsp, #tsp_frame.data_offset
        sub     imm1, temp4, #node_size
        b       makestackblock0_zero_test
makestackblock0_zero_loop:
        str     xzr, [imm0], #node_size
makestackblock0_zero_test:
        cmp     imm0, imm1
        b.ls    makestackblock0_zero_loop
        /* Write macptr header + address (same as makestackblock) */
        mov     imm0, #macptr_header
        add     imm1, tsp, #(tsp_frame.data_offset + macptr.size)
        str     imm0, [tsp, #tsp_frame.data_offset]
        add     arg_z, tsp, #(tsp_frame.data_offset + fulltag_misc)
        str     imm1, [arg_z, #macptr.address]
        /* PPC64: stfd fp_zero,macptr.domain -- zeros domain only (type already
         * zero from Zero_TSP_Frame pass above). */
        str     xzr, [arg_z, #macptr.domain]
        ret
makestackblock0_too_big:
        /* Too big: push one empty unboxed tsp frame, then heap-cons via
         * %new-gcable-ptr with clear-p=T (ppc-spentry.s:3340-3347).
         * Two args: arg_y=size, arg_z=t_value (clear-p). */
        mov     temp4, tsp
        sub     tsp, tsp, #tsp_frame.data_offset
        str     temp4, [tsp, #tsp_frame.backlink]
        str     tsp,   [tsp, #tsp_frame.type]
        mov     arg_y, arg_z                    /* ppc:3343 mr arg_y,arg_z (save block size) */
        add     arg_z, rnil, #t_offset          /* ppc:3344 li arg_z,t_value (clear-p = T)   */
        mov     nargs, #(2 << fixnumshift)      /* ppc:3345 set_nargs(2)          */
        ref_nrs_symbol fname, new_gcable_ptr    /* ppc:3346 li fname,nrs.new_gcable_ptr */
        ldr     nfn, [fname, #symbol.fcell]     /* ppc:3347 jump_fname()          */
        ldr     temp0, [nfn, #_function.codevector]
        br      temp0
endsp makestackblock0
