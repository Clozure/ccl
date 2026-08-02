/* SPDX-License-Identifier: Apache-2.0 */

/* CLOZURE-WIP-FOUNDATION (source: lisp-kernel/arm64-spentry.s @ 33e61e6)
 *
 * His 5 subprim bodies (fix_overflow, makes64, makeu64, the self-described
 * misc_ref "toy", ffcall) VERBATIM, plus THE SPTAB POPULATION, which is ours.
 *
 * WHY THIS FILE MUST BE DELIVERED BYTE-FOR-BYTE (16m30, promoted by
 * patch 0114 in 16m63):
 *
 *   The sptab is the subprim dispatch table: the compiler emits every
 *   subprim call as `ldr xN,[rcontext,#(tcr.sptab + 8*index)]` + `blr xN`, so
 *   an unpopulated row is a jump to address 0.  His tip ships the table with
 *   119 of 123 rows `.quad 0` ("slots not yet implemented hold 0") -- only
 *   fix_overflow/makeu64/makes64/misc_ref are wired.  Our 128-of-132 rows are
 *   what make the port able to run at all; the bodies they point at live in
 *   our spentry-{A..E}.s.
 *
 *   That population existed ONLY as a commit on a test machine's local
 *   branch, in a work-in-progress snapshot atop 6b6540e that was never part
 *   of the patch series.  No patch reproduced it.  It has now silently
 *   evaporated TWICE: 16m22 (stamp-clear reverted it -> "cold-load wild blr
 *   via [rnil+0x358]", recovered as patch 0036) and again in 16m30, when a
 *   pin-advance sync reset this file to "pin + patches 0003/0004" — a state
 *   that md5-matched the x86 BUILD box exactly, and which therefore looked
 *   verified while carrying only 18 populated rows.  Boot died instantly at
 *   `blr` on sptab index 56.
 *
 *   The x86 box is NOT a witness for this file: it cross-compiles level-0 and
 *   never builds an ARM kernel, so its copy of arm64-spentry.s is dead weight
 *   there and drifted to the patch-series state unnoticed.  Cross-box md5
 *   equality proves synchronization, NOT correctness.
 *
 *   Patch 0114 now delivers these exact bytes through the numbered series,
 *   and the make gate no longer copies this file after patch application. Its
 *   sptab-specific need() controls catch a silent return to zero rows.
 *
 * RELATION TO THE PATCH SERIES: patches 0003 (ffcall) and 0004 (bind family)
 * each carry an arm64-spentry.s half plus an arm64-constants.h half. Patch 0114
 * is derived against the cumulative pinned series after those patches, and its
 * final file contains both changes. Their constants.h halves remain live and
 * must keep applying.
 *
 * STILL ZERO (live-verified in the booted TCR, 16m30): indices 64, 65, 86, 121.
 * Each is a `blr` to 0 the moment the compiler emits a call to it.
 *
 * ⚠️ The row COMMENTS ("// 55 SPstack_misc_alloc") are his ARM32-derived
 * numbering and are NOT the physical index — our inserted rows shifted them.
 * The physical position is what the compiler indexes.  Never read the comment
 * as the index; run tools/subprim-index-lint.py, which compares physical
 * position against the compiler's *subprims* order.
 */

#include "arm64-constants.h"
#include "arm64-macros.s"

/* fixnum 1 and the *INTERRUPT-LEVEL* tlb index (x86-constants64.s:414/867).
   The Makefile assembles each spentry-*.s as its OWN translation unit, so the
   equates and the poll macro below are per-file by construction; these are
   character-for-character the block already in spentry-C-bind-catch-throw.s:83
   and spentry-E-ffi.s:57.  arm64-constants.h defines neither (it has only
   fixnumshift/fixnummask) and it is Matt's file, not an overlay master. */
.set fixnumone, (1<<fixnumshift)
.set INTERRUPT_LEVEL_BINDING_INDEX, fixnumone

/* Poll for a deferred interrupt (ppc-macros.s check_pending_interrupt;
   clobbers nargs).  Canonical copy: spentry-E-ffi.s:218. */
.macro check_pending_interrupt
        ldr nargs, [rcontext, #tcr.tlb_pointer]
        ldr nargs, [nargs, #INTERRUPT_LEVEL_BINDING_INDEX]
        cmp nargs, #0
        b.lt 8887f                       /* interrupts disabled: done */
        b.gt 8886f                       /* level>0: an interrupt is deferred */
        ldr nargs, [rcontext, #tcr.interrupt_pending]
        cbz nargs, 8887f
8886:   uuo_interrupt_now                /* uuo_misc 4 at pin 9c61574 */
8887:
.endm

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
 * Call a foreign function.
 *
 * Entry (from the compiler's aapcs64-ff-call handler + alloc-c-frame):
 *   sp -> c_frame: backlink@0 (SP from before the allocation),
 *         savelr@8 (0), the 8 GPR argument words at c_frame.params,
 *         any stack (overflow) argument words immediately above them,
 *         FP staging words above those (dead here: d0-d7 already hold
 *         the FP args, reloaded by compiled code before we're called).
 *   arg_z = the foreign entry point: a macptr, or a fixnum-locative
 *           (an 8-aligned raw address whose bits are their own fixnum).
 *   lr    = return address in the compiled caller.
 *
 * Exit: C integer result in x0 (= imm0), FP result in d0; the c_frame
 * is popped (sp restored from its backlink); no node register holds
 * C garbage.
 *
 * Register notes: x19-x28 (save0-3, rnil, tsp, vsp, allocptr,
 * allocbase, rcontext) are AAPCS64 callee-saved, so the pinned lisp
 * registers survive the call for free.  fn (x7) is an argument
 * register: park it on the value stack (GC-visible via
 * tcr.save_vsp) before the argument load clobbers it.  We don't
 * save/restore FPCR: lisp runs with the process-default FPCR and a
 * conforming callee doesn't dirty it.  Cumulative FPSR exception
 * flags from the callee are published to tcr.foreign_fpsr.
 *
 * FPSR IS CUMULATIVE, SO THE WINDOW MUST BE OPENED AS WELL AS CLOSED.
 * Zeroing FPSR only on the way back makes tcr.foreign_fpsr carry every
 * flag raised since the PREVIOUS ff-call -- all the inline lisp float
 * arithmetic in between -- and %ffi-exception-status then charges it to
 * this callee.  Measured 16m48b: clear FPSR, `(* most-positive-single-float
 * most-positive-single-float)' inline (no FFI at all, FPSR := 0x14
 * OFC|IXC), then `(log 2.0d0)' => FLOATING-POINT-OVERFLOW on (2.0D0);
 * the immediately following identical call returns 0.693...  Every FFI
 * transcendental was affected (log/exp/sin/atan all reproduced); the ANSI
 * suite caught it as PRINT.SHORT-FLOAT.RANDOM / PRINT.SINGLE-FLOAT.RANDOM,
 * whose deftest bodies constant-fold (expt 10.0f0 100) -- an overflow in
 * SINGLE range but not in double, which is exactly why the double and long
 * variants of the same test passed.  ARM32 has no tcr.foreign_fpsr and so
 * must clear at each of its 33 lisp call sites
 * (`#+arm-target (%set-fpscr-status 0)', level-1/l1-numbers.lisp); arm64 is
 * not `arm-target', gets no such clear, and does not need one -- the slot
 * this spentry owns is the right seam.
 */
spentry ffcall
        str fn, [vsp, #-node_size]!
        /* save3 carries the frame base across the call (callee-saved);
         * its lisp value is parked next to fn. */
        str save3, [vsp, #-node_size]!
        mov save3, sp
        /* ---- THE BOUNDARY LISP FRAME (16m30; canonical note, the two
         * siblings in spentry-E-ffi.s point here) ----
         *
         * His alloc-c-frame vinsn (arm64-vinsns.lisp:287) builds this frame
         * as {header@0, savedsp@8, params@16..} and RESERVES 4 words at the
         * frame TOP for a boundary lisp_frame, with the header's element
         * count deliberately covering them so the GC skips the uninitialized
         * words; the ff-call sequence "builds the frame there and then
         * shrinks the count by 4 to publish it" (arm642.lisp:6323).
         *
         * There is therefore NO savelr slot to park lr in, and [sp,#8] is
         * the saved SP we must not touch.  We used to do
         * `str lr,[sp,#c_frame.savelr]' with savelr=8 -- clobbering the
         * saved SP -- and then restore sp from offset 0, which is the
         * HEADER: sp became 0xded (the header for a 14-word frame,
         * (13<<8)|subtag_u64_vector) and the caller's own epilogue
         * `ldp x7,x30,[sp,#16]' took a SIGBUS.
         *
         * PPC64 is the shape reference: poweropen_ffcall (ppc-spentry.s:1595)
         * likewise puts the return address in lisp_frame.savelr of a boundary
         * frame, never in the c_frame.
         *
         * reserved_base = sp + node_size + node_size*(count - 4)
         *               = sp + node_size*(count - 3).
         * imm0-2 are free here; arg_z still holds the entry point. */
        ldr imm0, [sp, #c_frame.header]
        lsr imm1, imm0, #num_subtag_bits        /* element count = words-1 */
        sub imm1, imm1, #3
        mov imm2, sp                            /* add-shifted with Rn=sp is */
        add imm2, imm2, imm1, lsl #node_shift   /* an encoding trap; go via a reg */
        mov imm1, #lisp_frame_marker
        str imm1, [imm2, #lisp_frame.marker]
        str vsp, [imm2, #lisp_frame.savevsp]
        str fn,  [imm2, #lisp_frame.savefn]
        str lr,  [imm2, #lisp_frame.savelr]
        sub imm0, imm0, #(4 << num_subtag_bits) /* publish the 4 words */
        str imm0, [sp, #c_frame.header]
        /* Unbox the entry point into temp4 (x16 = IP0). */
        /* PPC-faithful discrimination (ppc:1802-1814 extract_typecode):
         * macptr iff fulltag_misc AND header subtag == subtag_macptr;
         * anything else the raw bits ARE the address.  A bare tst
         * #tagmask misclassified 4-aligned C entry points (16m5l). */
        and imm2, arg_z, #fulltagmask
        cmp imm2, #fulltag_misc
        b.ne 8f
        ldurb w2, [arg_z, #misc_subtag_offset]
        cmp imm2, #subtag_macptr
        b.ne 8f
        ldur temp4, [arg_z, #macptr.address]
        b 9f
8:      mov temp4, arg_z        // fixnum-locative / raw code address
9:
        /* Publish lisp state to the TCR for the GC, then go foreign. */
        str vsp, [rcontext, #tcr.save_vsp]
        str tsp, [rcontext, #tcr.save_tsp]
        str allocptr, [rcontext, #tcr.save_allocptr]
        str allocbase, [rcontext, #tcr.save_allocbase]
        /* Load the outgoing GPR args (clobbers imm0-imm5/nargs/fn) and
         * pop the frame head + param words so any stack args sit
         * exactly at SP for the callee. */
        ldp x0, x1, [sp, #c_frame.params]
        ldp x2, x3, [sp, #(c_frame.params + 2*node_size)]
        ldp x4, x5, [sp, #(c_frame.params + 4*node_size)]
        ldp x6, x7, [sp, #(c_frame.params + 6*node_size)]
        /* AAPCS64, no stack args (<=8 GPR/<=8 FP, enforced loud in the
         * w13 codegen): keep SP at the frame head -- the callee's stack
         * grows BELOW its incoming SP, so popping the frame here hands
         * the saved lr/backlink to the callee as scratch (16m5c crash:
         * return jumped into the c_frame).  Stack-arg layout = ratify
         * item (frame head must move above the param area). */
        /* Record the lisp<->foreign boundary for the GC (16m41; protocol note
         * in spentry-E-ffi.s).  The walk must start at the c_frame base: word
         * 0 there is the frame's own ivector header, whose (already shrunk)
         * count strides exactly onto the boundary lisp_frame built above.
         * Park the enclosing boundary in param word 0 -- dead now that the
         * args are loaded, INSIDE the c_frame ivector so the GC never scans
         * it, and above SP so the callee cannot touch it.
         *
         * ORDER MATTERS, and it is why the arg loads moved above the valence
         * store: the boundary must be in place BEFORE this thread advertises
         * foreign valence, or a GC in the window reads a stale boundary and
         * walks the wrong region (ARM-family ff-call stores it first for the
         * same reason).  temp0 is scratch: the return path re-nils every temp,
         * and it is not an AAPCS64 argument register the way imm0 is. */
        ldr temp0, [rcontext, #tcr.last_lisp_frame]
        str temp0, [sp, #c_frame.params]
        mov temp0, sp
        str temp0, [rcontext, #tcr.last_lisp_frame]
        mov temp0, #TCR_STATE_FOREIGN
        str temp0, [rcontext, #tcr.valence]
        /* Open the capture window: discard lisp-side cumulative flags so
         * tcr.foreign_fpsr below is the CALLEE's, not "everything since the
         * last ff-call".  See the header note.  msr writes FPSR only --
         * PSTATE.NZCV is a separate register in AArch64, so this cannot
         * disturb the condition flags. */
        msr fpsr, xzr
        blr temp4
        /* Back.  x0/d0 hold the results; imm1/imm2 are scratch. */
        mrs imm1, fpsr
        str imm1, [rcontext, #tcr.foreign_fpsr]
        msr fpsr, xzr
        /* A GC may have run while we were foreign. */
        ldr allocptr, [rcontext, #tcr.save_allocptr]
        ldr allocbase, [rcontext, #tcr.save_allocbase]
        /* Recover lr from the boundary lisp_frame and sp from the SAVED SP
         * word -- not from offset 0, which is the header (16m30, see the
         * entry note).  The count has already been shrunk by 4, so
         * reserved_base = save3 + node_size*(count + 1).  x0/d0 hold the
         * results: imm1/imm2 only, never imm0.
         * SUBPRIM-POPS is the w13 contract: the caller's epilogue runs with
         * sp back at its own lisp frame (confirmed against the emitted
         * MAKE-GCABLE-MACPTR epilogue, which pops a 32-byte frame at sp). */
        ldr imm1, [save3, #c_frame.header]
        lsr imm1, imm1, #num_subtag_bits
        add imm1, imm1, #1
        add imm2, save3, imm1, lsl #node_shift
        ldr lr, [imm2, #lisp_frame.savelr]
        ldr imm1, [save3, #c_frame.savedsp]
        /* Hand the enclosing foreign boundary back BEFORE sp moves (16m41). */
        ldr imm2, [save3, #c_frame.params]
        str imm2, [rcontext, #tcr.last_lisp_frame]
        mov sp, imm1
        ldr save3, [vsp], #node_size
        ldr fn, [vsp], #node_size
        /* Clear C garbage out of the volatile node registers, then --
         * and only then -- declare lisp valence: the GC must never see
         * a stale pointer in a node register of a lisp-valence thread. */
        mov arg_w, rnil
        mov arg_x, rnil
        mov arg_y, rnil
        mov arg_z, rnil
        mov temp0, rnil
        mov temp1, rnil
        mov temp2, rnil
        mov temp3, rnil
        mov temp4, rnil
        mov temp5, rnil
        mov nargs, xzr
        str xzr, [rcontext, #tcr.valence]   // TCR_STATE_LISP
        /* Take any interrupt that was DEFERRED while we held foreign valence
         * (ppc:1691 check_pending_interrupt(cr1), at the same seam: after the
         * valence store, immediately before returning to lisp).  16m57 ROOT:
         * interrupt_handler cannot take an interrupt on a TCR_STATE_FOREIGN
         * thread, so it records one by setting TCR_INTERRUPT_LEVEL to fixnum 1
         * (arm64-exceptions.c:2253-2255, = ppc-exceptions.c:2144-2151).  With
         * no poll here that record is the ONLY one -- tcr.interrupt_pending is
         * NOT set on that path -- and the very next *interrupt-level* unbind
         * (_SPunbind_to, watched) overwrites it, so the interrupt is lost
         * forever.  Symptom: a saved image whose listener calls (quit) never
         * exits, because the sleeping Initial process never runs the
         * process-interrupt thunk.  nargs is dead here (just zeroed above),
         * exactly as in the sibling _SPffcall_return_registers
         * (spentry-E-ffi.s:378) and _SPsyscall (:666); x0/d0 hold the foreign
         * result and the macro does not touch them. */
        check_pending_interrupt
        ret
endsp ffcall

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
        .quad _SPbuiltin_plus // 0 SPbuiltin_plus
        .quad _SPbuiltin_minus // 1 SPbuiltin_minus
        .quad _SPbuiltin_times // 2 SPbuiltin_times
        .quad _SPbuiltin_div // 3 SPbuiltin_div
        .quad _SPbuiltin_eq // 4 SPbuiltin_eq
        .quad _SPbuiltin_ne // 5 SPbuiltin_ne
        .quad _SPbuiltin_gt // 6 SPbuiltin_gt
        .quad _SPbuiltin_ge // 7 SPbuiltin_ge
        .quad _SPbuiltin_lt // 8 SPbuiltin_lt
        .quad _SPbuiltin_le // 9 SPbuiltin_le
        .quad _SPbuiltin_eql // 10 SPbuiltin_eql
        .quad _SPbuiltin_length // 11 SPbuiltin_length
        .quad _SPbuiltin_seqtype // 12 SPbuiltin_seqtype
        .quad _SPbuiltin_assq // 13 SPbuiltin_assq
        .quad _SPbuiltin_memq // 14 SPbuiltin_memq
        .quad _SPbuiltin_logbitp // 15 SPbuiltin_logbitp
        .quad _SPbuiltin_logior // 16 SPbuiltin_logior
        .quad _SPbuiltin_logand // 17 SPbuiltin_logand
        .quad _SPbuiltin_ash // 18 SPbuiltin_ash
        .quad _SPbuiltin_negate // 19 SPbuiltin_negate
        .quad _SPbuiltin_logxor // 20 SPbuiltin_logxor
        .quad _SPbuiltin_aref1 // 21 SPbuiltin_aref1
        .quad _SPbuiltin_aset1 // 22 SPbuiltin_aset1
        .quad _SPfuncall // 23 SPfuncall
        .quad _SPmkcatch1v // 24 SPmkcatch1v
        .quad _SPmkcatchmv // 25 SPmkcatchmv
        .quad _SPmkunwind // 26 SPmkunwind
        .quad _SPbind // 27 SPbind
        .quad _SPconslist // 28 SPconslist
        .quad _SPconslist_star // 29 SPconslist_star
        .quad _SPmakes32 // 30 SPmakes32
        .quad _SPmakeu32 // 31 SPmakeu32
        .quad _SPfix_overflow // 32
        .quad _SPmakeu64 // 33
        .quad _SPmakes64 // 34
        .quad _SPmvpass // 35 SPmvpass
        .quad _SPvalues // 36 SPvalues
        .quad _SPnvalret // 37 SPnvalret
        .quad _SPthrow // 38 SPthrow
        .quad _SPnthrowvalues // 39 SPnthrowvalues
        .quad _SPnthrow1value // 40 SPnthrow1value
        .quad _SPbind_self // 41 SPbind_self
        .quad _SPbind_nil // 42 SPbind_nil
        .quad _SPbind_self_boundp_check // 43 SPbind_self_boundp_check
        .quad _SPrplaca // 44 SPrplaca
        .quad _SPrplacd // 45 SPrplacd
        .quad _SPgvset // 46 SPgvset
        .quad _SPset_hash_key // 47 SPset_hash_key
        .quad _SPstore_node_conditional // 48 SPstore_node_conditional
        .quad _SPset_hash_key_conditional // 49 SPset_hash_key_conditional
        .quad _SPstkconslist // 50 SPstkconslist
        .quad _SPstkconslist_star // 51 SPstkconslist_star
        .quad _SPmkstackv // 52 SPmkstackv
        .quad _SPsetqsym // 53 SPsetqsym
        .quad _SPprogvsave // 54 SPprogvsave
        .quad _SPstack_misc_alloc // 55 SPstack_misc_alloc
        .quad _SPgvector // 56 SPgvector
        .quad _SPfitvals // 57 SPfitvals
        .quad _SPnthvalue // 58 SPnthvalue
        .quad _SPdefault_optional_args // 59 SPdefault_optional_args
        .quad _SPopt_supplied_p // 60 SPopt_supplied_p
        .quad _SPheap_rest_arg // 61 SPheap_rest_arg
        .quad _SPreq_heap_rest_arg // 62 SPreq_heap_rest_arg
        .quad _SPheap_cons_rest_arg // 63 SPheap_cons_rest_arg
        .quad 0 // 64 SPcheck_fpu_exception
        .quad 0 // 65 SPdiscard_stack_object
        .quad _SPksignalerr // 66 SPksignalerr
        .quad _SPstack_rest_arg // 67 SPstack_rest_arg
        .quad _SPreq_stack_rest_arg // 68 SPreq_stack_rest_arg
        .quad _SPstack_cons_rest_arg // 69 SPstack_cons_rest_arg
        .quad _SPcall_closure // 70 SPcall_closure
        .quad _SPspreadargz // 71 SPspreadargz
        .quad _SPtfuncallgen // 72 SPtfuncallgen
        .quad _SPtfuncallslide // 73 SPtfuncallslide
        .quad _SPjmpsym // 74 SPjmpsym
        .quad _SPtcallsymgen // 75 SPtcallsymgen
        .quad _SPtcallsymslide // 76 SPtcallsymslide
        .quad _SPtcallnfngen // 77 SPtcallnfngen
        .quad _SPtcallnfnslide // 78 SPtcallnfnslide
        .quad _SPmisc_ref // 79
        .quad _SPsubtag_misc_ref // 80 SPsubtag_misc_ref
        .quad _SPmakestackblock // 81 SPmakestackblock
        .quad _SPmakestackblock0 // 82 SPmakestackblock0
        .quad _SPmakestacklist // 83 SPmakestacklist
        .quad _SPstkgvector // 84 SPstkgvector
        .quad _SPmisc_alloc // 85 SPmisc_alloc
        .quad 0 // 86 SPatomic_incf_node
        .quad _SPrecover_values // 87 SPrecover_values
        .quad _SPinteger_sign // 88 SPinteger_sign
        .quad _SPsubtag_misc_set // 89 SPsubtag_misc_set
        .quad _SPmisc_set // 90 SPmisc_set
        .quad _SPspread_lexprz // 91 SPspread_lexprz
        .quad _SPreset // 92 SPreset
        .quad _SPmvslide // 93 SPmvslide
        .quad _SPsave_values // 94 SPsave_values
        .quad _SPadd_values // 95 SPadd_values
        .quad _SPmisc_alloc_init // 96 SPmisc_alloc_init
        .quad _SPstack_misc_alloc_init // 97 SPstack_misc_alloc_init
        .quad _SPpopj // 98 SPpopj
        .quad _SPgetu64 // 99 SPgetu64
        .quad _SPgets64 // 100 SPgets64
        .quad _SPspecref // 101 SPspecref
        .quad _SPspecrefcheck // 102 SPspecrefcheck
        .quad _SPspecset // 103 SPspecset
        .quad _SPgets32 // 104 SPgets32
        .quad _SPgetu32 // 105 SPgetu32
        .quad _SPmvpasssym // 106 SPmvpasssym
        .quad _SPunbind // 107 SPunbind
        .quad _SPunbind_n // 108 SPunbind_n
        .quad _SPunbind_to // 109 SPunbind_to
        .quad _SPprogvrestore // 110 SPprogvrestore
        .quad _SPbind_interrupt_level_0 // 111 SPbind_interrupt_level_0
        .quad _SPbind_interrupt_level_m1 // 112 SPbind_interrupt_level_m1
        .quad _SPbind_interrupt_level // 113 SPbind_interrupt_level
        .quad _SPunbind_interrupt_level // 114 SPunbind_interrupt_level
        .quad _SParef2 // 115 SParef2
        .quad _SParef3 // 116 SParef3
        .quad _SPaset2 // 117 SPaset2
        .quad _SPaset3 // 118 SPaset3
        .quad _SPkeyword_bind // 119 SPkeyword_bind
        .quad _SPffcall // 120 SPffcall
        .quad 0 // 121 SPdebind
        .quad _SPcallback // 122 SPcallback
        .quad _SPffcall_return_registers // 123 SPffcall_return_registers (PROPOSED extension, 16m5f)
        .quad _SPtfuncallvsp // 124 SPtfuncallvsp (PROPOSED extension, 16m5f)
        .quad _SPcallbuiltin // 125 SPcallbuiltin (PROPOSED extension, 16m5f)
        .quad _SPcallbuiltin0 // 126 SPcallbuiltin0 (PROPOSED extension, 16m5f)
        .quad _SPcallbuiltin1 // 127 SPcallbuiltin1 (PROPOSED extension, 16m5f)
        .quad _SPcallbuiltin2 // 128 SPcallbuiltin2 (PROPOSED extension, 16m5f)
        .quad _SPcallbuiltin3 // 129 SPcallbuiltin3 (PROPOSED extension, 16m5f)
        .quad _SPlexpr_entry // 130 SPlexpr_entry (PROPOSED extension, 16m5f)
        .quad _SPnmkunwind // 131 SPnmkunwind (PROPOSED extension, 16m5f)
C(sptab_end):
