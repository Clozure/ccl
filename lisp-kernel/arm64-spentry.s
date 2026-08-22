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
#include "arm64-globals-proposed.s"

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
 * is popped (sp lands on the saved previous SP by dropping the
 * boundary lisp_frame reserved at the frame top); no node register
 * holds C garbage.
 *
 * Register notes: x19-x28 (save0-3, rnil, tsp, vsp, allocptr,
 * allocbase, rcontext) are AAPCS64 callee-saved, so the pinned lisp
 * registers survive the call for free.  fn (x7) is an argument
 * register: park it on the value stack (GC-visible via
 * tcr.save_vsp) before the argument load clobbers it.  We don't
 * save/restore FPCR: lisp runs with the process-default FPCR and a
 * conforming callee doesn't dirty it.
 *
 * NO FPSR ACCESS ON THIS PATH.  This spentry used to own an FPSR capture
 * window -- `msr fpsr, xzr' before the blr, `mrs' + publish to
 * tcr.foreign_fpsr + `msr' after, opened as well as closed because FPSR
 * is cumulative (16m48b: a window only closed charges every flag raised
 * since the PREVIOUS ff-call, including inline lisp float arithmetic, to
 * this callee; log/exp/sin/atan all reproduced, ANSI caught it as
 * PRINT.SHORT-FLOAT.RANDOM).  MEASURED on Neoverse-N1 (16m82,
 * tools/perf/ffcall-replica.c): those three FPSR accesses cost 13.3 ns
 * of an 18 ns per-call transition excess over x86-64 -- FPSR is not
 * renamed, and each access synchronizes the FP pipeline.  x86-64 pays
 * nothing on its common path (the SIGFPE handler captures MXCSR lazily);
 * AArch64 without trapped-FP support cannot take that path, but it can
 * take ARM32's, which is where the flag window now lives: the float
 * transcendental wrappers -- the ONLY consumers of the captured flags --
 * clear the cumulative flags at the call site (`%set-fpscr-status 0',
 * level-1/l1-numbers.lisp, the idiom ARM32 has always used there) and
 * read the LIVE FPSR afterwards (%get-fpscr-status, which
 * %ffi-exception-status now wraps).  A plain ff-call touches no FP
 * state at all, and lisp's accrued FPSR flags survive foreign calls
 * instead of being discarded per call.  The 16m48b attribution
 * guarantee is preserved: the call-site clear immediately before the
 * foreign call is exactly what prevents it.
 */
spentry ffcall
        /* Spill fn AND all four boxed NVRs to the vstack (the protocol the
         * spentry-E-ffi.s header always prescribed: "save0-3 are still
         * vpushed so the GC can SEE them while the thread is foreign").
         * tcr.save_vsp is published below this spill, so a foreign-era GC
         * keeps and RELOCATES these five values; they are reloaded -- with
         * any relocation applied -- after the call.  A conforming callee
         * preserves x19-x22, but preservation is not FORWARDING: a lisp
         * value that stays only in an NVR register across the call misses
         * any GC relocation.  save0/save1 (and, in the siblings, save2/
         * save3) also carry raw kernel state across the call from here on:
         * 8/16-aligned stack addresses, which mark_xp/forward_xp read as
         * fixnums (no-op roots) whenever we are suspended in lisp valence. */
        str fn, [vsp, #-node_size]!
        str save3, [vsp, #-node_size]!
        str save2, [vsp, #-node_size]!
        str save1, [vsp, #-node_size]!
        str save0, [vsp, #-node_size]!
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
        /* Build order is the alloc-c-frame contract (the design note at
         * arm64-vinsns.lisp ALLOC-C-FRAME spells it out): a slot that is
         * still COVERED by the u64-vector header is invisible to the GC,
         * so a real fn/lr stored here before the count shrink goes STALE
         * if a GC moves the caller in that window -- the register and
         * vstack copies are forwarded, the covered slot is not, and the
         * return path reads savelr back from this frame.  So: harmless
         * zeros while covered, publish, THEN stp the real fn/lr into the
         * frame the GC now walks and forwards.  marker/savevsp may be
         * prestored: a constant and a never-relocated stack address. */
        stp xzr, xzr, [imm2, #lisp_frame.savefn]
        sub imm0, imm0, #(4 << num_subtag_bits) /* publish the 4 words */
        str imm0, [sp, #c_frame.header]
        stp fn, lr, [imm2, #lisp_frame.savefn]
        /* Hoist everything the return path will need out of the frame head
         * [sp, sp+80) into callee-saved registers: at the blr, SP steps
         * over header+savedsp+params, putting that region below the
         * callee's incoming SP, where the callee's own frame (or any
         * signal frame) clobbers it.
         *   save1 = the boundary lisp_frame published above (sp is
         *           restored to it after the call; the previous SP is
         *           save1 + lisp_frame.size by construction --
         *           arm642-c-frame-words reserves the 4 boundary words
         *           directly below the saved previous SP)
         *   save0 = the enclosing foreign boundary (previously parked in
         *           param word 0; that park dies with the region) */
        mov save1, imm2
        ldr save0, [rcontext, #tcr.last_lisp_frame]
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
        /* Record the lisp<->foreign boundary for the GC (16m41 protocol,
         * re-pointed for stack args): the boundary is now the PUBLISHED
         * boundary lisp_frame itself -- mark_cstack_area classifies a walk
         * that STARTS on lisp_frame_marker, which is what the syscall
         * sibling also hands it -- because the old start point, the c_frame
         * base, dies once SP steps over it at the blr.
         * Everything below the boundary is foreign to the GC while the
         * callee runs, which was already the status of the raw, never-
         * scanned param/stack-arg words when the ivector cover held them.
         *
         * ORDER (one store per step; auditable per instruction boundary):
         *   1. boundary store BEFORE the valence store (16m41): a GC that
         *      suspends us FOREIGN must find the new boundary; while we
         *      are still LISP the walk starts at context SP and this word
         *      is ignored, so storing it early is inert.
         *   2. valence store BEFORE the SP step: while LISP the walk
         *      starts at SP, which must still name the self-describing
         *      frame head (ivector header striding onto the boundary
         *      frame); once FOREIGN the walk starts at the boundary and
         *      SP is free to move.
         * temp0 is scratch: the return path re-nils every temp, and it is
         * not an AAPCS64 argument register the way imm0 is. */
        str save1, [rcontext, #tcr.last_lisp_frame]
        mov temp0, #TCR_STATE_FOREIGN
        str temp0, [rcontext, #tcr.valence]
        /* ARM64-DEVIATION: step SP over header+savedsp+params[0..7] so the
         * callee sees its stack arguments AT [SP], per AAPCS64 5.4.2 (the
         * single NSAA area the codegen marshals at param words 8..; +80 =
         * c_frame.size + the 8 GPR param words).  The syscall sibling does
         * NOT do this: `svc' takes no stack arguments, so it leaves SP at
         * the frame head and its c_frame stays live above SP throughout.
         * PPC64 never moves SP here -- PowerOpen stack params live in the
         * CALLER's frame at positive offsets from the caller's SP; x86-64
         * is the shape donor (_SPffcall's ffcall_setup pops the frame head
         * plus 6 GPR words so rsp == &param[6] at the call).  The frame
         * base is 16-aligned and 80 = 5*16, so SP is 16-byte aligned at
         * the blr as AAPCS64 requires.  From this instruction on, the
         * region [old SP, old SP+80) belongs to the callee/signals; every
         * value the return path needs was hoisted to save0/save1 above. */
        add sp, sp, #(c_frame.size + 8*node_size)
        blr temp4
        /* Back.  x0/d0 hold the results.  [save3, save3+80) is DEAD -- it
         * sat below the callee's incoming SP -- so nothing on this path may
         * read the c_frame head: the return runs entirely from the save0/
         * save1 hoist.  x0/d0 are never touched.  No FPSR access here --
         * the float wrappers own the flag window (header note). */
        /* Retreat SP onto the boundary lisp_frame (it sat at/above the
         * callee's incoming SP, so it is intact, and the foreign-era GC has
         * been walking AND FORWARDING its slots).  This is PPC64's shape:
         * poweropen_ffcall likewise returns onto the boundary frame, reads
         * savelr/savefn from it after the valence flip, then discards it.
         * The saved previous SP is not needed: the frame top IS the
         * previous SP (arm642-c-frame-words reserves these 4 words directly
         * below it), so the discard below lands sp exactly there --
         * SUBPRIM-POPS, the w13 contract. */
        mov sp, save1
        /* Make EVERY node register GC-valid, then -- and only then -- flip
         * to lisp valence; every reload moves BELOW the flip (donor order:
         * ppc-spentry.s poweropen_ffcall tail and the x86-64 ffcall tail
         * both zero/nil the node set, flip, then pop).  Popping an NVR
         * while still FOREIGN was a stale-register window: a GC after the
         * pop forwards the vstack slot but never the register.  After the
         * flip the suspended context IS this thread's GC image --
         * mark_xp/forward_xp cover the registers, and the still-unpopped
         * slots stay inside the context-vsp scan -- so each pop reads a
         * forwarded slot into a forwarded register file.  save0 (a raw
         * cstack address) stays live across the flip: it reads as a fixnum,
         * so it is GC-valid without being nil'd.  allocptr/allocbase cross
         * the flip as VOID_ALLOCPTR (PPC's li allocptr,-dnode_size idiom):
         * their pre-call register values are stale if a foreign-era GC ran,
         * and normalize_tcr treats VOID as "no allocation in flight". */
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
        mov fn, rnil
        mov save1, xzr
        mov save2, xzr
        mov save3, xzr
        mov allocptr, #-dnode_size          // VOID_ALLOCPTR
        mov allocbase, #-dnode_size
        str xzr, [rcontext, #tcr.valence]   // TCR_STATE_LISP
        /* Hand the enclosing foreign boundary back.  Post-flip on purpose:
         * while we were foreign the boundary had to keep naming OUR
         * lisp_frame (handing it back early would have cut the caller's
         * frames out of the foreign walk); now that valence is LISP the
         * walk starts at context SP and this word is dormant until the
         * next foreign transition.  A signal/uuo in the window saves and
         * re-points it itself (exit_signal_handler restores), so lisp run
         * under an interrupt here still nests correctly. */
        str save0, [rcontext, #tcr.last_lisp_frame]
        /* lr from the frame AT [sp], AFTER the flip: from the flip onward
         * the GC forwards the context's LR/PC, and until the flip it
         * forwarded the frame slot we read from -- no instruction boundary
         * where a relocated caller leaves a stale return pc.  (The old
         * pre-flip read had exactly that hole, foreign-GC-after-read.) */
        ldr lr, [sp, #lisp_frame.savelr]
        /* Reload the NVRs from their (possibly forwarded) vstack slots and
         * the allocation pointers from the TCR -- a foreign-era GC leaves
         * VOID_ALLOCPTR there, which forces a fresh segment at the next
         * allocation, as intended. */
        ldr save0, [vsp], #node_size
        ldr save1, [vsp], #node_size
        ldr save2, [vsp], #node_size
        ldr save3, [vsp], #node_size
        ldr fn, [vsp], #node_size
        ldr allocptr, [rcontext, #tcr.save_allocptr]
        ldr allocbase, [rcontext, #tcr.save_allocbase]
        /* Drop the 32-byte boundary frame: sp = the saved previous SP. */
        add sp, sp, #lisp_frame.size
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

/* ppc-constants.s:171 "(UNSIGNED-BYTE 16), one less than TSTACK_SOFTPROT" */
.set tstack_alloc_limit, 0xffff

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
        ldr     temp0, [nfn, #_function.code_vector]
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
1:      /* imm1 = byte count; round up to the total tsp allocation (frame
         * header + object header + data), ppc-spentry.s:1058. */
        dnode_align imm1, imm1, (tsp_frame.fixed_overhead + node_size)
        mov     imm3, #tstack_alloc_limit
        cmp     imm1, imm3
        b.ge    9f
        /* Push a boxed frame of imm1 bytes (built below the live tsp and
         * published atomically).  "_nz": imm1 always includes the frame
         * overhead + object header, so the data area is never empty. */
        TSP_Alloc_Var_Boxed_nz imm1, temp4
        str     imm0, [tsp, #tsp_frame.data_offset]  /* object header */
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
         * (TSP_Alloc_Fixed_Unboxed (0), ppc-spentry.s:1068 -- type=self,
         * nonzero, so GC skips it) so the compiler's balancing discard-
         * temp-frame still has a frame to pop, then heap-cons via
         * misc_alloc instead; arg_y/arg_z are unchanged, matching
         * misc_alloc's own (count, subtag) calling convention. */
        TSP_Alloc_Fixed_Unboxed 0, temp4
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
        dnode_align imm0, imm0, (tsp_frame.fixed_overhead + macptr.size)
        mov     imm1, #tstack_alloc_limit
        cmp     imm0, imm1
        b.ge    1f
        /* Push a raw/unboxed frame of imm0 bytes (built below the live tsp
         * and published atomically -- see arm64-macros.s). */
        TSP_Alloc_Var_Unboxed imm0, temp4
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
        TSP_Alloc_Fixed_Unboxed 0, temp4
        mov     nargs, #(1 << fixnumshift)      /* ppc:3319 set_nargs(1)          */
        ref_nrs_symbol fname, new_gcable_ptr    /* ppc:3320 li fname,nrs.new_gcable_ptr */
        ldr     nfn, [fname, #symbol.fcell]     /* ppc:3321 jump_fname()          */
        ldr     temp0, [nfn, #_function.code_vector]
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
        /* Push a boxed frame of imm0 bytes (built below the live tsp and
         * published atomically).  imm0 == fixed_overhead when arg_y=0, so the
         * data area may be empty -- the leading-test TSP_Alloc_Var_Boxed (not
         * the "_nz" do-while) handles that. */
        TSP_Alloc_Var_Boxed imm0, temp4
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
3:      /* Too big for the tstack: push one empty BOXED tsp frame
         * (TSP_Alloc_Fixed_Boxed(0), ppc-spentry.s:3377), then heap-cons
         * cell by cell via Cons. */
        TSP_Alloc_Fixed_Boxed 0, temp4
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
        ldr     temp0, [nfn, #_function.code_vector]
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
        ldr     temp0, [nfn, #_function.code_vector]
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
        dnode_align imm0, imm0, (tsp_frame.fixed_overhead + macptr.size)
        mov     imm1, #tstack_alloc_limit
        cmp     imm0, imm1
        b.ge    makestackblock0_too_big
        /* Push a raw/unboxed frame of imm0 bytes (built below the live tsp
         * and published atomically).  The frame stays raw, so the GC skips
         * it -- the data-zeroing below is for the block's contents (clear-p),
         * not GC safety. */
        TSP_Alloc_Var_Unboxed imm0, temp4
        /* Zero the data area [data_offset .. old_tsp).  old_tsp = tsp + imm0
         * (Var_Unboxed preserves imm0); end (old_tsp-8) in imm1, cursor imm0. */
        add     imm1, tsp, imm0
        sub     imm1, imm1, #node_size
        add     imm0, tsp, #tsp_frame.data_offset
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
        TSP_Alloc_Fixed_Unboxed 0, temp4
        mov     arg_y, arg_z                    /* ppc:3343 mr arg_y,arg_z (save block size) */
        add     arg_z, rnil, #t_offset          /* ppc:3344 li arg_z,t_value (clear-p = T)   */
        mov     nargs, #(2 << fixnumshift)      /* ppc:3345 set_nargs(2)          */
        ref_nrs_symbol fname, new_gcable_ptr    /* ppc:3346 li fname,nrs.new_gcable_ptr */
        ldr     nfn, [fname, #symbol.fcell]     /* ppc:3347 jump_fname()          */
        ldr     temp0, [nfn, #_function.code_vector]
        br      temp0
endsp makestackblock0

/*
 * Cluster B: vectors-misc subprims
 * Ported from vendor/ccl/lisp-kernel/ppc-spentry.s (PPC64 branch)
 *
 * 22 subprims: gvset, set_hash_key, store_node_conditional,
 * set_hash_key_conditional, conslist, conslist_star, stkconslist,
 * stkconslist_star, mkstackv, progvsave, gvector, misc_ref,
 * subtag_misc_ref, stkconsyz, stkgvector, subtag_misc_set, misc_set,
 * progvrestore, aref2, aref3, aset2, aset3
 */

/* PORT-NOTE: All 22 subprims ported line-by-line from PPC64.
   misc_ref (~150 lines) and misc_set (~180 lines) cover integer/node/string/bit
   vectors, and since 16m37/16m41 ALSO the four float-vector subtags
   (single/double) and the two complex ones -- the note here that they were
   "omitted pending Misc_Alloc_Fixed and subtag constants" is stale; both exist
   and both are used by those legs. aref2/3 and aset2/3 provide
   2D/3D array indexing with displaced-array follow chains. File builds once
   missing constants are defined (35 #error directives guard missing definitions). */

/* Derived constants (same derivations as spentry-A/-C/-D):
 * dnode_shift: ppc-constants64.s:37 (log2 dnode_size=16);
 * bitmap_shift: ppc-constants64.s (log2 nbits_in_word=64). */
.set dnode_shift, 4
.set bitmap_shift, 6

/* Lisp error selectors: errors.s deferr(NAME,N) = boxed fixnum N. */
.set XBADVEC,    (2<<fixnumshift)       /* errors.s:177 */
.set XSETBADVEC, (7<<fixnumshift)       /* errors.s:182 */
.set XNOTELT,    (174<<fixnumshift)     /* errors.s:227 */
.set XIMPROPERLIST, (170<<fixnumshift)  /* errors.s:223 */
.set tstack_alloc_limit, 0xffff         /* ppc-constants.s:171 (as spentry-A) */

/* misc_complex_dfloat_offset (16m48) — Matt's arm64-arch.lisp:259-261:
     ;;; There is a pad word after the uvector header so that the
     ;;; complex-double-float elements are 16-byte aligned.
     (defconstant misc-complex-dfloat-offset (+ misc-data-offset node-size))
   Element 0 of a complex-double-float VECTOR starts one word past the normal
   data offset, exactly as on x86-64 (x8664-arch.lisp:442).
   ARM64-REFERENCE-CHAIN: clozure-wip=yes; ppc64=yes; reason=Matt's arm64-arch.lisp:259-261 governs and is cited first; PPC64 applies the SAME +node_size bias, spelled ppc64::complex-double-float.realpart at ppc2.lisp:1440-1450 (define-fixedsized-object puts pad at misc-data-offset, so realpart IS misc-data-offset+node-size) -- it just defines no misc-complex-dfloat-offset constant; x8664-arch.lisp:442 is corroboration, because low tags ARE the x86-64 model.
   The lisp side already relies on it -- l0-array.lisp's %uvector-replace biases its
   %copy-ivector-to-ivector offsets by (- misc-complex-dfloat-offset
   misc-data-offset), and %init-misc's cdf leg starts at
   complex-double-float.realpart, which is the same 4 -- but misc_ref/misc_set
   here used the unpadded misc_data_offset, so every bulk fill or copy landed
   one double ahead of what an element read saw.  That is MAKE-SEQUENCE.30 and
   SUBSEQ.SPECIALIZED-VECTOR.3.
   Costs no space and no GC change: misc_alloc computes dnode_align(16n + 8) =
   16n + 16 (spentry-A:479-481), i.e. a 16n+8-byte data area, and the GC's
   suffix_dnodes = ((total+15)>>4)-1 is n for total = 8+16n and for 16+16n
   alike, so the pad was already reserved and already walked. */
.set misc_complex_dfloat_offset, (misc_data_offset + node_size)

/* UUO scheme: Matt's own arm64-uuo.s @ 115b7aa (included above).  It
   ALREADY defines xtype_array2d = 0x30 / xtype_array3d = 0x40 ("Keep
   these in sync with the values in arm64-arch.lisp"), which is the
   numbering *arm64-xtype-specifiers* (arm64-trap-support.lisp:215)
   decodes; the expected-type field is 8 bits wide, so nothing here
   needs compacting.  16m41: local .sets of 40/44 shadowed his values
   and made a tripped aset2/aref2 trap report "(SIGNED-BYTE 64)"
   (0x28 = his xtype_s64) with the ARRAY as datum. */

/* (The local tsp_alloc_var_boxed macro that lived here -- which flipped the
   frame to boxed BEFORE zeroing its data, exposing garbage nodes to the GC --
   has been replaced by TSP_Alloc_Var_Boxed in arm64-macros.s, which builds the
   frame below the live tsp and publishes it atomically.) */

/* ===== gvset ===== */
/* ported from ppc-spentry.s:568-608 (PPC64 branch) */
        .globl C(egc_gvset)
        .globl C(egc_gvset_did_store)
spentry gvset
C(egc_gvset):
        cmp arg_z, arg_x
        add imm0, arg_y, #misc_data_offset
        str arg_z, [imm0, arg_x]
C(egc_gvset_did_store):
        b.le 9f
        add imm0, imm0, arg_x
        /* GC write barrier (ppc:575-608).  Shift constants are real; the
           four GLOBALS need the ARM64 lisp_globals anchor - #error +
           intended instruction until that idiom is ratified. */
        ref_global imm2, ref_base       /* ppc:575 (idiom: arm64-globals-proposed.s) */
        mov imm3, #0x8000000000000000 /* load_highbit */
        ref_global imm1, oldspace_dnode_count   /* ppc:580 */
        sub imm0, imm0, imm2
        lsr imm0, imm0, #dnode_shift
        cmp imm0, imm1
        lsr imm2, imm0, #8              /* refidx granule = 256 dnodes     */
        and imm4, imm0, #0x3f           /* extract_bit_shift_count         */
        lsr imm0, imm0, #bitmap_shift
        lsr imm3, imm3, imm4
        ref_global temp0, refbits       /* ppc:585 */
        b.hs 9f                         /* ppc cmplr = UNSIGNED bge        */
        lsl imm0, imm0, #3 /* word_shift */
        ldr imm1, [temp0, imm0]
        tst imm1, imm3
        b.ne 9f
        add temp0, temp0, imm0          /* ldxr/stxr take [Xn] only        */
1:      ldxr imm1, [temp0]
        orr imm1, imm1, imm3
        stxr w17, imm1, [temp0]         /* status=temp5/x17: w2 aliases imm2,
                                           which is STILL LIVE (granule) */
        cbnz w17, 1b
        dmb ish
        and imm4, imm2, #0x3f /* extract_bit_shift_count */
        lsr imm2, imm2, #bitmap_shift
        mov imm3, #0x8000000000000000
        ref_global temp0, ephemeral_refidx      /* ppc:600 */
        lsl imm2, imm2, #3
        lsr imm3, imm3, imm4
        add temp0, temp0, imm2          /* ldxr/stxr take [Xn] only        */
2:      ldxr imm1, [temp0]
        orr imm1, imm1, imm3
        stxr w17, imm1, [temp0]
        cbnz w17, 2b
        dmb ish
9:      ret
endsp gvset

/* ===== set_hash_key ===== */
/* ported from ppc-spentry.s:615-683 (PPC64 branch) */
        .globl C(egc_set_hash_key)
        .globl C(egc_set_hash_key_did_store)
spentry set_hash_key
C(egc_set_hash_key):
        cmp arg_z, arg_x
        add imm0, arg_y, #misc_data_offset
        str arg_z, [imm0, arg_x]
C(egc_set_hash_key_did_store):
        b.le 9f
        add imm0, imm0, arg_x           /* ppc:622 slot address            */
        /* -- memoize the stored reference (ppc:623-654) -- */
        ref_global imm2, ref_base       /* ppc:623                         */
        mov imm3, #0x8000000000000000   /* ppc:624 load_highbit            */
        ref_global imm1, oldspace_dnode_count   /* ppc:625                 */
        sub imm0, imm0, imm2            /* ppc:626                         */
        lsr imm0, imm0, #dnode_shift    /* ppc:627                         */
        cmp imm0, imm1                  /* ppc:628 cmplr                   */
        lsr imm2, imm0, #8              /* ppc:629 refidx granule          */
        and imm4, imm0, #0x3f           /* ppc:630                         */
        lsr imm0, imm0, #bitmap_shift   /* ppc:631                         */
        lsr imm3, imm3, imm4            /* ppc:632                         */
        ref_global temp0, refbits       /* ppc:633 (base kept for part 2)  */
        ref_global temp1, ephemeral_refidx      /* ppc:634 (kept)          */
        b.hs 9f                         /* ppc:635 bgelr (UNSIGNED)        */
        lsl imm0, imm0, #3              /* ppc:636 word_shift              */
        ldr imm1, [temp0, imm0]         /* ppc:637                         */
        tst imm1, imm3                  /* ppc:638 and.                    */
        b.ne 3f                         /* ppc:639 already memoized        */
        add temp2, temp0, imm0          /* ldxr/stxr take [Xn] only        */
1:      ldxr imm1, [temp2]              /* ppc:640 lrarx                   */
        orr imm1, imm1, imm3
        stxr w17, imm1, [temp2]         /* status=temp5/x17 (imm2 live)        */
        cbnz w17, 1b
        dmb ish                         /* ppc:644 isync                   */
        mov imm3, #0x8000000000000000   /* ppc:645                         */
        and imm4, imm2, #0x3f           /* ppc:646                         */
        lsr imm2, imm2, #bitmap_shift   /* ppc:647                         */
        lsr imm3, imm3, imm4            /* ppc:648                         */
        lsl imm2, imm2, #3              /* ppc:649                         */
        add temp2, temp1, imm2
2:      ldxr imm1, [temp2]              /* ppc:650                         */
        orr imm1, imm1, imm3
        stxr w17, imm1, [temp2]
        cbnz w17, 2b
        dmb ish                         /* ppc:654                         */
3:      /* -- memoize the hash VECTOR itself (ppc:656-683) -- */
        ref_global imm1, ref_base       /* ppc:656                         */
        sub imm0, arg_x, imm1           /* ppc:657                         */
        lsr imm0, imm0, #dnode_shift    /* ppc:658                         */
        lsr imm2, imm0, #8              /* ppc:659                         */
        mov imm3, #0x8000000000000000   /* ppc:660                         */
        and imm4, imm0, #0x3f           /* ppc:661                         */
        lsr imm0, imm0, #bitmap_shift   /* ppc:662                         */
        lsr imm3, imm3, imm4            /* ppc:663                         */
        lsl imm0, imm0, #3              /* ppc:664                         */
        ldr imm1, [temp0, imm0]         /* ppc:665 (refbits base kept)     */
        tst imm1, imm3                  /* ppc:666                         */
        b.ne 9f                         /* ppc:667 bnelr                   */
        add temp2, temp0, imm0
4:      ldxr imm1, [temp2]              /* ppc:668                         */
        orr imm1, imm1, imm3
        stxr w17, imm1, [temp2]
        cbnz w17, 4b
        dmb ish                         /* ppc:672                         */
        mov imm3, #0x8000000000000000   /* ppc:673                         */
        and imm4, imm2, #0x3f           /* ppc:674                         */
        lsr imm2, imm2, #bitmap_shift   /* ppc:675                         */
        lsr imm3, imm3, imm4            /* ppc:676                         */
        lsl imm2, imm2, #3              /* ppc:677                         */
        add temp2, temp1, imm2
5:      ldxr imm1, [temp2]              /* ppc:678                         */
        orr imm1, imm1, imm3
        stxr w17, imm1, [temp2]
        cbnz w17, 5b
        dmb ish                         /* ppc:682                         */
9:      ret                             /* ppc:683                         */
endsp set_hash_key

/* ===== store_node_conditional ===== */
/* ported from ppc-spentry.s:705-748 (PPC64 branch) */
        .globl C(egc_store_node_conditional)
spentry store_node_conditional
C(egc_store_node_conditional):
        cmp arg_z, arg_x
        ldr temp0, [vsp], #node_size          /* vpop(temp0) */
        asr imm4, temp0, #fixnumshift         /* unbox_fixnum(imm4,temp0) */
        add imm0, arg_x, imm4                 /* ldxr/stxr take [Xn] only */
1:      ldxr temp1, [imm0]
        cmp temp1, arg_y
        b.ne 9f
        stxr w17, arg_z, [imm0]               /* status=temp5/x17 (uniform)    */
        .globl C(egc_store_node_conditional_test)
C(egc_store_node_conditional_test):
        cbnz w17, 1b
        dmb ish
        /* -- memoize the stored reference (ppc:718-748) -- */
        ref_global imm2, ref_base       /* ppc:719 (imm0 = slot addr)      */
        ref_global imm1, oldspace_dnode_count   /* ppc:720                 */
        sub imm0, imm0, imm2            /* ppc:721                         */
        mov imm3, #0x8000000000000000   /* ppc:722                         */
        lsr imm0, imm0, #dnode_shift    /* ppc:723                         */
        cmp imm0, imm1                  /* ppc:724 cmplr                   */
        lsr imm2, imm0, #8              /* ppc:725                         */
        and imm4, imm0, #0x3f           /* ppc:726                         */
        lsr imm0, imm0, #bitmap_shift   /* ppc:727                         */
        lsr imm3, imm3, imm4            /* ppc:728                         */
        ref_global temp1, refbits       /* ppc:729                         */
        b.hs 8f                         /* ppc:730 bge (UNSIGNED)          */
        lsl imm0, imm0, #3              /* ppc:731                         */
        add temp1, temp1, imm0
2:      ldxr imm1, [temp1]              /* ppc:732                         */
        orr imm1, imm1, imm3
        stxr w17, imm1, [temp1]
        cbnz w17, 2b
        dmb ish                         /* ppc:736                         */
        mov imm3, #0x8000000000000000   /* ppc:737                         */
        and imm4, imm2, #0x3f           /* ppc:738                         */
        lsr imm2, imm2, #bitmap_shift   /* ppc:739                         */
        ref_global temp1, ephemeral_refidx      /* ppc:740                 */
        lsr imm3, imm3, imm4            /* ppc:741                         */
        lsl imm2, imm2, #3              /* ppc:742                         */
        add temp1, temp1, imm2
3:      ldxr imm1, [temp1]              /* ppc:743                         */
        orr imm1, imm1, imm3
        stxr w17, imm1, [temp1]
        cbnz w17, 3b
        dmb ish                         /* ppc:747                         */
        /* NOTE: PPC puts C(egc_write_barrier_end) at the END of
           set_hash_key_conditional (the runtime checks the whole family
           as one PC range); moved there - MERGE-ORDER NOTE: when these
           drafts land in Matt's arm64-spentry.s, the EGC family
           (rplaca..set_hash_key_conditional) must stay contiguous. */
8:      add arg_z, rnil, #t_offset            /* success => T              */
        ret
9:      clrex                                 /* PPC strcx-to-RESERVATION_
                                                 DISCHARGE = discharge the
                                                 reservation; AArch64 has a
                                                 dedicated insn (our v2
                                                 arm64-spentry.s uses it) */
        mov arg_z, rnil                       /* failure => NIL            */
        ret
endsp store_node_conditional

/* ===== set_hash_key_conditional ===== */
/* ported from ppc-spentry.s:754-835 (PPC64 branch) */
spentry set_hash_key_conditional
        .globl C(egc_set_hash_key_conditional)
C(egc_set_hash_key_conditional):
        cmp arg_z, arg_x
        ldr temp0, [vsp], #node_size
        asr imm4, temp0, #fixnumshift
        add imm0, arg_x, imm4                 /* ldxr/stxr take [Xn] only */
1:      ldxr temp1, [imm0]
        cmp temp1, arg_y
        b.ne 9f
        stxr w17, arg_z, [imm0]               /* status=temp5/x17 (uniform)    */
        .globl C(egc_set_hash_key_conditional_test)
C(egc_set_hash_key_conditional_test):
        cbnz w17, 1b
        dmb ish
        /* -- memoize the stored reference (ppc:768-797) -- */
        ref_global imm2, ref_base       /* ppc:769 (imm0 = slot addr)      */
        ref_global imm1, oldspace_dnode_count   /* ppc:770                 */
        sub imm0, imm0, imm2            /* ppc:771                         */
        mov imm3, #0x8000000000000000   /* ppc:772                         */
        lsr imm0, imm0, #dnode_shift    /* ppc:773                         */
        cmp imm0, imm1                  /* ppc:774 cmplr                   */
        lsr imm2, imm0, #8              /* ppc:775                         */
        and imm4, imm0, #0x3f           /* ppc:776                         */
        lsr imm0, imm0, #bitmap_shift   /* ppc:777                         */
        lsr imm3, imm3, imm4            /* ppc:778                         */
        ref_global temp2, refbits       /* ppc:779                         */
        ref_global temp1, ephemeral_refidx      /* ppc:780                 */
        b.hs 8f                         /* ppc:781 bge (UNSIGNED)          */
        lsl imm0, imm0, #3              /* ppc:782                         */
        add temp0, temp2, imm0          /* [Xn] form (temp0 free)          */
2:      ldxr imm1, [temp0]              /* ppc:783                         */
        orr imm1, imm1, imm3
        stxr w17, imm1, [temp0]
        cbnz w17, 2b
        dmb ish                         /* ppc:787                         */
        mov imm3, #0x8000000000000000   /* ppc:788                         */
        and imm4, imm2, #0x3f           /* ppc:789                         */
        lsr imm2, imm2, #bitmap_shift   /* ppc:790                         */
        lsr imm3, imm3, imm4            /* ppc:791                         */
        lsl imm2, imm2, #3              /* ppc:792                         */
        add temp0, temp1, imm2
3:      ldxr imm1, [temp0]              /* ppc:793                         */
        orr imm1, imm1, imm3
        stxr w17, imm1, [temp0]
        cbnz w17, 3b
        dmb ish                         /* ppc:797                         */
        /* -- memoize the hash VECTOR itself (ppc:799-828) -- */
        ref_global temp1, refbits       /* ppc:800                         */
        ref_global imm1, ref_base       /* ppc:801                         */
        sub imm0, arg_x, imm1           /* ppc:802                         */
        lsr imm0, imm0, #dnode_shift    /* ppc:803                         */
        mov imm3, #0x8000000000000000   /* ppc:804                         */
        lsr imm2, imm0, #8              /* ppc:805                         */
        and imm4, imm0, #0x3f           /* ppc:806                         */
        lsr imm0, imm0, #bitmap_shift   /* ppc:807                         */
        lsr imm3, imm3, imm4            /* ppc:808                         */
        lsl imm0, imm0, #3              /* ppc:809                         */
        ldr imm1, [temp1, imm0]         /* ppc:810                         */
        tst imm1, imm3                  /* ppc:811 and.                    */
        b.ne 8f                         /* ppc:812                         */
        add temp0, temp1, imm0
4:      ldxr imm1, [temp0]              /* ppc:813                         */
        orr imm1, imm1, imm3
        stxr w17, imm1, [temp0]
        cbnz w17, 4b
        dmb ish                         /* ppc:817                         */
        ref_global temp1, ephemeral_refidx      /* ppc:818                 */
        mov imm3, #0x8000000000000000   /* ppc:819                         */
        and imm4, imm2, #0x3f           /* ppc:820                         */
        lsr imm2, imm2, #bitmap_shift   /* ppc:821                         */
        lsr imm3, imm3, imm4            /* ppc:822                         */
        lsl imm2, imm2, #3              /* ppc:823                         */
        add temp0, temp1, imm2
5:      ldxr imm1, [temp0]              /* ppc:824                         */
        orr imm1, imm1, imm3
        stxr w17, imm1, [temp0]
        cbnz w17, 5b
        dmb ish                         /* ppc:828                         */
        .globl C(egc_write_barrier_end)
C(egc_write_barrier_end):               /* ppc:829 (family END marker)     */
8:      add arg_z, rnil, #t_offset            /* success => T              */
        ret
9:      clrex                                 /* PPC strcx-to-RESERVATION_
                                                 DISCHARGE = discharge the
                                                 reservation; AArch64 has a
                                                 dedicated insn (our v2
                                                 arm64-spentry.s uses it) */
        mov arg_z, rnil                       /* failure => NIL            */
        ret
endsp set_hash_key_conditional

/* ===== conslist ===== */
/* ported from ppc-spentry.s:839-851 (PPC64 branch) */
spentry conslist
        mov arg_z, rnil                 /* li arg_z,nil_value -> rnil      */
        cmp nargs, #0
        b 2f
        /* Loop test POST-Cons: Matt's Cons macro clobbers NZCV via its
         * allocptr,allocbase compare (spentry-D:404 class) -- the old
         * pre-Cons cmp made b.ne test the ALLOCATION flags = infinite
         * loop (16m5n, first &key fn via keyword_bind). */
1:      ldr temp0, [vsp]
        add vsp, vsp, #node_size
        Cons arg_z, temp0, arg_z
        subs nargs, nargs, #(1<<fixnumshift)
2:      b.ne 1b
        ret
endsp conslist

/* ===== conslist_star ===== */
/* ported from ppc-spentry.s:855-866 (PPC64 branch) */
spentry conslist_star
        cmp nargs, #0
        b 2f
        /* Same post-Cons loop-test discipline as conslist above. */
1:      ldr temp0, [vsp]
        add vsp, vsp, #node_size
        Cons arg_z, temp0, arg_z
        subs nargs, nargs, #(1<<fixnumshift)
2:      b.ne 1b
        ret
endsp conslist_star

/* ===== stkconslist ===== */
/* ported from ppc-spentry.s:870-888 (PPC64 branch) */
spentry stkconslist
        mov arg_z, rnil                 /* ppc:871 li arg_z,nil_value (was
                                           wrongly the TAG constant)       */
        add imm1, nargs, nargs          /* ppc:873                         */
        add imm1, imm1, #tsp_frame.fixed_overhead  /* ppc:874              */
        TSP_Alloc_Var_Boxed imm1, imm2  /* ppc:875 (links+marks+ZEROES;
                                           PPC has no ts_area limit check
                                           here - drafter confusion)       */
        add imm1, tsp, #(tsp_frame.data_offset + fulltag_cons) /* ppc:876  */
        cmp nargs, #0                   /* ppc:872 cmpri cr1 - recomputed
                                           AFTER the alloc (macro clobbers
                                           NZCV)                           */
        b 2f
1:      ldr temp0, [vsp]
        cmp nargs, #(1<<fixnumshift)
        add vsp, vsp, #node_size
        /* _rplaca/_rplacd: PPC64 881-882 */
        str temp0, [imm1, #cons.car]
        str arg_z, [imm1, #cons.cdr]
        mov arg_z, imm1
        add imm1, imm1, #cons.size
        sub nargs, nargs, #(1<<fixnumshift)
2:      b.ne 1b
        ret
endsp stkconslist

/* ===== stkconslist_star ===== */
/* ported from ppc-spentry.s:892-909 (PPC64 branch) */
spentry stkconslist_star
        add imm1, nargs, nargs          /* ppc:894                         */
        add imm1, imm1, #tsp_frame.fixed_overhead  /* ppc:895              */
        TSP_Alloc_Var_Boxed imm1, imm2  /* ppc:896                         */
        add imm1, tsp, #(tsp_frame.data_offset + fulltag_cons) /* ppc:897  */
        cmp nargs, #0                   /* ppc:893 cmpri cr1 (post-alloc)  */
        b 2f
1:      ldr temp0, [vsp]
        cmp nargs, #(1<<fixnumshift)
        add vsp, vsp, #node_size
        str temp0, [imm1, #cons.car]
        str arg_z, [imm1, #cons.cdr]
        mov arg_z, imm1
        add imm1, imm1, #cons.size
        sub nargs, nargs, #(1<<fixnumshift)
2:      b.ne 1b
        ret
endsp stkconslist_star

/* ===== mkstackv ===== */
/* ported from ppc-spentry.s:914-933 (PPC64 branch) */
spentry mkstackv
        cmp nargs, #0
        /* dnode_align + TSP_Alloc_Var_Boxed_nz: PPC64 916-917.  fixed_overhead
           is a dnode multiple, so folding it into the round-up delta with the
           header word (node_size) is exact.  Data area is always >= one dnode
           (header + pad), so the _nz form's do-while zero is safe. */
        dnode_align imm1, nargs, (node_size + tsp_frame.fixed_overhead)
        TSP_Alloc_Var_Boxed_nz imm1, imm2  /* ppc:917 */
        lsl imm0, nargs, #(num_subtag_bits - fixnumshift)
        mov temp0, #subtag_simple_vector    /* not a valid logical-imm:    */
        orr imm0, imm0, temp0               /* materialize, then orr       */
        str imm0, [tsp, #tsp_frame.data_offset]      /* store header (data_offset=16, was mis-guessed 8) */
        add arg_z, tsp, #(tsp_frame.data_offset + fulltag_misc)
        cmp nargs, #0                   /* ppc:915 cr0 (post-alloc)        */
        b.eq 2f
        add imm0, arg_z, #misc_data_offset
        add imm1, imm0, nargs
1:      sub nargs, nargs, #node_size
        cmp nargs, #0
        ldr temp1, [vsp]
        add vsp, vsp, #node_size
        str temp1, [imm1, #-node_size]!
        b.ne 1b
2:      ret
endsp mkstackv

/* ===== progvsave ===== */
/* ported from ppc-spentry.s:949-1019 (PPC64 branch) - ~70 lines */
spentry progvsave
        /* Error unless arg_z is a proper list (Floyd; ppc:953-969).  All
           nil tests compare against rnil (the VALUE - the old #fulltag_nil
           comparands were the TAG, never equal to a pointer).
           ARM64-DEVIATION: PPC's trap_unless_list passes nil (nil is
           list-tagged there); nil has its OWN fulltag here, so the
           nil check is hoisted BEFORE each cons-tag check.  BOTH of them
           (16m48): the hoist was originally applied only to the temp2 =
           cdr(fast) check, and fast lands exactly ON nil whenever the list
           has EVEN length -- fast advances two conses per iteration and so
           steps over the last cons of an even list.  A missing nil test at
           the loop top therefore made every even-length values list report
           XIMPROPERLIST, which is PROGV.8 and MISC.299/301/305/644.  PPC
           needs no such test because cdr(nil) reads back as nil there. */
        cmp arg_z, rnil                 /* ppc:953                         */
        mov arg_x, arg_z                /* ppc:954 fast                    */
        mov temp1, arg_z                /* ppc:955 slow                    */
        b.eq 9f                         /* ppc:956 null list is proper     */
0:      cmp arg_x, rnil                 /* fast ran off the end => proper  */
        b.eq 9f                         /*   (ppc: trap_unless_list(nil) ok)*/
        and imm0, arg_x, #fulltagmask   /* ppc:958 trap_unless_list(fast)  */
        cmp imm0, #fulltag_cons
        b.ne progvsave_improper
        ldr temp2, [arg_x, #cons.cdr]   /* ppc:959 cdr(fast)               */
        cmp temp2, rnil                 /* ppc:960 cmpri cr3               */
        b.eq 9f                         /* ppc:963 (hoisted: see header)   */
        and imm0, temp2, #fulltagmask   /* ppc:961 trap_unless_list        */
        cmp imm0, #fulltag_cons
        b.ne progvsave_improper
        ldr arg_x, [temp2, #cons.cdr]   /* ppc:962 cdr(cdr(fast))          */
        ldr temp1, [temp1, #cons.cdr]   /* ppc:964 cdr(slow)               */
        cmp arg_x, temp1                /* ppc:965                         */
        b.ne 0b                         /* ppc:966                         */
progvsave_improper:                     /* circular or non-list            */
        mov arg_y, #XIMPROPERLIST       /* ppc:967 (errors.s:223)          */
        mov nargs, #(2<<fixnumshift)    /* ppc:968                         */
        b _SPksignalerr                 /* ppc:969                         */
9:      /* Length of arg_y (a proper list); imm0 = boxed count (ppc:974-980) */
        mov imm0, #(-node_size)
        mov arg_x, arg_y
1:      cmp arg_x, rnil                 /* ppc:977                         */
        add imm0, imm0, #node_size      /* ppc:978                         */
        ldr arg_x, [arg_x, #cons.cdr]   /* ppc:979 (cdr of nil is read but
                                           discarded - loop exits on Z)    */
        b.ne 1b                         /* ppc:980                         */
        cmp imm0, #0                    /* ppc:984                         */
        add imm1, imm0, imm0            /* ppc:985                         */
        add imm1, imm1, imm0            /* ppc:986 3*count*node_size       */
        add imm1, imm1, #(dnode_size + node_size - 1)   /* ppc:987         */
        and imm1, imm1, #(~(dnode_size - 1))            /* dnode_align     */
        b.ne 2f                         /* ppc:988                         */
        /* count 0: empty boxed frame (ppc:989 TSP_Alloc_Fixed_Boxed(16)).
           The macro zeroes both data words, so the count(=0) store is subsumed. */
        TSP_Alloc_Fixed_Boxed 2*node_size, imm2
        ret                             /* ppc:990                         */
2:      add imm1, imm1, #tsp_frame.fixed_overhead       /* ppc:992         */
        TSP_Alloc_Var_Boxed imm1, imm2  /* ppc:993 (zeroes; clobbers NZCV) */
        str imm0, [tsp, #tsp_frame.data_offset]         /* ppc:994 count   */
        ldr imm2, [tsp, #tsp_frame.backlink]            /* ppc:995 cursor
                                           = frame end (triplets push down)*/
        mov arg_x, arg_y                /* ppc:996                         */
        ldr imm1, [rcontext, #tcr.db_link]              /* ppc:997         */
        ldr imm3, [rcontext, #tcr.tlb_limit]            /* ppc:998         */
3:      /* Binding loop (ppc:999-1017).  PPC keeps cr1 (arg_z nil) live
           from loop top; the trlle-trap cmp clobbers NZCV here, so the
           arg_z test is recomputed just before its branch. */
        ldr temp0, [arg_x, #cons.car]   /* ppc:1000 symbol                 */
        ldur imm0, [temp0, #symbol.binding_index]       /* ppc:1001 (=49)  */
        ldr arg_x, [arg_x, #cons.cdr]   /* ppc:1002                        */
        cmp imm3, imm0                  /* ppc:1003 trlle(imm3,imm0):      */
        b.hi 10f                        /*   trap if tlb_limit <= index    */
        uuo_error_tlb_too_small imm0      /*   (same code as spentry-C:202)  */
10:     ldr imm4, [rcontext, #tcr.tlb_pointer]  /* ppc:1004 reload post-trap */
        ldr temp3, [imm4, imm0]         /* ppc:1005 old value              */
        mov temp2, #unbound_marker      /* ppc:1007 (arm64-constants.h:169)*/
        cmp arg_z, rnil                 /* ppc:999 cmpri cr1 (recomputed)  */
        b.eq 4f                         /* ppc:1008 beq cr1                */
        ldr temp2, [arg_z, #cons.car]   /* ppc:1009 new value              */
        ldr arg_z, [arg_z, #cons.cdr]   /* ppc:1010                        */
4:      /* triplet: (old-value, binding-index, db-link) pushed downward */
        str temp3, [imm2, #-node_size]! /* ppc:1011                        */
        str imm0, [imm2, #-node_size]!  /* ppc:1012                        */
        str imm1, [imm2, #-node_size]!  /* ppc:1013                        */
        str temp2, [imm4, imm0]         /* ppc:1014 install new value      */
        mov imm1, imm2                  /* ppc:1015                        */
        cmp arg_x, rnil                 /* ppc:1006 cmpri cr0 (recomputed) */
        b.ne 3b                         /* ppc:1016                        */
        str imm2, [rcontext, #tcr.db_link]              /* ppc:1017        */
        ret                             /* ppc:1018                        */
endsp progvsave

/* ===== gvector ===== */
/* ported from ppc-spentry.s:1125-1149 (PPC64 branch).  Caller vpushes the
 * boxed subtype first, then the elements in order; nargs = byte-scaled
 * (count+1)*node_size counting the subtype (boot-16m5: observed live call
 * site vpush x4 + mov nargs,#0x20).  Result (fulltag_misc) in arg_z. */
spentry gvector
        sub nargs, nargs, #node_size
        ldr arg_z, [vsp, nargs]               /* boxed subtype (deepest)  */
        asr imm0, arg_z, #fixnumshift         /* unbox_fixnum(imm0,arg_z) */
        lsl imm1, nargs, #(num_subtag_bits - fixnumshift)
        orr imm0, imm0, imm1                  /* header = count<<8 | subtag */
        add imm1, nargs, #(node_size + (dnode_size - 1))
        and imm1, imm1, #~(dnode_size - 1)    /* dnode_align(nargs+node_size) */
        Misc_Alloc arg_z, imm0, imm1
        mov imm1, nargs
        mov imm2, #misc_data_offset           /* negative; keep out of add-imm */
        add imm2, imm1, imm2
        b 2f
1:      str temp0, [arg_z, imm2]
2:      sub imm1, imm1, #node_size
        cmp imm1, #0
        sub imm2, imm2, #node_size
        ldr temp0, [vsp], #node_size          /* vpop; fencepost pops subtype too */
        b.ge 1b
        ret
endsp gvector

/* ===== misc_ref ===== */
/* ported from ppc-spentry.s:2405-3203 (PPC64 branch) - ~450 lines with dispatch + type handlers */
spentry misc_ref
        /* Validate fulltag misc and fixnum index */
        and imm0, arg_y, #fulltagmask
        cmp imm0, #fulltag_misc
        b.ne misc_ref_invalid
        and imm0, arg_z, #fixnummask
        cbnz imm0, misc_ref_invalid
        /* Bounds check */
        ldr imm0, [arg_y, #misc_header_offset]
        lsr imm1, imm0, #num_subtag_bits
        lsl imm1, imm1, #fixnumshift
        cmp arg_z, imm1
        b.ge misc_ref_invalid
        /* Extract subtag */
        and imm1, imm0, #subtagmask
misc_ref_common:
        /* Compare-chain dispatch (subtag values from arm64-constants.h).
           All node uvectors first: fulltag_nodeheader_0 (6) and _1 (0xe)
           share low-3-bits #b110, and PPC64's jump table routes EVERY
           real nodeheader subtag to the plain node read (ppc:2405ff) —
           the per-subtag chain missed catch-frame/hash-vector/slot-vector/
           lock/instance/istruct/… (boot-16m5b sibling sweep). */
        and imm2, imm1, #7
        cmp imm2, #6                    /* fulltag_nodeheader_{0,1} & 7 */
        b.eq misc_ref_node
        cmp imm1, #subtag_u8_vector
        b.eq misc_ref_u8
        cmp imm1, #subtag_s8_vector
        b.eq misc_ref_s8
        cmp imm1, #subtag_u16_vector
        b.eq misc_ref_u16
        cmp imm1, #subtag_s16_vector
        b.eq misc_ref_s16
        cmp imm1, #subtag_u32_vector
        b.eq misc_ref_u32
        cmp imm1, #subtag_s32_vector
        b.eq misc_ref_s32
        cmp imm1, #subtag_u64_vector
        b.eq misc_ref_u64
        cmp imm1, #subtag_s64_vector
        b.eq misc_ref_s64
        cmp imm1, #subtag_fixnum_vector
        b.eq misc_ref_fixnum_vector
        /* Float vectors (16m37).  These were absent from BOTH this chain and
           misc_set_common's, so uvref/uvset on a single- or double-float
           vector fell through to misc_ref_invalid.  PPC64 routes all four
           float-vector subtags (ppc:2620/2641/2658/2616). */
        cmp imm1, #subtag_single_float_vector
        b.eq misc_ref_single_float_vector
        cmp imm1, #subtag_double_float_vector
        b.eq misc_ref_double_float_vector
        /* COMPLEX float vectors (16m41): 16m37 added only the two real float
           subtags, so :initial-contents on a (complex single-float) or
           (complex double-float) array still fell through to
           misc_ref_invalid -- regression stage 11, EVERY.32. */
        cmp imm1, #subtag_complex_single_float_vector
        b.eq misc_ref_complex_single_float_vector
        cmp imm1, #subtag_complex_double_float_vector
        b.eq misc_ref_complex_double_float_vector
        cmp imm1, #subtag_simple_base_string
        b.eq misc_ref_string
        cmp imm1, #subtag_bit_vector
        b.eq misc_ref_bit_vector
        cmp imm1, #subtag_code_vector
        b.eq misc_ref_u32
        cmp imm1, #subtag_bignum
        b.eq misc_ref_u32
        /* PPC64 jump table (ppc:2454/2471): macptr + dead_macptr read as
           raw 64-bit words; double-float/xcode-vector as 2×u32. */
        cmp imm1, #subtag_macptr
        b.eq misc_ref_u64
        cmp imm1, #subtag_dead_macptr
        b.eq misc_ref_u64
        cmp imm1, #subtag_double_float
        b.eq misc_ref_u32
        cmp imm1, #subtag_xcode_vector
        b.eq misc_ref_u32
        b misc_ref_invalid
misc_ref_node:
        add imm0, arg_y, arg_z
        ldr arg_z, [imm0, #misc_data_offset]
        ret
misc_ref_u8:
        lsr imm0, arg_z, #fixnumshift
        add imm2, arg_y, #misc_data_offset
        ldrb w0, [imm2, imm0]
        lsl arg_z, x0, #fixnumshift
        ret
misc_ref_s8:
        lsr imm0, arg_z, #fixnumshift
        add imm2, arg_y, #misc_data_offset
        ldrsb x0, [imm2, imm0]
        lsl arg_z, x0, #fixnumshift
        ret
misc_ref_u16:
        lsr imm0, arg_z, #fixnumshift
        lsl imm0, imm0, #1
        add imm2, arg_y, #misc_data_offset
        ldrh w0, [imm2, imm0]
        lsl arg_z, x0, #fixnumshift
        ret
misc_ref_s16:
        lsr imm0, arg_z, #fixnumshift
        lsl imm0, imm0, #1
        add imm2, arg_y, #misc_data_offset
        ldrsh x0, [imm2, imm0]
        lsl arg_z, x0, #fixnumshift
        ret
misc_ref_u32:
        lsr imm0, arg_z, #fixnumshift
        lsl imm0, imm0, #2
        add imm2, arg_y, #misc_data_offset
        ldr w0, [imm2, imm0]
        lsl arg_z, x0, #fixnumshift
        ret
misc_ref_s32:
        lsr imm0, arg_z, #fixnumshift
        lsl imm0, imm0, #2
        add imm2, arg_y, #misc_data_offset
        ldrsw x0, [imm2, imm0]
        lsl arg_z, x0, #fixnumshift
        ret
misc_ref_u64:
        add imm0, arg_y, arg_z
        ldr imm0, [imm0, #misc_data_offset]
        b _SPmakeu64
misc_ref_s64:
        add imm0, arg_y, arg_z
        ldr imm0, [imm0, #misc_data_offset]
        b _SPmakes64
misc_ref_fixnum_vector:
        add imm0, arg_y, arg_z
        ldr imm0, [imm0, #misc_data_offset]
        lsl arg_z, imm0, #fixnumshift
        ret
misc_ref_string:
        /* 32-bit chars (see misc_set_string); PPC64 misc_ref_new_string. */
        lsr imm0, arg_z, #1             /* boxed idx -> idx*4              */
        add imm2, arg_y, #misc_data_offset
        ldr w0, [imm2, imm0]
        lsl imm0, x0, #charcode_shift
        orr arg_z, imm0, #subtag_character
        ret
misc_ref_bit_vector:
        /* ARM64 LSB0 bit order */
        lsr imm0, arg_z, #fixnumshift
        lsr imm2, imm0, #5
        lsl imm2, imm2, #2
        add imm2, arg_y, imm2
        ldr w3, [imm2, #misc_data_offset]
        and imm1, imm0, #31
        lsr w3, w3, w1
        and w3, w3, #1
        lsl arg_z, x3, #fixnumshift
        ret
misc_ref_single_float_vector:
        /* ppc:2757-2762.  32-bit elements, so the same index math as
           misc_ref_u32: boxed idx >> fixnumshift, then << 2. */
        lsr imm0, arg_z, #fixnumshift
        lsl imm0, imm0, #2
        add imm2, arg_y, #misc_data_offset
        ldr w0, [imm2, imm0]
        /* ppc:2761-2762 (rldicr 32,31 + ori).  Single-floats are IMMEDIATE on
           arm64, the raw IEEE bits riding the high 32 with the tag in the low
           byte.  NB the tag spelling: arm64-arch.lisp:83 defines
           subtag-single-float AS fulltag-single-float, but that alias is
           Lisp-side only -- arm64-constants.h defines fulltag_single_float and
           has no subtag_ name, so #subtag_single_float does not assemble. */
        lsl arg_z, x0, #32
        orr arg_z, arg_z, #fulltag_single_float
        ret
misc_ref_double_float_vector:
        /* ppc:2700-2705.  64-bit elements: fixnumshift == word_shift == 3, so
           the boxed index IS the byte offset, exactly as in misc_ref_u64. */
        add imm0, arg_y, arg_z
        ldr imm0, [imm0, #misc_data_offset]
        /* Unlike PPC, arm64-constants.h defines no double_float_header, so
           build it here -- but the count is a LITERAL 2, not
           double_float.element_count.  _endstructf derives element_count as
           (size - header) / NODE_SIZE, and a header count is in the units of
           the object's IVECTOR CLASS: double_float is ivector_class_32_bit
           with an 8-byte payload, so the count is 2 thirty-two-bit elements
           and the node-derived value is 1.  Both 64-bit reference ports
           hardcode the literal for exactly this reason
           (ppc-constants64.s:362, x86-constants64.s:691
           def_header(double_float_header,2,...)); only the 32-bit ports
           derive it, where node_size == the element size.  Deriving it here
           made every kernel-boxed double claim one element, so (uvref d 1)
           was out of bounds: DOUBLE-FLOAT-BITS -- on every float print path
           -- signalled $XARROOB, and EQL against a Lisp-boxed double was
           false because the two headers disagreed (16m45).
           imm0 must survive Misc_Alloc_Fixed (including a uuo_alloc trip
           through the allocator) -- PPC relies on exactly that, ppc:2701-2704. */
        mov imm1, #((2 << num_subtag_bits) | subtag_double_float)
        Misc_Alloc_Fixed arg_z, imm1, double_float.size
        str imm0, [arg_z, #double_float.value]
        ret
misc_ref_complex_single_float_vector:
        /* 16m41.  Vector element = 2 packed singles = 8 bytes, and the subtag
           is in ivector_class_64_bit, so the boxed index IS the byte offset
           (fixnumshift == 3), exactly as misc_ref_double_float_vector.  The
           SCALAR complex_single_float is {realpart:4, imagpart:4}
           (arm64-constants.h:344-347), i.e. the same 8-byte word, so one load
           and one store carry both parts.
           imm0 must survive Misc_Alloc_Fixed with the header in imm2 -- the
           makeu128 precedent (spentry-A:167-176) depends on exactly that. */
        add imm0, arg_y, arg_z
        ldr imm0, [imm0, #misc_data_offset]
        /* Literal 2, not complex_single_float.element_count: ivector_class_32_bit
           over an 8-byte payload, so the count is 2 thirty-two-bit elements
           where _endstructf's node-derived value is 1.  x8664 canon:
           setup-complex-single-float-allocation, (make-vheader 2 ...),
           x8664-vinsns:2527 -- and our own complex-single-float->heap vinsn
           already uses the literal.  See the note at
           misc_ref_double_float_vector. */
        mov imm2, #((2 << num_subtag_bits) | subtag_complex_single_float)
        Misc_Alloc_Fixed arg_z, imm2, complex_single_float.size
        str imm0, [arg_z, #complex_single_float.realpart]
        ret
misc_ref_complex_double_float_vector:
        /* 16m41.  Vector element = 2 doubles = 16 bytes; the subtag is in
           ivector_class_other_bit, so compute the offset: 16i = boxed<<1.
           16m48: the note here used to say "vector data starts right after
           the header (no x8664-style pad)".  That is FALSE and it is the
           MAKE-SEQUENCE.30 / SUBSEQ.SPECIALIZED-VECTOR.3 bug -- Matt's own
           arm64-arch.lisp:259-261 declares the pad, and every LISP-side
           writer already honours it.  See misc_complex_dfloat_offset above.
           The SCALAR complex_double_float carries its own pad
           (arm64-constants.h:349-353: {pad, realpart, imagpart}), so the
           store side still uses .realpart. */
        lsl imm3, arg_z, #1
        add imm3, imm3, arg_y
        add imm3, imm3, #misc_complex_dfloat_offset
        ldp imm0, imm1, [imm3]
        /* Literal 6, not complex_double_float.element_count: ivector_class_32_bit
           over a 24-byte payload {pad, realpart, imagpart}, so 6 thirty-two-bit
           elements where the node-derived value is 3.  Worse than the other two
           here: an under-count of 3 makes the GC size this 32-byte object at 24
           (8 + (3<<2), dnode-rounded), so a heap walk would resume INSIDE it.
           x8664 canon: (make-vheader 6 ...), x8664-vinsns:2522 /
           def_header(complex_double_float_header,6,...) in both 64-bit
           constants files; our complex-double-float->heap vinsn already uses
           the literal.  See the note at misc_ref_double_float_vector. */
        mov imm2, #((6 << num_subtag_bits) | subtag_complex_double_float)
        Misc_Alloc_Fixed arg_z, imm2, complex_double_float.size
        /* stur, not stp: _structf offsets are tag-biased (realpart = header +
           pad - fulltag_misc), so the immediate is not a multiple of 8 and
           ldp/stp -- which have no unscaled form -- will not assemble.  Same
           reason the rest of this file reaches tagged slots with ldur/stur. */
        stur imm0, [arg_z, #complex_double_float.realpart]
        stur imm1, [arg_z, #(complex_double_float.realpart + 8)]
        ret
misc_ref_invalid:
        mov arg_x, #XBADVEC             /* errors.s:177 deferr           */
        mov nargs, #(3<<fixnumshift)
        b _SPksignalerr
endsp misc_ref

/* ===== subtag_misc_ref ===== */
/* ported from ppc-spentry.s:3205-3224 (PPC64 branch) */
spentry subtag_misc_ref
        and imm0, arg_y, #fulltagmask
        cmp imm0, #fulltag_misc
        b.ne 1f
        and imm0, arg_z, #fixnummask
        cbnz imm0, 1f
        ldr imm0, [arg_y, #misc_header_offset]
        lsr imm1, imm0, #num_subtag_bits
        lsl imm1, imm1, #fixnumshift
        cmp arg_z, imm1
        b.ge 1f
        asr imm1, arg_x, #fixnumshift         /* unbox_fixnum(imm1,arg_x) = subtag override */
        b misc_ref_common
1:      mov arg_x, #XBADVEC             /* errors.s:177 deferr           */
        mov nargs, #(3<<fixnumshift)
        b _SPksignalerr
endsp subtag_misc_ref

/* ===== stkconsyz ===== */
/* ported from ppc-spentry.s:3226-3241 (PPC64 branch) */
spentry stkconsyz
        mov imm0, rnil                  /* li imm0,nil_value -> rnil       */
        str imm0, [vsp, #-node_size]!         /* vpush(imm0) */
        str imm0, [vsp, #-node_size]!
        str imm0, [vsp, #-node_size]!
        and imm0, vsp, #(1<<node_shift)       /* Check alignment */
        cbz imm0, 1f
        str arg_y, [vsp, #(node_size*2)]
        str arg_z, [vsp, #node_size]
        add arg_z, vsp, #(fulltag_cons + node_size)
        ret
1:      str arg_y, [vsp, #node_size]
        str arg_z, [vsp]
        add arg_z, vsp, #fulltag_cons
        ret
endsp stkconsyz

/* ===== stkgvector ===== */
/* ported from ppc-spentry.s:3393-3420 (PPC64 branch) - ~30 lines */
spentry stkgvector
        sub imm0, nargs, #(1<<fixnumshift)
        add imm1, vsp, nargs
        ldr temp0, [imm1, #-node_size]!          /* pop subtag from stack */
        lsl imm2, imm0, #(num_subtag_bits - fixnumshift)  /* element_count << num_subtag_bits (PPC slri = shift LEFT; the earlier lsr right-shifted the count into the low byte -> header count field always 0 -> malformed stack closures overflowed the vstack in _SPcall_closure) */
        asr imm3, temp0, #fixnumshift            /* unbox subtag */
        orr imm2, imm3, imm2                     /* header = (element_count << num_subtag_bits) | subtag */
        dnode_align imm0, imm0, (node_size + tsp_frame.fixed_overhead)
        /* Push a boxed frame of imm0 bytes (built below the live tsp and
           published atomically).  "_nz": imm0 always covers frame overhead +
           object header, so the data area is never empty.  (An earlier bare
           `sub tsp' dropped the backlink and fed tsp:=0 into a later
           TSP_Unlink -- 16m5k wall, gdb-observed 2026-07-17.) */
        TSP_Alloc_Var_Boxed_nz imm0, imm4
        str imm2, [tsp, #tsp_frame.data_offset]  /* store header (data_offset=16) */
        add arg_z, tsp, #(tsp_frame.data_offset + fulltag_misc)
        add imm3, arg_z, #misc_header_offset     /* pointer to header area for data copy */
        mov imm0, #(1<<fixnumshift)
        cmp imm0, nargs                          /* re-derive the entry test (nargs==fixnum 1 => no elements); the old cmp-at-top flags don't survive the zero loop */
        b 2f
1:      /* Copy loop */
        add imm0, imm0, #(1<<fixnumshift)
        cmp imm0, nargs
        ldr temp0, [imm1, #-node_size]!
        str temp0, [imm3, #node_size]!
2:      b.ne 1b
        add vsp, vsp, nargs
        ret
endsp stkgvector

/* ===== subtag_misc_set ===== */
/* ported from ppc-spentry.s:3907-4871 (PPC64 branch) */
spentry subtag_misc_set
        and imm0, arg_x, #fulltagmask
        cmp imm0, #fulltag_misc
        b.ne 1f
        and imm0, arg_y, #fixnummask
        cbnz imm0, 1f
        ldr imm0, [arg_x, #misc_header_offset]
        lsr imm1, imm0, #num_subtag_bits
        lsl imm1, imm1, #fixnumshift
        cmp arg_y, imm1
        b.ge 1f
        asr imm1, temp0, #fixnumshift         /* unbox subtag override from temp0 */
        b misc_set_common
1:      mov arg_w, #XBADVEC             /* errors.s:177 deferr           */
        mov nargs, #(4<<fixnumshift)
        b _SPksignalerr
endsp subtag_misc_set

/* ===== misc_set ===== */
/* ported from ppc-spentry.s:4873-6950 (PPC64 branch) - ~500 lines */
spentry misc_set
        and imm0, arg_x, #fulltagmask
        cmp imm0, #fulltag_misc
        b.ne misc_set_invalid
        and imm0, arg_y, #fixnummask
        cbnz imm0, misc_set_invalid
        ldr imm0, [arg_x, #misc_header_offset]
        lsr imm1, imm0, #num_subtag_bits
        lsl imm1, imm1, #fixnumshift
        cmp arg_y, imm1
        b.ge misc_set_invalid
        and imm1, imm0, #subtagmask
misc_set_common:
        /* Node vectors -> delegate to gvset for write barrier.  Class
           test, not per-subtag: nodeheader_{0,1} share low-3-bits #b110
           and PPC64's table routes every real nodeheader subtag to gvset
           (ppc:3921ff) — the chain missed catch-frame/hash-vector/
           slot-vector/lock/instance/istruct/… (boot-16m5b). */
        and imm2, imm1, #7
        cmp imm2, #6                    /* fulltag_nodeheader_{0,1} & 7 */
        b.eq _SPgvset
        /* Integer vectors */
        cmp imm1, #subtag_u8_vector
        b.eq misc_set_u8
        cmp imm1, #subtag_s8_vector
        b.eq misc_set_s8
        cmp imm1, #subtag_u16_vector
        b.eq misc_set_u16
        cmp imm1, #subtag_s16_vector
        b.eq misc_set_s16
        cmp imm1, #subtag_u32_vector
        b.eq misc_set_u32
        cmp imm1, #subtag_s32_vector
        b.eq misc_set_s32
        cmp imm1, #subtag_u64_vector
        b.eq misc_set_u64
        cmp imm1, #subtag_s64_vector
        b.eq misc_set_s64
        cmp imm1, #subtag_fixnum_vector
        b.eq misc_set_fixnum_vector
        /* Float vectors (16m37) -- parity twin of the misc_ref_common
           addition; both sides were missing all four float-vector subtags. */
        cmp imm1, #subtag_single_float_vector
        b.eq misc_set_single_float_vector
        cmp imm1, #subtag_double_float_vector
        b.eq misc_set_double_float_vector
        /* COMPLEX float vectors (16m41) -- parity twin of the misc_ref_common
           addition; this is the side EVERY.32 actually reached, via
           :initial-contents -> init-uvector-contents -> uvset. */
        cmp imm1, #subtag_complex_single_float_vector
        b.eq misc_set_complex_single_float_vector
        cmp imm1, #subtag_complex_double_float_vector
        b.eq misc_set_complex_double_float_vector
        cmp imm1, #subtag_simple_base_string
        b.eq misc_set_string
        cmp imm1, #subtag_bit_vector
        b.eq misc_set_bit_vector
        cmp imm1, #subtag_code_vector
        b.eq misc_set_u32
        cmp imm1, #subtag_bignum
        b.eq misc_set_u32
        /* PPC64 jump table (ppc:3954/3971): macptr + dead_macptr store as
           raw 64-bit words (cold load does misc_set(macptr,0,0) to null
           the address — boot-16m5b wall); double-float/xcode-vector as
           2×u32. */
        cmp imm1, #subtag_macptr
        b.eq misc_set_u64
        cmp imm1, #subtag_dead_macptr
        b.eq misc_set_u64
        cmp imm1, #subtag_double_float
        b.eq misc_set_u32
        cmp imm1, #subtag_xcode_vector
        b.eq misc_set_u32
        b misc_set_invalid
misc_set_u8:
        and imm0, arg_z, #fixnummask
        cbnz imm0, misc_set_bad
        lsr imm0, arg_z, #fixnumshift
        cmp imm0, #256
        b.hs misc_set_bad
        lsr imm4, arg_y, #fixnumshift   /* ppc:4297 idx                    */
        add imm2, arg_x, #misc_data_offset
        strb w0, [imm2, imm4]           /* ppc:4301 stbx (was [imm4,imm4]) */
        ret
misc_set_s8:
        and imm2, arg_z, #fixnummask
        cbnz imm2, misc_set_bad
        asr imm0, arg_z, #fixnumshift
        sxtb imm1, w0
        cmp x0, x1
        b.ne misc_set_bad
        lsr imm4, arg_y, #fixnumshift   /* ppc:4286 idx                    */
        add imm2, arg_x, #misc_data_offset
        strb w0, [imm2, imm4]           /* ppc:4293 stbx (was [imm4,imm4]) */
        ret
misc_set_u16:
        and imm0, arg_z, #fixnummask
        cbnz imm0, misc_set_bad
        lsr imm0, arg_z, #fixnumshift
        cmp imm0, #65536
        b.hs misc_set_bad
        lsr imm1, arg_y, #fixnumshift   /* ppc:4266 idx                    */
        lsl imm1, imm1, #1              /* *2 bytes                        */
        add imm2, arg_x, #misc_data_offset
        strh w0, [imm2, imm1]           /* (index was clobbered; store hit
                                           element 0 for every index)      */
        ret
misc_set_s16:
        and imm2, arg_z, #fixnummask
        cbnz imm2, misc_set_bad
        asr imm0, arg_z, #fixnumshift
        sxth imm1, w0
        cmp x0, x1
        b.ne misc_set_bad
        lsr imm1, arg_y, #fixnumshift
        lsl imm1, imm1, #1
        add imm2, arg_x, #misc_data_offset
        strh w0, [imm2, imm1]
        ret
misc_set_u32:
        /* ppc:4256-4263.  extract_unsigned_byte_bits_(imm0,arg_z,32): on
           a 64-bit target every (unsigned-byte 32) IS a fixnum, so a
           non-fixnum is simply bad - there is no bignum arm (the old
           old guard had a wrong premise). */
        and imm2, arg_z, #fixnummask
        cbnz imm2, misc_set_bad
        asr imm1, arg_z, #fixnumshift
        lsr imm2, imm1, #32             /* sign or high bits => not u32    */
        cbnz imm2, misc_set_bad
        lsr imm4, arg_y, #1             /* ppc:4258 boxed idx -> idx*4     */
        add imm2, arg_x, #misc_data_offset
        str w1, [imm2, imm4]            /* ppc:4262 stwx                   */
        ret
misc_set_s32:
        /* ppc:4243-4255; fixnum-only for the same reason as u32. */
        and imm2, arg_z, #fixnummask
        cbnz imm2, misc_set_bad
        asr imm0, arg_z, #fixnumshift
        sxtw imm1, w0                   /* ppc:4248-4249 sign-extend probe */
        cmp x0, x1
        b.ne misc_set_bad
        lsr imm4, arg_y, #1             /* boxed idx -> idx*4              */
        add imm2, arg_x, #misc_data_offset
        str w0, [imm2, imm4]            /* ppc:4254 stwx                   */
        ret
misc_set_u64:
        /* ppc:4303-4332.  Value > most-positive-fixnum arrives as a 2- or
           3-digit bignum.  ARM64-DEVIATION: PPC64 rotldi-swaps the two
           32-bit digits after the 64-bit load (big-endian); little-endian
           reads digit1:digit0 = the value directly - no rotate. */
        and imm0, arg_z, #fixnummask
        cbnz imm0, setu64_maybe_bignum  /* ppc:4310                        */
        asr imm0, arg_z, #fixnumshift   /* ppc:4311                        */
        tbnz imm0, #63, misc_set_bad    /* ppc:4312 blt (negative fixnum)  */
2:      add imm4, arg_x, arg_y
        str imm0, [imm4, #misc_data_offset]     /* ppc:4313 stdx           */
        ret
setu64_maybe_bignum:                    /* ppc:4315-4332                   */
        and imm2, arg_z, #fulltagmask
        cmp imm2, #fulltag_misc         /* ppc:4308/4316                   */
        b.ne misc_set_bad
        ldur imm1, [arg_z, #misc_header_offset] /* ppc:4317 getvheader     */
        ldur imm0, [arg_z, #misc_data_offset]   /* ppc:4318 (no rotldi)    */
        mov imm3, #two_digit_bignum_header      /* ppc:4320                */
        cmp imm1, imm3
        b.eq 3f
        mov imm3, #three_digit_bignum_header    /* ppc:4321                */
        cmp imm1, imm3
        b.ne misc_set_bad               /* ppc:4324                        */
        ldur w3, [arg_z, #(misc_data_offset+8)] /* ppc:4325 third digit    */
        cbnz w3, misc_set_bad           /* ppc:4326-4327 must be sign 0    */
        b 2b                            /* ppc:4328 store                  */
3:      tbnz imm0, #63, misc_set_bad    /* ppc:4330 2-digit must be >= 0   */
        b 2b                            /* ppc:4331 store                  */
misc_set_s64:
        /* ppc:4369-4387; bignum arm = exactly a 2-digit bignum (LE: no
           rotldi, see misc_set_u64). */
        and imm2, arg_z, #fixnummask
        cbnz imm2, sets64_maybe_bignum  /* ppc:4376                        */
        asr imm0, arg_z, #fixnumshift   /* ppc:4372                        */
2:      add imm4, arg_x, arg_y
        str imm0, [imm4, #misc_data_offset]     /* ppc:4377 stdx           */
        ret
sets64_maybe_bignum:                    /* ppc:4379-4387                   */
        and imm3, arg_z, #fulltagmask
        cmp imm3, #fulltag_misc         /* ppc:4374/4380                   */
        b.ne misc_set_bad
        ldur imm1, [arg_z, #misc_header_offset] /* ppc:4381 getvheader     */
        ldur imm0, [arg_z, #misc_data_offset]   /* ppc:4382 (no rotldi)    */
        mov imm3, #two_digit_bignum_header      /* ppc:4383                */
        cmp imm1, imm3
        b.ne misc_set_bad               /* ppc:4385                        */
        b 2b                            /* ppc:4386 store                  */
misc_set_fixnum_vector:
        and imm2, arg_z, #fixnummask
        cbnz imm2, misc_set_bad
        asr imm0, arg_z, #fixnumshift
        add imm4, arg_x, arg_y
        str imm0, [imm4, #misc_data_offset]
        ret
misc_set_string:
        /* ppc:4264-4272 misc_set_new_string: this design's strings are
           32-BIT chars (subtag_simple_base_string is ivector-class-32-bit)
           - the old byte-char body stored 1 byte at element 0 and masked
           the code to 8 bits.  Character check = full low byte. */
        and imm0, arg_z, #255           /* ppc:4265 extract_lowbyte        */
        cmp imm0, #subtag_character     /* ppc:4267                        */
        b.ne misc_set_bad
        lsr imm0, arg_z, #charcode_shift        /* ppc:4269 code           */
        lsr imm4, arg_y, #1             /* ppc:4266 boxed idx -> idx*4     */
        add imm2, arg_x, #misc_data_offset
        str w0, [imm2, imm4]            /* ppc:4271 stwx                   */
        ret
misc_set_bit_vector:
        /* ARM64 LSB0 bit order */
        cmp arg_z, #(1<<fixnumshift)
        b.hi misc_set_bad
        lsr imm0, arg_y, #fixnumshift
        lsr imm2, imm0, #5
        lsl imm2, imm2, #2
        add imm2, arg_x, imm2
        ldr w3, [imm2, #misc_data_offset]
        and imm1, imm0, #31
        mov w4, #1
        lsl w4, w4, w1
        bic w3, w3, w4
        lsr imm0, arg_z, #fixnumshift
        lsl w0, w0, w1
        orr w3, w3, w0
        str w3, [imm2, #misc_data_offset]
        ret
misc_set_single_float_vector:
        /* ppc:4234-4241.  arg_x=vector arg_y=boxed index arg_z=value. */
        and imm3, arg_z, #fulltagmask
        cmp imm3, #fulltag_single_float  /* see misc_ref_single_float_vector  */
        b.ne misc_set_bad
        lsr imm4, arg_y, #1             /* ppc:4236 boxed idx -> idx*4     */
        lsr imm0, arg_z, #32            /* ppc:4239 the IEEE bits ride high */
        add imm2, arg_x, #misc_data_offset
        str w0, [imm2, imm4]            /* ppc:4240 stwx                   */
        ret
misc_set_double_float_vector:
        /* ppc:4333-4339.  PPC's extract_typecode is tag-safe; we have no such
           macro, so use this file's own precedent (setu64_maybe_bignum): check
           fulltag_misc FIRST, or reading the header of an immediate faults
           instead of signalling. */
        and imm2, arg_z, #fulltagmask
        cmp imm2, #fulltag_misc
        b.ne misc_set_bad
        ldur imm1, [arg_z, #misc_header_offset]
        and imm1, imm1, #subtagmask
        cmp imm1, #subtag_double_float
        b.ne misc_set_bad
        ldr imm0, [arg_z, #double_float.value]   /* ppc:4337 misc_dfloat_offset */
        /* 64-bit elements: boxed index IS the byte offset (fixnumshift 3). */
        add imm4, arg_x, arg_y
        str imm0, [imm4, #misc_data_offset]      /* ppc:4338 stdx           */
        ret
misc_set_complex_single_float_vector:
        /* 16m41, parity twin of misc_ref_complex_single_float_vector.
           Type-check like misc_set_double_float_vector: fulltag_misc FIRST,
           or reading the header of an immediate faults instead of signalling. */
        and imm2, arg_z, #fulltagmask
        cmp imm2, #fulltag_misc
        b.ne misc_set_bad
        ldur imm1, [arg_z, #misc_header_offset]
        and imm1, imm1, #subtagmask
        cmp imm1, #subtag_complex_single_float
        b.ne misc_set_bad
        ldr imm0, [arg_z, #complex_single_float.realpart]  /* both parts */
        add imm4, arg_x, arg_y                  /* boxed idx IS the byte offset */
        str imm0, [imm4, #misc_data_offset]
        ret
misc_set_complex_double_float_vector:
        and imm2, arg_z, #fulltagmask
        cmp imm2, #fulltag_misc
        b.ne misc_set_bad
        ldur imm1, [arg_z, #misc_header_offset]
        and imm1, imm1, #subtagmask
        cmp imm1, #subtag_complex_double_float
        b.ne misc_set_bad
        ldur imm0, [arg_z, #complex_double_float.realpart]   /* ldur: see the */
        ldur imm1, [arg_z, #(complex_double_float.realpart + 8)] /* ref leg  */
        lsl imm4, arg_y, #1                     /* 16i = boxed<<1 */
        add imm4, imm4, arg_x
        add imm4, imm4, #misc_complex_dfloat_offset  /* 16m48: pad; see ref leg */
        stp imm0, imm1, [imm4]
        ret
misc_set_bad:
        mov arg_y, arg_z
        mov arg_z, arg_x
        mov arg_x, #XNOTELT             /* errors.s:227 deferr           */
        mov nargs, #(3<<fixnumshift)
        b _SPksignalerr
misc_set_invalid:
        mov temp0, #XSETBADVEC          /* errors.s:182 deferr           */
        mov nargs, #(4<<fixnumshift)
        b _SPksignalerr
endsp misc_set

/* ===== progvrestore ===== */
/* ported from ppc-spentry.s:6952-6958 (PPC64 branch) */
spentry progvrestore
        ldr imm0, [tsp, #tsp_frame.backlink]     /* ppc:6953 (backlink=0, was mis-guessed 16) */
        ldr imm0, [imm0, #tsp_frame.data_offset]  /* ppc:6954 (data_offset=16, was mis-guessed 8) */
        cmp imm0, #0
        asr imm0, imm0, #fixnumshift
        b.ne _SPunbind_n
        ret
endsp progvrestore

/* ===== aref2 ===== */
/* ported from ppc-spentry.s:7053-7083 (PPC64 branch) */
/* ABI inputs: arg_x=array, arg_y=i, arg_z=j
 * Computes row-major index, follows displaced-array chain, then branches to
 * misc_ref_common with: arg_y=underlying-vector, arg_z=row-major-index(boxed),
 * imm1=subtag. */

spentry aref2
        /* extract_typecode(imm2, arg_x): get fulltag, then if misc load subtag */
        and     imm2, arg_x, #fulltagmask
        cmp     imm2, #fulltag_misc
        b.ne    aref2_not_arrayH
        ldrb    w2, [arg_x, #misc_subtag_offset]
        /* trap_unless_lisptag_equal(arg_y, tag_fixnum) */
        tst     arg_y, #fixnummask
        b.ne    aref2_not_arrayH
        /* trap_unless_lisptag_equal(arg_z, tag_fixnum) */
        tst     arg_z, #fixnummask
        b.ne    aref2_not_arrayH
        /* Now test subtag == subtag_arrayH (PPC64 cmpri cr2 + bne cr2) */
        cmp     imm2, #subtag_arrayH
        b.ne    aref2_not_arrayH
        /* Check rank == 2 */
        ldr     imm1, [arg_x, #arrayH.rank]
        cmp     imm1, #(2 << fixnumshift)
        b.ne    aref2_not_arrayH
        /* Bounds check dim0: trlge(arg_y, dim0[0]) */
        ldr     imm0, [arg_x, #arrayH.dim0]
        cmp     arg_y, imm0
        b.hs    aref2_not_arrayH
        /* Bounds check dim1: trlge(arg_z, dim0[1]) */
        ldr     imm0, [arg_x, #(arrayH.dim0 + node_size)]
        cmp     arg_z, imm0
        b.hs    aref2_not_arrayH
        /* Row-major index: arg_z = arg_z + arg_y * unbox(dim1)
         * unbox_fixnum(imm0, imm0): imm0 still holds dim1 */
        asr     imm0, imm0, #fixnumshift
        mul     arg_y, arg_y, imm0
        add     arg_z, arg_z, arg_y
        /* Follow displaced-array chain: arg_y = array (the arrayH) */
        mov     arg_y, arg_x
aref2_follow:
        ldr     imm0, [arg_y, #arrayH.displacement]
        ldr     arg_y, [arg_y, #arrayH.data_vector]
        /* extract_subtag(imm1, arg_y) */
        ldrb    w1, [arg_y, #misc_subtag_offset]
        add     arg_z, arg_z, imm0
        cmp     imm1, #subtag_vectorH
        b.eq    aref2_follow
        cmp     imm1, #subtag_arrayH
        b.eq    aref2_follow
        /* Contract: arg_y=vector, arg_z=index(boxed), imm1=subtag */
        b       misc_ref_common
aref2_not_arrayH:
        uuo_error_reg_not_xtype arg_x, xtype_array2d /* ppc uuo_interr -> xtype trap */
endsp aref2

/* ===== aref3 ===== */
/* ported from ppc-spentry.s:7086-7122 (PPC64 branch) */
/* ABI inputs: temp0=array, arg_x=i, arg_y=j, arg_z=k
 * Computes row-major index, follows displaced-array chain, then branches to
 * misc_ref_common with: arg_y=underlying-vector, arg_z=row-major-index(boxed),
 * imm1=subtag. */
spentry aref3
        /* extract_typecode(imm2, temp0) */
        and     imm2, temp0, #fulltagmask
        cmp     imm2, #fulltag_misc
        b.ne    aref3_not_arrayH
        ldrb    w2, [temp0, #misc_subtag_offset]
        /* trap_unless_lisptag_equal(arg_x, tag_fixnum) */
        tst     arg_x, #fixnummask
        b.ne    aref3_not_arrayH
        /* trap_unless_lisptag_equal(arg_y, tag_fixnum) */
        tst     arg_y, #fixnummask
        b.ne    aref3_not_arrayH
        /* trap_unless_lisptag_equal(arg_z, tag_fixnum) */
        tst     arg_z, #fixnummask
        b.ne    aref3_not_arrayH
        /* Now test subtag == subtag_arrayH (PPC64 cmpri cr2 + bne cr2) */
        cmp     imm2, #subtag_arrayH
        b.ne    aref3_not_arrayH
        /* Check rank == 3 */
        ldr     imm1, [temp0, #arrayH.rank]
        cmp     imm1, #(3 << fixnumshift)
        b.ne    aref3_not_arrayH
        /* Load dims: dim2, dim1, dim0 (PPC64 loads in this order) */
        ldr     imm2, [temp0, #(arrayH.dim0 + (node_size * 2))]
        ldr     imm1, [temp0, #(arrayH.dim0 + node_size)]
        ldr     imm0, [temp0, #arrayH.dim0]
        /* Bounds: trlge(arg_z, imm2) */
        cmp     arg_z, imm2
        b.hs    aref3_not_arrayH
        asr     imm2, imm2, #fixnumshift    /* unbox dim2 */
        /* Bounds: trlge(arg_y, imm1) */
        cmp     arg_y, imm1
        b.hs    aref3_not_arrayH
        asr     imm1, imm1, #fixnumshift    /* unbox dim1 */
        /* Bounds: trlge(arg_x, imm0) */
        cmp     arg_x, imm0
        b.hs    aref3_not_arrayH
        /* Row-major: arg_z = k + j*dim2 + i*(dim1*dim2)
         * PPC64: mullr(arg_y,arg_y,imm2); mullr(imm1,imm2,imm1);
         *        mullr(arg_x,imm1,arg_x); add arg_z,arg_z,arg_y;
         *        add arg_z,arg_z,arg_x */
        mul     arg_y, arg_y, imm2
        mul     imm1, imm2, imm1
        mul     arg_x, imm1, arg_x
        add     arg_z, arg_z, arg_y
        add     arg_z, arg_z, arg_x
        /* Follow displaced-array chain: arg_y = temp0 (the arrayH) */
        mov     arg_y, temp0
aref3_follow:
        ldr     imm0, [arg_y, #arrayH.displacement]
        ldr     arg_y, [arg_y, #arrayH.data_vector]
        /* extract_subtag(imm1, arg_y) */
        ldrb    w1, [arg_y, #misc_subtag_offset]
        add     arg_z, arg_z, imm0
        cmp     imm1, #subtag_vectorH
        b.eq    aref3_follow
        cmp     imm1, #subtag_arrayH
        b.eq    aref3_follow
        /* Contract: arg_y=vector, arg_z=index(boxed), imm1=subtag */
        b       misc_ref_common
aref3_not_arrayH:
        uuo_error_reg_not_xtype temp0, xtype_array3d /* ppc uuo_interr -> xtype trap */
endsp aref3

/* ===== aset2 ===== */
/* ported from ppc-spentry.s:7127-7156 (PPC64 branch) */
/* ABI inputs: temp0=array, arg_x=i, arg_y=j, arg_z=newval
 * Computes row-major index, follows displaced-array chain, then branches to
 * misc_set_common with: arg_x=underlying-vector, arg_y=row-major-index(boxed),
 * arg_z=newval, imm1=subtag. */
spentry aset2
        /* extract_typecode(imm2, temp0) */
        and     imm2, temp0, #fulltagmask
        cmp     imm2, #fulltag_misc
        b.ne    aset2_not_arrayH
        ldrb    w2, [temp0, #misc_subtag_offset]
        /* trap_unless_lisptag_equal(arg_x, tag_fixnum) */
        tst     arg_x, #fixnummask
        b.ne    aset2_not_arrayH
        /* trap_unless_lisptag_equal(arg_y, tag_fixnum) */
        tst     arg_y, #fixnummask
        b.ne    aset2_not_arrayH
        /* Now test subtag == subtag_arrayH (PPC64 cmpri cr2 + bne cr2) */
        cmp     imm2, #subtag_arrayH
        b.ne    aset2_not_arrayH
        /* Check rank == 2 */
        ldr     imm1, [temp0, #arrayH.rank]
        cmp     imm1, #(2 << fixnumshift)
        b.ne    aset2_not_arrayH
        /* Bounds check dim0: trlge(arg_x, dim0[0]) */
        ldr     imm0, [temp0, #arrayH.dim0]
        cmp     arg_x, imm0
        b.hs    aset2_not_arrayH
        /* Bounds check dim1: trlge(arg_y, dim0[1]) */
        ldr     imm0, [temp0, #(arrayH.dim0 + node_size)]
        cmp     arg_y, imm0
        b.hs    aset2_not_arrayH
        /* Row-major: arg_y = arg_y + arg_x * unbox(dim1) */
        asr     imm0, imm0, #fixnumshift
        mul     arg_x, arg_x, imm0
        add     arg_y, arg_y, arg_x
        /* Follow displaced-array chain: arg_x = temp0 (the arrayH) */
        mov     arg_x, temp0
aset2_follow:
        ldr     imm0, [arg_x, #arrayH.displacement]
        ldr     arg_x, [arg_x, #arrayH.data_vector]
        /* extract_subtag(imm1, arg_x) */
        ldrb    w1, [arg_x, #misc_subtag_offset]
        add     arg_y, arg_y, imm0
        cmp     imm1, #subtag_vectorH
        b.eq    aset2_follow
        cmp     imm1, #subtag_arrayH
        b.eq    aset2_follow
        /* Contract: arg_x=vector, arg_y=index(boxed), arg_z=newval, imm1=subtag */
        b       misc_set_common
aset2_not_arrayH:
        uuo_error_reg_not_xtype temp0, xtype_array2d /* ppc uuo_interr -> xtype trap */
endsp aset2

/* ===== aset3 ===== */
/* ported from ppc-spentry.s:7160-7196 (PPC64 branch) */
/* ABI inputs: temp1=array, temp0=i, arg_x=j, arg_y=k, arg_z=new
 * Computes row-major index, follows displaced-array chain, then branches to
 * misc_set_common with: arg_x=underlying-vector, arg_y=row-major-index(boxed),
 * arg_z=newval, imm1=subtag. */
spentry aset3
        /* extract_typecode(imm2, temp1) */
        and     imm2, temp1, #fulltagmask
        cmp     imm2, #fulltag_misc
        b.ne    aset3_not_arrayH
        ldrb    w2, [temp1, #misc_subtag_offset]
        /* trap_unless_lisptag_equal(temp0, tag_fixnum) */
        tst     temp0, #fixnummask
        b.ne    aset3_not_arrayH
        /* trap_unless_lisptag_equal(arg_x, tag_fixnum) */
        tst     arg_x, #fixnummask
        b.ne    aset3_not_arrayH
        /* trap_unless_lisptag_equal(arg_y, tag_fixnum) */
        tst     arg_y, #fixnummask
        b.ne    aset3_not_arrayH
        /* Now test subtag == subtag_arrayH (PPC64 cmpri cr2 + bne cr2) */
        cmp     imm2, #subtag_arrayH
        b.ne    aset3_not_arrayH
        /* Check rank == 3 */
        ldr     imm1, [temp1, #arrayH.rank]
        cmp     imm1, #(3 << fixnumshift)
        b.ne    aset3_not_arrayH
        /* Load dims: dim2, dim1, dim0 */
        ldr     imm2, [temp1, #(arrayH.dim0 + (node_size * 2))]
        ldr     imm1, [temp1, #(arrayH.dim0 + node_size)]
        ldr     imm0, [temp1, #arrayH.dim0]
        /* Bounds: trlge(arg_y, imm2) */
        cmp     arg_y, imm2
        b.hs    aset3_not_arrayH
        asr     imm2, imm2, #fixnumshift    /* unbox dim2 */
        /* Bounds: trlge(arg_x, imm1) */
        cmp     arg_x, imm1
        b.hs    aset3_not_arrayH
        asr     imm1, imm1, #fixnumshift    /* unbox dim1 */
        /* Bounds: trlge(temp0, imm0) */
        cmp     temp0, imm0
        b.hs    aset3_not_arrayH
        /* Row-major: arg_y = k + j*dim2 + i*(dim1*dim2)
         * PPC64: mullr(arg_x,arg_x,imm2); mullr(imm1,imm2,imm1);
         *        mullr(temp0,imm1,temp0); add arg_y,arg_y,arg_x;
         *        add arg_y,arg_y,temp0 */
        mul     arg_x, arg_x, imm2
        mul     imm1, imm2, imm1
        mul     temp0, imm1, temp0
        add     arg_y, arg_y, arg_x
        add     arg_y, arg_y, temp0
        /* Follow displaced-array chain: arg_x = temp1 (the arrayH) */
        mov     arg_x, temp1
aset3_follow:
        ldr     temp0, [arg_x, #arrayH.displacement]
        ldr     arg_x, [arg_x, #arrayH.data_vector]
        /* extract_subtag(imm1, arg_x) */
        ldrb    w1, [arg_x, #misc_subtag_offset]
        add     arg_y, arg_y, temp0
        cmp     imm1, #subtag_vectorH
        b.eq    aset3_follow
        cmp     imm1, #subtag_arrayH
        b.eq    aset3_follow
        /* Contract: arg_x=vector, arg_y=index(boxed), arg_z=newval, imm1=subtag */
        b       misc_set_common
aset3_not_arrayH:
        uuo_error_reg_not_xtype temp1, xtype_array3d /* ppc uuo_interr -> xtype trap */
endsp aset3

/* ===== COMPLETION STATUS & MISSING CONSTANTS ===== */
/*
 * ALL 22 SUBPRIMS PORTED (logic complete, awaiting constant definitions):
 *   ✓ aref2 (2d array ref) - COMPLETE, exits via misc_ref_common
 *   ✓ aref3 (3d array ref) - COMPLETE, exits via misc_ref_common
 *   ✓ aset2 (2d array set) - COMPLETE, exits via misc_set_common
 *   ✓ aset3 (3d array set) - COMPLETE, exits via misc_set_common
 *   ✓ conslist, conslist_star (heap cons) - COMPLETE
 *   ✓ stkconslist, stkconslist_star (tstack cons) - needs tsp_frame offsets
 *   ✓ mkstackv (tstack vector) - needs tsp_frame offsets
 *   ✓ gvector (heap vector) - COMPLETE except dnode_align macro
 *   ✓ misc_ref (vector read) - COMPLETE: integer/node/string/bit + float and
 *     complex-float vectors (16m37/16m41)
 *   ✓ subtag_misc_ref (explicit subtag) - COMPLETE
 *   ✓ misc_set (vector write) - COMPLETE: same coverage as misc_ref above
 *   ✓ subtag_misc_set (explicit subtag) - COMPLETE
 *   ✓ gvset (GC write barrier) - LOGIC COMPLETE, needs GC globals
 *   ✓ set_hash_key (hash-table write) - LOGIC COMPLETE, needs GC globals
 *   ✓ store_node_conditional (atomic store+barrier) - LOGIC COMPLETE, needs GC globals
 *   ✓ set_hash_key_conditional (atomic hash store) - LOGIC COMPLETE, needs GC globals
 *   ✓ stkconsyz (tstack cons from Y/Z) - COMPLETE
 *   ✓ progvsave (special bindings) - LOGIC COMPLETE (~70 lines), needs tcr/tsp_frame/symbol offsets
 *   ✓ progvrestore (restore bindings) - LOGIC COMPLETE
 *   ✓ stkgvector (tstack general vector) - LOGIC COMPLETE (~30 lines), needs tsp_frame offsets
 *
 * MISSING CONSTANTS (must be defined in arm64-constants.h or arm64-macros.s):
 *
 * 1. GC write barrier (gvset, set_hash_key, store/set_*_conditional):
 *    - ref_base (global: base of reference bitmap)
 *    - refbits (global: pointer to refbits array)
 *    - ephemeral_refidx (global: pointer to ephemeral index array)
 *    - oldspace_dnode_count (global: size of oldspace in dnodes)
 *    - dnode_shift (constant: 4 for 16-byte dnodes)
 *    - bitmap_shift (constant: 9 for 512-entry bitmap chunks)
 *
 * 2. Symbolic values:
 *    - nil_value (address of NIL object; low-tag design unclear if static)
 *    - t_value (address of T object)
 *    - RESERVATION_DISCHARGE (address for clearing ldxr reservation)
 *
 * 3. Error codes:
 *    - XBADVEC (bad vector type/index error)
 *    - XNOTELT (bad element type error)
 *    - XSETBADVEC (bad vector for set operation)
 *
 * 4. TSP frame structure (for stkconslist*, mkstackv, stkgvector, progvsave/restore):
 *    - tsp_frame.fixed_overhead (frame header size, likely 8-16 bytes)
 *    - tsp_frame.data_offset (offset to data area, likely 8)
 *    - tsp_frame.backlink (offset to previous frame link)
 *    - tstack_alloc_limit (global or tcr field for overflow check)
 *
 * 5. TCR offsets (already in constants.h but needs verification):
 *    - tcr.ts_area (offset to tstack area pointer) - VERIFIED at tcr struct definition
 *    - tcr.db_link (special binding chain, for progvsave)
 *    - tcr.tlb_limit, tcr.tlb_pointer (thread-local binding array, for progvsave)
 *
 * 6. Alignment macros (referenced but not expanded):
 *    - dnode_align(dest, src, add) - align to 16-byte boundary
 *
 * 7. Float/complex support -- CLOSED (16m37 real floats, 16m41 complex): the
 *    constants and Misc_Alloc_Fixed all exist; misc_ref/misc_set dispatch every
 *    float and complex-float vector subtag.  Kept for the register/allocation
 *    notes below.
 *    - subtag_double_float, subtag_single_float, subtag_complex_single_float, etc.
 *    - Allocation macros: Misc_Alloc_Fixed for boxed float returns
 *    - Bignum header constants: one/two/three_digit_bignum_header
 *
 * 8. progvsave-specific:
 *    - symbol.binding_index (offset within symbol struct)
 *    - XIMPROPERLIST (error code for improper list)
 *    - Binding trap mechanism (PPC64 trlle → ARM64 conditional brk or bounds check)
 *
 * DESIGN NOTES:
 *   - ARM64 low-tag: fixnumshift=3, misc_data_offset=+4, misc_header_offset=-4
 *   - Bit vectors: ARM64 LSB0 bit order (bit 0 is rightmost)
 *   - Atomics: PPC64 ldarx/stdcx. → ARM64 ldxr/stxr + dmb ish (isync → dmb)
 *   - cons.size = 16 (2*node_size from struct definition)
 *   - _rplaca/_rplacd macros expanded inline as str to cons.car/cons.cdr offsets
 *   - Node vectors delegate to _SPgvset for write-barrier handling
 *   - Float/complex handlers marked #error due to missing constants (not design issues)
 */
/* SPDX-License-Identifier: Apache-2.0 */

/*
 * C-bind-catch-throw cluster: 41 subprims ported from PPC64 to ARM64 low-tag
 *
 * Source: vendor/ccl/lisp-kernel/ppc-spentry.s (PPC64 branch)
 * Target: Matt Emerson's upstream ARM64 low-tag design
 *
 * Register map (ARM64 low-tag).  Authoritative: the .req block in
 * lisp-kernel/arm64-constants.h:51-82.
 *   imm0-5 = x0-x5
 *   nargs = x6 (BOXED: the count is held shifted left by fixnumshift)
 *   fn = x7 (VOLATILE in calls)
 *   arg_w = x8, arg_x = x9, arg_y = x10, arg_z = x11
 *   temp0-5 = x12-x17   (authoritative: arm64-constants.h .req block)
 *   save0-3 = x19-x22
 *   rnil = x23
 *   tsp = x24 (REAL tsp register, PPC-style)
 *   vsp = x25
 *   allocptr = x26, allocbase = x27
 *   rcontext = x28
 *   lr = x30
 *
 * Low tags (bottom 4 bits), per arm64-constants.h:135-173:
 *   fulltag_cons = 0b0011
 *   fulltag_symbol = 0b0111
 *   fulltag_nil = 0b1011
 *   fulltag_misc = 0b1100
 *   tag_fixnum = 0b000 (even fixnums 0b0000, odd 0b1000)
 *   fixnumshift = 3
 *
 * Object layout (arm64-constants.h:171-173 -- these are DERIVED from
 * fulltag_misc, never independent numbers):
 *   misc_header_offset = -fulltag_misc
 *   misc_data_offset = misc_header_offset + node_size
 *   cons.cdr, cons.car per the _struct cons block in arm64-constants.h
 *
 * STATUS: all 41 subprims carry real ported bodies (0 stubs).  Sites that
 * depend on constants/conventions Matt has not yet defined are guarded with
 * #error + the intended instruction in a comment (see PROPOSED-CONSTANTS
 * below and upstream-port/MISSING-CONSTANTS-RATIFY.md).
 */

/*
 * ===========================================================================
 * PROPOSED-CONSTANTS (ratify with Matt)
 * ---------------------------------------------------------------------------
 * These are NOT in arm64-constants.h.  Values are DERIVED from the cited
 * sources; the C runtime (arm64-exceptions.c, gc) MUST agree on the struct
 * layouts and the uuo/udf encodings below.
 * ===========================================================================
 */

/* Function argument-descriptor word bits (in nargs on entry to
   destructuring_bind_inner/macro_bind).  PROPOSED: adopt the ARM32
   layout (vendor/ccl/lisp-kernel/arm-constants.s:561-567) -- counts in
   value bytes 0/1/2, flag bits high -- NOT the PPC bit numbering
   (ppc-constants.s:114-117 uses big-endian bit indices 4-7).
   MUST match the compiler's doadlword emission when Matt defines it. */
.set mask_keyp,     (1<<25)
.set mask_aok,      (1<<26)
.set mask_restp,    (1<<27)
.set mask_initopt,  (1<<29)

/* Lisp error selectors.  vendor/ccl/lisp-kernel/errors.s: deferr(NAME,N)
   expands to  NAME = N<<fixnumshift  (a boxed fixnum passed to .SPksignalerr). */
.set XVUNBND,       (1<<fixnumshift)     /* errors.s:176 deferr(XVUNBND,1)      */
.set XCONST,        (115<<fixnumshift)   /* errors.s:201 deferr(XCONST,115)     */
.set XBADKEYS,      (153<<fixnumshift)   /* errors.s:214 deferr(XBADKEYS,153)   */
.set XCALLTOOMANY,  (167<<fixnumshift)   /* errors.s:220 deferr(XCALLTOOMANY,167)*/
.set XCALLTOOFEW,   (168<<fixnumshift)   /* errors.s:221 deferr(XCALLTOOFEW,168) */
.set XCALLNOMATCH,  (169<<fixnumshift)   /* errors.s:222 deferr(XCALLNOMATCH,169)*/
.set XIMPROPERLIST, (170<<fixnumshift)   /* errors.s:223 deferr(XIMPROPERLIST,170)*/
.set XSYMNOBIND,    (178<<fixnumshift)   /* errors.s:231 deferr(XSYMNOBIND,178) */

/* Kernel (uuo_interr) error codes - raw small integers, NOT shifted.
   vendor/ccl/lisp-kernel/errors.s top block. */
.set error_throw_tag_missing, 3          /* errors.s:23  */
.set error_propagate_suspend, 10         /* errors.s:28  */

/* tcr.flags bit for a suspend that arrived while interrupts were disabled.
   ppc-constants64.h / x86-constants64.s TCR_FLAG_BIT_PENDING_SUSPEND. */
.set TCR_FLAG_BIT_PENDING_SUSPEND, 7

/* value-cell header: def_header(value_cell_header,1,subtag_value_cell),
   ppc-constants64.s:368 -- same (count<<num_subtag_bits)|subtag formula as
   arm64-constants.h's own two_digit_bignum_header; subtag_value_cell is
   already real there (SUBTAG(fulltag_nodeheader_1,5)). */
.set value_cell_header, (1 << num_subtag_bits) | subtag_value_cell

/* log2(dnode_size=16): ppc-constants64.s:37 dnode_shift = dnode_align_bits. */
.set dnode_shift, 4

/* symbol.flags bits (x86-constants64.s:707-710; low-tag => +fixnum_shift). */
.set sym_vbit_bound,      (0+fixnumshift)
.set sym_vbit_bound_mask, (1<<sym_vbit_bound)
.set sym_vbit_const,      (1+fixnumshift)
.set sym_vbit_const_mask, (1<<sym_vbit_const)

/* catch_frame comes from arm64-constants.h: PPC64's layout
   (ppc-constants64.s _structf(catch_frame); ppc-constants64.h:213), with
   regs sized to this design's nsaveregs=4 (save0..save3) instead of
   PPC's 8.  This file used to redefine it locally; keep one copy. */


/* GPR numbers, for encoding a register operand into a udf immediate. */

/* UUO / trap encodings.  CANONICAL: arm64-asm.lisp:435-450 (Matt's active
   layer) = `udf #imm16`, low 3 bits = format (7 nullary, 1 unary
   arg-count, 2 binary).  PROPOSED extensions (full namespace doc:
   spentry-A's trap block, BINDING): fmt 3 = unary-misc, reg in 7:3, sub
   in 15:8 (0 not_callable, 1 no_throw_tag, 2 tlb_too_small, 3 unbound,
   >= 4 = the errors.s errnum: stack_overflow/too_many_values/
   propagate_suspend here); nullary sub 4 = take-deferred-interrupt.
   arm64-exceptions.c must decode these.  \gpr is a gpr_* number. */
/* kernel trap error codes (errors.s:25/27) */
.set error_stack_overflow,   5
.set error_too_many_values,  7
/* signal a deferred/pending suspend (ppc-macros.s suspend_now).
   unary-misc sub = error_propagate_suspend (10); reg field unused (x0). */

/* control-stack lisp_frame build/discard (MARKER frame: \tmp carries the
   marker constant, not a backlink).  savelr gets \clpc (catch cleanup
   PC), savefn gets the volatile fn. */
.macro build_catch_lisp_frame tmp, clpc
        sub sp, sp, #lisp_frame.size
        mov \tmp, #lisp_frame_marker
        str \tmp, [sp, #lisp_frame.marker]
        str vsp, [sp, #lisp_frame.savevsp]
        str fn, [sp, #lisp_frame.savefn]
        str \clpc, [sp, #lisp_frame.savelr]
.endm

/* tsp_alloc_fixed_unboxed / Set_TSP_Frame_Boxed / TSP_Unlink moved to the
   canonical set in arm64-macros.s (TSP_Alloc_Fixed_Unboxed / Set_TSP_Frame_
   Boxed / TSP_Unlink).  GNU as macro names are case-insensitive, so the local
   copies would collide with the canonical ones; call sites bind to the
   arm64-macros.s versions unchanged. */

/* save/restore the boxed NVRs into/from a catch frame's regs[] (save0..save3).
   catch_frame is a fulltag_misc-biased _structf, so .regs = 36 is only
   4-aligned -- stp/ldp (which need an 8-scaled imm7) cannot be used; single
   str/ldr take any byte offset. */
.macro save_catch_regs cf
        str save0, [\cf, #(catch_frame.regs + 0*node_size)]
        str save1, [\cf, #(catch_frame.regs + 1*node_size)]
        str save2, [\cf, #(catch_frame.regs + 2*node_size)]
        str save3, [\cf, #(catch_frame.regs + 3*node_size)]
.endm
.macro restore_catch_regs cf
        ldr save0, [\cf, #(catch_frame.regs + 0*node_size)]
        ldr save1, [\cf, #(catch_frame.regs + 1*node_size)]
        ldr save2, [\cf, #(catch_frame.regs + 2*node_size)]
        ldr save3, [\cf, #(catch_frame.regs + 3*node_size)]
.endm

/*
 * ===========================================================================
 * CATCH/THROW/UNWIND SUBPRIMS
 * ===========================================================================
 */

/* mkcatch (ppc-macros.s:481-517).  Build a catch/unwind frame on the temp
   stack, with the caller's continuation saved in a control-stack lisp_frame.
   In: arg_z = catch tag, imm2 = mvflag (0 or fixnum 1).  Clobbers imm0-5 and
   nargs ONLY; preserves save0..save3 (stored into the frame) and ALL temp
   regs -- PPC parity: ppc-macros.s mkcatch scratches imm0-4/loc_pc/nargs,
   and callers (toplevel_loop, compiled catch) keep the funcall target in
   temp0 across the _SPmkcatch* call (16j boot: temp0-as-scratch clobbered
   the toplevel function -> udf #49 in _SPfuncall).
   PROPOSED: the cleanup-PC recovery assumes Matt's arm64 compiler emits a
   forward `b <cleanup>` immediately after the `bl _SPmkcatch*`, exactly as the
   PPC backend does. */
        .macro mkcatch
        ldr w5, [lr]                   /* imm5: the forward branch insn       */
        ldr imm0, [rcontext, #tcr.catch_top]
        sbfx imm5, imm5, #0, #26       /* B imm26, sign-extended              */
        add imm5, lr, imm5, lsl #2     /* cleanup PC = lr + imm26*4           */
        add lr, lr, #4                 /* normal return addr: skip the branch */
        build_catch_lisp_frame imm4, imm5     /* csp frame: fn,cleanupPC,vsp  */
        ldr imm3, [rcontext, #tcr.xframe]
        ldr imm1, [rcontext, #tcr.db_link]
        TSP_Alloc_Fixed_Unboxed catch_frame.size, imm4
        add nargs, tsp, #(tsp_frame.data_offset + fulltag_misc)  /* tagged cf
                                          (PPC uses nargs for this too)       */
        mov imm4, #((catch_frame.element_count<<num_subtag_bits) | subtag_catch_frame)
        str imm4, [nargs, #catch_frame.header]
        str arg_z, [nargs, #catch_frame.catch_tag]
        str imm0, [nargs, #catch_frame.link]    /* previous catch_top          */
        str imm2, [nargs, #catch_frame.mvflag]
        mov imm4, sp
        str imm4, [nargs, #catch_frame.csp]
        str imm1, [nargs, #catch_frame.db_link]
        save_catch_regs nargs
        str imm3, [nargs, #catch_frame.xframe]
        ldr imm0, [rcontext, #tcr.nfp]
        str imm0, [nargs, #catch_frame.nfp]
        Set_TSP_Frame_Boxed
        str nargs, [rcontext, #tcr.catch_top]
        set_nargs 0
        .endm

/* ported from ppc-spentry.s:61-64.  Single-value catch; tag in arg_z. */
spentry mkcatch1v
        mov imm2, #0
        mkcatch
        ret
endsp mkcatch1v

/* ported from ppc-spentry.s:72-75.  Multiple-value catch; tag in arg_z. */
spentry mkcatchmv
        mov imm2, #fixnum_one
        mkcatch
        ret
endsp mkcatchmv

/* ported from ppc-spentry.s:66-70.  Unwind-protect frame: tag = unbound_marker,
   mvflag = fixnum 1. */
spentry mkunwind
        mov arg_z, #unbound_marker
        mov imm2, #fixnum_one
        mkcatch
        ret
endsp mkunwind

/* ported from ppc-spentry.s:80-162 (PPC64 branch) */
/* Throw to a catch tag. Caller has pushed tag and 0 or more values; nargs = nvalues */
spentry throw
        /* ppc-spentry.s:80-162.  Caller pushed tag then 0+ values; nargs=nvalues. */
        ldr imm1, [rcontext, #tcr.catch_top]
        mov imm0, #0                    /* fixnum count of intervening frames  */
        ldr temp0, [vsp, nargs]         /* the throw tag (above the values)     */
        cbz imm1, 9f                    /* no catch frames -> tag not found     */
1:      /* _throw_loop */
        ldr temp1, [imm1, #catch_frame.catch_tag]
        mov imm2, imm1                  /* imm2 = candidate target frame        */
        ldr imm1, [imm1, #catch_frame.link]
        cmp temp0, temp1
        b.eq 2f                         /* _throw_found                         */
        add imm0, imm0, #fixnum_one
        cbnz imm1, 1b
        b 9f                            /* end of chain -> tag not found        */
2:      /* _throw_found: imm2 = target frame, imm0 = intervening count          */
        mov fn, #0                      /* GC-safe fn across the nthrow call     */
        add imm1, vsp, nargs
        sub imm1, imm1, #node_size      /* imm1 -> top (last) value slot        */
        ldr temp1, [imm2, #catch_frame.mvflag]
        cbnz temp1, 4f                  /* multiple-value receiver              */
        cmp nargs, #0                   /* single-value receiver                */
        set_nargs 1
        b.eq 3f                         /* no values thrown -> default NIL      */
        mov vsp, imm1                   /* keep only the top value              */
        b 4f
3:      /* _throw_default_1_val */
        vpush1 rnil
4:      /* _throw_all_values */
        bl _SPnthrowvalues
        ldr imm3, [rcontext, #tcr.catch_top]
        ldr imm1, [rcontext, #tcr.db_link]
        ldr imm0, [imm3, #catch_frame.db_link]
        sub tsp, imm3, #(tsp_frame.fixed_overhead + fulltag_misc)
        cmp imm0, imm1
        b.eq 5f                         /* _throw_dont_unbind                   */
        bl _SPunbind_to                 /* imm0 = target db_link; keeps nargs   */
5:      /* _throw_dont_unbind */
        ldr imm4, [imm3, #catch_frame.mvflag]
        add imm0, vsp, nargs
        ldr imm1, [imm3, #catch_frame.csp]
        ldr imm1, [imm1, #lisp_frame.savevsp]
        cbnz imm4, 6f                   /* _throw_multiple                      */
        ldr arg_z, [imm0, #-node_size]  /* single value into arg_z             */
        b 8f
6:      /* _throw_multiple */
        cmp nargs, #0
        b.eq 8f
        mov imm2, nargs
7:      /* _throw_mvloop: copy nargs values down onto the catcher's vsp        */
        sub imm2, imm2, #fixnum_one
        ldr temp0, [imm0, #-node_size]!
        str temp0, [imm1, #-node_size]!
        cmp imm2, #0
        b.gt 7b
8:      /* _throw_pushed_values */
        mov vsp, imm1
        ldr imm1, [imm3, #catch_frame.xframe]
        str imm1, [rcontext, #tcr.xframe]
        ldr imm1, [imm3, #catch_frame.nfp]
        str imm1, [rcontext, #tcr.nfp]
        ldr temp0, [imm3, #catch_frame.csp]
        mov sp, temp0
        ldr fn, [sp, #lisp_frame.savefn]
        ldr temp4, [sp, #lisp_frame.savelr]  /* catch exit / cleanup PC        */
        discard_lisp_frame
        mov lr, temp4
        restore_catch_regs imm3
        ldr imm3, [imm3, #catch_frame.link]
        str imm3, [rcontext, #tcr.catch_top]
        TSP_Unlink
        ret
9:      /* _throw_tag_not_found */
        uuo_error_no_throw_tag temp0
        str temp0, [vsp, nargs]         /* restore tag; retry after handler     */
        b _SPthrow
endsp throw

/* ported from ppc-spentry.s:166-284 (PPC64 branch) */
/* Unwind N frames (imm0 = count), processing unwind-protects */
/* N multiple values atop vstack, nargs = count */
/* tsp_alloc_var_boxed_nz moved to arm64-macros.s as TSP_Alloc_Var_Boxed_nz
   (publish-last, 2-reg: size, scratch -- size is clobbered as the zeroing
   cursor, the live tsp is the loop's end sentinel). */

spentry nthrowvalues
        /* ppc-spentry.s:166-284.  N values atop the vstack, nargs=count. */
        mov imm1, #1
        mov imm4, imm0                  /* imm4 = frame countdown (fixnum)      */
        str imm1, [rcontext, #tcr.unwinding]
1:      /* _nthrowv_nextframe */
        subs imm4, imm4, #fixnum_one
        ldr temp0, [rcontext, #tcr.catch_top]
        ldr imm1, [rcontext, #tcr.db_link]
        b.lt 8f                         /* countdown < 0 -> done                */
        ldr imm0, [temp0, #catch_frame.db_link]
        ldr imm3, [temp0, #catch_frame.link]
        str imm3, [rcontext, #tcr.catch_top]
        ldr temp1, [temp0, #catch_frame.catch_tag]
        ldr imm3, [temp0, #catch_frame.xframe]
        str imm3, [rcontext, #tcr.xframe]
        ldr imm3, [temp0, #catch_frame.nfp]
        str imm3, [rcontext, #tcr.nfp]
        ldr imm2, [temp0, #catch_frame.csp]
        mov sp, imm2                    /* sp = the frame's saved lisp_frame    */
        cmp imm0, imm1                  /* special bindings to undo?            */
        b.eq 2f
        mov temp4, lr
        bl _SPunbind_to                 /* imm0 = target db_link                */
        mov lr, temp4
2:      /* _nthrowv_dont_unbind */
        cmp temp1, #unbound_marker      /* unwind-protect frame?                */
        b.eq 4f
        /* --- catch frame --- */
        cmp imm4, #0
        b.ne 3f                         /* not the last frame -> just discard   */
        ldr imm0, [sp, #lisp_frame.savevsp]
        str xzr, [sp, #lisp_frame.savevsp]  /* stack-overflow marker            */
        add imm1, vsp, nargs
        mov imm2, nargs
        b 32f
31:     /* _nthrowv_push_loop: move values down onto catcher's vstack           */
        ldr temp1, [imm1, #-node_size]!
        str temp1, [imm0, #-node_size]!
32:     cmp imm2, #0
        sub imm2, imm2, #fixnum_one
        b.ne 31b
        mov vsp, imm0
        restore_catch_regs temp0
3:      /* _nthrowv_skip */
        sub tsp, temp0, #(tsp_frame.fixed_overhead + fulltag_misc)
        TSP_Unlink
        discard_lisp_frame
        b 1b
4:      /* _nthrowv_do_unwind: run the cleanup form with values preserved      */
        ldr imm3, [temp0, #catch_frame.xframe]
        str imm3, [rcontext, #tcr.xframe]
        ldr imm3, [temp0, #catch_frame.nfp]
        str imm3, [rcontext, #tcr.nfp]
        restore_catch_regs temp0
        sub tsp, temp0, #(tsp_frame.fixed_overhead + fulltag_misc)
        TSP_Unlink
        ldr temp4, [sp, #lisp_frame.savelr]   /* cleanup code address          */
        ldr nfn, [sp, #lisp_frame.savefn]     /* cleanup's own fn              */
        str fn, [sp, #lisp_frame.savefn]      /* stash caller fn in the frame  */
        mov fn, nfn
        str lr, [sp, #lisp_frame.savelr]      /* stash our return in the frame */
        /* allocate a boxed tsp frame: overhead + nargs bytes + 2 nodes        */
        add imm0, nargs, #(tsp_frame.fixed_overhead + (2*node_size) + (dnode_size-1))
        and imm0, imm0, #~(dnode_size-1)
        TSP_Alloc_Var_Boxed_nz imm0, imm1
        mov imm2, nargs
        add imm1, vsp, nargs            /* imm1 = top of value block            */
        add imm0, tsp, #tsp_frame.data_offset
        str nargs, [imm0]               /* data[0] = value count                */
        b 42f
41:     /* _nthrowv_tpushloop: stash values into the tsp frame                  */
        ldr temp0, [imm1, #-node_size]!
        str temp0, [imm0, #node_size]!
        sub imm2, imm2, #fixnum_one
42:     cmp imm2, #0
        b.ne 41b
        str imm4, [imm0, #node_size]!   /* stash throw count after the values   */
        ldr vsp, [sp, #lisp_frame.savevsp]
        str xzr, [rcontext, #tcr.unwinding]
        blr temp4                       /* call the cleanup form                */
        mov imm1, #1
        add imm0, tsp, #tsp_frame.data_offset
        str imm1, [rcontext, #tcr.unwinding]
        ldr fn, [sp, #lisp_frame.savefn]
        ldr temp4, [sp, #lisp_frame.savelr]
        discard_lisp_frame
        mov lr, temp4
        ldr nargs, [imm0]               /* restore value count                  */
        mov imm2, nargs
        b 44f
43:     /* _nthrowv_tpoploop: restore values onto the vstack                    */
        ldr temp0, [imm0, #node_size]!
        vpush1 temp0
        sub imm2, imm2, #fixnum_one
44:     cmp imm2, #0
        b.ne 43b
        ldr imm4, [imm0, #node_size]    /* restore throw count                  */
        TSP_Unlink
        b 1b
8:      /* _nthrowv_done */
        str xzr, [rcontext, #tcr.unwinding]
        mov imm4, nargs                 /* preserve nargs across the poll       */
        check_pending_interrupt
        mov nargs, imm4
        ret
endsp nthrowvalues

/* ported from ppc-spentry.s:289-363 (PPC64 branch) */
/* Single-value version of nthrowvalues. Value in arg_z */
spentry nthrow1value
        /* ppc-spentry.s:289-363.  Single value in arg_z; nargs unused. */
        mov imm1, #1                    /* ppc:290 */
        mov imm4, imm0                  /* ppc:291  imm4 = frame countdown       */
        str imm1, [rcontext, #tcr.unwinding]  /* ppc:292 */
1:      /* ppc:293 _nthrow1v_nextframe */
        subs imm4, imm4, #fixnum_one    /* ppc:294-295 (cr1)                     */
        ldr temp0, [rcontext, #tcr.catch_top]  /* ppc:296 */
        ldr imm1, [rcontext, #tcr.db_link]     /* ppc:297 */
        set_nargs 1                     /* ppc:298 (flag-preserving)             */
        b.lt 8f                         /* ppc:299 blt cr1 -> done               */
        ldr imm3, [temp0, #catch_frame.link]      /* ppc:300 */
        ldr imm0, [temp0, #catch_frame.db_link]   /* ppc:301 */
        str imm3, [rcontext, #tcr.catch_top]      /* ppc:303 */
        ldr imm3, [temp0, #catch_frame.xframe]    /* ppc:304 */
        ldr temp1, [temp0, #catch_frame.catch_tag]/* ppc:305 */
        ldr temp2, [temp0, #catch_frame.nfp]      /* ppc:306 */
        str imm3, [rcontext, #tcr.xframe]         /* ppc:308 */
        str temp2, [rcontext, #tcr.nfp]           /* ppc:309 */
        ldr imm2, [temp0, #catch_frame.csp]       /* ppc:310 (sp via scratch)    */
        mov sp, imm2
        cmp imm0, imm1                  /* ppc:302 cr0, at its branch            */
        b.eq 2f                         /* ppc:311 beq cr0 -> dont_unbind        */
        mov temp4, lr                   /* ppc:312 mflr                          */
        bl _SPunbind_to                 /* ppc:313 (clobbers flags)              */
        mov lr, temp4                   /* ppc:314 mtlr                          */
2:      /* ppc:315 _nthrow1v_dont_unbind */
        cmp temp1, #unbound_marker      /* ppc:307 cr7, recomputed post-unbind   */
        b.eq 4f                         /* ppc:316 beq cr7 -> do_unwind          */
        cmp imm4, #0                    /* ppc:295 cr1, recomputed               */
        b.ne 3f                         /* ppc:318 bne cr1 -> skip               */
        ldr vsp, [sp, #lisp_frame.savevsp]  /* ppc:319 */
        restore_catch_regs temp0        /* ppc:320 */
3:      /* ppc:321 _nthrow1v_skip */
        sub tsp, temp0, #(tsp_frame.fixed_overhead + fulltag_misc)  /* ppc:322 */
        TSP_Unlink                      /* ppc:323 */
        discard_lisp_frame              /* ppc:324 */
        b 1b                            /* ppc:325 */
4:      /* ppc:326 _nthrow1v_do_unwind */
        restore_catch_regs temp0        /* ppc:332 */
        sub tsp, temp0, #(tsp_frame.fixed_overhead + fulltag_misc)  /* ppc:333 */
        TSP_Unlink                      /* ppc:334 */
        ldr temp4, [sp, #lisp_frame.savelr]  /* ppc:335,337 cleanup PC -> temp4  */
        ldr nfn, [sp, #lisp_frame.savefn]    /* ppc:336 cleanup's own fn         */
        str fn, [sp, #lisp_frame.savefn]     /* ppc:338 stash caller fn          */
        mov fn, nfn                     /* ppc:340 */
        str lr, [sp, #lisp_frame.savelr]     /* ppc:339,341 stash our return     */
        /* fixed boxed tsp frame: value + throw count = 2 nodes (ppc:342)        */
        TSP_Alloc_Fixed_Unboxed 2*node_size, imm0
        Set_TSP_Frame_Boxed
        str arg_z, [tsp, #tsp_frame.data_offset]                /* ppc:343 */
        str imm4, [tsp, #(tsp_frame.data_offset + node_size)]   /* ppc:344 */
        ldr vsp, [sp, #lisp_frame.savevsp]   /* ppc:345 */
        str xzr, [rcontext, #tcr.unwinding]  /* ppc:346 */
        blr temp4                       /* ppc:347 bctrl -> cleanup form         */
        mov imm1, #1                    /* ppc:348 */
        ldr arg_z, [tsp, #tsp_frame.data_offset]                /* ppc:349 */
        str imm1, [rcontext, #tcr.unwinding] /* ppc:350 */
        ldr imm4, [tsp, #(tsp_frame.data_offset + node_size)]   /* ppc:351 */
        ldr fn, [sp, #lisp_frame.savefn]     /* ppc:352 */
        ldr temp4, [sp, #lisp_frame.savelr]  /* ppc:353 */
        discard_lisp_frame              /* ppc:354 */
        mov lr, temp4                   /* ppc:355 */
        TSP_Unlink                      /* ppc:356 */
        b 1b                            /* ppc:357 */
8:      /* ppc:358 _nthrow1v_done.  nargs is dead here, so the poll may clobber it. */
        str xzr, [rcontext, #tcr.unwinding]  /* ppc:359 */
        check_pending_interrupt         /* ppc:362 */
        ret                             /* ppc:363 */
endsp nthrow1value

/*
 * ===========================================================================
 * SPECIAL BINDING SUBPRIMS
 * ===========================================================================
 */

/* ported from ppc-spentry.s:367-386 (PPC64 branch) */
/* Bind symbol arg_y to value arg_z */
spentry bind
        ldr imm1, [arg_y, #symbol.binding_index]
        ldr imm0, [rcontext, #tcr.tlb_limit]
        cmp imm0, imm1                 /* trlle: trap unless limit > index */
        b.hi 1f
        uuo_error_tlb_too_small imm1
1:
        cmp imm1, #0                   /* binding_index 0 => not special */
        ldr imm2, [rcontext, #tcr.tlb_pointer]
        ldr imm0, [rcontext, #tcr.db_link]
        ldr temp1, [imm2, imm1]        /* old value at tlb[index] */
        b.eq 9f
        vpush1 temp1                   /* binding frame: old value */
        vpush1 imm1                    /*               tlb index  */
        vpush1 imm0                    /*               prev db_link */
        str arg_z, [imm2, imm1]        /* tlb[index] = new value */
        str vsp, [rcontext, #tcr.db_link]
        ret
9:
        mov arg_z, arg_y
        mov arg_y, #XSYMNOBIND
        set_nargs 2
        b _SPksignalerr
endsp bind

/* ported from ppc-spentry.s:389-411 (PPC64 branch) */
/* Bind symbol arg_z to its current value */
spentry bind_self
        ldr imm1, [arg_z, #symbol.binding_index]
        ldr imm0, [rcontext, #tcr.tlb_limit]
        cmp imm1, #0
        b.eq 9f
        cmp imm0, imm1                 /* trlle: trap unless limit > index */
        b.hi 1f
        uuo_error_tlb_too_small imm1
1:
        ldr imm2, [rcontext, #tcr.tlb_pointer]
        ldr imm0, [rcontext, #tcr.db_link]
        ldr temp1, [imm2, imm1]        /* current tlb value */
        mov temp0, temp1
        cmp temp1, #no_thread_local_binding_marker
        b.ne 2f
        ldr temp0, [arg_z, #symbol.vcell]  /* no per-thread value: use vcell */
2:
        vpush1 temp1                   /* old tlb contents */
        vpush1 imm1                    /* tlb index */
        vpush1 imm0                    /* prev db_link */
        str temp0, [imm2, imm1]
        str vsp, [rcontext, #tcr.db_link]
        ret
9:
        mov arg_y, #XSYMNOBIND
        set_nargs 2
        b _SPksignalerr
endsp bind_self

/* ported from ppc-spentry.s:414-432 (PPC64 branch) */
/* Bind symbol arg_z to NIL */
spentry bind_nil
        /* symbol in arg_z; bind it to NIL by reusing .SPbind
           (symbol->arg_y, value(nil)->arg_z). */
        mov arg_y, arg_z
        mov arg_z, rnil
        b _SPbind
endsp bind_nil

/* ported from ppc-spentry.s:436-458 (PPC64 branch) */
/* Bind symbol arg_z to its current value; trap if unbound */
spentry bind_self_boundp_check
        ldr imm1, [arg_z, #symbol.binding_index]
        ldr imm0, [rcontext, #tcr.tlb_limit]
        cmp imm1, #0
        b.eq 9f
        cmp imm0, imm1                 /* trlle: trap unless limit > index */
        b.hi 1f
        uuo_error_tlb_too_small imm1
1:
        ldr imm2, [rcontext, #tcr.tlb_pointer]
        ldr imm0, [rcontext, #tcr.db_link]
        ldr temp1, [imm2, imm1]
        mov temp0, temp1
        cmp temp1, #no_thread_local_binding_marker
        b.ne 2f
        ldr temp0, [arg_z, #symbol.vcell]
2:
        cmp temp0, #unbound_marker      /* treqi: trap if still unbound */
        b.ne 3f
        uuo_error_unbound arg_z         /* macro fixed upstream @ c9e7ffb */
3:
        vpush1 temp1                   /* old tlb contents */
        vpush1 imm1                    /* tlb index */
        vpush1 imm0                    /* prev db_link */
        str temp0, [imm2, imm1]
        str vsp, [rcontext, #tcr.db_link]
        ret
9:
        mov arg_y, #XSYMNOBIND
        set_nargs 2
        b _SPksignalerr
endsp bind_self_boundp_check

/* ported from ppc-spentry.s:938-953 (PPC64 branch) */
/* Set symbol value. Non-null symbol in arg_y, new value in arg_z */
/* NOTE: Line number search for setqsym in ppc-spentry.s */
spentry setqsym
        /* ppc-spentry.s:938-945.  Non-null symbol in arg_y, new value in arg_z.
           Constant symbol => error; otherwise the real work is in .SPspecset. */
        ldr imm0, [arg_y, #symbol.flags]
        tst imm0, #sym_vbit_const_mask
        b.eq _SPspecset
        mov arg_z, arg_y
        mov arg_y, #XCONST
        set_nargs 2
        b _SPksignalerr
endsp setqsym

/*
 * ===========================================================================
 * UNBINDING SUBPRIMS
 * ===========================================================================
 */

/* ported from ppc-spentry.s:6902-6910 (PPC64 branch) */
/* Unbind one binding frame */
spentry unbind
        ldr imm1, [rcontext, #tcr.db_link]
        ldr imm2, [rcontext, #tcr.tlb_pointer]
        ldr imm3, [imm1, #binding.sym]
        ldr temp1, [imm1, #binding.val]
        ldr imm1, [imm1, #binding.link]
        str temp1, [imm2, imm3]
        str imm1, [rcontext, #tcr.db_link]
        ret
endsp unbind

/* ported from ppc-spentry.s:6912-6923 (PPC64 branch) */
/* Unbind imm0 binding frames (imm0 is unboxed count, NOT a fixnum) */
spentry unbind_n
        ldr imm1, [rcontext, #tcr.db_link]
        ldr imm2, [rcontext, #tcr.tlb_pointer]
1:
        sub imm0, imm0, #1
        ldr imm3, [imm1, #binding.sym]
        ldr temp1, [imm1, #binding.val]
        cmp imm0, #0
        ldr imm1, [imm1, #binding.link]
        str temp1, [imm2, imm3]
        b.ne 1b
        str imm1, [rcontext, #tcr.db_link]
        ret
endsp unbind_n

/* ported from ppc-spentry.s:6928-6938 (PPC64 branch) */
/* Unbind back to db_link value in imm0. Clobbers imm1, imm2, imm5, arg_x, arg_y */
spentry unbind_to
        /* Unbind until db_link == imm0.  Clobbers imm1,imm2,arg_x,arg_y.
           NOTE: unlike PPC (imm5 scratch), we must NOT touch imm5==nargs,
           because .SPthrow relies on nargs surviving this call. */
        ldr imm1, [rcontext, #tcr.db_link]
        ldr imm2, [rcontext, #tcr.tlb_pointer]
1:
        ldr arg_x, [imm1, #binding.sym]
        ldr arg_y, [imm1, #binding.val]
        ldr imm1, [imm1, #binding.link]
        cmp imm0, imm1
        str arg_y, [imm2, arg_x]
        b.ne 1b
        str imm1, [rcontext, #tcr.db_link]
        ret
endsp unbind_to

/* ported from ppc-spentry.s:6963-6979 (PPC64 branch) */
/* Bind *INTERRUPT-LEVEL* to 0; check for pending interrupt if old value was negative */
spentry bind_interrupt_level_0
        /* ppc:6964-6979.  Bind *INTERRUPT-LEVEL* to 0; poll for a deferred
           interrupt if the old level was negative. */
        ldr imm4, [rcontext, #tcr.tlb_pointer]              /* ppc:6964 */
        ldr temp0, [imm4, #INTERRUPT_LEVEL_BINDING_INDEX]   /* ppc:6965 old level */
        ldr imm1, [rcontext, #tcr.db_link]                  /* ppc:6966 */
        mov imm3, #INTERRUPT_LEVEL_BINDING_INDEX            /* ppc:6968 */
        vpush1 temp0                    /* ppc:6969 binding frame: old value */
        vpush1 imm3                     /* ppc:6970               tlb index   */
        vpush1 imm1                     /* ppc:6971               prev db_link*/
        str xzr, [imm4, #INTERRUPT_LEVEL_BINDING_INDEX]     /* ppc:6972 tlb[il]=0 */
        str vsp, [rcontext, #tcr.db_link]                   /* ppc:6973 */
        cmp temp0, #0                   /* ppc:6967 cmpri(temp0,0) */
        b.eq 2f                         /* ppc:6974 beqlr -> old level 0, return */
        mov nargs, temp0                /* ppc:6975 mr nargs,temp0 */
        cmp temp0, #0                   /* recompute (ARM64 single NZCV) for bgt */
        b.gt 1f                         /* ppc:6976 bgt 1f -> old level>0, skip load */
        ldr nargs, [rcontext, #tcr.interrupt_pending]       /* ppc:6977 */
1:      cmp nargs, #0                   /* ppc:6978 trgti(nargs,0): trap if nargs>0 */
        b.le 2f
        uuo_interrupt_now                /* uuo_misc 4 at pin 9c61574 (was 3 @115b7aa, before he
                                            inserted uuo_debug_trap at 3) */
2:      ret                             /* ppc:6979 blr */
endsp bind_interrupt_level_0

/* ported from ppc-spentry.s:6983-6994 (PPC64 branch) */
/* Bind *INTERRUPT-LEVEL* to -1 (disable interrupts) */
spentry bind_interrupt_level_m1
        /* ppc:6984-6994.  Bind *INTERRUPT-LEVEL* to -1 (disable interrupts). */
        mov imm2, #(-fixnumone)         /* ppc:6984 li imm2,-fixnumone */
        mov imm3, #INTERRUPT_LEVEL_BINDING_INDEX            /* ppc:6985 */
        ldr imm4, [rcontext, #tcr.tlb_pointer]              /* ppc:6986 */
        ldr temp0, [imm4, #INTERRUPT_LEVEL_BINDING_INDEX]   /* ppc:6987 old level */
        ldr imm1, [rcontext, #tcr.db_link]                  /* ppc:6988 */
        vpush1 temp0                    /* ppc:6989 binding frame: old value */
        vpush1 imm3                     /* ppc:6990               tlb index   */
        vpush1 imm1                     /* ppc:6991               prev db_link*/
        str imm2, [imm4, #INTERRUPT_LEVEL_BINDING_INDEX]    /* ppc:6992 tlb[il]=-1 */
        str vsp, [rcontext, #tcr.db_link]                   /* ppc:6993 */
        ret                             /* ppc:6994 blr */
endsp bind_interrupt_level_m1

/* ported from ppc-spentry.s:6999-7011 (PPC64 branch) */
/* Bind *INTERRUPT-LEVEL* to value in arg_z */
spentry bind_interrupt_level
        /* ppc:7000-7011.  Bind *INTERRUPT-LEVEL* to arg_z; if arg_z==0, tail to
           _SPbind_interrupt_level_0. */
        cmp arg_z, #0                   /* ppc:7000 cmpri(arg_z,0) */
        mov imm3, #INTERRUPT_LEVEL_BINDING_INDEX            /* ppc:7001 */
        ldr imm4, [rcontext, #tcr.tlb_pointer]              /* ppc:7002 */
        ldr temp0, [imm4, #INTERRUPT_LEVEL_BINDING_INDEX]   /* ppc:7003 old level */
        ldr imm1, [rcontext, #tcr.db_link]                  /* ppc:7004 */
        b.eq _SPbind_interrupt_level_0  /* ppc:7005 beq -> bind to 0 */
        vpush1 temp0                    /* ppc:7006 binding frame: old value */
        vpush1 imm3                     /* ppc:7007               tlb index   */
        vpush1 imm1                     /* ppc:7008               prev db_link*/
        str arg_z, [imm4, #INTERRUPT_LEVEL_BINDING_INDEX]   /* ppc:7009 tlb[il]=arg_z */
        str vsp, [rcontext, #tcr.db_link]                   /* ppc:7010 */
        ret                             /* ppc:7011 blr */
endsp bind_interrupt_level

/* ported from ppc-spentry.s:7018-7047 (PPC64 branch) */
/* Unbind *INTERRUPT-LEVEL*; check for pending interrupt if transitioning negative->non-negative */
spentry unbind_interrupt_level
        /* ppc:7019-7047.  Unbind *INTERRUPT-LEVEL*; poll for a pending interrupt
           if the level goes from negative to non-negative.  nargs is often live,
           so save/restore it around any poll. */
        ldr imm0, [rcontext, #tcr.flags]                    /* ppc:7019 */
        ldr imm2, [rcontext, #tcr.tlb_pointer]              /* ppc:7020 */
        tst imm0, #(1<<TCR_FLAG_BIT_PENDING_SUSPEND)        /* ppc:7021 andi. cr0 */
        ldr imm1, [rcontext, #tcr.db_link]                  /* ppc:7022 */
        ldr temp1, [imm2, #INTERRUPT_LEVEL_BINDING_INDEX]   /* ppc:7023 old level */
        b.ne 3f                         /* ppc:7024 bne -> missed-suspend path */
1:      /* ppc:7025 (PPC label 0).  temp1=old level, imm1=binding, imm2=tlb_ptr. */
        mov temp0, temp1                /* preserve old level for the cr1 test  */
        ldr temp1, [imm1, #binding.val] /* ppc:7026 restored (new) level         */
        ldr imm1, [imm1, #binding.link] /* ppc:7027 new db_link                  */
        str temp1, [imm2, #INTERRUPT_LEVEL_BINDING_INDEX]   /* ppc:7029 */
        str imm1, [rcontext, #tcr.db_link]                  /* ppc:7030 */
        cmp temp0, #0                   /* ppc:7025 cmpri(cr1,old,0), adjacent    */
        b.ge 2f                         /* ppc:7031 bgelr cr1: old>=0 -> return   */
        cmp temp1, #0                   /* ppc:7028 cmpri(cr0,new,0), adjacent    */
        b.lt 2f                         /* ppc:7032 bltlr cr0: new<0 -> return     */
        mov imm2, nargs                 /* ppc:7033 save nargs across the poll     */
        check_pending_interrupt         /* ppc:7034 check_pending_interrupt(cr1)   */
        mov nargs, imm2                 /* ppc:7035 restore nargs                  */
2:      ret                             /* ppc:7036 blr                            */
3:      /* ppc:7037 (PPC label 5).  Missed a suspend; force suspend now if we are
           restoring interrupt level to -1 or greater. */
        cmp temp1, #(-2<<fixnumshift)   /* ppc:7039 cmpri(old,-2<<fixnumshift)     */
        b.ne 1b                         /* ppc:7040 bne 0b                          */
        ldr imm0, [imm1, #binding.val]  /* ppc:7041 restored value                 */
        cmp imm0, temp1                 /* ppc:7042 cmpr(restored,old)              */
        b.eq 1b                         /* ppc:7043 beq 0b                          */
        mov imm0, #(1<<fixnumshift)     /* ppc:7044 li imm0,1<<fixnumshift          */
        str imm0, [imm2, #INTERRUPT_LEVEL_BINDING_INDEX]    /* ppc:7045 */
        uuo_suspend_now                 /* ppc:7046 (his misc 4) */
        b 1b                            /* ppc:7047 b 0b                            */
endsp unbind_interrupt_level

/*
 * ===========================================================================
 * VALUES / MULTIPLE-VALUE SUBPRIMS
 * ===========================================================================
 */

/* ret1valn returns "1 multiple value" when a called function does not
   return multiple values.  Its presence on the stack (as a return
   address) identifies the frame to multiple-value-returning code.
   ppc-spentry.s:1167-1179; pmcl-kernel.c:2109 takes &ret1valn for
   lisp_global(RET1VALN).  Marker-frame restore order per this file's
   frame idiom (lr from savelr, vsp, fn, discard).

   ORDERING (16m5t root cause): in PPC this is a STANDALONE _exportfn
   defined BEFORE _spentry(values) (ppc:1171), and `values' falls
   straight through into local_label(return_values) (ppc:1214-1216).  A
   prior port mis-inserted this block BETWEEN `values' and
   `return_values', so `values' fell into ret1valn and ALWAYS delivered 1
   value (set_nargs 1) -- the mv-return branch logic at return_values was
   only reachable via nvalret's explicit `b'.  Kept here, ABOVE `spentry
   values', so the fall-through values -> return_values is intact. */
        .globl C(ret1valn)
C(ret1valn):
        ldr vsp, [sp, #lisp_frame.savevsp]      /* ppc:1173 savevsp@8 (unpaired)     */
        ldp fn, lr, [sp, #lisp_frame.savefn]    /* ppc:1175,1172 savefn@16 + savelr@24 */
        add sp, sp, #lisp_frame.size            /* ppc:1176 discard     */
        vpush1 arg_z                            /* ppc:1177             */
        set_nargs 1                             /* ppc:1178             */
        ret                                     /* ppc:1179 blr         */

/* nvalret (ppc-spentry.s:1270-1276): come here with saved context on top
 * of the stack.  Its return pc lives in savelr, so load it straight into
 * lr to match the `values' entry, then FALL THROUGH into the shared body
 * below.  PPC/ARM32 keep nvalret past `values' and branch back up to a
 * global `return_values'; falling through instead drops that branch and
 * the exported label (the two spentries are contiguous now that spentry-C
 * and spentry-D are one file).  pmcl-kernel.c:2110 takes &nvalret for
 * lisp_global(LEXPR_RETURN) (PPC exports it the same way, ppc:1267-1271). */
        .globl C(nvalret)
spentry nvalret
C(nvalret):
        ldr temp0, [sp, #lisp_frame.savevsp]    /* ppc:1273 savevsp@8 (unpaired)     */
        ldp fn, lr, [sp, #lisp_frame.savefn]    /* ppc:1274,1272 savefn@16 + savelr@24 (->lr) */
        discard_lisp_frame                      /* ppc:1275                */
        /* FALL THROUGH into `values'/the shared body (ppc:1276 was a branch). */
endsp nvalret

/* ported from ppc-spentry.s:1214-1248 (PPC64 branch) */
/* Return multiple values. nargs = count (fixnum), values on stack */
spentry values
        /* ppc-spentry.s:1214-1265 (PPC64 branch).  temp0 = entry vsp (VERIFIED
           cont-71); nargs = boxed value count.  No loc_pc register in this
           design -- lr(x30) carries the return pc and STAYS there through the
           whole body (ARM32 arm-spentry.s:596-635 does the same): PPC's `mflr
           loc_pc' has no analog because we never move it out of lr.  Reached
           two ways: called at `values' (pc already in lr), or fallen into from
           `nvalret' just above (which loaded the pc from savelr into lr) -- so
           the body is single-channel on lr and needs no shared label. */
        /* ppc:1217 ref_global(imm0,ret1val_addr): load the ret1val_addr global.
           No ref_global / lisp_globals idiom exists for ARM64 in this file or
           its includes (see spentry-A-alloc-numbers.s:25-26 and the open
           PORT-TODO at spentry-D-call-builtins.s:112-113). */
        ref_global imm0, ret1val_addr   /* ppc:1217 (idiom: arm64-globals-proposed.s) */
        mov arg_z, rnil                 /* ppc:1218 li arg_z,nil_value           */
        cmp nargs, #(4096-(dnode_size+dnode_size))  /* ppc:1221 cmpri cr2        */
        b.ge 2f                         /* ppc:1224 bge cr2 -> too many values   */
        cmp imm0, lr                    /* ppc:1222 cmpr cr1 (imm0==ret1val_addr?)*/
        b.eq 3f                         /* ppc:1225 beq cr1 -> return to real caller*/
        /* ppc:1226 mtlr loc_pc: no-op here -- the return pc is already in lr.   */
        add imm0, vsp, nargs            /* ppc:1227 add imm0,nargs,vsp            */
        cmp nargs, #fixnum_one          /* ppc:1223 cmpri cr0, recomputed here    */
        b.lt 1f                         /* ppc:1228 blt cr0 -> no values, keep nil*/
        ldr arg_z, [imm0, #-node_size]  /* ppc:1229 top value                     */
1:      /* ppc:1230 */
        mov vsp, temp0                  /* ppc:1231 restore entry vsp             */
        ret                             /* ppc:1232 blr                           */
2:      /* ppc:1234 */
        /* ppc:1235 uuo_interr(error_too_many_values,nargs) -- udf
           unary-misc, sub = errnum 7 (namespace: spentry-A trap block). */
        uuo_interr error_too_many_values, nargs /* PROPOSED ext (globals-proposed.s) */
        b 2b                            /* ppc:1236 */
3:      /* ppc:1239 return multiple values to real caller */
        ldr lr, [sp, #lisp_frame.savelr]      /* ppc:1240 ldr loc_pc (straight to lr) */
        add imm1, vsp, nargs            /* ppc:1241 add imm1,nargs,vsp            */
        ldr imm0, [sp, #lisp_frame.savevsp]   /* ppc:1242                        */
        ldr fn, [sp, #lisp_frame.savefn]      /* ppc:1243                        */
        /* ppc:1245 mtlr loc_pc: no-op -- savelr was loaded straight into lr.    */
        discard_lisp_frame              /* ppc:1247                              */
        cmp imm1, imm0                  /* ppc:1244 cmpr cr0, recomputed post-discard*/
        b.eq 7f                         /* ppc:1248 beqlr cr0 -> already in place */
        cmp nargs, #fixnum_one          /* ppc:1246 cmpri cr1                     */
        b.ne 4f                         /* ppc:1249 bne cr1                       */
        ldr arg_z, [vsp]                /* ppc:1250 ldr arg_z,0(vsp)              */
        mov vsp, imm0                   /* ppc:1251                              */
        vpush1 arg_z                    /* ppc:1252                              */
        ret                             /* ppc:1253 blr                           */
4:      /* ppc:1254 */
        cmp nargs, #fixnum_one          /* ppc:1246 cr1 recomputed                */
        b.lt 6f                         /* ppc:1255 blt cr1                       */
        mov imm2, #fixnum_one           /* ppc:1256                              */
5:      /* ppc:1257 */
        cmp imm2, nargs                 /* ppc:1258 cmpr cr0                       */
        add imm2, imm2, #fixnum_one     /* ppc:1259                              */
        ldr arg_z, [imm1, #-node_size]! /* ppc:1260 ldru pre-decrement            */
        str arg_z, [imm0, #-node_size]! /* ppc:1261 push pre-decrement            */
        b.ne 5b                         /* ppc:1262 bne cr0                        */
6:      /* ppc:1263 */
        mov vsp, imm0                   /* ppc:1264                              */
        ret                             /* ppc:1265 blr                           */
7:      /* ppc:1248 beqlr cr0 target: values already in place                     */
        ret
endsp values

/* ported from ppc-spentry.s:1424-1579 (PPC64 branch)
 *
 * keyword_bind: Function-entry keyword processor.
 *
 * Entry conditions (PPC convention, mapped to ARM64):
 *   nargs          = actual arg count (boxed fixnum)
 *   imm0           = canonical required+optional count (boxed fixnum)
 *   keyword_count  = number of defined keyword args (boxed fixnum, imm3/x3)
 *   keyword_vector = vector of keyword specifier symbols (temp3/x16)
 *   keyword_flags  = flag word, pre-seeded by caller (imm2/x2)
 *   nfn            = the function being entered (temp2/x15)
 *   fn             = caller's fn, to be saved (VOLATILE x7)
 *   lr             = return pc to record in the new frame
 *
 * DEVIATIONS from literal PPC transcription (forced by ISA):
 *   [D1] PPC keeps up to four CR results live simultaneously.  AArch64 has ONE
 *        NZCV.  Where PPC relies on a stale CR, we recompute the comparison or
 *        stash a boolean with `cset` into a free scratch.
 *   [D2] PPC pushes NILs via imm5 (=nargs).  Here nargs is still live, so we
 *        push the dedicated NIL register (rnil) instead.
 *   [D3] `loc_pc` -> lr.  keyword_bind saves lr into lisp_frame.savelr and
 *        returns with `ret`.
 */

/* Register aliases for keyword_bind (PPC ppc-spentry.s:1414-1422 mapping;
   temp3 = x15 on Matt's map -- an earlier revision mis-aliased the vector
   to x16/temp4, 16m5o).  nfn (= temp2 = x14) comes from the register map. */
keyword_flags  .req x2    /* imm2 */
keyword_count  .req x3    /* imm3 */
keyword_vector .req x15   /* temp3 (= fname; dead by keyword-entry time)  */
varptr         .req x19   /* save0 */
valptr         .req x20   /* save1 */
limit          .req x21   /* save2 */

spentry keyword_bind
        /* ARM64-DEVIATION (16m5o): PPC's keyword_bind built the fn's ONLY
           lisp frame here (ppc:1432-1438) because PPC2 emits just save-lr
           before it -- LR survives in loc_pc.  This design has no loc_pc:
           the blr that reaches us already clobbered lr, so the compiled
           prologue (save-lisp-context-variable) saved the caller's
           fn/lr/vsp in ITS frame -- with identical savevsp arithmetic
           (vsp + stack-arg bytes = caller's vsp).  Building a second
           frame here recorded a bogus savelr (mid-prologue pc) that
           poisoned unwinds and leaked cstack.  Entry contract now:
           compiled prologue owns the frame; fn already = nfn. */
        /* ppc:1445-1450: prime the pair-of-NILs loop. */
        mov arg_z, #0                           /* ppc:1446 li arg_z,0             */
        sub imm1, nargs, imm0                   /* ppc:1447 sub imm1,nargs,imm0   */
        mov imm4, vsp                           /* ppc:1448 mr imm4,vsp (for odd-keywords error) */
        cmp arg_z, imm3                         /* ppc:1444 cmpri(cr0,imm3,0) [D1]: arg_z==0 here, so this == keycount-vs-0, same predicate the in-loop cmp at 2: recomputes */
        b 3f                                    /* ppc:1450                        */
2:                                              /* ppc:1451-1456                   */
        add arg_z, arg_z, #fixnumone            /* ppc:1452 addi arg_z,fixnum_one  */
        cmp arg_z, imm3                         /* ppc:1453 cmplr(cr0,arg_z,imm3)  */
        vpush1 rnil                             /* ppc:1454-1455 vpush NIL [D2]    */
        vpush1 rnil
3:
        b.ne 2b                                 /* ppc:1458 bne cr0,2b             */
        /* ppc:1459-1461: if no pairs, done; if odd count, error. */
        cmp imm1, #0                            /* ppc:1449 cmpri(cr1,imm1,0)      */
        b.le kbind_ret                          /* ppc:1460 blelr cr1 (imm1<=0)    */
        tst imm1, #fixnumone                    /* ppc:1459 andi. arg_z,imm1,fixnum_one */
        b.ne kbind_odd_keywords                 /* ppc:1461 bne cr0,odd_keywords   */

        /* ppc:1465-1467: save the non-volatile ptr regs we are about to use. */
        vpush1 limit                            /* ppc:1465 vpush(limit)           */
        vpush1 valptr                           /* ppc:1466 vpush(valptr)          */
        vpush1 varptr                           /* ppc:1467 vpush(varptr)          */
        /* ppc:1469-1476: recompute user-arg pointer (stack may have moved).
           imm3 is a boxed fixnum == keycount*node_size. */
        add imm4, vsp, imm3                     /* ppc:1469 add imm4,vsp,imm3     */
        add imm4, imm4, imm3                    /* ppc:1470 add imm4,imm4,imm3    */
        add imm4, imm4, #3*node_size            /* ppc:1471 addi imm4,3*node_size  */
        mov varptr, imm4                        /* ppc:1473 mr varptr,imm4         */
        add limit, vsp, #3*node_size            /* ppc:1474 la limit,3*node_size(vsp) */
        mov valptr, limit                       /* ppc:1475 mr valptr,limit        */
        mov arg_z, imm1                         /* ppc:1476 mr arg_z,imm1          */
4:                                              /* ppc:1477-1489: slide pairs up, NIL sources */
        subs arg_z, arg_z, #(2<<fixnumshift)    /* ppc:1479 subi + cmpri(cr0,0)    */
        ldr arg_x, [varptr, #node_size*0]       /* ppc:1481 ldr(arg_x,0(varptr))   */
        ldr arg_y, [varptr, #node_size*1]       /* ppc:1482 ldr(arg_y,8(varptr))   */
        str rnil, [varptr, #node_size*0]        /* ppc:1483 str(nil,0(varptr)) [D2]*/
        str rnil, [varptr, #node_size*1]        /* ppc:1484 str(nil,8(varptr)) [D2]*/
        add varptr, varptr, #node_size*2        /* ppc:1485 la varptr,16(varptr)   */
        str arg_x, [valptr, #node_size*0]       /* ppc:1486 str(arg_x,0(valptr))   */
        str arg_y, [valptr, #node_size*1]       /* ppc:1487 str(arg_y,8(valptr))   */
        add valptr, valptr, #node_size*2        /* ppc:1488 la valptr,16(valptr)   */
        b.ne 4b                                 /* ppc:1489 bne cr0,4b             */

        /* ppc:1501: remember top-of-values for the badkeys conslist. */
        mov imm4, valptr                        /* ppc:1501 mr imm4,valptr         */
5:                                              /* ppc:1502-1515: per supplied pair */
        /* load key/value with pre-decrement (PPC ldru). */
        ldr arg_z, [valptr, #-node_size]!       /* ppc:1504 ldru(arg_z,-8(valptr)) */
        ldr arg_y, [valptr, #-node_size]!       /* ppc:1505 ldru(arg_y,-8(valptr)) */
        ref_nrs_symbol arg_x, kallowotherkeys   /* ppc:1507 */
        cmp arg_x, arg_z                        /* ppc:1509 cmpr(cr6,arg_x,arg_z)  */
        cset temp0, eq                          /* [D1] stash cr6 (is-aok) into temp0 */
        b.ne 6f                                 /* ppc:1511 bne cr6,6f             */
        tst keyword_flags, #(16<<fixnumshift)   /* ppc:1503 cmpri(cr0,keyword_flags,16<<fixnumshift) already seen? */
        b.ne 6f                                 /* ppc:1512 bge cr0,6f             */
        orr keyword_flags, keyword_flags, #(16<<fixnumshift) /* ppc:1513 ori      */
        cmp arg_y, rnil                         /* ppc:1506/1514 cmpri(cr1,arg_y,nil_value) */
        b.eq 6f                                 /* ppc:1514 beq cr1,6f             */
        orr keyword_flags, keyword_flags, #fixnumone /* ppc:1515 ori: note aok active */
6:                                              /* ppc:1516-1520                   */
        mov imm1, #misc_data_offset             /* ppc:1518 li imm1,misc_data_offset */
        mov imm0, #0                            /* ppc:1519 li imm0,0              */
        b 8f                                    /* ppc:1520 b 8f                   */
7:                                              /* ppc:1521-1536: scan keyword vector */
        add imm0, imm0, #fixnumone              /* ppc:1522 addi imm0,fixnum_one   */
        ldr arg_x, [keyword_vector, imm1]       /* ppc:1524 ldrx(arg_x,keyword_vector,imm1) */
        add imm1, imm1, #fixnumone              /* ppc:1526 addi imm1,fixnum_one (==node_size) */
        cmp arg_x, arg_z                        /* ppc:1525 cmpr(cr0,arg_x,arg_z)  */
        b.ne 8f                                 /* ppc:1527 bne cr0,8f (no match)  */
        /* matched this defined keyword */
        add imm0, imm0, imm0                    /* ppc:1528 add imm0,imm0,imm0 (pair stride) */
        sub imm0, varptr, imm0                  /* ppc:1529 sub imm0,varptr,imm0   */
        ldr arg_x, [imm0, #0]                   /* ppc:1530 ldr(arg_x,0(imm0)) current supplied-p */
        cmp arg_x, rnil                         /* ppc:1531 cmpri(cr0,arg_x,nil_value) */
        b.ne 9f                                 /* ppc:1533 bne cr0,9f already supplied */
        add arg_z, rnil, #t_offset              /* ppc:1532 li arg_z,t_value       */
        str arg_y, [imm0, #node_size]           /* ppc:1534 str(arg_y,node_size(imm0)) store value */
        str arg_z, [imm0, #0]                   /* ppc:1535 str(arg_z,0(imm0)) supplied-p = T */
        b 9f                                    /* ppc:1536 b 9f                   */
8:                                              /* ppc:1537-1542                   */
        cmp imm0, imm3                          /* ppc:1523/1538 [D1] recompute cr1 (idx==keycount) */
        b.ne 7b                                 /* ppc:1538 bne cr1,7b more keywords to try */
        /* unknown keyword */
        cbnz temp0, 9f                          /* ppc:1541 beq cr6,9f (it was :aok) [D1] */
        orr keyword_flags, keyword_flags, #(2<<fixnumshift) /* ppc:1542 ori: note unknown seen */
9:                                              /* ppc:1543-1544                   */
        cmp valptr, limit                       /* ppc:1544 [D1] recompute cr7 (valptr==limit?) */
        b.ne 5b                                 /* ppc:1544 bne cr7,5b             */
        /* ppc:1545-1558: restore ptr regs, then act on the flags. */
        vpop1 varptr                            /* ppc:1545 vpop(varptr)           */
        vpop1 valptr                            /* ppc:1546 vpop(valptr)           */
        vpop1 limit                             /* ppc:1547 vpop(limit)            */
        /* All keyword/value pairs have been processed.
           If we saw an unknown keyword and did not expect to, error.
           Unless bit 2 is set in keyword_flags, discard the pairs. */
        and imm0, keyword_flags, #((fixnumone)|(2<<fixnumshift)) /* ppc:1552 andi. */
        cmp imm0, #(2<<fixnumshift)             /* ppc:1553 cmpri(cr0,imm0,2<<fixnumshift) unknown+not-allowed? */
        b.eq kbind_badkeys                      /* ppc:1554 beq- cr0,badkeys       */
        tst keyword_flags, #(4<<fixnumshift)    /* ppc:1555 andi. imm2,keyword_flags,4<<fixnumshift keep pairs? */
        b.ne kbind_ret                          /* ppc:1556 bnelr cr0              */
        mov vsp, imm4                           /* ppc:1557 mr vsp,imm4 (discard key/value pairs) */
kbind_ret:
        ret                                     /* ppc:1558 blr; the fn's frame is the
                                                   compiled prologue's (see entry note) */

        /* ppc:1569-1579: error tails. */
kbind_odd_keywords:
        mov vsp, imm4                           /* ppc:1570 mr vsp,imm4            */
        mov nargs, imm1                         /* ppc:1571 mr nargs,imm1          */
        b kbind_signal                          /* ppc:1572 b 1f                   */
kbind_badkeys:
        sub nargs, imm4, vsp                    /* ppc:1574 sub nargs,imm4,vsp     */
kbind_signal:
        bl _SPconslist                          /* ppc:1576 bl _SPconslist          */
        mov arg_y, #XBADKEYS                    /* ppc:1577 li arg_y,XBADKEYS      */
        set_nargs 2                             /* ppc:1578 set_nargs(2)           */
        b _SPksignalerr                         /* ppc:1579 b _SPksignalerr        */
endsp keyword_bind

.unreq keyword_flags
.unreq keyword_count
.unreq keyword_vector
.unreq varptr
.unreq valptr
.unreq limit

/* ported from ppc-spentry.s:2025-2029 (PPC64 branch) */
/* Allocate &rest arg list on stack */
spentry stack_rest_arg
        /* ppc:2025-2028.  As in the heap-consed cases, only stack-cons the &rest
           arg.  No required args consumed -> subtract 0. */
        mov imm0, #0                    /* ppc:2026 li imm0,0 */
        /* vpush_argregs() (ppc-macros.s:329) inlined; cmplri -> unsigned.
           nargs is a boxed fixnum, one arg == node_size bytes (set_nargs). */
        cmp nargs, #0                   /* cmplri(cr0,nargs,0) */
        b.eq 3f                         /* nargs==0 -> push nothing */
        cmp nargs, #(node_size*2)       /* cmplri(cr1,nargs,node_size*2) */
        b.lo 2f                         /* nargs<2 -> only arg_z */
        b.eq 1f                         /* nargs==2 -> arg_y,arg_z */
        vpush1 arg_x                    /* nargs>=3 */
1:      vpush1 arg_y
2:      vpush1 arg_z
3:
        b _SPstack_cons_rest_arg        /* ppc:2028 */
endsp stack_rest_arg

/* ported from ppc-spentry.s:2031-2033 (PPC64 branch) */
/* Required stack rest arg variant */
spentry req_stack_rest_arg
        /* ppc:2031-2033.  imm0 (count of required args already consumed) is
           supplied by the caller; just vpush the arg regs and cons. */
        /* vpush_argregs() (ppc-macros.s:329) inlined; own labels. */
        cmp nargs, #0                   /* cmplri(cr0,nargs,0) */
        b.eq 3f                         /* nargs==0 -> push nothing */
        cmp nargs, #(node_size*2)       /* cmplri(cr1,nargs,node_size*2) */
        b.lo 2f                         /* nargs<2 -> only arg_z */
        b.eq 1f                         /* nargs==2 -> arg_y,arg_z */
        vpush1 arg_x                    /* nargs>=3 */
1:      vpush1 arg_y
2:      vpush1 arg_z
3:
        b _SPstack_cons_rest_arg        /* ppc:2033 */
endsp req_stack_rest_arg

/* ported from ppc-spentry.s:2035-2069 (PPC64 branch) */
/* Cons up &rest arg list on stack */
spentry stack_cons_rest_arg
        /* ppc:2035-2063.  imm0 = required args already consumed; nargs = total.
           Cons the rest args into a list on the temp stack (heap-cons if the
           block would be too large).  arg_z accumulates the list. */
        /* Labels 8/9 for the ble/bge targets (not 2/3): the inlined
           tsp_alloc_var_boxed_nz macro emits its own local 1:/2:, so a forward
           `2f` here would bind to the macro's label.  Matches throw/nthrowvalues
           high-label convention. */
        sub imm1, nargs, imm0           /* ppc:2036 imm1 = rest count (bytes)   */
        mov arg_z, rnil                 /* ppc:2039 li arg_z,nil_value          */
        cmp imm1, #0                    /* ppc:2037 cmpri(cr0,imm1,0), signed   */
        b.le 8f                         /* ppc:2040 ble cr0 -> always push cell */
        cmp imm1, #((4096-dnode_size)/2)/* ppc:2038 cmpri(cr1,...), signed      */
        b.ge 9f                         /* ppc:2041 bge cr1 -> too big, heap    */
        add imm1, imm1, imm1            /* ppc:2042 imm1 *= 2 -> cons byte count */
        /* dnode_align(imm2,imm1,tsp_frame.fixed_overhead) (ppc:2043), inlined
           per spentry-A:447-448. */
        add imm2, imm1, #(tsp_frame.fixed_overhead + dnode_size - 1)
        and imm2, imm2, #0xfffffffffffffff0
        TSP_Alloc_Var_Boxed_nz imm2, imm3   /* ppc:2044 TSP_Alloc_Var_Boxed */
        add imm0, tsp, #(tsp_frame.data_offset + fulltag_cons)  /* ppc:2045     */
1:      /* ppc:2046 */
        cmp imm1, #cons.size            /* ppc:2047 cmpri(cr0,imm1,cons.size)   */
        sub imm1, imm1, #cons.size      /* ppc:2048 subi (plain sub: keep flags)*/
        vpop1 arg_x                     /* ppc:2049 vpop(arg_x)                 */
        str arg_z, [imm0, #cons.cdr]    /* ppc:2050 _rplacd(imm0,arg_z)         */
        str arg_x, [imm0, #cons.car]    /* ppc:2051 _rplaca(imm0,arg_x)         */
        mov arg_z, imm0                 /* ppc:2052 mr arg_z,imm0               */
        add imm0, imm0, #cons.size      /* ppc:2053 la imm0,cons.size(imm0)     */
        b.ne 1b                         /* ppc:2054 bne cr0 (pre-decr test)     */
        vpush1 arg_z                    /* ppc:2055 vpush(arg_z)                */
        ret                             /* ppc:2056 blr                         */
8:      /* ppc:2057 */
        TSP_Alloc_Fixed_Unboxed 0, imm3 /* ppc:2058 TSP_Alloc_Fixed_Unboxed (0)  */
        vpush1 arg_z                    /* ppc:2059 vpush(arg_z)                */
        ret                             /* ppc:2060 blr                         */
9:      /* ppc:2061 */
        TSP_Alloc_Fixed_Unboxed 0, imm3 /* ppc:2062 TSP_Alloc_Fixed_Unboxed (0)  */
        b _SPheap_cons_rest_arg         /* ppc:2063                             */
endsp stack_cons_rest_arg

/* ported from ppc-spentry.s:2300-2357 (PPC64 branch) */
/* Tail-call function with vsp args */
spentry tfuncallvsp
        /* ppc:2300-2306.  No args vpushed: recover saved context from the
           lisp_frame, discard it, tail-dispatch on temp0.  lr IS x30, so the
           savelr load replaces PPC's loc_pc load + mtlr.  All three loads read
           the current sp-relative frame and MUST precede discard_lisp_frame. */
        ldr lr, [sp, #lisp_frame.savelr]        /* ppc:2301+2304 loc_pc/mtlr */
        ldr fn, [sp, #lisp_frame.savefn]        /* ppc:2302 */
        ldr vsp, [sp, #lisp_frame.savevsp]      /* ppc:2303 */
        discard_lisp_frame                      /* ppc:2305 */
        b _SPfuncall                            /* ppc:2306 do_funcall() (temp0) */
endsp tfuncallvsp

/* ported from ppc-spentry.s:2359-2369 (PPC64 branch) */
/* Tail-call symbol with vsp args */
spentry tcallsymvsp
        /* ppc:2359-2366.  No args vpushed: recover saved context, discard the
           frame, tail-dispatch via fname's fcell.  Loads precede the discard. */
        ldr lr, [sp, #lisp_frame.savelr]        /* ppc:2360+2364 loc_pc/mtlr */
        ldr fn, [sp, #lisp_frame.savefn]        /* ppc:2361 */
        ldr vsp, [sp, #lisp_frame.savevsp]      /* ppc:2362 */
        discard_lisp_frame                      /* ppc:2363 */
        b _SPjmpsym                             /* ppc:2365 jump_fname (fname) */
endsp tcallsymvsp

/* ported from ppc-spentry.s:2393-2412 (PPC64 branch) */
/* Tail-call nfn with vsp args */
spentry tcallnfnvsp
        /* ppc:2393-2399.  No args vpushed: recover saved context, discard the
           frame, tail-dispatch to nfn's code.  Loads precede the discard. */
        ldr lr, [sp, #lisp_frame.savelr]        /* ppc:2394+2398 loc_pc/mtlr */
        ldr fn, [sp, #lisp_frame.savefn]        /* ppc:2395 */
        ldr vsp, [sp, #lisp_frame.savevsp]      /* ppc:2396 */
        discard_lisp_frame                      /* ppc:2397 */
        b _SPjmpnfn                             /* ppc:2399 jump_nfn() (nfn) */
endsp tcallnfnvsp

/* ported from ppc-spentry.s:3271-3285 (PPC64 branch) */
/* Get vcell address on stack */
spentry stkvcellvsp
        /* ppc-spentry.s:3271-3292.  Push 3 NILs, then overlay a stack-allocated
           value-cell on two of them, placement chosen by (oddp vsp). */
        mov arg_z, rnil                 /* ppc:3272 li arg_z,nil_value          */
        vpush1 arg_z                    /* ppc:3273 vpush(arg_z)                */
        vpush1 arg_z                    /* ppc:3274 vpush(arg_z)                */
        vpush1 arg_z                    /* ppc:3275 vpush(arg_z)                */
        mov imm1, #(node_size*3)        /* ppc:3276 li imm1,node_size*3         */
        add imm0, vsp, imm1             /* ppc:3277 imm0 = old vsp (pre-push)   */
        tst vsp, #(1<<3)                /* ppc:3278 andi. (oddp vsp)? flag-only;
                                           1<<word_shift, word_shift==3 (spentry-B) */
        mov imm1, #value_cell_header    /* ppc:3279 (flag-neutral)              */
        ldr arg_z, [imm0]               /* ppc:3280 reload value at old vsp (flag-neutral) */
        b.eq 1f                         /* ppc:3281 beq cr0 -> even-vsp layout  */
        str arg_z, [vsp, #(node_size*2)]/* ppc:3282                             */
        str imm1, [vsp, #node_size]     /* ppc:3283                             */
        add arg_z, vsp, #(fulltag_misc+node_size) /* ppc:3284 la                */
        str arg_z, [imm0]               /* ppc:3285                             */
        ret                             /* ppc:3286 blr                         */
1:      /* ppc:3287 */
        str arg_z, [vsp, #node_size]    /* ppc:3288                             */
        str imm1, [vsp]                 /* ppc:3289                             */
        add arg_z, vsp, #fulltag_misc   /* ppc:3290 la                          */
        str arg_z, [imm0]               /* ppc:3291                             */
        ret                             /* ppc:3292 blr                         */
endsp stkvcellvsp

/* Register aliases for the destructuring trio (ppc-spentry.s:3538-3541 mapping).
   whole_reg = temp1 (x14); arg_reg = temp3/fname (x16); keyvect_reg = temp2/nfn (x15).
   keyword_bind's .unreq freed x16/x15 above; these re-alias the same physical regs. */
whole_reg   .req x14   /* temp1 */
arg_reg     .req x16   /* temp3 = fname */
keyvect_reg .req x15   /* temp2 = nfn */

/* ported from ppc-spentry.s:3545-3568 (PPC64 branch) */
/* Macro lambda-list binding.  Discard the macro name (car of the whole form),
   then fall into the shared destructuring machinery at destbind1. */
spentry macro_bind
        mov whole_reg, arg_reg              /* ppc:3547 mr whole_reg,arg_reg       */
        and imm0, arg_reg, #fulltagmask     /* ppc:3548 extract_fulltag            */
        cmp arg_reg, rnil                   /* ppc:3549 cmpri(cr1,arg_reg,nil_value)*/
        b.eq 0f                             /* ppc:3551 beq cr1,0f (nil form ok)   */
        cmp imm0, #fulltag_cons             /* ppc:3550 cmpri(cr0,imm0,fulltag_cons)*/
        b.ne 1f                             /* ppc:3552 bne- cr0,1f (not a list)   */
0:
        ldr arg_reg, [arg_reg, #cons.cdr]   /* ppc:3554 _cdr: drop the macro name  */
        b destbind1
1:                                          /* ppc:3564-3568 */
        mov arg_y, #XCALLNOMATCH            /* ppc:3565 li arg_y,XCALLNOMATCH      */
        mov arg_z, whole_reg                /* ppc:3566 mr arg_z,whole_reg         */
        set_nargs 2                         /* ppc:3567 set_nargs(2)               */
        b _SPksignalerr                     /* ppc:3568 b _SPksignalerr            */
endsp macro_bind

/* ported from ppc-spentry.s:3571-3573 (PPC64 branch) */
/* Destructuring bind entry.  Saves whole form and branches to shared body. */
spentry destructuring_bind
        mov whole_reg, arg_reg              /* ppc:3572 mr whole_reg,arg_reg       */
        b destbind1                         /* ppc:3573 b destbind1                */
endsp destructuring_bind

/* ported from ppc-spentry.s:3575-3814 (PPC64 branch)
 *
 * destructuring_bind_inner: tree-walking argument destructuring.
 *
 * nargs holds the argument-descriptor longword (NOT a boxed count):
 *   bits 0-7   required count      bits 8-15  optional count
 *   bits 16-23 keyword count       bit  mask_initopt  hard (supplied-p) opts
 *   bit mask_keyp  &key present    bit mask_aok  &allow-other-keys
 *   bit mask_restp &rest present
 * arg_reg = the list being destructured ; keyvect_reg = keyword vector ;
 * whole_reg = whole form (for errors) ; imm4 = saved entry vsp.
 * No lisp frame is built (runs in caller's frame); returns with `ret'.
 */
spentry destructuring_bind_inner
        mov whole_reg, arg_z                /* ppc:3576 mr whole_reg,arg_z         */
destbind1:
        /* ppc:3580-3585: pull required & optional counts. */
        ands imm0, nargs, #0xff             /* ppc:3580-3583 req count; sets Z if 0*/
        ubfx imm1, nargs, #8, #8            /* ppc:3585 opt count                  */
        /* ppc:3593-3595: save entry vsp; branch past req loop if none. */
        mov imm4, vsp                       /* ppc:3594 mr imm4,vsp                */
        b.eq 2f                             /* ppc:3595 beq cr0,2f                 */
1:                                          /* ppc:3596-3612: bind required args   */
        cmp arg_reg, rnil                   /* ppc:3597 cmpri(cr7,arg_reg,nil)     */
        b.eq db_toofew                      /* ppc:3607 beq cr7,toofew             */
        and imm3, arg_reg, #fulltagmask     /* ppc:3598-3599 extract_fulltag       */
        cmp imm3, #fulltag_cons             /* ppc:3600                            */
        b.ne db_badlist                     /* ppc:3608 bne cr3,badlist            */
        subs imm0, imm0, #1                 /* ppc:3605-3606 subi + cmpri cr0      */
        ldr arg_x, [arg_reg, #cons.car]     /* ppc:3609 ldr(arg_x,cons.car)        */
        ldr arg_reg, [arg_reg, #cons.cdr]   /* ppc:3610 ldr(arg_reg,cons.cdr)      */
        vpush1 arg_x                        /* ppc:3611 vpush(arg_x)               */
        b.ne 1b                             /* ppc:3612 bne cr0,1b                 */
2:                                          /* ppc:3613-3615                       */
        cbz imm1, db_rest_keys              /* ppc:3614 beq cr1,rest_keys (no opts)*/
        tst nargs, #mask_initopt            /* ppc:3615 cmpri(cr2,imm2,0)          */
        b.ne db_opt_supp                    /* ppc:3615 bne cr2,opt_supp           */

        /* ppc:3616-3642: 'simple' &optionals -- no supplied-p, default NIL. */
db_simple_opt_loop:
        cmp arg_reg, rnil                   /* ppc:3618 cmpri(cr0,arg_reg,nil)     */
        b.eq db_default_simple_opt          /* ppc:3629 beq cr0,default_simple_opt */
        and imm3, arg_reg, #fulltagmask     /* ppc:3619-3621 extract_fulltag       */
        cmp imm3, #fulltag_cons             /* ppc:3621                            */
        b.ne db_badlist                     /* ppc:3630 bne cr3,badlist            */
        subs imm1, imm1, #1                 /* ppc:3626-3627 subi + cmpri cr1      */
        ldr arg_x, [arg_reg, #cons.car]     /* ppc:3631 ldr(arg_x,cons.car)        */
        ldr arg_reg, [arg_reg, #cons.cdr]   /* ppc:3632 ldr(arg_reg,cons.cdr)      */
        vpush1 arg_x                        /* ppc:3633 vpush(arg_x)               */
        b.ne db_simple_opt_loop             /* ppc:3634 bne cr1,simple_opt_loop    */
        b db_rest_keys                      /* ppc:3635 b rest_keys                */
db_default_simple_opt:                      /* ppc:3639-3642                       */
        subs imm1, imm1, #1                 /* ppc:3637 subi + cmpri cr1 [D1]      */
        vpush1 rnil                         /* ppc:3640 vpush(imm5/nil) [D2]       */
        b.ne db_default_simple_opt          /* ppc:3641 bne cr1                    */
        b db_rest_keys                      /* ppc:3642 b rest_keys                */

        /* ppc:3643-3671: &optionals WITH supplied-p vars. */
db_opt_supp:                                /* ppc:3644-3645                       */
        add arg_y, rnil, #t_offset          /* ppc:3645 li arg_y,t_value           */
db_opt_supp_loop:                           /* ppc:3646-3663                       */
        cmp arg_reg, rnil                   /* ppc:3647 cmpri(cr0,arg_reg,nil)     */
        b.eq db_default_hard_opt            /* ppc:3657 beq cr0,default_hard_opt   */
        and imm3, arg_reg, #fulltagmask     /* ppc:3648-3650 extract_fulltag       */
        cmp imm3, #fulltag_cons             /* ppc:3650                            */
        b.ne db_badlist                     /* ppc:3658 bne cr3,badlist            */
        subs imm1, imm1, #1                 /* ppc:3655-3656 subi + cmpri cr1      */
        ldr arg_x, [arg_reg, #cons.car]     /* ppc:3659 ldr(arg_x,cons.car)        */
        ldr arg_reg, [arg_reg, #cons.cdr]   /* ppc:3660 ldr(arg_reg,cons.cdr)      */
        vpush1 arg_x                        /* ppc:3661 vpush(arg_x)               */
        vpush1 arg_y                        /* ppc:3662 vpush(arg_y) supplied-p=T  */
        b.ne db_opt_supp_loop               /* ppc:3663 bne cr1,opt_supp_loop      */
        b db_rest_keys                      /* ppc:3664 b rest_keys                */
db_default_hard_opt:                        /* ppc:3668-3671                       */
        subs imm1, imm1, #1                 /* ppc:3666 subi + cmpri cr1 [D1]      */
        vpush1 rnil                         /* ppc:3669 vpush(nil) value [D2]      */
        vpush1 rnil                         /* ppc:3670 vpush(nil) supplied-p      */
        b.ne db_default_hard_opt            /* ppc:3671 bne cr1                    */

db_rest_keys:                               /* ppc:3672-3677                       */
        tst nargs, #mask_restp              /* ppc:3674 bne cr5,have_rest          */
        b.ne db_have_rest
        tst nargs, #mask_keyp               /* ppc:3675 bne cr4,have_keys          */
        b.ne db_have_keys
        cmp arg_reg, rnil                   /* ppc:3673 cmpri(cr0,arg_reg,nil)     */
        b.ne db_toomany                     /* ppc:3676 bne cr0,toomany            */
        ret                                 /* ppc:3677 blr                        */
db_have_rest:                               /* ppc:3678-3680                       */
        vpush1 arg_reg                      /* ppc:3678 vpush(arg_reg)             */
        tst nargs, #mask_keyp               /* ppc:3679 beqlr cr4 (not keyp)       */
        b.eq db_ret                         /* ppc:3680 beqlr                      */
db_have_keys:                               /* ppc:3681-3685                       */
        /* Ensure arg_reg contains a proper, even-length list; length <= 512.
           imm0 is a plain 256-countdown budget (cheap circularity check). */
        mov imm0, #256                      /* ppc:3684 li imm0,256                */
        mov arg_x, arg_reg                  /* ppc:3685 mr arg_x,arg_reg           */
db_count_keys_loop:                         /* ppc:3686-3712                       */
        cmp arg_x, rnil                     /* ppc:3694 cmpri(cr0,arg_x,nil)       */
        b.eq db_counted_keys                /* ppc:3697 beq cr0,counted_keys       */
        and imm3, arg_x, #fulltagmask       /* ppc:3687-3689 extract_fulltag       */
        cmp imm3, #fulltag_cons             /* ppc:3689                            */
        b.ne db_badlist                     /* ppc:3698 bne cr3,badlist            */
        subs imm0, imm0, #1                 /* ppc:3695-3696 subi + cmpri cr4      */
        b.mi db_toomany                     /* ppc:3707 blt cr4,toomany            */
        ldr arg_x, [arg_x, #cons.cdr]      /* ppc:3699 ldr(arg_x,cons.cdr)        */
        cmp arg_x, rnil                     /* ppc:3708 cmpri(cr0,arg_x,nil)       */
        b.eq db_badkeys                     /* ppc:3709 beq cr0,db_badkeys (odd)   */
        and imm3, arg_x, #fulltagmask       /* ppc:3700-3702 extract_fulltag       */
        cmp imm3, #fulltag_cons             /* ppc:3702                            */
        b.ne db_badlist                     /* ppc:3710 bne cr3,badlist            */
        ldr arg_x, [arg_x, #cons.cdr]      /* ppc:3711 ldr(arg_x,cons.cdr)        */
        b db_count_keys_loop                /* ppc:3712 b count_keys_loop          */
db_counted_keys:                            /* ppc:3713-3732                       */
        /* For each defined keyword var push a (value,supplied-p)=(NIL,NIL). */
        ubfx imm0, nargs, #16, #8           /* ppc:3717 keyword count              */
        mov imm2, imm0                      /* ppc:3718 save keycount              */
        cbz imm0, db_push_pair_done         /* ppc:3717/3727 extrwi. + bne [D1]    */
db_push_pair_loop:                          /* ppc:3721-3725                       */
        subs imm0, imm0, #1                 /* ppc:3722-3723 subi + cmpri cr0      */
        vpush1 rnil                         /* ppc:3724 vpush(nil) [D2]            */
        vpush1 rnil                         /* ppc:3725 vpush(nil)                 */
        b.ne db_push_pair_loop              /* ppc:3727 bne cr0,push_pair_loop     */
db_push_pair_done:
        lsl imm2, imm2, #dnode_shift        /* ppc:3728 slwi pairs -> bytes        */
        add imm2, vsp, imm2                 /* ppc:3729 add imm2,vsp,imm2          */
        /* imm1 accumulates flags: bit0 = unknown-keywords-allowed (seed from
           mask_aok), bit1 = seen-:aok, unknown-keyword count in bits >= 2. */
        mov imm0, #0                        /* ppc:3730 li imm0,0                  */
        tst nargs, #mask_aok                /* ppc:3731 extrwi imm1,nargs,1,mask_aok*/
        cset imm1, ne                       /* imm1 = aok-allowed ? 1 : 0          */
        ubfx nargs, nargs, #16, #8          /* ppc:3732 nargs = keyword count (raw)*/

db_match_keys_loop:                         /* ppc:3747-3759                       */
        cmp arg_reg, rnil                   /* ppc:3748 cmpri(cr0,arg_reg,nil)     */
        mov imm0, #0                        /* ppc:3749 li imm0,0                  */
        mov imm3, #misc_data_offset         /* ppc:3750 li imm3,misc_data_offset   */
        b.eq db_matched_keys                /* ppc:3751 beq cr0,matched_keys       */
        ldr arg_x, [arg_reg, #cons.car]     /* ppc:3752 ldr(arg_x,cons.car)        */
        ref_nrs_symbol arg_y, kallowotherkeys   /* ppc:3753 */
        cmp arg_x, arg_y                    /* ppc:3754 cmpr(cr3,arg_x,arg_y)      */
        cset temp4, eq                      /* [D1] stash cr3 (is-aok) into temp4  */
        ldr arg_reg, [arg_reg, #cons.cdr]   /* ppc:3755 ldr(arg_reg,cons.cdr)      */
        ldr arg_y, [arg_reg, #cons.car]     /* ppc:3756 ldr(arg_y,cons.car)        */
        ldr arg_reg, [arg_reg, #cons.cdr]   /* ppc:3758 ldr(arg_reg,cons.cdr)      */
        b db_match_test                     /* ppc:3759 b match_test               */
db_match_loop:                              /* ppc:3760-3766                       */
        ldr temp0, [keyvect_reg, imm3]      /* ppc:3761 ldrx(temp0,keyvect_reg,imm3)*/
        add imm3, imm3, #node_size          /* ppc:3765 addi imm3,node_size        */
        add imm0, imm0, #1                  /* ppc:3763 addi imm0,1 (raw 1-based)  */
        cmp arg_x, temp0                    /* ppc:3762 cmpr(cr0,arg_x,temp0)      */
        b.eq db_match_hit                   /* ppc:3766 beq cr0 (hit)              */
db_match_test:                              /* ppc:3778-3782                       */
        cmp imm0, nargs                     /* ppc:3764/3779 [D1] recompute cr4    */
        b.ne db_match_loop                  /* ppc:3779 bne cr4,match_loop         */
        cbnz temp4, db_match_keys_check_aok /* ppc:3780 beq cr3,check_aok          */
        add imm1, imm1, #node_size          /* ppc:3781 addi imm1,node_size (count)*/
        b db_match_keys_loop                /* ppc:3782 b match_keys_loop          */
db_match_hit:                               /* ppc:3767-3777                       */
        lsl imm0, imm0, #dnode_shift        /* ppc:3768 slwi imm0,dnode_shift      */
        sub imm0, imm2, imm0                /* ppc:3769 subf imm0,imm0,imm2       */
        ldr temp0, [imm0, #0]               /* ppc:3770 ldr(temp0,0(imm0))         */
        cmp temp0, rnil                     /* ppc:3771 cmpri(cr0,temp0,nil)       */
        b.ne db_match_keys_loop             /* ppc:3773 bne cr0,match_keys_loop    */
        add temp0, rnil, #t_offset          /* ppc:3772 li temp0,t_value           */
        str arg_y, [imm0, #node_size*1]     /* ppc:3774 str(arg_y,node_size*1(imm0))*/
        str temp0, [imm0, #node_size*0]     /* ppc:3775 str(temp0,node_size*0(imm0))*/
        cbz temp4, db_match_keys_loop       /* ppc:3776 bne cr3,match_keys_loop    */
db_match_keys_check_aok:                    /* ppc:3783-3790                       */
        tst imm1, #2                        /* ppc:3784 andi. imm0,imm1,2 (seen-aok?)*/
        orr imm1, imm1, #2                  /* ppc:3786 ori imm1,imm1,2 (mark seen)*/
        b.ne db_match_keys_loop             /* ppc:3787 bne cr0,match_keys_loop    */
        cmp arg_y, rnil                     /* ppc:3785 cmpri(cr1,arg_y,nil)       */
        b.eq db_match_keys_loop             /* ppc:3788 beq cr1,match_keys_loop    */
        orr imm1, imm1, #1                  /* ppc:3789 ori imm1,imm1,1 (allow)    */
        b db_match_keys_loop                /* ppc:3790 b match_keys_loop          */
db_matched_keys:                            /* ppc:3791-3795                       */
        bic imm0, imm1, #3                  /* ppc:3792 clrrwi. imm0,imm1,2        */
        cbz imm0, db_ret                    /* ppc:3793 beqlr (none unknown)       */
        tst imm1, #1                        /* ppc:3794 andi. imm1,imm1,1          */
        b.ne db_ret                         /* ppc:3795 bnelr (allowed)            */
        /* fall through to db_badkeys */
db_badkeys:                                 /* ppc:3798-3800                       */
        mov arg_y, #XBADKEYS               /* ppc:3799 li arg_y,XBADKEYS          */
        b db_destructure_error              /* ppc:3800 b destructure_error        */
db_toomany:                                 /* ppc:3801-3803                       */
        mov arg_y, #XCALLTOOMANY            /* ppc:3802 li arg_y,XCALLTOOMANY      */
        b db_destructure_error              /* ppc:3803 b destructure_error        */
db_toofew:                                  /* ppc:3804-3806                       */
        mov arg_y, #XCALLTOOFEW             /* ppc:3805 li arg_y,XCALLTOOFEW       */
        b db_destructure_error              /* ppc:3806 b destructure_error        */
db_badlist:                                 /* ppc:3807-3809                       */
        mov arg_y, #XCALLNOMATCH            /* ppc:3808 li arg_y,XCALLNOMATCH      */
db_destructure_error:                       /* ppc:3810-3814                       */
        mov vsp, imm4                       /* ppc:3811 mr vsp,imm4 (undo vstack) */
        mov arg_z, whole_reg                /* ppc:3812 mr arg_z,whole_reg         */
        set_nargs 2                         /* ppc:3813 set_nargs(2)               */
        b _SPksignalerr                     /* ppc:3814 b _SPksignalerr            */
db_ret:
        ret                                 /* ppc:3677/3680 blr                   */
endsp destructuring_bind_inner

.unreq whole_reg
.unreq arg_reg
.unreq keyvect_reg

/* ported from ppc-spentry.s:3819-3878 (PPC64 branch) */
/* Recover multiple values from stack */
spentry recover_values
        /* ppc:3819-3854.  vpush the saved value set atop the vstack, bumping
           nargs, then discard the tsp frame.  Two loops share NZCV, so each
           cmp is recomputed at its consuming branch. */
        /* First, walk the segments reversing the previous-segment pointers.
           The chain ends when the previous pointer equals the prev tsp. */
        ldr imm0, [tsp, #tsp_frame.backlink]        /* ppc:3823 previous tsp     */
        mov imm1, tsp                               /* ppc:3824 current segment  */
        mov imm2, tsp                               /* ppc:3825 last segment     */
1:      /* ppc:3826 walkloop */
        ldr imm3, [imm1, #(tsp_frame.fixed_overhead+node_size)] /* ppc:3827 next  */
        str imm2, [imm1, #(tsp_frame.fixed_overhead+node_size)] /* ppc:3829 rev   */
        mov imm2, imm1                              /* ppc:3830 last<-current     */
        mov imm1, imm3                              /* ppc:3831 current<-next     */
        cmp imm0, imm3                              /* ppc:3828 last segment?     */
        b.ne 1b                                     /* ppc:3832 bne cr0           */
        /* final segment ptr now in imm2 (ppc:3834); walk backwards pushing
           values onto the vstack and incrementing nargs. */
2:      /* ppc:3836 pushloop */
        ldr imm0, [imm2, #tsp_frame.data_offset]    /* ppc:3837 count in segment  */
        add imm3, imm2, #(tsp_frame.data_offset+(2*node_size)) /* ppc:3840 la     */
        add imm3, imm3, imm0                        /* ppc:3841                   */
        add nargs, nargs, imm0                      /* ppc:3842                   */
        cmp imm0, #0                                /* ppc:3838 cr0 entry test    */
        b 4f                                        /* ppc:3843                   */
3:      /* ppc:3844 inner push body */
        ldr arg_z, [imm3, #-node_size]!             /* ppc:3845 ldru              */
        cmp imm0, #fixnum_one                       /* ppc:3846 cr0 (pre-decr)    */
        sub imm0, imm0, #fixnum_one                 /* ppc:3847                   */
        vpush1 arg_z                                /* ppc:3848 vpush (flag-neut) */
4:      /* ppc:3849 inner entry/continue test */
        b.ne 3b                                     /* ppc:3850 bne cr0           */
        cmp imm2, tsp                               /* ppc:3839 cr1 (test old imm2)*/
        ldr imm2, [imm2, #(tsp_frame.data_offset+node_size)] /* ppc:3851 prev seg */
        b.ne 2b                                     /* ppc:3852 bne cr1 (ldr flag-neut)*/
        TSP_Unlink                                  /* ppc:3853 unlink(tsp)       */
        ret                                         /* ppc:3854 blr               */
endsp recover_values

/* ported from ppc-spentry.s:4987-5011 (PPC64 branch) */
/* Save multiple values to stack */
spentry save_values
        /* ppc:4987-5011.  nargs values atop the vstack -> a boxed tsp frame,
           popping them off the vstack.  save_values_to_tsp is the common exit
           shared with add_values (a REAL named file-local label, since a
           numeric local will not resolve across the separate add_values body). */
        mov imm1, tsp                               /* ppc:4988                   */
        /* common exit: nargs = values in this set, imm1 = tsp before the call. */
save_values_to_tsp:                                 /* ppc:4992 (named label)     */
        mov imm2, tsp                               /* ppc:4993 previous tsp      */
        dnode_align imm0, nargs, (tsp_frame.fixed_overhead + 2*node_size) /* ppc:4994 */
        TSP_Alloc_Var_Boxed_nz imm0, imm3     /* ppc:4995 */
        str imm1, [tsp, #tsp_frame.backlink]        /* ppc:4996 one tsp "frame"   */
        str nargs, [tsp, #tsp_frame.data_offset]    /* ppc:4997 value count       */
        str imm2, [tsp, #(tsp_frame.data_offset+node_size)] /* ppc:4998 prev tsp  */
        add imm3, tsp, #(tsp_frame.data_offset+node_size*2) /* ppc:4999 la        */
        add imm3, imm3, nargs                       /* ppc:5000                   */
        add imm0, vsp, nargs                        /* ppc:5001 top of value block*/
        cmp imm0, vsp                               /* ppc:5002 cr0 entry test    */
        b 2f                                        /* ppc:5003                   */
1:      /* ppc:5004 */
        ldr arg_z, [imm0, #-node_size]!             /* ppc:5005 ldru              */
        cmp imm0, vsp                               /* ppc:5006 cr0 (pre-decr)    */
        str arg_z, [imm3, #-node_size]!             /* ppc:5007 stru (flag-neut)  */
2:      /* ppc:5008 */
        b.ne 1b                                     /* ppc:5009 bne cr0           */
        add vsp, vsp, nargs                         /* ppc:5010 discard values    */
        ret                                         /* ppc:5011 blr               */
endsp save_values

/* ported from ppc-spentry.s:5022-5026 (PPC64 branch) */
/* Add saved values to current values */
spentry add_values
        /* ppc:5022-5026.  Add the nargs values atop the vstack to the set saved
           in the top tsp frame; a fresh linked tsp element is built by falling
           into save_values_to_tsp (imm1 = current top-tsp backlink). */
        ldr imm1, [tsp, #tsp_frame.backlink]        /* ppc:5024 ldr imm1,0(tsp)   */
        cmp nargs, #0                               /* ppc:5023 cr0               */
        b.ne save_values_to_tsp                     /* ppc:5025 bne cr0           */
        ret                                         /* ppc:5026 blr               */
endsp add_values

/* ported from ppc-spentry.s:7200-7230 (PPC64 branch) */
/* Make an unwind marker (for %unwind-protect) */
spentry nmkunwind
        /* ppc-spentry.s:7200-7230.  Unwind-protect frame that first disables
           interrupts.  Bind *INTERRUPT-LEVEL* to -1 (saving the old level in
           arg_y, which mkcatch preserves), build the unwind-protect catch frame
           (tag=unbound_marker, mvflag=fixnum 1, exactly as .SPmkunwind), then
           tail into .SPbind_interrupt_level with arg_z = old level to re-bind it.
           The interrupt-level binding mirrors .SPbind_interrupt_level_m1; the
           frame build mirrors .SPmkunwind. */
        mov imm2, #(-fixnumone)         /* ppc:7201 li imm2,-fixnumone          */
        mov imm3, #INTERRUPT_LEVEL_BINDING_INDEX            /* ppc:7202         */
        ldr imm4, [rcontext, #tcr.tlb_pointer]              /* ppc:7203         */
        ldr arg_y, [imm4, #INTERRUPT_LEVEL_BINDING_INDEX]   /* ppc:7204 old lvl */
        ldr imm1, [rcontext, #tcr.db_link]                  /* ppc:7205         */
        vpush1 arg_y                    /* ppc:7206 binding frame: old value    */
        vpush1 imm3                     /* ppc:7207               tlb index     */
        vpush1 imm1                     /* ppc:7208               prev db_link   */
        str imm2, [imm4, #INTERRUPT_LEVEL_BINDING_INDEX]    /* ppc:7209 il=-1   */
        str vsp, [rcontext, #tcr.db_link]                   /* ppc:7210         */
        mov arg_z, #unbound_marker      /* ppc:7211 lwi(arg_z,unbound_marker)   */
        mov imm2, #fixnum_one           /* ppc:7212 li imm2,fixnum_one          */
        mkcatch                         /* ppc:7213 mkcatch() (preserves arg_y) */
        mov arg_z, arg_y                /* ppc:7214 mr arg_z,arg_y (old level)  */
        b _SPbind_interrupt_level       /* ppc:7215 b _SPbind_interrupt_level   */
endsp nmkunwind

/* ported from ppc-spentry.s:5348-5353 (PPC64 branch) */
/* Like restorefullcontext, only the saved return address winds up in
   loc-pc (lr) instead of getting thrashed around.  I.e. a normal restore
   and return. */
spentry restorecontext
        /* ppc-spentry.s:5348-5353.  Restore fn, vsp, lr from lisp_frame on sp,
           discard the frame, and return normally (saved lr → lr → ret). */
        ldr lr, [sp, #lisp_frame.savelr]    /* ppc:5349 ldr(loc_pc,savelr); mtlr */
        ldr vsp, [sp, #lisp_frame.savevsp]  /* ppc:5350                          */
        ldr fn, [sp, #lisp_frame.savefn]    /* ppc:5351                          */
        discard_lisp_frame                  /* ppc:5352                          */
        ret                                 /* ppc:5353 blr                      */
endsp restorecontext

/* ported from ppc-spentry.s:5320-5328 (PPC64 branch) */
/* Restore full context: jump to the CALLER's return address while restoring
   the frame's saved lr (i.e. the frame's savelr goes into lr, but execution
   resumes at what lr was on entry — the PPC ctr trick). */
spentry restorefullcontext
        /* ppc-spentry.s:5320-5328.  PPC idiom: mflr->mtctr, restore lr from
           frame, bctr.  ARM64 equivalent: save lr in scratch, restore lr from
           frame (so the "saved" return address lives in lr for its eventual
           caller), then branch to the original lr via the scratch.  imm0 is
           free (no live inputs besides the frame on sp). */
        mov imm0, lr                        /* ppc:5321-5322 mflr+mtctr (save lr)*/
        ldr lr, [sp, #lisp_frame.savelr]    /* ppc:5323-5324 restore saved lr    */
        ldr vsp, [sp, #lisp_frame.savevsp]  /* ppc:5325                          */
        ldr fn, [sp, #lisp_frame.savefn]    /* ppc:5326                          */
        discard_lisp_frame                  /* ppc:5327                          */
        br imm0                             /* ppc:5328 bctr                     */
endsp restorefullcontext

/* ported from ppc-spentry.s:6743-6756 (PPC64 branch) */
/* Restore current thread's interrupt level to arg_z, noting whether the
   tcr's interrupt_pending flag was set.  If level is being restored to 0
   and an interrupt was pending, signal it now. */
spentry restoreintlevel
        /* ppc-spentry.s:6743-6756.  Two conditions checked:
           1) arg_z != 0 (not restoring to level-0) → just store
           2) tcr.interrupt_pending == 0 → just store
           Both false → clear pending flag, trap (deferred interrupt). */
        cmp arg_z, #0                       /* ppc:6744 cmpri(cr1,arg_z,0)       */
        ldr imm0, [rcontext, #tcr.interrupt_pending] /* ppc:6745                 */
        b.ne 1f                             /* ppc:6747 bne cr1,1f (not level-0) */
        cbz imm0, 1f                        /* ppc:6748 beq cr0,1f (nothing pending) */
        str xzr, [rcontext, #tcr.interrupt_pending] /* ppc:6749 str(rzero,...)   */
        mov nargs, #fixnum_one              /* ppc:6750 li nargs,fixnum_one      */
        uuo_interrupt_now               /* ppc:6751 trgti: deferred-interrupt (uuo_misc 4 at the pin) */
        /* intended: trap if nargs>0, i.e. always — signals deferred interrupt   */
        ret                                 /* ppc:6752 blr (unreachable past trap) */
1:
        ldr nargs, [rcontext, #tcr.tlb_pointer]  /* ppc:6754                     */
        str arg_z, [nargs, #INTERRUPT_LEVEL_BINDING_INDEX] /* ppc:6755           */
        ret                                 /* ppc:6756 blr                      */
endsp restoreintlevel

/* ported from ppc-spentry.s:6726-6739 (PPC64 branch) */
/* arg_y = special symbol, arg_z = new value.  Set the thread-local binding
   if one exists; otherwise fall through to gvset on the symbol's vcell. */
spentry specset
        /* ppc-spentry.s:6726-6739.  Sibling of specref/specrefcheck (getters):
           same tlb_pointer/tlb_limit access pattern, but STORES instead of
           loading; on no_thread_local_binding_marker hit, stores to
           symbol.vcell via _SPgvset. */
        ldr imm3, [arg_y, #symbol.binding_index]   /* ppc:6727                  */
        ldr imm0, [rcontext, #tcr.tlb_limit]       /* ppc:6728                  */
        ldr imm2, [rcontext, #tcr.tlb_pointer]     /* ppc:6729                  */
        cmp imm3, imm0                      /* ppc:6730 cmpr(imm3,imm0)          */
        b.hs 1f                             /* ppc:6731 bge 1f (index>=limit)    */
        ldr temp1, [imm2, imm3]             /* ppc:6732 ldrx(temp1,imm2,imm3)   */
        cmp temp1, #no_thread_local_binding_marker /* ppc:6733                   */
        b.eq 1f                             /* ppc:6734 beq 1f (no local binding)*/
        str arg_z, [imm2, imm3]             /* ppc:6735 strx(arg_z,imm2,imm3)   */
        ret                                 /* ppc:6736 blr                      */
1:      /* No thread-local binding: store to symbol.vcell via _SPgvset.
           ppc:6737-6739: arg_x=symbol, arg_y=byte-offset, tail to _SPgvset. */
        mov arg_x, arg_y                    /* ppc:6737 mr arg_x,arg_y           */
        mov arg_y, #(symbol.vcell - misc_data_offset) /* ppc:6738                */
        b _SPgvset                          /* ppc:6739 b _SPgvset               */
endsp specset

/* ported from ppc-spentry.s:3246-3268 (PPC64 branch) */
/* Make a stack-consed value cell.  imm0 points to the closed-over value
   (already vpushed as a locative offset from vsp).  Replace that locative
   with the newly-minted vcell.  Sibling of stkvcellvsp (which is the same
   but assumes imm0 == vsp on entry). */
spentry stkvcell0
        /* ppc-spentry.s:3246-3268.  Like stkvcellvsp but imm0 is a locative
           pointing INTO the vstack (not necessarily vsp): compute delta first,
           then push 3 NILs, recompute, and overlay exactly as stkvcellvsp. */
        sub imm1, imm0, vsp             /* ppc:3247 sub imm1,imm0,vsp (delta)   */
        mov arg_z, rnil                 /* ppc:3248 li arg_z,nil_value           */
        vpush1 arg_z                    /* ppc:3249 vpush(arg_z)                 */
        vpush1 arg_z                    /* ppc:3250 vpush(arg_z)                 */
        vpush1 arg_z                    /* ppc:3251 vpush(arg_z)                 */
        add imm1, imm1, #(node_size*3)  /* ppc:3252 addi imm1,imm1,node_size*3  */
        add imm0, vsp, imm1             /* ppc:3253 add imm0,vsp,imm1 (recompute)*/
        tst vsp, #(1<<3)                /* ppc:3254 andi. imm1,vsp,1<<word_shift */
        mov imm1, #value_cell_header    /* ppc:3255 li imm1,value_cell_header    */
        ldr arg_z, [imm0]              /* ppc:3256 ldr(arg_z,0(imm0))           */
        b.eq 1f                         /* ppc:3257 beq cr0 -> even-vsp layout   */
        str arg_z, [vsp, #(node_size*2)]/* ppc:3258                              */
        str imm1, [vsp, #node_size]     /* ppc:3259                              */
        add arg_z, vsp, #(fulltag_misc+node_size) /* ppc:3260 la                 */
        str arg_z, [imm0]              /* ppc:3261                              */
        ret                             /* ppc:3262 blr                          */
1:      /* ppc:3263 even-vsp layout */
        str arg_z, [vsp, #node_size]    /* ppc:3264                              */
        str imm1, [vsp]                /* ppc:3265                              */
        add arg_z, vsp, #fulltag_misc   /* ppc:3266 la                           */
        str arg_z, [imm0]              /* ppc:3267                              */
        ret                             /* ppc:3268 blr                          */
endsp stkvcell0

/* NOTES
 *
 * All 41 subprims in this cluster carry real ported bodies (0 stubs).
 * Every site that depends on a constant/convention Matt has not yet defined
 * is guarded with #error + the intended instruction in a comment; the deduped
 * union lives in upstream-port/MISSING-CONSTANTS-RATIFY.md.  Open #error
 * classes in this file:
 *   - ret1val_addr ref-global idiom (values, ppc:1217)
 *   - error_too_many_values uuo_interr trap convention (values, ppc:1235)
 *   - nrs.kallowotherkeys rnil-relative ref (keyword_bind ppc:1507,
 *     destructuring_bind_inner ppc:3753)
 *   - deferred-interrupt trap trgti (restoreintlevel ppc:6751)
 * PROPOSED (ratify): mask_initopt/keyp/aok/restp adopt the ARM32 bit layout
 * (arm-constants.s:561-567), NOT the PPC big-endian bit indices; must match
 * the compiler's doadlword emission when Matt defines it.
 * Struct layouts (catch_frame with nsaveregs=4, lisp_frame, tsp_frame,
 * binding, symbol) are PROPOSED in this file's header blocks; the C runtime
 * and GC must agree.
 */

/*
 * ARM64 subprim implementations: call/arglist/builtin operations (59 subprims)
 * Ported from PPC64 to ARM64 for upstream low-tag design
 */

/* ========== BUILTIN FUNCTION VECTOR INDICES ========== */
/* From vendor/ccl/lisp-kernel/ppc-constants.s:128-151.
 * These are lisp-level vector indices into %builtin-functions% --
 * arch-independent, safe to define here. */
.set _builtin_plus,     0
.set _builtin_minus,    1
.set _builtin_times,    2
.set _builtin_div,      3
.set _builtin_eq,       4
.set _builtin_ne,       5
.set _builtin_gt,       6
.set _builtin_ge,       7
.set _builtin_lt,       8
.set _builtin_le,       9
.set _builtin_eql,      10
.set _builtin_length,   11
.set _builtin_seqtype,  12
.set _builtin_assq,     13
.set _builtin_memq,     14
.set _builtin_logbitp,  15
.set _builtin_logior,   16
.set _builtin_logand,   17
.set _builtin_ash,      18
.set _builtin_negate,   19
.set _builtin_logxor,   20
.set _builtin_aref1,    21
.set _builtin_aset1,    22

/* ========== LOCAL HELPER MACROS ========== */

/* Lisp error selectors: errors.s deferr(NAME,N) = boxed fixnum N. */
.set XSTKOVER,  (75<<fixnumshift)       /* errors.s:196  */
.set XNOSPREAD, (120<<fixnumshift)      /* errors.s:202  */

/* Kernel (uuo) error codes - raw, NOT boxed.  errors.s top block. */
.set error_object_not_list, 133         /* errors.s:38-48 def_type_error
                                           counter: 128=array +5 => list */

/* GC write-barrier shift constants (same derivations as spentry-B). */
.set dnode_shift, 4
.set bitmap_shift, 6

/* UUO / trap encodings.  CANONICAL: arm64-asm.lisp:435-450 (Matt's active
 * layer) = `udf #imm16`, low 3 bits = format.  fmt 3 = unary-misc is a
 * PROPOSED extension (reg in 7:3, sub in 15:8: 0 not_callable,
 * 1 no_throw_tag, 2 tlb_too_small, 3 unbound, >= 4 = errors.s errnum —
 * not_list below; full namespace doc: spentry-A's trap block).
 * arm64-exceptions.c must decode.  gpr numbers for the registers here. */
/* trap_unless_list's trap (ppc-macros.s): object in \gpr is not a list. */

/* jump_builtin: dispatch to Lisp builtin handler via %builtin-functions%
 * vector.  Macro equivalent of PPC64 jump_builtin (ppc-spentry.s:37-42);
 * nrs/globals idiom per arm64-globals-proposed.s. */
.macro jump_builtin idx, nargs_count
        ref_nrs_value fname, builtin_functions
        set_nargs \nargs_count
        ldr fname, [fname, #(misc_data_offset + (\idx) * node_size)]
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.code_vector]
        br temp0
.endm

/* ========== BASIC CALL/JUMP OPERATIONS ========== */

/* ported from ppc-spentry.s:44-45 (PPC64 branch: jump_fname macro) */
spentry jmpsym
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.code_vector]
        br temp0
endsp jmpsym

/* ported from ppc-spentry.s:47-48 (PPC64 branch: jump_nfn macro) */
spentry jmpnfn
        ldr temp0, [nfn, #_function.code_vector]
        br temp0
endsp jmpnfn

/* ported from ppc-spentry.s:51-52 (PPC64 branch: do_funcall macro,
 * ppc-macros.s).  Call temp0 if it is a symbol or a function, else trap.
 * PPC dispatches on the TYPECODE (subtag_symbol / subtag_function);
 * here symbols keep a dedicated pointer fulltag, while a function is an
 * ordinary miscobj (fulltag_function removed, patch 0055), so: symbol
 * fulltag -> fcell, misc fulltag + header subtag_function -> call, else
 * trap.  The SYMBOL path jumps through the fcell object's slot 0
 * UNCHECKED, exactly like PPC64: a real function's slot 0 is its
 * codevector, and the macro/special-op/udf fcell simple-vectors carry
 * %macro-code%/%udf-code% at slot 0, which signal. */
spentry funcall
        and imm0, temp0, #fulltagmask
        cmp imm0, #fulltag_symbol
        b.eq 2f
        cmp imm0, #fulltag_misc
        b.ne 3f
        ldrb w1, [temp0, #misc_subtag_offset]  /* imm1 = header subtag (ldurb form) */
        cmp imm1, #subtag_function
        b.ne 3f
        mov nfn, temp0
        ldr temp0, [nfn, #_function.code_vector]
        br temp0
2:      /* symbol: call its function cell (unchecked slot-0 jump) */
        mov fname, temp0
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.code_vector]
        br temp0
3:      /* ppc-macros.s do_funcall: uuo_interr(error_cant_call, temp0) */
        uuo_error_reg_not_callable temp0 /* his macro name */
endsp funcall

/* ========== CONS MUTATION (with EGC write barrier) ========== */

/* ported from ppc-spentry.s:482-520 (PPC64 branch).
 * The store is real; the EGC write-barrier memoization (ppc:487-519:
 * dnode math on (arg_y - ref_base), set bit in refbits + ephemeral_refidx
 * with ldxr/stxr) needs the ref_base/oldspace_dnode_count/refbits/
 * ephemeral_refidx GLOBALS, which have no ARM64 anchor yet - the same
 * open idiom as the spentry-B barrier sites.  #error so a build cannot
 * silently drop the memoization (young-object refs would be lost by the
 * EGC). */
/* pc_luser_xp window labels (ppc-spentry.s:480-486 places egc_rplaca at
 * the subprim entry and did_store right after the str; NOTE: PPC's single
 * contiguous [write_barrier_start, write_barrier_end) window does NOT
 * exist on ARM64 — the barrier family is split between this file and
 * spentry-B, so arm64-exceptions.c's pc_luser_xp checks per-family
 * windows instead). */
        .globl C(egc_rplaca)
        .globl C(egc_rplaca_did_store)
spentry rplaca
C(egc_rplaca):
        cmp arg_z, arg_y                /* ppc:484 cmplr(cr2,arg_z,arg_y)  */
        str arg_z, [arg_y, #cons.car]   /* ppc:485 _rplaca                 */
C(egc_rplaca_did_store):
        b.ls 1f                         /* ppc:487 blelr cr2 (no barrier)  */
        ref_global imm2, ref_base               /* ppc:488 ref_global      */
        mov imm3, #0x8000000000000000           /* ppc:489 load_highbit    */
        ref_global imm1, oldspace_dnode_count   /* ppc:493                 */
        sub imm0, arg_y, imm2                   /* ppc:490                 */
        lsr imm0, imm0, #dnode_shift            /* ppc:491                 */
        cmp imm0, imm1                          /* ppc:495 cmplr           */
        lsr imm2, imm0, #8                      /* ppc:492 refidx granule  */
        and imm4, imm0, #0x3f                   /* ppc:494 bit shift count */
        lsr imm0, imm0, #bitmap_shift           /* ppc:497                 */
        lsr imm3, imm3, imm4                    /* ppc:496                 */
        ref_global temp0, refbits               /* ppc:498                 */
        b.hs 1f                                 /* ppc:499 bgelr (UNSIGNED)*/
        lsl imm0, imm0, #3                      /* ppc:500 word_shift      */
        ldr imm1, [temp0, imm0]                 /* ppc:501                 */
        tst imm1, imm3                          /* ppc:502 and.            */
        b.ne 1f                                 /* ppc:503 bnelr           */
        add temp0, temp0, imm0                  /* ldxr/stxr take [Xn]     */
2:      ldxr imm1, [temp0]                      /* ppc:504 lrarx           */
        orr imm1, imm1, imm3                    /* ppc:505                 */
        stxr w17, imm1, [temp0]                  /* ppc:506 strcx           */
        cbnz w17, 2b                             /* ppc:507                 */
        dmb ish                                 /* ppc:508 isync           */
        and imm4, imm2, #0x3f                   /* ppc:509                 */
        lsr imm2, imm2, #bitmap_shift           /* ppc:510                 */
        mov imm3, #0x8000000000000000           /* ppc:511                 */
        ref_global temp0, ephemeral_refidx      /* ppc:512                 */
        lsl imm2, imm2, #3                      /* ppc:513                 */
        lsr imm3, imm3, imm4                    /* ppc:514                 */
        add temp0, temp0, imm2                  /* ldxr/stxr take [Xn]     */
3:      ldxr imm1, [temp0]                      /* ppc:515 lrarx           */
        orr imm1, imm1, imm3                    /* ppc:516                 */
        stxr w17, imm1, [temp0]                  /* ppc:517 strcx           */
        cbnz w17, 3b                             /* ppc:518                 */
        dmb ish                                 /* ppc:519 isync           */
1:      ret
endsp rplaca

/* ported from ppc-spentry.s:524-562 (PPC64 branch); see rplaca above. */
/* pc_luser_xp window labels (ppc-spentry.s:522-528). */
        .globl C(egc_rplacd)
        .globl C(egc_rplacd_did_store)
spentry rplacd
C(egc_rplacd):
        cmp arg_z, arg_y
        str arg_z, [arg_y, #cons.cdr]
C(egc_rplacd_did_store):
        b.ls 1f
        ref_global imm2, ref_base               /* ppc:528 ref_global      */
        mov imm3, #0x8000000000000000           /* ppc:529 load_highbit    */
        ref_global imm1, oldspace_dnode_count   /* ppc:533                 */
        sub imm0, arg_y, imm2                   /* ppc:530                 */
        lsr imm0, imm0, #dnode_shift            /* ppc:531                 */
        cmp imm0, imm1                          /* ppc:535 cmplr           */
        lsr imm2, imm0, #8                      /* ppc:532 refidx granule  */
        and imm4, imm0, #0x3f                   /* ppc:534 bit shift count */
        lsr imm0, imm0, #bitmap_shift           /* ppc:537                 */
        lsr imm3, imm3, imm4                    /* ppc:536                 */
        ref_global temp0, refbits               /* ppc:538                 */
        b.hs 1f                                 /* ppc:539 bgelr (UNSIGNED)*/
        lsl imm0, imm0, #3                      /* ppc:540 word_shift      */
        ldr imm1, [temp0, imm0]                 /* ppc:541                 */
        tst imm1, imm3                          /* ppc:542 and.            */
        b.ne 1f                                 /* ppc:543 bnelr           */
        add temp0, temp0, imm0                  /* ldxr/stxr take [Xn]     */
2:      ldxr imm1, [temp0]                      /* ppc:544 lrarx           */
        orr imm1, imm1, imm3                    /* ppc:545                 */
        stxr w17, imm1, [temp0]                  /* ppc:546 strcx           */
        cbnz w17, 2b                             /* ppc:547                 */
        dmb ish                                 /* ppc:548 isync           */
        and imm4, imm2, #0x3f                   /* ppc:549                 */
        lsr imm2, imm2, #bitmap_shift           /* ppc:550                 */
        mov imm3, #0x8000000000000000           /* ppc:551                 */
        ref_global temp0, ephemeral_refidx      /* ppc:552                 */
        lsl imm2, imm2, #3                      /* ppc:553                 */
        lsr imm3, imm3, imm4                    /* ppc:554                 */
        add temp0, temp0, imm2                  /* ldxr/stxr take [Xn]     */
3:      ldxr imm1, [temp0]                      /* ppc:555 lrarx           */
        orr imm1, imm1, imm3                    /* ppc:556                 */
        stxr w17, imm1, [temp0]                  /* ppc:557 strcx           */
        cbnz w17, 3b                             /* ppc:558                 */
        dmb ish                                 /* ppc:559 isync           */
1:      ret
/* end of the rplaca/rplacd pc_luser_xp window (this file's half of the
 * split barrier family; spentry-B holds the other four families). */
        .globl C(egc_rplacd_end)
C(egc_rplacd_end):
endsp rplacd

/* ========== MULTIPLE VALUES ========== */

/* ported from ppc-spentry.s:1153-1165 (PPC64 branch).
 * Funcall temp0, returning multiple values if it does. */
spentry mvpass
        cmp nargs, #(nargregs<<fixnumshift)     /* ppc:1154                */
        mov imm0, vsp                           /* ppc:1156                */
        b.le 1f                                 /* ppc:1157                */
        sub imm0, imm0, #(nargregs<<fixnumshift) /* ppc:1158               */
        add imm0, imm0, nargs                   /* ppc:1159                */
1:
        /* ppc:1161 build_lisp_frame(fn,loc_pc,imm0) - MARKER frame */
        sub sp, sp, #lisp_frame.size
        mov temp1, #lisp_frame_marker
        str temp1, [sp, #lisp_frame.marker]
        str imm0,  [sp, #lisp_frame.savevsp]
        str fn,    [sp, #lisp_frame.savefn]
        str lr,    [sp, #lisp_frame.savelr]
        /* ppc:1162 ref_global(loc_pc,ret1val_addr); ppc:1164 mtlr */
        ref_global lr, ret1val_addr             /* ppc:1162+1164           */
        mov fn, xzr                             /* ppc:1163 li fn,0        */
        b _SPfuncall                            /* ppc:1165 do_funcall     */
endsp mvpass

/* ported from ppc-spentry.s:1181-1193 (PPC64 branch) */
spentry fitvals
        /* Adjust value count: imm0 = desired count, nargs = actual count */
        subs imm0, imm0, nargs
        mov imm1, rnil
        b.ge 2f
        /* Too many values - discard extras */
        sub vsp, vsp, imm0
        ret
1:      /* Push nils */
        subs imm0, imm0, #node_size
        str imm1, [vsp, #-node_size]!
        add nargs, nargs, #node_size
2:      b.ne 1b
        ret
endsp fitvals

/* ported from ppc-spentry.s:1196-1207 (PPC64 branch) */
spentry nthvalue
        /* Get nth value: top of vstack is index (tagged fixnum) */
        add imm0, vsp, nargs            /* ppc:1197                        */
        ldr imm1, [imm0]                /* ppc:1198                        */
        cmp imm1, nargs                 /* ppc:1199 cmplr = UNSIGNED, so a
                                           negative index wraps high => nil */
        mov arg_z, rnil                 /* ppc:1200                        */
        neg imm1, imm1                  /* ppc:1201                        */
        sub imm1, imm1, #node_size      /* ppc:1202                        */
        b.hs 1f                         /* ppc:1203 bge on the UNSIGNED cmp */
        ldr arg_z, [imm0, imm1]         /* ppc:1204 ldrx                   */
1:      add vsp, imm0, #node_size       /* ppc:1206                        */
        ret
endsp nthvalue

/* ========== OPTIONAL/REST/KEYWORD ARGUMENTS ========== */

/* ported from ppc-spentry.s:1282-1293 (PPC64 branch).
 * Provide nil defaults for missing &optional args; imm0 = (fixnum) upper
 * limit on required + &optional count.  nargs preserved.
 * ARM64-DEVIATION: PPC parks nil in imm5, but Matt's imm5 ALIASES nargs
 * (arm64-constants.h:45-46, the ledger's imm5/x5-vs-x6 item) - use temp0.
 * The nargs-vs-imm0 compare is redone AFTER the vpush block (whose cmp
 * clobbers NZCV) and is UNSIGNED (ppc:1283 cmplr). */
spentry default_optional_args
        mov temp0, rnil                 /* ppc:1284 li imm5,nil_value      */
        /* ppc:1285 vpush_argregs */
        cbz nargs, 2f
        cmp nargs, #(2<<fixnumshift)
        b.lt 3f
        b.eq 4f
        str arg_x, [vsp, #-node_size]!
4:      str arg_y, [vsp, #-node_size]!
3:      str arg_z, [vsp, #-node_size]!
2:
        mov imm1, nargs                 /* ppc:1286                        */
        cmp nargs, imm0                 /* ppc:1283 cmplr(cr7,nargs,imm0)  */
        b.hs 1f                         /* ppc:1287 bgelr cr7 (unsigned)   */
5:      add imm1, imm1, #fixnumone      /* ppc:1289                        */
        cmp imm1, imm0                  /* ppc:1290                        */
        str temp0, [vsp, #-node_size]!  /* ppc:1291 vpush(nil)             */
        b.ne 5b                         /* ppc:1292                        */
1:      ret
endsp default_optional_args

/* ported from ppc-spentry.s:1299-1315 (PPC64 branch).
 * Push T/NIL supplied-p flags for each of the imm0 &optional args;
 * supplied iff (< i nargs), computed branchlessly exactly as PPC64
 * (xor/sradi/or/sub/srdi = sign-bit trick; both operands are nonnegative
 * fixnums, then flag * t_offset + nil). */
spentry opt_supplied_p
        mov imm1, xzr                   /* ppc:1300                        */
1:      eor imm2, imm1, nargs           /* ppc:1304                        */
        asr imm2, imm2, #63             /* ppc:1305 sradi                  */
        orr imm2, imm2, imm1            /* ppc:1306                        */
        add imm1, imm1, #fixnumone      /* ppc:1307 addi fixnumone         */
        cmp imm1, imm0                  /* ppc:1308                        */
        sub imm2, imm2, nargs           /* ppc:1309 subf                   */
        lsr imm2, imm2, #63             /* ppc:1310 srdi -> 1 iff supplied */
        mov temp0, #t_offset            /* ppc:1311 mulli imm2,t_offset    */
        mul imm2, imm2, temp0
        add imm2, imm2, rnil            /* ppc:1312 addi imm2,nil_value    */
        str imm2, [vsp, #-node_size]!   /* ppc:1313 vpush                  */
        b.ne 1b                         /* ppc:1314                        */
        ret
endsp opt_supplied_p

/* ported from ppc-spentry.s:1336-1352 (PPC64 branch).
 * If nargs <= imm0(=0 here), vpush nil; else cons a list of the excess
 * args and vpush it.
 * ARM64-DEVIATION (all three rest-arg loops): PPC compares BEFORE Cons
 * (PPC's Cons preserves CR); Matt's Cons macro does `cmp allocptr,
 * allocbase` (arm64-macros.s:36-45) and CLOBBERS NZCV, so the loop test
 * is a fresh `cmp imm1, #0` AFTER the decrement. */
spentry heap_rest_arg
        mov imm0, xzr                   /* ppc:1337                        */
        /* ppc:1338 vpush_argregs */
        cbz nargs, 2f
        cmp nargs, #(2<<fixnumshift)
        b.lt 3f
        b.eq 4f
        str arg_x, [vsp, #-node_size]!
4:      str arg_y, [vsp, #-node_size]!
3:      str arg_z, [vsp, #-node_size]!
2:
        sub imm1, nargs, imm0           /* ppc:1339                        */
        mov arg_z, rnil                 /* ppc:1341                        */
        b 6f
5:      ldr temp0, [vsp]                /* ppc:1344                        */
        add vsp, vsp, #node_size        /* ppc:1346                        */
        Cons arg_z, temp0, arg_z        /* ppc:1347                        */
        sub imm1, imm1, #fixnumone      /* ppc:1348                        */
6:      cmp imm1, #0                    /* ppc:1340/1345 (post-Cons here)  */
        b.gt 5b
        str arg_z, [vsp, #-node_size]!  /* ppc:1350 vpush                  */
        ret
endsp heap_rest_arg

/* ported from ppc-spentry.s:1358-1373 (PPC64 branch).
 * Like heap_rest_arg, but imm0 = (fixnum) count of required args to
 * leave on the vstack.  Flags note as heap_rest_arg. */
spentry req_heap_rest_arg
        /* vpush_argregs */
        cbz nargs, 2f
        cmp nargs, #(2<<fixnumshift)
        b.lt 3f
        b.eq 4f
        str arg_x, [vsp, #-node_size]!
4:      str arg_y, [vsp, #-node_size]!
3:      str arg_z, [vsp, #-node_size]!
2:
        sub imm1, nargs, imm0
        mov arg_z, rnil
        b 6f
5:      ldr temp0, [vsp]
        add vsp, vsp, #node_size
        Cons arg_z, temp0, arg_z
        sub imm1, imm1, #fixnumone
6:      cmp imm1, #0
        b.gt 5b
        str arg_z, [vsp, #-node_size]!
        ret
endsp req_heap_rest_arg

/* ported from ppc-spentry.s:1376-1390 (PPC64 branch).
 * As above, argregs already vpushed by caller.  Flags note as
 * heap_rest_arg. */
spentry heap_cons_rest_arg
        sub imm1, nargs, imm0
        mov arg_z, rnil
        b 2f
1:      ldr temp0, [vsp]
        add vsp, vsp, #node_size
        Cons arg_z, temp0, arg_z
        sub imm1, imm1, #fixnumone
2:      cmp imm1, #0
        b.gt 1b
        str arg_z, [vsp, #-node_size]!
        ret
endsp heap_cons_rest_arg

/* ported from ppc-spentry.s:1393-1396 (PPC64 branch) */
spentry simple_keywords
        mov imm0, xzr
        /* vpush argregs */
        cbz nargs, 2f
        cmp nargs, #(node_size * 2)
        b.lt 3f
        b.eq 4f
        str arg_x, [vsp, #-node_size]!
4:      str arg_y, [vsp, #-node_size]!
3:      str arg_z, [vsp, #-node_size]!
2:      b _SPkeyword_bind
endsp simple_keywords

/* ported from ppc-spentry.s:1398-1400 (PPC64 branch) */
spentry keyword_args
        /* vpush argregs */
        cbz nargs, 2f
        cmp nargs, #(node_size * 2)
        b.lt 3f
        b.eq 4f
        str arg_x, [vsp, #-node_size]!
4:      str arg_y, [vsp, #-node_size]!
3:      str arg_z, [vsp, #-node_size]!
2:      b _SPkeyword_bind
endsp keyword_args

/* ported from ppc-spentry.s:2020-2022 (PPC64 branch):
 * li fname,nrs.errdisp; jump_fname. */
spentry ksignalerr
        ref_nrs_symbol fname, errdisp   /* ppc:2021 li fname,nrs.errdisp   */
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.code_vector]
        br temp0
endsp ksignalerr

/* ========== CLOSURE CALLS ========== */

/* ported from ppc-spentry.s:2076-2166 (PPC64 branch).
 * Prepend all but the first two (closure code, fn) and last two (name,
 * lfbits) elements of nfn (the closure vector) to the arglist, then call
 * the function in slot 1.  PPC keeps two condition registers live (cr0 =
 * nargs vs nargregs, cr1 = nargs vs 1); flattened here with re-compares
 * placed so no intervening instruction sets NZCV (ldr/str/add/sub/mov are
 * all non-flag-setting).  Labels are .L-local to avoid numeric-label
 * leaks across this long body. */
spentry call_closure
        /* The closure arrives misc-tagged (fulltag_function removed,
         * patch 0055), and this body addresses it misc-relative (PPC
         * shape) - no retag needed, exactly as on PPC. */
        /* ppc:2079-2080 vector_length(imm0,nfn,imm0) - 4 slots overhead */
        ldr imm0, [nfn, #misc_header_offset]
        lsr imm0, imm0, #num_subtag_bits
        lsl imm0, imm0, #fixnumshift
        sub imm0, imm0, #(4<<fixnumshift)   /* imm0 = inherited arg count  */
        mov imm1, #(misc_data_offset + (2<<fixnumshift)) /* ppc:2081 1st arg */
        mov imm4, rnil                      /* ppc:2082                    */
        cmp nargs, #(nargregs<<fixnumshift) /* ppc:2077 cmpri cr0          */
        b.le .Lcc_no_insert                 /* ppc:2083 ble cr0            */
        /* Args already vpushed: vpush imm0 NILs, slide the vpushed args
           down, insert the inherited args (ppc:2084-2115). */
        mov imm2, #0                        /* ppc:2088                    */
.Lcc_push_nil_loop:
        add imm2, imm2, #fixnumone          /* ppc:2090                    */
        cmp imm2, imm0                      /* ppc:2091 cmpr cr2           */
        str imm4, [vsp, #-node_size]!       /* ppc:2092 vpush              */
        b.ne .Lcc_push_nil_loop             /* ppc:2093                    */
        mov imm3, vsp                       /* ppc:2095                    */
        add imm4, vsp, imm0                 /* ppc:2096                    */
        sub imm2, nargs, #(nargregs<<fixnumshift) /* ppc:2097              */
.Lcc_copy_already_loop:
        cmp imm2, #fixnumone                /* ppc:2099 cmpri cr2          */
        sub imm2, imm2, #fixnumone          /* ppc:2100                    */
        ldr fname, [imm4]                   /* ppc:2101                    */
        add imm4, imm4, #fixnumone          /* ppc:2102                    */
        str fname, [imm3]                   /* ppc:2103                    */
        add imm3, imm3, #fixnumone          /* ppc:2104                    */
        b.ne .Lcc_copy_already_loop         /* ppc:2105                    */
.Lcc_insert_loop:
        cmp imm0, #fixnumone                /* ppc:2108 cmpri cr2          */
        ldr fname, [nfn, imm1]              /* ppc:2109 ldrx               */
        add imm1, imm1, #fixnumone          /* ppc:2110                    */
        add nargs, nargs, #fixnumone        /* ppc:2111                    */
        sub imm0, imm0, #fixnumone          /* ppc:2112                    */
        str fname, [imm4, #-node_size]!     /* ppc:2113 push(fname,imm4)   */
        b.ne .Lcc_insert_loop               /* ppc:2114                    */
        b .Lcc_go                           /* ppc:2115                    */
.Lcc_no_insert:
        /* nargregs or fewer args vpushed (ppc:2116-2120); NZCV still holds
           the nargs-vs-nargregs compare. */
        add imm2, imm1, imm0                /* ppc:2119                    */
        b.ne .Lcc_set_regs                  /* ppc:2120 bne cr0            */
.Lcc_vpush_remaining:                       /* exactly nargregs args       */
        cmp imm0, #fixnumone                /* ppc:2122 cmpri cr2          */
        ldr fname, [nfn, imm1]              /* ppc:2123                    */
        add imm1, imm1, #fixnumone          /* ppc:2124                    */
        str fname, [vsp, #-node_size]!      /* ppc:2125 vpush              */
        sub imm0, imm0, #fixnumone          /* ppc:2126                    */
        add nargs, nargs, #fixnumone        /* ppc:2127                    */
        b.ne .Lcc_vpush_remaining           /* ppc:2128                    */
        b .Lcc_go                           /* ppc:2129                    */
.Lcc_set_regs:
        /* nargs < nargregs: fill arg regs from the inherited args'
           HIGH end (imm2), possibly spilling the rest (ppc:2130-2160). */
        cmp nargs, #fixnumone               /* ppc:2078 cmpri cr1          */
        b.le .Lcc_set_y_z                   /* ppc:2133 ble cr1            */
.Lcc_set_arg_x:                             /* nargs was 2                 */
        sub imm0, imm0, #fixnumone          /* ppc:2135                    */
        sub imm2, imm2, #fixnumone          /* ppc:2137                    */
        ldr arg_x, [nfn, imm2]              /* ppc:2138 ldrx               */
        add nargs, nargs, #fixnumone        /* ppc:2139                    */
        cmp imm0, #0                        /* ppc:2136 cmpri cr0          */
        b.ne .Lcc_vpush_remaining           /* ppc:2140                    */
        b .Lcc_go                           /* ppc:2141                    */
.Lcc_set_y_z:                               /* NZCV: nargs vs fixnumone    */
        b.ne .Lcc_set_arg_z                 /* ppc:2144 bne cr1 (nargs=0)  */
.Lcc_set_arg_y:                             /* nargs was 1                 */
        sub imm0, imm0, #fixnumone          /* ppc:2147                    */
        sub imm2, imm2, #fixnumone          /* ppc:2149                    */
        ldr arg_y, [nfn, imm2]              /* ppc:2150                    */
        add nargs, nargs, #fixnumone        /* ppc:2151                    */
        cmp imm0, #0                        /* ppc:2148                    */
        b.ne .Lcc_set_arg_x                 /* ppc:2152                    */
        b .Lcc_go                           /* ppc:2153                    */
.Lcc_set_arg_z:                             /* nargs was 0                 */
        sub imm0, imm0, #fixnumone          /* ppc:2155                    */
        sub imm2, imm2, #fixnumone          /* ppc:2157                    */
        ldr arg_z, [nfn, imm2]              /* ppc:2158                    */
        add nargs, nargs, #fixnumone        /* ppc:2159                    */
        cmp imm0, #0                        /* ppc:2156                    */
        b.ne .Lcc_set_arg_y                 /* ppc:2160                    */
.Lcc_go:
        ldr nfn, [nfn, #(misc_data_offset + node_size)] /* ppc:2163 slot 1 */
        ldr temp0, [nfn, #_function.code_vector]         /* ppc:2164        */
        br temp0                            /* ppc:2165-2166 mtctr+bctr    */
endsp call_closure

/* ========== INTEGER/NATURAL CONVERSION ========== */

/* ported from ppc-spentry.s:2173-2202: the PPC64 branch of getxlong is
 * EMPTY (the __ifdef(`PPC64') arm has no code - only the PPC32 arm has a
 * body), i.e. this subprim is unreferenced on 64-bit targets.  Ported as
 * a loud trap, exactly like the trap-only PPC64 entries in spentry-E
 * (ffcallX/callbackX). */
spentry getxlong
        brk #0
endsp getxlong

/* ========== ARGUMENT SPREADING ========== */

/* ported from ppc-spentry.s:2209-2252 (PPC64 branch).
 * Everything up to the last arg has been vpushed; nargs = boxed count of
 * things already pushed.  Spread the list in arg_z, then set arg_x/y/z +
 * nargs as for a normal call.  ppc2-invoke-fn assumes temp1 preserved.
 * PPC keeps cr0 (nil check) and cr1 (cons check) live; flattened with
 * the cons check at loop top and the nil check at loop bottom. */
spentry spreadargz
        and imm1, arg_z, #fulltagmask   /* ppc:2211 extract_fulltag        */
        mov imm0, xzr                   /* ppc:2218 li imm0,0              */
        mov arg_y, arg_z                /* ppc:2219 save for error case    */
        cmp arg_z, rnil                 /* ppc:2217 cmpri cr0              */
        b.eq 2f                         /* ppc:2220 beq cr0                */
1:      cmp imm1, #fulltag_cons         /* ppc:2212/2228 cmpri cr1         */
        b.ne 3f                         /* ppc:2222 bne cr1 -> error       */
        ldr arg_x, [arg_z, #cons.car]   /* ppc:2223 _car                   */
        ldr arg_z, [arg_z, #cons.cdr]   /* ppc:2224 _cdr                   */
        and imm1, arg_z, #fulltagmask   /* ppc:2227                        */
        str arg_x, [vsp, #-node_size]!  /* ppc:2233 vpush                  */
        add imm0, imm0, #fixnumone      /* ppc:2234                        */
        cmp arg_z, rnil                 /* ppc:2225 cmpri cr0              */
        b.ne 1b                         /* ppc:2235                        */
2:      adds nargs, nargs, imm0         /* ppc:2237 add. (sets Z)          */
        b.eq 9f                         /* ppc:2239 beqlr- cr0             */
        cmp nargs, #(2<<fixnumshift)    /* ppc:2238 cmpri cr2              */
        ldr arg_z, [vsp], #node_size    /* ppc:2240 vpop                   */
        b.lt 9f                         /* ppc:2241 bltlr cr2              */
        ldr arg_y, [vsp], #node_size    /* ppc:2242 vpop                   */
        b.eq 9f                         /* ppc:2243 beqlr cr2              */
        ldr arg_x, [vsp], #node_size    /* ppc:2244 vpop                   */
9:      ret                             /* ppc:2245 blr                    */
        /* Improper tail: discard pushes, signal XNOSPREAD (ppc:2247-2252) */
3:      add vsp, vsp, imm0              /* ppc:2248                        */
        mov arg_z, arg_y                /* ppc:2249 recover original arg_z */
        mov arg_y, #XNOSPREAD           /* ppc:2250                        */
        set_nargs 2                     /* ppc:2251                        */
        b _SPksignalerr                 /* ppc:2252                        */
endsp spreadargz

/* ========== TAIL CALLS ========== */

/* ported from ppc-spentry.s:2256-2277 (PPC64 branch) */
spentry tfuncallgen
        /* PORT-TODO: fn-volatile protocol decision needed */
        /* Tail funcall - general case */
        cmp nargs, #(nargregs << fixnumshift)
        ldr x30, [sp, #lisp_frame.savelr]
        ldr fn, [sp, #lisp_frame.savefn]
        b.le 2f

        /* Some args vpushed - slide them down */
        ldr imm0, [sp, #lisp_frame.savevsp]
        add sp, sp, #lisp_frame.size

        sub imm1, nargs, #(nargregs << fixnumshift)
        add imm1, imm1, vsp
1:      ldr temp2, [imm1, #-node_size]!
        cmp imm1, vsp
        str temp2, [imm0, #-node_size]!
        b.ne 1b
        mov vsp, imm0
        b _SPfuncall

2:      ldr vsp, [sp, #lisp_frame.savevsp]
        add sp, sp, #lisp_frame.size
        b _SPfuncall
endsp tfuncallgen

/* ported from ppc-spentry.s:2282-2297 (PPC64 branch) */
spentry tfuncallslide
        /* PORT-TODO: fn-volatile protocol decision needed */
        /* Tail funcall - args were vpushed */
        ldr x30, [sp, #lisp_frame.savelr]
        ldr fn, [sp, #lisp_frame.savefn]
        ldr imm0, [sp, #lisp_frame.savevsp]
        add sp, sp, #lisp_frame.size

        sub imm1, nargs, #(nargregs << fixnumshift)
        add imm1, imm1, vsp
1:      ldr temp2, [imm1, #-node_size]!
        cmp imm1, vsp
        str temp2, [imm0, #-node_size]!
        b.ne 1b
        mov vsp, imm0
        b _SPfuncall
endsp tfuncallslide

/* tfuncallvsp (ppc:2299-2306) lives in spentry-C-bind-catch-throw.s
   (the W4 gate-32 port); an earlier draft here duplicated the symbol. */

/* ported from ppc-spentry.s:2313-2336 (PPC64 branch) */
spentry tcallsymgen
        /* PORT-TODO: fn-volatile protocol decision needed */
        /* Tail call symbol - general case */
        cmp nargs, #(nargregs << fixnumshift)
        ldr x30, [sp, #lisp_frame.savelr]
        ldr fn, [sp, #lisp_frame.savefn]
        b.le 2f

        ldr imm0, [sp, #lisp_frame.savevsp]
        add sp, sp, #lisp_frame.size

        sub imm1, nargs, #(nargregs << fixnumshift)
        add imm1, imm1, vsp
1:      ldr temp2, [imm1, #-node_size]!
        cmp imm1, vsp
        str temp2, [imm0, #-node_size]!
        b.ne 1b
        mov vsp, imm0
        /* Jump to fname */
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.code_vector]
        br temp0

2:      ldr vsp, [sp, #lisp_frame.savevsp]
        add sp, sp, #lisp_frame.size
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.code_vector]
        br temp0
endsp tcallsymgen

/* ported from ppc-spentry.s:2341-2356 (PPC64 branch) */
spentry tcallsymslide
        /* PORT-TODO: fn-volatile protocol decision needed */
        /* Tail call symbol - args vpushed */
        ldr x30, [sp, #lisp_frame.savelr]
        ldr fn, [sp, #lisp_frame.savefn]
        ldr imm0, [sp, #lisp_frame.savevsp]
        add sp, sp, #lisp_frame.size

        sub imm1, nargs, #(nargregs << fixnumshift)
        add imm1, imm1, vsp
1:      ldr temp2, [imm1, #-node_size]!
        cmp imm1, vsp
        str temp2, [imm0, #-node_size]!
        b.ne 1b
        mov vsp, imm0
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.code_vector]
        br temp0
endsp tcallsymslide

/* ported from ppc-spentry.s:2369-2372 (PPC64 branch) */
spentry tcallnfngen
        /* Tail call nfn - general */
        cmp nargs, #(nargregs << fixnumshift)
        b.le _SPtcallnfnvsp
        b _SPtcallnfnslide
endsp tcallnfngen

/* ported from ppc-spentry.s:2376-2391 (PPC64 branch) */
spentry tcallnfnslide
        /* PORT-TODO: fn-volatile protocol decision needed */
        ldr x30, [sp, #lisp_frame.savelr]
        ldr fn, [sp, #lisp_frame.savefn]
        ldr imm0, [sp, #lisp_frame.savevsp]
        add sp, sp, #lisp_frame.size

        sub imm1, nargs, #(nargregs << fixnumshift)
        add imm1, imm1, vsp
1:      ldr fname, [imm1, #-node_size]!
        cmp imm1, vsp
        str fname, [imm0, #-node_size]!
        b.ne 1b
        mov vsp, imm0
        ldr temp0, [nfn, #_function.code_vector]
        br temp0
endsp tcallnfnslide

/* ========== BUILTIN ARITHMETIC ========== */

/* ported from ppc-spentry.s:5492-5517 (PPC64 branch) */
spentry builtin_plus
        /* Fixnum addition with overflow to bignum */
        and imm0, arg_y, #tagmask
        and imm1, arg_z, #tagmask
        cmp imm0, #tag_fixnum
        b.ne 1f
        cmp imm1, #tag_fixnum
        b.ne 1f

        adds arg_z, arg_y, arg_z
        b.vc 2f  /* No overflow */

        /* Overflow - make bignum */
        asr imm0, arg_z, #fixnumshift
        eor imm0, imm0, #0xe000000000000000
        mov imm1, #two_digit_bignum_header
        Misc_Alloc_Fixed arg_z, imm1, aligned_bignum_size(2)
        str imm0, [arg_z, #misc_data_offset]
2:      ret

1:      /* Not both fixnums - dispatch to Lisp */
        jump_builtin _builtin_plus, 2  /* ppc:5517 */
endsp builtin_plus

/* ported from ppc-spentry.s:5518-5543 (PPC64 branch) */
spentry builtin_minus
        and imm0, arg_y, #tagmask
        and imm1, arg_z, #tagmask
        cmp imm0, #tag_fixnum
        b.ne 1f
        cmp imm1, #tag_fixnum
        b.ne 1f

        subs arg_z, arg_y, arg_z
        b.vc 2f

        /* Overflow to bignum */
        asr imm0, arg_z, #fixnumshift
        eor imm0, imm0, #0xe000000000000000
        mov imm1, #two_digit_bignum_header
        Misc_Alloc_Fixed arg_z, imm1, aligned_bignum_size(2)
        str imm0, [arg_z, #misc_data_offset]
2:      ret

1:      jump_builtin _builtin_minus, 2  /* ppc:5543 */
endsp builtin_minus

/* ported from ppc-spentry.s:5544-5576 (PPC64 branch) */
spentry builtin_times
        and imm0, arg_y, #tagmask
        and imm1, arg_z, #tagmask
        cmp imm0, #tag_fixnum
        b.ne 1f
        cmp imm1, #tag_fixnum
        b.ne 1f

        asr imm2, arg_y, #fixnumshift
        /* Multiply with overflow detection */
        asr imm3, arg_z, #fixnumshift
        mul imm1, imm3, imm2  /* low 64 bits */
        smulh imm0, imm3, imm2  /* high 64 bits */

        /* Check if result fits in fixnum.  GC SAFETY (Matt 2026-07-11):
           imm scratch, never a node reg.
           16m5t FIX: the old single test `asr imm1,#61 == smulh` accepted
           s62 products; fixnums are s61 (value bits = 64-3).  2^60 then
           boxed to 2^63 (= -2^60), and -2^61 boxed to EXACTLY 0 -- the
           *base-power* doubling loop wedged at 0 (l0-int.lisp:155 spin).
           PPC gets this free by multiplying BOXED*unboxed (mulldo. OV ==
           fixnum overflow, ppc:5548); with both operands unboxed we need
           BOTH: product fits s64 (smulh == sign of low) AND low fits s61
           (sbfx round-trip, Matt's makes64 idiom). */
        asr imm4, imm1, #63
        cmp imm4, imm0
        b.ne 2f
        sbfx imm4, imm1, #0, #(nbits_in_word - nfixnumtagbits)
        cmp imm4, imm1
        b.ne 2f
        lsl arg_z, imm1, #fixnumshift
        ret

2:      /* Result doesn't fit in fixnum - call makes128 */
        b _SPmakes128

1:      jump_builtin _builtin_times, 2  /* ppc:5576 */
endsp builtin_times

/* ========== BUILTIN COMPARISONS ========== */

/* ported from ppc-spentry.s:5581-5594 (PPC64 branch) */
spentry builtin_eq
        and imm0, arg_y, #tagmask
        and imm1, arg_z, #tagmask
        cmp imm0, #tag_fixnum
        b.ne 1f
        cmp imm1, #tag_fixnum
        b.ne 1f
        cmp arg_y, arg_z
        mov arg_z, rnil
        b.ne 2f
        /* PORT-TODO: load t_value constant */
        add arg_z, rnil, #t_offset      /* t_value = NIL + t_offset        */
2:      ret
1:      jump_builtin _builtin_eq, 2  /* ppc:5594 */
endsp builtin_eq

/* ported from ppc-spentry.s:5596-5609 (PPC64 branch) */
spentry builtin_ne
        and imm0, arg_y, #tagmask
        and imm1, arg_z, #tagmask
        cmp imm0, #tag_fixnum
        b.ne 1f
        cmp imm1, #tag_fixnum
        b.ne 1f
        cmp arg_y, arg_z
        mov arg_z, rnil
        b.eq 2f
        add arg_z, rnil, #t_offset      /* t_value = NIL + t_offset        */
2:      ret
1:      jump_builtin _builtin_ne, 2  /* ppc:5609 */
endsp builtin_ne

/* ported from ppc-spentry.s:5611-5624 (PPC64 branch) */
spentry builtin_gt
        and imm0, arg_y, #tagmask
        and imm1, arg_z, #tagmask
        cmp imm0, #tag_fixnum
        b.ne 1f
        cmp imm1, #tag_fixnum
        b.ne 1f
        cmp arg_y, arg_z
        mov arg_z, rnil
        b.le 2f
        add arg_z, rnil, #t_offset      /* t_value = NIL + t_offset        */
2:      ret
1:      jump_builtin _builtin_gt, 2  /* ppc:5624 */
endsp builtin_gt

/* ported from ppc-spentry.s:5626-5639 (PPC64 branch) */
spentry builtin_ge
        and imm0, arg_y, #tagmask
        and imm1, arg_z, #tagmask
        cmp imm0, #tag_fixnum
        b.ne 1f
        cmp imm1, #tag_fixnum
        b.ne 1f
        cmp arg_y, arg_z
        mov arg_z, rnil
        b.lt 2f
        add arg_z, rnil, #t_offset      /* t_value = NIL + t_offset        */
2:      ret
1:      jump_builtin _builtin_ge, 2  /* ppc:5639 */
endsp builtin_ge

/* ported from ppc-spentry.s:5641-5654 (PPC64 branch) */
spentry builtin_lt
        and imm0, arg_y, #tagmask
        and imm1, arg_z, #tagmask
        cmp imm0, #tag_fixnum
        b.ne 1f
        cmp imm1, #tag_fixnum
        b.ne 1f
        cmp arg_y, arg_z
        mov arg_z, rnil
        b.ge 2f
        add arg_z, rnil, #t_offset      /* t_value = NIL + t_offset        */
2:      ret
1:      jump_builtin _builtin_lt, 2  /* ppc:5654 */
endsp builtin_lt

/* ported from ppc-spentry.s:5656-5669 (PPC64 branch) */
spentry builtin_le
        and imm0, arg_y, #tagmask
        and imm1, arg_z, #tagmask
        cmp imm0, #tag_fixnum
        b.ne 1f
        cmp imm1, #tag_fixnum
        b.ne 1f
        cmp arg_y, arg_z
        mov arg_z, rnil
        b.gt 2f
        add arg_z, rnil, #t_offset      /* t_value = NIL + t_offset        */
2:      ret
1:      jump_builtin _builtin_le, 2  /* ppc:5669 */
endsp builtin_le

/* ported from ppc-spentry.s:5672-5689 (PPC64 branch) */
spentry builtin_eql
        cmp arg_y, arg_z
        b.eq 1f

        and imm2, arg_y, #fulltagmask
        and imm3, arg_z, #fulltagmask
        cmp imm2, #fulltag_misc
        b.ne 2f
        cmp imm3, #fulltag_misc
        b.ne 2f

        ldrb w0, [arg_y, #misc_subtag_offset]
        ldrb w1, [arg_z, #misc_subtag_offset]
        cmp imm0, imm1
        b.ne 2f

        /* Same subtag - dispatch to generic eql */
        jump_builtin _builtin_eql, 2  /* ppc:5685 */

1:      add arg_z, rnil, #t_offset      /* t_value = NIL + t_offset        */
        ret
2:      mov arg_z, rnil
        ret
endsp builtin_eql

/* ========== BUILTIN SEQUENCE OPS ========== */

/* ported from ppc-spentry.s:5691-5759 (PPC64 branch) */
spentry builtin_length
        cmp arg_z, rnil
        b.eq 1f

        /* Check typecode */
        and imm0, arg_z, #fulltagmask
        cmp imm0, #fulltag_misc
        b.ne 3f  /* Maybe cons */

        ldrb w0, [arg_z, #misc_subtag_offset]
        cmp imm0, #subtag_simple_vector
        b.eq 0f
        cmp imm0, #subtag_vectorH
        b.eq 2f

        /* Check if CL ivector (ppc:5698-5700 ivector_typecode_p + compare).
           Exclude node-headers (arrayH etc.): they are >= min_cl_ivector_subtag
           numerically in this tag scheme but are not CL sequences, so dispatch
           them to Lisp.  See builtin_aref1 for the rationale. */
        and imm1, imm0, #tagmask
        cmp imm1, #tag_nodeheader
        b.eq 8f
        cmp imm0, #min_cl_ivector_subtag
        b.ge 0f

        /* Check for cons */
        and imm0, arg_z, #fulltagmask
        cmp imm0, #fulltag_cons
        b.eq 4f
        b 8f  /* Error */

0:      /* Simple vector or ivector - get length from header */
        ldr imm0, [arg_z, #misc_header_offset]
        /* Extract length as fixnum */
        lsr imm0, imm0, #num_subtag_bits
        lsl arg_z, imm0, #fixnumshift
        ret

1:      /* nil - length 0 */
        mov arg_z, xzr
        ret

2:      /* vectorH - load logsize slot */
        ldr arg_z, [arg_z, #vectorH.logsize]
        ret

3:      /* Check if cons */
        cmp imm0, #fulltag_cons
        b.ne 8f
4:      /* List - count with Floyd cycle detection (ppc:5718-5737 PPC64).
           PPC keeps cr0/cr1/cr7 live; flattened one-compare-per-branch:
           fast pointer steps every iteration, slow pointer every SECOND
           iteration (odd count), cycle iff fast==slow. */
        mov temp2, #(-1 << fixnumshift)     /* ppc:5719                    */
        mov temp0, arg_z                    /* ppc:5720 fast pointer       */
        mov temp1, arg_z                    /* ppc:5721 slow pointer       */
5:      and imm0, temp0, #fulltagmask       /* ppc:5723 extract_fulltag    */
        add temp2, temp2, #fixnumone        /* ppc:5726                    */
        cmp temp0, rnil                     /* ppc:5724 cmpdi cr7          */
        b.eq 9f                             /* ppc:5727 done: proper end   */
        cmp imm0, #fulltag_cons             /* ppc:5725 cmpdi cr1          */
        b.ne 8f                             /* ppc:5729 not a list         */
        and imm1, temp1, #fulltagmask       /* ppc:5730                    */
        ldr temp0, [temp0, #cons.cdr]       /* ppc:5731 _cdr fast          */
        tst temp2, #fixnumone               /* ppc:5728 andi. (odd/even)   */
        b.eq 5b                             /* ppc:5733 even: skip slow    */
        cmp imm1, #fulltag_cons             /* ppc:5732 cmpdi cr1          */
        b.ne 8f                             /* ppc:5734                    */
        ldr temp1, [temp1, #cons.cdr]       /* ppc:5735 _cdr slow          */
        cmp temp0, temp1                    /* ppc:5736                    */
        b.ne 5b                             /* ppc:5737 no cycle yet       */
        /* fast==slow: circular; fall into the generic dispatch (ppc:5755) */
8:      /* Not a sequence - dispatch to Lisp */
        jump_builtin _builtin_length, 1  /* ppc:5756 */
9:      mov arg_z, temp2
        ret
endsp builtin_length

/* ported from ppc-spentry.s:5761-5784 (PPC64 branch) */
spentry builtin_seqtype
        cmp arg_z, rnil
        b.eq 1f

        and imm0, arg_z, #fulltagmask
        cmp imm0, #fulltag_cons
        b.eq 1f

        cmp imm0, #fulltag_misc
        b.ne 2f
        ldrb w0, [arg_z, #misc_subtag_offset]
        cmp imm0, #subtag_simple_vector
        b.eq 0f
        cmp imm0, #subtag_vectorH
        b.eq 0f

        /* Check if CL ivector (ppc:5775-5777 ivector_typecode_p + compare).
           Exclude node-headers (arrayH etc.) → dispatch to Lisp.  See
           builtin_aref1 for the tag-scheme rationale. */
        and imm1, imm0, #tagmask
        cmp imm1, #tag_nodeheader
        b.eq 2f
        cmp imm0, #min_cl_ivector_subtag
        b.lt 2f

0:      mov arg_z, rnil
        ret
1:      add arg_z, rnil, #t_offset      /* t_value = NIL + t_offset        */
        ret
2:      jump_builtin _builtin_seqtype, 1  /* ppc:5784 */
endsp builtin_seqtype

/* ported from ppc-spentry.s:5786-5802 (PPC64 branch).
 * PPC keeps three CRs live (cr0 = car match, cr1 = tail nil, cr2 = pair
 * nil); flattened with one compare per branch.  trap_unless_list is only
 * reached with a non-nil operand, so the cons-tag check suffices (on
 * Matt's design nil has its own fulltag, arm64-constants.h:94). */
spentry builtin_assq
        cmp arg_z, rnil                 /* ppc:5787                        */
        b.eq 9f                         /* ppc:5788 beqlr                  */
1:      and imm0, arg_z, #fulltagmask   /* ppc:5789 trap_unless_list       */
        cmp imm0, #fulltag_cons
        b.eq 0f
        uuo_interr error_object_not_list, arg_z
0:      ldr arg_x, [arg_z, #cons.car]   /* ppc:5790                        */
        ldr arg_z, [arg_z, #cons.cdr]   /* ppc:5791                        */
        cmp arg_x, rnil                 /* ppc:5792 cmpri cr2              */
        b.eq 2f                         /* ppc:5794 beq cr2 (skip nil pair)*/
        and imm0, arg_x, #fulltagmask   /* ppc:5795 trap_unless_list       */
        cmp imm0, #fulltag_cons
        b.eq 3f
        uuo_interr error_object_not_list, arg_x
3:      ldr temp0, [arg_x, #cons.car]   /* ppc:5796                        */
        cmp temp0, arg_y                /* ppc:5797                        */
        b.ne 2f                         /* ppc:5798                        */
        mov arg_z, arg_x                /* ppc:5799 found                  */
        ret                             /* ppc:5800                        */
2:      cmp arg_z, rnil                 /* ppc:5793 cmpri cr1 (recomputed) */
        b.ne 1b                         /* ppc:5801                        */
9:      ret                             /* ppc:5802                        */
endsp builtin_assq

/* ported from ppc-spentry.s:5804-5815 (PPC64 branch); flag/trap notes as
 * builtin_assq.  Returns the tail of arg_z whose car is eq to arg_y. */
spentry builtin_memq
        cmp arg_z, rnil                 /* ppc:5805 cmpri cr1              */
        b 2f                            /* ppc:5806                        */
1:      and imm0, arg_z, #fulltagmask   /* ppc:5807 trap_unless_list       */
        cmp imm0, #fulltag_cons
        b.eq 0f
        uuo_interr error_object_not_list, arg_z
0:      ldr arg_x, [arg_z, #cons.car]   /* ppc:5808                        */
        ldr temp0, [arg_z, #cons.cdr]   /* ppc:5809                        */
        cmp arg_x, arg_y                /* ppc:5810                        */
        b.eq 9f                         /* ppc:5812 beqlr (found this cons)*/
        mov arg_z, temp0                /* ppc:5813                        */
        cmp arg_z, rnil                 /* ppc:5811 cmpri cr1 (recomputed) */
2:      b.ne 1b                         /* ppc:5814                        */
9:      ret                             /* ppc:5815                        */
endsp builtin_memq

/* ========== BUILTIN CALL DISPATCHERS ========== */

/* ported from ppc-spentry.s:5270-5274 (PPC64 branch)
 * callbuiltin: imm0 = boxed index into %builtin-functions%; dispatch to that
 * symbol's function definition.  nargs already set by caller. */
spentry callbuiltin
        /* ppc:5271 ref_nrs_value(fname,builtin_functions) */
        /* ppc:5272 la imm0,misc_data_offset(imm0) -- add data bias to index */
        /* ppc:5273 ldrx(fname,fname,imm0) -- load symbol from vector */
        /* ppc:5274 jump_fname() */
        ref_nrs_value fname, builtin_functions
        add imm0, imm0, #misc_data_offset
        ldr fname, [fname, imm0]
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.code_vector]
        br temp0
endsp callbuiltin

/* ported from ppc-spentry.s:5280-5285 (PPC64 branch) */
spentry callbuiltin0
        set_nargs 0                     /* ppc:5281 */
        /* ppc:5282-5285: ref_nrs_value + la + ldrx + jump_fname */
        ref_nrs_value fname, builtin_functions
        add imm0, imm0, #misc_data_offset
        ldr fname, [fname, imm0]
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.code_vector]
        br temp0
endsp callbuiltin0

/* ported from ppc-spentry.s:5287-5292 (PPC64 branch) */
spentry callbuiltin1
        set_nargs 1                     /* ppc:5289 (set_nargs before ref in PPC) */
        /* ppc:5288,5290-5292: ref_nrs_value + la + ldrx + jump_fname */
        ref_nrs_value fname, builtin_functions
        add imm0, imm0, #misc_data_offset
        ldr fname, [fname, imm0]
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.code_vector]
        br temp0
endsp callbuiltin1

/* ported from ppc-spentry.s:5294-5299 (PPC64 branch) */
spentry callbuiltin2
        set_nargs 2                     /* ppc:5295 */
        /* ppc:5296-5299: ref_nrs_value + la + ldrx + jump_fname */
        ref_nrs_value fname, builtin_functions
        add imm0, imm0, #misc_data_offset
        ldr fname, [fname, imm0]
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.code_vector]
        br temp0
endsp callbuiltin2

/* ported from ppc-spentry.s:5302-5307 (PPC64 branch) */
spentry callbuiltin3
        set_nargs 3                     /* ppc:5303 */
        /* ppc:5304-5307: ref_nrs_value + la + ldrx + jump_fname */
        ref_nrs_value fname, builtin_functions
        add imm0, imm0, #misc_data_offset
        ldr fname, [fname, imm0]
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.code_vector]
        br temp0
endsp callbuiltin3

/* ========== FRAME RESTORE ========== */

/* ported from ppc-spentry.s:5310-5318 (PPC64 branch)
 * popj: restore context from lisp frame and return.
 * PPC64 loads loc_pc from frame then mtlr+blr; ARM64 has no loc_pc register --
 * load directly into lr (x30) and ret. */
spentry popj
        .globl C(popj)
C(popj):
        ldr x30, [sp, #lisp_frame.savelr]      /* ppc:5313 ldr(loc_pc,savelr) */
        ldr vsp, [sp, #lisp_frame.savevsp]      /* ppc:5314 */
        ldr fn, [sp, #lisp_frame.savefn]        /* ppc:5316 */
        discard_lisp_frame                      /* ppc:5317 */
        ret                                     /* ppc:5318 blr */
endsp popj

/* ========== BUILTIN LOGICAL OPERATIONS ========== */

/* ported from ppc-spentry.s:5823-5845 (PPC64 branch)
 * builtin_logbitp: (logbitp arg_y arg_z) for fixnum args where
 * 0 <= arg_y < 61 (logbitp_max_bit on 64-bit). */
.set logbitp_max_bit, 61

spentry builtin_logbitp
        /* ppc:5825 cmplri(cr2,arg_y,logbitp_max_bit<<fixnum_shift) */
        cmp arg_y, #(logbitp_max_bit << fixnumshift)
        /* ppc:5826-5829 extract tags, check both fixnum */
        and imm0, arg_y, #tagmask
        and imm1, arg_z, #tagmask
        cmp imm0, #tag_fixnum
        b.ne 1f
        cmp imm1, #tag_fixnum
        b.ne 1f
        /* Bail if arg_y >= logbitp_max_bit (unsigned compare already set above;
         * but we clobbered flags with the tag checks -- recompute) */
        cmp arg_y, #(logbitp_max_bit << fixnumshift)
        b.hs 1f
        /* ppc:5830 unbox_fixnum(imm0,arg_y) */
        asr imm0, arg_y, #fixnumshift
        /* ppc:5831 subfic imm0,imm0,logbitp_max_bit -> compute shift amount */
        /* PPC64: rldcl imm0,arg_z,imm0,63 = rotate arg_z left by imm0, clear
         * bits 0-62, leaving only bit 63 (the target bit rotated to LSB).
         * ARM64 equivalent: shift arg_z right by (logbitp_max_bit - imm0)
         * positions within the fixnum bits, then AND #1. But PPC's subfic
         * computes (logbitp_max_bit - bit_index), so after unboxing arg_z we
         * right-shift by that amount. Actually simpler: just shift arg_z right
         * by the bit position and mask. arg_z is a tagged fixnum so bit N of
         * the fixnum value is at position N+fixnumshift in the register. */
        add imm0, imm0, #fixnumshift    /* adjust for tag bits */
        lsr imm0, arg_z, imm0           /* shift target bit to bit 0 */
        and imm0, imm0, #1              /* isolate the bit */
        /* ppc:5834 mulli imm0,imm0,t_offset; ppc:5842 addi arg_z,nil_value */
        mov imm1, #t_offset
        mul imm0, imm0, imm1
        add arg_z, rnil, imm0
        ret
1:      /* ppc:5845 */
        jump_builtin _builtin_logbitp, 2
endsp builtin_logbitp

/* ported from ppc-spentry.s:5847-5857 (PPC64 branch) */
spentry builtin_logior
        and imm0, arg_y, #tagmask       /* ppc:5848 */
        and imm1, arg_z, #tagmask       /* ppc:5849 */
        cmp imm0, #tag_fixnum           /* ppc:5850 */
        b.ne 1f                         /* ppc:5852 */
        cmp imm1, #tag_fixnum           /* ppc:5851 */
        b.ne 1f                         /* ppc:5853 */
        orr arg_z, arg_y, arg_z         /* ppc:5854 */
        ret                             /* ppc:5855 */
1:      jump_builtin _builtin_logior, 2 /* ppc:5857 */
endsp builtin_logior

/* ported from ppc-spentry.s:5859-5869 (PPC64 branch) */
spentry builtin_logand
        and imm0, arg_y, #tagmask       /* ppc:5860 */
        and imm1, arg_z, #tagmask       /* ppc:5861 */
        cmp imm0, #tag_fixnum           /* ppc:5862 */
        b.ne 1f                         /* ppc:5864 */
        cmp imm1, #tag_fixnum           /* ppc:5863 */
        b.ne 1f                         /* ppc:5865 */
        and arg_z, arg_y, arg_z         /* ppc:5866 */
        ret                             /* ppc:5867 */
1:      jump_builtin _builtin_logand, 2 /* ppc:5869 */
endsp builtin_logand

/* ported from ppc-spentry.s:5871-5990 (PPC64 branch)
 * builtin_ash: arithmetic shift.  Positive arg_z = left shift, negative = right.
 * PPC64 branch only (5872-5930). */
spentry builtin_ash
        /* ppc:5873 cmpdi cr1,arg_z,0 */
        cmp arg_z, #0
        /* ppc:5874-5877 extract tags, compare to fixnum */
        and imm0, arg_y, #tagmask
        and imm1, arg_z, #tagmask
        cmp imm0, #tag_fixnum
        b.ne 9f
        cmp imm1, #tag_fixnum
        b.ne 9f
        /* ppc:5878 cmpdi cr2,arg_z,-(63<<3) -- check shift magnitude */
        /* Retest arg_z sign (flags clobbered by tag checks) */
        cmp arg_z, #0
        b.gt 2f
        /* ppc:5881 bne cr1,0f -- if arg_z != 0, proceed; else return arg_y */
        b.ne 0f
        mov arg_z, arg_y                /* ppc:5882 (ash n 0) => n */
        ret                             /* ppc:5883 */
0:
        /* Negative shift (right shift) */
        /* ppc:5885 unbox_fixnum(imm1,arg_y) */
        asr imm1, arg_y, #fixnumshift
        /* ppc:5886 unbox_fixnum(imm0,arg_z) -- shift count (negative) */
        asr imm0, arg_z, #fixnumshift
        /* ppc:5889 neg imm2,imm0 -- positive shift count */
        neg imm2, imm0
        /* ppc:5878/5890 bgt cr2 / li imm2,63 -- clamp to 63 */
        cmp imm2, #63
        b.le 1f
        mov imm2, #63
1:
        /* ppc:5893 srad imm0,imm1,imm2 */
        asr imm0, imm1, imm2
        /* ppc:5894 box_fixnum(arg_z,imm0) */
        lsl arg_z, imm0, #fixnumshift
        ret                             /* ppc:5895 */
2:
        /* Positive shift (left shift) */
        /* ppc:5897 Integer-length of arg_y/imm1 to imm2 */
        asr imm1, arg_y, #fixnumshift   /* ppc:5885 (reuse) */
        asr imm0, arg_z, #fixnumshift   /* ppc:5886 (reuse) */
        /* ppc:5898 cntlzd. imm2,imm1 */
        cmp imm1, #0
        b.ge 3f
        /* Negative value: count leading zeros of NOT(imm1) */
        mvn imm2, imm1                  /* ppc:5900 not imm2,imm1 */
        clz imm2, imm2                  /* ppc:5901 cntlzd imm2,imm2 */
        b 4f
3:      clz imm2, imm1                  /* ppc:5898 cntlzd imm2,imm1 */
4:
        /* ppc:5903 subfic imm2,imm2,64 -- integer-length = 64 - clz */
        mov imm3, #64
        sub imm2, imm3, imm2
        /* ppc:5904 add imm2,imm2,imm0 -- total bits needed */
        add imm2, imm2, imm0
        /* ppc:5905 cmpdi cr1,imm2,63-fixnumshift -- fits in fixnum? */
        cmp imm2, #(63 - fixnumshift)
        /* ppc:5907 sld imm2,imm1,imm0 -- perform the shift */
        lsl imm2, imm1, imm0
        b.gt 6f
        /* ppc:5909 box_fixnum(arg_z,imm2) -- result fits */
        lsl arg_z, imm2, #fixnumshift
        ret                             /* ppc:5910 */
6:
        /* Result does not fit in a fixnum */
        /* ppc:5906 cmpdi cr2,imm0,64 */
        cmp imm0, #64
        b.gt 9f                         /* ppc:5912 shift > 64: bail to generic */
        b.eq ash_shift64                /* ppc:5913 shift == 64 exactly */
        /* ppc:5920-5925: Shift left by fewer than 64 bits, result not fixnum */
        /* ppc:5921 subfic imm0,imm0,64 */
        mov imm3, #64
        sub imm3, imm3, imm0           /* 64 - shift_count */
        /* Need to check sign for signed vs unsigned result */
        cmp imm1, #0
        b.lt 8f
        /* ppc:5923 srd imm0,imm1,imm0 -- high part (unsigned) */
        lsr imm0, imm1, imm3
        mov imm1, imm2                  /* ppc:5924 mr imm1,imm2 (low part) */
        b _SPmakeu128                   /* ppc:5925 */
8:
        /* ppc:5927 srad imm0,imm1,imm0 -- high part (signed) */
        asr imm0, imm1, imm3
        mov imm1, imm2                  /* ppc:5928 */
        b _SPmakes128                   /* ppc:5929 */
ash_shift64:
        /* ppc:5915-5918: Shift left by exactly 64 bits */
        mov imm0, imm1                  /* ppc:5915 mr imm0,imm1 */
        mov imm1, #0                    /* ppc:5916 li imm1,0 */
        /* ppc:5917-5918: beq _SPmakes128 / b _SPmakeu128
         * PPC branches on cr0.eq from cntlzd. -- this reflects whether
         * original value was negative. */
        cmp imm0, #0
        b.lt _SPmakes128
        b _SPmakeu128
9:
        /* ppc:5990 */
        jump_builtin _builtin_ash, 2
endsp builtin_ash

/* ported from ppc-spentry.s:5992-6013 (PPC64 branch)
 * builtin_negate: negate a fixnum, overflow to bignum. */
spentry builtin_negate
        /* ppc:5993 extract_lisptag_(imm0,arg_z) */
        and imm0, arg_z, #tagmask
        /* ppc:5994 bne- cr0,1f */
        cmp imm0, #tag_fixnum
        b.ne 1f
        /* ppc:5995 nego. arg_z,arg_z -- negate with overflow detect.
         * ARM64: negs sets NZCV; V=1 iff overflow (arg_z == INT64_MIN-equivalent,
         * i.e., most-negative-fixnum). */
        negs arg_z, arg_z
        /* ppc:5996 bnslr+ -- return if no overflow */
        b.vc 2f
        /* Overflow: arg_z holds the WRAPPED negation of most-negative-fixnum.
         * ppc:5997 mtxer rzero (clear OV -- no ARM64 equivalent needed)
         * ppc:5998-6004: unbox and store as a two-digit bignum with the sign
         * bit flipped (PPC's rotldi+xoris = flip bit 2^63 of the unboxed
         * value; the wrapped unboxed result is -2^60 but the true value is
         * +2^60, and eor #0xe000... corrects the top bits).  This is EXACTLY
         * Matt's own _SPfix_overflow body (arm64-spentry.s:10-17) -- mirror it. */
        asr imm0, arg_z, #fixnumshift          /* ppc:5998 unbox_fixnum */
        eor imm0, imm0, #0xe000000000000000    /* ppc:6001-6002 sign-flip trick */
        mov imm1, #two_digit_bignum_header     /* ppc:6000 */
        Misc_Alloc_Fixed arg_z, imm1, aligned_bignum_size(2)  /* ppc:6003 */
        str imm0, [arg_z, #misc_data_offset]   /* ppc:6004 */
2:      ret
1:      /* ppc:6013 */
        jump_builtin _builtin_negate, 1
endsp builtin_negate

/* ported from ppc-spentry.s:6015-6025 (PPC64 branch) */
spentry builtin_logxor
        and imm0, arg_y, #tagmask       /* ppc:6016 */
        and imm1, arg_z, #tagmask       /* ppc:6017 */
        cmp imm0, #tag_fixnum           /* ppc:6018 */
        b.ne 1f                         /* ppc:6020 */
        cmp imm1, #tag_fixnum           /* ppc:6019 */
        b.ne 1f                         /* ppc:6021 */
        eor arg_z, arg_y, arg_z         /* ppc:6022 */
        ret                             /* ppc:6023 */
1:      jump_builtin _builtin_logxor, 2 /* ppc:6025 */
endsp builtin_logxor

/* ========== BUILTIN ARRAY ACCESS ========== */

/* ported from ppc-spentry.s:3213-3221 (PPC64 branch)
 * builtin_aref1: fast path for simple-vector / CL ivector aref;
 * falls through to _SPsubtag_misc_ref or dispatches to Lisp. */
spentry builtin_aref1
        /* ppc:3214 extract_typecode(imm0,arg_y) */
        and imm0, arg_y, #fulltagmask
        cmp imm0, #fulltag_misc
        b.ne 1f
        ldrb w0, [arg_y, #misc_subtag_offset]
        /* ppc:3215 cmpri(cr0,imm0,subtag_simple_vector) */
        cmp imm0, #subtag_simple_vector
        /* ppc:3216 box_fixnum(arg_x,imm0) -- save typecode for subtag_misc_ref */
        lsl arg_x, imm0, #fixnumshift
        b.eq _SPsubtag_misc_ref         /* ppc:3217 */
        /* ppc:3218 ivector_typecode_p(imm1,imm0,imm2) (ppc-macros.s:747):
           ONLY immediate-header subtags are CL ivectors; the macro zeroes a
           node-header subtag so the following compare fails.  We must do the
           same: node-header subtags (vectorH=0xae, arrayH=0xa6, ...) are
           numerically >= min_cl_ivector_subtag (0x94) in this tag scheme, so a
           raw compare misclassifies a complex array as a simple ivector and
           does a raw misc_ref on its HEADER (bound = header slot count = 5),
           instead of dispatching to Lisp %aref1 (which unwraps the vectorH).
           tag_nodeheader (low nlisptagbits) is shared by fulltag_nodeheader_0/1. */
        and imm1, imm0, #tagmask
        cmp imm1, #tag_nodeheader
        b.eq 1f
        cmp imm0, #min_cl_ivector_subtag  /* ppc:3219-3220 */
        b.ge _SPsubtag_misc_ref
1:      jump_builtin _builtin_aref1, 2  /* ppc:3221 */
endsp builtin_aref1

/* ported from ppc-spentry.s:6030-6038 (PPC64 branch)
 * builtin_aset1: fast path for simple-vector / CL ivector aset;
 * falls through to _SPsubtag_misc_set or dispatches to Lisp. */
spentry builtin_aset1
        /* ppc:6031 extract_typecode(imm0,arg_x) */
        and imm0, arg_x, #fulltagmask
        cmp imm0, #fulltag_misc
        b.ne 1f
        ldrb w0, [arg_x, #misc_subtag_offset]
        /* ppc:6032 cmpri(cr0,imm0,subtag_simple_vector) */
        cmp imm0, #subtag_simple_vector
        /* ppc:6033 box_fixnum(temp0,imm0) -- subtag_misc_set wants boxed typecode */
        lsl temp0, imm0, #fixnumshift
        b.eq _SPsubtag_misc_set         /* ppc:6034 */
        /* ppc:6035-6037 ivector_typecode_p + compare.  Exclude node-headers
           (vectorH/arrayH) before the >= test — see builtin_aref1 for the
           tag-scheme rationale (raw compare would treat a complex array as an
           ivector and misc_set into its header). */
        and imm1, imm0, #tagmask
        cmp imm1, #tag_nodeheader
        b.eq 1f
        cmp imm0, #min_cl_ivector_subtag
        b.ge _SPsubtag_misc_set
1:      jump_builtin _builtin_aset1, 3  /* ppc:6038 */
endsp builtin_aset1

/* ========== DEBUGGER / RESET ========== */

/* ported from ppc-spentry.s:6043-6046 (PPC64 branch)
 * breakpoint: enter the debugger.
 * PPC: tw 28,sp,sp (unconditional trap).
 *
 * ARM64: uuo_debug_trap (arm64-uuo.s: uuo_misc 3), NOT `brk #N'.  This
 * carried an #error and a "brk #<encoding> TBD" note for far too long; the
 * encoding was never undecided, it was already defined upstream and simply
 * not looked for.  Two things made it stay wrong: `breakpoint' is not
 * reached by our boot or test path, so nothing ever failed; and the note
 * assumed brk, which is the same mistake patch 0047 swept out of 61 other
 * sites -- brk does NOT satisfy the kernel's IS_UUO test
 * (`((i) & 0xffff0000) == 0', arm64-exceptions.c:175), so a brk here would
 * have reached handle_uuo's caller as an unrecognized SIGTRAP rather than
 * as a debugger entry.  udf, which is what uuo_misc emits, does satisfy it.
 * The macro reaches us via arm64-macros.s:4. */
spentry breakpoint
        mov x0, #0                      /* ppc:6044 li r3,0 */
        uuo_debug_trap                  /* ppc:6045 tw 28,sp,sp */
        ret                             /* ppc:6046 blr -- if handler returned */
endsp breakpoint

/* ported from ppc-spentry.s:4941-4949 (PPC64 branch)
 * reset: signal stack overflow by throwing to toplcatch with XSTKOVER.
 * PPC: nop (for alignment); ref_nrs_value(temp0,toplcatch); push tag+code;
 * set_nargs(1); b _SPthrow. */
spentry reset
        .globl _SPthrow
        nop                             /* ppc:4943 alignment nop */
        ref_nrs_value temp0, toplcatch  /* ppc:4944                        */
        mov temp1, #XSTKOVER            /* ppc:4945 (deferr errors.s:196)  */
        vpush1 temp0                    /* ppc:4946 */
        vpush1 temp1                    /* ppc:4947 */
        set_nargs 1                     /* ppc:4948 */
        b _SPthrow                      /* ppc:4949 */
endsp reset

/* ========== MULTIPLE-VALUE STACK OPERATIONS ========== */

/* ported from ppc-spentry.s:4954-4968 (PPC64 branch)
 * mvslide: slide nargs worth of values up the vstack.
 * imm0 = difference between current vsp and target (byte offset).
 * Copies nargs bytes of values from [vsp..vsp+nargs) to
 * [vsp+nargs+imm0 - nargs .. vsp+nargs+imm0), i.e., slides them
 * up by imm0 bytes, then sets vsp to the new base. */
spentry mvslide
        /* PPC computes imm2/imm0 BEFORE testing nargs (branch-delay style) */
        mov imm3, nargs                 /* ppc:4956 mr imm3,nargs */
        add imm2, vsp, nargs            /* ppc:4957 add imm2,vsp,nargs */
        add imm2, imm2, imm0           /* ppc:4958 add imm2,imm2,imm0 -- target end */
        add imm0, vsp, nargs            /* ppc:4959 add imm0,vsp,nargs -- source end */
        cbz nargs, 2f                   /* ppc:4955/4960 cmpri+beq (after setup) */
1:      /* ppc:4962-4966 copy loop (pre-decrement load/store) */
        sub imm3, imm3, #(1 << fixnumshift)  /* ppc:4963 subi imm3,fixnum_one */
        ldr temp0, [imm0, #-node_size]! /* ppc:4964 ldru(temp0,-node_size(imm0)) */
        str temp0, [imm2, #-node_size]! /* ppc:4965 stru(temp0,-node_size(imm2)) */
        cbnz imm3, 1b                   /* ppc:4962/4966 cmpri+bne */
2:      mov vsp, imm2                   /* ppc:4968 mr vsp,imm2 */
        ret                             /* ppc:4969 blr */
endsp mvslide

/* ========== ARGUMENT REGISTER OPERATIONS ========== */

/* ported from ppc-spentry.s:3859-3878 (PPC64 branch)
 * vpopargregs: pop 0-3 values from vstack into arg registers based on nargs.
 * nargs=0: do nothing. nargs=8(1 arg): pop arg_z.
 * nargs=16(2 args): pop arg_z, arg_y. nargs>=24(3+): pop arg_z, arg_y, arg_x. */
spentry vpopargregs
        cbz nargs, 4f                   /* ppc:3860 cmpri(cr0,nargs,0); beqlr */
        cmp nargs, #(2 << fixnumshift)  /* ppc:3861 cmpri(cr1,nargs,2<<fixnumshift) */
        b.eq 2f                         /* ppc:3863 beq cr1,yz */
        b.lt 3f                         /* ppc:3864 blt cr1,z */
        /* 3+ args: pop all three */
        ldr arg_z, [vsp, #(node_size * 0)]  /* ppc:3865 */
        ldr arg_y, [vsp, #(node_size * 1)]  /* ppc:3866 */
        ldr arg_x, [vsp, #(node_size * 2)]  /* ppc:3867 */
        add vsp, vsp, #(node_size * 3)  /* ppc:3868 la vsp,node_size*3(vsp) */
        ret                             /* ppc:3869 */
2:      /* 2 args */
        ldr arg_z, [vsp, #(node_size * 0)]  /* ppc:3871 */
        ldr arg_y, [vsp, #(node_size * 1)]  /* ppc:3872 */
        add vsp, vsp, #(node_size * 2)  /* ppc:3873 */
        ret                             /* ppc:3874 */
3:      /* 1 arg */
        ldr arg_z, [vsp, #(node_size * 0)]  /* ppc:3876 */
        add vsp, vsp, #(node_size * 1)  /* ppc:3877 */
4:      ret                             /* ppc:3878 / 3862 beqlr fallthrough */
endsp vpopargregs

/* ========== MULTIPLE-VALUE PASS VIA SYMBOL ========== */

/* ported from ppc-spentry.s:6886-6898 (PPC64 branch)
 * mvpasssym: like mvpass, but fname is known to be a symbol.
 * Build lisp frame, set lr to ret1val_addr, jump through fname. */
spentry mvpasssym
        /* ppc:6887 cmpri(cr0,nargs,node_size*nargregs) */
        cmp nargs, #(node_size * nargregs)
        /* ppc:6888 mflr loc_pc -- save return address; ARM64: lr already is it */
        mov imm0, vsp                   /* ppc:6889 mr imm0,vsp */
        b.le 1f                         /* ppc:6890 ble+ cr0,1f */
        sub imm0, imm0, #(node_size * nargregs)  /* ppc:6891 */
        add imm0, imm0, nargs          /* ppc:6892 */
1:
        /* ppc:6894 build_lisp_frame(fn,loc_pc,imm0) -- MARKER frame
         * (Matt's popj layout; no backlink word). */
        sub sp, sp, #lisp_frame.size
        mov temp0, #lisp_frame_marker
        str temp0, [sp, #lisp_frame.marker]
        str imm0, [sp, #lisp_frame.savevsp]
        str fn, [sp, #lisp_frame.savefn]
        str x30, [sp, #lisp_frame.savelr]
        /* ppc:6895 ref_global(loc_pc,ret1val_addr); ppc:6897 mtlr */
        ref_global lr, ret1val_addr     /* ppc:6895+6897 */
        mov fn, xzr                     /* ppc:6896 li fn,0 */
        /* ppc:6898 jump_fname() */
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.code_vector]
        br temp0
endsp mvpasssym

/* NOTES */

/* OPEN #error SITES (deduped in upstream-port/MISSING-CONSTANTS-RATIFY.md):
 * - breakpoint trap encoding (this file's only remaining #error) --
 *   Matt's-call ratify item.
 * RESOLVED since first draft: NRS/lisp_globals ref idiom
 * (arm64-globals-proposed.s -- jump_builtin, callbuiltin, ksignalerr,
 * reset, mvpass all real now), EGC write-barrier globals (rplaca/rplacd),
 * trap encodings (canonical arm64-uuo.s scheme + PROPOSED extensions; see
 * the trap block above and spentry-A's namespace doc).  All other former
 * MISSING-CONSTANT holes are derived locally in the header block above
 * (symbol.fcell, _function.code_vector, t_offset, lisp_frame marker layout,
 * vectorH.logsize, XSTKOVER, XNOSPREAD, error_object_not_list). */

/* PORT-TODO items requiring design decisions or missing mechanisms:
 *
 * 1. fn-volatile protocol (HIGH PRIORITY): PPC64 fn is nonvolatile (callee-saved),
 *    but ARM64 fn=x7 is VOLATILE per upstream design. Every place PPC64 code depends
 *    on fn surviving a BL needs a protocol decision - either:
 *    a) Save/restore fn around calls (where?)
 *    b) Change calling convention to make fn nonvolatile (conflicts with AAPCS64?)
 *    c) Use a different register for fn in ARM64
 *    Affected subprims: jmpsym, funcall, mvpass, tfuncall*, tcall*
 *
 * 2. .SPbuiltin dispatch mechanism: RESOLVED -- jump_builtin macro defined
 *    locally (line ~122), fully real via ref_nrs_value
 *    (arm64-globals-proposed.s). All 12 prior PORT-TODO dispatcher sites
 *    replaced with jump_builtin invocations.
 *
 * 3. EGC write barrier: rplaca/rplacd have complex refbits/ephemeral_refidx
 *    manipulation that requires access to global state. Need to verify the
 *    mechanism in upstream ARM64.
 *
 * 4. keyword_args / call_closure: These have very complex stack manipulation
 *    that needs careful line-by-line porting with full understanding of the
 *    keyword binding protocol and closure layout.
 *
 * 5. Missing subprims referenced: _SPkeyword_bind, _SPmakes128, ret1val_addr,
 *    and various error handlers. These are defined elsewhere and need to be
 *    coordinated.
 *
 * 6. Numeric local labels: This file uses simple numeric labels (1:, 2:, etc.)
 *    following the style of his existing code. These are file-scoped in GNU as,
 *    which matches his style, but differs from our high-tag port's approach
 *    of using local_label() macros. His style is cleaner for short subprims.
 */

/* UNCERTAINTIES:
 *
 * - nargs arithmetic: PPC64 nargs is a TAGGED fixnum (confirmed in both ports).
 *   All nargs comparisons use (nargregs << fixnumshift) to convert untagged
 *   constant to tagged form. This is correct for fixnumshift=3.
 *
 * - register allocation in complex subprims: Some subprims use many temporaries
 *   and may exceed available ARM64 temp registers (temp0-4 = x13-x17, only 5).
 *   May need to spill to stack or use save registers with care.
 *
 * - Branch distance: Some of the dispatch-heavy subprims (keyword_args,
 *   builtin_length) have many forward/backward branches that may exceed
 *   ARM64's ±1MB branch range if separated. Should be fine within one file.
 */
/* SPDX-License-Identifier: Apache-2.0 */

/*
 * E-ffi cluster: 8 subprims ported from PPC64 to ARM64 low-tag
 *   ffcall, ffcall_return_registers, ffcallX (trap-only), callback,
 *   callbackX (trap-only), syscall, lexpr_entry, spread_lexprz
 *
 * Source: vendor/ccl/lisp-kernel/ppc-spentry.s (PPC64 poweropen_* branches:
 *   ffcall @1595, ffcall_return_registers @1796, callbackX @2068,
 *   ffcallX @3526, spread_lexprz @4883, callback @5031, lexpr_entry @5362,
 *   syscall @5402).  The eabi_* bodies (@6055/6158/6346) are PPC32 and were
 *   NOT used.  AAPCS64 mechanics (FPCR/FPSR split, d0-d7 arg staging,
 *   callee-saved x19-x28/d8-d15, svc #0) follow the boot-validated
 *   implementations in ../../lisp-kernel/arm64-spentry.s (eabi_ff_call,
 *   arm64_syscall, eabi_callback, lexpr_entry, spread_lexprz), re-mapped to
 *   Matt Emerson's register assignments and low-tag scheme.
 * Target: Matt Emerson's upstream ARM64 low-tag design (pin 115b7aa).
 *
 * Register map (arm64-constants.h @ 115b7aa, unified): imm0-5=x0-x5,
 * nargs=x6 (holds FIXNUMS), fn=x7 (VOLATILE), arg_w-arg_z=x8-x11,
 * temp0-temp5=x12-x17 (nfn=temp2, fname=temp3), save0-3=x19-x22,
 * rnil=x23, tsp=x24, vsp=x25, allocptr=x26, allocbase=x27, rcontext=x28.
 * fixnumshift=3, node_size=8.
 *
 * AAPCS64 facts this port leans on (vs PPC):
 *   - rcontext=x28, rnil=x23, tsp=x24, vsp=x25, allocptr/allocbase=x26/x27
 *     and save0-3=x19-x22 are ALL callee-saved: a conforming C callee
 *     preserves them, so PPC's "mr save0,rcontext / restore" dance and the
 *     nil-out of rnil-adjacent registers are unnecessary.  save0-3 are
 *     still vpushed so the GC can SEE them while the thread is foreign.
 *   - fn=x7 and nargs=x6 are AAPCS64 argument registers: clobbered by the
 *     C-arg loads, so fn is saved in the lisp_frame (savefn slot) exactly
 *     as on PPC.
 *   - loc_pc -> lr ([D3] convention, cf. spentry-D): AArch64 keeps the
 *     return pc in lr; frames save/restore lr.
 *   - PPC FPSCR (one register) -> FPCR (control) + FPSR (status).
 *
 * STATUS: all 8 subprims carry real ported bodies (ffcallX/callbackX are
 * trap-only ON PPC64 - `.long 0x7c800008` debug traps - and are ported as
 * brk traps).  Sites depending on constants/conventions Matt has not yet
 * defined carry #error + the intended instruction in a comment.
 */

/*
 * ===========================================================================
 * PROPOSED-CONSTANTS (ratify with Matt)
 * ---------------------------------------------------------------------------
 * NOT in arm64-constants.h.  Values DERIVED from the cited sources; the C
 * runtime and compiler must agree.
 * ===========================================================================
 */

/* Thread valence values: lisp-kernel/constants.h:27-28 (present verbatim in
   Matt's tree; a C header, so re-equated here for the assembler). */
.set TCR_STATE_LISP,    0
.set TCR_STATE_FOREIGN, 1

/* c_frame (the outgoing-argument staging frame built by the compiler's
   alloc-c-frame vinsn) is defined in arm64-constants.h since upstream
   patch 0003: {backlink, savelr, params...} -- the 8 GPR argument words
   at c_frame.params, stack (overflow) words immediately above.  The
   pre-w13 local definition that lived here (with a reserved transition
   lisp_frame above the frame) is retired. */

/*
 * ---------------------------------------------------------------------------
 * Local helper macros (same equates/idioms as spentry-C:176-273)
 * ---------------------------------------------------------------------------
 */

/* Spill/reload the boxed NVRs to/from the vstack so the GC can see them
   while the thread is in foreign code (ppc-macros.s vpush_saveregs, sized
   to this design's nsaveregs=4). */
.macro vpush_saveregs
        vpush1 save0
        vpush1 save1
        vpush1 save2
        vpush1 save3
.endm
.macro vpop_saveregs
        vpop1 save3
        vpop1 save2
        vpop1 save1
        vpop1 save0
.endm

/* ===========================================================================
 * THE LISP <-> FOREIGN CSTACK BOUNDARY PROTOCOL          (16m41; ARM64-DEVIATION)
 * ===========================================================================
 *
 * A thread's cs_area is its whole pthread stack (thread_manager.c:1550
 * register_cstack_holding_area_lock(stack_base, stack_size)), so it holds C
 * frames as well as lisp frames -- glibc's TLS block and the thread's C
 * startup frames at the top, the callee's frames below every ff-call.  The GC
 * walks [cs_area->active, cs_area->high) LINEARLY and asserts it lands exactly
 * on ->high (arm64-gc.c mark_cstack_area), because Matt's frames are MARKER
 * frames: word 0 is subtag_lisp_frame_marker, NOT a backlink.
 *
 * PPC64 needs nothing here and so is not the donor: its frames ARE backlinks,
 * the C ABI mandates one at word 0 of every frame, and ppc-gc.c:1022 is a
 * chain walk that strides over a C region for free (ppc-subprims.s:73
 * start_lisp does no bookkeeping at all).  AAPCS64 mandates no such chain --
 * x29 is optional and gcc -O2 omits it -- so on a marker walk the C regions
 * have to be described explicitly.  Matt's WIP has no start_lisp/ffcall to
 * adopt from.  The mechanism below is therefore the ARM-family one (marker
 * frames + linear walk is the same shape there), rendered for AAPCS64:
 *
 *   tcr.last_lisp_frame = the LOWEST lisp-owned cstack word while the thread
 *   is foreign; initialized to cs_area->high (= "no lisp frames yet") in
 *   thread_manager.c:1553.  The GC reads it in normalize_tcr's ff-call branch.
 *
 *   lisp -> foreign (ffcall): store the c_frame base there.  Its word 0 is
 *   the frame's own ivector header, so the walk starts on a self-describing
 *   region; the old value is parked in a dead param word (inside that
 *   ivector, hence unscanned, and above SP, hence untouchable by the callee).
 *
 *   foreign -> lisp (start_lisp, callback): the C region between the new lisp
 *   SP and the previous boundary cannot be walked, so COVER it with a
 *   synthetic u64-vector header that strides exactly onto the previous
 *   boundary.  16m41 observed the cost of not doing this: the walk climbed
 *   three real lisp frames, ran into the C region above them, guessed through
 *   an x29 hop and four zero words, then read a spilled 0.9d0
 *   (0x3feccccccccccccd) as an ivector header and strode 1.8e16 words.
 *
 * Arithmetic (mirrors the ARM-family idiom, re-derived for 64-bit):
 *   count = ((OLD - sp) + node_size) >> node_shift   u64 elements
 *   header = (count << num_subtag_bits) | subtag_u64_vector
 * with the pair {header, OLD} pushed at sp-16, skip_over_ivector
 * (arm64-gc.c:1033) returns (sp-16) + dnode_align(count*8 + 8) == OLD, since
 * OLD and sp are both 16-aligned (AAPCS64) so count*8 + 8 == OLD - (sp-16)
 * exactly.  A single `lsl #(num_subtag_bits - node_shift)' does the two
 * shifts because (OLD - sp) + 8 always has its low 3 bits clear.
 */
.macro cover_foreign_stack_region hdr, old
        ldr  \old, [rcontext, #tcr.last_lisp_frame]
        mov  \hdr, sp                   /* SP cannot be the Rm of a SUB */
        sub  \hdr, \old, \hdr           /* bytes of C region above sp */
        add  \hdr, \hdr, #node_size
        lsl  \hdr, \hdr, #(num_subtag_bits - node_shift)
        add  \hdr, \hdr, #subtag_u64_vector   /* low 8 bits clear: add == orr */
        stp  \hdr, \old, [sp, #-16]!
.endm

/* Pop the cover pair and restore the ENCLOSING boundary.  Restoring matters:
   without it a nested transition (lisp -> ffcall -> callback -> lisp ->
   ffcall -> return -> callback returns) would leave last_lisp_frame naming a
   region that has already been popped, and the next walk would start in dead
   stack. */
.macro uncover_foreign_stack_region scratch, old
        ldp  \scratch, \old, [sp], #16
        str  \old, [rcontext, #tcr.last_lisp_frame]
.endm

/*
 * ===========================================================================
 * FFI SUBPRIMS
 * ===========================================================================
 */

/* _SPffcall lives in arm64-spentry.s (upstream patch 0003): it was
 * re-ported there against the w13 aapcs64-ff-call codegen unit's
 * c_frame protocol ([backlink,savelr,params...]; entry point unboxed
 * from a macptr OR a fixnum-locative; no FPCR switching -- lisp runs
 * with the process-default FPCR; no FPSR access either -- the float
 * wrappers own the flag window, see `spentry ffcall').  The earlier
 * draft that lived here used the pre-w13 frame layout and the removed
 * tcr.lisp_mxcsr/ffi_exception slots. */

/* Just like ffcall, but saves all AAPCS64 result registers into a buffer
 * whose macptr is passed in arg_y, before returning to lisp (ppc:1796
 * poweropen_ffcall_return_registers; needed because several C result
 * registers are dedicated lisp registers).
 * PROPOSED buffer layout (RATIFY - lisp-side ff-call glue must match):
 *   [0..56]   x0..x7   (8 GPRs; PPC stores its 8 GPR args/results)
 *   [64..120] d0..d7   (8 FPRs; PPC stores f1-f13 - AAPCS64 result FPRs
 *                       are d0-d7, so 8 doubles here)
 */
/* Body = patch-0003 _SPffcall with the result-buffer stores inserted
 * at the return; save2 carries the buffer address across the call
 * (callee-saved), parked on the vstack like save3/fn. */
spentry ffcall_return_registers
        /* fn + all four boxed NVRs, exactly as `spentry ffcall'
         * (arm64-spentry.s -- the CANONICAL NOTE for this whole body): the
         * vstack copies are what the GC forwards while we are foreign; the
         * registers carry raw kernel state (and the result buffer) across
         * the call and are reloaded, relocations applied, after it. */
        str fn, [vsp, #-node_size]!             /* ppc:1799 vpush_saveregs   */
        str save3, [vsp, #-node_size]!
        str save2, [vsp, #-node_size]!
        str save1, [vsp, #-node_size]!
        str save0, [vsp, #-node_size]!
        mov save3, sp
        /* Park lr in the boundary lisp_frame his alloc-c-frame RESERVED at the
         * frame top, and publish it by shrinking the header count by 4.  The
         * c_frame has NO savelr and [sp,#8] is the saved SP; writing lr there
         * and restoring sp from the header word gave sp=0xded (16m30).
         * CANONICAL NOTE: `spentry ffcall' in arm64-spentry.s. */
        ldr imm0, [sp, #c_frame.header]
        lsr imm1, imm0, #num_subtag_bits        /* element count = words-1 */
        sub imm1, imm1, #3
        mov imm2, sp                            /* add-shifted with Rn=sp is */
        add imm2, imm2, imm1, lsl #node_shift   /* an encoding trap */
        mov imm1, #lisp_frame_marker
        str imm1, [imm2, #lisp_frame.marker]
        str vsp, [imm2, #lisp_frame.savevsp]
        /* Zeros while covered, publish, THEN the real fn/lr -- the
         * alloc-c-frame build contract; rationale at the canonical note. */
        stp xzr, xzr, [imm2, #lisp_frame.savefn]
        sub imm0, imm0, #(4 << num_subtag_bits)
        str imm0, [sp, #c_frame.header]
        stp fn, lr, [imm2, #lisp_frame.savefn]
        /* Cross-call hoist (canonical note): the frame head [sp, sp+80)
         * dies once SP steps over it at the blr. */
        mov save1, imm2                         /* boundary lisp_frame       */
        ldr save0, [rcontext, #tcr.last_lisp_frame] /* enclosing boundary    */
        /* Buffer address -> save2 (PPC uses save7, ppc:1800); its lisp
         * value is already in the 5-slot vstack spill above. */
        ldur save2, [arg_y, #macptr.address]
        /* Unbox the entry point into temp4 (ppc:1802-1814
           extract_typecode): macptr iff fulltag_misc AND header subtag
           == subtag_macptr; anything else the raw bits ARE the address.
           A bare `tst #tagmask` misclassifies 4-aligned C entry points
           (lisp_malloc = ...eac, fulltag 0xc) as macptrs (16m5l). */
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
        /* Publish lisp state to the TCR for the GC, then go foreign
           (ppc:1816-1827). */
        str vsp, [rcontext, #tcr.save_vsp]
        str tsp, [rcontext, #tcr.save_tsp]
        str allocptr, [rcontext, #tcr.save_allocptr]
        str allocbase, [rcontext, #tcr.save_allocbase]
        /* Load the outgoing GPR args (ppc:1836-1843); the SP step that
           puts the NSAA stack args at [SP] comes after the valence flip,
           per the ordering note in `spentry ffcall' (arm64-spentry.s). */
        ldp x0, x1, [sp, #c_frame.params]
        ldp x2, x3, [sp, #(c_frame.params + 2*node_size)]
        ldp x4, x5, [sp, #(c_frame.params + 4*node_size)]
        ldp x6, x7, [sp, #(c_frame.params + 6*node_size)]
        /* Boundary bookkeeping + valence + SP step, identical to `spentry
         * ffcall' in arm64-spentry.s (the ordering rationale lives there):
         * boundary = the published lisp_frame, stored before the valence
         * flip; after the flip SP steps over the doomed frame head, putting
         * the NSAA stack args at [SP] for the callee.
         * temp0, not imm0: imm0 is x0, now an outgoing argument. */
        str save1, [rcontext, #tcr.last_lisp_frame]
        mov temp0, #TCR_STATE_FOREIGN
        str temp0, [rcontext, #tcr.valence]
        add sp, sp, #(c_frame.size + 8*node_size) /* ARM64-DEVIATION: NSAA
                          stack args at [SP]; canonical note in ffcall */
        blr temp4                               /* ppc:1849 bctrl            */
        /* Store every AAPCS64 result register into the buffer
           (ppc:1851-1871 stores r3-r10/f1-f13). */
        stp x0, x1, [save2, #(0*node_size)]
        stp x2, x3, [save2, #(2*node_size)]
        stp x4, x5, [save2, #(4*node_size)]
        stp x6, x7, [save2, #(6*node_size)]
        stp d0, d1, [save2, #((8*node_size) + 0*8)]
        stp d2, d3, [save2, #((8*node_size) + 2*8)]
        stp d4, d5, [save2, #((8*node_size) + 4*8)]
        stp d6, d7, [save2, #((8*node_size) + 6*8)]
        /* ---- common return path (= `spentry ffcall', arm64-spentry.s;
         * the per-boundary GC audit lives there).  The frame head
         * [save3, save3+80) is DEAD; run entirely from the hoist.
         * No FPSR access -- the float wrappers own the flag window. ---- */
        mov sp, save1                       /* retreat onto the boundary frame */
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
        mov fn, rnil
        mov save1, xzr
        mov save2, xzr
        mov save3, xzr
        mov allocptr, #-dnode_size          // VOID_ALLOCPTR (ppc idiom)
        mov allocbase, #-dnode_size
        str xzr, [rcontext, #tcr.valence]   // TCR_STATE_LISP
        str save0, [rcontext, #tcr.last_lisp_frame] /* enclosing boundary back */
        ldr lr, [sp, #lisp_frame.savelr]    /* post-flip: GC-forwarded */
        ldr save0, [vsp], #node_size
        ldr save1, [vsp], #node_size
        ldr save2, [vsp], #node_size
        ldr save3, [vsp], #node_size
        ldr fn, [vsp], #node_size
        ldr allocptr, [rcontext, #tcr.save_allocptr]
        ldr allocbase, [rcontext, #tcr.save_allocbase]
        add sp, sp, #lisp_frame.size        /* drop the frame: sp = prev SP */
        check_pending_interrupt
        ret
endsp ffcall_return_registers

/* Just like ffcall, but the record-by-value result is >16 bytes and not
 * an HFA, so AAPCS64 (6.9) wants the caller-allocated result buffer's
 * address in x8 (the indirect result-area register) at the call.  The
 * buffer's MACPTR rides in arg_y (exactly as ffcall_return_registers'
 * regbuf does); its address is loaded into x8 alongside the x0-x7
 * argument loads.  x8 needs no channel through the c_frame: the plain
 * ffcall body never touches arg_w=x8 between entry and the blr, and the
 * common return path re-nils it.  No PPC analog: PowerOpen (and SysV)
 * pass the hidden result pointer as the FIRST integer argument; AAPCS64
 * alone dedicates x8. */
spentry ffcall_indirect_result
        /* fn + all four boxed NVRs -- byte-identical to `spentry ffcall'
         * (arm64-spentry.s), whose comments are the canonical note. */
        str fn, [vsp, #-node_size]!
        str save3, [vsp, #-node_size]!
        str save2, [vsp, #-node_size]!
        str save1, [vsp, #-node_size]!
        str save0, [vsp, #-node_size]!
        mov save3, sp
        /* Boundary lisp_frame build (zeros while covered, publish, then
         * the real fn/lr) + cross-call hoist -- byte-identical to `spentry
         * ffcall' (arm64-spentry.s), whose comments are the canonical
         * note. */
        ldr imm0, [sp, #c_frame.header]
        lsr imm1, imm0, #num_subtag_bits        /* element count = words-1 */
        sub imm1, imm1, #3
        mov imm2, sp                            /* add-shifted with Rn=sp is */
        add imm2, imm2, imm1, lsl #node_shift   /* an encoding trap */
        mov imm1, #lisp_frame_marker
        str imm1, [imm2, #lisp_frame.marker]
        str vsp, [imm2, #lisp_frame.savevsp]
        stp xzr, xzr, [imm2, #lisp_frame.savefn]
        sub imm0, imm0, #(4 << num_subtag_bits)
        str imm0, [sp, #c_frame.header]
        stp fn, lr, [imm2, #lisp_frame.savefn]
        mov save1, imm2                         /* boundary lisp_frame       */
        ldr save0, [rcontext, #tcr.last_lisp_frame] /* enclosing boundary    */
        /* Unbox the entry point into temp4 (canonical note: `spentry
           ffcall', arm64-spentry.s; the bare-tst trap is 16m5l). */
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
        /* Load the outgoing GPR args; the SP step that exposes the NSAA
         * stack args at [SP] comes after the valence flip, per the
         * ordering note in `spentry ffcall' (arm64-spentry.s). */
        ldp x0, x1, [sp, #c_frame.params]
        ldp x2, x3, [sp, #(c_frame.params + 2*node_size)]
        ldp x4, x5, [sp, #(c_frame.params + 4*node_size)]
        ldp x6, x7, [sp, #(c_frame.params + 6*node_size)]
        /* THE VARIANT'S ONE ADDITION: indirect result-area pointer.
         * arg_y is still live (nothing above touches it) and x8 is dead
         * from here to the blr; the callee writes the record through x8
         * and returns void. */
        ldur x8, [arg_y, #macptr.address]
        /* Boundary bookkeeping + valence + SP step, identical to `spentry
         * ffcall' (arm64-spentry.s) -- the ordering rationale lives there.
         * temp0, not imm0: imm0 is x0, now an outgoing argument. */
        str save1, [rcontext, #tcr.last_lisp_frame]
        mov temp0, #TCR_STATE_FOREIGN
        str temp0, [rcontext, #tcr.valence]
        add sp, sp, #(c_frame.size + 8*node_size) /* ARM64-DEVIATION: NSAA
                          stack args at [SP]; canonical note in ffcall */
        blr temp4
        /* ---- common return path (= `spentry ffcall', arm64-spentry.s;
         * the per-boundary GC audit lives there).  The frame head
         * [save3, save3+80) is DEAD; run entirely from the hoist.
         * No FPSR access -- the float wrappers own the flag window. ---- */
        mov sp, save1                       /* retreat onto the boundary frame */
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
        mov fn, rnil
        mov save1, xzr
        mov save2, xzr
        mov save3, xzr
        mov allocptr, #-dnode_size          // VOID_ALLOCPTR (ppc idiom)
        mov allocbase, #-dnode_size
        str xzr, [rcontext, #tcr.valence]   // TCR_STATE_LISP
        str save0, [rcontext, #tcr.last_lisp_frame] /* enclosing boundary back */
        ldr lr, [sp, #lisp_frame.savelr]    /* post-flip: GC-forwarded */
        ldr save0, [vsp], #node_size
        ldr save1, [vsp], #node_size
        ldr save2, [vsp], #node_size
        ldr save3, [vsp], #node_size
        ldr fn, [vsp], #node_size
        ldr allocptr, [rcontext, #tcr.save_allocptr]
        ldr allocbase, [rcontext, #tcr.save_allocbase]
        add sp, sp, #lisp_frame.size        /* drop the frame: sp = prev SP */
        check_pending_interrupt
        ret
endsp ffcall_indirect_result

/* Deprecated "swap exception handling info" variant.  TRAP-ONLY ON PPC64
 * TOO: ppc-spentry.s:3526-3527 is just `.long 0x7c800008` (debug trap). */
spentry ffcallX
        brk #0                                  /* ppc:3527 debug trap       */
endsp ffcallX

/* Darwin-JNI exception-port variant.  TRAP-ONLY ON PPC64 TOO:
 * ppc-spentry.s:2068-2069 is just `.long 0x7c800008` (debug trap). */
spentry callbackX
        brk #0                                  /* ppc:2069 debug trap       */
endsp callbackX

/* C-to-lisp callback (ppc:5031 poweropen_callback).  Restore lisp context,
 * then funcall %pascal-functions% with two args: callback-index and a
 * "frame pointer" fixnum from which the lisp glue reads the C arguments
 * and into which it writes the C result.
 *
 * PROPOSED-CONVENTION CALLBACK-IDX (RATIFY): the make-callback trampoline
 * stub enters here with the UNBOXED callback index in arg_y=x10 (PPC uses
 * r11; any AAPCS64 caller-saved scratch works - the trampoline generator,
 * level-1/arm64-callback-support.lisp, is a lisp-side deliverable that
 * must match).  x8 is NOT usable for the index: AAPCS64 6.9 delivers the
 * indirect result-area pointer there when the callback returns a >16-byte
 * non-HFA record, so the incoming x8 is live argument material -- it is
 * captured below (via save2, which survives the get_tcr C call) into the
 * padding word of the foreign-sp stash, at CBF-248
 * (arm64-arch.lisp callback-frame.x8-save-offset), where the lisp glue
 * reads it as the struct-return pointer.  (The index lived in x8 until
 * 16m71 and silently clobbered that pointer.)
 *
 * PROPOSED frame contract (RATIFY - lisp-side callback glue must match;
 * boot-validated shape from our v2 tree): x0..x7 are pushed so the x0 slot
 * abuts the incoming sp; CBF = &x0save.  The C caller's stack args then
 * sit contiguously at CBF+64 (the PowerOpen single-linear-offset property,
 * reproduced).  d0..d7 saves at CBF-64..-8; incoming x8 at CBF-248.  The
 * GPR result is reloaded from CBF+0/+8, the FPR result from CBF-64..-40
 * (d0-d3: an HFA return of up to 4 members, AAPCS64 6.9; a scalar FP
 * result only populates d0's slot).  CBF is 16-aligned, so it is its own
 * fixnum boxing.
 *
 * ARM64-DEVIATION (vs PPC64 poweropen_callback):
 *   - callee-saved set = x19-x28 + fp/lr and d8-d15 (+FPCR/FPSR pair), NOT
 *     PPC's r13-r31/f-block; lisp may clobber d8-d15, so they are saved
 *     here (RATIFY: alternatively restrict Matt's compiler FPR pool).
 *   - get_tcr is reached by a direct `bl` (kernel-internal symbol;
 *     PPC indirects through a lisp_global for TOC reasons, ppc:5115).
 *   - save0-3 enter lisp as 0 (valid fixnums; our v2-validated choice)
 *     rather than PPC's restore_saveregs-from-vstack (ppc:5149).  The
 *     outer ffcall reloads its own save0-3 from its vstack spill, so
 *     values are never lost.  RATIFY if Matt wants PPC's reload.
 */
spentry callback
        /* Save the C argument registers so the lisp glue can read them
           (ppc:5036-5043 stores r3-r10 into the caller frame; AAPCS64 has
           no reserved param area, so push them - x0 slot lands at CBF). */
        stp x6, x7, [sp, #-16]!
        stp x4, x5, [sp, #-16]!
        stp x2, x3, [sp, #-16]!
        stp x0, x1, [sp, #-16]!
        mov arg_x, sp                           /* arg_x=x10: CBF            */
        stp d6, d7, [sp, #-16]!
        stp d4, d5, [sp, #-16]!
        stp d2, d3, [sp, #-16]!
        stp d0, d1, [sp, #-16]!                 /* d0 save @ CBF-64          */
        /* Save the AAPCS64 callee-saved GPRs (ppc:5052-5071 saves r13-r31). */
        stp x19, x20, [sp, #-16]!
        stp x21, x22, [sp, #-16]!
        stp x23, x24, [sp, #-16]!
        stp x25, x26, [sp, #-16]!
        stp x27, x28, [sp, #-16]!
        stp x29, lr,  [sp, #-16]!
        /* Save the callee-saved FPRs + the foreign FPCR/FPSR
           (ppc:5072-5096 saves f1-f13 + FPSCR). */
        stp d8,  d9,  [sp, #-16]!
        stp d10, d11, [sp, #-16]!
        stp d12, d13, [sp, #-16]!
        stp d14, d15, [sp, #-16]!
        mrs imm0, fpcr
        mrs imm1, fpsr
        stp imm0, imm1, [sp, #-16]!
        /* Stash index + CBF + incoming x8 in just-saved callee-saved
           regs: they must survive the get_tcr C call (x9-x17 are
           caller-saved, and a linker veneer may clobber x16/x17).  x8 is
           the AAPCS64 indirect result-area pointer when the callback
           returns a >16-byte non-HFA record -- garbage otherwise, and
           harmless to carry. */
        mov save0, arg_y                        /* callback index (x10)  */
        mov save1, arg_x                        /* CBF                   */
        mov save2, arg_w                        /* incoming x8           */
        /* Recover the thread context (ppc:5114-5124 get_tcr(1)). */
        mov x0, #1
        bl get_tcr
        mov rcontext, x0
        /* Stash the exact foreign sp for the return path, pairing it
           with the incoming x8 (slot CBF-248 =
           callback-frame.x8-save-offset; was an xzr padding word). */
        mov imm0, sp
        stp imm0, save2, [sp, #-16]!
        /* Restore lisp context (ppc:5127-5147). */
        ldr vsp, [rcontext, #tcr.save_vsp]
        ldr tsp, [rcontext, #tcr.save_tsp]
        /* FP state (ppc:5141-5142 restores the lisp FPSCR): no FPCR
           switch in Matt's TCR design (f067047 removed lisp_mxcsr; lisp
           runs with the process-default FPCR) -- just clear the sticky
           exception flags before entering lisp code. */
        msr fpsr, xzr
        /* Marshal the lisp args from the stash BEFORE zeroing (ppc:5109-
           5112): boxed index; CBF is 16-aligned = its own fixnum. */
        lsl arg_y, save0, #fixnumshift
        mov arg_z, save1
        /* Zero the remaining node registers (ppc:5102-5108,5133-5140; 0 is
           fixnum 0, GC-valid).  fn=0: subprim, not a lisp function. */
        mov fn, #0
        mov arg_w, #0
        mov arg_x, #0
        mov temp0, #0
        mov temp1, #0
        mov temp2, #0
        mov temp3, #0
        mov temp4, #0
        mov save0, #0
        mov save1, #0
        mov save2, #0
        mov save3, #0
        /* Re-materialize rnil (ppc:5130 restores NIL as part of
           restore_lisp_context).  We entered from FOREIGN state: a conforming
           C callee preserves x23, but the caller that reaches .SPcallback
           (callback_to_lisp's foreign trampoline) is under no such contract,
           so rnil=x23 arrives clobbered (observed 16m20: x23=0x2, garbage ->
           the rnil-relative nrs.callbacks ref below SIGSEGV'd).  Every other
           spentry assumes rnil is live; the callback is the sole re-entry that
           must reload it.  Use the SAME idiom as start_lisp below: nil_value
           is patched into the C global lisp_nil at initial heap mapping (Matt
           2026-07-11), so it is NOT a compile-time immediate. */
        ldr rnil, =lisp_nil
        ldr rnil, [rnil]
        /* Cover the foreign region below the enclosing lisp boundary -- the C
           caller's frames plus every register block this spentry just pushed --
           so the GC's linear cstack walk strides from here straight to
           tcr.last_lisp_frame (the enclosing ff-call's c_frame, or
           cs_area->high for a C thread that has never run lisp).  Pushed LAST,
           so the header sits at the lowest address of what it describes; the
           exit path accounts for the extra 16 bytes.  BEFORE the valence store,
           so this thread never advertises lisp valence with a stretch of
           unwalkable stack under it.  ARM64-DEVIATION: PPC's chain walk needs
           no cover (protocol note near the top of this file). */
        cover_foreign_stack_region imm0, imm1
        mov imm0, #TCR_STATE_LISP               /* ppc:5130/5145             */
        str imm0, [rcontext, #tcr.valence]
        ldr allocptr,  [rcontext, #tcr.save_allocptr]   /* ppc:5146-5147     */
        ldr allocbase, [rcontext, #tcr.save_allocbase]
        /* Call (%pascal-functions% index CBF) (ppc:5151-5158). */
        set_nargs 2
        ref_nrs_symbol fname, callbacks         /* ppc:5157 li fname,nrs.callbacks */
        ldr nfn, [fname, #symbol.fcell]
        ldr temp4, [nfn, #_function.code_vector]
        blr temp4
        /* Lisp wrote the result into CBF+0/+8 / CBF-64..-40 (glue
           contract; a >16-byte non-HFA record went through the pointer
           captured at CBF-248 instead).  CBF is recomputed below from
           the restored sp (fixed layout); first publish lisp state back
           to the tcr (ppc:5159-5169). */
        str allocptr,  [rcontext, #tcr.save_allocptr]   /* ppc:5166-5169     */
        str allocbase, [rcontext, #tcr.save_allocbase]
        str vsp,       [rcontext, #tcr.save_vsp]
        str tsp,       [rcontext, #tcr.save_tsp]
        /* Exit lisp context (ppc:5171-5172). */
        mov imm0, #TCR_STATE_FOREIGN
        str imm0, [rcontext, #tcr.valence]
        /* Drop the C-region cover FIRST (it was pushed last) and restore the
           enclosing boundary, then unwind to the foreign sp stash that now
           sits 16 bytes above SP. */
        uncover_foreign_stack_region imm0, imm1
        /* Unwind to the exact foreign sp stash. */
        ldr imm0, [sp]
        mov sp, imm0
        /* Restore foreign FPCR/FPSR + callee-saved FPRs (ppc:5174-5178). */
        ldp imm0, imm1, [sp], #16
        msr fpcr, imm0
        msr fpsr, imm1
        ldp d14, d15, [sp], #16
        ldp d12, d13, [sp], #16
        ldp d10, d11, [sp], #16
        ldp d8,  d9,  [sp], #16
        /* Reload the C result BEFORE x19/x20 are restored: sp now points at
           the callee-saved GPR block; CBF = sp + 96 + 64. */
        add imm2, sp, #(6*16 + 4*16)            /* imm2 = CBF                */
        ldr x0, [imm2]                          /* GPR result (ppc:5213-5214)*/
        ldr x1, [imm2, #8]
        /* FPR result: d0-d3 from the fp save slots -- an HFA return
           occupies up to four V registers, one member each (AAPCS64
           6.9); a scalar FP result only means d0, and for any non-FP
           return all four are dead scratch the C caller ignores. */
        ldp d0, d1, [imm2, #-64]
        ldp d2, d3, [imm2, #-48]
        /* Restore callee-saved GPRs (ppc:5179-5197) and pop the arg-save
           areas (ppc:5212); x0/x1/d0 carry the result (ppc:5225 blr). */
        ldp x29, lr,  [sp], #16
        ldp x27, x28, [sp], #16
        ldp x25, x26, [sp], #16
        ldp x23, x24, [sp], #16
        ldp x21, x22, [sp], #16
        ldp x19, x20, [sp], #16
        add sp, sp, #(16*8)                     /* drop d0-d7 + x0-x7 saves  */
        ret
endsp callback

/* Do a LINUX system call (ppc:5402 poweropen_syscall; the Darwin
 * carry-flag/return-twice protocol is NOT ported).  Same c_frame contract
 * and lisp<->foreign transition as ffcall; the middle is the AArch64
 * Linux syscall sequence instead of a call:
 *   x8 = syscall number (unboxed from arg_z), x0-x5 = args, `svc #0'
 * (analog of x86-spentry64.s:4619 syscall; AArch64 Linux takes <=6
 * integer args).
 * ARM64-DEVIATION: Linux/AArch64 returns -errno directly in x0, so PPC's
 * error-path negation (ppc:5441-5454) has no analog - imm0 carries the
 * raw result.  No FP args => the FPCR dance is skipped (as on PPC, which
 * doesn't touch the FPSCR in syscall). */
#ifdef DARWIN
#error "Darwin syscall convention not ported (svc #0x80 + carry-flag error protocol)"
#endif
/* Body = patch-0003 _SPffcall shape (same c_frame contract and
 * lisp<->foreign transition) with the AArch64 Linux syscall sequence
 * in the middle instead of a call. */
spentry syscall
        /* fn + all four boxed NVRs, exactly as `spentry ffcall'
         * (arm64-spentry.s -- the CANONICAL NOTE for this whole body), and for
         * the same reason: the vstack copies are what the GC forwards while
         * this thread is foreign.  The kernel preserves x19-x22 across the
         * trap, but preservation is not FORWARDING -- a lisp value that
         * survives only in an NVR misses any relocation a foreign-era GC
         * applied.  PPC64 vpush_saveregs()es all eight of its save regs here
         * (ppc:5404) for exactly this; two of five was an arm64 shortfall. */
        str fn, [vsp, #-node_size]!             /* ppc:5404 vpush_saveregs   */
        str save3, [vsp, #-node_size]!
        str save2, [vsp, #-node_size]!
        str save1, [vsp, #-node_size]!
        str save0, [vsp, #-node_size]!
        mov save3, sp
        /* Park lr in the boundary lisp_frame his alloc-c-frame RESERVED at the
         * frame top, and publish it by shrinking the header count by 4.  The
         * c_frame has NO savelr and [sp,#8] is the saved SP; writing lr there
         * and restoring sp from the header word gave sp=0xded (16m30).
         * CANONICAL NOTE: `spentry ffcall' in arm64-spentry.s. */
        ldr imm0, [sp, #c_frame.header]
        lsr imm1, imm0, #num_subtag_bits        /* element count = words-1 */
        sub imm1, imm1, #3
        mov imm2, sp                            /* add-shifted with Rn=sp is */
        add imm2, imm2, imm1, lsl #node_shift   /* an encoding trap */
        mov imm1, #lisp_frame_marker
        str imm1, [imm2, #lisp_frame.marker]
        str vsp, [imm2, #lisp_frame.savevsp]
        /* Zeros while covered, publish, THEN the real fn/lr (the
         * alloc-c-frame build contract; rationale at the canonical note in
         * `spentry ffcall', arm64-spentry.s): a covered slot is invisible
         * to the GC, so a real fn/lr stored pre-publish goes stale if a GC
         * moves the caller in the window -- and this path reads savelr
         * back after the svc. */
        stp xzr, xzr, [imm2, #lisp_frame.savefn]
        sub imm0, imm0, #(4 << num_subtag_bits)
        str imm0, [sp, #c_frame.header]
        stp fn, lr, [imm2, #lisp_frame.savefn]
        /* Cross-trap hoist (canonical note: `spentry ffcall'): everything the
         * return path needs, in callee-saved registers, so that path never
         * reads the c_frame head -- and never reads BELOW SP, which is what
         * the old return path did. */
        mov save1, imm2                         /* boundary lisp_frame       */
        ldr save0, [rcontext, #tcr.last_lisp_frame] /* enclosing boundary    */
        /* Publish lisp state to the TCR for the GC, then go foreign
           (ppc:5405-5422). */
        str vsp, [rcontext, #tcr.save_vsp]
        str tsp, [rcontext, #tcr.save_tsp]
        str allocptr, [rcontext, #tcr.save_allocptr]
        str allocbase, [rcontext, #tcr.save_allocbase]
        /* (No tcr.foreign_fpsr store: the slot is no longer consumed --
           the float wrappers read the live FPSR; see `spentry ffcall'.) */
        /* Syscall number + up to 6 args (ppc:5424-5432 loads r3-r10 + r0). */
        asr x8, arg_z, #fixnumshift             /* ppc:5432 unbox_fixnum     */
        ldp x0, x1, [sp, #c_frame.params]
        ldp x2, x3, [sp, #(c_frame.params + 2*node_size)]
        ldp x4, x5, [sp, #(c_frame.params + 4*node_size)]
        /* Boundary bookkeeping + valence, identical to `spentry ffcall' in
         * arm64-spentry.s (the ordering rationale lives there): the boundary
         * is the PUBLISHED boundary lisp_frame, stored BEFORE the valence
         * flip so a GC that suspends us foreign finds it.
         *
         * ARM64-DEVIATION vs the three ff-call siblings: there is NO SP step
         * here.  They step SP over the frame head so the callee sees its NSAA
         * stack arguments at [SP] (AAPCS64 5.4.2); a Linux/AArch64 syscall has
         * no stack arguments at all -- x8 is the number and x0-x5 are the <=6
         * integer arguments -- so the step would expose nothing.  Leaving SP
         * at the frame head keeps the whole c_frame ABOVE SP for the duration
         * of the trap, and removes every below-SP read the old return path
         * relied on.  It also drops an unstated assumption: the old code took
         * `sp + c_frame.size + 8*node_size' to BE the boundary lisp_frame,
         * which only holds while the frame has exactly 8 param words; save1
         * is the address the header count actually strides to. */
        str save1, [rcontext, #tcr.last_lisp_frame]
        mov temp0, #TCR_STATE_FOREIGN
        str temp0, [rcontext, #tcr.valence]
        svc #0                                  /* ppc:5433 sc               */
        /* ---- return path (x0 = raw result / -errno) (ppc:5455-5489) ----
         * Order is PPC64's, instruction for instruction: make every node
         * register GC-valid, flip to lisp valence, and only THEN reload
         * anything a foreign-era GC may have moved.  ppc:5478-5487 flips
         * tcr.valence and then does vpop_saveregs / ldr savelr / ldr savefn /
         * discard_lisp_frame; x86-spentry64.s:4619 syscall stores
         * TCR_STATE_LISP and only then pops fn and the rest.  Both ports do
         * the reloads AFTER the flip because the GC forwards the vstack slot
         * and the TCR slot but never the register: popping while still
         * FOREIGN makes a stale node pointer live at the flip, and `fn' --
         * a function pointer -- is the worst of them.
         * No scratch register is used at all, so x0 (the result) and imm1-5
         * are untouched on this path. */
        mov sp, save1                       /* onto the boundary lisp_frame  */
        mov arg_w, rnil                         /* ppc:5461-5468             */
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
        mov fn, rnil
        mov save1, xzr
        mov save2, xzr
        mov save3, xzr
        mov allocptr,  #-dnode_size         // VOID_ALLOCPTR (ppc idiom)
        mov allocbase, #-dnode_size
        str xzr, [rcontext, #tcr.valence]       /* TCR_STATE_LISP; ppc:5481  */
        /* save0 is a raw cstack address, so it reads as a fixnum and is
         * GC-valid across the flip without being nil'd. */
        str save0, [rcontext, #tcr.last_lisp_frame] /* enclosing boundary back */
        ldr lr, [sp, #lisp_frame.savelr]    /* post-flip: GC-forwarded       */
        ldr save0, [vsp], #node_size
        ldr save1, [vsp], #node_size
        ldr save2, [vsp], #node_size
        ldr save3, [vsp], #node_size
        ldr fn,    [vsp], #node_size
        ldr allocptr,  [rcontext, #tcr.save_allocptr]   /* ppc:5470-5472     */
        ldr allocbase, [rcontext, #tcr.save_allocbase]
        add sp, sp, #lisp_frame.size        /* drop the frame: sp = prev SP  */
        check_pending_interrupt                 /* ppc:5488                  */
        ret                                     /* ppc:5489 blr              */
endsp syscall

/*
 * ===========================================================================
 * LEXPR SUBPRIMS
 * ===========================================================================
 */

/* lexpr_entry (ppc:5362, PPC64 branch).  Nargs is valid; all arg regs +
 * lexpr-count already vpushed by the caller's prologue; imm0 = the vsp to
 * restore (entry-vsp).  Return all values the caller returns to ITS
 * caller, hiding the variable-length arglist; if the caller's caller
 * expects one value, take the simpler path.
 *
 * PROPOSED-CONVENTION LEXPR-RA (RATIFY): PPC compares/keeps the CALLER's
 * return pc in loc_pc, while lr holds the return-to-prologue from the
 * `bla' - two live return addresses.  Matt's map has no loc_pc (x24=tsp),
 * so the lexpr function's prologue must pass the caller's return pc in
 * temp4=x16 before `bl _SPlexpr_entry`; lr = return-to-prologue.  On the
 * multiple-value path temp4 comes back = ret1val_addr for the prologue's
 * save-lisp-context-lexpr to store; on the single-value path temp4 =
 * lexpr_return1v (both as on PPC, which returns them in loc_pc). */
spentry lexpr_entry
        ref_global imm1, ret1val_addr           /* ppc:5363 (idiom: arm64-globals-proposed.s) */
        cmp imm1, temp4                         /* ppc:5364 cmpr w/ loc_pc   */
        /* FRAME-A (ppc:5365 build_lisp_frame(fn,loc_pc,imm0)): marker
           frame; savevsp=entry-vsp, savelr=caller return pc. */
        sub sp, sp, #lisp_frame.size
        mov imm2, #lisp_frame_marker
        str imm2,  [sp, #lisp_frame.marker]
        str imm0,  [sp, #lisp_frame.savevsp]
        str fn,    [sp, #lisp_frame.savefn]
        str temp4, [sp, #lisp_frame.savelr]
        b.ne 1f                                 /* ppc:5366                  */
        /* Multiple-value case (caller's caller expects MVs).  FRAME-B
           (ppc:5367-5368 build_lisp_frame(rzero,lexpr_return,vsp)):
           savevsp=vsp (the count cell), savefn=0 (frame owns no fn),
           savelr=lexpr_return. */
        ref_global imm2, lexpr_return           /* ppc:5367 */
        sub sp, sp, #lisp_frame.size
        mov imm3, #lisp_frame_marker
        str imm3, [sp, #lisp_frame.marker]
        str vsp,  [sp, #lisp_frame.savevsp]
        str xzr,  [sp, #lisp_frame.savefn]
        str imm2, [sp, #lisp_frame.savelr]
        mov temp4, imm1                         /* ppc:5369 loc_pc=ret1val   */
        /* Control-stack limit check (ppc:5370-5371 trllt(sp,cs_limit)). */
        ldr imm0, [rcontext, #tcr.cs_limit]
        cmp sp, imm0
        b.hi 2f
        uuo_error_cstack_overflow
2:      mov fn, #0                              /* ppc:5372                  */
        ret                                     /* ppc:5373 blr (to prologue)*/
        /* Single-value case: return to something that pops the variable-
           length frame off the vstack (ppc:5377-5382). */
1:
        ref_global temp4, lexpr_return1v        /* ppc:5378 */
        ldr imm0, [rcontext, #tcr.cs_limit]     /* ppc:5379-5380             */
        cmp sp, imm0
        b.hi 3f
        uuo_error_cstack_overflow
3:      mov fn, #0                              /* ppc:5381                  */
        ret                                     /* ppc:5382 blr              */
endsp lexpr_entry

/* "Spread" the lexpr in arg_z (ppc:4883, PPC64 branch).  arg_z = a fixnum
 * pointing at the lexpr block {count, argN-1 ... arg0}; the boxed count
 * (fixnumshift=3) IS the byte length of the arg block, exactly as on
 * PPC64 - so PPC's `add imm1,arg_z,imm0' ports verbatim.
 * ppc2-invoke-fn assumes temp1 is preserved here.
 * NOTE (label discipline): `9:' is deliberately defined ONCE per branch
 * region below - GNU as numeric labels are file-scoped and a dangling
 * `9f' binds to the NEXT `9:' anywhere in the file (the exact bug our
 * v2 tree hit in this very subprim, s92 cont-56). */
spentry spread_lexprz
        ldr imm0, [arg_z, #0]                   /* ppc:4884 lexpr count      */
        add imm1, arg_z, imm0                   /* ppc:4887                  */
        add nargs, nargs, imm0                  /* ppc:4889                  */
        add imm1, imm1, #node_size              /* ppc:4892 la node_size     */
        cmp imm0, #(3<<fixnumshift)             /* ppc:4885/4893             */
        b.ge 9f
        cmp imm0, #(2<<fixnumshift)             /* ppc:4886/4894             */
        b.eq 2f
        cmp imm0, #0                            /* ppc:4888/4895             */
        b.ne 1f
        /* lexpr count was 0: vpop the arg regs the caller vpushed
           (ppc:4896-4904; ldr does not disturb NZCV, so one cmp per
           condition suffices). */
        cmp nargs, #0                           /* ppc:4890 cr1              */
        b.eq 4f
        vpop1 arg_z
        cmp nargs, #(2<<fixnumshift)            /* ppc:4891 cr2              */
        b.lt 4f
        vpop1 arg_y
        b.eq 4f
        vpop1 arg_x
4:      ret

        /* vpush args from the lexpr until only three remain, then assign
           them to arg_x/arg_y/arg_z (ppc:4909-4919). */
8:      cmp imm0, #(4<<fixnumshift)             /* ppc:4910 cr3              */
        sub imm0, imm0, #fixnumone              /* ppc:4911                  */
        ldr arg_z, [imm1, #-node_size]!         /* ppc:4912 ldru             */
        vpush1 arg_z                            /* ppc:4913                  */
9:      b.ne 8b                                 /* ppc:4915                  */
        ldr arg_x, [imm1, #-(node_size*1)]      /* ppc:4916                  */
        ldr arg_y, [imm1, #-(node_size*2)]      /* ppc:4917                  */
        ldr arg_z, [imm1, #-(node_size*3)]      /* ppc:4918                  */
        ret

        /* count 2: set arg_y/arg_z from the lexpr, maybe vpop arg_x
           (ppc:4923-4928). */
2:      cmp nargs, #(2<<fixnumshift)
        ldr arg_y, [imm1, #-(node_size*1)]
        ldr arg_z, [imm1, #-(node_size*2)]
        b.eq 5f                                 /* ppc:4926 beqlr cr2        */
        vpop1 arg_x
5:      ret

        /* count 1: set arg_z from the lexpr, maybe vpop arg_y/arg_x
           (ppc:4932-4938). */
1:      cmp nargs, #(2<<fixnumshift)
        ldr arg_z, [imm1, #-node_size]
        b.lt 6f                                 /* ppc:4934 bltlr cr2        */
        vpop1 arg_y
        b.eq 6f                                 /* ppc:4936 beqlr cr2        */
        vpop1 arg_x
6:      ret
endsp spread_lexprz

/*
 * ===========================================================================
 * OPEN #error SITES (dedupe into upstream-port/MISSING-CONSTANTS-RATIFY.md)
 * ---------------------------------------------------------------------------
 *  - ARM64 lisp_globals ref idiom: ret1val_addr, lexpr_return,
 *    lexpr_return1v (lexpr_entry x3)
 *  - (RESOLVED 16m20) nil_value materialization from foreign state
 *    (callback): reload rnil from the C global lisp_nil, as start_lisp does.
 *  - stack-overflow (trllt) trap convention (lexpr_entry x2; shared with
 *    spentry-C:1646)
 *  - Darwin syscall protocol (guarded, Linux-only build unaffected)
 * ===========================================================================
 */

/* ===========================================================================
 * start_lisp: the C -> Lisp world entry (called by pmcl-kernel.c's
 * start_lisp callers / thread startup with x0 = TCR, w1 = resetp).
 * ported from ppc-subprims.s:73-205 (PPC64/EABI branch logic): save the
 * C callee-saved state, establish the Lisp register world from the
 * TCR's save_* slots, set valence, call toplevel_loop (C) -- or _SPreset
 * when resetp -- then tear back down to the C world.
 * ARM64-DEVIATION (mechanics): AAPCS64 callee set = x19-x28 + d8-d15 +
 * fp/lr (PPC saved r13-r31); rnil has no PPC analog (PPC materializes
 * nil as an immediate) and is loaded from the C global lisp_nil, which
 * the image loader updates at mapping time (Matt 2026-07-11 mail:
 * nil_value/t_value are patched at initial heap mapping).
 * =========================================================================== */
        .globl C(start_lisp)
C(start_lisp):
        stp     x29, x30, [sp, #-16]!   /* ppc:74 mflr/save               */
        mov     x29, sp
        stp     x19, x20, [sp, #-16]!   /* ppc:102-121 save nonvolatiles  */
        stp     x21, x22, [sp, #-16]!
        stp     x23, x24, [sp, #-16]!
        stp     x25, x26, [sp, #-16]!
        stp     x27, x28, [sp, #-16]!
        stp     d8,  d9,  [sp, #-16]!   /* AAPCS64 callee-saved FP lows   */
        stp     d10, d11, [sp, #-16]!
        stp     d12, d13, [sp, #-16]!
        stp     d14, d15, [sp, #-16]!
        mov     rcontext, x0            /* ppc:126 mr rcontext,r3         */
        mov     w9, w1                  /* stash resetp (volatile scratch;
                                           no call before the test)       */
        /* Zero the node registers (ppc:144-159). */
        mov     fn, xzr
        mov     arg_w, xzr
        mov     arg_x, xzr
        mov     arg_y, xzr
        mov     arg_z, xzr
        mov     temp0, xzr
        mov     temp1, xzr
        mov     temp2, xzr
        mov     temp3, xzr
        mov     temp4, xzr
        mov     temp5, xzr
        mov     save0, xzr
        mov     save1, xzr
        mov     save2, xzr
        mov     save3, xzr
        /* rnil: from the C global (image loader patches nil_value). */
        ldr     rnil, =lisp_nil
        ldr     rnil, [rnil]
        /* Lisp stack/alloc state from the TCR (ppc:162-165). */
        ldr     vsp, [rcontext, #tcr.save_vsp]
        ldr     tsp, [rcontext, #tcr.save_tsp]
        ldr     allocptr, [rcontext, #tcr.save_allocptr]
        ldr     allocbase, [rcontext, #tcr.save_allocbase]
        /* Cover the C region we are entering lisp from -- everything between
           this SP and tcr.last_lisp_frame (= cs_area->high on a thread's first
           entry, so glibc's TLS block and the whole C startup path) -- so the
           GC's linear cstack walk strides over it in one step.  MUST be the
           last thing pushed before lisp runs: the header has to sit at the
           lowest address of the region it describes.  See the protocol note
           above (ARM64-DEVIATION: PPC needs none, ppc-subprims.s:73). */
        cover_foreign_stack_region imm0, imm1
        mov     imm0, #TCR_STATE_LISP   /* ppc:166-167                    */
        str     imm0, [rcontext, #tcr.valence]
        cbnz    w9, 1f                  /* ppc:168 bne cr0,1f             */
        bl      C(toplevel_loop)        /* ppc:169                        */
        b       2f                      /* ppc:170                        */
1:      bl      _SPreset                /* ppc:172                        */
2:      /* Save the Lisp world back (ppc:174-179). */
        str     allocptr, [rcontext, #tcr.save_allocptr]
        str     allocbase, [rcontext, #tcr.save_allocbase]
        str     tsp, [rcontext, #tcr.save_tsp]
        str     vsp, [rcontext, #tcr.save_vsp]
        mov     imm0, #TCR_STATE_FOREIGN
        str     imm0, [rcontext, #tcr.valence]
        /* Drop the C-region cover and hand the enclosing boundary back: this
           thread is foreign again with no lisp frames of ours left. */
        uncover_foreign_stack_region imm0, imm1
        ldp     d14, d15, [sp], #16     /* restore nonvolatiles           */
        ldp     d12, d13, [sp], #16
        ldp     d10, d11, [sp], #16
        ldp     d8,  d9,  [sp], #16
        ldp     x27, x28, [sp], #16
        ldp     x25, x26, [sp], #16
        ldp     x23, x24, [sp], #16
        ldp     x21, x22, [sp], #16
        ldp     x19, x20, [sp], #16
        ldp     x29, x30, [sp], #16
        ret

/* toplevel_loop: run the vpushed toplevel function under %toplevel-catch%
 * until it leaves NIL on the vstack.  ported from ppc-subprims.s:34-64.
 * The `b 3f' after `bl _SPmkcatch1v' is the CLEANUP ADDRESS, not control
 * flow: mkcatch decodes it from [lr] and returns to lr+4 (spentry-C:286-294
 * protocol, same as PPC).  Called from start_lisp with the Lisp world live. */
        .globl C(toplevel_loop)
C(toplevel_loop):
        /* A MARKER lisp_frame, not a raw AAPCS64 {x29,x30} pair (16m56).
         * start_lisp installs its C-region cover at exactly the SP it calls us
         * with (:865-872), so whatever this function pushes is the FIRST word an
         * ascending mark_cstack_area walk reaches BELOW that cover.  A saved x29
         * is a 16-aligned cstack address: not lisp_frame_marker, not an
         * immheader, so the walker's raw-backlink branch (arm64-gc.c:1343) sets
         * current to it and jumps 176 bytes UP, straight over the cover, into
         * the C chain -- the stage-3 `UNKNOWN STACK WORD'.  Measured: the walk's
         * first backlink step was `+22 words' == the 176 bytes between this
         * frame and start_lisp's x29 (its own {x29,x30} pair + 9 register pairs
         * + the 16-byte cover pair).  A normal 32-byte lisp_frame strides
         * exactly onto the cover header instead.
         *
         * There is no PPC64 frame to port: ppc-subprims.s:35-40 allocates NO
         * frame at all, storing lr into the CALLER's PowerOpen linkage slot
         * (c_frame.savelr(sp)) and never moving sp, so PPC's toplevel_loop
         * leaves nothing unclassifiable on the cstack.  AAPCS64 reserves no such
         * caller-provided slot, so the frame has to be ours; the marker frame is
         * the smallest shape that keeps the emitter contract this file's
         * boundary-protocol note (:144-190) states.  Only lr needs preserving.
         * savefn is fn, which start_lisp zeroed -- fixnum 0, a valid GC root
         * (the walker marks savevsp/savefn as nodes and savelr as a pc
         * locative; savelr here is a 4-aligned kernel .text address, which
         * mark_pc_root treats as a no-op exactly as it already does for the
         * savelr of every lisp frame that returns into a subprim).
         * x29 is now left ALONE, which satisfies AAPCS64's callee-saved
         * guarantee for it trivially.
         * ARM64-DEVIATION: a marker frame where PPC needs no frame, for the same
         * reason the covers above exist -- marker walk, not a backlink chain. */
        sub     sp, sp, #lisp_frame.size
        mov     imm0, #lisp_frame_marker /* imm0 is dead here: start_lisp's last
                                            use was the valence store (:873) */
        str     imm0, [sp, #lisp_frame.marker]
        str     vsp, [sp, #lisp_frame.savevsp]
        str     fn,  [sp, #lisp_frame.savefn]
        str     lr,  [sp, #lisp_frame.savelr]   /* ppc:35-40 mflr/save    */
        b       3f                      /* ppc:41 b test                  */
1:      /* loop, ppc:42-45 */
        ref_nrs_value arg_z, toplcatch  /* ppc:43 catch tag = %toplevel-catch% value */
        bl      _SPmkcatch1v            /* ppc:44                         */
        b       3f                      /* ppc:45 cleanup address word    */
        /* catch body, ppc:47-51 */
        set_nargs 0                     /* ppc:47                         */
        bl      _SPfuncall              /* ppc:48 funcall temp0           */
        mov     arg_z, rnil             /* ppc:49 li arg_z,nil_value      */
        mov     imm0, #fixnumone        /* ppc:50                         */
        bl      _SPnthrow1value         /* ppc:51 unwind the catch        */
3:      /* test, ppc:52-55 */
        ldr     temp0, [vsp]            /* ppc:53 the toplevel fn (vpushed by caller) */
        cmp     temp0, rnil             /* ppc:54                         */
        b.ne    1b                      /* ppc:55                         */
        ldr     lr, [sp, #lisp_frame.savelr]    /* ppc:56-62 restore lr    */
        discard_lisp_frame              /* pop the 32-byte marker frame   */
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
        .quad _SPffcall_indirect_result // 132 SPffcall_indirect_result (PROPOSED extension, 16m71)
C(sptab_end):
