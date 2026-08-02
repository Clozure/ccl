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

#include "arm64-constants.h"
#include "arm64-macros.s"
#include "arm64-globals-proposed.s"

/*
 * ===========================================================================
 * PROPOSED-CONSTANTS (ratify with Matt)
 * ---------------------------------------------------------------------------
 * NOT in arm64-constants.h.  Values DERIVED from the cited sources; the C
 * runtime and compiler must agree.
 * ===========================================================================
 */

/* fixnum 1 and the *INTERRUPT-LEVEL* tlb index (x86-constants64.s:414/867;
   same equates as spentry-C). */
.set fixnumone, (1<<fixnumshift)
.set INTERRUPT_LEVEL_BINDING_INDEX, fixnumone

/* Thread valence values: lisp-kernel/constants.h:27-28 (present verbatim in
   Matt's tree; a C header, so re-equated here for the assembler). */
.set TCR_STATE_LISP,    0
.set TCR_STATE_FOREIGN, 1

/* lisp_frame: Matt's ARM-family MARKER frame (ground truth: his popj vinsn,
   compiler/ARM64/arm64-vinsns.lisp:61-67, + subtag_lisp_frame_marker,
   arm64-constants.h:177).  Same equates as spentry-A:55-59. */
.set lisp_frame.marker,  0
.set lisp_frame.savevsp, 8
.set lisp_frame.savefn,  16
.set lisp_frame.savelr,  24
.set lisp_frame.size,    32

/* symbol.fcell / function codevector: slot order from ppc-constants64.s
   :237-245/:223-226.  Symbols keep their dedicated pointer tag; a function
   is an ordinary miscobj (fulltag_function removed, patch 0055), so its
   codevector slot is misc_data_offset.  Same equates as spentry-A. */
.set symbol.fcell, (3*node_size - fulltag_symbol)
.set _function.codevector, misc_data_offset

/* area: ppc-constants64.s:382-401 _struct(area,0).
   16m41 CORRECTION: the note here said "Matt's tcr has NO
   tcr.last_lisp_frame, so [writing area.active from asm] is the line-port".
   STALE -- arm64-constants.h:470 (asm) / :531 (C) both carry
   last_lisp_frame at this pin, and nothing in this file ever wrote
   area.active either, so the boundary went unrecorded entirely.  The
   protocol now lives in tcr.last_lisp_frame (see the macros above); the C
   side does the area lookup in normalize_tcr, which also keeps the
   cs_area->older chain consistent, something a raw asm store could not. */
_struct area, 0
  _node pred
  _node succ
  _node low
  _node high
  _node active
  _node softlimit
  _node hardlimit
_ends

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

.macro vpush1 reg
        str \reg, [vsp, #-node_size]!
.endm
.macro vpop1 reg
        ldr \reg, [vsp], #node_size
.endm
.macro set_nargs n
        mov nargs, #((\n)<<fixnumshift)
.endm
.macro discard_lisp_frame
        add sp, sp, #lisp_frame.size
.endm

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

/* Poll for a deferred interrupt (ppc-macros.s check_pending_interrupt;
   clobbers nargs).  arm64-uuo.s's uuo_interrupt_now, which is
   uuo_misc 4 at pin 9c61574 -- it was misc 3 @115b7aa, before
   uuo_debug_trap was inserted at 3.  We invoke the MACRO, so the renumber
   costs nothing; only a hardcoded number would have broken.  Stack-overflow
   sites use the PROPOSED uuo_interr extension. */
.set error_stack_overflow, 5            /* errors.s:25 */
.macro check_pending_interrupt
        ldr nargs, [rcontext, #tcr.tlb_pointer]
        ldr nargs, [nargs, #INTERRUPT_LEVEL_BINDING_INDEX]
        cmp nargs, #0
        b.lt 8887f                       /* interrupts disabled: done */
        b.gt 8886f                       /* level>0: cannot be pending here */
        ldr nargs, [rcontext, #tcr.interrupt_pending]
        cbz nargs, 8887f
8886:   uuo_interrupt_now                /* uuo_misc 4 at pin 9c61574 (was 3 @115b7aa, before he
                                            inserted uuo_debug_trap at 3) */
8887:
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
 * with the process-default FPCR; FPSR exception flags published to
 * tcr.foreign_fpsr per Matt's f067047 TCR).  The earlier draft that
 * lived here used the pre-w13 frame layout and the removed
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
        str fn, [vsp, #-node_size]!             /* ppc:1799 vpush_saveregs   */
        str save3, [vsp, #-node_size]!
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
        str fn,  [imm2, #lisp_frame.savefn]
        str lr,  [imm2, #lisp_frame.savelr]
        sub imm0, imm0, #(4 << num_subtag_bits)
        str imm0, [sp, #c_frame.header]
        /* Buffer address -> save2 (PPC uses save7, ppc:1800). */
        str save2, [vsp, #-node_size]!
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
        /* Load the outgoing GPR args and pop the frame head + param
           words so stack args sit exactly at SP (ppc:1836-1843).
           Above the valence store since 16m41: see the ordering note in
           `spentry ffcall' (arm64-spentry.s) -- the boundary has to be
           recorded before this thread advertises foreign valence, and the
           park slot is only dead once the args are loaded. */
        ldp x0, x1, [sp, #c_frame.params]
        ldp x2, x3, [sp, #(c_frame.params + 2*node_size)]
        ldp x4, x5, [sp, #(c_frame.params + 4*node_size)]
        ldp x6, x7, [sp, #(c_frame.params + 6*node_size)]
        /* AAPCS64, no stack args (<=8 GPR/<=8 FP, enforced loud in the
         * w13 codegen): keep SP at the frame head -- the callee's stack
         * grows BELOW its incoming SP, so popping the frame here hands
         * the saved lr/backlink to the callee as scratch (16m5c crash in
         * the _SPffcall twin: return jumped into the c_frame).  Stack-arg
         * layout = ratify item (frame head must move above params). */
        /* Boundary bookkeeping + valence, identical to `spentry ffcall' in
         * arm64-spentry.s -- see the protocol note at the top of this file.
         * temp0, not imm0: imm0 is x0, now an outgoing argument. */
        ldr temp0, [rcontext, #tcr.last_lisp_frame]
        str temp0, [sp, #c_frame.params]
        mov temp0, sp
        str temp0, [rcontext, #tcr.last_lisp_frame]
        mov temp0, #TCR_STATE_FOREIGN
        str temp0, [rcontext, #tcr.valence]
        /* Open the FPSR capture window -- FPSR is cumulative, so clearing it
           only on the way back publishes every flag raised since the PREVIOUS
           ff-call.  Canonical note: `spentry ffcall' in arm64-spentry.s. */
        msr fpsr, xzr
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
        /* ---- common return path (= patch-0003 ffcall) ---- */
        mrs imm1, fpsr
        str imm1, [rcontext, #tcr.foreign_fpsr]
        msr fpsr, xzr
        /* A GC may have run while we were foreign. */
        ldr allocptr, [rcontext, #tcr.save_allocptr]
        ldr allocbase, [rcontext, #tcr.save_allocbase]
        /* lr from the boundary lisp_frame; sp from the SAVED SP word, not the
         * header at offset 0 (16m30).  Count is already shrunk by 4, so
         * reserved_base = save3 + node_size*(count+1).  imm1/imm2 only. */
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
        ldr save2, [vsp], #node_size
        ldr save3, [vsp], #node_size
        ldr fn, [vsp], #node_size
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
        check_pending_interrupt
        ret
endsp ffcall_return_registers

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
 * stub enters here with the UNBOXED callback index in arg_w=x8 (16m14:
 * fixed a =x9 typo here — the code below reads arg_w, which is x8; the
 * level-1/arm64-callback-support.lisp generator stamps x8) (PPC uses
 * r11; any AAPCS64 caller-saved scratch works - the trampoline generator
 * is a lisp-side deliverable that must match).
 *
 * PROPOSED frame contract (RATIFY - lisp-side callback glue must match;
 * boot-validated shape from our v2 tree): x0..x7 are pushed so the x0 slot
 * abuts the incoming sp; CBF = &x0save.  The C caller's stack args then
 * sit contiguously at CBF+64 (the PowerOpen single-linear-offset property,
 * reproduced).  d0..d7 saves at CBF-64..-8.  The GPR result is reloaded
 * from CBF+0/+8, the FPR result from CBF-64.  CBF is 16-aligned, so it is
 * its own fixnum boxing.
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
        /* Stash index + CBF in just-saved callee-saved regs: they must
           survive the get_tcr C call (x9-x17 are caller-saved, and a
           linker veneer may clobber x16/x17). */
        mov save0, arg_w
        mov save1, arg_x
        /* Recover the thread context (ppc:5114-5124 get_tcr(1)). */
        mov x0, #1
        bl get_tcr
        mov rcontext, x0
        /* Stash the exact foreign sp for the return path. */
        mov imm0, sp
        stp imm0, xzr, [sp, #-16]!
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
        ldr temp4, [nfn, #_function.codevector]
        blr temp4
        /* Lisp wrote the result into CBF+0/+8 / CBF-64 (glue contract).
           CBF is recomputed below from the restored sp (fixed layout);
           first publish lisp state back to the tcr (ppc:5159-5169). */
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
        ldur d0, [imm2, #-64]                   /* FPR result                */
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
        str fn, [vsp, #-node_size]!             /* ppc:5404 vpush_saveregs   */
        str save3, [vsp, #-node_size]!
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
        str fn,  [imm2, #lisp_frame.savefn]
        str lr,  [imm2, #lisp_frame.savelr]
        sub imm0, imm0, #(4 << num_subtag_bits)
        str imm0, [sp, #c_frame.header]
        /* Publish lisp state to the TCR for the GC, then go foreign
           (ppc:5405-5422). */
        str vsp, [rcontext, #tcr.save_vsp]
        str tsp, [rcontext, #tcr.save_tsp]
        str allocptr, [rcontext, #tcr.save_allocptr]
        str allocbase, [rcontext, #tcr.save_allocbase]
        str xzr, [rcontext, #tcr.foreign_fpsr]  /* syscalls raise no FP
                           exceptions; publish a clean slate (PPC zeroes
                           ffi_exception here, ppc:5415-5419) */
        /* Syscall number + up to 6 args (ppc:5424-5432 loads r3-r10 + r0). */
        asr x8, arg_z, #fixnumshift             /* ppc:5432 unbox_fixnum     */
        ldp x0, x1, [sp, #c_frame.params]
        ldp x2, x3, [sp, #(c_frame.params + 2*node_size)]
        ldp x4, x5, [sp, #(c_frame.params + 4*node_size)]
        /* 16m41 PARITY (this spentry goes foreign exactly like ffcall and was
         * missing the same bookkeeping): park the enclosing boundary in the
         * now-dead param word 0, then record the boundary and only then
         * advertise foreign valence.  Unlike ffcall this frame is POPPED
         * before the trap, so the boundary is taken AFTER the pop and names
         * live stack -- the caller's own region, since the c_frame (still
         * intact below SP, which is what the return path already relies on)
         * has nothing the GC needs. */
        ldr temp0, [rcontext, #tcr.last_lisp_frame]
        str temp0, [sp, #c_frame.params]
        add sp, sp, #(c_frame.size + 8*node_size)
        mov temp0, sp
        str temp0, [rcontext, #tcr.last_lisp_frame]
        mov temp0, #TCR_STATE_FOREIGN
        str temp0, [rcontext, #tcr.valence]
        svc #0                                  /* ppc:5433 sc               */
        /* ---- return path (x0 = raw result / -errno) (ppc:5455-5489) ---- */
        ldr allocptr,  [rcontext, #tcr.save_allocptr]   /* ppc:5470-5472     */
        ldr allocbase, [rcontext, #tcr.save_allocbase]
        /* lr from the boundary lisp_frame; sp from the SAVED SP word, not the
         * header at offset 0 (16m30).  Count is already shrunk by 4, so
         * reserved_base = save3 + node_size*(count+1).  imm1/imm2 only. */
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
        str xzr, [rcontext, #tcr.valence]       /* TCR_STATE_LISP; ppc:5481  */
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
        uuo_interr error_stack_overflow, sp /* ppc:5371 trllt (PROPOSED ext) */
2:      mov fn, #0                              /* ppc:5372                  */
        ret                                     /* ppc:5373 blr (to prologue)*/
        /* Single-value case: return to something that pops the variable-
           length frame off the vstack (ppc:5377-5382). */
1:
        ref_global temp4, lexpr_return1v        /* ppc:5378 */
        ldr imm0, [rcontext, #tcr.cs_limit]     /* ppc:5379-5380             */
        cmp sp, imm0
        b.hi 3f
        uuo_interr error_stack_overflow, sp /* ppc:5380 trllt (PROPOSED ext) */
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
