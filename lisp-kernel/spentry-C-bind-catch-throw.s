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

#include "arm64-constants.h"
#include "arm64-macros.s"
#include "arm64-globals-proposed.s"

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

/* fixnum 1, and the *INTERRUPT-LEVEL* special's tlb byte-index.
   x86-constants64.s:414 fixnumone=(1<<fixnumshift); :867 = fixnumone. */
.set fixnumone, (1<<fixnumshift)
.set fixnum_one, fixnumone
.set INTERRUPT_LEVEL_BINDING_INDEX, fixnumone

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

/* Function codevector lives in slot 0 (CLAUDE.md "codevector @ slot 0"; PPC
   _function.entrypoint).  Referenced through a fulltag_misc function pointer. */
.set function.codevector, misc_data_offset

/* symbol.flags bits (x86-constants64.s:707-710; low-tag => +fixnum_shift). */
.set sym_vbit_bound,      (0+fixnumshift)
.set sym_vbit_bound_mask, (1<<sym_vbit_bound)
.set sym_vbit_const,      (1+fixnumshift)
.set sym_vbit_const_mask, (1<<sym_vbit_const)

/*
 * ---------------------------------------------------------------------------
 * PROPOSED struct layouts (ratify with Matt; C side must match)
 * Defined with the _struct/_node/_field/_ends macros from arm64-constants.h.
 * ---------------------------------------------------------------------------
 */

/* symbol: referenced through a fulltag_symbol pointer (see spentry-D funcall,
   x86-constants64.s:616 `_structf(symbol,-fulltag_symbol)`).  Field order is
   the CCL-universal pname,vcell,fcell,...,binding_index. */
_structf symbol, -fulltag_symbol
  _node pname
  _node vcell
  _node fcell
  _node package_predicate
  _node flags
  _node plist
  _node binding_index
_endstructf

/* binding frame (vstack-consed): ppc-constants64.s _struct(binding,0). */
_struct binding, 0
  _node link
  _node sym
  _node val
_ends

/* lisp_frame on the control stack: Matt's ARM-family MARKER frame, NOT
   PPC's backlink frame (ground truth: his popj vinsn, compiler/ARM64/
   arm64-vinsns.lisp:61-67, + subtag_lisp_frame_marker).  Same layout as
   spentry-A/-D/-E.  Frame builds store #lisp_frame_marker at slot 0; no
   backlink word.  fn is VOLATILE (x7) in this design, so it is saved
   here across catch. */
.set lisp_frame.marker, 0
.set lisp_frame.savevsp, 8
.set lisp_frame.savefn, 16
.set lisp_frame.savelr, 24
.set lisp_frame.size, 32

/* temp-stack frame header: ppc-constants64.s _struct(tsp_frame,0).
   backlink+type = 2 nodes of fixed overhead; data follows. */
_struct tsp_frame, 0
  _node backlink
  _node type
_ends
.set tsp_frame.fixed_overhead, tsp_frame.size
.set tsp_frame.data_offset, tsp_frame.size

/* catch_frame: PPC64 layout (ppc-constants64.s _structf(catch_frame);
   ppc-constants64.h:213), but with regs sized to this design's nsaveregs=4
   (save0..save3) instead of PPC's 8. */
_structf catch_frame
  _node catch_tag           /* unbound_marker => unwind-protect, else catch */
  _node link                /* previous catch frame                         */
  _node mvflag              /* 0 => single value, fixnum 1 => multiple       */
  _node csp                 /* saved control-stack lisp_frame pointer        */
  _node db_link             /* special-binding chain head                    */
  _field regs, (nsaveregs*node_size)  /* save0..save3                        */
  _node xframe              /* exception-frame chain                         */
  _node nfp                 /* numeric/foreign frame pointer                 */
_endstructf

/*
 * ---------------------------------------------------------------------------
 * Local helper macros (this design has no ppc-macros.s / arm64-uuo.s in scope)
 * ---------------------------------------------------------------------------
 */

/* vstack push/pop (grows toward lower addresses). */
.macro vpush1 reg
        str \reg, [vsp, #-node_size]!
.endm
.macro vpop1 reg
        ldr \reg, [vsp], #node_size
.endm

/* nargs is a BOXED fixnum (== byte count of value block on the vstack). */
.macro set_nargs n
        mov nargs, #((\n)<<fixnumshift)
.endm

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
.macro discard_lisp_frame
        add sp, sp, #lisp_frame.size
.endm

/* temp-stack allocation (real tsp register, PPC discipline).
   ppc-macros.s TSP_Alloc_Fixed_Unboxed / Set_TSP_Frame_{Un,}boxed. */
.macro tsp_alloc_fixed_unboxed nbytes, tmp
        mov \tmp, tsp
        sub tsp, tsp, #((\nbytes) + tsp_frame.data_offset)
        str \tmp, [tsp, #tsp_frame.backlink]
        str tsp, [tsp, #tsp_frame.type]         /* non-zero => unboxed */
.endm
.macro set_tsp_frame_boxed
        str xzr, [tsp, #tsp_frame.type]         /* zero => boxed (GC-scanned) */
.endm
/* pop one tsp frame (ppc-macros.s unlink(tsp)). */
.macro tsp_unlink
        ldr tsp, [tsp, #tsp_frame.backlink]
.endm

/* save/restore the boxed NVRs into/from a catch frame's regs[] (save0..save3).
   catch_frame is a fulltag_misc-biased _structf, so .regs = 44 is only
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

/* Poll for a deferred interrupt (ppc-macros.s check_pending_interrupt).
   Clobbers nargs; callers that must preserve it save/restore around this. */
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
        tsp_alloc_fixed_unboxed catch_frame.size, imm4
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
        set_tsp_frame_boxed
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
        tsp_unlink
        ret
9:      /* _throw_tag_not_found */
        uuo_error_no_throw_tag temp0
        str temp0, [vsp, nargs]         /* restore tag; retry after handler     */
        b _SPthrow
endsp throw

/* ported from ppc-spentry.s:166-284 (PPC64 branch) */
/* Unwind N frames (imm0 = count), processing unwind-protects */
/* N multiple values atop vstack, nargs = count */
/* Variable-size boxed tsp frame (ppc-macros.s TSP_Alloc_Var_Boxed_nz).
   \size = 16-aligned data byte count (in a reg).  \p,\e scratch. */
        .macro tsp_alloc_var_boxed_nz size, p, e
        mov \p, tsp
        sub tsp, tsp, \size
        str \p, [tsp, #tsp_frame.backlink]
        /* zero data words [data_offset .. backlink) so GC sees clean slots */
        add \e, tsp, \size                    /* end = old tsp                */
        add \p, tsp, #tsp_frame.data_offset
        /* \@-unique labels: a bare 1:/2: inside a macro CAPTURES callers'
           1f/2f branches that cross the expansion (the 16m5t makes128/
           Misc_Alloc_Fixed class, patch 0011) */
.Ltavb\@:
        cmp \p, \e
        b.hs .Ltavbdone\@
        str xzr, [\p], #node_size
        b .Ltavb\@
.Ltavbdone\@:
        str xzr, [tsp, #tsp_frame.type]       /* boxed                        */
        .endm

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
        tsp_unlink
        discard_lisp_frame
        b 1b
4:      /* _nthrowv_do_unwind: run the cleanup form with values preserved      */
        ldr imm3, [temp0, #catch_frame.xframe]
        str imm3, [rcontext, #tcr.xframe]
        ldr imm3, [temp0, #catch_frame.nfp]
        str imm3, [rcontext, #tcr.nfp]
        restore_catch_regs temp0
        sub tsp, temp0, #(tsp_frame.fixed_overhead + fulltag_misc)
        tsp_unlink
        ldr temp4, [sp, #lisp_frame.savelr]   /* cleanup code address          */
        ldr nfn, [sp, #lisp_frame.savefn]     /* cleanup's own fn              */
        str fn, [sp, #lisp_frame.savefn]      /* stash caller fn in the frame  */
        mov fn, nfn
        str lr, [sp, #lisp_frame.savelr]      /* stash our return in the frame */
        /* allocate a boxed tsp frame: overhead + nargs bytes + 2 nodes        */
        add imm0, nargs, #(tsp_frame.fixed_overhead + (2*node_size) + (dnode_size-1))
        and imm0, imm0, #~(dnode_size-1)
        tsp_alloc_var_boxed_nz imm0, imm1, imm2
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
        tsp_unlink
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
        tsp_unlink                      /* ppc:323 */
        discard_lisp_frame              /* ppc:324 */
        b 1b                            /* ppc:325 */
4:      /* ppc:326 _nthrow1v_do_unwind */
        restore_catch_regs temp0        /* ppc:332 */
        sub tsp, temp0, #(tsp_frame.fixed_overhead + fulltag_misc)  /* ppc:333 */
        tsp_unlink                      /* ppc:334 */
        ldr temp4, [sp, #lisp_frame.savelr]  /* ppc:335,337 cleanup PC -> temp4  */
        ldr nfn, [sp, #lisp_frame.savefn]    /* ppc:336 cleanup's own fn         */
        str fn, [sp, #lisp_frame.savefn]     /* ppc:338 stash caller fn          */
        mov fn, nfn                     /* ppc:340 */
        str lr, [sp, #lisp_frame.savelr]     /* ppc:339,341 stash our return     */
        /* fixed boxed tsp frame: value + throw count = 2 nodes (ppc:342)        */
        tsp_alloc_fixed_unboxed 2*node_size, imm0
        set_tsp_frame_boxed
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
        tsp_unlink                      /* ppc:356 */
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
   straight through `mflr loc_pc' into local_label(return_values)
   (ppc:1214-1216).  A prior port mis-inserted this block BETWEEN
   `values' `mov temp4,lr' and `return_values', so `values' fell into
   ret1valn and ALWAYS delivered 1 value (set_nargs 1) -- the mv-return
   branch logic at return_values was only reachable via nvalret's
   explicit `b'.  Kept here, ABOVE `spentry values', so the fall-through
   values -> return_values is intact. */
        .globl C(ret1valn)
C(ret1valn):
        ldr lr, [sp, #lisp_frame.savelr]        /* ppc:1172 ldr loc_pc  */
        ldr vsp, [sp, #lisp_frame.savevsp]      /* ppc:1173             */
        ldr fn, [sp, #lisp_frame.savefn]        /* ppc:1175             */
        add sp, sp, #lisp_frame.size            /* ppc:1176 discard     */
        vpush1 arg_z                            /* ppc:1177             */
        set_nargs 1                             /* ppc:1178             */
        ret                                     /* ppc:1179 blr         */

/* ported from ppc-spentry.s:1214-1248 (PPC64 branch) */
/* Return multiple values. nargs = count (fixnum), values on stack */
spentry values
        /* ppc-spentry.s:1214-1265 (PPC64 branch).  temp0 = entry vsp (VERIFIED
           cont-71); nargs = boxed value count.  No loc_pc register in this
           design -- lr(x30) carries the return pc, stashed in temp4. */
        mov temp4, lr                   /* ppc:1215 mflr loc_pc (->temp4)        */
        /* FALL THROUGH to return_values (ppc:1216 local_label). */

        .globl return_values
return_values:                          /* ppc:1216 shared entry; spentry-D nvalret must `b return_values' */
        /* ppc:1217 ref_global(imm0,ret1val_addr): load the ret1val_addr global.
           No ref_global / lisp_globals idiom exists for ARM64 in this file or
           its includes (see spentry-A-alloc-numbers.s:25-26 and the open
           PORT-TODO at spentry-D-call-builtins.s:112-113). */
        ref_global imm0, ret1val_addr   /* ppc:1217 (idiom: arm64-globals-proposed.s) */
        mov arg_z, rnil                 /* ppc:1218 li arg_z,nil_value           */
        cmp nargs, #(4096-(dnode_size+dnode_size))  /* ppc:1221 cmpri cr2        */
        b.ge 2f                         /* ppc:1224 bge cr2 -> too many values   */
        cmp imm0, temp4                 /* ppc:1222 cmpr cr1 (imm0==ret1val_addr?)*/
        b.eq 3f                         /* ppc:1225 beq cr1 -> return to real caller*/
        mov lr, temp4                   /* ppc:1226 mtlr loc_pc                   */
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
        ldr temp4, [sp, #lisp_frame.savelr]   /* ppc:1240 ldr loc_pc            */
        add imm1, vsp, nargs            /* ppc:1241 add imm1,nargs,vsp            */
        ldr imm0, [sp, #lisp_frame.savevsp]   /* ppc:1242                        */
        ldr fn, [sp, #lisp_frame.savefn]      /* ppc:1243                        */
        mov lr, temp4                   /* ppc:1245 mtlr loc_pc                   */
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
        tsp_alloc_var_boxed_nz imm2, imm3, imm4   /* ppc:2044 TSP_Alloc_Var_Boxed */
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
        tsp_alloc_fixed_unboxed 0, imm3 /* ppc:2058 TSP_Alloc_Fixed_Unboxed(0)  */
        vpush1 arg_z                    /* ppc:2059 vpush(arg_z)                */
        ret                             /* ppc:2060 blr                         */
9:      /* ppc:2061 */
        tsp_alloc_fixed_unboxed 0, imm3 /* ppc:2062 TSP_Alloc_Fixed_Unboxed(0)  */
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
        tsp_unlink                                  /* ppc:3853 unlink(tsp)       */
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
        /* dnode_align(imm0,nargs,tsp_frame.fixed_overhead+2*node_size) ppc:4994,
           inlined per spentry-A:447-448. */
        add imm0, nargs, #(tsp_frame.fixed_overhead+(2*node_size)+dnode_size-1)
        and imm0, imm0, #0xfffffffffffffff0
        tsp_alloc_var_boxed_nz imm0, imm3, imm4     /* ppc:4995 (imm3+scratch)    */
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

/* ported from ppc-spentry.s:5330-5334 (PPC64 branch) */
/* Save context and vsp */
spentry savecontextvsp
        /* ppc-spentry.s:5330-5334.  Save fn, return-pc, vsp into a control-stack
           lisp_frame, install nfn as the current fn, trap on stack overflow.
           No generic build_lisp_frame macro here; inline it (mirrors
           build_catch_lisp_frame but stores lr as savelr).  loc_pc == lr == x30.
           temp0 is a free scratch (not a live input; mkcatch uses it likewise). */
        ldr imm0, [rcontext, #tcr.cs_limit]     /* ppc:5331 cs_limit            */
        sub sp, sp, #lisp_frame.size            /* ppc:5332 build_lisp_frame:   */
        mov temp0, #lisp_frame_marker           /*   MARKER frame               */
        str temp0, [sp, #lisp_frame.marker]
        str vsp, [sp, #lisp_frame.savevsp]
        str fn, [sp, #lisp_frame.savefn]
        str lr, [sp, #lisp_frame.savelr]        /* loc_pc == return pc          */
        mov fn, nfn                             /* ppc:5333 mr fn,nfn           */
        cmp sp, imm0                            /* ppc:5334 trllt(sp,imm0)      */
        b.hs 1f                                 /* sp>=cs_limit: no overflow    */
        uuo_interr error_stack_overflow, sp /* ppc:5334 trllt (PROPOSED ext) */
1:      ret                                     /* ppc:5335 blr                 */
endsp savecontextvsp

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

/* ported from ppc-spentry.s:5337-5343 (PPC64 branch) */
/* Save context with vsp+imm0 as the stored vsp (callee-save setup variant). */
spentry savecontext0
        /* ppc-spentry.s:5337-5343.  Like savecontextvsp, but the stored vsp is
           (vsp + imm0), and cs_limit is loaded AFTER building the frame (the PPC
           ordering differs from savecontextvsp).  imm0 is an unboxed byte delta. */
        add imm0, vsp, imm0             /* ppc:5338 add imm0,vsp,imm0           */
        sub sp, sp, #lisp_frame.size    /* ppc:5339 build_lisp_frame: MARKER    */
        mov temp0, #lisp_frame_marker
        str temp0, [sp, #lisp_frame.marker]
        str imm0, [sp, #lisp_frame.savevsp] /* stored vsp = vsp+delta           */
        str fn, [sp, #lisp_frame.savefn]
        str lr, [sp, #lisp_frame.savelr]
        ldr imm0, [rcontext, #tcr.cs_limit] /* ppc:5340 ldr(imm0,tcr.cs_limit)  */
        mov fn, nfn                     /* ppc:5341 mr fn,nfn                   */
        cmp sp, imm0                    /* ppc:5342 trllt(sp,imm0)              */
        b.hs 1f                         /* sp>=cs_limit: no overflow            */
        uuo_interr error_stack_overflow, sp /* ppc:5342 trllt (PROPOSED ext) */
1:      ret                             /* ppc:5343 blr                         */
endsp savecontext0

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
 *   - stack-overflow (trllt) trap convention (savecontextvsp ppc:5333;
 *     savecontext0 ppc:5342)
 *   - deferred-interrupt trap trgti (restoreintlevel ppc:6751)
 * PROPOSED (ratify): mask_initopt/keyp/aok/restp adopt the ARM32 bit layout
 * (arm-constants.s:561-567), NOT the PPC big-endian bit indices; must match
 * the compiler's doadlword emission when Matt defines it.
 * Struct layouts (catch_frame with nsaveregs=4, lisp_frame, tsp_frame,
 * binding, symbol) are PROPOSED in this file's header blocks; the C runtime
 * and GC must agree.
 */
