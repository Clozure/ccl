/* SPDX-License-Identifier: Apache-2.0 */

#include "arm64-constants.h"
#include "arm64-macros.s"
#include "arm64-globals-proposed.s"

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

/* tsp_frame: ppc-constants64.s:228-233 {backlink@0, type@8}; fixed_overhead
   and data_offset alias offset 16 (same equates as spentry-A:42-46).
   dnode_size itself is already real in Matt's arm64-constants.h. */
.set tsp_frame.backlink, 0
.set tsp_frame.type, 8
.set tsp_frame.fixed_overhead, 16
.set tsp_frame.data_offset, 16
.set tsp_frame.size, 16

/* Lisp error selectors: errors.s deferr(NAME,N) = boxed fixnum N. */
.set XBADVEC,    (2<<fixnumshift)       /* errors.s:177 */
.set XSETBADVEC, (7<<fixnumshift)       /* errors.s:182 */
.set XNOTELT,    (174<<fixnumshift)     /* errors.s:227 */
.set XIMPROPERLIST, (170<<fixnumshift)  /* errors.s:223 */
.set tstack_alloc_limit, 0xffff         /* ppc-constants.s:171 (as spentry-A) */

/* symbol.binding_index: slot 7 via the dedicated symbol fulltag
   (ppc-constants64.s:237-245 order; arm64 bias = -fulltag_symbol). */
.set symbol.binding_index, (7*node_size - fulltag_symbol)

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

/* Variable-sized BOXED tstack frame (ppc-macros.s:714-719
   TSP_Alloc_Var_Boxed): link the old tsp, mark boxed (type=0), and ZERO
   the data area so the GC never scans garbage.  \size = total bytes
   including tsp_frame.fixed_overhead; clobbers both operands and NZCV. */
.macro tsp_alloc_var_boxed size, tmp
        mov \tmp, tsp
        sub tsp, tsp, \size
        str \tmp, [tsp, #tsp_frame.backlink]
        str xzr, [tsp, #tsp_frame.type]
        add \size, tsp, #tsp_frame.fixed_overhead
        b 8886f
8885:   str xzr, [\size], #node_size
8886:   cmp \size, \tmp
        b.ne 8885b
.endm

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
        tsp_alloc_var_boxed imm1, imm2  /* ppc:875 (links+marks+ZEROES;
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
        tsp_alloc_var_boxed imm1, imm2  /* ppc:896                         */
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
        /* dnode_align + TSP_Alloc_Var_Boxed_nz: PPC64 916-917 */
        add imm1, nargs, #(dnode_size + node_size - 1)  /* dnode_size is real (arm64-constants.h:33) */
        and imm1, imm1, #(~(dnode_size - 1))
        add imm1, imm1, #tsp_frame.fixed_overhead
        tsp_alloc_var_boxed imm1, imm2  /* ppc:917 TSP_Alloc_Var_Boxed_nz
                                           (was a bare sub: no backlink/
                                           type/zeroing - GC hazard)       */
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
        /* count 0: empty boxed frame (ppc:989 TSP_Alloc_Fixed_Boxed(16)) */
        mov imm2, tsp
        sub tsp, tsp, #(2*node_size + tsp_frame.fixed_overhead)
        str imm2, [tsp, #tsp_frame.backlink]
        str xzr, [tsp, #tsp_frame.type]                 /* boxed           */
        str xzr, [tsp, #tsp_frame.data_offset]          /* count = 0       */
        str xzr, [tsp, #(tsp_frame.data_offset + node_size)]
        ret                             /* ppc:990                         */
2:      add imm1, imm1, #tsp_frame.fixed_overhead       /* ppc:992         */
        tsp_alloc_var_boxed imm1, imm2  /* ppc:993 (zeroes; clobbers NZCV) */
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
        /* dnode_align: (imm0 + node_size + tsp_frame.fixed_overhead + dnode_size - 1) & ~(dnode_size-1) */
        add imm0, imm0, #(node_size + tsp_frame.fixed_overhead + dnode_size - 1)  /* fixed_overhead=16, was mis-guessed 8 */
        and imm0, imm0, #(~(dnode_size - 1))
        /* TSP_Alloc_Var_Boxed_nz (ppc-macros.s:721-725): push frame WITH
           backlink, zero the data area, mark boxed.  The previous bare
           `sub tsp` dropped the backlink (PPC's stru writes it as a
           store-with-update side effect) — the frame's [tsp]=0 then fed
           tsp:=0 into the caller's tsp_unlink on the toplevel fn's second
           lap (16m5k wall, gdb-observed 2026-07-17). */
        mov imm4, tsp
        sub tsp, tsp, imm0
        str imm4, [tsp, #tsp_frame.backlink]
        mov imm3, tsp
        sub imm0, imm4, #node_size
3:      cmp imm3, imm0
        b.eq 4f
        str xzr, [imm3, #node_size]!
        b 3b
4:      str xzr, [tsp, #tsp_frame.type]          /* Set_TSP_Frame_Boxed */
        str imm2, [tsp, #tsp_frame.data_offset]  /* store header (data_offset=16, was mis-guessed 8) */
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

/* PROPOSED-CONSTANTS: arrayH struct offsets (not yet in arm64-constants.h).
 * define-lisp-object arrayH fulltag-misc (arm64-arch.lisp:683): slots
 * header, rank, physsize, data-vector, displacement, flags, then dims --
 * slot k sits at (k*node_size - fulltag_misc) from the tagged pointer
 * (fulltag_misc = 12, arm64-constants.h:144; misc-data-offset = -4).
 * 16m41: this block used to hand-number the offsets with PPC64's bias
 * (-4, i.e. rank@4 ... dim0@44), +8 off for every slot, so the rank
 * check read physsize and aset2/aref2/aset3/aref3 trapped on EVERY
 * valid array.  Symbolic now: correct by construction.  */
#ifndef ARRAYH_STRUCT_DEFINED
.set arrayH.rank,         (1*node_size - fulltag_misc)
.set arrayH.physsize,     (2*node_size - fulltag_misc)
.set arrayH.data_vector,  (3*node_size - fulltag_misc)
.set arrayH.displacement, (4*node_size - fulltag_misc)
.set arrayH.flags,        (5*node_size - fulltag_misc)
.set arrayH.dim0,         (6*node_size - fulltag_misc)
#define ARRAYH_STRUCT_DEFINED
#endif

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
