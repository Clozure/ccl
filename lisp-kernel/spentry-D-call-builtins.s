/* SPDX-License-Identifier: Apache-2.0 */

/*
 * ARM64 subprim implementations: call/arglist/builtin operations (59 subprims)
 * Ported from PPC64 to ARM64 for upstream low-tag design
 */

#include "arm64-constants.h"
#include "arm64-macros.s"
#include "arm64-globals-proposed.s"

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

/* lisp_frame: Matt's ARM-family MARKER frame, NOT PPC's backlink frame
 * (ground truth: his popj vinsn, compiler/ARM64/arm64-vinsns.lisp:61-67,
 * + subtag_lisp_frame_marker, arm64-constants.h:177).  Same equates as
 * spentry-A:55-59 / spentry-E.  Frame builds store #lisp_frame_marker at
 * slot 0; there is NO backlink word. */
.set lisp_frame.marker, 0
.set lisp_frame.savevsp, 8
.set lisp_frame.savefn, 16
.set lisp_frame.savelr, 24
.set lisp_frame.size, 32

/* symbol.fcell / function codevector: slot order from ppc-constants64.s
 * :237-245/:223-226.  Symbols keep their dedicated pointer tag; a function
 * is an ordinary miscobj (fulltag_function removed, patch 0055), so its
 * codevector slot sits at misc_data_offset (-4). */
.set symbol.fcell, (3*node_size - fulltag_symbol)
.set _function.codevector, misc_data_offset

/* fixnum 1 (x86-constants64.s:414). */
.set fixnumone, (1<<fixnumshift)

/* vectorH.logsize: slot 0 of a (misc-tagged) vector header,
 * ppc-constants64.s:259-265 _structf(vectorH). */
.set vectorH.logsize, misc_data_offset

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

/* discard_lisp_frame: pop one lisp frame from sp. */
.macro discard_lisp_frame
        add sp, sp, #lisp_frame.size
.endm

/* jump_builtin: dispatch to Lisp builtin handler via %builtin-functions%
 * vector.  Macro equivalent of PPC64 jump_builtin (ppc-spentry.s:37-42);
 * nrs/globals idiom per arm64-globals-proposed.s. */
.macro jump_builtin idx, nargs_count
        ref_nrs_value fname, builtin_functions
        set_nargs \nargs_count
        ldr fname, [fname, #(misc_data_offset + (\idx) * node_size)]
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.codevector]
        br temp0
.endm

/* ========== BASIC CALL/JUMP OPERATIONS ========== */

/* ported from ppc-spentry.s:44-45 (PPC64 branch: jump_fname macro) */
spentry jmpsym
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.codevector]
        br temp0
endsp jmpsym

/* ported from ppc-spentry.s:47-48 (PPC64 branch: jump_nfn macro) */
spentry jmpnfn
        ldr temp0, [nfn, #_function.codevector]
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
        ldr temp0, [nfn, #_function.codevector]
        br temp0
2:      /* symbol: call its function cell (unchecked slot-0 jump) */
        mov fname, temp0
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.codevector]
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

/* ported from ppc-spentry.s:1270-1276 (PPC64 branch).
 * Come here with saved context on top of stack.  Tail into the shared
 * return_values entry exported by spentry-C (_spentry(values):
 * contract there is temp4 = return pc, temp0 = entry vsp). */
/* pmcl-kernel.c:2110 takes &nvalret for lisp_global(LEXPR_RETURN)
   (PPC exports it the same way, ppc-spentry.s:1267-1271). */
        .globl C(nvalret)
spentry nvalret
C(nvalret):
        ldr temp4, [sp, #lisp_frame.savelr]     /* ppc:1272 ldr loc_pc     */
        ldr temp0, [sp, #lisp_frame.savevsp]    /* ppc:1273                */
        ldr fn, [sp, #lisp_frame.savefn]        /* ppc:1274                */
        discard_lisp_frame                      /* ppc:1275                */
        b return_values                         /* ppc:1276 (.globl in C)  */
endsp nvalret

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
        ldr temp0, [nfn, #_function.codevector]
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
        ldr temp0, [nfn, #_function.codevector]         /* ppc:2164        */
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
        ldr temp0, [nfn, #_function.codevector]
        br temp0

2:      ldr vsp, [sp, #lisp_frame.savevsp]
        add sp, sp, #lisp_frame.size
        ldr nfn, [fname, #symbol.fcell]
        ldr temp0, [nfn, #_function.codevector]
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
        ldr temp0, [nfn, #_function.codevector]
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
        ldr temp0, [nfn, #_function.codevector]
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
        ldr temp0, [nfn, #_function.codevector]
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
        ldr temp0, [nfn, #_function.codevector]
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
        ldr temp0, [nfn, #_function.codevector]
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
        ldr temp0, [nfn, #_function.codevector]
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
        ldr temp0, [nfn, #_function.codevector]
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
        ldr temp0, [nfn, #_function.codevector]
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
 * (symbol.fcell, _function.codevector, t_offset, lisp_frame marker layout,
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
