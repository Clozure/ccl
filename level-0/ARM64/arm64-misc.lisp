;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;; arm64-misc.lisp — ACTIVE seed of level-0/ARM64/ for Matt Emerson's
;;; upstream arm64 tree (pin 6b6540e).  Deployed into
;;; $UPSTREAM_ROOT/level-0/ARM64/ by tools/upstream-compile-test.sh before
;;; the xload dump; grows DEMAND-DRIVEN — each boot's cold-load fatal names
;;; the next function to lift from upstream-port/level-0/drafts/arm64-misc.lisp.
;;; PPC64 LINE-PORT (source: vendor/ccl/level-0/PPC/ppc-misc.lisp)
;;; Per-line citations: "; ppc:NNN" = line NNN of that file.
;;;
;;; TCR/area offsets resolve SYMBOLICALLY against his arm64-arch.lisp, which
;;; the kernel's arm64-constants.h asserts itself in sync with (tcr.spare
;;; static check) — W4-D20 reconciled at this pin.  Current values, for the
;;; reader: tcr.save-vsp=32, tcr.vs-area=56; area.high=24, area.active=32.

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "ARM64-LAPMACROS"))

;;; =====================================================================
;;; %current-tcr — ppc:445
;;; =====================================================================
(defarm64lapfunction %current-tcr ()
  (mov arg_z rcontext)                             ; ppc:446
  (ret))                                           ; ppc:447

;;; =====================================================================
;;; %tcr-toplevel-function — ppc:449
;;; =====================================================================
;;; cr0 used twice (tcr vs rcontext; then high vs active/vsp).  Loads are
;;; flag-safe so each (cmp) is placed just before its branch.
(defarm64lapfunction %tcr-toplevel-function ((tcr arg_z))
  (check-nargs 1)                                  ; ppc:450
  (mov imm0 vsp)                                   ; ppc:452 (default vsp)
  (ldr temp0 (:@ tcr (:$ arm64::tcr.vs-area)))     ; ppc:453 (vs-area=56)
  (ldr imm1 (:@ temp0 (:$ arm64::area.high)))      ; ppc:454 (area.high=24)
  (cmp tcr rcontext)                               ; ppc:451 (cmpr)
  (b.eq @room)                                     ; ppc:455 (beq)
  (ldr imm0 (:@ temp0 (:$ arm64::area.active)))    ; ppc:456 (area.active=32)
  @room
  (cmp imm1 imm0)                                  ; ppc:458 (cmpr)
  (mov arg_z rnil)                                 ; ppc:459 (li nil — flag-safe)
  (b.eq @done)                                     ; ppc:460 (beqlr)
  (ldur arg_z (:@ imm1 (:$ (- arm64::node-size)))) ; ppc:461 ([high - node-size])
  @done
  (ret))                                           ; ppc:462

;;; =====================================================================
;;; %set-tcr-toplevel-function — ppc:464
;;; =====================================================================
(defarm64lapfunction %set-tcr-toplevel-function ((tcr arg_y) (fun arg_z))
  (check-nargs 2)                                  ; ppc:465
  (mov imm0 vsp)                                   ; ppc:467
  (ldr temp0 (:@ tcr (:$ arm64::tcr.vs-area)))     ; ppc:468
  (ldr imm1 (:@ temp0 (:$ arm64::area.high)))      ; ppc:469
  (cmp tcr rcontext)                               ; ppc:466 (cmpr)
  (b.eq @check-room)                               ; ppc:470 (beq)
  (ldr imm0 (:@ temp0 (:$ arm64::area.active)))    ; ppc:471
  @check-room
  (cmp imm1 imm0)                                  ; ppc:473 (cmpr)
  (push xzr imm1)                                  ; ppc:474 (push rzero imm1 — flag-safe)
  (b.ne @have-room)                                ; ppc:475 (bne)
  (str imm1 (:@ temp0 (:$ arm64::area.active)))    ; ppc:476
  (str imm1 (:@ tcr (:$ arm64::tcr.save-vsp)))     ; ppc:477 (save-vsp=32)
  @have-room
  (str fun (:@ imm1 (:$ 0)))                       ; ppc:479
  (ret))                                           ; ppc:480

;;; =====================================================================
;;; interrupt-level — ppc:409
;;; =====================================================================
(defarm64lapfunction interrupt-level ()
  (ldr arg_z (:@ rcontext (:$ arm64::tcr.tlb-pointer)))            ; ppc:410
  (ldr arg_z (:@ arg_z (:$ arm64::interrupt-level-binding-index))) ; ppc:411
  (ret))                                           ; ppc:412

;;; =====================================================================
;;; disable-lisp-interrupts — ppc:415
;;; =====================================================================
(defarm64lapfunction disable-lisp-interrupts ()
  (mov imm0 (:$ (ash -1 arm64::fixnumshift)))      ; ppc:416 (li imm0 '-1)
  (ldr imm1 (:@ rcontext (:$ arm64::tcr.tlb-pointer)))             ; ppc:417
  (ldr arg_z (:@ imm1 (:$ arm64::interrupt-level-binding-index)))  ; ppc:418
  (str imm0 (:@ imm1 (:$ arm64::interrupt-level-binding-index)))   ; ppc:419
  (ret))                                           ; ppc:420

;;; =====================================================================
;;; set-interrupt-level — ppc:422
;;; =====================================================================
(defarm64lapfunction set-interrupt-level ((new arg_z))
  (ldr imm1 (:@ rcontext (:$ arm64::tcr.tlb-pointer)))             ; ppc:423
  (trap-unless-lisptag= new arm64::tag-fixnum imm0)                ; ppc:424
  (str new (:@ imm1 (:$ arm64::interrupt-level-binding-index)))    ; ppc:425
  (ret))                                           ; ppc:426

;;; =====================================================================
;;; restore-interrupt-level — ppc:430
;;; =====================================================================
;;; Two CR fields (cr1: old vs 0; cr0: interrupt-pending vs 0) serialized:
;;; loads are flag-safe, so both (cmp) sit right before their branches.
(defarm64lapfunction restore-interrupt-level ((old arg_z))
  (ldr imm0 (:@ rcontext (:$ arm64::tcr.interrupt-pending)))       ; ppc:432
  (ldr imm1 (:@ rcontext (:$ arm64::tcr.tlb-pointer)))             ; ppc:433
  (cmp old (:$ 0))                                 ; ppc:431 (cmpri cr1)
  (b.ne @store)                                    ; ppc:435 (bne cr1)
  (cmp imm0 (:$ 0))                                ; ppc:434 (cmpri cr0)
  (b.eq @store)                                    ; ppc:436 (beq cr0)
  (str xzr (:@ rcontext (:$ arm64::tcr.interrupt-pending)))        ; ppc:437
  (mov old (:$ (ash 1 arm64::fixnumshift)))        ; ppc:438 (li old '1)
  @store
  (str old (:@ imm1 (:$ arm64::interrupt-level-binding-index)))    ; ppc:440
  (ret))                                           ; ppc:441

;;; =====================================================================
;;; set-%gcable-macptrs% — ppc:505
;;; =====================================================================
;;; imm0 = &gcable-pointers (rnil + negative kernel-global offset → sub).
;;; Push ptr onto the gcable list head atomically.  status=(:w temp4)
;;; per the kernel ll/sc idiom (spentry-B:113-129).
(defarm64lapfunction set-%gcable-macptrs% ((ptr arg_z))
  (sub imm0 rnil (:$ (- (arm64::%kernel-global 'gcable-pointers)))) ; ppc:506
  @again
  (ldxr arg_y (:@ imm0))                           ; ppc:508 lrarx (old head)
  (stur arg_y (:@ ptr (:$ arm64::xmacptr.link)))   ; ppc:509
  (stxr (:w temp4) ptr (:@ imm0))                  ; ppc:510 strcx.
  (cbnz (:w temp4) @again)                         ; ppc:511
  (dmb (:$ 11))                                    ; ppc:512 isync → dmb ish
  (ret))                                           ; ppc:513

;;; =====================================================================
;;; get-saved-register-values — ppc:933  (modern arch: plain DEFUN)
;;; =====================================================================
;;; The PPC LAP vpushes save0..save7 (8-NVR specific).  The x86-64 and
;;; ARM32 twins both reduce this to (values) (x86-misc:805, arm-misc:1066);
;;; call-check-regs (l1-readloop) then trivially passes.  Follow them.
(defun get-saved-register-values ()
  (values))

;;; =====================================================================
;;; %current-db-link — ppc:947
;;; =====================================================================
(defarm64lapfunction %current-db-link ()
  (ldr arg_z (:@ rcontext (:$ arm64::tcr.db-link))) ; ppc:948
  (ret))                                           ; ppc:949

;;; =====================================================================
;;; %no-thread-local-binding-marker — ppc:951
;;; =====================================================================
(defarm64lapfunction %no-thread-local-binding-marker ()
  (mov arg_z (:$ arm64::subtag-no-thread-local-binding)) ; ppc:952
  (ret))                                           ; ppc:953

;;; =====================================================================
;;; %store-node-conditional — ppc:483
;;; =====================================================================
;;; Whole body = TAIL jump to the EGC-memoizing subprim (PPC: ba).
(defarm64lapfunction %store-node-conditional ((offset 0) (object arg_x) (old arg_y) (new arg_z))
  (jump-subprim .SPstore-node-conditional))        ; ppc:484 (ba)

;;; =====================================================================
;;; %store-immediate-conditional — ppc:486
;;; =====================================================================
;;; ll/sc: current=temp1, status=(:w temp4).  offset vpop'd, unboxed, added
;;; into imm1... base kept in imm2 ([Xn]-only; imm1 is macro scratch).
(defarm64lapfunction %store-immediate-conditional ((offset 0) (object arg_x) (old arg_y) (new arg_z))
  (vpop temp0)                                     ; ppc:487
  (unbox-fixnum imm0 temp0)                        ; ppc:488
  (add imm2 object imm0)                           ; base = object+offset
  @again
  (ldxr temp1 (:@ imm2))                           ; ppc:491 lrarx current
  (cmp temp1 old)                                  ; ppc:492 cmpr
  (b.ne @lose)                                     ; ppc:493 (bne)
  (stxr (:w temp4) new (:@ imm2))                  ; ppc:494 strcx.
  (cbnz (:w temp4) @again)                         ; ppc:495 (bne @again)
  (dmb (:$ 11))                                    ; ppc:496 isync → dmb ish
  (add arg_z rnil (:$ arm64::t-offset))            ; ppc:497 (li arg_z T)
  (ret)                                            ; ppc:498
  @lose
  (clrex)                                          ; ppc:500-501 reservation-discharge
  (mov arg_z rnil)                                 ; ppc:502 (li nil)
  (ret))                                           ; ppc:503

;;; =====================================================================
;;; %atomic-incf-node — ppc:555
;;; =====================================================================
(defarm64lapfunction %atomic-incf-node ((by arg_x) (node arg_y) (disp arg_z))
  (check-nargs 3)                                 ; ppc:556
  (unbox-fixnum imm1 disp)                          ; ppc:557
  (add imm0 node imm1)                             ; base = node+disp ([Xn]-only)
  @again
  (ldxr arg_z (:@ imm0))                           ; ppc:559 lrarx
  (add arg_z arg_z by)                             ; ppc:560
  (stxr (:w temp4) arg_z (:@ imm0))                ; ppc:561 strcx.
  (cbnz (:w temp4) @again)                          ; ppc:562 (bne- @again)
  (dmb (:$ 11))                                    ; ppc:563 isync
  (ret))                                           ; ppc:564

;;; =====================================================================
;;; %atomic-incf-ptr — ppc:566
;;; =====================================================================
(defarm64lapfunction %atomic-incf-ptr ((ptr arg_z))
  (macptr-ptr imm1 ptr)                            ; ppc:567 (base — [Xn] directly)
  @again
  (ldxr imm0 (:@ imm1))                            ; ppc:569 lrarx
  (add imm0 imm0 (:$ 1))                           ; ppc:570 (addi raw +1)
  (stxr (:w temp4) imm0 (:@ imm1))                 ; ppc:571 strcx.
  (cbnz (:w temp4) @again)                          ; ppc:572
  (dmb (:$ 11))                                    ; ppc:573 isync
  (box-fixnum arg_z imm0)                           ; ppc:574
  (ret))                                           ; ppc:575

;;; =====================================================================
;;; %atomic-incf-ptr-by — ppc:577
;;; =====================================================================
(defarm64lapfunction %atomic-incf-ptr-by ((ptr arg_y) (by arg_z))
  (macptr-ptr imm1 ptr)                            ; ppc:578
  (unbox-fixnum imm2 by)                            ; ppc:579
  @again
  (ldxr imm0 (:@ imm1))                            ; ppc:581 lrarx
  (add imm0 imm0 imm2)                             ; ppc:582
  (stxr (:w temp4) imm0 (:@ imm1))                 ; ppc:583 strcx.
  (cbnz (:w temp4) @again)                          ; ppc:584
  (dmb (:$ 11))                                    ; ppc:585 isync
  (box-fixnum arg_z imm0)                           ; ppc:586
  (ret))                                           ; ppc:587

;;; =====================================================================
;;; %atomic-decf-ptr — ppc:589
;;; =====================================================================
(defarm64lapfunction %atomic-decf-ptr ((ptr arg_z))
  (macptr-ptr imm1 ptr)                            ; ppc:590
  @again
  (ldxr imm0 (:@ imm1))                            ; ppc:592 lrarx
  (sub imm0 imm0 (:$ 1))                           ; ppc:593 (subi raw 1)
  (stxr (:w temp4) imm0 (:@ imm1))                 ; ppc:594 strcx.
  (cbnz (:w temp4) @again)                          ; ppc:595
  (dmb (:$ 11))                                    ; ppc:596 isync
  (box-fixnum arg_z imm0)                           ; ppc:597
  (ret))                                           ; ppc:598

;;; =====================================================================
;;; %atomic-decf-ptr-if-positive — ppc:600
;;; =====================================================================
;;; cmp imm0 0 flags survive the flag-safe (sub) to b.eq (skip store when
;;; the loaded value was 0).  Note PPC boxes the ALREADY-decremented imm0 on
;;; the @done path (value was 0 → returns boxed -1) — preserved.
(defarm64lapfunction %atomic-decf-ptr-if-positive ((ptr arg_z))
  (macptr-ptr imm1 ptr)                            ; ppc:601
  @again
  (ldxr imm0 (:@ imm1))                            ; ppc:603 lrarx
  (cmp imm0 (:$ 0))                               ; ppc:604 (cmpri cr1)
  (sub imm0 imm0 (:$ 1))                           ; ppc:605 (subi 1 — flag-safe)
  (b.eq @done)                                     ; ppc:606 (beq) value was 0
  (stxr (:w temp4) imm0 (:@ imm1))                 ; ppc:607 strcx.
  (cbnz (:w temp4) @again)                          ; ppc:608
  (dmb (:$ 11))                                    ; ppc:609 isync
  (box-fixnum arg_z imm0)                           ; ppc:610
  (ret)                                            ; ppc:611
  @done
  (clrex)                                          ; ppc:613-615 reservation-discharge
  (box-fixnum arg_z imm0)                           ; ppc:614
  (ret))                                           ; ppc:616

;;; =====================================================================
;;; %atomic-swap-ptr — ppc:618
;;; =====================================================================
(defarm64lapfunction %atomic-swap-ptr ((ptr arg_y) (newval arg_z))
  (dmb (:$ 11))                                    ; ppc:619 (sync → dmb ish)
  (macptr-ptr imm1 ptr)                            ; ppc:620
  (unbox-fixnum imm2 arg_z)                         ; ppc:621 (newval)
  @again
  (ldxr imm0 (:@ imm1))                            ; ppc:623 lrarx
  (stxr (:w temp4) imm2 (:@ imm1))                 ; ppc:624 strcx.
  (cbnz (:w temp4) @again)                          ; ppc:625
  (dmb (:$ 11))                                    ; ppc:626 isync
  (box-fixnum arg_z imm0)                           ; ppc:627
  (ret))                                           ; ppc:628

;;; =====================================================================
;;; %ptr-store-conditional — ppc:632
;;; =====================================================================
(defarm64lapfunction %ptr-store-conditional ((ptr arg_x) (expected-oldval arg_y) (newval arg_z))
  (macptr-ptr imm0 ptr)                            ; ppc:633 (base)
  (unbox-fixnum imm1 expected-oldval)              ; ppc:634
  (unbox-fixnum imm2 newval)                        ; ppc:635
  @again
  (ldxr imm3 (:@ imm0))                            ; ppc:637 lrarx
  (cmp imm3 imm1)                                  ; ppc:638 cmpr
  (b.ne @done)                                     ; ppc:639 (bne-)
  (stxr (:w temp4) imm2 (:@ imm0))                 ; ppc:640 strcx.
  (cbnz (:w temp4) @again)                          ; ppc:641 (bne- @again)
  (dmb (:$ 11))                                    ; ppc:642 isync
  (box-fixnum arg_z imm3)                           ; ppc:643
  (ret)                                            ; ppc:644
  @done
  (clrex)                                          ; ppc:646-648 reservation-discharge
  (box-fixnum arg_z imm3)                           ; ppc:647
  (ret))                                           ; ppc:649

;;; =====================================================================
;;; %ptr-store-fixnum-conditional — ppc:651
;;; =====================================================================
;;; address=imm0, actual-oldval=imm1.  newval stored as-is (tagged fixnum).
(defarm64lapfunction %ptr-store-fixnum-conditional ((ptr arg_x) (expected-oldval arg_y) (newval arg_z))
  (macptr-ptr imm0 ptr)                            ; ppc:654 (base)
  @again
  (ldxr imm1 (:@ imm0))                            ; ppc:656 lrarx actual-oldval
  (cmp imm1 expected-oldval)                        ; ppc:657 cmpr
  (b.ne @done)                                     ; ppc:658 (bne-)
  (stxr (:w temp4) newval (:@ imm0))               ; ppc:659 strcx.
  (cbnz (:w temp4) @again)                          ; ppc:660 (bne- @again)
  (dmb (:$ 11))                                    ; ppc:661 isync
  (mov arg_z imm1)                                 ; ppc:662
  (ret)                                            ; ppc:663
  @done
  (clrex)                                          ; ppc:665-667 reservation-discharge
  (mov arg_z imm1)                                 ; ppc:666
  (ret))                                           ; ppc:668

;;; =====================================================================
;;; %fixnum-truncate — ppc-numbers.lisp:251 (#+ppc64-target arm; placed
;;; here like called-for-mv-p below, pending the numbers-file story vs
;;; Matt's own arm64-numbers.lisp).  Promoted 16m14 from the wave-2
;;; draft (drafts/arm64-numbers.lisp) on l1-lisp-threads demand; the
;;; draft's DECIDE sites resolved to since-ratified idioms:
;;; jump-subprim (PPC ba), load-nfn-constant (PPC `ld rd 'const nfn`).
;;; Returns (values quotient remainder).  PPC's divdo. OV-on-zero has no
;;; ARM64 analog: sdiv neither traps nor flags, so the unboxed divisor
;;; is cbz-tested BEFORE dividing.  PPC's (mtxer rzero) XER-clearing
;;; dropped (NZCV is not sticky).  negs supplies nego.'s V-on-overflow
;;; (dividend = most-negative-fixnum); the ldur in load-nfn-constant
;;; does not disturb NZCV, so the b.vc still tests the negs.
;;; =====================================================================
(defarm64lapfunction %fixnum-truncate ((dividend arg_y) (divisor arg_z))
  (let ((unboxed-quotient imm0)
        (unboxed-dividend imm1)
        (unboxed-divisor imm2)
        (unboxed-product imm3)
        (boxed-quotient temp1)
        (remainder temp2))
    ;; (cmpdi divisor '-1) — boxed fixnum -1 = raw -8; cmp's aimm is
    ;; unsigned, so compare via cmn #8 (Z iff divisor = -8).  ppc:259
    (cmn divisor (:$ (ash 1 arm64::fixnumshift)))
    (unbox-fixnum unboxed-dividend dividend)  ; ppc:260
    (unbox-fixnum unboxed-divisor divisor)    ; ppc:261
    (b.eq @neg)                               ; ppc:262
    (cbz unboxed-divisor @div-by-zero)        ; ppc:263 (divdo. OV half)
    (sdiv unboxed-quotient unboxed-dividend unboxed-divisor) ; ppc:263
    (box-fixnum boxed-quotient unboxed-quotient) ; ppc:264
    (mul unboxed-product unboxed-quotient unboxed-divisor) ; ppc:265 (mulld)
    (b @ok)                                   ; ppc:266 (bns+ @ok)
    @div-by-zero
    ;; ppc:267 (mtxer rzero) — dropped, no XER
    (save-lisp-context)                       ; ppc:268
    (set-nargs 3)                             ; ppc:269
    (load-constant arg_x truncate)            ; ppc:270
    (call-symbol divide-by-zero-error)        ; ppc:271 — does not return
    @ok                                       ; ppc:273
    (sub imm0 unboxed-dividend unboxed-product) ; ppc:274 (subf)
    (vpush boxed-quotient)                    ; ppc:275
    (box-fixnum remainder imm0)               ; ppc:276
    (vpush remainder)                         ; ppc:277
    (set-nargs 2)                             ; ppc:278
    ;; (la temp0 '2 vsp) — temp0 = entry vsp, the .SPvalues frame base
    (add temp0 vsp (:$ (* 2 arm64::fixnumone))) ; ppc:279
    (jump-subprim .SPvalues)                  ; ppc:280 (ba .SPvalues)
    @neg
    ;; ppc:282 (nego. dividend dividend) — V set iff dividend is
    ;; most-negative-fixnum (raw #x8000000000000000)
    (negs dividend dividend)
    (load-nfn-constant arg_z *least-positive-bignum*) ; ppc:283 (ld ... nfn)
    (b.vc @ret)                               ; ppc:284 (bns @ret)
    ;; ppc:285 (mtxer rzero) — dropped, no XER
    (ldur dividend (:@ arg_z (:$ arm64::symbol.vcell))) ; ppc:286
    @ret
    (mov temp0 vsp)                           ; ppc:288 (mr temp0 vsp)
    (vpush dividend)                          ; ppc:289
    ;; ppc:290 (vpush rzero) — raw 0 = boxed fixnum 0; xzr supplies it
    (vpush xzr)
    (set-nargs 2)                             ; ppc:291
    (jump-subprim .SPvalues)))                ; ppc:292 (ba .SPvalues)

;;; =====================================================================
;;; called-for-mv-p — ppc-numbers.lisp:296 (placed here pending the
;;; numbers-file story vs Matt's own arm64-numbers.lisp; the demand
;;; loop only needs it deployed).  Answer: was my caller invoked for
;;; multiple values — i.e. is its frame's savelr the magic ret1valaddr
;;; kernel global?
;;;
;;; 16m52 FIX — the nfp leg was WRONG since it was written, and only
;;; became reachable when patches/0084 added (! save-nfp) to
;;; arm642-lambda, so functions finally HAVE nfp frames (reflex 9: a
;;; fix that makes dead code live re-arms the landmines).  Symptom: the
;;; cross build went green and its image died in cold load with
;;;   unhandled read fault at pc 0x30000000aa90, fault address 0x405
;;; inside this function, called from TRUNCATE.  0x405 = 0x3ed + 24,
;;; where 0x3ed is a u64-vector header ((3<<8)|0xed), dereferenced as a
;;; pointer.  Watchpoint-confirmed writer: save-nfp's own
;;; `stp x0,x1,[sp,#-32]!' in TRUNCATE's prologue.
;;; comms/COLDLOAD-16m52.md has the disassembly and the green/broken A/B
;;; (this function is BYTE-IDENTICAL in both images; the producer moved).
;;;
;;; ARM64-DEVIATION: PPC's `(ldr imm1 0 imm1)' (ppc:302) reads word 0 of
;;; the nfp frame as the back-chain to the caller's frame.  That is
;;; correct on PPC, whose ABI mandates a back-chain word at [sp] --
;;; AAPCS64 has NO back-chain, and Matt's save-nfp
;;; (arm64-vinsns.lisp:53 at pin 9c61574) documents the layout as
;;;   [header] [saved tcr.nfp = element 0] [data...]
;;; with restore-nfp reading the link at [sp+8].  So word 0 is the
;;; HEADER, and the caller's lisp frame sits past the whole frame.  Skip
;;; it by its own size — 8 (header) + 8 * element-count, count being
;;; header>>8 — rather than by a hard-coded 32, so a deeper nfp frame
;;; (save-nfp is gated on arm642-max-nfp-depth) stays correct.
;;; NB the earlier comment here cited ARM32 arm-numbers.lisp:166 as the
;;; "nfp-design sibling" and a "16m5p header reshape" that guarantees a
;;; backlink slot.  ARM32's frame really does start with a backlink;
;;; ARM64's does not, and no such reshape exists in this tree's vinsn.
;;; =====================================================================
(defarm64lapfunction called-for-mv-p ()
  (ldr temp0 (:@ rcontext (:$ arm64::tcr.nfp)))    ; arm:167
  (add imm1 sp (:$ 0))                             ; ppc:297 mr imm1,sp
  (cmp temp0 imm1)                                 ; ppc:299
  (b.ne @nonfp)                                    ; ppc:301 (bne @notnfp)
  ;; ARM64-DEVIATION for ppc:302 — see the header block above.
  (ldr imm2 (:@ imm1 (:$ 0)))                      ; the u64-vector header
  (lsr imm2 imm2 (:$ 8))                           ; element count
  (lsl imm2 imm2 (:$ 3))                           ; * node-size = data bytes
  (add imm1 imm1 imm2)                             ; past header's elements
  (add imm1 imm1 (:$ 8))                           ; past the header word itself
  @nonfp
  (ldr imm1 (:@ imm1 (:$ 24)))                     ; ppc:304 lisp-frame.savelr (@24)
  (ref-global imm0 ret1valaddr)                    ; ppc:300
  (eq->boolean arg_z imm0 imm1 imm2)               ; ppc:305
  (ret))                                           ; ppc:306

;;; =====================================================================
;;; %truncate-short-float->fixnum / %round-nearest-short-float->fixnum —
;;; ppc-numbers.lisp:141/:17x (PPC64 branches).  NAMING (mail item):
;;; the level-0 callers use the PPC64 SHORT-float names; Matt's own
;;; arm64-numbers.lisp:49/:62 defines the same bodies under
;;; %truncate/%round-nearest-SINGLE-float->fixnum.  These twins carry
;;; HIS bodies verbatim under the canonical names until he ratifies one
;;; spelling (then one set dies).
;;; =====================================================================
(defarm64lapfunction %truncate-short-float->fixnum ((arg arg_z))
  (get-single-float-bits imm0 arg)      ; his arm64-numbers.lisp:50
  (fmov s0 (:w imm0))                   ; his :51
  (fcvtzs imm0 s0)                      ; his :52 (= ppc:142 fctidz)
  (box-fixnum arg_z imm0)               ; ppc:148
  (ret))                                ; ppc:149

(defarm64lapfunction %round-nearest-short-float->fixnum ((arg arg_z))
  (get-single-float-bits imm0 arg)      ; his arm64-numbers.lisp:63
  (fmov s0 (:w imm0))                   ; his :64
  (fcvtns imm0 s0)                      ; his :65 (round-to-nearest-even)
  (box-fixnum arg_z imm0)
  (ret))

;;; =====================================================================
;;; %heap-bytes-allocated — ppc:285 (#+ppc64-target; the ppc32 twin @270 is
;;; the same body over a hi/lo pair and is SKIPped)
;;;
;;; Same class as VALUES below and as 16m44's %ffi-exception-status: a
;;; function every other port defines in its per-arch level-0 file, with no
;;; arm64 arm anywhere (his tree, this overlay, the patches).  Cost: ANSI
;;; TIME.1-8, all eight of them, because CL:TIME reports through
;;; l0-misc.lisp's %heap-bytes-allocated and the tests assert that TIME
;;; WRITES to *trace-output* -- so an undefined function there is eight
;;; failures that say nothing about arm64.  Found by
;;; tools/probes/ansi-tail-77-clusters.lisp, which named it directly.
;;;
;;; total_bytes_allocated is only current as of the last allocation-pointer
;;; reset, so the live delta (last_allocptr - allocptr) has to be added back
;;; -- unless there is no such delta to add:
;;;   last_allocptr == 0            no allocation since the last reset
;;;   allocptr == VOID_ALLOCPTR     the allocator is disabled
;;; PPC64 spends two condition registers on those two tests; one NZCV does
;;; here, serialized (cbz, then cmn).
;;;
;;; VOID_ALLOCPTR is -dnode_size = -16 on arm64 (gc.h:126); the 0x8000...-16
;;; spelling at gc.h:124 is PPC64-only.  So `cmn allocptr #16' is right by
;;; OUR arch, not by copying PPC's immediate -- which happens to agree.
;;;
;;; tcr.total-bytes-allocated is a single 64-bit slot here
;;; (arm64-arch.lisp:762);
;;; PPC64 reads its `-high' half, which IS the whole count on a 64-bit
;;; machine.
;;;
;;; Promoted from upstream-port/level-0/drafts/arm64-misc.lisp:214 with the
;;; one correction that draft promotion always needs on this lane: the draft
;;; ended in (call-subprim .SPmakeu64), which LINKS, where ppc:295 is
;;; `ba .SPmakeu64' -- a tail transfer, so .SPmakeu64's return goes to our
;;; caller.  Getting that wrong is what produced the stage-11 udf #0
;;; regression in apply+, and tools/draft-tail-subprim-lint.py exists to
;;; name these sites; it named this one.
;;; =====================================================================
(defarm64lapfunction %heap-bytes-allocated ()
  (ldr imm2 (:@ rcontext (:$ arm64::tcr.last-allocptr)))         ; ppc:286
  (ldr imm0 (:@ rcontext (:$ arm64::tcr.total-bytes-allocated))) ; ppc:289
  (cbz imm2 @go)                        ; ppc:291 (beq cr1) last-allocptr == 0
  (cmn allocptr (:$ 16))                ; ppc:288 (cmpri allocptr,-16)
  (b.eq @go)                            ; ppc:292 (beq) allocptr == VOID_ALLOCPTR
  (sub imm2 imm2 allocptr)              ; ppc:290
  (add imm0 imm0 imm2)                  ; ppc:293
  @go
  (jump-subprim .SPmakeu64))            ; ppc:295 (ba .SPmakeu64), imm0 in

;;; =====================================================================
;;; values — ppc:298
;;;
;;; VALUES had NO arm64 definition at all.  It is a per-architecture LAP
;;; function on every other backend -- ppc-misc.lisp:298,
;;; ARM/arm-misc.lisp:577, X86/x86-misc.lisp:394,
;;; X86/X8632/x8632-misc.lisp:278 -- and neither Matt's tree (which has no
;;; level-0/ARM64/arm64-misc.lisp) nor this overlay nor any patch supplied
;;; one.  So (fboundp 'values) answered NIL in the running image, observed
;;; live via tools/probes/ansi-aux-missing-defs.lisp.
;;;
;;; Why that was invisible until the ANSI suite: VALUES in operator position
;;; is handled by the compiler, so (values a b) is fine everywhere and the
;;; whole boot and REPL ladder never needed the FUNCTION.  Only #'values
;;; does, and in compiled code that lowers to (%function 'values), resolved
;;; against the fbinding -- hence "Undefined function VALUES".
;;;
;;; This is what stages 8 and 9 were both reporting.  ansi-aux.lsp's
;;; eqt/eqlt/equalt/equalpt are each (apply #'values (mapcar #'notnot
;;; (multiple-value-list ...))), so loading its fasl dies on the first of
;;; them at line 74 and every later definition in the file -- including
;;; def-fold-test at line 1172 -- is never installed.  That is why stage 8
;;; said "Unbound variable: CL-TEST::CONS.FOLD.1": def-fold-test was not a
;;; macro, so (def-fold-test cons.fold.1 ...) read as a function call and
;;; evaluated its first argument as a variable.
;;;
;;; PPC64 is the line-port reference and ARM32 is identical to it here.
;;; Promoted from upstream-port/level-0/drafts/arm64-misc.lisp:232 with one
;;; correction: the draft ended in (call-subprim .SPvalues), which is `blr`
;;; and LINKS.  PPC is `ba .SPvalues` -- a tail transfer, so .SPvalues'
;;; return goes to VALUES' own caller -- which on this lane is
;;; (jump-subprim ...), the spelling the rest of this file already uses for
;;; ppc `ba` (see %store-node-conditional and the two .SPvalues jumps in
;;; %fixnum-truncate).
;;;
;;; nargs is SCALED (a byte count, count << fixnumshift) -- vpush-argregs
;;; compares it against (ash 2 arm64::fixnumshift) directly -- and
;;; vpush-argregs has just decremented vsp by exactly that many bytes, so
;;; nargs + vsp recovers the ENTRY vsp, which is .SPvalues' documented
;;; temp0 input (the frame base).
;;; =====================================================================
(defarm64lapfunction values ()
  (:arglist (&rest values))
  (vpush-argregs)                                 ; ppc:300
  (add temp0 nargs vsp)                           ; ppc:301 — temp0 = entry vsp
  (jump-subprim .SPvalues))                       ; ppc:302 (ba .SPvalues)

;;; =====================================================================
;;; %setf-macptr-to-object — ppc:307 (#+ppc-target: covers ppc32 & ppc64)
;;; =====================================================================
(defarm64lapfunction %setf-macptr-to-object ((macptr arg_y) (object arg_z))
  (check-nargs 2)                                 ; ppc:308
  (trap-unless-typecode= arg_y arm64::subtag-macptr) ; ppc:309 (DECIDE-8 brk)
  (stur arg_z (:@ arg_y (:$ arm64::macptr.address))) ; ppc:310 (str→stur, addr=-4)
  (ret))                                          ; ppc:311

;;; =====================================================================
;;; %fixnum-from-macptr — ppc:313 (parity twin of the setter above)
;;; =====================================================================
(defarm64lapfunction %fixnum-from-macptr ((macptr arg_z))
  (check-nargs 1)                                 ; ppc:314
  (trap-unless-typecode= arg_z arm64::subtag-macptr) ; ppc:315
  (ldur imm0 (:@ arg_z (:$ arm64::macptr.address))) ; ppc:316 (ldr→ldur)
  (trap-unless-lisptag= imm0 arm64::tag-fixnum imm1) ; ppc:317 (DECIDE-8 brk)
  (mov arg_z imm0)                                 ; ppc:318
  (ret))                                          ; ppc:319

;;; =====================================================================
;;; fudge-heap-pointer — ppc:871 (#+ppc64-target; ppc32 twin @857 SKIP)
;;; =====================================================================
;;; Builds an ivector header inside a malloc'd block (16m15 demand:
;;; %make-heap-ivector at *terminal-io* io-buffer setup).  clrrdi (clear
;;; low 4 bits) → and ~15 (ldb-wrapped logimm).  subf rD,rA,rB = rB-rA.
;;; sth halfword store → sturh (W-src, unscaled -2).  Header goes at the
;;; untagged base (misc-header-offset = -fulltag-misc in his layout, so
;;; [base+0] = [tagged-12]); MATCHED PAIR with %%make-disposable's
;;; delta-halfword readback below.
(defarm64lapfunction fudge-heap-pointer ((ptr arg_x) (subtype arg_y) (len arg_z))
  (check-nargs 3)                                 ; ppc:872
  (macptr-ptr imm1 ptr)                            ; ppc:873 (address)
  (add imm0 imm1 (:$ 17))                          ; ppc:874 (+17: 2 delta + 15 align)
  (and imm0 imm0 (:$ (ldb (byte 64 0) (lognot 15)))) ; ppc:875 (clrrdi 4)
  (sub imm1 imm0 imm1)                             ; ppc:876 (subf: delta = imm0-imm1)
  (sturh (:w imm1) (:@ imm0 (:$ -2)))              ; ppc:877 (sth delta @[base-2])
  (unbox-fixnum imm1 subtype)                       ; ppc:878
  (lsl imm2 len (:$ (- arm64::num-subtag-bits arm64::fixnumshift))) ; ppc:879 (sldi 5)
  (orr imm1 imm2 imm1)                             ; ppc:880
  (str imm1 (:@ imm0 (:$ 0)))                      ; ppc:881 (std header word)
  (add arg_z imm0 (:$ arm64::fulltag-misc))        ; ppc:882 (tag it)
  (ret))                                          ; ppc:883

;;; =====================================================================
;;; %%make-disposable — ppc:885 (MATCHED PAIR with fudge-heap-pointer)
;;; =====================================================================
(defarm64lapfunction %%make-disposable ((ptr arg_y) (vector arg_z))
  (check-nargs 2)                                 ; ppc:886
  (sub imm0 vector (:$ arm64::fulltag-misc))       ; ppc:887 (addr = vect less tag)
  (ldurh (:w imm1) (:@ imm0 (:$ -2)))              ; ppc:888 (lhz delta halfword)
  (sub imm0 imm0 imm1)                             ; ppc:889 (orig addr = addr - delta)
  (stur imm0 (:@ ptr (:$ arm64::macptr.address)))  ; ppc:890 (str→stur)
  (ret))                                          ; ppc:891

;;; =====================================================================
;;; %vect-data-to-macptr — ppc:913 (#+ppc64-target; ppc32 twin @894 SKIP)
;;; =====================================================================
;;; Promotion fix vs the wave-5 draft: la with negative misc-data-offset
;;; → sub (the ratified idiom; add with a negative imm does not encode).
(defarm64lapfunction %vect-data-to-macptr ((vect arg_y) (ptr arg_z))
  (sub imm0 vect (:$ (- arm64::misc-data-offset))) ; ppc:914 (la → sub)
  (stur imm0 (:@ ptr (:$ arm64::macptr.address)))  ; ppc:915 (std → stur)
  (ret))                                          ; ppc:916

;;; =====================================================================
;;; %misc-address-fixnum / %ivector-from-macptr — ppc:850 / :928
;;; (16m48h promotion.)  The two remaining members of the address-arithmetic
;;; set whose other members (%vect-data-to-macptr above, %fixnum-from-macptr,
;;; %setf-macptr-to-object) are already here.
;;; =====================================================================
;;; BOTH needed the negative-immediate flip, and both drafts had it wrong in
;;; opposite directions — the reason tools/lap-negative-immediate-lint.py now
;;; exists.  On this low-tag design misc-data-offset is -4 and
;;; (- fulltag-misc node-size) is +4, and AArch64's add/sub immediate field is
;;; UNSIGNED, so the sign has to live in the choice of mnemonic:
;;;   * PPC's (la arg_z misc-data-offset misc-object) is a signed -4 =>
;;;     (sub … (:$ (- misc-data-offset))).  The draft wrote `add' of -4.
;;;   * PPC64's (addi arg_z imm0 (- fulltag-misc node-size)) is a signed +4 =>
;;;     (add … (:$ (- fulltag-misc node-size))).  The draft wrote the
;;;     algebraically equal but unencodable `sub' of (- node-size fulltag-misc).
;;; Ratified idiom reference: :610-612 below and arm64-def.lisp:288-292.
(defarm64lapfunction %misc-address-fixnum ((misc-object arg_z))
  (check-nargs 1)                                  ; ppc:851
  (sub arg_z misc-object (:$ (- arm64::misc-data-offset))) ; ppc:852 (la → sub)
  (ret))                                           ; ppc:853

;;; PPC64's #+ppc64-target arm (ppc:928); the ppc32 twin at :919 has to mask
;;; the pointer to a node boundary first and is skipped.
(defarm64lapfunction %ivector-from-macptr ((ptr arg_z))
  (macptr-ptr imm0 ptr)                            ; ppc:929
  (add arg_z imm0 (:$ (- arm64::fulltag-misc arm64::node-size))) ; ppc:930 (addi +4)
  (ret))                                           ; ppc:931

;;; =====================================================================
;;; %macptr->dead-macptr — ppc:673 (16m48h promotion).  Exact mirror of
;;; %revive-macptr (arm64-utils.lisp), which is already promoted and uses the
;;; same sturb through misc-subtag-offset; this one writes the dead subtag
;;; instead of the live one.
;;; =====================================================================
;;; subtag-dead-macptr comes from (define-subtag dead-macptr
;;; ivector-class-64-bit 2) at arm64-arch.lisp:145, one value above
;;; subtag-macptr's (… 1) at :144.  This is a LITTLE-endian low-tag design, so
;;; the subtag is the header's LOW byte and misc-subtag-offset ==
;;; misc-header-offset == -12 (arm64-arch.lisp:251-253) — contrast PPC64,
;;; where big-endian puts it at header+7.  -12 is not 8-aligned, hence sturb.
(defarm64lapfunction %macptr->dead-macptr ((macptr arg_z))
  (check-nargs 1)                                  ; ppc:674
  (mov imm0 (:$ arm64::subtag-dead-macptr))        ; ppc:675 (li)
  (sturb (:w imm0) (:@ macptr (:$ arm64::misc-subtag-offset))) ; ppc:676 (stb)
  (ret))                                           ; ppc:677

;;; =====================================================================
;;; %suspend-other-threads / %resume-other-threads — ppc:1006/:1018
;;; (16m5w demand #3, via walk-dynamic-area's with-other-threads-
;;; suspended).  uuo-interr kernel services (PPC UUO_INTERR ppc:1400/
;;; 1406) are LIVE in arm64-exceptions.c (UUO_MISC_IS_INTERR dispatch);
;;; the lapmacro emits the PROPOSED misc-format interr encoding.
;;; =====================================================================
(defarm64lapfunction %suspend-other-threads ()
  (check-nargs 0)                                 ; ppc:1007
  (uuo-interr arch::error-suspend-all)            ; ppc:1008
  (mov arg_z rnil)                                ; ppc:1009 (li nil)
  (ret))                                          ; ppc:1010

(defarm64lapfunction %resume-other-threads ()
  (check-nargs 0)                                 ; ppc:1019
  (uuo-interr arch::error-resume-all)             ; ppc:1020
  (mov arg_z rnil)                                ; ppc:1021 (li nil)
  (ret))                                          ; ppc:1022

;;; =====================================================================
;;; Per-TCR thread control — ppc:994/1000/1012/1024 (16m48h promotion).
;;; The four per-target siblings of the two -other-threads functions above.
;;; =====================================================================
;;; KERNEL SIDE VERIFIED, all four, in the same UUO_INTERR dispatch the pair
;;; above already goes through (arm64-exceptions.c:1638-1670): it reads
;;;   TCR *target = (TCR *)xpGPR(xp,arg_z);            (:1641, ppc:1389)
;;; and services error_interrupt -> raise_thread_interrupt (:1646),
;;; error_suspend -> lisp_suspend_tcr (:1649), error_resume ->
;;; lisp_resume_tcr (:1655), error_kill -> kill_tcr (:1661), each writing its
;;; result back into imm0 — exactly the calling convention these bodies use.
;;; The errnums are arch.lisp:61/62/64/66 (11/12/14/16).
;;;
;;; ne0->boolean (drafts/arm64-lapmacros-additions.lisp:463) is PPC's
;;; ne0->boolean; the imm0 result is a C int, not a lisp object, so it must be
;;; turned into T/NIL rather than returned.  %%tcr-interrupt is the exception:
;;; its result is a real count, so it is boxed.
(defarm64lapfunction %%tcr-interrupt ((target arg_z))
  (check-nargs 1)                                 ; ppc:995
  (uuo-interr arch::error-interrupt)              ; ppc:996
  (box-fixnum arg_z imm0)                         ; ppc:997
  (ret))                                          ; ppc:998

(defarm64lapfunction %suspend-tcr ((target arg_z))
  (check-nargs 1)                                 ; ppc:1001
  (uuo-interr arch::error-suspend)                ; ppc:1002
  (ne0->boolean arg_z imm0 imm1)                  ; ppc:1003
  (ret))                                          ; ppc:1004

(defarm64lapfunction %resume-tcr ((target arg_z))
  (check-nargs 1)                                 ; ppc:1013
  (uuo-interr arch::error-resume)                 ; ppc:1014
  (ne0->boolean arg_z imm0 imm1)                  ; ppc:1015
  (ret))                                          ; ppc:1016

(defarm64lapfunction %kill-tcr ((target arg_z))
  (check-nargs 1)                                 ; ppc:1025
  (uuo-interr arch::error-kill)                   ; ppc:1026
  (ne0->boolean arg_z imm0 imm1)                  ; ppc:1027
  (ret))                                          ; ppc:1028

;;; =====================================================================
;;; %check-deferred-gc — ppc:985 (16m48h promotion)
;;; =====================================================================
;;; ARM64-DEVIATION (idiom only, same predicate): PPC shifts the
;;; pending-suspend flag bit up to the sign bit with `slri.' and then tests
;;; the sign with bgelr.  AArch64 tests the bit where it lies, with tbz —
;;; the natural idiom, and what x86-64 does too with btq (x86-misc.lisp:853).
;;; The bit position is (+ tcr-flag-bit-pending-suspend fixnumshift) exactly as
;;; PPC computes it, because tcr.flags holds a BOXED fixnum.
;;;
;;; The draft's `(brk (:$ #xf00a))' placeholder becomes the real
;;; (uuo-interr arch::error-propagate-suspend): the errnum is arch.lisp:60
;;; (=10) and the kernel has a real case for it at arm64-exceptions.c:1644,
;;; which — faithfully to PPC (ppc:1392-1393) — does nothing but let the
;;; pending suspend be taken on the way out of the handler.
(defarm64lapfunction %check-deferred-gc ()
  (ldr imm0 (:@ rcontext (:$ arm64::tcr.flags)))  ; ppc:986
  (mov arg_z rnil)                                ; ppc:988 (li nil)
  (tbz imm0 (:$ (+ arch::tcr-flag-bit-pending-suspend arm64::fixnumshift)) @done)
  (uuo-interr arch::error-propagate-suspend)      ; ppc:990
  (add arg_z rnil (:$ arm64::t-offset))           ; ppc:991 (li arg_z t)
  @done
  (ret))                                          ; ppc:992

;;; =====================================================================
;;; 16m5y copy cluster — promoted from drafts/arm64-misc.lisp (wave-5
;;; vetted) with the negative-immediate flips: misc-data-offset = -4 is
;;; not an encodable add immediate, so (add X Y (:$ misc-data-offset))
;;; becomes (sub X Y (:$ (- misc-data-offset))) — the active-seed
;;; convention (arm64-array.lisp:312 etc.).  Register-offset ldrb/strb
;;; take the possibly-negative computed offset in a REGISTER (fine).
;;; =====================================================================

;;; =====================================================================
;;; %copy-ptr-to-ivector — ppc:29 (mirror of %copy-ivector-to-ptr: src is
;;; a macptr, dest an ivector).
;;; =====================================================================
(defarm64lapfunction %copy-ptr-to-ivector ((src (* 1 arm64::node-size))
                                           (src-byte-offset 0)
                                           (dest arg_x)
                                           (dest-byte-offset arg_y)
                                           (nbytes arg_z))
  (ldr temp0 (:@ vsp (:$ arm64::node-size)))       ; ppc:41 src macptr node
  (ldur imm0 (:@ temp0 (:$ arm64::macptr.address))) ; ppc:42 macptr-ptr → address
  (ldr imm1 (:@ vsp (:$ 0)))                        ; ppc:43 src-byte-offset (boxed)
  (unbox-fixnum imm1 imm1)                           ; ppc:44
  (unbox-fixnum imm2 dest-byte-offset)               ; ppc:45
  (sub imm2 imm2 (:$ (- arm64::misc-data-offset)))   ; ppc:46 la dest-byteptr
  (cmp nbytes (:$ 0))                                ; ppc:40
  (b @test)                                          ; ppc:47
  @loop
  (subs nbytes nbytes (:$ (ash 1 arm64::fixnumshift))) ; ppc:49/50
  (ldrb (:w imm3) (:@ imm0 imm1))                    ; ppc:51 lbzx
  (add imm1 imm1 (:$ 1))                             ; ppc:52
  (strb (:w imm3) (:@ dest imm2))                    ; ppc:53 stbx
  (add imm2 imm2 (:$ 1))                             ; ppc:54
  @test
  (b.ne @loop)                                       ; ppc:56
  (mov arg_z dest)                                   ; ppc:57
  (add vsp vsp (:$ (ash 2 arm64::fixnumshift)))      ; ppc:58
  (ret))

;;; =====================================================================
;;; %copy-ivector-to-ptr — ppc:61
;;; =====================================================================
(defarm64lapfunction %copy-ivector-to-ptr ((src (* 1 arm64::node-size))
                                           (src-byte-offset 0)
                                           (dest arg_x)
                                           (dest-byte-offset arg_y)
                                           (nbytes arg_z))
  (ldr temp0 (:@ vsp (:$ arm64::node-size)))     ; ppc:66 src ivector
  (ldr imm0 (:@ vsp (:$ 0)))                      ; ppc:68 src-byte-offset
  (unbox-fixnum imm0 imm0)                         ; ppc:69
  (sub imm0 imm0 (:$ (- arm64::misc-data-offset)))     ; ppc:70
  (unbox-fixnum imm2 dest-byte-offset)             ; ppc:71
  (ldur imm1 (:@ dest (:$ arm64::macptr.address))) ; ppc:72 (macptr.address=4 → ldur)
  (cmp nbytes (:$ 0))                              ; ppc:67
  (b @test)                                        ; ppc:73
  @loop
  (subs nbytes nbytes (:$ (ash 1 arm64::fixnumshift))) ; ppc:75/76
  (ldrb (:w imm3) (:@ temp0 imm0))                 ; ppc:77 lbzx
  (add imm0 imm0 (:$ 1))                           ; ppc:78
  (strb (:w imm3) (:@ imm1 imm2))                  ; ppc:79 stbx
  (add imm2 imm2 (:$ 1))                           ; ppc:80
  @test
  (b.ne @loop)                                     ; ppc:82
  (mov arg_z dest)                                 ; ppc:83
  (add vsp vsp (:$ (ash 2 arm64::fixnumshift)))    ; ppc:84
  (ret))

;;; =====================================================================
;;; %copy-ivector-to-ivector — ppc:163 (#+ppc64-target; ppc32 twin @88 SKIP)
;;; =====================================================================
;;; Overlap-aware byte copy.  Three PPC CR fields: cr0 (loop count), cr1
;;; (src vs dest), cr2 (offsets).  Single-NZCV: cr1/cr2 branch decisions
;;; are resolved UP FRONT (sequentially) before either byte loop, which
;;; then use their own (cmp nbytes 0).  cmpd/cmpdi are SIGNED → b.lt/b.ge.
(defarm64lapfunction %copy-ivector-to-ivector ((src-offset 8)
                                               (src-byte-offset-offset 0)
                                               (dest arg_x)
                                               (dest-byte-offset arg_y)
                                               (nbytes arg_z))
  (sub nbytes nbytes (:$ (ash 1 arm64::fixnumshift))) ; ppc:170 predecrement
  (ldr imm0 (:@ vsp (:$ 0)))                      ; ppc:171 src-byte-offset (boxed)
  (ldr temp0 (:@ vsp (:$ arm64::node-size)))      ; ppc:173 src
  (add vsp vsp (:$ (ash 2 arm64::fixnumshift)))   ; ppc:174 (la vsp '2)
  (cmp temp0 dest)                                ; ppc:175 cmpd cr1 (src==dest?)
  (b.ne @setup-fwd)                               ; ppc:181 (bne cr1) different → forward
  (cmp imm0 dest-byte-offset)                     ; ppc:176 cmpdi cr2 (offsets, SIGNED)
  (b.eq @done)                                    ; ppc:183 (beq cr2) same vec+off → nothing
  (b.lt @setup-back)                              ; ppc:184 (blt cr2) src-off<dest-off → backward
  @setup-fwd
  (unbox-fixnum imm0 imm0)                         ; ppc:177
  (unbox-fixnum imm1 dest-byte-offset)             ; ppc:178
  (sub imm0 imm0 (:$ (- arm64::misc-data-offset)))     ; ppc:179
  (sub imm1 imm1 (:$ (- arm64::misc-data-offset)))     ; ppc:180
  (b @test)                                        ; ppc:185
  @loop
  (sub nbytes nbytes (:$ (ash 1 arm64::fixnumshift))) ; ppc:187
  (ldrb (:w imm3) (:@ temp0 imm0))                 ; ppc:188 lbzx
  (add imm0 imm0 (:$ 1))                           ; ppc:190
  (strb (:w imm3) (:@ dest imm1))                  ; ppc:191 stbx
  (add imm1 imm1 (:$ 1))                           ; ppc:192
  @test
  (cmp nbytes (:$ 0))                              ; ppc:189 cmpdi
  (b.ge @loop)                                     ; ppc:194 (bge)
  @done
  (mov arg_z dest)                                 ; ppc:196
  (ret)                                            ; ppc:197
  @setup-back
  (unbox-fixnum imm0 imm0)                         ; ppc:177 (shared setup)
  (unbox-fixnum imm1 dest-byte-offset)             ; ppc:178
  (sub imm0 imm0 (:$ (- arm64::misc-data-offset)))     ; ppc:179
  (sub imm1 imm1 (:$ (- arm64::misc-data-offset)))     ; ppc:180
  (unbox-fixnum imm2 nbytes)                        ; ppc:200
  (add imm0 imm0 imm2)                              ; ppc:201
  (add imm1 imm1 imm2)                              ; ppc:202
  (b @back-test)                                    ; ppc:203
  @back-loop
  (sub nbytes nbytes (:$ (ash 1 arm64::fixnumshift))) ; ppc:205
  (ldrb (:w imm3) (:@ temp0 imm0))                 ; ppc:206 lbzx
  (sub imm0 imm0 (:$ 1))                           ; ppc:208
  (strb (:w imm3) (:@ dest imm1))                  ; ppc:209 stbx
  (sub imm1 imm1 (:$ 1))                           ; ppc:210
  @back-test
  (cmp nbytes (:$ 0))                              ; ppc:207
  (b.ge @back-loop)                                ; ppc:212
  (mov arg_z dest)                                 ; ppc:213
  (ret))

;;; =====================================================================
;;; %copy-gvector-to-gvector — ppc:217
;;; =====================================================================
;;; Node (8-byte) elements.  Boxed element index == byte offset (fixnumshift
;;; == word-shift == 3), so misc-data-offset arithmetic matches PPC verbatim.
;;; ldrx/strx → regoff (:@ base index).  cr1 (src vs dest) SIGNED; cr2
;;; (elements) SIGNED.  Same single-NZCV restructuring as the ivector twin.
(defarm64lapfunction %copy-gvector-to-gvector ((src (* 1 arm64::node-size))
                                               (src-element 0)
                                               (dest arg_x)
                                               (dest-element arg_y)
                                               (nelements arg_z))
  (sub nelements nelements (:$ (ash 1 arm64::fixnumshift))) ; ppc:222 predecrement
  (ldr imm0 (:@ vsp (:$ 0)))                      ; ppc:224 src-element (boxed)
  (ldr temp0 (:@ vsp (:$ arm64::node-size)))      ; ppc:225 src
  (add vsp vsp (:$ (ash 2 arm64::fixnumshift)))   ; ppc:226
  (cmp temp0 dest)                                ; ppc:227 cmpr cr1
  (b.ne @setup-fwd)                               ; ppc:231 (bne cr1)
  (cmp imm0 dest-element)                         ; ppc:228 cmpri cr2 (SIGNED)
  (b.eq @done)                                    ; ppc:233 (beq cr2)
  (b.lt @setup-back)                              ; ppc:234 (blt cr2)
  @setup-fwd
  (sub imm0 imm0 (:$ (- arm64::misc-data-offset)))     ; ppc:229
  (sub imm1 dest-element (:$ (- arm64::misc-data-offset))) ; ppc:230
  (b @test)                                        ; ppc:235
  @loop
  (sub nelements nelements (:$ (ash 1 arm64::fixnumshift))) ; ppc:237
  (ldr temp1 (:@ temp0 imm0))                      ; ppc:239 ldrx
  (add imm0 imm0 (:$ (ash 1 arm64::fixnumshift)))  ; ppc:240 (addi '1 = one node)
  (str temp1 (:@ dest imm1))                       ; ppc:241 strx
  (add imm1 imm1 (:$ (ash 1 arm64::fixnumshift)))  ; ppc:242
  @test
  (cmp nelements (:$ 0))                           ; ppc:238
  (b.ge @loop)                                     ; ppc:244
  @done
  (mov arg_z dest)                                 ; ppc:246
  (ret)                                            ; ppc:247
  @setup-back
  (sub imm0 imm0 (:$ (- arm64::misc-data-offset)))     ; ppc:229 (shared setup)
  (sub imm1 dest-element (:$ (- arm64::misc-data-offset))) ; ppc:230
  (add imm1 imm1 nelements)                        ; ppc:250 (add imm1 nelements imm1)
  (add imm0 imm0 nelements)                        ; ppc:251
  (b @back-test)                                    ; ppc:252
  @back-loop
  (sub nelements nelements (:$ (ash 1 arm64::fixnumshift))) ; ppc:254
  (ldr temp1 (:@ temp0 imm0))                      ; ppc:256 ldrx
  (sub imm0 imm0 (:$ (ash 1 arm64::fixnumshift)))  ; ppc:257
  (str temp1 (:@ dest imm1))                       ; ppc:258 strx
  (sub imm1 imm1 (:$ (ash 1 arm64::fixnumshift)))  ; ppc:259
  @back-test
  (cmp nelements (:$ 0))                           ; ppc:255
  (b.ge @back-loop)                                ; ppc:261
  (mov arg_z dest)                                 ; ppc:262
  (ret))

;;; =====================================================================
;;; %lock-gc-lock — ppc:517
;;; =====================================================================
;;; Atomically incf (or decf if negative) gc-inhibit-count.  PPC has NO
;;; isync here (commented out) — omitted faithfully.  cmp flags (arg_y vs 0)
;;; survive the flag-safe add to b.ge.
(defarm64lapfunction %lock-gc-lock ()
  (sub imm0 rnil (:$ (- (arm64::%kernel-global 'gc-inhibit-count)))) ; ppc:518 (&global)
  @again
  (ldxr arg_y (:@ imm0))                           ; ppc:520 lrarx
  (cmp arg_y (:$ 0))                               ; ppc:521 (cmpri cr1)
  (add arg_z arg_y (:$ (ash 1 arm64::fixnumshift))) ; ppc:522 (addi '1 — flag-safe)
  (b.ge @store)                                    ; ppc:523 (bge cr1)
  (sub arg_z arg_y (:$ (ash 1 arm64::fixnumshift))) ; ppc:524 (subi '1)
  @store
  (stxr (:w temp4) arg_z (:@ imm0))                ; ppc:526 strcx.
  (cbnz (:w temp4) @again)                          ; ppc:527
  (ret))                                           ; ppc:529 (no isync — see ppc:528)

;;; =====================================================================
;;; %unlock-gc-lock — ppc:534
;;; =====================================================================
;;; cr1 = (arg_y vs -1) via (cmn arg_y #1).  cbnz/stxr do NOT touch NZCV,
;;; so the last iteration's cmn flags survive to the post-loop (b.ne) that
;;; decides whether to fire the immediate-GC trap (DECIDE-3).
(defarm64lapfunction %unlock-gc-lock ()
  (sub imm0 rnil (:$ (- (arm64::%kernel-global 'gc-inhibit-count)))) ; ppc:536 (&global)
  @again
  (ldxr arg_y (:@ imm0))                           ; ppc:538 lrarx
  (cmn arg_y (:$ (ash 1 arm64::fixnumshift)))      ; ppc:539 (cmpri cr1 arg_y -1)
  (sub arg_z arg_y (:$ (ash 1 arm64::fixnumshift))) ; ppc:540 (subi '1 — flag-safe)
  (b.gt @store)                                    ; ppc:541 (bgt cr1)
  (add arg_z arg_y (:$ (ash 1 arm64::fixnumshift))) ; ppc:542 (addi '1)
  @store
  (stxr (:w temp4) arg_z (:@ imm0))                ; ppc:544 strcx.
  (cbnz (:w temp4) @again)                          ; ppc:545
  (b.ne @done)                                     ; ppc:546 (bnelr cr1) arg_y!=-1 → return
  ;; count went -1 -> 0: try an immediate GC.
  (mov imm0 (:$ arch::gc-trap-function-immediate-gc)) ; ppc:549 (li -1 → movn)
  (uuo-gc-trap)                                    ; ppc:550 (trlgei → DECIDE-3; args imm1)
  @done
  (ret))                                           ; ppc:551

;;; =====================================================================
;;; %fixnum-gcd — ppc-numbers.lisp:355 (the #+ppc64-target arm; the
;;; #+ppc32-target twin at ppc:310 is skipped).  Placed here, not in a
;;; replacement for Matt's own level-0/ARM64/arm64-numbers.lisp, per the
;;; %fixnum-truncate / called-for-mv-p precedent above: our overlay files
;;; are COPIED OVER his tree, so owning a file he owns would mean carrying
;;; his 8 LAP functions forward by hand at every pin advance.
;;;
;;; PROMOTED 16m34 from the wave-2 draft (drafts/arm64-numbers.lisp:164) on
;;; LIVE REPL demand: (/ 1 3) died with "Undefined function
;;; CCL::%FIXNUM-GCD called with arguments (1 3)", so no ratio could be
;;; constructed.  Callers: l0-numbers.lisp:2066, l0-bignum64.lisp:892/:1998.
;;;
;;; Binary GCD.  n1, n2 must be positive nonzero fixnums (PPC's contract,
;;; inherited — the ctz idiom below is undefined at 0).
;;; ut0/vt0 = trailing-zero counts + fixnumshift, so the final left shift
;;; re-boxes the result; u/v = odd parts.
;;;
;;; PPC keeps TWO compares live: cmpw cr2 ut0 vt0 BEFORE the loop and the
;;; cr0 u/v compare inside it, resolving cr2 only at the equality exit
;;; (blelr cr2 ⇒ shift by ut0, else fall through and shift by vt0).
;;; AArch64 has ONE NZCV, so the cr2 result is materialized before the loop:
;;;   (cmp ut0 vt0) (csel ut0 ut0 vt0 (:? le))   ; ut0 := min(ut0, vt0)
;;; after which the equality exit is just the speculative (lsl arg_z u ut0)
;;; already in flight, and PPC's second sld + blr collapse into one (ret).
;;; VETTED equivalent: PPC's ut0 is read ONLY by that speculative shift and
;;; by the blelr it resolves, vt0 ONLY by the else-shift, and neither is
;;; written inside the loop — so pre-selecting the min changes the selection
;;; POINT, not the value.  cmpw is a 32-bit compare but both operands are
;;; ctz+3 ∈ [3,66], so the 64-bit cmp is equivalent.
;;;
;;; ctz idiom: PPC (neg temp u) (and temp temp u) (cntlzd) (subfic 63) =
;;; 63 - clz(u & -u) = ctz(u).  Ported literally, with (- 63 clz) as
;;; (eor #63) — equal for clz ∈ [0,63], which holds because u & -u is a
;;; single set bit for u ≠ 0.  Matt uses the same trick in %fixnum-intlen.
;;;
;;; Flag safety (verified against his assembler): lsl/lsr with a register
;;; or immediate count are the lslv/lsrv/UBFM aliases (arm64-asm.lisp:
;;; 818-823) and set no flags, so every b.cond below still tests the cmp or
;;; ands that precedes it — exactly as PPC's srd/srdi sit between cmpd and
;;; the branches.
;;; =====================================================================
(defarm64lapfunction %fixnum-gcd ((n1 arg_y) (n2 arg_z))
  (let ((temp imm0)
        (u imm1)
        (v imm2)
        (ut0 imm3)
        (vt0 imm4))
    (unbox-fixnum u n1)                    ; ppc:361
    (unbox-fixnum v n2)                    ; ppc:362
    (neg temp u)                           ; ppc:363
    (and temp temp u)                      ; ppc:364 — temp = u & -u
    (clz ut0 temp)                         ; ppc:365 (cntlzd)
    (eor ut0 ut0 (:$ 63))                  ; ppc:366 (subfic ut0 ut0 63)
    (neg temp v)                           ; ppc:367
    (and temp temp v)                      ; ppc:368
    (clz vt0 temp)                         ; ppc:369 (cntlzd)
    (eor vt0 vt0 (:$ 63))                  ; ppc:370 (subfic vt0 vt0 63)
    (lsr u u ut0)                          ; ppc:372 (srd)
    (lsr v v vt0)                          ; ppc:373 (srd)
    (add ut0 ut0 (:$ arm64::fixnum-shift)) ; ppc:374 (addi)
    (add vt0 vt0 (:$ arm64::fixnum-shift)) ; ppc:375 (addi)
    (cmp ut0 vt0)                          ; ppc:371 (cmpw cr2) — moved here
    (csel ut0 ut0 vt0 (:? le))             ; ppc:381 (blelr cr2) resolved: min
    @loop
    (cmp u v)                              ; ppc:377 (cmpd cr0)
    (lsl arg_z u ut0)                      ; ppc:378 (sld) — speculative
    (b.gt @u>v)                            ; ppc:379 (bgt cr0)
    (b.lt @u<v)                            ; ppc:380 (blt cr0)
    ;; u = v: arg_z already holds u << min(ut0,vt0), covering both
    ;; ppc:381 (blelr cr2) and ppc:382-383 (sld arg_z u vt0 / blr).
    (ret)
    @u>v
    (sub u u v)                            ; ppc:385
    @shiftu
    (ands temp u (:$ 2))                   ; ppc:387 (andi. temp u (ash 1 1))
    (lsr u u (:$ 1))                       ; ppc:388 (srdi) — flag-safe
    (b.eq @shiftu)                         ; ppc:389 (beq cr0)
    (b @loop)                              ; ppc:390
    @u<v
    (sub v v u)                            ; ppc:392
    @shiftv
    (ands temp v (:$ 2))                   ; ppc:394 (andi.)
    (lsr v v (:$ 1))                       ; ppc:395 (srdi) — flag-safe
    (b.eq @shiftv)                         ; ppc:396 (beq cr0)
    (b @loop)))                            ; ppc:397

;;; =====================================================================
;;; 16m48h promotion batch — level-0 draft items whose only blocker was the
;;; promotion itself.  Bucketed by tools/drafted-promotion-buckets.py; every
;;; constant below was checked to exist in his tree AT THE PIN, because a
;;; DEPLOYED lap body whose constant had been renamed out from under it is
;;; exactly what cost 16m31 a cycle (c-frame.backlink).
;;; =====================================================================

;;; =====================================================================
;;; pending-user-interrupt — ppc:956
;;; =====================================================================
;;; Read the interrupt-pending kernel global and clear it in one go.  PPC
;;; stores rzero; AArch64's zero register is xzr, and set-global's in-range
;;; form is a plain stur, so xzr goes straight in (intflag is kernel-global
;;; index 8, arm64-arch.lisp:312, comfortably inside stur's simm9 window).
;;; NOTE: this is the FIRST promoted caller of the set-global lapmacro
;;; (drafts/arm64-lapmacros-additions.lisp:303) — ref-global has many, its
;;; store twin had none.
(defarm64lapfunction pending-user-interrupt ()
  (ref-global arg_z intflag)                      ; ppc:957
  (set-global xzr intflag)                        ; ppc:960 (set-global rzero)
  (ret))                                          ; ppc:962

;;; =====================================================================
;;; %safe-get-ptr — ppc:966
;;; =====================================================================
;;; Dereference a possibly-bad pointer with the kernel primed to recover:
;;; tcr.safe-ref-address holds the address being read, and the fault handler
;;; uses it to recover instead of dying.  PPC needs no recovery frame and
;;; neither do we; the x86-64 twin adds a :tra frame because its handler
;;; resumes differently.  Must be called with interrupts disabled (PPC says
;;; so at ppc:965).
(defarm64lapfunction %safe-get-ptr ((src arg_y) (dest arg_z))
  (check-nargs 2)                                 ; ppc:967
  (macptr-ptr imm0 src)                           ; ppc:968
  (str imm0 (:@ rcontext (:$ arm64::tcr.safe-ref-address))) ; ppc:969
  (ldr imm0 (:@ imm0 (:$ 0)))                     ; ppc:970 — may fault
  (stur imm0 (:@ dest (:$ arm64::macptr.address))) ; ppc:971 (str -> stur)
  (ret))                                          ; ppc:972

;;; =====================================================================
;;; The 64-bit foreign-memory accessor quartet — ppc:332/350/372/398
;;; (all four are the #+ppc64-target arms; the ppc32 twins at :322/:340/
;;; :358/:384 split the value across two 32-bit halves and are skipped.)
;;; =====================================================================
;;; TAIL CORRECTION on the two getters: the draft ended each in
;;; (call-subprim .SPmakeu64 / .SPmakes64), which emits `bl' — the subprim
;;; would return into the end of this code vector and fall through into the
;;; next object.  PPC's `ba' is a tail BRANCH, so it is (jump-subprim ...).
;;; These are entries 17 and 18 of tools/draft-tail-subprim-lint.py.
;;;
;;; Both DECIDEs the draft attached to that line are STALE:
;;;   * ".SPmakeu64/.SPmakes64 are not in Matt's table" — they are, at
;;;     arm64-arch.lisp:443 and :444, as are .SPgetu64/:509 and .SPgets64/:510.
;;;   * "call-subprim's imm0 scratch clobbers makeu64's imm0 INPUT" — the
;;;     scratch is imm1, not imm0 (drafts/arm64-lapmacros-additions.lisp:257).
;;;     Moot here anyway, since jump-subprim is what we emit.
;;;
;;; The two SETTERS keep (call-subprim .SPgetu64/.SPgets64): those really are
;;; PPC's linked `bla' (ppc:377/403) — the unboxed value has to come back here
;;; to be stored.  PPC then tail-branches to .SPpopj to pop the frame and
;;; return; .SPpopj is in his table (:511) but a frame pop plus a return is
;;; precisely what restore-full-lisp-context + ret already are inline, so we
;;; spend two instructions instead of a subprim transfer.
(defarm64lapfunction %%get-unsigned-longlong ((ptr arg_y) (offset arg_z))
  (trap-unless-typecode= ptr arm64::subtag-macptr) ; ppc:333
  (macptr-ptr imm1 ptr)                            ; ppc:334
  (unbox-fixnum imm2 offset)                       ; ppc:335
  (ldr imm0 (:@ imm1 imm2))                        ; ppc:336 (ldx -> reg offset)
  (jump-subprim .SPmakeu64))                       ; ppc:337 (ba — TAIL branch)

(defarm64lapfunction %%get-signed-longlong ((ptr arg_y) (offset arg_z))
  (trap-unless-typecode= ptr arm64::subtag-macptr) ; ppc:351
  (macptr-ptr imm1 ptr)                            ; ppc:352
  (unbox-fixnum imm2 offset)                       ; ppc:353
  (ldr imm0 (:@ imm1 imm2))                        ; ppc:354 (ldx -> reg offset)
  (jump-subprim .SPmakes64))                       ; ppc:355 (ba — TAIL branch)

(defarm64lapfunction %%set-unsigned-longlong ((ptr arg_x) (offset arg_y) (val arg_z))
  (save-lisp-context)                              ; ppc:375
  (trap-unless-typecode= ptr arm64::subtag-macptr) ; ppc:376
  (call-subprim .SPgetu64)                          ; ppc:377 (bla — LINKED)
  (macptr-ptr imm2 ptr)                            ; ppc:378
  (unbox-fixnum imm3 offset)                        ; ppc:379
  (str imm0 (:@ imm2 imm3))                        ; ppc:380 (stdx -> reg offset)
  (restore-full-lisp-context)                       ; ppc:381 (.SPpopj inlined)
  (ret))

(defarm64lapfunction %%set-signed-longlong ((ptr arg_x) (offset arg_y) (val arg_z))
  (save-lisp-context)                              ; ppc:401
  (trap-unless-typecode= ptr arm64::subtag-macptr) ; ppc:402
  (call-subprim .SPgets64)                          ; ppc:403 (bla — LINKED)
  (macptr-ptr imm2 ptr)                            ; ppc:404
  (unbox-fixnum imm3 offset)                        ; ppc:405
  (str imm0 (:@ imm2 imm3))                        ; ppc:406 (stdx -> reg offset)
  (restore-full-lisp-context)                       ; ppc:407 (.SPpopj inlined)
  (ret))

;;; =====================================================================
;;; %staticp / %static-inverse-cons — ppc:1034 / :1049
;;; =====================================================================
;;; The static-cons index round trip.  %staticp maps a static cons to its
;;; index (or NIL); %static-inverse-cons maps an index back to the cons (or
;;; NIL if out of range or already collected).
;;;
;;; ARM64-DEVIATION in %staticp, and it is x86-64's shape rather than PPC's,
;;; deliberately: PPC computes (ndnodes - index) into imm1, tests it against
;;; zero on cr0, THEN adds 128, and returns nil via blelr — i.e. it relies on
;;; keeping a compare result live across the add.  AArch64 has one NZCV, so
;;; the compare has to BE the subtraction: (subs imm2 imm1 imm0) sets C=1,Z=0
;;; exactly when ndnodes > index unsigned, which is the HI condition, and csel
;;; then picks the boxed result or the NIL already sitting in arg_z.  That is
;;; instruction-for-instruction what x86-64 does (x86-misc.lisp:958-970: subq,
;;; lea 128, lea with the fixnumone scale, cmovaq — `A' = above = HI), and
;;; csel with a (:? cond) operand is already used by eight promoted functions
;;; in this tree.  Semantically it is very slightly STRICTER than PPC: PPC's
;;; unsigned "<= 0" only rejects index == ndnodes, so an index PAST the end
;;; returns a huge bogus value there and NIL here.
(defarm64lapfunction %staticp ((x arg_z))
  (check-nargs 1)                                  ; ppc:1035
  (ref-global temp0 static-cons-area)              ; ppc:1036
  (ldr imm1 (:@ temp0 (:$ arm64::area.low)))       ; ppc:1037
  (sub imm0 x imm1)                                ; ppc:1038 (x - low)
  (lsr imm0 imm0 (:$ arm64::dnode-shift))          ; ppc:1041 (srri -> dnode index)
  (ldr imm1 (:@ temp0 (:$ arm64::area.ndnodes)))   ; ppc:1039
  (mov arg_z rnil)                                 ; ppc:1040 (li nil default)
  (subs imm2 imm1 imm0)                            ; ppc:1042-1043 (sub + cmplri)
  (add imm1 imm2 (:$ 128))                         ; ppc:1044 (la 128)
  (box-fixnum imm1 imm1)                           ; ppc:1046
  (csel arg_z imm1 arg_z (:? hi))                  ; ppc:1045 (blelr inverted)
  (ret))                                           ; ppc:1047

;;; The lisptag test resolves on NZCV before the second compare needs it, and
;;; ref-global's worst case is sub+ldr — neither touches flags — so the
;;; interleaving PPC gets from cr0/cr1 is preserved here by ordering alone.
;;; PPC's `cmplr' is UNSIGNED, so its bgt is b.hi, not b.gt.
(defarm64lapfunction %static-inverse-cons ((n arg_z))
  (check-nargs 1)                                  ; ppc:1050
  (extract-lisptag imm0 arg_z)                     ; ppc:1051
  (ref-global temp0 static-cons-area)              ; ppc:1053 (flag-safe)
  (cbnz imm0 @fail)                                ; ppc:1052+1054 (cmpri 0 / bne)
  (sub n n (:$ (ash 128 arm64::fixnumshift)))      ; ppc:1055 (la n '-128 n)
  (ldr imm0 (:@ temp0 (:$ arm64::area.ndnodes)))   ; ppc:1056
  (ldr imm1 (:@ temp0 (:$ arm64::area.high)))      ; ppc:1057
  (box-fixnum arg_y imm0)                          ; ppc:1058
  (sub imm1 imm1 n)                                ; ppc:1059
  (sub imm1 imm1 n)                                ; ppc:1061 (n subtracted twice)
  (cmp arg_z arg_y)                                ; ppc:1060 (cmplr — unsigned)
  (b.hi @fail)                                     ; ppc:1062 (bgt on a cmplr)
  (add arg_z imm1 (:$ arm64::fulltag-cons))        ; ppc:1063 (la fulltag-cons)
  (ldur arg_y (:@ arg_z (:$ arm64::cons.car)))     ; ppc:1064 (cons.car = +5 -> ldur)
  (cmp arg_y (:$ arm64::unbound-marker))           ; ppc:1065
  (b.ne @done)                                     ; ppc:1066 (bnelr) — live cons
  @fail
  (mov arg_z rnil)                                 ; ppc:1068 (li nil)
  @done
  (ret))                                           ; ppc:1069

;;; =====================================================================
;;; %code-vector-last-instruction — ppc:826
;;; =====================================================================
;;; PPC64 already expresses this as a portable DEFUN (the #+ppc32-target LAP
;;; twin at ppc:791 is the one with arch dependence, and is skipped), so it is
;;; carried verbatim with no arm64 content at all.
(defun %code-vector-last-instruction (cv)
  (do* ((i 1 (1+ i))
        (instr nil)
        (n (uvsize cv)))
       ((= i n) instr)
    (declare (fixnum i n))
    (let* ((next (uvref cv i)))
      (declare (type (unsigned-byte 32) next))
      (if (zerop next)
        (return instr)
        (setq instr next)))))

;;; =====================================================================
;;; %%save-application — ppc:841
;;; =====================================================================
;;; PROMOTED 16m51 (lane S3) out of upstream-port/level-0/drafts/arm64-misc.lisp
;;; :671-680, where it sat unbuilt as bucket C-JUDGMENT / marker DECIDE-3.  It is
;;; the LAP function that traps into the kernel to write an image, and it was the
;;; ONLY missing link in the whole (save-application ...) chain: everything above
;;; it (lib/dumplisp.lisp:66/:123/:172/:319) and everything below it
;;; (arm64-exceptions.c handle_gc_trap SAVE_APPLICATION arm -> image.c
;;; save_application_internal) is real.  Inventory: comms/PHASE3-SAVE-16m51.md §1.
;;;
;;; The draft's DECIDE-3 marker was STALE.  It flagged `trlgei allocptr 0' ->
;;; `uuo-gc-trap' as undecided, but that substitution is already decided AND
;;; SHIPPING for the four sibling gc-trap LAP functions in this same overlay:
;;; arm64-utils.lisp egc / %configure-egc / purify / impurify all write plain
;;; (uuo-gc-trap) against the same `; ppc:NNN (trlgei allocptr 0)' citation, and
;;; against the same kernel handler.  Nothing was left to decide.
;;;
;;; Two ARM64-DEVIATIONs, both ISA-forced, both already precedented here — and
;;; NEITHER is an ARM32 borrow (PPC64 has the analog; only its spelling changes):
;;;
;;;   ARM64-DEVIATION (logical-or-immediate spelling): PPC's `ori rD,rA,imm'
;;;     (ppc:843) is AArch64's `orr Rd,Rn,#imm'.  8 = 0b1000 is a single set
;;;     bit, hence a valid AArch64 bitmask immediate.  The (orr r r (:$ n)) form
;;;     is live in this overlay already: arm64-float.lisp:677.
;;;   ARM64-DEVIATION (no conditional-trap-on-immediate): PPC's
;;;     `trlgei allocptr,0' (ppc:845) is "trap if logically >= 0", i.e. an
;;;     unconditional trap.  AArch64 has no such instruction; the port uses
;;;     Matt's OWN uuo-gc-trap (compiler/ARM64/arm64-asm.lisp:438), which takes
;;;     NO operand.  ARM32 writes (uuo-gc-trap (:? al)); arm64 must not.
;;;
;;; Register contract, cross-checked against the kernel that consumes it
;;; (upstream-port/lisp-kernel/arm64-exceptions.c): handle_gc_trap reads
;;; selector = xpGPR(xp,imm0) and arg = xpGPR(xp,imm1), and its
;;; GC_TRAP_FUNCTION_SAVE_APPLICATION arm passes `arg' as the fd to
;;; save_application(unsigned fd, Boolean egc_was_enabled).  So imm0 = flags|8
;;; and imm1 = fd is exactly right, and the :purify / :impurify bits ride in the
;;; same imm0 word (selector & GC_TRAP_FUNCTION_PURIFY, gc.h:145).
;;; gc-trap-function-save-application = 8 is compiler/arch.lisp:357 — an
;;; arch-independent defconstant, no arm64 gap.
;;;
;;; No check-nargs, exactly as PPC64 has none.
(defarm64lapfunction %%save-application ((flags arg_y) (fd arg_z))
  (unbox-fixnum imm0 flags)                        ; ppc:842
  (orr imm0 imm0 (:$ arch::gc-trap-function-save-application)) ; ppc:843 (ori)
  (unbox-fixnum imm1 fd)                           ; ppc:844
  (uuo-gc-trap)                                    ; ppc:845 (trlgei allocptr 0)
  (ret))                                           ; ppc:846
