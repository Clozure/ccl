;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;; arm64-utils.lisp — ACTIVE seed (demand-driven from cold-load fatals;
;;; lifted from upstream-port/level-0/drafts/arm64-utils.lisp wave-3).
;;; PPC64 LINE-PORT (source: vendor/ccl/level-0/PPC/ppc-utils.lisp)
;;; Per-line citations: "; ppc:NNN" = line NNN of that file.

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "ARM64-LAPMACROS"))

;;; =====================================================================
;;; %kernel-import — ppc:619
;;; =====================================================================
;;; offset is a boxed fixnum, one of the target::kernel-import-xxx BYTE
;;; offsets; unboxing yields the raw byte offset.
(defarm64lapfunction %kernel-import ((offset arg_z))
  (ref-global imm0 kernel-imports)      ; ppc:621
  (unbox-fixnum imm1 arg_z)             ; ppc:622
  (ldr arg_z (:@ imm0 imm1))            ; ppc:623 (ldrx)
  (ret))                                ; ppc:624

;;; =====================================================================
;;; %get-unboxed-ptr — ppc:626
;;; =====================================================================
(defarm64lapfunction %get-unboxed-ptr ((macptr arg_z))
  (macptr-ptr imm0 arg_z)               ; ppc:627
  (ldr arg_z (:@ imm0 (:$ 0)))          ; ppc:628
  (ret))                                ; ppc:629

;;; =====================================================================
;;; true / false — ppc:664/673
;;; =====================================================================
;;; nargs is boxed and fixnumshift=3=word-shift: the boxed count IS the
;;; byte count, so (add vsp vsp imm0) needs no scaling.  PPC's blelr
;;; (return if nargs <= 3, unsigned) becomes b.hi-over-ret.
(defarm64lapfunction true ()
  (cmp nargs (:$ (ash 3 arm64::fixnumshift))) ; ppc:666
  (add arg_z rnil (:$ arm64::t-offset)) ; ppc:667 (li arg_z t)
  (b.hi @pop)                           ; ppc:668 (blelr inverted)
  (ret)
  @pop
  (sub imm0 nargs (:$ (ash 3 arm64::fixnumshift))) ; ppc:669
  (add vsp vsp imm0)                    ; ppc:670
  (ret))                                ; ppc:671

(defarm64lapfunction false ()
  (cmp nargs (:$ (ash 3 arm64::fixnumshift))) ; ppc:674
  (mov arg_z rnil)                      ; ppc:675
  (b.hi @pop)                           ; ppc:676 (blelr inverted)
  (ret)
  @pop
  (sub imm0 nargs (:$ (ash 3 arm64::fixnumshift))) ; ppc:677
  (add vsp vsp imm0)                    ; ppc:678
  (ret))                                ; ppc:679

;;; constant-ref — ppc:681 (16m13 demand: CONSTANTLY does
;;; (%copy-function #'constant-ref) + set-nth-immediate; with
;;; constant-ref undefined, every (constantly x) initfunction was a
;;; copied UDF placeholder that trapped "undefined function" when
;;; %early-shared-initialize funcalled it).  Constant-pool slot via
;;; nfn-relative quoted-symbol syntax (DECIDE-14 precedent,
;;; arm64-clos.lisp).  ldur does not disturb NZCV.
(defarm64lapfunction constant-ref ()
  (cmp nargs (:$ (ash 3 arm64::fixnumshift))) ; ppc:682
  (ldur arg_z (:@ nfn (:$ 'constant)))  ; ppc:683 (ldr arg_z 'constant nfn)
  (b.hi @pop)                           ; ppc:684 (blelr inverted)
  (ret)
  @pop
  (sub imm0 nargs (:$ (ash 3 arm64::fixnumshift))) ; ppc:685
  (add vsp vsp imm0)                    ; ppc:686
  (ret))                                ; ppc:687

;;; =====================================================================
;;; %address-of — from vendor/ccl/level-0/PPC/ppc-utils.lisp:35
;;; (#+ppc64-target arm; ppc32 twin ppc:21 skipped).  16m21b demand:
;;; the error/object printer (l1-io.lisp:388 "BOGUS object @ #x…" and
;;; the #<CODE-VECTOR …> print path) funcalls %address-of; it was left
;;; unpromoted in drafts, so a compile-time error's printer hit
;;; "Undefined function %ADDRESS-OF" -> recursive printing error.
;;; =====================================================================
;;;
;;; %address-of a fixnum is the fixnum itself; anything else is its
;;; (tagged) address as an integer — a fixnum if it fits in 60 bits, else
;;; a bignum via .SPmakeu64.  Low-tag arithmetic is identical to PPC64
;;; (fixnumshift 3 both).  Bignum path: TAIL (jump-subprim .SPmakeu64,
;;; imm0 = its input), the canonical no-link idiom already used at
;;; arm64-def.lisp:103 / x86-utils.lisp:29 — NOT the draft's call-subprim
;;; (whose dispatch scratch collides with imm0; DECIDE W4-D11).
;;; .SPmakeu64 IS in Matt's subprims table (Matt's own impl @ 115b7aa,
;;; spentry-A-alloc-numbers.s:187).

(defarm64lapfunction %address-of ((arg arg_z))
  (ands imm0 arg (:$ arm64::tagmask))   ; ppc:38 (clrldi. — low 3 tag bits)
  (b.ne @nonfixnum)                     ; ppc:39 (beqlr — fixnum: unchanged)
  (ret)
  @nonfixnum
  (mov imm0 arg_z)                      ; ppc:40
  ;; ppc:42 (clrrdi.): EQ iff the address fits in a fixnum (top 4 bits
  ;; clear).  Mask #xF000000000000000 is a single ones-run = valid
  ;; logical immediate; ldb wrap for the no-negative-immediate encoder.
  (ands imm1 imm0 (:$ (ldb (byte 64 0)
                           (lognot (1- (ash 1 (- 63 arm64::fixnumshift)))))))
  (box-fixnum arg_z imm0)               ; ppc:43 (lsl — NZCV-safe)
  (b.ne @bignum)                        ; ppc:44 (beqlr+ — boxed fixnum done)
  (ret)
  @bignum
  (jump-subprim .SPmakeu64))            ; ppc:45 (ba — TAIL no-link)

;;; =====================================================================
;;; %normalize-areas — from vendor/ccl/level-0/PPC/ppc-utils.lisp:59
;;; (16m5v demand; promoted from drafts/arm64-utils.lisp:72)
;;; =====================================================================
;;;
;;; Update the current thread's stack-area "active" pointers, then return
;;; the active dynamic area (successor of the all-areas list header).
;;; Matt's design has a tsp register (x24), so the tsp store is direct.
;;; (mov imm1 sp) is the ADD alias (arm64-asm.lisp:467) — SP legal as Rn.

(defarm64lapfunction %normalize-areas ()
  (let ((address imm0))
    ;; update active pointer for tsp area.  ppc:64-65
    (ldr address (:@ rcontext (:$ arm64::tcr.ts-area)))
    (str tsp (:@ address (:$ arm64::area.active)))
    ;; update active pointer for vsp area.  ppc:68-69
    (ldr address (:@ rcontext (:$ arm64::tcr.vs-area)))
    (str vsp (:@ address (:$ arm64::area.active)))
    ;; update active pointer for SP area.  ppc:72-73
    (ldr arg_z (:@ rcontext (:$ arm64::tcr.cs-area)))
    (mov imm1 sp)                       ; (str sp …) not encodable; stage
    (str imm1 (:@ arg_z (:$ arm64::area.active)))
    (ref-global arg_z all-areas)        ; ppc:76
    (ldr arg_z (:@ arg_z (:$ arm64::area.succ)))  ; ppc:77
    (ret)))                             ; ppc:79 (blr)

;;; =====================================================================
;;; %active-dynamic-area — from vendor/ccl/level-0/PPC/ppc-utils.lisp:81
;;; (same donor cluster; trivial sibling)
;;; =====================================================================

(defarm64lapfunction %active-dynamic-area ()
  (ref-global arg_z all-areas)          ; ppc:82
  (ldr arg_z (:@ arg_z (:$ arm64::area.succ)))  ; ppc:83
  (ret))                                ; ppc:84 (blr)

;;; =====================================================================
;;; %walk-dynamic-area — from vendor/ccl/level-0/PPC/ppc-utils.lisp:366
;;; (16m5w demand; promoted from drafts/arm64-utils.lisp:288 with three
;;; promotion fixes: (1) ppc:376 (:regsave sentinel 0) dropped — his lap
;;; DSL has no :regsave (DECIDE-7, 16m5q precedent arm64-array.lisp:287);
;;; (2) draft's stray temp1 at the loop-head load unified to the imm5
;;; scan pointer (x5 free — nargs is x6); (3) alloc-trap branch b.hi →
;;; b.hs, the canonical Misc_Alloc shape (w1:340).)
;;; =====================================================================
;;;
;;; Like walk-static-area but objects may be consed while walking:
;;; terminate at a freshly-allocated sentinel cons.  The sentinel is
;;; allocated by FORCING the allocation trap (allocbase set to a huge
;;; value so the no-trap branch is never taken) — the kernel completes
;;; the allocation exactly as PPC's tdlt-always-traps idiom did; the
;;; allocation request is conveyed by allocptr's fulltag (cons).
;;; PPC's cr0 (tenured-area zero test, ppc:381) stays live across the
;;; sentinel allocation; our alloc protocol contains a cmp, so the
;;; selection is resolved FIRST with csel — same value, earlier point.

(defarm64lapfunction %walk-dynamic-area ((a arg_y) (f arg_z))
  (let ((fun save0)
        (obj save1)
        (sentinel save2)
        (header imm0)
        (tag imm1)
        (subtag imm2)
        (bytes imm3)
        (elements imm4))
    (save-lisp-context)                 ; ppc:375
    ;; ppc:376 (:regsave sentinel 0) — no :regsave in his lap DSL (DECIDE-7)
    (vpush fun)                         ; ppc:377
    (vpush obj)                         ; ppc:378
    (vpush sentinel)                    ; ppc:379
    (ref-global imm0 tenured-area)      ; ppc:380
    (cmp imm0 (:$ 0))                   ; ppc:381 (cmpdi cr0)
    (csel a imm0 a (:? ne))             ; ppc:390-391 (if :ne (mr a imm0)) — moved up
    ;; allocbase := #x8000_0000_0000 - 16 so the trap below always fires.
    (movz allocbase (:$ #x8000 :lsl 32)) ; ppc:382-383 (lwi #x8000; sldi 32)
    (sub allocbase allocbase (:$ 16))   ; ppc:384 (subi allocbase allocbase 16)
    ;; tagged-cons allocptr, then the canonical alloc-trap protocol.
    (sub allocptr allocptr (:$ (- arm64::cons.size arm64::fulltag-cons))) ; ppc:385 (la)
    (cmp allocptr allocbase)            ; ppc:386 (tdlt allocptr allocbase) …
    (b.hs @no-trap)                     ; … canonical Misc_Alloc shape (w1:340)
    (uuo-alloc-trap)
    @no-trap
    (mov sentinel allocptr)             ; ppc:387 (mr)
    (and allocptr allocptr (:$ (ldb (byte 64 0) (lognot arm64::fulltagmask)))) ; ppc:388 (clrrdi ntagbits)
    (mov fun f)                         ; ppc:389 (mr)
    (ldr imm5 (:@ a (:$ arm64::area.low))) ; ppc:392 (ld imm5 …)
    @loop
    (ldr header (:@ imm5 (:$ 0)))       ; ppc:394
    ;; ppc:395-399 — header-vs-cons discrimination on the header fulltag
    (and tag header (:$ arm64::fulltagmask))
    (cmp tag (:$ arm64::fulltag-immheader-0))
    (b.eq @misc)
    (cmp tag (:$ arm64::fulltag-immheader-1))
    (b.eq @misc)
    (cmp tag (:$ arm64::fulltag-immheader-2))
    (b.eq @misc)
    (cmp tag (:$ arm64::fulltag-nodeheader-0))
    (b.eq @misc)
    (cmp tag (:$ arm64::fulltag-nodeheader-1))
    (b.eq @misc)
    ;; cons
    (add obj imm5 (:$ arm64::fulltag-cons))    ; ppc:400 (la)
    (cmp obj sentinel)                  ; ppc:401 (cmpd cr0)
    (mov arg_z obj)                     ; ppc:402 — mov/set-nargs preserve NZCV
    (set-nargs 1)                       ; ppc:403
    (mov temp0 fun)                     ; ppc:404
    (b.eq @done)                        ; ppc:405 (beq cr0)
    (call-subprim .SPfuncall)           ; ppc:406 (bla .SPfuncall)
    (add imm5 obj (:$ (- arm64::cons.size arm64::fulltag-cons))) ; ppc:407 (la)
    (b @loop)                           ; ppc:408
    @misc
    (add obj imm5 (:$ arm64::fulltag-misc))    ; ppc:410 (la)
    (mov arg_z obj)                     ; ppc:411
    (set-nargs 1)                       ; ppc:412
    (mov temp0 fun)                     ; ppc:413
    (call-subprim .SPfuncall)           ; ppc:414
    (getvheader header obj)             ; ppc:415
    ;; ppc:416-438 — size dispatch on ivector class / subtag
    (and tag header (:$ arm64::fulltagmask))    ; ppc:416 (extract-lowtag)
    (header-size elements header)       ; ppc:425
    (cmp tag (:$ arm64::fulltag-nodeheader-0))  ; ppc:418 (cr1)
    (b.eq @8bytes)                      ; ppc:427 (beq cr1)
    (cmp tag (:$ arm64::fulltag-nodeheader-1))  ; ppc:418 (cr1)
    (b.eq @8bytes)                      ; ppc:427
    (cmp tag (:$ arm64::ivector-class-64-bit))  ; ppc:420 (cr2)
    (b.eq @8bytes)                      ; ppc:428 (beq cr2)
    (cmp tag (:$ arm64::ivector-class-32-bit))  ; ppc:422 (cr4)
    (b.eq @4bytes)                      ; ppc:432 (beq cr4)
    (and subtag header (:$ #xff))       ; ppc:419 (extract-lowbyte)
    (cmp subtag (:$ arm64::subtag-bit-vector))  ; ppc:423 (cr5)
    (b.eq @bit)                         ; ppc:436
    (cmp subtag (:$ arm64::subtag-complex-double-float-vector)) ; ppc:424 (cr6)
    (b.eq @16bytes)                     ; ppc:434 (beq cr6)
    (cmp subtag (:$ arm64::min-8-bit-ivector-subtag))
    (b.hs @1byte)                       ; ppc:430 (beq cr3 — 8-bit class)
    (lsl bytes elements (:$ 1))         ; ppc:435 (sldi bytes elements 1)
    (b @bump)
    @1byte
    (mov bytes elements)                ; ppc:429 (mr bytes elements)
    (b @bump)
    @bit
    (add bytes elements (:$ 7))         ; ppc:437 (la elements 7 elements)
    (lsr bytes bytes (:$ 3))            ; ppc:438 (srdi bytes elements 3)
    (b @bump)
    @16bytes
    (lsl bytes elements (:$ 4))         ; ppc:433 (sldi bytes elements 4)
    (b @bump)
    @4bytes
    (lsl bytes elements (:$ 2))         ; ppc:431 (sldi bytes elements 2)
    (b @bump)
    @8bytes
    (lsl bytes elements (:$ 3))         ; ppc:426 (sldi bytes elements 3)
    @bump
    (add bytes bytes (:$ (+ 8 15)))     ; ppc:440 (la bytes (+ 8 15) bytes)
    (and bytes bytes (:$ (ldb (byte 64 0) (lognot 15)))) ; ppc:441 (clrrdi 4)
    (sub imm5 obj (:$ arm64::fulltag-misc))    ; ppc:442 (subi)
    (add imm5 imm5 bytes)               ; ppc:443
    (b @loop)                           ; ppc:444
    @done
    (mov arg_z rnil)                    ; ppc:446 (li arg_z nil)
    (vpop sentinel)                     ; ppc:447
    (vpop obj)                          ; ppc:448
    (vpop fun)                          ; ppc:449
    (restore-full-lisp-context)         ; ppc:450
    (ret)))                             ; ppc:451 (blr)

;;; ppc:453-455 — plain lisp, carried verbatim.
(defun walk-dynamic-area (area func)
  (with-other-threads-suspended
      (%walk-dynamic-area area func)))

;;; =====================================================================
;;; walk-static-area — from vendor/ccl/level-0/PPC/ppc-utils.lisp:184
;;; (#+ppc64-target arm; the #+ppc32-target twin at ppc:111 is skipped)
;;; =====================================================================
;;; (16m5y demand; promoted from drafts/arm64-utils.lisp:138 — :regsave
;;; dropped per DECIDE-7, otherwise verbatim.)
;;;
;;; Call f on every object in static area a.  Structure is the PPC64
;;; line-port; TWO tag-scheme adaptations (both mirrored from the x86-64
;;; twin, vendor/ccl/level-0/X86/x86-utils.lisp:92, whose tag scheme Matt's
;;; design copies):
;;;  1. header-vs-cons: PPC64 tests a 2-bit lowtag (lowtag-immheader /
;;;     lowtag-nodeheader).  Matt's design has FIVE header fulltags
;;;     {immheader-0=5, nodeheader-0=6, immheader-1=12, immheader-2=13,
;;;     nodeheader-1=14} with no common low-bit pattern — each is tested.
;;;     Header fulltags are disjoint from every valid node fulltag, so a
;;;     cons's cdr word (at [base+0]) can never be misread as a header.
;;;  2. element-size dispatch: PPC64's ivector classes (64/8/32-bit)
;;;     differ from Matt's (64-bit / 32-bit / other-bit, where "other"
;;;     mixes 16-bit, 8-bit, bit, and complex-double-float vectors);
;;;     dispatch rewritten per HIS arm64-arch.lisp constants.
;;; PPC64 keeps 6 CRs live across the dispatch; ARM64's single NZCV forces
;;; sequential cmp/branch — the subtag byte is therefore extracted while
;;; header is still live, and elements lives in imm4 (NOT PPC's imm0=header
;;; alias) so header survives until the subtag extraction.
;;;
;;; .SPfuncall convention: callee in temp0 (matches PPC; confirmed against
;;; our upstream-port spentry draft, spentry-D-call-builtins.s:159).

(defarm64lapfunction walk-static-area ((a arg_y) (f arg_z))
  (let ((fun save0)
        (obj save1)
        (limit save2)
        (header imm0)
        (tag imm1)
        (subtag imm2)
        (bytes imm3)
        (elements imm4))
    (save-lisp-context)                 ; ppc:193
    ;; ppc:194 (:regsave limit 0) — no :regsave in his lap DSL (DECIDE-7)
    (vpush fun)                         ; ppc:195
    (vpush obj)                         ; ppc:196
    (vpush limit)                       ; ppc:197
    (mov fun f)                         ; ppc:198 (mr)
    (ldr limit (:@ a (:$ arm64::area.active)))  ; ppc:199
    (ldr obj (:@ a (:$ arm64::area.low)))       ; ppc:200
    (b @test)                           ; ppc:201
    @loop
    (ldr header (:@ obj (:$ 0)))        ; ppc:203
    ;; ppc:204-208 — header-vs-cons (adaptation 1 above)
    (and tag header (:$ arm64::fulltagmask))
    (cmp tag (:$ arm64::fulltag-immheader-0))
    (b.eq @misc)
    (cmp tag (:$ arm64::fulltag-immheader-1))
    (b.eq @misc)
    (cmp tag (:$ arm64::fulltag-immheader-2))
    (b.eq @misc)
    (cmp tag (:$ arm64::fulltag-nodeheader-0))
    (b.eq @misc)
    (cmp tag (:$ arm64::fulltag-nodeheader-1))
    (b.eq @misc)
    ;; cons
    (add arg_z obj (:$ arm64::fulltag-cons))    ; ppc:209 (la)
    (set-nargs 1)                       ; ppc:210
    (mov temp0 fun)                     ; ppc:211 (mr temp0 fun)
    ;; ppc:212 (bla .SPFuncall) — DECIDE: in our spentry drafts
    ;; (spentry-D), not yet in Matt's *subprims* table.
    (call-subprim .SPfuncall)
    (add obj obj (:$ arm64::cons.size)) ; ppc:213 (la obj cons.size obj)
    (b @test)                           ; ppc:214
    @misc
    (add arg_z obj (:$ arm64::fulltag-misc))    ; ppc:216 (la)
    (set-nargs 1)                       ; ppc:217
    (mov temp0 fun)                     ; ppc:218
    (call-subprim .SPfuncall)           ; ppc:219 — same DECIDE
    (ldr header (:@ obj (:$ 0)))        ; ppc:220
    ;; ppc:221-243 — size dispatch (adaptation 2 above)
    (and tag header (:$ arm64::fulltagmask))    ; ppc:221 (extract-lowtag)
    (header-size elements header)       ; ppc:230
    (cmp tag (:$ arm64::fulltag-nodeheader-0))  ; ppc:223 (cr1)
    (b.eq @8bytes)                      ; ppc:232 (beq cr1)
    (cmp tag (:$ arm64::fulltag-nodeheader-1))  ; ppc:223 (cr1)
    (b.eq @8bytes)                      ; ppc:232
    (cmp tag (:$ arm64::ivector-class-64-bit))  ; ppc:225 (cr2)
    (b.eq @8bytes)                      ; ppc:233 (beq cr2)
    (cmp tag (:$ arm64::ivector-class-32-bit))  ; ppc:227 (cr4)
    (b.eq @4bytes)                      ; ppc:237 (beq cr4)
    ;; ivector-class-other-bit: dispatch on the full subtag byte
    (and subtag header (:$ #xff))       ; ppc:224 (extract-lowbyte)
    (cmp subtag (:$ arm64::subtag-bit-vector))  ; ppc:228 (cr5)
    (b.eq @bit)                         ; ppc:241
    (cmp subtag (:$ arm64::subtag-complex-double-float-vector)) ; ppc:229 (cr6)
    (b.eq @16bytes)                     ; ppc:239 (beq cr6)
    ;; 8-bit ivectors have the largest other-bit array subtags except
    ;; bit-vector (already dispatched): subtag >= min-8-bit-ivector-subtag
    ;; means 1 byte/element, else 16-bit (2 bytes) — x86-utils.lisp:160-167.
    (cmp subtag (:$ arm64::min-8-bit-ivector-subtag))
    (b.hs @1byte)                       ; ppc:235 (beq cr3 — 8-bit class)
    (lsl bytes elements (:$ 1))         ; ppc:240 (sldi bytes elements 1)
    (b @bump)
    @1byte
    (mov bytes elements)                ; ppc:234 (mr bytes elements)
    (b @bump)
    @bit
    (add bytes elements (:$ 7))         ; ppc:242 (la elements 7 elements)
    (lsr bytes bytes (:$ 3))            ; ppc:243 (srdi bytes elements 3)
    (b @bump)
    @16bytes
    (lsl bytes elements (:$ 4))         ; ppc:238 (sldi bytes elements 4)
    (b @bump)
    @4bytes
    (lsl bytes elements (:$ 2))         ; ppc:236 (sldi bytes elements 2)
    (b @bump)
    @8bytes
    (lsl bytes elements (:$ 3))         ; ppc:231 (sldi bytes elements 3)
    @bump
    ;; header word + round up to a 16-byte dnode.  ppc:245-246
    (add bytes bytes (:$ (+ 8 15)))     ; ppc:245 (la bytes (+ 8 15) bytes)
    (and bytes bytes (:$ (ldb (byte 64 0) (lognot 15)))) ; ppc:246 (clrrdi 4)
    (add obj obj bytes)                 ; ppc:247
    @test
    (cmp obj limit)                     ; ppc:249 (cmpld — unsigned)
    (b.lo @loop)                        ; ppc:250 (blt)
    (vpop limit)                        ; ppc:251
    (vpop obj)                          ; ppc:252
    (vpop fun)                          ; ppc:253
    (restore-full-lisp-context)         ; ppc:254
    (ret)))                             ; ppc:255 (blr)

;;; =====================================================================
;;; %revive-macptr — ppc-utils.lisp:632 (unguarded)
;;; =====================================================================
;;; Demand: 16m9a l1-aprims (revives the pointer defloadvars around
;;; l1-aprims:50).  Store subtag-macptr into the object's subtag byte —
;;; his misc-subtag-offset = misc-header-offset = -12 (LE low byte of
;;; the header word), arm64-arch.lisp:251-253.
(defarm64lapfunction %revive-macptr ((p arg_z))
  (mov imm0 (:$ arm64::subtag-macptr))     ; ppc:633 (li)
  ;; misc-subtag-offset is NEGATIVE (-12): byte stores need the unscaled
  ;; STURB form (the seeds' "-4 unscaled STUR" class, byte flavor).
  (sturb (:w imm0) (:@ p (:$ arm64::misc-subtag-offset))) ; ppc:634 (stb)
  (ret))                                   ; ppc:635 (blr)

;;; =====================================================================
;;; %class-of-instance — ppc-utils.lisp:459
;;; =====================================================================
;;; Sibling of class-of (same donor block): *class-table* routes
;;; standard-instances here, so the 16m10 demand pulls both.
;;; instance.class-wrapper and %wrapper-class are CCL-package constants
;;; from library/lispequ.lisp def-accessors — target-independent slot
;;; indices; the svref lapmacro supplies the misc-data-offset/ldur shape.
(defarm64lapfunction %class-of-instance ((i arg_z))
  (svref arg_z instance.class-wrapper i)  ; ppc:460
  (svref arg_z %wrapper-class arg_z)      ; ppc:461
  (ret))                                  ; ppc:462 (blr)

;;; =====================================================================
;;; class-of — ppc-utils.lisp:464
;;; =====================================================================
;;; Demand: 16m10 frontier — l1-clos-boot toplevels die at
;;; `undefined function CLASS-OF` (uuo 0x23d udf-call, nargs 1).
;;;
;;; Dispatch index into *class-table* (256-entry vector filled by
;;; l1-clos-boot under patch 0001's arm64 branch): subtag byte for
;;; fulltag-misc objects, else the low byte of the object itself — the
;;; per-slice fills cover every fulltag whose payload reaches the low
;;; byte (fixnum payload / cons+nil address bits in bits 4-7), and
;;; immediates/single-floats keep their payload above bit 7 so their
;;; low byte IS the canonical typecode (patch 0001 U-RATIFY note).
;;;
;;; ARM64-DEVIATION (symbols, misc SUBTAGS on PPC64, carry their own
;;; POINTER fulltag here — 7; functions are ordinary miscobjs since the
;;; fulltag-function removal, patch 0055):
;;;  1. arg dispatch: a symbol pointer's ADDRESS bits reach the low byte
;;;     and patch 0001 deliberately leaves those slices unfilled —
;;;     canonicalize to the BARE fulltag (the table holds the class at
;;;     v[fulltag-symbol]; functions go through @misc to
;;;     v[subtag-function] like every other uvector).
;;;  2. table-entry functionp: PPC's own extract-typecode vs
;;;     subtag-function test (ppc:482-483), valid again under unified
;;;     tags.
;;; Tail transfers (entry fn / no-class-error) use the EQUAL canon
;;; (arm64-pred.lisp:209-212): fcell + slot-0 load + br; PPC's
;;; `ba .SPjmpsym` has no upstream subprim.
(defarm64lapfunction class-of ((x arg_z))
  (check-nargs 1)                        ; ppc:465
  (extract-fulltag imm0 x)               ; ppc:466
  (cmp imm0 (:$ arm64::fulltag-misc))    ; ppc:467 (cmpri)
  (b.eq @misc)                           ; ppc:468
  (cmp imm0 (:$ arm64::fulltag-symbol))  ; deviation 1: bare fulltag
  (b.eq @done)                           ; (functions are misc-tagged now:
                                         ;  fulltag-function removed, 0055)
  (extract-lowbyte imm0 x)               ; ppc:469
  (b @done)                              ; ppc:470
  @misc
  (extract-subtag imm0 x)                ; ppc:472
  @done
  (lsl imm0 imm0 (:$ arm64::word-shift)) ; ppc:474 (slri)
  (load-nfn-constant temp1 *class-table*) ; ppc:475 (ldr temp1 '*class-table* nfn)
  (sub imm0 imm0 (:$ (- arm64::misc-data-offset))) ; ppc:476 (addi; negative imm ⇒ sub)
  (ldur temp1 (:@ temp1 (:$ arm64::symbol.vcell))) ; ppc:477 (vcell = 9, unscaled)
  (ldr temp0 (:@ temp1 imm0))            ; ppc:478 (ldrx — reg-offset load)
  (cmp temp0 rnil)                       ; ppc:479 (cmpri cr0 temp0 nil)
  (b.eq @bad)                            ; ppc:480
  ;; functionp? — deviation 2 (PPC shape restored, patch 0055)
  (extract-typecode imm1 temp0)          ; ppc:482
  (cmp imm1 (:$ arm64::subtag-function)) ; ppc:483 (cmpri subtag-function)
  (b.ne @ret)                            ; ppc:484 — not function: return entry
  ;; tail-call the entry with x still in arg_z  ppc:486-490
  (mov nfn temp0)                        ; ppc:486 (mr)
  (ldur imm0 (:@ nfn (:$ arm64::misc-function-offset))) ; ppc:487 (ldr misc-data-offset)
  (set-nargs 1)                          ; ppc:488
  (br imm0)                              ; ppc:489-490 (mtctr/bctr)
  @bad
  (load-nfn-constant fname no-class-error) ; ppc:492
  ;; ppc:493 (ba .spjmpsym) — no-link tail jump per the EQUAL canon;
  ;; nargs still 1 from entry (as on PPC), arg_z still x.
  (ldur nfn (:@ fname (:$ arm64::symbol.fcell)))
  (ldur imm0 (:@ nfn (:$ arm64::misc-function-offset)))
  (br imm0)
  @ret
  (mov arg_z temp0)                      ; ppc:495 — return frob from table
  (ret))                                 ; ppc:496 (blr)

;;; =====================================================================
;;; gc — ppc-utils.lisp:507.  PROMOTED 16m34 from the wave-3 draft
;;; (drafts/arm64-utils.lisp:514) on LIVE REPL demand: rung 17 of the
;;; capability ladder reported "Undefined function GC called with
;;; arguments () ", the only failure in an otherwise 18/19-green run.
;;;
;;; Only `gc` is lifted, not the rest of the ppc:507-616 GC-trap family
;;; (egc, %configure-egc, gc-verbose, ...), per this file's demand-driven
;;; convention stated in the header — those come when a boot names them.
;;;
;;; PPC's (trlgei allocptr 0) is an always-true conditional trap the
;;; kernel decodes as the GC trap, with the selector in imm0.  AArch64
;;; has Matt's dedicated uuo-gc-trap template instead, the same way the
;;; x86-64 version uses its own (x86-utils.lisp:367).
;;;
;;; TRAP ENCODING VERIFIED END-TO-END (this was the one real risk, since a
;;; selector the kernel does not recognize would crash the REPL instead of
;;; erroring cleanly):
;;;   - canon: lisp-kernel/arm64-uuo.s — misc-format UUOs are
;;;     `udf #((info << 2) | uuo_format_misc)` with uuo_format_misc = 0,
;;;     and `uuo_gc_trap` is `uuo_misc 2`, i.e. the word 0x08.
;;;   - compiler: Matt's arm64-asm.lisp:438 still carries a STALE 3-bit
;;;     format ((ash 2 3)|#x7 = 0x17), but OUR patch
;;;     0012-arm64-asm-uuo-templates-renumber.patch rewrites it to
;;;     (ash 2 2) = 0x08, matching the canon.
;;;   - kernel: arm64-exceptions.c:229 GC_TRAP_INSTRUCTION = 0x00000008,
;;;     tested at :1244, dispatching handle_gc_trap (:611), which reads the
;;;     selector from imm0 and the arg from imm1 — exactly what this
;;;     function sets up.  (Note that uuo_misc_gc_trap has no case in the
;;;     handle_uuo misc switch, and correctly so: the GC trap is caught by
;;;     the GC_TRAP_INSTRUCTION test BEFORE handle_uuo is reached.)
;;; =====================================================================
(defarm64lapfunction gc ()
  (check-nargs 0)                           ; ppc:508
  (mov imm0 (:$ arch::gc-trap-function-gc)) ; ppc:509 (li)
  (uuo-gc-trap)                             ; ppc:510 (trlgei allocptr 0)
  (mov arg_z rnil)                          ; ppc:511 (li arg_z target-nil)
  (ret))                                    ; ppc:512 (blr)

;;; =====================================================================
;;; %allocate-list — PPC64 line-port of ppc-utils.lisp:602 (16m48).
;;; PROMOTED from drafts/arm64-utils.lisp with both of the corrections the
;;; drafting protocol names, each now a LIVE facility rather than a DECIDE:
;;;
;;;   * the draft's `(brk (:$ #xf012))' placeholder becomes the real
;;;     `(uuo-interr arch::error-allocate-list)'.  The lapmacro is defined
;;;     (drafts/arm64-lapmacros-additions.lisp:660) and already carries two
;;;     PROMOTED callers in arm64-misc.lisp:598/604 (%suspend-other-threads,
;;;     %resume-other-threads), so the misc-format interr encoding is proven
;;;     end to end; the kernel decodes error_allocate_list (=18,
;;;     compiler/arch.lisp:68) at arm64-exceptions.c:1664 and services it
;;;     with allocate_list() at :522, which builds the whole list — GCing at
;;;     most once — and returns it in arg_z.
;;;   * `(call-subprim .SPnvalret)' becomes `(jump-subprim .SPnvalret)'.
;;;     PPC's `ba' is a TAIL branch, and every donor agrees: ARM32 spjump
;;;     (arm-utils.lisp:337), x86-64 jmp-subprim (x86-utils.lisp:469).
;;;     .SPnvalret is in Matt's own subprim table (arm64-arch.lisp:447).
;;;     This was the 29th and last entry in tools/draft-tail-subprim-lint.py.
;;;
;;; WHY IT MATTERS: l0-aprims.lisp:222 routes MAKE-LIST here for
;;; (>= size (ash 1 16)) — 65536 — and nothing smaller.  Undefined, that is
;;; a CCL::UNDEFINED-FUNCTION-CALL naming CCL::%ALLOCATE-LIST, which is
;;; exactly what LENGTH.LIST.3 `(length (make-list 200000))' signalled; the
;;; measured 16m48 size ladder puts the cliff between 50000 and 100000,
;;; straddling 65536.  One test in 21679 reaches it, which is why a whole
;;; missing definition survived this long.
;;;
;;; Two values are returned and the caller keeps one — l0-aprims wraps the
;;; call in (values ...).  Faithful to every donor: PPC pushes arg_z then
;;; arg_y, ARM32 the same, x86-64 pushes arg_z then allocptr.  PPC64 is our
;;; donor, so arg_y it is.
;;; =====================================================================
(defarm64lapfunction %allocate-list ((initial-element arg_y) (nconses arg_z))
  (check-nargs 2)                             ; ppc:603
  (save-lisp-context)                         ; ppc:604
  (uuo-interr arch::error-allocate-list)      ; ppc:605 uuo_interr ... rzero
  (vpush arg_z)                               ; ppc:606 (the list)
  (vpush arg_y)                               ; ppc:607
  (set-nargs 2)                               ; ppc:608
  (jump-subprim .SPnvalret))                  ; ppc:609 (ba — TAIL branch)
