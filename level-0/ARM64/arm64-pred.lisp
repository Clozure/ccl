;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;; arm64-pred.lisp — ACTIVE seed (demand-driven from cold-load fatals;
;;; first demand: EQL, 16m5m boot).
;;; PPC64 LINE-PORT (source: vendor/ccl/level-0/PPC/ppc-pred.lisp)
;;; Per-line citations: "; ppc:NNN" = line NNN of that file
;;; (the #+ppc64-target branches).

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "ARM64-LAPMACROS"))

;;; =====================================================================
;;; eql — ppc:130 (PPC64 branch)
;;; =====================================================================
;;; PPC's parallel CR-field compares are re-serialized: each cmp/branch
;;; pair is adjacent (same transform as %set-tcr-toplevel-function).
;;; The @macptr dispatch happens BEFORE the full-header equality test
;;; (ppc:153 sits above ppc:156): macptrs of different lengths are EQL
;;; when their subtags and addresses match.
(defarm64lapfunction eql ((x arg_y) (y arg_z))
  "Return T if OBJ1 and OBJ2 represent the same object, otherwise NIL."
  (check-nargs 2)                                    ; ppc:132
  @tail
  (cmp x y)                                          ; ppc:134
  (b.eq @win)                                        ; ppc:139
  (extract-fulltag imm0 x)                           ; ppc:135
  (extract-fulltag imm1 y)                           ; ppc:136
  (cmp imm0 (:$ arm64::fulltag-misc))                ; ppc:137
  (b.ne @lose)                                       ; ppc:140
  (cmp imm1 (:$ arm64::fulltag-misc))                ; ppc:138
  (b.ne @lose)                                       ; ppc:141
  ;; Objects are both fulltag-misc.  Headers must match exactly;
  ;; dispatch on subtag.  (ppc:142-143)
  (getvheader imm0 x)                                ; ppc:144
  (getvheader imm1 y)                                ; ppc:145
  (extract-lowbyte imm2 imm1)                        ; ppc:147 (y subtag; imm1 keeps the full header)
  (cmp imm2 (:$ arm64::subtag-macptr))               ; ppc:148
  (b.eq @macptr)                                     ; ppc:153
  (cmp imm0 imm1)                                    ; ppc:146
  (b.ne @lose)                                       ; ppc:156
  (cmp imm2 (:$ arm64::subtag-bignum))               ; ppc:149
  (b.eq @bignum)                                     ; ppc:157
  (cmp imm2 (:$ arm64::subtag-complex-single-float)) ; ppc:151
  (b.eq @complex-single)                             ; ppc:158
  (cmp imm2 (:$ arm64::subtag-complex-double-float)) ; ppc:152
  (b.eq @complex-double)                             ; ppc:159
  (cmp imm2 (:$ arm64::subtag-double-float))         ; ppc:150
  (b.eq @double-float)                               ; ppc:160
  (cmp imm2 (:$ arm64::subtag-complex))              ; ppc:154
  (b.eq @node)                                       ; ppc:161
  (cmp imm2 (:$ arm64::subtag-ratio))                ; ppc:155
  (b.eq @node)                                       ; ppc:162
  @lose
  (mov arg_z rnil)                                   ; ppc:164
  (ret)                                              ; ppc:165
  @double-float
  ;; One 64-bit compare covers his two 32-bit value cells (ppc64 ld).
  (ldur imm0 (:@ x (:$ arm64::double-float.value)))  ; ppc:167
  (ldur imm1 (:@ y (:$ arm64::double-float.value)))  ; ppc:168
  @test
  (cmp imm0 imm1)                                    ; ppc:170
  (b.ne @lose)                                       ; ppc:171
  @win
  (add arg_z rnil (:$ arm64::t-offset))              ; ppc:173
  (ret)                                              ; ppc:174
  ;; Macptr objects can have different lengths, but their subtags must
  ;; match.  (ppc:175-176)
  @macptr
  (extract-lowbyte imm0 imm0)                        ; ppc:178
  (cmp imm0 imm2)                                    ; ppc:179 (imm2 = y subtag)
  (b.ne @lose)                                       ; ppc:180
  (ldur imm0 (:@ x (:$ arm64::macptr.address)))      ; ppc:181
  (ldur imm1 (:@ y (:$ arm64::macptr.address)))      ; ppc:182
  (b @test)                                          ; ppc:183
  ;; Ratio or complex: corresponding node parts of both objects must be
  ;; EQL.  Recurse on numer/realpart; tail-call on denom/imagpart.
  ;; complex.realpart == ratio.numer (slot 0) and complex.imagpart ==
  ;; ratio.denom (slot 1) on his layout, as on PPC64 (ppc:189-190 aka
  ;; comment).
  @node                                              ; ppc:184-185 (@ratio/@complex)
  (vpush x)                                          ; ppc:186
  (vpush y)                                          ; ppc:187
  (save-lisp-context)                                ; ppc:188
  (ldur x (:@ x (:$ arm64::ratio.numer)))            ; ppc:189
  (ldur y (:@ y (:$ arm64::ratio.numer)))            ; ppc:190
  (bl @tail)                                         ; ppc:191
  (cmp arg_z rnil)                                   ; ppc:192
  (restore-full-lisp-context)                        ; ppc:193
  (vpop y)                                           ; ppc:194
  (vpop x)                                           ; ppc:195
  (b.eq @lose)                                       ; ppc:196
  (ldur x (:@ x (:$ arm64::ratio.denom)))            ; ppc:197
  (ldur y (:@ y (:$ arm64::ratio.denom)))            ; ppc:198
  (b @tail)                                          ; ppc:199
  @complex-single
  (mov imm0 (:$ 2))                                  ; ppc:201
  (mov imm1 (:$ arm64::complex-single-float.realpart)) ; ppc:202
  (b @bignum-next)                                   ; ppc:203
  @complex-double
  (mov imm0 (:$ 4))                                  ; ppc:205
  (mov imm1 (:$ arm64::complex-double-float.realpart)) ; ppc:206
  (b @bignum-next)                                   ; ppc:207
  @bignum
  ;; x's header is in imm0 and y's is identical (compared above); the
  ;; element count controls the loop, which runs at least once — there
  ;; is no 0-element bignum.  (ppc:209-212)
  (header-size imm0 imm0)                            ; ppc:213
  (mov imm1 (:$ arm64::misc-data-offset))            ; ppc:214
  @bignum-next
  ;; 32-bit digits; ldr(:w) zero-extends so the 64-bit cmp is exact.
  (ldr (:w imm2) (:@ x imm1))                        ; ppc:217 (lwzx)
  (ldr (:w imm3) (:@ y imm1))                        ; ppc:218 (lwzx)
  (cmp imm2 imm3)                                    ; ppc:219
  (b.ne @lose)                                       ; ppc:222
  (subs imm0 imm0 (:$ 1))                            ; ppc:220 + ppc:216's last-time test
  (add imm1 imm1 (:$ 4))                             ; ppc:221 (flags preserved)
  (b.ne @bignum-next)                                ; ppc:223
  (b @win))                                          ; ppc:224-225 (li arg_z t; blr)

;;; =====================================================================
;;; equal — ppc:302 (PPC64 branch)
;;; =====================================================================
;;; Demand: 16m8 l1-symhash wall — MAKE-HASH-TABLE's (eq test #'equal)
;;; resolved EQUAL's fcell to the canonical udf object and stored it as
;;; nhash.compareF; the first colliding %HASH-PROBE funcalled it (fatal
;;; mis-named FAST-MOD-3 from stale temp3 — that fn was fine).
;;; PPC's parallel CR-field compares re-serialized per the eql pattern
;;; above.  ARM64-DEVIATION: ppc:339-340's cs-limit stack probe (tdllt)
;;; omitted — the lane's type/trap UUOs are brk placeholders pending the
;;; uuo-canon RATIFY sweep; a deep-recursion overflow dies loudly via the
;;; P0 SEGV dump instead of a lisp error.
;;; Named calls: linked = call-symbol (lapmacros-additions, offsets
;;; verified there); the hairy-equal tail jump is the same sequence with
;;; br (ppc:370-371 ld fname / ba .SPjmpsym).
(defarm64lapfunction equal ((x arg_y) (y arg_z))
  "Return T if X and Y are EQL or if they are structured components
  whose elements are EQUAL. Strings and bit-vectors are EQUAL if they
  are the same length and have identical components. Other arrays must be
  EQ to be EQUAL.  Pathnames are EQUAL if their components are."
  (check-nargs 2)                                    ; ppc:307
  @top
  (cmp x y)                                          ; ppc:309 (cr0)
  (b.eq @win)                                        ; ppc:315
  (extract-fulltag imm0 x)                           ; ppc:310
  (extract-fulltag imm1 y)                           ; ppc:311
  (cmp imm0 imm1)                                    ; ppc:312 (cr1)
  (b.ne @lose)                                       ; ppc:316
  (cmp imm0 (:$ arm64::fulltag-cons))                ; ppc:313 (cr2)
  (b.eq @cons)                                       ; ppc:317
  (cmp imm0 (:$ arm64::fulltag-misc))                ; ppc:314 (cr3)
  (b.eq @misc)                                       ; ppc:318
  @lose
  (mov arg_z rnil)                                   ; ppc:320
  (ret)                                              ; ppc:321
  @win
  (add arg_z rnil (:$ arm64::t-offset))              ; ppc:323
  (ret)                                              ; ppc:324
  @cons
  ;; If the CARs are EQ, avoid saving context: tail-iterate on the CDRs.
  (%car temp0 x)                                     ; ppc:328
  (%car temp1 y)                                     ; ppc:329
  (cmp temp0 temp1)                                  ; ppc:330
  (b.ne @recurse)                                    ; ppc:331
  (%cdr x x)                                         ; ppc:332
  (%cdr y y)                                         ; ppc:333
  (b @top)                                           ; ppc:334
  @recurse
  (vpush x)                                          ; ppc:336
  (vpush y)                                          ; ppc:337
  (save-lisp-context)                                ; ppc:338
  (mov x temp0)                                      ; ppc:341
  (mov y temp1)                                      ; ppc:342
  (bl @top)                                          ; ppc:343
  (cmp arg_z rnil)                                   ; ppc:344
  ;; ppc:345 (mr nfn fn): fn = SELF for this frame's lifetime (save's
  ;; mov fn nfn; callees restore fn); nfn may be clobbered (inner @misc
  ;; leaves nfn = EQL's object).  Re-establish nfn = self BEFORE the
  ;; restore flips fn back to the caller — the b @top loop's next
  ;; save-lisp-context re-derives fn from nfn (16m8 KEY-fatal class).
  (mov nfn fn)                                       ; ppc:345
  (restore-full-lisp-context)                        ; ppc:346 (fn = CALLER after this)
  (vpop y)                                           ; ppc:347
  (vpop x)                                           ; ppc:348
  (b.eq @lose)                                       ; ppc:349
  (%cdr x x)                                         ; ppc:350
  (%cdr y y)                                         ; ppc:351
  (b @top)                                           ; ppc:352
  @misc
  ;; Both uvectors: try EQL; if that fails, tail-call HAIRY-EQUAL
  ;; (late-bound by name — defined in level-1 sysutils).
  (vpush x)                                          ; ppc:355
  (vpush y)                                          ; ppc:356
  (save-lisp-context)                                ; ppc:357
  (set-nargs 2)                                      ; ppc:358/361
  (call-symbol eql)                                  ; ppc:360+362 (bla .SPjmpsym)
  (cmp arg_z rnil)                                   ; ppc:363
  ;; ppc:364 (mr nfn fn): as at @recurse — after the restore below, fn is
  ;; the CALLER's fn, and 16m8's boot proved it (fn-relative hairy-equal
  ;; load fetched KEY from %HASH-PROBE's pool -> udf "KEY").  nfn = self
  ;; makes the donor's nfn-relative constant load (ppc:370) work.
  (mov nfn fn)                                       ; ppc:364
  (restore-full-lisp-context)                        ; ppc:365 (fn = CALLER after this)
  (vpop y)                                           ; ppc:366
  (vpop x)                                           ; ppc:367
  (b.ne @win)                                        ; ppc:368
  (set-nargs 2)                                      ; ppc:369
  (load-nfn-constant fname hairy-equal)              ; ppc:370 (ld fname 'hairy-equal nfn)
  (ldur nfn (:@ fname (:$ arm64::symbol.fcell)))     ; ppc:371 (ba .SPjmpsym, no-link)
  (ldur imm0 (:@ nfn (:$ arm64::misc-function-offset)))
  (br imm0))
