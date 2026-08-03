;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;; arm64-def.lisp — ACTIVE seed (demand-driven from cold-load fatals;
;;; lifted from upstream-port/level-0/drafts/arm64-def.lisp wave-4).
;;; PPC64 LINE-PORT (source: vendor/ccl/level-0/PPC/ppc-def.lisp)
;;; Per-line citations: "; ppc:NNN" = line NNN of that file.

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "ARM64-LAPMACROS"))











;;; =====================================================================
;;; %function-vector-to-function / %function-to-function-vector — IDENTITY
;;; since the fulltag_function removal (patch 0055): a function IS its
;;; uvector, as on PPC64 (one misc-tagged object).  The type check is
;;; kept (x86-def.lisp:21 precedent) so a non-function still traps; the
;;; retag arithmetic is gone.  Callers (nfasload/l0-def/l1 builders)
;;; flow through unchanged.
(defarm64lapfunction %function-vector-to-function ((arg arg_z))
  (trap-unless-typecode= arg arm64::subtag-function) ; x86:21
  (ret))

(defarm64lapfunction %function-to-function-vector ((arg arg_z))
  (trap-unless-typecode= arg arm64::subtag-function)
  (ret))

;;; %nth-immediate — immediate (constant) N of a function.  Donor by NAME
;;; = x86-def.lisp:37, but the body follows MATT'S arm64 function shape
;;; {code-vector@slot0, constants@slot1..} (16k4 / e4440cb), NOT x8664's
;;; inline-code layout: immediate n = uvector slot (1+ n).  fun is
;;; misc-tagged (fulltag-function removed, patch 0055): slot(1+n) sits at
;;; fun-12+8+8(1+n) = fun+4+8n, and boxed n IS 8n — one add + unscaled ldur.
(defarm64lapfunction %nth-immediate ((fun arg_y) (n arg_z))
  (trap-unless-typecode= fun arm64::subtag-function)
  (add imm0 fun n)
  (ldur arg_z (:@ imm0 (:$ (+ arm64::misc-data-offset arm64::node-size))))
  (ret))

;;; %set-nth-immediate — setter twin, donor x86-def.lisp:45 (16m11b
;;; demand: l1-clos-boot gf-dcode install).  Store goes through .SPgvset
;;; for the GC write barrier (x86:50 jmp .SPgvset), never inline.
;;; gvset contract (spentry-B:77): arg_x = misc-tagged vector, arg_y =
;;; boxed slot index (= byte offset at fixnumshift 3), arg_z = value.
;;; Slot = (1+ n) on Matt's {cv@0, imms@1..} shape — x86:47-48's
;;; code-words lookup drops out (no inline code here).
(defarm64lapfunction %set-nth-immediate ((fun arg_x) (n arg_y) (new arg_z))
  (trap-unless-typecode= fun arm64::subtag-function)  ; x86:46
  (add arg_y n (:$ (ash 1 arm64::fixnumshift)))       ; boxed 1+n
  ;; fun is already the misc-tagged vector (fulltag-function removed,
  ;; patch 0055) — no retag before .SPgvset.
  (jump-subprim .SPgvset))                            ; x86:50

;;; closure-function — x86-def.lisp:496 verbatim (his function shape:
;;; a closure's inner function = immediate 0, with the vector
;;; indirection for lfun-vector-reached cases).  16m5t demand.
(defun closure-function (fun)
  (while (and (functionp fun)  (not (compiled-function-p fun)))
    (setq fun (%nth-immediate fun 0))
    (when (vectorp fun)
      (setq fun (svref fun 0))))
  fun)

;;; =====================================================================
;;; PPC compares nargs on cr0 BEFORE check-nargs; Matt's check-nargs is
;;; itself cmp-based (one NZCV) so the compare moves AFTER it (x86:175 order).

(defarm64lapfunction %fixnum-ref ((fixnum arg_y) #| &optional |# (offset arg_z))
  (check-nargs 1 2)                     ; ppc:75
  (cmp nargs (:$ (ash 1 arm64::fixnumshift))) ; ppc:74
  (b.ne @2-args)                        ; ppc:76
  (mov fixnum offset)                   ; ppc:77 (mr)
  (mov offset (:$ 0))                   ; ppc:78 (li)
  @2-args
  (unbox-fixnum imm0 offset)            ; ppc:80
  (ldr arg_z (:@ fixnum imm0))          ; ppc:81 ldrx — regoff form (wave-3)
  (ret))                                ; ppc:82

;;; %fixnum-ref-natural — ppc:98 (#+ppc64-target arm; ppc32 twin ppc:86 skipped)
(defarm64lapfunction %fixnum-ref-natural ((fixnum arg_y) #| &optional |# (offset arg_z))
  (check-nargs 1 2)                     ; ppc:100
  (cmp nargs (:$ (ash 1 arm64::fixnumshift))) ; ppc:99
  (b.ne @2-args)                        ; ppc:101
  (mov fixnum offset)                   ; ppc:102
  (mov offset (:$ 0))                   ; ppc:103
  @2-args
  (unbox-fixnum imm0 offset)            ; ppc:105
  (ldr imm0 (:@ fixnum imm0))           ; ppc:106 ldx
  ;; ppc:107 (ba .SPmakeu64) — TAIL jump.  DECIDE W4-D10 (not in table; needs
  ;; jmp-subprim/no-link form) + W4-D11 (imm0 is .SPmakeu64's INPUT and
  ;; call-subprim's dispatch scratch — broken until scratch moves off imm0).
  (jump-subprim .SPmakeu64))

;;; %fixnum-set — ppc:109
(defarm64lapfunction %fixnum-set ((fixnum arg_x) (offset arg_y) #| &optional |# (new-value arg_z))
  (check-nargs 2 3)                     ; ppc:111
  (cmp nargs (:$ (ash 2 arm64::fixnumshift))) ; ppc:110
  (b.ne @3-args)                        ; ppc:112
  (mov fixnum offset)                   ; ppc:113
  (mov offset (:$ 0))                   ; ppc:114
  @3-args
  (unbox-fixnum imm0 offset)            ; ppc:116
  (str new-value (:@ fixnum imm0))      ; ppc:117 strx — regoff form
  ;; ppc:118 (mr arg_z new-value) — new-value IS arg_z; no-op elided
  (ret))                                ; ppc:119

;;; %fixnum-set-natural — ppc:159 (#+ppc64-target arm; ppc32 twin ppc:122 skipped)
;;; DEVIATION (DECIDE W4-D19): the PPC64 body pokes bignum digits inline —
;;; 32-bit digits in BIG-ENDIAN order (ld+rotldi swap, ppc:176-190); that
;;; layout does not transfer to little-endian arm64.  The x86-64 twin
;;; (x86:213-226) instead calls .SPgetu64 to unbox-with-typecheck; mirror it
;;; (bignum digits are 32-bit on Matt's design too, arch :143).
(defarm64lapfunction %fixnum-set-natural ((fixnum arg_x) (offset arg_y) #| &optional |# (new-value arg_z))
  (check-nargs 2 3)                     ; ppc:161
  (save-lisp-context)                   ; x86:216 save-simple-frame
  (cmp nargs (:$ (ash 2 arm64::fixnumshift))) ; ppc:160 (frame build flag-safe)
  (b.ne @3-args)                        ; ppc:162
  (mov fixnum offset)                   ; ppc:163
  (mov offset (:$ 0))                   ; ppc:164
  @3-args
  ;; .SPgetu64: arg_z -> unboxed u64 in imm0, type-error if not (x86:222).
  ;; DECIDE W4-D10 (not in Matt's table; exists in our spentry-B drafts).
  (call-subprim .SPgetu64)
  (unbox-fixnum imm1 offset)            ; ppc:166 / x86:223
  (str imm0 (:@ fixnum imm1))           ; ppc:192 stdx / x86:224
  (restore-full-lisp-context)           ; x86:225
  ;; arg_z (= new-value) preserved by .SPgetu64 — ppc:193 (mr arg_z new-value)
  (ret))                                ; ppc:194

;;; =====================================================================
;;; Frame / stack accessors — ppc:197-273







;;; =====================================================================

(defarm64lapfunction %get-object ((macptr arg_y) (offset arg_z))
  (check-nargs 2)                       ; ppc:1127
  (trap-unless-typecode= arg_y arm64::subtag-macptr) ; ppc:1128 (W4-D16 brk)
  (macptr-ptr imm0 arg_y)               ; ppc:1129
  (trap-unless-lisptag= arg_z arm64::tag-fixnum imm1) ; ppc:1130 (W4-D16 brk)
  (unbox-fixnum imm1 arg_z)             ; ppc:1131
  (ldr arg_z (:@ imm0 imm1))            ; ppc:1132 ldrx — regoff
  (ret))                                ; ppc:1133

(defarm64lapfunction %set-object ((macptr arg_x) (offset arg_y) (value arg_z))
  (check-nargs 3)                       ; ppc:1137
  (trap-unless-typecode= arg_x arm64::subtag-macptr) ; ppc:1138
  (macptr-ptr imm0 arg_x)               ; ppc:1139
  (trap-unless-lisptag= arg_y arm64::tag-fixnum imm1) ; ppc:1140
  (unbox-fixnum imm1 arg_y)             ; ppc:1141
  (str arg_z (:@ imm0 imm1))            ; ppc:1142 strx — regoff
  (ret))                                ; ppc:1143

;;; =====================================================================
;;; Method-context apply family — ppc:1146-1224

;;; =====================================================================
;;; %make-code-executable — ppc:22
;;; =====================================================================
;;; FF-call MakeDataExecutable for I/D-cache sync — REQUIRED on arm64
;;; (freshly loaded fasl code vectors must be flushed before execution),
;;; unlike the x86-64 twin's no-op (x86:98).  PPC builds a PowerOpen
;;; c-frame + .SPpoweropen-ffcall; here the AAPCS64 c_frame + .SPffcall
;;; (kernel contract: `spentry ffcall' in
;;; upstream-port/lisp-kernel/arm64-spentry.s, 16m30):
;;;   {header@0, savedsp@8, param0..7@16..72, 4 reserved boundary
;;;    lisp_frame words@80..111} = 112 bytes / 14 words.
;;; The header is a REAL uvector header whose element count (words-1 = 13)
;;; deliberately COVERS the 4 reserved words.  _SPffcall derives the
;;; boundary-frame base from that count, parks lr there, publishes the
;;; frame by shrinking the count by 4, and on return restores sp from
;;; savedsp -- NOT from offset 0, which is the header.
;;; 16m31: 16m30 renamed backlink/savelr -> header/savedsp but missed this
;;; DEPLOYED twin (its handoff claimed only the undeployed drafts were
;;; left), so this body still referenced the now-unbound
;;; arm64::c-frame.backlink and walled the XLOAD in l0 arm64-def.
;;; save-lisp-context preserves the entry lr across the
;;; call-subprim blr; restore-full-lisp-context pops the lisp frame.
;;; TBI: codev's tag rides in the top byte and is ignored by the kernel's
;;; VA cache-maintenance, so the tagged pointer is a valid address arg.
(defarm64lapfunction %make-code-executable ((codev arg_z))
  (let ((len imm2)
        (word-offset imm0))
    (save-lisp-context)                 ; ppc:25
    (getvheader word-offset codev)      ; ppc:26
    (header-size len word-offset)       ; ppc:27
    ;; ppc:32-33 (stru sp -(c-frame) sp): str sp unencodable (Rt=xzr) —
    ;; stage the saved SP through imm1 (wave-3 idiom).
    (mov imm1 sp)
    (sub sp sp (:$ arm64::c-frame.ffcall-size))  ; 16 head + 8 params + 4 reserved
    (movz imm3 (:$ arm64::c-frame.ffcall-header))
    (str imm3 (:@ sp (:$ arm64::c-frame.header)))
    (str imm1 (:@ sp (:$ arm64::c-frame.savedsp)))
    (sub imm0 codev (:$ (- arm64::misc-data-offset))) ; ppc:34 (la; canonical la→sub idiom)
    (lsl len len (:$ 2))                ; ppc:35 slri — 32-bit code elements
    (str imm0 (:@ sp (:$ arm64::c-frame.param0)))         ; ppc:36
    (str len (:@ sp (:$ (+ arm64::c-frame.param0 8))))    ; ppc:37 param1
    (ref-global imm3 kernel-imports)    ; ppc:38
    (ldr arg_z (:@ imm3 (:$ arm64::kernel-import-makedataexecutable))) ; ppc:39
    (call-subprim .SPffcall)            ; ppc:40 (bla — LINKED call)
    (mov arg_z rnil)                    ; ppc:41
    (restore-full-lisp-context)         ; ppc:42
    (ret)))                             ; ppc:43

;;; =====================================================================
;;; %lookup-subprim-address — ARM-ISA analog (arm-def.lisp:612); no PPC64
;;; body exists because PPC64's `ba' encodes subprim addresses directly
;;; (ppc-callback-support needs no lookup).  Returns the absolute kernel
;;; address of the subprim whose rcontext-relative sptab offset is SUBP —
;;; a boxed fixnum holding the same arm64::subprimitive-offset value the
;;; call-subprim/jump-subprim lapmacros use.  The per-thread sptab
;;; entries are identical static kernel addresses in every thread, so
;;; the current thread's entry is a valid jump target for foreign
;;; trampoline code (consumer: level-1/arm64-callback-support.lisp).
;;; ARM64-DEVIATION: ARM32 tail-jumps .SPmakeu32 to box the address;
;;; kernel text addresses fit a 61-bit low-tag fixnum with room to
;;; spare, and no .SPmakeu64 subprim exists on this lane — box-fixnum.
;;; =====================================================================
(defarm64lapfunction %lookup-subprim-address ((subp arg_z))
  (check-nargs 1)
  (unbox-fixnum imm1 subp)              ; arm:613 (:lsr subp fixnumshift)
  (ldr imm0 (:@ rcontext imm1))         ; arm:613 (sptab entry)
  (box-fixnum arg_z imm0)               ; arm:614 (.SPmakeu32 — see note)
  (ret))

;;; =====================================================================
;;; %get-kernel-global-from-offset — ppc:45
;;; =====================================================================
;;; offset = the (negative) rnil-relative offset from arm64::%kernel-global,
;;; boxed.  PPC adds (target-nil-value); rnil holds tagged nil (x86:151 same).

(defarm64lapfunction %get-kernel-global-from-offset ((offset arg_z))
  (check-nargs 1)                       ; ppc:46
  (unbox-fixnum imm0 offset)            ; ppc:47
  (add imm0 imm0 rnil)                  ; ppc:48 (addi imm0 imm0 nil-value)
  (ldr arg_z (:@ imm0 (:$ 0)))          ; ppc:49
  (ret))                                ; ppc:50

;;; %set-kernel-global-from-offset — ppc:52
(defarm64lapfunction %set-kernel-global-from-offset ((offset arg_y) (new-value arg_z))
  (check-nargs 2)                       ; ppc:53
  (unbox-fixnum imm0 offset)            ; ppc:54
  (add imm0 imm0 rnil)                  ; ppc:55
  (str new-value (:@ imm0 (:$ 0)))      ; ppc:56
  (ret))                                ; ppc:57

;;; %get-kernel-global-ptr-from-offset — ppc:61 (16m48h promotion; third
;;; member of the kernel-global trio above).  Reads the global and stores it
;;; into the caller's macptr rather than boxing it.  macptr.address = -4
;;; (define-fixedsized-object macptr, arm64-arch.lisp:611), not 8-aligned,
;;; so the store is stur, not str.
(defarm64lapfunction %get-kernel-global-ptr-from-offset ((offset arg_y)
                                                         (ptr arg_z))
  (check-nargs 2)                       ; ppc:63
  (unbox-fixnum imm0 offset)            ; ppc:64
  (add imm0 imm0 rnil)                  ; ppc:65
  (ldr imm0 (:@ imm0 (:$ 0)))           ; ppc:66
  (stur imm0 (:@ ptr (:$ arm64::macptr.address))) ; ppc:67 (str -> stur)
  (ret))                                ; ppc:68

;;; =====================================================================
;;; %current-frame-ptr / %current-vsp — ppc:197/202
;;; =====================================================================
(defarm64lapfunction %current-frame-ptr ()
  (check-nargs 0)                       ; ppc:198
  (mov arg_z sp)                        ; ppc:199 (mr arg_z sp; MOV Xd,SP = ADD alias)
  (ret))                                ; ppc:200

(defarm64lapfunction %current-vsp ()
  (check-nargs 0)                       ; ppc:203
  (mov arg_z vsp)                       ; ppc:204
  (ret))                                ; ppc:205

;;; =====================================================================
;;; %set-current-vsp / %current-tsp / %set-current-tsp — ppc:210/216/223
;;; (16m48h promotion; the three siblings of %current-vsp above, which was
;;; promoted alone.)
;;; =====================================================================
;;; Matt's register map has a REAL tsp register (x24), same discipline as
;;; PPC64, so these are direct ports.  The x86-64 twins have to go through
;;; tcr.save-tsp instead (x86-def.lisp:235) — not needed here.
(defarm64lapfunction %set-current-vsp ((new-vsp arg_z))
  (check-nargs 1)                       ; ppc:211
  (mov vsp new-vsp)                     ; ppc:212
  (ret))                                ; ppc:213

(defarm64lapfunction %current-tsp ()
  (check-nargs 0)                       ; ppc:216
  (mov arg_z tsp)                       ; ppc:217
  (ret))                                ; ppc:218

(defarm64lapfunction %set-current-tsp ((new-tsp arg_z))
  (check-nargs 1)                       ; ppc:223
  (mov tsp new-tsp)                     ; ppc:224
  (ret))                                ; ppc:225

;;; =====================================================================
;;; %code-vector-pc — ppc:327 (promoted from the def draft; demand:
;;; 16m17 arm64-trap-support return-address-offset — xcmain/%xerr-disp
;;; fake-frame construction).
;;; =====================================================================
;;; Returns the boxed byte-offset of *pcptr's value within code-vector,
;;; or nil if the PC is outside it.  loc-pc -> imm2; the nil move is
;;; NZCV-safe between cmp and b.hs.  32-bit code elements, so
;;; header-size << 2 = byte size (as PPC64's slri 2, ppc:334).
(defarm64lapfunction %code-vector-pc ((code-vector arg_y) (pcptr arg_z))
  (macptr-ptr imm0 pcptr)               ; ppc:328
  (ldr imm2 (:@ imm0 (:$ 0)))           ; ppc:329 (ldr loc-pc 0 imm0)
  (sub imm0 imm2 code-vector)           ; ppc:330
  ;; ppc:331 (subi imm0 imm0 misc-data-offset).  misc-data-offset is
  ;; NEGATIVE here (-4: fulltag-misc 12, arm64-arch.lisp:268), so
  ;; subtracting it is an add of the positive constant — the assembler
  ;; refuses negative immediates (neg-imm drift class, 16m5).
  (add imm0 imm0 (:$ (- arm64::misc-data-offset)))
  (getvheader imm1 code-vector)         ; ppc:332
  (header-size imm1 imm1)               ; ppc:333
  (lsl imm1 imm1 (:$ 2))                ; ppc:334 slri — 32-bit code elements
  (cmp imm0 imm1)                       ; ppc:335 cmplr (unsigned)
  (mov arg_z rnil)                      ; ppc:336 (li — NZCV-safe)
  (b.hs @ret)                           ; ppc:337 (bgelr)
  (box-fixnum arg_z imm0)               ; ppc:338
  @ret
  (ret))                                ; ppc:339

;;; =====================================================================
;;; Frame-walk / catch-top set — ppc:227/236/241/270/289 (promoted from
;;; the def draft; demand: 16m17 lib/arm64-backtrace cfp-lfun cluster,
;;; 16m21 %frame-backlink runtime call from the trap reporter/backtrace).
;;; lisp-frame layout is kernel ground truth: marker@0 savevsp@8
;;; savefn@16 savelr@24 — 4 nodes = 32 bytes (arm64-constants.h:378
;;; `_struct lisp_frame`; spentry-D-call-builtins.s:60-64).
;;; =====================================================================

;;; %%frame-backlink — ppc:227.  ARM64-DEVIATION: PPC loads a stored
;;; backlink from lisp-frame word @0, but Matt's marker frame has marker@0
;;; and NO stored backlink.  Under the DECIDED cstack-walk design (Option
;;; A, comms/ARM64-CSTACK-WALK-DECISION.md — cstack is a homogeneous 32B
;;; frame chain, nfp + stack-cons on the TSP), the parent (older) frame is
;;; simply the next 32-byte frame at a higher address, so backlink = p+32
;;; (kernel-verified lisp_frame size).  Matches the fixed-frame assumption
;;; the rest of this set already bakes in.  (16m21: this fell through the
;;; quartet promotion and was left undefined, looping the trap reporter.)
(defarm64lapfunction %%frame-backlink ((p arg_z))
  (check-nargs 1)                       ; ppc:228
  (add arg_z arg_z (:$ 32))             ; ppc:229 — lisp_frame.size (32)
  (ret))                                ; ppc:230

;;; %%frame-savefn — ppc:236
(defarm64lapfunction %%frame-savefn ((p arg_z))
  (check-nargs 1)                       ; ppc:237
  (ldr arg_z (:@ arg_z (:$ 16)))        ; ppc:238 lisp-frame.savefn -> @16
  (ret))                                ; ppc:239

;;; %cfp-lfun — ppc:241
;;; Returns (values lfun pc-offset) or (values nil nil).
;;; PPC tests extract-typecode == subtag-function; since the
;;; fulltag-function removal (patch 0055) that is the RIGHT test here
;;; too — a function is a misc-tagged uvector, exactly PPC's shape.
;;; loc-pc register does not exist — savelr staged in imm2.
(defarm64lapfunction %cfp-lfun ((p arg_z))
  (ldr arg_y (:@ p (:$ 16)))            ; ppc:242 lisp-frame.savefn
  (extract-typecode imm0 arg_y)         ; ppc:243
  (ldr imm2 (:@ p (:$ 24)))             ; ppc:245 lisp-frame.savelr -> imm2
  (cmp imm0 (:$ arm64::subtag-function)) ; ppc:244
  (b.ne @no)                            ; ppc:246
  ;; codevector = function slot 0 @ misc-function-offset (-4) — ldur
  (ldur arg_x (:@ arg_y (:$ arm64::misc-function-offset))) ; ppc:247
  (sub imm1 imm2 arg_x)                 ; ppc:248 pc - tagged codevector
  ;; ppc:249 (la imm1 (- misc-data-offset) imm1).  misc-data-offset is
  ;; NEGATIVE here (-4), so subtracting it is an add of the positive
  ;; constant (neg-imm drift class, 16m5).
  (add imm1 imm1 (:$ (- arm64::misc-data-offset)))
  (getvheader imm0 arg_x)               ; ppc:250
  (header-length imm0 imm0)             ; ppc:251 boxed element count
  (cmp imm1 imm0)                       ; ppc:252 cmplr (unsigned; boxed
                                        ; count over-bounds byte length
                                        ; 2x, same slack as PPC64)
  (box-fixnum imm1 imm1)                ; ppc:253 (lsl — NZCV-safe)
  (b.hs @no)                            ; ppc:254 bge (unsigned)
  (vpush arg_y)                         ; ppc:255
  (vpush imm1)                          ; ppc:256
  @go
  (set-nargs 2)                         ; ppc:258
  (add temp0 vsp (:$ (* 2 arm64::node-size))) ; ppc:259 (la temp0 '2 vsp); temp0 = entry-vsp
  (jump-subprim .SPvalues)              ; ppc:260 ba — TAIL no-link
  @no
  (mov imm0 rnil)                       ; ppc:262
  (vpush imm0)                          ; ppc:263
  (vpush imm0)                          ; ppc:264
  (b @go))                              ; ppc:265

;;; %%frame-savevsp — ppc:270
(defarm64lapfunction %%frame-savevsp ((p arg_z))
  (check-nargs 1)                       ; ppc:271
  (ldr arg_z (:@ arg_z (:$ 8)))         ; ppc:272 lisp-frame.savevsp -> @8
  (ret))                                ; ppc:273

;;; %catch-top — ppc:289
;;; tcr.catch-top = 24 (arm64-arch.lisp:744-749 tcr layout, = kernel
;;; arm64-constants.h:441 _struct tcr: next/prev/db_link/catch_top).
;;; The tcr arg is a "fixnum" whose bits ARE the tcr address
;;; (8-aligned), as on PPC64.
(defarm64lapfunction %catch-top ((tcr arg_z))
  (check-nargs 1)                       ; ppc:290
  (ldr arg_z (:@ tcr (:$ arm64::tcr.catch-top))) ; ppc:291
  (cbnz arg_z @ret)                     ; ppc:292-293 (cmpri 0 / bne)
  (mov arg_z rnil)                      ; ppc:294
  @ret
  (ret))                                ; ppc:296

;;; %catch-tsp — ppc:298 (16m48h promotion; the sibling of %catch-top above).
;;; Given a misc-tagged catch frame allocated on the TEMP stack, recover the
;;; tsp frame base: back off fulltag-misc plus the 2-node tsp-frame header.
;;; Identical constant expression to PPC64 — word-shift is 3 on both.
(defarm64lapfunction %catch-tsp ((catch arg_z))
  (check-nargs 1)                       ; ppc:299
  (sub arg_z arg_z (:$ (+ arm64::fulltag-misc
                          (ash 1 (1+ arm64::word-shift))))) ; ppc:300-301 (la neg)
  (ret))                                ; ppc:302

;;; %fixnum-address-of — ppc:308 (16m48h promotion).  Like %address-of
;;; (arm64-utils.lisp:94) but conses nothing: the address is left-shifted
;;; like any other fixnum, so an address that does not fit is simply wrong
;;; rather than boxed into a bignum.  That is PPC64's contract, not a
;;; deviation.
(defarm64lapfunction %fixnum-address-of ((x arg_z))
  (check-nargs 1)                       ; ppc:309
  (box-fixnum arg_z x)                  ; ppc:310
  (ret))                                ; ppc:311

;;; %save-standard-binding-list / %saved-bindings-address — ppc:315/321
;;; (16m48h promotion).  Both reach the vstack area through tcr.vs-area and
;;; then area.high; `push' is the wave-1 predecrement-store lapmacro, which
;;; updates its address register only, exactly like PPC's `stru'.
;;; Neither has a check-nargs on PPC64 either.
(defarm64lapfunction %save-standard-binding-list ((bindings arg_z))
  (ldr imm0 (:@ rcontext (:$ arm64::tcr.vs-area)))  ; ppc:316
  (ldr imm1 (:@ imm0 (:$ arm64::area.high)))        ; ppc:317
  (push bindings imm1)                              ; ppc:318 (stru)
  (ret))                                            ; ppc:319

(defarm64lapfunction %saved-bindings-address ()
  (ldr imm0 (:@ rcontext (:$ arm64::tcr.vs-area)))  ; ppc:322
  (ldr imm1 (:@ imm0 (:$ arm64::area.high)))        ; ppc:323
  (sub arg_z imm1 (:$ arm64::node-size))            ; ppc:324 (la negative)
  (ret))                                            ; ppc:325

;;; %uvector-data-fixnum — ppc:283 (16m48h promotion).  TWO corrections to
;;; the draft, both of them the mechanical promotion fixups:
;;;
;;;   * the draft INLINED the fulltag check as extract-fulltag + cmp + b.eq +
;;;     `(brk (:$ #xf0fd))', with a DECIDE saying trap-unless-fulltag= was not
;;;     among the wave-1 macro additions and no type-error UUO was ratified.
;;;     Both halves are now stale: trap-unless-fulltag= IS defined
;;;     (drafts/arm64-lapmacros-additions.lisp:378) and emits the real
;;;     (uuo-error-reg-not-xtype ...), and its lisptag/typecode siblings
;;;     already have ten promoted callers in this tree.  So this is PPC's own
;;;     one-liner (ppc-def.lisp:285) rather than an inlined placeholder.
;;;   * PPC's `(la arg_z misc-data-offset arg_z)' cannot become an `add': on
;;;     this low-tag design misc-data-offset is -4 and the AArch64 add/sub
;;;     immediate field is unsigned.  Flip to `sub' of the negation, the
;;;     ratified idiom (see arm64-misc.lisp:610-612).  Detector:
;;;     tools/lap-negative-immediate-lint.py.
(defarm64lapfunction %uvector-data-fixnum ((uv arg_z))
  (check-nargs 1)                       ; ppc:284
  (trap-unless-fulltag= arg_z arm64::fulltag-misc) ; ppc:285
  (sub arg_z arg_z (:$ (- arm64::misc-data-offset))) ; ppc:286 (la -> sub)
  (ret))                                ; ppc:287

;;; =====================================================================
;;; %copy-function / replace-function-code / closure-function —
;;; ppc-def.lisp:1227/1237/1245
;;; =====================================================================
;;; Demand: 16m11a frontier — l1-clos-boot gf-dcode install dies at
;;; `undefined function REPLACE-FUNCTION-CODE` (nargs 2).
;;;
;;; Since the fulltag-function removal (patch 0055) functions are plain
;;; misc-tagged uvectors, exactly PPC's shape, and the
;;; %function-to-function-vector calls below are identity-with-typecheck.
;;; Bodies keep PPC64 semantics (codevector @ slot 0, whole-gvector copy
;;; — Matt's function shape is PPC64's, not x8664's inline-code one).
;;; ppc:1245 closure-function is already active above (16m5t, x86 body).

(defun %copy-function (proto &optional target) ; ppc:1227
  (let* ((protov (%function-to-function-vector proto))
         (total-size (uvsize protov))
         (newv (if target
                 (%function-to-function-vector target)
                 (allocate-typed-vector :function total-size))))
    (declare (fixnum total-size))
    (when target
      (unless (eql total-size (uvsize newv))
        (error "Wrong size target ~s" target)))
    (%copy-gvector-to-gvector protov 0 newv 0 total-size)
    (%function-vector-to-function newv)))

(defun replace-function-code (target-fn proto-fn) ; ppc:1237
  (if (typep target-fn 'function)
    (if (typep proto-fn 'function)
      (setf (uvref (%function-to-function-vector target-fn) 0)
            (uvref (%function-to-function-vector proto-fn) 0))
      (report-bad-arg proto-fn 'function))
    (report-bad-arg target-fn 'function)))

;;; =====================================================================
;;; Method-context apply family — ppc:1146-1224 (promoted from the def
;;; draft; demand: 16m11c udf %APPLY-LEXPR-TAIL-WISE, l1-clos-boot; the
;;; two spread siblings ride along — same dcode application paths).
;;; =====================================================================
;;; nargs is a TAGGED fixnum (boxed count IS byte count, fixnumshift=3).
;;; next-method-context = temp1 on Matt's map too (arm64-asm.lisp:203
;;; define-register-alias), same as PPC.
;;; lr-preservation idiom (PPC mflr loc-pc / bla / mtlr loc-pc, no loc-pc
;;; register here): build-lisp-frame, call the subprim, then pop ONLY fn/lr
;;; and discard the frame — vsp must NOT be reloaded because the spread
;;; subprim pushed the spread args on the vstack.  DECIDE W4-D17.
;;; Final dispatch through codevector slot 0 (misc-function-offset -4) + br:
;;; wave-3 DECIDE-6 recurrence (W4-D15).
;;; Kernel bodies exist: spread_lexprz spentry-E:561, spreadargz
;;; spentry-D:631; .SPspread-lexprz registration verified (w12:20).

(defarm64lapfunction %apply-lexpr-with-method-context ((magic arg_x)
                                                       (function arg_y)
                                                       (args arg_z))
  ;; Somebody's called (or tail-called) us.       ; ppc:1149-1154 comments
  (mov next-method-context magic)       ; ppc:1155 (mr temp1 magic)
  (mov nfn function)                    ; ppc:1156
  (set-nargs 0)                         ; ppc:1157
  (build-lisp-frame)                    ; ppc:1158 (mflr loc-pc) — W4-D17
  ;; .SPspread-lexprz preserves nfn/next-method-context (PPC contract;
  ;; spentry-E @4883).  DECIDE W4-D10: not in Matt's table.
  (call-subprim .SPspread-lexprz)      ; ppc:1159 (bla)
  (ldp fn lr (:@ sp (:$ 16)))           ; ppc:1160 (mtlr loc-pc)
  (add sp sp (:$ 32))                   ;   discard frame; vsp NOT reloaded
  (ldur imm0 (:@ nfn (:$ arm64::misc-function-offset))) ; ppc:1161 codevector
  (br imm0))                            ; ppc:1162-1163 (mtctr/bctr) — W4-D15

(defarm64lapfunction %apply-with-method-context ((magic arg_x)
                                                 (function arg_y)
                                                 (args arg_z))
  ;; Same shape; spreads a LIST instead of a lexpr.  ; ppc:1169-1174
  (mov next-method-context magic)       ; ppc:1175
  (mov nfn function)                    ; ppc:1176
  (set-nargs 0)                         ; ppc:1177
  (build-lisp-frame)                    ; ppc:1178 — W4-D17
  (call-subprim .SPspreadargZ)          ; ppc:1179 (bla) — W4-D10
  (ldp fn lr (:@ sp (:$ 16)))           ; ppc:1180
  (add sp sp (:$ 32))
  (ldur imm0 (:@ nfn (:$ arm64::misc-function-offset))) ; ppc:1181
  (br imm0))                            ; ppc:1182-1183 — W4-D15

;;; %apply-lexpr-tail-wise — ppc:1188
;;; Preconditions ppc:1189-1199 apply verbatim (lexpr via .SPlexpr-entry;
;;; LR = lexpr-cleanup code; cleanup EQ ret1valaddr => discard the extra
;;; MV frame).  Frame field offsets are the MARKER frame's (savevsp@8,
;;; savefn@16, savelr@24; W4-D18 — matches build-lisp-frame/
;;; restore-lisp-frame's {marker,vsp,fn,lr}); nargs loaded from the lexpr
;;; is BOXED (byte count), so (sub vsp imm0 nargs) is unscaled exactly as
;;; PPC64.
;;; PPC keeps cr0/cr1/cr2 live; single NZCV -> cr2 resolved first, then the
;;; nargs compares re-issued at the pop points (vpop/ldp are flag-safe).
;;; ppc:1204 (mr imm5 nargs) elided — VESTIGIAL dead code in the source:
;;; imm5 is never read after the copy (the tail-called method clobbers
;;; volatiles), and BOTH other ports drop it (arm-def.lisp:527-557,
;;; x86-def.lisp:458ff).  NB imm5/nargs are DISTINCT registers on both
;;; PPC (r8/r11) and Matt's map (x5/x6) — an earlier "aliases" claim
;;; here was wrong (record corrected in wave-8 lead-verify).
(defarm64lapfunction %apply-lexpr-tail-wise ((method arg_y) (args arg_z))
  (ref-global imm0 ret1valaddr)         ; ppc:1201
  (ldr nargs (:@ args (:$ 0)))          ; ppc:1203 lexpr count (boxed)
  (mov nfn method)                      ; ppc:1207
  (ldur temp0 (:@ nfn (:$ arm64::misc-function-offset))) ; ppc:1208 (mtctr temp0)
  (cmp lr imm0)                         ; ppc:1200-1202 (mflr/cmpr cr2)
  (b.ne @no-mv-frame)                   ; ppc:1210 (if (:cr2 :eq) ...)
  (add sp sp (:$ 32))                   ; ppc:1211 discard MV frame
  @no-mv-frame
  (ldr lr (:@ sp (:$ 24)))              ; ppc:1212 savelr (mtlr ppc:1216)
  (ldr fn (:@ sp (:$ 16)))              ; ppc:1213 savefn
  (ldr imm0 (:@ sp (:$ 8)))             ; ppc:1214 savevsp
  (sub vsp imm0 nargs)                  ; ppc:1215 (boxed nargs IS byte count)
  (add sp sp (:$ 32))                   ; ppc:1217 (la sp lisp-frame.size sp)
  (cbz nargs @jump)                     ; ppc:1205/1218 (cr0; beqctr)
  (vpop arg_z)                          ; ppc:1219
  (cmp nargs (:$ (ash 2 arm64::fixnumshift))) ; ppc:1206 (cr1 re-issued)
  (b.lo @jump)                          ; ppc:1220 (bltctr cr1)
  (vpop arg_y)                          ; ppc:1221 (ldr post-index: flag-safe)
  (b.eq @jump)                          ; ppc:1222 (beqctr cr1)
  (vpop arg_x)                          ; ppc:1223
  @jump
  (br temp0))                           ; ppc:1224 (bctr) — W4-D15


;;; ---------------------------------------------------------------------
;;; apply+ — promoted 16m41 (regression stage 11: SETF-APPLY.1-4).
;;;
;;; (apply+ f butlast last) = (apply f (append butlast (list last))).
;;; This is the ONLY thing (setf (apply #'aref ...) ...) expands into
;;; (lib/setf.lisp:505's `(define-setf-method apply ...)' leg), and it is a
;;; PER-ARCH LAP definition -- present for PPC/x86/ARM32 and, until now,
;;; absent on arm64, so all four SETF-APPLY tests reported
;;; UNDEFINED-FUNCTION-CALL.  Same class as 16m37's missing VALUES.
;;;
;;; LOGIC DONOR: ppc-def.lisp:1256-1278 (line-for-line below).
;;; SHAPE DONOR for the two places PPC cannot be followed: arm-def.lisp:591,
;;; the other port with no loc-pc register and no conditional-store-free
;;; branchless push.  Three corrections to the wave-4 draft, each verified
;;; against this lane rather than assumed:
;;;
;;;  1. `(:arglist ...)' pseudo-op, NOT PPC's `(defun (&lap ...))' + a
;;;     trailing `(lfun-bits #'apply+ ...)'.  arm64-lap.lisp:124 implements
;;;     :arglist as `(setq *arm64-lap-lfun-bits* (encode-lambda-list arg))',
;;;     which encodes 3-required-plus-rest exactly as PPC's explicit
;;;     `(logior $lfbits-rest-bit (dpb 3 $lfbits-numreq 0))'.  Setting both
;;;     would be redundant; ARM32 sets only the arglist.
;;;  2. The callee goes in temp0, as PPC does -- NOT in nfn, as ARM32 does.
;;;     OUR .SPfuncall dispatches on temp0 (spentry-D-call-builtins.s:146
;;;     `and imm0,temp0,#fulltagmask'), so ARM32's `(ldr nfn (:@ nfn
;;;     'funcall))' would hand the subprim a stale callee.
;;;  3. nfn (the constant-pool base for `'funcall') and temp0 (`last') must
;;;     survive .SPspreadargz.  VERIFIED by reading it
;;;     (spentry-D-call-builtins.s:631-660): it writes only imm0, imm1,
;;;     arg_x, arg_y, arg_z, nargs and vsp.  PPC relies on the same contract.
;;;
;;; lr across the subprim: build-lisp-frame, call, then pop ONLY fn/lr and
;;; discard the frame -- vsp must NOT be reloaded, because the spread pushed
;;; the spread args onto the vstack.  That is the vetted W4-D17 idiom already
;;; live in %apply-lexpr-with-method-context above.
;;; ARM64-DEVIATION: PPC's branchless `blt cr0 @nopush' is kept as a real
;;; branch (a64 has no conditional store; ARM32 uses `strhs' instead).
(defarm64lapfunction apply+ ()
  (:arglist (function arg1 arg2 &rest other-args))
  (check-nargs 3 nil)                   ; ppc:1258
  (vpush arg_x)                         ; ppc:1259
  (mov temp0 arg_z)                     ; ppc:1260 last
  (mov arg_z arg_y)                     ; ppc:1261 butlast
  (sub nargs nargs (:$ (ash 2 arm64::fixnumshift))) ; ppc:1262 (subi nargs '2)
  (build-lisp-frame)                    ; ppc:1263 (mflr loc-pc) — W4-D17
  (call-subprim .SPspreadargz)          ; ppc:1264 (bla)
  (ldp fn lr (:@ sp (:$ 16)))           ; ppc:1266 (mtlr loc-pc); flag-safe
  (add sp sp (:$ 32))                   ;   discard frame; vsp NOT reloaded
  (cmp nargs (:$ (ash 3 arm64::fixnumshift))) ; ppc:1265 (cmpri cr0 nargs '3)
  (add nargs nargs (:$ (ash 1 arm64::fixnumshift))) ; ppc:1267 count for last
  (b.lt @nopush)                        ; ppc:1268 (blt cr0 @nopush)
  (vpush arg_x)                         ; ppc:1269
  @nopush
  (mov arg_x arg_y)                     ; ppc:1271
  (mov arg_y arg_z)                     ; ppc:1272
  (mov arg_z temp0)                     ; ppc:1273
  (ldur temp0 (:@ nfn (:$ 'funcall)))   ; ppc:1274 constant-pool ref (DECIDE-14)
  ;; jump-subprim, NOT call-subprim: PPC's `ba' is a tail BRANCH.  call-subprim
  ;; emits `bl', so .SPfuncall returned into the end of this code vector and
  ;; execution fell through into the NEXT object's `udf #0' sentinel --
  ;; "Unhandled exception 4 at 0x300000004ac4, insn 0x00000000".  This is the
  ;; SAME correction arm64-misc.lisp:457-460 records for the VALUES draft in
  ;; 16m37; the drafts systematically spell tail subprim jumps as calls.
  (jump-subprim .SPfuncall))           ; ppc:1275 (ba .SPfuncall) — TAIL
