;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;; arm64-clos.lisp — Wave-8 DRAFT port of vendor/ccl/level-0/PPC/ppc-clos.lisp
;;; PPC64 LINE-PORT (source: vendor/ccl/level-0/PPC/ppc-clos.lisp)
;;; Target: Matt Emerson upstream arm64 (low-tag) design, pin d71a5ad.
;;; Per-line citations: "; ppc:NNN" = line NNN of vendor/ccl/level-0/PPC/ppc-clos.lisp.
;;; Cross-ref (same low-tag family): vendor/ccl/level-0/X86/x86-clos.lisp ("; x86:NNN").
;;;
;;; CLOS slot indices (slot-id.index, gf.dispatch-table, gf.dcode, ...) are
;;; TARGET-INDEPENDENT def-accessors enums from vendor/ccl/library/lispequ.lisp
;;; (slot-id: :1208-1212 → slot-id.index = 2; generic-function: :1186-1194 →
;;; gf.dispatch-table = 3, gf.dcode = 4).  Used by NAME, as on PPC.
;;;
;;; FUNCTION OBJECTS (W8-D80, updated by the fulltag-function removal,
;;; patch 0055): generic functions and dcode functions are ordinary
;;; miscobjs.  Slot k of a function is at
;;;   (+ (* k arm64::node-size) arm64::misc-function-offset)     [-4 + 8k]
;;; (misc-function-offset = misc-data-offset now, so these are just the
;;; usual misc slot loads) and the final dispatch is ldur codevector
;;; @ -4 + br.  slot-id objects are istructs — plain svref applies.
;;;
;;; ONE-NZCV: PPC's cmplr/cmpri pairs are serialized — unsigned bounds
;;; compare (cmplr → b.hs) issued adjacent to its branch; the map-entry
;;; zero test (cmpri/cmpwi 0 → cbz) uses no flags at all.
;;;
;;; STATUS: DRAFT — not assembled; ledger in wave8-array-clos-report.md.

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "ARM64-LAPMACROS"))

;;; It's easier to keep this in LAP; we want to play around with its
;;; constants.  ; ppc:19-20

;;; =====================================================================
;;; %small-map-slot-id-lookup — ppc:25
;;; =====================================================================
;;; This just maps a SLOT-ID to a SLOT-DEFINITION or NIL.
;;; The map is a vector of (UNSIGNED-BYTE 8); this should
;;; be used when there are less than 255 slots in the class.  ; ppc:22-24
;;; 'map / 'table are function-constant-pool refs (DECIDE-14).
(defarm64lapfunction %small-map-slot-id-lookup ((slot-id arg_z))
  (ldur temp1 (:@ nfn (:$ 'map)))             ; ppc:26 — DECIDE-14
  (svref arg_x slot-id.index slot-id)   ; ppc:27 boxed index (istruct: plain svref)
  (getvheader imm0 temp1)               ; ppc:28
  (header-length imm3 imm0)             ; ppc:29 boxed map length
  (ldur temp0 (:@ nfn (:$ 'table)))           ; ppc:30 — DECIDE-14
  (lsr imm0 arg_x (:$ arm64::word-shift)) ; ppc:32 srri — boxed → raw byte index (u8 map)
  (sub imm0 imm0 (:$ (- arm64::misc-data-offset))) ; ppc:33
  (mov imm1 (:$ arm64::misc-data-offset)) ; ppc:34 out-of-bounds default: table[0]
  (cmp arg_x imm3)                      ; ppc:31 cmplr — UNSIGNED, issued adjacent to branch
  (b.hs @have-scaled-table-index)       ; ppc:35 bge on cmplr → b.hs
  (ldrb (:w imm1) (:@ temp1 imm0))      ; ppc:36 lbzx — map entry
  (lsl imm1 imm1 (:$ arm64::word-shift)) ; ppc:37
  (sub imm1 imm1 (:$ (- arm64::misc-data-offset))) ; ppc:38
  @have-scaled-table-index              ; ppc:39
  (ldr arg_z (:@ temp0 imm1))           ; ppc:40 ldrx (regoff)
  (ret))                                ; ppc:41

;;; =====================================================================
;;; %large-map-slot-id-lookup — ppc:44 [ppc64-sections: ppc64 arm ported;
;;;   the #+pp32-target (sic) arm ignored]
;;; =====================================================================
;;; The same idea, only the map is a vector of (UNSIGNED-BYTE 32).  ; ppc:43
(defarm64lapfunction %large-map-slot-id-lookup ((slot-id arg_z))
  (ldur temp1 (:@ nfn (:$ 'map)))             ; ppc:45 — DECIDE-14
  (svref arg_x slot-id.index slot-id)   ; ppc:46
  (getvheader imm0 temp1)               ; ppc:47
  (header-length imm3 imm0)             ; ppc:48
  (ldur temp0 (:@ nfn (:$ 'table)))           ; ppc:49 — DECIDE-14
  ;; W8-D81 UPSTREAM BUG: ppc:53 reads (srdi imm0 imm0 1), but imm0 holds
  ;; the map HEADER there; the boxed index is in arg_x (compare the
  ;; parallel %large-slot-id-value, ppc:105 (srdi imm0 arg_x 1)).  Ported
  ;; with the intended source register.  Queue for the next Matt mail.
  (lsr imm0 arg_x (:$ 1))               ; ppc:53 boxed → index*4 (u32 map)
  (sub imm0 imm0 (:$ (- arm64::misc-data-offset))) ; ppc:54
  (mov imm1 (:$ arm64::misc-data-offset)) ; ppc:58
  (cmp arg_x imm3)                      ; ppc:50 cmplr — adjacent to branch
  (b.hs @have-scaled-table-index)       ; ppc:59
  (ldr (:w imm1) (:@ temp1 imm0))       ; ppc:60 lwzx — map entry
  (lsl imm1 imm1 (:$ arm64::word-shift)) ; ppc:61
  (sub imm1 imm1 (:$ (- arm64::misc-data-offset))) ; ppc:62
  @have-scaled-table-index              ; ppc:63
  (ldr arg_z (:@ temp0 imm1))           ; ppc:64
  (ret))                                ; ppc:65

;;; =====================================================================
;;; %small-slot-id-value — ppc:67
;;; =====================================================================
(defarm64lapfunction %small-slot-id-value ((instance arg_y) (slot-id arg_z))
  (ldur temp1 (:@ nfn (:$ 'map)))             ; ppc:68 — DECIDE-14
  (svref arg_x slot-id.index slot-id)   ; ppc:69
  (getvheader imm0 temp1)               ; ppc:70
  (ldur temp0 (:@ nfn (:$ 'table)))           ; ppc:71 — DECIDE-14
  (header-length imm3 imm0)             ; ppc:72
  (lsr imm0 arg_x (:$ arm64::word-shift)) ; ppc:74
  (sub imm0 imm0 (:$ (- arm64::misc-data-offset))) ; ppc:75
  (cmp arg_x imm3)                      ; ppc:73 cmplr — adjacent to branch
  (b.hs @missing)                       ; ppc:76
  (ldrb (:w imm1) (:@ temp1 imm0))      ; ppc:77 lbzx
  (cbz imm1 @missing)                   ; ppc:78/81 (cmpri 0 / beq → cbz, before scaling)
  (lsl imm1 imm1 (:$ arm64::word-shift)) ; ppc:79
  (sub imm1 imm1 (:$ (- arm64::misc-data-offset))) ; ppc:80
  (ldr arg_z (:@ temp0 imm1))           ; ppc:82 slot-definition
  (ldur arg_x (:@ nfn (:$ 'class)))           ; ppc:83 — DECIDE-14
  (ldur nfn (:@ nfn (:$ '%maybe-std-slot-value))) ; ppc:84 — DECIDE-14 (last nfn use first)
  (ldur temp0 (:@ nfn (:$ arm64::misc-function-offset))) ; ppc:85 codevector — W8-D80
  (set-nargs 3)                         ; ppc:86
  (br temp0)                            ; ppc:87-88 mtctr/bctr
  @missing                              ; ppc:89 (%slot-id-ref-missing instance id)
  (ldur nfn (:@ nfn (:$ '%slot-id-ref-missing))) ; ppc:90 — DECIDE-14
  (set-nargs 2)                         ; ppc:91
  (ldur temp0 (:@ nfn (:$ arm64::misc-function-offset))) ; ppc:92 — W8-D80
  (br temp0))                           ; ppc:93-94

;;; =====================================================================
;;; %large-slot-id-value — ppc:96 [mixed-arch-body: ppc64 arm ported]
;;; =====================================================================
(defarm64lapfunction %large-slot-id-value ((instance arg_y) (slot-id arg_z))
  (ldur temp1 (:@ nfn (:$ 'map)))             ; ppc:97 — DECIDE-14
  (svref arg_x slot-id.index slot-id)   ; ppc:98
  (getvheader imm0 temp1)               ; ppc:99
  (ldur temp0 (:@ nfn (:$ 'table)))           ; ppc:100 — DECIDE-14
  (header-length imm3 imm0)             ; ppc:101
  (lsr imm0 arg_x (:$ 1))               ; ppc:104-105 #+ppc64 arm: boxed → index*4
  (sub imm0 imm0 (:$ (- arm64::misc-data-offset))) ; ppc:106
  (cmp arg_x imm3)                      ; ppc:102 cmplr — adjacent to branch
  (b.hs @missing)                       ; ppc:110
  (ldr (:w imm1) (:@ temp1 imm0))       ; ppc:111 lwzx
  (cbz imm1 @missing)                   ; ppc:112/115 (cmpri 0 / beq → cbz)
  (lsl imm1 imm1 (:$ arm64::word-shift)) ; ppc:113
  (sub imm1 imm1 (:$ (- arm64::misc-data-offset))) ; ppc:114
  @have-scaled-table-index              ; ppc:116 (vestigial label in PPC too — kept)
  (ldur arg_x (:@ nfn (:$ 'class)))           ; ppc:117 — DECIDE-14
  (ldur nfn (:@ nfn (:$ '%maybe-std-slot-value-using-class))) ; ppc:118 — DECIDE-14
  (ldr arg_z (:@ temp0 imm1))           ; ppc:119 slot-definition
  (ldur temp0 (:@ nfn (:$ arm64::misc-function-offset))) ; ppc:120 — W8-D80
  (set-nargs 3)                         ; ppc:121
  (br temp0)                            ; ppc:122-123
  @missing                              ; ppc:124 (%slot-id-ref-missing instance id)
  (ldur nfn (:@ nfn (:$ '%slot-id-ref-missing))) ; ppc:125 — DECIDE-14
  (set-nargs 2)                         ; ppc:126
  (ldur temp0 (:@ nfn (:$ arm64::misc-function-offset))) ; ppc:127 — W8-D80
  (br temp0))                           ; ppc:128-129

;;; =====================================================================
;;; %small-set-slot-id-value — ppc:131
;;; =====================================================================
;;; header-length lands in imm5, as on PPC.  (imm5=x5 and nargs=x6 are
;;; DISTINCT on Matt's map — corrected record; even under aliasing this
;;; would be safe: last read precedes both set-nargs forms, and entry
;;; nargs is dead — no check-nargs in the PPC original.)
(defarm64lapfunction %small-set-slot-id-value ((instance arg_x)
                                               (slot-id arg_y)
                                               (new-value arg_z))
  (ldur temp1 (:@ nfn (:$ 'map)))             ; ppc:134 — DECIDE-14
  (svref imm3 slot-id.index slot-id)    ; ppc:135 boxed index
  (getvheader imm0 temp1)               ; ppc:136
  (ldur temp0 (:@ nfn (:$ 'table)))           ; ppc:137 — DECIDE-14
  (header-length imm5 imm0)             ; ppc:138 (imm5 scratch — distinct from nargs, see above)
  (lsr imm0 imm3 (:$ arm64::word-shift)) ; ppc:140
  (sub imm0 imm0 (:$ (- arm64::misc-data-offset))) ; ppc:141
  (cmp imm3 imm5)                       ; ppc:139 cmplr — adjacent to branch
  (b.hs @missing)                       ; ppc:142
  (ldrb (:w imm1) (:@ temp1 imm0))      ; ppc:143 lbzx
  (cbz imm1 @missing)                   ; ppc:144/147 (cmpwi 0 / beq → cbz)
  (lsl imm1 imm1 (:$ arm64::word-shift)) ; ppc:145
  (sub imm1 imm1 (:$ (- arm64::misc-data-offset))) ; ppc:146
  @have-scaled-table-index              ; ppc:148
  (ldur temp1 (:@ nfn (:$ 'class)))           ; ppc:149 — DECIDE-14
  (ldr arg_y (:@ temp0 imm1))           ; ppc:150 slot-definition (overwrites slot-id)
  (ldur nfn (:@ nfn (:$ '%maybe-std-setf-slot-value-using-class))) ; ppc:151 — DECIDE-14
  (set-nargs 4)                         ; ppc:152
  (ldur temp0 (:@ nfn (:$ arm64::misc-function-offset))) ; ppc:153 — W8-D80
  (vpush temp1)                         ; ppc:154 class = first (vstack) arg
  (br temp0)                            ; ppc:155-156
  @missing                              ; ppc:157 (%slot-id-set-missing instance id new-value)
  (ldur nfn (:@ nfn (:$ '%slot-id-set-missing))) ; ppc:158 — DECIDE-14
  (set-nargs 3)                         ; ppc:159
  (ldur temp0 (:@ nfn (:$ arm64::misc-function-offset))) ; ppc:160 — W8-D80
  (br temp0))                           ; ppc:161-162

;;; =====================================================================
;;; %large-set-slot-id-value — ppc:164 [ppc64-sections: ppc64 arm ported]
;;; =====================================================================
(defarm64lapfunction %large-set-slot-id-value ((instance arg_x)
                                               (slot-id arg_y)
                                               (new-value arg_z))
  (ldur temp1 (:@ nfn (:$ 'map)))             ; ppc:167 — DECIDE-14
  (svref imm3 slot-id.index slot-id)    ; ppc:168
  (getvheader imm0 temp1)               ; ppc:169
  (ldur temp0 (:@ nfn (:$ 'table)))           ; ppc:170 — DECIDE-14
  (header-length imm5 imm0)             ; ppc:171 (imm5 scratch, as in the small twin)
  (cmp imm3 imm5)                       ; ppc:172 cmplr — BEFORE the index is halved (as PPC)
  (lsr imm3 imm3 (:$ 1))                ; ppc:173 #+ppc64 arm: boxed → index*4 (flag-safe)
  (sub imm0 imm3 (:$ (- arm64::misc-data-offset))) ; ppc:174 (flag-safe)
  (b.hs @missing)                       ; ppc:175
  (ldr (:w imm1) (:@ temp1 imm0))       ; ppc:176 lwzx
  (cbz imm1 @missing)                   ; ppc:177/180 (cmpwi 0 / beq → cbz)
  (lsl imm1 imm1 (:$ arm64::word-shift)) ; ppc:178
  (sub imm1 imm1 (:$ (- arm64::misc-data-offset))) ; ppc:179
  @have-scaled-table-index              ; ppc:181
  (ldur temp1 (:@ nfn (:$ 'class)))           ; ppc:182 — DECIDE-14
  (ldr arg_y (:@ temp0 imm1))           ; ppc:183 slot-definition
  (ldur nfn (:@ nfn (:$ '%maybe-std-setf-slot-value-using-class))) ; ppc:184 — DECIDE-14
  (set-nargs 4)                         ; ppc:185
  ;; ppc:186 is (svref temp0 0 nfn) — same load as the small twin's
  ;; (ldr temp0 misc-data-offset nfn); nfn is FUNCTION-tagged → W8-D80.
  (ldur temp0 (:@ nfn (:$ arm64::misc-function-offset))) ; ppc:186 — W8-D80
  (vpush temp1)                         ; ppc:187
  (br temp0)                            ; ppc:188-189
  @missing                              ; ppc:190 (%slot-id-set-missing instance id new-value)
  ;; ppc:191 loads '%slot-id-ref-missing (with set-nargs 3) although the
  ;; comment and the small twin (ppc:158) say %slot-id-set-missing.
  ;; x86-64 does the SAME (x86:151-153: set-missing comment,
  ;; ref-missing load) — a cross-arch upstream quirk, ported FAITHFULLY.
  ;; W8-D82 ledger note.
  (ldur nfn (:@ nfn (:$ '%slot-id-ref-missing))) ; ppc:191 — DECIDE-14
  (set-nargs 3)                         ; ppc:192
  (ldur temp0 (:@ nfn (:$ arm64::misc-function-offset))) ; ppc:193 — W8-D80
  (br temp0))                           ; ppc:194-195

;;; =====================================================================
;;; *gf-proto* — ppc:197-218 (#-dont-use-lexprs branch; W8-D83 closed
;;; 16m10, demanded by l1-dcode load: register-dcode-proto references)
;;; =====================================================================
;;; The gag lexpr prototype CLOS instantiates for real gfs.  PPC keeps
;;; the caller's return pc in loc_pc across .SPlexpr-entry (mflr/mtlr);
;;; Matt's map has no loc_pc, so the LEXPR-RA channel is temp4 (spentry-E
;;; lexpr_entry contract): prologue passes caller's return pc in temp4,
;;; the subprim hands back the continuation the body must return through
;;; (ret1val_addr on the mv path, lexpr_return1v on the 1v path) in
;;; temp4, restored into lr before the tail transfer.  call-subprim
;;; clobbers only imm1+lr, so temp4 survives into the kernel.  nfn (the
;;; gf) is untouched by lexpr_entry; gf.dispatch-table = 3, gf.dcode = 4
;;; (lispequ.lisp:1186-1194), both svrefs re-bias per W8-D80.
(defparameter *gf-proto*
  (nfunction
   gag
   (lambda (&lap &lexpr args)
     (arm64-lap-function
      gag
      ()
      (mov temp4 lr)                        ; ppc:205 (mflr loc-pc) — LEXPR-RA
      (vpush-argregs)                       ; ppc:206
      (vpush nargs)                         ; ppc:207
      (add imm0 vsp nargs)                  ; ppc:208
      (add imm0 imm0 (:$ arm64::node-size)) ; ppc:209 caller's vsp
      (call-subprim .SPlexpr-entry)         ; ppc:210 (bla) — DECIDE-10
      (mov lr temp4)                        ; ppc:211 (mtlr loc-pc) — lexpr cleanup continuation
      (mov arg_z vsp)                       ; ppc:212 lexpr
      (ldur arg_y (:@ nfn (:$ (+ (* gf.dispatch-table arm64::node-size)
                                 arm64::misc-function-offset)))) ; ppc:213 dispatch table
      (set-nargs 2)                         ; ppc:214
      (ldur nfn (:@ nfn (:$ (+ (* gf.dcode arm64::node-size)
                               arm64::misc-function-offset))))   ; ppc:215 dcode function
      (ldur temp0 (:@ nfn (:$ arm64::misc-function-offset)))     ; ppc:216 codevector — W8-D80
      (br temp0)))))                        ; ppc:217-218 — DECIDE-15

;;; =====================================================================
;;; funcallable-trampoline — ppc:271
;;; =====================================================================
;;; nfn is the funcallable instance = a FUNCTION-tagged gvector: both
;;; svrefs re-bias to misc-function-offset (W8-D80).
;;; gf.dcode = 4 (lispequ.lisp:1186-1194).
(defarm64lapfunction funcallable-trampoline ()
  (ldur nfn (:@ nfn (:$ (+ (* gf.dcode arm64::node-size)
                           arm64::misc-function-offset)))) ; ppc:272 (svref gf.dcode)
  (ldur temp0 (:@ nfn (:$ arm64::misc-function-offset)))   ; ppc:273 (svref 0 = codevector)
  (br temp0))                           ; ppc:274-275 mtctr/bctr — DECIDE-15

;;; =====================================================================
;;; unset-fin-trampoline — ppc:278
;;; =====================================================================
;;; This can't reference any of the function's constants.  ; ppc:277
;;; (Satisfied: only subprims, immediates and rnil below.)
;;; PPC saves lr in loc-pc across the first subprim call and only builds
;;; a frame later via .SPsavecontextvsp.  No loc-pc register here: build
;;; the marker frame at ENTRY (savevsp is identical — .SPheap-rest-arg's
;;; push is popped again before PPC's frame is built) and let the frame
;;; carry lr across both subprim calls (W4-D17 idiom).
(defarm64lapfunction unset-fin-trampoline ()
  (build-lisp-frame)                    ; ppc:279 (mflr loc-pc) + ppc:282 (.SPsavecontextvsp)
  (call-subprim .SPheap-rest-arg)       ; ppc:280 cons up &rest arg, vpush it — DECIDE-10
  (vpop arg_z)                          ; ppc:281 whoops, didn't really want to
  (mov arg_x (:$ (ash $xnofinfunction arm64::fixnumshift))) ; ppc:283 li '#.$XNOFINFUNCTION
  (mov arg_y nfn)                       ; ppc:284
  (set-nargs 3)                         ; ppc:285
  (call-subprim .SPksignalerr)          ; ppc:286 — DECIDE-10
  (mov arg_z rnil)                      ; ppc:287
  (restore-lisp-frame)                  ; ppc:288 (ba .SPpopj) → inline restore+ret (W5 idiom)
  (ret))

;;; =====================================================================
;;; gag-one-arg — ppc:291
;;; =====================================================================
;;; is a winner - saves ~15%  ; ppc:290
;;; nfn = the gf (FUNCTION-tagged): gf.dispatch-table = 3, gf.dcode = 4
;;; (lispequ.lisp:1186-1194); both svrefs re-bias per W8-D80.
(defarm64lapfunction gag-one-arg ((arg arg_z))
  (check-nargs 1)                       ; ppc:292
  (ldur arg_y (:@ nfn (:$ (+ (* gf.dispatch-table arm64::node-size)
                             arm64::misc-function-offset)))) ; ppc:293 dispatch table first
  (set-nargs 2)                         ; ppc:294
  (ldur nfn (:@ nfn (:$ (+ (* gf.dcode arm64::node-size)
                           arm64::misc-function-offset))))   ; ppc:295 dcode function
  (ldur temp0 (:@ nfn (:$ arm64::misc-function-offset)))     ; ppc:296 codevector — W8-D80
  (br temp0))                           ; ppc:297-298 — DECIDE-15

;;; =====================================================================
;;; gag-two-arg — ppc:301
;;; =====================================================================
(defarm64lapfunction gag-two-arg ((arg0 arg_y) (arg1 arg_z))
  (check-nargs 2)                       ; ppc:302
  (ldur arg_x (:@ nfn (:$ (+ (* gf.dispatch-table arm64::node-size)
                             arm64::misc-function-offset)))) ; ppc:303 dispatch table first
  (set-nargs 3)                         ; ppc:304
  (ldur nfn (:@ nfn (:$ (+ (* gf.dcode arm64::node-size)
                           arm64::misc-function-offset))))   ; ppc:305 dcode function
  (ldur temp0 (:@ nfn (:$ arm64::misc-function-offset)))     ; ppc:306 codevector — W8-D80
  (br temp0))                           ; ppc:307-308 — DECIDE-15

;;; =====================================================================
;;; *cm-proto* — ppc:310-330 (W8-D83 closed 16m10)
;;; =====================================================================
;;; Combined-method prototype: same lexpr shape as *gf-proto* (LEXPR-RA
;;; temp4 channel, see the *gf-proto* header comment).  Slots differ:
;;; combined-method.thing = 1, combined-method.dcode = 2
;;; (lispequ.lisp:1178-1184); both svrefs re-bias per W8-D80.
(defparameter *cm-proto*
  (nfunction
   gag
   (lambda (&lap &lexpr args)
     (arm64-lap-function
      gag
      ()
      (mov temp4 lr)                        ; ppc:317 (mflr loc-pc) — LEXPR-RA
      (vpush-argregs)                       ; ppc:318
      (vpush nargs)                         ; ppc:319
      (add imm0 vsp nargs)                  ; ppc:320
      (add imm0 imm0 (:$ arm64::node-size)) ; ppc:321 caller's vsp
      (call-subprim .SPlexpr-entry)         ; ppc:322 (bla) — DECIDE-10
      (mov lr temp4)                        ; ppc:323 (mtlr loc-pc) — lexpr cleanup continuation
      (mov arg_z vsp)                       ; ppc:324 lexpr
      (ldur arg_y (:@ nfn (:$ (+ (* combined-method.thing arm64::node-size)
                                 arm64::misc-function-offset)))) ; ppc:325 thing
      (set-nargs 2)                         ; ppc:326
      (ldur nfn (:@ nfn (:$ (+ (* combined-method.dcode arm64::node-size)
                               arm64::misc-function-offset))))   ; ppc:327 dcode function
      (ldur temp0 (:@ nfn (:$ arm64::misc-function-offset)))     ; ppc:328 codevector — W8-D80
      (br temp0)))))                        ; ppc:329-330 — DECIDE-15
