;;;; -*- Mode: Lisp; Package: CCL -*-
;;;;
;;;; arm64-symbol.lisp — ARM64 LAP function drafts for symbol functions
;;;;
;;;; Ported line-by-line from vendor/ccl/level-0/PPC/ppc-symbol.lisp (PPC64 arms).
;;;; Register map: Matt Emerson's upstream arm64 (arm64-asm.lisp).
;;;; Tags: LOW tags, fixnumshift=3, misc-data-offset=4, misc-header-offset=-4.
;;;;
;;;; STATUS: LEAD-VERIFIED 2026-07-08 (line-by-line vs PPC64; assemble-gate clean
;;;; except DECIDE-blocked sites — see drafts/wave1-verify-report.md)

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "ARM64-ARCH")
  (require "ARM64-LAPMACROS"))

;;; =====================================================================
;;; %function — from vendor/ccl/level-0/PPC/ppc-symbol.lisp:49 (ppc64 arm)
;;; =====================================================================
;;;
;;; Assumes macros & special-operators have something that's not FUNCTIONP
;;; in their function-cells.
;;; PPC64 version: type-check sym as symbol, load fcell, check if function.
;;;
;;; Key layout (arm64-arch.lisp):
;;;   subtag-symbol = 22, symbol.fcell = 17, subtag-function = 150
;;;   error-udf (arch::error-udf) — undefined function error

;;; 16m5v REWRITE to the x8664 donor (x86-symbol.lisp:29) — Matt's tag
;;; model: a symbol POINTER has fulltag-symbol(7); extract-typecode only
;;; yields subtag-symbol(22) for the MISC-tagged symvector alias, so the
;;; old trap-unless-typecode= brk'd on every real symbol (observed live:
;;; typecode=7 vs 22 at the #xf0ff brk).  The fcell functionp check is
;;; the PPC typecode test again since the fulltag-function removal
;;; (patch 0055): a function is misc + subtag-function.  NIL maps to
;;; NILSYM, the real NRS symbol at rnil+t-offset+symbol.size (= rnil+92;
;;; VERIFIED live: pname @0x13067 = "NIL").
(defarm64lapfunction %function ((sym arg_z))
  (check-nargs 1)
  (let ((symaddr temp0))
    (add symaddr rnil (:$ (+ arm64::t-offset arm64::symbol.size))) ; x86:32 nilsym
    (cmp sym rnil)                                   ; x86:33
    (csel symaddr sym symaddr (:? ne))               ; x86:34 (cmovneq)
    (trap-unless-fulltag= symaddr arm64::fulltag-symbol) ; x86:35
    (mov arg_y sym)                                  ; x86:36
    (ldur arg_z (:@ symaddr (:$ arm64::symbol.fcell))) ; x86:37
    (extract-typecode imm0 arg_z)                    ; x86:38
    (cmp imm0 (:$ arm64::subtag-function))           ; x86:39
    (b.eq @ok)                                       ; x86:40
    (uuo-error-udf arg_y)                            ; x86:41
    @ok
    (ret)))

;;; =====================================================================
;;; %symbol->symptr — from vendor/ccl/level-0/PPC/ppc-symbol.lisp:67
;;; =====================================================================
;;;
;;; ⚠ The NILSYM mapping IS REQUIRED on arm64 (16m14; an earlier draft of
;;; this header claimed otherwise and that claim leaked into the w9
;;; %symbol->symptr VINSN, which shredded T's symbol struct whenever lisp
;;; wrote a symbol slot of NIL).  NIL has fulltag-nil(0xb): the blind
;;; symptr→symvector retag (+5) is only valid for fulltag-symbol(7)
;;; pointers, so NIL must first map to NILSYM — the real NRS symbol at
;;; rnil+t-offset+symbol.size (=rnil+92=0x13067; pname "NIL", verified
;;; live in w4 and again in 16m14).

;;; 16m5v REWRITE to the x8664 donor (x86-symbol.lisp:47) — see %function
;;; above for the tag-model rationale.  NIL → NILSYM (a real symbol), so
;;; downstream symvector ops (SYMBOL-NAME et al.) work on the result;
;;; the old version passed NIL through and typecode-trapped real symbols.
(defarm64lapfunction %symbol->symptr ((sym arg_z))
  (let ((tag imm0))
    (add tag rnil (:$ (+ arm64::t-offset arm64::symbol.size))) ; x86:49 nilsym
    (cmp sym rnil)                                   ; x86:50
    (csel sym tag sym (:? eq))                       ; x86:51 (cmoveq)
    (b.eq @done)                                     ; x86:52
    (trap-unless-fulltag= sym arm64::fulltag-symbol) ; x86:53
    @done
    (ret)))                                          ; x86:55

;;; =====================================================================
;;; %symptr->symbol — from vendor/ccl/level-0/PPC/ppc-symbol.lisp:80
;;; =====================================================================
;;;
;;; Traps unless symptr is a symbol.  On arm64, also need to handle the
;;; NIL case (return rnil if symptr is NIL).

;;; 16m5v REWRITE to the x8664 donor (x86-symbol.lisp:58) — typecheck the
;;; symptr (fulltag-symbol), then NILSYM → NIL (inverse of %symbol->symptr).
(defarm64lapfunction %symptr->symbol ((symptr arg_z))
  (trap-unless-fulltag= symptr arm64::fulltag-symbol) ; x86:59-62
  (let ((nilsym imm0))
    (add nilsym rnil (:$ (+ arm64::t-offset arm64::symbol.size)))
    (cmp symptr nilsym)                              ; x86:64
    (csel symptr rnil symptr (:? eq))                ; x86:65-68
    (ret)))                                          ; x86:69

;;; =====================================================================
;;; %symptr->symvector / %symvector->symptr — x8664 donors
;;; (x86-symbol.lisp:75/:79); pure retags between the fulltag-symbol
;;; pointer and its fulltag-misc uvector alias (same object).  PPC has
;;; no analog (16m5v family seed; SYMBOL-NAME et al. call these).
;;; =====================================================================
(defarm64lapfunction %symptr->symvector ((symptr arg_z))
  ;; x86:76 is (subb (- fulltag-symbol fulltag-misc)) = subtract −5;
  ;; arm64 immediates are unsigned — flip to add (misc−symbol = +5).
  (add arg_z symptr (:$ (- arm64::fulltag-misc arm64::fulltag-symbol)))
  (ret))                                             ; x86:77

(defarm64lapfunction %symvector->symptr ((symbol-vector arg_z))
  (sub arg_z symbol-vector (:$ (- arm64::fulltag-misc arm64::fulltag-symbol))) ; x86:80 (addb −5 → sub +5)
  (ret))                                             ; x86:81

;;; =====================================================================
;;; %symptr-value — from vendor/ccl/level-0/PPC/ppc-symbol.lisp:92
;;; =====================================================================
;;;
;;; Tail-call to .SPspecref subprim.

(defarm64lapfunction %symptr-value ((symptr arg_z))
  (jump-subprim .SPspecref))

;;; =====================================================================
;;; %set-symptr-value — from vendor/ccl/level-0/PPC/ppc-symbol.lisp:95
;;; =====================================================================
;;;
;;; Tail-call to .SPspecset subprim.

(defarm64lapfunction %set-symptr-value ((symptr arg_y) (val arg_z))
  (jump-subprim .SPspecset))

;;; =====================================================================
;;; %symptr-binding-address — from vendor/ccl/level-0/PPC/ppc-symbol.lisp:98
;;; =====================================================================
;;;
;;; Returns the binding address: if the symbol has a thread-local binding,
;;; return (values tlb-pointer binding-index); otherwise return
;;; (values symptr symbol.vcell-offset).
;;;
;;; Key offsets (arm64-arch.lisp):
;;;   symbol.binding-index = 49
;;;   tcr.tlb-limit = 296, tcr.tlb-pointer = 304
;;;   subtag-no-thread-local-binding = (logior fulltag-imm-1 (ash 4 4)) = 74
;;;   symbol.vcell = 9

(defarm64lapfunction %symptr-binding-address ((symptr arg_z))
  ;; (ldr imm3 target::symbol.binding-index symptr)
  (ldur imm3 (:@ symptr (:$ arm64::symbol.binding-index)))
  ;; (ldr imm2 target::tcr.tlb-limit target::rcontext)
  (ldr imm2 (:@ rcontext (:$ arm64::tcr.tlb-limit)))
  ;; (ldr imm4 target::tcr.tlb-pointer target::rcontext)
  (ldr imm4 (:@ rcontext (:$ arm64::tcr.tlb-pointer)))
  ;; (cmplr imm3 imm2) — unsigned compare: binding-index >= tlb-limit?
  (cmp imm3 imm2)
  (b.hs @sym)
  ;; (ldrx temp0 imm4 imm3) — load from tlb at index
  (ldr temp0 (:@ imm4 imm3))
  ;; (cmpdi temp0 target::subtag-no-thread-local-binding)
  (cmp temp0 (:$ arm64::subtag-no-thread-local-binding))
  ;; (slri imm3 imm3 target::fixnumshift) — slri = sldi = shift LEFT
  ;; (vendor/ccl/compiler/PPC/ppc-lapmacros.lisp:173): boxes the raw
  ;; binding-index as a fixnum before vpush.  Same shift on arm64
  ;; (fixnumshift=3 on both).
  (lsl imm3 imm3 (:$ arm64::fixnumshift))
  (b.eq @sym)
  ;; Thread-local binding exists — return (values tlb-pointer index)
  (vpush imm4)
  (vpush imm3)
  (set-nargs 2)
  ;; (la temp0 '2 vsp) — temp0 = vsp + 2*fixnumone = vsp + 16
  ;; This is the "values frame" base for .SPvalues
  (add temp0 vsp (:$ 16))
  (jump-subprim .SPvalues)
  @sym
  ;; No thread-local binding — return (values symptr symbol.vcell-offset)
  ;; PPC64: (li arg_y '#.target::symbol.vcell)
  ;; symbol.vcell = 9 on arm64; as a fixnum-tagged constant:
  ;; the offset itself is what gets pushed, not a fixnum-encoded version.
  ;; Actually looking at PPC64: (li arg_y '#.target::symbol.vcell) where
  ;; #. reads the constant at read-time.  On PPC64 symbol.vcell = 32 (a
  ;; fixnum since fixnumshift=3 and 32 is divisible by 8).  On arm64
  ;; symbol.vcell = 9 which is NOT a fixnum (not multiple of 8).
  ;; This is a layout mismatch — the field offset isn't fixnum-aligned.
  ;; We need to pass the fixnum-tagged cell index instead.
  ;; PPC64 symbol.vcell IS a fixnum (all field offsets are multiples of 8
  ;; on PPC64 because node-size=8 and bias doesn't break alignment).
  ;; On arm64, symbol.vcell = 9 (= -fulltag-symbol + 2*8 = -7 + 16).
  ;; Hmm, not a fixnum.  The caller uses this to index into the object.
  ;; DECIDE-BLOCKED: symbol.vcell offset not fixnum-aligned on arm64;
  ;; need to understand how callers use this return value (raw offset
  ;; vs fixnum-tagged cell index).  For now, pass the raw offset.
  (mov arg_y (:$ arm64::symbol.vcell))
  (vpush arg_z)
  (vpush arg_y)
  (set-nargs 2)
  (add temp0 vsp (:$ 16))
  (jump-subprim .SPvalues))

;;; =====================================================================
;;; %tcr-binding-location — from vendor/ccl/level-0/PPC/ppc-symbol.lisp:121
;;; =====================================================================
;;;
;;; Returns the address of a symbol's thread-local binding in tcr's TLB,
;;; or NIL if no thread-local binding.

(defarm64lapfunction %tcr-binding-location ((tcr arg_y) (sym arg_z))
  ;; (ldr imm3 target::symbol.binding-index sym)
  (ldur imm3 (:@ sym (:$ arm64::symbol.binding-index)))
  ;; (ldr imm2 target::tcr.tlb-limit tcr)
  (ldr imm2 (:@ tcr (:$ arm64::tcr.tlb-limit)))
  ;; (ldr imm4 target::tcr.tlb-pointer tcr)
  (ldr imm4 (:@ tcr (:$ arm64::tcr.tlb-pointer)))
  ;; (li arg_z nil)
  (mov arg_z rnil)
  ;; (cmplr imm3 imm2) — unsigned: if binding-index >= tlb-limit, return nil
  (cmp imm3 imm2)
  (b.hs @done)
  ;; (ldrx temp0 imm4 imm3) — load from TLB
  (ldr temp0 (:@ imm4 imm3))
  ;; (cmpri temp0 target::subtag-no-thread-local-binding)
  (cmp temp0 (:$ arm64::subtag-no-thread-local-binding))
  (b.eq @done)
  ;; (add arg_z imm4 imm3) — return address = tlb-pointer + index
  (add arg_z imm4 imm3)
  @done
  (ret))

;;; =====================================================================
;;; %pname-hash — from vendor/ccl/level-0/PPC/ppc-symbol.lisp:135
;;; =====================================================================
;;;
;;; Hash a pname string.  PPC64 uses rotlwi (32-bit rotate-left-immediate,
;;; even on PPC64) and XOR accumulation over 32-bit chunks, then
;;; (slri accum 5) + (srri arg_z accum (- 5 fixnumshift)) — net << 3 —
;;; to box the 32-bit hash as a fixnum.
;;;
;;; On arm64 there is no 32-bit rotate of an X register; a 64-bit
;;; (ror #59) is WRONG because bits 27-31 must wrap into bits 0-4 but
;;; ror64 moves them into bits 32-36, polluting later iterations.
;;; Emulate rotl32 exactly (upper 32 bits of accum are zero at loop
;;; entry): tmp = accum >> 27; accum = (accum << 5 | tmp) & #xffffffff.

(defarm64lapfunction %pname-hash ((str arg_y) (len arg_z))
  (let ((nextw imm1)
        (accum imm0)
        (offset imm2))
    ;; (cmpwi cr0 len 0)
    (cbz len @done)
    (mov offset (:$ arm64::misc-data-offset))
    (mov accum (:$ 0))
    @loop
    ;; PPC64: (cmpri cr1 len '1) + (bne cr1 @loop) at end = loop while len>1.
    ;; ARM64: decrement first, branch on nonzero result (equivalent).
    (sub len len (:$ arm64::fixnumone))
    ;; (lwzx nextw str offset) — 32-bit load; w1 = W alias of nextw/imm1/x1
    ;; (Matt's arm64-asm.lisp:144); avoids over-reading past the last char.
    (add imm3 str offset)
    (ldr w1 (:@ imm3 (:$ 0)))
    ;; (addi offset offset 4)
    (add offset offset (:$ 4))
    ;; (rotlwi accum accum 5) — 32-bit rotate left by 5, emulated
    (lsr imm4 accum (:$ 27))
    (lsl accum accum (:$ 5))
    (orr accum accum imm4)
    (and accum accum (:$ #xffffffff))
    ;; (xor accum accum nextw)
    (eor accum accum nextw)
    ;; loop while len > 0 (decremented above)
    (cbnz len @loop)
    ;; Produce fixnum result:
    ;; PPC64: (slri accum accum 5) then (srri arg_z accum (- 5 fixnumshift)).
    ;; Those are 64-bit shifts (sldi/srdi) of a 32-bit-clean value: net
    ;; effect is accum << fixnumshift with nothing dropped (35-bit result,
    ;; a positive fixnum).  accum is 32-bit clean here, so same two shifts.
    (lsl accum accum (:$ 5))
    (lsr arg_z accum (:$ (- 5 arm64::fixnumshift)))
    @done
    (ret)))

;;; =====================================================================
;;; %string-hash — from vendor/ccl/level-0/PPC/ppc-symbol.lisp:155
;;; =====================================================================
;;;
;;; Same as %pname-hash but with a start offset parameter.
;;; PPC64: (srwi offset start 1) then (la offset misc-data-offset offset)
;;; On 64-bit-target, start is a character index (fixnum); chars are 32-bit,
;;; so byte offset = start * 4 / 8 * 4 = start >> 1 (since start is fixnum,
;;; char-size = 4 bytes, fixnumshift = 3: byte-offset = (start >> 3) * 4
;;; = start >> 1... wait.  Let me re-derive.
;;; start is a fixnum character index.  To get byte offset into the string
;;; data:  byte_offset = (unbox start) * char-size = (start >> fixnumshift) * 4
;;; = (start >> 3) * 4 = start >> 1.  Then add misc-data-offset.
;;; PPC64: (srwi offset start 1) — this is the same as (lsr offset start 1).
;;; Then (la offset misc-data-offset offset) = offset += misc-data-offset.

(defarm64lapfunction %string-hash ((start arg_x) (str arg_y) (len arg_z))
  (let ((nextw imm1)
        (accum imm0)
        (offset imm2))
    (cbz len @done)
    ;; Compute starting byte offset: start >> 1 + misc-data-offset
    ;; (start is fixnum-tagged char index; chars are 32-bit = 4 bytes;
    ;; byte offset = (start / fixnumone) * 4 = start * 4 / 8 = start / 2)
    (lsr offset start (:$ 1))
    (sub offset offset (:$ (- arm64::misc-data-offset)))
    (mov accum (:$ 0))
    @loop
    (sub len len (:$ arm64::fixnumone))
    ;; (lwzx nextw str offset) — 32-bit load via w1 (see %pname-hash)
    (add imm3 str offset)
    (ldr w1 (:@ imm3 (:$ 0)))
    (add offset offset (:$ 4))
    ;; (rotlwi accum accum 5) — 32-bit rotate left by 5, emulated
    ;; (see %pname-hash: 64-bit ror would leak bits 27-31 into 32-36)
    (lsr imm4 accum (:$ 27))
    (lsl accum accum (:$ 5))
    (orr accum accum imm4)
    (and accum accum (:$ #xffffffff))
    (eor accum accum nextw)
    (cbnz len @loop)
    ;; Produce fixnum result (same as %pname-hash)
    (lsl accum accum (:$ 5))
    (lsr arg_z accum (:$ (- 5 arm64::fixnumshift)))
    @done
    (ret)))

;;; =====================================================================
;;; %ensure-tlb-index — from vendor/ccl/level-0/PPC/ppc-symbol.lisp:183
;;; =====================================================================
;;;
;;; Ensure the TLB has room for binding-index IDX.
;;; PPC64: load tlb-limit, trap if limit <= idx, return tlb-pointer.
;;; The trap (trlle) causes the kernel to extend the TLB.

(defarm64lapfunction %ensure-tlb-index ((idx arg_z))
  ;; (ldr arg_y target::tcr.tlb-limit target::rcontext)
  (ldr arg_y (:@ rcontext (:$ arm64::tcr.tlb-limit)))
  ;; (trlle arg_y idx) — trap if arg_y <= idx (unsigned)
  ;; ARM64: cmp arg_y, idx; if arg_y <= idx (i.e. idx >= arg_y), trap.
  ;; DECIDE-BLOCKED: trap convention for TLB extension (trlle equivalent).
  ;; Using cmp + b.hi skip + brk placeholder.
  (cmp arg_y idx)
  (b.hi @ok)
  (uuo-error-tlb-too-small idx)           ; TLB extension trap placeholder
  @ok
  ;; (ldr arg_z target::tcr.tlb-pointer target::rcontext)
  (ldr arg_z (:@ rcontext (:$ arm64::tcr.tlb-pointer)))
  (ret))
