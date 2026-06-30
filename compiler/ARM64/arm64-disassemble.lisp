;;;;-*- Mode: Lisp; Package: (ARM64 :use CL) -*-
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

(in-package "ARM64")

(defvar *disassemble-print-lisp-register-names* t)
(defvar *disassemble-print-hex-threshold* 100)
(defvar *disassemble-print-instruction-word* t)
(defvar *disassemble-show-preferred-aliases* t)

(defstruct (disassembled-instruction (:conc-name di-))
  (word 0 :type (unsigned-byte 32))
  label                        ;name of the label defined here, or nil
  mnemonic
  operands)

;; Some encodings should disassemble to preferred aliases.  The
;; rewrite function may alter the disassembled-instruction struct (by
;; modifying its name and operands) so that the preferred alias will be
;; printed.
(defparameter *alias-rewriters*
  '(("add" . rewrite-add)
    ("adds" . rewrite-adds)
    ("sub" . rewrite-sub)
    ("subs" . rewrite-subs)
    ("ands" . rewrite-ands)
    ("orr" . rewrite-orr)
    ("orn" . rewrite-orn)
    ("sbc" . rewrite-sbc)
    ("sbcs" . rewrite-sbcs)
    ("madd" . rewrite-madd)
    ("msub" . rewrite-msub)
    ("smaddl" . rewrite-smaddl)
    ("smsubl" . rewrite-smsubl)
    ("umaddl" . rewrite-umaddl)
    ("umsubl" . rewrite-umsubl)
    ("lslv" . rewrite-lslv)
    ("lsrv" . rewrite-lsrv)
    ("asrv" . rewrite-asrv)
    ("rorv" . rewrite-rorv)
    ("csinc" . rewrite-csinc)
    ("csinv" . rewrite-csinv)
    ("csneg" . rewrite-csneg)
    ("ubfm" . rewrite-ubfm)
    ("sbfm" . rewrite-sbfm)
    ("bfm" . rewrite-bfm)
    ("extr" . rewrite-extr)
    ("movz" . rewrite-movz)
    ("movn" . rewrite-movn)))

(defun find-rewriter (mnemonic)
  (cdr (assoc mnemonic *alias-rewriters* :test #'equalp)))

(defun find-template (insn-word)
  (dotimes (i (length *instruction-templates*))
    (let* ((template (svref *instruction-templates* i))
           (opcode (instruction-template-base-opcode template))
           (mask (instruction-template-mask template)))
      (when (= (logand insn-word mask) opcode)
        (return template)))))

(defun disassemble-instruction (insn-word)
  (let* ((di (make-disassembled-instruction :word insn-word))
         (template (find-template insn-word)))
    (if (null template)
      (setf (di-mnemonic di) "<unknown>")
      (let ((mnemonic (instruction-template-name template)))
        (setf (di-mnemonic di) mnemonic)
        (setf (di-operands di)
              (mapcar #'(lambda (spec)
                          (decode-operand insn-word spec))
                      (instruction-template-operand-specs template)))
        ;; Update the di if the instruction has a preferred dissassembly.
        (when *disassemble-show-preferred-aliases*
          (let ((rewriter (find-rewriter mnemonic)))
            (when rewriter (funcall rewriter di))))))
    di))

;; True if operand represents the bare, unmodified zero register (xzr/wzr).
(defun zr-operand-p (operand)
  (and (register-operand-p operand)
       (null (register-operand-modifier operand))
       (let ((r (register-operand-register operand)))
         (and (eq (register-family r) :gpr)
              (= (register-number r) 31)
              (not (logtest (register-flags r) $rflag-sp))))))

;; True if operand represents the bare, unmodified stack pointer (sp/wsp).
(defun sp-operand-p (operand)
  (and (register-operand-p operand)
       (null (register-operand-modifier operand))
       (let ((r (register-operand-register operand)))
         (and (eq (register-family r) :gpr)
              (= (register-number r) 31)
              (logtest (register-flags r) $rflag-sp)))))

;; True if operand is an immediate of value 0 with no shift.
(defun zero-immediate-operand-p (operand)
  (and (immediate-operand-p operand)
       (eql (immediate-operand-value operand) 0)
       (let ((shift (immediate-operand-shift operand)))
         (or (null shift) (zerop shift)))))

;; Return list with the nth element omitted.
(defun remove-nth (n list)
  (append (subseq list 0 n) (nthcdr (1+ n) list)))

(defun rewrite-subs (di)
  (let ((operands (di-operands di)))
    (cond
      ((zr-operand-p (first operands))
       ;; subs ZR, Rn, x => cmp Rn, x (where x is anything)
       (setf (di-mnemonic di) "cmp"
             (di-operands di) (rest operands)))
      ((zr-operand-p (second operands))
       ;; subs Rd, ZR, Rm => negs Rd, Rm
       (setf (di-mnemonic di) "negs"
             (di-operands di) (remove-nth 1 operands))))))

(defun rewrite-add (di)
  ;; Rewrite add Rd, Rn, #0 => mov Rd, Rn, but only when Rd or Rn is SP.
  ;; Section C6.2.4 of the manual specifies this special case.
  (let* ((operands (di-operands di))
         (rd (first operands))
         (rn (second operands))
         (imm (third operands)))
    (when (and (zero-immediate-operand-p imm)
               (or (sp-operand-p rd) (sp-operand-p rn)))
      (setf (di-mnemonic di) "mov"
            (di-operands di) (list rd rn)))))

(defun rewrite-drop-zr (di mnemonic index)
  ;; When the operand at index is the zero register, drop it and rename.
  (let ((operands (di-operands di)))
    (when (zr-operand-p (nth index operands))
      (setf (di-mnemonic di) mnemonic
            (di-operands di) (remove-nth index operands)))))

(defun rewrite-adds (di) (rewrite-drop-zr di "cmn" 0))   ;adds ZR, Rn, x
(defun rewrite-ands (di) (rewrite-drop-zr di "tst" 0))   ;ands ZR, Rn, x
(defun rewrite-orn (di) (rewrite-drop-zr di "mvn" 1))    ;orn Rd,xzr,Rm{,shift}
(defun rewrite-sbc (di) (rewrite-drop-zr di "ngc" 1))    ;sbc Rd,xzr,Rm
(defun rewrite-sbcs (di) (rewrite-drop-zr di "ngcs" 1))  ;sbcs Rd,xzr,Rm
(defun rewrite-sub (di) (rewrite-drop-zr di "neg" 1))    ;sub Rd,xzr,Rm{,shift}
(defun rewrite-madd (di) (rewrite-drop-zr di "mul" 3))   ;madd Rd,Rn,Rm,xzr
(defun rewrite-msub (di) (rewrite-drop-zr di "mneg" 3))
(defun rewrite-smaddl (di) (rewrite-drop-zr di "smull" 3))
(defun rewrite-smsubl (di) (rewrite-drop-zr di "smnegl" 3))
(defun rewrite-umaddl (di) (rewrite-drop-zr di "umull" 3))
(defun rewrite-umsubl (di) (rewrite-drop-zr di "umnegl" 3))

(defun movz-form-p (value datasize)
  ;; Can value be encoded as a wide immediate for movz?
  (and (encode-wide-immediate value datasize) t))

(defun movn-form-p (value datasize)
  ;; Can value be encoded as a wide immedaite for movn?  In other words,
  ;; is its datasize-wide inverse valid for movz?
  (movz-form-p (logand (lognot value) (1- (ash 1 datasize))) datasize))

(defun rewrite-orr (di)
  ;; orr Rd, xzr, Rm (unshifted)  => mov Rd, Rm     (register)
  ;; orr Rd, xzr, #bitmask        => mov Rd, #bitmask, but only when the value
  ;; is neither movz- nor movn-representable -- the assembler tries movz, then
  ;; movn, then orr-bitmask, so a value those reach first must stay orr to
  ;; round-trip.  A shifted orr is not a mov and falls through unchanged.
  (let* ((operands (di-operands di))
         (rd (first operands))
         (rn (second operands))
         (rm (third operands))
         (d (register-width (register-operand-register rd))))
    (when (zr-operand-p rn)
      (cond
        ((and (register-operand-p rm)
              (null (register-operand-modifier rm)))
         (setf (di-mnemonic di) "mov" (di-operands di) (list rd rm)))
        ((and (immediate-operand-p rm)
              (let ((v (immediate-operand-value rm)))
                (and (not (movz-form-p v d)) (not (movn-form-p v d)))))
         (setf (di-mnemonic di) "mov" (di-operands di) (list rd rm)))))))

(defun rewrite-movz (di)
  ;; movz Rd, #imm16, lsl #shift  => mov Rd, #(imm16 << shift).  Only the
  ;; non-canonical zero (imm16=0 with a nonzero shift) must stay movz: the
  ;; assembler would re-encode mov #0 with shift 0, a different word.
  (let* ((operands (di-operands di))
         (rd (first operands))
         (imm-op (second operands))
         (imm16 (immediate-operand-value imm-op))
         (shift (or (immediate-operand-shift imm-op) 0)))
    (unless (and (zerop imm16) (plusp shift))
      (setf (di-mnemonic di) "mov"
            (di-operands di) (list rd (decoded-immediate
                                       (ash imm16 shift)))))))

(defun rewrite-movn (di)
  ;; movn Rd, #imm16, lsl #shift  => mov Rd, #~(imm16 << shift).  Skip it when
  ;; the loaded value is movz-representable (the assembler would pick movz and
  ;; produce a different word), or for the non-canonical zero shift.
  (let* ((operands (di-operands di))
         (rd (first operands))
         (imm-op (second operands))
         (imm16 (immediate-operand-value imm-op))
         (shift (or (immediate-operand-shift imm-op) 0))
         (d (register-width (register-operand-register rd)))
         (value (logand (lognot (ash imm16 shift)) (1- (ash 1 d)))))
    (when (and (not (and (zerop imm16) (plusp shift)))
               (not (movz-form-p value d)))
      (setf (di-mnemonic di) "mov"
            (di-operands di) (list rd (decoded-immediate value))))))

;; The variable-shift data-processing instructions are disassembled as
;; lsl/lsr/asr/ror; the operands are unchanged.
(defun rewrite-lslv (di) (setf (di-mnemonic di) "lsl"))
(defun rewrite-lsrv (di) (setf (di-mnemonic di) "lsr"))
(defun rewrite-asrv (di) (setf (di-mnemonic di) "asr"))
(defun rewrite-rorv (di) (setf (di-mnemonic di) "ror"))

(defun same-register-p (a b)
  ;; True if A and B are the same bare GPR (same number, neither modified).
  (and (register-operand-p a) (register-operand-p b)
       (null (register-operand-modifier a))
       (null (register-operand-modifier b))
       (= (register-number (register-operand-register a))
          (register-number (register-operand-register b)))))

(defun invert-condition-operand (cond-op)
  ;; A new condition-operand for the inverse condition, or NIL for al/nv,
  ;; which have no inverse (their low bit isn't a negation).
  (let ((value (condition-operand-value cond-op)))
    (when (< value 14)
      (let ((inverse (logxor value 1)))
        (make-condition-operand :name (lookup-arm64-condition-value inverse)
                                :value inverse)))))

(defun rewrite-cond-select (di set-mnemonic inc-mnemonic)
  ;; csinc/csinv Rd, Rn, Rm, cond:
  ;;   Rn=Rm=zr => cset/csetm Rd, invert(cond)
  ;;   Rn=Rm    => cinc/cinv  Rd, Rn, invert(cond)
  ;; The encoding stores the inverse condition, so the displayed alias inverts
  ;; it back.  al/nv are excluded (no inverse), leaving the canonical form.
  (let* ((operands (di-operands di))
         (rd (first operands))
         (rn (second operands))
         (rm (third operands))
         (inverse (invert-condition-operand (fourth operands))))
    (when (and inverse (same-register-p rn rm))
      (if (zr-operand-p rn)
        (setf (di-mnemonic di) set-mnemonic
              (di-operands di) (list rd inverse))
        (setf (di-mnemonic di) inc-mnemonic
              (di-operands di) (list rd rn inverse))))))

(defun rewrite-csinc (di) (rewrite-cond-select di "cset" "cinc"))
(defun rewrite-csinv (di) (rewrite-cond-select di "csetm" "cinv"))

(defun rewrite-csneg (di)
  ;; csneg Rd, Rn, Rm, cond with Rn=Rm => cneg Rd, Rn, invert(cond).  There is
  ;; no zero-register spelling for csneg.
  (let* ((operands (di-operands di))
         (rn (second operands))
         (inverse (invert-condition-operand (fourth operands))))
    (when (and inverse (same-register-p rn (third operands)))
      (setf (di-mnemonic di) "cneg"
            (di-operands di) (list (first operands) rn inverse)))))

;;; Bitfield aliases.  There are a bunch of them.
;;;
;;; The canonical sbfm/ubfm/bfm carry two raw 6-bit fields, immr and
;;; imms.  Each preferred alias is selected by a condition on them and
;;; then displays them as a shift, a width, or a lsb/width pair.  The
;;; extract forms sbfx/ubfx/bfxil are lapmacros on the assembler side
;;; (their imms is lsb+width-1, coupling both operands), but that is
;;; invisible here: we emit the alias and the lapmacro re-encodes it.
;;; lsb=immr, width=imms-immr+1.

;; Make an immediate operand with the value fully decoded (i.e., any
;; shifts already performed, etc.)
(defun decoded-immediate (value)
  (make-immediate-operand :value value :shift 0))

(defun register-operand-with-width (operand width)
  ;; Return a register operand corresponding to the supplied operand except
  ;; that it has the specified width.  For example: x13 -> w13.
  (make-register-operand
   :register (gpr-ref (register-number (register-operand-register operand))
                      width)))

(defun rewrite-ubfm (di)
  (let* ((operands (di-operands di))
         (rd (first operands))
         (rn (second operands))
         (immr (immediate-operand-value (third operands)))
         (imms (immediate-operand-value (fourth operands)))
         (d (register-width (register-operand-register rd))))
    (cond
      ;; uxtb/uxth: 32-bit zero-extend of a byte/halfword
      ((and (= d 32) (= immr 0) (= imms 7))
       (setf (di-mnemonic di) "uxtb"
             (di-operands di) (list rd rn)))
      ((and (= d 32) (= immr 0) (= imms 15))
       (setf (di-mnemonic di) "uxth"
             (di-operands di) (list rd rn)))
      ;; lsr #immr: imms is 31 or 63
      ((= imms (1- d))
       (setf (di-mnemonic di) "lsr"
             (di-operands di) (list rd rn (decoded-immediate immr))))
      ;; lsl #(d-1-imms): immr is one past imms.  Checked before ubfiz, which
      ;; this also satisfies (imms < immr).
      ((= immr (1+ imms))
       (setf (di-mnemonic di) "lsl"
             (di-operands di) (list rd rn
                                    (decoded-immediate (- (1- d) imms)))))
      ;; ubfiz #lsb, #width
      ((< imms immr)
       (setf (di-mnemonic di) "ubfiz"
             (di-operands di) (list rd rn
                                    (decoded-immediate (logand (- immr)
                                                               (1- d)))
                                    (decoded-immediate (1+ imms)))))
      ;; if we get here: ubfx #lsb, #width
      (t
       (setf (di-mnemonic di) "ubfx"
             (di-operands di) (list rd rn
                                    (decoded-immediate immr)
                                    (decoded-immediate
                                     (- (1+ imms) immr))))))))

(defun rewrite-sbfm (di)
  (let* ((operands (di-operands di))
         (rd (first operands))
         (rn (second operands))
         (immr (immediate-operand-value (third operands)))
         (imms (immediate-operand-value (fourth operands)))
         (d (register-width (register-operand-register rd))))
    (cond
      ;; sxtb/sxth/sxtw: source is a W register
      ((and (= immr 0) (= imms 7))
       (setf (di-mnemonic di) "sxtb"
             (di-operands di) (list rd (register-operand-with-width rn 32))))
      ((and (= immr 0) (= imms 15))
       (setf (di-mnemonic di) "sxth"
             (di-operands di) (list rd (register-operand-with-width rn 32))))
      ((and (= d 64) (= immr 0) (= imms 31))
       (setf (di-mnemonic di) "sxtw"
             (di-operands di) (list rd (register-operand-with-width rn 32))))
      ;; asr #immr
      ((= imms (1- d))
       (setf (di-mnemonic di) "asr"
             (di-operands di) (list rd rn (decoded-immediate immr))))
      ;; sbfiz #lsb, #width
      ((< imms immr)
       (setf (di-mnemonic di) "sbfiz"
             (di-operands di) (list rd rn
                                    (decoded-immediate (logand (- immr)
                                                               (1- d)))
                                    (decoded-immediate (1+ imms)))))
      ;; sbfx #lsb, #width
      (t
       (setf (di-mnemonic di) "sbfx"
             (di-operands di) (list rd rn
                                    (decoded-immediate immr)
                                    (decoded-immediate
                                     (- (1+ imms) immr))))))))

(defun rewrite-bfm (di)
  ;; bfm Rd, Rn, immr, imms:
  ;;   imms < immr, Rn=zr  => bfc Rd, #lsb, #width       (clear)
  ;;   imms < immr         => bfi Rd, Rn, #lsb, #width   (insert)
  ;;   imms >= immr        => bfxil Rd, Rn, #lsb, #width (insert low; lapmacro)
  (let* ((operands (di-operands di))
         (rd (first operands))
         (rn (second operands))
         (immr (immediate-operand-value (third operands)))
         (imms (immediate-operand-value (fourth operands)))
         (d (register-width (register-operand-register rd)))
         (lsb (decoded-immediate (logand (- immr) (1- d))))
         (width (decoded-immediate (1+ imms))))
    (cond
      ((< imms immr)
       (if (zr-operand-p rn)
         (setf (di-mnemonic di) "bfc"
               (di-operands di) (list rd lsb width))
         (setf (di-mnemonic di) "bfi"
               (di-operands di) (list rd rn lsb width))))
      (t
       (setf (di-mnemonic di) "bfxil"
             (di-operands di) (list rd rn
                                    (decoded-immediate immr)
                                    (decoded-immediate
                                     (- (1+ imms) immr))))))))

(defun rewrite-extr (di)
  ;; extr Rd, Rn, Rm, #lsb with Rn=Rm is a rotate: ror Rd, Rn, #lsb.
  (let* ((operands (di-operands di))
         (rn (second operands)))
    (when (same-register-p rn (third operands))
      (setf (di-operands di) (list (first operands) rn (fourth operands))
            (di-mnemonic di) "ror"))))

(defun make-di-vector (code-vector)
  (let* ((n (uvsize code-vector))
         (v (make-array n)))
    (declare (fixnum n) (simple-vector v))
    (dotimes (i n v)
      (declare (fixnum i))
      (setf (svref v i) (disassemble-instruction (uvref code-vector i))))))

(defun resolve-labels (di-vector)
  ;; Connect each branch's label reference to the label it targets.  For a
  ;; reference whose target is in range, name the target instruction's label
  ;; (defining it if necessary) and point the operand at it.  A target
  ;; outside this code vector (a tail-call or a branch into a subprim) keeps
  ;; a nil TARGET and prints as a raw offset.  Because the arm64 code vector
  ;; holds no embedded data, every in-range target names a real instruction.
  (let ((n (length di-vector)))
    (declare (fixnum n))
    (dotimes (i n di-vector)
      (declare (fixnum i))
      (dolist (op (di-operands (svref di-vector i)))
        (when (label-operand-p op)
          (let ((target (+ i (truncate (label-operand-offset op) 4))))
            (when (and (<= 0 target) (< target n))
              (let ((di (svref di-vector target)))
                (setf (label-operand-target op) target
                      (label-operand-name op) (ensure-label di target))))))))))

(defun ensure-label (di index)
  ;; The name of the label defined at DI, defining one (named by its byte
  ;; offset from the start of the code vector, like L0, L4, L8 ...) if this
  ;; is the first reference to it.
  (or (di-label di)
      (setf (di-label di) (format nil "L~d" (* 4 index)))))

(defun print-di-vector (di-vector stream)
  (dotimes (i (length di-vector))
    (let ((di (svref di-vector i)))
      (when (di-label di)
        (format stream "~&~a" (di-label di)))
      (print-di di stream))))

(defun print-di (di stream)
  (let ((operands (di-operands di)))
    (format stream "~&  (~a" (di-mnemonic di))
    (dolist (op operands)
      (write-char #\space stream)
      (print-operand op stream))
    (format stream ")"))
  (when *disassemble-print-instruction-word*
    (format stream "~60t; ~8,'0x" (di-word di))))

(defun print-operand (operand stream)
  (etypecase operand
    (register-operand (print-register-operand operand stream))
    (immediate-operand (print-immediate-operand operand stream))
    (memory-operand (print-memory-operand operand stream))
    (condition-operand (print-condition-operand operand stream))
    (label-operand (print-label-operand operand stream))))

(defun print-label-operand (operand stream)
  ;; A reference that resolved to a local label prints as that label's name;
  ;; one that lands outside this code vector falls back to the signed byte
  ;; displacement.
  (let ((name (label-operand-name operand)))
    (if name
      (write-string name stream)
      (format stream "~d" (label-operand-offset operand)))))

(defun print-register-operand (operand stream)
  (let* ((r (register-operand-register operand))
         (name (register-name r)))
    (when *disassemble-print-lisp-register-names*
      (setq name (or (cdr (assoc name *register-alias-names*
                                 :test #'string-equal))
                     name)))
    (cond
      ((register-operand-modifier operand)
       (format stream "(~a ~(~s~) ~d)" name
               (register-operand-modifier operand)
               (register-operand-amount operand)))
      (t (format stream "~a" name)))))

(defun print-immediate-operand (operand stream)
  (format stream "(:$ ")
  (let ((shift (immediate-operand-shift operand))
        (value (immediate-operand-value operand)))
    (if (> (abs value) *disassemble-print-hex-threshold*)
      (format stream "#x~x" value)
      (format stream "~d" value))
    (when (and shift (/= shift 0))
      (format stream " :lsl ~d" shift))
    (format stream ")")))

(defun print-memory-operand (operand stream)
  (cond ((memory-operand-pre-indexed operand)
         (format stream "(:@! "))
        ((memory-operand-post-indexed operand)
         (format stream "(:@+ "))
        (t
         (format stream "(:@ ")))
  (print-register-operand (memory-operand-base operand) stream)
  (let ((offset (memory-operand-offset operand)))
    (when offset
      (write-char #\space stream)
      (etypecase offset
        (register-operand (print-register-operand offset stream))
        (immediate-operand (print-immediate-operand offset stream))))))
    
(defun print-condition-operand (operand stream)
  (format stream "(:? ~(~a~))" (condition-operand-name operand)))

(defun disassemble-code-vector (code-vector &optional
                                              (stream *debug-io*))
  (let ((di-vector (make-di-vector code-vector)))
    (resolve-labels di-vector)
    (print-di-vector di-vector stream)))

(defun ccl::arm64-disassemble-xfunction (xfunction &optional
                                                     (stream *debug-io*))
  (let* ((code-vector (uvref xfunction 1))
         (di-vector (make-di-vector code-vector)))
    (resolve-labels di-vector)
    (print-di-vector di-vector stream)))
