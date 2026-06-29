;;;;-*- Mode: Lisp; Package: (ARM64 :use CL) -*-
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

(in-package "ARM64")

(defvar *print-lisp-register-names* t)

(defstruct (disassembled-instruction (:conc-name di-))
  (word 0 :type (unsigned-byte 32))
  label                        ;name of the label defined here, or nil
  mnemonic
  operands)

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
    ("csneg" . rewrite-csneg)))

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
        ;; If the instruction has a preferred alias, dissassemble
        ;; to that form.
        (let ((rewriter (cdr (assoc mnemonic *alias-rewriters*
                                    :test 'equalp))))
          (when rewriter (funcall rewriter di)))))
    di))

;; True if operand represents a bare, unmodified zero register (xzr/wzr).
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

(defun rewrite-rd-zr (di mnemonic)
  ;; When the destination is the zero register, drop it and rename the di.
  (let ((operands (di-operands di)))
    (when (zr-operand-p (first operands))
      (setf (di-mnemonic di) mnemonic
            (di-operands di) (rest operands)))))

(defun rewrite-subs (di)
  (let ((operands (di-operands di)))
    (cond
      ((zr-operand-p (first operands))
       (setf (di-mnemonic di) "cmp"
             (di-operands di) (rest operands)))
      ((zr-operand-p (second operands))
       (setf (di-mnemonic di) "negs"
             (di-operands di) (list* (car operands) (cddr operands)))))))

(defun rewrite-adds (di) (rewrite-rd-zr di "cmn"))
(defun rewrite-ands (di) (rewrite-rd-zr di "tst"))

(defun rewrite-add (di)
  ;; add Rd, Rn, #0 => mov Rd, Rn, but only when SP is Rd or Rn.  Between two
  ;; ordinary registers add#0 is not preferred as mov (that's the orr form);
  ;; the mov spelling exists for add because orr cannot name the stack pointer.
  (let* ((operands (di-operands di))
         (rd (first operands))
         (rn (second operands))
         (imm (third operands)))
    (when (and (zero-immediate-operand-p imm)
               (or (sp-operand-p rd) (sp-operand-p rn)))
      (setf (di-mnemonic di) "mov"
            (di-operands di) (list rd rn)))))

(defun remove-nth (n list)
  (append (subseq list 0 n) (nthcdr (1+ n) list)))

(defun rewrite-drop-zr (di mnemonic index)
  ;; When the operand at INDEX is the zero register, drop it and rename.  This
  ;; is the shape behind neg/mvn/ngc/ngcs (Rn=zr, index 1) and the multiply
  ;; aliases mul/mneg/smull/... (Ra=zr, index 3).
  (let ((operands (di-operands di)))
    (when (zr-operand-p (nth index operands))
      (setf (di-mnemonic di) mnemonic
            (di-operands di) (remove-nth index operands)))))

(defun rewrite-sub (di) (rewrite-drop-zr di "neg" 1))    ;sub Rd,xzr,Rm{,shift}
(defun rewrite-sbc (di) (rewrite-drop-zr di "ngc" 1))    ;sbc Rd,xzr,Rm
(defun rewrite-sbcs (di) (rewrite-drop-zr di "ngcs" 1))  ;sbcs Rd,xzr,Rm
(defun rewrite-madd (di) (rewrite-drop-zr di "mul" 3))   ;madd Rd,Rn,Rm,xzr
(defun rewrite-msub (di) (rewrite-drop-zr di "mneg" 3))
(defun rewrite-smaddl (di) (rewrite-drop-zr di "smull" 3))
(defun rewrite-smsubl (di) (rewrite-drop-zr di "smnegl" 3))
(defun rewrite-umaddl (di) (rewrite-drop-zr di "umull" 3))
(defun rewrite-umsubl (di) (rewrite-drop-zr di "umnegl" 3))

(defun rewrite-orn (di) (rewrite-drop-zr di "mvn" 1))    ;orn Rd,xzr,Rm{,shift}

(defun rewrite-orr (di)
  ;; orr Rd, xzr, Rm (unshifted) => mov Rd, Rm (register).  A shifted orr is
  ;; not the register mov, and the bitmask-immediate orr (a different mov) has
  ;; an immediate third operand; both fall through unchanged here.
  (let* ((operands (di-operands di))
         (rn (second operands))
         (rm (third operands)))
    (when (and (zr-operand-p rn)
               (register-operand-p rm)
               (null (register-operand-modifier rm)))
      (setf (di-mnemonic di) "mov"
            (di-operands di) (list (first operands) rm)))))

;; The variable-shift data-processing instructions are always disassembled
;; under their lsl/lsr/asr/ror spellings; the operands are unchanged.
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

(defun disassemble-code-vector (code-vector &optional (stream *standard-output*))
  (print-di-vector stream (make-di-vector code-vector)))

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
  ;; is the first reference to it.  Matches the other CCL disassemblers.
  (or (di-label di)
      (setf (di-label di) (format nil "L~d" (* 4 index)))))

(defun print-di-vector (stream di-vector)
  (dotimes (i (length di-vector))
    (let ((di (svref di-vector i)))
      (when (di-label di)
        (format stream "~&~a" (di-label di)))
      (print-di stream di))))

(defun print-di (stream di)
  (let ((operands (di-operands di)))
    (format stream "~&  (~a" (di-mnemonic di))
    (dolist (op operands)
      (write-char #\space stream)
      (print-operand stream op))
    (format stream ")")))

(defun print-operand (stream operand)
  (etypecase operand
    (register-operand (print-register-operand stream operand))
    (immediate-operand (print-immediate-operand stream operand))
    (memory-operand (print-memory-operand stream operand))
    (condition-operand (print-condition-operand stream operand))
    (label-operand (print-label-operand stream operand))))

(defun print-label-operand (stream operand)
  ;; A reference that resolved to a local label prints as that label's name;
  ;; one that lands outside this code vector falls back to the signed byte
  ;; displacement.
  (let ((name (label-operand-name operand)))
    (if name
      (write-string name stream)
      (format stream "~d" (label-operand-offset operand)))))

(defun print-register-operand (stream operand)
  (let* ((r (register-operand-register operand))
         (name (register-name r)))
    (when *print-lisp-register-names*
      (setq name (or (cdr (assoc name *register-alias-names*
                                 :test #'string-equal))
                     name)))
    (cond
      ((register-operand-modifier operand)
       (format stream "(~a ~(~s~) ~d)" name
               (register-operand-modifier operand)
               (register-operand-amount operand)))
      (t (format stream "~a" name)))))

(defun print-immediate-operand (stream operand)
  (format stream "(:$ ")
  (let ((shift (immediate-operand-shift operand))
        (value (immediate-operand-value operand)))
    (if (and shift (/= shift 0))
      (format stream "~d :lsl ~d)" value shift)
      (format stream "~d)" value))))

(defun print-memory-operand (stream operand)
  (format stream "(:@ ")
  (print-register-operand stream (memory-operand-base operand))
  (let ((offset (memory-operand-offset operand)))
    (when offset
      (write-char #\space stream)
      (etypecase offset
        (register-operand (print-register-operand stream offset))
        (immediate-operand (print-immediate-operand stream offset))))))
    
(defun print-condition-operand (stream operand))

(defun ccl::arm64-disassemble-xfunction (xfunction &optional (stream *debug-io*))
  (let* ((code-vector (uvref xfunction 1))
         (di-vector (make-di-vector code-vector)))
    (resolve-labels di-vector)
    (print-di-vector stream di-vector)))
