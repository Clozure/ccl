;;;;-*- Mode: Lisp; Package: (ARM64 :use CL) -*-
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

(in-package "ARM64")

(defvar *print-lisp-register-names* t)

(defstruct disassembled-instruction
  (word 0 :type (unsigned-byte 32))
  label                        ;name of the label defined here, or nil
  mnemonic
  operands)

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
      (setf (disassembled-instruction-mnemonic di) "<unknown>")
      (progn
        (setf (disassembled-instruction-mnemonic di)
              (instruction-template-name template))
        (setf (disassembled-instruction-operands di)
              (mapcar #'(lambda (spec)
                          (decode-operand insn-word spec))
                      (instruction-template-operand-specs template)))))
    di))

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
      (dolist (op (disassembled-instruction-operands (svref di-vector i)))
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
  (or (disassembled-instruction-label di)
      (setf (disassembled-instruction-label di) (format nil "L~d" (* 4 index)))))

(defun print-di-vector (stream di-vector)
  (dotimes (i (length di-vector))
    (let ((di (svref di-vector i)))
      (when (disassembled-instruction-label di)
        (format stream "~&~a" (disassembled-instruction-label di)))
      (print-di stream di))))

(defun print-di (stream di)
  (let ((operands (disassembled-instruction-operands di)))
    (format stream "~&  (~a" (disassembled-instruction-mnemonic di))
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
