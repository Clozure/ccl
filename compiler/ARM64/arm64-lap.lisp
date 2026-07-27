;;;; -*- Mode: Lisp; Package: CCL -*-
;;;; SPDX-License-Identifier: Apache-2.0

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "ARM64-ARCH")
  (require "DLL-NODE")
  (require "ARM64-ASM"))

;;; Brief syntax for arm64 LAP notation
;;;
;;; register
;;;   * a symbol naming a register: x0, w0, s0, d0, sp/xzr/wsp/wzr or an alias
;;;
;;; immediate
;;; (:$ val)
;;; (:$ val :lsl amount)
;;; (:? condition)
;;;
;;; memory
;;; (:@ base)
;;; (:@ base offset)
;;; (:@! base offset)   ;pre-indexed
;;; (:@+ base offset)   ;post-indexed
;;; base is a bare register operand; offset is either an immediate (:$ n)
;;; or a register, possibly modified, e.g. (:@ x0 (x1 :lsl 3)) or
;;; (:@ x0 (w1 :uxtw 2)).
;;;
;;; label
;;;  * a symbol that doesn't name a register

(defvar *arm64-lap-lfun-bits* 0)

(defun arm64-lap-macro-function (name)
  (declare (special *arm64-backend*))
  (gethash (string name) (backend-lap-macros *arm64-backend*)))

(defun (setf arm64-lap-macro-function) (def name)
  (declare (special *arm64-backend*))
  (let* ((s (string name)))
    (when (gethash s arm64::*instruction-template-lists*)
      (error "~s already defines an arm64 instruction. " name))
    (setf (gethash s (backend-lap-macros *arm64-backend*)) def)))

(defmacro defarm64lapmacro (name arglist &body body)
  `(progn
     (setf (arm64-lap-macro-function ',name)
           (nfunction (arm64-lap-macro ,name)
                      ,(parse-macro name arglist body)))
     (record-source-file ',name 'lap-macro)
     ',name))

(defun arm64-lap-macroexpand-1 (form)
  (if (and (consp form) (atom (car form)))
    (let* ((expander (arm64-lap-macro-function (car form))))
      (if expander
        (values (funcall expander form nil) t)
        (values form nil)))
    (values form nil)))

(defun arm64-show-dll-nodes (elements)
  (do-dll-nodes (e elements)
    (format t "~&~s " e)
    (when (typep e 'arm64::instruction)
      (format t "~x" (arm64::instruction-word e))))
  (terpri))

(defun %define-arm64-lap-function (name body &optional (bits 0))
  (with-dll-node-freelist (elements arm64::*instruction-freelist*)
    (let* ((arm64::*labels* ())
           (arm64::*constants* ())
           (*arm64-lap-lfun-bits* bits)
           (name-cell (list name))
           (section-size -1)
           (current elements))
      (dolist (form body)
        (setq current (arm64-lap-form form current)))
      (rplacd name-cell (length arm64::*constants*))
      (push name-cell arm64::*constants*)
      (setq section-size (arm64::finalize current))
      ;; (format t "~&section size: ~s" section-size)
      ;; (arm64-show-dll-nodes current)
      (arm64-lap-generate-code current section-size *arm64-lap-lfun-bits*)
      )))

(defun arm64-lap-generate-code (seg code-vector-size &optional (lfbits 0))
  (declare (fixnum code-vector-size))
  (let* ((target-backend *target-backend*)
         (cross-compiling (target-arch-case
                           (:arm64 (not (eq *host-backend* target-backend)))
                           (t t)))
         (prefix (arch::target-code-vector-prefix
                  (backend-target-arch *target-backend*)))
         (prefix-size (length prefix))
         (constants-size (+ 2 (length arm64::*constants*)))
         (constants-vector (%alloc-misc constants-size
                                        (if cross-compiling
                                          target::subtag-xfunction
                                          target::subtag-function)))
         (i prefix-size))
    (declare (fixnum i constants-size))
    (let* ((code-vector (%alloc-misc
                         (+ code-vector-size prefix-size)
                         (if cross-compiling
                           target::subtag-xcode-vector
                           arm64::subtag-code-vector))))
      (dotimes (j prefix-size)
        (setf (uvref code-vector j) (pop prefix)))
      (do-dll-nodes (insn seg)
        (unless (eql (arm64::instruction-element-size insn) 0)
          (setf (uvref code-vector i) (arm64::instruction-word insn))
          (incf i)))
      (dolist (pair arm64::*constants*)
        (let ((imm (car pair))
              (k (cdr pair)))
          (setf (uvref constants-vector (1+ k)) imm)))
      (setf (uvref constants-vector (1- constants-size)) lfbits
            (uvref constants-vector 0) code-vector)
      constants-vector)))

(defun arm64-lap-pseudo-op (directive arg current)
  (ecase directive
    (:arglist (setq *arm64-lap-lfun-bits* (encode-lambda-list arg)))
    (:opcode (let* ((val (logand #xffffffff (eval arg)))
                    (insn (arm64::make-instruction nil)))
               (setf (arm64::instruction-word insn) val)
               (arm64::emit-element current insn))))
  current)

;;; (let ((name val) ...) &body body)
;;; each "val" gets a chance to be treated as an ARM64 register name
;;; before being evaluated.
(defun arm64-lap-equate-form (eqlist body current)
  (collect ((symbols)
            (vals)
            (rsymbols)
            (rvals))
    (dolist (pair eqlist)
      (destructuring-bind (symbol value) pair
        (unless (and symbol (symbolp symbol)
                     (not (constant-symbol-p symbol))
                     (not (arm64::lookup-register symbol)))
            (error "~s is not a bindable symbol name." symbol))
        (let ((regval (and value
                           (or (typep value 'symbol)
                               (typep value 'string))
                           (arm64::lookup-register value))))
          (if regval
            (progn
              (rsymbols symbol)
              (rvals regval))
            (progn
              (symbols symbol)
              (vals (eval value)))))))
    ;; Keep registers separate from the progv bindings so the assembler
    ;; can tell a register alias from a label without having to eval.
    (let ((arm64::*lap-register-equates*
            (pairlis (rsymbols) (rvals) arm64::*lap-register-equates*)))
      (progv (symbols) (vals)
        (dolist (form body current)
          (setq current (arm64-lap-form form current)))))))

(defun arm64-lap-form (form current)
  (if (and form (symbolp form))
    (arm64::emit-label current form)
    (if (or (atom form) (not (symbolp (car form))))
      (error "Invalid arm64 lap form ~s" form)
      (multiple-value-bind (expansion expanded)
          (arm64-lap-macroexpand-1 form)
        (if expanded
          (setq current (arm64-lap-form expansion current))
          (let ((name (car form)))
            (if (keywordp name)
              (setq current
                    (arm64-lap-pseudo-op name (cadr form) current))
              (case name
                ((progn) (dolist (f (cdr form))
                           (setq current (arm64-lap-form f current))))
                ((let) (setq current (arm64-lap-equate-form (cadr form)
                                                            (cddr form)
                                                            current)))
                (t
                 (arm64::assemble-instruction current form)))))))))
  current)

(defmacro defarm64lapfunction (&environment env name arglist &body body
                               &aux doc)
  (if (not (endp body))
    (and (stringp (car body))
         (cdr body)
         (setq doc (car body))
         (setq body (cdr body))))
  `(progn
     (eval-when (:compile-toplevel)
       (note-function-info ',name t ,env))
     #-arm64-target
     (progn
       (eval-when (:load-toplevel)
         (%defun (nfunction ,name
                            (lambda (&lap 0)
                              (arm64-lap-function ,name ,arglist ,@body)))
                 ,doc))
       (eval-when (:execute)
         (%define-arm64-lap-function ',name '((let ,arglist ,@body)))))
     #+arm64-target	; just shorthand for defun
     (%defun (nfunction ,name
                        (lambda (&lap 0)
                          (arm64-lap-function ,name ,arglist ,@body)))
             ,doc)))
