;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2010 Clozure Associates
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html


(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "ARM-ARCH")
  (require "DLL-NODE")
  (require "ARM-ASM")
  (require "SUBPRIMS"))


(defun arm-lap-macro-function (name)
  (declare (special *arm-backend*))
  (gethash (string name) (backend-lap-macros *arm-backend*)))

(defun (setf arm-lap-macro-function) (def name)
  (declare (special *arm-backend*))
  (let* ((s (string name)))
    (when (gethash s arm::*arm-instruction-ordinals*)
      (error "~s already defines an arm instruction . " name))
    (setf (gethash s (backend-lap-macros *arm-backend*)) def)))

(defmacro defarmlapmacro (name arglist &body body)
  `(progn
     (setf (arm-lap-macro-function ',name)
           (nfunction (arm-lap-macro ,name) ,(parse-macro name arglist body)))
     (record-source-file ',name 'arm-lap)
     ',name))

(defvar *arm-lap-lfun-bits* 0)






(defun arm-lap-macroexpand-1 (form)
  (unless (and (consp form) (atom (car form)))
    (values form nil))
  (let* ((expander (arm-lap-macro-function (car form))))
    (if expander
      (values (funcall expander form nil) t)
      (values form nil))))





(defun %define-arm-lap-function (name body &optional (bits 0))
  (with-dll-node-freelist (primary arm::*lap-instruction-freelist*)
      (let* ((arm::*lap-labels* ())
             (arm::*called-subprim-jmp-labels* ())
             (name-cell (list name))
             (arm::*arm-constants* ())
             (*arm-lap-lfun-bits* bits)
             (arm::*arm-register-names* arm::*standard-arm-register-names*))
        (dolist (form body)
          (arm-lap-form form primary))
        (rplacd name-cell (length arm::*arm-constants*))
        (push name-cell arm::*arm-constants*)
        (arm-lap-generate-code primary
                               (arm::arm-finalize primary)
                               *arm-lap-lfun-bits*))))



#+big-endian-host
(defun set-arm-code-vector-word (code-vector i insn)
  (setf (uvref code-vector i)
        (logior (ash (arm::lap-instruction-opcode-high insn) 16)
                         (arm::lap-instruction-opcode-low insn))))

#+little-endian-host
(defun set-arm-code-vector-word (code-vector i insn)
  (declare (type (simple-array (unsigned-byte 16) (*)) code-vector)
           (fixnum i)
           (optimize (speed 3) (safety 0)))
  (let* ((j (+ i i)))
    (declare (fixnum j))
    (setf (aref code-vector j) (arm::lap-instruction-opcode-low insn)
          (aref code-vector (the fixnum (1+ j)))
          (arm::lap-instruction-opcode-high insn))))
    
  



(defun arm-lap-generate-code (seg code-vector-size bits)
  (declare (fixnum code-vector-size))
  (let* ((target-backend *target-backend*)
         (cross-compiling (target-arch-case
                           (:arm (not (eq *host-backend* target-backend)))
                           (t t)))
         (constants-size (+ 3 (length arm::*arm-constants*)))
         (constants-vector (%alloc-misc
                            constants-size
			    (if cross-compiling
			      target::subtag-xfunction
			      target::subtag-function)))
         (i 0))
    (declare (fixnum i constants-size))
    (let* ((code-vector (%alloc-misc
                         code-vector-size
                         (if cross-compiling
                           target::subtag-xcode-vector
                           arm::subtag-code-vector))))
      (do-dll-nodes (insn seg)
        (unless (eql (arm::instruction-element-size insn) 0)
          (set-arm-code-vector-word code-vector i insn)
          (incf i)))
      (dolist (immpair arm::*arm-constants*)
        (let* ((imm (car immpair))
               (k (cdr immpair)))
          (declare (fixnum k))
          (setf (uvref constants-vector (+ 2 k)) imm)))
      (setf (uvref constants-vector (1- constants-size)) bits ; lfun-bits
            (uvref constants-vector 1) code-vector
            (uvref constants-vector 0) (ash (arm::arm-subprimitive-address '.SPfix-nfn-entrypoint) (- arm::fixnumshift)))
      #+arm-target (%make-code-executable code-vector)
      constants-vector)))

(defun arm-lap-pseudo-op (directive arg)
  (ecase directive
    (:arglist (setq *arm-lap-lfun-bits* (encode-lambda-list arg)))))
       

       
(defun arm-lap-form (form seg)
  (if (and form (symbolp form))
    (arm::emit-lap-label seg form)
    (if (or (atom form) (not (symbolp (car form))))
      (error "~& unknown ARM-LAP form: ~S ." form)
      (multiple-value-bind (expansion expanded)
                           (arm-lap-macroexpand-1 form)
        (if expanded
          (arm-lap-form expansion seg)
          (let* ((name (car form)))
            (if (keywordp name)
              (arm-lap-pseudo-op name (cadr form))
              (case name
                ((progn) (dolist (f (cdr form)) (arm-lap-form f seg)))
                ((let) (arm-lap-equate-form (cadr form) (cddr form) seg))
                (t
                 (arm::assemble-instruction seg form))))))))))

;;; (let ((name val) ...) &body body)
;;; each "val" gets a chance to be treated as a ARM register name
;;; before being evaluated.
(defun arm-lap-equate-form (eqlist body seg)
  (collect ((symbols)
            (values))
    (let* ((arm::*arm-register-names* arm::*arm-register-names*))
      (dolist (pair eqlist)
        (destructuring-bind (symbol value) pair
          (unless (and symbol
                       (symbolp symbol)
                       (not (constant-symbol-p symbol))
                       (not (arm::get-arm-register symbol)))
            (error "~s is not a bindable symbol name . " symbol))
          (let* ((regval (and value
                              (or (typep value 'symbol)
                                  (typep value 'string))
                              (arm::get-arm-register value))))
            (if regval
              (arm::define-arm-register symbol regval)
              (progn
                (symbols symbol)
                (values (eval value)))))))

    (progv (symbols) (values)
      (dolist (form body)
        (arm-lap-form form seg))))))








(defmacro defarmlapfunction (&environment env name arglist &body body
                             &aux doc)
  (if (not (endp body))
      (and (stringp (car body))
           (cdr body)
           (setq doc (car body))
           (setq body (cdr body))))
  `(progn
     (eval-when (:compile-toplevel)
       (note-function-info ',name t ,env))
     #-arm-target
     (progn
       (eval-when (:load-toplevel)
         (%defun (nfunction ,name (lambda (&lap 0) (arm-lap-function ,name ,arglist ,@body))) ,doc))
       (eval-when (:execute)
         (%define-arm-lap-function ',name '((let ,arglist ,@body)))))
     #+arm-target	; just shorthand for defun
     (%defun (nfunction ,name (lambda (&lap 0) (arm-lap-function ,name ,arglist ,@body))) ,doc)))
 


(provide "ARM-LAP")
