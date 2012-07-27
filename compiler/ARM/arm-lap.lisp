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
    (with-dll-node-freelist (data arm::*lap-instruction-freelist*)
    
      (let* ((arm::*lap-labels* ())
             (name-cell (list name))
             (arm::*arm-constants* ())
             (*arm-lap-lfun-bits* bits)
             (current primary)
             (arm::*arm-register-names* arm::*standard-arm-register-names*)
             (sections (vector primary data)))
        (declare (dynamic-extent primary))
        (dolist (form body)
          (setq current (arm-lap-form form current sections)))
        (rplacd name-cell (length arm::*arm-constants*))
        (push name-cell arm::*arm-constants*)
        (arm-lap-generate-code primary
                               (arm::arm-finalize primary (arm-drain-constant-pool primary data))
                               *arm-lap-lfun-bits*)))))



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
            (uvref constants-vector 0) 0)
      #+arm-target (progn
                     (%fix-fn-entrypoint constants-vector)
                     (%make-code-executable code-vector))
      constants-vector)))

;;; This can be called as a result of a :DRAIN-CONSTANT-POOL directive
;;; or at the end of a function. In either case, it shouldn't be possible
;;; for code to reach the point where the constants are appended to
;;; the primary section.
(defun arm-drain-constant-pool (primary constants &optional force)
  (let* ((constants-size (arm::section-size constants)))
    (unless (= constants-size 0)
      (let* ((force-label-name (when force (gensym))))
        (when force
          (arm::assemble-instruction primary `(b ,force-label-name)))
        (when (logtest 7 (arm::section-size primary))
          (arm::assemble-instruction primary '(nop)))
        (let* ((marker (arm::make-lap-instruction nil))
               (code-count (arm::make-lap-instruction nil))
               (constant-count (arm::make-lap-instruction nil)))
          (arm::emit-lap-instruction-element marker primary)
          (arm::emit-lap-instruction-element code-count primary)
          (arm::set-field-value code-count (byte 32 0) (ash (arm::section-size primary) -2))
          (arm::emit-lap-instruction-element constant-count primary)
          (arm::set-field-value constant-count (byte 32 0) (ash (arm::section-size constants) -2))
          (do-dll-nodes (element constants)
            (remove-dll-node element)
            (arm::emit-lap-instruction-element element primary))
          (when force (arm::emit-lap-label primary force-label-name))
          t)))))

  
(defun arm-lap-pseudo-op (directive arg current sections)
  (flet ((check-section (directive section)
           (unless (eq current (svref sections (ecase section
                                                 (:data 1)
                                                 (:code 0))))
             (error "~s directive should only be used inside ~s section"
                    directive section))))
    (ecase directive
      (:arglist (setq *arm-lap-lfun-bits* (encode-lambda-list arg)))
      ((:code :text) (setq current (svref sections 0)))
      (:data (setq current (svref sections 1))
             (when (logtest 7 (arm::section-size current))
               (arm-lap-pseudo-op :word 0 current sections)))
      ((:word :opcode)
       (if (eq directive :word)
         (check-section :word :data)
         (check-section :opcode :code))
       (let* ((val (logand #xffffffff (eval arg)))
              (instruction (arm::make-lap-instruction nil)))
         (setf (arm::lap-instruction-opcode-low instruction)
               (ldb (byte 16 0) val)
               (arm::lap-instruction-opcode-high instruction)
               (ldb (byte 16 16) val))
         (arm::emit-lap-instruction-element instruction current)))
      (:single (check-section :single :data)
               (arm-lap-pseudo-op :word (single-float-bits (float (eval arg) 0.0)) current sections))
      (:double (check-section :double :data)
               (multiple-value-bind (high low)
                   (double-float-bits (float (eval arg) 0.0d0))
                 (arm-lap-pseudo-op :word low current sections)
                 (arm-lap-pseudo-op :word high current sections)))
      (:drain-constant-pool
       (setq current (svref sections 0))
       (arm-drain-constant-pool current (svref sections 1))))
  current))
       

       
(defun arm-lap-form (form current sections)
  (if (and form (symbolp form))
    (arm::emit-lap-label current form)
    (if (or (atom form) (not (symbolp (car form))))
      (error "~& unknown ARM-LAP form: ~S ." form)
      (multiple-value-bind (expansion expanded)
                           (arm-lap-macroexpand-1 form)
        (if expanded
          (setq current (arm-lap-form expansion current sections))
          (let* ((name (car form)))
            (if (keywordp name)
              (setq current (arm-lap-pseudo-op name (cadr form) current sections))
              (case name
                ((progn) (dolist (f (cdr form)) (setq current (arm-lap-form f current sections))))
                ((let) (setq current (arm-lap-equate-form (cadr form) (cddr form) current sections)))
                (t
                 (arm::assemble-instruction current form)))))))))
  current)

;;; (let ((name val) ...) &body body)
;;; each "val" gets a chance to be treated as a ARM register name
;;; before being evaluated.
(defun arm-lap-equate-form (eqlist body current sections)
  (collect ((symbols)
            (vals))
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
                (vals (eval value)))))))
    (progv (symbols) (vals)
      (dolist (form body current)
        (setq current (arm-lap-form form current sections)))))))








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
