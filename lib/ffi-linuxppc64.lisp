;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2007, Clozure Associates and contributors
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

;;; LinuxPPC64
;;; Structures whose size is less than 64 bits are passed "right-justified"
;;; in a GPR.
;;; Larger structures passed by value are passed in GPRs as N doublewords.
;;; If the structure would require > 64-bit alignment, this might result
;;; in some GPRs/parameter area words being skipped.  (We don't handle this).
;;; All structures - of any size - are returned by passing a pointer
;;; in the first argument.

(defun linux64::record-type-returns-structure-as-first-arg (rtype)
  (when (and rtype
             (not (typep rtype 'unsigned-byte))
             (not (member rtype *foreign-representation-type-keywords*
                          :test #'eq)))
    (let* ((ftype (if (typep rtype 'foreign-type)
                    rtype
                    (parse-foreign-type rtype))))
      (typep ftype 'foreign-record-type))))

(defun linux64::expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
  (let* ((result-type-spec (or (car (last args)) :void)))
    (multiple-value-bind (result-type error)
        (ignore-errors (parse-foreign-type result-type-spec))
      (if error
        (setq result-type-spec :void result-type *void-foreign-type*)
        (setq args (butlast args)))
      (collect ((argforms))
        (when (eq (car args) :monitor-exception-ports)
          (argforms (pop args)))
        (when (typep result-type 'foreign-record-type)
          (setq result-type *void-foreign-type*
                result-type-spec :void)
          (argforms :address)
          (argforms (pop args)))
        (unless (evenp (length args))
          (error "~s should be an even-length list of alternating foreign types and values" args))        
        (do* ((args args (cddr args)))
             ((null args))
          (let* ((arg-type-spec (car args))
                 (arg-value-form (cadr args)))
            (if (or (member arg-type-spec *foreign-representation-type-keywords*
                            :test #'eq)
                    (typep arg-type-spec 'unsigned-byte))
              (progn
                (argforms arg-type-spec)
                (argforms arg-value-form))
              (let* ((ftype (parse-foreign-type arg-type-spec)))
                (if (typep ftype 'foreign-record-type)
                  (let* ((bits (ensure-foreign-type-bits ftype)))
                    (if (< bits 64)
                      (progn
                        (argforms :unsigned-doubleword)
                        (argforms `(ash (%%get-unsigned-longlong ,arg-value-form 0) ,(- bits 64))))
                      (progn
                        (argforms (ceiling bits 64))
                        (argforms arg-value-form))))
                  (progn
                    (argforms (foreign-type-to-representation-type ftype))
                    (argforms (funcall arg-coerce arg-type-spec arg-value-form))))))))
        (argforms (foreign-type-to-representation-type result-type))
        (funcall result-coerce result-type-spec `(,@callform ,@(argforms)))))))

(defun linux64::generate-callback-bindings (stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name)
  (collect ((lets)
            (rlets)
            (inits)
            (dynamic-extent-names))
    (let* ((rtype (parse-foreign-type result-spec))
           (fp-regs-form nil))
      (flet ((set-fp-regs-form ()
               (unless fp-regs-form
                 (setq fp-regs-form `(%get-ptr ,stack-ptr ,(- ppc64::c-frame.unused-1 ppc64::c-frame.param0))))))
        (when (typep rtype 'foreign-record-type)
          (setq argvars (cons struct-result-name argvars)
                argspecs (cons :address argspecs)
                rtype *void-foreign-type*))
        (when (typep rtype 'foreign-float-type)
          (set-fp-regs-form))
        (do* ((argvars argvars (cdr argvars))
              (argspecs argspecs (cdr argspecs))
              (fp-arg-num 0)
              (offset 0 (+ offset delta))
              (delta 8 8)
              (bias 0 0)
              (use-fp-args nil nil))
             ((null argvars)
              (values (rlets) (lets) (dynamic-extent-names) (inits) rtype fp-regs-form (- ppc64::c-frame.savelr ppc64::c-frame.param0)))
          (let* ((name (car argvars))
                 (spec (car argspecs))
                 (argtype (parse-foreign-type spec))
                 (bits (ensure-foreign-type-bits argtype)))
            (if (and (typep argtype 'foreign-record-type)
                     (< bits 64))
              (progn
                (when name (rlets (list name (foreign-record-type-name argtype))))
                (when name (inits `(setf (%%get-unsigned-longlong ,name 0)
                                    (ash (%%get-unsigned-longlong ,stack-ptr ,offset)
                                     ,(- 64 bits))))))
              (let* ((access-form
                      `(,(cond
                          ((typep argtype 'foreign-single-float-type)
                           (if (< (incf fp-arg-num) 14)
                             (progn
                               (setq use-fp-args t)
                               '%get-single-float-from-double-ptr)
                             (progn
                               (setq bias 4)
                               '%get-single-float)))
                          ((typep argtype 'foreign-double-float-type)
                           (if (< (incf fp-arg-num) 14)
                             (setq use-fp-args t))
                           '%get-double-float)
                          ((and (typep argtype 'foreign-integer-type)
                                (= (foreign-integer-type-bits argtype) 64)
                                (foreign-integer-type-signed argtype))
                           '%%get-signed-longlong)
                          ((and (typep argtype 'foreign-integer-type)
                                (= (foreign-integer-type-bits argtype) 64)
                                (not (foreign-integer-type-signed argtype)))
                           '%%get-unsigned-longlong)
                          ((or (typep argtype 'foreign-pointer-type)
                               (typep argtype 'foreign-array-type))
                           '%get-ptr)
                          ((typep argtype 'foreign-record-type)
                           (setq delta (* (ceiling bits 64) 8))
                           '%inc-ptr)
                          (t
                           (cond ((typep argtype 'foreign-integer-type)
                                  (let* ((bits (foreign-integer-type-bits argtype))
                                         (signed (foreign-integer-type-signed argtype)))
                                    (cond ((<= bits 8)
                                           (setq bias 7)
                                           (if signed
                                             '%get-signed-byte '
                                             '%get-unsigned-byte))
                                          ((<= bits 16)
                                           (setq bias 6)
                                           (if signed
                                             '%get-signed-word 
                                             '%get-unsigned-word))
                                          ((<= bits 32)
                                           (setq bias 4)
                                           (if signed
                                             '%get-signed-long 
                                             '%get-unsigned-long))
                                          (t
                                           (error "Don't know how to access foreign argument of type ~s" (unparse-foreign-type argtype))))))
                                 (t
                                  (error "Don't know how to access foreign argument of type ~s" (unparse-foreign-type argtype))))))
                        ,(if use-fp-args fp-args-ptr stack-ptr)
                        ,(if use-fp-args (* 8 (1- fp-arg-num))
                             `(+ ,offset ,bias)))))
                (when name (lets (list name access-form)))
                #+nil
                (when (eq spec :address)
                  (dynamic-extent-names name))
                (when use-fp-args (set-fp-regs-form))))))))))


;;; All structures are "returned" via the implicit first argument; we'll have
;;; already translated the return type to :void in that case.
(defun linux64::generate-callback-return-value (stack-ptr fp-args-ptr result return-type struct-return-arg)
  (declare (ignore struct-return-arg))
  (unless (eq return-type *void-foreign-type*)
    (when (typep return-type 'foreign-single-float-type)
      (setq result `(float ,result 0.0d0)))    
    (let* ((return-type-keyword (foreign-type-to-representation-type return-type))
           (result-ptr (case return-type-keyword
                   ((:single-float :double-float)
                    fp-args-ptr)
                   (t stack-ptr))))
      `(setf (,
              (case return-type-keyword
                                 (:address '%get-ptr)
                                 (:signed-doubleword '%%get-signed-longlong)
                                 (:unsigned-doubleword '%%get-unsigned-longlong)
                                 ((:double-float :single-float)
                                  (setq stack-ptr `(%get-ptr ,stack-ptr ,(- ppc64::c-frame.unused-1 ppc64::c-frame.param0)))
                                  '%get-double-float)
                                 (t '%%get-signed-longlong )
                                 ) ,result-ptr 0) ,result))))
