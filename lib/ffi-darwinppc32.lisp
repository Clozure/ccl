;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2007-2009 Clozure Associates and contributors
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

;;; If a record type has a single scalar field, return the type
;;; of that field.
(defun darwin32::record-type-has-single-scalar-field (record-type)
  (when (eq (foreign-record-type-kind record-type) :struct)
    (require-foreign-type-bits record-type)
    (let* ((fields (foreign-record-type-fields record-type)))
      (when (null (cdr fields))
        (let* ((f0 (car fields))
               (type (foreign-record-field-type f0)))
          (typecase type
            ((or foreign-record-type foreign-array-type) nil)
            (otherwise type)))))))

;;; If type denotes a foreign record type, return T if it would
;;; be "returned" by passing it as the first argument to the callee.
;;; On DarwinPPC32, this is true of all record types except for
;;; those for which RECORD-TYPE-HAS-SINGLE-SCALAR-FIELD returns
;;; true.
(defun darwin32::record-type-returns-structure-as-first-arg (rtype)
  (when (and rtype
             (not (typep rtype 'unsigned-byte))
             (not (member rtype *foreign-representation-type-keywords*
                          :test #'eq)))
    (let* ((ftype (if (typep rtype 'foreign-type)
                    rtype
                    (parse-foreign-type rtype))))
      (and (typep ftype 'foreign-record-type)
           (not (darwin32::record-type-has-single-scalar-field ftype))))))


;;; Structures that contain a single scalar field are "returned"
;;; as a value with that field's type.
;;; Other structures are "returned" by passing a pointer to a structure
;;; of the appropriate type as the first argument.
;;; Structures that contain a single scalar field are passed by value
;;; by passing the value of that field as a scalar.
;;; Structures that contain more than one field are passed by value
;;; as a sequence of N 32-bit words; %ff-call understands an unsigned
;;; integer argument "type" specifier to denote this.

(defun darwin32::expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
  (let* ((result-type-spec (or (car (last args)) :void))
         (enclosing-form nil))
    (multiple-value-bind (result-type error)
        (ignore-errors (parse-foreign-type result-type-spec))
      (if error
        (setq result-type-spec :void result-type *void-foreign-type*)
        (setq args (butlast args)))
      (collect ((argforms))
        (when (eq (car args) :monitor-exception-ports)
          (argforms (pop args)))
        (when (typep result-type 'foreign-record-type)
          (let* ((single-scalar (darwin32::record-type-has-single-scalar-field result-type))
                 (result-form (pop args)))
            (if single-scalar
              (progn
                (setq enclosing-form `(setf ,(%foreign-access-form result-form single-scalar 0 nil))
                      result-type single-scalar
                      result-type-spec (foreign-type-to-representation-type result-type)))
                      
              (progn
                (argforms :address)
                (argforms result-form)
                (setq result-type *void-foreign-type*
                      result-type-spec :void)))))
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
                  (let* ((single-scalar (darwin32::record-type-has-single-scalar-field ftype)))
                    (if single-scalar
                      (progn
                        (argforms (foreign-type-to-representation-type single-scalar))
                        (argforms (%foreign-access-form arg-value-form single-scalar 0 nil)))
                      (let* ((bits (ensure-foreign-type-bits ftype)))
                        (argforms (ceiling bits 32))
                        (argforms arg-value-form))))
                  (progn
                    (argforms (foreign-type-to-representation-type ftype))
                    (argforms (funcall arg-coerce arg-type-spec arg-value-form))))))))
        (argforms (foreign-type-to-representation-type result-type))
        (let* ((call (funcall result-coerce result-type-spec `(,@callform ,@(argforms)))))
          (if enclosing-form
            `(,@enclosing-form ,call)
            call))))))
                  
            
            
;;; Return 7 values:
;;; A list of RLET bindings
;;; A list of LET* bindings
;;; A list of DYNAMIC-EXTENT declarations for the LET* bindings
;;; A list of initializaton forms for (some) structure args
;;; A FOREIGN-TYPE representing the "actual" return type.
;;; A form which can be used to initialize FP-ARGS-PTR, relative
;;;  to STACK-PTR.  (This is unused on linuxppc32.)
;;; The byte offset of the foreign return address, relative to STACK-PTR

(defun darwin32::generate-callback-bindings (stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name)
  (collect ((lets)
            (rlets)
            (inits)
            (dynamic-extent-names))
    (let* ((rtype (parse-foreign-type result-spec))
           (fp-regs-form nil))
      (flet ((set-fp-regs-form ()
               (unless fp-regs-form
                 (setq fp-regs-form `(%get-ptr ,stack-ptr ,(- ppc32::c-frame.unused-1 ppc32::c-frame.param0))))))
        (when (typep rtype 'foreign-record-type)
          (if (darwin32::record-type-has-single-scalar-field rtype)
            (rlets (list struct-result-name (foreign-record-type-name rtype)))
            (setq argvars (cons struct-result-name argvars)
                  argspecs (cons :address argspecs)
                  rtype *void-foreign-type*)))
        (when (typep rtype 'foreign-float-type)
          (set-fp-regs-form))
        (do* ((argvars argvars (cdr argvars))
              (argspecs argspecs (cdr argspecs))
              (fp-arg-num 0)
              (offset 0 (+ offset delta))
              (delta 4 4)
              (bias 0 0)
              (use-fp-args nil nil))
             ((null argvars)
              (values (rlets) (lets) (dynamic-extent-names) (inits) rtype fp-regs-form (- ppc32::c-frame.savelr ppc32::c-frame.param0)))
          (flet ((next-scalar-arg (argtype)
                   `(,(cond
                       ((typep argtype 'foreign-single-float-type)
                        (if (< (incf fp-arg-num) 14)
                          (progn
                            (setq use-fp-args t)
                            '%get-single-float-from-double-ptr)
                          (progn
                            '%get-single-float)))
                       ((typep argtype 'foreign-double-float-type)
                        (setq delta 8)
                        (if (< (incf fp-arg-num) 14)
                          (setq use-fp-args t))
                        '%get-double-float)
                       ((and (typep argtype 'foreign-integer-type)
                             (= (foreign-integer-type-bits argtype) 64)
                             (foreign-integer-type-signed argtype))
                        (setq delta 8)
                        '%%get-signed-longlong)
                       ((and (typep argtype 'foreign-integer-type)
                             (= (foreign-integer-type-bits argtype) 64)
                             (not (foreign-integer-type-signed argtype)))
                        (setq delta 8)
                        '%%get-unsigned-longlong)
                       ((or (typep argtype 'foreign-pointer-type)
                            (typep argtype 'foreign-array-type))
                        '%get-ptr)
                       (t
                        (cond ((typep argtype 'foreign-integer-type)
                               (let* ((bits (foreign-integer-type-bits argtype))
                                      (signed (foreign-integer-type-signed argtype)))
                                 (cond ((<= bits 8)
                                        (setq bias 3)
                                        (if signed
                                          '%get-signed-byte
                                          '%get-unsigned-byte))
                                       ((<= bits 16)
                                        (setq bias 2)
                                        (if signed
                                          '%get-signed-word 
                                          '%get-unsigned-word))
                                       ((<= bits 32)
                                        (if signed
                                          '%get-signed-long 
                                          '%get-unsigned-long))
                                       (t
                                        (error "Don't know how to access foreign argument of type ~s" (unparse-foreign-type argtype))))))
                              (t
                               (error "Don't know how to access foreign argument of type ~s" (unparse-foreign-type argtype))))))
                     ,(if use-fp-args fp-args-ptr stack-ptr)
                     ,(if use-fp-args (* 8 (1- fp-arg-num))
                          (+ offset bias)))))                   
          (let* ((name (car argvars))
                 (spec (car argspecs))
                 (argtype (parse-foreign-type spec)))
            (if (typep argtype 'foreign-record-type)
              (let* ((type0 (darwin32::record-type-has-single-scalar-field argtype)))
                (if type0
                  (progn
                    (when name (rlets (list name (foreign-record-type-name argtype))))
                    (let* ((init `(setf ,(%foreign-access-form name type0 0 nil)
                             ,(next-scalar-arg type0))))
                      (when name (inits init))))
                  (progn
                    (setq delta (* (ceiling (foreign-record-type-bits argtype) 32) 4))
                    (when name ; no side-efects hers     
                    (lets (list name `(%inc-ptr ,stack-ptr ,offset)))))))
              (let* ((pair (list name (next-scalar-arg argtype))))
                (when name (lets pair))))
            #+nil
            (when (or (typep argtype 'foreign-pointer-type)
                      (typep argtype 'foreign-array-type))
              (dynamic-extent-names name))
            (when use-fp-args (set-fp-regs-form)))))))))

(defun darwin32::generate-callback-return-value (stack-ptr fp-args-ptr result return-type struct-return-arg)
  (unless (eq return-type *void-foreign-type*)
    ;; Coerce SINGLE-FLOAT result to DOUBLE-FLOAT
    (when (typep return-type 'foreign-single-float-type)
      (setq result `(float ,result 0.0d0)))    
    (when (typep return-type 'foreign-record-type)
      ;;; Would have been mapped to :VOID unless record-type contained
      ;;; a single scalar field.
      (let* ((field0 (car (foreign-record-type-fields return-type))))
        (setq result (%foreign-access-form struct-return-arg
                                           (foreign-record-field-type field0)
                                           0
                                           nil)
              return-type (foreign-record-field-type field0))))
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
                                  '%get-double-float)
                                 (:unsigned-fullword '%get-unsigned-long)
                                 (t '%get-long )
                                 ) ,result-ptr 0) ,result))))

