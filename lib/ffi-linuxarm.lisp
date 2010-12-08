;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2010 Clozure Associates and contributors
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

;;; LinuxARM:
;;; Structures whose size is 64 bits are passed by value; the caller
;;; instead passes a pointer to the structure or a copy of it.
;;; Structures whose size is <= 32 bits are returned as scalars.
(defun arm-linux::record-type-returns-structure-as-first-arg (rtype)
  (when (and rtype
             (not (typep rtype 'unsigned-byte))
             (not (member rtype *foreign-representation-type-keywords*
                          :test #'eq)))
    (let* ((ftype (if (typep rtype 'foreign-type)
                    rtype
                    (parse-foreign-type rtype))))
      (when (typep ftype 'foreign-record-type)
        (ensure-foreign-type-bits ftype)
        (> (foreign-type-bits ftype) 32)))))


(defun arm-linux::expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
  (let* ((result-type-spec (or (car (last args)) :void))
         (enclosing-form nil)
         (result-form nil))
    (multiple-value-bind (result-type error)
        (ignore-errors (parse-foreign-type result-type-spec))
      (if error
        (setq result-type-spec :void result-type *void-foreign-type*)
        (setq args (butlast args)))
      (collect ((argforms))
        (when (typep result-type 'foreign-record-type)
          (setq result-form (pop args))
          (if (arm-linux::record-type-returns-structure-as-first-arg result-type)
            (progn
              (setq result-type *void-foreign-type*
                    result-type-spec :void)
              (argforms :address)
              (argforms result-form))
            ;; This only happens in the SVR4 ABI.
            (progn
              (setq result-type (parse-foreign-type :unsigned-doubleword)
                    result-type-spec :unsigned-doubleword
                    enclosing-form `(setf (%%get-unsigned-longlong ,result-form 0))))))
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
                  (progn
                    (argforms :address)
                    (argforms arg-value-form))
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
;;;  to STACK-PTR.  (This is unused on linuxarm.)
;;; The byte offset of the foreign return address, relative to STACK-PTR
(defun arm-linux::generate-callback-bindings (stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name)
  (declare (ignore fp-args-ptr))
  (collect ((lets)
            (rlets)
            (dynamic-extent-names))
    (let* ((rtype (parse-foreign-type result-spec)))
      (when (typep rtype 'foreign-record-type)
        (let* ((bits (ensure-foreign-type-bits rtype)))
          (if (<= bits 64)
            (rlets (list struct-result-name (foreign-record-type-name rtype)))
            (setq argvars (cons struct-result-name argvars)
                  argspecs (cons :address argspecs)
                  rtype *void-foreign-type*))))
          (let* ((offset 0)
                 (nextoffset offset))
            (do* ((argvars argvars (cdr argvars))
                  (argspecs argspecs (cdr argspecs)))
                 ((null argvars)
                  (values (rlets) (lets) (dynamic-extent-names) nil rtype nil 0 #|wrong|#))
              (let* ((name (car argvars))
                     (spec (car argspecs))
                     (argtype (parse-foreign-type spec)))
                (if (typep argtype 'foreign-record-type)
                  (setq argtype (parse-foreign-type :address)))
                (let* ((access-form
                        `(,(cond
                            ((typep argtype 'foreign-single-float-type)
                             (setq nextoffset (+ offset 4))
                             '%get-single-float-from-double-ptr)
                            ((typep argtype 'foreign-double-float-type)
                             (when (logtest offset 4)
                               (incf offset 4))
                             (setq nextoffset (+ offset 8))
                             '%get-double-float)
                            ((and (typep argtype 'foreign-integer-type)
                                  (= (foreign-integer-type-bits argtype) 64)
                                  (foreign-integer-type-signed argtype))
                             (when (logtest offset 4)
                               (incf offset 4))
                             (setq nextoffset (+ offset 8))
                             '%%get-signed-longlong)
                            ((and (typep argtype 'foreign-integer-type)
                                  (= (foreign-integer-type-bits argtype) 64)
                                  (not (foreign-integer-type-signed argtype)))
                             (when (logtest offset 4)
                               (incf offset 4))
                             (setq nextoffset (+ offset 8))
                             '%%get-unsigned-longlong)
                            (t
                             (setq nextoffset (+ offset 4))
                             (cond ((typep argtype 'foreign-pointer-type) '%get-ptr)
                                   ((typep argtype 'foreign-integer-type)
                                    (let* ((bits (foreign-integer-type-bits argtype))
                                           (signed (foreign-integer-type-signed argtype)))
                                      (cond ((<= bits 8)
                                             (if signed
                                               '%get-signed-byte
                                               '%get-unsigned-byte))
                                            ((<= bits 16)
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
                          ,stack-ptr
                          ,offset)))
                  (when name (lets (list name access-form)))
                  (setq offset nextoffset))))))))

(defun arm-linux::generate-callback-return-value (stack-ptr fp-args-ptr result return-type struct-return-arg)
  (declare (ignore fp-args-ptr))
  (unless (eq return-type *void-foreign-type*)
    (let* ((return-type-keyword
            (if (typep return-type 'foreign-record-type)
              (progn
                (setq result `(%%get-unsigned-longlong ,struct-return-arg 0))
                :unsigned-doubleword)
              (foreign-type-to-representation-type return-type)))
           (offset -8))
      `(setf (,
              (case return-type-keyword
                (:address '%get-ptr)
                (:signed-doubleword '%%get-signed-longlong)
                (:unsigned-doubleword '%%get-unsigned-longlong)
                ((:double-float :single-float) '%get-double-float)
                (t '%get-long)) ,stack-ptr ,offset) ,result))))
      
                 
