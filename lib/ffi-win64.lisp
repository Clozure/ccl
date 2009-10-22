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

;;; Win64:
;;; Structures are never actually passed by value; the caller
;;; instead passes a pointer to the structure or a copy of it.
;;; 
(defun win64::record-type-returns-structure-as-first-arg (rtype)
  (when (and rtype
             (not (typep rtype 'unsigned-byte))
             (not (member rtype *foreign-representation-type-keywords*
                          :test #'eq)))
    (let* ((ftype (if (typep rtype 'foreign-type)
                    rtype
                    (parse-foreign-type rtype))))
      (and (typep ftype 'foreign-record-type)
           (> (ensure-foreign-type-bits ftype) 64)))))


(defun win64::expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
  (let* ((result-type-spec (or (car (last args)) :void))
         (enclosing-form nil)
         (result-form nil))
    (multiple-value-bind (result-type error)
        (ignore-errors (parse-foreign-type result-type-spec))
      (if error
        (setq result-type-spec :void result-type *void-foreign-type*)
        (setq args (butlast args)))
      (collect ((argforms))
        (when (eq (car args) :monitor-exception-ports)
          (argforms (pop args)))
        (when (typep result-type 'foreign-record-type)
          (setq result-form (pop args))
          (if (win64::record-type-returns-structure-as-first-arg result-type)
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
;;;  to STACK-PTR.
;;; The byte offset of the foreign return address, relative to STACK-PTR
(defun win64::generate-callback-bindings (stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name)
  (declare (ignore fp-args-ptr))
  (collect ((lets)
            (rlets)
            (inits))
    (let* ((rtype (parse-foreign-type result-spec)))
      (when (typep rtype 'foreign-record-type)
        (if (win64::record-type-returns-structure-as-first-arg rtype)
          (setq argvars (cons struct-result-name argvars)
                argspecs (cons :address argspecs)
                rtype :address)
          (rlets (list struct-result-name (foreign-record-type-name rtype)))))
      (do* ((argvars argvars (cdr argvars))
            (argspecs argspecs (cdr argspecs))
            (arg-num 0)
            (gpr-arg-offset -8)
            (fpr-arg-offset -40)
            (memory-arg-offset 48)
            (fp nil nil))
           ((null argvars)
            (values (rlets) (lets) nil (inits) rtype nil 8))
        (flet ((next-gpr ()
                 (if (<= (incf arg-num) 4)
                   (prog1
                       gpr-arg-offset
                     (decf gpr-arg-offset 8)
                     (decf fpr-arg-offset 8))
                   (prog1
                       memory-arg-offset
                     (incf memory-arg-offset 8))))
               (next-fpr ()
                 (if (<= (incf arg-num) 4)
                   (prog1
                       fpr-arg-offset
                     (decf fpr-arg-offset 8)
                     (decf gpr-arg-offset 8))
                   (prog1
                       memory-arg-offset
                     (incf memory-arg-offset 8)))))
          (let* ((name (car argvars))
                 (spec (car argspecs))
                 (argtype (parse-foreign-type spec)))
            (if (typep argtype 'foreign-record-type)
              (setq argtype :address))
            (let* ((access-form
                    `(,
                          (ecase (foreign-type-to-representation-type argtype)
                            (:single-float (setq fp t) '%get-single-float)
                            (:double-float (setq fp t) '%get-double-float)
                            (:signed-doubleword  '%%get-signed-longlong)
                            (:signed-fullword '%get-signed-long)
                            (:signed-halfword '%get-signed-word)
                            (:signed-byte '%get-signed-byte)
                            (:unsigned-doubleword '%%get-unsigned-longlong)
                            (:unsigned-fullword '%get-unsigned-long)
                            (:unsigned-halfword '%get-unsigned-word)
                            (:unsigned-byte '%get-unsigned-byte)
                            (:address
                             #+nil
                             (dynamic-extent-names name)
                             '%get-ptr))
                          ,stack-ptr
                          ,(if fp (next-fpr) (next-gpr)))))
              (when name (lets (list name access-form))))))))))

(defun win64::generate-callback-return-value (stack-ptr fp-args-ptr result return-type struct-return-arg)
  (declare (ignore fp-args-ptr))
  (unless (eq return-type *void-foreign-type*)
    (when (typep return-type 'foreign-single-float-type)
      (setq result `(float ,result 0.0d0)))    
    (let* ((return-type-keyword
            (if (typep return-type 'foreign-record-type)
              (progn
                (setq result `(%%get-unsigned-longlong ,struct-return-arg 0))
                :unsigned-doubleword)
              (foreign-type-to-representation-type return-type)))
           (offset (case return-type-keyword
                   ((:single-float :double-float)
                    -24)
                   (t -8))))
      `(setf (,
              (case return-type-keyword
                (:address '%get-ptr)
                (:signed-doubleword '%%get-signed-longlong)
                (:unsigned-doubleword '%%get-unsigned-longlong)
                ((:double-float :single-float) '%get-double-float)
                (t '%get-long)) ,stack-ptr ,offset) ,result))))
      
                 
