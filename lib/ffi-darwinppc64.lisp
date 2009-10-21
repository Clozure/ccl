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

;;; On DarwinPPC64:
;;; Structures whose size is exactly 16 bytes are passed in 2 GPRs,
;;; regardless of the types of their elements, when they are passed
;;; by value.
;;; Structures which contain unions are passed in N GPRs when passed
;;; by value.
;;; All other structures passed by value are passed by passing their
;;; constituent elements as scalars.  (Sort of.)  GPR's are "consumed"
;;; for and possibly/partly loaded with the contents of each 64-bit
;;; word; FPRs (and vector registers) are consumed/loaded for each
;;; field of the indicated type.
;;; Structures whose size is exactly 16 bytes are returned in GPR3
;;; and GPR4.
;;; Structures which contain unions are "returned" by passing a pointer
;;; to a structure instance in the first argument.
;;; All other structures are returned by returning their constituent
;;; elements as scalars.  (Note that - in some cases - we may need
;;; to reserve space in the foreign stack frame to handle scalar
;;; return values that don't fit in registers.  Need a way to tell
;;; %ff-call about this, as well as runtime support.)


(defun darwin64::record-type-contains-union (rtype)
  ;;; RTYPE is a FOREIGN-RECORD-TYPE object.
  ;;; If it, any of its fields, or any fields in an
  ;;; embedded structure or array field is a union,
  ;;; return true.
  ;;; (If this function returns true, we can't
  ;;; pass a structure of type RTYPE - or return one -
  ;;; by passing or returning the values of all of
  ;;; its fields, since some fields are aliased.
  ;;; However, if the record's size is exactly 128
  ;;; bits, we can pass/return  it in two GPRs.)
  (ensure-foreign-type-bits rtype)
  (or (eq (foreign-record-type-kind rtype) :union)
      (dolist (f (foreign-record-type-fields rtype))
        (let* ((fieldtype (foreign-record-field-type f)))
          (if (and (typep fieldtype 'foreign-record-type)
                   (darwin64::record-type-contains-union fieldtype))
            (return t))
          (if (typep fieldtype 'foreign-array-type)
            (let* ((atype (foreign-array-type-element-type fieldtype)))
              (if (and (typep atype 'foreign-record-type)
                       (darwin64::record-type-contains-union atype))
                (return t))))))))

;;; On DarwinPPC64, we only have to pass a structure as a first
;;; argument if the type contains a union
(defun darwin64::record-type-returns-structure-as-first-arg (rtype)
  (when (and rtype
             (not (typep rtype 'unsigned-byte))
             (not (member rtype *foreign-representation-type-keywords*
                          :test #'eq)))
    (let* ((ftype (if (typep rtype 'foreign-type)
                    rtype
                    (parse-foreign-type rtype))))
      (and (typep ftype 'foreign-record-type)
           (not (= (ensure-foreign-type-bits ftype) 128))
           (darwin64::record-type-contains-union ftype)))))





;;; Generate code to set the fields in a structure R of record-type
;;; RTYPE, based on the register values in REGBUF (8 64-bit GPRs,
;;; followed by 13 64-bit GPRs.)
;;; This also handles the 16-byte structure case.
;;; (It doesn't yet handle embedded arrays or bitfields.)
(defun darwin64::struct-from-regbuf-values (r rtype regbuf)
  (let* ((bits (ccl::ensure-foreign-type-bits rtype)))
    (collect ((forms))
      (cond ((= bits 128)               ;(and (eql day 'tuesday) ...)
             (forms `(setf (ccl::%get-signed-long ,r 0)
                      (ccl::%get-signed-long ,regbuf 0)
                      (ccl::%get-signed-long ,r 4)
                      (ccl::%get-signed-long ,regbuf 4)
                      (ccl::%get-signed-long ,r 8)
                      (ccl::%get-signed-long ,regbuf 8)
                      (ccl::%get-signed-long ,r 12)
                      (ccl::%get-signed-long ,regbuf 12))))
            ;;; One (slightly naive) way to do this is to just
            ;;; copy GPRs into the structure until it's full,
            ;;; then go back and overwrite float-typed fields
            ;;; with FPRs.  That'd be very naive if all fields
            ;;; were float-typed, slightly naive if some fields
            ;;; were properly-aligned DOUBLE-FLOATs or if two
            ;;; SINGLE-FLOATs were packed inro a 64-bit word,
            ;;; and not that bad if a SINGLE-FLOAT shared a
            ;;; 64-bit word with a non-FP field.
            (t
             (let* ((fpr-offset (* 8 8))
                    (fields (foreign-record-type-fields rtype)))
               (flet ((next-fpr-offset ()
                        (prog1 fpr-offset
                          (incf fpr-offset 8))))
                 (unless (all-floats-in-field-list fields)
                   (do* ((b 0 (+ b 32))
                         (w 0 (+ w 4)))
                        ((>= b bits))
                     (declare (fixnum b w))
                     (forms `(setf (%get-unsigned-long ,r ,w)
                              (%get-unsigned-long ,regbuf ,w)))))
                 (when (some-floats-in-field-list fields)
                   (labels ((do-fp-fields (fields accessors)
                              (dolist (field fields)
                                (let* ((field-type (foreign-record-field-type field))
                                       (field-accessor-list (append accessors (list (foreign-record-field-name field))))
                                       (valform ()))
                                  (etypecase field-type
                                    (foreign-record-type
                                     (do-fp-fields (foreign-record-type-fields field-type)
                                       field-accessor-list))
                                    (foreign-double-float-type
                                     (setq valform
                                           `(%get-double-float  ,regbuf ,(next-fpr-offset))))
                                    (foreign-single-float-type
                                     (setq valform
                                           `(%get-single-float-from-double-ptr
                                             ,regbuf ,(next-fpr-offset))))
                                    (foreign-array-type
                                     (error "Embedded array-type."))
                                    )
                                  (when valform
                                    (forms `(setf ,(%foreign-access-form
                                                    r
                                                    rtype
                                                    0
                                                    field-accessor-list)
                                             ,valform)))))))
                     (do-fp-fields (foreign-record-type-fields rtype) nil )))))))
      `(progn ,@(forms) nil))))

;;; "Return" the structure R of foreign type RTYPE, by storing the
;;; values of its fields in STACK-PTR and FP-ARG-PTR
(defun darwin64::return-struct-to-registers (r rtype stack-ptr fp-args-ptr)
  (let* ((bits (require-foreign-type-bits rtype)))
    (collect ((forms))
      (cond ((= bits 128)               ;(and (eql day 'tuesday) ...)
             (forms `(setf (ccl::%get-unsigned-long ,stack-ptr 0)
                      (ccl::%get-unsigned-long ,r 0)
                      (ccl::%get-unsigned-long ,stack-ptr 4)
                      (ccl::%get-unsigned-long ,r 4)
                      (ccl::%get-unsigned-long ,stack-ptr 8)
                      (ccl::%get-unsigned-long ,r 8)
                      (ccl::%get-unsigned-long ,stack-ptr 12)
                      (ccl::%get-unsigned-long ,r 12))))
            (t
             (let* ((fpr-offset 0)
                    (fields (foreign-record-type-fields rtype)))
               (unless (all-floats-in-field-list fields)
                   (do* ((b 0 (+ b 32))
                         (w 0 (+ w 4)))
                        ((>= b bits))
                     (declare (fixnum b w))
                     (forms `(setf (%get-unsigned-long ,stack-ptr ,w)
                              (%get-unsigned-long ,r ,w)))))
               (when (some-floats-in-field-list fields)
               (flet ((next-fpr-offset ()
                        (prog1 fpr-offset
                          (incf fpr-offset 8))))
                 (labels ((do-fp-fields (fields accessors)
                            (dolist (field fields)
                              (let* ((field-type (foreign-record-field-type field))
                                     (field-accessor-list (append accessors (list (foreign-record-field-name field))))
                                     (valform ()))
                                (etypecase field-type
                                  (foreign-record-type
                                   (do-fp-fields (foreign-record-type-fields field-type)
                                     field-accessor-list))
                                  (foreign-double-float-type
                                   (setq valform
                                         `(%get-double-float  ,fp-args-ptr ,(next-fpr-offset))))
                                  (foreign-single-float-type
                                   (setq valform
                                         `(%get-double-float  ,fp-args-ptr ,(next-fpr-offset))))

                                  (foreign-array-type
                                   (error "Embedded array-type."))
                                  )
                                (when valform
                                  (let* ((field-form (%foreign-access-form
                                                      r
                                                      rtype
                                                      0
                                                      field-accessor-list)))
                                    (when (typep field-type 'foreign-single-float-type)
                                      (setq field-form `(float ,field-form 0.0d0)))
                                    (forms `(setf ,valform ,field-form))))))))
                   (do-fp-fields fields nil )))))))
      `(progn ,@(forms) nil))))

;;; Return an ordered list of all scalar fields in the record type FTYPE.
(defun darwin64::flatten-fields (ftype)
  (if (darwin64::record-type-contains-union ftype)
    (error "Can't flatten fields in ~s: contains union" ftype))
  (collect ((fields))
    (labels ((flatten (field-list bit-offset)
               (dolist (field field-list)
                 (let* ((field-type (foreign-record-field-type field))
                        (next-offset (+ bit-offset (foreign-record-field-offset field))))
                   (typecase field-type
                     (foreign-record-type
                      (flatten (foreign-record-type-fields field-type) next-offset))
                     (foreign-array-type
                      (let* ((element-type (foreign-array-type-element-type field-type))
                             (nbits (foreign-type-bits element-type))
                             (align (foreign-type-alignment  element-type))
                             (dims (foreign-array-type-dimensions field-type))
                             (n (or (and (null (cdr dims)) (car dims))
                                    (error "Can't handle multidimensional foreign arrays")))
                             (pos next-offset))
                        (dotimes (i n)
                          (fields (make-foreign-record-field :type element-type
                                                             :bits nbits
                                                             :offset pos))
                          (setq pos (align-offset (+ pos nbits) align)))))
                     (t
                      (fields (make-foreign-record-field :type field-type
                                                         :bits (foreign-record-field-bits field)
                                                         :offset next-offset))))))))
      (flatten (foreign-record-type-fields ftype) 0)
      (fields))))

               
             

(defun darwin64::expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
  (let* ((result-type-spec (or (car (last args)) :void))
         (regbuf nil)
         (result-temp nil)
         (result-form nil)
         (struct-result-type nil)
         (structure-arg-temp nil))
    (multiple-value-bind (result-type error)
        (ignore-errors (parse-foreign-type result-type-spec))
      (if error
        (setq result-type-spec :void result-type *void-foreign-type*)
        (setq args (butlast args)))
      (collect ((argforms))
        (when (eq (car args) :monitor-exception-ports)
          (argforms (pop args)))
        (when (typep result-type 'foreign-record-type)
          (setq result-form (pop args)
                struct-result-type result-type
                result-type *void-foreign-type*
                result-type-spec :void)
          (if (darwin64::record-type-returns-structure-as-first-arg struct-result-type)
            (progn
              (argforms :address)
              (argforms result-form))
            (progn
              (setq regbuf (gensym)
                    result-temp (gensym))
              (argforms :registers)
              (argforms regbuf))))
        (let* ((valform nil))
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
                (let* ((ftype (parse-foreign-type arg-type-spec))
                       (bits (foreign-type-bits ftype)))
                  (if (typep ftype 'foreign-record-type)
                    (if (or (darwin64::record-type-contains-union ftype)
                            (= bits 128))
                      (progn
                        (argforms (ceiling (foreign-record-type-bits ftype) 64))
                        (argforms arg-value-form))
                      (let* ((flattened-fields (darwin64::flatten-fields ftype)))

                        (flet ((single-float-at-offset (offset)
                                 (dolist (field flattened-fields)
                                   (let* ((field-offset (foreign-record-field-offset field)))
                                     (when (> field-offset offset)
                                       (return nil))
                                     (if (and (= field-offset offset)
                                              (typep (foreign-record-field-type field)
                                                     'foreign-single-float-type))
                                       (return t)))))
                               (double-float-at-offset (offset)
                                 (dolist (field flattened-fields)
                                   (let* ((field-offset (foreign-record-field-offset field)))
                                     (when (> field-offset offset)
                                       (return nil))
                                     (if (and (= field-offset offset)
                                              (typep (foreign-record-field-type field)
                                                     'foreign-double-float-type))
                                       (return t))))))
                        (unless structure-arg-temp
                          (setq structure-arg-temp (gensym)))
                        (setq valform `(%setf-macptr ,structure-arg-temp ,arg-value-form))
                        (do* ((bit-offset 0 (+ bit-offset 64))
                              (byte-offset 0 (+ byte-offset 8)))
                             ((>= bit-offset bits))
                          (if (double-float-at-offset bit-offset)
                            (progn
                              (argforms :double-float)
                              (argforms `(%get-double-float ,valform ,byte-offset)))
                            (let* ((high-single (single-float-at-offset bit-offset))
                                   (low-single (single-float-at-offset (+ bit-offset 32))))
                              (if high-single
                                (if low-single
                                  (argforms :hybrid-float-float)
                                  (argforms :hybrid-float-int))
                                (if low-single
                                  (argforms :hybrid-int-float)
                                  (argforms :unsigned-doubleword)))
                              (argforms `(%%get-unsigned-longlong ,valform ,byte-offset))))
                          (setq valform structure-arg-temp)))))
                    (progn
                      (argforms (foreign-type-to-representation-type ftype))
                      (argforms (funcall arg-coerce arg-type-spec arg-value-form))))))))
          (argforms (foreign-type-to-representation-type result-type))
          (let* ((call (funcall result-coerce result-type-spec `(,@callform ,@(argforms)))))
            (when structure-arg-temp
              (setq call `(let* ((,structure-arg-temp (%null-ptr)))
                           (declare (dynamic-extent ,structure-arg-temp)
                                    (type macptr ,structure-arg-temp))
                           ,call)))
            (if regbuf
              `(let* ((,result-temp (%null-ptr)))
                (declare (dynamic-extent ,result-temp)
                         (type macptr ,result-temp))
                (%setf-macptr ,result-temp ,result-form)
                (%stack-block ((,regbuf (+ (* 8 8) (* 8 13))))
                  ,call
                  ,(darwin64::struct-from-regbuf-values result-temp struct-result-type regbuf)))
              call)))))))
            
            
;;; Return 7 values:
;;; A list of RLET bindings
;;; A list of LET* bindings
;;; A list of DYNAMIC-EXTENT declarations for the LET* bindings
;;; A list of initializaton forms for (some) structure args
;;; A FOREIGN-TYPE representing the "actual" return type.
;;; A form which can be used to initialize FP-ARGS-PTR, relative
;;;  to STACK-PTR.  (This is unused on linuxppc32.)
;;; The byte offset of the foreign return address, relative to STACK-PTR

(defun darwin64::generate-callback-bindings (stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name)
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
          (if (darwin64::record-type-contains-union rtype)
            (setq argvars (cons struct-result-name argvars)
                  argspecs (cons :address argspecs)
                  rtype *void-foreign-type*)
            (rlets (list struct-result-name (or (foreign-record-type-name rtype)
                                                result-spec)))))
        (when (typep rtype 'foreign-float-type)
          (set-fp-regs-form))
        (do* ((argvars argvars (cdr argvars))
              (argspecs argspecs (cdr argspecs))
              (fp-arg-num 0)
              (offset 0)
              (delta 0)
              (bias 0)
              (use-fp-args nil nil))
             ((null argvars)
              (values (rlets) (lets) (dynamic-extent-names) (inits) rtype fp-regs-form (- ppc64::c-frame.savelr ppc64::c-frame.param0)))
          (flet ((next-scalar-arg (argtype)
                   (setq delta 8 bias 0)
                   (prog1
                       `(,(cond
                           ((typep argtype 'foreign-single-float-type)
                            (if (< (incf fp-arg-num) 14)
                              (progn
                                (setq use-fp-args t)
                                '%get-single-float-from-double-ptr)
                              (progn
                                '%get-single-float)))
                           ((typep argtype 'foreign-double-float-type)
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
                              (+ offset bias)))
                     (incf offset delta))))
            (let* ((name (car argvars))
                   (spec (car argspecs))
                   (argtype (parse-foreign-type spec))
                   (bits (foreign-type-bits argtype)))
              (if (typep argtype 'foreign-record-type)
                (if (or (darwin64::record-type-contains-union argtype)
                        (= bits 128))
                  (progn (setq delta (* (ceiling bits 64) 8))
                         (when name (lets (list name `(%inc-ptr ,stack-ptr ,offset ))))
                         (incf offset delta))

                  (let* ((flattened-fields (darwin64::flatten-fields argtype)))
                    (flet ((double-float-at-offset (offset)
                             (dolist (field flattened-fields)
                               (let* ((field-offset (foreign-record-field-offset field)))
                                 (when (> field-offset offset) (return))
                                 (if (and (= field-offset offset)
                                          (typep (foreign-record-field-type field)
                                                 'foreign-double-float-type))
                                   (return t)))))
                           (single-float-at-offset (offset)
                             (dolist (field flattened-fields)
                               (let* ((field-offset (foreign-record-field-offset field)))
                                 (when (> field-offset offset) (return))
                                 (if (and (= field-offset offset)
                                          (typep (foreign-record-field-type field)
                                                 'foreign-single-float-type))
                                   (return t))))))
                      (when name (rlets (list name (or (foreign-record-type-name argtype)
                                            spec))))
                      (do* ((bit-offset 0 (+ bit-offset 64))
                            (byte-offset 0 (+ byte-offset 8)))
                           ((>= bit-offset bits))
                        (if (double-float-at-offset bit-offset)
                          (let* ((init `(setf (%get-double-float ,name ,byte-offset)
                                   ,(next-scalar-arg (parse-foreign-type :double-float)))))
                            (when name
                              (inits init)))
                          (let* ((high-single (single-float-at-offset bit-offset))
                                 (low-single (single-float-at-offset (+ bit-offset 32)))
                                 (init `(setf (%%get-unsigned-longlong ,name ,byte-offset)
                                     ,(next-scalar-arg (parse-foreign-type '(:unsigned 64))))))
                            (when name (inits init))
                            (when high-single
                              (when (< (incf fp-arg-num) 14)
                                (set-fp-regs-form)
                                (when name
                                  (inits `(setf (%get-single-float ,name ,byte-offset)
                                         (%get-single-float-from-double-ptr
                                          ,fp-args-ptr
                                          ,(* 8 (1- fp-arg-num))))))))
                            (when low-single
                              (when (< (incf fp-arg-num) 14)
                                (set-fp-regs-form)
                                (when name
                                  (inits `(setf (%get-single-float ,name ,(+ 4 byte-offset))
                                         (%get-single-float-from-double-ptr
                                          ,fp-args-ptr
                                          ,(* 8 (1- fp-arg-num))))))))))))))
                (let* ((form (next-scalar-arg argtype)))
                  (when name 
                    (lets (list name form)))))
              #+nil
              (when (or (typep argtype 'foreign-pointer-type)
                        (typep argtype 'foreign-array-type))
                (dynamic-extent-names name))
              (when use-fp-args (set-fp-regs-form)))))))))

(defun darwin64::generate-callback-return-value (stack-ptr fp-args-ptr result return-type struct-return-arg)
  (unless (eq return-type *void-foreign-type*)
    (when (typep return-type 'foreign-single-float-type)
      (setq result `(float ,result 0.0d0)))    
    (if (typep return-type 'foreign-record-type)
      ;;; Would have been mapped to :VOID unless record-type contained
      ;;; a single scalar field.
      (darwin64::return-struct-to-registers struct-return-arg return-type stack-ptr fp-args-ptr)
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
                                 (t '%%get-signed-longlong )
                                 ) ,result-ptr 0) ,result)))))


