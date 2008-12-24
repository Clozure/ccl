(in-package "CCL")

;; Always use the "hidden first arg" convention on linuxx8632
(defun x86-linux32::record-type-returns-structure-as-first-arg (rtype)
  (declare (ignore rtype))
  t)

;;; All arguments are passed on the stack.
;;;
;;; (We don't support the __m64, __m128, __m128d, and __m128i types.)

(defun x86-linux32::expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
  (let* ((result-type-spec (or (car (last args)) :void))
	 (result-form nil))
    (multiple-value-bind (result-type error)
	(ignore-errors (parse-foreign-type result-type-spec))
      (if error
	(setq result-type-spec :void result-type *void-foreign-type*)
	(setq args (butlast args)))
      (collect ((argforms))
	(when (typep result-type 'foreign-record-type)
	  (setq result-form (pop args)
		result-type *void-foreign-type*
		result-type-spec :void)
	  (argforms :address)
	  (argforms result-form))
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
                     (bits (ensure-foreign-type-bits ftype)))
                    (when (and (typep ftype 'foreign-record-type)
                             (eq (foreign-record-type-kind ftype) :transparent-union))
                      (ensure-foreign-type-bits ftype)
                      (setq ftype (foreign-record-field-type
                                   (car (foreign-record-type-fields ftype)))
                            arg-type-spec (foreign-type-to-representation-type ftype)
                            bits (ensure-foreign-type-bits ftype)))
                    (if (typep ftype 'foreign-record-type)
                      (argforms (ceiling bits 32))
                      (argforms (foreign-type-to-representation-type ftype)))
		(argforms (funcall arg-coerce arg-type-spec arg-value-form))))))
	  (argforms (foreign-type-to-representation-type result-type))
	  (let* ((call (funcall result-coerce result-type-spec `(,@callform ,@(argforms)))))
	    call)))))

;;; Return 7 values:
;;; A list of RLET bindings
;;; A list of LET* bindings
;;; A list of DYNAMIC-EXTENT declarations for the LET* bindings
;;; A list of initializaton forms for (some) structure args (not used on x8632)
;;; A FOREIGN-TYPE representing the "actual" return type.
;;; A form which can be used to initialize FP-ARGS-PTR, relative
;;;  to STACK-PTR.  (This is unused on linuxppc32.)
;;; The byte offset of the foreign return address, relative to STACK-PTR

(defun x86-linux32::generate-callback-bindings (stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name)
  (declare (ignore fp-args-ptr))
  (collect ((lets)
	    (rlets)
	    (dynamic-extent-names))
    (let* ((rtype (parse-foreign-type result-spec)))
      (when (typep rtype 'foreign-record-type)
	(if (x86-linux32::record-type-returns-structure-as-first-arg rtype)
	  (setq argvars (cons struct-result-name argvars)
		argspecs (cons :address argspecs)
		rtype *void-foreign-type*)
	  (rlets (list struct-result-name (foreign-record-type-name rtype)))))
      (do* ((argvars argvars (cdr argvars))
	    (argspecs argspecs (cdr argspecs))
	    (offset 8))
	   ((null argvars)
	    (values (rlets) (lets) (dynamic-extent-names) nil rtype nil 4))
	(let* ((name (car argvars))
	       (spec (car argspecs))
	       (argtype (parse-foreign-type spec))
	       (bits (require-foreign-type-bits argtype))
	       (double nil))
	  (if (typep argtype 'foreign-record-type)
	    (lets (list name `(%inc-ptr ,stack-ptr ,(prog1 offset
							   (incf offset (* 4 (ceiling bits 32)))))))
	    (progn
	      (lets (list name
			  `(,
			    (ecase (foreign-type-to-representation-type argtype)
			      (:single-float '%get-single-float)
			      (:double-float (setq double t) '%get-double-float)
			      (:signed-doubleword (setq double t)
						  '%%get-signed-longlong)
			      (:signed-fullword '%get-signed-long)
			      (:signed-halfword '%get-signed-word)
			      (:signed-byte '%get-signed-byte)
			      (:unsigned-doubleword (setq double t)
						    '%%get-unsigned-longlong)
			      (:unsigned-fullword '%get-unsigned-long)
			      (:unsigned-halfword '%get-unsigned-word)
			      (:unsigned-byte '%get-unsigned-byte)
			      (:address '%get-ptr))
			    ,stack-ptr
			    ,offset)))
	      (incf offset 4)
	      (when double (incf offset 4)))))))))

(defun x86-linux32::generate-callback-return-value (stack-ptr fp-args-ptr result return-type struct-return-arg)
  (declare (ignore fp-args-ptr struct-return-arg))
  (unless (eq return-type *void-foreign-type*)
    (if (typep return-type 'foreign-record-type)
      ;; Should have been mapped to :VOID
      (error "Shouldn't be trying to return a structure by value on linuxx8632")
      (let* ((return-type-keyword (foreign-type-to-representation-type return-type)))
        (collect ((forms))
          (forms 'progn)
          (case return-type-keyword
            (:single-float
             (forms `(setf (%get-unsigned-byte ,stack-ptr -16) 1)))
            (:double-float
             (forms `(setf (%get-unsigned-byte ,stack-ptr -16) 2))))
          (forms
           `(setf (,
                   (case return-type-keyword
                     (:address '%get-ptr)
                     (:signed-doubleword '%%get-signed-longlong)
                     (:unsigned-doubleword '%%get-unsigned-longlong)
                     (:double-float '%get-double-float)
                     (:single-float '%get-single-float)
                     (:unsigned-fullword '%get-unsigned-long)
                     (t '%get-signed-long)
                     ) ,stack-ptr -8) ,result))
          (forms))))))

