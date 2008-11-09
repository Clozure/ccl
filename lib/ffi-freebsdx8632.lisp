(in-package "CCL")

;;; Some small structures are returned in EAX and EDX.  Otherwise,
;;; return values are placed at the address specified by the caller.
(defun x86-freebsd32::record-type-returns-structure-as-first-arg (rtype)
  (when (and rtype
	     (not (typep rtype 'unsigned-byte))
	     (not (member rtype *foreign-representation-type-keywords*
			  :test #'eq)))
    (let* ((ftype (if (typep rtype 'foreign-type)
		    rtype
		    (parse-foreign-type rtype)))
	   (nbits (ensure-foreign-type-bits ftype)))
      (not (member nbits '(8 16 32 64))))))

(defun x86-freebsd32::struct-from-regbuf-values (r rtype regbuf)
  (ecase (ensure-foreign-type-bits rtype)
    (8 `(setf (%get-unsigned-byte ,r 0) (%get-unsigned-byte ,regbuf 0)))
    (16 `(setf (%get-unsigned-word ,r 0) (%get-unsigned-word ,regbuf 0)))
    (32 `(setf (%get-unsigned-long ,r 0) (%get-unsigned-long ,regbuf 0)))
    (64 `(setf (%%get-unsigned-longlong ,r 0)
	       (%%get-unsigned-longlong ,regbuf 0)))))

;;; All arguments are passed on the stack.
;;;
;;; (We don't support the __m64, __m128, __m128d, and __m128i types.)

(defun x86-freebsd32::expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
  (let* ((result-type-spec (or (car (last args)) :void))
	 (regbuf nil)
	 (result-temp nil)
	 (result-form nil)
	 (struct-result-type nil))
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
	  (if (x86-freebsd32::record-type-returns-structure-as-first-arg result-type)
	    (progn
	      (argforms :address)
	      (argforms result-form))
	    (progn
	      (setq regbuf (gensym)
		    result-temp (gensym))
	      (argforms :registers)
	      (argforms regbuf))))
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
                    (if (and (typep ftype 'foreign-record-type)
                             (<= bits 32))
                      (argforms (ceiling bits 32))
                      (argforms (foreign-type-to-representation-type ftype)))
		(argforms (funcall arg-coerce arg-type-spec arg-value-form))))))
	  (argforms (foreign-type-to-representation-type result-type))
	  (let* ((call (funcall result-coerce result-type-spec `(,@callform ,@(argforms)))))
	    (if regbuf
	      `(let* ((,result-temp (%null-ptr)))
		 (declare (dynamic-extent ,result-temp)
			  (type macptr ,result-temp))
		 (%setf-macptr ,result-temp ,result-form)
		 (%stack-block ((,regbuf 8))
		   ,call
		   ,(x86-freebsd32::struct-from-regbuf-values result-temp struct-result-type regbuf)))
	      call))))))

;;; Return 7 values:
;;; A list of RLET bindings
;;; A list of LET* bindings
;;; A list of DYNAMIC-EXTENT declarations for the LET* bindings
;;; A list of initializaton forms for (some) structure args
;;; A FOREIGN-TYPE representing the "actual" return type.
;;; A form which can be used to initialize FP-ARGS-PTR, relative
;;;  to STACK-PTR.  (This is unused on freebsdppc32.)
;;; The byte offset of the foreign return address, relative to STACK-PTR

(defun x86-freebsd32::generate-callback-bindings (stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name)
  (declare (ignore fp-args-ptr))
  (collect ((lets)
	    (rlets)
	    (inits)
	    (dynamic-extent-names))
    (let* ((rtype (parse-foreign-type result-spec)))
      (when (typep rtype 'foreign-record-type)
	(if (x86-freebsd32::record-type-returns-structure-as-first-arg rtype)
	  (setq argvars (cons struct-result-name argvars)
		argspecs (cons :address argspecs)
		rtype *void-foreign-type*)
	  (rlets (list struct-result-name (foreign-record-type-name rtype)))))
      (do* ((argvars argvars (cdr argvars))
	    (argspecs argspecs (cdr argspecs))
	    (offset 8 (incf offset 4)))
	   ((null argvars)
	    (values (rlets) (lets) (dynamic-extent-names) (inits) rtype nil 4))
	(let* ((name (car argvars))
	       (spec (car argspecs))
	       (argtype (parse-foreign-type spec))
	       (bits (require-foreign-type-bits argtype))
	       (double nil))
	  (if (typep argtype 'foreign-record-type)
	    (progn
	      (format t "~& arg is some foreign type"))
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
			  ,offset))))
	  (when double (incf offset 4)))))))

(defun x86-freebsd32::generate-callback-return-value (stack-ptr fp-args-ptr result return-type struct-return-arg)
  (declare (ignore fp-args-ptr))
  (format t "~&in generate-callback-return-value")
  (unless (eq return-type *void-foreign-type*)
    (if (typep return-type 'foreign-record-type)
      ;; Would have been mapped to :VOID unless record-type was <= 64 bits
      (format t "~&need to return structure ~s by value" return-type)
      (let* ((return-type-keyword (foreign-type-to-representation-type return-type)))
        (ccl::collect ((forms))
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

