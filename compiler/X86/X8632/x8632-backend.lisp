;;;-*- Mode: Lisp; Package: CCL -*-

;;; Copyright 2009 Clozure Associates
;;; This file is part of Clozure CL.
;;;
;;; Clozure CL is licensed under the terms of the Lisp Lesser GNU
;;; Public License , known as the LLGPL and distributed with Clozure
;;; CL as the file "LICENSE".  The LLGPL consists of a preamble and
;;; the LGPL, which is distributed with Clozure CL as the file "LGPL".
;;; Where these conflict, the preamble takes precedence.
;;;
;;; Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;; The LLGPL is also available online at
;;; http://opensource.franz.com/preamble.html

(in-package "CCL")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "BACKEND"))

(eval-when (:compile-toplevel :execute)
  (require "NXENV")
  (require "X8632ENV"))

(defvar *x8632-vinsn-templates* (make-hash-table :test #'eq))

(defvar *known-x8632-backends* ())

#+darwinx86-target
(defvar *darwinx8632-backend*
  (make-backend :lookup-opcode 'lookup-x86-opcode
                :lookup-macro #'false
                :lap-opcodes x86::*x86-opcode-templates*
                :define-vinsn 'define-x86-vinsn
                :p2-dispatch *x862-specials*
                :p2-vinsn-templates *x8632-vinsn-templates*
                :p2-template-hash-name '*x8632-vinsn-templates*
                :p2-compile 'x862-compile
                :platform-syscall-mask (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) 
                :target-specific-features
                '(:x8632 :x86-target :darwin-target :darwinx86-target :x8632-target
                  :darwinx8632-target
                  :little-endian-target
                  :32-bit-target)
                :target-fasl-pathname (make-pathname :type "dx32fsl")
                :target-platform (logior platform-cpu-x86
                                         platform-os-darwin
                                         platform-word-size-32)
                :target-os :darwinx86
                :name :darwinx8632
                :target-arch-name :x8632
                :target-foreign-type-data nil
                :target-arch x8632::*x8632-target-arch*
                :lisp-context-register x8632::fs
		:num-arg-regs 2
                ))


#+darwinx86-target
(pushnew *darwinx8632-backend* *known-x8632-backends* :key #'backend-name)

#+linuxx86-target
(defvar *linuxx8632-backend*
  (make-backend :lookup-opcode 'lookup-x86-opcode
                :lookup-macro #'false
                :lap-opcodes x86::*x86-opcode-templates*
                :define-vinsn 'define-x86-vinsn
                :p2-dispatch *x862-specials*
                :p2-vinsn-templates *x8632-vinsn-templates*
                :p2-template-hash-name '*x8632-vinsn-templates*
                :p2-compile 'x862-compile
                :platform-syscall-mask (logior platform-os-linux platform-cpu-x86 platform-word-size-32) 
                :target-specific-features
                '(:x8632 :x86-target :linux-target :linuxx86-target :x8632-target
                  :linuxx8632-target
                  :little-endian-target
                  :32-bit-target)
                :target-fasl-pathname (make-pathname :type "lx32fsl")
                :target-platform (logior platform-cpu-x86
                                         platform-os-linux
                                         platform-word-size-32)
                :target-os :linuxx86
                :name :linuxx8632
                :target-arch-name :x8632
                :target-foreign-type-data nil
                :target-arch x8632::*x8632-target-arch*
                :lisp-context-register x8632::fs
		:num-arg-regs 2
                ))

#+linuxx86-target
(pushnew *linuxx8632-backend* *known-x8632-backends* :key #'backend-name)

#+windows-target
(defvar *win32-backend*
  (make-backend :lookup-opcode 'lookup-x86-opcode
                :lookup-macro #'false
                :lap-opcodes x86::*x86-opcode-templates*
                :define-vinsn 'define-x86-vinsn
                :p2-dispatch *x862-specials*
                :p2-vinsn-templates *x8632-vinsn-templates*
                :p2-template-hash-name '*x8632-vinsn-templates*
                :p2-compile 'x862-compile
                :platform-syscall-mask (logior platform-os-windows platform-cpu-x86 platform-word-size-32) 
                :target-specific-features
                '(:x8632 :x86-target :windows-target :win32-target :x8632-target
                  :windowsx8632-target
                  :little-endian-target
                  :32-bit-target)
                :target-fasl-pathname (make-pathname :type "wx32fsl")
                :target-platform (logior platform-cpu-x86
                                         platform-os-windows
                                         platform-word-size-32)
                :target-os :win32
                :name :win32
                :target-arch-name :x8632
                :target-foreign-type-data nil
                :target-arch x8632::*x8632-target-arch*
                :lisp-context-register x8632::es
		:num-arg-regs 2
                ))

#+windows-target
(pushnew *win32-backend* *known-x8632-backends* :key #'backend-name)

#+solaris-target
(defvar *solaris-x8632-backend*
  (make-backend :lookup-opcode 'lookup-x86-opcode
                :lookup-macro #'false
                :lap-opcodes x86::*x86-opcode-templates*
                :define-vinsn 'define-x86-vinsn
                :p2-dispatch *x862-specials*
                :p2-vinsn-templates *x8632-vinsn-templates*
                :p2-template-hash-name '*x8632-vinsn-templates*
                :p2-compile 'x862-compile
                :platform-syscall-mask (logior platform-os-solaris platform-cpu-x86 platform-word-size-32) 
                :target-specific-features
                '(:x8632 :x86-target :solaris-target :x8632-target
                  :solarisx8632-target
                  :little-endian-target
                  :32-bit-target)
                :target-fasl-pathname (make-pathname :type "sx32fsl")
                :target-platform (logior platform-cpu-x86
                                         platform-os-solaris
                                         platform-word-size-32)
                :target-os :solarisx8632
                :name :solarisx8632
                :target-arch-name :x8632
                :target-foreign-type-data nil
                :target-arch x8632::*x8632-target-arch*
                :lisp-context-register x8632::fs
		:num-arg-regs 2
                ))
#+solaris-target
(pushnew *solaris-x8632-backend* *known-x8632-backends* :key #'backend-name)

#+freebsd-target
(defvar *freebsd-x8632-backend*
  (make-backend :lookup-opcode 'lookup-x86-opcode
                :lookup-macro #'false
                :lap-opcodes x86::*x86-opcode-templates*
                :define-vinsn 'define-x86-vinsn
                :p2-dispatch *x862-specials*
                :p2-vinsn-templates *x8632-vinsn-templates*
                :p2-template-hash-name '*x8632-vinsn-templates*
                :p2-compile 'x862-compile
                :platform-syscall-mask (logior platform-os-freebsd platform-cpu-x86 platform-word-size-32) 
                :target-specific-features
                '(:x8632 :x86-target :freebsd-target :x8632-target
                  :freebsdsx8632-target
                  :little-endian-target
                  :32-bit-target)
                :target-fasl-pathname (make-pathname :type "fx32fsl")
                :target-platform (logior platform-cpu-x86
                                         platform-os-freebsd
                                         platform-word-size-32)
                :target-os :freebsdx8632
                :name :freebsdx8632
                :target-arch-name :x8632
                :target-foreign-type-data nil
                :target-arch x8632::*x8632-target-arch*
                :lisp-context-register x8632::fs
		:num-arg-regs 2
                ))

#+freebsd-target
(pushnew *freebsd-x8632-backend* *known-x8632-backends* :key #'backend-name)

(defvar *x8632-backend* (car *known-x8632-backends*))

(defun fixup-x8632-backend ()
  (dolist (b *known-x8632-backends*)
    (setf #| (backend-lap-opcodes b) x86::*x86-opcodes* |#
          (backend-p2-dispatch b) *x862-specials*
          (backend-p2-vinsn-templates b)  *x8632-vinsn-templates*)
    (or (backend-lap-macros b) (setf (backend-lap-macros b)
                                     (make-hash-table :test #'equalp)))))


(fixup-x8632-backend)

#+x8632-target
(setq *host-backend* *x8632-backend* *target-backend* *x8632-backend*)


(defun setup-x8632-ftd (backend)
  (or (backend-target-foreign-type-data backend)
      (let* ((name (backend-name backend))
             (ftd
              (case name
                (:darwinx8632
                 (make-ftd :interface-db-directory "ccl:darwin-x86-headers;"
			   :interface-package-name "X86-DARWIN32"
                           :attributes '(:bits-per-word  32
                                         :signed-char t
                                         :struct-by-value t
                                         :prepend-underscore t)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "X86-DARWIN32")
			   :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "X86-DARWIN32")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "X86-DARWIN32")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "X86-DARWIN32")))
                (:linuxx8632
                 (make-ftd :interface-db-directory "ccl:x86-headers;"
			   :interface-package-name "X86-LINUX32"
                           :attributes '(:bits-per-word  32
                                         :signed-char nil
                                         :struct-by-value t
                                         :float-results-in-x87 t)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "X86-LINUX32")
			   :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "X86-LINUX32")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "X86-LINUX32")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "X86-LINUX32")))
                (:win32
                 (make-ftd :interface-db-directory "ccl:win32-headers;"
			   :interface-package-name "WIN32"
                           :attributes '(:bits-per-word  32
                                         :signed-char nil
                                         :struct-by-value t
                                         :float-results-in-x87 t)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "WIN32")
			   :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "WIN32")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "WIN32")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "WIN32")))
                (:solarisx8632
                 (make-ftd :interface-db-directory "ccl:solarisx86-headers;"
			   :interface-package-name "X86-SOLARIS32"
                           :attributes '(:bits-per-word  32
                                         :signed-char nil
                                         :struct-by-value t
                                         :float-results-in-x87 t)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "X86-SOLARIS32")
			   :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "X86-SOLARIS32")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "X86-SOLARIS32")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "X86-SOLARIS32")))
                (:freebsdx8632
                 (make-ftd :interface-db-directory "ccl:freebsd-headers;"
			   :interface-package-name "X86-FREEBSD32"
                           :attributes '(:bits-per-word  32
                                         :signed-char nil
                                         :struct-by-value t
                                         :float-results-in-x87 t)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "X86-FREEBSD32")
			   :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "X86-FREEBSD32")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "X86-FREEBSD32")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "X86-FREEBSD32")))
                )))
        (install-standard-foreign-types ftd)
        (use-interface-dir :libc ftd)
        (setf (backend-target-foreign-type-data backend) ftd))))

#-x8632-target
(setup-x8632-ftd *x8632-backend*)

(pushnew *x8632-backend* *known-backends* :key #'backend-name)

;;; FFI stuff.  The vanilla i386 ABI always returns structures as a
;;; hidden first argument.  Some systems (Darwin, FreeBSD) use a
;;; variant that returns small (<= 64 bit) structures in registers.

;;; A returned structure is passed as a hidden first argument.
(defun x8632::record-type-returns-structure-as-first-arg (rtype)
  (declare (ignore rtype))
  t)

;;; All arguments are passed on the stack.
(defun x8632::expand-ff-call (callform args
			      &key (arg-coerce #'null-coerce-foreign-arg)
			      (result-coerce #'null-coerce-foreign-result))
  (let* ((result-type-spec (or (car (last args)) :void))
	 (struct-by-value-p nil)
	 (result-op nil)
	 (result-temp nil)
	 (result-form nil))
    (multiple-value-bind (result-type error)
	(ignore-errors (parse-foreign-type result-type-spec))
      (if error
	(setq result-type-spec :void result-type *void-foreign-type*)
	(setq args (butlast args)))
      (collect ((argforms))
	(when (typep result-type 'foreign-record-type)
	  (setq result-form (pop args))
	  (if (funcall (ftd-ff-call-struct-return-by-implicit-arg-function
			*target-ftd*) result-type)
	    (progn
	      (setq result-type *void-foreign-type*
		    result-type-spec :void)
	      (argforms :address)
	      (argforms result-form))
	    (progn
	      (ecase (foreign-type-bits result-type)
		(8 (setq result-type-spec :unsigned-byte
			 result-op '%get-unsigned-byte))
		(16 (setq result-type-spec :unsigned-halfword
			  result-op '%get-unsigned-word))
		(32 (setq result-type-spec :unsigned-fullword
			  result-op '%get-unsigned-long))
		(64 (setq result-type-spec :unsigned-doubleword
			  result-op '%%get-unsigned-longlong)))
	      (setq result-type (parse-foreign-type result-type-spec))
	      (setq result-temp (gensym))
	      (setq struct-by-value-p t))))
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
			   (eq (foreign-record-type-kind ftype)
			       :transparent-union))
		  (ensure-foreign-type-bits ftype)
		  (setq ftype (foreign-record-field-type
			       (car (foreign-record-type-fields ftype)))
			arg-type-spec (foreign-type-to-representation-type
				       ftype)
			bits (ensure-foreign-type-bits ftype)))
		(if (typep ftype 'foreign-record-type)
		  (argforms (ceiling bits 32))
		  (argforms (foreign-type-to-representation-type ftype)))
		(argforms (funcall arg-coerce arg-type-spec arg-value-form))))))
	(argforms (foreign-type-to-representation-type result-type))
	(let* ((call (funcall result-coerce result-type-spec
			      `(,@callform ,@(argforms)))))
	  (if struct-by-value-p
	    `(let* ((,result-temp (%null-ptr)))
	       (declare (dynamic-extent ,result-temp)
			(type macptr ,result-temp))
	       (%setf-macptr ,result-temp ,result-form)
	       (setf (,result-op ,result-temp 0)
		     ,call))
	    call))))))

;;; Return 8 values:
;;; A list of RLET bindings
;;; A list of LET* bindings
;;; A list of DYNAMIC-EXTENT declarations for the LET* bindings
;;; A list of initializaton forms for (some) structure args (not used on x8632)
;;; A FOREIGN-TYPE representing the "actual" return type.
;;; A form which can be used to initialize FP-ARGS-PTR, relative
;;;  to STACK-PTR.  (not used on x8632)
;;; The byte offset of the foreign return address, relative to STACK-PTR
;;; The number of argument bytes pushed on the stack by the caller, or NIL
;;;  if this can't be determined. (Only meaningful on Windows.)

(defun x8632::generate-callback-bindings (stack-ptr fp-args-ptr argvars
                                                    argspecs result-spec
                                                    struct-result-name)
  (declare (ignore fp-args-ptr))
  (collect ((lets)
	    (rlets)
	    (dynamic-extent-names))
    (let* ((rtype (parse-foreign-type result-spec)))
      (when (typep rtype 'foreign-record-type)
	(if (funcall (ftd-ff-call-struct-return-by-implicit-arg-function
		      *target-ftd*) rtype)
	  (setq argvars (cons struct-result-name argvars)
		argspecs (cons :address argspecs)
		rtype *void-foreign-type*)
	  (rlets (list struct-result-name (foreign-record-type-name rtype)))))
      (do* ((argvars argvars (cdr argvars))
	    (argspecs argspecs (cdr argspecs))
	    (offset 8))
	   ((null argvars)
	    (values (rlets) (lets) (dynamic-extent-names) nil rtype nil 4
		    (- offset 8)))
	(let* ((name (car argvars))
	       (spec (car argspecs))
	       (argtype (parse-foreign-type spec))
	       (bits (require-foreign-type-bits argtype))
	       (double nil))
	  (if (typep argtype 'foreign-record-type)
            (let* ((form `(%inc-ptr ,stack-ptr
                           ,(prog1 offset
                                   (incf offset
                                         (* 4 (ceiling bits 32)))))))
              (when name (lets (list name form))))
	    (let* ((form `(,
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
	      (when name (lets (list name form)))
	      (incf offset 4)
	      (when double (incf offset 4)))))))))

(defun x8632::generate-callback-return-value (stack-ptr fp-args-ptr result
					      return-type struct-return-arg)
  (declare (ignore fp-args-ptr))
  (unless (eq return-type *void-foreign-type*)
    (if (typep return-type 'foreign-record-type)
      ;; If the struct result is returned via a hidden argument, the
      ;; return type would have been mapped to :VOID.  On some
      ;; systems, small (<= 64 bits) structs are returned by value,
      ;; which we arrange to retrieve here.
      (ecase (ensure-foreign-type-bits return-type)
	(8 `(setf (%get-unsigned-byte ,stack-ptr -8)
		  (%get-unsigned-byte ,struct-return-arg 0)))
	(16 `(setf (%get-unsigned-word ,stack-ptr -8)
		   (%get-unsigned-word ,struct-return-arg 0)))
	(32 `(setf (%get-unsigned-long ,stack-ptr -8)
		   (%get-unsigned-long ,struct-return-arg 0)))
	(64 `(setf (%%get-unsigned-longlong ,stack-ptr -8)
	       (%%get-unsigned-longlong ,struct-return-arg 0))))
      (let* ((return-type-keyword (foreign-type-to-representation-type
				   return-type)))
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



#+x8632-target
(require "X8632-VINSNS")

(provide "X8632-BACKEND")

