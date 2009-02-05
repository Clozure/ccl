;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2005 Clozure Associates
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "BACKEND"))

(eval-when (:compile-toplevel :execute)
  (require "NXENV")
  (require "X8664ENV"))


(defvar *x8664-vinsn-templates* (make-hash-table :test #'eq))



(defvar *known-x8664-backends* ())


#+(or linuxx86-target (not x86-target))
(defvar *linuxx8664-backend*
  (make-backend :lookup-opcode 'lookup-x86-opcode
                :lookup-macro #'false
                :lap-opcodes x86::*x86-opcode-templates*
                :define-vinsn 'define-x86-vinsn
                :platform-syscall-mask (logior platform-os-linux platform-cpu-x86 platform-word-size-64) 
		:p2-dispatch *x862-specials*
		:p2-vinsn-templates *x8664-vinsn-templates*
		:p2-template-hash-name '*x8664-vinsn-templates*
		:p2-compile 'x862-compile
		:target-specific-features
		'(:x8664 :x86-target :linux-target :linuxx86-target :x8664-target
                  :linuxx8664-target
                  :little-endian-target
                  :64-bit-target)
		:target-fasl-pathname (make-pathname :type "lx64fsl")
		:target-platform (logior platform-cpu-x86
                                         platform-os-linux
                                         platform-word-size-64)
		:target-os :linuxx86
		:name :linuxx8664
		:target-arch-name :x8664
		:target-foreign-type-data nil


                :target-arch x8664::*x8664-target-arch*
                :lisp-context-register x8664::gs
                ))


#+darwinx86-target
(defvar *darwinx8664-backend*
  (make-backend :lookup-opcode 'lookup-x86-opcode
		:lookup-macro #'false
                :lap-opcodes x86::*x86-opcode-templates*
                :define-vinsn 'define-x86-vinsn
		:p2-dispatch *x862-specials*
		:p2-vinsn-templates *x8664-vinsn-templates*
		:p2-template-hash-name '*x8664-vinsn-templates*
		:p2-compile 'x862-compile
                :platform-syscall-mask (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) 
		:target-specific-features
		'(:x8664 :x86-target :darwin-target :darwinx86-target :x8664-target
                  :darwinx8664-target
                  :little-endian-target
                  :64-bit-target)
		:target-fasl-pathname (make-pathname :type "dx64fsl")
		:target-platform (logior platform-cpu-x86
                                         platform-os-darwin
                                         platform-word-size-64)
		:target-os :darwinx86
		:name :darwinx8664
		:target-arch-name :x8664
		:target-foreign-type-data nil
                :target-arch x8664::*x8664-target-arch*
                ;; Overload %gs until Apple straightens things out.
                :lisp-context-register x8664::gs
                ))

#+freebsdx86-target
(defvar *freebsdx8664-backend*
  (make-backend :lookup-opcode 'lookup-x86-opcode
		:lookup-macro #'false
                :lap-opcodes x86::*x86-opcode-templates*
                :define-vinsn 'define-x86-vinsn
		:p2-dispatch *x862-specials*
		:p2-vinsn-templates *x8664-vinsn-templates*
		:p2-template-hash-name '*x8664-vinsn-templates*
		:p2-compile 'x862-compile
		:target-specific-features
		'(:x8664 :x86-target :freebsd-target :freebsdx86-target :x8664-target
                  :freebsdx8664-target                  
                  :little-endian-target
                  :64-bit-target)
		:target-fasl-pathname (make-pathname :type "fx64fsl")
		:target-platform (logior platform-cpu-x86
                                         platform-os-freebsd
                                         platform-word-size-64)
		:target-os :freebsdx86
		:name :freebsdx8664
		:target-arch-name :x8664
		:target-foreign-type-data nil
                :target-arch x8664::*x8664-target-arch*
                :platform-syscall-mask (logior platform-os-freebsd platform-cpu-x86 platform-word-size-64)
                :lisp-context-register x8664::gs
                ))

#+solarisx86-target
(defvar *solarisx8664-backend*
  (make-backend :lookup-opcode 'lookup-x86-opcode
		:lookup-macro #'false
                :lap-opcodes x86::*x86-opcode-templates*
                :define-vinsn 'define-x86-vinsn
		:p2-dispatch *x862-specials*
		:p2-vinsn-templates *x8664-vinsn-templates*
		:p2-template-hash-name '*x8664-vinsn-templates*
		:p2-compile 'x862-compile
		:target-specific-features
		'(:x8664 :x86-target :solaris-target :solarisx86-target :x8664-target
                  :solarisx8664-target
                  :solarisx64-target
                  :little-endian-target
                  :64-bit-target)
		:target-fasl-pathname (make-pathname :type "sx64fsl")
		:target-platform (logior platform-cpu-x86
                                         platform-os-solaris
                                         platform-word-size-64)
		:target-os :solarisx86
		:name :solarisx8664
		:target-arch-name :x8664
		:target-foreign-type-data nil
                :target-arch x8664::*x8664-target-arch*
                :platform-syscall-mask (logior platform-os-solaris platform-cpu-x86 platform-word-size-64)
                :lisp-context-register x8664::gs
                ))

#+win64-target
(defvar *win64-backend*
  (make-backend :lookup-opcode 'lookup-x86-opcode
		:lookup-macro #'false
                :lap-opcodes x86::*x86-opcode-templates*
                :define-vinsn 'define-x86-vinsn
		:p2-dispatch *x862-specials*
		:p2-vinsn-templates *x8664-vinsn-templates*
		:p2-template-hash-name '*x8664-vinsn-templates*
		:p2-compile 'x862-compile
		:target-specific-features
		'(:x8664 :x86-target :win64-target :windows-target :x8664-target
                  :winx64-target                  
                  :little-endian-target
                  :64-bit-target)
		:target-fasl-pathname (make-pathname :type "wx64fsl")
		:target-platform (logior platform-cpu-x86
                                         platform-os-windows
                                         platform-word-size-64)
		:target-os :win64
		:name :win64
		:target-arch-name :x8664
		:target-foreign-type-data nil
                :target-arch x8664::*x8664-target-arch*
                :platform-syscall-mask (logior platform-os-windows platform-cpu-x86 platform-word-size-64)
                :lisp-context-register x8664::r11
                ))

#+(or linuxx86-target (not x86-target))
(pushnew *linuxx8664-backend* *known-x8664-backends* :key #'backend-name)


#+darwinx86-target
(pushnew *darwinx8664-backend* *known-x8664-backends* :key #'backend-name)

#+freebsdx86-target
(pushnew *freebsdx8664-backend* *known-x8664-backends* :key #'backend-name)

#+solarisx86-target
(pushnew *solarisx8664-backend* *known-x8664-backends* :key #'backend-name)

#+win64-target
(pushnew *win64-backend* *known-x8664-backends* :key #'backend-name)

(defvar *x8664-backend* (car *known-x8664-backends*))

(defun fixup-x8664-backend ()
  (dolist (b *known-x8664-backends*)
    (setf #| (backend-lap-opcodes b) x86::*x86-opcodes* |#
	  (backend-p2-dispatch b) *x862-specials*
	  (backend-p2-vinsn-templates b)  *x8664-vinsn-templates*)
    (or (backend-lap-macros b) (setf (backend-lap-macros b)
                                     (make-hash-table :test #'equalp)))))



(fixup-x8664-backend)

#+x8664-target
(setq *host-backend* *x8664-backend* *target-backend* *x8664-backend*)

(defun setup-x8664-ftd (backend)
  (or (backend-target-foreign-type-data backend)
      (let* ((name (backend-name backend))
             (ftd
              (case name
                (:linuxx8664
                 (make-ftd :interface-db-directory
                           (if (eq backend *host-backend*)
                             "ccl:x86-headers64;"
                             "ccl:cross-x86-headers64;")
                           :interface-package-name "X86-LINUX64"
                           :attributes '(:bits-per-word  64
                                         :struct-by-value t)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "X86-LINUX64")
                           :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "X86-LINUX64")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "X86-LINUX64")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "X86-LINUX64")))
                (:darwinx8664
                 (make-ftd :interface-db-directory
                           (if (eq backend *host-backend*)
                             "ccl:darwin-x86-headers64;"
                             "ccl:cross-darwin-x86-headers64;")
                           :interface-package-name "X86-DARWIN64"
                           :attributes '(:bits-per-word  64
                                         :signed-char t
                                         :struct-by-value t
                                         :prepend-underscore t)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "X86-DARWIN64")
                           :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "X86-DARWIN64")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "X86-DARWIN64")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "X86-DARWIN64")))
                (:freebsdx8664
                 (make-ftd :interface-db-directory
                           (if (eq backend *host-backend*)
                             "ccl:freebsd-headers64;"
                             "ccl:cross-freebsd-headers64;")
                           :interface-package-name "X86-FREEBSD64"
                           :attributes '(:bits-per-word  64
                                         :struct-by-value t)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "X86-FREEBSD64")
                           :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "X86-FREEBSD64")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "X86-FREEBSD64")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "X86-FREEBSD64")))
                (:solarisx8664
                 (make-ftd :interface-db-directory
                           (if (eq backend *host-backend*)
                             "ccl:solarisx64-headers;"
                             "ccl:cross-solarisx64-headers;")
                           :interface-package-name "X86-SOLARIS64"
                           :attributes '(:bits-per-word  64
                                         :struct-by-value t)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "X86-SOLARIS64")
                           :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "X86-SOLARIS64")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "X86-SOLARIS64")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "X86-SOLARIS64")))
                (:win64
                 (make-ftd :interface-db-directory
                           (if (eq backend *host-backend*)
                             "ccl:win64-headers;"
                             "ccl:cross-win64-headers;")
                           :interface-package-name "WIN64"
                           :attributes '(:bits-per-word  64
                                         :struct-by-value t
                                         :bits-per-long 32)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "WIN64")
                           :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "WIN64")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "WIN64")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "WIN64"))))))
        (install-standard-foreign-types ftd)
        (use-interface-dir :libc ftd)
        (setf (backend-target-foreign-type-data backend) ftd))))

#-x8664-target
(setup-x8664-ftd *x8664-backend*)

(pushnew *x8664-backend* *known-backends* :key #'backend-name)

;;; FFI stuff.  Seems to be shared by Darwin/Linux/FreeBSD.

;;; A returned structure is passed as an invisible first argument if
;;; it's more than 2 doublewords long or if it contains unaligned fields.
;;; Not clear how the latter case can happen, so this just checks for
;;; the first.
(defun x8664::record-type-returns-structure-as-first-arg (rtype)
  (when (and rtype
             (not (typep rtype 'unsigned-byte))
             (not (member rtype *foreign-representation-type-keywords*
                          :test #'eq)))
    (let* ((ftype (if (typep rtype 'foreign-type)
                    rtype
                    (parse-foreign-type rtype))))
      (> (ensure-foreign-type-bits ftype) 128))))

;;; On x8664, structures can be passed by value:
;;;  a) in memory, if they're more than 128 bits in size or if there aren't
;;;     enough of the right kind of register to pass them entirely in registers.
;;;  b) as a series of 64-bit chunks, passed in GPRs if any component of the
;;;     chunk is a non FLOAT or in FPRs otherwise.
;;; Note that this means that a chunk consisting of two SINGLE-FLOATs would
;;; be passed in the low 64 bit of an SSE (xmm) register.

(defun x8664::field-is-of-class-integer (field)
  ;; Return true if field is of "class" integer or if it's a record
  ;; type of class integer.  (See the System V AMD64 ABI document for
  ;; a convoluted definition of field "classes".)
  (let* ((ftype (foreign-record-field-type field)))
    (typecase ftype
      ((or foreign-integer-type foreign-pointer-type) t)
      (foreign-record-type (dolist (f (foreign-record-type-fields ftype))
                             (when (x8664::field-is-of-class-integer f)
                               (return t))))
      (otherwise nil))))

(defun x8664::classify-8byte (field-list bit-limit)
  ;; CDR down the fields in FIELD-LIST until we find a field of class integer,
  ;; hit the end of the list, or find a field whose offset is >= BIT-LIMIT.
  ;; In the first case, return :INTEGER.  In other cases, return :FLOAT.
  (dolist (field field-list :float)
    (if (<= bit-limit (foreign-record-field-offset field))
      (return :float)
      (if (x8664::field-is-of-class-integer field)
        (return :integer)))))

;;; Return a first value :memory, :integer, or::float and a second
;;; value of NIL, :integer, or :float according to how the structure
;;; RTYPE should ideally be passed or returned.  Note that the caller
;;; may decide to turn this to :memory if there aren't enough
;;; available registers of the right class when passing an instance of
;;; this structure type.
(defun x8664::classify-record-type (rtype)
  (let* ((nbits (ensure-foreign-type-bits rtype))
         (fields (foreign-record-type-fields rtype)))
    (cond ((> nbits 128) (values :memory nil))
          ((<= nbits 64) (values (x8664::classify-8byte fields 64) nil))
          (t (values (x8664::classify-8byte fields 64)
               (do* ()
                    ((>= (foreign-record-field-offset (car fields)) 64)
                     (x8664::classify-8byte fields 128))
                 (setq fields (cdr fields))))))))

(defun x8664::struct-from-regbuf-values (r rtype regbuf)
  (multiple-value-bind (first second)
      (x8664::classify-record-type rtype)
    (let* ((gpr-offset 0)
           (fpr-offset 16))
      ;; Do this 32 bits at a time, to avoid consing.
      (collect ((forms))
        (case first
          (:integer (forms `(setf (%get-unsigned-long ,r 0)
                             (%get-unsigned-long ,regbuf 0)))
                    (forms `(setf (%get-unsigned-long ,r 4)
                             (%get-unsigned-long ,regbuf 4)))
                    (setq gpr-offset 8))
          (:float (forms `(setf (%get-unsigned-long ,r 0)
                             (%get-unsigned-long ,regbuf 16)))
                  (forms `(setf (%get-unsigned-long ,r 4)
                             (%get-unsigned-long ,regbuf 20)))
                  (setf fpr-offset 24)))
        (case second
          (:integer (forms `(setf (%get-unsigned-long ,r 8)
                             (%get-unsigned-long ,regbuf ,gpr-offset)))
                    (forms `(setf (%get-unsigned-long ,r 12)
                             (%get-unsigned-long ,regbuf ,(+ gpr-offset 4)))))
          (:float (forms `(setf (%get-unsigned-long ,r 8)
                             (%get-unsigned-long ,regbuf ,fpr-offset)))
                  (forms `(setf (%get-unsigned-long ,r 12)
                             (%get-unsigned-long ,regbuf ,(+ fpr-offset 4))))))
        `(progn ,@(forms))))))

(defun x8664::expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
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
          (if (x8664::record-type-returns-structure-as-first-arg struct-result-type)
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
            (do* ((args args (cddr args))
                  (remaining-gprs 6)
                  (remaining-fprs 8))
                 ((null args))
              (let* ((arg-type-spec (car args))
                     (arg-value-form (cadr args)))
                (if (or (member arg-type-spec *foreign-representation-type-keywords*
                                :test #'eq)
                        (typep arg-type-spec 'unsigned-byte))
                  (progn
                    (if (or (eq arg-type-spec :double-float)
                            (eq arg-type-spec :single-float))
                      (decf remaining-fprs)
                      (unless (typep arg-type-spec 'unsigned-byte)
                        (decf remaining-gprs)))
                    (argforms arg-type-spec)
                    (argforms arg-value-form))
                  (let* ((ftype (parse-foreign-type arg-type-spec)))
                    (when (and (typep ftype 'foreign-record-type)
                             (eq (foreign-record-type-kind ftype) :transparent-union))
                      (ensure-foreign-type-bits ftype)
                      (setq ftype (foreign-record-field-type
                                   (car (foreign-record-type-fields ftype)))
                            arg-type-spec (foreign-type-to-representation-type ftype)))
                    (if (typep ftype 'foreign-record-type)
                      (multiple-value-bind (first8 second8)
                          (x8664::classify-record-type ftype)
                        (let* ((gprs remaining-gprs)
                               (fprs remaining-fprs))
                          (case first8
                            (:integer (if (< (decf gprs) 0) (setq first8 :memory)))
                            (:float (if (< (decf fprs) 0) (setq first8 :memory))))
                          (case second8
                            (:integer (if (< (decf gprs) 0) (setq first8 :memory)))
                            (:float (if (< (decf fprs) 0) (setq first8 :memory)))))
                        (if (eq first8 :memory)
                          (progn
                            (argforms (ceiling (foreign-record-type-bits ftype) 64))
                            (argforms arg-value-form))
                          (progn
                            (if second8
                              (progn
                                (unless structure-arg-temp
                                  (setq structure-arg-temp (gensym)))
                                (setq valform `(%setf-macptr ,structure-arg-temp ,arg-value-form)))
                              (setq valform arg-value-form))
                            (if (eq first8 :float)
                              (progn
                                (decf remaining-fprs)
                                (argforms :double-float)
                                (argforms `(%get-double-float ,valform 0)))
                              (progn
                                (decf remaining-gprs)
                                (argforms :unsigned-doubleword)
                                (argforms `(%%get-unsigned-longlong ,valform 0))))
                            (when second8
                              (setq valform structure-arg-temp)
                              (if (eq second8 :float)
                                (progn
                                (decf remaining-fprs)
                                (argforms :double-float)
                                (argforms `(%get-double-float ,valform 8)))
                              (progn
                                (decf remaining-gprs)
                                (argforms :unsigned-doubleword)
                                (argforms `(%%get-unsigned-longlong ,valform 8))))))))
                      (let* ((rtype (foreign-type-to-representation-type ftype)))
                        (if (or (eq rtype :singlefloat) (eq rtype :double-float))
                          (decf remaining-fprs)
                          (decf remaining-gprs))
                        (argforms rtype)
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
                  (%stack-block ((,regbuf (+ (* 2 8) (* 2 8))))
                    ,call
                    ,(x8664::struct-from-regbuf-values result-temp struct-result-type regbuf)))
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

(defun x8664::generate-callback-bindings (stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name)
  (declare (ignore fp-args-ptr))
  (collect ((lets)
            (rlets)
            (inits)
            (dynamic-extent-names))
    (let* ((rtype (parse-foreign-type result-spec)))
      (when (typep rtype 'foreign-record-type)
        (if (x8664::record-type-returns-structure-as-first-arg rtype)
          (setq argvars (cons struct-result-name argvars)
                argspecs (cons :address argspecs)
                rtype *void-foreign-type*)
          (rlets (list struct-result-name (foreign-record-type-name rtype)))))
      (do* ((argvars argvars (cdr argvars))
            (argspecs argspecs (cdr argspecs))
            (gpr-arg-num 0)
            (gpr-arg-offset -8)
            (fpr-arg-num 0)
            (fpr-arg-offset -56)
            (memory-arg-offset 16)
            (fp nil nil))
           ((null argvars)
            (values (rlets) (lets) (dynamic-extent-names) (inits) rtype nil 8))
        (flet ((next-gpr ()
                 (if (<= (incf gpr-arg-num) 6)
                   (prog1
                       gpr-arg-offset
                     (decf gpr-arg-offset 8))
                   (prog1
                       memory-arg-offset
                     (incf memory-arg-offset 8))))
               (next-fpr ()
                 (if (<= (incf fpr-arg-num) 8)
                   (prog1
                       fpr-arg-offset
                     (decf fpr-arg-offset 8))
                   (prog1
                       memory-arg-offset
                     (incf memory-arg-offset 8)))))
          (let* ((name (car argvars))
                 (spec (car argspecs))
                 (argtype (parse-foreign-type spec))
                 (bits (require-foreign-type-bits argtype)))
            (if (typep argtype 'foreign-record-type)
              (multiple-value-bind (first8 second8)
                  (x8664::classify-record-type argtype)
                (let* ((gprs (- 6 gpr-arg-num))
                       (fprs (- 8 fpr-arg-num)))
                  (case first8
                    (:integer (if (< (decf gprs) 0) (setq first8 :memory)))
                    (:float (if (< (decf fprs) 0) (setq first8 :memory))))
                  (case second8
                    (:integer (if (< (decf gprs) 0) (setq first8 :memory)))
                    (:float (if (< (decf fprs) 0) (setq first8 :memory)))))
                (if (eq first8 :memory)
                  (progn
                    (lets (list name `(%inc-ptr ,stack-ptr ,(prog1 memory-arg-offset
                                                                   (incf memory-arg-offset (* 8 (ceiling bits 64)))))))
                         (dynamic-extent-names name))
                  (progn
                    (rlets (list name (foreign-record-type-name argtype)))
                    (inits `(setf (%%get-unsigned-longlong ,name 0)
                             (%%get-unsigned-longlong ,stack-ptr ,(if (eq first8 :integer) (next-gpr) (next-fpr)))))
                    (if second8
                      (inits `(setf (%%get-unsigned-longlong ,name 8)
                             (%%get-unsigned-longlong ,stack-ptr ,(if (eq second8 :integer) (next-gpr) (next-fpr)))))))))
                (lets (list name
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
                             ,(if fp (next-fpr) (next-gpr))))))))))))

(defun x8664::generate-callback-return-value (stack-ptr fp-args-ptr result return-type struct-return-arg)
  (declare (ignore fp-args-ptr))
  (unless (eq return-type *void-foreign-type*)
    (let* ((gpr-offset -8)
           (fpr-offset -24))
      (if (typep return-type 'foreign-record-type)
      ;;; Would have been mapped to :VOID unless record-type was <= 128 bits.
        (collect ((forms))
          (multiple-value-bind (first8 second8)
              (x8664::classify-record-type return-type)
            (forms `(setf (%%get-signed-longlong ,stack-ptr ,(if (eq first8 :integer) gpr-offset fpr-offset))
                     (%%get-signed-longlong ,struct-return-arg 0)))
            (when second8
              (if (eq first8 :integer) (decf gpr-offset 8) (decf fpr-offset 8))
              (forms `(setf (%%get-signed-longlong ,stack-ptr ,(if (eq first8 :integer) gpr-offset fpr-offset))
                       (%%get-signed-longlong ,struct-return-arg 8))))
            `(progn ,@(forms))))
        (let* ((return-type-keyword (foreign-type-to-representation-type return-type))
               (offset (case return-type-keyword
                         ((:single-float :double-float) fpr-offset)
                         (t gpr-offset))))
          `(setf (,
                  (case return-type-keyword
                    (:address '%get-ptr)
                    (:signed-doubleword '%%get-signed-longlong)
                    (:unsigned-doubleword '%%get-unsigned-longlong)
                    (:double-float '%get-double-float)
                    (:single-float '%get-single-float)
                    (:unsigned-fullword '%get-unsigned-long)
                    (t '%%get-signed-longlong )
                    ) ,stack-ptr ,offset) ,result))))))



#+x8664-target
(require "X8664-VINSNS")

(provide "X8664-BACKEND")
