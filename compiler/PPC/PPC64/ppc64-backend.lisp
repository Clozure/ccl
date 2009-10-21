;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2004, 2005 Clozure Associates
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
  (require "BACKEND"))

(eval-when (:compile-toplevel :execute)
  (require "NXENV")
  (require "PPCENV"))


;;; Callbacks.  Both LinuxPPC64 and DarwinPPC64 follow something
;;; close to the PowerOpen ABI.  LinuxPPC uses transition vectors
;;; and a TOC, but it's not clear that we need to care about that
;;; here.
(defun define-ppc64-poweropen-callback (name args body env)
  (let* ((stack-word (gensym))
         (stack-ptr (gensym))
         (fp-arg-regs (gensym))
         (fp-arg-num 0)
         (arg-names ())
         (arg-types ())
         (return-type :void)
         (args args)
         (woi nil)
	 (monitor nil)
         (dynamic-extent-names ())
         (error-return nil))
    (loop
      (when (null args) (return))
      (when (null (cdr args))
        (setq return-type (car args))
        (return))
      (if (eq (car args) :without-interrupts)
        (setq woi (cadr args) args (cddr args))
	(if (eq (car args) :monitor-exception-ports)
	  (setq monitor (cadr args) args (cddr args))
          (if (eq (car args) :error-return)
            (setq error-return
                  (cadr args)                  
                  args (cddr args))
            (progn
              (push (foreign-type-to-representation-type (pop args)) arg-types)
              (push (pop args) arg-names))))))
    (setq arg-names (nreverse arg-names)
          arg-types (nreverse arg-types))
    (setq return-type (foreign-type-to-representation-type return-type))
    (when (eq return-type :void)
      (setq return-type nil))
    (let* ((offset 0)
           (need-stack-pointer (or arg-names return-type error-return))
           (lets
             (mapcar
	      #'(lambda (name type)
		  (let* ((delta 8)
			 (bias 0)
                         (use-fp-args nil))
		    (prog1
			(list name
			      `(,
				(if (typep type 'unsigned-byte)
				  (progn (setq delta (* 8 type)) '%inc-ptr)
				  (ecase type
				    (:single-float
                                     (if (< (incf fp-arg-num) 14)
                                       (progn
                                         (setq use-fp-args t)
                                         '%get-single-float-from-double-ptr)
                                       (progn
                                         (setq bias 4)
                                         '%get-single-float)))
				    (:double-float
                                     (setq delta 8)
                                     (if (< (incf fp-arg-num) 14)
                                       (setq use-fp-args t))
                                     '%get-double-float)
				    (:signed-doubleword (setq delta 8) '%%get-signed-longlong)
				    (:signed-fullword
                                     (setq bias 4)
                                     '%get-signed-long)
				    (:signed-halfword (setq bias 6)
                                                      '%get-signed-word)
				    (:signed-byte (setq bias 7)
                                                  '%get-signed-byte)
				    (:unsigned-doubleword (setq delta 8) '%%get-unsigned-longlong)
				    (:unsigned-fullword
                                     (setq bias 4)
                                     '%get-unsigned-long)
				    (:unsigned-halfword
                                     (setq bias 6)
                                     '%get-unsigned-word)
				    (:unsigned-byte
                                     (setq bias 7)
                                     '%get-unsigned-byte)
				    (:address '%get-ptr)))
				,(if use-fp-args fp-arg-regs stack-ptr)
				,(if use-fp-args (* 8 (1- fp-arg-num))
                                     `(+ ,offset ,bias))))
		      (when (or (eq type :address)
				(typep type 'unsigned-byte))
			(push name dynamic-extent-names))
		      (incf offset delta))))
	      arg-names arg-types)))
      (multiple-value-bind (body decls doc) (parse-body body env t)
        `(progn
           (declaim (special ,name))
           (define-callback-function
             (nfunction ,name
                        (lambda (,stack-word)
                          (declare (ignorable ,stack-word))
                          (block ,name
                            (with-macptrs (,@(and need-stack-pointer (list `(,stack-ptr))))
                              ,(when need-stack-pointer
                                 `(%setf-macptr-to-object ,stack-ptr ,stack-word))
                              ,(defcallback-body  stack-ptr lets dynamic-extent-names
                                                 decls body return-type error-return
                                                 (- ppc64::c-frame.savelr ppc64::c-frame.param0)
                                                 fp-arg-regs
                                                 )))))
             ,doc
             ,woi
	     ,monitor))))))

(defun defcallback-body-ppc64-poweropen (stack-ptr lets dynamic-extent-names decls body return-type error-return error-delta  fp-arg-ptr)
  (let* ((result (gensym))
         (result-ptr (case return-type
                   ((:single-float :double-float) fp-arg-ptr)
                   (t stack-ptr)))
         (condition-name (if (atom error-return) 'error (car error-return)))
         (error-return-function (if (atom error-return) error-return (cadr error-return)))
         (body
   	  `(with-macptrs ((,fp-arg-ptr (%get-ptr ,stack-ptr (- ppc64::c-frame.unused-1 ppc64::c-frame.param0))))
            (declare (ignorable ,fp-arg-ptr))
            (let ,lets
              (declare (dynamic-extent ,@dynamic-extent-names))
              ,@decls

              (let ((,result (progn ,@body)))
                (declare (ignorable ,result))
                ,@(progn
                   ;; Coerce SINGLE-FLOAT result to DOUBLE-FLOAT
                   (when (eq return-type :single-float)
                     (setq result `(float ,result 0.0d0)))
                   nil)

                ,(when return-type
                       `(setf (,
                               (case return-type
                                 (:address '%get-ptr)
                                 (:signed-doubleword '%%get-signed-longlong)
                                 (:unsigned-doubleword '%%get-unsigned-longlong)
                                 ((:double-float :single-float) '%get-double-float)
                                 (t '%%get-signed-longlong )) ,result-ptr 0) ,result)))))))
    (if error-return
      (let* ((cond (gensym)))
        `(handler-case ,body
          (,condition-name (,cond) (,error-return-function ,cond ,stack-ptr (%inc-ptr ,stack-ptr ,error-delta)))))
      body)))

(defvar *ppc64-vinsn-templates* (make-hash-table :test #'eq))



(defvar *known-ppc64-backends* ())


#+linuxppc-target
(defvar *linuxppc64-backend*
  (make-backend :lookup-opcode #'lookup-ppc-opcode
		:lookup-macro #'ppc::ppc-macro-function
		:lap-opcodes ppc::*ppc-opcodes*
                :define-vinsn 'define-ppc-vinsn
                :platform-syscall-mask (logior platform-os-linux platform-cpu-ppc)                
                
		:p2-dispatch *ppc2-specials*
		:p2-vinsn-templates *ppc64-vinsn-templates*
		:p2-template-hash-name '*ppc64-vinsn-templates*
		:p2-compile 'ppc2-compile
		:target-specific-features
		'(:powerpc :ppc-target :poweropen-target :linux-target :linuxppc-target :ppc64-target :64-bit-target :big-endian-target)
		:target-fasl-pathname (make-pathname :type "p64fsl")
		:target-platform (logior platform-cpu-ppc
                                         platform-os-linux
                                         platform-word-size-64)
		:target-os :linuxppc
		:name :linuxppc64
		:target-arch-name :ppc64
		:target-foreign-type-data nil
                :target-arch ppc64::*ppc64-target-arch*
                :define-callback 'define-ppc64-poweropen-callback
                :defcallback-body 'defcallback-body-ppc64-poweropen
                ))


#+darwinppc-target
(defvar *darwinppc64-backend*
  (make-backend :lookup-opcode #'lookup-ppc-opcode
		:lookup-macro #'ppc::ppc-macro-function
		:lap-opcodes ppc::*ppc-opcodes*
                :define-vinsn 'define-ppc-vinsn
                :platform-syscall-mask (logior platform-os-darwin platform-cpu-ppc)                
                
		:p2-dispatch *ppc2-specials*
		:p2-vinsn-templates *ppc64-vinsn-templates*
		:p2-template-hash-name '*ppc64-vinsn-templates*
		:p2-compile 'ppc2-compile
		:target-specific-features
		'(:powerpc :ppc-target :darwin-target :darwinppc-target :ppc64-target :64-bit-target :big-endian-target)
		:target-fasl-pathname (make-pathname :type "d64fsl")
		:target-platform (logior platform-cpu-ppc
                                         platform-os-darwin
                                         platform-word-size-64)
		:target-os :darwinppc
		:name :darwinppc64
		:target-arch-name :ppc64
		:target-foreign-type-data nil
                :target-arch ppc64::*ppc64-target-arch*
                :define-callback 'define-ppc64-poweropen-callback
                :defcallback-body 'defcallback-body-ppc64-poweropen))

#+linuxppc-target
(pushnew *linuxppc64-backend* *known-ppc64-backends* :key #'backend-name)


#+darwinppc-target
(pushnew *darwinppc64-backend* *known-ppc64-backends* :key #'backend-name)

(defvar *ppc64-backend* (car *known-ppc64-backends*))

(defun fixup-ppc64-backend ()
  (dolist (b *known-ppc64-backends*)
    (setf (backend-lap-opcodes b) ppc::*ppc-opcodes*
	  (backend-p2-dispatch b) *ppc2-specials*
	  (backend-p2-vinsn-templates b)  *ppc64-vinsn-templates*)
    (or (backend-lap-macros b) (setf (backend-lap-macros b)
                                     (make-hash-table :test #'equalp)))))



(fixup-ppc64-backend)

#+ppc64-target
(setq *host-backend* *ppc64-backend* *target-backend* *ppc64-backend*)
#-ppc64-target
(unless (backend-target-foreign-type-data *ppc64-backend*)
  (let* ((ftd (make-ftd
               :interface-db-directory
               #+darwinppc-target "ccl:darwin-headers64;"
               #+linuxppc-target "ccl:headers64;"
               :interface-package-name
               #+darwinppc-target "DARWIN64"
               #+linuxppc-target "LINUX64"
               :attributes
               #+darwinppc-target
               '(:signed-char t
                 :struct-by-value t
                 :struct-return-in-registers t
                 :struct-return-explicit t
                 :struct-by-value-by-field t
                 :prepend-underscores t
                 :bits-per-word  64)
               #+linuxppc-target
               '(:bits-per-word  64)
               :ff-call-expand-function
               #+linuxppc-target
               'linux64::expand-ff-call
               #+darwinppc-target
               'darwin64::expand-ff-call
               :ff-call-struct-return-by-implicit-arg-function
               #+linuxppc-target
               linux64::record-type-returns-structure-as-first-arg
               #+darwinppc-target
               darwin64::record-type-returns-structure-as-first-arg
               :callback-bindings-function
               #+linuxppc-target
               linux64::generate-callback-bindings
               #+darwinppc-target
               darwin64::generate-callback-bindings
               :callback-return-value-function
               #+linuxppc-target
               linux64::generate-callback-return-value
               #+darwinppc-target
               darwin64::generate-callback-return-value
               )))
    (install-standard-foreign-types ftd)
    (use-interface-dir :libc ftd)
    (setf (backend-target-foreign-type-data *ppc64-backend*) ftd)))
  
(pushnew *ppc64-backend* *known-backends* :key #'backend-name)

#+ppc64-target
(require "PPC64-VINSNS")

(provide "PPC64-BACKEND")
