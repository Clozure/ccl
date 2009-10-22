;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2009 Clozure Associates
;;;   Copyright (C) 1994-2001 Digitool, Inc
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
  (require "PPCENV")
  (require "PPC32-ARCH"))





(defvar *ppc32-vinsn-templates* (make-hash-table :test #'eq))




(defvar *known-ppc32-backends* ())


#+linuxppc-target
(defvar *linuxppc32-backend*
  (make-backend :lookup-opcode #'lookup-ppc-opcode
		:lookup-macro #'ppc::ppc-macro-function
		:lap-opcodes ppc::*ppc-opcodes*
                :define-vinsn 'define-ppc-vinsn
                :platform-syscall-mask (logior platform-os-linux platform-cpu-ppc)
		:p2-dispatch *ppc2-specials*
		:p2-vinsn-templates *ppc32-vinsn-templates*
		:p2-template-hash-name '*ppc32-vinsn-templates*
		:p2-compile 'ppc2-compile
		:target-specific-features
		'(:powerpc :ppc-target :eabi-target :linux-target :linuxppc-target :ppc32-target :32-bit-target :big-endian-target)
		:target-fasl-pathname (make-pathname :type "pfsl")
		:target-platform (logior platform-word-size-32
                                         platform-cpu-ppc
                                         platform-os-linux)
		:target-os :linuxppc
		:name :linuxppc32
		:target-arch-name :ppc32
		:target-foreign-type-data nil
                :target-arch ppc32::*ppc32-target-arch*))


#+darwinppc-target
(defvar *darwinppc32-backend*
  (make-backend :lookup-opcode #'lookup-ppc-opcode
		:lookup-macro #'ppc::ppc-macro-function
		:lap-opcodes ppc::*ppc-opcodes*
                :define-vinsn 'define-ppc-vinsn
                :platform-syscall-mask (logior platform-os-darwin platform-cpu-ppc)                
		:p2-dispatch *ppc2-specials*
		:p2-vinsn-templates *ppc32-vinsn-templates*
		:p2-template-hash-name '*ppc32-vinsn-templates*
		:p2-compile 'ppc2-compile
		:target-specific-features
		'(:powerpc :ppc-target :darwin-target :darwinppc-target :ppc32-target :32-bit-target :big-endian-target)
		:target-fasl-pathname (make-pathname :type "dfsl")
		:target-platform (logior platform-word-size-32
                                         platform-cpu-ppc
                                         platform-os-darwin)
		:target-os :darwinppc
		:name :darwinppc32
		:target-arch-name :ppc32
		:target-foreign-type-data nil
                :target-arch ppc32::*ppc32-target-arch*))

#+linuxppc-target
(pushnew *linuxppc32-backend* *known-ppc32-backends* :key #'backend-name)


#+darwinppc-target
(pushnew *darwinppc32-backend* *known-ppc32-backends* :key #'backend-name)

(defvar *ppc32-backend* (car *known-ppc32-backends*))

(defun fixup-ppc32-backend ()
  (dolist (b *known-ppc32-backends*)
    (setf (backend-lap-opcodes b) ppc::*ppc-opcodes*
	  (backend-p2-dispatch b) *ppc2-specials*
	  (backend-p2-vinsn-templates b)  *ppc32-vinsn-templates*)
    (or (backend-lap-macros b) (setf (backend-lap-macros b)
                                     (make-hash-table :test #'equalp)))))



(fixup-ppc32-backend)

#+ppc32-target
(setq *host-backend* *ppc32-backend* *target-backend* *ppc32-backend*)
#-ppc32-target
(unless (backend-target-foreign-type-data *ppc32-backend*)
  (let* ((ftd (make-ftd
               :interface-db-directory
               #+darwinppc-target "ccl:darwin-headers;"
               #+linuxppc-target "ccl:headers;"
               :interface-package-name
               #+darwinppc-target "DARWIN32"
               #+linuxppc-target "LINUX32"
               :attributes
               #+darwinppc-target
               '(:signed-char t
                 :struct-by-value t
                 :prepend-underscores t
                 :bits-per-word  32
                 :poweropen-alignment t)
               #+linuxppc-target
               '(:bits-per-word 32)
               :ff-call-expand-function
               #+linuxppc-target
               'linux32::expand-ff-call
               #+darwinppc-target
               'darwin32::expand-ff-call
               :ff-call-struct-return-by-implicit-arg-function
               #+linuxppc-target
               linux32::record-type-returns-structure-as-first-arg
               #+darwinppc-target
               darwin32::record-type-returns-structure-as-first-arg
               :callback-bindings-function
               #+linuxppc-target
               linux32::generate-callback-bindings
               #+darwinppc-target
               darwin32::generate-callback-bindings
               :callback-return-value-function
               #+linuxppc-target
               linux32::generate-callback-return-value
               #+darwinppc-target
               darwin32::generate-callback-return-value
               )))
    (install-standard-foreign-types ftd)
    (use-interface-dir :libc ftd)
    (setf (backend-target-foreign-type-data *ppc32-backend*) ftd)))

(pushnew *ppc32-backend* *known-backends* :key #'backend-name)

#+ppc32-target
(require "PPC32-VINSNS")
(provide "PPC32-BACKEND")
