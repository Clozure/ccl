;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
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

;; prepare-mcl-environment.lisp
;; Load this into a PPCCL to make it into an MCL-PPC for shipping
;; Sort of.

(in-package "CCL")

(defun %reset-outermost-binding (symbol value)
  (let* ((symvector (symptr->symvector symbol))
         (idx (%svref symvector target::symbol.binding-index-cell))
         (marker (%no-thread-local-binding-marker)))
    (if (> idx 0)
      (do-db-links (db var)
        (when (eq var idx)
          (let* ((oldval (%fixnum-ref db (* 2 target::node-size))))
            (unless (eq oldval marker)
              (setf (%fixnum-ref db (* 2 target::node-size)) value))))))
    (setf (uvref symvector target::symbol.vcell-cell) value)))

(defun freeze-current-definitions ()
  ;; Set the frozen bits so that redefine-kernel-function
  ;; will error if a builtin function is redefined.
  (do-all-symbols (s)
    (when (fboundp s)
      (%symbol-bits s (bitset $sym_fbit_frozen (%symbol-bits s)))))
  ;; Force an error if a kernel method is redefined.
  (make-all-methods-kernel))

(defun thaw-current-definitions ()
  ;; Clear the frozen bits on all fboundp symbols
  (do-all-symbols (s)
    (when (fboundp s)
      (%symbol-bits s (bitclr $sym_fbit_frozen (%symbol-bits s)))))
  ;; Allow redefinition of kernel methods.
  (make-all-methods-non-kernel))

(defun set-user-environment (&optional (freeze-definitions nil))
  "Arrange that the outermost special bindings of *PACKAGE* and
*WARN-IF-REDEFINE-KERNEL* restore values of the CL-USER package and T
respectively. If the optional argument is true, marks all globally
defined functions and methods as being predefined (this is a fairly
expensive operation.)"
  (when freeze-definitions
    (freeze-current-definitions))
  ;; enable redefine-kernel-function's error checking
  (%reset-outermost-binding '*warn-if-redefine-kernel* t)
  ;; Set the top-level *package* to the CL-USER package
  (%reset-outermost-binding '*package* (find-package "CL-USER")))

(defun set-development-environment (&optional (thaw-definitions nil))
  "Arrange that the outermost special bindings of *PACKAGE* and
*WARN-IF-REDEFINE-KERNEL* restore values of the CCL package and NIL
respectively. If the optional argument is true, mark all globally
defined functions and methods as being not predefined (this is a
fairly expensive operation.)"
  (when thaw-definitions
    (thaw-current-definitions))
  ;; enable redefine-kernel-function's error checking
  (%reset-outermost-binding '*warn-if-redefine-kernel* nil)
  ;; Set the top-level *package* to the CCL package
  (%reset-outermost-binding '*package* (find-package "CCL")))
  


(defmacro in-development-mode (&body body)
  `(let* ((*package* (find-package "CCL"))
	  (*warn-if-redefine-kernel* nil))
    ,@body))




