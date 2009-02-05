;;;-*- Mode: Lisp; Package: CCL -*-
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

(in-package "CCL")

(defstruct subprimitive-info
  name
  offset
  nailed-down
  argument-mask
  registers-used
  )

(defmethod make-load-form ((s subprimitive-info) &optional env)
  (make-load-form-saving-slots s :environment env))

(defmethod print-object ((s subprimitive-info) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~A @ #x~x" 
            (subprimitive-info-name s)
            (subprimitive-info-offset s))))

(defun %subprim-name->offset (name table)
  (let* ((sprec (find name table 
                      :test #'string-equal 
                      :key #'subprimitive-info-name)))
    (if sprec
      (subprimitive-info-offset sprec)
      (error "subprim named ~s not found." name))))

(defun subprim-name->offset (name &optional (backend *target-backend*))
  (+ (backend-lowmem-bias backend)
     (%subprim-name->offset name  (arch::target-subprims-table
                                   (backend-target-arch backend)))))

(provide "SUBPRIMS")
