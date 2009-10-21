;;;-*- Mode: Lisp; Package: CCL -*-
;;;
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

(eval-when (:compile-toplevel)
  (require "NUMBER-MACROS"))

(defun coerce-to-complex-type (num type)
  (cond ((complexp num)
         (let ((real (%realpart num))
               (imag (%imagpart num)))
           (if (and (typep real type)
                    (typep imag type))
             num
             (complex (coerce real type)
                      (coerce imag type)))))
        (t (complex (coerce num type)))))

;;; end of l0-complex.lisp
