;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2010 Clozure Associates
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

(defun make-callback-trampoline (index &optional info)
  (declare (ignore info))
  (let* ((p (%allocate-callback-pointer 16)))
    (macrolet ((arm-lap-word (instruction-form)
                 (uvref (uvref (compile nil `(lambda (&lap 0) (arm-lap-function () ((?? 0)) ,instruction-form))) 1) 0)))
      (setf (%get-unsigned-long p 0)
            (dpb (ldb (byte 12 0) index)
                 (byte 12 0)
                 (dpb (ldb (byte 4 12) index)
                      (byte 4 16)
                      (arm-lap-word (movw r12 (:$ ??)))))
            (%get-unsigned-long p 4)
            (arm-lap-word (ldr pc (:@ pc (:$ -4))))
            (%get-unsigned-long p 8)
            (%lookup-subprim-address #.(subprim-name->offset '.SPeabi-callback)))
      (ff-call (%kernel-import #.arm::kernel-import-makedataexecutable) 
               :address p 
               :unsigned-fullword 12
               :void)
      p)))
                    
