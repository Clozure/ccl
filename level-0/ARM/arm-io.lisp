;;; -*- Mode: Lisp; Package: CCL; -*-
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

;;; not very smart yet

(defarmlapfunction %get-errno ()
  (mov temp0 (:$ 0))
  (ldr imm1 (:@ rcontext (:$ arm::tcr.errno-loc)))
  (ldr imm0 (:@ imm1 (:$ 0)))
  (str temp0 (:@ imm1 (:$ 0)))
  (rsb imm0 imm0 (:$ 0))
  (box-fixnum arg_z imm0)
  (bx lr))

; end
