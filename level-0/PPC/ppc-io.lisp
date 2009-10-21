;;; -*- Mode: Lisp; Package: CCL; -*-
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

;;; not very smart yet

(defppclapfunction %get-errno ()
  (ldr imm1 target::tcr.errno-loc target::rcontext)
  (lwz imm0 0 imm1)
  (stw rzero 0 imm1)
  (neg imm0 imm0)
  (box-fixnum arg_z imm0)
  (blr))

; end
