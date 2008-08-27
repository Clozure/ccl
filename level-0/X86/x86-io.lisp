;;; -*- Mode: Lisp; Package: CCL; -*-
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

;;; not very smart yet

#+x8664-target
(defx86lapfunction %get-errno ()
  (movq (:rcontext x8664::tcr.errno-loc) (% imm1))
  (movslq (@ (% imm1)) (% imm0))
  (movss (% fpzero) (@ (% imm1)))
  (negq (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

#+x8632-target
(defx8632lapfunction %get-errno ()
  (movl (:rcontext x8632::tcr.errno-loc) (% imm0))
  (movl (@ (% imm0)) (% imm0))
  (neg (% imm0))
  (box-fixnum imm0 arg_z)
  (movl (:rcontext x8632::tcr.errno-loc) (% imm0))
  (movss (% fpzero) (@ (% imm0)))
  (single-value-return))

