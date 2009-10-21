; -*- Mode:Lisp; Package:CCL; -*-
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

;; distrib-inits.lisp

; Things that are in the development environment that need to be
; added to the distribution environment.

; This needs to be compiled after everything is loaded.

(in-package "CCL")

; *def-accessor-types* is used by the inspector to name slots in uvectors
(dolist (cell '#.*def-accessor-types*)
  (add-accessor-types (list (car cell)) (cdr cell)))
