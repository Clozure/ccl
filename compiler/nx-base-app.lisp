;;;-*-Mode: LISP; Package: CCL -*-
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


; Loaded instead of compiler for standalone applications.

(in-package "CCL")

;(require 'numbers)
(require 'sort)
(require 'hash)

; this file is now equiv to nx-basic
(%include "ccl:compiler;nx-basic.lisp")  ; get cons-var, augment-environment
; nx-basic includes lambda-list

; End of nx-base-app.lisp
