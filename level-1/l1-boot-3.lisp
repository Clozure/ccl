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

;;; l1-boot-3.lisp
;;; Third part of l1-boot

(in-package "CCL")

(catch :toplevel
    (or (find-package "COMMON-LISP-USER")
        (make-package "COMMON-LISP-USER" :use '("COMMON-LISP" "CCL") :NICKNAMES '("CL-USER")))
)

(set-periodic-task-interval .33)
(setq cmain xcmain)
(setq %err-disp %xerr-disp)

;;; do heap sanity checking for now
#+x8632-target
(setq *gc-event-status-bits* (logior *gc-event-status-bits*
				     $gc-integrity-check-bit))
;;;end of l1-boot-3.lisp

