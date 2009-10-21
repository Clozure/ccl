;;-*-Mode: LISP; Package: CCL -*-
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

; Level-1.lisp

(in-package "CCL")

(macrolet ((l1-load (name)
	     (let* ((namestring
		     (concatenate 'simple-base-string
                                  "./l1-fasls/"
				  (string name)
                                  (namestring (backend-target-fasl-pathname
                                               *target-backend*)))))
	       `(%fasload ,namestring)))
	   (bin-load (name)
	     (let* ((namestring
		     (concatenate 'simple-base-string
                                  "./bin/"
				  (string name)
                                  (namestring (backend-target-fasl-pathname
                                               *target-backend*)))))
	       `(%fasload ,namestring))))

  (l1-load "l1-cl-package")
  (l1-load "l1-utils")
  (l1-load "l1-init")
  (l1-load "l1-symhash")
  (l1-load "l1-numbers")
  (l1-load "l1-aprims")
  #+ppc-target
  (l1-load "ppc-callback-support")
  #+x86-target
  (l1-load "x86-callback-support")
  (l1-load "l1-callbacks")
  (l1-load "l1-sort")
  (bin-load "lists")
  (bin-load "sequences")
  (l1-load "l1-dcode")
  (l1-load "l1-clos-boot")
  (bin-load "hash")
  (l1-load "l1-clos")
  (bin-load "defstruct")
  (bin-load "dll-node")
  (l1-load "l1-unicode")
  (l1-load "l1-streams")
  (l1-load "linux-files")
  (bin-load "chars")
  (l1-load "l1-files")
  (provide "SEQUENCES")
  (provide "DEFSTRUCT")
  (provide "CHARS")
  (provide "LISTS")
  (provide "DLL-NODE")
  (l1-load "l1-typesys")
  (l1-load "sysutils")
  #+ppc-target
  (l1-load "ppc-threads-utils")
  #+x86-target
  (l1-load "x86-threads-utils")
  (l1-load "l1-lisp-threads")
  (l1-load "l1-application")
  (l1-load "l1-processes")
  (l1-load "l1-io")
  (l1-load "l1-reader")
  (l1-load "l1-readloop")
  (l1-load "l1-readloop-lds")
  (l1-load "l1-error-system")

  (l1-load "l1-events")
  #+ppc-target
  (l1-load "ppc-trap-support")
  #+x86-target
  (l1-load "x86-trap-support")
  (l1-load "l1-format")
  (l1-load "l1-sysio")
  (l1-load "l1-pathnames")
  (l1-load "l1-boot-lds")

  (l1-load "l1-boot-1")
  (l1-load "l1-boot-2")
  (l1-load "l1-boot-3")

  )

(require "PREPARE-MCL-ENVIRONMENT")
(progn
  (%set-toplevel #'(lambda ()
                     (setq *loading-file-source-file* nil
                           *loading-toplevel-location* nil)
                     (toplevel-loop)))
  (set-user-environment t)
  (toplevel))
