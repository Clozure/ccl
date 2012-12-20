;;;-*- Mode: Lisp; Package: CCL -*-
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

;;; l1-boot-3.lisp
;;; Third part of l1-boot

(in-package "CCL")

;;; Register Emacs-friendly aliases for some character encodings.
;;; This could go on forever; try to recognize at least some common
;;; cases.  (The precise set of encoding/coding-system names supported
;;; by Emacs likely depends on Emacs version, loaded Emacs packages, etc.)

(dotimes (i 16)
  (let* ((key (find-symbol (format nil "LATIN~d" i) :keyword))
         (existing (and key (lookup-character-encoding key))))
    (when existing
      (define-character-encoding-alias (intern (format nil "LATIN-~d" i) :keyword) existing)
      (define-character-encoding-alias (intern (format nil "ISO-LATIN-~d" i) :keyword) existing))))

(define-character-encoding-alias :mule-utf-8 :utf-8)

(catch :toplevel
    (or (find-package "COMMON-LISP-USER")
        (make-package "COMMON-LISP-USER" :use '("COMMON-LISP" "CCL") :NICKNAMES '("CL-USER")))
)

(set-periodic-task-interval .33)
(setq cmain xcmain)
(setq %err-disp %xerr-disp)

;;;end of l1-boot-3.lisp

