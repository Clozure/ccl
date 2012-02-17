;;;-*-Mode: LISP; Package: CCL -*-
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

(in-package "CCL")

(defparameter *openmcl-major-version* 1)
(defparameter *openmcl-minor-version* 8)
(defparameter *openmcl-revision* "prerelease")
;;; May be set by xload-level-0
(defvar *openmcl-svn-revision* nil)
(defparameter *openmcl-dev-level* nil)

(defparameter *openmcl-version* (format nil "~d.~d~@[-~a~]~@[-r~a~] ~@[+~s~] (~@[~A: ~]~~A)"
					*openmcl-major-version*
					*openmcl-minor-version*
					(unless (null *openmcl-revision*)
					  *openmcl-revision*)
					(if (and (typep *openmcl-svn-revision* 'string)
                                                 (> (length *openmcl-svn-revision*) 0))
                                          *openmcl-svn-revision*)
                                        *optional-features*
                                        *openmcl-dev-level*))




;;; end
