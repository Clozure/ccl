;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Dec 11 22:15:54 2004
;;;; Contains: Tests of various string-returning functions from section 25

(in-package :cl-test)

(defmacro def-env-tests (fn-name)
  (flet ((%name (suffix) (intern (concatenate 'string (symbol-name fn-name) suffix)
				 (find-package :cl-test))))
    `(progn
       (deftest ,(%name ".1")
	 (let ((x (,fn-name)))
	   (or (not x)
	       (notnot (stringp x))))
	 t)
       (deftest ,(%name ".ERROR.1")
	 (signals-error (,fn-name nil) program-error)
	 t))))

(def-env-tests lisp-implementation-type)
(def-env-tests lisp-implementation-version)
(def-env-tests short-site-name)
(def-env-tests long-site-name)
(def-env-tests machine-instance)
(def-env-tests machine-type)
(def-env-tests machine-version)
(def-env-tests software-type)
(def-env-tests software-version)
