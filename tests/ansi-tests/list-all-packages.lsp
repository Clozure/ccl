;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 21 17:47:37 2004
;;;; Contains: Tests of LIST-ALL-PACKAGES

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list-all-packages

;; list-all-packages returns a list
(deftest list-all-packages.1
  (numberp (ignore-errors (list-length (list-all-packages))))
  t)

;; The required packages are present
(deftest list-all-packages.2
  (progn
    (set-up-packages)
    (notnot
     (subsetp
      (list (find-package "CL")
	    (find-package "CL-USER")
	    (find-package "KEYWORD")
	    (find-package "A")
	    (find-package "REGRESSION-TEST")
	    (find-package "CL-TEST")
	    (find-package "B"))
      (list-all-packages))))
  t)

;; The list returned has only packages in it
(deftest list-all-packages.3
  (notnot-mv (every #'packagep (list-all-packages)))
  t)

;; It returns a list of the same packages each time it is called
(deftest list-all-packages.4
  (let ((p1 (list-all-packages))
	(p2 (list-all-packages)))
    (and (subsetp p1 p2)
	 (subsetp p2 p1)))
  t)

(deftest list-all-packages.error.1
  (signals-error (list-all-packages nil) program-error)
  t)
