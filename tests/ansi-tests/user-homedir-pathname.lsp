;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Dec 11 22:26:24 2004
;;;; Contains: Tests of USER-HOMEDIR-PATHNAME

(in-package :cl-test)

(deftest user-homedir-pathname.1
  (let ((pn (user-homedir-pathname)))
    (notnot pn))
  t)

(deftest user-homedir-pathname.2
  (let* ((pn-list (multiple-value-list (user-homedir-pathname)))
	 (pn (first pn-list)))
    (values
     (length pn-list)
     (notnot-mv (pathnamep pn))))
  1 t)

(deftest user-homedir-pathname.3
  (let ((pn (user-homedir-pathname)))
    (pathname-name pn))
  nil)

(deftest user-homedir-pathname.4
  (let ((pn (user-homedir-pathname)))
    (pathname-type pn))
  nil)

(deftest user-homedir-pathname.5
  (let ((pn (user-homedir-pathname)))
    (pathname-version pn))
  nil)

;; (deftest user-homedir-pathname.6
;;  (let* ((pn (user-homedir-pathname))
;;	 (host (pathname-host pn)))
;;    (or (not host)
;;	(equalt pn (user-homedir-pathname host))))
;;  t)

(deftest user-homedir-pathname.7
  (let* ((pn (user-homedir-pathname :unspecific)))
    (or (null pn)
	(notnot (pathnamep pn))))
  t)

(deftest user-homedir-pathname.error.1
  (signals-error (user-homedir-pathname :unspecific nil) program-error)
  t)

