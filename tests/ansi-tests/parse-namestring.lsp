;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 14 13:59:18 2004
;;;; Contains: Tests of PARSE-NAMESTRING

(in-package :cl-test)

;;; "Parsing a null string always succeeds, producing a pathname
;;;  with all components (except the host) equal to nil."

(deftest parse-namestring.1
  (let ((vals (multiple-value-list (parse-namestring ""))))
    (assert (= (length vals) 2))
    (let ((pn (first vals))
	  (pos (second vals)))
      (values
       (pathname-directory pn)
       (pathname-device pn)
       (pathname-name pn)
       (pathname-type pn)
       (pathname-version pn)
       pos)))
  nil nil nil nil nil 0)

(deftest parse-namestring.2
  (let ((vals (multiple-value-list (parse-namestring (make-array 0 :element-type 'base-char)))))
    (assert (= (length vals) 2))
    (let ((pn (first vals))
	  (pos (second vals)))
      (values
       (pathname-directory pn)
       (pathname-device pn)
       (pathname-name pn)
       (pathname-type pn)
       (pathname-version pn)
       pos)))
  nil nil nil nil nil 0)

(deftest parse-namestring.3
  (let ((vals (multiple-value-list (parse-namestring (make-array 4 :element-type 'base-char
								 :initial-element #\X
								 :fill-pointer 0)))))
    (assert (= (length vals) 2))
    (let ((pn (first vals))
	  (pos (second vals)))
      (values
       (pathname-directory pn)
       (pathname-device pn)
       (pathname-name pn)
       (pathname-type pn)
       (pathname-version pn)
       pos)))
  nil nil nil nil nil 0)

(deftest parse-namestring.4
  (loop for etype in '(standard-char base-char character)
	for s0 = (make-array 4 :element-type etype :initial-element #\X)
	for s = (make-array 0 :element-type etype :displaced-to s0
			    :displaced-index-offset 1)
	for vals = (multiple-value-list (parse-namestring s))
	for pn = (first vals)
	for pos = (second vals)
	do (assert (= (length vals) 2))
	nconc
	(let ((result (list (pathname-directory pn)
			    (pathname-device pn)
			    (pathname-name pn)
			    (pathname-type pn)
			    (pathname-version pn)
			    pos)))
	  (unless (equal result '(nil nil nil nil nil 0))
	    (list (list etype result)))))
  nil)

;;; Error tests

(deftest parse-namestring.error.1
  (signals-error (parse-namestring) program-error)
  t)

(deftest parse-name-string.error.2
  (signals-error (parse-namestring "" nil *default-pathname-defaults* :foo nil) program-error)
  t)

(deftest parse-name-string.error.3
  (signals-error (parse-namestring "" nil *default-pathname-defaults* :start) program-error)
  t)


