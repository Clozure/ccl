;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 13 20:09:50 2004
;;;; Contains: Tests for STREAM-ELEMENT-TYPE

(in-package :cl-test)

(deftest stream-element-type.1
  (loop for s in (list *debug-io* *error-output* *query-io*
		       *standard-input* *standard-output*
		       *trace-output* *terminal-io*)
	for results = (multiple-value-list (stream-element-type s))
	unless (and (eql (length results) 1)
		    (car results))
	collect s)
  nil)

(deftest stream-element-type.2
  (let ((pn "foo.txt"))
    (loop for i from 1 to 100
	  for etype = `(unsigned-byte ,i)
	  for s = (progn (delete-all-versions pn)
			 (open pn :direction :output
			       :element-type etype))
	  unless
	  (multiple-value-bind (sub good)
	      (subtypep etype (stream-element-type s))
	    (close s)
	    (or sub (not good)))
	  collect i))
  nil)

(deftest stream-element-type.3
  (let ((pn "foo.txt"))
    (loop for i from 1 to 100
	  for etype = `(signed-byte ,i)
	  for s = (progn (delete-all-versions pn)
			 (open pn :direction :output
			       :element-type etype))
	  unless
	  (multiple-value-bind (sub good)
	      (subtypep etype (stream-element-type s))
	    (close s)
	    (or sub (not good)))
	  collect i))
  nil)

(deftest stream-element-type.4
  (let ((pn "foo.txt"))
    (loop for i from 1 to 100
	  for etype = `(integer 0 ,i)
	  for s = (progn (delete-all-versions pn)
			 (open pn :direction :output
			       :element-type etype))
	  unless
	  (multiple-value-bind (sub good)
	      (subtypep etype (stream-element-type s))
	    (close s)
	    (or sub (not good)))
	  collect i))
  nil)


(deftest stream-element-type.5
  :notes (:assume-no-simple-streams)
  (let ((pn "foo.txt"))
    (delete-all-versions pn)
    (let ((s (open pn :direction :output)))
      (let ((etype (stream-element-type s)))
	(unwind-protect
	    (equalt (multiple-value-list (subtypep* 'character etype))
		    '(nil t))
	  (close s)))))
  nil)

(deftest stream-element-type.6
  :notes (:assume-no-simple-streams)
  (let ((pn "foo.txt"))
    (delete-all-versions pn)
    (let ((s (open pn :direction :output
		   :element-type :default)))
      (let ((etype (stream-element-type s)))
	(unwind-protect
	    (multiple-value-bind (sub1 good1) (subtypep* etype 'integer)
	      (multiple-value-bind (sub2 good2) (subtypep* etype 'character)
		(or (not good1)
		    (not good2)
		    sub1 sub2)))
	  (close s)))))
  t)

(deftest stream-element-type.error.1
  (signals-error (stream-element-type) program-error)
  t)

(deftest stream-element-type.error.2
  (signals-error (stream-element-type *standard-input* nil) program-error)
  t)

(deftest stream-element-type.error.3
  (check-type-error #'stream-element-type #'streamp)
  nil)
