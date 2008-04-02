;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan  3 10:37:16 2005
;;;; Contains: Tests of SET-MACRO-CHARACTER

(in-package :cl-test)

(def-syntax-test set-macro-character.1
  (let ((*readtable* (copy-readtable))
	(*package* (find-package :cl-test)))
    (let ((v1 (read-from-string "?!")))
      (assert (eql v1 '?!))
      (flet ((%f (stream char)
		 (declare (ignore stream))
		 (assert (eql char #\?))
		 17))
	(let ((fn #'%f))
	  (assert (equal (multiple-value-list
			  (set-macro-character #\? fn nil))
			 '(t)))
	  (values
	   (multiple-value-list (read-from-string "?!"))
	   (multiple-value-list (read-from-string "!?")))))))
  (17 1)
  (! 1))

(def-syntax-test set-macro-character.2
  (let ((rt (copy-readtable))
	(*package* (find-package :cl-test)))
    (let ((v1 (read-from-string "?!")))
      (assert (eql v1 '?!))
      (flet ((%f (stream char)
		 (declare (ignore stream))
		 (assert (eql char #\?))
		 17))
	(let ((fn #'%f))
	  (assert (equal (multiple-value-list
			  (set-macro-character #\? fn t rt))
			 '(t)))
	  (let ((*readtable* rt))
	    (values
	     (multiple-value-list (read-from-string "?!"))
	     (multiple-value-list (read-from-string "!?"))))))))
  (17 1)
  (!? 2))

(defun set-macro-character.3-test-fn (stream char)
  (declare (ignore stream))
  (assert (eql char #\?))
  :foo)

(def-syntax-test set-macro-character.3
  (let ((*readtable* (copy-readtable))
	(*package* (find-package :cl-test)))
    (let ((v1 (read-from-string "?!"))
	  (fn 'set-macro-character.3-test-fn))
      (assert (eql v1 '?!))
      (assert (equal (multiple-value-list
		      (set-macro-character #\? fn nil))
		     '(t)))
      (values
       (multiple-value-list (read-from-string "?!"))
       (multiple-value-list (read-from-string "!?")))))
  (:foo 1)
  (! 1))
