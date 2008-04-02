;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb  5 11:42:24 2005
;;;; Contains: Tests of dispatch macro character functions

(in-package :cl-test)

(deftest make-dispatch-macro-character.1
  (with-standard-io-syntax
   (let* ((*readtable* (copy-readtable nil))
	  (*package* (find-package "CL-TEST")))
     (values
      (make-dispatch-macro-character #\!)
      (read-from-string "123!"))))
  t 123)

(deftest make-dispatch-macro-character.2
  (with-standard-io-syntax
   (let* ((*readtable* (copy-readtable nil))
	  (*package* (find-package "CL-TEST")))
     (values
      (make-dispatch-macro-character #\! t)
      (read-from-string "123!"))))
  t 123!)

(deftest make-dispatch-macro-character.3
  (with-standard-io-syntax
   (let* ((*readtable* (copy-readtable nil))
	  (*package* (find-package "CL-TEST")))
     (values
      (make-dispatch-macro-character #\!)
      (loop for c across +standard-chars+
	    for result = (handler-case
			  (read-from-string (coerce (list #\! c #\X) 'string))
			  (reader-error (c) :good)
			  (error (c) :bad))
	    unless (eql result :good)
	    collect (list c result)))))
  t nil)

(deftest make-dispatch-macro-character.4
  (with-standard-io-syntax
   (let* ((rt (copy-readtable nil))
	  (*package* (find-package "CL-TEST")))
     (values
      (make-dispatch-macro-character #\! t rt)
      (read-from-string "!")
      (let ((*readtable* rt))
	(read-from-string "123!")))))
  t ! 123!)

(deftest make-dispatch-macro-character.error.1
  (let ((*readtable* (copy-readtable nil)))
    (signals-error (make-dispatch-macro-character) program-error))
  t)

(deftest make-dispatch-macro-character.error.2
  (let ((*readtable* (copy-readtable nil)))
    (signals-error (make-dispatch-macro-character #\! t *readtable* nil)
		   program-error))
  t)

;;; GET-DISPATCH-MACRO-CHARACTER

(deftest get-dispatch-macro-character.1
  (loop for c across +standard-chars+
	when (and (not (eql c #\#))
			  (handler-case
			   (list
			     (get-dispatch-macro-character c #\a)
			     c)
			   (error (cnd) nil)))
	collect it)
  nil)



	  