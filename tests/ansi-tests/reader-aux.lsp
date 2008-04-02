;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Jan 14 07:43:48 2005
;;;; Contains: Auxiliary functions and macros for reader tests

(in-package :cl-test)

;;; Define a test using standard io syntax

(defmacro def-syntax-test (name form &body expected-results)
  `(deftest ,name
     (with-standard-io-syntax (let ((*package* (find-package :cl-test))) ,form))
     ,@expected-results))

;;; Macros for testing specific features

(defmacro def-syntax-vector-test (name form &body expected-elements)
  `(def-syntax-test ,name
     (let ((v (read-from-string ,form)))
       (assert (simple-vector-p v))
       v)
     ,(apply #'vector expected-elements)))

(defmacro def-syntax-bit-vector-test (name form &body expected-elements)
  `(def-syntax-test ,name
     (let ((v (read-from-string ,form)))
       (assert (simple-bit-vector-p v))
       v)
     ,(make-array (length expected-elements) :element-type 'bit :initial-contents expected-elements)))

(defmacro def-syntax-unintern-test (name string)
  `(deftest ,name
     (let ((s (read-from-string ,(concatenate 'string "#:" string))))
       (values
	(symbol-package s)
	(symbol-name s)))
     nil ,(string-upcase string)))

(defmacro def-syntax-array-test (name form expected-result)
  `(def-syntax-test ,name
     (let ((v (read-from-string ,form)))
       (assert (typep v 'simple-array))
       (assert (not (array-has-fill-pointer-p v)))
       (assert (eql (array-element-type v)
		    (upgraded-array-element-type t)))
       v)
     ,(eval expected-result)))


