;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep  6 15:47:42 2003
;;;; Contains: Tests of RANDOM

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(compile-and-load "random-aux.lsp")

(deftest random.error.1
  (signals-error (random) program-error)
  t)

(deftest random.error.2
  (signals-error (random 10 *random-state* nil) program-error)
  t)

(deftest random.error.3
  (check-type-error #'random (typef '(real (0))))
  nil)

(deftest random.1
  (loop for i from 2 to 30
	for n = (ash 1 i)
	nconc
	(loop for j = (1+ (random n))
	      repeat 20
	      nconc
	      (loop for r = (random j)
		    repeat i
		    unless (and (integerp r)
				(<= 0 r)
				(< r j))
		    collect (list j r))))
  nil)


(deftest random.2
  (loop for i from 2 to 20
	for n = (ash 1 i)
	nconc
	(loop for j = (random (float n))
	      repeat 20
	      unless (zerop j)
	      nconc
	      (loop for r = (random j)
		    repeat 20
		    unless (and (eql (float r j) r)
				(<= 0 r)
				(< r j))
		    collect (list j r))))
  nil)

(deftest random.3
  (binomial-distribution-test 10000
			      #'(lambda () (eql (random 2) 0)))
  t)

(deftest random.4
  (binomial-distribution-test 10000
			      #'(lambda () (< (random 1.0s0) 0.5s0)))
  t)

(deftest random.5
  (binomial-distribution-test 10000
			      #'(lambda () (< (random 1.0d0) 0.5d0)))
  t)

(deftest random.6
  (binomial-distribution-test 10000
			      #'(lambda () (evenp (random 1024))))
  t)

(deftest random.7
  (loop for x in '(10.0s0 20.0f0 30.0d0 40.0l0)
	for r = (random x)
	unless (eql (float r x) r)
	collect (list x r))
  nil)

(deftest random.8
  (let* ((f1 '(lambda (x) (random (if x 10 20))))
	 (f2 (compile nil f1)))
    (values
     (loop repeat 100 always (<= 0 (funcall f2 t) 9))
     (loop repeat 100 always (<= 0 (funcall f2 nil) 19))))
  t t)
    

;;; Do more statistical tests here

	