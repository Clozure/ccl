;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct 31 21:41:49 2004
;;;; Contains: Additional tests of the REAL type specifier

(in-package :cl-test)

(deftest real.1
  (loop for i = 1 then (ash i 1)
	for tp = `(real 0 ,i)
	repeat 200
	unless (and (not (typep -1 tp))
		    (not (typep -0.0001 tp))
		    (typep 0 tp)
		    (typep 0.0001 tp)
		    (typep 1 tp)
		    (typep i tp)
		    (not (typep (1+ i) tp)))
	collect (list i tp))
  nil)

(deftest real.2
  (loop for i = 1 then (ash i 1)
	for tp = `(real ,(- i) 0)
	repeat 200
	unless (and (not (typep (- -1 i) tp))
		    (typep (- i) tp)
		    (typep -1 tp)
		    (typep 0 tp)
		    (not (typep 1 tp))
		    (not (typep i tp))
		    (not (typep (1+ i) tp)))
	collect (list i tp))
  nil)

(deftest real.3
  (loop for i = 4 then (ash i 1)
	for tp = `(real 0 ,(/ i 3))
	repeat 200
	unless (and (not (typep -1 tp))
		    (not (typep -0.0001 tp))
		    (typep 0 tp)
		    (typep 0.0001 tp)
		    (typep 1 tp)
		    (typep (/ i 3) tp)
		    (not (typep (/ (1+ i) 3) tp)))
	collect (list i tp))
  nil)

(deftest real.4
  (loop for i = 4 then (ash i 1)
	for tp = `(real ,(- (/ i 3)) 0)
	repeat 200
	unless (and (not (typep (- -1 (/ i 3)) tp))
		    (typep (- (/ i 3)) tp)
		    (typep -1 tp)
		    (typep 0 tp)
		    (not (typep 1 tp))
		    (not (typep (/ i 3) tp))
		    (not (typep (1+ (/ i 3)) tp)))
	collect (list i tp))
  nil)
