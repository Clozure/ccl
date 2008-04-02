;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jun 21 22:35:48 2003
;;;; Contains: Tests of SET

(in-package :cl-test)

(deftest set.1
  (let ((*var-used-in-set-tests* 'a)
	(var '*var-used-in-set-tests*))
    (declare (special *var-used-in-set-tests*))
    (values
     *var-used-in-set-tests*
     (set var 'b)
     *var-used-in-set-tests*))
  a b b)

(deftest set.2
  (let ((*var-used-in-set-tests* 'a)
	(var '*var-used-in-set-tests*))
    (declare (special *var-used-in-set-tests*))
    (values
     (let ((*var-used-in-set-tests* 'c))
       (list (set var 'b) *var-used-in-set-tests* (symbol-value var)))
     *var-used-in-set-tests*))
  (b c b)
  b)

(deftest set.error.1
  (signals-error (set) program-error)
  t)

(deftest set.error.2
  (signals-error
   (let ((*var-used-in-set-tests* 'a))
     (declare (special *var-used-in-set-tests*))
     (set '*var-used-in-set-tests*))
   program-error)
  t)

(deftest set.error.3
  (signals-error
   (let ((*var-used-in-set-tests* 'a))
     (declare (special *var-used-in-set-tests*))
     (set '*var-used-in-set-tests* nil nil))
   program-error)
  t)

(deftest set.error.4
  (signals-error
   (let ((*var-used-in-set-tests* 'a) (*y* 'b))
     (declare (special *var-used-in-set-tests*))
     (set '*var-used-in-set-tests* nil '*y* nil))
   program-error)
  t)
