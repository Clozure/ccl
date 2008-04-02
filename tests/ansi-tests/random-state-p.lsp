;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep  6 17:50:04 2003
;;;; Contains: Tests of RANDOM-STATE-P

(in-package :cl-test)

(deftest random-state-p.error.1
  (signals-error (random-state-p) program-error)
  t)

(deftest random-state-p.error.2
  (signals-error (random-state-p nil nil) program-error)
  t)

(deftest random-state-p.1
  (check-type-predicate #'random-state-p 'random-state)
  nil)

(deftest random-state-p.2
  (notnot-mv (random-state-p *random-state*))
  t)

(deftest random-state-p.3
  (notnot-mv (random-state-p (make-random-state)))
  t)
