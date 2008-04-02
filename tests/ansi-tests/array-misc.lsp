;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 22 21:17:25 2003
;;;; Contains: Misc. tests of array features

(in-package :cl-test)

(deftest array-dimension-limit.1
  (and (<= 1024 array-dimension-limit) t)
  t)

(deftest array-dimension-limit.2
  (and (typep array-dimension-limit 'fixnum) t)
  t)

(deftest array-total-size-limit.1
  (and (<= 1024 array-total-size-limit) t)
  t)

(deftest array-total-size-limit.2
  (and (typep array-total-size-limit 'fixnum) t)
  t)

(deftest array-rank-limit.1
  (and (<= 8 array-rank-limit) t)
  t)

(deftest array-rank-limit.2
  (and (typep array-rank-limit 'fixnum) t)
  t)
