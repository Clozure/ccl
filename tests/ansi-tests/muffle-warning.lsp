;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar 23 08:46:05 2003
;;;; Contains: Tests of the MUFFLE-WARNING restart and function

(in-package :cl-test)

(deftest muffle-warning.1
  (restart-case
   (progn (muffle-warning) 'bad)
   (muffle-warning () 'good))
  good)

(deftest muffle-warning.2
  (let ((c1 (make-condition 'error))
	(c2 (make-condition 'error)))
    (restart-case
     (with-condition-restarts
      c1
      (list (first (compute-restarts)))
      (muffle-warning c2))
     (muffle-warning () 'bad)
     (muffle-warning () 'good)))
  good)

(deftest muffle-warning.3
  (restart-case
   (progn (muffle-warning nil) 'bad)
   (muffle-warning () 'good))
  good)

(deftest muffle-warning.4
  (let ((c1 (make-condition 'error))
	(c2 (make-condition 'error)))
    (restart-case
     (with-condition-restarts
      c1
      (list (first (compute-restarts)))
      (muffle-warning nil))
     (muffle-warning () 'good)
     (muffle-warning () 'bad)))
  good)

(deftest muffle-warning.5
  (signals-error
   (let ((c1 (make-condition 'error))
	 (c2 (make-condition 'error)))
     (with-condition-restarts
      c1
      (compute-restarts)
      ;; All conditions are now associated with c1
      (muffle-warning c2)))
   control-error)
  t)

