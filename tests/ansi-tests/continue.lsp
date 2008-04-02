;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar 23 08:37:15 2003
;;;; Contains: Tests of CONTINUE restart and function

(in-package :cl-test)

(deftest continue.1
  (restart-case
   (progn (continue) 'bad)
   (continue () 'good))
  good)

(deftest continue.2
  (let ((c1 (make-condition 'error))
	(c2 (make-condition 'error)))
    (restart-case
     (with-condition-restarts
      c1
      (list (first (compute-restarts)))
      (continue c2))
     (continue () 'bad)
     (continue () 'good)))
  good)

(deftest continue.3
  (restart-case
   (progn (continue nil) 'bad)
   (continue () 'good))
  good)

(deftest continue.4
  (let ((c1 (make-condition 'error))
	(c2 (make-condition 'error)))
    (restart-case
     (with-condition-restarts
      c1
      (list (first (compute-restarts)))
      (continue nil))
     (continue () 'good)
     (continue () 'bad)))
  good)

(deftest continue.5
  (let ((c1 (make-condition 'error))
	(c2 (make-condition 'error)))
    (with-condition-restarts
     c1
     (compute-restarts)
     ;; All conditions are now associated with c1
     (continue c2)))
  nil)


