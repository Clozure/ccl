;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar 23 08:25:50 2003
;;;; Contains: Tests of the ABORT restart and function

(in-package :cl-test)

(deftest abort.1
  (restart-case
   (progn (abort) 'bad)
   (abort () 'good))
  good)

(deftest abort.2
  (let ((c1 (make-condition 'error))
	(c2 (make-condition 'error)))
    (restart-case
     (with-condition-restarts
      c1
      (list (first (compute-restarts)))
      (abort c2))
     (abort () 'bad)
     (abort () 'good)))
  good)

(deftest abort.3
  (restart-case
   (progn (abort nil) 'bad)
   (abort () 'good))
  good)

(deftest abort.4
  (let ((c1 (make-condition 'error))
	(c2 (make-condition 'error)))
    (restart-case
     (with-condition-restarts
      c1
      (list (first (compute-restarts)))
      (abort nil))
     (abort () 'good)
     (abort () 'bad)))
  good)

(deftest abort.5
  (signals-error
   (let ((c1 (make-condition 'error))
	 (c2 (make-condition 'error)))
     (with-condition-restarts
      c1
      (compute-restarts)
      ;; All conditions are now associated with c1
      (abort c2)))
   control-error)
  t)
