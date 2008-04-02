;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar 23 09:10:22 2003
;;;; Contains: Tests for STORE-VALUE restart and function

(in-package :cl-test)

(deftest store-value.1
  (restart-case
   (progn (store-value 10) 'bad)
   (store-value (x) (list x 'good)))
  (10 good))

(deftest store-value.2
  (let ((c1 (make-condition 'error))
	(c2 (make-condition 'error)))
    (restart-case
     (with-condition-restarts
      c1
      (list (first (compute-restarts)))
      (store-value 17 c2))
     (store-value (x) (list x 'bad))
     (store-value (x) (list x 'good))))
  (17 good))

(deftest store-value.3
  (restart-case
   (progn (store-value 11 nil) 'bad)
   (store-value (x) (list x 'good)))
  (11 good))

(deftest store-value.4
  (let ((c1 (make-condition 'error))
	(c2 (make-condition 'error)))
    (restart-case
     (with-condition-restarts
      c1
      (list (first (compute-restarts)))
      (store-value 18 nil))
     (store-value (x) (list x 'good))
     (store-value (x) (list x 'bad))))
  (18 good))

(deftest store-value.5
  (let ((c1 (make-condition 'error))
	(c2 (make-condition 'error)))
     (with-condition-restarts
      c1
      (compute-restarts)
      ;; All conditions are now associated with c1
      (store-value 21 c2)))
  nil)
