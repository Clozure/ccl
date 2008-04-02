;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar 23 09:13:59 2003
;;;; Contains: Tests for USE-VALUE restart and function

(in-package :cl-test)

(deftest use-value.1
  (restart-case
   (progn (use-value 10) 'bad)
   (use-value (x) (list x 'good)))
  (10 good))

(deftest use-value.2
  (let ((c1 (make-condition 'error))
	(c2 (make-condition 'error)))
    (restart-case
     (with-condition-restarts
      c1
      (list (first (compute-restarts)))
      (use-value 17 c2))
     (use-value (x) (list x 'bad))
     (use-value (x) (list x 'good))))
  (17 good))

(deftest use-value.3
  (restart-case
   (progn (use-value 11 nil) 'bad)
   (use-value (x) (list x 'good)))
  (11 good))

(deftest use-value.4
  (let ((c1 (make-condition 'error))
	(c2 (make-condition 'error)))
    (restart-case
     (with-condition-restarts
      c1
      (list (first (compute-restarts)))
      (use-value 18 nil))
     (use-value (x) (list x 'good))
     (use-value (x) (list x 'bad))))
  (18 good))

(deftest use-value.5
  (let ((c1 (make-condition 'error))
	(c2 (make-condition 'error)))
     (with-condition-restarts
      c1
      (compute-restarts)
      ;; All conditions are now associated with c1
      (use-value 21 c2)))
  nil)

