;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 13 15:13:07 2003
;;;; Contains: Tests of APPLY

(in-package :cl-test)

;;; Error cases

(deftest apply.error.1
  (signals-error (apply) program-error)
  t)

(deftest apply.error.2
  (signals-error (apply #'cons) program-error)
  t)

(deftest apply.error.3
  (signals-error (apply #'cons nil) program-error)
  t)

(deftest apply.error.4
  (signals-error (apply #'cons (list 1 2 3))
		 program-error)
  t)

;;; Non-error cases

(deftest apply.1
  (apply #'cons 'a 'b nil)
  (a . b))

(deftest apply.2
  (apply #'cons 'a '(b))
  (a . b))

(deftest apply.3
  (apply #'cons '(a b))
  (a . b))

(deftest apply.4
  (let ((zeros (make-list (min 10000 (1- call-arguments-limit))
			  :initial-element 1)))
    (apply #'+ zeros))
  #.(min 10000 (1- call-arguments-limit)))

(deftest apply.5
  (apply 'cons '(a b))
  (a . b))

(deftest apply.6
  (macrolet ((%m (z) z))
	    (apply (expand-in-current-env (%m 'cons)) 1 2 nil))
  (1 . 2))

(deftest apply.7
  (macrolet ((%m (z) z))
	    (apply #'cons (expand-in-current-env (%m 1)) '(2)))
  (1 . 2))

(deftest apply.8
  (macrolet ((%m (z) z))
	    (apply #'cons (expand-in-current-env (%m '(1 2)))))
  (1 . 2))

(deftest apply.order.1
  (let ((i 0) x y z)
    (values
     (apply (progn (setf x (incf i))
		   #'list)
	    (progn (setf y (incf i))
		   'b)
	    (progn (setf z (incf i))
		   (list 'a)))
     i x y z))
  (b a) 3 1 2 3)

