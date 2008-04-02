;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:26:48 2003
;;;; Contains: Tests of ACONS

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest acons.1
  (let* ((x (copy-tree '((c . d) (e . f))))
	 (xcopy (make-scaffold-copy x))
	 (result (acons 'a 'b x)))
    (and
     (check-scaffold-copy x xcopy)
     (eqt (cdr result) x)
     result))
  ((a . b) (c . d) (e . f)))

(deftest acons.2
  (acons 'a 'b nil)
  ((a . b)))

(deftest acons.3
  (acons 'a 'b 'c)
  ((a . b) . c))

(deftest acons.4
  (acons '((a b)) '(((c d) e) f) '((1 . 2)))
  (( ((a b)) . (((c d) e) f)) (1 . 2)))

(deftest acons.5
  (acons "ancd" 1.143 nil)
  (("ancd" . 1.143)))

(deftest acons.6
  (acons #\R :foo :bar)
  ((#\R . :foo) . :bar))

(deftest acons.7
  (macrolet ((%m (z) z)) (acons (expand-in-current-env (%m 'a)) 'b '(c)))
  ((a . b) c))

(deftest acons.8
  (macrolet ((%m (z) z)) (acons 'a (expand-in-current-env (%m 'b)) '(c)))
  ((a . b) c))

(deftest acons.9
  (macrolet ((%m (z) z)) (acons 'a 'b (expand-in-current-env (%m '(c)))))
  ((a . b) c))

(deftest acons.order.1
  (let ((i 0) x y z)
    (values
     (acons (progn (setf x (incf i)) 'a)
	    (progn (setf y (incf i)) 'b)
	    (progn (setf z (incf i)) '((c . d))))
     i x y z))
  ((a . b)(c . d))
  3 1 2 3)

(def-fold-test acons.fold.1 (acons 'x 'y nil))
(def-fold-test acons.fold.2 (acons 1 2 '((3 . 4) (5 . 6))))

;;; Error tests

(deftest acons.error.1
  (signals-error (acons) program-error)
  t)

(deftest acons.error.2
  (signals-error (acons 'a) program-error)
  t)

(deftest acons.error.3
  (signals-error (acons 'a 'b) program-error)
  t)

(deftest acons.error.4
  (signals-error (acons 'a 'b 'c 'd) program-error)
  t)
