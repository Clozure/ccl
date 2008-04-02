;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:37:00 2003
;;;; Contains: Tests of GET-PROPERTIES

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest get-properties.1
  (get-properties nil nil)
  nil nil nil)

(deftest get-properties.2
  (get-properties '(a b) nil)
  nil nil nil)

(deftest get-properties.3
  (get-properties '(a b c d) '(a))
  a b (a b c d))

(deftest get-properties.4
  (get-properties '(a b c d) '(c))
  c d (c d))

(deftest get-properties.5
  (get-properties '(a b c d) '(c a))
  a b (a b c d))

(deftest get-properties.6
  (get-properties '(a b c d) '(b))
  nil nil nil)

(deftest get-properties.7
  (get-properties '("aa" b c d) (list (copy-seq "aa")))
  nil nil nil)

;;; I removed the next test (noticed by Duane Rettig) because
;;; the non-eqness of numbers may not be necesarily preserved.
;;; The standard says numbers may be copied at any time, and
;;; this might mean eql numbers are copied to a canonical eq
;;; value
#|
(deftest get-properties.8
  (get-properties '(1000000000000 b c d) (list (1+ 999999999999)))
  nil nil nil)
|#

(deftest get-properties.9
  (let* ((x (copy-list '(a b c d e f g h a c)))
	 (xcopy (make-scaffold-copy x))
	 (y (copy-list '(x y f g)))
	 (ycopy (make-scaffold-copy y)))
    (multiple-value-bind
	(indicator value tail)
	(get-properties x y)
      (and
       (check-scaffold-copy x xcopy)
       (check-scaffold-copy y ycopy)
       (eqt tail (nthcdr 6 x))
       (values indicator value tail))))
  g h (g h a c))

(deftest get-properties.order.1
  (let ((i 0) x y)
    (values
     (multiple-value-list
      (get-properties (progn (setf x (incf i)) '(a b c d))
		      (progn (setf y (incf i)) '(c))))
     i x y))
  (c d (c d)) 2 1 2)

(deftest get-properties.error.1
  (signals-error (get-properties) program-error)
  t)

(deftest get-properties.error.2
  (signals-error (get-properties nil) program-error)
  t)

(deftest get-properties.error.3
  (signals-error (get-properties nil nil nil) program-error)
  t)

(deftest get-properties.error.4
  (signals-error (get-properties '(a 1 b 2 c 3) '(x . y)) type-error)
  t)

(deftest get-properties.error.5
  (signals-error (get-properties '(a 1 b 2 c 3 . d) '(x y)) type-error)
  t)

(deftest get-properties.error.6
  (signals-error (get-properties '(a 1 b 2 c . d) '(x y)) type-error)
  t)
