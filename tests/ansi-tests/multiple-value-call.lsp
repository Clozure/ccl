;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 23:35:07 2002
;;;; Contains: Tests of MULTIPLE-VALUE-CALL, MULTIPLE-VALUE-LIST

(in-package :cl-test)

(deftest multiple-value-call.1
  (multiple-value-call #'+ (values 1 2) (values) 3 (values 4 5 6))
  21)

(deftest multiple-value-call.2
  (multiple-value-call 'list)
  nil)

(deftest multiple-value-call.3
  (multiple-value-call 'list (floor 13 4))
  (3 1))

;;; Macros are expanded in the appropriate environment

(deftest multiple-value-call.4
  (macrolet
   ((%m (z) z))
   (multiple-value-call (expand-in-current-env (%m #'list)) (values 1 2)))
  (1 2))

(deftest multiple-value-call.5
  (macrolet
   ((%m (z) z))
   (multiple-value-call 'list (expand-in-current-env (%m (values 1 2)))))
  (1 2))


