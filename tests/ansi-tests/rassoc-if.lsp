;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:34:59 2003
;;;; Contains: Tests of RASSOC-IF

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest rassoc-if.1
    (let* ((x (rev-assoc-list '((1 . a) (3 . b) (6 . c) (7 . d))))
	   (xcopy (make-scaffold-copy x))
	   (result (rassoc-if #'evenp x)))
      (and
       (check-scaffold-copy x xcopy)
       (eqt result (third x))
       result))
  (c . 6))

(deftest rassoc-if.2
  (let* ((x (rev-assoc-list '((1 . a) (3 . b) (6 . c) (7 . d))))
	 (xcopy (make-scaffold-copy x))
	 (result (rassoc-if #'oddp x :key #'1+)))
    (and
     (check-scaffold-copy x xcopy)
     (eqt result (third x))
     result))
  (c . 6))

(deftest rassoc-if.3
    (let* ((x (rev-assoc-list '((1 . a) nil (3 . b) (6 . c) (7 . d))))
	   (xcopy (make-scaffold-copy x))
	   (result (rassoc-if #'evenp x)))
      (and
       (check-scaffold-copy x xcopy)
       (eqt result (fourth x))
       result))
  (c . 6))

(deftest rassoc-if.4
    (rassoc-if #'null
	       (rev-assoc-list '((a . b) nil (c . d) (nil . e) (f . g))))
  (e))

;;; Order of argument evaluation

(deftest rassoc-if.order.1
  (let ((i 0) x y)
    (values
     (rassoc-if (progn (setf x (incf i)) #'null)
		(progn (setf y (incf i))
		       '((1 . a) (2 . b) (17) (4 . d))))
     i x y))
  (17) 2 1 2)

(deftest rassoc-if.order.2
  (let ((i 0) x y z)
    (values
     (rassoc-if (progn (setf x (incf i)) #'null)
		(progn (setf y (incf i))
		       '((1 . a) (2 . b) (17) (4 . d)))
		:key (progn (setf z (incf i)) #'null))
     i x y z))
  (1 . a) 3 1 2 3)


;;; Keyword tests

(deftest rassoc-if.allow-other-keys.1
  (rassoc-if #'null '((1 . a) (2) (3 . c)) :bad t :allow-other-keys t)
  (2))

(deftest rassoc-if.allow-other-keys.2
  (rassoc-if #'null '((1 . a) (2) (3 . c)) :allow-other-keys t :bad t)
  (2))

(deftest rassoc-if.allow-other-keys.3
  (rassoc-if #'identity '((1 . a) (2) (3 . c)) :allow-other-keys t :bad t
	  :key 'not)
  (2))

(deftest rassoc-if.allow-other-keys.4
  (rassoc-if #'null '((1 . a) (2) (3 . c)) :allow-other-keys t)
  (2))

(deftest rassoc-if.allow-other-keys.5
  (rassoc-if #'null '((1 . a) (2) (3 . c)) :allow-other-keys nil)
  (2))

(deftest rassoc-if.keywords.6
  (rassoc-if #'identity '((1 . a) (2) (3 . c)) :key #'not :key #'identity)
  (2))

;;; Error tests

(deftest rassoc-if.error.1
  (signals-error (rassoc-if) program-error)
  t)

(deftest rassoc-if.error.2
  (signals-error (rassoc-if #'null) program-error)
  t)

(deftest rassoc-if.error.3
  (signals-error (rassoc-if #'null nil :bad t) program-error)
  t)

(deftest rassoc-if.error.4
  (signals-error (rassoc-if #'null nil :key) program-error)
  t)

(deftest rassoc-if.error.5
  (signals-error (rassoc-if #'null nil 1 1) program-error)
  t)

(deftest rassoc-if.error.6
  (signals-error (rassoc-if #'null nil :bad t :allow-other-keys nil) program-error)
  t)

(deftest rassoc-if.error.7
  (signals-error (rassoc-if #'cons '((a . b)(c . d))) program-error)
  t)

(deftest rassoc-if.error.8
  (signals-error (rassoc-if #'car '((a . b)(c . d))) type-error)
  t)

(deftest rassoc-if.error.9
  (signals-error (rassoc-if #'identity '((a . b)(c . d)) :key #'cons) program-error)
  t)

(deftest rassoc-if.error.10
  (signals-error (rassoc-if #'identity '((a . b)(c . d)) :key #'car) type-error)
  t)

(deftest rassoc-if.error.11
  (signals-error (rassoc-if #'not '((a . b) . c)) type-error)
  t)

(deftest rassoc-if.error.12
  (check-type-error #'(lambda (x) (rassoc-if #'identity x)) #'listp)
  nil)
