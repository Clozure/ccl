;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:28:37 2003
;;;; Contains: Tests of ASSOC-IF-NOT

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest assoc-if-not.1
    (let* ((x (copy-list '((1 . a) (3 . b) (6 . c) (7 . d))))
	   (xcopy (make-scaffold-copy x))
	   (result (assoc-if-not #'oddp x)))
      (and
       (check-scaffold-copy x xcopy)
       (eqt result (third x))
       result))
  (6 . c))

(deftest assoc-if-not.2
  (let* ((x (copy-list '((1 . a) (3 . b) (6 . c) (7 . d))))
	 (xcopy (make-scaffold-copy x))
	 (result (assoc-if-not #'evenp x :key #'1+)))
    (and
     (check-scaffold-copy x xcopy)
     (eqt result (third x))
     result))
  (6 . c))

(deftest assoc-if-not.3
  (let* ((x (copy-list '((1 . a) nil (3 . b) (6 . c) (7 . d))))
	 (xcopy (make-scaffold-copy x))
	 (result (assoc-if-not #'oddp x)))
    (and
     (check-scaffold-copy x xcopy)
     (eqt result (fourth x))
     result))
  (6 . c))

(deftest assoc-if-not.4
  (assoc-if-not #'identity '((a . b) nil (c . d) (nil . e) (f . g)))
  (nil . e))

;;; Order of argument evaluation tests

(deftest assoc-if-not.order.1
  (let ((i 0) x y)
    (values
     (assoc-if-not (progn (setf x (incf i)) #'identity)
		   (progn (setf y (incf i))
			  '((a . 1) (b . 2) (nil . 17) (d . 4))))
     i x y))
  (nil . 17) 2 1 2)

(deftest assoc-if-not.order.2
  (let ((i 0) x y z)
    (values
     (assoc-if-not (progn (setf x (incf i)) #'identity)
		   (progn (setf y (incf i))
			  '((a . 1) (b . 2) (nil . 17) (d . 4)))
		   :key (progn (setf z (incf i)) #'null))
     i x y z))
  (a . 1) 3 1 2 3)

;;; Keyword tests

(deftest assoc-if-not.allow-other-keys.1
  (assoc-if-not #'identity
		'((a . 1) (nil . 2) (c . 3)) :bad t :allow-other-keys t)
  (nil . 2))

(deftest assoc-if-not.allow-other-keys.2
  (assoc-if-not #'identity '((a . 1) (nil . 2) (c . 3))
	    :allow-other-keys t :also-bad t)
  (nil . 2))

(deftest assoc-if-not.allow-other-keys.3
  (assoc-if-not #'identity '((a . 1) (nil . 2) (c . 3))
	    :allow-other-keys t :also-bad t :key #'not)
  (a . 1))

(deftest assoc-if-not.allow-other-keys.4
  (assoc-if-not #'identity '((a . 1) (nil . 2) (c . 3)) :allow-other-keys t)
  (nil . 2))

(deftest assoc-if-not.allow-other-keys.5
  (assoc-if-not #'identity '((a . 1) (nil . 2) (c . 3)) :allow-other-keys nil)
  (nil . 2))

(deftest assoc-if-not.keywords.6
  (assoc-if-not #'identity '((a . 1) (nil . 2) (c . 3))
		:key #'identity :key #'null)
  (nil . 2))

(deftest assoc-if-not.keywords.7
  (assoc-if-not #'identity '((a . 1) (nil . 2) (c . 3)) :key nil :key #'null)
  (nil . 2))

;;; Macro env tests

(deftest assoc-if-not.env.1
  (macrolet
   ((%m (z) z))
   (let ((alist '((1 . a) (3 . b) (4 . c) (6 . d))))
     (values
      (assoc-if-not (expand-in-current-env (%m 'oddp)) alist)
      (assoc-if-not (expand-in-current-env (%m #'oddp)) alist)
      (assoc-if-not 'oddp (expand-in-current-env (%m alist))))))
  (4 . c)
  (4 . c)
  (4 . c))

(deftest assoc-if-not.env.2
  (macrolet
   ((%m (z) z))
   (let ((alist '((1 . a) (3 . b) (4 . c) (6 . d))))
     (values
      (assoc-if-not 'evenp alist (expand-in-current-env (%m :key)) #'1+)
      (assoc-if-not #'evenp alist :key (expand-in-current-env (%m '1+)))
      )))
  (4 . c)
  (4 . c))
      
;;; Error tests

(deftest assoc-if-not.error.1
  (signals-error (assoc-if-not) program-error)
  t)

(deftest assoc-if-not.error.2
  (signals-error (assoc-if-not #'null) program-error)
  t)

(deftest assoc-if-not.error.3
  (signals-error (assoc-if-not #'null nil :bad t)
		 program-error)
  t)

(deftest assoc-if-not.error.4
  (signals-error (assoc-if-not #'null nil :key)
		 program-error)
  t)

(deftest assoc-if-not.error.5
  (signals-error (assoc-if-not #'null nil 1 1)
		 program-error)
  t)

(deftest assoc-if-not.error.6
  (signals-error (assoc-if-not #'null nil :bad t :allow-other-keys nil)
		 program-error)
  t)

(deftest assoc-if-not.error.7
  (signals-error (assoc-if-not #'cons '((a b)(c d)))
		 program-error)
  t)

(deftest assoc-if-not.error.8
  (signals-error (assoc-if-not #'identity '((a b)(c d)) :key #'cons)
		 program-error)
  t)

(deftest assoc-if-not.error.9
  (signals-type-error x 'a (assoc-if-not #'car '((a b)(c d))))
  t)

(deftest assoc-if-not.error.10
  (signals-type-error x 'a (assoc-if-not #'identity '((a b)(c d)) :key #'car))
  t)

(deftest assoc-if-not.error.11
  (signals-error (assoc-if-not #'identity '((a . b) . c))
		 type-error)
  t)

(deftest assoc-if-not.error.12
  (signals-error (assoc-if-not #'identity '((a . b) :bad (c . d)))
		 type-error)
  t)

(deftest assoc-if-not.error.13
  (signals-type-error x 'y (assoc-if-not #'identity x))
  t)
