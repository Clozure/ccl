;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:27:57 2003
;;;; Contains: Tests of ASSOC-IF

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest assoc-if.1
    (let* ((x (copy-list '((1 . a) (3 . b) (6 . c) (7 . d))))
	   (xcopy (make-scaffold-copy x))
	   (result (assoc-if #'evenp x)))
      (and
       (check-scaffold-copy x xcopy)
       (eqt result (third x))
       result))
  (6 . c))

(deftest assoc-if.2
  (let* ((x (copy-list '((1 . a) (3 . b) (6 . c) (7 . d))))
	 (xcopy (make-scaffold-copy x))
	 (result (assoc-if #'oddp x :key #'1+)))
    (and
     (check-scaffold-copy x xcopy)
     (eqt result (third x))
     result))
  (6 . c))

(deftest assoc-if.3
    (let* ((x (copy-list '((1 . a) nil (3 . b) (6 . c) (7 . d))))
	   (xcopy (make-scaffold-copy x))
	   (result (assoc-if #'evenp x)))
      (and
       (check-scaffold-copy x xcopy)
       (eqt result (fourth x))
       result))
  (6 . c))

(deftest assoc-if.4
  (assoc-if #'null '((a . b) nil (c . d) (nil . e) (f . g)))
  (nil . e))

(deftest assoc-if.5
  (let () (assoc-if #'null '((a . b) nil (c . d) (nil . e) (f . g))))
  (nil . e))


;;; Order of argument evaluation

(deftest assoc-if.order.1
  (let ((i 0) x y)
    (values
     (assoc-if (progn (setf x (incf i)) #'null)
	       (progn (setf y (incf i))
		      '((a . 1) (b . 2) (nil . 17) (d . 4))))
     i x y))
  (nil . 17) 2 1 2)

(deftest assoc-if.order.2
  (let ((i 0) x y z)
    (values
     (assoc-if (progn (setf x (incf i)) #'null)
	       (progn (setf y (incf i))
		      '((a . 1) (b . 2) (nil . 17) (d . 4)))
	       :key (progn (setf z (incf i)) #'null))
     i x y z))
  (a . 1) 3 1 2 3)

;;; Keyword tests

(deftest assoc-if.allow-other-keys.1
  (assoc-if #'null '((a . 1) (nil . 2) (c . 3)) :bad t :allow-other-keys t)
  (nil . 2))

(deftest assoc-if.allow-other-keys.2
  (assoc-if #'null '((a . 1) (nil . 2) (c . 3))
	    :allow-other-keys t :also-bad t)
  (nil . 2))

(deftest assoc-if.allow-other-keys.3
  (assoc-if #'null '((a . 1) (nil . 2) (c . 3))
	    :allow-other-keys t :also-bad t :key #'not)
  (a . 1))

(deftest assoc-if.allow-other-keys.4
  (assoc-if #'null '((a . 1) (nil . 2) (c . 3)) :allow-other-keys t)
  (nil . 2))

(deftest assoc-if.allow-other-keys.5
  (assoc-if #'null '((a . 1) (nil . 2) (c . 3)) :allow-other-keys nil)
  (nil . 2))

(deftest assoc-if.keywords.6
  (assoc-if #'null '((a . 1) (nil . 2) (c . 3)) :key #'identity :key #'null)
  (nil . 2))

(deftest assoc-if.keywords.7
  (assoc-if #'null '((a . 1) (nil . 2) (c . 3)) :key nil :key #'null)
  (nil . 2))

;;; Macro env tests

(deftest assoc-if.env.1
  (macrolet
   ((%m (z) z))
   (let ((alist '((1 . a) (3 . b) (6 . c) (8 . d) (-1 . e))))
     (values
      (assoc-if (expand-in-current-env (%m 'evenp)) alist)
      (assoc-if (expand-in-current-env (%m #'evenp)) alist)
      (assoc-if #'evenp (expand-in-current-env (%m alist)))
      (assoc-if 'oddp alist (expand-in-current-env (%m :key)) '1+)
      (assoc-if 'oddp alist :key (expand-in-current-env (%m #'1+)))
      )))
  (6 . c)
  (6 . c)
  (6 . c)
  (6 . c)
  (6 . c))      

;;; Error cases

(deftest assoc-if.error.1
  (signals-error (assoc-if) program-error)
  t)

(deftest assoc-if.error.2
  (signals-error (assoc-if #'null) program-error)
  t)

(deftest assoc-if.error.3
  (signals-error (assoc-if #'null nil :bad t)
		 program-error)
  t)

(deftest assoc-if.error.4
  (signals-error (assoc-if #'null nil :key)
		 program-error)
  t)

(deftest assoc-if.error.5
  (signals-error (assoc-if #'null nil 1 1)
		 program-error)
  t)

(deftest assoc-if.error.6
  (signals-error (assoc-if #'null nil :bad t :allow-other-keys nil)
		 program-error)
  t)

(deftest assoc-if.error.7
  (signals-error (assoc-if #'cons '((a b)(c d)))
		 program-error)
  t)

(deftest assoc-if.error.8
  (signals-error (assoc-if #'identity '((a b)(c d)) :key #'cons)
		 program-error)
  t)

(deftest assoc-if.error.9
  (signals-type-error x 'a (assoc-if #'car '((a b)(c d))))
  t)

(deftest assoc-if.error.10
  (signals-type-error x 'a (assoc-if #'identity '((a b)(c d)) :key #'car))
  t)

(deftest assoc-if.error.11
  (signals-error (assoc-if #'null '((a . b) . c))
		 type-error)
  t)

(deftest assoc-if.error.12
  (signals-error (assoc-if #'null '((a . b) :bad (c . d)))
		 type-error)
  t)

(deftest assoc-if.error.13
  (signals-type-error x 'y (assoc-if #'null x))
  t)
