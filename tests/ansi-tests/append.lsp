;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:36:46 2003
;;;; Contains: Tests of APPEND

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest append.1
  (append)
  nil)

(deftest append.2
  (append 'x)
  x)

(deftest append.3
  (let ((x (list 'a 'b 'c 'd))
	(y (list 'e 'f 'g)))
    (let ((xcopy (make-scaffold-copy x))
	  (ycopy (make-scaffold-copy y)))
      (let ((result (append x y)))
	(and
	 (check-scaffold-copy x xcopy)
	 (check-scaffold-copy y ycopy)
	 result))))
  (a b c d e f g))

(deftest append.4
  (append (list 'a) (list 'b) (list 'c)
	  (list 'd) (list 'e) (list 'f)
	  (list 'g) 'h)
  (a b c d e f g . h))

(deftest append.5
  (append nil nil nil nil nil nil nil nil 'a)
  a)

(deftest append.6
  (append-6-body)
  0)

;;; Test suggested by Peter Graves
(deftest append.7
  (let ((x (list 'a 'b 'c 'd)))
    (eq (append x nil) x))
  nil)

;;; Compiler macro expansion in correct env

(deftest append.8
  (macrolet ((%m (z) z))
	    (append (expand-in-current-env (%m '(a b c)))))
  (a b c))

(deftest append.9
  (macrolet ((%m (z) z))
	    (append (expand-in-current-env (%m (list 1 2 3))) (list 4 5 6)))
  (1 2 3 4 5 6))

(deftest append.10
  (macrolet ((%m (z) z))
	    (append (list 1 2 3) (expand-in-current-env (%m (list 4 5 6)))))
  (1 2 3 4 5 6))

;;; Order of evaluation tests

(deftest append.order.1
  (let ((i 0) x y z)
    (values
     (append (progn (setf x (incf i)) (copy-list '(a b c)))
	     (progn (setf y (incf i)) (copy-list '(d e f)))
	     (progn (setf z (incf i)) (copy-list '(g h i))))
     i x y z))
  (a b c d e f g h i) 3 1 2 3)

(deftest append.order.2
  (let ((i 0)) (values (append (incf i)) i))
  1 1)

(def-fold-test append.fold.1 (append '(a b c) nil))
(def-fold-test append.fold.2 (append nil '(x) nil))

;;; Error tests

(deftest append.error.1
  (signals-error (append '(a . b) '(z))
		 type-error)
  t)

(deftest append.error.2
  (signals-error (append '(x y z) '(a . b) '(z))
		 type-error)
  t)
