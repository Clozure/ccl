;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:29:07 2003
;;;; Contains: Tests of COPY-ALIST

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest copy-alist.1
    (let* ((x (copy-tree '((a . b) (c . d) nil (e f) ((x) ((y z)) w)
			   ("foo" . "bar") (#\w . 1.234)
			   (1/3 . 4123.4d5))))
	   (xcopy (make-scaffold-copy x))
	   (result (copy-alist x)))
      (and
       (check-scaffold-copy x xcopy)
       (= (length x) (length result))
       (every #'(lambda (p1 p2)
		  (or (and (null p1) (null p2))
		      (and (not (eqt p1 p2))
			   (eqlt (car p1) (car p2))
			   (eqlt (cdr p1) (cdr p2)))))
	      x result)
       t))
  t)

(def-fold-test copy-alist.2 (copy-alist '((a . b) nil (c . d))))
(def-fold-test copy-alist.3 (car (copy-alist '((a . b) nil (c . d)))))
(def-fold-test copy-alist.4 (caddr (copy-alist '((a . b) nil (c . d)))))

;;; Error tests

(deftest copy-alist.error.1
  (signals-error (copy-alist) program-error)
  t)

(deftest copy-alist.error.2
  (signals-error (copy-alist nil nil) program-error)
  t)

(deftest copy-alist.error.3
  (signals-error (copy-alist '((a . b) . c)) type-error)
  t)
