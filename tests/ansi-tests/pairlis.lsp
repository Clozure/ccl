;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:30:55 2003
;;;; Contains: Tests of PAIRLIS

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

;; Pairlis has two legal behaviors: the pairs
;; can be prepended in the same order, or in the
;; reverse order, that they appear in the first
;; two arguments

(defun my-pairlis (x y &optional alist)
  (if (null x)
      alist
    (acons (car x) (car y)
	   (my-pairlis (cdr x) (cdr y) alist))))

(deftest pairlis.1
    (pairlis nil nil nil)
  nil)

(deftest pairlis.2
    (pairlis '(a) '(b) nil)
  ((a . b)))

(deftest pairlis.3
    (let* ((x (copy-list '(a b c d e)))
	   (xcopy (make-scaffold-copy x))
	   (y (copy-list '(1 2 3 4 5)))
	   (ycopy (make-scaffold-copy y))
	   (result (pairlis x y))
	   (expected (my-pairlis x y)))
      (and
       (check-scaffold-copy x xcopy)
       (check-scaffold-copy y ycopy)
       (or
	(equal result expected)
	(equal result (reverse expected)))
       t))
  t)

(deftest pairlis.4
    (let* ((x (copy-list '(a b c d e)))
	   (xcopy (make-scaffold-copy x))
	   (y (copy-list '(1 2 3 4 5)))
	   (ycopy (make-scaffold-copy y))
	   (z '((x . 10) (y . 20)))
	   (zcopy (make-scaffold-copy z))
	   (result (pairlis x y z))
	   (expected (my-pairlis x y z)))
      (and
       (check-scaffold-copy x xcopy)
       (check-scaffold-copy y ycopy)
       (check-scaffold-copy z zcopy)
       (eqt (cdr (cddr (cddr result))) z)
       (or
	(equal result expected)
	(equal result (append (reverse (subseq expected 0 5))
			      (subseq expected 5))))
       t))
  t)

(def-fold-test pairlis.fold.1 (pairlis '(a b) '(c d)))

;;; Error tests

(deftest pairlis.error.1
  (signals-error (pairlis) program-error)
  t)

(deftest pairlis.error.2
  (signals-error (pairlis nil) program-error)
  t)

(deftest pairlis.error.3
  (signals-error (pairlis nil nil nil nil) program-error)
  t)

(deftest pairlis.error.4
  (signals-error (pairlis 'a '(1)) type-error)
  t)

(deftest pairlis.error.5
  (signals-error (pairlis '(a) 'b) type-error)
  t)

(deftest pairlis.error.6
  (signals-error (pairlis '(a . b) '(c . d)) type-error)
  t)

(deftest pairlis.error.7
  (check-type-error #'(lambda (x) (pairlis x '(a b))) #'listp)
  nil)

(deftest pairlis.error.8
  (check-type-error #'(lambda (x) (pairlis '(a b) x)) #'listp)
  nil)
