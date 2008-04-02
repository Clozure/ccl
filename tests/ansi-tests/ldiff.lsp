;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:46:56 2003
;;;; Contains: Tests of LDIFF

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest ldiff.1
  (let* ((x (copy-tree '(a b c d e f)))
	 (xcopy (make-scaffold-copy x)))
    (let ((result (ldiff x (cdddr x))))
      (and (check-scaffold-copy x xcopy)
	   result)))
  (a b c))

(deftest ldiff.2
  (let* ((x (copy-tree '(a b c d e f)))
	 (xcopy (make-scaffold-copy x)))
    (let ((result (ldiff x 'a)))
      (and
       (check-scaffold-copy x xcopy)
       (zerop
	(loop
	 for a on x and b on result count
	 (eqt a b)))
       result)))
  (a b c d e f))

;; Works when the end of the dotted list is a symbol
(deftest ldiff.3
  (let* ((x (copy-tree '(a b c d e . f)))
	 (xcopy (make-scaffold-copy x)))
    (let ((result (ldiff x 'a)))
      (and
       (check-scaffold-copy x xcopy)
       result)))
  (a b c d e . f))

;; Works when the end of the dotted list is a fixnum
(deftest ldiff.4
  (let* ((n 18)
	 (x (list* 'a 'b 'c 18))
	 (xcopy (make-scaffold-copy x)))
    (let ((result (ldiff x n)))
      (and
       (check-scaffold-copy x xcopy)
       result)))
  (a b c))

;; Works when the end of the dotted list is a larger
;; integer (that is eql, but probably not eq).
(deftest ldiff.5
  (let* ((n 18000000000000)
	 (x (list* 'a 'b 'c (1- 18000000000001)))
	 (xcopy (make-scaffold-copy x)))
    (let ((result (ldiff x n)))
      (and
       (check-scaffold-copy x xcopy)
       result)))
  (a b c))

;; Test works when the end of a dotted list is a string
(deftest ldiff.6
  (let* ((n (copy-seq "abcde"))
	 (x (list* 'a 'b 'c n))
	 (xcopy (make-scaffold-copy x)))
    (let ((result (ldiff x n)))
      (if (equal result (list 'a 'b 'c))
	  (check-scaffold-copy x xcopy)
	result)))
  t)

;; Check that having the cdr of a dotted list be string-equal, but
;; not eql, does not result in success
(deftest ldiff.7
  (let* ((n (copy-seq "abcde"))
	 (x (list* 'a 'b 'c n))
	 (xcopy (make-scaffold-copy x)))
    (let ((result (ldiff x (copy-seq n))))
      (if (equal result x)
	  (check-scaffold-copy x xcopy)
	result)))
  t)

;; Check that on failure, the list returned by ldiff is
;; a copy of the list, not the list itself.

(deftest ldiff.8
  (let ((x (list 'a 'b 'c 'd)))
    (let ((result (ldiff x '(e))))
      (and (equal x result)
	   (loop
	    for c1 on x
	    for c2 on result
	    count (eqt c1 c2)))))
  0)

(deftest ldiff.order.1
  (let ((i 0) x y)
    (values
     (ldiff (progn (setf x (incf i))
		   (list* 'a 'b 'c 'd))
	    (progn (setf y (incf i))
		   'd))
     i x y))
  (a b c) 2 1 2)

(def-fold-test ldiff.fold.1 (ldiff '(a b c) 'x))
(def-fold-test ldiff.fold.2 (let ((x '(a b c))) (ldiff x (cddr x))))

;; Error checking

(deftest ldiff.error.1
  (signals-type-error x 10 (ldiff x 'a))
  t)

;; Single atoms are not dotted lists, so the next
;; case should be a type-error
(deftest ldiff.error.2
  (signals-type-error x 'a (ldiff x 'a))
  t)

(deftest ldiff.error.3
  (signals-type-error x (make-array '(10) :initial-element 'a) (ldiff x '(a)))
  t)

(deftest ldiff.error.4
  (signals-type-error x 1.23 (ldiff x t))
  t)

(deftest ldiff.error.5
  (signals-type-error x #\w (ldiff x 'a))
  t)

(deftest ldiff.error.6
  (signals-error (ldiff) program-error)
  t)

(deftest ldiff.error.7
  (signals-error (ldiff nil) program-error)
  t)

(deftest ldiff.error.8
  (signals-error (ldiff nil nil nil) program-error)
  t)

;; Note!  The spec is ambiguous on whether this next test
;; is correct.  The spec says that ldiff should be prepared
;; to signal an error if the list argument is not a proper
;; list or dotted list.  If listp is false, the list argument
;; is neither (atoms are not dotted lists).
;;
;; However, the sample implementation *does* work even if
;; the list argument is an atom.
;;
#|
(defun ldiff-12-body ()
  (loop
   for x in *universe*
   count (and (not (listp x))
	      (not (eqt 'type-error
			(catch-type-error (ldiff x x)))))))

(deftest ldiff-12
  (ldiff-12-body)
  0)
|#
