;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:47:26 2003
;;;; Contains: Tests of TAILP

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest tailp.1
  (let ((x (copy-tree '(a b c d e . f))))
    (and
     (tailp x x)
     (tailp (cdr x) x)
     (tailp (cddr x) x)
     (tailp (cdddr x) x)
     (tailp (cddddr x) x)
     t))
  t)

;; The next four tests test that tailp handles dotted lists.  See
;; TAILP-NIL:T in the X3J13 documentation.

(deftest tailp.2
  (notnot-mv (tailp 'e (copy-tree '(a b c d . e))))
  t)

(deftest tailp.3
  (tailp 'z (copy-tree '(a b c d . e)))
  nil)

(deftest tailp.4
  (notnot-mv (tailp 10203040506070
		    (list* 'a 'b (1- 10203040506071))))
  t)

(deftest tailp.5
  (let ((x "abcde")) (tailp x (list* 'a 'b (copy-seq x))))
  nil)

(deftest tailp.error.5
  (signals-error (tailp) program-error)
  t)

(deftest tailp.error.6
  (signals-error (tailp nil) program-error)
  t)

(deftest tailp.error.7
  (signals-error (tailp nil nil nil) program-error)
  t)

;; Test that tailp does not modify its arguments

(deftest tailp.6
    (let* ((x (copy-list '(a b c d e)))
	   (y (cddr x)))
      (let ((xcopy (make-scaffold-copy x))
	    (ycopy (make-scaffold-copy y)))
	(and
	 (tailp y x)
	 (check-scaffold-copy x xcopy)
	 (check-scaffold-copy y ycopy))))
  t)

;; Note!  The spec is ambiguous on whether this next test
;; is correct.  The spec says that tailp should be prepared
;; to signal an error if the list argument is not a proper
;; list or dotted list.  If listp is false, the list argument
;; is neither (atoms are not dotted lists).
;;
;; However, the sample implementation *does* work even if
;; the list argument is an atom.
;;

#|
(defun tailp.7-body ()
  (loop
      for x in *universe*
      count (and (not (listp x))
		 (eqt 'type-error
		     (catch-type-error (tailp x x))))))

(deftest tailp.7
    (tailp.7-body)
  0)
|#
    
(deftest tailp.order.1
  (let ((i 0) x y)
    (values
     (notnot
      (tailp (progn (setf x (incf i)) 'd)
	     (progn (setf y (incf i)) '(a b c . d))))
     i x y))
  t 2 1 2)

