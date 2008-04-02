;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan 25 11:55:48 2003
;;;; Contains: Tests of (array t ...) type specifiers

(in-package :cl-test)

;;; Tests of (array t)

(deftest array-t.2.1
  (notnot-mv (typep #() '(array t)))
  t)

(deftest array-t.2.2
  (notnot-mv (typep #0aX '(array t)))
  t)

(deftest array-t.2.3
  (notnot-mv (typep #2a(()) '(array t)))
  t)

(deftest array-t.2.4
  (notnot-mv (typep #(1 2 3) '(array t)))
  t)

(deftest array-t.2.5
  (typep "abcd" '(array t))
  nil)

(deftest array-t.2.6
  (typep #*010101 '(array t))
  nil)

;;; Tests of (array t ())

(deftest array-t.3.1
  (notnot-mv (typep #() '(array t nil)))
  nil)

(deftest array-t.3.2
 (notnot-mv (typep #0aX '(array t nil)))
  t)

(deftest array-t.3.3
  (typep #2a(()) '(array t nil))
  nil)

(deftest array-t.3.4
  (typep #(1 2 3) '(array t nil))
  nil)

(deftest array-t.3.5
  (typep "abcd" '(array t nil))
  nil)

(deftest array-t.3.6
  (typep #*010101 '(array t nil))
  nil)

;;; Tests of (array t 1)
;;; The '1' indicates rank, so this is equivalent to 'vector'

(deftest array-t.4.1
  (notnot-mv (typep #() '(array t 1)))
  t)

(deftest array-t.4.2
  (typep #0aX '(array t 1))
  nil)

(deftest array-t.4.3
  (typep #2a(()) '(array t 1))
  nil)

(deftest array-t.4.4
  (notnot-mv (typep #(1 2 3) '(array t 1)))
  t)

(deftest array-t.4.5
  (typep "abcd" '(array t 1))
  nil)

(deftest array-t.4.6
  (typep #*010101 '(array t 1))
  nil)

;;; Tests of (array t 0)

(deftest array-t.5.1
  (typep #() '(array t 0))
  nil)

(deftest array-t.5.2
  (notnot-mv (typep #0aX '(array t 0)))
  t)

(deftest array-t.5.3
  (typep #2a(()) '(array t 0))
  nil)

(deftest array-t.5.4
  (typep #(1 2 3) '(array t 0))
  nil)

(deftest array-t.5.5
  (typep "abcd" '(array t 0))
  nil)

(deftest array-t.5.6
  (typep #*010101 '(array t 0))
  nil)

;;; Tests of (array t *)

(deftest array-t.6.1
  (notnot-mv (typep #() '(array t *)))
  t)

(deftest array-t.6.2
  (notnot-mv (typep #0aX '(array t *)))
  t)

(deftest array-t.6.3
  (notnot-mv (typep #2a(()) '(array t *)))
  t)

(deftest array-t.6.4
  (notnot-mv (typep #(1 2 3) '(array t *)))
  t)

(deftest array-t.6.5
  (typep "abcd" '(array t *))
  nil)

(deftest array-t.6.6
  (typep #*010101 '(array t *))
  nil)

;;; Tests of (array t 2)

(deftest array-t.7.1
  (typep #() '(array t 2))
  nil)

(deftest array-t.7.2
  (typep #0aX '(array t 2))
  nil)

(deftest array-t.7.3
  (notnot-mv (typep #2a(()) '(array t 2)))
  t)

(deftest array-t.7.4
  (typep #(1 2 3) '(array t 2))
  nil)

(deftest array-t.7.5
  (typep "abcd" '(array t 2))
  nil)

(deftest array-t.7.6
  (typep #*010101 '(array t 2))
  nil)

;;; Testing '(array t (--))

(deftest array-t.8.1
  (typep #() '(array t (1)))
  nil)
	 
(deftest array-t.8.2
  (notnot-mv (typep #() '(array t (0))))
  t)

(deftest array-t.8.3
  (notnot-mv (typep #() '(array t (*))))
  t)

(deftest array-t.8.4
  (typep #(a b c) '(array t (2)))
  nil)
	 
(deftest array-t.8.5
  (notnot-mv (typep #(a b c) '(array t (3))))
  t)

(deftest array-t.8.6
  (notnot-mv (typep #(a b c) '(array t (*))))
  t)

(deftest array-t.8.7
  (typep #(a b c) '(array t (4)))
  nil)

(deftest array-t.8.8
  (typep #2a((a b c)) '(array t (*)))
  nil)

(deftest array-t.8.9
  (typep #2a((a b c)) '(array t (3)))
  nil)

(deftest array-t.8.10
  (typep #2a((a b c)) '(array t (1)))
  nil)

(deftest array-t.8.11
  (typep "abc" '(array t (2)))
  nil)
	 
(deftest array-t.8.12
  (typep "abc" '(array t (3)))
  nil)

(deftest array-t.8.13
  (typep "abc" '(array t (*)))
  nil)

(deftest array-t.8.14
  (typep "abc" '(array t (4)))
  nil)

;;; Two dimensional array type tests

(deftest array-t.9.1
  (typep #() '(array t (* *)))
  nil)

(deftest array-t.9.2
  (typep "abc" '(array t (* *)))
  nil)

(deftest array-t.9.3
  (typep #(a b c) '(array t (3 *)))
  nil)

(deftest array-t.9.4
  (typep #(a b c) '(array t (* 3)))
  nil)

(deftest array-t.9.5
  (typep "abc" '(array t (3 *)))
  nil)

(deftest array-t.9.6
  (typep "abc" '(array t (* 3)))
  nil)

(deftest array-t.9.7
  (notnot-mv (typep #2a((a b)(c d)(e f)) '(array t (* *))))
  t)

(deftest array-t.9.8
  (notnot-mv (typep #2a((a b)(c d)(e f)) '(array t (3 *))))
  t)

(deftest array-t.9.9
  (typep #2a((a b)(c d)(e f)) '(array t (2 *)))
  nil)

(deftest array-t.9.10
  (notnot-mv (typep #2a((a b)(c d)(e f)) '(array t (* 2))))
  t)

(deftest array-t.9.11
  (typep #2a((a b)(c d)(e f)) '(array t (* 3)))
  nil)

(deftest array-t.9.12
  (notnot-mv (typep #2a((a b)(c d)(e f)) '(array t (3 2))))
  t)

(deftest array-t.9.13
  (typep #2a((a b)(c d)(e f)) '(array t (2 3)))
  nil)
