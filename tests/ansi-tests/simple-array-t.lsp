;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 07:23:45 2003
;;;; Contains: Tests of SIMPLE-ARRAY on T element type

(in-package :cl-test)

;;; Tests of (simple-array t)

(deftest simple-array-t.2.1
  (notnot-mv (typep #() '(simple-array t)))
  t)

(deftest simple-array-t.2.2
  (notnot-mv (typep #0aX '(simple-array t)))
  t)

(deftest simple-array-t.2.3
  (notnot-mv (typep #2a(()) '(simple-array t)))
  t)

(deftest simple-array-t.2.4
  (notnot-mv (typep #(1 2 3) '(simple-array t)))
  t)

(deftest simple-array-t.2.5
  (typep "abcd" '(simple-array t))
  nil)

(deftest simple-array-t.2.6
  (typep #*010101 '(simple-array t))
  nil)

;;; Tests of (simple-array t ())

(deftest simple-array-t.3.1
  (notnot-mv (typep #() '(simple-array t nil)))
  nil)

(deftest simple-array-t.3.2
 (notnot-mv (typep #0aX '(simple-array t nil)))
  t)

(deftest simple-array-t.3.3
  (typep #2a(()) '(simple-array t nil))
  nil)

(deftest simple-array-t.3.4
  (typep #(1 2 3) '(simple-array t nil))
  nil)

(deftest simple-array-t.3.5
  (typep "abcd" '(simple-array t nil))
  nil)

(deftest simple-array-t.3.6
  (typep #*010101 '(simple-array t nil))
  nil)

;;; Tests of (simple-array t 1)
;;; The '1' indicates rank, so this is equivalent to 'vector'

(deftest simple-array-t.4.1
  (notnot-mv (typep #() '(simple-array t 1)))
  t)

(deftest simple-array-t.4.2
  (typep #0aX '(simple-array t 1))
  nil)

(deftest simple-array-t.4.3
  (typep #2a(()) '(simple-array t 1))
  nil)

(deftest simple-array-t.4.4
  (notnot-mv (typep #(1 2 3) '(simple-array t 1)))
  t)

(deftest simple-array-t.4.5
  (typep "abcd" '(simple-array t 1))
  nil)

(deftest simple-array-t.4.6
  (typep #*010101 '(simple-array t 1))
  nil)

;;; Tests of (simple-array t 0)

(deftest simple-array-t.5.1
  (typep #() '(simple-array t 0))
  nil)

(deftest simple-array-t.5.2
  (notnot-mv (typep #0aX '(simple-array t 0)))
  t)

(deftest simple-array-t.5.3
  (typep #2a(()) '(simple-array t 0))
  nil)

(deftest simple-array-t.5.4
  (typep #(1 2 3) '(simple-array t 0))
  nil)

(deftest simple-array-t.5.5
  (typep "abcd" '(simple-array t 0))
  nil)

(deftest simple-array-t.5.6
  (typep #*010101 '(simple-array t 0))
  nil)

;;; Tests of (simple-array t *)

(deftest simple-array-t.6.1
  (notnot-mv (typep #() '(simple-array t *)))
  t)

(deftest simple-array-t.6.2
  (notnot-mv (typep #0aX '(simple-array t *)))
  t)

(deftest simple-array-t.6.3
  (notnot-mv (typep #2a(()) '(simple-array t *)))
  t)

(deftest simple-array-t.6.4
  (notnot-mv (typep #(1 2 3) '(simple-array t *)))
  t)

(deftest simple-array-t.6.5
  (typep "abcd" '(simple-array t *))
  nil)

(deftest simple-array-t.6.6
  (typep #*010101 '(simple-array t *))
  nil)

;;; Tests of (simple-array t 2)

(deftest simple-array-t.7.1
  (typep #() '(simple-array t 2))
  nil)

(deftest simple-array-t.7.2
  (typep #0aX '(simple-array t 2))
  nil)

(deftest simple-array-t.7.3
  (notnot-mv (typep #2a(()) '(simple-array t 2)))
  t)

(deftest simple-array-t.7.4
  (typep #(1 2 3) '(simple-array t 2))
  nil)

(deftest simple-array-t.7.5
  (typep "abcd" '(simple-array t 2))
  nil)

(deftest simple-array-t.7.6
  (typep #*010101 '(simple-array t 2))
  nil)

;;; Testing '(simple-array t (--))

(deftest simple-array-t.8.1
  (typep #() '(simple-array t (1)))
  nil)
	 
(deftest simple-array-t.8.2
  (notnot-mv (typep #() '(simple-array t (0))))
  t)

(deftest simple-array-t.8.3
  (notnot-mv (typep #() '(simple-array t (*))))
  t)

(deftest simple-array-t.8.4
  (typep #(a b c) '(simple-array t (2)))
  nil)
	 
(deftest simple-array-t.8.5
  (notnot-mv (typep #(a b c) '(simple-array t (3))))
  t)

(deftest simple-array-t.8.6
  (notnot-mv (typep #(a b c) '(simple-array t (*))))
  t)

(deftest simple-array-t.8.7
  (typep #(a b c) '(simple-array t (4)))
  nil)

(deftest simple-array-t.8.8
  (typep #2a((a b c)) '(simple-array t (*)))
  nil)

(deftest simple-array-t.8.9
  (typep #2a((a b c)) '(simple-array t (3)))
  nil)

(deftest simple-array-t.8.10
  (typep #2a((a b c)) '(simple-array t (1)))
  nil)

(deftest simple-array-t.8.11
  (typep "abc" '(simple-array t (2)))
  nil)
	 
(deftest simple-array-t.8.12
  (typep "abc" '(simple-array t (3)))
  nil)

(deftest simple-array-t.8.13
  (typep "abc" '(simple-array t (*)))
  nil)

(deftest simple-array-t.8.14
  (typep "abc" '(simple-array t (4)))
  nil)

;;; Two dimensional simple-array type tests

(deftest simple-array-t.9.1
  (typep #() '(simple-array t (* *)))
  nil)

(deftest simple-array-t.9.2
  (typep "abc" '(simple-array t (* *)))
  nil)

(deftest simple-array-t.9.3
  (typep #(a b c) '(simple-array t (3 *)))
  nil)

(deftest simple-array-t.9.4
  (typep #(a b c) '(simple-array t (* 3)))
  nil)

(deftest simple-array-t.9.5
  (typep "abc" '(simple-array t (3 *)))
  nil)

(deftest simple-array-t.9.6
  (typep "abc" '(simple-array t (* 3)))
  nil)

(deftest simple-array-t.9.7
  (notnot-mv (typep #2a((a b)(c d)(e f)) '(simple-array t (* *))))
  t)

(deftest simple-array-t.9.8
  (notnot-mv (typep #2a((a b)(c d)(e f)) '(simple-array t (3 *))))
  t)

(deftest simple-array-t.9.9
  (typep #2a((a b)(c d)(e f)) '(simple-array t (2 *)))
  nil)

(deftest simple-array-t.9.10
  (notnot-mv (typep #2a((a b)(c d)(e f)) '(simple-array t (* 2))))
  t)

(deftest simple-array-t.9.11
  (typep #2a((a b)(c d)(e f)) '(simple-array t (* 3)))
  nil)

(deftest simple-array-t.9.12
  (notnot-mv (typep #2a((a b)(c d)(e f)) '(simple-array t (3 2))))
  t)

(deftest simple-array-t.9.13
  (typep #2a((a b)(c d)(e f)) '(simple-array t (2 3)))
  nil)
