;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 07:20:31 2003
;;;; Contains: Tests of SIMPLE-ARRAY

(in-package :cl-test)

;;; Tests of simple-array by itself

(deftest simple-array.1.1
  (notnot-mv (typep #() 'simple-array))
  t)

(deftest simple-array.1.2
  (notnot-mv (typep #0aX 'simple-array))
  t)

(deftest simple-array.1.3
  (notnot-mv (typep #2a(()) 'simple-array))
  t)

(deftest simple-array.1.4
  (notnot-mv (typep #(1 2 3) 'simple-array))
  t)

(deftest simple-array.1.5
  (notnot-mv (typep "abcd" 'simple-array))
  t)

(deftest simple-array.1.6
  (notnot-mv (typep #*010101 'simple-array))
  t)

(deftest simple-array.1.7
  (typep nil 'simple-array)
  nil)

(deftest simple-array.1.8
  (typep 'x 'simple-array)
  nil)

(deftest simple-array.1.9
  (typep '(a b c) 'simple-array)
  nil)

(deftest simple-array.1.10
  (typep 10.0 'simple-array)
  nil)

(deftest simple-array.1.11
  (typep #'(lambda (x) (cons x nil)) 'simple-array)
  nil)

(deftest simple-array.1.12
  (typep 1 'simple-array)
  nil)

(deftest simple-array.1.13
  (typep (1+ most-positive-fixnum) 'simple-array)
  nil)

;;; Tests of (simple-array *)

(deftest simple-array.2.1
  (notnot-mv (typep #() '(simple-array *)))
  t)

(deftest simple-array.2.2
  (notnot-mv (typep #0aX '(simple-array *)))
  t)

(deftest simple-array.2.3
  (notnot-mv (typep #2a(()) '(simple-array *)))
  t)

(deftest simple-array.2.4
  (notnot-mv (typep #(1 2 3) '(simple-array *)))
  t)

(deftest simple-array.2.5
  (notnot-mv (typep "abcd" '(simple-array *)))
  t)

(deftest simple-array.2.6
  (notnot-mv (typep #*010101 '(simple-array *)))
  t)

;;; Tests of (simple-array * ())

(deftest simple-array.3.1
  (notnot-mv (typep #() '(simple-array * nil)))
  nil)

(deftest simple-array.3.2
 (notnot-mv (typep #0aX '(simple-array * nil)))
  t)

(deftest simple-array.3.3
  (typep #2a(()) '(simple-array * nil))
  nil)

(deftest simple-array.3.4
  (typep #(1 2 3) '(simple-array * nil))
  nil)

(deftest simple-array.3.5
  (typep "abcd" '(simple-array * nil))
  nil)

(deftest simple-array.3.6
  (typep #*010101 '(simple-array * nil))
  nil)

;;; Tests of (simple-array * 1)
;;; The '1' indicates rank, so this is equivalent to 'vector'

(deftest simple-array.4.1
  (notnot-mv (typep #() '(simple-array * 1)))
  t)

(deftest simple-array.4.2
  (typep #0aX '(simple-array * 1))
  nil)

(deftest simple-array.4.3
  (typep #2a(()) '(simple-array * 1))
  nil)

(deftest simple-array.4.4
  (notnot-mv (typep #(1 2 3) '(simple-array * 1)))
  t)

(deftest simple-array.4.5
  (notnot-mv (typep "abcd" '(simple-array * 1)))
  t)

(deftest simple-array.4.6
  (notnot-mv (typep #*010101 '(simple-array * 1)))
  t)

;;; Tests of (simple-array * 0)

(deftest simple-array.5.1
  (typep #() '(simple-array * 0))
  nil)

(deftest simple-array.5.2
  (notnot-mv (typep #0aX '(simple-array * 0)))
  t)

(deftest simple-array.5.3
  (typep #2a(()) '(simple-array * 0))
  nil)

(deftest simple-array.5.4
  (typep #(1 2 3) '(simple-array * 0))
  nil)

(deftest simple-array.5.5
  (typep "abcd" '(simple-array * 0))
  nil)

(deftest simple-array.5.6
  (typep #*010101 '(simple-array * 0))
  nil)

;;; Tests of (simple-array * *)

(deftest simple-array.6.1
  (notnot-mv (typep #() '(simple-array * *)))
  t)

(deftest simple-array.6.2
  (notnot-mv (typep #0aX '(simple-array * *)))
  t)

(deftest simple-array.6.3
  (notnot-mv (typep #2a(()) '(simple-array * *)))
  t)

(deftest simple-array.6.4
  (notnot-mv (typep #(1 2 3) '(simple-array * *)))
  t)

(deftest simple-array.6.5
  (notnot-mv (typep "abcd" '(simple-array * *)))
  t)

(deftest simple-array.6.6
  (notnot-mv (typep #*010101 '(simple-array * *)))
  t)

;;; Tests of (simple-array * 2)

(deftest simple-array.7.1
  (typep #() '(simple-array * 2))
  nil)

(deftest simple-array.7.2
  (typep #0aX '(simple-array * 2))
  nil)

(deftest simple-array.7.3
  (notnot-mv (typep #2a(()) '(simple-array * 2)))
  t)

(deftest simple-array.7.4
  (typep #(1 2 3) '(simple-array * 2))
  nil)

(deftest simple-array.7.5
  (typep "abcd" '(simple-array * 2))
  nil)

(deftest simple-array.7.6
  (typep #*010101 '(simple-array * 2))
  nil)

;;; Testing '(simple-array * (--))

(deftest simple-array.8.1
  (typep #() '(simple-array * (1)))
  nil)
	 
(deftest simple-array.8.2
  (notnot-mv (typep #() '(simple-array * (0))))
  t)

(deftest simple-array.8.3
  (notnot-mv (typep #() '(simple-array * (*))))
  t)

(deftest simple-array.8.4
  (typep #(a b c) '(simple-array * (2)))
  nil)
	 
(deftest simple-array.8.5
  (notnot-mv (typep #(a b c) '(simple-array * (3))))
  t)

(deftest simple-array.8.6
  (notnot-mv (typep #(a b c) '(simple-array * (*))))
  t)

(deftest simple-array.8.7
  (typep #(a b c) '(simple-array * (4)))
  nil)

(deftest simple-array.8.8
  (typep #2a((a b c)) '(simple-array * (*)))
  nil)

(deftest simple-array.8.9
  (typep #2a((a b c)) '(simple-array * (3)))
  nil)

(deftest simple-array.8.10
  (typep #2a((a b c)) '(simple-array * (1)))
  nil)

(deftest simple-array.8.11
  (typep "abc" '(simple-array * (2)))
  nil)
	 
(deftest simple-array.8.12
  (notnot-mv (typep "abc" '(simple-array * (3))))
  t)

(deftest simple-array.8.13
  (notnot-mv (typep "abc" '(simple-array * (*))))
  t)

(deftest simple-array.8.14
  (typep "abc" '(simple-array * (4)))
  nil)

;;; Two dimensional simple-array type tests

(deftest simple-array.9.1
  (typep #() '(simple-array * (* *)))
  nil)

(deftest simple-array.9.2
  (typep "abc" '(simple-array * (* *)))
  nil)

(deftest simple-array.9.3
  (typep #(a b c) '(simple-array * (3 *)))
  nil)

(deftest simple-array.9.4
  (typep #(a b c) '(simple-array * (* 3)))
  nil)

(deftest simple-array.9.5
  (typep "abc" '(simple-array * (3 *)))
  nil)

(deftest simple-array.9.6
  (typep "abc" '(simple-array * (* 3)))
  nil)

(deftest simple-array.9.7
  (notnot-mv (typep #2a((a b)(c d)(e f)) '(simple-array * (* *))))
  t)

(deftest simple-array.9.8
  (notnot-mv (typep #2a((a b)(c d)(e f)) '(simple-array * (3 *))))
  t)

(deftest simple-array.9.9
  (typep #2a((a b)(c d)(e f)) '(simple-array * (2 *)))
  nil)

(deftest simple-array.9.10
  (notnot-mv (typep #2a((a b)(c d)(e f)) '(simple-array * (* 2))))
  t)

(deftest simple-array.9.11
  (typep #2a((a b)(c d)(e f)) '(simple-array * (* 3)))
  nil)

(deftest simple-array.9.12
  (notnot-mv (typep #2a((a b)(c d)(e f)) '(simple-array * (3 2))))
  t)

(deftest simple-array.9.13
  (typep #2a((a b)(c d)(e f)) '(simple-array * (2 3)))
  nil)
