;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan 25 08:46:58 2003
;;;; Contains: Tests of the ARRAY type specifier

(in-package :cl-test)

;;; Tests of array by itself

(deftest array.1.1
  (notnot-mv (typep #() 'array))
  t)

(deftest array.1.2
  (notnot-mv (typep #0aX 'array))
  t)

(deftest array.1.3
  (notnot-mv (typep #2a(()) 'array))
  t)

(deftest array.1.4
  (notnot-mv (typep #(1 2 3) 'array))
  t)

(deftest array.1.5
  (notnot-mv (typep "abcd" 'array))
  t)

(deftest array.1.6
  (notnot-mv (typep #*010101 'array))
  t)

(deftest array.1.7
  (typep nil 'array)
  nil)

(deftest array.1.8
  (typep 'x 'array)
  nil)

(deftest array.1.9
  (typep '(a b c) 'array)
  nil)

(deftest array.1.10
  (typep 10.0 'array)
  nil)

(deftest array.1.11
  (typep #'(lambda (x) (cons x nil)) 'array)
  nil)

(deftest array.1.12
  (typep 1 'array)
  nil)

(deftest array.1.13
  (typep (1+ most-positive-fixnum) 'array)
  nil)


;;; Tests of (array *)

(deftest array.2.1
  (notnot-mv (typep #() '(array *)))
  t)

(deftest array.2.2
  (notnot-mv (typep #0aX '(array *)))
  t)

(deftest array.2.3
  (notnot-mv (typep #2a(()) '(array *)))
  t)

(deftest array.2.4
  (notnot-mv (typep #(1 2 3) '(array *)))
  t)

(deftest array.2.5
  (notnot-mv (typep "abcd" '(array *)))
  t)

(deftest array.2.6
  (notnot-mv (typep #*010101 '(array *)))
  t)

;;; Tests of (array * ())

(deftest array.3.1
  (notnot-mv (typep #() '(array * nil)))
  nil)

(deftest array.3.2
 (notnot-mv (typep #0aX '(array * nil)))
  t)

(deftest array.3.3
  (typep #2a(()) '(array * nil))
  nil)

(deftest array.3.4
  (typep #(1 2 3) '(array * nil))
  nil)

(deftest array.3.5
  (typep "abcd" '(array * nil))
  nil)

(deftest array.3.6
  (typep #*010101 '(array * nil))
  nil)

;;; Tests of (array * 1)
;;; The '1' indicates rank, so this is equivalent to 'vector'

(deftest array.4.1
  (notnot-mv (typep #() '(array * 1)))
  t)

(deftest array.4.2
  (typep #0aX '(array * 1))
  nil)

(deftest array.4.3
  (typep #2a(()) '(array * 1))
  nil)

(deftest array.4.4
  (notnot-mv (typep #(1 2 3) '(array * 1)))
  t)

(deftest array.4.5
  (notnot-mv (typep "abcd" '(array * 1)))
  t)

(deftest array.4.6
  (notnot-mv (typep #*010101 '(array * 1)))
  t)

;;; Tests of (array * 0)

(deftest array.5.1
  (typep #() '(array * 0))
  nil)

(deftest array.5.2
  (notnot-mv (typep #0aX '(array * 0)))
  t)

(deftest array.5.3
  (typep #2a(()) '(array * 0))
  nil)

(deftest array.5.4
  (typep #(1 2 3) '(array * 0))
  nil)

(deftest array.5.5
  (typep "abcd" '(array * 0))
  nil)

(deftest array.5.6
  (typep #*010101 '(array * 0))
  nil)

;;; Tests of (array * *)

(deftest array.6.1
  (notnot-mv (typep #() '(array * *)))
  t)

(deftest array.6.2
  (notnot-mv (typep #0aX '(array * *)))
  t)

(deftest array.6.3
  (notnot-mv (typep #2a(()) '(array * *)))
  t)

(deftest array.6.4
  (notnot-mv (typep #(1 2 3) '(array * *)))
  t)

(deftest array.6.5
  (notnot-mv (typep "abcd" '(array * *)))
  t)

(deftest array.6.6
  (notnot-mv (typep #*010101 '(array * *)))
  t)

;;; Tests of (array * 2)

(deftest array.7.1
  (typep #() '(array * 2))
  nil)

(deftest array.7.2
  (typep #0aX '(array * 2))
  nil)

(deftest array.7.3
  (notnot-mv (typep #2a(()) '(array * 2)))
  t)

(deftest array.7.4
  (typep #(1 2 3) '(array * 2))
  nil)

(deftest array.7.5
  (typep "abcd" '(array * 2))
  nil)

(deftest array.7.6
  (typep #*010101 '(array * 2))
  nil)

;;; Testing '(array * (--))

(deftest array.8.1
  (typep #() '(array * (1)))
  nil)
	 
(deftest array.8.2
  (notnot-mv (typep #() '(array * (0))))
  t)

(deftest array.8.3
  (notnot-mv (typep #() '(array * (*))))
  t)

(deftest array.8.4
  (typep #(a b c) '(array * (2)))
  nil)
	 
(deftest array.8.5
  (notnot-mv (typep #(a b c) '(array * (3))))
  t)

(deftest array.8.6
  (notnot-mv (typep #(a b c) '(array * (*))))
  t)

(deftest array.8.7
  (typep #(a b c) '(array * (4)))
  nil)

(deftest array.8.8
  (typep #2a((a b c)) '(array * (*)))
  nil)

(deftest array.8.9
  (typep #2a((a b c)) '(array * (3)))
  nil)

(deftest array.8.10
  (typep #2a((a b c)) '(array * (1)))
  nil)

(deftest array.8.11
  (typep "abc" '(array * (2)))
  nil)
	 
(deftest array.8.12
  (notnot-mv (typep "abc" '(array * (3))))
  t)

(deftest array.8.13
  (notnot-mv (typep "abc" '(array * (*))))
  t)

(deftest array.8.14
  (typep "abc" '(array * (4)))
  nil)

;;; Two dimensional array type tests

(deftest array.9.1
  (typep #() '(array * (* *)))
  nil)

(deftest array.9.2
  (typep "abc" '(array * (* *)))
  nil)

(deftest array.9.3
  (typep #(a b c) '(array * (3 *)))
  nil)

(deftest array.9.4
  (typep #(a b c) '(array * (* 3)))
  nil)

(deftest array.9.5
  (typep "abc" '(array * (3 *)))
  nil)

(deftest array.9.6
  (typep "abc" '(array * (* 3)))
  nil)

(deftest array.9.7
  (notnot-mv (typep #2a((a b)(c d)(e f)) '(array * (* *))))
  t)

(deftest array.9.8
  (notnot-mv (typep #2a((a b)(c d)(e f)) '(array * (3 *))))
  t)

(deftest array.9.9
  (typep #2a((a b)(c d)(e f)) '(array * (2 *)))
  nil)

(deftest array.9.10
  (notnot-mv (typep #2a((a b)(c d)(e f)) '(array * (* 2))))
  t)

(deftest array.9.11
  (typep #2a((a b)(c d)(e f)) '(array * (* 3)))
  nil)

(deftest array.9.12
  (notnot-mv (typep #2a((a b)(c d)(e f)) '(array * (3 2))))
  t)

(deftest array.9.13
  (typep #2a((a b)(c d)(e f)) '(array * (2 3)))
  nil)
