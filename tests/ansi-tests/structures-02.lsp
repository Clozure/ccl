;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May  3 22:46:54 1998
;;;; Contains: Test code for structures, part 02

(in-package :cl-test)
(declaim (optimize (safety 3)))

;; Test initializers for fields

(defvar *s-2-f6-counter* 0)

(defstruct s-2
  (f1 0)
  (f2 'a)
  (f3 1.21)
  (f4 #\d)
  (f5 (list 'a 'b))
  (f6 (incf *s-2-f6-counter*)))

;; Standard structure tests


;; Fields have appropriate values
(deftest structure-2-1
  (let ((*s-2-f6-counter* 0))
    (let ((s (make-s-2)))
      (and
       (eqlt (s-2-f1 s) 0)
       (eqt  (s-2-f2 s) 'a)
       (= (s-2-f3 s) 1.21)
       (eqlt (s-2-f4 s) #\d)
       (equalt (s-2-f5 s) '(a b))
       (eqlt (s-2-f6 s) *s-2-f6-counter*)
       (eqlt *s-2-f6-counter* 1))))
  t)

;; Two successive invocations of make-s-2 return different objects
(deftest structure-2-2
  (let ((*s-2-f6-counter* 0))
    (eqt (s-2-f5 (make-s-2))
	 (s-2-f5 (make-s-2))))
  nil)

;; Creation with various fields does the right thing
(deftest structure-2-3
  (let* ((*s-2-f6-counter* 0)
	 (s (make-s-2 :f1 17)))
    (and
     (eqlt (s-2-f1 s) 17)
     (eqt  (s-2-f2 s) 'a)
     (= (s-2-f3 s) 1.21)
     (eqlt (s-2-f4 s) #\d)
     (equalt (s-2-f5 s) '(a b))
     (eqlt (s-2-f6 s) *s-2-f6-counter*)
     (eqlt *s-2-f6-counter* 1)))
  t)

(deftest structure-2-4
  (let* ((*s-2-f6-counter* 0)
	 (s (make-s-2 :f2 'z)))
    (and
     (eqlt (s-2-f1 s) 0)
     (eqt  (s-2-f2 s) 'z)
     (= (s-2-f3 s) 1.21)
     (eqlt (s-2-f4 s) #\d)
     (equalt (s-2-f5 s) '(a b))
     (eqlt (s-2-f6 s) *s-2-f6-counter*)
     (eqlt *s-2-f6-counter* 1)))
  t)

(deftest structure-2-5
  (let* ((*s-2-f6-counter* 0)
	 (s (make-s-2 :f3 1.0)))
    (and
     (eqlt (s-2-f1 s) 0)
     (eqt  (s-2-f2 s) 'a)
     (= (s-2-f3 s) 1.0)
     (eqlt (s-2-f4 s) #\d)
     (equalt (s-2-f5 s) '(a b))
     (eqlt (s-2-f6 s) *s-2-f6-counter*)
     (eqlt *s-2-f6-counter* 1)))
  t)

(deftest structure-2-6
  (let* ((*s-2-f6-counter* 0)
	 (s (make-s-2 :f4 #\z)))
    (and
     (eqlt (s-2-f1 s) 0)
     (eqt  (s-2-f2 s) 'a)
     (= (s-2-f3 s) 1.21)
     (eqlt (s-2-f4 s) #\z)
     (equalt (s-2-f5 s) '(a b))
     (eqlt (s-2-f6 s) *s-2-f6-counter*)
     (eqlt *s-2-f6-counter* 1)))
  t)

(deftest structure-2-7
  (let* ((*s-2-f6-counter* 0)
	 (s (make-s-2 :f5 '(c d e))))
    (and
     (eqlt (s-2-f1 s) 0)
     (eqt  (s-2-f2 s) 'a)
     (= (s-2-f3 s) 1.21)
     (eqlt (s-2-f4 s) #\d)
     (equalt (s-2-f5 s) '(c d e))
     (eqlt (s-2-f6 s) *s-2-f6-counter*)
     (eqlt *s-2-f6-counter* 1)))
  t)

(deftest structure-2-8
  (let* ((*s-2-f6-counter* 0)
	 (s (make-s-2 :f6 10)))
    (and
     (eqlt (s-2-f1 s) 0)
     (eqt  (s-2-f2 s) 'a)
     (= (s-2-f3 s) 1.21)
     (eqlt (s-2-f4 s) #\d)
     (equalt (s-2-f5 s) '(a b))
     (eqlt (s-2-f6 s) 10)
     (eqlt *s-2-f6-counter* 0)))
  t)

;;; Tests using the defstruct-with-tests infrastructure

(defstruct-with-tests struct-test-03 a b c d)

(defstruct-with-tests (struct-test-04) a b c)

(defstruct-with-tests (struct-test-05 :constructor) a05 b05 c05)
(defstruct-with-tests (struct-test-06 (:constructor)) a06 b06 c06)

(defstruct-with-tests (struct-test-07 :conc-name) a07 b07)
(defstruct-with-tests (struct-test-08 (:conc-name)) a08 b08)
(defstruct-with-tests (struct-test-09 (:conc-name nil)) a09 b09)
(defstruct-with-tests (struct-test-10 (:conc-name "")) a10 b10)
(defstruct-with-tests (struct-test-11 (:conc-name "BLAH-")) a11 b11)
(defstruct-with-tests (struct-test-12 (:conc-name BLAH-)) a12 b12)
(defstruct-with-tests (struct-test-13 (:conc-name #\X)) foo-a13 foo-b13)

(defstruct-with-tests (struct-test-14 (:predicate)) a14 b14)
(defstruct-with-tests (struct-test-15 (:predicate nil)) a15 b15)
(defstruct-with-tests (struct-test-16 :predicate) a16 b16)
(defstruct-with-tests (struct-test-17
		       (:predicate struct-test-17-alternate-pred))
  a17 b17)

(defstruct-with-tests (struct-test-18 :copier) a18 b18)
(defstruct-with-tests (struct-test-19 (:copier)) a19 b19)
(defstruct-with-tests (struct-test-20 (:copier nil)) a20 b20)
(defstruct-with-tests (struct-test-21 (:copier struct-test-21-alt-copier))
  a21 b21)

(defstruct-with-tests struct-test-22 (a22) (b22))
(defstruct-with-tests struct-test-23 (a23 1) (b23 2))
(defstruct-with-tests struct-test-24
  (a24 1 :type fixnum)
  (b24 2 :type integer))

(defstruct-with-tests struct-test-25)
(defstruct-with-tests struct-test-26
  (a26 nil :read-only nil)
  (b26 'a  :read-only nil))

(defstruct-with-tests struct-test-27
  (a27 1    :read-only t)
  (b27 1.4  :read-only a))

(defstruct-with-tests struct-test-28
  (a28 1    :type integer :read-only t)
  (b28 'xx  :read-only a :type symbol))

(defstruct-with-tests struct-test-29
  a29
  (b29 'xx  :read-only 1)
  c29)

(defstruct-with-tests struct-test-30 #:a30 #:b30)
(defstruct-with-tests #:struct-test-31 a31 b31)

(defpackage struct-test-package (:use))

(defstruct-with-tests struct-test-32
  struct-test-package::a32 struct-test-package::b32)

;;; If the :conc-name option is given no argument or
;;; a nil argument, the accessor names are the same as
;;; slot names.  Note that this is different from prepending
;;; an empty string, since that may get you a name in
;;; a different package.

(defstruct-with-tests (struct-test-33 (:conc-name))
  struct-test-package::a33 struct-test-package::b33)
(defstruct-with-tests (struct-test-34 :conc-name)
  struct-test-package::a34 struct-test-package::b34)
(defstruct-with-tests (struct-test-35 (:conc-name nil))
  struct-test-package::a35 struct-test-package::b35)

(defstruct-with-tests (struct-test-36 (:conc-name ""))
  struct-test-package::st36-a36 struct-test-package::st26-b36)

;;; List and vector structures

(defstruct-with-tests (struct-test-37 (:type list)) a37 b37 c37)

(deftest structure-37-1
  (make-struct-test-37 :a37 1 :b37 2 :c37 4)
  (1 2 4))

(defstruct-with-tests (struct-test-38 (:type list) :named) a38 b38 c38)

(deftest structure-38-1
  (make-struct-test-38 :a38 11 :b38 12 :c38 4)
  (struct-test-38 11 12 4))

(defstruct-with-tests (struct-test-39 (:predicate nil)
				      (:type list) :named)
  a39 b39 c39)

(deftest structure-39-1
  (make-struct-test-39 :a39 11 :b39 12 :c39 4)
  (struct-test-39 11 12 4))

(defstruct-with-tests (struct-test-40 (:type vector)) a40 b40)
(defstruct-with-tests (struct-test-41 (:type vector) :named) a41 b41)
(defstruct-with-tests (struct-test-42 (:type (vector t))) a42 b42)
(defstruct-with-tests (struct-test-43 (:type (vector t)) :named) a43 b43)

(defstruct-with-tests (struct-test-44 (:type list))
  (a44 0 :type integer)
  (b44 'a :type symbol))

;;; Confirm that the defined structure types are all disjoint
(deftest structs-are-disjoint
  (loop for s1 in *defstruct-with-tests-names*
	sum (loop for s2 in *defstruct-with-tests-names*
		  unless (eq s1 s2)
		  count (not (equalt (multiple-value-list
				      (subtypep* s1 s2))
				     '(nil t)))))
  0)

(defstruct-with-tests (struct-test-45 (:type list) (:initial-offset 2))
  a45 b45)

(deftest structure-45-1
  (cddr (make-struct-test-45 :a45 1 :b45 2))
  (1 2))

(defstruct-with-tests (struct-test-46 (:type list)
				      (:include struct-test-45))
  c46 d46)

(deftest structure-46-1
  (cddr (make-struct-test-46 :a45 1 :b45 2 :c46 3 :d46 4))
  (1 2 3 4))

(defstruct-with-tests (struct-test-47 (:type list)
				      (:initial-offset 3)
				      (:include struct-test-45))
  c47 d47)

(deftest structure-47-1
  (let ((s (make-struct-test-47 :a45 1 :b45 2 :c47 3 :d47 4)))
    (values (third s) (fourth s) (eighth s) (ninth s)))
  1 2 3 4)

(defstruct-with-tests (struct-test-48 (:type list)
				      (:initial-offset 0)
				      (:include struct-test-45))
  c48 d48)

(deftest structure-48-1
  (cddr (make-struct-test-48 :a45 1 :b45 2 :c48 3 :d48 4))
  (1 2 3 4))

(defstruct-with-tests (struct-test-49 (:type (vector bit)))
  (a49 0 :type bit)
  (b49 0 :type bit))

(defstruct-with-tests (struct-test-50 (:type (vector character)))
  (a50 #\g :type character)
  (b50 #\k :type character))

(defstruct-with-tests (struct-test-51 (:type (vector (integer 0 255))))
  (a51 17 :type (integer 0 255))
  (b51 25 :type (integer 0 255)))

(defstruct-with-tests (struct-test-52 (:type vector)
				      (:initial-offset 0))
  a52 b52)

(defstruct-with-tests (struct-test-53 (:type vector)
				      (:initial-offset 5))
  "This is struct-test-53"
  a53 b53)

(deftest structure-53-1
  (let ((s (make-struct-test-53 :a53 10 :b53 'a)))
    (values (my-aref s 5) (my-aref s 6)))
  10 a)

(defstruct-with-tests (struct-test-54 (:type vector)
				      (:initial-offset 2)
				      (:include struct-test-53))
  "This is struct-test-54"
  a54 b54)

(deftest structure-54-1
  (let ((s (make-struct-test-54 :a53 8 :b53 'g :a54 10 :b54 'a)))
    (values (my-aref s 5) (my-aref s 6) (my-aref s 9) (my-aref s 10)))
  8 g 10 a)

(defstruct-with-tests (struct-test-55 (:type list)
				      (:initial-offset 2)
				      :named)
  a55 b55 c55)

(deftest structure-55-1
  (let ((s (make-struct-test-55 :a55 'p :c55 'q)))
    (values (third s) (fourth s) (sixth s)))
  struct-test-55 p q)

(defstruct-with-tests (struct-test-56 (:type list)
				      (:initial-offset 3)
				      (:include struct-test-55)
				      :named)
  d56 e56)

(deftest structure-56-1
  (let ((s (make-struct-test-56 :a55 3 :b55 7 :d56 'x :e56 'y)))
    (mapcar #'(lambda (i) (nth i s)) '(2 3 4 9 10 11)))
  (struct-test-55 3 7 struct-test-56 x y))

(defstruct-with-tests (struct-test-57 (:include struct-test-22))
  c57 d57)

(defstruct-with-tests struct-test-58
  "This is struct-test-58"  a-58 b-58)

(defstruct-with-tests (struct-test-59 (:include struct-test-58))
  "This is struct-test-59"  a-59 b-59)

;;; When a field name of a structure is also a special variable,
;;; the constructor must not bind that name.

(defvar *st-60* 100)

(defstruct-with-tests struct-test-60
  (a60 *st-60* :type integer)
  (*st-60* 0 :type integer)
  (b60 *st-60* :type integer))

(deftest structure-60-1
  (let ((*st-60* 10))
    (let ((s (make-struct-test-60 :*st-60* 200)))
      (values (struct-test-60-a60 s)
	      (struct-test-60-*st-60* s)
	      (struct-test-60-b60 s))))
  10 200 10)


;;; When default initializers of the wrong type are given, they do not
;;; cause an error unless actually invoked

(defstruct struct-test-61
  (a nil :type integer)
  (b 0 :type symbol))

(deftest structure-61-1
  (let ((s (make-struct-test-61 :a 10 :b 'c)))
    (values (struct-test-61-a s)
	    (struct-test-61-b s)))
  10 c)

;;; Initializer forms are evaluated only when needed, and are
;;; evaluated in the lexical environment in which they were defined

(eval-when (:load-toplevel :execute)
  (let ((x nil))
    (flet ((%f () x)
	  (%g (y) (setf x y)))
      (defstruct struct-test-62
	(a (progn (setf x 'a) nil))
	(f #'%f)
	(g #'%g)))))

(deftest structure-62-1
  (let* ((s (make-struct-test-62 :a 1))
	 (f (struct-test-62-f s)))
    (assert (typep f 'function))
    (values
     (struct-test-62-a s)
     (funcall (the function f))))
  1 nil)

(deftest structure-62-2
  (let* ((s (make-struct-test-62))
	 (f (struct-test-62-f s))
	 (g (struct-test-62-g s)))
    (assert (typep f 'function))
    (assert (typep g 'function))
    (locally
     (declare (type function f g))
     (values
      (struct-test-62-a s)
      (funcall f)
      (funcall g nil)
      (funcall f))))
  nil a nil nil)

;;; Keywords are allowed in defstruct
(defstruct-with-tests :struct-test-63 a63 b63 c63)
(defstruct-with-tests struct-test-64 :a63 :b63 :c63)

(defstruct-with-tests struct-test-65
    array-dimension-limit
    array-rank-limit
    array-total-size-limit
    boole-1
    boole-2
    boole-and
    boole-andc1
    boole-andc2
    boole-c1
    boole-c2
    boole-clr
    boole-eqv
    boole-ior
    boole-nand
    boole-nor
    boole-orc1
    boole-orc2
    boole-set
    boole-xor
    call-arguments-limit
    char-code-limit
    double-float-epsilon
    double-float-negative-epsilon
    internal-time-units-per-second
    lambda-list-keywords
    lambda-parameters-limit
    least-negative-double-float
    least-negative-long-float
    least-negative-normalized-double-float
    least-negative-normalized-long-float
    least-negative-normalized-short-float
    least-negative-normalized-single-float
    least-negative-short-float
    least-negative-single-float
    least-positive-double-float
    least-positive-long-float
    least-positive-normalized-double-float
    least-positive-normalized-long-float
    least-positive-normalized-short-float
    least-positive-normalized-single-float
    least-positive-short-float
    least-positive-single-float
    long-float-epsilon
    long-float-negative-epsilon
    most-negative-double-float
    most-negative-fixnum
    most-negative-long-float
    most-negative-short-float
    most-negative-single-float
    most-positive-double-float
    most-positive-fixnum
    most-positive-long-float
    most-positive-short-float
    most-positive-single-float
    multiple-values-limit
    pi
    short-float-epsilon
    short-float-negative-epsilon
    single-float-epsilon
    single-float-negative-epsilon
    t)

(defstruct-with-tests struct-test-66 nil)

(defstruct-with-tests struct-test-67
  (a 0 :type (integer 0 (#.(ash 1 32))))
  (b nil))

(defstruct-with-tests (struct-test-68 (:include struct-test-67))
  c d)  

;;; Error tests

(deftest copy-structure.error.1
  (signals-error (copy-structure) program-error)
  t)

(deftest copy-structure.error.2
  (signals-error (copy-structure (make-s-2) nil) program-error)
  t)

