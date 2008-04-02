;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct  7 07:34:29 2002
;;;; Contains: Tests for type FUNCTION and the special form FUNCTION

(in-package :cl-test)

;;;
;;; Note! There are significant incompatibilities between CLTL1 and ANSI CL
;;; in the meaning of FUNCTION and FUNCTIONP.
;;;

(deftest function.1
  (typep nil 'function)
  nil)

;;; The next test demonstrates an incompatibility between CLtL1 and ANSI CL.
;;; In ANSI CL, symbols are no longer of type FUNCTION.
(deftest function.2
  (typep 'identity 'function)
  nil)

(deftest function.3
  (not-mv (typep #'identity 'function))
  nil)

(deftest function.4
  (loop for x in *cl-symbol-names*
	for s = (find-symbol x "CL")
	for f = (and (fboundp s)
		     (symbol-function s)
		     (not (special-operator-p s))
		     (not (macro-function s))
		     (symbol-function s))
	unless (or (null f)
		   (typep f 'function))
	collect x)
  nil)

(deftest function.5
  (typep '(setf car) 'function)
  nil)

;;; The next test demonstrates an incompatibility between CLtL1 and ANSI CL.
;;; In ANSI CL, lambda forms are no longer of type FUNCTION.
(deftest function.6
  (typep '(lambda (x) x) 'function)
  nil)

(report-and-ignore-errors
 (defun (setf function-7-accessor) (y x) (setf (car x) y) y))

(deftest function.7
  (not-mv (typep #'(setf function-7-accessor) 'function))
  nil)

(deftest function.8
  (not-mv (typep #'(lambda (x) x) 'function))
  nil)

(deftest function.9
  (not-mv (typep (compile nil '(lambda (x) x)) 'function))
  nil)

;;; The next test demonstrates an incompatibility between CLtL1 and ANSI CL.
;;; In ANSI CL, symbols and cons can no longer also be of type FUNCTION.
(deftest function.10
  (check-predicate (typef '(not (and (or number character symbol
					 cons array)
				     function))))
  nil)

(deftest function.11
  (flet ((%f () nil)) (typep '%f 'function))
  nil)

(deftest function.12
  (flet ((%f () nil)) (not-mv (typep #'%f 'function)))
  nil)

(deftest function.13
  (labels ((%f () nil)) (not-mv (typep #'%f 'function)))
  nil)

;;; "If name is a function name, the functional definition of that
;;; name is that established by the innermost lexically enclosing flet,
;;; labels, or macrolet form, if there is one." (page for FUNCTION, sec. 5.3)
;;;            ^^^^^^^^
;;;(deftest function.14
;;;  (macrolet ((%f () nil)) (not-mv (typep #'%f 'function)))
;;;  nil)

;;; Tests of FUNCTION type specifiers

(deftest function.14
  (flet ((%f () nil))
    (declare (optimize safety debug))
    (let ((f #'%f))
      (declare (type (function () null) f))
      (funcall f)))
  nil)

(deftest function.15
  (flet ((%f (x) (declare (ignore x)) nil))
    (declare (ftype (function (nil) nil) %f))
    :good)
  :good)

(deftest function.16
  (flet ((%f (x) (declare (ignore x)) nil))
    (declare (ftype (function (t) null) %f))
    (values
     (%f 'a)
     (locally (declare (ftype (function (integer) t) %f))
	      (%f 10))
     (%f 'b)))
  nil nil nil)

(deftest function.17
  (flet ((%f (&optional x) x))
    (declare (ftype (function (&optional integer) t) %f))
    (values (%f) (%f 10) (%f) (%f (1+ most-positive-fixnum))))
  nil 10 nil #.(1+ most-positive-fixnum))

(deftest function.18
  (flet ((%f (&rest x) x))
    (declare (ftype (function (&rest symbol) t) %f))
    (values (%f) (%f 'a) (%f 'a 'b 'c)))
  () (a) (a b c))

(deftest function.19
  (flet ((%f (&key foo bar) (list foo bar)))
    (declare (ftype (function (&key (:foo t) (:bar t)) list) %f))
    (values
     (%f) (%f :foo 1)
     (%f :foo 1 :foo 2)
     (%f :bar 'a)
     (%f :bar 'a :bar 'b)
     (%f :foo 'x :bar 'y)
     (%f :bar 'x :foo 'y)
     (%f :bar 'x :foo 'y :bar 'z :foo 'w)
     ))
  (nil nil)
  (1 nil)
  (1 nil)
  (nil a)
  (nil a)
  (x y)
  (y x)
  (y x))

(deftest function.20
  (flet ((%f (&key foo) foo))
    (declare (ftype (function (&key (:foo t) (:allow-other-keys t)) t) %f))
    (values (%f) (%f :foo 'a) (%f :allow-other-keys nil)
	    (%f :allow-other-keys t :foo 'z)))
  nil a nil z)

(deftest function.21
  (flet ((%f (&key foo &allow-other-keys) foo))
    (declare (ftype (function (&key (:foo integer)) t) %f))
    (values (%f) (%f :foo 123)))
  nil 123)

(deftest function.22
  (flet ((%f (&key foo &allow-other-keys) foo))
    (declare (ftype (function (&key (:foo integer) (:bar t)) t) %f))
    (values (%f) (%f :foo 123) (%f :bar 'x) (%f :foo 12 :bar 'y)))
  nil 123 nil 12)

(deftest function.23
  (flet ((%f (&key foo &allow-other-keys) foo))
    (declare (ftype (function (&key (:foo integer) &allow-other-keys) t) %f))
    (values (%f) (%f :foo 123) (%f :bar 'x) (%f :foo 12 :bar 'y)))
  nil 123 nil 12)

(deftest function.24
  (flet ((%f (&rest r &key foo bar) (list r foo bar)))
    (declare (ftype (function (&rest symbol &key (:foo t) (:bar t)) list) %f))
    (values (%f) (%f :foo 'a) (%f :bar 'b) (%f :bar 'd :foo 'c)))
  (nil nil nil)
  ((:foo a) a nil)
  ((:bar b) nil b)
  ((:bar d :foo c) c d))
