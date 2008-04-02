;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May 29 07:16:15 2005
;;;; Contains: Tests of the DECLARATION declarations


(in-package :cl-test)

(deftest declaration.1
  (progn (declaim (declaration)) nil)
  nil)

(deftest declaration.2
  (progn (proclaim '(declaration)) nil)
  nil)

(deftest declaration.3
  (let ((sym (gensym))
	(sym2 (gensym)))
    (proclaim `(declaration ,sym ,sym2))
    nil)
  nil)

;;; For the error tests, see the page in the CLHS for TYPE:
;;; "A symbol cannot be both the name of a type and the name
;;;  of a declaration. Defining a symbol as the name of a class,
;;;  structure, condition, or type, when the symbol has been
;;;  declared as a declaration name, or vice versa, signals an error."

;;; Declare these only if bad declarations produce warnings.

(when (block done
	(handler-bind ((warning #'(lambda (c) (return-from done t))))
		      (eval `(let () (declare (,(gensym))) nil))))

(deftest declaration.4
  (let ((sym (gensym)))
    (proclaim `(declaration ,sym))
    (eval `(signals-error-always (deftype ,sym () t) error)))
  t t)

(deftest declaration.5
  (let ((sym (gensym)))
    (proclaim `(declaration ,sym))
    (eval `(signals-error-always (defstruct ,sym a b c) error)))
  t t)

(deftest declaration.6
  (let ((sym (gensym)))
    (proclaim `(declaration ,sym))
    (eval `(signals-error-always (defclass ,sym () (a b c)) error)))
  t t)

(deftest declaration.7
  (let ((sym (gensym)))
    (proclaim `(declaration ,sym))
    (eval `(signals-error-always (define-condition ,sym (condition) (a b c))
				 error)))
  t t)

(deftest declaration.8
  (let ((sym (gensym)))
    (eval `(deftype ,sym () 'error))
    (eval `(signals-error-always (proclaim '(declaration ,sym))
				 error)))
  t t)

(deftest declaration.9
  (let ((sym (gensym)))
    (eval `(defstruct ,sym a b c))
    (eval `(signals-error-always (proclaim '(declaration ,sym))
				 error)))
  t t)

(deftest declaration.10
  (let ((sym (gensym)))
    (eval `(defclass ,sym () (a b c)))
    (eval `(signals-error-always (proclaim '(declaration ,sym))
				 error)))
  t t)

(deftest declaration.11
  (let ((sym (gensym)))
    (eval `(define-condition ,sym (condition) (a b c)))
    (eval `(signals-error-always (proclaim '(declaration ,sym))
				 error)))
  t t)

)







