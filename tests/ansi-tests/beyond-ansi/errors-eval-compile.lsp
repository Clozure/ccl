;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 28 06:37:41 2005
;;;; Contains: Tests for nonstandard exceptional conditions in section 3

(in-package :ba-test)

(declaim (notinline compile-fails?))

(compile-and-load "ba-aux.lsp")

;;; Utility functions

(defun compile-fails? (&rest args)
  (cl:handler-case
   (let ((vals (multiple-value-list (apply #'compile args))))
     (if (and (= (length vals) 3)
	      (cadr vals)
	      (caadr vals))
	 t
       (apply #'values nil vals)))
   (error () t)))

;;; Tests of COMPILE

(deftest compile.1
  (loop for x in *mini-universe*
	unless (or (function-name-p x)
		   (compile-fails? x))
	collect x)
  nil)

(deftest compile.2
  (compile-fails? nil)
  t)

(deftest compile.3
  (let ((sym (gensym)))
    (eval `(defun ,sym () nil))
    (loop for x in *mini-universe*
	  unless (or (functionp x) (and (consp x) (eql (car x) 'lambda))
		     (compile-fails? sym x))
	  collect x))
  nil)

(deftest compile.4
  (compile-fails? nil '(lambda))
  t)

(deftest compile.5
  (compile-fails? nil '(lambda x))
  t)

;;; EVAL-WHEN tests

(def-all-error-test eval-when.1 'listp '(eval-when x nil))

;;; LOAD-TIME-VALUE

(def-error-test load-time-value.1 (load-time-value))
(def-error-test load-time-value.2 (load-time-value nil nil nil))

;;; QUOTE

(def-error-test quote.1 (quote))
(def-error-test quote.2 (quote . x))
(def-error-test quote.3 (quote t . x))
(def-error-test quote.4 (quote t x))

;;; COMPILER-MACRO-FUNCTION

(def-all-error-test compiler-macro-function.1
  'function-name-p '(compiler-macro-function x))

(def-all-error-test compiler-macro-function.2
  'function-name-p
  '(setf (compiler-macro-function x) #'rplacd))

;;; DEFINE-COMPILER-MACRO

(def-error-test define-compiler-macro.1 (define-compiler-macro))

(deftest define-compiler-macro.2
  (let ((sym (gensym)))
    (eval `(signals-error (define-compiler-macro ,sym) error)))
  t)

(def-error-test define-compiler-macro.3 (define-compiler-macro . foo))

(deftest define-compiler-macro.4
  (let ((sym (gensym)))
    (eval `(signals-error (define-compiler-macro ,sym () . foo) error)))
  t)

;;; DEFMACRO

(def-error-test defmacro.1 (defmacro))
(deftest defmacro.2
  (let ((sym (gensym)))
    (eval `(signals-error (defmacro ,sym) error)))
  t)

(def-error-test defmacro.3 (defmacro . foo))
(deftest defmacro.4
  (let ((sym (gensym)))
    (eval `(signals-error (defmacro ,sym () . foo) error)))
  t)

;;; MACRO-FUNCTION

(def-all-error-test macro-funtion.1 'symbolp '(macro-function x))

(def-all-error-test macro-funtion.2
  'symbolp '(setf (macro-function x) (macro-function 'pop)))

;;; DEFINE-SYMBOL-MACRO

(deftest define-symbol-macro.1
  (let ((sym (gensym)))
    (eval `(signals-error (define-symbol-macro ,sym) error)))
  t)

(deftest define-symbol-macro.2
  (let ((sym (gensym)))
    (eval `(signals-error (define-symbol-macro ,sym t nil) error)))
  t)

(def-all-error-test define-symbol-macro.3 'symbolp '(define-symbol-macro x))

;;; IGNORE

(def-all-error-test ignore.1
  'symbol-or-function-p '(locally (declare (ignore x)) nil))

(def-error-test ignore.2 (locally (declare (ignore . foo)) nil))

;;; IGNORABLE

(def-all-error-test ignorable.1
  'symbol-or-function-p '(locally (declare (ignorable x)) nil))

(def-error-test ignorable.2 (locally (declare (ignorable . foo)) nil))

;;; DYNAMIC-EXTENT

(def-all-error-test dynamic-extent.1
  'symbol-or-function-p '(locally (declare (dynamic-extent x)) nil))

(def-error-test dynamic-extent.2
  (locally (declare (dynamic-extent . foo)) nil))

;;; TYPE declarations
;;; Test that violation of the type declarations is detected, and
;;; leads to an error in safe code.

#-sbcl
(deftest type.1
  (loop for x in *mini-universe*
	for tp = (type-of x)
	for lambda-form = `(lambda (y) (declare (optimize safety)
						(type (not ,tp) y)) y)
	for fn = (progn (print lambda-form)
			(eval `(function ,lambda-form)))
	unless (eval `(signals-error (funcall ',fn ',x) error))
	collect x)
  nil)

(deftest type.2
  (let* ((utypes (coerce (mapcar #'type-of *universe*) 'vector))
	 (n (length utypes)))
    (flet ((%rtype () (elt utypes (random n))))
      (loop for x in *mini-universe*
	    for tp = (loop for tp = (%rtype)
			   while (typep x tp)
			   finally (return tp))
	    for lambda-form = `(lambda (y) (declare (optimize safety)
						(type ,tp y)) y)
	    for fn = (progn ;; (print lambda-form)
			    (eval `(function ,lambda-form)))
	    unless (eval `(signals-error (funcall ',fn ',x) error))
	    collect x)))
  nil)

(deftest type.2c
  (let* ((utypes (coerce (mapcar #'type-of *universe*) 'vector))
	 (n (length utypes)))
    (flet ((%rtype () (elt utypes (random n))))
      (loop for x in *mini-universe*
	    for tp = (loop for tp = (%rtype)
			   while (typep x tp)
			   finally (return tp))
	    for lambda-form = `(lambda (y) (declare (optimize safety)
						(type ,tp y)) y)
	    for fn = (progn ;; (print lambda-form)
			    (compile nil lambda-form))
	    unless (eval `(signals-error (funcall ',fn ',x) error))
	    collect x)))
  nil)

(deftest type.3
  (loop for x in *mini-universe*
	for tp = (type-of x)
	for lambda-form = `(lambda (z) (declare (optimize safety))
			     (let ((y z))
			       (declare (type ,tp y))
			       y))
	for fn = (progn ;; (print lambda-form)
		   (eval `(function ,lambda-form)))
	unless (or (typep nil tp)
		   (eval `(signals-error (funcall ',fn nil) error)))
	collect x)
  nil)

(deftest type.3c
  (loop for x in *mini-universe*
	for tp = (type-of x)
	for lambda-form = `(lambda (z) (declare (optimize safety))
			     (let ((y z))
			       (declare (type ,tp y))
			       y))
	for fn = (progn ;; (print lambda-form)
		   (compile nil lambda-form))
	unless (or (typep nil tp)
		   (eval `(signals-error (funcall ',fn nil) error)))
	collect x)
  nil)

(deftest type.4
  (loop for x in *mini-universe*
	for tp = (type-of x)
	for lambda-form = `(lambda (z) (declare (optimize safety))
			     (the ,tp z))
	for fn = (progn ;; (print lambda-form)
		   (eval `(function ,lambda-form)))
	unless (or (typep nil tp)
		   (eval `(signals-error (funcall ',fn nil) error)))
	collect x)
  nil)

(deftest type.5
  (signals-error (let () (declare (type . foo)) nil) error)
  t)

(deftest type.6
  (signals-error (let () (declare (type integer . foo)) nil) error)
  t)

(deftest type.7
  (signals-error (let () (declare (integer . foo)) nil) error)
  t)

(deftest type.8
  (signals-error (let ((x (make-array 3 :initial-element 0
				      :element-type '(integer 0 2))))
		   (declare (optimize safety)
			    (type (array (integer 0 2) (3)) x))
		   (setf (aref x 0) 3)
		   (aref x 0))
		 error)
  t)

;; Move the type tests off to another file, eventually.

;;; INLINE

(def-all-error-test inline.1
  'function-name-p '(locally (declare (inline x)) nil))

(def-error-test inline.2 (locally (declare (inline . x)) nil))

;;; NOTINLINE

(def-all-error-test notinline.1
  'function-name-p '(locally (declare (notinline x)) nil))

(def-error-test notinline.2 (locally (declare (notinline . x)) nil))

;;; FTYPE

(def-error-test ftype.1
  (macrolet ((%m () :foo))
    (declare (ftype (function (&rest t) t) %m))
    (%m)))

(def-error-test ftype.2
  (flet ((%f () :foo))
    (declare (ftype (function () (eql :bar)) %f))
    (%f)))

(def-error-test ftype.3 (locally (declare (ftype)) nil))
(def-error-test ftype.4 (locally (declare (ftype symbol)) nil))
(def-error-test ftype.5 (locally (declare (ftype (function () t) . foo)) nil))

(def-all-error-test ftype.6
  'function-name-p '(locally (declare (ftype (function () t) x)) nil))

;;; DECLARATIONS

(def-error-test declaration.1 (proclaim '(declaration . foo)))

(def-all-error-test declaration.2 'symbolp '(proclaim (declaration x)))

;;; OPTIMIZE

(def-error-test optimize.1 (locally (declare (optimize .foo)) nil))

(def-all-error-test optimize.2
  'symbolp '(locally (declare (optimize (x 0))) nil))

(def-all-error-test optimize.3
  (typef '(mod 4)) '(locally (declare (optimize (speed x)))))

;;; SPECIAL

(def-error-test special.1 (locally (declare (special . x)) nil))
(def-all-error-test special.2 'symbolp '(locally (declare (special x)) nil))

;;; LOCALLY

(def-error-test locally.1 (locally . x))

;;; THE

(def-error-test the.1 (the))
(def-error-test the.2 (the t))
(def-error-test the.3 (the t :a :b))
(def-error-test the.4 (setf (the) nil))
(def-error-test the.5 (setf (the t) nil))
(def-error-test the.6 (let (x y) (setf (the t x y) nil)))

;;; 
