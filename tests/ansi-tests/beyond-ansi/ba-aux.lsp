;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon May 30 06:45:08 2005
;;;; Contains: Aux. files for beyond-ansi tests

(in-package :ba-test)

(defun function-name-p (x)
  (or (symbolp x)
      (and (consp x)
	   (eql (car x) 'setf)
	   (consp (cdr x))
	   (symbolp (cadr x))
	   (null (cddr x)))))

(defun symbol-or-function-p (x)
  (or (symbolp x)
      (and (consp x)
	   (eql (car x) 'function)
	   (consp (cdr x))
	   (null (cddr x))
	   (function-name-p (cadr x)))))

(defun symbol-or-list-p (x)
  (or (symbolp x) (listp x)))

(defun function-designator-p (x)
  (or (functionp x)
      (and (symbolp x) (not (macro-function x)) (not (special-operator-p x)))))

(defun type-specifier-p (x)
  (typep x '(or symbol list class)))

(defun causes-error-p (pred formf &key (vals *mini-universe*) (var 'x))
  (when (symbolp pred)
    (assert (fboundp pred))
    (setf pred (symbol-function pred)))
  (loop for x in vals
	for inner-form = (if (functionp formf)
			     (funcall formf x)
			   (subst `',x var formf))
	for form = `(signals-error ,inner-form error)
	unless (or (funcall pred x) (eval form))
	collect x))

(defmacro def-all-error-test (name pred form &rest other-args)
  `(deftest ,name
     (causes-error-p ,pred ,form ,@other-args)
     nil))

(defmacro def-error-test (name form)
  `(deftest ,name
     (signals-error ,form error)
     t))


