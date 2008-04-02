;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 23 20:48:12 2003
;;;; Contains: Tests for WARN

(in-package :cl-test)

(deftest warn.1
  (let ((warned nil))
    (handler-bind
     ((warning #'(lambda (c)
		   (assert (typep c 'simple-warning))
		   (setf warned t)
		   (muffle-warning c))))
     (values
      (multiple-value-list (warn "This is a warning"))
      warned)))
  (nil) t)

(deftest warn.2
  (let ((warned nil))
    (handler-bind
     ((warning #'(lambda (c)
		   (assert (typep c 'simple-warning))
		   (setf warned t)
		   (muffle-warning))))
     (values
      (multiple-value-list (warn "This is a warning"))
      warned)))
  (nil) t)

(deftest warn.3
  (with-output-to-string
    (*error-output*)
    (let ((warned nil))
      (handler-bind
       ((warning #'(lambda (c)
		     (assert (typep c 'simple-warning))
		     (setf warned t)
		     (muffle-warning c))))
       (warn "Foo!"))))
  "")

(deftest warn.4
  (let ((str (with-output-to-string
	       (*error-output*)
	       (warn "Foo!"))))
    (not (string= str "")))
  t)

(deftest warn.5
  (let ((warned nil))
    (handler-bind
     ((simple-warning #'(lambda (c)
			  (assert (typep c 'simple-warning))
			  (setf warned t)
			  (muffle-warning c))))
     (values
      (multiple-value-list (warn "This is a warning"))
      warned)))
  (nil) t)

(deftest warn.6
  (let ((warned nil))
    (handler-bind
     ((simple-condition #'(lambda (c)
			    (assert (typep c 'simple-warning))
			    (setf warned t)
			    (muffle-warning c))))
     (values
      (multiple-value-list (warn "This is a warning"))
      warned)))
  (nil) t)

(deftest warn.7
  (let ((warned nil))
    (handler-bind
     ((condition #'(lambda (c)
		     (assert (typep c 'simple-warning))
		     (setf warned t)
		     (muffle-warning c))))
     (values
      (multiple-value-list (warn "This is a warning"))
      warned)))
  (nil) t)

(deftest warn.8
  (let ((warned nil))
    (handler-bind
     ((warning #'(lambda (c)
		   (assert (typep c 'simple-warning))
		   (setf warned t)
		   (muffle-warning c))))
     (values
      (multiple-value-list (warn 'simple-warning :format-control "Foo!"))
      warned)))
  (nil) t)

(deftest warn.9
  (let ((warned nil))
    (handler-bind
     ((warning #'(lambda (c)
		   (assert (typep c 'warning))
		   (setf warned t)
		   (muffle-warning c))))
     (values
      (multiple-value-list (warn 'warning))
      warned)))
  (nil) t)

(deftest warn.10
  (let ((warned nil))
    (handler-bind
     ((warning #'(lambda (c)
		   (assert (typep c 'simple-warning))
		   (setf warned t)
		   (muffle-warning c))))
     (values
      (multiple-value-list (warn (make-condition 'simple-warning :format-control "Foo!")))
      warned)))
  (nil) t)

(deftest warn.11
  (let ((warned nil))
    (handler-bind
     ((warning #'(lambda (c)
		   (assert (typep c 'warning))
		   (setf warned t)
		   (muffle-warning c))))
     (values
      (multiple-value-list (warn (make-condition 'warning)))
      warned)))
  (nil) t)

(deftest warn.12
  (signals-error (warn 'condition) type-error)
  t)

(deftest warn.13
  (signals-error (warn 'simple-condition) type-error)
  t)

(deftest warn.14
  (signals-error (warn (make-condition 'simple-warning) :format-control "Foo") type-error)
  t)

(deftest warn.15
  (signals-error (warn) program-error)
  t)

(deftest warn.16
  (signals-error (warn (make-condition 'condition)) type-error)
  t)

(deftest warn.17
  (signals-error (warn (make-condition 'simple-condition)) type-error)
  t)

(deftest warn.18
  (signals-error (warn (make-condition 'simple-error)) type-error)
  t)

(deftest warn.19
  (let ((warned nil))
    (handler-bind
     ((warning #'(lambda (c)
		   (assert (typep c 'simple-warning))
		   (setf warned t)
		   (muffle-warning c))))
     (values
      (multiple-value-list
       (warn (make-condition 'simple-warning
			     :format-control (formatter "Foo!"))))
      warned)))
  (nil) t)