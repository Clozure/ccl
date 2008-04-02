;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 28 21:37:43 2003
;;;; Contains: Tests of ERROR

(in-package :cl-test)

(deftest error.1
  (let ((fmt "Error"))
    (handler-case
     (error fmt)
     (simple-error (c) (frob-simple-error c fmt))))
  t)

(deftest error.2
  (let* ((fmt "Error")
	 (cnd (make-condition 'simple-error :format-control fmt)))
    (handler-case
     (error cnd)
     (simple-error (c) (frob-simple-error c fmt))))
  t)

(deftest error.3
  (let ((fmt "Error"))
    (handler-case
     (error 'simple-error :format-control fmt)
     (simple-error (c) (frob-simple-error c fmt))))
  t)

(deftest error.4
  (let ((fmt "Error: ~A"))
    (handler-case
     (error fmt 10)
     (simple-error (c) (frob-simple-error c fmt 10))))
  t)

(deftest error.5
  (let ((fmt (formatter "Error")))
    (handler-case
     (error fmt)
     (simple-error (c) (frob-simple-error c fmt))))
  t)

(deftest error.6
  (handler-case
   (error 'simple-condition)
   (error (c) (declare (ignore c)) :wrong)
   (simple-condition (c) (declare (ignore c)) :right))
  :right)

(deftest error.7
  (handler-case
   (error 'simple-warning)
   (error (c) (declare (ignore c)) :wrong)
   (simple-warning (c) (declare (ignore c)) :right)
   (condition (c) (declare (ignore c)) :wrong2))
  :right)

(deftest error.8
  (let ((fmt "Boo!"))
    (handler-case
     (error 'simple-warning :format-control fmt)
     (simple-warning (c) (frob-simple-warning c fmt))))
  t)

(deftest error.9
  (let ((fmt (formatter "Boo!")))
    (handler-case
     (error 'simple-warning :format-control fmt)
     (simple-warning (c) (frob-simple-warning c fmt))))
  t)

(deftest error.10
  (let ((fmt (formatter "Error")))
    (handler-case
     (error 'simple-error :format-control fmt)
     (simple-error (c) (frob-simple-error c fmt))))
  t)

(deftest error.11
  (let ((fmt (formatter "Error")))
    (handler-case
     (error fmt)
     (simple-error (c) (frob-simple-error c fmt))))
  t)

(deftest error.12
  (let* ((fmt (formatter "Error"))
	 (cnd (make-condition 'simple-error :format-control fmt)))
    (handler-case
     (error cnd)
     (simple-error (c) (frob-simple-error c fmt))))
  t)

;;; Tests for other conditions will in their own files.
