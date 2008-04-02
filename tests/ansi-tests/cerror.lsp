;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 19:45:27 2003
;;;; Contains: Tests of CERROR


(in-package :cl-test)

(deftest cerror.1
  (let ((fmt "Cerror"))
    (handler-case (cerror "Keep going." fmt)
		  (simple-error (c) (frob-simple-error c fmt))))
  t)

(deftest cerror.2
  (let* ((fmt "Cerror")
	 (cnd (make-condition 'simple-error :format-control fmt)))
    (handler-case (cerror "Continue on." cnd)
		  (simple-error (c) (frob-simple-error c fmt))))
  t)

(deftest cerror.2a
  (let* ((fmt (formatter "Cerror"))
	 (cnd (make-condition 'simple-error :format-control fmt)))
    (handler-case (cerror "Continue on." cnd)
		  (simple-error (c) (frob-simple-error c fmt))))
  t)

(deftest cerror.3
  (let ((fmt "Cerror"))
    (handler-case (cerror "Continue" 'simple-error :format-control fmt)
		  (simple-error (c) (frob-simple-error c fmt))))
  t)

(deftest cerror.4
  (let ((fmt "Cerror: ~A"))
    (handler-case (cerror "On on" fmt 10)
		  (simple-error (c) (frob-simple-error c fmt 10))))
  t)

(deftest cerror.4a
  (let ((fmt (formatter "Cerror: ~A")))
    (handler-case (cerror "On on" fmt 10)
		  (simple-error (c) (frob-simple-error c fmt 10))))
  t)

(deftest cerror.5
  (let ((fmt (formatter "Cerror")))
    (handler-case (cerror "Keep going." fmt)
		  (simple-error (c) (frob-simple-error c fmt))))
  t)

;;; Continuing from a cerror

(deftest cerror.6
  (handler-bind ((simple-error #'(lambda (c) (continue c))))
		(progn
		  (cerror "Wooo" 'simple-error)
		  10))
  10)

;;; Program error cases

(deftest cerror.error.1
  (signals-error (cerror) program-error)
  t)

(deftest cerror.error.2
  (signals-error (cerror "foo") program-error)
  t)

