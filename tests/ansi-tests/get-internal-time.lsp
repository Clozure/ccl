;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May  8 20:28:21 2005
;;;; Contains: Tests of GET-INTERNAL-REAL-TIME, GET-INTERNAL-RUN-TIME

(in-package :cl-test)

(deftest get-internal-real-time.1
  (notnot-mv (typep (multiple-value-list (get-internal-real-time)) '(cons unsigned-byte null)))
  t)

(deftest get-internal-real-time.2
  (funcall
   (compile
    nil
    '(lambda ()
       (let ((prev (get-internal-real-time)))
	 (loop for next = (get-internal-real-time)
	       repeat 100000
	       do (assert (>= next prev))
	       do (setf prev next))))))
  nil)

(deftest get-internal-real-time.error.1
  (signals-error (get-internal-real-time nil) program-error)
  t)

(deftest get-internal-real-time.error.2
  (signals-error (get-internal-real-time :allow-other-keys t) program-error)
  t)

;;;;;

(deftest get-internal-run-time.1
  (notnot-mv (typep (multiple-value-list (get-internal-run-time)) '(cons unsigned-byte null)))
  t)

(deftest get-internal-run-time.2
  (funcall
   (compile
    nil
    '(lambda ()
       (let ((prev (get-internal-run-time)))
	 (loop for next = (get-internal-run-time)
	       repeat 100000
	       do (assert (>= next prev))
	       do (setf prev next))))))
  nil)

(deftest get-internal-run-time.error.1
  (signals-error (get-internal-run-time nil) program-error)
  t)

(deftest get-internal-run-time.error.2
  (signals-error (get-internal-run-time :allow-other-keys t) program-error)
  t)

;;;

(deftest internal-time-units-per-second.1
  (notnot-mv (constantp 'internal-time-units-per-second))
  t)

(deftest internal-time-units-per-second.2
  (notnot-mv (typep internal-time-units-per-second '(integer 1)))
  t)
