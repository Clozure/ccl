;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May  8 19:25:41 2005
;;;; Contains: Tests of GET-UNIVERSAL-TIME, GET-DECODED-TIME

(in-package :cl-test)

;;; Note -- this ignores the possibilty that the time cannot
;;; be determined.

(deftest get-universal-time.1
  (notnot-mv (typep (get-universal-time) 'unsigned-byte))
  t)

(deftest get-universal-time.2
  (let* ((time1 (get-universal-time))
	 (vals (multiple-value-list (get-decoded-time)))
	 (time2 (get-universal-time)))
    (when (= time1 time2)
      (let ((vals2 (multiple-value-list (decode-universal-time time1))))
	(assert (= (length vals) 9))
	(assert (= (length vals2) 9))
	(assert (equal (subseq vals 0 7)
		       (subseq vals2 0 7)))
	(assert (if (elt vals 7) (elt vals2 7) (not (elt vals2 7))))
	(assert (= (elt vals 8) (elt vals2 8)))))
    (values)))

(deftest get-universal-time.3
  (let* ((first (get-universal-time))
	 (prev first))
    (loop for time = (get-universal-time)
	  do (assert (>= time prev))
	  do (setf prev time)
	  until (>= time (+ 5 first))))
  nil)

;;; Error tests

(deftest get-universal-time.error.1
  (signals-error (get-universal-time nil) program-error)
  t)

(deftest get-universal-time.error.2
  (signals-error (get-universal-time :allow-other-keys t) program-error)
  t)

(deftest get-decoded-time.error.1
  (signals-error (get-decoded-time nil) program-error)
  t)

(deftest get-decoded-time.error.2
  (signals-error (get-decoded-time :allow-other-keys t) program-error)
  t)



	       

