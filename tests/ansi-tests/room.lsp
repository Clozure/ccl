;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Dec 12 09:20:47 2004
;;;; Contains: Tests of ROOM

(in-package :cl-test)

(deftest room.1
  (let ((s (with-output-to-string
	     (*standard-output*)
	     (room))))
    (not (zerop (length s))))
  t)

(deftest room.2
  (let ((s (with-output-to-string
	     (*standard-output*)
	     (room nil))))
    (not (zerop (length s))))
  t)

(deftest room.3
  (let ((s (with-output-to-string
	     (*standard-output*)
	     (room :default))))
    (not (zerop (length s))))
  t)

(deftest room.4
  (let ((s (with-output-to-string
	     (*standard-output*)
	     (room t))))
    (not (zerop (length s))))
  t)

;;; Error tests

(deftest room.errpr.1
  (signals-error (with-output-to-string (*standard-output*) (room nil nil)) program-error)
  t)






