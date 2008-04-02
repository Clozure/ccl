;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep 11 20:13:22 2003
;;;; Contains: Tests of BYTE, BYTE-SIZE, and BYTE-POSITION

(in-package :cl-test)

(deftest byte.error.1
  (signals-error (byte) program-error)
  t)

(deftest byte.error.2
  (signals-error (byte 1) program-error)
  t)

(deftest byte.error.3
  (signals-error (byte 1 1 nil) program-error)
  t)

(deftest byte.1
  (progn (byte 0 0) :good)
  :good)

(deftest byte.2
  (progn (byte 1 1) :good)
  :good)

(deftest byte.3
  (loop for i from 0 to 100
	always
	(loop for j from 0 to 100
	      always
	      (let ((bspec (byte i j)))
		(and (eql i (byte-size bspec))
		     (eql j (byte-position bspec))))))
  t)

(deftest byte.4
  (macrolet
   ((%m (z) z))
   (let ((b (byte (expand-in-current-env (%m 2)) 5)))
     (values (byte-size b) (byte-position b))))
  2 5)

(deftest byte.5
  (macrolet
   ((%m (z) z))
   (let ((b (byte 31 (expand-in-current-env (%m 7)))))
     (values (byte-size b) (byte-position b))))
  31 7)

(deftest byte-size.1
  (macrolet ((%m (z) z)) (byte-size (expand-in-current-env (%m (byte 3 7)))))
  3)

(deftest byte-position.1
  (macrolet ((%m (z) z)) (byte-position (expand-in-current-env (%m (byte 3 7)))))
  7)

(deftest byte-position.error.1
  (signals-error (byte-position) program-error)
  t)

(deftest byte-position.error.2
  (signals-error (byte-position (byte 1 1) nil)
		 program-error)
  t)

(deftest byte-size.error.1
  (signals-error (byte-size) program-error)
  t)

(deftest byte-size.error.2
  (signals-error (byte-size (byte 1 1) nil) program-error)
  t)
