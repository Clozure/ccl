;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May  8 12:54:34 2005
;;;; Contains: Tests of ENCODE-UNIVERSAL-TIME

;;; See also the tests in decode-universal-time.lsp

(in-package :cl-test)

(deftest encode-universal-time.1
  (loop with count = 0
	for year = (+ 1900 (random 1000))
	;; Gregorian leap year algorithm
	for leap? = (and (= (mod year 4) 0)
			 (or (/= (mod year 100) 0)
			     (= (mod year 400) 0)))
	for month = (1+ (random 12))
	for date = (1+ (random (elt (if leap?
					#(0 31 29 31 30 31 30 31 31 30 31 30 31)
				      #(0 31 28 31 30 31 30 31 31 30 31 30 31))
				    month)))
	for hour = (random 24)
	for minute = (random 60)
	for second = (random 60)
	for tz = (if (and (= year 1900) (= date 0) (= month 0))
		     (random 25)
		   (- (random 49) 24))
	for time = (encode-universal-time second minute hour date month year tz)
	for decoded-vals = (multiple-value-list (decode-universal-time time tz))
	for vals = (list second minute hour date month year (elt decoded-vals 6)
			 nil tz)
	repeat 20000
	unless (equal vals decoded-vals)
	collect (progn (incf count) (list vals time decoded-vals))
	until (>= count 100))
  nil)

#|
(deftest encode-universal-time.2
  (loop with count = 0
	for year = (+ 1901 (random 1000))
	;; Gregorian leap year algorithm
	for leap? = (and (= (mod year 4) 0)
			 (or (/= (mod year 100) 0)
			     (= (mod year 400) 0)))
	for month = (1+ (random 12))
	for date = (1+ (random (elt (if leap?
					#(0 31 29 31 30 31 30 31 31 30 31 30 31)
				      #(0 31 28 31 30 31 30 31 31 30 31 30 31))
				    month)))
	for hour = (random 24)
	for minute = (random 60)
	for second = (random 60)
	for time = (encode-universal-time second minute hour date month year)
	for decoded-vals = (multiple-value-list (decode-universal-time time))
	for vals = (list second minute hour date month year (elt decoded-vals 6)
			 (elt decoded-vals 7) (elt decoded-vals 8))
	repeat 20000
	unless (equal vals decoded-vals)
	collect (progn (incf count) (list vals time decoded-vals))
	until (>= count 100))
  nil)
|#

(deftest encode-universal-time.3
  (loop with count = 0
	for year = (+ 1900 (random 1000))
	;; Gregorian leap year algorithm
	for leap? = (and (= (mod year 4) 0)
			 (or (/= (mod year 100) 0)
			     (= (mod year 400) 0)))
	for month = (1+ (random 12))
	for date = (1+ (random (elt (if leap?
					#(0 31 29 31 30 31 30 31 31 30 31 30 31)
				      #(0 31 28 31 30 31 30 31 31 30 31 30 31))
				    month)))
	for hour = (random 24)
	for minute = (random 60)
	for second = (random 60)
	for tz = (/ (if (and (= year 1900) (= date 0) (= month 0))
			(random (1+ (* 24 3600)))
		      (- (random (1+ (* 48 3600))) (* 24 3600)))
		    3600)
	for time = (encode-universal-time second minute hour date month year tz)
	for decoded-vals = (multiple-value-list (decode-universal-time time tz))
	for vals = (list second minute hour date month year (elt decoded-vals 6)
			 nil tz)
	repeat 20000
	unless (equal vals decoded-vals)
	collect (progn (incf count) (list vals time decoded-vals))
	until (>= count 100))
  nil)

;;; Error cases

(deftest encode-universal-time.error.1
  (signals-error (encode-universal-time 0 0 0 1 1) program-error)
  t)

(deftest encode-universal-time.error.2
  (signals-error (encode-universal-time 0 0 0 1 1 1901 0 nil) program-error)
  t)
