;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May  7 07:00:58 2005
;;;; Contains: Tests of DECODE-UNIVERSAL-TIME

(in-package :cl-test)

(deftest decode-universal-time.1
  (decode-universal-time 0 0)
  0 0 0 1 1 1900 0 nil 0)

(deftest decode-universal-time.2
  (decode-universal-time 0 -1)
  0 0 1 1 1 1900 0 nil -1)

(deftest decode-universal-time.3
  (let ((count 0))
    (loop for time = (random 10000000000)
	  for tz = (- (random 49) 24)
	  for (second minute hour date month year day daylight-p zone)
	  = (multiple-value-list (decode-universal-time time tz))
	  for time2 = (encode-universal-time second minute hour date month year zone)
	  repeat 1000
	  unless (and (eql tz zone) (eql time time2) (null daylight-p))
	  collect (progn (incf count)
			 (list time tz (list second minute hour date month year day daylight-p zone) time2))
	  until (>= count 100)))
  nil)

(deftest decode-universal-time.4
  (let ((count 0))
    (loop for time = (random 10000000000)
	  for tz = (/ (- (random (1+ (* 48 3600))) (* 24 3600)) 3600)
	  for (second minute hour date month year day daylight-p zone)
	  = (multiple-value-list (decode-universal-time time tz))
	  for time2 = (encode-universal-time second minute hour date month year zone)
	  repeat 1000
	  unless (and (eql tz zone) (eql time time2) (null daylight-p))
	  collect (progn (incf count)
			 (list time tz (list second minute hour date month year day daylight-p zone) time2))
	  until (>= count 100)))
  nil)

(deftest decode-universal-time.5
  (let ((count 0))
    (loop for time = (random 10000000000)
	  for (second minute hour date month year day daylight-p zone)
	  = (handler-case
	     (multiple-value-list (decode-universal-time time))
	     (error (c) (print time) (error c)))
	  for time2 = (encode-universal-time second minute hour date month year)
	  repeat 1000
	  unless (let ((daylight-p-2 (nth-value 7 (decode-universal-time time2))))
		   (or (eql time time2)
		       (and daylight-p (not daylight-p-2) ; (eql time (- time2 3600))
			    )
		       (and (not daylight-p) daylight-p-2 ; (eql time (+ time2 3600))
			    )))
	  collect (progn (incf count)
			 (list time (list second minute hour date month year day daylight-p zone) time2))
	  until (>= count 100)))
  nil)

(deftest decode-universal-time.6
  (let ((vals0 (multiple-value-list (get-decoded-time)))
	(vals1 (multiple-value-list (decode-universal-time (get-universal-time))))
	(vals2 (multiple-value-list (get-decoded-time))))
    (when (equal vals0 vals2)
      (assert (= (length vals1) 9))
      (assert (= (length vals2) 9))
      (assert (equal (subseq vals1 0 7) (subseq vals2 0 7)))
      (assert (if (elt vals1 7) (elt vals2 7) (not (elt vals2 7))))
      (assert (= (elt vals1 8) (elt vals2 8))))
    (values)))

(deftest decode-universal-time.7
  (decode-universal-time  (* 365 3600 24) 0)
  0 0 0 1 1 1901 1 nil 0)

(deftest decode-universal-time.8
  (decode-universal-time  (* 2 365 3600 24) 0)
  0 0 0 1 1 1902 2 nil 0)

(deftest decode-universal-time.9
  (decode-universal-time  (* 3 365 3600 24) 0)
  0 0 0 1 1 1903 3 nil 0)

(deftest decode-universal-time.10
  (decode-universal-time  (* 4 365 3600 24) 0)
  0 0 0 1 1 1904 4 nil 0)

(deftest decode-universal-time.11
  (decode-universal-time (+ (* 24 3600) (* 5 365 3600 24)) 0)
  0 0 0 1 1 1905 6 nil 0)

(deftest decode-universal-time.12
  (loop for time = (random 100000000000)
	for tz = (- (random 49) 24)
	for interval = (1+ (random 10000))
	for time2 = (+ time (* interval  24 3600))
	;; 'time2' is exactly interval days after 'time'
	for day = (nth-value 6 (decode-universal-time time tz))
	for day2 = (nth-value 6 (decode-universal-time time2 tz))
	repeat 1000
	;; Check that the days of the week are consistent
	unless (= (mod day2 7) (mod (+ day interval) 7))
	collect (list time time2 tz interval day day2))
  nil)

;;; Error tests

(deftest decode-universal-time.error.1
  (signals-error (decode-universal-time) program-error)
  t)

(deftest decode-universal-time.error.2
  (signals-error (decode-universal-time 0 0 nil) program-error)
  t)




