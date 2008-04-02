;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Mar  2 07:32:57 2004
;;;; Contains: Tests of printing of floating point numbers

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest print.short-float.1
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*read-default-float-format* 'short-float))
     (loop for i from -4000 to 4000
	   for f = (float i 0.0s0)
	   for s1 = (with-output-to-string (s) (prin1 f s))
	   for s2 = (format nil "~A.0" i)
	   unless (equalp s1 s2)
	   collect (list i f s1 s2))))
  nil)

(deftest print.short-float.2
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*read-default-float-format* 'short-float))
     (loop for i = (- (random 20000000) 10000000)
	   for f = (float i 0.0s0)
	   for s1 = (with-output-to-string (s) (prin1 f s))
	   for s2 = (format nil "~A.0" i)
	   repeat 10000
	   unless (or (/= i (rational f)) ; not enough bits
		   ;; (> (nth-value 1 (integer-decode-float f)) 0)
		   (equalp s1 s2))
	   collect (list i f s1 s2))))
  nil)

(defparameter *possible-short-float-exponent-markers*
  (loop for type in '(short-float single-float double-float long-float)
		     for c across "SFDL"
		     when (subtypep 'short-float type)
		     nconc (list c (char-downcase c))))

(deftest print.short-float.3
  (let ((chars *possible-short-float-exponent-markers*))
    (loop for type in '(single-float double-float long-float)
	  nconc
	  (and (not (subtypep 'short-float type))
	       (with-standard-io-syntax
		(let ((*print-readably* nil)
		      (*read-default-float-format* type))
		  (loop for i from -4000 to 4000
			for f = (float i 0.0s0)
			for s1 = (with-output-to-string (s) (prin1 f s))
			for len1 = (length s1)
			for s2 = (format nil "~A.0" i)
			unless (and (> len1 4)
				    (string-equal s1 s2 :start1 0 :end1 (- len1 2))
				    (eql (char s1 (- len1 1)) #\0)
				    (member (char s1 (- len1 2)) chars))
			collect (list type i f s1 s2)))))))
  nil)

(deftest print.short-float.4
  (let ((chars *possible-short-float-exponent-markers*))
    (loop for type in '(single-float double-float long-float)
	  nconc
	  (and (not (subtypep 'short-float type))
	       (with-standard-io-syntax
		(let ((*print-readably* nil)
		      (*read-default-float-format* type))
		  (loop for i = (- (random 20000000) 10000000)
			for f = (float i 0.0s0)
			for s1 = (with-output-to-string (s) (prin1 f s))
			for len1 = (length s1)
			for s2 = (format nil "~A.0" i)
			repeat 10000
			unless (or (/= i (rational f))  ;; not enough bits
				;; (> (nth-value 1 (integer-decode-float f)) 0)
				(and (> len1 4)
				     (string-equal s1 s2 :start1 0 :end1 (- len1 2))
				     (eql (char s1 (- len1 1)) #\0)
				     (member (char s1 (- len1 2)) chars)))
			collect (list type i f s1 s2)))))))
  nil)

(deftest print.short-float.random
  (let ((lower-bound (if (< (log least-positive-short-float 10) -100)
			 (expt 0.1s0 100)
		       least-positive-short-float))
	(upper-bound (/ (if (> (log most-positive-short-float 10) 100)
			    (expt 10.0s0 100)
			  most-positive-short-float)
			10)))
    (loop for sf = lower-bound then (* 10 sf)
	  while (< sf upper-bound)
	  nconc
	  (loop for x = (handler-case (random sf) (arithmetic-error (c) 0.0s0))
		for y = (if (coin) (- x) x)
		repeat 10
		nconc (randomly-check-readability y))))
  nil)

;;; single floats

(deftest print.single-float.1
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*read-default-float-format* 'single-float))
     (loop for i from -4000 to 4000
	   for f = (float i 0.0f0)
	   for s1 = (with-output-to-string (s) (prin1 f s))
	   for s2 = (format nil "~A.0" i)
	   unless (equalp s1 s2)
	   collect (list i f s1 s2))))
  nil)

(deftest print.single-float.2
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*read-default-float-format* 'single-float))
     (loop for i = (- (random 20000000) 10000000)
	   for f = (float i 0.0f0)
	   for s1 = (with-output-to-string (s) (prin1 f s))
	   for s2 = (format nil "~A.0" i)
	   repeat 10000
	   unless (or (/= i (rational f))  ;; not enough bits
		   ;; (> (nth-value 1 (integer-decode-float f)) 0)
		   (equalp s1 s2))
	   collect (list i f s1 s2))))
  nil)

(defparameter *possible-single-float-exponent-markers*
  (loop for type in '(short-float single-float double-float long-float)
		     for c across "SFDL"
		     when (subtypep 'single-float type)
		     nconc (list c (char-downcase c))))

(deftest print.single-float.3
  (let ((chars *possible-single-float-exponent-markers*))
    (loop for type in '(short-float double-float long-float)
	  nconc
	  (and (not (subtypep 'single-float type))
	       (with-standard-io-syntax
		(let ((*print-readably* nil)
		      (*read-default-float-format* type))
		  (loop for i from -4000 to 4000
			for f = (float i 0.0f0)
			for s1 = (with-output-to-string (s) (prin1 f s))
			for len1 = (length s1)
			for s2 = (format nil "~A.0" i)
			unless (and (> len1 4)
				    (string-equal s1 s2 :start1 0 :end1 (- len1 2))
				    (eql (char s1 (- len1 1)) #\0)
				    (member (char s1 (- len1 2)) chars))
			collect (list type i f s1 s2)))))))
  nil)

(deftest print.single-float.4
  (let ((chars *possible-single-float-exponent-markers*))
    (loop for type in '(short-float double-float long-float)
	  nconc
	  (and (not (subtypep 'single-float type))
	       (with-standard-io-syntax
		(let ((*print-readably* nil)
		      (*read-default-float-format* type))
		  (loop for i = (- (random 20000000) 10000000)
			for f = (float i 0.0f0)
			for s1 = (with-output-to-string (s) (prin1 f s))
			for len1 = (length s1)
			for s2 = (format nil "~A.0" i)
			repeat 10000
			unless (or (/= i (rational f))  ;; not enough bits
				;; (> (nth-value 1 (integer-decode-float f)) 0)
				(and (> len1 4)
				     (string-equal s1 s2 :start1 0 :end1 (- len1 2))
				     (eql (char s1 (- len1 1)) #\0)
				     (member (char s1 (- len1 2)) chars)))
			collect (list type i f s1 s2)))))))
  nil)

(deftest print.single-float.random
  (let ((lower-bound (if (< (log least-positive-single-float 10) -100)
			 (expt 0.1f0 100)
		       least-positive-single-float))
	(upper-bound (/ (if (> (log most-positive-single-float 10) 100)
			    (expt 10.0f0 100)
			  most-positive-single-float)
			10)))
    (loop for f = lower-bound then (* 10 f)
	  while (< f upper-bound)
	  nconc
	  (loop for x = (handler-case (random f) (arithmetic-error (c) 0.0f0))
		for y = (if (coin) (- x) x)
		repeat 10
		nconc (randomly-check-readability y))))
  nil)

;;; double float

(deftest print.double-float.1
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*read-default-float-format* 'double-float))
     (loop for i from -4000 to 4000
	   for f = (float i 0.0d0)
	   for s1 = (with-output-to-string (s) (prin1 f s))
	   for s2 = (format nil "~A.0" i)
	   unless (equalp s1 s2)
	   collect (list i f s1 s2))))
  nil)

(deftest print.double-float.2
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*read-default-float-format* 'double-float))
     (loop for i = (- (random 20000000) 10000000)
	   for f = (float i 0.0d0)
	   for s1 = (with-output-to-string (s) (prin1 f s))
	   for s2 = (format nil "~A.0" i)
	   repeat 10000
	   unless (or (/= i (rational f))  ;; not enough bits
		   ;; (> (nth-value 1 (integer-decode-float f)) 0)
		   (equalp s1 s2))
	   collect (list i f s1 s2))))
  nil)

(defparameter *possible-double-float-exponent-markers*
  (loop for type in '(short-float single-float double-float long-float)
		     for c across "SFDL"
		     when (subtypep 'double-float type)
		     nconc (list c (char-downcase c))))

(deftest print.double-float.3
  (let ((chars *possible-double-float-exponent-markers*))
    (loop for type in '(short-float double-float long-float)
	  nconc
	  (and (not (subtypep 'double-float type))
	       (with-standard-io-syntax
		(let ((*print-readably* nil)
		      (*read-default-float-format* type))
		  (loop for i from -4000 to 4000
			for f = (float i 0.0d0)
			for s1 = (with-output-to-string (s) (prin1 f s))
			for len1 = (length s1)
			for s2 = (format nil "~A.0" i)
			unless (and (> len1 4)
				    (string-equal s1 s2 :start1 0 :end1 (- len1 2))
				    (eql (char s1 (- len1 1)) #\0)
				    (member (char s1 (- len1 2)) chars))
			collect (list type i f s1 s2)))))))
  nil)

(deftest print.double-float.4
  (let ((chars *possible-double-float-exponent-markers*))
    (loop for type in '(short-float double-float long-float)
	  nconc
	  (and (not (subtypep 'double-float type))
	       (with-standard-io-syntax
		(let ((*print-readably* nil)
		      (*read-default-float-format* type))
		  (loop for i = (- (random 20000000) 10000000)
			for f = (float i 0.0d0)
			for s1 = (with-output-to-string (s) (prin1 f s))
			for len1 = (length s1)
			for s2 = (format nil "~A.0" i)
			repeat 10000
			unless (or (/= i (rational f))  ;; not enough bits
				;; (> (nth-value 1 (integer-decode-float f)) 0)
				(and (> len1 4)
				     (string-equal s1 s2 :start1 0 :end1 (- len1 2))
				     (eql (char s1 (- len1 1)) #\0)
				     (member (char s1 (- len1 2)) chars)))
			collect (list type i f s1 s2)))))))
  nil)

(deftest print.double-float.random
  (let ((lower-bound (if (< (log least-positive-double-float 10) -100)
			 (expt 0.1d0 100)
		       least-positive-double-float))
	(upper-bound (/ (if (> (log most-positive-double-float 10) 100)
			    (expt 10.0d0 100)
			  most-positive-double-float)
			10)))
    (loop for f = lower-bound then (* 10 f)
	  while (< f upper-bound)
	  nconc
	  (loop for x = (handler-case (random f) (arithmetic-error (c) 0.0d0))
		for y = (if (coin) (- x) x)
		repeat 10
		nconc (randomly-check-readability y))))
  nil)

;;; long float

(deftest print.long-float.1
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*read-default-float-format* 'long-float))
     (loop for i from -4000 to 4000
	   for f = (float i 0.0l0)
	   for s1 = (with-output-to-string (s) (prin1 f s))
	   for s2 = (format nil "~A.0" i)
	   unless (equalp s1 s2)
	   collect (list i f s1 s2))))
  nil)

(deftest print.long-float.2
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*read-default-float-format* 'long-float))
     (loop for i = (- (random 20000000) 10000000)
	   for f = (float i 0.0l0)
	   for s1 = (with-output-to-string (s) (prin1 f s))
	   for s2 = (format nil "~A.0" i)
	   repeat 10000
	   unless (or (/= i (rational f)) ;; not enough bits
		   ;; (> (nth-value 1 (integer-decode-float f)) 0)
		   (equalp s1 s2))
	   collect (list i f s1 s2))))
  nil)

(defparameter *possible-long-float-exponent-markers*
  (loop for type in '(short-float single-float double-float long-float)
		     for c across "SFDL"
		     when (subtypep 'long-float type)
		     nconc (list c (char-downcase c))))

(deftest print.long-float.3
  (let ((chars *possible-long-float-exponent-markers*))
    (loop for type in '(short-float double-float long-float)
	  nconc
	  (and (not (subtypep 'long-float type))
	       (with-standard-io-syntax
		(let ((*print-readably* nil)
		      (*read-default-float-format* type))
		  (loop for i from -4000 to 4000
			for f = (float i 0.0l0)
			for s1 = (with-output-to-string (s) (prin1 f s))
			for len1 = (length s1)
			for s2 = (format nil "~A.0" i)
			unless (and (> len1 4)
				    (string-equal s1 s2 :start1 0 :end1 (- len1 2))
				    (eql (char s1 (- len1 1)) #\0)
				    (member (char s1 (- len1 2)) chars))
			collect (list type i f s1 s2)))))))
  nil)

(deftest print.long-float.4
  (let ((chars *possible-long-float-exponent-markers*))
    (loop for type in '(short-float double-float long-float)
	  nconc
	  (and (not (subtypep 'long-float type))
	       (with-standard-io-syntax
		(let ((*print-readably* nil)
		      (*read-default-float-format* type))
		  (loop for i = (- (random 20000000) 10000000)
			for f = (float i 0.0l0)
			for s1 = (with-output-to-string (s) (prin1 f s))
			for len1 = (length s1)
			for s2 = (format nil "~A.0" i)
			repeat 10000
			unless (or (/= i (rational f))  ;; not enough bits
				;; (> (nth-value 1 (integer-decode-float f)) 0)
				(and (> len1 4)
				     (string-equal s1 s2 :start1 0 :end1 (- len1 2))
				     (eql (char s1 (- len1 1)) #\0)
				     (member (char s1 (- len1 2)) chars)))
			collect (list type i f s1 s2)))))))
  nil)

(deftest print.long-float.random
  (let ((lower-bound (if (< (log least-positive-long-float 10) -100)
			 (expt 0.1l0 100)
		       least-positive-long-float))
	(upper-bound (/ (if (> (log most-positive-long-float 10) 100)
			    (expt 10.0l0 100)
			  most-positive-long-float)
			10)))
    (loop for f = lower-bound then (* 10 f)
	  while (< f upper-bound)
	  nconc
	  (loop for x = (handler-case (random f) (arithmetic-error (c) 0.0l0))
		for y = (if (coin) (- x) x)
		repeat 10
		nconc (randomly-check-readability y))))
  nil)