;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 14 08:43:45 2004
;;;; Contains: Tests of CONCATENATED-STREAM-STREAMS

(in-package :cl-test)

(deftest concatenated-stream-streams.1
  (concatenated-stream-streams (make-concatenated-stream))
  nil)

(deftest concatenated-stream-streams.2
  (equalt (list (list *standard-input*))
	  (multiple-value-list
	   (concatenated-stream-streams
	    (make-concatenated-stream *standard-input*))))
  t)

(deftest concatenated-stream-streams.3
  (with-input-from-string
   (s1 "abc")
   (with-input-from-string
    (s2 "def")
    (let ((s (make-concatenated-stream s1 s2)))
      (equalt (list (list s1 s2))
	      (multiple-value-list
	       (concatenated-stream-streams s))))))
  t)

(deftest concatenated-stream-streams.4
  (with-input-from-string
   (s1 "")
   (with-input-from-string
    (s2 "def")
    (let ((s (make-concatenated-stream s1 s2)))
      (equalt (list (list s1 s2))
	      (multiple-value-list
	       (concatenated-stream-streams s))))))
  t)

(deftest concatenated-stream-streams.5
  (with-input-from-string
   (s1 "")
   (with-input-from-string
    (s2 "def")
    (let ((s (make-concatenated-stream s1 s2)))
      (values
       (read-char s)
       (equalt (list (list s2))
	       (multiple-value-list
		(concatenated-stream-streams s)))))))
  #\d t)

;;; Error cases

(deftest concatenated-stream-streams.error.1
  (signals-error (concatenated-stream-streams) program-error)
  t)

(deftest concatenated-stream-streams.error.2
  (signals-error (concatenated-stream-streams
		  (make-concatenated-stream)
		  nil)
		 program-error)
  t)


