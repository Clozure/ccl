;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 14 19:42:07 2004
;;;; Contains: Tests of MAKE-STRING-OUTPUT-STREAM

(in-package :cl-test)

(deftest make-string-output-stream.1
  (let ((s (make-string-output-stream)))
    (values
     (notnot (typep s 'stream))
     (notnot (typep s 'string-stream))
     (input-stream-p s)
     (notnot (output-stream-p s))
     (notnot (open-stream-p s))))
  t t nil t t)

(deftest make-string-output-stream.2
  (let ((s (make-string-output-stream :element-type 'character)))
    (values
     (notnot (typep s 'stream))
     (notnot (typep s 'string-stream))
     (input-stream-p s)
     (notnot (output-stream-p s))
     (notnot (open-stream-p s))))
  t t nil t t)

(deftest make-string-output-stream.3
  (let ((s (make-string-output-stream :element-type 'base-char)))
    (values
     (notnot (typep s 'stream))
     (notnot (typep s 'string-stream))
     (input-stream-p s)
     (notnot (output-stream-p s))
     (notnot (open-stream-p s))))
  t t nil t t)

(deftest make-string-output-stream.4
  :notes (:nil-vectors-are-strings)
  (let ((s (make-string-output-stream :element-type nil)))
    (values
     (notnot (typep s 'stream))
     (notnot (typep s 'string-stream))
     (input-stream-p s)
     (notnot (output-stream-p s))
     (notnot (open-stream-p s))))
  t t nil t t)

(deftest make-string-output-stream.5
  (let ((s (make-string-output-stream :allow-other-keys nil)))
    (values
     (notnot (typep s 'stream))
     (notnot (typep s 'string-stream))
     (input-stream-p s)
     (notnot (output-stream-p s))
     (notnot (open-stream-p s))))
  t t nil t t)

(deftest make-string-output-stream.6
  (let ((s (make-string-output-stream :allow-other-keys t :foo 'bar)))
    (values
     (notnot (typep s 'stream))
     (notnot (typep s 'string-stream))
     (input-stream-p s)
     (notnot (output-stream-p s))
     (notnot (open-stream-p s))))
  t t nil t t)

(deftest make-string-output-stream.7
  (let ((s (make-string-output-stream :foo 'bar :allow-other-keys t
				      :allow-other-keys nil
				      :foo2 'x)))
    (values
     (notnot (typep s 'stream))
     (notnot (typep s 'string-stream))
     (input-stream-p s)
     (notnot (output-stream-p s))
     (notnot (open-stream-p s))))
  t t nil t t)

(deftest make-string-output-stream.8
  (let ((s (make-string-output-stream)))
    (write-string "abc" s)
    (write-string "def" s)
    (get-output-stream-string s))
  "abcdef")

(deftest make-string-output-stream.9
  (let ((s (make-string-output-stream :element-type 'character)))
    (write-string "abc" s)
    (write-string "def" s)
    (get-output-stream-string s))
  "abcdef")

(deftest make-string-output-stream.10
  (let ((s (make-string-output-stream :element-type 'base-char)))
    (write-string "abc" s)
    (write-string "def" s)
    (get-output-stream-string s))
  "abcdef")

(deftest make-string-output-stream.11
  :notes (:nil-vectors-are-strings)
  (let ((s (make-string-output-stream :element-type nil)))
    (get-output-stream-string s))
  "")

(deftest make-string-output-stream.12
  :notes (:nil-vectors-are-strings)
  (let ((s (make-string-output-stream :element-type nil)))
    (typep #\a (array-element-type (get-output-stream-string s))))
  nil)

(deftest make-string-output-stream.13
  (let ((s (make-string-output-stream)))
    (values
     (close s)
     (open-stream-p s)))
  t nil)

;;; Error tests

(deftest make-string-output-stream.error.1
  (signals-error (make-string-output-stream nil) program-error)
  t)

(deftest make-string-output-stream.error.2
  (signals-error (make-string-output-stream :foo nil) program-error)
  t)

(deftest make-string-output-stream.error.3
  (signals-error (make-string-output-stream :allow-other-keys nil
					    :foo 'bar)
		 program-error)
  t)




