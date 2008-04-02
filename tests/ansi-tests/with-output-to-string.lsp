;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 14 20:33:51 2004
;;;; Contains: Tests of WITH-OUTPUT-TO-STRING

(in-package :cl-test)


(deftest with-output-to-string.1
  (with-output-to-string (s))
  "")

(deftest with-output-to-string.2
  (with-output-to-string (s) (write-char #\3 s))
  "3")

(deftest with-output-to-string.3
  (with-output-to-string (s (make-array 10 :fill-pointer 0
					:element-type 'character)))
  nil)

(deftest with-output-to-string.4
  :notes (:allow-nil-arrays :nil-vectors-are-strings)
  (let ((str (make-array 10 :fill-pointer 0 :element-type 'character)))
    (values
     (with-output-to-string
       (s str :element-type nil)
       (write-string "abcdef" s))
     str))
  "abcdef" "abcdef")

(deftest with-output-to-string.5
  (with-output-to-string (s (make-array 10 :fill-pointer 0
					:element-type 'character))
			 (values)))

(deftest with-output-to-string.6
  (with-output-to-string (s (make-array 10 :fill-pointer 0
					:element-type 'character))
			 (values 'a 'b 'c 'd))
  a b c d)

(deftest with-output-to-string.7
  (with-output-to-string (s nil :element-type 'character)
			 (write-char #\& s))
  "&")

(deftest with-output-to-string.8
  (let ((str (with-output-to-string (s nil :element-type 'base-char)
				    (write-char #\8 s))))
    (assert (typep str 'simple-base-string))
    str)
  "8")

(deftest with-output-to-string.9
  :notes (:allow-nil-arrays :nil-vectors-are-strings)
  (with-output-to-string (s nil :element-type nil))
  "")

(deftest with-output-to-string.10
  (let* ((s1 (make-array 20 :element-type 'character
			 :initial-element #\.))
	 (s2 (make-array 10 :element-type 'character
			 :displaced-to s1
			 :displaced-index-offset 5
			 :fill-pointer 0)))

    (values
     (with-output-to-string
       (s s2)
       (write-string "0123456789" s))
     s1
     s2))
  "0123456789"
  ".....0123456789....."
  "0123456789")

(deftest with-output-to-string.11
  (with-output-to-string (s) (declare (optimize safety)))
  "")

(deftest with-output-to-string.12
  (with-output-to-string (s) (declare (optimize safety))
			 (declare (optimize (speed 0))))
  "")

(deftest with-output-to-string.13
  (with-output-to-string
    (s)
    (write-char #\0 s)
    (write-char #\4 s)
    (write-char #\9 s))
  "049")

(deftest with-output-to-string.14
  (let* ((str1 (make-array '(256) :element-type 'base-char :fill-pointer 0))
	 (str2 (with-output-to-string
		 (s nil :element-type 'base-char)
		 (loop for i below 256
		       for c = (code-char i)
		       when (typep c 'base-char)
		       do (progn (write-char c s)
				 (vector-push c str1))))))
    (if (string= str1 str2) :good
      (list str1 str2)))
  :good)

;;; Free declaration scope

(deftest with-output-to-string.15
  (block done
    (let ((x :bad))
      (declare (special x))
      (let ((x :good))
	(with-output-to-string (s (return-from done x))
			       (declare (special x))))))
  :good)

(deftest with-output-to-string.16
  (block done
    (let ((x :bad))
      (declare (special x))
      (let ((x :good)
	    (str (make-array '(10) :element-type 'character
			     :fill-pointer 0)))
	(with-output-to-string (s str :element-type (return-from done x))
			       (declare (special x))))))
  :good)

