;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jan 22 21:34:04 2004
;;;; Contains: Tests of FILE-STRING-LENGTH

(in-package :cl-test)

(deftest file-string-length.1
  (with-open-file 
    (s "tmp.dat" :direction :output
       :if-exists :supersede)
    (loop for x across +standard-chars+
	  for len = (file-string-length s x)
	  do (assert (typep len '(or null (integer 0))))
	  do (let ((pos1 (file-position s)))
	       (write-char x s)
	       (let ((pos2 (file-position s)))
		 (when (and pos1 pos2 len)
		   (assert (= (+ pos1 len) pos2)))))))
  nil)

(deftest file-string-length.2
  (with-open-file 
    (s "tmp.dat" :direction :output
       :if-exists :supersede)
    (loop for x across +standard-chars+
	  for len = (file-string-length s (string x))
	  do (assert (typep len '(or null (integer 0))))
	  do (let ((pos1 (file-position s)))
	       (write-sequence (string x) s)
	       (let ((pos2 (file-position s)))
		 (when (and pos1 pos2 len)
		   (assert (= (+ pos1 len) pos2)))))))
  nil)

(deftest file-string-length.3
  (with-open-file
   (stream "tmp.dat" :direction :output
	   :if-exists :supersede)
   (let* ((s1 "abcde")
	  (n (file-string-length stream s1)))
     (do-special-strings
      (s2 s1 nil)
      (assert (= (file-string-length stream s2) n)))))
  nil)

;;; Error tests

(deftest file-string-length.error.1
  (signals-error (file-string-length) program-error)
  t)

(deftest file-string-length.error.2
  (signals-error
   (with-open-file 
    (s "tmp.dat" :direction :output
       :if-exists :supersede)
    (file-string-length s))
   program-error)
  t)

(deftest file-string-length.error.3
  (signals-error
   (with-open-file 
    (s "tmp.dat" :direction :output
       :if-exists :supersede)
    (file-string-length s #\x nil))
   program-error)
  t)


  

