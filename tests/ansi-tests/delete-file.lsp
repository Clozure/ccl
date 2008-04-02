;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 13 18:42:29 2004
;;;; Contains: Tests for DELETE-FILE

(in-package :cl-test)

(deftest delete-file.1
  (let ((pn "scratchfile.txt"))
    (unless (probe-file pn)
      (with-open-file (s pn :direction :output)
		      (format s "Contents~%")))
    (values
     (notnot (probe-file pn))
     (multiple-value-list (delete-file pn))
     (probe-file pn)))
  t (t) nil)

(deftest delete-file.2
  (let ((pn #p"scratchfile.txt"))
    (unless (probe-file pn)
      (with-open-file (s pn :direction :output)
		      (format s "Contents~%")))
    (values
     (notnot (probe-file pn))
     (multiple-value-list (delete-file pn))
     (probe-file pn)))
  t (t) nil)

(deftest delete-file.3
  (let ((pn "CLTEST:scratchfile.txt"))
    (assert (typep (pathname pn) 'logical-pathname))
    (unless (probe-file pn)
      (with-open-file (s pn :direction :output)
		      (format s "Contents~%")))
    (values
     (notnot (probe-file pn))
     (multiple-value-list (delete-file pn))
     (probe-file pn)))
  t (t) nil)

(deftest delete-file.4
  (let ((pn "CLTEST:scratchfile.txt"))
    (assert (typep (pathname pn) 'logical-pathname))
    (unless (probe-file pn)
      (with-open-file (s pn :direction :output)
		      (format s "Contents~%")))
    (let ((s (open pn :direction :input)))
      (close s)
      (values
       (notnot (probe-file pn))
       (multiple-value-list (delete-file s))
       (probe-file pn))))
  t (t) nil)

;;; Specialized string tests

(deftest delete-file.5
  (do-special-strings
   (pn "scratchfile.txt" nil)
   (unless (probe-file pn)
     (with-open-file (s pn :direction :output)
		     (format s "Contents~%")))
   (assert (probe-file pn))
   (assert (equal (multiple-value-list (delete-file pn)) '(t)))
   (assert (not (probe-file pn))))
  nil)

;;; Error tests

(deftest delete-file.error.1
  (signals-error (delete-file) program-error)
  t)

(deftest delete-file.error.2
  (let ((pn "scratch.txt"))
    (unless (probe-file pn)
      (with-open-file (s pn :direction :output)
		      (format s "Contents~%")))
    (values
     (notnot (probe-file pn))
     (signals-error (delete-file "scratch.txt" nil) program-error)
     (notnot (probe-file pn))
     (delete-file pn)
     (probe-file pn)))
  t t t t nil)

#|
(deftest delete-file.error.3
  (let ((pn "nonexistent.txt"))
    (when (probe-file pn) (delete-file pn))
    (signals-error (delete-file "nonexistent.txt") file-error))
  t)
|#

