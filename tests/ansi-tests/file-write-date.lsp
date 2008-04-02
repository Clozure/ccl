;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan  6 06:01:35 2004
;;;; Contains: Tests for FILE-WRITE-DATE

(in-package :cl-test)

(deftest file-write-date.1
  (let* ((pn "file-write-date.lsp")
	 (date (file-write-date pn))
	 (time (get-universal-time)))
    (or (null date)
	(and (integerp date)
	     (<= 0 date time)
	     t)))
  t)

(deftest file-write-date.2
  (let* ((pn #p"file-write-date.lsp")
	 (date (file-write-date pn))
	 (time (get-universal-time)))
    (or (null date)
	(and (integerp date)
	     (<= 0 date time)
	     t)))
  t)
	     
(deftest file-write-date.3
  (let* ((pn (truename "file-write-date.lsp"))
	 (date (file-write-date pn))
	 (time (get-universal-time)))
    (or (null date)
	(and (integerp date)
	     (<= 0 date time)
	     t)))
  t)

(deftest file-write-date.4
  (loop for pn in (directory
		   (make-pathname :name :wild :type :wild
				  :defaults *default-pathname-defaults*))
	for date = (file-write-date pn)
	for time = (get-universal-time)
	unless (or (null date)
		   (<= 0 date time))
	collect (list pn date time))
  nil)

(deftest file-write-date.5
  (length (multiple-value-list (file-write-date "file-write-date.lsp")))
  1)

;;; Specialized string tests

(deftest file-write-date.6
  (let* ((str "file-write-date.lsp")
	 (date (file-write-date str)))
    (do-special-strings
     (s str nil)
     (assert (equal (file-write-date s) date))))
  nil)

;;; FIXME
;;; Add LPN test

;;; Error tests

(deftest file-write-date.error.1
  (signals-error (file-write-date) program-error)
  t)

(deftest file-write-date.error.2
  (signals-error (file-write-date "file-write-date.lsp" nil)
		 program-error)
  t)

(deftest file-write-date.error.3
  (signals-error-always
   (file-write-date (make-pathname :name :wild :type "lsp"
				   :defaults *default-pathname-defaults*))
   file-error)
  t t)

(deftest file-write-date.error.4
  (signals-error-always
   (file-write-date (make-pathname :name "file-write-date" :type :wild
				   :defaults *default-pathname-defaults*))
   file-error)
  t t)
