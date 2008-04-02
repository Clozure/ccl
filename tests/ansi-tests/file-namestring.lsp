;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep 11 07:40:47 2004
;;;; Contains: Tests for FILE-NAMESTRING

(in-package :cl-test)

(deftest file-namestring.1
  (let* ((vals (multiple-value-list
		(file-namestring "file-namestring.lsp")))
	 (s (first vals)))
    (if (and (null (cdr vals))
	     (stringp s)
	     (equal (file-namestring s) s))
	:good
      vals))
  :good)

(deftest file-namestring.2
  (do-special-strings
   (s "file-namestring.lsp" nil)
   (let ((ns (file-namestring s)))
     (assert (stringp ns))
     (assert (string= (file-namestring ns) ns))))
  nil)

(deftest file-namestring.3
  (let* ((name "file-namestring.lsp")
	 (pn (merge-pathnames (pathname name)))
	 (name2 (with-open-file (s pn :direction :input)
				(file-namestring s)))
	 (name3 (file-namestring pn)))
    (or (equalt name2 name3) (list name2 name3)))
  t)

;;; Error tests

(deftest file-namestring.error.1
  (signals-error (file-namestring) program-error)
  t)

(deftest file-namestring.error.2
  (signals-error (file-namestring "file-namestring.lsp" nil) program-error)
  t)
