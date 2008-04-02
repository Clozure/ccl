;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep 12 06:21:42 2004
;;;; Contains: Tests for DIRECTORY-NAMESTRING

(in-package :cl-test)

(deftest directory-namestring.1
  (let* ((vals (multiple-value-list
		(directory-namestring "directory-namestring.lsp")))
	 (s (first vals)))
    (if (and (null (cdr vals))
	     (stringp s)
	     (equal (directory-namestring s) s))
	:good
      vals))
  :good)

(deftest directory-namestring.2
  (do-special-strings
   (s "directory-namestring.lsp" nil)
   (let ((ns (directory-namestring s)))
     (assert (stringp ns))
     (assert (string= (directory-namestring ns) ns))))
  nil)

;;; Lispworks makes another assumption about filename normalization
;;; when using file streams as pathname designators, so this test
;;; doesn't work there.
;;; (This is another example of the difficulty of testing a feature
;;;  in which so much is left up to the implementation.)
#-lispworks
(deftest directory-namestring.3
  (let* ((name "directory-namestring.lsp")
	 (pn (merge-pathnames (pathname name)))
	 (name2 (with-open-file (s pn :direction :input)
				(directory-namestring s)))
	 (name3 (directory-namestring pn)))
    (or (equalt name2 name3) (list name2 name3)))
  t)

;;; Error tests

(deftest directory-namestring.error.1
  (signals-error (directory-namestring) program-error)
  t)

(deftest directory-namestring.error.2
  (signals-error (directory-namestring "directory-namestring.lsp" nil) program-error)
  t)
