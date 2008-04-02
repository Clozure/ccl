;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep 12 06:23:50 2004
;;;; Contains: Tests of ENOUGH-NAMESTRING

(in-package :cl-test)

(deftest enough-namestring.1
  (let* ((vals (multiple-value-list (enough-namestring "enough-namestring.lsp")))
	 (s (first vals)))
    (if (and (null (cdr vals))
	     (stringp s)
	     (equal (enough-namestring s) s))
	:good
      vals))
  :good)

(deftest enough-namestring.2
  (do-special-strings
   (s "enough-namestring.lsp" nil)
   (let ((ns (enough-namestring s)))
     (assert (stringp ns))
     (assert (string= (enough-namestring ns) ns))))
  nil)

(deftest enough-namestring.3
  (let* ((name "enough-namestring.lsp")
	 (pn (merge-pathnames (pathname name)))
	 (name2 (enough-namestring pn))
	 (name3 (enough-namestring name)))
    (or (equalt name2 name3) (list name2 name3)))
  t)

(deftest enough-namestring.4
  (let* ((name "enough-namestring.lsp")
	 (pn (merge-pathnames (pathname name)))
	 (name2 (with-open-file (s pn :direction :input) (enough-namestring s)))
	 (name3 (enough-namestring name)))
    (or (equalt name2 name3) (list name2 name3)))
  t)

(deftest enough-namestring.5
  (let* ((vals (multiple-value-list (enough-namestring "enough-namestring.lsp"
						       *default-pathname-defaults*)))
	 (s (first vals)))
    (if (and (null (cdr vals))
	     (stringp s)
	     (equal (enough-namestring s) s))
	:good
      vals))
  :good)

(deftest enough-namestring.6
  (let* ((vals (multiple-value-list (enough-namestring "enough-namestring.lsp"
						       (namestring *default-pathname-defaults*))))
	 (s (first vals)))
    (if (and (null (cdr vals))
	     (stringp s)
	     (equal (enough-namestring s) s))
	:good
      vals))
  :good)

(deftest enough-namestring.7
  (do-special-strings
   (s (namestring *default-pathname-defaults*) nil)
   (let* ((vals (multiple-value-list (enough-namestring "enough-namestring.lsp" s)))
	 (s2 (first vals)))
     (assert (null (cdr vals)))
     (assert (stringp s2))
     (assert (equal (enough-namestring s2) s2))))
  nil)

;;; Error tests

(deftest enough-namestring.error.1
  (signals-error (enough-namestring) program-error)
  t)

(deftest enough-namestring.error.2
  (signals-error
   (enough-namestring "enough-namestring.lsp" *default-pathname-defaults* nil)
   program-error)
  t)
