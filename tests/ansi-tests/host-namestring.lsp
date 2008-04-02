;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep 12 06:22:40 2004
;;;; Contains: Tests of HOST-NAMESTRING

(in-package :cl-test)

(deftest host-namestring.1
  (let* ((vals (multiple-value-list
		(host-namestring "host-namestring.lsp")))
	 (s (first vals)))
    (if (and (null (cdr vals))
	     (or (null s)
		 (stringp s)
		 ;; (equal (host-namestring s) s)
		 ))
	:good
      vals))
  :good)

(deftest host-namestring.2
  (do-special-strings
   (s "host-namestring.lsp" nil)
   (let ((ns (host-namestring s)))
     (when ns
       (assert (stringp ns))
       ;; (assert (string= (host-namestring ns) ns))
       )))
  nil)

(deftest host-namestring.3
  (let* ((name "host-namestring.lsp")
	 (pn (merge-pathnames (pathname name)))
	 (name2 (with-open-file (s pn :direction :input)
				(host-namestring s)))
	 (name3 (host-namestring pn)))
    (or (equalt name2 name3) (list name2 name3)))
  t)

;;; Error tests

(deftest host-namestring.error.1
  (signals-error (host-namestring) program-error)
  t)

(deftest host-namestring.error.2
  (signals-error (host-namestring "host-namestring.lsp" nil) program-error)
  t)

