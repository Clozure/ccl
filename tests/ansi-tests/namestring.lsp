;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep  2 07:24:42 2004
;;;; Contains: Tests for NAMESTRING

(in-package :cl-test)

(deftest namestring.1
  (let* ((vals (multiple-value-list (namestring "namestring.lsp")))
	 (s (first vals)))
    (if (and (null (cdr vals))
	     (stringp s)
	     (equal (namestring s) s))
	:good
      vals))
  :good)

(deftest namestring.2
  (do-special-strings
   (s "namestring.lsp" nil)
   (let ((ns (namestring s)))
     (assert (stringp ns))
     (assert (string= (namestring ns) ns))))
  nil)

;;; I'm not convinced these tested required behavior, so I'm commenting
;;; them out for now.  FIXME: determine if they are bogus
#|
(deftest namestring.3
  (let* ((name "namestring.lsp")
	 (pn (merge-pathnames (pathname name)))
	 (name2 (namestring pn))
	 (pn2 (pathname name2)))
    (or (equalt pn pn2) (list (list pn (pathname-host pn) (pathname-device pn)
				    (pathname-directory pn) (pathname-name pn)
				    (pathname-type pn) (pathname-version pn))
			      (list pn2 (pathname-host pn2) (pathname-device pn2)
				    (pathname-directory pn2) (pathname-name pn2)
				    (pathname-type pn2) (pathname-version pn2)))))
  t)

(deftest namestring.4
  (let* ((name "namestring.lsp")
	 (pn (merge-pathnames (pathname name)))
	 (name2 (with-open-file (s pn :direction :input) (namestring s)))
	 (pn2 (pathname name2)))
    (or (equalt pn pn2) (list (list pn (pathname-host pn) (pathname-device pn)
				    (pathname-directory pn) (pathname-name pn)
				    (pathname-type pn) (pathname-version pn))
			      (list pn2 (pathname-host pn2) (pathname-device pn2)
				    (pathname-directory pn2) (pathname-name pn2)
				    (pathname-type pn2) (pathname-version pn2)))))
  t)
|#

;;; Error tests

(deftest namestring.error.1
  (signals-error (namestring) program-error)
  t)

(deftest namestring.error.2
  (signals-error (namestring "namestring.lsp" nil) program-error)
  t)
