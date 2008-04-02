;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan  6 05:32:37 2004
;;;; Contains: Tests of TRUENAME

(in-package :cl-test)

(deftest truename.1
  (let* ((pn #p"truename.lsp")
	 (tn (truename pn)))
    (values
     (notnot (pathnamep pn))
     (typep pn 'logical-pathname)
     (equalt (pathname-name pn) (pathname-name tn))
     (equalt (pathname-type pn) (pathname-type tn))
     ))
  t nil t t)

(deftest truename.2
  (let* ((name "truename.lsp")
	 (pn (pathname name))
	 (tn (truename name)))
    (values
     (notnot (pathnamep pn))
     (typep pn 'logical-pathname)
     (equalt (pathname-name pn) (pathname-name tn))
     (equalt (pathname-type pn) (pathname-type tn))
     ))
  t nil t t)

(deftest truename.3
  (let* ((pn #p"truename.lsp"))
    (with-open-file
     (s pn :direction :input)
     (let ((tn (truename s)))
       (values
	(notnot (pathnamep pn))
	(typep pn 'logical-pathname)
	(equalt (pathname-name pn) (pathname-name tn))
	(equalt (pathname-type pn) (pathname-type tn))
	))))
  t nil t t)

(deftest truename.4
  (let* ((pn #p"truename.lsp"))
    (let ((s (open pn :direction :input)))
      (close s)
      (let ((tn (truename s)))
	(values
	 (notnot (pathnamep pn))
	 (typep pn 'logical-pathname)
	 (equalt (pathname-name pn) (pathname-name tn))
	 (equalt (pathname-type pn) (pathname-type tn))
	 ))))
  t nil t t)

(deftest truename.5
  (let* ((lpn "CLTEST:foo.txt")
	 (pn (translate-logical-pathname lpn)))
    (unless (probe-file lpn)
      (with-open-file (s lpn :direction :output) (format s "Stuff~%")))
    (let ((tn (truename lpn)))
      (values
       (notnot (pathnamep pn))
       (if (equalt (pathname-name pn) (pathname-name tn))
	   t (list (pathname-name pn) (pathname-name tn)))
       (if (equalt (pathname-type pn) (pathname-type tn))
	   t (list (pathname-type pn) (pathname-type tn)))
       )))
  t t t)

;;; Specialized string tests

(deftest truename.6
  (do-special-strings
   (s "truename.lsp" nil)
   (assert (equalp (truename s) (truename "truename.lsp"))))
  nil)

;;; Error tests

(deftest truename.error.1
  (signals-error (truename) program-error)
  t)

(deftest truename.error.2
  (signals-error (truename "truename.lsp" nil) program-error)
  t)

(deftest truename.error.3
  (signals-error-always (truename "nonexistent") file-error)
  t t)

(deftest truename.error.4
  (signals-error-always (truename #p"nonexistent") file-error)
  t t)

(deftest truename.error.5
  (signals-error-always (truename (logical-pathname "CLTESTROOT:nonexistent")) file-error)
  t t)

(deftest truename.error.6
  (signals-error-always
   (let ((pn (make-pathname :name :wild
			    :defaults *default-pathname-defaults*)))
     (truename pn))
   file-error)
  t t)
