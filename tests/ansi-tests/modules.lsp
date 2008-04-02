;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 30 19:51:06 2005
;;;; Contains: Tests of *MODULES*, PROVIDE, and REQUIRE

(in-package :cl-test)

(deftest modules.1
  (notnot (every #'stringp *modules*))
  t)

(deftest modules.2
  (let ((*modules* *modules*))
    (provide "FOO")
    (notnot (member "FOO" *modules* :test #'string=)))
  t)

(deftest modules.3
  (let ((*modules* *modules*))
    (provide "FOO")
    (provide "FOO")
    (count "FOO" *modules* :test #'string=))
  1)

(deftest modules.4
  (let ((*modules* *modules*))
    (provide "FOO")
    (require "FOO")
    (values)))

(deftest modules.5
  (let ((*modules* *modules*))
    (provide :|FOO|)
    (notnot (member "FOO" *modules* :test #'string=)))
  t)

(deftest modules.6
  (let ((*modules* *modules*))
    (provide "FOO")
    (require :|FOO|)
    (values)))

(deftest modules.7
  (let ((*modules* *modules*)
	(fn 'modules7-fun))
    (when (fboundp fn) (fmakunbound fn))
    (require "MODULES-7" #p"modules7.lsp")
    (funcall fn))
  :good)

(deftest modules.8
  (let ((*modules* *modules*)
	(fns '(modules8a-fun modules8b-fun)))
    (dolist (fn fns)
      (when (fboundp fn) (fmakunbound fn)))
    (require "MODULES-8" '(#p"modules8a.lsp" #p"modules8b.lsp"))
    (mapcar #'funcall fns))
  (:good :also-good))

(deftest modules.9
  (signals-error (require "AB7djaCgaaL") error)
  t)

(deftest modules.10
  (do-special-strings
   (s "FOO")
   (let ((*modules* *modules*))
     (provide s)
     (assert (member "FOO" *modules* :test #'string=))))
  nil)

(deftest modules.11
  (do-special-strings
   (s "FOO")
   (let ((*modules* *modules*))
     (provide "FOO")
     (require s)
     (values)))
  nil)

(deftest modules.12
  (unless (member "Z" *modules* :test #'string=)
    (let ((*modules* *modules*))
      (provide #\Z)
      (not (member "Z" *modules* :test #'string=))))
  nil)

(deftest modules.13
  (unless (member "Z" *modules* :test #'string=)
    (let ((*modules* *modules*))
      (provide "Z")
      (require #\Z)
      nil))
  nil)