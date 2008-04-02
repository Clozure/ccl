;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 15 07:46:22 2004
;;;; Contains: Tests for PATHNAME-MATCH-P

(in-package :cl-test)

(compile-and-load "pathnames-aux.lsp")

;;; Much of the behavior cannot be tested portably.

(deftest pathname-match-p.1
  (let ((pn1 (make-pathname :name :wild))
	(pn2 (make-pathname :name "foo")))
    (pathname-match-p pn1 pn2))
  nil)

(deftest pathname-match-p.2
  (let ((pn1 (make-pathname :type :wild))
	(pn2 (make-pathname :type "txt")))
    (pathname-match-p pn1 pn2))
  nil)

(deftest pathname-match-p.3
  (let ((pn1 (make-pathname :directory '(:absolute :wild)))
	(pn2 (make-pathname :directory '(:absolute))))
    (pathname-match-p pn1 pn2))
  nil)

(deftest pathname-match-p.4
  (let ((pn1 (make-pathname :directory '(:relative :wild)))
	(pn2 (make-pathname :directory '(:relative))))
    (pathname-match-p pn1 pn2))
  nil)

(deftest pathname-match-p.5
  (let ((pn1 (make-pathname :directory '(:relative :wild)))
	(pn2 (make-pathname :directory nil)))
    (and (wild-pathname-p pn1)
	 (not (pathname-directory pn2))
	 (not (pathname-match-p pn1 pn2))))
  nil)

(deftest pathname-match-p.6
  (let ((pn1 (make-pathname :version :wild))
	(pn2 (make-pathname)))
    (and (wild-pathname-p pn1)
	 (not (pathname-version pn2))
	 (not (pathname-match-p pn1 pn2))))
  nil)

;;; Specialized string tests

(deftest pathname-match-p.7
  (let ((wpn (parse-namestring "CLTEST:*.LSP")))
    (assert (wild-pathname-p wpn))
    (do-special-strings
     (s "CLTEST:FOO.LSP" nil)
     (assert (pathname-match-p s wpn))))
  nil)

(deftest pathname-match-p.8
  (do-special-strings
   (s "CLTEST:*.LSP" nil)
   (assert (pathname-match-p "CLTEST:FOO.LSP" s)))
  nil)
   

;;; Add more tests here

;;; Here are error tests

(deftest pathname-match-p.error.1
  (signals-error (pathname-match-p) program-error)
  t)

(deftest pathname-match-p.error.2
  (signals-error (pathname-match-p #p"") program-error)
  t)

(deftest pathname-match-p.error.3
  (signals-error (pathname-match-p #p"" #p"" nil) program-error)
  t)

(deftest pathname-match-p.error.4
  (check-type-error #'(lambda (x) (pathname-match-p x #p""))
		    #'could-be-pathname-designator)
  nil)

(deftest pathname-match-p.error.5
  (check-type-error #'(lambda (x) (declare (optimize (safety 0))) (pathname-match-p x #p""))
		    #'could-be-pathname-designator)
  nil)

(deftest pathname-match-p.error.6
  (check-type-error #'(lambda (x) (pathname-match-p #p"" x))
		    #'could-be-pathname-designator)
  nil)

(deftest pathname-match-p.error.7
  (check-type-error #'(lambda (x) (declare (optimize (safety 0))) (pathname-match-p #p"" x))
		    #'could-be-pathname-designator)
  nil)
