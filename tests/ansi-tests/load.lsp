;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Apr 12 21:51:49 2005
;;;; Contains: Tests of LOAD

(in-package :cl-test)

(defun load-file-test (file funname &rest args &key
			    if-does-not-exist
			    (print nil print-p)
			    (verbose nil verbose-p)
			    (*load-print* nil)
			    (*load-verbose* nil)
			    external-format)
  (declare (ignorable external-format if-does-not-exist
		      print print-p verbose verbose-p))
  (fmakunbound funname)
  (let* ((str (make-array '(0) :element-type 'character :adjustable t
			  :fill-pointer 0))
	 (vals (multiple-value-list
		(with-output-to-string
		  (*standard-output* str)
		  (apply #'load file :allow-other-keys t args))))
	 (print? (if print-p print *load-print*))
	 (verbose? (if verbose-p verbose *load-verbose*)))
      (values
       (let ((v1 (car vals))
	     (v2 (or (and verbose-p (not verbose))
		     (and (not verbose-p) (not *load-verbose*))
		     (position #\; str)))
	     (v3 (or (and print-p (not print))
		     (and (not print-p) (not *load-print*))
		     (> (length str) 0)))
	     (v4 (if (or print? verbose?)
		     (> (length str) 0)
		   t)))
	 (if (and (= (length vals) 1) v1 v2 v3 v4) t (list vals v2 v3 v4 str)))
       (funcall funname))))

(deftest load.1
  (load-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1)
  t nil)

(deftest load.2
  (load-file-test #p"compile-file-test-file.lsp" 'compile-file-test-fun.1)
  t nil)

(deftest load.3
  (with-input-from-string
   (s "(in-package :cl-test) (defun load-file-test-fun.2 () 'good)")
   (load-file-test s 'load-file-test-fun.2))
  t good)

(deftest load.4
  (load-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		  :external-format :default)
  t nil)

(deftest load.5
  (load-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		  :verbose t)
  t nil)

(deftest load.6
  (load-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		  :*load-verbose* t)
  t nil)

(deftest load.7
  (load-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		  :*load-verbose* t :verbose nil)
  t nil)

(deftest load.8
  (with-input-from-string
   (s "(in-package :cl-test) (defun load-file-test-fun.2 () 'good)")
   (load-file-test s 'load-file-test-fun.2 :verbose t))
  t good)

(deftest load.9
  (load-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		  :print t)
  t nil)

(deftest load.10
  (load-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		  :*load-print* t)
  t nil)

(deftest load.11
  (load-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		  :*load-print* t :print nil)
  t nil)

(deftest load.12
  (load-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		  :*load-print* nil :print t)
  t nil)

(deftest load.13
  (with-input-from-string
   (s "(in-package :cl-test) (defun load-file-test-fun.2 () 'good)")
   (load-file-test s 'load-file-test-fun.2 :print t))
  t good)

(deftest load.14
  (load "nonexistent-file.lsp" :if-does-not-exist nil)
  nil)

(defpackage LOAD-TEST-PACKAGE (:use "COMMON-LISP"))

(deftest load.15
  (let ((*package* (find-package "LOAD-TEST-PACKAGE")))
    (with-input-from-string
     (s "(defun f () 'good)")
     (load-file-test s 'load-test-package::f)))
  t load-test-package::good)

(deftest load.15a
  (let ((*package* (find-package "CL-TEST")))
    (values
     (with-input-from-string
      (s "(eval-when (:load-toplevel :execute) (setq *package* (find-package \"LOAD-TEST-PACKAGE\")))
          (defun f () 'good)")
      (multiple-value-list (load-file-test s 'load-test-package::f)))
     (read-from-string "GOOD")))
  (t load-test-package::good) good)

(deftest load.16
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\! (get-macro-character #\'))
    (with-input-from-string
     (s "(in-package :cl-test) (defun load-file-test-fun.3 () !good)")
     (load-file-test s 'load-file-test-fun.3)))
  t good)

(deftest load.16a
  (let ((*readtable* *readtable*)
	(*package* (find-package "CL-TEST")))
    (values
     (with-input-from-string
      (s "(in-package :cl-test)
         (eval-when (:load-toplevel :execute)
            (setq *readtable* (copy-readtable nil))
            (set-macro-character #\\! (get-macro-character #\\')))
         (defun load-file-test-fun.3 () !good)")
      (multiple-value-list
       (load-file-test s 'load-file-test-fun.3)))
     (read-from-string "!FOO")))
  (t good) !FOO)

(deftest load.17
  (let ((file #p"load-test-file.lsp"))
    (fmakunbound 'load-file-test-fun.1)
    (fmakunbound 'load-file-test-fun.2)
    (values
     (notnot (load file))
     (let ((p1 (pathname (merge-pathnames file)))
	   (p2 (funcall 'load-file-test-fun.1)))
       (equalpt-or-report p1 p2))
     (let ((p1 (truename file))
	   (p2 (funcall 'load-file-test-fun.2)))
       (equalpt-or-report p1 p2))))
  t t t)

;;; Test that the load pathname/truename variables are bound
;;; properly when loading compiled files

(deftest load.18
  (let* ((file "load-test-file-2.lsp")
	 (target (enough-namestring (compile-file-pathname file))))
    (declare (special *load-test-var.1* *load-test-var.2*))
    (compile-file file)
    (makunbound '*load-test-var.1*)
    (makunbound '*load-test-var.2*)
    (load target)
    (values
     (let ((p1 (pathname (merge-pathnames target)))
	   (p2 *load-test-var.1*))
       (equalpt-or-report p1 p2))
     (let ((p1 (truename target))
	   (p2 *load-test-var.2*))
       (equalpt-or-report p1 p2))))
  t t)

(deftest load.19
  (let ((file (logical-pathname "CLTEST:LDTEST.LSP"))
	(fn 'load-test-fun-3)
	(*package* (find-package "CL-TEST")))
    (with-open-file
     (s file :direction :output :if-exists :supersede
	:if-does-not-exist :create)
     (format s "(in-package :cl-test) (defun ~a () :foo)" fn))
    (fmakunbound fn)
    (values
     (notnot (load file))
     (funcall fn)))
  t :foo)

;;; Defaults of the load variables

(deftest load-pathname.1
  *load-pathname*
  nil)

(deftest load-truename.1
  *load-truename*
  nil)

(deftest load-print.1
  *load-print*
  nil)

;;; Error tests

(deftest load.error.1
  (signals-error (load "nonexistent-file.lsp") file-error)
  t)

(deftest load.error.2
  (signals-error (load) program-error)
  t)

(deftest load.error.3
  (signals-error (load "compile-file-test-file.lsp" :bad-key-arg t)
		 program-error)
  t)
