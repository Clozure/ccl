;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr  9 08:25:25 2005
;;;; Contains: Tests of COMPILE-FILE

(in-package :cl-test)

(defun compile-file-test (file funname &rest args &key
			       expect-warnings 
			       expect-style-warnings output-file
			       (print nil print-p)
			       (verbose nil verbose-p)
			       (*compile-print* nil)
			       (*compile-verbose* nil)
			       external-format)
  (declare (ignorable external-format))
  (let* ((target-pathname (or output-file
			      (compile-file-pathname file)))
	 (actual-warnings-p nil)
	 (actual-style-warnings-p nil))
    (when (probe-file target-pathname)
      (delete-file target-pathname))
    (fmakunbound funname)
    (let* ((str (make-array '(0) :element-type 'character :adjustable t :fill-pointer 0))
	   (vals (multiple-value-list
		  (handler-bind
		   ((style-warning #'(lambda (c)
				       (declare (ignore c))
				       (setf actual-style-warnings-p t)
				       nil))
		    ((or error warning)
		     #'(lambda (c)
			 (unless (typep c 'style-warning)
			   (setf actual-warnings-p t))
			 nil)))
		   (with-output-to-string
		     (*standard-output* str)
		     (apply #'compile-file file :allow-other-keys t args))))))
      (assert (= (length vals) 3))
      (destructuring-bind
	  (output-truename warnings-p failure-p)
	  vals
	(print (namestring (truename target-pathname)))
	(print (namestring output-truename))
	(values
	 (let ((v1 (or print verbose
		       (and (not print-p) *compile-print*)
		       (and (not verbose-p) *compile-verbose*)
		       (string= str "")))
	       (v2 (or (and verbose-p (not verbose))
		       (and (not verbose-p) (not *compile-verbose*))
		       (position #\; str)))
	       (v3 (if actual-warnings-p failure-p t))
	       (v4 (if expect-warnings failure-p t))
	       (v5 (if expect-style-warnings warnings-p t))
	       (v6 (or (null output-truename) (pathnamep output-truename)))
	       (v7 (equalpt-or-report (namestring (truename target-pathname))
				      (namestring output-truename)))
	       (v8 (not (fboundp funname))))
	   (if (and v1 v2 v3 v4 v5 v6 (eql v7 t) v8) t
	     (list v1 v2 v3 v4 v5 v6 v7 v8)))
	 (progn
	   (load output-truename)
	   (funcall funname)))))))

(deftest compile-file.1
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1)
  t nil)

(deftest compile-file.2
  (compile-file-test "compile-file-test-file-2.lsp" 'compile-file-test-fun.2
		     :expect-style-warnings t)
  t nil)

(deftest compile-file.2a
  (compile-file-test "compile-file-test-file-2a.lsp" 'compile-file-test-fun.2a
		     :expect-warnings t)
  t nil)

(deftest compile-file.3
  (let ((*package* (find-package "CL-TEST")))
    (compile-file-test "compile-file-test-file-3.lsp" 'compile-file-test-fun.3))
  t nil)

(deftest compile-file.4
  (let ((*package* (find-package "CL-USER")))
    (compile-file-test "compile-file-test-file-3.lsp" 'cl-user::compile-file-test-fun.3))
  t nil)

(deftest compile-file.5
  (compile-file-test #p"compile-file-test-file.lsp" 'compile-file-test-fun.1)
  t nil)

(deftest compile-file.6
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :output-file "foo.fasl")
  t nil)

(deftest compile-file.6a
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :output-file "foo.ufsl")
  t nil)

(deftest compile-file.7
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :external-format :default)
  t nil)

(deftest compile-file.8
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :output-file #p"foo.fasl")
  t nil)

(deftest compile-file.9
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :print t)
  t nil)

(deftest compile-file.10
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :verbose t)
  t nil)

(deftest compile-file.11
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :print nil)
  t nil)

(deftest compile-file.12
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :verbose nil)
  t nil)

;;; A file stream is a pathname designator
(deftest compile-file.13
  (with-open-file (s "compile-file-test-file.lsp" :direction :input)
		  (compile-file-test s 'compile-file-test-fun.1))
  t nil)

(deftest compile-file.14
  (let ((s (open "foo.fasl" :direction :output :if-exists :supersede
		 :if-does-not-exist :create)))
    (close s)
    (compile-file-test "compile-file-test-file.lsp"
		       'compile-file-test-fun.1
		       :output-file s))
  t nil)

(deftest compile-file.15
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\! (get-macro-character #\'))
    (compile-file-test "compile-file-test-file-4.lsp" 'compile-file-test-fun.4))
  t foo)

;;; Tests for *compile-file-truename*, *compile-file-pathname*

(deftest compile-file.16
  (let* ((file #p"compile-file-test-file-5.lsp")
	 (target-pathname (compile-file-pathname file))
	 (*compile-print* nil)
	 (*compile-verbose* nil))
    (when (probe-file target-pathname)
      (delete-file target-pathname))
    (compile-file file)
    (load target-pathname)
    (values
     (equalpt-or-report (truename file) (funcall 'compile-file-test-fun.5))
     (equalpt-or-report (pathname (merge-pathnames file))
			(funcall 'compile-file-test-fun.5a))))
  t t)

;;; Add tests of logical pathnames

(deftest compile-file.17
  (let ((file (logical-pathname "CLTEST:COMPILE-FILE-TEST-LP.LSP")))
    (with-open-file
     (s file :direction :output :if-exists :supersede :if-does-not-exist :create)
     (format s "(in-package :cl-test)~%(defun compile-file-test-lp.fun () nil)~%"))
    (compile-file-test file 'compile-file-test-lp.fun))
  t nil)

(deftest compile-file.18
  (let ((file (logical-pathname "CLTEST:COMPILE-FILE-TEST-LP.OUT")))
    (with-open-file
     (s file :direction :output :if-exists :supersede :if-does-not-exist :create))
    (compile-file-test "compile-file-test-file.lsp"
		       'compile-file-test-fun.1
		       :output-file file))
  t nil)

(deftest compile-file.19
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :*compile-verbose* t)
  t nil)

(deftest compile-file.20
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :*compile-print* t)
  t nil)  

(deftest compile-file-pathname.1
  *compile-file-pathname*
  nil)

(deftest compile-file-truename.1
  *compile-file-truename*
  nil)

;;; Error cases

(deftest compile-file.error.1
  (signals-error (compile-file "nonexistent-file-to-compile.lsp") file-error)
  t)

(deftest compile-file.error.2
  (signals-error (compile-file) program-error)
  t)




