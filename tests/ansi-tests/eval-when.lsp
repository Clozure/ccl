;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr  6 17:00:30 2003
;;;; Contains: Tests for EVAL-WHEN

;;; The following test was suggested by Sam Steingold,
;;; so I've created this file to hold it.

(in-package :cl-test)

(defvar *eval-when.1-collector*)

(deftest eval-when.1
  
  (let ((forms nil) all (ff "generated-eval-when-test-file.lisp"))
    (dolist (c '(nil (:compile-toplevel)))
      (dolist (l '(nil (:load-toplevel)))
	(dolist (x '(nil (:execute)))
	  (push `(eval-when (,@c ,@l ,@x)
		   (push '(,@c ,@l ,@x) *eval-when.1-collector*))
		forms))))
    (dolist (c '(nil (:compile-toplevel)))
      (dolist (l '(nil (:load-toplevel)))
	(dolist (x '(nil (:execute)))
	  (push `(let () (eval-when (,@c ,@l ,@x)
			   (push '(let ,@c ,@l ,@x) *eval-when.1-collector*)))
		forms))))
    (with-open-file (o ff :direction :output :if-exists :supersede)
		    (dolist (f forms)
		      (prin1 f o)
		      (terpri o)))
    (let ((*eval-when.1-collector* nil))
      (load ff)
      (push (cons "load source" *eval-when.1-collector*) all))
    (let ((*eval-when.1-collector* nil))
      (compile-file ff)
      (push (cons "compile source" *eval-when.1-collector*) all))
    (let ((*eval-when.1-collector* nil))
      (load (compile-file-pathname ff))
      (push (cons "load compiled" *eval-when.1-collector*) all))
    (delete-file ff)
    (delete-file (compile-file-pathname ff))
    #+clisp (delete-file (make-pathname :type "lib" :defaults ff))
    (nreverse all))
  
  (("load source"
    (:execute) (:load-toplevel :execute) (:compile-toplevel :execute)
    (:compile-toplevel :load-toplevel :execute)
    (let :execute) (let :load-toplevel :execute)
    (let :compile-toplevel :execute)
    (let :compile-toplevel :load-toplevel :execute))
   ("compile source"
    (:compile-toplevel) (:compile-toplevel :execute)
    (:compile-toplevel :load-toplevel)
    (:compile-toplevel :load-toplevel :execute))
   ("load compiled"
    (:load-toplevel) (:load-toplevel :execute)
    (:compile-toplevel :load-toplevel)
    (:compile-toplevel :load-toplevel :execute)
    (let :execute) (let :load-toplevel :execute)
    (let :compile-toplevel :execute)
    (let :compile-toplevel :load-toplevel :execute))))

;;; More EVAL-WHEN tests to go here

(deftest eval-when.2
  (eval-when () :bad)
  nil)

(deftest eval-when.3
  (eval-when (:execute))
  nil)

(deftest eval-when.4
  (eval-when (:execute) :good)
  :good)

(deftest eval-when.5
  (eval-when (:compile-toplevel) :bad)
  nil)

(deftest eval-when.6
  (eval-when (:load-toplevel) :bad)
  nil)

(deftest eval-when.7
  (eval-when (:compile-toplevel :execute) :good)
  :good)

(deftest eval-when.8
  (eval-when (:load-toplevel :execute) :good)
  :good)

(deftest eval-when.9
  (eval-when (:load-toplevel :compile-toplevel) :bad)
  nil)

(deftest eval-when.10
  (eval-when (:load-toplevel :compile-toplevel :execute) :good)
  :good)

(deftest eval-when.11
  (eval-when (:execute) (values 'a 'b 'c 'd))
  a b c d)

(deftest eval-when.12
  (let ((x :good))
    (values (eval-when (:load-toplevel) (setq x :bad)) x))
  nil :good)

(deftest eval-when.13
  (let ((x :good))
    (values (eval-when (:compile-toplevel) (setq x :bad)) x))
  nil :good)

(deftest eval-when.14
  (let ((x :bad))
    (values (eval-when (:execute) (setq x :good)) x))
  :good :good)

(deftest eval-when.15
  (let ((x :good))
    (values (eval-when (load) (setq x :bad)) x))
  nil :good)

(deftest eval-when.16
  (let ((x :good))
    (values (eval-when (compile) (setq x :bad)) x))
  nil :good)

(deftest eval-when.17
  (let ((x :bad))
    (values (eval-when (eval) (setq x :good)) x))
  :good :good)

;;; Macros are expanded in the appropriate environment

(deftest eval-when.18
  (macrolet ((%m (z) z))
	    (eval-when (:execute) (expand-in-current-env (%m :good))))
  :good)
