;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Feb 19 07:06:48 2004
;;;; Contains: Tests of IMPORT

(in-package :cl-test)

(compile-and-load "package-aux.lsp")

;;; Create a package name that does not collide with an existing package
;;; name or nickname
(defvar *import-package-test-name*
  (loop for i from 1
	for name = (format nil "ITP-~A" i)
	unless (find-package name) return name))

(deftest import.1
  (let ((pkg-name *import-package-test-name*))
    (safely-delete-package pkg-name)
    (let ((pkg (eval `(defpackage ,pkg-name (:use))))
	  (sym 'foo))
      (values
       (multiple-value-list (import sym pkg))
       (eqlt (find-symbol (symbol-name sym) pkg) sym)
       (eqlt (symbol-package sym) (find-package :cl-test))
       (external-symbols-in-package pkg)
       )))
  (t) t t nil)

(deftest import.2
  (let ((pkg-name *import-package-test-name*))
    (safely-delete-package pkg-name)
    (let ((pkg (eval `(defpackage ,pkg-name (:use))))
	  (sym 'foo))
      (values
       (multiple-value-list (import (list sym) pkg))
       (eqlt (find-symbol (symbol-name sym) pkg) sym)
       (eqlt (symbol-package sym) (find-package :cl-test))
       (external-symbols-in-package pkg)
       )))
  (t) t t nil)

(deftest import.3
  (let ((pkg-name *import-package-test-name*))
    (safely-delete-package pkg-name)
    (let ((*package* (eval `(defpackage ,pkg-name (:use))))
	  (sym 'foo))
      (values
       (multiple-value-list (import sym))
       (eqlt (find-symbol (symbol-name sym)) sym)
       (eqlt (symbol-package sym) (find-package :cl-test))
       (external-symbols-in-package *package*)
       )))
  (t) t t nil)

(deftest import.4
  (let ((pkg-name *import-package-test-name*))
    (safely-delete-package pkg-name)
    (let ((pkg (eval `(defpackage ,pkg-name (:use))))
	  (syms '(foo bar baz)))
      (values
       (multiple-value-list (import syms pkg))
       (loop for sym in syms always
	     (eqlt (find-symbol (symbol-name sym) pkg) sym))
       (loop for sym in syms always
	     (eqlt (symbol-package sym) (find-package :cl-test)))
       (external-symbols-in-package pkg)
       )))
  (t) t t nil)

(deftest import.5
  (let ((pkg-name *import-package-test-name*))
    (safely-delete-package pkg-name)
    (let ((pkg (eval `(defpackage ,pkg-name (:use))))
	  (sym (make-symbol (symbol-name :foo))))
      (values
       (multiple-value-list (import sym pkg))
       (eqlt (symbol-package sym) pkg)
       (eqlt (find-symbol (symbol-name sym) pkg) sym)
       (external-symbols-in-package pkg)
       )))
  (t) t t nil)

(deftest import.6
  (let ((pkg-name *import-package-test-name*))
    (safely-delete-package pkg-name)
    (let* ((pkg (eval `(defpackage ,pkg-name (:use))))
	   (sym (intern (symbol-name :foo) pkg)))
      (values
       (multiple-value-list (import sym pkg))
       (eqlt (symbol-package sym) pkg)
       (eqlt (find-symbol (symbol-name sym) pkg) sym)
       (external-symbols-in-package pkg)
       )))
  (t) t t nil)

(deftest import.7
  (let ((pkg-name *import-package-test-name*))
    (safely-delete-package pkg-name)
    (let* ((pkg (eval `(defpackage ,pkg-name (:use) (:export #:foo))))
	   (sym (intern (symbol-name :foo) pkg)))
      (values
       (multiple-value-list (import sym pkg))
       (eqlt (symbol-package sym) pkg)
       (eqlt (find-symbol (symbol-name sym) pkg) sym)
       (length (external-symbols-in-package pkg))
       (eqlt (car (external-symbols-in-package pkg)) sym)
       )))
  (t) t t 1 t)

(deftest import.8
  (let ((pkg-name *import-package-test-name*))
    (safely-delete-package pkg-name)
    (let ((pkg (eval `(defpackage ,pkg-name (:use))))
	  (sym 'foo))
      (values
       (multiple-value-list (import sym pkg-name))
       (eqlt (find-symbol (symbol-name sym) pkg) sym)
       (eqlt (symbol-package sym) (find-package :cl-test))
       (external-symbols-in-package pkg)
       )))
  (t) t t nil)

(deftest import.9
  (let ((pkg-name "Z"))
    (safely-delete-package pkg-name)
    (let ((pkg (eval `(defpackage ,pkg-name (:use))))
	  (sym 'foo))
      (values
       (multiple-value-list (import sym #\Z))
       (eqlt (find-symbol (symbol-name sym) pkg) sym)
       (eqlt (symbol-package sym) (find-package :cl-test))
       (external-symbols-in-package pkg)
       )))
  (t) t t nil)

(deftest import.10
  (let ((pkg-name *import-package-test-name*))
    (safely-delete-package pkg-name)
    (let ((pkg (eval `(defpackage ,pkg-name (:use))))
	  (sym 'foo))
      (values
       (let ((pname (make-array (length pkg-name) :element-type 'base-char
				:initial-contents pkg-name)))
	 (multiple-value-list (import sym pname)))
       (eqlt (find-symbol (symbol-name sym) pkg) sym)
       (eqlt (symbol-package sym) (find-package :cl-test))
       (external-symbols-in-package pkg)
       )))
  (t) t t nil)

(deftest import.11
  (let ((pkg-name *import-package-test-name*))
    (safely-delete-package pkg-name)
    (let ((pkg (eval `(defpackage ,pkg-name (:use))))
	  (sym 'foo))
      (values
       (let ((pname (make-array (+ 3 (length pkg-name))
				:element-type 'base-char
				:fill-pointer (length pkg-name)
				:initial-contents (concatenate 'string pkg-name "XYZ"))))
	 (multiple-value-list (import sym pname)))
       (eqlt (find-symbol (symbol-name sym) pkg) sym)
       (eqlt (symbol-package sym) (find-package :cl-test))
       (external-symbols-in-package pkg)
       )))
  (t) t t nil)

(deftest import.12
  (let ((pkg-name *import-package-test-name*))
    (safely-delete-package pkg-name)
    (let ((pkg (eval `(defpackage ,pkg-name (:use))))
	  (sym 'foo))
      (values
       (let* ((pname0 (make-array (+ 4 (length pkg-name))
				:element-type 'base-char
				:fill-pointer (length pkg-name)
				:initial-contents (concatenate 'string "  " pkg-name "XY")))
	      (pname (make-array (length pkg-name) :element-type 'base-char
				 :displaced-to pname0
				 :displaced-index-offset 2)))
	 (multiple-value-list (import sym pname)))
       (eqlt (find-symbol (symbol-name sym) pkg) sym)
       (eqlt (symbol-package sym) (find-package :cl-test))
       (external-symbols-in-package pkg)
       )))
  (t) t t nil)



;;; Error tests

(deftest import.error.1
  (signals-error (import) program-error)
  t)

(deftest import.error.2
  (signals-error (import 'nil (find-package :cl-test) nil) program-error)
  t)

(deftest import.error.3
  (signals-error
   (let ((pkg-name *import-package-test-name*))
     (safely-delete-package pkg-name)
     (let* ((pkg (eval `(defpackage ,pkg-name (:use))))
	    (sym 'foo)
	    (name (symbol-name sym)))
       (intern name pkg)
       (import sym pkg)))
   package-error)
  t)

(deftest import.error.4
  (let ((pkg-name *import-package-test-name*))
    (safely-delete-package pkg-name)
    (let* ((pkg (eval `(defpackage ,pkg-name (:use))))
	   (sym 'foo)
	   (name (symbol-name sym))
	   (isym (intern name pkg))
	   (outer-restarts (compute-restarts)))
      (block done
	(and
	 (handler-bind
	  ((package-error
	    #'(lambda (c)
		;; There should be at least one restart
		;; associated with this condition that was
		;; not a preexisting restart
		(let ((my-restarts
		       (remove 'abort
			       (set-difference (compute-restarts c)
					       outer-restarts)
			       :key #'restart-name)))
		  (assert my-restarts)
		; (unintern isym pkg)
		; (when (find 'continue my-restarts :key #'restart-name) (continue c))
		(return-from done :good)))))
	  (import sym pkg))
	 (eqlt (find-symbol name pkg) sym)
	 (eqlt (symbol-package sym) (find-package "CL-TEST"))
	 :good))))
  :good)


(deftest import.error.5
  (let ((pkg-name *import-package-test-name*))
    (safely-delete-package pkg-name)
    (let* ((pkg (eval `(defpackage ,pkg-name (:use))))
	   (sym 'foo)
	   (name (symbol-name sym))
	   (isym (shadow name pkg))  ;; shadow instead of intern
	   (outer-restarts (compute-restarts)))
      (block done
	(and
	 (handler-bind
	  ((package-error
	    #'(lambda (c)
		;; There should be at least one restart
		;; associated with this condition that was
		;; not a preexisting restart
		(let ((my-restarts
		       (remove 'abort
			       (set-difference (compute-restarts c)
					       outer-restarts)
			       :key #'restart-name)))
		  (assert my-restarts)
		  ; (unintern isym pkg)
		  ; (when (find 'continue my-restarts :key #'restart-name) (continue c))
		  (return-from done :good)))))
	  (import sym pkg))
	 (eqlt (find-symbol name pkg) sym)
	 (eqlt (symbol-package sym) (find-package "CL-TEST"))
	 :good))))
  :good)
