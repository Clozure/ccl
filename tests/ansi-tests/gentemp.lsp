;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jun 22 09:32:09 2003
;;;; Contains: Tests of GENTEMP

(in-package :cl-test)

(deftest gentemp.1
  (let* ((package-name "GENTEMP-TEST-PACKAGE"))
    (unwind-protect
	(let* ((pkg (make-package package-name :use nil))
	       (gcounter *gensym-counter*)
	       (sym (let ((*package* pkg)) (gentemp)))
	       (sym-name (symbol-name sym)))
	  (values
	   (=t gcounter *gensym-counter*) ;; wasn't changed
	   (eqlt (aref sym-name 0) #\T)
	   (notnot (every #'digit-char-p (subseq sym-name 1)))
	   (eql (symbol-package sym) pkg)
	   ;; Not external
	   (do-external-symbols (s pkg t) (when (eql s sym) (return nil)))
	   ))
      (delete-package package-name)))
  t t t t t)

(deftest gentemp.2
  (let* ((package-name "GENTEMP-TEST-PACKAGE"))
    (unwind-protect
	(let* ((pkg (make-package package-name :use nil))
	       (gcounter *gensym-counter*)
	       (sym (let ((*package* pkg)) (gentemp "X")))
	       (sym-name (symbol-name sym)))
	  (values
	   (=t gcounter *gensym-counter*) ;; wasn't changed
	   (eqlt (aref sym-name 0) #\X)
	   (notnot (every #'digit-char-p (subseq sym-name 1)))
	   (eql (symbol-package sym) pkg)
	   ;; Not external
	   (do-external-symbols (s pkg t) (when (eql s sym) (return nil)))
	   ))
      (delete-package package-name)))
  t t t t t)

(deftest gentemp.3
  (let* ((package-name "GENTEMP-TEST-PACKAGE"))
    (unwind-protect
	(let* ((pkg (make-package package-name :use nil))
	       (gcounter *gensym-counter*)
	       (sym (gentemp "X" package-name))
	       (sym-name (symbol-name sym)))
	  (values
	   (=t gcounter *gensym-counter*) ;; wasn't changed
	   (eqlt (aref sym-name 0) #\X)
	   (notnot (every #'digit-char-p (subseq sym-name 1)))
	   (eql (symbol-package sym) pkg)
	   ;; Not external
	   (do-external-symbols (s pkg t) (when (eql s sym) (return nil)))
	   ))
      (delete-package package-name)))
  t t t t t)

(deftest gentemp.4
  (let* ((package-name "GENTEMP-TEST-PACKAGE"))
    (unwind-protect
	(let* ((pkg (make-package package-name :use nil))
	       (gcounter *gensym-counter*)
	       (sym (gentemp "" (make-symbol package-name)))
	       (sym-name (symbol-name sym)))
	  (values
	   (=t gcounter *gensym-counter*) ;; wasn't changed
	   (notnot (every #'digit-char-p sym-name))
	   (eql (symbol-package sym) pkg)
	   ;; Not external
	   (do-external-symbols (s pkg t) (when (eql s sym) (return nil)))
	   ))
      (delete-package package-name)))
  t t t t)

(deftest gentemp.5
  (let* ((package-name "Z"))
    (safely-delete-package package-name)
    (unwind-protect
	(let* ((pkg (make-package package-name :use nil))
	       (gcounter *gensym-counter*)
	       (sym (gentemp "Y" #\Z))
	       (sym-name (symbol-name sym)))
	  (values
	   (=t gcounter *gensym-counter*) ;; wasn't changed
	   (eqlt (aref sym-name 0) #\Y)
	   (notnot (every #'digit-char-p (subseq sym-name 1)))
	   (eql (symbol-package sym) pkg)
	   ;; Not external
	   (do-external-symbols (s pkg t) (when (eql s sym) (return nil)))
	   ))
      (delete-package package-name)))
  t t t t t)

(deftest gentemp.6
  (let* ((package-name "GENTEMP-TEST-PACKAGE"))
    (unwind-protect
	(let* ((*package* (make-package package-name :use nil))
	       (syms (loop repeat 100 collect (gentemp))))
	  (=t (length syms) (length (remove-duplicates syms))))
      (delete-package package-name)))
  t)

;;; Error tests

(deftest gentemp.error.1
  (loop for x in *mini-universe*
	unless (or (stringp x)
		   (eql (eval `(signals-type-error x ',x (gentemp x))) t))
	collect x)
  nil)

(deftest gentemp.error.2
  (loop for x in *mini-universe*
	unless (or (typep x 'package)
		   (string-designator-p x)
		   (eql (eval `(signals-type-error x ',x (gentemp "T" x))) t))
	collect x)
  nil)

(deftest gentemp.error.3
  (signals-error (gentemp "" *package* nil) program-error)
  t)
