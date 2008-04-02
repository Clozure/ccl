;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:01:58 1998
;;;; Contains: Tests of DELETE-PACKAGE

(in-package :cl-test)
(declaim (optimize (safety 3)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delete-package

;; check return value of delete-package, and check
;; that package-name is nil on the deleted package object
(deftest delete-package.1
  (progn
    (safely-delete-package :test1)
    (let ((p (make-package :test1 :use nil)))
      (list
       (notnot (delete-package :test1))
       (notnot (packagep p))
       (package-name p))))
  (t t nil))

(deftest delete-package.2
  (progn
    (safely-delete-package :test1)
    (let ((p (make-package :test1 :use nil)))
      (list
       (notnot (delete-package :test1))
       (notnot (packagep p))
       (delete-package p))))
  (t t nil))

;; Check that deletion of different package designators works
(deftest delete-package.3
  (progn
    (safely-delete-package "X")
    (make-package "X")
    (handler-case
     (notnot (delete-package "X"))
     (error (c) c)))
  t)

(deftest delete-package.4
  (progn
    (safely-delete-package "X")
    (make-package "X")
    (handler-case
     (notnot (delete-package #\X))
     (error (c) c)))
  t)

;;; PFD 10/14/02 -- These tests are broken again.  I suspect
;;;   some sort of interaction with the test harness.

;;; PFD 01.18.03  This test is working, but suspicious.

(deftest delete-package.5
  (prog (p1 s1 p2 s2 p3)
	(declare (ignorable p1 p2 p3 s1 s2))
	(safely-delete-package "P3")
	(safely-delete-package "P2")
	(safely-delete-package "P1")
	
	(setq p1 (make-package "P1" :use ()))
	(setq s1 (intern "S1" P1))
	(export s1 "P1")
	
	(setq p2 (make-package "P2" :use '("P1")))
	(setq s2  (intern "S2" p2))
	(export s1 p2)
	(export s2 "P2")
	
	(setf p3 (make-package "P3" :use '("P2")))
	
	;; Delete the P2 package, catching the continuable
	;; error and deleting the package

	(let ((outer-restarts (compute-restarts)))
	  (handler-bind ((package-error
			  #'(lambda (c)
			      ;; (let ((r (find-restart 'continue c))) (and r (invoke-restart r)))
			      (let ((my-restarts
				     (remove 'abort
					     (set-difference (compute-restarts c)
							     outer-restarts)
					     :key #'restart-name)))
				(assert my-restarts)
				(when (find 'continue my-restarts :key #'restart-name)
				  (continue c))
				(return t)
				))))
		      (delete-package p2)))
	
	(unless (and (equal (package-name P1) "P1")
		     (null  (package-name P2))
		     (equal (package-name P3) "P3"))
	  (return 'fail1))
	
	(unless (eqt (symbol-package S1) P1)
	  (return 'fail2))
	(unless (equal (prin1-to-string S1) "P1:S1")
	  (return 'fail3))
	
	(unless (equal (multiple-value-list (find-symbol "S1" P3))
		       '(nil nil))
	  (return 'fail4))
	
	(unless (equal (multiple-value-list (find-symbol "S2" P3))
		       '(nil nil))
	  (return 'fail5))
	
	(unless (and (null (package-used-by-list P1))
		     (null (package-used-by-list P3)))
	  (return 'fail6))
	
	(unless (and (packagep P1)
		     (packagep P2)
		     (packagep P3)) (return 'fail7))
	
	(unless (and (null (package-use-list P1))
		     (null (package-use-list P3)))
	  (return 'fail8))
	
	(safely-delete-package P3)
	(safely-delete-package P1)
	(return t)
	)
  t)

;; deletion of a nonexistent package should cause a continuable
;; package-error  (same comments for delete-package.5 apply
;; here as well)

(deftest delete-package.6
  (block done
    (let ((outer-restarts (compute-restarts)))
      (safely-delete-package "TEST-20")
      (handler-bind ((package-error
		      #'(lambda (c)
			  (assert (set-difference (compute-restarts c)
						  outer-restarts))
			  (return-from done :good))))
		    (delete-package "TEST-20"))))
  :good)

;;; Specialized sequences

(defmacro def-delete-package-test (test-name name-form)
  `(deftest ,test-name
     (let ((name ,name-form))
       (safely-delete-package name)
       (let ((p (make-package name :use nil)))
	 (list
	  (notnot (delete-package :test1))
	  (notnot (packagep p))
	  (package-name p))))
     (t t nil)))

(def-delete-package-test delete-package.7
  (make-array '(5) :initial-contents "TEST1"
	      :element-type 'base-char))

(def-delete-package-test delete-package.8
  (make-array '(10) :initial-contents "TEST1XXXXX"
	      :fill-pointer 5
	      :element-type 'base-char))

(def-delete-package-test delete-package.9
  (make-array '(10) :initial-contents "TEST1XXXXX"
	      :fill-pointer 5
	      :element-type 'character))

(def-delete-package-test delete-package.10
  (make-array '(5) :initial-contents "TEST1"
	      :adjustable t
	      :element-type 'base-char))

(def-delete-package-test delete-package.11
  (make-array '(5) :initial-contents "TEST1"
	      :adjustable t
	      :element-type 'character))

(def-delete-package-test delete-package.12
  (let* ((etype 'character)
	 (name2 (make-array '(10) :initial-contents "XXXTEST1YY"
			    :element-type etype)))
    (make-array '(5) :displaced-to name2
		:displaced-index-offset 3
		:element-type etype)))

(def-delete-package-test delete-package.13
  (let* ((etype 'base-char)
	 (name2 (make-array '(10) :initial-contents "XXXTEST1YY"
			    :element-type etype)))
    (make-array '(5) :displaced-to name2
		:displaced-index-offset 3
		:element-type etype)))

;;; Error tests

(deftest delete-package.error.1
  (signals-error (delete-package) program-error)
  t)

(deftest delete-package.error.2
  (progn
    (unless (find-package "TEST-DPE2")
      (make-package "TEST-DPE2" :use nil))
    (signals-error (delete-package "TEST-DPE2" nil)
		   program-error))
  t)
