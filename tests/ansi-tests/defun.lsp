;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 16 23:40:32 2003
;;;; Contains: Tests of DEFUN

(in-package :cl-test)


;;; Tests for implicit blocks

(defun defun-test-fun-1 ()
  (return-from defun-test-fun-1 'good))

(deftest defun.1
  (defun-test-fun-1)
  good)

(defun defun-test-fun-2 ()
  (return-from defun-test-fun-2 (values)))

(deftest defun.2
  (defun-test-fun-2))

(defun defun-test-fun-3 ()
  (return-from defun-test-fun-3 (values 'a 'b 'c 'd 'e 'f)))

(deftest defun.3
  (defun-test-fun-3)
  a b c d e f)

(defun defun-test-fun-4 (x)
  (car x))

(deftest defun.4
  (let ((x (list 'a 'b)))
    (values
     (setf (defun-test-fun-4 x) 'c)
     x))
  c
  (c b))

(report-and-ignore-errors
 (defun (setf defun-test-fun-4) (newval x)
   (return-from defun-test-fun-4 (setf (car x) newval))))

(deftest defun.5
  (let ((x 1))
    (declare (special x))
    (let ((x 2))
      (defun defun-test-fun-5 (&aux (y x))
	(declare (special x))
	(values y x))
      (defun-test-fun-5)))
  2 1)

(deftest defun.6
  (let ((x 1))
    (declare (special x))
    (let ((x 2))
      (defun defun-test-fun-5 (&optional (y x))
	(declare (special x))
	(values y x))
      (defun-test-fun-5)))
  2 1)

(deftest defun.7
  (let ((x 1))
    (declare (special x))
    (let ((x 2))
      (defun defun-test-fun-5 (&key (y x))
	(declare (special x))
	(values y x))
      (defun-test-fun-5)))
  2 1)

;; Documentation

(deftest defun.8
  (let* ((sym (gensym))
	 (doc "DEFUN.8")
	 (form `(defun ,sym () ,doc nil)))
    (or (documentation sym 'function) doc))
  "DEFUN.8")

;;; Error tests

(deftest defun.error.1
  (signals-error (funcall (macro-function 'defun))
		 program-error)
  t)

(deftest defun.error.2
  (signals-error (funcall (macro-function 'defun)
			   '(defun nonexistent-function ()))
		 program-error)
  t)

(deftest defun.error.3
  (signals-error (funcall (macro-function 'defun)
			   '(defun nonexistent-function ())
			   nil nil)
		 program-error)
  t)

;;; More comprehensive error handling tests of calls to
;;; user-defined functions

(deftest defun.error.4
  (let* ((name (gensym)))
    (loop for i below (min 100 lambda-parameters-limit)
	  for params = nil then (cons (gensym) params)
	  for args = nil then (cons nil args)
	  for expected = '(1 2 3)
	  for fn = (eval `(prog2 (proclaim '(optimize (safety 0)))
				 (defun ,name ,params (values ,@expected))
				 (proclaim '(optimize safety))))
	  when
	  (cond
	   ((not (equal (multiple-value-list (apply fn args)) expected))
	    (list i :fail1))
	   ((not (equal (multiple-value-list
			 (apply (symbol-function fn) args))
			expected))
	    (list i :fail2))
	   ((not (equal (multiple-value-list (eval `(,name ,@args)))
			expected))
	    (list i :fail3))
	   ;; Error cases
	   ((and (> i 0)
		 (let ((val (eval `(signals-error (,name ,@(cdr args)) program-error))))
		   (and (not (eq val t)) :fail4))))
	   ((and (< i (1- call-arguments-limit))
		 (let ((val (eval `(signals-error (,name nil ,@args) program-error))))
		   (and (not (eq val t)) :fail5)))))
	  collect it))
  nil)

