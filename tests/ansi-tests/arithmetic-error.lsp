;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Contains: Tests of ARITHMETIC-ERROR condition and associated accessors

(in-package :cl-test)

(deftest arithmethic-error.1
  (let ((a (make-condition 'arithmetic-error
			   :operation '/
			   :operands '(0 0))))
    (values
     (notnot (typep a 'arithmetic-error))
     (notnot (typep a (find-class 'arithmetic-error)))
     (multiple-value-list (arithmetic-error-operation a))
     (multiple-value-list (arithmetic-error-operands a))))
  t t (/) ((0 0)))

(deftest arithmethic-error.2
  (let ((a (make-condition 'arithmetic-error
			   :operation #'/
			   :operands '(0 0))))
    (values
     (notnot (typep a 'arithmetic-error))
     (notnot (typep a 'error))
     (notnot (typep a 'serious-condition))
     (notnot (typep a 'condition))
     (notnot (typep a (find-class 'arithmetic-error)))
     (notnot (typep (arithmetic-error-operation a) 'function))
     (funcall (arithmetic-error-operation a) 1 2)
     (multiple-value-list (arithmetic-error-operands a))))
  t t t t t t 1/2 ((0 0)))

(deftest arithmetic-error.3
  (let ((a (make-condition 'arithmetic-error
			   :operation '/
			   :operands '(0 0))))
    (macrolet
     ((%m (z) z))
     (values
      (arithmetic-error-operation (expand-in-current-env (%m a)))
      (arithmetic-error-operands (expand-in-current-env (%m a))))))
  / (0 0))

;;; Error tests

(deftest arithmetic-error-operation.error.1
  (signals-error (arithmetic-error-operation) program-error)
  t)

(deftest arithmetic-error-operation.error.2
  (signals-error (arithmetic-error-operation
		  (make-condition 'arithmetic-error :operation '/
				  :operands '(1 0))
		  nil)
		 program-error)
  t)

(deftest arithmetic-error-operands.error.1
  (signals-error (arithmetic-error-operands) program-error)
  t)

(deftest arithmetic-error-operands.error.2
  (signals-error (arithmetic-error-operands
		  (make-condition 'arithmetic-error :operation '/
				  :operands '(1 0))
		  nil)
		 program-error)
  t)
