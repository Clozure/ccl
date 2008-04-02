;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Dec 12 19:53:11 2004
;;;; Contains: Tests of TRACE, UNTRACE

(in-package :cl-test)

(defun function-to-trace (x) (car x))
(defun another-function-to-trace (x) (cdr x))
(defun (setf function-to-trace) (val arg) (setf (car arg) val))

(declaim (notinline function-to-trace
		    another-function-to-trace
		    (setf function-to-trace)))

(deftest trace.1
  (progn
    (untrace)  ;; ensure it's not traced
    (with-output-to-string
      (*trace-output*)
      (assert (eql (function-to-trace '(a)) 'a))))
  "")

(deftest trace.2
  (progn
    (trace function-to-trace)
    (equal "" (with-output-to-string
		(*trace-output*)
		(assert (eql (function-to-trace '(b)) 'b)))))
  nil)

(deftest trace.3
  (progn
    (untrace)
    (trace function-to-trace)
    (prog1 (trace)
      (untrace)
      (assert (null (trace)))))
  (function-to-trace))


(deftest trace.4
  (progn
    (untrace)
    (trace function-to-trace)
    (handler-bind ((warning #'muffle-warning))
		  (trace function-to-trace))
    (prog1 (trace)
      (untrace)
      (assert (null (trace)))))
  (function-to-trace))

(deftest trace.5
  (progn
    (untrace)
    (trace (setf function-to-trace))
    (prog1 (trace)
      (untrace)
      (assert (null (trace)))))
  ((setf function-to-trace)))

(deftest trace.6
  (progn
    (untrace)
    (trace (setf function-to-trace))
    (handler-bind ((warning #'muffle-warning))
		  (trace (setf function-to-trace)))
    (prog1 (trace)
      (untrace)
      (assert (null (trace)))))
  ((setf function-to-trace)))

(deftest trace.7
  (progn
    (untrace)
    (with-output-to-string
      (*trace-output*)
      (let ((x (list nil)))
	(assert (eql (setf (function-to-trace x) 'a) 'a))
	(assert (equal x '(a))))))
  "")

(deftest trace.8
  (progn
    (untrace)
    (trace (setf function-to-trace))
    (equal ""
	   (with-output-to-string
	     (*trace-output*)
	     (let ((x (list nil)))
	       (assert (eql (setf (function-to-trace x) 'a) 'a))
	       (assert (equal x '(a)))))))
  nil)

(deftest trace.9
  (progn
    (untrace)
    (trace function-to-trace another-function-to-trace)
    (assert (not (equal "" (with-output-to-string
			     (*trace-output*)
			     (assert (eql (function-to-trace '(b)) 'b))))))
    (assert (not (equal "" (with-output-to-string
			     (*trace-output*)
			     (assert (eql (another-function-to-trace '(c . d))
					  'd))))))
    (prog1
	(sort (copy-list (trace))
	      #'(lambda (k1 k2) (string< (symbol-name k1)
					 (symbol-name k2))))
      (untrace)))
  (another-function-to-trace function-to-trace))

(deftest trace.10
  (progn
    (untrace)
    (assert (null (trace)))
    (trace function-to-trace)
    (untrace function-to-trace)
    (assert (null (trace)))
    (handler-bind ((warning #'muffle-warning)) (untrace function-to-trace))
    (assert (null (trace)))
    nil)
  nil)

(deftest trace.11
  (progn
    (untrace)
    (trace function-to-trace another-function-to-trace)
    (untrace function-to-trace another-function-to-trace)
    (trace))
  nil)

;;; Tracing a generic function

(declaim (notinline generic-function-to-trace))

(deftest trace.12
  (progn
    (untrace)
    (eval '(defgeneric generic-function-to-trace (x y)))
    (trace generic-function-to-trace)
    (prog1 (trace) (untrace)))
  (generic-function-to-trace))

(deftest trace.13
  (progn
    (untrace)
    (eval '(defgeneric generic-function-to-trace (x y)))
    (trace generic-function-to-trace)
    (eval '(defmethod generic-function-to-trace ((x t)(y t)) nil))
    (prog1 (trace) (untrace)))
  (generic-function-to-trace))

(deftest trace.14
  (progn
    (untrace)
    (eval '(defgeneric generic-function-to-trace (x y)))
    (trace generic-function-to-trace)
    (eval '(defmethod generic-function-to-trace ((x t)(y t)) nil))
    (assert (not (equal (with-output-to-string
			  (*trace-output*)
			  (assert (null (generic-function-to-trace 'a 'b))))
			"")))
    (prog1
	(trace)
      (untrace generic-function-to-trace)
      (assert (null (trace)))))
  (generic-function-to-trace))

(declaim (notinline generic-function-to-trace2))

(deftest trace.15
  (progn
    (untrace)
    (let* ((gf (eval '(defgeneric generic-function-to-trace2 (x y))))
	   (m (eval '(defmethod generic-function-to-trace2
		       ((x integer)(y integer))
		       :foo))))
      (eval '(defmethod generic-function-to-trace2
	       ((x symbol)(y symbol)) :bar))
      (assert (eql (generic-function-to-trace2 1 2) :foo))
      (assert (eql (generic-function-to-trace2 'a 'b) :bar))
      (trace generic-function-to-trace2)
      (assert (equal (trace) '(generic-function-to-trace2)))
      (remove-method gf m)
      (prog1 (trace) (untrace))))
  (generic-function-to-trace2))
