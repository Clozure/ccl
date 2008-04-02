;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Mar 21 22:28:53 2003
;;;; Contains: Tests for RESTART-BIND

(in-package :cl-test)

(deftest restart-bind.1
  (restart-bind () nil)
  nil)

(deftest restart-bind.2
  (restart-bind () (values)))
  
(deftest restart-bind.3
  (restart-bind () (values 'a 'b 'c 'd 'e 'f))
  a b c d e f)

(deftest restart-bind.4
  (block nil
    (restart-bind () (return 'good) 'bad))
  good)

(deftest restart-bind.5
  (block done
    (tagbody
     (restart-bind () (go 10) (return-from done 'bad))
     10
     (return-from done 'good)))
  good)

(deftest restart-bind.6
  (restart-bind ())
  nil)

(deftest restart-bind.7
  (block done
    (restart-bind ((foo #'(lambda () (return-from done 'good))))
		  (invoke-restart 'foo)
		  'bad))
  good)

(deftest restart-bind.8
  (block done
    (restart-bind ((foo #'(lambda () (return-from done 'good))))
		  (let ((restart (find-restart 'foo)))
		    (and (typep restart 'restart)
			 (invoke-restart restart)))
		  'bad))
  good)

(deftest restart-bind.9
  (restart-bind ((foo #'(lambda (a b c) (list c a b))))
		(invoke-restart 'foo 1 2 3))
  (3 1 2))

(deftest restart-bind.10
  (flet ((%f () (invoke-restart 'foo 'x 'y 'z)))
    (restart-bind ((foo #'(lambda (a b c) (list c a b))))
		  (%f)))
  (z x y))

(deftest restart-bind.11
  (restart-bind
   ((foo #'(lambda () 'bad)))
   (restart-bind
    ((foo #'(lambda () 'good)))
    (invoke-restart 'foo)))
  good)

(deftest restart-bind.12
  (let ((*x* 'bad))
    (declare (special *x*))
    (restart-bind
     ((foo #'(lambda () (declare (special *x*)) *x*)))
     (let ((*x* 'good))
       (declare (special *x*))
       (invoke-restart 'foo))))
  good)

(deftest restart-bind.13
  (restart-bind
   ((foo #'(lambda () 'bad)))
   (flet ((%f () (invoke-restart 'foo)))
     (restart-bind
      ((foo #'(lambda () 'good)))
      (%f))))
  good)

(deftest restart-bind.14
  (let ((x 10) (y nil))
    (restart-bind
     ((foo #'(lambda ()
	       (when (> x 0)
		 (push 'a y)
		 (decf x)
		 (invoke-restart 'foo))
	       y)))
     (invoke-restart 'foo)))
  (a a a a a a a a a a))

(deftest restart-bind.15
  (block done
    (let ((i 0))
      (restart-bind ((foo (progn (incf i)
				 #'(lambda () (return-from done i)))))
		    (invoke-restart 'foo)
		    'bad)))
  1)

(deftest restart-bind.16
  (let ((i 0))
    (values
     (with-output-to-string
       (s)
       (restart-bind
	((foo #'(lambda () nil)
	      :report-function (progn (incf i)
				      #'(lambda (s) (format s "A report")))))
	(let ((*print-escape* nil))
	  (format s "~A" (find-restart 'foo)))))
     i))
  "A report"
  1)

(deftest restart-bind.17
  (restart-bind
   ((foo #'(lambda () 'good))
    (foo #'(lambda () 'bad)))
   (invoke-restart 'foo))
  good)

(deftest restart-bind.18
  (restart-bind
   ((foo #'(lambda () 'good))
    (bar #'(lambda () 'bad)))
   (invoke-restart 'foo))
  good)

(deftest restart-bind.19
  (restart-bind
   ((foo #'(lambda () 'bad))
    (bar #'(lambda () 'good)))
   (invoke-restart 'bar))
  good)

;;; Using the :test-function to associate a restart with a condition

;;; This test is disabled until I figure out how to fix
;;; it.  See sbcl-devel mailing list, Oct 2005
#|
(deftest restart-bind.20
  (let ((c (make-condition 'error)))
    (restart-bind
     ((foo #'(lambda () 'bad)
	   :test-function #'(lambda (c1) (not (eq c c1))))
      (foo #'(lambda () 'good)
	   :test-function #'(lambda (c2) (or (null c2)
					     (eq c c2)))))
     (invoke-restart (find-restart 'foo c))))
  good)
|#

(deftest restart-bind.21
  (let ((c (make-condition 'error)))
    (restart-bind
     ((foo #'(lambda () 'bad)
	   :test-function #'(lambda (c1) nil))
      (foo #'(lambda () 'good)
	   :test-function #'(lambda (c2) t)))
     (invoke-restart (find-restart 'foo c))))
  good)

(deftest restart-bind.22
  (let ((c (make-condition 'error))
	(i 0))
    (values
     (restart-bind
      ((foo #'(lambda () 'good)
	    :test-function (progn (incf i) #'(lambda (c2) t))))
      (invoke-restart (find-restart 'foo c)))
     i))
  good
  1)

;;; Error tests

(deftest restart-bind.error.1
  (signals-error
   (restart-bind
    ((foo #'(lambda () t)))
    (invoke-restart 'foo 'a))
   program-error)
  t)

(deftest restart-bind.error.2
  (signals-error
   (restart-bind
    ((foo #'(lambda (x) x)))
    (invoke-restart 'foo))
   program-error)
  t)

(deftest restart-bind.error.3
  (signals-error
   (restart-bind
    ((foo #'identity))
    (invoke-restart 'foo))
   program-error)
  t)

(deftest restart-bind.23
  (restart-bind
   ((foo #'(lambda () 'good)))
   (invoke-restart-interactively 'foo))
  good)

(deftest restart-bind.24
  (let ((i 0))
    (values
     (restart-bind
      ((foo
	#'(lambda (x y z) (list z y x))
	:interactive-function (progn (incf i)
				     #'(lambda () (list 'a 'b 'c)))))
      (invoke-restart-interactively 'foo))
     i))
  (c b a)
  1)

