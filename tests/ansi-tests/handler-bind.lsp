;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Feb 28 22:07:25 2003
;;;; Contains: Tests of HANDLER-BIND

(in-package :cl-test)

(deftest handler-bind.1
  (handler-bind ())
  nil)

(deftest handler-bind.2
  (handler-bind () (values)))

(deftest handler-bind.3
  (handler-bind () (values 1 2 3))
  1 2 3)

(deftest handler-bind.4
  (let ((x 0))
    (values
     (handler-bind () (incf x) (+ x 10))
     x))
  11 1)

(deftest handler-bind.5
  (block foo
    (handler-bind ((error #'(lambda (c) (return-from foo 'good))))
		  (error "an error")))
  good)

(deftest handler-bind.6
  (block foo
    (handler-bind
     ((error #'(lambda (c) (return-from foo 'good))))
     (handler-bind ((error #'(lambda (c) (error c)))
		    (error #'(lambda (c) (return-from foo 'bad))))
		   (error "an error"))))
  good)

(defun handler-bind.7-handler-fn (c)
  (declare (ignore c))
  (throw 'foo 'good))

(deftest handler-bind.7
  (catch 'foo
    (handler-bind ((simple-error #'handler-bind.7-handler-fn))
		  (error "simple error")))
  good)

(deftest handler-bind.8
  (catch 'foo
    (handler-bind ((simple-error 'handler-bind.7-handler-fn))
		  (error "simple error")))
  good)

(deftest handler-bind.9
  (catch 'foo
    (handler-bind ((simple-error #.(symbol-function
				    'handler-bind.7-handler-fn)))
		  (error "simple error")))
  good)

(deftest handler-bind.10
  (block done
    (flet ((%foo () (signal "A simple condition"))
	   (%succeed (c) (declare (ignore c)) (return-from done 'good))
	   (%fail (c) (declare (ignore c)) (return-from done 'bad)))
      (handler-bind
       ((error #'%fail)
	(simple-condition #'%succeed))
       (%foo))))
  good)

(deftest handler-bind.11
  (block done
    (handler-bind
     ((error #'(lambda (c) c))
      (error #'(lambda (c) (declare (ignore c)) (return-from done 'good))))
     (error "an error")))
  good)

(deftest handler-bind.12
  (block done
    (handler-bind
     ((error #'(lambda (c) (declare (ignore c)) (return-from done 'good))))
     (handler-bind
      ((error #'(lambda (c) c)))
      (error "an error"))))
  good)

(deftest handler-bind.13
  (handler-bind
   ((error #'(lambda (c) (declare (ignore c))
	       (throw 'done 'good))))
   (catch 'done
     (error "an error")))
  good)

(deftest handler-bind.14
  (catch 'done
    (handler-bind
     ((symbol #'identity)  ;; can never succeed
      (error #'(lambda (c) (declare (ignore c))
		 (throw 'done 'good))))
     (error "an error")))
  good)

(deftest handler-bind.15
  (catch 'done
    (handler-bind
     ((nil #'(lambda (c) (declare (ignore c))
	       (throw 'done 'bad)))
      (error #'(lambda (c) (declare (ignore c))
		 (throw 'done 'good))))
     (error "an error")))
  good)

(deftest handler-bind.16
  (catch 'done
    (handler-bind
     (((not error) #'identity)
      (error
       #'(lambda (c) (declare (ignore c))
	   (throw 'done 'good))))
     (error "an error")))
  good)

(deftest handler-bind.17
  (catch 'done
    (handler-bind
     ((#.(find-class 'error)
	 #'(lambda (c) (declare (ignore c))
	     (throw 'done 'good))))
     (error "an error")))
  good)

;;; More handler-bind tests elsewhere



  
		

		
  
