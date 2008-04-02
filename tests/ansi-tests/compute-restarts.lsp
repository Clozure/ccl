;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 22 23:48:53 2003
;;;; Contains: Tests of COMPUTE-RESTARTS

(in-package :cl-test)

(deftest compute-restarts.1
  (loop for r in (compute-restarts)
	always (typep r 'restart))
  t)

(deftest compute-restarts.2
  (loop for r in (compute-restarts)
	always (typep r (find-class 'restart)))
  t)

(deftest compute-restarts.3
  (restart-case
   (let ((r (find-restart 'foo)))
     (eqt r (find 'foo (compute-restarts) :key #'restart-name)))
   (foo () nil))
  t)

(deftest compute-restarts.4
  (loop for r1 in (compute-restarts)
	for r2 in (compute-restarts)
	always (eq r1 r2))
  t)

(deftest compute-restarts.5
  (restart-case
   (loop for r1 in (compute-restarts)
	 for r2 in (compute-restarts)
	 always (eq r1 r2))
   (foo () t)
   (bar () t)
   (foo () nil))
  t)

(deftest compute-restarts.6
  (restart-case
   (let* ((restarts (compute-restarts))
	  (p (position 'foo restarts :key #'restart-name))
	  (r (find 'foo restarts :start (1+ p) :key #'restart-name)))
     (invoke-restart r))
   (foo () 'bad)
   (foo () 'good)
   (foo () 'bad))
  good)

(deftest compute-restarts.7
  (handler-bind
   ((error #'(lambda (c)
	       (let* ((restarts (compute-restarts c))
		      (r (remove 'foo restarts
				 :test-not #'eq
				 :key #'restart-name)))
		 (invoke-restart (second r))))))
   (restart-case
    (error "an error")
    (foo () 'bad)
    (foo () 'good)
    (foo () 'bad)))
  good)

(deftest compute-restarts.8
  (handler-bind
   ((error #'(lambda (c)
	       (declare (ignore c))
	       (let* ((restarts (compute-restarts))
		      (r (remove 'foo restarts
				 :test-not #'eq
				 :key #'restart-name)))
		 (invoke-restart (second r))))))
   (restart-case
    (error "an error")
    (foo () 'bad)
    (foo () 'good)
    (foo () 'bad)))
  good)

(deftest compute-restarts.9
  (let ((c2 (make-condition 'error)))
    (block done
      (handler-bind
       ((error #'(lambda (c)
		   (declare (ignore c))
		   (let* ((restarts (compute-restarts c2))
			  (r (remove 'foo restarts
				     :test-not #'eq
				     :key #'restart-name)))
		     ;; (write restarts)
		     (return-from done
		       (values r
			       (mapcar #'restart-name r)))))))
       (restart-case
	(error "an error")
	(foo () 'bad)
	(foo () 'also-bad)))))
  nil nil)

;;; This test is disabled until I figure out how to fix
;;; it.  See sbcl-devel mailing list, Oct 2005
#|
(deftest compute-restarts.10
  (let ((c2 (make-condition 'error)))
    (block done
      (handler-bind
       ((error #'(lambda (c)
		   (declare (ignore c))
		   (let* ((restarts (compute-restarts c2))
			  (r (remove 'foo restarts
				     :test-not #'eq
				     :key #'restart-name)))
		     ;; (write restarts)
		     (return-from done
		       (values r
			       (mapcar #'restart-name r)))))))
       (restart-case
	(progn (error "an error"))
	(foo () :test (lambda (c) (or (null c) (not (eq c c2))))
	     'bad)
	(foo () :test (lambda (c) (or (null c) (not (eq c c2))))
	     'also-bad)))))
  nil nil)
|#




