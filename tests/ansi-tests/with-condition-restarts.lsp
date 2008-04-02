;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar 23 04:06:06 2003
;;;; Contains: Tests of WITH-CONDITION-RESTARTS

(in-package :cl-test)

(deftest with-condition-restarts.1
  (let (a b c (i 0))
    (values
     (with-condition-restarts
      (progn (setf a (incf i)) (make-condition 'error))
      (progn (setf b (incf i)) nil)
      (setf c (incf i)))
     a b c i))
  3 1 2 3 3)

(deftest with-condition-restarts.2
  (with-condition-restarts
   (make-condition 'error)
   nil
   (values)))

(deftest with-condition-restarts.3
  (with-condition-restarts
   (make-condition 'error)
   nil
   (values 'a 'b 'c 'd 'e 'f))
  a b c d e f)

(deftest with-condition-restarts.4
  (block done
    (tagbody
     (with-condition-restarts
      (make-condition 'error)
      nil
      (go 10)
      10
      (return-from done 'bad))
     10
     (return-from done 'good)))
  good)

(deftest with-condition-restarts.5
  (let ((c (make-condition 'error)))
    (restart-case
     (with-condition-restarts
      c
      (list (find-restart 'foo))
      'good)
     (foo () 'bad)))
  good)

(deftest with-condition-restarts.6
  (let ((c (make-condition 'error))
	(c2 (make-condition 'error)))
    (handler-bind
     ((error #'(lambda (c) (invoke-restart (find-restart 'foo c2)))))
     (restart-case
      (with-condition-restarts
       c
       (list (find-restart 'foo))
       (signal c2))
      (foo () 'bad)
      (foo () 'good))))
  good)

(deftest with-condition-restarts.7
  (let ((c (make-condition 'error))
	(c2 (make-condition 'error)))
    (handler-bind
     ((error #'(lambda (c) (invoke-restart 'foo))))
     (restart-case
      (with-condition-restarts
       c
       (list (find-restart 'foo))
       (signal c2))
      (foo () 'good)
      (foo () 'bad))))
  good)

;;; test that the association of a restart with a condition
;;; has dynamic extent

(deftest with-condition-restarts.8
  (let ((c (make-condition 'error))
	(c2 (make-condition 'error)))
    (restart-case
     (progn
       (with-condition-restarts
	c
	(list (find-restart 'foo)))
       (invoke-restart (find-restart 'foo c2)))
     (foo () 'good)
     (foo () 'bad)))
  good)
