;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 31 08:04:45 2003
;;;; Contains: Tests of NEXT-METHOD-P

(in-package :cl-test)

(defgeneric nmp-gf-01 (x)
  (:method ((x integer)) (notnot-mv (next-method-p)))
  (:method ((x number)) 'foo)
  (:method ((x symbol)) (next-method-p)))

(deftest next-method-p.1
  (nmp-gf-01 10)
  t)

(deftest next-method-p.2
  (nmp-gf-01 1.2)
  foo)

(deftest next-method-p.3
  (nmp-gf-01 'a)
  nil)

(defgeneric nmp-gf-02 (x y)
  (:method ((x integer) (y symbol)) (notnot-mv (next-method-p)))
  (:method ((x number) (y (eql nil))) 'foo))

(deftest next-method-p.4
  (nmp-gf-02 10 nil)
  t)

(deftest next-method-p.5
  (nmp-gf-02 10 'a)
  nil)

(defgeneric nmp-gf-03 (x y)
  (:method ((x integer) (y symbol)) #'next-method-p)
  (:method ((x t) (y (eql nil))) (constantly 1)))

(deftest next-method-p.6
  (notnot-mv (funcall (the function (nmp-gf-03 10 nil))))
  t)
  
(deftest next-method-p.7
  (funcall (nmp-gf-03 10 'a))
  nil)

(defgeneric nmp-gf-04 (x y))
(defmethod nmp-gf-04 ((x integer) (y symbol)) #'next-method-p)
(defmethod nmp-gf-04 ((x t) (y (eql nil))) (constantly 2))

(deftest next-method-p.8
  (notnot-mv (funcall (the function (nmp-gf-04 10 nil))))
  t)
  
(deftest next-method-p.9
  (funcall (nmp-gf-04 10 'a))
  nil)

;; With AROUND methods

(defgeneric nmp-gf-05 (x))
(defmethod nmp-gf-05 :around ((x number)) (notnot-mv (next-method-p)))
(defmethod nmp-gf-05 ((x integer)) 'foo)

(deftest next-method-p.10
  (nmp-gf-05 10)
  t)

;; Need to also test next-method-p in builtin method combinations

;;; Error tests

(deftest next-method-p.error.1
  (signals-error
   (progn
     (eval '(defmethod nmp-gf-06 ((x t)) (next-method-p nil)))
     (nmp-gf-06 nil))
   program-error)
  t)
