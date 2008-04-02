;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon May 19 20:07:40 2003
;;;; Contains: More tests of structures

(in-package :cl-test)

;;; I realized I had forgotten to test slot override in :include
;;; clauses in defstruct.

(defstruct struct-include-01a
  a (b 0))

(defstruct (struct-include-01b (:include struct-include-01a
					 (a 100) (b 'x)))
  (c 200) d)

(deftest struct-include.1
  (let ((obj (make-struct-include-01b)))
    (values
     (typep* obj 'struct-include-01a)
     (typep* obj 'struct-include-01b)
     (struct-include-01a-a obj)
     (struct-include-01a-b obj)
     (struct-include-01b-a obj)
     (struct-include-01b-b obj)
     (struct-include-01b-c obj)))
  t t 100 x 100 x 200)


(deftest struct-include.2
  (let ((obj (make-struct-include-01b :a 1 :b 2 :c 3 :d 4)))
    (values
     (typep* obj 'struct-include-01a)
     (typep* obj 'struct-include-01b)
     (struct-include-01a-a obj)
     (struct-include-01a-b obj)
     (struct-include-01b-a obj)
     (struct-include-01b-b obj)
     (struct-include-01b-c obj)
     (struct-include-01b-d obj)
     ))
  t t 1 2 1 2 3 4)

(defstruct struct-include-02a
  (a 0 :type number))

(defstruct (struct-include-02b (:include struct-include-02a
					 (a 10 :type integer))))

(deftest struct-include.3
  (let ((obj (make-struct-include-02b)))
    (values
     (typep* obj 'struct-include-02a)
     (typep* obj 'struct-include-02b)
     (struct-include-02a-a obj)
     (struct-include-02b-a obj)))
  t t 10 10)

(deftest struct-include.4
  (let ((obj (make-struct-include-02a)))
    (values
     (typep* obj 'struct-include-02a)
     (typep* obj 'struct-include-02b)
     (struct-include-02a-a obj)))
  t nil 0)

(deftest struct-include.5
  (let ((obj (make-struct-include-02b :a 100)))
    (values
     (typep* obj 'struct-include-02a)
     (typep* obj 'struct-include-02b)
     (struct-include-02a-a obj)
     (struct-include-02b-a obj)))
  t t 100 100)

(defstruct struct-include-03a
  (a 0 :type number))

(defstruct (struct-include-03b (:include struct-include-03a (a))))

(deftest struct-include.5a
  (let ((obj (make-struct-include-03b :a 100)))
    (values
     (typep* obj 'struct-include-03a)
     (typep* obj 'struct-include-03b)
     (struct-include-03a-a obj)
     (struct-include-03b-a obj)))
  t t 100 100)

(defstruct struct-include-04a a b)

(defstruct (struct-include-04b (:include struct-include-04a
					 (a 0 :read-only t))))

(deftest struct-include.6
  (let ((obj (make-struct-include-04b)))
    (values
     (typep* obj 'struct-include-04a)
     (typep* obj 'struct-include-04b)
     (struct-include-04a-a obj)
     (struct-include-04b-a obj)))
  t t 0 0)

(deftest struct-include.7
  (let ((obj (make-struct-include-04b :a 1 :b 2)))
    (values
     (typep* obj 'struct-include-04a)
     (typep* obj 'struct-include-04b)
     (struct-include-04a-a obj)
     (struct-include-04b-a obj)
     (struct-include-04a-b obj)
     (struct-include-04b-b obj)))
  t t 1 1 2 2)
