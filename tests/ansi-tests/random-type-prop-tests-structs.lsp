;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Contains: Random type prop tests: structures

(in-package :cl-test)

(defstruct rtpt-1 a b)

(defmethod make-random-element-of-type ((type (eql 'rtpt-1)))
  (make-rtpt-1 :a (make-random-element-of-type t)
	       :b (make-random-element-of-type t)))

(defmethod replicate ((obj rtpt-1))
  (or (gethash obj *replicate-table*)
      (let ((x (make-rtpt-1)))
	(setf (gethash obj *replicate-table*) x)
	(setf (rtpt-1-a x) (replicate (rtpt-1-a obj)))
	(setf (rtpt-1-b x) (replicate (rtpt-1-b obj)))
	x)))

(defmethods make-random-type-containing*
  (1 ((val rtpt-1)) 'rtpt-1))

(def-type-prop-test structure-ref.1 'rtpt-1-a '(rtpt-1) 1)

(def-type-prop-test copy-structure.1 'copy-structure '(rtpt-1) 1
  :test #'equalp)


(defstruct rtpt-2 a)
(defstruct (rtpt-2.1 (:include rtpt-2)) c d)
(defstruct (rtpt-2.2 (:include rtpt-2)) d e)

(defmethod make-random-element-of-type ((type (eql 'rtpt-2)))
  (rcase
   (1 (make-rtpt-2 :a (make-random-element-of-type t)))
   (1 (make-random-element-of-type 'rtpt-2.1))
   (1 (make-random-element-of-type 'rtpt-2.2))))

(defmethod make-random-element-of-type ((type (eql 'rtpt-2.1)))
  (make-rtpt-2.1 :a (make-random-element-of-type t)
		 :c (make-random-element-of-type t)
		 :d (make-random-element-of-type t)))

(defmethod make-random-element-of-type ((type (eql 'rtpt-2.2)))
  (make-rtpt-2.2 :a (make-random-element-of-type t)
		 :d (make-random-element-of-type t)
		 :e (make-random-element-of-type t)))

(defmethod replicate ((obj rtpt-2))
  (replicate-with (obj x (make-rtpt-2))
		  (setf (rtpt-2-a x) (replicate (rtpt-2-a obj)))))

(defmethod replicate ((obj rtpt-2.1))
  (replicate-with (obj x (make-rtpt-2.1))
		  (setf (rtpt-2.1-a x) (replicate (rtpt-2.1-a obj)))
		  (setf (rtpt-2.1-c x) (replicate (rtpt-2.1-c obj)))
		  (setf (rtpt-2.1-d x) (replicate (rtpt-2.1-d obj)))))

(defmethod replicate ((obj rtpt-2.2))
  (replicate-with (obj x (make-rtpt-2.2))
		  (setf (rtpt-2.2-a x) (replicate (rtpt-2.2-a obj)))
		  (setf (rtpt-2.2-d x) (replicate (rtpt-2.2-d obj)))
		  (setf (rtpt-2.2-e x) (replicate (rtpt-2.2-e obj)))))

(defmethods make-random-type-containing*
  (1 ((val rtpt-2)) 'rtpt-2)
  (1 ((val rtpt-2.1)) 'rtpt-2.1)
  (1 ((val rtpt-2.2)) 'rtpt-2.2))

(def-type-prop-test structure-ref.2 'rtpt-2-a '(rtpt-2) 1)

