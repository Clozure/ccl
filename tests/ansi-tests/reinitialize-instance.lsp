;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Apr 28 21:56:47 2003
;;;; Contains: Tests for REINITIALIZE-INSTANCE

(in-package :cl-test)

;;; Many of the classes used here are defined in defclass-??.lsp

(deftest reinitialize-instance.1
  (let* ((obj (make-instance 'class-01))
	 (obj2 (reinitialize-instance obj)))
    (values
     (eqt obj obj2)
     (map-slot-boundp* obj '(s1 s2 s3))))
  t (nil nil nil))


(deftest reinitialize-instance.2
  (let* ((obj (make-instance 'class-01))
	 (obj2 (reinitialize-instance obj :allow-other-keys nil)))
    (values
     (eqt obj obj2)
     (map-slot-boundp* obj '(s1 s2 s3))))
  t (nil nil nil))

(deftest reinitialize-instance.3
  (let* ((obj (make-instance 'class-01))
	 (obj2 (reinitialize-instance obj :allow-other-keys t)))
    (values
     (eqt obj obj2)
     (map-slot-boundp* obj '(s1 s2 s3))))
  t (nil nil nil))

(deftest reinitialize-instance.4
  (let* ((obj (make-instance 'class-01))
	 (obj2 (reinitialize-instance obj :allow-other-keys t
				      :allow-other-keys nil)))
    (values
     (eqt obj obj2)
     (map-slot-boundp* obj '(s1 s2 s3))))
  t (nil nil nil))

(deftest reinitialize-instance.5
  (let* ((obj (make-instance 'class-07))
	 (obj2 (reinitialize-instance obj :s1a 'a :s2 'b :s1a 'bad
				      :s2 'bad2  :s1b 'bad3)))
    (values
     (eqt obj obj2)
     (map-slot-value obj '(s1 s2))))
  t (a b))

(deftest reinitialize-instance.6
  (let* ((obj (make-instance 'class-07 :s1a 'a))
	 (obj2 (reinitialize-instance obj :s1b 'b)))
    (values
     (eqt obj obj2)
     (slot-value obj 's1)
     (slot-boundp* obj 's2)))
  t b nil)

(deftest reinitialize-instance.7
  (let* ((obj (make-instance 'class-07 :s1a 'a))
	 (obj2 (reinitialize-instance obj :s2 'b)))
    (values
     (eqt obj obj2)
     (slot-value obj 's1)
     (slot-value obj 's2)))
  t a b)


;;; Tests of user-defined methods

(defclass reinit-class-01 ()
  ((a :initarg :a) (b :initarg :b)))

(defmethod reinitialize-instance :after ((instance reinit-class-01)
					 &rest initargs
					 &key (x nil x-p))
  (declare (ignore initargs))
  (when x-p (setf (slot-value instance 'a) x))
  instance)

(deftest reinitialize-instance.8
  (let* ((obj (make-instance 'reinit-class-01))
	 (obj2 (reinitialize-instance obj :a 1 :b 3)))
    (values
     (eqt obj obj2)
     (map-slot-value obj2 '(a b))))
  t (1 3))

(deftest reinitialize-instance.9
  (let* ((obj (make-instance 'reinit-class-01 :a 10 :b 20))
	 (obj2 (reinitialize-instance obj :x 3)))
    (values
     (eqt obj obj2)
     (map-slot-value obj2 '(a b))))
  t (3 20))

(deftest reinitialize-instance.10
  (let* ((obj (make-instance 'reinit-class-01 :a 10 :b 20))
	 (obj2 (reinitialize-instance obj :x 3 :x 100)))
    (values
     (eqt obj obj2)
     (map-slot-value obj2 '(a b))))
  t (3 20))

;;; Order of evaluation tests

(deftest reinitialize-instance.order.1
  (let* ((obj (make-instance 'reinit-class-01))
	 (i 0) x y z w
	 (obj2 (reinitialize-instance
		(progn (setf x (incf i)) obj)
		:b (setf y (incf i))
		:a (setf z (incf i))
		:b (setf w (incf i)))))
    (values
     (eqt obj obj2)
     (map-slot-value obj2 '(a b))
     i x y z w))
  t (3 2) 4 1 2 3 4)

;;; Error cases

(deftest reinitialize-instance.error.1
  (handler-case
   (eval '(reinitialize-instance (make-instance 'class-01) :garbage t))
   (error () :good))
  :good)

(deftest reinitialize-instance.error.2
  (signals-error (reinitialize-instance) program-error)
  t)
