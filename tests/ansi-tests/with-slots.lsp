;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 17 18:04:10 2003
;;;; Contains: Tests of WITH-SLOTS

(in-package :cl-test)

(deftest with-slots.1
  (with-slots () nil)
  nil)

(deftest with-slots.2
  (with-slots () nil (values)))

(deftest with-slots.3
  (with-slots () nil (values 'a 'b 'c 'd 'e 'f))
  a b c d e f)

(deftest with-slots.4
  (let ((x 0) (y 10) (z 20))
    (values
     x y z
     (with-slots () (incf x) (incf y 3) (incf z 100))
     x y z))
  0 10 20
  120
  1 13 120)

;;; with-slots is an implicit progn, not a tagbody

(deftest with-slots.5
  (block done
    (tagbody
     (with-slots () nil
		 (go 10)
		 10
		 (return-from done :bad))
     10
     (return-from done :good)))
  :good)

;;; with-slots has no implicit block
(deftest with-slots.6
  (block nil
    (with-slots () nil (return :good))
    (return :bad))
  :good)


;;; Tests on standard objects

(defclass with-slots-class-01 () ((a :initarg :a)
				  (b :initarg :b)
				  (c :initarg :c)))

(deftest with-slots.7
  (let ((obj (make-instance 'with-slots-class-01 :a 'x :b 'y :c 'z)))
    (with-slots (a b c) obj (values a b c)))
  x y z)

(deftest with-slots.8
  (let ((obj (make-instance 'with-slots-class-01 :a 'x :b 'y :c 'z)))
    (with-slots
     (a b c) obj
     (values (setf a 'p) (setf b 'q) (setf c 'r)
	     (map-slot-value obj '(a b c)))))
  p q r (p q r))

(deftest with-slots.9
  (let ((obj (make-instance 'with-slots-class-01 :a 'x :b 'y :c 'z)))
    (with-slots
     (a b c) obj
     (values (setq a 'p) (setq b 'q) (setq c 'r)
	     (map-slot-value obj '(a b c)))))
  p q r (p q r))

(deftest with-slots.10
  (let ((obj (make-instance 'with-slots-class-01 :a 'x :b 'y :c 'z)))
    (with-slots ((a2 a) (b2 b) (c2 c)) obj (values a2 b2 c2)))
  x y z)

(deftest with-slots.11
  (let ((obj (make-instance 'with-slots-class-01 :a 'x :b 'y :c 'z)))
    (with-slots
     ((a2 a) (b2 b) (c2 c)) obj
     (values (setf a2 'p) (setf b2 'q) (setf c2 'r)
	     (map-slot-value obj '(a b c)))))
  p q r (p q r))

(deftest with-slots.12
  (let ((obj (make-instance 'with-slots-class-01 :a 'x :b 'y :c 'z)))
    (with-slots
     ((a2 a) (b2 b) (c2 c)) obj
     (values (setq a2 'p) (setq b2 'q) (setq c2 'r)
	     (map-slot-value obj '(a b c)))))
  p q r (p q r))

(deftest with-slots.13
  (let ((obj (make-instance 'with-slots-class-01)))
    (with-slots
     (a b c) obj
     (values (setf a 'p) (setf b 'q) (setf c 'r)
	     (map-slot-value obj '(a b c)))))
  p q r (p q r))

(deftest with-slots.14
  (let ((obj (make-instance 'with-slots-class-01 :a 1 :b 2 :c 3)))
    (with-slots (a b c) obj
		(let ((obj (make-instance 'with-slots-class-01
					  :a 'bad :b 'bad :c 'bad)))
		  (values a b c))))
  1 2 3)


(deftest with-slots.15
  (let ((obj (make-instance 'with-slots-class-01 :a 1 :b 2 :c 3)))
    (with-slots (a b c) obj
		(with-slots
		 ((a2 a) (b2 b) (c2 c))
		 (make-instance 'with-slots-class-01
				:a 'bad :b 'bad :c 'bad)
		 (values a b c))))
  1 2 3)

(deftest with-slots.16
  (let ((obj (make-instance 'with-slots-class-01 :a 'bad :b 'bad :c 'bad)))
    (with-slots (a b c) obj
		(with-slots
		 (a b c)
		 (make-instance 'with-slots-class-01 :a 1 :b 2 :c 3)
		 (values a b c))))
  1 2 3)


(deftest with-slots.17
  (let ((obj (make-instance 'with-slots-class-01 :a 1 :b 2 :c 'bad)))
    (with-slots (a b) obj
		(with-slots
		 (c)
		 (make-instance 'with-slots-class-01 :a 'bad :b 'bad :c 3)
		 (values a b c))))
  1 2 3)

;;; If slot is unbound, act as if slot-value had been called

(defmethod slot-unbound ((class t)
			 (instance with-slots-class-01)
			 slot-name)
  'missing)

(deftest with-slots.18
  (let ((obj (make-instance 'with-slots-class-01)))
    (with-slots (a b c) obj (values a b c)))
  missing missing missing)

(deftest with-slots.19
  (let ((obj (make-instance 'with-slots-class-01 :a 'x :b 'y :c 'z)))
    (with-slots (a b c) obj
		(declare (optimize (speed 3) (safety 3)))
		(values a b c)))
  x y z)

(deftest with-slots.20
  (let ((obj (make-instance 'with-slots-class-01 :a 'x :b 'y :c 'z)))
    (with-slots (a b c) obj
		(declare (optimize (speed 3) (safety 3)))
		(declare (special *x*))
		(values a b c)))
  x y z)

;;; Free declaration scope test

(deftest with-slots.21
  (block done
    (let ((x :bad))
      (declare (special x))
      (let ((x :good))
	(with-slots nil (return-from done x)
		    (declare (special x))))))
  :good)