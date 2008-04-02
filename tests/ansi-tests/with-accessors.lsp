;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 17 17:07:29 2003
;;;; Contains: Tests of WITH-ACCESSORS

(in-package :cl-test)

(deftest with-accessors.1
  (with-accessors () nil)
  nil)

(deftest with-accessors.2
  (with-accessors () nil (values)))

(deftest with-accessors.3
  (with-accessors () nil (values 'a 'b 'c 'd 'e 'f))
  a b c d e f)

(deftest with-accessors.4
  (let (x y z)
    (with-accessors () (setf x 1) (setf y 5) (setf z 12) (values x y z)))
  1 5 12)

;; with-accessors defines an implicit progn, not a tagbody
(deftest with-accessors.5
  (block done
    (tagbody
     (with-accessors
      nil nil
      (go 10)
      10
      (return-from done :bad))
     10
     (return-from done :good)))
  :good)		     

(defclass with-accessors-class-01 ()
  ((a :initarg :a :accessor wa-a)
   (b :initarg :b :accessor wa-b)
   (c :initarg :c :accessor wa-c)))

(deftest with-accessors.6
  (let ((obj (make-instance 'with-accessors-class-01 :a 'x :b 'y :c 'z)))
    (with-accessors
     ((a wa-a) (b wa-b) (c wa-c))
     obj
     (values a b c)))
  x y z)

(deftest with-accessors.7
  (let ((obj (make-instance 'with-accessors-class-01)))
    (with-accessors
     ((a wa-a) (b wa-b) (c wa-c))
     obj
     (values (setf a 'x) (setf b 'y) (setf c 'z)
	     (map-slot-value obj '(a b c)))))
  x y z (x y z))

(deftest with-accessors.8
  (let ((obj (make-instance 'with-accessors-class-01)))
    (with-accessors
     ((a wa-a) (b wa-b) (c wa-c))
     obj
     (values (setq a 'x) (setq b 'y) (setq c 'z)
	     (map-slot-value obj '(a b c)))))
  x y z (x y z))

(deftest with-accessors.9
  (let ((obj (make-instance 'with-accessors-class-01 :a 5 :b 19 :c 312)))
    (with-accessors
     ((a wa-a) (b wa-b) (c wa-c))
     obj
     (values (incf a 4) (incf b 412) (incf c 75)
	     (map-slot-value obj '(a b c)))))
  9 431 387 (9 431 387))

(deftest with-accessors.10
  (let ((obj (make-instance 'with-accessors-class-01 :a 5 :b 19 :c 312)))
    (with-accessors
     ((a wa-a) (b wa-b) (c wa-c))
     obj
     (declare (optimize (speed 3) (safety 3)))
     (values a b c)))
  5 19 312)

(deftest with-accessors.11
  (let ((obj (make-instance 'with-accessors-class-01 :a 5 :b 19 :c 312)))
    (with-accessors
     ((a wa-a) (b wa-b) (c wa-c))
     obj
     (declare (optimize (speed 3) (safety 3)))
     (declare (special *x*)) ;; not used
     (values a b c)))
  5 19 312)

;;; with-accessors on structure accessors

(defstruct (with-accessors-struct-02 (:conc-name "WA-2-")) a b c)

(deftest with-accessors.12
  (let ((obj (make-with-accessors-struct-02 :a 'x :b 'y :c 'z)))
    (with-accessors ((a wa-2-a) (b wa-2-b) (c wa-2-c))
		    obj
		    (values a b c)))
  x y z)

(deftest with-accessors.13
  (let ((obj (make-with-accessors-struct-02)))
    (with-accessors
     ((a wa-2-a) (b wa-2-b) (c wa-2-c))
     obj
     (values (setf a 'x) (setf b 'y) (setf c 'z)
	     (wa-2-a obj) (wa-2-b obj) (wa-2-c obj))))
  x y z x y z)

;;; Free declaration scope test

(deftest with-accessors.14
  (block done
    (let ((x :bad))
      (declare (special x))
      (let ((x :good))
	(with-accessors nil (return-from done x)
			(declare (special x))))))
  :good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest with-accessors.15
  (macrolet
   ((%m (z) z))
   (let ((obj (make-with-accessors-struct-02 :a 'x :b 'y :c 'z)))
     (with-accessors ((a wa-2-a) (b wa-2-b) (c wa-2-c))
		     (expand-in-current-env (%m obj))
		     (values a b c))))
  x y z)

