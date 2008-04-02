;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon May 12 21:19:36 2003
;;;; Contains: Tests of MAKE-INSTANCE

(in-package :cl-test)

;;; MAKE-INSTANCE is used in many other tests as well

(deftest make-instance.error.1
  (signals-error (make-instance) program-error)
  t)

(defclass make-instance-class-01 ()
  ((a :initarg :a) (b :initarg :b)))

(deftest make-instance.error.2
  (signals-error (make-instance 'make-instance-class-01 :a)
		 program-error)
  t)

(deftest make-instance.error.3
  (handler-case (progn (eval '(make-instance 'make-instance-class-01 :z 1))
		       t)
		(error () :good))
  :good)

(deftest make-instance.error.4
  (handler-case (progn (eval '(make-instance
			       (find-class 'make-instance-class-01)
			       :z 1))
		       t)
		(error () :good))
  :good)

(deftest make-instance.error.5
  (signals-error (let () (make-instance) nil)
		 program-error)
  t)

(deftest make-instance.error.6
  (loop for cl in *built-in-classes*
	unless (eval `(signals-error (make-instance ',cl) error))
	collect cl)
  nil)

;; Definitions of methods

(defmethod make-instance ((x make-instance-class-01)
			  &rest initargs &key &allow-other-keys)
  initargs)

(deftest make-instance.1
  (make-instance (make-instance 'make-instance-class-01))
  nil)

(deftest make-instance.2
  (make-instance (make-instance 'make-instance-class-01) :a 1 :b 2)
  (:a 1 :b 2))

#|
(when *can-define-metaclasses*
  
  (defclass make-instance-class-02 ()
    (a b c)
    (:metaclass substandard-class))
  
  (defmethod make-instance ((class (eql (find-class 'make-instance-class-02)))
			    &rest initargs &key (x nil) (y nil) (z nil)
			    &allow-other-keys)
    (declare (ignore initargs))
    (let ((obj (allocate-instance class)))
      (setf (slot-value obj 'a) x
	    (slot-value obj 'b) y
	    (slot-value obj 'c) z)
      obj))
  
  (deftest make-instance.3
    (let ((obj (make-instance 'make-instance-class-02)))
      (values
       (eqt (class-of obj) (find-class 'make-instance-class-02))
       (slot-value obj 'a)
       (slot-value obj 'b)
       (slot-value obj 'c)))
    t nil nil nil)
  
  (deftest make-instance.4
    (let ((obj (make-instance 'make-instance-class-02 :z 10 :y 45 :x 'd)))
      (values
       (eqt (class-of obj) (find-class 'make-instance-class-02))
       (slot-value obj 'a)
       (slot-value obj 'b)
       (slot-value obj 'c)))
    t d 45 10)
  
  
  (deftest make-instance.5
    (let ((obj (make-instance (find-class 'make-instance-class-02) :y 'g)))
      (values
       (eqt (class-of obj) (find-class 'make-instance-class-02))
       (slot-value obj 'a)
       (slot-value obj 'b)
       (slot-value obj 'c)))
    t nil g nil)
  
  (deftest make-instance.6
    (eq (make-instance 'make-instance-class-02)
	(make-instance 'make-instance-class-02))
    nil)

  ;; Customization of make-instance
  
  (defclass make-instance-class-03 ()
    ((a :initform 1) (b :initarg :b) c)
    (:metaclass substandard-class))

  (defmethod make-instance ((class (eql (find-class 'make-instance-class-03)))
			    &rest initargs
			    &key (x nil x-p) (y nil y-p) (z nil z-p)
			    &allow-other-keys)
    (declare (ignore initargs))
    (let ((obj (allocate-instance (find-class 'make-instance-class-03))))
      (when x-p (setf (slot-value obj 'a) x))
      (when y-p (setf (slot-value obj 'b) y))
      (when z-p (setf (slot-value obj 'c) z))
      obj))
  
  (deftest make-instance.7
    (let ((obj (make-instance 'make-instance-class-03)))
      (values
       (eqt (class-of obj)
	    (find-class 'make-instance-class-03))
       (map-slot-boundp* obj '(a b c))))
    t (nil nil nil))
  
  (deftest make-instance.8
    (let* ((class (find-class 'make-instance-class-03))
	   (obj (make-instance class :b 10)))
      (values
       (eqt (class-of obj) class)
       (map-slot-boundp* obj '(a b c))))
    t (nil nil nil))
  
  (deftest make-instance.9
    (let* ((class (find-class 'make-instance-class-03))
	   (obj (make-instance class :x 'g :z 'i :y 'k :foo t :x 'bad)))
      (values
       (eqt (class-of obj) class)
       (map-slot-boundp* obj '(a b c))
       (map-slot-value obj '(a b c))))
    t (t t t) (g k i))

  ;; After method combination

  (defparameter *make-instance-class-04-var* 0)

  (defclass make-instance-class-04 ()
    ((a :initform *make-instance-class-04-var*))
    (:metaclass substandard-class))

  (defmethod make-instance :after
    ((class (eql (find-class 'make-instance-class-04)))
     &rest initargs &key &allow-other-keys)
    (declare (ignore initargs))
    (incf *make-instance-class-04-var* 10))
  
  (deftest make-instance.10
    (let* ((*make-instance-class-04-var* 0)
	   (obj (make-instance 'make-instance-class-04)))
      (values
       (slot-value obj 'a)
       *make-instance-class-04-var*))
    0 10)
  
  ;; Around method combination

  (defclass make-instance-class-05 ()
    ((a :initarg :a) (b :initarg :b :initform 'foo) c)
    (:metaclass substandard-class))

  (defmethod make-instance :around
    ((class (eql (find-class 'make-instance-class-05)))
     &rest initargs &key &allow-other-keys)
    (declare (ignore initargs))
    (let ((obj (call-next-method)))
      (setf (slot-value obj 'c) 'bar)
      obj))
  
  (deftest make-instance.11
    (let ((obj (make-instance 'make-instance-class-05)))
      (values
       (map-slot-boundp* obj '(a b c))
       (map-slot-value obj '(b c))))
    (nil t t)
    (foo bar))
  )
|#

;;; Order of argument evaluation

(deftest make-instance.order.1
  (let* ((i 0) x y
	 (obj (make-instance 'make-instance-class-01
			     :a (setf x (incf i))
			     :b (setf y (incf i)))))
    (values
     (map-slot-value obj '(a b))
     i x y))
  (1 2) 2 1 2)

(deftest make-instance.order.2
  (let* ((i 0) x y z w
	 (obj (make-instance 'make-instance-class-01
			     :a (setf x (incf i))
			     :b (setf y (incf i))
			     :b (setf z (incf i))
			     :a (setf w (incf i)))))
    (values
     (map-slot-value obj '(a b))
     i x y z w))
  (1 2) 4 1 2 3 4)

(deftest make-instance.order.3
  (let* ((i 0) u x y z w
	 (obj (make-instance (prog1 'make-instance-class-01
				    (setf u (incf i)))
			     :a (setf x (incf i))
			     :b (setf y (incf i))
			     :b (setf z (incf i))
			     :a (setf w (incf i)))))
    (values
     (map-slot-value obj '(a b))
     i u x y z w))
  (2 3) 5 1 2 3 4 5)
