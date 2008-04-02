;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Apr 29 04:09:06 2003
;;;; Contains: Tests of SHARED-INITIALIZE

(in-package :cl-test)

(defclass shared-init-class-01 ()
  ((a :initform 'x :initarg :a)
   (b :initform 'y :initarg :b)
   (c :initarg :c)
   d))

(deftest shared-initialize.1.1
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj nil :a 1 :b 3 :c 14))
     (map-slot-boundp* obj '(a b c d))
     (map-slot-value obj '(a b c))))
  (nil nil nil nil)
  t
  (t t t nil)
  (1 3 14))

(deftest shared-initialize.1.2
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj nil))
     (map-slot-boundp* obj '(a b c d))))
  (nil nil nil nil)
  t
  (nil nil nil nil))

(deftest shared-initialize.1.3
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj nil :a 1 :a 2))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)))
  (nil nil nil nil)
  t
  (t nil nil nil)
  1)

(deftest shared-initialize.1.4
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj nil :a 1 :a 2 :allow-other-keys nil))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)))
  (nil nil nil nil)
  t
  (t nil nil nil)
  1)

(deftest shared-initialize.1.5
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj '(a) :a 1))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)))
  (nil nil nil nil)
  t
  (t nil nil nil)
  1)

(deftest shared-initialize.1.6
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj '(a)))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)))
  (nil nil nil nil)
  t
  (t nil nil nil)
  x)

(deftest shared-initialize.1.7
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj t))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (nil nil nil nil)
  t
  (t t nil nil)
  x y)

(deftest shared-initialize.1.8
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj t :b 10 :c 100))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)
     (slot-value obj 'b)
     (slot-value obj 'c)))
  (nil nil nil nil)
  t
  (t t t nil)
  x 10 100)

(deftest shared-initialize.1.9
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj nil :a 1 :b 10 :c 100))
     (eqt obj (shared-initialize obj nil :a 5 :b 37 :c 213))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)
     (slot-value obj 'b)
     (slot-value obj 'c)))
  (nil nil nil nil)
  t t
  (t t t nil)
  5 37 213)

(deftest shared-initialize.1.10
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (setf (slot-value obj 'a) 1000)
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj '(a)))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)))
  (t nil nil nil)
  t
  (t nil nil nil)
  1000)

;;; Initforms in the lexical environment of the defclass

(declaim (special *shared-init-var-02-init*
		  *shared-init-var-02-query*))

(declaim (type function *shared-init-var-02-init* *shared-init-var-02-query*))

(let ((ainit 0) (binit 0))
  (flet ((%init (a b) (setf ainit a binit b))
	 (%query () (list ainit binit)))
    (setf *shared-init-var-02-init* #'%init
	  *shared-init-var-02-query* #'%query)
    (defclass shared-init-class-02 ()
      ((a :initform (incf ainit) :initarg :a)
       (b :initform (incf binit) :initarg :b)
       (c :initarg :c)
       (d))
      (:default-initargs :c 100))))

(deftest shared-initialize.2.1
  (progn
    (funcall *shared-init-var-02-init* 5 10)
    (let ((obj (allocate-instance (find-class 'shared-init-class-02))))
      (values
       (funcall *shared-init-var-02-query*)
       (eqt obj (shared-initialize obj t))
       (slot-value obj 'a)
       (slot-value obj 'b)
       (map-slot-boundp* obj '(a b c d))
       (funcall *shared-init-var-02-query*))))
  (5 10)
  t
  6 11
  (t t nil nil)
  (6 11))

(deftest shared-initialize.2.2
  (progn
    (funcall *shared-init-var-02-init* 5 10)
    (let ((obj (allocate-instance (find-class 'shared-init-class-02))))
      (values
       (funcall *shared-init-var-02-query*)
       (eqt obj (shared-initialize obj nil))
       (map-slot-boundp* obj '(a b c d))
       (funcall *shared-init-var-02-query*))))
  (5 10)
  t
  (nil nil nil nil)
  (5 10))

(deftest shared-initialize.2.3
  (progn
    (funcall *shared-init-var-02-init* 5 10)
    (let ((obj (allocate-instance (find-class 'shared-init-class-02))))
      (values
       (funcall *shared-init-var-02-query*)
       (eqt obj (shared-initialize obj '(a)))
       (slot-value obj 'a)
       (map-slot-boundp* obj '(a b c d))
       (funcall *shared-init-var-02-query*))))
  (5 10)
  t
  6
  (t nil nil nil)
  (6 10))

(deftest shared-initialize.2.4
  (progn
    (funcall *shared-init-var-02-init* 5 10)
    (let ((obj (allocate-instance (find-class 'shared-init-class-02))))
      (values
       (funcall *shared-init-var-02-query*)
       (eqt obj (shared-initialize obj '(b)))
       (slot-value obj 'b)
       (map-slot-boundp* obj '(a b c d))
       (funcall *shared-init-var-02-query*))))
  (5 10)
  t
  11
  (nil t nil nil)
  (5 11))

(deftest shared-initialize.2.5
  (progn
    (funcall *shared-init-var-02-init* 5 10)
    (let ((obj (allocate-instance (find-class 'shared-init-class-02))))
      (values
       (funcall *shared-init-var-02-query*)
       (eqt obj (shared-initialize obj t :a 34 :b 49))
       (map-slot-value obj '(a b))
       (map-slot-boundp* obj '(a b c d))
       (funcall *shared-init-var-02-query*))))
  (5 10)
  t
  (34 49)
  (t t nil nil)
  (5 10))

(deftest shared-initialize.2.6
  (progn
    (funcall *shared-init-var-02-init* 5 10)
    (let ((obj (allocate-instance (find-class 'shared-init-class-02))))
      (values
       (funcall *shared-init-var-02-query*)
       (eqt obj (shared-initialize obj '(a b c d) :a 34 :b 49))
       (map-slot-value obj '(a b))
       (map-slot-boundp* obj '(a b c d))
       (funcall *shared-init-var-02-query*))))
  (5 10)
  t
  (34 49)
  (t t nil nil)
  (5 10))

;;; Defining new methods on shared-initialize

(defstruct shared-init-class-03
  a b c)

(defmethod shared-initialize ((obj shared-init-class-03)
			      slots-to-init
			      &key
			      (a nil a-p)
			      (b nil b-p)
			      (c nil c-p)
			      &allow-other-keys)
  (declare (ignore slots-to-init))
;;  (when a-p (setf (slot-value obj 'a) a))
;;  (when b-p (setf (slot-value obj 'b) b))
;;  (when c-p (setf (slot-value obj 'c) c))
  (when a-p (setf (shared-init-class-03-a obj) a))
  (when b-p (setf (shared-init-class-03-b obj) b))
  (when c-p (setf (shared-init-class-03-c obj) c))
  obj)

(deftest shared-initialize.3.1
  (let ((obj (make-shared-init-class-03)))
    (values
     (eqt obj (shared-initialize obj nil :a 1 :b 5 :c 19))
     (shared-init-class-03-a obj)
     (shared-init-class-03-b obj)
     (shared-init-class-03-c obj)))
  t 1 5 19)


;;; Inheritance

(defclass shared-init-class-04a ()
  ((a :initform 4 :initarg :a)
   (b :initform 8 :initarg :b)))

(defclass shared-init-class-04b (shared-init-class-04a)
  ((c :initform 17 :initarg :c) d)
  (:default-initargs :a 1))

(deftest shared-initialize.4.1
  (let ((obj (allocate-instance (find-class 'shared-init-class-04b))))
    (values
     (eqt obj (shared-initialize obj nil :a 'x))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)))
  t
  (t nil nil nil)
  x)

(deftest shared-initialize.4.2
  (let ((obj (allocate-instance (find-class 'shared-init-class-04b))))
    (values
     (eqt obj (shared-initialize obj nil))
     (map-slot-boundp* obj '(a b c d))))
  t
  (nil nil nil nil))

(deftest shared-initialize.4.3
  (let ((obj (allocate-instance (find-class 'shared-init-class-04b))))
    (values
     (eqt obj (shared-initialize obj t))
     (map-slot-boundp* obj '(a b c d))
     (map-slot-value obj '(a b c))))
  t
  (t t t nil)
  (4 8 17))

(deftest shared-initialize.4.4
  (let ((obj (allocate-instance (find-class 'shared-init-class-04b))))
    (values
     (eqt obj (shared-initialize obj '(a c)))
     (map-slot-boundp* obj '(a b c d))
     (map-slot-value obj '(a c))))
  t
  (t nil t nil)
  (4 17))

(deftest shared-initialize.4.5
  (let ((obj (allocate-instance (find-class 'shared-init-class-04b))))
    (values
     (eqt obj (shared-initialize obj '(a c) :c 81))
     (map-slot-boundp* obj '(a b c d))
     (map-slot-value obj '(a c))))
  t
  (t nil t nil)
  (4 81))

(deftest shared-initialize.4.6
  (let ((obj (allocate-instance (find-class 'shared-init-class-04b))))
    (values
     (eqt obj (shared-initialize obj '(a c) :a 91))
     (map-slot-boundp* obj '(a b c d))
     (map-slot-value obj '(a c))))
  t
  (t nil t nil)
  (91 17))

(deftest shared-initialize.4.7
  (let ((obj (allocate-instance (find-class 'shared-init-class-04b))))
    (values
     (eqt obj (shared-initialize obj '(c)))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'c)))
  t
  (nil nil t nil)
  17)

;;; shared-initialize and class slots

(defclass shared-init-class-05 ()
  ((a :initarg :a :allocation :class)
   (b :initarg :b :initform 'foo :allocation :class)))

(deftest shared-initialize.5.1
  (let* ((class (find-class 'shared-init-class-05))
	 (obj (allocate-instance class)))
    (slot-makunbound obj 'a)
    (slot-makunbound obj 'b)
    (values
     (eqt obj (shared-initialize obj t))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'b)))
  t
  (nil t)
  foo)

(deftest shared-initialize.5.2
  (let* ((class (find-class 'shared-init-class-05))
	 (obj (allocate-instance class)))
    (slot-makunbound obj 'a)
    (slot-makunbound obj 'b)
    (values
     (eqt obj (shared-initialize obj '(b)))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'b)))
  t
  (nil t)
  foo)

(deftest shared-initialize.5.3
  (let* ((class (find-class 'shared-init-class-05))
	 (obj (allocate-instance class))
	 (obj2 (allocate-instance class)))
    (slot-makunbound obj 'a)
    (slot-makunbound obj 'b)
    (values
     (eqt obj (shared-initialize obj t :a 117))
     (map-slot-boundp* obj '(a b))
     (map-slot-value obj '(a b))
     (map-slot-value obj2 '(a b))))
  t
  (t t)
  (117 foo)
  (117 foo))

(deftest shared-initialize.5.4
  (let* ((class (find-class 'shared-init-class-05))
	 (obj (allocate-instance class))
	 (obj2 (allocate-instance class)))
    (slot-makunbound obj 'a)
    (values
     (setf (slot-value obj 'b) 'bar)
     (eqt obj (shared-initialize obj t :a 117))
     (map-slot-boundp* obj '(a b))
     (map-slot-value obj '(a b))
     (map-slot-value obj2 '(a b))))
  bar
  t
  (t t)
  (117 bar)
  (117 bar))

;;; Shared initargs

(defclass shared-init-class-06 ()
  ((a :initarg :i1 :initarg :i2 :initform 'x)
   (b :initarg :i2 :initarg :i3 :initform 'y)))

(deftest shared-initialize.6.1
  (let* ((class (find-class 'shared-init-class-06))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (shared-initialize obj nil))
     (map-slot-boundp* obj '(a b))))
  (nil nil)
  t
  (nil nil))

(deftest shared-initialize.6.2
  (let* ((class (find-class 'shared-init-class-06))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (shared-initialize obj t))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (nil nil)
  t
  (t t)
  x y)

(deftest shared-initialize.6.3
  (let* ((class (find-class 'shared-init-class-06))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (shared-initialize obj nil :i1 'z))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)))
  (nil nil)
  t
  (t nil)
  z)

(deftest shared-initialize.6.4
  (let* ((class (find-class 'shared-init-class-06))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (shared-initialize obj nil :i2 'z))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (nil nil)
  t
  (t t)
  z z)

(deftest shared-initialize.6.5
  (let* ((class (find-class 'shared-init-class-06))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (shared-initialize obj nil :i1 'w :i2 'z))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (nil nil)
  t
  (t t)
  w z)

(deftest shared-initialize.6.6
  (let* ((class (find-class 'shared-init-class-06))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (shared-initialize obj nil :i2 'z :i1 'w))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (nil nil)
  t
  (t t)
  z z)

(deftest shared-initialize.6.7
  (let* ((class (find-class 'shared-init-class-06))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (shared-initialize obj nil :i2 'z :i2 'w))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (nil nil)
  t
  (t t)
  z z)


(deftest shared-initialize.6.8
  (let* ((class (find-class 'shared-init-class-06))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (shared-initialize obj nil :i2 'z :i2 'w :foo t))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (nil nil)
  t
  (t t)
  z z)


(deftest shared-initialize.6.9
  (let* ((class (find-class 'shared-init-class-06))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (shared-initialize obj nil :allow-other-keys nil
				 :i2 'z :i2 'w :foo t))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (nil nil)
  t
  (t t)
  z z)

;;; Before methods fill in slots before the default system method

(defclass shared-init-class-07 ()
  ((a :initform 'x)
   (b :initform 'y)))

(defmethod shared-initialize :before ((obj shared-init-class-07) slot-names &rest args)
  (declare (ignore args slot-names))
  (setf (slot-value obj 'a) 'foo)
  obj)

(deftest shared-initialize.7.1
  (let* ((class (find-class 'shared-init-class-07))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (shared-initialize obj nil))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)))
  (nil nil) t (t nil) foo)

(deftest shared-initialize.7.2
  (let* ((class (find-class 'shared-init-class-07))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (shared-initialize obj t))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (nil nil) t (t t) foo y)

;;; :around method tests

(defclass shared-init-class-08 ()
  ((a :initform 'x)
   (b :initform 'y)))

(defmethod shared-initialize :around ((obj shared-init-class-08) slot-names
				      &rest args &key only &allow-other-keys)
  (declare (ignore slot-names args))
  (setf (slot-value obj 'a) 'foo)
  (if only obj (call-next-method)))

(deftest shared-initialize.8.1
  (let* ((class (find-class 'shared-init-class-08))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (shared-initialize obj nil))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)))
  (nil nil)
  t
  (t nil)
  foo)

(deftest shared-initialize.8.2
  (let* ((class (find-class 'shared-init-class-08))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (shared-initialize obj t))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (nil nil)
  t
  (t t)
  foo y)

(deftest shared-initialize.8.3
  (let* ((class (find-class 'shared-init-class-08))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (shared-initialize obj t :only t))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)))
  (nil nil)
  t
  (t nil)
  foo)

;;;

(defclass shared-init-class-09 ()
  ((a :allocation :class :initform 'x)
   (b :initform 'y)))

(deftest shared-initialize.9.1
  (let* ((class (find-class 'shared-init-class-09))
	 (obj (allocate-instance class)))
    (slot-makunbound obj 'a)
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (shared-initialize obj '(b)))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'b)))
  (nil nil)
  t
  (nil t)
  y)

;;; Order of evaluation tests

(deftest shared-initialize.order.1
  (let ((obj (allocate-instance (find-class 'shared-init-class-01)))
	(i 0) x r y z w q)
    (values
     (eqt obj
	  (shared-initialize (progn (setf x (incf i)) obj)
			     (progn (setf r (incf i)) nil)
			     :b (setf y (incf i))
			     :a (setf z (incf i))
			     :b (setf w (incf i))
			     :c (setf q (incf i))))
     (map-slot-value obj '(a b c))
     i x r y z w q))
  t (4 3 6)
  6 1 2 3 4 5 6)

;;; Error tests

(deftest shared-initialize.error.1
  (signals-error (shared-initialize) program-error)
  t)

(deftest shared-initialize.error.2
  (signals-error (let ((obj (allocate-instance
			      (find-class 'shared-init-class-01))))
		    (shared-initialize obj))
		 program-error)
  t)

(deftest shared-initialize.error.3
  (signals-error (let ((obj (allocate-instance
			      (find-class 'shared-init-class-01))))
		    (shared-initialize obj nil :a))
		 program-error)
  t)

(deftest shared-initialize.error.4
  (signals-error (let ((obj (allocate-instance
			      (find-class 'shared-init-class-01))))
		    (shared-initialize obj nil '(a b c) nil))
		 program-error)
  t)
