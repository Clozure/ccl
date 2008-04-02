;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May  3 14:23:29 2003
;;;; Contains: Tests of CHANGE-CLASS

(in-package :cl-test)

(defclass change-class-class-01a ()
  ((a :initarg :a) (b :initarg :b) (c :initarg :c)))

(defclass change-class-class-01b ()
  ((c :initarg :c2) (d :initarg :d2) (b :initarg :b2)))

(deftest change-class.1.1
  (let ((obj (make-instance 'change-class-class-01a))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (map-slot-boundp* obj '(a b c))
     (slot-exists-p obj 'd)
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))))
  t nil (nil nil nil)
  nil t nil t nil (nil nil nil))

(deftest change-class.1.2
  (let ((obj (make-instance 'change-class-class-01a :a 1))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (map-slot-boundp* obj '(a b c))
     (slot-exists-p obj 'd)
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))))
  t nil (t nil nil)
  nil t nil t nil (nil nil nil))

(deftest change-class.1.3
  (let ((obj (make-instance 'change-class-class-01a :b 2))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (map-slot-boundp* obj '(a b c))
     (slot-exists-p obj 'd)
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (slot-value obj 'b)))
  t nil (nil t nil)
  nil t nil t nil (t nil nil) 2)

(deftest change-class.1.4
  (let ((obj (make-instance 'change-class-class-01a :a 1 :b 2 :c 5))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (map-slot-boundp* obj '(a b c))
     (slot-exists-p obj 'd)
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (map-slot-value obj '(b c))))
  t nil (t t t)
  nil t nil t nil (t t nil) (2 5))

(deftest change-class.1.5
  (let ((obj (make-instance 'change-class-class-01a :a 1 :b 2 :c 5))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (eqt obj (change-class obj new-class :b2 8 :c2 76))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (map-slot-value obj '(b c))))
  t nil t nil (t t nil) (8 76))

(deftest change-class.1.6
  (let ((obj (make-instance 'change-class-class-01a :a 1 :b 2 :c 5))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (eqt obj (change-class obj new-class :b2 19 :b2 34))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (map-slot-value obj '(b c))))
  t nil t nil (t t nil) (19 5))

(deftest change-class.1.7
  (let ((obj (make-instance 'change-class-class-01a :a 1 :b 2 :c 5))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (eqt obj (change-class obj new-class :allow-other-keys nil))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (map-slot-value obj '(b c))))
  t nil t nil (t t nil) (2 5))

(deftest change-class.1.8
  (let ((obj (make-instance 'change-class-class-01a :a 1 :b 2 :c 5))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (eqt obj (change-class obj new-class :allow-other-keys t))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (map-slot-value obj '(b c))))
  t nil t nil (t t nil) (2 5))

(deftest change-class.1.9
  (let ((obj (make-instance 'change-class-class-01a :a 1 :b 2 :c 5))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (eqt obj (change-class obj new-class :allow-other-keys t
			    :nonsense t))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (map-slot-value obj '(b c))))
  t nil t nil (t t nil) (2 5))

(deftest change-class.1.10
  (let ((obj (make-instance 'change-class-class-01a :a 1 :b 2 :c 5))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (eqt obj (change-class obj new-class :bad 0 :allow-other-keys t
			    :allow-other-keys nil :nonsense t))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (map-slot-value obj '(b c))))
  t nil t nil (t t nil) (2 5))

(deftest change-class.1.11
  (handler-case
   (eval
    '(let ((obj (make-instance 'change-class-class-01a))
	   (new-class (find-class 'change-class-class-01b)))
       (declare (optimize (safety 3)))
       (eqt obj (change-class obj new-class :nonsense t))))
   (error () :expected-error))
  :expected-error)

;; test of class name as second argument
(deftest change-class.1.12
  (let ((obj (make-instance 'change-class-class-01a :b 1))
	;; (new-class (find-class 'change-class-class-01b))
	)
    (values
     (eqt obj (change-class obj 'change-class-class-01b :c2 3))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (map-slot-value obj '(b c))))
  t nil t nil (t t nil) (1 3))


;;; Shared slots

(defclass change-class-class-02a ()
  ((a :initarg :a :allocation :class)
   (b :initarg :b :allocation :class)))

(defclass change-class-class-02b ()
  ((a :initarg :a2)
   (b :initarg :b2)))

(deftest change-class.2.1
  (let ((obj (make-instance 'change-class-class-02a))
	(new-class (find-class 'change-class-class-02b)))
    (slot-makunbound obj 'a)
    (slot-makunbound obj 'b)
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-02a)
     (typep* obj 'change-class-class-02b)
     (map-slot-boundp* (make-instance 'change-class-class-02a) '(a b))
     (map-slot-boundp* obj '(a b))))
  (nil nil)
  t nil t
  (nil nil)
  (nil nil))

(deftest change-class.2.2
  (let ((obj (make-instance 'change-class-class-02a))
	(obj2 (make-instance 'change-class-class-02a))
	obj3
	(new-class (find-class 'change-class-class-02b)))
    (setf (slot-value obj 'a) 'foo)
    (slot-makunbound obj 'b)
    (values
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj2 'a)
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-02a)
     (typep* obj 'change-class-class-02b)
     (map-slot-boundp* (setf obj3 (make-instance 'change-class-class-02a))
		       '(a b))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj2 'a)
     (slot-value obj3 'a)
     (eqt obj obj2) (eqt obj obj3) (eqt obj2 obj3)
     ))
  (t nil)
  foo foo
  t nil t
  (t nil)
  (t nil)
  foo foo foo
  nil nil nil)

(deftest change-class.2.3
  (let ((obj (make-instance 'change-class-class-02a))
	(obj2 (make-instance 'change-class-class-02a))
	(new-class (find-class 'change-class-class-02b)))
    (setf (slot-value obj 'a) 1
	  (slot-value obj 'b) 16)
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-02a)
     (typep* obj 'change-class-class-02b)
     (map-slot-boundp* obj2 '(a b))
     (map-slot-boundp* (make-instance 'change-class-class-02a) '(a b))
     (map-slot-boundp* obj '(a b))
     (progn (slot-makunbound obj2 'a)
	    (slot-makunbound obj2 'b)
	    (map-slot-boundp* obj '(a b)))))
		 
  (t t)
  t nil t
  (t t)
  (t t)
  (t t)
  (t t))

;;; Destination slots are shared

(defclass change-class-class-03a ()
  ((a :initarg :a) (b :initarg :b)))

(defclass change-class-class-03b ()
  ((a :allocation :class :initarg :a2)
   (b :allocation :class :initarg :b2)))

(deftest change-class.3.1
  (let* ((obj (make-instance 'change-class-class-03a))
	 (new-class (find-class 'change-class-class-03b))
	 (obj2 (make-instance new-class))
	 obj3)
    (slot-makunbound obj2 'a)
    (slot-makunbound obj2 'b)
    (values
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-03a)
     (typep* obj 'change-class-class-03b)
     (typep* obj new-class)
     (eqt (setq obj3 (make-instance new-class)) obj)
     (map-slot-boundp* obj '(a b))
     (map-slot-boundp* obj2 '(a b))
     (map-slot-boundp* obj3 '(a b))
     ))
  t nil t t nil (nil nil) (nil nil) (nil nil))

(deftest change-class.3.2
  (let* ((obj (make-instance 'change-class-class-03a :a 1))
	 (new-class (find-class 'change-class-class-03b))
	 (obj2 (make-instance new-class))
	 obj3)
    (slot-makunbound obj2 'a)
    (setf (slot-value obj2 'b) 17)
    (values
     (map-slot-boundp* obj2 '(a b))
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-03a)
     (typep* obj 'change-class-class-03b)
     (typep* obj new-class)
     (eqt (setq obj3 (make-instance new-class)) obj)
     (map-slot-boundp* obj '(a b))
     (map-slot-boundp* obj2 '(a b))
     (map-slot-boundp* obj3 '(a b))
     (slot-value obj 'b)
     (slot-value obj2 'b)
     (slot-value obj3 'b)
     ))
  (nil t) t nil t t nil (nil t) (nil t) (nil t) 17 17 17)

;;; Destination class has slot initforms

(defclass change-class-class-04a ()
  ((a :initarg :a) (b :initarg :b)))

(defclass change-class-class-04b ()
  ((a :initform 'x :initarg :a2)
   (c :initform 'y :initarg :c2)))

(deftest change-class.4.1
  (let ((obj (make-instance 'change-class-class-04a))
	(new-class (find-class 'change-class-class-04b)))
    (values
     (eqt obj (change-class obj new-class))
     (map-slot-boundp* obj '(a c))
     (slot-value obj 'c)))
  t
  (nil t)
  y)

(deftest change-class.4.2
  (let ((obj (make-instance 'change-class-class-04a))
	(new-class (find-class 'change-class-class-04b)))
    (values
     (eqt obj (change-class obj new-class :a2 'z))
     (map-slot-value obj '(a c))))
  t
  (z y))

(deftest change-class.4.3
  (let ((obj (make-instance 'change-class-class-04a :a 'p :b 'q))
	(new-class (find-class 'change-class-class-04b)))
    (values
     (eqt obj (change-class obj new-class))
     (map-slot-value obj '(a c))))
  t
  (p y))

(deftest change-class.4.4
  (let ((obj (make-instance 'change-class-class-04a))
	(new-class (find-class 'change-class-class-04b)))
    (values
     (eqt obj (change-class obj new-class :c2 'k))
     (map-slot-boundp* obj '(a c))
     (slot-value obj 'c)))
  t
  (nil t)
  k)

(deftest change-class.4.5
  (let* ((class (find-class 'change-class-class-04b))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a c))
     (eqt obj (change-class obj class))
     (map-slot-boundp* obj '(a c))))
  (nil nil)
  t
  (nil nil))
     

;;; Custom methods for change-class

(declaim (special *changed-class-on-class-05*))

(defclass change-class-class-05 ()
  (a b c))

(report-and-ignore-errors
 (defmethod change-class
   ((obj change-class-class-05)
    (new-class (eql (find-class 'change-class-class-05)))
    &rest initargs &key &allow-other-keys)
   (declare (ignore initargs new-class))
   (setq *changed-class-on-class-05* t)
   obj))

(deftest change-class.5
  (let ((*changed-class-on-class-05* nil)
	(obj (make-instance 'change-class-class-05)))
    (values
     (eqt obj (change-class obj (find-class 'change-class-class-05)))
     *changed-class-on-class-05*))
  t t)

;;; Method that invokes the standard method with call-next-method

(defclass change-class-class-06 ()
  ((a :initarg :a) (b :initarg :b) (c :initarg :c)))

(report-and-ignore-errors
 (defmethod change-class
   ((obj change-class-class-06)
    (new-class standard-class)
    &rest initargs &key &allow-other-keys)
   (declare (ignore initargs))
   (setf (slot-value obj 'a) 123)
   (call-next-method)))

(deftest change-class.6.1
  (let* ((class (find-class 'change-class-class-06))
	 (obj (make-instance class)))
    (values
     (map-slot-boundp* obj '(a b c))
     (eqt obj (change-class obj class))
     (map-slot-boundp* obj '(a b c))
     (slot-value obj 'a)
     ))
  (nil nil nil)
  t
  (t nil nil)
  123)

(deftest change-class.6.2
  (let* ((class (find-class 'change-class-class-06))
	 (obj (make-instance class :a 'bad)))
    (values
     (map-slot-boundp* obj '(a b c))
     (eqt obj (change-class obj class))
     (map-slot-boundp* obj '(a b c))
     (slot-value obj 'a)
     ))
  (t nil nil)
  t
  (t nil nil)
  123)

;;; Before method

(defclass change-class-class-07 ()
  ((a :initform 'x :initarg :a)
   (b :initform 'y :initarg :b)
   (c :initarg :c)))

(defclass change-class-class-07b ()
  ((a :initform 'aa :initarg :a)
   (d :initform 'dd :initarg :d)))

(report-and-ignore-errors
 (defmethod change-class :before
   ((obj change-class-class-07)
    (new-class standard-class)
    &rest initargs &key &allow-other-keys)
   (declare (ignore initargs))
   (setf (slot-value obj 'a) 'z)
   obj))
  
(deftest change-class.7.1
  (let* ((class (find-class 'change-class-class-07))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b c))
     (eqt obj (change-class obj class))
     (map-slot-boundp* obj '(a b c))
     (slot-value obj 'a)))
  (nil nil nil)
  t
  (t nil nil)
  z)

(deftest change-class.7.2
  (let* ((class (find-class 'change-class-class-07))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b c))
     (eqt obj (change-class obj class :a 10))
     (map-slot-boundp* obj '(a b c))
     (slot-value obj 'a)))
  (nil nil nil)
  t
  (t nil nil)
  10)

(deftest change-class.7.3
  (let* ((class (find-class 'change-class-class-07))
	 (obj (allocate-instance class)))
    (values
     (map-slot-boundp* obj '(a b c))
     (eqt obj (change-class obj class :b 10))
     (map-slot-boundp* obj '(a b c))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (nil nil nil)
  t
  (t t nil)
  z 10)

(deftest change-class.7.4
  (let* ((class (find-class 'change-class-class-07))
	 (new-class (find-class 'change-class-class-07b))
	 (obj (allocate-instance class)))
    (values
     (eqt obj (change-class obj new-class))
     (map-slot-boundp* obj '(a d))
     (slot-value obj 'a)
     (slot-value obj 'd)))
  t (t t) z dd)

(deftest change-class.7.5
  (let* ((class (find-class 'change-class-class-07))
	 (new-class (find-class 'change-class-class-07b))
	 (obj (allocate-instance class)))
    (values
     (eqt obj (change-class obj new-class :allow-other-keys nil))
     (map-slot-boundp* obj '(a d))
     (slot-value obj 'a)
     (slot-value obj 'd)))
  t (t t) z dd)

(deftest change-class.7.6
  (let* ((class (find-class 'change-class-class-07))
	 (new-class (find-class 'change-class-class-07b))
	 (obj (allocate-instance class)))
    (values
     (eqt obj (change-class obj new-class :allow-other-keys t))
     (map-slot-boundp* obj '(a d))
     (slot-value obj 'a)
     (slot-value obj 'd)))
  t (t t) z dd)


;;; After method

(report-and-ignore-errors
 (defclass change-class-class-08 ()
   ((a :initarg :a) (b :initarg :b))))

(report-and-ignore-errors
 (defmethod change-class :after
   ((obj change-class-class-08)
    (class (eql (find-class 'change-class-class-08)))
    &rest initargs &key &allow-other-keys)
   (declare (ignore initargs))
   (setf (slot-value obj 'a) 'z)
   obj))

(deftest change-class.8.1
  (let* ((class (find-class 'change-class-class-08))
	 (obj (make-instance class)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (change-class obj class))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)))
  (nil nil)
  t
  (t nil)
  z)
       
(deftest change-class.8.2
  (let* ((class (find-class 'change-class-class-08))
	 (obj (make-instance class :a 1 :b 2)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (change-class obj class))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (t t)
  t
  (t t)
  z 2)

(deftest change-class.8.3
  (let* ((class (find-class 'change-class-class-08))
	 (obj (make-instance class)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (change-class obj class :a 12 :b 17))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (nil nil)
  t
  (t t)
  z 17)

;;; Put around method test here

;;; Put more inheritance tests here
  
;;; Error tests

(deftest change-class.error.1
  (signals-error (change-class) program-error)
  t)

(deftest change-class.error.2
  (signals-error (change-class (make-instance 'change-class-class-01a))
		 program-error)
  t)

(deftest change-class.error.3
  (signals-error
   (let ((obj (make-instance 'change-class-class-01a))
	 (new-class (find-class 'change-class-class-01b)))
     (change-class obj new-class :c2))
   program-error)
  t)

(deftest change-class.error.4
  (signals-error
   (let ((obj (make-instance 'change-class-class-01a))
	 (new-class (find-class 'change-class-class-01b)))
     (change-class obj new-class '(nonsense) 'a))
   program-error)
  t)

;;; According to the page for BUILT-IN-CLASS, using CHANGE-CLASS
;;; to change the class to/from a builtin class should raise a
;;; signal of type ERROR.

(deftest change-class.error.5
  (let ((built-in-class (find-class 'built-in-class)))
    (loop for e in *mini-universe*
	  for class = (class-of e)
	  when (and (eq (class-of class) built-in-class)
		    (handler-case
		     (progn
		       (change-class (make-instance 'change-class-class-01a)
				     class)
		       t)
		     (error () nil)))
	  collect e))
  nil)

(deftest change-class.error.6
  (let ((built-in-class (find-class 'built-in-class)))
    (loop for e in *mini-universe*
	  for class = (class-of e)
	  when (and (eq (class-of class) built-in-class)
		    (handler-case
		     (progn
		       (change-class e (find-class 'change-class-class-01a))
		       t)
		     (error () nil)))
	  collect e))
  nil)
