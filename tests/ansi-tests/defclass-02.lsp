;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Apr 25 07:16:57 2003
;;;; Contains: Tests of DEFCLASS with simple inheritance

(in-package :cl-test)

;;;

(defclass class-0201 ()
  ((a :initform 'x) (b :allocation :instance) (c :reader class-0201-c)))

(defclass class-0202 (class-0201)
  (d (e :initform 'y) (f :allocation :instance)))

(deftest class-0201.1
  (let ((c (make-instance 'class-0201)))
    (values (map-slot-boundp* c '(a b c))
	    (map-slot-exists-p* c '(a b c))
	    (slot-value c 'a)
	    (map-typep* c (list 'class-0201 'class-0202
				(find-class 'class-0201)
				(find-class 'class-0202)))
	    (class-name (class-of c))
	    ))
  (t nil nil)
  (t t t)
  x
  (t nil t nil)
  class-0201)

(deftest class-0202.1
  (let ((c (make-instance 'class-0202)))
    (values (map-slot-boundp* c '(a b c d e f))
	    (map-slot-value c '(a e))
	    (map-typep* c (list 'class-0201 'class-0202
				(find-class 'class-0201)
				(find-class 'class-0202)))
	    (class-name (class-of c))
	    ))
  (t nil nil nil t nil)
  (x y)
  (t t t t)
  class-0202)

;;;


(defclass class-0203 ()
  ((a :allocation :class) (b :allocation :instance)))

(defclass class-0204 (class-0203)
  (c d))

(deftest class-0203.1
  (let ((c1 (make-instance 'class-0203))
	(c2 (make-instance 'class-0204)))
    (values
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b c d))
     (setf (slot-value c1 'a) 'x)
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b c d))
     (slot-value c1 'a)
     (slot-value c2 'a)
     (eqt (slot-makunbound c1 'a) c1)
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b c d))))
  (nil nil)
  (nil nil nil nil)
  x
  (t nil)
  (t nil nil nil)
  x x
  t
  (nil nil)
  (nil nil nil nil))

  
(deftest class-0203.2
  (let ((c1 (make-instance 'class-0203))
	(c2 (make-instance 'class-0204)))
    (values
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b c d))
     (setf (slot-value c1 'a) 'x)
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b c d))
     (slot-value c1 'a)
     (slot-value c2 'a)
     (eqt (slot-makunbound c2 'a) c2)
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b c d))))
  (nil nil)
  (nil nil nil nil)
  x
  (t nil)
  (t nil nil nil)
  x x
  t
  (nil nil)
  (nil nil nil nil))

;;;

(defclass class-0205a ()
  ((a :initform 'x)
   (b :initform 'y)
   c))

(defclass class-0205b (class-0205a)
  ((a :initform 'z)
   b
   (c :initform 'w)))

(deftest class-0205a.1
  (let ((c (make-instance 'class-0205a)))
    (values
     (slot-value c 'a)
     (slot-value c 'b)
     (slot-boundp c 'c)))
  x y nil)

(deftest class-0205b.1
  (let ((c (make-instance 'class-0205b)))
    (map-slot-value c '(a b c)))
  (z y w))

;;;

(defclass class-0206a ()
  ((a :allocation :instance)
   (b :allocation :class)))

(defclass class-0206b (class-0206a)
  ((a :allocation :class)
   (b :allocation :instance)))

(deftest class-0206.1
  (let ((c1 (make-instance 'class-0206a))	
	(c2 (make-instance 'class-0206b)))
    (values
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b))
     (setf (slot-value c1 'a) 'x)
     (setf (slot-value c1 'b) 'y)
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b))
     (map-slot-value c1 '(a b))
     (progn (slot-makunbound c1 'a)
	    (slot-makunbound c1 'b)
	    (setf (slot-value c2 'a) 'x))
     (setf (slot-value c2 'b) 'y)
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b))
     (map-slot-value c2 '(a b))
     (progn (slot-makunbound c2 'a)
	    (slot-makunbound c2 'b)
	    nil)))
  (nil nil) (nil nil)
  x y
  (t t) (nil nil)
  (x y)
  x y
  (nil nil) (t t)
  (x y)
  nil)

;;;

;;; Show shadowing of slots by :allocation

(defclass class-0207a ()
  ((a :allocation :class)))

(defclass class-0207b (class-0207a)
  ((a :allocation :instance)))

(defclass class-0207c (class-0207b)
  ((a :allocation :class)))

(deftest class-0207.1
  (let ((c1 (make-instance 'class-0207a))
	(c2 (make-instance 'class-0207b))
	(c3 (make-instance 'class-0207c)))
    (slot-makunbound c1 'a)
    (slot-makunbound c2 'a)
    (slot-makunbound c3 'a)
    (values
     (setf (slot-value c1 'a) 'x)
     (slot-boundp* c1 'a)
     (slot-boundp* c2 'a)
     (slot-boundp* c3 'a)
     (slot-value c1 'a)
     (setf (slot-value c2 'a) 'y)
     (slot-boundp* c1 'a)
     (slot-boundp* c2 'a)
     (slot-boundp* c3 'a)
     (slot-value c1 'a)
     (slot-value c2 'a)
     (setf (slot-value c3 'a) 'z)
     (slot-boundp* c1 'a)
     (slot-boundp* c2 'a)
     (slot-boundp* c3 'a)
     (slot-value c1 'a)
     (slot-value c2 'a)
     (slot-value c3 'a)))
  x
  t nil nil
  x
  y
  t t nil
  x y
  z
  t t t
  x y z)

;;;

;;; Initforms are inherited even if :allocation changes

(defclass class-0208a ()
  ((a :allocation :class :initform 'x)))

(defclass class-0208b (class-0208a)
  ((a :allocation :instance)))

(deftest class-0208.1
  (values
   (slot-value (make-instance 'class-0208a) 'a)
   (slot-value (make-instance 'class-0208b) 'a))
  x x)

;;;

;;; That was failing when things were reloaded.
;;; Try a test that redefines it

(deftest class-redefinition.1
  (let*
    ((cobj1 (eval '(defclass class-0209a ()
		     ((a :allocation :class :initform 'x)))))
     (cobj2 (eval '(defclass class-0209b (class-0209a)
		     ((a :allocation :instance)))))
     (cobj3 (eval '(defclass class-0209a ()
		     ((a :allocation :class :initform 'x)))))
     (cobj4 (eval '(defclass class-0209b (class-0209a)
		     ((a :allocation :instance))))))
    (values
     (eqt cobj1 cobj3)
     (eqt cobj2 cobj4)
     (class-name cobj1)
     (class-name cobj2)
     (slot-value (make-instance 'class-0209a) 'a)
     (slot-value (make-instance 'class-0209b) 'a)))
  t t
  class-0209a
  class-0209b
  x x)

(deftest class-redefinition.2
  (let*
      (
       (cobj1 (eval '(defclass class-0210a ()
		       ((a :allocation :class)))))
       (cobj2 (eval '(defclass class-0210b (class-0210a)
		       ((a :allocation :instance)))))
       (cobj3 (eval '(defclass class-0210c (class-0210b)
		       ((a :allocation :class)))))
       (dummy (progn
		(setf (slot-value (make-instance 'class-0210a) 'a) :bad1)
		(make-instance 'class-0210b)
		(make-instance 'class-0210c)
		nil))
       (cobj4 (eval '(defclass class-0210a ()
		       ((a :allocation :class)))))
       (cobj5 (eval '(defclass class-0210b (class-0210a)
		       ((a :allocation :instance)))))
       (cobj6 (eval '(defclass class-0210c (class-0210b)
		       ((a :allocation :class))))))
    (list
     (eqt cobj1 cobj4)
     (eqt cobj2 cobj5)
     (eqt cobj3 cobj6)
     (class-name cobj1)
     (class-name cobj2)
     (class-name cobj3)
     (let ((c1 (make-instance 'class-0210a))
	   (c2 (make-instance 'class-0210b))
	   (c3 (make-instance 'class-0210c)))
       (slot-makunbound c1 'a)
       (slot-makunbound c2 'a)
       (slot-makunbound c3 'a)
       (list
	(setf (slot-value c1 'a) 'x)
	(and (slot-boundp* c1 'a) (slot-value c1 'a))
	(slot-boundp* c2 'a)
	(slot-boundp* c3 'a)
	(setf (slot-value c2 'a) 'y)
	(and (slot-boundp* c1 'a) (slot-value c1 'a))
	(and (slot-boundp* c2 'a) (slot-value c2 'a))
	(slot-boundp* c3 'a)
	(setf (slot-value c3 'a) 'z)
	(and (slot-boundp* c1 'a) (slot-value c1 'a))
	(and (slot-boundp* c2 'a) (slot-value c2 'a))
	(and (slot-boundp* c3 'a) (slot-value c3 'a))))))
  (t t t 
     class-0210a
     class-0210b
     class-0210c
     (x
      x nil nil
      y
      x y nil
      z
      x y z)))

;;; Same as class-redefinition.1, but reverse the order in which
;;; the classes are redefined.
(deftest class-redefinition.3
  (let*
    ((cobj1 (eval '(defclass class-redef-03a ()
		     ((a :allocation :class :initform 'x)))))
     (cobj2 (eval '(defclass class-redef-03b (class-redef-03a)
		     ((a :allocation :instance)))))
     (cobj4 (eval '(defclass class-redef-03b (class-redef-03a)
		     ((a :allocation :instance)))))
     (cobj3 (eval '(defclass class-redef-03a ()
		     ((a :allocation :class :initform 'x))))))
    (values
     (eqt cobj1 cobj3)
     (eqt cobj2 cobj4)
     (class-name cobj1)
     (class-name cobj2)
     (slot-value (make-instance 'class-redef-03a) 'a)
     (slot-value (make-instance 'class-redef-03b) 'a)))
  t t
  class-redef-03a
  class-redef-03b
  x x)

;;; Initforms are inherited even if :allocation changes

(defclass class-0211a ()
  ((a :allocation :instance :initform 'x)))

(defclass class-0211b (class-0211a)
  ((a :allocation :class)))

(deftest class-0211.1
  (values
   (slot-value (make-instance 'class-0211a) 'a)
   (slot-value (make-instance 'class-0211b) 'a))
  x x)

;;;

;;; Inheritance of :initargs

(defclass class-0212a ()
  ((a :initarg :a1)))

(defclass class-0212b (class-0212a)
  ((a :initarg :a2)
   (b :initarg :b)))

(deftest class-0212.1
  (let ((c (make-instance 'class-0212a :a1 'x)))
    (values
     (typep* c 'class-0212a)
     (typep* c 'class-0212b)
     (slot-value c 'a)
     (slot-exists-p c 'b)))
  t nil x nil)

(deftest class-0212.2
  (let ((c (make-instance 'class-0212b :a1 'x)))
    (values
     (typep* c 'class-0212a)
     (typep* c 'class-0212b)
     (slot-value c 'a)
     (slot-boundp* c 'b)))
  t t x nil)

(deftest class-0212.3
  (let ((c (make-instance 'class-0212b :a2 'x :b 'y)))
    (values
     (typep* c 'class-0212a)
     (typep* c 'class-0212b)
     (slot-value c 'a)
     (slot-value c 'b)))
  t t x y)

(deftest class-0212.4
  (let ((c (make-instance 'class-0212b :a1 'z :a2 'x :b 'y)))
    (values
     (typep* c 'class-0212a)
     (typep* c 'class-0212b)
     (slot-value c 'a)
     (slot-value c 'b)))
  t t z y)

(deftest class-0212.5
  (let ((c (make-instance 'class-0212b :a2 'x :b 'y :a1 'z)))
    (values
     (typep* c 'class-0212a)
     (typep* c 'class-0212b)
     (slot-value c 'a)
     (slot-value c 'b)))
  t t x y)

;;;

(defclass class-0213a ()
  ((a :initarg :a1)))

(defclass class-0213b (class-0213a)
  (b))

(deftest class-0213.1
  (let ((c (make-instance 'class-0213a :a1 'x)))
    (values
     (typep* c 'class-0213a)
     (typep* c 'class-0213b)
     (slot-value c 'a)
     (slot-exists-p c 'b)))
  t nil x nil)

(deftest class-0213.2
  (let ((c (make-instance 'class-0213b :a1 'x)))
    (values
     (typep* c 'class-0213a)
     (typep* c 'class-0213b)
     (slot-value c 'a)
     (slot-boundp* c 'b)))
  t t x nil)

;;;

(defclass class-0214a ()
  ((a :initarg :a1 :allocation :class)))

(defclass class-0214b (class-0214a)
  (b))

(deftest class-0214.1
  (let ((c (make-instance 'class-0214a :a1 'x)))
    (values
     (typep* c 'class-0214a)
     (typep* c 'class-0214b)
     (slot-value c 'a)
     (slot-exists-p c 'b)))
  t nil x nil)

(deftest class-0214.2
  (let ((c (make-instance 'class-0214b :a1 'y)))
    (values
     (typep* c 'class-0214a)
     (typep* c 'class-0214b)
     (slot-value c 'a)
     (slot-boundp* c 'b)))
  t t y nil)

;;;

(defclass class-0215a ()
  ((a :initarg :a1 :allocation :instance)))

(defclass class-0215b (class-0215a)
  ((a :allocation :class)))

(deftest class-0215.1
  (let ((c (make-instance 'class-0215a :a1 'x)))
    (values
     (typep* c 'class-0215a)
     (typep* c 'class-0215b)
     (slot-value c 'a)))
  t nil x)

(deftest class-0215.2
  (let ((c (make-instance 'class-0215b :a1 'y)))
    (values
     (typep* c 'class-0215a)
     (typep* c 'class-0215b)
     (slot-value c 'a)))
  t t y)


;;; Tests of defaulted initargs

(defclass class-0216a ()
  ((a :initarg :a1)
   (b :initarg :b1)))

(defclass class-0216b (class-0216a)
  ()
  (:default-initargs :a1 'x))

(deftest class-0216.1
  (let ((c (make-instance 'class-0216a)))
    (values
     (typep* c 'class-0216a)
     (typep* c 'class-0216b)
     (slot-boundp c 'a)
     (slot-boundp c 'b)))
  t nil nil nil)

(deftest class-0216.2
  (let ((c (make-instance 'class-0216b)))
    (values
     (typep* c 'class-0216a)
     (typep* c 'class-0216b)
     (slot-value c 'a)
     (slot-boundp c 'b)))
  t t x nil)

;;;

(defclass class-0217a ()
  ((a :initarg :a1)
   (b :initarg :b1)
   (c :initarg :c1)
   (d :initarg :d1))
  (:default-initargs :a1 10 :b1 20))

(defclass class-0217b (class-0217a)
  ()
  (:default-initargs :a1 30 :c1 40))

(deftest class-0217.1
  (let ((c (make-instance 'class-0217a)))
    (values
     (map-slot-boundp* c '(a b c d))
     (map-slot-value c '(a b))))
  (t t nil nil)
  (10 20))

(deftest class-0217.2
  (let ((c (make-instance 'class-0217a :a1 'x :c1 'y)))
    (values
     (map-slot-boundp* c '(a b c d))
     (map-slot-value c '(a b c))))
  (t t t nil)
  (x 20 y))

(deftest class-0217.3
  (let ((c (make-instance 'class-0217b)))
    (values
     (map-slot-boundp* c '(a b c d))
     (map-slot-value c '(a b c))))
  (t t t nil)
  (30 20 40))

(deftest class-0217.4
  (let ((c (make-instance 'class-0217b :a1 'x :d1 'y)))
    (values
     (map-slot-boundp* c '(a b c d))
     (map-slot-value c '(a b c d))))
  (t t t t)
  (x 20 40 y))

;;;

(defclass class-0218a ()
  ((a :initarg :a1))
  (:default-initargs :a1 'x))

(defclass class-0218b (class-0218a)
  ((a :initform 'y)))

(deftest class-0218.1
  (let ((c (make-instance 'class-0218a)))
     (slot-value c 'a))
  x)

(deftest class-0218.2
  (let ((c (make-instance 'class-0218b)))
     (slot-value c 'a))
  x)

;;;

(declaim (special *class-0219-a-1* *class-0219-a-2*))

(defclass class-0219a ()
  ((a :initarg :a1))
  (:default-initargs :a1 (setf *class-0219-a-1* 'x)))

(defclass class-0219b ()
  ((a :initarg :a1))
  (:default-initargs :a1 (setf *class-0219-a-2* 'y)))

(deftest class-0219.1
  (let ((*class-0219-a-1* nil))
    (values
     (slot-value (make-instance 'class-0219a) 'a)
     *class-0219-a-1*))
  x x)

(deftest class-0219.2
  (let ((*class-0219-a-1* nil)
	(*class-0219-a-2* nil))
    (values
     (slot-value (make-instance 'class-0219b) 'a)
     *class-0219-a-1*
     *class-0219-a-2*))
  y nil y)

;;;

(defclass class-0220a ()
  ((a :type (integer 0 10) :initarg :a)))

(defclass class-0220b (class-0220a)
  ((a :type (integer -5 5))))

(deftest class-0220.1
  (slot-value (make-instance 'class-0220a :a 10) 'a)
  10)

(deftest class-0220.2
  (slot-value (make-instance 'class-0220a :a 0) 'a)
  0)

(deftest class-0220.3
  (slot-value (make-instance 'class-0220b :a 0) 'a)
  0)

(deftest class-0220.4
  (slot-value (make-instance 'class-0220b :a 5) 'a)
  5)

;;;

(defclass class-0221a ()
  (a b c)
  (:documentation "This is class class-0221a"))

(defclass class-0221b (class-0221a)
  ())

(defclass class-0221c (class-0221a)
  ()
  (:documentation "This is class class-0221c"))

(deftest class-0221.1
  (let* ((cl (find-class 'class-0221a))
	 (doc (documentation cl t)))
    (or (null doc)
	(equalt doc "This is class class-0221a")))
  t)

(deftest class-0221.2
  (let* ((cl (find-class 'class-0221b))
	 (doc (documentation cl t)))
    doc)
  nil)

(deftest class-0221.3
  (let* ((cl (find-class 'class-0221c))
	 (doc (documentation cl t)))
    (or (null doc)
	(equalt doc "This is class class-0221c")))
  t)

;;;

(defclass class-0222a ()
  ((s1 :reader s1-r :writer s1-w :accessor s1-acc)))

(defclass class-0222b (class-0222a)
  ())

(deftest class-0222.1
  (let ((c (make-instance 'class-0222a)))
    (values
     (s1-w 'x c)
     (s1-r c)
     (s1-acc c)
     (setf (s1-acc c) 'y)
     (s1-r c)))
  x x x y y)

(deftest class-0222.2
  (let ((c (make-instance 'class-0222b)))
    (values
     (s1-w 'x c)
     (s1-r c)
     (s1-acc c)
     (setf (s1-acc c) 'y)
     (s1-r c)))
  x x x y y)

;;;

(defclass class-0223a ()
  ((s1 :reader s-r :writer s-w :accessor s-acc)))

(defclass class-0223b (class-0223a)
  ((s2 :reader s-r :writer s-w :accessor s-acc)))

(deftest class-0223.1
  (let ((c (make-instance 'class-0223b)))
    (values
     (setf (slot-value c 's1) 'x)
     (setf (slot-value c 's2) 'y)
     (s-r c)
     (s-acc c)
     (s-w 'z c)
     (slot-value c 's1)
     (slot-value c 's2)
     (s-r c)
     (s-acc c)))
  x y y y z x z z z)

