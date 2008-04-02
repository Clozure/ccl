;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 27 16:23:59 2003
;;;; Contains: Tests of DEFCLASS with more involved inheritance

(in-package :cl-test)

;;;

(defclass class-0301a ()
  (a b))

(defclass class-0301b ()
  (a c))

(defclass class-0301c (class-0301a class-0301b)
  (d))

(deftest class-0301.1
  (let ((c (make-instance 'class-0301c)))
    (values
     (typep* c 'class-0301a)
     (typep* c 'class-0301b)
     (typep* c 'class-0301c)
     (typep* c (find-class 'class-0301a))
     (typep* c (find-class 'class-0301b))
     (typep* c (find-class 'class-0301c))
     (map-slot-boundp* c '(a b c d))
     (setf (slot-value c 'a) 'w)
     (setf (slot-value c 'b) 'x)
     (setf (slot-value c 'c) 'y)
     (setf (slot-value c 'd) 'z)
     (map-slot-boundp* c '(a b c d))
     (map-slot-value c '(a b c d))))
  t t t
  t t t
  (nil nil nil nil)
  w x y z
  (t t t t)
  (w x y z))

;;;

(defclass class-0302a ()
  ((a :initform 'x) b (c :initform 'w)))

(defclass class-0302b ()
  ((a :initform 'y) (b :initform 'z)))

(defclass class-0302c (class-0302a class-0302b)
  (a b (c :initform 'v) d))

(deftest class-0302.1
  (let ((c (make-instance 'class-0302c)))
    (values
     (map-slot-boundp* c '(a b c d))
     (map-slot-value c '(a b c))))
  (t t t nil)
  (x z v))

;;;

(defclass class-0303a ()
  ((a :allocation :class) b))

(defclass class-0303b ()
  (a (b :allocation :class)))

(defclass class-0303c (class-0303a class-0303b) ())

(deftest class-0303.1
  (let ((c1 (make-instance 'class-0303a))
	(c2 (make-instance 'class-0303b))
	(c3 (make-instance 'class-0303c)))
    (slot-makunbound c1 'a)
    (slot-makunbound c2 'b)
    (values
     (loop for c in (list c1 c2 c3)
	   collect (map-slot-boundp* c '(a b)))
     (list (setf (slot-value c1 'a) 'x1)
	   (slot-boundp* c2 'a)
	   (slot-value c3 'a))
     (list (setf (slot-value c2 'a) 'x2)
	   (slot-value c1 'a)
	   (slot-value c2 'a)
	   (slot-value c3 'a))
     (list (setf (slot-value c3 'a) 'x3)
	   (slot-value c1 'a)
	   (slot-value c2 'a)
	   (slot-value c3 'a))
     ;;;
     (list (setf (slot-value c1 'b) 'y1)
	   (slot-value c1 'b)
	   (slot-boundp* c2 'b)
	   (slot-boundp* c3 'b))
     (list (setf (slot-value c2 'b) 'y2)
	   (slot-value c1 'b)
	   (slot-value c2 'b)
	   (slot-boundp c3 'b))
     (list (setf (slot-value c3 'b) 'y3)
	   (slot-value c1 'b)
	   (slot-value c2 'b)
	   (slot-value c3 'b))))
  ((nil nil) (nil nil) (nil nil))
  (x1 nil x1)
  (x2 x1 x2 x1)
  (x3 x3 x2 x3)
  ;;
  (y1 y1 nil nil)
  (y2 y1 y2 nil)
  (y3 y1 y2 y3))

;;;

(defclass class-0304a ()
  ((a :initform 'x)))

(defclass class-0304b (class-0304a) ())

(defclass class-0304c (class-0304a)
  ((a :initform 'y)))

(defclass class-0304d (class-0304b class-0304c)
  ())

(deftest class-0304.1
  (slot-value (make-instance 'class-0304d) 'a)
  y)

;;;

(defclass class-0305a ()
  ((a :initarg :a))
  (:default-initargs :a 'x))

(defclass class-0305b (class-0305a) ())

(defclass class-0305c (class-0305a)
  ()
  (:default-initargs :a 'y))

(defclass class-0305d (class-0305b class-0305c)
  ())

(deftest class-0305.1
  (slot-value (make-instance 'class-0305d) 'a)
  y)


;;; A test showing nonmonotonicity in the CLOS CPL algorithm

(defclass class-0306a () ((a :initform nil :reader a-slot)))
(defclass class-0306b (class-0306a) ((a :initform 'x)))
(defclass class-0306c (class-0306a) ((a :initform 'y)))
(defclass class-0306d (class-0306b) ())
(defclass class-0306e (class-0306b) ())
(defclass class-0306f (class-0306d class-0306c) ())
(defclass class-0306g (class-0306e) ())
(defclass class-0306h (class-0306f class-0306g) ())

;;; Class class-0306c should precede class-0306b in the
;;; CPL for class-0306h, even though it follows it in the CPLs
;;; for the direct superclasses of class-0306h.

(deftest class-0306.1
  (loop for obj in
	(mapcar #'make-instance
	     '(class-0306a class-0306b class-0306c class-0306d
	       class-0306e class-0306f class-0306g class-0306h))
	collect (slot-value obj 'a))
  (nil x y x x x x y))

(deftest class-0306.2
  (loop for obj in
	(mapcar #'make-instance
	     '(class-0306a class-0306b class-0306c class-0306d
	       class-0306e class-0306f class-0306g class-0306h))
	collect (a-slot obj))
  (nil x y x x x x y))

;;; A class redefinition test that came up in cmucl

(deftest class-0307.1
  (progn
    (setf (find-class 'class-0307a) nil
	  (find-class 'class-0307b) nil)
    (eval '(defclass class-0307a () ()))
    (eval '(defclass class-0307b (class-0307a) (a)))
    (eval '(defclass class-0307a () ((a :initform nil))))
    (eval '(defclass class-0307b (class-0307a) ((a :initform 'x))))
    (slot-value (make-instance 'class-0307b) 'a))
  x)

(deftest class-0308.1
  (progn
    (setf (find-class 'class-0308a) nil
	  (find-class 'class-0308b) nil)
    (eval '(defclass class-0308a () ()))
    (eval '(defclass class-0308b (class-0308a) (a)))
    (eval '(defclass class-0308a () ((a :initarg :a))))
    (eval '(defclass class-0308b (class-0308a) ()))
    (slot-value (make-instance 'class-0308b :a 'x) 'a))
  x)

;;; More class redefinition tests

(deftest class-0309.1
  (progn
    (setf (find-class 'class-0309) nil)
    (let* ((class1 (eval '(defclass class-0309 () ((a) (b) (c)))))
	   (obj1 (make-instance 'class-0309)))
      (setf (class-name class1) nil)
      (let ((class2 (eval '(defclass class-0309 () ((a) (b) (c))))))
	(values
	 (eqt (class-of obj1) class1)
	 (eqt class1 class2)
	 (typep* obj1 class1)
	 (typep* obj1 class2)))))
  t nil t nil)

(deftest class-0310.1
  (progn
    (setf (find-class 'class-0310a) nil
	  (find-class 'class-0310b) nil)
    (let* ((class1 (eval '(defclass class-0310a () ((a) (b) (c)))))
	   (obj1 (make-instance 'class-0310a)))
      (setf (class-name class1) 'class-0310b)
      (let ((class2 (eval '(defclass class-0310a () ((a) (b) (c))))))
	(values
	 (eqt (class-of obj1) class1)
	 (eqt class1 class2)
	 (typep* obj1 class1)
	 (typep* obj1 class2)
	 (class-name class1)
	 (class-name class2)))))
  t nil t nil class-0310b class-0310a)

(deftest class-0311.1
  (progn
    (setf (find-class 'class-0311) nil)
    (let* ((class1 (eval '(defclass class-0311 () ((a) (b) (c)))))
	   (obj1 (make-instance 'class-0311)))
      (setf (find-class 'class-0311) nil)
      (let ((class2 (eval '(defclass class-0311 () ((a) (b) (c))))))
	(values
	 (eqt (class-of obj1) class1)
	 (eqt class1 class2)
	 (typep* obj1 class1)
	 (typep* obj1 class2)
	 (class-name class1)
	 (class-name class2)
	 (eqt (find-class 'class-0311) class1)
	 (eqt (find-class 'class-0311) class2)))))
  t nil t nil class-0311 class-0311 nil t)
