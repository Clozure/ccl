;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 20:58:54 2003
;;;; Contains: Tests for DEFCLASS, part 01


(in-package :cl-test)

;;; I've decided to write some 'manual' tests, then refactor these back
;;; to the automatic mechanisms I'll put into defclass-aux.lsp after
;;; I have a better understanding of the object system

(defclass class-01 () (s1 s2 s3))

(deftest class-01.1
  (notnot-mv (typep (make-instance 'class-01) 'class-01))
  t)

(deftest class-01.2
  (notnot-mv (typep (make-instance (find-class 'class-01)) 'class-01))
  t)

(deftest class-01.3
  (let ((c (make-instance 'class-01)))
    (values
     (setf (slot-value c 's1) 12)
     (setf (slot-value c 's2) 18)
     (setf (slot-value c 's3) 27)
     (loop for s in '(s1 s2 s3) collect (slot-value c s))))
  12 18 27
  (12 18 27))

;;;;

(defclass class-02 () ((s1) (s2) (s3)))
  
(deftest class-02.1
  (notnot-mv (typep (make-instance 'class-02) 'class-02))
  t)

(deftest class-02.2
  (notnot-mv (typep (make-instance (find-class 'class-02)) 'class-02))
  t)

(deftest class-02.3
  (let ((c (make-instance 'class-02)))
    (values
     (setf (slot-value c 's1) 12)
     (setf (slot-value c 's2) 18)
     (setf (slot-value c 's3) 27)
     (loop for s in '(s1 s2 s3) collect (slot-value c s))))
  12 18 27
  (12 18 27))

;;;;

(defclass class-03 () ((s1 :type integer) (s2 :type t) (s3 :type fixnum)))
  
(deftest class-03.1
  (notnot-mv (typep (make-instance 'class-03) 'class-03))
  t)

(deftest class-03.2
  (notnot-mv (typep (make-instance (find-class 'class-03)) 'class-03))
  t)

(deftest class-03.3
  (let ((c (make-instance 'class-03)))
    (values
     (setf (slot-value c 's1) 12)
     (setf (slot-value c 's2) 'a)
     (setf (slot-value c 's3) 27)
     (loop for s in '(s1 s2 s3) collect (slot-value c s))))
  12 a 27
  (12 a 27))

;;;;

(defclass class-04 ()
  ((s1 :reader s1-r) (s2 :writer s2-w) (s3 :accessor s3-a)))

;;; Readers, writers, and accessors
(deftest class-04.1
  (let ((c (make-instance 'class-04)))
    (values
     (setf (slot-value c 's1) 'a)
     (setf (slot-value c 's2) 'b)
     (setf (slot-value c 's3) 'c)
     (s1-r c)
     (slot-value c 's2)
     (s2-w 'd c)
     (slot-value c 's2)
     (s3-a c)
     (setf (s3-a c) 'e)
     (slot-value c 's3)
     (s3-a c)))
  a b c a b d d c e e e)

(deftest class-04.2
  (notnot-mv (typep #'s1-r 'generic-function))
  t)

(deftest class-04.3
  (notnot-mv (typep #'s2-w 'generic-function))
  t)

(deftest class-04.4
  (notnot-mv (typep #'s3-a 'generic-function))
  t)

(deftest class-04.5
  (notnot-mv (typep #'(setf s3-a) 'generic-function))
  t)

;;;;

(defclass class-05 () (s1 (s2 :allocation :instance) (s3 :allocation :class)))
  
(deftest class-05.1
  (let ((c1 (make-instance 'class-05))
	(c2 (make-instance 'class-05)))
    (values
     (not (eql c1 c2))
     (list
      (setf (slot-value c1 's1) 12)
      (setf (slot-value c2 's1) 17)
      (slot-value c1 's1)
      (slot-value c2 's1))
     (list
      (setf (slot-value c1 's2) 'a)
      (setf (slot-value c2 's2) 'b)
      (slot-value c1 's2)
      (slot-value c2 's2))
     (list
      (setf (slot-value c1 's3) 'x)
      (slot-value c1 's3)
      (slot-value c2 's3)
      (setf (slot-value c2 's3) 'y)
      (slot-value c1 's3)
      (slot-value c2 's3)
      (setf (slot-value c1 's3) 'z)
      (slot-value c1 's3)
      (slot-value c2 's3))
     (slot-value (make-instance 'class-05) 's3)))
  t
  (12 17 12 17)
  (a b a b)
  (x x x y y y z z z)
  z)

;;;;

(defclass class-06 () ((s1 :reader s1-r1 :reader s1-r2 :writer s1-w1 :writer s1-w2)))
(defclass class-06a () ((s1 :reader s1-r1) s3))

(deftest class-06.1
  (let ((c (make-instance 'class-06)))
    (values
     (setf (slot-value c 's1) 'x)
     (slot-value c 's1)
     (s1-r1 c)
     (s1-r2 c)
     (s1-w1 'y c)
     (slot-value c 's1)
     (s1-r1 c)
     (s1-r2 c)
     (s1-w2 'z c)
     (slot-value c 's1)
     (s1-r1 c)
     (s1-r2 c)))
  x x x x y y y y z z z z)

(deftest class-06.2
  (let ((c1 (make-instance 'class-06))
	(c2 (make-instance 'class-06a)))
    (values
     (setf (slot-value c1 's1) 'x)
     (setf (slot-value c2 's1) 'y)
     (mapcar #'s1-r1 (list c1 c2))))
  x y (x y))

;;;;

(defclass class-07 () ((s1 :initarg :s1a :initarg :s1b :reader s1)
		       (s2 :initarg :s2 :reader s2)))

(deftest class-07.1
  (let ((c (make-instance 'class-07)))
    (values
     (slot-boundp c 's1)
     (slot-boundp c 's2)))
  nil nil)

(deftest class-07.2
  (let ((c (make-instance 'class-07 :s1a 'x)))
    (values
     (notnot (slot-boundp c 's1))
     (s1 c)
     (slot-boundp c 's2)))
  t x nil)

(deftest class-07.3
  (let ((c (make-instance 'class-07 :s1b 'x)))
    (values
     (notnot (slot-boundp c 's1))
     (s1 c)
     (slot-boundp c 's2)))
  t x nil)

(deftest class-07.4
  (let ((c (make-instance 'class-07 :s1a 'y :s1b 'x)))
    (values
     (notnot (slot-boundp c 's1))
     (s1 c)
     (slot-boundp c 's2)))
  t y nil)


(deftest class-07.5
  (let ((c (make-instance 'class-07 :s1b 'y :s1a 'x)))
    (values
     (notnot (slot-boundp c 's1))
     (s1 c)
     (slot-boundp c 's2)))
  t y nil)

(deftest class-07.6
  (let ((c (make-instance 'class-07 :s1a 'y :s1a 'x)))
    (values
     (notnot (slot-boundp c 's1))
     (s1 c)
     (slot-boundp c 's2)))
  t y nil)

(deftest class-07.7
  (let ((c (make-instance 'class-07 :s2 'a :s1a 'b)))
    (values
     (notnot (slot-boundp c 's1))
     (notnot (slot-boundp c 's2))
     (s1 c)
     (s2 c)))
  t t b a)

(deftest class-07.8
  (let ((c (make-instance 'class-07 :s2 'a :s1a 'b :s2 'x :s1a 'y :s1b 'z)))
    (values
     (notnot (slot-boundp c 's1))
     (notnot (slot-boundp c 's2))
     (s1 c)
     (s2 c)))
  t t b a)

(deftest class-07.9
  (let ((c (make-instance 'class-07 :s1b 'x :s1a 'y)))
    (values
     (notnot (slot-boundp c 's1))
     (slot-boundp c 's2)
     (s1 c)))
  t nil x)

(deftest class-07.10
  (let ((c (make-instance 'class-07 :s1a 'x :s2 'y :allow-other-keys nil)))
    (values (s1 c) (s2 c)))
  x y)

(deftest class-07.11
  (let ((c (make-instance 'class-07 :s1a 'a :s2 'b :garbage 'z
			  :allow-other-keys t)))
    (values (s1 c) (s2 c)))
  a b)

(deftest class-07.12
  (let ((c (make-instance 'class-07 :s1a 'd :s2 'c :garbage 'z
			  :allow-other-keys t
			  :allow-other-keys nil)))
    (values (s1 c) (s2 c)))
  d c)


;;;;

(declaim (special *class-08-s2-initvar*))

(defclass class-08 ()
  ((s1 :initform 0) (s2 :initform *class-08-s2-initvar*)))

(deftest class-08.1
  (let* ((*class-08-s2-initvar* 'x)
	 (c (make-instance 'class-08)))
    (values
     (slot-value c 's1)
     (slot-value c 's2)))
  0 x)

;;;;

(declaim (special *class-09-s2-initvar*))

(defclass class-09 ()
  ((s1 :initform 0 :initarg :s1)
   (s2 :initform *class-09-s2-initvar* :initarg :s2)))

(deftest class-09.1
  (let* ((*class-09-s2-initvar* 'x)
	 (c (make-instance 'class-09)))
    (values
     (slot-value c 's1)
     (slot-value c 's2)))
  0 x)

(deftest class-09.2
  (let* ((*class-09-s2-initvar* 'x)
	 (c (make-instance 'class-09 :s1 1)))
    (values
     (slot-value c 's1)
     (slot-value c 's2)))
  1 x)

(deftest class-09.3
  (let* ((c (make-instance 'class-09 :s2 'a)))
    (values
     (slot-value c 's1)
     (slot-value c 's2)))
  0 a)

(deftest class-09.4
  (let* ((c (make-instance 'class-09 :s2 'a :s1 10 :s1 'bad :s2 'bad)))
    (values
     (slot-value c 's1)
     (slot-value c 's2)))
  10 a)

;;;;

(declaim (special *class-10-s1-initvar*))

(defclass class-10 ()
  ((s1 :initform (incf *class-10-s1-initvar*) :initarg :s1)))

(deftest class-10.1
  (let* ((*class-10-s1-initvar* 0)
	 (c (make-instance 'class-10)))
    (values
     *class-10-s1-initvar*
     (slot-value c 's1)))
  1 1)

(deftest class-10.2
  (let* ((*class-10-s1-initvar* 0)
	 (c (make-instance 'class-10 :s1 10)))
    (values
     *class-10-s1-initvar*
     (slot-value c 's1)))
  0 10)

;;;;

(let ((x 7))
  (defclass class-11 ()
    ((s1 :initform x :initarg :s1))))

(deftest class-11.1
  (slot-value (make-instance 'class-11) 's1)
  7)

(deftest class-11.2
  (slot-value (make-instance 'class-11 :s1 100) 's1)
  100)

;;;

(flet ((%f () 'x))
  (defclass class-12 ()
    ((s1 :initform (%f) :initarg :s1))))

(deftest class-12.1
  (slot-value (make-instance 'class-12) 's1)
  x)

(deftest class-12.2
  (slot-value (make-instance 'class-12 :s1 'y) 's1)
  y)

;;;

(defclass class-13 ()
  ((s1 :allocation :class :initarg :s1)))

(deftest class-13.1
  (let ((c1 (make-instance 'class-13))
	(c2 (make-instance 'class-13 :s1 'foo)))
    (values
     (slot-value c1 's1)
     (slot-value c2 's1)))
  foo foo)

;;;

(defclass class-14 ()
  ((s1 :initarg nil :reader s1)))

(deftest class-14.1
  (let ((c (make-instance 'class-14 nil 'x)))
    (s1 c))
  x)

;;;

(defclass class-15 ()
  ((s1 :initarg :allow-other-keys :reader s1)))

;;; Dicussion on comp.lang.lisp convinced me this test was bogus.
;;; The default value of :allow-other-keys specified in 7.1.2 is not
;;; the same as the default value forms, specified by :default-initargs,
;;; that are used to produce the defaulted initialization argument list.

;;; (deftest class-15.1
;;;  (let ((c (make-instance 'class-15)))
;;;    (s1 c))
;;;  nil)

(deftest class-15.2
  (let ((c (make-instance 'class-15 :allow-other-keys nil)))
    (s1 c))
  nil)

(deftest class-15.3
  (let ((c (make-instance 'class-15 :allow-other-keys t)))
    (s1 c))
  t)

(deftest class-15.4
  (let ((c (make-instance 'class-15 :allow-other-keys t
			  :allow-other-keys nil)))
    (s1 c))
  t)

(deftest class-15.5
  (let ((c (make-instance 'class-15 :allow-other-keys nil
			  :allow-other-keys t)))
    (s1 c))
  nil)

(deftest class-15.6
  (let ((c (make-instance 'class-15 :allow-other-keys t
			  :foo 'bar)))
    (s1 c))
  t)

(deftest class-15.7
  (let ((c (make-instance 'class-15 :allow-other-keys t
			  :allow-other-keys nil
			  :foo 'bar)))
    (s1 c))
  t)

;;; Tests of :default-initargs

(defclass class-16 ()
  ((s1 :initarg :s1))
  (:default-initargs :s1 'x))

(deftest class-16.1
  (let ((c (make-instance 'class-16)))
    (slot-value c 's1))
  x)

(deftest class-16.2
  (let ((c (make-instance 'class-16 :s1 'y)))
    (slot-value c 's1))
  y)

(deftest class-16.3
  (let ((c (make-instance 'class-16 :s1 nil)))
    (slot-value c 's1))
  nil)

;;;

(defclass class-17 ()
  ((s1 :initarg :s1 :initform 'foo))
  (:default-initargs :s1 'bar))

(deftest class-17.1
  (let ((c (make-instance 'class-17)))
    (slot-value c 's1))
  bar)

(deftest class-17.2
  (let ((c (make-instance 'class-17 :s1 'z)))
    (slot-value c 's1))
  z)

(deftest class-17.3
  (let ((c (make-instance 'class-17 :s1 nil)))
    (slot-value c 's1))
  nil)

;;;

(defclass class-18 ()
  ((s1 :initarg :s1 :initarg :s1b))
  (:default-initargs :s1 'x :s1b 'y))

(deftest class-18.1
  (let ((c (make-instance 'class-18)))
    (slot-value c 's1))
  x)

(deftest class-18.2
  (let ((c (make-instance 'class-18 :s1 'z)))
    (slot-value c 's1))
  z)

(deftest class-18.3
  (let ((c (make-instance 'class-18 :s1 nil)))
    (slot-value c 's1))
  nil)

(deftest class-18.4
  (let ((c (make-instance 'class-18 :s1b 'z)))
    (slot-value c 's1))
  z)

(deftest class-18.5
  (let ((c (make-instance 'class-18 :s1b nil)))
    (slot-value c 's1))
  nil)

;;;

(declaim (special *class-19-s1-initvar*))

(defclass class-19 ()
  ((s1 :initarg :s1))
  (:default-initargs :s1 (setf *class-19-s1-initvar* 'a)))

(deftest class-19.1
  (let* ((*class-19-s1-initvar* nil)
	 (c (make-instance 'class-19)))
    (declare (special *class-19-s1-initvar*))
    (values
     (slot-value c 's1)
     *class-19-s1-initvar*))
  a a)

(deftest class-19.2
  (let* ((*class-19-s1-initvar* nil)
	 (c (make-instance 'class-19 :s1 nil)))
    (declare (special *class-19-s1-initvar*))
    (values
     (slot-value c 's1)
     *class-19-s1-initvar*))
  nil nil)

(deftest class-19.3
  (let* ((*class-19-s1-initvar* nil)
	 (c (make-instance 'class-19 :s1 'x)))
    (declare (special *class-19-s1-initvar*))
    (values
     (slot-value c 's1)
     *class-19-s1-initvar*))
  x nil)

;;;

(declaim (special *class-20-s1-initvar-1* *class-20-s1-initvar-2*))

(defclass class-20 ()
  ((s1 :initarg :s1 :initarg :s1b))
  (:default-initargs :s1 (setf *class-20-s1-initvar-1* 'a)
		     :s1b (setf *class-20-s1-initvar-2* 'b)))

(deftest class-20.1
  (let* (*class-20-s1-initvar-1*
	 *class-20-s1-initvar-2*
	 (c (make-instance 'class-20)))
    (declare (special *class-20-s1-initvar-1*
		      *class-20-s1-initvar-2*))
    (values
     (slot-value c 's1)
     *class-20-s1-initvar-1*
     *class-20-s1-initvar-2*))
  a a b)

(deftest class-20.2
  (let* (*class-20-s1-initvar-1*
	 *class-20-s1-initvar-2*
	 (c (make-instance 'class-20 :s1 'x)))
    (declare (special *class-20-s1-initvar-1*
		      *class-20-s1-initvar-2*))
    (values
     (slot-value c 's1)
     *class-20-s1-initvar-1*
     *class-20-s1-initvar-2*))
  x nil b)

(deftest class-20.3
  (let* (*class-20-s1-initvar-1*
	 *class-20-s1-initvar-2*
	 (c (make-instance 'class-20 :s1b 'y)))
    (declare (special *class-20-s1-initvar-1*
		      *class-20-s1-initvar-2*))
    (values
     (slot-value c 's1)
     *class-20-s1-initvar-1*
     *class-20-s1-initvar-2*))
  y a nil)

;;;

(declaim (special *class-21-s1-initvar-1* *class-21-s1-initvar-2*))

(let ((*class-21-s1-initvar-1* 0)
      (*class-21-s1-initvar-2* 0))
  (defclass class-21 ()
    ((s1 :initarg :s1  :initarg :s1b)
     (s2 :initarg :s1b :initarg :s2))
    (:default-initargs :s1  (incf *class-21-s1-initvar-1*)
		       :s1b (incf *class-21-s1-initvar-2*))))

(deftest class-21.1
  (let* ((*class-21-s1-initvar-1* 10)
	 (*class-21-s1-initvar-2* 20)
	 (c (make-instance 'class-21)))
    (declare (special *class-21-s1-initvar-1*
		      *class-21-s1-initvar-2*))
    (values
     (slot-value c 's1)
     (slot-value c 's2)
     *class-21-s1-initvar-1*
     *class-21-s1-initvar-2*))
  11 21 11 21)

(deftest class-21.2
  (let* ((*class-21-s1-initvar-1* 10)
	 (*class-21-s1-initvar-2* 20)
	 (c (make-instance 'class-21 :s1 'x)))
    (declare (special *class-21-s1-initvar-1*
		      *class-21-s1-initvar-2*))
    (values
     (slot-value c 's1)
     (slot-value c 's2)
     *class-21-s1-initvar-1*
     *class-21-s1-initvar-2*))
  x 21 10 21)

(deftest class-21.3
  (let* ((*class-21-s1-initvar-1* 10)
	 (*class-21-s1-initvar-2* 20)
	 (c (make-instance 'class-21 :s1 'x :s1b 'y)))
    (declare (special *class-21-s1-initvar-1*
		      *class-21-s1-initvar-2*))
    (values
     (slot-value c 's1)
     (slot-value c 's2)
     *class-21-s1-initvar-1*
     *class-21-s1-initvar-2*))
  x y 10 20)

(deftest class-21.4
  (let* ((*class-21-s1-initvar-1* 10)
	 (*class-21-s1-initvar-2* 20)
	 (c (make-instance 'class-21 :s1b 'y)))
    (declare (special *class-21-s1-initvar-1*
		      *class-21-s1-initvar-2*))
    (values
     (slot-value c 's1)
     (slot-value c 's2)
     *class-21-s1-initvar-1*
     *class-21-s1-initvar-2*))
  y y 11 20)

(deftest class-21.5
  (let* ((*class-21-s1-initvar-1* 10)
	 (*class-21-s1-initvar-2* 20)
	 (c (make-instance 'class-21 :s2 'y)))
    (declare (special *class-21-s1-initvar-1*
		      *class-21-s1-initvar-2*))
    (values
     (slot-value c 's1)
     (slot-value c 's2)
     *class-21-s1-initvar-1*
     *class-21-s1-initvar-2*))
  11 y 11 21)

;;; Documentation strings

(defclass class-22 ()
  ((s1 :documentation "This is slot s1 in class class-22")))

(deftest class-22.1
  (notnot-mv (typep (make-instance 'class-22) 'class-22))
  t)

;;; We can't portably get at the docstring of slots

;;;

(defclass class-23 ()
  (s1 s2 s3)
  (:documentation "This is class-23 in ansi-tests"))

(deftest class-23.1
  (notnot-mv (typep (make-instance 'class-23) 'class-23))
  t)

(deftest class-23.2
  (let ((doc (documentation 'class-23 'type)))
    (or (null doc)
	(equalt doc "This is class-23 in ansi-tests")))
  t)

(deftest class-23.3
  (let ((doc (documentation (find-class 'class-23) 'type)))
    (or (null doc)
	(equalt doc "This is class-23 in ansi-tests")))
  t)

(deftest class-23.4
  (let ((doc (documentation (find-class 'class-23) t)))
    (or (null doc)
	(equalt doc "This is class-23 in ansi-tests")))
  t)

;;;

(defclass class-24 ()
  ((s1 :initarg :allow-other-keys :reader s1))
  (:default-initargs :allow-other-keys t))

(deftest class-24.1
  (s1 (make-instance 'class-24))
  t)

(deftest class-24.2
  (s1 (make-instance 'class-24 :nonsense t))
  t)

(deftest class-24.3
  (s1 (make-instance 'class-24 :allow-other-keys nil))
  nil)

(deftest class-24.4
  (s1 (make-instance 'class-24 :allow-other-keys 'a :foo t))
  a)

;;;

(defclass class-25 ()
  ((s1 :initarg :allow-other-keys :reader s1))
  (:default-initargs :allow-other-keys nil))

(deftest class-25.1
  (s1 (make-instance 'class-25))
  nil)

(deftest class-25.2
  (s1 (make-instance 'class-25 :allow-other-keys t))
  t)

(deftest class-25.3
  (s1 (make-instance 'class-25 :allow-other-keys t :foo nil))
  t)

(deftest class-25.4
  (s1 (make-instance 'class-25 :allow-other-keys t :allow-other-keys nil))
  t)

(deftest class-25.5
  (s1 (make-instance 'class-25 :allow-other-keys t :allow-other-keys nil
		     :foo t))
  t)

(deftest class-25.6
  (s1 (make-instance 'class-25 :allow-other-keys 'foo :allow-other-keys 'bar))
  foo)

;;;

(defclass class-26 ()
  ((s1-26 :writer (setf s1-26))))

(deftest class-26.1
  (let ((c (make-instance 'class-26)))
    (values
     (slot-boundp c 's1-26)
     (setf (s1-26 c) 'x)
     (slot-value c 's1-26)
     (typep* #'(setf s1-26) 'generic-function)))
  nil x x t)

;;;

(defclass class-27 ()
  (a (b :initform 10) (c :initarg :c) (d :initarg :d))
  (:metaclass standard-class)
  (:default-initargs :d 17))

(deftest class-27.1
  (let ((class (find-class 'class-27)))
    (values
     (subtypep* 'class-27 'standard-object)
     (subtypep* 'class-27 t)
     (subtypep* 'class-27 (find-class 'standard-object))
     (subtypep* 'class-27 (find-class t))
     (subtypep* class 'standard-object)
     (subtypep* class t)
     (subtypep* class (find-class 'standard-object))
     (subtypep* class (find-class t))))
  t t t t t t t t)

(deftest class-27.2
  (let ((c (make-instance 'class-27)))
    (values
     (slot-boundp* c 'a)
     (slot-value c 'b)
     (slot-boundp* c 'c)
     (slot-value c 'd)))
  nil 10 nil 17)

(deftest class-27.3
  (let ((c (make-instance 'class-27 :c 26 :d 43)))
    (values
     (slot-boundp* c 'a)
     (slot-value c 'b)
     (slot-value c 'c)
     (slot-value c 'd)))
  nil 10 26 43)

;;;

(declaim (special *class-28-reset-fn*
		  *class-28-query-fn*))

(declaim (type function *class-28-reset-fn* *class-28-query-fn*))

(let ((x 0) (y 0))
  (flet ((%reset (a b) (setf x a y b))
	 (%query () (list x y)))
    (setf *class-28-reset-fn* #'%reset
	  *class-28-query-fn* #'%query)
    (defclass class-28 ()
      ((s1 :initform (incf x) :initarg :s1)
       (s2 :initarg :s2))
      (:default-initargs :s2 (incf y)))))

(deftest class-28.1
  (let ((class (find-class 'class-28)))
    (funcall *class-28-reset-fn* 5 10)
    (list
     (funcall *class-28-query-fn*)
     (let ((obj (make-instance 'class-28)))
       (list
	(typep* obj 'class-28)
	(typep* obj class)
	(eqt (class-of obj) class)
	(map-slot-value obj '(s1 s2))
	(funcall *class-28-query-fn*)))))
  ((5 10)
   (t t t (6 11) (6 11))))

(deftest class-28.2
  (let ((class (find-class 'class-28)))
    (funcall *class-28-reset-fn* 5 10)
    (list
     (funcall *class-28-query-fn*)
     (let ((obj (make-instance 'class-28 :s1 17)))
       (list
	(typep* obj 'class-28)
	(typep* obj class)
	(eqt (class-of obj) class)
	(map-slot-value obj '(s1 s2))
	(funcall *class-28-query-fn*)))))
  ((5 10)
   (t t t (17 11) (5 11))))


(deftest class-28.3
  (let ((class (find-class 'class-28)))
    (funcall *class-28-reset-fn* 5 10)
    (list
     (funcall *class-28-query-fn*)
     (let ((obj (make-instance 'class-28 :s2 17)))
       (list
	(typep* obj 'class-28)
	(typep* obj class)
	(eqt (class-of obj) class)
	(map-slot-value obj '(s1 s2))
	(funcall *class-28-query-fn*)))))
  ((5 10)
   (t t t (6 17) (6 10))))


      
      