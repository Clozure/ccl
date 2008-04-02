;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar  8 22:38:53 2003
;;;; Contains: Tests of DEFINE-CONDITION (part 1)

(in-package :cl-test)

;;;

(define-condition-with-tests condition-1 nil nil)

(define-condition-with-tests condition-2 (condition) nil)

#-gcl (define-condition-with-tests #:condition-3 nil nil)

(define-condition-with-tests condition-4 nil
  ((slot1 :initarg :slot1 :reader condition-4/slot-1)
   (slot2 :initarg :slot2 :reader condition-4/slot-2)))

(deftest condition-4-slots.1
  (let ((c (make-condition 'condition-4 :slot1 'a :slot2 'b)))
    (and (typep c 'condition-4)
	 (eqlt (condition-4/slot-1 c) 'a)
	 (eqlt (condition-4/slot-2 c) 'b)))
  t)

(define-condition-with-tests condition-5 nil
  ((slot1 :initarg :slot1 :initform 'x :reader condition-5/slot-1)
   (slot2 :initarg :slot2 :initform 'y :reader condition-5/slot-2)))

(deftest condition-5-slots.1
  (let ((c (make-condition 'condition-5 :slot1 'a :slot2 'b)))
    (and (typep c 'condition-5)
	 (eqlt (condition-5/slot-1 c) 'a)
	 (eqlt (condition-5/slot-2 c) 'b)))
  t)

(deftest condition-5-slots.2
  (let ((c (make-condition 'condition-5 :slot1 'a)))
    (and (typep c 'condition-5)
	 (eqlt (condition-5/slot-1 c) 'a)
	 (eqlt (condition-5/slot-2 c) 'y)))
  t)

(deftest condition-5-slots.3
  (let ((c (make-condition 'condition-5 :slot2 'b)))
    (and (typep c 'condition-5)
	 (eqlt (condition-5/slot-1 c) 'x)
	 (eqlt (condition-5/slot-2 c) 'b)))
  t)

(deftest condition-5-slots.4
  (let ((c (make-condition 'condition-5)))
    (and (typep c 'condition-5)
	 (eqlt (condition-5/slot-1 c) 'x)
	 (eqlt (condition-5/slot-2 c) 'y)))
  t)

(define-condition-with-tests condition-6 nil
  ((slot1 :initarg :slot1 :initarg :both-slots
	  :initform 'x :reader condition-6/slot-1)
   (slot2 :initarg :slot2 :initarg :both-slots
	  :initform 'y :reader condition-6/slot-2)))

(deftest condition-6-slots.1
  (let ((c (make-condition 'condition-6 :both-slots 'a)))
    (and (typep c 'condition-6)
	 (eqlt (condition-6/slot-1 c) 'a)
	 (eqlt (condition-6/slot-2 c) 'a)))
  t)

(deftest condition-6-slots.2
  (let ((c (make-condition 'condition-6)))
    (and (typep c 'condition-6)
	 (eqlt (condition-6/slot-1 c) 'x)
	 (eqlt (condition-6/slot-2 c) 'y)))
  t)

(deftest condition-6-slots.3
  (let ((c (make-condition 'condition-6 :slot1 'a :both-slots 'b)))
    (and (typep c 'condition-6)
	 (eqlt (condition-6/slot-1 c) 'a)
	 (eqlt (condition-6/slot-2 c) 'b)))
  t)

(deftest condition-6-slots.4
  (let ((c (make-condition 'condition-6 :slot2 'b :both-slots 'a)))
    (and (typep c 'condition-6)
	 (eqlt (condition-6/slot-1 c) 'a)
	 (eqlt (condition-6/slot-2 c) 'b)))
  t)

(deftest condition-6-slots.5
  (let ((c (make-condition 'condition-6 :both-slots 'a :slot1 'c :slot2 'd)))
    (and (typep c 'condition-6)
	 (eqlt (condition-6/slot-1 c) 'a)
	 (eqlt (condition-6/slot-2 c) 'a)))
  t)

(define-condition-with-tests condition-7 nil
  ((s :initarg :i1 :initarg :i2 :reader condition-7/s)))

(deftest condition-7-slots.1
  (let ((c (make-condition 'condition-7 :i1 'a)))
    (and (typep c 'condition-7)
	 (eqlt (condition-7/s c) 'a)))
  t)

(deftest condition-7-slots.2
  (let ((c (make-condition 'condition-7 :i2 'a)))
    (and (typep c 'condition-7)
	 (eqlt (condition-7/s c) 'a)))
  t)

(deftest condition-7-slots.3
  (let ((c (make-condition 'condition-7 :i1 'a :i2 'b)))
    (and (typep c 'condition-7)
	 (eqlt (condition-7/s c) 'a)))
  t)

(deftest condition-7-slots.4
  (let ((c (make-condition 'condition-7 :i2 'a :i1 'b)))
    (and (typep c 'condition-7)
	 (eqlt (condition-7/s c) 'a)))
  t)

(defparameter *condition-8-counter* 0)

(define-condition-with-tests condition-8 nil
  ((s :initarg :i1 :initform (incf *condition-8-counter*) :reader condition-8/s)))

(deftest condition-8-slots.1
  (let ((*condition-8-counter* 100))
    (declare (special *condition-8-counter*))
    (values
     (condition-8/s (make-condition 'condition-8))
     *condition-8-counter*))
  101 101)

(define-condition-with-tests condition-9 nil
  ((s1 :initarg :i1 :initform 15 :reader condition-9/s1)
   (s2 :initarg :i2 :initform 37 :reader condition-9/s2)))

(deftest condition-9-slots.1
  (let ((c (make-condition 'condition-9)))
    (values (notnot (typep c 'condition-9))
	    (condition-9/s1 c)
	    (condition-9/s2 c)))
  t 15 37)

(deftest condition-9-slots.2
  (let ((c (make-condition 'condition-9 :i1 3)))
    (values (notnot (typep c 'condition-9))
	    (condition-9/s1 c)
	    (condition-9/s2 c)))
  t 3 37)

(deftest condition-9-slots.3
  (let ((c (make-condition 'condition-9 :i2 3)))
    (values (notnot (typep c 'condition-9))
	    (condition-9/s1 c)
	    (condition-9/s2 c)))
  t 15 3)

(deftest condition-9-slots.4
  (let ((c (make-condition 'condition-9 :i2 3 :i2 8)))
    (values (notnot (typep c 'condition-9))
	    (condition-9/s1 c)
	    (condition-9/s2 c)))
  t 15 3)

(deftest condition-9-slots.5
  (let ((c (make-condition 'condition-9 :i1 3 :i2 8)))
    (values (notnot (typep c 'condition-9))
	    (condition-9/s1 c)
	    (condition-9/s2 c)))
  t 3 8)

(deftest condition-9-slots.6
  (let ((c (make-condition 'condition-9 :i1 3 :i2 8 :i1 100 :i2 500)))
    (values (notnot (typep c 'condition-9))
	    (condition-9/s1 c)
	    (condition-9/s2 c)))
  t 3 8)

;;; (define-condition-with-tests condition-10 nil
;;;   ((s1 :initarg :i1 :writer condition-10/s1-w :reader condition-10/s1-r)))
;;; 
;;; (deftest condition-10-slots.1
;;;   (let ((c (make-condition 'condition-10 :i1 11)))
;;;      (condition-10/s1-r c))
;;;   11)
;;; 
;;; (deftest condition-10-slots.2
;;;   (let ((c (make-condition 'condition-10 :i1 11)))
;;;      (condition-10/s1-w 17 c))
;;;   17)
;;; 
;;; (deftest condition-10-slots.3
;;;   (let ((c (make-condition 'condition-10 :i1 11)))
;;;      (condition-10/s1-w 107 c)
;;;      (condition-10/s1-r c))
;;;   107)
;;; 
;;; (define-condition-with-tests condition-11 nil
;;;   ((s1 :initarg :i1 :writer (setf condition-11/w) :reader condition-11/r)))
;;; 
;;; (deftest condition-11-slots.1
;;;   (let ((c (make-condition 'condition-11 :i1 11)))
;;;      (condition-11/r c))
;;;   11)
;;; 
;;; (deftest condition-11-slots.2
;;;   (let ((c (make-condition 'condition-11 :i1 11)))
;;;      (setf (condition-11/w c) 17))
;;;   17)
;;; 
;;; (deftest condition-11-slots.3
;;;   (let ((c (make-condition 'condition-11 :i1 11)))
;;;      (setf (condition-11/w c) 117)
;;;      (condition-11/r c))
;;;   117)
;;; 
;;; (deftest condition-11-slots.4
;;;   (let ((c (make-condition 'condition-11 :i1 11)))
;;;     (values
;;;      (funcall #'(setf condition-11/w) 117 c)
;;;      (condition-11/r c)))
;;;   117 117)

;;; The condition-12 and condition-13 tests have been removed.  Duane Rettig
;;; convincingly argued that the feature being tested (non-symbol
;;; slot names) remains in the standard only because of editing errors.

;;; (define-condition-with-tests condition-12 nil
;;;   (((slot1) :initarg :slot1 :reader condition-12/slot-1)
;;;    ((slot2) :initarg :slot2 :reader condition-12/slot-2)))
;;; 
;;; (deftest condition-12-slots.1
;;;   (let ((c (make-condition 'condition-12 :slot1 'a :slot2 'b)))
;;;     (and (typep c 'condition-12)
;;; 	 (eqlt (condition-12/slot-1 c) 'a)
;;; 	 (eqlt (condition-12/slot-2 c) 'b)))
;;;   t)
;;; 
;;; (define-condition-with-tests condition-13 nil
;;;   (((slot1 10) :initarg :slot1 :reader condition-13/slot-1)))
;;; 
;;; (deftest condition-13-slots.1
;;;   (let ((c (make-condition 'condition-13)))
;;;     (and (typep c 'condition-13)
;;; 	 (condition-13/slot-1 c)))
;;;   10)
 
(define-condition-with-tests condition-14 nil
  ((s1 :initarg :i1 :type fixnum :reader condition-14/s1)
   (s2 :initarg :i2 :type t :reader condition-14/s2)))

(deftest condition-14-slots.1
  (let ((c (make-condition 'condition-14 :i1 10)))
    (and (typep c 'condition-14)
	 (condition-14/s1 c)))
  10)

(deftest condition-14-slots.2
  (let ((c (make-condition 'condition-14 :i2 'a)))
    (and (typep c 'condition-14)
	 (condition-14/s2 c)))
  a)

(deftest condition-14-slots.3
  (let ((c (make-condition 'condition-14 :i1 10 :i2 'h)))
    (and (typep c 'condition-14)
	 (eqlt (condition-14/s1 c) 10)
	 (condition-14/s2 c)))
  h)

(define-condition-with-tests condition-15 nil
  ((s1 :type nil)))

(define-condition-with-tests condition-16 nil
  ((slot1))
  (:report "The report for condition-16"))

(deftest condition-16-report.1
  (let ((*print-escape* nil)
	(c (make-condition 'condition-16)))
    (with-output-to-string (s) (print-object c s)))
  "The report for condition-16")

(defun condition-17-report (c s)
  (format s "condition-17: ~A" (condition-17/s c)))

(define-condition-with-tests condition-17 nil
  ((s :initarg :i1 :reader condition-17/s ))
  (:report condition-17-report))

(deftest condition-17-report.1
  (let ((*print-escape* nil)
	(c (make-condition 'condition-17 :i1 1234)))
    (with-output-to-string (s) (print-object c s)))
  "condition-17: 1234")

(define-condition-with-tests condition-18 nil
  ((s :initarg :i1 :reader condition-18/s ))
  (:report (lambda (c s) (format s "condition-18: ~A" (condition-18/s c)))))

(deftest condition-18-report.1
  (let ((*print-escape* nil)
	(c (make-condition 'condition-18 :i1 4321)))
    (with-output-to-string (s) (print-object c s)))
  "condition-18: 4321")

;;;
;;; Tests of :default-initargs
;;;
;;; There is an inconsistency in the ANSI spec.  DEFINE-CONDITION
;;; says that in (:default-initargs . <foo>), <foo> is a list of pairs.
;;; However, DEFCLASS says it's a list whose alternate elements
;;; are initargs and initforms.  I have taken the second interpretation.
;;;

(define-condition-with-tests condition-19 nil
  ((s1 :reader condition-19/s1 :initarg :i1)
   (s2 :reader condition-19/s2 :initarg :i2))
  (:default-initargs :i1 10
		     :i2 20))

(deftest condition-19-slots.1
  (let ((c (make-condition 'condition-19)))
    (values
     (notnot (typep c 'condition-19))
     (condition-19/s1 c)
     (condition-19/s2 c)))
  t 10 20)

(deftest condition-19-slots.2
  (let ((c (make-condition 'condition-19 :i1 'a)))
    (values
     (notnot (typep c 'condition-19))
     (condition-19/s1 c)
     (condition-19/s2 c)))
  t a 20)

(deftest condition-19-slots.3
  (let ((c (make-condition 'condition-19 :i2 'a)))
    (values
     (notnot (typep c 'condition-19))
     (condition-19/s1 c)
     (condition-19/s2 c)))
  t 10 a)

(deftest condition-19-slots.4
  (let ((c (make-condition 'condition-19 :i1 'x :i2 'y)))
    (values
     (notnot (typep c 'condition-19))
     (condition-19/s1 c)
     (condition-19/s2 c)))
  t x y)

(deftest condition-19-slots.5
  (let ((c (make-condition 'condition-19 :i2 'y :i1 'x)))
    (values
     (notnot (typep c 'condition-19))
     (condition-19/s1 c)
     (condition-19/s2 c)))
  t x y)

(defparameter *condition-20/s1-val* 0)
(defparameter *condition-20/s2-val* 0)

(define-condition-with-tests condition-20 nil
  ((s1 :reader condition-20/s1 :initarg :i1)
   (s2 :reader condition-20/s2 :initarg :i2))
  (:default-initargs :i1 (incf *condition-20/s1-val*)
		     :i2 (incf *condition-20/s2-val*)))

(deftest condition-20-slots.1
  (let ((*condition-20/s1-val* 0)
	(*condition-20/s2-val* 10))
    (declare (special *condition-20/s1-val* *condition-20/s2-val*))
    (let ((c (make-condition 'condition-20)))
      (values
       (notnot (typep c 'condition-20))
       (condition-20/s1 c)
       (condition-20/s2 c)
       *condition-20/s1-val*
       *condition-20/s2-val*)))
  t 1 11 1 11)

(deftest condition-20-slots.2
  (let ((*condition-20/s1-val* 0)
	(*condition-20/s2-val* 10))
    (declare (special *condition-20/s1-val* *condition-20/s2-val*))
    (let ((c (make-condition 'condition-20 :i1 'x)))
      (values
       (notnot (typep c 'condition-20))
       (condition-20/s1 c)
       (condition-20/s2 c)
       *condition-20/s1-val*
       *condition-20/s2-val*)))
  t x 11 0 11)

(deftest condition-20-slots.3
  (let ((*condition-20/s1-val* 0)
	(*condition-20/s2-val* 10))
    (declare (special *condition-20/s1-val* *condition-20/s2-val*))
    (let ((c (make-condition 'condition-20 :i2 'y)))
      (values
       (notnot (typep c 'condition-20))
       (condition-20/s1 c)
       (condition-20/s2 c)
       *condition-20/s1-val*
       *condition-20/s2-val*)))
  t 1 y 1 10)

(deftest condition-20-slots.4
  (let ((*condition-20/s1-val* 0)
	(*condition-20/s2-val* 10))
    (declare (special *condition-20/s1-val* *condition-20/s2-val*))
    (let ((c (make-condition 'condition-20 :i2 'y :i1 'x)))
      (values
       (notnot (typep c 'condition-20))
       (condition-20/s1 c)
       (condition-20/s2 c)
       *condition-20/s1-val*
       *condition-20/s2-val*)))
  t x y 0 10)


;;;;;;;;; tests of inheritance

(define-condition-with-tests condition-21 (condition-4) nil)

(deftest condition-21-slots.1
  (let ((c (make-condition 'condition-21 :slot1 'a :slot2 'b)))
    (and (typep c 'condition-4)
	 (typep c 'condition-21)
	 (eqlt (condition-4/slot-1 c) 'a)
	 (eqlt (condition-4/slot-2 c) 'b)))
  t)

(define-condition-with-tests condition-22 (condition-4)
  ((slot3 :initarg :slot3 :reader condition-22/slot-3)
   (slot4 :initarg :slot4 :reader condition-22/slot-4)))

(deftest condition-22-slots.1
  (let ((c (make-condition 'condition-22 :slot1 'a :slot2 'b
			   :slot3 'c :slot4 'd)))
    (and (typep c 'condition-4)
	 (typep c 'condition-22)
	 (eqlt (condition-4/slot-1 c) 'a)
	 (eqlt (condition-4/slot-2 c) 'b)
	 (eqlt (condition-22/slot-3 c) 'c)
	 (eqlt (condition-22/slot-4 c) 'd)
	 ))
  t)

(define-condition-with-tests condition-23 (condition-5) nil)

(deftest condition-23-slots.1
  (let ((c (make-condition 'condition-23 :slot1 'a :slot2 'b)))
    (and (typep c 'condition-5)
	 (typep c 'condition-23)
	 (eqlt (condition-5/slot-1 c) 'a)
	 (eqlt (condition-5/slot-2 c) 'b)
	 ))
  t)

(deftest condition-23-slots.2
  (let ((c (make-condition 'condition-23 :slot1 'a)))
    (and (typep c 'condition-5)
	 (typep c 'condition-23)
	 (eqlt (condition-5/slot-1 c) 'a)
	 (eqlt (condition-5/slot-2 c) 'y)
	 ))
  t)

(deftest condition-23-slots.3
  (let ((c (make-condition 'condition-23 :slot2 'b)))
    (and (typep c 'condition-5)
	 (typep c 'condition-23)
	 (eqlt (condition-5/slot-1 c) 'x)
	 (eqlt (condition-5/slot-2 c) 'b)
	 ))
  t)

(deftest condition-23-slots.4
  (let ((c (make-condition 'condition-23)))
    (and (typep c 'condition-5)
	 (typep c 'condition-23)
	 (eqlt (condition-5/slot-1 c) 'x)
	 (eqlt (condition-5/slot-2 c) 'y)
	 ))
  t)

(define-condition-with-tests condition-24 (condition-5)
  nil
  (:default-initargs :slot1 'z))

(deftest condition-24-slots.1
  (let ((c (make-condition 'condition-24)))
    (and (typep c 'condition-5)
	 (typep c 'condition-24)
	 (eqlt (condition-5/slot-1 c) 'z)
	 (eqlt (condition-5/slot-2 c) 'y)
	 ))
  t)

(deftest condition-24-slots.2
  (let ((c (make-condition 'condition-24 :slot1 'a)))
    (and (typep c 'condition-5)
	 (typep c 'condition-24)
	 (eqlt (condition-5/slot-1 c) 'a)
	 (eqlt (condition-5/slot-2 c) 'y)
	 ))
  t)

(deftest condition-24-slots.3
  (let ((c (make-condition 'condition-24 :slot2 'a)))
    (and (typep c 'condition-5)
	 (typep c 'condition-24)
	 (eqlt (condition-5/slot-1 c) 'z)
	 (eqlt (condition-5/slot-2 c) 'a)
	 ))
  t)

(deftest condition-24-slots.4
  (let ((c (make-condition 'condition-24 :slot1 'b :slot2 'a)))
    (and (typep c 'condition-5)
	 (typep c 'condition-24)
	 (eqlt (condition-5/slot-1 c) 'b)
	 (eqlt (condition-5/slot-2 c) 'a)
	 ))
  t)

;;; Multiple inheritance

(define-condition-with-tests condition-25a nil
  ((s1 :initarg :s1 :initform 'a :reader condition-25a/s1)))

(define-condition-with-tests condition-25b nil
  ((s2 :initarg :s2 :initform 'b :reader condition-25b/s2)))

(define-condition-with-tests condition-25 (condition-25a condition-25b)
  ((s3 :initarg :s3 :initform 'c :reader condition-25/s3)))

(deftest condition-25-slots.1
  (let ((c (make-condition 'condition-25)))
    (and (typep c 'condition-25a)
	 (typep c 'condition-25b)
	 (typep c 'condition-25)
	 (eqlt (condition-25a/s1 c) 'a)
	 (eqlt (condition-25b/s2 c) 'b)
	 (eqlt (condition-25/s3 c) 'c)))
  t)

(deftest condition-25-slots.2
  (let ((c (make-condition 'condition-25 :s1 'x)))
    (and (typep c 'condition-25a)
	 (typep c 'condition-25b)
	 (typep c 'condition-25)
	 (eqlt (condition-25a/s1 c) 'x)
	 (eqlt (condition-25b/s2 c) 'b)
	 (eqlt (condition-25/s3 c) 'c)))
  t)

(deftest condition-25-slots.3
  (let ((c (make-condition 'condition-25 :s2 'x)))
    (and (typep c 'condition-25a)
	 (typep c 'condition-25b)
	 (typep c 'condition-25)
	 (eqlt (condition-25a/s1 c) 'a)
	 (eqlt (condition-25b/s2 c) 'x)
	 (eqlt (condition-25/s3 c) 'c)))
  t)

(deftest condition-25-slots.4
  (let ((c (make-condition 'condition-25 :s3 'x)))
    (and (typep c 'condition-25a)
	 (typep c 'condition-25b)
	 (typep c 'condition-25)
	 (eqlt (condition-25a/s1 c) 'a)
	 (eqlt (condition-25b/s2 c) 'b)
	 (eqlt (condition-25/s3 c) 'x)))
  t)

(deftest condition-25-slots.5
  (let ((c (make-condition 'condition-25 :s3 'z :s2 'y :s1 'x)))
    (and (typep c 'condition-25a)
	 (typep c 'condition-25b)
	 (typep c 'condition-25)
	 (eqlt (condition-25a/s1 c) 'x)
	 (eqlt (condition-25b/s2 c) 'y)
	 (eqlt (condition-25/s3 c) 'z)))
  t)

;;;

(define-condition-with-tests condition-26a nil
  ((s1 :initarg :s1 :initform 'a :reader condition-26a/s1)))

(define-condition-with-tests condition-26b (condition-26a) nil)
(define-condition-with-tests condition-26c (condition-26a) nil)
(define-condition-with-tests condition-26 (condition-26b condition-26c) nil)

(deftest condition-26-slots.1
  (let ((c (make-condition 'condition-26)))
    (and (typep c 'condition-26a)
	 (typep c 'condition-26b)
	 (typep c 'condition-26c)
	 (typep c 'condition-26)
	 (eqlt (condition-26a/s1 c) 'a)))
  t)

(deftest condition-26-slots.2
  (let ((c (make-condition 'condition-26 :s1 'x)))
    (and (typep c 'condition-26a)
	 (typep c 'condition-26b)
	 (typep c 'condition-26c)
	 (typep c 'condition-26)
	 (eqlt (condition-26a/s1 c) 'x)))
  t)


;;; Test that a slot reader is truly a generic function

(define-condition-with-tests condition-27a nil
  ((s0 :initarg :s0 :initform 10 :reader condition-27a/s0)
   (s1 :initarg :s1 :initform 'a :reader condition-27/s1)))

(define-condition-with-tests condition-27b nil
  ((s1 :initarg :s1 :initform 'a :reader condition-27/s1)
   (s2 :initarg :s2 :initform 16 :reader condition-27b/s2)))

(deftest condition-27-slots.1
  (let ((c (make-condition 'condition-27a)))
    (and (typep c 'condition-27a)
	 (not (typep c 'condition-27b))
	 (eqlt (condition-27/s1 c) 'a)))
  t)

(deftest condition-27-slots.2
  (let ((c (make-condition 'condition-27b)))
    (and (typep c 'condition-27b)
	 (not (typep c 'condition-27a))
	 (eqlt (condition-27/s1 c) 'a)))
  t)

(deftest condition-27-reader-is-generic
  (notnot-mv (typep #'condition-27/s1 'generic-function))
  t)

;;; More inheritance

;;; These test that condition slots are inherited like CLOS
;;; slots.  It's not entirely clear to me if the standard
;;; demands this (one of the issues does, but that issue wasn't
;;; fully integrated into the standard.)

#|
(define-condition-with-tests condition-28a nil
  ((s1 :initarg :i1 :initform 'x :reader condition-28a/s1)))

(define-condition-with-tests condition-28 (condition-28a)
  ((s1 :initarg :i1a :reader condition-28/s1)))

(deftest condition-28-slots.1
  (let ((c (make-condition 'condition-28)))
    (and (typep c 'condition-28a)
	 (typep c 'condition-28)
	 (eqlt (condition-28a/s1 c) 'x)
	 (eqlt (condition-28/s1 c) 'x)))
  t)

(deftest condition-28-slots.2
  (let ((c (make-condition 'condition-28 :i1 'z)))
    (and (typep c 'condition-28a)
	 (typep c 'condition-28)
	 (eqlt (condition-28a/s1 c) 'z)
	 (eqlt (condition-28/s1 c) 'z)))
  t)

(deftest condition-28-slots.3
  (let ((c (make-condition 'condition-28 :i1a 'w)))
    (and (typep c 'condition-28a)
	 (typep c 'condition-28)
	 (eqlt (condition-28a/s1 c) 'w)
	 (eqlt (condition-28/s1 c) 'w)))
  t)

(deftest condition-28-slots.4
  (let ((c (make-condition 'condition-28 :i1 'y :i1a 'w)))
    (and (typep c 'condition-28a)
	 (typep c 'condition-28)
	 (eqlt (condition-28a/s1 c) 'y)
	 (eqlt (condition-28/s1 c) 'y)))
  t)

(deftest condition-28-slots.5
  (let ((c (make-condition 'condition-28 :i1a 'y :i1 'w)))
    (and (typep c 'condition-28a)
	 (typep c 'condition-28)
	 (eqlt (condition-28a/s1 c) 'y)
	 (eqlt (condition-28/s1 c) 'y)))
  t)
|#


;;; Documentation

;;; Pitman says this should have been in the spec, but it isn't really
;;; (define-condition-with-tests condition-29 nil
;;;  ((s1 :initarg :i1 :initform 'x
;;;       :documentation "This is slot s1 in condition condition-29")))

(define-condition-with-tests condition-30 nil
  ((s1 :initarg :i1 :initform 'x))
  (:documentation "This is class condition-30"))
