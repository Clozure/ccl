;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 17 11:54:54 2003
;;;; Contains: Tests of MAKE-LOAD-FORM-SAVING-SLOTS

(in-package :cl-test)

;;; These are tests of MAKE-LOAD-FORM-SAVING-SLOTS proper; tests involving
;;; file compilation will be located elsewhere.


(defstruct mlfss-01 a b c)

(deftest make-load-form-saving-slots.1
  (let* ((obj (make-mlfss-01))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots obj))))
    (values
     (length forms)
     (let ((newobj (eval (first forms))))
       (eval (subst newobj obj (second forms)))
       (eqt (class-of obj) (class-of newobj)))))
  2 t)

(deftest make-load-form-saving-slots.2
  (let* ((obj (make-mlfss-01))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots obj :slot-names '(a b)))))
    (values
     (length forms)
     (let ((newobj (eval (first forms))))
       (eval (subst newobj obj (second forms)))
       (eqt (class-of obj) (class-of newobj)))))
  2 t)

(defclass mlfss-02 () ((a :initarg :a) (b :initarg :b) (c :initarg :c)))

(deftest make-load-form-saving-slots.3
  (let* ((obj (make-instance 'mlfss-02))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots obj))))
     (let ((newobj (eval (first forms))))
       (eval (subst newobj obj (second forms)))
       (values
	(length forms)
	(eqt (class-of obj) (class-of newobj))
	(map-slot-boundp* newobj '(a b c)))))
  2 t (nil nil nil))

(deftest make-load-form-saving-slots.4
  (let* ((obj (make-instance 'mlfss-02 :a 1 :b 'a :c '(x y z)))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots obj :slot-names '(a b c)))))
     (let ((newobj (eval (first forms))))
       (eval (subst newobj obj (second forms)))
       (values
	(length forms)
	(eqt (class-of obj) (class-of newobj))
	(map-slot-boundp* newobj '(a b c))
	(map-slot-value newobj '(a b c)))))
  2 t (t t t) (1 a (x y z)))


(deftest make-load-form-saving-slots.5
  (let* ((obj (make-instance 'mlfss-02 :a #(x y z)))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots obj :slot-names '(a b)))))
     (let ((newobj (eval (first forms))))
       (eval (subst newobj obj (second forms)))
       (values
	(length forms)
	(eqt (class-of obj) (class-of newobj))
	(map-slot-boundp* newobj '(a b c))
	(slot-value newobj 'a))))
  2 t (t nil nil) #(x y z))

(deftest make-load-form-saving-slots.6
  (let* ((obj (make-instance 'mlfss-02))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots obj :allow-other-keys nil))))
     (let ((newobj (eval (first forms))))
       (eval (subst newobj obj (second forms)))
       (values
	(length forms)
	(eqt (class-of obj) (class-of newobj))
	(map-slot-boundp* newobj '(a b c)))))
  2 t (nil nil nil))

;;; If :slot-names is missing, all initialized slots are retained
(deftest make-load-form-saving-slots.7
  (let* ((obj (make-instance 'mlfss-02 :a (list 'x) :c 6/5))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots obj))))
     (let ((newobj (eval (first forms))))
       (eval (subst newobj obj (second forms)))
       (values
	(length forms)
	(eqt (class-of obj) (class-of newobj))
	(map-slot-boundp* newobj '(a b c))
	(map-slot-value newobj '(a c)))))
  2 t (t nil t) ((x) 6/5))

;;; If :slot-names is present, all initialized slots in the list are retained
(deftest make-load-form-saving-slots.8
  (let* ((obj (make-instance 'mlfss-02 :a (list 'x) :c 6/5))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots obj :slot-names '(c)))))
     (let ((newobj (eval (first forms))))
       (eval (subst newobj obj (second forms)))
       (values
	(length forms)
	(eqt (class-of obj) (class-of newobj))
	(map-slot-boundp* newobj '(a b c))
	(slot-value newobj 'c))))
  2 t (nil nil t) 6/5)

;; It takes an :environment parameter
(deftest make-load-form-saving-slots.9
  (let* ((obj (make-instance 'mlfss-02 :a 7 :c 64 :b 100))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots obj :environment nil))))
     (let ((newobj (eval (first forms))))
       (eval (subst newobj obj (second forms)))
       (values
	(length forms)
	(eqt (class-of obj) (class-of newobj))
	(map-slot-boundp* newobj '(a b c))
	(map-slot-value newobj '(a b c)))))
  2 t (t t t) (7 100 64))

(defpackage "CL-TEST-MLFSS-PACKAGE" (:use) (:export #:a))
(defstruct mlfss-03 cl-test-mlfss-package:a)

(deftest make-load-form-savings-slots.10
  (let* ((obj (make-mlfss-03 :a 17))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots obj))))
    (let ((newobj (eval (first forms))))
      (eval (subst newobj obj (second forms)))
      (values
       (mlfss-03-a obj)
       (length forms)
       (eqt (class-of obj) (class-of newobj))
       (mlfss-03-a newobj))))
  17 2 t 17)

(deftest make-load-form-savings-slots.11
  (let* ((obj (make-mlfss-03 :a 17))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots
		  obj
		  :slot-names '(cl-test-mlfss-package:a)))))
    (let ((newobj (eval (first forms))))
      (eval (subst newobj obj (second forms)))
      (values
       (mlfss-03-a obj)
       (length forms)
       (eqt (class-of obj) (class-of newobj))
       (mlfss-03-a newobj))))
  17 2 t 17)


(defstruct mlfss-04 (a 0 :read-only t))

(deftest make-load-form-savings-slots.12
  (let* ((obj (make-mlfss-04 :a 123))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots obj))))
    (let ((newobj (eval (first forms))))
      (eval (subst newobj obj (second forms)))
      (values
       (mlfss-04-a obj)
       (length forms)
       (eqt (class-of obj) (class-of newobj))
       (mlfss-04-a newobj))))
  123 2 t 123)


;;; General error tests

(deftest make-load-form-saving-slots.error.1
  (signals-error (make-load-form-saving-slots) program-error)
  t)

(deftest make-load-form-saving-slots.error.2
  (signals-error (make-load-form-saving-slots (make-instance 'mlfss-02)
					       :slot-names)
		 program-error)
  t)

(deftest make-load-form-saving-slots.error.3
  (signals-error (make-load-form-saving-slots (make-instance 'mlfss-02)
					       (gensym) t)
		 program-error)
  t)
