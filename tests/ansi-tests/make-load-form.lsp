;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 17 09:16:20 2003
;;;; Contains: Tests of MAKE-LOAD-FORM

(in-package :cl-test)

;;; These tests are just of MAKE-LOAD-FORM itself; tests of file compilation
;;; that depend on MAKE-LOAD-FORM will be found elsewhere.

(defclass make-load-form-class-01 () (a b c))

(deftest make-load-form.1
  (let* ((fun #'make-load-form)
	 (obj (make-instance 'make-load-form-class-01)))
    (if (eql (or (find-method fun nil '(standard-object) nil)
		 (find-method fun nil (list (find-class t)) nil)
		 :none)
	     (car (compute-applicable-methods fun (list obj))))
	;; The default method applies
	(handler-case
	 (progn (make-load-form obj) :bad)
	 (error () :good))
      :good))
  :good)

(defstruct make-load-form-struct-02 a b c)

(deftest make-load-form.2
  (let* ((fun #'make-load-form)
	 (obj (make-make-load-form-struct-02)))
    (if (eql (or (find-method fun nil '(structure-object) nil)
		 (find-method fun nil (list (find-class t)) nil)
		 :none)
	     (car (compute-applicable-methods fun (list obj))))
	;; The default method applies
	(handler-case
	 (progn (make-load-form obj) :bad)
	 (error () :good))
      :good))
  :good)

(define-condition make-load-form-condition-03 () ((a) (b) (c)))

(deftest make-load-form.3
  (let* ((fun #'make-load-form)
	 (obj (make-condition 'make-load-form-condition-03)))
    (if (eql (or (find-method fun nil '(condition) nil)
		 (find-method fun nil (list (find-class t)) nil)
		 :none)
	     (car (compute-applicable-methods fun (list obj))))
	;; The default method applies
	(handler-case
	 (progn (make-load-form obj :bad))
	 (error () :good))
      :good))
  :good)

;;; Make sure these errors are due to the method, not due to lack of
;;; methods

(deftest make-load-form.4
  (let* ((obj (make-instance 'make-load-form-class-01))
	 (fun #'make-load-form)
	 (methods (compute-applicable-methods fun (list obj))))
     (notnot-mv methods))
  t)

(deftest make-load-form.5
  (let* ((obj (make-make-load-form-struct-02))
	 (fun #'make-load-form)
	 (methods (compute-applicable-methods fun (list obj))))
    (notnot-mv methods))
  t)

(deftest make-load-form.6
  (let* ((obj (make-condition 'make-load-form-condition-03))
	 (fun #'make-load-form)
	 (methods (compute-applicable-methods fun (list obj))))
    (notnot-mv methods))
  t)

(deftest make-load-form.7
  (let* ((obj (make-instance 'make-load-form-class-01))
	 (fun #'make-load-form)
	 (methods (compute-applicable-methods fun (list obj nil))))
    (notnot-mv methods))
  t)

(deftest make-load-form.8
  (let* ((obj (make-make-load-form-struct-02))
	 (fun #'make-load-form)
	 (methods (compute-applicable-methods fun (list obj nil))))
    (notnot-mv methods))
  t)

(deftest make-load-form.9
  (let* ((obj (make-condition 'make-load-form-condition-03))
	 (fun #'make-load-form)
	 (methods (compute-applicable-methods fun (list obj nil))))
    (notnot-mv methods))
  t)
  
(deftest make-load-form.10
  (macrolet
      ((%m (&environment env)
	   (let* ((obj (make-instance 'make-load-form-class-01))
		  (fun #'make-load-form)
		  (methods (compute-applicable-methods fun (list obj env))))
	     (notnot-mv methods))))
    (%m))
  t)

(deftest make-load-form.11
  (macrolet
      ((%m (&environment env)
	   (let* ((obj (make-make-load-form-struct-02))
		  (fun #'make-load-form)
		  (methods (compute-applicable-methods fun (list obj env))))
	     (notnot-mv methods))))
    (%m))
  t)

(deftest make-load-form.12
  (macrolet
      ((%m (&environment env)
	   (let* ((obj (make-condition 'make-load-form-condition-03))
		  (fun #'make-load-form)
		  (methods (compute-applicable-methods fun (list obj env))))
	     (notnot-mv methods))))
    (%m))
  t)

;;; User-defined methods

(defclass make-load-form-class-04 ()
  ((a :initarg :a) (b :initarg :b) (c :initarg :c)))

(defmethod make-load-form ((obj make-load-form-class-04)
			   &optional (env t))
  (declare (ignore env))
  (let ((newobj (gensym)))
    `(let ((,newobj (allocate-instance (find-class 'make-load-form-class-04))))
       ,@(loop for slot-name in '(a b c)
	      when (slot-boundp obj slot-name)
	      collect `(setf (slot-value ,newobj ',slot-name)
			     ',(slot-value obj slot-name)))
       ,newobj)))

(deftest make-load-form.13
  (let* ((obj (make-instance 'make-load-form-class-04))
	 (obj2 (eval (make-load-form obj))))
    (values
     (eqt (class-of obj2) (class-of obj))
     (map-slot-boundp* obj2 '(a b c))))
  t (nil nil nil))

(deftest make-load-form.14
  (let* ((obj (make-instance 'make-load-form-class-04 :a 1 :b '(a b c) :c 'a))
	 (obj2 (eval (make-load-form obj))))
    (values
     (eqt (class-of obj2) (class-of obj))
     (map-slot-boundp* obj2 '(a b c))
     (map-slot-value obj2 '(a b c))))
  t
  (t t t)
  (1 (a b c) a))

(deftest make-load-form.15
  (let* ((obj (make-instance 'make-load-form-class-04 :b '(a b c) :c 'a))
	 (obj2 (eval (make-load-form obj nil))))
    (values
     (eqt (class-of obj2) (class-of obj))
     (map-slot-boundp* obj2 '(a b c))
     (map-slot-value obj2 '(b c))))
  t
  (nil t t)
  ((a b c) a))

#|
(defclass make-load-form-class-05a ()
  ((a :initarg :a)))

(defclass make-load-form-class-05b (make-load-form-class-05a)
  ((b :initarg :b)))

(defmethod make-load-form ((obj make-load-form-class-05a)
			   &optional (env t))
  (declare (ignore env))
  (let ((newobj (gensym)))
    `(let ((,newobj (allocate-instance (find-class 'make-load-form-class-04))))
       ,@(when (slot-boundp obj 'a)
	   `((setf (slot-value ,newobj 'a) ',(slot-value obj 'a))))
       ,newobj)))

(defmethod make-load-form :around ((obj make-load-form-class-05b)
				   &optional (env t))
  (declare (ignore env))
  (let ((newobj (gensym)))
    `(let ((,newobj (allocate-instance (find-class 'make-load-form-class-04))))
       ,@(when (slot-boundp obj 'a)
	   `((setf (slot-value ,newobj 'a) ',(slot-value obj 'a))))
       ,newobj)))
|#



;;; Other error tests

(deftest make-load-form.error.1
  (signals-error (make-load-form) program-error)
  t)

(deftest make-load-form.error.2
  (signals-error
   (let ((obj (make-instance 'make-load-form-class-04 :b '(a b c) :c 'a)))
     (make-load-form obj nil nil))
   program-error)
  t)
