;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jun  9 07:02:53 2005
;;;; Contains: Separate tests for DEFMETHOD

(in-package :cl-test)

(deftest defmethod.1
  (let ((sym (gensym)))
    (values
     (typep* (eval `(defmethod ,sym (x) (list x))) 'standard-method)
     (typep* (fdefinition sym) 'standard-generic-function)
     (funcall sym 1)))
  t t (1))

(deftest defmethod.2
  (let* ((sym (gensym))
	 (method
	  (eval `(defmethod ,sym ((x integer)) (list x)))))
    (values
     (typep* method 'standard-method)
     (typep* (fdefinition sym) 'standard-generic-function)
     (funcall sym 1)))
  t t (1))

(deftest defmethod.3
  (let* ((sym (gensym))
	 (method
	  (eval `(let ((x 0)) (defmethod ,sym ((x (eql (incf x)))) (list x))))))
    (values
     (typep* method 'standard-method)
     (typep* (fdefinition sym) 'standard-generic-function)
     (funcall sym 1)
     (funcall sym 1)))
  t t (1) (1))

(deftest defmethod.4
  (let* ((sym (gensym))
	 (method
	  (eval `(defmethod (setf ,sym) ((x t) (y cons)) (setf (car y) x)))))
    (values
     (typep* method 'standard-method)
     (fboundp sym)
     (typep* (fdefinition `(setf ,sym)) 'standard-generic-function)
     (let ((x (cons 1 2))) (list (funcall (fdefinition `(setf ,sym)) 3 x) x))))
  t nil t (3 (3 . 2)))

(deftest defmethod.5
  (let* ((sym (gensym))
	 (method
	  (eval `(defmethod ,sym ((x integer)) (return-from ,sym (list x))))))
    (values
     (typep* method 'standard-method)
     (typep* (fdefinition sym) 'standard-generic-function)
     (funcall sym 1)))
  t t (1))

(deftest defmethod.6
  (let* ((sym (gensym))
	 (method
	  (eval `(defmethod (setf ,sym) ((x t) (y cons)) (return-from ,sym (setf (car y) x))))))
    (values
     (typep* method 'standard-method)
     (fboundp sym)
     (typep* (fdefinition `(setf ,sym)) 'standard-generic-function)
     (let ((x (cons 1 2))) (list (funcall (fdefinition `(setf ,sym)) 3 x) x))))
  t nil t (3 (3 . 2)))

(deftest defmethod.7
  (let* ((sym (gensym))
	 (method
	  (eval `(defmethod ,sym ((x integer) &aux (y (list x))) y))))
    (values
     (typep* method 'standard-method)
     (typep* (fdefinition sym) 'standard-generic-function)
     (funcall sym 1)))
  t t (1))

(deftest defmethod.8
  (let* ((sym (gensym))
	 (method (eval `(defmethod ,sym ((x integer) &key z) (list x z)))))
    (values
     (typep* method 'standard-method)
     (typep* (fdefinition sym) 'standard-generic-function)
     (funcall sym 1)
     (funcall sym 2 :z 3)
     (funcall sym 4 :allow-other-keys nil)
     (funcall sym 5 :allow-other-keys t :bogus 17)
     (funcall sym 6 :allow-other-keys t :allow-other-keys nil :bogus 17)
     ))
  t t (1 nil) (2 3) (4 nil) (5 nil) (6 nil))

(deftest defmethod.9
  (let* ((sym (gensym))
	 (method (eval `(defmethod ,sym ((x integer) &key (z :missing)) (list x z)))))
    (values
     (typep* method 'standard-method)
     (typep* (fdefinition sym) 'standard-generic-function)
     (funcall sym 1)
     (funcall sym 2 :z 3)
     (funcall sym 4 :allow-other-keys nil)
     ))
  t t (1 :missing) (2 3) (4 :missing))

(deftest defmethod.10
  (let* ((sym (gensym))
	 (method (eval `(defmethod ,sym ((x integer) &key (z :missing z-p)) (list x z (notnot z-p))))))
    (values
     (typep* method 'standard-method)
     (typep* (fdefinition sym) 'standard-generic-function)
     (funcall sym 1)
     (funcall sym 2 :z 3)
     (funcall sym 4 :allow-other-keys nil)
     ))
  t t (1 :missing nil) (2 3 t) (4 :missing nil))

(deftest defmethod.11
  (let* ((sym (gensym))
	 (method (eval `(defmethod ,sym ((x integer) &rest z) (list x z)))))
    (values
     (typep* method 'standard-method)
     (typep* (fdefinition sym) 'standard-generic-function)
     (funcall sym 1)
     (funcall sym 2 3)
     ))
  t t (1 nil) (2 (3)))

;;; Error cases

;;; Lambda liss not congruent

(deftest defmethod.error.1
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x y)))
    (eval `(signals-error (defmethod ,sym ((x t)) x) error)))
  t)

(deftest defmethod.error.2
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x y)))
    (eval `(signals-error (defmethod ,sym ((x t) (y t) (z t)) (list x y z)) error)))
  t)

(deftest defmethod.error.3
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x y &optional z)))
    (eval `(signals-error (defmethod ,sym ((x t) (y t) (z t)) (list x y z)) error)))
  t)

(deftest defmethod.error.4
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x y &optional z)))
    (eval `(signals-error (defmethod ,sym ((x t) (y t) &optional) (list x y)) error)))
  t)

(deftest defmethod.error.5
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x y &optional z)))
    (eval `(signals-error (defmethod ,sym ((x t) (y t) &optional z w) (list x y z w)) error)))
  t)

(deftest defmethod.error.6
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x &rest z)))
    (eval `(signals-error (defmethod ,sym ((x t)) (list x)) error)))
  t)

(deftest defmethod.error.7
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x)))
    (eval `(signals-error (defmethod ,sym ((x t) &rest z) (list x z)) error)))
  t)

(deftest defmethod.error.8
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x &key z)))
    (eval `(signals-error (defmethod ,sym ((x t)) (list x)) error)))
  t)

(deftest defmethod.error.9
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x)))
    (eval `(signals-error (defmethod ,sym ((x t) &key z) (list x z)) error)))
  t)

(deftest defmethod.error.10
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x &key z)))
    (eval `(signals-error (defmethod ,sym ((x t) &key) x) error)))
  t)

(deftest defmethod.error.11
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x &key)))
    (eval `(signals-error (defmethod ,sym ((x t)) x) error)))
  t)

(deftest defmethod.error.12
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x)))
    (eval `(signals-error (defmethod ,sym ((x t) &key) x) error)))
  t)

;;; Calling the implicitly defined generic function

(deftest defmethod.error.13
  (let ((sym (gensym)))
    (eval `(locally (declare (optimize safety)) (defmethod ,sym ((x t)) x)))
    (values (eval `(signals-error (,sym) program-error))
	    (eval `(signals-error (,sym 1 2) program-error))))
  t t)

(deftest defmethod.error.14
  (let ((sym (gensym)))
    (eval `(locally (declare (optimize safety)) (defmethod ,sym ((x t) &key) x)))
    (values (eval `(signals-error (,sym) program-error))
	    (eval `(signals-error (,sym 1 2) program-error))
	    (eval `(signals-error (,sym 1 :bogus t) program-error))
	    (eval `(signals-error (,sym 1 :allow-other-keys nil :allow-other-keys t :bogus t) program-error))))
  t t t t)

(deftest defmethod.error.15
  (let ((sym (gensym)))
    (eval `(locally (declare (optimize safety)) (defmethod ,sym ((x t) &key y) x)))
    (values (eval `(signals-error (,sym 1 :bogus t) program-error))
	    (eval `(signals-error (,sym 1 :y) program-error))
	    (eval `(signals-error (,sym 1 3 nil) program-error))))
  t t t)


