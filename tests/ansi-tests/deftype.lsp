;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 12:56:56 2003
;;;; Contains: Tests of DEFTYPE

(in-package :cl-test)

(compile-and-load "types-aux.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; deftype

(deftest deftype.1
  (typep 1 '(even-array integer (10)))
  nil)

(deftest deftype.2
  (typep nil '(even-array t (*)))
  nil)

(deftest deftype.3
  (notnot-mv (typep (make-array '(10)) '(even-array t (*))))
  t)

(deftest deftype.4
  (typep (make-array '(5)) '(even-array t (*)))
  nil)

(deftest deftype.5
  (notnot-mv (typep (make-string 10) '(even-array character (*))))
  t)

(deftest deftype.6
  (notnot-mv
   (typep (make-array '(3 5 6) :element-type '(unsigned-byte 8))
	  '(even-array (unsigned-byte 8))))
  t)

(deftest deftype.7
  (let ((sym (gensym)))
    (assert (eq (eval `(deftype ,sym () '(integer 0 10))) sym))
    (documentation sym 'type))
  nil)

(deftest deftype.8
  (let ((sym (gensym)))
    (assert (eq (eval `(deftype ,sym () "FOO" '(integer 0 10))) sym))
    (or (documentation sym 'type) "FOO"))
  "FOO")

(deftest deftype.9
  (let* ((sym (gensym))
	 (form `(deftype ,sym (&optional x) `(integer 0 ,x))))
    (values
     (eqlt (eval form) sym)
     (multiple-value-list (subtypep* `(,sym) 'unsigned-byte))
     (multiple-value-list (subtypep* 'unsigned-byte `(,sym)))
     (multiple-value-list (subtypep* `(,sym 4) '(integer 0 4)))
     (multiple-value-list (subtypep* '(integer 0 4) `(,sym 4)))
     (loop for x in '(a -1 0 1 2 3 4 5 b)
	   collect (notnot (typep x sym)))
     (loop for x in '(a -1 0 1 2 3 4 5 b)
	   collect (notnot (typep x `(,sym 4))))
     ))
  t (t t) (t t) (t t) (t t)
  (nil nil t t t t t t nil)
  (nil nil t t t t t nil nil))

(deftest deftype.10
  (let* ((sym (gensym))
	 (form `(deftype ,sym (&optional (x 14)) `(integer 0 ,x))))
    (values
     (eqlt (eval form) sym)
     (multiple-value-list (subtypep* `(,sym) '(integer 0 14)))
     (multiple-value-list (subtypep* '(integer 0 14) `(,sym)))
     (multiple-value-list (subtypep* `(,sym 4) '(integer 0 4)))
     (multiple-value-list (subtypep* '(integer 0 4) `(,sym 4)))
     (loop for x in '(a -1 0 1 2 3 4 5 14 15 b)
	   collect (notnot (typep x sym)))
     (loop for x in '(a -1 0 1 2 3 4 5 14 15 b)
	   collect (notnot (typep x `(,sym 4))))
     ))
  t (t t) (t t) (t t) (t t)
  (nil nil t t t t t t t nil nil)
  (nil nil t t t t t nil nil nil nil))

(deftest deftype.11
  (let* ((sym (gensym))
	 (form `(deftype ,sym (&key foo bar) `(integer ,foo ,bar))))
    (values
     (eqlt (eval form) sym)
     (multiple-value-list (subtypep* `(,sym) 'integer))
     (multiple-value-list (subtypep* 'integer `(,sym)))

     (multiple-value-list (subtypep* `(,sym :allow-other-keys nil) 'integer))
     (multiple-value-list (subtypep* 'integer `(,sym :allow-other-keys nil)))
     (multiple-value-list (subtypep* `(,sym :xyz 17 :allow-other-keys t) 'integer))
     (multiple-value-list (subtypep* 'integer `(,sym :allow-other-keys t abc nil)))

     (multiple-value-list (subtypep* `(,sym :foo 3) '(integer 3)))
     (multiple-value-list (subtypep* '(integer 3) `(,sym :foo 3)))
     (multiple-value-list (subtypep* `(,sym :bar 10) '(integer * 10)))
     (multiple-value-list (subtypep* '(integer * 10) `(,sym :bar 10)))

     (multiple-value-list (subtypep* `(,sym :foo 3 :foo 4 :bar 6) '(integer 3 6)))
     (multiple-value-list (subtypep* '(integer 3 6) `(,sym :foo 3 :foo 4 :bar 6)))
     (multiple-value-list (subtypep* `(,sym :bar * :foo (1)) '(integer 2)))
     (multiple-value-list (subtypep* '(integer 2) `(,sym :bar * :foo (1))))
     ))
  t
  (t t) (t t)
  (t t) (t t) (t t) (t t)
  (t t) (t t) (t t) (t t)
  (t t) (t t) (t t) (t t)
  )

(deftest deftype.12
  (let* ((sym (gensym))
	 (form `(deftype ,sym (&key foo bar &allow-other-keys) `(integer ,foo ,bar))))
    (values
     (eqlt (eval form) sym)
     (multiple-value-list (subtypep* `(,sym :xyz t) 'integer))
     (multiple-value-list (subtypep* 'integer `(,sym :xyz t)))

     (multiple-value-list (subtypep* `(,sym :allow-other-keys nil abc t) 'integer))
     (multiple-value-list (subtypep* 'integer `(,sym :allow-other-keys nil abc t)))
     (multiple-value-list (subtypep* `(,sym :foo -10 :bar 20) '(integer -10 20)))
     (multiple-value-list (subtypep* '(integer -10 20) `(,sym :foo -10 :bar 20)))
     ))
  t
  (t t) (t t)
  (t t) (t t) (t t) (t t)
  )

(deftest deftype.13
  (let* ((sym (gensym))
	 (form `(deftype ,sym (&rest args) (if args `(member ,@args) nil))))
    (values
     (eqlt (eval form) sym)
;;     (multiple-value-list (subtypep* sym nil))
;;     (multiple-value-list (subtypep* nil sym))
     (multiple-value-list (subtypep* `(,sym) nil))
     (multiple-value-list (subtypep* nil `(,sym)))
     (notnot (typep 'a `(,sym a)))
     (notnot (typep 'b `(,sym a)))
     (notnot (typep '* `(,sym a)))
     (notnot (typep 'a `(,sym a b)))
     (notnot (typep 'b `(,sym a b)))
     (notnot (typep 'c `(,sym a b)))))
  t
  (t t) (t t)
  t nil nil t t nil)

;;; I've removed this test, because EVAL can cause implicit compilation,
;;; and the semantic constraints on compilation forbid redefinition of
;;; of the types produced by DEFTYPE at runtime.
#|
(deftest deftype.14
  (let* ((sym (gensym))
	 (*f* nil)
	 (form `(let ((x 1))
		  (declare (special *f*))
		  (setf *f* #'(lambda (y) (setf x y)))
		  (deftype ,sym () `(integer 0 ,x)))))
    (declare (special *f*))
    (values
     (eqlt (eval form) sym)
     (loop for i from -1 to 3 collect (typep* i sym))
     (funcall *f* 2)
     (loop for i from -1 to 3 collect (typep* i sym))))
  t (nil t t nil nil) 2 (nil t t t nil))
|#

(deftest deftype.15
  (let* ((sym (gensym))
	 (form `(let ((a 1))
		  (deftype ,sym (&optional (x a))
		    (declare (special a))
		    `(integer 0 ,x)))))
    (values
     (eqlt (eval form) sym)
     (let ((a 2))
       (declare (special a))
       (loop for i from -1 to 3 collect (typep* i `(,sym 1))))
     (let ((a 2))
       (declare (special a))
       (loop for i from -1 to 3 collect (typep* i sym)))))
  t
  (nil t t nil nil)
  (nil t t nil nil))
    
(deftest deftype.16
  (let* ((sym (gensym))
	 (form `(deftype ,sym () (return-from ,sym 'integer))))
    (values
     (eqlt (eval form) sym)
     (subtypep* sym 'integer)
     (subtypep* 'integer sym)))
  t t t)

(deftest deftype.17
  (let* ((sym (gensym))
	 (form `(deftype ,sym () (values 'integer t))))
    (values
     (eqlt (eval form) sym)
     (subtypep* sym 'integer)
     (subtypep* 'integer sym)))
  t t t)

(deftest deftype.18
  (let* ((sym (gensym))
	 (form `(deftype ,sym ())))
    (values
     (eqlt (eval form) sym)
     (subtypep* sym nil)
     (subtypep* nil sym)))
  t t t)

(deftest deftype.19
  (let* ((sym (gensym))
	 (form `(deftype ,sym ()
		  (declare (optimize speed safety debug compilation-speed space))
		  'integer)))		  
    (values
     (eqlt (eval form) sym)
     (subtypep* sym 'integer)
     (subtypep* 'integer sym)))
  t t t)
  
;;; Error tests

(deftest deftype.error.1
  (signals-error (funcall (macro-function 'deftype))
		 program-error)
  t)

(deftest deftype.error.2
  (signals-error (funcall (macro-function 'deftype)
			   '(deftype nonexistent-type () nil))
		 program-error)
  t)

(deftest deftype.error.3
  (signals-error (funcall (macro-function 'deftype)
			   '(deftype nonexistent-type () nil)
			   nil nil)
		 program-error)
  t)

