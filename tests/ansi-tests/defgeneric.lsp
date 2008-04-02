;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 17 20:55:50 2003
;;;; Contains: Tests of DEFGENERIC

(in-package :cl-test)

;;; Various error cases

(defun defgeneric-testfn-01 (x) x)

(deftest defgeneric.error.1
  ;; Cannot make ordinary functions generic
  (let* ((name 'defgeneric-testfn-01)
	 (fn (symbol-function name)))
    (if (not (typep fn 'generic-function))
	(handler-case
	 (progn (eval `(defgeneric ,name ())) :bad)
	 (program-error () :good))
      :good))
  :good)

(defmacro defgeneric-testmacro-02 (x) x)

(deftest defgeneric.error.2
  ;; Cannot make macros generic
  (let* ((name 'defgeneric-testmacro-02))
    (handler-case
     (progn (eval `(defgeneric ,name ())) :bad)
     (program-error () :good)))
  :good)

(deftest defgeneric.error.3
  ;; Cannot make special operators generic
  (loop for name in *cl-special-operator-symbols*
	for result =
	(handler-case
	 (progn (eval `(defgeneric ,name ())) t)
	 (program-error () nil))
	when result collect name)
  nil)

(deftest defgeneric.error.4
  (signals-error (defgeneric defgeneric-error-fn.4 (x y)
		    (:argument-precedence-order x y x))
		 program-error)
  t)

(deftest defgeneric.error.5
  (signals-error (defgeneric defgeneric-error-fn.5 (x)
		    (:documentation "some documentation")
		    (:documentation "illegally repeated documentation"))
		 program-error)
  t)

(deftest defgeneric.error.6
  (signals-error (defgeneric defgeneric-error-fn.6 (x)
		    (unknown-option nil))
		 program-error)
  t)

(deftest defgeneric.error.7
  (handler-case
   (progn
     (eval '(defgeneric defgeneric-error-fn.7 (x y)
	      (:method ((x t)) x)))
     :bad)
   (error () :good))
  :good)

(deftest defgeneric.error.8
  (signals-error (defgeneric defgeneric-error-fn.8 (x y)
		    (:argument-precedence-order x))
		 program-error)
  t)


;;; Non-congruent methods cause defgeneric to signal an error

(deftest defgeneric.error.9
  (handler-case
   (progn
     (eval '(defgeneric defgeneric-error-fn.9 (x)
	      (:method ((x t)(y t)) t)))
     :bad)
   (error () :good))
  :good)


(deftest defgeneric.error.10
  (handler-case
   (progn
     (eval '(defgeneric defgeneric-error-fn.10 (x &optional y)
	      (:method ((x t)) t)))
     :bad)
   (error () :good))
  :good)

(deftest defgeneric.error.11
  (handler-case
   (progn
     (eval '(defgeneric defgeneric-error-fn.11 (x &optional y)
	      (:method (x &optional y z) t)))
     :bad)
   (error () :good))
  :good)

(deftest defgeneric.error.12
  (handler-case
   (progn
     (eval '(defgeneric defgeneric-error-fn.12 (x &rest y)
	      (:method (x) t)))
     :bad)
   (error () :good))
  :good)

(deftest defgeneric.error.13
  (handler-case
   (progn
     (eval '(defgeneric defgeneric-error-fn.13 (x)
	      (:method (x &rest y) t)))
     :bad)
   (error () :good))
  :good)

(deftest defgeneric.error.14
  (handler-case
   (progn
     (eval '(defgeneric defgeneric-error-fn.14 (x &key)
	      (:method (x) t)))
     :bad)
   (error () :good))
  :good)

(deftest defgeneric.error.15
  (handler-case
   (progn
     (eval '(defgeneric defgeneric-error-fn.15 (x &key y)
	      (:method (x) t)))
     :bad)
   (error () :good))
  :good)

(deftest defgeneric.error.16
  (handler-case
   (progn
     (eval '(defgeneric defgeneric-error-fn.16 (x)
	      (:method (x &key) t)))
     :bad)
   (error () :good))
  :good)

(deftest defgeneric.error.17
  (handler-case
   (progn
     (eval '(defgeneric defgeneric-error-fn.17 (x)
	      (:method (x &key foo) t)))
     :bad)
   (error () :good))
  :good)

(deftest defgeneric.error.18
  (handler-case
   (progn
     (eval '(defgeneric defgeneric-error-fn.18 (x &key foo)
	      (:method (x &key) t)))
     :bad)
   (error () :good))
  :good)

(deftest defgeneric.error.19
  (handler-case
   (progn
     (eval '(defgeneric defgeneric-error-fn.19 (x &key foo)
	      (:method (x &key bar) t)))
     :bad)
   (error () :good))
  :good)

;;; A close reading of the rules for keyword arguments to
;;; generic functions convinced me that the following two
;;; error tests are necessary.  See sections 7.6.5 of the CLHS.

(deftest defgeneric.error.20
  (signals-error
   (let ((fn (defgeneric defgeneric-error-fn.20 (x &key)
	       (:method ((x number) &key foo) (list x foo))
	       (:method ((x symbol) &key bar) (list x bar)))))
     (funcall fn 1 :bar 'a))
   program-error)
  t)

(deftest defgeneric.error.21
  (signals-error
   (let ((fn (defgeneric defgeneric-error-fn.21 (x &key)
	       (:method ((x number) &key foo &allow-other-keys) (list x foo))
	       (:method ((x symbol) &key bar) (list x bar)))))
     (funcall fn 'x :foo 'a))
   program-error)
  t)

;;;

(deftest defgeneric.error.22
  (progn
    (defgeneric defgeneric-error-fn.22 (x))
    (defmethod defgeneric-error-fn.22 ((x t)) nil)
    (handler-case
     (eval '(defgeneric defgeneric-error-fn.22 (x y)))
     (error () :good)))
  :good)


;;; Non error cases

(deftest defgeneric.1
  (let ((fn (eval '(defgeneric defgeneric.fun.1 (x y z)
		     (:method ((x t) (y t) (z t)) (list x y z))))))
    (declare (type function fn))
    (values
     (typep* fn 'generic-function)
     (typep* fn 'standard-generic-function)
     (funcall fn 'a 'b 'c)
     (apply fn 1 2 3 nil)
     (apply fn (list 4 5 6))
     (mapcar fn '(1 2) '(3 4) '(5 6))
     (defgeneric.fun.1 'd 'e 'f)))
  t t (a b c) (1 2 3) (4 5 6) ((1 3 5) (2 4 6)) (d e f))

(deftest defgeneric.2
  (let ((fn (eval '(defgeneric defgeneric.fun.2 (x y z)
		     (:documentation "boo!")
		     (:method ((x t) (y t) (z t)) (vector x y z))))))
    (declare (type function fn))
    (values
     (typep* fn 'generic-function)
     (typep* fn 'standard-generic-function)
     (funcall fn 'a 'b 'c)
     (defgeneric.fun.2 'd 'e 'f)
     (let ((doc (documentation fn t)))
       (or (not doc)
	   (and (stringp doc) (string=t doc "boo!"))))
     (let ((doc (documentation fn 'function)))
       (or (not doc)
	   (and (stringp doc) (string=t doc "boo!"))))
     (setf (documentation fn t) "foo")
     (let ((doc (documentation fn t)))
       (or (not doc)
	   (and (stringp doc) (string=t doc "foo"))))
     (setf (documentation fn 'function) "bar")
     (let ((doc (documentation fn t)))
       (or (not doc)
	   (and (stringp doc) (string=t doc "bar"))))))
     
  t t #(a b c) #(d e f) t t "foo" t "bar" t)

(deftest defgeneric.3
  (let ((fn (eval '(defgeneric defgeneric.fun.3 (x y)
		     (:method ((x t) (y symbol)) (list x y))
		     (:method ((x symbol) (y t)) (list y x))))))
    (declare (type function fn))
    (values
     (typep* fn 'generic-function)
     (typep* fn 'standard-generic-function)
     (funcall fn 1 'a)
     (funcall fn 'b 2)
     (funcall fn 'a 'b)))
  t t
  (1 a)
  (2 b)
  (b a))

(deftest defgeneric.4
  (let ((fn (eval '(defgeneric defgeneric.fun.4 (x y)
		     (:argument-precedence-order y x)
		     (:method ((x t) (y symbol)) (list x y))
		     (:method ((x symbol) (y t)) (list y x))))))
    (declare (type function fn))
    (values
     (typep* fn 'generic-function)
     (typep* fn 'standard-generic-function)
     (funcall fn 1 'a)
     (funcall fn 'b 2)
     (funcall fn 'a 'b)))
  t t
  (1 a)
  (2 b)
  (a b))

(deftest defgeneric.5
  (let ((fn (eval '(defgeneric defgeneric.fun.5 ()
		     (:method () (values))))))
    (declare (type function fn))
    (values
     (typep* fn 'generic-function)
     (typep* fn 'standard-generic-function)
     (multiple-value-list (funcall fn))
     (multiple-value-list (defgeneric.fun.5))
     (multiple-value-list (apply fn nil))))
  t t nil nil nil)

(deftest defgeneric.6
  (let ((fn (eval '(defgeneric defgeneric.fun.6 ()
		     (:method () (values 'a 'b 'c))))))
    (declare (type function fn))
    (values
     (typep* fn 'generic-function)
     (typep* fn 'standard-generic-function)
     (multiple-value-list (funcall fn))
     (multiple-value-list (defgeneric.fun.6))
     (multiple-value-list (apply fn nil))))
  t t (a b c) (a b c) (a b c))

(deftest defgeneric.7
  (let ((fn (eval '(defgeneric defgeneric.fun.7 ()
		     (:method () (return-from defgeneric.fun.7 'a) 'b)))))
    (declare (type function fn))
    (values
     (typep* fn 'generic-function)
     (typep* fn 'standard-generic-function)
     (multiple-value-list (funcall fn))
     (multiple-value-list (defgeneric.fun.7))
     (multiple-value-list (apply fn nil))))
  t t (a) (a) (a))

(deftest defgeneric.8
  (let ((fn (eval '(defgeneric defgeneric.fun.8 (x &optional y z)
		     (:method ((x number) &optional y z)
			      (list x y z))
		     (:method ((p symbol) &optional q r)
			      (list r q p))))))
    (declare (type function fn))
    (values
     (typep* fn 'generic-function)
     (typep* fn 'standard-generic-function)
     (multiple-value-list (funcall fn 1))
     (multiple-value-list (funcall fn 1 2))
     (multiple-value-list (funcall fn 1 2 3))
     (multiple-value-list (defgeneric.fun.8 'a))
     (multiple-value-list (defgeneric.fun.8 'a 'b))
     (multiple-value-list (defgeneric.fun.8 'a 'b 'c))
     (multiple-value-list (apply fn '(x y z)))))
  t t
  ((1 nil nil))
  ((1 2 nil))
  ((1 2 3))
  ((nil nil a))
  ((nil b a))
  ((c b a))
  ((z y x)))

(deftest defgeneric.9
  (let ((fn (eval '(defgeneric defgeneric.fun.9 (x &optional y z)
		     (:method ((x number) &optional (y 10) (z 20))
			      (list x y z))
		     (:method ((p symbol) &optional (q 's) (r 't))
			      (list r q p))))))
    (declare (type function fn))
    (values
     (funcall fn 1)
     (funcall fn 1 2)
     (funcall fn 1 2 3)
     (funcall fn 'a)
     (funcall fn 'a 'b)
     (funcall fn 'a 'b 'c)))
  (1 10 20)
  (1 2 20)
  (1 2 3)
  (t s a)
  (t b a)
  (c b a))

 (deftest defgeneric.10
   (let ((fn (eval '(defgeneric defgeneric.fun.10 (x &rest y)
		      (:method ((x number) &key foo) (list x foo))))))
     (declare (type function fn))
     (values
      (funcall fn 1)
      (funcall fn 1 :foo 'a)
      (defgeneric.fun.10 5/3 :foo 'x :foo 'y)
      (defgeneric.fun.10 10 :bar t :allow-other-keys t)
      (defgeneric.fun.10 20 :allow-other-keys nil :foo 'x)))
   (1 nil)
   (1 a)
   (5/3 x)
   (10 nil)
   (20 x))

 (deftest defgeneric.11
   (let ((fn (eval '(defgeneric defgeneric.fun.11 (x &key)
		      (:method ((x number) &key foo) (list x foo))))))
     (declare (type function fn))
     (values
      (funcall fn 1)
      (funcall fn 1 :foo 'a)
      (defgeneric.fun.11 5/3 :foo 'x :foo 'y)
      (defgeneric.fun.11 11 :bar t :allow-other-keys t)
      (defgeneric.fun.11 20 :allow-other-keys nil :foo 'x)))
   (1 nil)
   (1 a)
   (5/3 x)
   (11 nil)
   (20 x))

 (deftest defgeneric.12
   (let ((fn (eval '(defgeneric defgeneric.fun.12 (x &key foo bar baz)
		      (:method ((x number) &rest y) (list x y))))))
     (declare (type function fn))
     (values
      (funcall fn 1)
      (funcall fn 1 :foo 'a)
      (defgeneric.fun.12 5/3 :foo 'x :foo 'y :bar 'z)
      (defgeneric.fun.12 11 :zzz t :allow-other-keys t)
      (defgeneric.fun.12 20 :allow-other-keys nil :foo 'x)))
   (1 nil)
   (1 (:foo a))
   (5/3 (:foo x :foo y :bar z))
   (11 (:zzz t :allow-other-keys t))
   (20 (:allow-other-keys nil :foo x)))

 (deftest defgeneric.13
   (let ((fn (eval '(defgeneric defgeneric.fun.13 (x &key)
		      (:method ((x number) &key foo) (list x foo))
		      (:method ((x symbol) &key bar) (list x bar))))))
     (declare (type function fn))
     (values
      (funcall fn 1)
      (funcall fn 'a)
      (funcall fn 1 :foo 2)
      ;; (funcall fn 1 :foo 2 :bar 3)
      ;; (funcall fn 1 :bar 4)
      ;; (funcall fn 'a :foo 'b)
      (funcall fn 'a :bar 'b)
      ;; (funcall fn 'a :foo 'c :bar 'b)
      ))
   (1 nil)
   (a nil)
   (1 2)
   ;; (1 2)
   ;; (1 nil)
   ;; (a nil)
   (a b)
   ;; (a b)
   )

 (deftest defgeneric.14
   (let ((fn (eval '(defgeneric defgeneric.fun.14 (x &key &allow-other-keys)
		      (:method ((x number) &key foo) (list x foo))
		      (:method ((x symbol) &key bar) (list x bar))))))
     (declare (type function fn))
     (values
      (funcall fn 1)
      (funcall fn 'a)
      (funcall fn 1 :foo 2)
      (funcall fn 1 :foo 2 :bar 3)
      (funcall fn 1 :bar 4)
      (funcall fn 'a :foo 'b)
      (funcall fn 'a :bar 'b)
      (funcall fn 'a :foo 'c :bar 'b)
      (funcall fn 1 :baz 10)
      (funcall fn 'a :baz 10)
      (funcall fn 1 :allow-other-keys nil :baz 'a)
      (funcall fn 'a :allow-other-keys nil :baz 'b)
      ))
   (1 nil)
   (a nil)
   (1 2)
   (1 2)
   (1 nil)
   (a nil)
   (a b)
   (a b)
   (1 nil)
   (a nil)
   (1 nil)
   (a nil))

 (deftest defgeneric.15
   (let ((fn (eval '(defgeneric defgeneric.fun.15 (x &key)
		      (:method ((x number) &key foo &allow-other-keys)
			       (list x foo))
		      (:method ((x symbol) &key bar) (list x bar))))))
     (declare (type function fn))
     (values
      (funcall fn 1)
      (funcall fn 'a)
      (funcall fn 1 :foo 2)
      (funcall fn 1 :foo 2 :bar 3)
      (funcall fn 1 :bar 4)
      (funcall fn 'a :allow-other-keys t :foo 'b)
      (funcall fn 'a :bar 'b)
      (funcall fn 'a :foo 'c :bar 'b :allow-other-keys t)
      (funcall fn 1 :baz 10)
      ;; (funcall fn 'a :baz 10)
      (funcall fn 1 :allow-other-keys nil :baz 'a)
      ;; (funcall fn 'a :allow-other-keys nil :baz 'b)
      ))
   (1 nil)
   (a nil)
   (1 2)
   (1 2)
   (1 nil)
   (a nil)
   (a b)
   (a b)
   (1 nil)
   ;; (a nil)
   (1 nil)
   ;; (a nil)
   )

 (deftest defgeneric.16
   (let ((fn (eval '(defgeneric defgeneric.fun.16 (x &key)
		      (:method ((x number) &key (foo 'a))
			       (list x foo))
		      (:method ((x symbol) &key foo)
			       (list x foo))))))
     (declare (type function fn))
     (values
      (funcall fn 1)
      (funcall fn 1 :foo nil)
      (funcall fn 1 :foo 2)
      (funcall fn 'x)
      (funcall fn 'x :foo nil)
      (funcall fn 'x :foo 'y)))
   (1 a)
   (1 nil)
   (1 2)
   (x nil)
   (x nil)
   (x y))

 (deftest defgeneric.17
   (let ((fn (eval '(defgeneric defgeneric.fun.17 (x &key)
		      (:method ((x number) &key (foo 'a foo-p))
			       (list x foo (notnot foo-p)))
		      (:method ((x symbol) &key foo)
			       (list x foo))))))
     (declare (type function fn))
     (values
      (funcall fn 1)
      (funcall fn 1 :foo nil)
      (funcall fn 1 :foo 2)
      (funcall fn 'x)
      (funcall fn 'x :foo nil)
      (funcall fn 'x :foo 'y)))
   (1 a nil)
   (1 nil t)
   (1 2 t)
   (x nil)
   (x nil)
   (x y))

(deftest defgeneric.18
   (let ((fn (eval '(defgeneric defgeneric.fun.18 (x &optional y)
		      (:method ((x number) &optional (y 'a))
			       (list x y))
		      (:method ((x symbol) &optional (z nil z-p))
			       (list x z (notnot z-p)))))))
     (declare (type function fn))
     (values
      (funcall fn 1)
      (funcall fn 1 nil)
      (funcall fn 1 2)
      (funcall fn 'x)
      (funcall fn 'x nil)
      (funcall fn 'x 'y)))
   (1 a)
   (1 nil)
   (1 2)
   (x nil nil)
   (x nil t)
   (x y t))

 (deftest defgeneric.19
   (let ((fn (eval '(defgeneric defgeneric.fun.19 (x &key)
		      (:method ((x number) &key ((:bar foo) 'a foo-p))
			       (list x foo (notnot foo-p)))))))
     (declare (type function fn))
     (values
      (funcall fn 1)
      (funcall fn 1 :bar nil)
      (funcall fn 1 :bar 2)))
   (1 a nil)
   (1 nil t)
   (1 2 t))

(deftest defgeneric.20
   (let ((fn (eval '(defgeneric defgeneric.fun.20 (x &optional y z)
		      (:method ((x number)
				&optional (y (1+ x) y-p)
				          (z (if y-p (1+ y) (+ x 10))
					     z-p))
			       (list x y (notnot y-p) z (notnot z-p)))))))
     (declare (type function fn))
     (values
      (funcall fn 1)
      (funcall fn 1 5)
      (funcall fn 1 5 9)))
   (1 2 nil 11 nil)
   (1 5 t 6 nil)
   (1 5 t 9 t))

(deftest defgeneric.21
   (let ((fn (eval '(defgeneric defgeneric.fun.21 (x &key)
		      (:method ((x number)
				&key (y (1+ x) y-p)
				(z (if y-p (1+ y) (+ x 10))
				   z-p))
			       (list x y (notnot y-p) z (notnot z-p)))))))
     (declare (type function fn))
     (values
      (funcall fn 1)
      (funcall fn 1 :y 5)
      (funcall fn 1 :y 5 :z 9)
      (funcall fn 1 :z 8)
      (funcall fn 1 :z 8 :y 4)))
   (1 2 nil 11 nil)
   (1 5 t 6 nil)
   (1 5 t 9 t)
   (1 2 nil 8 t)
   (1 4 t 8 t))

(deftest defgeneric.22
   (let ((fn (eval '(defgeneric defgeneric.fun.22 (x &key)
		      (:method ((x number) &key ((:allow-other-keys y)))
			       (list x y))))))
     (declare (type function fn))
     (values
      (funcall fn 1)
      (funcall fn 1 :allow-other-keys nil)
      (funcall fn 1 :allow-other-keys t)
      (funcall fn 1 :foo 'x :allow-other-keys t :bar 'y)
      (funcall fn 1 :allow-other-keys t :foo 'x)
      (funcall fn 1 :allow-other-keys nil :allow-other-keys t)
      (funcall fn 1 :foo 'x :allow-other-keys t :allow-other-keys nil)
      (funcall fn 1 :allow-other-keys t 'foo 'y :allow-other-keys nil)
      (funcall fn 1 :allow-other-keys t :allow-other-keys nil '#:foo 'z)))
   (1 nil)
   (1 nil)
   (1 t)
   (1 t)
   (1 t)
   (1 nil)
   (1 t)
   (1 t)
   (1 t))

(deftest defgeneric.23
   (let ((fn (eval '(defgeneric defgeneric.fun.23 (x)
		      (:method ((x number) &aux (y (1+ x))) (list x y))
		      (:method ((x symbol) &aux (z (list x))) (list x z))))))
     (declare (type function fn))
     (values
      (funcall fn 1)
      (funcall fn 'a)))
   (1 2) (a (a)))


(deftest defgeneric.24
   (let ((fn (eval '(defgeneric defgeneric.fun.24 (x)
		      (:method ((x number) &aux (y (1+ x)) (z (1+ y)))
			       (list x y z))
		      (:method ((x symbol) &aux (y (list x)) (z (list x y)))
			       (list x y z))))))
     (values
      (funcall fn 1)
      (funcall fn 'a)))
   (1 2 3)
   (a (a) (a (a))))

(deftest defgeneric.25
  (let ((fn (eval '(defgeneric defgeneric.fun.25 (x &optional y &key)
		      (:method ((x symbol) &optional (y 'd y-p)
				&key ((:foo bar) (list x y) bar-p)
				&aux (z (list x y (notnot y-p)
					      bar (notnot bar-p))))
			       z)))))
    (declare (type function fn))
    (values
     (funcall fn 'a)
     (funcall fn 'a 'b)
     (funcall fn 'a 'b :foo 'c)))
  (a d nil (a d) nil)
  (a b t (a b) nil)
  (a b t c t))

(deftest defgeneric.26
  (let ((fn (eval '(defgeneric defgeneric.fun.26 (x)
		     (declare (optimize (safety 3)))
		     (:method ((x symbol)) x)
		     (declare (optimize (debug 3)))))))
    (declare (type function fn))
    (funcall fn 'a))
  a)

#|
(when (subtypep (class-of (find-class 'standard-method))
		'standard-class)
  (defclass substandard-method (standard-method) ())
  (deftest defgeneric.27
    (let ((fn (eval '(defgeneric defgeneric.fun.27 (x y)
		       (:method-class substandard-method)
		       (:method ((x number) (y number)) (+ x y))
		       (:method ((x string) (y string))
				(concatenate 'string x y))))))
      (declare (type function fn))
      (values
       (funcall fn 1 2)
       (funcall fn "1" "2")))
    3 "12"))
|#

(deftest defgeneric.28
  (let ((fn (eval '(defgeneric defgeneric.fun.28 (x &key)
		     (:method ((x integer) &key foo) (list x foo))
		     (:method ((x number) &key bar) (list x bar))
		     (:method ((x t) &key baz) (list x baz))))))
    (declare (type function fn))
    (values
      
     (funcall fn 1)
     (funcall fn 1 :foo 'a)
     (funcall fn 1 :bar 'b)
     (funcall fn 1 :baz 'c)
     (funcall fn 1 :bar 'b :baz 'c)
     (funcall fn 1 :foo 'a :bar 'b)
     (funcall fn 1 :foo 'a :baz 'c)
     (funcall fn 1 :foo 'a :bar 'b :baz 'c)
     
     (funcall fn 5/3)
     (funcall fn 5/3 :bar 'b)
     (funcall fn 5/3 :baz 'c)
     (funcall fn 5/3 :bar 'b :baz 'c)
     
     (funcall fn 'x)
     (funcall fn 'x :baz 'c)
     
     ))

  (1 nil) (1 a) (1 nil) (1 nil)
  (1 nil) (1 a) (1 a)   (1 a)

  (5/3 nil) (5/3 b)   (5/3 nil) (5/3 b)

  (x nil) (x c))

(defclass defgeneric.29.class.1 () ())
(defclass defgeneric.29.class.2 () ())
(defclass defgeneric.29.class.3
  (defgeneric.29.class.1 defgeneric.29.class.2)
  ())

(deftest defgeneric.29
  (let ((fn
	 (eval '(defgeneric defgeneric.fun.29 (x &key)
		  (:method ((x defgeneric.29.class.1) &key foo) foo)
		  (:method ((x defgeneric.29.class.2) &key bar) bar)))))
    (declare (type function fn))
    (let ((x (make-instance 'defgeneric.29.class.3)))
      (values
       (funcall fn x)
       (funcall fn x :foo 'a)
       (funcall fn x :bar 'b)
       (funcall fn x :foo 'a :bar 'b)
       (funcall fn x :bar 'b :foo 'a))))
  nil a nil a a)

;;; I'm not sure this one is proper
;;; Added :metaclass at prompting of Martin Simmons
(when (subtypep (class-of (find-class 'standard-generic-function))
		'standard-class)
  (defclass substandard-generic-function (standard-generic-function) ()
    (:metaclass #.(class-name (class-of
			       (find-class 'standard-generic-function)))))
  (deftest defgeneric.30
    (let ((fn
	   (eval '(defgeneric defgeneric.fun.29 (x)
		    (:generic-function-class substandard-generic-function)
		    (:method ((x symbol)) 1)
		    (:method ((x integer)) 2)))))
      (declare (type function fn))
      (values
       (typep* fn 'substandard-generic-function)
       (typep* fn 'standard-generic-function)
       (typep* fn 'generic-function)
       (typep* fn 'function)
       (funcall fn 'a)
       (funcall fn 1)
       (defgeneric.fun.29 'x)
       (defgeneric.fun.29 12345678901234567890)))
    t t t t 1 2 1 2))

(deftest defgeneric.31
  (progn
    (defgeneric defgeneric.fun.31 (x) (:method ((x t)) t))
    (defgeneric defgeneric.fun.31 (x y) (:method ((x t) (y t)) (list x y)))
    (defgeneric.fun.31 'a 'b))
  (a b))

(deftest defgeneric.32
  (progn
    (defgeneric defgeneric.fun.32 (x) (:method ((x symbol)) :bad))
    (defgeneric defgeneric.fun.32 (x) (:method ((x t)) :good))
    (defgeneric.fun.32 'x))
  :good)

(deftest defgeneric.33
  (let ((fn
	 (eval
	  '(defgeneric (setf defgeneric.fun.33) (x y &rest args)
	     (:method (x (y cons) &rest args)
		      (assert (null args)) (setf (car y) x))
	     (:method (x (y array) &rest args)
		      (setf (apply #'aref y args) x))))))
    (declare (type function fn))
    (values
     (let ((z (list 'a 'b)))
       (list
	(setf (defgeneric.fun.33 z) 'c)
	z))
     (let ((a (make-array '(10) :initial-element nil)))
       (list
	(setf (defgeneric.fun.33 a 5) 'd)
	a))))
  (c (c b))
  (d #(nil nil nil nil nil d nil nil nil nil)))

(deftest defgeneric.34
  (let ((fn (eval '(defgeneric #:defgeneric.fun.34 (x)
		     (:method ((x t)) (list x :good))))))
    (funcall fn 10))
  (10 :good))

(deftest defgeneric.35
  (let ((fn (eval '(defgeneric defgeneric.fun.35 (x)
		     (:method ((x (eql 'a)))
			      (declare (optimize (speed 0)))
			      "FOO"
			      (declare (optimize (safety 3)))
			      x)))))
    (declare (type function fn))
    (values
     (funcall fn 'a)
     (let ((method (first (compute-applicable-methods fn '(a)))))
       (and method
	    (let ((doc (documentation method t)))
	      (list
	       (or (null doc) (equalt doc "FOO"))
	       (setf (documentation method t) "BAR")
	       (let ((doc (documentation method t)))
		 (or (null doc) (equalt doc "BAR")))
	       ))))))
  a (t "BAR" t))
