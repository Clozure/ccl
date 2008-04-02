;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 12:35:24 2003
;;;; Contains: Tests of DEFMACRO

(in-package :cl-test)

(deftest defmacro.error.1
  (signals-error (funcall (macro-function 'defmacro))
		 program-error)
  t)

(deftest defmacro.error.2
  (signals-error (funcall (macro-function 'defmacro)
			   '(defmacro nonexistent-macro ()))
		 program-error)
  t)

(deftest defmacro.error.3
  (signals-error (funcall (macro-function 'defmacro)
			   '(defmacro nonexistent-macro ())
			   nil nil)
		 program-error)
  t)

;;; FIXME
;;; Need to add non-error tests

(deftest defmacro.1
  (progn
    (assert (eq (defmacro defmacro.1-macro (x y) `(list 1 ,x 2 ,y 3))
		'defmacro.1-macro))
    (assert (macro-function 'defmacro.1-macro))
    (eval `(defmacro.1-macro 'a 'b)))
  (1 a 2 b 3))

(deftest defmacro.2
  (progn
    (assert (eq (defmacro defmacro.2-macro (x y)
		  (return-from defmacro.2-macro `(cons ,x ,y)))
		'defmacro.2-macro))
    (assert (macro-function 'defmacro.2-macro))
    (eval `(defmacro.2-macro 'a 'b)))
  (a . b))

;;; The macro function is defined in the lexical environment in which
;;; the defmacro form occurs.
(deftest defmacro.3
  (let (fn)
    (let ((x 0))
      (setq fn #'(lambda (n) (setq x n)))
      (defmacro defmacro.3-macro () `',x))
    (values
     (eval '(defmacro.3-macro))
     (funcall fn 'a)
     (eval '(defmacro.3-macro))))
  0 a a)

;;; Declarations are allowed.

;;; Free special declarations do not apply to the forms
;;; in the lambda list
(deftest defmacro.4
  (let ((y :good))
    (assert (eq (defmacro defmacro.4-macro (&optional (x y))
		  (declare (special y))
		  x)
		'defmacro.4-macro))
    (let ((y :bad))
      (declare (special y))
      (values (macroexpand-1 '(defmacro.4-macro)))))
  :good)

(deftest defmacro.5
  (progn
    (assert (eq (defmacro defmacro.5-macro ()
		  (declare) (declare) "a doc string" (declare)
		  t)
		'defmacro.5-macro))
    (eval `(defmacro.5-macro)))
  t)

;;; &whole argument, top level
(deftest defmacro.6
  (progn
    (defmacro defmacro.6-macro (&whole w arg)
      `(list ',w ',arg))
    (eval `(defmacro.6-macro x)))
  ((defmacro.6-macro x) x))

;;; &whole argument in destructuring
(deftest defmacro.7
  (progn
    (defmacro defmacro.7-macro (arg1 (&whole w arg2))
      `(list ',w ',arg1 ',arg2))
    (eval `(defmacro.7-macro x (y))))
  ((y) x y)) 

;;; keyword parameters
(deftest defmacro.8
  (progn
    (defmacro defmacro.8-macro (&key foo bar)
      `(list ',foo ',bar))
    (mapcar #'eval '((defmacro.8-macro :foo x)
		     (defmacro.8-macro :bar y)
		     (defmacro.8-macro :bar a :foo b)
		     (defmacro.8-macro :bar a :foo b :bar c))))
  ((x nil) (nil y) (b a) (b a)))

;;; keyword parameters with default value
(deftest defmacro.9
  (progn
    (defmacro defmacro.9-macro (&key (foo 1) (bar 2))
      `(list ',foo ',bar))
    (mapcar #'eval '((defmacro.9-macro :foo x)
		     (defmacro.9-macro :bar y)
		     (defmacro.9-macro :foo nil)
		     (defmacro.9-macro :bar nil)
		     (defmacro.9-macro :bar a :foo b)
		     (defmacro.9-macro :bar a :foo b :bar c))))
  ((x 2) (1 y) (nil 2) (1 nil) (b a) (b a)))

;;; keyword parameters with supplied-p parameter
(deftest defmacro.10
  (progn
    (defmacro defmacro.10-macro (&key (foo 1 foo-p) (bar 2 bar-p))
      `(list ',foo ,(notnot foo-p) ',bar ,(notnot bar-p)))
    (mapcar #'eval '((defmacro.10-macro)
		     (defmacro.10-macro :foo x)
		     (defmacro.10-macro :bar y)
		     (defmacro.10-macro :foo nil)
		     (defmacro.10-macro :bar nil)
		     (defmacro.10-macro :foo x :bar y)
		     (defmacro.10-macro :bar y :foo x)
		     (defmacro.10-macro :bar a :bar b)
		     (defmacro.10-macro :foo a :foo b))))
  ((1 nil 2 nil) (x t 2 nil) (1 nil y t)
   (nil t 2 nil) (1 nil nil t) (x t y t)
   (x t y t) (1 nil a t) (a t 2 nil)))

;;; key arguments in destructuring

(deftest defmacro.11
  (progn
    (defmacro defmacro.11-macro ((&key foo bar)) `(list ',foo ',bar))
    (mapcar #'eval '((defmacro.11-macro nil)
		     (defmacro.11-macro (:foo x))
		     (defmacro.11-macro (:bar y))
		     (defmacro.11-macro (:foo x :bar y :foo z))
		     (defmacro.11-macro (:bar y :bar z :foo x)))))
  ((nil nil) (x nil) (nil y) (x y) (x y)))

;;;  key arguments in destructuring and defaults

(deftest defmacro.12
  (progn
    (let ((foo-default 1)
	  (bar-default 2))
      (defmacro defmacro.12-macro ((&key (foo foo-default)
					 (bar bar-default)))
	`(list ',foo ',bar)))
    (mapcar #'eval '((defmacro.12-macro nil)
		     (defmacro.12-macro (:foo x))
		     (defmacro.12-macro (:bar y))
		     (defmacro.12-macro (:foo x :bar y :foo z))
		     (defmacro.12-macro (:bar y :bar z :foo x)))))
  ((1 2) (x 2) (1 y) (x y) (x y)))

;;;  key arguments in destructuring and supplied-p parameter

(deftest defmacro.13
  (progn
    (let ((foo-default 1)
	  (bar-default 2))
      (defmacro defmacro.13-macro ((&key (foo foo-default foo-p)
					 (bar bar-default bar-p)))
	`(list ',foo ,(notnot foo-p) ',bar ,(notnot bar-p))))
    (mapcar #'eval '((defmacro.13-macro nil)
		     (defmacro.13-macro (:foo x))
		     (defmacro.13-macro (:bar y))
		     (defmacro.13-macro (:foo nil :bar nil :foo 4 :bar 14))
		     (defmacro.13-macro (:foo 1 :bar 2))
		     (defmacro.13-macro (:foo x :bar y :foo z))
		     (defmacro.13-macro (:bar y :bar z :foo x)))))
  ((1 nil 2 nil) (x t 2 nil) (1 nil y t)
   (nil t nil t) (1 t 2 t)
   (x t y t) (x t y t)))

;;; rest parameter
(deftest defmacro.14
  (progn
    (defmacro defmacro.14-macro (foo &rest bar)
      `(list ',foo ',bar))
    (mapcar #'eval '((defmacro.14-macro x)
		     (defmacro.14-macro x y)
		     (defmacro.14-macro x y z))))
  ((x nil) (x (y)) (x (y z))))

;;; rest parameter with destructuring
(deftest defmacro.15
  (progn
    (defmacro defmacro.15-macro (foo &rest (bar . baz))
      `(list ',foo ',bar ',baz))
    (eval '(defmacro.15-macro x y z)))
  (x y (z)))

;;; rest parameter w. whole
(deftest defmacro.16
  (progn
    (defmacro defmacro.16-macro (&whole w foo &rest bar)
      `(list ',w ',foo ',bar))
    (mapcar #'eval '((defmacro.16-macro x)
		     (defmacro.16-macro x y)
		     (defmacro.16-macro x y z))))
  (((defmacro.16-macro x) x nil)
   ((defmacro.16-macro x y) x (y))
   ((defmacro.16-macro x y z) x (y z))))

;;; env parameter
(deftest defmacro.17
  (progn
    (defmacro defmacro.17-macro (x &environment env)
      `(quote ,(macroexpand x env)))
    (eval
     `(macrolet ((%m () :good))
	(defmacro.17-macro (%m)))))
  :good)

(deftest defmacro.17a
  (progn
    (defmacro defmacro.17a-macro (&environment env x)
      `(quote ,(macroexpand x env)))
    (eval
     `(macrolet ((%m () :good))
	(defmacro.17a-macro (%m)))))
  :good)

;;; &optional with supplied-p parameter
;;; Note: this is required to be T if the parameter is present (3.4.4.1.2)
(deftest defmacro.18
  (progn
    (defmacro defmacro.18-macro (x &optional (y 'a y-p) (z 'b z-p))
      `(list ',x ',y ',y-p ',z ',z-p))
    (mapcar #'eval '((defmacro.18-macro p)
		     (defmacro.18-macro p q)
		     (defmacro.18-macro p q r))))
  ((p a nil b nil)
   (p q t b nil)
   (p q t r t)))

;;; Optional with destructuring
(deftest defmacro.19
  (progn
    (defmacro defmacro.19-macro (&optional ((x . y) '(a . b)))
      `(list ',x ',y))
    (mapcar #'eval '((defmacro.19-macro)
		     (defmacro.19-macro (c d)))))
  ((a b) (c (d))))

;;; Allow other keys

(deftest defmacro.20
  (progn
    (defmacro defmacro.20-macro (&key x y z &allow-other-keys)
      `(list ',x ',y ',z))
    (mapcar #'eval '((defmacro.20-macro)
		     (defmacro.20-macro :x a)
		     (defmacro.20-macro :y b)
		     (defmacro.20-macro :z c)
		     (defmacro.20-macro :x a :y b)
		     (defmacro.20-macro :z c :y b)
		     (defmacro.20-macro :z c :x a)
		     (defmacro.20-macro :z c :x a :y b)
		     (defmacro.20-macro nil nil)
		     (defmacro.20-macro :allow-other-keys nil)
		     (defmacro.20-macro :allow-other-keys nil :foo bar)
		     (defmacro.20-macro :z c :z nil :x a :abc 0 :y b :x t))))
  ((nil nil nil)
   (a nil nil)
   (nil b nil)
   (nil nil c)
   (a b nil)
   (nil b c)
   (a nil c)
   (a b c)
   (nil nil nil)
   (nil nil nil)
   (nil nil nil)
   (a b c)))

(deftest defmacro.21
  (progn
    (defmacro defmacro.21-macro (&key x y z)
      `(list ',x ',y ',z))
    (mapcar #'eval '((defmacro.21-macro)
		     (defmacro.21-macro :x a)
		     (defmacro.21-macro :y b)
		     (defmacro.21-macro :z c)
		     (defmacro.21-macro :x a :y b)
		     (defmacro.21-macro :z c :y b)
		     (defmacro.21-macro :z c :x a)
		     (defmacro.21-macro :z c :x a :y b)
		     (defmacro.21-macro :allow-other-keys nil)
		     (defmacro.21-macro :allow-other-keys t :foo bar))))
  ((nil nil nil)
   (a nil nil)
   (nil b nil)
   (nil nil c)
   (a b nil)
   (nil b c)
   (a nil c)
   (a b c)
   (nil nil nil)
   (nil nil nil)))

		     
		     
	    