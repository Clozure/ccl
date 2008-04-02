;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Nov 27 06:43:21 2002
;;;; Contains: Tests of LAMBDA forms

(in-package :cl-test)

(deftest lambda.1
  ((lambda (x) x) 'a)
  a)

(deftest lambda.2
  ((lambda () 'a))
  a)

(deftest lambda.3
  ((lambda () "documentation" 'a))
  a)

(deftest lambda.4
  ((lambda (x) (declare (type symbol x)) x) 'z)
  z)

(deftest lambda.5
  ((lambda (&aux (x 'a)) x))
  a)

(deftest lambda.6
  ((lambda (&aux (x 'a)) (declare (type symbol x)) x))
  a)

(deftest lambda.7
  ((lambda () "foo"))
  "foo")

(deftest lambda.8
  ((lambda () "foo" "bar"))
  "bar")

(deftest lambda.9
  ((lambda (x y) (declare (ignore x)) "foo" (declare (ignore y)) "bar") 1 2)
  "bar")

(deftest lambda.10
  ((lambda (x) (declare (type symbol x) (ignorable x))) 'z)
  nil)

(deftest lambda.11
  ((lambda (x &optional y z) (list x y z)) 1 2)
  (1 2 nil))

(deftest lambda.12
  ((lambda (&optional (x 'a) (y 'b) (z 'c)) (list x y z)) 1 nil)
  (1 nil c))

(deftest lambda.13
  ((lambda (&optional (x 'a x-p) (y 'b y-p) (z 'c z-p))
     (list* x y z (mapcar #'notnot (list x-p y-p z-p)))) 1 nil)
  (1 nil c t t nil))

(deftest lambda.14
  (let ((x 1))
    ((lambda (&optional (x (1+ x))) x)))
  2)

(deftest lambda.15
  ((lambda (y &optional (x (1+ y))) (list y x)) 10)
  (10 11))

(deftest lambda.16
  ((lambda (y &optional (x (1+ y))) (list y x)) 10 14)
  (10 14))

(deftest lambda.17
  ((lambda (&rest x) x) 1 2 3)
  (1 2 3))

(deftest lambda.18
  (let ((b 10))
    ((lambda (&optional (a b) (b (1+ a))) (list a b)) 3 7))
  (3 7))

(deftest lambda.19
  (let ((b 10))
    ((lambda (&optional (a b) (b (1+ a))) (list a b)) 3))
  (3 4))

(deftest lambda.20
  (let ((b 10))
    ((lambda (&optional (a b) (b (1+ a))) (list a b))))
  (10 11))

(deftest lambda.21
  (flet ((%f () (locally (declare (special *x*)) (incf *x*))))
    ((lambda (*x*)
       (declare (special *x*))
       (%f)
       *x*)
     10))
  11)

(deftest lambda.22
  (flet ((%f () (locally (declare (special *x*)) (1+ *x*))))
    ((lambda (*x*)
       (declare (special *x*))
       (%f))
     15))
  16)

(deftest lambda.23
  ((lambda (&key a) a))
  nil)

(deftest lambda.24
  ((lambda (&key a b c) (list a b c)))
  (nil nil nil))

(deftest lambda.25
  ((lambda (&key (a 1) (b 2) (c 3)) (list a b c)))
  (1 2 3))

(deftest lambda.26
  ((lambda (&key)))
  nil)

(deftest lambda.27
  ((lambda (&key) 'good) :allow-other-keys nil)
  good)

(deftest lambda.28
  ((lambda (&key) 'good) :allow-other-keys t :foo t)
  good)

(deftest lambda.29
  ((lambda (&key) 'good) :allow-other-keys t :allow-other-keys nil :foo t)
  good)

(deftest lambda.30
  ((lambda (&key x) x) :allow-other-keys t :x 10
   :allow-other-keys nil :foo t)
  10)

(deftest lambda.31
  ((lambda (&rest x &key) x))
  nil)

(deftest lambda.32
  ((lambda (&rest x &key) x) :allow-other-keys nil)
  (:allow-other-keys nil))

(deftest lambda.33
  ((lambda (&rest x &key) x) :w 5 :allow-other-keys t :x 10)
  (:w 5 :allow-other-keys t :x 10))

(deftest lambda.34
  ((lambda (&key (a 1 a-p) (b 2 b-p) (c 3 c-p)) (list a (notnot a-p)
						      b (notnot b-p)
						      c (notnot c-p)))
   :c 5 :a 0)
  (0 t 2 nil 5 t))

(deftest lambda.35
  ((lambda (&key (a 1 a-p) (b 2 b-p) (c 3 c-p)) (list a (notnot a-p)
						      b (notnot b-p)
						      c (notnot c-p)))
   :c 5 :a nil :a 17 :c 100)
  (nil t 2 nil 5 t))

(deftest lambda.36
  ((lambda (&key (a 1 a-p) (b 2 b-p) (c 3 c-p)) (list a (notnot a-p)
						      b (notnot b-p)
						      c (notnot c-p)))
   :c 5 :a 0 :allow-other-keys t 'b 100)
  (0 t 2 nil 5 t))

(deftest lambda.37
  (let ((b 1))
    ((lambda (&key (a b) b) (list a b)) :b 'x))
  (1 x))

(deftest lambda.38
  (let ((b 1))
    ((lambda (&key (a b) b) (list a b)) :b 'x :a nil))
  (nil x))

(deftest lambda.39
  (let ((a-p :bad))
    (declare (ignorable a-p))
    ((lambda (&key (a nil a-p) (b a-p)) (list a (notnot a-p) (notnot b)))))
  (nil nil nil))
     
(deftest lambda.40
  (let ((a-p :bad))
    (declare (ignorable a-p))
    ((lambda (&key (a nil a-p) (b a-p)) (list a (notnot a-p) (notnot b)))
     :a 1))
  (1 t t))

(deftest lambda.41
  (let ((a-p :bad))
    (declare (ignorable a-p))
    ((lambda (&key (a nil a-p) (b a-p)) (list a (notnot a-p) (notnot b)))
     :a nil))
  (nil t t))

(deftest lambda.42
  ((lambda (&key a b &allow-other-keys) (list a b)) :a 1 :b 2)
  (1 2))

(deftest lambda.43
  ((lambda (&key a b &allow-other-keys) (list a b)) :b 2 :a 1)
  (1 2))

(deftest lambda.44
  ((lambda (&key a b &allow-other-keys) (list a b)) :z 10 :b 2 :b nil :a 1
   :a 2 'x 100)
  (1 2))

(deftest lambda.45
  ((lambda (&key a b &allow-other-keys) (list a b)) :allow-other-keys nil
   :z 10 :b 2 :b nil :a 1 :a 2 'x 100)
  (1 2))

(deftest lambda.46
  ((lambda (&key a b allow-other-keys) (list allow-other-keys a b))
   :allow-other-keys nil :a 1 :b 2)
  (nil 1 2))

(deftest lambda.47
  ((lambda (&key a b allow-other-keys) (list allow-other-keys a b))
   :c 10 :allow-other-keys t :a 1 :b 2 :d 20)
  (t 1 2))

(deftest lambda.48
  ((lambda (&key a b allow-other-keys &allow-other-keys)
     (list allow-other-keys a b))
   :d 40 :allow-other-keys nil :a 1 :b 2 :c 20)
  (nil 1 2))

(deftest lambda.49
  ((lambda (&key a b allow-other-keys &allow-other-keys)
     (list allow-other-keys a b))
   :d 40 :a 1 :b 2 :c 20)
  (nil 1 2))

(deftest lambda.50
  ((lambda (&key a b ((:allow-other-keys aok)))
     (list aok a b))
   :d 40 :a 1 :allow-other-keys t :b 2 :c 20)
  (t 1 2))

(deftest lambda.51
  ((lambda (&key &allow-other-keys)) :a 1 :b 2 :c 3)
  nil)

;;; Free declaration scope

(deftest lambda.52
  (let ((x :bad))
    (declare (special x))
    (let ((x :good))
      ((lambda (&optional (y x)) (declare (special x)) y))))
  :good)

(deftest lambda.53
  (let ((x :bad))
    (declare (special x))
    (let ((x :good))
      ((lambda (&key (y x)) (declare (special x)) y))))
  :good)

(deftest lambda.54
  (let ((x :bad))
    (declare (special x))
    (let ((x :good))
      ((lambda (&aux (y x)) (declare (special x)) y))))
  :good)

(deftest lambda.55
  (let* ((doc "LMB55")
	 (fn (eval `#'(lambda () ,doc nil)))
	 (cfn (compile nil fn)))
    (values
     (or (documentation fn t) doc)
     (or (documentation cfn t) doc)))
  "LMB55"
  "LMB55")

(deftest lambda.56
  (let* ((doc "LMB56")
	 (fn (eval `#'(lambda () ,doc nil)))
	 (cfn (compile nil fn)))
    (values
     (or (documentation fn 'function) doc)
     (or (documentation cfn 'function) doc)))
  "LMB56"
  "LMB56")

;;; Uninterned symbols as lambda variables

(deftest lambda.57
  ((lambda (#1=#:foo) #1#) 17)
  17)

(deftest lambda.58
  ((lambda (&rest #1=#:foo) #1#) 'a 'b 'c)
  (a b c))

(deftest lambda.59
  ((lambda (&optional #1=#:foo) #1#))
  nil)

(deftest lambda.60
  ((lambda (&optional (#1=#:foo t)) #1#))
  t)

(deftest lambda.61
  ((lambda (&optional (#1=#:foo t)) #1#) 'bar)
  bar)

(deftest lambda.62
  ((lambda (&key #1=#:foo) #1#) :foo 12)
  12)

;;; Test that declarations for aux variables are handled properly

(deftest lambda.63
  (let ((y :bad1))
    (declare (ignore y))
    (let ((y :bad2))
      (declare (special y))
      (flet ((%f () y))
	((lambda (x &aux (y :good))
	   (declare (special y) (ignore x))
	   (%f))
	 nil))))
  :good)

(deftest lambda.64
  (let ((x :bad))
    (declare (special x))
    (flet ((%f () x))
      ((lambda (x &aux (y (%f)))
	 (declare (type t y) (special x))
	 y)
       :good)))
  :good)

;;; Tests of lambda as a macro

(deftest lambda.macro.1
  (notnot (macro-function 'lambda))
  t)

(deftest lambda.macro.2
  (funcall (eval (macroexpand '(lambda () 10))))
  10)

;;; Error tests

(deftest lambda.error.1
  (signals-error (funcall (macro-function 'lambda))
		 program-error)
  t)

(deftest lambda.error.2
  (signals-error (funcall (macro-function 'lambda) '(lambda ()))
		 program-error)
  t)

(deftest lambda.error.3
  (signals-error (funcall (macro-function 'lambda) '(lambda ()) nil nil)
		 program-error)
  t)
