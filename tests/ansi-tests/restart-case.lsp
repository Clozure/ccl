;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 22 06:58:03 2003
;;;; Contains: Tests for RESTART-CASE

(in-package :cl-test)

(deftest restart-case.1
  (restart-case (values)))

(deftest restart-case.2
  (restart-case 1)
  1)

(deftest restart-case.3
  (restart-case (values 'a 'b 'c 'd 'e 'f))
  a b c d e f)

(deftest restart-case.4
  (restart-case (progn (invoke-restart 'foo) 'bad)
		(foo () 'good))
  good)

(deftest restart-case.5
  (restart-case (progn (invoke-restart 'foo) 'bad)
		(foo ()))
  nil)

(deftest restart-case.6
  (restart-case
   (progn (invoke-restart 'foo) 'bad)
   (bar () 'bad2)
   (foo () 'good)
   (foo () 'bad3))
  good)

(deftest restart-case.7
  (restart-case
   (invoke-restart 'foo 'a 'b 'c 'd)
   (foo (w x y z) (list z y x w)))
  (d c b a))

(deftest restart-case.8
  (restart-case
   (invoke-restart 'foo :a 1 :b 2)
   (foo (&key a b c d) (list a b c d)))
  (1 2 nil nil))

(deftest restart-case.9
  (restart-case
   (invoke-restart 'foo 1 2 3 4)
   (foo (&rest args) (reverse args)))
  (4 3 2 1))

(deftest restart-case.10
  (restart-case
   (invoke-restart 'foo 1 2 3)
   (foo (a b &optional c d) (list a b c d)))
  (1 2 3 nil))

(deftest restart-case.11
  (restart-case
   (invoke-restart 'foo 1 2)
   (foo (x y) (declare (type fixnum x y)) (+ x y)))
  3)

(deftest restart-case.12
  (restart-case
   (restart-case (invoke-restart 'foo 1)
		 (foo (x) (invoke-restart 'foo (1+ x))))
   (foo (y) (+ 4 y)))
  6)

(deftest restart-case.13
  (let ((i 10))
    (values
     (restart-case (progn (invoke-restart 'foo) 'bad)
		   (foo () (incf i 100) 'good))
     i))
  good 110)

(deftest restart-case.14
  (restart-case
   (invoke-restart 'foo 1 2)
   (foo (x y)
	(declare (type fixnum x))
	(declare (type fixnum y))
	(+ x y)))
  3)

(deftest restart-case.15
  (restart-case
   (invoke-restart 'foo 1 2)
   (foo (x y)
	(declare (ignore x y))
	(declare (type fixnum x))
	(declare (type fixnum y))))
  nil)

(deftest restart-case.16
  (restart-case
   (invoke-restart 'foo)
   (foo () (values))))

(deftest restart-case.17
  (restart-case
   (invoke-restart 'foo)
   (foo () (values 'a 'b 'c 'd 'e 'f)))
  a b c d e f)

(deftest restart-case.18
  (restart-case
   (invoke-restart 'foo)
   (foo () :test (lambda (c) (declare (ignore c)) t) 'good))
  good)

(deftest restart-case.19
  (restart-case
   (invoke-restart 'foo)
   (foo () :test (lambda (c) (declare (ignore c)) nil) 'bad)
   (foo () 'good))
  good)

(deftest restart-case.20
  (with-output-to-string
    (s)
    (restart-case
     (let ((restart (find-restart 'foo))
	   (*print-escape* nil))
       (format s "~A" restart))
     (foo () :report "A report")))
  "A report")

(deftest restart-case.21
  (with-output-to-string
    (s)
    (flet ((%f (s2) (format s2 "A report")))
      (restart-case
       (let ((restart (find-restart 'foo))
	     (*print-escape* nil))
	 (format s "~A" restart))
       (foo () :report %f))))
  "A report")

(deftest restart-case.22
  (with-output-to-string
    (s)
    (restart-case
     (let ((restart (find-restart 'foo))
	   (*print-escape* nil))
       (format s "~A" restart))
     (foo () :report (lambda (s2) (format s2 "A report")))))
  "A report")

;;; Special cases when restart-case associates the restarts with
;;; a condition

(deftest restart-case.23
  (handler-bind
   ((error #'(lambda (c) (declare (ignore c)) (invoke-restart 'foo))))
   (restart-case
    (error "Boo!")
    (foo () 'good)))
  good)

(deftest restart-case.24
  (handler-bind
   ((error #'(lambda (c) (invoke-restart (find-restart 'foo c)))))
   (restart-case
    (error "Boo!")
    (foo () 'good)))
  good)


;;; Test that the inner restart-case has associated its restart with
;;; the condition to be raised by the error form.

(deftest restart-case.25
  (handler-bind
   ((error #'(lambda (c2) (invoke-restart (find-restart 'foo c2)))))
   (handler-bind
    ((error #'(lambda (c) (declare (ignore c)) (error "Blah"))))
    (restart-case
     (restart-case
      (error "Boo!")
      (foo () 'bad))
     (foo () 'good))))
  good)

(deftest restart-case.26
  (handler-bind
   ((error #'(lambda (c2) (invoke-restart (find-restart 'foo c2)))))
   (handler-bind
    ((simple-condition #'(lambda (c) (declare (ignore c)) (error "Blah"))))
    (restart-case
     (restart-case
      (signal "Boo!")
      (foo () 'bad))
     (foo () 'good))))
  good)

(deftest restart-case.27
  (handler-bind
   ((error #'(lambda (c2) (invoke-restart (find-restart 'foo c2)))))
   (handler-bind
    ((error #'(lambda (c) (declare (ignore c)) (error "Blah"))))
    (restart-case
     (restart-case
      (cerror "" "")
      (foo () 'bad))
     (foo () 'good))))
  good)

(deftest restart-case.28
  (handler-bind
   ((error #'(lambda (c2) (invoke-restart (find-restart 'foo c2)))))
   (handler-bind
    ((warning #'(lambda (c) (declare (ignore c)) (error "Blah"))))
    (restart-case
     (restart-case
      (warn "Boo!")
      (foo () 'bad))
     (foo () 'good))))
  good)

(deftest restart-case.29
  (macrolet ((%m (&rest args) (cons 'error args)))
    (handler-bind
     ((error #'(lambda (c2) (invoke-restart (find-restart 'foo c2)))))
     (handler-bind
      ((error #'(lambda (c) (declare (ignore c)) (error "Blah"))))
      (restart-case
       (restart-case
	(%m "Boo!")
	(foo () 'bad))
       (foo () 'good)))))
  good)

(deftest restart-case.30
  (symbol-macrolet ((%s (error "Boo!")))
    (handler-bind
     ((error #'(lambda (c2) (invoke-restart (find-restart 'foo c2)))))
     (handler-bind
      ((error #'(lambda (c) (declare (ignore c)) (error "Blah"))))
      (restart-case
       (restart-case
	%s
	(foo () 'bad))
       (foo () 'good)))))
  good)

(deftest restart-case.31
  (macrolet ((%m2 (&rest args) (cons 'error args)))
    (macrolet ((%m (&rest args &environment env)
		   (macroexpand (cons '%m2 args) env)))
      (handler-bind
       ((error #'(lambda (c2) (invoke-restart (find-restart 'foo c2)))))
       (handler-bind
	((error #'(lambda (c) (declare (ignore c)) (error "Blah"))))
	(restart-case
	 (restart-case
	  (%m "Boo!")
	  (foo () 'bad))
	 (foo () 'good))))))
  good)

(deftest restart-case.32
  (restart-case
   (invoke-restart-interactively 'foo)
   (foo () 'good))
  good)

(deftest restart-case.33
  (restart-case
   (invoke-restart-interactively 'foo)
   (foo (w x y z)
	:interactive (lambda () (list 'a 'b 'c 'd))
	(list x w z y)))
  (b a d c))

(deftest restart-case.34
  (flet ((%f () (list 'a 'b 'c 'd)))
    (restart-case
     (invoke-restart-interactively 'foo)
     (foo (w x y z)
	  :interactive %f
	  (list x w z y))))
  (b a d c))

(deftest restart-case.35
  (restart-case
   (loop for i from 1 to 4
	 for r in (compute-restarts)
	 collect (restart-name r))
   (foo () t)
   (bar () t)
   (foo () 'a)
   (nil () :report (lambda (s) (format s "Anonymous restart"))  10))
  (foo bar foo nil))

(deftest restart-case.36
  (let ((x :bad))
    (declare (special x))
    (let ((x :good))
      (restart-case
       (invoke-restart 'foo)
       (foo (&aux (y x))
	    (declare (special x))
	    y))))
  :good)