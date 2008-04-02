;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Dec 20 05:58:06 2002
;;;; Contains: BOA Constructor Tests

(in-package :cl-test)

(defun sbt-slots (sname s &rest slots)
  (loop for slotname in slots collect
	(let ((fun (intern (concatenate 'string (string sname)
					"-" (string slotname))
			   :cl-test)))
	  (funcall (symbol-function fun) s))))

;;; See the DEFSTRUCT page, and section 3.4.6 (Boa Lambda Lists)

(defstruct* (sbt-01 (:constructor sbt-01-con (b a c)))
  a b c)

(deftest structure-boa-test-01/1
  (let ((s (sbt-01-con 1 2 3)))
    (values (sbt-01-a s)
	    (sbt-01-b s)
	    (sbt-01-c s)))
  2 1 3)

(defstruct* (sbt-02 (:constructor sbt-02-con (a b c))
		   (:constructor sbt-02-con-2 (a b))
		   (:constructor sbt-02-con-3 ()))
  (a 'x) (b 'y) (c 'z))

(deftest structure-boa-test-02/1
  (let ((s (sbt-02-con 1 2 3)))
    (values (sbt-02-a s)
	    (sbt-02-b s)
	    (sbt-02-c s)))
  1 2 3)

(deftest structure-boa-test-02/2
  (let ((s (sbt-02-con-2 'p 'q)))
    (values (sbt-02-a s)
	    (sbt-02-b s)
	    (sbt-02-c s)))
  p q z)

(deftest structure-boa-test-02/3
  (let ((s (sbt-02-con-3)))
    (values (sbt-02-a s)
	    (sbt-02-b s)
	    (sbt-02-c s)))
  x y z)

;;; &optional in BOA LL

(defstruct* (sbt-03 (:constructor sbt-03-con (a b &optional c)))
  c b a)

(deftest structure-boa-test-03/1
  (let ((s (sbt-03-con 1 2)))
    (values (sbt-03-a s) (sbt-03-b s)))
  1 2)

(deftest structure-boa-test-03/2
  (let ((s (sbt-03-con 1 2 3)))
    (values (sbt-03-a s) (sbt-03-b s) (sbt-03-c s)))
  1 2 3)


(defstruct* (sbt-04 (:constructor sbt-04-con (a b &optional c)))
  (c nil) b (a nil))

(deftest structure-boa-test-04/1
  (let ((s (sbt-04-con 1 2)))
    (values (sbt-04-a s) (sbt-04-b s) (sbt-04-c s)))
  1 2 nil)

(deftest structure-boa-test-04/2
  (let ((s (sbt-04-con 1 2 4)))
    (values (sbt-04-a s) (sbt-04-b s) (sbt-04-c s)))
  1 2 4)


(defstruct* (sbt-05 (:constructor sbt-05-con (&optional a b c)))
  (c 1) (b 2) (a 3))

(deftest structure-boa-test-05/1
  (let ((s (sbt-05-con)))
    (values (sbt-05-a s) (sbt-05-b s) (sbt-05-c s)))
  3 2 1)

(deftest structure-boa-test-05/2
  (let ((s (sbt-05-con 'x)))
    (values (sbt-05-a s) (sbt-05-b s) (sbt-05-c s)))
  x 2 1)

(deftest structure-boa-test-05/3
  (let ((s (sbt-05-con 'x 'y)))
    (values (sbt-05-a s) (sbt-05-b s) (sbt-05-c s)))
  x y 1)

(deftest structure-boa-test-05/4
  (let ((s (sbt-05-con 'x 'y 'z)))
    (values (sbt-05-a s) (sbt-05-b s) (sbt-05-c s)))
  x y z)


(defstruct* (sbt-06 (:constructor sbt-06-con (&optional (a 'p) (b 'q) (c 'r))))
  (c 1) (b 2) (a 3))

(deftest structure-boa-test-06/1
  (let ((s (sbt-06-con)))
    (values (sbt-06-a s) (sbt-06-b s) (sbt-06-c s)))
  p q r)

(deftest structure-boa-test-06/2
  (let ((s (sbt-06-con 'x)))
    (values (sbt-06-a s) (sbt-06-b s) (sbt-06-c s)))
  x q r)

(deftest structure-boa-test-06/3
  (let ((s (sbt-06-con 'x 'y)))
    (values (sbt-06-a s) (sbt-06-b s) (sbt-06-c s)))
  x y r)

(deftest structure-boa-test-06/4
  (let ((s (sbt-06-con 'x 'y 'z)))
    (values (sbt-06-a s) (sbt-06-b s) (sbt-06-c s)))
  x y z)


;;; Test presence flag in optional parameters

(defstruct* (sbt-07 (:constructor sbt-07-con
				 (&optional (a 'p a-p) (b 'q b-p) (c 'r c-p)
					    &aux (d (list (notnot a-p)
							  (notnot b-p)
							  (notnot c-p))))))
  a b c d)

(deftest structure-boa-test-07/1
  (sbt-slots 'sbt-07 (sbt-07-con) :a :b :c :d)
  (p q r (nil nil nil)))

(deftest structure-boa-test-07/2
  (sbt-slots 'sbt-07 (sbt-07-con 'x) :a :b :c :d)
  (x q r (t nil nil)))

(deftest structure-boa-test-07/3
  (sbt-slots 'sbt-07 (sbt-07-con 'x 'y) :a :b :c :d)
  (x y r (t t nil)))

(deftest structure-boa-test-07/4
  (sbt-slots 'sbt-07 (sbt-07-con 'x 'y 'z) :a :b :c :d)
  (x y z (t t t)))


;;; Keyword arguments

(defstruct* (sbt-08 (:constructor sbt-08-con
				 (&key ((:foo a)))))
  a)

(deftest structure-boa-test-08/1
  (sbt-slots 'sbt-08 (sbt-08-con :foo 10) :a)
  (10))

(defstruct* (sbt-09 (:constructor sbt-09-con
				 (&key (a 'p a-p)
				       ((:x b) 'q)
				       (c 'r)
				       d
				       ((:y e))
				       ((:z f) 's z-p)
				       &aux (g (list (notnot a-p)
						     (notnot z-p))))))
  a b c d e f g)

(deftest structure-boa-test-09/1
  (sbt-slots 'sbt-09 (sbt-09-con) :a :b :c :f :g)
  (p q r s (nil nil)))

(deftest structure-boa-test-09/2
  (sbt-slots 'sbt-09 (sbt-09-con :d 1) :a :b :c :d :f :g)
  (p q r 1 s (nil nil)))

(deftest structure-boa-test-09/3
  (sbt-slots 'sbt-09 (sbt-09-con :a 1) :a :b :c :f :g)
  (1 q r s (t nil)))

(deftest structure-boa-test-09/4
  (sbt-slots 'sbt-09 (sbt-09-con :x 1) :a :b :c :f :g)
  (p 1 r s (nil nil)))

(deftest structure-boa-test-09/5
  (sbt-slots 'sbt-09 (sbt-09-con :c 1) :a :b :c :f :g)
  (p q 1 s (nil nil)))

(deftest structure-boa-test-09/6
  (sbt-slots 'sbt-09 (sbt-09-con :y 1) :a :b :c :e :f :g)
  (p q r 1 s (nil nil)))

(deftest structure-boa-test-09/7
  (sbt-slots 'sbt-09 (sbt-09-con :z 1) :a :b :c :f :g)
  (p q r 1 (nil t)))

;;; Aux variable overriding a default value

(defstruct* (sbt-10 (:constructor sbt-10-con (&aux (a 10)
						  (b (1+ a)))))
  (a 1) (b 2))

(deftest structure-boa-test-10/1
  (sbt-slots 'sbt-10 (sbt-10-con) :a :b)
  (10 11))

;;; Aux variables with no value

(defstruct* (sbt-11 (:constructor sbt-11-con (&aux a b)))
  a (b 0 :type integer))

(deftest structure-boa-test-11/1
  (let ((s (sbt-11-con)))
    (setf (sbt-11-a s) 'p)
    (setf (sbt-11-b s) 10)
    (sbt-slots 'sbt-11 s :a :b))
  (p 10))

;;; Arguments that correspond to no slots

(defstruct* (sbt-12 (:constructor sbt-12-con (a &optional (b 1)
					       &rest c
					       &aux (d (list a b c)))))
  d)

(deftest structure-boa-12/1
  (sbt-12-d (sbt-12-con 'x))
  (x 1 nil))

(deftest structure-boa-12/2
  (sbt-12-d (sbt-12-con 'x 'y))
  (x y nil))

(deftest structure-boa-12/3
  (sbt-12-d (sbt-12-con 'x 'y 1 2 3))
  (x y (1 2 3)))


(defstruct* (sbt-13 (:constructor sbt-13-con
				 (&key (a 1) (b 2) c &aux (d (list a b c)))))
  d)

(deftest structure-boa-test-13/1
  (sbt-13-d (sbt-13-con))
  (1 2 nil))

(deftest structure-boa-test-13/2
  (sbt-13-d (sbt-13-con :a 10))
  (10 2 nil))

(deftest structure-boa-test-13/3
  (sbt-13-d (sbt-13-con :b 10))
  (1 10 nil))

(deftest structure-boa-test-13/4
  (sbt-13-d (sbt-13-con :c 10))
  (1 2 10))

(deftest structure-boa-test-13/5
  (sbt-13-d (sbt-13-con :c 10 :a 3))
  (3 2 10))

(deftest structure-boa-test-13/6
  (sbt-13-d (sbt-13-con :c 10 :b 3))
  (1 3 10))

(deftest structure-boa-test-13/7
  (sbt-13-d (sbt-13-con :a 10 :b 3))
  (10 3 nil))

(deftest structure-boa-test-13/8
  (sbt-13-d (sbt-13-con :a 10 :c 'a :b 3))
  (10 3 a))


;;; Allow other keywords

(defstruct* (sbt-14 (:constructor sbt-14-con (&key a b c &allow-other-keys)))
  (a 1) (b 2) (c 3))

(deftest structure-boa-test-14/1
  (sbt-slots 'sbt-14 (sbt-14-con) :a :b :c)
  (1 2 3))

(deftest structure-boa-test-14/2
  (sbt-slots 'sbt-14 (sbt-14-con :a 9) :a :b :c)
  (9 2 3))

(deftest structure-boa-test-14/3
  (sbt-slots 'sbt-14 (sbt-14-con :b 9) :a :b :c)
  (1 9 3))

(deftest structure-boa-test-14/4
  (sbt-slots 'sbt-14 (sbt-14-con :c 9) :a :b :c)
  (1 2 9))

(deftest structure-boa-test-14/5
  (sbt-slots 'sbt-14 (sbt-14-con :d 9) :a :b :c)
  (1 2 3))

;;; Keywords are in the correct package, and slot names are not
;;; keyword parameters if not specified.

(defstruct* (sbt-15 (:constructor sbt-15-con
				  (&key ((:x a) nil)
					((y  b) nil)
					(c nil))))
  a b c)

(deftest structure-boa-test-15/1
  (sbt-slots 'sbt-15 (sbt-15-con :x 1 'y 2 :c 3) :a :b :c)
  (1 2 3))

(deftest structure-boa-test-15/2
  (signals-error (sbt-15-con :a 1) program-error)
  t)

(deftest structure-boa-test-15/3
  (signals-error (sbt-15-con :b 1) program-error)
  t)

(deftest structure-boa-test-15/4
  (signals-error (sbt-15-con 'x 1) program-error)
  t)

(deftest structure-boa-test-15/5
  (signals-error (sbt-15-con :y 1) program-error)
  t)

(deftest structure-boa-test-15/6
  (signals-error (sbt-15-con 'c 1) program-error)
  t)

(deftest structure-boa-test-15/7
  (signals-error (sbt-15-con 'a 1) program-error)
  t)

(deftest structure-boa-test-15/8
  (signals-error (sbt-15-con 'b 1) program-error)
  t)


;;; Default constructor w. BOA constructor, and error cases

(defstruct* (sbt-16 (:constructor)
		    (:constructor sbt-16-con (a b c)))
   a b c)

(deftest structure-boa-test-16/1
  (sbt-slots 'sbt-16 (make-sbt-16 :a 1 :b 2 :c 3) :a :b :c)
  (1 2 3))

(deftest structure-boa-test-16/2
  (sbt-slots 'sbt-16 (sbt-16-con 4 5 6) :a :b :c)
  (4 5 6))

(deftest structure-boa-test-16/3
  (signals-error (make-sbt-16 :d 1) program-error)
  t)

(deftest structure-boa-test-16/4
  (signals-error (make-sbt-16 :a) program-error)
  t)

(deftest structure-boa-test-16/5
  (signals-error (make-sbt-16 'a) program-error)
  t)

(deftest structure-boa-test-16/6
  (signals-error (make-sbt-16 1 1) program-error)
  t)

(deftest structure-boa-test-16/7
  (sbt-slots 'sbt-16 (make-sbt-16 :a 1 :b 2 :c 3 :d 5 :allow-other-keys t)
	     :a :b :c)
  (1 2 3))

(deftest structure-boa-test-16/8
  (sbt-slots 'sbt-16 (make-sbt-16 :allow-other-keys t :a 1 :b 2 :c 3 :d 5)
	     :a :b :c)
  (1 2 3))

;;; :allow-other-keys turns off keyword error checking, including
;;; invalid (nonsymbol) keyword arguments
;;;(deftest structure-boa-test-16/9
;;;  (sbt-slots 'sbt-16 (make-sbt-16 :allow-other-keys t
;;;                                  :a 3 :b 6 :c 9 1000 1000)
;;;	     :a :b :c)
;;;  (3 6 9))

;;; Repeated keyword arguments are allowed; the leftmost one is used
(deftest structure-boa-test-16/10
  (sbt-slots 'sbt-16 (make-sbt-16 :a 1 :a 2 :b 3 :b 4 :c 5 :c 6) :a :b :c)
  (1 3 5))

(deftest structure-boa-test-16/11
  (sbt-slots 'sbt-16 (make-sbt-16 :allow-other-keys t 
				  :allow-other-keys nil
				  :a 1 :b 2 :c 3 :d 5)
	     :a :b :c)
  (1 2 3))

;; Checking of # of keywords is suppressed when :allow-other-keys is true
;;;(deftest structure-boa-test-16/12
;;;  (sbt-slots 'sbt-16 (make-sbt-16 :allow-other-keys t :a 3 :b 6 :c 9 :a)
;;;	     :a :b :c)
;;;  (3 6 9))


;;; Error test

(def-macro-test struct.error.1 (defstruct nonexistent-structure-type a b c))
