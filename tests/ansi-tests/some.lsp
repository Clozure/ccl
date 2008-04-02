;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 07:07:07 2002
;;;; Contains: Tests for SOME

(in-package :cl-test)

(deftest some.1
  (some #'identity nil)
  nil)

(deftest some.2
  (some #'identity #())
  nil)

(deftest some.3
  (let ((count 0))
    (values
     (some #'(lambda (x) (incf count) (if (>= x 10) x nil))
	    '(1 2 4 13 5 1))
     count))
  13 4)

(deftest some.4
  (some #'/= '(1 2 3 4) '(1 2 3 4 5))
  nil)

(deftest some.5
  (some #'/= '(1 2 3 4 5) '(1 2 3 4))
  nil)

(deftest some.6
  (not-mv (some #'/= '(1 2 3 4 5) '(1 2 3 4 6)))
  nil)

(deftest some.7
  (some #'(lambda (x y) (and x y))
	'(nil t t nil t) #(t nil nil t nil nil))
  nil)

(deftest some.8
  (let ((x '(1))
	(args nil))
    (loop for i from 1 below (1- (min 100 call-arguments-limit))
	  do (push x args)
	  always (apply #'some #'/= args)))
  nil)

(deftest some.9
  (some #'zerop #*11111111111111)
  nil)

(deftest some.10
  (some #'zerop #*)
  nil)

(deftest some.11
  (not-mv (some #'zerop #*1111111011111))
  nil)

(deftest some.12
  (some #'(lambda (x) (not (eql x #\a))) "aaaaaaaa")
  nil)

(deftest some.13
  (some #'(lambda (x) (eql x #\a)) "")
  nil)

(deftest some.14
  (not-mv (some #'(lambda (x) (not (eql x #\a))) "aaaaaabaaaa"))
  nil)

(deftest some.15
  (some 'null '(1 2 3 4))
  nil)

(deftest some.16
  (not-mv (some 'null '(1 2 3 nil 5)))
  nil)

;;; Other specialized sequences

(deftest some.17
  (let ((v (make-array '(10) :initial-contents '(0 0 0 0 1 2 3 4 5 6)
		       :fill-pointer 4)))
    (loop for j from 0 to 9
	  do (setf (fill-pointer v) j)
	  collect (notnot (some #'plusp v))))
  (nil nil nil nil nil t t t t t))

(deftest some.18
  (loop for i from 1 to 40
	for type = `(unsigned-byte ,i)
	unless
	(let ((v (make-array '(10) :initial-contents (loop for j in '(0 0 0 0 1 2 3 4 5 6)
							   collect (mod j (ash 1 i)))
			     :element-type type
			     :fill-pointer 4)))
	  (equal (loop for j from 0 to 9
		       do (setf (fill-pointer v) j)
		       collect (notnot (some #'plusp v)))
		 '(nil nil nil nil nil t t t t t)))
	collect i)
  nil)

(deftest some.19
  (loop for i from 1 to 40
	for type = `(signed-byte ,i)
	unless
	(let ((v (make-array '(10) :initial-contents '(0 0 0 0 -1 -1 -1 -1 -1 -1)
			     :element-type type
			     :fill-pointer 4)))
	  (equal (loop for j from 0 to 9
		       do (setf (fill-pointer v) j)
		       collect (notnot (some #'minusp v)))
		 '(nil nil nil nil nil t t t t t)))
	collect i)
  nil)

(deftest some.20
  (let ((v (make-array '(10) :initial-contents "abcd012345"
		       :element-type 'character
		       :fill-pointer 4)))
    (loop for j from 0 to 9
	  do (setf (fill-pointer v) j)
	  collect (notnot (some #'digit-char-p v))))
  (nil nil nil nil nil t t t t t))

(deftest some.21
  (let ((v (make-array '(10) :initial-contents "abcd012345"
		       :element-type 'base-char
		       :fill-pointer 4)))
    (loop for j from 0 to 9
	  do (setf (fill-pointer v) j)
	  collect (notnot (some #'digit-char-p v))))
  (nil nil nil nil nil t t t t t))

(deftest some.22
  (let ((v (make-array '(5) :initial-contents "abcde"
		       :element-type 'base-char)))
    (values
     (some #'digit-char-p v)
     (setf (aref v 2) #\0)
     (notnot (some #'digit-char-p v))))
  nil #\0 t)

(deftest some.23
  (loop for type in '(short-float single-float double-float long-float)
	for v = (make-array '(9)
			    :element-type type
			    :initial-contents
			    (mapcar #'(lambda (x) (coerce x type)) '(1 2 3 4 5 6 0 8 3)))
	unless (some #'zerop v)
	collect (list type v))
  nil)

(deftest some.24
  (loop for type in '(short-float single-float double-float long-float)
	for v = (make-array '(9)
			    :element-type type
			    :fill-pointer 6
			    :initial-contents
			    (mapcar #'(lambda (x) (coerce x type)) '(1 2 3 4 5 6 0 8 3)))
	when (some #'zerop v)
	collect (list type v))
  nil)

(deftest some.25
  (loop for type in '(short-float single-float double-float long-float)
	for ctype = `(complex ,type)
	for v = (make-array '(6)
			    :element-type ctype
			    :initial-contents
			    (mapcar #'(lambda (x) (complex x (coerce x type))) '(1 2 3 4 5 6)))
	when (some (complement #'complexp) v)
	collect (list type v))
  nil)

;;; Displaced vectors

(deftest some.26
  (let* ((v1 (make-array '(10) :initial-contents '(1 3 2 4 6 8 5 7 9 1)))
	 (v2 (make-array '(4) :displaced-to v1
			 :displaced-index-offset 2)))
    (values
     (notnot (some #'oddp v1))
     (some #'oddp v2)))
  t nil)

(deftest some.27
  (loop for i from 1 to 40
	for type = `(unsigned-byte ,i)
	unless
	(let* ((v1 (make-array '(10) :initial-contents '(1 1 0 0 0 0 1 1 1 1)
			       :element-type type))
	       (v2 (make-array '(4) :displaced-to v1
			       :displaced-index-offset 2
			       :element-type type)))
	  (and (some 'oddp v1))
	       (not (some #'oddp v2)))
	collect i)
  nil)

(deftest some.28
  (loop for i from 1 to 40
	for type = `(signed-byte ,i)
	unless
	(let* ((v1 (make-array '(10) :initial-contents '(-1 -1 0 0 0 0 -1 -1 -1 -1)
			       :element-type type))
	       (v2 (make-array '(4) :displaced-to v1
			       :displaced-index-offset 2
			       :element-type type)))
	  (and (some 'oddp v1)
	       (not (some #'oddp v2))))
	collect i)
  nil)

(deftest some.29
  (let* ((s1 (make-array '(8) :initial-contents "12abc345" :element-type 'character)))
    (loop for i from 0 to 6
	  for s2 = (make-array '(2) :element-type 'character
			       :displaced-to s1
			       :displaced-index-offset i)
	  collect (notnot (some 'digit-char-p s2))))
  (t t nil nil t t t))

(deftest some.30
  (let* ((s1 (make-array '(8) :initial-contents "12abc345" :element-type 'base-char)))
    (loop for i from 0 to 6
	  for s2 = (make-array '(2) :element-type 'base-char
			       :displaced-to s1
			       :displaced-index-offset i)
	  collect (notnot (some 'digit-char-p s2))))
  (t t nil nil t t t))

(deftest some.31
  (let ((v (make-array '(10) :initial-contents '(1 2 3 4 5 6 7 8 9 10)
		       :adjustable t)))
    (values
     (some #'minusp v)
     (progn
       (adjust-array v '(11) :initial-element -1)
       (notnot (some #'minusp v)))))
  nil t)

(deftest some.32
  (let ((v (make-array '(10) :initial-contents '(1 2 3 4 5 6 7 8 9 10)
		       :fill-pointer 10
		       :adjustable t)))
    (values
     (some #'minusp v)
     (progn
       (adjust-array v '(11) :initial-element -1)
       (some #'minusp v))))
  nil nil)

(deftest some.order.1
  (let ((i 0) x y)
    (values
     (some (progn (setf x (incf i)) #'null)
	   (progn (setf y (incf i)) '(a b c d)))
     i x y))
  nil 2 1 2)

(deftest some.order.2
  (let ((i 0) x y z)
    (values
     (some (progn (setf x (incf i)) #'eq)
	   (progn (setf y (incf i)) '(a b c d))
	   (progn (setf z (incf i)) '(e f g h)))
     i x y z))
  nil 3 1 2 3)


(deftest some.error.1
  (check-type-error #'(lambda (x) (some x '(a b c)))
		    (typef '(or symbol function)))
  nil)

(deftest some.error.4
  (check-type-error #'(lambda (x) (some #'null x)) #'sequencep)
  nil)

(deftest some.error.7
  (check-type-error #'(lambda (x) (some #'eql () x)) #'sequencep)
  nil)

(deftest some.error.8
  (signals-error (some) program-error)
  t)

(deftest some.error.9
  (signals-error (some #'null) program-error)
  t)

(deftest some.error.10
  (signals-error (locally (some 1 '(a b c)) t) type-error)
  t)

(deftest some.error.11
  (signals-error (some #'cons '(a b c)) program-error)
  t)

(deftest some.error.12
  (signals-error (some #'car '(a b c)) type-error)
  t)

(deftest some.error.13
  (signals-error (some #'cons '(a b c) '(b c d) '(c d e)) program-error)
  t)

(deftest some.error.14
  (signals-error (some #'null '(a b . c)) type-error)
  t)

