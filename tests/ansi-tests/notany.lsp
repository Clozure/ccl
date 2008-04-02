;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 07:14:14 2002
;;;; Contains: Tests for NOTANY

(in-package :cl-test)

(deftest notany.1
  (not-mv (notany #'identity nil))
  nil)

(deftest notany.2
  (not-mv (notany #'identity #()))
  nil)

(deftest notany.3
  (let ((count 0))
    (values
     (notany #'(lambda (x) (incf count) (if (>= x 10) x nil))
	    '(1 2 4 13 5 1))
     count))
  nil 4)

(deftest notany.4
  (not-mv (notany #'/= '(1 2 3 4) '(1 2 3 4 5)))
  nil)

(deftest notany.5
  (not-mv (notany #'/= '(1 2 3 4 5) '(1 2 3 4)))
  nil)

(deftest notany.6
  (notany #'/= '(1 2 3 4 5) '(1 2 3 4 6))
  nil)

(deftest notany.7
  (not-mv (notany #'(lambda (x y) (and x y))
	       '(nil t t nil t) #(t nil nil t nil nil)))
  nil)

(deftest notany.8
  (let* ((x '(1))
	 (args (list x)))
    (not
     (loop for i from 2 below (1- (min 100 call-arguments-limit))
	   do (push x args)
	   always (apply #'notany #'/= args))))
  nil)

(deftest notany.9
  (not-mv (notany #'zerop #*11111111111111))
  nil)

(deftest notany.10
  (not-mv (notany #'zerop #*))
  nil)

(deftest notany.11
  (notany #'zerop #*1111111011111)
  nil)

(deftest notany.12
  (not-mv (notany #'(lambda (x) (not (eql x #\a))) "aaaaaaaa"))
  nil)

(deftest notany.13
  (not-mv (notany #'(lambda (x) (eql x #\a)) ""))
  nil)

(deftest notany.14
  (notany #'(lambda (x) (not (eql x #\a))) "aaaaaabaaaa")
  nil)

(deftest notany.15
  (not-mv (notany 'null '(1 2 3 4)))
  nil)

(deftest notany.16
  (notany 'null '(1 2 3 nil 5))
  nil)

;;; Other specialized sequences

(deftest notany.17
  (let ((v (make-array '(10) :initial-contents '(0 0 0 0 1 2 3 4 5 6)
		       :fill-pointer 4)))
    (loop for j from 0 to 9
	  do (setf (fill-pointer v) j)
	  collect (not (notany #'plusp v))))
  (nil nil nil nil nil t t t t t))

(deftest notany.18
  (loop for i from 1 to 40
	for type = `(unsigned-byte ,i)
	unless
	(let ((v (make-array '(10) :initial-contents (loop for j in '(0 0 0 0 1 2 3 4 5 6)
							   collect (mod j (ash 1 i)))
			     :element-type type
			     :fill-pointer 4)))
	  (equal (loop for j from 0 to 9
		       do (setf (fill-pointer v) j)
		       collect (not (notany #'plusp v)))
		 '(nil nil nil nil nil t t t t t)))
	collect i)
  nil)

(deftest notany.19
  (loop for i from 1 to 40
	for type = `(signed-byte ,i)
	unless
	(let ((v (make-array '(10) :initial-contents '(0 0 0 0 -1 -1 -1 -1 -1 -1)
			     :element-type type
			     :fill-pointer 4)))
	  (equal (loop for j from 0 to 9
		       do (setf (fill-pointer v) j)
		       collect (not (notany #'minusp v)))
		 '(nil nil nil nil nil t t t t t)))
	collect i)
  nil)

(deftest notany.20
  (let ((v (make-array '(10) :initial-contents "abcd012345"
		       :element-type 'character
		       :fill-pointer 4)))
    (loop for j from 0 to 9
	  do (setf (fill-pointer v) j)
	  collect (not (notany #'digit-char-p v))))
  (nil nil nil nil nil t t t t t))

(deftest notany.21
  (let ((v (make-array '(10) :initial-contents "abcd012345"
		       :element-type 'base-char
		       :fill-pointer 4)))
    (loop for j from 0 to 9
	  do (setf (fill-pointer v) j)
	  collect (not (notany #'digit-char-p v))))
  (nil nil nil nil nil t t t t t))

(deftest notany.22
  (let ((v (make-array '(5) :initial-contents "abcde"
		       :element-type 'base-char)))
    (values
     (notnot (notany #'digit-char-p v))
     (setf (aref v 2) #\0)
     (notany #'digit-char-p v)))
  t #\0 nil)

(deftest notany.23
  (loop for type in '(short-float single-float double-float long-float)
	for v = (make-array '(9)
			    :element-type type
			    :initial-contents
			    (mapcar #'(lambda (x) (coerce x type)) '(1 2 3 4 5 6 0 8 3)))
	when (notany #'zerop v)
	collect (list type v))
  nil)

(deftest notany.24
  (loop for type in '(short-float single-float double-float long-float)
	for v = (make-array '(9)
			    :element-type type
			    :fill-pointer 6
			    :initial-contents
			    (mapcar #'(lambda (x) (coerce x type)) '(1 2 3 4 5 6 0 8 3)))
	unless (notany #'zerop v)
	collect (list type v))
  nil)

(deftest notany.25
  (loop for type in '(short-float single-float double-float long-float)
	for ctype = `(complex ,type)
	for v = (make-array '(6)
			    :element-type ctype
			    :initial-contents
			    (mapcar #'(lambda (x) (complex x (coerce x type))) '(1 2 3 4 5 6)))
	unless (notany (complement #'complexp) v)
	collect (list type v))
  nil)

;;; Displaced vectors

(deftest notany.26
  (let* ((v1 (make-array '(10) :initial-contents '(1 3 2 4 6 8 5 7 9 1)))
	 (v2 (make-array '(4) :displaced-to v1
			 :displaced-index-offset 2)))
    (values
     (notany #'oddp v1)
     (notnot (notany #'oddp v2))))
  nil t)

(deftest notany.27
  (loop for i from 1 to 40
	for type = `(unsigned-byte ,i)
	unless
	(let* ((v1 (make-array '(10) :initial-contents '(1 1 0 0 0 0 1 1 1 1)
			       :element-type type))
	       (v2 (make-array '(4) :displaced-to v1
			       :displaced-index-offset 2
			       :element-type type)))
	  (and (not (notany 'oddp v1))
	       (notany #'oddp v2)))
	collect i)
  nil)

(deftest notany.28
  (loop for i from 1 to 40
	for type = `(signed-byte ,i)
	unless
	(let* ((v1 (make-array '(10) :initial-contents '(-1 -1 0 0 0 0 -1 -1 -1 -1)
			       :element-type type))
	       (v2 (make-array '(4) :displaced-to v1
			       :displaced-index-offset 2
			       :element-type type)))
	  (and (not (notany 'oddp v1))
	       (notany #'oddp v2)))
	collect i)
  nil)

(deftest notany.29
  (let* ((s1 (make-array '(8) :initial-contents "12abc345" :element-type 'character)))
    (loop for i from 0 to 6
	  for s2 = (make-array '(2) :element-type 'character
			       :displaced-to s1
			       :displaced-index-offset i)
	  collect (not (notany 'digit-char-p s2))))
  (t t nil nil t t t))

(deftest notany.30
  (let* ((s1 (make-array '(8) :initial-contents "12abc345" :element-type 'base-char)))
    (loop for i from 0 to 6
	  for s2 = (make-array '(2) :element-type 'base-char
			       :displaced-to s1
			       :displaced-index-offset i)
	  collect (not (notany 'digit-char-p s2))))
  (t t nil nil t t t))

(deftest notany.31
  (let ((v (make-array '(10) :initial-contents '(1 2 3 4 5 6 7 8 9 10)
		       :adjustable t)))
    (values
     (notnot (notany #'minusp v))
     (progn
       (adjust-array v '(11) :initial-element -1)
       (notany #'minusp v))))
  t nil)

(deftest notany.32
  (let ((v (make-array '(10) :initial-contents '(1 2 3 4 5 6 7 8 9 10)
		       :fill-pointer 10
		       :adjustable t)))
    (values
     (notnot (notany #'minusp v))
     (progn
       (adjust-array v '(11) :initial-element -1)
       (notnot (notany #'minusp v)))))
  t t)


(deftest notany.order.1
  (let ((i 0) a b)
    (values
     (not (notany (progn (setf a (incf i)) 'null)
		  (progn (setf b (incf i)) '(a b c))))
     i a b))
  nil 2 1 2)

;;; Error cases

(deftest notany.error.1
  (check-type-error #'(lambda (x) (notany x '(a b c)))
		    (typef '(or symbol function)))
  nil)

(deftest notany.error.4
  (check-type-error #'(lambda (x) (notany #'null x)) #'sequencep)
  nil)

(deftest notany.error.7
  (check-type-error #'(lambda (x) (notany #'eql () x)) #'sequencep)
  nil)

(deftest notany.error.8
  (signals-error (notany) program-error)
  t)

(deftest notany.error.9
  (signals-error (notany #'null) program-error)
  t)

(deftest notany.error.10
  (signals-error (locally (notany 1 '(a b c)) t) type-error)
  t)

(deftest notany.error.11
  (signals-error (notany #'cons '(a b c)) program-error)
  t)

(deftest notany.error.12
  (signals-error (notany #'cons '(a b c) '(1 2 4) '(g h j)) program-error)
  t)

(deftest notany.error.13
  (signals-error (notany #'car '(a b c)) type-error)
  t)