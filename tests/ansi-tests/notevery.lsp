;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 07:20:12 2002
;;;; Contains: Tests for NOTEVERY

(in-package :cl-test)

(deftest notevery.1
  (notevery #'identity nil)
  nil)

(deftest notevery.2
  (notevery #'identity #())
  nil)

(deftest notevery.3
  (let ((count 0))
    (values
     (not (notevery #'(lambda (x) (incf count) (< x 10))
		    '(1 2 4 13 5 1)))
     count))
  nil 4)

(deftest notevery.4
  (notevery #'= '(1 2 3 4) '(1 2 3 4 5))
  nil)

(deftest notevery.5
  (notevery #'= '(1 2 3 4 5) '(1 2 3 4))
  nil)

(deftest notevery.6
  (not-mv (notevery #'= '(1 2 3 4 5) '(1 2 3 4 6)))
  nil)

(deftest notevery.7
  (notevery #'(lambda (x y) (or x y))
	    '(nil t t nil t) #(t nil t t nil nil))
  nil)

(deftest notevery.8
  (let ((x '(1))
	(args nil))
    (not
     (loop for i from 1 below (1- (min 100 call-arguments-limit))
	   do (push x args)
	   always (not (apply #'notevery #'= args)))))
  nil)

(deftest notevery.9
  (notevery #'zerop #*000000000000)
  nil)

(deftest notevery.10
  (notevery #'zerop #*)
  nil)

(deftest notevery.11
  (not-mv (notevery #'zerop #*0000010000))
  nil)

(deftest notevery.12
  (notevery #'(lambda (x) (eql x #\a)) "aaaaaaaa")
  nil)

(deftest notevery.13
  (notevery #'(lambda (x) (eql x #\a)) "")
  nil)

(deftest notevery.14
  (not-mv (notevery #'(lambda (x) (eql x #\a)) "aaaaaabaaaa"))
  nil)

(deftest notevery.15
  (not-mv (notevery 'null '(nil nil t nil)))
  nil)

(deftest notevery.16
  (notevery 'null '(nil nil nil nil))
  nil)

;;; Other specialized sequences

(deftest notevery.17
  (let ((v (make-array '(10) :initial-contents '(0 0 0 0 1 2 3 4 5 6)
		       :fill-pointer 4)))
    (loop for j from 0 to 9
	  do (setf (fill-pointer v) j)
	  collect (not (notevery #'zerop v))))
  (t t t t t nil nil nil nil nil))

(deftest notevery.18
  (loop for i from 1 to 40
	for type = `(unsigned-byte ,i)
	unless
	(let ((v (make-array '(10) :initial-contents '(0 0 0 0 1 1 1 1 1 1)
			     :element-type type
			     :fill-pointer 4)))
	  (equal (loop for j from 0 to 9
		       do (setf (fill-pointer v) j)
		       collect (not (notevery #'zerop v)))
		 '(t t t t t nil nil nil nil nil)))
	collect i)
  nil)

(deftest notevery.19
  (loop for i from 1 to 40
	for type = `(signed-byte ,i)
	unless
	(let ((v (make-array '(10) :initial-contents '(0 0 0 0 -1 -1 -1 -1 -1 -1)
			     :element-type type
			     :fill-pointer 4)))
	  (equal (loop for j from 0 to 9
		       do (setf (fill-pointer v) j)
		       collect (not (notevery #'zerop v)))
		 '(t t t t t nil nil nil nil nil)))
	collect i)
  nil)

(deftest notevery.20
  (let ((v (make-array '(10) :initial-contents "abcd012345"
		       :element-type 'character
		       :fill-pointer 4)))
    (loop for j from 0 to 9
	  do (setf (fill-pointer v) j)
	  collect (not (notevery #'alpha-char-p v))))
  (t t t t t nil nil nil nil nil))

(deftest notevery.21
  (let ((v (make-array '(10) :initial-contents "abcd012345"
		       :element-type 'base-char
		       :fill-pointer 4)))
    (loop for j from 0 to 9
	  do (setf (fill-pointer v) j)
	  collect (not (notevery #'alpha-char-p v))))
  (t t t t t nil nil nil nil nil))

(deftest notevery.22
  (let ((v (make-array '(5) :initial-contents "abcde"
		       :element-type 'base-char)))
    (values
     (not (notevery #'alpha-char-p v))
     (setf (aref v 2) #\0)
     (not (notevery #'alpha-char-p v))))
  t #\0 nil)

;;; Displaced vectors

(deftest notevery.23
  (let* ((v1 (make-array '(10) :initial-contents '(1 3 2 4 6 8 5 7 9 1)))
	 (v2 (make-array '(4) :displaced-to v1
			 :displaced-index-offset 2)))
    (values
     (not (notevery #'evenp v1))
     (not (notevery 'evenp v2))))
  nil t)

(deftest notevery.24
  (loop for i from 1 to 40
	for type = `(unsigned-byte ,i)
	unless
	(let* ((v1 (make-array '(10) :initial-contents '(1 1 0 0 0 0 1 1 1 1)
			       :element-type type))
	       (v2 (make-array '(4) :displaced-to v1
			       :displaced-index-offset 2
			       :element-type type)))
	  (and (notevery 'evenp v1)
	       (not (notevery #'evenp v2))))
	collect i)
  nil)

(deftest notevery.25
  (loop for i from 1 to 40
	for type = `(signed-byte ,i)
	unless
	(let* ((v1 (make-array '(10) :initial-contents '(-1 -1 0 0 0 0 -1 -1 -1 -1)
			       :element-type type))
	       (v2 (make-array '(4) :displaced-to v1
			       :displaced-index-offset 2
			       :element-type type)))
	  (and (notevery 'evenp v1)
	       (not (notevery #'evenp v2))))
	collect i)
  nil)

(deftest notevery.26
  (let* ((s1 (make-array '(8) :initial-contents "12abc345" :element-type 'character)))
    (loop for i from 0 to 6
	  for s2 = (make-array '(2) :element-type 'character
			       :displaced-to s1
			       :displaced-index-offset i)
	  collect (not (notevery 'alpha-char-p s2))))
  (nil nil t t nil nil nil))

(deftest notevery.27
  (let* ((s1 (make-array '(8) :initial-contents "12abc345" :element-type 'base-char)))
    (loop for i from 0 to 6
	  for s2 = (make-array '(2) :element-type 'base-char
			       :displaced-to s1
			       :displaced-index-offset i)
	  collect (not (notevery 'alpha-char-p s2))))
  (nil nil t t nil nil nil))

;;; adjustable vectors

(deftest notevery.28
  (let ((v (make-array '(10) :initial-contents '(1 2 3 4 5 6 7 8 9 10)
		       :adjustable t)))
    (values
     (not (notevery #'plusp v))
     (progn
       (adjust-array v '(11) :initial-element -1)
       (not (notevery #'plusp v)))))
  t nil)

(deftest notevery.29
  (let ((v (make-array '(10) :initial-contents '(1 2 3 4 5 6 7 8 9 10)
		       :fill-pointer 10
		       :adjustable t)))
    (values
     (not (notevery #'plusp v))
     (progn
       (adjust-array v '(11) :initial-element -1)
       (not (notevery #'plusp v)))))
  t t)

;;; Float, complex vectors

(deftest notevery.30
  (loop for type in '(short-float single-float double-float long-float)
	for v = (make-array '(6)
			    :element-type type
			    :initial-contents
			    (mapcar #'(lambda (x) (coerce x type)) '(1 2 3 4 5 6)))
	when (notevery #'plusp v)
	collect (list type v))
  nil)

(deftest notevery.31
  (loop for type in '(short-float single-float double-float long-float)
	for v = (make-array '(6)
			    :element-type type
			    :fill-pointer 5
			    :initial-contents
			    (mapcar #'(lambda (x) (coerce x type)) '(1 2 3 4 5 -1)))
	when (notevery #'plusp v)
	collect (list type v))
  nil)

(deftest notevery.32
  (loop for type in '(short-float single-float double-float long-float)
	for ctype = `(complex ,type)
	for v = (make-array '(6)
			    :element-type ctype
			    :initial-contents
			    (mapcar #'(lambda (x) (complex x (coerce x type))) '(1 2 3 4 5 6)))
	when (notevery #'complexp v)
	collect (list type v))
  nil)


(deftest notevery.order.1
  (let ((i 0) a b)
    (values
     (notevery (progn (setf a (incf i)) #'identity)
	       (progn (setf b (incf i)) '(a b c d)))
     i a b))
  nil 2 1 2)

;;; Error cases

(deftest notevery.error.1
  (check-type-error #'(lambda (x) (notevery x '(a b c)))
		    (typef '(or symbol function)))
  nil)

(deftest notevery.error.4
  (check-type-error #'(lambda (x) (notevery #'null x)) #'sequencep)
  nil)

(deftest notevery.error.7
  (check-type-error #'(lambda (x) (notevery #'eql () x)) #'sequencep)
  nil)

(deftest notevery.error.8
  (signals-error (notevery) program-error)
  t)

(deftest notevery.error.9
  (signals-error (notevery #'null) program-error)
  t)

(deftest notevery.error.10
  (signals-error (locally (notevery 1 '(a b c)) t) type-error)
  t)

(deftest notevery.error.11
  (signals-error (notevery #'cons '(a b c)) program-error)
  t)

(deftest notevery.error.12
  (signals-error (notevery #'cons '(a b c) '(1 2 4) '(g h j)) program-error)
  t)

(deftest notevery.error.13
  (signals-error (notevery #'car '(a b c)) type-error)
  t)