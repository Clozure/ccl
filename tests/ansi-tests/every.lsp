;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 17 23:25:58 2002
;;;; Contains: Tests of EVERY

(in-package :cl-test)

(deftest every.1
  (notnot-mv (every #'identity nil))
  t)

(deftest every.2
  (notnot-mv (every #'identity #()))
  t)

(deftest every.3
  (let ((count 0))
    (values
     (every #'(lambda (x) (incf count) (< x 10))
	    '(1 2 4 13 5 1))
     count))
  nil 4)

(deftest every.4
  (notnot-mv (every #'= '(1 2 3 4) '(1 2 3 4 5)))
  t)

(deftest every.5
  (notnot-mv (every #'= '(1 2 3 4 5) '(1 2 3 4)))
  t)

(deftest every.6
  (every #'= '(1 2 3 4 5) '(1 2 3 4 6))
  nil)

(deftest every.7
  (notnot-mv (every #'(lambda (x y) (or x y))
		    '(nil t t nil t) #(t nil t t nil nil)))
  t)

(deftest every.8
  (let ((x '(1))
	(args nil))
    (loop for i from 1 below (1- (min 100 call-arguments-limit))
	  do (push x args)
	  always (apply #'every #'= args)))
  t)

(deftest every.9
  (notnot-mv (every #'zerop #*000000000000))
  t)

(deftest every.10
  (notnot-mv (every #'zerop #*))
  t)

(deftest every.11
  (every #'zerop #*0000010000)
  nil)

(deftest every.12
  (notnot-mv (every #'(lambda (x) (eql x #\a)) "aaaaaaaa"))
  t)

(deftest every.13
  (notnot-mv (every #'(lambda (x) (eql x #\a)) ""))
  t)

(deftest every.14
  (every #'(lambda (x) (eql x #\a)) "aaaaaabaaaa")
  nil)

(deftest every.15
  (every 'null '(nil nil t nil))
  nil)

(deftest every.16
  (notnot-mv (every 'null '(nil nil nil nil)))
  t)

;;; Other specialized sequences

(deftest every.17
  (let ((v (make-array '(10) :initial-contents '(0 0 0 0 1 2 3 4 5 6)
		       :fill-pointer 4)))
    (loop for j from 0 to 9
	  do (setf (fill-pointer v) j)
	  collect (notnot (every #'zerop v))))
  (t t t t t nil nil nil nil nil))

(deftest every.18
  (loop for i from 1 to 40
	for type = `(unsigned-byte ,i)
	unless
	(let ((v (make-array '(10) :initial-contents '(0 0 0 0 1 1 1 1 1 1)
			     :element-type type
			     :fill-pointer 4)))
	  (equal (loop for j from 0 to 9
		       do (setf (fill-pointer v) j)
		       collect (notnot (every #'zerop v)))
		 '(t t t t t nil nil nil nil nil)))
	collect i)
  nil)

(deftest every.19
  (loop for i from 1 to 40
	for type = `(signed-byte ,i)
	unless
	(let ((v (make-array '(10) :initial-contents '(0 0 0 0 -1 -1 -1 -1 -1 -1)
			     :element-type type
			     :fill-pointer 4)))
	  (equal (loop for j from 0 to 9
		       do (setf (fill-pointer v) j)
		       collect (notnot (every #'zerop v)))
		 '(t t t t t nil nil nil nil nil)))
	collect i)
  nil)

(deftest every.20
  (let ((v (make-array '(10) :initial-contents "abcd012345"
		       :element-type 'character
		       :fill-pointer 4)))
    (loop for j from 0 to 9
	  do (setf (fill-pointer v) j)
	  collect (notnot (every #'alpha-char-p v))))
  (t t t t t nil nil nil nil nil))

(deftest every.21
  (let ((v (make-array '(10) :initial-contents "abcd012345"
		       :element-type 'base-char
		       :fill-pointer 4)))
    (loop for j from 0 to 9
	  do (setf (fill-pointer v) j)
	  collect (notnot (every #'alpha-char-p v))))
  (t t t t t nil nil nil nil nil))

(deftest every.22
  (let ((v (make-array '(5) :initial-contents "abcde"
		       :element-type 'base-char)))
    (values
     (notnot (every #'alpha-char-p v))
     (setf (aref v 2) #\0)
     (every #'alpha-char-p v)))
  t #\0 nil)

;;; Displaced vectors

(deftest every.23
  (let* ((v1 (make-array '(10) :initial-contents '(1 3 2 4 6 8 5 7 9 1)))
	 (v2 (make-array '(4) :displaced-to v1
			 :displaced-index-offset 2)))
    (values
     (every #'evenp v1)
     (notnot (every 'evenp v2))))
  nil t)

(deftest every.24
  (loop for i from 1 to 40
	for type = `(unsigned-byte ,i)
	unless
	(let* ((v1 (make-array '(10) :initial-contents '(1 1 0 0 0 0 1 1 1 1)
			       :element-type type))
	       (v2 (make-array '(4) :displaced-to v1
			       :displaced-index-offset 2
			       :element-type type)))
	  (and (not (every 'evenp v1))
	       (every #'evenp v2)))
	collect i)
  nil)

(deftest every.25
  (loop for i from 1 to 40
	for type = `(signed-byte ,i)
	unless
	(let* ((v1 (make-array '(10) :initial-contents '(-1 -1 0 0 0 0 -1 -1 -1 -1)
			       :element-type type))
	       (v2 (make-array '(4) :displaced-to v1
			       :displaced-index-offset 2
			       :element-type type)))
	  (and (not (every 'evenp v1))
	       (every #'evenp v2)))
	collect i)
  nil)

(deftest every.26
  (let* ((s1 (make-array '(8) :initial-contents "12abc345" :element-type 'character)))
    (loop for i from 0 to 6
	  for s2 = (make-array '(2) :element-type 'character
			       :displaced-to s1
			       :displaced-index-offset i)
	  collect (notnot (every 'alpha-char-p s2))))
  (nil nil t t nil nil nil))

(deftest every.27
  (let* ((s1 (make-array '(8) :initial-contents "12abc345" :element-type 'base-char)))
    (loop for i from 0 to 6
	  for s2 = (make-array '(2) :element-type 'base-char
			       :displaced-to s1
			       :displaced-index-offset i)
	  collect (notnot (every 'alpha-char-p s2))))
  (nil nil t t nil nil nil))

;;; adjustable vectors

(deftest every.28
  (let ((v (make-array '(10) :initial-contents '(1 2 3 4 5 6 7 8 9 10)
		       :adjustable t)))
    (values
     (notnot (every #'plusp v))
     (progn
       (adjust-array v '(11) :initial-element -1)
       (every #'plusp v))))
  t nil)

(deftest every.29
  (let ((v (make-array '(10) :initial-contents '(1 2 3 4 5 6 7 8 9 10)
		       :fill-pointer 10
		       :adjustable t)))
    (values
     (notnot (every #'plusp v))
     (progn
       (adjust-array v '(11) :initial-element -1)
       (every #'plusp v))))
  t t)

;;; Float, complex vectors

(deftest every.30
  (loop for type in '(short-float single-float double-float long-float)
	for v = (make-array '(6)
			    :element-type type
			    :initial-contents
			    (mapcar #'(lambda (x) (coerce x type)) '(1 2 3 4 5 6)))
	unless (every #'plusp v)
	collect (list type v))
  nil)

(deftest every.31
  (loop for type in '(short-float single-float double-float long-float)
	for v = (make-array '(6)
			    :element-type type
			    :fill-pointer 5
			    :initial-contents
			    (mapcar #'(lambda (x) (coerce x type)) '(1 2 3 4 5 -1)))
	unless (every #'plusp v)
	collect (list type v))
  nil)

(deftest every.32
  (loop for type in '(short-float single-float double-float long-float)
	for ctype = `(complex ,type)
	for v = (make-array '(6)
			    :element-type ctype
			    :initial-contents
			    (mapcar #'(lambda (x) (complex x (coerce x type))) '(1 2 3 4 5 6)))
	unless (every #'complexp v)
	collect (list type v))
  nil)

;;; Order of arguments

(deftest every.order.1
  (let ((i 0) x y)
    (values
     (every (progn (setf x (incf i)) #'null)
	    (progn (setf y (incf i)) '(nil nil a nil)))
     i x y))
  nil 2 1 2)

(deftest every.order.2
  (let ((i 0) x y z)
    (values
     (every (progn (setf x (incf i)) #'equal)
	    (progn (setf y (incf i)) '(nil nil a nil))
	    (progn (setf z (incf i)) '(nil nil a b)))
     i x y z))
  nil 3 1 2 3)

;;; Error cases

(deftest every.error.1
  (check-type-error #'(lambda (x) (every x '(a b c)))
		    #'(lambda (x) (typep x '(or function symbol))))
  nil)

(deftest every.error.2
  (check-type-error #'(lambda (x) (every #'null x))
		    #'(lambda (x) (typep x 'sequence)))
  nil)

(deftest every.error.3
  (check-type-error #'(lambda (x) (every #'eq () x))
		    #'(lambda (x) (typep x 'sequence)))
  nil)

(deftest every.error.8
  (signals-error (every) program-error)
  t)

(deftest every.error.9
  (signals-error (every #'null) program-error)
  t)

(deftest every.error.10
  (signals-error (locally (every 1 '(a b c)) t) type-error)
  t)

(deftest every.error.11
  (signals-error (every #'cons '(a b c)) program-error)
  t)

(deftest every.error.12
  (signals-error (every #'cons '(a b c) '(1 2 3) '(4 5 6)) program-error)
  t)

(deftest every.error.13
  (signals-error (every #'car '(a b c)) type-error)
  t)

(deftest every.error.14
  (signals-error (every #'identity '(1 2 3 . 4)) type-error)
  t)
