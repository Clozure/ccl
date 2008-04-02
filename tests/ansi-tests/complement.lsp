;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct  6 20:04:33 2002
;;;; Contains: Tests for COMPLEMENT

(in-package :cl-test)

(deftest complement.1
  (notnot-mv (funcall (complement #'identity) nil))
  t)

(deftest complement.2
  (funcall (complement #'identity) t)
  nil)

(deftest complement.3
  (check-predicate
   #'(lambda (x) (eql (funcall (cl::complement #'not) x)
		      (not (not x)))))
  nil)

(deftest complement.4
  (let ((x '(#\b)))
    (loop for i from 2 to (min 256 (1- call-arguments-limit))
	  always (progn
		   (push #\a x)
		   (apply (complement #'char=) x))))
  t)

(deftest complement.5
  (notnot-mv (complement #'identity))
  t)

(deftest complement.6
  (flet ((%f (&rest args) (notnot (evenp (length args)))))
    (let ((cf (complement #'%f)))
      (values
       (%f) (%f 'a) (%f 'a 'b) (%f 'a 'b 'c)
       (funcall cf) (funcall cf 'a) (funcall cf 'a 'b) (funcall cf 'a 'b 'c))))
  t nil t nil
  nil t nil t)

(deftest complement.7
  (flet ((%f (&optional x y) (if x (not y) y)))
    (let ((cf (complement #'%f)))
      (values
       (%f) (%f nil) (%f t) (%f nil nil) (%f t nil) (%f nil t) (%f t t)
       (funcall cf) (funcall cf nil) (funcall cf t)
       (funcall cf nil nil) (funcall cf t nil)
       (funcall cf nil t) (funcall cf t t))))
  nil nil t nil t t nil
  t t nil t nil nil t)

(deftest complement.8
  (flet ((%f (&key x y) (if x (not y) y)))
    (let ((cf (complement #'%f)))
      (values
       (list
       (%f)
       (%f :x nil) (%f :x t)
       (%f :y nil) (%f :y t :y nil)
       (%f :x nil :y nil) (%f :x t :y nil)
       (%f :y t :x nil) (%f :x t :y t))
      
       (list
	(funcall cf) (funcall cf :x nil) (funcall cf :x t)
	(funcall cf :y nil) (funcall cf :y t)
	(funcall cf :x nil :y nil) (funcall cf :x t :y nil)
	(funcall cf :y t :x nil) (funcall cf :x t :y t :x nil))
       (list
	(funcall cf :x nil :y t :foo nil :allow-other-keys t)
	(funcall cf :x nil :y t :allow-other-keys nil)))))
  (nil nil t nil t nil t t nil)
  (t t nil t nil t nil nil t)
  (nil nil))

(deftest complement.9
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x y)))
    (eval `(defmethod ,sym ((x integer) (y integer)) (evenp (+ x y))))
    (eval `(defmethod ,sym ((x t) (y t)) nil))
    (let ((cf (complement (symbol-function sym))))
      (values (funcall cf 'a 'b)
	      (funcall cf 0 0)
	      (funcall cf 0 1)
	      (funcall cf 1 0)
	      (funcall cf 1 1))))
  t nil t t nil)

(deftest complement.10
  (let ((cf (complement (compile nil '(lambda (x y) (evenp (+ x y)))))))
    (values (funcall cf 0 0)
	    (funcall cf 0 1)
	    (funcall cf 1 0)
	    (funcall cf 1 1)))
  nil t t nil)    

(deftest complement.order.1
  (let ((i 0))
    (let ((fn (complement (progn (incf i) #'null))))
      (values
       i
       (mapcar fn '(a b nil c 1 nil t nil))
       i)))
  1 (t t nil t t nil t nil) 1)

;;; Error tests

(deftest complement.error.1
  (signals-error (complement) program-error)
  t)

(deftest complement.error.2
  (signals-error (complement #'not t) program-error)
  t)

(deftest complement.error.3
  (signals-error (funcall (complement #'identity))
		 program-error)
  t)

(deftest complement.error.4
  (signals-error (funcall (complement #'identity) t t)
		 program-error)
  t)

(deftest complement.error.5
  (signals-error (funcall (complement #'(lambda (&key) t)) :foo t) program-error)
  t)

(deftest complement.error.6
  (signals-error (funcall (complement #'(lambda (&key) t)) :allow-other-keys nil
			  :allow-other-keys t :foo t) program-error)
  t)

(deftest complement.error.7
  (signals-error (funcall (complement #'(lambda (x &rest y) (and x (evenp (length y))))))
		 program-error)
  t)
