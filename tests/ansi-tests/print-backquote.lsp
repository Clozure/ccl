;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jun 10 19:31:01 2004
;;;; Contains: Tests of printing of backquote forms (and fragments thereof)

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")
(compile-and-load "backquote-aux.lsp")

(deftest print.backquote.random.1
  (let* ((x '`(a ,b ,@c (d . ,e) ,.f #(1 2 ,p ,@q ,.r s) g))
	 (y (copy-tree x)))
    (or
     (loop
      repeat 20
      nconc (randomly-check-readability y :test #'is-similar))
     (and (not (equal x y)) (list :modified x y))))
  nil)

(deftest print.backquote.random.2
  (let* ((x '`(,@a ,@b))
	 (y (copy-tree x)))
    (or
     (loop
      repeat 20
      nconc (randomly-check-readability y :test #'is-similar))
     (and (not (is-similar x y)) (list :modified x y))))
  nil)

(deftest print.backquote.random.3
  (let* ((x '`(,.a ,.b))
	 (y (copy-tree x)))
    (or
     (loop
      repeat 20
      nconc (randomly-check-readability y :test #'is-similar))
     (and (not (is-similar x y)) (list :modified x y))))
  nil)

(deftest print.backquote.random.4
  (let* ((x '`(,a ,b))
	 (y (copy-tree x)))
    (or
     (loop
      repeat 20
      nconc (randomly-check-readability y :test #'is-similar))
     (and (not (is-similar x y)) (list :modified x y))))
  nil)

(deftest print.backquote.random.5
  (let* ((x '`#(,a ,b))
	 (y (copy-tree x)))
    (or
     (loop
      repeat 20
      nconc (randomly-check-readability y :test #'is-similar))
     (and (not (is-similar x y)) (list :modified x y))))
  nil)

(deftest print.backquote.random.6
  (let ((x '`(,@a ,@b)))
    (and (consp x)
	 (symbolp (car x))
	 (loop
	  repeat 20
	  nconc (randomly-check-readability (list (car x)) :test #'is-similar))))
  nil)

(deftest print.backquote.random.7
  (let ((x '`(,.a ,.b)))
    (and (consp x)
	 (symbolp (car x))
	 (loop
	  repeat 20
	  nconc (randomly-check-readability (list (car x)) :test #'is-similar))))
  nil)

(deftest print.backquote.random.8
  (let ((x '`(,a ,b)))
    (and (consp x)
	 (symbolp (car x))
	 (loop
	  repeat 20
	  nconc (randomly-check-readability (list (car x)) :test #'is-similar))))
  nil)

(deftest print.backquote.random.9
  (let ((x '`#(,a ,b)))
    (and (consp x)
	 (symbolp (car x))
	 (loop
	  repeat 20
	  nconc (randomly-check-readability (list (car x)) :test #'is-similar))))
  nil)

(deftest print.backquote.random.10
  (let ((x '`#(,a , .b)))
    (loop
     repeat 20
     nconc (randomly-check-readability x :test #'is-similar)))
  nil)

(deftest print.backquote.random.11
  (let ((x '`#(,a , @b)))
    (loop
     repeat 20
     nconc (randomly-check-readability x :test #'is-similar)))
  nil)

(deftest print.backquote.random.12
  (let ((x '`#(,a ,b c)))
    (and (consp x)
	 (symbolp (car x))
	 (loop
	  repeat 20
	  nconc (randomly-check-readability (list (car x)) :test #'is-similar))))
  nil)

(deftest print.backquote.random.13
  (let* ((x '`#(,a ,b c))
	 (y (copy-tree x)))
    (or
     (loop
      repeat 20
      nconc (randomly-check-readability x :test #'is-similar))
     (and (not (is-similar x y)) (list :modified x y))))     
  nil)

(deftest print.backquote.random.14
  (loop for x = (make-random-backquoted-form 100)
	repeat 500
	nconc (randomly-check-readability x :test #'is-similar))
  nil)