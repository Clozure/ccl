;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:28:35 2003
;;;; Contains: Tests of PUSHNEW

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest pushnew.1
  (let ((x nil))
    (let ((y (pushnew 'a x)))
      (and
       (eqt x y)
       (equal x '(a))
       t)))
  t)

(deftest pushnew.2
  (let* ((x (copy-tree '(b c d a k f q)))
	 (y (pushnew 'a x)))
    (and
     (eqt x y)
     x))
  (b c d a k f q))

(deftest pushnew.3
  (let* ((x (copy-tree '(1 2 3 4 5 6 7 8)))
	 (y (pushnew 7 x)))
    (and
     (eqt x y)
     x))
  (1 2 3 4 5 6 7 8))

(deftest pushnew.4
  (let* ((x (copy-tree '((a b) 1 "and" c d e)))
	 (y (pushnew (copy-tree '(c d)) x
		     :test 'equal)))
    (and (eqt x y)
	 x))
  ((c d) (a b) 1 "and" c d e))

(deftest pushnew.5
  (let* ((x (copy-tree '((a b) 1 "and" c d e)))
	 (y (pushnew (copy-tree '(a b)) x
		     :test 'equal)))
    (and
     (eqt x y)
     x))
  ((a b) 1 "and" c d e))

(deftest pushnew.6
  (let* ((x (copy-tree '((a b) (c e) (d f) (g h))))
	 (y (pushnew (copy-tree '(d i)) x :key #'car))
	 (z (pushnew (copy-tree '(z 10)) x :key #'car)))
    (and (eqt y (cdr z))
	 (eqt z x)
	 x))
  ((z 10) (a b) (c e) (d f) (g h)))

(deftest pushnew.7
  (let* ((x (copy-tree '(("abc" 1) ("def" 2) ("ghi" 3))))
	 (y (pushnew (copy-tree '("def" 4)) x
		     :key #'car :test #'string=))
	 (z (pushnew (copy-tree '("xyz" 10))
		     x
		     :key #'car :test #'string=)))
    (and
     (eqt y (cdr x))
     (eqt x z)
     x))
  (("xyz" 10) ("abc" 1) ("def" 2) ("ghi" 3)))

(deftest pushnew.8
  (let* ((x (copy-tree '(("abc" 1) ("def" 2) ("ghi" 3))))
	 (y (pushnew (copy-tree '("def" 4)) x
		     :key #'car :test-not (complement #'string=)))
	 (z (pushnew (copy-tree '("xyz" 10)) x
		     :key #'car :test-not (complement #'string=))))
    (and
     (eqt y (cdr x))
     (eqt x z)
     x))
  (("xyz" 10) ("abc" 1) ("def" 2) ("ghi" 3)))

(deftest pushnew.9
  (let* ((x (copy-tree '(("abc" 1) ("def" 2) ("ghi" 3))))
	 (y (pushnew (copy-tree '("def" 4)) x
		     :key 'car :test-not (complement #'string=)))
	 (z (pushnew (copy-tree '("xyz" 10)) x
		     :key 'car :test-not (complement #'string=))))
    (and
     (eqt y (cdr x))
     (eqt x z)
     x))
  (("xyz" 10) ("abc" 1) ("def" 2) ("ghi" 3)))

;; Check that a NIL :key argument is the same as no key argument at all
(deftest pushnew.10
  (let* ((x (list 'a 'b 'c 'd))
	 (result (pushnew 'z x :key nil)))
    result)
  (z a b c d))

;; Check that a NIL :key argument is the same as no key argument at all
(deftest pushnew.11
  (let* ((x (copy-tree '((a b) 1 "and" c d e)))
	 (y (pushnew (copy-tree '(a b)) x
		     :test 'equal :key nil)))
    (and
     (eqt x y)
     x))
  ((a b) 1 "and" c d e))

(deftest pushnew.12
  (let ((i 0) x y z (d '(b c)))
    (values
     (pushnew (progn (setf x (incf i)) 'a)
	      d
	      :key (progn (setf y (incf i)) #'identity)
	      :test (progn (setf z (incf i)) #'eql))
     d i x y z))
  (a b c) (a b c)
  3 1 2 3)

(deftest pushnew.13
  (let ((i 0) x y z (d '(b c)))
    (values
     (pushnew (progn (setf x (incf i)) 'a)
	      d
	      :key (progn (setf y (incf i)) #'identity)
	      :test-not (progn (setf z (incf i)) (complement #'eql)))
     d i x y z))
  (a b c) (a b c)
  3 1 2 3)

(deftest pushnew.14
  (let ((i 0) x y z (d '(b c)))
    (values
     (pushnew (progn (setf x (incf i)) 'a)
	      d
	      :test (progn (setf z (incf i)) #'eql)
	      :key (progn (setf y (incf i)) #'identity))
     d i x y z))
  (a b c) (a b c)
  3 1 3 2)

(deftest pushnew.15
  (let ((i 0) x y z (d '(b c)))
    (values
     (pushnew (progn (setf x (incf i)) 'a)
	      d
	      :test-not (progn (setf z (incf i)) (complement #'eql))
	      :key (progn (setf y (incf i)) #'identity))
     d i x y z))
  (a b c) (a b c)
  3 1 3 2)

(deftest pushnew.16
  (let ((x '(1 2 3)))
    (values
     (pushnew 10 x :test #'<=)
     x))
  (10 1 2 3)
  (10 1 2 3))

(deftest pushnew.17
  (let ((x '(1 2 3)))
    (values
     (pushnew 10 x :test #'>)
     x))
  (1 2 3)
  (1 2 3))
(deftest pushnew.18
  (let ((x '(1 2 3)))
    (values
     (pushnew 10 x :test-not #'>)
     x))
  (10 1 2 3)
  (10 1 2 3))

(deftest pushnew.19
  (let ((x '(1 2 3)))
    (values
     (pushnew 10 x :test-not #'<=)
     x))
  (1 2 3)
  (1 2 3))

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest pushnew.20
  (macrolet
   ((%m (z) z))
   (let ((x nil))
     (values
      (pushnew (expand-in-current-env (%m 1)) x)
      x)))
  (1) (1))

(deftest pushnew.21
  (macrolet
   ((%m (z) z))
   (let ((x nil))
     (values
      (pushnew 1 (expand-in-current-env (%m x)))
      x)))
  (1) (1))

(deftest pushnew.22
  (macrolet
   ((%m (z) z))
   (let ((x '(a b)))
     (values
      (pushnew 1 x :test (expand-in-current-env (%m #'eql)))
      x)))
  (1 a b) (1 a b))

(deftest pushnew.23
  (macrolet
   ((%m (z) z))
   (let ((x '(1)))
     (values
      (pushnew 1 x :test-not (expand-in-current-env (%m #'eql)))
      x)))
  (1 1) (1 1))

(deftest pushnew.24
  (macrolet
   ((%m (z) z))
   (let ((x '(3)))
     (values
      (pushnew 1 x :key (expand-in-current-env (%m #'evenp)))
      x)))
  (3) (3))

(defharmless pushnew.test-and-test-not.1
  (let ((x '(b c))) (pushnew 'a x :test #'eql :test-not #'eql)))
  
(defharmless pushnew.test-and-test-not.2
  (let ((x '(b c))) (pushnew 'a x :test-not #'eql :test #'eql)))
  

(deftest pushnew.order.1
  (let ((x (vector nil nil nil nil))
	(y (vector 'a 'b 'c 'd))
	(i 1))
    (pushnew (aref y (incf i)) (aref x (incf i)))
    (values x y i))
  #(nil nil nil (c))
  #(a b c d)
  3)

(deftest pushnew.order.2
  (let ((x (vector nil nil nil nil nil))
	(y (vector 'a 'b 'c 'd 'e))
	(i 1))
    (pushnew (aref y (incf i)) (aref x (incf i))
	     :test (progn (incf i) #'eql))
    (values x y i))
  #(nil nil nil (c) nil)
  #(a b c d e)
  4)

(deftest pushnew.order.3
  (let ((x '(a b c)))
    (values
     (pushnew (progn (setq x '(d e)) 'z) x)
     x))
  (z d e) (z d e))

(deftest pushnew.error.1
  (signals-error
   (let ((x '(a b)))
     (pushnew 'c x :test #'identity))
   program-error)
  t)

(deftest pushnew.error.2
  (signals-error
   (let ((x '(a b)))
     (pushnew 'c x :test-not #'identity))
   program-error)
  t)

(deftest pushnew.error.3
  (signals-error
   (let ((x '(a b)))
     (pushnew 'c x :key #'cons))
   program-error)
  t)

(def-macro-test pushnew.error.4 (pushnew x y))
