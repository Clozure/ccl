;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 15 07:27:22 2004
;;;; Contains: Tests of ADJUST-ARRAY

(in-package :cl-test)

(defun listify-form (form)
  (cond
   ((integerp form) `'(,form))
   ((null form) nil)
   ((and (consp form)
	 (eq (car form) 'quote)
	 (consp (cadr form)))
    form)
   (t `(let ((x ,form)) (if (listp x) x (list x))))))
		     

(defmacro def-adjust-array-test (name args1 args2 expected-result)
  `(deftest ,name
     (let* ((a1 (make-array ,@args1))
	    (a2 (adjust-array a1 ,@args2)))
       (assert (or (not (adjustable-array-p a1)) (eq a1 a2)))
       (assert (or (adjustable-array-p a1)
		   (equal (array-dimensions a1) ,(listify-form (first args1)))))
       (assert (equal (array-dimensions a2) ,(listify-form (first args2))))
       ,@(unless (or (member :displaced-to args1)
		     (member :displaced-to args2))
	   (list '(assert (not (array-displacement a2)))))
       a2)
     ,expected-result))

(defmacro def-adjust-array-fp-test (name args1 args2 misc &rest expected-results)
   `(deftest ,name
     (let* ((a1 (make-array ,@args1))
	    (a2 (adjust-array a1 ,@args2)))
       (assert (or (not (adjustable-array-p a1)) (eq a1 a2)))
       (assert (or (adjustable-array-p a1)
		   (equal (array-dimensions a1) ,(listify-form (first args1)))))
       (assert (equal (array-dimensions a2) ,(listify-form (first args2))))
       ,@(unless (or (member :displaced-to args1)
		     (member :displaced-to args2))
	   (list '(assert (not (array-displacement a2)))))
       ,@(when misc (list misc))
       (values
	(fill-pointer a2)
	a2))
     ,@expected-results))

(def-adjust-array-test adjust-array.1
  (5 :initial-contents '(a b c d e))
  (4)
  #(a b c d))

(def-adjust-array-test adjust-array.2
  (5 :initial-contents '(a b c d e))
  (8 :initial-element 'x)
  #(a b c d e x x x))


(def-adjust-array-test adjust-array.3
  (5 :initial-contents '(a b c d e))
  (4 :initial-contents '(w x y z))
  #(w x y z))

(def-adjust-array-test adjust-array.4
  (5 :initial-contents '(a b c d e))
  (8 :initial-contents '(8 7 6 5 4 3 2 1))
  #(8 7 6 5 4 3 2 1))

(def-adjust-array-fp-test adjust-array.5
  (5 :initial-contents '(a b c d e) :fill-pointer 3)
  (4)
  (assert (eq (aref a2 3) 'd))
  3 #(a b c))

(def-adjust-array-fp-test adjust-array.6
  (5 :initial-contents '(a b c d e) :fill-pointer 3)
  (4 :fill-pointer nil)
  (assert (eq (aref a2 3) 'd))
  3 #(a b c))

(def-adjust-array-fp-test adjust-array.7
  (5 :initial-contents '(a b c d e) :fill-pointer 3)
  (4 :fill-pointer t)
  nil
  4 #(a b c d))

(def-adjust-array-fp-test adjust-array.8
  (5 :initial-contents '(a b c d e) :fill-pointer 3)
  (4 :fill-pointer 2)
  (progn (assert (eq (aref a2 2) 'c))
	 (assert (eq (aref a2 3) 'd)))
  2 #(a b))

(def-adjust-array-fp-test adjust-array.9
  (5 :initial-contents '(a b c d e) :fill-pointer 3)
  (8 :fill-pointer 5 :initial-element 'x)
  (assert (equal (list (aref a2 5) (aref a2 6) (aref a2 7)) '(x x x)))
  5 #(a b c d e))

(deftest adjust-array.10
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)))
	 (a2 (adjust-array a1 4 :displaced-to nil)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    a2)
  #(a b c d))

(deftest adjust-array.11
  (let* ((a0 (make-array 7 :initial-contents '(x a b c d e y)))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1))
	 (a2 (adjust-array a1 4)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    a2)
  #(a b c d))

(deftest adjust-array.12
  (let* ((a0 (make-array 7 :initial-contents '(1 2 3 4 5 6 7)))
	 (a1 (make-array 5 :initial-contents '(a b c d e)))
	 (a2 (adjust-array a1 4 :displaced-to a0)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (equal (multiple-value-list (array-displacement a2))
		   (list a0 0)))
    a2)
  #(1 2 3 4))

(deftest adjust-array.13
  (let* ((a0 (make-array 7 :initial-contents '(1 2 3 4 5 6 7)))
	 (a1 (make-array 5 :initial-contents '(a b c d e)))
	 (a2 (adjust-array a1 4 :displaced-to a0
			   :displaced-index-offset 2)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (equal (multiple-value-list (array-displacement a2))
		   (list a0 2)))
    a2)
  #(3 4 5 6))

(deftest adjust-array.14
  (let* ((a0 (make-array 7 :initial-contents '(1 2 3 4 5 6 7)))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1))
	 (a2 (adjust-array a1 4 :displaced-to a0)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (equal (multiple-value-list (array-displacement a2))
		   (list a0 0)))
    a2)
  #(1 2 3 4))

(deftest adjust-array.15
  (let* ((a0 (make-array 7 :initial-contents '(1 2 3 4 5 6 7)))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1))
	 (a2 (make-array 4 :displaced-to a1 :displaced-index-offset 1))
	 (a3 (adjust-array a2 4 :displaced-to a1)))
    a3)
  #(2 3 4 5))

(deftest adjust-array.16
  (let* ((a0 (make-array 7 :initial-contents '(1 2 3 4 5 6 7)))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1))
	 (a2 (adjust-array a1 5 :displaced-to a0)))
    a2)
  #(1 2 3 4 5))

(def-adjust-array-test adjust-array.17
  (nil :initial-element 'x)
  (nil)
  #0ax)

(def-adjust-array-test adjust-array.18
  (nil :initial-element 'x)
  (nil :initial-contents 'y)
  #0ay)

(def-adjust-array-test adjust-array.19
  (nil :initial-element 'x)
  (nil :initial-element 'y)
  #0ax)

(deftest adjust-array.20
  (let* ((a0 (make-array nil :initial-element 'x))
	 (a1 (make-array nil :displaced-to a0))
	 (a2 (adjust-array a1 nil)))
    a2)
  #0ax)

;; 2-d arrays

(def-adjust-array-test adjust-array.21
  ('(4 5) :initial-contents '((1 2 3 4 5)
			      (3 4 5 6 7)
			      (5 6 7 8 9)
			      (7 8 9 1 2)))
  ('(2 3))
  #2a((1 2 3)(3 4 5)))

(def-adjust-array-test adjust-array.22
  ('(4 5) :initial-contents '((1 2 3 4 5)
			      (3 4 5 6 7)
			      (5 6 7 8 9)
			      (7 8 9 1 2)))
  ('(6 8) :initial-element 0)
  #2a((1 2 3 4 5 0 0 0)
      (3 4 5 6 7 0 0 0)
      (5 6 7 8 9 0 0 0)
      (7 8 9 1 2 0 0 0)
      (0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)))

(deftest adjust-array.23
  (let* ((a1 (make-array '(4 5) :initial-contents '((#\1 #\2 #\3 #\4 #\5)
						    (#\3 #\4 #\5 #\6 #\7)
						    (#\5 #\6 #\7 #\8 #\9)
						    (#\7 #\8 #\9 #\1 #\2))
			 :element-type 'character))
	 (a2 (adjust-array a1 '(2 3) :element-type 'character)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a2) '(2 3))))
    (assert (not (typep 0 (array-element-type a2))))
    a2)
  #2a((#\1 #\2 #\3)(#\3 #\4 #\5)))

;;; Macro expansion tests

(deftest adjust-array.24
  (macrolet
   ((%m (z) z))
   (let ((a (make-array '(4) :initial-contents '(a b c d))))
     (adjust-array (expand-in-current-env (%m a)) '(4))))
  #(a b c d))

(deftest adjust-array.25
  (macrolet
   ((%m (z) z))
   (let ((a (make-array '(4) :initial-contents '(a b c d))))
     (adjust-array a (expand-in-current-env (%m '(4))))))
  #(a b c d))

(deftest adjust-array.26
  (macrolet
   ((%m (z) z))
   (let ((a (make-array '(4) :initial-contents '(a b c d))))
     (adjust-array a '(4) (expand-in-current-env (%m :element-type)) t)))
  #(a b c d))

(deftest adjust-array.27
  (macrolet
   ((%m (z) z))
   (let ((a (make-array '(4) :initial-contents '(a b c d))))
     (adjust-array a '(4) :element-type
		   (expand-in-current-env (%m t)))))
  #(a b c d))

(deftest adjust-array.28
  (macrolet
   ((%m (z) z))
   (let ((a (make-array '(4) :initial-contents '(a b c d))))
     (adjust-array a '(6) (expand-in-current-env (%m :initial-element)) 17)))
  #(a b c d 17 17))

(deftest adjust-array.29
  (macrolet
   ((%m (z) z))
   (let ((a (make-array '(4) :initial-contents '(a b c d))))
     (adjust-array a '(7) :initial-element (expand-in-current-env (%m 5)))))
  #(a b c d 5 5 5))

(deftest adjust-array.30
  (macrolet
   ((%m (z) z))
   (let ((a (make-array '(4) :initial-contents '(a b c d))))
     (adjust-array a '(6) (expand-in-current-env (%m :initial-contents))
		   '(1 2 3 4 5 6))))
  #(1 2 3 4 5 6))

(deftest adjust-array.31
  (macrolet
   ((%m (z) z))
   (let ((a (make-array '(4) :initial-contents '(a b c d))))
     (adjust-array a '(3) :initial-contents
		   (expand-in-current-env (%m "ABC")))))
  #(#\A #\B #\C))

(deftest adjust-array.32
  (macrolet
   ((%m (z) z))
   (let ((a (make-array '(4) :initial-contents '(a b c d))))
     (adjust-array a '(4) (expand-in-current-env (%m :fill-pointer)) nil)))
  #(a b c d))

(deftest adjust-array.33
  (macrolet
   ((%m (z) z))
   (let ((a (make-array '(4) :initial-contents '(a b c d))))
     (adjust-array a '(4) :fill-pointer (expand-in-current-env (%m nil)))))
  #(a b c d))

(deftest adjust-array.34
  (macrolet
   ((%m (z) z))
   (let ((a (make-array '(4) :initial-contents '(a b c d))))
     (adjust-array a '(4) (expand-in-current-env (%m :displaced-to)) nil)))
  #(a b c d))

(deftest adjust-array.35
  (macrolet
   ((%m (z) z))
   (let ((a (make-array '(4) :initial-contents '(a b c d))))
     (adjust-array a '(4) :displaced-to
		   (expand-in-current-env (%m nil)))))
  #(a b c d))

(deftest adjust-array.36
  (macrolet
   ((%m (z) z))
   (let ((a (make-array '(4) :initial-contents '(a b c d)))
	 (c (make-array '(8) :initial-contents '(1 2 3 4 5 6 7 8))))
     (adjust-array a '(3) :displaced-to c
		   (expand-in-current-env (%m :displaced-index-offset))
		   2)))
  #(3 4 5))

(deftest adjust-array.37
  (macrolet
   ((%m (z) z))
   (let ((a (make-array '(4) :initial-contents '(a b c d)))
	 (c (make-array '(8) :initial-contents '(1 2 3 4 5 6 7 8))))
     (adjust-array a '(5) :displaced-to c
		   :displaced-index-offset
		   (expand-in-current-env (%m 1)))))
  #(2 3 4 5 6))

;;; Adjust an adjustable array

(def-adjust-array-test adjust-array.adjustable.1
  (5 :initial-contents '(a b c d e) :adjustable t)
  (4)
  #(a b c d))

(def-adjust-array-test adjust-array.adjustable.2
  (5 :initial-contents '(a b c d e) :adjustable t)
  (8 :initial-element 'x)
  #(a b c d e x x x))

(def-adjust-array-test adjust-array.adjustable.3
  (5 :initial-contents '(a b c d e) :adjustable t)
  (4 :initial-contents '(w x y z))
  #(w x y z))

(def-adjust-array-test adjust-array.adjustable.4
  (5 :initial-contents '(a b c d e) :adjustable t)
  (8 :initial-contents '(8 7 6 5 4 3 2 1))
  #(8 7 6 5 4 3 2 1))

(def-adjust-array-fp-test adjust-array.adjustable.5
  (5 :initial-contents '(a b c d e) :fill-pointer 3 :adjustable t)
  (4)
  (assert (eq (aref a2 3) 'd))
  3 #(a b c))

(def-adjust-array-fp-test adjust-array.adjustable.6
  (5 :initial-contents '(a b c d e) :fill-pointer 3 :adjustable t)
  (4 :fill-pointer nil)
  (assert (eq (aref a2 3) 'd))
  3 #(a b c))

(def-adjust-array-fp-test adjust-array.adjustable.7
  (5 :initial-contents '(a b c d e) :fill-pointer 3 :adjustable t)
  (4 :fill-pointer t)
  nil
  4 #(a b c d))

(def-adjust-array-fp-test adjust-array.adjustable.8
  (5 :initial-contents '(a b c d e) :fill-pointer 3 :adjustable t)
  (4 :fill-pointer 2)
  (assert (equal (list (aref a2 2) (aref a2 3)) '(c d)))
  2 #(a b))

(def-adjust-array-fp-test adjust-array.adjustable.9
  (5 :initial-contents '(a b c d e) :fill-pointer 3 :adjustable t)
  (8 :fill-pointer 5 :initial-element 'x)
  (assert (equal (list (aref a2 5) (aref a2 6) (aref a2 7)) '(x x x)))
  5 #(a b c d e))

(deftest adjust-array.adjustable.10
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)
			 :adjustable t))
	 (a2 (adjust-array a1 4 :displaced-to nil)))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    a2)
  #(a b c d))

(deftest adjust-array.adjustable.11
  (let* ((a0 (make-array 7 :initial-contents '(x a b c d e y)))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :adjustable t))
	 (a2 (adjust-array a1 4)))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    a2)
  #(a b c d))

(deftest adjust-array.adjustable.12
  (let* ((a0 (make-array 7 :initial-contents '(x a b c d e y)))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :adjustable t))
	 (a2 (adjust-array a1 4 :displaced-to a0)))
    (assert (eq a1 a2))
    a2)
  #(x a b c))

(deftest adjust-array.adjustable.13
  (let* ((a0 (make-array 7 :initial-contents '(x a b c d e y)))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :adjustable t))
	 (a2 (make-array 4 :displaced-to a1 :displaced-index-offset 1)))
    (assert (eq a1 (adjust-array a1 5 :displaced-to a0
				 :displaced-index-offset 2)))
    a2)
  #(c d e y))


;;;; Strings

(loop for element-type in '(character base-char)
      for forms = `(
(def-adjust-array-test adjust-array.string.1
  (5 :element-type 'character :initial-contents "abcde")
  (4 :element-type 'character)
  "abcd")

(def-adjust-array-test adjust-array.string.2
  (5 :element-type 'character :initial-contents "abcde")
  (8 :element-type 'character :initial-element #\x)
  "abcdexxx")

(def-adjust-array-test adjust-array.string.3
  (5 :element-type 'character :initial-contents "abcde")
  (4 :element-type 'character :initial-contents "wxyz")
  "wxyz")

(def-adjust-array-test adjust-array.string.4
  (5 :element-type 'character :initial-contents "abcde")
  (8 :element-type 'character :initial-contents "87654321")
  "87654321")

(def-adjust-array-fp-test adjust-array.string.5
  (5 :element-type 'character :initial-contents "abcde" :fill-pointer 3)
  (4 :element-type 'character)
  (assert (eql (aref a2 3) #\d))
  3 "abc")

(def-adjust-array-fp-test adjust-array.string.6
  (5 :element-type 'character :initial-contents "abcde" :fill-pointer 3)
  (4 :element-type 'character :fill-pointer nil)
  (assert (eql (aref a2 3) #\d))
  3 "abc")

(def-adjust-array-fp-test adjust-array.string.7
  (5 :element-type 'character :initial-contents "abcde" :fill-pointer 3)
  (4 :element-type 'character :fill-pointer t)
  nil
  4 "abcd")

(def-adjust-array-fp-test adjust-array.string.8
  (5 :element-type 'character :initial-contents "abcde" :fill-pointer 3)
  (4 :element-type 'character :fill-pointer 2)
  (progn (assert (eql (aref a2 2) #\c))
	 (assert (eql (aref a2 3) #\d)))
  2 "ab")

(def-adjust-array-fp-test adjust-array.string.9
  (5 :element-type 'character :initial-contents "abcde" :fill-pointer 3)
  (8 :element-type 'character :fill-pointer 5 :initial-element #\x)
  (assert (equal (list (aref a2 5) (aref a2 6) (aref a2 7))
		 '(#\x #\x #\x)))
  5 "abcde")

(deftest adjust-array.string.10
  (let* ((a1 (make-array 5 :element-type 'character :initial-contents "abcde"))
	 (a2 (adjust-array a1 4 :displaced-to nil :element-type 'character)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    a2)
  "abcd")

(deftest adjust-array.string.11
  (let* ((a0 (make-array 7 :initial-contents "xabcdey" :element-type 'character))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :element-type 'character))
	 (a2 (adjust-array a1 4 :element-type 'character)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    a2)
  "abcd")

(deftest adjust-array.string.12
  (let* ((a0 (make-array 7 :initial-contents "1234567" :element-type 'character))
	 (a1 (make-array 5 :initial-contents "abcde" :element-type 'character))
	 (a2 (adjust-array a1 4 :displaced-to a0 :element-type 'character)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (equal (multiple-value-list (array-displacement a2))
		   (list a0 0)))
    a2)
  "1234")

(deftest adjust-array.string.13
  (let* ((a0 (make-array 7 :initial-contents "1234567" :element-type 'character))
	 (a1 (make-array 5 :initial-contents "abcde" :element-type 'character))
	 (a2 (adjust-array a1 4 :displaced-to a0
			   :displaced-index-offset 2
			   :element-type 'character)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (equal (multiple-value-list (array-displacement a2))
		   (list a0 2)))
    a2)
  "3456")

(deftest adjust-array.string.14
  (let* ((a0 (make-array 7 :initial-contents "1234567" :element-type 'character))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :element-type 'character))
	 (a2 (adjust-array a1 4 :displaced-to a0 :element-type 'character)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (equal (multiple-value-list (array-displacement a2))
		   (list a0 0)))
    a2)
  "1234")

(deftest adjust-array.string.15
  (let* ((a0 (make-array 7 :initial-contents "1234567" :element-type 'character))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :element-type 'character))
	 (a2 (make-array 4 :displaced-to a1 :displaced-index-offset 1
			 :element-type 'character))
	 (a3 (adjust-array a2 4 :displaced-to a1 :element-type 'character)))
    a3)
  "2345")

(deftest adjust-array.string.16
  (let* ((a0 (make-array 7 :initial-contents "1234567" :element-type 'character))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :element-type 'character))
	 (a2 (adjust-array a1 5 :displaced-to a0 :element-type 'character)))
    a2)
  "12345")

(def-adjust-array-test adjust-array.string.17
  (nil :initial-element #\x :element-type 'character)
  (nil)
  #.(make-array nil :initial-element #\x :element-type 'character))

(def-adjust-array-test adjust-array.string.18
  (nil :initial-element #\x :element-type 'character)
  (nil :initial-contents #\y :element-type 'character)
  #.(make-array nil :initial-element #\y :element-type 'character))

(def-adjust-array-test adjust-array.string.19
  (nil :initial-element #\x :element-type 'character)
  (nil :initial-element #\y :element-type 'character)
  #.(make-array nil :initial-element #\x :element-type 'character))


(deftest adjust-array.string.20
  (let* ((a0 (make-array nil :initial-element #\x :element-type 'character))
	 (a1 (make-array nil :displaced-to a0 :element-type 'character))
	 (a2 (adjust-array a1 nil :element-type 'character)))
    a2)
   #.(make-array nil :initial-element #\x :element-type 'character))

(def-adjust-array-test adjust-array.string.adjustable.1
  (5 :initial-contents "abcde" :adjustable t :element-type 'character)
  (4 :element-type 'character)
  "abcd")

(def-adjust-array-test adjust-array.string.adjustable.2
  (5 :initial-contents "abcde" :adjustable t :element-type 'character)
  (8 :initial-element #\x :element-type 'character)
  "abcdexxx")

(def-adjust-array-test adjust-array.string.adjustable.3
  (5 :initial-contents "abcde" :adjustable t :element-type 'character)
  (4 :initial-contents "wxyz" :element-type 'character)
  "wxyz")

(def-adjust-array-test adjust-array.string.adjustable.4
  (5 :initial-contents "abcde" :adjustable t :element-type 'character)
  (8 :initial-contents "87654321" :element-type 'character)
  "87654321")

(def-adjust-array-fp-test adjust-array.string.adjustable.5
  (5 :initial-contents "abcde" :fill-pointer 3 :adjustable t :element-type 'character)
  (4 :element-type 'character :initial-element #\Space)
  (assert (eql (aref a2 3) #\d))
  3 "abc")

(def-adjust-array-fp-test adjust-array.string.adjustable.6
  (5 :initial-contents "abcde" :fill-pointer 3 :adjustable t :element-type 'character)
  (4 :fill-pointer nil :element-type 'character :initial-element #\?)
  (assert (eql (aref a2 3) #\d))
  3 "abc")

(def-adjust-array-fp-test adjust-array.string.adjustable.7
  (5 :initial-contents "abcde" :fill-pointer 3 :adjustable t :element-type 'character)
  (4 :fill-pointer t :element-type 'character :initial-element #\!)
  nil
  4 "abcd")

(def-adjust-array-fp-test adjust-array.string.adjustable.8
  (5 :initial-contents "abcde" :fill-pointer 3 :adjustable t :element-type 'character)
  (4 :fill-pointer 2 :element-type 'character :initial-element #\X)
  (assert (equal (list (aref a2 2) (aref a2 3)) '(#\c #\d)))
  2 "ab")

(def-adjust-array-fp-test adjust-array.string.adjustable.9
  (5 :initial-contents "abcde" :fill-pointer 3 :adjustable t :element-type 'character)
  (8 :fill-pointer 5 :initial-element #\x :element-type 'character)
  (assert (equal (list (aref a2 5) (aref a2 6) (aref a2 7)) '(#\x #\x #\x)))
  5 "abcde")

(deftest adjust-array.string.adjustable.10
  (let* ((a1 (make-array 5 :initial-contents "abcde"
			 :adjustable t :element-type 'character))
	 (a2 (adjust-array a1 4 :displaced-to nil :element-type 'character)))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    a2)
  "abcd")

(deftest adjust-array.string.adjustable.11
  (let* ((a0 (make-array 7 :initial-contents "xabcdey" :element-type 'character))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :adjustable t :element-type 'character))
	 (a2 (adjust-array a1 4 :element-type 'character)))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    a2)
  "abcd")

(deftest adjust-array.string.adjustable.12
  (let* ((a0 (make-array 7 :initial-contents "xabcdey" :element-type 'character))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :adjustable t :element-type 'character))
	 (a2 (adjust-array a1 4 :displaced-to a0 :element-type 'character)))
    (assert (eq a1 a2))
    a2)
  "xabc")

(deftest adjust-array.string.adjustable.13
  (let* ((a0 (make-array 7 :initial-contents "xabcdey" :element-type 'character))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :adjustable t :element-type 'character))
	 (a2 (make-array 4 :displaced-to a1 :displaced-index-offset 1
			 :element-type 'character)))
    (assert (eq a1 (adjust-array a1 5 :displaced-to a0
				 :displaced-index-offset 2
				 :element-type 'character)))
    a2)
  "cdey")
)
  for forms2 = (subst element-type 'character forms)
  for forms3 = (mapcar #'(lambda (form)
			   (destructuring-bind (dt name . body) form
			     `(,dt ,(if (eql element-type 'character) name
				      (intern (replace (copy-seq (symbol-name name))
						       "BASEST"
						       :start1 13 :end1 19)
					      (symbol-package name)))
				   ,@ body)))
		       forms2)
  do (eval `(progn ,@forms3)))

;; 2-d arrays

(def-adjust-array-test adjust-array.string.21
  ('(4 5) :initial-contents '("12345" "34567" "56789" "78912")
   :element-type 'character)
  ('(2 3))
  #.(make-array '(2 3) :initial-contents '("123" "345")
		:element-type 'character))

(def-adjust-array-test adjust-array.string.22
  ('(4 5) :initial-contents  '("12345" "34567" "56789" "78912")
   :element-type 'character)
  ('(6 8) :initial-element #\0 :element-type 'character)
  #.(make-array '(6 8)
		:initial-contents '("12345000" "34567000" "56789000"
                                    "78912000" "00000000" "00000000")
		:element-type 'character))

(def-adjust-array-test adjust-array.bit-vector.1
  (5 :element-type 'bit :initial-contents #*01100)
  (4 :element-type 'bit)
  #*0110)

(def-adjust-array-test adjust-array.bit-vector.2
  (5 :element-type 'bit :initial-contents #*01100)
  (8 :element-type 'bit :initial-element 1)
  #*01100111)

(def-adjust-array-test adjust-array.bit-vector.3
  (5 :element-type 'bit :initial-contents #*01100)
  (4 :element-type 'bit :initial-contents #*1011)
  #*1011)

(def-adjust-array-test adjust-array.bit-vector.4
  (5 :element-type 'bit :initial-contents #*01100)
  (8 :element-type 'bit :initial-contents #*11110000)
  #*11110000)

(def-adjust-array-fp-test adjust-array.bit-vector.5
  (5 :element-type 'bit :initial-contents #*01100 :fill-pointer 3)
  (4 :element-type 'bit)
  (assert (eql (aref a2 3) 0))
  3 #*011)

(def-adjust-array-fp-test adjust-array.bit-vector.6
  (5 :element-type 'bit :initial-contents #*01100 :fill-pointer 3)
  (4 :element-type 'bit :fill-pointer nil)
  (assert (eql (aref a2 3) 0))
  3 #*011)

(def-adjust-array-fp-test adjust-array.bit-vector.7
  (5 :element-type 'bit :initial-contents #*01100 :fill-pointer 3)
  (4 :element-type 'bit :fill-pointer t)
  nil
  4 #*0110)

(def-adjust-array-fp-test adjust-array.bit-vector.8
  (5 :element-type 'bit :initial-contents #*01100 :fill-pointer 3)
  (4 :element-type 'bit :fill-pointer 2)
  (progn (assert (eql (aref a2 2) 1))
	 (assert (eql (aref a2 3) 0)))
  2 #*01)

(def-adjust-array-fp-test adjust-array.bit-vector.9
  (5 :element-type 'bit :initial-contents #*01100 :fill-pointer 3)
  (8 :element-type 'bit :fill-pointer 5 :initial-element 1)
  (assert (equal (list (aref a2 5) (aref a2 6) (aref a2 7))
		 '(1 1 1)))
  5 #*01100)

(deftest adjust-array.bit-vector.10
  (let* ((a1 (make-array 5 :element-type 'bit :initial-contents #*01100))
	 (a2 (adjust-array a1 4 :displaced-to nil :element-type 'bit)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    a2)
  #*0110)

(deftest adjust-array.bit-vector.11
  (let* ((a0 (make-array 7 :initial-contents #*0011001 :element-type 'bit))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :element-type 'bit))
	 (a2 (adjust-array a1 4 :element-type 'bit)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    a2)
  #*0110)

(deftest adjust-array.bit-vector.12
  (let* ((a0 (make-array 7 :initial-contents #*1010101 :element-type 'bit))
	 (a1 (make-array 5 :initial-contents #*01100 :element-type 'bit))
	 (a2 (adjust-array a1 4 :displaced-to a0 :element-type 'bit)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (equal (multiple-value-list (array-displacement a2))
		   (list a0 0)))
    a2)
  #*1010)

(deftest adjust-array.bit-vector.13
  (let* ((a0 (make-array 7 :initial-contents #*1011101 :element-type 'bit))
	 (a1 (make-array 5 :initial-contents #*01100 :element-type 'bit))
	 (a2 (adjust-array a1 4 :displaced-to a0
			   :displaced-index-offset 2
			   :element-type 'bit)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (equal (multiple-value-list (array-displacement a2))
		   (list a0 2)))
    a2)
  #*1110)

(deftest adjust-array.bit-vector.14
  (let* ((a0 (make-array 7 :initial-contents #*1011001 :element-type 'bit))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :element-type 'bit))
	 (a2 (adjust-array a1 4 :displaced-to a0 :element-type 'bit)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (equal (multiple-value-list (array-displacement a2))
		   (list a0 0)))
    a2)
  #*1011)

(deftest adjust-array.bit-vector.15
  (let* ((a0 (make-array 7 :initial-contents #*1100010 :element-type 'bit))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :element-type 'bit))
	 (a2 (make-array 4 :displaced-to a1 :displaced-index-offset 1
			 :element-type 'bit))
	 (a3 (adjust-array a2 4 :displaced-to a1 :element-type 'bit)))
    a3)
  #*1000)

(deftest adjust-array.bit-vector.16
  (let* ((a0 (make-array 7 :initial-contents #*1011011 :element-type 'bit))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :element-type 'bit))
	 (a2 (adjust-array a1 5 :displaced-to a0 :element-type 'bit)))
    a2)
  #*10110)

(def-adjust-array-test adjust-array.bit-vector.17
  (nil :initial-element 0 :element-type 'bit)
  (nil)
  #.(make-array nil :initial-element 0 :element-type 'bit))

(def-adjust-array-test adjust-array.bit-vector.18
  (nil :initial-element 0 :element-type 'bit)
  (nil :initial-contents 1 :element-type 'bit)
  #.(make-array nil :initial-element 1 :element-type 'bit))

(def-adjust-array-test adjust-array.bit-vector.19
  (nil :initial-element 1 :element-type 'bit)
  (nil :initial-element 0 :element-type 'bit)
  #.(make-array nil :initial-element 1 :element-type 'bit))

(deftest adjust-array.bit-vector.20
  (let* ((a0 (make-array nil :initial-element 1 :element-type 'bit))
	 (a1 (make-array nil :displaced-to a0 :element-type 'bit))
	 (a2 (adjust-array a1 nil :element-type 'bit)))
    a2)
   #.(make-array nil :initial-element 1 :element-type 'bit))

;; 2-d arrays

(def-adjust-array-test adjust-array.bit-vector.21
  ('(4 5) :initial-contents '(#*11100 #*00110 #*00001 #*11111)
   :element-type 'bit)
  ('(2 3))
  #.(make-array '(2 3) :initial-contents '(#*111 #*001)
		:element-type 'bit))

(def-adjust-array-test adjust-array.bit-vector.22
  ('(4 5) :initial-contents  '(#*11100 #*00110 #*00001 #*11111)
   :element-type 'bit)
  ('(6 8) :initial-element 0 :element-type 'bit)
  #.(make-array '(6 8)
		:initial-contents '(#*11100000 #*00110000 #*00001000
                                    #*11111000 #*00000000 #*00000000)
		:element-type 'bit))

;;; Adjustable bit vector tests

(def-adjust-array-test adjust-array.bit-vector.adjustable.1
  (5 :initial-contents '(1 0 1 1 0) :adjustable t :element-type 'bit)
  (4 :element-type 'bit)
  #*1011)

(def-adjust-array-test adjust-array.bit-vector.adjustable.2
  (5 :initial-contents '(1 0 1 0 1) :adjustable t :element-type 'bit)
  (8 :initial-element '1 :element-type 'bit)
  #*10101111)

(def-adjust-array-test adjust-array.bit-vector.adjustable.3
  (5 :initial-contents '(0 1 0 1 0) :adjustable t :element-type 'bit)
  (4 :initial-contents '(1 1 1 0) :element-type 'bit)
  #*1110)

(def-adjust-array-test adjust-array.bit-vector.adjustable.4
  (5 :initial-contents '(1 0 0 1 0) :adjustable t :element-type 'bit)
  (8 :initial-contents '(0 1 0 1 1 0 1 0) :element-type 'bit)
  #*01011010)

(def-adjust-array-fp-test adjust-array.bit-vector.adjustable.5
  (5 :initial-contents '(1 1 1 0 0) :fill-pointer 3 :adjustable t :element-type 'bit)
  (4 :element-type 'bit :initial-element 0)
  (assert (eql (aref a2 3) 0))
  3 #*111)

(def-adjust-array-fp-test adjust-array.bit-vector.adjustable.6
  (5 :initial-contents '(0 0 0 1 1) :fill-pointer 3 :adjustable t :element-type 'bit)
  (4 :fill-pointer nil :element-type 'bit :initial-element 1)
  (assert (eql (aref a2 3) 1))
  3 #*000)

(def-adjust-array-fp-test adjust-array.bit-vector.adjustable.7
  (5 :initial-contents '(1 1 0 1 1) :fill-pointer 3 :adjustable t :element-type 'bit)
  (4 :fill-pointer t :element-type 'bit :initial-element 1)
  nil
  4 #*1101)

(def-adjust-array-fp-test adjust-array.bit-vector.adjustable.8
  (5 :initial-contents '(0 1 1 1 0) :fill-pointer 3 :adjustable t :element-type 'bit)
  (4 :fill-pointer 2 :element-type 'bit :initial-element 0)
  (assert (equal (list (aref a2 2) (aref a2 3)) '(1 1)))
  2 #*01)

(def-adjust-array-fp-test adjust-array.bit-vector.adjustable.9
  (5 :initial-contents '(1 0 0 0 1) :fill-pointer 3 :adjustable t :element-type 'bit)
  (8 :fill-pointer 5 :initial-element 1 :element-type 'bit)
  (assert (equal (list (aref a2 5) (aref a2 6) (aref a2 7)) '(1 1 1)))
  5 #*10001)

(deftest adjust-array.bit-vector.adjustable.10
  (let* ((a1 (make-array 5 :initial-contents '(0 1 1 0 1)
			 :adjustable t :element-type 'bit))
	 (a2 (adjust-array a1 4 :displaced-to nil :element-type 'bit)))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    a2)
  #*0110)

(deftest adjust-array.bit-vector.adjustable.11
  (let* ((a0 (make-array 7 :initial-contents '(0 1 0 1 1 1 0)
			 :element-type 'bit))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :adjustable t :element-type 'bit))
	 (a2 (adjust-array a1 4 :element-type 'bit)))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    a2)
  #*1011)

(deftest adjust-array.bit-vector.adjustable.12
  (let* ((a0 (make-array 7 :initial-contents '(0 0 1 1 1 1 1)
			 :element-type 'bit))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :adjustable t :element-type 'bit))
	 (a2 (adjust-array a1 4 :displaced-to a0 :element-type 'bit)))
    (assert (eq a1 a2))
    a2)
  #*0011)

(deftest adjust-array.bit-vector.adjustable.13
  (let* ((a0 (make-array 7 :initial-contents '(1 0 0 0 0 0 1) :element-type 'bit))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :adjustable t :element-type 'bit))
	 (a2 (make-array 4 :displaced-to a1 :displaced-index-offset 1
			 :element-type 'bit)))
    (assert (eq a1 (adjust-array a1 5 :displaced-to a0
				 :displaced-index-offset 2
				 :element-type 'bit)))
    a2)
  #*0001)

;;; FIXME. specialized integer array tests

;;; FIXNME float array tests

;;; Error cases

(deftest adjust-array.error.1
  (signals-error (adjust-array) program-error)
  t)

(deftest adjust-array.error.2
  (signals-error (adjust-array (make-array 10 :initial-element nil))
		 program-error)
  t)

(deftest adjust-array.error.3
  (signals-error (adjust-array (make-array 10 :initial-element nil)
			       8 :bad t)
		 program-error)
  t)

(deftest adjust-array.error.4
  (signals-error (adjust-array (make-array 10 :initial-element nil)
			       8 :initial-element)
		 program-error)
  t)

(deftest adjust-array.error.5
  (signals-error (adjust-array (make-array 10 :initial-element nil)
			       8
			       :allow-other-keys nil
			       :allow-other-keys t
			       :bad t)
		 program-error)
  t)

(deftest adjust-array.error.6
  (signals-error
   (let ((a (make-array 5 :initial-element 'x)))
     (adjust-array a :fill-pointer 4))
   error)
  t)
