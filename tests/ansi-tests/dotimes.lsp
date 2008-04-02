;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan  8 07:27:15 2005
;;;; Contains: Tests of DOTIMES

(in-package :cl-test)


(deftest dotimes.1
  (dotimes (i 10))
  nil)

(deftest dotimes.2
  (dotimes (i 10 'a))
  a)

(deftest dotimes.3
  (dotimes (i 10 (values))))

(deftest dotimes.3a
  (dotimes (i 10 (values 'a 'b 'c)))
  a b c)

(deftest dotimes.4
  (let ((x nil))
    (dotimes (i 5 x) (push i x)))
  (4 3 2 1 0))

(deftest dotimes.5
  (let ((x nil))
    (dotimes (i 0 x) (push i x)))
  nil)

(deftest dotimes.6
  (block done
    (dotimes (i -1 'good)
      (return-from done 'bad)))
  good)

(deftest dotimes.7
  (block done
    (dotimes (i (1- most-negative-fixnum) 'good)
      (return-from done 'bad)))
  good)

;;; Implicit nil block has the right scope
(deftest dotimes.8
  (block nil
    (dotimes (i (return 1)))
    2)
  2)

(deftest dotimes.9
  (block nil
    (dotimes (i 10 (return 1)))
    2)
  2)

(deftest dotimes.10
  (block nil
    (dotimes (i 10) (return 1))
    2)
  2)

(deftest dotimes.11
  (let ((x nil))
    (dotimes (i 10)
      (push i x)
      (when (= i 5) (return x))))
  (5 4 3 2 1 0))

;;; Check there's an implicit tagbody
(deftest dotimes.12
  (let ((even nil)
	(odd nil))
    (dotimes (i 8 (values (reverse even)
			  (reverse odd)))
      (when (evenp i) (go even))
      (push i odd)
      (go done)
      even
      (push i even)
      done))
  (0 2 4 6)
  (1 3 5 7))

;;; Check that at the time the result form is evaluated,
;;; the index variable is set to the number of times the loop
;;; was executed.

(deftest dotimes.13
  (let ((i 100))
    (dotimes (i 10 i)))
  10)

(deftest dotimes.14
  (let ((i 100))
    (dotimes (i 0 i)))
  0)

(deftest dotimes.15
  (let ((i 100))
    (dotimes (i -1 i)))
  0)

;;; Check that the variable is not bound in the count form
(deftest dotimes.16
  (let ((i nil))
    (values
     i
     (dotimes (i (progn (setf i 'a) 10) i))
     i))
  nil 10 a)

;;; Check special variable decls
(deftest dotimes.17
  (let ((i 0) (y nil))
    (declare (special i))
    (flet ((%f () i))
      (dotimes (i 4)
	(push (%f) y)))
    y)
  (0 0 0 0))

(deftest dotimes.17a
  (let ((i 0) (y nil) (bound 4))
    (declare (special i))
    (flet ((%f () i))
      (dotimes (i bound)
	(push (%f) y)))
    y)
  (0 0 0 0))

(deftest dotimes.18
  (let ((i 0) (y nil))
    (declare (special i))
    (flet ((%f () i))
      (dotimes (i 4)
	(declare (special i))
	(push (%f) y)))
    y)
  (3 2 1 0))

(deftest dotimes.18a
  (let ((i 0) (y nil) (bound 4))
    (declare (special i))
    (flet ((%f () i))
      (dotimes (i bound)
	(declare (special i))
	(push (%f) y)))
    y)
  (3 2 1 0))

(deftest dotimes.19
  (dotimes (i 100 i))
  100)

(deftest dotimes.20
  (dotimes (i -100 i))
  0)

(deftest dotimes.21
  (let ((x 0))
    (dotimes (i (1- most-negative-fixnum) (values i x))
      (declare (type fixnum i))
      (incf x)))
  0 0)

;;; Scope of free declarations

(deftest dotimes.22
  (block done
    (let ((x :bad))
      (declare (special x))
      (let ((x :good))
	(dotimes (i (return-from done x))
	  (declare (special x))))))
  :good)

(deftest dotimes.23
  (let ((x :good))
    (declare (special x))
    (let ((x :bad))
      (dotimes (i 10 x)
	(declare (special x)))))
  :good)

(deftest dotimes.23a
  (let ((x :good) (bound 10))
    (declare (special x))
    (let ((x :bad))
      (dotimes (i bound x)
	(declare (special x)))))
  :good)

(deftest dotimes.24
  (let ((bound 4) (j 0))
    (values
     (dotimes (i bound)
       (incf j) (decf bound))
     bound j))
  nil 0 4)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest dotimes.25
  (macrolet
   ((%m (z) z))
   (let (result)
     (dotimes (i (expand-in-current-env (%m 4)) result)
       (push i result))))
  (3 2 1 0))

(deftest dotimes.26
  (macrolet
   ((%m (z) z))
   (let (result)
     (dotimes (i 4 (expand-in-current-env (%m result)))
       (push i result))))
  (3 2 1 0))

(def-macro-test dotimes.error.1
  (dotimes (i 10)))
