;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 12 14:41:16 2002
;;;; Contains: Tests of UNWIND-PROTECT

(in-package :cl-test)

(deftest unwind-protect.1
  (let ((x nil))
    (unwind-protect
	(push 1 x)
      (incf (car x))))
  (2))

(deftest unwind-protect.2
  (let ((x nil))
    (block foo
      (unwind-protect
	  (progn (push 1 x) (return-from foo x))
	(incf (car x)))))
  (2))

(deftest unwind-protect.3
  (let ((x nil))
    (tagbody
      (unwind-protect
	  (progn (push 1 x) (go done))
	(incf (car x)))
      done)
    x)
  (2))

(deftest unwind-protect.4
  (let ((x nil))
    (catch 'done
      (unwind-protect
	  (progn (push 1 x) (throw 'done x))
	(incf (car x)))))
  (2))

(deftest unwind-protect.5
  (let ((x nil))
    (ignore-errors
      (unwind-protect
	  (progn (push 1 x) (error "Boo!"))
	(incf (car x))))
    x)
  (2))

(deftest unwind-protect.6
  (let ((x nil))
    (block done
      (flet ((%f () (return-from done nil)))
	(unwind-protect (%f)
	  (push 'a x))))
    x)
  (a))

(deftest unwind-protect.7
  (let ((x nil))
    (block done
      (flet ((%f () (return-from done nil)))
	(unwind-protect
	    (unwind-protect (%f)
	      (push 'b x))
	  (push 'a x))))
    x)
  (a b))

(deftest unwind-protect.8
  (let ((x nil))
    (block done
      (unwind-protect
	  (flet ((%f () (return-from done nil)))
	    (unwind-protect
		(unwind-protect (%f)
		  (push 'b x))
	      (push 'a x)))
	(push 'c x)))
    x)
  (c a b))

(deftest unwind-protect.9
  (let ((x nil))
    (handler-case
      (flet ((%f () (error 'type-error :datum 'foo :expected-type nil)))
	(unwind-protect (handler-case (%f))
	  (push 'a x)))
      (type-error () x)))
  (a))

;;; No implicit tagbody
(deftest unwind-protect.10
  (block done
    (tagbody
     (unwind-protect
	 'foo
       (go 10)
       10
       (return-from done 'bad))
     10
     (return-from done 'good)))
  good)

;;; Executes all forms of the implicit progn
(deftest unwind-protect.11
  (let ((x nil) (y nil))
    (values
     (block nil
       (unwind-protect (return 'a)
	 (setf y 'c)
	 (setf x 'b)))
     x y))
  a b c)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest unwind-protect.12
  (macrolet
   ((%m (z) z))
   (unwind-protect (expand-in-current-env (%m :good)) :bad))
  :good)

(deftest unwind-protect.13
  (macrolet
   ((%m (z) z))
   (unwind-protect :good (expand-in-current-env (%m :bad))))
  :good)


