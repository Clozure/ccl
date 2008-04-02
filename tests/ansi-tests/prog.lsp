;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 19 09:21:57 2002
;;;; Contains: Tests of PROG

(in-package :cl-test)

(deftest prog.1
  (prog ())
  nil)

(deftest prog.2
  (prog () 'a)
  nil)

(deftest prog.3
  (prog () (return 'a))
  a)

(deftest prog.4
  (prog () (return (values 1 2 3 4 5)))
  1 2 3 4 5)

(deftest prog.5
  (let ((x 'a))
    (prog ((x 'b) (y x))
	  (declare (type symbol x y))
	  (return (values x y))))
  b a)

(deftest prog.6
  (let ((x 'a))
    (prog (x) (setq x 'b))
    x)
  a)

(deftest prog.7
  (prog ((i 1) (s 0))
	(declare (type fixnum i s))
	again
	(when (> i 10) (return s))
	(incf s i)
	(incf i)
	(go again))
  55)

(deftest prog.8
  (let ((x 0))
    (prog ((y (incf x)) (z (incf x)))
	  (return (values x y z))))
  2 1 2)

(deftest prog.9
  (flet ((%f () (locally (declare (special z)) z)))
    (prog ((z 10))
	  (declare (special z))
	  (return (%f))))
  10)

(deftest prog.10
  (prog ()
	(return
	 (1+
	  (prog ()
		(go end)
		done
		(return 1)
		end
		(go done))))
	done
	(return 'bad))
  2)

(deftest prog.11
  (let ((x :bad))
    (declare (special x))
    (let ((x :good))
      (prog ((y x))
	    (declare (special x))
	    (return y))))
  :good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest prog.12
  (macrolet
   ((%m (z) z))
   (prog ((x (expand-in-current-env (%m :good)))) (return x)))
  :good)

(def-macro-test prog.error.1 (prog nil))

;;; Tests of PROG*

(deftest prog*.1
  (prog* ())
  nil)

(deftest prog*.2
  (prog* () 'a)
  nil)

(deftest prog*.3
  (prog* () (return 'a))
  a)

(deftest prog*.4
  (prog* () (return (values 1 2 3 4 5)))
  1 2 3 4 5)

(deftest prog*.5
  (let ((x 'a))
    (prog* ((z x) (x 'b) (y x))
	  (declare (type symbol x y))
	  (return (values x y z))))
  b b a)

(deftest prog*.6
  (let ((x 'a))
    (prog* (x) (setq x 'b))
    x)
  a)

(deftest prog*.7
  (prog* ((i 1) (s 0))
	(declare (type fixnum i s))
	again
	(when (> i 10) (return s))
	(incf s i)
	(incf i)
	(go again))
  55)

(deftest prog*.8
  (let ((x 0))
    (prog* ((y (incf x)) (z (incf x)))
	  (return (values x y z))))
  2 1 2)

(deftest prog*.9
  (flet ((%f () (locally (declare (special z)) z)))
    (prog* ((z 10))
	  (declare (special z))
	  (return (%f))))
  10)

(deftest prog*.10
  (prog* ()
	(return
	 (1+
	  (prog* ()
		(go end)
		done
		(return 1)
		end
		(go done))))
	done
	(return 'bad))
  2)

(deftest prog*.11
  (let ((x :bad))
    (declare (special x))
    (let ((x :good))
      (prog* ((y x))
	     (declare (special x))
	     (return y))))
  :good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest prog*.12
  (macrolet
   ((%m (z) z))
   (prog* ((x (expand-in-current-env (%m :good)))) (return x)))
  :good)

(def-macro-test prog*.error.1 (prog* nil))
