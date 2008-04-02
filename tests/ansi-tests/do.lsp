;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan  8 07:25:18 2005
;;;; Contains: Tests of DO

(in-package :cl-test)


(deftest do.1
  (do ((i 0 (1+ i)))
      ((>= i 10) i))
  10)

(deftest do.2
  (do ((i 0 (1+ j))
       (j 0 (1+ i)))
      ((>= i 10) (+ i j)))
  20)

(deftest do.3
  (let ((x nil))
    (do ((i 0 (1+ i)))
	((>= i 10) x)
      (push i x)))
  (9 8 7 6 5 4 3 2 1 0))

(deftest do.4
  (let ((x nil))
    (do ((i 0 (1+ i)))
	((>= i 10) x)
      (declare (fixnum i))
      (push i x)))
  (9 8 7 6 5 4 3 2 1 0))

(deftest do.5
  (do ((i 0 (1+ i)))
      (nil)
    (when (> i 10) (return i)))
  11)

;;; Zero iterations
(deftest do.6
  (do ((i 0 (+ i 10)))
      ((> i -1) i)
    (return 'bad))
  0)

;;; Tests of go tags
(deftest do.7
  (let ((x nil))
    (do ((i 0 (1+ i)))
	((>= i 10) x)
      (go around)
      small
      (push 'a x)
      (go done)
      big
      (push 'b x)
      (go done)
      around
      (if (> i 4) (go big) (go small))
      done))
  (b b b b b a a a a a))

;;; No increment form
(deftest do.8
  (do ((i 0 (1+ i))
       (x nil))
      ((>= i 10) x)
    (push 'a x))
  (a a a a a a a a a a))

;;; No do locals
(deftest do.9
  (let ((i 0))
    (do ()
	((>= i 10) i)
      (incf i)))
  10)

;;; Return of no values
(deftest do.10
  (do ((i 0 (1+ i)))
      ((> i 10) (values))))

;;; Return of two values
(deftest do.11
  (do ((i 0 (1+ i)))
      ((> i 10) (values i (1+ i))))
  11 12)

;;; The results* list is an implicit progn
(deftest do.12
  (do ((i 0 (1+ i)))
      ((> i 10) (incf i) (incf i) i))
  13)

(deftest do.13
  (do ((i 0 (1+ i)))
      ((> i 10)))
  nil)

;; Special var
(deftest do.14
  (let ((x 0))
    (flet ((%f () (locally (declare (special i))
			   (incf x i))))
      (do ((i 0 (1+ i)))
	  ((>= i 10) x)
	(declare (special i))
	(%f))))
  45)

;;; Confirm that the variables in succesive iterations are
;;; identical
(deftest do.15
  (mapcar #'funcall
	  (let ((x nil))
	    (do ((i 0 (1+ i)))
		((= i 5) x)
	      (push #'(lambda () i) x))))
  (5 5 5 5 5))

;;; Scope of free declarations

(deftest do.16
  (block done
    (let ((x :bad))
      (declare (special x))
      (let ((x :good))
	(do ((i (return-from done x) 0))
	    (t nil)
	  (declare (special x))))))
  :good)

(deftest do.17
  (block done
    (let ((x :good))
      (declare (special x))
      (let ((x :bad))
	(do ((i 0 (return-from done x)))
	    (nil nil)
	  (declare (special x))))))
  :good)

(deftest do.18
  (block done
    (let ((x :good))
      (declare (special x))
      (let ((x :bad))
	(do ((i 0 0))
	    ((return-from done x) nil)
	  (declare (special x))))))
  :good)

(deftest do.19
  (let ((x :good))
    (declare (special x))
    (let ((x :bad))
      (do () (t x)
	(declare (special x)))))
  :good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest do.20
  (let ((result nil))
    (macrolet
     ((%m (z) z))
     (do ((x (expand-in-current-env (%m 0)) (+ x 2)))
	 ((> x 10) result)
	 (push x result))))
  (10 8 6 4 2 0))

(deftest do.21
  (let ((result nil))
    (macrolet
     ((%m (z) z))
     (do ((x 0 (expand-in-current-env (%m (+ x 2)))))
	 ((> x 10) result)
	 (push x result))))
  (10 8 6 4 2 0))

(deftest do.22
  (let ((result nil))
    (macrolet
     ((%m (z) z))
     (do ((x 0 (+ x 2)))
	 ((expand-in-current-env (%m (> x 10))) result)
	 (push x result))))
  (10 8 6 4 2 0))

(deftest do.23
  (let ((result nil))
    (macrolet
     ((%m (z) z))
     (do ((x 0 (+ x 2)))
	 ((> x 10) (expand-in-current-env (%m result)))
	 (push x result))))
  (10 8 6 4 2 0))

(def-macro-test do.error.1
  (do ((i 0 (1+ i))) ((= i 5) 'a)))
