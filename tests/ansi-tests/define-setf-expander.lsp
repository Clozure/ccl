;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 17:19:35 2003
;;;; Contains: Tests of DEFINE-SETF-EXPANDER

(in-package :cl-test)

(def-macro-test define-setf-expander.error.1
  (define-setf-expander nonexistent-access-fn (x)))

;;; Non-error tests

(defun my-car (x) (car x))

(ignore-errors
  (defparameter *define-setf-expander-vals.1*
    (multiple-value-list
     (define-setf-expander my-car (place &environment env)
       (multiple-value-bind (temps vals stores set-form get-form)
	   (get-setf-expansion place env)
	 (declare (ignore stores set-form))
	 (let ((store (gensym))
	       (temp (gensym)))
	   (values
	    `(,@temps ,temp)
	    `(,@vals ,get-form)
	    `(,store)
	    `(progn (rplaca ,temp ,store) ,store)
	    `(my-car ,temp))))))))

(deftest define-setf-expander.1
  *define-setf-expander-vals.1*
  (my-car))

(deftest define-setf-expander.2
  (let ((a (list 'x 'y)))
    (values
     (copy-list a)
     (my-car a)
     (setf (my-car a) 'z)
     a))
  (x y) x z (z y))

(deftest define-setf-expander.3
  (multiple-value-bind (temps vals stores set get)
      (get-setf-expansion '(my-car x))
    (values
     (and (listp temps)
	  (notnot (every #'symbolp temps)))
     (notnot (listp vals))
     (and (listp stores)
	  (= (length stores) 1)
	  (notnot (every #'symbolp stores)))
     (equalt get `(my-car ,(second (second set))))))
  t t t t)

(deftest define-setf-expander.4
  (let ((a (list (list 1))))
    (values
     (copy-tree a)
     (my-car (my-car a))
     (setf (my-car (my-car a)) 2)
     a))
  ((1)) 1 2 ((2)))

(defun my-assoc (key alist)
  (loop for pair in alist
	when (and (consp pair) (eql key (car pair)))
	return pair))

(ignore-errors
  (define-setf-expander my-assoc (key place &environment env)
    (multiple-value-bind (temps vals stores set-form get-form)
	(get-setf-expansion place env)
      (let ((store (gensym))
	    (key-temp (gensym))
	    (pair-temp (gensym))
	    (place-temp (gensym)))
	(return-from my-assoc
	  (values
	   `(,@temps ,key-temp ,place-temp ,pair-temp)
	   `(,@vals ,key ,get-form (my-assoc ,key-temp ,place-temp))
	   `(,store)
	   `(if (null ,pair-temp)
		(let ((,(car stores)
		       (cons (cons ,key-temp ,store) ,place-temp)))
		  ,set-form
		  ,store)
	      (setf (cdr ,pair-temp) ,store))
	   `(cdr ,pair-temp)))))))

(deftest define-setf-expander.5
  (let ((x nil))
    (values
     (copy-tree x)
     (setf (my-assoc 'foo x) 1)
     (copy-tree x)
     (setf (my-assoc 'foo x) 2)
     (copy-tree x)
     (setf (my-assoc 'bar x) 3)
     (copy-tree x)))
  nil 1 ((foo . 1)) 2 ((foo . 2)) 3 ((bar . 3) (foo . 2)))

(deftest define-setf-expander.6
  (let ((n (gensym))
	(doc "D-S-EX.6"))
    (assert (null (documentation n 'setf)))
    (assert (eql (eval `(define-setf-expander ,n ()
			  ,doc (values nil nil nil nil nil)))
		 n))
    (or (documentation n 'setf) doc))
  "D-S-EX.6")

(deftest define-setf-expander.7
  (let ((n (gensym))
	(doc "D-S-EX.7"))
    (assert (null (documentation n 'setf)))
    (assert (eql (eval `(define-setf-expander ,n ()
			  (values nil nil nil nil nil)))
		 n))
    (assert (null (documentation n 'setf)))
    (values
     (setf (documentation n 'setf) doc)
     (or (documentation n 'setf) doc)))
  "D-S-EX.7"
  "D-S-EX.7")
