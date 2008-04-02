;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Apr 19 07:28:40 2004
;;;; Contains: Tests of printing of conses

(compile-and-load "printer-aux.lsp")

(in-package :cl-test)

(deftest print.cons.1
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))
     (write-to-string '(|A|) :case :upcase :pretty nil :escape nil)))
  "(A)")

(deftest print.cons.2
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))
     (write-to-string '(|A| |B|) :case :upcase :pretty nil :escape nil)))
  "(A B)")

(deftest print.cons.3
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))
     (write-to-string (cons '|A| '|B|) :case :upcase :pretty nil :escape nil)))
  "(A . B)")

(deftest print.cons.4
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))
     (write-to-string (let ((s '#:|X|)) (cons s s)) :case :upcase :pretty nil :escape t)))
  "(#:X . #:X)")

(deftest print.cons.5
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))
     (write-to-string (let ((s '#:|X|)) (cons s s)) :case :upcase :pretty nil :escape t :circle t)))
  "(#1=#:X . #1#)")

(deftest print.cons.6
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))
     (write-to-string (let ((s1 (make-symbol "X"))
			    (s2 (make-symbol "X")))
			(list s1 s2 s1 s2))
		      :case :upcase :pretty nil :escape t :circle t)))
  "(#1=#:X #2=#:X #1# #2#)")

(deftest print.cons.7
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))
     (write-to-string (let ((a (list 17 nil)))
			(setf (cdr a) a)
			a)
		      :circle t :pretty nil :escape nil)))
  "#1=(17 . #1#)")

;;; Random printing

(deftest print.cons.random.1
  (trim-list
   (loop
    for x = (make-random-cons-tree (random 100))
    repeat 50
    nconc (randomly-check-readability x))
   10)
  nil)

;; random circular cons graphs
#-lispworks
(deftest print.cons.random.2
  (loop repeat 50
	nconc
	(let* ((n 20)
	       (conses (apply #'vector
			      (loop repeat n collect (cons nil nil)))))
	  (loop for x across conses
		for j = (random n)
		for k = (random n)
		do (setf (car x) (elt conses j)
			 (cdr x) (elt conses k)))
	  (randomly-check-readability (elt conses 0) :test #'is-similar
				      :circle t)))
  nil)

;;; Printing with *print-length*

(deftest print.cons.length.1
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))
     (write-to-string '(a) :length 0 :pretty nil :escape nil)))
  "(...)")

(deftest print.cons.length.2
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))
     (write-to-string '(81) :length 1 :pretty nil :escape nil)))
  "(81)")

(deftest print.cons.length.3
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))
     (write-to-string '(4 . 8) :length 1 :pretty nil :escape nil)))
  "(4 . 8)")

(deftest print.cons.length.4
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))   
     (write-to-string '(4 8) :length 1 :pretty nil :escape nil)))
  "(4 ...)")

(deftest print.cons.length.5
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))
     (write-to-string '(a b c d e f g h i j k l m n o p)
		      :case :downcase :length 10
		      :pretty nil :escape nil)))
  "(a b c d e f g h i j ...)")


(deftest print.cons.length.6
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))   
     (write-to-string '(((((((0)))))))
		      :case :downcase :length 3
		      :pretty nil :escape nil)))
  "(((((((0)))))))")

;;; Printing with *print-level*

(deftest print.cons.level.1
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))
     (write-to-string '(a)
		      :case :downcase :level 0
		      :escape nil :pretty nil)))
  "#")

(deftest print.cons.level.2
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))
     (write-to-string '(a)
		      :case :downcase :level 1
		      :escape nil :pretty nil)))
  "(a)")

(deftest print.cons.level.3
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))
     (write-to-string '((a))
		      :case :downcase :level 1
		      :escape nil :pretty nil)))
  "(#)")


(deftest print.cons.level.4
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))
     (write-to-string '(a)
		      :case :downcase :level 2
		      :escape nil :pretty nil)))
  "(a)")

(deftest print.cons.level.5
  (my-with-standard-io-syntax
   (let ((*print-readably* nil))
     (write-to-string '(#(a) #*1101 "abc")
		      :case :downcase :level 1
		      :pretty nil)))
  "(# #*1101 \"abc\")")
