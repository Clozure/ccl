;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jun 26 21:55:26 2004
;;;; Contains: Tests of PPRINT-LINEAR

(in-package :cl-test)

;;; When printing a non-list, the result is the same as calling WRITE."
(deftest pprint-linear.1
  (my-with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil))
     (loop for obj in *mini-universe*
	   nconc
	   (and (not (listp obj))
		(let ((s1 (write-to-string obj))
		      (s2 (with-output-to-string (s) (assert (equal (multiple-value-list
								     (pprint-linear s obj))
								    '(nil))))))
		  (unless (equal s1 s2)
		    (list (list obj s1 s2))))))))
  nil)

(deftest pprint-linear.2
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-readably* nil))
     (loop for obj in *mini-universe*
	   nconc
	   (and (not (listp obj))
		(let ((s1 (write-to-string obj))
		      (s2 (with-output-to-string (s) (assert (equal (multiple-value-list
								     (pprint-linear s obj))
								    '(nil))))))
		  (unless (equal s1 s2)
		    (list (list obj s1 s2))))))))
  nil)

(defmacro def-pprint-linear-test (name args expected-value &key (margin 100) (circle nil))
  `(deftest ,name
     (my-with-standard-io-syntax
      (let ((*print-pretty* t)
	    (*print-readably* nil)
	    (*print-right-margin* ,margin)
	    (*package* (find-package "CL-TEST"))
	    (*print-circle* ,circle))
	(with-output-to-string
	  (s)
	  (pprint-linear s ,@args))))
     ,expected-value))

(def-pprint-linear-test pprint-linear.3 ('(|A|)) "(A)")
(def-pprint-linear-test pprint-linear.4 ('(|A|) t) "(A)")
(def-pprint-linear-test pprint-linear.5 ('(|A|) nil) "A")
(def-pprint-linear-test pprint-linear.6 ('(1 2 3 4 5)) "(1 2 3 4 5)")
(def-pprint-linear-test pprint-linear.7 ('((1) (2) #(3) "abc" 5) nil) "(1) (2) #(3) \"abc\" 5")

;;; The fourth argument is ignored
(def-pprint-linear-test pprint-linear.8 ('(1 2 3 4 5) t nil) "(1 2 3 4 5)")
(def-pprint-linear-test pprint-linear.9 ('(1 2 3 4 5) nil t) "1 2 3 4 5")

;;; Takes T, NIL as stream designators

(deftest pprint-linear.10
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-readably* nil)
	 (*print-right-margin* 100))
     (with-output-to-string
       (os)
       (with-input-from-string
	(is "")
	(with-open-stream (*terminal-io* (make-two-way-stream is os))
			  (pprint-linear t '(1 2 3)))))))
  "(1 2 3)")

(deftest pprint-linear.11
  (my-with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100))
     (with-output-to-string (*standard-output*) (pprint-linear nil '(1 2 3)))))
  "(1 2 3)")

(deftest pprint-linear.12
  (my-with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*package* (find-package :cl-test))
	 (obj '(|M| |M| |M| |M| |M| |M| |M| |M| |M| |M|)))
     (loop for i from 1 to 10
	   for result =
	   (let* ((*print-right-margin* i)
		  (s (with-output-to-string (os)
					    (terpri os)
					    (pprint-linear os obj))))
	     (cond
	      ((not (eql (elt s 0) #\Newline))
	       (list :bad1 s))
	      ((not (equal (read-from-string s) obj))
	       (list :bad2 s))
	      ((< (count #\Newline s) (length obj))
	       (list :bad3 s))
	      (t t)))
	   unless (eql result t)
	   collect (list i result))))
  nil)

(deftest pprint-linear.13
  (my-with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*package* (find-package :cl-test))
	 (obj '(|M| |M| |M| |M| |M| |M| |M| |M| |M| |M| |M|)))
     (loop for i from 1 to 10
	   for result =
	   (let* ((*print-right-margin* i)
		  (s (with-output-to-string (os)
					    (terpri os)
					    (pprint-linear os obj nil))))
	     (cond
	      ((not (eql (elt s 0) #\Newline))
	       (list :bad1 s))
	      ((not (equal (read-from-string (concatenate 'string "(" s ")"))
			   obj))
	       (list :bad2 s))
	      ((< (count #\Newline s) (length obj))
	       (list :bad3 s))
	      (t t)))
	   unless (eql result t)
	   collect (list i result))))
  nil)

;;; 
(def-pprint-linear-test pprint-linear.14 ((let ((x (list '|A|))) (list x x)))
  "(#1=(A) #1#)" :circle t)

;;; Error tests

(deftest pprint-linear.error.1
  (signals-error (pprint-linear) program-error)
  t)

(deftest pprint-linear.error.2
  (signals-error (pprint-linear *standard-output*) program-error)
  t)

(deftest pprint-linear.error.3
  (signals-error (pprint-linear *standard-output* nil t t t) program-error)
  t)
