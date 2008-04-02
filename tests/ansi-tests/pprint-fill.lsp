;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Jun 25 22:03:01 2004
;;;; Contains: Tests of PPRINT-FILL

(in-package :cl-test)

;;; When printing a non-list, the result is the same as calling WRITE."
(deftest pprint-fill.1
  (my-with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil))
     (loop for obj in *mini-universe*
	   nconc
	   (and (not (listp obj))
		(let ((s1 (write-to-string obj))
		      (s2 (with-output-to-string (s) (pprint-fill s obj))))
		  (unless (equal s1 s2)
		    (list (list obj s1 s2))))))))
  nil)

(deftest pprint-fill.2
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-readably* nil))
     (loop for obj in *mini-universe*
	   nconc
	   (and (not (listp obj))
		(let ((s1 (write-to-string obj))
		      (s2 (with-output-to-string (s) (pprint-fill s obj))))
		  (unless (equal s1 s2)
		    (list (list obj s1 s2))))))))
  nil)

(defmacro def-pprint-fill-test (name args expected-value &key (margin 100) (circle nil) (len nil))
  `(deftest ,name
     (my-with-standard-io-syntax
      (let ((*print-pretty* t)
	    (*print-readably* nil)
	    (*print-right-margin* ,margin)
	    (*package* (find-package "CL-TEST"))
	    (*print-length* ,len)
	    (*print-circle* ,circle))
	(with-output-to-string (s) (pprint-fill s ,@args))))
     ,expected-value))

(def-pprint-fill-test pprint-fill.3 ('(|A|)) "(A)")
(def-pprint-fill-test pprint-fill.4 ('(|A|) t) "(A)")
(def-pprint-fill-test pprint-fill.5 ('(|A|) nil) "A")
(def-pprint-fill-test pprint-fill.6 ('(1 2 3 4 5)) "(1 2 3 4 5)")
(def-pprint-fill-test pprint-fill.7 ('((1) (2) #(3) "abc" 5) nil) "(1) (2) #(3) \"abc\" 5")

;;; The fourth argument is ignored
(def-pprint-fill-test pprint-fill.8 ('(1 2 3 4 5) t nil) "(1 2 3 4 5)")
(def-pprint-fill-test pprint-fill.9 ('(1 2 3 4 5) nil t) "1 2 3 4 5")

;;; Takes T, NIL as stream designators

(deftest pprint-fill.10
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-readably* nil)
	 (*print-right-margin* 100))
     (with-output-to-string
       (os)
       (with-input-from-string
	(is "")
	(with-open-stream (*terminal-io* (make-two-way-stream is os))
			  (pprint-fill t '(1 2 3)))))))
  "(1 2 3)")

(deftest pprint-fill.11
  (my-with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100))
     (with-output-to-string (*standard-output*) (pprint-fill nil '(1 2 3)))))
  "(1 2 3)")


;;; Now tests for cases that should be wrapped
;;; It's not entirely clear what they should be doing
;;; but check for some obvious properties

(deftest pprint-fill.12
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
					    (pprint-fill os obj))))
	     (cond
	      ((not (eql (elt s 0) #\Newline))
	       (list :bad1 s))
	      ((not (equal (read-from-string s) obj))
	       (list :bad2 s))
	      ((not (find #\Newline s :start 1))
	       (list :bad3 s))
	      (t t)))
	   unless (eql result t)
	   collect (list i result))))
  nil)


(deftest pprint-fill.13
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
					    (pprint-fill os obj nil))))
	     (cond
	      ((not (eql (elt s 0) #\Newline))
	       (list :bad1 s))
	      ((not (equal (read-from-string (concatenate 'string "(" s ")"))
			   obj))
	       (list :bad2 s))
	      ((not (find #\Newline s :start 1))
	       (list :bad3 s))
	      (t t)))
	   unless (eql result t)
	   collect (list i result))))
  nil)

;;; 
(def-pprint-fill-test pprint-fill.14 ((let ((x (list '|A|))) (list x x)))
  "(#1=(A) #1#)" :circle t)

(def-pprint-fill-test pprint-fill.15 ((let ((x (list '|A|))) (setf (cdr x) x) x))
  "#1=(A . #1#)" :circle t :len 500)


;;; Test that pprint-fill returns NIL

(deftest pprint-fill.return-values.1
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*package* (find-package "CL-TEST")))
     (with-open-stream (s (make-broadcast-stream))
		       (pprint-fill s '(a b)))))
  nil)

(deftest pprint-fill.return-values.2
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*package* (find-package :cl-test)))
     (with-open-stream (s (make-broadcast-stream))
		       (pprint-fill s 10 nil t))))
  nil)

;;; Error tests

(deftest pprint-fill.error.1
  (signals-error (pprint-fill) program-error)
  t)

(deftest pprint-fill.error.2
  (signals-error (pprint-fill *standard-output*) program-error)
  t)

(deftest pprint-fill.error.3
  (signals-error (pprint-fill *standard-output* nil t t t) program-error)
  t)
