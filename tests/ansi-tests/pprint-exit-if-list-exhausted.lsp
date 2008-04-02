;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jul  6 06:11:01 2004
;;;; Contains: Tests of PPRINT-EXIT-IF-LIST-EXHAUSTED, PPRINT-POP

(in-package :cl-test)

(deftest pprint-exit-if-list-exhausted.1
  (with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 )
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(1 2))
	(assert (equal (multiple-value-list
			(pprint-exit-if-list-exhausted))
		       '(nil)))
	(write (pprint-pop) :stream os)
	(assert (equal (multiple-value-list
			(pprint-exit-if-list-exhausted))
		       '(nil)))
	(write #\Space :stream os)
	(write (pprint-pop) :stream os)
	(pprint-exit-if-list-exhausted)
	(assert nil)))))
  "1 2")

(deftest pprint-exit-if-list-exhausted.2
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 )
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(1 2))
	(assert (equal (multiple-value-list
			(pprint-exit-if-list-exhausted))
		       '(nil)))
	(write (pprint-pop) :stream os)
	(assert (equal (multiple-value-list
			(pprint-exit-if-list-exhausted))
		       '(nil)))
	(write #\Space :stream os)
	(write (pprint-pop) :stream os)
	(pprint-exit-if-list-exhausted)
	(assert nil)))))
  "1 2")

(deftest pprint-exit-if-list-exhausted.3
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 )
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(1 . 2))
	(assert (equal (multiple-value-list
			(pprint-exit-if-list-exhausted))
		       '(nil)))
	(write (pprint-pop) :stream os)
	(write #\Space :stream os)
	(assert (equal (multiple-value-list
			(pprint-exit-if-list-exhausted))
		       '(nil)))
	(pprint-pop)
	(assert nil)))))
  "1 . 2")

(deftest pprint-exit-if-list-exhausted.4
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 )
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(1 . 2) :prefix "[" :suffix "]")
	(assert (equal (multiple-value-list
			(pprint-exit-if-list-exhausted))
		       '(nil)))
	(write (pprint-pop) :stream os)
	(write #\Space :stream os)
	(assert (equal (multiple-value-list
			(pprint-exit-if-list-exhausted))
		       '(nil)))
	(pprint-pop)
	(assert nil)))))
  "[1 . 2]")

;;; Tests focusing on pprint-pop

(deftest pprint-pop.1
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (*print-length* 0))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os nil)
	(pprint-pop)
	(assert nil)))))
  "...")

(deftest pprint-pop.2
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (*print-length* 0))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os 1)
	(pprint-pop)))))
  "1")

(deftest pprint-pop.3
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (*print-length* 1))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(1))
	(assert (equal '(1) (multiple-value-list (pprint-pop))))))))
  "")

(deftest pprint-pop.4
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (*print-length* 0))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(1 2 3) :prefix "{" :suffix "}")
	(pprint-pop)
	(assert nil)))))
  "{...}")

(deftest pprint-pop.5
  (flet ((%f (len)
	     (with-standard-io-syntax
	      (let ((*print-pretty* t)
		    (*print-escape* nil)
		    (*print-right-margin* 100)
		    (*print-readably* nil)
		    (*print-length* len))
		(with-output-to-string
		  (os)
		  (pprint-logical-block
		   (os '(1 2 3 4 5) :prefix "{" :suffix "}")
		   (pprint-exit-if-list-exhausted)
		   (write (pprint-pop) :stream os)
		   (loop (pprint-exit-if-list-exhausted)
			 (write #\Space :stream os)
			 (write (pprint-pop) :stream os))))))))
    (values (%f 0) (%f 1) (%f 2) (%f 3) (%f 4) (%f 5) (%f 6)))
  "{...}"
  "{1 ...}"
  "{1 2 ...}"
  "{1 2 3 ...}"
  "{1 2 3 4 ...}"
  "{1 2 3 4 5}"
  "{1 2 3 4 5}")

(deftest pprint-pop.6
  (flet ((%f (len)
	     (with-standard-io-syntax
	      (let ((*print-pretty* t)
		    (*print-escape* nil)
		    (*print-right-margin* 100)
		    (*print-readably* nil)
		    (*print-length* len))
		(with-output-to-string
		  (os)
		  (pprint-logical-block
		   (os '(1 2 . 3) :prefix "{" :suffix "}")
		   (pprint-exit-if-list-exhausted)
		   (write (pprint-pop) :stream os)
		   (loop (pprint-exit-if-list-exhausted)
			 (write #\Space :stream os)
			 (write (pprint-pop) :stream os))))))))
    (values (%f 0) (%f 1) (%f 2) (%f 3) (%f 4)))
  "{...}"
  "{1 ...}"
  "{1 2 . 3}"
  "{1 2 . 3}"
  "{1 2 . 3}")

;;; pprint-pop and circularity/sharing

(deftest pprint-pop.7
  (flet ((%f (len)
	     (with-standard-io-syntax
	      (let ((*print-pretty* t)
		    (*print-escape* nil)
		    (*print-right-margin* 100)
		    (*print-readably* nil)
		    (*print-length* len)
		    (*print-circle* t))
		(with-output-to-string
		  (os)
		  (let* ((tail (list 1))
			 (x (list* tail 2 tail)))
		    (pprint-logical-block
		     (os x :prefix "<" :suffix ">")
		     (pprint-exit-if-list-exhausted)
		     (write (pprint-pop) :stream os)
		     (loop (pprint-exit-if-list-exhausted)
			   (write #\Space :stream os)
			   (write (pprint-pop) :stream os)))))))))
    (values (%f nil) (%f 0) (%f 1) (%f 2) (%f 3) (%f 4)))
  "<#1=(1) 2 . #1#>"
  "<...>"
  "<(1) ...>"
  "<(1) 2 ...>"
  "<#1=(1) 2 . #1#>"
  "<#1=(1) 2 . #1#>")

(deftest pprint-pop.8
  (flet ((%f (len)
	     (with-standard-io-syntax
	      (let ((*print-pretty* t)
		    (*print-escape* nil)
		    (*print-right-margin* 100)
		    (*print-readably* nil)
		    (*print-length* len)
		    (*print-circle* t))
		(with-output-to-string
		  (os)
		  (let* ((tail (list 2))
			 (x (list* 1 tail)))
		    (setf (cdr tail) tail)
		    (pprint-logical-block
		     (os x :prefix "[[" :suffix "]]")
		     (pprint-exit-if-list-exhausted)
		     (write (pprint-pop) :stream os)
		     (loop (pprint-exit-if-list-exhausted)
			   (write #\Space :stream os)
			   (write (pprint-pop) :stream os)))))))))
    (values (%f 0) (%f 1) (%f 2) (%f 3) (%f 10) (%f 20)))
  "[[...]]"
  "[[1 ...]]"
  "[[1 2 ...]]"
  "[[1 . #1=(2 . #1#)]]"
  "[[1 . #1=(2 . #1#)]]"
  "[[1 . #1=(2 . #1#)]]")

;;; pprint-pop when pprint-logical-block is given NIL

(deftest pprint-pop.9
  (flet ((%f (len)
	     (with-standard-io-syntax
	      (let ((*print-pretty* t)
		    (*print-escape* nil)
		    (*print-right-margin* 100)
		    (*print-readably* nil)
		    (*print-length* len))
		(with-output-to-string
		  (os)
		  (pprint-logical-block
		   (os nil :prefix "{" :suffix "}")
		   (let ((vals (multiple-value-list (pprint-pop))))
		     (assert (equal vals '(nil)) () "First call returned ~A" vals))
		   (write 1 :stream os)
		   (write #\Space :stream os)
		   (let ((vals (multiple-value-list (pprint-pop))))
		     (assert (equal vals '(nil)) () "Second call returned ~A" vals))
		   (write 2 :stream os)
		   (write #\Space :stream os)
		   (let ((vals (multiple-value-list (pprint-pop))))
		     (assert (equal vals '(nil)) () "Third call returned ~A" vals))
		   (write 3 :stream os)
		   ))))))
    (values (%f nil) (%f 0) (%f 1) (%f 2) (%f 3) (%f 4)))
  "{1 2 3}"
  "{...}"
  "{1 ...}"
  "{1 2 ...}"
  "{1 2 3}"
  "{1 2 3}")

;;; Error cases

(deftest pprint-exit-if-list-exhausted.error.1
  (signals-error (pprint-exit-if-list-exhausted) error)
  t)

(deftest pprint-exit-if-list-exhausted.error.1-unsafe
  (locally (declare (optimize (safety 0)))
	   (signals-error (locally (declare (optimize (safety 0)))
				   (pprint-exit-if-list-exhausted))
			  error))
  t)

(deftest pprint-pop.error.1
  (signals-error (pprint-pop) error)
  t)


(deftest pprint-pop.error.1-unsafe
  (locally (declare (optimize (safety 0)))
	   (signals-error (locally (declare (optimize (safety 0))) (pprint-pop)) error))
  t)
