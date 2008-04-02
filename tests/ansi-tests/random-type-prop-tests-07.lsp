;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar 13 17:03:52 2005
;;;; Contains: Random type prop tests, part 7 (strings)

(in-package :cl-test)

(def-type-prop-test simple-string-p 'simple-string-p '(t) 1)
(def-type-prop-test char 'char (list 'string (index-type-for-dim 0)) 2)
(def-type-prop-test schar 'schar (list 'simple-string (index-type-for-dim 0)) 2)

(def-type-prop-test string 'string '((or string symbol character)) 1)
(def-type-prop-test string-upcase 'string-upcase '(string) 1)
(def-type-prop-test string-downcase 'string-downcase '(string) 1)
(def-type-prop-test string-capitalize 'string-capitalize '(string) 1)

(def-type-prop-test string-trim.1 'string-trim '(string string) 2)
(def-type-prop-test string-trim.2 'string-trim (list #'(lambda () (make-list-type (random 10) 'null 'character))
						     'string)
  2)
(def-type-prop-test string-left-trim.1 'string-left-trim '(string string) 2)
(def-type-prop-test string-left-trim.2 'string-left-trim (list #'(lambda () (make-list-type (random 10) 'null 'character))
							       'string)
  2)
(def-type-prop-test string-right-trim.1 'string-right-trim '(string string) 2)
(def-type-prop-test string-right-trim.2 'string-right-trim
  (list #'(lambda () (make-list-type (random 10) 'null 'character)) 'string)
  2)

(defmacro def-string-comparison-type-prop-test (op)
  (flet ((%makename (n) (intern (format nil "~A.~A" op n) :cl-test)))
    `(progn
       (def-type-prop-test ,(%makename 1) ',op '(string string) 2)
       (def-type-prop-test ,(%makename 2) ',op 
	 `(string string (eql :start1) ,#'index-type-for-v1)
	 4)
       (def-type-prop-test ,(%makename 3) ',op 
	 `(string string (eql :start2) ,#'index-type-for-v2)
	 4)
       (def-type-prop-test ,(%makename 4) ',op 
	 `(string string (eql :end1) ,#'end-type-for-v1)
	 4)
       (def-type-prop-test ,(%makename 5) ',op 
	 `(string string (eql :end2) ,#'end-type-for-v2)
	 4)
       (def-type-prop-test ,(%makename 6) ',op 
	 `(string string
		  (eql :start1) ,#'index-type-for-v1
		  (eql :end1) ,#'end-type-for-v1)
	 6)
       (def-type-prop-test ,(%makename 7) ',op 
	 `(string string
		  (eql :start1) ,#'index-type-for-v1
		  (eql :end2) ,#'end-type-for-v2)
	 6)
       (def-type-prop-test ,(%makename 8) ',op 
	 `(string string
		  (eql :start2) ,#'index-type-for-v2
		  (eql :end1) ,#'end-type-for-v1)
	 6)
       (def-type-prop-test ,(%makename 9) ',op 
	 `(string string
		  (eql :start2) ,#'index-type-for-v2
		  (eql :end2) ,#'end-type-for-v2)
	 6)
       (def-type-prop-test ,(%makename 10) ',op 
	 `(string string
		  (eql :start1) ,#'index-type-for-v1
		  (eql :start2) ,#'index-type-for-v2
		  (eql :end1) ,#'end-type-for-v1)
	 8)
       (def-type-prop-test ,(%makename 11) ',op 
	 `(string string
		  (eql :start1) ,#'index-type-for-v1
		  (eql :start2) ,#'index-type-for-v2
		  (eql :end2) ,#'end-type-for-v2)
	 8)
       (def-type-prop-test ,(%makename 12) ',op 
	 `(string string
		  (eql :start1) ,#'index-type-for-v1
		  (eql :end2) ,#'end-type-for-v2
		  (eql :end1) ,#'end-type-for-v1)
	 8)
       (def-type-prop-test ,(%makename 13) ',op 
	 `(string string
		  (eql :start2) ,#'index-type-for-v2
		  (eql :end2) ,#'end-type-for-v2
		  (eql :end1) ,#'end-type-for-v1)
	 8)
       (def-type-prop-test ,(%makename 14) ',op 
	 `(string string
		  (eql :start1) ,#'index-type-for-v1
		  (eql :start2) ,#'index-type-for-v2
		  (eql :end2) ,#'end-type-for-v2
		  (eql :end1) ,#'end-type-for-v1)
	 10)
       )))
     
(def-string-comparison-type-prop-test string=)
(def-string-comparison-type-prop-test string/=)
(def-string-comparison-type-prop-test string<)
(def-string-comparison-type-prop-test string<=)
(def-string-comparison-type-prop-test string>)
(def-string-comparison-type-prop-test string>=)

(def-string-comparison-type-prop-test string-equal)
(def-string-comparison-type-prop-test string-not-equal)
(def-string-comparison-type-prop-test string-lessp)
(def-string-comparison-type-prop-test string-greaterp)
(def-string-comparison-type-prop-test string-not-lessp)
(def-string-comparison-type-prop-test string-not-greaterp)

(def-type-prop-test stringp 'stringp '(t) 1)

(def-type-prop-test make-string.1 'make-string '((integer 0 100) (eql :initial-element) character) 3)
(def-type-prop-test make-string.2 'make-string `((integer 0 100) (eql :initial-element) character
						 (eql :element-type)
						 ,#'(lambda (&rest args)
						      `(eql (and character
								 ,(make-random-type-containing (third args))))))
  5)
