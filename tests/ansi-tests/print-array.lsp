;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Apr 22 22:38:11 2004
;;;; Contains: Tests of printing of arrays (other than vectors)

(compile-and-load "printer-aux.lsp")

(in-package :cl-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zero dimensional arrays

(deftest print.array.0.1
  (let ((a (make-array nil :initial-element 0)))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#0A0")

(deftest print.array.0.2
  (with-standard-io-syntax
   (let ((a (make-array nil :initial-element '|A|))
	 (*package* (find-package "CL-TEST")))
     (write-to-string a :readably nil :array t)))
  "#0AA")

(deftest print.array.0.3
  (let* ((a (make-array nil :initial-element 0))
	 (result (write-to-string a :readably nil :array nil)))
    (values
     (subseq result 0 2)
     (subseq result (1- (length result)))))
  "#<" ">")

(deftest print.array.0.4
   (let ((a (make-array nil :initial-element 0 :adjustable t)))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#0A0")

(deftest print.array.0.5
   (let* ((a (make-array nil :initial-element 0 :adjustable t))
	  (b (make-array nil :displaced-to a :displaced-index-offset 0)))
    (with-standard-io-syntax
     (write-to-string b :readably nil :array t)))
  "#0A0")

(deftest print.array.0.6
  (let ((a (make-array nil :initial-element 0
		       :element-type '(integer 0 2))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#0A0")

(deftest print.array.0.7
  (loop for a = (make-array nil :initial-element (- (random 1000000) 500000))
	repeat 30 nconc (randomly-check-readability a :test #'is-similar))
  nil)

(deftest print.array.0.8
  (loop for i from 1 to 64
	for type = `(unsigned-byte ,i)
	nconc
	(let ((a (make-array nil :initial-element 1 :element-type type)))
	  (loop repeat 5 nconc (randomly-check-readability a :test #'is-similar
							   :can-fail t))))
  nil)

(deftest print.array.0.9
  (loop for a = (make-array nil :initial-element (random 1000000) :adjustable t)
	repeat 30
	nconc (randomly-check-readability a :test #'is-similar))
  nil)

(deftest print.array.0.10
  (loop for a = (make-array nil :initial-element (random 1000000000))
	for b = (make-array nil :displaced-to a :displaced-index-offset 0)
	repeat 30 nconc (randomly-check-readability b :test #'is-similar))
  nil)

(deftest print.array.0.11
  (loop for type in '(short-float single-float double-float long-float float)
	for zero = (coerce 0 type)
	for a = (make-array nil :initial-element zero
			    :element-type type)
	nconc
	(loop repeat 30 nconc (randomly-check-readability a :test #'is-similar
							  :can-fail t)))
  nil)

(deftest print.array.0.12
  (loop for type0 in '(short-float single-float double-float long-float float)
	for type = `(complex ,type0)
	for zero = (complex (coerce 0.0s0 type0))
	for a = (make-array nil :initial-element zero
			    :element-type type)
	nconc
	(loop repeat 30 nconc (randomly-check-readability a :test #'is-similar
							  :can-fail t)))
  nil)

(deftest print.array.0.13
  (let ((result (write-to-string (make-array nil :initial-element 0)
				 :readably nil :array nil)))
    (values
     (subseq result 0 2)
     (subseq result (1- (length result)))))
  "#<" ">")

(deftest print.array.0.14
  (loop for i from 1 to 64
	for type = `(unsigned-byte ,i)
	for a = (make-array nil :element-type type :initial-element 1)
	for result = (write-to-string a :readably nil :array nil)
	unless (and (string= (subseq result 0 2) "#<")
		    (string= (subseq result (1- (length result))) ">"))
	collect (list i result))
  nil)

(deftest print.array.0.15
  (loop for i from 1 to 64
	for type = `(signed-byte ,i)
	for a = (make-array nil :element-type type :initial-element -1)
	for result = (write-to-string a :readably nil :array nil)
	unless (and (string= (subseq result 0 2) "#<")
		    (string= (subseq result (1- (length result))) ">"))
	collect (list i result))
  nil)

(deftest print.array.0.16
  (loop for type in '(short-float single-float double-float long-float)
	for a = (make-array nil :element-type type
			    :initial-element (coerce 17 type))
	for result = (write-to-string a :readably nil :array nil)
	unless (and (string= (subseq result 0 2) "#<")
		    (string= (subseq result (1- (length result))) ">"))
	collect (list type result))
  nil)

(deftest print.array.0.17
  (loop for type0 in '(short-float single-float double-float
				   long-float float real)
	for type = `(complex ,type0)
	for a = (make-array nil :element-type type
			    :initial-element (complex 0 (coerce 3 type0)))
	for result = (write-to-string a :readably nil :array nil)
	unless (and (string= (subseq result 0 2) "#<")
		    (string= (subseq result (1- (length result))) ">"))
	collect (list type result))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Two-d arrays
(deftest print.array.2.1
  (let ((a (make-array '(1 1) :initial-contents '((1)))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A((1))")

(deftest print.array.2.2
  (let ((a (make-array '(2 3) :initial-contents '((1 3 8)(2 6 10)))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A((1 3 8) (2 6 10))")

(deftest print.array.2.3
  (let ((a (make-array '(0 1))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A()")

(deftest print.array.2.4
  (let ((a (make-array '(1 0))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A(())")

(deftest print.array.2.5
  (let ((a (make-array '(0 0))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A()")

(deftest print.array.2.6
  (let ((a (make-array '(10 0))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A(() () () () () () () () () ())")

(deftest print.array.2.7
  (let* ((a (make-array '(3 3) :initial-contents '((1 3 8) (2 67 121) (65 432 6))))
	 (b (make-array '(3 3) :displaced-to a
			:displaced-index-offset 0)))
    (with-standard-io-syntax
     (write-to-string b :readably nil :array t)))
  "#2A((1 3 8) (2 67 121) (65 432 6))")

(deftest print.array.2.8
  (let* ((a (make-array '(3 3) :initial-contents '((1 3 8) (2 67 121) (65 432 6))))
	 (b (make-array '(2 3) :displaced-to a
			:displaced-index-offset 0)))
    (with-standard-io-syntax
     (write-to-string b :readably nil :array t)))
  "#2A((1 3 8) (2 67 121))")

(deftest print.array.2.9
  (let* ((a (make-array '(3 3) :initial-contents '((1 3 8) (2 67 121) (65 432 6))))
	 (b (make-array '(2 2) :displaced-to a
			:displaced-index-offset 4)))
    (with-standard-io-syntax
     (write-to-string b :readably nil :array t)))
  "#2A((67 121) (65 432))")

(deftest print.array.2.10
  (let* ((a (make-array '(3 3) :initial-contents '((1 3 8) (2 67 121) (65 432 6))))
	 (b (make-array '(2 2) :displaced-to a
			:displaced-index-offset 4
			:adjustable t)))
    (with-standard-io-syntax
     (write-to-string b :readably nil :array t)))
  "#2A((67 121) (65 432))")

(deftest print.array.2.11
  (let* ((a (make-array '(3 4)
			:initial-contents '((7 8 9 10) (65 12 42 -1) (:|W| :|X| :|Y| :|Z| ))
			:adjustable t)))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A((7 8 9 10) (65 12 42 -1) (:W :X :Y :Z))")

(deftest print.array.2.12
  (let ((desired-result "#2A((0 1 1) (1 1 0))"))
    (loop for i from 2 to 64
	  for a = (make-array '(2 3) :element-type `(unsigned-byte ,i)
			      :initial-contents '((0 1 1) (1 1 0)))
	  for result = (with-standard-io-syntax
			(write-to-string a :readably nil :array t))
	  unless (string= desired-result result)
	  collect (list i a result)))
  nil)

(deftest print.array.2.13
  (let ((desired-result "#2A((0 -1 -1) (-1 -1 0))"))
    (loop for i from 1 to 64
	  for a = (make-array '(2 3) :element-type `(signed-byte ,i)
			      :initial-contents '((0 -1 -1) (-1 -1 0)))
	  for result = (with-standard-io-syntax
			(write-to-string a :readably nil :array t))
	  unless (string= desired-result result)
	  collect (list i a result)))
  nil)

(deftest print.array.2.14
  (let ((desired-result "#2A((0 1 1) (1 1 0))"))
    (loop for i from 2 to 64
	  for a = (make-array '(2 3) :element-type `(unsigned-byte ,i)
			      :adjustable t
			      :initial-contents '((0 1 1) (1 1 0)))
	  for result = (with-standard-io-syntax
			(write-to-string a :readably nil :array t))
	  unless (string= desired-result result)
	  collect (list i a result)))
  nil)

(deftest print.array.2.15
  (let ((desired-result "#2A((0 -1 -1) (-1 -1 0))"))
    (loop for i from 1 to 64
	  for a = (make-array '(2 3) :element-type `(signed-byte ,i)
			      :adjustable t
			      :initial-contents '((0 -1 -1) (-1 -1 0)))
	  for result = (with-standard-io-syntax
			(write-to-string a :readably nil :array t))
	  unless (string= desired-result result)
	  collect (list i a result)))
  nil)

(deftest print.array.2.16
  (let ((desired-result "#2A((1 1) (1 0))"))
    (loop for i from 2 to 64
	  for type = `(unsigned-byte ,i)
	  for a = (make-array '(2 3) :element-type type
			      :adjustable t
			      :initial-contents '((0 1 1) (1 1 0)))
	  for b = (make-array '(2 2) :displaced-to a :displaced-index-offset 2
			      :element-type type)
	  for result = (with-standard-io-syntax
			(write-to-string b :readably nil :array t))
	  unless (string= desired-result result)
	  collect (list i b result)))
  nil)

(deftest print.array.2.17
  (let ((desired-result "#2A((1 -1) (-2 0))"))
    (loop for i from 2 to 64
	  for type = `(signed-byte ,i)
	  for a = (make-array '(2 3) :element-type type
			      :adjustable t
			      :initial-contents '((0 1 1) (-1 -2 0)))
	  for b = (make-array '(2 2) :displaced-to a :displaced-index-offset 2
			      :element-type type)
	  for result = (with-standard-io-syntax
			(write-to-string b :readably nil :array t))
	  unless (string= desired-result result)
	  collect (list i b result)))
  nil)

(deftest print.array.2.20
  (let* ((a (make-array '(9) :initial-contents '(1 3 8 2 67 121 65 432 6)))
	 (b (make-array '(2 2) :displaced-to a
			:displaced-index-offset 1)))
    (with-standard-io-syntax
     (write-to-string b :readably nil :array t)))
  "#2A((3 8) (2 67))")

(deftest print.array.2.21
  (trim-list
   (loop
      for dims = (list (random 4) (random 4))
      for a = (make-array dims :initial-element (- (random 1000000) 500000))
      repeat 100
      nconc (let ((result (randomly-check-readability a :test #'is-similar :can-fail t)))
	      (and result (list (cons dims (first result))))))
   10)
  nil)

(deftest print.array.2.22
  (loop for a = (make-array (list (random 4) (random 4))
			    :initial-element (- (random 1000000) 500000)
			    :adjustable t)
	repeat 100 nconc (randomly-check-readability a :test #'is-similar
						     :can-fail t))
  nil)

(deftest print.array.2.23
  (loop for d1 = (random 10)
	for d2 = (random 10)
	for a = (make-array (list d1 d2)
			    :initial-element (- (random 1000000) 500000))
	for d1a = (random (1+ d1))
	for d2a = (random (1+ d2))
	for offset = (random (1+ (- (* d1 d2) (* d1a d2a))))
	for b = (make-array (list d1a d2a) :displaced-to a
			    :displaced-index-offset offset)
	repeat 100 nconc (randomly-check-readability b :test #'is-similar
						     :can-fail t))
  nil)

(deftest print.array.2.24
  (loop for i from 1 to 64
	for type = `(unsigned-byte ,i)
	nconc
	(let ((a (make-array '(3 4) :initial-element 1 :element-type type)))
	  (loop repeat 5 nconc (randomly-check-readability a :test #'is-similar
							   :can-fail t))))
  nil)

(deftest print.array.2.25
  (let ((a (make-array '(3 4) :initial-element #\a :element-type 'character)))
    (loop repeat 10 nconc (randomly-check-readability a :test #'is-similar
						      :can-fail t)))
  nil)

(deftest print.array.2.26
  (let ((a (make-array '(3 4) :initial-element #\a :element-type 'base-char)))
    (loop repeat 10 nconc (randomly-check-readability a :test #'is-similar
						      :can-fail t)))
  nil)


(deftest print.array.2.27
  (let ((str (write-to-string (make-array '(2 3) :initial-element 0)
			      :readably nil :array nil)))
    (values (subseq str 0 2) (subseq str (1- (length str)))))
  "#<" ">")

(deftest print.array.2.28
  (loop for i from 1 to 64
	for type = `(unsigned-byte ,i)
	for a = (make-array '(4 3) :element-type type :initial-element 1)
	for result = (write-to-string a :readably nil :array nil)
	unless (and (string= (subseq result 0 2) "#<")
		    (string= (subseq result (1- (length result))) ">"))
	collect (list i result))
  nil)

(deftest print.array.2.29
  (loop for i from 1 to 64
	for type = `(signed-byte ,i)
	for a = (make-array '(4 8) :element-type type :initial-element -1)
	for result = (write-to-string a :readably nil :array nil)
	unless (and (string= (subseq result 0 2) "#<")
		    (string= (subseq result (1- (length result))) ">"))
	collect (list i result))
  nil)

(deftest print.array.2.30
  (loop for type in '(short-float single-float double-float long-float)
	for a = (make-array '(5 7) :element-type type
			    :initial-element (coerce 17 type))
	for result = (write-to-string a :readably nil :array nil)
	unless (and (string= (subseq result 0 2) "#<")
		    (string= (subseq result (1- (length result))) ">"))
	collect (list type result))
  nil)

(deftest print.array.2.31
  (loop for type0 in '(short-float single-float double-float
				   long-float float real)
	for type = `(complex ,type0)
	for a = (make-array '(13 5) :element-type type
			    :initial-element (complex 0 (coerce 3 type0)))
	for result = (write-to-string a :readably nil :array nil)
	unless (and (string= (subseq result 0 2) "#<")
		    (string= (subseq result (1- (length result))) ">"))
	collect (list type result))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Three D arrays

(deftest print.array.3.1
  (let* ((a (make-array '(1 2 3) :initial-contents '(((:|A| :|B| :|C|) (:|D| :|E| :|F|)))))
	 (b (make-array '(3 2 1) :displaced-to a
			:displaced-index-offset 0)))
    (with-standard-io-syntax
     (values
      (write-to-string a :readably nil :array t)
      (write-to-string b :readably nil :array t))))
  "#3A(((:A :B :C) (:D :E :F)))"
  "#3A(((:A) (:B)) ((:C) (:D)) ((:E) (:F)))")


;;; Multidimensional arrays

(deftest print.array.multi-dim.1
  (with-standard-io-syntax
   (loop for d in (remove array-rank-limit
			  '(4 5 6 7 8 9 10 12 16 20 30 40 100 200 400 600 800 1023)
			  :test #'<=)
	 for dims = (make-list d :initial-element 1)
	 for a = (make-array dims :initial-element 0)
	 for result = (with-standard-io-syntax
		       (write-to-string a :readably nil :array t))
	 for expected-result =
	 (concatenate 'string
		      (format nil "#~DA" d)
		      (make-string d :initial-element #\()
		      "0"
		      (make-string d :initial-element #\)))
	 unless (string= result expected-result)
	 collect (list d result expected-result)))
  nil)

(deftest print.array.multi-dim.2
  (with-standard-io-syntax
   (loop for d = (+ 4 (random (min (- array-rank-limit 4) 1000)))
	 for p = (random d)
	 for dims = (let ((list (make-list d :initial-element 1)))
		      (setf (elt list p) 0)
		      list)
	 for a = (make-array dims :initial-element 0)
	 for result = (with-standard-io-syntax
		       (write-to-string a :readably nil :array t))
	 for expected-result =
	 (concatenate 'string
		      (format nil "#~DA" d)
		      (make-string (1+ p) :initial-element #\()
		      (make-string (1+ p) :initial-element #\)))
	 repeat 50
	 unless (string= result expected-result)
	 collect (list d result expected-result)))
  nil)

;;; To add: more tests for high dimensional arrays, including arrays with
;;; element types
