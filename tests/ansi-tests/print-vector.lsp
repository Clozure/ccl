;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Apr 20 22:36:53 2004
;;;; Contains: Tests of vector printing

(compile-and-load "printer-aux.lsp")

(in-package :cl-test)

;;; Empty vector tests

(deftest print.vector.1
  (with-standard-io-syntax
   (write-to-string #() :readably nil :array t))
  "#()")

(deftest print.vector.2
  (with-standard-io-syntax
   (loop for i from 2 to 100
	 for a = (make-array '(0) :element-type `(unsigned-byte ,i))
	 for s = (write-to-string a :readably nil :array t :pretty nil)
	 unless (string= s "#()")
	 collect (list i s)))
  nil)

(deftest print.vector.3
  (with-standard-io-syntax
   (loop for i from 1 to 100
	 for a = (make-array '(0) :element-type `(signed-byte ,i))
	 for s = (write-to-string a :readably nil :array t :pretty nil)
	 unless (string= s "#()")
	 collect (list i s)))
  nil)

(deftest print.vector.4
  (with-standard-io-syntax
   (loop for type in '(short-float single-float double-float long-float)
	 for a = (make-array '(0) :element-type type)
	 for s = (write-to-string a :readably nil :array t :pretty nil)
	 unless (string= s "#()")
	 collect (list type s)))
  nil)

;;; Nonempty vectors

(deftest print.vector.5
  (with-standard-io-syntax
   (let* ((*package* (find-package "CL-TEST"))
	  (result
	   (write-to-string #(a b c)
			    :readably nil :array t
			    :pretty nil :case :downcase)))
     (or (and (string= result "#(a b c)") t)
	 result)))
  t)

(deftest print.vector.6
  (with-standard-io-syntax
   (loop
    for i from 2 to 100
    for a = (make-array '(4) :element-type `(unsigned-byte ,i)
			:initial-contents '(3 0 2 1))
    for s = (write-to-string a :readably nil :array t :pretty nil)
    unless (string= s "#(3 0 2 1)")
    collect (list i a s)))
  nil)

(deftest print.vector.7
  (with-standard-io-syntax
   (loop
    for i from 2 to 100
    for a = (make-array '(4) :element-type `(signed-byte ,i)
			:initial-contents '(-2 -1 0 1))
    for s = (write-to-string a :readably nil :array t :pretty nil)
    unless (string= s "#(-2 -1 0 1)")
    collect (list i a s)))
  nil)

;;; Vectors with fill pointers

(deftest print.vector.fill.1
  (with-standard-io-syntax
   (let ((v (make-array '(10) :initial-contents '(a b c d e f g h i j)
			:fill-pointer 0))
	 (*package* (find-package "CL-TEST")))
     (loop for i from 0 to 10
	   do (setf (fill-pointer v) i)
	   collect (write-to-string v :readably nil :array t :pretty nil
				    :case :downcase))))
  ("#()"
   "#(a)"
   "#(a b)"
   "#(a b c)"
   "#(a b c d)"
   "#(a b c d e)"
   "#(a b c d e f)"
   "#(a b c d e f g)"
   "#(a b c d e f g h)"
   "#(a b c d e f g h i)"
   "#(a b c d e f g h i j)"))

(deftest print.vector.fill.2
  (with-standard-io-syntax
   (let ((expected '("#()" "#(0)" "#(0 1)" "#(0 1 2)" "#(0 1 2 3)")))
     (loop for i from 2 to 100
	   nconc
	   (let ((v (make-array '(4) :initial-contents '(0 1 2 3)
				:element-type `(unsigned-byte ,i)
				:fill-pointer 0)))
	     (loop for fp from 0 to 4
		   for expected-result in expected
		   for actual-result =
		   (progn
		     (setf (fill-pointer v) fp)
		     (write-to-string v :readably nil :array t :pretty nil))
		   unless (string= expected-result actual-result)
		   collect (list i fp expected-result actual-result))))))
  nil)

(deftest print.vector.fill.3
  (with-standard-io-syntax
   (let ((expected '("#()" "#(0)" "#(0 -1)" "#(0 -1 -2)" "#(0 -1 -2 1)")))
     (loop for i from 2 to 100
	   nconc
	   (let ((v (make-array '(4) :initial-contents '(0 -1 -2 1)
				:element-type `(signed-byte ,i)
				:fill-pointer 0)))
	     (loop for fp from 0 to 4
		   for expected-result in expected
		   for actual-result =
		   (progn
		     (setf (fill-pointer v) fp)
		     (write-to-string v :readably nil :array t :pretty nil))
		   unless (string= expected-result actual-result)
		   collect (list i fp expected-result actual-result))))))
  nil)

;;; Displaced vectors

(deftest print.vector.displaced.1
  (let* ((v1 (vector 'a 'b 'c 'd 'e 'f 'g))
	 (v2 (make-array 3 :displaced-to v1 :displaced-index-offset 4)))
    (with-standard-io-syntax
     (write-to-string v2 :readably nil :array t :case :downcase :pretty nil
		      :escape nil)))
  "#(e f g)")

(deftest print.vector.displaced.2
  (with-standard-io-syntax
   (loop for i from 2 to 100
	 nconc
	 (let* ((type `(unsigned-byte ,i))
		(v1 (make-array 8 :element-type type
				:initial-contents '(0 1 2 3 0 1 2 3)))
		(v2 (make-array 5 :displaced-to v1
				:displaced-index-offset 2
				:element-type type))
		(result
		 (write-to-string v2 :readably nil :array t :pretty nil)))
	   (unless (string= result "#(2 3 0 1 2)")
	     (list (list i v1 v2 result))))))
  nil)


(deftest print.vector.displaced.3
  (with-standard-io-syntax
   (loop for i from 2 to 100
	 nconc
	 (let* ((type `(signed-byte ,i))
		(v1 (make-array 8 :element-type type
				:initial-contents '(0 1 -1 -2 0 1 -1 -2)))
		(v2 (make-array 5 :displaced-to v1
				:displaced-index-offset 2
				:element-type type))
		(result
		 (write-to-string v2 :readably nil :array t :pretty nil)))
	   (unless (string= result "#(-1 -2 0 1 -1)")
	     (list (list i v1 v2 result))))))
  nil)


;;; Adjustable vectors

(deftest print.vector.adjustable.1
  (with-standard-io-syntax
   (let ((v (make-array '(10) :initial-contents '(a b c d e f g h i j)
			:adjustable t)))
     (write-to-string v :readably nil :array t :case :downcase :pretty nil
		      :escape nil)))
  "#(a b c d e f g h i j)")

(deftest print.vector.adjustable.2
  (with-standard-io-syntax
   (loop for i from 2 to 100
	 for type = `(unsigned-byte ,i)
	 for v = (make-array '(8) :initial-contents '(0 1 2 3 3 0 2 1)
			     :adjustable t)
	 for s =
	 (write-to-string v :readably nil :array t :case :downcase :pretty nil
			  :escape nil)
	 unless (string= s "#(0 1 2 3 3 0 2 1)")
	 collect (list i v s)))
  nil)

(deftest print.vector.adjustable.3
  (with-standard-io-syntax
   (loop for i from 2 to 100
	 for type = `(signed-byte ,i)
	 for v = (make-array '(8) :initial-contents '(0 1 -1 -2 -1 0 -2 1)
			     :adjustable t)
	 for s =
	 (write-to-string v :readably nil :array t :case :downcase :pretty nil
			  :escape nil)
	 unless (string= s "#(0 1 -1 -2 -1 0 -2 1)")
	 collect (list i v s)))
  nil)

;;; Printing with *print-array* and *print-readably* bound to nil

(deftest print.vector.unreadable.1
  (with-standard-io-syntax
   (subseq (write-to-string #(a b c d e) :array nil :readably nil) 0 2))
  "#<")

(deftest print.vector.unreadable.2
  (with-standard-io-syntax
   (loop for i from 2 to 100
	 for type = `(unsigned-byte ,i)
	 for v = (make-array '(4) :element-type type
			     :initial-contents '(0 1 2 3))
	 for result = (write-to-string v :array nil :readably nil)
	 unless (string= (subseq result 0 2) "#<")
	 collect (list i type v result)))
  nil)


(deftest print.vector.unreadable.3
  (with-standard-io-syntax
   (loop for i from 2 to 100
	 for type = `(signed-byte ,i)
	 for v = (make-array '(4) :element-type type
			     :initial-contents '(0 1 -2 -1))
	 for result = (write-to-string v :array nil :readably nil)
	 unless (string= (subseq result 0 2) "#<")
	 collect (list i type v result)))
  nil)

;;; Readability tests

(deftest print.vector.random.1
  (trim-list
   (loop for v in *universe*
	 when (vectorp v)
	 nconc
	 (loop repeat 10
	       nconc (randomly-check-readability
		      v :test #'equalp
		      :can-fail (not (subtypep t (array-element-type v))))))
   10)
  nil)

(deftest print.vector.random.2
  (trim-list
   (loop for i from 2 to 100
	 for type = `(unsigned-byte ,i)
	 for v = (make-array '(4) :element-type type
			     :initial-contents '(1 3 2 0))
	 nconc
	 (loop repeat 10
	       nconc (randomly-check-readability v :test #'equalp
						 :can-fail t)))
   10)
  nil)

(deftest print.vector.random.3
  (trim-list
   (loop for i from 2 to 100
	 for type = `(signed-byte ,i)
	 for v = (make-array '(4) :element-type type
			     :initial-contents '(-1 1 0 -2))
	 nconc
	 (loop repeat 10
	       nconc (randomly-check-readability v :test #'equalp
						 :can-fail t)))
   10)
  nil)

(deftest print.vector.random.4
  (trim-list
   (loop for v = (make-random-vector (1+ (random 100)))
	 repeat 1000
	 nconc (randomly-check-readability v :test #'equalp))
   10)
  nil)

;;; *print-length* checks

(deftest print.vector.length.1
  (with-standard-io-syntax
   (write-to-string #() :pretty nil :length 0 :readably nil))
  "#()")

(deftest print.vector.length.2
  (with-standard-io-syntax
   (write-to-string #(1) :pretty nil :length 0 :readably nil))
  "#(...)")

(deftest print.vector.length.3
  (with-standard-io-syntax
   (write-to-string #(1) :pretty nil :length 1 :readably nil))
  "#(1)")

(deftest print.vector.length.4
  (with-standard-io-syntax
   (write-to-string #(a b c d e f g h)
		    :pretty nil
		    :array t :escape nil
		    :length 5 :case :downcase
		    :readably nil))
  "#(a b c d e ...)")

(deftest print.vector.length.5
  (with-standard-io-syntax
   (loop for i from 2 to 100
	 for type = `(unsigned-byte ,i)
	 for v = (make-array '(0) :element-type type)
	 for result = (write-to-string v :array t :readably nil
				       :pretty nil
				       :length 0)
	 unless (string= result "#()")
	 collect (list i type v result)))
  nil)

(deftest print.vector.length.6
  (with-standard-io-syntax
   (loop for i from 2 to 100
	 for type = `(unsigned-byte ,i)
	 for v = (make-array '(1) :element-type type :initial-contents '(2))
	 for result = (write-to-string v
				       :pretty nil
				       :array t
				       :readably nil
				       :length 0)
	 unless (string= result "#(...)")
	 collect (list i type v result)))
  nil)

(deftest print.vector.length.7
  (with-standard-io-syntax
   (loop for i from 1 to 100
	 for type = `(signed-byte ,i)
	 for v = (make-array '(1) :element-type type :initial-contents '(-1))
	 for result = (write-to-string v
				       :pretty nil
				       :array t
				       :readably nil
				       :length 0)
	 unless (string= result "#(...)")
	 collect (list i type v result)))
  nil)

(deftest print.vector.length.8
  (with-standard-io-syntax
   (loop for i from 2 to 100
	 for type = `(unsigned-byte ,i)
	 for v = (make-array '(4) :element-type type
			     :initial-contents '(1 3 0 2))
	 for result = (write-to-string v
				       :pretty nil
				       :array t
				       :readably nil
				       :length 2)
	 unless (string= result "#(1 3 ...)")
	 collect (list i type v result)))
  nil)

(deftest print.vector.length.9
  (with-standard-io-syntax
   (loop for i from 2 to 100
	 for type = `(signed-byte ,i)
	 for v = (make-array '(4) :element-type type
			     :initial-contents '(1 -2 0 -1))
	 for result = (write-to-string v
				       :pretty nil
				       :array t
				       :readably nil
				       :length 2)
	 unless (string= result "#(1 -2 ...)")
	 collect (list i type v result)))
  nil)

;;; Printing with *print-level* bound

(deftest print.vector.level.1
  (with-standard-io-syntax
   (write-to-string #() :level 0 :readably nil :pretty nil))
  "#")

(deftest print.vector.level.2
  (with-standard-io-syntax
   (write-to-string #() :level 1 :readably nil :pretty nil))
  "#()")

(deftest print.vector.level.3
  (with-standard-io-syntax
   (write-to-string #(17) :level 1 :readably nil :pretty nil))
  "#(17)")

(deftest print.vector.level.4
  (with-standard-io-syntax
   (write-to-string #(4 (17) 9 (a) (b) 0) :level 1 :readably nil :pretty nil))
  "#(4 # 9 # # 0)")

(deftest print.vector.level.5
  (with-standard-io-syntax
   (write-to-string '(#(a)) :level 1 :readably nil :pretty nil))
  "(#)")

(deftest print.vector.level.6
  (with-standard-io-syntax
   (write-to-string '#(#(a)) :level 1 :readably nil :pretty nil))
  "#(#)")

