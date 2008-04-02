;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar  6 11:47:55 2004
;;;; Contains: Tests of symbol printing

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

;;; Symbol printing when escaping is off

(defun princ.symbol.fn (sym case *print-case* expected)
  (setf (readtable-case *readtable*) case)
  (let ((str (with-output-to-string (s) (princ sym s))))
    (or (equalt str expected)
	(list str expected))))

(defun prin1.symbol.fn (sym case *print-case* expected)
  (setf (readtable-case *readtable*) case)
  (let ((str (with-output-to-string (s) (prin1 sym s))))
    (or (and (member str expected :test #'string=) t)
	(list str expected))))

(deftest print.symbol.1
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*readtable* (copy-readtable nil)))
     (flet ((%p (&rest args) (apply #'princ.symbol.fn args)))
       (values
	(%p '|XYZ| :upcase :upcase "XYZ")
	(%p '|XYZ| :upcase :downcase "xyz")
	(%p '|XYZ| :upcase :capitalize "Xyz")
	(%p '|XYZ| :downcase :upcase "XYZ")
	(%p '|XYZ| :downcase :downcase "XYZ")
	(%p '|XYZ| :downcase :capitalize "XYZ")
	(%p '|XYZ| :preserve :upcase "XYZ")
	(%p '|XYZ| :preserve :downcase "XYZ")
	(%p '|XYZ| :preserve :capitalize "XYZ")
	(%p '|XYZ| :invert :upcase "xyz")
	(%p '|XYZ| :invert :downcase "xyz")
	(%p '|XYZ| :invert :capitalize "xyz")))))
  t t t t t t t t t t t t)

(deftest print.symbol.2
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*readtable* (copy-readtable nil)))
     (flet ((%p (&rest args) (apply #'princ.symbol.fn args)))
       (values
	(%p '|xyz| :upcase :upcase "xyz")
	(%p '|xyz| :upcase :downcase "xyz")
	(%p '|xyz| :upcase :capitalize "xyz")
	(%p '|xyz| :downcase :upcase "XYZ")
	(%p '|xyz| :downcase :downcase "xyz")
	(%p '|xyz| :downcase :capitalize "Xyz")
	(%p '|xyz| :preserve :upcase "xyz")
	(%p '|xyz| :preserve :downcase "xyz")
	(%p '|xyz| :preserve :capitalize "xyz")
	(%p '|xyz| :invert :upcase "XYZ")
	(%p '|xyz| :invert :downcase "XYZ")
	(%p '|xyz| :invert :capitalize "XYZ")))))
  t t t t t t t t t t t t)

(deftest print.symbol.3
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*readtable* (copy-readtable nil)))
     (flet ((%p (&rest args) (apply #'princ.symbol.fn args)))
       (values
	(%p '|Xyz| :upcase :upcase "Xyz")
	(%p '|Xyz| :upcase :downcase "xyz")
	(%p '|Xyz| :upcase :capitalize "Xyz")
	(%p '|Xyz| :downcase :upcase "XYZ")
	(%p '|Xyz| :downcase :downcase "Xyz")
	(%p '|Xyz| :downcase :capitalize "Xyz")
	(%p '|Xyz| :preserve :upcase "Xyz")
	(%p '|Xyz| :preserve :downcase "Xyz")
	(%p '|Xyz| :preserve :capitalize "Xyz")
	(%p '|Xyz| :invert :upcase "Xyz")
	(%p '|Xyz| :invert :downcase "Xyz")
	(%p '|Xyz| :invert :capitalize "Xyz")))))
  t t t t t t t t t t t t)

(deftest print.symbol.4
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*readtable* (copy-readtable nil)))
     (flet ((%p (&rest args) (apply #'princ.symbol.fn args)))
       (values
	(%p '|xYZ| :upcase :upcase "xYZ")
	(%p '|xYZ| :upcase :downcase "xyz")
	(%p '|xYZ| :upcase :capitalize "xyz")
	(%p '|xYZ| :downcase :upcase "XYZ")
	(%p '|xYZ| :downcase :downcase "xYZ")
	(%p '|xYZ| :downcase :capitalize "XYZ")
	(%p '|xYZ| :preserve :upcase "xYZ")
	(%p '|xYZ| :preserve :downcase "xYZ")
	(%p '|xYZ| :preserve :capitalize "xYZ")
	(%p '|xYZ| :invert :upcase "xYZ")
	(%p '|xYZ| :invert :downcase "xYZ")
	(%p '|xYZ| :invert :capitalize "xYZ")))))
  t t t t t t t t t t t t)

(deftest print.symbol.5
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*readtable* (copy-readtable nil)))
     (flet ((%p (&rest args) (apply #'princ.symbol.fn args)))
       (values
	(%p '|X1Z| :upcase :upcase "X1Z")
	(%p '|X1Z| :upcase :downcase "x1z")
	(%p '|X1Z| :upcase :capitalize "X1z")
	(%p '|X1Z| :downcase :upcase "X1Z")
	(%p '|X1Z| :downcase :downcase "X1Z")
	(%p '|X1Z| :downcase :capitalize "X1Z")
	(%p '|X1Z| :preserve :upcase "X1Z")
	(%p '|X1Z| :preserve :downcase "X1Z")
	(%p '|X1Z| :preserve :capitalize "X1Z")
	(%p '|X1Z| :invert :upcase "x1z")
	(%p '|X1Z| :invert :downcase "x1z")
	(%p '|X1Z| :invert :capitalize "x1z")))))
  t t t t t t t t t t t t)

(deftest print.symbol.6
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*readtable* (copy-readtable nil)))
     (flet ((%p (&rest args) (apply #'princ.symbol.fn args)))
       (values
	(%p '|x1z| :upcase :upcase "x1z")
	(%p '|x1z| :upcase :downcase "x1z")
	(%p '|x1z| :upcase :capitalize "x1z")
	(%p '|x1z| :downcase :upcase "X1Z")
	(%p '|x1z| :downcase :downcase "x1z")
	(%p '|x1z| :downcase :capitalize "X1z")
	(%p '|x1z| :preserve :upcase "x1z")
	(%p '|x1z| :preserve :downcase "x1z")
	(%p '|x1z| :preserve :capitalize "x1z")
	(%p '|x1z| :invert :upcase "X1Z")
	(%p '|x1z| :invert :downcase "X1Z")
	(%p '|x1z| :invert :capitalize "X1Z")))))
  t t t t t t t t t t t t)

(deftest print.symbol.7
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*readtable* (copy-readtable nil)))
     (flet ((%p (&rest args) (apply #'princ.symbol.fn args)))
       (values
	(%p '|X1z| :upcase :upcase "X1z")
	(%p '|X1z| :upcase :downcase "x1z")
	(%p '|X1z| :upcase :capitalize "X1z")
	(%p '|X1z| :downcase :upcase "X1Z")
	(%p '|X1z| :downcase :downcase "X1z")
	(%p '|X1z| :downcase :capitalize "X1z")
	(%p '|X1z| :preserve :upcase "X1z")
	(%p '|X1z| :preserve :downcase "X1z")
	(%p '|X1z| :preserve :capitalize "X1z")
	(%p '|X1z| :invert :upcase "X1z")
	(%p '|X1z| :invert :downcase "X1z")
	(%p '|X1z| :invert :capitalize "X1z")))))
  t t t t t t t t t t t t)

(deftest print.symbol.8
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*readtable* (copy-readtable nil)))
     (flet ((%p (&rest args) (apply #'princ.symbol.fn args)))
       (values
	(%p '|x1Z| :upcase :upcase "x1Z")
	(%p '|x1Z| :upcase :downcase "x1z")
	(%p '|x1Z| :upcase :capitalize "x1z")
	(%p '|x1Z| :downcase :upcase "X1Z")
	(%p '|x1Z| :downcase :downcase "x1Z")
	(%p '|x1Z| :downcase :capitalize "X1Z")
	(%p '|x1Z| :preserve :upcase "x1Z")
	(%p '|x1Z| :preserve :downcase "x1Z")
	(%p '|x1Z| :preserve :capitalize "x1Z")
	(%p '|x1Z| :invert :upcase "x1Z")
	(%p '|x1Z| :invert :downcase "x1Z")
	(%p '|x1Z| :invert :capitalize "x1Z")))))
  t t t t t t t t t t t t)

(deftest print.symbol.9
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*readtable* (copy-readtable nil)))
     (flet ((%p (&rest args) (apply #'princ.symbol.fn args)))
       (values
	(%p '|X Z| :upcase :upcase "X Z")
	(%p '|X Z| :upcase :downcase "x z")
	(%p '|X Z| :upcase :capitalize "X Z")
	(%p '|X Z| :downcase :upcase "X Z")
	(%p '|X Z| :downcase :downcase "X Z")
	(%p '|X Z| :downcase :capitalize "X Z")
	(%p '|X Z| :preserve :upcase "X Z")
	(%p '|X Z| :preserve :downcase "X Z")
	(%p '|X Z| :preserve :capitalize "X Z")
	(%p '|X Z| :invert :upcase "x z")
	(%p '|X Z| :invert :downcase "x z")
	(%p '|X Z| :invert :capitalize "x z")))))
  t t t t t t t t t t t t)

(deftest print.symbol.10
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*readtable* (copy-readtable nil)))
     (flet ((%p (&rest args) (apply #'princ.symbol.fn args)))
       (values
	(%p '|x z| :upcase :upcase "x z")
	(%p '|x z| :upcase :downcase "x z")
	(%p '|x z| :upcase :capitalize "x z")
	(%p '|x z| :downcase :upcase "X Z")
	(%p '|x z| :downcase :downcase "x z")
	(%p '|x z| :downcase :capitalize "X Z")
	(%p '|x z| :preserve :upcase "x z")
	(%p '|x z| :preserve :downcase "x z")
	(%p '|x z| :preserve :capitalize "x z")
	(%p '|x z| :invert :upcase "X Z")
	(%p '|x z| :invert :downcase "X Z")
	(%p '|x z| :invert :capitalize "X Z")))))
  t t t t t t t t t t t t)

(deftest print.symbol.11
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*readtable* (copy-readtable nil)))
     (flet ((%p (&rest args) (apply #'princ.symbol.fn args)))
       (values
	(%p '|X z| :upcase :upcase "X z")
	(%p '|X z| :upcase :downcase "x z")
	(%p '|X z| :upcase :capitalize "X z")
	(%p '|X z| :downcase :upcase "X Z")
	(%p '|X z| :downcase :downcase "X z")
	(%p '|X z| :downcase :capitalize "X Z")
	(%p '|X z| :preserve :upcase "X z")
	(%p '|X z| :preserve :downcase "X z")
	(%p '|X z| :preserve :capitalize "X z")
	(%p '|X z| :invert :upcase "X z")
	(%p '|X z| :invert :downcase "X z")
	(%p '|X z| :invert :capitalize "X z")))))
  t t t t t t t t t t t t)

(deftest print.symbol.12
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*readtable* (copy-readtable nil)))
     (flet ((%p (&rest args) (apply #'princ.symbol.fn args)))
       (values
	(%p '|x Z| :upcase :upcase "x Z")
	(%p '|x Z| :upcase :downcase "x z")
	(%p '|x Z| :upcase :capitalize "x Z")
	(%p '|x Z| :downcase :upcase "X Z")
	(%p '|x Z| :downcase :downcase "x Z")
	(%p '|x Z| :downcase :capitalize "X Z")
	(%p '|x Z| :preserve :upcase "x Z")
	(%p '|x Z| :preserve :downcase "x Z")
	(%p '|x Z| :preserve :capitalize "x Z")
	(%p '|x Z| :invert :upcase "x Z")
	(%p '|x Z| :invert :downcase "x Z")
	(%p '|x Z| :invert :capitalize "x Z")))))
  t t t t t t t t t t t t)

;;; Randomized printing tests

(deftest print.symbol.random.1
  (let ((pkg-name "PRINT-SYMBOL-TEST-PACKAGE"))
    (when (find-package pkg-name)
      (delete-package pkg-name))
    (prog1
	(let ((*package* (make-package pkg-name)))
	  (trim-list
	   (loop for c across +standard-chars+
		 nconc
		 (loop repeat 50
		       nconc (randomly-check-readability (intern (string c)))))
	   10))
;;      (delete-package pkg-name)
      ))
  nil)

(deftest print.symbol.random.2
  (let ((pkg-name "PRINT-SYMBOL-TEST-PACKAGE"))
    (when (find-package pkg-name)
      (delete-package pkg-name))
    (prog1
	(let ((*package* (make-package pkg-name))
	      (count 0))
	  (trim-list
	   (loop for c1 = (random-from-seq +standard-chars+)
		 for c2 = (random-from-seq +standard-chars+)
		 for string = (concatenate 'string (string c1) (string c2))
		 for result = (randomly-check-readability (intern string))
		 for tries from 1 to 10000
		 when result do (incf count)
		 nconc result
		 when (= count 10)
		 collect	(format nil "... ~A out of ~A, stopping test ..."
					count tries)
		 while (< count 10))
	   10))
      ;; (delete-package pkg-name)
      ))
  nil)

(deftest print.symbol.random.3
  (let ((count 0)
	(symbols (make-array '(1000) :fill-pointer 0 :adjustable t)))
    ;; Find all symbols that have a home package, put into array SYMBOLS
    (do-all-symbols (s)
      (when (symbol-package s)
	(vector-push-extend s symbols (array-dimension symbols 0))))
    (loop for i = (random (fill-pointer symbols))
	  for s = (aref symbols i)
	  for tries from 1 to 10000
	  for problem = (randomly-check-readability s)
	  nconc problem
	  when problem do (incf count)
	  while (< count 10)))
  nil)

(deftest print.symbol.random.4
  (let ((count 0)
	(symbols (make-array '(1000) :fill-pointer 0 :adjustable t)))
    ;; Find all symbols that have a home package, put into array SYMBOLS
    (do-all-symbols (s)
      (when (symbol-package s)
	(vector-push-extend s symbols (array-dimension symbols 0))))
    (loop for i = (random (fill-pointer symbols))
	  for s = (aref symbols i)
	  for tries from 1 to 10000
	  for problem = (let ((*package* (symbol-package s)))
			  (randomly-check-readability s))
	  nconc problem
	  when problem do (incf count)
	  while (< count 10)))
  nil)

;;;; Tests of printing with escaping enabled

(deftest prin1.symbol.1
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test))
	 (*readtable* (copy-readtable nil)))
     (flet ((%p (&rest args) (apply #'prin1.symbol.fn args)))
       (values
	(%p '|X| :upcase :upcase     '("x" "X" "\\X" "|X|"))
	(%p '|X| :upcase :downcase   '("x" "X" "\\X" "|X|"))
	(%p '|X| :upcase :capitalize '("x" "X" "\\X" "|X|"))
	(%p '|X| :downcase :upcase     '("\\X" "|X|"))
	(%p '|X| :downcase :downcase   '("\\X" "|X|"))
	(%p '|X| :downcase :capitalize '("\\X" "|X|"))
	(%p '|X| :preserve :upcase     '("X" "\\X" "|X|"))
	(%p '|X| :preserve :downcase   '("X" "\\X" "|X|"))
	(%p '|X| :preserve :capitalize '("X" "\\X" "|X|"))
	(%p '|X| :invert :upcase       '("x" "\\X" "|X|"))
	(%p '|X| :invert :downcase     '("x" "\\X" "|X|"))
	(%p '|X| :invert :capitalize   '("x" "\\X" "|X|"))
	))))
  t t t t t t t t t t t t)

(deftest prin1.symbol.2
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test))
	 (*readtable* (copy-readtable nil)))
     (flet ((%p (&rest args) (apply #'prin1.symbol.fn args)))
       (values
	(%p '|x| :upcase :upcase     '("\\x" "|x|"))
	(%p '|x| :upcase :downcase   '("\\x" "|x|"))
	(%p '|x| :upcase :capitalize '("\\x" "|x|"))
	(%p '|x| :downcase :upcase     '("x" "X" "\\x" "|x|"))
	(%p '|x| :downcase :downcase   '("x" "X" "\\x" "|x|"))
	(%p '|x| :downcase :capitalize '("x" "X" "\\x" "|x|"))
	(%p '|x| :preserve :upcase     '("x" "\\x" "|x|"))
	(%p '|x| :preserve :downcase   '("x" "\\x" "|x|"))
	(%p '|x| :preserve :capitalize '("x" "\\x" "|x|"))
	(%p '|x| :invert :upcase       '("X" "\\x" "|x|"))
	(%p '|x| :invert :downcase     '("X" "\\x" "|x|"))
	(%p '|x| :invert :capitalize   '("X" "\\x" "|x|"))
	))))
  t t t t t t t t t t t t)

(deftest prin1.symbol.3
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test))
	 (*readtable* (copy-readtable nil)))
     (flet ((%p (&rest args) (apply #'prin1.symbol.fn args)))
       (values
	(%p '|1| :upcase :upcase     '("\\1" "|1|"))
	(%p '|1| :upcase :downcase   '("\\1" "|1|"))
	(%p '|1| :upcase :capitalize '("\\1" "|1|"))
	(%p '|1| :downcase :upcase     '("1" "\\1" "|1|"))
	(%p '|1| :downcase :downcase   '("1" "\\1" "|1|"))
	(%p '|1| :downcase :capitalize '("1" "\\1" "|1|"))
	(%p '|1| :preserve :upcase     '("1" "\\1" "|1|"))
	(%p '|1| :preserve :downcase   '("1" "\\1" "|1|"))
	(%p '|1| :preserve :capitalize '("1" "\\1" "|1|"))
	(%p '|1| :invert :upcase       '("1" "\\1" "|1|"))
	(%p '|1| :invert :downcase     '("1" "\\1" "|1|"))
	(%p '|1| :invert :capitalize   '("1" "\\1" "|1|"))
	))))
  t t t t t t t t t t t t)

;;; Random symbol printing tests when *print-escape* is true
;;; and *print-readably* is false.

;;; I AM NOT SURE THESE ARE CORRECT, SO THEY ARE COMMENTED OUT FOR NOW -- PFD

#|
(deftest print.symbol.escaped-random.1
  (let ((pkg-name "PRINT-SYMBOL-TEST-PACKAGE"))
    (when (find-package pkg-name)
      (delete-package pkg-name))
    (prog1
	(let ((*package* (make-package pkg-name))
	      (result
	       (loop for c across +standard-chars+
		     for s = (intern (string c))
		     append
		     (loop repeat 50
			   nconc (randomly-check-readability
				  s
				  :readable nil
				  :escape t)))))
	  (subseq result 0 (min (length result) 10)))
      ;; (delete-package pkg-name)
      ))
  nil)

(deftest print.symbol.escaped-random.2
  (let ((result
	 (loop for c across +standard-chars+
	       for s = (make-symbol (string c))
	       nconc
	       (loop repeat 50
		     nconc (randomly-check-readability
			    s
			    :readable nil
			    :escape t
			    :gensym t
			    :test #'similar-uninterned-symbols)))))
    (subseq result 0 (min (length result) 10)))
  nil)

(deftest print.symbol.escaped-random.3
  (let ((pkg-name "PRINT-SYMBOL-TEST-PACKAGE"))
    (when (find-package pkg-name)
      (delete-package pkg-name))
    (prog1
	(let ((*package* (make-package pkg-name))
	      (result
	       (loop for i below 256
		     for c = (code-char i)
		     when c
		     nconc
		     (let ((s (intern (string c))))
		       (loop repeat 50
			     nconc (randomly-check-readability
				    s
				    :readable nil
				    :escape t))))))
	  (subseq result 0 (min (length result) 10)))
      ;; (delete-package pkg-name)
      ))
  nil)
	
(deftest print.symbol.escaped-random.4
  (let ((result
	 (loop for i below 256
	       for c = (code-char i)
	       when c
	       nconc
	       (let ((s (make-symbol (string c))))
		 (loop repeat 50
		       nconc (randomly-check-readability
			      s
			      :readable nil
			      :escape t
			      :gensym t
			      :test #'similar-uninterned-symbols))))))
    (subseq result 0 (min (length result) 10)))
  nil)

(deftest print.symbol.escaped-random.5
  (loop for s in *universe*
	when (and (symbolp s) (symbol-package s) )
	nconc
	(loop repeat 50
	      nconc (randomly-check-readability
		     s
		     :readable nil
		     :escape t)))
  nil)

(deftest print.symbol.escaped-random.6
  (let ((*package* (find-package "KEYWORD")))
    (loop for s in *universe*
	  when (and (symbolp s) (symbol-package s))
	  nconc
	  (loop repeat 50
		nconc (randomly-check-readability
		       s
		       :readable nil
		       :escape t))))
  nil)

(deftest print.symbol.escaped-random.7
  (loop for s in *universe*
	when (and (symbolp s) (not (symbol-package s)))
	nconc
	(loop repeat 50
	      nconc (randomly-check-readability
		     s
		     :readable nil
		     :escape t
		     :gensym t
		     :test #'similar-uninterned-symbols)))
  nil)								    
			  
(deftest print.symbol.escaped-random.8
  (let ((*package* (find-package "KEYWORD")))
    (loop for s in *universe*
	  when (and (symbolp s) (not (symbol-package s)))
	  nconc
	  (loop repeat 50
		nconc (randomly-check-readability
		       s
		       :readable nil
		       :escape t
		       :gensym t
		       :test #'similar-uninterned-symbols))))
  nil)

(deftest print.symbol.escaped.9
  (let* ((*package* (find-package "CL-TEST"))
	 (s (intern "()")))
    (randomly-check-readability s :readable nil :escape t))
  nil)

(deftest print.symbol.escaped.10
  (let* ((*package* (find-package "KEYWORD"))
	 (s (intern "()")))
    (randomly-check-readability s :readable nil :escape t))
  nil)

|#

;;; Tests of printing package prefixes

(deftest print.symbol.prefix.1
  (with-standard-io-syntax
   (let ((s (write-to-string (make-symbol "ABC") :gensym t :case :upcase :escape t :readably nil)))
     (if (string= s "#:ABC") t s)))
  t)

(deftest print.symbol.prefix.2
  (with-standard-io-syntax
   (let ((s (write-to-string (make-symbol "ABC") :gensym nil :case :upcase :readably nil :escape nil)))
     (if (string= s "ABC") t s)))
  t)

(deftest print.symbol.prefix.3
  (with-standard-io-syntax
   (let ((s (write-to-string (make-symbol "ABC")
			     :gensym nil :case :upcase
			     :readably t :escape nil)))
     (if (and (string= (subseq s 0 2) "#:")
	      (string= (symbol-name (read-from-string s)) "ABC"))
	 t s)))
  t)

(deftest print.symbol.prefix.4
  (with-standard-io-syntax
   (let ((s (write-to-string (make-symbol "ABC") :gensym nil :case :upcase :readably nil :escape t)))
     (if (string= s "ABC") t s)))
  t)

(deftest print.symbol.prefix.5
  (with-standard-io-syntax
   (let ((pkg-name "PRINT-SYMBOL-TEST-PACKAGE"))
     (when (find-package pkg-name)
       (delete-package pkg-name))
     (let ((pkg (make-package pkg-name)))
       (multiple-value-prog1
	(let* ((*package* (find-package "CL-TEST"))
	       (s (intern "ABC" pkg)))
	  (values
	   (write-to-string s :case :upcase :readably nil :escape t)
	   (let ((*package* pkg))
	     (write-to-string s :case :upcase :readably nil :escape t))
	   (let ((*package* pkg))
	     (write-to-string s :case :downcase :readably nil :escape t))
	   ))
	;; (delete-package pkg)
	))))
  "PRINT-SYMBOL-TEST-PACKAGE::ABC"
  "ABC"
  "abc")


(deftest print.symbol.prefix.6
  (let ((pkg-name "PRINT-SYMBOL-TEST-PACKAGE"))
    (when (find-package pkg-name)
      (delete-package pkg-name))
    (let ((pkg (make-package pkg-name)))
      (prog1
	  (with-standard-io-syntax
	   (let* ((*package* pkg)
		  (s (intern "X" pkg)))
	     (write-to-string s :case :upcase :readably nil))
	   ;; (delete-package pkg)
	   ))))
  "X")

(deftest print.symbol.prefix.6a
  (with-standard-io-syntax
   (let ((*package* (find-package "CL-TEST")))
     (write-to-string 'x :case :upcase :readably nil)))
  "X")

(deftest print.symbol.prefix.6b
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (optimize speed (safety 0)))
       (with-standard-io-syntax
	(let ((*package* (find-package "CL-TEST")))
	  (write-to-string 'cl-test::x :case :upcase :readably nil))))))
  "X")

(deftest print.symbol.prefix.7
  (with-standard-io-syntax
   (let ((pkg-name "PRINT-SYMBOL-TEST-PACKAGE")
	 (pkg-name2 "ANOTHER-PRINT-SYMBOL-TEST-PACKAGE"))
     (when (find-package pkg-name)
       (delete-package pkg-name))
     (when (find-package pkg-name2)
       (delete-package pkg-name2))
     (prog1
	 (let* ((pkg (make-package pkg-name))
		(pkg2 (make-package pkg-name2))
		(s (intern "ABC" pkg)))
	   (import s pkg2)
	   (let ((*package* pkg2))
	     (write-to-string s :case :upcase :readably nil :escape t)))
       ;; (delete-package pkg)
       )))
  "ABC")

(deftest print.symbol.prefix.8
  (with-standard-io-syntax
   (let ((pkg-name "PRINT-SYMBOL-TEST-PACKAGE")
	 (pkg-name2 "ANOTHER-PRINT-SYMBOL-TEST-PACKAGE"))
     (when (find-package pkg-name)
       (delete-package pkg-name))
     (when (find-package pkg-name2)
       (delete-package pkg-name2))
     (prog1
	 (let* ((pkg (make-package pkg-name))
		(pkg2 (make-package pkg-name2))
		(s (intern "ABC" pkg2)))
	   (import s pkg)
	   (delete-package pkg2)
	   (let ((*package* pkg))
	     (write-to-string s :case :upcase :gensym t :readably nil :escape t)))
       ;; (delete-package pkg)
       )))
  "#:ABC")

(deftest print.symbol.prefix.9
  (with-standard-io-syntax
   (let ((pkg-name "PRINT-SYMBOL-TEST-PACKAGE"))
     (when (find-package pkg-name)
       (delete-package pkg-name))
     (prog1
	 (let* ((pkg (make-package pkg-name))
		(s (intern "ABC" pkg)))
	   (export s pkg)
	   (let ((*package* (find-package "CL-TEST")))
	     (write-to-string s :case :upcase :readably nil :escape t)))
       ;; (delete-package pkg)
       )))
  "PRINT-SYMBOL-TEST-PACKAGE:ABC")


(deftest print.symbol.prefix.10
  (with-standard-io-syntax
   (let ((pkg-name "PRINT-SYMBOL-TEST-PACKAGE"))
     (when (find-package pkg-name)
       (delete-package pkg-name))
     (prog1
	 (let* ((pkg (make-package pkg-name))
		(s :|X|))
	   (import s pkg)
	   (let ((*package* pkg))
	     (write-to-string s :case :upcase :readably nil :escape t)))
       ;; (delete-package pkg)
       )))
  ":X")

