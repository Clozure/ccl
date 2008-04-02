;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 22 07:10:22 2004
;;;; Contains: Tests for FIND-ALL-SYMBOLS

(in-package :cl-test)

(deftest find-all-symbols.1
  (let ((all-packages (list-all-packages)))
    (loop
     for package in all-packages
     append
     (let ((failures nil))
       (do-symbols (sym package failures)
	 (when (eql (symbol-package sym) package)
	   (let* ((name (symbol-name sym))
		  (similar (find-all-symbols name))
		  (similar2 (find-all-symbols sym)))
	     (unless (and (member sym similar)
			  (subsetp similar similar2)
			  (subsetp similar2 similar)
			  (loop for sym2 in similar
				always (string= name (symbol-name sym2))))
	       (push sym failures))))))))
  nil)

;;; FIXME -- test that each symbol found is accessible in some package

(deftest find-all-symbols.2
  (loop for i from 0 to 255
	for c = (code-char i)
	when (and (characterp c)
		  (loop for sym in (find-all-symbols c)
			thereis (not (string= (symbol-name sym)
					      (string c)))))
	collect c)
  nil)

;;; Unusual strings

(deftest find-all-symbols.3
  (let* ((name (make-array '(3) :initial-contents "NIL"
			   :element-type 'base-char))
	 (symbols (find-all-symbols name)))
    (values
     (notnot (every #'(lambda (s) (string= (symbol-name s) "NIL")) symbols))
     (some #'not symbols)))
  t t)

(deftest find-all-symbols.4
  (let* ((name (make-array '(5) :initial-contents "NILXY"
			   :fill-pointer 3
			   :element-type 'character))
	 (symbols (find-all-symbols name)))
    (values
     (notnot (every #'(lambda (s) (string= (symbol-name s) "NIL")) symbols))
     (some #'not symbols)))
  t t)

(deftest find-all-symbols.5
  (let* ((name (make-array '(5) :initial-contents "NILXY"
			   :fill-pointer 3
			   :element-type 'base-char))
	 (symbols (find-all-symbols name)))
    (values
     (notnot (every #'(lambda (s) (string= (symbol-name s) "NIL")) symbols))
     (some #'not symbols)))
  t t)

(deftest find-all-symbols.6
  (let* ((name (make-array '(3) :initial-contents "NIL"
			   :adjustable t
			   :element-type 'base-char))
	 (symbols (find-all-symbols name)))
    (values
     (notnot (every #'(lambda (s) (string= (symbol-name s) "NIL")) symbols))
     (some #'not symbols)))
  t t)

(deftest find-all-symbols.7
  (let* ((name (make-array '(3) :initial-contents "NIL"
			   :adjustable t
			   :element-type 'character))
	 (symbols (find-all-symbols name)))
    (values
     (notnot (every #'(lambda (s) (string= (symbol-name s) "NIL")) symbols))
     (some #'not symbols)))
  t t)

(deftest find-all-symbols.8
  (let* ((type 'character)
	 (name0 (make-array '(9) :initial-contents "XYZNILABC"
			    :element-type type))
	 (name (make-array '(3) :element-type type
			   :displaced-to name0
			   :displaced-index-offset 3))
	 (symbols (find-all-symbols name)))
    (values
     (notnot (every #'(lambda (s) (string= (symbol-name s) "NIL")) symbols))
     (some #'not symbols)))
  t t)

(deftest find-all-symbols.9
  (let* ((type 'base-char)
	 (name0 (make-array '(9) :initial-contents "XYZNILABC"
			    :element-type type))
	 (name (make-array '(3) :element-type type
			   :displaced-to name0
			   :displaced-index-offset 3))
	 (symbols (find-all-symbols name)))
    (values
     (notnot (every #'(lambda (s) (string= (symbol-name s) "NIL")) symbols))
     (some #'not symbols)))
  t t)  

;;; Error tests

(deftest find-all-symbols.error.1
  (signals-error (find-all-symbols) program-error)
  t)

(deftest find-all-symbols.error.2
  (signals-error (find-all-symbols "CAR" nil) program-error)
  t)


