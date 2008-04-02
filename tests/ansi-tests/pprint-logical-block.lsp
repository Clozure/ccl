;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jul  4 07:17:52 2004
;;;; Contains: Tests of PPRINT-LOGICAL-BLOCK

(in-package :cl-test)

(deftest pprint-logical-block.1
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-right-margin* 100)
	 (*print-readably* nil))
     (with-open-stream
      (os (make-string-output-stream))
      (values
       (multiple-value-list (pprint-logical-block (os 1)))
       (get-output-stream-string os)))))
  (nil) "1")

(deftest pprint-logical-block.2
  (with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (val '(1 a (b) (c . d) 1.0s0 2.0f0 -3.0d0 4.0l0 1/2 #(x y z))))
     (string=t (with-output-to-string (s) (write val :stream s))
	       (with-output-to-string (s) (pprint-logical-block (s val) (write val :stream s))))))
  t)

(deftest pprint-logical-block.3
  (with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil))
     (with-output-to-string
       (*standard-output*)
       (pprint-logical-block (nil 1)))))
  "1")

(deftest pprint-logical-block.4
  (with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil))
     (with-output-to-string
       (os)
       (with-input-from-string
	(is "")
	(with-open-stream (*terminal-io* (make-two-way-stream is os))
			  (pprint-logical-block (t 1)))))))
  "1")

(deftest pprint-logical-block.5
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (val '(1)))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os val)
	(write (car val) :stream os)))))
  "1")

(deftest pprint-logical-block.6
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (val '(2)))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os val :prefix "[" :suffix "]")
	(write (car val) :stream os)))))
  "[2]")

(deftest pprint-logical-block.7
  :notes (:nil-vectors-are-strings)
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (val '(3)))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os val
	    :prefix (make-array '(0) :element-type nil)
	    :suffix (make-array '(0) :element-type nil))
	(write (car val) :stream os)))))
  "3")

(deftest pprint-logical-block.8
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (val '(4)))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os val
	    :prefix (make-array '(10) :element-type 'character
				:initial-contents "abcdefghij"
				:fill-pointer 3)
	    :suffix (make-array '(2) :element-type 'base-char
				:initial-contents "!?"
				:adjustable t))
	(write (car val) :stream os)))))
  "abc4!?")

(deftest pprint-logical-block.9
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (*print-level* 1)
	 (val '((4))))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os val :prefix "{" :suffix "}")
	(pprint-logical-block
	 (os (car val)  :prefix "[" :suffix "]")
	 (write (caar val) :stream os))))))
  "{#}")

(deftest pprint-logical-block.10
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (*print-level* 0)
	 (val '(5)))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os val :prefix "[" :suffix "]")
	(write (car val) :stream os)))))
  "#")

(deftest pprint-logical-block.11
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (val '(6)))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os val :per-line-prefix "abcd")
	(write (car val) :stream os)))))
  "abcd6")

(deftest pprint-logical-block.12
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (val '(a b c)))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os val :per-line-prefix "abcd")
	(write 1 :stream os)
	(terpri os)
	(terpri os)
	(write 2 :stream os)
	(terpri os)
	(write 3 :stream os)))))

  "abcd1
abcd
abcd2
abcd3")

;;; Same as pprint-logical-block.10, but *print-pretty* is bound to nil
(deftest pprint-logical-block.13
  (with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (*print-level* 0)
	 (val '(5)))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os val :prefix "[" :suffix "]")
	(write (car val) :stream os)))))
  "#")

;;; Both :suffix and :per-line-prefix may be supplied
(deftest pprint-logical-block.14
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (val '(6)))
     (with-output-to-string
       (os)
       (pprint-logical-block (os val :per-line-prefix "[" :suffix "]")
			     (write (car val) :stream os)))))
  "[6]")

;;; Declarations are allowed

(deftest pprint-logical-block.15
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (x 0))
     (with-output-to-string
       (os)
       (declare (integer x))
       (declare (optimize (safety 3))))))
  "")

;;; Two conditions that cause :prefix, :suffix to be omitted

(deftest pprint-logical-block.16
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (val 9))
     (with-output-to-string
       (os)
       (pprint-logical-block (os val :prefix "[" :suffix "]")
			     (write val :stream os)))))
  "9")

(deftest pprint-logical-block.17
  (with-standard-io-syntax
   (let* ((*print-pretty* t)
	  (*print-right-margin* 100)
	  (*print-readably* nil)
	  (*print-circle* t)
	  (v1 '(8))
	  (val (list v1 v1)))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os val :prefix "(" :suffix ")")
	(pprint-logical-block (os (car val) :prefix "(" :suffix ")")
			      (write (caar val) :stream os))
	(write-char #\Space os)
	(pprint-logical-block (os (cadr val) :prefix "(" :suffix ")")
			      (write (caadr val) :stream os))))))
  "(#1=(8) #1#)")

;;; Error cases

(deftest pprint-logical-block.error.1
  (check-type-error #'(lambda (x)
			(pprint-logical-block (*standard-output* '(1) :prefix x)))
		    #'stringp)
  nil)

(deftest pprint-logical-block.error.1-unsafe
  (check-type-error #'(lambda (x)
			(declare (optimize (safety 0)))
			(pprint-logical-block (*standard-output* '(1) :prefix x)))
		    #'stringp)
  nil)

(deftest pprint-logical-block.error.2
  (check-type-error #'(lambda (x)
			(pprint-logical-block (*standard-output* '(1) :suffix x)))
		    #'stringp)
  nil)

(deftest pprint-logical-block.error.2-unsafe
  (check-type-error #'(lambda (x)
			(declare (optimize (safety 0)))
			(pprint-logical-block (*standard-output* '(1) :suffix x)))
		    #'stringp)
  nil)

(deftest pprint-logical-block.error.3
  (check-type-error #'(lambda (x)
			(pprint-logical-block (*standard-output* '(1) :per-line-prefix x)))
		    #'stringp)
  nil)

(deftest pprint-logical-block.error.3-unsafe
  (check-type-error #'(lambda (x)
			(declare (optimize (safety 0)))
			(pprint-logical-block (*standard-output* '(1) :per-line-prefix x)))
		    #'stringp)
  nil)

(deftest pprint-logical-block.error.4
  (signals-error (with-standard-io-syntax
		  (let ((*print-pretty* t)
			(*print-right-margin* 100)
			(*print-readably* nil)
			(val '(7)))
		    (pprint-logical-block (os val :prefix "" :per-line-prefix "")
					  (write (car val) :stream os))))
		 error)
  t)

(deftest pprint-logical-block.error.4-unsafe
  (signals-error (with-standard-io-syntax
		  (let ((*print-pretty* t)
			(*print-right-margin* 100)
			(*print-readably* nil)
			(val '(7)))
		    (pprint-logical-block (os val :prefix "" :per-line-prefix "")
					  (write (car val) :stream os))))
		 error
		 :safety 0)
  t)
