;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jul  3 08:50:40 2004
;;;; Contains: Tests of PPRINT-INDENT

(in-package :cl-test)

(deftest pprint-indent.1
  (with-standard-io-syntax
   (let ((*print-pretty* nil))
     (with-open-stream (*standard-output* (make-string-output-stream))
		       (pprint-indent :block 0))))
  nil)

(deftest pprint-indent.2
  (with-standard-io-syntax
   (let ((*print-pretty* nil))
     (with-open-stream (*standard-output* (make-broadcast-stream))
		       (pprint-indent :current 0))))
  nil)

(deftest pprint-indent.3
  (with-standard-io-syntax
   (let ((*print-pretty* nil))
     (with-open-stream (s (make-string-output-stream))
		       (pprint-indent :current 10 s))))
  nil)

(deftest pprint-indent.4
  (with-standard-io-syntax
   (let ((*print-pretty* nil))
     (with-open-stream (s (make-string-output-stream))
		       (pprint-indent :block 1/2 s))))
  nil)

(deftest pprint-indent.5
  (with-standard-io-syntax
   (let ((*print-pretty* nil))
     (with-open-stream (s (make-string-output-stream))
		       (pprint-indent :block 0.1 s))))
  nil)

(deftest pprint-indent.6
  (with-standard-io-syntax
   (let ((*print-pretty* nil))
     (loop for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
	   unless
	   (equal
	    (multiple-value-list
	     (with-open-stream (s (make-string-output-stream))
			       (pprint-indent :block x s)))
	    '(nil))
	   collect x)))
  nil)

(deftest pprint-indent.7
  (with-standard-io-syntax
   (let ((*print-pretty* nil))
     (with-open-stream (*standard-output* (make-broadcast-stream))
		       (pprint-indent :current 0 nil))))
  nil)

(deftest pprint-indent.8
  (with-standard-io-syntax
   (let ((*print-pretty* nil))
     (with-open-stream
      (os (make-string-output-stream))
      (with-open-stream
       (is (make-string-input-stream ""))
       (with-open-stream (*terminal-io* (make-two-way-stream is os))
			 (pprint-indent :current 0 t))))))
  nil)

;;; Now test with pprint-logical-block

;;; :current

(deftest pprint-indent.9
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-escape* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(|M| |M|))
	(write '|M| :stream os)
	(pprint-indent :current 3 os)
	(pprint-newline :mandatory os)
	(write '|M| :stream os)))))
  "M
    M")

(deftest pprint-indent.10
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-escape* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(|M| |M|) :prefix "(" :suffix ")")
	(write '|M| :stream os)
	(pprint-indent :current 1 os)
	(pprint-newline :mandatory os)
	(write '|M| :stream os)))))
  "(M
   M)")

(deftest pprint-indent.11
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-escape* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(|M| |M|) :prefix "(" :suffix ")")
	(write '|M| :stream os)
	(pprint-indent :current -1 os)
	(pprint-newline :mandatory os)
	(write '|M| :stream os)))))
  "(M
 M)")

(deftest pprint-indent.12
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-escape* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(|M| |M|) :prefix "(" :suffix ")")
	(write '|M| :stream os)
	(pprint-indent :current -2.0 os)
	(pprint-newline :mandatory os)
	(write '|M| :stream os)))))
  "(M
M)")

;;; :block

(deftest pprint-indent.13
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-escape* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(|M| |M|))
	(write '|MMM| :stream os)
	(pprint-indent :block 0 os)
	(pprint-newline :mandatory os)
	(write '|MMMMM| :stream os)))))
  "MMM
MMMMM")

(deftest pprint-indent.13a
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-escape* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(|M| |M|) :prefix "(" :suffix ")")
	(write '|MMM| :stream os)
	(pprint-indent :block 0 os)
	(pprint-newline :mandatory os)
	(write '|MMMMM| :stream os)))))
  "(MMM
 MMMMM)")

(deftest pprint-indent.14
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-escape* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(|M| |M|))
	(write '|MMM| :stream os)
	(pprint-indent :block 1 os)
	(pprint-newline :mandatory os)
	(write '|MMMMM| :stream os)))))
  "MMM
 MMMMM")

(deftest pprint-indent.15
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-escape* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(|M| |M|))
	(write '|MMM| :stream os)
	(pprint-indent :block -1 os)
	(pprint-newline :mandatory os)
	(write '|MMMMM| :stream os)))))
  "MMM
MMMMM")

(deftest pprint-indent.16
  (loop for n in '(3.0s0 3.0f0 3.0d0 3.0l0)
	unless (string=
		(with-standard-io-syntax
		 (let ((*print-pretty* t)
		       (*print-readably* nil)
		       (*print-right-margin* 100)
		       (*print-escape* nil))
		   (with-output-to-string
		     (os)
		     (pprint-logical-block
		      (os '(|M| |M|))
		      (write '|MMM| :stream os)
		      (pprint-indent :block n os)
		      (pprint-newline :mandatory os)
		      (write '|MMMMM| :stream os)))))
		"MMM
   MMMMM")
	collect n)
  nil)

;;; *print-pretty* must be true for pprint-indent to have an effect

(deftest pprint-indent.17
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-escape* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(|M| |M|))
	(write '|M| :stream os)
	(let ((*print-pretty* nil)) (pprint-indent :current 3 os))
	(pprint-newline :mandatory os)
	(write '|M| :stream os)))))
  "M
M")

(deftest pprint-indent.18
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-escape* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(|M| |M|))
	(write '|M| :stream os)
	(let ((*print-pretty* nil)) (pprint-indent :block 3 os))
	(pprint-newline :mandatory os)
	(write '|M| :stream os)))))
  "M
M")

;;; indentation interaction with :per-line-prefix

(deftest pprint-indent.19
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-escape* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(|M| |M| |M|) :per-line-prefix ">>>>")
	(write '|M| :stream os)
	(pprint-indent :block 2 os)
	(write #\Space :stream os)
	(write '|M| :stream os)
	(pprint-newline :mandatory os)
	(write '|M| :stream os)))))
  ">>>>M M
>>>>  M")

(deftest pprint-indent.20
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-escape* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(|M| |M|) :per-line-prefix ">>>>")
	(write '|M| :stream os)
	(pprint-indent :block -1 os)
	(pprint-newline :mandatory os)
	(write '|M| :stream os)))))
  ">>>>M
>>>>M")

(deftest pprint-indent.21
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-escape* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(|M| |M| |M| |M|) :per-line-prefix ">>>>")
	(write '|M| :stream os)
	(pprint-indent :block 3 os)
	(pprint-newline :mandatory os)
	(write '|M| :stream os)
	(pprint-indent :current -2 os)
	(pprint-newline :mandatory os)
	(write '|M| :stream os)
	(pprint-indent :current -5 os)
	(pprint-newline :mandatory os)
	(write '|M| :stream os)
	))))
	
  ">>>>M
>>>>   M
>>>>  M
>>>>M")

;;; In miser mode, indentation is ignored

(deftest pprint-indent.22
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-miser-width* 200)
	 (*print-escape* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(1 2 3) :prefix "(" :suffix ")")
	(write 1 :stream os)
	(pprint-indent :current 1 os)
	(pprint-newline :mandatory os)
	(write 2 :stream os)
	(pprint-indent :block 3 os)
	(pprint-newline :mandatory os)
	(write 3 :stream os)))))
  "(1
 2
 3)")

;;; TERPRI or printing newline characters does not invoke indentation

(deftest pprint-indent.23
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-escape* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(1 2 3 4))
	(pprint-indent :block 2 os)
	(write 1 :stream os)
	(terpri os)
	(write 2 :stream os)
	(write #\Newline :stream os)
	(write 3 :stream os)
	(pprint-newline :mandatory os)
	(write 4 :stream os)))))
  "1
2
3
  4")

;;; Error cases

(deftest pprint-indent.error.1
  (signals-error (pprint-indent) program-error)
  t)

(deftest pprint-indent.error.2
  (signals-error (pprint-indent :current) program-error)
  t)

(deftest pprint-indent.error.3
  (signals-error (pprint-indent :block 0 *standard-output* nil) program-error)
  t)

(deftest pprint-indent.error.4
  (loop for x in *mini-universe*
	when (and (not (member x '(:block :current)))
		  (not (eval `(signals-error (pprint-indent ',x 0) error))))
	collect x)
  nil)

(deftest pprint-indent.error.4-unsafe
  (loop for x in *mini-universe*
	when (and (not (member x '(:block :current)))
		  (not (eval `(signals-error (locally (declare (optimize (safety 0))) (pprint-indent ',x 0))
					     error))))
	collect x)
  nil)

