;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jul  7 07:48:01 2004
;;;; Contains: Tests of PPRINT-NEWLINE

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(defmacro def-pprint-newline-test (name form expected-value &rest key-args)
  `(def-pprint-test ,name
     (with-output-to-string
       (*standard-output*)
       (pprint-logical-block (*standard-output* nil) ,form))
     ,expected-value
     ,@key-args))

;;; NIL designates the standard output

(def-pprint-test pprint-newline.1
  (with-output-to-string
    (*standard-output*)
    (pprint-logical-block
     (*standard-output* nil)
     (dotimes (i 8)
       (write-char #\A)
       (write-char #\Space)
       (pprint-newline :fill nil))))
  "A A A A A
A A A "
  :margin 10)

;;; T designates the stream *terminal-io*
(def-pprint-test pprint-newline.2
  (with-output-to-string
    (os)
    (with-input-from-string
     (is "")
     (with-open-stream
      (*terminal-io* (make-two-way-stream is os))
      (pprint-logical-block
       (*terminal-io* nil)
       (dotimes (i 8)
	 (write "A " :stream t)
	 (pprint-newline :fill t))))))
  "A A A A A
A A A "
  :margin 10)

;;; No stream is standard output

(def-pprint-test pprint-newline.3
  (with-output-to-string
    (*standard-output*)
    (pprint-logical-block
     (*standard-output* nil)
     (dotimes (i 8)
       (write-char #\A)
       (write-char #\Space)
       (pprint-newline :fill))))
  "A A A A A
A A A "
  :margin 10)

;;; :linear

(def-ppblock-test pprint-newline.linear.1
  (progn
   (dotimes (i 2) (write "A ") (pprint-newline :fill))
   (write "B ") (pprint-newline :linear)
   (dotimes (i 3) (write "A ") (pprint-newline :fill)))
  "A A B
A A A "
  :margin 10)

(def-ppblock-test pprint-newline.linear.2
  (progn
   (dotimes (i 2) (write "A ") (pprint-newline :fill))
   (write "B ") (pprint-newline :linear)
   (dotimes (i 2) (write "C ") (pprint-newline :fill))
   (write "D ") (pprint-newline :linear)
   (dotimes (i 3) (write "A ") (pprint-newline :fill)))
  "A A B
C C D
A A A "
  :margin 10)

(def-ppblock-test pprint-newline.linear.3
  (dotimes (i 4) (write "A ") (pprint-newline :linear))
  "A A A A "
  :margin 10)

(def-ppblock-test pprint-newline.linear.4
  (dotimes (i 4) (write "A ") (pprint-newline :linear))
  "A A A A "
  :margin 10
  :miser 10)

(def-ppblock-test pprint-newline.linear.5
  (dotimes (i 10) (write "A ") (pprint-newline :linear))
  "A A A A A A A A A A "
  :margin 10
  :pretty nil)

(def-ppblock-test pprint-newline.linear.6
  (dotimes (i 4) (write "A             ") (pprint-newline :linear))
  "A
A
A
A
"
  :margin 10)

(def-ppblock-test pprint-newline.linear.7
  (progn
    (dotimes (i 4) (write "A ") (pprint-newline :linear))
    (terpri)
    (dotimes (i 4) (write "A ") (pprint-newline :linear)))
  "A
A
A
A

A
A
A
A
"
  :margin 10)

(def-ppblock-test pprint-newline.linear.8
  (progn
    (pprint-logical-block (*standard-output* nil)
			  (dotimes (i 4)
			    (write "A ")
			    (pprint-newline :linear)))
    (pprint-newline :linear)
    (pprint-logical-block (*standard-output* nil)
			  (dotimes (i 4)
			    (write "A ")
			    (pprint-newline :linear))))
  "A A A A
A A A A "
  :margin 10)
    
(def-ppblock-test pprint-newline.linear.9
  (dotimes (i 10) (write "A ") (let ((*print-pretty* nil)) (pprint-newline :linear)))
  "A A A A A A A A A A "
  :margin 10)

(deftest pprint-newline.linear.10
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*print-escape* nil)
	 (*print-pretty* t)
	 (*print-right-margin* 4)
	 (*print-miser-width* nil))
     (with-output-to-string
       (*standard-output*)
       (dotimes (i 5) (write "A ") (pprint-newline :linear)))))
  "A A A A A ")

;;; :miser

(def-ppblock-test pprint-newline.miser.1
  (dotimes (i 10) (write "A ") (pprint-newline :miser))
  "A A A A A A A A A A "
  :margin 10)

(def-ppblock-test pprint-newline.miser.2
  (dotimes (i 10) (write "A ") (pprint-newline :miser))
  "A A A A A A A A A A "
  :margin 10
  :miser 0)

(def-ppblock-test pprint-newline.miser.3
  (dotimes (i 10) (write "A ") (pprint-newline :miser))
  "A A A A A A A A A A "
  :margin 10
  :miser 9)

(def-ppblock-test pprint-newline.miser.4
  (dotimes (i 10) (write "A ") (pprint-newline :miser))
  "A
A
A
A
A
A
A
A
A
A
"
  :margin 10
  :miser 10)

(def-ppblock-test pprint-newline.miser.5
  (dotimes (i 10) (write "A ") (pprint-newline :miser))
  "A A A A A A A A A A "
  :margin 10
  :miser 10
  :pretty nil)

(def-ppblock-test pprint-newline.miser.6
  (progn
    (terpri)
    (write "A")
    (pprint-newline :miser))
  "
A
"
  :margin 20
  :miser 20)

(def-ppblock-test pprint-newline.miser.7
  (progn
    (pprint-newline :miser)
    (write "A")
    (terpri))
  "
A
"
  :margin 20
  :miser 20)

(def-ppblock-test pprint-newline.miser.8
  (progn
    (write "AAAA ")
    (pprint-newline :linear)
    (pprint-logical-block
     (*standard-output* nil)
     (dotimes (i 4) (write "A ") (pprint-newline :miser))))
  "AAAA
A A A A "
  :margin 10
  :miser 8)

(def-ppblock-test pprint-newline.miser.9
  (progn
    (write "AAAA ")
    (pprint-newline :fill)
    (pprint-logical-block
     (*standard-output* nil)
     (dotimes (i 4) (write "A ") (pprint-newline :miser))))
  "AAAA
A A A A "
  :margin 10
  :miser 8)

(def-ppblock-test pprint-newline.miser.10
  (pprint-logical-block (*standard-output* nil :prefix "(" :suffix ")")
			(write "A")
			(pprint-newline :miser)
			(pprint-newline :mandatory))
  "(A

 )"
  :margin 20
  :miser 20)

(def-ppblock-test pprint-newline.miser.11
  (pprint-logical-block (*standard-output* nil :prefix "(" :suffix ")")
			(write "A")
			(pprint-newline :miser)
			(pprint-newline :mandatory))
  "(A

 )"
  :margin 20
  :miser 19)

(def-ppblock-test pprint-newline.miser.12
  (pprint-logical-block (*standard-output* nil :prefix "(" :suffix ")")
			(write "A")
			(pprint-newline :miser)
			(pprint-newline :mandatory))
  "(A
 )"
  :margin 20
  :miser 18)

(deftest pprint-newline.miser.13
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*print-escape* nil)
	 (*print-pretty* t)
	 (*print-right-margin* 4)
	 (*print-miser-width* 4))
     (with-output-to-string
       (*standard-output*)
       (dotimes (i 5) (write "A ") (pprint-newline :miser)))))
  "A A A A A ")

;;; :fill

(def-ppblock-test pprint-newline.fill.1
  (dotimes (i 10) (write "A ") (pprint-newline :fill))
  "A A A A A
A A A A A "
  :margin 10)

(def-ppblock-test pprint-newline.fill.2
  (dotimes (i 10) (write "A ") (pprint-newline :fill))
  "A A A
A A A
A A A
A "
  :margin 6)

(def-ppblock-test pprint-newline.fill.3
  (dotimes (i 10) (write "A ") (pprint-newline :fill))
  "A A A
A A A
A A A
A "
  :margin 7)

(def-ppblock-test pprint-newline.fill.4
    (dotimes (i 10) (write "A ") (pprint-newline :fill))
  "A A A A A
A A A A A "
  :margin 10
  :miser 9)

(def-ppblock-test pprint-newline.fill.5
    (dotimes (i 10) (write "A ") (pprint-newline :fill))
  "A
A
A
A
A
A
A
A
A
A
"
  :margin 10
  :miser 10)

(def-ppblock-test pprint-newline.fill.6
    (dotimes (i 5)
      (write '(A B))
      (write #\Space)
      (pprint-newline :fill))
  "(A B) (A B)
(A B) (A B)
(A B) "
  :margin 12)

(def-ppblock-test pprint-newline.fill.7
    (dolist (x '(A (A B) (A A A A A A A A) X (C D) (E F)))
      (pprint-fill nil x)
      (write #\Space)
      (pprint-newline :fill))
  "A (A B)
(A A A A A
 A A A)
X (C D)
(E F) "
  :margin 12)

(def-ppblock-test pprint-newline.fill.8
    (dotimes (i 5)
      (write '(A B) :pretty nil)
      (write #\Space)
      (let ((*print-pretty* nil)) (pprint-newline :fill)))
  "(A B) (A B) (A B) (A B) (A B) "
  :margin 12)

(deftest pprint-newline.fill.9
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*print-escape* nil)
	 (*print-right-margin* 4)
	 (*print-pretty* t)
	 (*print-miser-width* nil))
     (with-output-to-string
       (*standard-output*)
       (dotimes (i 5) (write "A ") (pprint-newline :fill)))))
  "A A A A A ")

(deftest pprint-newline.fill.10
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*print-escape* nil)
	 (*print-right-margin* 4)
	 (*print-pretty* t)
	 (*print-miser-width* 4))
     (with-output-to-string
       (*standard-output*)
       (dotimes (i 5) (write "A ") (pprint-newline :fill)))))
  "A A A A A ")


;;; :mandatory

(def-ppblock-test pprint-newline.mandatory.1
  (dotimes (i 4) (write "A ") (pprint-newline :mandatory))
  "A
A
A
A
")

(def-ppblock-test pprint-newline.mandatory.2
  (dotimes (i 4) (write "A ") (pprint-newline :mandatory))
  "A
A
A
A
"
  :margin 10)

(def-ppblock-test pprint-newline.mandatory.3
  (progn
    (write "A ")
    (pprint-newline :mandatory)
    (write "A "))
  "A
A "
  :margin 1)

(def-ppblock-test pprint-newline.mandatory.4
  (dotimes (i 4) (write "A ") (pprint-newline :mandatory))
  "A A A A "
  :pretty nil)

(def-ppblock-test pprint-newline.mandatory.5
  (pprint-logical-block
   (*standard-output* nil :prefix "<<<" :suffix ">>>")
   (dotimes (i 4) (write "A ") (pprint-newline :mandatory))
   (write "A"))
  "<<<A
   A
   A
   A
   A>>>")

(deftest pprint-newline.mandatory.6
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*print-escape* nil)
	 (*print-pretty* t)
	 (*print-right-margin* 4)
	 (*print-miser-width* nil))
     (with-output-to-string
       (*standard-output*)
       (dotimes (i 5) (write "A ") (pprint-newline :mandatory)))))
  "A A A A A ")

;;; Error cases

(deftest pprint-newline.error.1
  (check-type-error #'pprint-newline
		    (typef '(member :linear :miser :fill :mandatory)))
  nil)

(deftest pprint-newline.error.1-unsafe
  (check-type-error #'(lambda (x) (declare (optimize (safety 0))) (pprint-newline x))
		    (typef '(member :linear :miser :fill :mandatory)))
  nil)

(deftest pprint-newline.error.2
  (signals-error (pprint-newline) program-error)
  t)

(deftest pprint-newline.error.3
  (signals-error (pprint-newline :fill nil nil) program-error)
  t)
