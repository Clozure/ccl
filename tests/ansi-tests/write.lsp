;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jul 15 06:43:55 2004
;;;; Contains: Tests of WRITE

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")
;; (compile-and-load "write-aux.lsp")

;;; This function is also incidentally tested elsewhere.

(deftest write.1
  (random-write-test 1000)
  nil)

(deftest write.2
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (with-output-to-string
       (*standard-output*)
       (write 2 :stream nil))))
  "2")

(deftest write.3
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (with-output-to-string
       (os)
       (with-input-from-string
	(is "")
	(with-open-stream (*terminal-io* (make-two-way-stream is os))
			  (write 3 :stream t))))))
  "3")

(deftest write.4
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (with-output-to-string
       (os)
       (write 4 :stream os))))
  "4")

(deftest write.5
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (with-output-to-string
       (*standard-output*)
       (write 5 :allow-other-keys nil))))
  "5")

(deftest write.6
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (with-output-to-string
       (*standard-output*)
       (write 6 :allow-other-keys t :foo 'bar))))
  "6")

(deftest write.7
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (with-output-to-string
       (*standard-output*)
       (write 7 :base 10 :base 3))))
  "7")

;;; Error tests

(deftest write.error.1
  (signals-error (write) program-error)
  t)

(deftest write.error.2
  (signals-error (write 1 :stream) program-error)
  t)

(deftest write.error.3
  (signals-error (write 1 :allow-other-keys nil :foo 'bar) program-error)
  t)

(deftest write.error.4
  (signals-error (write 1 :allow-other-keys nil :allow-other-keys t :foo 'bar) program-error)
  t)








