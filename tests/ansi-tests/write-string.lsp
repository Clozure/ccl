;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 18 21:13:32 2004
;;;; Contains: Tests of WRITE-STRING

(in-package :cl-test)

(deftest write-string.1
  (let (result)
    (values
     (with-output-to-string
       (*standard-output*)
       (setq result (multiple-value-list (write-string ""))))
     result))
  "" (""))

(deftest write-string.2
  :notes (:nil-vectors-are-strings)
  (let (result)
    (values
     (with-output-to-string
       (*standard-output*)
       (setq result
	     (multiple-value-list
	      (write-string (make-array '(0) :element-type nil)))))
     result))
  "" (""))

(deftest write-string.3
  (let (result)
    (values
     (with-output-to-string
       (*standard-output*)
       (setq result (multiple-value-list (write-string "abcde"))))
     result))
  "abcde" ("abcde"))

(deftest write-string.4
  (let (result)
    (values
     (with-output-to-string
       (s)
       (setq result (multiple-value-list (write-string "abcde" s :start 1))))
     result))
  "bcde" ("abcde"))

(deftest write-string.5
  (let (result)
    (values
     (with-output-to-string
       (s)
       (setq result (multiple-value-list
		     (write-string "abcde" s :start 1 :end 3))))
     result))
  "bc" ("abcde"))

(deftest write-string.6
  (let (result)
    (values
     (with-output-to-string
       (s)
       (setq result (multiple-value-list
		     (write-string "abcde" s :start 1 :end nil))))
     result))
  "bcde" ("abcde"))

(deftest write-string.7
  (let (result)
    (values
     (with-output-to-string
       (s)
       (setq result (multiple-value-list (write-string "abcde" s :end 3))))
     result))
  "abc" ("abcde"))

(deftest write-string.8
  (let (result)
    (values
     (with-output-to-string
       (s)
       (setq result (multiple-value-list
		     (write-string "abcde" s :end 3 :allow-other-keys nil))))
     result))
  "abc" ("abcde"))

(deftest write-string.9
  (let (result)
    (values
     (with-output-to-string
       (s)
       (setq result
	     (multiple-value-list
	      (write-string "abcde" s :end 3 :allow-other-keys t :foo 'bar))))
     result))
  "abc" ("abcde"))

(deftest write-string.10
  (let (result)
    (values
     (with-output-to-string
       (s)
       (setq result (multiple-value-list
		     (write-string "abcde" s :end 3 :end 2))))
     result))
  "abc" ("abcde"))

(deftest write-string.11
  (with-input-from-string
   (is "abcd")
   (with-output-to-string
     (os)
     (let ((*terminal-io* (make-two-way-stream is os)))
       (write-string "951" t)
       (close *terminal-io*))))
  "951")

(deftest write-string.12
  (with-output-to-string
    (*standard-output*)
    (write-string "-=|!" nil))
  "-=|!")

;;; Specialized string tests

(deftest write-string.13
  (let (result)
    (do-special-strings
     (s "abcde" nil)
     (assert (equal
	      (with-output-to-string
		(*standard-output*)
		(setq result (multiple-value-list (write-string "abcde"))))
	      "abcde"))
     (assert (equal result '("abcde")))))
  nil)

;;; Error tests

(deftest write-string.error.1
  (signals-error (write-string) program-error)
  t)

(deftest write-string.error.2
  (signals-error (write-string "" *standard-output* :start) program-error)
  t)

(deftest write-string.error.3
  (signals-error (write-string "" *standard-output* :foo nil) program-error)
  t)

(deftest write-string.error.4
  (signals-error (write-string "" *standard-output*
			       :allow-other-keys nil
			       :foo nil)
		 program-error)
  t)
