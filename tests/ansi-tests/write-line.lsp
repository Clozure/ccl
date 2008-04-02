;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 19 06:49:26 2004
;;;; Contains: Tests of WRITE-LINE

(in-package :cl-test)

(deftest write-line.1
  (let (result)
    (values
     (with-output-to-string
       (*standard-output*)
       (setq result (multiple-value-list (write-line ""))))
     result))
  #.(string #\Newline)
  (""))

(deftest write-line.2
  :notes (:nil-vectors-are-strings)
  (let (result)
    (values
     (with-output-to-string
       (*standard-output*)
       (setq result
	     (multiple-value-list
	      (write-line (make-array '(0) :element-type nil)))))
     result))
  #.(string #\Newline)
  (""))

(deftest write-line.3
  (let (result)
    (values
     (with-output-to-string
       (*standard-output*)
       (setq result (multiple-value-list (write-line "abcde"))))
     result))
  #.(concatenate 'string "abcde" (string #\Newline))
  ("abcde"))

(deftest write-line.4
  (let (result)
    (values
     (with-output-to-string
       (s)
       (setq result (multiple-value-list (write-line "abcde" s :start 1))))
     result))
  #.(concatenate 'string "bcde" (string #\Newline))
  ("abcde"))

(deftest write-line.5
  (let (result)
    (values
     (with-output-to-string
       (s)
       (setq result (multiple-value-list
		     (write-line "abcde" s :start 1 :end 3))))
     result))
  #.(concatenate 'string "bc" (string #\Newline))
  ("abcde"))

(deftest write-line.6
  (let (result)
    (values
     (with-output-to-string
       (s)
       (setq result (multiple-value-list
		     (write-line "abcde" s :start 1 :end nil))))
     result))
  #.(concatenate 'string "bcde" (string #\Newline))
  ("abcde"))

(deftest write-line.7
  (let (result)
    (values
     (with-output-to-string
       (s)
       (setq result (multiple-value-list (write-line "abcde" s :end 3))))
     result))
  #.(concatenate 'string "abc" (string #\Newline))
  ("abcde"))

(deftest write-line.8
  (let (result)
    (values
     (with-output-to-string
       (s)
       (setq result (multiple-value-list
		     (write-line "abcde" s :end 3 :allow-other-keys nil))))
     result))
  #.(concatenate 'string "abc" (string #\Newline))
  ("abcde"))

(deftest write-line.9
  (let (result)
    (values
     (with-output-to-string
       (s)
       (setq result
	     (multiple-value-list
	      (write-line "abcde" s :end 3 :allow-other-keys t :foo 'bar))))
     result))
  #.(concatenate 'string "abc" (string #\Newline))
  ("abcde"))

(deftest write-line.10
  (let (result)
    (values
     (with-output-to-string
       (s)
       (setq result (multiple-value-list
		     (write-line "abcde" s :end 3 :end 2))))
     result))
  #.(concatenate 'string "abc" (string #\Newline))
  ("abcde"))

(deftest write-line.11
  (with-input-from-string
   (is "abcd")
   (with-output-to-string
     (os)
     (let ((*terminal-io* (make-two-way-stream is os)))
       (write-line "951" t)
       (close *terminal-io*))))
  #.(concatenate 'string "951" (string #\Newline)))

(deftest write-line.12
  (with-output-to-string
    (*standard-output*)
    (write-line "-=|!" nil))
  #.(concatenate 'string "-=|!" (string #\Newline)))

;;; Specialized string tests

(deftest write-line.13
  (do-special-strings
   (s "abcde" nil)
   (assert (equal
	    (with-output-to-string
	      (*standard-output*)
	      (multiple-value-list (write-line "abcde")))
	    #.(concatenate 'string "abcde" (string #\Newline)))))
  nil)

;;; Error tests

(deftest write-line.error.1
  (signals-error (write-line) program-error)
  t)

(deftest write-line.error.2
  (signals-error (write-line "" *standard-output* :start) program-error)
  t)

(deftest write-line.error.3
  (signals-error (write-line "" *standard-output* :foo nil) program-error)
  t)

(deftest write-line.error.4
  (signals-error (write-line "" *standard-output*
			       :allow-other-keys nil
			       :foo nil)
		 program-error)
  t)

