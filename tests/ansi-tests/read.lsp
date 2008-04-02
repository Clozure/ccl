;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Dec 31 07:52:06 2004
;;;; Contains: Tests of READ

(in-package :cl-test)

;;; Input stream designators

(deftest read.1
  (block done
    (with-input-from-string
     (is "1 2 3")
     (with-output-to-string
       (os)
       (with-open-stream
	(*terminal-io* (make-two-way-stream is os))
	(return-from done (read t))))))
  1)

(deftest read.2
  (with-input-from-string
   (*standard-input* "1 2 3")
   (read nil))
  1)

(deftest read.3
  (with-input-from-string
   (*standard-input* "1 2 3")
   (read))
  1)

(deftest read.4
  (with-input-from-string
   (s "1 2 3")
   (read s))
  1)

;;; eof handling

(deftest read.5
  (with-input-from-string (s "") (read s nil))
  nil)

(deftest read.6
  (with-input-from-string (s "") (read s nil 'foo))
  foo)

(deftest read.7
  (with-input-from-string (s "1") (read s))
  1)

(deftest read.8
  (let ((*package* (find-package "CL-TEST")))
    (with-input-from-string (s "X") (read s)))
  |X|)

(deftest read.9
  (with-input-from-string (s "1.2") (read s))
  1.2)

(deftest read.10
  (with-input-from-string (s "1.0s0") (read s))
  1.0s0)

(deftest read.11
  (with-input-from-string (s "1.0f0") (read s))
  1.0f0)

(deftest read.12
  (with-input-from-string (s "1.0d0") (read s))
  1.0d0)

(deftest read.13
  (with-input-from-string (s "1.0l0") (read s))
  1.0l0)

(deftest read.14
  (with-input-from-string (s "()") (read s))
  nil)

(deftest read.15
  (with-input-from-string (s "(1 2 3)") (read s))
  (1 2 3))

;;; Throwing away whitespace chars

(deftest read.16
  (with-standard-io-syntax
   (with-input-from-string
    (s ":ABC X")
    (assert (eq (read s) :|ABC|))
    (read-char s)))
  #\X)

(deftest read.17
  (with-standard-io-syntax
   (with-input-from-string
    (s ":ABC  X")
    (assert (eq (read s) :|ABC|))
    (read-char s)))
  #\Space)

(deftest read.18
  (with-standard-io-syntax
   (with-input-from-string
    (s ":ABC(")
    (assert (eq (read s) :|ABC|))
    (read-char s)))
  #\()

;;; eof value

(deftest read.19
  (with-input-from-string
   (s "")
   (read s nil 'foo))
  foo)			  

;;; Error tests

(deftest read.error.1
  (signals-error (with-input-from-string (s "") (read s)) end-of-file)
  t)

(deftest read.error.2
  (signals-error (with-input-from-string (s "") (read s)) stream-error)
  t)

(deftest read.error.3
  (signals-error (with-input-from-string (s "") (read s t)) stream-error)
  t)

(deftest read.error.4
  (signals-error (with-input-from-string (s "(") (read s nil)) end-of-file)
  t)

(deftest read.error.5
  (signals-error (with-input-from-string (s "(") (read s t)) end-of-file)
  t)

(deftest read.error.6
  (signals-error (with-input-from-string (s "#(") (read s t)) end-of-file)
  t)

(deftest read.error.7
  (signals-error (with-input-from-string (s "#S(") (read s t)) end-of-file)
  t)

;;; Note -- cannot easily test calls with RECURSIVE-P set to T
;;; These have to be done from reader macro functions so that READ is not
;;; called without having any requisite dynamic environment created
;;; around the call.

(deftest read.error.8
  (signals-error
   (with-input-from-string
    (s "1 2 3")
    (read s nil nil nil nil))
   program-error)
  t)
