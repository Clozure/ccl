;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan  1 08:54:28 2005
;;;; Contains: Tests of READ-PRESERVING-WHITESPACE

(in-package :cl-test)

;;; Input stream designators

(deftest read-preserving-whitespace.1
  (block done
    (with-input-from-string
     (is "1 2 3")
     (with-output-to-string
       (os)
       (with-open-stream
	(*terminal-io* (make-two-way-stream is os))
	(return-from done (read-preserving-whitespace t))))))
  1)

(deftest read-preserving-whitespace.2
  (with-input-from-string
   (*standard-input* "1 2 3")
   (read-preserving-whitespace nil))
  1)

(deftest read-preserving-whitespace.3
  (with-input-from-string
   (*standard-input* "1 2 3")
   (read-preserving-whitespace))
  1)

(deftest read-preserving-whitespace.4
  (with-input-from-string
   (s "1 2 3")
   (read-preserving-whitespace s))
  1)

;;; eof handling

(deftest read-preserving-whitespace.5
  (with-input-from-string (s "") (read-preserving-whitespace s nil))
  nil)

(deftest read-preserving-whitespace.6
  (with-input-from-string (s "") (read-preserving-whitespace s nil 'foo))
  foo)

(deftest read-preserving-whitespace.7
  (with-input-from-string (s "1") (read-preserving-whitespace s))
  1)

(deftest read-preserving-whitespace.8
  (let ((*package* (find-package "CL-TEST")))
    (with-input-from-string (s "X") (read-preserving-whitespace s)))
  |X|)

(deftest read-preserving-whitespace.9
  (with-input-from-string (s "1.2") (read-preserving-whitespace s))
  1.2)

(deftest read-preserving-whitespace.10
  (with-input-from-string (s "1.0s0") (read-preserving-whitespace s))
  1.0s0)

(deftest read-preserving-whitespace.11
  (with-input-from-string (s "1.0f0") (read-preserving-whitespace s))
  1.0f0)

(deftest read-preserving-whitespace.12
  (with-input-from-string (s "1.0d0") (read-preserving-whitespace s))
  1.0d0)

(deftest read-preserving-whitespace.13
  (with-input-from-string (s "1.0l0") (read-preserving-whitespace s))
  1.0l0)

(deftest read-preserving-whitespace.14
  (with-input-from-string (s "()") (read-preserving-whitespace s))
  nil)

(deftest read-preserving-whitespace.15
  (with-input-from-string (s "(1 2 3)") (read-preserving-whitespace s))
  (1 2 3))

;;; Throwing away whitespace chars

(deftest read-preserving-whitespace.16
  (with-standard-io-syntax
   (with-input-from-string
    (s ":ABC X")
    (assert (eq (read-preserving-whitespace s) :|ABC|))
    (read-char s)))
  #\Space)

(deftest read-preserving-whitespace.17
  (with-standard-io-syntax
   (with-input-from-string
    (s ":ABC  X")
    (assert (eq (read-preserving-whitespace s) :|ABC|))
    (read-char s)))
  #\Space)

(deftest read-preserving-whitespace.18
  (with-standard-io-syntax
   (with-input-from-string
    (s ":ABC(")
    (assert (eq (read-preserving-whitespace s) :|ABC|))
    (read-char s)))
  #\()

;;; eof value

(deftest read-preserving-whitespace.19
  (with-input-from-string
   (s "")
   (read-preserving-whitespace s nil 'foo))
  foo)			  

;;; Error tests

(deftest read-preserving-whitespace.error.1
  (signals-error (with-input-from-string (s "") (read-preserving-whitespace s)) end-of-file)
  t)

(deftest read-preserving-whitespace.error.2
  (signals-error (with-input-from-string (s "") (read-preserving-whitespace s)) stream-error)
  t)

(deftest read-preserving-whitespace.error.3
  (signals-error (with-input-from-string (s "") (read-preserving-whitespace s t)) stream-error)
  t)

(deftest read-preserving-whitespace.error.4
  (signals-error (with-input-from-string (s "(") (read-preserving-whitespace s nil)) end-of-file)
  t)

(deftest read-preserving-whitespace.error.5
  (signals-error (with-input-from-string (s "(") (read-preserving-whitespace s t)) end-of-file)
  t)

(deftest read-preserving-whitespace.error.6
  (signals-error (with-input-from-string (s "#(") (read-preserving-whitespace s t)) end-of-file)
  t)

(deftest read-preserving-whitespace.error.7
  (signals-error (with-input-from-string (s "#S(") (read-preserving-whitespace s t)) end-of-file)
  t)

;;; Note -- cannot easily test calls with RECURSIVE-P set to T.  These have to be
;;; done from read-preserving-whitespaceer macro functions so that READ-PRESERVING-WHITESPACE
;;; is not called without having any requisite dynamic environment created
;;; around the call.

(deftest read-preserving-whitespace.error.8
  (signals-error
   (with-input-from-string
    (s "1 2 3")
    (read-preserving-whitespace s nil nil nil nil))
   program-error)
  t)
