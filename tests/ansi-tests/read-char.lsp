;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 18 08:53:56 2004
;;;; Contains: Tests of READ-CHAR

(in-package :cl-test)

(deftest read-char.1
  (with-input-from-string
   (*standard-input* "a")
   (read-char))
  #\a)

(deftest read-char.2
  (with-input-from-string
   (*standard-input* "abc")
   (values
    (read-char)
    (read-char)
    (read-char)))
  #\a #\b #\c)

(when (code-char 0)
  (deftest read-char.3
    (with-input-from-string
     (*standard-input* (concatenate 'string
				    "a"
				    (string (code-char 0))
				    "b"))
     (values
      (read-char)
      (read-char)
      (read-char)))
    #\a #.(code-char 0) #\b))

(deftest read-char.4
  (with-input-from-string
   (s "abc")
   (values
    (read-char s)
    (read-char s)
    (read-char s)))
  #\a #\b #\c)

(deftest read-char.5
  (with-input-from-string
   (s "")
   (read-char s nil))
  nil)

(deftest read-char.6
  (with-input-from-string
   (s "")
   (read-char s nil 'foo))
  foo)

(deftest read-char.7
  (with-input-from-string
   (s "abc")
   (values
    (read-char s nil nil)
    (read-char s nil nil)
    (read-char s nil nil)))
  #\a #\b #\c)

(deftest read-char.8
  (with-input-from-string
   (s "abc")
   (values
    (read-char s nil t)
    (read-char s nil t)
    (read-char s nil t)))
  #\a #\b #\c)

(deftest read-char.9
  (with-input-from-string
   (is "!?*")
   (let ((*terminal-io* (make-two-way-stream is (make-string-output-stream))))
     (read-char t)))
  #\!)

(deftest read-char.10
  (with-input-from-string
   (*standard-input* "345")
   (read-char nil))
  #\3)


;;; Error tests

(deftest read-char.error.1
  (signals-error
   (with-input-from-string
    (s "abc")
    (read-char s nil nil nil nil))
   program-error)
  t)

(deftest read-char.error.2
  (signals-error-always
   (with-input-from-string
    (s "")
    (read-char s))
   end-of-file)
  t t)

(deftest read-char.error.3
  (signals-error-always
   (with-input-from-string
    (s "")
    (read-char s t))
   end-of-file)
  t t)

(deftest read-char.error.4
  (signals-error-always
   (with-input-from-string
    (s "")
    (read-char s t t))
   end-of-file)
  t t)
