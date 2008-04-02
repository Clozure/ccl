;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 18 20:32:38 2004
;;;; Contains: Tests of READ-CHAR-NO-HANG

(in-package :cl-test)

(deftest read-char-no-hang.1
  (with-input-from-string
   (*standard-input* "a")
   (read-char-no-hang))
  #\a)

(deftest read-char-no-hang.2
  (with-input-from-string
   (*standard-input* "abc")
   (values
    (read-char-no-hang)
    (read-char-no-hang)
    (read-char-no-hang)))
  #\a #\b #\c)

(when (code-char 0)
  (deftest read-char-no-hang.3
    (with-input-from-string
     (*standard-input* (concatenate 'string
				    "a"
				    (string (code-char 0))
				    "b"))
     (values
      (read-char-no-hang)
      (read-char-no-hang)
      (read-char-no-hang)))
    #\a #.(code-char 0) #\b))

(deftest read-char-no-hang.4
  (with-input-from-string
   (s "abc")
   (values
    (read-char-no-hang s)
    (read-char-no-hang s)
    (read-char-no-hang s)))
  #\a #\b #\c)

(deftest read-char-no-hang.5
  (with-input-from-string
   (s "")
   (read-char-no-hang s nil))
  nil)

(deftest read-char-no-hang.6
  (with-input-from-string
   (s "")
   (read-char-no-hang s nil 'foo))
  foo)

(deftest read-char-no-hang.7
  (with-input-from-string
   (s "abc")
   (values
    (read-char-no-hang s nil nil)
    (read-char-no-hang s nil nil)
    (read-char-no-hang s nil nil)))
  #\a #\b #\c)

(deftest read-char-no-hang.8
  (with-input-from-string
   (s "abc")
   (values
    (read-char-no-hang s nil t)
    (read-char-no-hang s nil t)
    (read-char-no-hang s nil t)))
  #\a #\b #\c)

(deftest read-char-no-hang.9
  (with-input-from-string
   (is "!?*")
   (let ((*terminal-io* (make-two-way-stream is (make-string-output-stream))))
     (read-char-no-hang t)))
  #\!)

(deftest read-char-no-hang.10
  (with-input-from-string
   (*standard-input* "345")
   (read-char-no-hang nil))
  #\3)

;;; Need a test of the non-hanging.
;;; This is hard to do portably.

;;; Error tests

(deftest read-char-no-hang.error.1
  (signals-error
   (with-input-from-string
    (s "abc")
    (read-char-no-hang s nil nil nil nil))
   program-error)
  t)

(deftest read-char-no-hang.error.2
  (signals-error-always
   (with-input-from-string
    (s "")
    (read-char-no-hang s))
   end-of-file)
  t t)

(deftest read-char-no-hang.error.3
  (signals-error-always
   (with-input-from-string
    (s "")
    (read-char-no-hang s t))
   end-of-file)
  t t)

(deftest read-char-no-hang.error.4
  (signals-error-always
   (with-input-from-string
    (s "")
    (read-char-no-hang s t t))
   end-of-file)
  t t)
