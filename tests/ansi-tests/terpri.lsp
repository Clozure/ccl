;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 18 20:35:57 2004
;;;; Contains: Tests of TERPRI

(in-package :cl-test)

(deftest terpri.1
  (let (result)
    (values
     (with-output-to-string
       (*standard-output*)
       (write-char #\a)
       (setq result (terpri)))
     result))
  #.(concatenate 'string "a" (string #\Newline))
  nil)

(deftest terpri.2
  (let (result)
    (values
     (with-output-to-string
       (s)
       (write-char #\a s)
       (setq result (terpri s)))
     result))
  #.(concatenate 'string "a" (string #\Newline))
  nil)

(deftest terpri.3
  (with-output-to-string
    (s)
    (write-char #\x s)
    (terpri s)
    (terpri s)
    (write-char #\y s))
  #.(concatenate 'string "x" (string #\Newline) (string #\Newline) "y"))

(deftest terpri.4
  (with-output-to-string
    (os)
    (let ((*terminal-io* (make-two-way-stream *standard-input* os)))
      (terpri t)
      (finish-output t)))
  #.(string #\Newline))

(deftest terpri.5
  (with-output-to-string
    (*standard-output*)
    (terpri nil))
  #.(string #\Newline))

;;; Error tests

(deftest terpri.error.1
  (signals-error
   (with-output-to-string
     (s)
     (terpri s nil))
   program-error)
  t)

