;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 18 20:41:18 2004
;;;; Contains: Tests of FRESH-LINE

(in-package :cl-test)

(deftest fresh-line.1
  (let (result)
    (values
     (with-output-to-string
       (*standard-output*)
       (write-char #\a)
       (setq result (notnot (fresh-line))))
     result))
  #.(concatenate 'string "a" (string #\Newline))
  t)

(deftest fresh-line.2
  (let (result)
    (values
     (with-output-to-string
       (s)
       (write-char #\a s)
       (setq result (notnot (fresh-line s))))
     result))
  #.(concatenate 'string "a" (string #\Newline))
  t)

(deftest fresh-line.3
  (with-output-to-string
    (s)
    (write-char #\x s)
    (fresh-line s)
    (fresh-line s)
    (write-char #\y s))
  #.(concatenate 'string "x" (string #\Newline) "y"))

(deftest fresh-line.4
  (let (result)
    (values
     (with-output-to-string
       (*standard-output*)
       (setq result (multiple-value-list (fresh-line))))
     result))
  "" (nil))

(deftest fresh-line.5
  (let (result)
    (values
     (with-output-to-string
       (s)
       (write-char #\Space s)
       (setq result
	     (list
	      (multiple-value-list (notnot-mv (fresh-line s)))
	      (multiple-value-list (fresh-line s))
	      (multiple-value-list (fresh-line s)))))
     result))
  " 
" ((t) (nil) (nil)))

(deftest fresh-line.6
  (with-output-to-string
    (os)
    (let ((*terminal-io* (make-two-way-stream *standard-input* os)))
      (write-char #\a t)
      (fresh-line t)
      (finish-output t)))
  #.(concatenate 'string (string #\a) (string #\Newline)))

(deftest fresh-line.7
  (with-output-to-string
    (*standard-output*)
    (write-char #\a nil)
    (terpri nil))
  #.(concatenate 'string (string #\a) (string #\Newline)))

;;; Error tests

(deftest fresh-line.error.1
  (signals-error
   (with-output-to-string
     (s)
     (fresh-line s nil))
   program-error)
  t)
