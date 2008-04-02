;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan  1 11:17:21 2005
;;;; Contains: Tests of READ-DELIMITED-LIST

(in-package :cl-test)

(deftest read-delimited-list.1
  (with-input-from-string
   (*standard-input* "1 2 3)")
   (read-delimited-list #\)))
  (1 2 3))

(deftest read-delimited-list.2
  (with-input-from-string
   (*standard-input* "1 2 3 ]")
   (read-delimited-list #\] nil))
  (1 2 3))

(deftest read-delimited-list.3
  (with-input-from-string
   (is "1 2 3)")
   (with-open-stream
     (os (make-broadcast-stream))
     (with-open-stream
      (*terminal-io* (make-two-way-stream is os))
      (read-delimited-list #\) t))))
  (1 2 3))

(deftest read-delimited-list.4
  (with-input-from-string
   (is "1 2 3)X")
   (values
    (read-delimited-list #\) is)
    (notnot (eql (read-char is) #\X))))
  (1 2 3) t)

(deftest read-delimited-list.5
  (with-input-from-string
   (is "1 2 3) X")
   (values
    (read-delimited-list #\) is nil)
    (notnot (eql (read-char is) #\Space))))
  (1 2 3) t)

(deftest read-delimited-list.6
  (with-input-from-string
   (is (concatenate 'string "1 2 3" (string #\Newline) "]"))
   (read-delimited-list #\] is))
  (1 2 3))

;;; Tests with RECURSIVE-P set to true must be done inside a reader macro function

;;; Error tests

(deftest read-delimited-list.error.1
  (signals-error (read-delimited-list) program-error)
  t)

(deftest read-delimited-list.error.2
  (signals-error
   (with-input-from-string
    (is "1 2 3)")
    (read-delimited-list #\) is nil nil))
   program-error)
  t)
