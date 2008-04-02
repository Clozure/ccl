;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 18 20:53:59 2004
;;;; Contains: Tests of READ-LINE

(in-package :cl-test)

(deftest read-line.1
  (with-input-from-string
   (*standard-input* " abcd ")
   (let ((vals (multiple-value-list (read-line))))
     (assert (= (length vals) 2))
     (values (first vals) (notnot (second vals)))))
  " abcd " t)

(deftest read-line.2
  (with-input-from-string
   (*standard-input* (string #\Newline))
   (read-line))
  "" nil)

(deftest read-line.3
  (with-input-from-string
   (s (concatenate 'string "abc" (string #\Newline)))
   (read-line s))
  "abc" nil)

(deftest read-line.4
  (with-input-from-string
   (s "")
   (let ((vals (multiple-value-list (read-line s nil))))
     (assert (= (length vals) 2))
     (values (first vals) (notnot (second vals)))))
  nil t)

(deftest read-line.5
  (with-input-from-string
   (s "")
   (let ((vals (multiple-value-list (read-line s nil 'foo))))
     (assert (= (length vals) 2))
     (values (first vals) (notnot (second vals)))))
  foo t)

(deftest read-line.6
  (with-input-from-string
   (s " abcd ")
   (let ((vals (multiple-value-list (read-line s t nil t))))
     (assert (= (length vals) 2))
     (values (first vals) (notnot (second vals)))))
  " abcd " t)

(deftest read-line.7
  (with-input-from-string
   (is "abc")
   (let ((*terminal-io* (make-two-way-stream is *standard-output*)))
     (let ((vals (multiple-value-list (read-line t))))
       (assert (= (length vals) 2))
       (assert (second vals))
       (first vals))))
  "abc")

(deftest read-line.8
  (with-input-from-string
   (*standard-input* "abc")
   (let ((vals (multiple-value-list (read-line nil))))
     (assert (= (length vals) 2))
     (assert (second vals))
     (first vals)))
  "abc")

;;; Error tests

(deftest read-line.error.1
  (signals-error
   (with-input-from-string
    (s (concatenate 'string "abc" (string #\Newline)))
    (read-line s t nil nil nil))
   program-error)
  t)

(deftest read-line.error.2
  (signals-error-always
   (with-input-from-string
    (s "")
    (read-line s))
   end-of-file)
  t t)

(deftest read-line.error.3
  (signals-error-always
   (with-input-from-string
    (*standard-input* "")
    (read-line))
   end-of-file)
  t t)

(deftest read-line.error.4
  (signals-error-always
   (with-input-from-string
    (s "")
    (read-line s t))
   end-of-file)
  t t)

