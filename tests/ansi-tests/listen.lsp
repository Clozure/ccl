;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 27 21:16:39 2004
;;;; Contains: Tests of LISTEN

(in-package :cl-test)

(deftest listen.1
  (with-input-from-string (s "") (listen s))
  nil)

(deftest listen.2
  (with-input-from-string (s "x") (notnot-mv (listen s)))
  t)

(deftest listen.3
  (with-input-from-string (*standard-input* "") (listen))
  nil)

(deftest listen.4
  (with-input-from-string (*standard-input* "A") (notnot-mv (listen)))
  t)

;;; (deftest listen.5
;;;  (when (interactive-stream-p *standard-input*)
;;;    (clear-input) (listen))
;;;  nil)

(deftest listen.6
  (with-input-from-string
   (s "x")
   (values
    (read-char s)
    (listen s)
    (unread-char #\x s)
    (notnot (listen s))
    (read-char s)))
  #\x nil nil t #\x)

(deftest listen.7
  (with-open-file
   (s "listen.lsp")
   (values
    (notnot (listen s))
    (handler-case
     (locally (declare (optimize safety))
	      (loop (read-char s)))
     (end-of-file () (listen s)))))
  t nil)

(deftest listen.8
  (with-input-from-string
   (is "abc")
   (let ((*terminal-io* (make-two-way-stream is (make-broadcast-stream))))
     (notnot-mv (listen t))))
  t)

(deftest listen.9
  (with-input-from-string
   (*standard-input* "345")
   (notnot-mv (listen nil)))
  t)

;;; Error tests

(deftest listen.error.1
  :notes (:assume-no-simple-streams)
  (signals-error (listen *standard-input* nil) program-error)
  t)

(deftest listen.error.2
  (signals-error (listen *standard-input* nil nil) program-error)
  t)
