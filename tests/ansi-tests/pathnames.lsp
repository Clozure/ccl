;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov 29 04:21:53 2003
;;;; Contains: Various tests on pathnames

(in-package :cl-test)

(deftest pathnames-print-and-read-properly
  (with-standard-io-syntax
   (loop
    for p1 in *pathnames*
    for s = (handler-case (write-to-string p1 :readably t)
			  (print-not-readable () :unreadable-error))
    unless (eql s :unreadable-error)
    append
    (let ((p2 (read-from-string s)))
     (unless (equal p1 p2)
       (list (list p1 s p2))))))
  nil)
