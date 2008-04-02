;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Dec  2 07:44:40 2002
;;;; Contains: Tests of *FEATURES*

(in-package :cl-test)

(deftest features.1
  (let ((f *features*))
    (or (not (member :draft-ansi-cl f))
	(not (intersection '(:draft-ansi-cl-2 :ansi-cl) f))))
  t)

(deftest features.2
  (let ((f *features*))
    (or (not (intersection '(:x3j13 :draft-ansi-cl :ansi-cl) f))
	(notnot (member :common-lisp f))))
  t)

(deftest features.3
  (not (member :cltl2 *features*))
  t)

(deftest features.4
  (notnot (every #'symbolp *features*))
  t)
