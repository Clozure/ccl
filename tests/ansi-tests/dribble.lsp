;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May 15 12:56:29 2005
;;;; Contains: Tests of DRIBBLE

(in-package :cl-test)

;;; Error tests only -- cannot depend on using it in a program
;;; See the CLHS DRIBBLE and issue DRIBBLE-TECHNIQUE for an explanation

(deftest dribble.error.1
  (signals-error (dribble "dribble.out" nil) program-error)
  t)


;;; FIXME -- more error tests here
