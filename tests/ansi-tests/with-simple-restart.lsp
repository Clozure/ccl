;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar 23 04:36:52 2003
;;;; Contains: Tests for WITH-SIMPLE-RESTART

(in-package :cl-test)

(deftest with-simple-restart.1
  (with-simple-restart (foo ""))
  nil)

(deftest with-simple-restart.2
  (with-simple-restart (foo "") (values)))

(deftest with-simple-restart.3
  (with-simple-restart (foo "") (values 1 2 3 4 5 6 7 8 9 10))
  1 2 3 4 5 6 7 8 9 10)

(deftest with-simple-restart.4
  (block nil
    (tagbody
     (with-simple-restart
      (foo "")
      (go 10)
      10
      (return 'bad))
     10
     (return 'good)))
  good)

(deftest with-simple-restart.5
  (with-simple-restart
   (foo "zzz")
   (invoke-restart 'foo))
  nil t)

(deftest with-simple-restart.6
  (flet ((%f () (invoke-restart 'foo)))
    (with-simple-restart
     (foo "zzz")
     (%f)))
  nil t)

(deftest with-simple-restart.7
  (with-simple-restart
   (foo (formatter "xxx"))
   (invoke-restart 'foo))
  nil t)

(deftest with-simple-restart.8
  (with-simple-restart
   (nil "")
   (invoke-restart (first (compute-restarts))))
  nil t)
