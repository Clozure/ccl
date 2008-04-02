;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 12 12:30:46 2002
;;;; Contains: Tests of BLOCK

(in-package :cl-test)

(deftest block.1
  (block foo
    (return-from foo 1))
  1)

(deftest block.2
  (block nil
    (block foo
      (return 'good))
    'bad)
  good)

(deftest block.3
  (block done
    (flet ((%f (x) (return-from done x)))
      (%f 'good))
    'bad)
  good)

(deftest block.4
  (block foo
    (block foo
      (return-from foo 'bad))
    'good)
  good)

(deftest block.5
  (block done
    (flet ((%f (x) (return-from done x)))
      (mapcar #'%f '(good bad bad)))
    'bad)
  good)

(deftest block.6
  (block b1
    (return-from b1 (values))
    1))

(deftest block.7
  (block b1
    (return-from b1 (values 1 2 3 4))
    1)
  1 2 3 4)

(deftest block.8
  (block foo)
  nil)

(deftest block.9
  (block foo (values 'a 'b) (values 'c 'd))
  c d)

(deftest block.10
  (block done
    (flet ((%f (x) (return-from done x)))
      (block done (mapcar #'%f '(good bad bad))))
    'bad)
  good)

;;; Block has no tagbody
(deftest block.11
  (block done
    (tagbody
     (block nil
       (go 10)
       10
       (return-from done 'bad))
     10
     (return-from done 'good)))
  good)

;;; Macros are expanded in the appropriate environment

(deftest block.12
  (macrolet ((%m (z) z))
	    (block foo (expand-in-current-env (%m :good))))
  :good)

#|
(deftest return.error.1
  (signals-error
   (block nil
     (return 'a 'b))
   program-error)
  t)
|#
