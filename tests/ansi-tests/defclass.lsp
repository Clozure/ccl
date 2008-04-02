;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Mar 24 03:39:54 2003
;;;; Contains: Tests of DEFCLASS

(in-package :cl-test)

(defclass-with-tests defclass-1 nil nil)
(defclass-with-tests defclass-2 nil (slot1 slot2 slot3))

(defclass-with-tests defclass-3 (defclass-1) nil)
(defclass-with-tests defclass-4 (defclass-1 defclass-2) (slot1 slot4))

;;; At end, generate slot tests

(generate-slot-tests) ;; a macro


