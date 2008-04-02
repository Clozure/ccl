;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 29 17:26:57 2004
;;;; Contains: Tests associated with BASE-STRING

(in-package :cl-test)

(deftest base-string.1
  (subtypep* 'base-string 'string)
  t t)

(deftest base-string.2
  (subtypep* 'base-string 'vector)
  t t)

(deftest base-string.3
  (subtypep* 'base-string 'array)
  t t)

(deftest base-string.4
  (subtypep* 'base-string 'sequence)
  t t)

(deftest base-string.5
  :notes (:allow-nil-arrays :nil-vectors-are-strings)
  (subtypep* '(array nil (*)) 'base-string)
  nil t)

(deftest base-string.6
  :notes (:nil-vectors-are-strings)
  (subtypep* 'string 'base-string)
  nil t)
