;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 29 17:30:40 2004
;;;; Contains: Tests associated with SIMPLE-BASE-STRING

(in-package :cl-test)

(deftest simple-base-string.1
  (subtypep* 'simple-base-string 'string)
  t t)

(deftest simple-base-string.2
  (subtypep* 'simple-base-string 'vector)
  t t)

(deftest simple-base-string.3
  (subtypep* 'simple-base-string 'simple-array)
  t t)

(deftest simple-base-string.4
  (subtypep* 'simple-base-string 'array)
  t t)

(deftest simple-base-string.5
  (subtypep* 'simple-base-string 'sequence)
  t t)

(deftest simple-base-string.6
  (subtypep* 'simple-base-string 'base-string)
  t t)

(deftest simple-base-string.7
  (subtypep* 'simple-base-string 'simple-string)
  t t)

(deftest simple-base-string.8
  (subtypep* 'simple-base-string 'simple-vector)
  nil t)

(deftest simple-base-string.9
  :notes (:allow-nil-arrays :nil-vectors-are-strings)
  (subtypep* '(simple-array nil (*)) 'simple-base-string)
  nil t)

(deftest simple-base-string.10
  :notes (:allow-nil-arrays :nil-vectors-are-strings)
  (typep* (make-array '(0) :element-type nil) 'simple-base-string)
  nil)

(deftest simple-base-string.11
  :notes (:allow-nil-arrays :nil-vectors-are-strings)
  (typep* (make-array '(12) :element-type nil) 'simple-base-string)
  nil)
