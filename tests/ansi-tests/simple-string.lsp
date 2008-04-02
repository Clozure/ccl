;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 29 17:27:46 2004
;;;; Contains: Tests associated with SIMPLE-STRING

(in-package :cl-test)

(deftest simple-string.1
  (subtypep* 'simple-string 'string)
  t t)

(deftest simple-string.2
  (subtypep* 'simple-string 'vector)
  t t)

(deftest simple-string.3
  (subtypep* 'simple-string 'simple-array)
  t t)

(deftest simple-string.4
  (subtypep* 'simple-string 'array)
  t t)

(deftest simple-string.5
  (subtypep* 'simple-string 'sequence)
  t t)

(deftest simple-string.6
  (subtypep* 'simple-string '(simple-array * (*)))
  t t)

(deftest simple-string.7
  (subtypep* 'simple-string '(simple-array * 1))
  t t)

(deftest simple-string.8
  :notes (:nil-vectors-are-strings)
  (subtypep* 'simple-string '(simple-array character (*)))
  nil t)

(deftest simple-string.9
  :notes (:nil-vectors-are-strings)
  (subtypep* 'simple-string '(simple-array base-char (*)))
  nil t)

(deftest simple-string.10
  :notes (:nil-vectors-are-strings)
  (subtypep* 'simple-string 'simple-base-string)
  nil t)

(deftest simple-string.11 
  :notes (:nil-vectors-are-strings)
  (subtypep* '(simple-array nil (*)) 'simple-string)
  t t)

(deftest simple-string.12
  :notes (:nil-vectors-are-strings)
  (typep* (make-array '(0) :element-type nil) 'simple-string)
  t)

(deftest simple-string.13
  :notes (:nil-vectors-are-strings)
  (typep* (make-array '(12) :element-type nil) 'simple-string)
  t)

(deftest simple-string.14
  (typep* "abc" '(simple-string))
  t)

(deftest simple-string.15
  (typep* "abc" '(simple-string *))
  t)

(deftest simple-string.16
  (typep* "abc" '(simple-string 3))
  t)

(deftest simple-string.17
  (typep* "abc" '(simple-string 2))
  nil)

(deftest simple-string.18
  (typep* "abc" '(simple-string 4))
  nil)

