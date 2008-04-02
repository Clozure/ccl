;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 21:36:33 2003
;;;; Contains: Tests for MAKE-HASH-TABLE

(in-package :cl-test)

;; (eval-when (:load-toplevel :compile-toplevel :execute)
;;  (compile-and-load "hash-table-aux.lsp"))

(deftest make-hash-table.1
  (let ((ht (make-hash-table)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.2
  (let ((ht (make-hash-table :size 0)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.3
  (let ((ht (make-hash-table :size 100)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.4
  (let ((ht (make-hash-table :test #'eq)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.5
  (let ((ht (make-hash-table :test 'eq)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.6
  (let ((ht (make-hash-table :test #'eql)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.7
  (let ((ht (make-hash-table :test 'eql)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.8
  (let ((ht (make-hash-table :test #'equal)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.9
  (let ((ht (make-hash-table :test 'equal)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.10
  (let ((ht (make-hash-table :test #'equalp)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.11
  (let ((ht (make-hash-table :test 'equalp)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.12
  (let ((ht (make-hash-table :rehash-size 1)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.13
  (let ((ht (make-hash-table :rehash-size 1000)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.14
  (let ((ht (make-hash-table :rehash-size (+ 1.0f0 single-float-epsilon))))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.15
  (let ((ht (make-hash-table :rehash-size 2.0)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.16
  (let ((ht (make-hash-table :rehash-threshold 0)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.17
  (let ((ht (make-hash-table :rehash-threshold 0.0s0)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.18
  (let ((ht (make-hash-table :rehash-threshold 0.0f0)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.19
  (let ((ht (make-hash-table :rehash-threshold 0.0d0)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.20
  (let ((ht (make-hash-table :rehash-threshold 0.0l0)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.21
  (let ((ht (make-hash-table :rehash-threshold 1/2)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.22
  (let ((ht (make-hash-table :rehash-threshold 0.1s0)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.23
  (let ((ht (make-hash-table :rehash-threshold 0.2f0)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.24
  (let ((ht (make-hash-table :rehash-threshold 0.8d0)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.25
  (let ((ht (make-hash-table :rehash-threshold 0.99f0)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.26
  (let ((ht (make-hash-table :rehash-threshold least-positive-short-float)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.27
  (let ((ht (make-hash-table :rehash-threshold least-positive-single-float)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.28
  (let ((ht (make-hash-table :rehash-threshold least-positive-double-float)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.29
  (let ((ht (make-hash-table :rehash-threshold least-positive-long-float)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)














     
 