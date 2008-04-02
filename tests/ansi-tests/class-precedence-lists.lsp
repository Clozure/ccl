;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jun  4 20:18:29 2003
;;;; Contains: Tests that builtin classes have the right CPLs

(in-package :cl-test)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless #| (fboundp 'class-precedence-list-foo) |# nil
    (report-and-ignore-errors
      (defgeneric class-precedence-list-foo (x)
        (:method-combination list)
        .
        #.(loop for s in *cl-types-that-are-classes-symbols*
                collect
                `(:method list ((x ,s))  ',s))))))

(defmacro def-cpl-test (objform expected-cpl &optional name)
  (let* ((ordered (loop for e = expected-cpl then (cdr e)
                        for x = (car e)
                        for y = (cadr e)
                        while y
                        always (subtypep x y))))
    `(deftest ,(or name
                   (intern (concatenate 'string
                                        (symbol-name (first expected-cpl))
                                        "-CPL")
                           :cl-test))
       (let* ((obj ,objform)
              (cpl (class-precedence-list-foo obj)))
         (or ,(if ordered
                  nil
                `(and (not (eql (class-of obj) (find-class ',(first expected-cpl))))
                      (progn (format t "~%Note: ~S not a direct instance of ~A~%"
                                     ',objform ',(first expected-cpl))
                             t)))               
             (and ,(if ordered t `(eql (first cpl) ',(first expected-cpl)))
                  (is-noncontiguous-sublist-of ',expected-cpl cpl))))
       t)))

;;; Condition types

(defmacro def-cond-cpl-test (expected-cpl)
  `(def-cpl-test (make-condition ',(first expected-cpl)) ,expected-cpl))

(def-cond-cpl-test (arithmetic-error error serious-condition condition t))
(def-cond-cpl-test (cell-error error serious-condition condition t))
(def-cond-cpl-test (condition t))
(def-cond-cpl-test (control-error error serious-condition condition t))
(def-cond-cpl-test (division-by-zero arithmetic-error error
                                     serious-condition condition t))
(def-cond-cpl-test (end-of-file stream-error error serious-condition condition t))
(def-cond-cpl-test (error serious-condition condition t))
(def-cond-cpl-test (file-error error serious-condition condition t))
(def-cond-cpl-test (floating-point-inexact arithmetic-error error
                                           serious-condition condition t))
(def-cond-cpl-test (floating-point-invalid-operation
                    arithmetic-error error serious-condition condition t))
(def-cond-cpl-test (floating-point-overflow arithmetic-error error
                                            serious-condition condition t))
(def-cond-cpl-test (floating-point-underflow arithmetic-error error
                                             serious-condition condition t))
(def-cond-cpl-test (package-error error serious-condition condition t))
(def-cond-cpl-test (parse-error error serious-condition condition t))
(def-cond-cpl-test (print-not-readable error serious-condition condition t))
(def-cond-cpl-test (program-error error serious-condition condition t))
(def-cond-cpl-test (reader-error parse-error stream-error
                                 error serious-condition condition t))
(def-cond-cpl-test (serious-condition condition t))
(def-cond-cpl-test (simple-condition condition t))
(def-cond-cpl-test (simple-error simple-condition error serious-condition
                                 condition t))
(def-cond-cpl-test (simple-type-error simple-condition type-error
                                      error serious-condition condition t))
(def-cond-cpl-test (simple-warning simple-condition warning condition t))
(def-cond-cpl-test (storage-condition serious-condition condition t))
(def-cond-cpl-test (stream-error error serious-condition condition t))
(def-cond-cpl-test (style-warning warning condition t))
(def-cond-cpl-test (type-error error serious-condition condition t))
(def-cond-cpl-test (unbound-slot cell-error error serious-condition condition t))
(def-cond-cpl-test (unbound-variable cell-error error serious-condition condition t))
(def-cond-cpl-test (undefined-function cell-error error serious-condition condition t))
(def-cond-cpl-test (warning condition t))

(def-cpl-test (make-array '(2 3 4)) (array t))
(def-cpl-test (make-array '(10) :element-type 'bit :adjustable t :fill-pointer 5)
  (bit-vector vector array sequence t))
(def-cpl-test (make-broadcast-stream) (broadcast-stream stream t))
(def-cpl-test (class-of 'symbol) (built-in-class class standard-object t))
(def-cpl-test #\a (character t) character-cpl.1)
(def-cpl-test #c(1.0 2.0) (complex number t) complex-cpl.1)
(def-cpl-test #c(1 2) (complex number t) complex-cpl.2)
(def-cpl-test #c(1/2 2/3) (complex number t) complex-cpl.3)
(def-cpl-test (make-concatenated-stream) (concatenated-stream stream t))
(def-cpl-test '(a b c) (cons list sequence t))
(def-cpl-test (let ((out (make-string-output-stream)))
                (make-echo-stream (make-string-input-stream "foo") out))
  (echo-stream stream t))

(def-cpl-test (open "class-precedence-lists.lsp" :direction :probe)
  (file-stream stream t))

(def-cpl-test 1.0s0 (float real number t) float-cpl.1)
(def-cpl-test 1.0f0 (float real number t) float-cpl.2)
(def-cpl-test 1.0d0 (float real number t) float-cpl.3)
(def-cpl-test 1.0l0 (float real number t) float-cpl.4)

(def-cpl-test #'car (function t))
;; (def-cpl-test #'make-instance (generic-function function t))

(def-cpl-test (make-hash-table) (hash-table t) hash-table-cpl.1)
(def-cpl-test (make-hash-table :test 'eq) (hash-table t) hash-table-cpl.2)
(def-cpl-test (make-hash-table :test 'equal) (hash-table t) hash-table-cpl.3)

(def-cpl-test 0 (integer rational real number t) integer-cpl.1)
(def-cpl-test (1+ most-positive-fixnum) (integer rational real number t) integer-cpl.2)
(def-cpl-test (1- most-negative-fixnum) (integer rational real number t) integer-cpl.3)

(def-cpl-test nil (list sequence t) list-cpl.1)
(def-cpl-test '(a b c) (list sequence t) list-cpl.2)

;;; Insert a test for LOGICAL-PATHNAME here
;;; (def-cpl-test ????? (logical-pathname pathname t))

;;; (def-cpl-test (find-method #'class-name nil (list (find-class 'class)))
;;;  (method t))

;;; Insert test for METHOD-COMBINATION here

(def-cpl-test nil (null symbol list sequence t))

(def-cpl-test (find-package "CL") (package t))
(def-cpl-test #p"foo" (pathname t))
(def-cpl-test *random-state* (random-state t))
(def-cpl-test 5/3 (ratio rational real number t))
(def-cpl-test *readtable* (readtable t))

(defclass cpl-example-class () ())

(def-cpl-test (find-class 'cpl-example-class)
  (standard-class class standard-object t))

(defgeneric cpl-example-gf (x y))

(def-cpl-test #'cpl-example-gf (standard-generic-function generic-function function t))

(def-cpl-test (eval '(defmethod cpl-example-gf ((x t) (y t)) (list y x)))
  (standard-method method standard-object t))

(def-cpl-test (make-array '(10) :element-type 'character :initial-element #\a
                          :fill-pointer t :adjustable t)
  (string vector array sequence t) string-cpl.1)

(def-cpl-test "abcd" (string vector array sequence t) string-cpl.2)

(def-cpl-test (make-string-input-stream "abcdef") (string-stream stream t))

(defstruct cpl-example-structure-class a b c)

;;; No test for STRUCTURE-OBJECT

(def-cpl-test 'a (symbol t))

(defparameter *cpl-input-stream* (make-string-input-stream "foofoofoofoo"))

(def-cpl-test (make-synonym-stream '*cpl-input-stream*) (synonym-stream stream t))

(defparameter *cpl-output-stream* (make-string-output-stream))

(def-cpl-test (make-two-way-stream *cpl-input-stream* *cpl-output-stream*)
  (two-way-stream stream t))

(def-cpl-test (make-array '(10) :fill-pointer t :adjustable t :initial-element '(a b c))
  (vector array sequence t))
