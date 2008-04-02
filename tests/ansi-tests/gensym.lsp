;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jun 14 05:43:47 2003
;;;; Contains: Tests of GENSYM

(in-package :cl-test)

;;; Gensym returns unique symbols
(deftest gensym.1
  (equal (gensym) (gensym))
  nil)

;;; Gensym returns symbols with distinct print names
(deftest gensym.2
  (string= (symbol-name (gensym))
	   (symbol-name (gensym)))
  nil)

;;; Gensym uses the *gensym-counter* special variable,
;;; but does not increment it until after the symbol
;;; has been created.
(deftest gensym.3
  (let ((*gensym-counter* 1))
     (symbol-name (gensym)))
  #.(string '#:g1))

;;; Gensym uses the string argument instead of the default
(deftest gensym.4
  (let ((*gensym-counter* 1327))
    (symbol-name (gensym "FOO")))
  "FOO1327")

;;; The symbol returned by gensym should be unbound
(deftest gensym.5
  (boundp (gensym))
  nil)

;;; The symbol returned by gensym should have no function binding
(deftest gensym.6
  (fboundp (gensym))
  nil)

;;; The symbol returned by gensym should have no property list
(deftest gensym.7
  (symbol-plist (gensym))
  nil)

;;; The symbol returned by gensym should be uninterned
(deftest gensym.8
  (symbol-package (gensym))
  nil)

;;; *gensym-counter* is incremented by gensym
(deftest gensym.9
  (let ((*gensym-counter* 12345))
    (gensym)
    *gensym-counter*)
  12346)

;;; Gensym works when *gensym-counter* is Really Big
;;; (and does not increment the counter until after creating
;;; the symbol.)
(deftest gensym.10
  (let ((*gensym-counter* 1234567890123456789012345678901234567890))
    (symbol-name (gensym)))
  #.(string '#:g1234567890123456789012345678901234567890))

;;; gensym increments Really Big values of *gensym-counter*
(deftest gensym.11
  (let ((*gensym-counter* 12345678901234567890123456789012345678901234567890))
    (gensym)
    *gensym-counter*)
  12345678901234567890123456789012345678901234567891)

;;; Gensym uses an integer argument instead of the counter
(deftest gensym.12
  (let ((*gensym-counter* 10))
    (symbol-name (gensym 123)))
  #.(string '#:g123))

;;; When given an integer argument, gensym does not increment the
;;; *gensym-counter*
(deftest gensym.13
  (let ((*gensym-counter* 10))
    (gensym 123)
    *gensym-counter*)
  10)

;;; GENSYM counter is a non-negative integer
(deftest gensym-counter.1
  (and (integerp *gensym-counter*)
       (>= *gensym-counter* 0)
       t)
  t)

;;; Check response to erroneous arguments
;;; Note! NIL is not the same as no argument
;;; gensym should be implemented so that its only
;;; argument defaults to "G", with NIL causing an error.

(deftest gensym.error.1
  (check-type-error #'gensym #'(lambda (x) (typep x '(or string unsigned-byte))))
  nil)

(deftest gensym.error.7
  (signals-error (gensym 10 'foo) program-error)
  t)

(deftest gensym.error.8
  (signals-error (locally (gensym t) t) type-error)
  t)

(deftest gensym.error.9
  (signals-error (gensym "FOO" nil) program-error)
  t)

