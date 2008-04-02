;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon May 30 07:49:10 2005
;;;; Contains: Tests for non-ansi exceptional situations in Section 4 of CLHS

(in-package :ba-test)

(compile-and-load "ba-aux.lsp")

;;; COERCE

(def-all-error-test coerce.1 'listp '(coerce t x))

;;; DEFTYPE

(def-error-test deftype.1 (deftype))
(def-error-test deftype.2 (deftype #.(gensym)))
(def-error-test deftype.3 (deftype . foo))
(def-all-error-test deftype.4 'symbolp '(deftype x () t))

;;; SUBTYPEP

(def-all-error-test subtypep.1 'type-specifier-p '(subtypep x t))
(def-all-error-test subtypep.2 'type-specifier-p '(subtypep nil x))

;;; TYPEP

(def-all-error-test typep.1 'type-specifier-p '(typep nil x))

;;; SATISFIES

(def-error-test satisfies.1 (typep nil '(satifies)))
(def-error-test satisfies.2 (typep nil '(satifies null nil)))
(def-all-error-test satisfies.3 'symbolp '(typep nil (satisfies x)))

;;; MEMBER (type specifier)

(def-error-test member.type.1 (typep nil 'member))
(def-error-test member.type.2 (typep nil '(member . foo)))
(def-error-test member.type.3 (typep nil '(member bar . foo)))

;;; NOT (type specifier)

(def-error-test not.type.1 (typep nil 'not))
(def-error-test not.type.2 (typep nil '(not)))
(def-error-test not.type.3 (typep nil '(not *)))
(def-error-test not.type.4 (typep nil '(not nil nil)))
(def-all-error-test not.type.5 'type-specifier-p '(typep nil '(not x)))
(def-error-test not.type.6 (typep nil '(not . foo)))

;;; AND (type specifier)

(def-error-test and.type.1 (typep nil 'and))
(def-error-test and.type.2 (typep nil '(and *)))
(def-error-test and.type.3 (typep nil '(and t * t)))
(def-error-test and.type.4 (typep nil '(and . foo)))
(def-all-error-test and.type.5 'type-specifier-p '(typep t '(and t t x t)))

;;; OR (type specifier)

(def-error-test or.type.1 (typep nil 'or))
(def-error-test or.type.2 (typep nil '(or *)))
(def-error-test or.type.3 (typep nil '(or nil * nil)))
(def-error-test or.type.4 (typep nil '(or . foo)))
(def-all-error-test or.type.5 'type-specifier-p '(typep t '(or nil x nil)))

;;; VALUES (type specifier)

(def-error-test values.type.1 (typep nil 'values))
(def-error-test values.type.2 (the values (values)))
(def-error-test values.type.3 (the (values . foo) (values)))
(def-error-test values.type.4 (the (values *) t))
(def-all-error-test values.type.5 'type-specifier-p '(the (values x) t))

;;; EQL (type specifier)

(def-error-test eql.type.1 (typep nil 'eql))
(def-error-test eql.type.2 (typep nil '(eql)))
(def-error-test eql.type.3 (typep nil '(eql nil nil)))
(def-error-test eql.type.4 (typep nil '(eql . foo)))

;;; TYPE-ERROR-DATUM

(def-all-error-test type-error-datum.1
  (typef 'type-error) '(type-error-datum x))

;;; TYPE-ERROR-EXPECTED-TYPE

(def-all-error-test type-error-expected-type.1
  (typef 'type-error) '(type-error-expected-type x))

;;; FUNCTION (type specifier)

(def-error-test function.type.1
  (locally (declare (type (function . foo) f)) nil))
(def-error-test function.type.2
  (locally (declare (type (function () . foo) f)) nil))
(def-error-test function.type.3
  (locally (declare (type (function (t . t) t) f)) nil))
(def-error-test function.type.4
  (locally (declare (type (function (&optional . foo) t) f)) nil))
(def-error-test function.type.5
  (locally (declare (type (function (&rest . foo) t) f)) nil))
(def-error-test function.type.6
  (locally (declare (type (function (&key . foo) t) f)) nil))
(def-error-test function.type.7
  (locally (declare (type (function (&key :foo) t) f)) nil))
(def-error-test function.type.8
  (locally (declare (type (function (&key (:foo . bar)) t) f)) nil))
(def-error-test function.type.9
  (locally (declare (type (function (&key (:foo t . bar)) t) f)) nil))
(def-error-test function.type.10
  (locally (declare (type (function (&key (:foo t nil)) t) f)) nil))
