;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:29:48 1998
;;;; Contains: Testing of CL Features related to "CONS", part 1

(in-package :cl-test)

(declaim (optimize (safety 3)))

(compile-and-load "cons-aux.lsp")

;;
;; Test the subtype relationships between null, list, cons and atom
;;
(deftest subtypep-null-list
  (subtypep* 'null 'list)
  t t)

(deftest subtypep-cons-list
  (subtypep* 'cons 'list)
  t t)

(deftest subtypep-null-cons
  (subtypep* 'null 'cons)
  nil t)

(deftest subtypep-cons-null
  (subtypep* 'cons 'null)
  nil t)

(deftest subtypep-null-atom
  (subtypep* 'null 'atom)
  t t)

(deftest subtypep-cons-atom
  (subtypep* 'cons 'atom)
  nil t)

(deftest subtypep-atom-cons
  (subtypep* 'atom 'cons)
  nil t)

(deftest subtypep-atom-list
  (subtypep* 'atom 'list)
  nil t)

(deftest subtypep-list-atom
  (subtypep* 'list 'atom)
  nil t)

;;
;; Check that the elements of *universe* in type null
;; are those for which the null predice is true.
;;
(deftest null-null-universe
  (check-type-predicate 'null 'null)
  nil)

(defvar *cons-fns*
  (list 'cons 'consp 'atom 'rplaca 'rplacd
	'car 'cdr 'caar 'cadr 'cdar 'cddr
	'caaar 'caadr 'cadar 'caddr
	'cdaar 'cdadr 'cddar 'cdddr
	'caaaar 'caaadr 'caadar 'caaddr
	'cadaar 'cadadr 'caddar 'cadddr
	'cdaaar 'cdaadr 'cdadar 'cdaddr
	'cddaar 'cddadr 'cdddar 'cddddr
	'copy-tree 'sublis 'nsublis
	'subst 'subst-if 'subst-if-not
	'nsubst 'nsubst-if 'nsubst-if-not
	'tree-equal
	'copy-list
	'list
	'list*
	'list-length
	'listp
	'make-list
	'first 'second 'third 'fourth
	'fifth 'sixth 'seventh 'eighth 'ninth 'tenth
	'nth
	'endp
	'null
	'nconc
	'append
	'revappend 'nreconc
	'butlast 'nbutlast
	'last 'ldiff 'tailp
	'nthcdr 'rest
	'member 'member-if 'member-if-not
	'mapc 'mapcar 'mapcan 'mapl 'maplist 'mapcon
	'acons
	'assoc 'assoc-if 'assoc-if-not
	'copy-alist
	'pairlis
	'rassoc 'rassoc-if 'rassoc-if-not
	'get-properties
	'getf
	'intersection
	'nintersection
	'adjoin
	'set-difference 'nset-difference
	'set-exclusive-or 'nset-exclusive-or
	'subsetp
	'union 'nunion
	))

;; All the cons functions have a function binding

(deftest function-bound-cons-fns
  (loop
   for x in *cons-fns* count
   (when (or (not (fboundp x))
	     (not (functionp (symbol-function x))))
     (format t "~%~S not bound to a function" x)
     t))
  0)

;; All the cons-related macros have a macro binding
(deftest macro-bound-cons-macros
  (notnot-mv (every #'macro-function
		    (list 'push 'pop 'pushnew 'remf)))
  t)

;; None of the cons-related functions have macro bindings
(deftest no-cons-fns-are-macros
  (some #'macro-function *cons-fns*)
  nil)
