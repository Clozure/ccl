;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue May 31 08:08:49 2005
;;;; Contains: Tests of non-ANSI exceptional situations from CLHS section 5, part 2

(in-package :ba-test)

(compile-and-load "ba-aux.lsp")

;;; FUNCALL

(def-all-error-test funcall.1 'function-designator-p '(funcall x))
(def-error-test funcall.2 (funcall cons 1 . 2))

;;; FUNCTION

(def-error-test function.1 (function))
(def-error-test function.2 (function . cons))
(def-error-test function.3 (function cons . foo))
(def-error-test function.4 (function cons nil))
(def-all-error-test function.5 'function-name-p '(function x))
(def-all-error-test function.6
  (constantly nil) #'(lambda (x) `(function ,x))
  :vals cl-test::*cl-macro-symbols*)
(def-all-error-test function.7
  (constantly nil) #'(lambda (x) `(function ,x))
  :vals cl-test::*cl-special-operator-symbols*)
(def-error-test function.8 (macrolet ((%m () nil)) #'%m))

;;; FUNCTION-LAMBDA-EXPRESSION

(def-all-error-test function-lambda-expression.1
  'functionp '(function-lambda-expression x))

;;; DEFCONSTANT

(def-error-test defconstant.1 (defconstant))
(def-error-test defconstant.2 (defconstant . foo))
(def-error-test defconstant.3 (defconstant #.(gensym)))
(def-error-test defconstant.4 (defconstant #.(gensym) . foo))
(def-error-test defconstant.5 (defconstant #.(gensym) nil . foo))
(def-error-test defconstant.6 (defconstant #.(gensym) nil "foo" . bar))

(def-all-error-test defconstant.7 'symbolp
  #'(lambda (x) `(defconstant ,x nil)))

(def-all-error-test defconstant.8 'stringp
  #'(lambda (x) `(defconstant ,(gensym) nil ,x)))

;;; DEFPARAMETER

(def-error-test defparameter.1 (defparameter))
(def-error-test defparameter.2 (defparameter . foo))
(def-error-test defparameter.3 (defparameter #.(gensym)))
(def-error-test defparameter.4 (defparameter #.(gensym) . foo))
(def-error-test defparameter.5 (defparameter #.(gensym) nil . foo))
(def-error-test defparameter.6 (defparameter #.(gensym) nil "foo" . bar))

(def-all-error-test defparameter.7 'symbolp
  #'(lambda (x) `(defparameter ,x nil)))

(def-all-error-test defparameter.8 'stringp
  #'(lambda (x) `(defparameter ,(gensym) nil ,x)))

;;; DEFVAR

(def-error-test defvar.1 (defvar))
(def-error-test defvar.2 (defvar . foo))
(def-error-test defvar.4 (defvar #.(gensym) . foo))
(def-error-test defvar.5 (defvar #.(gensym) nil . foo))
(def-error-test defvar.6 (defvar #.(gensym) nil "foo" . bar))

(def-all-error-test defvar.7 'symbolp
  #'(lambda (x) `(defvar ,x nil)))

(def-all-error-test defvar.8 'stringp
  #'(lambda (x) `(defvar ,(gensym) nil ,x)))

;;; DESTRUCTURING-BIND

(def-error-test destructuring-bind.1 (destructuring-bind))
(def-error-test destructuring-bind.2 (destructuring-bind x))
(def-all-error-test destructuring-bind.3
  (typef '(or symbol cons))
  #'(lambda (x) `(destructuring-bind ,x nil)))
(def-error-test destructuring-bind.4 (destructuring-bind (x) '(a) nil (declare) x))

;;; LET

(def-error-test let.1 (let))
(def-error-test let.2 (let . x))
(def-all-error-test let.3 'listp #'(lambda (x) `(let ,x nil)))
(def-error-test let.4 (let () . x))
(def-error-test let.5 (let (x . 1) nil))
(def-error-test let.6 (let ((x) . y) nil))
(def-error-test let.7 (let ((x 1 . 2)) nil))
(def-error-test let.8 (let ((x 1 2)) nil))
(def-error-test let.9 (let ((x 1) (x 2)) x))
(def-error-test let.10 (let ((t 1)) t))
(def-all-error-test let.11 (typef '(or cons symbol))
  #'(lambda (x) `(let (,x) nil)))
(def-all-error-test let.12 'symbolp
  #'(lambda (x) `(let ((,x)) nil)))

(def-error-test let.13 (let ((x 0) (x 1)) x))

;;; LET*

(def-error-test let*.1 (let*))
(def-error-test let*.2 (let* . x))
(def-all-error-test let*.3 'listp #'(lambda (x) `(let* ,x nil)))
(def-error-test let*.4 (let* () . x))
(def-error-test let*.5 (let* (x . 1) nil))
(def-error-test let*.6 (let* ((x) . y) nil))
(def-error-test let*.7 (let* ((x 1 . 2)) nil))
(def-error-test let*.8 (let* ((x 1 2)) nil))
(def-error-test let*.10 (let* ((t 1)) t))
(def-all-error-test let*.11 (typef '(or cons symbol))
  #'(lambda (x) `(let* (,x) nil)))
(def-all-error-test let*.12 'symbolp
  #'(lambda (x) `(let* ((,x)) nil)))

;;; PROGV

(def-error-test progv.1 (progv))
(def-error-test progv.2 (progv '(a)))
(def-all-error-test progv.3 'listp '(progv x nil nil))
(def-all-error-test progv.4 'listp '(progv '(a) x nil))

;;; SETQ

(def-error-test setq.1 (setq . x))
(def-error-test setq.2 (let ((x t)) (setq x)))
(def-error-test setq.3 (let ((x t)) (setq x . foo)))
(def-error-test setq.4 (let ((x 1)) (setq x nil . foo)))
(def-error-test setq.5 (let ((x 1) (y 2)) (setq x nil y)))
(def-all-error-test setq.6 'symbolp #'(lambda (x) `(setq ,x nil)))
(def-error-test setq.7
  (let ((sym (gensym)))
    (eval `(defconstant ,sym nil))
    (eval `(setq ,sym t))
    (eval sym)))

;;; PSETQ

(def-error-test psetq.1 (psetq . x))
(def-error-test psetq.2 (let ((x t)) (psetq x)))
(def-error-test psetq.3 (let ((x t)) (psetq x . foo)))
(def-error-test psetq.4 (let ((x 1)) (psetq x nil . foo)))
(def-error-test psetq.5 (let ((x 1) (y 2)) (psetq x nil y)))
(def-all-error-test psetq.6 'symbolp #'(lambda (x) `(psetq ,x nil)))
(def-error-test psetq.7
  (let ((sym (gensym)))
    (eval `(defconstant ,sym nil))
    (eval `(psetq ,sym t))
    (eval sym)))
;;; I suggest it would be useful for PSETQ to detect when it is
;;; being asked to assign to the same variable twice, since this
;;; isn't well defined.
(def-error-test psetq.8 (let ((x 0)) (psetq x 1 x 2) x))

;;; BLOCK

(def-error-test block.1 (block))
(def-error-test block.2 (block . foo))
(def-all-error-test block.3 'symbolp #'(lambda (x) `(block ,x)))
(def-error-test block.4 (block nil . foo))

;;; CATCH

(def-error-test catch.1 (catch))
(def-error-test catch.2 (catch . foo))
(def-error-test catch.3 (catch 'tag . foo))
(def-all-error-test catch.4 (constantly nil) '(catch x (throw x nil))
  :vals *cl-symbols*)


;;; GO

(def-error-test go.1 (go))
(def-error-test go.2 (go . foo))
(def-all-error-test go.3 (typef '(or symbol integer))
  #'(lambda (x) `(go ,x)))
(def-error-test go.4 (tagbody (go done . foo) done))
(def-error-test go.5 (tagbody (go done foo) done))

;;; RETURN-FROM

(def-error-test return-from.1 (return-from))
(def-error-test return-from.2 (return-from . foo))
(def-error-test return-from.3 (return-from foo))
(def-error-test return-from.4 (block foo (return-from foo . t)))
(def-error-test return-from.5 (block foo (return-from foo nil . 2)))
(def-error-test return-from.6 (block foo (return-from foo nil 3)))

;;; RETURN

(def-error-test return.1 (return . x))
(def-error-test return.2 (return nil . x))

;;; TAGBODY

(def-error-test tagbody.1 (tagbody . x))
(def-all-error-test tagbody.2 (typef '(or symbol integer cons))
  #'(lambda (x) `(tagbody ,x)))

;;; THROW

(def-error-test throw.1 (throw))
(def-error-test throw.2 (throw . x))
(def-error-test throw.3 (catch 'a (throw 'a)))
(def-error-test throw.4 (catch 'a (throw 'a . x)))
(def-error-test throw.5 (catch 'a (throw 'a 1 . x)))
(def-error-test throw.6 (catch 'a (throw 'a 1 'x)))

;;; UNWIND-PROTECT

(def-error-test unwind-protect.1 (unwind-protect))
(def-error-test unwind-protect.2 (unwind-protect . x))
(def-error-test unwind-protect.3 (unwind-protect nil . x))

;;; NOT

(def-error-test not.1 (not . x))
(def-error-test not.2 (not nil . x))


;;; EQ

(def-error-test eq.1 (eq . 1))
(def-error-test eq.2 (eq 'x . 2))
(def-error-test eq.3 (eq :foo 2 . 17))

;;; EQL

(def-error-test eql.1 (eql . 1))
(def-error-test eql.2 (eql 'x . 2))
(def-error-test eql.3 (eql :foo 2 . 17))

;;; EQUAL

(def-error-test equal.1 (equal . 1))
(def-error-test equal.2 (equal 'x . 2))
(def-error-test equal.3 (equal :foo 2 . 17))

;;; EQUALP

(def-error-test equalp.1 (equalp . 1))
(def-error-test equalp.2 (equalp 'x . 2))
(def-error-test equalp.3 (equalp :foo 2 . 17))

;;; IDENTITY

(def-error-test identity.1 (identity . 0))
(def-error-test identity.2 (identity 0 . "foo"))

;;; COMPLEMENT

(def-error-test complement.1 (complement . 1.2))
(def-error-test complement.2 (complement #'plusp . #(1 2)))
(def-error-test complement.3 (complement #'zerop #*110101 . #c(1 2)))
(def-all-error-test complement.4 'functionp '(complement x))

;;; CONSTANTLY

(def-error-test constantly.1 (constantly . 1/2))
(def-error-test constantly.2 (constantly :foo . 1/2))

;;; EVERY

(def-error-test every.1 (every . :foo))
(def-error-test every.2 (every 'null . (list)))
(def-error-test every.3 (every (gensym) '(a b c d)))

;;; SOME

(def-error-test some.1 (some . :foo))
(def-error-test some.2 (some 'null . (list)))
(def-error-test some.3 (some (gensym) '(a b c d)))

;;; NOTEVERY

(def-error-test notevery.1 (notevery . :foo))
(def-error-test notevery.2 (notevery 'null . (list)))
(def-error-test notevery.3 (notevery (gensym) '(a b c d)))

;;; NOTANY

(def-error-test notany.1 (notany . :foo))
(def-error-test notany.2 (notany 'null . (list)))
(def-error-test notany.3 (notany (gensym) '(a b c d)))

;;; AND

(def-error-test and.1 (and . #.(make-hash-table)))
(def-error-test and.2 (and t . :foo))

;;; COND

(def-error-test cond.1 (cond . 1))
(def-error-test cond.2 (cond (t . 2)))
(def-error-test cond.3 (cond nil))
(def-error-test cond.4 (cond (nil) . "foo"))

;;; IF

(def-error-test if.1 (if))
(def-error-test if.2 (if . t))
(def-error-test if.3 (if t))
(def-error-test if.4 (if nil))
(def-error-test if.5 (if t . 1))
(def-error-test if.6 (if nil . 2))
(def-error-test if.7 (if t 1 . 2))
(def-error-test if.8 (if nil #\x . #\y))
(def-error-test if.9 (if t 1 2 . 3))
(def-error-test if.10 (if nil #\x #\y . 1.23d4))
(def-error-test if.11 (if t 1 2 3))
(def-error-test if.12 (if nil #\x #\y nil nil nil))

;;; OR

(def-error-test or.1 (or . :foo))
(def-error-test or.2 (or nil . :bar))

;;; WHEN

(def-error-test when.1 (when))
(def-error-test when.2 (when . #\$))
(def-error-test when.3 (when t . x))
(def-error-test when.4 (when t nil . "A"))

;;; UNLESS

(def-error-test unless.1 (unless))
(def-error-test unless.2 (unless . #*1011))
(def-error-test unless.3 (unless nil . t))
(def-error-test unless.4 (unless nil nil . #()))
