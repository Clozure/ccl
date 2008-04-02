;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jun 14 07:00:58 2005
;;;; Contains: Tests of non-ANSI exceptions sutation from CLHS section 5, part 3
(in-package :ba-test)

(compile-and-load "ba-aux.lsp")

;;; CASE

(def-error-test case.1 (case . 1))
(def-error-test case.2 (case nil . 1))
(def-error-test case.3 (case nil (nil . 1)))
(def-error-test case.4 (case 'x nil))
(def-error-test case.5 (case 'x ((nil . x) 1)))
(def-error-test case.6 (case))

;;; CCASE

(def-error-test ccase.1 (ccase . 1))
(def-error-test ccase.2 (let ((x nil)) (ccase x . 1)))
(def-error-test ccase.3 (let ((x nil)) (ccase x (nil . 1))))
(def-error-test ccase.4 (let ((x 'x)) (ccase x nil)))
(def-error-test ccase.5 (let ((x 'x)) (ccase x ((nil . x) 1))))
(def-error-test ccase.6 (ccase 1 (1 nil))) ;; 1 is not a place!
(def-error-test ccase.7 (ccase))

;;; ECASE

(def-error-test ecase.1 (ecase . 1))
(def-error-test ecase.2 (ecase nil . 1))
(def-error-test ecase.3 (ecase nil (nil . 1)))
(def-error-test ecase.4 (ecase 'x nil))
(def-error-test ecase.5 (ecase 'x ((nil . x) 1)))
(def-error-test ecase.6 (ecase))

;;; TYPECASE

(def-error-test typecase.1 (typecase))
(def-error-test typecase.2 (typecase . :foo))
(def-error-test typecase.3 (typecase 'x . #\X))
(def-error-test typecase.4 (typecase 'x (#.(gensym) t)))
(def-error-test typecase.5 (typecase 'x (symbol . :foo)))
(def-error-test typecase.6 (typecase 'x . :foo))
(def-error-test typecase.7 (typepcase 'x (t . :foo)))
(def-error-test typecase.8 (typepcase 'x (otherwise . :foo)))

;;; CTYPECASE

(def-error-test ctypecase.1 (ctypecase))
(def-error-test ctypecase.2 (ctypecase . :foo))
(def-error-test ctypecase.3 (let ((x 'x)) (ctypecase x . #\X)))
(def-error-test ctypecase.4 (let ((x 'x)) (ctypecase x (#.(gensym) t))))
(def-error-test ctypecase.5 (let ((x 'x)) (ctypecase x (symbol . :foo))))
(def-error-test ctypecase.6 (let ((x 'x)) (ctypecase x . :foo)))
(def-error-test ctypecase.7 (let ((x 'x)) (ctypecase x (t . :foo))))
(def-error-test ctypecase.8 (let ((x 'x)) (ctypecase x (otherwise . :foo))))
(def-error-test ctypecase.9 (ctypecase 1 (integer :bad)))

;;; ETYPECASE

(def-error-test etypecase.1 (etypecase))
(def-error-test etypecase.2 (etypecase . :foo))
(def-error-test etypecase.3 (etypecase 'x . #\X))
(def-error-test etypecase.4 (etypecase 'x (#.(gensym) t)))
(def-error-test etypecase.5 (etypecase 'x (symbol . :foo)))
(def-error-test etypecase.6 (etypecase 'x . :foo))

;;; MULTIPLE-VALUE-BIND

(def-error-test multiple-value-bind.1 (multiple-value-bind))
(def-error-test multiple-value-bind.2 (multiple-value-bind .
					  #.(1+ most-positive-fixnum)))
(def-error-test multiple-value-bind.3 (multiple-value-bind (x)))
(def-error-test multiple-value-bind.4 (multiple-value-bind (x . y) 1 x))
(def-error-test multiple-value-bind.5 (multiple-value-bind (x) . :foo))
(def-error-test multiple-value-bind.6 (multiple-value-bind (x) nil . :bar))
(def-error-test multiple-value-bind.7
  (multiple-value-bind (x) nil "doc string" . 1))
(def-error-test multiple-value-bind.8
  (multiple-value-bind (x) nil (declare) . 1))
(def-error-test multiple-value-bind.9
  (multiple-value-bind (x) 1 (declare (type symbol x)) x))
(def-error-test multiple-value-bind.10
  (multiple-value-bind (x) 1 nil (declare) nil))
(def-error-test multiple-value-bind.11
  (multiple-value-bind (x) 1 "foo" "bar" (declare) nil))

;;; MULTIPLE-VALUE-CALL

(def-error-test multiple-value-call.1 (multiple-value-call))
(def-error-test multiple-value-call.2 (multiple-value-call . :x))
(def-error-test multiple-value-call.3 (multiple-value-call 'list . :x))
(def-error-test multiple-value-call.4 (multiple-value-call 'list 1 . :x))
(def-all-error-test multiple-value-call.5 'function-designator-p
  '(multiple-value-call x nil))
(def-error-test multiple-value-call.6 (multiple-value-call (gensym)))

;;; MULTIPLE-VALUE-LIST

(def-error-test multiple-value-list.1 (multiple-value-list))
(def-error-test multiple-value-list.2 (multiple-value-list . 1))
(def-error-test multiple-value-list.3 (multiple-value-list 1 . 2))
(def-error-test multiple-value-list.4 (multiple-value-list 1 2))

;;; MULTIPLE-VALUE-PROG1

(def-error-test multiple-value-prog1.1 (multiple-value-prog1))
(def-error-test multiple-value-prog1.2 (multiple-value-prog1 . 1))
(def-error-test multiple-value-prog1.3 (multiple-value-prog1 :x . :y))

;;; MULTIPLE-VALUE-SETQ

(def-error-test multiple-value-setq.1 (multiple-value-setq))
(def-error-test multiple-value-setq.2 (let (x) (multiple-value-setq (x)) x))
(def-error-test multiple-value-setq.3
  (let (x y) (multiple-value-setq (x . y) nil (list x y))))
(def-all-error-test multiple-value-setq.4 'symbolp
  #'(lambda (x) `(multiple-value-setq (,x) nil)))
(def-all-error-test multiple-value-setq.5 (constantly nil)
  #'(lambda (x) `(multiple-value-setq (,x) nil))
  :vals cl-test::*cl-constant-symbols*)

;;; VALUES

(def-all-error-test values.1 'listp #'(lambda (x) (cons 'values x)))
(def-all-error-test values.2 'listp #'(lambda (x) (list* 'values 1 x)))

;;; NTH-VALUE

(def-error-test nth-value.1 (nth-value))
(def-error-test nth-value.2 (nth-value 0))
(def-error-test nth-value.3 (nth-value 1 '(a b c) 2))
(def-all-error-test nth-value.4 (constantly nil) #'(lambda (x) `(nth-value ',x)))
(def-all-error-test nth-value.5 (constantly nil) #'(lambda (x) `(nth-value . ,x)))
(def-all-error-test nth-value.6 (constantly nil) #'(lambda (x) `(nth-value 0 . ,x)))
(def-all-error-test nth-value.7 'integerp #'(lambda (x) `(nth-value ',x nil)))
(def-error-test nth-value.8 (nth-value -1 'x))
(def-all-error-test nth-value.9 'null #'(lambda (x) `(nth-value 0 'a . ,x)))

;;; PROG

(def-error-test prog.1 (prog))
(def-all-error-test prog.2 'listp #'(lambda (x) `(prog . ,x)))
(def-all-error-test prog.3 'listp #'(lambda (x) `(prog ,x)))
(def-all-error-test prog.4 'listp #'(lambda (x) `(prog () . ,x)))
(def-all-error-test prog.5 (typef '(or symbol cons))  #'(lambda (x) `(prog (,x))))
(def-all-error-test prog.6 'listp #'(lambda (x) `(prog (v . ,x))))
(def-all-error-test prog.7 'listp #'(lambda (x) `(prog ((v . ,x)))))
(def-error-test prog.8 (prog ((x nil nil))))
(def-all-error-test prog.9 'null #'(lambda (x) `(prog ((v nil . ,x)))))

;;; PROG*

(def-error-test prog*.1 (prog*))
(def-all-error-test prog*.2 'listp #'(lambda (x) `(prog* . ,x)))
(def-all-error-test prog*.3 'listp #'(lambda (x) `(prog* ,x)))
(def-all-error-test prog*.4 'listp #'(lambda (x) `(prog* () . ,x)))
(def-all-error-test prog*.5 (typef '(or symbol cons))  #'(lambda (x) `(prog* (,x))))
(def-all-error-test prog*.6 'listp #'(lambda (x) `(prog* (v . ,x))))
(def-all-error-test prog*.7 'listp #'(lambda (x) `(prog* ((v . ,x)))))
(def-error-test prog*.8 (prog* ((x nil nil))))
(def-all-error-test prog*.9 'null #'(lambda (x) `(prog* ((v nil . ,x)))))

;;; PROG1

(def-error-test prog1.1 (prog1))
(def-all-error-test prog1.2 #'listp #'(lambda (x) `(prog1 . ,x)))
(def-all-error-test prog1.3 #'listp #'(lambda (x) `(prog1 nil . ,x)))

;;; PROG2

(def-error-test prog2.1 (prog2))
(def-all-error-test prog2.2 #'listp #'(lambda (x) `(prog2 . ,x)))
(def-error-test prog2.3 (prog2 t))
(def-all-error-test prog2.4 #'listp #'(lambda (x) `(prog2 nil . ,x)))
(def-all-error-test prog2.5 #'listp #'(lambda (x) `(prog2 'a 'b . ,x)))
(def-all-error-test prog2.6 #'listp #'(lambda (x) `(prog2 'a 'b nil . ,x)))

;;; PROGN

(def-all-error-test progn.1 'listp #'(lambda (x) `(progn . ,x)))
(def-all-error-test progn.2 'listp #'(lambda (x) `(progn nil . ,x)))
(def-all-error-test progn.3 'listp #'(lambda (x) `(progn 'a 'b . ,x)))

;;; DEFINE-MODIFY-MACRO

(def-error-test define-modify-macro.1 (define-modify-macro))
(def-error-test define-modify-macro.2 (define-modify-macro #.(gensym)))
(def-all-error-test define-modify-macro.3 'symbolp #'(lambda (x) `(define-modify-macro ,x ())))
(def-all-error-test define-modify-macro.4 'listp #'(lambda (x) `(define-modify-macro #.(gensym) ,x)))
(def-all-error-test define-modify-macro.5 'listp #'(lambda (x) `(define-modify-macro #.(gensym) () . ,x)))
(def-all-error-test define-modify-macro.6 'symbolp #'(lambda (x) `(define-modify-macro #.(gensym) () ,x)))
(def-all-error-test define-modify-macro.7 'stringp #'(lambda (x) `(define-modify-macro #.(gensym) () #.(gensym) ,x)))
(def-all-error-test define-modify-macro.8 'listp #'(lambda (x) `(define-modify-macro #.(gensym) () #.(gensym) . ,x)))
(def-all-error-test define-modify-macro.9 'listp #'(lambda (x) `(define-modify-macro #.(gensym) () #.(gensym) "foo" . ,x)))
(def-all-error-test define-modify-macro.10 (constantly nil)
  #'(lambda (x) `(define-modify-macro #.(gensym) () #.(gensym) "foo" ,x)))

;;; DEFSETF

(def-error-test defsetf.1 (defsetf))
(def-error-test defsetf.2 (defsetf #.(gensym)))
(def-all-error-test defsetf.3 'listp #'(lambda (x) `(defsetf ,x)))
(def-all-error-test defsetf.4 'listp #'(lambda (x) `(defsetf #.(gensym) . ,x)))
(def-all-error-test defsetf.5 'listp #'(lambda (x) `(defsetf #.(gensym) #.(gensym) . ,x)))
(def-all-error-test defsetf.6 'stringp #'(lambda (x) `(defsetf #.(gensym) #.(gensym) ,x)))
(def-all-error-test defsetf.7 'null #'(lambda (x) `(defsetf #.(gensym) #.(gensym) "foo" . ,x)))
(def-all-error-test defsetf.8 (constantly nil) #'(lambda (x) `(defsetf #.(gensym) #.(gensym) "foo" ,x)))
(def-all-error-test defsetf.9 (typef '(or list symbol)) #'(lambda (x) `(defsetf #.(gensym) ,x)))

;;; Need long form defsetf error tests

;;; FIXME: add tests for defsetf-lambda-lists

(def-all-error-test defsetf.10 'symbolp #'(lambda (x)  `(defsetf #.(gensym) (#1=#.(gensym)) (,x) #1#)))
(def-all-error-test defsetf.11 'listp #'(lambda (x) `(defsetf #.(gensym) (#.(gensym)) ., x)))
(def-all-error-test defsetf.12 'listp #'(lambda (x) `(defsetf #.(gensym) (#.(gensym)) , x)))
(def-all-error-test defsetf.13 'listp #'(lambda (x) `(defsetf #.(gensym) (#.(gensym)) (a . ,x))))

(def-error-test defsetf.14 (defsetf #.(gensym) () () nil (declare (optimize)) nil))
(def-error-test defsetf.15 (defsetf #.(gensym) () () "foo" "bar" (declare (optimize)) nil))

;;; FIXME -- Add tests for DEFINE-SETF-EXPANDER

(def-error-test get-setf-expansion.1 (get-setf-expansion))
(def-all-error-test get-setf-expansion.2 'listp #'(lambda (x) `(get-setf-expansion . ,x)))
(def-all-error-test get-setf-expansion.3 (typef '(or list symbol))
  #'(lambda (x) `(get-setf-expansion ,x)))

;;; FIXME -- figure out how to test for invalid environment objects
;;;   Must make an assumption about what can be an environment

;;; SETF tests

(def-all-error-test setf.1 (constantly nil) #'(lambda (x) `(setf ,x)))
(def-all-error-test setf.2 'listp #'(lambda (x) `(setf . ,x)))
(def-all-error-test setf.3 'listp #'(lambda (x) `(setf ,x nil)))
(def-all-error-test setf.4 'listp #'(lambda (x) `(let (a) (setf a . ,x))))

;;; PSETF tests

(def-all-error-test psetf.1 (constantly nil) #'(lambda (x) `(psetf ,x)))
(def-all-error-test psetf.2 'listp #'(lambda (x) `(psetf . ,x)))
(def-all-error-test psetf.3 'listp #'(lambda (x) `(psetf ,x nil)))
(def-all-error-test psetf.4 'listp #'(lambda (x) `(let (a) (psetf a . ,x))))

;;; SHIFTF tests

(def-error-test shiftf.1 (shiftf))
(def-all-error-test shiftf.2 'listp #'(lambda (x) `(shiftf . ,x)))
(def-all-error-test shiftf.3 (constantly nil) #'(lambda (x) `(shiftf ,x)))
(def-all-error-test shiftf.4 'listp #'(lambda (x) `(let (a) (shiftf a . ,x))))
(def-all-error-test shiftf.5 'listp #'(lambda (x) `(shiftf ,x nil)))
(def-all-error-test shiftf.6 'listp #'(lambda (x) `(let (a b) (shiftf a b . ,x))))
(def-all-error-test shiftf.7 'listp #'(lambda (x) `(let (a) (shiftf ,x a nil))))
(def-all-error-test shiftf.8 'listp #'(lambda (x) `(let (a) (shiftf a ,x nil))))

;;; ROTATEF tests

(def-all-error-test rotatef.1 'listp #'(lambda (x) `(rotatef . ,x)))
(def-all-error-test rotatef.2 'listp #'(lambda (x) `(rotatef ,x)))
(def-all-error-test rotatef.3 'listp #'(lambda (x) `(let (a) (rotatef a ,x))))
(def-all-error-test rotatef.4 'listp #'(lambda (x) `(let (a) (rotatef a . ,x))))
(def-all-error-test rotatef.5 'listp #'(lambda (x) `(let (a) (rotatef ,x a))))
