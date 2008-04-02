;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 19 11:42:14 2002
;;;; Contains: Tests of DEFINE-MODIFY-MACRO

(in-package :cl-test)

(deftest define-modify-macro.1
  (values
   (eval '(define-modify-macro dmm1-appendf (&rest args) 
	    append "Append lists onto a list"))
   (eval 
    '(let ((u '(p q r)) v)
       (list
	(setq v u)
	(dmm1-appendf u '(a b c d))
	(dmm1-appendf u ())
	(dmm1-appendf u '(e f g))
	u
	v))))
  dmm1-appendf
  ((p q r)
   (p q r a b c d)
   (p q r a b c d)
   (p q r a b c d e f g)
   (p q r a b c d e f g)
   (p q r)))

(deftest define-modify-macro.2
  (values
   (eval '(define-modify-macro new-incf (&optional (delta 1)) +))
   (eval
    '(let ((i 10))
       (list
	(new-incf i)
	(new-incf i 100)
	i))))
  new-incf
  (11 111 111))

(deftest define-modify-macro.3
  (values
   (eval '(define-modify-macro new-incf1 (&optional (delta 1)) +))
   (eval
    '(let ((a (vector 0 0 0 0 0))
	   (i 1))
       (list
	(new-incf1 (aref a (incf i)))
	a
	i))))
  new-incf1
  (1 #(0 0 1 0 0) 2))

(deftest define-modify-macro.4
  (values
   (eval '(define-modify-macro new-incf2 (&optional (delta 1)) +))
   (eval
    '(let ((a (vector 0 0 0 0 0))
	   (i 1))
       (list
	(new-incf2 (aref a (incf i)) (incf i))
	a
	i))))
  new-incf2
  (3 #(0 0 3 0 0) 3))

;;; (deftest define-modify-macro.error.1
;;;   (signals-error (define-modify-macro)  program-error)
;;;   t)
;;; 
;;; (deftest define-modify-macro.error.2
;;;   (signals-error (define-modify-macro dfm-error-1) program-error)
;;;   t)
;;; 
;;; (deftest define-modify-macro.error.3
;;;   (signals-error (define-modify-macro dfm-error-2 ()) program-error)
;;;   t)
;;; 
;;; (deftest define-modify-macro.error.4
;;;   (signals-error (define-modify-macro dfm-error-2 () nil "Documentation"
;;; 		    "extra illegal argument")
;;;                   program-error)
;;;   t)

(def-macro-test define-modify-macro.error.1
  (define-modify-macro nonexistent-modify-macro () foo))

;;; Documentation tests

(deftest define-modify-macro.documentation.1
  (let ((sym (gensym)))
    (eval `(define-modify-macro ,sym (&optional (delta 1)) +))
    (values
     (documentation sym 'function)
     (documentation (macro-function sym) 'function)
     (documentation (macro-function sym) t)))
  nil nil nil)

(deftest define-modify-macro.documentation.2
  (let ((sym (gensym))
	(doc "DMM-DOC"))
    (eval `(define-modify-macro ,sym (&optional (delta 1)) + ,doc))
    (values
     (equalt doc (or (documentation sym 'function) doc))
     (equalt doc (or (documentation (macro-function sym) 'function) doc))
     (equalt doc (or (documentation (macro-function sym) t) doc))))
  t t t)
