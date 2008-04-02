;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jun  9 20:57:34 2003
;;;; Contains: Generators for lambda expressions

(in-package :rctest)

(compile-and-load "generator.lsp")


(defgenerator lambda-list-generator
  :body
  (let ((vars (loop for i from 1 to size collect (gensym))))
    (values vars t vars)))

(defvar *lambda-list-generator* (make-instance 'lambda-list-generator))

(defgenerator lambda-generator.1 :keys (vars)
  :body
  (let* ((s1 (random (min 5 size)))
	 (s2 (- size s1)))
    (multiple-value-bind (lambda-list success1 lambda-vars)
	(apply #'generate *lambda-list-generator* s1 ctxt)
      (let ((vars (append (mapcar #'list lambda-vars) vars)))
	(multiple-value-bind (body success2)
	    (apply #'generate 'implicit-progn-generator s2 :vars vars ctxt)
	  (if (and success1 success2)
	      (values `(lambda ,lambda-list ,@body))
	    (values nil nil)))))))

(defvar *lambda-generator* (make-instance 'lambda-generator.1))
