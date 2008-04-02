;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct  7 22:11:31 2002
;;;; Contains: Tests for LAMBDA-LIST-KEYWORDS

(in-package :cl-test)

;;; The variable is bound
(deftest lambda-list-keywords.1
  (not-mv (boundp 'lambda-list-keywords))
  nil)

;;; The variable is a constant
(deftest lambda-list-keywords.2
  (not-mv (constantp 'lambda-list-keywords))
  nil)

;;; The standard keywords are present in the list
(deftest lambda-list-keywords.3
  (and (consp lambda-list-keywords)
       (not-mv (set-difference '(&allow-other-keys 
				 &aux &body &environment
				 &key &optional &rest &whole)
			       lambda-list-keywords)))
  t)

;;; No lambda list keywords are in the keyword package
;;; (deftest lambda-list-keywords.4
;;;  (some #'keywordp lambda-list-keywords)
;;;  nil)

;;; Every keyword starts with an ampersand
(deftest lambda-list-keywords.5
  (notevery #'(lambda (sym)
		(and (symbolp sym)
		     (let ((name (symbol-name sym)))
		       (and (> (length name) 0)
			    (eql (aref name 0) #\&)))))
	    lambda-list-keywords)
  nil)
