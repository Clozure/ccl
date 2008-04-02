;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Nov 28 06:43:51 2002
;;;; Contains: Aux. functions for cl-symbols.lsp

(in-package :cl-test)

(declaim (optimize (safety 3)))

(defun is-external-symbol-of (sym package)
  (multiple-value-bind (sym2 status)
      (find-symbol (symbol-name sym) package)
    (and (eqt sym sym2)
	 (eqt status :external))))

(defun test-if-not-in-cl-package (str)
  (multiple-value-bind (sym status)
      (find-symbol #+lower-case str #-lower-case (string-upcase str) 'common-lisp)
      (or
       ;; Symbol not present in the common lisp package as an external symbol
       (not (eqt status :external))
       ;; Check if it has any properties whose indicators are
       ;; external in any of the standard packages or are accessible
       ;; in CL-USER
       (let ((plist (symbol-plist sym)))
	 (loop for e = plist then (cddr e)
	       for indicator = (car e)
	       while e
	       when (and (symbolp indicator)
			 (or (is-external-symbol-of indicator
						    "COMMON-LISP")
			     (is-external-symbol-of indicator "KEYWORD")
			     (eqt indicator (find-symbol
					     (symbol-name indicator)
					     "COMMON-LISP-USER"))))
	       collect indicator)))))

(defun safe-symbol-name (sym)
  (catch-type-error (symbol-name sym)))

(defun safe-make-symbol (name)
  (catch-type-error (make-symbol name)))
