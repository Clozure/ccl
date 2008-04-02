;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov 29 05:06:57 2003
;;;; Contains: Tests of the function PATHNAME

(in-package :cl-test)

(deftest pathname.1
  (loop for x in *pathnames*
	always (eq x (pathname x)))
  t)

(deftest pathname.2
  (equalt #p"ansi-aux.lsp" (pathname "ansi-aux.lsp"))
  t)

(deftest pathname.3
  (let ((s (open "ansi-aux.lsp" :direction :input)))
    (prog1 (equalt (truename (pathname s)) (truename #p"ansi-aux.lsp"))
      (close s)))
  t)

(deftest pathname.4
  (let ((s (open "ansi-aux.lsp" :direction :input)))
    (close s)
    (equalt (truename (pathname s)) (truename #p"ansi-aux.lsp")))
  t)

(deftest pathname.5
  (loop for x in *logical-pathnames*
	always (eq x (pathname x)))
  t)

(deftest pathname.6
  (equalt #p"ansi-aux.lsp"
	  (pathname (make-array 12 :initial-contents "ansi-aux.lsp"
				:element-type 'base-char)))
  t)

(deftest pathname.7
  (equalt #p"ansi-aux.lsp"
	  (pathname (make-array 15 :initial-contents "ansi-aux.lspXXX"
				:element-type 'base-char
				:fill-pointer 12)))
  t)

(deftest pathname.8
  (equalt #p"ansi-aux.lsp"
	  (pathname (make-array 12 :initial-contents "ansi-aux.lsp"
				:element-type 'base-char
				:adjustable t)))
  t)

(deftest pathname.9
  (equalt #p"ansi-aux.lsp"
	  (pathname (make-array 15 :initial-contents "ansi-aux.lspXXX"
				:element-type 'character
				:fill-pointer 12)))
  t)

(deftest pathname.10
  (equalt #p"ansi-aux.lsp"
	  (pathname (make-array 12 :initial-contents "ansi-aux.lsp"
				:element-type 'character
				:adjustable t)))
  t)

(deftest pathname.11
  (loop for etype in '(standard-char base-char character)
	collect
	(equalt #p"ansi-aux.lsp"
		(pathname
		 (let* ((s (make-array 15 :initial-contents "XXansi-aux.lspX"
				       :element-type etype)))
		   (make-array 12 :element-type etype
			       :displaced-to s
			       :displaced-index-offset 2)))))
  (t t t))

;;; Error tests

(deftest pathname.error.1
  (signals-error (pathname) program-error)
  t)

(deftest pathname.error.2
  (signals-error (pathname (first *pathnames*) nil) program-error)
  t)
