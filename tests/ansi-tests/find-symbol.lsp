;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 07:49:34 1998
;;;; Contains: Tests for FIND-SYMBOL

(in-package :cl-test)

(compile-and-load "packages-00.lsp")

;;(declaim (optimize (safety 3)))

;; Test find-symbol, with the various combinations of
;; package designators

(deftest find-symbol.1
  (find-symbol "aBmAchb1c")
  nil nil)

(deftest find-symbol.2
  (find-symbol "aBmAchb1c" "CL")
  nil nil)

(deftest find-symbol.3
  (find-symbol "aBmAchb1c" "COMMON-LISP")
  nil nil)

(deftest find-symbol.4
  (find-symbol "aBmAchb1c" "KEYWORD")
  nil nil)

(deftest find-symbol.5
  (find-symbol "aBmAchb1c" "COMMON-LISP-USER")
  nil nil)

(deftest find-symbol.6
  (find-symbol (string '#:car) "CL")
  car :external)

(deftest find-symbol.7
  (find-symbol (string '#:car) "COMMON-LISP")
  car :external)

(deftest find-symbol.8
  (values (find-symbol (string '#:car) "COMMON-LISP-USER"))
  car #| :inherited |# )

(deftest find-symbol.9
  (find-symbol (string '#:car) "CL-TEST")
  car :inherited)

(deftest find-symbol.10
  (find-symbol (string '#:test) "KEYWORD")
  :test :external)

(deftest find-symbol.11
  (find-symbol (string '#:find-symbol.11) "CL-TEST")
  find-symbol.11 :internal)

(deftest find-symbol.12
  (progn
    (set-up-packages)
    (let ((vals (multiple-value-list (find-symbol "FOO" #\A))))
      (values (length vals)
	      (package-name (symbol-package (first vals)))
	      (symbol-name (first vals))
	      (second vals))))
  2 "A" "FOO" :external)

(deftest find-symbol.13
  (progn
    (set-up-packages)
    (intern "X" (find-package "A"))
    (let ((vals (multiple-value-list (find-symbol "X" #\A))))
      (values (length vals)
	      (package-name (symbol-package (first vals)))
	      (symbol-name (first vals))
	      (second vals))))
  2 "A" "X" :internal)

(deftest find-symbol.14
  (progn
    (set-up-packages)
    (let ((vals (multiple-value-list (find-symbol "FOO" #\B))))
      (values (length vals)
	      (package-name (symbol-package (first vals)))
	      (symbol-name (first vals))
	      (second vals))))
  2 "A" "FOO" :inherited)

(deftest find-symbol.15
  (find-symbol "FOO" "FS-B")
  FS-A::FOO :inherited)

(deftest find-symbol.16
  (find-symbol "FOO" (find-package "FS-B"))
  FS-A::FOO :inherited)

(deftest find-symbol.17
  (let ((name (make-array '(3) :initial-contents "FOO"
			  :element-type 'base-char)))
    (find-symbol name "FS-B"))
  FS-A::FOO :inherited)

(deftest find-symbol.18
  (let ((name (make-array '(4) :initial-contents "FOOD"
			  :element-type 'character
			  :fill-pointer 3)))
    (find-symbol name "FS-B"))
  FS-A::FOO :inherited)

(deftest find-symbol.19
  (let ((name (make-array '(4) :initial-contents "FOOD"
			  :element-type 'base-char
			  :fill-pointer 3)))
    (find-symbol name "FS-B"))
  FS-A::FOO :inherited)

(deftest find-symbol.20
  (let* ((name0 (make-array '(5) :initial-contents "XFOOY"
			    :element-type 'character))
	 (name (make-array '(3) :element-type 'character
			   :displaced-to name0
			   :displaced-index-offset 1)))
    (find-symbol name "FS-B"))
  FS-A::FOO :inherited)

(deftest find-symbol.21
  (let* ((name0 (make-array '(5) :initial-contents "XFOOY"
			    :element-type 'base-char))
	 (name (make-array '(3) :element-type 'base-char
			   :displaced-to name0
			   :displaced-index-offset 1)))
    (find-symbol name "FS-B"))
  FS-A::FOO :inherited)

(deftest find-symbol.22
  (find-symbol "FOO" (make-array '(4) :initial-contents "FS-B" :element-type 'base-char))
  FS-A::FOO :inherited)

(deftest find-symbol.23
  (find-symbol "FOO" (make-array '(5) :initial-contents "FS-BX"
				 :fill-pointer 4
				 :element-type 'base-char))
  FS-A::FOO :inherited)



(deftest find-symbol.order.1
  (let ((i 0) x y)
    (values
     (find-symbol (progn (setf x (incf i)) (string '#:car))
		  (progn (setf y (incf i)) "COMMON-LISP"))
     i x y))
  car 2 1 2)

(deftest find-symbol.error.1
  (signals-error (find-symbol) program-error)
  t)

(deftest find-symbol.error.2
  (signals-error (find-symbol "CAR" "CL" nil) program-error)
  t)
