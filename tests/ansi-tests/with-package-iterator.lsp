;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:03:36 1998
;;;; Contains: Tests of WITH-PACKAGE-ITERATOR

(in-package :cl-test)
(declaim (optimize (safety 3)))

(compile-and-load "package-aux.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; with-package-iterator

(deftest with-package-iterator.1
  (with-package-iterator-internal (list (find-package "COMMON-LISP-USER")))
  t)

(deftest with-package-iterator.2
  (with-package-iterator-external (list (find-package "COMMON-LISP-USER")))
  t)

(deftest with-package-iterator.3
  (with-package-iterator-inherited (list (find-package "COMMON-LISP-USER")))
  t)

(deftest with-package-iterator.4
  (with-package-iterator-all (list (find-package "COMMON-LISP-USER")))
  t)

;;; Should test on some packages containing shadowed symbols,
;;; multiple inheritance

(deftest with-package-iterator.5
  (progn
    (set-up-packages)
    (with-package-iterator-all '("A")))
  t)

(deftest with-package-iterator.6
  (progn
    (set-up-packages)
    (with-package-iterator-all '(#:|A|)))
  t)

(deftest with-package-iterator.7
  (progn
    (set-up-packages)
    (with-package-iterator-all '(#\A)))
  t)

(deftest with-package-iterator.8
  (progn
    (set-up-packages)
    (with-package-iterator-internal (list (find-package "A"))))
  t)

(deftest with-package-iterator.9
  (progn
    (set-up-packages)
    (with-package-iterator-external (list (find-package "A"))))
  t)

(deftest with-package-iterator.10
  (progn
    (set-up-packages)
    (with-package-iterator-inherited (list (find-package "A"))))
  t)

(deftest with-package-iterator.11
  (signals-error 
   (with-package-iterator (x "COMMON-LISP-USER"))
   program-error)
  t)

;;; Apply to all packages
(deftest with-package-iterator.12
  (loop
   for p in (list-all-packages) count
   (handler-case
    (progn
      (format t "Package ~S~%" p)
      (not (with-package-iterator-internal (list p))))
    (error (c)
	   (format "Error ~S on package ~A~%" c p)
	   t)))
  0)

(deftest with-package-iterator.13
  (loop
   for p in (list-all-packages) count
   (handler-case
    (progn
      (format t "Package ~S~%" p)
      (not (with-package-iterator-external (list p))))
    (error (c)
	   (format "Error ~S on package ~A~%" c p)
	   t)))
  0)

(deftest with-package-iterator.14
  (loop
   for p in (list-all-packages) count
   (handler-case
    (progn
      (format t "Package ~S~%" p)
      (not (with-package-iterator-inherited (list p))))
    (error (c)
	   (format t "Error ~S on package ~S~%" c p)
	   t)))
  0)

(def-macro-test with-package-iterator.error.1
  (with-package-iterator (x "CL" :external) nil))


;;; Specialized sequence tests

(defmacro def-with-package-iterator-test (test-name name-form)
  `(deftest ,test-name
     (let ((name ,name-form))
       (safely-delete-package name)
       (let* ((p (make-package name :use nil))
	      (result nil)
	      (s (intern "X" p)))
	 (with-package-iterator
	  (x name :internal)
	  (loop
	   (multiple-value-bind
	       (good? sym)
	       (x)
	       (unless good?
		 (safely-delete-package name)
		 (return (equalt (list s) result)))
	     (push sym result))))))
     t))

(def-with-package-iterator-test with-package-iterator.15
  (make-array 5 :initial-contents "TEST1"
	      :element-type 'base-char))

(def-with-package-iterator-test with-package-iterator.16
  (make-array 8 :initial-contents "TEST1XXX"
	      :fill-pointer 5
	      :element-type 'base-char))

(def-with-package-iterator-test with-package-iterator.17
  (make-array 8 :initial-contents "TEST1XXX"
	      :fill-pointer 5
	      :element-type 'character))

(def-with-package-iterator-test with-package-iterator.18
  (make-array 5 :initial-contents "TEST1"
	      :adjustable t
	      :element-type 'base-char))

(def-with-package-iterator-test with-package-iterator.19
  (make-array 5 :initial-contents "TEST1"
	      :adjustable t
	      :element-type 'character))

(def-with-package-iterator-test with-package-iterator.20
  (let* ((etype 'base-char)
	 (name0 (make-array 10 :initial-contents "XTEST1YzYY"
			    :element-type etype)))
    (make-array 5 :element-type etype
		:displaced-to name0
		:displaced-index-offset 1)))

(def-with-package-iterator-test with-package-iterator.21
  (let* ((etype 'character)
	 (name0 (make-array 10 :initial-contents "XTEST1YzYY"
			    :element-type etype)))
    (make-array 5 :element-type etype
		:displaced-to name0
		:displaced-index-offset 1)))

;;; Free declaration scope

(deftest with-package-iterator.22
  (block done
    (let ((x :bad))
      (declare (special x))
      (let ((x :good))
	(with-package-iterator (s (return-from done x) :internal)
			       (declare (special x))))))
  :good)
