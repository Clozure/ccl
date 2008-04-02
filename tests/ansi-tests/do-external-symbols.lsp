;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 21 18:26:08 2004
;;;; Contains: Tests of DO-EXTERNAL-SYMBOLS

(in-package :cl-test)

(compile-and-load "package-aux.lsp")

(declaim (optimize (safety 3)))

(defun collect-external-symbols (pkg)
  (remove-duplicates
   (sort-symbols
    (let ((all nil))
      (do-external-symbols (x pkg all) (push x all))))))

(deftest do-external-symbols.1
    (collect-external-symbols "DS1")
  (DS1:A DS1:B))

(deftest do-external-symbols.2
    (collect-external-symbols "DS2")
  (DS2:A DS2:G DS2:H))

(deftest do-external-symbols.3
    (collect-external-symbols "DS3")
  (DS1:A DS3:B DS2:G DS3:I DS3:J DS3:K))

(deftest do-external-symbols.4
    (collect-external-symbols "DS4")
  ())

(deftest do-external-symbols.5
    (equalt (collect-external-symbols "KEYWORD")
	    (collect-symbols "KEYWORD"))
  t)

;; Test that do-external-symbols works without
;; a return value (and that the default return value is nil)

(deftest do-external-symbols.6
  (do-external-symbols (s "DS1") (declare (ignore s)) t)
  nil)

;; Test that do-external-symbols works without
;; a package being specified

(deftest do-external-symbols.7
  (let ((x nil)
	(*package* (find-package "DS1")))
    (list
     (do-external-symbols (s) (push s x))
     (sort-symbols x)))
  (nil (DS1:A DS1:B)))

;; Test that the tags work in the tagbody,
;;  and that multiple statements work

(deftest do-external-symbols.8
  (handler-case
   (let ((x nil))
     (list
      (do-external-symbols
       (s "DS1")
       (when (equalt (symbol-name s) "A") (go bar))
       (push s x)
       (go foo)
       bar
       (push t x)
       foo)
      (sort-symbols x)))
   (error (c) c))
  (NIL (DS1:B T)))

;;; Specialized sequence tests

(defmacro def-do-external-symbols-test (test-name name-form)
  `(deftest ,test-name
     (collect-external-symbols ,name-form)
     (DS1:A DS1:B)))

(def-do-external-symbols-test do-external-symbols.9
  (make-array 3 :element-type 'base-char :initial-contents "DS1"))

(def-do-external-symbols-test do-external-symbols.10
  (make-array 6 :element-type 'base-char :initial-contents "DS1XXX"
	      :fill-pointer 3))

(def-do-external-symbols-test do-external-symbols.11
  (make-array 6 :element-type 'character :initial-contents "DS1XXX"
	      :fill-pointer 3))

(def-do-external-symbols-test do-external-symbols.12
  (make-array 3 :element-type 'base-char :initial-contents "DS1"
	      :adjustable t))

(def-do-external-symbols-test do-external-symbols.13
  (make-array 3 :element-type 'character :initial-contents "DS1"
	      :adjustable t))

(def-do-external-symbols-test do-external-symbols.14
  (let* ((etype 'base-char)
	 (name0 (make-array 6 :element-type etype :initial-contents "XDS1XX")))
    (make-array 3 :element-type etype
		:displaced-to name0 :displaced-index-offset 1)))

(def-do-external-symbols-test do-external-symbols.15
  (let* ((etype 'character)
	 (name0 (make-array 6 :element-type etype :initial-contents "XDS1XX")))
    (make-array 3 :element-type etype
		:displaced-to name0 :displaced-index-offset 1)))

;;; Free declaration scope tests

(deftest do-external-symbols.16
  (block done
    (let ((x :bad))
      (declare (special x))
      (let ((x :good))
	(do-external-symbols (s (return-from done x))
	  (declare (special x))))))
  :good)

(deftest do-external-symbols.17
  (let ((x :good))
    (declare (special x))
    (let ((x :bad))
      (do-external-symbols (s "CL-TEST" x)
	(declare (special x)))))
  :good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest do-external-symbols.18
  (macrolet
   ((%m (z) z))
   (do-external-symbols (s (expand-in-current-env (%m "CL-TEST")) :good)))
  :good)

(deftest do-external-symbols.19
  (macrolet
   ((%m (z) z))
   (do-external-symbols (s "CL-TEST" (expand-in-current-env (%m :good)))))
  :good)

;;; Error tests

(def-macro-test do-external-symbols.error.1
  (do-external-symbols (x "CL")))