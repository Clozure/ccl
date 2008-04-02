;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 21 18:24:59 2004
;;;; Contains: Tests of DO-SYMBOLS

(in-package :cl-test)

(compile-and-load "package-aux.lsp")

(declaim (optimize (safety 3)))

(deftest do-symbols.1
  (progn
    (set-up-packages)
    (equalt
     (remove-duplicates
      (sort-symbols (let ((all nil))
		      (do-symbols (x "B" all) (push x all)))))
     (list (find-symbol "BAR" "B")
	   (find-symbol "FOO" "A"))))
  t)

;;
;; Test up some test packages
;;

(defun collect-symbols (pkg)
  (remove-duplicates
   (sort-symbols
    (let ((all nil))
      (do-symbols (x pkg all) (push x all))))))

(deftest do-symbols.2
  (collect-symbols "DS1")
  (DS1:A DS1:B DS1::C DS1::D))

(deftest do-symbols.3
  (collect-symbols "DS2")
  (DS2:A DS2::E DS2::F DS2:G DS2:H))

(deftest do-symbols.4
  (collect-symbols "DS3")
  (DS1:A DS3:B DS2:G DS2:H DS3:I DS3:J DS3:K DS3::L DS3::M))

(deftest do-symbols.5
  (remove-duplicates
   (collect-symbols "DS4")
   :test #'(lambda (x y)
	     (and (eqt x y)
		  (not (eqt x 'DS4::B)))))
  (DS1:A DS1:B DS2::F DS3:G DS3:I DS3:J DS3:K DS4::X DS4::Y DS4::Z))


;; Test that do-symbols works without
;; a return value (and that the default return value is nil)

(deftest do-symbols.6
  (do-symbols (s "DS1") (declare (ignore s)) t)
  nil)

;; Test that do-symbols works without a package being specified

(deftest do-symbols.7
  (let ((x nil)
	(*package* (find-package "DS1")))
    (list
     (do-symbols (s) (push s x))
     (sort-symbols x)))
  (nil (DS1:A DS1:B DS1::C DS1::D)))

;; Test that the tags work in the tagbody,
;;  and that multiple statements work

(deftest do-symbols.8
  (handler-case
   (let ((x nil))
     (list
      (do-symbols
       (s "DS1")
       (when (equalt (symbol-name s) "C") (go bar))
       (push s x)
       (go foo)
       bar
       (push t x)
       foo)
      (sort-symbols x)))
   (error (c) c))
  (NIL (DS1:A DS1:B DS1::D T)))

;;; Specialized sequences

(defmacro def-do-symbols-test (test-name name-form)
  `(deftest ,test-name
     (let ((name ,name-form))
       (assert (string= name "B"))
       (set-up-packages)
       (equalt
	(remove-duplicates
	 (sort-symbols (let ((all nil))
			 (do-symbols (x name all) (push x all)))))
	(list (find-symbol "BAR" "B")
	      (find-symbol "FOO" "A"))))
     t))

(def-do-symbols-test do-symbols.9
  (make-array 1 :element-type 'base-char :initial-contents "B"))

(def-do-symbols-test do-symbols.10
  (make-array 5 :element-type 'character
	      :fill-pointer 1
	      :initial-contents "BXXXX"))

(def-do-symbols-test do-symbols.11
  (make-array 5 :element-type 'base-char
	      :fill-pointer 1
	      :initial-contents "BXXXX"))

(def-do-symbols-test do-symbols.12
  (make-array 1 :element-type 'base-char
	      :adjustable t :initial-contents "B"))

(def-do-symbols-test do-symbols.13
  (make-array 1 :element-type 'character
	      :adjustable t :initial-contents "B"))

(def-do-symbols-test do-symbols.14
  (let* ((etype 'base-char)
	 (name0 (make-array 4 :element-type etype :initial-contents "XBYZ")))
    (make-array 1 :element-type etype
		:displaced-to name0 :displaced-index-offset 1)))

(def-do-symbols-test do-symbols.15
  (let* ((etype 'character)
	 (name0 (make-array 4 :element-type etype :initial-contents "XBYZ")))
    (make-array 1 :element-type etype
		:displaced-to name0 :displaced-index-offset 1)))

;;; Free declaration scope tests

(deftest do-symbols.16
  (block done
    (let ((x :bad))
      (declare (special x))
      (let ((x :good))
	(do-symbols (s (return-from done x))
	  (declare (special x))))))
  :good)

(deftest do-symbols.17
  (let ((x :good))
    (declare (special x))
    (let ((x :bad))
      (do-symbols (s "CL-TEST" x)
	(declare (special x)))))
  :good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest do-symbols.18
  (macrolet
   ((%m (z) z))
   (do-symbols (s (expand-in-current-env (%m "CL-TEST")) :good)))
  :good)

(deftest do-symbols.19
  (macrolet
   ((%m (z) z))
   (do-symbols (s "CL-TEST" (expand-in-current-env (%m :good)))))
  :good)

(def-macro-test do-symbols.error.1
  (do-symbols (x "CL")))
