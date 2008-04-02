;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jul 26 13:07:51 2004
;;;; Contains: Tests of binding the *PRINT-LEVEL* variable

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

#|
(deftest print-level.1
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (loop for x in *mini-universe*
	   for s1 = (write-to-string x)
	   for s2 = (let ((*print-level* 0)) (write-to-string x))
	   when (and (or (consp x)
			 (and (arrayp x)
			      (not (stringp x))
			      (not (typep x 'bit-vector)))
			 (typep (class-of x) 'structure-class))
		      (not (string= s2 "#")))
	   collect (list x s1 s2))))
  nil)
|#

(defclass print-level-test-class nil (a b c))

;;; The CLHS page for PRINT-OBJECT makes it clear that tests
;;; PRINT-LEVEL.2,6,7,10,11 were testing for implementation-dependent
;;; behavior. They have been commented out.

#|
(deftest print-level.2
  (with-standard-io-syntax
   (write-to-string (make-instance 'print-level-test-class)
		    :level 0
		    :readably nil))
  "#")
|#

(deftest print-level.3
  (with-standard-io-syntax
   (write-to-string (make-array '(4) :initial-contents '(a b c d))
		    :readably nil
		    :array t
		    :level 0))
  "#")

(deftest print-level.4
  (with-standard-io-syntax
   (write-to-string (make-array '(4) :initial-contents '(1 1 0 1)
				:element-type 'bit)
		    :readably nil
		    :array t
		    :level 0))
  "#*1101")

(deftest print-level.5
  (with-standard-io-syntax
   (write-to-string "abcd"
		    :readably nil
		    :array t
		    :level 0))
  "\"abcd\"")

(define-condition print-level-condition (condition) (a b c))

#|
(deftest print-level.6
  (with-standard-io-syntax
   (write-to-string (make-condition 'print-level-condition)
		    :level 0 :pretty nil :readably nil))
  "#")

(deftest print-level.7
  (with-standard-io-syntax
   (write-to-string (make-condition 'print-level-condition)
		    :level 0 :pretty t :readably nil))
  "#")
|#

(defstruct print-level-struct)

(deftest print-level.8
  (with-standard-io-syntax
   (let* ((*package* (find-package "CL-TEST"))
	  (*print-pretty* nil)
	  (s (make-print-level-struct)))
     (values
      (write-to-string s :level 0   :readably nil)
      (write-to-string s :level 1   :readably nil)
      (write-to-string s :level nil :readably nil))))
  "#S(PRINT-LEVEL-STRUCT)"
  "#S(PRINT-LEVEL-STRUCT)"
  "#S(PRINT-LEVEL-STRUCT)")

(deftest print-level.9
  (with-standard-io-syntax
   (let* ((*package* (find-package "CL-TEST"))
	  (*print-pretty* t)
	  (s (make-print-level-struct)))
     (values
      (write-to-string s :level 0   :readably nil)
      (write-to-string s :level 1   :readably nil)
      (write-to-string s :level nil :readably nil))))
  "#S(PRINT-LEVEL-STRUCT)"
  "#S(PRINT-LEVEL-STRUCT)"
  "#S(PRINT-LEVEL-STRUCT)")

(defstruct print-level-struct2 a b c)

#|
(deftest print-level.10
  (with-standard-io-syntax
   (let ((*package* (find-package "CL-TEST")))
     (write-to-string (make-print-level-struct2)
		      :level 0 :pretty nil :readably nil)))
  "#")

(deftest print-level.11
  (with-standard-io-syntax
   (let ((*package* (find-package "CL-TEST")))
     (write-to-string (make-print-level-struct2)
		      :level 0 :pretty t :readably nil)))
  "#")
|#

(deftest print-level.12
  (with-standard-io-syntax
   (let ((*print-level* (1+ most-positive-fixnum)))
     (write-to-string '((1 2) (3 4)) :pretty nil :readably nil)))
  "((1 2) (3 4))")
