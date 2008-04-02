;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Dec 31 09:31:33 2003
;;;; Contains: Tests (such as they are) for LOAD-LOGICAL-PATHNAME-TRANSLATIONS

(in-package :cl-test)

;;; The function LOAD-LOGICAL-PATHNAME-TRANSLATIONS is almost entirely
;;; untestable, since the basic behavior is implementation defined.

(deftest load-logical-pathname-translations.1
  (load-logical-pathname-translations "CLTESTROOT")
  nil)

;;; Error cases

(deftest load-logical-pathname-translations.error.1
  (handler-case
   (progn (load-logical-pathname-translations
	   "THEREHADBETTERNOTBEAHOSTCALLEDTHIS")
	 nil)
   (error () :good))
  :good)

(deftest load-logical-pathname-translations.error.2
  (signals-error (load-logical-pathname-translations)
		 program-error)
  t)

(deftest load-logical-pathname-translations.error.3
  (signals-error (load-logical-pathname-translations "CLTESTROOT" nil)
		 program-error)
  t)

